// Boost.Geometry (aka GGL, Generic Geometry Library)

// Copyright (c) 2012 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_GEOMETRY_ALGORITHMS_DETAIL_BUFFER_BUFFERED_PIECE_COLLECTION_HPP
#define BOOST_GEOMETRY_ALGORITHMS_DETAIL_BUFFER_BUFFERED_PIECE_COLLECTION_HPP


#include <algorithm>
#include <cstddef>
#include <set>
#include <boost/range.hpp>


#include <boost/geometry/core/coordinate_type.hpp>
#include <boost/geometry/core/point_type.hpp>

#include <boost/geometry/algorithms/equals.hpp>
#include <boost/geometry/algorithms/covered_by.hpp>

#include <boost/geometry/extensions/strategies/buffer_side.hpp>

#include <boost/geometry/extensions/algorithms/buffer/buffered_ring.hpp>
#include <boost/geometry/extensions/algorithms/buffer/buffer_policies.hpp>
#include <boost/geometry/extensions/algorithms/buffer/side_on_convex_range.hpp>

#include <boost/geometry/algorithms/detail/overlay/add_rings.hpp>
#include <boost/geometry/algorithms/detail/overlay/assign_parents.hpp>
#include <boost/geometry/algorithms/detail/overlay/calculate_distance_policy.hpp>
#include <boost/geometry/algorithms/detail/overlay/enrich_intersection_points.hpp>
#include <boost/geometry/algorithms/detail/overlay/enrichment_info.hpp>
#include <boost/geometry/algorithms/detail/overlay/enrich_intersection_points.hpp>
#include <boost/geometry/algorithms/detail/overlay/ring_properties.hpp>
#include <boost/geometry/algorithms/detail/overlay/traversal_info.hpp>
#include <boost/geometry/algorithms/detail/overlay/traverse.hpp>
#include <boost/geometry/algorithms/detail/overlay/turn_info.hpp>
#include <boost/geometry/algorithms/detail/occupation_info.hpp>
#include <boost/geometry/algorithms/detail/partition.hpp>

#ifdef BOOST_GEOMETRY_DEBUG_BUFFER_OCCUPATION
#  include <boost/geometry/algorithms/detail/overlay/debug_turn_info.hpp>
#  include <boost/geometry/io/wkt/wkt.hpp>
#endif


namespace boost { namespace geometry
{


#ifndef DOXYGEN_NO_DETAIL
namespace detail { namespace buffer
{

enum segment_relation_code
{
    segment_relation_on_left, 
    segment_relation_on_right, 
    segment_relation_within,
    segment_relation_disjoint
};


// Checks if an intersection point is inside a geometry
// In some cases a trivial check might be done, e.g. using symmetric distance:
// the point must be further than the distance from the geometry
template <typename Tag>
struct check_original
{
};

template <>
struct check_original<polygon_tag>
{
    template <typename Point, typename Geometry, typename DistanceStrategy>
    static inline int apply(Point const& point, Geometry const& geometry, DistanceStrategy const& distance_strategy)
    {
        return geometry::covered_by(point, geometry) ? 1 : -1;
    }
};

template <>
struct check_original<linestring_tag>
{
    template <typename Point, typename Geometry, typename DistanceStrategy>
    static inline int apply(Point const& point, Geometry const& geometry, DistanceStrategy const& distance_strategy)
    {
        return 0;
    }
};

template <>
struct check_original<point_tag>
{
    template <typename Point, typename Geometry, typename DistanceStrategy>
    static inline int apply(Point const& point, Geometry const& geometry, DistanceStrategy const& distance_strategy)
    {
        return 0;
    }
};


template <typename P>
class relaxed_side
{
public :

    // Template member function, because it is not always trivial
    // or convenient to explicitly mention the typenames in the
    // strategy-struct itself.

    // Types can be all three different. Therefore it is
    // not implemented (anymore) as "segment"

    static inline int apply(P const& p1, P const& p2, P const& p)
    {
        typedef typename coordinate_type<P>::type coordinate_type;

        coordinate_type const x = get<0>(p);
        coordinate_type const y = get<1>(p);

        coordinate_type const sx1 = get<0>(p1);
        coordinate_type const sy1 = get<1>(p1);
        coordinate_type const sx2 = get<0>(p2);
        coordinate_type const sy2 = get<1>(p2);

        // Promote float->double, small int->int
        typedef typename geometry::select_most_precise
            <
                coordinate_type,
                double
            >::type promoted_type;

        promoted_type const dx = sx2 - sx1;
        promoted_type const dy = sy2 - sy1;
        promoted_type const dpx = x - sx1;
        promoted_type const dpy = y - sy1;

        promoted_type const s 
            = geometry::detail::determinant<promoted_type>
                (
                    dx, dy, 
                    dpx, dpy
                );

        promoted_type const zero = promoted_type();
        promoted_type const relaxed_epsilon = std::numeric_limits<double>::epsilon() * 5.0;

        return math::abs(s) < relaxed_epsilon ? 0
            : s > zero ? 1 
            : -1;
    }
};



template <typename Ring>
struct buffered_piece_collection
{
    typedef typename geometry::point_type<Ring>::type point_type;
    typedef typename geometry::coordinate_type<Ring>::type coordinate_type;

    struct piece
    {
        piece_type type;
        int index;

        // These both form a complete clockwise ring for each piece (with one dupped point)

        // 1: half, part of offsetted_rings
        segment_identifier first_seg_id;
        int last_segment_index; // no segment-identifier - it is always the same

        // 2: half, not part (will be indexed in one vector too)
        std::vector<point_type> helper_segments; // 3 points for segment, 2 points for join - 0 points for flat-end
    };


    typedef typename strategy::side::services::default_strategy
        <
            typename cs_tag<point_type>::type
        >::type side_strategy;
    typedef std::vector<piece> piece_vector;

    piece_vector m_pieces;
    buffered_ring_collection<buffered_ring<Ring> > offsetted_rings; // indexed by multi_index
    buffered_ring_collection<Ring> traversed_rings;
    segment_identifier current_segment_id;

    std::map<std::pair<segment_identifier, segment_identifier>, std::set<int> > m_turn_indices_per_segment_pair;


    typedef std::vector<buffer_turn_info<point_type> > turn_vector_type;
    typedef detail::overlay::get_turn_info
        <
            point_type, point_type, buffer_turn_info<point_type>,
            turn_assign_for_buffer
        > turn_policy;
    turn_vector_type m_turns;


    // To check clustered locations we keep track of segments being opposite somewhere
    std::set<segment_identifier> m_in_opposite_segments;

    struct buffer_occupation_info : public occupation_info<angle_info<point_type, coordinate_type> >
    {
        std::set<segment_identifier> seg_ids;
        std::set<int> turn_indices;
    };

    typedef occupation_map<point_type, buffer_occupation_info> occupation_map_type;
    occupation_map_type m_occupation_map;


    struct redundant_turn
    {
        inline bool operator()(buffer_turn_info<point_type> const& turn) const
        {
            // Erase discarded turns (location not OK) and the turns
            // only used to detect oppositeness.
            return turn.location != location_ok 
                || turn.opposite();
        }
    };


    inline bool is_neighbor(piece const& piece1, piece const& piece2) const
    {
        if (piece1.first_seg_id.multi_index != piece2.first_seg_id.multi_index)
        {
            return false;
        }

        if (std::abs(piece1.index - piece2.index) == 1)
        {
            return true;
        }

        int const last = boost::size(m_pieces) - 1;
        return (piece1.index == 0 && piece2.index == last)
            || (piece1.index == last && piece2.index == 0)
            ;
    }

    inline bool skip_neighbor(piece const& piece1, piece const& piece2) const
    {
        return piece1.type != piece2.type && is_neighbor(piece1, piece2);
    }

    template <typename Range, typename Iterator>
    inline void move_to_next_point(Range const& range, Iterator& next) const
    {
        ++next;
        if (next == boost::end(range))
        {
            next = boost::begin(range) + 1;
        }
    }

    template <typename Range, typename Iterator>
    inline Iterator next_point(Range const& range, Iterator it) const
    {
        Iterator result = it;
        move_to_next_point(range, result);
        while(geometry::equals(*it, *result))
        {
            move_to_next_point(range, result);
        }
        return result;
    }

    inline void calculate_turns(piece const& piece1, piece const& piece2)
    {
        typedef typename boost::range_iterator<buffered_ring<Ring> const>::type iterator;

        segment_identifier seg_id1 = piece1.first_seg_id;
        segment_identifier seg_id2 = piece2.first_seg_id;

        if (seg_id1.segment_index < 0 || seg_id2.segment_index < 0)
        {
            return;
        }

        buffered_ring<Ring> const& ring1 = offsetted_rings[seg_id1.multi_index];
        iterator it1_first = boost::begin(ring1) + seg_id1.segment_index;
        iterator it1_last = boost::begin(ring1) + piece1.last_segment_index;

        buffered_ring<Ring> const& ring2 = offsetted_rings[seg_id2.multi_index];
        iterator it2_first = boost::begin(ring2) + seg_id2.segment_index;
        iterator it2_last = boost::begin(ring2) + piece2.last_segment_index;

        buffer_turn_info<point_type> the_model;
        the_model.operations[0].piece_index = piece1.index;
        the_model.operations[0].seg_id = piece1.first_seg_id;

        iterator it1 = it1_first;
        for (iterator prev1 = it1++; 
                it1 != it1_last; 
                prev1 = it1++, the_model.operations[0].seg_id.segment_index++)
        {
            the_model.operations[1].piece_index = piece2.index;
            the_model.operations[1].seg_id = piece2.first_seg_id;

            iterator next1 = next_point(ring1, it1);

            iterator it2 = it2_first;
            for (iterator prev2 = it2++; 
                    it2 != it2_last;
                    prev2 = it2++, the_model.operations[1].seg_id.segment_index++)
            {
                // Revert (this is used more often - should be common function TODO)
                the_model.operations[0].other_id = the_model.operations[1].seg_id;
                the_model.operations[1].other_id = the_model.operations[0].seg_id;

                iterator next2 = next_point(ring2, it2);

                turn_policy::apply(*prev1, *it1, *next1,
                                    *prev2, *it2, *next2,
                                    the_model, std::back_inserter(m_turns));
            }
        }
    }

    inline void fill_opposite_segments()
    {
        for (typename boost::range_iterator<turn_vector_type const>::type it =
            boost::begin(m_turns); it != boost::end(m_turns); ++it)
        {
            if (it->is_opposite)
            {
                m_in_opposite_segments.insert(it->operations[0].seg_id);
                m_in_opposite_segments.insert(it->operations[1].seg_id);
//std::cout << " " << it->operations[0].seg_id.segment_index;
            }
        }
    }

    inline segment_relation_code get_segment_relation(point_type const& point,
                segment_identifier const& seg_id) const
    {
        typedef typename boost::range_iterator<std::vector<point_type> const>::type iterator_type;
        iterator_type it = boost::begin(offsetted_rings[seg_id.multi_index]) + seg_id.segment_index;
        iterator_type prev = it++;
        int side = side_strategy::apply(point, *prev, *it);
        if (side == 0)
        {
            if (geometry::equals(point, *prev))
            {
                return segment_relation_on_left;
            }
            else if (geometry::equals(point, *it))
            {
                return segment_relation_on_right;
            }
            else if (collinear_point_on_segment(point, *prev, *it))
            {
                return segment_relation_within;
            }
        }
        return segment_relation_disjoint;
    }

    inline void add_angles(int turn_index, int operation_index, point_type const& point, buffer_turn_operation<point_type> const& operation)
    {
        point_type mapped_point;
        buffer_occupation_info& info = m_occupation_map.find_or_insert(point, mapped_point);
        info.turn_indices.insert(turn_index);
        info.seg_ids.insert(operation.seg_id);
        add_incoming_and_outgoing_angles(mapped_point, point, 
                    offsetted_rings[operation.seg_id.multi_index], 
                    turn_index, operation_index,
                    operation.seg_id, 
                    info);
    }

    inline void add_angles(int turn_index)
    {
        if (! m_occupation_map.contains_turn_index(turn_index))
        {
            m_occupation_map.insert_turn_index(turn_index);

            buffer_turn_info<point_type> const& turn = m_turns[turn_index];

//std::cout << "Adding point " << turn_index << " " << geometry::wkt(turn.point) << std::endl;

            add_angles(turn_index, 0, turn.point, turn.operations[0]);
            add_angles(turn_index, 1, turn.point, turn.operations[1]);
        }
    }



    inline void classify_turn(buffer_turn_info<point_type>& turn, piece const& pc) const
    {
        if (pc.type == buffered_flat_end)
        {
            return;
        }

        // Don't check against a piece of which is was preferred in the "situations" for multi - map
        if (turn.piece_indices_to_skip.count(pc.index) > 0)
        {
            return;
        }

        int flat_ends_involved = 0;
        for (int i = 0; i < int(boost::size(turn.operations)); i++)
        {
            // Don't check any turn against a piece of which is itself the result
            if (turn.operations[i].piece_index == pc.index)
            {
                return;
            }

            piece const& piece_from_intersection = m_pieces[turn.operations[i].piece_index];
            if (piece_from_intersection.type == buffered_flat_end)
            {
                flat_ends_involved++;
            }
        }

        segment_identifier seg_id = pc.first_seg_id;
        if (seg_id.segment_index < 0)
        {
            // Should not occur
            std::cout << "Warning: negative segment_index" << std::endl;
            return;
        }

        segment_identifier on_segment_seg_id;

        buffered_ring<Ring> const& ring = offsetted_rings[seg_id.multi_index];

        if (pc.type == buffered_circle)
        {
            // The piece is a full (pseudo) circle. There are no helper segments. We only check if it is the turn is inside the generated circle,
            // or on the border.
            int const side_wrt_circle = side_on_convex_range< /*relaxed_side<point_type> */ side_strategy >(turn.point, 
                            boost::begin(ring) + seg_id.segment_index, 
                            boost::begin(ring) + pc.last_segment_index,
                            seg_id, on_segment_seg_id);
            switch (side_wrt_circle)
            {
                case 0 : turn.count_on_offsetted++; break;
                case -1 : turn.count_within++; break;
            }
            return;
        }

        int side_helper = side_on_convex_range<side_strategy>(turn.point, pc.helper_segments);
        if (side_helper == 1)
        {
            // Left or outside
            return;
        }

        int const side_offsetted = side_on_convex_range< /*relaxed_side<point_type> */ side_strategy >(turn.point, 
                        boost::begin(ring) + seg_id.segment_index, 
                        boost::begin(ring) + pc.last_segment_index,
                        seg_id, on_segment_seg_id);
        if (side_offsetted == 1)
        {
            return;
        }

        if (side_offsetted == -1 && side_helper == -1)
        {
            // It is within (assumed that both halves form a closed convex clockwise ring)
            turn.count_within++;
        }
        if (side_offsetted == 0)
        {
            turn.count_on_offsetted++;
        }
        if (side_helper == 0)
        {
            if (geometry::equals(turn.point, pc.helper_segments.back())
                || geometry::equals(turn.point, pc.helper_segments.front()))
            {
                turn.count_on_corner++;
            }
            else
            {
                if (flat_ends_involved == 0)
                {
                    turn.count_on_helper++;
#ifdef BOOST_GEOMETRY_DEBUG_WITH_MAPPER
                    std::ostringstream out;
                    out << "HLP " << pc.index;
                    turn.debug_string += out.str();
#endif
                }
                else
                {
                    turn.count_on_corner++;
                }
            }
        }
    }

    inline void debug_turns_by_indices(std::string const& caption, std::set<int> const& indices) const
    {
#ifdef BOOST_GEOMETRY_DEBUG_BUFFER_OCCUPATION
        std::cout << caption << ": " << indices.size() << std::endl;
        for (std::set<int>::const_iterator sit = indices.begin(); sit != indices.end(); ++sit)
        {
            int const index = *sit;
            std::cout << "Keep "  << index // << "[" << sit->second << "]"
                << " "<< si(m_turns[index].operations[0].seg_id)
                << " "<< si(m_turns[index].operations[1].seg_id)
                << " " << m_turns[index].operations[0].piece_index
                << "/" << m_turns[index].operations[1].piece_index
                << " " << method_char(m_turns[index].method)
                << " " << operation_char(m_turns[index].operations[0].operation)
                << "/" << operation_char(m_turns[index].operations[1].operation)
                << std::endl;
        }
#endif
    }

    inline void fill_segment_map()
    {
        m_turn_indices_per_segment_pair.clear();
        int index = 0;
        for (typename boost::range_iterator<turn_vector_type>::type it =
            boost::begin(m_turns); it != boost::end(m_turns); ++it, ++index)
        {
            m_turn_indices_per_segment_pair
                [
                    ordered_pair
                        (
                            m_turns[index].operations[0].seg_id,
                            m_turns[index].operations[1].seg_id
                        )
                ].insert(index);
        }
    }


    // Sets "count_on_multi" (if not kept) or "piece_indices_to_skip" (if kept)
    // for to avoid within operations for these pieces
    inline void process_left_turns(buffer_occupation_info const& info,
                    std::set<int> const& keep_indices)
    {
        for (std::set<int>::const_iterator sit1 = info.turn_indices.begin();
            sit1 != info.turn_indices.end();
            ++sit1)
        {
            if (keep_indices.count(*sit1) == 0)
            {
                m_turns[*sit1].count_on_multi++;
            }
            else
            {
                for (std::set<int>::const_iterator sit2 = info.turn_indices.begin();
                    sit2 != info.turn_indices.end();
                    ++sit2)
                {
                    if (sit2 != sit1)
                    {
                        m_turns[*sit1].piece_indices_to_skip.insert(m_turns[*sit2].operations[0].piece_index);
                        m_turns[*sit1].piece_indices_to_skip.insert(m_turns[*sit2].operations[1].piece_index);
                    }
                }
            }
        }
    }

    inline void get_left_turns(buffer_occupation_info& info)
    {
        debug_turns_by_indices("Examine", info.turn_indices);

        std::set<int> keep_indices;
        info.get_left_turns(m_turns, m_turn_indices_per_segment_pair, keep_indices);

        if (! keep_indices.empty())
        {
#ifdef BOOST_GEOMETRY_DEBUG_BUFFER_OCCUPATION
            std::cout << "USE " << keep_indices.size() << " OF " << info.turn_indices.size() << " TURNS" << std::endl;
#endif
            process_left_turns(info, keep_indices);
        }
    }

    inline int piece_count(buffer_occupation_info const& info)
    {
        std::set<int> piece_indices;

        for (std::set<int>::const_iterator sit = info.turn_indices.begin();
            sit != info.turn_indices.end();
            ++sit)
        {
            piece_indices.insert(m_turns[*sit].operations[0].piece_index);
            piece_indices.insert(m_turns[*sit].operations[1].piece_index);
        }
        return piece_indices.size();
    }

    inline void classify_occupied_locations()
    {
        for (typename boost::range_iterator<typename occupation_map_type::map_type>::type it =
                boost::begin(m_occupation_map.map);
            it != boost::end(m_occupation_map.map); ++it)
        {
            buffer_occupation_info& info = it->second;

#ifdef BOOST_GEOMETRY_DEBUG_BUFFER_OCCUPATION
            std::cout << std::endl << "left turns: " << piece_count(info) << " "
                //<< std::setprecision(20)
                << geometry::wkt(it->first) << std::endl;
#endif
            if (piece_count(info) > 2)
            {
                if (info.occupied())
                {
#ifdef BOOST_GEOMETRY_DEBUG_BUFFER_OCCUPATION
                    std::cout << "-> occupied" << std::endl;

                    // std::set<int> turn_indices;
                    //info.get_left_turns(it->first, m_turns, turn_indices, keep_indices); // just for debug-info
#endif

                    for (std::set<int>::const_iterator sit = info.turn_indices.begin();
                        sit != info.turn_indices.end();
                        ++sit)
                    {
                        m_turns[*sit].count_on_occupied++;
                    }
                }
                else
                {
                    get_left_turns(info);
                }
                //std::cout << geometry::wkt(it->first) << " " << int(info.occupied()) << std::endl;
            }
        }
    }

    // The "get_left_turn" process indicates, if it is a u/u turn (both only applicable
    // for union, two separate turns), which is indicated in the map. If done so, set
    // the other to "none", it is part of an occupied situation and should not be followed.
    inline void process_uu()
    {
        for (typename boost::range_iterator<turn_vector_type>::type it =
            boost::begin(m_turns); it != boost::end(m_turns); ++it)
        {
            if (it->both(detail::overlay::operation_union)
                && (it->operations[0].include_in_occupation_map
                    || it->operations[1].include_in_occupation_map))
            {
                bool set_to_none = false;

                // Avoid both turns of a u/u turn to be included.
                if (! it->operations[0].include_in_occupation_map)
                {
                    it->operations[0].operation = detail::overlay::operation_none;
                    set_to_none = true;
                }
                if (! it->operations[1].include_in_occupation_map)
                {
                    it->operations[1].operation = detail::overlay::operation_none;
                    set_to_none = true;
                }
                if (set_to_none)
                {
                    std::cout << "-";
                }
#if defined(BOOST_GEOMETRY_COUNT_DOUBLE_UU)
                else
                {
                    it->count_on_uu++;
                }
#endif
#ifdef BOOST_GEOMETRY_DEBUG_BUFFER_OCCUPATION
                //std::cout << "Keep "  << *sit
                //    << " "<< si(m_turns[*sit].operations[0].seg_id)
                //    << " "<< si(m_turns[*sit].operations[1].seg_id)
                //    << " " << m_turns[*sit].operations[0].piece_index
                //    << "/" << m_turns[*sit].operations[1].piece_index
                //    << " " << method_char(m_turns[*sit].method)
                //    << " " << operation_char(m_turns[*sit].operations[0].operation)
                //    << "/" << operation_char(m_turns[*sit].operations[1].operation)
                    //<< std::endl;
#endif

            }
        }
    }

#define BOOST_GEOMETRY_DEBUG_BUFFER_SITUATION_MAP
#ifdef BOOST_GEOMETRY_DEBUG_BUFFER_SITUATION_MAP
    inline int get_side(point_type const& point, Ring const& ring, int segment_index)
    {
        typedef typename boost::range_iterator<Ring const> iterator_type;
        iterator_type it = boost::begin(ring) + segment_index;
        iterator_type prev = it++;
        return side_strategy::apply(point, *prev, *it);
    }
#endif

    template <typename Iterator>
    static inline point_type const& select_for_side(Iterator first, Iterator second, int index)
    {
        return index == 0 ? *first : *second;
    }


    inline int get_side(segment_identifier const& seg_id1, segment_identifier const& seg_id2, int which = 1) const
    {
        typedef typename boost::range_iterator<Ring const>::type iterator_type;

        Ring const& ring1 = offsetted_rings[seg_id1.multi_index];
        Ring const& ring2 = offsetted_rings[seg_id2.multi_index];

        iterator_type it1 = boost::begin(ring1) + seg_id1.segment_index;
        iterator_type it2 = boost::begin(ring2) + seg_id2.segment_index;

        iterator_type prev1 = it1++;
        iterator_type prev2 = it2++;

        int code1 = side_strategy::apply(select_for_side(prev1, it1, which), *prev2, *it2);
        int code2 = side_strategy::apply(select_for_side(prev2, it2, which), *prev1, *it1);

        if (code1 == 1 && code2 == -1) return 1;
        if (code1 == -1 && code2 == 1) return -1;

        // ROBUSTNESS: in near collinear cases one might be zero, the other non-zero.
        // This happens several times.
        if (code1 != 0) return code1;
        if (code2 != 0) return -code2;

  //      // Check if the other side gives some more info
  //      // (I've never seen this is the case though it might be so, if they are much longer.
        //int code1f = side_strategy::apply(*prev1, *prev2, *it2);
        //int code2f = side_strategy::apply(*prev2, *prev1, *it1);

  //      if (code1f != 0 || code2f != 0)
  //      {
  //          std::cout << "From: " << code1f << " " << code2f << std::endl;
  //          if (code1f != 0) return -code1f;
  //          if (code2f != 0) return code2f;
  //      }

        // Collinear?
#ifdef BOOST_GEOMETRY_DEBUG_BUFFER_SITUATION_MAP
        //std::cout << "Collinear: " << code1 << " " << code2 << std::endl;
#endif
        return 0;
    }



    inline void debug_segment(segment_identifier id)
    {
        typedef typename boost::range_iterator<buffered_ring<Ring> const>::type iterator;

        buffered_ring<Ring> const& ring = offsetted_rings[id.multi_index];
        iterator it = boost::begin(ring) + id.segment_index;
        iterator prev = it++;
        geometry::model::referring_segment<point_type const&> segment(*prev, *it);
        //std::cout << geometry::wkt(*prev) << " " << geometry::wkt(*it) << std::endl;
    }


    struct cluster_info
    {
        inline cluster_info(int i, point_type p, buffer_turn_operation<point_type> op)
            : turn_index(i)
            , point(p)
            , operation(op)
        {}

        inline cluster_info()
            : turn_index(-1)
        {}

        int turn_index;
        point_type point;
        buffer_turn_operation<point_type> operation;
    };

    struct clustered_info
    {
        int piece_index;
        std::set<segment_identifier> intersecting_ids;
        std::vector<cluster_info> intersecting_segments;
    };

#ifdef OLD
    struct situation_info
    {
        std::set<int> turn_indices;
        std::set<segment_identifier> seg_ids;
    };
#endif

    static inline bool add_mutual_intersection(clustered_info const& cluster, segment_identifier const& seg_id)
    {
        bool result = false;
        //if (cluster.intersecting_ids.count(seg_id) > 0)
        typedef typename boost::range_iterator<std::vector<cluster_info> const>::type iterator_type;
        for(iterator_type it = cluster.intersecting_segments.begin(); it != cluster.intersecting_segments.end(); it++)
        {
            if (it->operation.seg_id == seg_id)
            {
                result = true;
            }
        }
        return result;
    }

    inline bool mutually_interact(cluster_info const& a, cluster_info const& b, clustered_info const& other) const
    {
        // Either these two segments intersect, or they are perfectly collinear.
        // First check the intersection:
        if (add_mutual_intersection(other, b.operation.seg_id))
        {
            std::cout << "#";
            return true;
        }

        // Then the collinearity
        if (get_side(a.operation.seg_id, b.operation.seg_id) == 0)
        {
            std::cout << "1";
            return true;

        }

        if (get_side(a.operation.seg_id, b.operation.seg_id, 0) == 0)
        {
            std::cout << "0";
            return true;
        }

        if (geometry::equals(a.point, b.point))
        {
            std::cout << "=";
            return true;
        }

        relaxed_less<point_type> comparator;
        if (comparator.equals(a.point, b.point))
        {
            std::cout << "*";
            return true;
        }

        return false;
    }

    inline void add_mutual_intersections(clustered_info const& cluster, std::map<segment_identifier, clustered_info> const& map)
    {
        typedef std::map<segment_identifier, clustered_info> map_type;
        typedef typename boost::range_iterator<map_type const>::type map_it;
        typedef typename boost::range_iterator<std::vector<cluster_info> const>::type cluster_it;
        for(cluster_it it1 = cluster.intersecting_segments.begin(); it1 != cluster.intersecting_segments.end(); it1++)
        {
            map_it const& other_cluster_it = map.find(it1->operation.seg_id);
            if (other_cluster_it != map.end())
            {
                for(cluster_it it2 = it1 + 1; it2 != cluster.intersecting_segments.end(); it2++)
                {
                    if (! m_occupation_map.contains_turn_index(it1->turn_index)
                        || ! m_occupation_map.contains_turn_index(it2->turn_index))
                    {
                        if (mutually_interact(*it1, *it2, other_cluster_it->second))
                        {
                            add_angles(it1->turn_index);
                            add_angles(it2->turn_index);
                        }
                    }
                }
            }
        }
    }


    inline void add_multi_intersections_to_occupation_map()
    {
        // Pass 1: create map of all segments
        typedef std::map<segment_identifier, clustered_info> map_type;
        map_type map;

        int index = 0;
        for (typename boost::range_iterator<turn_vector_type>::type it =
            boost::begin(m_turns); it != boost::end(m_turns); ++it, ++index)
        {
            buffer_turn_info<point_type> const& turn = *it;

            // Take care with all the indices
            map[turn.operations[0].seg_id].piece_index = turn.operations[0].piece_index;
            map[turn.operations[0].seg_id].intersecting_segments.push_back(cluster_info(index, turn.point, turn.operations[1]));
            map[turn.operations[0].seg_id].intersecting_ids.insert(turn.operations[1].seg_id);

            map[turn.operations[1].seg_id].piece_index = turn.operations[1].piece_index;
            map[turn.operations[1].seg_id].intersecting_segments.push_back(cluster_info(index, turn.point, turn.operations[0]));
            map[turn.operations[1].seg_id].intersecting_ids.insert(turn.operations[0].seg_id);
        }

        // Pass 2: 
        // Verify all segments crossing with more than one segment, and if they intersect each other,
        // add that pair
        for (typename map_type::const_iterator mit = map.begin(); mit != map.end(); ++mit)
        {
            if (mit->second.intersecting_segments.size() > 1)
            {
                add_mutual_intersections(mit->second, map);
            }
        }
    }

    inline void get_occupation()
    {
        fill_opposite_segments();

        // Pass 1: fill all segments part of opposite segments
        int index = 0;
        for (typename boost::range_iterator<turn_vector_type>::type it =
            boost::begin(m_turns); it != boost::end(m_turns); ++it, ++index)
        {
            buffer_turn_info<point_type>& turn = *it;
//std::cout << "Referring to point " << geometry::wkt(turn.point) << std::endl;
            if (m_in_opposite_segments.count(turn.operations[0].seg_id) > 0
                || m_in_opposite_segments.count(turn.operations[1].seg_id) > 0)
            {
                add_angles(index);
            }
        }

        // Pass 2: add multi intersection
        add_multi_intersections_to_occupation_map();

        // Pass 3: fill all segments intersecting in points present in the map
        index = 0;
        for (typename boost::range_iterator<turn_vector_type>::type it =
            boost::begin(m_turns); it != boost::end(m_turns); ++it, ++index)
        {
            buffer_turn_info<point_type>& turn = *it;
            if (m_in_opposite_segments.count(turn.operations[0].seg_id) == 0
                && m_in_opposite_segments.count(turn.operations[1].seg_id) ==  0)
            {
                // See if it is in the map
                if (m_occupation_map.contains(turn.point))
                {
                    add_angles(index);
                }
            }
        }
    }

    inline void classify_turns()
    {

        // Check if it is inside any of the pieces
        // Now: quadratic
        // TODO: in partition.
        for (typename boost::range_iterator<turn_vector_type>::type it =
            boost::begin(m_turns); it != boost::end(m_turns); ++it)
        {
            //if (it->count_on_occupied == 0)
            {
                typename std::vector<piece>::const_iterator pit;
                for (pit = boost::begin(m_pieces);
                    pit != boost::end(m_pieces);
                    ++pit)
                {
                    classify_turn(*it, *pit);
                }
            }
        }
    }

    template <typename Turn>
    inline bool classify_turn_inside(Turn const& turn) const
    {
        return turn.count_within > 0 
            || turn.count_on_multi > 0
            || turn.count_on_helper > 0
            || turn.count_on_occupied > 0
            ;
    }

    inline void classify_inside()
    {
        // Set results:
        for (typename boost::range_iterator<turn_vector_type>::type it =
            boost::begin(m_turns); it != boost::end(m_turns); ++it)
        {
            if (classify_turn_inside(*it))
            {
                it->location = inside_buffer;
            }

#if defined(BOOST_GEOMETRY_COUNT_DOUBLE_UU)
            else if (it->count_on_uu > 0)
            {
                extern int g_count_double_uu;
                g_count_double_uu++;
                std::cout << "UU";
            }
#endif

        }
    }

    template <typename Geometry, typename DistanceStrategy>
    inline void check_remaining_points(Geometry const& input_geometry, DistanceStrategy const& distance_strategy)
    {
        int const factor = distance_strategy.factor();
        for (typename boost::range_iterator<turn_vector_type>::type it =
            boost::begin(m_turns); it != boost::end(m_turns); ++it)
        {
            if (it->location == location_ok)
            {
                int code = check_original
                        <
                            typename geometry::tag<Geometry>::type
                        >::apply(it->point, input_geometry, distance_strategy);
                if (code * factor == 1)
                {
                    it->location = inside_original;
                }
            }
        }
    }

    template <typename Turns>
    static inline void split_uu_turns(Turns& turns)
    {
        Turns added;
        
        for (typename boost::range_iterator<Turns>::type it = boost::begin(turns);
            it != boost::end(turns); ++it)
        {
            if (it->both(detail::overlay::operation_union))
            {
                //std::cout << "U";

                typename boost::range_value<Turns>::type turn = *it; // copy by value
                // std::swap(turn.operations[0], turn.operations[1]);
                turn.operations[0].operation = detail::overlay::operation_continue;
                turn.operations[1].operation = detail::overlay::operation_continue;
                it->operations[1].operation = detail::overlay::operation_continue;
                it->operations[0].operation = detail::overlay::operation_continue;
                added.push_back(turn);
            }
        }

        for (typename boost::range_iterator<Turns>::type it = boost::begin(added);
            it != boost::end(added); ++it)
        {
            turns.push_back(*it);
        }

        if (added.size() > 0)
        {
            for (typename boost::range_iterator<Turns>::type it = boost::begin(turns);
                it != boost::end(turns); ++it)
            {
                std::cout << "Turn"
                    << " "<< si(it->operations[0].seg_id)
                    << " "<< si(it->operations[1].seg_id)
                    << " " << it->operations[0].piece_index
                    << "/" << it->operations[1].piece_index
                    << " " << method_char(it->method)
                    << " " << operation_char(it->operations[0].operation)
                    << "/" << operation_char(it->operations[1].operation)
                    << std::endl;
            }
        }

    }


    template <typename Geometry, typename DistanceStrategy>
    inline void get_turns(Geometry const& input_geometry, DistanceStrategy const& distance_strategy)
    {
        // Now: quadratic
        // TODO use partition

        for(typename piece_vector::const_iterator it1 = boost::begin(m_pieces);
            it1 != boost::end(m_pieces);
            ++it1)
        {
            for(typename piece_vector::const_iterator it2 = it1 + 1;
                it2 != boost::end(m_pieces);
                ++it2)
            {
                if (! skip_neighbor(*it1, *it2))
                {
                    calculate_turns(*it1, *it2);
                }
            }
        }

        //discard_uu_turns();
        for (typename boost::range_iterator<turn_vector_type>::type it =
            boost::begin(m_turns); it != boost::end(m_turns); ++it)
        {
            //if (it->both(detail::overlay::operation_union))
            //{
            //    std::cout << "double UU" << std::endl;
            //}
            //std::cout << std::setprecision(16) << geometry::wkt(it->point)
   //             << " " << it->operations[0].piece_index << "/" << it->operations[1].piece_index 
   //             << " " << si(it->operations[0].seg_id) << "/" << si(it->operations[1].seg_id)
            //    << " " << method_char(it->method)
            //    << ":" << operation_char(it->operations[0].operation)
            //    << "/" << operation_char(it->operations[1].operation)
   //             << std::endl;
        }

        //split_uu_turns(m_turns);


        fill_segment_map();
        get_occupation();
        classify_occupied_locations();
        process_uu();
        classify_turns();
        classify_inside();

        check_remaining_points(input_geometry, distance_strategy);
    }

    inline void start_new_ring()
    {
        int const n = offsetted_rings.size();
        current_segment_id.source_index = 0;
        current_segment_id.multi_index = n;
        current_segment_id.ring_index = -1;
        current_segment_id.segment_index = 0;

        offsetted_rings.resize(n + 1);
    }

    inline int add_point(point_type const& p)
    {
        BOOST_ASSERT
            (
                boost::size(offsetted_rings) > 0
            );

        current_segment_id.segment_index++;
        offsetted_rings.back().push_back(p);
        return offsetted_rings.back().size();
    }

    //-------------------------------------------------------------------------

    inline piece& add_piece(piece_type type, bool decrease_segment_index_by_one)
    {
        piece pc;
        pc.type = type;
        pc.index = boost::size(m_pieces);
        pc.first_seg_id = current_segment_id;

        std::size_t const n = boost::size(offsetted_rings.back());
        pc.first_seg_id.segment_index = decrease_segment_index_by_one ? n - 1 : n;

        m_pieces.push_back(pc);
        return m_pieces.back();
    }

    inline void add_piece(piece_type type, point_type const& p1, point_type const& p2, 
            point_type const& b1, point_type const& b2)
    {
        // If the last type was a join, the segment_id of next segment should be decreased by one.
        bool const last_type_join = ! m_pieces.empty() 
                && m_pieces.back().first_seg_id.multi_index == current_segment_id.multi_index
                && (
                        m_pieces.back().type == buffered_join 
                        || m_pieces.back().type == buffered_round_end
                    );

        piece& pc = add_piece(type, last_type_join);

        // If it follows a non-join (so basically the same piece-type) point b1 should be added.
        // There should be two intersections later and it should be discarded.
        // But for now we need it to calculate intersections
        if (! last_type_join)
        {
            add_point(b1);
        }
        pc.last_segment_index = add_point(b2);

        pc.helper_segments.push_back(b2);
        pc.helper_segments.push_back(p2);
        pc.helper_segments.push_back(p1);
        pc.helper_segments.push_back(b1);
    }

    template <typename Range>
    inline piece& add_piece(piece_type type, Range const& range, bool decrease_segment_index_by_one)
    {
        piece& pc = add_piece(type, decrease_segment_index_by_one);

        bool first = true;
        int last = offsetted_rings.back().size() + 1;
        for (typename Range::const_iterator it = boost::begin(range);
            it != boost::end(range);
            ++it)
        {
            bool add = true;
            if (first)
            {
                // Only for very first one, add first. In all other cases it is shared with previous.
                add = offsetted_rings.back().empty();
                first = false;
            }
            if (add)
            {
                last = add_point(*it);
            }

        }

        pc.last_segment_index = last;

        return pc;
    }

    template <typename Range>
    inline void add_piece(piece_type type, point_type const& p, Range const& range)
    {
        piece& pc = add_piece(type, range, true);

        if (boost::size(range) > 0)
        {
            pc.helper_segments.push_back(range.back());
            pc.helper_segments.push_back(p);
            pc.helper_segments.push_back(range.front());
        }
    }

    template <typename EndcapStrategy, typename Range>
    inline void add_endcap(EndcapStrategy const& strategy, Range const& range, point_type const& end_point)
    {
        piece_type pt = strategy.get_piece_type();
        if (pt == buffered_flat_end)
        {
            // It is flat, should just be added, without helper segments
            add_piece(pt, range, true);
        }
        else
        {
            // Normal case, it has an "inside", helper segments should be added
            add_piece(pt, end_point, range);
        }
    }

    //-------------------------------------------------------------------------

    inline void enrich()
    {
        typedef typename strategy::side::services::default_strategy
        <
            typename cs_tag<Ring>::type
        >::type side_strategy_type;

        enrich_intersection_points<false, false>(m_turns,
                    detail::overlay::operation_union,
                    offsetted_rings, offsetted_rings,
                    side_strategy_type());
    }

    // Discards all rings which do have not-OK intersection points only.
    // Those can never be traversed and should not be part of the output.
    inline void discard_rings()
    {
        for (typename boost::range_iterator<turn_vector_type const>::type it =
            boost::begin(m_turns); it != boost::end(m_turns); ++it)
        {
            if (it->location != location_ok)
            {
                offsetted_rings[it->operations[0].seg_id.multi_index].has_discarded_intersections = true;
                offsetted_rings[it->operations[1].seg_id.multi_index].has_discarded_intersections = true;
            }
            else if (! it->both(detail::overlay::operation_union))
            {
                offsetted_rings[it->operations[0].seg_id.multi_index].has_accepted_intersections = true;
                offsetted_rings[it->operations[1].seg_id.multi_index].has_accepted_intersections = true;
            }
        }
    }
                    
    inline void discard_turns()
    {
        m_turns.erase
            (
                std::remove_if(boost::begin(m_turns), boost::end(m_turns),
                                redundant_turn()),
                boost::end(m_turns)
            );

    }

   // inline void discard_uu_turns()
   // {
   //     m_turns.erase
            //(
            //    std::remove_if(boost::begin(m_turns), boost::end(m_turns),
            //                    uu_turn()),
            //    boost::end(m_turns)
            //);

   // }

    inline void traverse()
    {
        typedef detail::overlay::traverse
            <
                false, false, 
                buffered_ring_collection<buffered_ring<Ring> >, buffered_ring_collection<buffered_ring<Ring > >,
                backtrack_for_buffer
            > traverser;

        traversed_rings.clear();
        traverser::apply(offsetted_rings, offsetted_rings,
                        detail::overlay::operation_union,
                        m_turns, traversed_rings);
    }

    template <typename GeometryOutput, typename OutputIterator>
    inline OutputIterator assign(OutputIterator out)
    {
        typedef detail::overlay::ring_properties<point_type> properties;

        std::map<ring_identifier, properties> selected;

        // Select all rings which do not have any self-intersection (other ones should be traversed)
        int index = 0;
        for(typename buffered_ring_collection<buffered_ring<Ring> >::const_iterator it = boost::begin(offsetted_rings);
            it != boost::end(offsetted_rings);
            ++it, ++index)
        {
            if (! it->has_intersections())
            {
                ring_identifier id(0, index, -1);
                selected[id] = properties(*it, true);
            }

//std::cout << geometry::wkt(*it) << std::endl;
        }

        // Select all created rings
        index = 0;
        for (typename boost::range_iterator<buffered_ring_collection<Ring> const>::type
                it = boost::begin(traversed_rings);
                it != boost::end(traversed_rings);
                ++it, ++index)
        {
            ring_identifier id(2, index, -1);
            selected[id] = properties(*it, true);
        }

        detail::overlay::assign_parents(offsetted_rings, traversed_rings, selected, false);
        return detail::overlay::add_rings<GeometryOutput>(selected, offsetted_rings, traversed_rings, out);
    }

    template <typename GeometryOutput>
    inline void assign_offsetted_rings(GeometryOutput& out)
    {
        typedef typename boost::range_iterator<buffered_ring<Ring> const>::type iterator;

        for(typename buffered_ring_collection<buffered_ring<Ring> >::const_iterator it = boost::begin(offsetted_rings);
            it != boost::end(offsetted_rings);
            ++it)
        {
            for (iterator pit = boost::begin(*it); pit != boost::end(*it); ++pit)
            {
                out.push_back(*pit);
            }
        }
    }


};


}} // namespace detail::buffer
#endif // DOXYGEN_NO_DETAIL


}} // namespace boost::geometry

#endif // BOOST_GEOMETRY_ALGORITHMS_DETAIL_BUFFER_BUFFERED_PIECE_COLLECTION_HPP
