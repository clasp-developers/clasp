// Boost.Geometry (aka GGL, Generic Geometry Library)

// Copyright (c) 2012 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_GEOMETRY_ALGORITHMS_DETAIL_BUFFER_BUFFERED_PIECE_COLLECTION_WM_HPP
#define BOOST_GEOMETRY_ALGORITHMS_DETAIL_BUFFER_BUFFERED_PIECE_COLLECTION_WM_HPP


#include <boost/geometry/extensions/algorithms/buffer/buffered_piece_collection.hpp>
#include <boost/geometry/algorithms/unique.hpp>



namespace boost { namespace geometry
{


#ifndef DOXYGEN_NO_DETAIL
namespace detail { namespace buffer
{


#ifdef BOOST_GEOMETRY_DEBUG_WITH_MAPPER
 
template <typename Ring>
struct buffered_piece_collection_with_mapper 
        : public buffered_piece_collection<Ring>
{


    template <typename Mapper>
    inline void map_opposite_locations(Mapper& mapper)
    {
        for (typename boost::range_iterator<typename occupation_map_type::map_type>::type it =
            boost::begin(m_occupation_map.map); 
            it != boost::end(m_occupation_map.map); ++it)
        {
            mapper.map(it->first, it->second.occupied() ? "fill:rgb(255,0,255);" : "fill:rgb(0,192,0);", 7);

            std::ostringstream out;
            out << it->second.angles.size() << std::endl;
            for (std::set<int>::const_iterator sit = it->second.turn_indices.begin(); sit != it->second.turn_indices.end(); ++sit)
            {
                out << "," << *sit;
            }
            mapper.text(it->first, out.str(), "fill:rgb(0,0,0);font-family='Arial';font-size:10px", 6, 8);

            for (unsigned int i = 0; i < it->second.angles.size(); i++)
            {
                double const angle = it->second.angles[i].angle;
                bool const incoming = it->second.angles[i].incoming;
                segment_identifier seg_id = it->second.angles[i].seg_id;

                geometry::model::linestring<point_type> line;
                point_type p1, p2;
                geometry::set<0>(p1, geometry::get<0>(it->first) + cos(angle) * 0.1);
                geometry::set<1>(p1, geometry::get<1>(it->first) + sin(angle) * 0.1);
                geometry::set<0>(p2, geometry::get<0>(it->first) + cos(angle) * 0.4);
                geometry::set<1>(p2, geometry::get<1>(it->first) + sin(angle) * 0.4);
                std::ostringstream out;
                out << (incoming ? "i" : "o") << " " << si(seg_id);
                // out << " " << angle;
                if (incoming)
                {
                    int offset = 7;
                    //if (tp.debug) offset += 5;
                    //out << " " << tp.debug_info;
                    line.push_back(p1);
                    line.push_back(p2);
                    mapper.map(line, "stroke:rgb(0,0,255);stroke-width:1", 1);
                    mapper.map(p1, "fill:rgb(0,0,0);", 2);
                    mapper.text(p2, out.str(), "fill:rgb(0,0,0);font-family='Arial';font-size:8px", 2, offset);
                }
                else
                {
                    line.push_back(p1);
                    line.push_back(p2);
                    mapper.map(line, "stroke:rgb(255,0,0);stroke-width:1", 1);
                    mapper.map(p2, "fill:rgb(0,0,0);", 2);
                    mapper.text(p2, out.str(), "fill:rgb(0,0,0);font-family='Arial';font-size:8px", 2, -2);
                }
            }
        }
    }

    template <typename Mapper>
    inline void map_turns(Mapper& mapper)
    {
        typedef typename geometry::coordinate_type<point_type>::type coordinate_type;
        std::map<std::pair<coordinate_type, coordinate_type>, int> offsets;


        int index = 0;
        for (typename boost::range_iterator<turn_vector_type>::type it =
            boost::begin(m_turns); it != boost::end(m_turns); ++it)
        {
            if (! it->opposite())
            {
                std::pair<coordinate_type, coordinate_type> p
                    = std::make_pair(geometry::get<0>(it->point), geometry::get<1>(it->point));

                char color = 'g';
                std::string fill = "fill:rgb(0,255,0);";
                switch(it->location)
                {
                    case inside_buffer : fill = "fill:rgb(255,0,0);"; color = 'r'; break;
                    case inside_original : fill = "fill:rgb(0,0,255);"; color = 'b'; break;
                }
                std::ostringstream out;
                out << it->operations[0].piece_index << "/" << it->operations[1].piece_index 
                    << " " << si(it->operations[0].seg_id) << "/" << si(it->operations[1].seg_id)
                    << std::endl;
                //out << " " <<  m_pieces[it->operations[0].piece_index].first_seg_id.segment_index
                //     << "+" << m_pieces[it->operations[1].piece_index].first_seg_id.segment_index;
                //out << " " <<  m_pieces[it->operations[0].piece_index].index
                //     << "," << m_pieces[it->operations[1].piece_index].index << std::endl;
                //out << " " <<  it->operations[0].seg_id.segment_index
                //     << "|" << it->operations[1].seg_id.segment_index;
                out << " " << method_char(it->method)
                    << ":" << operation_char(it->operations[0].operation)
                    << "/" << operation_char(it->operations[1].operation);
                out << " " << it->count_within
                    << "-" << it->count_on_helper
                    << "-" << it->count_on_corner
                    << "-" << it->count_on_offsetted
                    << "-" << it->count_on_occupied
                    << "-" << it->count_on_multi
                    //<< it->debug_string
                    ;
                out << color << std::endl;

                out << " " <<  it->operations[0].seg_id.segment_index
                     << "|" << it->operations[1].seg_id.segment_index;
                //out << it->operations[0].enriched.travels_to_vertex_index
                //    << "/" << it->operations[1].enriched.travels_to_vertex_index;

                offsets[p] += 10;
                int offset = offsets[p];

                mapper.map(it->point, fill, 6);
                mapper.text(it->point, out.str(), "fill:rgb(0,0,0);font-family='Arial';font-size:9px;", 5, offset);

                offsets[p] += 25;
            }
        }
    }

    template <typename Tag, typename Mapper>
    inline void map_pieces(Mapper& mapper, bool pieces = true, bool indices = true)
    {
        for(typename piece_vector::const_iterator it = boost::begin(m_pieces);
            it != boost::end(m_pieces);
            ++it)
        {
            Ring corner;

            segment_identifier seg_id = it->first_seg_id;

            if (seg_id.segment_index >= 0)
            {
                buffered_ring<Ring> const& ring = offsetted_rings[seg_id.multi_index];

                std::copy(boost::begin(ring) + seg_id.segment_index, 
                        boost::begin(ring) + it->last_segment_index, 
                        std::back_inserter(corner));
                std::copy(boost::begin(it->helper_segments), 
                        boost::end(it->helper_segments), 
                        std::back_inserter(corner));
            }

            {
                // Corners of onesided buffers are empty.
                // Mapping this might result (for buffer_line_two_bends_right_d_r) in a
                // "unknown location(0): fatal error in "test_main_caller( argc, argv )":
                // class boost::numeric::positive_overflow: bad numeric conversion: positive overflow"
                // This is only in release-mode of MSVC, only for the pieces (mapping of rings)
                // Must be somewhere in either transform or ublas
                // TODO: find out why
                // Making them unique helps somehow (while it then still has the same coordinates...)
                geometry::unique(corner);
            }

            if (pieces && ! corner.empty())
            {
                if (it->type == buffered_segment)
                {
                    if(boost::is_same<Tag, ring_tag>::value || boost::is_same<Tag, polygon_tag>::value)
                    {
                        mapper.map(corner, "opacity:0.3;fill:rgb(255,128,0);stroke:rgb(0,0,0);stroke-width:1");
                    }
                    else if(boost::is_same<Tag, linestring_tag>::value)
                    {
                        mapper.map(corner, "opacity:0.3;fill:rgb(0,255,0);stroke:rgb(0,0,0);stroke-width:1");
                    }
                }
                else
                {
                    mapper.map(corner, "opacity:0.3;fill:rgb(255,0,0);stroke:rgb(0,0,0);stroke-width:1");
                }
            }

            if (indices && ! corner.empty())
            {

                // Put starting piece_index / segment_index in centroid
                point_type centroid;
                if (corner.size() > 3)
                {
                    geometry::centroid(corner, centroid);
                }
                else
                {
                    centroid = corner.front();
                }
                std::ostringstream out;
                out << it->index << "/" << it->first_seg_id.segment_index << ".." << it->last_segment_index - 1;
                mapper.text(centroid, out.str(), "fill:rgb(255,0,0);font-family='Arial';", 5, 5);
            }
        }
    }


    template <typename Mapper>
    inline void map_offsetted_points(Mapper& mapper)
    {
        for(typename buffered_ring_collection<buffered_ring<Ring> >::const_iterator oit = boost::begin(offsetted_rings);
            oit != boost::end(offsetted_rings);
            ++oit)
        {
            int index = 0;
            for (typename boost::range_iterator<std::vector<point_type> const>::type pit = boost::begin(*oit); pit != boost::end(*oit); ++pit)
            {
                mapper.map(*pit, "fill:rgb(0,0,0);", 2);
                std::ostringstream out;
                out << index++;
                mapper.text(*pit, out.str(), "fill:rgb(0,0,255);font-family='Arial';", -5, -5);
            }
        }
    }

    template <typename Mapper>
    inline void map_traverse(Mapper& mapper)
    {
        for(typename buffered_ring_collection<Ring>::const_iterator it = boost::begin(traversed_rings);
            it != boost::end(traversed_rings);
            ++it)
        {
            mapper.map(*it, "opacity:0.4;fill:none;stroke:rgb(0,255,0);stroke-width:8");
        }
    }

    template <typename Mapper>
    inline void map_offsetted(Mapper& mapper)
    {
        for(typename buffered_ring_collection<buffered_ring<Ring> >::const_iterator it = boost::begin(offsetted_rings);
            it != boost::end(offsetted_rings);
            ++it)
        {
            if (it->discarded())
            {
                mapper.map(*it, "opacity:0.4;fill:none;stroke:rgb(255,0,0);stroke-width:8");
            }
            else
            {
                mapper.map(*it, "opacity:0.4;fill:none;stroke:rgb(0,0,255);stroke-width:8");
            }
        }
    }
};

#endif


}} // namespace detail::buffer
#endif // DOXYGEN_NO_DETAIL


}} // namespace boost::geometry

#endif // BOOST_GEOMETRY_ALGORITHMS_DETAIL_BUFFER_BUFFERED_PIECE_COLLECTION_WM_HPP
