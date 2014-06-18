// Boost.Geometry (aka GGL, Generic Geometry Library)

// Copyright (c) 2012 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_GEOMETRY_ALGORITHMS_DETAIL_BUFFER_SIDE_ON_CONVEX_RANGE_HPP
#define BOOST_GEOMETRY_ALGORITHMS_DETAIL_BUFFER_SIDE_ON_CONVEX_RANGE_HPP

#include <boost/range.hpp>
#include <boost/geometry/algorithms/detail/overlay/segment_identifier.hpp>

namespace boost { namespace geometry
{

#ifndef DOXYGEN_NO_DETAIL
namespace detail { namespace buffer
{

template <int D>
struct collinear_point_on_segment_check
{
    template <typename T>
    static inline bool apply_sorted(T const& subject, T const& c1, T const& c2)
    {
        return subject >= c1 && subject <= c2;
    }

    template <typename P0, typename P1, typename P2>
    static inline bool apply(P0 const& subject, P1 const& p1, P2 const& p2)
    {
        typedef typename geometry::coordinate_type<P0>::type coordinate_type;
        coordinate_type const cs = geometry::get<D>(subject);
        coordinate_type const c1 = geometry::get<D>(p1);
        coordinate_type const c2 = geometry::get<D>(p2);
        return c1 > c2 
            ? apply_sorted(cs, c2, c1)
            : apply_sorted(cs, c1, c2)
            ;
    }
};


// Checks is subject-point is on the segment, provided that it is already determined that it was collinear
template <typename P0, typename P1, typename P2>
inline bool collinear_point_on_segment(P0 const& subject, P1 const& p1, P2 const& p2)
{
    return collinear_point_on_segment_check<0>::apply(subject, p1, p2)
        && collinear_point_on_segment_check<1>::apply(subject, p1, p2);
}


template <typename SideStrategy, typename Point, typename Range>
inline int side_on_convex_range(Point const& subject, Range const& range)
{
    if (boost::empty(range))
    {
        return 1;
    }

    bool has_collinear = false;

    typedef typename boost::range_iterator
        <
            Range const
        >::type iterator_type;

    iterator_type it = boost::begin(range);
    for (iterator_type prev = it++;
        it != boost::end(range);
        prev = it++)
    {
        int const side = SideStrategy::apply(subject, *prev, *it);
        switch(side)
        {
            case 1 : 
                return 1;
            case 0 : 
                // Check if it is really on the segment.
                // If not, it is either on the left (because polygon is convex)
                // or it is still on one of the other segments (if segments are collinear)
                if (collinear_point_on_segment(subject, *prev, *it))
                {
                    return 0;
                }
                has_collinear = true;
                break;
        }
    }
    return has_collinear ? 1 : -1;
}

template <typename SideStrategy, typename Point, typename Iterator>
static inline int side_on_convex_range(Point const& subject,
            Iterator first, Iterator last,
            /* by value: */ segment_identifier seg_id,
            segment_identifier& on_segment_seg_id)
{
    bool has_collinear = false;
    Iterator it = first;
    for (Iterator prev = it++; it != last; prev = it++, seg_id.segment_index++)
    {
        int side = SideStrategy::apply(subject, *prev, *it);
        switch(side)
        {
            case 1 : 
                return 1;
            case 0 : 
                // Check if it is REALLY on the segment.
                // If not, it is either on the left (because polygon is convex)
                // or it is still on one of the other segments (if segments are collinear)
                if (collinear_point_on_segment(subject, *prev, *it))
                {
                    on_segment_seg_id = seg_id;
                    return 0;
                }
                has_collinear = true;
                break;
        }
    }
    return has_collinear ? 1 : -1;
}

}} // namespace detail::buffer
#endif // DOXYGEN_NO_DETAIL

}} // namespace boost::geometry

#endif // BOOST_GEOMETRY_ALGORITHMS_DETAIL_BUFFER_SIDE_ON_CONVEX_RANGE_HPP
