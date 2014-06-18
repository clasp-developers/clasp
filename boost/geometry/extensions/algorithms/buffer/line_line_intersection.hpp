// Boost.Geometry (aka GGL, Generic Geometry Library)

// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_GEOMETRY_ALGORITHMS_DETAIL_BUFFER_LINE_LINE_INTERSECTION_HPP
#define BOOST_GEOMETRY_ALGORITHMS_DETAIL_BUFFER_LINE_LINE_INTERSECTION_HPP


#include <boost/geometry/util/math.hpp>

namespace boost { namespace geometry
{


#ifndef DOXYGEN_NO_DETAIL
namespace detail { namespace buffer
{


// TODO: once change this to proper strategy
// It is different from current segment intersection because these are not segments but lines
// If we have the Line concept, we can create a strategy
template <typename Point, typename Line1, typename Line2 = Line1>
struct line_line_intersection
{
    template <typename A, typename B, typename C, typename D>
    static inline A det(A const& a, B const& b, C const& c, D const& d)
    {
        return a * d - b * c;
    }

    static inline bool apply(Line1 const& line1, Line2 const& line2, Point& p)
    {
        // See http://mathworld.wolfram.com/Line-LineIntersection.html
        typedef typename coordinate_type<Point>::type coordinate_type;
        coordinate_type x1 = get<0,0>(line1), y1 = get<0,1>(line1);
        coordinate_type x2 = get<1,0>(line1), y2 = get<1,1>(line1);
        coordinate_type x3 = get<0,0>(line2), y3 = get<0,1>(line2);
        coordinate_type x4 = get<1,0>(line2), y4 = get<1,1>(line2);

        coordinate_type denominator = det(x1 - x2, y1 - y2, x3 - x4, y3 - y4);

        // TODO: use something else then denominator (sides?) to determine this.

        // If denominator is zero, segments are parallel.
        // We have context information, so know that it should then
        // be the case that line1.p2 == line2.p1, and that is the
        // intersection point.
        if (geometry::math::equals(denominator, 0.0))
        {
            set<0>(p, x2);
            set<1>(p, y2);
            return false;
        }

        coordinate_type d1 = det(x1, y1, x2, y2);
        coordinate_type d2 = det(x3, y3, x4, y4);
        coordinate_type px = det(d1, x1 - x2, d2, x3 - x4) / denominator;
        coordinate_type py = det(d1, y1 - y2, d2, y3 - y4) / denominator;

        set<0>(p, px);
        set<1>(p, py);

#ifdef BOOST_GEOMETRY_DEBUG_BUFFER
        if (geometry::math::abs(denominator) < 1.0e-7)
        {
            std::cout << "small " << denominator << std::endl;
        }
#endif
        return geometry::math::abs(denominator) > 1.0e-7;
    }
};


}} // namespace detail::buffer
#endif // DOXYGEN_NO_DETAIL


}} // namespace boost::geometry


#endif // BOOST_GEOMETRY_ALGORITHMS_DETAIL_BUFFER_LINE_LINE_INTERSECTION_HPP
