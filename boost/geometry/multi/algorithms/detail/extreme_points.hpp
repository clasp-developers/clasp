// Boost.Geometry (aka GGL, Generic Geometry Library)

// Copyright (c) 2007-2013 Barend Gehrels, Amsterdam, the Netherlands.
// Copyright (c) 2008-2013 Bruno Lalande, Paris, France.
// Copyright (c) 2009-2013 Mateusz Loskot, London, UK.
// Copyright (c) 2013 Adam Wulkiewicz, Lodz, Poland.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_GEOMETRY_MULTI_ALGORITHMS_DETAIL_EXTREME_POINTS_HPP
#define BOOST_GEOMETRY_MULTI_ALGORITHMS_DETAIL_EXTREME_POINTS_HPP


#include <cstddef>

#include <boost/range.hpp>

#include <boost/geometry/multi/core/tags.hpp>
#include <boost/geometry/multi/geometries/concepts/check.hpp>

#include <boost/geometry/algorithms/detail/extreme_points.hpp>
    


namespace boost { namespace geometry
{


#ifndef DOXYGEN_NO_DISPATCH
namespace dispatch
{



template<typename MultiPolygon, std::size_t Dimension>
struct extreme_points<MultiPolygon, Dimension, multi_polygon_tag>
{
    template <typename Extremes, typename Intruders>
    static inline bool apply(MultiPolygon const& multi, Extremes& extremes, Intruders& intruders)
    {
        // Get one for the very first polygon, that is (for the moment) enough.
        // It is not guaranteed the "extreme" then, but for the current purpose
        // (point_on_surface) it can just be this point.
        if (boost::size(multi) >= 1)
        {
            return extreme_points
                <
                    typename boost::range_value<MultiPolygon const>::type,
                    Dimension,
                    polygon_tag
                >::apply(*boost::begin(multi), extremes, intruders);
        }

        return false;
    }
};


} // namespace dispatch
#endif // DOXYGEN_NO_DISPATCH


}} // namespace boost::geometry


#endif // BOOST_GEOMETRY_MULTI_ALGORITHMS_DETAIL_EXTREME_POINTS_HPP
