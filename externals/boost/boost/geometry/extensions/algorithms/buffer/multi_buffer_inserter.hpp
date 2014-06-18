// Boost.Geometry (aka GGL, Generic Geometry Library)

// Copyright (c) 2012 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_GEOMETRY_ALGORITHMS_DETAIL_BUFFER_MULTI_BUFFER_INSERTER_HPP
#define BOOST_GEOMETRY_ALGORITHMS_DETAIL_BUFFER_MULTI_BUFFER_INSERTER_HPP

#include <boost/range.hpp>
#include <boost/typeof/typeof.hpp>

#include <boost/geometry/multi/core/point_type.hpp>
#include <boost/geometry/multi/algorithms/distance.hpp>

#include <boost/geometry/extensions/algorithms/buffer/buffer_inserter.hpp>


namespace boost { namespace geometry
{

#ifndef DOXYGEN_NO_DETAIL
namespace detail { namespace buffer
{

template <>
struct check_original<multi_polygon_tag>
{
    template <typename Point, typename Geometry, typename DistanceStrategy>
    static inline int apply(Point const& point, Geometry const& geometry, DistanceStrategy const& distance_strategy)
    {
        return geometry::covered_by(point, geometry) ? 1 : -1;
    }
};

template <>
struct check_original<multi_linestring_tag>
{
    template <typename Point, typename Geometry, typename DistanceStrategy>
    static inline int apply(Point const& point, Geometry const& geometry, DistanceStrategy const& distance_strategy)
    {
        return 0;
    }
};

template <>
struct check_original<multi_point_tag>
{
    template <typename Point, typename Geometry, typename DistanceStrategy>
    static inline int apply(Point const& point, Geometry const& geometry, DistanceStrategy const& distance_strategy)
    {
        return 0;
        //auto dist = boost::geometry::distance(point, geometry);
        //auto d2 = distance_strategy.apply(point, point, buffer_side_left) * 0.99; // TODO: depends on chord length
        //return (dist < d2) ? 1 : -1;
    }
};

template
<
    typename Multi,
    typename PolygonOutput
>
struct multi_buffer_inserter
{
    template
    <
        typename Collection, typename DistanceStrategy, typename JoinStrategy, typename EndStrategy
    >
    static inline void apply(Multi const& multi,
            Collection& collection,
            DistanceStrategy const& distance,
            JoinStrategy const& join_strategy,
            EndStrategy const& end_strategy)
    {
        typedef typename geometry::ring_type<PolygonOutput>::type output_ring_type;
        typedef dispatch::buffer_inserter
            <
                typename single_tag_of
                            <
                                typename tag<Multi>::type
                            >::type,
                typename boost::range_value<Multi const>::type, 
                output_ring_type
            > policy;
            
        for (typename boost::range_iterator<Multi const>::type
                it = boost::begin(multi);
            it != boost::end(multi);
            ++it)
        {
            policy::apply(*it, collection, distance, join_strategy, end_strategy);
        }
    }
};

}} // namespace detail::buffer
#endif // DOXYGEN_NO_DETAIL



#ifndef DOXYGEN_NO_DISPATCH
namespace dispatch
{


template
<
    typename Multi,
    typename PolygonOutput
>
struct buffer_inserter<multi_polygon_tag, Multi, PolygonOutput>
    : public detail::buffer::multi_buffer_inserter<Multi, PolygonOutput>
{};

template
<
    typename Multi,
    typename PolygonOutput
>
struct buffer_inserter<multi_linestring_tag, Multi, PolygonOutput>
    : public detail::buffer::multi_buffer_inserter<Multi, PolygonOutput>
{};


template
<
    typename Multi,
    typename PolygonOutput
>
struct buffer_inserter<multi_point_tag, Multi, PolygonOutput>
    : public detail::buffer::multi_buffer_inserter<Multi, PolygonOutput>
{};


} // namespace dispatch
#endif // DOXYGEN_NO_DISPATCH


}} // namespace boost::geometry

#endif // BOOST_GEOMETRY_ALGORITHMS_DETAIL_BUFFER_MULTI_BUFFER_INSERTER_HPP
