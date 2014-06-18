// Boost.Geometry (aka GGL, Generic Geometry Library)

// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_GEOMETRY_EXTENSIONS_ALGORITHMS_OFFSET_HPP
#define BOOST_GEOMETRY_EXTENSIONS_ALGORITHMS_OFFSET_HPP

#include <boost/config.hpp>

#include <boost/range/functions.hpp>

#include <boost/geometry/core/point_type.hpp>
#include <boost/geometry/extensions/algorithms/buffer/buffer_inserter.hpp>
#include <boost/geometry/extensions/strategies/buffer.hpp>
#include <boost/geometry/extensions/strategies/buffer_distance_asymmetric.hpp>
#include <boost/geometry/extensions/strategies/buffer_end_skip.hpp>
#include <boost/geometry/algorithms/detail/disjoint.hpp>
#include <boost/geometry/geometries/concepts/check.hpp>

#include <boost/geometry/geometries/segment.hpp>

namespace boost { namespace geometry
{


#ifndef DOXYGEN_NO_DETAIL
namespace detail { namespace offset
{


template
<
    typename Range,
    typename RangeOut
>
struct offset_range
    : public geometry::detail::buffer::buffer_range
        <
            RangeOut,
            linestring_tag
        >
{
    typedef geometry::detail::buffer::buffer_range
        <
            RangeOut,
            linestring_tag
        > super;
    template
    <
        typename Collection,
        typename DistanceStrategy,
        typename JoinStrategy,
        typename EndStrategy
    >
    static inline void apply(Collection& collection, Range const& range,
                DistanceStrategy const& distance_strategy,
                JoinStrategy const& join_strategy,
                EndStrategy const& end_strategy,
                bool reverse)
    {
        collection.start_new_ring();
        if (reverse)
        {
            super::iterate(collection, boost::rbegin(range), boost::rend(range),
                buffer_side_left,
                distance_strategy, join_strategy, end_strategy);
        }
        else
        {
            super::iterate(collection, boost::begin(range), boost::end(range),
                buffer_side_left,
                distance_strategy, join_strategy, end_strategy);
        }
    }
};

}} // namespace detail::offset
#endif



#ifndef DOXYGEN_NO_DISPATCH
namespace dispatch
{

template
<
    typename GeometryTag,
    typename GeometryOutTag,
    typename Geometry,
    typename GeometryOut
>
struct offset
{};


template
<
    typename Geometry,
    typename GeometryOut
>
struct offset
    <
        linestring_tag,
        linestring_tag,
        Geometry,
        GeometryOut
    >
    : detail::offset::offset_range
        <
            Geometry,
            GeometryOut
        >
{};


} // namespace dispatch
#endif // DOXYGEN_NO_DISPATCH


template
<
    typename Geometry,
    typename GeometryOut,
    typename JoinStrategy,
    typename Distance
>
inline void offset(Geometry const& geometry, GeometryOut& out,
            JoinStrategy const& join_strategy,
            Distance const& distance)
{
    concept::check<Geometry const>();
    concept::check<GeometryOut>();

    typedef typename geometry::point_type<Geometry>::type point_type;

    bool reverse = distance < 0;
    typedef strategy::buffer::distance_asymmetric
        <
            typename geometry::coordinate_type<Geometry>::type
        > distance_strategy_type;
    distance_strategy_type distance_strategy(geometry::math::abs(distance), geometry::math::abs(distance));

    detail::buffer::buffered_piece_collection
        <
            model::ring<point_type>
        > collection;

    typedef strategy::buffer::end_skip<point_type, point_type> end_strategy_type;
    end_strategy_type end_strategy;

    dispatch::offset
    <
        typename tag<Geometry>::type,
        typename tag<GeometryOut>::type,
        Geometry,
        GeometryOut
    >::apply(collection, geometry, distance_strategy, join_strategy, end_strategy, reverse);


    collection.assign_offsetted_rings(out);

}


}} // namespace boost::geometry


#endif // BOOST_GEOMETRY_EXTENSIONS_ALGORITHMS_OFFSET_HPP
