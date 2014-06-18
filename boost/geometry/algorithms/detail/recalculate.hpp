// Boost.Geometry (aka GGL, Generic Geometry Library)

// Copyright (c) 2013 Barend Gehrels, Amsterdam, the Netherlands.
// Copyright (c) 2013 Bruno Lalande, Paris, France.
// Copyright (c) 2013 Mateusz Loskot, London, UK.
// Copyright (c) 2013 Adam Wulkiewicz, Lodz, Poland.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_GEOMETRY_ALGORITHMS_DETAIL_RECALCULATE_HPP
#define BOOST_GEOMETRY_ALGORITHMS_DETAIL_RECALCULATE_HPP


#include <cstddef>

#include <boost/concept/requires.hpp>
#include <boost/concept_check.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/mpl/if.hpp>
#include <boost/numeric/conversion/bounds.hpp>
#include <boost/numeric/conversion/cast.hpp>
#include <boost/type_traits.hpp>

#include <boost/geometry/arithmetic/arithmetic.hpp>
#include <boost/geometry/algorithms/append.hpp>
#include <boost/geometry/algorithms/clear.hpp>
#include <boost/geometry/core/access.hpp>
#include <boost/geometry/core/exterior_ring.hpp>
#include <boost/geometry/core/tags.hpp>

#include <boost/geometry/geometries/concepts/check.hpp>


#include <boost/geometry/util/for_each_coordinate.hpp>


namespace boost { namespace geometry
{

#ifndef DOXYGEN_NO_DETAIL
namespace detail { namespace recalculate
{

template <std::size_t Dimension>
struct recalculate_point
{
    template <typename Point1, typename Point2, typename Strategy>
    static inline void apply(Point1& point1, Point2 const& point2, Strategy const& strategy)
    {
        std::size_t const dim = Dimension - 1;
        geometry::set<dim>(point1, strategy.template apply<dim>(geometry::get<dim>(point2)));
        recalculate_point<dim>::apply(point1, point2, strategy);
    }
};

template <>
struct recalculate_point<0>
{
    template <typename Point1, typename Point2, typename Strategy>
    static inline void apply(Point1&, Point2 const&, Strategy const&)
    {
    }
};


template <std::size_t Dimension>
struct recalculate_indexed
{
    template <typename Geometry1, typename Geometry2, typename Strategy>
    static inline void apply(Geometry1& geometry1, Geometry2 const& geometry2, Strategy const& strategy)
    {
        // Do it for both indices in one dimension
        std::size_t const dim = Dimension - 1;
        geometry::set<0, dim>(geometry1, strategy.template apply<dim>(geometry::get<0, dim>(geometry2)));
        geometry::set<1, dim>(geometry1, strategy.template apply<dim>(geometry::get<1, dim>(geometry2)));
        recalculate_indexed<dim>::apply(geometry1, geometry2, strategy);
    }
};

template <>
struct recalculate_indexed<0>
{

    template <typename Geometry1, typename Geometry2, typename Strategy>
    static inline void apply(Geometry1& , Geometry2 const& , Strategy const& )
    {
    }
};



}} // namespace detail::recalculate
#endif // DOXYGEN_NO_DETAIL

#ifndef DOXYGEN_NO_DISPATCH
namespace dispatch
{

template 
<
    typename Geometry1,
    typename Geometry2,
    typename Tag1 = typename geometry::tag<Geometry1>::type,
    typename Tag2 = typename geometry::tag<Geometry2>::type
>
struct recalculate
{
    BOOST_MPL_ASSERT_MSG
        (
            false, NOT_OR_NOT_YET_IMPLEMENTED_FOR_THIS_GEOMETRY_TYPE
            , (types<Geometry1>)
        );
};

template <typename Point1, typename Point2>
struct recalculate<Point1, Point2, point_tag, point_tag>
    : detail::recalculate::recalculate_point<geometry::dimension<Point1>::value>
{};

template <typename Box1, typename Box2>
struct recalculate<Box1, Box2, box_tag, box_tag>
    : detail::recalculate::recalculate_indexed<geometry::dimension<Box1>::value>
{};

template <typename Segment1, typename Segment2>
struct recalculate<Segment1, Segment2, segment_tag, segment_tag>
    : detail::recalculate::recalculate_indexed<geometry::dimension<Segment1>::value>
{};


} // namespace dispatch
#endif // DOXYGEN_NO_DISPATCH



template <typename Geometry1, typename Geometry2, typename Strategy>
inline void recalculate(Geometry1& geometry1, Geometry2 const& geometry2, Strategy const& strategy)
{
    concept::check<Geometry1>();
    concept::check<Geometry2 const>();

    // static assert dimensions (/types) are the same

    dispatch::recalculate<Geometry1, Geometry2>::apply(geometry1, geometry2, strategy);
}


}} // namespace boost::geometry


#endif // BOOST_GEOMETRY_ALGORITHMS_DETAIL_RECALCULATE_HPP
