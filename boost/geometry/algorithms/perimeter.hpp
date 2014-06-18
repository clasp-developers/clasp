// Boost.Geometry (aka GGL, Generic Geometry Library)

// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.
// Copyright (c) 2008-2012 Bruno Lalande, Paris, France.
// Copyright (c) 2009-2012 Mateusz Loskot, London, UK.

// Parts of Boost.Geometry are redesigned from Geodan's Geographic Library
// (geolib/GGL), copyright (c) 1995-2010 Geodan, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_GEOMETRY_ALGORITHMS_PERIMETER_HPP
#define BOOST_GEOMETRY_ALGORITHMS_PERIMETER_HPP


#include <boost/variant/static_visitor.hpp>
#include <boost/variant/apply_visitor.hpp>
#include <boost/variant/variant_fwd.hpp>
#include <boost/geometry/algorithms/length.hpp>
#include <boost/geometry/algorithms/detail/calculate_null.hpp>
#include <boost/geometry/algorithms/detail/calculate_sum.hpp>
// #include <boost/geometry/algorithms/detail/throw_on_empty_input.hpp>
#include <boost/geometry/core/cs.hpp>
#include <boost/geometry/core/closure.hpp>
#include <boost/geometry/geometries/concepts/check.hpp>
#include <boost/geometry/strategies/default_length_result.hpp>
#include <boost/geometry/strategies/default_strategy.hpp>
#include <boost/geometry/util/compress_variant.hpp>
#include <boost/geometry/util/transform_variant.hpp>


namespace boost { namespace geometry
{

#ifndef DOXYGEN_NO_DISPATCH
namespace dispatch
{

// Default perimeter is 0.0, specializations implement calculated values
template <typename Geometry, typename Tag = typename tag<Geometry>::type>
struct perimeter : detail::calculate_null
{
    typedef typename default_length_result<Geometry>::type return_type;

    template <typename Strategy>
    static inline return_type apply(Geometry const& geometry, Strategy const& strategy)
    {
        return calculate_null::apply<return_type>(geometry, strategy);
    }
};

template <typename Geometry>
struct perimeter<Geometry, ring_tag>
    : detail::length::range_length
        <
            Geometry,
            closure<Geometry>::value
        >
{};

template <typename Polygon>
struct perimeter<Polygon, polygon_tag> : detail::calculate_polygon_sum
{
    typedef typename default_length_result<Polygon>::type return_type;
    typedef detail::length::range_length
                <
                    typename ring_type<Polygon>::type,
                    closure<Polygon>::value
                > policy;

    template <typename Strategy>
    static inline return_type apply(Polygon const& polygon, Strategy const& strategy)
    {
        return calculate_polygon_sum::apply<return_type, policy>(polygon, strategy);
    }
};


// box,n-sphere: to be implemented

} // namespace dispatch
#endif // DOXYGEN_NO_DISPATCH


namespace resolve_strategy {

struct perimeter
{
    template <typename Geometry, typename Strategy>
    static inline typename default_length_result<Geometry>::type
    apply(Geometry const& geometry, Strategy const& strategy)
    {
        return dispatch::perimeter<Geometry>::apply(geometry, strategy);
    }

    template <typename Geometry>
    static inline typename default_length_result<Geometry>::type
    apply(Geometry const& geometry, default_strategy)
    {
        typedef typename strategy::distance::services::default_strategy
            <
                point_tag,
                typename point_type<Geometry>::type
            >::type strategy_type;

        return dispatch::perimeter<Geometry>::apply(geometry, strategy_type());
    }
};

} // namespace resolve_strategy


namespace resolve_variant {

template <typename Geometry>
struct perimeter
{
    typedef typename default_length_result<Geometry>::type result_type;

    template <typename Strategy>
    static inline result_type
    apply(Geometry const& geometry, Strategy const& strategy)
    {
        concept::check<Geometry const>();
        return resolve_strategy::perimeter::apply(geometry, strategy);
    }
};

template <BOOST_VARIANT_ENUM_PARAMS(typename T)>
struct perimeter<boost::variant<BOOST_VARIANT_ENUM_PARAMS(T)> >
{
    typedef typename compress_variant<
        typename transform_variant<
            boost::variant<BOOST_VARIANT_ENUM_PARAMS(T)>,
            default_length_result<boost::mpl::placeholders::_>
        >::type
    >:: type result_type;

    template <typename Strategy>
    struct visitor: boost::static_visitor<result_type>
    {
        Strategy const& m_strategy;

        visitor(Strategy const& strategy): m_strategy(strategy) {}

        template <typename Geometry>
        result_type operator()(Geometry const& geometry) const
        {
            return perimeter<Geometry>::apply(geometry, m_strategy);
        }
    };

    template <typename Strategy>
    static inline result_type
    apply(boost::variant<BOOST_VARIANT_ENUM_PARAMS(T)> const& geometry,
          Strategy const& strategy)
    {
        return boost::apply_visitor(visitor<Strategy>(strategy), geometry);
    }
};

} // namespace resolve_variant


/*!
\brief \brief_calc{perimeter}
\ingroup perimeter
\details The function perimeter returns the perimeter of a geometry,
    using the default distance-calculation-strategy
\tparam Geometry \tparam_geometry
\param geometry \param_geometry
\return \return_calc{perimeter}

\qbk{[include reference/algorithms/perimeter.qbk]}
 */
template<typename Geometry>
inline typename resolve_variant::perimeter<Geometry>::result_type perimeter(
        Geometry const& geometry)
{
    // detail::throw_on_empty_input(geometry);
    return resolve_variant::perimeter<Geometry>::apply(geometry, default_strategy());
}

/*!
\brief \brief_calc{perimeter} \brief_strategy
\ingroup perimeter
\details The function perimeter returns the perimeter of a geometry,
    using specified strategy
\tparam Geometry \tparam_geometry
\tparam Strategy \tparam_strategy{distance}
\param geometry \param_geometry
\param strategy strategy to be used for distance calculations.
\return \return_calc{perimeter}

\qbk{distinguish,with strategy}
\qbk{[include reference/algorithms/perimeter.qbk]}
 */
template<typename Geometry, typename Strategy>
inline typename resolve_variant::perimeter<Geometry>::result_type perimeter(
        Geometry const& geometry, Strategy const& strategy)
{
    // detail::throw_on_empty_input(geometry);
    return resolve_variant::perimeter<Geometry>::apply(geometry, strategy);
}

}} // namespace boost::geometry

#endif // BOOST_GEOMETRY_ALGORITHMS_PERIMETER_HPP

