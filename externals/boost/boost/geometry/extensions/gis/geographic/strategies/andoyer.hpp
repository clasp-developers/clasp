// Boost.Geometry (aka GGL, Generic Geometry Library)

// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_GEOMETRY_EXTENSIONS_GIS_GEOGRAPHIC_STRATEGIES_ANDOYER_HPP
#define BOOST_GEOMETRY_EXTENSIONS_GIS_GEOGRAPHIC_STRATEGIES_ANDOYER_HPP


#include <boost/geometry/strategies/distance.hpp>
#include <boost/geometry/core/radian_access.hpp>
#include <boost/geometry/core/coordinate_type.hpp>
#include <boost/geometry/util/select_calculation_type.hpp>
#include <boost/geometry/util/promote_floating_point.hpp>
#include <boost/geometry/util/math.hpp>

#include <boost/geometry/extensions/gis/geographic/detail/ellipsoid.hpp>


namespace boost { namespace geometry
{

namespace strategy { namespace distance
{


/*!
\brief Point-point distance approximation taking flattening into account
\ingroup distance
\tparam RadiusType Type of specified radius of the Earth
\tparam CalculationType \tparam_calculation
\author After Andoyer, 19xx, republished 1950, republished by Meeus, 1999
\note Although not so well-known, the approximation is very good: in all cases the results
are about the same as Vincenty. In my (Barend's) testcases the results didn't differ more than 6 m
\see http://nacc.upc.es/tierra/node16.html
\see http://sci.tech-archive.net/Archive/sci.geo.satellite-nav/2004-12/2724.html
\see http://home.att.net/~srschmitt/great_circle_route.html (implementation)
\see http://www.codeguru.com/Cpp/Cpp/algorithms/article.php/c5115 (implementation)
\see http://futureboy.homeip.net/frinksamp/navigation.frink (implementation)
\see http://www.voidware.com/earthdist.htm (implementation)
*/
template
<
    typename RadiusType,
    typename CalculationType = void
>
class andoyer
{
public :
    template <typename Point1, typename Point2>
    struct calculation_type
        : promote_floating_point
          <
              typename select_calculation_type
                  <
                      Point1,
                      Point2,
                      CalculationType
                  >::type
          >
    {};

    typedef RadiusType radius_type;

    inline andoyer()
        : m_ellipsoid()
    {}

    explicit inline andoyer(RadiusType f)
        : m_ellipsoid(f)
    {}

    explicit inline andoyer(geometry::detail::ellipsoid<RadiusType> const& e)
        : m_ellipsoid(e)
    {}


    template <typename Point1, typename Point2>
    inline typename calculation_type<Point1, Point2>::type
    apply(Point1 const& point1, Point2 const& point2) const
    {
        return calc<typename calculation_type<Point1, Point2>::type>
            (
                get_as_radian<0>(point1), get_as_radian<1>(point1),
                get_as_radian<0>(point2), get_as_radian<1>(point2)
            );
    }

    inline geometry::detail::ellipsoid<RadiusType> ellipsoid() const
    {
        return m_ellipsoid;
    }

    inline RadiusType radius() const
    {
        return m_ellipsoid.a();
    }


private :
    geometry::detail::ellipsoid<RadiusType> m_ellipsoid;

    template <typename CT, typename T>
    inline CT calc(T const& lon1,
                T const& lat1,
                T const& lon2,
                T const& lat2) const
    {
        CT const G = (lat1 - lat2) / 2.0;
        CT const lambda = (lon1 - lon2) / 2.0;

        if (geometry::math::equals(lambda, 0.0)
            && geometry::math::equals(G, 0.0))
        {
            return 0.0;
        }

        CT const F = (lat1 + lat2) / 2.0;

        CT const sinG2 = math::sqr(sin(G));
        CT const cosG2 = math::sqr(cos(G));
        CT const sinF2 = math::sqr(sin(F));
        CT const cosF2 = math::sqr(cos(F));
        CT const sinL2 = math::sqr(sin(lambda));
        CT const cosL2 = math::sqr(cos(lambda));

        CT const S = sinG2 * cosL2 + cosF2 * sinL2;
        CT const C = cosG2 * cosL2 + sinF2 * sinL2;

        CT const c0 = 0;
        CT const c1 = 1;
        CT const c2 = 2;
        CT const c3 = 3;

        if (geometry::math::equals(S, c0) || geometry::math::equals(C, c0))
        {
            return c0;
        }

        CT const omega = atan(sqrt(S / C));
        CT const r3 = c3 * sqrt(S * C) / omega; // not sure if this is r or greek nu
        CT const D = c2 * omega * m_ellipsoid.a();
        CT const H1 = (r3 - c1) / (c2 * C);
        CT const H2 = (r3 + c1) / (c2 * S);
        CT const f = m_ellipsoid.f();

        return D * (c1 + f * H1 * sinF2 * cosG2 - f * H2 * cosF2 * sinG2);
    }
};


#ifndef DOXYGEN_NO_STRATEGY_SPECIALIZATIONS
namespace services
{

template <typename RadiusType, typename CalculationType>
struct tag<andoyer<RadiusType, CalculationType> >
{
    typedef strategy_tag_distance_point_point type;
};


template <typename RadiusType, typename CalculationType, typename P1, typename P2>
struct return_type<andoyer<RadiusType, CalculationType>, P1, P2>
    : andoyer<RadiusType, CalculationType>::template calculation_type<P1, P2>
{};


template <typename RadiusType, typename CalculationType>
struct comparable_type<andoyer<RadiusType, CalculationType> >
{
    typedef andoyer<RadiusType, CalculationType> type;
};


template <typename RadiusType, typename CalculationType>
struct get_comparable<andoyer<RadiusType, CalculationType> >
{
    static inline andoyer<RadiusType, CalculationType> apply(andoyer<RadiusType, CalculationType> const& input)
    {
        return input;
    }
};

template <typename RadiusType, typename CalculationType, typename P1, typename P2>
struct result_from_distance<andoyer<RadiusType, CalculationType>, P1, P2>
{
    template <typename T>
    static inline typename return_type<andoyer<RadiusType, CalculationType>, P1, P2>::type 
        apply(andoyer<RadiusType, CalculationType> const& , T const& value)
    {
        return value;
    }
};


template <typename Point1, typename Point2>
struct default_strategy<point_tag, Point1, Point2, geographic_tag, geographic_tag>
{
    typedef strategy::distance::andoyer<typename select_coordinate_type<Point1, Point2>::type> type;
};


} // namespace services
#endif // DOXYGEN_NO_STRATEGY_SPECIALIZATIONS


}} // namespace strategy::distance


}} // namespace boost::geometry


#endif // BOOST_GEOMETRY_EXTENSIONS_GIS_GEOGRAPHIC_STRATEGIES_ANDOYER_HPP
