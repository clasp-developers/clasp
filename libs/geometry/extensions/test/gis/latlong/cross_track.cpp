// Boost.Geometry (aka GGL, Generic Geometry Library)
// Unit Test

// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.
// Copyright (c) 2008-2012 Bruno Lalande, Paris, France.
// Copyright (c) 2009-2012 Mateusz Loskot, London, UK.

// Parts of Boost.Geometry are redesigned from Geodan's Geographic Library
// (geolib/GGL), copyright (c) 1995-2010 Geodan, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)


#include <geometry_test_common.hpp>

#include <boost/geometry/algorithms/assign.hpp>
#include <boost/geometry/algorithms/distance.hpp>

#include <boost/geometry/strategies/spherical/distance_haversine.hpp>
#include <boost/geometry/strategies/spherical/distance_cross_track.hpp>
#include <boost/geometry/strategies/concepts/distance_concept.hpp>
#include <boost/geometry/extensions/gis/geographic/strategies/andoyer.hpp>
#include <boost/geometry/extensions/gis/geographic/strategies/distance_cross_track.hpp>
#include <boost/geometry/geometries/point.hpp>
#include <boost/geometry/geometries/segment.hpp>


template <typename Point>
void test_distance(
            typename bg::coordinate_type<Point>::type const& lon1,
            typename bg::coordinate_type<Point>::type const& lat1,
            typename bg::coordinate_type<Point>::type const& lon2,
            typename bg::coordinate_type<Point>::type const& lat2,
            typename bg::coordinate_type<Point>::type const& lon3,
            typename bg::coordinate_type<Point>::type const& lat3,
            typename bg::coordinate_type<Point>::type const& radius,
            typename bg::coordinate_type<Point>::type const&expected,
            typename bg::coordinate_type<Point>::type const&tolerance)
{
    typedef typename bg::strategy::distance::services::default_strategy
        <
            bg::segment_tag, Point, Point
        >::type strategy_type;

    typedef typename bg::strategy::distance::services::return_type
        <
            strategy_type, Point, Point
        >::type return_type;


#if !defined(BOOST_MSVC)
    BOOST_CONCEPT_ASSERT
        (
            (bg::concept::PointSegmentDistanceStrategy<strategy_type, Point, Point>)
        );
#endif

    Point p1, p2, p3;
    bg::assign_values(p1, lon1, lat1);
    bg::assign_values(p2, lon2, lat2);
    bg::assign_values(p3, lon3, lat3);

    strategy_type strategy;
    return_type d = strategy.apply(p1, p2, p3);
    BOOST_CHECK_CLOSE(d, expected, tolerance);

    // Test the "default strategy" registration
    bg::model::referring_segment<Point const> segment(p2, p3);
    d = bg::distance(p1, segment);
    BOOST_CHECK_CLOSE(d, expected, tolerance);
}

template <typename Point>
void test_all()
{
    double const average_earth_radius = 6372795.0;

    // distance (Paris <-> Amsterdam/Barcelona),
    // with coordinates rounded as below ~87 km
    // should be is equal
    // to distance (Paris <-> Barcelona/Amsterdam)
    double p_to_ab = 86.8238 * 1000.0;
    test_distance<Point>(2, 48, 4, 52, 2, 41, average_earth_radius, p_to_ab, 0.1);

    // Note that for andoyer/geographic it is currently not symmetrical
    p_to_ab = 86.7186 * 1000.0;
    test_distance<Point>(2, 48, 2, 41, 4, 52, average_earth_radius, p_to_ab, 0.1);

    /*
        SQL Server queries:
            SELECT 'Am*dam-Barcelona - Paris' as dist, 0.001 * geography::STGeomFromText('LINESTRING(4.9 52.36667, 2.183333 41.383333)', 4326)
            .STDistance(geography::STGeomFromText('POINT(2.350833 48.856667)', 4326))
            union
            SELECT 'London-Vienna - Paris' as dist, 0.001 * geography::STGeomFromText('LINESTRING(-0.1275 51.507222, 16.373056 48.208333)', 4326)
            .STDistance(geography::STGeomFromText('POINT(2.350833 48.856667)', 4326))
        -> 112.377543374738 252.655192414418

        PostGis Queries:
            SELECT 'Am*dam-Barcelona - Paris' as dist, 0.001 * ST_Distance(ST_GeographyFromText('LINESTRING(4.9 52.36667, 2.183333 41.383333)')
            , ST_GeographyFromText('POINT(2.350833 48.856667)'))
            union
            SELECT 'London-Vienna - Paris' as dist, 0.001 * ST_Distance(ST_GeographyFromText('LINESTRING(-0.1275 51.507222, 16.373056 48.208333)')
            , ST_GeographyFromText('POINT(2.350833 48.856667)'))

        -> 112.377594693992, 252.655380438615

    */

    // There are small deviations from the result.
    // TODO: fix this, probably another strategy is necessary for ellipsoidal.
    test_distance<Point>(2.350833, 48.856667, 4.9, 52.36667, 2.183333, 41.383333, average_earth_radius, 112.377 * 1000.0, 0.25);
    test_distance<Point>(2.350833, 48.856667, -0.1275, 51.507222, 16.373056, 48.208333, average_earth_radius, 252.655 * 1000.0, 0.25);
}


int test_main(int, char* [])
{
    test_all<bg::model::point<double, 2, bg::cs::geographic<bg::degree> > >();

#if defined(HAVE_TTMATH)
    typedef ttmath::Big<1,4> tt;
    test_all<bg::model::point<tt, 2, bg::cs::geographic<bg::degree> > >();
#endif

    return 0;
}
