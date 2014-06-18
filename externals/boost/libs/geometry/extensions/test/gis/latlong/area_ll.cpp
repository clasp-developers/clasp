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

#include <boost/geometry/algorithms/area.hpp>
#include <boost/geometry/algorithms/transform.hpp>
#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/io/wkt/read.hpp>
#include <boost/geometry/strategies/strategies.hpp>

#include <boost/geometry/extensions/gis/latlong/point_ll.hpp>
#include <boost/geometry/extensions/gis/geographic/strategies/area_huiller_earth.hpp>
#include <boost/geometry/extensions/gis/projections/epsg.hpp>
#include <boost/geometry/extensions/gis/projections/parameters.hpp>
#include <boost/geometry/extensions/gis/projections/proj/sterea.hpp>

//#include <test_common/test_point.hpp>

template <typename PRJ, typename XY, typename LL>
void add_to_ring(PRJ const& prj, LL const& ll,
                 bg::model::ring<LL>& ring_ll,
                 bg::model::ring<XY>& ring_xy)
{
    ring_ll.push_back(ll);

    XY xy;
    prj.forward(ll, xy);
    ring_xy.push_back(xy);
}

template <typename XY, typename LL>
void test_area_polygon_ll(bool concave, bool hole, double perc)
{
    BOOST_ASSERT(! (concave && hole) );

    typedef typename bg::coordinate_type<LL>::type T;

    // Amsterdam, Rotterdam, The Hague, Utrecht,
    // these cities together are the city group "Randstad"

    LL a, r, h, u;
    // Amsterdam 52 22'23"N 4 53'32"E
    a.lat(bg::dms<bg::north, T>(52, 22, 23));
    a.lon(bg::dms<bg::east, T>(4, 53, 32));

    // Rotterdam 51 55'51"N 4 28'45"E
    r.lat(bg::dms<bg::north, T>(51, 55, 51));
    r.lon(bg::dms<bg::east, T>(4, 28, 45));

    // The hague: 52 4' 48" N, 4 18' 0" E
    h.lat(bg::dms<bg::north, T>(52, 4, 48));
    h.lon(bg::dms<bg::east, T>(4, 18, 0));

    // Utrecht
    u.lat(bg::dms<bg::north, T>(52, 5, 36));
    u.lon(bg::dms<bg::east, T>(5, 7, 10));


    // For checking calculated area, use the Dutch projection (RD), this is EPSG code 28992
    bg::projections::sterea_ellipsoid<LL, XY> dutch_prj(bg::projections::init(28992));

    // Add them in clockwise direction
    bg::model::polygon<LL> randstad;
    bg::model::polygon<XY> randstad_xy;
    add_to_ring(dutch_prj, a, randstad.outer(), randstad_xy.outer());
    add_to_ring(dutch_prj, u, randstad.outer(), randstad_xy.outer());

    // Concave case
    if (concave)
    {
        // Add the city "Alphen" to create a concave case
        // Alphen 52 7' 48" N, 4 39' 0" E
        LL alphen(
            bg::latitude<T>(bg::dms<bg::north, T>(52, 7, 48)),
            bg::longitude<T>(bg::dms<bg::east, T>(4, 39)));
        add_to_ring(dutch_prj, alphen, randstad.outer(), randstad_xy.outer());
    }

    add_to_ring(dutch_prj, r, randstad.outer(), randstad_xy.outer());
    add_to_ring(dutch_prj, h, randstad.outer(), randstad_xy.outer());
    add_to_ring(dutch_prj, a, randstad.outer(), randstad_xy.outer());

    // Hole case
    if (hole)
    {
        // Gouda 52 1' 12" N, 4 42' 0" E
        LL gouda(
            bg::latitude<T>(bg::dms<bg::north, T>(52, 1, 12)),
            bg::longitude<T>(bg::dms<bg::east, T>(4, 42)));
        // Woerden 52 5' 9" N, 4 53' 0" E
        LL woerden(
            bg::latitude<T>(bg::dms<bg::north, T>(52, 5, 9)),
            bg::longitude<T>(bg::dms<bg::east, T>(4, 53, 0)));
        // Uithoorn 52 13' 48" N, 4 49' 48" E
        LL uithoorn(bg::latitude<T>
            (bg::dms<bg::north, T>(52, 13, 48)),
            bg::longitude<T>(bg::dms<bg::east, T>(4, 49, 48)));
        // Alphen 52 7' 48" N, 4 39' 0" E
        LL alphen(bg::latitude<T>(
            bg::dms<bg::north, T>(52, 7, 48)),
            bg::longitude<T>(bg::dms<bg::east, T>(4, 39)));

        randstad.inners().resize(1);
        randstad_xy.inners().resize(1);

        typename bg::model::polygon<LL>::ring_type& ring = randstad.inners()[0];
        typename bg::model::polygon<XY>::ring_type& ring_xy = randstad_xy.inners()[0];

        // Add them in counter-clockwise direction (see map of the Netherlands)
        add_to_ring(dutch_prj, gouda, ring, ring_xy);
        add_to_ring(dutch_prj, woerden, ring, ring_xy);
        add_to_ring(dutch_prj, uithoorn, ring, ring_xy);
        add_to_ring(dutch_prj, alphen, ring, ring_xy);
        add_to_ring(dutch_prj, gouda, ring, ring_xy);
    }


    // Check the area in square KM
    static const double KM2 = 1.0e6;
    double d_ll = bg::area(randstad) / KM2;
    double d_xy = bg::area(randstad_xy) / KM2;

    BOOST_CHECK_CLOSE(d_ll, d_xy, 1.0);
    if (hole)
    {
        BOOST_CHECK_CLOSE(d_ll, 1148.210, perc);
        BOOST_CHECK_CLOSE(d_xy, 1151.573, perc);
    }
    else
    {
        BOOST_CHECK_CLOSE(d_ll, concave ? 977.786 : 1356.168, perc);
        BOOST_CHECK_CLOSE(d_xy, concave ? 980.658 : 1360.140, perc);

        // No hole: area of outer should be equal to area of ring
        double r_ll = bg::area(randstad.outer()) / KM2;
        double r_xy = bg::area(randstad_xy.outer()) / KM2;

        BOOST_CHECK_CLOSE(d_ll, r_ll, perc);
        BOOST_CHECK_CLOSE(d_xy, r_xy, perc);
    }

    // Calculate are using specified strategy, here with radius in KM
    // We then don't have to divide by KM*KM to get the same result
    bg::strategy::area::huiller<LL, long double> strategy(6372.8);
    d_ll = bg::area(randstad, strategy);
    BOOST_CHECK_CLOSE(d_ll, d_xy, 1.0);
}



template <typename T>
void test_latlong(double perc)
{
    test_area_polygon_ll<bg::model::d2::point_xy<T>, bg::model::ll::point<bg::degree, T> >(false, false, perc);

    // with concavities
    test_area_polygon_ll<bg::model::d2::point_xy<T>, bg::model::ll::point<bg::degree, T> >(true, false, perc);

    // with holes
    test_area_polygon_ll<bg::model::d2::point_xy<T>, bg::model::ll::point<bg::degree, T> >(false, true, perc);

}

int test_main(int, char* [])
{
    test_latlong<double>(0.01);
    //test_latlong<float>(0.3); // LL area calculations using projections differ

    return 0;
}
