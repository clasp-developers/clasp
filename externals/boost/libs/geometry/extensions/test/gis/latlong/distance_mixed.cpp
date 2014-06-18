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
#include <boost/test/unit_test.hpp>

#include <boost/geometry/algorithms/assign.hpp>

#include <boost/geometry/geometry.hpp>
#include <boost/geometry/extensions/gis/latlong/latlong.hpp>

#include <test_common/test_point.hpp>



int test_main(int, char* [])
{
    using namespace bg::strategy::distance;

    bg::model::ll::point<bg::degree> paris;
    paris.lat(bg::dms<bg::north>(48, 52, 0));
    paris.lon(bg::dms<bg::east>(2, 19, 59));

    bg::model::ll::point<bg::degree> amsterdam;
    amsterdam.lat(bg::dms<bg::north>(52, 22, 23));
    amsterdam.lon(bg::dms<bg::east>(4, 53, 32));

    bg::model::ll::point<bg::radian> paris_rad, amsterdam_rad;
    transform(amsterdam, amsterdam_rad);
    transform(paris, paris_rad);

    // Distance paris-amsterdam is about 430 km
    double expected = 429.984 * 1000.0;
    double tolerance = 0.001;

    // Combinations deg-deg, rad-rad, deg-rad, rad-de
    BOOST_CHECK_CLOSE(distance(paris, amsterdam), expected, tolerance);
    BOOST_CHECK_CLOSE(distance(paris_rad, amsterdam_rad), expected, tolerance);
    BOOST_CHECK_CLOSE(distance(paris, amsterdam_rad), expected, tolerance);
    BOOST_CHECK_CLOSE(distance(paris_rad, amsterdam), expected, tolerance);

    // With specified strategy
    vincenty<double> the_strategy;
    BOOST_CHECK_CLOSE(distance(paris, amsterdam, the_strategy), expected, tolerance);
    BOOST_CHECK_CLOSE(distance(paris_rad, amsterdam_rad, the_strategy), expected, tolerance);
    BOOST_CHECK_CLOSE(distance(paris, amsterdam_rad, the_strategy), expected, tolerance);
    BOOST_CHECK_CLOSE(bg::distance(paris_rad, amsterdam, the_strategy), expected, tolerance);


    // Distance point-linestring, linestring-point...
    bg::model::ll::point<bg::degree> barcelona(
        bg::latitude<>(bg::dms<bg::north>(41, 23)),
        bg::longitude<>(bg::dms<bg::east>(2, 11))
        );

    bg::model::linestring<bg::model::ll::point<bg::degree> > ab;
    ab.push_back(amsterdam);
    ab.push_back(barcelona);

    // Distance paris to line amsteram-barcelona is about 113 km
    expected = 113.168 * 1000.0;

    BOOST_CHECK_CLOSE(distance(ab, paris), expected, tolerance);
    BOOST_CHECK_CLOSE(distance(paris, ab), expected, tolerance);
    BOOST_CHECK_CLOSE(distance(paris, ab, the_strategy), expected, tolerance);
    BOOST_CHECK_CLOSE(distance(ab, paris, the_strategy), expected, tolerance);

    // line-type in degrees, point-type in radians (supported since new distance-strategy approach)
    BOOST_CHECK_CLOSE(distance(ab, paris_rad, the_strategy), expected, tolerance);

    return 0;
}
