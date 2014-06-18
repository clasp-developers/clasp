// Boost.Geometry (aka GGL, Generic Geometry Library)
//
// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#include <geometry_test_common.hpp>

#include <boost/geometry/extensions/nsphere/nsphere.hpp>

#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/geometries/point_xy.hpp>

#include <boost/geometry/strategies/strategies.hpp>

#include <boost/geometry/io/wkt/read.hpp>


template <typename Geometry>
void test_circle(std::string const& wkt_geometry, bool expected)
{
    typedef bg::model::nsphere<bg::model::d2::point_xy<double>, double> circle_type;
    circle_type circle;
    bg::assign_values(circle, 1.0, 1.0, 3.0);

    Geometry geometry;
    bg::read_wkt(wkt_geometry, geometry);

    bool detected = bg::within(geometry, circle);

    BOOST_CHECK_MESSAGE(detected == expected,
        "within: " << wkt_geometry
        << " in circle (1,1) with radius 3"
        << " -> Expected: " << expected
        << " detected: " << detected);
}

template <typename P>
void test_circles()
{
    test_circle<P>("POINT(2 1)", true);
    test_circle<P>("POINT(12 1)", false);

    test_circle<bg::model::linestring<P> >("LINESTRING(1 1,2 1,2 2)", true);
    test_circle<bg::model::linestring<P> >("LINESTRING(1 1,2 1,2 2,10 10)", false);

    test_circle<bg::model::polygon<P> >("POLYGON((1 1,2 1,2 2,1 1))", true);
    test_circle<bg::model::polygon<P> >("POLYGON((1 1,2 1,2 2,10 10,1 1))", false);

    test_circle<bg::model::box<P> >("BOX(1 1,2 2)", true);
    test_circle<bg::model::box<P> >("BOX(1 1,10 10)", false);
}


int test_main( int , char* [] )
{
    test_circles<bg::model::d2::point_xy<double> >();

    return 0;
}
