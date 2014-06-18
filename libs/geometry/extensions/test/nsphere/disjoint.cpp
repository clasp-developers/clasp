// Boost.Geometry (aka GGL, Generic Geometry Library)
//
// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.
// Copyright (c) 2013 Adam Wulkiewicz, Lodz, Poland.
// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#include <geometry_test_common.hpp>

#include <boost/geometry/extensions/nsphere/nsphere.hpp>

#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/geometries/point_xy.hpp>

#include <boost/geometry/strategies/strategies.hpp>

#include <boost/geometry/io/wkt/read.hpp>

typedef bg::model::d2::point_xy<double> point_type;
typedef bg::model::box<point_type> box_type;
typedef bg::model::nsphere<point_type, double> circle_type;

template <typename Geometry>
void test_circle(Geometry const& geometry, std::string const& wkt_geometry, bool expected)
{
    circle_type circle(point_type(1.0, 1.0), 3.0);

    bool detected = bg::disjoint(geometry, circle);

    BOOST_CHECK_MESSAGE(detected == expected,
        "disjoint: " << wkt_geometry
        << " in circle (1,1) with radius 3"
        << " -> Expected: " << expected
        << " detected: " << detected);
}

template <typename Geometry>
void test_circle(std::string const& wkt_geometry, bool expected)
{
    Geometry geometry;
    bg::read_wkt(wkt_geometry, geometry);

    test_circle<Geometry>(geometry, wkt_geometry, expected);
}

int test_main( int , char* [] )
{
    test_circle<point_type>("POINT(2 1)", false);
    test_circle<point_type>("POINT(12 1)", true);

    test_circle<box_type>("BOX(2 1, 4 4)", false);
    test_circle<box_type>("BOX(-3 -3, -2 -2)", true);

    test_circle<circle_type>(circle_type(point_type(4, 4), 2), "CIRCLE(4 4, 2)", false);
    test_circle<circle_type>(circle_type(point_type(4, 4), 0.5), "CIRCLE(4 4, 0.5)", true);

    return 0;
}
