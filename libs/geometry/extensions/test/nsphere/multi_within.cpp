// Boost.Geometry (aka GGL, Generic Geometry Library)
//
// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#include <geometry_test_common.hpp>


#include <boost/geometry/algorithms/correct.hpp>
#include <boost/geometry/algorithms/within.hpp>
#include <boost/geometry/core/cs.hpp>
#include <boost/geometry/geometries/box.hpp>
#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/geometries/ring.hpp>
#include <boost/geometry/geometries/linestring.hpp>
#include <boost/geometry/geometries/point.hpp>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/geometries/polygon.hpp>
#include <boost/geometry/multi/algorithms/within.hpp>
#include <boost/geometry/multi/core/point_type.hpp>
#include <boost/geometry/multi/geometries/multi_geometries.hpp>
#include <boost/geometry/strategies/strategies.hpp>
#include <boost/geometry/extensions/nsphere/nsphere.hpp>

int test_main( int , char* [] )
{
    typedef bg::model::d2::point_xy<double> gl_point;
    typedef bg::model::nsphere<gl_point, double> gl_circle;
    typedef bg::model::ring<gl_point> gl_ring;
    typedef bg::model::polygon<gl_point> gl_polygon;
    typedef bg::model::multi_polygon<gl_polygon> gl_multi_polygon;

    gl_circle circle(gl_point(1, 1), 2.5);

    gl_ring ring;
    ring.push_back(gl_point(0,0));
    ring.push_back(gl_point(1,0));
    ring.push_back(gl_point(1,1));
    ring.push_back(gl_point(0,1));
    bg::correct(ring);

    gl_polygon pol;
    pol.outer() = ring;
    gl_multi_polygon multi_polygon;
    multi_polygon.push_back(pol);

    // Multipolygon in circle
    BOOST_CHECK_EQUAL(bg::within(multi_polygon, circle), true);

    multi_polygon.front().outer().insert(multi_polygon.front().outer().begin() + 1, gl_point(10, 10));
    BOOST_CHECK_EQUAL(bg::within(multi_polygon, circle), false);

    return 0;
}
