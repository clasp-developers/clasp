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

#include <iostream>
#include <iterator>
#include <string>


#include <geometry_test_common.hpp>

#include <boost/geometry/algorithms/num_points.hpp>
#include <boost/geometry/extensions/algorithms/remove_holes_if.hpp>
#include <boost/geometry/algorithms/make.hpp>
#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/io/wkt/read.hpp>

#include <boost/geometry/strategies/strategies.hpp>



template <typename G, typename Predicate>
void test_remove_holes_if(std::string const& wkt, Predicate const& predicate,
            int expected_points)
{
    G g;
    bg::read_wkt(wkt, g);
    bg::remove_holes_if(g, predicate);
    BOOST_CHECK_EQUAL(bg::num_points(g), expected_points);
}

template <typename P>
void test_all()
{
    bg::elongated_hole<bg::model::ring<P> > elongated(0.05);

    // No holes
    test_remove_holes_if<bg::model::polygon<P> >("POLYGON((0 0,0 4,4 4,4 0,0 0))", elongated, 5);

    // Square hole (ratio 1/4), kept
    test_remove_holes_if<bg::model::polygon<P> >("POLYGON((0 0,0 4,4 4,4 0,0 0), (1 1,1 2,2 2,2 1,1 1))", elongated, 10);

    // Elongated hole
    test_remove_holes_if<bg::model::polygon<P> >("POLYGON((0 0,0 4,4 4,4 0,0 0), (1 1,1 2,1.02 2,1.02 1,1 1))", elongated, 5);

    // Invalid hole - removed by "elongated" predicate as well
    test_remove_holes_if<bg::model::polygon<P> >("POLYGON((0 0,0 4,4 4,4 0,0 0), (1 1,1 2))", elongated, 5);

    // Invalid hole
    bg::invalid_hole<bg::model::ring<P> > invalid;
    test_remove_holes_if<bg::model::polygon<P> >("POLYGON((0 0,0 4,4 4,4 0,0 0), (1 1,1 2))", invalid, 5);

    // Valid hole
    test_remove_holes_if<bg::model::polygon<P> >("POLYGON((0 0,0 4,4 4,4 0,0 0), (1 1,1 2,1.02 2,1.02 1,1 1))", invalid, 10);

}

int test_main(int, char* [])
{
    //test_all<bg::model::d2::point_xy<float> >();
    test_all<bg::model::d2::point_xy<double> >();

    return 0;
}
