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

#if defined(_MSC_VER)
// We deliberately mix float/double's here so turn off warning
#pragma warning( disable : 4305 )
#endif // defined(_MSC_VER)

#include <string>

#include <geometry_test_common.hpp>

#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/extensions/algorithms/selected.hpp>
#include <boost/geometry/io/wkt/read.hpp>


template <typename G, typename P>
void test_selected(G const& g, P const& point, bool result, double dist)
{
    bool sel = bg::selected(g, point, dist);
    BOOST_CHECK_EQUAL(sel, result);
}

template <typename G, typename P>
void test_selected(std::string const& wkt, P const& point, bool result, double dist)
{
    G g;
    bg::read_wkt(wkt, g);
    test_selected(g, point, result, dist);
}

template <typename P>
void test_all()
{
    test_selected<P>("POINT(1 1)", P(1,1), true, 0.001);
    test_selected<P>("POINT(1 1)", P(3,3), false, 2);
    test_selected<P>("POINT(1 1)", P(1,2.00001), false, 1);
    test_selected<P>("POINT(1 1)", P(1,1.99999), true, 1);
    test_selected<P>("POINT(1 1)", P(1.99999,1.99999), false, 1);

    test_selected<bg::model::linestring<P> >("LINESTRING(1 1,2 2)", P(1,1), true, 0.0001);
    test_selected<bg::model::linestring<P> >("LINESTRING(1 1,2 2)", P(2,2), true, 0.0001);
    test_selected<bg::model::linestring<P> >("LINESTRING(1 1,2 2)", P(2.01,2.01), false, 0.0001);
    test_selected<bg::model::linestring<P> >("LINESTRING(1 1,2 2)", P(1,0.9), false, 0.0001);
    test_selected<bg::model::linestring<P> >("LINESTRING(1 1,2 2)", P(1.5,1.5), true, 0.0001);
    test_selected<bg::model::linestring<P> >("LINESTRING(1 1,2 2)", P(1.5,1.6), false, 0.0001);
    test_selected<bg::model::linestring<P> >("LINESTRING(1 1,2 2,3 0,5 0,5 8)", P(5,5.000001), true, 0.0001);

    // Lines with zero,one points
    test_selected<bg::model::linestring<P> >("LINESTRING( )", P(1,1), false, 0.0001);
    test_selected<bg::model::linestring<P> >("LINESTRING(1 1)", P(1,1), true, 0.0001);
    test_selected<bg::model::linestring<P> >("LINESTRING(1 2)", P(1,1), false, 0.0001);

    // nyi
    //test_selected<bg::model::ring<P> >();

    test_selected<bg::model::polygon<P> >("POLYGON((0 0,0 7,4 2,2 0,0 0))", P(0.001, 0.001), true, 0.0001);
    test_selected<bg::model::polygon<P> >("POLYGON((0 0,0 7,4 2,2 0,0 0))", P(1, 1), true, 0.0001);
    test_selected<bg::model::polygon<P> >("POLYGON((0 0,0 7,4 2,2 0,0 0))", P(2, 5), false, 0.0001);

    typedef bg::model::box<P> B;
    test_selected(bg::make<B>(0,0,4,4), P(2,2), true, 0.001);
    test_selected(bg::make<B>(0,0,4,4), P(5,5), false, 0.001);
    test_selected(bg::make<B>(0,0,4,4), P(0,0), false, 0.001);
    test_selected(bg::make<B>(0,0,4,4), P(4,4), false, 0.001);

    // nyi
    //test_selected<bg::segment<P> >();
    //test_selected<bg::segment<const P> >();
}

int test_main(int, char* [])
{
    // Integer not applicable here, just because of the tests using floating point
    // test_all<bg::model::d2::point_xy<int> >();

    test_all<bg::model::d2::point_xy<float> >();
    test_all<bg::model::d2::point_xy<double> >();

    return 0;
}
