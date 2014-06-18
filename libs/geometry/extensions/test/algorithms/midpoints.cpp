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

#include <boost/geometry/extensions/algorithms/midpoints.hpp>
#include <boost/geometry/algorithms/make.hpp>
#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/geometries/point_xy.hpp>

#include <boost/geometry/io/dsv/write.hpp>


#include <boost/geometry/io/wkt/wkt.hpp>


template <typename G>
void test_midpoints(G const& g, bool start_and_end)
{
    G processed;
    bg::midpoints(g, start_and_end, std::back_inserter(processed));
    std::cout << bg::dsv(processed) << std::endl;
}

template <typename G>
void test_midpoints(std::string const& wkt)
{
    G g;
    bg::read_wkt(wkt, g);
    test_midpoints(g, true);
    test_midpoints(g, false);
}

template <typename P>
void test_all()
{
    test_midpoints<bg::model::linestring<P> >("LINESTRING(1 1,2 2,3 3)");
}

int test_main(int, char* [])
{
    //test_all<bg::model::d2::point_xy<float> >();
    test_all<bg::model::d2::point_xy<double> >();

    return 0;
}
