// Boost.Geometry (aka GGL, Generic Geometry Library)
// Unit Test

// Copyright (c) 2009-2012 Mateusz Loskot, London, UK.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#include <cstddef>

#include <boost/test/floating_point_comparison.hpp>
#include <boost/test/included/test_exec_monitor.hpp>

#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/geometries/point_xy.hpp>

#include <boost/geometry/extensions/index/rtree/rtree.hpp>

namespace bg = boost::geometry;

int test_main(int, char* [])
{

    // TODO: mloskot - This is ONLY compilation test and
    // placeholder to implement real test.

    bg::index::rtree<bg::model::box<bg::model::d2::point_xy<double> >, std::size_t> si(1, 6);
    return 0;
}
