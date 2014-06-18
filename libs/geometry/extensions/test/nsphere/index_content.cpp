// Boost.Geometry (aka GGL, Generic Geometry Library)
// Unit Test

// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.
// Copyright (c) 2013 Adam Wulkiewicz, Lodz, Poland.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#include <geometry_test_common.hpp>

#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/strategies/strategies.hpp>

#include <boost/geometry/extensions/nsphere/nsphere.hpp>

template <typename P, typename T>
void test_content_circle()
{
    bg::model::nsphere<P, T> c;

    bg::set<0>(c.center(), 0);
    bg::set<1>(c.center(), 0);
    c.radius(2);

    double d = bg::index::detail::content(c);
    BOOST_CHECK_CLOSE(d, 4 * 3.1415926535897932384626433832795, 0.001);
}

template <typename P, typename T>
void test_content_sphere()
{
    bg::model::nsphere<P, T> s;

    bg::set<0>(s, 0);
    bg::set<1>(s, 0);
    bg::set<2>(s, 0);
    bg::set_radius<0>(s, 2);

    double d = bg::index::detail::content(s);
    BOOST_CHECK_CLOSE(d, 32 * 3.1415926535897932384626433832795 / 3.0, 0.001);
}

int test_main(int, char* [])
{
    test_content_circle<bg::model::point<double, 2, bg::cs::cartesian>, double>();
    test_content_sphere<bg::model::point<double, 3, bg::cs::cartesian>, double>();
    return 0;
}
