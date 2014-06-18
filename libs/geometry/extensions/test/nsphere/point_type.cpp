// Boost.Geometry (aka GGL, Generic Geometry Library)
// Unit Test

// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)


#include <geometry_test_common.hpp>

#include <boost/geometry/extensions/nsphere/nsphere.hpp>
#include <boost/geometry/multi/core/point_type.hpp>


template <typename G, typename Expected>
void test_geometry()
{
    BOOST_CHECK_EQUAL(typeid(typename bg::point_type<G>::type).name(),
        typeid(Expected).name());
}

template <typename P>
void test_all()
{
    test_geometry<bg::model::nsphere<P, double> , P>();
}

int test_main(int, char* [])
{
    test_all<bg::model::point<int, 2, bg::cs::cartesian> >();
    test_all<bg::model::point<float, 2, bg::cs::cartesian> >();
    test_all<bg::model::point<double, 2, bg::cs::cartesian> >();

    return 0;
}
