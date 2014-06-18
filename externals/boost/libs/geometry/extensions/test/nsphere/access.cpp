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


#include <boost/geometry/extensions/nsphere/nsphere.hpp>
#include <boost/geometry/geometries/adapted/boost_tuple.hpp>
#include <boost/geometry/geometries/adapted/c_array.hpp>
#include <boost/geometry/geometries/point.hpp>
#include <boost/tuple/tuple.hpp>



template <typename G>
void test_get_set()
{
    typedef typename bg::coordinate_type<G>::type coordinate_type;

    G g;
    bg::set<0>(g, coordinate_type(1));
    bg::set<1>(g, coordinate_type(2));

    coordinate_type x = bg::get<0>(g);
    coordinate_type y = bg::get<1>(g);

    BOOST_CHECK_CLOSE(double(x), 1.0, 0.0001);
    BOOST_CHECK_CLOSE(double(y), 2.0, 0.0001);
}


template <typename P>
void test_all()
{
    typedef typename bg::coordinate_type<P>::type coordinate_type;

    // N-SPHERE, setting sphere center
    test_get_set<bg::model::nsphere<P, coordinate_type> >();

}


int test_main(int, char* [])
{
    test_get_set<int[2]>();
    test_get_set<float[2]>();
    test_get_set<double[2]>();
    test_get_set<double[3]>();

    test_get_set<boost::tuple<double, double> >();

    test_all<bg::model::point<int, 2, bg::cs::cartesian> >();
    test_all<bg::model::point<float, 2, bg::cs::cartesian> >();
    test_all<bg::model::point<double, 2, bg::cs::cartesian> >();

    return 0;
}
