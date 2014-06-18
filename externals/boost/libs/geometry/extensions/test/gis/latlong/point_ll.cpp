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

#include <boost/geometry/algorithms/transform.hpp>
#include <boost/geometry/io/wkt/wkt.hpp>
#include <boost/geometry/strategies/strategies.hpp>

#include <boost/geometry/extensions/gis/latlong/point_ll.hpp>


#include <test_common/test_point.hpp>

template <typename P>
void test_all()
{
    typedef typename bg::coordinate_type<P>::type type;

    P p1(bg::latitude<type>(bg::dms<bg::south, type>(12, 2, 36)),
         bg::longitude<type>(bg::dms<bg::west, type>(77, 1, 42)));

    // Check decimal/degree conversion
    BOOST_CHECK_CLOSE(bg::get<0>(p1), type(-77.0283), 0.001);
    BOOST_CHECK_CLOSE(bg::get<1>(p1), type(-12.0433), 0.001);

    // Check degree/radian conversion
    bg::model::ll::point<bg::radian, type> p2;
    bg::transform(p1, p2);

    BOOST_CHECK_CLOSE(bg::get<0>(p2), type(-1.3444), 0.001);
    BOOST_CHECK_CLOSE(bg::get<1>(p2), type(-0.210196), 0.001);

    // Check degree/radian conversion back
    P p3;
    bg::transform(p2, p3);
    BOOST_CHECK_CLOSE(bg::get<0>(p3), type(-77.0283), 0.001);
    BOOST_CHECK_CLOSE(bg::get<1>(p3), type(-12.0433), 0.001);


    // Check decimal/degree conversion back
    int d;
    int m;
    double s;
    bool positive;
    char cardinal;

    bg::dms<bg::cd_lat, type> d1(bg::get<0>(p3));
    d1.get_dms(d, m, s, positive, cardinal);

    BOOST_CHECK(d == 77);
    BOOST_CHECK(m == 1);
    BOOST_CHECK_CLOSE(s, double(42), 0.1);
    BOOST_CHECK(positive == false);
    BOOST_CHECK(cardinal == 'S');

    // Check dd conversion as string, back. We cannot do that always because of the precision.
    // Only double gives correct results
    //std::string st = d1.get_dms();
    //std::cout << st << std::endl;
    //BOOST_CHECK(st == "77 1'42\" S");
}

int test_main(int, char* [])
{
    test_all<bg::model::ll::point<bg::degree, float> >();
    test_all<bg::model::ll::point<bg::degree, double> >();

    return 0;
}
