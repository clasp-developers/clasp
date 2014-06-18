// Boost.Geometry (aka GGL, Generic Geometry Library)
// Unit Test

// Copyright (c) 2009-2012 Mateusz Loskot, London, UK.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#include <iterator>
#include <string>
#include <vector>

#include <boost/test/included/test_exec_monitor.hpp>
#include <boost/test/included/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>
#include <boost/cstdint.hpp>

#include <boost/geometry/algorithms/equals.hpp>
#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/io/wkt/read.hpp>
#include <boost/geometry/extensions/gis/io/wkb/read_wkb.hpp>
#include <boost/geometry/extensions/gis/io/wkb/utility.hpp>

namespace bg = boost::geometry;

namespace { // anonymous

typedef std::vector<boost::uint8_t> byte_vector;

template <typename Geometry>
void test_geometry_wrong_wkb(std::string const& wkbhex, std::string const& wkt)
{
    byte_vector wkb;
    BOOST_CHECK( bg::hex2wkb(wkbhex, std::back_inserter(wkb)) );
    Geometry g_wkb;
    std::cout << bg::read_wkb(wkb.begin(), wkb.end(), g_wkb) << std::endl;
}

template <typename Geometry, bool IsEqual>
void test_geometry_equals(std::string const& wkbhex, std::string const& wkt)
{
    byte_vector wkb;
    BOOST_CHECK( bg::hex2wkb(wkbhex, std::back_inserter(wkb)) );
    Geometry g_wkb;
    BOOST_CHECK( bg::read_wkb(wkb.begin(), wkb.end(), g_wkb) );

    Geometry g_expected;
    bg::read_wkt(wkt, g_expected);
    BOOST_CHECK( bg::equals(g_wkb, g_expected) == IsEqual );
}

//template <typename P, bool Result>
//void test_polygon_wkt(std::string const& wkt)
//{
//    typedef bg::model::linestring<P> linestring_type;
//    typedef bg::model::polygon<linestring_type> polygon_type;
//
//    polygon_type poly;
//    bg::read_wkb(wkb, poly);
//}

} // namespace anonymous

int test_main(int, char* [])
{
    typedef bg::model::point<double, 2, bg::cs::cartesian> point_type;
    typedef bg::model::linestring<point_type> linestring_type;

    //
    // POINT
    //

    test_geometry_equals<point_type, true>(
        "01010000005839B4C876BEF33F83C0CAA145B61640", "POINT (1.234 5.678)");

    // XYZ - POINT(1.234 5.678 99) - Z coordinate ignored
    test_geometry_equals<point_type, true>(
        "01010000805839B4C876BEF33F83C0CAA145B616400000000000C05840", "POINT(1.234 5.678)");

    // SRID=32632;POINT(1.234 5.678) - PostGIS EWKT
    test_geometry_equals<point_type, false>(
        "0101000020787F00005839B4C876BEF33F83C0CAA145B61640", "POINT (1.234 5.678)");

    // SRID=4326;POINT(1.234 5.678 99) - PostGIS EWKT
    test_geometry_equals<point_type, false>(
        "01010000A0E61000005839B4C876BEF33F83C0CAA145B616400000000000C05840", "POINT(1.234 5.678)");

    // POINTM(1.234 5.678 99) - XYM with M compound ignored
    test_geometry_equals<point_type, true>(
        "01010000405839B4C876BEF33F83C0CAA145B616400000000000C05840", "POINT (1.234 5.678)");

    // SRID=32632;POINTM(1.234 5.678 99)
    test_geometry_equals<point_type, false>(
        "0101000060787F00005839B4C876BEF33F83C0CAA145B616400000000000C05840", "POINT (1.234 5.678)");

    // POINT(1.234 5.678 15 79) - XYZM - Z and M compounds ignored
    test_geometry_equals<point_type, true>(
        "01010000C05839B4C876BEF33F83C0CAA145B616400000000000002E400000000000C05340",
        "POINT (1.234 5.678)");

    // SRID=4326;POINT(1.234 5.678 15 79) - XYZM + SRID
    test_geometry_equals<point_type, false>(
        "01010000E0E61000005839B4C876BEF33F83C0CAA145B616400000000000002E400000000000C05340",
        "POINT (1.234 5.678)");

    return 0;
}
