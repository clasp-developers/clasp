// Boost.Geometry (aka GGL, Generic Geometry Library)
// Unit Test

// Copyright (c) 2007-2013 Barend Gehrels, Amsterdam, the Netherlands.
// Copyright (c) 2008-2013 Bruno Lalande, Paris, France.
// Copyright (c) 2009-2013 Mateusz Loskot, London, UK.
// Copyright (c) 2013 Adam Wulkiewicz, Lodz, Poland.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#include <string>

#include <geometry_test_common.hpp>

#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/algorithms/equals.hpp>
#include <boost/geometry/algorithms/make.hpp>
#include <boost/geometry/extensions/algorithms/distance_info.hpp>
#include <boost/geometry/io/wkt/wkt.hpp>


template <typename Result>
void check_distance_info(Result const& result,
                          std::string const& expected_pp,
                          bool expected_on_segment,
                          double expected_projected_distance,
                          double expected_real_distance,
                          double expected_fraction)
{
    if (! expected_pp.empty())
    {
        std::ostringstream out;
        out << bg::wkt(result.projected_point1);
        std::string wkt_projected = out.str();

        BOOST_CHECK_EQUAL(wkt_projected, expected_pp);
    }
    BOOST_CHECK_EQUAL(result.on_segment, expected_on_segment);
    BOOST_CHECK_CLOSE(result.fraction1, expected_fraction, 0.001);
    BOOST_CHECK_CLOSE(result.projected_distance1, expected_projected_distance, 0.001);
    BOOST_CHECK_CLOSE(result.real_distance, expected_real_distance, 0.001);
}

template <typename Geometry1, typename Geometry2>
void test_distance_info(Geometry1 const& geometry1, Geometry2 const& geometry2,
                          std::string const& expected_pp,
                          bool expected_on_segment,
                          double expected_projected_distance,
                          double expected_real_distance,
                          double expected_fraction)
{
    typename bg::distance_info_result<typename bg::point_type<Geometry1>::type> result, reversed_result;
    bg::distance_info(geometry1, geometry2, result);
    check_distance_info(result, 
                expected_pp, expected_on_segment,
                expected_projected_distance, expected_real_distance,
                expected_fraction);

    // Check reversed version too.
    std::string reversed_expected_pp = expected_pp;
    if (boost::is_same<typename bg::tag<Geometry1>::type, bg::point_tag>::value
        && boost::is_same<typename bg::tag<Geometry2>::type, bg::point_tag>::value
        )
    {
        // For point-point, we cannot check projected-point again, it is also the other one.
        reversed_expected_pp.clear();
    }
    bg::distance_info(geometry2, geometry1, reversed_result);
    check_distance_info(reversed_result, 
                reversed_expected_pp,
                expected_on_segment,
                expected_projected_distance, expected_real_distance,
                expected_fraction);
}

template <typename Geometry1>
void test_distance_info(std::string const& wkt, std::string const& wkt_point,
                          std::string const& expected_pp,
                          bool expected_on_segment,
                          double expected_projected_distance,
                          double expected_real_distance,
                          double expected_fraction)
{
    Geometry1 geometry1;
    typename bg::point_type<Geometry1>::type point;
    bg::read_wkt(wkt, geometry1);
    bg::read_wkt(wkt_point, point);

    test_distance_info(geometry1, point, expected_pp, expected_on_segment,
                expected_projected_distance, expected_real_distance,
                expected_fraction);
}

template <typename P>
void test_2d()
{
    test_distance_info<bg::model::segment<P> >("LINESTRING(2 0,4 0)", "POINT(3 2)", "POINT(3 0)", true, 2.0, 2.0, 0.5);
    test_distance_info<bg::model::segment<P> >("LINESTRING(2 0,4 0)", "POINT(2 0)", "POINT(2 0)", true, 0.0, 0.0, 0.0);
    test_distance_info<bg::model::segment<P> >("LINESTRING(2 0,4 0)", "POINT(4 0)", "POINT(4 0)", true, 0.0, 0.0, 1.0);
    test_distance_info<bg::model::segment<P> >("LINESTRING(2 0,4 0)", "POINT(5 2)", "POINT(5 0)", false, 2.0, sqrt(5.0), 1.5);
    test_distance_info<bg::model::segment<P> >("LINESTRING(2 0,4 0)", "POINT(0 2)", "POINT(0 0)", false, 2.0, sqrt(8.0), -1.0);

    // Degenerated segment
    test_distance_info<bg::model::segment<P> >("LINESTRING(2 0,2 0)", "POINT(4 0)", "POINT(2 0)", false, 2.0, 2.0, 0.0);

    // Linestring
    test_distance_info<bg::model::linestring<P> >("LINESTRING(2 0,4 0)", "POINT(3 2)", "POINT(3 0)", true, 2.0, 2.0, 0.5);


    // Point-point
    test_distance_info<P>("Point(1 1)", "POINT(2 2)", "POINT(2 2)", false, sqrt(2.0), sqrt(2.0), 0.0);
}

template <typename P>
void test_3d()
{
    test_distance_info<bg::model::segment<P> >("LINESTRING(0 0 0,5 5 5)", "POINT(2 3 4)", "POINT(3 3 3)", true, sqrt(2.0), sqrt(2.0), 0.6);
}

int test_main(int, char* [])
{
    test_2d<bg::model::d2::point_xy<int> >();
    test_2d<bg::model::d2::point_xy<float> >();
    test_2d<bg::model::d2::point_xy<double> >();

    test_3d<bg::model::point<double, 3, bg::cs::cartesian> >();

    return 0;
}
