// Boost.Geometry (aka GGL, Generic Geometry Library)
// Unit Test

// Copyright (c) 2010-2012 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#include <iostream>
#include <string>

#define TEST_WITH_SVG

#include <geometry_test_common.hpp>

#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/geometries/point_xy.hpp>

#include <boost/geometry/algorithms/length.hpp>
#include <boost/geometry/algorithms/num_points.hpp>
#include <boost/geometry/algorithms/unique.hpp>
#include <boost/geometry/extensions/algorithms/offset.hpp>
#include <boost/geometry/multi/io/wkt/read.hpp>

#include <boost/geometry/strategies/strategies.hpp>

#include <boost/geometry/extensions/strategies/buffer.hpp>


#if defined(TEST_WITH_SVG)
#  include <boost/geometry/multi/algorithms/envelope.hpp>
#  include <boost/geometry/io/svg/svg_mapper.hpp>
#endif


template <typename GeometryOut, typename Geometry>
void test_offset(std::string const& caseid, Geometry const& geometry,
        double distance,
        double expected_length, double percentage)
{
    typedef typename bg::coordinate_type<Geometry>::type coordinate_type;
    typedef typename bg::point_type<Geometry>::type point_type;

    // TODO: also make tests for miter
    typedef bg::strategy::buffer::join_round
        <
            point_type,
            point_type
        > join_strategy;

    GeometryOut moved_by_offset;
    bg::offset(geometry, moved_by_offset, join_strategy(), distance);

    typename bg::default_length_result<Geometry>::type length
                    = bg::length(moved_by_offset);

    /*
    std::size_t count = bg::num_points(moved_by_offset);
    BOOST_CHECK_MESSAGE(count == expected_point_count,
            "offset: " << caseid
            << " #points expected: " << expected_point_count
            << " detected: " << count
            << " type: " << string_from_type<coordinate_type>::name()
            );
    */


    //BOOST_CHECK_EQUAL(holes, expected_hole_count);
    BOOST_CHECK_CLOSE(length, expected_length, percentage);

#if defined(TEST_WITH_SVG)
    {
        std::ostringstream filename;
        filename << "offset_"
            << caseid << "_"
            << string_from_type<coordinate_type>::name()
            << ".svg";

        std::ofstream svg(filename.str().c_str());

        bg::svg_mapper
            <
                typename bg::point_type<Geometry>::type
            > mapper(svg, 500, 500);
        mapper.add(geometry);
        mapper.add(moved_by_offset);

        mapper.map(geometry, "opacity:0.6;fill:rgb(0,0,255);stroke:rgb(0,0,0);stroke-width:1");
        mapper.map(moved_by_offset, "opacity:0.6;fill:none;stroke:rgb(255,0,0);stroke-width:5");
    }
#endif
}


template <typename Geometry>
void test_one(std::string const& caseid, std::string const& wkt, double distance,
        double expected_length_plus, double expected_length_minus, bool do_plus, bool do_min)
{
    Geometry geometry;
    bg::read_wkt(wkt, geometry);

    double percentage = 0.01;
    if (do_plus) test_offset<Geometry>(caseid + "_a", geometry, distance, expected_length_plus, percentage);
    if (do_min) test_offset<Geometry>(caseid + "_b", geometry, -distance, expected_length_minus, percentage);
}




template <typename P>
void test_all()
{

    typedef bg::model::linestring<P> linestring;

    static std::string const simplex = "LINESTRING(0 0,1 1)";
    static std::string const one_bend = "LINESTRING(0 0,4 5,7 4)";
    static std::string const two_bends = "LINESTRING(0 0,4 5,7 4,10 6)";
    static std::string const overlapping = "LINESTRING(0 0,4 5,7 4,10 6, 10 2,2 2)";
    static std::string const curve = "LINESTRING(2 7,3 5,5 4,7 5,8 7)";
    static std::string const reallife1 = "LINESTRING(76396.40464822574 410095.6795147947,76397.85016212701 410095.211865792,76401.30666443033 410095.0466387949,76405.05892643372 410096.1007777959,76409.45103273794 410098.257640797,76412.96309264141 410101.6522238015)";

    test_one<linestring>("ls_simplex", simplex, 0.5, std::sqrt(2.0), std::sqrt(2.0), true, true);
    test_one<linestring>("one_bend", one_bend, 0.5, 10.17328, 8.8681, true, false);

    // Most of the tests below fail because the internal implementation of buffer is changed in the meantime (on purpose).
    // The offset now contains knots which should be removed separately, apart from the buffer algorithm.
    // The offset algorithm is therefore hardly usable now (only convex pieces are handled correctly...)

    // TODO: decide about this / implement this correctly.

    //test_one<linestring>("two_bends", two_bends, 0.5, 13.2898, 12.92811);
    //test_one<linestring>("overlapping", overlapping, 0.5, 27.1466, 22.0596);
    test_one<linestring>("curve", curve, 0.5, 7.7776,  10.0507, false, true);
    //test_one<linestring>("reallife1", reallife1, 16.5, 5.4654, 36.4943);
}


int test_main(int, char* [])
{
    test_all<bg::model::d2::point_xy<double> >();

    return 0;
}
