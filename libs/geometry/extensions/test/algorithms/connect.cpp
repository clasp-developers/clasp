// Boost.Geometry (aka GGL, Generic Geometry Library)
// Unit Test

// Copyright (c) 2010-2012 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#include <iostream>
#include <string>

#include <geometry_test_common.hpp>

#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/geometries/point_xy.hpp>

#include <boost/geometry/algorithms/buffer.hpp>
#include <boost/geometry/algorithms/num_points.hpp>
#include <boost/geometry/algorithms/unique.hpp>
#include <boost/geometry/extensions/algorithms/connect.hpp>
#include <boost/geometry/multi/io/wkt/read.hpp>
#include <boost/geometry/multi/algorithms/area.hpp>
#include <boost/geometry/multi/algorithms/length.hpp>
#include <boost/geometry/multi/algorithms/num_points.hpp>
#include <boost/geometry/multi/algorithms/unique.hpp>
#include <boost/geometry/multi/geometries/multi_linestring.hpp>
#include <boost/geometry/strategies/strategies.hpp>


#if defined(TEST_WITH_SVG)
#  include <boost/geometry/multi/algorithms/envelope.hpp>
#  include <boost/geometry/io/svg/svg_mapper.hpp>
#endif




template <typename GeometryOut, typename Geometry>
void test_connect(std::string const& caseid, Geometry const& geometry,
        std::size_t expected_count, std::size_t expected_point_count,
        double expected_length, double limit = -1, double percentage = 0.001)
{
    typedef typename bg::coordinate_type<Geometry>::type coordinate_type;

    std::vector<GeometryOut> connected_vector;
    if (limit > 0)
    {
        bg::connect(geometry, connected_vector, limit);
    }
    else
    {
        bg::connect(geometry, connected_vector);
    }

    typename bg::default_length_result<Geometry>::type length = 0;
    std::size_t count = boost::size(connected_vector);
    std::size_t point_count = 0;

    BOOST_FOREACH(GeometryOut& connected, connected_vector)
    {
        bg::unique(connected);
        length += bg::length(connected);
        point_count += bg::num_points(connected);
    }

    BOOST_CHECK_MESSAGE(count == expected_count,
            "connect: " << caseid
            << " #lines expected: " << expected_count
            << " detected: " << count
            << " type: " << string_from_type<coordinate_type>::name()
            );


    BOOST_CHECK_EQUAL(point_count, expected_point_count);
    BOOST_CHECK_CLOSE(length, expected_length, percentage);


#if defined(TEST_WITH_SVG)
    {
        std::ostringstream filename;
        filename << "connect_"
            << caseid << "_"
            << string_from_type<coordinate_type>::name()
            << ".svg";

        std::ofstream svg(filename.str().c_str());

        bg::svg_mapper
            <
                typename bg::point_type<Geometry>::type
            > mapper(svg, 500, 500);
        mapper.add(geometry);

        bg::model::box<typename bg::point_type<Geometry>::type> extent;
        bg::envelope(geometry, extent);
        bg::buffer(extent, extent, 0.1);
        mapper.add(extent);


        mapper.map(geometry, "opacity:0.6;fill:rgb(0,0,255);stroke:rgb(0,0,0);stroke-width:1");
        BOOST_FOREACH(GeometryOut& connected, connected_vector)
        {
           mapper.map(connected, "opacity:0.6;fill:none;stroke:rgb(255,0,0);stroke-width:5");
        }
    }
#endif
}


template <typename Geometry, typename GeometryOut>
void test_one(std::string const& caseid, std::string const& wkt,
        std::size_t expected_count, std::size_t expected_point_count,
        double expected_length, double limit = -1, double percentage = 0.001)
{
    Geometry geometry;
    bg::read_wkt(wkt, geometry);

    test_connect<GeometryOut>(caseid, geometry,
        expected_count, expected_point_count,
        expected_length, limit, percentage);

#ifdef BOOST_GEOMETRY_TEST_MULTI_PERMUTATIONS
    // Test different combinations of a multi-polygon

    int n = geometry.size();

    // test them in all orders
    std::vector<int> indices;
    for (int i = 0; i < n; i++)
    {
        indices.push_back(i);
    }
    int permutation = 0;
    do
    {
        std::ostringstream out;
        out << caseid;
        Geometry geometry2;
        for (int i = 0; i < n; i++)
        {
            int index = indices[i];
            out << "_" << index;
            geometry2.push_back(geometry[index]);
        }
        test_connect<GeometryOut>(out.str(), geometry2,
                expected_point_count, expected_length, percentage);
    } while (std::next_permutation(indices.begin(), indices.end()));
#endif

}




template <typename P>
void test_all()
{


    typedef bg::model::linestring<P> linestring;
    typedef bg::model::multi_linestring<linestring> multi_linestring;

    test_one<multi_linestring, linestring>("ls_simplex",
        "MULTILINESTRING((0 0,1 1),(1 1,2 2))",
        1, 3, 2 * std::sqrt(2.0));

    // Opposites, forming one line
    test_one<multi_linestring, linestring>("ls_simplex_opposite_to",
        "MULTILINESTRING((0 0,1 1),(2 2,1 1))",
        1, 3, 2 * std::sqrt(2.0));
    test_one<multi_linestring, linestring>("ls_simplex_opposite_from",
        "MULTILINESTRING((1 1,0 0),(1 1,2 2))",
        1, 3, 2 * std::sqrt(2.0));

    // Two output linestrings
    test_one<multi_linestring, linestring>("ls_simplex_two",
        "MULTILINESTRING((0 0,1 1),(1 1,2 2),(3 3,4 4),(4 4,5 5))",
        2, 6, 4 * std::sqrt(2.0));

    // Linestrings forming a ring
    test_one<multi_linestring, linestring>("ls_simplex_ring",
        "MULTILINESTRING((0 0,0 1),(1 1,1 0),(0 1,1 1),(1 0,0 0))",
        1, 5, 4.0);

    // disconnected rings
    test_one<multi_linestring, linestring>("ls_disconnected_ring1",
        "MULTILINESTRING((0 0,0 1.01),(1.02 1.03,0.99 0),(0 0.98,1.001 1),(1.01 0,0 0))",
        1, 8, 4.137147, 0.5);
    test_one<multi_linestring, linestring>("ls_disconnected_ring2",
        "MULTILINESTRING((0 0,0 1.01),(1.02 1.03,0.99 0),(0 0.98,1.001 1),(1.01 0,0 0))",
        1, 8, 4.137147, 0.1);
    test_one<multi_linestring, linestring>("ls_disconnected_ring3",
        "MULTILINESTRING((0 0,0 1.01),(1.02 1.03,0.99 0),(0 0.98,1.001 1),(1.01 0,0 0))",
        3, 7, 4.05163658, 0.01);

    test_one<multi_linestring, linestring>("ls_disconnected_ring4",
        "MULTILINESTRING((0.01 0,0 1.01),(1.02 1.03,0.99 0),(0 0.98,1.001 1),(1.01 0,0.02 0))",
        1, 8, 4.1172, 0.1);
}


int test_main(int, char* [])
{
    test_all<bg::model::d2::point_xy<double> >();

    return 0;
}
