// Boost.Geometry (aka GGL, Generic Geometry Library)
// Unit Test

// Copyright (c) 2010-2012 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

//#define BOOST_GEOMETRY_DEBUG_WITH_MAPPER
//#define BOOST_GEOMETRY_DEBUG_ASSEMBLE
//#define BOOST_GEOMETRY_DEBUG_IDENTIFIER

#include <geometry_test_common.hpp>

#include <test_buffer.hpp>

#include <boost/geometry/multi/multi.hpp> // TODO: more specific
#include <boost/geometry/multi/geometries/multi_geometries.hpp>
#include <boost/geometry/extensions/algorithms/buffer/multi_buffer_inserter.hpp>


static std::string const simplex = "MULTILINESTRING((0 0,4 5),(5 4,10 0))";
static std::string const two_bends = "MULTILINESTRING((0 0,4 5,7 4,10 6),(1 5,5 9,8 6))";
static std::string const turn_inside = "MULTILINESTRING((0 0,4 5,7 4,10 6),(1 5,5 9,8 6),(0 4,-2 6))";


template <typename P>
void test_all()
{
    namespace buf = bg::strategy::buffer;
    typedef bg::model::linestring<P> linestring;
    typedef bg::model::multi_linestring<linestring> multi_linestring_type;
    typedef bg::model::polygon<P> polygon;

    // Round joins / round ends
    test_one<multi_linestring_type, buf::join_round, buf::end_round, polygon>("simplex", simplex, 49.0217, 1.5, 1.5);
    test_one<multi_linestring_type, buf::join_round, buf::end_round, polygon>("two_bends", two_bends, 74.73, 1.5, 1.5);
    test_one<multi_linestring_type, buf::join_round, buf::end_round, polygon>("turn_inside", turn_inside, 86.3313, 1.5, 1.5);
    test_one<multi_linestring_type, buf::join_round, buf::end_round, polygon>("two_bends_asym", two_bends, 58.3395, 1.5, 0.75);

    // Round joins / flat ends:
    test_one<multi_linestring_type, buf::join_round, buf::end_flat, polygon>("simplex", simplex, 38.2623, 1.5, 1.5);
    test_one<multi_linestring_type, buf::join_round, buf::end_flat, polygon>("two_bends", two_bends, 64.6217, 1.5, 1.5);
    // TODO this should be fixed test_one<multi_linestring_type, buf::join_round, buf::end_flat, polygon>("turn_inside", turn_inside, 99, 1.5, 1.5);
    test_one<multi_linestring_type, buf::join_round, buf::end_flat, polygon>("two_bends_asym", two_bends, 52.3793, 1.5, 0.75);

    // This one is far from done:
    // test_one<multi_linestring_type, buf::join_round, polygon>("turn_inside_asym_neg", turn_inside, 99, +1.5, -1.0);

    // Miter / divide joins, various ends
    test_one<multi_linestring_type, buf::join_round_by_divide, buf::end_flat, polygon>("two_bends", two_bends, 64.6217, 1.5, 1.5);
    test_one<multi_linestring_type, buf::join_miter, buf::end_flat, polygon>("two_bends", two_bends, 65.1834, 1.5, 1.5);
    test_one<multi_linestring_type, buf::join_miter, buf::end_round, polygon>("two_bends", two_bends, 75.2917, 1.5, 1.5);
}



int test_main(int, char* [])
{
    test_all<bg::model::point<double, 2, bg::cs::cartesian> >();

    return 0;
}
