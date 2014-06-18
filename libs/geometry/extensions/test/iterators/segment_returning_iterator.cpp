// Boost.Geometry (aka GGL, Generic Geometry Library)
//
// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.
// Copyright (c) 2008-2012 Bruno Lalande, Paris, France.
// Copyright (c) 2009-2012 Mateusz Loskot, London, UK.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#include <algorithm>
#include <list>
#include <sstream>
#include <string>
#include <vector>

#include <geometry_test_common.hpp>

#include <boost/geometry/core/coordinate_type.hpp>
#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/geometries/segment.hpp>
#include <boost/geometry/extensions/iterators/segment_returning_iterator.hpp>
#include <boost/geometry/io/wkt/read.hpp>

template <typename C>
void test_linestring(std::string const& wkt, std::string const& expected)
{
    typedef C point_list;
    typedef typename C::value_type point;
    typedef typename C::iterator base_iterator;
    typedef bg::segment_returning_iterator<base_iterator, point> segment_returning_iterator;
    typedef typename segment_returning_iterator::value_type segment;
    typedef bg::model::linestring<point> linestring;

    linestring g;
    bg::read_wkt(wkt, g);

    point_list v;
    std::copy(g.begin(), g.end(), std::back_insert_iterator<point_list>(v));
    BOOST_CHECK_EQUAL(g.size(), v.size());

    segment_returning_iterator it(v.begin(), v.end());
    segment_returning_iterator end(v.end());

    std::ostringstream oss;
    while (it != end)
    {
        segment const& s = *it;
        oss << bg::get<0>(s.first) << bg::get<1>(s.first)
            << bg::get<0>(s.second) << bg::get<1>(s.second);
        ++it;
    }
    BOOST_CHECK_EQUAL(oss.str(), expected);
}

int test_main(int, char* [])
{
    // Test std::vector
    typedef std::vector<bg::model::d2::point_xy<double> > points_v;
    test_linestring<points_v>("linestring empty", "");
    test_linestring<points_v>("linestring ()", "");
    test_linestring<points_v>("linestring (1 1)", "");
    test_linestring<points_v>("linestring (1 1, 2 2, 3 3)", "11222233");
    test_linestring<points_v>("linestring (1 1, 2 2, 3 3, 4 4)", "112222333344");
    test_linestring<points_v>("linestring (1 1, 2 2, 3 3, 4 4, 5 5, 6 6)", "11222233334444555566");

    // Test std::list
    typedef std::list<bg::model::d2::point_xy<double> > points_l;
    test_linestring<points_l>("linestring empty", "");
    test_linestring<points_l>("linestring ()", "");
    test_linestring<points_l>("linestring (1 1)", "");
    test_linestring<points_l>("linestring (1 1, 2 2, 3 3)", "11222233");
    test_linestring<points_l>("linestring (1 1, 2 2, 3 3, 4 4)", "112222333344");
    test_linestring<points_l>("linestring (1 1, 2 2, 3 3, 4 4, 5 5, 6 6)", "11222233334444555566");

    return 0;
}
