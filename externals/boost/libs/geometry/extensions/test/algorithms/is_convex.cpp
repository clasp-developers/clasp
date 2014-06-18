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

#include <cstddef>
#include <string>


#include <geometry_test_common.hpp>

#include <boost/geometry/algorithms/convex_hull.hpp>


#include <boost/geometry/algorithms/area.hpp>
#include <boost/geometry/algorithms/num_points.hpp>

#include <boost/geometry/io/wkt/read.hpp>
#include <boost/geometry/io/wkt/write.hpp>

#include <boost/geometry/strategies/strategies.hpp>

#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/geometries/point_xy.hpp>

#include <boost/geometry/util/as_range.hpp>


template <typename Geometry>
void test_geometry(std::string const& wkt,
                      std::size_t size_original, std::size_t size_hull,
                      double expected_area)
{

    Geometry geometry;
    bg::read_wkt(wkt, geometry);

    typedef typename bg::point_type<Geometry>::type P;
    typename bg::strategy::side::services::default_strategy
        <
            typename bg::cs_tag<P>::type
        >::type side;

    boost::ignore_unused_variable_warning(side);

    typedef typename bg::range_type<Geometry>::type range_type;
    typedef typename boost::range_const_iterator<range_type>::type iterator;

    range_type const& range = bg::as_range<range_type>(geometry);

    iterator it1 = boost::begin(range);
    iterator it3 = it1++;
    iterator it2 = it1++;

    for (;
        it2 != boost::end(range);
        ++it1, ++it2, ++it3)
    {
       // Last/closing point
       if (it1 == boost::end(range))
       {
           it1 = boost::begin(range) + 1;
       }
       int s = side.apply(*it1, *it2, *it3);

       if (s != 1)
       {
          std::cout << "NOT CONVEX!";
       }
       if (s == 0)
       {
          std::cout << " COLLINEAR!";
       }

       std::cout
           << " " << bg::wkt(*it3)
           << " " << bg::wkt(*it2)
           << " " << bg::wkt(*it1)
           << " " << s
           << std::endl;
    }

    std::cout << bg::area(geometry) << " " << bg::wkt(geometry) << std::endl;
}

template <typename P>
void test_all()
{
    // rectangular, with concavity
    test_geometry<bg::model::polygon<P> >(
        "polygon((1 1, 1 4, 3 4, 3 3, 4 3, 4 4, 5 4, 5 1, 1 1))",
                9, 5, 12.0);


   // concavity at start/closing point
   test_geometry<bg::model::polygon<P> >(
        "polygon((1 1,0 2,3 3,1 0,1 1))", 9, 5, 12.0);

    // from sample polygon, with concavity
    test_geometry<bg::model::polygon<P> >(
        "polygon((2.0 1.3, 2.4 1.7, 2.8 1.8, 3.4 1.2, 3.7 1.6,3.4 2.0, 4.1 3.0"
        ", 5.3 2.6, 5.4 1.2, 4.9 0.8, 2.9 0.7,2.0 1.3))",
                12, 8, 5.245);
}

int test_main(int, char* [])
{
    //test_all<bg::model::d2::point_xy<int> >();
    //test_all<bg::model::d2::point_xy<float> >();
    test_all<bg::model::d2::point_xy<double> >();

#if defined(HAVE_TTMATH)
    test_all<bg::model::d2::point_xy<ttmath_big> >();
#endif

    return 0;
}
