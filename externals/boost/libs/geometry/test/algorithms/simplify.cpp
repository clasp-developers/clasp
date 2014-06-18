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

#include <iterator>


#include <algorithms/test_simplify.hpp>
#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/geometries/point_xy.hpp>

#include <test_geometries/wrapped_boost_array.hpp>
#include <test_common/test_point.hpp>


template <typename P>
void test_all()
{
    test_geometry<bg::model::linestring<P> >(
        "LINESTRING(0 0,5 5,10 10)",
        "LINESTRING(0 0,10 10)", 1.0);

    test_geometry<bg::model::linestring<P> >(
        "LINESTRING(0 0, 5 5, 6 5, 10 10)",
        "LINESTRING(0 0,10 10)", 1.0);

    test_geometry<bg::model::linestring<P> >(
        "LINESTRING(0 0,5 5,7 5,10 10)",
        "LINESTRING(0 0,5 5,7 5,10 10)", 1.0);

    // Mail 2013-10-07, real-life test, piece of River Leine
    // PostGIS returns exactly the same result
    test_geometry<bg::model::linestring<P> >(
         "LINESTRING(4293586 3290439,4293568 3290340,4293566 3290332,4293570 3290244,4293576 3290192"
                   ",4293785 3289660,4293832 3289597,4293879 3289564,4293937 3289545,4294130 3289558"
                   ",4294204 3289553,4294240 3289539,4294301 3289479,4294317 3289420,4294311 3289353"
                   ",4294276 3289302,4293870 3289045,4293795 3288978,4293713 3288879,4293669 3288767"
                   ",4293654 3288652,4293657 3288563,4293690 3288452,4293761 3288360,4293914 3288215"
                   ",4293953 3288142,4293960 3288044,4293951 3287961,4293913 3287875,4293708 3287628"
                   ",4293658 3287542,4293633 3287459,4293630 3287383,4293651 3287323,4293697 3287271"
                   ",4293880 3287128,4293930 3287045,4293938 3286977,4293931 3286901,4293785 3286525"
                   ",4293775 3286426,4293786 3286358,4293821 3286294,4294072 3286076,4294134 3285986)",
          "LINESTRING(4293586 3290439,4293785 3289660,4294317 3289420,4293654 3288652,4293960 3288044"
                  ",4293633 3287459,4293786 3286358,4294134 3285986)", 250);

    /* TODO fix this
    test_geometry<test::wrapped_boost_array<P, 10> >(
        "LINESTRING(0 0,5 5,7 5,10 10)",
        "LINESTRING(0 0,5 5,7 5,10 10)", 1.0);
    */

    test_geometry<bg::model::ring<P> >(
        "POLYGON((4 0,8 2,8 7,4 9,0 7,0 2,2 1,4 0))",
        "POLYGON((4 0,8 2,8 7,4 9,0 7,0 2,4 0))", 1.0);

    test_geometry<bg::model::polygon<P> >(
        "POLYGON((4 0,8 2,8 7,4 9,0 7,0 2,2 1,4 0))",
        "POLYGON((4 0,8 2,8 7,4 9,0 7,0 2,4 0))", 1.0);

    test_geometry<bg::model::polygon<P> >(
        "POLYGON((4 0,8 2,8 7,4 9,0 7,0 2,2 1,4 0),(7 3,7 6,1 6,1 3,4 3,7 3))",
        "POLYGON((4 0,8 2,8 7,4 9,0 7,0 2,4 0),(7 3,7 6,1 6,1 3,7 3))", 1.0);

/*

Above can be checked in PostGIS by:

select astext(ST_Simplify(geomfromtext('LINESTRING(0 0, 5 5, 10 10)'),1.0)) as simplified
union all select astext(ST_Simplify(geomfromtext('LINESTRING(0 0, 5 5, 6 5, 10 10)'),1.0))
etc
*/

    // Just check compilation
    test_geometry<P>(
        "POINT(0 0)",
        "POINT(0 0)", 1.0);


    test_geometry<bg::model::ring<P> >(
        "POLYGON((4 0,8 2,8 7,4 9,0 7,0 2,2 1,4 0))",
        "POLYGON((4 0,8 2,8 7,4 9,0 7,0 2,4 0))", 1.0);
}


template <typename P>
void test_spherical()
{
    test_geometry<bg::model::linestring<P> >(
        "LINESTRING(4.1 52.1,4.2 52.2,4.3 52.3)",
        "LINESTRING(4.1 52.1,4.3 52.3)", 0.01);
}


int test_main(int, char* [])
{
    // Integer compiles, but simplify-process fails (due to distances)
    //test_all<bg::model::d2::point_xy<int> >();

    test_all<bg::model::d2::point_xy<float> >();
    test_all<bg::model::d2::point_xy<double> >();

    test_spherical<bg::model::point<double, 2, bg::cs::spherical_equatorial<bg::degree> > >();

#if defined(HAVE_TTMATH)
    test_all<bg::model::d2::point_xy<ttmath_big> >();
    test_spherical<bg::model::point<ttmath_big, 2, bg::cs::spherical_equatorial<bg::degree> > >();
#endif

    return 0;
}
