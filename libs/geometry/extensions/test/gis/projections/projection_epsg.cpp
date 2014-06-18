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

#if defined(_MSC_VER)
#pragma warning( disable : 4305 ) // truncation double -> float
#endif // defined(_MSC_VER)




#include <geometry_test_common.hpp>

#include <boost/geometry/extensions/gis/projections/parameters.hpp>
#include <boost/geometry/extensions/gis/projections/projection.hpp>
#include <boost/geometry/extensions/gis/projections/proj/sterea.hpp>
#include <boost/geometry/extensions/gis/projections/proj/tmerc.hpp>


#include <boost/geometry/algorithms/transform.hpp>

#include <boost/geometry/core/coordinate_type.hpp>

#include <boost/geometry/geometries/adapted/c_array.hpp>
#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/extensions/gis/latlong/point_ll.hpp>
#include <test_common/test_point.hpp>

template <int E, typename P1, typename P2>
void test_one(double lon, double lat,
              typename bg::coordinate_type<P2>::type x,
              typename bg::coordinate_type<P2>::type y)
{
    typedef typename bg::coordinate_type<P2>::type coord_type;

    typedef bg::projections::epsg_traits<E, P1, P2> epsg_traits;
    bg::projections::parameters par = bg::projections::detail::pj_init_plus(epsg_traits::par());

    typedef typename epsg_traits::type prj_type;
    prj_type prj(par);

    P1 ll;
    ll.lon(lon);
    ll.lat(lat);

    P2 xy;
    prj.forward(ll, xy);

    BOOST_CHECK_CLOSE(bg::get<0>(xy), x, 0.001);
    BOOST_CHECK_CLOSE(bg::get<1>(xy), y, 0.001);
}

template <typename D, typename P>
void test_deg_rad(double factor)
{
    typedef typename bg::coordinate_type<P>::type coord_type;
    typedef bg::model::ll::point<D, coord_type> point_type;

    test_one<28992, point_type, P>(4.897000 * factor, 52.371000 * factor, 121590.388077, 487013.903377);
    test_one<29118, point_type, P>(4.897000 * factor, 52.371000 * factor, 4852882, 9129373);
}

template <typename P>
void test_all()
{
    test_deg_rad<bg::degree, P>(1.0);
    test_deg_rad<bg::radian, P>(bg::math::d2r);
}

int test_main(int, char* [])
{
    // Commented out most the types because otherwise it cannot be linked
    //test_all<int[2]>();
    //test_all<float[2]>();
    //test_all<double[2]>();
    //test_all<test::test_point>();
    //test_all<bg::model::d2::point_xy<int> >();
    ////test_all<bg::model::d2::point_xy<float> >();
    ////test_all<bg::model::d2::point_xy<long double> >();

    test_all<bg::model::d2::point_xy<double> >();

    return 0;
}
