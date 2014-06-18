// Boost.Geometry (aka GGL, Generic Geometry Library)
// Unit Test

// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)


#include <geometry_test_common.hpp>

#include <boost/geometry/extensions/nsphere/nsphere.hpp>

#include <boost/geometry/algorithms/detail/assign_values.hpp>
#include <boost/geometry/core/cs.hpp>
#include <boost/geometry/geometries/point.hpp>
#include <boost/geometry/geometries/adapted/c_array.hpp>
#include <test_common/test_point.hpp>


BOOST_GEOMETRY_REGISTER_C_ARRAY_CS(cs::cartesian)


namespace bg = boost::geometry;

// define a custom circle
struct custom_circle
{
    float x,y;
    int r;
};

// adapt custom circle using traits
namespace boost { namespace geometry { namespace traits {

template<>
struct tag<custom_circle>
{
    typedef nsphere_tag type;
};

template<>
struct point_type<custom_circle>
{
    typedef model::point<double, 2, cs::cartesian> type;
};

template<>
struct radius_type<custom_circle>
{
    typedef int type;
};

template<>
struct access<custom_circle, 0>
{
    static inline const float& get(custom_circle const& c)
    {
        return c.x;
    }

    static inline void set(custom_circle& c, float const& value)
    {
        c.y = value;
    }
};

template<>
struct access<custom_circle, 1>
{
    static inline const float& get(const custom_circle& c)
    {
        return c.y;
    }

    static inline void set(custom_circle& c, float const& value)
    {
        c.y = value;
    }
};

template<>
struct radius_access<custom_circle, int, 0>
{
    static inline int get(custom_circle const& c)
    {
        return c.r;
    }

    static inline void set(custom_circle& c, int const& radius)
    {
        c.r = radius;
    }
};

}}} // namespace bg::traits


template <typename S, typename RT, typename CT>
void check_nsphere(S& to_check, RT radius, CT center_x, CT center_y, CT center_z)
{
    BOOST_CONCEPT_ASSERT( (bg::concept::ConstNsphere<S>) );
    BOOST_CONCEPT_ASSERT( (bg::concept::Nsphere<S>) );


    BOOST_CHECK_EQUAL(bg::get_radius<0>(to_check), radius);

    BOOST_CHECK_EQUAL(bg::get<0>(to_check), center_x);
    BOOST_CHECK_EQUAL(bg::get<1>(to_check), center_y);
    if (bg::dimension<S>::value >= 3)
    {
        BOOST_CHECK_EQUAL(bg::get<2>(to_check), center_z);
    }
}

template <typename P, typename T>
void test_construction()
{
    typedef typename bg::coordinate_type<P>::type ctype;

    bg::model::nsphere<P, T> c1;
    check_nsphere(c1, 0, 0,0,0);

    P center;
    bg::assign_values(center, 1, 2, 3);
    bg::model::nsphere<P, T> c2(center, 4);
    check_nsphere(c2, 4, 1,2,3);
}

template <typename C>
void test_assignment_3d()
{
    C c;

    // by hand
    bg::set<0>(c, 5);
    bg::set<1>(c, 50);
    bg::set<2>(c, 500);

    bg::set_radius<0>(c, 5000);
    check_nsphere(c, 5000, 5,50,500);

    bg::assign_values(c, 6, 60, 600);
    check_nsphere(c, 5000, 6,60,600);
}

template <typename C>
void test_assignment_2d()
{
    C c;

    // by hand
    bg::set<0>(c, 5);
    bg::set<1>(c, 50);

    bg::set_radius<0>(c, 5000);
}


template <typename P, typename T>
void test_all()
{
    test_construction<P, T>();
    test_assignment_3d<bg::model::nsphere<P, T> >();
}

template <typename P>
void test_all()
{
    test_all<P, int>();
    test_all<P, float>();
    test_all<P, double>();
}

int test_main(int, char* [])
{
    test_all<int[3]>();
    test_all<float[3]>();
    test_all<double[3]>();
    test_all<test::test_point>();
    test_all<bg::model::point<int, 3, bg::cs::cartesian> >();
    test_all<bg::model::point<float, 3, bg::cs::cartesian> >();
    test_all<bg::model::point<double, 3, bg::cs::cartesian> >();

    test_assignment_2d<custom_circle>();

    return 0;
}
