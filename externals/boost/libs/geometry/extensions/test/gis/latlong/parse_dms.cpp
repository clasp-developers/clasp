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

#include <string>

#include <geometry_test_common.hpp>

#include <boost/geometry/strategies/strategies.hpp>

#include <boost/geometry/extensions/gis/geographic/strategies/dms_parser.hpp>



void check(bg::dms_result const& r, double v, int axis)
{
    double const d = r;
    BOOST_CHECK_CLOSE(d, v, 0.0001);
    BOOST_CHECK(int(r.axis()) == axis);
}

template <bool as_radian>
void test_dms(std::string const& s, double v, int axis)
{
    bg::strategy::dms_parser<as_radian> parser;
    check(parser(s.c_str()), v, axis);
}

template <bool as_radian>
void test_dms_french(std::string const& s, double v, int axis)
{
    bg::strategy::dms_parser<as_radian, 'N', 'E', 'S', 'O'> parser;
    check(parser(s.c_str()), v, axis);
}

template <bool as_radian>
void test_dms_dutch(std::string const& s, double v, int axis)
{
    bg::strategy::dms_parser<as_radian, 'N', 'O', 'Z', 'W'> parser;
    check(parser(s.c_str()), v, axis);
}

template <bool as_radian>
void test_dms_case(std::string const& s, double v, int axis)
{
    bg::strategy::dms_parser<as_radian, 'n', 'e', 's', 'w'> parser;
    check(parser(s.c_str()), v, axis);
}

int test_main(int, char* [])
{
    test_dms<false>("5.9E", 5.9, 0);
    test_dms<false>("5.9W", -5.9, 0);
    test_dms<false>("5.9N", 5.9, 1);
    test_dms<false>("5.9S", -5.9, 1);

    // nearly pi -> nearly 180
    test_dms<false>("3.14RE", 179.90874767107848755314620611629, 0);

    test_dms<false>("5.9dE", 5.9, 0);
    test_dms<false>("5.9dW", -5.9, 0);
    test_dms<false>("5.9dN", 5.9, 1);
    test_dms<false>("5.9dS", -5.9, 1);

    // with minutes
    test_dms<false>("5d15E", 5.25, 0);
    test_dms<false>("5d30W", -5.50, 0);
    test_dms<false>("5d45N", 5.75, 1);
    test_dms<false>("5d00S", -5.00, 1);

    // with minutes and indicator
    test_dms<false>("5d15'E", 5.25, 0);
    test_dms<false>("5d30'W", -5.50, 0);
    test_dms<false>("5d45'N", 5.75, 1);
    test_dms<false>("5d00'S", -5.00, 1);

    // with minutes/seconds
    test_dms<false>("5d10'05E", 5.168055, 0);
    test_dms<false>("5d20'15W", -5.33750, 0);
    test_dms<false>("5d30'25N", 5.5069444, 1);
    test_dms<false>("5d40'35S", -5.676389, 1);

    // with seconds only
    double f3600 = 1/3600.0;
    test_dms<false>("5d01\"E", 5 + f3600, 0);
    test_dms<false>("5d02\"W", -(5 + 2.0 * f3600), 0);
    test_dms<false>("5d03\"N", 5 + 3.0 * f3600, 1);
    test_dms<false>("5d04\"S", -(5 + 4.0 * f3600), 1);

    // with spaces, signs, etc etc
    test_dms<false>("5d 10' 05\" E", 5.168055, 0);
    test_dms<false>("+5d 10' 05\" E", 5.168055, 0);
    test_dms<false>("-5d 10' 05\" E", -5.168055, 0);

    // reversed orders and default
    test_dms<false>("5d 05\" 10'  E", 5.168055, 0);
    test_dms<false>("E5 05\" 10' ", 5.168055, 0);
    test_dms<false>("5", 5, 0);

    // in radians
    test_dms<true>("3.14RE", 3.14, 0);
    test_dms<true>("3.14RW", -3.14, 0);

    // other languages
    test_dms_french<false>("52O", -52, 0);
    test_dms_dutch <false>("52O", +52, 0);

    // lower/upper case
    test_dms<false>("52e", 52, 0);
    test_dms_case<false>("52e", 52, 0);
    test_dms_case<false>("52E", 52, 0);

    return 0;
}
