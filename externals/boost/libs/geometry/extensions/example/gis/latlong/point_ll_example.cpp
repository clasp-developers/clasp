// Boost.Geometry (aka GGL, Generic Geometry Library)
//
// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.
// Copyright (c) 2008-2012 Bruno Lalande, Paris, France.
// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//
// Point Example - showing geographic (latitude longitude) points

#include <iostream>
#include <iomanip>

#include <boost/geometry/geometry.hpp>
#include <boost/geometry/extensions/algorithms/parse.hpp>
#include <boost/geometry/extensions/gis/latlong/latlong.hpp>

// Formula to get the course (direction) between two points.
// This might be a GGL-function in the future.
template <typename P1, typename P2>
inline double get_course(P1 const& p1, P2 const& p2)
{
    double const& lat1 = boost::geometry::get_as_radian<1>(p1);
    double const& lon1 = boost::geometry::get_as_radian<0>(p1);
    double const& lat2 = boost::geometry::get_as_radian<1>(p2);
    double const& lon2 = boost::geometry::get_as_radian<0>(p2);
    // http://williams.best.vwh.net/avform.htm#Crs
    return atan2(sin(lon1-lon2)*cos(lat2),
       cos(lat1)*sin(lat2)-sin(lat1)*cos(lat2)*cos(lon1-lon2));
}


// Formula to calculate the point at a distance/angle from another point
// This might be a GGL-function in the future.
template <typename P1, typename P2>
inline void point_at_distance(P1 const& p1,
        double distance, double tc, double radius,
        P2& p2)
{
    double const two_pi = 2.0 * boost::geometry::math::pi<double>();
    double earth_perimeter = radius * two_pi;
;
    double d = (distance / earth_perimeter) * two_pi;
    double const& lat1 = boost::geometry::get_as_radian<1>(p1);
    double const& lon1 = boost::geometry::get_as_radian<0>(p1);

    // http://williams.best.vwh.net/avform.htm#LL
    double lat = asin(sin(lat1)*cos(d)+cos(lat1)*sin(d)*cos(tc));
    double dlon = atan2(sin(tc)*sin(d)*cos(lat1),cos(d)-sin(lat1)*sin(lat));
    double lon = lon1 - dlon;

    boost::geometry::set_from_radian<1>(p2, lat);
    boost::geometry::set_from_radian<0>(p2, lon);
}



int main()
{
    using namespace boost::geometry;

    typedef model::ll::point<degree> latlon_point;
    
    latlon_point paris;

    // Assign coordinates to the latlong point, using the methods lat and lon
    // Paris 48 52' 0" N, 2 19' 59" E
    paris.lat(dms<north>(48, 52, 0));
    paris.lon(dms<east>(2, 19, 59));

    std::cout << "Paris: " << boost::geometry::dsv(paris) << std::endl;

    // Constructor using explicit latitude/longitude
    // Lima 12 2' 36" S, 77 1' 42" W
    latlon_point lima(
            latitude<>(dms<south>(12, 2, 36)),
            longitude<>(dms<west>(77, 1, 42)));

    std::cout << "Lima: " << boost::geometry::dsv(lima) << std::endl;

    // Construction with parse utiity
    latlon_point amsterdam = parse<latlon_point>("52 22'23\"N", "4 53'32\"E");
    std::cout << "Amsterdam: " << boost::geometry::dsv(amsterdam) << std::endl;

    // Calculate the distance using the default strategy (Andoyer), and Vincenty
    std::cout << std::setprecision(9);
    std::cout << "Distance Paris-Lima, Andoyer (default) "
        << 0.001 * distance(paris, lima)
        << " km" << std::endl;

    std::cout << "Distance Paris-Lima, Vincenty "
        << 0.001 * distance(paris, lima, strategy::distance::vincenty<double>())
        << " km" << std::endl;

    // Using great circle (=haversine), this is less precise because earth is not a sphere
    double const average_earth_radius = 6372795.0;
    std::cout << "Distance Paris-Lima, great circle "
        << 0.001 * distance(paris, lima, strategy::distance::haversine<double>(average_earth_radius))
        << " km" << std::endl;

    // Convert a latlong point to radians. This might be convenient, although algorithms
    // are transparent on degree/radians
    model::ll::point<radian> paris_rad;
    transform(paris, paris_rad);
    std::cout << "Paris in radians: " << boost::geometry::dsv(paris_rad) << std::endl;

    model::ll::point<radian> amsterdam_rad;
    transform(amsterdam, amsterdam_rad);
    std::cout << "Amsterdam in radians: " << boost::geometry::dsv(amsterdam_rad) << std::endl;

    std::cout << "Distance Paris-Amsterdam, (degree) " << 0.001 * distance(paris, amsterdam) << " km" << std::endl;
    std::cout << "Distance Paris-Amsterdam, (radian) " << 0.001 * distance(paris_rad, amsterdam_rad) << " km" << std::endl;

    std::cout << "Distance Paris-Amsterdam, (mixed) " << 0.001 * distance(paris, amsterdam_rad) << " km" << std::endl;

    // Other way round: have Amsterdam and go 430 km to the south (i.e. first calculate direction)
    double tc = get_course(amsterdam, paris);
    std::cout << "Course: " << (tc * boost::geometry::math::r2d) << std::endl;

    latlon_point paris_calculated;
    point_at_distance(amsterdam, 430 * 1000.0, tc, average_earth_radius, paris_calculated);
    std::cout << "Paris calculated (degree): " << boost::geometry::dsv(paris_calculated) << std::endl;

    return 0;
}
