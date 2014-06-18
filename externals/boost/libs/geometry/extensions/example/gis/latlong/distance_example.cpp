// Boost.Geometry (aka GGL, Generic Geometry Library)
//
// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.
// Copyright (c) 2008-2012 Bruno Lalande, Paris, France.
// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//
// Distance Example

// This sample demonstrates the use of latlong-points, xy-points,
// calculate distances between latlong points using different formulas,
// calculate distance between points using pythagoras

#include <iostream>

#include <boost/geometry/geometry.hpp>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/strategies/spherical/distance_cross_track.hpp>
#include <boost/geometry/extensions/gis/latlong/latlong.hpp>
#include <boost/geometry/extensions/gis/geographic/strategies/andoyer.hpp>
#include <boost/geometry/extensions/gis/projections/proj/sterea.hpp>
#include <boost/geometry/extensions/gis/projections/proj/laea.hpp>
#include <boost/geometry/extensions/gis/projections/parameters.hpp>

// BSG 28-10-2010
// TODO: clear up this test
// it is more a test than an example
// the results are sometimes WRONG

int main()
{

    using namespace boost::geometry;

    typedef model::ll::point<degree> latlon_point;
    typedef model::d2::point_xy<double> xy_point;

    latlon_point city1;
    // Amsterdam 52 22'23"N 4 53'32"E
    std::string const city1_name = "Amsterdam";
    city1.lat(dms<north>(52, 22, 23));
    city1.lon(dms<east>(4, 53, 32));

    // Rotterdam 51 55'51"N 4 28'45"E
    // latlon_point city2(latitude<>(dms<north>(51, 55, 51)), longitude<>(dms<east>(4, 28, 45)));
    // Paris 48 52' 0" N, 2 19' 59" E
    std::string const city2_name = "Paris";
    latlon_point city2(latitude<>(dms<north>(48, 52, 0)), longitude<>(dms<east>(2, 19, 59)));

    // The Hague: 52 4' 48" N, 4 18' 0" E
    //latlon_point city3(longitude<>(dms<east>(4, 18, 0)), latitude<>(dms<north>(52, 4, 48)));
    // Barcelona
    std::string const city3_name = "Barcelona";
    latlon_point city3(longitude<>(dms<east>(2, 11, 0)), latitude<>(dms<north>(41, 23, 0)));


    model::ll::point<radian> city1_rad, city2_rad, city3_rad;
    transform(city1, city1_rad);
    transform(city2, city2_rad);
    transform(city3, city3_rad);

    /*
    projections::sterea_ellipsoid<model::ll::point<radian>, xy_point> proj
        (projections::init(
        "+lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m"));
    */
    projections::laea_ellipsoid<model::ll::point<radian>, xy_point> proj
        (projections::init(
        " +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m"));


    xy_point city1_prj, city2_prj, city3_prj;
    proj.forward(city1_rad, city1_prj);
    proj.forward(city3_rad, city3_prj);
    proj.forward(city2_rad, city2_prj);

    // ------------------------------------------------------------------------------------------
    // Distances
    // ------------------------------------------------------------------------------------------

    std::cout << "Distance " << city1_name << "-" << city2_name << ": " << std::endl;
    std::cout << "haversine:              " << 0.001 * distance(city1, city2) << " km" << std::endl;
    std::cout << "haversine rad:          " << 0.001 * distance(city1_rad, city2_rad) << " km" << std::endl;
    std::cout << "haversine other radius: " << distance(city1, city2, strategy::distance::haversine<double>(6371.0) ) << " km" << std::endl;
    std::cout << "andoyer:                " << 0.001 * distance(city1, city2, strategy::distance::andoyer<double>() ) << " km" << std::endl;
    std::cout << "vincenty:               " << 0.001 * distance(city1, city2, strategy::distance::vincenty<double>() ) << " km" << std::endl;
    std::cout << "vincenty rad:           " << 0.001 * distance(city1_rad, city2_rad, strategy::distance::vincenty<double>() ) << " km" << std::endl;
    std::cout << "Projected, pythagoras:  " << 0.001 * distance(city1_prj, city2_prj) << " km" << std::endl;

    std::cout << std::endl;
    std::cout << "Distance " << city1_name << "-" << city3_name << ": " << std::endl;
    std::cout << "andoyer:                " << 0.001 * distance(city1, city3, strategy::distance::andoyer<double>()) << " km" << std::endl;
    std::cout << "Distance " << city2_name << "-" << city3_name << ": " << std::endl;
    std::cout << "andoyer:                " << 0.001 * distance(city2, city3, strategy::distance::andoyer<double>()) << " km" << std::endl;

    // ------------------------------------------------------------------------------------------
    // Distances to segments
    // ------------------------------------------------------------------------------------------
    std::cout << std::endl << city3_name << " - line " << city1_name << "," << city2_name << std::endl;

    model::segment<xy_point> ar_xy(city1_prj, city2_prj);

    double dr = distance(city3_prj, ar_xy);
    std::cout << "projected: " << 0.001 * dr << std::endl;

    dr = distance(city3, model::segment<latlon_point>(city1, city2));
    std::cout << "in LL: " << 0.001 * dr << std::endl;

    std::cout << std::endl << city2_name << " - line " << city1_name << "," << city3_name << std::endl;
    dr = distance(city2_prj, model::segment<xy_point>(city1_prj, city3_prj));
    std::cout << "projected: " << 0.001 * dr << std::endl;
    dr = distance(city2, model::segment<latlon_point>(city1, city3));
    std::cout << "in LL: " << 0.001 * dr << std::endl;
    std::cout << std::endl;


    // ------------------------------------------------------------------------------------------
    // Compilation
    // ------------------------------------------------------------------------------------------
    // Next line does not compile because Vincenty cannot work on xy-points
    //std::cout << "vincenty on xy:         " << 0.001 * distance(city1_prj, city2_prj, formulae::distance::vincenty<double>() ) << " km" << std::endl;

    // Next line does not compile because you cannot (yet) assign degree to radian directly
    //ll::point<radian> a_rad2 = city1;

    // Next line does not compile because you cannot assign latlong to xy
    // d2::point axy = city1;

    // ------------------------------------------------------------------------------------------
    // Length
    // ------------------------------------------------------------------------------------------
    // Length calculations use distances internally. The lines below take automatically the default
    // formulae for distance. However, you can also specify city1 formula explicitly.

    model::linestring<latlon_point> line1;
    append(line1, city1);
    append(line1, city2);
    std::cout << "length: " << length(line1) << std::endl;
    std::cout << "length using Vincenty: " << length(line1, strategy::distance::vincenty<double>()) << std::endl;

    model::linestring<xy_point> line2;
    append(line2, city1_prj);
    append(line2, city2_prj);
    std::cout << "length: " << length(line2) << std::endl;

    return 0;
}
