// Boost.Geometry (aka GGL, Generic Geometry Library)
//
// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.
// Copyright (c) 2008-2012 Bruno Lalande, Paris, France.
// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//
// Projection example 1, direct

#include <boost/geometry.hpp>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/extensions/algorithms/parse.hpp>

#include <boost/geometry/extensions/gis/latlong/latlong.hpp>
#include <boost/geometry/extensions/gis/projections/parameters.hpp>
#include <boost/geometry/extensions/gis/projections/proj/robin.hpp>

int main()
{
    using namespace boost::geometry;

    // Initialize projection parameters
    projections::parameters par = projections::init("+ellps=WGS84 +units=m");

    // Construct a Robinson projection, using specified point types
    // (This delivers a projection without virtual methods. Note that in p02 example
    //  the projection is created using a factory, which delivers a projection with virtual methods)
    typedef model::ll::point<degree> point_ll_deg;
    typedef model::d2::point_xy<double> point_xy;
    projections::robin_spheroid<point_ll_deg, point_xy> prj(par);

    // Define Amsterdam / Barcelona in decimal degrees / degrees/minutes
    point_ll_deg amsterdam = parse<point_ll_deg>("52.4N", "5.9E");
    point_ll_deg barcelona = parse<point_ll_deg>("41 23'N", "2 11'E");

    point_xy pa, pb;

    // Now do the projection. "Forward" means from latlong to meters.
    // (Note that a map projection might fail. This is not 'exceptional'.
    // Therefore the forward function does not throw but returns false)
    if (prj.forward(amsterdam, pa) && prj.forward(barcelona, pb))
    {
        std::cout << "Amsterdam: " << wkt(pa) << std::endl << "Barcelona: " << wkt(pb) << std::endl;

        std::cout << "Distance (unprojected):" << distance(amsterdam, barcelona) / 1000.0 << " km" << std::endl;
        std::cout << "Distance (  projected):" << distance(pa, pb) / 1000.0 << " km" << std::endl;

        // Do the inverse projection. "Inverse" means from meters to latlong
        // It also might fail or might not exist, not all projections
        // have their inverse implemented
        point_ll_deg a1;
        if (prj.inverse(pa, a1))
        {
            std::cout << "Amsterdam (original): " << wkt(amsterdam)  << std::endl
                << "Amsterdam (projected, and back):" << wkt(a1) << std::endl;
            std::cout << "Distance a-a': " << distance(amsterdam, a1) << " meter" << std::endl;
        }
    }

    return 0;
}
