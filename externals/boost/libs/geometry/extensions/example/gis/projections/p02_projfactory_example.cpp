// Boost.Geometry (aka GGL, Generic Geometry Library)
//
// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.
// Copyright (c) 2008-2012 Bruno Lalande, Paris, France.
// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//
// Projection example 2, using factory

#include <boost/geometry.hpp>
#include <boost/geometry/geometries/point_xy.hpp>

#include <boost/geometry/extensions/gis/latlong/latlong.hpp>
#include <boost/geometry/extensions/gis/projections/parameters.hpp>
#include <boost/geometry/extensions/gis/projections/factory.hpp>

#include <boost/shared_ptr.hpp>

int main()
{
    using namespace boost::geometry;

    // Initialize projection parameters. For construction using a factory the projection name is required.
    projections::parameters par = projections::init("+proj=robin +ellps=WGS84 +units=m");

    // Construct the specified projection, using specified point types
    // Note that this is the only difference from p01_projection_example. It constructs a projection
    // with virtual methods, which can be used polymorphically. Therefore it is a pointer. For
    // convenience we use a boost shared pointer here.
    typedef model::ll::point<degree> point_ll_deg;
    typedef model::d2::point_xy<double> point_xy;
    projections::factory<point_ll_deg, point_xy> fac;
    boost::shared_ptr<projections::projection<point_ll_deg, point_xy> > prj(fac.create_new(par));

    // Define Amsterdam / Barcelona in decimal degrees / degrees/minutes
    point_ll_deg amsterdam(longitude<>(5.9), latitude<>(52.4));
    point_ll_deg barcelona(
        latitude<>(dms<north>(41, 23)),
        longitude<>(dms<east>(2, 11))
        );

    point_xy pa, pb;

    // Do the forward projection
    if (prj->forward(amsterdam, pa) && prj->forward(barcelona, pb))
    {
        std::cout << "Amsterdam: " << wkt(pa) << std::endl << "Barcelona: " << wkt(pb) << std::endl;

        std::cout << "Distance (unprojected):" << distance(amsterdam, barcelona) / 1000.0 << " km" << std::endl;
        std::cout << "Distance (  projected):" << distance(pa, pb) / 1000.0 << " km" << std::endl;

        // Get the inverse
        point_ll_deg a1;
        if (prj->inverse(pa, a1))
        {
            std::cout << "Amsterdam (original): " << wkt(amsterdam)  << std::endl
                << "Amsterdam (projected, and back):" << wkt(a1) << std::endl;
            std::cout << "Distance a-a': " << distance(amsterdam, a1) << " meter" << std::endl;
        }
    }

    return 0;
}
