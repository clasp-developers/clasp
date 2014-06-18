// Boost.Geometry (aka GGL, Generic Geometry Library)
//
// Copyright (c) 2012 Barend Gehrels, Amsterdam, the Netherlands.
//
// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//
// Projection example 5 (reworked from 4), using small factory

#include <fstream>

#include <boost/foreach.hpp>
#include <boost/geometry.hpp>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/geometries/register/point.hpp>
#include <boost/geometry/multi/geometries/multi_polygon.hpp>

#include <boost/geometry/io/svg/svg_mapper.hpp>
#include <boost/geometry/extensions/gis/latlong/latlong.hpp>

#include <boost/geometry/extensions/gis/projections/parameters.hpp>
#include <boost/geometry/extensions/gis/projections/proj/goode.hpp>
#include <boost/geometry/extensions/gis/projections/proj/moll.hpp>
#include <boost/geometry/extensions/gis/projections/proj/natearth.hpp>
#include <boost/geometry/extensions/gis/projections/proj/robin.hpp>

#include <boost/geometry/extensions/gis/projections/new_projection.hpp>

// Define a specific projection transformer
// (NOTE: this might become part of the library - copied from p04)
template <typename Projection>
struct projection_transformer
{
    Projection const& m_prj;

    inline projection_transformer(Projection const& prj)
        : m_prj(prj)
    {}

    inline bool apply(typename Projection::geographic_point_type const& p1,
                typename Projection::cartesian_point_type& p2) const
    {
        return m_prj.forward(p1, p2);
    }
};

void p05_example(int projection_id,
        std::string const& wkt_filename,
        std::string const& svg_filename)
{
    using namespace boost::geometry;
    using namespace boost::geometry::projections;

    typedef model::ll::point<degree> pll;
    typedef model::d2::point_xy<double> pxy;

    // Idea and headerfile "new_projection" submitted by Krzysztof Czainski:
    // They are useful, when:
    // - you have a small set of types of projections you'll use,
    // - you know the type of projection during it's creation, and later 
    //   you want to use it through an abstract base pointer,
    // - you want to avoid the overhead of factory: generating code for
    //   creating a projection of every type you don't use, and selecting
    //   the type from a string.

    projection<pll, pxy>* prj = NULL;

    parameters pars = projections::init("+ellps=WGS84");
    switch(projection_id)
    {
        case 1 : prj = new_projection<robin_spheroid<pll, pxy> >(pars); break;
        case 2 : prj = new_projection<moll_spheroid<pll, pxy> >(pars); break;
        case 3 : prj = new_projection<goode_spheroid<pll, pxy> >(pars); break;
        case 4 : prj = new_projection<natearth_spheroid<pll, pxy> >(pars); break;
        default : return;
    }

    typedef model::multi_polygon<model::polygon<pll> > mp_ll;
    typedef model::multi_polygon<model::polygon<pxy> > mp_xy;

    std::vector<mp_ll> countries_in_ll;

    // Read polygons from WKT
    std::ifstream cpp_file(wkt_filename.c_str());
    if (! cpp_file.is_open())
    {
        throw std::string("File not found: ") + wkt_filename;
    }

    while (! cpp_file.eof() )
    {
        std::string line;
        std::getline(cpp_file, line);
        if (boost::starts_with(line, "MULTIPOLYGON"))
        {
            countries_in_ll.resize(countries_in_ll.size() + 1);
            boost::geometry::read_wkt(line, countries_in_ll.back());
        }
    }

    projection_transformer<projection<pll, pxy> > strategy(*prj);

    // Project the polygons, and at the same time get the bounding box (in xy)
    std::vector<mp_xy> countries_in_xy;
    model::box<pxy> bbox;
    assign_inverse(bbox);
    BOOST_FOREACH(mp_ll const& country_ll, countries_in_ll) 
    {
        mp_xy country_xy;
        if (transform(country_ll, country_xy, strategy))
        {
            expand(bbox, return_envelope<model::box<pxy> >(country_xy));
            countries_in_xy.push_back(country_xy);
        }
    }

    // Create an SVG image
    std::ofstream svg(svg_filename.c_str());
    boost::geometry::svg_mapper<pxy> mapper(svg, 1000, 800);
    mapper.add(bbox);

    BOOST_FOREACH(mp_xy const& country, countries_in_xy) 
    {
        mapper.map(country, "fill-opacity:0.6;fill:rgb(153,204,0);stroke:rgb(0,128,0);stroke-width:0.2");
    }

    delete prj;
}

int main(int argc, char** argv)
{
    // Note, file location: trunk/libs/geometry/example/data
    // update path below if necessary
    std::string const data = "../../../../example/data/world.wkt";
    try
    {
        p05_example(1, data, "p05_world_1.svg");
        p05_example(2, data, "p05_world_2.svg");
        p05_example(3, data, "p05_world_3.svg");
        p05_example(4, data, "p05_world_4.svg");
    }
    catch(std::exception const& e)
    {
        std::cerr << "Exception: " << e.what() << std::endl;
        return 1;
    }
    catch(std::string const& s)
    {
        std::cerr << "Exception: " << s << std::endl;
        return 1;
    }

    return 0;
}
