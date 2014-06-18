// Boost.Geometry (aka GGL, Generic Geometry Library)
//
// Copyright (c) 2012 Barend Gehrels, Amsterdam, the Netherlands.
//
// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//
// Projection example 4, reworked version of example 3
// Now using svg mapper, multi polygons and specific transform strategy

#include <fstream>

#include <boost/foreach.hpp>
#include <boost/geometry.hpp>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/geometries/register/point.hpp>
#include <boost/geometry/multi/geometries/multi_polygon.hpp>

#include <boost/geometry/io/svg/svg_mapper.hpp>
#include <boost/geometry/extensions/gis/latlong/latlong.hpp>

#include <boost/geometry/extensions/gis/projections/parameters.hpp>
#include <boost/geometry/extensions/gis/projections/proj/robin.hpp>

// Define a specific projection transformer
// (NOTE: this might become part of the library)
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

void read_wkt_and_project_and_map_svg(std::string const& wkt_filename,
        std::string const& svg_filename)
{
    using namespace boost::geometry;

    typedef model::ll::point<degree> point_ll_deg;
    typedef model::d2::point_xy<double> point_xy;

    typedef model::multi_polygon<model::polygon<point_ll_deg> > mp_ll;
    typedef model::multi_polygon<model::polygon<point_xy> > mp_xy;

    typedef projections::robin_spheroid<point_ll_deg, point_xy> robin;

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

    robin prj(projections::init("+ellps=WGS84 +units=m"));
    projection_transformer<robin> projection(prj);

    // Project the polygons, and at the same time get the bounding box (in xy)
    std::vector<mp_xy> countries_in_xy;
    model::box<point_xy> bbox;
    assign_inverse(bbox);
    BOOST_FOREACH(mp_ll const& country_ll, countries_in_ll) 
    {
        mp_xy country_xy;
        if (transform(country_ll, country_xy, projection))
        {
            expand(bbox, return_envelope<model::box<point_xy> >(country_xy));
            countries_in_xy.push_back(country_xy);
        }
    }

    // Create an SVG image
    std::ofstream svg(svg_filename.c_str());
    boost::geometry::svg_mapper<point_xy> mapper(svg, 1000, 800);
    mapper.add(bbox);

    BOOST_FOREACH(mp_xy const& country, countries_in_xy) 
    {
        mapper.map(country, "fill-opacity:0.6;fill:rgb(153,204,0);stroke:rgb(0,128,0);stroke-width:0.2");
    }
}

int main(int argc, char** argv)
{
    try
    {
        // Note, file location: trunk/libs/geometry/example/data
        // update path below if necessary
        read_wkt_and_project_and_map_svg(
            "../../../../example/data/world.wkt",
            "world4.svg");
    }
    catch(std::exception const& e)
    {
        std::cout << "Exception: " << e.what() << std::endl;
        return 1;
    }
    catch(std::string const& s)
    {
        std::cout << "Exception: " << s << std::endl;
        return 1;
    }

    return 0;
}
