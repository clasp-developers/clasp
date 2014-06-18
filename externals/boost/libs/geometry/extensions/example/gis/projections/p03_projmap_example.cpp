// Boost.Geometry (aka GGL, Generic Geometry Library)
//
// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.
// Copyright (c) 2008-2012 Bruno Lalande, Paris, France.
// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//
// Projection example 3, combined with shapelib and SVG

#include <fstream>

#include <boost/geometry.hpp>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/geometries/register/point.hpp>
#include <boost/geometry/multi/geometries/multi_polygon.hpp>

#include <boost/geometry/io/svg/write_svg.hpp>
#include <boost/geometry/extensions/gis/latlong/latlong.hpp>
#include <boost/geometry/extensions/gis/projections/project_transformer.hpp>

void read_wkt_and_project_and_write_svg(std::string const& wkt_filename,
        std::string const& projection_parameters,
        std::string const& svg_filename)
{
    using namespace boost::geometry;

    // Declare a vector containing the world countries
    typedef model::ll::point<degree> point_ll_deg;
    typedef model::polygon<point_ll_deg> polygon_ll_deg;
    std::vector<polygon_ll_deg> ll_polygons;

    typedef model::d2::point_xy<double> point_xy;

    // Read polygons from a Well-Known Text file using the ggl parser
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
            typedef boost::geometry::model::multi_polygon<boost::geometry::model::polygon<point_ll_deg> > mp_type;
            mp_type mp;
            boost::geometry::read_wkt(line, mp);
            for (mp_type::const_iterator it = boost::begin(mp);
                it != boost::end(mp); ++it)
            {
                ll_polygons.push_back(*it);
            }
        }
    }

    // Our latlong polygon collection will be projected into this vector
    // (Of course it is also possible to do this while reading and have one vector)
    std::vector<model::polygon<point_xy> > xy_polygons;

    // Declare transformation strategy which contains a projection
    projections::project_transformer
        <
            point_ll_deg,
            point_xy
        > projection(projection_parameters);

    // Project the polygons, and at the same time get the bounding box (in xy)
    model::box<point_xy> bbox;
    assign_inverse(bbox);
    for (std::vector<polygon_ll_deg>::const_iterator it = ll_polygons.begin();
         it != ll_polygons.end();
         ++it)
    {
        model::polygon<point_xy> xy_polygon;

        if (transform(*it, xy_polygon, projection))
        {
            // Update bbox with box of this projected polygon
            expand(bbox, return_envelope<model::box<point_xy> >(xy_polygon));

            // Add projected polygon
            xy_polygons.push_back(xy_polygon);
        }
    }

    // Create an SVG image
    std::ofstream out(svg_filename.c_str());
    out
        << "<?xml version=\"1.0\" standalone=\"no\"?>" << std::endl
        << "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"" << std::endl
        << "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">" << std::endl
        << "<svg width=\"100%\" height=\"100%\" version=\"1.1\"" << std::endl
        << "xmlns=\"http://www.w3.org/2000/svg\">" << std::endl;

    // Setup the transformation to SVG
    // (alternatively this could be skipped because SVG can transform itself,
    // but this example shows it like this)
    typedef boost::geometry::model::d2::point_xy<int> svg_point;
    boost::geometry::strategy::transform::map_transformer
        <
            double, 2, 2,
            true, true
        > svg_transformer(bbox, 800, 600);

    // Create the background
    boost::geometry::model::box<svg_point> box;
    boost::geometry::assign_values(box, 0, 0, 800, 600);
    out << boost::geometry::svg(box, "fill:rgb(0,0,255)") << std::endl;

    for (std::vector<model::polygon<point_xy> >::const_iterator it = xy_polygons.begin();
         it != xy_polygons.end();
         ++it)
    {
        boost::geometry::model::polygon<svg_point> svg_polygon;
        boost::geometry::transform(*it, svg_polygon, svg_transformer);
        out << boost::geometry::svg(svg_polygon,
            "fill:rgb(0,255,0);stroke:rgb(0,0,0);stroke-width:0.2")
            << std::endl;
    }
    out << "</svg>" << std::endl;
}

int main(int argc, char** argv)
{
    try
    {
        // Note, file location: trunk/libs/geometry/example/data
        // update path below if necessary
        read_wkt_and_project_and_write_svg(
            "../../../../example/data/world.wkt",
            "+proj=moll +ellps=clrk66",
            "world.svg");
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
