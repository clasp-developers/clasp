// Boost.Geometry (aka GGL, Generic Geometry Library)
//
// Copyright (c) 2010-2012 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)


#include "shapefil.h"

#include <boost/noncopyable.hpp>

#include <boost/geometry.hpp>
#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/multi/geometries/multi_geometries.hpp>

#include <boost/geometry/extensions/gis/io/shapelib/shape_creator.hpp>
#include <boost/geometry/extensions/gis/io/shapelib/shp_create_object.hpp>
#include <boost/geometry/extensions/gis/io/shapelib/shp_create_object_multi.hpp>
#include <boost/geometry/extensions/gis/io/shapelib/dbf_write_attribute.hpp>

#include <boost/geometry/io/wkt/wkt.hpp>


// Writing shapefiles has never been easier.

// Small derived class to be able to write shapes by WKT
template<typename Geometry>
class shape_creator : public boost::geometry::shape_creator<Geometry>
{
public :
    shape_creator(std::string const& name)
        : boost::geometry::shape_creator<Geometry>(name)
    {}

    // Name inspired on OGC "STGeomFromText"
    int AddGeomFromText(std::string const& wkt)
    {
        Geometry geometry;
        boost::geometry::read_wkt(wkt, geometry);
        boost::geometry::correct(geometry);
        return AddShape(geometry);
    }
};


// Next ~80 lines write 8 shapefiles including one or two features
// complete with a DBF with one attribute
int main()
{
    namespace bg = boost::geometry;
    typedef bg::model::point<double, 2, bg::cs::cartesian> point_type;
    try
    {
        {
            shape_creator<bg::model::ring<point_type> > sc("out/pol");
            sc.AddField<int>("dummy", 10);
            int r = sc.AddGeomFromText("POLYGON((0 0,0 1,1 1,1 0,0 0))");
            sc.WriteField(r, 0, 10);
        }

        {
            shape_creator<bg::model::polygon<point_type> > sc("out/donut");
            sc.AddField<int>("dummy", 10);
            int r = sc.AddGeomFromText("POLYGON((0 0,0 7,4 2,2 0,0 0),(1 1,2 1,1 2,1 1))");
            sc.WriteField(r, 0, 10);
        }

        {
            shape_creator<bg::model::polygon<point_type> > sc("out/donut2");
            sc.AddField<int>("dummy", 10);
            int r = sc.AddGeomFromText("POLYGON ((174618.3423614502 504723.43828582764, 174658.97901153564 504727.76149749756, 174655.07703399658 504763.57702636719, 174655.89733123779 504767.03034210205, 174657.70821380615 504770.11280822754, 174660.34678649902 504772.52502441406, 174663.55408477783 504774.04540252686, 174871.51212310791 504793.95350646973, 174809.82281494141 504969.10485076904, 174592.13478088379 504995.37437438965, 174596.52947998047 504954.86187744141, 174596.31950378418 504951.57875061035, 174595.33219909668 504948.68436431885, 174597.70560455322 504928.34646606445, 174618.3423614502 504723.43828582764), (174658.489402771 504823.78255462646, 174646.87094116211 504938.40633392334, 174772.98456573486 504923.18756866455, 174803.11683654785 504837.63452911377, 174658.489402771 504823.78255462646))");
            sc.WriteField(r, 0, 10);
        }


        {
            shape_creator<bg::model::linestring<point_type> > sc("out/ls");
            sc.AddField<int>("dummy", 10);
            int r = sc.AddGeomFromText("LINESTRING(0 0,3 3,5 4,7 1)");
            sc.WriteField(r, 0, 10);
        }

        {
            shape_creator<point_type> sc("out/pnt");
            sc.AddField<int>("dummy", 10);
            // Write three points including attribute
            int r = sc.AddGeomFromText("POINT(2 2)"); sc.WriteField(r, 0, 22);
            r = sc.AddGeomFromText("POINT(3 2)"); sc.WriteField(r, 0, 32);
            r = sc.AddGeomFromText("POINT(2 3)"); sc.WriteField(r, 0, 23);
        }

        {
            shape_creator<bg::model::box<point_type> > sc("out/box");
            sc.AddField<int>("dummy", 10);
            int r = sc.AddGeomFromText("POLYGON((1 1,2 2))");
        }

        {
            shape_creator<bg::model::segment<point_type> > sc("out/seg");
            sc.AddField<short>("dummy", 10);

            // This time, write to shape as geometry and not as WKT
            // (because bg::segment is currently const -> no WKT support)
            point_type p1, p2;
            bg::read_wkt("POINT(1 1)", p1);
            bg::read_wkt("POINT(2 2)", p2);
            bg::model::segment<point_type> s(p1, p2);

            int r = sc.AddShape(s);
            sc.WriteField(r, 0, 10);
        }

        {
            shape_creator<bg::model::multi_point<point_type> > sc("out/mpnt");
            sc.AddField<float>("dummy", 10);
            int r = sc.AddGeomFromText("MULTIPOINT((0 0),(1 1),(5 2),(7 3))");
            sc.WriteField(r, 0, 10.1f);
        }

        {
            shape_creator<bg::model::multi_linestring<bg::model::linestring<point_type> > > sc("out/ml");
            sc.AddField<double>("dummy", 10);
            int r = sc.AddGeomFromText("MULTILINESTRING((0 0,1 1,2 0,3 1),(4 4,5 3,6 5))");
            sc.WriteField(r, 0, 10.2);
        }

        {
            shape_creator<bg::model::multi_polygon<bg::model::polygon<point_type> > > sc("out/mp");
            sc.AddField<std::string>("dummy", 10);
            int r = sc.AddGeomFromText("MULTIPOLYGON(((0 0,0 7,4 2,2 0,0 0),(1 1,2 1,1 2,1 1)),((10 10,10 11,11 11,11 10,10 10)))");
            sc.WriteField(r, 0, "test");
        }
    }
    catch(std::exception const& e)
    {
        std::cerr << e.what() << std::endl;
    }

    return 0;
}