// Boost.Geometry (aka GGL, Generic Geometry Library) 
// Unit Test

// Copyright (c) 2010-2012 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)


#ifndef BOOST_GEOMETRY_TEST_BUFFER_HPP
#define BOOST_GEOMETRY_TEST_BUFFER_HPP

//#define BOOST_GEOMETRY_DEBUG_WITH_MAPPER
//#define TEST_WITH_SVG

#include <fstream>
#include <iomanip>

#include <boost/foreach.hpp>
#include <geometry_test_common.hpp>


#include <boost/geometry/algorithms/envelope.hpp>
#include <boost/geometry/algorithms/area.hpp>
#include <boost/geometry/algorithms/buffer.hpp>
#include <boost/geometry/algorithms/centroid.hpp>
#include <boost/geometry/algorithms/union.hpp>

#include <boost/geometry/algorithms/detail/overlay/debug_turn_info.hpp>

#include <boost/geometry/geometries/geometries.hpp>

#include <boost/geometry/strategies/strategies.hpp>

#include <boost/geometry/algorithms/disjoint.hpp>
#include <boost/geometry/algorithms/intersects.hpp>

#include <boost/geometry/extensions/algorithms/buffer/buffer_inserter.hpp>

#include <boost/geometry/extensions/strategies/buffer.hpp>
#include <boost/geometry/extensions/strategies/buffer_end_round.hpp>
#include <boost/geometry/extensions/strategies/buffer_end_flat.hpp>
#include <boost/geometry/extensions/strategies/buffer_end_skip.hpp>



#include <boost/geometry/io/wkt/wkt.hpp>


#if defined(TEST_WITH_SVG)
#  include <boost/geometry/io/svg/svg_mapper.hpp>
#endif


#if defined(TEST_WITH_SVG)
#include <boost/geometry/algorithms/detail/overlay/self_turn_points.hpp>
template <typename Geometry, typename Mapper>
void post_map(Geometry const& geometry, Mapper& mapper)
{
    typedef bg::detail::overlay::turn_info
    <
        typename bg::point_type<Geometry>::type
    > turn_info;

    std::vector<turn_info> turns;

    bg::detail::self_get_turn_points::no_interrupt_policy policy;
    bg::self_turns
        <
            bg::detail::overlay::assign_null_policy
        >(geometry, turns, policy);

    BOOST_FOREACH(turn_info const& turn, turns)
    {
        mapper.map(turn.point, "fill:rgb(255,128,0);stroke:rgb(0,0,100);stroke-width:1", 3);
    }
}
#endif

//-----------------------------------------------------------------------------
template <template<typename, typename> class JoinStrategy>
struct JoinTestProperties { };

template<> struct JoinTestProperties<boost::geometry::strategy::buffer::join_round>
{ 
    static std::string name() { return "round"; }
    static double tolerance() { return 0.1; }
};

template<> struct JoinTestProperties<boost::geometry::strategy::buffer::join_miter>
{ 
    static std::string name() { return "miter"; }
    static double tolerance() { return 0.001; }
};

template<> struct JoinTestProperties<boost::geometry::strategy::buffer::join_round_by_divide>
{ 
    static std::string name() { return "divide"; }
    static double tolerance() { return 0.1; }
};


//-----------------------------------------------------------------------------
template <template<typename, typename> class EndStrategy>
struct EndTestProperties { };

template<> struct EndTestProperties<boost::geometry::strategy::buffer::end_round>
{ 
    static std::string name() { return "round"; }
    static double tolerance() { return 0.1; }
};

template<> struct EndTestProperties<boost::geometry::strategy::buffer::end_flat>
{ 
    static std::string name() { return "flat"; }
    static double tolerance() { return 0.001; }
};

template<> struct EndTestProperties<boost::geometry::strategy::buffer::end_skip>
{ 
    static std::string name() { return ""; }
    static double tolerance() { return 0.001; }
};

template
<
    typename GeometryOut,
    template<typename, typename> class JoinStrategy,
    template<typename, typename> class EndStrategy,
    typename Geometry
>
void test_buffer(std::string const& caseid, Geometry const& geometry,
            bool check, double expected_area,
            double distance_left, double distance_right,
            int expected_self_tangencies)
{
    namespace bg = boost::geometry;

    typedef typename bg::coordinate_type<Geometry>::type coordinate_type;
    typedef typename bg::point_type<Geometry>::type point_type;
    typedef bg::strategy::buffer::distance_asymmetric<coordinate_type> distance;

    typedef typename bg::ring_type<GeometryOut>::type ring_type;

	typedef typename bg::tag<Geometry>::type tag;
	// TODO use something different here:
	std::string type = boost::is_same<tag, bg::polygon_tag>::value ? "poly"
		: boost::is_same<tag, bg::linestring_tag>::value ? "line"
		: boost::is_same<tag, bg::point_tag>::value ? "point"
		: boost::is_same<tag, bg::multi_polygon_tag>::value ? "multipoly"
		: boost::is_same<tag, bg::multi_linestring_tag>::value ? "multiline"
		: boost::is_same<tag, bg::multi_point_tag>::value ? "multipoint"
		: ""
		;

    typedef typename bg::point_type<GeometryOut>::type output_point_type;

    std::string join_name = JoinTestProperties<JoinStrategy>::name();
    std::string end_name = EndTestProperties<EndStrategy>::name();

    if (boost::is_same<tag, bg::point_tag>::value 
        || boost::is_same<tag, bg::multi_point_tag>::value)
    {
        join_name.clear();
    }

    std::ostringstream complete;
    complete
        << type << "_"
        << caseid << "_"
        << string_from_type<coordinate_type>::name()
        << "_" << join_name
        << (end_name.empty() ? "" : "_") << end_name
         // << "_" << point_buffer_count
        ;

    //std::cout << complete.str() << std::endl;

    std::ostringstream filename;
    filename << "buffer_" << complete.str() << ".svg";

#if defined(TEST_WITH_SVG)
    std::ofstream svg(filename.str().c_str());
    bg::svg_mapper<point_type> mapper(svg, 1000, 1000);

    {
        bg::model::box<point_type> box;
        bg::envelope(geometry, box);
        double d = std::abs(distance_left);
		if (distance_right > -998)
		{
			d += std::abs(distance_right);
		}
        else
        {
            distance_right = distance_left;
        }

        bg::buffer(box, box, d * (join_name == "miter" ? 2.0 : 1.1));
        mapper.add(box);
    }
#endif

    JoinStrategy
        <
            point_type,
            output_point_type
        > join_strategy;

    EndStrategy
        <
            point_type,
            output_point_type
        > end_strategy;

    bg::strategy::buffer::distance_asymmetric
        <
            coordinate_type
        > 
    distance_strategy(distance_left, distance_right);

    std::vector<GeometryOut> buffered;

    bg::buffer_inserter<GeometryOut>(geometry, std::back_inserter(buffered),
                        distance_strategy, 
                        join_strategy,
                        end_strategy
#ifdef BOOST_GEOMETRY_DEBUG_WITH_MAPPER
                        , mapper
#endif
                                );

    //// Remove duplicate point (this step should go automatically in the end)
    //BOOST_FOREACH(GeometryOut& polygon, buffered)
    //{
    //    bg::unique(polygon);
    //}

    typename bg::default_area_result<GeometryOut>::type area = 0;
    BOOST_FOREACH(GeometryOut const& polygon, buffered)
    {
        area += bg::area(polygon);
    }

    //std::cout << caseid << " " << distance_left << std::endl;
    //std::cout << "INPUT: " << bg::wkt(geometry) << std::endl;
    //std::cout << "OUTPUT: " << area << std::endl;
    //BOOST_FOREACH(GeometryOut const& polygon, buffered)
    //{
    //    std::cout << bg::wkt(polygon) << std::endl;
    //}


    if (expected_area > -0.1)
    {
        double tol = JoinTestProperties<JoinStrategy>::tolerance() 
            + EndTestProperties<EndStrategy>::tolerance();

		if (expected_area < 1.0e-5)
		{
			tol /= 1.0e6;
		}

		typename bg::default_area_result<GeometryOut>::type tolerance = tol;


        BOOST_CHECK_MESSAGE
            (
                bg::math::abs(area - expected_area) < tolerance,
                complete.str() << " not as expected. " 
                << " Expected: "  << expected_area
                << " Detected: "  << area
            );

        // Be sure resulting polygon does not contain
        // self-intersections
        // But indentation5 should contain 1 self-ip TODO give this check as an argument
        if (expected_self_tangencies == 0
            && ! boost::contains(complete.str(), "indentation5_d_r")
            && ! boost::contains(complete.str(), "flower25_d_r")
            && ! boost::contains(complete.str(), "multipoly_rt_d_d_m")
			)
        {
            BOOST_FOREACH(GeometryOut const& polygon, buffered)
            {
                BOOST_CHECK_MESSAGE
                    (
                        ! bg::intersects(polygon), 
                        complete.str() << " output is self-intersecting. " 
                    );
            }
        }
    }

#if defined(TEST_WITH_SVG)
    // Map input geometry in green
    mapper.map(geometry, "opacity:0.5;fill:rgb(0,128,0);stroke:rgb(0,128,0);stroke-width:10");

    BOOST_FOREACH(GeometryOut const& polygon, buffered)
    {
        mapper.map(polygon, "opacity:0.4;fill:rgb(255,255,128);stroke:rgb(0,0,0);stroke-width:3");
        //mapper.map(polygon, "opacity:0.2;fill:none;stroke:rgb(255,0,0);stroke-width:3");
        post_map(polygon, mapper);
    }
#endif
}


#ifdef BOOST_GEOMETRY_CHECK_WITH_POSTGIS
static int counter = 0;
#endif

template
<
    typename Geometry,
    template<typename, typename> class JoinStrategy,
    template<typename, typename> class EndStrategy,
    typename GeometryOut
>
void test_one(std::string const& caseid, std::string const& wkt,
        double expected_area,
        double distance_left, double distance_right = -999,
        int expected_self_tangencies = 0)
{
    namespace bg = boost::geometry;
    Geometry g;
    bg::read_wkt(wkt, g);

    typedef typename bg::point_type<Geometry>::type point_type;


#ifdef BOOST_GEOMETRY_CHECK_WITH_POSTGIS
    std::cout
        << (counter > 0 ? "union " : "")
        << "select " << counter++
        << ", '" << caseid << "' as caseid"
        << ", ST_Area(ST_Buffer(ST_GeomFromText('" << wkt << "'), "
        << distance_left
        << ", 'endcap=" << end_name << " join=" << join_name << "'))"
        << ", "  << expected_area
        << std::endl;
#endif

    test_buffer<GeometryOut, JoinStrategy, EndStrategy>
            (caseid, g, false, expected_area,
            distance_left, distance_right, expected_self_tangencies);
}



#endif
