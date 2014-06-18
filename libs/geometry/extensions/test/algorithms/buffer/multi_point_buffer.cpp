// Boost.Geometry (aka GGL, Generic Geometry Library)
// Unit Test

// Copyright (c) 2010-2012 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

//#define BOOST_GEOMETRY_DEBUG_WITH_MAPPER
//#define BOOST_GEOMETRY_DEBUG_ASSEMBLE
//#define BOOST_GEOMETRY_DEBUG_IDENTIFIER

#include <geometry_test_common.hpp>

#include <boost/geometry/algorithms/buffer.hpp>
#include <boost/geometry/core/coordinate_type.hpp>

#include <boost/geometry/strategies/strategies.hpp>

#include <boost/geometry/geometries/point.hpp>
#include <boost/geometry/geometries/box.hpp>

#include <test_buffer.hpp>

#include <boost/geometry/multi/multi.hpp> // TODO: more specific
#include <boost/geometry/multi/geometries/multi_geometries.hpp>
#include <boost/geometry/extensions/algorithms/buffer/multi_buffer_inserter.hpp>


static std::string const simplex = "MULTIPOINT((5 5),(7 7))";
static std::string const three = "MULTIPOINT((5 8),(9 8),(7 11))";

// Generates error (extra polygon on top of rest) at distance 14.0:
static std::string const multipoint_a = "MULTIPOINT((39 44),(38 37),(41 29),(15 33),(58 39))";

// Just one with holes at distance ~ 15
static std::string const multipoint_b = "MULTIPOINT((5 56),(98 67),(20 7),(58 60),(10 4),(75 68),(61 68),(75 62),(92 26),(74 6),(67 54),(20 43),(63 30),(45 7))";


template <typename P>
void test_all()
{
    //std::cout << typeid(bg::coordinate_type<P>::type).name() << std::endl;

    namespace buf = bg::strategy::buffer;
    typedef bg::model::polygon<P> polygon;
    typedef bg::model::multi_point<P> multi_point_type;

	double const pi = boost::geometry::math::pi<double>();

    test_one<multi_point_type, buf::join_miter, buf::end_round, polygon>("simplex1", simplex, 2.0 * pi, 1.0, 1.0);
    test_one<multi_point_type, buf::join_miter, buf::end_round, polygon>("simplex2", simplex, 22.8372, 2.0, 2.0);
    test_one<multi_point_type, buf::join_miter, buf::end_round, polygon>("simplex3", simplex, 44.5692, 3.0, 3.0);

    test_one<multi_point_type, buf::join_miter, buf::end_round, polygon>("three1", three, 3.0 * pi, 1.0, 1.0);
    test_one<multi_point_type, buf::join_miter, buf::end_round, polygon>("three2", three, 36.7592, 2.0, 2.0);
    test_one<multi_point_type, buf::join_miter, buf::end_round, polygon>("three19", three, 33.6914, 1.9, 1.9);
    test_one<multi_point_type, buf::join_miter, buf::end_round, polygon>("three21", three, 39.6394, 2.1, 2.1);
    test_one<multi_point_type, buf::join_miter, buf::end_round, polygon>("three3", three, 65.533, 3.0, 3.0);

    test_one<multi_point_type, buf::join_miter, buf::end_round, polygon>("multipoint_a", multipoint_a, 2049.98, 14.0, 14.0);
    test_one<multi_point_type, buf::join_miter, buf::end_round, polygon>("multipoint_b", multipoint_b, 7109.88, 15.0, 15.0);
    test_one<multi_point_type, buf::join_miter, buf::end_round, polygon>("multipoint_b1", multipoint_b, 6911.89, 14.7, 14.7);
    test_one<multi_point_type, buf::join_miter, buf::end_round, polygon>("multipoint_b2", multipoint_b, 7174.79, 15.1, 15.1);
}

template 
<
    typename GeometryOut, 
    template<typename, typename> class JoinStrategy,
    template<typename, typename> class EndStrategy,
    typename Geometry
>
double test_growth(Geometry const& geometry, int n, int d, double distance)
{
    namespace bg = boost::geometry;

    typedef typename bg::coordinate_type<Geometry>::type coordinate_type;
    typedef typename bg::point_type<Geometry>::type point_type;

    typedef typename bg::ring_type<GeometryOut>::type ring_type;

	typedef typename bg::tag<Geometry>::type tag;

    // extern int point_buffer_count;
    std::ostringstream complete;
    complete
        << "point" << "_"
        << "growth" << "_"
        << string_from_type<coordinate_type>::name()
        << "_" << "r"
        << "_" << n
        << "_" << d
         // << "_" << point_buffer_count
        ;

    //std::cout << complete.str() << std::endl;

    std::ostringstream filename;
    filename << "buffer_" << complete.str() << ".svg";

    std::ofstream svg(filename.str().c_str());

#ifdef BOOST_GEOMETRY_DEBUG_WITH_MAPPER
    bg::svg_mapper<point_type> mapper(svg, 500, 500);

    {
        bg::model::box<point_type> box;
        bg::envelope(geometry, box);

        bg::buffer(box, box, distance * 1.01);
        mapper.add(box);
    }
#endif

    JoinStrategy
        <
            point_type,
            typename bg::point_type<GeometryOut>::type
        > join_strategy;
    EndStrategy
        <
            point_type,
            typename bg::point_type<GeometryOut>::type
        > end_strategy;

    typedef bg::strategy::buffer::distance_symmetric<coordinate_type> distance_strategy_type;
    distance_strategy_type distance_strategy(distance);

    std::vector<GeometryOut> buffered;

    bg::buffer_inserter<GeometryOut>(geometry, std::back_inserter(buffered),
                        distance_strategy, 
                        join_strategy,
                        end_strategy
#ifdef BOOST_GEOMETRY_DEBUG_WITH_MAPPER
                        , mapper
#endif
                                );

    typename bg::default_area_result<GeometryOut>::type area = 0;
    BOOST_FOREACH(GeometryOut const& polygon, buffered)
    {
        area += bg::area(polygon);
    }

#ifdef BOOST_GEOMETRY_DEBUG_WITH_MAPPER
    // Map input geometry in green
    mapper.map(geometry, "opacity:0.5;fill:rgb(0,128,0);stroke:rgb(0,128,0);stroke-width:10");

    BOOST_FOREACH(GeometryOut const& polygon, buffered)
    {
        mapper.map(polygon, "opacity:0.4;fill:rgb(255,255,128);stroke:rgb(0,0,0);stroke-width:3");
    }
#endif

    return area;
}

template <typename P>
void test_growth(int n, int distance_count)
{
    srand(int(time(NULL)));
    //std::cout << typeid(bg::coordinate_type<P>::type).name() << std::endl;
    boost::timer t;

    namespace buf = bg::strategy::buffer;
    typedef bg::model::polygon<P> polygon;
    typedef bg::model::multi_point<P> multi_point_type;

    multi_point_type multi_point;
    for (int i = 0; i < n; i++)
    {
        P point(rand() % 100, rand() % 100);
        multi_point.push_back(point);
    }

    std::cout << bg::wkt(multi_point) << std::endl;

    double previous_area = 0;
    double epsilon = 0.1;
    double distance = 15.0;
    for (int d = 0; d < distance_count; d++, distance += epsilon)
    {
        double area = test_growth<polygon, buf::join_miter, buf::end_round>(multi_point, n, d, distance);
        if (area < previous_area)
        {
            std::cout << "Error: " << area << " < " << previous_area << std::endl
                << " n=" << n << " distance=" << distance
                << bg::wkt(multi_point) << std::endl;
        }
        previous_area = area;
    }
    std::cout << "n=" << n << " time=" << t.elapsed() << std::endl;
}

int test_main(int, char* [])
{
    //std::cout << std::setprecision(6);
    //test_all<bg::model::point<float, 2, bg::cs::cartesian> >();
    test_all<bg::model::point<double, 2, bg::cs::cartesian> >();


#ifdef BOOST_GEOMETRY_BUFFER_TEST_GROWTH
    for (int i = 5; i <= 50; i++)
    {
        test_growth<bg::model::point<double, 2, bg::cs::cartesian> >(i, 20);
    }
#endif

    return 0;
}
