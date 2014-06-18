// Boost.Geometry (aka GGL, Generic Geometry Library)
// Unit Test

// Copyright (c) 2011-2012 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#include <iostream>
#include <iomanip>
#include <string>

//#define BOOST_GEOMETRY_DEBUG_MARK_SPIKES

#include <geometry_test_common.hpp>

#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/multi/multi.hpp>
#include <boost/geometry/multi/geometries/multi_polygon.hpp>
#include <boost/geometry/strategies/strategies.hpp>

#include <boost/geometry/extensions/algorithms/mark_spikes.hpp>
#include <boost/geometry/extensions/algorithms/remove_marked.hpp>

#include <boost/geometry/io/wkt/wkt.hpp>



#if defined(TEST_WITH_SVG)
#  include <boost/geometry/io/svg/svg_mapper.hpp>
#endif

// BSG 2011/08/05
// Note: this unit test currently does give errors



template <typename Geometry, typename Policy>
inline void test_remove_indentations(std::string const& id,
            Geometry const& geometry,
            Policy const& policy,
            double expected_area, double expected_perimeter)
{
    typedef typename bg::point_type<Geometry>::type point_type;


    std::map<bg::ring_identifier, std::vector<bool> > mark_map;

    double a = bg::area(geometry);
    double p = bg::perimeter(geometry);

    bool marked = bg::mark_spikes(geometry, mark_map, policy);

    Geometry processed;
    bg::remove_marked(geometry, processed, mark_map);
    bg::correct(processed);


    double detected_area = bg::area(processed);
    double detected_perimeter = bg::perimeter(processed);

    BOOST_CHECK_CLOSE(detected_area, expected_area, 0.01);
    BOOST_CHECK_CLOSE(detected_perimeter, expected_perimeter, 0.01);

    /***/
#if defined(BOOST_GEOMETRY_DEBUG_MARK_SPIKES)

    std::cout << id << " area=" << a << " perimeter=" << p;
    if (marked)
    {
        std::cout << " MARKED"
            << ", new area=" << detected_area
            << " new perimeter=" << detected_perimeter;

        //std::cout << std::endl << std::setprecision(16) << " new WKT:" << bg::wkt(processed);
    }
    std::cout << std::endl;
#endif

#if defined(TEST_WITH_SVG)
    {
        std::ostringstream filename;
        filename << "remove_indentations_" << id << ".svg";
        std::ofstream svg(filename.str().c_str());

        bg::svg_mapper<typename bg::point_type<Geometry>::type> mapper(svg, 500, 500);
        mapper.add(geometry);
        mapper.map(geometry, "fill-opacity:0.3;opacity:0.6;fill:rgb(51,51,153);stroke:rgb(0,0,255);stroke-width:2");
        mapper.map(processed, "opacity:0.6;fill:none;stroke:rgb(255,0,0);stroke-width:3");
    }
#endif
}

template <typename Geometry, typename Policy>
void test_geometry(std::string const& id, std::string const& wkt, Policy const& policy,
            double expected_area, double expected_perimeter)
{
    Geometry geometry;
    bg::read_wkt(wkt, geometry);
    bg::correct(geometry);
    test_remove_indentations(id, geometry, policy, expected_area, expected_perimeter);
}


#if ! defined(GGL_TEST_MULTI)

template <typename P>
void test_all()
{
    typedef bg::model::ring<P> ring;
    typedef bg::model::polygon<P> polygon;
    typedef bg::model::multi_polygon<polygon> mp;

    test_geometry<polygon>("case1",
            "POLYGON((0 0,0 100,100 100,100 51,150 51,150 90,151 90,151 50,100 50,100 0,0 0))",
            bg::select_gapped_spike<>(2.0, 0.5), 10000.0, 400.0);

    test_geometry<mp>("texel_183836",
            "MULTIPOLYGON( ((114766.45300292969 560935.37800598145, 114839.08099365234 560958.13200378418, 114919.38200378418 560983.13500976562, 114945.70799255371 560990.80799865723, 114946.2380065918 560992.39601135254, 114930.75900268555 561046.63500976562, 114923.41007995606 561073.32402038574, 114924.41039576962 561073.6148228565, 114903.32929992676 561145.36410522461, 114895.68840026856 561171.36880493164, 114894.6575012207 561174.46450805664, 114894.59449768066 561174.43589782715, 114894.31700134277 561175.496307373, 114894.01480102539 561178.669998169, 114894.57029724121 561179.59580993652, 114969.90060575516 561202.33975150948, 114969.85419015621 561202.51050945686, 114942.83399963379 561194.24600219727, 114895.87001037598 561180.091003418, 114893.3570098877 561179.16500854492, 114872.97556998512 561162.44151731138, 114893.35699462891 561179.16400146484, 114894.90800476074 561173.2380065918, 114892.52699279785 561174.03199768066, 114889.74899291992 561171.51899719238, 114879.56199645996 561162.78800964355, 114844.50500488281 561131.30200195312, 114809.71200561523 561099.1549987793, 114787.22300720215 561079.17900085449, 114771.87699890137 561065.68499755859, 114753.62100219727 561048.4880065918, 114727.0299987793 561029.17300415039, 114717.10800170898 561021.5, 114701.89500427246 561011.04901123047, 114700.83599853516 561009.59400939941, 114701.89500427246 561006.94799804688, 114715.38800048828 560981.28300476074, 114729.9409942627 560953.370010376, 114738.27499389648 560937.62699890137, 114739.46600341797 560937.75900268555, 114766.45300292969 560935.37800598145), (114825.04400634766 560971.37200927734, 114823.72099304199 560971.76800537109, 114822.66299438477 560971.90100097656, 114821.07600402832 560973.22399902344, 114821.07600402832 560975.33999633789, 114821.47200012207 560976.26600646973, 114823.19200134277 560978.11799621582, 114825.04400634766 560978.51499938965, 114827.42599487305 560978.25100708008, 114828.48399353027 560976.796005249, 114828.61599731445 560974.15000915527, 114827.82200622559 560972.8270111084, 114825.04400634766 560971.37200927734), (114893.16600036621 561141.21200561523, 114891.51400756836 561142.51000976562, 114890.68800354004 561144.2799987793, 114890.92399597168 561146.9940032959, 114893.04200744629 561149.43600463867, 114897.01100158691 561148.77500915527, 114898.59399414063 561145.10600280762, 114898.00399780273 561142.9820098877, 114895.64399719238 561141.56600952148, 114893.16600036621 561141.21200561523)))",
            bg::select_gapped_spike<>(2.0, 0.1), 31052.272, 794.761);
    test_geometry<mp>("texel_195365",
            "MULTIPOLYGON( ((115681.31161499021 560944.083480835, 115681.2671573319 560944.22872300365, 115755.0199966431 560968.15299987793, 115727.1070022583 561055.33200073242, 115707.6600036621 561120.94999694824, 115708.341003418 561121.30999755859, 115728.7350006104 561055.82598876953, 115735.858001709 561033.60101318359, 115738.58300018311 561034.407989502, 115739.43000030521 561034.66000366211, 115739.2969970703 561037.56900024414, 115724.7450027466 561084.66598510742, 115713.5 561123.16198730469, 115736.263999939 561131.608001709, 115789.30400085451 561150.01800537109, 115866.6949996948 561176.07998657227, 115921.46299743649 561194.59899902344, 115984.4339981079 561215.10598754883, 115986.9479980469 561198.83401489258, 115992.372001648 561159.808013916, 115995.4150009155 561122.50100708008, 115997.0019989014 561087.70700073242, 115997.266998291 561057.28100585938, 115996.8700027466 561041.53799438477, 115986.5510025024 561038.89300537109, 115864.5780029297 561002.38000488281, 115757.55999755859 560968.05499267578, 115757.5589981079 560968.05499267578, 115749.0859985352 560965.33700561523, 115681.31161499021 560944.083480835)))",
            bg::select_gapped_spike<>(2.0, 0.9), 45002.987, 924.56);

    // Found some PostGIS stored procedure as well.
    // http://trac.osgeo.org/postgis/wiki/UsersWikiExamplesSpikeRemover
    // (note that that algorithm is completely different from this one)
    test_geometry<polygon>("pg1",
            "POLYGON((3480407.01 5483810.171,3480407.01 5483810.17,3480409.11 5483777.431,3480407.348 5483777.421,3480405.15 5483777.409,3480404.816 5483777.407,3480394.58 5483777.35,3480395.36 5483811.12,3480404.55 5483810.46,3480405.951 5483810.295,3480406.312 5483795.106,3480405.951 5483810.296,3480406.903 5483810.184,3480407.01 5483810.171))",
            bg::select_gapped_spike<>(10.0, 0.1), 435.535, 92.8071);

    /*
    TODO: for this distance point-segment should be used during marking
    test_geometry<polygon>("pg2",
            "POLYGON((3415632.49 5291021.49,3415632.488 5291021.494,3415632.49 5291021.494,3415628.93 5291028.28,3415642.95 5291001.56,3415651.18 5290985.86,3415659.27 5290984.61,3415644.71 5290947.81,3415629.17 5290921.83,3415621.28 5290929.72,3415640.21 5290959.43,3415625.38 5290971.41,3415627.79 5290983.94,3415629.49 5290992.19,3415630.14 5290995.36,3415625.65 5291022.5,3415632.49 5291021.49))",
            bg::select_gapped_spike<>(20.0, 0.5), 1535.72, 257.895);
    */


    // Testcases from original/previous "remove_spikes" test procedure (these are spikes with area 0.0)
    {
        bg::select_gapped_spike<> policy(0.001, 0.001);

        test_geometry<polygon>("box",
                "POLYGON((0 0,0 4,4 4,4 0,0 0))",
                policy, 16, 16);
        test_geometry<polygon>("spike_right",
                "POLYGON((0 0,0 4,4 4,4 2,6 2,4 2,4 0,0 0))",
                policy, 16, 16);
        test_geometry<polygon>("spike_at_first",
                "POLYGON((0 0,-1 3,0 0,0 4,4 4,4 0,0 0))",
                policy, 16, 16);
        test_geometry<polygon>("spike_at_closing",
                "POLYGON((-1 0,0 0,0 4,4 4,4 0,0 0,-1 0))",
                policy, 16, 16);
        test_geometry<polygon>("double_spike",
                "POLYGON((0 0,0 4,4 4,4 2,6 2,5 2,4 2,4 0,0 0))",
                policy, 16, 16);
        test_geometry<polygon>("three_double_spike",
                "POLYGON((0 0,0 4,4 4,4 2,6 2,5 2,4.5 2,4 2,4 0,0 0))",
                policy, 16, 16);
    }
}

int test_main(int, char* [])
{
    test_all<bg::model::d2::point_xy<double> >();
    return 0;
}
#endif
