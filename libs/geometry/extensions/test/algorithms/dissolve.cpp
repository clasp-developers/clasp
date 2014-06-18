// Boost.Geometry (aka GGL, Generic Geometry Library)
// Unit Test

// Copyright (c) 2010-2012 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

//#define BOOST_GEOMETRY_DEBUG_ENRICH
//#define BOOST_GEOMETRY_DEBUG_SEGMENT_IDENTIFIER

#include <geometry_test_common.hpp>

#include <boost/geometry/extensions/algorithms/dissolve.hpp>
#include <boost/geometry/extensions/multi/algorithms/dissolve.hpp>

// To check results
#include <boost/geometry/algorithms/length.hpp>
#include <boost/geometry/algorithms/area.hpp>
#include <boost/geometry/algorithms/num_points.hpp>
#include <boost/geometry/algorithms/unique.hpp>

#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/geometries/point_xy.hpp>

#include <boost/geometry/strategies/strategies.hpp>

#include <boost/geometry/io/wkt/wkt.hpp>
#include <boost/geometry/multi/io/wkt/wkt.hpp>

#include <boost/geometry/multi/algorithms/for_each.hpp>

#include <boost/geometry/multi/geometries/multi_linestring.hpp>
#include <boost/geometry/multi/geometries/multi_polygon.hpp>


#if defined(TEST_WITH_SVG)
#  include <boost/geometry/io/svg/svg_mapper.hpp>
#  include <boost/geometry/io/svg/write_svg_multi.hpp>
#endif

template <typename Mapper>
struct map_segment
{
    map_segment(Mapper& m)
        : m_mapper(&m)
    {}

    map_segment<Mapper>& operator=(map_segment<Mapper> const& other)
    {
        if(this != &other)
        {
            this->m_mapper = other.m_mapper;
        }
        return *this;
    }


    template <typename Segment>
    inline void operator()(Segment const& s)
    {
        // create a little offset
        m_mapper->map(s, "opacity:0.6;fill:none;stroke:rgb(0,0,0);stroke-width:2");
    }

    Mapper* m_mapper;
};


template <typename GeometryOut, typename Geometry>
void test_dissolve(std::string const& caseid, Geometry const& geometry,
        std::size_t expected_hole_count, std::size_t expected_point_count,
        double expected_length_or_area, double percentage)
{
    typedef typename bg::coordinate_type<Geometry>::type coordinate_type;

    static const bool is_line = bg::geometry_id<GeometryOut>::type::value == 2;

    //std::cout << bg::area(geometry) << std::endl;

    std::vector<GeometryOut> dissolved_vector;
    bg::dissolve_inserter<GeometryOut>(geometry, std::back_inserter(dissolved_vector));

    typename bg::default_area_result<Geometry>::type length_or_area = 0;
    //std::size_t holes = 0;
    std::size_t count = 0;

    BOOST_FOREACH(GeometryOut& dissolved, dissolved_vector)
    {
        bg::unique(dissolved);

        length_or_area +=
            is_line ? bg::length(dissolved) : bg::area(dissolved);

        //holes += bg::num_interior_rings(dissolved);
        count += bg::num_points(dissolved);
    }

    BOOST_CHECK_MESSAGE(count == expected_point_count,
            "dissolve: " << caseid
            << " #points expected: " << expected_point_count
            << " detected: " << count
            << " type: " << string_from_type<coordinate_type>::name()
            );


    //BOOST_CHECK_EQUAL(holes, expected_hole_count);
    BOOST_CHECK_CLOSE(length_or_area, expected_length_or_area, percentage);

    // Compile check, it should also compile inplace, outputting to the same geometry
    {
        std::vector<GeometryOut> dissolved;
        bg::dissolve(geometry, dissolved);
    }


#if defined(TEST_WITH_SVG)
    {
        std::ostringstream filename;
        filename << "dissolve_"
            << caseid << "_"
            << string_from_type<coordinate_type>::name()
            << ".svg";

        std::ofstream svg(filename.str().c_str());

        typedef
        bg::svg_mapper
            <
                typename bg::point_type<Geometry>::type
            > mapper_type;

        mapper_type mapper(svg, 500, 500);
        mapper.add(geometry);

        mapper.map(geometry, "opacity:0.6;fill:rgb(0,0,255);stroke:rgb(0,0,0);stroke-width:1;fill-rule:nonzero");

        bg::for_each_segment(geometry, map_segment<mapper_type>(mapper));


        BOOST_FOREACH(GeometryOut& dissolved, dissolved_vector)
        {
           mapper.map(dissolved, "opacity:0.6;fill:none;stroke:rgb(255,0,0);stroke-width:5");
        }
    }
#endif
}


template <typename Geometry, typename GeometryOut>
void test_one(std::string const& caseid, std::string const& wkt,
        std::size_t expected_hole_count, std::size_t expected_point_count,
        double expected_length_or_area, double percentage = 0.001)
{
    Geometry geometry;
    bg::read_wkt(wkt, geometry);

    test_dissolve<GeometryOut>(caseid, geometry,
        expected_hole_count, expected_point_count,
        expected_length_or_area, percentage);

#ifdef BOOST_GEOMETRY_TEST_MULTI_PERMUTATIONS
    // Test different combinations of a multi-polygon

    int n = geometry.size();

    // test them in all orders
    std::vector<int> indices;
    for (int i = 0; i < n; i++)
    {
        indices.push_back(i);
    }
    int permutation = 0;
    do
    {
        std::ostringstream out;
        out << caseid;
        Geometry geometry2;
        for (int i = 0; i < n; i++)
        {
            int index = indices[i];
            out << "_" << index;
            geometry2.push_back(geometry[index]);
        }
        test_dissolve<GeometryOut>(out.str(), geometry2, expected_hole_count,
                expected_point_count, expected_length_or_area, percentage);
    } while (std::next_permutation(indices.begin(), indices.end()));
#endif

}




template <typename P>
void test_all()
{
    typedef bg::model::ring<P> ring;
    typedef bg::model::polygon<P> polygon;

    // Simplex
    test_one<polygon, polygon>("1",
        "POLYGON((0 0,0 4,1.5 2.5,2.5 1.5,4 0,0 0))",
        0, 6, 8);

    // Self intersecting
    test_one<polygon, polygon>("2",
        "POLYGON((1 2,1 1,2 1,2 2.25,3 2.25,3 0,0 0,0 3,3 3,2.75 2,1 2))",
        1, 12, 7.9296875);

    // Self intersecting in last segment
    test_one<polygon, polygon>("3",
        "POLYGON((0 2,2 4,2 0,4 2,0 2))",
        0, 8, 4.0);

    // Self tangent
    test_one<polygon, polygon>("4",
        "POLYGON((0 0,0 4,4 4,4 0,2 4,0 0))",
        0, 8, 8.0);


    // Self tangent in corner
    test_one<polygon, polygon>("5",
        "POLYGON((0 0,0 4,4 4,4 0,0 4,2 0,0 0))",
        0, 8, 12.0);


    // With spike
    test_one<polygon, polygon>("6",
        "POLYGON((0 0,0 4,4 4,4 2,6 2,4 2,4 0,0 0))",
        0, 6, 16);


    // Non intersection, but with duplicate
    test_one<polygon, polygon>("d1",
        "POLYGON((0 0,0 4,4 0,4 0,0 0))",
        0, 4, 8);


    // With many duplicates
    test_one<polygon, polygon>("d2",
        "POLYGON((0 0,0 1,0 1,0 1,0 2,0 2,0 3,0 3,0 3,0 3,0 4,2 4,2 4,4 4,4 0,4 0,3 0,3 0,3 0,3 0,3 0,0 0))",
        0, 10, 16);

    // Hole: interior tangent to exterior
    test_one<polygon, polygon>("h1",
        "POLYGON((0 0,0 4,4 4,4 0,0 0),(1 2,2 4,3 2,1 2))",
        0, 6, 16);

    // Hole: interior intersecting exterior
    test_one<polygon, polygon>("h2",
        "POLYGON((0 0,0 4,4 4,4 0,0 0),(1 1,1 3,5 4,1 1))",
        0, 8, 16.25);

    // Hole: two intersecting holes
    test_one<polygon, polygon>("h3",
        "POLYGON((0 0,0 4,4 4,4 0,0 0),(1 1,1 3,3 3,3 1,1 1),(2 2,2 3.5,3.5 3.5,3.5 2,2 2))",
        0, 5, 16);

    // Hole: self-intersecting hole
    test_one<polygon, polygon>("h4",
        "POLYGON((0 0,0 4,4 4,4 0,0 0),(1 1,3 3,3 2.5,1 3.5,1.5 3.5,1 1))",
        1, 9, 14.484848484848484);

    // See power point
    test_one<polygon, polygon>("case_1",
        "POLYGON((1 3,0 9,9 5,1 7,9 8,2 5,10 10,9 2,1 3))",
        0, 7, 50.48056402439);

    // See power point, and http://en.wikipedia.org/wiki/Pentagram
    test_one<polygon, polygon>("pentagram",
        "POLYGON((5 0,2.5 9,9.5 3.5,0.5 3.5,7.5 9,5 0))",
        0, 11, 25.6158412);


    // Multi-geometries
    {
        typedef bg::model::multi_polygon<polygon> multi_polygon;

        test_one<multi_polygon, polygon>("three_triangles",
            "MULTIPOLYGON(((1 1,5 5,8 0,1 1)),((4 2,0 8,5 9,4 2)),((5 3,4 8,10 4,5 3)))" ,
            1, 13, 42.614078674948232);

        test_one<multi_polygon, polygon>("simplex_two",
            "MULTIPOLYGON(((0 0,1 4,4 1,0 0)),((2 2,3 6,6 3,2 2)))",
            0, 8, 14.7);
        test_one<multi_polygon, polygon>("simplex_three",
            "MULTIPOLYGON(((0 0,1 4,4 1,0 0)),((2 2,3 6,6 3,2 2)),((3 4,5 6,6 2,3 4)))",
            0, 14, 16.7945);
        test_one<multi_polygon, polygon>("simplex_four",
            "MULTIPOLYGON(((0 0,1 4,4 1,0 0)),((2 2,3 6,6 3,2 2)),((3 4,5 6,6 2,3 4)),((5 5,7 7,8 4,5 5)))",
            0, 18, 20.7581);

        // disjoint
        test_one<multi_polygon, polygon>("simplex_disjoint",
            "MULTIPOLYGON(((0 0,1 4,4 1,0 0)),((1 6,2 10,5 7,1 6)),((3 4,5 6,6 2,3 4)),((6 5,8 7,9 4,6 5)))",
            0, 16, 24.0);

        // new hole of four
        test_one<multi_polygon, polygon>("new_hole",
            "MULTIPOLYGON(((0 0,1 4,4 1,0 0)),((2 2,3 6,6 3,2 2)),((3 4,5 6,6 2,3 4)),((3 1,5 4,8 4,3 1)))",
            1, 18, 19.5206);

        // GGL mailing list - report Javier - 2011, March 7
        test_one<multi_polygon, polygon>("ggl_list_20110307_javier_01_a",
            "MULTIPOLYGON(((560 -400, 600 -400, 600 -440, 560 -440, 560 -400)), ((480 -400, 520 -400, 520 -440, 480 -440, 480 -400)), ((600 -320, 640 -320, 640 -360, 600 -360, 600 -320)), ((520 -400, 560 -400, 560 -440, 520 -440, 520 -400)))",
            1, 14, 6400);
        test_one<polygon, polygon>("ggl_list_20110307_javier_01_b",
            "POLYGON((0 0, 2000 0, 2000 -2000, 0 -2000, 0 0), (560 -400, 560 -440, 600 -440, 600 -400, 560 -400), (480 -400, 480 -440, 520 -440, 520 -400, 480 -400), (600 -320, 600 -360, 640 -360, 640 -320, 600 -320), (520 -400, 520 -440, 560 -440, 560 -400, 520 -400))",
            1, 19, 3993600);
    }



/*
    //Should be solved (completely) differently
    // From mail on the ggl-mailing list
    test_one<polygon, polygon>("mail_denis_1",
        "POLYGON((55 10, 141 237, 249 23, 21 171, 252 169, 24 89, 266 73, 55 10))",
        0, 7, 50.48056402439);
    // Source: http://upload.wikimedia.org/wikipedia/commons/8/83/Acute_heptagram.svg
    test_one<polygon, polygon>("acute_heptagram",
        "POLYGON((409 5,229 793.631528,733.348792 161.198146,4.543671 512.172194,813.456329 512.172194,84.651208 161.198146,589 793.631528))",
        0, 11, 25.6158412);

    // Source: http://upload.wikimedia.org/wikipedia/commons/a/a7/Obtuse_heptagram.svg
    test_one<polygon, polygon>("obtuse_heptagram",
        "POLYGON((409 5,813.456329 512.172194,229 793.631528,84.651208 161.198146,733.348792 161.198146,589 793.631528,4.543671 512.172194))",
        0, 11, 25.6158412);
*/


    std::string const ticket17 = "POLYGON ((-122.28139163 37.37319149,-122.28100699 37.37273669,-122.28002186 37.37303123,-122.27979681 37.37290072,-122.28007349 37.37240493,-122.27977334 37.37220360,-122.27819720 37.37288580,-122.27714184 37.37275161,-122.27678628 37.37253167,-122.27766437 37.37180973,-122.27804382 37.37121453,-122.27687664 37.37101354,-122.27645829 37.37203386,-122.27604423 37.37249110,-122.27632234 37.37343339,-122.27760980 37.37391082,-122.27812478 37.37800320,-122.26117222 37.39121007,-122.25572289 37.39566631,-122.25547269 37.39564971,-122.25366304 37.39552993,-122.24919976 37.39580268,-122.24417933 37.39366907,-122.24051443 37.39094143,-122.23246277 37.38100418,-122.23606766 37.38141338,-122.24001587 37.37738940,-122.23666848 37.37609347,-122.23057450 37.37882170,-122.22679803 37.37807143,-122.22525727 37.37448817,-122.22523229 37.37443000,-122.23083199 37.37609347,-122.23033486 37.37777891,-122.23169030 37.37732117,-122.23229178 37.37709687,-122.23237761 37.37631249,-122.23297776 37.37438834,-122.23872850 37.37165986,-122.24044511 37.36934068,-122.24671067 37.36865847,-122.24825570 37.36981819,-122.25151719 37.36947713,-122.25357721 37.36756706,-122.26001451 37.36579354,-122.25615213 37.36545239,-122.25486458 37.36245083,-122.25357721 37.36108651,-122.25194642 37.36013139,-122.24885652 37.35958557,-122.24911401 37.35849399,-122.25357721 37.35808470,-122.25675286 37.35897159,-122.25855539 37.35753887,-122.26181687 37.35828939,-122.26713837 37.35897159,-122.26782510 37.36108651,-122.26662339 37.36456559,-122.27288911 37.36722601,-122.27366159 37.36531602,-122.27168740 37.36470213,-122.27391900 37.36374701,-122.27074326 37.36245083,-122.27134408 37.35951742,-122.27426240 37.36135926,-122.27709482 37.36115474,-122.27966974 37.36231438,-122.27958391 37.36463382,-122.27572152 37.36463382,-122.27563569 37.36524779,-122.27700899 37.36593000,-122.27709482 37.36763529,-122.27554978 37.36838573,-122.27667254 37.36931478,-122.27677932 37.36932073,-122.27769362 37.36853987,-122.27942490 37.36830803,-122.28178776 37.36677917,-122.28509559 37.36443500,-122.28845129 37.36413744,-122.29194403 37.36695946,-122.29382577 37.36726817,-122.29600414 37.36898512,-122.29733083 37.36995398,-122.29593239 37.37141436,-122.29416649 37.37075898,-122.29325026 37.37108436,-122.29652910 37.37311697,-122.29584237 37.37374461,-122.29537583 37.37573372,-122.29487677 37.37752502,-122.30923212 37.37593011,-122.31122484 37.38230086,-122.31467994 37.38092472,-122.31715663 37.38252181,-122.32307970 37.38166978,-122.31985618 37.37667694,-122.32210304 37.37580220,-122.32581446 37.37589532,-122.32401730 37.37331839,-122.32960417 37.37189020,-122.33465527 37.37331906,-122.33425328 37.37623680,-122.33620676 37.37726132,-122.33397986 37.37822382,-122.33358918 37.38036590,-122.33202637 37.37986918,-122.33147954 37.38101784,-122.33394080 37.38198017,-122.33545239 37.38587943,-122.33478058 37.38785697,-122.33386050 37.38723721,-122.33350041 37.38571137,-122.33122003 37.38548891,-122.33140008 37.38650606,-122.33366042 37.38817490,-122.33244019 37.39157602,-122.33298157 37.39419201,-122.33164013 37.39477028,-122.33202017 37.39518351,-122.33358038 37.39499282,-122.33376050 37.39597811,-122.33550067 37.39734478,-122.33556069 37.39481797,-122.33344040 37.39292676,-122.33638094 37.38892189,-122.34240644 37.38852719,-122.34906293 37.38726898,-122.35072321 37.39338769,-122.34910291 37.39445252,-122.34796272 37.39410291,-122.34449043 37.39640534,-122.34500223 37.39729709,-122.34936291 37.39670910,-122.35098322 37.39531066,-122.35364623 37.39554510,-122.35434369 37.39612111,-122.35798429 37.39600988,-122.35768430 37.39478621,-122.36334519 37.39206871,-122.36604726 37.39203267,-122.36778592 37.39335592,-122.36518870 37.40022011,-122.36554552 37.40247752,-122.36370519 37.40331974,-122.36270506 37.40530591,-122.36320512 37.40670418,-122.36149849 37.40851392,-122.36730580 37.41054938,-122.37263720 37.41378932,-122.37161871 37.42076600,-122.36566153 37.42006292,-122.36520547 37.42742106,-122.37165953 37.43661157,-122.35943972 37.44459022,-122.35356359 37.44600810,-122.33792254 37.45796329,-122.35228518 37.47478091,-122.35127080 37.48181199,-122.34867342 37.48487322,-122.34359717 37.48801082,-122.33388431 37.48677650,-122.33142321 37.48429747,-122.32929580 37.48473149,-122.32609609 37.48291144,-122.32344850 37.48228229,-122.31924364 37.48410234,-122.31677299 37.48114051,-122.31431751 37.47848973,-122.31259201 37.47682190,-122.31515972 37.47568196,-122.31691389 37.47360309,-122.31292494 37.46960081,-122.31130153 37.46937743,-122.30889894 37.47124987,-122.30612839 37.47011613,-122.30149630 37.46568378,-122.30064277 37.46363784,-122.29283821 37.45922376,-122.28630141 37.45415497,-122.28883099 37.44629920,-122.28316717 37.44197138,-122.27554148 37.42297597,-122.25597410 37.40553692,-122.25196579 37.40129593,-122.25012043 37.40049143,-122.24823207 37.39897758,-122.24754551 37.39740941,-122.24778582 37.39621607,-122.24934787 37.39599102,-122.25005170 37.39871849,-122.25222328 37.39863668,-122.25342491 37.39737529,-122.25520162 37.39667289,-122.25528737 37.39522726,-122.27747460 37.37809616,-122.27977493 37.37858717,-122.28157729 37.37920106,-122.28322534 37.37952846,-122.28416939 37.38092656,-122.28621223 37.37984219,-122.28638389 37.37613857,-122.28382607 37.37843722,-122.27930278 37.37718220,-122.28196361 37.37652740,-122.28295058 37.37568167,-122.28216101 37.37523148,-122.28114822 37.37543608,-122.27934569 37.37528613,-122.27996369 37.37448121,-122.28104521 37.37454944,-122.28185197 37.37422883,-122.28290767 37.37474038,-122.28376597 37.37467224,-122.28428104 37.37399012,-122.28402346 37.37338989,-122.28610922 37.37364914,-122.28651264 37.37327388,-122.28672722 37.37207343,-122.28628398 37.37205448,-122.28574460 37.37166682,-122.28479711 37.37200981,-122.28327731 37.37137228,-122.28285511 37.37100700,-122.28279409 37.37125669,-122.28315527 37.37173756,-122.28321872 37.37220569,-122.28187007 37.37231918,-122.28193109 37.37294908,-122.28139163 37.37319149))";
    test_one<polygon, polygon>("ticket17", ticket17,
        1, 228, 0.00920834633689);

    return; // next one does not work for gcc/linux
    // Real-life
    std::string const toolkit = "POLYGON((170718 605997,170718 605997,170776 606016,170773 606015,170786 606020,170778 606016,170787 606021,170781 606017,170795 606028,170795 606028,170829 606055,170939 606140,170933 605968,170933 605968,170932 605908,170929 605834,170920 605866,170961 605803,170739 605684,170699 605749,170691 605766,170693 605762,170686 605775,170688 605771,170673 605794,170676 605790,170668 605800,170672 605796,170651 605818,170653 605816,170639 605829,170568 605899,170662 605943,170633 605875,170603 605961,170718 605997))";
    test_one<polygon, polygon>("toolkit", toolkit,
           0, 25, 91756.916526794434);

}


int test_main(int, char* [])
{
    test_all<bg::model::d2::point_xy<double> >();
    return 0;
}
