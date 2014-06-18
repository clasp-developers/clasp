// Boost.Geometry (aka GGL, Generic Geometry Library)
// Unit Test

// Copyright (c) 2010-2012 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

//#define HAVE_TTMATH
#ifdef HAVE_TTMATH
#include <boost/geometry/extensions/contrib/ttmath_stub.hpp>
#endif


#include <geometry_test_common.hpp>

#include <test_buffer.hpp>

#include <boost/geometry/multi/multi.hpp> // TODO: more specific
#include <boost/geometry/multi/geometries/multi_geometries.hpp>
#include <boost/geometry/extensions/algorithms/buffer/multi_buffer_inserter.hpp>




static std::string const simplex
    = "MULTIPOLYGON(((0 1,2 5,5 3,0 1)),((1 1,5 2,5 0,1 1)))";

static std::string const zonethru
    = "MULTIPOLYGON(((0 0,0 6,5 6,5 4,3 4,3 0,0 0)),((5 0,5 2,7 2,7 6,10 6,10 0,5 0)))";

static std::string const wrapped
    = "MULTIPOLYGON(((0 0,0 10,10 10,10 0,0 0),(2 2,8 2,8 8,2 8,2 2)),((4 4,4 6,6 6,6 4,4 4)))";

static std::string const triangles
    = "MULTIPOLYGON(((0 4,3 0,-2.5 -1,0 4)),((3 8,5.5 13,8 8,3 8)),((11 4,13.5 -1,8 0,11 4)))";

// From robustness tests

// Case with duplicate points (due to chained boxes) (round)
static std::string const rt_a
    = "MULTIPOLYGON(((2 7,2 8,3 8,3 7,2 7)),((5 4,5 5,6 5,6 4,5 4)),((5 8,6 8,6 7,6 6,5 6,5 7,4 7,4 8,5 8)),((3 5,4 5,4 4,3 4,2 4,2 5,3 5)))";

// Case with u-u (miter)
static std::string const rt_b
    = "MULTIPOLYGON(((8 4,8 5,9 5,9 4,8 4)),((6 2,6 3,7 3,7 2,6 2)),((8 0,8 1,9 1,9 0,8 0)),((9 7,9 8,10 8,10 7,9 7)))";

// Case with geometry::equals( turn.point(7.0000000000000000, 4.3368086899420177e-019), helper_segment(7.0000000000000000, 0.00000000000000000))) (round)
static std::string const rt_c
	= "MULTIPOLYGON(((6 1,6 2,7 2,7 1,6 1)),((8 0,8 1,9 1,9 0,8 0)))";

// Case with round corner on same perpendicular points (round)
static std::string const rt_d
	= "MULTIPOLYGON(((2 2,2 3,3 2,2 2)),((2 5,2 6,3 5,2 5)),((2 4,2 5,3 4,2 4)),((3 2,3 3,4 2,3 2)),((4 4,4 5,5 4,4 4)),((5 6,5 5,4 5,4 6,5 7,5 6)),((2 2,3 1,3 0,2 0,2 1,1 1,1 2,2 2)),((1 3,1 2,0 2,1 3)),((1 4,2 4,2 3,1 3,1 4)))";

// Case with missing turning point (miter) and many intersections (round, OK)
static std::string const rt_e
	= "MULTIPOLYGON(((0 6,0 7,1 6,0 6)),((3 7,3 8,4 8,4 7,3 7)),((4 6,4 7,5 7,4 6)),((3 6,3 7,4 6,3 6)),((1 9,2 10,2 9,1 9)),((1 9,1 8,0 8,0 9,1 9)),((3 5,3 4,2 4,2 5,2 6,3 5)))";

// Extract of e (miter)
static std::string const rt_f
	= "MULTIPOLYGON(((0 6,0 7,1 6,0 6)),((1 9,1 8,0 8,0 9,1 9)))";

// Robustness / turn problem (collinearity with turn after), solved in get_turn-info
static std::string const rt_g1
	= "MULTIPOLYGON(((3 8,3 9,4 9,3 8)),((7 5,7 6,8 5,7 5)),((1 8,1 9,2 9,1 8)),((1 6,1 7,2 7,1 6)))";

static std::string const rt_g2
    = "MULTIPOLYGON(((6 5,7 5,6 4,5 4,5 5,6 5)),((4 2,4 3,5 3,4 2)))";

static std::string const rt_g3
    = "MULTIPOLYGON(((4 2,5 3,5 2,4 2)),((2 0,3 1,3 0,2 0)))";


// IP on match of 3 lines
static std::string const rt_h
	= "MULTIPOLYGON(((4 7,4 8,5 7,4 7)),((4 8,5 9,5 8,4 8)),((9 1,10 2,10 1,9 1)),((4 1,4 2,5 2,4 1)),((2 9,2 10,3 10,2 9)),((7 7,8 8,8 7,7 7)),((3 4,4 4,3 3,2 3,2 4,3 4)))";

// r=1.16 (touching)
static std::string const rt_i
	= "MULTIPOLYGON(((2 1,2 2,3 2,2 1)),((3 2,3 3,4 3,3 2)))";

// r=1.16 (touching in the middle)
static std::string const rt_j
	= "MULTIPOLYGON(((2 4,2 5,3 5,2 4)),((5 3,5 4,6 4,5 3)),((9 4,9 5,10 5,10 4,9 4)),((0 2,0 3,1 3,0 2)))";

// Occupied (but not completely - due to duplicate point there)
static std::string const rt_k
	= "MULTIPOLYGON(((3 7,3 8,4 8,3 7)),((0 4,0 5,1 5,0 4)),((3 5,3 6,4 6,4 5,3 5)),((2 6,3 7,3 6,2 6)),((3 2,3 1,2 1,2 2,3 3,3 2)),((0 9,1 10,1 9,1 8,0 8,0 9)))";

// Segment-intersection problem (nearly collinear not reported as collinear), fixed
static std::string const rt_l
	= "MULTIPOLYGON(((2 5,2 6,3 5,2 5)),((6 1,6 2,7 1,6 1)))";

// Segment-intersection problem (missed touch because of robustness) (with 4), fixed
static std::string const rt_m1
	= "MULTIPOLYGON(((4 2,4 3,5 2,4 2)),((1 2,2 3,2 2,1 2)))";

// Same, with 2
static std::string const rt_m2
	= "MULTIPOLYGON(((0 3,1 4,1 3,0 3)),((3 6,4 7,4 6,4 5,3 5,3 6)))";


// Segment-intersection problem (disjoint nearly collinear segments were reported as intersecting), fixed.
static std::string const rt_n
	= "MULTIPOLYGON(((0 0,1 1,1 0,0 0)),((3 3,4 4,4 3,3 3)))";

// Segment intersection of 3 pieces in one point, plus all intersection points are within the other piece (due to precision)
static std::string const rt_o1
    = "MULTIPOLYGON(((8 4,8 5,9 5,8 4)),((9 4,10 5,10 4,9 4)),((6 2,6 3,7 3,6 2)))";

static std::string const rt_o2
    = "MULTIPOLYGON(((8 4,8 5,9 5,8 4)),((6 5,7 4,6 4,6 5)),((9 4,10 5,10 4,9 4)),((6 2,6 3,7 3,6 2)))";

static std::string const rt_o3
    = "MULTIPOLYGON(((8 4,8 5,9 5,8 4)),((6.5 5,7.5 4,6.5 4,6.5 5)),((8.5 4,9.5 5,9.5 4,8.5 4)),((6 2,6 3,7 3,6 2)),((10 4,11 5,11 4,10 4)))";

static std::string const rt_o4
    = "MULTIPOLYGON(((8 4,8 5,9 5,8 4)),((6.5 5,7.5 4,6.5 4,6.5 5)),((8.5 4,9.5 5,9.5 4,8.5 4)),((6 2,6 3,7 3,6 2)),((10 4,11 5,11 4,10 4)),((10 3,11 3,11 2,10 3)))";

// Occupied - intersection
static std::string const rt_p1
    = "MULTIPOLYGON(((5 2,5 3,6 3,6 2,5 2)),((8 0,8 1,9 0,8 0)),((8 2,9 3,9 2,8 2)))";

static std::string const rt_p2
    = "MULTIPOLYGON(((9 3,9 4,10 3,9 3)),((9 4,9 5,10 5,9 4)),((8 6,9 7,9 6,8 6)))";

static std::string const rt_p3
    = "MULTIPOLYGON(((3 8,3 9,4 9,3 8)),((3 7,3 8,4 8,3 7)),((0 8,0 9,1 8,0 8)))";

// Occupation map: robustness, nearly collinear, both incoming
static std::string const rt_p4
    = "MULTIPOLYGON(((8 8,9 9,9 8,8 8)),((5 8,5 9,6 9,5 8)),((6 5,6 6,7 6,6 5)),((4 7,4 8,5 8,4 7)))";

// Occupation map: Two collinear segments intersecting one segment
static std::string const rt_p5
    = "MULTIPOLYGON(((3 2,4 3,4 2,3 2)),((2 2,2 3,3 3,3 2,2 2)),((4 3,5 4,5 3,4 3)))";

// Occupied map: only two pieces involved so leave early
static std::string const rt_p6
    = "MULTIPOLYGON(((7 3,7 4,8 3,7 3)),((7 6,8 7,8 6,7 6)))";

// Occupation map: needing relaxed_less for map
static std::string const rt_p7
    = "MULTIPOLYGON(((6 6,7 7,7 6,6 6)),((3 4,3 5,4 4,3 4)),((2 6,3 7,3 6,2 6)))";

// Occupation map: needing relaxed_less PLUS relaxed_equals for map
static std::string const rt_p8
    = "MULTIPOLYGON(((4 7,4 8,5 7,4 7)),((5 3,6 4,6 3,5 3)),((8 5,8 6,9 6,8 5)))";

// Occupation map: needing go back for extra outgoing vectors too
static std::string const rt_p9
    = "MULTIPOLYGON(((1 6,1 7,2 6,1 6)),((4 3,4 4,5 3,4 3)),((3 4,2 3,2 4,3 5,3 4)))";

static std::string const rt_p10
    = "MULTIPOLYGON(((6 6,7 7,7 6,6 6)),((4 6,5 7,5 6,4 6)),((8 8,9 9,9 8,8 8)))";

// Occupation map, multiple back/forward cases and combinations
static std::string const rt_p11
    = "MULTIPOLYGON(((0 8,0 9,1 9,1 8,0 8)),((2 8,2 9,3 9,3 8,2 8)),((3 8,4 9,4 8,3 8)),((4 1,4 2,5 2,4 1)))";

// Occupation map - incoming angle, non-zero
static std::string const rt_p12
    = "MULTIPOLYGON(((8 4,9 5,9 4,8 4)),((5 5,5 6,6 6,6 5,5 5)),((8 4,8 3,7 3,8 4)))";

// Occupation map - outgoing angle, 1
static std::string const rt_p13
    = "MULTIPOLYGON(((5 2,6 3,6 2,5 2)),((3 0,4 1,4 0,3 0)),((3 1,3 2,4 2,4 1,3 1)))";

// Occupation map - touch which was originally in other segment-id
static std::string const rt_p14
    = "MULTIPOLYGON(((9 9,10 10,10 9,9 9)),((7 7,8 8,8 7,7 7)),((6 6,6 7,7 7,7 6,6 6)))";

// Occupation map - needing measuring sides from original point and not center point
static std::string const rt_p15
    = "MULTIPOLYGON(((5 2,5 3,6 3,6 2,5 2)),((4 1,5 2,5 1,4 1)),((8 5,9 6,9 5,8 5)))";

// Occupation map - needing other approach w.r.t. discarding (collinear) segments
static std::string const rt_p16
    = "MULTIPOLYGON(((5 7,5 8,6 7,5 7)),((9 3,9 4,10 3,9 3)),((6 7,7 8,7 7,6 7)))";

// Occupation map - outputting two valid turns (resulting in the wrong choice in the end)
static std::string const rt_p17
    = "MULTIPOLYGON(((4 8,5 9,5 8,4 8)),((1 8,2 9,2 8,1 8)),((2 6,3 7,3 6,2 6)))";

// Occupation map - outputting no valid turns (needing to take other turns into account)
static std::string const rt_p18
	= "MULTIPOLYGON(((7 6,8 7,8 6,7 6)),((7 3,7 4,8 3,7 3)),((5 4,6 5,6 4,5 4)))";

// Occupation map - showing wrong approach in p17/p18, now new approach with keep_indices
static std::string const rt_p19
	= "MULTIPOLYGON(((0 5,1 6,1 5,0 5)),((0 7,0 8,1 7,0 7)),((3 4,3 5,4 4,3 4)))";

// Occupation map: two non-collinear segments non-intersecting, needing relaxed_equal
static std::string const rt_p20
	= "MULTIPOLYGON(((2 3,2 4,3 4,3 3,2 3)),((0 5,0 6,1 6,0 5)),((2 7,2 8,3 8,2 7)))";

// Occupation map: turn more right should still be included
static std::string const rt_p21
    = "MULTIPOLYGON(((4 2,4 3,5 3,4 2)),((4 1,5 2,5 1,4 1)),((5 2,6 3,6 2,5 2)))";


static std::string const rt_p22
    = "MULTIPOLYGON(((4 8,5 9,5 8,4 8)),((5 9,6 10,6 9,5 9)),((1 7,1 8,2 8,2 7,1 7)),((2 6,3 7,3 6,2 6)))";


// Occupation map with a uu-turn
static std::string const rt_q1
    = "MULTIPOLYGON(((4 6,4 7,5 7,5 6,4 6)),((1 6,1 7,2 7,2 6,1 6)),((1 9,1 10,2 10,2 9,1 9)))";

// Occupation map with twice a uu-turn
static std::string const rt_q2
    = "MULTIPOLYGON(((0 6,0 7,1 6,0 6)),((2 6,2 7,3 6,2 6)),((4 5,3 5,3 6,4 6,5 6,5 5,4 4,4 5)))";

// Robustness issue related to collinear correction (meeting) and selecting the right axis (x/y)
static std::string const rt_r
    = "MULTIPOLYGON(((3 1,2 0,1 1,2 2,3 1)),((5 3,4 2,3 2,4 4,5 3)))";

// Robustness - flagged by "meeting"
static std::string const rt_s1
    = "MULTIPOLYGON(((4 1,5 2,5 1,4 1)),((5 2,6 3,6 2,5 2)),((7 1,7 2,8 1,7 1)))";

// Robustness - flagged by "disjoint"
static std::string const rt_s2
    = "MULTIPOLYGON(((0 0,1 1,1 0,0 0)),((2 4,2 5,3 4,2 4)),((3.5 3.5,4 4,4 3,3 3,3 4,3.5 3.5)))";

// Robustness issue in get_turn_info (touch, collinear, blocking q)
static std::string const rt_t
    = "MULTIPOLYGON(((1 3,1 4,2 3,1 3)),((1 4,0 3,0 4,0 5,1 4)))";




template <typename P>
void test_all()
{
    namespace buf = bg::strategy::buffer;

    typedef bg::model::polygon<P> polygon_type;
    typedef bg::model::multi_polygon<polygon_type> multi_polygon_type;

    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("triangles424", triangles, 417.910, 4.24);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("triangles425", triangles, 418.918, 4.25);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("triangles426", triangles, 419.927, 4.26);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("zonethru_10", zonethru, 96.0000, 1.0);

    test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("multi_simplex_05", simplex, 23.7030, 0.5);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("multi_simplex_05", simplex, 24.5965, 0.5);
    test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("multi_simplex_10", simplex, 34.2532, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("multi_simplex_10", simplex, 38.1379, 1.0);
    test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("multi_simplex_20", simplex, 59.9159, 2.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("multi_simplex_20", simplex, 77.7060, 2.0);
    test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("multi_simplex_50", simplex, 174.46, 5.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("multi_simplex_50", simplex, 298.797, 5.0);

    // This one does not work:
    // test_one<multi_polygon_type, buf::join_round_by_divide, buf::end_skip, polygon_type>("multi_simplex_50", simplex, 'd', 174.46, 5.0);
    
    test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("zonethru_05", zonethru, 67.4627, 0.5);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("zonethru_05", zonethru, 68.0000, 0.5);
    test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("zonethru_10", zonethru, 93.8508, 1.0, -999, 1);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("zonethru_10", zonethru, 96.0000, 1.0);
    test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("zonethru_15", zonethru, 114.584, 1.5);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("zonethru_15", zonethru, 117.000, 1.5);
    test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("wrapped_05", wrapped, 104.570, 0.5);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("wrapped_05", wrapped, 105.000, 0.5);
    test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("wrapped_10", wrapped, 142.281, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("wrapped_10", wrapped, 144.000, 1.0);
    test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("wrapped_15", wrapped, 167.066, 1.5);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("wrapped_15", wrapped, 169.000, 1.5);

    // TODO: there is still an undetected hole inside rt_a
    test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("rt_a", rt_a, 34.5381, 1.0);

    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_a", rt_a, 36, 1.0);
    test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("rt_b", rt_b, 31.4186, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_b", rt_b, 34, 1.0);
    test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("rt_c", rt_c, 14.7093, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_c", rt_c, 16, 1.0);
    test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("rt_d", rt_d, 18.8726, 0.3);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_d", rt_d, 19.8823, 0.3);
    test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("rt_e", rt_e, 14.1866, 0.3);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_e", rt_e, 15.1198, 0.3);
    // This does not add anything: test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("rt_f", rt_f, 4.28937, 0.3);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_f", rt_f, 4.60853, 0.3);

    test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("rt_g1", rt_g1, 24.719, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_g1", rt_g1, 30.3137, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_g2", rt_g2, 18.5711, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_g3", rt_g3, 16.5711, 1.0);

    test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("rt_h", rt_h, 47.6012, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_h", rt_h, 61.7058, 1.0);
    test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("rt_i", rt_i, 10.7528, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_i", rt_i, 13.6569, 1.0);
    test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("rt_j", rt_j, 28.7309, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_j", rt_j, 35.1421, 1.0);
    test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("rt_k", rt_k, 42.0092, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_k", rt_k, 48.0563, 1.0);
    // This does not add anything: test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("rt_l", rt_l, 14.1074, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_l", rt_l, 19.3995, 1.0);
    test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("rt_m1", rt_m1, 14.1074, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_m1", rt_m1, 19.4853, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_m2", rt_m2, 21.4853, 1.0);

    // This does not add anything: test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("rt_n", rt_n,  14.1074, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_n", rt_n,  18.4853, 1.0);

    test_one<multi_polygon_type, buf::join_round, buf::end_skip, polygon_type>("rt_o1", rt_o1, 17.536, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_o1", rt_o1, 20.9142, 1.0);

    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_o2", rt_o2, 25.7426, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_o3", rt_o3, 28.8247, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_o4", rt_o4, 34.6532, 1.0);

    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_p1", rt_p1, 24.8211, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_p2", rt_p2, 21.4853, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_p3", rt_p3, 22.3995, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_p4", rt_p4, 33.0563, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_p5", rt_p5, 17, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_p6", rt_p6, 18.4853, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_p7", rt_p7, 26.2279, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_p8", rt_p8, 29.0563, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_p9", rt_p9, 26.1421, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_p10", rt_p10, 23.3995, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_p11", rt_p11, 28.7426, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_p12", rt_p12, 22.5711, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_p13", rt_p13, 19.9142, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_p14", rt_p14, 20.8284, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_p15", rt_p15, 23.6569, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_p16", rt_p16, 23.4853, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_p17", rt_p17, 25.3137, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_p18", rt_p18, 23.3137, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_p19", rt_p19, 25.5637, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_p20", rt_p20, 25.4853, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_p21", rt_p21, 17.1716, 1.0);
    // test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_p22", rt_p22, 99, 1.0); TODO fix this one

    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_q1", rt_q1, 27, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_q2", rt_q2, 26.4853, 1.0);

    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_r", rt_r, 21.0761, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_s1", rt_s1, 20.4853, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_s2", rt_s2, 24.6495, 1.0);
    test_one<multi_polygon_type, buf::join_miter, buf::end_skip, polygon_type>("rt_t", rt_t, 15.6569, 1.0);


}

int point_buffer_count;

int test_main(int, char* [])
{
    test_all<bg::model::point<double, 2, bg::cs::cartesian> >();
    //test_all<bg::model::point<ttmath_big, 2, bg::cs::cartesian> >();
    
    return 0;
}

