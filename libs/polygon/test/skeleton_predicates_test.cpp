// Boost.Polygon library skeleton_predicates_test.cpp file

//          Copyright Andrii Sydorchuk 2012.
//          Copyright Lucanus Simonson 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

// See http://www.boost.org for updates, documentation, and revision history.

#define BOOST_TEST_MODULE skeleton_predicates_test

#include <boost/test/test_case_template.hpp>
#include <boost/polygon/detail/skeleton_predicates.hpp>
using boost::polygon::detail::int32;
using boost::polygon::detail::fpt64;
using boost::polygon::detail::intersection_event;
using boost::polygon::detail::skeleton_ctype_traits;
using boost::polygon::detail::skeleton_predicates;

#include <boost/polygon/point_data.hpp>
#include <boost/polygon/segment_data.hpp>
using boost::polygon::point_data;
using boost::polygon::segment_data;

typedef point_data<int32> point_type;
typedef segment_data<int32> segment_type;
typedef skeleton_ctype_traits<int32> ctraits_type;
typedef intersection_event<fpt64> event_type;
skeleton_predicates<ctraits_type>::event_formation_predicate<
    segment_data<int32>, intersection_event<fpt64> > create_event;

BOOST_AUTO_TEST_CASE(event_predicate_test1) {
  point_type point1(0, 0);
  point_type point2(0, 4);
  point_type point3(4, 4);
  point_type point4(4, 0);
  segment_type segment1(point1, point2);
  segment_type segment2(point2, point3);
  segment_type segment3(point3, point4);
  event_type e;
  BOOST_CHECK(create_event(segment1, segment2, segment3, &e));
  BOOST_CHECK_EQUAL(e.x(), 2);
  BOOST_CHECK_EQUAL(e.y(), 2);
  BOOST_CHECK_EQUAL(e.r(), 2);
}

BOOST_AUTO_TEST_CASE(event_predicate_test2) {
  point_type point1(0, 0);
  point_type point2(0, 4);
  point_type point3(4, 4);
  point_type point4(4, 0);
  segment_type segment1(point4, point3);
  segment_type segment2(point3, point2);
  segment_type segment3(point2, point1);
  event_type e;
  BOOST_CHECK(create_event(segment1, segment2, segment3, &e));
  BOOST_CHECK_EQUAL(e.x(), 2);
  BOOST_CHECK_EQUAL(e.y(), 2);
  BOOST_CHECK_EQUAL(e.r(), 2);
}
