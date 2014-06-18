// Boost.Polygon library detail/skeleton_predicates.hpp header file

//          Copyright Andrii Sydorchuk 2012.
//          Copyright Lucanus Simonson 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

// See http://www.boost.org for updates, documentation, and revision history.

#ifndef BOOST_POLYGON_DETAIL_SKELETON_PREDICATES
#define BOOST_POLYGON_DETAIL_SKELETON_PREDICATES

#ifndef BOOST_POLYGON_NO_DEPS
#include <boost/cstdint.hpp>
#endif

#include <cmath>

#include "voronoi_ctypes.hpp"
#include "voronoi_robust_fpt.hpp"

namespace boost {
namespace polygon {
namespace detail {

template <typename T>
struct skeleton_ctype_traits;

template <>
struct skeleton_ctype_traits<int32> {
  typedef int32 int_type;
  typedef int64 int_x2_type;
  typedef extended_int<16> big_int_type;
  typedef fpt64 fpt_type;
  typedef type_converter_fpt to_fpt_converter_type;
};

template <typename T>
class intersection_event {
 public:
  typedef T coordinate_type;

  intersection_event() {}

  coordinate_type x() const { return x_; }
  void x(const coordinate_type& val) { x_ = val; }

  coordinate_type y() const { return y_; }
  void y(const coordinate_type& val) { y_ = val; }

  coordinate_type r() const { return r_; }
  void r(const coordinate_type& val) { r_ = val; }

 private:
  coordinate_type x_;
  coordinate_type y_;
  coordinate_type r_;
};

template <typename CTYPE_TRAITS>
class skeleton_predicates {
 public:
  typedef typename CTYPE_TRAITS::int_type int_type;
  typedef typename CTYPE_TRAITS::int_x2_type int_x2_type;
  typedef typename CTYPE_TRAITS::big_int_type big_int_type;
  typedef typename CTYPE_TRAITS::fpt_type fpt_type;
  typedef typename CTYPE_TRAITS::to_fpt_converter_type to_fpt_converter;

  template <typename Segment>
  class event_existence_predicate {
   public:
    typedef Segment segment_type;

    bool operator()(const segment_type& segment1,
                    const segment_type& segment2,
                    const segment_type& segment3) {
      // TODO(asydorchuk): For now don't do any checks and just return true.
      // Should be enough to solve convex polygon case.
      return true;
    }
  };

  template <typename Segment, typename Event>
  class mp_event_formation_functor {
   public:
    typedef Segment segment_type;
    typedef Event event_type;
    typedef robust_sqrt_expr<big_int_type, fpt_type, to_fpt_converter>
        robust_sqrt_expr_type;

    void operator()(
        const segment_type& segment1,
        const segment_type& segment2,
        const segment_type& segment3,
        event_type* event,
        bool recompute_x = true,
        bool recompute_y = true,
        bool recompute_r = true) {
      a[0] = static_cast<int_x2_type>(segment1.high().x()) -
             static_cast<int_x2_type>(segment1.low().x());
      a[1] = static_cast<int_x2_type>(segment2.high().x()) -
             static_cast<int_x2_type>(segment2.low().x());
      a[2] = static_cast<int_x2_type>(segment3.high().x()) -
             static_cast<int_x2_type>(segment3.low().x());

      b[0] = static_cast<int_x2_type>(segment1.high().y()) -
             static_cast<int_x2_type>(segment1.low().y());
      b[1] = static_cast<int_x2_type>(segment2.high().y()) -
             static_cast<int_x2_type>(segment2.low().y());
      b[2] = static_cast<int_x2_type>(segment3.high().y()) -
             static_cast<int_x2_type>(segment3.low().y());

      c[0] = static_cast<int_x2_type>(segment1.low().x()) *
             static_cast<int_x2_type>(segment1.high().y()) -
             static_cast<int_x2_type>(segment1.low().y()) *
             static_cast<int_x2_type>(segment1.high().x());
      c[1] = static_cast<int_x2_type>(segment2.low().x()) *
             static_cast<int_x2_type>(segment2.high().y()) -
             static_cast<int_x2_type>(segment2.low().y()) *
             static_cast<int_x2_type>(segment2.high().x());
      c[2] = static_cast<int_x2_type>(segment3.low().x()) *
             static_cast<int_x2_type>(segment3.high().y()) -
             static_cast<int_x2_type>(segment3.low().y()) *
             static_cast<int_x2_type>(segment3.high().x());

      for (int i = 0; i < 3; ++i) {
        int j = (i+1) % 3;
        int k = (i+2) % 3;
        cA[i] = a[j] * b[k] - a[k] * b[j];
        cB[i] = a[i] * a[i] + b[i] * b[i];
      }
      fpt_type inv_denom = 1.0 / to_fpt(sqrt_expr_.eval3(cA, cB));

      if (recompute_r) {
        for (int i = 0; i < 3; ++i) {
          cA[i] = cA[i] * c[i];
        }
        fpt_type r = to_fpt(cA[0] + cA[1] + cA[2]);
        event->r(r * inv_denom);
        if (is_neg(event->r())) {
          event->r(-event->r());
	}
      }

      if (recompute_x) {
        for (int i = 0; i < 3; ++i) {
          int j = (i+1) % 3;
          int k = (i+2) % 3;
          cA[i] = a[j] * c[k] - a[k] * c[j];
        }
        fpt_type x = to_fpt(sqrt_expr_.eval3(cA, cB));
        event->x(x * inv_denom);
      }

      if (recompute_y) {
        for (int i = 0; i < 3; ++i) {
          int j = (i+1) % 3;
          int k = (i+2) % 3;
          cA[i] = b[j] * c[k] - b[k] * c[j];
        }
        fpt_type y = to_fpt(sqrt_expr_.eval3(cA, cB));
        event->y(y * inv_denom);
      }
    }

   private:
    big_int_type a[3];
    big_int_type b[3];
    big_int_type c[3];
    big_int_type cA[3];
    big_int_type cB[3];
    to_fpt_converter to_fpt;
    robust_sqrt_expr_type sqrt_expr_;
  };

  template <typename Segment,
            typename Event,
            typename EEP = event_existence_predicate<Segment>,
            typename EFF = mp_event_formation_functor<Segment, Event> >
  class event_formation_predicate {
   public:
    typedef Segment segment_type;
    typedef Event event_type;
    typedef EEP event_existence_predicate_type;
    typedef EFF event_formation_functor_type;

    bool operator()(const segment_type& segment1, const segment_type& segment2,
                    const segment_type& segment3, event_type* event) {
      if (!event_existence_predicate_(segment1, segment2, segment3)) {
        return false;
      }
      event_formation_functor_(segment1, segment2, segment3, event);
      return true;
    }

   private:
    event_existence_predicate_type event_existence_predicate_;
    event_formation_functor_type event_formation_functor_;
  };
};
}  // detail
}  // polygon
}  // boost

#endif  // BOOST_POLYGON_DETAIL_SKELETON_PREDICATES
