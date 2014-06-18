// Boost.Geometry (aka GGL, Generic Geometry Library)
//
// Copyright (c) 2010-2012 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//
// Experiment using Proto to construct geometries

#include <vector>
#include <string>
#include <iostream>



#include <boost/proto/core.hpp>
#include <boost/proto/transform.hpp>
#include <boost/type_traits/add_reference.hpp>

#include <boost/mpl/assert.hpp>


#include <boost/geometry/geometry.hpp>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/io/wkt/wkt.hpp>

namespace bg = boost::geometry;
namespace proto = boost::proto;
using proto::_;


struct proto_geometry_tag {};


// Add a coordinate to a ring or linestring
struct append_point : proto::callable
{
    template<typename Sig>
    struct result;

    template<typename This, typename Geometry, typename T1, typename T2>
    struct result<This(Geometry, T1, T2)>
        : boost::add_reference<Geometry>
    {};

    template<typename Geometry, typename T1, typename T2>
    Geometry& operator()(Geometry& geometry, T1 const& x, T2 const& y) const
    {
        typedef typename boost::geometry::point_type<Geometry>::type point;
        point p;
        boost::geometry::set<0>(p, x);
        boost::geometry::set<1>(p, y);
        geometry.push_back(p);
        return geometry;
    }
};

// The grammar for valid geometry expressions,
// and a transform that populates the geometry
struct geometry_grammar
  : proto::or_
    <
        proto::when
            <
                proto::function
                <
                    proto::terminal<proto_geometry_tag>
                  , proto::terminal<proto::convertible_to< double > >
                  , proto::terminal<proto::convertible_to< double > >
                >
              , append_point
                (
                    proto::_data
                  , proto::_value(proto::_child1)
                  , proto::_value(proto::_child2)
                )
            >
      , proto::when
            <
                proto::function
                <
                    geometry_grammar
                    , proto::terminal<proto::convertible_to< double > >
                    , proto::terminal<proto::convertible_to< double > >
                >
                , append_point
                    (
                        geometry_grammar(proto::_child0)
                      , proto::_value(proto::_child1)
                      , proto::_value(proto::_child2)
                    )
            >
    >
{};

template<typename Expr>
struct proto_geometry_expression;

struct proto_geometry_domain
  : proto::domain<proto::pod_generator<proto_geometry_expression>, geometry_grammar>
{};

// An expression wrapper that provides a conversion to a
// ring, that uses the geometry_grammar
template<typename Expr>
struct proto_geometry_expression
{
    BOOST_PROTO_BASIC_EXTENDS(Expr, proto_geometry_expression, proto_geometry_domain)
    BOOST_PROTO_EXTENDS_FUNCTION()

    template <typename T>
    operator T () const
    {
        BOOST_MPL_ASSERT((proto::matches<Expr, geometry_grammar>));
        T geometry;
        return geometry_grammar()(*this, 0, geometry);
    }

    // NOT finished, need more knowledge about Proto
    template <typename T>
    inline T inner() const
    {
        BOOST_MPL_ASSERT((proto::matches<Expr, geometry_grammar>));
        T geometry;
        return geometry_grammar()(*this, 0, geometry);
    }
    inline void inner()
    {
    }
};



int main()
{
    proto_geometry_expression<proto::terminal<proto_geometry_tag>::type> const
            geometry_of = {{{}}};

    // Initialize a ring:
    typedef bg::model::ring<bg::model::d2::point_xy<double> > ring_type;
    ring_type ring = geometry_of(16, 1)(15,2)(14, 3)(13,4)(12, 3.14)(1,6);
    std::cout << bg::wkt(ring) << std::endl;

    // Initialize a line
    typedef bg::model::linestring<bg::model::d2::point_xy<double> > line_type;
    line_type line = geometry_of(1, 1)(2, 2)(3, 3);
    std::cout << bg::wkt(line) << std::endl;

    // For Polygon (not finished) it should be something like:
    typedef bg::model::polygon<bg::model::d2::point_xy<double> > polygon_type;
    //polygon_type polygon = geometry_of(0, 0)(0, 10)(10, 10)(10, 0)(0, 0).inner(2, 2)(2, 5)(5, 5)(5, 2)(2, 2);
    //std::cout << bg::wkt(polygon) << std::endl;

    return 0;
}
