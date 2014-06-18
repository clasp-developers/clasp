// Boost.Geometry (aka GGL, Generic Geometry Library)

// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_GEOMETRY_ALGORITHM_REMOVE_WITHIN_DISTANCE_HPP
#define BOOST_GEOMETRY_ALGORITHM_REMOVE_WITHIN_DISTANCE_HPP


#include <algorithm>

#include <boost/range.hpp>

#include <boost/geometry/algorithms/distance.hpp>
#include <boost/geometry/algorithms/detail/point_on_border.hpp>

#include <boost/geometry/core/interior_rings.hpp>


namespace boost { namespace geometry
{



#ifndef DOXYGEN_NO_DETAIL
namespace detail { namespace buffer
{


template<typename Geometry, typename GeometryInput, typename T>
struct remove_false_ring_predicate
{
private :
    typedef typename point_type<Geometry>::type point_type;

    GeometryInput const& m_input;
    T m_distance;

public :
    remove_false_ring_predicate(GeometryInput const& input,
            T const& distance)
        : m_input(input)
        , m_distance(distance)
    {
    }

    inline bool operator()(Geometry const& geometry)
    {
        point_type point, point_input;
        if (geometry::point_on_border(point, geometry, false)
            && geometry::point_on_border(point_input, m_input, false))
        {
            if (m_distance > T())
            {
                // If the input is within the output, it is acceptable
                if (geometry::within(point_input, geometry))
                {
                    return false;
                }

                // Check the distance to the input geometry
                // (for a polygon, this is: the distance from OUTSIDE
                // to the border
                T d = geometry::distance(point, m_input);
                if (d < m_distance)
                {
                    return true;
                }
            }
        }
        return false;
    }
};



template<typename Polygon, typename Geometry>
struct polygon_remove_false_rings
{
    template <typename T>
    static inline void apply(Polygon& polygon, Geometry const& input, T const& distance)
    {
        interior_rings(polygon).erase
            (
                std::remove_if
                    (
                        boost::begin(interior_rings(polygon)),
                        boost::end(interior_rings(polygon)),
                        remove_false_ring_predicate
                            <
                                typename ring_type<Polygon>::type,
                                Geometry,
                                T
                            >(input, distance)
                    ),
                boost::end(interior_rings(polygon))
            );

    }
};


template<typename Collection, typename Geometry, typename T>
inline void collection_remove_within_distance(Collection& dissolved,
            Geometry const& input,
            T const& distance)
{
    typedef typename boost::range_value<Collection>::type polygon_type;
    // 1: remove all polygons which are false positive (positive,
    //    but too close to the input geometry
    dissolved.erase
        (
            std::remove_if(boost::begin(dissolved), boost::end(dissolved),
                remove_false_ring_predicate
                    <
                        polygon_type,
                        Geometry,
                        T
                    >(input, distance)),
            boost::end(dissolved)
        );

    // 2: within all polygons, remove false negative interior rings
    for (typename boost::range_iterator<Collection>::type
        it = boost::begin(dissolved);
        it != boost::end(dissolved);
        ++it)
    {
        polygon_remove_false_rings
            <
                typename boost::range_value<Collection>::type,
                Geometry
            >::apply(*it, input, distance);
    }
}


}} // namespace detail::buffer


#endif // DOXYGEN_NO_DETAIL



}} // namespace boost::geometry


#endif // BOOST_GEOMETRY_ALGORITHM_REMOVE_WITHIN_DISTANCE_HPP
