// Boost.Geometry (aka GGL, Generic Geometry Library)

// Copyright (c) 2007-2013 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_GEOMETRY_EXTENSIONS_STRATEGIES_BUFFER_END_ROUND_HPP
#define BOOST_GEOMETRY_EXTENSIONS_STRATEGIES_BUFFER_END_ROUND_HPP


// Buffer strategies

#include <boost/geometry/core/cs.hpp>
#include <boost/geometry/strategies/tags.hpp>
#include <boost/geometry/strategies/side.hpp>
#include <boost/geometry/util/math.hpp>
#include <boost/geometry/util/select_most_precise.hpp>

#include <boost/geometry/extensions/strategies/buffer_side.hpp>



namespace boost { namespace geometry
{


namespace strategy { namespace buffer
{


template
<
    typename PointIn,
    typename PointOut
>
class end_round
{
    typedef typename strategy::side::services::default_strategy<typename cs_tag<PointIn>::type>::type side;
    typedef typename coordinate_type<PointOut>::type coordinate_type;

    typedef typename geometry::select_most_precise
        <
            typename geometry::select_most_precise
                <
                    typename geometry::coordinate_type<PointIn>::type,
                    typename geometry::coordinate_type<PointOut>::type
                >::type,
            double
        >::type promoted_type;

    int m_steps_per_circle;

    template <typename RangeOut>
    inline void generate_points(PointIn const& point,
                promoted_type alpha,
                promoted_type const& buffer_distance,
                RangeOut& range_out) const
    {
        promoted_type const two = 2.0;
        promoted_type const two_pi = two * geometry::math::pi<promoted_type>();

        int point_buffer_count = m_steps_per_circle;

        promoted_type const diff = two_pi / promoted_type(point_buffer_count);

        // For half circle: 
        point_buffer_count /= 2;
        point_buffer_count++;

        for (int i = 0; i < point_buffer_count; i++, alpha -= diff)
        {
            typename boost::range_value<RangeOut>::type p;
            set<0>(p, get<0>(point) + buffer_distance * cos(alpha));
            set<1>(p, get<1>(point) + buffer_distance * sin(alpha));
            range_out.push_back(p);
        }
    }

    // COPIED FROM OCCUPATION_INFO
    template <typename T, typename P1, typename P2>
    static inline T calculate_angle(P1 const& from_point, P2 const& to_point)
    {
        typedef P1 vector_type;
        vector_type v = from_point;
        geometry::subtract_point(v, to_point);
        return atan2(geometry::get<1>(v), geometry::get<0>(v));
    }

public :
    inline end_round(int steps_per_circle = 100)
        : m_steps_per_circle(steps_per_circle)
    {}

    template <typename RangeOut, typename DistanceStrategy>
    inline void apply(PointIn const& penultimate_point, 
                PointIn const& perp_left_point,
                PointIn const& ultimate_point,
                PointIn const& perp_right_point,
                buffer_side_selector side,
                DistanceStrategy const& distance,
                RangeOut& range_out) const
    {
        promoted_type alpha = calculate_angle<promoted_type>(perp_left_point, ultimate_point);

        promoted_type const dist_left = distance.apply(penultimate_point, ultimate_point, buffer_side_left);
        promoted_type const dist_right = distance.apply(penultimate_point, ultimate_point, buffer_side_right);
        if (geometry::math::equals(dist_left, dist_right))
        {
            generate_points(ultimate_point, alpha, dist_left, range_out);
        }
        else
        {
            promoted_type const two = 2.0;
            promoted_type dist_half_diff = (dist_left - dist_right) / two;

            if (side == buffer_side_right)
            {
                dist_half_diff = -dist_half_diff;
            }

            PointIn shifted_point;
            set<0>(shifted_point, get<0>(ultimate_point) + dist_half_diff * cos(alpha));
            set<1>(shifted_point, get<1>(ultimate_point) + dist_half_diff * sin(alpha));
            generate_points(shifted_point, alpha, (dist_left + dist_right) / two, range_out);
        }
    }

    static inline piece_type get_piece_type()
    {
        return buffered_round_end;
    }
};


}} // namespace strategy::buffer

}} // namespace boost::geometry

#endif // BOOST_GEOMETRY_EXTENSIONS_STRATEGIES_BUFFER_END_ROUND_HPP
