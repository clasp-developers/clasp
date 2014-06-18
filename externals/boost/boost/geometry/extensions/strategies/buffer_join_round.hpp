// Boost.Geometry (aka GGL, Generic Geometry Library)

// Copyright (c) 2007-2013 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_GEOMETRY_EXTENSIONS_STRATEGIES_BUFFER_JOIN_ROUND_HPP
#define BOOST_GEOMETRY_EXTENSIONS_STRATEGIES_BUFFER_JOIN_ROUND_HPP


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
struct join_round
{
    inline join_round(int steps_per_circle = 100)
        : m_steps_per_circle(steps_per_circle)
    {}

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
    inline void generate_points(PointIn const& vertex,
                PointIn const& perp1, PointIn const& perp2,
                promoted_type const& buffer_distance,
                RangeOut& range_out) const
    {
        promoted_type dx1 = get<0>(perp1) - get<0>(vertex);
        promoted_type dy1 = get<1>(perp1) - get<1>(vertex);
        promoted_type dx2 = get<0>(perp2) - get<0>(vertex);
        promoted_type dy2 = get<1>(perp2) - get<1>(vertex);

        dx1 /= buffer_distance;
        dy1 /= buffer_distance;
        dx2 /= buffer_distance;
        dy2 /= buffer_distance;

        promoted_type angle_diff = acos(dx1 * dx2 + dy1 * dy2);

        promoted_type two = 2.0;
        promoted_type steps = m_steps_per_circle;
        int n = boost::numeric_cast<int>(steps * angle_diff 
                    / (two * geometry::math::pi<promoted_type>()));

        if (n > 1000)
        {
            // TODO change this / verify this
            std::cout << dx1 << ", " << dy1 << " .. " << dx2 << ", " << dy2 << std::endl;
            std::cout << angle_diff << " -> " << n << std::endl;
            n = 1000;
        }
        else if (n <= 1)
        {
            return;
        }

        promoted_type const angle1 = atan2(dy1, dx1);
        promoted_type diff = angle_diff / promoted_type(n);
        promoted_type a = angle1 - diff;

        for (int i = 0; i < n - 1; i++, a -= diff)
        {
            PointIn p;
            set<0>(p, get<0>(vertex) + buffer_distance * cos(a));
            set<1>(p, get<1>(vertex) + buffer_distance * sin(a));
            range_out.push_back(p);
        }
    }

    template <typename RangeOut>
    inline void apply(PointIn const& ip, PointIn const& vertex,
                PointIn const& perp1, PointIn const& perp2,
                coordinate_type const& buffer_distance,
                RangeOut& range_out) const
    {
        if (geometry::equals(perp1, perp2))
        {
            //std::cout << "Corner for equal points " << geometry::wkt(ip) << " " << geometry::wkt(perp1) << std::endl;
            return;
        }

        coordinate_type zero = 0;
        int signum = buffer_distance > zero ? 1
                   : buffer_distance < zero ? -1
                   : 0;

        if (side::apply(perp1, ip, perp2) == signum)
        {
        }
        else
        {
            // Generate 'vectors'
            coordinate_type vix = (get<0>(ip) - get<0>(vertex));
            coordinate_type viy = (get<1>(ip) - get<1>(vertex));

            coordinate_type length_i = sqrt(vix * vix + viy * viy);

            coordinate_type const bd = geometry::math::abs(buffer_distance);
            coordinate_type prop = bd / length_i;

            PointIn bp;
            set<0>(bp, get<0>(vertex) + vix * prop);
            set<1>(bp, get<1>(vertex) + viy * prop);

            range_out.push_back(perp1);
            generate_points(vertex, perp1, perp2, bd, range_out);
            range_out.push_back(perp2);
        }
    }
};




}} // namespace strategy::buffer


}} // namespace boost::geometry

#endif // BOOST_GEOMETRY_EXTENSIONS_STRATEGIES_BUFFER_JOIN_ROUND_HPP
