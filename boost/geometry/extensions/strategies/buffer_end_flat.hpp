// Boost.Geometry (aka GGL, Generic Geometry Library)

// Copyright (c) 2007-2013 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_GEOMETRY_EXTENSIONS_STRATEGIES_BUFFER_END_FLAT_HPP
#define BOOST_GEOMETRY_EXTENSIONS_STRATEGIES_BUFFER_END_FLAT_HPP


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
class end_flat
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


public :

    template <typename RangeOut, typename DistanceStrategy>
    inline void apply(PointIn const& penultimate_point, 
                PointIn const& perp_left_point,
                PointIn const& ultimate_point,
                PointIn const& perp_right_point,
                buffer_side_selector side,
                DistanceStrategy const& distance,
                RangeOut& range_out) const
    {
        promoted_type const dist_left = distance.apply(penultimate_point, ultimate_point, buffer_side_left);
        promoted_type const dist_right = distance.apply(penultimate_point, ultimate_point, buffer_side_right);

        bool reversed = (side == buffer_side_left && dist_right < 0 && -dist_right > dist_left)
                    || (side == buffer_side_right && dist_left < 0 && -dist_left > dist_right)
                    ;
        if (reversed)
        {
            range_out.push_back(perp_right_point);
            range_out.push_back(perp_left_point);
        }
        else
        {
            range_out.push_back(perp_left_point);
            range_out.push_back(perp_right_point);
        }
        // Don't add the ultimate_point (endpoint of the linestring).
        // The buffer might be generated completely at one side.
        // In other cases it does no harm but is further useless
    }

    static inline piece_type get_piece_type()
    {
        return buffered_flat_end;
    }
};


}} // namespace strategy::buffer

}} // namespace boost::geometry

#endif // BOOST_GEOMETRY_EXTENSIONS_STRATEGIES_BUFFER_END_FLAT_HPP
