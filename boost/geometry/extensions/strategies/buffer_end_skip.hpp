// Boost.Geometry (aka GGL, Generic Geometry Library)

// Copyright (c) 2007-2013 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_GEOMETRY_EXTENSIONS_STRATEGIES_BUFFER_END_SKIP_HPP
#define BOOST_GEOMETRY_EXTENSIONS_STRATEGIES_BUFFER_END_SKIP_HPP


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
class end_skip
{
public :

    template <typename RangeOut, typename DistanceStrategy>
    inline void apply(PointIn const& , 
                PointIn const& ,
                PointIn const& ,
                PointIn const& ,
                buffer_side_selector ,
                DistanceStrategy const& ,
                RangeOut& ) const
    {
    }

    static inline piece_type get_piece_type()
    {
        return buffered_flat_end;
    }
};


}} // namespace strategy::buffer

}} // namespace boost::geometry

#endif // BOOST_GEOMETRY_EXTENSIONS_STRATEGIES_BUFFER_END_SKIP_HPP
