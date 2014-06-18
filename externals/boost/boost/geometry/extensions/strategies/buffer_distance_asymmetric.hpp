// Boost.Geometry (aka GGL, Generic Geometry Library)

// Copyright (c) 2007-2013 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_GEOMETRY_EXTENSIONS_STRATEGIES_BUFFER_DISTANCE_ASYMMETRIC_HPP
#define BOOST_GEOMETRY_EXTENSIONS_STRATEGIES_BUFFER_DISTANCE_ASYMMETRIC_HPP


// Buffer strategies

#include <boost/geometry/extensions/strategies/buffer_side.hpp>


namespace boost { namespace geometry
{

namespace strategy { namespace buffer
{


template<typename CoordinateType>
class distance_asymmetric
{
public :
    distance_asymmetric(CoordinateType const& left,
                CoordinateType const& right)
        : m_left(left)
        , m_right(right)
    {}

    template <typename Point>
    inline CoordinateType apply(Point const& , Point const& ,
                buffer_side_selector side)  const
    {
        return side == buffer_side_left ? m_left : m_right;
    }
    
    inline int factor() const
    {
        return m_left < 0 ? -1 : 1;
    }

private :
    CoordinateType m_left;
    CoordinateType m_right;
};


}} // namespace strategy::buffer


}} // namespace boost::geometry

#endif // BOOST_GEOMETRY_EXTENSIONS_STRATEGIES_BUFFER_DISTANCE_ASYMMETRIC_HPP
