// Boost.Geometry (aka GGL, Generic Geometry Library)

// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.
// Copyright (c) 2008-2012 Bruno Lalande, Paris, France.
// Copyright (c) 2009-2012 Mateusz Loskot, London, UK.

// Parts of Boost.Geometry are redesigned from Geodan's Geographic Library
// (geolib/GGL), copyright (c) 1995-2010 Geodan, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_GEOMETRY_EXTENSIONS_GEOMETRIES_QUANTITY_POINT_HPP
#define BOOST_GEOMETRY_EXTENSIONS_GEOMETRIES_QUANTITY_POINT_HPP

#include <cstddef>

#include <boost/mpl/int.hpp>

#include <boost/geometry/core/cs.hpp>
#include <boost/geometry/geometries/point.hpp>

#include <boost/units/quantity.hpp>


namespace boost { namespace geometry
{

namespace cs
{

template <typename Unit>
struct units_cartesian {};

}

namespace traits
{

template<typename U>
struct cs_tag<cs::units_cartesian<U> >
{
    typedef cartesian_tag type;
};

}


namespace model
{

// Define a point type to interoperate with Boost.Units, having
// 1. a constructor taking quantities
// 2. defining a quantified coordinate system
// Note that all values are still stored in "normal" types as double
template
<
    typename Units,
    std::size_t DimensionCount = 2,
    typename CoordinateType = double,
    typename CoordinateSystem = cs::units_cartesian<Units>
>
class quantity_point
    : public model::point<CoordinateType, DimensionCount, CoordinateSystem>
{
    typedef boost::units::quantity<Units, CoordinateType> qtype;

public :

    // Templated constructor to allow constructing with other units then qtype,
    // e.g. to convert from centimeters to meters
    template <typename Quantity>
    inline quantity_point(Quantity const& x, Quantity const& y)
        : model::point<CoordinateType, DimensionCount, CoordinateSystem>(
            qtype(x).value(),
            qtype(y).value())
    {}
};

}


// Adapt quantity_point to the Point Concept
#ifndef DOXYGEN_NO_TRAITS_SPECIALIZATIONS
namespace traits
{

template <typename Units, std::size_t DimensionCount, typename CoordinateType, typename CoordinateSystem>
struct tag<model::quantity_point<Units, DimensionCount, CoordinateType, CoordinateSystem> >
{
    typedef point_tag type;
};

template<typename Units, std::size_t DimensionCount, typename CoordinateType, typename CoordinateSystem>
struct coordinate_type<model::quantity_point<Units, DimensionCount, CoordinateType, CoordinateSystem> >
{
    typedef CoordinateType type;
};

template<typename Units, std::size_t DimensionCount, typename CoordinateType, typename CoordinateSystem>
struct coordinate_system<model::quantity_point<Units, DimensionCount, CoordinateType, CoordinateSystem> >
{
    typedef CoordinateSystem type;
};

template<typename Units, std::size_t DimensionCount, typename CoordinateType, typename CoordinateSystem>
struct dimension<model::quantity_point<Units, DimensionCount, CoordinateType, CoordinateSystem> >
    : boost::mpl::int_<DimensionCount>
{};

template<typename Units, std::size_t DimensionCount, typename CoordinateType, typename CoordinateSystem, std::size_t Dimension>
struct access<model::quantity_point<Units, DimensionCount, CoordinateType, CoordinateSystem>, Dimension >
{
    static inline CoordinateType get(
        model::quantity_point<Units, DimensionCount, CoordinateType, CoordinateSystem> const& p)
    {
        return p.template get<Dimension>();
    }

    static inline void set(model::quantity_point<Units, DimensionCount, CoordinateType, CoordinateSystem>& p,
        CoordinateType const& value)
    {
        p.template set<Dimension>(value);
    }
};

} // namespace traits
#endif // DOXYGEN_NO_TRAITS_SPECIALIZATIONS


}} // namespace boost::geometry

#endif // BOOST_GEOMETRY_EXTENSIONS_GEOMETRIES_QUANTITY_POINT_HPP
