// Boost.Geometry (aka GGL, Generic Geometry Library)

// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.
// Copyright (c) 2008-2012 Bruno Lalande, Paris, France.
// Copyright (c) 2009-2012 Mateusz Loskot, London, UK.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//
// Example combining Boost.Geometry with Boost.Units

#include <iostream>

#include <boost/geometry/geometry.hpp>


#include <boost/units/quantity.hpp> 
#include <boost/units/systems/si/length.hpp>
#include <boost/units/systems/cgs/length.hpp>
#include <boost/units/systems/si/io.hpp>


// TEMPORARY this will go to somewhere within Boost.Geometry
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
template <typename U, std::size_t D = 2, typename T = double, typename CS = cs::units_cartesian<U> >
class quantity_point : public model::point<T, D, CS>
{
    typedef boost::units::quantity<U, T> qtype;

public :

    // Templated constructor to allow constructing with other units then qtype,
    // e.g. to convert from centimeters to meters
    template <typename Q>
    inline quantity_point(Q const& x, Q const& y)
        : model::point<T, D, CS>(
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



// For extra support for functions as distance,area,get,set
namespace units
{
    namespace detail
    {
        // Define an extra meta-function to get the units of a coordinate system
        template <typename CS>
        struct unit_dimension 
        {
            // define it as dimensionless
            // or MPL ASSERT
        };

        template <typename U>
        struct unit_dimension<cs::units_cartesian<U> >
        {
            typedef U type;
        };
    }

    // Define an extra metafunction to define the quantity of a Geometry type
    template <typename Geometry, typename CT = typename coordinate_type<Geometry>::type>
    struct quantity
    {
        typedef boost::units::quantity
            <
                typename detail::unit_dimension
                    <
                        typename coordinate_system<Geometry>::type
                    >::type, 
                CT
            > type;
    };


    template <typename Geometry1, typename Geometry2>
    inline typename quantity<Geometry1, typename default_distance_result<Geometry1, Geometry2>::type>::type
        distance(Geometry1 const& g1, Geometry2 const& g2)
    {
        typedef typename quantity<Geometry1, typename default_distance_result<Geometry1, Geometry2>::type>::type q;
        return q::from_value(geometry::distance(g1, g2));
    }

    template <std::size_t Index, typename Point>
    inline typename quantity<Point>::type get(Point const& p)
    {
        typedef typename quantity<Point>::type q;
        return q::from_value(geometry::get<Index>(p));
    }
}

}}
// END TEMPORARY



int main(void)
{
    using namespace boost::geometry;
    using namespace boost::units;

    // 1: using it directly
    {
        typedef model::quantity_point<si::length, 2> point;
        point p1(1 * si::meter, 2 * si::meter);
        point p2(3 * si::meter, 4 * si::meter);

        std::cout << get<0>(p2) << std::endl;

        // This is a little inconvenient:
        quantity<si::length> d = distance(p1, p2) * si::meter;

        std::cout << d << std::endl;
    }

    // 2: same but now using centimeters, and using boost::geometry::units::
    {
        typedef model::quantity_point<cgs::length, 2> point;
        point p1(1 * si::meter, 2 * si::meter);
        point p2(3 * si::meter, 4 * si::meter);

        std::cout << boost::geometry::units::get<0>(p2) << std::endl;
        quantity<cgs::length> d = boost::geometry::units::distance(p1, p2);
        std::cout << d << std::endl;
    }

    return 0;
}
