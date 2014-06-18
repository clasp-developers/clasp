// Boost.Geometry (aka GGL, Generic Geometry Library)

// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.
// Copyright (c) 2008-2012 Bruno Lalande, Paris, France.
// Copyright (c) 2009-2012 Mateusz Loskot, London, UK.

// Parts of Boost.Geometry are redesigned from Geodan's Geographic Library
// (geolib/GGL), copyright (c) 1995-2010 Geodan, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_GEOMETRY_EXTENSIONS_GIS_GEOGRAPHIC_DETAIL_ELLIPSOID_HPP
#define BOOST_GEOMETRY_EXTENSIONS_GIS_GEOGRAPHIC_DETAIL_ELLIPSOID_HPP


namespace boost { namespace geometry { namespace detail
{


/*!
    \brief Defines ellipsoid values for use in distance calculations
    \details They have a constructor with the earth radius
    \note Will be moved / merged with projections
    \todo Optionally specify earth model, defaulting to WGS84
    - See http://en.wikipedia.org/wiki/Figure_of_the_Earth
    - and http://en.wikipedia.org/wiki/World_Geodetic_System#A_new_World_Geodetic_System:_WGS84
    \note
*/
template <typename T>
class ellipsoid
{
    public :
        ellipsoid(T const& a, T const& b)
            : m_a(a)
            , m_b(b)
            , m_f((a - b) / a)
        {}
        ellipsoid()
            : m_a(T(6378137.0))
            , m_b(T(6356752.314245))
            , m_f((m_a - m_b) / m_a)
        {}
        // Unit sphere
        ellipsoid(T const& f)
            : m_a(1.0)
            , m_f(f)
        {}

        T a() const { return m_a; }
        T b() const { return m_b; }
        T f() const { return m_f; }

    private :
        T m_a, m_b, m_f; // equatorial radius, polar radius, flattening
};




}}} // namespace boost::geometry::detail


#endif // BOOST_GEOMETRY_EXTENSIONS_GIS_GEOGRAPHIC_DETAIL_ELLIPSOID_HPP
