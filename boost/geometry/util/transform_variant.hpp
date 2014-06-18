// Boost.Geometry (aka GGL, Generic Geometry Library)

// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.
// Copyright (c) 2008-2012 Bruno Lalande, Paris, France.
// Copyright (c) 2009-2012 Mateusz Loskot, London, UK.

// Parts of Boost.Geometry are redesigned from Geodan's Geographic Library
// (geolib/GGL), copyright (c) 1995-2010 Geodan, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_GEOMETRY_UTIL_TRANSFORM_VARIANT_HPP
#define BOOST_GEOMETRY_UTIL_TRANSFORM_VARIANT_HPP


#include <boost/mpl/transform.hpp>
#include <boost/variant/variant_fwd.hpp>


namespace boost { namespace geometry
{


/*!
    \brief Meta-function that takes a boost::variant type and an MPL lambda
        expression and returns a variant type over the same types as the
        initial variant type, each trasnformed using the lambda expression.
    \ingroup utility
    \par Example
    \code
        typedef variant<int, float, long> variant_type;
        typedef transform_variant<variant_type, add_pointer<_> > transformed;
        typedef variant<int*, float*, long*> result;
        BOOST_MPL_ASSERT(( equal<result, transformed> ));
    \endcode
*/

template <typename Variant, typename Op>
struct transform_variant:
    make_variant_over<
        typename mpl::transform<
            typename Variant::types,
            Op
        >::type
    >
{};


}} // namespace boost::geometry


#endif // BOOST_GEOMETRY_UTIL_TRANSFORM_VARIANT_HPP
