//  (C) Copyright Gennadiy Rozental 2011-2012.
//  Distributed under the Boost Software License, Version 1.0.
//  (See accompanying file LICENSE_1_0.txt or copy at 
//  http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/test for the library home page.
//
//  File        : $RCSfile$
//
//  Version     : $Revision$
//
//  Description : defines range generator
// ***************************************************************************

#ifndef BOOST_TEST_DATA_MONOMORPHIC_GENERATORS_XRANGE_HPP_112011GER
#define BOOST_TEST_DATA_MONOMORPHIC_GENERATORS_XRANGE_HPP_112011GER

// Boost.Test
#include <boost/test/data/config.hpp>

#include <boost/test/data/monomorphic/generators/keywords.hpp>
#include <boost/test/data/monomorphic/generate.hpp>

// Boost
#include <boost/optional.hpp>
#include <boost/utility/enable_if.hpp>
#include <boost/type_traits/is_unsigned.hpp>

// STL
#include <limits>
#include <cmath>

#include <boost/test/detail/suppress_warnings.hpp>

//____________________________________________________________________________//

namespace boost {
namespace unit_test {
namespace data {
namespace monomorphic {

// ************************************************************************** //
// **************             monomorphic::xrange_t            ************** //
// ************************************************************************** //

template<typename SampleType, typename StepType=SampleType>
class xrange_t {
public:
    typedef SampleType data_type;

    xrange_t( SampleType const& begin, StepType const& step, data::size_t size )
    : m_curr( begin )
    , m_step( step )
    , m_size( size )
    {}

    // Generator interface
    data::size_t    capacity() const { return m_size; }
    SampleType      next() 
    {
        BOOST_TEST_DS_ASSERT( m_size != 0, "No more elements in range" );

        SampleType res = m_curr;

        m_curr += m_step;
        --m_size;

        return res;
    }
private:
    // Data members
    SampleType      m_curr;
    StepType        m_step;
    data::size_t    m_size;
};

//____________________________________________________________________________//

namespace ds_detail {

template<typename SampleType, typename StepType=SampleType>
struct make_xrange {
    static StepType    abs( StepType s, mpl::true_* )   { return s; }
    static StepType    abs( StepType s, mpl::false_* )  { return std::abs(s); }

    typedef xrange_t<SampleType, StepType> range_gen;

    template<typename Params>
    static generated_by<range_gen>
    _( Params const& params )
    {
        SampleType           begin_val  = params.has( data::begin )  ? params[data::begin] : SampleType();
        optional<SampleType> end_val    = params.has( data::end )    ? params[data::end]   : optional<SampleType>();
        StepType             step_val   = params.has( data::step )   ? params[data::step]  : 1;

        BOOST_TEST_DS_ASSERT( step_val != 0, "Range step can't be zero" );

        data::size_t size;
        if( !end_val.is_initialized() )
            size = BOOST_TEST_DS_INFINITE_SIZE;
        else {
            BOOST_TEST_DS_ASSERT( (step_val < 0) ^ (begin_val < *end_val), "Invalid step direction" );

            SampleType  abs_distance    = step_val < 0 ? begin_val - *end_val : *end_val-begin_val;
            StepType    abs_step        = make_xrange::abs(step_val, (typename boost::is_unsigned<StepType>::type*)0 );
            std::size_t s = static_cast<std::size_t>(abs_distance/abs_step);

            if( static_cast<SampleType>(s*abs_step) < abs_distance )
                s++;

            size = s;
        }

        return generated_by<range_gen>( range_gen( begin_val, step_val, size ) );
    }
};

} // namespace ds_detail
} // namespace monomorphic

//____________________________________________________________________________//

template<typename SampleType, typename Params>
inline monomorphic::generated_by<monomorphic::xrange_t<SampleType> >
xrange( Params const& params )
{
    return monomorphic::ds_detail::make_xrange<SampleType>::_( params );
}

//____________________________________________________________________________//

template<typename SampleType>
inline monomorphic::generated_by<monomorphic::xrange_t<SampleType> >
xrange( SampleType const& end_val )
{
    return monomorphic::ds_detail::make_xrange<SampleType>::_( data::end=end_val );
}

//____________________________________________________________________________//

template<typename SampleType, typename Params>
inline typename enable_if_c<nfp::is_named_params<Params>::value,monomorphic::generated_by<monomorphic::xrange_t<SampleType> > >::type
xrange( SampleType const& end_val, Params const& params )
{
    return monomorphic::ds_detail::make_xrange<SampleType>::
            _(( params, data::end=end_val ));
}

//____________________________________________________________________________//

template<typename SampleType>
inline monomorphic::generated_by<monomorphic::xrange_t<SampleType> >
xrange( SampleType const& begin_val, SampleType const& end_val )
{
    return monomorphic::ds_detail::make_xrange<SampleType>::
            _(( data::begin=begin_val, data::end=end_val ));
}

//____________________________________________________________________________//

template<typename SampleType,typename StepType>
inline monomorphic::generated_by<monomorphic::xrange_t<SampleType> >
xrange( SampleType const& begin_val, SampleType const& end_val, StepType const& step_val )
{
    return monomorphic::ds_detail::make_xrange<SampleType,StepType>::
            _(( data::begin=begin_val, data::end=end_val, data::step=step_val ));
}

//____________________________________________________________________________//

} // namespace data
} // namespace unit_test
} // namespace boost

#include <boost/test/detail/enable_warnings.hpp>

#endif // BOOST_TEST_DATA_MONOMORPHIC_GENERATORS_XRANGE_HPP_112011GER
