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
#define BOOST_TEST_DATA_MONOMORPHIC_GENERATORS_RANDOM_HPP_101512GER

// Boost.Test
#include <boost/test/data/config.hpp>

#ifndef BOOST_NO_0X_HDR_RANDOM

#include <boost/test/data/monomorphic/generate.hpp>
#include <boost/test/data/monomorphic/generators/keywords.hpp>

// STL
#include <random>

#include <boost/test/detail/suppress_warnings.hpp>

//____________________________________________________________________________//

namespace boost {
namespace unit_test {
namespace data {

namespace {
nfp::keyword<struct seed_t>         seed;
nfp::keyword<struct distrbution_t>  distribution;
nfp::keyword<struct engine_t>       engine;
} // local namespace

namespace monomorphic {

namespace ds_detail {
template<typename SampleType>
struct default_distribution {
    typedef typename mpl::if_<std::is_integral<SampleType>,
                              std::uniform_int_distribution<SampleType>,
                              std::uniform_real_distribution<SampleType> >::type type;
};

} // namespace ds_detail

// ************************************************************************** //
// **************                   random_t                   ************** //
// ************************************************************************** //

template<typename SampleType        = double, 
         typename DistributionType  = typename ds_detail::default_distribution<SampleType>::type,
         typename EngineType        = std::default_random_engine>
class random_t {
public:
    typedef SampleType          data_type;
    typedef DistributionType    distr_type;
    typedef EngineType          engine_type;

    random_t()
    : m_distribution()
    , m_engine( std::random_device()() )
    {}
    explicit random_t( distr_type&& d )
    : m_distribution( std::forward<distr_type>(d) )
    , m_engine( std::random_device()() ){}
    random_t( engine_type&& e, distr_type&& d )
    : m_distribution( std::forward<distr_type>(d) )
    , m_engine( std::forward<engine_type>(e) ){}

    // Generator interface
    data::size_t        capacity() const    { return BOOST_TEST_DS_INFINITE_SIZE; }
    SampleType          next() 
    {
        return m_distribution( m_engine );
    }
    template<typename SeedType>
    void seed( SeedType&& seed )            { m_engine.seed( std::forward<SeedType>( seed ) ); }

private:
    // Data members
    DistributionType    m_distribution;
    EngineType          m_engine;
};

//____________________________________________________________________________//

} // namespace monomorphic

inline monomorphic::generated_by<monomorphic::random_t<>>
random()
{
    return monomorphic::generated_by<monomorphic::random_t<>>( monomorphic::random_t<>() );
}

//____________________________________________________________________________//

template<typename SampleType>
inline monomorphic::generated_by<monomorphic::random_t<SampleType>>
random( SampleType begin, SampleType end )
{
    typedef monomorphic::random_t<SampleType> Gen;
    typedef typename Gen::distr_type distr_type;
    return monomorphic::generated_by<Gen>( Gen( distr_type(begin,end) ) );
}

//____________________________________________________________________________//

namespace ds_detail {
template<typename Params>
struct random_gen_type {
    typedef typename nfp::param_type<Params,decltype(distribution),std::uniform_real_distribution<>>::type distr_type;
    typedef typename nfp::param_type<Params,decltype(engine),std::default_random_engine>::type engine_type;
    typedef typename distr_type::result_type sample_type;

    typedef monomorphic::random_t<sample_type,distr_type,engine_type> type;
};

}

template<typename Params>
inline monomorphic::generated_by<typename ds_detail::random_gen_type<Params>::type>
random( Params const& params )
{
    typedef typename ds_detail::random_gen_type<Params>::type Gen;
    typedef typename Gen::distr_type distr_type;
    typedef typename Gen::engine_type engine_type;

    std::random_device rd;
    engine_type E;
//    engine_type E( rd );
    if( params.has(engine) )
        E = params[engine];

    distr_type D;
    if( params.has(distribution) )
        D = params[distribution];

    Gen G( std::move(E), std::move(D) );

    if( params.has(seed) )
        G.seed( params[seed] );

    return monomorphic::generated_by<Gen>( std::move(G) );
}

} // namespace data
} // namespace unit_test
} // namespace boost

#include <boost/test/detail/enable_warnings.hpp>

#endif // BOOST_NO_0X_HDR_RANDOM


#endif // BOOST_TEST_DATA_MONOMORPHIC_GENERATORS_RANDOM_HPP_101512GER
