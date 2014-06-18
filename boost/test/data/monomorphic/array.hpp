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
//  Description : defines monomorphic dataset based on stl sequence
// ***************************************************************************

#ifndef BOOST_TEST_DATA_MONOMORPHIC_ARRAY_HPP_121411GER
#define BOOST_TEST_DATA_MONOMORPHIC_ARRAY_HPP_121411GER

// Boost.Test
#include <boost/test/data/config.hpp>
#include <boost/test/data/monomorphic/dataset.hpp>

#include <boost/test/detail/suppress_warnings.hpp>

//____________________________________________________________________________//

namespace boost {
namespace unit_test {
namespace data {
namespace monomorphic {

// ************************************************************************** //
// **************                     array                    ************** //
// ************************************************************************** //

template<typename T>
class array : public monomorphic::dataset<T> {
    typedef monomorphic::dataset<T> base;
    typedef typename base::iter_ptr iter_ptr;

    struct iterator : public base::iterator {
        // Constructor
        explicit    iterator( T const* begin, data::size_t size )
        : m_it( begin )
        , m_singleton( size == 1 )
        {}

        // forward iterator interface 
        virtual T const&    operator*()     { return *m_it; }
        virtual void        operator++()    { if( !m_singleton ) ++m_it; }

    private:
        // Data members
        T const*            m_it;
        bool                m_singleton;
    };

public:
    enum { arity = 1 };

    // Constructor
    array( T const* arr, std::size_t size )
    : m_arr( arr )
    , m_size( size )
    {}

    // dataset interface
    virtual data::size_t    size() const            { return m_size; } 
    virtual iter_ptr        begin() const           { return boost::make_shared<iterator>( m_arr, m_size ); }

private:
    // Data members
    T const*        m_arr;
    std::size_t     m_size;
};

//____________________________________________________________________________//

template<typename T>
struct is_dataset<array<T> > : mpl::true_ {};

} // namespace monomorphic

template<typename T, std::size_t size>
inline monomorphic::array<T>
make( T (&a)[size] )
{
    return monomorphic::array<T>( a, size );
}

//____________________________________________________________________________//

} // namespace data
} // namespace unit_test
} // namespace boost

#include <boost/test/detail/enable_warnings.hpp>

#endif // BOOST_TEST_DATA_MONOMORPHIC_ARRAY_HPP_121411GER

