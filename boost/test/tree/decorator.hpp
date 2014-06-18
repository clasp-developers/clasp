//  (C) Copyright Gennadiy Rozental 2011-2012.
//  Distributed under the Boost Software License, Version 1.0.
//  (See accompanying file LICENSE_1_0.txt or copy at 
//  http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/test for the library home page.
//
//  File        : $RCSfile$
//
//  Version     : $Revision: 62016 $
//
//  Description : defines decorators to be using with auto registered test units
// ***************************************************************************

#ifndef BOOST_TEST_TREE_DECORATOR_HPP_091911GER
#define BOOST_TEST_TREE_DECORATOR_HPP_091911GER

// Boost.Test
#include <boost/test/detail/config.hpp>
#include <boost/test/detail/global_typedef.hpp>

#include <boost/test/tree/fixture.hpp>

#include <boost/test/utils/basic_cstring/basic_cstring.hpp>

// Boost
#include <boost/shared_ptr.hpp>
#include <boost/function/function0.hpp>

#include <boost/test/detail/suppress_warnings.hpp>

//____________________________________________________________________________//

namespace boost {
namespace unit_test {

class test_unit;

namespace decorator {

// ************************************************************************** //
// **************             decorator::collector             ************** //
// ************************************************************************** //

class for_test_unit;
typedef boost::shared_ptr<for_test_unit> for_test_unit_ptr;

class BOOST_TEST_DECL collector {
public:
    explicit                collector( for_test_unit const& );
    static collector*&      instance();

    void                    store_in( test_unit& tu );

private:
    // Data members
    for_test_unit_ptr       m_tu_decorator;
};

// ************************************************************************** //
// **************           decorator::for_test_unit           ************** //
// ************************************************************************** //

class BOOST_TEST_DECL for_test_unit {
public:
    virtual                 ~for_test_unit() {}

    // composition interface 1
    for_test_unit const&    operator+() const { return *this; }
    for_test_unit const&    operator+( for_test_unit const& rhs ) const;

    // composition interface 2
    for_test_unit const&    operator-() const { return *this; }
    for_test_unit const&    operator-( for_test_unit const& rhs ) const { return *this + rhs; }

    // composition interface 3
    for_test_unit const&    operator*() const { return *this; }
    for_test_unit const&    operator*( for_test_unit const& rhs ) const { return *this + rhs; }

    // application interface
    void                    apply( test_unit& tu );
    for_test_unit*          clone() const;

private:
    friend class collector;

    // decorator::for_test_unit interface
    virtual for_test_unit*  do_clone() const = 0;
    virtual void            do_apply( test_unit& ) = 0;

    // Data members
    mutable for_test_unit_ptr m_next;
};

// ************************************************************************** //
// **************               decorator::label               ************** //
// ************************************************************************** //

class BOOST_TEST_DECL label : public decorator::for_test_unit {
public:
    explicit                label( const_string l ) : m_label( l ) {}

private:
    // decorator::for_test_unit interface
    virtual void            do_apply( test_unit& tu );
    virtual for_test_unit*  do_clone() const            { return new label( m_label ); }

    // Data members
    const_string            m_label;
};

// ************************************************************************** //
// **************         decorator::expected_failures         ************** //
// ************************************************************************** //

class BOOST_TEST_DECL expected_failures : public decorator::for_test_unit {
public:
    explicit                expected_failures( counter_t ef ) : m_exp_fail( ef ) {}

private:
    // decorator::for_test_unit interface
    virtual void            do_apply( test_unit& tu );
    virtual for_test_unit*  do_clone() const            { return new expected_failures( m_exp_fail ); }

    // Data members
    counter_t               m_exp_fail;
};

// ************************************************************************** //
// **************              decorator::timeout              ************** //
// ************************************************************************** //

class BOOST_TEST_DECL timeout : public decorator::for_test_unit {
public:
    explicit                timeout( unsigned t ) : m_timeout( t ) {}

private:
    // decorator::for_test_unit interface
    virtual void            do_apply( test_unit& tu );
    virtual for_test_unit*  do_clone() const            { return new timeout( m_timeout ); }

    // Data members
    unsigned                m_timeout;
};

// ************************************************************************** //
// **************            decorator::description            ************** //
// ************************************************************************** //

class BOOST_TEST_DECL description : public decorator::for_test_unit {
public:
    explicit                description( const_string descr ) : m_description( descr ) {}

private:
    // decorator::for_test_unit interface
    virtual void            do_apply( test_unit& tu );
    virtual for_test_unit*  do_clone() const            { return new description( m_description ); }

    // Data members
    const_string            m_description;
};

// ************************************************************************** //
// **************            decorator::depends_on             ************** //
// ************************************************************************** //

class BOOST_TEST_DECL depends_on : public decorator::for_test_unit {
public:
    explicit                depends_on( const_string dependency ) : m_dependency( dependency ) {}

private:
    // decorator::for_test_unit interface
    virtual void            do_apply( test_unit& tu );
    virtual for_test_unit*  do_clone() const            { return new depends_on( m_dependency ); }

    // Data members
    const_string            m_dependency;
};

// ************************************************************************** //
// **************        decorator::enable_if/disable_if       ************** //
// ************************************************************************** //

class BOOST_TEST_DECL enable_if : public decorator::for_test_unit {
public:
    explicit                enable_if( bool condition ) : m_condition( condition ) {}

private:
    // decorator::for_test_unit interface
    virtual void            do_apply( test_unit& tu );
    virtual for_test_unit*  do_clone() const            { return new enable_if( m_condition ); }

    // Data members
    bool                    m_condition;
};

class BOOST_TEST_DECL disable_if : public enable_if {
public:
    explicit    disable_if( bool condition ) : enable_if( !condition ) {}
};

// ************************************************************************** //
// **************              decorator::fixture              ************** //
// ************************************************************************** //

class BOOST_TEST_DECL fixture_t : public decorator::for_test_unit {
public:
    // Constructor
    explicit    fixture_t( test_unit_fixture_ptr impl ) : m_impl( impl ) {}

private:
    // decorator::for_test_unit interface
    virtual void            do_apply( test_unit& tu );
    virtual for_test_unit*  do_clone() const            { return new fixture_t( m_impl ); }

    // Data members
    test_unit_fixture_ptr   m_impl;
};

//____________________________________________________________________________//

template<typename F>
inline fixture_t
fixture()
{
    return fixture_t( test_unit_fixture_ptr( new unit_test::class_based_fixture<F>() ) );
}

//____________________________________________________________________________//

template<typename F, typename Arg>
inline fixture_t
fixture( Arg const& arg )
{
    return fixture_t( test_unit_fixture_ptr( new unit_test::class_based_fixture<F,Arg>( arg ) ) );
}

//____________________________________________________________________________//

inline fixture_t
fixture( boost::function<void()> const& setup, boost::function<void()> const& teardown = boost::function<void()>() )
{
    return fixture_t( test_unit_fixture_ptr( new unit_test::function_based_fixture( setup, teardown ) ) );
}

//____________________________________________________________________________//

} // namespace decorator

using decorator::label;
using decorator::expected_failures;
using decorator::timeout;
using decorator::description;
using decorator::depends_on;
using decorator::enable_if;
using decorator::disable_if;
using decorator::fixture;

} // namespace unit_test
} // namespace boost

#include <boost/test/detail/enable_warnings.hpp>

#endif // BOOST_TEST_TREE_DECORATOR_HPP_091911GER

