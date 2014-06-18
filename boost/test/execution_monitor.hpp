//  (C) Copyright Gennadiy Rozental 2001-2012.
//  (C) Copyright Beman Dawes 2001.
//  Distributed under the Boost Software License, Version 1.0.
//  (See accompanying file LICENSE_1_0.txt or copy at 
//  http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/test for the library home page.
//
//  File        : $RCSfile$
//
//  Version     : $Revision$
//
//  Description : defines abstract monitor interfaces and implements execution exception
//  The original Boost Test Library included an implementation detail function
//  named catch_exceptions() which caught otherwise uncaught C++ exceptions.
//  It was derived from an existing test framework by Beman Dawes.  The
//  intent was to expand later to catch other detectable but platform dependent
//  error events like Unix signals or Windows structured C exceptions.
//
//  Requests from early adopters of the Boost Test Library included
//  configurable levels of error message detail, elimination of templates,
//  separation of error reporting, and making the catch_exceptions() facilities
//  available as a public interface.  Support for unit testing also stretched
//  the function based design.  Implementation within the header became less
//  attractive due to the need to include many huge system dependent headers,
//  although still preferable in certain cases.
//
//  All those issues have been addressed by introducing the class-based
//  design presented here.
// ***************************************************************************

#ifndef BOOST_TEST_EXECUTION_MONITOR_HPP_071894GER
#define BOOST_TEST_EXECUTION_MONITOR_HPP_071894GER

// Boost.Test
#include <boost/test/detail/global_typedef.hpp>
#include <boost/test/detail/fwd_decl.hpp>
#include <boost/test/utils/class_properties.hpp>

// Boost
#include <boost/shared_ptr.hpp>
#include <boost/scoped_array.hpp>
#include <boost/type.hpp>
#include <boost/cstdlib.hpp>
#include <boost/function/function0.hpp>

#include <boost/test/detail/suppress_warnings.hpp>

#ifdef BOOST_SEH_BASED_SIGNAL_HANDLING

// for the FP constants and control routines
#include <float.h>

#ifndef EM_INVALID
#define EM_INVALID _EM_INVALID
#endif

#ifndef EM_DENORMAL
#define EM_DENORMAL _EM_DENORMAL
#endif

#ifndef EM_ZERODIVIDE
#define EM_ZERODIVIDE _EM_ZERODIVIDE
#endif

#ifndef EM_OVERFLOW
#define EM_OVERFLOW _EM_OVERFLOW
#endif

#ifndef EM_UNDERFLOW
#define EM_UNDERFLOW _EM_UNDERFLOW
#endif

#ifndef MCW_EM
#define MCW_EM _MCW_EM
#endif

#else // based on ISO C standard

#if !defined(BOOST_NO_FENV_H) 
  #include <boost/detail/fenv.hpp>
#endif

#endif

//____________________________________________________________________________//

namespace boost {

// ************************************************************************** //
// **************        detail::translator_holder_base        ************** //
// ************************************************************************** //

namespace detail {

class translator_holder_base;
typedef boost::shared_ptr<translator_holder_base> translator_holder_base_ptr;

class BOOST_TEST_DECL translator_holder_base {
protected:
    typedef boost::unit_test::const_string const_string;
public:
    // Constructor
    translator_holder_base( translator_holder_base_ptr next, const_string tag )
    : m_next( next )
    , m_tag( std::string() + tag )
    {
    }

    // Destructor
    virtual     ~translator_holder_base() {}

    // translator holder interface
    // invokes the function F inside the try/catch guarding against specific exception
    virtual int operator()( boost::function<int ()> const& F ) = 0;

    // erases specific translator holder from the chain
    translator_holder_base_ptr erase( translator_holder_base_ptr this_, const_string tag ) 
    {
        if( m_next )
            m_next = m_next->erase( m_next, tag );

        return m_tag == tag ? m_next : this_;
    }
#ifndef BOOST_NO_RTTI
    virtual translator_holder_base_ptr erase( translator_holder_base_ptr this_, std::type_info const& ) = 0;
    template<typename ExceptionType>
    translator_holder_base_ptr erase( translator_holder_base_ptr this_, boost::type<ExceptionType>* = 0 )
    {
        if( m_next )
            m_next = m_next->erase<ExceptionType>( m_next );

        return erase( this_, typeid(ExceptionType) );
    }
#endif

protected:
    // Data members
    translator_holder_base_ptr  m_next;
    std::string                 m_tag;
};

} // namespace detail

// ************************************************************************** //
// **************              execution_exception             ************** //
// ************************************************************************** //
    
//  design rationale: fear of being out (or nearly out) of memory.
    
class BOOST_TEST_DECL execution_exception {
    typedef boost::unit_test::const_string const_string;
public:
    enum error_code {
        //  These values are sometimes used as program return codes.
        //  The particular values have been chosen to avoid conflicts with
        //  commonly used program return codes: values < 100 are often user
        //  assigned, values > 255 are sometimes used to report system errors.
        //  Gaps in values allow for orderly expansion.
        
        no_error               = 0,   // for completeness only; never returned
        user_error             = 200, // user reported non-fatal error
        cpp_exception_error    = 205, // see note (1) below
        system_error           = 210, // see note (2) below
        timeout_error          = 215, // only detectable on certain platforms
        user_fatal_error       = 220, // user reported fatal error
        system_fatal_error     = 225  // see note (2) below
        
        //  Note 1: Only uncaught C++ exceptions are treated as errors.
        //  If the application catches a C++ exception, it will never reach
        //  the execution_monitor.
        
        //  Note 2: These errors include Unix signals and Windows structured
        //  exceptions.  They are often initiated by hardware traps.
        //
        //  The implementation decides what is a fatal_system_exception and what is
        //  just a system_exception.  Fatal errors are so likely to have corrupted
        //  machine state (like a stack overflow or addressing exception) that it
        //  is unreasonable to continue execution.
    };
    
    struct BOOST_TEST_DECL location {
        explicit    location( char const* file_name = 0, size_t line_num = 0, char const* func = 0 );

        const_string    m_file_name;
        size_t          m_line_num;
        const_string    m_function;
    };

    // Constructor
    execution_exception( error_code ec_, const_string what_msg_, location const& location_ ); // max length 256 inc '\0'

    // Access methods
    error_code      code() const    { return m_error_code; }
    const_string    what() const    { return m_what; }
    location const& where() const   { return m_location; }

private:
    // Data members
    error_code      m_error_code;
    const_string    m_what;
    location        m_location;
}; // execution_exception

// ************************************************************************** //
// **************               execution_monitor              ************** //
// ************************************************************************** //

class BOOST_TEST_DECL execution_monitor {
    typedef boost::unit_test::const_string const_string;
public:
    // Constructor
    execution_monitor();

    // Public properties
    
    //  The p_catch_system_errors parameter specifies whether the monitor should 
    //  try to catch system errors/exceptions that would cause program to crash 
    //  in regular case
    unit_test::readwrite_property<bool> p_catch_system_errors; 

    //  The p_auto_start_dbg parameter specifies whether the monitor should 
    //  try to attach debugger in case of caught system error
    unit_test::readwrite_property<bool> p_auto_start_dbg;

    //  The p_timeout parameter specifies the seconds that elapse before
    //  a timer_error occurs.  May be ignored on some platforms.
    unit_test::readwrite_property<int>  p_timeout;

    //  The p_use_alt_stack parameter specifies whether the monitor should
    //  use alternative stack for the signal catching
    unit_test::readwrite_property<bool> p_use_alt_stack;

    //  The p_detect_fp_exceptions parameter specifies whether the monitor should
    //  try to detect hardware floating point exceptions (!= 0), and which specific exception to catch
    unit_test::readwrite_property<unsigned> p_detect_fp_exceptions;

    int         execute( boost::function<int ()> const& F ); 
    //  Returns:  Value returned by function call F().
    //
    //  Effects:  Calls executes supplied function F inside a try/catch block which also may
    //  include other unspecified platform dependent error detection code.
    //
    //  Throws: execution_exception on an uncaught C++ exception,
    //  a hardware or software signal, trap, or other exception.
    //
    //  Note: execute() doesn't consider it an error for F to return a non-zero value.

    void         vexecute( boost::function<void ()> const& F ); 
    //  Effects:  Same as above, but returns nothing
    
    // register custom (user supplied) exception translator
    template<typename ExceptionType, typename ExceptionTranslator>
    void        register_exception_translator( ExceptionTranslator const& tr, const_string tag = const_string(), boost::type<ExceptionType>* = 0 );

    // erase custom exception translator
    void        erase_exception_translator( const_string tag )
    {
        m_custom_translators = m_custom_translators->erase( m_custom_translators, tag );
    }
    template<typename ExceptionType>
    void        erase_exception_translator( boost::type<ExceptionType>* = 0 )
    {
        m_custom_translators = m_custom_translators->erase<ExceptionType>( m_custom_translators );
    }

private:
    // implementation helpers
    int         catch_signals( boost::function<int ()> const& F );

    // Data members
    detail::translator_holder_base_ptr  m_custom_translators;
    boost::scoped_array<char>           m_alt_stack;
}; // execution_monitor

// ************************************************************************** //
// **************          detail::translator_holder           ************** //
// ************************************************************************** //

namespace detail {

template<typename ExceptionType, typename ExceptionTranslator>
class translator_holder : public translator_holder_base
{
public:
    explicit    translator_holder( ExceptionTranslator const& tr, translator_holder_base_ptr& next, const_string tag = const_string() )
    : translator_holder_base( next, tag ), m_translator( tr ) {}

    // translator holder interface
    virtual int operator()( boost::function<int ()> const& F )
    {
        try {
            return m_next ? (*m_next)( F ) : F();
        } catch( ExceptionType const& e ) {
            m_translator( e );
            return boost::exit_exception_failure;
        }
    }
#ifndef BOOST_NO_RTTI
    virtual translator_holder_base_ptr erase( translator_holder_base_ptr this_, std::type_info const& ti ) 
    {
        return ti == typeid(ExceptionType) ? m_next : this_;
    }
#endif

private:
    // Data members
    ExceptionTranslator m_translator;
};

} // namespace detail

template<typename ExceptionType, typename ExceptionTranslator>
void
execution_monitor::register_exception_translator( ExceptionTranslator const& tr, const_string tag, boost::type<ExceptionType>* )
{
    m_custom_translators.reset( 
        new detail::translator_holder<ExceptionType,ExceptionTranslator>( tr, m_custom_translators, tag ) );
}

// ************************************************************************** //
// **************               execution_aborted              ************** //
// ************************************************************************** //

struct execution_aborted {};

// ************************************************************************** //
// **************                  system_error                ************** //
// ************************************************************************** //

class system_error {
public:
    // Constructor
    explicit    system_error( char const* exp );

    unit_test::readonly_property<long>          p_errno; 
    unit_test::readonly_property<char const*>   p_failed_exp; 
};

#define BOOST_TEST_SYS_ASSERT( exp ) if( (exp) ) ; else throw ::boost::system_error( BOOST_STRINGIZE( exp ) )

// ************************************************************************** //
// **************Floating point exception management interface ************** //
// ************************************************************************** //

namespace fpe {

enum masks {
    BOOST_FPE_OFF       = 0,

#ifdef BOOST_SEH_BASED_SIGNAL_HANDLING
    BOOST_FPE_DIVBYZERO = EM_ZERODIVIDE,
    BOOST_FPE_INEXACT   = EM_INEXACT,
    BOOST_FPE_INVALID   = EM_INVALID,
    BOOST_FPE_OVERFLOW  = EM_OVERFLOW,
    BOOST_FPE_UNDERFLOW = EM_UNDERFLOW|EM_DENORMAL,

    BOOST_FPE_ALL       = MCW_EM,
#elif defined(BOOST_NO_FENV_H) || defined(BOOST_CLANG) 
    BOOST_FPE_ALL       = 1,
#else
    BOOST_FPE_DIVBYZERO = FE_DIVBYZERO,
    BOOST_FPE_INEXACT   = FE_INEXACT,
    BOOST_FPE_INVALID   = FE_INVALID,
    BOOST_FPE_OVERFLOW  = FE_OVERFLOW,
    BOOST_FPE_UNDERFLOW = FE_UNDERFLOW,

    BOOST_FPE_ALL       = FE_ALL_EXCEPT,
#endif
    BOOST_FPE_INV       = BOOST_FPE_ALL+1
};

//____________________________________________________________________________//

// return the previous set of enabled exceptions when successful, and BOOST_FPE_INV otherwise
unsigned BOOST_TEST_DECL enable( unsigned mask );
unsigned BOOST_TEST_DECL disable( unsigned mask );

//____________________________________________________________________________//

} // namespace fpe
}  // namespace boost


#include <boost/test/detail/enable_warnings.hpp>

#endif

