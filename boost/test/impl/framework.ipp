//  (C) Copyright Gennadiy Rozental 2005-2012.
//  Distributed under the Boost Software License, Version 1.0.
//  (See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/test for the library home page.
//
//  File        : $RCSfile$
//
//  Version     : $Revision$
//
//  Description : implements framework API - main driver for the test
// ***************************************************************************

#ifndef BOOST_TEST_FRAMEWORK_IPP_021005GER
#define BOOST_TEST_FRAMEWORK_IPP_021005GER

// Boost.Test
#include <boost/test/framework.hpp>
#include <boost/test/execution_monitor.hpp>
#include <boost/test/debug.hpp>
#include <boost/test/unit_test_log.hpp>
#include <boost/test/unit_test_monitor.hpp>
#include <boost/test/results_collector.hpp>
#include <boost/test/progress_monitor.hpp>
#include <boost/test/results_reporter.hpp>

#include <boost/test/tree/observer.hpp>
#include <boost/test/tree/test_unit.hpp>
#include <boost/test/tree/visitor.hpp>
#include <boost/test/tree/traverse.hpp>
#include <boost/test/tree/test_case_counter.hpp>

#if BOOST_TEST_SUPPORT_TOKEN_ITERATOR
#include <boost/test/utils/iterator/token_iterator.hpp>
#endif

#include <boost/test/unit_test_parameters.hpp>
#include <boost/test/detail/global_typedef.hpp>

#include <boost/test/utils/foreach.hpp>
#include <boost/test/utils/basic_cstring/io.hpp>

// Boost
#include <boost/timer.hpp>
#include <boost/bind.hpp>

// STL
#include <map>
#include <set>
#include <cstdlib>
#include <ctime>

#ifdef BOOST_NO_STDC_NAMESPACE
namespace std { using ::time; using ::srand; }
#endif

#include <boost/test/detail/suppress_warnings.hpp>

//____________________________________________________________________________//

namespace boost {
namespace unit_test {

// ************************************************************************** //
// **************            test_init call wrapper            ************** //
// ************************************************************************** //

namespace ut_detail {

void
invoke_init_func( init_unit_test_func init_func )
{
#ifdef BOOST_TEST_ALTERNATIVE_INIT_API
    if( !(*init_func)() )
        throw std::runtime_error( "test module initialization failed" );
#else
    test_suite*  manual_test_units = (*init_func)( framework::master_test_suite().argc, framework::master_test_suite().argv );

    if( manual_test_units )
        framework::master_test_suite().add( manual_test_units );
#endif
}

// ************************************************************************** //
// **************                  name_filter                 ************** //
// ************************************************************************** //

typedef std::list<std::pair<test_unit_id,bool> > tu_enable_list;

class name_filter : public test_tree_visitor {
    struct component {
        component( const_string name ) // has to be implicit
        {
            if( name == "*" )
                m_kind  = SFK_ALL;
            else if( first_char( name ) == '*' && last_char( name ) == '*' ) {
                m_kind  = SFK_SUBSTR;
                m_name  = name.substr( 1, name.size()-1 );
            }
            else if( first_char( name ) == '*' ) {
                m_kind  = SFK_TRAILING;
                m_name  = name.substr( 1 );
            }
            else if( last_char( name ) == '*' ) {
                m_kind  = SFK_LEADING;
                m_name  = name.substr( 0, name.size()-1 );
            }
            else {
                m_kind  = SFK_MATCH;
                m_name  = name;
            }
        }

        bool            pass( test_unit const& tu ) const
        {
            const_string name( tu.p_name );
    
            switch( m_kind ) {
            default:
            case SFK_ALL:
                return true;
            case SFK_LEADING:
                return name.substr( 0, m_name.size() ) == m_name;
            case SFK_TRAILING:
                return name.size() >= m_name.size() && name.substr( name.size() - m_name.size() ) == m_name;
            case SFK_SUBSTR:
                return name.find( m_name ) != const_string::npos;
            case SFK_MATCH:
                return m_name == tu.p_name.get();
            }
        }
        enum kind { SFK_ALL, SFK_LEADING, SFK_TRAILING, SFK_SUBSTR, SFK_MATCH };

        kind            m_kind;
        const_string    m_name;
    };

public:
    // Constructor
    name_filter( tu_enable_list& tu_to_enable, const_string tc_to_run ) : m_tu_to_enable( tu_to_enable ), m_depth( 0 )
    {
#ifdef BOOST_TEST_SUPPORT_TOKEN_ITERATOR
        string_token_iterator tit( tc_to_run, (dropped_delimeters = "/", kept_delimeters = dt_none) );

        while( tit != string_token_iterator() ) {
            m_components.push_back( std::vector<component>( string_token_iterator( *tit, (dropped_delimeters = ",", kept_delimeters = dt_none)  ), 
                                                            string_token_iterator() ) );

            ++tit;           
        }
#endif
    }

private:
    bool            filter_unit( test_unit const& tu )
    {
        // skip master test suite
        if( m_depth == 0 )
            return true;

        // corresponding name filters are at level m_depth-1
        std::vector<component> const& filters = m_components[m_depth-1];

        // look for match
        return std::find_if( filters.begin(), filters.end(), bind( &component::pass, _1, boost::ref(tu) ) ) != filters.end();
    }

    // test_tree_visitor interface
    virtual void    visit( test_case const& tc )
    {
        // make sure we only accept test cases if we match last component of the filter
        if( m_depth == m_components.size() && filter_unit( tc ) )
            m_tu_to_enable.push_back( std::make_pair( tc.p_id, false ) ); // found a test case; add it to enable list without children
    }
    virtual bool    test_suite_start( test_suite const& ts )
    {
        if( filter_unit( ts ) ) {
            if( m_depth < m_components.size() ) {
                ++m_depth;
                return true;
            }

            m_tu_to_enable.push_back( std::make_pair( ts.p_id, true ) ); // found a test suite; add it to enable list with children and stop recursion
        }

        return false;
    }
    virtual void    test_suite_finish( test_suite const& /*ts*/ )
    {
        --m_depth;
    }

    // Data members
    typedef std::vector<std::vector<component> > components_per_level;

    components_per_level    m_components;
    tu_enable_list&         m_tu_to_enable;
    unsigned                m_depth;
};

// ************************************************************************** //
// **************                 label_filter                 ************** //
// ************************************************************************** //

class label_filter : public test_tree_visitor {
public:
    label_filter( tu_enable_list& tu_to_enable, const_string label )
    : m_tu_to_enable( tu_to_enable )
    , m_label( label )
    {}

private:
    // test_tree_visitor interface
    virtual bool    visit( test_unit const& tu )
    {
        if( tu.has_label( m_label ) ) {
            // found a test unit; add it to list of tu to enable with children and stop recursion in case of suites
            m_tu_to_enable.push_back( std::make_pair( tu.p_id, tu.p_type == TUT_SUITE ) );
            return false;
        }

        return true;
    }

    // Data members
    tu_enable_list& m_tu_to_enable;
    const_string    m_label;
};

// ************************************************************************** //
// **************                 change_status                ************** //
// ************************************************************************** //

class change_status : public test_tree_visitor {
public:
    explicit        change_status( bool enable_or_disable ) 
    : m_new_status( enable_or_disable )
    , m_made_change( false )
    {}

    bool            made_change() const { return m_made_change; }

private:
    // test_tree_visitor interface
    virtual bool    visit( test_unit const& tu )
    {
        if( tu.p_enabled.get() ^ m_new_status ) {
            tu.p_enabled.value  = m_new_status;
            m_made_change       = true;
        }
        return true;
    }

    // Data members
    bool            m_new_status;
    bool            m_made_change;
};

// ************************************************************************** //
// **************                 change_status                ************** //
// ************************************************************************** //

class remove_disabled : public test_tree_visitor {
public:
    explicit        remove_disabled( bool remove_from_tree ) 
    : m_remove_from_tree( remove_from_tree )
    , m_made_change( false )
    {}

    bool            made_change() const { return m_made_change; }

private:
    // test_tree_visitor interface
    virtual bool    visit( test_unit const& tu )
    {
        if( !m_remove_from_tree && !tu.p_enabled )
            return false;

        // check if any of dependencies are disabled
        if( tu.p_enabled ) {
            BOOST_TEST_FOREACH( test_unit_id, dep_id, tu.p_dependencies.get() ) {
                test_unit const& dep = framework::get( dep_id, TUT_ANY );

                if( !dep.p_enabled ) {
                    BOOST_TEST_MESSAGE( "Disable test " << tu.p_type_name << ' ' << tu.p_name << 
                                        " since it depends on disabled test " << dep.p_type_name << ' ' << dep.p_name );

                    tu.p_enabled.value = false;
                    m_made_change      = true;
                    break;
                }
            }
        }

        // if this test unit is disabled - disable all subunits and remove it from the tree if requested
        if( !tu.p_enabled ) {
            if( tu.p_type == TUT_SUITE ) {
                ut_detail::change_status disabler( false );
                traverse_test_tree( tu.p_id, disabler, true );
                m_made_change |= disabler.made_change();
            }

            if( m_remove_from_tree )
                framework::get<test_suite>( tu.p_parent_id ).remove( tu.p_id );
        }

        return tu.p_enabled;
    }

    // Data members
    bool m_remove_from_tree;
    bool m_made_change;
};

} // namespace ut_detail

// ************************************************************************** //
// **************                   framework                  ************** //
// ************************************************************************** //

class framework_impl : public test_tree_visitor {
public:
    framework_impl()
    : m_curr_test_case( INV_TEST_UNIT_ID )
    , m_next_test_case_id( MIN_TEST_CASE_ID )
    , m_next_test_suite_id( MIN_TEST_SUITE_ID )
    , m_is_initialized( false )
    , m_test_in_progress( false )
    , m_context_idx( 0 )
    {
    }

    ~framework_impl() { clear(); }

    void            clear()
    {
        while( !m_test_units.empty() ) {
            test_unit_store::value_type const& tu     = *m_test_units.begin();
            test_unit const*                   tu_ptr = tu.second;

            // the delete will erase this element from map
            if( ut_detail::test_id_2_unit_type( tu.second->p_id ) == TUT_SUITE )
                delete static_cast<test_suite const*>(tu_ptr);
            else
                delete static_cast<test_case const*>(tu_ptr);
        }
    }
                                    
    void            set_tu_id( test_unit& tu, test_unit_id id ) { tu.p_id.value = id; }

    // test_tree_visitor interface implementation
    bool            test_unit_start( test_unit const& tu )
    {
        if( !tu.check_dependencies() ) {
            BOOST_TEST_FOREACH( test_observer*, to, m_observers )
                to->test_unit_skipped( tu );

            return false;
        }

        // notify all observers
        BOOST_TEST_FOREACH( test_observer*, to, m_observers )
            to->test_unit_start( tu );

        // first execute setup fixtures if any; any failure here leads to test unit abortion
        BOOST_TEST_FOREACH( test_unit_fixture_ptr, F, tu.p_fixtures.get() ) {
            if( unit_test_monitor.execute_and_translate( boost::bind( &test_unit_fixture::setup, F ), 0 ) != unit_test_monitor_t::test_ok )
                return false;
        }

        return true;
    }

    void            test_unit_finish( test_unit const& tu, unit_test_monitor_t::error_level run_result, unsigned long elapsed )
    {
        // if run error is critical skip teardown, who knows what the state of the program at this point
        if( !unit_test_monitor.is_critical_error( run_result ) ) {
            // execute teardown fixtures if any
            BOOST_TEST_FOREACH( test_unit_fixture_ptr, F, tu.p_fixtures.get() ) {
                run_result = unit_test_monitor.execute_and_translate( boost::bind( &test_unit_fixture::teardown, F ), 0 );

                if( unit_test_monitor.is_critical_error( run_result ) )
                    break;
            }
        }

        // notify all observers about abortion
        if( unit_test_monitor.is_critical_error( run_result ) ) {
            BOOST_TEST_FOREACH( test_observer*, to, m_observers )
                to->test_aborted();
        }

        // notify all observers about completion
        BOOST_TEST_REVERSE_FOREACH( test_observer*, to, m_observers )
            to->test_unit_finish( tu, elapsed );

        if( unit_test_monitor.is_critical_error( run_result ) )
            throw framework::test_being_aborted();
    }

    // test_tree_visitor interface implementation
    void            visit( test_case const& tc )
    {
        // all the setup work
        if( !test_unit_start( tc ) )
            return;

        // setup contexts
        m_context_idx = 0;
        test_unit_id bkup = m_curr_test_case;
        m_curr_test_case = tc.p_id;

        // execute the test case body
        boost::timer tc_timer;
        unit_test_monitor_t::error_level run_result = unit_test_monitor.execute_and_translate( tc.p_test_func, tc.p_timeout );
        unsigned long elapsed = static_cast<unsigned long>( tc_timer.elapsed() * 1e6 );

        // cleanup leftover context
        m_context.clear();

        // restore state and abort if necessary
        m_curr_test_case = bkup;

        // all the teardown work
        test_unit_finish( tc, run_result, elapsed );
    }

    bool            test_suite_start( test_suite const& ts )
    {
        return test_unit_start( ts );
    }

    void            test_suite_finish( test_suite const& ts )
    {
        test_unit_finish( ts, unit_test_monitor_t::test_ok, 0 );
    }

    //////////////////////////////////////////////////////////////////
    struct priority_order {
        bool operator()( test_observer* lhs, test_observer* rhs ) const
        {
            return (lhs->priority() < rhs->priority()) || ((lhs->priority() == rhs->priority()) && (lhs < rhs));
        }
    };

    typedef std::map<test_unit_id,test_unit*>       test_unit_store;
    typedef std::set<test_observer*,priority_order> observer_store;
    struct context_frame {
        context_frame( std::string const& d, int id, bool sticky )
        : descr( d )
        , frame_id( id )
        , is_sticky( sticky )
        {}

        std::string descr;
        int         frame_id;
        bool        is_sticky;
    };
    typedef std::vector<context_frame> context_data;

    master_test_suite_t* m_master_test_suite;
    std::vector<test_suite*> m_auto_test_suites;

    test_unit_id    m_curr_test_case;
    test_unit_store m_test_units;

    test_unit_id    m_next_test_case_id;
    test_unit_id    m_next_test_suite_id;

    bool            m_is_initialized;
    bool            m_test_in_progress;

    observer_store  m_observers;
    context_data    m_context;
    int             m_context_idx;

    boost::execution_monitor m_aux_em;
};

//____________________________________________________________________________//

namespace {

#if defined(__CYGWIN__)
framework_impl& s_frk_impl() { static framework_impl* the_inst = 0; if(!the_inst) the_inst = new framework_impl; return *the_inst; }
#else
framework_impl& s_frk_impl() { static framework_impl the_inst; return the_inst; }
#endif

} // local namespace

//____________________________________________________________________________//

namespace framework {

// ************************************************************************** //
// **************                 apply_filters                ************** //
// ************************************************************************** //

namespace impl {
void
apply_filters( test_unit_id master_tu_id )
{
    if( runtime_config::test_to_run().empty() ) {
        // enable all test units for this run
        ut_detail::change_status enabler( true );
        traverse_test_tree( master_tu_id, enabler, true );
    }
    else {
        // 10. collect tu to enable and disable based on filters
        ut_detail::tu_enable_list tu_to_enable;
        ut_detail::tu_enable_list tu_to_disable;
        bool had_enable_filter = false;

        BOOST_TEST_FOREACH( const_string, filter, runtime_config::test_to_run() ) {
            BOOST_TEST_SETUP_ASSERT( !filter.is_empty(), "Invalid filter specification" );

            bool enable_or_disable = true;

            // 11. Decide if this "enabler" or "disabler" filter
            if( filter[0] == '!' ) {
                enable_or_disable = false;
                filter.trim_left( 1 );
                BOOST_TEST_SETUP_ASSERT( !filter.is_empty(), "Invalid filter specification" );
            }

            if( enable_or_disable )
                had_enable_filter = true;

            // 12. Choose between name filter and label filter
            if( filter[0] == '@' ) {
                filter.trim_left( 1 );
                ut_detail::label_filter lf( enable_or_disable ? tu_to_enable : tu_to_disable, filter );
                traverse_test_tree( master_tu_id, lf, true );
            }
            else {
                ut_detail::name_filter nf( enable_or_disable ? tu_to_enable : tu_to_disable, filter );
                traverse_test_tree( master_tu_id, nf, true );
            }
        }

        // 15. At this point we collected 2 lists: tu to enable and tu to disable
        //     If first list is not empty we'll disable all tu and only enable those we need
        //     otherwise we enable all
        //     If second list is not empty we'll go through tree based on whatever is currently 
        //     enabled and disable units from second list

        ut_detail::change_status change_status( tu_to_enable.empty() && !had_enable_filter );
        traverse_test_tree( master_tu_id, change_status, true );

        // 20. enable tu collected along with their parents, dependencies and children where necessary
        while( !tu_to_enable.empty() ) {
            std::pair<test_unit_id,bool>    data = tu_to_enable.front();
            test_unit const&                tu   = framework::get( data.first, TUT_ANY );

            tu_to_enable.pop_front();

            if( tu.p_enabled ) 
                continue;

            // 21. enable tu
            tu.p_enabled.value = true;

            // 22. master test suite - we are done
            if( tu.p_id == master_tu_id )
                continue;

            // 23. add parent to the list (without children)
            if( !framework::get( tu.p_parent_id, TUT_ANY ).p_enabled )
                tu_to_enable.push_back( std::make_pair( tu.p_parent_id, false ) );

            // 24. add dependencies to the list (with children)
            BOOST_TEST_FOREACH( test_unit_id, dep_id, tu.p_dependencies.get() ) {
                test_unit const& dep = framework::get( dep_id, TUT_ANY );

                if( !dep.p_enabled ) {
                    BOOST_TEST_MESSAGE( "Including test " << dep.p_type_name << ' ' << dep.p_name << 
                                        " as a dependacy of test " << tu.p_type_name << ' ' << tu.p_name );

                    tu_to_enable.push_back( std::make_pair( dep_id, true ) );
                }
            }

            // 25. add all children to the list recursively
            if( data.second && tu.p_type == TUT_SUITE ) {
                class collect_disabled : public test_tree_visitor {
                public:
                    explicit        collect_disabled( ut_detail::tu_enable_list& tu_to_enable ) : m_tu_to_enable( tu_to_enable ) {}

                private:
                    // test_tree_visitor interface
                    virtual bool    visit( test_unit const& tu )
                    {
                        if( !tu.p_enabled )
                            m_tu_to_enable.push_back( std::make_pair( tu.p_id, false ) );

                        return true;
                    }

                    // Data members
                    ut_detail::tu_enable_list& m_tu_to_enable;
                } V( tu_to_enable );

                traverse_test_tree( tu.p_id, V, true );
            }
        }

        // 30. disable tu collected along with their parents, dependents and children where necessary
        bool made_change = false;

        // 31. First go through the collected list and disable all tu along with their children
        while( !tu_to_disable.empty() ) {
            std::pair<test_unit_id,bool>    data = tu_to_disable.front();
            test_unit const&                tu   = framework::get( data.first, TUT_ANY );

            tu_to_disable.pop_front();

            if( !tu.p_enabled ) 
                continue;

            ut_detail::change_status disabler( false );
            traverse_test_tree( tu.p_id, disabler, true );
            made_change |= disabler.made_change();
        }

        // 32. Now disable all tu which depends on disabled
        while( made_change ) {
            ut_detail::remove_disabled rd( false );

            traverse_test_tree( master_tu_id, rd, true );
            made_change = rd.made_change();
        } 
    }
}

//____________________________________________________________________________//

} // namespace impl

// ************************************************************************** //
// **************                framework::init               ************** //
// ************************************************************************** //

void
init( init_unit_test_func init_func, int argc, char* argv[] )
{
    runtime_config::init( argc, argv );

    // set the log level and format
    unit_test_log.set_threshold_level( runtime_config::log_level() );
    unit_test_log.set_format( runtime_config::log_format() );

    // set the report level and format
    results_reporter::set_level( runtime_config::report_level() );
    results_reporter::set_format( runtime_config::report_format() );

    // register default observers
    register_observer( results_collector );
    register_observer( unit_test_log );

    if( runtime_config::show_progress() )
        register_observer( progress_monitor );

    if( runtime_config::detect_memory_leaks() > 0 ) {
        debug::detect_memory_leaks( true, runtime_config::memory_leaks_report_file() );
        debug::break_memory_alloc( runtime_config::detect_memory_leaks() );
    }

    // init master unit test suite
    master_test_suite().argc = argc;
    master_test_suite().argv = argv;

    try {
        s_frk_impl().m_aux_em.vexecute( boost::bind( &ut_detail::invoke_init_func, init_func ) );
    }
    catch( execution_exception const& ex )  {
        throw setup_error( ex.what() );
    }

    // Apply all decorators to the auto test units
    class apply_decorators : public test_tree_visitor {
    private:
        // test_tree_visitor interface
        virtual bool    visit( test_unit const& tu )
        {
            if( tu.p_decorators.get() )
                tu.p_decorators.get()->apply( const_cast<test_unit&>(tu) );

            return true;
        }
    } ad;
    traverse_test_tree( master_test_suite().p_id, ad, true );

    // Let's see if anything was disabled during construction. These test units and anything 
    // that depends on them are removed from the test tree
    bool made_change = false;
    do {
        ut_detail::remove_disabled rd( true );
        traverse_test_tree( master_test_suite().p_id, rd, true );

        made_change = rd.made_change();
    } while( made_change );

    // apply all name and label filters
    impl::apply_filters( master_test_suite().p_id );

    // We are Done!
    s_frk_impl().m_is_initialized = true;
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************                is_initialized                ************** //
// ************************************************************************** //

bool
is_initialized()
{
    return  s_frk_impl().m_is_initialized;
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************             framework::shutdown              ************** //
// ************************************************************************** //

void
shutdown()
{
    // eliminating some fake memory leak reports. See for more details:
    // http://connect.microsoft.com/VisualStudio/feedback/details/106937/memory-leaks-reported-by-debug-crt-inside-typeinfo-name

#  if BOOST_WORKAROUND(BOOST_MSVC,  <= 1600 ) && !defined(_DLL) && defined(_DEBUG)
#  if BOOST_WORKAROUND(BOOST_MSVC,  < 1600 )
#define _Next next
#define _MemPtr memPtr
#endif
   __type_info_node* pNode   = __type_info_root_node._Next;
   __type_info_node* tmpNode = &__type_info_root_node;

   for( ; pNode!=NULL; pNode = tmpNode ) {
      tmpNode = pNode->_Next;
      delete pNode->_MemPtr;
      delete pNode;
   }
#  if BOOST_WORKAROUND(BOOST_MSVC,  < 1600 )
#undef _Next 
#undef _MemPtr
#endif
#  endif
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************              register_test_unit              ************** //
// ************************************************************************** //

void
register_test_unit( test_case* tc )
{
    BOOST_TEST_SETUP_ASSERT( tc->p_id == INV_TEST_UNIT_ID, BOOST_TEST_L( "test case already registered" ) );

    test_unit_id new_id = s_frk_impl().m_next_test_case_id;

    BOOST_TEST_SETUP_ASSERT( new_id != MAX_TEST_CASE_ID, BOOST_TEST_L( "too many test cases" ) );

    typedef framework_impl::test_unit_store::value_type map_value_type;

    s_frk_impl().m_test_units.insert( map_value_type( new_id, tc ) );
    s_frk_impl().m_next_test_case_id++;

    s_frk_impl().set_tu_id( *tc, new_id );
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************              register_test_unit              ************** //
// ************************************************************************** //

void
register_test_unit( test_suite* ts )
{
    BOOST_TEST_SETUP_ASSERT( ts->p_id == INV_TEST_UNIT_ID, BOOST_TEST_L( "test suite already registered" ) );

    test_unit_id new_id = s_frk_impl().m_next_test_suite_id;

    BOOST_TEST_SETUP_ASSERT( new_id != MAX_TEST_SUITE_ID, BOOST_TEST_L( "too many test suites" ) );

    typedef framework_impl::test_unit_store::value_type map_value_type;
    s_frk_impl().m_test_units.insert( map_value_type( new_id, ts ) );
    s_frk_impl().m_next_test_suite_id++;

    s_frk_impl().set_tu_id( *ts, new_id );
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************             deregister_test_unit             ************** //
// ************************************************************************** //

void
deregister_test_unit( test_unit* tu )
{
    s_frk_impl().m_test_units.erase( tu->p_id );
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************                     clear                    ************** //
// ************************************************************************** //

void
clear()
{
    s_frk_impl().clear();
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************               register_observer              ************** //
// ************************************************************************** //

void
register_observer( test_observer& to )
{
    s_frk_impl().m_observers.insert( &to );
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************              deregister_observer             ************** //
// ************************************************************************** //

void
deregister_observer( test_observer& to )
{
    s_frk_impl().m_observers.erase( &to );
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************                reset_observer                ************** //
// ************************************************************************** //

void
reset_observers()
{
    s_frk_impl().m_observers.clear();
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************                  add_context                 ************** //
// ************************************************************************** //

int
add_context( ::boost::unit_test::lazy_ostream const& context_descr, bool sticky )
{
    std::stringstream buffer;
    context_descr( buffer );
    int res_idx  = s_frk_impl().m_context_idx++;

    s_frk_impl().m_context.push_back( framework_impl::context_frame( buffer.str(), res_idx, sticky ) );

    return res_idx;
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************                 clear_context                ************** //
// ************************************************************************** //

struct frame_with_id {
    explicit frame_with_id( int id ) : m_id( id ) {}

    bool    operator()( framework_impl::context_frame const& f )
    {
        return f.frame_id == m_id;
    }
    int     m_id;
};

void
clear_context( int frame_id )
{
    if( frame_id == -1 ) {   // clear all non sticky frames
        for( int i=s_frk_impl().m_context.size()-1; i>=0; i-- )
            if( !s_frk_impl().m_context[i].is_sticky )
                s_frk_impl().m_context.erase( s_frk_impl().m_context.begin()+i );
    }
 
    else { // clear specific frame
        framework_impl::context_data::iterator it = 
            std::find_if( s_frk_impl().m_context.begin(), s_frk_impl().m_context.end(), frame_with_id( frame_id ) );

        if( it != s_frk_impl().m_context.end() ) // really an internal error if this is not true
            s_frk_impl().m_context.erase( it );
    }
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************                  get_context                 ************** //
// ************************************************************************** //

context_generator
get_context()
{
    return context_generator();
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************               context_generator              ************** //
// ************************************************************************** //

bool
context_generator::is_empty() const
{
    return s_frk_impl().m_context.empty();
}

//____________________________________________________________________________//

const_string
context_generator::next() const
{
    return m_curr_frame < s_frk_impl().m_context.size() ? s_frk_impl().m_context[m_curr_frame++].descr : const_string();
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************               master_test_suite              ************** //
// ************************************************************************** //

master_test_suite_t&
master_test_suite()
{
    if( !s_frk_impl().m_master_test_suite )
        s_frk_impl().m_master_test_suite = new master_test_suite_t;

    return *s_frk_impl().m_master_test_suite;
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************            current_auto_test_suite           ************** //
// ************************************************************************** //

test_suite&
current_auto_test_suite( test_suite* ts, bool push_or_pop )
{
    if( s_frk_impl().m_auto_test_suites.empty() )
        s_frk_impl().m_auto_test_suites.push_back( &framework::master_test_suite() );

    if( !push_or_pop )
        s_frk_impl().m_auto_test_suites.pop_back();
    else if( ts )
        s_frk_impl().m_auto_test_suites.push_back( ts );

    return *s_frk_impl().m_auto_test_suites.back();
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************               current_test_case              ************** //
// ************************************************************************** //

test_case const&
current_test_case()
{
    return get<test_case>( s_frk_impl().m_curr_test_case );
}

//____________________________________________________________________________//

test_unit_id
current_test_case_id()
{
    return s_frk_impl().m_curr_test_case;
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************                framework::get                ************** //
// ************************************************************************** //

test_unit&
get( test_unit_id id, test_unit_type t )
{
    test_unit* res = s_frk_impl().m_test_units[id];

    if( (res->p_type & t) == 0 )
        throw internal_error( "Invalid test unit type" );

    return *res;
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************                framework::run                ************** //
// ************************************************************************** //

void
run( test_unit_id id, bool continue_test )
{
    if( id == INV_TEST_UNIT_ID )
        id = master_test_suite().p_id;

    test_case_counter tcc;
    traverse_test_tree( id, tcc );

    BOOST_TEST_SETUP_ASSERT( tcc.p_count != 0 , runtime_config::test_to_run().empty() 
        ? BOOST_TEST_L( "test tree is empty" ) 
        : BOOST_TEST_L( "no test cases matching filter" ) );

    bool    call_start_finish   = !continue_test || !s_frk_impl().m_test_in_progress;
    bool    was_in_progress     = s_frk_impl().m_test_in_progress;

    s_frk_impl().m_test_in_progress = true;

    if( call_start_finish ) {
        BOOST_TEST_FOREACH( test_observer*, to, s_frk_impl().m_observers ) {
            try {
                s_frk_impl().m_aux_em.vexecute( boost::bind( &test_observer::test_start, to, tcc.p_count ) );
            }
            catch( execution_exception const& ex )  {
                throw setup_error( ex.what() );
            }
        }
    }

    switch( runtime_config::random_seed() ) {
    case 0:
        break;
    case 1: {
        unsigned int seed = static_cast<unsigned int>( std::time( 0 ) );
        BOOST_TEST_MESSAGE( "Test cases order is shuffled using seed: " << seed );
        std::srand( seed );
        break;
    }
    default:
        BOOST_TEST_MESSAGE( "Test cases order is shuffled using seed: " << runtime_config::random_seed() );
        std::srand( runtime_config::random_seed() );
    }

    try {
        traverse_test_tree( id, s_frk_impl() );
    }
    catch( framework::test_being_aborted const& ) {
        // abort already reported
    }

    if( call_start_finish ) {
        BOOST_TEST_REVERSE_FOREACH( test_observer*, to, s_frk_impl().m_observers )
            to->test_finish();
    }

    s_frk_impl().m_test_in_progress = was_in_progress;
}

//____________________________________________________________________________//

void
run( test_unit const* tu, bool continue_test )
{
    run( tu->p_id, continue_test );
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************               assertion_result               ************** //
// ************************************************************************** //

void
assertion_result( unit_test::assertion_result ar )
{
    BOOST_TEST_FOREACH( test_observer*, to, s_frk_impl().m_observers )
        to->assertion_result( ar );
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************               exception_caught               ************** //
// ************************************************************************** //

void
exception_caught( execution_exception const& ex )
{
    BOOST_TEST_FOREACH( test_observer*, to, s_frk_impl().m_observers )
        to->exception_caught( ex );
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************               test_unit_aborted              ************** //
// ************************************************************************** //

void
test_unit_aborted( test_unit const& tu )
{
    BOOST_TEST_FOREACH( test_observer*, to, s_frk_impl().m_observers )
        to->test_unit_aborted( tu );
}

//____________________________________________________________________________//

} // namespace framework
} // namespace unit_test
} // namespace boost

#include <boost/test/detail/enable_warnings.hpp>

#endif // BOOST_TEST_FRAMEWORK_IPP_021005GER
