/*
 *  Copyright 2001-2004 David Abrahams.
 *  Distributed under the Boost Software License, Version 1.0.
 *  (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt)
 */
#include "jam.h"

#include "modules.h"
#include "string.h"
#include "hash.h"
#include "newstr.h"
#include "lists.h"
#include "parse.h"
#include "rules.h"
#include "variable.h"
#include "strings.h"
#include <assert.h>

static struct hash * module_hash = 0;


static char * new_module_str( module_t * m, char * suffix )
{
    char * result;
    string s;
    string_copy( &s, m->name );
    string_append( &s, suffix );
    result = newstr( s.value );
    string_free( &s );
    return result;
}


module_t * bindmodule( char * name )
{
    PROFILE_ENTER( BINDMODULE );

    string s;
    module_t m_;
    module_t * m = &m_;

    if ( !module_hash )
        module_hash = hashinit( sizeof( module_t ), "modules" );

    string_new( &s );
    if ( name )
    {
        string_append( &s, name );
        string_push_back( &s, '.' );
    }

    m->name = s.value;

    if ( hashenter( module_hash, (HASHDATA * *)&m ) )
    {
        m->name = newstr( m->name );
        m->variables = 0;
        m->rules = 0;
        m->imported_modules = 0;
        m->class_module = 0;
        m->native_rules = 0;
        m->user_module = 0;
    }
    string_free( &s );

    PROFILE_EXIT( BINDMODULE );

    return m;
}

/*
 * demand_rules() - Get the module's "rules" hash on demand.
 */
struct hash * demand_rules( module_t * m )
{
    if ( !m->rules )
        m->rules = hashinit( sizeof( RULE ), new_module_str( m, "rules" ) );
    return m->rules;
}


/*
 * delete_module() - wipe out the module's rules and variables.
 */

static void delete_rule_( void * xrule, void * data )
{
    rule_free( (RULE *)xrule );
}


void delete_module( module_t * m )
{
    /* Clear out all the rules. */
    if ( m->rules )
    {
        hashenumerate( m->rules, delete_rule_, (void *)0 );
        hashdone( m->rules );
        m->rules = 0;
    }

    if ( m->variables )
    {
        var_hash_swap( &m->variables );
        var_done();
        var_hash_swap( &m->variables );
        m->variables = 0;
    }
}


module_t * root_module()
{
    static module_t * root = 0;
    if ( !root )
        root = bindmodule( 0 );
    return root;
}

void enter_module( module_t * m )
{
    var_hash_swap( &m->variables );
}


void exit_module( module_t * m )
{
    var_hash_swap( &m->variables );
}


void import_module( LIST * module_names, module_t * target_module )
{
    PROFILE_ENTER( IMPORT_MODULE );

    struct hash * h;

    if ( !target_module->imported_modules )
        target_module->imported_modules = hashinit( sizeof( char * ), "imported" );
    h = target_module->imported_modules;

    for ( ; module_names; module_names = module_names->next )
    {
        char * s = module_names->string;
        char * * ss = &s;
        hashenter( h, (HASHDATA * *)&ss );
    }

    PROFILE_EXIT( IMPORT_MODULE );
}


static void add_module_name( void * r_, void * result_ )
{
    char * * r = (char * *)r_;
    LIST * * result = (LIST * *)result_;

    *result = list_new( *result, copystr( *r ) );
}


LIST * imported_modules( module_t * module )
{
    LIST * result = L0;
    if ( module->imported_modules )
        hashenumerate( module->imported_modules, add_module_name, &result );
    return result;
}
