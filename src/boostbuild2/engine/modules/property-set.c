/* Copyright Vladimir Prus 2003. Distributed under the Boost */
/* Software License, Version 1.0. (See accompanying */
/* file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt) */

#include "../compile.h"
#include "../lists.h"
#include "../native.h"
#include "../object.h"
#include "../strings.h"
#include "../timestamp.h"
#include "../variable.h"

#include <string.h>


LIST * get_grist( char const * const f )
{
    char const * const end = strchr( f, '>' );
    string s[ 1 ];
    LIST * result;

    string_new( s );

    string_append_range( s, f, end + 1 );
    result = list_new( object_new( s->value ) );

    string_free( s );
    return result;
}


/*
rule create ( raw-properties * )
{
    raw-properties = [ sequence.unique
        [ sequence.insertion-sort $(raw-properties) ] ] ;

    local key = $(raw-properties:J=-:E=) ;

    if ! $(.ps.$(key))
    {
        .ps.$(key) = [ new property-set $(raw-properties) ] ;
    }
    return $(.ps.$(key)) ;
}
*/

LIST * property_set_create( FRAME * frame, int flags )
{
    LIST * properties = lol_get( frame->args, 0 );
    LIST * sorted = L0;
    LIST * unique;
    LIST * val;
    string var[ 1 ];
    OBJECT * name;
    LISTITER iter;
    LISTITER end;

    sorted = list_sort( properties );
    unique = list_unique( sorted );

    string_new( var );
    string_append( var, ".ps." );

    iter = list_begin( unique );
    end = list_end( unique );
    for ( ; iter != end; iter = list_next( iter ) )
    {
        string_append( var, object_str( list_item( iter ) ) );
        string_push_back( var, '-' );
    }
    name = object_new( var->value );
    val = var_get( frame->module, name );
    if ( list_empty( val ) )
    {
        OBJECT * const rulename = object_new( "new" );
        val = call_rule( rulename, frame, list_append( list_new( object_new(
            "property-set" ) ), unique ), 0 );
        /* The 'unique' variable is freed in 'call_rule'. */
        object_free( rulename );
        var_set( frame->module, name, list_copy( val ), VAR_SET );
    }
    else
    {
        list_free( unique );
        val = list_copy( val );
    }
    object_free( name );
    string_free( var );
    list_free( sorted );

    return val;
}


void init_property_set()
{
    char const * args[] = { "raw-properties", "*", 0 };
    declare_native_rule( "property-set", "create", args, property_set_create, 1
        );
}
