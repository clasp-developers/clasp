/* Copyright Vladimir Prus 2003. Distributed under the Boost */
/* Software License, Version 1.0. (See accompanying */
/* file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt) */

#include "class.h"
#include "strings.h"
#include "variable.h"
#include "frames.h"
#include "rules.h"
#include "newstr.h"

#include "hash.h"


static struct hash * classes = 0;


static void check_defined( LIST * class_names )
{
    for ( ; class_names; class_names = class_names->next )
    {
        char * * p = &class_names->string;
        if ( !hashcheck( classes, (HASHDATA * *)&p ) )
        {
            printf( "Class %s is not defined\n", class_names->string );
            abort();
        }
    }
}


static char * class_module_name( char * declared_name )
{
    string name[ 1 ];
    char * result;

    string_new( name );
    string_append( name, "class@" );
    string_append( name, declared_name );

    result = newstr( name->value );
    string_free( name );

    return result;
}


struct import_base_data
{
    char     * base_name;
    module_t * base_module;
    module_t * class_module;
};


static void import_base_rule( void * r_, void * d_ )
{
    RULE * r = (RULE *)r_;
    RULE * ir1;
    RULE * ir2;
    struct import_base_data * d = (struct import_base_data *)d_;
    string qualified_name[ 1 ];

    string_new      ( qualified_name               );
    string_append   ( qualified_name, d->base_name );
    string_push_back( qualified_name, '.'          );
    string_append   ( qualified_name, r->name      );

    ir1 = import_rule( r, d->class_module, r->name );
    ir2 = import_rule( r, d->class_module, qualified_name->value );

    /* Copy 'exported' flag. */
    ir1->exported = ir2->exported = r->exported;

    /* If we are importing a class method, localize it. */
    if ( ( r->module == d->base_module ) || ( r->module->class_module &&
        ( r->module->class_module == d->base_module ) ) )
        ir1->module = ir2->module = d->class_module;

    string_free( qualified_name );
}


/*
 * For each exported rule 'n', declared in class module for base, imports that
 * rule in 'class' as 'n' and as 'base.n'. Imported rules are localized and
 * marked as exported.
 */

static void import_base_rules( module_t * class, char * base )
{
    module_t * base_module = bindmodule( class_module_name( base ) );
    struct import_base_data d;
    d.base_name = base;
    d.base_module = base_module;
    d.class_module = class;

    if ( base_module->rules )
        hashenumerate( base_module->rules, import_base_rule, &d );

    import_module( imported_modules( base_module ), class );
}


char * make_class_module( LIST * xname, LIST * bases, FRAME * frame )
{
    char       * name = class_module_name( xname->string );
    char     * * pp = &xname->string;
    module_t   * class_module = 0;
    module_t   * outer_module = frame->module;

    if ( !classes )
        classes = hashinit( sizeof( char * ), "classes" );

    if ( hashcheck( classes, (HASHDATA * *)&pp ) )
    {
        printf( "Class %s already defined\n", xname->string );
        abort();
    }
    else
    {
        hashenter( classes, (HASHDATA * *)&pp );
    }
    check_defined( bases );

    class_module = bindmodule( name );

    exit_module( outer_module );
    enter_module( class_module );

    var_set( "__name__", xname, VAR_SET );
    var_set( "__bases__", bases, VAR_SET );

    exit_module( class_module );
    enter_module( outer_module );

    for ( ; bases; bases = bases->next )
        import_base_rules( class_module, bases->string );

    return name;
}
