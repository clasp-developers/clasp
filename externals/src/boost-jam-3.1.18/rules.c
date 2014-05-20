/*
 * Copyright 1993, 1995 Christopher Seiwald.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

# include "jam.h"
# include "lists.h"
# include "parse.h"
# include "variable.h"
# include "rules.h"
# include "newstr.h"
# include "hash.h"
# include "modules.h"
# include "search.h"
# include "lists.h"
# include "pathsys.h"
# include "timestamp.h"

/*  This file is ALSO:
 *  Copyright 2001-2004 David Abrahams.
 *  Distributed under the Boost Software License, Version 1.0.
 *  (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt)
 */

/*
 * rules.c - access to RULEs, TARGETs, and ACTIONs
 *
 * External routines:
 *
 *    bindrule()     - return pointer to RULE, creating it if necessary.
 *    bindtarget()   - return pointer to TARGET, creating it if necessary.
 *    touch_target() - mark a target to simulate being new.
 *    targetlist()   - turn list of target names into a TARGET chain.
 *    targetentry()  - add a TARGET to a chain of TARGETS.
 *    actionlist()   - append to an ACTION chain.
 *    addsettings()  - add a deferred "set" command to a target.
 *    pushsettings() - set all target specific variables.
 *    popsettings()  - reset target specific variables to their pre-push values.
 *    freesettings() - delete a settings list.
 *    rules_done()   - free RULE and TARGET tables.
 *
 * 04/12/94 (seiwald) - actionlist() now just appends a single action.
 * 08/23/94 (seiwald) - Support for '+=' (append to variable)
 */

static void set_rule_actions( RULE *, rule_actions * );
static void set_rule_body   ( RULE *, argument_list *, PARSE * procedure );

static struct hash * targethash = 0;

struct _located_target
{
    char   * file_name;
    TARGET * target;
};
typedef struct _located_target LOCATED_TARGET ;

static struct hash * located_targets = 0;


/*
 * target_include() - adds the 'included' TARGET to the list of targets included
 * by the 'including' TARGET. Such targets are modeled as dependencies of the
 * internal include node belonging to the 'including' TARGET.
 */

void target_include( TARGET * including, TARGET * included )
{
    TARGET * internal;
    if ( !including->includes )
    {
        including->includes = copytarget( including );
        including->includes->original_target = including;
    }
    internal = including->includes;
    internal->depends = targetentry( internal->depends, included );
}


/*
 * enter_rule() - return pointer to RULE, creating it if necessary in
 * target_module.
 */

static RULE * enter_rule( char * rulename, module_t * target_module )
{
    RULE rule;
    RULE * r = &rule;

    r->name = rulename;

    if ( hashenter( demand_rules( target_module ), (HASHDATA * *)&r ) )
    {
        r->name = newstr( rulename );   /* never freed */
        r->procedure = (PARSE *)0;
        r->module = 0;
        r->actions = 0;
        r->arguments = 0;
        r->exported = 0;
        r->module = target_module;
#ifdef HAVE_PYTHON
        r->python_function = 0;
#endif
    }
    return r;
}


/*
 * define_rule() - return pointer to RULE, creating it if necessary in
 * target_module. Prepare it to accept a body or action originating in
 * src_module.
 */

static RULE * define_rule
(
    module_t * src_module,
    char     * rulename,
    module_t * target_module
)
{
    RULE * r = enter_rule( rulename, target_module );
    if ( r->module != src_module ) /* if the rule was imported from elsewhere, clear it now */
    {
        set_rule_body( r, 0, 0 );
        set_rule_actions( r, 0 );
        r->module = src_module; /* r will be executed in the source module */
    }
    return r;
}


void rule_free( RULE * r )
{
    freestr( r->name );
    r->name = "";
    parse_free( r->procedure );
    r->procedure = 0;
    if ( r->arguments )
        args_free( r->arguments );
    r->arguments = 0;
    if ( r->actions )
        actions_free( r->actions );
    r->actions = 0;
}


/*
 * bindtarget() - return pointer to TARGET, creating it if necessary.
 */

TARGET * bindtarget( char const * target_name )
{
    TARGET target;
    TARGET * t = &target;

    if ( !targethash )
        targethash = hashinit( sizeof( TARGET ), "targets" );

    /* Perforce added const everywhere. No time to merge that change. */
#ifdef NT
    target_name = short_path_to_long_path( (char *)target_name );
#endif
    t->name = (char *)target_name;

    if ( hashenter( targethash, (HASHDATA * *)&t ) )
    {
        memset( (char *)t, '\0', sizeof( *t ) );
        t->name = newstr( (char *)target_name );  /* never freed */
        t->boundname = t->name;  /* default for T_FLAG_NOTFILE */
    }

    return t;
}


static void bind_explicitly_located_target( void * xtarget, void * data )
{
    TARGET * t = (TARGET *)xtarget;
    if ( !( t->flags & T_FLAG_NOTFILE ) )
    {
        /* Check if there's a setting for LOCATE */
        SETTINGS * s = t->settings;
        for ( ; s ; s = s->next )
        {
            if ( strcmp( s->symbol, "LOCATE" ) == 0 )
            {
                pushsettings( t->settings );
                /* We are binding a target with explicit LOCATE. So third
                 * argument is of no use: nothing will be returned through it.
                 */
                t->boundname = search( t->name, &t->time, 0, 0 );
                popsettings( t->settings );
                break;
            }
        }
    }
}


void bind_explicitly_located_targets()
{
    if ( targethash )
        hashenumerate( targethash, bind_explicitly_located_target, (void *)0 );
}


/* TODO: It is probably not a good idea to use functions in other modules like
  this. */
void call_bind_rule( char * target, char * boundname );


TARGET * search_for_target ( char * name, LIST * search_path )
{
    PATHNAME f[1];
    string buf[1];
    LOCATED_TARGET lt;
    LOCATED_TARGET * lta = &lt;
    time_t time;
    int found = 0;
    TARGET * result;

    string_new( buf );

    path_parse( name, f );

    f->f_grist.ptr = 0;
    f->f_grist.len = 0;

    while ( search_path )
    {
        f->f_root.ptr = search_path->string;
        f->f_root.len = strlen( search_path->string );

        string_truncate( buf, 0 );
        path_build( f, buf, 1 );

        lt.file_name = buf->value ;

        if ( !located_targets )
            located_targets = hashinit( sizeof(LOCATED_TARGET),
                                        "located targets" );

        if ( hashcheck( located_targets, (HASHDATA * *)&lta ) )
        {
            return lta->target;
        }

        timestamp( buf->value, &time );
        if ( time )
        {
            found = 1;
            break;
        }

        search_path = list_next( search_path );
    }

    if ( !found )
    {
        f->f_root.ptr = 0;
        f->f_root.len = 0;

        string_truncate( buf, 0 );
        path_build( f, buf, 1 );

        timestamp( buf->value, &time );
    }

    result = bindtarget( name );
    result->boundname = newstr( buf->value );
    result->time = time;
    result->binding = time ? T_BIND_EXISTS : T_BIND_MISSING;

    call_bind_rule( result->name, result->boundname );

    string_free( buf );

    return result;
}


/*
 * copytarget() - make a new target with the old target's name.
 *
 * Not entered into hash table -- for internal nodes.
 */

TARGET * copytarget( const TARGET * ot )
{
    TARGET * t = (TARGET *)BJAM_MALLOC( sizeof( *t ) );
    memset( (char *)t, '\0', sizeof( *t ) );
    t->name = copystr( ot->name );
    t->boundname = t->name;

    t->flags |= T_FLAG_NOTFILE | T_FLAG_INTERNAL;

    return t;
}


/*
 * touch_target() - mark a target to simulate being new.
 */

void touch_target( char * t )
{
    bindtarget( t )->flags |= T_FLAG_TOUCHED;
}


/*
 * targetlist() - turn list of target names into a TARGET chain.
 *
 * Inputs:
 *  chain   existing TARGETS to append to
 *  targets list of target names
 */

TARGETS * targetlist( TARGETS * chain, LIST * target_names )
{
    for ( ; target_names; target_names = list_next( target_names ) )
        chain = targetentry( chain, bindtarget( target_names->string ) );
    return chain;
}


/*
 * targetentry() - add a TARGET to a chain of TARGETS.
 *
 * Inputs:
 *  chain   existing TARGETS to append to
 *  target  new target to append
 */

TARGETS * targetentry( TARGETS * chain, TARGET * target )
{
    TARGETS * c = (TARGETS *)BJAM_MALLOC( sizeof( TARGETS ) );
    c->target = target;

    if ( !chain ) chain = c;
    else chain->tail->next = c;
    chain->tail = c;
    c->next = 0;

    return chain;
}


/*
 * targetchain() - append two TARGET chains.
 *
 * Inputs:
 *  chain   exisitng TARGETS to append to
 *  target  new target to append
 */

TARGETS * targetchain( TARGETS * chain, TARGETS * targets )
{
    if ( !targets ) return chain;
    if ( !chain   ) return targets;

    chain->tail->next = targets;
    chain->tail = targets->tail;

    return chain;
}

/*
 * actionlist() - append to an ACTION chain.
 */

ACTIONS * actionlist( ACTIONS * chain, ACTION * action )
{
    ACTIONS * actions = (ACTIONS *)BJAM_MALLOC( sizeof( ACTIONS ) );

    actions->action = action;

    if ( !chain ) chain = actions;
    else chain->tail->next = actions;
    chain->tail = actions;
    actions->next = 0;

    return chain;
}

static SETTINGS * settings_freelist;


/*
 * addsettings() - add a deferred "set" command to a target.
 *
 * Adds a variable setting (varname=list) onto a chain of settings for a
 * particular target. 'flag' controls the relationship between new and old
 * values in the same way as in var_set() function (see variable.c). Returns
 * the head of the settings chain.
 */

SETTINGS * addsettings( SETTINGS * head, int flag, char * symbol, LIST * value )
{
    SETTINGS * v;

    /* Look for previous settings. */
    for ( v = head; v; v = v->next )
        if ( !strcmp( v->symbol, symbol ) )
            break;

    /* If not previously set, alloc a new. */
    /* If appending, do so. */
    /* Else free old and set new. */
    if ( !v )
    {
        v = settings_freelist;

        if ( v )
            settings_freelist = v->next;
        else
            v = (SETTINGS *)BJAM_MALLOC( sizeof( *v ) );

        v->symbol = newstr( symbol );
        v->value = value;
        v->next = head;
        head = v;
    }
    else if ( flag == VAR_APPEND )
    {
        v->value = list_append( v->value, value );
    }
    else if ( flag != VAR_DEFAULT )
    {
        list_free( v->value );
        v->value = value;
    }
    else
        list_free( value );

    /* Return (new) head of list. */
    return head;
}


/*
 * pushsettings() - set all target specific variables.
 */

void pushsettings( SETTINGS * v )
{
    for ( ; v; v = v->next )
        v->value = var_swap( v->symbol, v->value );
}


/*
 * popsettings() - reset target specific variables to their pre-push values.
 */

void popsettings( SETTINGS * v )
{
    pushsettings( v );  /* just swap again */
}


/*
 * copysettings() - duplicate a settings list, returning the new copy.
 */

SETTINGS * copysettings( SETTINGS * head )
{
    SETTINGS * copy = 0;
    SETTINGS * v;
    for ( v = head; v; v = v->next )
        copy = addsettings( copy, VAR_SET, v->symbol, list_copy( 0, v->value ) );
    return copy;
}


/*
 * freetargets() - delete a targets list.
 */

void freetargets( TARGETS * chain )
{
    while ( chain )
    {
        TARGETS * n = chain->next;
        BJAM_FREE( chain );
        chain = n;
    }
}


/*
 * freeactions() - delete an action list.
 */

void freeactions( ACTIONS * chain )
{
    while ( chain )
    {
        ACTIONS * n = chain->next;
        BJAM_FREE( chain );
        chain = n;
    }
}


/*
 * freesettings() - delete a settings list.
 */

void freesettings( SETTINGS * v )
{
    while ( v )
    {
        SETTINGS * n = v->next;
        freestr( v->symbol );
        list_free( v->value );
        v->next = settings_freelist;
        settings_freelist = v;
        v = n;
    }
}


static void freetarget( void * xt, void * data )
{
    TARGET * t = (TARGET *)xt;
    if ( t->settings ) freesettings( t->settings            );
    if ( t->depends  ) freetargets ( t->depends             );
    if ( t->includes ) freetarget  ( t->includes, (void *)0 );
    if ( t->actions  ) freeactions ( t->actions             );
}


/*
 * rules_done() - free RULE and TARGET tables.
 */

void rules_done()
{
    hashenumerate( targethash, freetarget, 0 );
    hashdone( targethash );
    while ( settings_freelist )
    {
        SETTINGS * n = settings_freelist->next;
        BJAM_FREE( settings_freelist );
        settings_freelist = n;
    }
}


/*
 * args_new() - make a new reference-counted argument list.
 */

argument_list * args_new()
{
    argument_list * r = (argument_list *)BJAM_MALLOC( sizeof(argument_list) );
    r->reference_count = 0;
    lol_init( r->data );
    return r;
}


/*
 * args_refer() - add a new reference to the given argument list.
 */

void args_refer( argument_list * a )
{
    ++a->reference_count;
}


/*
 * args_free() - release a reference to the given argument list.
 */

void args_free( argument_list * a )
{
    if ( --a->reference_count <= 0 )
    {
        lol_free( a->data );
        BJAM_FREE( a );
    }
}


/*
 * actions_refer() - add a new reference to the given actions.
 */

void actions_refer( rule_actions * a )
{
    ++a->reference_count;
}


/*
 * actions_free() - release a reference to the given actions.
 */

void actions_free( rule_actions * a )
{
    if ( --a->reference_count <= 0 )
    {
        freestr( a->command );
        list_free( a->bindlist );
        BJAM_FREE( a );
    }
}


/*
 * set_rule_body() - set the argument list and procedure of the given rule.
 */

static void set_rule_body( RULE * rule, argument_list * args, PARSE * procedure )
{
    if ( args )
        args_refer( args );
    if ( rule->arguments )
        args_free( rule->arguments );
    rule->arguments = args;

    if ( procedure )
        parse_refer( procedure );
    if ( rule->procedure )
        parse_free( rule->procedure );
    rule->procedure = procedure;
}


/*
 * global_name() - given a rule, return the name for a corresponding rule in the
 * global module.
 */

static char * global_rule_name( RULE * r )
{
    if ( r->module == root_module() )
        return r->name;

    {
        char name[4096] = "";
        strncat( name, r->module->name, sizeof( name ) - 1 );
        strncat( name, r->name, sizeof( name ) - 1 );
        return newstr( name);
    }
}


/*
 * global_rule() - given a rule, produce the corresponding entry in the global
 * module.
 */

static RULE * global_rule( RULE * r )
{
    if ( r->module == root_module() )
        return r;

    {
        char * name = global_rule_name( r );
        RULE * result = define_rule( r->module, name, root_module() );
        freestr( name );
        return result;
    }
}


/*
 * new_rule_body() - make a new rule named rulename in the given module, with
 * the given argument list and procedure. If exported is true, the rule is
 * exported to the global module as modulename.rulename.
 */

RULE * new_rule_body( module_t * m, char * rulename, argument_list * args, PARSE * procedure, int exported )
{
    RULE * local = define_rule( m, rulename, m );
    local->exported = exported;
    set_rule_body( local, args, procedure );

    /* Mark the procedure with the global rule name, regardless of whether the
     * rule is exported. That gives us something reasonably identifiable that we
     * can use, e.g. in profiling output. Only do this once, since this could be
     * called multiple times with the same procedure.
     */
    if ( procedure->rulename == 0 )
        procedure->rulename = global_rule_name( local );

    return local;
}


static void set_rule_actions( RULE * rule, rule_actions * actions )
{
    if ( actions )
        actions_refer( actions );
    if ( rule->actions )
        actions_free( rule->actions );
    rule->actions = actions;
}


static rule_actions * actions_new( char * command, LIST * bindlist, int flags )
{
    rule_actions * result = (rule_actions *)BJAM_MALLOC( sizeof( rule_actions ) );
    result->command = copystr( command );
    result->bindlist = bindlist;
    result->flags = flags;
    result->reference_count = 0;
    return result;
}


RULE * new_rule_actions( module_t * m, char * rulename, char * command, LIST * bindlist, int flags )
{
    RULE * local = define_rule( m, rulename, m );
    RULE * global = global_rule( local );
    set_rule_actions( local, actions_new( command, bindlist, flags ) );
    set_rule_actions( global, local->actions );
    return local;
}


/*
 * Looks for a rule in the specified module, and returns it, if found. First
 * checks if the rule is present in the module's rule table. Second, if name of
 * the rule is in the form name1.name2 and name1 is in the list of imported
 * modules, look in module 'name1' for rule 'name2'.
 */

RULE * lookup_rule( char * rulename, module_t * m, int local_only )
{
    RULE       rule;
    RULE     * r = &rule;
    RULE     * result = 0;
    module_t * original_module = m;

    r->name = rulename;

    if ( m->class_module )
        m = m->class_module;

    if ( m->rules && hashcheck( m->rules, (HASHDATA * *)&r ) )
        result = r;
    else if ( !local_only && m->imported_modules )
    {
        /* Try splitting the name into module and rule. */
        char *p = strchr( r->name, '.' ) ;
        if ( p )
        {
            *p = '\0';
            /* Now, r->name keeps the module name, and p+1 keeps the rule name.
             */
            if ( hashcheck( m->imported_modules, (HASHDATA * *)&r ) )
                result = lookup_rule( p + 1, bindmodule( rulename ), 1 );
            *p = '.';
        }
    }

    if ( result )
    {
        if ( local_only && !result->exported )
            result = 0;
        else
        {
            /* Lookup started in class module. We have found a rule in class
             * module, which is marked for execution in that module, or in some
             * instances. Mark it for execution in the instance where we started
             * the lookup.
             */
            int execute_in_class = ( result->module == m );
            int execute_in_some_instance = ( result->module->class_module &&
                ( result->module->class_module == m ) );
            if ( ( original_module != m ) &&
                ( execute_in_class || execute_in_some_instance ) )
                result->module = original_module;
        }
    }

    return result;
}


RULE * bindrule( char * rulename, module_t * m )
{
    RULE * result = lookup_rule( rulename, m, 0 );
    if ( !result )
        result = lookup_rule( rulename, root_module(), 0 );
    /* We have only one caller, 'evaluate_rule', which will complain about
     * calling an undefined rule. We could issue the error here, but we do not
     * have the necessary information, such as frame.
     */
    if ( !result )
        result = enter_rule( rulename, m );
    return result;
}


RULE * import_rule( RULE * source, module_t * m, char * name )
{
    RULE * dest = define_rule( source->module, name, m );
    set_rule_body( dest, source->arguments, source->procedure );
    set_rule_actions( dest, source->actions );
    return dest;
}
