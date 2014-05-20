/*
 * Copyright 1993, 2000 Christopher Seiwald.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

/*  This file is ALSO:
 *  Copyright 2001-2004 David Abrahams.
 *  Distributed under the Boost Software License, Version 1.0.
 *  (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt)
 */

# include "jam.h"

# include "lists.h"
# include "parse.h"
# include "compile.h"
# include "variable.h"
# include "expand.h"
# include "rules.h"
# include "newstr.h"
# include "make.h"
# include "search.h"
# include "hdrmacro.h"
# include "hash.h"
# include "modules.h"
# include "strings.h"
# include "builtins.h"
# include "class.h"

# include <assert.h>
# include <string.h>
# include <stdarg.h>

/*
 * compile.c - compile parsed jam statements
 *
 * External routines:
 *
 *  compile_append() - append list results of two statements
 *  compile_eval() - evaluate if to determine which leg to compile
 *  compile_foreach() - compile the "for x in y" statement
 *  compile_if() - compile 'if' rule
 *  compile_while() - compile 'while' rule
 *  compile_include() - support for 'include' - call include() on file
 *  compile_list() - expand and return a list
 *  compile_local() - declare (and set) local variables
 *  compile_null() - do nothing -- a stub for parsing
 *  compile_on() - run rule under influence of on-target variables
 *  compile_rule() - compile a single user defined rule
 *  compile_rules() - compile a chain of rules
 *  compile_set() - compile the "set variable" statement
 *  compile_setcomp() - support for `rule` - save parse tree
 *  compile_setexec() - support for `actions` - save execution string
 *  compile_settings() - compile the "on =" (set variable on exec) statement
 *  compile_switch() - compile 'switch' rule
 *
 * Internal routines:
 *
 *  debug_compile() - printf with indent to show rule expansion.
 *  evaluate_rule() - execute a rule invocation
 *
 *  builtin_depends() - DEPENDS/INCLUDES rule
 *  builtin_echo() - ECHO rule
 *  builtin_exit() - EXIT rule
 *  builtin_flags() - NOCARE, NOTFILE, TEMPORARY rule
 *
 * 02/03/94 (seiwald) - Changed trace output to read "setting" instead of
 *          the awkward sounding "settings".
 * 04/12/94 (seiwald) - Combined build_depends() with build_includes().
 * 04/12/94 (seiwald) - actionlist() now just appends a single action.
 * 04/13/94 (seiwald) - added shorthand L0 for null list pointer
 * 05/13/94 (seiwald) - include files are now bound as targets, and thus
 *          can make use of $(SEARCH)
 * 06/01/94 (seiwald) - new 'actions existing' does existing sources
 * 08/23/94 (seiwald) - Support for '+=' (append to variable)
 * 12/20/94 (seiwald) - NOTIME renamed NOTFILE.
 * 01/22/95 (seiwald) - Exit rule.
 * 02/02/95 (seiwald) - Always rule; LEAVES rule.
 * 02/14/95 (seiwald) - NoUpdate rule.
 * 09/11/00 (seiwald) - new evaluate_rule() for headers().
 * 09/11/00 (seiwald) - compile_xxx() now return LIST *.
 *          New compile_append() and compile_list() in
 *          support of building lists here, rather than
 *          in jamgram.yy.
 * 01/10/00 (seiwald) - built-ins split out to builtin.c.
 */

static void debug_compile( int which, char *s, FRAME* frame );
int glob( char *s, char *c );
/* Internal functions from builtins.c */
void backtrace( FRAME *frame );
void backtrace_line( FRAME *frame );
void print_source_line( PARSE* p );

struct frame * frame_before_python_call;

void frame_init( FRAME* frame )
{
    frame->prev = 0;
    frame->prev_user = 0;
    lol_init(frame->args);
    frame->module = root_module();
    frame->rulename = "module scope";
    frame->procedure = 0;
}


void frame_free( FRAME* frame )
{
    lol_free( frame->args );
}


/*
 * compile_append() - append list results of two statements
 *
 *  parse->left more compile_append() by left-recursion
 *  parse->right    single rule
 */

LIST * compile_append( PARSE * parse, FRAME * frame )
{
    /* Append right to left. */
    return list_append(
        parse_evaluate( parse->left, frame ),
        parse_evaluate( parse->right, frame ) );
}


/*
 * compile_eval() - evaluate if to determine which leg to compile
 *
 * Returns:
 *  list    if expression true - compile 'then' clause
 *  L0  if expression false - compile 'else' clause
 */

static int lcmp( LIST * t, LIST * s )
{
    int status = 0;

    while ( !status && ( t || s ) )
    {
        char *st = t ? t->string : "";
        char *ss = s ? s->string : "";

        status = strcmp( st, ss );

        t = t ? list_next( t ) : t;
        s = s ? list_next( s ) : s;
    }

    return status;
}

LIST * compile_eval( PARSE * parse, FRAME * frame )
{
    LIST * ll;
    LIST * lr;
    LIST * s;
    LIST * t;
    int status = 0;

    /* Short circuit lr eval for &&, ||, and 'in'. */

    ll = parse_evaluate( parse->left, frame );
    lr = 0;

    switch ( parse->num )
    {
        case EXPR_AND:
        case EXPR_IN : if ( ll ) goto eval; break;
        case EXPR_OR : if ( !ll ) goto eval; break;
        default: eval: lr = parse_evaluate( parse->right, frame );
    }

    /* Now eval. */
    switch ( parse->num )
    {
    case EXPR_NOT: if ( !ll      ) status = 1; break;
    case EXPR_AND: if ( ll && lr ) status = 1; break;
    case EXPR_OR : if ( ll || lr ) status = 1; break;

    case EXPR_IN:
        /* "a in b": make sure each of ll is equal to something in lr. */
        for ( t = ll; t; t = list_next( t ) )
        {
            for ( s = lr; s; s = list_next( s ) )
            if ( !strcmp( t->string, s->string ) )
                break;
            if ( !s ) break;
        }
        /* No more ll? Success. */
        if ( !t ) status = 1;
        break;

    case EXPR_EXISTS: if ( lcmp( ll, L0 ) != 0 ) status = 1; break;
    case EXPR_EQUALS: if ( lcmp( ll, lr ) == 0 ) status = 1; break;
    case EXPR_NOTEQ : if ( lcmp( ll, lr ) != 0 ) status = 1; break;
    case EXPR_LESS  : if ( lcmp( ll, lr ) < 0  ) status = 1; break;
    case EXPR_LESSEQ: if ( lcmp( ll, lr ) <= 0 ) status = 1; break;
    case EXPR_MORE  : if ( lcmp( ll, lr ) > 0  ) status = 1; break;
    case EXPR_MOREEQ: if ( lcmp( ll, lr ) >= 0 ) status = 1; break;
    }

    if ( DEBUG_IF )
    {
        debug_compile( 0, "if", frame );
        list_print( ll );
        printf( "(%d) ", status );
        list_print( lr );
        printf( "\n" );
    }

    /* Find something to return. */
    /* In odd circumstances (like "" = "") */
    /* we'll have to return a new string. */

    if ( !status ) t = 0;
    else if ( ll ) t = ll, ll = 0;
    else if ( lr ) t = lr, lr = 0;
    else t = list_new( L0, newstr( "1" ) );

    if ( ll ) list_free( ll );
    if ( lr ) list_free( lr );
    return t;
}


/*
 * compile_foreach() - compile the "for x in y" statement
 *
 * Compile_foreach() resets the given variable name to each specified
 * value, executing the commands enclosed in braces for each iteration.
 *
 *  parse->string   index variable
 *  parse->left variable values
 *  parse->right    rule to compile
 */

LIST * compile_foreach( PARSE * parse, FRAME * frame )
{
    LIST     * nv = parse_evaluate( parse->left, frame );
    LIST     * l;
    SETTINGS * s = 0;

    if ( parse->num )
    {
        s = addsettings( s, VAR_SET, parse->string, L0 );
        pushsettings( s );
    }

    /* Call var_set to reset $(parse->string) for each val. */

    for ( l = nv; l; l = list_next( l ) )
    {
        LIST * val = list_new( L0, copystr( l->string ) );
        var_set( parse->string, val, VAR_SET );
        list_free( parse_evaluate( parse->right, frame ) );
    }

    if ( parse->num )
    {
        popsettings( s );
        freesettings( s );
    }

    list_free( nv );

    return L0;
}

/*
 * compile_if() - compile 'if' rule
 *
 *  parse->left     condition tree
 *  parse->right        then tree
 *  parse->third        else tree
 */

LIST * compile_if( PARSE * p, FRAME * frame )
{
    LIST * l = parse_evaluate( p->left, frame );
    if ( l )
    {
        list_free( l );
        return parse_evaluate( p->right, frame );
    }
    return parse_evaluate( p->third, frame );
}


LIST * compile_while( PARSE * p, FRAME * frame )
{
    LIST * r = 0;
    LIST * l;
    while ( ( l = parse_evaluate( p->left, frame ) ) )
    {
        list_free( l );
        if ( r ) list_free( r );
        r = parse_evaluate( p->right, frame );
    }
    return r;
}


/*
 * compile_include() - support for 'include' - call include() on file
 *
 *  parse->left list of files to include (can only do 1)
 */

LIST * compile_include( PARSE * parse, FRAME * frame )
{
    LIST * nt = parse_evaluate( parse->left, frame );

    if ( DEBUG_COMPILE )
    {
        debug_compile( 0, "include", frame);
        list_print( nt );
        printf( "\n" );
    }

    if ( nt )
    {
        TARGET * t = bindtarget( nt->string );

        /* DWA 2001/10/22 - Perforce Jam cleared the arguments here, which
         * prevents an included file from being treated as part of the body of a
         * rule. I did not see any reason to do that, so I lifted the
         * restriction.
         */

        /* Bind the include file under the influence of */
        /* "on-target" variables.  Though they are targets, */
        /* include files are not built with make(). */

        pushsettings( t->settings );
        /* We don't expect that file to be included is generated by some
           action. Therefore, pass 0 as third argument.
           If the name resolves to directory, let it error out.  */
        t->boundname = search( t->name, &t->time, 0, 0 );
        popsettings( t->settings );

        parse_file( t->boundname, frame );
    }

    list_free( nt );

    return L0;
}

static LIST* evaluate_in_module ( char* module_name, PARSE * p, FRAME* frame)
{
    LIST* result;

    module_t* outer_module = frame->module;
    frame->module = module_name ? bindmodule( module_name ) : root_module();

    if ( outer_module != frame->module )
    {
        exit_module( outer_module );
        enter_module( frame->module );
    }

    result = parse_evaluate( p, frame );

    if ( outer_module != frame->module )
    {
        exit_module( frame->module );
        enter_module( outer_module );
        frame->module = outer_module;
    }

    return result;
}


LIST * compile_module( PARSE * p, FRAME * frame )
{
    /* Here we are entering a module declaration block. */
    LIST * module_name = parse_evaluate( p->left, frame );
    LIST * result = evaluate_in_module( module_name ? module_name->string : 0,
                                       p->right, frame );
    list_free( module_name );
    return result;
}


LIST * compile_class( PARSE * p, FRAME * frame )
{
    /** Todo: check for empty class name.
        Check for class redeclaration. */

    char * class_module = 0;

    LIST * name = parse_evaluate( p->left->right, frame );
    LIST * bases = 0;

    if ( p->left->left )
        bases = parse_evaluate( p->left->left->right, frame );

    class_module = make_class_module( name, bases, frame );
    evaluate_in_module( class_module, p->right, frame );

    return L0;
}


/*
 * compile_list() - expand and return a list.
 *
 *  parse->string - character string to expand.
 */

LIST * compile_list( PARSE * parse, FRAME * frame )
{
    /* s is a copyable string */
    char * s = parse->string;
    return var_expand( L0, s, s + strlen( s ), frame->args, 1 );
}


/*
 * compile_local() - declare (and set) local variables.
 *
 *  parse->left    list of variables
 *  parse->right   list of values
 *  parse->third   rules to execute
 */

LIST * compile_local( PARSE * parse, FRAME * frame )
{
    LIST * l;
    SETTINGS * s = 0;
    LIST     * nt = parse_evaluate( parse->left, frame );
    LIST     * ns = parse_evaluate( parse->right, frame );
    LIST     * result;

    if ( DEBUG_COMPILE )
    {
        debug_compile( 0, "local", frame );
        list_print( nt );
        printf( " = " );
        list_print( ns );
        printf( "\n" );
    }

    /* Initial value is ns. */
    for ( l = nt; l; l = list_next( l ) )
        s = addsettings( s, VAR_SET, l->string, list_copy( (LIST *)0, ns ) );

    list_free( ns );
    list_free( nt );

    /* Note that callees of the current context get this "local" variable,
     * making it not so much local as layered.
     */

    pushsettings( s );
    result = parse_evaluate( parse->third, frame );
    popsettings( s );

    freesettings( s );

    return result;
}


/*
 * compile_null() - do nothing -- a stub for parsing.
 */

LIST * compile_null( PARSE * parse, FRAME * frame )
{
    return L0;
}


/*
 * compile_on() - run rule under influence of on-target variables
 *
 *  parse->left    list of files to include (can only do 1).
 *  parse->right   rule to run.
 *
 * EXPERIMENTAL!
 */

LIST * compile_on( PARSE * parse, FRAME * frame )
{
    LIST * nt = parse_evaluate( parse->left, frame );
    LIST * result = 0;

    if ( DEBUG_COMPILE )
    {
        debug_compile( 0, "on", frame );
        list_print( nt );
        printf( "\n" );
    }

    if ( nt )
    {
        TARGET * t = bindtarget( nt->string );
        pushsettings( t->settings );
        result = parse_evaluate( parse->right, frame );
        popsettings( t->settings );
    }

    list_free( nt );

    return result;
}


/*
 * compile_rule() - compile a single user defined rule.
 *
 *  parse->string   name of user defined rule.
 *  parse->left     parameters (list of lists) to rule, recursing left.
 *
 * Wrapped around evaluate_rule() so that headers() can share it.
 */

LIST * compile_rule( PARSE * parse, FRAME * frame )
{
    FRAME   inner[ 1 ];
    LIST  * result;
    PARSE * p;

    /* Build up the list of arg lists. */
    frame_init( inner );
    inner->prev = frame;
    inner->prev_user = frame->module->user_module ? frame : frame->prev_user;
    inner->module = frame->module;  /* This gets fixed up in evaluate_rule(), below. */
    inner->procedure = parse;
    for ( p = parse->left; p; p = p->left )
        lol_add( inner->args, parse_evaluate( p->right, frame ) );

    /* And invoke the rule. */
    result = evaluate_rule( parse->string, inner );
    frame_free( inner );
    return result;
}


static void argument_error( char * message, RULE * rule, FRAME * frame, LIST* arg )
{
    LOL * actual = frame->args;
    assert( frame->procedure != 0 );
    backtrace_line( frame->prev );
    printf( "*** argument error\n* rule %s ( ", frame->rulename );
    lol_print( rule->arguments->data );
    printf( " )\n* called with: ( " );
    lol_print( actual );
    printf( " )\n* %s %s\n", message, arg ? arg->string : "" );
    print_source_line( rule->procedure );
    printf( "see definition of rule '%s' being called\n", rule->name );
    backtrace( frame->prev );
    exit( 1 );
}


/* Define delimiters for type check elements in argument lists (and return type
 * specifications, eventually).
 */
# define TYPE_OPEN_DELIM '['
# define TYPE_CLOSE_DELIM ']'

/*
 * is_type_name() - true iff the given string represents a type check
 * specification.
 */

static int is_type_name( char * s )
{
    return ( s[ 0 ] == TYPE_OPEN_DELIM ) &&
        ( s[ strlen( s ) - 1 ] == TYPE_CLOSE_DELIM );
}


/*
 * arg_modifier - if the next element of formal is a single character, return
 * that; return 0 otherwise. Used to extract "*+?" modifiers * from argument
 * lists.
 */

static char arg_modifier( LIST * formal )
{
    if ( formal->next )
    {
        char * next = formal->next->string;
        if ( next && ( next[ 0 ] != 0 ) && ( next[ 1 ] == 0 ) )
            return next[ 0 ];
    }
    return 0;
}


/*
 * type_check() - checks that each element of values satisfies the requirements
 * of type_name.
 *
 *      caller   - the frame of the rule calling the rule whose arguments are
 *                 being checked
 *
 *      called   - the rule being called
 *
 *      arg_name - a list element containing the name of the argument being
 *                 checked
 */

static void type_check
(
    char  * type_name,
    LIST  * values,
    FRAME * caller,
    RULE  * called,
    LIST  * arg_name
)
{
    static module_t * typecheck = 0;

    /* If nothing to check, bail now. */
    if ( !values || !type_name )
        return;

    if ( !typecheck )
        typecheck = bindmodule( ".typecheck" );

    /* If the checking rule can not be found, also bail. */
    {
        RULE checker_, *checker = &checker_;

        checker->name = type_name;
        if ( !typecheck->rules || !hashcheck( typecheck->rules, (HASHDATA * *)&checker ) )
            return;
    }

    exit_module( caller->module );

    while ( values != 0 )
    {
        LIST *error;
        FRAME frame[1];
        frame_init( frame );
        frame->module = typecheck;
        frame->prev = caller;
        frame->prev_user = caller->module->user_module ? caller : caller->prev_user;

        enter_module( typecheck );
        /* Prepare the argument list */
        lol_add( frame->args, list_new( L0, values->string ) );
        error = evaluate_rule( type_name, frame );

        exit_module( typecheck );

        if ( error )
            argument_error( error->string, called, caller, arg_name );

        frame_free( frame );
        values = values->next;
    }

    enter_module( caller->module );
}

/*
 * collect_arguments() - local argument checking and collection
 */
static SETTINGS *
collect_arguments( RULE* rule, FRAME* frame )
{
    SETTINGS *locals = 0;

    LOL * all_actual = frame->args;
    LOL * all_formal = rule->arguments ? rule->arguments->data : 0;
    if ( all_formal ) /* Nothing to set; nothing to check */
    {
        int max = all_formal->count > all_actual->count
            ? all_formal->count
            : all_actual->count;

        int n;
        for ( n = 0; n < max ; ++n )
        {
            LIST *actual = lol_get( all_actual, n );
            char *type_name = 0;

            LIST *formal;
            for ( formal = lol_get( all_formal, n ); formal; formal = formal->next )
            {
                char* name = formal->string;

                if ( is_type_name(name) )
                {
                    if ( type_name )
                        argument_error( "missing argument name before type name:", rule, frame, formal );

                    if ( !formal->next )
                        argument_error( "missing argument name after type name:", rule, frame, formal );

                    type_name = formal->string;
                }
                else
                {
                    LIST* value = 0;
                    char modifier;
                    LIST* arg_name = formal; /* hold the argument name for type checking */

                    /* Stop now if a variable number of arguments are specified */
                    if ( name[0] == '*' && name[1] == 0 )
                        return locals;

                    modifier = arg_modifier( formal );

                    if ( !actual && modifier != '?' && modifier != '*' )
                        argument_error( "missing argument", rule, frame, formal );

                    switch ( modifier )
                    {
                    case '+':
                    case '*':
                        value = list_copy( 0, actual );
                        actual = 0;
                        /* skip an extra element for the modifier */
                        formal = formal->next;
                        break;
                    case '?':
                        /* skip an extra element for the modifier */
                        formal = formal->next;
                        /* fall through */
                    default:
                        if ( actual ) /* in case actual is missing */
                        {
                            value = list_new( 0, actual->string );
                            actual = actual->next;
                        }
                    }

                    locals = addsettings( locals, VAR_SET, name, value );
                    type_check( type_name, value, frame, rule, arg_name );
                    type_name = 0;
                }
            }

            if ( actual )
            {
                argument_error( "extra argument", rule, frame, actual );
            }
        }
    }
    return locals;
}

RULE *
enter_rule( char *rulename, module_t *target_module );

#ifdef HAVE_PYTHON

static int python_instance_number = 0;

static LIST*
call_python_function(RULE* r, FRAME* frame)
{
    LIST * result = 0;
    PyObject * arguments = PyTuple_New( frame->args->count );
    int i ;
    PyObject * py_result;

    for ( i = 0; i < frame->args->count; ++i )
    {
        PyObject * arg = PyList_New(0);
        LIST* l = lol_get( frame->args, i);

        for ( ; l; l = l->next )
        {
            PyObject * v = PyString_FromString(l->string);
            /* Steals reference to 'v' */
            PyList_Append( arg, v );
        }
        /* Steals reference to 'arg' */
        PyTuple_SetItem( arguments, i, arg );
    }

    frame_before_python_call = frame;
    py_result = PyObject_CallObject( r->python_function, arguments );
    Py_DECREF( arguments );
    if ( py_result != NULL )
    {
        if ( PyList_Check( py_result ) )
        {
            int size = PyList_Size( py_result );
            int i;
            for ( i = 0; i < size; ++i )
            {
                PyObject * item = PyList_GetItem( py_result, i );
                if ( PyString_Check( item ) )
                {
                    result = list_new( result,
                                      newstr( PyString_AsString( item ) ) );
                }
                else
                {
                    fprintf( stderr, "Non-string object returned by Python call.\n" );
                }
            }
        }
        else if ( PyInstance_Check( py_result ) )
        {
            static char instance_name[1000];
            static char imported_method_name[1000];
            module_t * m;
            PyObject * method;
            PyObject * method_name = PyString_FromString("foo");
            RULE * r;

            fprintf(stderr, "Got instance!\n");

            snprintf(instance_name, 1000,
                     "pyinstance%d", python_instance_number);
            snprintf(imported_method_name, 1000,
                     "pyinstance%d.foo", python_instance_number);
            ++python_instance_number;

            m = bindmodule(instance_name);

            /* This is expected to get bound method. */
            method = PyObject_GetAttr(py_result, method_name);

            r = bindrule( imported_method_name, root_module() );

            r->python_function = method;

            result = list_new(0, newstr(instance_name));

            Py_DECREF( method_name );
        }
        else if ( py_result == Py_None )
        {
            result = L0;
        }
        else
        {
            fprintf(stderr, "Non-list object returned by Python call\n");
        }

        Py_DECREF( py_result );
    }
    else
    {
        PyErr_Print();
        fprintf(stderr,"Call failed\n");
    }

    return result;
}


module_t * python_module()
{
    static module_t * python = 0;
    if ( !python )
        python = bindmodule("__python__");
    return python;
}

#endif


/*
 * evaluate_rule() - execute a rule invocation.
 */

LIST *
evaluate_rule(
    char  * rulename,
    FRAME * frame )
{
    LIST          * result = L0;
    RULE          * rule;
    profile_frame   prof[1];
    module_t      * prev_module = frame->module;

    LIST * l;
    {
        LOL arg_context_, * arg_context = &arg_context_;
        if ( !frame->prev )
            lol_init(arg_context);
        else
            arg_context = frame->prev->args;
        l = var_expand( L0, rulename, rulename+strlen(rulename), arg_context, 0 );
    }

    if ( !l )
    {
        backtrace_line( frame->prev );
        printf( "warning: rulename %s expands to empty string\n", rulename );
        backtrace( frame->prev );
        return result;
    }

    rulename = l->string;
    rule = bindrule( l->string, frame->module );

#ifdef HAVE_PYTHON
    if ( rule->python_function )
    {
        /* The below messing with modules is due to the way modules are
         * implemented in Jam. Suppose we are in module M1 now. The global
         * variable map actually holds 'M1' variables, and M1->variables hold
         * global variables.
         *
         * If we call Python right away, Python calls back Jam and then Jam
         * does 'module M1 { }' then Jam will try to swap the current global
         * variables with M1->variables. The result will be that global
         * variables map will hold global variables, and any variable settings
         * we do will go to the global module, not M1.
         *
         * By restoring basic state, where the global variable map holds global
         * variable, we make sure any future 'module M1' entry will work OK.
         */

        LIST * result;
        module_t * m = python_module();

        frame->module = m;

        exit_module( prev_module );
        enter_module( m );

        result = call_python_function( rule, frame );

        exit_module( m );
        enter_module ( prev_module );

        return result;
    }
#endif

    /* Drop the rule name. */
    l = list_pop_front( l );

    /* Tack the rest of the expansion onto the front of the first argument. */
    frame->args->list[0] = list_append( l, lol_get( frame->args, 0 ) );

    if ( DEBUG_COMPILE )
    {
        /* Try hard to indicate in which module the rule is going to execute. */
        if ( rule->module != frame->module
             && rule->procedure != 0 && strcmp( rulename, rule->procedure->rulename ) )
        {
            char buf[256] = "";
            strncat( buf, rule->module->name, sizeof( buf ) - 1 );
            strncat( buf, rule->name, sizeof( buf ) - 1 );
            debug_compile( 1, buf, frame );
        }
        else
        {
            debug_compile( 1, rulename, frame );
        }

        lol_print( frame->args );
        printf( "\n" );
    }

    if ( rule->procedure && rule->module != prev_module )
    {
        /* Propagate current module to nested rule invocations. */
        frame->module = rule->module;

        /* Swap variables. */
        exit_module( prev_module );
        enter_module( rule->module );
    }

    /* Record current rule name in frame. */
    if ( rule->procedure )
    {
        frame->rulename = rulename;
        /* And enter record profile info. */
        if ( DEBUG_PROFILE )
            profile_enter( rule->procedure->rulename, prof );
    }

    /* Check traditional targets $(<) and sources $(>). */
    if ( !rule->actions && !rule->procedure )
    {
        backtrace_line( frame->prev );
        printf( "rule %s unknown in module %s\n", rule->name, frame->module->name );
        backtrace( frame->prev );
        exit( 1 );
    }

    /* If this rule will be executed for updating the targets then construct the
     * action for make().
     */
    if ( rule->actions )
    {
        TARGETS * t;
        ACTION  * action;

        /* The action is associated with this instance of this rule. */
        action = (ACTION *)BJAM_MALLOC( sizeof( ACTION ) );
        memset( (char *)action, '\0', sizeof( *action ) );

        action->rule = rule;
        action->targets = targetlist( (TARGETS *)0, lol_get( frame->args, 0 ) );
        action->sources = targetlist( (TARGETS *)0, lol_get( frame->args, 1 ) );

        /* If we have a group of targets all being built using the same action
         * then we must not allow any of them to be used as sources unless they
         * had all already been built in the first place or their joined action
         * has had a chance to finish its work and build all of them anew.
         *
         * Without this it might be possible, in case of a multi-process build,
         * for their action, triggered by buiding one of the targets, to still
         * be running when another target in the group reports as done in order
         * to avoid triggering the same action again and gets used prematurely.
         *
         * As a quick-fix to achieve this effect we make all the targets list
         * each other as 'included targets'. More precisely, we mark the first
         * listed target as including all the other targets in the list and vice
         * versa. This makes anyone depending on any of those targets implicitly
         * depend on all of them, thus making sure none of those targets can be
         * used as sources until all of them have been built. Note that direct
         * dependencies could not have been used due to the 'circular
         * dependency' issue.
         *
         * TODO: Although the current implementation solves the problem of one
         * of the targets getting used before its action completes its work it
         * also forces the action to run whenever any of the targets in the
         * group is not up to date even though some of them might not actually
         * be used by the targets being built. We should see how we can
         * correctly recognize such cases and use that to avoid running the
         * action if possible and not rebuild targets not actually depending on
         * targets that are not up to date.
         *
         * TODO: Using the 'include' feature might have side-effects due to
         * interaction with the actual 'inclusion scanning' system. This should
         * be checked.
         */
        if ( action->targets )
        {
            TARGET * t0 = action->targets->target;
            for ( t = action->targets->next; t; t = t->next )
            {
                target_include( t->target, t0 );
                target_include( t0, t->target );
            }
        }

        /* Append this action to the actions of each target. */
        for ( t = action->targets; t; t = t->next )
            t->target->actions = actionlist( t->target->actions, action );
    }

    /* Now recursively compile any parse tree associated with this rule.
     * parse_refer()/parse_free() call pair added to ensure rule not freed
     * during use.
     */
    if ( rule->procedure )
    {
        SETTINGS * local_args = collect_arguments( rule, frame );
        PARSE * parse = rule->procedure;
        parse_refer( parse );

        pushsettings( local_args );
        result = parse_evaluate( parse, frame );
        popsettings( local_args );
        freesettings( local_args );

        parse_free( parse );
    }

    if ( frame->module != prev_module )
    {
        exit_module( frame->module );
        enter_module( prev_module );
    }

    if ( DEBUG_PROFILE && rule->procedure )
        profile_exit( prof );

    if ( DEBUG_COMPILE )
        debug_compile( -1, 0, frame);

    return result;
}


/*
 * Call the given rule with the specified parameters. The parameters should be
 * of type LIST* and end with a NULL pointer. This differs from 'evaluate_rule'
 * in that frame for the called rule is prepared inside 'call_rule'.
 *
 * This function is useful when a builtin rule (in C) wants to call another rule
 * which might be implemented in Jam.
 */

LIST * call_rule( char * rulename, FRAME * caller_frame, ... )
{
    va_list va;
    LIST * result;

    FRAME       inner[1];
    frame_init( inner );
    inner->prev = caller_frame;
    inner->prev_user = caller_frame->module->user_module ?
        caller_frame : caller_frame->prev_user;
    inner->module = caller_frame->module;
    inner->procedure = 0;

    va_start( va, caller_frame );
    for ( ; ; )
    {
        LIST * l = va_arg( va, LIST* );
        if ( !l )
            break;
        lol_add( inner->args, l );
    }
    va_end( va );

    result = evaluate_rule( rulename, inner );

    frame_free( inner );

    return result;
}


/*
 * compile_rules() - compile a chain of rules
 *
 *  parse->left single rule
 *  parse->right    more compile_rules() by right-recursion
 */

LIST * compile_rules( PARSE * parse, FRAME * frame )
{
    /* Ignore result from first statement; return the 2nd. */
    /* Optimize recursion on the right by looping. */
    do list_free( parse_evaluate( parse->left, frame ) );
    while ( ( parse = parse->right )->func == compile_rules );
    return parse_evaluate( parse, frame );
}


/*
 * assign_var_mode() - convert ASSIGN_XXX compilation flag into corresponding
 *                     VAR_XXX variable set flag.
 */

static int assign_var_mode( int parsenum, char const * * tracetext )
{
    char const * trace;
    int          setflag;
    switch ( parsenum )
    {
        case ASSIGN_SET    : setflag = VAR_SET    ; trace = "=" ; break;
        case ASSIGN_APPEND : setflag = VAR_APPEND ; trace = "+="; break;
        case ASSIGN_DEFAULT: setflag = VAR_DEFAULT; trace = "?="; break;
        default:             setflag = VAR_SET    ; trace = ""  ; break;
    }
    if ( tracetext )
        *tracetext = trace ;
    return setflag;
}

/*
 * compile_set() - compile the "set variable" statement
 *
 *  parse->left variable names
 *  parse->right    variable values
 *  parse->num  ASSIGN_SET/APPEND/DEFAULT
 */

LIST * compile_set( PARSE * parse, FRAME * frame )
{
    LIST       * nt = parse_evaluate( parse->left, frame );
    LIST       * ns = parse_evaluate( parse->right, frame );
    LIST       * l;
    char const * trace;
    int          setflag = assign_var_mode( parse->num, &trace );

    if ( DEBUG_COMPILE )
    {
        debug_compile( 0, "set", frame );
        list_print( nt );
        printf( " %s ", trace );
        list_print( ns );
        printf( "\n" );
    }

    /* Call var_set to set variable. var_set keeps ns, so need to copy it. */
    for ( l = nt; l; l = list_next( l ) )
        var_set( l->string, list_copy( L0, ns ), setflag );
    list_free( nt );
    return ns;
}


/*
 * compile_setcomp() - support for `rule` - save parse tree.
 *
 *  parse->string  rule name
 *  parse->left    rules for rule
 *  parse->right   optional list-of-lists describing arguments
 */

LIST * compile_setcomp( PARSE * parse, FRAME * frame )
{
    argument_list * arg_list = 0;

    /* Create new LOL describing argument requirements if supplied. */
    if ( parse->right )
    {
        PARSE * p;
        arg_list = args_new();
        for ( p = parse->right; p; p = p->left )
            lol_add( arg_list->data, parse_evaluate( p->right, frame ) );
    }

    new_rule_body( frame->module, parse->string, arg_list, parse->left, !parse->num );
    return L0;
}


/*
 * compile_setexec() - support for `actions` - save execution string.
 *
 *  parse->string   rule name
 *  parse->string1  OS command string
 *  parse->num      flags
 *  parse->left     `bind` variables
 *
 * Note that the parse flags (as defined in compile.h) are transferred directly
 * to the rule flags (as defined in rules.h).
 */

LIST * compile_setexec( PARSE * parse, FRAME * frame )
{
    LIST * bindlist = parse_evaluate( parse->left, frame );
    new_rule_actions( frame->module, parse->string, parse->string1, bindlist, parse->num );
    return L0;
}


/*
 * compile_settings() - compile the "on =" (set variable on exec) statement.
 *
 *  parse->left   variable names
 *  parse->right  target name
 *  parse->third  variable value
 *  parse->num    ASSIGN_SET/APPEND
 */

LIST * compile_settings( PARSE * parse, FRAME * frame )
{
    LIST       * nt = parse_evaluate( parse->left, frame );
    LIST       * ns = parse_evaluate( parse->third, frame );
    LIST       * targets = parse_evaluate( parse->right, frame );
    LIST       * ts;
    char const * trace;
    int          setflag = assign_var_mode( parse->num, &trace );

    if ( DEBUG_COMPILE )
    {
        debug_compile( 0, "set", frame );
        list_print( nt );
        printf( " on " );
        list_print( targets );
        printf( " %s ", trace );
        list_print( ns );
        printf( "\n" );
    }

    /* Call addsettings() to save variable setting. addsettings() keeps ns, so
     * need to copy it. Pass append flag to addsettings().
     */
    for ( ts = targets; ts; ts = list_next( ts ) )
    {
        TARGET * t = bindtarget( ts->string );
        LIST   * l;

        for ( l = nt; l; l = list_next( l ) )
        t->settings = addsettings( t->settings, setflag, l->string,
            list_copy( (LIST *)0, ns ) );
    }

    list_free( nt );
    list_free( targets );
    return ns;
}


/*
 * compile_switch() - compile 'switch' rule.
 *
 *  parse->left   switch value (only 1st used)
 *  parse->right  cases
 *
 *  cases->left   1st case
 *  cases->right  next cases
 *
 *  case->string  argument to match
 *  case->left    parse tree to execute
 */

LIST * compile_switch( PARSE * parse, FRAME * frame )
{
    LIST * nt = parse_evaluate( parse->left, frame );
    LIST * result = 0;

    if ( DEBUG_COMPILE )
    {
        debug_compile( 0, "switch", frame );
        list_print( nt );
        printf( "\n" );
    }

    /* Step through cases. */
    for ( parse = parse->right; parse; parse = parse->right )
    {
        if ( !glob( parse->left->string, nt ? nt->string : "" ) )
        {
            /* Get & exec parse tree for this case. */
            parse = parse->left->left;
            result = parse_evaluate( parse, frame );
            break;
        }
    }

    list_free( nt );
    return result;
}


/*
 * debug_compile() - printf with indent to show rule expansion.
 */

static void debug_compile( int which, char * s, FRAME * frame )
{
    static int level = 0;
    static char indent[36] = ">>>>|>>>>|>>>>|>>>>|>>>>|>>>>|>>>>|";

    if ( which >= 0 )
    {
        int i;

        print_source_line( frame->procedure );

        i = ( level + 1 ) * 2;
        while ( i > 35 )
        {
            fputs( indent, stdout );
            i -= 35;
        }

        printf( "%*.*s ", i, i, indent );
    }

    if ( s )
        printf( "%s ", s );

    level += which;
}
