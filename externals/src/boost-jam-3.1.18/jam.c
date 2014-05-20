/*
 * /+\
 * +\   Copyright 1993-2002 Christopher Seiwald and Perforce Software, Inc.
 * \+/
 *
 * This file is part of jam.
 *
 * License is hereby granted to use this software and distribute it
 * freely, as long as this copyright notice is retained and modifications
 * are clearly marked.
 *
 * ALL WARRANTIES ARE HEREBY DISCLAIMED.
 */

/*  This file is ALSO:
 *  Copyright 2001-2004 David Abrahams.
 *  Distributed under the Boost Software License, Version 1.0.
 *  (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt)
 */

/*
 * jam.c - make redux
 *
 * See Jam.html for usage information.
 *
 * These comments document the code.
 *
 * The top half of the code is structured such:
 *
 *                       jam
 *                      / | \
 *                 +---+  |  \
 *                /       |   \
 *         jamgram     option  \
 *        /  |   \              \
 *       /   |    \              \
 *      /    |     \             |
 *  scan     |     compile      make
 *   |       |    /  | \       / |  \
 *   |       |   /   |  \     /  |   \
 *   |       |  /    |   \   /   |    \
 * jambase parse     |   rules  search make1
 *                   |           |      |   \
 *                   |           |      |    \
 *                   |           |      |     \
 *               builtins    timestamp command execute
 *                               |
 *                               |
 *                               |
 *                             filesys
 *
 *
 * The support routines are called by all of the above, but themselves
 * are layered thus:
 *
 *                     variable|expand
 *                      /  |   |   |
 *                     /   |   |   |
 *                    /    |   |   |
 *                 lists   |   |   pathsys
 *                    \    |   |
 *                     \   |   |
 *                      \  |   |
 *                     newstr  |
 *                        \    |
 *                         \   |
 *                          \  |
 *                          hash
 *
 * Roughly, the modules are:
 *
 *  builtins.c - jam's built-in rules
 *  command.c - maintain lists of commands
 *  compile.c - compile parsed jam statements
 *  execunix.c - execute a shell script on UNIX
 *  execvms.c - execute a shell script, ala VMS
 *  expand.c - expand a buffer, given variable values
 *  file*.c - scan directories and archives on *
 *  hash.c - simple in-memory hashing routines
 *  hdrmacro.c - handle header file parsing for filename macro definitions
 *  headers.c - handle #includes in source files
 *  jambase.c - compilable copy of Jambase
 *  jamgram.y - jam grammar
 *  lists.c - maintain lists of strings
 *  make.c - bring a target up to date, once rules are in place
 *  make1.c - execute command to bring targets up to date
 *  newstr.c - string manipulation routines
 *  option.c - command line option processing
 *  parse.c - make and destroy parse trees as driven by the parser
 *  path*.c - manipulate file names on *
 *  hash.c - simple in-memory hashing routines
 *  regexp.c - Henry Spencer's regexp
 *  rules.c - access to RULEs, TARGETs, and ACTIONs
 *  scan.c - the jam yacc scanner
 *  search.c - find a target along $(SEARCH) or $(LOCATE)
 *  timestamp.c - get the timestamp of a file or archive member
 *  variable.c - handle jam multi-element variables
 *
 * 05/04/94 (seiwald) - async multiprocess (-j) support
 * 02/08/95 (seiwald) - -n implies -d2.
 * 02/22/95 (seiwald) - -v for version info.
 * 09/11/00 (seiwald) - PATCHLEVEL folded into VERSION.
 * 01/10/01 (seiwald) - pathsys.h split from filesys.h
 */


#include "jam.h"
#include "option.h"
#include "patchlevel.h"

/* These get various function declarations. */
#include "lists.h"
#include "parse.h"
#include "variable.h"
#include "compile.h"
#include "builtins.h"
#include "rules.h"
#include "newstr.h"
#include "scan.h"
#include "timestamp.h"
#include "make.h"
#include "strings.h"
#include "expand.h"
#include "filesys.h"
#include "output.h"

/* Macintosh is "special" */
#ifdef OS_MAC
    #include <QuickDraw.h>
#endif

/* And UNIX for this. */
#ifdef unix
    #include <sys/utsname.h>
    #include <signal.h>
#endif

struct globs globs =
{
    0,          /* noexec */
    1,          /* jobs */
    0,          /* quitquick */
    0,          /* newestfirst */
    0,          /* pipes action stdout and stderr merged to action output */
#ifdef OS_MAC
    { 0, 0 },   /* debug - suppress tracing output */
#else
    { 0, 1 },   /* debug ... */
#endif
    0,          /* output commands, not run them */
    0           /* action timeout */
};

/* Symbols to be defined as true for use in Jambase. */
static char * othersyms[] = { OSMAJOR, OSMINOR, OSPLAT, JAMVERSYM, 0 };


/* Known for sure:
 *  mac needs arg_enviro
 *  OS2 needs extern environ
 */

#ifdef OS_MAC
    #define use_environ arg_environ
    #ifdef MPW
        QDGlobals qd;
    #endif
#endif

/* on Win32-LCC */
#if defined( OS_NT ) && defined( __LCC__ )
    #define use_environ _environ
#endif

# if defined( __MWERKS__)
    #define use_environ _environ
    extern char * * _environ;
#endif

#ifndef use_environ
    #define use_environ environ
    #if !defined( __WATCOM__ ) && !defined( OS_OS2 ) && !defined( OS_NT )
        extern char **environ;
    #endif
#endif

#if YYDEBUG != 0
    extern int yydebug;
#endif

#ifndef NDEBUG
static void run_unit_tests()
{
#if defined( USE_EXECNT )
    extern void execnt_unit_test();
    execnt_unit_test();
#endif
    string_unit_test();
    var_expand_unit_test();
}
#endif

int anyhow = 0;

#ifdef HAVE_PYTHON
    extern PyObject * bjam_call         ( PyObject * self, PyObject * args );
    extern PyObject * bjam_import_rule  ( PyObject * self, PyObject * args );
    extern PyObject * bjam_define_action( PyObject * self, PyObject * args );
    extern PyObject * bjam_variable     ( PyObject * self, PyObject * args );
    extern PyObject * bjam_backtrace    ( PyObject * self, PyObject * args );
#endif

int main( int argc, char * * argv, char * * arg_environ )
{
    int                     n;
    char                  * s;
    struct option           optv[N_OPTS];
    char            const * all = "all";
    int                     status;
    int                     arg_c = argc;
    char          *       * arg_v = argv;
    char            const * progname = argv[0];

    BJAM_MEM_INIT();

# ifdef OS_MAC
    InitGraf(&qd.thePort);
# endif

    --argc;
    ++argv;

    if ( getoptions( argc, argv, "-:l:d:j:p:f:gs:t:ano:qv", optv ) < 0 )
    {
        printf( "\nusage: %s [ options ] targets...\n\n", progname );

        printf( "-a      Build all targets, even if they are current.\n" );
        printf( "-dx     Set the debug level to x (0-9).\n" );
        printf( "-fx     Read x instead of Jambase.\n" );
        /* printf( "-g      Build from newest sources first.\n" ); */
        printf( "-jx     Run up to x shell commands concurrently.\n" );
        printf( "-lx     Limit actions to x number of seconds after which they are stopped.\n" );
        printf( "-n      Don't actually execute the updating actions.\n" );
        printf( "-ox     Write the updating actions to file x.\n" );
        printf( "-px     x=0, pipes action stdout and stderr merged into action output.\n" );
        printf( "-q      Quit quickly as soon as a target fails.\n" );
        printf( "-sx=y   Set variable x=y, overriding environment.\n" );
        printf( "-tx     Rebuild x, even if it is up-to-date.\n" );
        printf( "-v      Print the version of jam and exit.\n" );
        printf( "--x     Option is ignored.\n\n" );

        exit( EXITBAD );
    }

    /* Version info. */
    if ( ( s = getoptval( optv, 'v', 0 ) ) )
    {
        printf( "Boost.Jam  " );
        printf( "Version %s. %s.\n", VERSION, OSMINOR );
        printf( "   Copyright 1993-2002 Christopher Seiwald and Perforce Software, Inc.  \n" );
        printf( "   Copyright 2001 David Turner.\n" );
        printf( "   Copyright 2001-2004 David Abrahams.\n" );
        printf( "   Copyright 2002-2008 Rene Rivera.\n" );
        printf( "   Copyright 2003-2008 Vladimir Prus.\n" );

        return EXITOK;
    }

    /* Pick up interesting options. */
    if ( ( s = getoptval( optv, 'n', 0 ) ) )
        globs.noexec++, globs.debug[2] = 1;

    if ( ( s = getoptval( optv, 'p', 0 ) ) )
    {
        /* Undocumented -p3 (acts like both -p1 -p2) means separate pipe action
         * stdout and stderr.
         */
        globs.pipe_action = atoi( s );
        if ( ( 3 < globs.pipe_action ) || ( globs.pipe_action < 0 ) )
        {
            printf(
                "Invalid pipe descriptor '%d', valid values are -p[0..3].\n",
                globs.pipe_action );
            exit( EXITBAD );
        }
    }

    if ( ( s = getoptval( optv, 'q', 0 ) ) )
        globs.quitquick = 1;

    if ( ( s = getoptval( optv, 'a', 0 ) ) )
        anyhow++;

    if ( ( s = getoptval( optv, 'j', 0 ) ) )
        globs.jobs = atoi( s );

    if ( ( s = getoptval( optv, 'g', 0 ) ) )
        globs.newestfirst = 1;

    if ( ( s = getoptval( optv, 'l', 0 ) ) )
        globs.timeout = atoi( s );

    /* Turn on/off debugging */
    for ( n = 0; ( s = getoptval( optv, 'd', n ) ); ++n )
    {
        int i;

        /* First -d, turn off defaults. */
        if ( !n )
            for ( i = 0; i < DEBUG_MAX; ++i )
                globs.debug[i] = 0;

        i = atoi( s );

        if ( ( i < 0 ) || ( i >= DEBUG_MAX ) )
        {
            printf( "Invalid debug level '%s'.\n", s );
            continue;
        }

        /* n turns on levels 1-n. */
        /* +n turns on level n. */
        if ( *s == '+' )
            globs.debug[i] = 1;
        else while ( i )
            globs.debug[i--] = 1;
    }

    {
        PROFILE_ENTER( MAIN );

#ifdef HAVE_PYTHON
        {
            PROFILE_ENTER( MAIN_PYTHON );
            Py_Initialize();
            {
                static PyMethodDef BjamMethods[] = {
                    {"call", bjam_call, METH_VARARGS,
                     "Call the specified bjam rule."},
                    {"import_rule", bjam_import_rule, METH_VARARGS,
                     "Imports Python callable to bjam."},
                    {"define_action", bjam_define_action, METH_VARARGS,
                     "Defines a command line action."},
                    {"variable", bjam_variable, METH_VARARGS,
                     "Obtains a variable from bjam's global module."},
                    {"backtrace", bjam_backtrace, METH_VARARGS,
                     "Returns bjam backtrace from the last call into Python."},
                    {NULL, NULL, 0, NULL}
                };

                Py_InitModule( "bjam", BjamMethods );
            }
            PROFILE_EXIT( MAIN_PYTHON );
        }
#endif

#ifndef NDEBUG
        run_unit_tests();
#endif
#if YYDEBUG != 0
        if ( DEBUG_PARSE )
            yydebug = 1;
#endif

        /* Set JAMDATE. */
        var_set( "JAMDATE", list_new( L0, outf_time(time(0)) ), VAR_SET );

        /* Set JAM_VERSION. */
        var_set( "JAM_VERSION",
                 list_new( list_new( list_new( L0,
                   newstr( VERSION_MAJOR_SYM ) ),
                   newstr( VERSION_MINOR_SYM ) ),
                   newstr( VERSION_PATCH_SYM ) ),
                   VAR_SET );

        /* Set JAMUNAME. */
#ifdef unix
        {
            struct utsname u;

            if ( uname( &u ) >= 0 )
            {
                var_set( "JAMUNAME",
                         list_new(
                             list_new(
                                 list_new(
                                     list_new(
                                         list_new( L0,
                                            newstr( u.sysname ) ),
                                         newstr( u.nodename ) ),
                                     newstr( u.release ) ),
                                 newstr( u.version ) ),
                             newstr( u.machine ) ), VAR_SET );
            }
        }
#endif /* unix */

        /* Load up environment variables. */

        /* First into the global module, with splitting, for backward
         * compatibility.
         */
        var_defines( use_environ, 1 );

        /* Then into .ENVIRON, without splitting. */
        enter_module( bindmodule(".ENVIRON") );
        var_defines( use_environ, 0 );
        exit_module( bindmodule(".ENVIRON") );

        /*
         * Jam defined variables OS & OSPLAT. We load them after environment, so
         * that setting OS in environment does not change Jam's notion of the
         * current platform.
         */
        var_defines( othersyms, 1 );

        /* Load up variables set on command line. */
        for ( n = 0; ( s = getoptval( optv, 's', n ) ); ++n )
        {
            char *symv[2];
            symv[ 0 ] = s;
            symv[ 1 ] = 0;
            var_defines( symv, 1 );
            enter_module( bindmodule(".ENVIRON") );
            var_defines( symv, 0 );
            exit_module( bindmodule(".ENVIRON") );
        }

        /* Set the ARGV to reflect the complete list of arguments of invocation.
         */
        for ( n = 0; n < arg_c; ++n )
            var_set( "ARGV", list_new( L0, newstr( arg_v[n] ) ), VAR_APPEND );

        /* Initialize built-in rules. */
        load_builtins();

        /* Add the targets in the command line to the update list. */
        for ( n = 1; n < arg_c; ++n )
        {
            if ( arg_v[ n ][ 0 ] == '-' )
            {
                char * f = "-:l:d:j:f:gs:t:ano:qv";
                for ( ; *f; ++f ) if ( *f == arg_v[ n ][ 1 ] ) break;
                if ( ( f[ 1 ] == ':' ) && ( arg_v[ n ][ 2 ] == '\0' ) ) ++n;
            }
            else
            {
                mark_target_for_updating( arg_v[ n ] );
            }
        }

        if (!targets_to_update())
            mark_target_for_updating("all");

        /* Parse ruleset. */
        {
            FRAME frame[ 1 ];
            frame_init( frame );
            for ( n = 0; ( s = getoptval( optv, 'f', n ) ); ++n )
                parse_file( s, frame );

            if ( !n )
                parse_file( "+", frame );
        }

        status = yyanyerrors();

        /* Manually touch -t targets. */
        for ( n = 0; ( s = getoptval( optv, 't', n ) ); ++n )
            touch_target( s );

        /* If an output file is specified, set globs.cmdout to that. */
        if ( ( s = getoptval( optv, 'o', 0 ) ) )
        {
            if ( !( globs.cmdout = fopen( s, "w" ) ) )
            {
                printf( "Failed to write to '%s'\n", s );
                exit( EXITBAD );
            }
            ++globs.noexec;
        }

        /* The build system may set the PARALLELISM variable to override -j
           options.  */
        {
            LIST *p = L0;
            p = var_get ("PARALLELISM");
            if (p)
            {
                int j = atoi (p->string);
                if (j == -1)
                {
                    printf( "Invalid value of PARALLELISM: %s\n", p->string);
                }
                else
                {
                    globs.jobs = j;
                }
            }
        }

        /* KEEP_GOING overrides -q option. */
        {
            LIST *p = L0;
            p = var_get ("KEEP_GOING");
            if (p)
            {
                int v = atoi (p->string);
                if (v == 0)
                    globs.quitquick = 1;
                else
                    globs.quitquick = 0;
            }
        }

        /* Now make target. */
        {
            PROFILE_ENTER( MAIN_MAKE );

            LIST * targets = targets_to_update();
            if (targets)
            {
                int targets_count = list_length( targets );
                const char * * targets2 = (const char * *)
                    BJAM_MALLOC( targets_count * sizeof( char * ) );
                int n = 0;
                for ( ; targets; targets = list_next( targets ) )
                    targets2[ n++ ] = targets->string;
                status |= make( targets_count, targets2, anyhow );
                free( targets );
            }

            PROFILE_EXIT( MAIN_MAKE );
        }

        PROFILE_EXIT( MAIN );
    }

    if ( DEBUG_PROFILE )
        profile_dump();

    /* Widely scattered cleanup. */
    var_done();
    file_done();
    rules_done();
    stamps_done();
    str_done();

    /* Close cmdout. */
    if ( globs.cmdout )
        fclose( globs.cmdout );

#ifdef HAVE_PYTHON
    Py_Finalize();
#endif

    BJAM_MEM_CLOSE();

    return status ? EXITBAD : EXITOK;
}
