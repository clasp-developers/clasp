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
# include "rules.h"
# include "variable.h"
# include "regexp.h"
# include "hdrmacro.h"
# include "hash.h"
# include "newstr.h"
# include "strings.h"

/*
 * hdrmacro.c - handle header files that define macros used in
 *              #include statements.
 *
 *  we look for lines like "#define MACRO  <....>" or '#define MACRO  "    "'
 *  in the target file. When found, we
 *
 *  we then phony up a rule invocation like:
 *
 *  $(HDRRULE) <target> : <resolved included files> ;
 *
 * External routines:
 *    headers1() - scan a target for "#include MACRO" lines and try
 *                 to resolve them when needed
 *
 * Internal routines:
 *    headers1() - using regexp, scan a file and build include LIST
 *
 * 04/13/94 (seiwald) - added shorthand L0 for null list pointer
 * 09/10/00 (seiwald) - replaced call to compile_rule with evaluate_rule,
 *      so that headers() doesn't have to mock up a parse structure
 *      just to invoke a rule.
 */

/* this type is used to store a dictionary of file header macros */
typedef struct header_macro
{
  char * symbol;
  char * filename;  /* we could maybe use a LIST here ?? */
} HEADER_MACRO;

static struct hash * header_macros_hash = 0;


/*
 * headers() - scan a target for include files and call HDRRULE
 */

# define MAXINC 10

void
macro_headers( TARGET *t )
{
    static regexp *re = 0;
    FILE    *f;
    char    buf[ 1024 ];

    if ( DEBUG_HEADER )
        printf( "macro header scan for %s\n", t->name );

    /* this regexp is used to detect lines of the form       */
    /* "#define  MACRO  <....>" or "#define  MACRO  "....."  */
    /* in the header macro files..                           */
    if ( re == 0 )
    {
        re = regex_compile(
            "^[     ]*#[    ]*define[   ]*([A-Za-z][A-Za-z0-9_]*)[  ]*"
            "[<\"]([^\">]*)[\">].*$" );
    }

    if ( !( f = fopen( t->boundname, "r" ) ) )
        return;

    while ( fgets( buf, sizeof( buf ), f ) )
    {
        HEADER_MACRO var;
        HEADER_MACRO *v = &var;

        if ( regexec( re, buf ) && re->startp[1] )
        {
            /* we detected a line that looks like "#define  MACRO  filename */
            re->endp[1][0] = '\0';
            re->endp[2][0] = '\0';

            if ( DEBUG_HEADER )
                printf( "macro '%s' used to define filename '%s' in '%s'\n",
                        re->startp[1], re->startp[2], t->boundname );

            /* add macro definition to hash table */
            if ( !header_macros_hash )
                header_macros_hash = hashinit( sizeof( HEADER_MACRO ), "hdrmacros" );

            v->symbol   = re->startp[1];
            v->filename = 0;
            if ( hashenter( header_macros_hash, (HASHDATA **)&v ) )
            {
                v->symbol   = newstr( re->startp[1] );  /* never freed */
                v->filename = newstr( re->startp[2] );  /* never freed */
            }
            /* XXXX: FOR NOW, WE IGNORE MULTIPLE MACRO DEFINITIONS !! */
            /*       WE MIGHT AS WELL USE A LIST TO STORE THEM..      */
        }
    }

    fclose( f );
}


char * macro_header_get( const char * macro_name )
{
    HEADER_MACRO var;
    HEADER_MACRO * v = &var;

    v->symbol = (char* )macro_name;

    if ( header_macros_hash && hashcheck( header_macros_hash, (HASHDATA **)&v ) )
    {
        if ( DEBUG_HEADER )
            printf( "### macro '%s' evaluated to '%s'\n", macro_name, v->filename );
        return v->filename;
    }
    return 0;
}
