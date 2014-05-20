/*
 * Copyright 1993-2002 Christopher Seiwald and Perforce Software, Inc.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

/*  This file is ALSO:
 *  Copyright 2001-2004 David Abrahams.
 *  Distributed under the Boost Software License, Version 1.0.
 *  (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt)
 */

#include "jam.h"
#include "lists.h"
#include "search.h"
#include "timestamp.h"
#include "pathsys.h"
#include "variable.h"
#include "newstr.h"
#include "compile.h"
#include "strings.h"
#include "hash.h"
#include "filesys.h"
#include <string.h>


typedef struct _binding
{
    char * binding;
    char * target;
} BINDING;

static struct hash *explicit_bindings = 0;


void call_bind_rule
(
    char * target_,
    char * boundname_
)
{
    LIST * bind_rule = var_get( "BINDRULE" );
    if ( bind_rule )
    {
        /* No guarantee that the target is an allocated string, so be on the
         * safe side.
         */
        char * target = copystr( target_ );

        /* Likewise, do not rely on implementation details of newstr.c: allocate
         * a copy of boundname.
         */
        char * boundname = copystr( boundname_ );
        if ( boundname && target )
        {
            /* Prepare the argument list. */
            FRAME frame[1];
            frame_init( frame );

            /* First argument is the target name. */
            lol_add( frame->args, list_new( L0, target ) );

            lol_add( frame->args, list_new( L0, boundname ) );
            if ( lol_get( frame->args, 1 ) )
                evaluate_rule( bind_rule->string, frame );

            /* Clean up */
            frame_free( frame );
        }
        else
        {
            if ( boundname )
                freestr( boundname );
            if ( target )
                freestr( target );
        }
    }
}

/*
 * search.c - find a target along $(SEARCH) or $(LOCATE)
 * First, check if LOCATE is set. If so, use it to determine
 * the location of target and return, regardless of whether anything
 * exists on that location.
 *
 * Second, examine all directories in SEARCH. If there's file already
 * or there's another target with the same name which was placed
 * to this location via LOCATE setting, stop and return the location.
 * In case of previous target, return it's name via the third argument.
 *
 * This bevahiour allow to handle dependency on generated files. If
 * caller does not expect that target is generated, 0 can be passed as
 * the third argument.
 */

char *
search(
    char *target,
    time_t *time,
    char **another_target,
    int file
)
{
    PATHNAME f[1];
    LIST    *varlist;
    string    buf[1];
    int     found = 0;
    /* Will be set to 1 if target location is specified via LOCATE. */
    int     explicitly_located = 0;
    char    *boundname = 0;

    if ( another_target )
        *another_target = 0;

    if (! explicit_bindings )
        explicit_bindings = hashinit( sizeof(BINDING),
                                     "explicitly specified locations");

    string_new( buf );
    /* Parse the filename */

    path_parse( target, f );

    f->f_grist.ptr = 0;
    f->f_grist.len = 0;

    if ( ( varlist = var_get( "LOCATE" ) ) )
      {
        f->f_root.ptr = varlist->string;
        f->f_root.len = strlen( varlist->string );

        path_build( f, buf, 1 );

        if ( DEBUG_SEARCH )
            printf( "locate %s: %s\n", target, buf->value );

        explicitly_located = 1;

        timestamp( buf->value, time );
        found = 1;
    }
    else if ( ( varlist = var_get( "SEARCH" ) ) )
    {
        while ( varlist )
        {
            BINDING b, *ba = &b;
            file_info_t *ff;

            f->f_root.ptr = varlist->string;
            f->f_root.len = strlen( varlist->string );

            string_truncate( buf, 0 );
            path_build( f, buf, 1 );

            if ( DEBUG_SEARCH )
                printf( "search %s: %s\n", target, buf->value );

            ff = file_query(buf->value);
            timestamp( buf->value, time );

            b.binding = buf->value;

            if ( hashcheck( explicit_bindings, (HASHDATA**)&ba ) )
            {
                if ( DEBUG_SEARCH )
                    printf(" search %s: found explicitly located target %s\n",
                           target, ba->target);
                if ( another_target )
                    *another_target = ba->target;
                found = 1;
                break;
            }
            else if ( ff && ff->time )
            {
                if ( !file || ff->is_file )
                {
                    found = 1;
                    break;
                }
            }

            varlist = list_next( varlist );
        }
    }

    if ( !found )
    {
        /* Look for the obvious */
        /* This is a questionable move.  Should we look in the */
        /* obvious place if SEARCH is set? */

        f->f_root.ptr = 0;
        f->f_root.len = 0;

        string_truncate( buf, 0 );
        path_build( f, buf, 1 );

        if ( DEBUG_SEARCH )
            printf( "search %s: %s\n", target, buf->value );

        timestamp( buf->value, time );
    }

    boundname = newstr( buf->value );
    string_free( buf );

    if ( explicitly_located )
    {
        BINDING b;
        BINDING * ba = &b;
        b.binding = boundname;
        b.target = target;
        /* CONSIDER: we probably should issue a warning is another file
           is explicitly bound to the same location. This might break
           compatibility, though. */
        hashenter( explicit_bindings, (HASHDATA * *)&ba );
    }

    /* prepare a call to BINDRULE if the variable is set */
    call_bind_rule( target, boundname );

    return boundname;
}
