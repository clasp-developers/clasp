/*
 * Copyright 1993, 1995 Christopher Seiwald.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

/*  This file is ALSO:
 *  Copyright 2001-2004 David Abrahams.
 *  Distributed under the Boost Software License, Version 1.0.
 *  (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt)
 */

/*
 * command.c - maintain lists of commands
 */

#include "jam.h"

#include "lists.h"
#include "parse.h"
#include "variable.h"
#include "rules.h"

#include "command.h"
#include <limits.h>
#include <string.h>


/*
 * cmd_new() - return a new CMD or 0 if too many args
 */

CMD * cmd_new( RULE * rule, LIST * targets, LIST * sources, LIST * shell )
{
    CMD * cmd = (CMD *)BJAM_MALLOC( sizeof( CMD ) );
    /* Lift line-length limitation entirely when JAMSHELL is just "%". */
    int no_limit = ( shell && !strcmp(shell->string,"%") && !list_next(shell) );
    int max_line = MAXLINE;
    int allocated = -1;

    cmd->rule = rule;
    cmd->shell = shell;
    cmd->next = 0;

    lol_init( &cmd->args );
    lol_add( &cmd->args, targets );
    lol_add( &cmd->args, sources );
    cmd->buf = 0;

    do
    {
        BJAM_FREE( cmd->buf );  /* free any buffer from previous iteration */

        cmd->buf = (char*)BJAM_MALLOC_ATOMIC( max_line + 1 );

        if ( cmd->buf == 0 )
            break;

        allocated = var_string( rule->actions->command, cmd->buf, max_line, &cmd->args );

        max_line = max_line * 2;
    }
    while ( ( allocated < 0 ) && ( max_line < INT_MAX / 2 ) );

    if ( !no_limit )
    {
        /* Bail if the result will not fit in MAXLINE. */
        char * s = cmd->buf;
        while ( *s )
        {
            size_t l = strcspn( s, "\n" );

            if ( l > MAXLINE )
            {
                /* We do not free targets/sources/shell if bailing. */
                cmd_free( cmd );
                return 0;
            }

            s += l;
            if ( *s )
                ++s;
        }
    }

    return cmd;
}


/*
 * cmd_free() - free a CMD
 */

void cmd_free( CMD * cmd )
{
    lol_free( &cmd->args );
    list_free( cmd->shell );
    BJAM_FREE( cmd->buf );
    BJAM_FREE( (char *)cmd );
}
