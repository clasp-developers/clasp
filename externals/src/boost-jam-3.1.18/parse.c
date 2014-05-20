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

#include "jam.h"
#include "lists.h"
#include "parse.h"
#include "scan.h"
#include "newstr.h"
#include "modules.h"
#include "frames.h"

/*
 * parse.c - make and destroy parse trees as driven by the parser
 *
 * 09/07/00 (seiwald) - ref count on PARSE to avoid freeing when used,
 *      as per Matt Armstrong.
 * 09/11/00 (seiwald) - structure reworked to reflect that (*func)()
 *      returns a LIST *.
 */

static PARSE * yypsave;

void parse_file( char * f, FRAME * frame )
{
    /* Suspend scan of current file and push this new file in the stream. */
    yyfparse( f );

    /* Now parse each block of rules and execute it. Execute it outside of the
     * parser so that recursive calls to yyrun() work (no recursive yyparse's).
     */

    for ( ; ; )
    {
        PARSE * p;

        /* Filled by yyparse() calling parse_save(). */
        yypsave = 0;

        /* If parse error or empty parse, outta here. */
        if ( yyparse() || !( p = yypsave ) )
            break;

        /* Run the parse tree. */
        parse_evaluate( p, frame );
        parse_free( p );
    }
}


void parse_save( PARSE * p )
{
    yypsave = p;
}


PARSE * parse_make(
    LIST  * (* func)( PARSE *, FRAME * ),
    PARSE * left,
    PARSE * right,
    PARSE * third,
    char  * string,
    char  * string1,
    int     num )
{
    PARSE * p = (PARSE *)BJAM_MALLOC( sizeof( PARSE ) );

    p->func = func;
    p->left = left;
    p->right = right;
    p->third = third;
    p->string = string;
    p->string1 = string1;
    p->num = num;
    p->refs = 1;
    p->rulename = 0;

    if ( left )
    {
        p->file = left->file;
        p->line = left->line;
    }
    else
    {
        yyinput_stream( &p->file, &p->line );
    }

    return p;
}


void parse_refer( PARSE * p )
{
    ++p->refs;
}


void parse_free( PARSE * p )
{
    if ( --p->refs )
        return;

    if ( p->string )
        freestr( p->string );
    if ( p->string1 )
        freestr( p->string1 );
    if ( p->left )
        parse_free( p->left );
    if ( p->right )
        parse_free( p->right );
    if ( p->third )
        parse_free( p->third );
    if ( p->rulename )
        freestr( p->rulename );

    BJAM_FREE( (char *)p );
}


LIST * parse_evaluate( PARSE * p, FRAME * frame )
{
    frame->procedure = p;
    return (*p->func)( p, frame );
}
