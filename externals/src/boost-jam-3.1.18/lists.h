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
 * lists.h - the LIST structure and routines to manipulate them
 *
 * The whole of jam relies on lists of strings as a datatype.  This
 * module, in conjunction with newstr.c, handles these relatively
 * efficiently.
 *
 * Structures defined:
 *
 *  LIST - list of strings
 *  LOL - list of LISTs
 *
 * External routines:
 *
 *  list_append() - append a list onto another one, returning total
 *  list_new() - tack a string onto the end of a list of strings
 *  list_copy() - copy a whole list of strings
 *  list_sublist() - copy a subset of a list of strings
 *  list_free() - free a list of strings
 *  list_print() - print a list of strings to stdout
 *  list_length() - return the number of items in the list
 *
 *  lol_init() - initialize a LOL (list of lists)
 *  lol_add() - append a LIST onto an LOL
 *  lol_free() - free the LOL and its LISTs
 *  lol_get() - return one of the LISTs in the LOL
 *  lol_print() - debug print LISTS separated by ":"
 *
 * 04/13/94 (seiwald) - added shorthand L0 for null list pointer
 * 08/23/94 (seiwald) - new list_append()
 */

#ifndef LISTS_DWA20011022_H
# define LISTS_DWA20011022_H

/*
 * LIST - list of strings
 */

typedef struct _list LIST;

struct _list {
    LIST    *next;
    LIST    *tail;      /* only valid in head node */
    char    *string;    /* private copy */
};

/*
 * LOL - list of LISTs
 */

typedef struct _lol LOL;

# define LOL_MAX 19

struct _lol {
    int count;
    LIST    *list[ LOL_MAX ];
};

LIST *  list_append( LIST *l, LIST *nl );
LIST *  list_copy( LIST *l, LIST  *nl );
void    list_free( LIST *head );
LIST *  list_new( LIST *head, char *string );
void    list_print( LIST *l );
int list_length( LIST *l );
LIST *  list_sublist( LIST *l, int start, int count );
LIST *  list_pop_front( LIST *l );
LIST *  list_sort( LIST *l);
LIST *  list_unique( LIST *sorted_list);
int     list_in(LIST* l, char* value);

# define list_next( l ) ((l)->next)

# define L0 ((LIST *)0)

void    lol_add( LOL *lol, LIST *l );
void    lol_init( LOL *lol );
void    lol_free( LOL *lol );
LIST *  lol_get( LOL *lol, int i );
void    lol_print( LOL *lol );
void    lol_build( LOL* lol, char** elements );

#endif

