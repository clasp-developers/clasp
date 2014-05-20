/*
 * Copyright 1993, 1995 Christopher Seiwald.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

# include "jam.h"
# include "newstr.h"
# include "lists.h"

/*
 * lists.c - maintain lists of strings
 *
 * This implementation essentially uses a singly linked list, but
 * guarantees that the head element of every list has a valid pointer
 * to the tail of the list, so the new elements can efficiently and
 * properly be appended to the end of a list.
 *
 * To avoid massive allocation, list_free() just tacks the whole freed
 * chain onto freelist and list_new() looks on freelist first for an
 * available list struct.  list_free() does not free the strings in the
 * chain: it lazily lets list_new() do so.
 *
 * 08/23/94 (seiwald) - new list_append()
 * 09/07/00 (seiwald) - documented lol_*() functions
 */

static LIST *freelist = 0;  /* junkpile for list_free() */

/*
 * list_append() - append a list onto another one, returning total
 */

LIST * list_append( LIST * l, LIST * nl )
{
    if ( !nl )
    {
        /* Just return l */
    }
    else if ( !l )
    {
        l = nl;
    }
    else
    {
        /* Graft two non-empty lists. */
        l->tail->next = nl;
        l->tail = nl->tail;
    }

    return l;
}

/*
 * list_new() - tack a string onto the end of a list of strings
 */

LIST * list_new( LIST * head, char * string )
{
    LIST * l;

    if ( DEBUG_LISTS )
        printf( "list > %s <\n", string );

    /* Get list struct from freelist, if one available.  */
    /* Otherwise allocate. */
    /* If from freelist, must free string first */

    if ( freelist )
    {
        l = freelist;
        freestr( l->string );
        freelist = freelist->next;
    }
    else
    {
        l = (LIST *)BJAM_MALLOC( sizeof( LIST ) );
    }

    /* If first on chain, head points here. */
    /* If adding to chain, tack us on. */
    /* Tail must point to this new, last element. */

    if ( !head ) head = l;
    else head->tail->next = l;
    head->tail = l;
    l->next = 0;

    l->string = string;

    return head;
}


/*
 * list_copy() - copy a whole list of strings (nl) onto end of another (l).
 */

LIST * list_copy( LIST * l, LIST * nl )
{
    for ( ; nl; nl = list_next( nl ) )
        l = list_new( l, copystr( nl->string ) );
    return l;
}


/*
 * list_sublist() - copy a subset of a list of strings.
 */

LIST * list_sublist( LIST * l, int start, int count )
{
    LIST * nl = 0;
    for ( ; l && start--; l = list_next( l ) );
    for ( ; l && count--; l = list_next( l ) )
        nl = list_new( nl, copystr( l->string ) );
    return nl;
}


static int str_ptr_compare( void const * va, void const * vb )
{
    char * a = *( (char * *)va );
    char * b = *( (char * *)vb );
    return strcmp(a, b);
}


LIST * list_sort( LIST * l )
{
    int len;
    int ii;
    char * * strings;
    LIST * listp;
    LIST * result = 0;

    if ( !l )
        return L0;

    len = list_length( l );
    strings = (char * *)BJAM_MALLOC( len * sizeof(char*) );

    listp = l;
    for ( ii = 0; ii < len; ++ii )
    {
        strings[ ii ] = listp->string;
        listp = listp->next;
    }

    qsort( strings, len, sizeof( char * ), str_ptr_compare );

    for ( ii = 0; ii < len; ++ii )
        result = list_append( result, list_new( 0, strings[ ii ] ) );

    BJAM_FREE( strings );

    return result;
}


/*
 * list_free() - free a list of strings
 */

void list_free( LIST * head )
{
    /* Just tack onto freelist. */
    if ( head )
    {
        head->tail->next = freelist;
        freelist = head;
    }
}


/*
 * list_pop_front() - remove the front element from a list of strings
 */

LIST * list_pop_front( LIST * l )
{
    LIST * result = l->next;
    if ( result )
    {
        result->tail = l->tail;
        l->next = L0;
        l->tail = l;
    }
    list_free( l );
    return result;
}


/*
 * list_print() - print a list of strings to stdout
 */

void list_print( LIST * l )
{
    LIST * p = 0;
    for ( ; l; p = l, l = list_next( l ) )
        if ( p )
            printf( "%s ", p->string );
    if ( p )
        printf( "%s", p->string );
}


/*
 * list_length() - return the number of items in the list
 */

int list_length( LIST * l )
{
    int n = 0;
    for ( ; l; l = list_next( l ), ++n );
    return n;
}


int list_in( LIST * l, char * value )
{
    for ( ; l; l = l->next )
        if ( strcmp( l->string, value ) == 0 )
            return 1;
    return 0;
}


LIST * list_unique( LIST * sorted_list )
{
    LIST * result = 0;
    LIST * last_added = 0;

    for ( ; sorted_list; sorted_list = sorted_list->next )
    {
        if ( !last_added || strcmp( sorted_list->string, last_added->string ) != 0 )
        {
            result = list_new( result, sorted_list->string );
            last_added = sorted_list;
        }
    }
    return result;
}


/*
 * lol_init() - initialize a LOL (list of lists).
 */

void lol_init( LOL * lol )
{
    lol->count = 0;
}


/*
 * lol_add() - append a LIST onto an LOL.
 */

void lol_add( LOL * lol, LIST * l )
{
    if ( lol->count < LOL_MAX )
        lol->list[ lol->count++ ] = l;
}


/*
 * lol_free() - free the LOL and its LISTs.
 */

void lol_free( LOL * lol )
{
    int i;
    for ( i = 0; i < lol->count; ++i )
        list_free( lol->list[ i ] );
    lol->count = 0;
}


/*
 * lol_get() - return one of the LISTs in the LOL.
 */

LIST * lol_get( LOL * lol, int i )
{
    return i < lol->count ? lol->list[ i ] : 0;
}


/*
 * lol_print() - debug print LISTS separated by ":".
 */

void lol_print( LOL * lol )
{
    int i;

    for ( i = 0; i < lol->count; ++i )
    {
        if ( i )
            printf( " : " );
        list_print( lol->list[ i ] );
    }
}
