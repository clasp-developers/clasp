/*
 * Copyright 1993, 2000 Christopher Seiwald.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

/*  This file is ALSO:
 *  Copyright 2001-2004 David Abrahams.
 *  Copyright 2005 Reece H. Dunn.
 *  Copyright 2005 Rene Rivera.
 *  Distributed under the Boost Software License, Version 1.0.
 *  (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt)
 */

#include "jam.h"
#include "lists.h"
#include "parse.h"
#include "variable.h"
#include "expand.h"
#include "hash.h"
#include "filesys.h"
#include "newstr.h"
#include "strings.h"
#include "pathsys.h"
#include <stdlib.h>
#include <stdio.h>

/*
 * variable.c - handle Jam multi-element variables.
 *
 * External routines:
 *
 *  var_defines() - load a bunch of variable=value settings.
 *  var_string()  - expand a string with variables in it.
 *  var_get()     - get value of a user defined symbol.
 *  var_set()     - set a variable in jam's user defined symbol table.
 *  var_swap()    - swap a variable's value with the given one.
 *  var_done()    - free variable tables.
 *
 * Internal routines:
 *
 *  var_enter() - make new var symbol table entry, returning var ptr.
 *  var_dump()  - dump a variable to stdout.
 *
 * 04/13/94 (seiwald) - added shorthand L0 for null list pointer
 * 08/23/94 (seiwald) - Support for '+=' (append to variable)
 * 01/22/95 (seiwald) - split environment variables at blanks or :'s
 * 05/10/95 (seiwald) - split path variables at SPLITPATH (not :)
 * 09/11/00 (seiwald) - defunct var_list() removed
 */

static struct hash *varhash = 0;

/*
 * VARIABLE - a user defined multi-value variable
 */

typedef struct _variable VARIABLE ;

struct _variable
{
    char * symbol;
    LIST * value;
};

static VARIABLE * var_enter( char * symbol );
static void var_dump( char * symbol, LIST * value, char * what );


/*
 * var_hash_swap() - swap all variable settings with those passed
 *
 * Used to implement separate settings spaces for modules
 */

void var_hash_swap( struct hash * * new_vars )
{
    struct hash * old = varhash;
    varhash = *new_vars;
    *new_vars = old;
}


/*
 * var_defines() - load a bunch of variable=value settings
 *
 * If preprocess is false, take the value verbatim.
 *
 * Otherwise, if the variable value is enclosed in quotes, strip the
 * quotes.
 *
 * Otherwise, if variable name ends in PATH, split value at :'s.
 *
 * Otherwise, split the value at blanks.
 */

void var_defines( char * const * e, int preprocess )
{
    string buf[1];

    string_new( buf );

    for ( ; *e; ++e )
    {
        char * val;

# ifdef OS_MAC
        /* On the mac (MPW), the var=val is actually var\0val */
        /* Think different. */

        if ( ( val = strchr( *e, '=' ) ) || ( val = *e + strlen( *e ) ) )
# else
        if ( ( val = strchr( *e, '=' ) ) )
# endif
        {
            LIST * l = L0;
            char * pp;
            char * p;
# ifdef OPT_NO_EXTERNAL_VARIABLE_SPLIT
            char split = '\0';
# else
    # ifdef OS_MAC
            char split = ',';
    # else
            char split = ' ';
    # endif
# endif
            size_t len = strlen( val + 1 );

            int quoted = ( val[1] == '"' ) && ( val[len] == '"' ) &&
                ( len > 1 );

            if ( quoted && preprocess )
            {
                string_append_range( buf, val + 2, val + len );
                l = list_new( l, newstr( buf->value ) );
                string_truncate( buf, 0 );
            }
            else
            {
                /* Split *PATH at :'s, not spaces. */
                if ( val - 4 >= *e )
                {
                    if ( !strncmp( val - 4, "PATH", 4 ) ||
                        !strncmp( val - 4, "Path", 4 ) ||
                        !strncmp( val - 4, "path", 4 ) )
                        split = SPLITPATH;
                }

                /* Do the split. */
                for
                (
                    pp = val + 1;
                    preprocess && ( ( p = strchr( pp, split ) ) != 0 );
                    pp = p + 1
                )
                {
                    string_append_range( buf, pp, p );
                    l = list_new( l, newstr( buf->value ) );
                    string_truncate( buf, 0 );
                }

                l = list_new( l, newstr( pp ) );
            }

            /* Get name. */
            string_append_range( buf, *e, val );
            var_set( buf->value, l, VAR_SET );
            string_truncate( buf, 0 );
        }
    }
    string_free( buf );
}


/*
 * var_string() - expand a string with variables in it
 *
 * Copies in to out; doesn't modify targets & sources.
 */

int var_string( char * in, char * out, int outsize, LOL * lol )
{
    char * out0 = out;
    char * oute = out + outsize - 1;

    while ( *in )
    {
        char * lastword;
        int    dollar = 0;

        /* Copy white space. */
        while ( isspace( *in ) )
        {
            if ( out >= oute )
                return -1;
            *out++ = *in++;
        }

        lastword = out;

        /* Copy non-white space, watching for variables. */
        while ( *in && !isspace( *in ) )
        {
            if ( out >= oute )
                return -1;

            if ( ( in[ 0 ] == '$' ) && ( in[ 1 ] == '(' ) )
            {
                ++dollar;
                *out++ = *in++;
            }
            #ifdef OPT_AT_FILES
            else if ( ( in[ 0 ] == '@' ) && ( in[ 1 ] == '(' ) )
            {
                int depth = 1;
                char * ine = in + 2;
                char * split = 0;

                /* Scan the content of the response file @() section. */
                while ( *ine && ( depth > 0 ) )
                {
                    switch ( *ine )
                    {
                    case '(': ++depth; break;
                    case ')': --depth; break;
                    case ':':
                        if ( ( depth == 1 ) && ( ine[ 1 ] == 'E' ) && ( ine[ 2 ] == '=' ) )
                            split = ine;
                        break;
                    }
                    ++ine;
                }

                if ( !split )
                {
                    /*  the @() reference doesn't match the @(foo:E=bar) format.
                        hence we leave it alone by copying directly to output. */
                    int l = 0;
                    if ( out + 2 >= oute ) return -1;
                    *( out++ ) = '@';
                    *( out++ ) = '(';
                    l = var_string( in + 2, out, oute - out, lol );
                    if ( l < 0 ) return -1;
                    out += l;
                    if ( out + 1 >= oute ) return -1;
                    *( out++ ) = ')';
                }
                else if ( depth == 0 )
                {
                    string file_name_v;
                    int file_name_l = 0;
                    const char * file_name_s = 0;

                    /* Expand the temporary file name var inline. */
                    #if 0
                    string_copy( &file_name_v, "$(" );
                    string_append_range( &file_name_v, in + 2, split );
                    string_push_back( &file_name_v, ')' );
                    #else
                    string_new( &file_name_v );
                    string_append_range( &file_name_v, in + 2, split );
                    #endif
                    file_name_l = var_string( file_name_v.value, out, oute - out + 1, lol );
                    string_free( &file_name_v );
                    if ( file_name_l < 0 ) return -1;
                    file_name_s = out;

                    /* For stdout/stderr we will create a temp file and generate
                     * a command that outputs the content as needed.
                     */
                    if ( ( strcmp( "STDOUT", out ) == 0 ) ||
                        ( strcmp( "STDERR", out ) == 0 ) )
                    {
                        int err_redir = strcmp( "STDERR", out ) == 0;
                        out[ 0 ] = '\0';
                        file_name_s = path_tmpfile();
                        file_name_l = strlen(file_name_s);
                        #ifdef OS_NT
                        if ( ( out + 7 + file_name_l + ( err_redir ? 5 : 0 ) ) >= oute )
                            return -1;
                        sprintf( out,"type \"%s\"%s", file_name_s,
                            err_redir ? " 1>&2" : "" );
                        #else
                        if ( ( out + 6 + file_name_l + ( err_redir ? 5 : 0 ) ) >= oute )
                            return -1;
                        sprintf( out,"cat \"%s\"%s", file_name_s,
                            err_redir ? " 1>&2" : "" );
                        #endif
                        /* We also make sure that the temp files created by this
                         * get nuked eventually.
                         */
                        file_remove_atexit( file_name_s );
                    }

                    /* Expand the file value into the file reference. */
                    var_string_to_file( split + 3, ine - split - 4, file_name_s,
                        lol );

                    /* Continue on with the expansion. */
                    out += strlen( out );
                }

                /* And continue with the parsing just past the @() reference. */
                in = ine;
            }
            #endif
            else
            {
                *out++ = *in++;
            }
        }

        /* Add zero to 'out' so that 'lastword' is correctly zero-terminated. */
        if ( out >= oute )
            return -1;
        /* Do not increment, intentionally. */
        *out = '\0';

        /* If a variable encountered, expand it and and embed the
         * space-separated members of the list in the output.
         */
        if ( dollar )
        {
            LIST * l = var_expand( L0, lastword, out, lol, 0 );

            out = lastword;

            while ( l )
            {
                int so = strlen( l->string );

                if ( out + so >= oute )
                    return -1;

                strcpy( out, l->string );
                out += so;
                l = list_next( l );
                if ( l ) *out++ = ' ';
            }

            list_free( l );
        }
    }

    if ( out >= oute )
        return -1;

    *out++ = '\0';

    return out - out0;
}


void var_string_to_file( const char * in, int insize, const char * out, LOL * lol )
{
    char const * ine = in + insize;
    FILE * out_file = 0;
    int out_debug = DEBUG_EXEC ? 1 : 0;
    if ( globs.noexec )
    {
        /* out_debug = 1; */
    }
    else if ( strcmp( out, "STDOUT" ) == 0 )
    {
        out_file = stdout;
    }
    else if ( strcmp( out, "STDERR" ) == 0 )
    {
        out_file = stderr;
    }
    else
    {
        /* Handle "path to file" filenames. */
        string out_name;
        if ( ( out[ 0 ] == '"' ) && ( out[ strlen( out ) - 1 ] == '"' ) )
        {
            string_copy( &out_name, out + 1 );
            string_truncate( &out_name, out_name.size - 1 );
        }
        else
        {
            string_copy( &out_name,out );
        }
        out_file = fopen( out_name.value, "w" );
        if ( !out_file )
        {
            printf( "failed to write output file '%s'!\n", out_name.value );
            exit( EXITBAD );
        }
        string_free( &out_name );
    }

    if ( out_debug ) printf( "\nfile %s\n", out );

    while ( *in && ( in < ine ) )
    {
        int dollar = 0;
        const char * output_0 = in;
        const char * output_1 = in;

        /* Copy white space. */
        while ( ( output_1 < ine ) && isspace( *output_1 ) )
            ++output_1;

        if ( output_0 < output_1 )
        {
            if ( out_file  ) fwrite( output_0, output_1 - output_0, 1, out_file );
            if ( out_debug ) fwrite( output_0, output_1 - output_0, 1, stdout   );
        }
        output_0 = output_1;

        /* Copy non-white space, watching for variables. */
        while ( ( output_1 < ine ) && *output_1 && !isspace( *output_1 ) )
        {
            if ( ( output_1[ 0 ] == '$' ) && ( output_1[ 1 ] == '(' ) )
                ++dollar;
            ++output_1;
        }

        /* If a variable encountered, expand it and embed the space-separated
         * members of the list in the output.
         */
        if ( dollar )
        {
            LIST * l = var_expand( L0, (char *)output_0, (char *)output_1, lol, 0 );

            while ( l )
            {
                if ( out_file ) fputs( l->string, out_file );
                if ( out_debug ) puts( l->string );
                l = list_next( l );
                if ( l )
                {
                    if ( out_file ) fputc( ' ', out_file );
                    if ( out_debug ) fputc( ' ', stdout );
                }
            }

            list_free( l );
        }
        else if ( output_0 < output_1 )
        {
            if ( out_file )
            {
                const char * output_n = output_0;
                while ( output_n < output_1 )
                {
                    output_n += fwrite( output_n, 1, output_1-output_n, out_file );
                }
            }
            if ( out_debug )
            {
                const char * output_n = output_0;
                while ( output_n < output_1 )
                {
                    output_n += fwrite( output_n, 1, output_1-output_n, stdout );
                }
            }
        }

        in = output_1;
    }

    if ( out_file && ( out_file != stdout ) && ( out_file != stderr ) )
    {
        fflush( out_file );
        fclose( out_file );
    }

    if ( out_debug ) fputc( '\n', stdout );
}


/*
 * var_get() - get value of a user defined symbol.
 *
 * Returns NULL if symbol unset.
 */

LIST * var_get( char * symbol )
{
    LIST * result = 0;
#ifdef OPT_AT_FILES
    /* Some "fixed" variables... */
    if ( strcmp( "TMPDIR", symbol ) == 0 )
    {
        result = list_new( L0, newstr( (char *)path_tmpdir() ) );
    }
    else if ( strcmp( "TMPNAME", symbol ) == 0 )
    {
        result = list_new( L0, newstr( (char *)path_tmpnam() ) );
    }
    else if ( strcmp( "TMPFILE", symbol ) == 0 )
    {
        result = list_new( L0, newstr( (char *)path_tmpfile() ) );
    }
    else if ( strcmp( "STDOUT", symbol ) == 0 )
    {
        result = list_new( L0, newstr( "STDOUT" ) );
    }
    else if ( strcmp( "STDERR", symbol ) == 0 )
    {
        result = list_new( L0, newstr( "STDERR" ) );
    }
    else
#endif
    {
        VARIABLE var;
        VARIABLE * v = &var;

        v->symbol = symbol;

        if ( varhash && hashcheck( varhash, (HASHDATA * *)&v ) )
        {
            if ( DEBUG_VARGET )
                var_dump( v->symbol, v->value, "get" );
            result = v->value;
        }
    }
    return result;
}


/*
 * var_set() - set a variable in Jam's user defined symbol table.
 *
 * 'flag' controls the relationship between new and old values of the variable:
 * SET replaces the old with the new; APPEND appends the new to the old; DEFAULT
 * only uses the new if the variable was previously unset.
 *
 * Copies symbol. Takes ownership of value.
 */

void var_set( char * symbol, LIST * value, int flag )
{
    VARIABLE * v = var_enter( symbol );

    if ( DEBUG_VARSET )
        var_dump( symbol, value, "set" );

    switch ( flag )
    {
    case VAR_SET:
        /* Replace value */
        list_free( v->value );
        v->value = value;
        break;

    case VAR_APPEND:
        /* Append value */
        v->value = list_append( v->value, value );
        break;

    case VAR_DEFAULT:
        /* Set only if unset */
        if ( !v->value )
            v->value = value;
        else
            list_free( value );
        break;
    }
}


/*
 * var_swap() - swap a variable's value with the given one.
 */

LIST * var_swap( char * symbol, LIST * value )
{
    VARIABLE * v = var_enter( symbol );
    LIST     * oldvalue = v->value;
    if ( DEBUG_VARSET )
        var_dump( symbol, value, "set" );
    v->value = value;
    return oldvalue;
}


/*
 * var_enter() - make new var symbol table entry, returning var ptr.
 */

static VARIABLE * var_enter( char * symbol )
{
    VARIABLE var;
    VARIABLE * v = &var;

    if ( !varhash )
        varhash = hashinit( sizeof( VARIABLE ), "variables" );

    v->symbol = symbol;
    v->value = 0;

    if ( hashenter( varhash, (HASHDATA * *)&v ) )
        v->symbol = newstr( symbol );  /* never freed */

    return v;
}


/*
 * var_dump() - dump a variable to stdout.
 */

static void var_dump( char * symbol, LIST * value, char * what )
{
    printf( "%s %s = ", what, symbol );
    list_print( value );
    printf( "\n" );
}


/*
 * var_done() - free variable tables.
 */

static void delete_var_( void * xvar, void * data )
{
    VARIABLE * v = (VARIABLE *)xvar;
    freestr( v->symbol );
    list_free( v-> value );
}


void var_done()
{
    hashenumerate( varhash, delete_var_, (void *)0 );
    hashdone( varhash );
}
