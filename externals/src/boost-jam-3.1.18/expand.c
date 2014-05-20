/*
 * Copyright 1993-2002 Christopher Seiwald and Perforce Software, Inc.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

# include "jam.h"
# include "lists.h"
# include "variable.h"
# include "expand.h"
# include "pathsys.h"
# include "newstr.h"
# include <assert.h>
# include <stdlib.h>
# include <limits.h>

# ifdef OS_CYGWIN
#  include <sys/cygwin.h>
#  include <windows.h>
# endif

/*
 * expand.c - expand a buffer, given variable values
 *
 * External routines:
 *
 *  var_expand() - variable-expand input string into list of strings
 *
 * Internal routines:
 *
 *  var_edit_parse() - parse : modifiers into PATHNAME structure.
 *  var_edit_file() - copy input target name to output, modifying filename.
 *  var_edit_shift() - do upshift/downshift mods.
 *
 * 01/25/94 (seiwald) - $(X)$(UNDEF) was expanding like plain $(X)
 * 04/13/94 (seiwald) - added shorthand L0 for null list pointer
 * 01/11/01 (seiwald) - added support for :E=emptyvalue, :J=joinval
 */

typedef struct
{
    PATHNAME f;           /* :GDBSMR -- pieces */
    char     parent;      /* :P -- go to parent directory */
    char     filemods;    /* one of the above applied */
    char     downshift;   /* :L -- downshift result */
    char     upshift;     /* :U -- upshift result */
    char     to_slashes;  /* :T -- convert "\" to "/" */
    char     to_windows;  /* :W -- convert cygwin to native paths */
    PATHPART empty;       /* :E -- default for empties */
    PATHPART join;        /* :J -- join list with char */
} VAR_EDITS ;

static void var_edit_parse( char * mods, VAR_EDITS * edits );
static void var_edit_file ( char * in, string * out, VAR_EDITS * edits );
static void var_edit_shift( string * out, VAR_EDITS * edits );

#define MAGIC_COLON '\001'
#define MAGIC_LEFT  '\002'
#define MAGIC_RIGHT '\003'


/*
 * var_expand() - variable-expand input string into list of strings.
 *
 * Would just copy input to output, performing variable expansion, except that
 * since variables can contain multiple values the result of variable expansion
 * may contain multiple values (a list). Properly performs "product" operations
 * that occur in "$(var1)xxx$(var2)" or even "$($(var2))".
 *
 * Returns a newly created list.
 */

LIST * var_expand( LIST * l, char * in, char * end, LOL * lol, int cancopyin )
{
    char out_buf[ MAXSYM ];
    string buf[ 1 ];
    string out1[ 1 ];  /* temporary buffer */
    size_t prefix_length;
    char * out;
    char * inp = in;
    char * ov;  /* for temp copy of variable in outbuf */
    int depth;

    if ( DEBUG_VAREXP )
        printf( "expand '%.*s'\n", end - in, in );

    /* This gets a lot of cases: $(<) and $(>). */
    if
    (
        ( in[ 0 ] == '$'  ) &&
        ( in[ 1 ] == '('  ) &&
        ( in[ 3 ] == ')'  ) &&
        ( in[ 4 ] == '\0' )
    )
    {
        switch ( in[ 2 ] )
        {
            case '<': return list_copy( l, lol_get( lol, 0 ) );
            case '>': return list_copy( l, lol_get( lol, 1 ) );

            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                return list_copy( l, lol_get( lol, in[ 2 ] - '1' ) );
        }
    }
    else if ( in[0] == '$' && in[1] == '(' && in[2] == '1' && in[4] == ')' &&
              in[5] == '\0') {

        switch( in[3] )
        {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            return list_copy( l, lol_get( lol, in[3]-'0'+10-1 ) );
        }
    }

    /* Expand @() files, to single item plus accompanying file. */
    if ( ( in[ 0 ] == '@' ) && ( in[ 1 ] == '(' ) && ( *( end - 1 ) == ')' ) )
    {
        /* We try the expansion until it fits within the propective output
         * buffer.
         */
        char * at_buf = 0;
        int at_size = MAXJPATH;
        int at_len = 0;
        do
        {
            BJAM_FREE( at_buf );
            at_buf = (char *)BJAM_MALLOC_ATOMIC( at_size + 1 );
            at_len = var_string( in, at_buf, at_size, lol );
            at_size *= 2;
        }
        while ( ( at_len < 0 ) && ( at_size < INT_MAX / 2 ) );
        /* Return the result as a single item list. */
        if ( at_len > 0 )
        {
            LIST * r;
            string_copy( buf, at_buf );
            r = list_new( l, newstr( buf->value ) );
            string_free( buf );
            BJAM_FREE( at_buf );
            return r;
        }
        BJAM_FREE( at_buf );
    }

    /* Just try simple copy of in to out. */
    while ( in < end )
        if ( ( *in++ == '$' ) && ( *in == '(' ) )
            goto expand;

    /* No variables expanded - just add copy of input string to list. */

    /* 'cancopyin' is an optimization: if the input was already a list item, we
     * can use copystr() to put it on the new list. Otherwise, we use the slower
     * newstr().
     */
    if ( cancopyin )
        return list_new( l, copystr( inp ) );

    {
        LIST * r;
        string_new( buf );
        string_append_range( buf, inp, end );
        r = list_new( l, newstr( buf->value ) );
        string_free( buf );
        return r;
    }

expand:
    string_new( buf );
    string_append_range( buf, inp, in - 1 );  /* Copy the part before '$'. */
    /*
     * Input so far (ignore blanks):
     *
     *  stuff-in-outbuf $(variable) remainder
     *                   ^                   ^
     *                   in                  end
     * Output so far:
     *
     *  stuff-in-outbuf $
     *  ^                ^
     *  out_buf          out
     *
     *
     * We just copied the $ of $(...), so back up one on the output. We now find
     * the matching close paren, copying the variable and modifiers between the
     * $( and ) temporarily into out_buf, so that we can replace :'s with
     * MAGIC_COLON. This is necessary to avoid being confused by modifier values
     * that are variables containing :'s. Ugly.
     */

    depth = 1;
    inp = ++in;  /* Skip over the '('. */

    while ( ( in < end ) && depth )
    {
        switch ( *in++ )
        {
            case '(': ++depth; break;
            case ')': --depth; break;
        }
    }

    /*
     * Input so far (ignore blanks):
     *
     *  stuff-in-outbuf $(variable) remainder
     *                    ^        ^         ^
     *                    inp      in        end
     */
    prefix_length = buf->size;
    string_append_range( buf, inp, in - 1 );

    out = buf->value + prefix_length;
    for ( ov = out; ov < buf->value + buf->size; ++ov )
    {
        switch ( *ov )
        {
            case ':': *ov = MAGIC_COLON; break;
            case '[': *ov = MAGIC_LEFT ; break;
            case ']': *ov = MAGIC_RIGHT; break;
        }
    }

    /*
     * Input so far (ignore blanks):
     *
     *  stuff-in-outbuf $(variable) remainder
     *                              ^        ^
     *                              in       end
     * Output so far:
     *
     *  stuff-in-outbuf variable
     *  ^               ^       ^
     *  out_buf         out     ov
     *
     * Later we will overwrite 'variable' in out_buf, but we will be done with
     * it by then. 'variable' may be a multi-element list, so may each value for
     * '$(variable element)', and so may 'remainder'. Thus we produce a product
     * of three lists.
     */
    {
        LIST * variables = 0;
        LIST * remainder = 0;
        LIST * vars;

        /* Recursively expand variable name & rest of input. */
        if ( out < ov ) variables = var_expand( L0, out, ov, lol, 0 );
        if ( in < end ) remainder = var_expand( L0, in, end, lol, 0 );

        /* Now produce the result chain. */

        /* For each variable name. */
        for ( vars = variables; vars; vars = list_next( vars ) )
        {
            LIST * value = 0;
            LIST * evalue = 0;
            char * colon;
            char * bracket;
            string variable[1];
            char * varname;
            int sub1 = 0;
            int sub2 = -1;
            VAR_EDITS edits;

            /* Look for a : modifier in the variable name. Must copy into
             * varname so we can modify it.
             */
            string_copy( variable, vars->string );
            varname = variable->value;

            if ( ( colon = strchr( varname, MAGIC_COLON ) ) )
            {
                string_truncate( variable, colon - varname );
                var_edit_parse( colon + 1, &edits );
            }

            /* Look for [x-y] subscripting. sub1 and sub2 are x and y. */
            if ( ( bracket = strchr( varname, MAGIC_LEFT ) ) )
            {
                /* Make all syntax errors in [] subscripting result in the same
                 * behavior: silenty return an empty expansion (by setting sub2
                 * = 0). Brute force parsing; May get moved into yacc someday.
                 */

                char * s = bracket + 1;

                string_truncate( variable, bracket - varname );

                do  /* so we can use "break" */
                {
                    /* Allow negative indexes. */
                    if ( !isdigit( *s ) && ( *s != '-' ) )
                    {
                        sub2 = 0;
                        break;
                    }
                    sub1 = atoi( s );

                    /* Skip over the first symbol, which is either a digit or dash. */
                    ++s;
                    while ( isdigit( *s ) ) ++s;

                    if ( *s == MAGIC_RIGHT )
                    {
                        sub2 = sub1;
                        break;
                    }

                    if ( *s != '-' )
                    {
                        sub2 = 0;
                        break;
                    }

                    ++s;

                    if ( *s == MAGIC_RIGHT )
                    {
                        sub2 = -1;
                        break;
                    }

                    if ( !isdigit( *s ) && ( *s != '-' ) )
                    {
                        sub2 = 0;
                        break;
                    }

                    /* First, compute the index of the last element. */
                    sub2 = atoi( s );
                    while ( isdigit( *++s ) );

                    if ( *s != MAGIC_RIGHT )
                        sub2 = 0;

                } while ( 0 );

                /* Anything but the end of the string, or the colon introducing
                 * a modifier is a syntax error.
                 */
                ++s;
                if ( *s && ( *s != MAGIC_COLON ) )
                    sub2 = 0;

                *bracket = '\0';
            }

            /* Get variable value, with special handling for $(<), $(>), $(n).
             */
            if ( !varname[1] )
            {
                if ( varname[0] == '<' )
                    value = lol_get( lol, 0 );
                else if ( varname[0] == '>' )
                    value = lol_get( lol, 1 );
                else if ( ( varname[0] >= '1' ) && ( varname[0] <= '9' ) )
                    value = lol_get( lol, varname[0] - '1' );
                else if( varname[0] == '1' && varname[1] >= '0' &&
                     varname[1] <= '9' && !varname[2] )
                value = lol_get( lol, varname[1] - '0' + 10 - 1 );
            }

            if ( !value )
                value = var_get( varname );

            /* Handle negitive indexes: part two. */
            {
                int length = list_length( value );

                if ( sub1 < 0 )
                    sub1 = length + sub1;
                else
                    sub1 -= 1;

                if ( sub2 < 0 )
                    sub2 = length + 1 + sub2 - sub1;
                else
                    sub2 -= sub1;
                /* The "sub2 < 0" test handles the semantic error of sub2 <
                 * sub1.
                 */
                if ( sub2 < 0 )
                    sub2 = 0;
            }

            /* The fast path: $(x) - just copy the variable value. This is only
             * an optimization.
             */
            if ( ( out == out_buf ) && !bracket && !colon && ( in == end ) )
            {
                string_free( variable );
                l = list_copy( l, value );
                continue;
            }

            /* Handle start subscript. */
            while ( ( sub1 > 0 ) && value )
                --sub1, value = list_next( value );

            /* Empty w/ :E=default?. */
            if ( !value && colon && edits.empty.ptr )
                evalue = value = list_new( L0, newstr( edits.empty.ptr ) );

            /* For each variable value. */
            string_new( out1 );
            for ( ; value; value = list_next( value ) )
            {
                LIST * rem;
                size_t postfix_start;

                /* Handle end subscript (length actually). */

                if ( sub2 >= 0 && --sub2 < 0 )
                    break;

                string_truncate( buf, prefix_length );

                /* Apply : mods, if present */

                if ( colon && edits.filemods )
                    var_edit_file( value->string, out1, &edits );
                else
                    string_append( out1, value->string );

                if ( colon && ( edits.upshift || edits.downshift || edits.to_slashes || edits.to_windows ) )
                    var_edit_shift( out1, &edits );

                /* Handle :J=joinval */
                /* If we have more values for this var, just keep appending them
                 * (using the join value) rather than creating separate LIST
                 * elements.
                 */
                if ( colon && edits.join.ptr &&
                    ( list_next( value ) || list_next( vars ) ) )
                {
                    string_append( out1, edits.join.ptr );
                    continue;
                }

                string_append( buf, out1->value );
                string_free( out1 );
                string_new( out1 );

                /* If no remainder, append result to output chain. */
                if ( in == end )
                {
                    l = list_new( l, newstr( buf->value ) );
                    continue;
                }

                /* For each remainder, append the complete string to the output
                 * chain. Remember the end of the variable expansion so we can
                 * just tack on each instance of 'remainder'.
                 */
                postfix_start = buf->size;
                for ( rem = remainder; rem; rem = list_next( rem ) )
                {
                    string_truncate( buf, postfix_start );
                    string_append( buf, rem->string );
                    l = list_new( l, newstr( buf->value ) );
                }
            }
            string_free( out1 );

            /* Toss used empty. */
            if ( evalue )
                list_free( evalue );

            string_free( variable );
        }

        /* variables & remainder were gifts from var_expand and must be freed. */
        if ( variables ) list_free( variables );
        if ( remainder ) list_free( remainder );

        if ( DEBUG_VAREXP )
        {
            printf( "expanded to " );
            list_print( l );
            printf( "\n" );
        }

        string_free( buf );
        return l;
    }
}


/*
 * var_edit_parse() - parse : modifiers into PATHNAME structure
 *
 * The : modifiers in a $(varname:modifier) currently support replacing or
 * omitting elements of a filename, and so they are parsed into a PATHNAME
 * structure (which contains pointers into the original string).
 *
 * Modifiers of the form "X=value" replace the component X with the given value.
 * Modifiers without the "=value" cause everything but the component X to be
 * omitted. X is one of:
 *
 *  G <grist>
 *  D directory name
 *  B base name
 *  S .suffix
 *  M (member)
 *  R root directory - prepended to whole path
 *
 * This routine sets:
 *
 *  f->f_xxx.ptr = 0
 *  f->f_xxx.len = 0
 *      -> leave the original component xxx
 *
 *  f->f_xxx.ptr = string
 *  f->f_xxx.len = strlen( string )
 *      -> replace component xxx with string
 *
 *  f->f_xxx.ptr = ""
 *  f->f_xxx.len = 0
 *      -> omit component xxx
 *
 * var_edit_file() below and path_build() obligingly follow this convention.
 */

static void var_edit_parse( char * mods, VAR_EDITS * edits )
{
    int havezeroed = 0;
    memset( (char *)edits, 0, sizeof( *edits ) );

    while ( *mods )
    {
        char * p;
        PATHPART * fp;

        switch ( *mods++ )
        {
            case 'L': edits->downshift = 1; continue;
            case 'U': edits->upshift = 1; continue;
            case 'P': edits->parent = edits->filemods = 1; continue;
            case 'E': fp = &edits->empty; goto strval;
            case 'J': fp = &edits->join; goto strval;
            case 'G': fp = &edits->f.f_grist; goto fileval;
            case 'R': fp = &edits->f.f_root; goto fileval;
            case 'D': fp = &edits->f.f_dir; goto fileval;
            case 'B': fp = &edits->f.f_base; goto fileval;
            case 'S': fp = &edits->f.f_suffix; goto fileval;
            case 'M': fp = &edits->f.f_member; goto fileval;
            case 'T': edits->to_slashes = 1; continue;
            case 'W': edits->to_windows = 1; continue;
            default:
                return;  /* Should complain, but so what... */
        }

    fileval:
        /* Handle :CHARS, where each char (without a following =) selects a
         * particular file path element. On the first such char, we deselect all
         * others (by setting ptr = "", len = 0) and for each char we select
         * that element (by setting ptr = 0).
         */
        edits->filemods = 1;

        if ( *mods != '=' )
        {
            if ( !havezeroed++ )
            {
                int i;
                for ( i = 0; i < 6; ++i )
                {
                    edits->f.part[ i ].len = 0;
                    edits->f.part[ i ].ptr = "";
                }
            }

            fp->ptr = 0;
            continue;
        }

    strval:
        /* Handle :X=value, or :X */
        if ( *mods != '=' )
        {
            fp->ptr = "";
            fp->len = 0;
        }
        else if ( ( p = strchr( mods, MAGIC_COLON ) ) )
        {
            *p = 0;
            fp->ptr = ++mods;
            fp->len = p - mods;
            mods = p + 1;
        }
        else
        {
            fp->ptr = ++mods;
            fp->len = strlen( mods );
            mods += fp->len;
        }
    }
}


/*
 * var_edit_file() - copy input target name to output, modifying filename.
 */

static void var_edit_file( char * in, string * out, VAR_EDITS * edits )
{
    PATHNAME pathname;

    /* Parse apart original filename, putting parts into "pathname". */
    path_parse( in, &pathname );

    /* Replace any pathname with edits->f */
    if ( edits->f.f_grist .ptr ) pathname.f_grist  = edits->f.f_grist;
    if ( edits->f.f_root  .ptr ) pathname.f_root   = edits->f.f_root;
    if ( edits->f.f_dir   .ptr ) pathname.f_dir    = edits->f.f_dir;
    if ( edits->f.f_base  .ptr ) pathname.f_base   = edits->f.f_base;
    if ( edits->f.f_suffix.ptr ) pathname.f_suffix = edits->f.f_suffix;
    if ( edits->f.f_member.ptr ) pathname.f_member = edits->f.f_member;

    /* If requested, modify pathname to point to parent. */
    if ( edits->parent )
        path_parent( &pathname );

    /* Put filename back together. */
    path_build( &pathname, out, 0 );
}


/*
 * var_edit_shift() - do upshift/downshift mods.
 */

static void var_edit_shift( string * out, VAR_EDITS * edits )
{
    /* Handle upshifting, downshifting and slash translation now. */
    char * p;
    for ( p = out->value; *p; ++p)
    {
        if ( edits->upshift )
            *p = toupper( *p );
        else if ( edits->downshift )
            *p = tolower( *p );
        if ( edits->to_slashes && ( *p == '\\' ) )
            *p = '/';
# ifdef OS_CYGWIN
        if ( edits->to_windows )
        {
            char result[ MAX_PATH + 1 ];
            cygwin_conv_to_win32_path( out->value, result );
            assert( strlen( result ) <= MAX_PATH );
            string_free( out );
            string_copy( out, result );
        }
# endif
    }
    out->size = p - out->value;
}


#ifndef NDEBUG
void var_expand_unit_test()
{
    LOL lol[ 1 ];
    LIST * l;
    LIST * l2;
    LIST * expected = list_new( list_new( L0, newstr( "axb" ) ), newstr( "ayb" ) );
    LIST * e2;
    char axyb[] = "a$(xy)b";
    char azb[] = "a$($(z))b";
    char path[] = "$(p:W)";

# ifdef OS_CYGWIN
    char cygpath[ 256 ];
    cygwin_conv_to_posix_path( "c:\\foo\\bar", cygpath );
# else
    char cygpath[] = "/cygdrive/c/foo/bar";
# endif

    lol_init(lol);
    var_set( "xy", list_new( list_new( L0, newstr( "x" ) ), newstr( "y" ) ), VAR_SET );
    var_set( "z", list_new( L0, newstr( "xy" ) ), VAR_SET );
    var_set( "p", list_new( L0, newstr( cygpath ) ), VAR_SET );

    l = var_expand( 0, axyb, axyb + sizeof( axyb ) - 1, lol, 0 );
    for ( l2 = l, e2 = expected; l2 && e2; l2 = list_next( l2 ), e2 = list_next( e2 ) )
        assert( !strcmp( e2->string, l2->string ) );
    assert( l2 == 0 );
    assert( e2 == 0 );
    list_free( l );

    l = var_expand( 0, azb, azb + sizeof( azb ) - 1, lol, 0 );
    for ( l2 = l, e2 = expected; l2 && e2; l2 = list_next( l2 ), e2 = list_next( e2 ) )
        assert( !strcmp( e2->string, l2->string ) );
    assert( l2 == 0 );
    assert( e2 == 0 );
    list_free( l );

    l = var_expand( 0, path, path + sizeof( path ) - 1, lol, 0 );
    assert( l != 0 );
    assert( list_next( l ) == 0 );
# ifdef OS_CYGWIN
    /* On some installations of cygwin the drive letter is expanded to other
     * case. This has been reported to be the case if cygwin has been installed
     * to C:\ as opposed to C:\cygwin. Since case of the drive letter will not
     * matter, we allow for both.
     */
    assert( !strcmp( l->string, "c:\\foo\\bar" ) ||
            !strcmp( l->string, "C:\\foo\\bar" ) );
# else
    assert( !strcmp( l->string, cygpath ) );
# endif
    list_free( l );
    list_free( expected );
    lol_free( lol );
}
#endif
