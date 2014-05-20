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

# include "jam.h"
# include "pathsys.h"

# ifdef OS_VMS

# define DEBUG

/*
 * pathvms.c - manipulate file names on VMS
 *
 * External routines:
 *
 *  path_parse() - split a file name into dir/base/suffix/member
 *  path_build() - build a filename given dir/base/suffix/member
 *  path_parent() - make a PATHNAME point to its parent dir
 *
 * File_parse() and path_build() just manipuate a string and a structure;
 * they do not make system calls.
 *
 * WARNING!  This file contains voodoo logic, as black magic is
 * necessary for wrangling with VMS file name.  Woe be to people
 * who mess with this code.
 *
 * 02/09/95 (seiwald) - bungled R=[xxx] - was using directory length!
 * 05/03/96 (seiwald) - split from filevms.c
 */

/*
 * path_parse() - split a file name into dir/base/suffix/member.
 */

void path_parse( char * file, PATHNAME * f )
{
    char * p;
    char * q;
    char * end;

    memset( (char *)f, 0, sizeof( *f ) );

    /* Look for <grist> */

    if ( ( file[0] == '<' ) && ( p = strchr( file, '>' ) ) )
    {
        f->f_grist.ptr = file;
        f->f_grist.len = p - file;
        file = p + 1;
    }

    /* Look for dev:[dir] or dev: */

    if ( ( p = strchr( file, ']' ) ) || ( p = strchr( file, ':' ) ) )
    {
        f->f_dir.ptr = file;
        f->f_dir.len = p + 1 - file;
        file = p + 1;
    }

    end = file + strlen( file );

    /* Look for (member). */

    if ( ( p = strchr( file, '(' ) ) && ( end[ -1 ] == ')' ) )
    {
        f->f_member.ptr = p + 1;
        f->f_member.len = end - p - 2;
        end = p;
    }

    /* Look for .suffix */
    /* This would be memrchr(). */

    p = 0;
    q = file;

    while ( q = (char *)memchr( q, '.', end - q ) )
        p = q++;

    if ( p )
    {
        f->f_suffix.ptr = p;
        f->f_suffix.len = end - p;
        end = p;
    }

    /* Leaves base. */
    f->f_base.ptr = file;
    f->f_base.len = end - file;

    /* Is this a directory without a file spec? */
    f->parent = 0;
}

/*
 *  dir     mods        result
 *  ---     ---     ------
 * Rerooting:
 *
 *  (none)      :R=dev:     dev:
 *  devd:       :R=dev:     devd:
 *  devd:[dir]  :R=dev:     devd:[dir]
 *  [.dir]      :R=dev:     dev:[dir]   questionable
 *  [dir]       :R=dev:     dev:[dir]
 *
 *  (none)      :R=[rdir]   [rdir]      questionable
 *  devd:       :R=[rdir]   devd:
 *  devd:[dir]  :R=[rdir]   devd:[dir]
 *  [.dir]      :R=[rdir]   [rdir.dir]  questionable
 *  [dir]       :R=[rdir]   [rdir]
 *
 *  (none)      :R=dev:[root]   dev:[root]
 *  devd:       :R=dev:[root]   devd:
 *  devd:[dir]  :R=dev:[root]   devd:[dir]
 *  [.dir]      :R=dev:[root]   dev:[root.dir]
 *  [dir]       :R=dev:[root]   [dir]
 *
 * Climbing to parent:
 *
 */

# define DIR_EMPTY  0   /* empty string */
# define DIR_DEV    1   /* dev: */
# define DIR_DEVDIR 2   /* dev:[dir] */
# define DIR_DOTDIR 3   /* [.dir] */
# define DIR_DASHDIR    4   /* [-] or [-.dir] */
# define DIR_ABSDIR 5   /* [dir] */
# define DIR_ROOT   6   /* [000000] or dev:[000000] */

# define G_DIR      0   /* take just dir */
# define G_ROOT     1   /* take just root */
# define G_VAD      2   /* root's dev: + [abs] */
# define G_DRD      3   /* root's dev:[dir] + [.rel] */
# define G_VRD      4   /* root's dev: + [.rel] made [abs] */
# define G_DDD      5   /* root's dev:[dir] + . + [dir] */

static int grid[7][7] = {

/* root/dir EMPTY   DEV DEVDIR  DOTDIR  DASH,   ABSDIR  ROOT */
/* EMPTY */ G_DIR,  G_DIR,  G_DIR,  G_DIR,  G_DIR,  G_DIR,  G_DIR,
/* DEV */   G_ROOT, G_DIR,  G_DIR,  G_VRD,  G_VAD,  G_VAD,  G_VAD,
/* DEVDIR */    G_ROOT, G_DIR,  G_DIR,  G_DRD,  G_VAD,  G_VAD,  G_VAD,
/* DOTDIR */    G_ROOT, G_DIR,  G_DIR,  G_DRD,  G_DIR,  G_DIR,  G_DIR,
/* DASHDIR */   G_ROOT, G_DIR,  G_DIR,  G_DRD,  G_DDD,  G_DIR,  G_DIR,
/* ABSDIR */    G_ROOT, G_DIR,  G_DIR,  G_DRD,  G_DIR,  G_DIR,  G_DIR,
/* ROOT */  G_ROOT, G_DIR,  G_DIR,  G_VRD,  G_DIR,  G_DIR,  G_DIR,

};

struct dirinf
{
    int flags;

    struct
    {
        char * ptr;
        int   len;
    } dev, dir;
};

static char * strnchr( char * buf, int c, int len )
{
    while ( len-- )
        if ( *buf && ( *buf++ == c ) )
            return buf - 1;
    return 0;
}


static void dir_flags( char * buf, int len, struct dirinf * i )
{
    char * p;

    if ( !buf || !len )
    {
        i->flags = DIR_EMPTY;
        i->dev.ptr =
        i->dir.ptr = 0;
        i->dev.len =
        i->dir.len = 0;
    }
    else if ( p = strnchr( buf, ':', len ) )
    {
        i->dev.ptr = buf;
        i->dev.len = p + 1 - buf;
        i->dir.ptr = buf + i->dev.len;
        i->dir.len = len - i->dev.len;
        i->flags = i->dir.len && *i->dir.ptr == '[' ? DIR_DEVDIR : DIR_DEV;
    }
    else
    {
        i->dev.ptr = buf;
        i->dev.len = 0;
        i->dir.ptr = buf;
        i->dir.len = len;

        if ( ( *buf == '[' ) && ( buf[1] == ']' ) )
            i->flags = DIR_EMPTY;
        else if ( ( *buf == '[' ) && ( buf[1] == '.' ) )
            i->flags = DIR_DOTDIR;
        else if ( ( *buf == '[' ) && ( buf[1] == '-' ) )
            i->flags = DIR_DASHDIR;
        else
            i->flags = DIR_ABSDIR;
    }

    /* But if its rooted in any way. */

    if ( ( i->dir.len == 8 ) && !strncmp( i->dir.ptr, "[000000]", 8 ) )
        i->flags = DIR_ROOT;
}


/*
 * path_build() - build a filename given dir/base/suffix/member
 */

void path_build( PATHNAME * f, string * file, int binding )
{
    struct dirinf root;
    struct dirinf dir;
    int g;

    file_build1( f, file );

    /* Get info on root and dir for combining. */
    dir_flags( f->f_root.ptr, f->f_root.len, &root );
    dir_flags( f->f_dir.ptr, f->f_dir.len, &dir );

    /* Combine. */
    switch ( g = grid[ root.flags ][ dir.flags ] )
    {
    case G_DIR:
        /* take dir */
        string_append_range( file, f->f_dir.ptr, f->f_dir.ptr + f->f_dir.len  );
        break;

    case G_ROOT:
        /* take root */
        string_append_range( file, f->f_root.ptr, f->f_root.ptr + f->f_root.len  );
        break;

    case G_VAD:
        /* root's dev + abs directory */
        string_append_range( file, root.dev.ptr, root.dev.ptr + root.dev.len  );
        string_append_range( file, dir.dir.ptr, dir.dir.ptr + dir.dir.len  );
        break;

    case G_DRD:
    case G_DDD:
        /* root's dev:[dir] + rel directory */
        string_append_range( file, f->f_root.ptr, f->f_root.ptr + f->f_root.len  );

        /* sanity checks: root ends with ] */

        if ( file->value[file->size - 1] == ']' )
            string_pop_back( file );

        /* Add . if separating two -'s */

        if ( g == G_DDD )
            string_push_back( file, '.' );

        /* skip [ of dir */
        string_append_range( file, dir.dir.ptr + 1, dir.dir.ptr + 1 + dir.dir.len - 1  );
        break;

    case G_VRD:
        /* root's dev + rel directory made abs */
        string_append_range( file, root.dev.ptr, root.dev.ptr + root.dev.len  );
        string_push_back( file, '[' );
        /* skip [. of rel dir */
        string_append_range( file, dir.dir.ptr + 2, dir.dir.ptr + 2 + dir.dir.len - 2  );
        break;
    }

# ifdef DEBUG
    if ( DEBUG_SEARCH && ( root.flags || dir.flags ) )
        printf( "%d x %d = %d (%s)\n", root.flags, dir.flags,
                grid[ root.flags ][ dir.flags ], file->value );
# endif

    /*
     * Now do the special :P modifier when no file was present.
     *  (none)      (none)
     *  [dir1.dir2] [dir1]
     *  [dir]       [000000]
     *  [.dir]      (none)
     *  []      []
     */

    if ( ( file->value[ file->size - 1 ] == ']' ) && f->parent )
    {
        char * p = file->value + file->size;
        while ( p-- > file->value )
        {
            if ( *p == '.' )
            {
                /* If we've truncated everything and left with '[',
                   return empty string. */
                if ( p == file->value + 1 )
                    string_truncate( file, 0 );
                else
                {
                    string_truncate( file, p - file->value );
                    string_push_back( file, ']' );
                }
                break;
            }
            
            if ( *p == '-' )
            {
                /* handle .- or - */
                if ( ( p > file->value ) && ( p[ -1 ] == '.' ) )
                    --p;

                *p++ = ']';
                break;
            }
            
            if ( *p == '[' )
            {
                if ( p[ 1 ] == ']' )
                {
                    /* CONSIDER: I don't see any use of this code. We immediately
                       break, and 'p' is a local variable. */
                    p += 2;
                }
                else
                {
                    string_truncate( file, p - file->value );
                    string_append( file, "[000000]" );
                }
                break;
            }
        }
    }

    /* Now copy the file pieces. */
    if ( f->f_base.len )
    {
        string_append_range( file, f->f_base.ptr, f->f_base.ptr + f->f_base.len  );
    }

    /* If there is no suffix, we append a "." onto all generated names. This
     * keeps VMS from appending its own (wrong) idea of what the suffix should
     * be.
     */
    if ( f->f_suffix.len )
        string_append_range( file, f->f_suffix.ptr, f->f_suffix.ptr + f->f_suffix.len  );
    else if ( binding && f->f_base.len )
        string_push_back( file, '.' );

    if ( f->f_member.len )
    {
        string_push_back( file, '(' );
        string_append_range( file, f->f_member.ptr, f->f_member.ptr + f->f_member.len  );
        string_push_back( file, ')' );
    }

# ifdef DEBUG
    if ( DEBUG_SEARCH )
        printf( "built %.*s + %.*s / %.*s suf %.*s mem %.*s -> %s\n",
               f->f_root.len, f->f_root.ptr,
               f->f_dir.len, f->f_dir.ptr,
               f->f_base.len, f->f_base.ptr,
               f->f_suffix.len, f->f_suffix.ptr,
               f->f_member.len, f->f_member.ptr,
               file->value );
# endif
}


/*
 *  path_parent() - make a PATHNAME point to its parent dir
 */

void path_parent( PATHNAME * f )
{
    if ( f->f_base.len )
    {
        f->f_base.ptr =
        f->f_suffix.ptr =
        f->f_member.ptr = "";

        f->f_base.len =
        f->f_suffix.len =
        f->f_member.len = 0;
    }
    else
    {
        f->parent = 1;
    }
}

# endif /* VMS */
