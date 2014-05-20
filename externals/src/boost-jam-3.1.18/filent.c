/*
 * Copyright 1993, 1995 Christopher Seiwald.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

/*  This file is ALSO:
 *  Copyright 2001-2004 David Abrahams.
 *  Copyright 2005 Rene Rivera.
 *  Distributed under the Boost Software License, Version 1.0.
 *  (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt)
 */

# include "jam.h"

# include "filesys.h"
# include "pathsys.h"
# include "strings.h"
# include "newstr.h"

# ifdef OS_NT

# ifdef __BORLANDC__
# if __BORLANDC__ < 0x550
# include <dir.h>
# include <dos.h>
# endif
# undef FILENAME    /* cpp namespace collision */
# define _finddata_t ffblk
# endif

# include <io.h>
# include <sys/stat.h>
# include <ctype.h>

/*
 * filent.c - scan directories and archives on NT
 *
 * External routines:
 *
 *  file_dirscan() - scan a directory for files
 *  file_time() - get timestamp of file, if not done by file_dirscan()
 *  file_archscan() - scan an archive for files
 *
 * File_dirscan() and file_archscan() call back a caller provided function
 * for each file found.  A flag to this callback function lets file_dirscan()
 * and file_archscan() indicate that a timestamp is being provided with the
 * file.   If file_dirscan() or file_archscan() do not provide the file's
 * timestamp, interested parties may later call file_time().
 *
 * 07/10/95 (taylor)  Findfirst() returns the first file on NT.
 * 05/03/96 (seiwald) split apart into pathnt.c
 */

/*
 * file_dirscan() - scan a directory for files
 */

void file_dirscan( char * dir, scanback func, void * closure )
{
    PROFILE_ENTER( FILE_DIRSCAN );

    file_info_t * d = 0;

    dir = short_path_to_long_path( dir );

    /* First enter directory itself */

    d = file_query( dir );

    if ( !d || !d->is_dir )
    {
        PROFILE_EXIT( FILE_DIRSCAN );
        return;
    }

    if ( !d->files )
    {
        PATHNAME f;
        string filespec[ 1 ];
        string filename[ 1 ];
        long handle;
        int ret;
        struct _finddata_t finfo[ 1 ];
        LIST * files = L0;
        int d_length = strlen( d->name );

        memset( (char *)&f, '\0', sizeof( f ) );

        f.f_dir.ptr = d->name;
        f.f_dir.len = d_length;

        /* Now enter contents of directory */

        /* Prepare file search specification for the findfirst() API. */
        if ( d_length == 0 )
            string_copy( filespec, ".\\*" );
        else
        {
            /*
             * We can not simply assume the given folder name will never include
             * its trailing path separator or otherwise we would not support the
             * Windows root folder specified without its drive letter, i.e. '\'.
             */
            char trailingChar = d->name[ d_length - 1 ] ;
            string_copy( filespec, d->name );
            if ( ( trailingChar != '\\' ) && ( trailingChar != '/' ) )
                string_append( filespec, "\\" );
            string_append( filespec, "*" );
        }

        if ( DEBUG_BINDSCAN )
            printf( "scan directory %s\n", dir );

        #if defined(__BORLANDC__) && __BORLANDC__ < 0x550
        if ( ret = findfirst( filespec->value, finfo, FA_NORMAL | FA_DIREC ) )
        {
            string_free( filespec );
            PROFILE_EXIT( FILE_DIRSCAN );
            return;
        }

        string_new ( filename );
        while ( !ret )
        {
            file_info_t * ff = 0;

            f.f_base.ptr = finfo->ff_name;
            f.f_base.len = strlen( finfo->ff_name );

            string_truncate( filename, 0 );
            path_build( &f, filename );

            files = list_new( files, newstr(filename->value) );
            ff = file_info( filename->value );
            ff->is_file = finfo->ff_attrib & FA_DIREC ? 0 : 1;
            ff->is_dir = finfo->ff_attrib & FA_DIREC ? 1 : 0;
            ff->size = finfo->ff_fsize;
            ff->time = (finfo->ff_ftime << 16) | finfo->ff_ftime;

            ret = findnext( finfo );
        }
        # else
        handle = _findfirst( filespec->value, finfo );

        if ( ret = ( handle < 0L ) )
        {
            string_free( filespec );
            PROFILE_EXIT( FILE_DIRSCAN );
            return;
        }

        string_new( filename );
        while ( !ret )
        {
            file_info_t * ff = 0;

            f.f_base.ptr = finfo->name;
            f.f_base.len = strlen( finfo->name );

            string_truncate( filename, 0 );
            path_build( &f, filename, 0 );

            files = list_new( files, newstr( filename->value ) );
            ff = file_info( filename->value );
            ff->is_file = finfo->attrib & _A_SUBDIR ? 0 : 1;
            ff->is_dir = finfo->attrib & _A_SUBDIR ? 1 : 0;
            ff->size = finfo->size;
            ff->time = finfo->time_write;

            ret = _findnext( handle, finfo );
        }

        _findclose( handle );
        # endif
        string_free( filename );
        string_free( filespec );

        d->files = files;
    }

    /* Special case \ or d:\ : enter it */
    {
        unsigned long len = strlen(d->name);
        if ( len == 1 && d->name[0] == '\\' )
            (*func)( closure, d->name, 1 /* stat()'ed */, d->time );
        else if ( len == 3 && d->name[1] == ':' )
            (*func)( closure, d->name, 1 /* stat()'ed */, d->time );
    }

    /* Now enter contents of directory */
    if ( d->files )
    {
        LIST * files = d->files;
        while ( files )
        {
            file_info_t * ff = file_info( files->string );
            (*func)( closure, ff->name, 1 /* stat()'ed */, ff->time );
            files = list_next( files );
        }
    }

    PROFILE_EXIT( FILE_DIRSCAN );
}

file_info_t * file_query( char * filename )
{
    file_info_t * ff = file_info( filename );
    if ( ! ff->time )
    {
        struct stat statbuf;

        if ( stat( *filename ? filename : ".", &statbuf ) < 0 )
            return 0;

        ff->is_file = statbuf.st_mode & S_IFREG ? 1 : 0;
        ff->is_dir = statbuf.st_mode & S_IFDIR ? 1 : 0;
        ff->size = statbuf.st_size;
        ff->time = statbuf.st_mtime ? statbuf.st_mtime : 1;
    }
    return ff;
}

/*
 * file_time() - get timestamp of file, if not done by file_dirscan()
 */

int
file_time(
    char    *filename,
    time_t  *time )
{
    file_info_t * ff = file_query( filename );
    if ( !ff ) return -1;
    *time = ff->time;
    return 0;
}

int file_is_file(char* filename)
{
    file_info_t * ff = file_query( filename );
    if ( !ff ) return -1;
    return ff->is_file;
}


/*
 * file_archscan() - scan an archive for files
 */

/* Straight from SunOS */

#define ARMAG   "!<arch>\n"
#define SARMAG  8

#define ARFMAG  "`\n"

struct ar_hdr {
    char    ar_name[16];
    char    ar_date[12];
    char    ar_uid[6];
    char    ar_gid[6];
    char    ar_mode[8];
    char    ar_size[10];
    char    ar_fmag[2];
};

# define SARFMAG 2
# define SARHDR sizeof( struct ar_hdr )

void
file_archscan(
    char *archive,
    scanback func,
    void *closure )
{
    struct ar_hdr ar_hdr;
    char *string_table = 0;
    char buf[ MAXJPATH ];
    long offset;
    int fd;

    if ( ( fd = open( archive, O_RDONLY | O_BINARY, 0 ) ) < 0 )
        return;

    if ( read( fd, buf, SARMAG ) != SARMAG ||
        strncmp( ARMAG, buf, SARMAG ) )
    {
        close( fd );
        return;
    }

    offset = SARMAG;

    if ( DEBUG_BINDSCAN )
        printf( "scan archive %s\n", archive );

    while ( ( read( fd, &ar_hdr, SARHDR ) == SARHDR ) &&
           !memcmp( ar_hdr.ar_fmag, ARFMAG, SARFMAG ) )
    {
        long    lar_date;
        long    lar_size;
        char    *name = 0;
        char    *endname;
        char    *c;

        sscanf( ar_hdr.ar_date, "%ld", &lar_date );
        sscanf( ar_hdr.ar_size, "%ld", &lar_size );

        lar_size = ( lar_size + 1 ) & ~1;

        if (ar_hdr.ar_name[0] == '/' && ar_hdr.ar_name[1] == '/' )
        {
        /* this is the "string table" entry of the symbol table,
        ** which holds strings of filenames that are longer than
        ** 15 characters (ie. don't fit into a ar_name
        */

        string_table = BJAM_MALLOC_ATOMIC(lar_size+1);
        if (read(fd, string_table, lar_size) != lar_size)
            printf("error reading string table\n");
        string_table[lar_size] = '\0';
        offset += SARHDR + lar_size;
        continue;
        }
        else if (ar_hdr.ar_name[0] == '/' && ar_hdr.ar_name[1] != ' ')
        {
            /* Long filenames are recognized by "/nnnn" where nnnn is
            ** the offset of the string in the string table represented
            ** in ASCII decimals.
            */

            name = string_table + atoi( ar_hdr.ar_name + 1 );
            for ( endname = name; *endname && *endname != '\n'; ++endname) {}
        }
        else
        {
            /* normal name */
            name = ar_hdr.ar_name;
            endname = name + sizeof( ar_hdr.ar_name );
        }

        /* strip trailing white-space, slashes, and backslashes */

        while ( endname-- > name )
            if ( !isspace(*endname) && ( *endname != '\\' ) && ( *endname != '/' ) )
                break;
        *++endname = 0;

        /* strip leading directory names, an NT specialty */

        if ( c = strrchr( name, '/' ) )
        name = c + 1;
        if ( c = strrchr( name, '\\' ) )
        name = c + 1;

        sprintf( buf, "%s(%.*s)", archive, endname - name, name );
        (*func)( closure, buf, 1 /* time valid */, (time_t)lar_date );

        offset += SARHDR + lar_size;
        lseek( fd, offset, 0 );
    }

    close( fd );
}

# endif /* NT */
