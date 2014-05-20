#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <stdarg.h>

#include <otf.h>

/* for OTF_Error */
#include <OTF_Errno.h>

#include <jenkins_hash.h>

#include "otfaux.h"

OTFAUX_Thumbnail_Context*
OTFAUX_Thumbnail_create( uint64_t minTime, uint64_t maxTime, uint32_t width )
{
    OTFAUX_Thumbnail_Context* new_context = OTFAUX_State_create();

    if ( !new_context )
        return NULL;

    if ( !OTFAUX_State_setupThumbnail( new_context, minTime, maxTime, width ) )
    {
        OTFAUX_State_destroy( new_context );
        return NULL;
    }

    return new_context;
}


void
OTFAUX_Thumbnail_destroy( OTFAUX_Thumbnail_Context* tn_context )
{
    OTFAUX_State_destroy( tn_context );
}


int
OTFAUX_Thumbnail_declareProcess( OTFAUX_Thumbnail_Context* tn_context, uint64_t process_token )
{
    return OTFAUX_State_declareProcess( tn_context,
                                        process_token,
                                        1 );
}


int
OTFAUX_Thumbnail_handleEnter( OTFAUX_Thumbnail_Context* tn_context,
                              uint64_t timestamp,
                              uint64_t process_token,
                              uint32_t function_token )
{
    return OTFAUX_State_processEnter( tn_context,
                                      timestamp,
                                      process_token,
                                      function_token,
                                      0,
                                      NULL );
}


int
OTFAUX_Thumbnail_handleLeave( OTFAUX_Thumbnail_Context* tn_context,
                              uint64_t timestamp,
                              uint64_t process_token )
{
    return OTFAUX_State_processLeave( tn_context,
                                      timestamp,
                                      process_token,
                                      0 );
}


int
OTFAUX_Thumbnail_finalize( OTFAUX_Thumbnail_Context* tn_context )
{
    ( void )tn_context;
    /* finaliztion happens when writing */
     return 1;
}


char*
OTFAUX_Thumbnail_getFilename( const char* namestub )
{
    int length;
    char* name_buffer;

    if (!namestub)
        return NULL;

    length = strlen( namestub ) + strlen( ".thumb" ) + 1;
    name_buffer = (char*)malloc( length * sizeof(char));
    if (!name_buffer)
        return NULL;

    strcpy( name_buffer, namestub );
    strcat( name_buffer, ".thumb" );

    return name_buffer;
}


int
OTFAUX_Thumbnail_write( const OTFAUX_Thumbnail_Context* tn_context,
                        const char* namestub,
                        int create, ... )
{
    uint32_t total_number_of_procs = 0;

    if ( create ) {
        va_list args;
        va_start( args, create );
        total_number_of_procs = va_arg( args, uint32_t );
        va_end( args );
    }

    return OTFAUX_State_writeThumbnail( ( OTFAUX_State* )tn_context,
                                        namestub,
                                        create,
                                        total_number_of_procs );
}


struct OTFAUX_ThumbnailReader
{
    FILE* file;
    uint32_t width;
    uint32_t nprocs;
};


OTFAUX_ThumbnailReader*
OTFAUX_ThumbnailReader_open( const char* namestub )
{
    OTFAUX_ThumbnailReader* new_reader;
    char* filename;

    if (!namestub)
        return NULL;

    new_reader = calloc( 1, sizeof(*new_reader));
    if (!new_reader)
        return NULL;

    filename = OTFAUX_Thumbnail_getFilename( namestub );
    if (!filename) {
        free(new_reader);
        return NULL;
    }

    new_reader->file = fopen( filename, "r" );
    free(filename);
    if (!new_reader->file) {
        free(new_reader);
        return NULL;
    }

    /* read header */
    if (2 != fscanf( new_reader->file, "0:%x,%x\n",
                     &new_reader->width,
                     &new_reader->nprocs )) {
        fclose(new_reader->file);
        free(new_reader);
        return NULL;
    }

    return new_reader;
}


int
OTFAUX_ThumbnailReader_close( OTFAUX_ThumbnailReader* tn_reader )
{
    if (!tn_reader)
        return 0;

    fclose( tn_reader->file );
    free( tn_reader );

    return 1;
}


int
OTFAUX_ThumbnailReader_getWidth( const OTFAUX_ThumbnailReader* tn_reader,
                                 uint32_t* width )
{
    if (!tn_reader || !width)
        return 0;

    *width = tn_reader->width;

    return 1;
}


int
OTFAUX_ThumbnailReader_getNumberOfProcs( const OTFAUX_ThumbnailReader* tn_reader,
                                         uint32_t* nprocs )
{
    if (!tn_reader || !nprocs)
        return 0;

    *nprocs = tn_reader->nprocs;

    return 1;
}


int
OTFAUX_ThumbnailReader_read( OTFAUX_ThumbnailReader* tn_reader,
                             void (* handler)( void*,
                                               uint64_t /* process token */,
                                               const uint32_t* /* function tokens */ ),
                             void* data )
{
    int ret = 0;

    unsigned long long process;
    uint32_t* functions;
    int status;
    uint32_t i = 0, j = 0;

    if (!tn_reader || !tn_reader->file)
        return 0;

    functions = calloc( tn_reader->width, sizeof(*functions));

    /* loop processes */
    status = 1;
    for (i = 0; i < tn_reader->nprocs; i++)
    {
        char comma;
        status = fscanf( tn_reader->file, "%llx:", &process );
        if (1 != status)
            goto out;
        for (j = 0; j < tn_reader->width; ++j)
        {
            status = fscanf( tn_reader->file, "%x%c", &functions[j], &comma );
            if ( 2 != status || comma != ',' )
                goto out;
        }
        if (handler)
        {
            handler( data, process, functions );
        }

        if ( fgetc( tn_reader->file ) != '\n' )
        {
            goto out;
        }
    }

    if ( fgetc( tn_reader->file ) == EOF )
    {
        ret = ( i == tn_reader->nprocs
                && j == tn_reader->width
                && feof( tn_reader->file ) );
    }

out:
    free( functions );

    return ret;
}
