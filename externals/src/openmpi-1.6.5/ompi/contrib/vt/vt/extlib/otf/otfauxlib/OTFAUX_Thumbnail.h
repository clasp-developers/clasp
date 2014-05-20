#ifndef OTFAUX_THUMBNAIL_H
#define OTFAUX_THUMBNAIL_H

#include <otf.h>

#include <OTFAUX_State.h>

/**
 *  @file otfauxlib/OTFAUX_Thumbnail.h
 *
 *  @brief Provides a module to collect data for thumbnail generation.
 */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/**
 * @defgroup thumbnail Module for thumbnail generation.
 *
 * @usage:
 *
 *  ctx = OTFAUX_Thumbnail_Create(minTime, maxTime, 1024);
 *
 *  announce all interesting processes:
 *      OTFAUX_Thumbnail_declareProcess(ctx, ...);
 *
 *  repeatedly call for interesting processes:
 *      OTFAUX_Thumbnail_handleEnter(ctx, ...);
 *      OTFAUX_Thumbnail_handleLeave(ctx, ...);
 *
 *  at end, for all processes:
 *      OTFAUX_ThumbnailData td;
 *      OTFAUX_Thumbnail_getData(ctx, process, &td);
 *      .. do something with td.start_pixel and td.function ..
 *
 *  OTFAUX_Thumbnail_Destroy(ctx);
 *
 * @{
 */

/** Opaque type for using the thumbnail module. */
typedef OTFAUX_State OTFAUX_Thumbnail_Context;

/**
 * Create a context for thumbnail generation.
 *
 * @param minTime   Minimum timestamp of the trace file.
 * @param maxTime   Maximum timestamp of the trace file.
 * @param width     The width in pixels of the thumbnail.
 *
 * @return          The context.
 */
OTFAUX_Thumbnail_Context*
OTFAUX_Thumbnail_create( uint64_t minTime,
                         uint64_t maxTime,
                         uint32_t width );

/**
 * Destroy a context previously created with @a OTFAUX_Thumbnail_Create.
 *
 * @param tn_context   The context.
 */
void
OTFAUX_Thumbnail_destroy( OTFAUX_Thumbnail_Context* tn_context );

/**
 * Declares that the process @a process should be handled by this context.
 *
 * @param tn_context   The context.
 *
 * @return             1 on success.
 */
int
OTFAUX_Thumbnail_declareProcess( OTFAUX_Thumbnail_Context* tn_context,
                                 uint64_t process );

/**
 * Declare that the process @a process has entered the fucntion @a function
 * at timestamp @a timestamp.
 *
 * This function needs to be called in monotonically increasing timestamp order.
 *
 * @param tn_context   The context.
 * @param timestamp    The timestamp.
 * @param process      The process.
 * @param function     The function.
 *
 * @return             1 on success.
 */
int
OTFAUX_Thumbnail_handleEnter( OTFAUX_Thumbnail_Context* tn_context,
                              uint64_t timestamp,
                              uint64_t process,
                              uint32_t function );

/**
 * Declare that the process @a process has left the current fucntion at
 * timestamp @a timestamp.
 *
 * This function needs to be called in monotonically increasing timestamp order.
 *
 * @param tn_context   The context.
 * @param timestamp    The timestamp.
 * @param process      The process.
 *
 * @return             1 on success.
 */
int
OTFAUX_Thumbnail_handleLeave( OTFAUX_Thumbnail_Context* tn_context,
                              uint64_t timestamp,
                              uint64_t process );

/**
 * Declare that the handling of the enter and leave events is over.
 *
 * @param tn_context    The context.
 *
 * @return             1 on success.
 */
int
OTFAUX_Thumbnail_finalize( OTFAUX_Thumbnail_Context* tn_context );

char*
OTFAUX_Thumbnail_getFilename( const char* namestub );

/**
 * Writes the processes data of the context to a file.
 *
 * The writing is designed so that the data of multuiple contexts can be
 * written to one file to form a thumbnail. The @a create parameter alows
 * this. The writing of the first context should set the @a create
 * parameter and provide in the variable argument list the total number of
 * processes which will be written, over all comming contexts as an uint32_t.
 * If the @a create parameter is not set, no file will be created and no
 * header will be written, only the data from the given context will be
 * appended to the file. The width of the file should match the width of this
 * context.
 *
 * @param tn_context    The context.
 * @param namestub      The name of the file.
 * @param create        Create the thumb file, or append.
 * @param ...           The total number of processes as an uint32_t, if @a
 *                      create is set.
 *
 * @return             1 on success.
 */
int
OTFAUX_Thumbnail_write( const OTFAUX_Thumbnail_Context* tn_context,
                        const char* namestub,
                        int create, ... );

/**
 * @defgroup thumbnailreader Module to read a thumbnail.
 */

typedef struct OTFAUX_ThumbnailReader OTFAUX_ThumbnailReader;

OTFAUX_ThumbnailReader*
OTFAUX_ThumbnailReader_open( const char* namestub );

int
OTFAUX_ThumbnailReader_close( OTFAUX_ThumbnailReader* tn_reader );

int
OTFAUX_ThumbnailReader_getWidth( const OTFAUX_ThumbnailReader* tn_reader,
                                 uint32_t* width );

int
OTFAUX_ThumbnailReader_getNumberOfProcs( const OTFAUX_ThumbnailReader* tn_reader,
                                         uint32_t* nprocs );

int
OTFAUX_ThumbnailReader_read( OTFAUX_ThumbnailReader* tn_reader,
                             void (* process_handler)( void*,
                                                       uint64_t,
                                                       const uint32_t* ),
                             void* data );

/**
 * @}
 */

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTFAUX_THUMBNAIL_H */
