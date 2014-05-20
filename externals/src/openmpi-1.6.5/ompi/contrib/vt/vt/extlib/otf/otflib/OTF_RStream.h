/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/* NOTE
the OTF_RStream interface is not able to perform jumps to random timestamps,
and cannot quit at a certain maximum time stamp.
Maybe in future versions we add this. 
It is not in, because the OTF_Reader interface uses the buffer-methodes directly
to set the timestamps, so saving it in the stream too will be worthless in this
case. */

/** 
 *  @file OTF_RStream.h
 *
 *  @brief Provides read access to trace streams, which consist of multiple
 *  buffers.
 *
 *  \ingroup rstream
 */

/** \defgroup rstream Stream Reader Interface
 *
 * rstream provides an interface for dealing with a single stream of a trace.
 * A stream consists of up to four different buffers (event buffer,
 * definition buffer, snapshots buffer, statistics buffer).
 *
 * rstream is structured similarly to the reader, but it is not
 * able to perform jumps to random timestamps, and cannot quit at a certain
 * maximum time stamp.
 *
 * \section rstream_example A simple Example
 *
 * Common includes
 * \code
 * #include <stdio.h>
 * #include <assert.h>
 * #include "otf.h"
 * \endcode
 *
 *
 * Define the Handler(s).
 * We just want to process the def process event and print out all appearing processes.
 * \code
 * int handleDefProcess (void *userData, uint32_t stream, uint32_t process, const char *name, uint32_t parent) {
 *
 *     printf( "process %u is named \"%s\"\n", process, name );
 *
 *     return OTF_RETURN_OK;
 * }
 * \endcode
 *
 * \code
 * int main( int argc, char** argv ) {
 * \endcode
 *
 *     Declare a file manager, a reader, and a handler array
 *     \code
 *     OTF_RStream* rstream;
 *     OTF_FileManager* manager;
 *     OTF_HandlerArray* handlers;
 *     \endcode
 *
 *     Initialize the file manager. Do not open more than 100 files.
 *     \code
 *     manager= OTF_FileManager_open( 100 );
 *     assert( manager );
 *     \endcode
 *
 *     Initialize the handler array.
 *     \code
 *     handlers = OTF_HandlerArray_open();
 *     assert( handlers );
 *     \endcode
 * 
 *     Initialize the rstream object.
 *     \code
 
 *     rstream = OTF_RStream_open( "mytrace", 0, manager );
 *     assert( rstream );
 *     \endcode
 *
 *     Register your callback functions to the handler array.
 *     \code	
 *     OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleDefProcess, OTF_DEFPROCESS_RECORD );
 *     \endcode
 *
 *
 *     Do the actual reading.
 *     \code
 *     OTF_RStream_readDefinitions( rstream, handlers );
 *     \endcode
 *
 *
 *     Clean everything up before exiting the program.
 *     \code
 *     OTF_RStream_close( rstream );
 *     OTF_HandlerArray_close( handlers );
 *     OTF_FileManager_close( manager );
 *
 *     return 0;
 * }
 * \endcode
 *
 * Compile and link this using $ gcc -o test test.c `otfconfig --libs`.
 *
 */


#ifndef OTF_RSTREAM_H
#define OTF_RSTREAM_H


#include <stdlib.h>
#include <string.h>
#include <stdio.h>


#include "OTF_inttypes.h"


#include "OTF_FileManager.h"
#include "OTF_RBuffer.h"
#include "OTF_Filenames.h"
#include "OTF_HandlerArray.h"


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

struct struct_OTF_RStream {


	/**	name stub: all files will begin with this name */
	char* namestub;

	/**	Unique id for the current stream */
	uint32_t id;

	/**	Definitions buffer. Definitions buffer carries definition
		records */
	OTF_RBuffer* defBuffer;

	/**	Event buffer. The event buffer carries records for actual
		events, i.e. records with a time stamp */
	OTF_RBuffer* eventBuffer;

	/**	Snaps (snapshots) buffer. The snapshots buffer carries
		snapshots of the whole state at a point in time - as oppossed to
		events which only show changes in the state. This can be used to
		start from such a snapshot instead of from the very begining. */
	OTF_RBuffer* snapsBuffer;

	/**	Statistics buffer. Statistics buffer carries statistical
		information about a certain time interval resp. data at
		points of time that allow to derive statistics without
		reading through all events of that interval. */
	OTF_RBuffer* statsBuffer;

	/**	Marker buffer. */
	OTF_RBuffer* markerBuffer;

	/** Default size of buffers managed by this RStream. */
	uint32_t buffersizes;
	
#ifdef HAVE_ZLIB
	/** Default size of zbuffers managed by this RStream. */
	uint32_t zbuffersizes;
#endif /* HAVE_ZLIB */
	
	/** file handle manager */
	OTF_FileManager* manager;
	
	/** maximum number of records delivered by a single call to OTF_Reader_readXYZ()
	defaults to OTF_READ_MAXRECORDS == \infty */
	uint64_t recordLimit;
};
/** rstream object \ingroup rstream */
typedef struct struct_OTF_RStream OTF_RStream;


/**     
 * Create a new OTF_RStream instance.
 *
 * @param  nameStub     File name prefix which is going to be used by 
 *                      all sub-files which belong to the reader stream.
 * @param  id           Abitrary but unique identifier of the reader stream.
 *                      Must be > '0' for real streams. Use '0' for global definitions.
 * @param  manager      File handle manager. 
 *
 * @return              Initialized OTF_RStream instance or 0 if an error
 *                      occurred.
 *
 * \ingroup rstream
 */
OTF_RStream* OTF_RStream_open( const char* nameStub, uint32_t id, OTF_FileManager* manager );


/** 
 * Close an OTF_RStream instance and all its related files.
 *
 * @param rstream  Pointer to an initialized OTF_RStream object. See 
 *                 also OTF_RStream_open().
 * 
 * @return         1 if instance was closed successfully and 0 otherwise.
 *
 * \ingroup rstream
 */
int OTF_RStream_close( OTF_RStream* rstream );


/** 
 * Returns the definition buffer of the according reader stream.
 *
 * @param rstream  Pointer to an initialized OTF_RStream object. See 
 *                 also OTF_RStream_open().
 * 
 * @return         Initialized OTF_RBuffer instance or 0 if an error occured.
 *
 * \ingroup rstream
 */
OTF_RBuffer* OTF_RStream_getDefBuffer( OTF_RStream* rstream );


/** 
 * Forces the given definition buffer to the according reader stream.
 *
 * @param rstream  Pointer to an initialized OTF_RStream object. See 
 *                 also OTF_RStream_open().
 * @param rbuffer  The OTF_RBuffer to use.
 * 
 * @return         The previously used Buffer or NULL if none.
 *
 * \ingroup rstream
 */
OTF_RBuffer* OTF_RStream_setDefBuffer( OTF_RStream* rstream, OTF_RBuffer* rbuffer );


/**
 * Closes the stream definition buffer.
 *
 * @param rstream  Pointer to an initialized OTF_RStream object. See 
 *                 also OTF_RStream_open().
 *
 * @return         1 on success, 0 if an error occurs
 *
 * \ingroup rstream
 */
int OTF_RStream_closeDefBuffer( OTF_RStream* rstream );


/** 
 * Returns the event buffer of the according reader stream.
 *
 * @param rstream  Pointer to an initialized OTF_RStream object. See 
 *                 also OTF_RStream_open().
 * 
 * @return         Initialized OTF_RBuffer instance or 0 if an error occured.
 *
 * \ingroup rstream
 */
OTF_RBuffer* OTF_RStream_getEventBuffer( OTF_RStream* rstream );


/**
 * Closes the stream event buffer.
 *
 * @param rstream  Pointer to an initialized OTF_RStream object. See 
 *                 also OTF_RStream_open().
 *
 * @return         1 on success, 0 if an error occurs
 *
 * \ingroup rstream
 */
int OTF_RStream_closeEventBuffer( OTF_RStream* rstream );


/** 
 * Returns the snapshots buffer of the according reader stream.
 *
 * @param rstream  Pointer to an initialized OTF_RStream object. See 
 *                 also OTF_RStream_open().
 * 
 * @return         Initialized OTF_RBuffer instance or 0 if an error occured.
 *
 * \ingroup rstream
 */
OTF_RBuffer* OTF_RStream_getSnapsBuffer( OTF_RStream* rstream );


/**
 * Closes the stream snapshots buffer.
 *
 * @param rstream  Pointer to an initialized OTF_RStream object. See 
 *                 also OTF_RStream_open().
 *
 * @return         1 on success, 0 if an error occurs
 *
 * \ingroup rstream
 */
int OTF_RStream_closeSnapsBuffer( OTF_RStream* rstream );


/** 
 * Returns the statistics buffer of the according reader stream.
 *
 * @param rstream  Pointer to an initialized OTF_RStream object. See 
 *                 also OTF_RStream_open().
 * 
 * @return         Initialized OTF_RBuffer instance or 0 if an error occured.
 *
 * \ingroup rstream
 */
OTF_RBuffer* OTF_RStream_getStatsBuffer( OTF_RStream* rstream );

/**
 * Closes the stream statistics buffer.
 *
 * @param rstream  Pointer to an initialized OTF_RStream object. See 
 *                 also OTF_RStream_open().
 *
 * @return         1 on success, 0 if an error occurs
 *
 * \ingroup rstream
 */
int OTF_RStream_closeStatsBuffer( OTF_RStream* rstream );


/** 
 * Returns the marker buffer of the according reader stream.
 *
 * @param rstream  Pointer to an initialized OTF_RStream object. See 
 *                 also OTF_RStream_open().
 * 
 * @return         Initialized OTF_RBuffer instance or 0 if an error occured.
 *
 * \ingroup rstream
 */
OTF_RBuffer* OTF_RStream_getMarkerBuffer( OTF_RStream* rstream );


/**
 * Closes the stream marker buffer.
 *
 * @param rstream  Pointer to an initialized OTF_RStream object. See 
 *                 also OTF_RStream_open().
 *
 * @return         1 on success, 0 if an error occurs
 *
 * \ingroup rstream
 */
int OTF_RStream_closeMarkerBuffer( OTF_RStream* rstream );


/** 
 * Set the default buffer size for all buffers managed by this reader stream. 
 * This is only effective for future buffers and will not change already 
 * allocated buffers. Those can be changed with the buffers directly.
 *
 * @param rstream  Pointer to an initialized OTF_RStream object. See 
 *                 also OTF_RStream_open().
 * 
 * @param size     Intended buffer size.
 *
 * \ingroup rstream
 */
void OTF_RStream_setBufferSizes( OTF_RStream* rstream, uint32_t size );


/** 
 * Get the default buffer size for all buffers managed by this reader stream.
 *
 * @param rstream  Pointer to an initialized OTF_RStream object. See 
 *                 also OTF_RStream_open().
 * 
 * @return         Default buffer size for all buffers managed by this reader
 *                 stream.
 *
 * \ingroup rstream
 */
uint32_t OTF_RStream_getBufferSizes( OTF_RStream* rstream );


/** 
 * Set the default zbuffer size for all files managed by this reader stream.
 * This is only effective for future files and will not change already
 * allocated buffers. Those can be changed with the files directly.
 *
 * @param rstream  Pointer to an initialized OTF_RStream object. See 
 *                 also OTF_RStream_open().
 * 
 * @param size     Intended buffer size.
 *
 * \ingroup rstream
 */
void OTF_RStream_setZBufferSizes( OTF_RStream* rstream, uint32_t size );


/** 
 * Get the default zbuffer size for all files managed by this reader stream.
 *
 * @param rstream  Pointer to an initialized OTF_RStream object. See 
 *                 also OTF_RStream_open().
 * 
 * @return         Default buffer size for all buffers managed by this reader
 *                 stream.
 *
 * \ingroup rstream
 */
uint32_t OTF_RStream_getZBufferSizes( OTF_RStream* rstream );


/**
 * Sets the maximum number of records delivered by a single call to
 * OTF_RStream_readXYZ(). Defaults to OTF_READ_MAXRECORDS == \infty.
 * 'OTF_Reader_readXYZ()' returns with the number of records processed.
 * Successive calls to 'OTF_Reader_readXYZ()' will deliver the remaining
 * records.
 *
 * This function will NOT destroy a pending read operation, i.e. a read
 * operation currently interrupted CAN be continued!
 *
 * @param rstream  Pointer to an initialized OTF_RStream object. See 
 *                 also OTF_RStream_open().
 * @param limit    new record limit. has to be smaller than or equal to
 *                 OTF_READ_MAXRECORDS
 *
 * \ingroup rstream
 */
void OTF_RStream_setRecordLimit( OTF_RStream* rstream, uint64_t limit );


/**
 * Returns the current record limit.
 *
 * @param rstream  Pointer to an initialized OTF_RStream object. See 
 *                 also OTF_RStream_open().
 *
 * @return         Current record limit.
 *
 * \ingroup rstream
 */
uint64_t OTF_RStream_getRecordLimit( OTF_RStream* rstream );



/**
 * Reads all definitions from the stream.
 *
 * @param rstream   Pointer to an initialized OTF_RStream object. See 
 *                  also OTF_RStream_open().
 * @param handlers  Pointer to the handler array.
 *
 * @return          Number of records read or OTF_READ_MAXRECORDS
 *
 * \ingroup rstream
 */
uint64_t OTF_RStream_readDefinitions( OTF_RStream* rstream, 
	OTF_HandlerArray* handlers );


/**
 * Reads all events from the stream and calls the appropriated handler sorted
 * by time.
 *
 * @param rstream   Pointer to an initialized OTF_RStream object. See 
 *                  also OTF_RStream_open().
 * @param handlers  Pointer to the handler array.
 *
 * @return          Number of records read or OTF_READ_MAXRECORDS
 *
 * \ingroup rstream
 */
uint64_t OTF_RStream_readEvents( OTF_RStream* rstream, OTF_HandlerArray* handlers );


/**
 * Reads all snapshots from the stream.
 *
 * @param rstream   Pointer to an initialized OTF_RStream object. See 
 *                  also OTF_RStream_open().
 * @param handlers  Pointer to the handler array.
 *
 * @return          Number of records read or OTF_READ_MAXRECORDS
 *
 * \ingroup rstream
 */
uint64_t OTF_RStream_readSnapshots( OTF_RStream* rstream, OTF_HandlerArray* handlers );


/**
 * Reads all statistics from the stream.
 *
 * @param rstream   Pointer to an initialized OTF_RStream object. See 
 *                  also OTF_RStream_open().
 * @param handlers  Pointer to the handler array.
 *
 * @return          Number of records read or OTF_READ_MAXRECORDS
 *
 * \ingroup rstream
 */
uint64_t OTF_RStream_readStatistics( OTF_RStream* rstream, OTF_HandlerArray* handlers );


/**
 * Reads all markers from the stream.
 *
 * @param rstream   Pointer to an initialized OTF_RStream object. See 
 *                  also OTF_RStream_open().
 * @param handlers  Pointer to the handler array.
 *
 * @return          Number of records read or OTF_READ_MAXRECORDS
 *
 * \ingroup rstream
 */
uint64_t OTF_RStream_readMarker( OTF_RStream* rstream, OTF_HandlerArray* handlers );


/** depricated. @see OTF_RStream_eventTimeProgress() \ingroup rstream */
uint8_t OTF_RStream_eventProgress( OTF_RStream* rstream, uint64_t* minimum,
	uint64_t* current, uint64_t* maximum );
	
/** depricated. @see OTF_RStream_snapshotTimeProgress() \ingroup rstream */
uint8_t OTF_RStream_snapshotProgress( OTF_RStream* rstream,
		uint64_t* minimum, uint64_t* current, uint64_t* maximum );
		
/** depricated. @see OTF_RStream_statisticTimeProgress() \ingroup rstream */
uint8_t OTF_RStream_statisticProgress( OTF_RStream* rstream,
	uint64_t* minimum, uint64_t* current, uint64_t* maximum );
	
	
/**
 * Delivers a progress report for reading events. It is given in terms
 * of time stamps. A percentage can be computed as 
 * ( current - minimum ) / ( maximum - minimum ). 
 * This computation takes restricted time intervals into account which is not 
 * possible with OTF_RStream_eventBytesProgress().
 *
 * The progress report is only valid after one or several calls to
 * OTF_RStream_readEvents(). Otherwise the return arguments 'minimum', 'current' 
 * and 'maximum' are undefined! 
 * If 'minimum' > 'maximum' the values are invalid.
 *
 * @param rstream    Pointer to an initialized OTF_RStream object. See 
 *                   also OTF_RStream_open().
 * @param minimum    Return value for the minium time.
 * @param current    Return value for the current time.
 * @param maximum    Return value for the maximum time.
 *
 * @return           1 on success, 0 if an error occurs.
 *
 * \ingroup rstream
 */
uint8_t OTF_RStream_eventTimeProgress( OTF_RStream* rstream, uint64_t* minimum,
	uint64_t* current, uint64_t* maximum );


/**
 * Delivers a progress report for reading snapshots. It is given in terms
 * of time stamps. A percentage can be computed as 
 * ( current - minimum ) / ( maximum - minimum ). 
 * This computation takes restricted time intervals into account which is not 
 * possible with OTF_RStream_snapshotBytesProgress().
 *
 * The progress report is only valid after one or several calls to
 * OTF_RStream_readSnapshots(). Otherwise the return arguments 'minimum', 'current' 
 * and 'maximum' are undefined! 
 * If 'minimum' > 'maximum' the values are invalid.
 *
 * @param rstream    Pointer to an initialized OTF_RStream object. See 
 *                   also OTF_RStream_open().
 * @param minimum    Return value for the minium time.
 * @param current    Return value for the current time.
 * @param maximum    Return value for the maximum time.
 *
 * @return           1 on success, 0 if an error occurs.
 *
 * \ingroup rstream
 */
uint8_t OTF_RStream_snapshotTimeProgress( OTF_RStream* rstream,
		uint64_t* minimum, uint64_t* current, uint64_t* maximum );


/**
 * Delivers a progress report for reading statistics. It is given in terms
 * of time stamps. A percentage can be computed as 
 * ( current - minimum ) / ( maximum - minimum ). 
 * This computation takes restricted time intervals into account which is not 
 * possible with OTF_Reader_statisticBytesProgress().
 *
 * The progress report is only valid after one or several calls to
 * OTF_Reader_readStatistics(). Otherwise the return arguments 'minimum', 'current' 
 * and 'maximum' are undefined! 
 * If 'minimum' > 'maximum' the values are invalid.
 *
 * @param rstream    Pointer to an initialized OTF_RStream object. See 
 *                   also OTF_RStream_open().
 * @param minimum    Return value for the minium time.
 * @param current    Return value for the current time.
 * @param maximum    Return value for the maximum time.
 *
 * @return           1 on success, 0 if an error occurs.
 *
 * \ingroup rstream
 */
uint8_t OTF_RStream_statisticTimeProgress( OTF_RStream* rstream,
	uint64_t* minimum, uint64_t* current, uint64_t* maximum );


/**
 * Delivers a progress report for reading events. Progress is given in terms
 * of bytes. The percentage can be computed as ( current - minimum ) / ( maximum - minimum ). 
 * 
 * ATTENTION: This in only a rough estimate of the progress, because it is
 * computed based on the block I/O from files but not based on the actual bytes 
 * processed. This may result in constant values for small traces.
 * See also OTF_RStream_eventTimeProgress():
 *
 * The progress report is only valid after one or several calls to
 * OTF_RStream_readEvents(). Otherwise the return arguments 'minimum', 'current' and 'maximum' are
 * undefined! If 'minimum' > 'maximum' the values are invalid.
*
 * @param rstream    Pointer to an initialized OTF_RStream object. See 
 *                   also OTF_RStream_open().
 * @param minimum    Return value for the minium bytes read ( is 0 everytime ).
 * @param current    Return value for the current bytes read.
 * @param maximum    Return value for the filesize.
 *
 * @return           1 on success, 0 if an error occurs.
 *
 * \ingroup rstream
 */
uint8_t OTF_RStream_eventBytesProgress( OTF_RStream* rstream, uint64_t* minimum,
	uint64_t* current, uint64_t* maximum );


/**
 * Delivers a progress report for reading snapshots. Progress is given in terms
 * of bytes. The percentage can be computed as ( current - minimum ) / ( maximum - minimum ). 
 * 
 * ATTENTION: This in only a rough estimate of the progress, because it is
 * computed based on the block I/O from files but not based on the actual bytes 
 * processed. This may result in constant values for small traces.
 * See also OTF_RStream_snapshotTimeProgress():
 *
 * The progress report is only valid after one or several calls to
 * OTF_RStream_readSnapshots(). Otherwise the return arguments 'minimum', 'current' and 'maximum' are
 * undefined! If 'minimum' > 'maximum' the values are invalid.
 *
 * @param rstream    Pointer to an initialized OTF_RStream object. See 
 *                   also OTF_RStream_open().
 * @param minimum    Return value for the minium bytes read ( is 0 everytime ).
 * @param current    Return value for the current bytes read.
 * @param maximum    Return value for the filesize.
 *
 * @return           1 on success, 0 if an error occurs.
 *
 * \ingroup rstream
 */
uint8_t OTF_RStream_snapshotBytesProgress( OTF_RStream* rstream,
		uint64_t* minimum, uint64_t* current, uint64_t* maximum );

/**
 * Delivers a progress report for reading statistics. Progress is given in terms
 * of bytes. The percentage can be computed as ( current - minimum ) / ( maximum - minimum ). 
 * 
 * ATTENTION: This in only a rough estimate of the progress, because it is
 * computed based on the block I/O from files but not based on the actual bytes 
 * processed. This may result in constant values for small traces.
 * See also OTF_RStream_statisticTimeProgress():
 *
 * The progress report is only valid after one or several calls to
 * OTF_RStream_readStatistics(). Otherwise the return arguments 'minimum', 'current' and 'maximum' are
 * undefined! If 'minimum' > 'maximum' the values are invalid.
 *
 * @param rstream    Pointer to an initialized OTF_RStream object. See 
 *                   also OTF_RStream_open().
 * @param minimum    Return value for the minium bytes read ( is 0 everytime ).
 * @param current    Return value for the current bytes read.
 * @param maximum    Return value for the filesize.
 *
 * @return           1 on success, 0 if an error occurs.
 *
 * \ingroup rstream
 */
uint8_t OTF_RStream_statisticBytesProgress( OTF_RStream* rstream,
	uint64_t* minimum, uint64_t* current, uint64_t* maximum );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_RSTREAM_H */
