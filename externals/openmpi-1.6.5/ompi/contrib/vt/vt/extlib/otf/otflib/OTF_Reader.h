/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_Reader.h
 *
 *  @brief Transparently reads OTF traces which consist of multiple streams.
 *
 *  This interface should be used whenever a trace file is to be read as a
 *  whole.
 *
 *  \ingroup reader
 */


#ifndef OTF_READER_H
#define OTF_READER_H


#include "OTF_inttypes.h"


#include "OTF_MasterControl.h"
#include "OTF_FileManager.h"
#include "OTF_RStream.h"
#include "OTF_HandlerArray.h"


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/** \defgroup reader Reader Interface
 *
 * The reader provides high level read access to traces
 * disregarding the presence of streams.
 *
 * \section reader_example1 A simple Example
 *
 * \code
 * #include <stdio.h>
 * #include <assert.h>
 * #include "otf.h"
 * \endcode
 *
 * Define handlers/callbacks for the records you want to read.
 * \code
 * int handleEnter (void *userData, uint64_t time, uint32_t function, uint32_t process, uint32_t source) {
 *
 *     printf("we just entered function %u\n", function); 
 *
 *     return OTF_RETURN_OK;
 * }
 *
 * int handleLeave (void *userData, uint64_t time, uint32_t function, uint32_t process, uint32_t source) {
 *
 *     printf("byebye\n");
 *
 *     return OTF_RETURN_OK;
 * }
 * \endcode
 *
 * \code
 * int main( int argc, char** argv ) {
 * \endcode
 *
 *     Declare a file manager, a reader, and a handler array.
 *     \code
 *     OTF_FileManager* manager;
 *     OTF_Reader* reader;
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
 *     Initialize the reader.
 *     \code
 *     reader = OTF_Reader_open( "mytrace", manager );
 *     assert( reader );
 *     \endcode
 *
 *     Register your callback functions to the handler array.
 *     \code	
 *     OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleEnter, OTF_ENTER_RECORD );
 *     OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleLeave, OTF_LEAVE_RECORD );
 *     \endcode
 *
 *
 *     Do the actual reading.
 *     \code
 *     OTF_Reader_readEvents( reader, handlers );
 *     \endcode
 *
 *
 *     Clean everything up before exiting the program.
 *     \code
 *     OTF_Reader_close( reader );
 *     OTF_HandlerArray_close( handlers );
 *     OTF_FileManager_close( manager );
 *
 *     return 0;
 * }
 * \endcode
 *
 * Compile and link this using $ gcc -o test test.c `otfconfig --libs`.
 *
 *
 * \section reader_example2 A second, more complex Example
 *
 * Same as before
 * \code
 * #include <stdio.h>
 * #include <assert.h>
 * #include "otf.h"
 * \endcode
 *
 * Create a structure, which holds information needed in every handler.
 * We will register this structure to the handlers, so that the userData pointer
 * in every handler will point to it.
 * In this example we just want to count the occurences.
 * \code
 * typedef struct {
 *     uint64_t count;
 * } HandlerArgument;
 * \endcode
 *
 * Define four handlers.
 * In every handler we will increase HandlerArgument::count.
 * \code
 * int handleDefProcess (void *userData, uint32_t stream, uint32_t process, const char *name, uint32_t parent) {
 *     ((HandlerArgument*)userData)->count++;
 *     return OTF_RETURN_OK;
 * }
 * int handleDefFunction (void *userData, uint32_t stream, uint32_t func, const char *name, uint32_t funcGroup, uint32_t source) {
 *     ((HandlerArgument*)userData)->count++;
 *     return OTF_RETURN_OK;
 * }
 * int handleEnter (void *userData, uint64_t time, uint32_t function, uint32_t process, uint32_t source) {
 *     ((HandlerArgument*)userData)->count++;
 *     return OTF_RETURN_OK;
 * }
 * int handleLeave (void *userData, uint64_t time, uint32_t function, uint32_t process, uint32_t source) {
 *     ((HandlerArgument*)userData)->count++;
 *     return OTF_RETURN_OK;
 * }
 * \endcode
 *
 *
 * Same as before
 * \code
 * int main( int argc, char** argv ) {
 *
 *     OTF_FileManager* manager;
 *     OTF_Reader* reader;
 *     OTF_HandlerArray* handlers;
 * \endcode
 *
 *     We need some additional variables for the read progress
 *     \code
 *     uint64_t minbytes;
 *     uint64_t curbytes;
 *     uint64_t maxbytes;
 *     \endcode
 *
 *     \code
 *     uint64_t ret;
 *     HandlerArgument ha;
 *     ha.count = 0;
 *
 *     manager= OTF_FileManager_open( 100 );
 *     assert( manager );
 *
 *     handlers = OTF_HandlerArray_open();
 *     assert( handlers );
 *
 *     reader = OTF_Reader_open( "mytrace", manager );
 *     assert( reader );
 *     \endcode
 *
 *     Register handlers for define process records,
 *     define function records, enter records and leave records
 *     
 *     \code
 *     OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleDefProcess, OTF_DEFPROCESS_RECORD );
 *     \endcode
 *     Register the first handler argument ha to the handler where it should be passed into.
 *     \code
 *     OTF_HandlerArray_setFirstHandlerArg( handlers, &ha, OTF_DEFPROCESS_RECORD );
 *     \endcode
 *
 *     \code
 *     OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleDefFunction, OTF_DEFFUNCTION_RECORD );
 *     OTF_HandlerArray_setFirstHandlerArg( handlers, &ha, OTF_DEFFUNCTION_RECORD );
 *
 *     OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleEnter, OTF_ENTER_RECORD );
 *     OTF_HandlerArray_setFirstHandlerArg( handlers, &ha, OTF_ENTER_RECORD );
 *
 *     OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleLeave, OTF_LEAVE_RECORD );
 *     OTF_HandlerArray_setFirstHandlerArg( handlers, &ha, OTF_LEAVE_RECORD );
 *     \endcode
 *
 *     Read definitions ( .def files).
 *     Inside this function the defProcess and defFunction handler will be called.
 *     \code
 *     OTF_Reader_readDefinitions( reader, handlers );
 *     \endcode
 *
 *     Set the record limit to zero and read the events once.
 *     This initializes internal datastructures needed for getting the reading progress.
 *     \code
 *     OTF_Reader_setRecordLimit( reader, 0 );
 *     OTF_Reader_readEvents( reader, handlers );
 *     \endcode
 *     
 *     To leave OTF_Reader_readEvents() once in a while, in order to update the progress,
 *     set the record limit to an appropriate number. 100000 in this case.
 * 
 *     \code
 *     OTF_Reader_setRecordLimit( reader, 100000 );
 *     \endcode
 * 
 *     Read the trace until no records are left.
 *     \code
 *     while ( 0 != ( ret= OTF_Reader_readEvents( reader, handlers ) ) ) {
 *     \endcode
 * 
 *     If an error occurs, leave the program.
 *     \code
 *         if( OTF_READ_ERROR == ret ) {
 *             fprintf( stderr, "Error while reading events. Aborting\n" );
 *
 *             OTF_Reader_close( reader );
 *             OTF_HandlerArray_close( handlers );
 *             OTF_FileManager_close( manager );
 *
 *             exit(1);
 *         }
 *
 *     \endcode
 *     Update the progress.
 *     \code
 *         OTF_Reader_eventBytesProgress( reader, &minbytes, &curbytes, &maxbytes );
 *         printf( "%llub / %llub\n", (long long unsigned)(curbytes - minbytes), (long long unsigned)(maxbytes-minbytes) );
 *     \endcode
 *
 *     \code
 *     }
 *     \endcode
 *     Print out the gathered count of occurences of the four record types.
 *     \code
 *     printf( "count: %llu\n", (long long unsigned) ha.count );
 *     \endcode
 *
 *     Finish everything
 *     \code
 *     OTF_Reader_close( reader );
 *     OTF_HandlerArray_close( handlers );
 *     OTF_FileManager_close( manager );
 *
 *     return 0;
 * }
 * \endcode
 * Compile and link this using $ gcc -o test test.c `otfconfig --libs`.
 * 
 * When executing this test the output will be something like this:
 * \verbatim
 *   4194304b / 73530754b
 *   [..]
 *   73530754b / 73530754b
 *   count: 4582694 \endverbatim
 *
 */

/** reader object \ingroup reader */
typedef struct struct_OTF_Reader OTF_Reader;


/**
 * Open a MasterControl file and return a OTF_Reader.
 *
 * @param  namestub     File name prefix which is going to be used by 
 *                      all sub-files which belong to the trace.
 * @param  fileManager  File handle manager. 
 *
 * @return              Initialized OTF_Reader instance or 0 if a failure
 *                      occurred.
 *
 * \ingroup reader
 */
OTF_Reader* OTF_Reader_open( const char* namestub, OTF_FileManager* manager );

/** 
 * Set the default buffer size for all buffers managed by this Reader. 
 * This is only effective for future buffers and will not change already 
 * allocated buffers. Those can be changed with the buffers directly.
 *
 * @param reader  Pointer to an initialized OTF_Reader object. See 
 *                also OTF_Reader_open().
 * @param size    Intended buffer size.
 *
 * @return        1 on success, 0 if an error occurs.
 *
 * \ingroup reader
 */
int OTF_Reader_setBufferSizes( OTF_Reader* reader, uint32_t size );

/** 
 * Get the default buffer size.
 *
 * @param reader  Pointer to an initialized OTF_Reader object. See 
 *                also OTF_Reader_open().
 * 
 * @return        Default buffer size.
 *
 * \ingroup reader
 */
uint32_t OTF_Reader_getBufferSizes( OTF_Reader* reader );

/**
 * Set the default zbuffer size for all buffers managed by this Reader.
 * This is only effective for future files and will not change already
 * allocated zbuffers. Those can be changed with the files directly.
 *
 * @param reader  Pointer to an initialized OTF_Reader object. See 
 *                also OTF_Reader_open().
 * 
 * @param size    Intended zbuffer size.
 *
 * \ingroup reader
 */
void OTF_Reader_setZBufferSizes( OTF_Reader* reader, uint32_t size );

/** 
 * Get the default zbuffer size.
 *
 * @param reader  Pointer to an initialized OTF_Reader object. See 
 *                also OTF_Reader_open().
 * 
 * @return  zbuffer size.
 *
 * \ingroup reader
 */
uint32_t OTF_Reader_getZBufferSizes( OTF_Reader* reader );

/**
 * Get a pointer to the mastercontrol of the reader
 *
 * @param reader  Pointer to an initialized OTF_Reader object. See 
 *                also OTF_Reader_open().
 *
 * @return        Pointer to the mastercontrol.
 *
 * \ingroup reader
 */
OTF_MasterControl* OTF_Reader_getMasterControl( OTF_Reader* reader );

/** 
 * Close an OTF_Reader instance and all its related files.
 *
 * @param reader  Pointer to an initialized OTF_Reader object. See 
 *                also OTF_Reader_open().
 * 
 * @return        1 if instance was closed successfully and 0 otherwise.       
 *
 * \ingroup reader
 */
int OTF_Reader_close( OTF_Reader* reader );

/**
 * This function reads all definitions from trace.
 *
 * @param reader    Pointer to an initialized OTF_Reader object. See 
 *                  also OTF_Reader_open().
 * @param handlers  Pointer to the handler array.
 *
 * @return          number of records successfully read or OTF_READ_ERROR
 *
 * \ingroup reader
 */
uint64_t OTF_Reader_readDefinitions( OTF_Reader* reader, OTF_HandlerArray* handlers );

/**
 * This function reads all events from trace and calls the appropriate
 * handler sorted by time
 *
 * @param reader    Pointer to an initialized OTF_Reader object. See 
 *                  also OTF_Reader_open().
 * @param handlers  Pointer to the handler array.
 *
 * @return          number of records successfully read or OTF_READ_ERROR
 *
 * \ingroup reader
 */
uint64_t OTF_Reader_readEvents( OTF_Reader* reader, OTF_HandlerArray* handlers );

/**
 * This function reads all events from trace and calls the appropriate handler
 * NOT sorted by time.
 * It calls every handler in ONE stream sorted by time.
 * And walks through the streams one by one.
 * ( So the handlers of one process will be called, sorted by time, too )
 *
 * This function is faster than OTF_Reader_readEvents(), especially for
 * a bigger number of streams
 *
 * @see OTF_Reader_readEvents()
 *
 * \ingroup reader
 */
uint64_t OTF_Reader_readEventsUnsorted( OTF_Reader* reader, OTF_HandlerArray* handlers );

/**
 * This function reads all snapshots from trace
 *
 * @param reader    Pointer to an initialized OTF_Reader object. See 
 *                  also OTF_Reader_open().
 * @param handlers  Pointer to the handler array.
 *
 * @return          number of records successfully read or OTF_READ_ERROR
 *
 * \ingroup reader
 */
uint64_t OTF_Reader_readSnapshots( OTF_Reader* reader, OTF_HandlerArray* handlers );

/**
 * This function reads all snapshots from trace and calls the appropriate handler
 * NOT sorted by time.
 * It calls every handler in ONE stream sorted by time.
 * And it walks through the streams one by one.
 * ( So the handlers of one process will be called, sorted by time, too )
 *
 * This function is faster than OTF_Reader_readSnapshots(), especially for
 * a bigger number of streams
 *
 * @see OTF_Reader_readSnapshots()
 *
 * \ingroup reader
 */
uint64_t OTF_Reader_readSnapshotsUnsorted( OTF_Reader* reader, OTF_HandlerArray* handlers );

/**
 * This function reads all statistic records from trace
 *
 * @param reader    Pointer to an initialized OTF_Reader object. See 
 *                  also OTF_Reader_open().
 * @param handlers  Pointer to the handler array.
 *
 * @return          number of records successfully read or OTF_READ_ERROR
 *
 * \ingroup reader
 */
uint64_t OTF_Reader_readStatistics( OTF_Reader* reader, OTF_HandlerArray* handlers );

/**
 * This function reads all statistics from trace and calls the appropriate handler
 * NOT sorted by time.
 * It calls every handler in ONE stream sorted by time.
 * And it walks through the streams one by one.
 * ( So the handlers of one process will be called, sorted by time, too )
 *
 * This function is faster than OTF_Reader_readStatistics(), especially for
 * a bigger number of streams
 *
 * @see OTF_Reader_readStatistics()
 *
 * \ingroup reader
 */
uint64_t OTF_Reader_readStatisticsUnsorted( OTF_Reader* reader, OTF_HandlerArray* handlers );

/**
 * This function reads all markers from trace.
 *
 * @param reader    Pointer to an initialized OTF_Reader object. See 
 *                  also OTF_Reader_open().
 * @param handlers  Pointer to the handler array.
 *
 * @return          number of records successfully read or OTF_READ_ERROR
 *
 * \ingroup reader
 */
uint64_t OTF_Reader_readMarkers( OTF_Reader* reader, OTF_HandlerArray* handlers );

/**
 * Searchs a reader stream and returns it.
 * If the stream does not exist it will be created.
 *
 * @param reader    Pointer to an initialized OTF_Reader object. See 
 *                  also OTF_Reader_open().
 * @param id        Identifier of the stream searched.
 *
 * @return          Initialised OTF_RStream object.
 * 
 * \ingroup reader
 */
OTF_RStream* OTF_Reader_getStream( OTF_Reader* reader, uint32_t id );

/** disable given process. deprecated, use 'OTF_Reader_setProcessStatus()' instead. 

This funktion will destroy a pending read operation, i.e. a read operation 
currently interrupted cannot be continued! 
\ingroup reader */
int OTF_Reader_disableProcess( OTF_Reader* reader, uint32_t processId );

/** enable given process. deprecated, use 'OTF_Reader_setProcessStatus()' instead. 

This funktion will destroy a pending read operation, i.e. a read operation 
currently interrupted cannot be continued! 
\ingroup reader */
int OTF_Reader_enableProcess( OTF_Reader* reader, uint32_t processId );


/**
 * Returns the current process status
 *
 * @param reader     Pointer to an initialized OTF_Reader object. See 
 *                   also OTF_Reader_open().
 * @param processId  Identifier of the process to get the status from
 *
 * @return           Current process status. '1' for enabled, '0' for disabled
 *                   or unknown.
 *
 * \ingroup reader
 */
uint8_t OTF_Reader_getProcessStatus( OTF_Reader* reader, uint32_t processId );


/**
 * Sets the current status of the process. This function will destroy a pending
 * read operation, i.e. a read operation currently interrupted cannot be
 * continued!
 *
 * @param reader     Pointer to an initialized OTF_Reader object. See 
 *                   also OTF_Reader_open().
 * @param processId  Identifier of the process.
 * @param status     new status of the process. '1' for enabled, '0'
 *                   for disabled or unknown.
 *
 * @return           1 on success, 0 if an error occurs.
 *
 * \ingroup reader
 */
int OTF_Reader_setProcessStatus( OTF_Reader* reader, uint32_t processId, 
	uint8_t status );

/**
 * Sets the status for all processes with a single call. This function will
 * destroy a pending read operation, i.e. a read operation currently
 * interrupted cannot be continued! 
 *
 * @param reader     Pointer to an initialized OTF_Reader object. See 
 *                   also OTF_Reader_open().
 * @param status     new status of the process. 
 *
 * @return           1 on success, 0 if an error occurs.
 *
 * \ingroup reader
 */
int OTF_Reader_setProcessStatusAll( OTF_Reader* reader, uint8_t status );


/**
 * Set the minimum time and the maximum time of the reader.
 *
 * \par For example:
 *    minTime = 100000;
 * \n maxTime = 100003;
 * \n Times to read: 100000, 100001, 100002
 *
 * @param reader     Pointer to an initialized OTF_Reader object. See 
 *                   also OTF_Reader_open().
 * @param minTime    time where reading starts (including this timestamp)
 * @param maxTime    time where reading ends (excluding this timestamp)
 *
 * \ingroup reader
 */
void OTF_Reader_setTimeInterval( OTF_Reader* reader, uint64_t minTime,
	uint64_t maxTime );


/**
 * Returns the begin of current time interval.
 *
 * @param reader     Pointer to an initialized OTF_Reader object. See 
 *                   also OTF_Reader_open().
 *
 * \ingroup reader
 */
uint64_t OTF_Reader_getTimeIntervalMin( OTF_Reader* reader );


/**
 * Returns end of current time interval.
 *
 * @param reader     Pointer to an initialized OTF_Reader object. See 
 *                   also OTF_Reader_open().
 *
 * \ingroup reader
 */
uint64_t OTF_Reader_getTimeIntervalMax( OTF_Reader* reader );


/**
 * Set the maximum number of records delivered by a single call to
 * OTF_Reader_readXYZ(). Defaults to OTF_READ_MAXRECORDS == \infty.
 * 'OTF_Reader_readXYZ()' returns with the number of records processed.
 * Successive calls to 'OTF_Reader_readXYZ()' will deliver the remaining
 * records. This funktion will NOT destroy a pending read operation, i.e. a
 * read operation  currently interrupted CAN be continued!
 *
 * @param reader     Pointer to an initialized OTF_Reader object. See 
 *                   also OTF_Reader_open().
 * @param limit      record limit. has to be smaller than or equal to
 *                   OTF_READ_MAXRECORDS
 *
 * \ingroup reader
 */
void OTF_Reader_setRecordLimit( OTF_Reader* reader, uint64_t limit );


/**
 * Returns the current record limit.
 *
 * @param reader     Pointer to an initialized OTF_Reader object. See 
 *                   also OTF_Reader_open().
 *
 * @return           Current record limit.
 *
 * \ingroup reader
 */
uint64_t OTF_Reader_getRecordLimit( OTF_Reader* reader );

/**
 * Resets all filters for timelimit, process selection and record count limit.
 *
 * @param reader     Pointer to an initialized OTF_Reader object. See 
 *                   also OTF_Reader_open().
 *
 * \ingroup reader
 */
void OTF_Reader_reset( OTF_Reader* reader );

/**
 * Closes all streams that are open in the reader.
 *
 * @param reader     Pointer to an initialized OTF_Reader object. See 
 *                   also OTF_Reader_open().
 *
 * @return           1 on success, 0 if an error occurs.
 *
 * \ingroup reader
 */
int OTF_Reader_closeAllStreams( OTF_Reader* reader );


/** depricated. @see OTF_Reader_eventTimeProgress() */
uint8_t OTF_Reader_eventProgress( OTF_Reader* reader, uint64_t* minimum,
	uint64_t* current, uint64_t* maximum );
	
/** depricated. @see OTF_Reader_snapshotTimeProgress() */
uint8_t OTF_Reader_snapshotProgress( OTF_Reader* reader, 
		uint64_t* minimum, uint64_t* current, uint64_t* maximum );
		
/** depricated. @see OTF_Reader_statisticTimeProgress() */
uint8_t OTF_Reader_statisticProgress( OTF_Reader* reader,
	uint64_t* minimum, uint64_t* current, uint64_t* maximum );
	
	
/**
 * Delivers a progress report for reading events. It is given in terms
 * of time stamps. A percentage can be computed as 
 * ( current - minimum ) / ( maximum - minimum ). 
 * This computation takes restricted time intervals into account which is not 
 * possible with OTF_Reader_eventBytesProgress().
 *
 * The progress report is only valid after one or several calls to
 * OTF_Reader_readEvents(). Otherwise the return arguments 'minimum', 'current' 
 * and 'maximum' are undefined! 
 * If 'minimum' > 'maximum' the values are invalid.
 *
 * @param reader     Pointer to an initialized OTF_Reader object. See 
 *                   also OTF_Reader_open().
 * @param minimum    Return value for the minium time.
 * @param current    Return value for the current time.
 * @param maximum    Return value for the maximum time.
 *
 * @return           1 on success, 0 if an error occurs.
 *
 * \ingroup reader
 */
uint8_t OTF_Reader_eventTimeProgress( OTF_Reader* reader, uint64_t* minimum,
	uint64_t* current, uint64_t* maximum );


/**
 * Delivers a progress report for reading snapshots. It is given in terms
 * of time stamps. A percentage can be computed as 
 * ( current - minimum ) / ( maximum - minimum ). 
 * This computation takes restricted time intervals into account which is not 
 * possible with OTF_Reader_snapshotBytesProgress().
 *
 * The progress report is only valid after one or several calls to
 * OTF_Reader_readSnapshots(). Otherwise the return arguments 'minimum', 'current' 
 * and 'maximum' are undefined! 
 * If 'minimum' > 'maximum' the values are invalid.
 *
 * @param reader     Pointer to an initialized OTF_Reader object. See 
 *                   also OTF_Reader_open().
 * @param minimum    Return value for the minium time.
 * @param current    Return value for the current time.
 * @param maximum    Return value for the maximum time.
 *
 * @return           1 on success, 0 if an error occurs.
 *
 * \ingroup reader
 */
uint8_t OTF_Reader_snapshotTimeProgress( OTF_Reader* reader, 
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
 * @param reader     Pointer to an initialized OTF_Reader object. See 
 *                   also OTF_Reader_open().
 * @param minimum    Return value for the minium time.
 * @param current    Return value for the current time.
 * @param maximum    Return value for the maximum time.
 *
 * @return           1 on success, 0 if an error occurs.
 *
 * \ingroup reader
 */
uint8_t OTF_Reader_statisticTimeProgress( OTF_Reader* reader,
	uint64_t* minimum, uint64_t* current, uint64_t* maximum );


/**
 * Delivers a progress report for reading events. Progress is given in terms
 * of bytes. The percentage can be computed as ( current - minimum ) / ( maximum - minimum ). 
 * 
 * ATTENTION: This is only a rough estimate of the progress, because it is
 * computed based on the block I/O from files but not based on the actual bytes 
 * processed. This may result in constant values for small traces.
 * See also OTF_Reader_eventTimeProgress():
 *
 * This computation takes the read bytes of every active stream into account. 
 * The progress report is only valid after one or several calls to
 * OTF_Reader_readEvents(). Otherwise the return arguments 'minimum', 'current' and 'maximum' are
 * undefined! If 'minimum' > 'maximum' the values are invalid.
 *
 * @param reader     Pointer to an initialized OTF_Reader object. See 
 *                   also OTF_Reader_open().
 * @param minimum    Return value for the minium bytes read ( is 0 everytime ).
 * @param current    Return value for the current bytes read.
 * @param maximum    Return value for the filesize.
 *
 * @return           1 on success, 0 if an error occurs.
 *
 * \ingroup reader
 */
uint8_t OTF_Reader_eventBytesProgress( OTF_Reader* reader, uint64_t* minimum,
	uint64_t* current, uint64_t* maximum );


/**
 * Delivers a progress report for reading snapshots. Progress is given in terms
 * of bytes. The percentage can be computed as ( current - minimum ) / ( maximum - minimum ). 
 * 
 * ATTENTION: This is only a rough estimate of the progress, because it is
 * computed based on the block I/O from files but not based on the actual bytes 
 * processed. This may result in constant values for small traces.
 * See also OTF_Reader_snapshotTimeProgress():
 *
 * This computation takes the read bytes of every active stream into account. 
 * The progress report is only valid after one or several calls to
 * OTF_Reader_readSnapshots(). Otherwise the return arguments 'minimum', 'current' and 'maximum' are
 * undefined! If 'minimum' > 'maximum' the values are invalid.
 *
 * @param reader     Pointer to an initialized OTF_Reader object. See 
 *                   also OTF_Reader_open().
 * @param minimum    Return value for the minium bytes read ( is 0 everytime ).
 * @param current    Return value for the current bytes read.
 * @param maximum    Return value for the filesize.
 *
 * @return           1 on success, 0 if an error occurs.
 *
 * \ingroup reader
 */
uint8_t OTF_Reader_snapshotBytesProgress( OTF_Reader* reader, 
		uint64_t* minimum, uint64_t* current, uint64_t* maximum );

/**
 * Delivers a progress report for reading statistics. Progress is given in terms
 * of bytes. The percentage can be computed as ( current - minimum ) / ( maximum - minimum ). 
 * 
 * ATTENTION: This is only a rough estimate of the progress, because it is
 * computed based on the block I/O from files but not based on the actual bytes 
 * processed. This may result in constant values for small traces.
 * See also OTF_Reader_statisticTimeProgress():
 *
 * This computation takes the read bytes of every active stream into account. 
 * The progress report is only valid after one or several calls to
 * OTF_Reader_readStatistics(). Otherwise the return arguments 'minimum', 'current' and 'maximum' are
 * undefined! If 'minimum' > 'maximum' the values are invalid.
 *
 * @param reader     Pointer to an initialized OTF_Reader object. See 
 *                   also OTF_Reader_open().
 * @param minimum    Return value for the minium bytes read ( is 0 everytime ).
 * @param current    Return value for the current bytes read.
 * @param maximum    Return value for the filesize.
 *
 * @return           1 on success, 0 if an error occurs.
 *
 * \ingroup reader
 */
uint8_t OTF_Reader_statisticBytesProgress( OTF_Reader* reader,
	uint64_t* minimum, uint64_t* current, uint64_t* maximum );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_READER_H */
