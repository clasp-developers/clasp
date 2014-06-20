/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_Writer.h
 *
 *  @brief Transparently writes OTF traces which consist of multiple streams.
 *
 * \ingroup writer
 */

/** \defgroup writer Writer Interface
 *
 *  This interface should be used whenever a trace file is to be written as a
 *  whole.  Therefore, an initial call to the OTF_Writer_open() function
 *  allows to specify a number of streams which are going to be used to
 *  automatically partition the recorded event data. OTF than takes over the
 *  duty of distributing the data on multiple files.
 *
 *  \section writer_example A simple Example
 *
 *  \code
 *  #include <assert.h>
 *  #include "otf.h"
 *
 *  int main( int argc, char** argv ) {
 *  \endcode
 *
 *      Declare a file manager and a writer.
 *      \code
 * 	    OTF_FileManager* manager;
 * 	    OTF_Writer* writer;
 * 	    \endcode
 *
 *     	Initialize the file manager. Open at most 100 OS files.
 *     	\code
 *     	manager= OTF_FileManager_open( 100 );
 * 	    assert( manager );
 * 	    \endcode
 *
 * 	    Initialize the writer. Open file "test", writing one stream.
 * 	    \code
 * 	    writer = OTF_Writer_open( "test", 1, manager );
 *      assert( writer );
 *      \endcode
 *      
 *      Write some important Definition Records.
 *      Have a look at the specific functions to see what the parameters mean.
 *      \code
 *      OTF_Writer_writeDefTimerResolution( writer, 0, 1000 );
 *      OTF_Writer_writeDefProcess( writer, 0, 1, "proc one", 0 );
 *      OTF_Writer_writeDefFunctionGroup( writer, 0, 1000, "all functions" );
 *      OTF_Writer_writeDefFunction( writer, 0, 1, "main", 1000, 0 );
 *      \endcode
 *
 *
 *      Write an enter and a leave record.
 *      time = 10000, 20000
 *      process = 1
 *      function = 1
 *      Sourcecode location doesn't matter, so it's zero.
 *      \code
 *      OTF_Writer_writeEnter( writer, 10000, 1, 1, 0 );
 *      OTF_Writer_writeLeave( writer, 20000, 1, 1, 0 );
 *      \endcode
 *
 *      Clean up before exiting the program.
 *      \code
 *      OTF_Writer_close( writer );
 *      OTF_FileManager_close( manager );
 *
 *		return 0;
 * }
 * \endcode
 *
 * Compile this using $ gcc -o test test.c `otfconfig --libs`.
 *
 */

#ifndef OTF_WRITER_H
#define OTF_WRITER_H


#include "OTF_MasterControl.h"
#include "OTF_FileManager.h"
#include "OTF_WBuffer.h"
#include "OTF_WStream.h"


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/** writer object \ingroup writer */
typedef struct struct_OTF_Writer OTF_Writer;

/* *** public member functions *** */


/**     
 * Create a new OTF_Writer instance with a given number of automatic streams.
 *
 * Setting the number of streams to 0 causes the OTF_Writer object to create a
 * separate stream for each process. Important! Explicit calls to
 * OTF_Writer_assignProcess() can lead to an overall number of streams which
 * exceeds the initial number of streams in this call. 
 * OTF can reduce its file handle usage to a given number. Therefore, an 
 * initialized file manager instance is needed as parameter. 
 * See OTF_FileManager for further details.
 *
 * @param  fileNamePrefix   File name prefix which is going to be used by 
 *                          all sub-files which belong to the trace.
 * @param  numberOfStreams  Initial number of independent data streams to 
 *                          be generated.
 * @param  fileManager      File handle manager. 
 *
 *
 * @return                  Initialized OTF_Writer instance or 0 if a failure
 *                          occurred.
 *
 * \ingroup writer
 */
OTF_Writer* OTF_Writer_open( const char* fileNamePrefix, 
                             uint32_t numberOfStreams, 
                             OTF_FileManager* fileManager );


/** 
 * Close an OTF_Writer instance and all its related files.
 *
 * @param writer  Pointer to an initialized OTF_Writer object. See 
 *                also OTF_Writer_open().
 * 
 * @return        1 if instance was closed successfully and 0 otherwise.       
 *
 * \ingroup writer
 */
int OTF_Writer_close( OTF_Writer* writer );


/** 
 * Close all streams that are open in this writer instance.
 *
 * @param writer  Pointer to an initialized OTF_Writer object. See 
 *                also OTF_Writer_open().
 *
 * @return        1 on success, 0 if an error occurs.
 * 
 * \ingroup writer
 */
int OTF_Writer_closeAllStreams( OTF_Writer* writer );


/**
 * Set the standard compression method for all buffers managed by this writer
 *
 * @param writer       Pointer to an initialized OTF_Writer object. See 
 *                     also OTF_Writer_open().
 *
 * @param compression  compression level to apply to all following streams
 *                     0-9, where 0 means no compression is applied, and 9 is
 *                     the highest level of compression.
 *
 * @return             1 on success, 0 if an error occurs.
 *
 * \ingroup writer
 */
int OTF_Writer_setCompression( OTF_Writer* writer, OTF_FileCompression
	compression );

	
/**
 * Return the standard compression method for all buffers managed by this writer
 *
 * @param writer       Pointer to an initialized OTF_Writer object. See 
 *                     also OTF_Writer_open().
 *
 * @return             Standard compression level for all buffers managed by
 *                     this writer.
 *
 * \ingroup writer
 */
OTF_FileCompression OTF_Writer_getCompression( OTF_Writer* writer );


/**
 * Set the default buffer size for all buffers managed by this Writer. 
 * This is only effective for future buffers and will not change already 
 * allocated buffers. Those can be changed with the buffers directly.
 *
 * @param writer  Pointer to an initialized OTF_Writer object. See 
 *                also OTF_Writer_open().
 * 
 * @param size    Intended buffer size.
 *
 * \ingroup writer
 */
void OTF_Writer_setBufferSizes( OTF_Writer* writer, uint32_t size );


/** 
 * Get the default buffer size for all buffers managed by this Writer.
 *
 * @param writer  Pointer to an initialized OTF_Writer object. See 
 *                also OTF_Writer_open().
 * 
 * @return        Default buffer size for all buffers managed by this Writer.
 *
 * \ingroup writer
 */
uint32_t OTF_Writer_getBufferSizes( OTF_Writer* writer );


/**
 * Set the default zbuffer size for all buffers managed by this Reader.
 * This is only effective for future files and will not change already
 * allocated zbuffers. Those can be changed with the files directly.
 *
 * @param writer  Pointer to an initialized OTF_Writer object. See 
 *                also OTF_Writer_open().
 *
 * @param size    Intended zbuffer size.
 *
 * \ingroup writer
 */
void OTF_Writer_setZBufferSizes( OTF_Writer* writer, uint32_t size );

/** 
 * Get the default zbuffer size.
 *
 * @param writer  Pointer to an initialized OTF_Writer object. See 
 *                also OTF_Writer_open().
 *
 * @return  zbuffer size.
 *
 * \ingroup writer
 */
uint32_t OTF_Writer_getZBufferSizes( OTF_Writer* writer );

/**
 * Set the default ouput format. The format is applied to all streams opened by
 * the writer.
 *
 * @param writer  Pointer to an initialized OTF_Writer object. See 
 *                also OTF_Writer_open().
 *
 * @param format  Intended output format (OTF_WSTREAM_FORMAT_{LONG,SHORT}).
 *
 * \ingroup writer
 */
void OTF_Writer_setFormat( OTF_Writer* writer, uint32_t format );


/**
 * Get the default output format of all streams managed by this writer.
 *
 * @param writer  Pointer to an initialized OTF_Writer object. See 
 *                also OTF_Writer_open().
 *
 * @return        Default output format.
 *
 * \ingroup writer
 */
uint32_t OTF_Writer_getFormat( OTF_Writer* writer );


/**     
 * Explicitly assign a given process to a specific stream.
 *
 * Mind that 0 is not a valid stream or process identifier but a reserved
 * value. By default, processes are automatically assigned to streams.
 * Therefore, this call is optional. 
 * 
 * @param writer   Pointer to an initialized OTF_Writer object. See 
 *                 also OTF_Writer_open().
 * @param process  Process identifier. See also OTF_Writer_writeDefProcess().
 * @param stream   Target stream identifier with 
 *                 0 < stream <= number of streams as defined in 
 *                 OTF_Writer_open().
 *
 * @return         1 on success, 0 if an error occurs. 
 *
 * \ingroup writer
 */
uint32_t OTF_Writer_assignProcess( OTF_Writer* writer,
                                   uint32_t process, 
                                   uint32_t stream );


/**
 * Get a pointer to the master control object of the given writer instance.
 *
 * @param writer   Pointer to an initialized OTF_Writer object. See 
 *                 also OTF_Writer_open().
 *
 * @return         Pointer to a master control object. See OTF_MasterControl.
 *
 * \ingroup writer
 */
OTF_MasterControl* OTF_Writer_getMasterControl( OTF_Writer* writer );


/**
 * Set an alternative master control object. Use this only right after 
 * initialization but never after having written some records already!
 *
 * @param writer   Pointer to an initialized OTF_Writer object. See 
 *                 also OTF_Writer_open().
 * @param mc       new master control object
 *                 
 *
 * \ingroup writer
 */
void OTF_Writer_setMasterControl( OTF_Writer* writer, OTF_MasterControl* mc );


/** For a process with id 'processId' return a stream id of the stream the
    data is to be written to. If no mapping has been set so far it is defined
    in a way such that it is added to the stream with the least processes.
	\ingroup writer */
uint32_t OTF_Writer_mapProcess( OTF_Writer* writer, uint32_t processId );


/** Return the stream with the given stream id. If there is no such stream yet
    create one and append it to 'streams'. \ingroup writer */
OTF_WStream* OTF_Writer_getStream( OTF_Writer* writer, uint32_t stream );


/* Methods for writing public definition records ************************** */


/**
 * Write a comment record.
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param stream    Target stream identifier with 
 *                  0 < stream <= number of streams as defined in 
 *                  OTF_Writer_open().
 * @param comment   Arbitrary comment string.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefinitionComment( OTF_Writer* writer, 
                                       uint32_t stream, 
                                       const char* comment );


/**
 * Write a comment record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeDefinitionComment()
 * \ingroup writer
 */
int OTF_Writer_writeDefinitionCommentKV( OTF_Writer* writer, 
                                       uint32_t stream, 
                                       const char* comment,
				       OTF_KeyValueList* list );


/**
 * Write the timer resolution definition record. All timed event records
 * will be interpreted according to this definition. By default, a timer
 * resultion of 1 us i.e. 1,000,000 clock ticks is assumed. 
 *
 * @param writer          Pointer to an initialized OTF_Writer object. See 
 *                        also OTF_Writer_open().
 * @param stream          Target stream identifier with 
 *                        0 < stream <= number of streams as defined in 
 *                        OTF_Writer_open().
 * @param ticksPerSecond  Clock ticks per second of the timer.
 *
 * @return                1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefTimerResolution( OTF_Writer* writer, 
                                        uint32_t stream,
                                        uint64_t ticksPerSecond );


/**
 * Write the timer resolution definition record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeTimerResolution()
 * \ingroup writer
 */
int OTF_Writer_writeDefTimerResolutionKV( OTF_Writer* writer, 
                                        uint32_t stream,
                                        uint64_t ticksPerSecond,
					OTF_KeyValueList* list );


/**
 * Write a process definition record. 
 *
 * @param writer   Pointer to an initialized OTF_Writer object. See 
 *                 also OTF_Writer_open().
 * @param stream   Target stream identifier with 
 *                 0 < stream <= number of streams as defined in 
 *                 OTF_Writer_open().
 * @param process  Arbitrary but unique process identifier > 0.        
 * @param name     Name of the process e.g. "Process X".
 * @param parent   Previously declared parent process identifier or 0 if 
 *                 process has no parent.
 *
 * @return         1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefProcess( OTF_Writer* writer, 
                                uint32_t stream,
                                uint32_t process, 
                                const char* name, 
                                uint32_t parent );


/**
 * Write a process definition record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeDefProcess()
 * \ingroup writer
 */
int OTF_Writer_writeDefProcessKV( OTF_Writer* writer, 
                                uint32_t stream,
                                uint32_t process, 
                                const char* name, 
                                uint32_t parent,
				OTF_KeyValueList* list );


/**
 * Write a process group definition record.
 *
 * OTF supports groups of processes. Their main objective is to classify
 * processes depending on arbitrary characteristics. Processes can reside
 * in multiple groups. This record type is optional.
 *
 * @param writer         Pointer to an initialized OTF_Writer object. See 
 *                       also OTF_Writer_open().
 * @param stream         Target stream identifier with 
 *                       0 < stream <= number of streams as defined in 
 *                       OTF_Writer_open().
 * @param procGroup      Arbitrary but unique process group identifier > 0.
 * @param name           Name of the process group e.g. "Well Balanced".
 * @param numberOfProcs  The number of processes in the process group.
 * @param procs          Vector of process identifiers or previously defined
 *                       process group identifiers as defined with
 *                       OTF_Writer_writeDefProcess() resp. 
 *                       OTF_Writer_writeDefProcessGroup.
 *
 * @return               1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefProcessGroup( OTF_Writer* writer, 
                                     uint32_t stream,
                                     uint32_t procGroup, 
                                     const char* name, 
                                     uint32_t numberOfProcs, 
                                     const uint32_t* procs );


/**
 * Write a process group definition record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeDefProcessGroup()
 * \ingroup writer
 */
int OTF_Writer_writeDefProcessGroupKV( OTF_Writer* writer, 
                                     uint32_t stream,
                                     uint32_t procGroup, 
                                     const char* name, 
                                     uint32_t numberOfProcs, 
                                     const uint32_t* procs,
				     OTF_KeyValueList* list );


/**
 * Write an attribute list definition record.
 *
 * Defines a list of attributes that is assigned to a unique token.
 *
 * @param writer       Pointer to an initialized OTF_Writer object. See 
 *                     also OTF_Writer_open().
 * @param stream       Target stream identifier with 
 *                     0 < stream <= number of streams as defined in 
 *                     OTF_Writer_open().
 * @param attr_token   Arbitrary but unique attribute list identifier > 0.
 * @param num          Number of elements in the attribute list array.
 * @param array        An array of different attributes with type of OTF_ATTR_TYPE(). 
 *
 * @return             1 on success, 0 if an error occurs.
 * 
 * \ingroup writer
 */
int OTF_Writer_writeDefAttributeList( OTF_Writer* writer,
				      uint32_t stream,
				      uint32_t attr_token,
				      uint32_t num,
				      OTF_ATTR_TYPE* array );


/**
 * Write an attribute list definition record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeDefAttributeList()
 * \ingroup writer
 */
int OTF_Writer_writeDefAttributeListKV( OTF_Writer* writer,
				      uint32_t stream,
				      uint32_t attr_token,
				      uint32_t num,
				      OTF_ATTR_TYPE* array,
				      OTF_KeyValueList* list );


/**
 * Write a process or group attributes definition record.
 *
 * @param writer       Pointer to an initialized OTF_Writer object. See 
 *                     also OTF_Writer_open().
 * @param stream       Target stream identifier with 
 *                     0 < stream <= number of streams as defined in 
 *                     OTF_Writer_open().
 * @param proc_token   Arbitrary but unique process or process group identifier > 0.
 * @param attr_token   A unique token that was defined with OTF_Writer_writeDefAttributeList().
 *
 * @return             1 on success, 0 if an error occurs.
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefProcessOrGroupAttributes( OTF_Writer* writer,
						 uint32_t stream,
						 uint32_t proc_token,
						 uint32_t attr_token );


/**
 * Write a process or group attributes definition record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeDefProcessOrGroupAttributes()
 * \ingroup writer
 */
int OTF_Writer_writeDefProcessOrGroupAttributesKV( OTF_Writer* writer,
						 uint32_t stream,
						 uint32_t proc_token,
						 uint32_t attr_token,
						 OTF_KeyValueList* list );


/**
 * Write a function definition record.
 *
 * Defines a function of the given name. Functions can optionally belong to a
 * certain function group to be defined with the
 * OTF_Writer_writeDefFunctionGroup() call. A source code reference can
 * be added to the definition aswell.
 *
 * @param writer     Pointer to an initialized OTF_Writer object. See 
 *                   also OTF_Writer_open().
 * @param stream     Target stream identifier with 
 *                   0 < stream <= number of streams as defined in 
 *                   OTF_Writer_open().
 * @param func       Arbitrary but unique function identifier > 0.
 * @param name       Name of the function e.g. "DoSomething".
 * @param funcGroup  A function group identifier preliminary defined with
 *                   OTF_Writer_writeDefFunctionGroup() or 0 for no
 *                   function group assignment.        
 * @param source     Reference to the function's source code location 
 *                   preliminary defined with OTF_Writer_writeDefScl() or
 *                   0 for no source code location assignment.
 *
 * @return           1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefFunction( OTF_Writer* writer, 
                                 uint32_t stream,
                                 uint32_t func, 
                                 const char* name, 
                                 uint32_t funcGroup, 
                                 uint32_t source );


/**
 * Write a function definition record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeDefFunction()
 * \ingroup writer
 */
int OTF_Writer_writeDefFunctionKV( OTF_Writer* writer, 
                                 uint32_t stream,
                                 uint32_t func, 
                                 const char* name, 
                                 uint32_t funcGroup, 
                                 uint32_t source,
				 OTF_KeyValueList* list );


/**
 * Write a function group definition record.
 *
 * @param writer     Pointer to an initialized OTF_Writer object. See 
 *                   also OTF_Writer_open().
 * @param stream     Target stream identifier with 
 *                   0 < stream <= number of streams as defined in 
 *                   OTF_Writer_open().
 * @param funcGroup  An arbitrary but unique function group identifier > 0.
 * @param name       Name of the function group e.g. "Computation".
 *
 * @return           1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefFunctionGroup( OTF_Writer* writer, 
                                      uint32_t stream,
                                      uint32_t funcGroup, 
                                      const char* name );


/**
 * Write a function group definition record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeDefFunctionGroup()
 * \ingroup writer
 */
int OTF_Writer_writeDefFunctionGroupKV( OTF_Writer* writer, 
                                      uint32_t stream,
                                      uint32_t funcGroup, 
                                      const char* name,
				      OTF_KeyValueList* list );


/**
 * Write a collective operation definition record.
 *
 * @param writer      Initialized OTF_Writer instance.
 * @param stream      Target stream identifier with 
 *                    0 < stream <= number of streams as defined in 
 *                    OTF_Writer_open().
 * @param collOp      An arbitrary but unique collective op. identifier > 0.
 * @param name        Name of the collective operation e.g. "MPI_Bcast".
 * @param type        One of the five supported collective classes:
 *                    OTF_COLLECTIVE_TYPE_UNKNOWN (default),
 *                    OTF_COLLECTIVE_TYPE_BARRIER,
 *                    OTF_COLLECTIVE_TYPE_ONE2ALL,
 *                    OTF_COLLECTIVE_TYPE_ALL2ONE,
 *                    OTF_COLLECTIVE_TYPE_ALL2ALL.
 *
 * @return            1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefCollectiveOperation( OTF_Writer* writer, 
                                            uint32_t stream,
                                            uint32_t collOp,
                                            const char* name,
                                            uint32_t type );


/**
 * Write a collective operation definition record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeDefCollectiveOperation()
 * \ingroup writer
 */
int OTF_Writer_writeDefCollectiveOperationKV( OTF_Writer* writer, 
                                            uint32_t stream,
                                            uint32_t collOp,
                                            const char* name,
                                            uint32_t type,
					    OTF_KeyValueList* list );


/**
 * Write a counter definition record.
 *
 * @param writer        Initialized OTF_Writer instance.
 * @param stream        Target stream identifier with 
 *                      0 < stream <= number of streams as defined in 
 *                      OTF_Writer_open().
 * @param counter       An arbitrary but unique counter identifier.
 * @param name          Name of the counter e.g. "Cache Misses".
 * @param properties    A combination of a type and scope counter property.
 *                      OTF_COUNTER_TYPE_ACC (default) represents a counter
 *                      with monotonously increasing values e.g. a FLOP 
 *                      counter. OTF_COUNTER_TYPE_ABS on the other hand 
 *                      defines a counter with alternating absolute values e.g.
 *                      the memory usage of a process. The following counter
 *                      measurement scopes are supported: 
 *                      OTF_COUNTER_SCOPE_START (default) always refers to the 
 *                      start of the process, OTF_COUNTER_SCOPE_POINT refers
 *                      to exactly this moment in time, OTF_COUNTER_SCOPE_LAST
 *                      relates to the previous measurement, and
 *                      OTF_COUNTER_SCOPE_NEXT to the next measurement.
 *                      Examples: OTF_COUNTER_TYPE_ACC + 
 *                      OTF_COUNTER_SCOPE_START should be used for most 
 *                      standard hardware (PAPI) counters. 
 *                      OTF_COUNTER_TYPE_ABS + OTF_COUNTER_SCOPE_POINT could
 *                      be used to record information 'spikes'.
 *                      OTF_COUNTER_TYPE_ABS + OTF_COUNTER_SCOPE_NEXT works
 *                      for memory allocation recording. 
 * @param counterGroup  A previously defined counter group identifier or 0 
 *                      for no group.
 * @param unit          Unit of the counter e.g. "#" for "number of..." or 0 
 *                      for no unit. 
 *
 * @return              1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefCounter( OTF_Writer* writer,
                                uint32_t stream,
                                uint32_t counter,
                                const char* name,
                                uint32_t properties, 
                                uint32_t counterGroup, 
                                const char* unit );


/**
 * Write a counter definition record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeDefCounter()
 * \ingroup writer
 */
int OTF_Writer_writeDefCounterKV( OTF_Writer* writer,
                                uint32_t stream,
                                uint32_t counter,
                                const char* name,
                                uint32_t properties, 
                                uint32_t counterGroup, 
                                const char* unit,
				OTF_KeyValueList* list );


/**
 * Write a counter group definition record.
 *
 * @param writer       Initialized OTF_Writer instance.
 * @param stream       Target stream identifier with 
 *                     0 < stream <= number of streams as defined in 
 *                     OTF_Writer_open().
 * @param counterGroup An arbitrary but unique counter group identifier.
 * @param name         Counter group name.
 *
 * @return             1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefCounterGroup( OTF_Writer* writer, 
                                     uint32_t stream,
                                     uint32_t counterGroup, 
                                     const char* name );


/**
 * Write a counter group definition record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeDefCounterGroup()
 * \ingroup writer
 */
int OTF_Writer_writeDefCounterGroupKV( OTF_Writer* writer, 
                                     uint32_t stream,
                                     uint32_t counterGroup, 
                                     const char* name,
				     OTF_KeyValueList* list );


/**
 * Write a source code location (SCL) record.
 *
 * @param writer       Initialized OTF_Writer instance.
 * @param stream       Target stream identifier with 
 *                     0 < stream <= number of streams as defined in 
 *                     OTF_Writer_open().
 * @param source       Arbitrary but unique source code location 
 *                     identifier > 0.
 * @param sourceFile   Previously defined source file identifier. See 
 *                     OTF_Writer_writeDefSclFile(). 
 * @param line         Line number.
 *
 * @return             1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefScl( OTF_Writer* writer,
                            uint32_t stream,
                            uint32_t source,
                            uint32_t sourceFile, 
                            uint32_t line );


/**
 * Write a source code location (SCL) record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeDefScl()
 * \ingroup writer
 */
int OTF_Writer_writeDefSclKV( OTF_Writer* writer,
                            uint32_t stream,
                            uint32_t source,
                            uint32_t sourceFile, 
                            uint32_t line,
			    OTF_KeyValueList* list );


/**
 * Write a source code location (SCL) file record.
 *
 * @param writer       Initialized OTF_Writer instance.
 * @param stream       Target stream identifier with 
 *                     0 < stream <= number of streams as defined in 
 *                     OTF_Writer_open().
 * @param sourceFile   Arbitrary but unique source code location 
 *                     identifier != 0.
 * @param name         File name. 
 *
 * @return             1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefSclFile( OTF_Writer* writer,
                                uint32_t stream, 
                                uint32_t sourceFile,
                                const char* name );


/**
 * Write a source code location (SCL) file record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeDefSclFile()
 * \ingroup writer
 */
int OTF_Writer_writeDefSclFileKV( OTF_Writer* writer,
                                uint32_t stream, 
                                uint32_t sourceFile,
                                const char* name,
				OTF_KeyValueList* list );


/**
 * depricated. The Otf-Version-record is generated automatically at beginning of
 * tracing in the global definiton stream.
 *
 * \ingroup writer
 */
int OTF_Writer_writeOtfVersion( OTF_Writer* writer, uint32_t stream );


/**
 * Write a creator record. 
 *
 * @param writer       Initialized OTF_Writer instance.
 * @param stream       Target stream identifier with 
 *                     0 < stream <= number of streams as defined in 
 *                     OTF_Writer_open().
 * @param creator      String which identifies the creator of the 
 *                     file e.g. "TAU Version x.y.z".
 *
 * @return             1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefCreator( OTF_Writer* writer, uint32_t stream,
                                const char* creator );


/**
 * Write a creator record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeDefCreator()
 * \ingroup writer
 */
int OTF_Writer_writeDefCreatorKV( OTF_Writer* writer, uint32_t stream,
                                const char* creator,
				OTF_KeyValueList* list );




/**
 * Write a file definition record
 *
 * @param writer       Initialized OTF_Writer instance.
 * @param stream       Target stream identifier with 
 *                     0 < stream <= number of streams as defined in 
 *                     OTF_Writer_open().
 * @param token        Arbitrary, unique identifier for the file.
 *                     Has to be > 0.
 * @param name         Name of the file.
 * @param group        File group identifier or 0 for no group.
 *
 * @return             1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefFile( OTF_Writer* writer,
                             uint32_t stream,
                             uint32_t token,
                             const char* name,
                             uint32_t group );


/**
 * Write a file definition record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeDefFile()
 * \ingroup writer
 */
int OTF_Writer_writeDefFileKV( OTF_Writer* writer,
                             uint32_t stream,
                             uint32_t token,
                             const char* name,
                             uint32_t group,
			     OTF_KeyValueList* list );


/**
 * Write a file group definition record
 *
 * @param writer       Initialized OTF_Writer instance.
 * @param stream       Target stream identifier with 
 *                     0 < stream <= number of streams as defined in 
 *                     OTF_Writer_open().
 * @param token        Arbitrary, unique identifier for the file group.
 *                     Has to be > 0.
 * @param name         Name of the file group.
 *
 * @return             1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefFileGroup( OTF_Writer* writer,
                                  uint32_t stream,
                                  uint32_t token,
                                  const char* name );


/**
 * Write a file group definition record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeDefFileGroup()
 * \ingroup writer
 */
int OTF_Writer_writeDefFileGroupKV( OTF_Writer* writer,
                                  uint32_t stream,
                                  uint32_t token,
                                  const char* name,
				  OTF_KeyValueList* list );


/**
 * Write a key value definition record
 *
 * @param writer       Initialized OTF_Writer instance.
 * @param stream       Target stream identifier with 
 *                     0 < stream <= number of streams as defined in 
 *                     OTF_Writer_open().
 * @param key          Arbitrary, unique identifier for the key value pair.
 * @param type         Type of the key.
 * @param name         Name of the key value pair.
 * @param description  Description of the key value pair.
 *
 * @return             1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefKeyValue( OTF_Writer* writer,
				 uint32_t stream, 
				 uint32_t key,
				 OTF_Type type,
				 const char* name,
				 const char* description );


/**
 * Write a key value definition record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeDefKeyValue()
 * \ingroup writer
 */
int OTF_Writer_writeDefKeyValueKV( OTF_Writer* writer,
				 uint32_t stream, 
				 uint32_t key,			   
				 OTF_Type type,
				 const char* name,
				 const char* description,
				 OTF_KeyValueList* list );


/**
 * Writes a TimeRange definition
 *
 * @param writer       Initialized OTF_Writer instance.
 * @param streamid     Identifies the stream to which this definition
 *                     belongs to. stream = 0 represents a global
 *                     definition.
 * @param minTime      The smallest timestamp of the events in this @a streamid.
 * @param maxTime      The greates timestamp of the events in this @a streamid (inclusive).
 * @param list         Pointer to an OTF_KeyValueList() that contains individual data.
 *
 * @return             1 on success, 0 if an error occurs.
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefTimeRange( OTF_Writer*       writer,
                                  uint32_t          streamid,
                                  uint64_t          minTime,
                                  uint64_t          maxTime,
                                  OTF_KeyValueList* list );

/**
 * Writes a CounterAssignments definition
 *
 * @param writer       Initialized OTF_Writer instance.
 * @param streamid     Identifies the stream to which this definition
 *                     belongs to. stream = 0 represents a global
 *                     definition.
 * @param counter_token     Counter id.
 * @param number_of_members Number of entries in @procs_or_groups array.
 * @param procs_or_groups   The processes or process groups which have recorded
 *                          counter data for counter @counter_token.
 * @param list         Pointer to an OTF_KeyValueList() that contains individual data.
 *
 * @return             1 on success, 0 if an error occurs.
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefCounterAssignments( OTF_Writer*       writer,
                                           uint32_t          streamid,
                                           uint32_t          counter_token,
                                           uint32_t          number_of_members,
                                           const uint32_t*   procs_or_groups,
                                           OTF_KeyValueList* list );


/**
 * Writes a ProcessSubstitutes definition record
 *
 * @param writer       Initialized OTF_Writer instance.
 * @param streamid     Identifies the stream to which this definition
 *                     belongs to. stream = 0 represents a global
 *                     definition.
 * @param representative     Process ID of the process that represents several others.
 * @param numberOfProcs      Number of entries in @procs array.
 * @param procs              The processes which are represented by
 *                           @representative. It may or may not include 
 *                           @representative itself.
 * @param list         Pointer to an OTF_KeyValueList() that contains individual data.
 *
 * @return             1 on success, 0 if an error occurs.
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefProcessSubstitutes( OTF_Writer*       writer,
                                           uint32_t          streamid,
                                           uint32_t          representative,
                                           uint32_t          numberOfProcs,
                                           const uint32_t*   procs,
                                           OTF_KeyValueList* list );


/**
 * Write a auxiliary sample point definition record.
 *
 * @param writer       Initialized OTF_Writer instance.
 * @param stream       Target stream identifier with
 *                     0 < stream <= number of streams as defined in
 *                     OTF_Writer_open().
 * @param time         Time at which the auxiliary sample point information
 *                     is available.
 * @param type         Type of the auxiliary sample point.
 *                     See @a OTF_AuxSamplePointType.
 *
 * @param list         Pointer to an OTF_KeyValueList() that contains individual data.
 *
 * @return             1 on success, 0 if an error occurs.
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefAuxSamplePoint( OTF_Writer*            writer,
                                       uint32_t               streamid,
                                       uint64_t               time,
                                       OTF_AuxSamplePointType type,
                                       OTF_KeyValueList*      list );


/**
 * Write a no-operation record. This can be used to write an OTF_KeyValueList
 * that is not attached to a special event record.
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param time      The time when the NoOp event took place.
 * @param process   Process where action took place.
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeNoOpKV( OTF_Writer* writer,
			    uint64_t time,
			    uint32_t process,
                            OTF_KeyValueList* list );


/**
 * Write a function entry record.
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param time      The time when the function entry took place.
 * @param function  Function to be entered as defined with 
 *                  OTF_Writer_defFunction.
 * @param process   Process where action took place.
 * @param source    Optional reference to source code.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeEnter( OTF_Writer* writer, 
                           uint64_t time, 
                           uint32_t function, 
                           uint32_t process, 
                           uint32_t source );


/**
 * Write a function entry record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeEnter()
 * \ingroup writer
 */
int OTF_Writer_writeEnterKV( OTF_Writer* writer, 
                           uint64_t time, 
                           uint32_t function, 
                           uint32_t process, 
                           uint32_t source,
			   OTF_KeyValueList* list );


/**
 * Write a function leave record.
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param time      The time when the function leave took place.
 * @param function  Function which was left or 0 if stack integrety checking
 *                  is not needed.
 * @param process   Process where action took place.
 * @param source    Explicit source code location or 0.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeLeave( OTF_Writer* writer, 
                           uint64_t time, 
                           uint32_t function, 
                           uint32_t process, 
                           uint32_t source );


/**
 * Write a function leave record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeLeave()
 * \ingroup writer
 */
int OTF_Writer_writeLeaveKV( OTF_Writer* writer, 
                           uint64_t time, 
                           uint32_t function, 
                           uint32_t process, 
                           uint32_t source,
			   OTF_KeyValueList* list );


/**
 * Write a message retrieval record.
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param time      The time when the message was received.
 * @param receiver  Identifier of receiving process.
 * @param sender    Identifier of sending process.
 * @param procGroup Optional process-group sender and receiver belong to,
 *                  '0' for no group.
 * @param tag       Optional message type information.
 * @param length    Optional message length information.
 * @param source    Optional reference to source code.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeRecvMsg( OTF_Writer* writer, 
                             uint64_t time, 
                             uint32_t receiver, 
                             uint32_t sender, 
                             uint32_t procGroup, 
                             uint32_t tag, 
                             uint32_t length, 
                             uint32_t source );


/**
 * Write a message retrieval record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeRecvMsg()
 * \ingroup writer
 */
int OTF_Writer_writeRecvMsgKV( OTF_Writer* writer, 
                             uint64_t time, 
                             uint32_t receiver, 
                             uint32_t sender, 
                             uint32_t procGroup, 
                             uint32_t tag, 
                             uint32_t length, 
                             uint32_t source,
			     OTF_KeyValueList* list );


/**
 * Write a message send record.
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param time      The time when the message was send.
 * @param sender    Sender of the message.
 * @param receiver  Receiver of the message.
 * @param procGroup Optional process-group sender and receiver belong to,
 *                  '0' for no group.
 * @param tag       Optional message type information.
 * @param length    Optional message length information.
 * @param source    Optional reference to source code.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeSendMsg( OTF_Writer* writer, 
                             uint64_t time, 
                             uint32_t sender, 
                             uint32_t receiver, 
                             uint32_t procGroup, 
                             uint32_t tag, 
                             uint32_t length, 
                             uint32_t source );


/**
 * Write a message send record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeSendMsg()
 * \ingroup writer
 */
int OTF_Writer_writeSendMsgKV( OTF_Writer* writer, 
                             uint64_t time, 
                             uint32_t sender, 
                             uint32_t receiver, 
                             uint32_t procGroup, 
                             uint32_t tag, 
                             uint32_t length, 
                             uint32_t source,
			     OTF_KeyValueList* list );


/**
 * Write a counter measurement record.
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param time      Time when counter was measured.
 * @param process   Process where counter measurment took place.
 * @param counter   Counter which was measured. 
 * @param value     Counter value.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeCounter( OTF_Writer* writer, 
                             uint64_t time, 
                             uint32_t process, 
                             uint32_t counter, 
                             uint64_t value );


/**
 * Write a counter measurement record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeCounter()
 * \ingroup writer
 */
int OTF_Writer_writeCounterKV( OTF_Writer* writer, 
                             uint64_t time, 
                             uint32_t process, 
                             uint32_t counter, 
                             uint64_t value,
			     OTF_KeyValueList* list );


/**
 * Write a collective operation member record.
 * @deprecated This event record has been deprecated due to usage constraints.
 *             Please use OTF_Writer_writeBeginCollectiveOperation() and
 *             OTF_Writer_writeEndCollectiveOperation(), repectively.
 *
 * @param writer      Initialized OTF_Writer instance.
 * @param time        Time when collective operation was entered by member.
 * @param process     Process identifier i.e. collective member. 
 * @param collective  Collective identifier to be defined with
 *                    OTF_Writer_writeDefCollectiveOperation(). 
 * @param procGroup   Group of processes participating in this collective.
 * @param rootProc    Root process if != 0.
 * @param sent        Data volume sent by member or 0.
 * @param received    Data volumd received by member or 0.
 * @param duration    Time spent in collective operation.
 * @param source      Explicit source code location or 0.
 *
 * @return            1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeCollectiveOperation( OTF_Writer* writer, 
                                         uint64_t time, 
                                         uint32_t process, 
                                         uint32_t collective, 
                                         uint32_t procGroup, 
                                         uint32_t rootProc, 
                                         uint32_t sent, 
                                         uint32_t received, 
                                         uint64_t duration, 
                                         uint32_t source );


/**
 * Write a collective operation member record including an OTF_KeyValueList.
 * @deprecated This event record has been deprecated due to usage constraints.
 *             Please use OTF_Writer_writeBeginCollectiveOperationEV() and
 *             OTF_Writer_writeEndCollectiveOperationEV(), repectively.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeCollectiveOperation()
 * \ingroup writer
 */
int OTF_Writer_writeCollectiveOperationKV( OTF_Writer* writer, 
                                         uint64_t time, 
                                         uint32_t process, 
                                         uint32_t collective, 
                                         uint32_t procGroup, 
                                         uint32_t rootProc, 
                                         uint32_t sent, 
                                         uint32_t received, 
                                         uint64_t duration, 
                                         uint32_t source,
					 OTF_KeyValueList* list );


/**
 * Write a begin collective operation member record.
 *
 * @param writer      Initialized OTF_Writer instance.
 * @param time        Time when collective operation was entered by member.
 * @param process     Process identifier i.e. collective member. 
 * @param collOp      Collective identifier to be defined with
 *                    OTF_Writer_writeDefCollectiveOperation(). 
 * @param matchingId  Identifier for finding the associated end collective event
 *                    record. It must be unique within this procGroup.
 * @param procGroup   Group of processes participating in this collective.
 * @param rootProc    Root process if != 0.
 * @param sent        Data volume sent by member or 0.
 * @param received    Data volume received by member or 0.
 * @param scltoken    Explicit source code location or 0.
 *
 * @return            1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeBeginCollectiveOperation( OTF_Writer* writer,
					      uint64_t time,
					      uint32_t process,
					      uint32_t collOp,
					      uint64_t matchingId,
					      uint32_t procGroup,
					      uint32_t rootProc,
					      uint64_t sent,
					      uint64_t received,
					      uint32_t scltoken );


/**
 * Write a begin collective operation member record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeBeginCollectiveOperation()
 * \ingroup writer
 */
int OTF_Writer_writeBeginCollectiveOperationKV( OTF_Writer* writer,
					      uint64_t time,
					      uint32_t process,
					      uint32_t collOp,
					      uint64_t matchingId,
					      uint32_t procGroup,
					      uint32_t rootProc,
					      uint64_t sent,
					      uint64_t received,
					      uint32_t scltoken,
					      OTF_KeyValueList* list );


/**
 * Write an end collective operation member record.
 *
 * @param writer      Initialized OTF_Writer instance.
 * @param time        Time when collective operation was entered by member.
 * @param process     Process identifier i.e. collective member. 
 * @param matchingId  Matching identifier, must match a previous start
 *                    collective operation.
 *
 * @return            1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeEndCollectiveOperation( OTF_Writer* writer,
					    uint64_t time,
					    uint32_t process,
					    uint64_t matchingId );


/**
 * Write an end collective operation member record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeEndCollectiveOperation()
 * \ingroup writer
 */
int OTF_Writer_writeEndCollectiveOperationKV( OTF_Writer* writer,
					    uint64_t time,
					    uint32_t process,
					    uint64_t matchingId,
					    OTF_KeyValueList* list );


/**
 * Write a comment record.
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param time      Comments need a timestamp for a proper positioning in the 
 *                  trace.
 * @param process   Comments also need a process identifier for a proper 
 *                  positioning in the trace. 
 * @param comment   Arbitrary comment string.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
 
int OTF_Writer_writeEventComment( OTF_Writer* writer, 
                                  uint64_t time, 
                                  uint32_t process, 
                                  const char* comment );


/**
 * Write a comment record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeEventComment()
 * \ingroup writer
 */
int OTF_Writer_writeEventCommentKV( OTF_Writer* writer, 
                                  uint64_t time, 
                                  uint32_t process, 
                                  const char* comment,
				  OTF_KeyValueList* list );


/**
 * Write a begin process record
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param time      Time when process was referenced for the first time. 
 * @param process   Process identifier > 0.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
 
int OTF_Writer_writeBeginProcess( OTF_Writer* writer,
                                  uint64_t time,
                                  uint32_t process );


/**
 * Write a begin process record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeBeginProcess()
 * \ingroup writer
 */
int OTF_Writer_writeBeginProcessKV( OTF_Writer* writer,
                                  uint64_t time,
                                  uint32_t process,
				  OTF_KeyValueList* list );


/**
 * Write a end process record
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param time      Time when process was referenced for the last time. 
 * @param process   Process identifier > 0.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
 
int OTF_Writer_writeEndProcess( OTF_Writer* writer,
                                uint64_t time,
                                uint32_t process );


/**
 * Write a end process record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeEndProcess()
 * \ingroup writer
 */
int OTF_Writer_writeEndProcessKV( OTF_Writer* writer,
                                uint64_t time,
                                uint32_t process,
				OTF_KeyValueList* list );


/**
 * Write a file operation record
 * @deprecated This event record has been deprecated due to usage constraints.
 *             Please use OTF_Writer_writeBeginFileOperation() and
 *             OTF_Writer_writeEndFileOperation(), respectively.
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param time      Start time of the file operation. 
 * @param fileid    File identifier > 0.
 * @param handleid  File open identifier.
 * @param process   Process identifier > 0.
 * @param operation Type of file operation @see OTF_Handler_FileOperation()
 * @param bytes     Depends on operation @see OTF_Handler_FileOperation()
 * @param duration  time spent in the file operation
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeFileOperation( OTF_Writer* writer,
                                   uint64_t time,
                                   uint32_t fileid,
                                   uint32_t process,
                                   uint64_t handleid,
                                   uint32_t operation,
                                   uint64_t bytes,
                                   uint64_t duration,
                                   uint32_t source );


/**
 * Write a file operation record including an OTF_KeyValueList.
 * @deprecated This event record has been deprecated due to usage constraints.
 *             Please use OTF_Writer_writeBeginFileOperationKV() and
 *             OTF_Writer_writeEndFileOperationKV(), respectively.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeFileOperation()
 * \ingroup writer
 */
int OTF_Writer_writeFileOperationKV( OTF_Writer* writer,
                                   uint64_t time,
                                   uint32_t fileid,
                                   uint32_t process,
                                   uint64_t handleid,
                                   uint32_t operation,
                                   uint64_t bytes,
                                   uint64_t duration,
                                   uint32_t source,
				   OTF_KeyValueList* list );


/**
 * Write a begin file operation record
 *
 * @param writer      Initialized OTF_Writer instance.
 * @param time        Start time of file operation. 
 * @param process     Process identifier > 0.
 * @param matchingId  Operation identifier, used for finding the associated end
 *                    file operation event record.
 * @param scltoken    Optional reference to source code.
 *
 * @return            1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeBeginFileOperation( OTF_Writer* writer,
					uint64_t time,
					uint32_t process,
					uint64_t matchingId,
					uint32_t scltoken );


/**
 * Write a begin file operation record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeBeginFileOperation()
 * \ingroup writer
 */
int OTF_Writer_writeBeginFileOperationKV( OTF_Writer* writer,
					uint64_t time,
					uint32_t process,
					uint64_t matchingId,
					uint32_t scltoken,
					OTF_KeyValueList* list );


/**
 * Write an end file operation record
 *
 * @param writer      Initialized OTF_Writer instance.
 * @param time        End time of file operation. 
 * @param process     Process identifier > 0.
 * @param fileid      File identifier > 0.
 * @param matchingId  Operation identifier, must match a previous start file
 *                    operation event record.
 * @param handleId    Unique file open identifier.
 * @param operation   Type of file operation @see OTF_Handler_FileOperation()
 * @param bytes       Depends on operation @see OTF_Handler_FileOperation()
 * @param scltoken    Optional reference to source code.
 *
 * @return            1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeEndFileOperation( OTF_Writer* writer,
				      uint64_t time,
				      uint32_t process,
				      uint32_t fileid,
				      uint64_t matchingId,
                      uint64_t handleId,
				      uint32_t operation,
				      uint64_t bytes,
				      uint32_t scltoken );


/**
 * Write an end file operation record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeEndFileOperation()
 * \ingroup writer
 */
int OTF_Writer_writeEndFileOperationKV( OTF_Writer* writer,
				      uint64_t time,
				      uint32_t process,
				      uint32_t fileid,
				      uint64_t matchingId,
                      uint64_t handleId,
				      uint32_t operation,
				      uint64_t bytes,
				      uint32_t scltoken,
				      OTF_KeyValueList* list );


/**
 * Write a RMA put record - local end record.
 * The end of this transfer is marked by the NEXT end record on this <process>
 * with the same communicator/tag pair.
 *
 * @param writer      Initialized OTF_Writer instance.
 * @param time        Time when process was referenced for the last time.
 * @param process     Process initiating the transfer.
 * @param origin      If >0, Process whose memory will be transferred, instead
                      of this <process>.
 * @param target      Process whose memory will be written.
 * @param communicator Together with tag, it is used to identify the
 *                    corresponding RMA end record.
 * @param tag         Together with communicator, it is used to identify the
 *                    corresponding RMA end record.
 * @param bytes       How many bytes have been transfered by this call.
 * @param source      Explicit source code location or 0.
 *
 * @return            1 on success, 0 if an error occurs.
 *
 * \ingroup writer
 */
int OTF_Writer_writeRMAPut( OTF_Writer* writer,
                            uint64_t time,
                            uint32_t process,
                            uint32_t origin,
                            uint32_t target,
                            uint32_t communicator,
                            uint32_t tag,
                            uint64_t bytes,
                            uint32_t scltoken );


/**
 * Write a RMA put record - local end record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeRMAPut()
 * \ingroup writer
 */
int OTF_Writer_writeRMAPutKV( OTF_Writer* writer,
                            uint64_t time,
                            uint32_t process,
                            uint32_t origin,
                            uint32_t target,
                            uint32_t communicator,
                            uint32_t tag,
                            uint64_t bytes,
                            uint32_t scltoken,
			    OTF_KeyValueList* list );


/**
 * Write a RMA put record - remote end record.
 * The end of this transfer is marked by the NEXT end record on process <target>
 * with the same communicator/tag pair.
 *
 * @param writer      Initialized OTF_Writer instance.
 * @param time        Time when process was referenced for the last time.
 * @param process     Process initiating the transfer.
 * @param origin      If >0, Process whose memory will be transferred, instead
                      of this <process>.
 * @param target      Process whose memory will be written and where the end
 *                    record is located.
 * @param communicator Together with tag, it is used to identify the
 *                    corresponding RMA end record.
 * @param tag         Together with communicator, it is used to identify the
 *                    corresponding RMA end record.
 * @param bytes       How many bytes have been transfered by this call.
 * @param source      Explicit source code location or 0.
 *
 * @return            1 on success, 0 if an error occurs.
 *
 * \ingroup writer
 */
int OTF_Writer_writeRMAPutRemoteEnd( OTF_Writer* writer,
                                     uint64_t time,
                                     uint32_t process,
                                     uint32_t origin,
                                     uint32_t target,
                                     uint32_t communicator,
                                     uint32_t tag,
                                     uint64_t bytes,
                                     uint32_t scltoken );


/**
 * Write a RMA put record - remote end record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeRMAPutRemoteEnd()
 * \ingroup writer
 */
int OTF_Writer_writeRMAPutRemoteEndKV( OTF_Writer* writer,
                                     uint64_t time,
                                     uint32_t process,
                                     uint32_t origin,
                                     uint32_t target,
                                     uint32_t communicator,
                                     uint32_t tag,
                                     uint64_t bytes,
                                     uint32_t scltoken,
				     OTF_KeyValueList* list );


/**
 * Write a RMA get record.
 * The end of this transfer is marked by the NEXT end record on this <process>
 * with the same communicator/tag pair.
 *
 * @param writer      Initialized OTF_Writer instance.
 * @param time        Time when process was referenced for the last time.
 * @param process     Process initiating the transfer.
 * @param origin      If >0, Process where data will be transferred to (instead
                      of this <process>).
 * @param target      Process whose memory will be read.
 * @param communicator Together with tag, it is used to identify the
 *                    corresponding RMA end record.
 * @param tag         Together with communicator, it is used to identify the
 *                    corresponding RMA end record.
 * @param bytes       How many bytes have been transfered by this call.
 * @param source      Explicit source code location or 0.
 *
 * @return            1 on success, 0 if an error occurs.
 *
 * \ingroup writer
 */
int OTF_Writer_writeRMAGet( OTF_Writer* writer,
                            uint64_t time,
                            uint32_t process,
                            uint32_t origin,
                            uint32_t target,
                            uint32_t communicator,
                            uint32_t tag,
                            uint64_t bytes,
                            uint32_t scltoken );


/**
 * Write a RMA get record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeRMAGet()
 * \ingroup writer
 */
int OTF_Writer_writeRMAGetKV( OTF_Writer* writer,
                            uint64_t time,
                            uint32_t process,
                            uint32_t origin,
                            uint32_t target,
                            uint32_t communicator,
                            uint32_t tag,
                            uint64_t bytes,
                            uint32_t scltoken,
			    OTF_KeyValueList* list );


/**
 * Write a RMA end record.
 * The end record marks the finalization of all put and get operations with the
 * same communicator/tag pair that occured so far for this <process>.
 *
 * @param writer      Initialized OTF_Writer instance.
 * @param time        Time when process was referenced for the last time.
 * @param process     Process identifier > 0.
 * @param remote      If >0, ends RMA transfers on Process <remote>, instead of
                      this <process>.
                      [remote!=0 is really weird crap and would never be used by
                       sane programmers ;-) -- nevertheless, the IBM Cell could
                       be programmed like this.]
 * @param communicator Together with tag, it is used to identify the
 *                    related RMA put/get records.
 * @param tag         Together with communicator, it is used to identify the
 *                    related RMA put/get records.
 * @param source      Explicit source code location or 0.
 *
 * @return            1 on success, 0 if an error occurs.
 *
 * \ingroup writer
 */
int OTF_Writer_writeRMAEnd( OTF_Writer* writer,
                            uint64_t time,
                            uint32_t process,
                            uint32_t remote,
                            uint32_t communicator,
                            uint32_t tag,
                            uint32_t scltoken );


/**
 * Write a RMA end record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeRMAEnd()
 * \ingroup writer
 */
int OTF_Writer_writeRMAEndKV( OTF_Writer* writer,
                            uint64_t time,
                            uint32_t process,
                            uint32_t remote,
                            uint32_t communicator,
                            uint32_t tag,
                            uint32_t scltoken,
			    OTF_KeyValueList* list );


/* *** public snapshot record write handlers *** */

/**
 * Write a snapshot comment record.
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param time      Comments need a timestamp for a proper positioning in the 
 *                  trace.
 * @param process   Comments also need a process identifier for a proper 
 *                  positioning in the trace. 
 * @param comment   Arbitrary comment string.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
 
int OTF_Writer_writeSnapshotComment( OTF_Writer* writer, 
                                  uint64_t time, 
                                  uint32_t process, 
                                  const char* comment );


/**
 * Write a snapshot comment record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeSnapshotComment()
 * \ingroup writer
 */
int OTF_Writer_writeSnapshotCommentKV( OTF_Writer* writer, 
                                  uint64_t time, 
                                  uint32_t process, 
                                  const char* comment,
				  OTF_KeyValueList* list );


/** 
 * Write an enter snapshot which provides information about a past
 * function call
 *
 * @param writer        Initialized OTF_Writer instance.
 * @param time          Time when the snapshot was written(current time).
 * @param originaltime  Time when the according enter record was entered.
 *                      This call is still on the stack.(It has not been left
 *                      yet)
 * @param function      Function that the has been entered
 *                      OTF_Writer_defFunction.
 * @param process       Process where action took place.
 * @param source        Optional reference to source code.
 *
 * @return              1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeEnterSnapshot( OTF_Writer* writer, 
                           uint64_t time, 
                           uint64_t originaltime, 
                           uint32_t function, 
                           uint32_t process, 
                           uint32_t source );


/** 
 * Write an enter snapshot including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeEnterSnapshot()
 * \ingroup writer
 */
int OTF_Writer_writeEnterSnapshotKV( OTF_Writer* writer, 
                           uint64_t time, 
                           uint64_t originaltime, 
                           uint32_t function, 
                           uint32_t process, 
                           uint32_t source,
			   OTF_KeyValueList* list );


/**
 * Write a send snapshot which provides information about a past
 * message send operation that is still pending, i.e. not yet received
 *
 * @param writer        Initialized OTF_Writer instance.
 * @param time          Time when the snapshot was written(current time).
 * @param originaltime  Time when the message was sent
 * @param sender        Sender of the message.
 * @param receiver      Receiver of the message.
 * @param procGroup     Optional process-group sender and receiver belong to,
 *                      '0' for no group.
 * @param tag           Optional message type information.
 * @param length        Optional message length information.
 * @param source        Optional reference to source code.
 *
 * @return              1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeSendSnapshot( OTF_Writer* writer,
                                  uint64_t time,
                                  uint64_t originaltime,
                                  uint32_t sender,
                                  uint32_t receiver,
                                  uint32_t procGroup,
                                  uint32_t tag,
                                  uint32_t length,
                                  uint32_t source );


/**
 * Write a send snapshot including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeSendSnapshot()
 * \ingroup writer
 */
int OTF_Writer_writeSendSnapshotKV( OTF_Writer* writer,
                                  uint64_t time,
                                  uint64_t originaltime,
                                  uint32_t sender,
                                  uint32_t receiver,
                                  uint32_t procGroup,
                                  uint32_t tag,
                                  uint32_t length,
                                  uint32_t source,
				                  OTF_KeyValueList* list );


/**
 * Write a snapshot record for an open (and not yet closed) file
 *
 * @param writer        Initialized OTF_Writer instance.
 * @param time          Time when the snapshot was written(current time).
 * @param originaltime  Time when the message was sent.
 * @param fileid        File identifier.
 * @param process       Process where the file was opened.
 * @param handleid      Unique file open identifier. @see OTF_Handler_FileOperation()
 * @param source        Optional reference to source code.
 *
 * @return              1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeOpenFileSnapshot( OTF_Writer* writer,
                                      uint64_t time,
                                      uint64_t originaltime,
                                      uint32_t fileid,
                                      uint32_t process,
                                      uint64_t handleid,
                                      uint32_t source );


/**
 * Write a snapshot record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeOpenFileSnapshot()
 * \ingroup writer
 */
int OTF_Writer_writeOpenFileSnapshotKV( OTF_Writer* writer,
                                      uint64_t time,
                                      uint64_t originaltime,
                                      uint32_t fileid,
                                      uint32_t process,
                                      uint64_t handleid,
                                      uint32_t source,
				                      OTF_KeyValueList* list );


/**
 * Write a snapshot record for an unfinished collective operation.
 *
 * @param writer        Initialized OTF_Writer instance.
 * @param time          Time when the snapshot was written(current time).
 * @param originaltime  Time when the collective operation began.
 * @param process       Process identifier i.e. collective member. 
 * @param collOp        Collective identifier to be defined with
 *                      OTF_Writer_writeDefCollectiveOperation(). 
 * @param matchingId    Identifier for finding the associated end collective event
 *                      record. It must be unique within this procGroup.
 * @param procGroup     Group of processes participating in this collective.
 * @param rootProc      Root process if != 0.
 * @param sent          Data volume sent by member or 0.
 * @param received      Data volume received by member or 0.
 * @param scltoken      Explicit source code location or 0.
 *
 * @return              1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeBeginCollopSnapshot( OTF_Writer* writer,
					      uint64_t time,
                          uint64_t originaltime,
					      uint32_t process,
					      uint32_t collOp,
					      uint64_t matchingId,
					      uint32_t procGroup,
					      uint32_t rootProc,
					      uint64_t sent,
					      uint64_t received,
					      uint32_t scltoken );


/**
 * Write a snapshot record for an unfinished collective operation including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeBeginCollopSnapshot()
 * \ingroup writer
 */
int OTF_Writer_writeBeginCollopSnapshotKV( OTF_Writer* writer,
					      uint64_t time,
                          uint64_t originaltime,
					      uint32_t process,
					      uint32_t collOp,
					      uint64_t matchingId,
					      uint32_t procGroup,
					      uint32_t rootProc,
					      uint64_t sent,
					      uint64_t received,
					      uint32_t scltoken,
					      OTF_KeyValueList* list );


/**
 * Write a snapshot for an unfinished file operation.
 *
 * @param writer        Initialized OTF_Writer instance.
 * @param time          Time when the snapshot was written(current time).
 * @param originaltime  Time when the operation began.
 * @param process       Process identifier > 0.
 * @param matchingId    Operation identifier, used for finding the associated end
 *                      file operation event record.
 * @param scltoken      Optional reference to source code.
 *
 * @return              1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeBeginFileOpSnapshot( OTF_Writer* writer,
					uint64_t time,
                    uint64_t originaltime,
					uint32_t process,
					uint64_t matchingId,
					uint32_t scltoken );


/**
 * Write a snapshot for an unfinished file operation including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeBeginFileOperation()
 * \ingroup writer
 */
int OTF_Writer_writeBeginFileOpSnapshotKV( OTF_Writer* writer,
					uint64_t time,
                    uint64_t originaltime,
					uint32_t process,
					uint64_t matchingId,
					uint32_t scltoken,
					OTF_KeyValueList* list );
                           
/**
 * Write a snapshot for the count of finished collective operations from this
 * process on this communicator.
 *
 * @param writer        Initialized OTF_Writer instance.
 * @param time          Time when the snapshot was written(current time).
 * @param process       Process identifier > 0.
 * @param communicator  The communicator for what the count is for.
 * @param count         The count of completed collective operations from process @.
 *
 * @param list          Initialized OTF_KeyValueList() instance or NULL.
 *
 * @return              1 on success, 0 if an error occurs.
 *
 * \ingroup writer
 */
int OTF_Writer_writeCollopCountSnapshot( OTF_Writer* writer,
                                         uint64_t time,
                                         uint32_t process,
                                         uint32_t communicator,
                                         uint64_t count,
                                         OTF_KeyValueList *list );


/**
 * Write a snapshot for the last value of a counter into the snapshot.
 *
 * @param writer        Initialized OTF_Writer instance.
 * @param time          Time when the snapshot was written(current time).
 * @param originaltime  Sample time of the value.
 * @param process       Process identifier > 0.
 * @param counter       The counter.
 * @param value         The value of the counter.
 *
 * @param list          Initialized OTF_KeyValueList() instance or NULL.
 *
 * @return              1 on success, 0 if an error occurs.
 *
 * \ingroup writer
 */
int OTF_Writer_writeCounterSnapshot( OTF_Writer*       writer,
                                     uint64_t          time,
                                     uint64_t          originaltime,
                                     uint32_t          process,
                                     uint32_t          counter,
                                     uint64_t          value,
                                     OTF_KeyValueList *list );

/* *** public statistics record write handlers *** */


/**
 * Write a summary comment record.
 *
 * @param writer    Initialized OTF_Writer instance.
 * @param time      Comments need a timestamp for a proper positioning in the 
 *                  trace.
 * @param process   Comments also need a process identifier for a proper 
 *                  positioning in the trace. 
 * @param comment   Arbitrary comment string.
 *
 * @return          1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
 
int OTF_Writer_writeSummaryComment( OTF_Writer* writer, 
                                  uint64_t time, 
                                  uint32_t process, 
                                  const char* comment );


/**
 * Write a summary comment record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeSummaryComment()
 * \ingroup writer
 */
int OTF_Writer_writeSummaryCommentKV( OTF_Writer* writer, 
                                  uint64_t time, 
                                  uint32_t process, 
                                  const char* comment,
				  OTF_KeyValueList* list );


/**
 * Write a function summary record.
 *
 * @param writer       Initialized OTF_Writer instance.
 * @param time         Time when summary was computed. 
 * @param function     Function as defined with 
 *                     OTF_Handler_DefFunction.
 * @param process      Process of the given function.
 * @param count        Number of invocations.
 * @param excltime     Time spent exclusively in the given function.
 * @param incltime     Time spent in the given function including all
 *                     sub-routine calls.
 *
 * @return             1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */

int OTF_Writer_writeFunctionSummary( OTF_Writer* writer, 
        uint64_t time, uint32_t function, uint32_t process, 
        uint64_t count, uint64_t excltime, uint64_t incltime );


/**
 * Write a function summary record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeFunctionSummary()
 * \ingroup writer
 */
int OTF_Writer_writeFunctionSummaryKV( OTF_Writer* writer, 
        uint64_t time, uint32_t function, uint32_t process, 
        uint64_t count, uint64_t excltime, uint64_t incltime,
	OTF_KeyValueList* list );


/**
 * Write a functiongroup summary record.
 *
 * @param writer         Initialized OTF_Writer instance.
 * @param time           Time when summary was computed. 
 * @param functiongroup  Function group as defined with 
 *                       OTF_Handler_DefFunctionGroup.
 * @param process        Process of the given function group.
 * @param count          Number of invocations.
 * @param excltime       Time spent exclusively in the given function group.
 * @param incltime       Time spent in the given function group including all
 *                       sub-routine calls.
 *
 * @return               1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */

int OTF_Writer_writeFunctionGroupSummary( OTF_Writer* writer, 
        uint64_t time,  uint32_t functiongroup,  uint32_t process,  
        uint64_t count,  uint64_t excltime,  uint64_t incltime );


/**
 * Write a functiongroup summary record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeFunctionGroupSummary()
 * \ingroup writer
 */
int OTF_Writer_writeFunctionGroupSummaryKV( OTF_Writer* writer, 
        uint64_t time,  uint32_t functiongroup,  uint32_t process,  
        uint64_t count,  uint64_t excltime,  uint64_t incltime,
	OTF_KeyValueList* list );


/**
 * Write a message summary record.
 *
 * @param writer         Initialized OTF_Writer instance.
 * @param time           Time when summary was computed. 
 * @param process        Process where messages originated.
 * @param peer           Process where the message is sent to
 * @param comm           Communicator of message summary
 * @param tag            Message type/tag.
 * @param number_sent    The number of messages sent.
 * @param number_recved  The number of messages received.
 * @param bytes_sent     The number of bytes sent via messages of the given
 *                       type.
 * @param bytes_recved   The number of bytes received through messages of the 
 *                       given type.
 *
 * @return               1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */

int OTF_Writer_writeMessageSummary( OTF_Writer* writer, 
        uint64_t time, uint32_t process, uint32_t peer,
        uint32_t comm, uint32_t tag,  uint64_t number_sent,
        uint64_t number_recved, uint64_t bytes_sent, uint64_t bytes_recved );


/**
 * Write a message summary record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeMessageSummary()
 * \ingroup writer
 */
int OTF_Writer_writeMessageSummaryKV( OTF_Writer* writer, 
        uint64_t time, uint32_t process, uint32_t peer,
        uint32_t comm, uint32_t tag,  uint64_t number_sent,
        uint64_t number_recved, uint64_t bytes_sent, uint64_t bytes_recved,
	OTF_KeyValueList* list );


/**
 * Write a summary record of collective operations.
 *
 * @param writer         Initialized OTF_Writer instance.
 * @param time           Time when summary was computed.
 * @param process        Process identifier i.e. collective member.
 * @param comm		 Communicator of collective operation summary.
 * @param collective     Collective identifier to be defined with
 *                       OTF_Writer_writeDefCollectiveOperation().
 * @param number_sent    The number of messages sent by member or 0.
 * @param number_recved  The number of messages received by member or 0.
 * @param bytes_sent     The number of bytes sent by member or 0.
 * @param bytes_recved   The number of bytes received by member or 0.
 *
 * @return               1 on success, 0 if an error occurs.
 *
 * \ingroup writer
 */

int OTF_Writer_writeCollopSummary( OTF_Writer* writer, 
        uint64_t time, uint32_t process, uint32_t comm, uint32_t collective,
	uint64_t number_sent, uint64_t number_recved, uint64_t bytes_sent, uint64_t bytes_recved );


/**
 * Write a summary record of collective operations including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeCollopSummary()
 * \ingroup writer
 */
int OTF_Writer_writeCollopSummaryKV( OTF_Writer* writer, 
        uint64_t time, uint32_t process, uint32_t comm, uint32_t collective,
	uint64_t number_sent, uint64_t number_recved, uint64_t bytes_sent, uint64_t bytes_recved,
	OTF_KeyValueList* list );

/**
 * Writes a file operation summary record.
 *
 * @param writer         Initialized OTF_Writer instance.
 * @param time           Time when summary was computed.
 * @param fileid         File identifier or 0 for all  files.
 * @param process        Process where file operations occured.
 * @param nopen          Number of files opened.
 * @param nclose         Number of files closed.
 * @param nread          Number of read events.
 * @param nwrite         Number of write events.
 * @param nseek          Number of seek events.
 * @param bytesread      Number of bytes read.
 * @param byteswrite     Number of bytes written.
 *
 * @return               1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeFileOperationSummary( OTF_Writer* writer, uint64_t time,
	uint32_t fileid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite );


/**
 * Writes a file operation summary record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeFileOperationSummary()
 * \ingroup writer
 */
int OTF_Writer_writeFileOperationSummaryKV( OTF_Writer* writer, uint64_t time,
	uint32_t fileid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite, OTF_KeyValueList* list );


/**
 * Writes a file group operation summary record.
 *
 * @param writer         Initialized OTF_Writer instance.
 * @param time           Time when summary was computed.
 * @param groupid        File group identifier or 0 for all  files/groups.
 * @param process        Process where file operations occured.
 * @param nopen          Number of files opened.
 * @param nclose         Number of files closed.
 * @param nread          Number of read events.
 * @param nwrite         Number of write events.
 * @param nseek          Number of seek events.
 * @param bytesread      Number of bytes read.
 * @param byteswrite     Number of bytes written.
 *
 * @return               1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeFileGroupOperationSummary( OTF_Writer* writer, uint64_t time,
	uint32_t groupid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite );


/**
 * Writes a file group operation summary record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeFileGroupOperationSummary()
 * \ingroup writer
 */
int OTF_Writer_writeFileGroupOperationSummaryKV( OTF_Writer* writer, uint64_t time,
	uint32_t groupid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite, OTF_KeyValueList* list );
/* *** private member functions *** */


/* *** marker record types *** */


/**
 * Writes a def marker record.
 *
 * @param writer         Initialized OTF_Writer instance.
 * qparam streamID       stream identifier that must be 0, any other value is ignored
 * @param token          The newly defined marker token.
 * @param name           Its name
 * @param type           Marker type, one of OTF_MARKER_TYPE_xxx
 * *
 * @return               1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeDefMarker( OTF_Writer* writer, 
                               uint32_t streamID,
                               uint32_t token, 
                               const char* name, 
                               uint32_t type );


/**
 * Writes a def marker record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeDefMarker()
 * \ingroup writer
 */
int OTF_Writer_writeDefMarkerKV( OTF_Writer* writer, 
                               uint32_t streamID,
                               uint32_t token, 
                               const char* name, 
                               uint32_t type,
				OTF_KeyValueList* list );

/**
 * Writes a marker record.
 *
 * @param writer         Initialized OTF_Writer instance.
 * @param time           Time stamp of the marker record. Note that marker records are 
 *                       not sorted according to time stamps!
 * @param process        The process or process group of the marker.
 * @param token          A marker token defined by 'DefMarker' before.
 * @param text           Descriptive text. *
 * @return               1 on success, 0 if an error occurs.       
 *
 * \ingroup writer
 */
int OTF_Writer_writeMarker( OTF_Writer* writer, 
                            uint64_t time, 
                            uint32_t process, 
                            uint32_t token, 
                            const char* text );


/**
 * Writes a marker record including an OTF_KeyValueList.
 *
 * @param list      Initialized OTF_KeyValueList() instance or NULL.
 *
 * @see OTF_Writer_writeMarker()
 * \ingroup writer
 */
int OTF_Writer_writeMarkerKV( OTF_Writer* writer, 
                            uint64_t time, 
                            uint32_t process, 
                            uint32_t token, 
                            const char* text,
			    OTF_KeyValueList* list );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_WRITER_H */

