/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_WStream.h
 *
 *  @brief Provides write access to trace streams, which consist of multiple
 *  buffers.
 *
 *  \ingroup wstream
 */

/** \defgroup wstream Stream Writer Interface
 *
 * This interface provides functions for writing trace at stream level.
 * A stream is part of a trace and consists of up to four buffers
 * (event buffer, definition buffer, snapshots buffer, statistics buffer).
 * 
 * wstream is structured similarly to writer.
 *
 * Use this interface, if you want to a specific stream and the writer
 * interface does not provide the desired access.
 *
 * \section wstream_example A short Example
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
 * 	    OTF_WStream* wstream;
 * 	    \endcode
 *
 *     	Initialize the file manager. Open at most 100 OS files.
 *     	\code
 *     	manager= OTF_FileManager_open( 100 );
 * 	    assert( manager );
 * 	    \endcode
 *
 * 	    Initialize the wstream object. Open file "test", writing the first stream.
 * 	    \code
 * 	    wstream = OTF_WStream_open( "test", 0, manager );
 *      assert( wstream );
 *      \endcode
 *      
 *      Write some definition records.
 *      \code
 *      OTF_WStream_writeDefTimerResolution( wstream, 1000 );
 *      OTF_WStream_writeDefProcess( wstream, 1, "proc one", 0 );
 *      \endcode
 *
 *      Clean up before exiting the program.
 *      \code
 *      OTF_WStream_close( wstream );
 *      OTF_FileManager_close( manager );
 *
 *		return 0;
 * }
 * \endcode
 *
 * Compile this using $ gcc -o test test.c `otfconfig --libs`.
 *
 * When executing this program it only writes one file (test.0.def),
 * containg the written records.
 *
 */

#ifndef OTF_WSTREAM_H
#define OTF_WSTREAM_H


#include <stdlib.h>


#include "OTF_inttypes.h"


#include "OTF_Definitions.h"
#include "OTF_FileManager.h"
#include "OTF_WBuffer.h"
#include "OTF_Filenames.h"


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

struct struct_OTF_WStream {


	/**	name stub: all files will begin with this name */
	char* namestub;

	/**	Unique id for the current stream. */
	uint32_t id;

	/**	State wether to use long or short format,
		see OTF_WSTREAM_FORMAT_XXX macros above. */
	uint32_t format;


	/**	Definitions buffer. Definitions buffer carries definition
		records. */
	OTF_WBuffer* defBuffer;

	/**	Event buffer. The event buffer carries records for actual
		events, i.e. records with a time stamp */
	OTF_WBuffer* eventBuffer;

	/**	Snaps (snapshots) buffer. The snapshots buffer carries
		snapshots of the whole state at a point in time - as oppossed to
		events which only show changes in the state. This can be used to
		start from such a snapshot instead of from the very begining. */
	OTF_WBuffer* snapsBuffer;

	/**	Statistics buffer. Statistics buffer carries statistical
		information information about a certain time interval resp.
		data at points of time that allow to derive statistics without
		reading through all events of that interval. */
	OTF_WBuffer* statsBuffer;

	/**	Marker buffer. Marker buffer carries marker definitions and spots
		which live in a separate file and wich are not sorted according to time stamp
        in any way. */
	OTF_WBuffer* markerBuffer;

	/** Default compression method for all buffers managed by this stream */
	OTF_FileCompression compression;
	
	/** Default size of buffers managed by this WStream. */
	uint32_t buffersizes;

#ifdef HAVE_ZLIB
	/** Default size of zbuffers managed by this RStream. */
	uint32_t zbuffersizes;
#endif /* HAVE_ZLIB */
	
	/** file handle manager */
	OTF_FileManager* manager;
};
/** wstream object \ingroup wstream */
typedef struct struct_OTF_WStream OTF_WStream;


/**     
 * Create a new OTF_WStream instance.
 *
 * @param namestub     File name prefix which is going to be used by 
 *                     all sub-files which belong to the writer stream.
 * @param id           Abitrary but unique identifier of the writer stream.
		       Must be > '0' for real streams. Use '0' for global definitions.
 * @param manager      File handle manager. 
 *
 * @return             Initialized OTF_WStream instance or 0 if an error
 *                     occurred.
 *
 * \ingroup wstream
 */
OTF_WStream* OTF_WStream_open( const char* namestub, uint32_t id, 
	OTF_FileManager* manager );


/** 
 * Close an OTF_WStream instance and all its related files.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 * 
 * @return         1 if instance was closed successfully and 0 otherwise.
 *
 * \ingroup wstream
 */
int OTF_WStream_close( OTF_WStream* wstream );

			
/** 
 * Flush an OTF_WStream instance, i.e. flush all associated buffers if existing.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 * 
 * @return         1 if everything flushed successfully and 0 otherwise.
 *
 * \ingroup wstream
 */
int OTF_WStream_flush( OTF_WStream* wstream );


/** 
 * Returns the definition buffer of the according writer stream.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 * 
 * @return         Initialized OTF_WBuffer instance or 0 if an error occured.
 *
 * \ingroup wstream
 */
OTF_WBuffer* OTF_WStream_getDefBuffer( OTF_WStream* wstream );


/** 
 * Returns the event buffer of the according writer stream.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 * 
 * @return         Initialized OTF_WBuffer instance or 0 if an error occured.
 *
 * \ingroup wstream
 */
OTF_WBuffer* OTF_WStream_getEventBuffer( OTF_WStream* wstream );


/** 
 * Returns the snapshots buffer of the according writer stream.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 * 
 * @return         Initialized OTF_WBuffer instance or 0 if an error occured.
 *
 * \ingroup wstream
 */
OTF_WBuffer* OTF_WStream_getSnapshotBuffer( OTF_WStream* wstream );


/** 
 * Returns the statistics buffer of the according writer stream.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 * 
 * @return         Initialized OTF_WBuffer instance or 0 if an error occured.
 *
 * \ingroup wstream
 */
OTF_WBuffer* OTF_WStream_getStatsBuffer( OTF_WStream* wstream );


/** 
 * Returns the marker buffer of the according writer stream.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 * 
 * @return         Initialized OTF_WBuffer instance or 0 if an error occured.
 *
 * \ingroup wstream
 */
OTF_WBuffer* OTF_WStream_getMarkerBuffer( OTF_WStream* wstream );


/**
 * Set the standard compression method for all buffers managed by this writer
 * stream
 *
 * @param wstream      Pointer to an initialized OTF_WStream object. See 
 *                     also OTF_WStream_open().
 *
 * @param compression  Default compression level.
 *                     0-9, where 0 means no compression is applied, and 9 is
 *                     the highest level of compression.
 *
 * @return             1 on success, 0 if an error occurs.
 *
 * \ingroup wstream
 */
int OTF_WStream_setCompression( OTF_WStream* wstream, OTF_FileCompression
	compression );
	
	
/**
 * Return the standard compression method for all buffers managed by this writer
 * stream
 *
 * @param wstream      Pointer to an initialized OTF_WStream object. See 
 *                     also OTF_WStream_open().
 *
 * @return             Standard compression level for all buffers managed by
 *                     this writer stream.
 *
 * \ingroup wstream
 */
OTF_FileCompression OTF_WStream_getCompression( OTF_WStream* wstream );


/**
 * Set the default buffer size for all buffers managed by this writer stream. 
 * This is only effective for future buffers and will not change already 
 * allocated buffers. Those can be changed with the buffers directly.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 * 
 * @param size     Intended buffer size.
 *
 * \ingroup wstream
 */
void OTF_WStream_setBufferSizes( OTF_WStream* wstream, uint32_t size );

/** 
 * Get the default buffer size for all buffers managed by this writer stream.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 * 
 * @return         Default buffer size for all buffers managed by this writer
 *                 stream.
 *
 * \ingroup wstream
 */
uint32_t OTF_WStream_getBufferSizes( OTF_WStream* wstream );

/** 
 * Set the default zbuffer size for all files managed by this writer stream.
 * This is only effective for future files and will not change already
 * allocated buffers. Those can be changed with the files directly.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 *
 * @param size     Intended buffer size.
 *
 * \ingroup wstream
 */
void OTF_WStream_setZBufferSizes( OTF_WStream* wstream, uint32_t size );


/** 
 * Get the default zbuffer size for all files managed by this writer stream.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 *
 * @return         Default buffer size for all buffers managed by this reader
 *                 stream.
 *
 * \ingroup wstream
 */
uint32_t OTF_WStream_getZBufferSizes( OTF_WStream* wstream );

/**
 * Set the default ouput format.
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 *
 * @param format   Intended output format (OTF_WSTREAM_FORMAT_{LONG,SHORT})
 *
 * \ingroup wstream
 */
void OTF_WStream_setFormat( OTF_WStream* wstream, uint32_t format );

/**
 * Get the default output format
 *
 * @param wstream  Pointer to an initialized OTF_WStream object. See 
 *                 also OTF_WStream_open().
 *
 * @return         Default output format.
 *
 * \ingroup wstream
 */
uint32_t OTF_WStream_getFormat( OTF_WStream* wstream );


/* *** definition record write handlers *** ******************************** */


/**	Write a DEFINITIONCOMMENT record to stream 'wstream'. 
 * @see OTF_Writer_writeDefinitionComment()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefinitionComment( OTF_WStream* wstream,
	const char* comment );

/**	Write a DEFINITIONCOMMENT record including an OTF_KeyValueList to stream 'wstream'. 
 * @see OTF_Writer_writeDefinitionCommentEV()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefinitionCommentKV( OTF_WStream* wstream,
	const char* comment, OTF_KeyValueList* list );


/**	Write a DEFTIMERRESOLUTION record to stream 'wstream'.
 * @see OTF_Writer_writeDefTimerResolution()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefTimerResolution( OTF_WStream* wstream,
	uint64_t ticksPerSecond );

/**	Write a DEFTIMERRESOLUTION record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeDefTimerResolutionEV()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefTimerResolutionKV( OTF_WStream* wstream,
	uint64_t ticksPerSecond, OTF_KeyValueList* list );

/**	Write a DEFPROCESS record to stream 'wstream'.
 * @see OTF_Writer_writeDefProcess()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefProcess( OTF_WStream* wstream, uint32_t deftoken,
	const char* name, uint32_t parent );

/**	Write a DEFPROCESS record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeDefProcessKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefProcessKV( OTF_WStream* wstream, uint32_t deftoken,
	const char* name, uint32_t parent, OTF_KeyValueList* list );

/**	Write a DEFPROCESSGROUP record to stream 'wstream'.
 * @see OTF_Writer_writeDefProcessGroup()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefProcessGroup( OTF_WStream* wstream, uint32_t deftoken,
	const char* name, uint32_t n, const uint32_t* array );

/**	Write a DEFPROCESSGROUP record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeDefProcessGroupKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefProcessGroupKV( OTF_WStream* wstream, uint32_t deftoken,
	const char* name, uint32_t n, const uint32_t* array, OTF_KeyValueList* list );

/**	Write a DEFATTRLIST record to stream 'wstream'.
 * @see OTF_Writer_writeDefAttributeList()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefAttributeList( OTF_WStream* wstream, uint32_t attr_token,
	uint32_t num, OTF_ATTR_TYPE* array );

/**	Write a DEFATTRLIST record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeDefAttributeListKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefAttributeListKV( OTF_WStream* wstream, uint32_t attr_token,
	uint32_t num, OTF_ATTR_TYPE* array, OTF_KeyValueList* list );

/**	Write a DEFPROCESSORGROUPATTR record to stream 'wstream'.
 * @see OTF_Writer_writeDefAttributeList()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefProcessOrGroupAttributes( OTF_WStream* wstream,
	uint32_t proc_token, uint32_t attr_token );

/**	Write a DEFPROCESSORGROUPATTR record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeDefAttributeListKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefProcessOrGroupAttributesKV( OTF_WStream* wstream,
	uint32_t proc_token, uint32_t attr_token, OTF_KeyValueList* list );

/**	Write a DEFFUNCTION record to stream 'wstream'.
 * @see OTF_Writer_writeDefFunction()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefFunction( OTF_WStream* wstream, uint32_t deftoken,
	const char* name, uint32_t group, uint32_t scltoken );

/**	Write a DEFFUNCTION record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeDefFunctionKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefFunctionKV( OTF_WStream* wstream, uint32_t deftoken,
	const char* name, uint32_t group, uint32_t scltoken, OTF_KeyValueList* list );

/**	Write a DEFFUNCTIONGROUP record to stream 'wstream'.
 * @see OTF_Writer_writeDefFunctionGroup()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefFunctionGroup( OTF_WStream* wstream,
	uint32_t deftoken, const char* name );

/**	Write a DEFFUNCTIONGROUP record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeDefFunctionGroupKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefFunctionGroupKV( OTF_WStream* wstream,
	uint32_t deftoken, const char* name, OTF_KeyValueList* list );

/** Write a DEFCOLLECTIVEOPERATION record to stream 'wstream'.
 * @see OTF_Writer_writeDefCollectiveOperation()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefCollectiveOperation( OTF_WStream* wstream, 
	uint32_t collOp, const char* name, uint32_t type );

/** Write a DEFCOLLECTIVEOPERATION record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeDefCollectiveOperationKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefCollectiveOperationKV( OTF_WStream* wstream, 
	uint32_t collOp, const char* name, uint32_t type, OTF_KeyValueList* list );

/**	Write a DEFCOUNTER record to stream 'wstream'.
 * @see OTF_Writer_writeDefCounter()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefCounter( OTF_WStream* wstream, uint32_t deftoken,
	const char* name, uint32_t properties, uint32_t countergroup, 
	const char* unit );

/**	Write a DEFCOUNTER record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeDefCounterKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefCounterKV( OTF_WStream* wstream, uint32_t deftoken,
	const char* name, uint32_t properties, uint32_t countergroup, 
	const char* unit, OTF_KeyValueList* list );

/**	Write a DEFCOUNTERGROUP record to stream 'wstream'.
 * @see OTF_Writer_writeDefCounterGroup()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefCounterGroup( OTF_WStream* wstream,
	uint32_t deftoken, const char* name );

/**	Write a DEFCOUNTERGROUP record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeDefCounterGroupKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefCounterGroupKV( OTF_WStream* wstream,
	uint32_t deftoken, const char* name, OTF_KeyValueList* list );

/**	Write a DEFSCL record to stream 'wstream'. 
 * @see OTF_Writer_writeDefScl()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefScl( OTF_WStream* wstream, uint32_t deftoken,
	uint32_t sclfile, uint32_t sclline );

/**	Write a DEFSCL record including an OTF_KeyValueList to stream 'wstream'. 
 * @see OTF_Writer_writeDefSclKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefSclKV( OTF_WStream* wstream, uint32_t deftoken,
	uint32_t sclfile, uint32_t sclline, OTF_KeyValueList* list );

/**	Write a DEFSCLFILE record to stream 'wstream'.
 * @see OTF_Writer_writeDefSclFile()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefSclFile( OTF_WStream* wstream,
	uint32_t deftoken, const char* filename );

/**	Write a DEFSCLFILE record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeDefSclFileKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefSclFileKV( OTF_WStream* wstream,
	uint32_t deftoken, const char* filename, OTF_KeyValueList* list );

/**	Write a DEFCREATOR record to stream 'wstream'.
 * @see OTF_Writer_writeDefCreator()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefCreator( OTF_WStream* wstream, const char* creator );

/**	Write a DEFCREATOR record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeDefCreatorKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefCreatorKV( OTF_WStream* wstream, const char* creator, OTF_KeyValueList* list );

/**
 * Write a DEFUNIQUEID record to stream 'wstream'.
 * This record is generated automatically at beginning of tracing in the global
 * definition stream.
 *
 * @param wstream      Initialized OTF_WStream instance.
 * @return             1 on success, 0 if an error occurs.
 *
 * \ingroup wstream
 */
int OTF_WStream_writeUniqueId( OTF_WStream* wstream );

/** Write a DEFVERSION record to stream 'wstream'.
 * @see OTF_Writer_writeOtfVersion()
 * \ingroup wstream 
 */
int OTF_WStream_writeOtfVersion( OTF_WStream* wstream );

/** Write a DEFFILE record to stream 'wstream'. 
 * @see OTF_Writer_writeDefFile()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefFile( OTF_WStream* wstream, uint32_t token,
	const char* name, uint32_t group );

/** Write a DEFFILE record including an OTF_KeyValueList to stream 'wstream'. 
 * @see OTF_Writer_writeDefFileKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefFileKV( OTF_WStream* wstream, uint32_t token,
	const char* name, uint32_t group, OTF_KeyValueList* list );

/** Write a DEFFILEGROUP record to stream 'wstream'.
 * @see OTF_Writer_writeDefFileGroup()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefFileGroup( OTF_WStream* wstream, uint32_t token,
	const char* name );

/** Write a DEFFILEGROUP record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeDefFileGroupKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefFileGroupKV( OTF_WStream* wstream, uint32_t token,
	const char* name, OTF_KeyValueList* list );

/** Write a DEFKEYVALUE record to stream 'wstream'.
 * @see OTF_Writer_writeDefKeyValue()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefKeyValue( OTF_WStream* wstream,	uint32_t key,
	OTF_Type type, const char* name, const char *description );

/** Write a DEFKEYVALUE record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeDefKeyValueKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefKeyValueKV( OTF_WStream* wstream, uint32_t key,
	OTF_Type type, const char* name, const char *description,
	OTF_KeyValueList* list );


/** Write a DEFTIMERANGE record including an OTF_KeyValueList to
 *  stream 'wstream'.
 * @see OTF_Writer_writeDefTimeRange()
 * \ingroup wstream
 */
int OTF_WStream_writeDefTimeRange( OTF_WStream* wstream, uint64_t minTime,
	uint64_t maxTime, OTF_KeyValueList* list );

/** Write a DEFCOUNTERASSIGNMENTS record including an OTF_KeyValueList to
 *  stream 'wstream'.
 * @see OTF_Writer_writeDefCounterAssignments()
 * \ingroup wstream
 */
int OTF_WStream_writeDefCounterAssignments( OTF_WStream* wstream,
	uint32_t counter_token, uint32_t number_of_members,
	const uint32_t* procs_or_groups, OTF_KeyValueList* list );

/** Write a DEFPROCESSSUBTITUTES record including an OTF_KeyValueList to
 *  stream 'wstream'.
 * @see OTF_Writer_writeDefProcessSubsitutes()
 * \ingroup wstream
 */
int OTF_WStream_writeDefProcessSubstitutes( OTF_WStream* wstream,
	uint32_t representative, uint32_t numberOfProcs, const uint32_t* procs,
	OTF_KeyValueList* list );


/** Write a DEFAUXSAMPLEPOINT record to stream 'wstream'.
 * @see OTF_Writer_writeDefAuxSamplePoint()
 * \ingroup wstream
 */
int OTF_WStream_writeDefAuxSamplePoint( OTF_WStream*           wstream,
                                        uint64_t               time,
                                        OTF_AuxSamplePointType type,
                                        OTF_KeyValueList*      list );


/* *** event record write handlers *** ************************************* */


/**	Write a NOOP record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeNoOpKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeNoOpKV( OTF_WStream* wstream, uint64_t time,
    uint32_t process, OTF_KeyValueList* list );

/**	Write a ENTER record to stream 'wstream'.
 * @see OTF_Writer_writeEnter()
 * \ingroup wstream 
 */
int OTF_WStream_writeEnter( OTF_WStream* wstream, uint64_t time,
    uint32_t statetoken, uint32_t cpuid, uint32_t scltoken );

/**	Write a ENTER record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeEnterKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeEnterKV( OTF_WStream* wstream, uint64_t time,
    uint32_t statetoken, uint32_t cpuid, uint32_t scltoken, OTF_KeyValueList* list );

/**	Write a RECEIVE record to stream 'wstream'.
 * @see OTF_Writer_writeRecvMsg()
 * \ingroup wstream 
 */
int OTF_WStream_writeRecvMsg( OTF_WStream* wstream, uint64_t time,
    uint32_t receiver, uint32_t sender, uint32_t communicator,
    uint32_t msgtype, uint32_t msglength, uint32_t scltoken );

/**	Write a RECEIVE record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeRecvMsgKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeRecvMsgKV( OTF_WStream* wstream, uint64_t time,
    uint32_t receiver, uint32_t sender, uint32_t communicator,
    uint32_t msgtype, uint32_t msglength, uint32_t scltoken, OTF_KeyValueList* list );

/**	Write a SEND record to stream 'wstream'.
 * @see OTF_Writer_writeSendMsg()
 * \ingroup wstream 
 */
int OTF_WStream_writeSendMsg( OTF_WStream* wstream, uint64_t time, 
    uint32_t sender, uint32_t receiver, uint32_t communicator, 
    uint32_t msgtype, uint32_t msglength, uint32_t scltoken );

/**	Write a SEND record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeSendMsgKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeSendMsgKV( OTF_WStream* wstream, uint64_t time, 
    uint32_t sender, uint32_t receiver, uint32_t communicator, 
    uint32_t msgtype, uint32_t msglength, uint32_t scltoken, OTF_KeyValueList* list );

/**	Write a LEAVE record to stream 'wstream'.
 * @see OTF_Writer_writeLeave()
 * \ingroup wstream 
 */
int OTF_WStream_writeLeave( OTF_WStream* wstream, uint64_t time,
    uint32_t statetoken, uint32_t cpuid, uint32_t scltoken );

/**	Write a LEAVE record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeLeaveKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeLeaveKV( OTF_WStream* wstream, uint64_t time,
    uint32_t statetoken, uint32_t cpuid, uint32_t scltoken, OTF_KeyValueList* list );

/**	Write a COUNTER record to stream 'wstream'.
 * @see OTF_Writer_writeCounter()
 * \ingroup wstream 
 */
int OTF_WStream_writeCounter( OTF_WStream* wstream, uint64_t time, 
    uint32_t process, uint32_t counter_token, uint64_t value );

/**	Write a COUNTER record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeCounterKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeCounterKV( OTF_WStream* wstream, uint64_t time, 
    uint32_t process, uint32_t counter_token, uint64_t value, OTF_KeyValueList* list );

/** Write a COLLOP record to stream 'wstream'. 
 * @deprecated This event record has been deprecated due to usage constraints.
 *             Please use OTF_WStream_writeBeginCollectiveOperation() and
 *             OTF_WStream_writeEndCollectiveOperation(), repectively.
 * \ingroup wstream  */
int OTF_WStream_writeCollectiveOperation( OTF_WStream* wstream, uint64_t time, 
    uint32_t process, uint32_t functionToken, uint32_t communicator, 
    uint32_t rootprocess, uint32_t sent, uint32_t received, 
    uint64_t duration, uint32_t scltoken );

/** Write a COLLOP record including an OTF_KeyValueList to stream 'wstream'. 
 * @deprecated This event record has been deprecated due to usage constraints.
 *             Please use OTF_WStream_writeBeginCollectiveOperationKV() and
 *             OTF_WStream_writeEndCollectiveOperationKV(), repectively.
 * \ingroup wstream  */
int OTF_WStream_writeCollectiveOperationKV( OTF_WStream* wstream, uint64_t time, 
    uint32_t process, uint32_t functionToken, uint32_t communicator, 
    uint32_t rootprocess, uint32_t sent, uint32_t received, 
    uint64_t duration, uint32_t scltoken, OTF_KeyValueList* list );

/** Write a COLLOPBEGIN record to stream 'wstream'.
 * @see OTF_Writer_writeBeginCollectiveOperation()
 * \ingroup wstream 
 */
int OTF_WStream_writeBeginCollectiveOperation( OTF_WStream* wstream,
                uint64_t time, uint32_t process, uint32_t collOp,
                uint64_t matchingId, uint32_t procGroup, uint32_t rootProc,
                uint64_t sent, uint64_t received, uint32_t scltoken );

/** Write a COLLOPBEGIN record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeBeginCollectiveOperationKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeBeginCollectiveOperationKV( OTF_WStream* wstream,
                uint64_t time, uint32_t process, uint32_t collOp,
                uint64_t matchingId, uint32_t procGroup, uint32_t rootProc,
                uint64_t sent, uint64_t received, uint32_t scltoken, OTF_KeyValueList* list );

/** Write a COLLOPEND record to stream 'wstream'.
 * @see OTF_Writer_writeEndCollectiveOperation()
 * \ingroup wstream 
 */
int OTF_WStream_writeEndCollectiveOperation( OTF_WStream* wstream,
                uint64_t time, uint32_t process, uint64_t matchingId );

/** Write a COLLOPEND record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeEndCollectiveOperationKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeEndCollectiveOperationKV( OTF_WStream* wstream,
                uint64_t time, uint32_t process, uint64_t matchingId, OTF_KeyValueList* list );

/** Write a #EVTCOMMENT record to stream 'wstream'.
 * @see OTF_Writer_writeEventComment()
 * \ingroup wstream 
 */
int OTF_WStream_writeEventComment( OTF_WStream* wstream, uint64_t time, 
    uint32_t process, const char* comment );

/** Write a #EVTCOMMENT record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeEventCommentKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeEventCommentKV( OTF_WStream* wstream, uint64_t time, 
    uint32_t process, const char* comment, OTF_KeyValueList* list );

/** Write a PROCESSBEGIN record to stream 'wstream'.
 * @see OTF_Writer_writeBeginProcess()
 * \ingroup wstream 
 */
int OTF_WStream_writeBeginProcess( OTF_WStream* wstream, uint64_t time,
    uint32_t process );

/** Write a PROCESSBEGIN record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeBeginProcessKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeBeginProcessKV( OTF_WStream* wstream, uint64_t time,
    uint32_t process, OTF_KeyValueList* list );

/** Write a PROCESSEND record to stream 'wstream'.
 * @see OTF_Writer_writeEndProcess()
 * \ingroup wstream 
 */
int OTF_WStream_writeEndProcess( OTF_WStream* wstream, uint64_t time,
    uint32_t process );

/** Write a PROCESSEND record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeEndProcessKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeEndProcessKV( OTF_WStream* wstream, uint64_t time,
    uint32_t process, OTF_KeyValueList* list );

/** Write a FILEOP record to stream 'wstream'.
 * @deprecated This event record has been deprecated due to usage constraints.
 *             Please use OTF_WStream_writeBeginFileOperation() and
 *             OTF_WStream_writeEndFileOperation(), respectively.
 * \ingroup wstream  */
int OTF_WStream_writeFileOperation( OTF_WStream* wstream, uint64_t time,
	uint32_t fileid, uint32_t process, uint64_t handleid, uint32_t operation,
	uint64_t bytes, uint64_t duration, uint32_t source );

/** Write a FILEOP record including an OTF_KeyValueList to stream 'wstream'.
 * @deprecated This event record has been deprecated due to usage constraints.
 *             Please use OTF_WStream_writeBeginFileOperationKV() and
 *             OTF_WStream_writeEndFileOperationKV(), respectively.
 * \ingroup wstream  */
int OTF_WStream_writeFileOperationKV( OTF_WStream* wstream, uint64_t time,
	uint32_t fileid, uint32_t process, uint64_t handleid, uint32_t operation,
	uint64_t bytes, uint64_t duration, uint32_t source, OTF_KeyValueList* list );

/** Write a FILEOPBEGIN record to stream 'wstream'.
 * @see OTF_Writer_writeBeginFileOperation()
 * \ingroup wstream 
 */
int OTF_WStream_writeBeginFileOperation( OTF_WStream* wstream, uint64_t time,
    uint32_t process, uint64_t matchingId, uint32_t scltoken );

/** Write a FILEOPBEGIN record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeBeginFileOperationKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeBeginFileOperationKV( OTF_WStream* wstream, uint64_t time,
    uint32_t process, uint64_t matchingId, uint32_t scltoken, OTF_KeyValueList* list );

/** Write a FILEOPEND record to stream 'wstream'.
 * @see OTF_Writer_writeEndFileOperation()
 * \ingroup wstream 
 */
int OTF_WStream_writeEndFileOperation( OTF_WStream* wstream, uint64_t time,
                uint32_t process, uint32_t fileid, uint64_t matchingId,
                uint64_t handleId, uint32_t operation, uint64_t bytes,
                uint32_t scltoken );

/** Write a FILEOPEND record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeEndFileOperationKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeEndFileOperationKV( OTF_WStream* wstream, uint64_t time,
                uint32_t process, uint32_t fileid, uint64_t matchingId,
                uint64_t handleId, uint32_t operation, uint64_t bytes,
                uint32_t scltoken, OTF_KeyValueList* list );

/** Write a RMAPUT record to stream 'wstream'.
 * @see OTF_Writer_writeRMAPut()
 * \ingroup wstream 
 */
int OTF_WStream_writeRMAPut( OTF_WStream* wstream, uint64_t time,
    uint32_t process, uint32_t origin, uint32_t target, uint32_t communicator,
    uint32_t tag, uint64_t bytes, uint32_t scltoken );

/** Write a RMAPUT record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeRMAPutKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeRMAPutKV( OTF_WStream* wstream, uint64_t time,
    uint32_t process, uint32_t origin, uint32_t target, uint32_t communicator,
    uint32_t tag, uint64_t bytes, uint32_t scltoken, OTF_KeyValueList* list );

/** Write a RMAPUTRE record to stream 'wstream'.
 * @see OTF_Writer_writeRMAPutRemoteEnd()
 * \ingroup wstream 
 */
int OTF_WStream_writeRMAPutRemoteEnd( OTF_WStream* wstream, uint64_t time,
    uint32_t process, uint32_t origin, uint32_t target, uint32_t communicator,
    uint32_t tag, uint64_t bytes, uint32_t scltoken );

/** Write a RMAPUTRE record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeRMAPutRemoteEndKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeRMAPutRemoteEndKV( OTF_WStream* wstream, uint64_t time,
    uint32_t process, uint32_t origin, uint32_t target, uint32_t communicator,
    uint32_t tag, uint64_t bytes, uint32_t scltoken, OTF_KeyValueList* list );

/** Write a RMAGET record to stream 'wstream'.
 * @see OTF_Writer_writeRMAGet()
 * \ingroup wstream 
 */
int OTF_WStream_writeRMAGet( OTF_WStream* wstream, uint64_t time,
    uint32_t process, uint32_t origin, uint32_t target, uint32_t communicator,
    uint32_t tag, uint64_t bytes, uint32_t scltoken );

/** Write a RMAGET record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeRMAGetKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeRMAGetKV( OTF_WStream* wstream, uint64_t time,
    uint32_t process, uint32_t origin, uint32_t target, uint32_t communicator,
    uint32_t tag, uint64_t bytes, uint32_t scltoken, OTF_KeyValueList* list );

/** Write a RMAEND record to stream 'wstream'.
 * @see OTF_Writer_writeRMAEnd()
 * \ingroup wstream 
 */
int OTF_WStream_writeRMAEnd( OTF_WStream* wstream, uint64_t time,
    uint32_t process, uint32_t remote, uint32_t communicator, uint32_t tag,
    uint32_t scltoken );

/** Write a RMAEND record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeRMAEndKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeRMAEndKV( OTF_WStream* wstream, uint64_t time,
    uint32_t process, uint32_t remote, uint32_t communicator, uint32_t tag,
    uint32_t scltoken, OTF_KeyValueList* list );


/* *** public snapshot record write handlers *** */


/** Write a #TCOMMENT record to stream 'wstream'.
 * @see OTF_Writer_writeSnapshotComment()
 * \ingroup wstream 
 */
int OTF_WStream_writeSnapshotComment( OTF_WStream* wstream, uint64_t time, 
    uint32_t process, const char* comment );

/** Write a #TCOMMENT record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeSnapshotCommentKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeSnapshotCommentKV( OTF_WStream* wstream, uint64_t time, 
    uint32_t process, const char* comment, OTF_KeyValueList* list );

/** Write a TENTER record to stream 'wstream'.
 * @see OTF_Writer_writeEnterSnapshot()
 * \ingroup wstream 
 */
int OTF_WStream_writeEnterSnapshot( OTF_WStream* wstream, uint64_t time,
    uint64_t originaltime, uint32_t statetoken, uint32_t cpuid, uint32_t scltoken );

/** Write a TENTER record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeEnterSnapshotKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeEnterSnapshotKV( OTF_WStream* wstream, uint64_t time,
    uint64_t originaltime, uint32_t statetoken, uint32_t cpuid, uint32_t scltoken, OTF_KeyValueList* list );

/** Write a TSEND record to stream 'wstream'.
 * @see OTF_Writer_writeSendSnapshot()
 * \ingroup wstream 
 */
int OTF_WStream_writeSendSnapshot( OTF_WStream* wstream, uint64_t time,
		uint64_t originaltime, uint32_t sender, uint32_t receiver,
		uint32_t procGroup, uint32_t type, uint32_t length, uint32_t source );

/** Write a TSEND record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeSendSnapshotKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeSendSnapshotKV( OTF_WStream* wstream, uint64_t time,
		uint64_t originaltime, uint32_t sender, uint32_t receiver,
		uint32_t procGroup, uint32_t type, uint32_t length,
        uint32_t source, OTF_KeyValueList* list );

/** Write a TOPENFILE record to stream 'wstream'.
 * @see OTF_Writer_writeOpenFileSnapshot()
 * \ingroup wstream 
 */
int OTF_WStream_writeOpenFileSnapshot( OTF_WStream* wstream,uint64_t time,
	uint64_t originaltime, uint32_t fileid, uint32_t process, uint64_t handleid,
	uint32_t source );

/** Write a TOPENFILE record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeOpenFileSnapshotKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeOpenFileSnapshotKV( OTF_WStream* wstream,uint64_t time,
	uint64_t originaltime, uint32_t fileid, uint32_t process, uint64_t handleid,
	uint32_t source, OTF_KeyValueList* list );


/** Write a TBEGINCOLLOP record to stream 'wstream'.
 * @see OTF_Writer_writeBeginCollopSnapshot()
 * \ingroup wstream 
 */
int OTF_WStream_writeBeginCollopSnapshot( OTF_WStream* wstream, uint64_t time,
    uint64_t originaltime, uint32_t process, uint32_t collOp, uint64_t matchingId,
	uint32_t procGroup, uint32_t rootProc, uint64_t sent, uint64_t received,
    uint32_t scltoken );


/** Write a TBEGINCOLLOP record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeBeginCollopSnapshotKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeBeginCollopSnapshotKV( OTF_WStream* wstream, uint64_t time,
    uint64_t originaltime, uint32_t process, uint32_t collOp, uint64_t matchingId,
	uint32_t procGroup, uint32_t rootProc, uint64_t sent, uint64_t received,
    uint32_t scltoken, OTF_KeyValueList *list );


/** Write a TBEGINFILEOP record to stream 'wstream'.
 * @see OTF_Writer_writeBeginFileOpSnapshot()
 * \ingroup wstream 
 */
int OTF_WStream_writeBeginFileOpSnapshot( OTF_WStream* wstream, uint64_t time,
     uint64_t originaltime, uint32_t process, uint64_t matchingId, uint32_t scltoken );

/** Write a TBEGINFILEOP record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeBeginFileOpSnapshotKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeBeginFileOpSnapshotKV( OTF_WStream* wstream, uint64_t time,
     uint64_t originaltime, uint32_t process, uint64_t matchingId,
     uint32_t scltoken, OTF_KeyValueList *list );


/** Write a TCOLLOPCOUNT record to stream 'wstream'.
 * @see OTF_Writer_writeCollopCountSnapshot()
 * \ingroup wstream
 */
int OTF_WStream_writeCollopCountSnapshot( OTF_WStream* wstream,
                                          uint64_t time,
                                          uint32_t process,
                                          uint32_t communicator,
                                          uint64_t count,
                                          OTF_KeyValueList* list );


/** Write a TCOUNTER record to stream 'wstream'.
 * @see OTF_Writer_writeCounterSnapshot()
 * \ingroup wstream
 */
int OTF_WStream_writeCounterSnapshot( OTF_WStream*      wstream,
                                      uint64_t          time,
                                      uint64_t          originaltime,
                                      uint32_t          process,
                                      uint32_t          counter,
                                      uint64_t          value,
                                      OTF_KeyValueList* list );


/* *** public statistics record write handlers *** */


/** Write a SUMCOMMENT record to stream 'wstream'.
 * @see OTF_Writer_writeSummaryComment()
 * \ingroup wstream 
 */
int OTF_WStream_writeSummaryComment( OTF_WStream* wstream, uint64_t time, 
    uint32_t process, const char* comment );

/** Write a SUMCOMMENT record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeSummaryCommentKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeSummaryCommentKV( OTF_WStream* wstream, uint64_t time, 
    uint32_t process, const char* comment, OTF_KeyValueList* list );

/** Write a SUMFUNCTION record to stream 'wstream'.
 * @see OTF_Writer_writeFunctionSummary()
 * \ingroup wstream 
 */
int OTF_WStream_writeFunctionSummary( OTF_WStream* wstream, 
	uint64_t time, uint32_t function, uint32_t process, 
	uint64_t count, uint64_t excltime, uint64_t incltime );

/** Write a SUMFUNCTION record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeFunctionSummaryKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeFunctionSummaryKV( OTF_WStream* wstream, 
	uint64_t time, uint32_t function, uint32_t process, 
	uint64_t count, uint64_t excltime, uint64_t incltime, OTF_KeyValueList* list );

/** Write a SUMFUNCTIONGROUP record to stream 'wstream'.
 * @see OTF_Writer_writeFunctionGroupSummary()
 * \ingroup wstream 
 */
int OTF_WStream_writeFunctionGroupSummary( OTF_WStream* wstream, 
	uint64_t time,  uint32_t functiongroup,  uint32_t process,  
	uint64_t count,  uint64_t excltime,  uint64_t incltime );

/** Write a SUMFUNCTIONGROUP record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeFunctionGroupSummaryKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeFunctionGroupSummaryKV( OTF_WStream* wstream, 
	uint64_t time,  uint32_t functiongroup,  uint32_t process,  
	uint64_t count,  uint64_t excltime,  uint64_t incltime, OTF_KeyValueList* list );

/** Write a SUMMESSAGE record to stream 'wstream'. 
 * @see OTF_Writer_writeMessageSummary()
 * \ingroup wstream 
 */
int OTF_WStream_writeMessageSummary( OTF_WStream* wstream, 
	uint64_t time, uint32_t process, uint32_t peer, 
	uint32_t comm,  uint32_t tag, uint64_t number_sent, uint64_t number_recved,
	uint64_t bytes_sent,  uint64_t bytes_recved );

/** Write a SUMMESSAGE record including an OTF_KeyValueList to stream 'wstream'. 
 * @see OTF_Writer_writeMessageSummaryKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeMessageSummaryKV( OTF_WStream* wstream, 
	uint64_t time, uint32_t process, uint32_t peer, 
	uint32_t comm,  uint32_t tag, uint64_t number_sent, uint64_t number_recved,
	uint64_t bytes_sent,  uint64_t bytes_recved, OTF_KeyValueList* list );

/** Write a COLLOPMESSAGE record to stream 'wstream'.
 * @see OTF_Writer_writeCollopSummary()
 * \ingroup wstream 
 */
int OTF_WStream_writeCollopSummary( OTF_WStream* wstream,
	uint64_t time, uint32_t process, uint32_t comm, uint32_t collective,
	uint64_t number_sent, uint64_t number_recved, uint64_t bytes_sent,  uint64_t bytes_recved );

/** Write a COLLOPMESSAGE record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeCollopSummaryKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeCollopSummaryKV( OTF_WStream* wstream,
	uint64_t time, uint32_t process, uint32_t comm, uint32_t collective,
	uint64_t number_sent, uint64_t number_recved, uint64_t bytes_sent,  uint64_t bytes_recved, OTF_KeyValueList* list );

/** Write a SUMFILEOPERATION record to stream 'wstream'.
 * @see OTF_Writer_writeFileOperationSummary()
 * \ingroup wstream 
 */
int OTF_WStream_writeFileOperationSummary( OTF_WStream* wstream, uint64_t time,
	uint32_t fileid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite );

/** Write a SUMFILEOPERATION record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeFileOperationSummaryKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeFileOperationSummaryKV( OTF_WStream* wstream, uint64_t time,
	uint32_t fileid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite, OTF_KeyValueList* list );

/** Write a SUMFILEGROUPOPERATION record to stream 'wstream'.
 * @see OTF_Writer_writeFileGroupOperationSummary()
 * \ingroup wstream 
 */
int OTF_WStream_writeFileGroupOperationSummary( OTF_WStream* wstream, uint64_t time,
	uint32_t groupid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite );

/** Write a SUMFILEGROUPOPERATION record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeFileGroupOperationSummaryKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeFileGroupOperationSummaryKV( OTF_WStream* wstream, uint64_t time,
	uint32_t groupid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite, OTF_KeyValueList* list );

/*
int OTF_WStream_writeCounterSummary( OTF_WStream* wstream, 
	uint64_t time,  uint32_t process,  
	uint32_t counter,  uint64_t value );

int OTF_WStream_writeCollOpSummary( OTF_WStream* wstream, 
	uint64_t time, uint32_t process,  uint32_t root,  
	uint64_t bytes_sent, uint64_t bytes_recved );
*/



/* *** marker record types *** */

/** Write a def marker record to stream 'wstream'.
 * @see OTF_Writer_writeDefMarker()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefMarker( OTF_WStream* wstream, 
                                uint32_t token, 
                                const char* name, 
                                uint32_t type );

/** Write a def marker record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeDefMarkerKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeDefMarkerKV( OTF_WStream* wstream, 
                                uint32_t token, 
                                const char* name, 
                                uint32_t type,
				OTF_KeyValueList* list );

/** Write a marker record to stream 'wstream'.
 * @see OTF_Writer_writeMarker()
 * \ingroup wstream 
 */
int OTF_WStream_writeMarker( OTF_WStream* wstream, 
                             uint64_t time, 
                             uint32_t process, 
                             uint32_t token, 
                             const char* text );

/** Write a marker record including an OTF_KeyValueList to stream 'wstream'.
 * @see OTF_Writer_writeMarkerKV()
 * \ingroup wstream 
 */
int OTF_WStream_writeMarkerKV( OTF_WStream* wstream, 
                             uint64_t time, 
                             uint32_t process, 
                             uint32_t token, 
                             const char* text,
			     OTF_KeyValueList* list );


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_WSTREAM_H */

