/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "OTF_Platform.h"
#include "OTF_Definitions.h"

#include "OTF_Writer.h"
#include "OTF_Platform.h"
#include "OTF_Errno.h"

#include <assert.h>


/** Holds the data objects needed for global trace creation. */
struct struct_OTF_Writer {

    /** All files will start with this prefix. */
    char* namestub;

    /** Holds the current number of streams. */
    uint32_t n;

    /** Current size of array 'streams'. */
    uint32_t s;

    /** Sorted array of streams. */
    OTF_WStream** streams;

    /** Maximum number of streams allowed including the special stream
        0.  This applies if the Writer assigns processes to streams by
        its own devices. The number of streams might be higher if
        there are explicit mappings enforced with
        OTF_Writer_assignProcess() for some processes after automatic
        mappings took effect. */
    uint32_t m;

    /** Master control structure. Handles the mapping of processes to
        streams. */
    OTF_MasterControl* mc;

    /** Default size of buffers managed by this Writer. */
    uint32_t buffersizes;

    /** Default output format */
    uint32_t format;

    /** File handle manager. Handles the re-usage of file handles
        in a thread safe way. */
    OTF_FileManager* manager;

#ifdef HAVE_ZLIB
    /** Default compression type of buffers managed by this writer */
    OTF_FileCompression compression;

    /** Default size of zbuffers managed by this reader. */
    uint32_t zbuffersizes;
#endif /* HAVE_ZLIB */

    /** Are the definition header records (e.g. DEFVERSION, DEFUNIQUEID)
        written? 1= yes 0= no */
    uint32_t defHeaderWritten;
};


/** constructor - internal use only */
int OTF_Writer_init( OTF_Writer* writer );

/** destructor - internal use only */
int OTF_Writer_finish( OTF_Writer* writer );

/** write header records (e.g. DEFVERSION, DEFUNIQUEID) to global
    definition stream - internal use only */
int OTF_Writer_writeDefinitionHeader( OTF_Writer* writer );


/* ************************************************************************* */

int OTF_Writer_init( OTF_Writer* writer ) {


	writer->namestub= NULL;

	writer->n= 0;
	writer->s= 0;
	writer->streams= NULL;

	writer->m= 0;

	writer->mc= NULL;

	writer->buffersizes= 1024*1024;
	
	writer->format= OTF_WSTREAM_FORMAT_SHORT;

#ifdef HAVE_ZLIB
	writer->compression= 0;
	writer->zbuffersizes= OTF_ZBUFFER_DEFAULTSIZE;
#endif /* HAVE_ZLIB */

	writer->defHeaderWritten= 0;

	return 1;
}


int OTF_Writer_finish( OTF_Writer* writer ) {


	int tmpret;
	int ret= 1;
	tmpret= OTF_MasterControl_write( writer->mc, writer->namestub );
	if( 0 == tmpret ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_MasterControl_write() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );
	}
	ret&= tmpret;

	tmpret= OTF_Writer_closeAllStreams( writer );
	if( 0 == tmpret ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_Writer_closeAllStreams() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );
	}
	ret&= tmpret;
	
	free( writer->namestub );
	writer->namestub= NULL;

	free( writer->streams );
	writer->streams= NULL;
	writer->s= 0;

	OTF_MasterControl_close( writer->mc );

#ifdef HAVE_ZLIB
	writer->compression= 0;
	writer->zbuffersizes= 0;
#endif /* HAVE_ZLIB */

	return ret;
}


OTF_Writer* OTF_Writer_open( const char* namestub, uint32_t m, OTF_FileManager* manager ) {


	OTF_Writer* ret= NULL;

	if( NULL == manager ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"manager has not been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return NULL;
	}

	ret= (OTF_Writer*) malloc( sizeof(OTF_Writer) );
	if( NULL == ret ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return NULL;
	}

	OTF_Writer_init( ret );

	ret->m= ( 0 < m ) ? m : ((uint32_t) -1);

	ret->namestub= OTF_stripFilename( namestub );

	ret->manager= manager;

	ret->mc= OTF_MasterControl_new( ret->manager );
	if( NULL == ret->mc ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_MasterControl_new() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		free( ret->namestub );
		ret->namestub= NULL;
		free( ret );
		ret= NULL;

		return NULL;
	}
	
	return ret;
}


int OTF_Writer_close( OTF_Writer* writer ) {


	if( NULL == writer ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"writer has not been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}		

	if( 0 == OTF_Writer_finish( writer ) ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_Writer_finish() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		
		free( writer );
		writer = NULL;
		return 0;
	}

	free( writer );
	writer = NULL;

	return 1;
}


int OTF_Writer_setCompression( OTF_Writer* writer, OTF_FileCompression
	compression ) {
	
	
#ifdef HAVE_ZLIB
	if ( compression <= 9 ) {
	
		writer->compression = compression;
		
		return 1;
		
	} else {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"compression is no expected value (%u). ignored.\n",
				__FUNCTION__, __FILE__, __LINE__, compression );

		return 0;
	}
		
#else /* HAVE_ZLIB */

	if( 0 == compression ) {
	
		return 1;
		
	} else {
	
		return 0;
	}

#endif /* HAVE_ZLIB */
}

OTF_FileCompression OTF_Writer_getCompression( OTF_Writer* writer ) {


#ifdef HAVE_ZLIB
	return writer->compression;
#else /* HAVE_ZLIB */
	return 0;
#endif /* HAVE_ZLIB */
}


void OTF_Writer_setBufferSizes( OTF_Writer* writer, uint32_t size ) {


	if ( 50 > size ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"intended buffer size %u is too small, rejected.\n",
				__FUNCTION__, __FILE__, __LINE__, size );
		
		return;

	} else if ( 500 > size ) {
	
		OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				"buffer size %u is very small, accepted though.\n",
				__FUNCTION__, __FILE__, __LINE__, size );

	} else if ( 10 * 1024 *1024 < size ) {

		OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				"buffer size %u is rather big, accepted though.\n",
				__FUNCTION__, __FILE__, __LINE__, size );
	}

	writer->buffersizes= size;
}


uint32_t OTF_Writer_getBufferSizes( OTF_Writer* writer ) {


	return writer->buffersizes;
}


void OTF_Writer_setZBufferSizes( OTF_Writer* writer, uint32_t size ) {


#ifdef HAVE_ZLIB
	
	if ( 32 > size ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"intended zbuffer size %u is too small, rejected.\n",
				__FUNCTION__, __FILE__, __LINE__, size );
		
		return;

	} else if ( 512 > size ) {
	
		OTF_Warning( "ERROR in function %s, file: %s, line: %i:\n "
				"buffer size %u is very small, accepted though.\n",
				__FUNCTION__, __FILE__, __LINE__, size );

	} else if ( 10 * 1024 *1024 < size ) {

		OTF_Warning( "ERROR in function %s, file: %s, line: %i:\n "
				"buffer size %u is rather big, accepted though.\n",
				__FUNCTION__, __FILE__, __LINE__, size );

	}

	writer->zbuffersizes= size;
	
#endif /* HAVE_ZLIB */
}


uint32_t OTF_Writer_getZBufferSizes( OTF_Writer* writer ) {


#ifdef HAVE_ZLIB
	return writer->zbuffersizes;
#else /* HAVE_ZLIB */
	return 0;
#endif /* HAVE_ZLIB */
}


void OTF_Writer_setFormat( OTF_Writer* writer, uint32_t format ) {


	if ( format > 3 ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"unknown ouput format chosen.\n",
				__FUNCTION__, __FILE__, __LINE__ );

	}
	
	writer->format= format;
}


uint32_t OTF_Writer_getFormat( OTF_Writer* writer ) {


	return writer->format;
}


OTF_WStream* OTF_Writer_getStream( OTF_Writer* writer, uint32_t streamId ) {


	uint32_t a;
	uint32_t b;
	uint32_t c;

	uint32_t i;


	/* search if already present */

	a= 0;
	b= writer->n;

	if ( 0 < writer->n ) {


		if ( streamId == writer->streams[0]->id ) {

			return writer->streams[0];
		}

		if ( streamId == writer->streams[b-1]->id ) {

			return writer->streams[b-1];
		}


		while ( a < b ) {

			c= ( a + b ) / 2;

			if ( streamId == writer->streams[c]->id ) {

				/* found */
				return writer->streams[c];
			}

			if ( streamId < writer->streams[c]->id ) {

				/* [a,c) */
				b= c;

			} else {

				/* [c+1,b) */
				a= c+1;
			}
		}
	}


	/* not found, create & append at position 'a' */

	/*
	fprintf(stderr," at %u\n", a );
	*/

	if ( writer->s <= writer->n ) {

		writer->s += 10;
		writer->streams= (OTF_WStream**) realloc( writer->streams,
			writer->s * sizeof(OTF_WStream*) );
		if( NULL == writer->streams ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return NULL;
		}
	}

	for ( i= writer->n; i > a; --i ) {

		writer->streams[i]= writer->streams[i-1];
	}

	writer->streams[a]= OTF_WStream_open( writer->namestub, streamId, writer->manager );

#ifdef HAVE_ZLIB
	OTF_WStream_setCompression( writer->streams[a], writer->compression );
	OTF_WStream_setZBufferSizes( writer->streams[a], writer->zbuffersizes );
#endif /* HAVE_ZLIB */

	OTF_WStream_setBufferSizes( writer->streams[a], writer->buffersizes );
	OTF_WStream_setFormat( writer->streams[a], writer->format );

	++(writer->n);


	return writer->streams[a];
}


uint32_t OTF_Writer_assignProcess( OTF_Writer* writer,
		uint32_t process, uint32_t stream ) {


	if ( 0 == stream ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"stream id must not be '0'.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		
		return 0;
	}

	if( 0 == OTF_MasterControl_append( writer->mc, stream, process ) ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_MasterControl_append() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}

	return 1;
}


uint32_t OTF_Writer_mapProcess( OTF_Writer* writer, uint32_t process ) {


	uint32_t ret= OTF_MasterControl_mapReverse( writer->mc, process );
	uint32_t processes= (uint32_t) -1;
	uint32_t n;
	uint32_t i;
	OTF_MapEntry* entry;


	/* here there are some ways to handle the situation:
	first, define a static stream id which all processes which are not
	explicitly mapped go to ("ret= 0xff;").
	Second, add a new stream for the new process (if-branch below).
	Third, append the process to any existing stream, for example to
	the one with the fewest processes so far (else-branch below). */
	if ( 0 == ret ) {

		/*
		fprintf( stderr, "OTF_Writer_mapProcess() %u unknown\n", process );
		*/

		ret= (uint32_t) -1;

		n= OTF_MasterControl_getCount( writer->mc );

		if ( n < writer->m ) {

			/* add a new stream, add process */

			ret= OTF_MasterControl_getNewStreamId( writer->mc );

		} else {

			/* if no streams are present the former branch
			must take care!*/
			if( 0 >= n ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"no processes/stream have been defined.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				assert (0);
			}

			/* find stream with fewest processes and
			add there */

			for ( i = 0; i < n; ++i ) {

				entry = OTF_MasterControl_getEntryByIndex( writer->mc, i );

				if ( entry->n < processes ) {

					processes = entry->n;
					ret = entry->argument;
				}
			}

		}

		assert( (uint32_t) -1 != ret );

		OTF_Writer_assignProcess( writer, process, ret );
	}


	return ret;
}


OTF_MasterControl* OTF_Writer_getMasterControl( OTF_Writer* writer ) {


	return writer->mc;
}


void OTF_Writer_setMasterControl( OTF_Writer* writer, OTF_MasterControl* mc ) {


	OTF_MasterControl_close( writer->mc );

	writer->mc= mc;
}


int OTF_Writer_writeDefinitionHeader( OTF_Writer* writer ) {

	/* write DEFVERSION record */
	if ( 0 == OTF_WStream_writeOtfVersion( OTF_Writer_getStream( writer, 0 ) ) )
		return 0;

	/* write DEFUNIQUEID record */
	if ( 0 == OTF_WStream_writeUniqueId( OTF_Writer_getStream( writer, 0 ) ) )
		return 0;

	return 1;
}


/* *** definition record write handlers *** ******************************** */


int OTF_Writer_writeDefinitionComment( OTF_Writer* writer, uint32_t streamid, 
		const char* comment ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}
	
	return OTF_WStream_writeDefinitionComment( stream, comment );
}

int OTF_Writer_writeDefinitionCommentKV( OTF_Writer* writer, uint32_t streamid, 
		const char* comment, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefinitionCommentKV( stream, comment, list );
}


int OTF_Writer_writeDefTimerResolution( OTF_Writer* writer, uint32_t streamid, 
		uint64_t ticksPerSecond ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefTimerResolution( stream, ticksPerSecond );
}

int OTF_Writer_writeDefTimerResolutionKV( OTF_Writer* writer, uint32_t streamid, 
		uint64_t ticksPerSecond, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefTimerResolutionKV( stream, ticksPerSecond, list );
}


int OTF_Writer_writeDefProcess( OTF_Writer* writer, uint32_t streamid,
		uint32_t deftoken, const char* name, uint32_t parent ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefProcess( stream, deftoken, name, parent );
}

int OTF_Writer_writeDefProcessKV( OTF_Writer* writer, uint32_t streamid,
		uint32_t deftoken, const char* name, uint32_t parent, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefProcessKV( stream, deftoken, name, parent, list );
}


int OTF_Writer_writeDefProcessGroup( OTF_Writer* writer, uint32_t streamid,
		uint32_t deftoken, const char* name, uint32_t n,
		const uint32_t* array ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefProcessGroup( stream, deftoken, 
		name, n, (uint32_t*) array );
}

int OTF_Writer_writeDefProcessGroupKV( OTF_Writer* writer, uint32_t streamid,
		uint32_t deftoken, const char* name, uint32_t n,
		const uint32_t* array, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefProcessGroupKV( stream, deftoken, 
		name, n, (uint32_t*) array, list );
}


int OTF_Writer_writeDefAttributeList ( OTF_Writer* writer, uint32_t streamid,
		uint32_t attr_token, uint32_t num, OTF_ATTR_TYPE* array ) {

	
	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefAttributeList( stream, attr_token, num, array );
}

int OTF_Writer_writeDefAttributeListKV ( OTF_Writer* writer, uint32_t streamid,
		uint32_t attr_token, uint32_t num, OTF_ATTR_TYPE* array, OTF_KeyValueList *list ) {

	
	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefAttributeListKV( stream, attr_token, num, array, list );
}


int OTF_Writer_writeDefProcessOrGroupAttributes( OTF_Writer* writer, uint32_t streamid,
		uint32_t proc_token, uint32_t attr_token ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefProcessOrGroupAttributes( stream, proc_token, attr_token );
}

int OTF_Writer_writeDefProcessOrGroupAttributesKV( OTF_Writer* writer, uint32_t streamid,
		uint32_t proc_token, uint32_t attr_token, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefProcessOrGroupAttributesKV( stream, proc_token, attr_token, list );
}


int OTF_Writer_writeDefFunction( OTF_Writer* writer, uint32_t streamid,
	uint32_t deftoken, const char* name, uint32_t group, uint32_t scltoken ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefFunction( stream, deftoken, name, group, 
		scltoken );
}

int OTF_Writer_writeDefFunctionKV( OTF_Writer* writer, uint32_t streamid,
	uint32_t deftoken, const char* name, uint32_t group, uint32_t scltoken, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefFunctionKV( stream, deftoken, name, group, 
		scltoken, list );
}


int OTF_Writer_writeDefFunctionGroup( OTF_Writer* writer, uint32_t streamid, 
		uint32_t deftoken, const char* name ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefFunctionGroup( stream, deftoken, name );
}

int OTF_Writer_writeDefFunctionGroupKV( OTF_Writer* writer, uint32_t streamid, 
		uint32_t deftoken, const char* name, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefFunctionGroupKV( stream, deftoken, name, list );
}


int OTF_Writer_writeDefCollectiveOperation( OTF_Writer* writer, uint32_t streamid,
		uint32_t collective, const char* name, uint32_t type ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefCollectiveOperation( stream, collective, name, type );
}

int OTF_Writer_writeDefCollectiveOperationKV( OTF_Writer* writer, uint32_t streamid,
		uint32_t collective, const char* name, uint32_t type, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefCollectiveOperationKV( stream, collective, name, type, list );
}


int OTF_Writer_writeDefCounter( OTF_Writer* writer, uint32_t streamid,
		uint32_t deftoken, const char* name, uint32_t properties, 
		uint32_t countergroup, const char* unit ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefCounter( stream, deftoken, name, properties, 
		countergroup, unit );
}

int OTF_Writer_writeDefCounterKV( OTF_Writer* writer, uint32_t streamid,
		uint32_t deftoken, const char* name, uint32_t properties, 
		uint32_t countergroup, const char* unit, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefCounterKV( stream, deftoken, name, properties, 
		countergroup, unit, list );
}


int OTF_Writer_writeDefCounterGroup( OTF_Writer* writer, uint32_t streamid,
		uint32_t deftoken, const char* name ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefCounterGroup( stream, deftoken, name );
}

int OTF_Writer_writeDefCounterGroupKV( OTF_Writer* writer, uint32_t streamid,
		uint32_t deftoken, const char* name, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefCounterGroupKV( stream, deftoken, name, list );
}


int OTF_Writer_writeDefScl( OTF_Writer* writer, uint32_t streamid,
		uint32_t deftoken, uint32_t sclfile, uint32_t sclline ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefScl( stream, deftoken, sclfile, sclline );
}

int OTF_Writer_writeDefSclKV( OTF_Writer* writer, uint32_t streamid,
		uint32_t deftoken, uint32_t sclfile, uint32_t sclline, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefSclKV( stream, deftoken, sclfile, sclline, list );
}


int OTF_Writer_writeDefSclFile( OTF_Writer* writer, uint32_t streamid,
		uint32_t deftoken, const char* filename ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefSclFile( stream, deftoken, filename );
}

int OTF_Writer_writeDefSclFileKV( OTF_Writer* writer, uint32_t streamid,
		uint32_t deftoken, const char* filename, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefSclFileKV( stream, deftoken, filename, list );
}


/* depricated */
int OTF_Writer_writeOtfVersion( OTF_Writer* writer, uint32_t streamid ) {

	
	OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
			"calling depricated function. ignored.\n",
			__FUNCTION__, __FILE__, __LINE__ );
	
	return 1;
}


int OTF_Writer_writeDefCreator( OTF_Writer* writer, uint32_t streamid,
		const char* creator ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefCreator( stream, creator );
}

int OTF_Writer_writeDefCreatorKV( OTF_Writer* writer, uint32_t streamid,
		const char* creator, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefCreatorKV( stream, creator, list );
}


int OTF_Writer_writeDefFile( OTF_Writer* writer, uint32_t streamid,
	uint32_t token, const char* name, uint32_t group ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	return OTF_WStream_writeDefFile( stream, token, name, group );
}

int OTF_Writer_writeDefFileKV( OTF_Writer* writer, uint32_t streamid,
	uint32_t token, const char* name, uint32_t group, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	return OTF_WStream_writeDefFileKV( stream, token, name, group, list );
}


int OTF_Writer_writeDefFileGroup( OTF_Writer* writer, uint32_t streamid,
	uint32_t token, const char* name ) {

	
	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	return OTF_WStream_writeDefFileGroup( stream, token, name );
}

int OTF_Writer_writeDefFileGroupKV( OTF_Writer* writer, uint32_t streamid,
	uint32_t token, const char* name, OTF_KeyValueList *list ) {

	
	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	return OTF_WStream_writeDefFileGroupKV( stream, token, name, list );
}


int OTF_Writer_writeDefKeyValue( OTF_Writer* writer, uint32_t streamid, 
		uint32_t key, OTF_Type type, const char* name, const char *description ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefKeyValue( stream, key, type, name, description );
}

int OTF_Writer_writeDefKeyValueKV( OTF_Writer* writer, uint32_t streamid, uint32_t key,
		OTF_Type type, const char* name, const char *description, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

	if ( 0 == writer->defHeaderWritten ) {
	
		OTF_Writer_writeDefinitionHeader( writer );
		writer->defHeaderWritten= 1;
	}

	return OTF_WStream_writeDefKeyValueKV( stream, key, type, name, description, list );
}


int OTF_Writer_writeDefTimeRange( OTF_Writer*       writer,
                                  uint32_t          streamid,
                                  uint64_t          minTime,
                                  uint64_t          maxTime,
                                  OTF_KeyValueList* list ) {


    OTF_WStream* stream= OTF_Writer_getStream( writer, streamid );

    if ( 0 == writer->defHeaderWritten ) {

        OTF_Writer_writeDefinitionHeader( writer );
        writer->defHeaderWritten= 1;
    }

    return OTF_WStream_writeDefTimeRange( stream,
                                          minTime,
                                          maxTime,
                                          list );
}


int OTF_Writer_writeDefCounterAssignments( OTF_Writer*       writer,
                                           uint32_t          streamid,
                                           uint32_t          counter_token,
                                           uint32_t          number_of_members,
                                           const uint32_t*   procs_or_groups,
                                           OTF_KeyValueList* list ) {


    OTF_WStream* stream= OTF_Writer_getStream( writer, streamid );

    if ( 0 == writer->defHeaderWritten ) {

        OTF_Writer_writeDefinitionHeader( writer );
        writer->defHeaderWritten= 1;
    }

    return OTF_WStream_writeDefCounterAssignments( stream,
                                                   counter_token,
                                                   number_of_members,
                                                   procs_or_groups,
                                                   list );
}


int OTF_Writer_writeDefProcessSubstitutes( OTF_Writer* writer, uint32_t streamid,
        uint32_t representative, uint32_t numberOfProcs, const uint32_t* procs,
        OTF_KeyValueList* list ) {


    OTF_WStream* stream= OTF_Writer_getStream( writer, (uint32_t) streamid );

    if ( 0 == writer->defHeaderWritten ) {

        OTF_Writer_writeDefinitionHeader( writer );
        writer->defHeaderWritten= 1;
    }

    return OTF_WStream_writeDefProcessSubstitutes( stream, representative, 
        numberOfProcs, (uint32_t*) procs, list );
}


int OTF_Writer_writeDefAuxSamplePoint( OTF_Writer*            writer,
                                       uint32_t               streamid,
                                       uint64_t               time,
                                       OTF_AuxSamplePointType type,
                                       OTF_KeyValueList*      list ) {


    OTF_WStream* stream= OTF_Writer_getStream( writer, streamid );

    if ( 0 == writer->defHeaderWritten ) {

        OTF_Writer_writeDefinitionHeader( writer );
        writer->defHeaderWritten= 1;
    }

    return OTF_WStream_writeDefAuxSamplePoint( stream, time, type, list );
}


/* *** Event Records *** ****************************************** */

int OTF_Writer_writeNoOpKV( OTF_Writer* writer, uint64_t time, 
                            uint32_t process, OTF_KeyValueList* list ) {

	
	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );


	return OTF_WStream_writeNoOpKV( stream, time, 
		process, list );
}

int OTF_Writer_writeEnter( OTF_Writer* writer, uint64_t time, 
		uint32_t statetoken, uint32_t cpuid, uint32_t scltoken ) {

	
	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, cpuid ) );


	return OTF_WStream_writeEnter( stream, time, 
		statetoken, cpuid, scltoken );
}

int OTF_Writer_writeEnterKV( OTF_Writer* writer, uint64_t time, 
		uint32_t statetoken, uint32_t cpuid, uint32_t scltoken,
		OTF_KeyValueList *list ) {

	
	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, cpuid ) );


	return OTF_WStream_writeEnterKV( stream, time, 
		statetoken, cpuid, scltoken, list );
}


int OTF_Writer_writeRecvMsg( OTF_Writer* writer, uint64_t time, 
		uint32_t receiver, uint32_t sender, uint32_t communicator, 
		uint32_t msgtag, uint32_t msglength, uint32_t scltoken ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, 
		OTF_Writer_mapProcess( writer, receiver ) );


	return OTF_WStream_writeRecvMsg( stream, time, receiver, sender, 
		communicator, msgtag, msglength, scltoken );
}

int OTF_Writer_writeRecvMsgKV( OTF_Writer* writer, uint64_t time, 
		uint32_t receiver, uint32_t sender, uint32_t communicator, 
		uint32_t msgtag, uint32_t msglength, uint32_t scltoken,
		OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, 
		OTF_Writer_mapProcess( writer, receiver ) );


	return OTF_WStream_writeRecvMsgKV( stream, time, receiver, sender, 
		communicator, msgtag, msglength, scltoken, list );
}


int OTF_Writer_writeSendMsg( OTF_Writer* writer, uint64_t time, 
		uint32_t sender, uint32_t receiver, uint32_t communicator, 
		uint32_t msgtag, uint32_t msglength, uint32_t scltoken ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, 
		OTF_Writer_mapProcess( writer, sender ) );


	return OTF_WStream_writeSendMsg( stream, time, sender, receiver, 
		communicator, msgtag, msglength, scltoken );
}

int OTF_Writer_writeSendMsgKV( OTF_Writer* writer, uint64_t time, 
		uint32_t sender, uint32_t receiver, uint32_t communicator, 
		uint32_t msgtag, uint32_t msglength, uint32_t scltoken, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, 
		OTF_Writer_mapProcess( writer, sender ) );


	return OTF_WStream_writeSendMsgKV( stream, time, sender, receiver, 
		communicator, msgtag, msglength, scltoken, list );
}


int OTF_Writer_writeLeave( OTF_Writer* writer, uint64_t time, 
		uint32_t statetoken, uint32_t cpuid, uint32_t scltoken ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, 
		OTF_Writer_mapProcess( writer, cpuid ) );

	return OTF_WStream_writeLeave( stream, time, 
		statetoken, cpuid, scltoken );
}

int OTF_Writer_writeLeaveKV( OTF_Writer* writer, uint64_t time, 
		uint32_t statetoken, uint32_t cpuid, uint32_t scltoken, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer, 
		OTF_Writer_mapProcess( writer, cpuid ) );

	return OTF_WStream_writeLeaveKV( stream, time, 
		statetoken, cpuid, scltoken, list );
}


int OTF_Writer_writeCounter( OTF_Writer* writer, uint64_t time, 
		uint32_t process, uint32_t counter_token, uint64_t value ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeCounter( stream, time, 
		process, counter_token, value );
}

int OTF_Writer_writeCounterKV( OTF_Writer* writer, uint64_t time, 
		uint32_t process, uint32_t counter_token, uint64_t value, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeCounterKV( stream, time, 
		process, counter_token, value, list );
}


int OTF_Writer_writeCollectiveOperation( OTF_Writer* writer, uint64_t time, 
        uint32_t process, uint32_t collOp, uint32_t communicator, 
    	uint32_t rootprocess, uint32_t sent, uint32_t received, 
        uint64_t duration, uint32_t scltoken ) {


    OTF_WStream* stream= OTF_Writer_getStream( writer,
    OTF_Writer_mapProcess( writer, process ) );


    return OTF_WStream_writeCollectiveOperation( stream, time, process,
    	collOp, communicator, rootprocess, sent, received,
    	duration, scltoken );
}

int OTF_Writer_writeCollectiveOperationKV( OTF_Writer* writer, uint64_t time, 
        uint32_t process, uint32_t collOp, uint32_t communicator, 
    	uint32_t rootprocess, uint32_t sent, uint32_t received, 
        uint64_t duration, uint32_t scltoken, OTF_KeyValueList *list ) {


    OTF_WStream* stream= OTF_Writer_getStream( writer,
    OTF_Writer_mapProcess( writer, process ) );


    return OTF_WStream_writeCollectiveOperationKV( stream, time, process,
    	collOp, communicator, rootprocess, sent, received,
    	duration, scltoken, list );
}


int OTF_Writer_writeBeginCollectiveOperation( OTF_Writer* writer,
                uint64_t time, uint32_t process, uint32_t collOp,
                uint64_t matchingId, uint32_t procGroup, uint32_t rootProc,
                uint64_t sent, uint64_t received, uint32_t scltoken )
{
	OTF_WStream* stream = OTF_Writer_getStream( writer,
	                OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeBeginCollectiveOperation( stream, time,
			process, collOp, matchingId, procGroup, rootProc, sent,
			received, scltoken );
}

int OTF_Writer_writeBeginCollectiveOperationKV( OTF_Writer* writer,
                uint64_t time, uint32_t process, uint32_t collOp,
                uint64_t matchingId, uint32_t procGroup, uint32_t rootProc,
                uint64_t sent, uint64_t received, uint32_t scltoken, OTF_KeyValueList *list )
{
	OTF_WStream* stream = OTF_Writer_getStream( writer,
	                OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeBeginCollectiveOperationKV( stream, time,
			process, collOp, matchingId, procGroup, rootProc, sent,
			received, scltoken, list );
}


int OTF_Writer_writeEndCollectiveOperation( OTF_Writer* writer, uint64_t time,
                uint32_t process, uint64_t matchingId )
{
	OTF_WStream* stream = OTF_Writer_getStream( writer,
	                OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeEndCollectiveOperation( stream, time, process,
	                matchingId );
}

int OTF_Writer_writeEndCollectiveOperationKV( OTF_Writer* writer, uint64_t time,
                uint32_t process, uint64_t matchingId, OTF_KeyValueList *list )
{
	OTF_WStream* stream = OTF_Writer_getStream( writer,
	                OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeEndCollectiveOperationKV( stream, time, process,
	                matchingId, list );
}


int OTF_Writer_writeEventComment( OTF_Writer* writer, uint64_t time, 
		uint32_t process, const char* comment ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );


	return OTF_WStream_writeEventComment( stream, time, process,
		comment );
}

int OTF_Writer_writeEventCommentKV( OTF_Writer* writer, uint64_t time, 
		uint32_t process, const char* comment, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );


	return OTF_WStream_writeEventCommentKV( stream, time, process,
		comment, list );
}


int OTF_Writer_writeBeginProcess( OTF_Writer* writer, uint64_t time,
                                  uint32_t process ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeBeginProcess( stream, time, process );
}

int OTF_Writer_writeBeginProcessKV( OTF_Writer* writer, uint64_t time,
                                  uint32_t process, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeBeginProcessKV( stream, time, process, list );
}


int OTF_Writer_writeEndProcess( OTF_Writer* writer, uint64_t time,
                                uint32_t process ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeEndProcess( stream, time, process );
}

int OTF_Writer_writeEndProcessKV( OTF_Writer* writer, uint64_t time,
                                uint32_t process, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeEndProcessKV( stream, time, process, list );
}


int OTF_Writer_writeFileOperation( OTF_Writer* writer, uint64_t time, uint32_t fileid,
	uint32_t process, uint64_t handleid, uint32_t operation, uint64_t bytes,
	uint64_t duration, uint32_t source ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
	        OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeFileOperation( stream, time, fileid, process,
		handleid, operation, bytes, duration, source );
}

int OTF_Writer_writeFileOperationKV( OTF_Writer* writer, uint64_t time, uint32_t fileid,
	uint32_t process, uint64_t handleid, uint32_t operation, uint64_t bytes,
	uint64_t duration, uint32_t source, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
	        OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeFileOperationKV( stream, time, fileid, process,
		handleid, operation, bytes, duration, source, list );
}


int OTF_Writer_writeBeginFileOperation( OTF_Writer* writer, uint64_t time,
                uint32_t process, uint64_t matchingId, uint32_t scltoken )
{
	OTF_WStream* stream = OTF_Writer_getStream( writer,
	                OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeBeginFileOperation( stream, time, process,
	                matchingId, scltoken );
}

int OTF_Writer_writeBeginFileOperationKV( OTF_Writer* writer, uint64_t time,
                uint32_t process, uint64_t matchingId, uint32_t scltoken,
                OTF_KeyValueList *list )
{
	OTF_WStream* stream = OTF_Writer_getStream( writer,
	                OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeBeginFileOperationKV( stream, time, process,
	                matchingId, scltoken, list );
}


int OTF_Writer_writeEndFileOperation( OTF_Writer* writer, uint64_t time,
                uint32_t process, uint32_t fileid, uint64_t matchingId,
                uint64_t handleId, uint32_t operation, uint64_t bytes,
                uint32_t scltoken )
{
	OTF_WStream* stream = OTF_Writer_getStream( writer,
	                OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeEndFileOperation( stream, time, process, fileid,
	                matchingId, handleId, operation, bytes, scltoken );
}

int OTF_Writer_writeEndFileOperationKV( OTF_Writer* writer, uint64_t time,
                uint32_t process, uint32_t fileid, uint64_t matchingId, 
                uint64_t handleId, uint32_t operation, uint64_t bytes,
                uint32_t scltoken, OTF_KeyValueList *list )
{
	OTF_WStream* stream = OTF_Writer_getStream( writer,
	                OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeEndFileOperationKV( stream, time, process, fileid,
	                matchingId, handleId, operation, bytes, scltoken, list );
}


int OTF_Writer_writeRMAPut( OTF_Writer* writer, uint64_t time,
        uint32_t process, uint32_t origin, uint32_t target, uint32_t communicator,
        uint32_t tag, uint64_t bytes, uint32_t scltoken ) {


        OTF_WStream* stream= OTF_Writer_getStream( writer,
                OTF_Writer_mapProcess( writer, process ) );

        return OTF_WStream_writeRMAPut( stream, time, process, origin, target,
                communicator, tag, bytes, scltoken );
}

int OTF_Writer_writeRMAPutKV( OTF_Writer* writer, uint64_t time,
        uint32_t process, uint32_t origin, uint32_t target, uint32_t communicator,
        uint32_t tag, uint64_t bytes, uint32_t scltoken, OTF_KeyValueList *list ) {


        OTF_WStream* stream= OTF_Writer_getStream( writer,
                OTF_Writer_mapProcess( writer, process ) );

        return OTF_WStream_writeRMAPutKV( stream, time, process, origin, target,
                communicator, tag, bytes, scltoken, list );
}


int OTF_Writer_writeRMAPutRemoteEnd( OTF_Writer* writer, uint64_t time,
        uint32_t process, uint32_t origin, uint32_t target, uint32_t communicator,
        uint32_t tag, uint64_t bytes, uint32_t scltoken ) {


        OTF_WStream* stream= OTF_Writer_getStream( writer,
                OTF_Writer_mapProcess( writer, process ) );

        return OTF_WStream_writeRMAPutRemoteEnd( stream, time, process,
                origin, target, communicator, tag, bytes, scltoken );
}

int OTF_Writer_writeRMAPutRemoteEndKV( OTF_Writer* writer, uint64_t time,
        uint32_t process, uint32_t origin, uint32_t target, uint32_t communicator,
        uint32_t tag, uint64_t bytes, uint32_t scltoken, OTF_KeyValueList *list ) {


        OTF_WStream* stream= OTF_Writer_getStream( writer,
                OTF_Writer_mapProcess( writer, process ) );

        return OTF_WStream_writeRMAPutRemoteEndKV( stream, time, process,
                origin, target, communicator, tag, bytes, scltoken, list );
}


int OTF_Writer_writeRMAGet( OTF_Writer* writer, uint64_t time,
        uint32_t process, uint32_t origin, uint32_t target, uint32_t communicator,
        uint32_t tag, uint64_t bytes, uint32_t scltoken ) {


        OTF_WStream* stream= OTF_Writer_getStream( writer,
                OTF_Writer_mapProcess( writer, process ) );

        return OTF_WStream_writeRMAGet( stream, time, process, origin, target,
                communicator, tag, bytes, scltoken );
}

int OTF_Writer_writeRMAGetKV( OTF_Writer* writer, uint64_t time,
        uint32_t process, uint32_t origin, uint32_t target, uint32_t communicator,
        uint32_t tag, uint64_t bytes, uint32_t scltoken, OTF_KeyValueList *list ) {


        OTF_WStream* stream= OTF_Writer_getStream( writer,
                OTF_Writer_mapProcess( writer, process ) );

        return OTF_WStream_writeRMAGetKV( stream, time, process, origin, target,
                communicator, tag, bytes, scltoken, list );
}



int OTF_Writer_writeRMAEnd( OTF_Writer* writer, uint64_t time,
        uint32_t process, uint32_t remote, uint32_t communicator, uint32_t tag,
        uint32_t scltoken ) {


        OTF_WStream* stream= OTF_Writer_getStream( writer,
                OTF_Writer_mapProcess( writer, process ) );

        return OTF_WStream_writeRMAEnd( stream, time, process, remote, communicator,
                tag, scltoken );
}

int OTF_Writer_writeRMAEndKV( OTF_Writer* writer, uint64_t time,
        uint32_t process, uint32_t remote, uint32_t communicator, uint32_t tag,
        uint32_t scltoken, OTF_KeyValueList *list ) {


        OTF_WStream* stream= OTF_Writer_getStream( writer,
                OTF_Writer_mapProcess( writer, process ) );

        return OTF_WStream_writeRMAEndKV( stream, time, process, remote, communicator,
                tag, scltoken, list );
}


/* *** public snapshot record write handlers *** */


int OTF_Writer_writeSnapshotComment( OTF_Writer* writer, uint64_t time, 
		uint32_t process, const char* comment ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );


	return OTF_WStream_writeSnapshotComment( stream, time, process,
		comment );
}

int OTF_Writer_writeSnapshotCommentKV( OTF_Writer* writer, uint64_t time, 
		uint32_t process, const char* comment, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );


	return OTF_WStream_writeSnapshotCommentKV( stream, time, process,
		comment, list );
}


int OTF_Writer_writeEnterSnapshot( OTF_Writer* writer, uint64_t time, 
		uint64_t originaltime, uint32_t function, 
		uint32_t process, uint32_t source ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeEnterSnapshot( stream, time, originaltime,
		function, process, source );
}

int OTF_Writer_writeEnterSnapshotKV( OTF_Writer* writer, uint64_t time, 
		uint64_t originaltime, uint32_t function, 
		uint32_t process, uint32_t source, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeEnterSnapshotKV( stream, time, originaltime,
		function, process, source, list );
}


int OTF_Writer_writeSendSnapshot( OTF_Writer* writer, uint64_t time,
		uint64_t originaltime, uint32_t sender, uint32_t receiver,
		uint32_t procGroup, uint32_t type, uint32_t length, uint32_t source ) {
		
		
	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, sender ) );
		
	return OTF_WStream_writeSendSnapshot( stream, time, originaltime,
		sender, receiver, procGroup, type, length, source );
}

int OTF_Writer_writeSendSnapshotKV( OTF_Writer* writer, uint64_t time,
		uint64_t originaltime, uint32_t sender, uint32_t receiver,
		uint32_t procGroup, uint32_t type, uint32_t length,
        uint32_t source, OTF_KeyValueList *list ) {
		
		
	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, sender ) );
		
	return OTF_WStream_writeSendSnapshotKV( stream, time, originaltime,
		sender, receiver, procGroup, type, length, source, list );
}


int OTF_Writer_writeOpenFileSnapshot( OTF_Writer* writer, uint64_t time,
	uint64_t originaltime, uint32_t fileid, uint32_t process, uint64_t handleid,
	uint32_t source ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeOpenFileSnapshot( stream, time, originaltime,
		fileid, process, handleid, source );
}

int OTF_Writer_writeOpenFileSnapshotKV( OTF_Writer* writer, uint64_t time,
	uint64_t originaltime, uint32_t fileid, uint32_t process, uint64_t handleid,
	uint32_t source, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeOpenFileSnapshotKV( stream, time, originaltime,
		fileid, process, handleid, source, list );
}

int OTF_Writer_writeBeginCollopSnapshot( OTF_Writer* writer, uint64_t time,
    uint64_t originaltime, uint32_t process, uint32_t collOp, uint64_t matchingId,
    uint32_t procGroup, uint32_t rootProc, uint64_t sent, uint64_t received,
    uint32_t scltoken ) {


    OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );

    
	return OTF_WStream_writeBeginCollopSnapshot( stream, time, originaltime,
		process, collOp, matchingId, procGroup, rootProc, sent, received,
        scltoken );
}

int OTF_Writer_writeBeginCollopSnapshotKV( OTF_Writer* writer, uint64_t time,
    uint64_t originaltime, uint32_t process, uint32_t collOp, uint64_t matchingId,
    uint32_t procGroup, uint32_t rootProc, uint64_t sent, uint64_t received,
    uint32_t scltoken, OTF_KeyValueList *list ) {


    OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );

    
	return OTF_WStream_writeBeginCollopSnapshotKV( stream, time, originaltime,
		process, collOp, matchingId, procGroup, rootProc, sent, received,
        scltoken, list );
}

int OTF_Writer_writeBeginFileOpSnapshot( OTF_Writer* writer, uint64_t time,
    uint64_t originaltime, uint32_t process, uint64_t matchingId, uint32_t scltoken ) {


    OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );


    return OTF_WStream_writeBeginFileOpSnapshot( stream, time, originaltime, 
        process, matchingId, scltoken );

}

int OTF_Writer_writeBeginFileOpSnapshotKV( OTF_Writer* writer, uint64_t time,
    uint64_t originaltime, uint32_t process, uint64_t matchingId,
    uint32_t scltoken, OTF_KeyValueList *list ) {


    OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );


    return OTF_WStream_writeBeginFileOpSnapshotKV( stream, time, originaltime, 
        process, matchingId, scltoken, list );

}

int OTF_Writer_writeCollopCountSnapshot( OTF_Writer* writer,
                                         uint64_t time,
                                         uint32_t process,
                                         uint32_t communicator,
                                         uint64_t count,
                                         OTF_KeyValueList *list ) {

    OTF_WStream* stream= OTF_Writer_getStream( writer,
        OTF_Writer_mapProcess( writer, process ) );


    return OTF_WStream_writeCollopCountSnapshot( stream,
                                                 time,
                                                 process,
                                                 communicator,
                                                 count,
                                                 list );
}

int OTF_Writer_writeCounterSnapshot( OTF_Writer*       writer,
                                     uint64_t          time,
                                     uint64_t          originaltime,
                                     uint32_t          process,
                                     uint32_t          counter,
                                     uint64_t          value,
                                     OTF_KeyValueList *list ) {

    OTF_WStream* stream= OTF_Writer_getStream( writer,
        OTF_Writer_mapProcess( writer, process ) );

    return OTF_WStream_writeCounterSnapshot( stream,
                                             time,
                                             originaltime,
                                             process,
                                             counter,
                                             value,
                                             list );
}


/* *** public statistics record write handlers *** */


int OTF_Writer_writeSummaryComment( OTF_Writer* writer, uint64_t time, 
		uint32_t process, const char* comment ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );


	return OTF_WStream_writeSummaryComment( stream, time, process,
		comment );
}

int OTF_Writer_writeSummaryCommentKV( OTF_Writer* writer, uint64_t time, 
		uint32_t process, const char* comment, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );


	return OTF_WStream_writeSummaryCommentKV( stream, time, process,
		comment, list );
}


int OTF_Writer_writeFunctionSummary( OTF_Writer* writer, 
		uint64_t time, uint32_t function, uint32_t process, 
		uint64_t count, uint64_t excltime, uint64_t incltime ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeFunctionSummary( stream, 
		time, function, process, count, excltime, incltime );
}

int OTF_Writer_writeFunctionSummaryKV( OTF_Writer* writer, 
		uint64_t time, uint32_t function, uint32_t process, 
		uint64_t count, uint64_t excltime, uint64_t incltime, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeFunctionSummaryKV( stream, 
		time, function, process, count, excltime, incltime, list );
}


int OTF_Writer_writeFunctionGroupSummary( OTF_Writer* writer, 
		uint64_t time, uint32_t functiongroup, uint32_t process,  
		uint64_t count, uint64_t excltime, uint64_t incltime ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeFunctionGroupSummary( stream, 
		time, functiongroup, process, count, excltime, incltime );
}

int OTF_Writer_writeFunctionGroupSummaryKV( OTF_Writer* writer, 
		uint64_t time, uint32_t functiongroup, uint32_t process,  
		uint64_t count, uint64_t excltime, uint64_t incltime, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeFunctionGroupSummaryKV( stream, 
		time, functiongroup, process, count, excltime, incltime, list );
}


int OTF_Writer_writeMessageSummary( OTF_Writer* writer, 
		uint64_t time, uint32_t process, uint32_t peer, 
		uint32_t comm, uint32_t tag, uint64_t number_sent,
		uint64_t number_recved, uint64_t bytes_sent,  
		uint64_t bytes_recved ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeMessageSummary( stream, time, process, peer, comm,
		tag, number_sent, number_recved, bytes_sent, bytes_recved );
}

int OTF_Writer_writeMessageSummaryKV( OTF_Writer* writer, 
		uint64_t time, uint32_t process, uint32_t peer, 
		uint32_t comm, uint32_t tag, uint64_t number_sent,
		uint64_t number_recved, uint64_t bytes_sent,  
		uint64_t bytes_recved, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeMessageSummaryKV( stream, time, process, peer, comm,
		tag, number_sent, number_recved, bytes_sent, bytes_recved, list );
}


int OTF_Writer_writeCollopSummary( OTF_Writer* writer,
		uint64_t time, uint32_t process, uint32_t comm, uint32_t collective,
		uint64_t number_sent, uint64_t number_recved, uint64_t bytes_sent,
		uint64_t bytes_recved ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeCollopSummary( stream, time, process, comm,
		collective, number_sent, number_recved, bytes_sent, bytes_recved );
}

int OTF_Writer_writeCollopSummaryKV( OTF_Writer* writer,
		uint64_t time, uint32_t process, uint32_t comm, uint32_t collective,
		uint64_t number_sent, uint64_t number_recved, uint64_t bytes_sent,
		uint64_t bytes_recved, OTF_KeyValueList *list ) {


	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );

	return OTF_WStream_writeCollopSummaryKV( stream, time, process, comm,
		collective, number_sent, number_recved, bytes_sent, bytes_recved, list );
}


int OTF_Writer_writeFileOperationSummary( OTF_Writer* writer, uint64_t time,
	uint32_t fileid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite ) {

	
	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );


	return OTF_WStream_writeFileOperationSummary( stream, time, fileid, process,
		nopen, nclose, nread, nwrite, nseek, bytesread, byteswrite );
}

int OTF_Writer_writeFileOperationSummaryKV( OTF_Writer* writer, uint64_t time,
	uint32_t fileid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite, OTF_KeyValueList *list ) {

	
	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );


	return OTF_WStream_writeFileOperationSummaryKV( stream, time, fileid, process,
		nopen, nclose, nread, nwrite, nseek, bytesread, byteswrite, list );
}


int OTF_Writer_writeFileGroupOperationSummary( OTF_Writer* writer, uint64_t time,
	uint32_t groupid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite ) {

	
	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );


	return OTF_WStream_writeFileGroupOperationSummary( stream, time, groupid, process,
		nopen, nclose, nread, nwrite, nseek, bytesread, byteswrite );
}

int OTF_Writer_writeFileGroupOperationSummaryKV( OTF_Writer* writer, uint64_t time,
	uint32_t groupid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite, OTF_KeyValueList *list ) {

	
	OTF_WStream* stream= OTF_Writer_getStream( writer,
		OTF_Writer_mapProcess( writer, process ) );


	return OTF_WStream_writeFileGroupOperationSummaryKV( stream, time, groupid, process,
		nopen, nclose, nread, nwrite, nseek, bytesread, byteswrite, list );
}


int OTF_Writer_writeDefMarker( OTF_Writer* writer, uint32_t streamID,
    uint32_t token, const char* name, uint32_t type ) {

	OTF_WStream* stream= OTF_Writer_getStream( writer, 0 );

#ifdef OTF_DEBUG
	if ( 0 != streamID ) {

		OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				 "streamID value of '%u' is ignored, '0' is used instead\n",
				 __FUNCTION__, __FILE__, __LINE__, streamID );
	}
#endif /* OTF_DEBUG */

	return OTF_WStream_writeDefMarker( stream, token, name, type );
}

int OTF_Writer_writeDefMarkerKV( OTF_Writer* writer, uint32_t streamID,
    uint32_t token, const char* name, uint32_t type, OTF_KeyValueList *list ) {

	OTF_WStream* stream= OTF_Writer_getStream( writer, 0 );

#ifdef OTF_DEBUG
	if ( 0 != streamID ) {

		OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				 "streamID value of '%u' is ignored, '0' is used instead\n",
				 __FUNCTION__, __FILE__, __LINE__, streamID );
	}
#endif /* OTF_DEBUG */

	return OTF_WStream_writeDefMarkerKV( stream, token, name, type, list );
}


int OTF_Writer_writeMarker( OTF_Writer* writer, 
    uint64_t time, uint32_t process, uint32_t token, const char* text ) {

	OTF_WStream* stream= OTF_Writer_getStream( writer, 0 );

	return OTF_WStream_writeMarker( stream, time, process, token, text );
}

int OTF_Writer_writeMarkerKV( OTF_Writer* writer, 
    uint64_t time, uint32_t process, uint32_t token, const char* text, OTF_KeyValueList *list ) {

	OTF_WStream* stream= OTF_Writer_getStream( writer, 0 );

	return OTF_WStream_writeMarkerKV( stream, time, process, token, text, list );
}


int OTF_Writer_closeAllStreams( OTF_Writer* writer ) {


	int ret= 1;
	uint32_t i;

	for ( i= 0; i < writer->n; ++i ) {

		ret&= OTF_WStream_close( writer->streams[i] );
	}

	writer->n= 0;
	
	return ret;
}

