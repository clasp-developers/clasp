/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include "OTF_Platform.h"
#include "OTF_Definitions.h"

#include <string.h>
#include <stdio.h>
#include <errno.h>

#ifdef HAVE_SYS_TIME_H
	#include <sys/time.h>
#endif

#ifdef HAVE_SYS_TYPES_H
	#include <sys/types.h>
#endif

#ifdef HAVE_UNISTD_H
	#include <unistd.h>
#endif

#include "OTF_WStream.h"
#include "OTF_Platform.h"
#include "OTF_Filenames.h"

#include "OTF_Keywords.h"
#include "OTF_Errno.h"

/** constructor - internal use only */
int OTF_WStream_init( OTF_WStream* wstream );

/** destructor - internal use only */
int OTF_WStream_finish( OTF_WStream* wstream );

/** Write a DEFUNIQUEID record to stream 'wstream'. */
int OTF_WStream_writeDefUniqueId( OTF_WStream* wstream, uint64_t uid );

/** Write a DEFVERSION record to stream 'wstream'. */
int OTF_WStream_writeDefVersion( OTF_WStream* wstream, uint8_t major,
	uint8_t minor, uint8_t sub, const char* string );

/* ************************************************************************* */


int OTF_WStream_init( OTF_WStream* wstream ) {


	wstream->namestub= NULL;
	wstream->id= (uint32_t) -1;

	wstream->format= OTF_WSTREAM_FORMAT_SHORT;

	wstream->defBuffer= NULL;
	wstream->eventBuffer= NULL;
	wstream->snapsBuffer= NULL;
	wstream->statsBuffer= NULL;
	wstream->markerBuffer= NULL;

#ifdef HAVE_ZLIB
	wstream->compression= 0;
	wstream->zbuffersizes= OTF_ZBUFFER_DEFAULTSIZE;
#endif /* HAVE_ZLIB */

	wstream->buffersizes= 1024*1024;

	return 1;
}


int OTF_WStream_finish( OTF_WStream* wstream ) {


	int ret= 1;
#	ifdef OTF_DEBUG
		int tmpret;
#	endif
	
	free( wstream->namestub );
	wstream->namestub= NULL;

	wstream->id= (uint32_t) -1;


	if ( NULL != wstream->defBuffer ) {

#		ifndef OTF_DEBUG
			ret&= OTF_WBuffer_close( wstream->defBuffer );
#		else
			tmpret= OTF_WBuffer_close( wstream->defBuffer );
			if( 0 == tmpret ) {
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_close() failed for the def buffer.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			}
			ret&= tmpret;
#		endif

		wstream->defBuffer= NULL;
	}

	if ( NULL != wstream->eventBuffer ) {

#		ifndef OTF_DEBUG
			ret&= OTF_WBuffer_close( wstream->eventBuffer );
#		else
			tmpret= OTF_WBuffer_close( wstream->eventBuffer );
			if( 0 == tmpret ) {
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_close() failed for the event buffer.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			}
			ret&= tmpret;
#		endif

		wstream->eventBuffer= NULL;
	}

	if ( NULL != wstream->snapsBuffer ) {

#		ifndef OTF_DEBUG
			ret&= OTF_WBuffer_close( wstream->snapsBuffer );
#		else
			tmpret= OTF_WBuffer_close( wstream->snapsBuffer );
			if( 0 == tmpret ) {
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_close() failed for the snapshots buffer.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			}
			ret&= tmpret;
#		endif
		
		wstream->snapsBuffer= NULL;
	}

	if ( NULL != wstream->statsBuffer ) {

#		ifndef OTF_DEBUG
			ret&= OTF_WBuffer_close( wstream->statsBuffer );
#		else
			tmpret= OTF_WBuffer_close( wstream->statsBuffer );
			if( 0 == tmpret ) {
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_close() failed for the statistics buffer.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			}
			ret&= tmpret;
#		endif
		
		wstream->statsBuffer= NULL;
	}

	if ( NULL != wstream->markerBuffer ) {

#		ifndef OTF_DEBUG
			ret&= OTF_WBuffer_close( wstream->markerBuffer );
#		else
			tmpret= OTF_WBuffer_close( wstream->markerBuffer );
			if( 0 == tmpret ) {
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_close() failed for the statistics buffer.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			}
			ret&= tmpret;
#		endif
		
		wstream->markerBuffer= NULL;
	}

	return ret;
}


OTF_WStream* OTF_WStream_open( const char* namestub, uint32_t id,
		OTF_FileManager* manager ) {


	OTF_WStream* ret= NULL;

	if( NULL == manager ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"manager has not been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return NULL;
	}

	ret= (OTF_WStream*) malloc( sizeof(OTF_WStream) );
	if( NULL == ret ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return NULL;
	}

	OTF_WStream_init( ret );

	ret->namestub= strdup( namestub );
	ret->id= id;
	ret->manager= manager;

	/* leave buffers allone, they are allocated on demand */

	return ret;
}


int OTF_WStream_close( OTF_WStream* wstream ) {


	int ret= 0;
	
	if ( NULL != wstream ) {

		ret= OTF_WStream_finish( wstream );
		if( 0 == ret ) {
			
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WStream_finish() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
		}

		free( wstream );
		wstream = NULL;
	}

	return ret;
}


int OTF_WStream_flush( OTF_WStream* wstream ) {


	int retval= 1;

	if ( NULL != wstream->defBuffer ) {

		retval= retval & OTF_WBuffer_flush( wstream->defBuffer );
	}

	if ( NULL != wstream->eventBuffer ) {

		retval= retval & OTF_WBuffer_flush( wstream->eventBuffer );
	}

	if ( NULL != wstream->snapsBuffer ) {

		retval= retval & OTF_WBuffer_flush( wstream->snapsBuffer );
	}

	if ( NULL != wstream->statsBuffer ) {

		retval= retval & OTF_WBuffer_flush( wstream->statsBuffer );
	}

	if ( NULL != wstream->markerBuffer ) {

		retval= retval & OTF_WBuffer_flush( wstream->markerBuffer );
	}

	return retval;
}


OTF_WBuffer* OTF_WStream_getDefBuffer( OTF_WStream* wstream ) {


	/* have filename allocated an freed in order to avoid problems with 
	pre-allocated buffer being too short */


	char* filename;


	if ( NULL == wstream->defBuffer ) {


		filename= OTF_getFilename( wstream->namestub,
			wstream->id, OTF_FILETYPE_DEF, 0, NULL );
		if( NULL == filename ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_getFilename() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return NULL;
		}

#ifdef HAVE_ZLIB
		if ( wstream->compression > 0 ) {
		
			filename = strcat( filename, ".z" );
		}
#endif /* HAVE_ZLIB */

		if( NULL == filename ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return NULL;
		}

		wstream->defBuffer= OTF_WBuffer_open_zlevel( filename, wstream->manager,
			wstream->compression );

		if( NULL == wstream->defBuffer ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_open() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return NULL;
		}

#ifdef HAVE_ZLIB
		OTF_WBuffer_setZBufferSize( wstream->defBuffer, wstream->zbuffersizes );
#endif /* HAVE_ZLIB */
		
		free( filename );
		filename = NULL;

#		ifndef OTF_DEBUG
			OTF_WBuffer_setSize( wstream->defBuffer, wstream->buffersizes );
#		else
			if( 0 == OTF_WBuffer_setSize( wstream->defBuffer,
				wstream->buffersizes ) ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_setSize() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			}
#		endif
	}

	return wstream->defBuffer;
}


OTF_WBuffer* OTF_WStream_getEventBuffer( OTF_WStream* wstream ) {


	/* have filename allocated an freed in order to avoid problems with 
	pre-allocated buffer being too short */


	char* filename;


	if ( NULL == wstream->eventBuffer ) {


		filename= OTF_getFilename( wstream->namestub,
			wstream->id, OTF_FILETYPE_EVENT, 0, NULL );
		if( NULL == filename ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_getFilename() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return NULL;
		}

#ifdef HAVE_ZLIB
		if ( wstream->compression > 0 ) {
		
			filename = strcat( filename, ".z" );
		}
#endif /* HAVE_ZLIB */

		if( NULL == filename ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return NULL;
		}
		
		wstream->eventBuffer= OTF_WBuffer_open_zlevel( filename, wstream->manager,
			wstream->compression );

		if( NULL == wstream->eventBuffer ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_open() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return NULL;
		}

#ifdef HAVE_ZLIB
		OTF_WBuffer_setZBufferSize( wstream->eventBuffer, wstream->zbuffersizes );
#endif /* HAVE_ZLIB */
		
		free( filename );
		filename = NULL;

#		ifndef OTF_DEBUG
			OTF_WBuffer_setSize( wstream->eventBuffer, wstream->buffersizes );
#		else
			if( 0 == OTF_WBuffer_setSize( wstream->eventBuffer,
				wstream->buffersizes ) ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_setSize() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			}
#		endif
	}

	return wstream->eventBuffer;
}


OTF_WBuffer* OTF_WStream_getSnapshotBuffer( OTF_WStream* wstream ) {


	/* have filename allocated an freed in order to avoid problems with 
	pre-allocated buffer being too short */


	char* filename;

	if ( ( wstream->format & 2 ) == OTF_WSTREAM_FORMAT_INLINE_SNAPSHOTS ) {

		return OTF_WStream_getEventBuffer( wstream );
	}

	if ( NULL == wstream->snapsBuffer ) {


		filename= OTF_getFilename( wstream->namestub,
			wstream->id, OTF_FILETYPE_SNAPS, 0, NULL );
		if( NULL == filename ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_getFilename() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return NULL;
		}

#ifdef HAVE_ZLIB
		if ( wstream->compression > 0) {
		
			filename = strcat( filename, ".z" );
		}
#endif /* HAVE_ZLIB */

		if( NULL == filename ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return NULL;
		}
		
		wstream->snapsBuffer= OTF_WBuffer_open_zlevel( filename, wstream->manager,
			wstream->compression );
		if( NULL == wstream->snapsBuffer ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_open() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return NULL;
		}

#ifdef HAVE_ZLIB
		OTF_WBuffer_setZBufferSize( wstream->snapsBuffer, wstream->zbuffersizes );
#endif /* HAVE_ZLIB */
		
		free( filename );
		filename = NULL;

#		ifndef OTF_DEBUG
			OTF_WBuffer_setSize( wstream->snapsBuffer, wstream->buffersizes );
#		else
			if( 0 == OTF_WBuffer_setSize( wstream->snapsBuffer,
				wstream->buffersizes ) ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_setSize() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			}
#		endif
	}

	return wstream->snapsBuffer;
}


OTF_WBuffer* OTF_WStream_getStatsBuffer( OTF_WStream* wstream ) {


	/* have filename allocated an freed in order to avoid problems with 
	pre-allocated buffer being too short */


	char* filename;


	if ( NULL == wstream->statsBuffer ) {


		filename= OTF_getFilename( wstream->namestub,
			wstream->id, OTF_FILETYPE_STATS, 0, NULL );
		if( NULL == filename ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_getFilename() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return NULL;
		}

#ifdef HAVE_ZLIB
		if ( wstream->compression > 0) {
		
			filename = strcat( filename, ".z" );
		}
#endif /* HAVE_ZLIB */

		if( NULL == filename ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return NULL;
		}
		
		wstream->statsBuffer= OTF_WBuffer_open_zlevel( filename, wstream->manager,
			wstream->compression );
		if( NULL == wstream->statsBuffer ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_open() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return NULL;
		}

		free( filename );
		filename = NULL;

#		ifndef OTF_DEBUG
			OTF_WBuffer_setSize( wstream->statsBuffer, wstream->buffersizes );
#		else
			if( 0 == OTF_WBuffer_setSize( wstream->statsBuffer,
				wstream->buffersizes ) ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_setSize() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			}
#		endif
	}

	return wstream->statsBuffer;
}


OTF_WBuffer* OTF_WStream_getMarkerBuffer( OTF_WStream* wstream ) {


	/* have filename allocated an freed in order to avoid problems with 
	pre-allocated buffer being too short */


	char* filename;


	if ( NULL == wstream->markerBuffer ) {


		filename= OTF_getFilename( wstream->namestub,
			wstream->id, OTF_FILETYPE_MARKER, 0, NULL );
		if( NULL == filename ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_getFilename() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return NULL;
		}

#ifdef HAVE_ZLIB
		if ( wstream->compression > 0) {
		
			filename = strcat( filename, ".z" );
		}
#endif /* HAVE_ZLIB */

		if( NULL == filename ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return NULL;
		}
		
		wstream->markerBuffer= OTF_WBuffer_open_zlevel( filename, wstream->manager,
			wstream->compression );
		if( NULL == wstream->markerBuffer ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_open() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return NULL;
		}

		free( filename );
		filename = NULL;

#		ifndef OTF_DEBUG
			OTF_WBuffer_setSize( wstream->markerBuffer, wstream->buffersizes );
#		else
			if( 0 == OTF_WBuffer_setSize( wstream->markerBuffer,
				wstream->buffersizes ) ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_WBuffer_setSize() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			}
#		endif
	}

	return wstream->markerBuffer;
}


int OTF_WStream_setCompression( OTF_WStream* wstream, OTF_FileCompression
	compression ) {


#ifdef HAVE_ZLIB	
	if ( compression <= 9 ) {
	
		wstream->compression = compression;
		
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

	
OTF_FileCompression OTF_WStream_getCompression( OTF_WStream* wstream ) {

#ifdef HAVE_ZLIB
	return wstream->compression;
#else
	return 0;
#endif /* HAVE_ZLIB */
}


void OTF_WStream_setBufferSizes( OTF_WStream* wstream, uint32_t size ) {


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

	wstream->buffersizes= size;
}


uint32_t OTF_WStream_getBufferSizes(OTF_WStream* wstream) {


	return wstream->buffersizes;
}


void OTF_WStream_setZBufferSizes( OTF_WStream* wstream, uint32_t size ) {


#ifdef HAVE_ZLIB
	
	if ( 32 > size ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"intended zbuffer size %u is too small, rejected.\n",
				__FUNCTION__, __FILE__, __LINE__, size );
		
		return;

	} else if ( 512 > size ) {
	
		OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				"zbuffer size %u is very small, accepted though.\n",
				__FUNCTION__, __FILE__, __LINE__, size );

	} else if ( 10 * 1024 *1024 < size ) {

		OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				"zbuffer size %u is rather big, accepted though.\n",
				__FUNCTION__, __FILE__, __LINE__, size );

	}

	wstream->zbuffersizes= size;

#endif /* HAVE_ZLIB */
}


uint32_t OTF_WStream_getZBufferSizes(OTF_WStream* wstream) {


#ifdef HAVE_ZLIB
	return wstream->zbuffersizes;
#else /* HAVE_ZLIB */
	return 0;
#endif /* HAVE_ZLIB */
}


void OTF_WStream_setFormat( OTF_WStream* wstream, uint32_t format ) {


	if ( format > 3 ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"unknown ouput format chosen.\n",
				__FUNCTION__, __FILE__, __LINE__ );
	}
	
	wstream->format= format;
}


uint32_t OTF_WStream_getFormat( OTF_WStream* wstream ) {


	return wstream->format;
}


/* *** definition record write handlers *** ******************************** */


int OTF_WStream_writeDefinitionCommentKV( OTF_WStream* wstream,
		const char* comment, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {
	
		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFINITIONCOMMENT );

		OTF_WBuffer_writeString( buffer, comment );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFINITIONCOMMENT " " );

		OTF_WBuffer_writeString( buffer, comment );
		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeDefinitionComment( OTF_WStream* wstream,
		const char* comment ) {

	return OTF_WStream_writeDefinitionCommentKV(wstream, comment, NULL);
}


int OTF_WStream_writeDefTimerResolutionKV( OTF_WStream* wstream, 
		uint64_t ticksPerSecond, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);
	
		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFTIMERRESOLUTION );

		OTF_WBuffer_writeUint64( buffer, ticksPerSecond );
		OTF_WBuffer_writeNewline( buffer );
	
	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFTIMERRESOLUTION " " );

		OTF_WBuffer_writeUint64( buffer, ticksPerSecond );

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeDefTimerResolution( OTF_WStream* wstream, 
		uint64_t ticksPerSecond ) {

	return OTF_WStream_writeDefTimerResolutionKV( wstream, ticksPerSecond, NULL );
}


int OTF_WStream_writeDefProcessKV( OTF_WStream* wstream, uint32_t deftoken, 
		const char* name, uint32_t parent, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

#	ifdef OTF_DEBUG
		if( 0 == deftoken ) {
		
			OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif
			

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);
	
		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFPROCESS );

		OTF_WBuffer_writeUint32( buffer, deftoken );

		if ( NULL != name ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME );
			OTF_WBuffer_writeString( buffer, name );
		}

		if ( 0 != parent ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_PARENT );
			OTF_WBuffer_writeUint32( buffer, parent );
		}

		OTF_WBuffer_writeNewline( buffer );
	
	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFPROCESS " " );

		OTF_WBuffer_writeUint32( buffer, deftoken );

		if ( NULL != name ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NAME " " );
			OTF_WBuffer_writeString( buffer, name );
		}

		if ( 0 != parent ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_PARENT " " );
			OTF_WBuffer_writeUint32( buffer, parent );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeDefProcess( OTF_WStream* wstream, uint32_t deftoken, 
		const char* name, uint32_t parent ) {

	return OTF_WStream_writeDefProcessKV(wstream, deftoken, name, parent, NULL);
}


int OTF_WStream_writeDefProcessGroupKV( OTF_WStream* wstream, uint32_t deftoken, 
		const char* name, uint32_t n, const uint32_t* array, OTF_KeyValueList* list ) {


	unsigned int i;
	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

#	ifdef OTF_DEBUG
		if( 0 == deftoken ) {
		
			OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);
	
		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFPROCESSGROUP );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_MEMBERS );

		for ( i = 0; i < n; ++i ) {

			OTF_WBuffer_writeUint32( buffer, array[i] );
			OTF_WBuffer_writeChar( buffer, ',' );
		}

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME );
		OTF_WBuffer_writeString( buffer, name );

		OTF_WBuffer_writeNewline( buffer );
	
	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFPROCESSGROUP " " );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_MEMBERS " " );

		for ( i = 0; i < n; ++i ) {

			OTF_WBuffer_writeUint32( buffer, array[i] );
			OTF_WBuffer_writeChar( buffer, ',' );
		}

		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NAME " " );
		OTF_WBuffer_writeString( buffer, name );

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeDefProcessGroup( OTF_WStream* wstream, uint32_t deftoken, 
		const char* name, uint32_t n, const uint32_t* array ) {

	return OTF_WStream_writeDefProcessGroupKV(wstream, deftoken, name, n, array, NULL);
}


int OTF_WStream_writeDefAttributeListKV( OTF_WStream* wstream, uint32_t attr_token,
		uint32_t num, OTF_ATTR_TYPE* array, OTF_KeyValueList* list ) {


	uint32_t i;

	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFATTRLIST );

		OTF_WBuffer_writeUint32( buffer, attr_token );

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_MEMBERS );

		for ( i = 0; i < num; ++i ) {
			
			OTF_WBuffer_writeUint32( buffer, array[i] );
			OTF_WBuffer_writeChar( buffer, ',' );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFATTRLIST " " );

		OTF_WBuffer_writeUint32( buffer, attr_token );

		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_MEMBERS " " );

		for ( i = 0; i < num; ++i ) {
			
			OTF_WBuffer_writeUint32( buffer, array[i] );
			OTF_WBuffer_writeChar( buffer, ',' );
		}

		OTF_WBuffer_writeNewline( buffer );

	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeDefAttributeList( OTF_WStream* wstream, uint32_t attr_token,
		uint32_t num, OTF_ATTR_TYPE* array ) {

	return OTF_WStream_writeDefAttributeListKV(wstream, attr_token, num, array, NULL);
}


int OTF_WStream_writeDefProcessOrGroupAttributesKV( OTF_WStream* wstream,
		uint32_t proc_token, uint32_t attr_token, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFPROCESSORGROUPATTR );

		OTF_WBuffer_writeUint32( buffer, proc_token );

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_VALUE );
		OTF_WBuffer_writeUint32( buffer, attr_token );

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFPROCESSORGROUPATTR " " );

		OTF_WBuffer_writeUint32( buffer, proc_token );

		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_VALUE " " );
		OTF_WBuffer_writeUint32( buffer, attr_token );

		OTF_WBuffer_writeNewline( buffer );

	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeDefProcessOrGroupAttributes( OTF_WStream* wstream,
		uint32_t proc_token, uint32_t attr_token ) {

	return OTF_WStream_writeDefProcessOrGroupAttributesKV(wstream, proc_token, attr_token, NULL);
}


int OTF_WStream_writeDefFunctionKV( OTF_WStream* wstream, uint32_t deftoken, 
		const char* name, uint32_t group, uint32_t scltoken, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

#	ifdef OTF_DEBUG
		if( 0 == deftoken ) {
		
			OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif
			
	
	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);
	
		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFFUNCTION );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_GROUP );
		OTF_WBuffer_writeUint32( buffer, group );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME );
		OTF_WBuffer_writeString( buffer, name );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );
	
	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFFUNCTION " " );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_GROUP " " );
		OTF_WBuffer_writeUint32( buffer, group );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NAME " " );
		OTF_WBuffer_writeString( buffer, name );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeDefFunction( OTF_WStream* wstream, uint32_t deftoken, 
		const char* name, uint32_t group, uint32_t scltoken ) {

	return OTF_WStream_writeDefFunctionKV(wstream, deftoken, name, group, scltoken, NULL);
}


int OTF_WStream_writeDefFunctionGroupKV( OTF_WStream* wstream, 
		uint32_t deftoken, const char* name, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

#	ifdef OTF_DEBUG
		if( 0 == deftoken ) {
		
			OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif
			
	
	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFFUNCTIONGROUP );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFFUNCTIONGROUP " " );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NAME " " );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeDefFunctionGroup( OTF_WStream* wstream, 
		uint32_t deftoken, const char* name ) {

	return OTF_WStream_writeDefFunctionGroupKV(wstream, deftoken, name, NULL);
}


int OTF_WStream_writeDefCollectiveOperationKV( OTF_WStream* wstream, 
		uint32_t collOp, const char* name, uint32_t type, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

#	ifdef OTF_DEBUG
		if( 0 == collOp ) {
		
			OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif
	

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFCOLLOP );

		OTF_WBuffer_writeUint32( buffer, collOp );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_TYPE );
		OTF_WBuffer_writeUint32( buffer, type );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFCOLLOP " " );

		OTF_WBuffer_writeUint32( buffer, collOp );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NAME " " );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_TYPE " " );
		OTF_WBuffer_writeUint32( buffer, type );
		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeDefCollectiveOperation( OTF_WStream* wstream, 
		uint32_t collOp, const char* name, uint32_t type ) {

	return OTF_WStream_writeDefCollectiveOperationKV(wstream, collOp, name, type, NULL);
}


int OTF_WStream_writeDefCounterKV( OTF_WStream* wstream, uint32_t deftoken, 
		const char* name, uint32_t properties, uint32_t countergroup, 
		const char* unit, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

#	ifdef OTF_DEBUG
		if( 0 == deftoken ) {
		
			OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif
	

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFCOUNTER );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_GROUP );
		OTF_WBuffer_writeUint32( buffer, countergroup );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_PROPERTIES );
		OTF_WBuffer_writeUint32( buffer, properties );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_UNIT );
		OTF_WBuffer_writeString( buffer, unit );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFCOUNTER " " );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_GROUP " " );
		OTF_WBuffer_writeUint32( buffer, countergroup );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NAME " " );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_PROPERTIES " " );
		OTF_WBuffer_writeUint32( buffer, properties );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_UNIT " " );
		OTF_WBuffer_writeString( buffer, unit );
		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeDefCounter( OTF_WStream* wstream, uint32_t deftoken, 
		const char* name, uint32_t properties, uint32_t countergroup, 
		const char* unit ) {

	return OTF_WStream_writeDefCounterKV(wstream, deftoken, name, properties,
			countergroup, unit, NULL);
}


int OTF_WStream_writeDefCounterGroupKV( OTF_WStream* wstream, 
		uint32_t deftoken, const char* name, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

#	ifdef OTF_DEBUG
		if( 0 == deftoken ) {
		
			OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif
	

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFCOUNTERGROUP );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFCOUNTERGROUP " " );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NAME " " );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeDefCounterGroup( OTF_WStream* wstream, 
		uint32_t deftoken, const char* name ) {

	return OTF_WStream_writeDefCounterGroupKV(wstream, deftoken, name, NULL);
}


int OTF_WStream_writeDefSclKV( OTF_WStream* wstream, uint32_t deftoken, 
		uint32_t sclfile, uint32_t sclline, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

#	ifdef OTF_DEBUG
		if( 0 == deftoken ) {
		
			OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif
	

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFSCL );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_FILE );
		OTF_WBuffer_writeUint32( buffer, sclfile );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_LINE );
		OTF_WBuffer_writeUint32( buffer, sclline );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFSCL " " );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_FILE " " );
		OTF_WBuffer_writeUint32( buffer, sclfile );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_LINE " " );
		OTF_WBuffer_writeUint32( buffer, sclline );
		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeDefScl( OTF_WStream* wstream, uint32_t deftoken, 
		uint32_t sclfile, uint32_t sclline ) {

	return OTF_WStream_writeDefSclKV(wstream, deftoken, sclfile, sclline, NULL);
}


int OTF_WStream_writeDefSclFileKV( OTF_WStream* wstream, uint32_t deftoken, 
		const char* filename, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

#	ifdef OTF_DEBUG
		if( 0 == deftoken ) {
		
			OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif
	

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFSCLFILE );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME );
		OTF_WBuffer_writeString( buffer, filename );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFSCLFILE " " );

		OTF_WBuffer_writeUint32( buffer, deftoken );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NAME " " );
		OTF_WBuffer_writeString( buffer, filename );
		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeDefSclFile( OTF_WStream* wstream, uint32_t deftoken, 
		const char* filename ) {

	return OTF_WStream_writeDefSclFileKV(wstream, deftoken, filename, NULL);
}


int OTF_WStream_writeDefUniqueId( OTF_WStream* wstream, uint64_t uid ) {

	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFUNIQUEID );

		OTF_WBuffer_writeUint64( buffer, uid );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFUNIQUEID " " );

		OTF_WBuffer_writeUint64( buffer, uid );
		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeUniqueId( OTF_WStream* wstream ) {

	uint64_t uid;

	struct timeval tv;
	const uint64_t golden_ratio = 11400714819323198485ULL;
	static unsigned short rnd_state[3] = { 0, 0, 0 };

	/* generate an unique id */

	/* 0. initialize random state */
	if ( 0 == rnd_state[0] && 0 == rnd_state[1] && 0 == rnd_state[2] ) {

		rnd_state[0] = (unsigned short)getpid();
		rnd_state[1] = 1; rnd_state[2] = 2;
	}

	/* 1. get the current timestamp */
	if ( -1 == gettimeofday( &tv, NULL ) ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
			"gettimeofday() failed: %s\n",
			__FUNCTION__, __FILE__, __LINE__,
			strerror(errno) );

		return 0;
	}
	uid = (uint64_t)( ( tv.tv_sec * 1000000LL ) + tv.tv_usec );

	/* 2. multiply a pseudo-random integer */
	uid *= (uint64_t)nrand48( rnd_state );

	/* 3. multiply the golden ratio */
	uid *= golden_ratio;

	/* do actual write the unique-id record */
	return OTF_WStream_writeDefUniqueId( wstream, uid );
}


int OTF_WStream_writeDefVersion( OTF_WStream* wstream, uint8_t major,
		uint8_t minor, uint8_t sub, const char* string ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFVERSION );

		OTF_WBuffer_writeUint32( buffer, major );
		OTF_WBuffer_writeChar( buffer, '.' );
		OTF_WBuffer_writeUint32( buffer, minor );
		OTF_WBuffer_writeChar( buffer, '.' );
		OTF_WBuffer_writeUint32( buffer, sub );
		OTF_WBuffer_writeString( buffer, string );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFVERSION " " );

		OTF_WBuffer_writeUint32( buffer, major );
		OTF_WBuffer_writeChar( buffer, '.' );
		OTF_WBuffer_writeUint32( buffer, minor );
		OTF_WBuffer_writeChar( buffer, '.' );
		OTF_WBuffer_writeUint32( buffer, sub );
		OTF_WBuffer_writeString( buffer, string );
		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeOtfVersion( OTF_WStream* wstream ) {

	return OTF_WStream_writeDefVersion( wstream, OTF_VERSION_MAJOR,
		OTF_VERSION_MINOR, OTF_VERSION_SUB, OTF_VERSION_STRING );
}


int OTF_WStream_writeDefCreatorKV( OTF_WStream* wstream, const char* creator,
		OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFCREATOR );

		OTF_WBuffer_writeString( buffer, creator );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFCREATOR " " );

		OTF_WBuffer_writeString( buffer, creator );
		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeDefCreator( OTF_WStream* wstream, const char* creator ) {

	return OTF_WStream_writeDefCreatorKV(wstream, creator, NULL);
}


int OTF_WStream_writeDefFileKV( OTF_WStream* wstream, uint32_t token,
		const char* name, uint32_t group, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

#	ifdef OTF_DEBUG
		if( 0 == token ) {
		
			OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif
			
	
	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);
	
		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFFILE );

		OTF_WBuffer_writeUint32( buffer, token );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_GROUP );
		OTF_WBuffer_writeUint32( buffer, group );

		OTF_WBuffer_writeNewline( buffer );
	
	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFFILE " " );

		OTF_WBuffer_writeUint32( buffer, token );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NAME " " );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_GROUP " " );
		OTF_WBuffer_writeUint32( buffer, group );


		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeDefFile( OTF_WStream* wstream, uint32_t token,
		const char* name, uint32_t group ) {

	return OTF_WStream_writeDefFileKV(wstream, token, name, group, NULL);
}

int OTF_WStream_writeDefFileGroupKV( OTF_WStream* wstream, uint32_t token,
		const char* name, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

#	ifdef OTF_DEBUG
		if( 0 == token ) {
		
			OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif
			
	
	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);
	
		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFFILEGROUP );

		OTF_WBuffer_writeUint32( buffer, token );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME );
		OTF_WBuffer_writeString( buffer, name );

		OTF_WBuffer_writeNewline( buffer );
	
	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFFILEGROUP " " );

		OTF_WBuffer_writeUint32( buffer, token );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NAME " " );
		OTF_WBuffer_writeString( buffer, name );


		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeDefFileGroup( OTF_WStream* wstream, uint32_t token,
		const char* name ) {

	return OTF_WStream_writeDefFileGroupKV(wstream, token, name, NULL);
}


int OTF_WStream_writeDefKeyValueKV( OTF_WStream* wstream, uint32_t key, OTF_Type type,
		const char* name, const char *description,
		OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFKEYVALUE );

		OTF_WBuffer_writeUint32( buffer, key );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_TYPE );
		OTF_WBuffer_writeUint32( buffer, type );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_TAG );
		OTF_WBuffer_writeString( buffer, description );

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFKEYVALUE " " );

		OTF_WBuffer_writeUint32( buffer, key );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_TYPE " " );
		OTF_WBuffer_writeUint32( buffer, type );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NAME " " );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_TAG " " );
		OTF_WBuffer_writeString( buffer, description );

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeDefKeyValue( OTF_WStream* wstream,	uint32_t key,
	OTF_Type type, const char* name, const char *description ) {

	return OTF_WStream_writeDefKeyValueKV(wstream, key, type, name, description, NULL);
}


int OTF_WStream_writeDefTimeRange( OTF_WStream* wstream, uint64_t minTime,
		uint64_t maxTime, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	const char* recordkw= OTF_KEYWORD_S_DEF_PREFIX
			      OTF_KEYWORD_S_DEFTIMERANGE;
	const char* timekw=   OTF_KEYWORD_S_LOCAL_TIME;

	uint32_t ( *writeKeyValueList )( OTF_WBuffer*, OTF_KeyValueList* )=
		OTF_WBuffer_writeKeyValueList_short;

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		recordkw= OTF_KEYWORD_L_DEF_PREFIX
			  OTF_KEYWORD_L_DEFTIMERANGE " ";
		timekw=   " " OTF_KEYWORD_L_LOCAL_TIME " ";

		writeKeyValueList= OTF_WBuffer_writeKeyValueList_long;
	}

	writeKeyValueList( buffer, list );

	OTF_WBuffer_writeKeyword( buffer, recordkw );

	OTF_WBuffer_writeUint64( buffer, minTime );

	OTF_WBuffer_writeKeyword( buffer, timekw );

	OTF_WBuffer_writeUint64( buffer, maxTime );

	OTF_WBuffer_writeNewline( buffer );

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeDefCounterAssignments( OTF_WStream* wstream,
		uint32_t counter_token, uint32_t number_of_members,
		const uint32_t* procs_or_groups, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	uint32_t i;

	const char* recordkw=  OTF_KEYWORD_S_DEF_PREFIX
			       OTF_KEYWORD_S_DEFCOUNTERASSIGNMENTS;
	const char* memberskw= OTF_KEYWORD_S_LOCAL_MEMBERS;

	uint32_t ( *writeKeyValueList )( OTF_WBuffer*, OTF_KeyValueList* )=
		OTF_WBuffer_writeKeyValueList_short;

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		recordkw=  OTF_KEYWORD_L_DEF_PREFIX
			   OTF_KEYWORD_L_DEFCOUNTERASSIGNMENTS " ";
		memberskw= " " OTF_KEYWORD_L_LOCAL_MEMBERS " ";

		writeKeyValueList= OTF_WBuffer_writeKeyValueList_long;
	}

	writeKeyValueList( buffer, list );

	OTF_WBuffer_writeKeyword( buffer, recordkw );

	OTF_WBuffer_writeUint32( buffer, counter_token );

	OTF_WBuffer_writeKeyword( buffer, memberskw );
	for ( i = 0; i < number_of_members; ++i ) {

		OTF_WBuffer_writeUint32( buffer, procs_or_groups[i] );
		OTF_WBuffer_writeChar( buffer, ',' );
	}

	OTF_WBuffer_writeNewline( buffer );

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeDefProcessSubstitutes( OTF_WStream* wstream,
		uint32_t representative, uint32_t numberOfProcs,
		const uint32_t* procs, OTF_KeyValueList* list ) {


	unsigned int i;
	OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	
#	ifdef OTF_DEBUG
		if( 0 == representative ) {
		
			OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				"'0' is an invalid token.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		}
#	endif

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);
	
		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_DEF_PREFIX 
			OTF_KEYWORD_S_DEFPROCESSSUBSTITUTES );

		OTF_WBuffer_writeUint32( buffer, representative );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_MEMBERS );

		for ( i = 0; i < numberOfProcs; ++i ) {

			OTF_WBuffer_writeUint32( buffer, procs[i] );
			OTF_WBuffer_writeChar( buffer, ',' );
		}

		OTF_WBuffer_writeNewline( buffer );
	
	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_DEF_PREFIX 
			OTF_KEYWORD_L_DEFPROCESSSUBSTITUTES " " );

		OTF_WBuffer_writeUint32( buffer, representative );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_MEMBERS " " );

		for ( i = 0; i < numberOfProcs; ++i ) {

			OTF_WBuffer_writeUint32( buffer, procs[i] );
			OTF_WBuffer_writeChar( buffer, ',' );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeDefAuxSamplePoint( OTF_WStream*           wstream,
                                        uint64_t               time,
                                        OTF_AuxSamplePointType type,
                                        OTF_KeyValueList*      list ) {


    OTF_WBuffer* buffer= OTF_WStream_getDefBuffer( wstream );

    const char* recordkw= OTF_KEYWORD_S_DEF_PREFIX
                          OTF_KEYWORD_S_DEFAUXSAMPLEPOINT;
    const char* typekw=   OTF_KEYWORD_S_LOCAL_TYPE;

    uint32_t ( *writeKeyValueList )( OTF_WBuffer*, OTF_KeyValueList* )=
        OTF_WBuffer_writeKeyValueList_short;

    /* buffer can be NULL if file-open fails */
    if ( NULL == buffer ) {
        return 0;
    }

    if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {
        recordkw= OTF_KEYWORD_L_DEF_PREFIX
                  OTF_KEYWORD_L_DEFAUXSAMPLEPOINT " ";
        typekw=   " " OTF_KEYWORD_L_LOCAL_TYPE " ";

        writeKeyValueList= OTF_WBuffer_writeKeyValueList_long;
    }

    writeKeyValueList( buffer, list );

    OTF_WBuffer_writeKeyword( buffer, recordkw );
    OTF_WBuffer_writeUint64( buffer, time );

    OTF_WBuffer_writeKeyword( buffer, typekw );
    OTF_WBuffer_writeUint32( buffer, type );

    OTF_WBuffer_writeNewline( buffer );

    /* one or more of the last write operations could be failed;
    check otf_errno for errors */
    return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

/* *** event record write handlers *** ************************************* */

int OTF_WStream_writeNoOpKV( OTF_WStream* wstream, uint64_t time,
    uint32_t process, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_NOOP );

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_NOOP " " );

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeEnterKV( OTF_WStream* wstream, uint64_t time, 
		uint32_t statetoken, uint32_t cpuid, uint32_t scltoken,
		OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, cpuid ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_ENTER );

		OTF_WBuffer_writeUint32( buffer, statetoken );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_ENTER " " );

		OTF_WBuffer_writeUint32( buffer, statetoken );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeEnter( OTF_WStream* wstream, uint64_t time, 
		uint32_t statetoken, uint32_t cpuid, uint32_t scltoken ) {

	return OTF_WStream_writeEnterKV(wstream, time, statetoken, cpuid, scltoken, NULL);
}


int OTF_WStream_writeRecvMsgKV( OTF_WStream* wstream, uint64_t time, 
		uint32_t receiver, uint32_t sender, uint32_t communicator, 
		uint32_t msgtag, uint32_t msglength, uint32_t scltoken,
		OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, receiver ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_RECEIVE );

		OTF_WBuffer_writeUint32( buffer, sender );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_LENGTH );
		OTF_WBuffer_writeUint32( buffer, msglength );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_TAG );
		OTF_WBuffer_writeUint32( buffer, msgtag );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_COMMUNICATOR );
		OTF_WBuffer_writeUint32( buffer, communicator );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_RECEIVE " " );

		OTF_WBuffer_writeUint32( buffer, sender );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_LENGTH " " );
		OTF_WBuffer_writeUint32( buffer, msglength );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_TAG " " );
		OTF_WBuffer_writeUint32( buffer, msgtag );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_COMMUNICATOR " " );
		OTF_WBuffer_writeUint32( buffer, communicator );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeRecvMsg( OTF_WStream* wstream, uint64_t time, 
		uint32_t receiver, uint32_t sender, uint32_t communicator, 
		uint32_t msgtag, uint32_t msglength, uint32_t scltoken ) {


	return OTF_WStream_writeRecvMsgKV(wstream, time, receiver, sender, communicator,
			msgtag, msglength, scltoken, NULL);
}


int OTF_WStream_writeSendMsgKV( OTF_WStream* wstream, uint64_t time, uint32_t sender,
		uint32_t receiver, uint32_t communicator, uint32_t msgtag, 
		uint32_t msglength, uint32_t scltoken, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, sender ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_SEND );

		OTF_WBuffer_writeUint32( buffer, receiver );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_LENGTH );
		OTF_WBuffer_writeUint32( buffer, msglength );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_TAG );
		OTF_WBuffer_writeUint32( buffer, msgtag );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_COMMUNICATOR );
		OTF_WBuffer_writeUint32( buffer, communicator );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_SEND " " );

		OTF_WBuffer_writeUint32( buffer, receiver );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_LENGTH " " );
		OTF_WBuffer_writeUint32( buffer, msglength );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_TAG " " );
		OTF_WBuffer_writeUint32( buffer, msgtag );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_COMMUNICATOR " " );
		OTF_WBuffer_writeUint32( buffer, communicator );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeSendMsg( OTF_WStream* wstream, uint64_t time, uint32_t sender,
		uint32_t receiver, uint32_t communicator, uint32_t msgtag, 
		uint32_t msglength, uint32_t scltoken ) {

	return OTF_WStream_writeSendMsgKV(wstream, time, sender, receiver, communicator,
			msgtag, msglength, scltoken, NULL);
}


int OTF_WStream_writeLeaveKV( OTF_WStream* wstream, uint64_t time, 
		uint32_t statetoken, uint32_t cpuid, uint32_t scltoken,
		OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, cpuid ) ) return 0;


	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_LEAVE );

		if( 0 != statetoken || 0 != scltoken ) {
		
			OTF_WBuffer_writeUint32( buffer, statetoken );
		}

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_LEAVE " " );

		if( 0 != statetoken || 0 != scltoken ) {
		
			OTF_WBuffer_writeUint32( buffer, statetoken );
		}
		
		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeLeave( OTF_WStream* wstream, uint64_t time, 
		uint32_t statetoken, uint32_t cpuid, uint32_t scltoken ) {

	return OTF_WStream_writeLeaveKV(wstream, time, statetoken, cpuid, scltoken, NULL);
}


int OTF_WStream_writeCounterKV( OTF_WStream* wstream, uint64_t time, 
		uint32_t process, uint32_t counter_token, uint64_t value,
		OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_COUNTER );

		OTF_WBuffer_writeUint32( buffer, counter_token );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_VALUE );
		OTF_WBuffer_writeUint64( buffer, value );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_COUNTER " " );

		OTF_WBuffer_writeUint32( buffer, counter_token );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_VALUE " " );
		OTF_WBuffer_writeUint64( buffer, value );
		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeCounter( OTF_WStream* wstream, uint64_t time, 
		uint32_t process, uint32_t counter_token, uint64_t value ) {

	return OTF_WStream_writeCounterKV(wstream, time, process, counter_token, value, NULL);
}


int OTF_WStream_writeCollectiveOperationKV( OTF_WStream* wstream, uint64_t time, 
        uint32_t process, uint32_t functionToken, uint32_t communicator, 
	uint32_t rootprocess, uint32_t sent, uint32_t received, 
	uint64_t duration, uint32_t scltoken, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_COLLECTIVEOPERATION );

		OTF_WBuffer_writeUint32( buffer, functionToken );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_COMMUNICATOR );
		OTF_WBuffer_writeUint32( buffer, communicator );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_ROOT );
		OTF_WBuffer_writeUint32( buffer, rootprocess );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SENT );
		OTF_WBuffer_writeUint32( buffer, sent );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_RECVD );
		OTF_WBuffer_writeUint32( buffer, received );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_DURATION );
		OTF_WBuffer_writeUint64( buffer, duration );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_L_COLLECTIVEOPERATION " " );

		OTF_WBuffer_writeUint32( buffer, functionToken );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_COMMUNICATOR " " );
		OTF_WBuffer_writeUint32( buffer, communicator );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_ROOT " " );
		OTF_WBuffer_writeUint32( buffer, rootprocess );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SENT " " );
		OTF_WBuffer_writeUint32( buffer, sent );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_RECVD " " );
		OTF_WBuffer_writeUint32( buffer, received );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_DURATION " " );
		OTF_WBuffer_writeUint64( buffer, duration );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeCollectiveOperation( OTF_WStream* wstream, uint64_t time, 
        uint32_t process, uint32_t functionToken, uint32_t communicator, 
	uint32_t rootprocess, uint32_t sent, uint32_t received, 
	uint64_t duration, uint32_t scltoken ) {

	return OTF_WStream_writeCollectiveOperationKV(wstream, time, process, functionToken,
			communicator, rootprocess, sent, received, duration, scltoken, NULL);
}


int OTF_WStream_writeBeginCollectiveOperationKV( OTF_WStream* wstream,
		uint64_t time, uint32_t process, uint32_t collOp,
		uint64_t matchingId, uint32_t procGroup, uint32_t rootProc,
		uint64_t sent, uint64_t received, uint32_t scltoken,
		OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer = OTF_WStream_getEventBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer,
		                OTF_KEYWORD_S_BEGINCOLLECTIVEOPERATION );

		OTF_WBuffer_writeUint32( buffer, collOp );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_HANDLEID );
		OTF_WBuffer_writeUint64( buffer, matchingId );
		OTF_WBuffer_writeKeyword( buffer,
		                OTF_KEYWORD_S_LOCAL_COMMUNICATOR );
		OTF_WBuffer_writeUint32( buffer, procGroup );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_ROOT );
		OTF_WBuffer_writeUint32( buffer, rootProc );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SENT );
		OTF_WBuffer_writeUint64( buffer, sent );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_RECVD );
		OTF_WBuffer_writeUint64( buffer, received );

		if( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer,
			                OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer,
		                OTF_KEYWORD_L_BEGINCOLLECTIVEOPERATION " " );

		OTF_WBuffer_writeUint32( buffer, collOp );
		OTF_WBuffer_writeKeyword( buffer,
		                " " OTF_KEYWORD_L_LOCAL_HANDLEID " " );
		OTF_WBuffer_writeUint64( buffer, matchingId );
		OTF_WBuffer_writeKeyword( buffer,
		                " " OTF_KEYWORD_L_LOCAL_COMMUNICATOR " " );
		OTF_WBuffer_writeUint32( buffer, procGroup );
		OTF_WBuffer_writeKeyword( buffer,
		                " " OTF_KEYWORD_L_LOCAL_ROOT " " );
		OTF_WBuffer_writeUint32( buffer, rootProc );
		OTF_WBuffer_writeKeyword( buffer,
		                " " OTF_KEYWORD_L_LOCAL_SENT " " );
		OTF_WBuffer_writeUint64( buffer, sent );
		OTF_WBuffer_writeKeyword( buffer,
		                " " OTF_KEYWORD_L_LOCAL_RECVD " " );
		OTF_WBuffer_writeUint64( buffer, received );

		if( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer,
			                " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeBeginCollectiveOperation( OTF_WStream* wstream,
		uint64_t time, uint32_t process, uint32_t collOp,
		uint64_t matchingId, uint32_t procGroup, uint32_t rootProc,
		uint64_t sent, uint64_t received, uint32_t scltoken ) {

	return OTF_WStream_writeBeginCollectiveOperationKV(wstream, time, process,
			collOp, matchingId, procGroup, rootProc, sent, received,
			scltoken, NULL);
}


int OTF_WStream_writeEndCollectiveOperationKV( OTF_WStream* wstream,
                uint64_t time, uint32_t process, uint64_t matchingId,
		OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer = OTF_WStream_getEventBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer,
		                OTF_KEYWORD_S_ENDCOLLECTIVEOPERATION );

		OTF_WBuffer_writeUint64( buffer, matchingId );

		OTF_WBuffer_writeNewline( buffer );

	} else if( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer,
		                OTF_KEYWORD_L_ENDCOLLECTIVEOPERATION " " );

		OTF_WBuffer_writeUint64( buffer, matchingId );

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeEndCollectiveOperation( OTF_WStream* wstream,
                uint64_t time, uint32_t process, uint64_t matchingId ) {

	return OTF_WStream_writeEndCollectiveOperationKV(wstream, time, process, matchingId, NULL);
}


int OTF_WStream_writeEventCommentKV( OTF_WStream* wstream, uint64_t time, 
		uint32_t process, const char* comment, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_EVENTCOMMENT );

		OTF_WBuffer_writeString( buffer, comment );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_EVENTCOMMENT " " );

		OTF_WBuffer_writeString( buffer, comment );
		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeEventComment( OTF_WStream* wstream, uint64_t time, 
		uint32_t process, const char* comment ) {

	return OTF_WStream_writeEventCommentKV(wstream, time, process, comment, NULL);
}


int OTF_WStream_writeBeginProcessKV( OTF_WStream* wstream, uint64_t time,
    	uint32_t process, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_BEGINPROCESS );
			
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_BEGINPROCESS " " );
			
		OTF_WBuffer_writeNewline( buffer );

	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeBeginProcess( OTF_WStream* wstream, uint64_t time,
    	uint32_t process ) {

	return OTF_WStream_writeBeginProcessKV(wstream, time, process, NULL);
}


int OTF_WStream_writeEndProcessKV( OTF_WStream* wstream, uint64_t time,
    	uint32_t process, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_ENDPROCESS );
			
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_ENDPROCESS " " );
			
		OTF_WBuffer_writeNewline( buffer );

	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeEndProcess( OTF_WStream* wstream, uint64_t time,
    	uint32_t process ) {

	return OTF_WStream_writeEndProcessKV(wstream, time, process, NULL);
}


int OTF_WStream_writeFileOperationKV( OTF_WStream* wstream, uint64_t time,
	uint32_t fileid, uint32_t process, uint64_t handleid, uint32_t operation,
	uint64_t bytes, uint64_t duration, uint32_t source, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_FILEOPERATION );

		
    	OTF_WBuffer_writeUint32( buffer, fileid );
    	OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_HANDLEID );
    	OTF_WBuffer_writeUint64( buffer, handleid );
    	OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_OPERATION );
    	OTF_WBuffer_writeUint32( buffer, operation );
    	OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_BYTES );
    	OTF_WBuffer_writeUint64( buffer, bytes );
    	OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_DURATION );
    	OTF_WBuffer_writeUint64( buffer, duration );

    	if( 0 != source ) {
    		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
    		OTF_WBuffer_writeUint32( buffer, source );
    	}


		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_FILEOPERATION " " );


    	OTF_WBuffer_writeUint32( buffer, fileid );
    	OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_HANDLEID " " );
    	OTF_WBuffer_writeUint64( buffer, handleid );
    	OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_OPERATION " " );
    	OTF_WBuffer_writeUint32( buffer, operation );
    	OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_BYTES " " );
    	OTF_WBuffer_writeUint64( buffer, bytes );
    	OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_DURATION " " );
    	OTF_WBuffer_writeUint64( buffer, duration );

    	if( 0 != source ) {
    		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
    		OTF_WBuffer_writeUint32( buffer, source );
    	}
		
			
		OTF_WBuffer_writeNewline( buffer );

	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeFileOperation( OTF_WStream* wstream, uint64_t time,
	uint32_t fileid, uint32_t process, uint64_t handleid, uint32_t operation,
	uint64_t bytes, uint64_t duration, uint32_t source ) {

	return OTF_WStream_writeFileOperationKV(wstream, time, fileid, process,
			handleid, operation, bytes, duration, source, NULL);
}


int OTF_WStream_writeBeginFileOperationKV( OTF_WStream* wstream, uint64_t time,
        uint32_t process, uint64_t matchingId, uint32_t scltoken, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer = OTF_WStream_getEventBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer,
		                OTF_KEYWORD_S_BEGINFILEOPERATION_NEW );

		OTF_WBuffer_writeUint64( buffer, matchingId );

		if( 0 != scltoken ) {
			OTF_WBuffer_writeKeyword( buffer,
			                OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer,
		                OTF_KEYWORD_L_BEGINFILEOPERATION_NEW " " );

		OTF_WBuffer_writeUint64( buffer, matchingId );

		if( 0 != scltoken ) {
			OTF_WBuffer_writeKeyword( buffer,
			                " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );

	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeBeginFileOperation( OTF_WStream* wstream, uint64_t time,
        uint32_t process, uint64_t matchingId, uint32_t scltoken ) {

	return OTF_WStream_writeBeginFileOperationKV(wstream, time, process,
            matchingId, scltoken, NULL);
}


int OTF_WStream_writeEndFileOperationKV( OTF_WStream* wstream, uint64_t time,
                uint32_t process, uint32_t fileid, uint64_t matchingId,
                uint64_t handleId, uint32_t operation, uint64_t bytes,
                uint32_t scltoken, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer = OTF_WStream_getEventBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_ENDFILEOPERATION_NEW );

		OTF_WBuffer_writeUint32( buffer, fileid );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_MATCHID );
		OTF_WBuffer_writeUint64( buffer, matchingId );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_HANDLEID );
		OTF_WBuffer_writeUint64( buffer, handleId );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_OPERATION );
		OTF_WBuffer_writeUint32( buffer, operation );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_BYTES );
		OTF_WBuffer_writeUint64( buffer, bytes );

		if( 0 != scltoken ) {
			OTF_WBuffer_writeKeyword( buffer,
			                OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer,
		                OTF_KEYWORD_L_ENDFILEOPERATION_NEW " " );

		OTF_WBuffer_writeUint32( buffer, fileid );
		OTF_WBuffer_writeKeyword( buffer,
		                " " OTF_KEYWORD_L_LOCAL_MATCHID " " );
		OTF_WBuffer_writeUint64( buffer, matchingId );
		OTF_WBuffer_writeKeyword( buffer,
		                " " OTF_KEYWORD_L_LOCAL_HANDLEID " " );
		OTF_WBuffer_writeUint64( buffer, handleId );
		OTF_WBuffer_writeKeyword( buffer,
		                " " OTF_KEYWORD_L_LOCAL_OPERATION " " );
		OTF_WBuffer_writeUint32( buffer, operation );
		OTF_WBuffer_writeKeyword( buffer,
		                " " OTF_KEYWORD_L_LOCAL_BYTES " " );
		OTF_WBuffer_writeUint64( buffer, bytes );

		if( 0 != scltoken ) {
			OTF_WBuffer_writeKeyword( buffer,
			                " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );

	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeEndFileOperation( OTF_WStream* wstream, uint64_t time,
                uint32_t process, uint32_t fileid, uint64_t matchingId,
                uint64_t handleId, uint32_t operation, uint64_t bytes,
                uint32_t scltoken ) {

	return OTF_WStream_writeEndFileOperationKV(wstream, time, process, fileid,
			matchingId, handleId, operation, bytes, scltoken, NULL);
}


int OTF_WStream_writeRMAPutKV( OTF_WStream* wstream, uint64_t time,
        uint32_t process, uint32_t origin, uint32_t target,
        uint32_t communicator, uint32_t tag, uint64_t bytes,
        uint32_t scltoken, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_RMAPUT );

		OTF_WBuffer_writeUint32( buffer, origin );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_PROCESS );
		OTF_WBuffer_writeUint32( buffer, target );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_COMMUNICATOR );
		OTF_WBuffer_writeUint32( buffer, communicator );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_TAG );
		OTF_WBuffer_writeUint32( buffer, tag );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_LENGTH );
		OTF_WBuffer_writeUint64( buffer, bytes );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_L_RMAPUT " " );

		OTF_WBuffer_writeUint32( buffer, origin );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_PROCESS " " );
		OTF_WBuffer_writeUint32( buffer, target );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_COMMUNICATOR " " );
		OTF_WBuffer_writeUint32( buffer, communicator );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_TAG " " );
		OTF_WBuffer_writeUint32( buffer, tag );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_LENGTH " " );
		OTF_WBuffer_writeUint64( buffer, bytes );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeRMAPut( OTF_WStream* wstream, uint64_t time,
        uint32_t process, uint32_t origin, uint32_t target,
        uint32_t communicator, uint32_t tag, uint64_t bytes,
        uint32_t scltoken ) {

	return OTF_WStream_writeRMAPutKV(wstream, time, process, origin, target,
			communicator, tag, bytes, scltoken, NULL);
}


int OTF_WStream_writeRMAPutRemoteEndKV( OTF_WStream* wstream, uint64_t time,
        uint32_t process, uint32_t origin, uint32_t target,
        uint32_t communicator, uint32_t tag, uint64_t bytes,
        uint32_t scltoken, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_RMAPUTRE );

		OTF_WBuffer_writeUint32( buffer, origin );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_PROCESS );
		OTF_WBuffer_writeUint32( buffer, target );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_COMMUNICATOR );
		OTF_WBuffer_writeUint32( buffer, communicator );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_TAG );
		OTF_WBuffer_writeUint32( buffer, tag );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_LENGTH );
		OTF_WBuffer_writeUint64( buffer, bytes );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_L_RMAPUTRE " " );

		OTF_WBuffer_writeUint32( buffer, origin );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_PROCESS " " );
		OTF_WBuffer_writeUint32( buffer, target );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_COMMUNICATOR " " );
		OTF_WBuffer_writeUint32( buffer, communicator );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_TAG " " );
		OTF_WBuffer_writeUint32( buffer, tag );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_LENGTH " " );
		OTF_WBuffer_writeUint64( buffer, bytes );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeRMAPutRemoteEnd( OTF_WStream* wstream, uint64_t time,
        uint32_t process, uint32_t origin, uint32_t target,
        uint32_t communicator, uint32_t tag, uint64_t bytes,
        uint32_t scltoken ) {

	return OTF_WStream_writeRMAPutRemoteEndKV(wstream, time, process, origin,
			target, communicator, tag, bytes, scltoken, NULL);
}


int OTF_WStream_writeRMAGetKV( OTF_WStream* wstream, uint64_t time,
        uint32_t process, uint32_t origin, uint32_t target,
        uint32_t communicator, uint32_t tag, uint64_t bytes,
        uint32_t scltoken, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_RMAGET );

		OTF_WBuffer_writeUint32( buffer, origin );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_PROCESS );
		OTF_WBuffer_writeUint32( buffer, target );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_COMMUNICATOR );
		OTF_WBuffer_writeUint32( buffer, communicator );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_TAG );
		OTF_WBuffer_writeUint32( buffer, tag );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_LENGTH );
		OTF_WBuffer_writeUint64( buffer, bytes );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_L_RMAGET " " );

		OTF_WBuffer_writeUint32( buffer, origin );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_PROCESS " " );
		OTF_WBuffer_writeUint32( buffer, target );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_COMMUNICATOR " " );
		OTF_WBuffer_writeUint32( buffer, communicator );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_TAG " " );
		OTF_WBuffer_writeUint32( buffer, tag );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_LENGTH " " );
		OTF_WBuffer_writeUint64( buffer, bytes );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeRMAGet( OTF_WStream* wstream, uint64_t time,
        uint32_t process, uint32_t origin, uint32_t target,
        uint32_t communicator, uint32_t tag, uint64_t bytes,
        uint32_t scltoken ) {

	return OTF_WStream_writeRMAGetKV(wstream, time, process, origin, target,
			communicator, tag, bytes, scltoken, NULL);
}


int OTF_WStream_writeRMAEndKV( OTF_WStream* wstream, uint64_t time,
        uint32_t process, uint32_t remote, uint32_t communicator, uint32_t tag,
        uint32_t scltoken, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getEventBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_RMAEND );

		OTF_WBuffer_writeUint32( buffer, remote );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_COMMUNICATOR );
		OTF_WBuffer_writeUint32( buffer, communicator );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_TAG );
		OTF_WBuffer_writeUint32( buffer, tag );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_L_RMAEND " " );

		OTF_WBuffer_writeUint32( buffer, remote );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_COMMUNICATOR " " );
		OTF_WBuffer_writeUint32( buffer, communicator );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_TAG " " );
		OTF_WBuffer_writeUint32( buffer, tag );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeRMAEnd( OTF_WStream* wstream, uint64_t time,
        uint32_t process, uint32_t remote, uint32_t communicator, uint32_t tag,
        uint32_t scltoken ) {

	return OTF_WStream_writeRMAEndKV(wstream, time, process, remote,
			communicator, tag, scltoken, NULL);
}


/* *** public snapshot record write handlers *** */


int OTF_WStream_writeSnapshotCommentKV( OTF_WStream* wstream, uint64_t time, 
		uint32_t process, const char* comment, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getSnapshotBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_SNAPSHOT_PREFIX OTF_KEYWORD_S_SNAPSHOT_COMMENT );

		OTF_WBuffer_writeString( buffer, comment );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_SNAPSHOT_PREFIX OTF_KEYWORD_L_SNAPSHOT_COMMENT " " );

		OTF_WBuffer_writeString( buffer, comment );
		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeSnapshotComment( OTF_WStream* wstream, uint64_t time, 
		uint32_t process, const char* comment ) {

	return OTF_WStream_writeSnapshotCommentKV(wstream, time, process, comment, NULL);
}


int OTF_WStream_writeEnterSnapshotKV( OTF_WStream* wstream, uint64_t time,
	    	uint64_t originaltime, uint32_t statetoken, uint32_t cpuid,
		uint32_t scltoken, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getSnapshotBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, cpuid ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_SNAPSHOT_PREFIX OTF_KEYWORD_S_SNAPSHOT_ENTER );

		OTF_WBuffer_writeUint32( buffer, statetoken );

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_OTIME );
		OTF_WBuffer_writeUint64( buffer, originaltime );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_SNAPSHOT_PREFIX OTF_KEYWORD_L_SNAPSHOT_ENTER " " );

		OTF_WBuffer_writeUint32( buffer, statetoken );

		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_OTIME " " );
		OTF_WBuffer_writeUint64( buffer, originaltime );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeEnterSnapshot( OTF_WStream* wstream, uint64_t time,
	    uint64_t originaltime, uint32_t statetoken, uint32_t cpuid, uint32_t scltoken ) {

	return OTF_WStream_writeEnterSnapshotKV(wstream, time, originaltime,
			statetoken, cpuid, scltoken, NULL);
}


int OTF_WStream_writeSendSnapshotKV( OTF_WStream* wstream, uint64_t time,
		uint64_t originaltime, uint32_t sender, uint32_t receiver,
		uint32_t procGroup, uint32_t tag, uint32_t length, uint32_t source,
		OTF_KeyValueList* list ) {
	
	
	OTF_WBuffer* buffer= OTF_WStream_getSnapshotBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, sender ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_SNAPSHOT_PREFIX OTF_KEYWORD_S_SNAPSHOT_SEND );

		OTF_WBuffer_writeUint32( buffer, receiver );

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_OTIME );
		OTF_WBuffer_writeUint64( buffer, originaltime );
		
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_GROUP );
		OTF_WBuffer_writeUint32( buffer, procGroup );
		
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_TAG );
		OTF_WBuffer_writeUint32( buffer, tag );

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_LENGTH );
		OTF_WBuffer_writeUint32( buffer, length );
		
		if ( 0 != source ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, source );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_SNAPSHOT_PREFIX OTF_KEYWORD_L_SNAPSHOT_SEND " " );

		OTF_WBuffer_writeUint32( buffer, receiver );

		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_OTIME " " );
		OTF_WBuffer_writeUint64( buffer, originaltime );
		
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_GROUP " " );
		OTF_WBuffer_writeUint32( buffer, procGroup );
		
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_TAG " " );
		OTF_WBuffer_writeUint32( buffer, tag );

		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_LENGTH " " );
		OTF_WBuffer_writeUint32( buffer, length );
		
		if ( 0 != source ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, source );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeSendSnapshot( OTF_WStream* wstream, uint64_t time,
		uint64_t originaltime, uint32_t sender, uint32_t receiver,
		uint32_t procGroup, uint32_t tag, uint32_t length, uint32_t source ) {

	return OTF_WStream_writeSendSnapshotKV(wstream, time, originaltime, sender,
			receiver, procGroup, tag, length, source, NULL);
}


int OTF_WStream_writeOpenFileSnapshotKV( OTF_WStream* wstream,uint64_t time,
	uint64_t originaltime, uint32_t fileid, uint32_t process, uint64_t handleid,
	uint32_t source, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getSnapshotBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_SNAPSHOT_PREFIX OTF_KEYWORD_S_SNAPSHOT_OPENFILE );

		OTF_WBuffer_writeUint32( buffer, fileid );

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_OTIME );
		OTF_WBuffer_writeUint64( buffer, originaltime );

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_HANDLEID );
		OTF_WBuffer_writeUint64( buffer, handleid );

		if ( 0 != source ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, source );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_SNAPSHOT_PREFIX OTF_KEYWORD_L_SNAPSHOT_OPENFILE " " );

		OTF_WBuffer_writeUint32( buffer, fileid );

		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_OTIME " " );
		OTF_WBuffer_writeUint64( buffer, originaltime );

		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_HANDLEID " " );
		OTF_WBuffer_writeUint64( buffer, handleid );

		if ( 0 != source ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, source );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeOpenFileSnapshot( OTF_WStream* wstream,uint64_t time,
	uint64_t originaltime, uint32_t fileid, uint32_t process, uint64_t handleid,
	uint32_t source ) {

	return OTF_WStream_writeOpenFileSnapshotKV(wstream, time, originaltime, fileid,
			process, handleid, source, NULL);
}

int OTF_WStream_writeBeginCollopSnapshotKV( OTF_WStream* wstream, uint64_t time,
    uint64_t originaltime, uint32_t process, uint32_t collOp, uint64_t matchingId,
	uint32_t procGroup, uint32_t rootProc, uint64_t sent, uint64_t received,
    uint32_t scltoken, OTF_KeyValueList *list ) {


	OTF_WBuffer* buffer= OTF_WStream_getSnapshotBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer,
			OTF_KEYWORD_S_SNAPSHOT_PREFIX OTF_KEYWORD_S_SNAPSHOT_BEGINCOLLOP );

		OTF_WBuffer_writeUint64( buffer, originaltime );

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_COLLECTIVE );
		OTF_WBuffer_writeUint32( buffer, collOp );

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_HANDLEID );
		OTF_WBuffer_writeUint64( buffer, matchingId );

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_GROUP );
		OTF_WBuffer_writeUint32( buffer, procGroup );

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_ROOT );
		OTF_WBuffer_writeUint32( buffer, rootProc );

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SENT );
		OTF_WBuffer_writeUint64( buffer, sent );

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_RECVD );
		OTF_WBuffer_writeUint64( buffer, received );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_SNAPSHOT_PREFIX OTF_KEYWORD_L_SNAPSHOT_BEGINCOLLOP " " );

		OTF_WBuffer_writeUint64( buffer, originaltime );

		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_COLLECTIVE " " );
		OTF_WBuffer_writeUint32( buffer, collOp );

		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_HANDLEID " " );
		OTF_WBuffer_writeUint64( buffer, matchingId );

		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_GROUP " " );
		OTF_WBuffer_writeUint32( buffer, procGroup );

		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_ROOT " " );
		OTF_WBuffer_writeUint32( buffer, rootProc );

		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SENT " " );
		OTF_WBuffer_writeUint64( buffer, sent );

		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_RECVD " " );
		OTF_WBuffer_writeUint64( buffer, received );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeBeginCollopSnapshot( OTF_WStream* wstream, uint64_t time,
    uint64_t originaltime, uint32_t process, uint32_t collOp, uint64_t matchingId,
	uint32_t procGroup, uint32_t rootProc, uint64_t sent, uint64_t received,
    uint32_t scltoken ) {

	return OTF_WStream_writeBeginCollopSnapshotKV(wstream, time, originaltime,
			process, collOp, matchingId, procGroup, rootProc, sent,
			received, scltoken, NULL );
}

int OTF_WStream_writeBeginFileOpSnapshotKV( OTF_WStream* wstream, uint64_t time,
    uint64_t originaltime, uint32_t process, uint64_t matchingId,
    uint32_t scltoken, OTF_KeyValueList *list ) {


	OTF_WBuffer* buffer= OTF_WStream_getSnapshotBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer,
			OTF_KEYWORD_S_SNAPSHOT_PREFIX OTF_KEYWORD_S_SNAPSHOT_BEGINFILEOP );

		OTF_WBuffer_writeUint64( buffer, originaltime );

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_MATCHID );
		OTF_WBuffer_writeUint64( buffer, matchingId );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer,
			OTF_KEYWORD_L_SNAPSHOT_PREFIX OTF_KEYWORD_L_SNAPSHOT_BEGINFILEOP " " );

		OTF_WBuffer_writeUint64( buffer, originaltime );

		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_MATCHID " " );
		OTF_WBuffer_writeUint64( buffer, matchingId );

		if ( 0 != scltoken ) {

			OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SCL " " );
			OTF_WBuffer_writeUint32( buffer, scltoken );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeBeginFileOpSnapshot( OTF_WStream* wstream, uint64_t time,
     uint64_t originaltime, uint32_t process, uint64_t matchingId, uint32_t scltoken ) {


	return OTF_WStream_writeBeginFileOpSnapshotKV( wstream, time, originaltime,
			process, matchingId, scltoken, NULL);

}


int OTF_WStream_writeCollopCountSnapshot( OTF_WStream* wstream,
                                          uint64_t time,
                                          uint32_t process,
                                          uint32_t communicator,
                                          uint64_t count,
                                          OTF_KeyValueList* list ) {


    OTF_WBuffer* buffer= OTF_WStream_getSnapshotBuffer( wstream );

    const char* recordkw= OTF_KEYWORD_S_SNAPSHOT_PREFIX
                          OTF_KEYWORD_S_SNAPSHOT_COLLOPCOUNT;
    const char* countkw=  OTF_KEYWORD_S_LOCAL_COUNT;

    uint32_t ( *writeKeyValueList )( OTF_WBuffer*, OTF_KeyValueList* )=
        OTF_WBuffer_writeKeyValueList_short;

    /* buffer can be NULL if file-open fails */
    if ( NULL == buffer ) {
        return 0;
    }

    if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) {
        return 0;
    }

    if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {
        recordkw= OTF_KEYWORD_L_SNAPSHOT_PREFIX
                  OTF_KEYWORD_L_SNAPSHOT_COLLOPCOUNT " ";
        countkw=  " " OTF_KEYWORD_L_LOCAL_COUNT " ";

        writeKeyValueList = OTF_WBuffer_writeKeyValueList_long;
    }

    writeKeyValueList( buffer, list );

    OTF_WBuffer_writeKeyword( buffer, recordkw );
    OTF_WBuffer_writeUint32( buffer, communicator );

    OTF_WBuffer_writeKeyword( buffer, countkw );
    OTF_WBuffer_writeUint64( buffer, count );

    OTF_WBuffer_writeNewline( buffer );

    /* one or more of the last write operations could be failed;
    check otf_errno for errors */
    return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeCounterSnapshot( OTF_WStream*      wstream,
                                      uint64_t          time,
                                      uint64_t          originaltime,
                                      uint32_t          process,
                                      uint32_t          counter,
                                      uint64_t          value,
                                      OTF_KeyValueList* list ) {


    OTF_WBuffer* buffer = OTF_WStream_getSnapshotBuffer( wstream );

    const char* recordkw  = OTF_KEYWORD_S_SNAPSHOT_PREFIX
                            OTF_KEYWORD_S_SNAPSHOT_COUNTER;
    const char* counterkw = OTF_KEYWORD_S_LOCAL_COUNTER;
    const char* valuekw   = OTF_KEYWORD_S_LOCAL_VALUE;

    uint32_t ( *writeKeyValueList )( OTF_WBuffer*, OTF_KeyValueList* )
        = OTF_WBuffer_writeKeyValueList_short;

    /* buffer can be NULL if file-open fails */
    if ( NULL == buffer ) {
        return 0;
    }

    if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) {
        return 0;
    }

    if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {
        recordkw  = OTF_KEYWORD_L_SNAPSHOT_PREFIX
                    OTF_KEYWORD_L_SNAPSHOT_COUNTER  " ";
        counterkw = " " OTF_KEYWORD_L_LOCAL_COUNTER " ";
        valuekw   = " " OTF_KEYWORD_L_LOCAL_VALUE   " ";

        writeKeyValueList = OTF_WBuffer_writeKeyValueList_long;
    }

    writeKeyValueList( buffer, list );

    OTF_WBuffer_writeKeyword( buffer, recordkw );

    OTF_WBuffer_writeUint64( buffer, originaltime );

    OTF_WBuffer_writeKeyword( buffer, counterkw );
    OTF_WBuffer_writeUint32( buffer, counter );

    OTF_WBuffer_writeKeyword( buffer, valuekw );
    OTF_WBuffer_writeUint64( buffer, value );

    OTF_WBuffer_writeNewline( buffer );

    /* one or more of the last write operations could be failed;
    check otf_errno for errors */
    return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}


/* *** public summary record write handlers *** */


int OTF_WStream_writeSummaryCommentKV( OTF_WStream* wstream, uint64_t time, 
		uint32_t process, const char* comment, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getStatsBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_S_SUM_PREFIX OTF_KEYWORD_S_SUMCOMMENT );

		OTF_WBuffer_writeString( buffer, comment );
		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, 
			OTF_KEYWORD_L_SUM_PREFIX OTF_KEYWORD_L_SUMCOMMENT " " );

		OTF_WBuffer_writeString( buffer, comment );
		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeSummaryComment( OTF_WStream* wstream, uint64_t time, 
		uint32_t process, const char* comment ) {

	return OTF_WStream_writeSummaryCommentKV(wstream, time, process, comment, NULL);
}


int OTF_WStream_writeFunctionSummaryKV( OTF_WStream* wstream, 
		uint64_t time, uint32_t function, uint32_t process, 
		uint64_t count, uint64_t excltime, uint64_t incltime,
		OTF_KeyValueList* list) {


	OTF_WBuffer* buffer= OTF_WStream_getStatsBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_SUM_PREFIX
			OTF_KEYWORD_S_SUMFUNCTION );

		OTF_WBuffer_writeUint32( buffer, function );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_COUNT );
		OTF_WBuffer_writeUint64( buffer, count );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_EXCLTIME );
		OTF_WBuffer_writeUint64( buffer, excltime );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_INCLTIME );
		OTF_WBuffer_writeUint64( buffer, incltime );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_L_SUM_PREFIX
			OTF_KEYWORD_L_SUMFUNCTION " " );

		OTF_WBuffer_writeUint32( buffer, function );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_COUNT " " );
		OTF_WBuffer_writeUint64( buffer, count );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_EXCLTIME " " );
		OTF_WBuffer_writeUint64( buffer, excltime );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_INCLTIME " " );
		OTF_WBuffer_writeUint64( buffer, incltime );
	}

	OTF_WBuffer_writeNewline( buffer );

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeFunctionSummary( OTF_WStream* wstream, 
		uint64_t time, uint32_t function, uint32_t process, 
		uint64_t count, uint64_t excltime, uint64_t incltime ) {

	return OTF_WStream_writeFunctionSummaryKV(wstream, time, function, process,
			count, excltime, incltime, NULL);
}


int OTF_WStream_writeFunctionGroupSummaryKV( OTF_WStream* wstream, 
		uint64_t time,  uint32_t functiongroup,  uint32_t process,  
		uint64_t count,  uint64_t excltime,  uint64_t incltime,
		OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getStatsBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_SUM_PREFIX
			OTF_KEYWORD_S_SUMFUNCTIONGROUP );
	
		OTF_WBuffer_writeUint32( buffer, functiongroup );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_COUNT );
		OTF_WBuffer_writeUint64( buffer, count );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_EXCLTIME );
		OTF_WBuffer_writeUint64( buffer, excltime );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_INCLTIME );
		OTF_WBuffer_writeUint64( buffer, incltime );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_L_SUM_PREFIX
			OTF_KEYWORD_L_SUMFUNCTIONGROUP " " );

		OTF_WBuffer_writeUint32( buffer, functiongroup );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_COUNT " " );
		OTF_WBuffer_writeUint64( buffer, count );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_EXCLTIME " " );
		OTF_WBuffer_writeUint64( buffer, excltime );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_INCLTIME " " );
		OTF_WBuffer_writeUint64( buffer, incltime );
	}

	OTF_WBuffer_writeNewline( buffer );

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeFunctionGroupSummary( OTF_WStream* wstream, 
		uint64_t time,  uint32_t functiongroup,  uint32_t process,  
		uint64_t count,  uint64_t excltime,  uint64_t incltime ) {

	return OTF_WStream_writeFunctionGroupSummaryKV(wstream, time, functiongroup,
			process, count, excltime, incltime, NULL);
}


int OTF_WStream_writeMessageSummaryKV( OTF_WStream* wstream, 
		uint64_t time, uint32_t process, uint32_t peer, 
		uint32_t comm, uint32_t tag, uint64_t number_sent,
		uint64_t number_recvd, uint64_t bytes_sent, 
		uint64_t bytes_recved, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getStatsBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_SUM_PREFIX
			OTF_KEYWORD_S_SUMMESSAGE );

		OTF_WBuffer_writeUint32( buffer, peer );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_COMMUNICATOR );
		OTF_WBuffer_writeUint32( buffer, comm );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_TAG );
		OTF_WBuffer_writeUint32( buffer, tag );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERSENT );
		OTF_WBuffer_writeUint64( buffer, number_sent );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERRECVD );
		OTF_WBuffer_writeUint64( buffer, number_recvd );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SENT );
		OTF_WBuffer_writeUint64( buffer, bytes_sent );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_RECVD );
		OTF_WBuffer_writeUint64( buffer, bytes_recved );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_L_SUM_PREFIX
			OTF_KEYWORD_L_SUMMESSAGE " " );

		OTF_WBuffer_writeUint32( buffer, peer );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_COMMUNICATOR " " );
		OTF_WBuffer_writeUint32( buffer, comm );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_TAG " " );
		OTF_WBuffer_writeUint32( buffer, tag );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBERSENT " " );
		OTF_WBuffer_writeUint64( buffer, number_sent );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBERRECVD " " );
		OTF_WBuffer_writeUint64( buffer, number_recvd );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SENT " " );
		OTF_WBuffer_writeUint64( buffer, bytes_sent );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_RECVD " " );
		OTF_WBuffer_writeUint64( buffer, bytes_recved );
	}

	OTF_WBuffer_writeNewline( buffer );

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeMessageSummary( OTF_WStream* wstream, 
		uint64_t time, uint32_t process, uint32_t peer, 
		uint32_t comm, uint32_t tag, uint64_t number_sent,
		uint64_t number_recvd, uint64_t bytes_sent, 
		uint64_t bytes_recved ) {

	return OTF_WStream_writeMessageSummaryKV(wstream, time, process, peer, comm,
			tag, number_sent, number_recvd, bytes_sent, bytes_recved, NULL);
}


int OTF_WStream_writeCollopSummaryKV( OTF_WStream* wstream, 
		uint64_t time, uint32_t process, uint32_t comm, uint32_t collective,
		uint64_t number_sent, uint64_t number_recved, uint64_t bytes_sent,
		uint64_t bytes_recved, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getStatsBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_SUM_PREFIX
			OTF_KEYWORD_S_COLLOPMESSAGE );

		OTF_WBuffer_writeUint32( buffer, comm );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_COLLECTIVE );
		OTF_WBuffer_writeUint32( buffer, collective );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERSENT );
		OTF_WBuffer_writeUint64( buffer, number_sent );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERRECVD );
		OTF_WBuffer_writeUint64( buffer, number_recved );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_SENT );
		OTF_WBuffer_writeUint64( buffer, bytes_sent );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_RECVD );
		OTF_WBuffer_writeUint64( buffer, bytes_recved );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_L_SUM_PREFIX
			OTF_KEYWORD_L_COLLOPMESSAGE " " );

		OTF_WBuffer_writeUint32( buffer, comm );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_COLLECTIVE " " );
		OTF_WBuffer_writeUint32( buffer, collective );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBERSENT " " );
		OTF_WBuffer_writeUint64( buffer, number_sent );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBERRECVD " " );
		OTF_WBuffer_writeUint64( buffer, number_recved );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_SENT " " );
		OTF_WBuffer_writeUint64( buffer, bytes_sent );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_RECVD " " );
		OTF_WBuffer_writeUint64( buffer, bytes_recved );
	}

	OTF_WBuffer_writeNewline( buffer );

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeCollopSummary( OTF_WStream* wstream, 
		uint64_t time, uint32_t process, uint32_t comm, uint32_t collective,
		uint64_t number_sent, uint64_t number_recved, uint64_t bytes_sent,
		uint64_t bytes_recved ) {

	return OTF_WStream_writeCollopSummaryKV(wstream, time, process, comm, collective,
			number_sent, number_recved, bytes_sent, bytes_recved, NULL);
}


int OTF_WStream_writeFileOperationSummaryKV( OTF_WStream* wstream, uint64_t time,
	uint32_t fileid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getStatsBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_SUM_PREFIX
			OTF_KEYWORD_S_SUMFILEOPERATION );

		OTF_WBuffer_writeUint32( buffer, fileid );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBEROPEN );
		OTF_WBuffer_writeUint64( buffer, nopen );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERCLOSE );
		OTF_WBuffer_writeUint64( buffer, nclose );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERREAD );
		OTF_WBuffer_writeUint64( buffer, nread );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERWRITE );
		OTF_WBuffer_writeUint64( buffer, nwrite );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERSEEK );
		OTF_WBuffer_writeUint64( buffer, nseek );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_BYTESREAD );
		OTF_WBuffer_writeUint64( buffer, bytesread );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_BYTESWRITE );
		OTF_WBuffer_writeUint64( buffer, byteswrite );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_L_SUM_PREFIX
			OTF_KEYWORD_L_SUMFILEOPERATION " " );

		OTF_WBuffer_writeUint32( buffer, fileid );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBEROPEN " " );
		OTF_WBuffer_writeUint64( buffer, nopen );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBERCLOSE " " );
		OTF_WBuffer_writeUint64( buffer, nclose );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBERREAD " " );
		OTF_WBuffer_writeUint64( buffer, nread );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBERWRITE " " );
		OTF_WBuffer_writeUint64( buffer, nwrite );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBERSEEK " " );
		OTF_WBuffer_writeUint64( buffer, nseek );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_BYTESREAD " " );
		OTF_WBuffer_writeUint64( buffer, bytesread );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_BYTESWRITE " " );
		OTF_WBuffer_writeUint64( buffer, byteswrite );
	}

	OTF_WBuffer_writeNewline( buffer );

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeFileOperationSummary( OTF_WStream* wstream, uint64_t time,
	uint32_t fileid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite ) {

	return OTF_WStream_writeFileOperationSummaryKV(wstream, time, fileid, process, nopen,
			nclose, nread, nwrite, nseek, bytesread, byteswrite, NULL);
}


int OTF_WStream_writeFileGroupOperationSummaryKV( OTF_WStream* wstream, uint64_t time,
	uint32_t groupid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite, OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getStatsBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( 0 == OTF_WBuffer_setTimeAndProcess( buffer, time, process ) ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_SUM_PREFIX
			OTF_KEYWORD_S_SUMFILEGROUPOPERATION );

		OTF_WBuffer_writeUint32( buffer, groupid );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBEROPEN );
		OTF_WBuffer_writeUint64( buffer, nopen );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERCLOSE );
		OTF_WBuffer_writeUint64( buffer, nclose );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERREAD );
		OTF_WBuffer_writeUint64( buffer, nread );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERWRITE );
		OTF_WBuffer_writeUint64( buffer, nwrite );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERSEEK );
		OTF_WBuffer_writeUint64( buffer, nseek );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_BYTESREAD );
		OTF_WBuffer_writeUint64( buffer, bytesread );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_BYTESWRITE );
		OTF_WBuffer_writeUint64( buffer, byteswrite );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_L_SUM_PREFIX
			OTF_KEYWORD_L_SUMFILEGROUPOPERATION " " );

		OTF_WBuffer_writeUint32( buffer, groupid );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBEROPEN " " );
		OTF_WBuffer_writeUint64( buffer, nopen );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBERCLOSE " " );
		OTF_WBuffer_writeUint64( buffer, nclose );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBERREAD " " );
		OTF_WBuffer_writeUint64( buffer, nread );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBERWRITE " " );
		OTF_WBuffer_writeUint64( buffer, nwrite );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NUMBERSEEK " " );
		OTF_WBuffer_writeUint64( buffer, nseek );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_BYTESREAD " " );
		OTF_WBuffer_writeUint64( buffer, bytesread );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_BYTESWRITE " " );
		OTF_WBuffer_writeUint64( buffer, byteswrite );
	}

	OTF_WBuffer_writeNewline( buffer );

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeFileGroupOperationSummary( OTF_WStream* wstream, uint64_t time,
	uint32_t groupid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite ) {

	return OTF_WStream_writeFileGroupOperationSummaryKV(wstream, time, groupid,
			process, nopen, nclose, nread, nwrite, nseek, bytesread,
			byteswrite, NULL);
}


/** Write a def marker record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeDefMarkerKV( OTF_WStream* wstream,
		uint32_t token, const char* name, uint32_t type,
		OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getMarkerBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_MARKER_PREFIX
			OTF_KEYWORD_S_MARKER_DEFMARKER );
		OTF_WBuffer_writeUint32( buffer, token );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_TYPE );
		OTF_WBuffer_writeUint32( buffer, type );

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_L_MARKER_PREFIX
			OTF_KEYWORD_L_MARKER_DEFMARKER " " );
		OTF_WBuffer_writeUint32( buffer, token );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_NAME " " );
		OTF_WBuffer_writeString( buffer, name );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_TYPE " " );
		OTF_WBuffer_writeUint32( buffer, type );

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeDefMarker( OTF_WStream* wstream, 
        uint32_t token, const char* name, uint32_t type ) {

	return OTF_WStream_writeDefMarkerKV(wstream, token, name, type, NULL);
}


/** Write a marker record to stream 'wstream'. \ingroup wstream  */
int OTF_WStream_writeMarkerKV( OTF_WStream* wstream, 
        	uint64_t time, uint32_t process, uint32_t token, const char* text,
		OTF_KeyValueList* list ) {


	OTF_WBuffer* buffer= OTF_WStream_getMarkerBuffer( wstream );

	/* buffer can be NULL if file-open fails */
	if ( NULL == buffer ) return 0;

	/* time and process are written directly, 
	this is completely different from event records! */

	if ( OTF_WSTREAM_FORMAT_SHORT == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_short(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_MARKER_PREFIX
			OTF_KEYWORD_S_MARKER_MARKERSPOT );
		OTF_WBuffer_writeUint32( buffer, token );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_TIME );
		OTF_WBuffer_writeUint64( buffer, time );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_PROCESS );
		OTF_WBuffer_writeUint32( buffer, process );
		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_S_LOCAL_VALUE );
		OTF_WBuffer_writeString( buffer, text );

		OTF_WBuffer_writeNewline( buffer );

	} else if ( OTF_WSTREAM_FORMAT_LONG == ( wstream->format & 1 ) ) {

		OTF_WBuffer_writeKeyValueList_long(buffer, list);

		OTF_WBuffer_writeKeyword( buffer, OTF_KEYWORD_L_MARKER_PREFIX
			OTF_KEYWORD_L_MARKER_MARKERSPOT " " );
		OTF_WBuffer_writeUint32( buffer, token );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_TIME " " );
		OTF_WBuffer_writeUint64( buffer, time );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_PROCESS " " );
		OTF_WBuffer_writeUint32( buffer, process );
		OTF_WBuffer_writeKeyword( buffer, " " OTF_KEYWORD_L_LOCAL_VALUE " " );
		OTF_WBuffer_writeString( buffer, text );

		OTF_WBuffer_writeNewline( buffer );
	}

	/* one or more of the last write operations could be failed;
	check otf_errno for errors */
	return ( OTF_NO_ERROR == otf_errno ) ? 1 : 0;
}

int OTF_WStream_writeMarker( OTF_WStream* wstream, 
        uint64_t time, uint32_t process, uint32_t token, const char* text ) {

	return OTF_WStream_writeMarkerKV(wstream, time, process, token, text, NULL);
}
