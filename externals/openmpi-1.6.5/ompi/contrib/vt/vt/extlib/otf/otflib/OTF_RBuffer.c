/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch 
 also: patches by Rainer Keller, thanks a lot!
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

#include "OTF_Platform.h"
#include "OTF_RBuffer.h"
#include "OTF_Platform.h"
#include "OTF_Errno.h"

#include "OTF_Keywords.h"

/** constructor - internal use only */
int OTF_RBuffer_init( OTF_RBuffer* rbuffer );

/** destructor - internal use only */
int OTF_RBuffer_finish( OTF_RBuffer* rbuffer );

/** Fetch the next chunk from the file to buffer. Beforehand, move the
part [pos,end) to the beginning of the buffer in order to lose no record
fragments. Thus, read 'size-(end-pos)' bytes at most.
Return the number of bytes actually read. - internal use only */
size_t OTF_RBuffer_advance( OTF_RBuffer* rbuffer );


/** Check if there is a time stamp record at current buffer position. If so
read it and update the buffers time and go to the next record.
Return 1 in rbuffer case. Otherwise keep the buffer unchanged and return 0. */
char OTF_RBuffer_checkTimeRecord( OTF_RBuffer* rbuffer );


/** Check if there is a process record at current buffer position. If so
read it and update the buffers process and go to the next record.
Return 1 in rbuffer case. Otherwise keep the buffer unchanged and return 0. */
char OTF_RBuffer_checkProcessRecord( OTF_RBuffer* rbuffer );



/* *** internal functions which may be inlined ***************************** */


char OTF_RBuffer_checkTimeRecord( OTF_RBuffer* rbuffer ) {


	uint32_t p;
	char c;


	/* identify record type, assume we are at the begining of a record */
	/* tolerate leading spaces */
	/* do not change buffer or position until we are sure that rbuffer is a
	time record */

	p= rbuffer->pos;
	c= rbuffer->buffer[ p ];
	while ( ( ' ' == c ) || ( '\t' == c ) ) {
		
		++p;
		c= rbuffer->buffer[ p ];
	}

	/* check first real character c */
	if ( ( '0' <= c && c <= '9' ) || ( 'a' <= c && c <= 'f' ) ) {

		/* time stamp record affirmed */

		rbuffer->pos= p;
		rbuffer->time= OTF_RBuffer_readUint64( rbuffer );
		OTF_RBuffer_readNewline( rbuffer );
		return 1;
	}

	/* not a time record */
	return 0;
}


char OTF_RBuffer_checkProcessRecord( OTF_RBuffer* rbuffer ) {


	uint32_t p;
	char c;


	/* identify record type, assume we are at the begining of a record */
	/* tolerate leading spaces */
	/* do not change buffer or position until we are sure that rbuffer is a
	time record */

	p= rbuffer->pos;
	c= rbuffer->buffer[ p ];
	while ( ( ' ' == c ) || ( '\t' == c ) ) {

		++p;
		c= rbuffer->buffer[ p ];
	}

	/* check first real character c */
	c= rbuffer->buffer[ p ];
	if ( '*' == c ) {

		++p;
		c= rbuffer->buffer[ p ];

		/* skip spaces again */
		while ( ( ' ' == c ) || ( '\t' == c ) ) {

			++p;
			c= rbuffer->buffer[ p ];
		}

		c= rbuffer->buffer[ p ];
		if ( ( '0' <= c && c <= '9' ) || 
				( 'a' <= c && c <= 'f' ) ) {

			/* process specification record affirmed */
			rbuffer->pos= p;
			rbuffer->process= OTF_RBuffer_readUint32( rbuffer );
			OTF_RBuffer_readNewline( rbuffer );
			return 1;
		}
	}

	return 0;
}


/* *** public functions **************************************************** */


int OTF_RBuffer_init( OTF_RBuffer* rbuffer ) {


	rbuffer->file = NULL;

	/* buffer is allocated on demand */
	rbuffer->buffer= NULL;
	rbuffer->pos= 0;
	rbuffer->lastnewline= 0;
	rbuffer->end= 0;
	rbuffer->size= 0;
	rbuffer->jumpsize= 1024;
	
	rbuffer->array= NULL;
	rbuffer->arraysize= 0;

	rbuffer->time= (uint64_t) -1;
	rbuffer->process= (uint32_t) -1;

	rbuffer->filesize= (uint64_t) -1;
	rbuffer->firstTime= (uint64_t) -1;
	rbuffer->lastTime= (uint64_t) -1;
	
	rbuffer->list = NULL;
	
#ifdef HAVE_ZLIB
	rbuffer->zbuffersize= OTF_ZBUFFER_DEFAULTSIZE;
#endif /* HAVE_ZLIB */

	return 1;
}


int OTF_RBuffer_finish( OTF_RBuffer* rbuffer ) {


	free( rbuffer->buffer );
	rbuffer->buffer= NULL;
	
	if ( NULL != rbuffer->array ) {
		free( rbuffer->array );
		rbuffer->array= NULL;
	}

	return 1;
}


OTF_RBuffer* OTF_RBuffer_open( const char* filename, OTF_FileManager* manager ) {

	OTF_RBuffer* ret;

	/* Check the input parameters */
	if( NULL == manager ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"manager has not been defined.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return NULL;
	}

	ret= (OTF_RBuffer*) malloc( sizeof(OTF_RBuffer) );
	if ( NULL == ret ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );
				
		return NULL;
	}

	OTF_RBuffer_init( ret );

	ret->file= OTF_File_open( filename, manager, OTF_FILEMODE_READ );

	if ( NULL == ret->file ) {

/* *** commented because it can happen when defstream cannot be loaded
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_File_open() failed. filename '%s'\n",
				__FUNCTION__, __FILE__, __LINE__, filename );
*/

		free( ret );
		ret= NULL;

		return ret;
	}

	ret->list = OTF_KeyValueList_new();

	if (ret->list == NULL) {

		OTF_File_close(ret->file);
		ret->file = NULL;

		free( ret );
		ret= NULL;
				
		return NULL;
	}

#ifdef HAVE_ZLIB
	OTF_File_setZBufferSize( ret->file, ret->zbuffersize );
#endif /* HAVE_ZLIB */

	return ret;
}


OTF_RBuffer* OTF_RBuffer_open_with_external_buffer( uint32_t len, const char* buffer, uint8_t is_compressed ) {


	OTF_RBuffer* ret;


	ret= (OTF_RBuffer*) malloc( sizeof(OTF_RBuffer) );
	if ( NULL == ret ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return NULL;
	}

	OTF_RBuffer_init( ret );

	ret->file= OTF_File_open_with_external_buffer( len, buffer, is_compressed, OTF_FILEMODE_READ );
	if ( NULL == ret->file ) {

/* *** commented because it can happen when defstream cannot be loaded
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_File_open() failed. filename '%s'\n",
				__FUNCTION__, __FILE__, __LINE__, filename );
*/

		free( ret );
		ret= NULL;

		return ret;
	}

	ret->list = OTF_KeyValueList_new();

	if (ret->list == NULL) {

		OTF_File_close(ret->file);
		ret->file = NULL;

		free( ret );
		ret= NULL;
				
		return NULL;
	}

#ifdef HAVE_ZLIB
	OTF_File_setZBufferSize( ret->file, ret->zbuffersize );
#endif /* HAVE_ZLIB */

	return ret;
}


int OTF_RBuffer_close( OTF_RBuffer* rbuffer ) {


	int ret= 1;
	
	ret &= OTF_File_close( rbuffer->file );
	
	rbuffer->file= NULL;

	if( rbuffer->list != NULL) {
		OTF_KeyValueList_close( rbuffer->list);
	}

	OTF_RBuffer_finish( rbuffer );

	free( rbuffer );
	rbuffer = NULL;

	return ret;
}

int OTF_RBuffer_setSize( OTF_RBuffer* rbuffer, size_t size ) {


	if ( size < 100 ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"buffer size %u too small, rejected.\n",
				__FUNCTION__, __FILE__, __LINE__, (uint32_t) size );

		return 0;
	}

	if ( size < rbuffer->size ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"cannot shrink buffer from %u to %u.\n",
				__FUNCTION__, __FILE__, __LINE__, (uint32_t) rbuffer->size,
				(uint32_t) size );

		return 0;
	}

	rbuffer->size= (uint32_t) size;
	rbuffer->buffer= (char*) realloc( rbuffer->buffer, 
		rbuffer->size * sizeof(char) );
	if( NULL == rbuffer->buffer ) {
		
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}

	if ( rbuffer->jumpsize > size ) {
	
		rbuffer->jumpsize= (uint32_t) size;
	}

	return 1;
}


void OTF_RBuffer_setZBufferSize( OTF_RBuffer* rbuffer, uint32_t size ) {


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

	rbuffer->zbuffersize= size;
	
	if( NULL != rbuffer->file ) {
		OTF_File_setZBufferSize( rbuffer->file, rbuffer->zbuffersize );
	}
#endif /* HAVE_ZLIB */

}


int OTF_RBuffer_setJumpSize( OTF_RBuffer* rbuffer, size_t size ) {


	if ( size < 100 ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"jump buffer size %u too small, rejected.\n",
				__FUNCTION__, __FILE__, __LINE__, (uint32_t) size );

		return 0;
	}

	if ( size > rbuffer->size ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"buffer size is smaller than jump size %u.\n",
				__FUNCTION__, __FILE__, __LINE__, (uint32_t) size );

		return 0;
	}

	rbuffer->jumpsize = (uint32_t) size;

	return 1;
}


/** make the next record availabe from the buffer. return the pointer to the
record string which is terminated by '\n' not '\0' !
rbuffer funktion must be called before any record access. it ensures the record 
is available completely in the buffer. furthermore, time and process 
information is kept track of. 
It is recommended to use the 'OTF_RBuffer_readXXX()' functions below to read
record components instead of parsing manually. In any case, after reading
'OTF_RBuffer_readNewline()' needs to be called which proceeds to the next
record begin no matter if there are still characters from the current record
present or not. */
char* OTF_RBuffer_getRecord( OTF_RBuffer* rbuffer ) {


	/* make sure there is a complete record in the buffer but 
	 steal silently away time and process records */

	while ( OTF_RBuffer_guaranteeRecord( rbuffer ) ) {

		if ( OTF_RBuffer_checkTimeRecord( rbuffer ) ) {
		
			continue;
		}

		if ( OTF_RBuffer_checkProcessRecord( rbuffer ) ) {

			continue;
		}

		return rbuffer->buffer + rbuffer->pos;
	}


	/* error, could not read enough from file to buffer. 
	probably file is exceeded. test elsewhere */
	return NULL;
}


int OTF_RBuffer_guaranteeRecord( OTF_RBuffer* rbuffer ) {


	size_t add;
	int ret;


	/* check if there is at least one '\n' in the buffer */
	if ( rbuffer->pos < rbuffer->lastnewline ) {
	
		return 1;
	}

	add= OTF_RBuffer_advance( rbuffer );
	if ( 0 == add ) {

		/* no complete record available! end of file. */

		/*
		OTF_Error( "OTF_RBuffer_guaranteeRecord() "
			"cannot read, file exceeded\n" );
		*/

		return 0;
	}
	
	/* after advancing the buffer, check if now there is a
	complete record available */
	/* check if there is at least one '\n' in the buffer */
	if ( rbuffer->pos < rbuffer->lastnewline ) {
	
		/* success */
		return 1;
	}

	/* if not try to double buffer size until a complete record fits */

	/* hard bound for maximum buffer size */
	while ( 1024*1024*100 > rbuffer->size ) {


		ret= OTF_RBuffer_setSize( rbuffer, 2* rbuffer->size );

		if ( 1 != ret ) {

			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"cannot double buffer size.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return 0;
		}

		add= OTF_RBuffer_advance( rbuffer );
		if ( 0 == add ) {

			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"file exceeded.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return 0;
		}

		/* rbuffer->pos and rbuffer->end have changed */
		if ( rbuffer->pos < rbuffer->lastnewline ) {

			return 1;
		}
	}

	OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
			"buffer is too small. Extending buffer has finally failed.\n",
			__FUNCTION__, __FILE__, __LINE__ );

	return 0;
}

#define REALLOCSIZE 128
char *OTF_RBuffer_printRecord( OTF_RBuffer* rbuffer ) {


	char *ret= NULL;
	uint32_t pos= 0;
	uint32_t size= REALLOCSIZE;
	uint32_t c= rbuffer->pos;
	
	
	ret= (char*) malloc( size );
	if( NULL == ret ) {
		
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return NULL;
	}
	while ( ( '\n' != rbuffer->buffer[c] ) && ( c < rbuffer->end ) ) {
	
		while( (pos+1) >= size ) {
			
			ret= (char*) realloc( ret, size + REALLOCSIZE );
			if( NULL == ret ) {
				
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"no memory left.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				return NULL;
			}
			size+= REALLOCSIZE;
		}
		
		ret[pos]= rbuffer->buffer[c];
		++pos;
		++c;
	}
	
	ret[pos]= '\0';

		
	return ret;
}
#undef REALLOCSIZE

size_t OTF_RBuffer_advance( OTF_RBuffer* rbuffer ) {


	uint32_t i;
	uint32_t d;
	size_t ret;


	if ( 0 == rbuffer->size ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
			"buffer size not set!\n",
			__FUNCTION__, __FILE__, __LINE__ );
		exit(1);
	}

	if( rbuffer->pos > rbuffer->end ) {
		
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
			"current position exceeds the file length.\n",
			__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}

	d= rbuffer->end - rbuffer->pos;
	
	/* move trailing buffer contents to the begining */
	for ( i= 0; i < d; ++i ) {
	
		rbuffer->buffer[i]= rbuffer->buffer[ rbuffer->pos + i ];
	}
	rbuffer->pos= 0;

	ret= OTF_File_read( rbuffer->file, rbuffer->buffer + d, rbuffer->size - d );

	rbuffer->end= d + ret;

	i= rbuffer->end;
	rbuffer->lastnewline= 0;
	
	while ( i > 0 ) {
		
		--i;
		
		if ( '\n' == rbuffer->buffer[i] ) {
		
			rbuffer->lastnewline= i;
			break;
		}
	}

	return ret;
}


/** Jump to the given file position and restore buffer and references as if
the buffer had reached the position by advancing through the file linearly.
In particular, find the next record start, then find next timestamp and
process specification in order to set 'time' and 'process' to true values.
Return 1 on success. Otherwise, when 0 is returned the file is not that large 
or there are no appropriate time and process specifications on the tail of
the file. Then the buffer contents is undefined! */
int OTF_RBuffer_jump( OTF_RBuffer* rbuffer, uint64_t filepos ) {


	int ret;
	size_t read;
	/* uint64_t currentPos; */
	uint32_t i;

	ret= OTF_File_seek( rbuffer->file, filepos );

	if ( 0 != ret ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_File_seek() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		
		return 0;
	}

	rbuffer->pos= 0;
	read= OTF_File_read( rbuffer->file, rbuffer->buffer, rbuffer->jumpsize );

	rbuffer->end= (uint32_t) read;

	i= rbuffer->end;
	rbuffer->lastnewline= 0;

	while ( i > 0 ) {

		--i;

		if ( '\n' == rbuffer->buffer[i] ) {

			rbuffer->lastnewline= i;
			break;
		}
	}

	/* now we are somewhere in the middle of a record. read until next
	newline and take the next record which is complete */
	if ( 0 == OTF_RBuffer_guaranteeRecord( rbuffer ) ) {

		/*
		OTF_Error( "OTF_RBuffer_jump() "
				"ERROR: could not read far enough\n" );
		*/

		return 0;
	}

	/* with filepos == 0 we are sure to be already at the beginning of a record */
	if ( 0 != filepos ) {

		/* skip the incomplete record */
		OTF_RBuffer_readNewline( rbuffer );
	}

	/* make sure the next record is complete */
	if ( 0 == OTF_RBuffer_guaranteeRecord( rbuffer ) ) {

		/*
		OTF_Error( "OTF_RBuffer_jump() "
				"ERROR: could not read far enough\n" );
		*/

		return 0;
	}

	/* now we are at the begining of a record */

	rbuffer->time= (uint64_t) -1;
	rbuffer->process= (uint32_t) -1;

	/* search time stamp */

	while ( ((uint64_t) -1) == rbuffer->time ) {

		/* go on reading while time is undefined */

		if ( 0 == OTF_RBuffer_guaranteeRecord( rbuffer ) ) {

			/*
			OTF_Error( "OTF_RBuffer_jump() "
					"ERROR: could not read far enough\n" );
			*/
			return 0;
		}

		if ( OTF_RBuffer_checkTimeRecord( rbuffer ) ) {

			continue;
		}

		/* if no time record found, skip the current record */
		OTF_RBuffer_readNewline( rbuffer );
	}


	/* search process spec. */

	while ( ((uint32_t) -1) == rbuffer->process ) {

		/* go on reading while process is undefined */

		if ( 0 == OTF_RBuffer_guaranteeRecord( rbuffer ) ) {

			/*
			OTF_Error( "OTF_RBuffer_jump() "
					"ERROR: could not read far enough\n" );
			*/

			return 0;
		}

		if ( OTF_RBuffer_checkProcessRecord( rbuffer ) ) {

			continue;
		}

		/* if no time record found, skip the current record */
		OTF_RBuffer_readNewline( rbuffer );
	}

	return 1;
}


int OTF_RBuffer_readNewline( OTF_RBuffer* rbuffer ) {


	/* skip all non-newline characters */
	while ( '\n' != rbuffer->buffer[ rbuffer->pos ] ) {

		++(rbuffer->pos);

		if ( rbuffer->pos >= rbuffer->end ) {

			return 0;
		}
	}

	/* read newline */
	++(rbuffer->pos);

	/* dont need to catch ( rbuffer->pos >= rbuffer->end ) since rbuffer is
	its own error condition */

	return 1;
}


void OTF_RBuffer_skipSpaces( OTF_RBuffer* rbuffer ) {

	/* skip spaces */
	while ( ( ( ' ' == rbuffer->buffer[ rbuffer->pos ] ) ||
			( '\t' == rbuffer->buffer[ rbuffer->pos ] ) ) &&
			( rbuffer->pos < rbuffer->end ) ) {

		++(rbuffer->pos);
	}
}


uint64_t OTF_RBuffer_getCurrentTime( OTF_RBuffer* rbuffer ) {


	if ( rbuffer->time == (uint64_t) -1 ) {

		OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				"Invalid time.",
				__FUNCTION__, __FILE__, __LINE__ );
	}

	return rbuffer->time;
}


void OTF_RBuffer_setCurrentTime( OTF_RBuffer* rbuffer, uint64_t time ) {


	rbuffer->time= time;
}


uint32_t OTF_RBuffer_getCurrentProcess( OTF_RBuffer* rbuffer ) {


	return rbuffer->process;
}


void OTF_RBuffer_setCurrentProcess( OTF_RBuffer* rbuffer, uint32_t process ) {


	rbuffer->process= process;
}


uint64_t OTF_RBuffer_readUint64( OTF_RBuffer* rbuffer ) {


	char c;
	uint64_t ret = 0;


	OTF_RBuffer_skipSpaces( rbuffer );

	while( 1 ) {

		c= rbuffer->buffer[rbuffer->pos];

		if ( '0' <= c && c <= '9' ) {

			ret = (ret << 4) | ( c - '0' );
			++(rbuffer->pos);

		} else 	if ( 'a' <= c && c <= 'f' ) {

			ret = (ret << 4) | ( c - ( 'a' - 10 ) );
			++(rbuffer->pos);

		} else {

			return ret;
		}
	}
}


uint32_t OTF_RBuffer_readUint32( OTF_RBuffer* rbuffer ) {


	char c;
	uint32_t ret = 0;


	OTF_RBuffer_skipSpaces( rbuffer );

	while( 1 ) {

		c= rbuffer->buffer[rbuffer->pos];

		if ( '0' <= c && c <= '9' ) {

			ret = (ret << 4) | ( c - '0' );
			++(rbuffer->pos);

		} else if ( 'a' <= c && c <= 'f' ) {

			ret = (ret << 4) | ( c - ( 'a' - 10 ) );
			++(rbuffer->pos);

		} else {

			return ret;
		}
	}
}


int OTF_RBuffer_testChar( OTF_RBuffer* rbuffer, char c ) {


	OTF_RBuffer_skipSpaces( rbuffer );

	/* only advance the buffer position if the right character is found */
	if ( ( rbuffer->pos < rbuffer->end ) && 
			( c == rbuffer->buffer[rbuffer->pos] ) ) {

		++(rbuffer->pos);
		return 1;
	}

	return 0;
}


int OTF_RBuffer_testKeyword( OTF_RBuffer* rbuffer, const char* string ) {


	uint32_t i = 0;
	uint32_t j = rbuffer->pos;

	OTF_RBuffer_skipSpaces( rbuffer );

	/* only advance the buffer position if the right string is found */
	while ( string[i] )	{

		if ( string[i] != rbuffer->buffer[j] ) {

			return 0;

		} else {

			++i;
			++j;
		}
	}

	/* if the next character in buffer after the keyword is \in [A,Z]
	then the actual keyword is continued ==> not matching! */
	if ( ( 'A' <= rbuffer->buffer[j] ) && ( 'Z' >= rbuffer->buffer[j] ) ) {

		return 0;
	}

	rbuffer->pos = j;

	return 1;
}


int OTF_RBuffer_testPrefix( OTF_RBuffer* rbuffer, const char* string ) {


	uint32_t i = 0;
	uint32_t j = rbuffer->pos;


	OTF_RBuffer_skipSpaces( rbuffer );

	/* only advance the buffer position if the right string is found */
	while ( string[i] )	{

		if ( string[i] != rbuffer->buffer[j] ) {

			return 0;

		} else {

			++i;
			++j;
		}
	}

	rbuffer->pos = j;

	return 1;
}


/** Determine buffers firstTime and lastTime if not already set.
Return 1 on success, 0 on error. */
int OTF_RBuffer_getFileProperties( OTF_RBuffer* rbuffer ) {


/* range where the last timestamp is searched */
#define SEARCH_RANGE 4096


	uint64_t pos;
	uint32_t searchRange;


	/** if both are != -1 we assume everything is fine and return
	successful. otherwise we go fetching both. */
	if ( ( ((uint64_t) -1) != rbuffer->filesize ) ||
			( ((uint64_t) -1) != rbuffer->firstTime ) ||
			( ((uint64_t) -1) != rbuffer->lastTime ) ) {

		return 1;
	}

	rbuffer->filesize= OTF_File_size( rbuffer->file );


	/* jump to very beginning of file */
	if ( !OTF_RBuffer_jump( rbuffer, 0 ) ) {

		return 0;
	}


	/* get very first timestamp easily */
	rbuffer->firstTime= rbuffer->time;

	/* now get the last one - rbuffer is a bit more tricky */

	/* search range must be smaller than buffer size */
	searchRange= ( SEARCH_RANGE < rbuffer->size ) ? SEARCH_RANGE : rbuffer->size;
	if( 0 >= searchRange ) {
		
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"the search range is not allowed to be '0'.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}

	/* file position to jump to: a little bit before file end */
	pos= ( rbuffer->filesize > searchRange ) ? ( rbuffer->filesize - searchRange ) : 0;

	/* while unsuccessful jump */
	rbuffer->time= (uint64_t) -1;
	/*while ( ( !OTF_RBuffer_jump( rbuffer, pos ) ) && !!!!!!!!!!!!!!!!!!!!!!!!!
			( pos > searchRange ) ) {

		pos= pos - searchRange;
	}*/
	while ( ( !OTF_RBuffer_jump( rbuffer, pos ) ) &&
			( pos > 0 ) ) {

		pos= (searchRange < pos) ? pos - searchRange : 0;
	}
	
	if ( ((uint64_t) -1) == rbuffer->time ) {

		/* found no time stamp -> give up */
		return 0;
	}

	/* we found a time stamp but maybe not the last one.
	so traverse to the end of the file */
	while ( OTF_RBuffer_getRecord(rbuffer) ) {

		OTF_RBuffer_readNewline( rbuffer );
	}

	rbuffer->lastTime= rbuffer->time;


#undef SEARCH_RANGE

	return 1;
}


int OTF_RBuffer_searchTime( OTF_RBuffer* rbuffer, uint64_t time ) {


	uint64_t posA;
	uint64_t posB;
	uint64_t posC;

	uint64_t timeA;
	uint64_t timeB;
	uint64_t timeC;

	int ret;
	char* ret2;
	

#define mindist 1024

	if ( 0 == OTF_RBuffer_getFileProperties( rbuffer ) ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"could not determine file size (%llu) or first/last time"
				" (%llx/%llx)\n",
				__FUNCTION__, __FILE__, __LINE__,
				(unsigned long long) rbuffer->filesize,
				(unsigned long long) rbuffer->firstTime,
				(unsigned long long) rbuffer->lastTime );

		return 0;
	}

	posA = 0;
	posB = rbuffer->filesize;

	timeA= rbuffer->firstTime;
	timeB= rbuffer->lastTime;

	/* catch cases where search time lies outside
	the file's time interval */
	if ( time <= timeA ) {
	
		ret= OTF_RBuffer_jump( rbuffer, posA );
		if ( 1 != ret ) {

			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"unsuccessful jump to begin pos= %llu.\n",
					__FUNCTION__, __FILE__, __LINE__, (unsigned long long) posA );
			
			return 0;
		}

		return 1;
	}
	else if ( time > timeB ) {
		/* consume all records, so that the caller get none */

		do {
			/* need to loop, so that OTF_RBuffer_jump finds the
			start of a zlib block */
			if ( posB > rbuffer->jumpsize ) {
				posB -= rbuffer->jumpsize;
			} else {
				posB = 0;
			}
			ret= OTF_RBuffer_jump( rbuffer, posB );
		} while ( 1 != ret );
		if ( posB == 0 && 1 != ret ) {

			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"unsuccessful jump to begin pos= %llu.\n",
					__FUNCTION__, __FILE__, __LINE__, (unsigned long long) posB );

			return 0;
		}
		while ( OTF_RBuffer_getRecord( rbuffer ) ) {

			OTF_RBuffer_readNewline( rbuffer );
		}
		return 1;
	}


	/* do while the distance from 'posA' to 'posB' is big. 
	this works with plain or compressed file positions. */
	while ( posB - posA > mindist ) {

		/* strict half splitting. robust version, 
		maybe add proportional splitting later */
		posC= ( posA + posB ) / 2;

		ret= OTF_RBuffer_jump( rbuffer, posC );
		if (1!=ret){
		
			posB= posC;
			continue;
		}
		
		timeC= rbuffer->time;

		if ( time == timeC ) {
	
			/* hit exactly */
			return 1;
		}

		if ( time < timeC ) {

			posB= posC;
			timeB= timeC;
			/* continue; */

		} else /* if ( time > timeC ) */ {

			posA= posC;
			timeA= timeC;
			/* continue; */
		} 
	}

	/* target is nearly found, search linearly now */

	ret= OTF_RBuffer_jump( rbuffer, posA );
	if ( 1 != ret ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"unsuccessful jump to begin pos= %llu.\n",
				__FUNCTION__, __FILE__, __LINE__, (unsigned long long) posA );

		return 0;
	}

	while ( rbuffer->time < time ) {

		OTF_RBuffer_readNewline( rbuffer );
		ret2= OTF_RBuffer_getRecord( rbuffer );
		if( NULL == ret2 ) {
			
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_RBuffer_getRecord() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return 0;
		}
	}

	return 1;

#undef mindist
}


const char* OTF_RBuffer_readString( OTF_RBuffer* rbuffer ) {


	char* str;


	OTF_RBuffer_skipSpaces( rbuffer );

	if ( !OTF_RBuffer_testChar( rbuffer, '"' ) ) {

		return NULL;
	}

	str= &(rbuffer->buffer[rbuffer->pos]);

	while ( rbuffer->buffer[rbuffer->pos] != '"' ) {

		if ( rbuffer->pos < rbuffer->end ) {

			++(rbuffer->pos);

		} else {

			return NULL;
		}
	}

	rbuffer->buffer[rbuffer->pos] = '\0';
	++(rbuffer->pos);

	return str;
}


uint32_t OTF_RBuffer_readArray( OTF_RBuffer* rbuffer, uint32_t** array, uint32_t* size ) {


	uint32_t i= 0;
	uint32_t n= 0;

	const char* p= &( rbuffer->buffer[ rbuffer->pos ] );

	while ( ( ( '0' <= *p ) && ( '9' >= *p ) ) ||
			( ( 'a' <= *p ) && ( 'f' >= *p ) ) || 
			( ' '  == *p ) || 
			( '\t' == *p ) || 
			( ','  == *p ) )	{

		if ( ',' == *p ) {

			++n;
		}
		
		++p;
	}

	if ( n > (*size) ) {
		(*array)= (uint32_t*) realloc( (*array), n * sizeof(uint32_t) );
		assert( NULL != (*array) );
		(*size)= n;
	}
	
	for ( i= 0; i < n; ++i ) {

		OTF_RBuffer_skipSpaces( rbuffer );
		(*array)[i] = OTF_RBuffer_readUint32( rbuffer );
		OTF_RBuffer_testChar( rbuffer, ',' );
	}

/*
	OTF_RBuffer_skipSpaces( rbuffer );

	while ( 	( '0' <= (char) rbuffer->buffer[rbuffer->pos] &&
			(char) rbuffer->buffer[rbuffer->pos] <= '9' ) ||
			( 'a' <= (char) rbuffer->buffer[rbuffer->pos] &&
			(char) rbuffer->buffer[rbuffer->pos] <= 'f' ) )	{

		array[n] = OTF_RBuffer_readUint32( rbuffer );
		++n;
		OTF_RBuffer_testChar( rbuffer, ',' );
		OTF_RBuffer_skipSpaces( rbuffer );
	}
*/

	return n;
}


void OTF_RBuffer_skipKeyword( OTF_RBuffer* rbuffer ) {


	while ( (	(char) rbuffer->buffer[rbuffer->pos] >= 'A' &&
			(char) rbuffer->buffer[rbuffer->pos] <= 'Z' ) ||
			(char) rbuffer->buffer[rbuffer->pos] == '#') {


		++(rbuffer->pos);
	}
}


uint64_t OTF_RBuffer_getFileSize( OTF_RBuffer* rbuffer ) {


	return OTF_File_size( rbuffer->file );
}


uint64_t OTF_RBuffer_getFilePos( OTF_RBuffer* rbuffer ) {


	return OTF_File_tell( rbuffer->file );
}

uint32_t OTF_RBuffer_readBytes( OTF_RBuffer* rbuffer, uint8_t *array, uint32_t max_len ) {

	char c;
	uint32_t k = 0;
	uint32_t i = 0;

	OTF_RBuffer_skipSpaces( rbuffer );

	while( 1 ) {

		if (k == 2) {
			i++;
			k = 0;
		}

		c= rbuffer->buffer[rbuffer->pos];

		if ( '0' <= c && c <= '9' ) {
          
            if ( i >= max_len )         
                return i + 1;

			array[i] = ( array[i] << 4) | ( c - '0' );
			++(rbuffer->pos);

		} else if ( 'a' <= c && c <= 'f' ) {
          
            if ( i >= max_len )
                return i + 1;

			array[i] = ( array[i] << 4) | ( c - ( 'a' - 10 ) );
			++(rbuffer->pos);

		} else {
          
			return i;
		}

		k++;
	}
}

uint32_t OTF_RBuffer_readKeyValueList(OTF_RBuffer* buffer ) {


	OTF_KeyValuePair pair;

	if ( buffer->list == NULL) {

		return 0;
	}

	pair.key = OTF_RBuffer_readUint32( buffer );

	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_TYPE ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_TYPE ) ) {

		pair.type = (OTF_Type) OTF_RBuffer_readUint32( buffer );

	} else {
		/* Parse error */
		PARSE_ERROR( buffer );
		
		return 0;
	}
	
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_VALUE ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_VALUE ) ) {
		switch (pair.type) {
		case OTF_CHAR:
			OTF_RBuffer_readBytes( buffer, (uint8_t*) &(pair.value.otf_char), 1 );
			break;
		case OTF_INT8:
		  	pair.value.otf_int8 = OTF_RBuffer_readUint32( buffer );
			break;
		case OTF_UINT8:
			pair.value.otf_uint8 = OTF_RBuffer_readUint32( buffer );
			break;
		case OTF_INT16:
			pair.value.otf_int16 = OTF_RBuffer_readUint32( buffer );
			break;
		case OTF_UINT16:
			pair.value.otf_uint16 = OTF_RBuffer_readUint32( buffer );
			break;
		case OTF_INT32:
			pair.value.otf_int32 = OTF_RBuffer_readUint32( buffer );
			break;
		case OTF_UINT32:
			pair.value.otf_uint32 = OTF_RBuffer_readUint32( buffer );
			break;
		case OTF_INT64:
			pair.value.otf_int64 = OTF_RBuffer_readUint64( buffer );
			break;
		case OTF_UINT64:
			pair.value.otf_uint64 = OTF_RBuffer_readUint64( buffer );
			break;
		case OTF_DOUBLE:
			pair.value.otf_double = OTF_Int64ToDouble( OTF_RBuffer_readUint64( buffer ) );
			break;
		case OTF_FLOAT:
			pair.value.otf_float = OTF_Int32ToFloat( OTF_RBuffer_readUint32( buffer ) );
			break;
		case OTF_BYTE_ARRAY:
			pair.value.otf_byte_array.len = OTF_RBuffer_readBytes( buffer,
			pair.value.otf_byte_array.array, OTF_KEYVALUE_MAX_ARRAY_LEN);

			if( pair.value.otf_byte_array.len > OTF_KEYVALUE_MAX_ARRAY_LEN ) {

				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"byte-array exceeds the maximum length of %u bytes per line.\n",
						__FUNCTION__, __FILE__, __LINE__, OTF_KEYVALUE_MAX_ARRAY_LEN );

				PARSE_ERROR( buffer );

				return 0;

			}

			if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_LENGTH ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_LENGTH ) ) {

				pair.value.otf_byte_array.len = OTF_RBuffer_readUint32( buffer );

			}

			break;
		default:

			/* Pasre error */
			PARSE_ERROR( buffer );

			return 0;
		}

	} else {

		/* Parse error */
		PARSE_ERROR( buffer );
		
		return 0;
	}

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		OTF_KeyValueList_appendPair(buffer->list, pair);

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	return 1;
}

