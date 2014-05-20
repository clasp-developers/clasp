/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_RBuffer.h
 *
 *  @brief Provides read access to trace buffers.
 *
 *  \ingroup internal
 */


#ifndef OTF_RBUFFER_H
#define OTF_RBUFFER_H


#include <stdlib.h>
#include <stdio.h>


#include "OTF_inttypes.h"


#include "OTF_File.h"

#include "OTF_KeyValue.h"

/* *** some macros *** ****************************************** */
#define PARSE_ERROR( buffer ) { \
	char* record = OTF_RBuffer_printRecord( buffer ); \
	if ( NULL != record ) { \
		OTF_Error( "Parse error in function %s, file: %s, line: %i:\n %s\n", \
			__FUNCTION__, __FILE__, __LINE__, record ); \
		free( record ); \
	} \
}

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

struct struct_OTF_RBuffer {


	OTF_File* file;


	/**	actual buffer */
	char* buffer;

	/**	Current read position in buffer.
		'end <= pos' indicates an invalid state! */
	uint32_t pos;

	/**	Current end of data in buffer in case it is not full.
		'end <= pos' indicates an invalid state! */
	uint32_t end;
	
	/** Last '\n' in the buffer. */
	uint32_t lastnewline;

	/**	Current size of buffer. */
	uint32_t size;

	/**	If 'OTF_RBuffer_jump()' is called only 'jumpsize' bytes are
		read into buffer. */
	uint32_t jumpsize;

	/**	Array which hold the members of a DEFPROCESSGROUP record
		or the list of attributes of a DEFATTRLIST record. */
	uint32_t* array;
	
	/**	Current size of array. */
	uint32_t arraysize;

	/**	Current time inside this stream, necessary for state machine,
		(-1) means unknown. */
	uint64_t time;

	/**	Current process inside this stream, necessary for state machine,
		(-1) means unknown. */
	uint32_t process;

	/** Total size of the file in bytes. This is used in
		searchTime(). A value of (-1) means unknown.
		Determined by internal function OTF_RBuffer_getFileProperties(). */
	uint64_t filesize;

	/**	The very first timestamp of that stream. This is used in
		searchTime(). A value of (-1) means unknown.
		Determined by internal function OTF_RBuffer_getFileProperties(). */
	uint64_t firstTime;

	/**	The very last timestamp of that stream. This is used in
		searchTime(). A value of (-1) means unknown.
		Determined by internal function OTF_RBuffer_getFileProperties(). */
	uint64_t lastTime;
	
	OTF_KeyValueList* list;
	
#ifdef HAVE_ZLIB
	/** Default size of zbuffers managed by this buffer. */
	uint32_t zbuffersize;
#endif /* HAVE_ZLIB */
};
typedef struct struct_OTF_RBuffer OTF_RBuffer;


/**	constructor - internal use only */
OTF_RBuffer* OTF_RBuffer_open( const char* filename, OTF_FileManager* manager );

/**	constructor - internal use only -- special version with a memory buffer to read from 
instead of an input file, either compressed or uncompressed */
OTF_RBuffer* OTF_RBuffer_open_with_external_buffer( uint32_t len, const char* buffer, uint8_t is_compressed  );

/**	destructor - internal use only */
int OTF_RBuffer_close( OTF_RBuffer* rbuffer );


/**	Set buffer size. Cannot shrink buffer but only extend. */
int OTF_RBuffer_setSize( OTF_RBuffer* rbuffer, size_t size );

/**	Set zbuffer size. */
void OTF_RBuffer_setZBufferSize( OTF_RBuffer* rbuffer, uint32_t size );

/**	Set 'jumpsize'. Return 0 if 'size' is greater than the
	buffer size. */
int OTF_RBuffer_setJumpSize( OTF_RBuffer* rbuffer, size_t size );

/**	Make the next record availabe from the buffer. Return the pointer to the
	record string which is terminated by '\n' not '\0' !
	This funktion must be called before any record access. It ensures the 
	record is available completely in the buffer. Furthermore, time and process
	information is kept track of.
	It is recommended to use the 'OTF_RBuffer_readXXX()' functions below to 
	read record components instead of parsing manually. In any case, after 
	reading	'OTF_RBuffer_readNewline()' needs to be called which proceeds to 
	the next record begin no matter if there are still characters from the 
	current record present or not. */
char* OTF_RBuffer_getRecord( OTF_RBuffer* rbuffer );


/**	Ask the buffer to guarantee at least one complete record at the current
	read position inside the buffer. This means one line, e.g. '\n' character.
	If no complete record is found the buffer has to be advanced by reading new
	contents from file. Return 1 on success, 0 means the file is exceeded. */
int OTF_RBuffer_guaranteeRecord( OTF_RBuffer* rbuffer );


/**	Print the record at the current buffer position, i.e. until the next
	newline character. This is for debugging purposes only and won't modify the
	buffer in any way. */
char *OTF_RBuffer_printRecord( OTF_RBuffer* rbuffer );


/**	Jump to the given file position and restore buffer and references as if
	the buffer had reached the position by advancing through the file linearly.
	In particular, find the next record start, then find next timestamp and
	process specification in order to set 'time' and 'process' to true values.
	Return error code 1 on success. Otherwise the file is not that large or
	there are no appropriate time and process specifications on the tail of
	the file. Then the buffer contents is undefined */
int OTF_RBuffer_jump( OTF_RBuffer* rbuffer, uint64_t filepos );

/**	Read an 64bit unsigned integer in hex format from buffer and return it. */
uint64_t OTF_RBuffer_readUint64( OTF_RBuffer* rbuffer );

/**	Read an unsigned integer in hex format from buffer and return it. */
uint32_t OTF_RBuffer_readUint32( OTF_RBuffer* rbuffer );

/**	Read a string from buffer and return it. */
const char* OTF_RBuffer_readString( OTF_RBuffer* rbuffer );

/**	Read an array from buffer and return the number of elements. 
	(re)malloc memory for *array internally, needs to be freed by caller */
uint32_t OTF_RBuffer_readArray( OTF_RBuffer* rbuffer, uint32_t** array, uint32_t* size );

/**	Test if the next character equals the given one (leading spaces are
	ignored). If the right character is found return 1, and advance by 1 step.
	If the character was not found, keep the buffer position such that the test
	can be repeated with another character. */
int OTF_RBuffer_testChar( OTF_RBuffer* rbuffer, char c );

/**	Test if the next string equals the given one (leading spaces are
	ignored). The next character must not be an uppercase letter as used in 
	keywords. If the right string is found return 1, and advance the buffer
	position. If the string was not found, keep the buffer position such 
	that the test can be repeated with another string. */
int OTF_RBuffer_testKeyword( OTF_RBuffer* rbuffer, const char* string );

/**	Test if the next string equals the given one (leading spaces are
	ignored). This version is similar to the above function but does not test 
	the following character, i.e. it can be used to test for prefixes.
	If the right string is found return 1, and advance the buffer
	position. If the string was not found, keep the buffer position such 
	that the test can be repeated with another string. */
int OTF_RBuffer_testPrefix( OTF_RBuffer* rbuffer, const char* string );

/**	Read a newline such that the buffer pos is at the next record beginning.
	Skip all characters found, assume they are to be ignored.
	Return 1 on success, 0 on error. */
int OTF_RBuffer_readNewline( OTF_RBuffer* rbuffer );

/**	Advance the buffer position while there are spaces. */
void OTF_RBuffer_skipSpaces( OTF_RBuffer* rbuffer );

/**	Advance the buffer position while there are capital letters. */
void OTF_RBuffer_skipKeyword( OTF_RBuffer* rbuffer );

/**	Return the current time of the buffer. */
uint64_t OTF_RBuffer_getCurrentTime( OTF_RBuffer* rbuffer );

/**	Set the current time of the buffer to the given one. */
void OTF_RBuffer_setCurrentTime( OTF_RBuffer* rbuffer, uint64_t time );

/**	Return the current process of the buffer. */
uint32_t OTF_RBuffer_getCurrentProcess( OTF_RBuffer* rbuffer );

/**	Set the current process of the buffer to the given one. */
void OTF_RBuffer_setCurrentProcess( OTF_RBuffer* rbuffer, uint32_t process );

/**	Search the buffer for the given time and set the buffer position to
	the next record after that time. Return 1 on success, 0 on error. */
int OTF_RBuffer_searchTime( OTF_RBuffer* rbuffer, uint64_t time );

/**	Determine buffers filesize, firstTime and lastTime if not already set.
	Return 1 on success, 0 on error. */
int OTF_RBuffer_getFileProperties( OTF_RBuffer* rbuffer );

/** Returns the filesize of the file attached to this buffer */
uint64_t OTF_RBuffer_getFileSize( OTF_RBuffer* rbuffer );

/** Returns the fileposition of the file attached to this buffer */
uint64_t OTF_RBuffer_getFilePos( OTF_RBuffer* rbuffer );

/**	Read a byte array in hex format from buffer and return the number of
    bytes read (=lenght of array).
    If the return value is greater than max_len, there is more to read
    (this indicates an error in some cases!). */
uint32_t OTF_RBuffer_readBytes( OTF_RBuffer* rbuffer, uint8_t *array, uint32_t max_len );

/**	Read a KeyValueList from the buffer. Return 1 on success, 0 on error */
uint32_t OTF_RBuffer_readKeyValueList(OTF_RBuffer* buffer );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_RBUFFER_H */

