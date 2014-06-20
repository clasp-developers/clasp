/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_WBuffer.h
 *
 *  @brief Provides write access to trace buffers.
 *
 *  \ingroup internal
 */


#ifndef OTF_WBUFFER_H
#define OTF_WBUFFER_H


#include <stdlib.h>
#include <stdio.h>


#include "OTF_inttypes.h"


#include "OTF_File.h"
#include "OTF_Filenames.h"

#include "OTF_KeyValue.h"


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

struct struct_OTF_WBuffer {


	OTF_File* file;


	/**	Begin of the actual buffer. */
	char* buffer;

	/**	Current size of buffer. */
	uint32_t size;

	/**	Next write position in buffer. */
	uint32_t pos;

	/**	Current process inside this file buffer, necessary for state
		machine. This must not be part of OTF_WStream because there are
		multiple buffers per stream that might be written in parallel. */
	uint32_t process;

	/**	Current time inside this file buffer, necessary for state machine.
		This must not be part of OTF_WStream because there are multiple
		buffers per stream that might be written in parallel. */
	uint64_t time;
	
#ifdef HAVE_ZLIB
	/** Default size of zbuffers managed by this buffer. */
	uint32_t zbuffersize;
#endif /* HAVE_ZLIB */
};
typedef struct struct_OTF_WBuffer OTF_WBuffer;


/**	Constructor - internal use only */
OTF_WBuffer* OTF_WBuffer_open( const char* filename, OTF_FileManager* manager );

/**	Destructor - internal use only */
int OTF_WBuffer_close( OTF_WBuffer* wbuffer );

/**	Set the size of the buffer. Cannot shrink buffer but only extend. */
int OTF_WBuffer_setSize( OTF_WBuffer* wbuffer, size_t size );

/**	Set the size of the zbuffer. */
void OTF_WBuffer_setZBufferSize( OTF_WBuffer* wbuffer, uint32_t size );

/**	Writes the buffer contents to 'file' and marks the buffer empty again. */
int OTF_WBuffer_flush( OTF_WBuffer* wbuffer );

/**	Ask the buffer to guarantee at least 'space' bytes at current writing
	position before the next flush is necessary. Return 1 on success. */
int OTF_WBuffer_guarantee( OTF_WBuffer* wbuffer, size_t space );


/**	Set process state machine to 'p' and time stamp state machine to 't'.
	If 'p' is the current process and 't' is the current time stamp nothing
	is done. If the process has changed a process record will be written.
	If the time has changed the new time stamp and the current process will
	be written. If 't' is lower than the current time stamp
	it is regarded as an error. Return != 1 on success and 0 on error. */
int OTF_WBuffer_setTimeAndProcess( OTF_WBuffer* wbuffer, 
	uint64_t t, uint32_t p );

/* *** basic write operations *** */

/**	Append a key word to the write buffer. A key word is a string without
	quotes. Buffer flush is done if necessary. Return the number of bytes
	written. */
uint32_t OTF_WBuffer_writeKeyword( OTF_WBuffer* wbuffer, const char* keyword );

/**	Append a string to the write buffer. A string is surrounded by quotes.
	Buffer flush is done if necessary. Return the number of bytes written. */
uint32_t OTF_WBuffer_writeString( OTF_WBuffer* wbuffer, const char* string );

/**	Append a char to the write buffer. Buffer flush is done if necessary.
	Return the number of bytes written (=1). */
uint32_t OTF_WBuffer_writeChar( OTF_WBuffer* wbuffer, const char character );

/**	This function append an 8bit unsigned integer 'value' in hex format to
	the write buffer. Buffer flush is done if necessary. The return value
	is the number of written characters. */
uint32_t OTF_WBuffer_writeUint8( OTF_WBuffer* wbuffer, uint8_t value );

/**	This function append an 16bit unsigned integer 'value' in hex format to
	the write buffer. Buffer flush is done if necessary. The return value
	is the number of written characters. */
uint32_t OTF_WBuffer_writeUint16( OTF_WBuffer* wbuffer, uint16_t value );

/**	This function append an unsigned integer 'value' in hex format to
	the write buffer. Buffer flush is done if necessary. The return value
	is the number of written characters. */
uint32_t OTF_WBuffer_writeUint32( OTF_WBuffer* wbuffer, uint32_t value );

/**	This function append an 64bit unsigned integer 'value' in hex format to
	the write buffer. Buffer flush is done if necessary. The return value
	is the number of written characters. */
uint32_t OTF_WBuffer_writeUint64( OTF_WBuffer* wbuffer, uint64_t value );

/**	Append a newline character to the buffer. Buffer flush is done if
	necessary. Return the number of bytes written. */
uint32_t OTF_WBuffer_writeNewline( OTF_WBuffer* wbuffer );

/**	This function append an byte array in hex format to
	the write buffer. Buffer flush is done if necessary. The return value
	is the number of written characters. */
uint32_t OTF_WBuffer_writeBytes( OTF_WBuffer* wbuffer, const uint8_t *value, uint32_t len);

/**     Append a KeyValuePair to the buffer (short format). Return the number of bytes written. */
uint32_t OTF_WBuffer_writeKeyValuePair_short(OTF_WBuffer* buffer, OTF_KeyValuePair* pair);

/**     Append a KeyValuePair to the buffer (long format). Return the number of bytes written. */
uint32_t OTF_WBuffer_writeKeyValuePair_long(OTF_WBuffer* buffer, OTF_KeyValuePair* pair);

/**	Append a KeyValueList to the buffer (short format). Return the number of bytes written. */
uint32_t OTF_WBuffer_writeKeyValueList_short(OTF_WBuffer* buffer, OTF_KeyValueList *list );

/**	Append a KeyValueList to the buffer (long format). Return the number of bytes written. */
uint32_t OTF_WBuffer_writeKeyValueList_long(OTF_WBuffer* buffer, OTF_KeyValueList *list );

/** internal use */
OTF_WBuffer* OTF_WBuffer_open_zlevel( const char* filename,
	OTF_FileManager* manager, OTF_FileCompression compression );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_WBUFFER_H */

