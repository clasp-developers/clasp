/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch, Johannes Spazier
*/

/** 
 *  @file OTF_KeyValue.h
 *
 *  @brief Provides an additional list of key value pairs that can be added to records.
 *
 * \ingroup keyvalue
 */


/** 
 * \defgroup keyvalue KeyValueList Interface
 *
 * Provides an additional list of key-value pairs that can be added to records.
 * 
 *  \section keyvalue_write_example A short Example of writing a KeyValueList
 *
 * You can append key-value pairs to an OTF_KeyValueList instance that can be added to any record.
 *
 * After one call to OTF_Writer_writeXxxKV() the given KeyValueList instance will be empty.
 *
 *  \code
 *  #include <assert.h>
 *  #include "otf.h"
 *
 *  int main( int argc, char** argv ) {
 *
 * 	OTF_FileManager* manager;
 * 	OTF_Writer* writer;
 *	OTF_KeyValueList* KeyValueList;
 *
 * 	manager= OTF_FileManager_open( 100 );
 * 	assert( manager );
 *
 * 	writer = OTF_Writer_open( "mytrace", 1, manager );
 * 	assert( writer );
 *  \endcode
 *
 *  Initialize the prior declared OTF_KeyValueList.
 *
 *  \code
 * 	KeyValueList = OTF_KeyValueList_new();
 *  \endcode
 *
 * Write a DefKeyValue record that assigns key=1 to name="first_arg" with description="first argument of function" and type=OTF_INT32.
 *      \code   
 * 	OTF_Writer_writeDefKeyValue( writer, 0, 1, OTF_INT32, "first_arg", "first argument of function" );
 *      \endcode
 *
 * Append a signed integer for key=1 to the initialized KeyValueList.
 *      \code      
 * 	OTF_KeyValueList_appendInt32( KeyValueList, 1, 25);
 *      \endcode
 *
 * Write the entries of the KeyValueList together with the enter record.
 *	Afterwards the KeyValueList will be empty!
 *      \code
 * 	OTF_Writer_writeEnterKV( writer, 10000, 100, 1, 0, KeyValueList );
 *      \endcode
 *
 *  Clean up before exiting the program. Close the OTF_KeyValueList.
 *      \code
 *	OTF_KeyValueList_close( KeyValueList );
 * 	OTF_Writer_close( writer );
 * 	OTF_FileManager_close( manager );
 *
 * 	return 0;
 * }
 * \endcode
 *
 * Compile this using $ gcc -o write write.c `otfconfig --libs`.
 *
 *
 *  \section keyvalue_read_example A short Example of reading from a KeyValueList
 *
 *  \code
 *  #include <assert.h>
 *  #include <stdio.h>
 *  #include <string.h>
 *  #include "otf.h"
 *
 *  int global_key = 0;
 *
 * \endcode
 *
 * Define a callback to read all DefKeyValue records. 
 * \code
 * int handleDefKeyValue( void* userData, uint32_t stream, uint32_t key, OTF_Type type, const char* name, const char *description ) {
 * \endcode
 *
 * Find out which key you are looking for.
 * 	\code
 * 	if( strcmp( name, "first_arg") == 0 ) {
 *		global_key = key;
 * 	}
 *
 * 	return OTF_RETURN_OK;
 * }
 * \endcode
 *
 * Define a callback to read all enter records. 
 * \code
 * int handleEnter (void *userData, uint64_t time, uint32_t function, uint32_t process, uint32_t source, OTF_KeyValueList *list) {
 *
 * 	int value;
 *
 * \endcode
 * 
 * Ask for a key value pair with key=global_key. Save the value in variable "value".
 * \code
 * 	switch( OTF_KeyValueList_getInt32( list, global_key, &value) ) {
 * 	  case 0:
 * 		 printf("We entered function %u with argument %d\n", function, value);
 * 		 break;
 * 	  case 1:
 *		 printf("We entered function %u with no argument.\n", function);
 * 		 break;
 * 	  default: 
 * 		 printf("An error occurred while asking for key value pair.\n");
 * 	}
 *
 * 	return OTF_RETURN_OK;
 * }
 *
 * \endcode
 *
 * main() includes the common instructions to read an .otf-file.
 * \code
 *  int main( int argc, char** argv ) {
 *
 *  	OTF_FileManager* manager;
 * 	OTF_Reader* reader;
 * 	OTF_HandlerArray* handlers;
 *
 * 	manager= OTF_FileManager_open( 100 );
 * 	assert( manager );
 *
 *	handlers = OTF_HandlerArray_open();
 *	assert( handlers );
 *
 *	reader = OTF_Reader_open( "mytrace", manager );
 *	assert( reader );
 *
 *	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleDefKeyValue, OTF_DEFKEYVALUE_RECORD );
 *	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleEnter, OTF_ENTER_RECORD );
 *
 *	OTF_Reader_readDefinitions( reader, handlers );
 *	OTF_Reader_readEvents( reader, handlers );
 * 
 * 	OTF_Reader_close( reader );
 *	OTF_HandlerArray_close( handlers );
 * 	OTF_FileManager_close( manager );
 *
 * 	return 0;
 * }
 * \endcode
 *
 * Compile this using $ gcc -o read read.c `otfconfig --libs`.
 *
 */

#ifndef OTF_KEYVALUE_H
#define OTF_KEYVALUE_H

#include "OTF_inttypes.h"
#include "OTF_Definitions.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


/** An enum which holds all OTF datatypes that are relevant for OTF_KeyValueList. \ingroup miscellaneous */
typedef enum OTF_Type_enum {
    OTF_UNKNOWN = -1,
	OTF_CHAR = 0,		/**< */
	OTF_INT8 = 1, 		/**< */
	OTF_UINT8 = 2, 		/**< */
	OTF_INT16 = 3, 		/**< */
	OTF_UINT16 = 4, 	/**< */
	OTF_INT32 = 5, 		/**< */
 	OTF_UINT32 = 6, 	/**< */
	OTF_INT64 = 7, 		/**< */
	OTF_UINT64 = 8,		/**< */
	OTF_FLOAT = 9, 		/**< */
	OTF_DOUBLE = 10, 	/**< */
	OTF_BYTE_ARRAY = 11 /**< */
} OTF_Type;

/** @cond OTF_KeyValue */

/** the following lines are ignored by doxygen */

typedef struct byte_array_struct {
	uint8_t array[OTF_KEYVALUE_MAX_ARRAY_LEN];
	uint32_t len;
} byte_array;

typedef union OTF_Value_union {
	char	    otf_char;
	int8_t	    otf_int8;
	uint8_t	    otf_uint8;
	int16_t	    otf_int16;
	uint16_t    otf_uint16;
	int32_t	    otf_int32;
	uint32_t    otf_uint32;
	int64_t	    otf_int64;
	uint64_t    otf_uint64;
	float       otf_float;
	double 	    otf_double;
	byte_array  otf_byte_array;
} OTF_Value;

struct OTF_KeyValuePair_struct {
	uint32_t key;
	OTF_Type type;
	OTF_Value value;
};

typedef struct OTF_KeyValuePairList_struct {
	struct OTF_KeyValuePair_struct kvPair;
	struct OTF_KeyValuePairList_struct *kvNext;
    struct OTF_KeyValuePairList_struct *kvPrev;
} OTF_KeyValuePairList;

struct OTF_KeyValueList_struct {
	uint32_t key_count;             /* number of different keys in list --> user-relevant */
	uint32_t count;                 /* total number of entries in list (treat byte arrays particular) --> internal use only */
	uint32_t size;                  /* number of allocated entries --> internal */
	OTF_KeyValuePairList *kvBegin;   /* first element of the list */
	OTF_KeyValuePairList *kvEnd;     /* last allocated element of the list, may be used or not */
	OTF_KeyValuePairList *kvCurrent; /* first unused element in the list, insert new ones here */
};

/** @endcond */

/** Object type which holds a key-value list.*/
typedef struct OTF_KeyValueList_struct OTF_KeyValueList;

/** Object type which holds a key-value pair.*/
typedef struct OTF_KeyValuePair_struct OTF_KeyValuePair;

/**     
 * Create a new OTF_KeyValueList instance.
 *
 * @return             Initialized OTF_KeyValueList instance or NULL if an error
 *                     occurred.
 *
 * \ingroup keyvalue
 */
OTF_KeyValueList *OTF_KeyValueList_new(void);


/** 
 * Close an OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 * 
 * @return	   0 if instance was closed successfully and 1 otherwise.
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_close(OTF_KeyValueList* list);


/**
 * Clone an OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object or NULL.
 *                 See also OTF_KeyValueList_new().
 * 
 * @return	   Clone of the instance @a list or NULL.
 *
 * \ingroup keyvalue
 */
OTF_KeyValueList* OTF_KeyValueList_clone(OTF_KeyValueList* list);


/** 
 * Reset an OTF_KeyValueList instance without deallocating memory.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 * 
 * @return	   0 on success, 1 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_reset(OTF_KeyValueList* list);


/** 
 * Expand an OTF_KeyValueList by allocating more memory.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param num	   Number of elements for which memory should be allocated.
 * 
 * @return	   0 on success, 1 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_realloc(OTF_KeyValueList* list, uint32_t num);


/** @cond OTF_KeyValue */

/** the following lines are ignored by doxygen */

/** 
 * Append an OTF_KeyValuePair to a given OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param pair	   An initialized OTF_KeyValuePair object.
 * 
 * @return	   0 on success, 1 if pair already in list and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_appendPair(OTF_KeyValueList* list, OTF_KeyValuePair pair);

/** @endcond */


/** 
 * Append a character to a given OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the value in the KeyValueList.
 * 
 * @param value    A character that should be appended to the KeyValueList.
 *
 * @return	   0 on success, 1 if key already in list and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_appendChar(OTF_KeyValueList* list, uint32_t key, char value);


/** 
 * Append a signed integer of 8 bit to a OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the value in the KeyValueList.
 * 
 * @param value    A signed integer of 8 bit that should be appended to the KeyValueList.
 *
 * @return	   0 on success, 1 if key already in list and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_appendInt8(OTF_KeyValueList* list, uint32_t key, int8_t value);


/** 
 * Append an unsigned integer of 8 bit to a OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the value in the KeyValueList.
 * 
 * @param value    An unsigned integer of 8 bit that should be appended to the KeyValueList.
 *
 * @return	   0 on success, 1 if key already in list and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_appendUint8(OTF_KeyValueList* list, uint32_t key, uint8_t value);


/** 
 * Append a signed integer of 16 bit to a OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the value in the KeyValueList.
 * 
 * @param value    A signed integer of 16 bit that should be appended to the KeyValueList.
 *
 * @return	   0 on success, 1 if key already in list and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_appendInt16(OTF_KeyValueList* list, uint32_t key, int16_t value);


/** 
 * Append an unsigned integer of 16 bit to a OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the value in the KeyValueList.
 * 
 * @param value    An unsigned integer of 16 bit that should be appended to the KeyValueList.
 *
 * @return	   0 on success, 1 if key already in list and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_appendUint16(OTF_KeyValueList* list, uint32_t key, uint16_t value);


/** 
 * Append a signed integer of 32 bit to a OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the value in the KeyValueList.
 * 
 * @param value    A signed integer of 32 bit that should be appended to the KeyValueList.
 *
 * @return	   0 on success, 1 if key already in list and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_appendInt32(OTF_KeyValueList* list, uint32_t key, int32_t value);


/** 
 * Append an unsigned integer of 32 bit to a OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the value in the KeyValueList.
 * 
 * @param value    An unsigned integer of 32 bit that should be appended to the KeyValueList.
 *
 * @return	   0 on success, 1 if key already in list and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_appendUint32(OTF_KeyValueList* list, uint32_t key, uint32_t value);


/** 
 * Append a signed integer of 64 bit to a OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the value in the KeyValueList.
 * 
 * @param value    A signed integer of 64 bit that should be appended to the KeyValueList.
 *
 * @return	   0 on success, 1 if key already in list and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_appendInt64(OTF_KeyValueList* list, uint32_t key, int64_t value);


/** 
 * Append an unsigned integer of 64 bit to a given OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the value in the KeyValueList.
 * 
 * @param value    An unsigned integer of 64 bit that should be appended to the KeyValueList.
 *
 * @return	   0 on success, 1 if key already in list and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_appendUint64(OTF_KeyValueList* list, uint32_t key, uint64_t value);


/** 
 * Append a float to a given OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the value in the KeyValueList.
 * 
 * @param value    A float that should be appended to the KeyValueList.
 *
 * @return	   0 on success, 1 if key already in list and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_appendFloat(OTF_KeyValueList* list, uint32_t key, float value);


/** 
 * Append a double to a given OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the value in the KeyValueList.
 * 
 * @param value    A double that should be appended to the KeyValueList.
 *
 * @return	   0 on success, 1 if key already in list and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_appendDouble(OTF_KeyValueList* list, uint32_t key, double value);


/** 
 * Append a byte array to a given OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the value in the KeyValueList.
 * 
 * @param value    Pointer to a byte array that should be appended to the KeyValueList.
 *
 * @param len	   Lenght of byte array.
 *
 * @return	   0 on success, 1 if key already in list and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_appendByteArray(OTF_KeyValueList* list, uint32_t key, uint8_t *value, uint32_t len);


/** 
 * Append an existing OTF_KeyValueList to a given OTF_KeyValueList instance.
 *
 * @param dest_list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 	also OTF_KeyValueList_new().
 *
 * @param source_list   Pointer to an initialized OTF_KeyValueList instance that shall be appended to the given KeyValueList.
 *                 	See also OTF_KeyValueList_new();
 *
 * @return	   	0 on success, 1 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_appendKeyValueList(OTF_KeyValueList* dest_list, OTF_KeyValueList *source_list);


/** 
 * Read a character from the given OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param value    Pointer to a char array, where the result is saved.
 *
 * @param key	   An unique id that identifies the value in the OTF_KeyValueList.
 * 
 * @return	   0 on success, 1 if key not found, 2 if type differs
 *  	  	   and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_getChar(OTF_KeyValueList *list, uint32_t key, char *value);


/** 
 * Read a signed integer of 8 bit from the given OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the value in the OTF_KeyValueList.
 *
 * @param value    Pointer to a signed integer of 8 bit, where the result is saved.

 * 
 * @return	   0 on success, 1 if key not found, 2 if type differs
 *  	  	   and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_getInt8(OTF_KeyValueList *list, uint32_t key, int8_t *value);


/** 
 * Read an unsigned integer of 8 bit from the given OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the value in the OTF_KeyValueList.
 * 
 * @param value    Pointer to an unsigned integer of 8 bit, where the result is saved.
 *
 * @return	   0 on success, 1 if key not found, 2 if type differs
 *  	  	   and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_getUint8(OTF_KeyValueList *list, uint32_t key, uint8_t *value);


/** 
 * Read a signed integer of 16 bit from the given OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the value in the OTF_KeyValueList.
 *
 * @param value    Pointer to a signed integer of 16 bit, where the result is saved.

 * 
 * @return	   0 on success, 1 if key not found, 2 if type differs
 *  	  	   and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_getInt16(OTF_KeyValueList *list, uint32_t key, int16_t *value);


/** 
 * Read an unsigned integer of 16 bit from the given OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the value in the OTF_KeyValueList.
 * 
 * @param value    Pointer to an unsigned integer of 16 bit, where the result is saved.
 *
 * @return	   0 on success, 1 if key not found, 2 if type differs
 *  	  	   and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_getUint16(OTF_KeyValueList *list, uint32_t key, uint16_t *value);


/** 
 * Read a signed integer of 32 bit from the given OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the value in the OTF_KeyValueList.
 *
 * @param value    Pointer to a signed integer of 32 bit, where the result is saved.

 * 
 * @return	   0 on success, 1 if key not found, 2 if type differs
 *  	  	   and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_getInt32(OTF_KeyValueList *list, uint32_t key, int32_t *value);


/** 
 * Read an unsigned integer of 32 bit from the given OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the value in the OTF_KeyValueList.
 * 
 * @param value    Pointer to an unsigned integer of 32 bit, where the result is saved.
 *
 * @return	   0 on success, 1 if key not found, 2 if type differs
 *  	  	   and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_getUint32(OTF_KeyValueList *list, uint32_t key, uint32_t *value);


/** 
 * Read a signed integer of 64 bit from the given OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 * @param key	   An unique id that identifies the value in the OTF_KeyValueList.
 * 
 *
 * @param value    Pointer to a signed integer of 64 bit, where the result is saved.
 *
 * @return	   0 on success, 1 if key not found, 2 if type differs
 *  	  	   and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_getInt64(OTF_KeyValueList *list, uint32_t key, int64_t *value);


/** 
 * Read an unsigned integer of 64 bit from the given OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 * @param key	   An unique id that identifies the value in the OTF_KeyValueList.
 * 
 *
 * @param value    Pointer to an unsigned integer of 64 bit, where the result is saved.
 *
 * @return	   0 on success, 1 if key not found, 2 if type differs
 *  	  	   and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_getUint64(OTF_KeyValueList *list, uint32_t key, uint64_t *value);


/** 
 * Read a float from the given OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the value in the OTF_KeyValueList.
 *
 * @param value    Pointer to a float, where the result is saved.
 * 
 * @return	   0 on success, 1 if key not found, 2 if type differs
 *  	  	   and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_getFloat(OTF_KeyValueList *list, uint32_t key, float *value);


/** 
 * Read a double from the given OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the value in the OTF_KeyValueList.
 *
 * @param value    Pointer to a double, where the result is saved.
 * 
 * @return	   0 on success, 1 if key not found, 2 if type differs
 *  	  	   and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_getDouble(OTF_KeyValueList *list, uint32_t key, double *value);


/** 
 * Read a byte array from the given OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the value in the OTF_KeyValueList.
 *
 * @param value    Pointer to an allocated byte array, where the result is saved.
 *
 * @param len      Input and output parameter: Musst contain the number of bytes
 *                 that can be written to the buffer pointed by the parameter \a value.
 *                 Therefore at least \a len bytes musst be allocated for this buffer before.
 *                 At the end \e len contains the number of bytes written to the buffer.
 *                 This can be less or equal to the input of parameter \a len. To get the
 *                 total number of bytes stored in the byte-array for the specific \a key you
 *                 can use the function OTF_KeyValueList_getArrayLength() before.
 * 
 * @return	   0 on success, 1 if key not found, 2 if type differs
 *  	  	   and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_getByteArray(OTF_KeyValueList *list, uint32_t key, uint8_t *value, uint32_t *len);


/**
 * Provides the lenght of a byte array in an OTF_KeyValueList instance by given key.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the pair in the OTF_KeyValueList.
 *
 * @param len      Return value for the array length.
 * 
 * @return	   0 on success, 1 if key not found, 2 if type differs
 *  	  	   and 255 if an error occurs
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_getArrayLength(OTF_KeyValueList *list, uint32_t key, uint32_t *len);


/** 
 * Search for key in given OTF_KeyValueList instance and return its type.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   An unique id that identifies the pair in the OTF_KeyValueList.
 * 
 * @return         OTF_Type on success, OTF_UNKNOWN if key not found or an error occurred.
 *
 * \ingroup keyvalue
 */
OTF_Type OTF_KeyValueList_getTypeForKey(OTF_KeyValueList *list, uint32_t key);


/**
 * Search for key in the given OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key	   Token that shall be searched in the OTF_KeyValueList.
 * 
 * @return	   0 on success, 1 if key not found or an error occurred.
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_hasKey(OTF_KeyValueList *list, uint32_t key);


/**
 * Remove key from the given OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @param key      Token that shall be removed from the OTF_KeyValueList.
 * 
 * @return         0 on success, 1 if key not found or an error occurred.
 *
 * \ingroup keyvalue
 */
uint8_t OTF_KeyValueList_removeKey(OTF_KeyValueList *list, uint32_t key);


/**
 * Search for a key at the given index position.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 * @param index    Index position in the OTF_KeyValueList.
 *
 * @param key      Return value for the found key.
 *
 * @return         0 on success, 1 if index not in list
 *
 * \ingroup keyvalue
*/
uint8_t OTF_KeyValueList_getKeyByIndex(OTF_KeyValueList *list, uint32_t index, uint32_t *key);

/**
 * Search for a key-value pair at the given index position.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 * @param index    Index position in the OTF_KeyValueList.
 *
 * @param pair     Pointer to the found key-value pair.
 *
 * @return         0 on success, 1 if index not in list
 *
 * \ingroup keyvalue
*/
uint8_t OTF_KeyValueList_getPairByIndex(OTF_KeyValueList *list, uint32_t index, OTF_KeyValuePair **pair);

/**
 * Returns the number of elements in the given OTF_KeyValueList instance.
 *
 * @param list     Pointer to an initialized OTF_KeyValueList object. See 
 *                 also OTF_KeyValueList_new().
 *
 * @return         Number of elements currently in list.
 *
 * \ingroup keyvalue
*/
uint32_t OTF_KeyValueList_getCount(OTF_KeyValueList *list);


/** 
 * Convert an integer of 32 bit to a float.
 *
 * \ingroup keyvalue
 */
float OTF_Int32ToFloat(uint32_t value);


/** 
 * Convert a float to an integer of 32 bit.
 *
 * \ingroup keyvalue
 */
uint32_t OTF_FloatToInt32(float value);


/** 
 * Convert an integer of 64 bit to a double.
 *
 * \ingroup keyvalue
 */
double OTF_Int64ToDouble(uint64_t value);


/** 
 * Convert a double to a signed integer of 64 bit.
 *
 * \ingroup keyvalue
 */
uint64_t OTF_DoubleToInt64(double value);


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_KEYVALUE_H */


