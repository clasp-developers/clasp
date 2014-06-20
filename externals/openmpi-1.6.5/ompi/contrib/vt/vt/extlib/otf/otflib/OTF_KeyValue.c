/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch, Johannes Spazier
*/

#include "OTF_KeyValue.h"
#include "OTF_Errno.h"

#include <stdlib.h>
#include <string.h>


uint8_t OTF_KeyValueList_getValue(OTF_KeyValueList *list, uint32_t key, OTF_Type otf_type, OTF_Value *otf_value);

OTF_KeyValueList *OTF_KeyValueList_new() {

	OTF_KeyValueList *list = (OTF_KeyValueList*) malloc( sizeof(OTF_KeyValueList) );

	if (list == NULL) {
		/* error: not enough memory left */
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		return NULL;
	}

	list->kvBegin = (OTF_KeyValuePairList*) malloc( sizeof(OTF_KeyValuePairList) );

	if (list->kvBegin == NULL) {
		/* error: not enough memory left */
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		free(list);

		return NULL;
	}

	list->kvBegin->kvNext = NULL;
    list->kvBegin->kvPrev = NULL;

	list->kvEnd = list->kvCurrent = list->kvBegin;

    list->key_count = 0;
	list->count = 0;
	list->size = 1;

	if( OTF_KeyValueList_realloc(list, 9) ) {
		/* an error ocurred while realloc */
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		OTF_KeyValueList_close(list);
		return NULL;
	}

	return list;
}

uint8_t OTF_KeyValueList_close(OTF_KeyValueList* list) {

	OTF_KeyValuePairList *next;
	OTF_KeyValuePairList *p;

	if (list == NULL) {
		/* error */
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no list has been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		return 1;
	}

	p = list->kvBegin;

	while (p->kvNext != NULL) {
		next = p->kvNext;
		free(p);
		p = next;
	}

	free(p);
	free(list);

	return 0;
}

OTF_KeyValueList* OTF_KeyValueList_clone(OTF_KeyValueList* list) {

	OTF_KeyValueList *new_list;
	new_list = OTF_KeyValueList_new();
	if ( new_list == NULL || list == NULL ) {
		return new_list;
	}
	OTF_KeyValueList_appendKeyValueList( new_list, list );
	return new_list;
}

uint8_t OTF_KeyValueList_reset(OTF_KeyValueList* list) {
	
	/*OTF_KeyValuePairList *next;
	OTF_KeyValuePairList *p;
	int num = 0;*/

	if ( list == NULL) {
		/* error */
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no list has been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		return 1;
	}

	list->kvCurrent = list->kvBegin;
    list->key_count = 0;
	list->count = 0;


	/* deallocate memory, reduce size. */
	/* this is likely slower than keep the allocated memory */

/*
	p = list->kvBegin;

	while (p != NULL && num < 9) {
		p = p->kvNext;
		num++;
	}

	list->size = 10;
	list->kvEnd = p;
	p = p->kvNext;

	while (p != NULL) {
		next = p->kvNext;
		free(p);
		p = next;
	}

	list->kvEnd->kvNext = NULL;
*/

	return 0;
}

uint8_t OTF_KeyValueList_realloc(OTF_KeyValueList* list, uint32_t num) {

	uint32_t i;
	OTF_KeyValuePairList *p;

	if (list == NULL) {
		/* error */
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no list has been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		return 1;
	}

	p = list->kvEnd;
	
	for( i=0; i<num; i++ ) {
		p->kvNext = (OTF_KeyValuePairList*) malloc( sizeof(OTF_KeyValuePairList) );

		if( p->kvNext == NULL) {
			/* error: not enough memory left */
			/* maybe some memory was allocated before, therefore increase list */
			list->kvEnd = p;
			list->size += i;

			return 1;
		}

        p->kvNext->kvPrev = p;
		p = p->kvNext;	
		p->kvNext = NULL;
	}

	list->kvEnd = p;

	list->size += num;

	return 0;
}

uint8_t OTF_KeyValueList_appendPair(OTF_KeyValueList* list, OTF_KeyValuePair pair) {
	
	OTF_KeyValuePairList *p;
	uint32_t i;

	if (list == NULL) {
		/* error */
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no list has been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		return 255;
	}

	p = list->kvBegin;

	/* check if key already exists */
	for ( i=0; i<list->count; i++) {
        /* for byte arrays it is ok that one key can be multiple times in list
           --> OTF_KeyValueList_appendByteArray() controls the rest */
		if ( (p->kvPair.key == pair.key) && ( pair.type != OTF_BYTE_ARRAY ) ) {
			return 1;
		}
		p = p->kvNext;
	}	

	p = list->kvCurrent;

	if ( (list->size - list->count) <= 1 ) {
		if( OTF_KeyValueList_realloc(list, 10) ) {
			/* an error ocurred while realloc */
			if ( (list->size - list->count) < 1 ) {
				/* if no memory left, return with error */
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"no memory left.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				return 255;
			}
		}
	}

    if( ( list->kvCurrent->kvPrev == NULL ) || ( list->kvCurrent->kvPrev->kvPair.key != pair.key ) ) {
    
        list->key_count++;
      
    }

	p->kvPair = pair;
	list->kvCurrent = p->kvNext;

	list->count++;

	return 0;
}


uint8_t OTF_KeyValueList_appendChar(OTF_KeyValueList* list, uint32_t key, char value) {
	
	OTF_KeyValuePair pair;

	pair.key = key;
	pair.type = OTF_CHAR;
	pair.value.otf_char = value;

	return OTF_KeyValueList_appendPair(list, pair);
}


uint8_t OTF_KeyValueList_appendInt8(OTF_KeyValueList* list, uint32_t key, int8_t value) {
	
	OTF_KeyValuePair pair;

	pair.key = key;
	pair.type = OTF_INT8;
	pair.value.otf_int8 = value;

	return OTF_KeyValueList_appendPair(list, pair);
}


uint8_t OTF_KeyValueList_appendUint8(OTF_KeyValueList* list, uint32_t key, uint8_t value) {
	
	OTF_KeyValuePair pair;

	pair.key = key;
	pair.type = OTF_UINT8;
	pair.value.otf_uint8 = value;

	return OTF_KeyValueList_appendPair(list, pair);
}


uint8_t OTF_KeyValueList_appendInt16(OTF_KeyValueList* list, uint32_t key, int16_t value) {
	
	OTF_KeyValuePair pair;

	pair.key = key;
	pair.type = OTF_INT16;
	pair.value.otf_int16 = value;

	return OTF_KeyValueList_appendPair(list, pair);
}


uint8_t OTF_KeyValueList_appendUint16(OTF_KeyValueList* list, uint32_t key, uint16_t value) {
	
	OTF_KeyValuePair pair;

	pair.key = key;
	pair.type = OTF_UINT16;
	pair.value.otf_uint16 = value;

	return OTF_KeyValueList_appendPair(list, pair);
}


uint8_t OTF_KeyValueList_appendInt32(OTF_KeyValueList* list, uint32_t key, int32_t value) {
	
	OTF_KeyValuePair pair;

	pair.key = key;
	pair.type = OTF_INT32;
	pair.value.otf_int32 = value;

	return OTF_KeyValueList_appendPair(list, pair);
}


uint8_t OTF_KeyValueList_appendUint32(OTF_KeyValueList* list, uint32_t key, uint32_t value) {
	
	OTF_KeyValuePair pair;

	pair.key = key;
	pair.type = OTF_UINT32;
	pair.value.otf_uint32 = value;

	return OTF_KeyValueList_appendPair(list, pair);
}


uint8_t OTF_KeyValueList_appendInt64(OTF_KeyValueList* list, uint32_t key, int64_t value) {
	
	OTF_KeyValuePair pair;

	pair.key = key;
	pair.type = OTF_INT64;
	pair.value.otf_int64 = value;

	return OTF_KeyValueList_appendPair(list, pair);
}


uint8_t OTF_KeyValueList_appendUint64(OTF_KeyValueList* list, uint32_t key, uint64_t value) {

	OTF_KeyValuePair pair;

	pair.key = key;
	pair.type = OTF_UINT64;
	pair.value.otf_uint64 = value;

	return OTF_KeyValueList_appendPair(list, pair);
}


uint8_t OTF_KeyValueList_appendFloat(OTF_KeyValueList* list, uint32_t key, float value) {
	
	OTF_KeyValuePair pair;

	pair.key = key;
	pair.type = OTF_FLOAT;
	pair.value.otf_float = value;

	return OTF_KeyValueList_appendPair(list, pair);
}


uint8_t OTF_KeyValueList_appendDouble(OTF_KeyValueList* list, uint32_t key, double value) {
	
	OTF_KeyValuePair pair;

	pair.key = key;
	pair.type = OTF_DOUBLE;
	pair.value.otf_double = value;

	return OTF_KeyValueList_appendPair(list, pair);
}


uint8_t OTF_KeyValueList_appendByteArray(OTF_KeyValueList* list, uint32_t key, uint8_t *value, uint32_t len) {

	OTF_KeyValuePair pair;
    
    uint32_t remaining_len = len;
    uint8_t ret = 0;
    
    /* check here if key is not in list */
    if( ! OTF_KeyValueList_hasKey( list, key ) ) {
    
        /* key already in list */        
        return 1;
      
    }

    while( ( remaining_len > 0 ) && ( ! ret ) ) {
    
        len = remaining_len;
      
	    if (remaining_len > OTF_KEYVALUE_MAX_ARRAY_LEN) {
		    len = OTF_KEYVALUE_MAX_ARRAY_LEN;
	    }
		
	    pair.key = key;
	    pair.type = OTF_BYTE_ARRAY;

	    memcpy(pair.value.otf_byte_array.array, value, len);
        value+=len;
	    pair.value.otf_byte_array.len = remaining_len;
       
	    ret = OTF_KeyValueList_appendPair(list, pair);
        
        remaining_len -= len;
    
    }
    
    return ret;
}


uint8_t OTF_KeyValueList_appendKeyValueList(OTF_KeyValueList *dest_list, OTF_KeyValueList *source_list) {

	OTF_KeyValuePairList *p;
	uint32_t i;

	if ( source_list == NULL) {
		return 1;
	}

	p = source_list->kvBegin;

	for( i = 0; i < source_list->count; i++ ) {

		if ( 255 == OTF_KeyValueList_appendPair(dest_list, p->kvPair) ) {
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"error while appending OTF_KeyValueList.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return 1;
		}

		p = p->kvNext;
	}

	return 0;
}



double OTF_Int64ToDouble(uint64_t value) {
	double *d = (double*) &value;

	return *d;
}

uint64_t OTF_DoubleToInt64(double value) {
	uint64_t *i = (uint64_t*) &value;

	return *i;
}

float OTF_Int32ToFloat(uint32_t value) {
	float *d = (float*) &value;

	return *d;
}

uint32_t OTF_FloatToInt32(float value) {
	uint32_t *i = (uint32_t*) &value;

	return *i;
}

uint8_t OTF_KeyValueList_getValue(OTF_KeyValueList *list, uint32_t key, OTF_Type otf_type, OTF_Value *otf_value) {

	OTF_KeyValuePairList *p;
	uint32_t i;

	if (list == NULL) {
		/* error */
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no list has been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		return 255;
	}

	p = list->kvBegin;

	/* search key */
	for ( i=0; i<list->count; i++) {
		if ( p->kvPair.key == key ) {
			if ( p->kvPair.type == otf_type) {
				*otf_value = p->kvPair.value;
				/* found matching key and type, all right */
				return 0;
			} else {
				/* type of found key differs */
				return 2;
			}
		}
		p = p->kvNext;
	}

	/* no key in list matches the searched key */
	return 1;
}

uint8_t OTF_KeyValueList_getChar(OTF_KeyValueList *list, uint32_t key, char *value) {

	OTF_Value otf_value;
	int ret;
	
	if( ! (ret = OTF_KeyValueList_getValue(list, key, OTF_CHAR, &otf_value)) ) {
	  	*value = otf_value.otf_char;
	}

	return ret;
}

uint8_t OTF_KeyValueList_getInt8(OTF_KeyValueList *list, uint32_t key, int8_t *value) {

	OTF_Value otf_value;
	int ret;
	
	if( ! (ret = OTF_KeyValueList_getValue(list, key, OTF_INT8, &otf_value)) ) {
	  	*value = otf_value.otf_int8;
	}

	return ret;
}

uint8_t OTF_KeyValueList_getUint8(OTF_KeyValueList *list, uint32_t key, uint8_t *value) {

	OTF_Value otf_value;
	int ret;
	
	if( ! (ret = OTF_KeyValueList_getValue(list, key, OTF_UINT8, &otf_value)) ) {
	  	*value = otf_value.otf_uint8;
	}

	return ret;
}

uint8_t OTF_KeyValueList_getInt16(OTF_KeyValueList *list, uint32_t key, int16_t *value) {

	OTF_Value otf_value;
	int ret;
	
	if( ! (ret = OTF_KeyValueList_getValue(list, key, OTF_INT16, &otf_value)) ) {
	  	*value = otf_value.otf_int16;
	}

	return ret;
}

uint8_t OTF_KeyValueList_getUint16(OTF_KeyValueList *list, uint32_t key, uint16_t *value) {

	OTF_Value otf_value;
	int ret;
	
	if( ! (ret = OTF_KeyValueList_getValue(list, key, OTF_UINT16, &otf_value)) ) {
	  	*value = otf_value.otf_uint16;
	}

	return ret;
}

uint8_t OTF_KeyValueList_getInt32(OTF_KeyValueList *list, uint32_t key, int32_t *value) {

	OTF_Value otf_value;
	int ret;
	
	if( ! (ret = OTF_KeyValueList_getValue(list, key, OTF_INT32, &otf_value)) ) {
	  	*value = otf_value.otf_int32;
	}

	return ret;
}

uint8_t OTF_KeyValueList_getUint32(OTF_KeyValueList *list, uint32_t key, uint32_t *value) {

	OTF_Value otf_value;
	int ret;
	
	if( ! (ret = OTF_KeyValueList_getValue(list, key, OTF_UINT32, &otf_value)) ) {
	  	*value = otf_value.otf_uint32;
	}

	return ret;
}

uint8_t OTF_KeyValueList_getInt64(OTF_KeyValueList *list, uint32_t key, int64_t *value) {

	OTF_Value otf_value;
	int ret;
	
	if( ! (ret = OTF_KeyValueList_getValue(list, key, OTF_INT64, &otf_value)) ) {
	  	*value = otf_value.otf_int64;
	}

	return ret;
}

uint8_t OTF_KeyValueList_getUint64(OTF_KeyValueList *list, uint32_t key, uint64_t *value) {

	OTF_Value otf_value;
	int ret;
	
	if( ! (ret = OTF_KeyValueList_getValue(list, key, OTF_UINT64, &otf_value)) ) {
	  	*value = otf_value.otf_uint64;
	}

	return ret;
}

uint8_t OTF_KeyValueList_getFloat(OTF_KeyValueList *list, uint32_t key, float *value) {

	OTF_Value otf_value;
	int ret;
	
	if( ! (ret = OTF_KeyValueList_getValue(list, key, OTF_FLOAT, &otf_value)) ) {
	  	*value = otf_value.otf_float;
	}

	return ret;
}

uint8_t OTF_KeyValueList_getDouble(OTF_KeyValueList *list, uint32_t key, double *value) {

	OTF_Value otf_value;
	int ret;
	
	if( ! (ret = OTF_KeyValueList_getValue(list, key, OTF_DOUBLE, &otf_value)) ) {
	  	*value = otf_value.otf_double;
	}

	return ret;
}

uint8_t OTF_KeyValueList_getByteArray(OTF_KeyValueList *list, uint32_t key, uint8_t *value, uint32_t *len) {

	OTF_KeyValuePairList *p;
	uint32_t i;
	uint32_t max_len;

	if (list == NULL) {
		/* error */
		OTF_Error(  "ERROR in function %s, file: %s, line: %i:\n "
				"no list has been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		return 255;
	}

	p = list->kvBegin;

	max_len = *len;
	*len = 0;

	/* search key */
	for ( i=0; i<list->count; i++) {

		if ( p->kvPair.key == key ) {

			if ( p->kvPair.type == OTF_BYTE_ARRAY) {

				if( p->kvPair.value.otf_byte_array.len <= OTF_KEYVALUE_MAX_ARRAY_LEN ) {

					if( ( *len + p->kvPair.value.otf_byte_array.len ) > max_len ) {

						/* allocated memory pointed by "value" is not big enough to store the howle byte array */
						/* fill memory until the end and exit with an error-code */                       
						memcpy(value, p->kvPair.value.otf_byte_array.array, max_len - *len);

						*len = max_len;

						return 255;

					}

					*len += p->kvPair.value.otf_byte_array.len;

					memcpy(value, p->kvPair.value.otf_byte_array.array, p->kvPair.value.otf_byte_array.len);

					/* end of byte array reached, all right */
					return 0;

				} else {

					if( ( *len + OTF_KEYVALUE_MAX_ARRAY_LEN ) > max_len ) {

						/* allocated memory pointed by "value" is not big enough to store the howle byte array */
						/* fill memory until the end and exit with an error-code */                       
						memcpy(value, p->kvPair.value.otf_byte_array.array, max_len - *len);

						*len = max_len;

						return 255;

					}

					*len += OTF_KEYVALUE_MAX_ARRAY_LEN;

					memcpy(value, p->kvPair.value.otf_byte_array.array, OTF_KEYVALUE_MAX_ARRAY_LEN);

					value += OTF_KEYVALUE_MAX_ARRAY_LEN;

				}

			} else {

				/* type of found key differs */
				return 2;
			}

			} else {

				if( *len > 0 ) {

				/* byte-array not completed */
				return 255;

			}

		}

		p = p->kvNext;

	}

	/* no key in list matches the searched key */
	return 1;
}

uint8_t OTF_KeyValueList_getArrayLength(OTF_KeyValueList *list, uint32_t key, uint32_t *len) {

	OTF_Value otf_value;
	int ret;
	
	if( ! (ret = OTF_KeyValueList_getValue(list, key, OTF_BYTE_ARRAY, &otf_value)) ) {
	  	*len = otf_value.otf_byte_array.len;
	}

	return ret;
}

OTF_Type OTF_KeyValueList_getTypeForKey(OTF_KeyValueList *list, uint32_t key) {

	OTF_KeyValuePairList *p;
	uint32_t i;

	if (list == NULL) {
		/* error */
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no list has been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return OTF_UNKNOWN;
	}

	p = list->kvBegin;

	/* search key */
	for ( i=0; i<list->count; i++) {
		if ( p->kvPair.key == key ) {
			/* found matching key, all right, return type */
			return p->kvPair.type;
		}
		p = p->kvNext;
	}
	
	return OTF_UNKNOWN;
}

uint8_t OTF_KeyValueList_hasKey(OTF_KeyValueList *list, uint32_t key) {

	OTF_KeyValuePairList *p;
	uint32_t i;

	if (list == NULL) {
		/* error */
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no list has been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 2;
	}

	p = list->kvBegin;

	/* search key */
	for ( i=0; i<list->count; i++) {
		if ( p->kvPair.key == key ) {
			/* found matching key, all right, return true */
			return 0;
		}
		p = p->kvNext;
	}
	
	/* key not found, return false */
	return 1;
}

uint8_t OTF_KeyValueList_removeKey(OTF_KeyValueList *list, uint32_t key) {

	OTF_KeyValuePairList *p;
	uint32_t i;

	if (list == NULL) {
		/* error */
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no list has been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 2;
	}

	p = list->kvBegin;

	/* search key */
	for ( i=0; i<list->count; i++) {
		if ( p->kvPair.key == key ) {
			/* found matching key, remove them, return true */
			if ( p->kvPrev ) {
				p->kvPrev->kvNext = p->kvNext;
			} else {
				list->kvBegin = p->kvNext;
			}
			if ( p->kvNext ) {
				p->kvNext->kvPrev = p->kvPrev;
			}

			/* move the deleted element after the end of the list */
			p->kvPrev = list->kvEnd;
			p->kvNext = NULL;
			list->kvEnd->kvNext=p;
			list->kvEnd= p;

			list->count--;
			return 0;
		}
		p = p->kvNext;
	}

	/* key not found, return false */
	return 1;
}

uint8_t OTF_KeyValueList_getKeyByIndex(OTF_KeyValueList *list, uint32_t index, uint32_t *key) {

	OTF_KeyValuePairList *p;
	uint32_t i;
	uint32_t prev_key = 0;

	if (list == NULL) {
		/* error */
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no list has been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 2;
	}

	if (index >= list->key_count) {
		/* no key for this index */
		return 1;
	}

	p = list->kvBegin;

	prev_key = p->kvPair.key;

	/* go to key */
	for ( i=0; i<index; i++) {

		p = p->kvNext;

		if( p->kvPair.key == prev_key ) {

			i--;
		}

		prev_key = p->kvPair.key;

	}

	/* save found key for index */
	*key = p->kvPair.key;

	return 0;
}

uint8_t OTF_KeyValueList_getPairByIndex(OTF_KeyValueList *list, uint32_t index, OTF_KeyValuePair **pair) {

	OTF_KeyValuePairList *p;
	uint32_t i;
	uint32_t prev_key = 0;

	if (list == NULL) {
		/* error */
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no list has been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 2;
	}

	if (index >= list->key_count) {
		/* no key for this index */
		return 1;
	}

	p = list->kvBegin;

	prev_key = p->kvPair.key;

	/* go to key */
	for ( i=0; i<index; i++) {

		p = p->kvNext;

		if( p->kvPair.key == prev_key ) {

			i--;
		}

		prev_key = p->kvPair.key;

	}

	/* save found key-value pair for index */
	*pair = &(p->kvPair);

	return 0;
}

uint32_t OTF_KeyValueList_getCount(OTF_KeyValueList *list) {

	return list->key_count;
}

