/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include "OTF_Platform.h"
#include "OTF_MasterControl.h"
#include "OTF_Errno.h"

/**	constructor - internal use only */
int OTF_MasterControl_init( OTF_MasterControl* mc );

/**	destructor - internal use only */
void OTF_MasterControl_finalize( OTF_MasterControl* mc );

/** reset mapping to empty, keep file manager manager reference */
void OTF_MasterControl_reset( OTF_MasterControl* mc );


/**	Get the map entry with the given argument. If not yet there, create it -
	internal use only */
OTF_MapEntry* OTF_MasterControl_getMapEntry( OTF_MasterControl* mc,
	uint32_t argument );

/**	Insert a new map entry with given argument at specified pos -
	internal use only, called by OTF_MasterControl_getMapEntry() */
OTF_MapEntry* OTF_MasterControl_insertMapEntry( OTF_MasterControl* mc,
	uint32_t argument, uint32_t pos );

/**	Insert the value into the map entry. */
int OTF_MapEntry_insertValue( OTF_MapEntry* mc, uint32_t value );


/**	Insert a new reverse map entry value -> argument
	the given mapping _must_not_ be included so far
	return 1 on success, 0 if duplicate
	- internal use only */
int OTF_MasterControl_insertRMapEntry( OTF_MasterControl* mc,
	uint32_t value, uint32_t argument );


/* ************************************************************************** */


int OTF_MasterControl_init( OTF_MasterControl* mc ) {


	mc->n= 0;
	mc->s= 0;
	mc->map= NULL;
	
	mc->rn= 0;
	mc->rs= 0;
	mc->rmap= NULL;

	mc->manager= NULL;

	return 1;
}


void OTF_MasterControl_finalize( OTF_MasterControl* mc ) {


	uint32_t i;


	for ( i= 0; i < mc->n; i++ ) {

		free( mc->map[i].values );
	}
	
	free( mc->map );
	mc->map= NULL;
	free( mc->rmap );
	mc->rmap= NULL;
}


void OTF_MasterControl_reset( OTF_MasterControl* mc ) {


	uint32_t i;


	for ( i= 0; i < mc->n; i++ ) {

		free( mc->map[i].values );
	}
	
	free( mc->map );
	mc->n= 0;
	mc->s= 0;
	mc->map= NULL;

	free( mc->rmap );
	mc->rn= 0;
	mc->rs= 0;
	mc->rmap= NULL;
}


void OTF_MasterControl_finish( OTF_MasterControl* mc ) {


	OTF_MasterControl_close( mc );

	OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
			"OTF_MasterControl_finish() is deprecated, "
			"use OTF_MasterControl_close() instead.\n",
			__FUNCTION__, __FILE__, __LINE__ );
}


void OTF_MasterControl_close( OTF_MasterControl* mc ) {


	OTF_MasterControl_finalize( mc );

	free( mc );
	mc = NULL;
}


int OTF_MasterControl_read( OTF_MasterControl* mc, const char* namestub ) {


	char* filename;
	OTF_RBuffer* buffer;

	uint32_t argument;
	uint32_t value;

	int r;

	if( NULL == mc ) {
		
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"master control has not been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );
	
		return 0;
	}		

	/* finalize + init == reset */
	OTF_MasterControl_reset( mc );

	filename = OTF_getFilename( namestub,
		0 /* id */, 
		OTF_FILETYPE_MASTER /* type */,
		0, NULL );

	if ( NULL == filename ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_getFilename() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		
		return 0;
	}

/*
	if ( ! OTF_fileExists( filename ) ) {

		OTF_Error( "ERROR in '%s'.c: "
			"Invalid input file '%s'\n", __FUNCTION__, filename );
		return 0;
	}
*/

	buffer = OTF_RBuffer_open( filename, mc->manager );
	if( NULL == buffer ) {
		
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"could not open file \"%s\" for reading.\n",
				__FUNCTION__, __FILE__, __LINE__, filename );
		
		free( filename );
		filename = NULL;

		return 0;
	}
	
	OTF_RBuffer_setSize( buffer, 1024 );

	free( filename );
	filename = NULL;

	/* must not use getRecord here! */
	while ( OTF_RBuffer_guaranteeRecord( buffer ) ) {
	
		/*
		OTF_RBuffer_printRecord( buffer );
		*/

		/* IOFSL config line */
		if ( OTF_RBuffer_testChar( buffer, 'i' ) ) {
			uint32_t server_num;
			OTF_IofslMode mode;
			uint32_t streamid_bits;
			server_num = OTF_RBuffer_readUint32( buffer );
			if ( ! OTF_RBuffer_testChar( buffer, ':') ) {
				OTF_Error( "OTF_MasterControl_read() "
					"ERROR: missing expected character ':'" );
				free( buffer );
				return 0;
			}
			mode = (OTF_IofslMode)OTF_RBuffer_readUint32( buffer );
			if ( mode != OTF_IOFSL_MULTIFILE_SPLIT
				&& mode != OTF_IOFSL_MULTIFILE ) {
				OTF_Error( "OTF_MasterControl_read() "
					"ERROR: invalid IofslMode." );
				free( buffer );
				return 0;
			}

			if ( ! OTF_RBuffer_testChar( buffer, ':') ) {
				OTF_Error( "OTF_MasterControl_read() "
					"ERROR: missing expected character ':'" );
				free( buffer );
				return 0;
			}
			streamid_bits = OTF_RBuffer_readUint32( buffer );
			OTF_RBuffer_readNewline( buffer );

			OTF_FileManager_setIofsl( mc->manager,
					server_num, NULL, mode,
					0, 0, streamid_bits );
			continue;
		}

		/* read argument */
		argument= OTF_RBuffer_readUint32( buffer );
		if ( ! OTF_RBuffer_testChar( buffer, ':' ) ) {
		
			OTF_RBuffer_readNewline( buffer );
			continue;
		}

		do {

			value= OTF_RBuffer_readUint32( buffer );

			r= OTF_MasterControl_append( mc, argument, value );
			if ( 0 == r ) {
			
				OTF_Error( "OTF_MasterControl_read() "
						"ERROR: appending (%u,%u)\n",
						argument, value );
			}

		} while ( OTF_RBuffer_testChar( buffer, ',' ) );


		OTF_RBuffer_readNewline( buffer );
	}

	OTF_RBuffer_close( buffer );

	return 1;
}


OTF_MasterControl* OTF_MasterControl_new( OTF_FileManager* manager ) {


	OTF_MasterControl* ret;


	ret= (OTF_MasterControl*) malloc( sizeof(OTF_MasterControl) );
	if( NULL == ret ) {
		
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		
		return NULL;
	}

	OTF_MasterControl_init( ret );

	if( NULL == manager ) {
		
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"manager has not been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		free( ret );
		ret= NULL;
		
		return NULL;
	}
	ret->manager= manager;

	return ret;
}


OTF_MapEntry* OTF_MasterControl_insertMapEntry( OTF_MasterControl* mc,
		uint32_t argument, uint32_t pos ) {


	unsigned int i;


	/* realloc ? */
	if ( mc->n >= mc->s ) {
	
		mc->s= ( mc->s > 0 ) ? ( 2* mc->s ) : 10;
		mc->map= (OTF_MapEntry*) realloc( mc->map, 
			mc->s * sizeof(OTF_MapEntry) );
		if( NULL == mc->map ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );
		
			return NULL;
		}
	}

	/* move all entries at >= pos one up */
	for ( i= mc->n; i > pos; i-- ) {
	
		memcpy( &(mc->map[i]), &(mc->map[i-1]), sizeof(OTF_MapEntry) );
	}

	/* insert new entry */
	mc->map[pos].argument= argument;
	mc->map[pos].n= 0;
	mc->map[pos].s= 0;
	mc->map[pos].values= NULL;

	mc->n++;

	return &(mc->map[pos]);
}


OTF_MapEntry* OTF_MasterControl_getMapEntry( OTF_MasterControl* mc,
		uint32_t argument ) {


	unsigned int a= 0;
	unsigned int b= mc->n;
	unsigned int c;


	if ( 0 == mc->n ) {
	
		/* empty list, insert at 0 */

		return OTF_MasterControl_insertMapEntry( mc, argument, 0 );
	}


	if ( mc->map[a].argument > argument ) {
	
		/* insert at very beginning  */
	
		return OTF_MasterControl_insertMapEntry( mc, argument, 0 );
	}

	if ( mc->map[b-1].argument < argument ) {
	
		/* insert at very end  */
	
		return OTF_MasterControl_insertMapEntry( mc, argument, b );
	}


	if ( mc->map[a].argument == argument ) {
	
		/* found at a  */
	
		return &( mc->map[a] );
	}

	if ( mc->map[b-1].argument == argument ) {
	
		/* found at b */
	
		return &( mc->map[b-1] );
	}


	/* some where in [a,b), do binary search */


	while ( 1 ) {

		if ( b-1 == a ) {

			/* not found, insert at a+1 == b */
			return OTF_MasterControl_insertMapEntry( mc, argument, b );
		}

		c= ( a + b ) / 2;

		if ( mc->map[c].argument == argument ) {

			/* found at c */

			return &( mc->map[c] );
		}

		if ( mc->map[c].argument < argument ) {

			a= c;

		} else /* if ( mc->map[c].argument > argument ) */ {

			b= c;
		}
	}
}


int OTF_MapEntry_insertValue( OTF_MapEntry* mc, uint32_t value ) {


	unsigned int i;
	unsigned int j;
	unsigned int last;
	unsigned int middle;


	/* realloc ? */
	if ( mc->n >= mc->s ) {

		mc->s= ( mc->s > 0 ) ? ( 2* mc->s ) : 10;
		mc->values= (uint32_t*) realloc( mc->values,
			mc->s * sizeof(uint32_t) );
		if( NULL == mc->values ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );
		
			return 0;
		}
	}


	/* first entry */
	if ( mc->n == 0 ) {

		mc->values[0] = value;
		mc->n++;
		return 1;
	}

	/*	search sorted list binary,
		move remaining entries, insert*/

	i = 0;
	last = mc->n-1;

	while ( i < last+1 )	{

		middle = ( i + last ) / 2;

		if( mc->values[middle] < value ) {

			i = middle + 1;

		} else if( mc->values[middle] > value ) {

			last = middle - 1;

		} else {

			/* ERROR: value already there */
			return 0;
		}
	}

	/* move al entries at > i one up */
	for ( j= mc->n; j > i; j-- ) {

		mc->values[j]= mc->values[j-1];
	}

	mc->values[i]= value;

	mc->n++;

	return 1;
}


int OTF_MasterControl_insertRMapEntry( OTF_MasterControl* mc,
		uint32_t rargument, uint32_t rvalue ) {


	unsigned int i;
	unsigned int j;
	unsigned int last;
	unsigned int middle;


	/* realloc ? */
	if ( mc->rn >= mc->rs ) {

		mc->rs= ( mc->rs > 0 ) ? ( 2* mc->rs ) : 10;
		mc->rmap= (OTF_Pair*) realloc( mc->rmap,
			mc->rs * sizeof(OTF_Pair) );
		if( NULL == mc->rmap ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );
		
			return 0;
		}
	}

	/*	search sorted list binary, move remaining entries, insert*/

	i = 0;
	last = mc->rn-1;

	while ( i < last+1 )	{

		middle = ( i + last ) / 2;

		if ( mc->rmap[middle].argument < rargument ) {

			i = middle + 1;

		} else if( mc->rmap[middle].argument > rargument ) {

			last = middle - 1;

		} else {

			/* ERROR: value already there */
			return 0;
		}
	}

	/* move al entries at > i one up */
	for ( j= mc->rn; j > i; j-- ) {

		mc->rmap[j].argument= mc->rmap[j-1].argument;
		mc->rmap[j].value= mc->rmap[j-1].value;
	}

	mc->rmap[i].argument= rargument;
	mc->rmap[i].value= rvalue;

	mc->rn++;

	return 1;
}


int OTF_MasterControl_append( OTF_MasterControl* mc,
		uint32_t argument, uint32_t value ) {


	int ret;
	OTF_MapEntry* entry;


	if ( 0 == argument || ((uint32_t) -1) == argument ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"invalid argument value %x.\n",
				__FUNCTION__, __FILE__, __LINE__, argument );

		return 0;
	}

	/* insert into reverse mapping first, abort if value is
	already there in rmap. it is important to check rmap first! */
	ret= OTF_MasterControl_insertRMapEntry( mc, value, argument );

	if ( 0 == ret ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_MasterControl_insertRMapEntry() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		
		return 0;
	}


	entry = OTF_MasterControl_getMapEntry( mc, argument );
	if( NULL == entry ) {
		
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_MasterControl_getMapEntry() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}

	return OTF_MapEntry_insertValue( entry, value );
}


int OTF_MasterControl_appendList( OTF_MasterControl* mc, uint32_t argument,
	uint32_t l, uint32_t* values ) {


	uint32_t i;
	int ret= 1;


	for ( i= 0; i < l; i++ ) {

		ret&= OTF_MasterControl_append( mc, argument, values[i] );
	}

	return ret;
}


uint32_t OTF_MasterControl_mapReverse( OTF_MasterControl* mc, 
		uint32_t value ) {


	/* find value in reverse list */

	unsigned int a;
	unsigned int b;
	unsigned int c;

	a= 0;
	b= mc->rn;

	if ( 0 >= mc->rn ) {
	
		return 0;
	}

	if ( value == mc->rmap[a].argument ) {

		return mc->rmap[a].value;
	}

	if ( value == mc->rmap[b-1].argument ) {
		
		return mc->rmap[b-1].value;
	}

	while ( a < b ) {

		c= ( a + b ) / 2;

		if ( value == mc->rmap[c].argument ) {

			/* found */
			return mc->rmap[c].value;
		}

		if ( value < mc->rmap[c].argument ) {

			/* [a,c) */
			b= c;

		} else {

			/* [c+1,b) */
			a= c+1;
		}
	}

	return 0;
}



int OTF_MasterControl_write( OTF_MasterControl* mc, const char* namestub ) {


	char* filename= NULL;

	unsigned int i;
	unsigned int j;
	OTF_MapEntry* e; 
	OTF_WBuffer* buffer;


	filename= OTF_getFilename( namestub,
		0 /* id */, OTF_FILETYPE_MASTER /* type */,
		0, NULL );
	if( NULL == filename ) {
		
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_getFilename() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}

	buffer = OTF_WBuffer_open( filename, mc->manager );
	if ( NULL == buffer ) {
		
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_WBuffer_open( %s ) failed.\n",
				__FUNCTION__, __FILE__, __LINE__, filename );
		
		free( filename );

		return  0;
	}
	
	OTF_WBuffer_setSize( buffer, 1024 );

	free( filename );
	filename = NULL;


	for ( i= 0; i < mc->n; i++ ) {

		e= &(mc->map[i]);

		OTF_WBuffer_writeUint32( buffer, e->argument );
		OTF_WBuffer_writeChar( buffer, ':' );

		if ( 0 < e->n ) {

			OTF_WBuffer_writeUint32( buffer, e->values[0] );
		}

		for ( j= 1; j < e->n; j++ ) {

			OTF_WBuffer_writeChar( buffer, ',' );
			OTF_WBuffer_writeUint32( buffer, e->values[j] );
		}

		OTF_WBuffer_writeNewline( buffer );
	}

	if ( OTF_FileManager_isIofsl( mc->manager ) ) {
		uint32_t server_num;
		char ** server_list;
		OTF_IofslMode mode;
		uint32_t flags;
		uint32_t index_buffer_length;
		uint32_t streamid_bits;
		OTF_FileManager_getIofsl( mc->manager,
				&server_num, &server_list, &mode,
				&flags, &index_buffer_length, &streamid_bits );
		OTF_WBuffer_writeChar( buffer, 'i' );
		OTF_WBuffer_writeUint32( buffer, server_num );
		OTF_WBuffer_writeChar( buffer, ':' );
		OTF_WBuffer_writeUint32( buffer, (uint32_t)mode );
		OTF_WBuffer_writeChar( buffer, ':' );
		OTF_WBuffer_writeUint32( buffer, streamid_bits );
		OTF_WBuffer_writeNewline( buffer );
	}

	OTF_WBuffer_close( buffer );

	return 1;
}


int OTF_MasterControl_check( OTF_MasterControl* mc ) {


	unsigned int i;
	unsigned int j;
	OTF_MapEntry* e;


	/* check if everything is properly sorted */

	/* map sorted? */
	for ( i= 1; i < mc->n; i++ ) {

		if ( mc->map[i].argument <= mc->map[i-1].argument ) {

			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"map not sorted at %u.\n",
					__FUNCTION__, __FILE__, __LINE__, i );
			
			return 0;
		}
	}


	/* map[x] sorted? */
	for ( i= 0; i < mc->n; i++ ) {

		e= &(mc->map[i]);

		for ( j= 1; j < e->n; j++ ) {

			if ( e->values[j] <= e->values[j-1] ) {
		
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"map[%u] not sorted at %u.\n",
						__FUNCTION__, __FILE__, __LINE__, i, j );

				return 0;
			}
		}
	}

	/* rmap sorted */
	for ( i= 1; i < mc->rn; i++ ) {

		if ( mc->rmap[i].argument <= mc->rmap[i-1].argument ) {
	
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"rmap not sorted at %u.\n",
					__FUNCTION__, __FILE__, __LINE__, i );
			
			return 0;
		}
	}

	return 1;
}


void OTF_MasterControl_print( OTF_MasterControl* mc ) {


	unsigned int i;
	unsigned int j;
	OTF_MapEntry* e; 


	fprintf( stderr, "map:\n" );
	for ( i= 0; i < mc->n; i++ ) {

		e= &(mc->map[i]);

		fprintf( stderr, "%x: ", e->argument );

		for ( j= 0; j < e->n; j++ ) {

			fprintf( stderr, "%x ", e->values[j] );
		}

		fprintf( stderr, "\n" );
	}

	fprintf( stderr, "rmap:\n" );
	for ( i= 0; i < mc->rn; i++ ) {

		fprintf( stderr, "%x: %x\n", 
			mc->rmap[i].argument, mc->rmap[i].value );
	}
}


OTF_MapEntry* OTF_MasterControl_getEntry( OTF_MasterControl* mc, 
		uint32_t argument ) {


	uint32_t i;
	uint32_t last;
	uint32_t middle;


	if ( mc->n == 0 ) {

		return NULL;
	}

	i = 0;
	last = mc->n-1;

	while ( i < last+1 ) {

		middle = ( i + last ) / 2;

		if ( mc->map[middle].argument < argument ) {

			i = middle + 1;

		} else if ( mc->map[middle].argument > argument ) {

			last = middle - 1;

		} else {

			/* argument already there */
			return &( mc->map[middle] );
		}
	}

	/* argument not found */

	return NULL;
}


OTF_MapEntry* OTF_MasterControl_getEntryByIndex( OTF_MasterControl* mc, 
		uint32_t index ) {


	if ( index < mc->n ) {
	
		return &( mc->map[index] );
	}
	
	return NULL;
}



OTF_Pair* OTF_MasterControl_getREntryByIndex( OTF_MasterControl* mc, 
		uint32_t index ) {


	if ( index < mc->rn ) {
	
		return &( mc->rmap[index] );
	}
	
	return NULL;
}


uint32_t OTF_MasterControl_getCount( OTF_MasterControl* mc ) {


	return mc->n;
}

uint32_t OTF_MasterControl_getrCount( OTF_MasterControl* mc ) {


	return mc->rn;
}


uint32_t OTF_MasterControl_getValueCount( OTF_MasterControl* mc,
		uint32_t argument ) {


	OTF_MapEntry* entry= OTF_MasterControl_getEntry( mc, argument );
	
	if ( NULL != entry ) {

		return entry->n;	
	}

	return 0;
}


uint32_t* OTF_MasterControl_getValues( OTF_MasterControl* mc, 
		uint32_t argument ) {


	uint32_t i;
	uint32_t last;
	uint32_t middle;


	if ( mc->n == 0 ) {

		return NULL;
	}

	i = 0;
	last = mc->n-1;

	while ( i < last+1 ) {

		middle = ( i + last ) / 2;

		if( mc->map[middle].argument < argument ) {

			i = middle + 1;

		} else if ( mc->map[middle].argument > argument ) {

			last = middle - 1;

		} else {

			return mc->map[middle].values;
		}
	}

	/* argument not found */

	return NULL;
}


uint32_t OTF_MasterControl_getNewStreamId( OTF_MasterControl* mc ) {


	uint32_t n= mc->n;

	/* return smallest possible stream id by default, 0 is reserved */
	uint32_t ret= 1;


	if ( 0 < n ) {

		/* increment the current maximum argument by 1 */
		ret= mc->map[n-1].argument +1;
		
		/* catch the worst case where the 0 and (-1) are 
		already taken */
		
		while ( NULL != OTF_MasterControl_getEntry(mc, ret ) || 
				0 == ret || ((uint32_t) -1) == ret ) {
		
			++ret;
		}
	}

	return ret;
}


OTF_MasterControl* OTF_MasterControl_clone( OTF_MasterControl* mc,
		OTF_FileManager* manager ) {


	int ret= 1;
	uint32_t i;
	OTF_MasterControl* mc_clone;

	mc_clone= OTF_MasterControl_new( manager );
	if ( !mc_clone )
		return NULL;

	for ( i= 0; i < mc->n; i++ ) {
		OTF_MapEntry* entry= &mc->map[i];
		ret = ret && OTF_MasterControl_appendList( mc_clone,
				entry->argument, entry->n, entry->values );
	}

	if ( !ret ) {
		OTF_MasterControl_close( mc_clone );
		return NULL;
	}

	return mc_clone;
}
