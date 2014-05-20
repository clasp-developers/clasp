/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include <assert.h>

#include "OTF_Platform.h"
#include "OTF_Reader.h"
#include "OTF_RStream.h"
#include "OTF_Parse.h"
#include "OTF_Errno.h"

/* #include "OTF_Keywords.h" */

#include "OTF_Keywords.h"

#define HEAP_CHILDRENCOUNT 2


/* *** OTF_ProcessList headers *** ********************** */


struct struct_OTF_ProcessList {

	/** number of processes == dimension of arrays below */
	uint32_t n;

	/** sorted array of process ids */
	uint32_t* processes;

	/** array of process status values accociated to the process at same index 
	in array 'processes'. 
	status values: 0 - unknown or disabled, 1 - enabled */
	uint8_t* status;
};
typedef struct struct_OTF_ProcessList OTF_ProcessList;


/** OTF_ProcessList constructor */
int OTF_ProcessList_init( OTF_ProcessList* list, OTF_MasterControl* mc );

/** OTF_ProcessList destructor */
void OTF_ProcessList_finalize( OTF_ProcessList* list );


uint32_t OTF_ProcessList_getIndex( OTF_ProcessList* list, uint32_t pid );

int OTF_ProcessList_setStatus( OTF_ProcessList* list, uint32_t pid, uint8_t status );

int OTF_ProcessList_setStatusAll( OTF_ProcessList* list, uint8_t status );

uint8_t OTF_ProcessList_getStatus( OTF_ProcessList* list, uint32_t pid );


/* *** OTF_Heap headers *** ***************************** */


/** struct managing a sorted heap (a la heap sort) of OTF_RBuffers that belong 
to a common query */
struct struct_OTF_Heap {

	/** number of current entries */
	uint32_t n;
	
	/** array dimension s <= n */
	uint32_t s;

	/** array of OTF_RBuffers, during querying they are sorted according to 
	each buffers next time stamp */
	OTF_RBuffer** buffers;

	/** experimental. minimim, maximum and current progress value(*) of all buffers 
	in that heap, cut by the readers global setting(*).
	(*) progress value are time stamps or bytes */
	uint64_t progressMin;
	uint64_t progressMax;
	uint64_t progressCurrent;
	
	uint64_t bytesMax;
	uint64_t bytesMin;
	uint64_t bytesCurrent;
	uint64_t bytesDone;
};
typedef struct struct_OTF_Heap OTF_Heap;


/** initialize heap. taking RBuffers from the given  master control object, keep only 
that streams that have at least one process enabled. 'which' decides whether to take
definition buffer, event buffer, etc. from every stream */
int OTF_Heap_initEventHeap( OTF_Heap* heap, OTF_Reader* reader );
int OTF_Heap_initDefHeap( OTF_Heap* heap, OTF_Reader* reader );
int OTF_Heap_initStatisticsHeap( OTF_Heap* heap, OTF_Reader* reader );
int OTF_Heap_initSnapshotsHeap( OTF_Heap* heap, OTF_Reader* reader );
int OTF_Heap_initMarkerHeap( OTF_Heap* heap, OTF_Reader* reader );

void OTF_Heap_finalize( OTF_Heap* heap );

/** initial (full) sort operation */
int OTF_Heap_sort( OTF_Heap* heap );

/** do a fast re-sort after only the first entrie's sort key changed */
int OTF_Heap_resort( OTF_Heap* heap );

int OTF_Heap_checksorted( OTF_Heap* heap );


/* *** OTF_Reader headers ******************************* */


struct struct_OTF_Reader {

	/**	name stub: all files will begin with this name */
	char* namestub;

	/**	Number of streams. */
	uint32_t n;

	/**	List of stream sorted by stream id. */
	OTF_RStream** stream;

	OTF_ProcessList* processList;

	OTF_Heap* definitionHeap;
	OTF_Heap* eventHeap;
	OTF_Heap* snapshotsHeap;
	OTF_Heap* statisticsHeap;
	OTF_Heap* markerHeap;


	/**	Master control structure. */
	OTF_MasterControl* mc;

	/**	Contain the minimum time stamp where to start reading. */
	uint64_t minTime;

	/**	Contain the maximum time stamp where to stop reading. */
	uint64_t maxTime;

	/** maximum number of records delivered by a single call to OTF_Reader_readXYZ()
	defaults to OTF_READ_MAXRECORDS == \infty */
	uint64_t recordLimit;

	/** Default size of buffers managed by this Reader. */
	uint32_t buffersizes;

#ifdef HAVE_ZLIB
	/** Default size of zbuffers managed by this reader. */
	uint32_t zbuffersizes;
#endif /* HAVE_ZLIB */
	
	/** file handle manager */
	OTF_FileManager* manager;
};


/* *** reader functions macros *** *************************************** */

/** constructor - internal use only */
int OTF_Reader_init( OTF_Reader* reader );

/** destructor - internal use only */
int OTF_Reader_finish( OTF_Reader* reader );

/** dump existing OTF_Heap members. this is necessary after 
changing time interval or process selection */
void OTF_Reader_resetHeaps( OTF_Reader* reader );



/* *** ProcessList functions ****************** */


int OTF_ProcessList_init( OTF_ProcessList* list, OTF_MasterControl* mc ) {


		uint32_t i;
		uint32_t n;
		OTF_Pair* pair;

#		ifdef OTF_DEBUG
			uint8_t test;
#		endif

		n= OTF_MasterControl_getrCount( mc );
		if ( 0 >= n ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"no process has been defined in the master control.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return 0;
		}

		list->n= n;

		list->processes= (uint32_t*) malloc( n * sizeof(uint32_t) );
		if( NULL == list->processes ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return 0;
		}

		list->status= (uint8_t*) malloc( n * sizeof(uint8_t) );
		if( NULL == list->status ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			free( list->processes );
			list->processes= NULL;
			
			return 0;
		}

		/* put process entries to 'processes' sorted. */
		for ( i= 0; i < n; i++ ) {

			pair= OTF_MasterControl_getREntryByIndex( mc, i );
			if( NULL == pair ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_MasterControl_getREntryByIndex() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );

				free( list->processes );
				list->processes= NULL;
				free( list->status );
				list->status= NULL;
				
				return 0;
			}

			list->processes[i]= pair->argument;

			/* set to enabled status by default */
			list->status[i]= 1;
		}

#		ifdef OTF_DEBUG
			/* check if list->processes[i] is sorted like it is suppossed to be */
			test= 1;
			for ( i= 1; i < n; i++ ) {
			
				if ( list->processes[i] < list->processes[i-1] ) {
				
					test= 0;
				}
			}
	
			if ( 1 != test ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"process list from MasterControl is not sorted.\n",
					__FUNCTION__, __FILE__, __LINE__ );
				
				free( list->processes );
				list->processes= NULL;
				free( list->status );
				list->status= NULL;
				
				return 0;
			}
#		endif

		return 1;
}


void OTF_ProcessList_finalize( OTF_ProcessList* list ) {


	list->n= 0;

	if( list->processes != NULL ) {
		free( list->processes );
		list->processes= NULL;
	}

	if( list->status != NULL ) {
		free( list->status );
		list->status= NULL;
	}

}


uint32_t OTF_ProcessList_getIndex( OTF_ProcessList* list, uint32_t pid ) {


	uint32_t a= 0;
	uint32_t b= list->n -1;
	uint32_t c;


	if ( pid < list->processes[a] ) {

		/* not found */
		return (uint32_t) -1;
	}

	if ( pid > list->processes[b] ) {

		/* not found */
		return (uint32_t) -1;
	}

	if ( pid == list->processes[a] ) {
	
		return a;
	}

	if ( pid == list->processes[b] ) {
	
		return b;
	}

	while ( a +1 < b ) {

		c= ( a + b ) / 2;

		if ( pid == list->processes[c] ) {
		
			return c;
		}

		if ( pid < list->processes[c] ) {
		
			b= c;

		} else {
		
			a= c;
		}
	}

	/* not found */
	return (uint32_t) -1;
}


int OTF_ProcessList_setStatus( OTF_ProcessList* list, uint32_t pid, uint8_t status ) {
	

	uint32_t index= OTF_ProcessList_getIndex( list, pid );


	if ( index < list->n ) {
	
		list->status[index]= status;

		/* success */
		return 1;

	} else {

		/* pid not found, error */
		return 0;
	}
}


int OTF_ProcessList_setStatusAll( OTF_ProcessList* list, uint8_t status ) {


	uint32_t i;


	for ( i= 0; i < list->n; i++ ) {

		list->status[i]= status;
	}

	return 1;
}


uint8_t OTF_ProcessList_getStatus( OTF_ProcessList* list, uint32_t pid ) {


	uint32_t index= OTF_ProcessList_getIndex( list, pid );


	if ( index < list->n ) {

		/* success */
		return list->status[index];

	}

	/* pid not found, error */
	return 0;
}


/* *** OTF_Heap functions *** ***************** */


/** initialize heap. taking RBuffers from the given  master control object, keep only 
that streams that have at least one process enabled. 'which' decides whether to take
definition buffer, event buffer, etc. from every stream */
int OTF_Heap_initEventHeap( OTF_Heap* heap, OTF_Reader* reader ) {


	uint32_t i;
	uint32_t j;
	uint8_t enabled;
	OTF_MapEntry* entry;
	OTF_RStream* stream;
	OTF_RBuffer* buffer;


	heap->n= 0;
	heap->s= OTF_MasterControl_getCount( reader->mc );

	heap->buffers= (OTF_RBuffer**) malloc( heap->s * sizeof(OTF_RBuffer*) );
	if( NULL == heap->buffers ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );
	
		return 0;
	}

	for ( i= 0; i < heap->s; i++ ) {
	
		entry= OTF_MasterControl_getEntryByIndex( reader->mc, i );
		if( NULL == entry ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_MasterControl_getEntryByIndex() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
		
			free( heap->buffers );
			heap->buffers= NULL;
			
			return 0;
		}

		enabled= 0;
		j= 0;
		while ( ( j < entry->n ) && ( 0 == enabled ) ) {

			enabled= enabled || OTF_ProcessList_getStatus( reader->processList,
			entry->values[j] );
			j++;
		}

		if ( enabled ) {

			stream= OTF_Reader_getStream( reader, entry->argument );
			if( NULL == stream ) {
	
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"cannot get stream '%llu'\n",
						__FUNCTION__, __FILE__, __LINE__,
						(long long unsigned) entry->argument );

				free( heap->buffers );
				heap->buffers= NULL;
		
				return 0;
			}

			buffer= OTF_RStream_getEventBuffer( stream );
			if ( NULL != buffer ) {

				heap->buffers[ heap->n ]= buffer;
				(heap->n)++;

			} else {

				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"cannot open event buffer of stream %u.\n",
						__FUNCTION__, __FILE__, __LINE__, entry->argument );

				free( heap->buffers );
				heap->buffers= NULL;

				return 0;
			}
		}
	}

	heap->progressMin= 0;
	heap->progressMax= (uint64_t) -1;
	heap->progressCurrent= 0;

	return 1;
}


/** initialize heap. taking RBuffers from the given  master control object, keep only 
that streams that have at least one process enabled. 'which' decides whether to take
definition buffer, event buffer, etc. from every stream */
int OTF_Heap_initDefHeap( OTF_Heap* heap, OTF_Reader* reader ) {


	uint32_t i;
	uint32_t j;
	uint8_t enabled;
	OTF_MapEntry* entry;
	OTF_RStream* stream;
	OTF_RBuffer* buffer;


	heap->n= 0;
	heap->s= 1 + OTF_MasterControl_getCount( reader->mc );

	heap->buffers= (OTF_RBuffer**) malloc( heap->s * sizeof(OTF_RBuffer*) );
	if( NULL == heap->buffers ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}

	/* stream 0 */
	stream= OTF_Reader_getStream( reader, 0 );
	if( NULL != stream ) {
	
		buffer= OTF_RStream_getDefBuffer( stream );
		if( NULL != buffer ) {
		
			/* buffer == NULL allowed! */
			heap->buffers[ heap->n ]= buffer;
			(heap->n)++;
		}
	}

	/* remaining streams */

	for ( i= 0; i < heap->s -1; i++ ) {
	
		entry= OTF_MasterControl_getEntryByIndex( reader->mc, i );
		if( NULL == entry ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_MasterControl_getEntryByIndex() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			free( heap->buffers );
			heap->buffers= NULL;
			
			return 0;
		}

		enabled= 0;
		j= 0;
		while ( ( j < entry->n ) && ( 0 == enabled ) ) {

			enabled= enabled || OTF_ProcessList_getStatus( reader->processList,	entry->values[j] );
			j++;
		}

		if ( enabled ) {

			stream= OTF_Reader_getStream( reader, entry->argument );
			if( NULL == stream ) {
	
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"cannot get stream '%llu'\n",
						__FUNCTION__, __FILE__, __LINE__,
						(long long unsigned) entry->argument );

				free( heap->buffers );
				heap->buffers= NULL;
		
				return 0;
			}

			buffer= OTF_RStream_getDefBuffer( stream );

			/* buffer == NULL allowed! */
			heap->buffers[ heap->n ]= buffer;
			(heap->n)++;
		}
	}

	return 1;
}


/** initialize heap. taking RBuffers from the given  master control object, keep only 
that streams that have at least one process enabled. 'which' decides whether to take
definition buffer, event buffer, etc. from every stream */
int OTF_Heap_initStatisticsHeap( OTF_Heap* heap, OTF_Reader* reader ) {


	uint32_t i;
	uint32_t j;
	uint8_t enabled;
	OTF_MapEntry* entry;
	OTF_RStream* stream;
	OTF_RBuffer* buffer;


	heap->n= 0;
	heap->s= OTF_MasterControl_getCount( reader->mc );

	heap->buffers= (OTF_RBuffer**) malloc( heap->s * sizeof(OTF_RBuffer*) );
	if( NULL == heap->buffers ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}

	for ( i= 0; i < heap->s; i++ ) {
	
		entry= OTF_MasterControl_getEntryByIndex( reader->mc, i );
		if( NULL == entry ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_MasterControl_getEntryByIndex() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			free( heap->buffers );
			heap->buffers= NULL;
			
			return 0;
		}

		enabled= 0;
		j= 0;
		while ( ( j < entry->n ) && ( 0 == enabled ) ) {

			enabled= enabled || OTF_ProcessList_getStatus( reader->processList,
			entry->values[j] );
			j++;
		}

		if ( enabled ) {

			stream= OTF_Reader_getStream( reader, entry->argument );
			if( NULL == stream ) {
	
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"cannot get stream '%llu'\n",
						__FUNCTION__, __FILE__, __LINE__,
						(long long unsigned) entry->argument );

				free( heap->buffers );
				heap->buffers= NULL;
		
				return 0;
			}

			buffer= OTF_RStream_getStatsBuffer( stream );
			if ( NULL != buffer ) {

				heap->buffers[ heap->n ]= buffer;
				(heap->n)++;

			}
		}
	}

	heap->progressMin= 0;
	heap->progressMax= (uint64_t) -1;
	heap->progressCurrent= 0;

	return 1;
}


/** initialize heap. taking RBuffers from the given  master control object, keep only 
that streams that have at least one process enabled. 'which' decides whether to take
definition buffer, event buffer, etc. from every stream */
int OTF_Heap_initSnapshotsHeap( OTF_Heap* heap, OTF_Reader* reader ) {


	uint32_t i;
	uint32_t j;
	uint8_t enabled;
	OTF_MapEntry* entry;
	OTF_RStream* stream;
	OTF_RBuffer* buffer;


	heap->n= 0;
	heap->s= OTF_MasterControl_getCount( reader->mc );

	heap->buffers= (OTF_RBuffer**) malloc( heap->s * sizeof(OTF_RBuffer*) );
	if( NULL == heap->buffers ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}

	for ( i= 0; i < heap->s; i++ ) {
	
		entry= OTF_MasterControl_getEntryByIndex( reader->mc, i );
		if( NULL == entry ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_MasterControl_getEntryByIndex() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			free( heap->buffers );
			heap->buffers= NULL;
			
			return 0;
		}

		enabled= 0;
		j= 0;
		while ( ( j < entry->n ) && ( 0 == enabled ) ) {

			enabled= enabled || OTF_ProcessList_getStatus( reader->processList,
			entry->values[j] );
			j++;
		}

		if ( enabled ) {

			stream= OTF_Reader_getStream( reader, entry->argument );
			if( NULL == stream ) {
	
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"cannot get stream '%llu'\n",
						__FUNCTION__, __FILE__, __LINE__,
						(long long unsigned) entry->argument );

				free( heap->buffers );
				heap->buffers= NULL;
		
				return 0;
			}

			buffer= OTF_RStream_getSnapsBuffer( stream );
			if ( NULL != buffer ) {

				heap->buffers[ heap->n ]= buffer;
				(heap->n)++;

			}
		}
	}

	heap->progressMin= 0;
	heap->progressMax= (uint64_t) -1;
	heap->progressCurrent= 0;

	return 1;
}


/** initialize heap. taking RBuffers from the given  master control object, keep only 
that streams that have at least one process enabled. 'which' decides whether to take
definition buffer, event buffer, etc. from every stream */
int OTF_Heap_initMarkerHeap( OTF_Heap* heap, OTF_Reader* reader ) {


	uint32_t i;
	uint32_t j;
	uint8_t enabled;
	OTF_MapEntry* entry;
	OTF_RStream* stream;
	OTF_RBuffer* buffer;


	heap->n= 0;
	heap->s= 1 + OTF_MasterControl_getCount( reader->mc );

	heap->buffers= (OTF_RBuffer**) malloc( heap->s * sizeof(OTF_RBuffer*) );
	if( NULL == heap->buffers ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}

	/* stream 0 */
	stream= OTF_Reader_getStream( reader, 0 );
	if( NULL != stream ) {
	
		buffer= OTF_RStream_getMarkerBuffer( stream );
		if( NULL != buffer ) {
		
			/* buffer == NULL allowed! */
			heap->buffers[ heap->n ]= buffer;
			(heap->n)++;
		}
	}

	/* remaining streams */

	for ( i= 0; i < heap->s -1; i++ ) {
	
		entry= OTF_MasterControl_getEntryByIndex( reader->mc, i );
		if( NULL == entry ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_MasterControl_getEntryByIndex() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			free( heap->buffers );
			heap->buffers= NULL;
			
			return 0;
		}

		enabled= 0;
		j= 0;
		while ( ( j < entry->n ) && ( 0 == enabled ) ) {

			enabled= enabled || OTF_ProcessList_getStatus( reader->processList,	entry->values[j] );
			j++;
		}

		if ( enabled ) {

			stream= OTF_Reader_getStream( reader, entry->argument );
			if( NULL == stream ) {
	
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"cannot get stream '%llu'\n",
						__FUNCTION__, __FILE__, __LINE__,
						(long long unsigned) entry->argument );

				free( heap->buffers );
				heap->buffers= NULL;
		
				return 0;
			}

			buffer= OTF_RStream_getMarkerBuffer( stream );

			/* buffer == NULL allowed! */
			heap->buffers[ heap->n ]= buffer;
			(heap->n)++;
		}
	}

	return 1;
}


void OTF_Heap_finalize( OTF_Heap* heap ) {


	heap->n= 0;
	heap->s= 0;

	free( heap->buffers );
	heap->buffers= NULL;
}


int OTF_Heap_sort( OTF_Heap* heap ) {


	int32_t i;
	int32_t j;
	int32_t v;
	int32_t w;
	int32_t k;
	OTF_RBuffer* tmp;
	
	if( 0 == heap->n ) {
	
		return 1;
	}

	/* calculate the first node to begin with */
	i= ( heap->n - 1 ) / HEAP_CHILDRENCOUNT;


	/* establish a sorted heap ;) */
	for ( ; i >= 0; i-- ) {

		/* first descendant of v */
		v = i;
		w = HEAP_CHILDRENCOUNT * v + 1;

		while ( w < (int32_t) heap->n ) {

			/* pick the lowest descendant */
			for( j= 1, k= 0; j < HEAP_CHILDRENCOUNT; j++ ) {
			
				if ( w + j >= (int32_t) heap->n ) {
					break;
				}
	
				if ( heap->buffers[w+j]->time < heap->buffers[w+k]->time ) {
		
					k= j;
				}
			}
			w = w+k;
			
			if ( heap->buffers[v]->time < heap->buffers[w]->time ) {

				/* v has heap property, exit loop */
				break;
			}

			/* swap entries */
			tmp = heap->buffers[v];
			heap->buffers[v] = heap->buffers[w];
			heap->buffers[w] = tmp;

			/* continue further down the heap */
			v = w;
			w = HEAP_CHILDRENCOUNT * v + 1;
		}
	}
	
	return 1;
}


int OTF_Heap_resort( OTF_Heap* heap ) {


	uint32_t v;
	uint32_t w;
	uint32_t j;
	uint32_t k;
	OTF_RBuffer* tmp;
	

	/* assume a sorted heap except for the top element */

	/* follow the first item down the heap */
	v = 0;
	w = 1;
	
	while ( w < heap->n ) {
		
		/* pick the lowest descendant */
		for( j= 1, k= 0; j < HEAP_CHILDRENCOUNT; j++ ) {
		
			if ( w + j >= heap->n ) {
				break;
			}

			if ( heap->buffers[w+j]->time < heap->buffers[w+k]->time ) {
	
				k= j;
			}
		}
		w = w+k;
		
		
		
		if ( heap->buffers[v]->time < heap->buffers[w]->time ) {

			/* v has heap property */
			break;
		}
		
		/* swap entries */
		tmp = heap->buffers[v];
		heap->buffers[v] = heap->buffers[w];
		heap->buffers[w] = tmp;

		/* continue */
		v = w;
		w = HEAP_CHILDRENCOUNT * v + 1;
	}
	
	return 1;
}


int OTF_Heap_checksorted( OTF_Heap* heap ) {


	int ret= 1;
	uint32_t i;
	int j;


	/*
	fprintf( stderr, "%u-heap with %u entries:\n", HEAP_CHILDRENCOUNT, heap->n );
	for ( i= 0; i < heap->n; i++ ) {

		fprintf( stderr, "%i: %llu\n", i, heap->buffers[i]->time );
	}
	*/


	/* all but the top entry need to be >= their parent entry */
	for ( i= 1; i < heap->n; i++ ) {

		j= ( i -1 ) / HEAP_CHILDRENCOUNT;

		ret = ret && ( heap->buffers[j]->time <= heap->buffers[i]->time );
	}

	assert( 1 == ret );

	return ret;
}


/* *** Reader functions *********************** */


int OTF_Reader_init( OTF_Reader* reader ) {

	reader->namestub= NULL;

	reader->n= 0;
	reader->stream= NULL;

	reader->processList= NULL;

	reader->definitionHeap= NULL;
	reader->eventHeap= NULL;
	reader->snapshotsHeap= NULL;
	reader->statisticsHeap= NULL;
	reader->markerHeap= NULL;

	reader->mc= NULL;

	reader->minTime= 0;
	reader->maxTime= (uint64_t) -1;

	reader->recordLimit= OTF_READ_MAXRECORDS;

	reader->buffersizes= 1024*1024;
	
#ifdef HAVE_ZLIB
	reader->zbuffersizes= OTF_ZBUFFER_DEFAULTSIZE;
#endif /* HAVE_ZLIB */
	
	reader->manager= NULL;
	return 1;
}


int OTF_Reader_finish( OTF_Reader* reader ) {


	int ret= 1;
	
	free( reader->namestub );
	reader->namestub= NULL;

	ret&= OTF_Reader_closeAllStreams( reader );

	free( reader->stream );
	reader->stream = NULL;


	if ( NULL != reader->processList ) {

		OTF_ProcessList_finalize( reader->processList );
		free( reader->processList );
		reader->processList= NULL;
	}
	
	if ( NULL != reader->definitionHeap ) { 

		OTF_Heap_finalize( reader->definitionHeap ); 
		free( reader->definitionHeap );
		reader->definitionHeap= NULL;
	}
	if ( NULL != reader->eventHeap ) { 

		OTF_Heap_finalize( reader->eventHeap ); 
		free( reader->eventHeap );
		reader->eventHeap= NULL;
	}
	if ( NULL != reader->snapshotsHeap ) { 

		OTF_Heap_finalize( reader->snapshotsHeap );
		free( reader->snapshotsHeap );
		reader->snapshotsHeap= NULL;
	}
	if ( NULL != reader->statisticsHeap ) { 

		OTF_Heap_finalize( reader->statisticsHeap );
		free( reader->statisticsHeap );
		reader->statisticsHeap= NULL;
	}
	if ( NULL != reader->markerHeap ) { 

		OTF_Heap_finalize( reader->markerHeap ); 
		free( reader->markerHeap );
		reader->markerHeap= NULL;
	}

	if ( NULL != reader->mc ) {
		OTF_MasterControl_close( reader->mc );
		reader->mc = NULL;
	}

	return ret;
}


void OTF_Reader_resetHeaps( OTF_Reader* reader ) {


	if ( NULL != reader->definitionHeap ) { 

		OTF_Heap_finalize( reader->definitionHeap ); 
		free( reader->definitionHeap );
		reader->definitionHeap= NULL;
	}
	if ( NULL != reader->eventHeap ) { 

		OTF_Heap_finalize( reader->eventHeap ); 
		free( reader->eventHeap );
		reader->eventHeap= NULL;
	}
	if ( NULL != reader->snapshotsHeap ) { 

		OTF_Heap_finalize( reader->snapshotsHeap );
		free( reader->snapshotsHeap );
		reader->snapshotsHeap= NULL;
	}
	if ( NULL != reader->statisticsHeap ) { 

		OTF_Heap_finalize( reader->statisticsHeap );
		free( reader->statisticsHeap );
		reader->statisticsHeap= NULL;
	}
	if ( NULL != reader->markerHeap ) { 

		OTF_Heap_finalize( reader->markerHeap ); 
		free( reader->markerHeap );
		reader->markerHeap= NULL;
	}
}


OTF_Reader* OTF_Reader_open( const char* namestub, OTF_FileManager* manager ) {


	OTF_Reader* ret;
	int read;


	ret = (OTF_Reader*) malloc( sizeof( OTF_Reader ) );
	if( NULL == ret ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return NULL;
	}

	OTF_Reader_init( ret );

	ret->namestub = OTF_stripFilename( namestub );

	if( NULL == manager ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"manager has not been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		OTF_Reader_finish(ret);
		free( ret );
		ret= NULL;

		return NULL;
	}
	ret->manager=manager;

	ret->mc= OTF_MasterControl_new( manager );
	if( NULL == ret->mc ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_MasterControl_new() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		OTF_Reader_finish(ret);
		free( ret );
		ret= NULL;

		return NULL;
	}
	
	
	read= OTF_MasterControl_read( ret->mc, ret->namestub );
	if( 0 == read ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_MasterControl_read() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		OTF_Reader_finish(ret);
		free( ret );
		ret= NULL;

		return NULL;
	}

	ret->processList= (OTF_ProcessList*) malloc( sizeof(OTF_ProcessList) );
	if( NULL == ret->processList ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		OTF_Reader_finish(ret);
		free( ret );
		ret= NULL;
		
		return NULL;
	}
	ret->processList->status = NULL;
	ret->processList->processes = NULL;
	
	if( 0 == OTF_ProcessList_init( ret->processList, ret->mc ) ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_ProcessList_init() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		
		OTF_Reader_finish(ret);
		free( ret );
		ret= NULL;
	
		return NULL;
	}

	return ret;
}


int OTF_Reader_close( OTF_Reader* reader ) {


	int ret=1;
	if( NULL == reader ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no reader has been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );
	
		return 0;
	}

	ret&= OTF_Reader_finish( reader );

	free( reader );
	reader = NULL;

	return ret;
}

int OTF_Reader_setBufferSizes( OTF_Reader* reader, uint32_t size ) {


	if ( 50 > size ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"intended buffer size %u is too small, rejected.\n",
				__FUNCTION__, __FILE__, __LINE__, size );
		
		return 0;

	} else if ( 500 > size ) {
	
		OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				"buffer size %u is very small, accepted though.\n",
				__FUNCTION__, __FILE__, __LINE__, size );

	} else if ( 10 * 1024 *1024 < size ) {

		OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				"buffer size %u is rather big, accepted though.\n",
				__FUNCTION__, __FILE__, __LINE__, size );

	}

	reader->buffersizes= size;

	return 1;
}


uint32_t OTF_Reader_getBufferSizes( OTF_Reader* reader ) {


	return reader->buffersizes;
}


void OTF_Reader_setZBufferSizes( OTF_Reader* reader, uint32_t size ) {


#ifdef HAVE_ZLIB
	
	if ( 32 > size ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"intended zbuffer size %u is too small, rejected.\n",
				__FUNCTION__, __FILE__, __LINE__, size );
		
		return;

	} else if ( 512 > size ) {
	
		OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				"buffer size %u is very small, accepted though.\n",
				__FUNCTION__, __FILE__, __LINE__, size );

	} else if ( 10 * 1024 *1024 < size ) {

		OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				"buffer size %u is rather big, accepted though.\n",
				__FUNCTION__, __FILE__, __LINE__, size );
	}

	reader->zbuffersizes= size;
	
#endif /* HAVE_ZLIB */
}


uint32_t OTF_Reader_getZBufferSizes( OTF_Reader* reader ) {


#ifdef HAVE_ZLIB
	return reader->zbuffersizes;
#else /* HAVE_ZLIB */
	return 0;
#endif /* HAVE_ZLIB */
}


void OTF_Reader_reset( OTF_Reader* reader ) {


	OTF_Reader_resetHeaps( reader );

	/* reset time interval */
	OTF_Reader_setTimeInterval( reader, 0, (uint64_t) -1 );

	/* reset all processes to enabled */
	OTF_Reader_setProcessStatusAll( reader, 1 );

	/* reset record count limit to \infty */
	OTF_Reader_setRecordLimit( reader, OTF_READ_MAXRECORDS );
}


OTF_RStream* OTF_Reader_getStream( OTF_Reader* reader, uint32_t id ) {


	uint32_t i;
	uint32_t begin;
	uint32_t middle;
	uint32_t end;


	/* search list of stream for the given
	stream id  - binary search */

	begin = 0;
	end = reader->n-1;

	while ( begin < end+1 ) {

		middle = ( begin + end ) / 2;

		if ( reader->stream[middle]->id < id ) {

			begin = middle + 1;

		} else if ( reader->stream[middle]->id > id ) {

			end = middle - 1;

		} else {

			/* stream already there */
			return reader->stream[middle];
		}
	}

	/* add a new stream */
	++(reader->n);

	/* allocate  memory */
	reader->stream = (OTF_RStream**) realloc( reader->stream ,
		reader->n * sizeof( OTF_RStream* ) );
	if( NULL == reader->stream ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return NULL;
	}

	/* move all stream > i one up */
	for ( i = reader->n-1; i > begin; --i ) {

		reader->stream[i] = reader->stream[i-1];
	}

	reader->stream[i] = OTF_RStream_open( reader->namestub, id, reader->manager );

	OTF_RStream_setBufferSizes( reader->stream[i], reader->buffersizes );
	
#ifdef HAVE_ZLIB
	OTF_RStream_setZBufferSizes( reader->stream[i], reader->zbuffersizes );
#endif /* HAVE_ZLIB */
	
	return reader->stream[i];
}


int OTF_Reader_disableProcess( OTF_Reader* reader, uint32_t processId ) {


	return OTF_ProcessList_setStatus( reader->processList, processId, 0 );
}


int OTF_Reader_enableProcess( OTF_Reader* reader, uint32_t processId ) {


	return OTF_ProcessList_setStatus( reader->processList, processId, 1 );
}


/** get process status, '1' for enabled, '0' for disabled or unknown */
uint8_t OTF_Reader_getProcessStatus( OTF_Reader* reader, uint32_t processId ) {


	return OTF_ProcessList_getStatus( reader->processList, processId );
}

/** nicer equivalent to 'OTF_Reader_disableProcess()' and 'OTF_Reader_enableProcess()'.
status is '0' for disabled and '1' for enabled */
int OTF_Reader_setProcessStatus( OTF_Reader* reader, uint32_t processId, uint8_t status ) {


	OTF_Reader_resetHeaps( reader );

	return OTF_ProcessList_setStatus( reader->processList, processId, status );
}


int OTF_Reader_setProcessStatusAll( OTF_Reader* reader, uint8_t status ) {


	OTF_Reader_resetHeaps( reader );

	return OTF_ProcessList_setStatusAll( reader->processList, status );
}


uint64_t OTF_Reader_readDefinitions( OTF_Reader* reader, 
		OTF_HandlerArray* handlers ) {


	uint64_t recordcount= 0;

	int ret;
	char* pos;
	uint32_t i;

	OTF_MapEntry* entry;
	uint32_t streamId;

	char next_char = '\0';

	/* initialized? */
	if ( NULL == reader->definitionHeap ) {


		/* init */

		reader->definitionHeap= (OTF_Heap*) malloc( sizeof(OTF_Heap) );
		if( NULL == reader->definitionHeap ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return OTF_READ_ERROR;
		}

		if( 0 == OTF_Heap_initDefHeap( reader->definitionHeap, reader ) ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_Heap_initDefHeap() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			OTF_Heap_finalize( reader->definitionHeap );
			free( reader->definitionHeap );
			reader->definitionHeap= NULL;
			
			return OTF_READ_ERROR;
		}

		for ( i= 0; i < reader->definitionHeap->n; i++ ) {


			if ( NULL != reader->definitionHeap->buffers[i] ) {

				pos= OTF_RBuffer_getRecord( reader->definitionHeap->buffers[i] );

				/* remove empty streams */
				if ( NULL == pos ) {

					/* keep original order */
					reader->definitionHeap->buffers[i]= NULL;
				}
			}
		}
	}


	/* read streams */
	for ( i= 0; i < reader->definitionHeap->n; i ++ ) {


		if ( NULL == reader->definitionHeap->buffers[i] ) {
		
			continue;
		}

		streamId= 0;
		if ( i > 0 ) {

			entry= OTF_MasterControl_getEntryByIndex( reader->mc, i -1 );
			streamId= entry->argument;
		}

		pos= (char*) 0x2;
		while ( NULL != pos ) {

			if ( recordcount >= reader->recordLimit ) {

				/* record count limit reached, return */
				return recordcount;
			}

			/* remember next record type, if it will be a none
			   KEYVALUE record, dont't account it in recordcount */
			next_char = *(reader->definitionHeap->buffers[i]->buffer + reader->definitionHeap->buffers[i]->pos);

			ret= OTF_Reader_parseDefRecord( reader->definitionHeap->buffers[i], 
				handlers, streamId );
			if ( 0 == ret ) {

				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_Reader_parseDefRecord() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				OTF_Heap_finalize( reader->definitionHeap );
				free( reader->definitionHeap );
				reader->definitionHeap= NULL;
			
				return OTF_READ_ERROR;
			}

			/* Now reset the KeyValue list, if we consumed a none
			   KEYVALUE record */
			if ( next_char != OTF_KEYWORD_F_KEYVALUE_PREFIX /* 'K' */ ) {
				OTF_KeyValueList_reset(reader->definitionHeap->buffers[i]->list);
				recordcount++;
			}

			/* prepare next record in that stream */
			pos= OTF_RBuffer_getRecord( reader->definitionHeap->buffers[i] );
			if ( NULL == pos ) {

				/* test if the file is really exceeded or if there is some junk left.
				test here instead of in 'OTF_RBuffer_getRecord()'. 
				throw a warning/error in case*/

				if ( reader->definitionHeap->buffers[i]->pos < reader->definitionHeap->buffers[i]->end ) {

					ret= OTF_Reader_readUnknownDefRecord( reader->definitionHeap->buffers[i], 
						handlers, streamId );
					if ( 0 == ret ) {

						OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
								"OTF_Reader_readUnknownDefRecord() failed.\n",
								__FUNCTION__, __FILE__, __LINE__ );
						
						OTF_Heap_finalize( reader->definitionHeap );
						free( reader->definitionHeap );
						reader->definitionHeap= NULL;

						return OTF_READ_ERROR;
					}
				}

				/* stream is empty -> remove from heap */
				reader->definitionHeap->buffers[i]= NULL;
			}
		}
	}

	return recordcount;
}


OTF_MasterControl* OTF_Reader_getMasterControl( OTF_Reader* reader ) {


	return reader->mc;
}


uint64_t OTF_Reader_readEvents( OTF_Reader* reader, OTF_HandlerArray* handlers ) {


	uint64_t recordcount= 0;

	int ret;
	char* pos;
	uint32_t i;

	uint32_t currentprocess= 0;

#	ifdef OTF_DEBUG
		uint64_t oldtime= 0;
#	endif

	uint64_t p;

	char next_char = '\0';

	/* initialized? */
	if ( NULL == reader->eventHeap ) {


		/* init */
		reader->eventHeap= (OTF_Heap*) malloc( sizeof(OTF_Heap) );
		if( NULL == reader->eventHeap ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return OTF_READ_ERROR;
		}

		if( 0 == OTF_Heap_initEventHeap( reader->eventHeap, reader) ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_Heap_initEventHeap() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			
			OTF_Heap_finalize( reader->eventHeap );
			free( reader->eventHeap );
			reader->eventHeap= NULL;
			
			return OTF_READ_ERROR;
		}

		/* for progress report */
		reader->eventHeap->progressMin= (uint64_t) -1;
		reader->eventHeap->progressMax= 0;

		reader->eventHeap->bytesMax= 0;
		reader->eventHeap->bytesMin= 0;
		reader->eventHeap->bytesCurrent= 0;
		reader->eventHeap->bytesDone= 0;
		
		/* make all buffers in heap jump to start time */
		for ( i= 0; i < reader->eventHeap->n; i++ ) {


			ret= 1;
			
			/* the file has to retrieve his own information ... especially his "lastTime" is intressting */
			OTF_RBuffer_getFileProperties( reader->eventHeap->buffers[i] );
			
			/* get last filepos of lasttime (right in front of the next time) */
			if( reader->maxTime < reader->eventHeap->buffers[i]->lastTime ) {
				/* interval -> take the pos right behind the lasttime, right
				before the next time */
				ret= OTF_RBuffer_searchTime( reader->eventHeap->buffers[i], reader->maxTime );
				
				while( reader->maxTime >= reader->eventHeap->buffers[i]->time ) {
				
					OTF_RBuffer_getRecord( reader->eventHeap->buffers[i] );
					OTF_RBuffer_readNewline( reader->eventHeap->buffers[i] );
				}
				
				reader->eventHeap->bytesMax+= OTF_RBuffer_getFilePos( reader->eventHeap->buffers[i] );
			
			} else {
				/* no interval -> take the fileend */
				reader->eventHeap->bytesMax+= OTF_RBuffer_getFileSize(
					reader->eventHeap->buffers[i] );
			}

			
			/* get the filepos of the firsttime */
			ret&= OTF_RBuffer_searchTime( reader->eventHeap->buffers[i], reader->minTime );
			reader->eventHeap->bytesMin+= OTF_RBuffer_getFilePos( reader->eventHeap->buffers[i] );
			
			if( 0 == ret ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_RBuffer_searchTime() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				OTF_Heap_finalize( reader->eventHeap );
				free( reader->eventHeap );
				reader->eventHeap= NULL;
				
				return OTF_READ_ERROR;
			}

			/* for progress report: get union of all buffer's time interval */
			p= reader->eventHeap->buffers[i]->firstTime;
			reader->eventHeap->progressMin= ( reader->eventHeap->progressMin <= p ) ? 
				reader->eventHeap->progressMin : p;
			p= reader->eventHeap->buffers[i]->lastTime;
			reader->eventHeap->progressMax= ( reader->eventHeap->progressMax >= p ) ? 
				reader->eventHeap->progressMax : p;
			
			/* make sure buffer's time information is updated*/

			pos= OTF_RBuffer_getRecord( reader->eventHeap->buffers[i] );

			/* remove empty streams */
			if ( NULL == pos ) {

				reader->eventHeap->n--;
				reader->eventHeap->buffers[i]= reader->eventHeap->buffers[reader->eventHeap->n];

				/* make sure to repeat this for loop with the same index i */
				i--;
				continue;
			}
			
			/* inlined OTF_RBuffer_getCurrentTime() */
			if( reader->eventHeap->buffers[i]->time < reader->minTime ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"current time %llu < mintime %llu.\n",
						__FUNCTION__, __FILE__, __LINE__,
						(long long unsigned) reader->eventHeap->buffers[i]->time,
						(long long unsigned) reader->minTime );
			
				OTF_Heap_finalize( reader->eventHeap );
				free( reader->eventHeap );
				reader->eventHeap= NULL;
				
				return OTF_READ_ERROR;
			}
		}

		OTF_Heap_sort( reader->eventHeap );

#		ifdef OTF_DEBUG
		OTF_Heap_checksorted( reader->eventHeap );
#		endif /* OTF_DEBUG */

		/* for progress report: 
		cut intervals [progressMin,progressMax] and [minTime,maxTime] */
		reader->eventHeap->progressMin= ( reader->eventHeap->progressMin >= reader->minTime ) ?
			reader->eventHeap->progressMin : reader->minTime;
		reader->eventHeap->progressMax= ( reader->eventHeap->progressMax <= reader->maxTime ) ?
			reader->eventHeap->progressMax : reader->maxTime;

		if ( reader->eventHeap->n > 0 ) {

			reader->eventHeap->progressCurrent= reader->eventHeap->buffers[0]->time;

		} else {

			reader->eventHeap->progressCurrent= 0;
		}
	}

	while ( reader->eventHeap->n > 0 ) {

		if ( recordcount >= reader->recordLimit ) {

			/* record count limit reached, return */
			return recordcount;
		}

		/* inlined OTF_RBuffer_getCurrentTime() */
#		ifdef OTF_DEBUG
			oldtime= reader->eventHeap->progressCurrent;
#		endif
		reader->eventHeap->progressCurrent= reader->eventHeap->buffers[0]->time;

		/* debug check */
#		ifdef OTF_DEBUG
			if ( oldtime > reader->eventHeap->progressCurrent ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"Time decreases. %llu < %llu.\n",
					__FUNCTION__, __FILE__, __LINE__,
					(unsigned long long) reader->eventHeap->progressCurrent,
					(unsigned long long) oldtime );
				
				OTF_Heap_finalize( reader->eventHeap );
				free( reader->eventHeap );
				reader->eventHeap= NULL;
				
				return OTF_READ_ERROR;
			}
#		endif /* OTF_DEBUG */

		/* check for time interval */
		if ( reader->eventHeap->progressCurrent >= reader->maxTime ) {

			/* add the last filepos of this buffer to "bytesDone" */
			reader->eventHeap->bytesDone+= OTF_RBuffer_getFilePos(
				reader->eventHeap->buffers[0] );

			/* drop that stream */
			reader->eventHeap->n--;
			reader->eventHeap->buffers[0]= reader->eventHeap->buffers[reader->eventHeap->n];
			
			OTF_Heap_resort( reader->eventHeap );
			
			continue;
		}


		/* inlined OTF_RBuffer_getCurrentProcess() */
		currentprocess= reader->eventHeap->buffers[0]->process;

		/* check if process is enabled */
		if ( 0 == OTF_ProcessList_getStatus( reader->processList, currentprocess ) ) {

			/* ignore record */
			OTF_RBuffer_readNewline( reader->eventHeap->buffers[0] );

			/* prepare next record in that stream */
			pos= OTF_RBuffer_getRecord( reader->eventHeap->buffers[0] );
			if ( NULL == pos ) {

				/* test if the file is really exceeded or if there is some junk left.
				test here instead of in 'OTF_RBuffer_getRecord()'. 
				throw a warning/error in case*/

				if ( reader->eventHeap->buffers[0]->pos < reader->eventHeap->buffers[0]->end ) {

					ret= OTF_Reader_readUnknownRecord( reader->eventHeap->buffers[0], handlers );
					if ( 0 == ret ) {
					
						OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
								"OTF_Reader_readUnknownRecord() failed.\n",
								__FUNCTION__, __FILE__, __LINE__ );

						OTF_Heap_finalize( reader->eventHeap );
						free( reader->eventHeap );
						reader->eventHeap= NULL;

						return OTF_READ_ERROR;
					}
				}

				/* add the last filepos of this buffer to "bytesDone" */
				reader->eventHeap->bytesDone+= OTF_RBuffer_getFilePos(
					reader->eventHeap->buffers[0] );
					
				/* stream is empty -> remove from heap */
				reader->eventHeap->n--;
				reader->eventHeap->buffers[0]= 
					reader->eventHeap->buffers[reader->eventHeap->n];
					
			}

			OTF_Heap_resort( reader->eventHeap );

			continue;
		}

		/* remember next record type, if it will be a none KEYVALUE
		   record, dont't account it in recordcount */
		next_char = *(reader->eventHeap->buffers[0]->buffer + reader->eventHeap->buffers[0]->pos);

		ret= OTF_Reader_parseEventRecord( reader->eventHeap->buffers[0], handlers );
		if ( 0 == ret ) {

			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_Reader_parseEventRecord() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			OTF_Heap_finalize( reader->eventHeap );
			free( reader->eventHeap );
			reader->eventHeap= NULL;
			
			return OTF_READ_ERROR;
		}

		/* Now reset the KeyValue list, if we consumed a none KEYVALUE
		   record */
		if ( next_char != OTF_KEYWORD_F_KEYVALUE_PREFIX /* 'K' */ ) {
			OTF_KeyValueList_reset(reader->eventHeap->buffers[0]->list);
			recordcount++;
		}

		/* prepare next record in that stream */
		pos= OTF_RBuffer_getRecord( reader->eventHeap->buffers[0] );
		if ( NULL == pos ) {


		/* test if the file is really exceeded or if there is some junk left.
		test here instead of in 'OTF_RBuffer_getRecord()'. 
		throw a warning/error in case*/

			if ( reader->eventHeap->buffers[0]->pos < reader->eventHeap->buffers[0]->end ) {

				ret= OTF_Reader_readUnknownRecord( reader->eventHeap->buffers[0], handlers );
				if ( 0 == ret ) {

						OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
							"OTF_Reader_readUnknownRecord() failed.\n",
							__FUNCTION__, __FILE__, __LINE__ );

					OTF_Heap_finalize( reader->eventHeap );
					free( reader->eventHeap );
					reader->eventHeap= NULL;
			
					return OTF_READ_ERROR;
				}
			}

			/* add the last filepos of this buffer to "bytesDone" */
			reader->eventHeap->bytesDone+= OTF_RBuffer_getFilePos(
				reader->eventHeap->buffers[0] );
			
			/* stream is empty -> remove from heap */
			reader->eventHeap->n--;
			reader->eventHeap->buffers[0]= 
				reader->eventHeap->buffers[reader->eventHeap->n];
		}

		OTF_Heap_resort( reader->eventHeap );
	}

	return recordcount;
}


uint64_t OTF_Reader_readEventsUnsorted( OTF_Reader* reader, OTF_HandlerArray* handlers ) {


	uint64_t recordcount= 0;
	uint64_t p;
	uint32_t currentprocess= 0;
	char* pos;
	uint32_t i;
	int ret;

	double s_reziprok;
	uint64_t delta_t;
	
	char next_char = '\0';

	/* initialized? */
	if ( NULL == reader->eventHeap ) {


		/* init */
		reader->eventHeap= (OTF_Heap*) malloc( sizeof(OTF_Heap) );
		if( NULL == reader->eventHeap ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return OTF_READ_ERROR;
		}

		if( 0 == OTF_Heap_initEventHeap( reader->eventHeap, reader) ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_Heap_initEventHeap() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			
			OTF_Heap_finalize( reader->eventHeap );
			free( reader->eventHeap );
			reader->eventHeap= NULL;
			
			return OTF_READ_ERROR;
		}

		/* for progress report */
		reader->eventHeap->progressMin= (uint64_t) -1;
		reader->eventHeap->progressMax= 0;

		reader->eventHeap->bytesMax= 0;
		reader->eventHeap->bytesMin= 0;
		reader->eventHeap->bytesCurrent= 0;
		reader->eventHeap->bytesDone= 0;
		
		/* make all buffers in heap jump to start time */
		for ( i= 0; i < reader->eventHeap->n; i++ ) {


			ret= 1;
			
			/* the file has to retrieve his own information ... especially his "lastTime" is intressting */
			OTF_RBuffer_getFileProperties( reader->eventHeap->buffers[i] );
			
			/* get last filepos of lasttime (right in front of the next time) */
			if( reader->maxTime < reader->eventHeap->buffers[i]->lastTime ) {
				/* interval -> take the pos right behind the lasttime, right
				before the next time */
				ret= OTF_RBuffer_searchTime( reader->eventHeap->buffers[i], reader->maxTime );
				
				while( reader->maxTime >= reader->eventHeap->buffers[i]->time ) {
				
					OTF_RBuffer_getRecord( reader->eventHeap->buffers[i] );
					OTF_RBuffer_readNewline( reader->eventHeap->buffers[i] );
				}
				
				reader->eventHeap->bytesMax+= OTF_RBuffer_getFilePos( reader->eventHeap->buffers[i] );
			
			} else {
				/* no interval -> take the fileend */
				reader->eventHeap->bytesMax+= OTF_RBuffer_getFileSize(
					reader->eventHeap->buffers[i] );
			}

			
			/* get the filepos of the firsttime */
			ret&= OTF_RBuffer_searchTime( reader->eventHeap->buffers[i], reader->minTime );
			reader->eventHeap->bytesMin+= OTF_RBuffer_getFilePos( reader->eventHeap->buffers[i] );
			
			if( 0 == ret ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_RBuffer_searchTime() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				OTF_Heap_finalize( reader->eventHeap );
				free( reader->eventHeap );
				reader->eventHeap= NULL;
				
				return OTF_READ_ERROR;
			}

			/* for progress report: get union of all buffer's time interval */
			p= reader->eventHeap->buffers[i]->firstTime;
			reader->eventHeap->progressMin= ( reader->eventHeap->progressMin <= p ) ? 
				reader->eventHeap->progressMin : p;
			p= reader->eventHeap->buffers[i]->lastTime;
			reader->eventHeap->progressMax= ( reader->eventHeap->progressMax >= p ) ? 
				reader->eventHeap->progressMax : p;
			
			/* make sure buffer's time information is updated*/

			pos= OTF_RBuffer_getRecord( reader->eventHeap->buffers[i] );

			/* remove empty streams */
			if ( NULL == pos ) {

				reader->eventHeap->n--;
				reader->eventHeap->buffers[i]= reader->eventHeap->buffers[reader->eventHeap->n];

				/* make sure to repeat this for loop with the same index i */
				i--;
				continue;
			}
			
			/* inlined OTF_RBuffer_getCurrentTime() */
			if( reader->eventHeap->buffers[i]->time < reader->minTime ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"current time %llu < mintime %llu.\n",
						__FUNCTION__, __FILE__, __LINE__,
						(long long unsigned) reader->eventHeap->buffers[i]->time,
						(long long unsigned) reader->minTime );
			
				OTF_Heap_finalize( reader->eventHeap );
				free( reader->eventHeap );
				reader->eventHeap= NULL;
				
				return OTF_READ_ERROR;
			}
		}


		/* for progress report: 
		cut intervals [progressMin,progressMax] and [minTime,maxTime] */
		reader->eventHeap->progressMin= ( reader->eventHeap->progressMin >= reader->minTime ) ?
			reader->eventHeap->progressMin : reader->minTime;
		reader->eventHeap->progressMax= ( reader->eventHeap->progressMax <= reader->maxTime ) ?
			reader->eventHeap->progressMax : reader->maxTime;

	}


	s_reziprok= 1/(double)reader->eventHeap->s;
	delta_t= reader->eventHeap->progressMax-reader->eventHeap->progressMin;


	while ( reader->eventHeap->n > 0 && recordcount < reader->recordLimit ) {

		/* inlined OTF_RBuffer_getCurrentTime() */

		/* calculate a "current time", which is between min and max.
		 * In this case we have to: min + ((delta)/s)*(s-n) + (curtime-min)/s
		 *
		 * s= all streams
		 * n= streams left
		 * delta= max-min
		 */
		reader->eventHeap->progressCurrent= (uint64_t) (
			reader->eventHeap->progressMin
			+
			(delta_t*s_reziprok)
			*
			(reader->eventHeap->s-reader->eventHeap->n)
			+
			(reader->eventHeap->buffers[0]->time-reader->eventHeap->progressMin)*s_reziprok );
			

		/* check for time interval */
		if ( reader->eventHeap->buffers[0]->time >= reader->maxTime ) {

			/* add the last filepos of this buffer to "bytesDone" */
			reader->eventHeap->bytesDone+= OTF_RBuffer_getFilePos(
				reader->eventHeap->buffers[0] );

			/* drop that stream */
			reader->eventHeap->n--;
			reader->eventHeap->buffers[0]= reader->eventHeap->buffers[reader->eventHeap->n];
			
			continue;
		}


		/* inlined OTF_RBuffer_getCurrentProcess() */
		currentprocess= reader->eventHeap->buffers[0]->process;

		/* check if process is enabled */
		if ( 0 == OTF_ProcessList_getStatus( reader->processList, currentprocess ) ) {

			/* ignore record */
			OTF_RBuffer_readNewline( reader->eventHeap->buffers[0] );

			/* prepare next record in that stream */
			pos= OTF_RBuffer_getRecord( reader->eventHeap->buffers[0] );
			if ( NULL == pos ) {

				/* test if the file is really exceeded or if there is some junk left.
				test here instead of in 'OTF_RBuffer_getRecord()'. 
				throw a warning/error in case*/

				if ( reader->eventHeap->buffers[0]->pos < reader->eventHeap->buffers[0]->end ) {

					ret= OTF_Reader_readUnknownRecord( reader->eventHeap->buffers[0], handlers );
					if ( 0 == ret ) {

						OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
								"OTF_Reader_readUnknownRecord() failed.\n",
								__FUNCTION__, __FILE__, __LINE__ );

						OTF_Heap_finalize( reader->eventHeap );
						free( reader->eventHeap );
						reader->eventHeap= NULL;

						return OTF_READ_ERROR;
					}
				}

				/* add the last filepos of this buffer to "bytesDone" */
				reader->eventHeap->bytesDone+= OTF_RBuffer_getFilePos(
					reader->eventHeap->buffers[0] );
					
				/* stream is empty -> remove from heap */
				reader->eventHeap->n--;
				reader->eventHeap->buffers[0]= 
					reader->eventHeap->buffers[reader->eventHeap->n];
					
			}


			continue;
		}

		/* remember next record type, if it will be a none KEYVALUE
		   record, dont't account it in recordcount */
		next_char = *(reader->eventHeap->buffers[0]->buffer + reader->eventHeap->buffers[0]->pos);

		ret= OTF_Reader_parseEventRecord( reader->eventHeap->buffers[0], handlers );
		if ( 0 == ret ) {

			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_Reader_parseEventRecord() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			OTF_Heap_finalize( reader->eventHeap );
			free( reader->eventHeap );
			reader->eventHeap= NULL;
			
			return OTF_READ_ERROR;
		}

		/* Now reset the KeyValue list, if we consumed a none KEYVALUE
		   record */
		if ( next_char != OTF_KEYWORD_F_KEYVALUE_PREFIX /* 'K' */ ) {
			OTF_KeyValueList_reset(reader->eventHeap->buffers[0]->list);
			recordcount++;
		}

		/* prepare next record in that stream */
		pos= OTF_RBuffer_getRecord( reader->eventHeap->buffers[0] );
		if ( NULL == pos ) {

			/* test if the file is really exceeded or if there is some junk left.
			test here instead of in 'OTF_RBuffer_getRecord()'. 
			throw a warning/error in case*/

			if ( reader->eventHeap->buffers[0]->pos < reader->eventHeap->buffers[0]->end ) {

				ret= OTF_Reader_readUnknownRecord( reader->eventHeap->buffers[0], handlers );
				if ( 0 == ret ) {

					OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
							"OTF_Reader_readUnknownRecord() failed.\n",
							__FUNCTION__, __FILE__, __LINE__ );
					
					OTF_Heap_finalize( reader->eventHeap );
					free( reader->eventHeap );
					reader->eventHeap= NULL;
			
					return OTF_READ_ERROR;
				}
			}

			/* add the last filepos of this buffer to "bytesDone" */
			reader->eventHeap->bytesDone+= OTF_RBuffer_getFilePos(
				reader->eventHeap->buffers[0] );
			
			/* stream is empty -> remove from heap */
			reader->eventHeap->n--;
			reader->eventHeap->buffers[0]= 
				reader->eventHeap->buffers[reader->eventHeap->n];
		}

	}

	return recordcount;
}



uint64_t OTF_Reader_readSnapshots( OTF_Reader* reader, 
		OTF_HandlerArray* handlers ) {


	uint64_t recordcount= 0;

	int ret;
	char* pos;
	uint32_t i;

	uint32_t currentprocess= 0;

#	ifdef OTF_DEBUG
		uint64_t oldtime= 0;
#	endif

	uint64_t p;

	char next_char = '\0';


	/* initialized? */
	if ( NULL == reader->snapshotsHeap ) {


		/* init */
		reader->snapshotsHeap= (OTF_Heap*) malloc( sizeof(OTF_Heap) );
		if( NULL == reader->snapshotsHeap ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return OTF_READ_ERROR;
		}

		if( 0 == OTF_Heap_initSnapshotsHeap( reader->snapshotsHeap, reader) ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			OTF_Heap_finalize( reader->snapshotsHeap );
			free( reader->snapshotsHeap );
			reader->snapshotsHeap= NULL;

			return OTF_READ_ERROR;
		}

		/* for progress report */
		reader->snapshotsHeap->progressMin= (uint64_t) -1;
		reader->snapshotsHeap->progressMax= 0;

		reader->snapshotsHeap->bytesMax= 0;
		reader->snapshotsHeap->bytesMin= 0;
		reader->snapshotsHeap->bytesCurrent= 0;
		reader->snapshotsHeap->bytesDone= 0;
		
		/* make all buffers in heap jump to start time */
		for ( i= 0; i < reader->snapshotsHeap->n; i++ ) {


			ret= 1;
			
			/* the file has to retrieve his own information ... especially his "lastTime" is intressting */
			OTF_RBuffer_getFileProperties( reader->snapshotsHeap->buffers[i] );
			
			/* get last filepos of lasttime (right in front of the next time) */
			if( reader->maxTime < reader->snapshotsHeap->buffers[i]->lastTime ) {
				/* interval -> take the pos right behind the lasttime, right
				before the next time */
				ret= OTF_RBuffer_searchTime( reader->snapshotsHeap->buffers[i], reader->maxTime );
				
				while( reader->maxTime >= reader->snapshotsHeap->buffers[i]->time ) {
				
					OTF_RBuffer_getRecord( reader->snapshotsHeap->buffers[i] );
					OTF_RBuffer_readNewline( reader->snapshotsHeap->buffers[i] );
				}
				
				reader->snapshotsHeap->bytesMax+= OTF_RBuffer_getFilePos(
					reader->snapshotsHeap->buffers[i] );
			
			} else {
				/* no interval -> take the fileend */
				reader->snapshotsHeap->bytesMax+= OTF_RBuffer_getFileSize(
					reader->snapshotsHeap->buffers[i] );
			}

			
			/* get the filepos of the firsttime */
			ret&= OTF_RBuffer_searchTime( reader->snapshotsHeap->buffers[i],
				reader->minTime );
			reader->snapshotsHeap->bytesMin+= OTF_RBuffer_getFilePos(
				reader->snapshotsHeap->buffers[i] );
			
			if( 0 == ret ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_RBuffer_searchTime() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				OTF_Heap_finalize( reader->snapshotsHeap );
				free( reader->snapshotsHeap );
				reader->snapshotsHeap= NULL;
	
				return OTF_READ_ERROR;
			}

			/* for progress report: get union of all buffer's time interval */
			p= reader->snapshotsHeap->buffers[i]->firstTime;
			reader->snapshotsHeap->progressMin= ( reader->snapshotsHeap->progressMin <= p ) ? 
				reader->snapshotsHeap->progressMin : p;
			p= reader->snapshotsHeap->buffers[i]->lastTime;
			reader->snapshotsHeap->progressMax= ( reader->snapshotsHeap->progressMax >= p ) ? 
				reader->snapshotsHeap->progressMax : p;
			
			/* make sure buffer's time information is updated*/

			pos= OTF_RBuffer_getRecord( reader->snapshotsHeap->buffers[i] );

			/* remove empty streams */
			if ( NULL == pos ) {

				reader->snapshotsHeap->n--;
				reader->snapshotsHeap->buffers[i]=
					reader->snapshotsHeap->buffers[reader->snapshotsHeap->n];

				/* make sure to repeat this for loop with the same index i */
				i--;
				continue;
			}
			
			/* inlined OTF_RBuffer_getCurrentTime() */
			if( reader->snapshotsHeap->buffers[i]->time < reader->minTime ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"current time %llu < mintime %llu.\n",
						__FUNCTION__, __FILE__, __LINE__,
						(long long unsigned) reader->snapshotsHeap->buffers[i]->time,
						(long long unsigned) reader->minTime );

				OTF_Heap_finalize( reader->snapshotsHeap );
				free( reader->snapshotsHeap );
				reader->snapshotsHeap= NULL;
				
				return OTF_READ_ERROR;
			}
		}

		OTF_Heap_sort( reader->snapshotsHeap );

#		ifdef OTF_DEBUG
		OTF_Heap_checksorted( reader->snapshotsHeap );
#		endif /* OTF_DEBUG */

		/* for progress report: 
		cut intervals [progressMin,progressMax] and [minTime,maxTime] */
		reader->snapshotsHeap->progressMin= ( reader->snapshotsHeap->progressMin >= reader->minTime ) ?
			reader->snapshotsHeap->progressMin : reader->minTime;
		reader->snapshotsHeap->progressMax= ( reader->snapshotsHeap->progressMax <= reader->maxTime ) ?
			reader->snapshotsHeap->progressMax : reader->maxTime;

		if ( reader->snapshotsHeap->n > 0 ) {

			reader->snapshotsHeap->progressCurrent= reader->snapshotsHeap->buffers[0]->time;

		} else {

			reader->snapshotsHeap->progressCurrent= 0;
		}
	}

	while ( reader->snapshotsHeap->n > 0 ) {

		if ( recordcount >= reader->recordLimit ) {

			/* record count limit reached, return */
			return recordcount;
		}

		/* inlined OTF_RBuffer_getCurrentTime() */
#		ifdef OTF_DEBUG
			oldtime= reader->snapshotsHeap->progressCurrent;
#		endif
		reader->snapshotsHeap->progressCurrent= reader->snapshotsHeap->buffers[0]->time;

		/* debug check */
#		ifdef OTF_DEBUG
			if ( oldtime > reader->snapshotsHeap->progressCurrent ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"Time does decrease. %llu < %llu.\n",
					__FUNCTION__, __FILE__, __LINE__,
					(unsigned long long) reader->snapshotsHeap->progressCurrent,
					(unsigned long long) oldtime );
				
				OTF_Heap_finalize( reader->snapshotsHeap );
				free( reader->snapshotsHeap );
				reader->snapshotsHeap= NULL;
				
				return OTF_READ_ERROR;
			}
#		endif /* OTF_DEBUG */

		/* check for time interval */
		if ( reader->snapshotsHeap->progressCurrent >= reader->maxTime ) {

			/* add the last filepos of this buffer to "bytesDone" */
			reader->snapshotsHeap->bytesDone+= OTF_RBuffer_getFilePos(
				reader->snapshotsHeap->buffers[0] );
			
			/* drop that stream */
			reader->snapshotsHeap->n--;
			reader->snapshotsHeap->buffers[0]=
				reader->snapshotsHeap->buffers[reader->snapshotsHeap->n];

			OTF_Heap_resort( reader->snapshotsHeap );
			
			continue;
		}


		/* inlined OTF_RBuffer_getCurrentProcess() */
		currentprocess= reader->snapshotsHeap->buffers[0]->process;

		/* check if process is enabled */
		if ( 0 == OTF_ProcessList_getStatus( reader->processList, currentprocess ) ) {

			/* ignore record */
			OTF_RBuffer_readNewline( reader->snapshotsHeap->buffers[0] );

			/* prepare next record in that stream */
			pos= OTF_RBuffer_getRecord( reader->snapshotsHeap->buffers[0] );
			if ( NULL == pos ) {

				/* test if the file is really exceeded or if there is some junk left.
				test here instead of in 'OTF_RBuffer_getRecord()'. 
				throw a warning/error in case*/

				if ( reader->snapshotsHeap->buffers[0]->pos < reader->snapshotsHeap->buffers[0]->end ) {

					ret= OTF_Reader_readUnknownRecord( reader->snapshotsHeap->buffers[0], handlers );
					if ( 0 == ret ) {

						OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
								"OTF_Reader_readUnknownRecord() failed.\n",
								__FUNCTION__, __FILE__, __LINE__ );
						
						OTF_Heap_finalize( reader->snapshotsHeap );
						free( reader->snapshotsHeap );
						reader->snapshotsHeap= NULL;

						return OTF_READ_ERROR;
					}
				}

				/* add the last filepos of this buffer to "bytesDone" */
				reader->snapshotsHeap->bytesDone+= OTF_RBuffer_getFilePos(
					reader->snapshotsHeap->buffers[0] );
				
				/* stream is empty -> remove from heap */
				reader->snapshotsHeap->n--;
				reader->snapshotsHeap->buffers[0]= 
					reader->snapshotsHeap->buffers[reader->snapshotsHeap->n];
			}

			OTF_Heap_resort( reader->snapshotsHeap );

			continue;
		}

		/* remember next record type, if it will be a none KEYVALUE
		   record, dont't account it in recordcount */
		next_char = *(reader->snapshotsHeap->buffers[0]->buffer + reader->snapshotsHeap->buffers[0]->pos);

		ret= OTF_Reader_parseSnapshotsRecord( reader->snapshotsHeap->buffers[0], handlers );
		if ( 0 == ret ) {

			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_Reader_parseSnapshotsRecord() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			
			OTF_Heap_finalize( reader->snapshotsHeap );
			free( reader->snapshotsHeap );
			reader->snapshotsHeap= NULL;

			return OTF_READ_ERROR;
		}

		/* Now reset the KeyValue list, if we consumed a none KEYVALUE
		   record */
		if ( next_char != OTF_KEYWORD_F_KEYVALUE_PREFIX /* 'K' */ ) {
			OTF_KeyValueList_reset(reader->snapshotsHeap->buffers[0]->list);
			recordcount++;
		}

		/* prepare next record in that stream */
		pos= OTF_RBuffer_getRecord( reader->snapshotsHeap->buffers[0] );
		if ( NULL == pos ) {

			/* test if the file is really exceeded or if there is some junk left.
			test here instead of in 'OTF_RBuffer_getRecord()'. 
			throw a warning/error in case*/

			if ( reader->snapshotsHeap->buffers[0]->pos < reader->snapshotsHeap->buffers[0]->end ) {

				ret= OTF_Reader_readUnknownRecord( reader->snapshotsHeap->buffers[0], handlers );
				if ( 0 == ret ) {

					OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
							"OTF_Reader_readUnknownRecord() failed.\n",
							__FUNCTION__, __FILE__, __LINE__ );
					
					OTF_Heap_finalize( reader->snapshotsHeap );
					free( reader->snapshotsHeap );
					reader->snapshotsHeap= NULL;

					return OTF_READ_ERROR;
				}
			}

			/* add the last filepos of this buffer to "bytesDone" */
			reader->snapshotsHeap->bytesDone+= OTF_RBuffer_getFilePos(
				reader->snapshotsHeap->buffers[0] );
			
			/* stream is empty -> remove from heap */
			reader->snapshotsHeap->n--;
			reader->snapshotsHeap->buffers[0]= 
				reader->snapshotsHeap->buffers[reader->snapshotsHeap->n];
		}

		OTF_Heap_resort( reader->snapshotsHeap );
	}

	return recordcount;
}


uint64_t OTF_Reader_readSnapshotsUnsorted( OTF_Reader* reader, OTF_HandlerArray* handlers ) {


	uint64_t recordcount= 0;
	uint64_t p;
	uint32_t currentprocess= 0;
	char* pos;
	uint32_t i;
	int ret;

	double s_reziprok;
	uint64_t delta_t;

	char next_char = '\0';

	/* initialized? */
	if ( NULL == reader->snapshotsHeap ) {


		/* init */
		reader->snapshotsHeap= (OTF_Heap*) malloc( sizeof(OTF_Heap) );
		if( NULL == reader->snapshotsHeap ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return OTF_READ_ERROR;
		}

		if( 0 == OTF_Heap_initSnapshotsHeap( reader->snapshotsHeap, reader) ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_Heap_initSnapshotsHeap() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			
			OTF_Heap_finalize( reader->snapshotsHeap );
			free( reader->snapshotsHeap );
			reader->snapshotsHeap= NULL;
			
			return OTF_READ_ERROR;
		}

		/* for progress report */
		reader->snapshotsHeap->progressMin= (uint64_t) -1;
		reader->snapshotsHeap->progressMax= 0;

		reader->snapshotsHeap->bytesMax= 0;
		reader->snapshotsHeap->bytesMin= 0;
		reader->snapshotsHeap->bytesCurrent= 0;
		reader->snapshotsHeap->bytesDone= 0;
		
		/* make all buffers in heap jump to start time */
		for ( i= 0; i < reader->snapshotsHeap->n; i++ ) {


			ret= 1;
			
			/* the file has to retrieve his own information ... especially his "lastTime" is intressting */
			OTF_RBuffer_getFileProperties( reader->snapshotsHeap->buffers[i] );
			
			/* get last filepos of lasttime (right in front of the next time) */
			if( reader->maxTime < reader->snapshotsHeap->buffers[i]->lastTime ) {
				/* interval -> take the pos right behind the lasttime, right
				before the next time */
				ret= OTF_RBuffer_searchTime( reader->snapshotsHeap->buffers[i], reader->maxTime );
				
				while( reader->maxTime >= reader->snapshotsHeap->buffers[i]->time ) {
				
					OTF_RBuffer_getRecord( reader->snapshotsHeap->buffers[i] );
					OTF_RBuffer_readNewline( reader->snapshotsHeap->buffers[i] );
				}
				
				reader->snapshotsHeap->bytesMax+= OTF_RBuffer_getFilePos( reader->snapshotsHeap->buffers[i] );
			
			} else {
				/* no interval -> take the fileend */
				reader->snapshotsHeap->bytesMax+= OTF_RBuffer_getFileSize(
					reader->snapshotsHeap->buffers[i] );
			}

			
			/* get the filepos of the firsttime */
			ret&= OTF_RBuffer_searchTime( reader->snapshotsHeap->buffers[i], reader->minTime );
			reader->snapshotsHeap->bytesMin+= OTF_RBuffer_getFilePos( reader->snapshotsHeap->buffers[i] );
			
			if( 0 == ret ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_RBuffer_searchTime() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				OTF_Heap_finalize( reader->snapshotsHeap );
				free( reader->snapshotsHeap );
				reader->snapshotsHeap= NULL;
				
				return OTF_READ_ERROR;
			}

			/* for progress report: get union of all buffer's time interval */
			p= reader->snapshotsHeap->buffers[i]->firstTime;
			reader->snapshotsHeap->progressMin= ( reader->snapshotsHeap->progressMin <= p ) ?
				reader->snapshotsHeap->progressMin : p;
			p= reader->snapshotsHeap->buffers[i]->lastTime;
			reader->snapshotsHeap->progressMax= ( reader->snapshotsHeap->progressMax >= p ) ?
				reader->snapshotsHeap->progressMax : p;
			
			/* make sure buffer's time information is updated*/

			pos= OTF_RBuffer_getRecord( reader->snapshotsHeap->buffers[i] );

			/* remove empty streams */
			if ( NULL == pos ) {

				reader->snapshotsHeap->n--;
				reader->snapshotsHeap->buffers[i]= reader->snapshotsHeap->buffers[reader->snapshotsHeap->n];

				/* make sure to repeat this for loop with the same index i */
				i--;
				continue;
			}
			
			/* inlined OTF_RBuffer_getCurrentTime() */
			if( reader->snapshotsHeap->buffers[i]->time < reader->minTime ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"current time %llu < mintime %llu.\n",
						__FUNCTION__, __FILE__, __LINE__,
						(long long unsigned) reader->snapshotsHeap->buffers[i]->time,
						(long long unsigned) reader->minTime );
			
				OTF_Heap_finalize( reader->snapshotsHeap );
				free( reader->snapshotsHeap );
				reader->snapshotsHeap= NULL;
				
				return OTF_READ_ERROR;
			}
		}


		/* for progress report: 
		cut intervals [progressMin,progressMax] and [minTime,maxTime] */
		reader->snapshotsHeap->progressMin= ( reader->snapshotsHeap->progressMin >= reader->minTime ) ?
			reader->snapshotsHeap->progressMin : reader->minTime;
		reader->snapshotsHeap->progressMax= ( reader->snapshotsHeap->progressMax <= reader->maxTime ) ?
			reader->snapshotsHeap->progressMax : reader->maxTime;

	}


	s_reziprok= 1/(double)reader->snapshotsHeap->s;
	delta_t= reader->snapshotsHeap->progressMax-reader->snapshotsHeap->progressMin;


	while ( reader->snapshotsHeap->n > 0 && recordcount < reader->recordLimit ) {

		/* inlined OTF_RBuffer_getCurrentTime() */

		/* calculate a "current time", which is between min and max.
		 * In this case we have to: min + ((delta)/s)*(s-n) + (curtime-min)/s
		 *
		 * s= all streams
		 * n= streams left
		 * delta= max-min
		 */
		reader->snapshotsHeap->progressCurrent= (uint64_t) (
			reader->snapshotsHeap->progressMin
			+
			(delta_t*s_reziprok)
			*
			(reader->snapshotsHeap->s-reader->snapshotsHeap->n)
			+
			(reader->snapshotsHeap->buffers[0]->time-reader->snapshotsHeap->progressMin)*s_reziprok );

		/* check for time interval */
		if ( reader->snapshotsHeap->buffers[0]->time >= reader->maxTime ) {

			/* add the last filepos of this buffer to "bytesDone" */
			reader->snapshotsHeap->bytesDone+= OTF_RBuffer_getFilePos(
				reader->snapshotsHeap->buffers[0] );

			/* drop that stream */
			reader->snapshotsHeap->n--;
			reader->snapshotsHeap->buffers[0]= reader->snapshotsHeap->buffers[reader->snapshotsHeap->n];
			
			continue;
		}


		/* inlined OTF_RBuffer_getCurrentProcess() */
		currentprocess= reader->snapshotsHeap->buffers[0]->process;

		/* check if process is enabled */
		if ( 0 == OTF_ProcessList_getStatus( reader->processList, currentprocess ) ) {

			/* ignore record */
			OTF_RBuffer_readNewline( reader->snapshotsHeap->buffers[0] );

			/* prepare next record in that stream */
			pos= OTF_RBuffer_getRecord( reader->snapshotsHeap->buffers[0] );
			if ( NULL == pos ) {

				/* test if the file is really exceeded or if there is some junk left.
				test here instead of in 'OTF_RBuffer_getRecord()'. 
				throw a warning/error in case*/

				if ( reader->snapshotsHeap->buffers[0]->pos < reader->snapshotsHeap->buffers[0]->end ) {

					ret= OTF_Reader_readUnknownRecord( reader->snapshotsHeap->buffers[0], handlers );
					if ( 0 == ret ) {

						OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
								"OTF_Reader_readUnknownRecord() failed.\n",
								__FUNCTION__, __FILE__, __LINE__ );
						
						OTF_Heap_finalize( reader->snapshotsHeap );
						free( reader->snapshotsHeap );
						reader->snapshotsHeap= NULL;

						return OTF_READ_ERROR;
					}
				}

				/* add the last filepos of this buffer to "bytesDone" */
				reader->snapshotsHeap->bytesDone+= OTF_RBuffer_getFilePos(
					reader->snapshotsHeap->buffers[0] );
					
				/* stream is empty -> remove from heap */
				reader->snapshotsHeap->n--;
				reader->snapshotsHeap->buffers[0]=
					reader->snapshotsHeap->buffers[reader->snapshotsHeap->n];
					
			}


			continue;
		}

		/* remember next record type, if it will be a none KEYVALUE
		   record, dont't account it in recordcount */
		next_char = *(reader->snapshotsHeap->buffers[0]->buffer + reader->snapshotsHeap->buffers[0]->pos);

		ret= OTF_Reader_parseSnapshotsRecord( reader->snapshotsHeap->buffers[0], handlers );
		if ( 0 == ret ) {

			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_Reader_parseSnapshotsRecord() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			
			OTF_Heap_finalize( reader->snapshotsHeap );
			free( reader->snapshotsHeap );
			reader->snapshotsHeap= NULL;

			return OTF_READ_ERROR;
		}

		/* Now reset the KeyValue list, if we consumed a none KEYVALUE
		   record */
		if ( next_char != OTF_KEYWORD_F_KEYVALUE_PREFIX /* 'K' */ ) {
			OTF_KeyValueList_reset(reader->snapshotsHeap->buffers[0]->list);
			recordcount++;
		}

		/* prepare next record in that stream */
		pos= OTF_RBuffer_getRecord( reader->snapshotsHeap->buffers[0] );
		if ( NULL == pos ) {

			/* test if the file is really exceeded or if there is some junk left.
			test here instead of in 'OTF_RBuffer_getRecord()'. 
			throw a warning/error in case*/

			if ( reader->snapshotsHeap->buffers[0]->pos < reader->snapshotsHeap->buffers[0]->end ) {

				ret= OTF_Reader_readUnknownRecord( reader->snapshotsHeap->buffers[0], handlers );
				if ( 0 == ret ) {

					OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
							"OTF_Reader_readUnknownRecord() failed.\n",
							__FUNCTION__, __FILE__, __LINE__ );
					
					OTF_Heap_finalize( reader->snapshotsHeap );
					free( reader->snapshotsHeap );
					reader->snapshotsHeap= NULL;
			
					return OTF_READ_ERROR;
				}
			}

			/* add the last filepos of this buffer to "bytesDone" */
			reader->snapshotsHeap->bytesDone+= OTF_RBuffer_getFilePos(
				reader->snapshotsHeap->buffers[0] );
			
			/* stream is empty -> remove from heap */
			reader->snapshotsHeap->n--;
			reader->snapshotsHeap->buffers[0]=
				reader->snapshotsHeap->buffers[reader->snapshotsHeap->n];
		}

	}

	return recordcount;
}


uint64_t OTF_Reader_readStatistics( OTF_Reader* reader,
		OTF_HandlerArray* handlers ) {


	uint64_t recordcount= 0;

	int ret;
	char* pos;
	uint32_t i;

	uint32_t currentprocess= 0;

#	ifdef OTF_DEBUG
		uint64_t oldtime= 0;
#	endif

	uint64_t p;

	char next_char = '\0';

	/* initialized? */
	if ( NULL == reader->statisticsHeap ) {


		/* init */
		reader->statisticsHeap= (OTF_Heap*) malloc( sizeof(OTF_Heap) );
		if( NULL == reader->statisticsHeap ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return OTF_READ_ERROR;
		}

		if( 0 == OTF_Heap_initStatisticsHeap( reader->statisticsHeap, reader) ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_Heap_initStatisticsHeap() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			OTF_Heap_finalize( reader->statisticsHeap );
			free( reader->statisticsHeap );
			reader->statisticsHeap= NULL;
			
			return OTF_READ_ERROR;
		}

		
		/* for progress report */
		reader->statisticsHeap->progressMin= (uint64_t) -1;
		reader->statisticsHeap->progressMax= 0;

		reader->statisticsHeap->bytesMax= 0;
		reader->statisticsHeap->bytesMin= 0;
		reader->statisticsHeap->bytesCurrent= 0;
		reader->statisticsHeap->bytesDone= 0;
		
		/* make all buffers in heap jump to start time */
		for ( i= 0; i < reader->statisticsHeap->n; i++ ) {


			ret= 1;
			
			/* the file has to retrieve his own information ... especially his "lastTime" is intressting */
			OTF_RBuffer_getFileProperties( reader->statisticsHeap->buffers[i] );
			
			/* get last filepos of lasttime (right in front of the next time) */
			if( reader->maxTime < reader->statisticsHeap->buffers[i]->lastTime ) {
				/* interval -> take the pos right behind the lasttime, right
				before the next time */
				ret= OTF_RBuffer_searchTime( reader->statisticsHeap->buffers[i],
					reader->maxTime );
				
				while( reader->maxTime >= reader->statisticsHeap->buffers[i]->time ) {
				
					OTF_RBuffer_getRecord( reader->statisticsHeap->buffers[i] );
					OTF_RBuffer_readNewline( reader->statisticsHeap->buffers[i] );
				}
				
				reader->statisticsHeap->bytesMax+= OTF_RBuffer_getFilePos(
					reader->statisticsHeap->buffers[i] );
			
			} else {
				/* no interval -> take the fileend */
				reader->statisticsHeap->bytesMax+= OTF_RBuffer_getFileSize(
					reader->statisticsHeap->buffers[i] );
			}

			
			/* get the filepos of the firsttime */
			ret&= OTF_RBuffer_searchTime( reader->statisticsHeap->buffers[i],
				reader->minTime );
			reader->statisticsHeap->bytesMin+= OTF_RBuffer_getFilePos(
				reader->statisticsHeap->buffers[i] );
			if( 0 == ret ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_RBuffer_searchTime() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				OTF_Heap_finalize( reader->statisticsHeap );
				free( reader->statisticsHeap );
				reader->statisticsHeap= NULL;
	
				return OTF_READ_ERROR;
			}

			/* for progress report: get union of all buffer's time interval */
			p= reader->statisticsHeap->buffers[i]->firstTime;
			reader->statisticsHeap->progressMin= ( reader->statisticsHeap->progressMin <= p ) ? 
				reader->statisticsHeap->progressMin : p;
			p= reader->statisticsHeap->buffers[i]->lastTime;
			reader->statisticsHeap->progressMax= ( reader->statisticsHeap->progressMax >= p ) ? 
				reader->statisticsHeap->progressMax : p;
			
			/* make sure buffer's time information is updated*/

			pos= OTF_RBuffer_getRecord( reader->statisticsHeap->buffers[i] );

			/* remove empty streams */
			if ( NULL == pos ) {

				reader->statisticsHeap->n--;
				reader->statisticsHeap->buffers[i]=
					reader->statisticsHeap->buffers[reader->statisticsHeap->n];

				/* make sure to repeat this for loop with the same index i */
				i--;
				continue;
			}
			
			/* inlined OTF_RBuffer_getCurrentTime() */
			if( reader->statisticsHeap->buffers[i]->time < reader->minTime ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"current time %llu < mintime %llu.\n",
						__FUNCTION__, __FILE__, __LINE__,
						(long long unsigned) reader->statisticsHeap->buffers[i]->time,
						(long long unsigned) reader->minTime );

				OTF_Heap_finalize( reader->statisticsHeap );
				free( reader->statisticsHeap );
				reader->statisticsHeap= NULL;
				
				return OTF_READ_ERROR;
			}
		}
		
		OTF_Heap_sort( reader->statisticsHeap );
		
#		ifdef OTF_DEBUG
		OTF_Heap_checksorted( reader->statisticsHeap );
#		endif /* OTF_DEBUG */

		/* for progress report: 
		cut intervals [progressMin,progressMax] and [minTime,maxTime] */
		reader->statisticsHeap->progressMin= ( reader->statisticsHeap->progressMin >= reader->minTime ) ?
			reader->statisticsHeap->progressMin : reader->minTime;
		reader->statisticsHeap->progressMax= ( reader->statisticsHeap->progressMax <= reader->maxTime ) ?
			reader->statisticsHeap->progressMax : reader->maxTime;

		if ( reader->statisticsHeap->n > 0 ) {

			reader->statisticsHeap->progressCurrent= reader->statisticsHeap->buffers[0]->time;

		} else {

			reader->statisticsHeap->progressCurrent= 0;
		}
	}

	
	while ( reader->statisticsHeap->n > 0 ) {

		if ( recordcount >= reader->recordLimit ) {

			/* record count limit reached, return */
			return recordcount;
		}

		/* inlined OTF_RBuffer_getCurrentTime() */
#		ifdef OTF_DEBUG
			oldtime= reader->statisticsHeap->progressCurrent;
#		endif
		reader->statisticsHeap->progressCurrent= reader->statisticsHeap->buffers[0]->time;

		/* debug check */
#		ifdef OTF_DEBUG
			if ( oldtime > reader->statisticsHeap->progressCurrent ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"Time does decrease. %llu < %llu.\n",
					__FUNCTION__, __FILE__, __LINE__,
					(unsigned long long) reader->statisticsHeap->progressCurrent,
					(unsigned long long) oldtime );
				
				OTF_Heap_finalize( reader->statisticsHeap );
				free( reader->statisticsHeap );
				reader->statisticsHeap= NULL;
				
				return OTF_READ_ERROR;
			}
#		endif

		/* check for time interval */
		if ( reader->statisticsHeap->progressCurrent >= reader->maxTime ) {

			/* add the last filepos of this buffer to "bytesDone" */
			reader->statisticsHeap->bytesDone+= OTF_RBuffer_getFilePos(
				reader->statisticsHeap->buffers[0] );
			
			/* drop that stream */
			reader->statisticsHeap->n--;
			reader->statisticsHeap->buffers[0]=
				reader->statisticsHeap->buffers[reader->statisticsHeap->n];

			OTF_Heap_resort( reader->statisticsHeap );
			
			continue;
		}


		/* inlined OTF_RBuffer_getCurrentProcess() */
		currentprocess= reader->statisticsHeap->buffers[0]->process;

		/* check if process is enabled */
		if ( 0 == OTF_ProcessList_getStatus( reader->processList, currentprocess ) ) {

			/* ignore record */
			OTF_RBuffer_readNewline( reader->statisticsHeap->buffers[0] );

			/* prepare next record in that stream */
			pos= OTF_RBuffer_getRecord( reader->statisticsHeap->buffers[0] );
			if ( NULL == pos ) {

				/* test if the file is really exceeded or if there is some junk left.
				test here instead of in 'OTF_RBuffer_getRecord()'. 
				throw a warning/error in case*/

				if ( reader->statisticsHeap->buffers[0]->pos < reader->statisticsHeap->buffers[0]->end ) {

					ret= OTF_Reader_readUnknownRecord( reader->statisticsHeap->buffers[0], handlers );
					if ( 0 == ret ) {

						OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
							"OTF_Reader_readUnknownRecord() failed.\n",
							__FUNCTION__, __FILE__, __LINE__ );
						
						OTF_Heap_finalize( reader->statisticsHeap );
						free( reader->statisticsHeap );
						reader->statisticsHeap= NULL;

						return OTF_READ_ERROR;
					}
				}

				/* add the last filepos of this buffer to "bytesDone" */
				reader->statisticsHeap->bytesDone+= OTF_RBuffer_getFilePos(
					reader->statisticsHeap->buffers[0] );
				
				/* stream is empty -> remove from heap */
				reader->statisticsHeap->n--;
				reader->statisticsHeap->buffers[0]= 
					reader->statisticsHeap->buffers[reader->statisticsHeap->n];
			}

			OTF_Heap_resort( reader->statisticsHeap );

			continue;
		}

		/* remember next record type, if it will be a none KEYVALUE
		   record, dont't account it in recordcount */
		next_char = *(reader->statisticsHeap->buffers[0]->buffer + reader->statisticsHeap->buffers[0]->pos);

		ret= OTF_Reader_parseStatisticsRecord( reader->statisticsHeap->buffers[0], handlers );
		if ( 0 == ret ) {

			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_Reader_parseStatisticsRecord() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			
			OTF_Heap_finalize( reader->statisticsHeap );
			free( reader->statisticsHeap );
			reader->statisticsHeap= NULL;

			return OTF_READ_ERROR;
		}

		/* Now reset the KeyValue list, if we consumed a none KEYVALUE
		   record */
		if ( next_char != OTF_KEYWORD_F_KEYVALUE_PREFIX /* 'K' */ ) {
			OTF_KeyValueList_reset(reader->statisticsHeap->buffers[0]->list);
			recordcount++;
		}

		/* prepare next record in that stream */
		pos= OTF_RBuffer_getRecord( reader->statisticsHeap->buffers[0] );
		if ( NULL == pos ) {

			/* test if the file is really exceeded or if there is some junk left.
			test here instead of in 'OTF_RBuffer_getRecord()'. 
			throw a warning/error in case*/

			if ( reader->statisticsHeap->buffers[0]->pos < reader->statisticsHeap->buffers[0]->end ) {

				ret= OTF_Reader_readUnknownRecord( reader->statisticsHeap->buffers[0], handlers );
				if ( 0 == ret ) {

					OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
							"OTF_Reader_readUnknownRecord() failed.\n",
							__FUNCTION__, __FILE__, __LINE__ );
					
					OTF_Heap_finalize( reader->statisticsHeap );
					free( reader->statisticsHeap );
					reader->statisticsHeap= NULL;

					return OTF_READ_ERROR;
				}
			}

			/* add the last filepos of this buffer to "bytesDone" */
			reader->statisticsHeap->bytesDone+= OTF_RBuffer_getFilePos(
				reader->statisticsHeap->buffers[0] );
			
			/* stream is empty -> remove from heap */
			reader->statisticsHeap->n--;
			reader->statisticsHeap->buffers[0]= 
				reader->statisticsHeap->buffers[reader->statisticsHeap->n];
		}

		OTF_Heap_resort( reader->statisticsHeap );
	}
	

	return recordcount;
}



uint64_t OTF_Reader_readStatisticsUnsorted( OTF_Reader* reader, OTF_HandlerArray* handlers ) {


	uint64_t recordcount= 0;
	uint64_t p;
	uint32_t currentprocess= 0;
	char* pos;
	uint32_t i;
	int ret;

	double s_reziprok;
	uint64_t delta_t;


	char next_char = '\0';

	/* initialized? */
	if ( NULL == reader->statisticsHeap ) {


		/* init */
		reader->statisticsHeap= (OTF_Heap*) malloc( sizeof(OTF_Heap) );
		if( NULL == reader->statisticsHeap ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return OTF_READ_ERROR;
		}

		if( 0 == OTF_Heap_initStatisticsHeap( reader->statisticsHeap, reader) ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_Heap_initStatisticsHeap() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			
			OTF_Heap_finalize( reader->statisticsHeap );
			free( reader->statisticsHeap );
			reader->statisticsHeap= NULL;
			
			return OTF_READ_ERROR;
		}

		/* for progress report */
		reader->statisticsHeap->progressMin= (uint64_t) -1;
		reader->statisticsHeap->progressMax= 0;

		reader->statisticsHeap->bytesMax= 0;
		reader->statisticsHeap->bytesMin= 0;
		reader->statisticsHeap->bytesCurrent= 0;
		reader->statisticsHeap->bytesDone= 0;
		
		/* make all buffers in heap jump to start time */
		for ( i= 0; i < reader->statisticsHeap->n; i++ ) {


			ret= 1;
			
			/* the file has to retrieve his own information ... especially his "lastTime" is intressting */
			OTF_RBuffer_getFileProperties( reader->statisticsHeap->buffers[i] );
			
			/* get last filepos of lasttime (right in front of the next time) */
			if( reader->maxTime < reader->statisticsHeap->buffers[i]->lastTime ) {
				/* interval -> take the pos right behind the lasttime, right
				before the next time */
				ret= OTF_RBuffer_searchTime( reader->statisticsHeap->buffers[i], reader->maxTime );
				
				while( reader->maxTime >= reader->statisticsHeap->buffers[i]->time ) {
				
					OTF_RBuffer_getRecord( reader->statisticsHeap->buffers[i] );
					OTF_RBuffer_readNewline( reader->statisticsHeap->buffers[i] );
				}
				
				reader->statisticsHeap->bytesMax+= OTF_RBuffer_getFilePos( reader->statisticsHeap->buffers[i] );
			
			} else {
				/* no interval -> take the fileend */
				reader->statisticsHeap->bytesMax+= OTF_RBuffer_getFileSize(
					reader->statisticsHeap->buffers[i] );
			}

			
			/* get the filepos of the firsttime */
			ret&= OTF_RBuffer_searchTime( reader->statisticsHeap->buffers[i], reader->minTime );
			reader->statisticsHeap->bytesMin+= OTF_RBuffer_getFilePos( reader->statisticsHeap->buffers[i] );
			
			if( 0 == ret ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_RBuffer_searchTime() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				OTF_Heap_finalize( reader->statisticsHeap );
				free( reader->statisticsHeap );
				reader->statisticsHeap= NULL;
				
				return OTF_READ_ERROR;
			}

			/* for progress report: get union of all buffer's time interval */
			p= reader->statisticsHeap->buffers[i]->firstTime;
			reader->statisticsHeap->progressMin= ( reader->statisticsHeap->progressMin <= p ) ?
				reader->statisticsHeap->progressMin : p;
			p= reader->statisticsHeap->buffers[i]->lastTime;
			reader->statisticsHeap->progressMax= ( reader->statisticsHeap->progressMax >= p ) ?
				reader->statisticsHeap->progressMax : p;
			
			/* make sure buffer's time information is updated*/

			pos= OTF_RBuffer_getRecord( reader->statisticsHeap->buffers[i] );

			/* remove empty streams */
			if ( NULL == pos ) {

				reader->statisticsHeap->n--;
				reader->statisticsHeap->buffers[i]= reader->statisticsHeap->buffers[reader->statisticsHeap->n];

				/* make sure to repeat this for loop with the same index i */
				i--;
				continue;
			}
			
			/* inlined OTF_RBuffer_getCurrentTime() */
			if( reader->statisticsHeap->buffers[i]->time < reader->minTime ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"current time %llu < mintime %llu.\n",
						__FUNCTION__, __FILE__, __LINE__,
						(long long unsigned) reader->statisticsHeap->buffers[i]->time,
						(long long unsigned) reader->minTime );
			
				OTF_Heap_finalize( reader->statisticsHeap );
				free( reader->statisticsHeap );
				reader->statisticsHeap= NULL;
				
				return OTF_READ_ERROR;
			}
		}


		/* for progress report: 
		cut intervals [progressMin,progressMax] and [minTime,maxTime] */
		reader->statisticsHeap->progressMin= ( reader->statisticsHeap->progressMin >= reader->minTime ) ?
			reader->statisticsHeap->progressMin : reader->minTime;
		reader->statisticsHeap->progressMax= ( reader->statisticsHeap->progressMax <= reader->maxTime ) ?
			reader->statisticsHeap->progressMax : reader->maxTime;

	}


	s_reziprok= 1/(double)reader->statisticsHeap->s;
	delta_t= reader->statisticsHeap->progressMax-reader->statisticsHeap->progressMin;


	while ( reader->statisticsHeap->n > 0 && recordcount < reader->recordLimit ) {

		/* inlined OTF_RBuffer_getCurrentTime() */

		/* calculate a "current time", which is between min and max.
		 * In this case we have to: min + ((delta)/s)*(s-n) + (curtime-min)/s
		 *
		 * s= all streams
		 * n= streams left
		 * delta= max-min
		 */
		reader->statisticsHeap->progressCurrent= (uint64_t) (
			reader->statisticsHeap->progressMin
			+
			(delta_t*s_reziprok)
			*
			(reader->statisticsHeap->s-reader->statisticsHeap->n)
			+
			(reader->statisticsHeap->buffers[0]->time-reader->statisticsHeap->progressMin)*s_reziprok );

		/* check for time interval */
		if ( reader->statisticsHeap->buffers[0]->time >= reader->maxTime ) {

			/* add the last filepos of this buffer to "bytesDone" */
			reader->statisticsHeap->bytesDone+= OTF_RBuffer_getFilePos(
				reader->statisticsHeap->buffers[0] );

			/* drop that stream */
			reader->statisticsHeap->n--;
			reader->statisticsHeap->buffers[0]= reader->statisticsHeap->buffers[reader->statisticsHeap->n];
			
			continue;
		}


		/* inlined OTF_RBuffer_getCurrentProcess() */
		currentprocess= reader->statisticsHeap->buffers[0]->process;

		/* check if process is enabled */
		if ( 0 == OTF_ProcessList_getStatus( reader->processList, currentprocess ) ) {

			/* ignore record */
			OTF_RBuffer_readNewline( reader->statisticsHeap->buffers[0] );

			/* prepare next record in that stream */
			pos= OTF_RBuffer_getRecord( reader->statisticsHeap->buffers[0] );
			if ( NULL == pos ) {

				/* test if the file is really exceeded or if there is some junk left.
				test here instead of in 'OTF_RBuffer_getRecord()'. 
				throw a warning/error in case*/

				if ( reader->statisticsHeap->buffers[0]->pos < reader->statisticsHeap->buffers[0]->end ) {

					ret= OTF_Reader_readUnknownRecord( reader->statisticsHeap->buffers[0], handlers );
					if ( 0 == ret ) {

						OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
								"OTF_Reader_readUnknownRecord() failed.\n",
								__FUNCTION__, __FILE__, __LINE__ );
						
						OTF_Heap_finalize( reader->statisticsHeap );
						free( reader->statisticsHeap );
						reader->statisticsHeap= NULL;

						return OTF_READ_ERROR;
					}
				}

				/* add the last filepos of this buffer to "bytesDone" */
				reader->statisticsHeap->bytesDone+= OTF_RBuffer_getFilePos(
					reader->statisticsHeap->buffers[0] );
					
				/* stream is empty -> remove from heap */
				reader->statisticsHeap->n--;
				reader->statisticsHeap->buffers[0]=
					reader->statisticsHeap->buffers[reader->statisticsHeap->n];
					
			}


			continue;
		}

		/* remember next record type, if it will be a none KEYVALUE
		   record, dont't account it in recordcount */
		next_char = *(reader->statisticsHeap->buffers[0]->buffer + reader->statisticsHeap->buffers[0]->pos);

		ret= OTF_Reader_parseStatisticsRecord( reader->statisticsHeap->buffers[0], handlers );
		if ( 0 == ret ) {

			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_Reader_parseStatisticsRecord() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
			
			OTF_Heap_finalize( reader->statisticsHeap );
			free( reader->statisticsHeap );
			reader->statisticsHeap= NULL;
			
			return OTF_READ_ERROR;
		}

		/* Now reset the KeyValue list, if we consumed a none KEYVALUE
		   record */
		if ( next_char != OTF_KEYWORD_F_KEYVALUE_PREFIX /* 'K' */ ) {
			OTF_KeyValueList_reset(reader->statisticsHeap->buffers[0]->list);
			recordcount++;
		}

		/* prepare next record in that stream */
		pos= OTF_RBuffer_getRecord( reader->statisticsHeap->buffers[0] );
		if ( NULL == pos ) {

			/* test if the file is really exceeded or if there is some junk left.
			test here instead of in 'OTF_RBuffer_getRecord()'. 
			throw a warning/error in case*/

			if ( reader->statisticsHeap->buffers[0]->pos < reader->statisticsHeap->buffers[0]->end ) {

				ret= OTF_Reader_readUnknownRecord( reader->statisticsHeap->buffers[0], handlers );
				if ( 0 == ret ) {

					OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
							"OTF_Reader_readUnknownRecord() failed.\n",
							__FUNCTION__, __FILE__, __LINE__ );
					
					OTF_Heap_finalize( reader->statisticsHeap );
					free( reader->statisticsHeap );
					reader->statisticsHeap= NULL;
			
					return OTF_READ_ERROR;
				}
			}

			/* add the last filepos of this buffer to "bytesDone" */
			reader->statisticsHeap->bytesDone+= OTF_RBuffer_getFilePos(
				reader->statisticsHeap->buffers[0] );
			
			/* stream is empty -> remove from heap */
			reader->statisticsHeap->n--;
			reader->statisticsHeap->buffers[0]=
				reader->statisticsHeap->buffers[reader->statisticsHeap->n];
		}

	}

	return recordcount;
}


/* this is much simple than the other 'OTF_Reader_readXXX' functions because it 
only looks at stream '0' */
uint64_t OTF_Reader_readMarkers( OTF_Reader* reader, OTF_HandlerArray* handlers ) {


	uint64_t recordcount= 0;

	int ret;
	char* pos;
	uint32_t i;

	OTF_MapEntry* entry;
	uint32_t streamId;

	char next_char = '\0';

	/* initialized? */
	if ( NULL == reader->markerHeap ) {


		/* init */

		reader->markerHeap= (OTF_Heap*) malloc( sizeof(OTF_Heap) );
		if( NULL == reader->markerHeap ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			return OTF_READ_ERROR;
		}

		if( 0 == OTF_Heap_initMarkerHeap( reader->markerHeap, reader ) ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"OTF_Heap_initDefHeap() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			OTF_Heap_finalize( reader->markerHeap );
			free( reader->markerHeap );
			reader->markerHeap= NULL;
			
			return OTF_READ_ERROR;
		}

		for ( i= 0; i < reader->markerHeap->n; i++ ) {


			if ( NULL != reader->markerHeap->buffers[i] ) {

				pos= OTF_RBuffer_getRecord( reader->markerHeap->buffers[i] );

				/* remove empty streams */
				if ( NULL == pos ) {

					/* keep original order */
					reader->markerHeap->buffers[i]= NULL;
				}
			}
		}
	}


	/* read streams */
	for ( i= 0; i < reader->markerHeap->n; i ++ ) {


		if ( NULL == reader->markerHeap->buffers[i] ) {
		
			continue;
		}

		streamId= 0;
		if ( i > 0 ) {

			entry= OTF_MasterControl_getEntryByIndex( reader->mc, i -1 );
			streamId= entry->argument;
		}

		pos= (char*) 0x2;
		while ( NULL != pos ) {

			if ( recordcount >= reader->recordLimit ) {

				/* record count limit reached, return */
				return recordcount;
			}

			/* remember next record type, if it will be a none
			   KEYVALUE record, dont't account it in recordcount */
			next_char = *(reader->markerHeap->buffers[i]->buffer + reader->markerHeap->buffers[i]->pos);

			ret= OTF_Reader_parseMarkerRecord( reader->markerHeap->buffers[i], handlers, streamId );
			if ( 0 == ret ) {

				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_Reader_parseMarkerRecord() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				OTF_Heap_finalize( reader->markerHeap );
				free( reader->markerHeap );
				reader->markerHeap= NULL;
			
				return OTF_READ_ERROR;
			}

			/* Now reset the KeyValue list, if we consumed a none
			   KEYVALUE record */
			if ( next_char != OTF_KEYWORD_F_KEYVALUE_PREFIX /* 'K' */ ) {
				OTF_KeyValueList_reset(reader->markerHeap->buffers[i]->list);
				recordcount++;
			}

			/* prepare next record in that stream */
			pos= OTF_RBuffer_getRecord( reader->markerHeap->buffers[i] );
			if ( NULL == pos ) {

				/* test if the file is really exceeded or if there is some junk left.
				test here instead of in 'OTF_RBuffer_getRecord()'. 
				throw a warning/error in case*/

				if ( reader->markerHeap->buffers[i]->pos < reader->markerHeap->buffers[i]->end ) {

					ret= OTF_Reader_readUnknownMarkerRecord( reader->markerHeap->buffers[i], handlers, streamId );
					if ( 0 == ret ) {

						OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
								"OTF_Reader_readUnknownMarkerRecord() failed.\n",
								__FUNCTION__, __FILE__, __LINE__ );
						
						OTF_Heap_finalize( reader->markerHeap );
						free( reader->markerHeap );
						reader->markerHeap= NULL;

						return OTF_READ_ERROR;
					}
				}

				/* stream is empty -> remove from heap */
				reader->markerHeap->buffers[i]= NULL;
			}
		}
	}

	return recordcount;
}



void OTF_Reader_setTimeInterval( OTF_Reader* reader, 
		uint64_t minTime, uint64_t maxTime ) {


	OTF_Reader_resetHeaps( reader );

	reader->minTime = minTime;
	reader->maxTime = maxTime;
}


uint64_t OTF_Reader_getTimeIntervalMin( OTF_Reader* reader ) {


	return reader->minTime;
}


uint64_t OTF_Reader_getTimeIntervalMax( OTF_Reader* reader ) {


	return reader->maxTime;
}


void OTF_Reader_setRecordLimit( OTF_Reader* reader, uint64_t limit ) {


	if( limit == OTF_READ_ERROR ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"limit cannot be set to %llu. Reset to OTF_READ_MAXRECORDS.\n",
				__FUNCTION__, __FILE__, __LINE__,
				(long long unsigned) limit );
	
		limit= OTF_READ_MAXRECORDS;
	}

	reader->recordLimit= limit;
}


uint64_t OTF_Reader_getRecordLimit( OTF_Reader* reader ) {


	return reader->recordLimit;
}


int OTF_Reader_closeAllStreams( OTF_Reader* reader ) {


	int ret= 1;
	uint32_t i;
		
	for ( i = 0; i <  reader->n ; ++i ) {

		ret&= OTF_RStream_close( reader->stream[i] );
	}

	reader->n = 0;
	
	return ret;
}


uint8_t OTF_Reader_eventProgress( OTF_Reader* reader, uint64_t* minimum,
	uint64_t* current, uint64_t* maximum ) {
	
	
	return OTF_Reader_eventTimeProgress( reader, minimum, current, maximum );
}

	
uint8_t OTF_Reader_snapshotProgress( OTF_Reader* reader, 
		uint64_t* minimum, uint64_t* current, uint64_t* maximum ) {
		
		
	return OTF_Reader_snapshotTimeProgress( reader, minimum, current, maximum );
}

		
uint8_t OTF_Reader_statisticProgress( OTF_Reader* reader,
	uint64_t* minimum, uint64_t* current, uint64_t* maximum ) {
	
	
	return OTF_Reader_statisticTimeProgress( reader, minimum, current, maximum );
}


uint8_t OTF_Reader_eventTimeProgress( OTF_Reader* reader, 
		uint64_t* minimum, uint64_t* current, uint64_t* maximum ) {


	if ( NULL == reader->eventHeap ||
		reader->eventHeap->progressCurrent < reader->eventHeap->progressMin ||
		reader->eventHeap->progressCurrent > reader->eventHeap->progressMax ) {
	
		return 0;
	}

	*minimum= reader->eventHeap->progressMin;
	*current= reader->eventHeap->progressCurrent;
	*maximum= reader->eventHeap->progressMax;


	return 1;
}


uint8_t OTF_Reader_snapshotTimeProgress( OTF_Reader* reader, 
		uint64_t* minimum, uint64_t* current, uint64_t* maximum ) {


	if ( NULL == reader->snapshotsHeap ||
		reader->snapshotsHeap->progressCurrent < reader->snapshotsHeap->progressMin ||
		reader->snapshotsHeap->progressCurrent > reader->snapshotsHeap->progressMax ) {
	
		return 0;
	}

	*minimum= reader->snapshotsHeap->progressMin;
	*current= reader->snapshotsHeap->progressCurrent;
	*maximum= reader->snapshotsHeap->progressMax;

	return 1;
}


uint8_t OTF_Reader_statisticTimeProgress( OTF_Reader* reader, 
		uint64_t* minimum, uint64_t* current, uint64_t* maximum ) {


	if ( NULL == reader->statisticsHeap ||
		reader->statisticsHeap->progressCurrent < reader->statisticsHeap->progressMin ||
		reader->statisticsHeap->progressCurrent > reader->statisticsHeap->progressMax ) {
	
		return 0;
	}

	*minimum= reader->statisticsHeap->progressMin;
	*current= reader->statisticsHeap->progressCurrent;
	*maximum= reader->statisticsHeap->progressMax;
	
	return 1;
}


uint8_t OTF_Reader_eventBytesProgress( OTF_Reader* reader, uint64_t* minimum,
	uint64_t* current, uint64_t* maximum ) {
	
	
	uint32_t i;
	uint64_t pos;
	
	*minimum= reader->eventHeap->bytesMin;
 	*maximum= reader->eventHeap->bytesMax;
 	*current= reader->eventHeap->bytesDone;
 	
 	for( i= 0; i < reader->eventHeap->n; ++i ) {
 	
 		pos= OTF_RBuffer_getFilePos( reader->eventHeap->buffers[i] );
 		*current+= pos;
 	}
 	
 	if( *current > *maximum ) {
 		*current= *maximum;
 	}
	
	return 1;
}


uint8_t OTF_Reader_snapshotBytesProgress( OTF_Reader* reader, 
		uint64_t* minimum, uint64_t* current, uint64_t* maximum ) {
		
		
	uint32_t i;
	uint64_t pos;
	
	*minimum= reader->snapshotsHeap->bytesMin;
 	*maximum= reader->snapshotsHeap->bytesMax;
 	*current= reader->snapshotsHeap->bytesDone;
 	
 	for( i= 0; i < reader->snapshotsHeap->n; ++i ) {
 	
 		pos= OTF_RBuffer_getFilePos( reader->snapshotsHeap->buffers[i] );
 		*current+= pos;
 	}
 	
 	if( *current > *maximum ) {
 		*current= *maximum;
 	}
	
	return 1;
}


uint8_t OTF_Reader_statisticBytesProgress( OTF_Reader* reader,
	uint64_t* minimum, uint64_t* current, uint64_t* maximum ) {
	
	
	uint32_t i;
	uint64_t pos;
	
	*minimum= reader->statisticsHeap->bytesMin;
 	*maximum= reader->statisticsHeap->bytesMax;
 	*current= reader->statisticsHeap->bytesDone;
 	
 	for( i= 0; i < reader->statisticsHeap->n; ++i ) {
 	
 		pos= OTF_RBuffer_getFilePos( reader->statisticsHeap->buffers[i] );
 		*current+= pos;
 	}
 	
 	if( *current > *maximum ) {
 		*current= *maximum;
 	}
	
	return 1;
}
