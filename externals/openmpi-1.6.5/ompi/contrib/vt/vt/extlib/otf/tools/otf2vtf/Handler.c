/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include <stdlib.h>
#include <assert.h>

#include "otf.h"

#include "vtf3.h"

#include "Treehash.h"

#include "Handler.h"

#ifdef __GNUC__
#else /* __GNUC__ */
#define __PRETTY_FUNCTION__ "<unspecified function>"
#endif /* __GNUC__ */



/* *** Definition handler *** ************************************* */


int handleDefinitionComment( void* firsthandlerarg, uint32_t streamid,
		const char* comment ) {


	return ( 0 == VTF3_WriteComment( ((fcbT*) firsthandlerarg)->fcb, 0, comment ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleDeftimerresolution( void* firsthandlerarg, uint32_t streamid,
		uint64_t ticksPerSecond ) {


	return ( 0 == VTF3_WriteDefclkperiod( ((fcbT*) firsthandlerarg)->fcb,
		1.0  / (double) ticksPerSecond ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleDefprocess( void* firsthandlerarg, uint32_t streamid,
		uint32_t deftoken, const char* name, uint32_t parent ) {
	
	
	treehash_addnode ( ((fcbT*) firsthandlerarg)->p_hashtab, deftoken, name,
		parent );

	return OTF_RETURN_OK;
}


int handleDefprocessgroup( void* firsthandlerarg, uint32_t streamid,
		uint32_t deftoken, const char* name, uint32_t n, uint32_t* array ) {


	unsigned int* tripletarray;
	uint32_t i;


	/* translate DefProcessgroup to VTF3 DefCommunicator although this is 
	ambiguous, it could be used as a DefCpuGrp as well. */


	/* do a very simple tranlation from an ordinary array to a triplet array 
	like used in vtf3 */
	tripletarray= malloc( 3 * n * sizeof(unsigned int) );
	assert( tripletarray );
	for ( i= 0; i < n; i++ ) {

		tripletarray[3*i+0]= array[i];
		tripletarray[3*i+1]= array[i];
		tripletarray[3*i+2]= 1;
	}

	VTF3_WriteDefcommunicator( ((fcbT*) firsthandlerarg)->fcb,
    	deftoken /* int communicator */,
		n /* int communicatorsize */,
        n /* int tripletarraydim */,
		tripletarray /* const unsigned int *tripletarray */ );

	free( tripletarray );

	return OTF_RETURN_OK;
}


int handleDeffunction(  void* firsthandlerarg, uint32_t streamid,
		uint32_t deftoken, const char* name, uint32_t group,
		uint32_t scltoken ) {


	return ( 0 == VTF3_WriteDefstate( ((fcbT*) firsthandlerarg)->fcb, group, deftoken,
		name, scltoken ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleDeffunctiongroup( void* firsthandlerarg, uint32_t streamid,
		uint32_t deftoken, const char* name ) {


	return ( 0 == VTF3_WriteDefact( ((fcbT*) firsthandlerarg)->fcb, deftoken, name ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleDefcounter( void* firsthandlerarg, uint32_t streamid,
		uint32_t deftoken, const char* name, uint32_t properties, 
		uint32_t countergroup, const char* unit ) {


	uint64_t valuebounds[2]= { (uint64_t) 0, (uint64_t) -1 };

	int dodifferentiation;
	int datarephint;


	static int first= 1;
	
		if ( 1 == first ) {
			first= 0;
			fprintf( stderr, "translation of performance counter "
				"definitions not fully supported\n" );
		}
		
	dodifferentiation= 
		( OTF_COUNTER_TYPE_ACC == ( properties & OTF_COUNTER_TYPE_BITS ) ) ? 1 : 0;
		
	if ( OTF_COUNTER_SCOPE_START == ( properties & OTF_COUNTER_SCOPE_BITS ) ) {
		
		datarephint = VTF3_DATAREPHINT_SAMPLE;	

	} else if ( OTF_COUNTER_SCOPE_LAST == ( properties & OTF_COUNTER_SCOPE_BITS	) ) {
	
		datarephint = VTF3_DATAREPHINT_BEFORE;

	} else {

		datarephint = VTF3_DATAREPHINT_SAMPLE; /* standard value */
		
		fprintf( stderr, "otf2vtf: %s WARNING for counter def %u: "
			"counter type not supported\n", __PRETTY_FUNCTION__, deftoken );
	}

	return ( 0 == VTF3_WriteDefsamp( ((fcbT*) firsthandlerarg)->fcb,
		deftoken /* int sampletoken */,
		countergroup /* int sampleclasstoken */,
		0 /* int iscpugrpsamp */,
		0 /* unsigned int cpuorcpugrpid */,
		VTF3_VALUETYPE_UINT /* int valuetype */,
		valuebounds /* const void *valuebounds */,
		dodifferentiation /* int dodifferentiation */,
		datarephint /* int datarephint */,
		name /* const char *samplename */,
		unit /* const char *sampleunit  */ ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleDefcountergroup( void* firsthandlerarg, uint32_t streamid,
		uint32_t deftoken, const char* name ) {


	return ( 0 == VTF3_WriteDefsampclass( ((fcbT*) firsthandlerarg)->fcb, deftoken,
		name ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleDefCollectiveOperation( void* firsthandlerarg, uint32_t streamid,
	uint32_t deftoken, const char* name, uint32_t type ) {


	return ( 0 == VTF3_WriteDefglobalop( ((fcbT*) firsthandlerarg)->fcb, deftoken,
		name ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleDefscl(  void* firsthandlerarg, uint32_t streamid,
		uint32_t deftoken, uint32_t sclfile, uint32_t sclline ) {


	int sclfile2= sclfile;
	int sclline2= sclline;


	return ( 0 == VTF3_WriteDefscl( ((fcbT*) firsthandlerarg)->fcb, deftoken, 1,
		&sclfile2, &sclline2 ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleDefsclfile(  void* firsthandlerarg, uint32_t streamid,
		uint32_t deftoken, const char* filename ) {


	return ( 0 == VTF3_WriteDefsclfile( ((fcbT*) firsthandlerarg)->fcb, deftoken,
		filename ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleDefFile( void* firsthandlerarg, uint32_t streamid,
	uint32_t token, const char* name, uint32_t group ) {


	return ( 0 == VTF3_WriteDefiofile( ((fcbT*) firsthandlerarg)->fcb, (int)token-1,
		VTF3_NOCOMMUNICATOR, name ) )  ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}
	


/* *** Event handler *** ****************************************** */


int handleEventComment( void* firsthandlerarg, uint64_t time, 
		const char* comment ) {


	FileIOEndQueue_check( &(((fcbT*) firsthandlerarg)->FileIOQueue), time, firsthandlerarg );


	return ( 0 == VTF3_WriteComment( ((fcbT*) firsthandlerarg)->fcb, time, comment ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleCounter( void* firsthandlerarg, uint64_t time, uint32_t process,
		uint32_t token, uint64_t value ) {


	int sampletokenarray= token;
	int samplevaluetypearray= VTF3_VALUETYPE_UINT;
	uint64_t samplevaluearray= value;

	nodeT *node = treehash_searchnode( ((fcbT*) firsthandlerarg)->p_hashtab, process );

	if ( NULL == node ) {

		fprintf( stderr, "otf2vtf WARNING: undefined process %u, record ignored\n",
			process );
			
		return 0;
	}

	FileIOEndQueue_check( &(((fcbT*) firsthandlerarg)->FileIOQueue), time, firsthandlerarg );
	

	return ( 0 == VTF3_WriteSamp( ((fcbT*) firsthandlerarg)->fcb, time,
		node->processi, 1, &sampletokenarray, &samplevaluetypearray, &samplevaluearray ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleEnter( void* firsthandlerarg, uint64_t time, uint32_t statetoken,
		uint32_t cpuid, uint32_t scltoken ) {


	nodeT *node = treehash_searchnode(((fcbT*) firsthandlerarg)->p_hashtab,
		cpuid);

	if ( 0 == node ) {

		fprintf( stderr, "otf2vtf WARNING: undefined process %u, record ignored\n",
			cpuid );
			
		return OTF_RETURN_OK;
	}


    while (node->stacks <= node->stackc) {
        node->stacks += STACK_ALLOC_WIDTH;
        node->stack = realloc(node->stack,sizeof(uint32_t)*node->stacks);
        assert(node->stack);
    }

    node->stack[node->stackc] = statetoken;
    ++node->stackc;

	FileIOEndQueue_check( &(((fcbT*) firsthandlerarg)->FileIOQueue), time, firsthandlerarg );
	

	/* 1st test ********************************************/
	return ( 0 == VTF3_WriteDownto( ((fcbT*) firsthandlerarg)->fcb, time, statetoken,
		node->processi, scltoken ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleCollectiveOperation( void* firsthandlerarg, uint64_t time, 
		uint32_t process, uint32_t globaloptoken, uint32_t communicator, 
		uint32_t rootprocess, uint32_t sent, uint32_t received, 
		uint64_t duration, uint32_t scltoken ) {


	nodeT *node = treehash_searchnode(((fcbT*) firsthandlerarg)->p_hashtab,
		process);
        
    nodeT *root = treehash_searchnode(((fcbT*) firsthandlerarg)->p_hashtab,
        rootprocess);

	if ( 0 == node ) {

		fprintf( stderr, "otf2vtf WARNING: undefined process %u, record ignored\n",
			process );
			
		return OTF_RETURN_OK;
	}

	FileIOEndQueue_check( &(((fcbT*) firsthandlerarg)->FileIOQueue), time, firsthandlerarg );
	

	return ( 0 == VTF3_WriteGlobalop( ((fcbT*) firsthandlerarg)->fcb, time,
		globaloptoken, node->processi, communicator,
		root->processi, sent, received, duration, scltoken ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleRecvmsg( void* firsthandlerarg, uint64_t time, uint32_t receiver,
		uint32_t sender, uint32_t communicator, uint32_t msgtype, 
		uint32_t msglength, uint32_t scltoken ) {


	nodeT *nodesender = treehash_searchnode(
		((fcbT*) firsthandlerarg)->p_hashtab, sender);
	nodeT *nodereceiver = treehash_searchnode(
		((fcbT*) firsthandlerarg)->p_hashtab, receiver);

	if ( 0 == nodesender || 0 == nodereceiver ) {

		fprintf( stderr, "otf2vtf WARNING: undefined process %u or %u, record ignored\n",
			sender, receiver );
			
		return OTF_RETURN_OK;
	}

	FileIOEndQueue_check( &(((fcbT*) firsthandlerarg)->FileIOQueue), time, firsthandlerarg );
	

	return ( 0 == VTF3_WriteRecvmsg( ((fcbT*) firsthandlerarg)->fcb, time,
		nodereceiver->processi, nodesender->processi,
		communicator, msgtype, msglength, scltoken ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleSendmsg( void* firsthandlerarg, uint64_t time, uint32_t sender,
		uint32_t receiver, uint32_t communicator, uint32_t msgtype, 
		uint32_t msglength, uint32_t scltoken ) {


	nodeT *nodesender = treehash_searchnode(
		((fcbT*) firsthandlerarg)->p_hashtab, sender );
	nodeT *nodereceiver = treehash_searchnode(
		((fcbT*) firsthandlerarg)->p_hashtab, receiver);

	if ( 0 == nodesender || 0 == nodereceiver ) {

		fprintf( stderr, "otf2vtf WARNING: undefined process %u or %u, record ignored\n",
			sender, receiver );
			
		return OTF_RETURN_OK;
	}

	FileIOEndQueue_check( &(((fcbT*) firsthandlerarg)->FileIOQueue), time, firsthandlerarg );


	return ( 0 == VTF3_WriteSendmsg( ((fcbT*) firsthandlerarg)->fcb, time,
		nodesender->processi, nodereceiver->processi, communicator, msgtype,
		msglength, scltoken ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int handleLeave( void* firsthandlerarg, uint64_t time, uint32_t statetoken,
		uint32_t cpuid, uint32_t scltoken ) {
        

    uint32_t savedstatetoken;


	nodeT *node = treehash_searchnode(((fcbT*) firsthandlerarg)->p_hashtab,
		cpuid);

	if ( 0 == node ) {

		fprintf( stderr, "otf2vtf WARNING: undefined process %u, record ignored\n",
			cpuid );
			
		return OTF_RETURN_OK;
	}
    
    savedstatetoken = node->stack[node->stackc-1];

    assert(node->stackc);
    --node->stackc;

    if( statetoken == 0 ) {
        statetoken = savedstatetoken;
    }

	FileIOEndQueue_check( &(((fcbT*) firsthandlerarg)->FileIOQueue), time, firsthandlerarg );
	

	return ( 0 == VTF3_WriteUpfrom( ((fcbT*) firsthandlerarg)->fcb, time, statetoken,
		node->processi, scltoken ) )
		? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleBeginProcess( void* firsthandlerarg, uint64_t time,
		uint32_t process ) {


	fprintf( stderr, "otf2vtf: %s not yet implemented\n", __PRETTY_FUNCTION__ );

	return OTF_RETURN_OK;
}

int handleEndProcess( void* firsthandlerarg, uint64_t time,
		uint32_t process ) {


	fprintf( stderr, "otf2vtf: %s not yet implemented\n", __PRETTY_FUNCTION__ );

	return OTF_RETURN_OK;
}


int handleFileOperation( void* firsthandlerarg, uint64_t time, uint32_t fileid,
	uint32_t cpuid, uint64_t handleid, uint32_t operation, uint64_t bytes,
	uint64_t duration, uint32_t source ) {


	int iotype= VTF3_FILEIOTYPE_NOTYPE;
	int vtf3bytes;
	FileIOEndRecord* fier;

	nodeT *node = treehash_searchnode(((fcbT*) firsthandlerarg)->p_hashtab,
		cpuid);
	

	if( OTF_FILEOP_READ == operation ) {

		iotype= VTF3_FILEIOTYPE_READ;

	} else if ( OTF_FILEOP_WRITE == operation ) {

		iotype= VTF3_FILEIOTYPE_WRITE;

	}

	if( VTF3_FILEIOTYPE_NOTYPE != iotype ) {

		FileIOEndQueue_check( &(((fcbT*) firsthandlerarg)->FileIOQueue), time, firsthandlerarg );
		

		vtf3bytes= ( bytes > (uint64_t) ((unsigned int)(1<<31)) - 1) ?
			(int)((unsigned int)(1<<31)-1) : (int) bytes;

		VTF3_WriteFileiobegin( ((fcbT*) firsthandlerarg)->fcb, time, node->processi,
			iotype, (int) fileid-1, vtf3bytes, (int)source );
			

		fier= (FileIOEndRecord*) malloc( sizeof(FileIOEndRecord) );
		assert( fier );

		fier->time= time + duration;
		fier->process= node->processi;
		fier->iotype= iotype;
		fier->fileid= (int) fileid-1;
		fier->bytes= vtf3bytes;
		fier->source= (int) source;
		fier->next= NULL;

		FileIOEndQueue_push( &(((fcbT*) firsthandlerarg)->FileIOQueue), fier );
	}
	

	return OTF_RETURN_OK;
}
