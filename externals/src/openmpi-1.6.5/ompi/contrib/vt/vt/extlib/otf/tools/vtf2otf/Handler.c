/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include <stdio.h>

#include "OTF_inttypes.h"
#include "OTF_Platform.h"

#include "otf.h"

#ifdef HAVE_VTF3
#include "vtf3.h"
#endif /* HAVE_VTF3 */

#include "Handler.h"
#include "Stack.h"


#ifdef __GNUC__
#else /* __GNUC__ */
#define __PRETTY_FUNCTION__ "<unspecified function>"
#endif /* __GNUC__ */


#ifdef HAVE_VTF3


int handleClstrregval( void *fcb, double time, int clstrtoken,
		int clstrregarraydim, const int *clstrregtokenarray,
		const int *clstrregvaluetypearray,
		const void *clstrregvaluearray ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


int handleComment( void *fcb, double time, const char *comment ) {


	/** always write a VTF3 comment to an OTF DefinitionComment. a time stamp 
	in the VTF3 comment record is ignored it */
	OTF_Writer_writeDefinitionComment( ((fcbT*)fcb)->writer, 0, comment );


	/* old version:
	if ( time == 0 )
		OTF_Writer_writeDefinitionComment( fcb->writer, 0, comment );
	else

		/ *  which process to write the EventComment to * /

		OTF_Writer_writeEventComment( fcb->writer, time, ??? , comment );
	*/

	return 0;
}


int handleCpuregval( void *fcb, double time, unsigned int cpuid,
		int cpuregarraydim, const int *cpuregtokenarray,
		const int *cpuregvaluetypearray,
		const void *cpuregvaluearray ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


int handleDefact( void *fcb, int activitytoken,
		const char *activityname ) {


	OTF_Writer_writeDefFunctionGroup( ((fcbT*)fcb)->writer, 0,
		activitytoken + 1, activityname );

	return 0;
}


int handleDefact_obsol( void *fcb, int activitytoken,
	const char *activityname ) {


	OTF_Writer_writeDefFunctionGroup( ((fcbT*)fcb)->writer, 0,
		activitytoken + 1, activityname );

	return 0;
}


int handleDefclkperiod( void *fcb, double clkperiod ) {


	/* ' + 0.5' makes proper rounding */

	OTF_Writer_writeDefTimerResolution( ((fcbT*)fcb)->writer, 0, 
		(uint64_t) ( 1.0 / clkperiod + 0.5 ) );

	return 0;
}


int handleDefclstr( void *fcb, int clstrtoken, const char *clstrname,
		int cpuidarraydim, const unsigned int *cpuidarray ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


int handleDefclstrreg( void *fcb, int clstrregtoken,
		int clstrregclasstoken, int valuetype, const void *valuebounds, 
		const char *clstrregname, const char *clstrregunit ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


int handleDefclstrregclass( void *fcb, int clstrregclasstoken,
		const char *clstrregclassname ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


int handleDefcommunicator( void *fcb, int communicator,
		int communicatorsize, int tripletarraydim,
		const unsigned int *tripletarray ) {


	uint32_t* procs;
	uint32_t p;
	uint32_t i;
	uint32_t j;
	char tmp[32];
	/* set first two bits to 1 */
	uint32_t mcommunicator= ((uint32_t) communicator) | 0xc0000000;
	int existing;
	
	fcbT *fha= ((fcbT*)fcb);


	if ( 0 > communicator ) {

		fprintf( stderr, "WARNING in '%s': "
			"VTF3 communicator id '%i' < 0 invalid\n", "handleDefcommunicator", communicator );
	}
	
	/* add the mapping */
	existing= 1;
	while( 1 == existing ) {
	
		existing= 0;
	
		for( i= 0; i < (uint32_t)fha->reservedIdsc; ++i ) {
		
			if( fha->reservedIds[i] == mcommunicator ) {
				existing= 1;
				++mcommunicator;
				break;
			}
		}
	}
	addHash( fha->pghash, (uint32_t) communicator, mcommunicator );
	
	fha->reservedIds= (uint32_t *) realloc( fha->reservedIds, sizeof( uint32_t )
		* ( fha->reservedIdsc + 1 ) );
	fha->reservedIds[fha->reservedIdsc]= mcommunicator;
	++fha->reservedIdsc;
	
	
	procs= (uint32_t*) malloc( communicatorsize * sizeof(uint32_t) );
	assert( procs );

	p= 0;
	for ( i= 0; i < (uint32_t)tripletarraydim; i++ ) {

		for ( j= tripletarray[3*i+0]; j <= tripletarray[3*i+1]; j += tripletarray[3*i+2] ) {
		
			assert( p < (uint32_t)communicatorsize );
			procs[p]= j + 1;
			p++;
		}
	}

	assert( p == (uint32_t)communicatorsize );

	fha->processgroups= ( ProcessGroup* ) realloc( fha->processgroups,
		sizeof(ProcessGroup) * (fha->processgroupcount + 1) );
	assert( NULL != fha->processgroups );

	sprintf( tmp, "communicator %u", mcommunicator& 0x3fffffff );
	fha->processgroups[fha->processgroupcount].id= mcommunicator;
	fha->processgroups[fha->processgroupcount].size= communicatorsize;
	fha->processgroups[fha->processgroupcount].procs= procs;
	fha->processgroups[fha->processgroupcount].name= strdup( tmp );

	fha->processgroupcount++;

	return 0;
}


int handleDefcpugrp( void *fcb, unsigned int cpugrpid,
		int cpuorcpugrpidarraydim, const unsigned int *cpuorcpugrpidarray,
		const char *cpugrpname ) {


	uint32_t* procs;
	uint32_t i;
	int existing;
	uint32_t tmp;
	/* set first bit to 1 */
	uint32_t mpg= (uint32_t) cpugrpid | 0x80000000;

	fcbT *fha= ((fcbT*)fcb);


	if ( ( 1 << 31 ) > (int)cpugrpid ) {

		fprintf( stderr, "WARNING in '%s': "
			"VTF3 cpu group id '%u' < 2^31 invalid\n", "handleDefcpugrp", cpugrpid );
	}


	/* add the mapping */
	existing= 1;
	while( 1 == existing ) {
	
		existing= 0;
	
		for( i= 0; i < (uint32_t)fha->reservedIdsc; ++i ) {
		
			if( fha->reservedIds[i] == mpg ) {
				existing= 1;
				++mpg;
				break;
			}
		}
	}
	addHash( fha->pghash, (uint32_t) cpugrpid, mpg );
	
	fha->reservedIds= (uint32_t *) realloc( fha->reservedIds, sizeof( uint32_t )
		* ( fha->reservedIdsc + 1 ) );
	fha->reservedIds[fha->reservedIdsc]= mpg;
	++fha->reservedIdsc;
	
	procs= (uint32_t*) malloc( cpuorcpugrpidarraydim * sizeof(uint32_t) );
	assert( procs );

	for ( i= 0; i < (uint32_t)cpuorcpugrpidarraydim; i++ ) {

		if( 0 == (cpuorcpugrpidarray[i]&0x80000000) ) {
		
			procs[i]= cpuorcpugrpidarray[i] +1;
			
		} else {
		
			tmp= searchHash( fha->pghash, cpuorcpugrpidarray[i] );
			if( 0 != tmp ) {
			
				procs[i]= tmp;
				
			} else {
			
				procs[i]= cpuorcpugrpidarray[i];
			}
		}
	}

	fha->processgroups= ( ProcessGroup* ) realloc( fha->processgroups,
		sizeof(ProcessGroup) * (fha->processgroupcount + 1) );
	assert( NULL != fha->processgroups );

	fha->processgroups[fha->processgroupcount].id= mpg;
	fha->processgroups[fha->processgroupcount].size= cpuorcpugrpidarraydim;
	fha->processgroups[fha->processgroupcount].procs= procs;
	fha->processgroups[fha->processgroupcount].name= strdup( cpugrpname );

	fha->processgroupcount++;

	return 0;
}


int handleDefcpuname( void *fcb, unsigned int cpuid,
		const char *cpuname ) {


	int cpu = cpuid&0xffff;
	int thread = (cpuid>>16)&0xffff;


	if ( ((fcbT*)fcb)->threadnums[cpu] < thread ) {
		
		fprintf( stderr,
			"WARNING: Thread has not been defined, record ignored\n" );
			
		return 0;
	}
	
	((fcbT*)fcb)->processes[cpu][thread].name = strdup( cpuname );
	
	return 0;
}


int handleDefcpureg( void *fcb, int cpuregtoken, int cpuregclasstoken,
		int valuetype, const void *valuebounds,
		const char *cpuregname, const char *cpuregunit ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


int handleDefcpuregclass( void *fcb, int cpuregclasstoken,
		const char *cpuregclassname ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


/*
int handleDefcreator( void *fcbout, const char *creator )
{
	OTF_Writer_writeDefCreator( ((fcbT*)fcb)->writer, VTF32OTFCREATOR );

	return 0;
}
*/



int handleDefglobalop( void *fcb, int globaloptoken,
		const char* globalopname ) {


	OTF_Writer_writeDefCollectiveOperation( ((fcbT*)fcb)->writer, 
		0 /* uint32_t stream */, 
		globaloptoken + 1/* uint32_t collOp */, 
		globalopname /* const char* name */, 
		OTF_COLLECTIVE_TYPE_UNKNOWN /* uint32_t type */ );

	return 0;
}



int handleDefiofile( void *fcb, int iofiletoken, int communicator,
		const char *iofilename ) {


	OTF_Writer_writeDefFile( ((fcbT*)fcb)->writer, 0/*stream*/,
		iofiletoken+1/*token (+1 because 0 would be an invalid otf-token)*/,
		iofilename/*name*/, 1/*default group*/ );


	return 0;
}


int handleDefkparreg( void *fcb, int kparregtoken,
		const char *kparregname ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


int handleDefmsgname( void *fcb, int msgtype, int communicator,
		const char *msgname ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


int handleDefopenmpname( void *fcb, unsigned int nametoken,
		const char *openmpname ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


int handleDefopenmptype( void *fcb, unsigned int constructtypetoken,
		const char *constructtypename ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


int handleDefpattern( void *fcb, int activitytoken, int pattoken,
		int patshptoken, double radius, int ratio,
		int timesteparraydim, const double *timesteparray ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


int handleDefpatternshape( void *fcb, int activitytoken, int patshptoken,
		int patterntype, int patshptokenbref1, int patshptokenbref2 ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


int handleDefredfunc_obsol( void *fcb, int redfunctoken,
		const char *redfuncname ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


int handleDefsamp( void *fcb, int sampletoken, int sampleclasstoken,
		int iscpugrpsamp, unsigned int cpuorcpugrpid, int valuetype,
		const void *valuebounds, int dodifferentiation,
		int datarephint, const char *samplename, const char *sampleunit ) {


	/* default setting */
	uint32_t properties;

	static int first= 1;

	if ( 1 == first ) {
	
		first = 0;

		/* temporarily disabled */
		/*
		fprintf( stderr, "translation of performance counter "
			"definitions not fully supported\n" );
		*/
	}
		
	if ( datarephint == VTF3_DATAREPHINT_SAMPLE ) {
		
		properties = OTF_COUNTER_SCOPE_START;

	} else if ( datarephint == VTF3_DATAREPHINT_BEFORE ) {
		
		properties = OTF_COUNTER_SCOPE_LAST;

	} else {
	
		properties = OTF_COUNTER_SCOPE_START; /* standard value */
		
		fprintf( stderr, "vtf2otf: in %s: counter type not yet supported,"
			"might not be translated correctly!!!\n", __PRETTY_FUNCTION__ );
	}

	if ( dodifferentiation ) {
	
		properties |= OTF_COUNTER_TYPE_ACC;

	} else {

		properties |= OTF_COUNTER_TYPE_ABS;
	}

	OTF_Writer_writeDefCounter( ((fcbT*)fcb)->writer, 0, sampletoken + 1,
		samplename, properties, sampleclasstoken + 1, sampleunit );

	return 0;
}


int handleDefsampclass( void *fcb, int sampleclasstoken,
		const char *sampleclassname ) {


	OTF_Writer_writeDefCounterGroup( ((fcbT*)fcb)->writer, 0,
		sampleclasstoken + 1, sampleclassname );

	return 0;
}


int handleDefscl( void *fcb, int scltoken, int sclarraydim,
		const int *sclfiletokenarray, const int *scllinepositionarray ) {


	int i;


	for( i = 0; i < sclarraydim; ++i ) {


		OTF_Writer_writeDefScl( ((fcbT*)fcb)->writer, 0, scltoken,
		sclfiletokenarray[i] + 1, scllinepositionarray[i] );
	}

	return 0;
}

 
int handleDefsclfile( void *fcb, int sclfiletoken,
		const char *sclfilename ) {


	OTF_Writer_writeDefSclFile( ((fcbT*)fcb)->writer, 0, sclfiletoken + 1,
		sclfilename );

	return 0;
}


int handleDefstate( void *fcb, int activitytoken, int statetoken, 
		const char *statename, int scltoken ) {


	OTF_Writer_writeDefFunction( ((fcbT*)fcb)->writer, 0, ++statetoken,
		statename, activitytoken + 1, scltoken );

	return 0;
}


int handleDefstate_obsol( void *fcb, int activitytoken, 
		const char *activityname, unsigned int activityvalidity,
		int statetoken, const char *statename ) {


	OTF_Writer_writeDefFunction( ((fcbT*)fcb)->writer, 0, statetoken + 1,
		statename, activitytoken + 1, 0 );

	return 0;
}


int handleDefsyscpunames( void *fcb, int systemcpunamearraydim,
		char * const *systemcpunamearray ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}
 

int handleDefsyscpunums( void *fcb, int systemcpunumberarraydim,
		const int *systemcpunumberarray ) {


	int a = 0;
	int i = 0;


	((fcbT*)fcb)->processcount = 0;

	for( i = 0; i < systemcpunumberarraydim; i++)
		for( a = 0; a < systemcpunumberarray[i]; a++)
			((fcbT*)fcb)->processcount++;

	/* create cpu array (needed for threadnums and carrying the names) */
	((fcbT*)fcb)->processes = ( processT** ) malloc ( sizeof( processT* )
		* ((fcbT*)fcb)->processcount );
	((fcbT*)fcb)->threadnums = (int*) malloc( sizeof( int )
		* ((fcbT*)fcb)->processcount );

	for( i = 0; i < ((fcbT*)fcb)->processcount; i++) {


		((fcbT*)fcb)->threadnums[i] = 1;
		((fcbT*)fcb)->processes[i] = ( processT* ) malloc ( sizeof (processT) );
		((fcbT*)fcb)->processes[i][0].stack = Stack_new();
		((fcbT*)fcb)->processes[i][0].name = 0;
		((fcbT*)fcb)->processes[i][0].filepos= 0;
		((fcbT*)fcb)->processes[i][0].fileIObegin = (uint64_t) -1; /* invalid timestamp */
		((fcbT*)fcb)->processes[i][0].fileIObufferpos = 0;
		((fcbT*)fcb)->processes[i][0].fileIObufferlen = 0;
	}

	return 0;
}


int handleDefthreadnums( void *fcb, int threadnumarraydim,
		const int *threadnumarray ) {


	int i;
	int a;
	processT **processestmp;


	if ( 0 == ((fcbT*)fcb)->processes ) {

			fprintf( stderr, "ERROR: Missing NCPU record\n" );
			exit(1);
	}


	/* copy threadnumarray */
	free( ((fcbT*)fcb)->threadnums );

	((fcbT*)fcb)->threadnums = (int*) malloc( sizeof( int )
		* threadnumarraydim );
	memcpy( ((fcbT*)fcb)->threadnums, threadnumarray, sizeof( int )
		* threadnumarraydim );

	processestmp = ( processT** ) malloc ( sizeof( processT* )
		* ((fcbT*)fcb)->processcount );

	for( i = 0; i < threadnumarraydim; ++i ) {

		processestmp[i] = ( processT* ) malloc ( sizeof (processT)
			* threadnumarray[i]);

		processestmp[i][0].stack = ((fcbT*)fcb)->processes[i][0].stack;
		processestmp[i][0].name = ((fcbT*)fcb)->processes[i][0].name;
		processestmp[i][0].fileIObegin = (uint64_t) -1;
		processestmp[i][0].filepos = 0;
		processestmp[i][0].fileIObufferpos = 0;
		processestmp[i][0].fileIObufferlen = 0;
		/* remove pointers from original array (to prevent free errors later )*/
		((fcbT*)fcb)->processes[i][0].name = 0;
		((fcbT*)fcb)->processes[i][0].stack = 0;
		/* init the threads */
		for( a = 1; a < threadnumarray[i]; ++a ) {

			processestmp[i][a].stack = Stack_new();
			processestmp[i][a].name = 0;
			processestmp[i][a].fileIObegin = (uint64_t) -1; /* invalid timestamp */
			processestmp[i][a].filepos = 0;
			processestmp[i][a].fileIObufferpos = 0;
			processestmp[i][a].fileIObufferlen = 0;
		}
	}

	/* delete old processarray */
	for( i = 0; i < ((fcbT*)fcb)->processcount; ++i ) {

		free( ((fcbT*)fcb)->processes[i] );
	}

	free( ((fcbT*)fcb)->processes );

	((fcbT*)fcb)->processes = processestmp;
	((fcbT*)fcb)->processcount = threadnumarraydim;

	return 0;
}
 

int handleDeftimeoffset( void *fcb, double timeoffset ) {


	static int first= 1;

	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}
 

int handleDefunmerged( void *fcb ) {


	static int first= 1;

	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


/*
int handleDefversion( void *fcbout, int versionnumber )
{
	OTF_Writer_writeDefversion( (OTF_Writer*) fcbout, versionnumber, 0, 0 );

	return 0;
}
*/


int handleDownto( void *fcb, double time, int statetoken,
		unsigned int cpuid, int scltoken ) {


	StackEntryT entry;

	int cpu = (cpuid&0xffff);
	int thread = (cpuid>>16)&0xffff;


	++cpuid;
	++statetoken;

	OTF_Writer_writeEnter( ((fcbT*)fcb)->writer, (uint64_t) time, statetoken,
		cpuid, scltoken );

	entry.state = statetoken;

	Stack_push( ((fcbT*)fcb)->processes[cpu][thread].stack, &entry );

	return 0;
}
 

int handleUpfrom( void *fcb, double time, int statetoken,
		unsigned int cpuid, int scltoken ) {


	int cpu = cpuid&0xffff;
	int thread = (cpuid>>16)&0xffff;
	/* StackEntry entry; */ 


	++cpuid;
	++statetoken;
	
	/* entry = */ Stack_pop( ((fcbT*)fcb)->processes[cpu][thread].stack );

	OTF_Writer_writeLeave( ((fcbT*)fcb)->writer, (uint64_t) time, statetoken,
		cpuid, scltoken );

	return 0;
}
 

int handleUpto( void *fcb, double time, int statetoken, 
		unsigned int cpuid, int scltoken ) {


	StackEntryT  entry;
	int cpu = cpuid&0xffff;
	int thread = (cpuid>>16)&0xffff;


	++cpuid;
	++statetoken;

	entry = Stack_pop( ((fcbT*)fcb)->processes[cpu][thread].stack );

	OTF_Writer_writeLeave( ((fcbT*)fcb)->writer, (uint64_t) time, entry.state,
		cpuid, scltoken );

	return 0;
}


int handleExchange( void *fcb, double time, unsigned int cpuid,
		int exchangetype, int statetoken, int job, int scltoken ) {


	if ( exchangetype == VTF3_EXCHANGETYPE_DOWNTO ) {

		return handleDownto( fcb, time, statetoken, cpuid, scltoken );

	} else if ( exchangetype == VTF3_EXCHANGETYPE_UPTO ) {

		return handleUpto( fcb, time, statetoken, cpuid, scltoken );

	} else if ( exchangetype == VTF3_EXCHANGETYPE_UPFROM ) {

		return handleUpfrom( fcb, time, statetoken, cpuid, scltoken );

	} else {

		fprintf( stderr, "ERROR: Unknown exchange type\n");
		exit(1);
	}

	return 0;
}
 

int handleExchange_obsol( void *fcb, double time, unsigned int cpuid,
		int exchangetype, int statetoken, int activitytoken,
		const char *activityname, unsigned int activityvalidity, int job ) {


	static int first= 1;

	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


int handleFileiobegin( void *fcbx, double time, unsigned int cpuid,
		int fileiotype, int iofiletoken, int bytescopied, int scltoken ) {


	int cpu = (cpuid&0xffff);
	int thread = (cpuid>>16)&0xffff;
	uint32_t otfiotype;
	fcbT* fcb= (fcbT*) fcbx;
	processT* proc= &((fcbT*)fcb)->processes[cpu][thread];
	uint64_t duration;

	if( 0 == fcb->ioonly ) {

		if( VTF3_FILEIOTYPE_READ == fileiotype ) {
			otfiotype= OTF_FILEOP_READ;
		} else if ( VTF3_FILEIOTYPE_WRITE == fileiotype ) {
			otfiotype= OTF_FILEOP_WRITE;
		} else {
			fprintf( stderr, "ERROR: fileiobegin: unknown fileiotype %i.skipping.\n", fileiotype );
			return 0;
		}


		/* re-read the extra information if there is nothing left */
		if( proc->fileIObufferpos >= proc->fileIObufferlen ) {

			readFileIOBuffer( cpuid, proc, fcb->outputFile );
		}


		++cpuid;
		++iofiletoken;

		if( proc->fileIObufferpos >= proc->fileIObufferlen ) {
			duration= 0;
		} else if ( time == proc->fileIObuffer[proc->fileIObufferpos] ) {
			duration= proc->fileIObuffer[proc->fileIObufferpos+1];
		} else {
			duration= 0;
		}
		
		OTF_Writer_writeFileOperation( fcb->writer, time, iofiletoken, cpuid,
			fcb->handleid, otfiotype, (uint64_t) bytescopied, duration,
			(uint32_t) scltoken );

		++fcb->handleid;
		++proc->fileIObufferpos;

	
	} else {

		/* 1 == fcb->ioonly */

		if( proc->fileIObegin == (uint64_t) -1 ) {
		
			proc->fileIObegin= time; /* set starttimestamp */
			
		} else {
		
			fprintf( stderr, "ERROR: fileiobegin after fileiobegin (without end in between). Overwriting.\n" );
			return 0;
			
		}
		
	}

	return 0;
}


int handleFileioend( void *fcbx, double time, unsigned int cpuid,
		int fileiotype, int iofiletoken, int bytescopied, int scltoken ) {


	int cpu = (cpuid&0xffff);
	int thread = (cpuid>>16)&0xffff;
	fcbT* fcb= (fcbT*) fcbx;
	processT* proc= &((fcbT*)fcb)->processes[cpu][thread];

	if( 0 == fcb->ioonly ) {

		/* ignore, because the starttime(begin record) and duration(extra-info)
		are important */
	
	} else {

		/* 1 == fcb->ioonly */

		if( proc->fileIObegin != (uint64_t) -1 ) {

			if( proc->fileIObufferpos > 2045 ) writeFileIOBuffer( cpuid, proc, fcb->outputFile );

			proc->fileIObuffer[proc->fileIObufferpos]= proc->fileIObegin;
			proc->fileIObuffer[proc->fileIObufferpos+1]= time - proc->fileIObegin;

			proc->fileIObufferpos+= 2;
			
		} else {
		
			fprintf( stderr, "ERROR: fileioend with no fileiobegin. Ignoring.\n" );
			return 0;
			
		}
	}
	
	return 0;
}


int handleGlobalop( void *fcb, double time, int globaloptoken,
		unsigned int cpuid, int communicator, unsigned int rootcpuid, 
		int bytessent, int bytesreceived, double durationtimesteps, 
		int scltoken ) {


	uint32_t tmp;
	
	++cpuid;
	

	/* map it */
	tmp= searchHash( ((fcbT*)fcb)->pghash, (uint32_t) communicator );
	if( 0 != tmp ) {
		communicator= tmp;
	}
	
	
	if ( rootcpuid >= (unsigned int)((fcbT*)fcb)->processcount ) {

		/* invalid root cpu id => translate to '0' */
		rootcpuid= 0;

	} else {

		++rootcpuid;
	}

	OTF_Writer_writeCollectiveOperation( ((fcbT*)fcb)->writer, (uint64_t) time,
		cpuid, globaloptoken + 1, communicator, rootcpuid, bytessent, bytesreceived,
		(uint64_t) durationtimesteps, scltoken );

	return 0;
}


int handleKparregbarsum( void *fcb, double time, unsigned int cpuid,
		int kparregtoken, int seqn, int opasize, 
		const void *opastream, int scltoken ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


int handleKparregbegin( void *fcb, double time, unsigned int cpuid,
		int kparregtoken, int seqn, int numthreads, int scltoken ) {	


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


int handleKparregend( void *fcb, double time, unsigned int cpuid,
		int kparregtoken, int seqn, int opasize,
		const void *opastream, int scltoken ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


int handleMutexacquire( void *fcb, double time, unsigned int cpuid,
		int enterstatetoken, int leavestatetoken,
		int leavestatetokenisupfrom, double durationtimesteps,
		int mutexsize, const void *mutex, int scltoken ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


int handleMutexrelease( void *fcb, double time, unsigned int cpuid,
		int enterstatetoken, int leavestatetoken,
		int leavestatetokenisupfrom, double durationtimesteps,
		int mutexsize, const void *mutex, int scltoken ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


int handleOpenmpenter( void *fcb, double time, unsigned int cpuid,
		unsigned int constructtypetoken, unsigned int nametoken, 
		int scltoken ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


int handleOpenmpleave( void *fcb, double time, 
		unsigned int cpuid, int scltoken ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}

int handlePattern( void *fcb, double time, unsigned int cpuid,
		int patorpatshptoken, double durationtimesteps,
		int timesteparraydim, const double *timesteparray,
		const int *patchindexarray ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}


int handleRecvmsg( void *fcb, double time, unsigned int receiver,
		unsigned int sender, int communicator, int msgtype,
		int msglength, int scltoken ) {


	uint32_t tmp;
	
	if ( receiver == (uint32_t) -1 || sender == (uint32_t) -1 )
		fprintf( stderr, "cpuid is not supposed to be -1" );

	/* map it */
	tmp= searchHash( ((fcbT*)fcb)->pghash, (uint32_t) communicator );
	if( 0 != tmp ) {
		communicator= tmp;
	}
	
	
	++receiver;
	++sender;

	OTF_Writer_writeRecvMsg( ((fcbT*)fcb)->writer, (uint64_t) time, receiver,
		sender, communicator, msgtype, msglength, scltoken );

	return 0;
}


int handleSamp( void *fcb, double time, unsigned int cpuorcpugrpid,
		int samplearraydim, const int *sampletokenarray,
		const int *samplevaluetypearray, const void *samplevaluearray ) {


	int i;
	uint64_t tmp;
	uint32_t tmp2;

	/* group means first bit equals 1 */
	if ( (cpuorcpugrpid&0x80000000) == 0) {

		if ( cpuorcpugrpid == (uint32_t) -1 ) {

			fprintf( stderr, "cpuid is not supposed to be -1" );
		}

		++cpuorcpugrpid;
	} else {
	
		/* map it */
		tmp2= searchHash( ((fcbT*)fcb)->pghash, (uint32_t) cpuorcpugrpid );
		if( 0 != tmp2 ) {
			cpuorcpugrpid= tmp2;
		}
	
	}

	for( i = 0; i < samplearraydim; ++i ) {

		if ( samplevaluetypearray[i] == VTF3_VALUETYPE_FLOAT ) {
		
			tmp = (uint64_t) ( ((double*) samplevaluearray)[i] + 0.5f );
		} else {
		
			tmp = ( (uint64_t*) samplevaluearray )[i];
		}
		
		OTF_Writer_writeCounter( ((fcbT*)fcb)->writer, (uint64_t) time,
			cpuorcpugrpid, sampletokenarray[i] + 1, tmp );
	}

	return 0;
}


int handleSendmsg( void *fcb, double time, unsigned int sender,
		unsigned int receiver, int communicator, int msgtype,
		int msglength, int scltoken ) {


	uint32_t tmp;
	
	if ( receiver == (uint32_t) -1 || sender == (uint32_t) -1 )
		fprintf( stderr, "cpuid is not supposed to be -1" );

	/* map it */
	tmp= searchHash( ((fcbT*)fcb)->pghash, (uint32_t) communicator );
	if( 0 != tmp ) {
		communicator= tmp;
	}
	
	
	++receiver;
	++sender;

	OTF_Writer_writeSendMsg( ((fcbT*)fcb)->writer, (uint64_t) time, sender,
		receiver, communicator, msgtype, msglength, scltoken );

	return 0;
}


int handleSrcinfo_obsol( void *fcb, double time, int activitytoken,
		int statetoken, int scllineposition ) {


	static int first= 1;

	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}
 

int handleUnrecognizable( void *fcb, double lastvalidtime,
		int numberofunrecognizablechars, int typeofunrecognizablerecord,
		const char *unrecognizablerecord ) {


	static int first= 1;


	if ( 1 == first ) {

		first= 0;

		fprintf( stderr, "%s: Record not implemented in OTF_\n", 
			__PRETTY_FUNCTION__ );
	}

	return 0;
}

#endif /* HAVE_VTF3 */
