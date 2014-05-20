/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "OTF_Platform.h"
#include "otf.h"

#ifdef HAVE_VTF3
#include "vtf3.h"
#endif /* HAVE_VTF3 */

#include "Handler.h"
#include "Treehash.h"


#define OTF2VTF3VERSION 1
#define OTF2VTF3CREATOR "otf2vtf3"

#define SHOW_HELPTEXT { \
	int l = 0; while( Helptext[l] ) { printf( "%s", Helptext[l++] ); } }

static const char* Helptext[] = {
"                                                                           \n",
" otf2vtf - Convert OTF trace files to VTF3 format.                          \n",
"                                                                           \n",
" Syntax: otf2vtf [Options] <input file name>                               \n",
"                                                                           \n",
"   options:                                                                \n",
"      -h, --help    show this help message                                 \n",
"      -V            show OTF version                                       \n",
"      -o <file>     output file                                            \n",
"      -b <n>        size of the reader buffer                              \n",
"      -A            write VTF3 ASCII sub-format (default)                  \n",
"      -B            write VTF3 binary sub-format                           \n",
"                                                                           \n",
NULL };

void writenames_recursive ( void* firsthandlerarg, nodeT *p_node );

int main (int argc, char **argv) {


#ifdef HAVE_VTF3


	fcbT fha;
	int i;
	int numcpus;

	char* inputFile = NULL;
	char* outputFile = NULL;
	int buffersize= 1024;
	uint64_t ret_read;

	OTF_FileManager* manager;
	OTF_Reader* reader;
	OTF_HandlerArray* handlers;
	nodeT* p_root;
	
	int format= VTF3_FILEFORMAT_STD_ASCII;

	handlers = OTF_HandlerArray_open();

	/* argument handling */

	if ( 1 >= argc ) {

		SHOW_HELPTEXT;
		exit(0);
	}

	for ( i = 1; i < argc; i++ ) {
	
		if( 0 == strcmp( "-i", argv[i] ) ) {

			if( i+1 < argc ) {
			
				inputFile= strdup( argv[i+1] );
				++i;
			}

		} else if ( 0 == strcmp( "-o", argv[i] ) ) {

			if( i+1 < argc ) {
			
				outputFile = argv[i+1];
				++i;
			}

		} else if( ( 0 == strcmp( "-b", argv[i] ) ) && ( i+1 < argc ) ) {
		
			buffersize = atoi( argv[i+1] );
			++i;

		} else if ( 0 == strcmp( "--help", argv[i] ) ||
				0 == strcmp( "-h", argv[i] ) ) {
				
			SHOW_HELPTEXT;
			exit(0);

		} else if ( 0 == strcmp( "-A", argv[i] ) ) {
		
			format= VTF3_FILEFORMAT_STD_ASCII;

		} else if ( 0 == strcmp( "-B", argv[i] ) ) {
		
			format= VTF3_FILEFORMAT_STD_BINARY;

		} else if ( 0 == strcmp( "-V", argv[i] ) ) {
		
			printf( "%u.%u.%u \"%s\"\n", OTF_VERSION_MAJOR, OTF_VERSION_MINOR,
				OTF_VERSION_SUB, OTF_VERSION_STRING);
			exit( 0 );

		} else {

			if ( '-' != argv[i][0] ) {

				inputFile= strdup( argv[i] );

			} else {

				fprintf( stderr, "ERROR: Unknown argument.\n" );
				exit(1);
			}
		}
	}
	
	if ( NULL == inputFile ) {
	
		printf( " no input file specified\n" );
		exit(1);
	}

	if ( NULL == outputFile ) {
	
		printf( " no output file specified\n" );
		exit(1);
	}


	/* open filemanager */
	manager= OTF_FileManager_open( 100 );
	assert( NULL != manager );

	/* Open OTF Reader */
	reader = OTF_Reader_open( inputFile, manager );

	if( NULL == reader ) {
	
		fprintf( stderr, "cannot open input trace '%s'\n", inputFile );
		exit( 1 );
	}


	OTF_Reader_setBufferSizes( reader, buffersize );
	
	free( inputFile );

	/* Initialize VTF3. */
	(void) VTF3_InitTables ();

	fha.fcb = VTF3_OpenFileOutput ( outputFile, format, 0);
	assert( 0 != fha.fcb );
	
	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleDefinitionComment,
		OTF_DEFINITIONCOMMENT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_DEFINITIONCOMMENT_RECORD );

	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleDeftimerresolution,
		OTF_DEFTIMERRESOLUTION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_DEFTIMERRESOLUTION_RECORD );

	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleDefprocess,
		OTF_DEFPROCESS_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_DEFPROCESS_RECORD );

	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleDefprocessgroup,
		OTF_DEFPROCESSGROUP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_DEFPROCESSGROUP_RECORD );

	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleDeffunction,
		OTF_DEFFUNCTION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_DEFFUNCTION_RECORD );

	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleDeffunctiongroup,
		OTF_DEFFUNCTIONGROUP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_DEFFUNCTIONGROUP_RECORD );

	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleDefcounter,
		OTF_DEFCOUNTER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_DEFCOUNTER_RECORD );

	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleDefcountergroup,
		OTF_DEFCOUNTERGROUP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_DEFCOUNTERGROUP_RECORD );

	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleDefCollectiveOperation,
		OTF_DEFCOLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_DEFCOLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleDefscl,
		OTF_DEFSCL_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_DEFSCL_RECORD );

	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleDefsclfile,
		OTF_DEFSCLFILE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_DEFSCLFILE_RECORD );

	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleDefFile,
		OTF_DEFFILE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_DEFFILE_RECORD );


	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleEventComment,
		OTF_EVENTCOMMENT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_EVENTCOMMENT_RECORD );

	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleCounter,
		OTF_COUNTER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_COUNTER_RECORD );

	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleEnter,
		OTF_ENTER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_ENTER_RECORD );

	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*)
		handleCollectiveOperation, OTF_COLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha,
		OTF_COLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleRecvmsg,
		OTF_RECEIVE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_RECEIVE_RECORD );

	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleSendmsg,
		OTF_SEND_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_SEND_RECORD );

	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleLeave,
		OTF_LEAVE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_LEAVE_RECORD );
	
	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleBeginProcess,
		OTF_BEGINPROCESS_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, 
		OTF_BEGINPROCESS_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleEndProcess,
		OTF_ENDPROCESS_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha,
		OTF_ENDPROCESS_RECORD );
		
	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleFileOperation,
		OTF_FILEOPERATION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha,
		OTF_FILEOPERATION_RECORD );
		
		
	/***************/
	treehash_init( &p_root, &fha.p_hashtab);
	fha.FileIOQueue= FileIOEndQueue_init();
	/***************/
	
	
	VTF3_WriteDefversion( fha.fcb, OTF2VTF3VERSION);
	VTF3_WriteDefcreator( fha.fcb, OTF2VTF3CREATOR );
	
	ret_read = OTF_Reader_readDefinitions( reader, handlers );
	if( ret_read == OTF_READ_ERROR ) {
		fprintf(stderr,"An error occurred while reading the tracefile. It seems to be damaged. Abort.\n");
		return 1;
	}

	/***************/
	numcpus = 0;
	/* indicate the processes */
	
	for( i = 0; i < p_root->childrensize; i++ ) {
	
		numcpus = treehash_createindices( numcpus, p_root->p_children[i] );
	}
	
	if ( numcpus == 0)
	{
		fprintf( stderr, "There are no cpus.\n" );
		exit(1);
	}
		
	/* write VTF3_syscpunum-record */
	VTF3_WriteDefsyscpunums( fha.fcb, 1, &numcpus );
	
	/* write VFT3_cpuname-records */
	for( i = 0; i < p_root->childrensize; i++ ) {

		writenames_recursive( fha.fcb, p_root->p_children[i] );
	}
	/***************/

	ret_read = OTF_Reader_readEvents( reader, handlers );
	if( ret_read == OTF_READ_ERROR ) {
		fprintf(stderr,"An error occurred while reading the tracefile. It seems to be damaged. Abort.\n");
		return 1;
	}

	/***************/
	FileIOEndQueue_finish( &fha.FileIOQueue );
	treehash_deleteall( fha.p_hashtab );
	/***************/
	
	/* Close all devices. */
	(void) VTF3_Close ( fha.fcb );

	OTF_Reader_close( reader );
	OTF_HandlerArray_close( handlers );
	OTF_FileManager_close( manager );

#else /* HAVE_VTF3 */

	fprintf( stderr, "VTF3 not available, otf2vtf deactivated\n" );

#endif /* HAVE_VTF3 */

	return (0);
}

void writenames_recursive ( void* firsthandlerarg, nodeT *p_node ) {


#ifdef HAVE_VTF3
	int i;
	
	if( p_node->name )
		VTF3_WriteDefcpuname( firsthandlerarg, p_node->processi,
			p_node->name );
	
	for( i = 0; i < p_node->childrensize; i++ )
		writenames_recursive ( firsthandlerarg, p_node->p_children[i] );
		
#endif /* HAVE_VTF3 */
}

