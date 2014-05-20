/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/


#include "Handler.h"

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#include <vector>

using namespace std;

#if SIZEOF_LONG == 4
#  define ATOL8 atoll
#else
#  define ATOL8 atol
#endif

#define SHOW_HELPTEXT { \
	int l = 0; while( Helptext[l] ) { printf( "%s", Helptext[l++] ); } }

static const char* Helptext[] = {
"                                                                           \n",
" otfprint - Convert an OTF trace or parts of it into a human readable, long\n",
"            version.                                                       \n",
"                                                                           \n",
" Syntax: otfprint [options] <input file name>                              \n",
"                                                                           \n",
"   options:                                                                \n",
"      -h, --help    show this help message                                 \n",
"      -V            show OTF version                                       \n",
"      -f <n>        set max number of filehandles available                \n",
"                    (default: 50)                                          \n",
"      -o <file>     output file                                            \n",
"                    if the ouput file is unspecified the stdout will be    \n",
"                    used                                                   \n",
"                                                                           \n",
"      --num <a> <b> output only records no. [a,b]                          \n",
"      --time <a> <b>                                                       \n",
"                    output only records with time stamp in [a,b]           \n",
"                                                                           \n",
"      --nodef       omit definition records                                \n",
"      --noevent     omit event records                                     \n",
"      --nostat      omit statistic records                                 \n",
"      --nosnap      omit snapshot records                                  \n",
"      --nomarker    omit marker records                                    \n",
"                                                                           \n",
"      --nokeyvalue  omit key-value pairs                                   \n",
"      --fullkeyvalue                                                       \n",
"                    show key-value pairs including the contents            \n",
"                    of byte-arrays                                         \n",
"                                                                           \n",
"      --procs <a>   show only processes <a>                                \n",
"                    <a> is a space-seperated list of process-tokens        \n",
"      --records <a> show only records <a>                                  \n",
"                    <a> is a space-seperated list of record-type-numbers   \n",
"                    record-type-numbers can be found in OTF_Definitions.h  \n",
"                    (OTF_*_RECORD)                                         \n",
"                                                                           \n",
"      -s, --silent  do not display anything except the time otfprint       \n",
"                    needed to read the tracefile                           \n",
"                                                                           \n",
NULL };

int main ( int argc, const char** argv ) {


	const char* infile= NULL;
	int buffersize= 4*1024;

	OTF_FileManager* manager;
	int nfiles = 50;
	OTF_Reader* reader;

	OTF_HandlerArray* handlers;
	
	vector<uint32_t> enabledRecords;

	bool def= true;
	bool event= true;
	bool stat= true;
	bool snap= true;
	bool marker= true;

	uint64_t mintime= 0;
	uint64_t maxtime= (uint64_t) -1;
	uint64_t read;
    
    uint64_t start_t, end_t;
	
	Control fha;
	fha.num= 0;
	fha.minNum= 0;
	fha.maxNum= (uint64_t) -1;
	fha.outfile= stdout;
	fha.show_keyvalue = KV_BASIC_MODE;
    fha.silent_mode = false;
	for( uint32_t i= 0; i < OTF_NRECORDS; ++i ) {

		fha.records[i]= true;
	}
	

	/* argument handling */

	if ( 1 >= argc ) {

		SHOW_HELPTEXT;
		exit(0);
	}

	for ( int i = 1; i < argc; i++ ) {

		if ( ( 0 == strcmp( "-b", argv[i] ) ) && ( i+1 < argc ) ) {
		
			buffersize = atoi( argv[i+1] );
			++i;

		} else if ( ( 0 == strcmp( "-o", argv[i] ) ) && ( i+1 < argc ) ) {

			i++;
			if ( '-' == argv[i][0] && '\0' == argv[i][1] ) {

					/* someone wants to use stdout? then keep the default */
					continue;
			}

			fha.outfile= fopen( argv[i], "w" );
			if ( NULL == fha.outfile ) {

				fprintf( stderr, "ERROR: cannot open output file '%s'\n", argv[i] );
				exit(1);
			}

		} else if ( 0 == strcmp( "--help", argv[i] ) ||	0 == strcmp( "-h", argv[i] ) ) {

			SHOW_HELPTEXT;
			exit(0);

		} else if ( 0 == strcmp( "-V", argv[i] ) ) {

			printf( "%u.%u.%u \"%s\"\n", OTF_VERSION_MAJOR, OTF_VERSION_MINOR,
				OTF_VERSION_SUB, OTF_VERSION_STRING);
			exit(0);

		} else if ( ( 0 == strcmp( "-f", argv[i] ) ) && ( i+1 < argc ) ) {

			nfiles = atoi( argv[i+1] );
			if ( nfiles < 1 ) {

				fprintf( stderr, "ERROR: less than 1 filehandle is not permitted\n" );
				exit(1);
			}
			++i;

		} else if ( ( 0 == strcmp( "--num", argv[i] ) ) && ( i+2 < argc ) ) {

			fha.minNum= ATOL8( argv[i+1] );
			fha.maxNum= ATOL8( argv[i+2] );

			i += 2;

		} else if ( ( 0 == strcmp( "--time", argv[i] ) ) && ( i+2 < argc ) ) {

			mintime= ATOL8( argv[i+1] );
			maxtime= ATOL8( argv[i+2] );

			i += 2;

		}else if ( 0 == strcmp( "--nodef", argv[i] ) ) {

			def= false;

		} else if ( 0 == strcmp( "--noevent", argv[i] ) ) {

			event= false;

		} else if ( 0 == strcmp( "--nostat", argv[i] ) ) {

			stat= false;

		} else if ( 0 == strcmp( "--nosnap", argv[i] ) ) {

			snap= false;

		} else if ( 0 == strcmp( "--nomarker", argv[i] ) ) {

			marker= false;
			
		} else if ( 0 == strcmp( "--nokeyvalue", argv[i] ) ) {

			fha.show_keyvalue= KV_QUIET_MODE;

		} else if ( 0 == strcmp( "--fullkeyvalue", argv[i] ) ) {

			fha.show_keyvalue= KV_FULL_MODE;

		} else if ( (0 == strcmp( "--procs", argv[i] )) && ( i+1 < argc ) ) {

			while( i+1 < argc && argv[i+1][0] >= '0' && argv[i+1][0] <= '9' ) {

				enabledRecords.push_back( atol(argv[i+1] ) );

				++i;
			}
			
		} else if ( (0 == strcmp( "--records", argv[i] )) && ( i+1 < argc ) ) {

			bool error= false;
			long a;

			for( a= 0; a < OTF_NRECORDS; ++a ) fha.records[a]= false;

			while( i+1 < argc && argv[i+1][0] >= '0' && argv[i+1][0] <= '9' ) {

				a= atol(argv[i+1]);
				if ( a >= 0 && a < OTF_NRECORDS ) {
					fha.records[a]= true;
					i++;
				} else {
					fprintf( stderr, "ERROR: Unknown record-type-number '%li'.\n", a );
					error= true;
					break;
				}
			}

			if ( error ) exit(1);

		} else if ( 0 == strcmp( "--silent", argv[i] ) || 0 == strcmp( "-s", argv[i] ) ) { 
          
            fha.silent_mode = true;
          
        }  else {

			if ( '-' != argv[i][0] ) {
			
				infile= argv[i];

			} else {

				fprintf( stderr, "ERROR: Unknown argument '%s'.\n", argv[i] );
				exit(1);
			}
		}
	}


	if ( NULL == infile ) {
	
		printf( " no input file specified, abort\n" );
		exit(1);
	}
	

	/* open filemanager */
	manager= OTF_FileManager_open( nfiles );
	assert( manager );

	/* Open OTF Reader */
	reader= OTF_Reader_open( infile, manager );
	assert( reader );

	OTF_Reader_setBufferSizes( reader, buffersize );

	/* enable only the records needed */
	if( false == enabledRecords.empty() ) {
	
		OTF_Reader_setProcessStatusAll( reader, 0 );

		for( uint32_t i= 0; i < enabledRecords.size(); ++i ) {

			OTF_Reader_setProcessStatus( reader, enabledRecords[i], 1 );
		}
	}

	/* set timelimit */
	OTF_Reader_setTimeInterval( reader, mintime, maxtime );
	

	/* init handlers */
	handlers= OTF_HandlerArray_open();

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefinitionComment,
		OTF_DEFINITIONCOMMENT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_DEFINITIONCOMMENT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefTimerResolution,
		OTF_DEFTIMERRESOLUTION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_DEFTIMERRESOLUTION_RECORD);

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefProcess,
		OTF_DEFPROCESS_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_DEFPROCESS_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefProcessGroup,
		OTF_DEFPROCESSGROUP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_DEFPROCESSGROUP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefAttributeList,
		OTF_DEFATTRLIST_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_DEFATTRLIST_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefProcessOrGroupAttributes,
		OTF_DEFPROCESSORGROUPATTR_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_DEFPROCESSORGROUPATTR_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefFunction,
		OTF_DEFFUNCTION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_DEFFUNCTION_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefFunctionGroup,
		OTF_DEFFUNCTIONGROUP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_DEFFUNCTIONGROUP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefCounter,
		OTF_DEFCOUNTER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_DEFCOUNTER_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefCounterGroup,
		OTF_DEFCOUNTERGROUP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_DEFCOUNTERGROUP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefScl,
		OTF_DEFSCL_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_DEFSCL_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefSclFile,
		OTF_DEFSCLFILE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_DEFSCLFILE_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleDefCreator,
		OTF_DEFCREATOR_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_DEFCREATOR_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleDefUniqueId,
		OTF_DEFUNIQUEID_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_DEFUNIQUEID_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleDefVersion,
		OTF_DEFVERSION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_DEFVERSION_RECORD );

        OTF_HandlerArray_setHandler( handlers,
                (OTF_FunctionPointer*) handleDefFile,
                OTF_DEFFILE_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers,
        &fha, OTF_DEFFILE_RECORD );

        OTF_HandlerArray_setHandler( handlers,
                (OTF_FunctionPointer*) handleDefFileGroup,
                OTF_DEFFILEGROUP_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers,
        &fha, OTF_DEFFILEGROUP_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleDefCollectiveOperation,
		OTF_DEFCOLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_DEFCOLLOP_RECORD );

        OTF_HandlerArray_setHandler( handlers,
                (OTF_FunctionPointer*) handleDefTimeRange,
                OTF_DEFTIMERANGE_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers,
                &fha,
                OTF_DEFTIMERANGE_RECORD );

        OTF_HandlerArray_setHandler( handlers,
                (OTF_FunctionPointer*) handleDefCounterAssignments,
                OTF_DEFCOUNTERASSIGNMENTS_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers,
                &fha,
                OTF_DEFCOUNTERASSIGNMENTS_RECORD );

        OTF_HandlerArray_setHandler( handlers,
                (OTF_FunctionPointer*) handleDefProcessSubstitutes,
                OTF_DEFPROCESSSUBSTITUTES_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers,
                &fha,
                OTF_DEFPROCESSSUBSTITUTES_RECORD );

        OTF_HandlerArray_setHandler( handlers,
                (OTF_FunctionPointer*) handleDefAuxSamplePoint,
                OTF_DEFAUXSAMPLEPOINT_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers, &fha,
                OTF_DEFAUXSAMPLEPOINT_RECORD );


	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleNoOp,
		OTF_NOOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_NOOP_RECORD );	
	
	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleEventComment,
		OTF_EVENTCOMMENT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_EVENTCOMMENT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleCounter,
		OTF_COUNTER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_COUNTER_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleEnter,
		OTF_ENTER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_ENTER_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleCollectiveOperation,
		OTF_COLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_COLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleBeginCollectiveOperation,
		OTF_BEGINCOLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_BEGINCOLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleEndCollectiveOperation,
		OTF_ENDCOLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_ENDCOLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleRecvMsg,
		OTF_RECEIVE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_RECEIVE_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleSendMsg,
		OTF_SEND_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_SEND_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleLeave,
		OTF_LEAVE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_LEAVE_RECORD );

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

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleBeginFileOperation,
		OTF_BEGINFILEOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha,
		OTF_BEGINFILEOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleEndFileOperation,
		OTF_ENDFILEOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha,
		OTF_ENDFILEOP_RECORD );

        OTF_HandlerArray_setHandler( handlers, 
                (OTF_FunctionPointer*) handleRMAPut,
                OTF_RMAPUT_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers, &fha,
                OTF_RMAPUT_RECORD );

        OTF_HandlerArray_setHandler( handlers, 
                (OTF_FunctionPointer*) handleRMAPutRemoteEnd,
                OTF_RMAPUTRE_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers, &fha,
                OTF_RMAPUTRE_RECORD );

        OTF_HandlerArray_setHandler( handlers, 
                (OTF_FunctionPointer*) handleRMAGet,
                OTF_RMAGET_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers, &fha,
                OTF_RMAGET_RECORD );

        OTF_HandlerArray_setHandler( handlers, 
                (OTF_FunctionPointer*) handleRMAEnd,
                OTF_RMAEND_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers, &fha,
                OTF_RMAEND_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleDefKeyValue,
		OTF_DEFKEYVALUE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha,
		OTF_DEFKEYVALUE_RECORD );


	/* snapshot records */

    OTF_HandlerArray_setHandler( handlers, 
        (OTF_FunctionPointer*) handleSnapshotComment,
        OTF_SNAPSHOTCOMMENT_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &fha,
        OTF_SNAPSHOTCOMMENT_RECORD );
    
    
	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleEnterSnapshot,
		OTF_ENTERSNAPSHOT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha,
		OTF_ENTERSNAPSHOT_RECORD );
	
	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleSendSnapshot,
		OTF_SENDSNAPSHOT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha,
		OTF_SENDSNAPSHOT_RECORD );
        
    OTF_HandlerArray_setHandler( handlers, 
        (OTF_FunctionPointer*) handleOpenFileSnapshot,
        OTF_OPENFILESNAPSHOT_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &fha,
        OTF_OPENFILESNAPSHOT_RECORD ); 
        
    OTF_HandlerArray_setHandler( handlers, 
        (OTF_FunctionPointer*) handleBeginCollopSnapshot,
        OTF_BEGINCOLLOPSNAPSHOT_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &fha,
        OTF_BEGINCOLLOPSNAPSHOT_RECORD );  
        
    OTF_HandlerArray_setHandler( handlers, 
        (OTF_FunctionPointer*) handleBeginFileOpSnapshot,
        OTF_BEGINFILEOPSNAPSHOT_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &fha,
        OTF_BEGINFILEOPSNAPSHOT_RECORD );      

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleCollopCountSnapshot,
        OTF_COLLOPCOUNTSNAPSHOT_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &fha,
        OTF_COLLOPCOUNTSNAPSHOT_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleCounterSnapshot,
        OTF_COUNTERSNAPSHOT_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &fha,
        OTF_COUNTERSNAPSHOT_RECORD );

	
	/* summary records */

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleFunctionSummary,
		OTF_FUNCTIONSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_FUNCTIONSUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleFunctionGroupSummary,
		OTF_FUNCTIONGROUPSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_FUNCTIONGROUPSUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleMessageSummary,
		OTF_MESSAGESUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_MESSAGESUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleCollopSummary,
		OTF_COLLOPSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, 
        &fha, OTF_COLLOPSUMMARY_RECORD );


       /* I/O records */

        OTF_HandlerArray_setHandler( handlers,
                (OTF_FunctionPointer*) handleFileOperation,
                OTF_FILEOPERATION_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers,
        &fha, OTF_FILEOPERATION_RECORD );


	/* marker records */

	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleDefMarker, OTF_DEFMARKER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_DEFMARKER_RECORD );
	
	OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleMarker, OTF_MARKER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_MARKER_RECORD );	


	/* misc records */
	
	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleUnknown,
		OTF_UNKNOWN_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers,
        &fha, OTF_UNKNOWN_RECORD );

        
    start_t = time(NULL);
          
	if ( def && fha.num <= fha.maxNum ) {

        if( !fha.silent_mode ) {
		    fprintf( fha.outfile, "\ndefinitions:\n\n" );
        }
            
		read = OTF_Reader_readDefinitions( reader, handlers );
		if( read == OTF_READ_ERROR ) {
			fprintf(stderr,"An error occurred while reading definitions. The tracefile seems to be damaged. Abort.\n");
			return 1;
		}
	}

	if ( event && fha.num <= fha.maxNum ) {

        if( !fha.silent_mode ) {
		    fprintf( fha.outfile, "\nevents:\n\n" );
        }
        
		read = OTF_Reader_readEvents( reader, handlers );
		if( read == OTF_READ_ERROR ) {
			fprintf(stderr,"An error occurred while reading events. Maybe the tracefile has no events or it is damaged. Continue.\n");
		}
	}

	if ( stat && fha.num <= fha.maxNum ) {

        if( !fha.silent_mode ) {
		    fprintf( fha.outfile, "\nstatistics:\n\n" );
        }
        
		read = OTF_Reader_readStatistics( reader, handlers );
		if( read == OTF_READ_ERROR ) {
			fprintf(stderr,"An error occurred while reading statistics. The tracefile seems to be damaged. Abort.\n");
			return 1;
		}
	}

	if ( snap && fha.num <= fha.maxNum ) {

        if( !fha.silent_mode ) {
		    fprintf( fha.outfile, "\nsnapshots:\n\n" );
        }
		read = OTF_Reader_readSnapshots( reader, handlers );
		if( read == OTF_READ_ERROR ) {
			fprintf(stderr,"An error occurred while reading snapshots. The tracefile seems to be damaged. Abort.\n");
			return 1;
		}
	}

	if ( marker && fha.num <= fha.maxNum ) {

        if( !fha.silent_mode ) {
		    fprintf( fha.outfile, "\nmarkers:\n\n" );
        }
		read = OTF_Reader_readMarkers( reader, handlers );
		if( read == OTF_READ_ERROR ) {
			fprintf(stderr,"An error occurred while reading markers. The tracefile seems to be damaged. Abort.\n");
			return 1;
		}
	}
    
    end_t = time(NULL);

    if( !fha.silent_mode ) {
	    fprintf( fha.outfile, "\ndone\n" );
    }

	OTF_Reader_close( reader );
	OTF_HandlerArray_close( handlers );
	OTF_FileManager_close( manager );

        fprintf( fha.outfile, "processing time: %llu s\n", (unsigned long long int)(end_t - start_t) );

    if ( stdout != fha.outfile ) {

        fclose( fha.outfile );
        fha.outfile= NULL;
    }

	return 0;
}


