/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstring>
#include <cerrno>

#include <set>
#include <iostream>
#include <cassert>
#include <cmath>
using namespace std;

#ifdef HAVE_UNISTD_H
	#include <unistd.h>
#endif

#include "OTF_Platform.h"

#include "Handler.h"
#include "Control.h"

#define SHOW_HELPTEXT { \
	int l = 0; while( Helptext[l] ) { printf( "%s", Helptext[l++] ); } }

static const char* Helptext[] = {
"                                                                           \n",
" otfaux - Append snapshots and statistics to an existing OTF trace         \n",
"          at given 'break' time stamps.                                    \n",
"                                                                           \n",
" Syntax: otfaux [options] <input file name>                                \n",
"                                                                           \n",
"   options:                                                                \n",
"      -h, --help    show this help message                                 \n",
"      -V            show OTF version                                       \n",
"      -b <size>     buffer size for read and write operations              \n",
"      -n <n>        number of breaks (distributed regularly)               \n",
"                    if -p and -t are not set, the default for -n is 200    \n",
"                    breaks                                                 \n",
"      -p <p>        create break every 'p' ticks                           \n",
"                    (if both, -n and -p are specified the one producing    \n",
"                    more breaks wins)                                      \n",
"      -t <t>        define (additional) break at given time stamp          \n",
"      -F            force overwrite old snapshots and statistics           \n",
"      -R            delete existing snapshots and statistics only          \n",
"      -f <n>        max number of filehandles output                       \n",
"      --funcgroups  create functiongroup summaries instead of              \n",
"                    function summaries                                     \n",
"      --filegroups  create file group summaries instead of file            \n",
"                    summaries                                              \n",
"      -v            verbose mode, print break time stamps                  \n",
"      -a            show advancing progress during operation               \n",
"      -o <namestub> makes a copy of the input trace                        \n",
"      --thumbnail-procs <n>                                                \n",
"                    upper bound for the number of processes in the         \n",
"                    thumbnail                                              \n",
"      --thumbnail-samples <n>                                              \n",
"                    upper bound for the number of sampling points in the   \n",
"                    thumbnail                                              \n",
"      --inline-snapshots                                                   \n",
"                    produces inline snapshots                              \n",
"                    (requires the -o option)                               \n",
"      --match-messages                                                     \n",
"                    attach receive time information to send events         \n",
"                    (requires the -o option)                               \n",
"                                                                           \n",
"      --snapshots   write snapshots and a thumbnail but NO statistics      \n",
"                    (default mode)                                         \n",
"      --thumbnail   write ONLY a thumbnail                                 \n",
"      --statistics  write ONLY statistics but NO snapshots or a thumbnail  \n",
"      --all         write snapshots, a thumbnail, and statistics           \n",
"                                                                           \n",
"      -s a[,b]*     regard given streams only when computing statistics.   \n",
"                    expects a single token or comma separated list.        \n",
"                    this implies the '--statistics' option!                \n",
"      -l            list existing stream tokens                            \n",
"                                                                           \n",
NULL };

#define DEFAULT_SUMMARYNUMBER 200


void checkExistingFile( const char* tmpfilename, bool forceoverwrite, bool deleteonly );


static char *newdefnamestub;
static char *newdeffilename;
static char *out_namestub;

extern "C" void atexit_cleanup( void ) {

    if ( !out_namestub && newdefnamestub ) {

        unlink( newdefnamestub );
        unlink( newdeffilename );

    }
}

int main ( int argc, const char** argv ) {

	char* filename = NULL;
	int buffersize= 1024;

	/** minimum number of snapshots distributed regularly over the trace's 
	time interval, i.e. not at the very beginning or very end */
	uint64_t summary_number= 0;

	/** distance of successive snapshots in ticks */
	uint64_t summary_distance= (uint64_t) -1;

	OTF_FileManager* manager;
	OTF_Reader* reader= NULL;
	OTF_Writer* writer= NULL;
	OTF_MasterControl* mc= NULL;

	OTF_Writer* def_writer= NULL;

	OTF_HandlerArray* handlers;
	
	char *namestub;
	bool forceoverwrite= false;
	bool deleteonly= false;

	OTF_FileType type;
	OTF_FileCompression compression= OTF_FILECOMPRESSION_UNCOMPRESSED;
	unsigned int maxfilehandles = 100;

	bool verbose= false;
	bool usefunctiongroups= false;
	bool usefilegroups= false;
	bool showprogress= false;
	bool listonly= false;

    bool doSnapshots= true;
    bool doStatistics= false;
    bool doThumbnail= true;
    bool msgMatching= false;
    bool inlineSnapshots= false;

	uint64_t read;

	/* has something been set? 1= n, 2= p, 4= t */
	int npt= 0;
	
	/** set of selected streams, all streams if set is empty! */
	std::set<uint32_t> streams;

	/** list of explicit time stamps for snapshots */
	std::set<uint64_t> timestamps;

	uint64_t tmin= 0;
	uint64_t tcur= 0;
	uint64_t tmax= (uint64_t) -1;

	uint32_t thumbnail_procs = 512;
	uint32_t thumbnail_samples = 4096;

    double d;

	/* argument handling */

	if ( 1 >= argc ) {

		SHOW_HELPTEXT;
		exit(0);
	}

	for ( int i = 1; i < argc; i++ ) {

		if ( ( 0 == strcmp( "-i", argv[i] ) ) && ( i+1 < argc ) ) {
		
			filename= strdup( argv[i+1] );
			++i;

		} else if ( ( 0 == strcmp( "-b", argv[i] ) ) && ( i+1 < argc ) ) {
		
			buffersize = atoi( argv[i+1] );
			++i;

		} else if ( 0 == strcmp( "--help", argv[i] ) ||	0 == strcmp( "-h", argv[i] ) ) {
				
			SHOW_HELPTEXT;
			exit(0);

		} else if ( 0 == strcmp( "-V", argv[i] ) ) {
		
			printf( "%u.%u.%u \"%s\"\n", OTF_VERSION_MAJOR, OTF_VERSION_MINOR,
				OTF_VERSION_SUB, OTF_VERSION_STRING);
			exit( 0 );

		} else if ( ( 0 == strcmp( "-n", argv[i] ) ) && ( i+1 < argc ) ) {
		
			summary_number= atoi( argv[i+1] );
			npt= 1;
			++i;

		} else if ( ( 0 == strcmp( "-p", argv[i] ) ) && ( i+1 < argc ) ) {
		
			summary_distance= atoi( argv[i+1] );
			npt= 2;
			++i;

		} else if ( ( 0 == strcmp( "-t", argv[i] ) ) && ( i+1 < argc ) ) {
		
			uint64_t time = atoi( argv[i+1] );
			timestamps.insert( time );
			npt= 4;
			++i;

		} else if ( 0 == strcmp( "-F", argv[i] ) ) {
		
			forceoverwrite= true;

		} else if ( 0 == strcmp( "-R", argv[i] ) ) {
		
			deleteonly= true;

		} else if( 0 == strcmp( "-z", argv[i] ) ) {
		
			compression= OTF_FILECOMPRESSION_COMPRESSED;
			
		} else if ( ( 0 == strcmp( "-f", argv[i] ) ) && ( i+1 < argc ) ) {
		
			maxfilehandles= atoi( argv[i+1] );
			++i;

		} else if( 0 == strcmp( "-a", argv[i] ) ) {
		
			showprogress= true;

		} else if ( ( 0 == strcmp( "-o", argv[i] ) ) && ( i+1 < argc ) ) {

			out_namestub= OTF_stripFilename( argv[i+1] );
			++i;

		} else if( 0 == strcmp( "-v", argv[i] ) ) {
		
			verbose= true;
			
		}else if( 0 == strcmp( "--funcgroups", argv[i] ) ) {
		
			usefunctiongroups= true;
			
		} else if( 0 == strcmp( "--filegroups", argv[i] ) ) {
		
			usefilegroups= true;
			
		} else if( 0 == strcmp( "-l", argv[i] ) ) {
		
			listonly= true;

		} else if ( ( 0 == strcmp( "-s", argv[i] ) ) && ( i+1 < argc ) ) {

			/* operation for selected streams IMPLIES '--statistics' */
			doStatistics= true;
			doSnapshots= false;
			doThumbnail= false;


			const char* p= argv[i+1];
			++i;

			while ( '\0' != *p ) {

				uint32_t token= strtol( p, (char**) NULL, 16 );

				streams.insert( token );
				
				/* search comma or '\n' */
				while ( ( '\0' != *p ) && ( ',' != *p ) ) {

					++p;
				}

				/* skip comma */
				if ( ',' == *p ) {
				
					++p;
				}
			}

		} else if ( 0 == strcmp( "--thumbnail-procs", argv[i] )
			    || 0 == strncmp( "--thumbnail-procs=", argv[i], 18 )) {
		
			const char* arg;
			char* endptr;
			if ( !argv[i][17] ) {

				if ( i+1 == argc ) {

					printf( "ERROR: Missing argument for --thumbnail-procs.\n" );
					printf( "ERROR: Try '%s --help'.\n", argv[0] );
					exit(1);
				}

				arg = argv[++i];
			} else {

				arg = &argv[i][18];
			}

			thumbnail_procs= strtol( arg, &endptr, 0 );
			if ( endptr == arg || *endptr || !thumbnail_procs )
			{
				printf( "ERROR: Invalid argument for --thumbnail-procs: '%s'.\n", arg );
				printf( "ERROR: Try '%s --help'.\n", argv[0] );
				exit(1);
			}

		} else if ( 0 == strcmp( "--thumbnail-samples", argv[i] )
			    || 0 == strncmp( "--thumbnail-samples=", argv[i], 20 ) ) {
		
			const char* arg;
			char* endptr;
			if ( !argv[i][19] ) {

				if ( i+1 == argc ) {

					printf( "ERROR: Missing argument for --thumbnail-samples.\n" );
					printf( "ERROR: Try '%s --help'.\n", argv[0] );
					exit(1);
				}
				arg = argv[++i];
			} else {

				arg = &argv[i][20];
			}

			thumbnail_samples= strtol( arg, &endptr, 0 );
			if ( endptr == arg || *endptr || !thumbnail_samples )
			{
				printf( "ERROR: Invalid argument for --thumbnail-samples: '%s'.\n", arg );
				printf( "ERROR: Try '%s --help'.\n", argv[0] );
				exit(1);
			}

		} else if( 0 == strcmp( "--inline-snapshots", argv[i] ) ) {

			inlineSnapshots= true;

		} else if( 0 == strcmp( "--match-messages", argv[i] ) ) {

			msgMatching= true;

		} else if( 0 == strcmp( "--all", argv[i] ) ) {
		
			doStatistics= true;
			doSnapshots= true;
			doThumbnail= true;

		} else if( 0 == strcmp( "--snapshots", argv[i] ) ) {
		
			doStatistics= false;
			doSnapshots= true;
			doThumbnail= true;

		} else if( 0 == strcmp( "--thumbnail", argv[i] ) ) {
		
			doStatistics= false;
			doSnapshots= false;
			doThumbnail= true;

		} else if( 0 == strcmp( "--statistics", argv[i] ) ) {

			doStatistics= true;
			doSnapshots= false;
			doThumbnail= false;

		} else {

			if ( '-' != argv[i][0] ) {
			
				filename= strdup( argv[i] );

			} else {

				fprintf( stderr, "ERROR: Unknown argument.\n" );
				exit(1);
			}
		}
	}


	if ( NULL == filename ) {
	
		printf( " no input file specified, abort\n" );
		exit(1);
	}
	
	if ( ! ( doStatistics || doSnapshots || doThumbnail || msgMatching ) ) {

		printf( "ERROR: contradicting options '--snapshots', '--statistics', and '--thumbnail', abort\n" );
		exit(1);
	}


	if ( ( doSnapshots || doThumbnail ) && !streams.empty() ) {

		printf( "ERROR: contradicting options '--snapshots' and '-s', abort\n" );
		exit(1);
	}

	if ( inlineSnapshots && !doSnapshots ) {

		printf( "ERROR: option '--inline-snapshots' works only if producing snapshots\n" );
		exit(1);
	}

	if ( inlineSnapshots && !out_namestub ) {

		printf( "ERROR: option '--inline-snapshots' requires the -o option\n" );
		exit(1);
	}

	if ( out_namestub && ( deleteonly || forceoverwrite )  ) {

		printf( "ERROR: option '-o' does not work with the -R or -F option\n" );
		exit(1);
	}

	if ( msgMatching && !out_namestub ) {

		printf( "ERROR: option '--match-messages' requires the -o option\n" );
		exit(1);
	}

	/* n has been set, not p, not t */
	if ( 1 == npt && 2 > summary_number ) {
	
		printf( " you must at least have 2 breaks\n" );
		exit(1);
	}
	
	if ( 1 > maxfilehandles ) {
	
		printf( " there must be at least 1 available filehandle\n" );
		exit(1);
	}
	
	if ( 0 == npt ) {
	
		summary_number= 200;
	}
	
	/* open filemanager */
	manager= OTF_FileManager_open( maxfilehandles );
	assert( NULL != manager );

	/* Open OTF Reader */
	reader= OTF_Reader_open( filename, manager );
	if ( NULL == reader ) {

		fprintf( stderr, "%s ERROR: could not open '%s'\n", "otfaux", filename );
		return 1;
	}

	OTF_Reader_setBufferSizes( reader, buffersize );
	mc= OTF_Reader_getMasterControl( reader );


	if ( listonly ) {

		printf( "stream ID : process IDs ... \n" );

		uint32_t index= 0;
		const OTF_MapEntry* entry;
		while ( NULL != ( entry= OTF_MasterControl_getEntryByIndex( mc, index ) ) ) {

			printf( "%x :  ", entry->argument );
			for ( uint32_t i= 0; i < entry->n; ++i ) {

				printf( "%x ", entry->values[i] );
			}
			printf( "\n" );

			++index;
		}

		/* exit otfaux */
		return 0;
	}

	/* check if there are already existing statistics and snapshots */
	/* get streamcount and namestub */
	namestub= OTF_stripFilename( filename );
    if ( !out_namestub ) {
        int defnamefd;
        newdefnamestub= (char*)malloc( strlen( namestub ) + 1 + 6 + 1);
        sprintf( newdefnamestub, "%s.XXXXXX", namestub );
        defnamefd = mkstemp( newdefnamestub );
        if ( -1 == defnamefd ) {
            free( namestub );
            free( newdefnamestub );
            exit(1);
        }
        close( defnamefd );

        /* create new def filename early, so that we can remove it
           in our exit cleanup handler */
        type = OTF_FILETYPE_DEF | compression;
        newdeffilename = OTF_getFilename( newdefnamestub, 0, type, 0, NULL );
    }

    atexit( atexit_cleanup );

    if ( !out_namestub ) {
	for (set<uint32_t>::const_iterator jt= streams.begin(); jt != streams.end(); ++jt ) {

		char* tmpfilename;

		if ( doSnapshots ) {

			type= OTF_FILETYPE_SNAPS;
			tmpfilename= OTF_getFilename( namestub, *jt, type, 0, NULL );
			checkExistingFile( tmpfilename, forceoverwrite, deleteonly );
			free( tmpfilename );

			type|= OTF_FILECOMPRESSION_COMPRESSED;
			tmpfilename= OTF_getFilename( namestub, *jt, type, 0, NULL );
			checkExistingFile( tmpfilename, forceoverwrite, deleteonly );
			free( tmpfilename );
		}

		if ( doStatistics ) {
			type= OTF_FILETYPE_STATS;
			tmpfilename= OTF_getFilename( namestub, *jt, type, 0, NULL );
			checkExistingFile( tmpfilename, forceoverwrite, deleteonly );
			free( tmpfilename );

			type|= OTF_FILECOMPRESSION_COMPRESSED;
			tmpfilename= OTF_getFilename( namestub, *jt, type, 0, NULL );
			checkExistingFile( tmpfilename, forceoverwrite, deleteonly );
			free( tmpfilename );
		}

		if ( doThumbnail ) {
			tmpfilename= OTFAUX_Thumbnail_getFilename( namestub );
			checkExistingFile( tmpfilename, forceoverwrite, deleteonly );
			free( tmpfilename );
		}
	}
    }

	/* Open OTF Writer */
    if ( out_namestub ) {
	OTF_MasterControl* out_mc = OTF_MasterControl_clone( mc, manager );
	writer= OTF_Writer_open( out_namestub, 0, manager );
	OTF_Writer_setMasterControl( writer, out_mc );
    } else {

	writer= OTF_Writer_open( filename, 0, manager );
	OTF_Writer_setMasterControl( writer, mc );
    }
    assert( NULL != writer );
    OTF_Writer_setBufferSizes( writer, buffersize );
    OTF_Writer_setCompression( writer, compression );
    if ( inlineSnapshots ) {
        OTF_Writer_setFormat( writer, OTF_WSTREAM_FORMAT_INLINE_SNAPSHOTS );
    }
    free( filename );


	handlers= OTF_HandlerArray_open();

    /* make a copy of the definitions, and remove all DefAuxSamplePoint records */
    OTF_WStream* def_wstream;
    if ( !out_namestub ) {
        def_writer= OTF_Writer_open( newdefnamestub, 0, manager );
        assert( NULL != def_writer );
        OTF_Writer_setBufferSizes( def_writer, buffersize );
        OTF_Writer_setCompression( def_writer, compression );
        OTF_Writer_setMasterControl( def_writer, mc );
        /* operate only on the global def stream */
        def_wstream= OTF_Writer_getStream( def_writer, 0 );
    } else {
        def_wstream= OTF_Writer_getStream( writer, 0 );
    }

    /* increase buffer size for writing definitions (and markers), if necessary */
    if ( 10240 > buffersize ) {
        OTF_WStream_setBufferSizes( def_wstream, 10240 );
    }

    OTF_HandlerArray_getCopyHandler_stream( handlers, def_wstream );

    Control* control= new Control( writer, def_wstream, verbose,
            usefunctiongroups, usefilegroups,
            doSnapshots, doThumbnail, doStatistics,
            inlineSnapshots, msgMatching, !!out_namestub );

    /* overwrite copy handler array with our own handlers */
	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleDefFunction,
		OTF_DEFFUNCTION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_DEFFUNCTION_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleDeftimerresolution,
		OTF_DEFTIMERRESOLUTION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_DEFTIMERRESOLUTION_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleDefprocess,
		OTF_DEFPROCESS_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_DEFPROCESS_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleDefFile,
		OTF_DEFFILE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_DEFFILE_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleDefCollectiveOperation,
		OTF_DEFCOLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_DEFCOLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handlerDefKeyValue,
		OTF_DEFKEYVALUE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_DEFKEYVALUE_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleDefTimeRange,
		OTF_DEFTIMERANGE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_DEFTIMERANGE_RECORD );

    /* disable DefAuxSamplePoint handlers, so that they will not be copied */
    OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) NULL,
            OTF_DEFAUXSAMPLEPOINT_RECORD );

	/* cout << "read " << read << " defs" << endl; */
	read = OTF_Reader_readDefinitions( reader, handlers );
	if( read == OTF_READ_ERROR ) {
		fprintf(stderr,"An error occurred while reading the tracefile. It seems to be damaged. Abort.\n");
		return 1;
	}

    /* deleting files is done by now, cleanup and exit */
    if ( deleteonly ) {

        goto out;
    }

    /* copy markers to out trace */
    if ( out_namestub ) {
	read = OTF_Reader_readMarkers( reader, handlers );
	if( read == OTF_READ_ERROR ) {
		fprintf(stderr,"An error occurred while reading the tracefile. It seems to be damaged. Abort.\n");
		return 1;
	}

        if ( !doStatistics ) {
            read = OTF_Reader_readStatistics( reader, handlers );
            if( read == OTF_READ_ERROR ) {
                fprintf(stderr,"An error occurred while reading the tracefile. It seems to be damaged. Abort.\n");
                return 1;
            }
        }

        /* define message matching key-value tokens */
        if ( msgMatching ) {
            if ( control->recvTimeToken == 0 )
                control->recvTimeToken = ++control->maxKeyToken;
            OTF_WStream_writeDefKeyValue( def_wstream, control->recvTimeToken,
                                          OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_TIME_TYPE,
                                          OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_TIME_NAME,
                                          "" );
            if ( control->recvLengthToken == 0 )
                control->recvLengthToken = ++control->maxKeyToken;
            OTF_WStream_writeDefKeyValue( def_wstream, control->recvLengthToken,
                                          OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_SIZE_TYPE,
                                          OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_SIZE_NAME,
                                          "" );
            if ( control->recvSclToken == 0 )
                control->recvSclToken = ++control->maxKeyToken;
            OTF_WStream_writeDefKeyValue( def_wstream, control->recvSclToken,
                                          OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_SCL_TYPE,
                                          OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_SCL_NAME,
                                          "" );
        }

    }

    /* re-open handler array, to get clean handlers without copy handlers */
    OTF_HandlerArray_close( handlers );
    handlers= OTF_HandlerArray_open();

	if ( doSnapshots || msgMatching ) {
		/* we need to read all recvs  */
		OTF_HandlerArray_setHandler( handlers,
			(OTF_FunctionPointer*) handleRecvmsg,
			OTF_RECEIVE_RECORD );
		OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
			OTF_RECEIVE_RECORD );

	} else if ( !control->haveTimeRange ) {
		/* We just need the timerange of the trace */
		OTF_Reader_setRecordLimit( reader, 0 );
	}

	/** compute where to put snapshots */
	if ( doSnapshots || msgMatching || !control->haveTimeRange ) {
		read = OTF_Reader_readEvents( reader, handlers );
		if ( read == OTF_READ_ERROR ) {
			fprintf(stderr,"An error occurred while reading the tracefile. It seems to be damaged. Abort.\n");
			return 1;
		}
	}

        if ( control->haveTimeRange ) {
            tmin = control->minTime;
            tmax = control->maxTime;

        } else {
            OTF_Reader_eventTimeProgress( reader, &tmin, &tcur, &tmax );
            OTF_WStream_writeDefTimeRange( def_wstream, tmin, tmax, NULL );
        }


	OTF_Reader_reset( reader );

	/* message receive stats */
	control->collectRecvsOnly= false;

        /* copy all events to out trace */
        if ( out_namestub ) {
            OTF_HandlerArray_getCopyHandler( handlers, writer );
            if (doSnapshots) {
                OTF_HandlerArray_setHandler( handlers, 0, OTF_BEGINCOLLOPSNAPSHOT_RECORD );
                OTF_HandlerArray_setHandler( handlers, 0, OTF_BEGINFILEOPSNAPSHOT_RECORD );
                OTF_HandlerArray_setHandler( handlers, 0, OTF_COLLOPCOUNTSNAPSHOT_RECORD );
                OTF_HandlerArray_setHandler( handlers, 0, OTF_ENTERSNAPSHOT_RECORD );
                OTF_HandlerArray_setHandler( handlers, 0, OTF_OPENFILESNAPSHOT_RECORD );
                OTF_HandlerArray_setHandler( handlers, 0, OTF_SENDSNAPSHOT_RECORD );
            }
        }

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleCounter,
		OTF_COUNTER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_COUNTER_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleEnter,
		OTF_ENTER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_ENTER_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleSendmsg,
		OTF_SEND_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_SEND_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleRecvmsg,
		OTF_RECEIVE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_RECEIVE_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) handleLeave,
		OTF_LEAVE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control,
		OTF_LEAVE_RECORD );
	
    OTF_HandlerArray_setHandler( handlers, 
        (OTF_FunctionPointer*) handleCollectiveOperation,
        OTF_COLLOP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control, 
        OTF_COLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) handleFileOperation,
		OTF_FILEOPERATION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control, 
		OTF_FILEOPERATION_RECORD );
        
        
        
    OTF_HandlerArray_setHandler( handlers, 
        (OTF_FunctionPointer*) handleBeginCollectiveOperation,
        OTF_BEGINCOLLOP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control, 
        OTF_BEGINCOLLOP_RECORD );
        
    OTF_HandlerArray_setHandler( handlers, 
        (OTF_FunctionPointer*) handleEndCollectiveOperation,
        OTF_ENDCOLLOP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control, 
        OTF_ENDCOLLOP_RECORD );
        
    OTF_HandlerArray_setHandler( handlers, 
        (OTF_FunctionPointer*) handleBeginFileOperation,
        OTF_BEGINFILEOP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control, 
        OTF_BEGINFILEOP_RECORD );
        
    OTF_HandlerArray_setHandler( handlers, 
        (OTF_FunctionPointer*) handleEndFileOperation,
        OTF_ENDFILEOP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control, 
        OTF_ENDFILEOP_RECORD );
	
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleNoOp,
        OTF_NOOP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control, 
        OTF_NOOP_RECORD );


    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleEventComment,
        OTF_EVENTCOMMENT_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control, 
        OTF_EVENTCOMMENT_RECORD );


    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleBeginProcess,
        OTF_BEGINPROCESS_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control, 
        OTF_BEGINPROCESS_RECORD );


    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleEndProcess,
        OTF_ENDPROCESS_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control, 
        OTF_ENDPROCESS_RECORD );


    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleRMAPut,
        OTF_RMAPUT_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control, 
        OTF_RMAPUT_RECORD );



    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleRMAPutRemoteEnd,
        OTF_RMAPUTRE_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control, 
        OTF_RMAPUTRE_RECORD );



    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleRMAGet,
        OTF_RMAGET_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control, 
        OTF_RMAGET_RECORD );



    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleRMAEnd,
        OTF_RMAEND_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, (void*) control, 
        OTF_RMAEND_RECORD );

	
	if ( doThumbnail ) {
		uint32_t i;
		if ( tmax - tmin <= thumbnail_samples ) {
			thumbnail_samples = tmax - tmin;
		}

		/* cout << "time range [" <<  tmin << ", " << tmax << ") => " << thumbnail_samples << endl; */
		OTFAUX_State_setupThumbnail( control->aux_state,
		                             tmin,
		                             tmax,
		                             thumbnail_samples );


		if ( control->all_processes.size() < thumbnail_procs ) {
			thumbnail_procs = control->all_processes.size();
		}
		srand(1);
		for ( i = 0; i < thumbnail_procs; i++ ) {

			uint32_t r = (uint32_t)( ( (double)rand() / RAND_MAX ) *
				( control->all_processes.size() - 1 - i ) + 0.5 );
			uint32_t t = control->all_processes[i+r];
			control->all_processes[i+r] = control->all_processes[i];
			control->all_processes[i] = t;
			OTFAUX_State_declareProcess( control->aux_state, t, true );
		}

	}

    /*
     * Increment in order to place final statistics after the very last
     * event.
     */
    tmax += 1;

	/*
	cout << "total time " << 
		(unsigned long long) tmin << " - " << 
		(unsigned long long) tmax << endl;
	*/


	d= ((double) ( tmax - tmin ) ) / ((double) summary_number );
	d= ( d <= (double) summary_distance ) ? d : (double) summary_distance;
	d= ( 1.0 < d ) ? d : 1.0;

    /*
     * generated sample points, but don't include t_min,
     * it is defined as 0-point
     *
     * Tmax was incremented before to be after the very last event.
     */
    for ( double t= (double) tmin + d; t < tmax; t += d ) {
        control->addTime( (uint64_t) t );
    }


	/* append user defined time stamps */

	for ( set<uint64_t>::const_iterator it= timestamps.begin();
            it != timestamps.end();
            ++it ) {

		control->addTime( *it );
	}

    /*
     * Place very last statistics _after_ the last record but not right
     * before it. Needs to be triggered explictily in the end.
     *
     * Tmax was incremented before to be the timestamp after the very
     * last event timestamp.
     */
    if ( control->getLastTime() < tmax ) {

        control->addTime( tmax );
    }


	/* restrict streams resp. processes to be read */
    if ( !streams.empty() ) {

	/* first disable all processes */
	OTF_Reader_setProcessStatusAll( reader, 0 );

	/* then enable all processes of all selected streams */
	for (set<uint32_t>::const_iterator jt= streams.begin(); jt != streams.end(); ++jt ) {

		OTF_MapEntry* entry= OTF_MasterControl_getEntry( mc, *jt );

		if ( NULL != entry ) {

			for ( uint32_t i= 0; i < entry->n; ++i ) {

				OTF_Reader_setProcessStatus( reader, entry->values[i], 1 );
			}

		} else {
		
			printf( "WARNING: stream '%x' undefined\n", *jt );
		}
	}
    }

	if ( true == control->timestamps.empty() ) {
	
		fprintf( stderr,  " no statistics or snapshots will be created.\n" );
		exit(0);
	}

	if ( showprogress ) {

		/* show progress report */

		uint64_t min;
		uint64_t cur;
		uint64_t max;

		OTF_Reader_setRecordLimit( reader, 100000 );

		while ( 0 < ( read = OTF_Reader_readEvents( reader, handlers ) ) ) {

			if( read == OTF_READ_ERROR ) {
				fprintf(stderr,"An error occurred while reading the tracefile. It seems to be damaged. Abort. Abort.\n");
				return 1;
			}

			OTF_Reader_eventTimeProgress( reader, &min, &cur, &max );
			fprintf( stdout, "    progress %4.1f %%\r%10s", 
				100.0 * ( (double) ( cur - min ) ) / ( (double) ( max - min ) ), "" );
		}

		fprintf( stdout, "%40s\n", "" );

		OTF_Reader_setRecordLimit( reader, OTF_READ_MAXRECORDS );

	} else {

		read = OTF_Reader_readEvents( reader, handlers );
		if( read == OTF_READ_ERROR ) {
			fprintf(stderr,"An error occurred while reading the tracefile. It seems to be damaged. Abort.\n");
			return 1;
		}
		/* cout << "read " << read << " events" << endl; */
	}

    /*
     * Explicitly trigger writing for the timestamp of the very last
     * event such that this event is included in the final statistics.
     *
     * Tmax was incremented before to be the timestamp after the very
     * last event timestamp.
     */
    control->checkTime( tmax );
    
	if ( doThumbnail ) {
		OTFAUX_State_writeThumbnail( control->aux_state,
			out_namestub ? out_namestub : namestub,
			1, thumbnail_procs );
	}

out:
    delete control;

	OTF_Reader_close( reader );
    OTF_HandlerArray_close( handlers );

    if ( out_namestub ) {

        OTF_Writer_close( writer );
    } else {

        /* DO NOT close the writer in order to prevent the original MasterControl file
        to be overwritten. Instead close only all the streams of that writer. */
        OTF_Writer_closeAllStreams( writer );
        OTF_Writer_closeAllStreams( def_writer );

        /* rename new def file to old one */
        type = OTF_FILETYPE_DEF | OTF_FILECOMPRESSION_COMPRESSED;
        char* olddeffilename = OTF_getFilename( namestub, 0, type, 0, NULL );
        checkExistingFile( olddeffilename, 1, 1 );
        free( olddeffilename );

        type = OTF_FILETYPE_DEF;
        olddeffilename = OTF_getFilename( namestub, 0, type, 0, NULL );
        checkExistingFile( olddeffilename, 1, 1 );
        free( olddeffilename );

        type |= compression;
        olddeffilename = OTF_getFilename( namestub, 0, type, 0, NULL );

        /* new def file will become the old deffile */
        rename( newdeffilename, olddeffilename );
        /* mkstemp has created this fle */
        unlink( newdefnamestub );

        free( olddeffilename );
    }

    free( newdeffilename );
    free( newdefnamestub );
    free( out_namestub );
    free( namestub );
    out_namestub = NULL;
    newdefnamestub = NULL;

    OTF_FileManager_close( manager );

	return (0);
}


void checkExistingFile( const char* tmpfilename, bool forceoverwrite, bool deleteonly ) {


	FILE* tmpfile= fopen( tmpfilename, "rb" );
	if ( NULL != tmpfile ) {
	
		fclose( tmpfile );
			
		if ( forceoverwrite || deleteonly ) {

				unlink( tmpfilename );

		} else {
		
			printf( "ERROR: will not overwrite existing file '%s', abort\n", tmpfilename );
			exit( 1 );
		}
	}
}

