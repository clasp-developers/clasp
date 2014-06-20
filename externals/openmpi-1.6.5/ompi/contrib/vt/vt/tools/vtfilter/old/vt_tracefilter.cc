/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/


/*
	When generating a filterfile this tool creates a statistic over all processes and functions.
	It simply  adds all functions and all values together. Even the subfunctions of a function are simply
	put together.

	With using this statistic vtfilter decides which functions to filter.

	VampirTrace instead does filtering on a per process basis, which means
	That if someone sets the functionlimit to 3000, the function might actually be called
	12000 times on 4 different processes.
	
	vtfilter does not regard this case, because it's pretty difficult to calculate a correct
	reduction of the trace.
	 -> Filtering happens on a per process basis, but a filterfile belongs to every process.

	This means the calculation of the reduction percentage is wrong !!!
	


	Filtering using vtfilter is exactly the same as doing another tracerun with
	newly created filter file.
*/

#include "vt_filter_config.h"

#include "vt_filterc.h"
#include "vt_otfhandler.h"
#include "vt_filthandler.h"

#include <fstream>
#include <stdlib.h>
#include <string.h>
#if defined(HAVE_OMP) && HAVE_OMP
#	include <omp.h>
#endif // HAVE_OMP

#ifdef VT_MPI
#	define MASTER if( MyRank == 0 )
	static VT_MPI_INT NumRanks;
	static VT_MPI_INT MyRank;
#else // VT_MPI
#	define MASTER
#endif // VT_MPI


#if defined(HAVE_OMP) && HAVE_OMP
#	include <omp.h>
#else // HAVE_OMP
#	define omp_get_num_threads() 1
#	define omp_get_thread_num() 0
#       define omp_get_max_threads() 1
#endif // HAVE_OMP


#ifdef VT_FILTER_OLD
#	define main( n, a ) vtfilter_main( n, a )
#	define exit( s ) return s
#	ifdef VT_MPI
#		define MPI_Init( n, a )
#		define MPI_Abort( c, s )
#		define MPI_Finalize()
#	endif // VT_MPI
#endif // VT_FILTER_OLD

#if SIZEOF_LONG == 4
#  define ATOL8 atoll
#else
#  define ATOL8 atol
#endif

#define HELPTEXT "" \
"                                                                          \n" \
" vtfilter[-mpi] - filter generator for VampirTrace.                       \n" \
"                                                                          \n" \
" Syntax:                                                                  \n" \
"     Filter a trace file using an already existing filter file:           \n" \
"        vtfilter[-mpi] -filt [filt-options] <input trace file>            \n" \
"                                                                          \n" \
"     Generate a filter:                                                   \n" \
"        vtfilter[-mpi] -gen [gen-options] <input trace file>              \n" \
"                                                                          \n" \
" options:                                                                 \n" \
"     -h, --help            show this help message                         \n" \
"     -p                    show progress                                  \n" \
"                           (not yet implemented for vtfilter-mpi)         \n" \
"                                                                          \n" \
" filt-options:                                                            \n" \
"     -to <file>            output trace file name                         \n" \
"     -fi <file>            input filter file name                         \n" \
"                                                                          \n" \
"     -z <zlevel>           Set the compression level. Level reaches from 0\n" \
"                           to 9 where 0 is no compression and 9 is the    \n" \
"                           highest level. Standard is 4.                  \n" \
"     -f <n>                Set max number of file handles available.      \n" \
"                           Standard is 256.                               \n" \
"                                                                          \n" \
" gen-options:                                                             \n" \
"     -fo <file>            output filterfile name                         \n" \
"                                                                          \n" \
"     -r <n>                Reduce the trace size to <n> percent of the    \n" \
"                           original size. The program relies on the fact  \n" \
"                           that the major part of the trace are function  \n" \
"                           calls. The approximation of size will get      \n" \
"                           worse with a rising percentage of              \n" \
"                           communication and other non function calling   \n" \
"                           or performance counter records.                \n" \
"     -l <n>                Limit the number of accepted function calls    \n" \
"                           for filtered functions to <n>. Standard is 0.  \n" \
"     -ex <f>,<f>,...       Exclude certain symbols from filtering.        \n" \
"                           A symbol may contain wildcards.                \n" \
"     -in <f>,<f>,...       Force to include certain symbols into the      \n" \
"                           filter. A symbol may contain wildcards.        \n" \
"     -inc                  Automatically include children of included     \n" \
"                           functions as well into the filter.             \n" \
"                                                                          \n" \
"     -stats                Prints out the desired and the expected        \n" \
"                           percentage of file size.                       \n" \
"                                                                          \n" \
"   environment variables:                                                 \n" \
"     TRACEFILTER_EXCLUDEFILE  Specifies a file containing a list of       \n" \
"                              symbols not to be filtered. The list of     \n" \
"                              members can be seperated by space, comma,   \n" \
"                              tab, newline and may contain wildcards.     \n" \
"     TRACEFILTER_INCLUDEFILE  Specifies a file containing a list of       \n" \
"                              symbols  to be filtered.                    \n"


using namespace std;

static void initProgressDisplay();
static void finishProgressDisplay();
static void updateProgressDisplay( uint32_t i, uint64_t max, uint64_t cur );

static void dump ( const vector<Function>& functions, const set<uint32_t>& excluded,
	uint64_t timerresolution, uint64_t limit, ostream& out );
static vector<string> readTokensFromFile( const string& filename );
static map<uint32_t,uint64_t> readFilterFile( const string& filename, const map<string,uint32_t>& nm2tok );


enum Action { none, gen, filt };

int main( int argc, char** argv ) {

	ofstream of;

	string intrace;
	string outtrace;
	string infilt;
	string outfilt;
	
	vector<string> exsym;
	vector<string> insym;

	float expected_reducepercentage= 100.0f;
	float desired_reducepercentage= 100.0f;

	Action action = none;
	uint64_t invocationlimit = 0;
	bool inchildren = false;
	bool stats = false;
	OTF_FileCompression compression= 4;
	uint32_t nfiles = 256;
	bool showprogress = false;

	uint64_t retev, retst, retsn;

#ifdef VT_MPI
	MPI_Init( &argc, &argv );
	MPI_Comm_size( MPI_COMM_WORLD, &NumRanks );
	MPI_Comm_rank( MPI_COMM_WORLD, &MyRank );
#endif // VT_MPI
	
	/* *** need help? *** */
	if ( argc < 2 ) {
		MASTER cout << HELPTEXT << endl;
#ifdef VT_MPI
		MPI_Finalize();
#endif // VT_MPI
		return 0;
	}
	for( int i = 1; i < argc; i++ ) {
		if ( strcmp( "--help", argv[i] ) == 0 || strcmp( "-h", argv[i] ) == 0 ) {
			MASTER cout << HELPTEXT << endl;
#ifdef VT_MPI
			MPI_Finalize();
#endif // VT_MPI
			return 0;
		}
	}

	/* *** determine actuib to take *** */
	if( strcmp("-gen",argv[1]) == 0 ) action = gen;
	else if( strcmp("-filt",argv[1]) == 0 ) action = filt;
	else action = none;

	/* *** parse parameters *** */
	for ( int i = 2; i < argc; i++ ) {

		if ( strcmp("-fo", argv[i]) == 0 && ( i+1 < argc ) ) {
			++i;
			outfilt= argv[i];
		} else if (strcmp( "-fi",argv[i]) == 0 && ( i+1 < argc ) ) {
			++i;
			infilt = argv[i];
		} else if (strcmp( "-to",argv[i]) == 0 && ( i+1 < argc ) ) {
			++i;
			outtrace= argv[i];
		} else if ( strcmp( "-p", argv[i] ) == 0 ) {
#ifdef VT_MPI
			MASTER cerr << "Warning: Progress is not yet implemented for vtfilter-mpi. Ignoring option: \"-p\"" << endl;
#else // VT_MPI
			showprogress = true;
#endif // VT_MPI
		} else if ( 0 == strcmp("-z", argv[i]) && ( i+1 < argc ) ) {
			++i;
			compression = (uint32_t) atoi( argv[i] );
		} else if ( 0 == strcmp("-f", argv[i]) && ( i+1 < argc ) ) {
			++i;
			nfiles = (uint32_t) atoi(argv[i]);
		} else if ( 0 == strcmp("-r",argv[i]) && ( i+1 < argc ) ) {
			++i;
			desired_reducepercentage= (float) atof( argv[i] );
			expected_reducepercentage= desired_reducepercentage;
		} else if ( 0 == strcmp("-l", argv[i]) && ( i+1 < argc ) ) {
			++i;
			invocationlimit= (uint64_t) ATOL8( argv[i] );
		} else if ( 0 == strcmp( "-ex", argv[i] ) && ( i+1 < argc ) ) {

			++i;
			
			char* token= strtok( argv[i], "," );
			while( NULL != token ) {
				exsym.push_back( token );
				token= strtok( NULL, "," );
			}
			
		} else if ( 0 == strcmp( "-in", argv[i] ) && ( i+1 < argc ) ) {

			++i;

			char* token= strtok( argv[i], "," );
			while( NULL != token ) {
				insym.push_back( token );
				token= strtok( NULL, "," );
			}
			
		} else if ( 0 == strcmp( "-inc", argv[i] ) ) {

			inchildren= true;

		} else if ( 0 == strcmp( "-stats", argv[i] ) ) {

			stats= true;

		} else {

			if ( '-' != argv[i][0] ) {

				intrace= argv[i];

			} else {

				MASTER cerr << "Unknown option: \"" << argv[i] << "\". Aborting" << endl;
#ifdef VT_MPI
				MPI_Abort( MPI_COMM_WORLD, 1 );
#endif // VT_MPI
				exit(1);
			}
		}
	}

	/* *** check the correctness of the parameters *** */
	bool err = false;
	if( intrace.empty() == true ) {
		MASTER cerr << "No no input trace has been specified. Aborting" << endl;
		err = true;
	}

/*	if( nfiles < (uint32_t)omp_get_max_threads() ) {
		MASTER cerr << "The maximum number of open files is not allowed to be less than"
			       " the threadcount. Aborting" << endl;
#ifdef VT_MPI
		MPI_Abort( MPI_COMM_WORLD, 1 );
#endif // VT_MPI
		exit(1);
	}*/

	switch(action) {
		case gen:
			if( desired_reducepercentage < 0.0f || desired_reducepercentage > 100.0f ) {
				MASTER cerr << "Wrong reduction percentage: " << desired_reducepercentage << ". Aborting" << endl;
				err = true;
			}
			if( outfilt.empty() == true ) {
				MASTER cerr << "No output filter has been specified. Aborting" << endl;
				err = true;
			}
			break;
		case filt:
			if( outtrace.empty() == true ) {
				MASTER cerr << "No output trace has been specified. Aborting" << endl;
				err = true;
			}
			if( infilt.empty() == true ) {
				MASTER cerr << "No input filter has been specified. Aborting" << endl;
				err = true;
			}
			break;
		case none:
		default:
			MASTER cerr << "No action has been specified. Aborting" << endl;
			err = true;
			break;
	}

	if( err == true ) {
#ifdef VT_MPI
		MPI_Abort( MPI_COMM_WORLD, 1 );
#endif // VT_MPI
		exit(1);
	}

	/* *** cut the ".otf"-suffix from input/output trace file name *** */
	char* tmp;

	tmp = OTF_stripFilename(intrace.c_str());
	intrace = tmp;
	free(tmp);
	
	if(!outtrace.empty()) {
		tmp = OTF_stripFilename(outtrace.c_str());
		outtrace = tmp;
		free(tmp);
	}

	/* *** read the exclude symbols file, if there is one *** */
	char* tfef = getenv("TRACEFILTER_EXCLUDEFILE");
	char* tfif = getenv("TRACEFILTER_INCLUDEFILE");

	vector<string> ret = readTokensFromFile( tfef != NULL ? tfef : "" );
	for( size_t i = 0; i < ret.size(); ++i ) {
		exsym.push_back( ret[i] );
	}
	ret = readTokensFromFile( tfif != NULL ? tfif : "" );
	for( size_t i = 0; i < ret.size(); ++i ) {
		insym.push_back( ret[i] );
	}

	/* generate a new filter file */
	if( action == gen ) {

		HandlerArgument fha;
		
		fha.p2f.insert( pair<uint32_t,Filter>(0,Filter()) );

		uint64_t minbytes = 0, curbytes = 0, maxbytes = 0;

		/* *** Read the trace once without handling records -> get the min and maxbytes.
		       Read the definitions
		       Add all processes to the process2filter mapping *** */
		OTF_FileManager* pmanager;
		OTF_Reader* preader;
		OTF_HandlerArray* phandlers;

		pmanager= OTF_FileManager_open( nfiles );
		vt_assert( pmanager );
		preader = OTF_Reader_open( intrace.c_str(), pmanager );
		if( !preader ) {
			MASTER cerr << "Could not open input trace file \"" << intrace << "\". Aborting" << endl;
#ifdef VT_MPI
			MPI_Abort( MPI_COMM_WORLD, 1 );
#endif // VT_MPI
			exit(1);
		}
		phandlers = OTF_HandlerArray_open();
		vt_assert( phandlers );
		OTF_MasterControl* pmc = OTF_Reader_getMasterControl( preader );
		vt_assert( pmc );
			
		OTF_HandlerArray_setHandler( phandlers,
			(OTF_FunctionPointer*) handleDefTimerResolution, OTF_DEFTIMERRESOLUTION_RECORD );
		OTF_HandlerArray_setFirstHandlerArg( phandlers,
			&fha, OTF_DEFTIMERRESOLUTION_RECORD );

		OTF_HandlerArray_setHandler( phandlers,
			(OTF_FunctionPointer*) handleDefFunction, OTF_DEFFUNCTION_RECORD );
		OTF_HandlerArray_setFirstHandlerArg( phandlers,
			&fha, OTF_DEFFUNCTION_RECORD );

#ifndef VT_MPI
		uint64_t pminbytestmp, pcurbytestmp, pmaxbytestmp;

		/* progress stuff */
		OTF_Reader_setRecordLimit( preader, 0 );

		uint64_t pretev= OTF_Reader_readEvents( preader, phandlers );
		if( pretev == OTF_READ_ERROR ) {
			MASTER cerr << "Error while reading Events. Aborting" << endl;
			exit(1);
		}

		OTF_Reader_eventBytesProgress( preader, &pminbytestmp, &pcurbytestmp, &pmaxbytestmp );
		minbytes += pminbytestmp;
		maxbytes += pmaxbytestmp;
#else // VT_MPI
		/* *** minbytes and maxbytes not used for vtfilter-mpi
		       do the following to prevent "set but not used" warnings
		       when building with the PGI compiler *** */
		minbytes++;
		maxbytes++;
#endif // VT_MPI
		
		/* defs */
		OTF_Reader_setRecordLimit( preader, OTF_READ_MAXRECORDS );

		uint64_t pretde= OTF_Reader_readDefinitions( preader, phandlers );
		if( pretde == OTF_READ_ERROR ) {
			MASTER cerr << "Error while reading Definitions. Aborting" << endl;
#ifdef VT_MPI
			MPI_Abort( MPI_COMM_WORLD, 1 );
#endif // VT_MPI
			exit(1);
		}
		
		OTF_HandlerArray_close( phandlers );
		OTF_Reader_close( preader );
		OTF_FileManager_close( pmanager );


		if( showprogress ) {
			initProgressDisplay();
		}


		/* open the mastercontrol file */
		OTF_FileManager* mcmanager = OTF_FileManager_open(nfiles);
		vt_assert(mcmanager);
		OTF_MasterControl* mc = OTF_MasterControl_new(mcmanager);
		vt_assert(mc);
		if( 0 == OTF_MasterControl_read( mc, intrace.c_str() ) ) {
			MASTER cerr << "Could not read OTF Master Control File \"" << intrace << ".otf\". Aborting" << endl;
#ifdef VT_MPI
			MPI_Abort( MPI_COMM_WORLD, 1 );
#endif // VT_MPI
			exit(1);
		}
		
		/* *** prepare the parallel construct *** */
		int maxstreams = (int) OTF_MasterControl_getCount(mc);
		
		uint64_t readrecords= 0;
		uint64_t lastprogressupdate = 0;
		uint64_t progress_counter = 0;
		bool erroroccured = false;

		int streamindex_begin = 0;
		int streamindex_end = maxstreams;

#ifdef VT_MPI
		/* distribute streams to MPI ranks */
		int nworker = (NumRanks > maxstreams) ? maxstreams : NumRanks;
		int bstreams = maxstreams / nworker;
		bool idlerank = false;
		streamindex_begin = MyRank * bstreams;
		streamindex_end = streamindex_begin + bstreams;
		if( (MyRank == nworker - 1) && streamindex_end != maxstreams )
			streamindex_end = maxstreams;
		/* idle MPI rank? */
		if( MyRank >= nworker ) {
			streamindex_begin = streamindex_end = 0;
			fha.p2f.clear();
			idlerank = true;
		}
#endif // VT_MPI

		/* insert processes into the process2filter mapping */
		for( int i = 0; i < maxstreams; ++i ) {
			if( i >= streamindex_begin && i < streamindex_end ) {
				OTF_MapEntry* entry = OTF_MasterControl_getEntryByIndex( mc, i );
				for( uint32_t j = 0; j < entry->n; j++ ) {
					fha.p2f.insert( pair<uint32_t,Filter>(entry->values[j],Filter()) );
				}
			}
		}

#		if defined(HAVE_OMP) && HAVE_OMP
		if( streamindex_end-streamindex_begin < omp_get_max_threads() )
			omp_set_num_threads( streamindex_end-streamindex_begin );
#		pragma omp parallel for firstprivate(mc,nfiles,minbytes,maxbytes) \
			shared(fha,curbytes,readrecords,lastprogressupdate,progress_counter,erroroccured) \
			private(retev,retst,retsn)
#		endif // HAVE_OMP
		for( int streamindex = streamindex_begin; streamindex < streamindex_end; ++streamindex )
		{
			/* *** init otf *** */
			OTF_FileManager* manager;
			OTF_RStream* rstream;
			OTF_HandlerArray* handlers;

			uint32_t streamid = OTF_MasterControl_getEntryByIndex(mc,(uint32_t)streamindex)->argument;

			manager= OTF_FileManager_open( nfiles );
			vt_assert( manager );
			rstream = OTF_RStream_open( intrace.c_str(), streamid, manager );
			vt_assert( rstream );
			handlers = OTF_HandlerArray_open();
			vt_assert( handlers );

			
			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) handleEnter, OTF_ENTER_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, 
				&fha, OTF_ENTER_RECORD );

			OTF_HandlerArray_setHandler( handlers,
				(OTF_FunctionPointer*) handleLeave, OTF_LEAVE_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, 
				&fha, OTF_LEAVE_RECORD );

			OTF_HandlerArray_setHandler( handlers, 
				(OTF_FunctionPointer*) handleCollectiveOperation,
				OTF_COLLOP_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, 
				&fha, OTF_COLLOP_RECORD );

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

			if( showprogress ) {

				uint64_t minbytestmp, curbytestmp, lastbytestmp = 0, maxbytestmp;

				OTF_RStream_setRecordLimit( rstream, 100000 );

				while ( 0 != ( retev= OTF_RStream_readEvents( rstream, handlers ) ) ) {

					if( OTF_READ_ERROR == retev ) {
#						if defined(HAVE_OMP) && HAVE_OMP
#						pragma omp critical (consoleout)
#						endif // HAVE_OMP
						{
							cerr << "Error while reading events. aborting" << endl;
						}

						erroroccured = true;
						break;
					}

					OTF_RStream_eventBytesProgress( rstream,
						&minbytestmp, &curbytestmp, &maxbytestmp );

					/* update the progress */
#					if defined(HAVE_OMP) && HAVE_OMP
#					pragma omp critical (progressupdate)
#					endif // HAVE_OMP
					{
						curbytes += curbytestmp - lastbytestmp;

						lastbytestmp = curbytestmp;

						readrecords += retev;

						if( readrecords - lastprogressupdate >= 100000 ) {
							updateProgressDisplay( progress_counter++, maxbytes - minbytes, curbytes );
							lastprogressupdate = readrecords;
						}
					}


				}

			} else {

				retev = OTF_RStream_readEvents( rstream, handlers );
				if( retev == OTF_READ_ERROR ) {
#					if defined(HAVE_OMP) && HAVE_OMP
#					pragma omp critical (consoleout)
#					endif // HAVE_OMP
					{
						cerr << "Error while reading events. aborting" << endl;
					}
					erroroccured = true;
				}

			}

			OTF_HandlerArray_close( handlers );
			OTF_RStream_close( rstream );
			OTF_FileManager_close( manager );

		} /* end parallel */

		OTF_MasterControl_close(mc);
		OTF_FileManager_close(mcmanager);
		
		if( showprogress ) {
			finishProgressDisplay();
		}

		if( erroroccured ) {
#ifdef VT_MPI
			MPI_Abort( MPI_COMM_WORLD, 1 );
#endif // VT_MPI
			exit(1);
		}

		Filter filter;

		/* gather all information from the processes into a single filter structure */
		map<uint32_t /*procid*/, Filter>::const_iterator itfuncs;
		for( itfuncs = fha.p2f.begin(); itfuncs != fha.p2f.end(); ++itfuncs ) {
			filter += itfuncs->second;
		}

#ifdef VT_MPI
		if( NumRanks > 1 ) {
			/* send filter structure to rank 0 */

			MPI_Barrier( MPI_COMM_WORLD );

			VT_MPI_INT buffer_size = 0;
			char* buffer;

			if( MyRank != 0 && !idlerank ) {
				/* transform filter structure to buffer */
				buffer_size = filter.getPackSize();
				buffer = new char[buffer_size];

				VT_MPI_INT buffer_pos = 0;
				filter.packBuffer( buffer, buffer_size, buffer_pos );

				/* send filter buffer to rank 0 */
				MPI_Send( &buffer_size, 1, MPI_INT, 0, 100, MPI_COMM_WORLD );
				MPI_Send( buffer, buffer_size, MPI_PACKED, 0, 200, MPI_COMM_WORLD );

				delete [] buffer;
			} else if( MyRank == 0 ) {
				/* collect filter structures from all other ranks */
				for( int i = 1; i < nworker; i++ ) {
					/* receive filter buffer from rank i */
					MPI_Recv( &buffer_size, 1, MPI_INT, i, 100, MPI_COMM_WORLD, MPI_STATUS_IGNORE );
					buffer = new char[buffer_size];
					MPI_Recv( buffer, buffer_size, MPI_PACKED, i, 200, MPI_COMM_WORLD, MPI_STATUS_IGNORE );

					/* transform buffer to filter structure */
					Filter f;
					VT_MPI_INT buffer_pos = 0;
					f.unpackBuffer( buffer, buffer_size, buffer_pos );
					filter += f;

					delete [] buffer;
				}
			}
		}
#endif // VT_MPI

		MASTER {
			/* do some calculations after adding all functions */
			filter.postProcessing();

		
			/* all functions, including statistics */
			vector<Function> functions = filter.getFunctions();


			/* *** get the excluded and included tokens from their names *** */
			set<uint32_t> extok;
			set<uint32_t> intok;
			vector<string>::const_iterator its; /* symbol iterator for in- and excludes */
			vector<Function>::const_iterator itf; /* iterator for functions */
			bool excluded;


			for( itf = functions.begin(); itf != functions.end(); ++itf ) {

				excluded = false;

				for( its = exsym.begin(); its != exsym.end(); ++its ) {

					if( 0 == fnmatch( its->c_str(), itf->name.c_str(), FNM_NOESCAPE ) ) {
						extok.insert( itf->id );
						excluded = true;
						break;
					}
				}

				/* you cannot include something you already excluded */
				if( excluded ) continue;

				for( its = insym.begin(); its != insym.end(); ++its ) {

					if( 0 == fnmatch( its->c_str(), itf->name.c_str(), FNM_NOESCAPE ) ) {
						intok.insert( itf->id );
						break;
					}
				}
			}


			/* reduce functions - judging part */
			set<uint32_t> killed= filter.reduceTo( &expected_reducepercentage, extok,
				intok, inchildren, invocationlimit );

			/* print out some information */
			if( true == stats ) {
				cout << "desired " << desired_reducepercentage << "% expected " << expected_reducepercentage << "%" << endl;
			}

			/* print the filter + report to specified filter file */
			of.open( outfilt.c_str() );
		
			if( of.is_open() ) {
				dump( functions, killed, filter.getTimerResolution(), invocationlimit, of );
			} else {
				cerr << "Could not open output file \"" << outfilt << "\". Aborting" << endl;
			}

			of.close();
		}

	} else if( action == filt ) {

		MASTER {

#ifdef VT_MPI
		if( NumRanks > 1 )
			cerr << "Warning: Filtering a trace in parallel is not yet implemented. Using 1 process." << endl;
#endif // VT_MPI

		FiltHandlerArgument fha;
		
		/* Open the mastercontrol file -> threads need it to determine their workload.
		   Copy the mastercontrol file to the new trace */
		OTF_FileManager* mcmanager = OTF_FileManager_open(nfiles);
		vt_assert(mcmanager);
		fha.mc = OTF_MasterControl_new(mcmanager);
		vt_assert(fha.mc);
		if( 1 == OTF_MasterControl_read( fha.mc, intrace.c_str() ) ) {
			if( 0 == OTF_MasterControl_write( fha.mc, outtrace.c_str() ) ) {
				cerr << "Could not write OTF Master Control File \"" << outtrace << ".otf\". Aborting" << endl;
#ifdef VT_MPI
				MPI_Abort( MPI_COMM_WORLD, 1 );
#endif // VT_MPI
				exit(1);
			}
		} else {
			cerr << "Could not read OTF Master Control File \"" << intrace << ".otf\". Aborting" << endl;
#ifdef VT_MPI
			MPI_Abort( MPI_COMM_WORLD, 1 );
#endif // VT_MPI
			exit(1);
		}
		
		/* *** Read the Definitions:
		       1. Copy them
		       2. create a name-token mapping
		*** */
		OTF_FileManager* defmanager;
		OTF_RStream* defrstream;
		OTF_HandlerArray* defhandlers;

		defmanager= OTF_FileManager_open( nfiles );
		vt_assert( defmanager );
		defhandlers = OTF_HandlerArray_open();
		vt_assert( defhandlers );


		OTF_HandlerArray_setHandler( defhandlers, (OTF_FunctionPointer*) handleFiltDefinitionComment,
			OTF_DEFINITIONCOMMENT_RECORD );
		OTF_HandlerArray_setFirstHandlerArg( defhandlers, &fha, OTF_DEFINITIONCOMMENT_RECORD );

		OTF_HandlerArray_setHandler( defhandlers, (OTF_FunctionPointer*) handleFiltDefTimerResolution,
			OTF_DEFTIMERRESOLUTION_RECORD );
		OTF_HandlerArray_setFirstHandlerArg( defhandlers, &fha, OTF_DEFTIMERRESOLUTION_RECORD );

		OTF_HandlerArray_setHandler( defhandlers, (OTF_FunctionPointer*) handleFiltDefProcess,
			OTF_DEFPROCESS_RECORD );
		OTF_HandlerArray_setFirstHandlerArg( defhandlers, &fha, OTF_DEFPROCESS_RECORD );

		OTF_HandlerArray_setHandler( defhandlers, (OTF_FunctionPointer*) handleFiltDefProcessGroup,
			OTF_DEFPROCESSGROUP_RECORD );
		OTF_HandlerArray_setFirstHandlerArg( defhandlers, &fha, OTF_DEFPROCESSGROUP_RECORD );

		OTF_HandlerArray_setHandler( defhandlers, (OTF_FunctionPointer*) handleFiltDefFunction,
			OTF_DEFFUNCTION_RECORD );
		OTF_HandlerArray_setFirstHandlerArg( defhandlers, &fha, OTF_DEFFUNCTION_RECORD );

		OTF_HandlerArray_setHandler( defhandlers, (OTF_FunctionPointer*) handleFiltDefFunctionGroup,
			OTF_DEFFUNCTIONGROUP_RECORD );
		OTF_HandlerArray_setFirstHandlerArg( defhandlers, &fha, OTF_DEFFUNCTIONGROUP_RECORD );

		OTF_HandlerArray_setHandler( defhandlers, (OTF_FunctionPointer*) handleFiltDefCollectiveOperation,
			OTF_DEFCOLLOP_RECORD );
		OTF_HandlerArray_setFirstHandlerArg( defhandlers, &fha, OTF_DEFCOLLOP_RECORD );

		OTF_HandlerArray_setHandler( defhandlers, (OTF_FunctionPointer*) handleFiltDefCounter,
			OTF_DEFCOUNTER_RECORD );
		OTF_HandlerArray_setFirstHandlerArg( defhandlers, &fha, OTF_DEFCOUNTER_RECORD );

		OTF_HandlerArray_setHandler( defhandlers, (OTF_FunctionPointer*) handleFiltDefCounterGroup,
			OTF_DEFCOUNTERGROUP_RECORD );
		OTF_HandlerArray_setFirstHandlerArg( defhandlers, &fha, OTF_DEFCOUNTERGROUP_RECORD );

		OTF_HandlerArray_setHandler( defhandlers, (OTF_FunctionPointer*) handleFiltDefScl,
			OTF_DEFSCL_RECORD );
		OTF_HandlerArray_setFirstHandlerArg( defhandlers, &fha, OTF_DEFSCL_RECORD );

		OTF_HandlerArray_setHandler( defhandlers, (OTF_FunctionPointer*) handleFiltDefSclFile,
			OTF_DEFSCLFILE_RECORD );
		OTF_HandlerArray_setFirstHandlerArg( defhandlers, &fha, OTF_DEFSCLFILE_RECORD );

		OTF_HandlerArray_setHandler( defhandlers, (OTF_FunctionPointer*) handleFiltDefCreator,
			OTF_DEFCREATOR_RECORD );
		OTF_HandlerArray_setFirstHandlerArg( defhandlers, &fha, OTF_DEFCREATOR_RECORD );

		OTF_HandlerArray_setHandler( defhandlers, (OTF_FunctionPointer*) handleFiltDefFile,
			OTF_DEFFILE_RECORD );
		OTF_HandlerArray_setFirstHandlerArg( defhandlers, &fha, OTF_DEFFILE_RECORD );

		OTF_HandlerArray_setHandler( defhandlers, (OTF_FunctionPointer*) handleFiltDefFileGroup,
			OTF_DEFFILEGROUP_RECORD );
		OTF_HandlerArray_setFirstHandlerArg( defhandlers, &fha, OTF_DEFFILEGROUP_RECORD );


		/* explicitly read stream 0 */

		uint32_t streamid = 0;

		defrstream = OTF_RStream_open( intrace.c_str(), streamid, defmanager );
		vt_assert( defrstream );
		fha.wstream = OTF_WStream_open( outtrace.c_str(), streamid, defmanager );
		vt_assert( fha.wstream );

		OTF_WStream_setCompression( fha.wstream, compression );


		if( OTF_RStream_getDefBuffer(defrstream) != NULL
			&& OTF_RStream_readDefinitions( defrstream, defhandlers ) == OTF_READ_ERROR ) {
			cerr << "Error while reading definitions. aborting" << endl;
			OTF_WStream_close( fha.wstream );
			OTF_RStream_close( defrstream );
			OTF_HandlerArray_close( defhandlers );
#ifdef VT_MPI
			MPI_Abort( MPI_COMM_WORLD, 1 );
#endif // VT_MPI
			exit(1);
		}

		OTF_WStream_close( fha.wstream );
		OTF_RStream_close( defrstream );


		/* read all other streams */
		for( uint32_t i = 0; i < OTF_MasterControl_getCount(fha.mc); ++i ) {
		
			uint32_t streamid = OTF_MasterControl_getEntryByIndex(fha.mc,i)->argument ;
			
			defrstream = OTF_RStream_open( intrace.c_str(), streamid, defmanager );
			vt_assert( defrstream );
			fha.wstream = OTF_WStream_open( outtrace.c_str(), streamid, defmanager );
			vt_assert( fha.wstream );

			OTF_WStream_setCompression( fha.wstream, compression );


			if( OTF_RStream_getDefBuffer(defrstream) != NULL
				&& OTF_RStream_readDefinitions( defrstream, defhandlers ) == OTF_READ_ERROR ) {
				cerr << "Error while reading definitions. aborting" << endl;
				OTF_WStream_close( fha.wstream );
				OTF_RStream_close( defrstream );
				OTF_HandlerArray_close( defhandlers );
#ifdef VT_MPI
				MPI_Abort( MPI_COMM_WORLD, 1 );
#endif // VT_MPI
				exit(1);
			}
			
			OTF_WStream_close( fha.wstream );
			OTF_RStream_close( defrstream );
		}
		
		OTF_HandlerArray_close( defhandlers );
		OTF_FileManager_close( defmanager );


		/* create the function->limit mapping for every process */
		map<uint32_t,uint64_t> f2l = readFilterFile( infilt, fha.nm2tok );
		
		map<uint32_t,Process>::iterator itp;
		for( itp = fha.t2p.begin(); itp != fha.t2p.end(); ++itp ) {
			itp->second.f2l = f2l;
		}


		uint64_t minbytes = 0, curbytes = 0, maxbytes = 0;
		uint64_t pminbytestmp, pcurbytestmp, pmaxbytestmp;

		/* *** read the trace once without handling records -> get the min and maxbytes *** */
		OTF_FileManager* pmanager;
		OTF_Reader* preader;
		OTF_HandlerArray* phandlers;

		pmanager= OTF_FileManager_open( nfiles );
		vt_assert( pmanager );
		preader = OTF_Reader_open( intrace.c_str(), pmanager );
		vt_assert( preader );
		phandlers = OTF_HandlerArray_open();
		vt_assert( phandlers );

		OTF_Reader_setRecordLimit( preader, 0 );

		if( OTF_READ_ERROR != OTF_Reader_readEvents( preader, phandlers ) ) {
			OTF_Reader_eventBytesProgress( preader, &pminbytestmp, &pcurbytestmp, &pmaxbytestmp );
			minbytes += pminbytestmp;
			maxbytes += pmaxbytestmp;
		}
		if( OTF_READ_ERROR != OTF_Reader_readStatistics( preader, phandlers ) ) {
			OTF_Reader_statisticBytesProgress( preader, &pminbytestmp, &pcurbytestmp, &pmaxbytestmp );
			minbytes += pminbytestmp;
			maxbytes += pmaxbytestmp;
		}
		if( OTF_READ_ERROR != OTF_Reader_readSnapshots( preader, phandlers ) ) {
			OTF_Reader_snapshotBytesProgress( preader, &pminbytestmp, &pcurbytestmp, &pmaxbytestmp );
			minbytes += pminbytestmp;
			maxbytes += pmaxbytestmp;
		}
		
		
		OTF_HandlerArray_close( phandlers );
		OTF_Reader_close( preader );
		OTF_FileManager_close( pmanager );
		

		if( showprogress ) {
			initProgressDisplay();
		}

		/* *** prepare the parallel construct *** */
		int maxstreams = (int) OTF_MasterControl_getCount(fha.mc);
		
		uint64_t readrecords= 0;
		uint64_t lastprogressupdate = 0;
		uint32_t progress_counter = 0;

		//cerr << "parallel part " << endl;

#		if defined(HAVE_OMP) && HAVE_OMP
#		pragma omp parallel for firstprivate(fha,nfiles,compression,minbytes,maxbytes) \
			shared(curbytes,readrecords,lastprogressupdate,progress_counter) \
			private(retev,retst,retsn)
#		endif // HAVE_OMP
		for( int streamindex = 0; streamindex < maxstreams; ++streamindex )
		{

			uint32_t maxfiles = ( nfiles / omp_get_num_threads() ) +
				( (uint32_t)omp_get_thread_num() < (nfiles % (uint32_t)omp_get_num_threads()) ? 1 : 0 );
			
			uint32_t streamid = OTF_MasterControl_getEntryByIndex(fha.mc,(uint32_t)streamindex)->argument;


			OTF_FileManager* manager;
			OTF_RStream* rstream;
			OTF_HandlerArray* handlers;
			
			manager= OTF_FileManager_open( maxfiles );
			vt_assert( manager );
			rstream = OTF_RStream_open( intrace.c_str(), streamid, manager );
			vt_assert( rstream );
			handlers = OTF_HandlerArray_open();
			vt_assert( handlers );
			fha.wstream = OTF_WStream_open( outtrace.c_str(), streamid, manager );
			vt_assert( fha.wstream );


			//cerr << omp_get_thread_num() << " opened " << streamid << " " << fha.wstream << endl;
			
			
			OTF_WStream_setCompression( fha.wstream, compression );


			OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleFiltEnter,
				OTF_ENTER_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_ENTER_RECORD );

			OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleFiltLeave,
				OTF_LEAVE_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_LEAVE_RECORD );

			OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleFiltSendMsg,
				OTF_SEND_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_SEND_RECORD );

			OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleFiltRecvMsg,
				OTF_RECEIVE_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_RECEIVE_RECORD );

			OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleFiltCounter,
				OTF_COUNTER_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_COUNTER_RECORD );

			OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleFiltCollectiveOperation,
				OTF_COLLOP_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_COLLOP_RECORD );

			OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleFiltEventComment,
				OTF_EVENTCOMMENT_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_EVENTCOMMENT_RECORD );

			OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleFiltBeginProcess,
				OTF_BEGINPROCESS_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_BEGINPROCESS_RECORD );

			OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleFiltEndProcess,
				OTF_ENDPROCESS_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_ENDPROCESS_RECORD );

			OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleFiltFileOperation,
				OTF_FILEOPERATION_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_FILEOPERATION_RECORD );

			OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleFiltSnapshotComment,
				OTF_SNAPSHOTCOMMENT_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_SNAPSHOTCOMMENT_RECORD );

			OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleFiltEnterSnapshot,
				OTF_ENTERSNAPSHOT_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_ENTERSNAPSHOT_RECORD );

			OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleFiltSendSnapshot,
				OTF_SENDSNAPSHOT_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_SENDSNAPSHOT_RECORD );

			OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleFiltOpenFileSnapshot,
				OTF_OPENFILESNAPSHOT_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_OPENFILESNAPSHOT_RECORD );

			OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleFiltSummaryComment,
				OTF_SUMMARYCOMMENT_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_SUMMARYCOMMENT_RECORD );

			OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleFiltFunctionSummary,
				OTF_FUNCTIONSUMMARY_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_FUNCTIONSUMMARY_RECORD );

			OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleFiltFunctionGroupSummary,
				OTF_FUNCTIONGROUPSUMMARY_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_FUNCTIONGROUPSUMMARY_RECORD );

			OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleFiltMessageSummary,
				OTF_MESSAGESUMMARY_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_MESSAGESUMMARY_RECORD );

			OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleFiltFileOperationSummary,
				OTF_FILEOPERATIONSUMMARY_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_FILEOPERATIONSUMMARY_RECORD );

			OTF_HandlerArray_setHandler( handlers, (OTF_FunctionPointer*) handleFiltFileGroupOperationSummary,
				OTF_FILEGROUPOPERATIONSUMMARY_RECORD );
			OTF_HandlerArray_setFirstHandlerArg( handlers, &fha, OTF_FILEGROUPOPERATIONSUMMARY_RECORD );


			if( showprogress ) {

				uint64_t lastbytestmp = 0;
				uint64_t minbytestmp, curbytestmp, maxbytestmp;


				OTF_RStream_setRecordLimit( rstream, 100000 );

				//cerr << omp_get_thread_num() << " read events" << endl;
				while ( 0 != ( retev= OTF_RStream_readEvents( rstream, handlers ) ) ) {

					if( OTF_READ_ERROR == retev ) {
						break;
					}

					OTF_RStream_eventBytesProgress( rstream,
						&minbytestmp, &curbytestmp, &maxbytestmp );

#					if defined(HAVE_OMP) && HAVE_OMP
#					pragma omp critical (progressupdate)
#					endif // HAVE_OMP
					{
						curbytes += curbytestmp - lastbytestmp;
						lastbytestmp = curbytestmp;

						readrecords += retev;

						if( readrecords - lastprogressupdate >= 100000 ) {
							updateProgressDisplay( progress_counter++, maxbytes - minbytes, curbytes );
							lastprogressupdate = readrecords;
						}
					}
				}

				//cerr << omp_get_thread_num() << " end read events" << endl;
				
				lastbytestmp = 0;

				/* read stats */
				while ( 0 != ( retst= OTF_RStream_readStatistics( rstream, handlers ) ) ) {

					if( OTF_READ_ERROR == retst ) {
						break;
					}

					OTF_RStream_statisticBytesProgress( rstream,
						&minbytestmp, &curbytestmp, &maxbytestmp );

#					if defined(HAVE_OMP) && HAVE_OMP
#					pragma omp critical (progressupdate)
#					endif // HAVE_OMP
					{
						curbytes += curbytestmp - lastbytestmp;
						lastbytestmp = curbytestmp;

						readrecords += retev;

						if( readrecords - lastprogressupdate >= 100000 ) {
							updateProgressDisplay( progress_counter++, maxbytes - minbytes, curbytes );
							lastprogressupdate = readrecords;
						}
					}
				}

				lastbytestmp = 0;

				/* read snaps */
				while ( 0 != ( retsn= OTF_RStream_readSnapshots( rstream, handlers ) ) ) {

					if( OTF_READ_ERROR == retsn ) {
						break;
					}

					OTF_RStream_snapshotBytesProgress( rstream,
							&minbytestmp, &curbytestmp, &maxbytestmp );

#					if defined(HAVE_OMP) && HAVE_OMP
#					pragma omp critical (progressupdate)
#					endif // HAVE_OMP
					{
						curbytes += curbytestmp - lastbytestmp;
						lastbytestmp = curbytestmp;

						readrecords += retev;

						if( readrecords - lastprogressupdate >= 100000 ) {
							updateProgressDisplay( progress_counter++, maxbytes - minbytes, curbytes );
							lastprogressupdate = readrecords;
						}
					}
				}


			} else {

				/* filter events */
				OTF_RStream_readEvents( rstream, handlers );
				/* copy statistics */
				OTF_RStream_readStatistics( rstream, handlers );
				/* copy snapshots */
				OTF_RStream_readSnapshots( rstream, handlers );

			}

			/* close everything */
			OTF_HandlerArray_close( handlers );
			OTF_WStream_close( fha.wstream );
			OTF_RStream_close( rstream );
			OTF_FileManager_close( manager );


		} /* end parallel for */

		if( showprogress ) {
			finishProgressDisplay();
		}
		
		OTF_MasterControl_close(fha.mc);
		OTF_FileManager_close(mcmanager);
	} // MASTER

	}

#ifdef VT_MPI
	MPI_Barrier( MPI_COMM_WORLD );
	MPI_Finalize();
#endif // VT_MPI

	return 0;
}


void dump ( const vector<Function>& functions, const set<uint32_t>& excluded, uint64_t timerresolution, uint64_t limit, ostream& out ) {


	vector<string> excludedNames;

	out << "#### all functions sorted by stackdepth, subfunction count and invocationcount ###" << endl;
	out << "# del? - token - maxstackdepth - subfunccount - invocationcount - avg duration incl - avg duration excl - symbolname - subfunctionlist" << endl;


	for( vector<Function>::const_iterator it= functions.begin(); it != functions.end(); ++it ) {

		out << "# ";
		
		/* add the excluded function to the names list */
		if( excluded.find( it->id ) != excluded.end() ) {
			excludedNames.push_back( it->name );
			out << "x";
		} else {
			out << " ";
		}
		
		out.width( 5 );
		out << it->id  << " ";
		out.width( 3 );
		out << it->depth << " ";
		out.width( 3 );
		out << it->subFuncs.size() << " ";
		out.width( 9 );
		out << it->invocations << " ";
		out.width( 18 ); out.precision( 8 );
		if( 0 != it->invocations ) out << fixed << ((double)it->accDurationIncl / (double)it->invocations) / (double)timerresolution << " ";
		else out << "-" << " ";
		out.width( 18 ); out.precision( 8 );
		if( 0 != it->invocations ) out << fixed << ((double)it->accDurationExcl / (double)it->invocations) / (double)timerresolution << "   ";
		else out << "-" << "   ";
		out.width( 0 );
		out << it->name << "  -->  { ";
			
		for( set<uint32_t>::const_iterator it2= it->subFuncs.begin(); it2 != it->subFuncs.end(); ++it2 ) {

			out << *it2 << ", ";
		}

		out << " }" << endl;
	}


	out << endl << endl;

	out << "#### Filter ###" << endl;


	vector<string>::const_iterator its;
	for( its= excludedNames.begin(); its != excludedNames.end(); ++its ) {

		out << *its << " -- " << limit << endl;
	}
}


#define BUFSIZE 4096
vector<string> readTokensFromFile( const string& filename ) {

	FILE* file;
	vector<string> ret;


	if( filename.empty() == true && ( file= fopen( filename.c_str(), "r" ) ) != NULL ) {

		uint64_t readsize= 0;
		uint64_t lastreadsize= 0;
		uint64_t bufc= BUFSIZE;
		char *bufv= (char*) malloc( sizeof(char) * bufc );
		char *bufv2= bufv;
		
		readsize= lastreadsize= fread( bufv2, sizeof( char ), BUFSIZE, file );
		bufv2+= lastreadsize;
		while( lastreadsize > 0 ) {

			if( readsize+BUFSIZE > bufc ) {
				bufc+= BUFSIZE;
				bufv= (char*) realloc( bufv, sizeof(char)* bufc);
			}

			lastreadsize= fread( bufv2, sizeof( char ), BUFSIZE, file );
			bufv2+= lastreadsize;
			readsize+= lastreadsize;
		}
		fclose( file );
		
		char* token= strtok( bufv, " ,\n\t" );
		while( NULL != token ) {

			ret.push_back( token );
			cerr <<  token << endl;
			token= strtok( NULL, ",\n\t " );
		}

		free( bufv );
	}


	return ret;
}
#undef BUFSIZE


static map<uint32_t,uint64_t> readFilterFile( const string& filename, const map<string,uint32_t>& nm2tok ) {

	map<uint32_t,uint64_t> ret;

	fstream i;
    string line;
	char* doubledash;
	char* sline;
    string func;
	uint64_t ulimit;
    int a;

    if( filename.empty() == false ) {
        i.open( filename.c_str(), ios_base::in );
        if( i.is_open() == false ) return ret;

        for(;;) {

            getline( i, line );
            if( i.eof() == true ) break;

			doubledash = const_cast<char*>(strstr(line.c_str(), " -- ")); 
			if( doubledash != NULL ) a = doubledash-line.c_str();
			else a = -1;

            if( a == -1 ) continue;

            ulimit= ATOL8(line.substr(a+4, line.size()-a-4).c_str());
            line= line.substr(0, a);
            sline= new char[line.length()+1];
            strncpy( sline, line.c_str(), line.length() );
            sline[line.length()] = '\0';

            char* token = strtok(sline, ";");
			while( token ) {
				map<string,uint32_t>::const_iterator it = nm2tok.begin();
				for( it = nm2tok.begin(); it != nm2tok.end(); ++it ) {
					if( fnmatch(token, it->first.c_str(), FNM_NOESCAPE) == 0 ) {
						map<uint32_t,uint64_t>::const_iterator it2 = ret.find(it->second);
						if( it2 == ret.end() ) {
							ret.insert(pair<uint32_t,uint64_t>(it->second,ulimit));
						}
					}
				}
				token = strtok(NULL,";");
            }

			delete[] sline;
        }

        i.close();

    }

	return ret;
}


void initProgressDisplay() {


	printf( " %7.2f %%\r", 0.0 );
	fflush( stdout );
}


void finishProgressDisplay() {


	printf( " %7.2f %%  done\n", 100.0 );
	fflush( stdout );
}


void updateProgressDisplay( uint32_t i, uint64_t max, uint64_t cur ) {


/*	static char animation[]= {"-", "\\", "|", "/" }; */
	static const char* animation[]= { "", "." };


/*	printf( "%llu / %llu \n", cur, max ); */

	printf( " %7.2f %%  %s \r", 
		( ((double) cur) * 100.0 / ((double) max) ),
		animation[ i % ( sizeof(animation) / sizeof(animation[0]) ) ] );
	fflush( stdout );
}
