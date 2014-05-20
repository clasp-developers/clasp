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
#include <unistd.h>

#include "OTF_Platform.h"
#include "otf.h"

#ifdef HAVE_VTF3
#include "vtf3.h"
#endif /* HAVE_VTF3 */

#include "Handler.h"
#include "Stack.h"

#define VTF32OTFCREATOR "vtf2otf"

#define DEFAULT_OUTFILE "out"

#define SHOW_HELPTEXT { \
	int l = 0; while( Helptext[l] ) { printf( "%s", Helptext[l++] ); } }

static const char* Helptext[] = {
"                                                                           \n",
" vtf2otf - Convert VTF3 trace files to OTF format.                         \n",
"                                                                           \n",
" Syntax: vtf2otf [options] <input file name>                               \n",
"                                                                           \n",
"   options:                                                                \n",
"      -h, --help    show this help message                                 \n",
"      -V            show OTF version                                       \n",
"      -o <file>     namestub of the output file                            \n",
"                    (default: "DEFAULT_OUTFILE")                           \n",
"      -f <n>        max count of filehandles                               \n",
"                    (default: 100)                                         \n",
"      -n <n>        output stream count                                    \n",
"      -b <n>        size of the writer buffer                              \n",
"      -z <n>        use zlib compression                                   \n",
"      -io           compute io events. This is necessary for getting       \n",
"                    correct durations in IO-operations. Result of this     \n",
"                    step is a file with extra information. This file       \n",
"                    is used for creating correct duration-information in   \n",
"                    a normal run.                                          \n",
"                    If you do not have these extra-information-file, the   \n",
"                    duration of every IO-operation will be zero.           \n",
"                                                                           \n",
NULL };

int main (int argc, char **argv) {


	char* inputFile = NULL;
	char* outputFile = NULL;
	unsigned int nstreams= 0;
	unsigned int maxfilehandles= 100;

	void *fcbin;
	fcbT fcb;
	int nrectypes;
	int *recordtypes;
	int i;
	int a;
	VTF3_handler_t *handlers;
	void **firsthandlerargs;
	size_t bytesread;
	OTF_FileManager* manager= NULL;
	int buffersize = 1024 * 1024;
	OTF_FileCompression compression= OTF_FILECOMPRESSION_UNCOMPRESSED;
	char iofile[128];
	
	fcb.ioonly= 0;
	
	/* argument handling */

	if ( 1 >= argc ) {

		SHOW_HELPTEXT;
		exit(0);
	}

	for ( i = 1; i < argc; i++ ) {

		if ( ( 0 == strcmp( "-i", argv[i] ) ) && ( i+1 < argc ) ) {

			inputFile= argv[i+1];
			++i;

		} else if ( ( 0 == strcmp( "-o", argv[i] ) ) && ( i+1 < argc ) ) {
		
			fcb.outputFile= strdup( argv[i+1] );
			
			++i;

		} else if ( ( 0 == strcmp( "-n", argv[i] ) ) && ( i+1 < argc ) ) {
		
			nstreams = atoi( argv[i+1] );
			++i;

		} else if ( ( 0 == strcmp( "-f", argv[i] ) ) && ( i+1 < argc ) ) {
		
			maxfilehandles = atoi( argv[i+1] );
			++i;

		} else if ( ( 0 == strcmp( "-b", argv[i] ) ) && ( i+1 < argc ) ) {
		
			buffersize = atoi( argv[i+1] );
			++i;

		} else if ( 0 == strcmp( "-io", argv[i] ) ) {

			fcb.ioonly= 1;

		} else if ( ( 0 == strcmp( "-z", argv[i] ) ) && ( i+1 < argc ) ) {
		
			compression= atoi( argv [i+1] );
			++i;

		} else if ( 0 == strcmp( "--help", argv[i] ) ||

			0 == strcmp( "-h", argv[i] ) ) {
			
			SHOW_HELPTEXT;
			exit(0);

		} else if ( 0 == strcmp( "-V", argv[i] ) ) {
		
			printf( "%u.%u.%u \"%s\"\n", OTF_VERSION_MAJOR, OTF_VERSION_MINOR,
				OTF_VERSION_SUB, OTF_VERSION_STRING);
			exit( 0 );

		} else {

			if ( '-' != argv[i][0] ) {

				inputFile= argv[i];

			} else {				

				fprintf( stderr, "ERROR: Unknown option '%s'\n", argv[i] );
				exit(1);
			}
		}
	}

	/* check parameters */

	if ( NULL == inputFile ) {
	
		printf( " no input file specified\n" );
		exit(1);
	}

	if ( NULL == outputFile ) {
	
		/*
		printf( " no output file specified\n" );
		exit(1);
		*/
		outputFile= strdup( DEFAULT_OUTFILE".otf" );
	}

	if ( maxfilehandles < 1 ) {
	
		printf( " there must be at least 1 available filehandle\n" );
		exit(1);
	}

	fcb.processes= NULL;
	fcb.processcount= 0;
	fcb.threadnums= 0;
	fcb.processgroups= NULL;
	fcb.processgroupcount= 0;
	fcb.reservedIds= NULL;
	fcb.reservedIdsc= 0;
	fcb.pghash= initHash();
	fcb.handleid= 0;
	
/* Open FileManager */
	if( 0 == fcb.ioonly ) {
		manager= OTF_FileManager_open( maxfilehandles );
		assert( NULL != manager );
		
		/* Open OTF Writer */
		fcb.outputFile= OTF_stripFilename( fcb.outputFile );
		fcb.writer = OTF_Writer_open( fcb.outputFile, nstreams, manager );
		OTF_Writer_setBufferSizes( fcb.writer, buffersize );
		OTF_Writer_setCompression( fcb.writer, compression );
	}

	/* Initialize VTF3. */
	(void) VTF3_InitTables ();

	/* Again, how many different record types do exist ? */
	nrectypes = VTF3_GetRecTypeArrayDim ();

	/* Allocate three auxiliary arrays for the record types,
	the record handler entry point pointers and some data. */
	recordtypes = (int *) malloc ((size_t) nrectypes * sizeof (int));

	handlers = (VTF3_handler_t *) malloc ((size_t) nrectypes *
		sizeof (VTF3_handler_t));

	firsthandlerargs = (void **) malloc ((size_t) nrectypes *
		sizeof (void *));

	/* Store the record types onto the appropriate array.
	Pay attention, the caller does not know their ordering scheme. */
	(void) VTF3_GetRecTypeArray (recordtypes);

	/* What follows, this is the final handler table setup. */
	if( 0 == fcb.ioonly ) {
	
		for (i = 0; i < nrectypes; i++)
		{
			if ( VTF3_RECTYPE_CLSTRREGVAL == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleClstrregval;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_COMMENT == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleComment;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_CPUREGVAL == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleCpuregval;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFACT == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefact;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFACT_OBSOL == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefact_obsol;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFCLKPERIOD == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefclkperiod;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFCLSTR == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefclstr;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFCLSTRREG == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefclstrreg;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFCLSTRREGCLASS == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefclstrregclass;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFCOMMUNICATOR == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefcommunicator;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFCPUGRP == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefcpugrp;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFCPUNAME == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefcpuname;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFCPUREG == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefcpureg;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFCPUREGCLASS == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefcpuregclass;
				firsthandlerargs[i] = &fcb;
				continue;
			}
	/*		if ( VTF3_RECTYPE_DEFCREATOR == recordtypes[i] )
			{
	*/			/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
	/*			handlers[i] = (VTF3_handler_t) handleDefcreator;
				firsthandlerargs[i] = &fcb;
				continue;
			}
	*/
			if ( VTF3_RECTYPE_DEFGLOBALOP == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefglobalop;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFIOFILE == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefiofile;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFKPARREG == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefkparreg;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFMSGNAME == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefmsgname;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFOPENMPNAME == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefopenmpname;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFOPENMPTYPE == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefopenmptype;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFPATTERN == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefpattern;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFPATTERNSHAPE == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefpatternshape;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFREDFUNC_OBSOL == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefredfunc_obsol;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFSAMP == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefsamp;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFSAMPCLASS == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefsampclass;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFSCL == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefscl;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFSCLFILE == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefsclfile;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFSTATE == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefstate;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFSTATE_OBSOL == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefstate_obsol;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFSYSCPUNAMES == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefsyscpunames;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFSYSCPUNUMS == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefsyscpunums;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFTHREADNUMS == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefthreadnums;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFTIMEOFFSET == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDeftimeoffset;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFUNMERGED == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefunmerged;
				firsthandlerargs[i] = &fcb;
				continue;
			}
	/*		if ( VTF3_RECTYPE_DEFVERSION == recordtypes[i] )
			{
	*/			/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
	/*			handlers[i] = (VTF3_handler_t) handleDefversion;
				firsthandlerargs[i] = writer;
				continue;
			}
	*/		if ( VTF3_RECTYPE_DOWNTO == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDownto;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_EXCHANGE == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleExchange;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_EXCHANGE_OBSOL == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleExchange_obsol;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_FILEIOBEGIN == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleFileiobegin;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_FILEIOEND == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleFileioend;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_GLOBALOP == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleGlobalop;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_KPARREGBARSUM == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleKparregbarsum;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_KPARREGBEGIN == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleKparregbegin;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_KPARREGEND == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleKparregend;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_MUTEXACQUIRE == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleMutexacquire;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_MUTEXRELEASE == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleMutexrelease;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_OPENMPENTER == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleOpenmpenter;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_OPENMPLEAVE == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleOpenmpleave;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_PATTERN == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handlePattern;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_RECVMSG == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleRecvmsg;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_SAMP == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleSamp;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_SENDMSG == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleSendmsg;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_SRCINFO_OBSOL == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleSrcinfo_obsol;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_UNRECOGNIZABLE == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleUnrecognizable;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_UPFROM == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleUpfrom;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_UPTO == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleUpto;
				firsthandlerargs[i] = &fcb;
				continue;
			}
		}
		
	} else {

		/* 1 == fcb.ioonly */

		for (i = 0; i < nrectypes; i++)
		{
			if ( VTF3_RECTYPE_DEFSYSCPUNUMS == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefsyscpunums;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_DEFTHREADNUMS == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleDefthreadnums;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_FILEIOBEGIN == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleFileiobegin;
				firsthandlerargs[i] = &fcb;
				continue;
			}
			if ( VTF3_RECTYPE_FILEIOEND == recordtypes[i] )
			{
				/* Replace the predefined copy handler by our own one,
				do not forget to redirect the first argument, too. */
				handlers[i] = (VTF3_handler_t) handleFileioend;
				firsthandlerargs[i] = &fcb;
				continue;
			}
		}

	}

	/* Open the input device */
	fcbin = VTF3_OpenFileInput ( inputFile, handlers, firsthandlerargs, 0);

	/* Free the auxiliary arrays. */
	(void) free (firsthandlerargs);
	(void) free (handlers);
	(void) free (recordtypes);

	if ( NULL == fcbin ) {

		fprintf( stderr, "cannot open input file '%s'\n", inputFile );
		exit( 1 );
	}

	/* Now push the operation to portion-wise process
	the input file contents. */
	
	if( 0 == fcb.ioonly ) {
	
		OTF_Writer_writeDefCreator( fcb.writer, 0, VTF32OTFCREATOR );
		
		/* generate a standardfile group - vampir needs a filegroup(communicator)
		group 0 would be invalid */
		
		OTF_Writer_writeDefFileGroup( fcb.writer, 0, 1, "Default" );
	}
	
	do
	{
		bytesread = VTF3_ReadFileInputLtdBytes (fcbin, 50000);
	}
	while (bytesread != 0);
	
	if ( NULL == fcb.processes ) {
	
			fprintf( stderr, "ERROR: Missing NCPU record\n" );
			exit(1);
	}
	
	/* create defprocess records with thread hierarchie */
	if( 0 == fcb.ioonly ) {
		for( i = 0; i < fcb.processcount; i++ ) {
	
			OTF_Writer_writeDefProcess( fcb.writer, 0, i + 1,
				fcb.processes[i][0].name, 0);
	
			for( a = 1; a < fcb.threadnums[i]; ++a ) {
	
				OTF_Writer_writeDefProcess( fcb.writer, 0,
					(i + 1) + (a << 16), fcb.processes[i][a].name, i + 1);
			}
		}
	}

	/* create defprocessgroup records */
	for( i= 0;i < (int)fcb.processgroupcount; ++i ) {

		if( 0 == fcb.ioonly ) {
			OTF_Writer_writeDefProcessGroup( fcb.writer,
				0 /* uint32_t stream */,
				fcb.processgroups[i].id /* uint32_t procGroup */,
				fcb.processgroups[i].name /* const char* name */,
				fcb.processgroups[i].size /* uint32_t numberOfProcs */,
				fcb.processgroups[i].procs /* const uint32_t* procs */ );
		}

		free( fcb.processgroups[i].procs );
		free( fcb.processgroups[i].name );
	}
	
	free( fcb.processgroups );


		
	/* free process-array */
	for( i = 0; i < fcb.processcount; ++i ) {
		
		for( a = 0; a < fcb.threadnums[i]; ++a ) {

			writeFileIOBuffer( i + (a<<16)/*cpuid*/, &fcb.processes[i][a], fcb.outputFile );

			/* delete iofiles */
			if( 0 == fcb.ioonly ) {

				sprintf( iofile, "%s.%i.io", fcb.outputFile, i + (a<<16) );

				unlink( iofile );
			}
		
			Stack_delete( fcb.processes[i][a].stack );
			
			if ( 0 != fcb.processes[i][a].name ) {
			
				free( fcb.processes[i][a].name );
			}
		}
		
		free( fcb.processes[i] );
	}
	
	free( fcb.processes );
	
	/* Close all devices. */
	(void) VTF3_Close (fcbin);

	if( 0 == fcb.ioonly ) {
		OTF_Writer_close( fcb.writer );
		OTF_FileManager_close( manager );
	}
	
	closeHash( fcb.pghash );
	
	free( fcb.outputFile );


	return 0;
}
