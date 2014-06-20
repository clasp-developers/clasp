/*
 This test reads a trace file and shows the following after each reading step:
   - total read records
   - read duration
   - total read duration
   - byte progress
   - time progress

 Usage:
   progress_otf <inputfile> [#filemanagers] [#recordlimit]

 Default options:
   #filemanagers = 32
   #recordlimit  = 10000
 */

#include <assert.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>

#include "otf.h"


#define SHOW_HELPTEXT { \
	int l = 0; while( Helptext[l] ) { printf( "%s", Helptext[l++] ); } }

static const char* Helptext[] = {
"                                                                  \n",
" otfprogress - shwo reading progress of OTF for testing purposes. \n",
"                                                                  \n",
" otfprogress [Options] <input file name>                          \n",
"                                                                  \n",
"   options:                                                       \n",
"      -h, --help    show this help message                        \n",
"      -V            show OTF version                              \n",
"      -f <n>        set max number of filehandles available       \n",
"      -rb <size>    set buffersize of the reader                  \n",
"      -rl <size>    record limit per read call                    \n",
"                                                                  \n",
"                                                                  \n", NULL };


static uint64_t get_time( void );

int main( int argc, char** argv )
{
	OTF_FileManager* manager;
	OTF_Reader* reader;
	OTF_HandlerArray* handlers;

	char*    input_file = "";
	uint32_t nfiles = 32;
	uint64_t record_limit = 10000;
	uint64_t readerbuffersize = 1024 * 1024;
	int i;

	if ( 1 >= argc ) {

			SHOW_HELPTEXT;
			return 0;
	}

	for ( i = 1; i < argc; i++ ) {


		if ( ( 0 == strcmp( "-i", argv[i] ) ) && ( i+1 < argc ) ) {

			input_file= argv[i+1];
			++i;

		} else if ( ( 0 == strcmp( "-rb", argv[i] ) ) && ( i+1 < argc ) ) {

			readerbuffersize = atoi( argv[i+1] );
			++i;

		} else if ( ( 0 == strcmp( "-rl", argv[i] ) ) && ( i+1 < argc ) ) {

			record_limit = atoi( argv[i+1] );
			++i;

		} else if ( ( 0 == strcmp( "-f", argv[i] ) ) && ( i+1 < argc ) ) {

			nfiles = atoi( argv[i+1] );
			++i;

		} else if ( 0 == strcmp( "--help", argv[i] ) || 
				0 == strcmp( "-h", argv[i] ) ) {

			SHOW_HELPTEXT;
			return 0;

		} else if ( 0 == strcmp( "-V", argv[i] ) ) {
		
			printf( "%u.%u.%u \"%s\"\n", OTF_VERSION_MAJOR, OTF_VERSION_MINOR,
				OTF_VERSION_SUB, OTF_VERSION_STRING);
			exit( 0 );

		} else {

			if ( '-' != argv[i][0] ) {

				input_file= argv[i];

			} else{

				fprintf( stderr, "ERROR: Unknown option: '%s'\n", argv[i] );
				exit(1);
			}
		}
	}


  manager = OTF_FileManager_open( nfiles );
  assert( manager );

  handlers = OTF_HandlerArray_open();
  assert( handlers );

  reader = OTF_Reader_open( input_file, manager );
  assert( reader );

  OTF_Reader_setBufferSizes( reader, readerbuffersize );

  OTF_Reader_setRecordLimit( reader, record_limit );

  /* OTF_HandlerArray_setHandler(...) */

  printf( "input trace file :    %s\n", argv[1] );
  printf( "file num:             %u\n", nfiles );
  printf( "read buffer sizes:    %llu\n", (unsigned long long int)readerbuffersize );
  printf( "max. record limit:    %lli\n\n", (long long int) record_limit );

  printf( "   tot. read    read           tot. read      progress (%%)\n" );
  printf( "   records      duration (s)   duration (s)   byte     time\n" );

  while( 1 )
  {
    uint64_t minb = 0;
    uint64_t curb = 0;
    uint64_t maxb = 0;
    uint64_t mint = 0;
    uint64_t curt = 0;
    uint64_t maxt = 0;
    uint64_t read_rc;
    uint8_t  ret;

    uint64_t read_start;

    static uint64_t read_records_tot = 0;
    static uint64_t read_duration_tot = 0;
    uint64_t        read_duration;
    double          progress_bytes= 0.0;
    double          progress_times= 0.0;

    read_start = get_time();
    read_rc = OTF_Reader_readEvents( reader, handlers );
    read_duration = get_time() - read_start;

    if ( read_rc == 0 || read_rc == OTF_READ_ERROR )
      break;

    read_records_tot += read_rc;
    read_duration_tot += read_duration;

    ret= OTF_Reader_eventBytesProgress( reader, &minb, &curb, &maxb );
    if( ret == 1 && minb < curb && curb <= maxb ) {

        progress_bytes= ( (double)( curb - minb ) / (double)( maxb - minb ) ) * 100.0;
    }

    ret= OTF_Reader_eventTimeProgress( reader, &mint, &curt, &maxt );
    if( ret == 1 && mint < curt && curt <= maxt ) {

        progress_times= ( (double)( curt - mint ) / (double)( maxt - mint ) ) * 100.0;
    }

    printf( "%12lli%16.5f%15.5f%9.2f%9.2f\t\tb: %llu <= %llu <= %llu, \t\t t: %llu <= %llu <= %llu \n",
            (long long int)read_records_tot, (double)( read_duration / 1e6 ),
            (double)(read_duration_tot / 1e6),
            progress_bytes, progress_times,
            (unsigned long long int)minb, (unsigned long long int)curb,
            (unsigned long long int)maxb, (unsigned long long int)mint,
            (unsigned long long int)curt, (unsigned long long int)maxt );
  }

  OTF_Reader_close( reader );

  OTF_HandlerArray_close( handlers );

  OTF_FileManager_close( manager );

  return 0;
}

static uint64_t get_time()
{
  struct timeval tv;

  gettimeofday( &tv, NULL );

  return (uint64_t)(tv.tv_sec * 1e6 + tv.tv_usec);
}
