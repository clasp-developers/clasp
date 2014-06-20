/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Johannes Spazier
*/

#include <stdio.h>

#include "handler.h"
#include "otfaux.h"

#define fprintf_root \
    if( my_rank == 0 ) fprintf

#define FINISH_EVERYTHING(ret) \
    finish_everything( infile, outfile, info, &rank_data, ret )

#define SHOW_HELPTEXT if( my_rank == 0 ) { \
    int l = 0; \
    while( helptext[l] ) { fprintf( stdout, "%s", helptext[l++] ); } \
}

/* name of program executable */
#ifdef OTFMERGE_MPI
#   define EXENAME "otfmerge-mpi"
#else /* OTFMERGE_MPI */
#   define EXENAME "otfmerge"
#endif /* OTFMERGE_MPI */


static const char* helptext[] = {
"                                                                           \n",
" "EXENAME" - Change the number of streams for an existing OTF trace.       \n",
"                                                                           \n",
" Syntax: "EXENAME" [options] <input file name>                             \n",
"                                                                           \n",
"   options:                                                                \n",
"      -h, --help    show this help message                                 \n",
"      -V            show OTF version                                       \n",
"      -p            show progress                                          \n",
"      -n <n>        set number of streams for output                       \n",
"                    set this to 0 for using one stream per process         \n",
"                    (default: 1)                                           \n",
"      -f <n>        max. number of filehandles available per rank          \n",
"      -o <name>     namestub of the output file                            \n",
"                    (default: out)                                         \n",
"      -rb <size>    set buffersize of the reader (for each rank)           \n",
"      -wb <size>    set buffersize of the writer (for each rank)           \n",
"      -z <zlevel>   write compressed output                                \n",
"                    zlevel reaches from 0 to 9 where 0 is no               \n",
"                    compression and 9 is the highest level                 \n",
"      --stats       cover statistics too                                   \n",
"      --snaps       cover snapshots too                                    \n",
"      --long        write long OTF format                                  \n",
"                                                                           \n",
NULL };


int main(int argc, char **argv) {

    /* for all processes */
    int i, j;
    int my_rank = 0;
    int num_ranks = 1;
    uint64_t ret_read;
    int show_progress = 0;
    int max_fhandles = 100;
    char *outfile = NULL;
    char *infile = NULL;
    int rbufsize = 1024 * 1024;
    int wbufsize = 1024 * 1024;
    int format = OTF_WSTREAM_FORMAT_SHORT | OTF_WSTREAM_FORMAT_INLINE_SNAPSHOTS;
    int read_stats = 0;
    int read_snaps = 0;
    OTF_FileCompression compression= 0;
    RankData rank_data = { 0 ,NULL };
    ProgressInfo *info = NULL;
    GlobalData global_data;

    /* only for root process (0) */
    int num_cpus; /* number of cpus in input otf-file */
    int *cpus;    /* global array that contains all cpu-ids */
    int offset;
    int *p;
    int num_ostreams = 1;
    char *outfile_otf = NULL;
    FILE *master_file = NULL;
    OutStream *ostreams = NULL;

    /* progress related */
    uint64_t total_bytes = 0;
    uint64_t cur_bytes = 0;
    uint64_t cur_bytes_ges = 0;
    uint64_t min, max, cur;
    struct timeval tv;

    /* OTF related */
    OTF_Reader* reader = NULL;
    OTF_WStream* wstream = NULL;
    OTF_HandlerArray* handlers = NULL;
    OTF_MasterControl* master = NULL;
    OTF_FileManager* read_manager = NULL;
    OTF_FileManager* write_manager = NULL;
    OTF_MapEntry* entry = NULL;

#ifdef OTFMERGE_MPI
    /* MPI related */
    MPI_Status status;

    int array_of_blocklengths[2];
    MPI_Aint array_of_displacements[2];
    MPI_Datatype array_of_types[2];

    MPI_Aint first_var_address;
    MPI_Aint second_var_address;


    /* start MPI */
    MPI_Init( &argc, &argv );

    MPI_Comm_rank( MPI_COMM_WORLD, &my_rank );
    MPI_Comm_size( MPI_COMM_WORLD, &num_ranks );
#endif /* OTFMERGE_MPI */

    /* store some important things in a global structure */
    global_data.my_rank = my_rank;
    global_data.num_ranks = num_ranks;
    global_data.ranks_alive = num_ranks - 1;

    /* argument handling */
    if( 1 >= argc ) {

        SHOW_HELPTEXT

#ifdef OTFMERGE_MPI
        MPI_Finalize();
#endif /* OTFMERGE_MPI */

        return 0;
    }

    for ( i = 1; i < argc; i++ ) {

        if( ( 0 == strcmp( "-o", argv[i] ) ) && ( i+1 < argc ) ) {

            /* must be free'd at the end */
            outfile = OTF_stripFilename( argv[i+1] );
            ++i;

        } else if( ( 0 == strcmp( "-n", argv[i] ) ) && ( i+1 < argc ) ) {

            num_ostreams = atoi( argv[i+1] );
            ++i;

        } else if( 0 == strcmp( "-h", argv[i] ) ||
                   0 == strcmp( "--help", argv[i] ) ) {

            SHOW_HELPTEXT

            return FINISH_EVERYTHING(0);

        } else if( 0 == strcmp( "-V", argv[i] ) ) {

            fprintf_root( stdout, "%u.%u.%u \"%s\"\n",
                          OTF_VERSION_MAJOR, OTF_VERSION_MINOR,
                          OTF_VERSION_SUB, OTF_VERSION_STRING );

            return FINISH_EVERYTHING(0);

        } else if( 0 == strcmp( "-p", argv[i] ) ) {

            show_progress = 1;

        } else if( ( 0 == strcmp( "-f", argv[i] ) ) && ( i+1 < argc ) ) {

            max_fhandles = atoi( argv[i+1] );
            ++i;

        } else if( ( 0 == strcmp( "-rb", argv[i] ) ) && ( i+1 < argc ) ) {

            rbufsize = atoi( argv[i+1] );
            ++i;

        } else if( ( 0 == strcmp( "-wb", argv[i] ) ) && ( i+1 < argc ) ) {

            wbufsize = atoi( argv[i+1] );
            ++i;

        } else if( ( 0 == strcmp( "-z", argv[i] ) ) && ( i+1 < argc ) ) {

            compression = atoi( argv[i+1] );
            ++i;

        } else if( 0 == strcmp( "--long", argv[i] ) ) {

            format &= ~1;
            format |= OTF_WSTREAM_FORMAT_LONG;

        } else if( 0 == strcmp( "--snaps", argv[i] ) ) {

            read_snaps = 1;

        } else if( 0 == strcmp( "--stats", argv[i] ) ) {

            read_stats = 1;

        } else {

            if( '-' != argv[i][0] ) {

                /* must be free'd at the end */
                infile = OTF_stripFilename( argv[i] );

            } else {

                fprintf_root( stderr, "Error: unknown option: '%s'\n",
                              argv[i] );

                return FINISH_EVERYTHING(1);

            }

        }

    }

    if( ! outfile ) {

        outfile = strdup("out");

    } else if( outfile[ strlen(outfile) -1 ] == '/' ) {

        strncat( outfile, "out", 3 );

    }

    /* must be free'd at the end */
    outfile_otf = OTF_getFilename( outfile, 0, OTF_FILETYPE_MASTER, 0, NULL);

    /* check for neccessary options */
    if( infile == NULL ) {

        fprintf_root( stderr, "Error: no input file given.\n");

        return FINISH_EVERYTHING(1);

    }

    if( max_fhandles < 1 ) {

       fprintf_root( stderr,
                     "Error: less than 1 filehandle is not permitted.\n" );

       return FINISH_EVERYTHING(1);

    }

    if( num_ostreams < 0 ) {

        fprintf_root( stderr,
                      "Error: the number of streams must not be negative.\n" );

        return FINISH_EVERYTHING(1);

    }

    if( wbufsize < 0 || rbufsize < 0 ) {

        fprintf_root( stderr,
                      "Error: buffersize must be greater or equal 0.\n" );

        return FINISH_EVERYTHING(1);

    }

    if( my_rank == 0 ) {

        /* read master of input file */
        read_manager = OTF_FileManager_open( max_fhandles );

        if( NULL == read_manager ) {

            fprintf( stderr, "Error: unable to initialize file manager.\n" );

            return FINISH_EVERYTHING(1);

        }

        master = OTF_MasterControl_new( read_manager );
        OTF_MasterControl_read( master, infile );

        /* get the total number of processes in the otf master file */
        num_cpus = OTF_MasterControl_getrCount( master );

        /* set one stream per process */
        if( num_ostreams == 0 || num_ostreams > num_cpus ) {

            num_ostreams = num_cpus; 

        }

        /* allocate memory */
        ostreams = (OutStream*) malloc( num_ostreams * sizeof(OutStream) );
        cpus = (int*) malloc( num_cpus * sizeof(int) );
        p = cpus;

        /* allocate memory for the info array */
        info = (ProgressInfo*) malloc( num_ranks * sizeof(ProgressInfo) );

        /* fill the global cpus-array */
        i = 0;
        while( 1 ) {

            entry = OTF_MasterControl_getEntryByIndex( master, i );

            if( entry == NULL ) {

                break;
            }

            for( j = 0; (uint32_t)j < entry->n; j++ ) {

                *p++ = entry->values[j];

            }

            i++;

        }

        /* open new master file for output */
        master_file = fopen( outfile_otf, "w");

        if( NULL == master_file ) {

            fprintf( stderr, "Error: unable to open file \"%s\".\n",
                     outfile_otf);

            free( cpus );
            free( ostreams );

            OTF_MasterControl_close( master );
            OTF_FileManager_close( read_manager );

            return FINISH_EVERYTHING(1);

        }

        /* fill all ostreams with data and write the new master file */
        offset = 0;
        for( i = 0; i < num_ostreams; i++ ) {

            ostreams[i].id = i + 1;
            ostreams[i].num_cpus = ( num_cpus / num_ostreams ) +
                                   ( i < ( num_cpus % num_ostreams) ? 1 : 0 );

            ostreams[i].cpus =
                (int*) malloc( ostreams[i].num_cpus * sizeof(int) );

            /* append stream-id to new otf master file */
            fprintf( master_file, "%x:", ostreams[i].id);

            for( j = 0; j < ostreams[i].num_cpus; j++ ) {

                ostreams[i].cpus[j] = cpus[offset + j];

                /* append cpu-id to master file */
                fprintf( master_file, "%x", ostreams[i].cpus[j]);

                if( (j + 1) < ostreams[i].num_cpus ) {

                    fprintf( master_file, ",");

                }

            }

            fprintf( master_file, "\n" );
            offset += ostreams[i].num_cpus;

        }

        /* close new master file */
        fclose( master_file );

        /* free global cpus-array, because it is not needed anymore */
        if( cpus ) {

            free(cpus);
            cpus = NULL;

        }

        offset = 0;
        /* send needed data to all ranks */
        for( i = (num_ranks - 1); i >= 0; i-- ) {

            /* get number of output-streams, rank i has to handle */
            rank_data.num_ostreams =
                ( num_ostreams / num_ranks) +
                ( i < ( num_ostreams % num_ranks) ? 1 : 0 );

#ifdef OTFMERGE_MPI
            if( i > 0) {

                /* send number of output-streams to rank i */
                MPI_Ssend( &(rank_data.num_ostreams), 1, MPI_INT, i, 0,
                           MPI_COMM_WORLD);

            } else
#endif /* OTFMERGE_MPI */
            {

                /* save number of output-streams for rank 0 in rank_data */
                rank_data.ostreams =
                    (OutStream*) malloc( rank_data.num_ostreams *
                        sizeof(OutStream) );

            }

            info[i].num_cpus = 0;
            /* go through all output-streams of rank i */
            for( j = 0; j < rank_data.num_ostreams; j++ ) {

#ifdef OTFMERGE_MPI
                if( i > 0 ) {

                    /* send data to rank */
                    MPI_Ssend( &(ostreams[offset + j].id), 1, MPI_INT, i, 0,
                               MPI_COMM_WORLD);
                    MPI_Ssend( &(ostreams[offset + j].num_cpus), 1, MPI_INT, i, 0,
                               MPI_COMM_WORLD);
                    MPI_Ssend( ostreams[offset + j].cpus,
                               ostreams[offset + j].num_cpus, MPI_INT, i, 0,
                               MPI_COMM_WORLD);

                } else
#endif /* OTFMERGE_MPI */
                {

                    /* save data for rank 0 */
                    rank_data.ostreams[j].id = ostreams[offset + j].id;
                    rank_data.ostreams[j].num_cpus =
                        ostreams[offset + j].num_cpus;
                    rank_data.ostreams[j].cpus =
                        (int*) malloc( rank_data.ostreams[j].num_cpus *
                            sizeof(int));
                    memcpy(rank_data.ostreams[j].cpus, ostreams[offset + j].cpus,
                    rank_data.ostreams[j].num_cpus * (sizeof(int)));

                }

                info[i].num_cpus += ostreams[offset + j].num_cpus;

            }

            offset += rank_data.num_ostreams;

        }

        /* can be free'd here because all MPI_Ssends are finished --> they are synchron */
        for( i = 0; i < num_ostreams; i++ ) {

            if( ostreams[i].cpus ) {

                free( ostreams[i].cpus );
                ostreams[i].cpus = NULL;

            }

        }

        if( ostreams ) {

            free( ostreams );
            ostreams = NULL;

        }

        /* initialize the info array */
        for( i = 0; i < num_ranks; i++ ) {

            info[i].percent = (double) info[i].num_cpus / (double) num_cpus;
#ifdef OTFMERGE_MPI
            info[i].request = MPI_REQUEST_NULL;
#endif /* OTFMERGE_MPI */
            info[i].value.progress = 0.0;
            info[i].value.is_alive = 1;

        }

        /* close master */
        OTF_MasterControl_close( master );
        OTF_FileManager_close( read_manager );

    }
#ifdef OTFMERGE_MPI
    else { /* my_rank != 0 */

        info = (ProgressInfo*) malloc( 1 * sizeof(ProgressInfo) );

        info[0].request = MPI_REQUEST_NULL;
        info[0].value.progress = 0.0;
        info[0].value.is_alive = 1;

        /* receive number of output-streams for this rank */
        MPI_Recv( &(rank_data.num_ostreams), 1, MPI_INT, 0, 0, MPI_COMM_WORLD,
                  &status);

        /* allocate memory for output-streams */
        rank_data.ostreams =
            (OutStream*) malloc( rank_data.num_ostreams * sizeof(OutStream) );

        /* go through all output streams */
        for( i = 0; i < rank_data.num_ostreams; i++ ) {

            /* receive id of output-stream and number of cpus in this stream */
            MPI_Recv( &(rank_data.ostreams[i].id), 1, MPI_INT, 0, 0,
                      MPI_COMM_WORLD, &status);
            MPI_Recv( &(rank_data.ostreams[i].num_cpus), 1, MPI_INT, 0, 0,
                      MPI_COMM_WORLD, &status);

            /* allocate memory for cpus in stream */
            rank_data.ostreams[i].cpus =
                (int*) malloc( rank_data.ostreams[i].num_cpus * sizeof(int) );

            /* receive all cpu-ids */
            MPI_Recv( rank_data.ostreams[i].cpus,
                      rank_data.ostreams[i].num_cpus, MPI_INT, 0, 0,
                      MPI_COMM_WORLD, &status );

        }

    }

    if( show_progress ) {

        /* create new mpi datatype to transfer the progress */
        /* struct {
            double progress;
            uint8_t is_alive;
        };
        */

        array_of_blocklengths[0] = 1;
        array_of_blocklengths[1] = 1;

        MPI_Address( &(info[0].value.progress), &first_var_address );
        MPI_Address( &(info[0].value.is_alive), &second_var_address );

        array_of_displacements[0] = (MPI_Aint) 0;
        array_of_displacements[1] = second_var_address - first_var_address;

        array_of_types[0] = MPI_DOUBLE;
        array_of_types[1] = MPI_BYTE;

        MPI_Type_struct( 2, array_of_blocklengths, array_of_displacements,
                         array_of_types, &(global_data.buftype) );

        MPI_Type_commit( &(global_data.buftype) );

    }
#endif /* OTFMERGE_MPI */

    write_manager = OTF_FileManager_open( max_fhandles );
    if( NULL == write_manager ) {

        fprintf( stderr, "Error: unable to initialize write file manager.\n" );

        return FINISH_EVERYTHING(1);

    }

    /* the root process should read the definitions and markers now */
    if( my_rank == 0 ) {
	read_manager = OTF_FileManager_open( max_fhandles );
	if( NULL == read_manager ) {

	    fprintf( stderr, "Error: unable to initialize read file manager.\n" );

	    return FINISH_EVERYTHING(1);

	}

        wstream = OTF_WStream_open( outfile, 0, write_manager );

        OTF_WStream_setBufferSizes( wstream, wbufsize );
        OTF_WStream_setCompression( wstream, compression );
        OTF_WStream_setFormat( wstream, format );

        handlers = OTF_HandlerArray_open();

        setDefinitionHandlerArray( handlers, wstream );

        reader = OTF_Reader_open( infile, read_manager);

        if( reader == NULL) {

            fprintf( stderr, "Error: unable to open file %s.\n", infile );

            OTF_HandlerArray_close( handlers );
            OTF_WStream_close( wstream );
            OTF_FileManager_close( read_manager );
            OTF_FileManager_close( write_manager );

            return FINISH_EVERYTHING(1);
        }

        OTF_Reader_setBufferSizes( reader, rbufsize );

        if( OTF_READ_ERROR ==
            OTF_Reader_readDefinitions( reader, handlers ) ) {

            fprintf( stderr, "Error: while reading definitions from file %s\n",
                     infile );

            OTF_Reader_close( reader );
            OTF_HandlerArray_close( handlers );
            OTF_WStream_close( wstream );
            OTF_FileManager_close( read_manager );
            OTF_FileManager_close( write_manager );

            return FINISH_EVERYTHING(1);
        }

        if( OTF_READ_ERROR == OTF_Reader_readMarkers( reader, handlers ) ) {

            fprintf( stderr, "Error: while reading markers from file %s\n", infile );

            OTF_Reader_close( reader );
            OTF_HandlerArray_close( handlers );
            OTF_WStream_close( wstream );
            OTF_FileManager_close( read_manager );
            OTF_FileManager_close( write_manager );

            return FINISH_EVERYTHING(1);
        }

        /* close everything */
        OTF_HandlerArray_close( handlers );
        OTF_Reader_close( reader );
        OTF_WStream_close( wstream );
        OTF_FileManager_close( read_manager );
    }

    read_manager = OTF_FileManager_open( max_fhandles );
    if( NULL == read_manager ) {

        fprintf( stderr, "Error: unable to initialize read file manager.\n" );

        return FINISH_EVERYTHING(1);

    }


    for( i = 0; i < rank_data.num_ostreams; i++ ) {

        total_bytes = 0;
        cur_bytes = 0;
        cur_bytes_ges = 0;

        wstream =
            OTF_WStream_open( outfile, rank_data.ostreams[i].id, write_manager );

        OTF_WStream_setBufferSizes( wstream, wbufsize );
        OTF_WStream_setCompression( wstream, compression );
        OTF_WStream_setFormat( wstream, format );

        handlers = OTF_HandlerArray_open();

        setEventHandlerArray( handlers, wstream );

        reader = OTF_Reader_open( infile, read_manager);
        if( reader == NULL) {

            fprintf_root( stderr, "Error: unable to open file %s.\n", infile );

            OTF_HandlerArray_close( handlers );
            OTF_WStream_close( wstream );
            OTF_FileManager_close( read_manager );
            OTF_FileManager_close( write_manager );

            return FINISH_EVERYTHING(1);
        }

        OTF_Reader_setBufferSizes( reader, rbufsize );

        OTF_Reader_setProcessStatusAll ( reader, 0 );

        for( j = 0; j < rank_data.ostreams[i].num_cpus; j++ ) {

            OTF_Reader_setProcessStatus( reader, rank_data.ostreams[i].cpus[j],
                                         1 );

        }

        if( show_progress ) {

            OTF_Reader_setRecordLimit( reader, 0 );

            if( OTF_READ_ERROR == OTF_Reader_readEvents( reader, handlers ) ) {

                fprintf( stderr, "Error: while reading events from file %s\n",
                         infile );

                OTF_Reader_close( reader );
                OTF_HandlerArray_close( handlers );
                OTF_WStream_close( wstream );
                OTF_FileManager_close( read_manager );
                OTF_FileManager_close( write_manager );

                return FINISH_EVERYTHING(1);

            }

            if( read_snaps ) {

                if( OTF_READ_ERROR ==
                    OTF_Reader_readSnapshots( reader, handlers ) ) {

                    fprintf( stderr,
                             "Error: while reading snaphots from file %s\n",
                             infile );

                    OTF_Reader_close( reader );
                    OTF_HandlerArray_close( handlers );
                    OTF_WStream_close( wstream );
                    OTF_FileManager_close( read_manager );
                    OTF_FileManager_close( write_manager );

                    return FINISH_EVERYTHING(1);

                }

            }

            if( read_stats ) {

                if( OTF_READ_ERROR ==
                    OTF_Reader_readStatistics( reader, handlers ) ) {

                    fprintf( stderr,
                             "Error: while reading statistics from file %s\n",
                             infile );

                    OTF_Reader_close( reader );
                    OTF_HandlerArray_close( handlers );
                    OTF_WStream_close( wstream );
                    OTF_FileManager_close( read_manager );
                    OTF_FileManager_close( write_manager );

                    return FINISH_EVERYTHING(1);

                }

            }

            OTF_Reader_eventBytesProgress( reader, &min, &cur, &max );
            /* (min - max) is erroneous because with small traces min == max
            --> division by zero */
            total_bytes += max; /* max - min */

            if( read_snaps ) {

                OTF_Reader_snapshotBytesProgress( reader, &min, &cur, &max );
                total_bytes += max; /* max - min */

            }

            if( read_stats ) {

                OTF_Reader_statisticBytesProgress( reader, &min, &cur, &max );
                total_bytes += max; /* max - min */

            }

            OTF_Reader_setRecordLimit( reader, 100000 );

        }

        while( 0 != ( ret_read = OTF_Reader_readEvents( reader, handlers ) ) ) {

            if( ret_read == OTF_READ_ERROR) {

                fprintf( stderr, "Error: while reading events from file %s\n",
                         infile );

                OTF_Reader_close( reader );
                OTF_HandlerArray_close( handlers );
                OTF_WStream_close( wstream );
                OTF_FileManager_close( read_manager );
                OTF_FileManager_close( write_manager );

                return FINISH_EVERYTHING(1);

            }

            if( show_progress ) {

                OTF_Reader_eventBytesProgress( reader, &min, &cur, &max );

                cur_bytes = cur; /* cur - min */

                /* calculate rank specific progress for the current stream */
                global_data.tmp_progress =
                    (double) ( (double) cur_bytes / (double) total_bytes );

                update_progress( info, &global_data, i, rank_data.num_ostreams );

            }

        }

        cur_bytes_ges = cur_bytes;

        /* read snapshots */
        if( read_snaps ) {
            char* thumbnail_filename;

            /* keep external snapshots external */
            format &= ~2;
            OTF_WStream_setFormat( wstream, format );

            while( 0 != ( ret_read =
                          OTF_Reader_readSnapshots( reader, handlers ) ) ) {

                if( ret_read == OTF_READ_ERROR) {

                    fprintf( stderr,
                             "Error: while reading snapshots from file %s\n",
                             infile );

                    OTF_Reader_close( reader );
                    OTF_HandlerArray_close( handlers );
                    OTF_WStream_close( wstream );
                    OTF_FileManager_close( read_manager );
                    OTF_FileManager_close( write_manager );

                    return FINISH_EVERYTHING(1);

                }

                if( show_progress ) {

                    OTF_Reader_snapshotBytesProgress( reader, &min, &cur, &max );

                    cur_bytes = cur; /* cur - min */

                    /* calculate rank specific progress for the
                    current stream */
                    global_data.tmp_progress =
                        (double) ( (double) (cur_bytes + cur_bytes_ges) /
                        (double) total_bytes );

                    update_progress( info, &global_data, i, rank_data.num_ostreams );
                }

            }

            thumbnail_filename = OTFAUX_Thumbnail_getFilename( infile );
            if ( OTF_fileExists( thumbnail_filename ) && my_rank == 0 ) {
                char* thumbnail_outname;
                size_t bytes_read;
                char buffer[ 1024 ];
                FILE* instream;
                FILE* outstream;

                thumbnail_outname = OTFAUX_Thumbnail_getFilename( outfile );
                if ( !thumbnail_outname ) {
                    fprintf( stderr, "Error: out of memory.\n");

                    free( thumbnail_filename );
                    OTF_Reader_close( reader );
                    OTF_HandlerArray_close( handlers );
                    OTF_WStream_close( wstream );
                    OTF_FileManager_close( read_manager );
                    OTF_FileManager_close( write_manager );

                    return FINISH_EVERYTHING(1);
                }

                /* open files */
                instream = fopen( thumbnail_filename, "rb" );
                if ( !instream ) {
                    fprintf( stderr, "Error: Can't open existing thumbnail file for reading.\n");

                    free( thumbnail_filename );
                    free( thumbnail_outname );
                    OTF_Reader_close( reader );
                    OTF_HandlerArray_close( handlers );
                    OTF_WStream_close( wstream );
                    OTF_FileManager_close( read_manager );
                    OTF_FileManager_close( write_manager );

                    return FINISH_EVERYTHING(1);
                }

                outstream = fopen( thumbnail_outname, "wb" );
                if ( !outstream ) {
                    fprintf( stderr, "Error: Can't open thumbnail file for writing.\n");

                    fclose( instream );
                    free( thumbnail_outname );
                    OTF_Reader_close( reader );
                    OTF_HandlerArray_close( handlers );
                    OTF_WStream_close( wstream );
                    OTF_FileManager_close( read_manager );
                    OTF_FileManager_close( write_manager );

                    return FINISH_EVERYTHING(1);
                }
                free( thumbnail_outname );

                /* copy file */
                while ( ( bytes_read = fread( buffer, 1, sizeof( 1024 ), instream ) ) ) {

                    if ( bytes_read > fwrite( buffer, 1, bytes_read, outstream ) ) {

                        fprintf( stderr, "Error: Can't write thumbnail file.\n");

                        fclose( instream );
                        fclose( outstream );
                        OTF_Reader_close( reader );
                        OTF_HandlerArray_close( handlers );
                        OTF_WStream_close( wstream );
                        OTF_FileManager_close( read_manager );
                        OTF_FileManager_close( write_manager );

                        return FINISH_EVERYTHING(1);
                    }
                }

                /* close files */
                fclose( instream );
                fclose( outstream );

            }
            free( thumbnail_filename );

        }

        cur_bytes_ges += cur_bytes;

        /* read statistics */
        if( read_stats ) {

            while( 0 != ( ret_read =
                          OTF_Reader_readStatistics( reader, handlers ) ) ) {

                if( ret_read == OTF_READ_ERROR) {

                    fprintf( stderr,
                             "Error: while reading statistics from file %s\n",
                             infile );

                    OTF_Reader_close( reader );
                    OTF_HandlerArray_close( handlers );
                    OTF_WStream_close( wstream );
                    OTF_FileManager_close( read_manager );
                    OTF_FileManager_close( write_manager );

                    return FINISH_EVERYTHING(1);

                }

                if( show_progress ) {

                    OTF_Reader_statisticBytesProgress( reader, &min, &cur,
                                                       &max );

                    cur_bytes = cur; /* cur - min */

                    /* calculate rank specific progress for the
                    current stream */
                    global_data.tmp_progress =
                        (double) ( (double) (cur_bytes + cur_bytes_ges) /
                        (double) total_bytes );

                    update_progress( info, &global_data, i, rank_data.num_ostreams );
                }

            }

        }

        /* close everything */
        OTF_HandlerArray_close( handlers );
        OTF_Reader_close( reader );
        OTF_WStream_close( wstream );

    }

    if( show_progress ) {

        /* wait for other processes to finish */
        if( my_rank == 0 ) {

            /* set own progress to 100 % */
            global_data.tmp_progress = 1.0;

            /* check every 0.2 sec for new progress until all ranks
            have finished */
            while( 1 ) {

                /* update_progress() returns 0 if all ranks finished */
                if( ! update_progress( info, &global_data, 0, 1) ) {

                    break;

                }

                /* sleep 0.2 s --> select is used because of portability */
                tv.tv_sec = 0;
                tv.tv_usec = 200000;
                select(0, NULL, NULL, NULL, &tv);

            }

            printf("%7.2f %% done\n", 100.0);
            fflush( stdout );

#ifdef OTFMERGE_MPI
            /* clear all open requests in info array */
            for( i = 1; i < num_ranks; i++ ) {

                if( info[i].request != MPI_REQUEST_NULL ) {

                    MPI_Cancel( &(info[i].request) );

                }

            }
#endif /* OTFMERGE_MPI */

        }

#ifdef OTFMERGE_MPI
        if( my_rank != 0 ) {

            /* rank != 0 has finished and sends a last message to ranks 0 */

            /* first wait until the previous msg was received by rank 0 */
            MPI_Wait( &(info[0].request), &status );
            /* fill buffer with valid values */
            info[0].value.progress = 100.0;
            info[0].value.is_alive = 0;
            /* send message and wait until the buffer is free for reuse */
            MPI_Isend( &(info[0].value.progress), 1, global_data.buftype, 0, 0,
                       MPI_COMM_WORLD, &(info[0].request));
            MPI_Wait( &(info[0].request), &status );

        }
#endif /* OTFMERGE_MPI */

    }

    OTF_FileManager_close( read_manager );
    OTF_FileManager_close( write_manager );

    /* clear everything and exit */
    return FINISH_EVERYTHING(0);
}


double update_progress( ProgressInfo* info, GlobalData *data, int cur_ostream,
           int num_ostreams) {

    static double progress = 0.0;
    static int tmp = 0;
    char signs[2] = {' ','.'};

#ifdef OTFMERGE_MPI
    MPI_Status status;
    int flag = 0;
    int j;

    if( data->my_rank != 0 ) {

        /* check if previous msg was received by rank 0 already
        --> if not, do nothing in this function;
        else calculate new progress and send the result to root later on */
        MPI_Test( &(info[0].request), &flag, &status );

    }

    /* calculate progress if necessary */
    if( data->my_rank == 0 || flag )
#endif /* OTFMERGE_MPI */
    {

        info[0].value.progress =
            data->tmp_progress / (double)num_ostreams + (double)cur_ostream *
            ( 1.0 / (double)num_ostreams );
        info[0].value.progress *= 100.0;

    }

    /* show progress */
    if( data->my_rank == 0) {

        /* set the roots progress as the global progress first
        (in the rigth proportion) */
        progress = info[0].value.progress * info[0].percent;

#ifdef OTFMERGE_MPI
        /* listen to all ranks for new messages */
        for( j = 1; j < data->num_ranks; j++ ) {

            /* check if a new MPI_Irecv is necessary/if the previous msg
            was received */
            if( MPI_REQUEST_NULL == info[j].request ) {

                /* irecv with derived datatype --> double progress,
                uint8_t is_alive */
                MPI_Irecv( &(info[j].buf.progress), 1, data->buftype, j, 0,
                           MPI_COMM_WORLD, &(info[j].request) );

            }

            /* test if current msg was received */
            MPI_Test( &(info[j].request), &flag, &status );

            if( flag ) {

                /* got new values */

                /* MPI_REQUEST_NULL indicates that a new MPI_Irecv
                is necessary */
                info[j].request = MPI_REQUEST_NULL;
                /* the receive-buffer must be copied because its value is
                needed later on and the buffer itself is locked by MPI_Irecv */
                info[j].value.progress = info[j].buf.progress;

                /* check if it was the last msg from rank j 
                --> the second field of the buffer (is_alive) would be 0 */
                if( ! info[j].buf.is_alive ) {

                    /* decrease the number of still living ranks */
                    data->ranks_alive--;

                }

            }

            /* add the progress of rank j proportionally to the
            global progress */
            progress += info[j].value.progress * info[j].percent;

        }
#endif /* OTFMERGE_MPI */

        /* print progress */
        printf("%7.2f %% %c\r", progress, signs[tmp]);
        fflush(stdout);

        tmp ^= 1;

    }
#ifdef OTFMERGE_MPI
    else { /* data->my_rank != 0 */

        /* flag is only set if the send-buffer can be used again and a msg is
        necessary therefore */
        if( flag ) {

            /* send in synchronous mode --> this is because with MPI_Test we
            want to know if the root has started a matching receive operation
            already and not only if we can reuse the send-buffer */
            MPI_Issend( &(info[0].value.progress), 1, data->buftype, 0, 0,
                        MPI_COMM_WORLD, &(info[0].request));

        }

    }
#endif /* OTFMERGE_MPI */

    /* returns 0 if all ranks have finished */
    return data->ranks_alive;
}


int finish_everything( char *infile, char* outfile, ProgressInfo* info,
        RankData* data, int ret ) {

    int i;

    if( infile ) {

        free( infile );

    }

    if( outfile ) {

        free( outfile );

    }

    if( info ) {

        free( info );

    }

    if( data->ostreams ) {

        for( i = 0; i < data->num_ostreams; i++ ) {

            if( data->ostreams[i].cpus ) {

                free( data->ostreams[i].cpus );

            }

        }

        free( data->ostreams );

    }

#ifdef OTFMERGE_MPI
    if( ret == 0 ) {

        MPI_Finalize();

    } else {

        MPI_Abort( MPI_COMM_WORLD, ret ); 

    }
#endif /* OTFMERGE_MPI */

    return ret;
}
