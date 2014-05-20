/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
*/

#include <cassert>
#include <iostream>
#include <sstream>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <math.h>

#include "otf.h"
#include "otfaux.h"

#include "collect_dispersion.h"
#include "otfprofile.h"


using namespace std;

/* fence between statistics parts within the buffer for consistency checking */
enum { FENCE= 0xDEADBEEF };

/*store current callpath for each process  */
map<uint32_t,string> callpath;

static void prepare_progress( AllData& alldata, uint64_t max_bytes ) {
    Progress& progress= alldata.progress;

    progress.cur_bytes= 0;
    progress.max_bytes= max_bytes;

#ifdef OTFPROFILE_MPI
    progress.ranks_left= alldata.numRanks -1;

    if ( 1 < alldata.numRanks ) {

        /* reduce max. bytes to rank 0 */
        uint64_t sum_max_bytes;
        MPI_Reduce( &max_bytes, &sum_max_bytes, 1, MPI_LONG_LONG_INT, MPI_SUM,
                    0, MPI_COMM_WORLD );

        if ( 0 == alldata.myRank ) {

            progress.max_bytes= sum_max_bytes;

            progress.recv_buffers= new uint64_t[alldata.numRanks-1];
            assert( progress.recv_buffers );
            progress.recv_requests= new MPI_Request[alldata.numRanks-1];
            assert( progress.recv_requests );
            progress.recv_statuses= new MPI_Status[alldata.numRanks-1];
            assert( progress.recv_statuses );
            progress.recv_indices= new int[alldata.numRanks-1];
            assert( progress.recv_indices );
            progress.rank_cur_bytes= new uint64_t[alldata.numRanks-1];
            assert( progress.rank_cur_bytes );

            /* initialize array of current bytes read and start
            persistent communication */

            for ( uint32_t i= 0; i < alldata.numRanks -1; i++ ) {

                progress.rank_cur_bytes[i]= 0;

                /* create persistent request handle */
                MPI_Recv_init( &(progress.recv_buffers[i]), 1,
                               MPI_LONG_LONG_INT, i+1, Progress::MSG_TAG,
                               MPI_COMM_WORLD,
                               &(progress.recv_requests[i]) );

                /* start persistent communication */
                MPI_Start( &(progress.recv_requests[i]) );

            }

        } else { /* 0 != alldata.myRank */

            /* initialize request handle for sending progress to rank 0 */
            progress.send_request = MPI_REQUEST_NULL;

        }

        /* block until all worker ranks have reached this point to avoid that the
        progress does a big jump at beginning */
        MPI_Barrier( MPI_COMM_WORLD );

    }
#endif /* OTFPROFILE_MPI */

    if ( 0 == alldata.myRank ) {

        /* show initial progress */
        printf( "%7.2f %%\r", 0.0 );
        fflush( stdout );
    }

}


static void update_progress( AllData& alldata, uint64_t delta_bytes,
                bool wait= false ) {

    Progress& progress= alldata.progress;

    progress.cur_bytes += delta_bytes;

    uint64_t sum_cur_bytes= progress.cur_bytes;

#ifdef OTFPROFILE_MPI
    if ( 1 < alldata.numRanks ) {

        if ( 0 == alldata.myRank ) {

            /* get current bytes read from all worker ranks */

            int out_count;
            uint32_t i;

            /* either wait or test for one or more updates from worker ranks */

            if ( wait )
            {

                MPI_Waitsome( alldata.numRanks - 1, progress.recv_requests,
                              &out_count, progress.recv_indices,
                              progress.recv_statuses );

            } else {

                MPI_Testsome( alldata.numRanks - 1, progress.recv_requests,
                              &out_count, progress.recv_indices,
                              progress.recv_statuses );

            }

            if ( MPI_UNDEFINED != out_count ) {

                for ( i= 0; i < (uint32_t) out_count; i++ ) {

                    int index= progress.recv_indices[i];

                    /* worker rank (index+1) is finished? */
                    if ( (uint64_t)-1 == progress.recv_buffers[index] ) {

                        /* this rank is finished */
                        progress.ranks_left--;

                    } else {

                        /* update rank's current bytes read and restart
                        persistent communication */

                        progress.rank_cur_bytes[index]= progress.recv_buffers[index];

                        MPI_Start( &(progress.recv_requests[progress.recv_indices[i]]) );

                    }
                }

            }

            /* recompute sum of current bytes read */
            for( i= 0; i < alldata.numRanks -1; i++ ) {

                sum_cur_bytes += progress.rank_cur_bytes[i];
            }

        } else { /* 0 != alldata.myRank */

            int do_send = 1;
            MPI_Status status;

            /* send only if it's the first send or the request handle isn't
            currently in use */

            if ( MPI_REQUEST_NULL != progress.send_request ) {

                MPI_Test( &(progress.send_request), &do_send, &status );

            }

            if ( do_send ) {

                MPI_Issend( &(progress.cur_bytes), 1, MPI_LONG_LONG_INT, 0,
                            Progress::MSG_TAG, MPI_COMM_WORLD,
                            &progress.send_request );
            }

        }

    }
#endif /* OTFPROFILE_MPI */

    if ( 0 == alldata.myRank ) {

        /* show progress */

        double percent =
            100.0 * (double) sum_cur_bytes / (double) progress.max_bytes;

        static const char signs[2]= { '.',' ' };
        static int signi= 0;

        printf( "%7.2f %% %c\r", percent, signs[signi] );
        fflush( stdout );

        signi^= 1;

    }
}


static void finish_progress( AllData& alldata ) {

#ifdef OTFPROFILE_MPI
    Progress& progress= alldata.progress;

    if ( 1 < alldata.numRanks ) {

        if ( 0 == alldata.myRank ) {

            /* update progress until all worker ranks are
            finished / all bytes are read */

            while ( 0 < progress.ranks_left ) {

                update_progress( alldata, 0, true );
            }

            /* ensure that all requests are inactive before freeing memory */
            MPI_Waitall( alldata.numRanks - 1, progress.recv_requests,
                         progress.recv_statuses );

            /* free memory */
            delete [] progress.recv_buffers;
            delete [] progress.recv_requests;
            delete [] progress.recv_statuses;
            delete [] progress.recv_indices;
            delete [] progress.rank_cur_bytes;

        } else { /* 0 != alldata.myRank */

            MPI_Status status;
            MPI_Wait( &(progress.send_request), &status );

            /* send last current bytes read to rank 0 */
            MPI_Send( &(progress.cur_bytes), 1, MPI_LONG_LONG_INT, 0,
                      Progress::MSG_TAG, MPI_COMM_WORLD );

            /* send marker (-1) to rank 0 which indicates that this worker rank
            is finished */

            progress.cur_bytes = (uint64_t) -1;
            MPI_Send( &(progress.cur_bytes), 1, MPI_LONG_LONG_INT, 0,
                      Progress::MSG_TAG, MPI_COMM_WORLD );

        }

    }
#endif /* OTFPROFILE_MPI */

    if ( 0 == alldata.myRank ) {

        /* show final progress */
        printf( "%7.2f %% done\n", 100.0 );

    }
}


/* event record handler functions */

static int handle_enter( void* fha, uint64_t time, uint32_t function,
               uint32_t process, uint32_t source, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    list<StackType>& stack= alldata->stackPerProcess[ process ];
    stack.push_back( StackType( function, time ) );

    if(alldata->params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
    {
        /* create callpath */
        /* add callpath step on enter event */
        std::ostringstream os;
        os << " " << function;
        callpath[process] += os.str();
    }

    return OTF_RETURN_OK;
}


static int handle_leave( void* fha, uint64_t time, uint32_t function,
               uint32_t process, uint32_t source, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    list<StackType>& stack= alldata->stackPerProcess[ process ];
    assert( !stack.empty() );

    StackType& top= stack.back();
    list<StackType>::reverse_iterator parent_it= ++stack.rbegin();

    uint64_t func= top.fid;
    uint64_t incl_time= time - top.timestamp;
    uint64_t excl_time= incl_time - top.childDuration;
   
    map< uint64_t, FunctionData>::const_iterator it= alldata->functionMapGlobal.find( func );
    assert ( alldata->functionMapGlobal.end() != it );
    FunctionData functionData= it->second;

    double time_min = functionData.DISPERSION_OPTION.min;
    double time_max = functionData.DISPERSION_OPTION.max;
    double time_max_c = 0;
    double time_min_c = 0;
    double time_a= DISPERSION_OPTION;

    if(alldata->params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
    {
        /* get currentfunction from functionCallpathMapGlobal */
        map< PairCallpath, FunctionData,ltPairCallpath>::const_iterator itc= alldata->functionCallpathMapGlobal.find( PairCallpath(func,callpath[process]) );

        assert ( alldata->functionCallpathMapGlobal.end() != itc );
        FunctionData functionCallpathData= itc->second;

        time_min_c = functionCallpathData.DISPERSION_OPTION.min;
        time_max_c = functionCallpathData.DISPERSION_OPTION.max;
    }

    if ( parent_it != stack.rend() ) {

        parent_it->childDuration += incl_time;

    }

    stack.pop_back();

    if ( time_max > time_min) {
   
        uint64_t bin = (uint64_t) ( ( log(time_a) - log(time_min) ) /
                                   ( log(time_max) - log(time_min) )
                                   * 100 );
        uint64_t bin_c = 0;
        if(alldata->params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
            bin_c = (uint64_t) ( ( log(time_a) - log(time_min_c) ) /
                                   ( log(time_max_c) - log(time_min_c) )
                                   * 100 );
        alldata->functionDurationSectionMapPerRank[ Triple(process, func, bin )]
                                                .add( 1, excl_time, incl_time );
        if(alldata->params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
            alldata->functionDurationSectionCallpathMapPerRank[ Quadruple(process, func,callpath[process], bin_c ) ].add( 1, excl_time,callpath[process], incl_time );
    }
    
    if ( time_max == time_a || time_min == time_a ) {
        alldata->functionMinMaxLocationMap [ func ].add( excl_time, process, (time-incl_time) );
    }

    if(alldata->params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
    {
        if ( time_max == time_a || time_min == time_a ) {
            alldata->functionMinMaxLocationCallpathMap [ callpath[process] ].add( excl_time, process, (time-incl_time) );
        }
        /* go one step back on callpath, because of this leave */
        callpath[process] = callpath[process].substr (0,callpath[process].find_last_of(" "));
    }

    return OTF_RETURN_OK;
}

#ifdef OTFPROFILE_MPI
static void share_profiledata( AllData& alldata ) {

    assert( 1 < alldata.numRanks );

    char* buffer;
    int buffer_size= 0;
    int buffer_pos= 0;

    uint64_t fence= FENCE;
    uint32_t num_fences= 1;
   
    /* get size needed to send profile data to workers */

    if ( 0 == alldata.myRank ) {

        int size;

        int s1, s2;

        num_fences++;
        if(alldata.params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
        {
            size= alldata.functionCallpathMapGlobal.size(); /* map< PairCallpath, FunctionData, ltPairCallpath > functionCallpathMapGlobal; */
            num_fences++;
        }
        else
        {
            size= alldata.functionMapGlobal.size(); /* map< uint64_t, FunctionData > functionMapGlobal; */
        }
        /* get bytesize multiplying all pieces */
          
        MPI_Pack_size( num_fences, MPI_LONG_LONG_INT, MPI_COMM_WORLD, &s1 );
        buffer_size += s1;
          
        MPI_Pack_size( 1 + size * 7, MPI_LONG_LONG_INT, MPI_COMM_WORLD, &s1 );
        MPI_Pack_size( size * 6, MPI_DOUBLE, MPI_COMM_WORLD, &s2 );
        buffer_size += s1 + s2;

        /* get bytesize multiplying all pieces */
        MPI_Pack_size( 1 + size * 8, MPI_LONG_LONG_INT, MPI_COMM_WORLD, &s1 );
        MPI_Pack_size( size * 6, MPI_DOUBLE, MPI_COMM_WORLD, &s2 );
        buffer_size += s1 + s2;

        if(alldata.params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
        {
        	map< PairCallpath, FunctionData, ltPairCallpath >::const_iterator it=    alldata.functionCallpathMapGlobal.begin();
        	map< PairCallpath, FunctionData, ltPairCallpath >::const_iterator itend= alldata.functionCallpathMapGlobal.end();
        	for ( ; it != itend; ++it ) {
        		MPI_Pack_size( it->second.callpath.length(), MPI_CHAR, MPI_COMM_WORLD, &s1 );
        		buffer_size += s1;
        	}
        }

    } 

    /* broadcast buffer size */
    MPI_Bcast( &buffer_size, 1, MPI_INT, 0, MPI_COMM_WORLD );

    /* allocate buffer */
    buffer= new char[ buffer_size ];
    assert( buffer );

    uint64_t callpath_length=0;
    MPI_Allreduce(&(alldata.maxCallpathLength),&callpath_length,1,MPI_UNSIGNED_LONG_LONG,MPI_MAX,MPI_COMM_WORLD);
    char* callpath = new char[callpath_length];

    if ( 0 == alldata.myRank ) {
       /* pack parts */
       
       /* extra check that doesn't cost too much */
       MPI_Pack( (void*) &fence, 1, MPI_LONG_LONG_INT, buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
       
       /* pack size of functionMapGlobal */
       uint64_t func_map_global_size= alldata.functionMapGlobal.size();
       MPI_Pack( &func_map_global_size, 1, MPI_LONG_LONG_INT, buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
       
       /* pack functionMapGlobal */
       map< uint64_t, FunctionData >::const_iterator it=    alldata.functionMapGlobal.begin();
       map< uint64_t, FunctionData >::const_iterator itend= alldata.functionMapGlobal.end();
       for ( ; it != itend; ++it ) {
          
          MPI_Pack( (void*) &it->first,                1, MPI_LONG_LONG_INT, buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
          
          MPI_Pack( (void*) &it->second.count.min,     1, MPI_LONG_LONG_INT, buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
          MPI_Pack( (void*) &it->second.count.max,     1, MPI_LONG_LONG_INT, buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
          MPI_Pack( (void*) &it->second.count.sum,     1, MPI_LONG_LONG_INT, buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
          MPI_Pack( (void*) &it->second.count.cnt,     1, MPI_LONG_LONG_INT, buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
          
          MPI_Pack( (void*) &it->second.excl_time.min, 1, MPI_DOUBLE,        buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
          MPI_Pack( (void*) &it->second.excl_time.max, 1, MPI_DOUBLE,        buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
          MPI_Pack( (void*) &it->second.excl_time.sum, 1, MPI_DOUBLE,        buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
          MPI_Pack( (void*) &it->second.excl_time.cnt, 1, MPI_LONG_LONG_INT, buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
          
          MPI_Pack( (void*) &it->second.incl_time.min, 1, MPI_DOUBLE,        buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
          MPI_Pack( (void*) &it->second.incl_time.max, 1, MPI_DOUBLE,        buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
          MPI_Pack( (void*) &it->second.incl_time.sum, 1, MPI_DOUBLE,        buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
          MPI_Pack( (void*) &it->second.incl_time.cnt, 1, MPI_LONG_LONG_INT, buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
       }

       /* extra check that doesn't cost too much */
       MPI_Pack( (void*) &fence, 1, MPI_LONG_LONG_INT, buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );

       if(alldata.params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
       {
           /* pack size of functionCallpathMapGlobal */
           func_map_global_size= alldata.functionCallpathMapGlobal.size();
           MPI_Pack( &func_map_global_size, 1, MPI_LONG_LONG_INT, buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );

           /* pack functionCallpathMapGlobal */
           {
               map< PairCallpath, FunctionData, ltPairCallpath >::const_iterator it=    alldata.functionCallpathMapGlobal.begin();
               map< PairCallpath, FunctionData, ltPairCallpath >::const_iterator itend= alldata.functionCallpathMapGlobal.end();
               uint64_t len;
               for ( ; it != itend; ++it ) {
                   len = it->first.b.length();
                   MPI_Pack( (void*) &it->first.a,                1, MPI_LONG_LONG_INT, buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
                   MPI_Pack( (void*) &len,                1, MPI_LONG_LONG_INT, buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
                   MPI_Pack( (void*) &it->second.count.min,     1, MPI_LONG_LONG_INT, buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
                   MPI_Pack( (void*) &it->second.count.max,     1, MPI_LONG_LONG_INT, buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
                   MPI_Pack( (void*) &it->second.count.sum,     1, MPI_LONG_LONG_INT, buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
                   MPI_Pack( (void*) &it->second.count.cnt,     1, MPI_LONG_LONG_INT, buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );

                   MPI_Pack( (void*) &it->second.excl_time.min, 1, MPI_DOUBLE,        buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
                   MPI_Pack( (void*) &it->second.excl_time.max, 1, MPI_DOUBLE,        buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
                   MPI_Pack( (void*) &it->second.excl_time.sum, 1, MPI_DOUBLE,        buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
                   MPI_Pack( (void*) &it->second.excl_time.cnt, 1, MPI_LONG_LONG_INT, buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );

                   MPI_Pack( (void*) &it->second.incl_time.min, 1, MPI_DOUBLE,        buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
                   MPI_Pack( (void*) &it->second.incl_time.max, 1, MPI_DOUBLE,        buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
                   MPI_Pack( (void*) &it->second.incl_time.sum, 1, MPI_DOUBLE,        buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
                   MPI_Pack( (void*) &it->second.incl_time.cnt, 1, MPI_LONG_LONG_INT, buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
                   MPI_Pack( (void*) it->first.b.c_str(), len, MPI_CHAR, buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
               }
           }
                  /* extra check that doesn't cost too much */
                  MPI_Pack( (void*) &fence, 1, MPI_LONG_LONG_INT, buffer, buffer_size, &buffer_pos, MPI_COMM_WORLD );
       }
    }

    /* broadcast definitions buffer */
    MPI_Bcast( buffer, buffer_size, MPI_PACKED, 0, MPI_COMM_WORLD );
    /* unpack definitions from buffer */
    if ( 0 != alldata.myRank ) {
       
       /* unpack parts */
       
       /* extra check that doesn't cost too much */
       fence= 0;
       MPI_Unpack( buffer, buffer_size, &buffer_pos, &fence, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
       assert( FENCE == fence );

       /* unpack size of functionMapGlobal */
       uint64_t func_map_global_size= 0;
       MPI_Unpack( buffer, buffer_size, &buffer_pos, &func_map_global_size, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

          /* unpack functionMapGlobal */
          for ( uint64_t i= 0; i < func_map_global_size; i++ ) {
             
             uint64_t func;
             FunctionData tmp;
             
             MPI_Unpack( buffer, buffer_size, &buffer_pos, &func,              1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
             
             MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.count.min,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
             MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.count.max,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
             MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.count.sum,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
             MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.count.cnt,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
             
             MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.excl_time.min, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
             MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.excl_time.max, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
             MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.excl_time.sum, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
             MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.excl_time.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
             
             MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.incl_time.min, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
             MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.incl_time.max, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
             MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.incl_time.sum, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
             MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.incl_time.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
             
             alldata.functionMapGlobal[ func ].add( tmp );
          }
          /* extra check that doesn't cost too much */
          fence= 0;
          MPI_Unpack( buffer, buffer_size, &buffer_pos, &fence, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
          assert( FENCE == fence );

          /* unpack size of functionMapGlobal */
          func_map_global_size= 0;
          MPI_Unpack( buffer, buffer_size, &buffer_pos, &func_map_global_size, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
          if(alldata.params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
          {
              /* unpack functionMapCallpathGlobal */
              for ( uint64_t i= 0; i < func_map_global_size; i++ ) {

                  uint64_t func;
                  FunctionData tmp;
                  uint64_t len;

                  MPI_Unpack( buffer, buffer_size, &buffer_pos, &func,              1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
                  MPI_Unpack( buffer, buffer_size, &buffer_pos, &len,              1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

                  MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.count.min,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
                  MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.count.max,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
                  MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.count.sum,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
                  MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.count.cnt,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

                  MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.excl_time.min, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
                  MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.excl_time.max, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
                  MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.excl_time.sum, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
                  MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.excl_time.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

                  MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.incl_time.min, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
                  MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.incl_time.max, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
                  MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.incl_time.sum, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
                  MPI_Unpack( buffer, buffer_size, &buffer_pos, &tmp.incl_time.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
                  MPI_Unpack( buffer, buffer_size, &buffer_pos, callpath, len, MPI_CHAR, MPI_COMM_WORLD );

                  tmp.callpath = callpath;
                  tmp.callpath = tmp.callpath.substr (0,len);
                  alldata.functionCallpathMapGlobal[ PairCallpath(func,tmp.callpath) ].add( tmp );
              }

              /* extra check that doesn't cost too much */
              fence= 0;
              MPI_Unpack( buffer, buffer_size, &buffer_pos, &fence, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
              assert( FENCE == fence );
          }
    }
    delete[] buffer;
    delete[] callpath;
}
#endif /* OTFPROFILE_MPI */


static bool read_events( AllData& alldata, OTF_Reader* reader ) {

    bool error= false;

    /* open OTF handler array */
    OTF_HandlerArray* handlers= OTF_HandlerArray_open( );
    assert( handlers );

    /* set record handler functions */

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_enter,
        OTF_ENTER_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_leave,
        OTF_LEAVE_RECORD );
/*    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_counter,
        OTF_COUNTER_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_send,
        OTF_SEND_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_recv,
        OTF_RECEIVE_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_begin_collop,
        OTF_BEGINCOLLOP_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_end_collop,
        OTF_ENDCOLLOP_RECORD );
*/
    /* set record handler's first arguments */

    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_ENTER_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_LEAVE_RECORD );
/*    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_COUNTER_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_SEND_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_RECEIVE_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_BEGINCOLLOP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_ENDCOLLOP_RECORD );
*/
    /* select processes to read */
    OTF_Reader_setProcessStatusAll( reader, 0 );
    for ( uint32_t i= 0; i < alldata.myProcessesNum; i++ ) {

        OTF_Reader_enableProcess( reader, alldata.myProcessesList[ i ] );
    }

    /* prepare progress */
    if ( alldata.params.progress ) {

        OTF_Reader_setRecordLimit( reader, 0 );

        if ( OTF_READ_ERROR != OTF_Reader_readEvents( reader, handlers ) ) {

            uint64_t min, cur, max;

            OTF_Reader_eventBytesProgress( reader, &min, &cur, &max );
            prepare_progress( alldata, max );

        }

        OTF_Reader_setRecordLimit( reader, Progress::EVENTS_RECORD_LIMIT );

    }

    /* read events */

    uint64_t records_read= 0;

    while ( OTF_READ_ERROR !=
            ( records_read= OTF_Reader_readEvents( reader, handlers ) ) ) {

        /* update progress */
        if ( alldata.params.progress ) {

            uint64_t min, cur, max;
            static uint64_t last_cur= 0;

            OTF_Reader_eventBytesProgress( reader, &min, &cur, &max );
            update_progress( alldata, cur - last_cur );

            last_cur = cur;

        }

        /* stop reading if done */
        if ( 0 == records_read )
            break;
    }

    /* show error message if reading failed */
    if ( OTF_READ_ERROR == records_read ) {

        cerr << "ERROR: Could not read events." << endl;
        error= true;

    }

    /* close OTF handler array */
    OTF_HandlerArray_close( handlers );

    return !error;
}


bool CollectDispersion( AllData& alldata ) {

    bool error= false;
    /* start runtime measurement for collecting dispersion information */
    StartMeasurement( alldata, 1, true, "collect dispersion information" );

    /* open OTF file manager and reader */
    OTF_FileManager* manager=
        OTF_FileManager_open( alldata.params.max_file_handles );
    assert( manager );
    OTF_Reader* reader=
        OTF_Reader_open( alldata.params.input_file_prefix.c_str(), manager );
    assert( reader );
    do {

#ifdef OTFPROFILE_MPI

        /* share definitions needed for reading events to workers */

        if ( 1 < alldata.numRanks ) {
            share_profiledata( alldata );

        }
#endif /* OTFPROFILE_MPI */

        /* read data from events */
        if ( !alldata.params.read_from_stats ) {
            
            VerbosePrint( alldata, 1, true, "reading events for dispersion\n" );

            error= !read_events( alldata, reader );

        }

        /* finish progress */
        if ( alldata.params.progress ) {

            finish_progress( alldata );

        }

#ifdef OTFPROFILE_MPI
        /* synchronize error indicator with workers */
        SyncError( alldata, error );
#endif /* OTFPROFILE_MPI */

    } while( false );
    /* close OTF file manager and reader */

    OTF_Reader_close( reader );
    OTF_FileManager_close( manager );
    if ( !error ) {

        /* stop runtime measurement for collecting data */
        StopMeasurement( alldata, true, "collect dispersion information" );

    }
    return !error;
}
