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

#include "otf.h"
#include "otfaux.h"

#include "collect_data.h"
#include "otfprofile.h"


using namespace std;
/*store current callpath for each process  */
map<uint32_t,string> callpathMap;

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


/* definition record handler functions */

static int handle_def_creator( void* fha, uint32_t stream, const char* creator,
               OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    alldata->creator= creator;

    return OTF_RETURN_OK;
}


static int handle_def_version( void* fha, uint32_t stream,
               uint8_t major, uint8_t minor, uint8_t sub, const char* suffix,
               OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    ostringstream version;
    version << (int)major << "." << (int)minor;
    if ( sub > 0 ) {

        version << "." << (int)sub;

    }
    version << suffix;

    alldata->version= version.str();

    return OTF_RETURN_OK;
}


static int handle_def_comment( void* fha, uint32_t stream, const char* comment,
               OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    /* add new-line between each comment record */
    if ( 0 < alldata->comments.length() ) {

        alldata->comments+= "\n";

    }


    /* wrap lines after 80 characters */

    const string::size_type LINE_WRAP= 80;

    string tmp= comment;

    do {

        if ( tmp.length() <= LINE_WRAP ) {

            alldata->comments+= tmp;
            break;

        } else {

            string::size_type next_wrap=
                tmp.find_last_of( " .!?:;,", LINE_WRAP -1 );
            next_wrap= ( string::npos == next_wrap ) ? LINE_WRAP : next_wrap +1;

            alldata->comments+= tmp.substr( 0, next_wrap ) + '\n';
            tmp= tmp.substr( next_wrap );

        }

    } while( 0 != tmp.length() );

    return OTF_RETURN_OK;
}


static int handle_def_timerres( void* fha, uint32_t stream,
               uint64_t ticksPerSecond, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    alldata->timerResolution= ticksPerSecond;

    return OTF_RETURN_OK;
}


static int handle_def_process( void* fha, uint32_t stream, uint32_t process,
               const char* name, uint32_t parent, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    alldata->allProcesses.insert( Process( process, parent ) );
    alldata->processIdNameMap[process]= name;

    return OTF_RETURN_OK;
}


static int handle_def_function( void* fha, uint32_t stream, uint32_t function,
               const char* name, uint32_t funcGroup, uint32_t source,
               OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    alldata->functionIdNameMap[function]= name;

    return OTF_RETURN_OK;
}


static int handle_def_collop( void* fha, uint32_t stream, uint32_t collOp,
               const char* name, uint32_t type, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    alldata->collectiveOperationsToClasses[collOp]= type;

    return OTF_RETURN_OK;
}


static int handle_def_counter( void* fha, uint32_t stream, uint32_t counter,
               const char* name, uint32_t properties, uint32_t counterGroup,
               const char* unit, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    if ( OTF_COUNTER_TYPE_ACC == ( properties & OTF_COUNTER_TYPE_BITS ) ) {

        alldata->countersOfInterest.insert( counter );

        alldata->counterIdNameMap[counter]= name;

    }

    return OTF_RETURN_OK;
}


static int handle_def_keyvalue( void* fha, uint32_t stream, uint32_t key,
               OTF_Type type, const char* name, const char* description,
               OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    if ( 0 == strcmp( name, OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_TIME_NAME ) ) {

        alldata->recvTimeKey= key;

    }

    return OTF_RETURN_OK;
}


/* event record handler functions */

static int handle_enter( void* fha, uint64_t time, uint32_t function,
               uint32_t process, uint32_t source, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    list<StackType>& stack= alldata->stackPerProcess[ process ];
    stack.push_back( StackType( function, time ) );

    if (alldata->params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
    {
        /* store current callpath */
        std::ostringstream os;
        os << " "<<function;
        callpathMap[process] += os.str();
        /* save maximum length, for buffer allocation in reduce_data.cpp*/
        if(alldata->maxCallpathLength < callpathMap[process].length())
            alldata->maxCallpathLength = callpathMap[process].length();
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
    uint64_t incl= time - top.timestamp;
    uint64_t excl= incl - top.childDuration;

    if ( parent_it != stack.rend() ) {

        parent_it->childDuration += incl;

    }


    for ( map< uint64_t, StackType::CounterData >::const_iterator it=
          top.counterIdDataMap.begin( );
          it != top.counterIdDataMap.end( ); it++ ) {

        const uint64_t& counter= it->first;
        const uint64_t& firstvalue= it->second.firstValue;
        const uint64_t& lastvalue= it->second.lastValue;
        const uint64_t& lasttime= it->second.lastTime;

        if ( lasttime == time && firstvalue != (uint64_t)-1 &&
             lastvalue != (uint64_t)-1 ) {

            uint64_t counter_incl= lastvalue - firstvalue;
            uint64_t counter_excl= counter_incl - it->second.childDelta;

            alldata->counterMapPerFunctionRank[ Triple( process, func, counter ) ]
                .add( 1, counter_excl, counter_incl );

            if ( parent_it != stack.rend() ) {

                parent_it->counterIdDataMap[ counter ].childDelta+= counter_incl;

            }

        }

    }


    stack.pop_back();

    alldata->functionMapPerRank[ Pair( process, func ) ].add( 1, excl, incl );

    if(alldata->params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
    {
        /* store function by process, callpath and functionId*/
        alldata->functionCallpathMapPerRank[ TripleCallpath( process, callpathMap[process],func ) ].add( 1, excl, incl );
        alldata->functionCallpathMapPerRank[ TripleCallpath( process, callpathMap[process],func ) ].callpath = callpathMap[process];
        /* reduce callpath step at leave */
        callpathMap[process] = callpathMap[process].substr (0,callpathMap[process].find_last_of(" "));
    }

    return OTF_RETURN_OK;
}


static int handle_counter( void* fha, uint64_t time, uint32_t process,
               uint32_t counter, uint64_t value, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    list<StackType>& stack= alldata->stackPerProcess[ process ];

    if ( stack.empty( ) ) {

        return OTF_RETURN_OK;

    }

    if ( alldata->countersOfInterest.find( counter ) ==
         alldata->countersOfInterest.end( ) ) {

        return OTF_RETURN_OK;

    }

    StackType& top= stack.back( );

    if ( time == top.timestamp ) {

        top.counterIdDataMap[ counter ].firstValue= value;

    } else {

        map< uint64_t, StackType::CounterData >::iterator it=
            top.counterIdDataMap.find( counter );

        if ( it != top.counterIdDataMap.end() ) {

            StackType::CounterData& top_counter= it->second;
            top_counter.lastValue= value;
            top_counter.lastTime= time;

        }
    }

    return OTF_RETURN_OK;
}


static int handle_send( void* fha, uint64_t time, uint32_t sender,
               uint32_t receiver, uint32_t group, uint32_t type,
               uint32_t length, uint32_t source, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    double duration= 0.0;

    /* get matching receive time from key-values, if available */

    if ( 0 != alldata->recvTimeKey ) {

        uint64_t recv_time;
        if ( 0 == OTF_KeyValueList_getUint64( kvlist, alldata->recvTimeKey,
                 &recv_time ) ) {

            /* ignore "backward-running" messages */
            if( recv_time > time ) {

                duration= (double) ( recv_time - time );

            }

        }
    }

    alldata->messageMapPerRankPair[ Pair(sender, receiver) ]
        .add_send( 1, length, duration );
    alldata->messageMapPerRank[ sender ].add_send( 1, length, duration );

    /* get message speed */

    if ( length > 0 && duration > 0.0 ) {

        uint64_t speed_bin=
            Logi( (uint64_t)(
                  ( (double)length * (double)alldata->timerResolution ) /
                  duration ), MessageSpeedData::BIN_LOG_BASE );

        uint64_t length_bin= Logi( length, MessageSpeedData::BIN_LOG_BASE );

        alldata->messageSpeedMapPerLength[ Pair( speed_bin, length_bin ) ]
            .add( 1 );

    }

    return OTF_RETURN_OK;
}


static int handle_recv( void* fha, uint64_t time, uint32_t receiver,
               uint32_t sender, uint32_t group, uint32_t type, uint32_t length,
               uint32_t source, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    /* duration will never be available at receive event */
    double duration= 0.0;

    alldata->messageMapPerRankPair[ Pair(receiver, sender) ]
        .add_recv( 1, length, duration );
    alldata->messageMapPerRank[ receiver ].add_recv( 1, length, duration );

    return OTF_RETURN_OK;
}

static int handle_begin_collop( void* fha, uint64_t time, uint32_t process,
               uint32_t collOp, uint64_t matchingId, uint32_t procGroup,
               uint32_t rootProc, uint64_t sent, uint64_t received,
               uint32_t scltoken, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    alldata->pendingCollectives[ Pair( matchingId, process ) ]=
        PendingCollective( collOp, sent, received, time );

    return OTF_RETURN_OK;
}

static int handle_end_collop( void* fha, uint64_t time, uint32_t process,
               uint64_t matchingId, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    /* get corresponding pending collective operation */

    map< Pair, PendingCollective, ltPair >::iterator pending_it=
        alldata->pendingCollectives.find( Pair( matchingId, process ) );
    assert( pending_it != alldata->pendingCollectives.end() );

    const PendingCollective& pending= pending_it->second;

    /* get class of collective operation */

    map< uint64_t, uint64_t >::const_iterator op_class_it=
        alldata->collectiveOperationsToClasses.find( pending.collop );
    assert( op_class_it != alldata->collectiveOperationsToClasses.end() );

    const uint64_t& op_class= op_class_it->second;

    /* calculate duration */
    double duration= (double) ( time - pending.begin_time );

    /* add collective operation to statistics */

    if ( OTF_COLLECTIVE_TYPE_BARRIER == op_class ) {

        alldata->collectiveMapPerRank[ Pair( process, op_class ) ]
            .add_send( 1, 0, duration );
        alldata->collectiveMapPerRank[ Pair( process, op_class ) ]
            .add_recv( 1, 0, duration );

    } else {

        if ( 0 < pending.bytes_send ) {

            alldata->collectiveMapPerRank[ Pair( process, op_class ) ]
                .add_send( 1, pending.bytes_send, duration );

        }
        if ( 0 < pending.bytes_recv ) {

            alldata->collectiveMapPerRank[ Pair( process, op_class ) ]
                .add_recv( 1, pending.bytes_recv, duration );

        }

    }

    /* erase processed pending collective operation from map */
    alldata->pendingCollectives.erase( pending_it );

    return OTF_RETURN_OK;
}


static int handle_function_summary( void* fha, uint64_t time, uint32_t func,
               uint32_t process, uint64_t count, uint64_t exclTime,
               uint64_t inclTime, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    /* add/overwrite function statistics */

    FunctionData tmp;

    tmp.count.cnt = tmp.count.sum = count;
    tmp.count.min = tmp.count.max = 0;

    tmp.excl_time.cnt = count;
    tmp.excl_time.sum = exclTime;
    tmp.excl_time.min = tmp.excl_time.max = 0;

    tmp.incl_time.cnt = count;
    tmp.incl_time.sum = inclTime;
    tmp.incl_time.min = tmp.incl_time.max = 0;

    alldata->functionMapPerRank[ Pair( process, func ) ]= tmp;

    return OTF_RETURN_OK;
}


static int handle_message_summary( void* fha, uint64_t time, uint32_t process,
               uint32_t peer, uint32_t comm, uint32_t type, uint64_t sentNumber,
               uint64_t receivedNumber, uint64_t sentBytes,
               uint64_t receivedBytes, OTF_KeyValueList* kvlist ) {

    AllData* alldata= (AllData*) fha;


    /* do handle this record only if there is a peer and no
    communicator and tag (default behavior of VampirTrace) */
    if ( 0 != peer && 0 == comm && 0 == type ) {

        /* add/overwrite message statistics */

        MessageData tmp;

        if ( 0 < sentNumber ) {

            tmp.count_send.cnt= tmp.count_send.sum= sentNumber;
            tmp.count_send.min= tmp.count_send.max= 0;

            tmp.bytes_send.cnt= sentNumber;
            tmp.bytes_send.sum= sentBytes;
            tmp.bytes_send.min= tmp.bytes_send.max= 0;

        }
        if ( 0 < receivedNumber ) {

            tmp.count_recv.cnt= tmp.count_recv.sum= receivedNumber;
            tmp.count_recv.min= tmp.count_recv.max= 0;

            tmp.bytes_recv.cnt= receivedNumber;
            tmp.bytes_recv.sum= receivedBytes;
            tmp.bytes_recv.min= tmp.bytes_recv.max= 0;

        }

        alldata->messageMapPerRankPair[ Pair(process, peer) ]= tmp;
        alldata->messageMapPerRank[ process ]= tmp;
    }

    return OTF_RETURN_OK;
}


static int handle_collop_summary( void* fha, uint64_t time, uint32_t process,
               uint32_t comm, uint32_t collOp, uint64_t sentNumber,
               uint64_t receivedNumber, uint64_t sentBytes,
               uint64_t receivedBytes, OTF_KeyValueList* kvlist ) {


    AllData* alldata= (AllData*) fha;


    /* do handle this record only if there is a coll.-op and no communicator
    (default behavior of VampirTrace) */
    if ( 0 != collOp && 0 == comm ) {

        /* get class of collective operation */

        map< uint64_t, uint64_t >::const_iterator op_class_it=
            alldata->collectiveOperationsToClasses.find( collOp );
        assert( op_class_it != alldata->collectiveOperationsToClasses.end() );

        const uint64_t& op_class= op_class_it->second;

        /* add/overwrite collective operation statistics */

        CollectiveData tmp;

        if ( 0 < sentNumber ) {

            tmp.count_send.cnt= tmp.count_send.sum= sentNumber;
            tmp.count_send.min= tmp.count_send.max= 0;

            tmp.bytes_send.cnt= sentNumber;
            tmp.bytes_send.sum= sentBytes;
            tmp.bytes_send.min= tmp.bytes_send.max= 0;

        }
        if ( 0 < receivedNumber ) {

            tmp.count_recv.cnt= tmp.count_recv.sum= receivedNumber;
            tmp.count_recv.min= tmp.count_recv.max= 0;

            tmp.bytes_recv.cnt= receivedNumber;
            tmp.bytes_recv.sum= receivedBytes;
            tmp.bytes_recv.min= tmp.bytes_recv.max= 0;

        }

        alldata->collectiveMapPerRank[ Pair( process, op_class ) ]= tmp;

    }

    return OTF_RETURN_OK;
}


static bool read_definitions( AllData& alldata, OTF_Reader* reader ) {

    bool error= false;

    /* open OTF handler array */
    OTF_HandlerArray* handlers= OTF_HandlerArray_open( );
    assert( handlers );

    /* set record handler functions */

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_def_creator,
        OTF_DEFCREATOR_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_def_version,
        OTF_DEFVERSION_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_def_comment,
        OTF_DEFINITIONCOMMENT_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_def_timerres,
        OTF_DEFTIMERRESOLUTION_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_def_process,
        OTF_DEFPROCESS_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_def_function,
        OTF_DEFFUNCTION_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_def_collop,
        OTF_DEFCOLLOP_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_def_counter,
        OTF_DEFCOUNTER_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_def_keyvalue,
        OTF_DEFKEYVALUE_RECORD );

    /* set record handler's first arguments */

    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_DEFCREATOR_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_DEFVERSION_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_DEFINITIONCOMMENT_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_DEFTIMERRESOLUTION_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_DEFPROCESS_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_DEFFUNCTION_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_DEFCOLLOP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_DEFCOUNTER_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_DEFKEYVALUE_RECORD );

    /* read definitions */
    uint64_t read_ret= OTF_Reader_readDefinitions( reader, handlers );
    if ( OTF_READ_ERROR == read_ret ) {

        cerr << "ERROR: Could not read definitions." << endl;
        error= true;

    }

    /* close OTF handler array */
    OTF_HandlerArray_close( handlers );

    return !error;
}


#ifdef OTFPROFILE_MPI
static void share_definitions( AllData& alldata ) {

    assert( 1 < alldata.numRanks );

    char* buffer;
    int buffer_size= 0;
    int buffer_pos= 0;

    /* get size needed to send definitions to workers */

    if ( 0 == alldata.myRank ) {

        int size;

        MPI_Pack_size( 1 + alldata.functionIdNameMap.size() * 2 +
                       1 + alldata.counterIdNameMap.size() * 2 +
                       1 + alldata.collectiveOperationsToClasses.size() * 2 +
                       1 + alldata.countersOfInterest.size() +
                       1 /* timerResolution */ +
                       1 /* recvTimeKey */,
                       MPI_LONG_LONG_INT, MPI_COMM_WORLD, &buffer_size );

        /* functionIdNameMap seconds */
        for ( map< uint64_t, string >::const_iterator it =
              alldata.functionIdNameMap.begin();
              it != alldata.functionIdNameMap.end(); it++ ) {

            MPI_Pack_size( it->second.length() +1, MPI_CHAR,
                           MPI_COMM_WORLD, &size );
            buffer_size += size;

        }

        /* counterIdNameMap seconds */
        for ( map< uint64_t, string >::const_iterator it =
              alldata.counterIdNameMap.begin();
              it != alldata.counterIdNameMap.end(); it++ ) {

            MPI_Pack_size( it->second.length() +1, MPI_CHAR,
                           MPI_COMM_WORLD, &size );
            buffer_size += size;

        }

        /* get size of additional definitions needed for CSV creation */
        if ( alldata.params.create_csv ) {

            /* processIdNameMap.size() + firsts */
            MPI_Pack_size( 1 + alldata.processIdNameMap.size() * 2,
                           MPI_LONG_LONG_INT, MPI_COMM_WORLD, &size );
            buffer_size += size;

            /* processIdNameMap seconds */
            for ( map< uint64_t, string >::const_iterator it =
                  alldata.processIdNameMap.begin();
                  it != alldata.processIdNameMap.end(); it++ ) {

                MPI_Pack_size( it->second.length() +1, MPI_CHAR,
                               MPI_COMM_WORLD, &size );
                buffer_size += size;

            }

        }

    }

    /* broadcast buffer size */
    MPI_Bcast( &buffer_size, 1, MPI_INT, 0, MPI_COMM_WORLD );

    /* allocate buffer */
    buffer= new char[ buffer_size ];
    assert( buffer );

    /* pack definitions to buffer */

    if ( 0 == alldata.myRank ) {

        /* functionIdNameMap.size() */
        uint64_t funcid_name_map_size= alldata.functionIdNameMap.size();
        MPI_Pack( &funcid_name_map_size, 1, MPI_LONG_LONG_INT, buffer,
                  buffer_size, &buffer_pos, MPI_COMM_WORLD );

        /* functionIdNameMap */
        for ( map< uint64_t, string >::const_iterator it =
              alldata.functionIdNameMap.begin();
              it != alldata.functionIdNameMap.end(); it++ ) {

            /* functionIdNameMap.first */
            uint64_t first= it->first;
            MPI_Pack( &first, 1, MPI_LONG_LONG_INT, buffer, buffer_size,
                      &buffer_pos, MPI_COMM_WORLD );

            /* functionIdNameMap.second.length() */
            uint64_t second_length= it->second.length() +1;
            MPI_Pack( &second_length, 1, MPI_LONG_LONG_INT, buffer, buffer_size,
                      &buffer_pos, MPI_COMM_WORLD );

            /* functionIdNameMap.second */
            char* second= strdup( it->second.c_str() );
            assert( second );
            MPI_Pack( second, second_length, MPI_CHAR, buffer, buffer_size,
                      &buffer_pos, MPI_COMM_WORLD );
            free( second );

        }

        /* counterIdNameMap.size() */
        uint64_t cntrid_name_map_size= alldata.counterIdNameMap.size();
        MPI_Pack( &cntrid_name_map_size, 1, MPI_LONG_LONG_INT, buffer,
                  buffer_size, &buffer_pos, MPI_COMM_WORLD );

        /* counterIdNameMap */
        for ( map< uint64_t, string >::const_iterator it =
              alldata.counterIdNameMap.begin();
              it != alldata.counterIdNameMap.end(); it++ ) {

            /* counterIdNameMap.first */
            uint64_t first= it->first;
            MPI_Pack( &first, 1, MPI_LONG_LONG_INT, buffer, buffer_size,
                      &buffer_pos, MPI_COMM_WORLD );

            /* counterIdNameMap.second.length() */
            uint64_t second_length= it->second.length() +1;
            MPI_Pack( &second_length, 1, MPI_LONG_LONG_INT, buffer, buffer_size,
                      &buffer_pos, MPI_COMM_WORLD );

            /* counterIdNameMap.second */
            char* second= strdup( it->second.c_str() );
            assert( second );
            MPI_Pack( second, second_length, MPI_CHAR, buffer, buffer_size,
                      &buffer_pos, MPI_COMM_WORLD );
            free( second );

        }

        /* collectiveOperationsToClasses.size() */
        uint64_t collop_classes_map_size=
            alldata.collectiveOperationsToClasses.size();
        MPI_Pack( &collop_classes_map_size, 1, MPI_LONG_LONG_INT, buffer,
                  buffer_size, &buffer_pos, MPI_COMM_WORLD );

        /* collectiveOperationsToClasses */
        for ( map< uint64_t, uint64_t >::const_iterator it =
              alldata.collectiveOperationsToClasses.begin();
              it != alldata.collectiveOperationsToClasses.end(); it++ ) {

            /* collectiveOperationsToClasses.first */
            uint64_t first= it->first;
            MPI_Pack( &first, 1, MPI_LONG_LONG_INT, buffer, buffer_size,
                      &buffer_pos, MPI_COMM_WORLD );
            /* collectiveOperationsToClasses.second */
            uint64_t second= it->second;
            MPI_Pack( &second, 1, MPI_LONG_LONG_INT, buffer, buffer_size,
                      &buffer_pos, MPI_COMM_WORLD );

        }

        /* countersOfInterest.size() */
        uint64_t counters_size= alldata.countersOfInterest.size();
        MPI_Pack( &counters_size, 1, MPI_LONG_LONG_INT, buffer, buffer_size,
                  &buffer_pos, MPI_COMM_WORLD );

        /* countersOfInterest */
        for ( set< uint64_t >::const_iterator it=
              alldata.countersOfInterest.begin();
              it != alldata.countersOfInterest.end(); it++ ) {

            uint64_t counter= *it;
            MPI_Pack( &counter, 1, MPI_LONG_LONG_INT, buffer, buffer_size,
                      &buffer_pos, MPI_COMM_WORLD );

        }

        /* recvTimeKey */
        MPI_Pack( &(alldata.recvTimeKey), 1, MPI_LONG_LONG_INT, buffer,
                  buffer_size, &buffer_pos, MPI_COMM_WORLD );

        /* timerResolution */
        MPI_Pack( &(alldata.timerResolution), 1, MPI_LONG_LONG_INT, buffer,
                  buffer_size, &buffer_pos, MPI_COMM_WORLD );

        /* pack additional definitions needed for CSV creation */
        if ( alldata.params.create_csv ) {

            /* processIdNameMap.size() */
            uint64_t procid_name_map_size= alldata.processIdNameMap.size();
            MPI_Pack( &procid_name_map_size, 1, MPI_LONG_LONG_INT, buffer,
                      buffer_size, &buffer_pos, MPI_COMM_WORLD );

            /* processIdNameMap */
            for ( map< uint64_t, string >::const_iterator it =
                  alldata.processIdNameMap.begin();
                  it != alldata.processIdNameMap.end(); it++ ) {

                /* processIdNameMap.first */
                uint64_t first= it->first;
                MPI_Pack( &first, 1, MPI_LONG_LONG_INT, buffer, buffer_size,
                          &buffer_pos, MPI_COMM_WORLD );

                /* processIdNameMap.second.length() */
                uint64_t second_length= it->second.length() +1;
                MPI_Pack( &second_length, 1, MPI_LONG_LONG_INT, buffer, buffer_size,
                          &buffer_pos, MPI_COMM_WORLD );

                /* processIdNameMap.second */
                char* second= strdup( it->second.c_str() );
                assert( second );
                MPI_Pack( second, second_length, MPI_CHAR, buffer, buffer_size,
                          &buffer_pos, MPI_COMM_WORLD );
                free( second );

            }

        }

    }

    /* broadcast definitions buffer */
    MPI_Bcast( buffer, buffer_size, MPI_PACKED, 0, MPI_COMM_WORLD );

    /* unpack definitions from buffer */

    if ( 0 != alldata.myRank ) {

        /* functionIdNameMap.size() */
        uint64_t funcid_name_map_size;
        MPI_Unpack( buffer, buffer_size, &buffer_pos,
                    &funcid_name_map_size, 1, MPI_LONG_LONG_INT,
                    MPI_COMM_WORLD );

        /* functionIdNameMap */
        for ( uint64_t i= 0; i < funcid_name_map_size; i++ ) {

            /* functionIdNameMap.first */
            uint64_t first;
            MPI_Unpack( buffer, buffer_size, &buffer_pos, &first, 1,
                        MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            /* functionIdNameMap.second.length() */
            uint64_t second_length;

            MPI_Unpack( buffer, buffer_size, &buffer_pos, &second_length, 1,
                        MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            /* functionIdNameMap.second */
            char* second= new char[ second_length ];
            assert( second );
            MPI_Unpack( buffer, buffer_size, &buffer_pos, second,
                        second_length, MPI_CHAR, MPI_COMM_WORLD );

            alldata.functionIdNameMap[ first ]= second;

            delete [] second;
        }

        /* counterIdNameMap.size() */
        uint64_t cntrid_name_map_size;
        MPI_Unpack( buffer, buffer_size, &buffer_pos,
                    &cntrid_name_map_size, 1, MPI_LONG_LONG_INT,
                    MPI_COMM_WORLD );

        /* counterIdNameMap */
        for ( uint64_t i= 0; i < cntrid_name_map_size; i++ ) {

            /* counterIdNameMap.first */
            uint64_t first;
            MPI_Unpack( buffer, buffer_size, &buffer_pos, &first, 1,
                        MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            /* counterIdNameMap.second.length() */
            uint64_t second_length;

            MPI_Unpack( buffer, buffer_size, &buffer_pos, &second_length, 1,
                        MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            /* counterIdNameMap.second */
            char* second= new char[ second_length ];
            assert( second );
            MPI_Unpack( buffer, buffer_size, &buffer_pos, second,
                        second_length, MPI_CHAR, MPI_COMM_WORLD );

            alldata.counterIdNameMap[ first ]= second;

            delete [] second;
        }

        /* collectiveOperationsToClasses.size() */
        uint64_t collop_classes_map_size;
        MPI_Unpack( buffer, buffer_size, &buffer_pos,
                    &collop_classes_map_size, 1, MPI_LONG_LONG_INT,
                    MPI_COMM_WORLD );

        /* collectiveOperationsToClasses */
        for ( uint64_t i= 0; i < collop_classes_map_size; i++ ) {

            /* collectiveOperationsToClasses.first */
            uint64_t first;
            MPI_Unpack( buffer, buffer_size, &buffer_pos, &first, 1,
                        MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            /* collectiveOperationsToClasses.second */
            uint64_t second;
            MPI_Unpack( buffer, buffer_size, &buffer_pos, &second, 1,
                        MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            alldata.collectiveOperationsToClasses[ first ]= second;

        }

        /* countersOfInterest.size() */
        uint64_t counters_size;
        MPI_Unpack( buffer, buffer_size, &buffer_pos,
                    &counters_size, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

        /* countersOfInterest */
        for ( uint64_t i= 0; i < counters_size; i++ ) {

            uint64_t counter;
            MPI_Unpack( buffer, buffer_size, &buffer_pos, &counter, 1,
                        MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            alldata.countersOfInterest.insert( counter );

        }

        /* recvTimeKey */
        MPI_Unpack( buffer, buffer_size, &buffer_pos, &(alldata.recvTimeKey),
                    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

        /* timerResolution */
        MPI_Unpack( buffer, buffer_size, &buffer_pos,
                    &(alldata.timerResolution), 1, MPI_LONG_LONG_INT,
                    MPI_COMM_WORLD );
 
        /* unpack additional definitions needed for CSV creation */
        if ( alldata.params.create_csv ) {

            /* processIdNameMap.size() */
            uint64_t procid_name_map_size;
            MPI_Unpack( buffer, buffer_size, &buffer_pos,
                        &procid_name_map_size, 1, MPI_LONG_LONG_INT,
                        MPI_COMM_WORLD );

            /* processIdNameMap */
            for ( uint64_t i= 0; i < procid_name_map_size; i++ ) {

                /* processIdNameMap.first */
                uint64_t first;
                MPI_Unpack( buffer, buffer_size, &buffer_pos, &first, 1,
                            MPI_LONG_LONG_INT, MPI_COMM_WORLD );

                /* processIdNameMap.second.length() */
                uint64_t second_length;

                MPI_Unpack( buffer, buffer_size, &buffer_pos, &second_length, 1,
                            MPI_LONG_LONG_INT, MPI_COMM_WORLD );

                /* processIdNameMap.second */
                char* second= new char[ second_length ];
                assert( second );
                MPI_Unpack( buffer, buffer_size, &buffer_pos, second,
                            second_length, MPI_CHAR, MPI_COMM_WORLD );

                alldata.processIdNameMap[ first ]= second;

                delete [] second;
            }
        }
    }

    delete[] buffer;
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
    OTF_HandlerArray_setHandler( handlers,
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

    /* set record handler's first arguments */

    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_ENTER_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_LEAVE_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_COUNTER_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_SEND_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_RECEIVE_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_BEGINCOLLOP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_ENDCOLLOP_RECORD );

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


static bool read_statistics( AllData& alldata, OTF_Reader* reader ) {

    bool error= false;

    /* open OTF handler array */
    OTF_HandlerArray* handlers= OTF_HandlerArray_open( );
    assert( handlers );

    /* set record handler functions */

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_function_summary,
        OTF_FUNCTIONSUMMARY_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_message_summary,
        OTF_MESSAGESUMMARY_RECORD );
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handle_collop_summary,
        OTF_COLLOPSUMMARY_RECORD );

    /* set record handler's first arguments */

    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_FUNCTIONSUMMARY_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_MESSAGESUMMARY_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &alldata,
        OTF_COLLOPSUMMARY_RECORD );

    /* select processes to read */
    OTF_Reader_setProcessStatusAll( reader, 0 );
    for ( uint32_t i= 0; i < alldata.myProcessesNum; i++ ) {

        OTF_Reader_enableProcess( reader, alldata.myProcessesList[ i ] );
    }

    /* prepare progress */
    if ( alldata.params.progress ) {

        OTF_Reader_setRecordLimit( reader, 0 );

        if ( OTF_READ_ERROR != OTF_Reader_readStatistics( reader, handlers ) ) {

            uint64_t min, cur, max;
            OTF_Reader_statisticBytesProgress( reader, &min, &cur, &max );
            prepare_progress( alldata, max );

        }

        OTF_Reader_setRecordLimit( reader, Progress::STATS_RECORD_LIMIT );

    }

    /* read statistics */

    uint64_t records_read= 0;

    while ( OTF_READ_ERROR !=
            ( records_read= OTF_Reader_readStatistics( reader, handlers ) ) ) {

        /* update progress */
        if ( alldata.params.progress ) {

            uint64_t min, cur, max;
            static uint64_t last_cur= 0;

            OTF_Reader_statisticBytesProgress( reader, &min, &cur, &max );
            update_progress( alldata, cur - last_cur );

            last_cur = cur;

        }

        /* stop reading if done */
        if ( 0 == records_read )
            break;
    }

    /* show error message if reading failed */
    if ( OTF_READ_ERROR == records_read ) {

        cerr << "ERROR: Could not read statistics." << endl;
        error= true;

    }

    /* close OTF handler array */
    OTF_HandlerArray_close( handlers );

    return !error;
}


bool CollectData( AllData& alldata ) {
	alldata.maxCallpathLength = 0;
    bool error= false;

    /* start runtime measurement for collecting data */
    StartMeasurement( alldata, 1, true, "collect data" );

    /* open OTF file manager and reader */

    OTF_FileManager* manager=
        OTF_FileManager_open( alldata.params.max_file_handles );
    assert( manager );

    OTF_Reader* reader=
        OTF_Reader_open( alldata.params.input_file_prefix.c_str(), manager );
    assert( reader );

    do {

        if ( 0 == alldata.myRank ) {

            /* read definitions */

            VerbosePrint( alldata, 1, true, "reading definitions\n" );

            error= !read_definitions( alldata, reader );

        }

#ifdef OTFPROFILE_MPI
        /* broadcast error indicator to workers */
        if ( SyncError( alldata, error, 0 ) ) {

            break;

        }

        /* share definitions needed for reading events to workers */

        if ( 1 < alldata.numRanks ) {

            share_definitions( alldata );

        }
#endif /* OTFPROFILE_MPI */

        /* either read data from events or statistics */

        if ( alldata.params.read_from_stats ) {

            VerbosePrint( alldata, 1, true, "reading statistics\n" );

            error= !read_statistics( alldata, reader );

        } else {

            VerbosePrint( alldata, 1, true, "reading events\n" );

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
        StopMeasurement( alldata, true, "collect data" );

    }

    return !error;
}
