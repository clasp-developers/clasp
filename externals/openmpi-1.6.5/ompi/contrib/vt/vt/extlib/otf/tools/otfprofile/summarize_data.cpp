/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
*/

#include <cassert>
#include <iostream>

#include "otfprofile.h"
#include "summarize_data.h"


using namespace std;


static void get_grouping( AllData& alldata ) {

    uint32_t r_processes= alldata.allProcesses.size();
    uint32_t r_groups= alldata.params.max_groups;

    set< Process, ltProcess >::iterator pos= alldata.allProcesses.begin();

    for ( uint32_t c= 0;
          c < Grouping::MAX_GROUPS && 0 < r_processes; c++ ) {

        uint32_t n=
            ( ( r_processes / r_groups ) * r_groups < r_processes ) ?
            ( r_processes / r_groups + 1 ) : ( r_processes / r_groups );

        for ( uint32_t i= 0; i < n; i++ ) {

            alldata.grouping.insert( c+1, pos->process );

            pos++;
            r_processes--;

        }

        r_groups--;

    }
}


#ifdef OTFPROFILE_MPI
static void share_grouping( AllData& alldata ) {

    assert( 1 < alldata.numRanks );

    char* buffer;
    int buffer_size= 0;
    int buffer_pos= 0;

    if ( 0 == alldata.myRank ) {

        /* get size needed to send grouping information to workers */

        int size;

        /* alldata.grouping.groupsToProcesses.size() + firsts */
        MPI_Pack_size( 1 + alldata.grouping.groupsToProcesses.size(),
                       MPI_LONG_LONG_INT, MPI_COMM_WORLD, &size );
        buffer_size+= size;

        /* alldata.grouping.groupsToProcesses.second.size() + second */
        for ( map< uint64_t, set<uint64_t> >::const_iterator it=
              alldata.grouping.groupsToProcesses.begin();
              it != alldata.grouping.groupsToProcesses.end(); it++ ) {

            MPI_Pack_size( 1 + it->second.size(), MPI_LONG_LONG_INT,
                           MPI_COMM_WORLD, &size );
            buffer_size+= size;

        }

    }

    /* broadcast buffer size */
    MPI_Bcast( &buffer_size, 1, MPI_INT, 0, MPI_COMM_WORLD );

    /* allocate buffer */
    buffer= new char[ buffer_size ];
    assert( buffer );

    /* pack grouping information to buffer */

    if ( 0 == alldata.myRank ) {

        /* alldata.grouping.groupsToProcesses.size() */
        uint64_t clust_proc_map_size=
            alldata.grouping.groupsToProcesses.size();
        MPI_Pack( &clust_proc_map_size, 1, MPI_LONG_LONG_INT, buffer,
                  buffer_size, &buffer_pos, MPI_COMM_WORLD );

        /* alldata.grouping.groupsToProcesses */
        for ( map< uint64_t, set<uint64_t> >::const_iterator it=
              alldata.grouping.groupsToProcesses.begin();
              it != alldata.grouping.groupsToProcesses.end(); it++ ) {

            /* alldata.grouping.groupsToProcesses.first */
            uint64_t group= it->first;
            MPI_Pack( &group, 1, MPI_LONG_LONG_INT, buffer, buffer_size,
                      &buffer_pos, MPI_COMM_WORLD );

            /* alldata.grouping.groupsToProcesses.second.size() */
            uint64_t processes_size= it->second.size();
            MPI_Pack( &processes_size, 1, MPI_LONG_LONG_INT, buffer, buffer_size,
                      &buffer_pos, MPI_COMM_WORLD );

            /* alldata.grouping.groupsToProcesses.second */
            for ( set<uint64_t>::const_iterator it2= it->second.begin();
                  it2 != it->second.end(); it2++ ) {

                uint64_t process= *it2;
                MPI_Pack( &process, 1, MPI_LONG_LONG_INT, buffer, buffer_size,
                          &buffer_pos, MPI_COMM_WORLD );

            }

        }

    }

    /* broadcast definitions buffer */
    MPI_Bcast( buffer, buffer_size, MPI_PACKED, 0, MPI_COMM_WORLD );

    /* unpack grouping information from buffer */

    if ( 0 != alldata.myRank ) {

        /* alldata.grouping.groupsToProcesses.size() */
        uint64_t clust_proc_map_size;
        MPI_Unpack( buffer, buffer_size, &buffer_pos, &clust_proc_map_size, 1,
                    MPI_LONG_LONG_INT, MPI_COMM_WORLD );

        /* alldata.grouping.groupsToProcesses */
        for ( uint64_t i= 0; i < clust_proc_map_size; i++ ) {

            /* alldata.grouping.groupsToProcesses.first */
            uint64_t group;
            MPI_Unpack( buffer, buffer_size, &buffer_pos, &group, 1,
                        MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            /* alldata.grouping.groupsToProcesses.second.size() */
            uint64_t processes_size;
            MPI_Unpack( buffer, buffer_size, &buffer_pos, &processes_size, 1,
                        MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            /* alldata.grouping.groupsToProcesses.second */
            for ( uint64_t j= 0; j < processes_size; j++ ) {

                uint64_t process;
                MPI_Unpack( buffer, buffer_size, &buffer_pos, &process, 1,
                            MPI_LONG_LONG_INT, MPI_COMM_WORLD );

                alldata.grouping.insert( group, process );

            }

        }

    }

    delete[] buffer;
}
#endif /* OTFPROFILE_MPI */


bool SummarizeData( AllData& alldata ) {

    bool error= false;

    /* start runtime measurement for summarizing data */
    StartMeasurement( alldata, 1, true, "summarize data" );

    /* rank 0 gets grouping information */

    if ( 0 == alldata.myRank ) {

        get_grouping( alldata );

    }

#ifdef OTFPROFILE_MPI
    /* share grouping information to workers */

    if ( 1 < alldata.numRanks ) {

        share_grouping( alldata );

    }
#endif /* OTFPROFILE_MPI */

    /* macro to set min, max to sum before summarizing */
#   define MINMAX2SUM(v) \
    if( 0 != (v).cnt ) { \
        (v).cnt = 1; \
        (v).min= (v).max= (v).sum; \
    } else { \
        (v).cnt = 0; \
        /* (v).min= OTF_UINT64_MAX; (v).max= 0; \
           ^^^ this is set already by the constructor and never touched \
           if (v).cnt == 0. Therefore, it is ignored when computing min/max \
           further on. */ \
    }

    /* summarize map ( rank x func ) to map ( func ) */
    {
        map< Pair, FunctionData, ltPair >::const_iterator it= alldata.functionMapPerRank.begin();
        map< Pair, FunctionData, ltPair >::const_iterator itend= alldata.functionMapPerRank.end();
        while ( itend != it ) {

            const uint64_t& func= it->first.b;

            alldata.functionMapGlobal[ func ].add( it->second );
            it++;
        }

        /* in case of additional clustering or producing CSV output do not
        clear map ( rank x func ) because it is needed later */
        if ( !alldata.params.clustering.enabled &&
             !alldata.params.create_csv ) {

            alldata.functionMapPerRank.clear();
        }

        if(alldata.params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
        {
            map< TripleCallpath, FunctionData, ltTripleCallpath >::const_iterator it= alldata.functionCallpathMapPerRank.begin();
            map< TripleCallpath, FunctionData, ltTripleCallpath >::const_iterator itend= alldata.functionCallpathMapPerRank.end();
            while ( itend != it ) {

                const uint64_t& func= it->first.c;
                const string callpath= it->first.b;

                alldata.functionCallpathMapGlobal[ PairCallpath(func,callpath) ].add( it->second );
                it++;
            }

            /* in case of additional clustering or producing CSV output do not
            clear map ( rank x func ) because it is needed later */
            if ( !alldata.params.clustering.enabled &&
                 !alldata.params.create_csv ) {

                alldata.functionCallpathMapPerRank.clear();
            }
        }
    }
  
    /* summarize map ( rank x func x counter ) to map ( counter x func ) */
    {
        map< Triple, CounterData, ltTriple >::const_iterator it= alldata.counterMapPerFunctionRank.begin();
        map< Triple, CounterData, ltTriple >::const_iterator itend= alldata.counterMapPerFunctionRank.end();
        while ( itend != it ) {

            const uint64_t& func= it->first.b;
            const uint64_t& counter= it->first.c;

            alldata.counterMapGlobal[ Pair( counter, func ) ].add( it->second );
            it++;
        }

        /* in case of producing CSV output do not clear
        map ( rank x func x counter ) because it is needed later */
        if ( !alldata.params.create_csv ) {

            alldata.counterMapPerFunctionRank.clear();
        }
    }

    /* summarize map ( rank x rank ) to map ( group x group ) */
    {
        map< Pair, MessageData, ltPair >::const_iterator it= alldata.messageMapPerRankPair.begin();
        map< Pair, MessageData, ltPair >::const_iterator itend= alldata.messageMapPerRankPair.end();
        while ( itend != it ) {

            uint64_t group_a= it->first.a;
            uint64_t group_b= it->first.b;

            /* get copy of message data in order to keep original data
            unchanged for CSV output */
            MessageData data= it->second;

            if ( alldata.grouping.enabled ) {

                /* convert process IDs to group IDs */

                group_a= alldata.grouping.process2group( group_a );
                assert( 0 != group_a );
                group_b= alldata.grouping.process2group( group_b );
                assert( 0 != group_b );

            }

            MINMAX2SUM( data.count_send );
            MINMAX2SUM( data.count_recv );
            MINMAX2SUM( data.bytes_send );
            MINMAX2SUM( data.bytes_recv );
            MINMAX2SUM( data.duration_send );
            MINMAX2SUM( data.duration_recv );

            alldata.messageMapPerGroupPair[ Pair( group_a, group_b ) ].add( data );
            it++;
        }
        alldata.messageMapPerRankPair.clear();
    }

    /* summarize map ( rank ) to map ( group ) */
    {
        map< uint64_t, MessageData >::const_iterator it= alldata.messageMapPerRank.begin();
        map< uint64_t, MessageData >::const_iterator itend= alldata.messageMapPerRank.end();
        while ( itend != it ) {

            uint64_t group= it->first;

            /* get copy of message data in order to keep original data
            unchanged for CSV output */
            MessageData data= it->second;

            if ( alldata.grouping.enabled ) {

                /* convert process ID to group ID */
                group= alldata.grouping.process2group( group );
                assert( 0 != group );

            }

            MINMAX2SUM( data.count_send );
            MINMAX2SUM( data.count_recv );
            MINMAX2SUM( data.bytes_send );
            MINMAX2SUM( data.bytes_recv );
            MINMAX2SUM( data.duration_send );
            MINMAX2SUM( data.duration_recv );

            alldata.messageMapPerGroup[ group ].add( data );
            it++;
        }

        /* in case of producing CSV output do not clear map ( rank )
        because it is needed later */
        if ( !alldata.params.create_csv ) {

            alldata.messageMapPerRank.clear();
        }
    }

    /* summarize map ( rank x class ) to map ( class x group ) */
    {
        map< Pair, CollectiveData, ltPair >::iterator it= alldata.collectiveMapPerRank.begin();
        map< Pair, CollectiveData, ltPair >::iterator itend= alldata.collectiveMapPerRank.end();
        while ( itend != it ) {

            uint64_t group= it->first.a;
            const uint64_t& op_class= it->first.b;

            /* get copy of collective op. data in order to keep original data
            unchanged for CSV output */
            CollectiveData data= it->second;

            if ( alldata.grouping.enabled ) {

                /* convert process ID to group ID */
                group= alldata.grouping.process2group( group );
                assert( 0 != group );

            }

            MINMAX2SUM( data.count_send );
            MINMAX2SUM( data.count_recv );
            MINMAX2SUM( data.bytes_send );
            MINMAX2SUM( data.bytes_recv );
            MINMAX2SUM( data.duration_send );
            MINMAX2SUM( data.duration_recv );

            alldata.collectiveMapPerGroup[ Pair( op_class, group ) ].add( data );
            it++;
        }

        /* in case of producing CSV output do not clear map ( class x rank )
        because it is needed later */
        if ( !alldata.params.create_csv ) {

            alldata.collectiveMapPerRank.clear();
        }
    }

#ifdef OTFPROFILE_MPI
    /* synchronize error indicator with workers */
    /*SyncError( alldata, error );*/
#endif /* OTFPROFILE_MPI */

    if ( !error ) {

        /* stop runtime measurement for summarizing data */
        StopMeasurement( alldata, true, "summarize data" );
    }

    return !error;
}

bool SummarizeDataDispersion( AllData& alldata ) {
    
    bool error= false;
    
    /* start runtime measurement for summarizing dispersion data */
    StartMeasurement( alldata, 1, true, "summarize dispersion data" );
    
    VerbosePrint( alldata, 1, true, "summarize dispersion data\n" );
    
    /* summarize map ( rank x func x bin ) to map (  func x bin ) */
    {
        map< Triple, FunctionData, ltTriple>::const_iterator it= alldata.functionDurationSectionMapPerRank.begin();
        map< Triple, FunctionData, ltTriple>::const_iterator itend= alldata.functionDurationSectionMapPerRank.end();
        while ( itend != it ) {
     
            const uint64_t& func= it->first.b;
            const uint64_t& bin= it->first.c;
     
            alldata.functionDurationSectionMapGlobal[ Pair( func, bin ) ].add( it->second );
            it++;
        }

        if(alldata.params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
        {
            map< Quadruple, FunctionData, ltQuadruple>::const_iterator itc= alldata.functionDurationSectionCallpathMapPerRank.begin();
            map< Quadruple, FunctionData, ltQuadruple>::const_iterator itendc= alldata.functionDurationSectionCallpathMapPerRank.end();
            while ( itendc != itc ) {

                const uint64_t& func= itc->first.b;
                const string callpath= itc->first.c;
                const uint64_t& bin= itc->first.d;

                alldata.functionDurationSectionCallpathMapGlobal[ TripleCallpath( func, callpath, bin ) ].add( itc->second );
                itc++;
            }
        }
        /* in case of producing CSV output do not clear map ( rank x func x bin )
         because it is needed later */
        if ( !alldata.params.create_csv ) {    
            
            alldata.functionDurationSectionMapPerRank.clear();
            alldata.functionDurationSectionCallpathMapPerRank.clear();
        }
    }

#ifdef OTFPROFILE_MPI
    /* synchronize error indicator with workers */
    /*SyncError( alldata, error );*/
#endif /* OTFPROFILE_MPI */
    
    //cerr << " Size of functionDurationSectionMapGlobal: " << alldata.functionDurationSectionMapGlobal.size() << endl;
    
    if ( !error ) {
        
        /* stop runtime measurement for summarizing dispersion data */
        StopMeasurement( alldata, true, "summarize dispersion data" );
    }
    
    return !error;
}

