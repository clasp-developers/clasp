/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
*/

#include <cassert>
#include <iostream>

#include "otfprofile.h"
#include "reduce_data.h"


using namespace std;


/* fence between statistics parts within the buffer for consistency checking */
enum { FENCE= 0xDEADBEEF };


/* pack the local alldata into a buffer, return buffer */
static char* pack_worker_data( AllData& alldata, uint32_t sizes[12] ) {

    uint64_t fence= FENCE;
    uint32_t num_fences= 1;

    /* get the sizes of all parts that need to be transmitted */

    for ( uint32_t i= 1; i < 12; i++ ) {

        sizes[i]= 0;
    }

    if ( alldata.params.create_tex ) {

        sizes[1]= alldata.functionMapGlobal.size(); /* map< uint64_t, FunctionData > functionMapGlobal; */
        num_fences++;
        sizes[2]= alldata.functionDurationSectionMapGlobal.size(); /* map< Pair, FunctionData > functionDurationSectionMapGlobal; */
        num_fences++;
        sizes[3]= alldata.counterMapGlobal.size(); /* map< Pair, CounterData, ltPair > counterMapGlobal; */
        num_fences++;
        sizes[4]= alldata.messageMapPerGroupPair.size(); /* map< Pair, MessageData, ltPair > messageMapPerGroupPair; */
        num_fences++;
        sizes[5]= alldata.messageMapPerGroup.size(); /* map< uint64_t, MessageData > messageMapPerGroup; */
        num_fences++;
        sizes[6]= alldata.messageSpeedMapPerLength.size(); /* map< Pair, MessageSpeedData, ltPair > messageSpeedMapPerLength; */
        num_fences++;
        sizes[7]= alldata.collectiveMapPerGroup.size(); /* map< Pair, CollectiveData, ltPair > collectiveMapPerGroup; */
        num_fences++;
        if(alldata.params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
        {
            sizes[10]= alldata.functionDurationSectionCallpathMapGlobal.size(); /* map< TripleCallpath, FunctionData, ltTripleCallpath > functionDurationSectionCallpathMapGlobal; */
            num_fences++;
            sizes[11]= alldata.functionCallpathMapGlobal.size(); /* map< PairCallpath, FunctionData, ltPairCallpath > */
            num_fences++;
        }
    }

    if ( alldata.params.clustering.enabled ) {

        sizes[8]= alldata.functionMapPerRank.size(); /* map< Pair, FunctionData, ltPair > */
        num_fences++;
    }

    if ( alldata.params.dispersion.enabled) {
        
        sizes[9]= alldata.functionMinMaxLocationMap.size(); /* map< uint64_t, FunctionMinMaxLocactionData > functionMinMaxLocationMap; */
        num_fences++;
    }

    /* get bytesize multiplying all pieces */

    uint32_t bytesize= 0;
    int s1, s2;

    MPI_Pack_size( num_fences, MPI_LONG_LONG_INT, MPI_COMM_WORLD, &s1 );
    bytesize += s1;

    MPI_Pack_size( sizes[1] * 7, MPI_LONG_LONG_INT, MPI_COMM_WORLD, &s1 );
    MPI_Pack_size( sizes[1] * 6, MPI_DOUBLE, MPI_COMM_WORLD, &s2 );
    bytesize += s1 + s2;

    MPI_Pack_size( sizes[2] * 8, MPI_LONG_LONG_INT, MPI_COMM_WORLD, &s1 );
    MPI_Pack_size( sizes[2] * 6, MPI_DOUBLE, MPI_COMM_WORLD, &s2 );
    bytesize += s1 + s2;
    
    MPI_Pack_size( sizes[3] * 8, MPI_LONG_LONG_INT, MPI_COMM_WORLD, &s1 );
    MPI_Pack_size( sizes[3] * 6, MPI_DOUBLE, MPI_COMM_WORLD, &s2 );
    bytesize += s1 + s2;

    MPI_Pack_size( sizes[4] * 20, MPI_LONG_LONG_INT, MPI_COMM_WORLD, &s1 );
    MPI_Pack_size( sizes[4] * 6, MPI_DOUBLE, MPI_COMM_WORLD, &s2 );
    bytesize += s1 + s2;

    MPI_Pack_size( sizes[5] * 19, MPI_LONG_LONG_INT, MPI_COMM_WORLD, &s1 );
    MPI_Pack_size( sizes[5] * 6, MPI_DOUBLE, MPI_COMM_WORLD, &s2 );
    bytesize += s1 + s2;

    MPI_Pack_size( sizes[6] * 6, MPI_LONG_LONG_INT, MPI_COMM_WORLD, &s1 );
    bytesize += s1;

    MPI_Pack_size( sizes[7] * 20, MPI_LONG_LONG_INT, MPI_COMM_WORLD, &s1 );
    MPI_Pack_size( sizes[7] * 6, MPI_DOUBLE, MPI_COMM_WORLD, &s2 );
    bytesize += s1 + s2;

    MPI_Pack_size( sizes[8] * 8, MPI_LONG_LONG_INT, MPI_COMM_WORLD, &s1 );
    MPI_Pack_size( sizes[8] * 6, MPI_DOUBLE, MPI_COMM_WORLD, &s2 );
    bytesize += s1 + s2;

    MPI_Pack_size( sizes[9] * 7, MPI_LONG_LONG_INT, MPI_COMM_WORLD, &s1 );
    MPI_Pack_size( sizes[9] * 0, MPI_DOUBLE, MPI_COMM_WORLD, &s2 );
    bytesize += s1 + s2;


    MPI_Pack_size( sizes[10] * 9, MPI_LONG_LONG_INT, MPI_COMM_WORLD, &s1 );
    MPI_Pack_size( sizes[10] * 6, MPI_DOUBLE, MPI_COMM_WORLD, &s2 );
    bytesize += s1 + s2;

    if(alldata.params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
    {
        map< TripleCallpath, FunctionData, ltTripleCallpath >::const_iterator it=    alldata.functionDurationSectionCallpathMapGlobal.begin();
        map< TripleCallpath, FunctionData, ltTripleCallpath >::const_iterator itend= alldata.functionDurationSectionCallpathMapGlobal.end();

        for ( ; it != itend; ++it ) {
            MPI_Pack_size( it->first.b.length(), MPI_CHAR, MPI_COMM_WORLD, &s1 );
            bytesize += s1;
        }

        MPI_Pack_size( sizes[11] * 8, MPI_LONG_LONG_INT, MPI_COMM_WORLD, &s1 );
        MPI_Pack_size( sizes[11] * 6, MPI_DOUBLE, MPI_COMM_WORLD, &s2 );
        bytesize += s1 + s2;

        {
            map< PairCallpath, FunctionData, ltPairCallpath >::const_iterator it=    alldata.functionCallpathMapGlobal.begin();
            map< PairCallpath, FunctionData, ltPairCallpath >::const_iterator itend= alldata.functionCallpathMapGlobal.end();
            for ( ; it != itend; ++it ) {
                MPI_Pack_size( it->second.callpath.length(), MPI_CHAR, MPI_COMM_WORLD, &s1 );
                bytesize += s1;
            }
        }
    }

    /* get the buffer */
    sizes[0]= bytesize;
    char* buffer= alldata.guaranteePackBuffer( bytesize );

    /* pack parts */
    int position= 0;

    /* extra check that doesn't cost too much */
    MPI_Pack( (void*) &fence, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

    if ( alldata.params.create_tex ) {

        /* pack functionMapGlobal */
        {
            map< uint64_t, FunctionData >::const_iterator it=    alldata.functionMapGlobal.begin();
            map< uint64_t, FunctionData >::const_iterator itend= alldata.functionMapGlobal.end();
            for ( ; it != itend; ++it ) {

                MPI_Pack( (void*) &it->first,                1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.count.min,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count.max,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count.sum,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count.cnt,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.excl_time.min, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.excl_time.max, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.excl_time.sum, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.excl_time.cnt, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.incl_time.min, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.incl_time.max, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.incl_time.sum, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.incl_time.cnt, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            }
            alldata.functionMapGlobal.clear();
        }

        /* extra check that doesn't cost too much */
        MPI_Pack( (void*) &fence, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

        if(alldata.params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
        {
        /* pack functionCallpathMapGlobal */
                {
                    map< PairCallpath, FunctionData, ltPairCallpath >::const_iterator it=    alldata.functionCallpathMapGlobal.begin();
                    map< PairCallpath, FunctionData, ltPairCallpath>::const_iterator itend= alldata.functionCallpathMapGlobal.end();
                    uint64_t len;
                    for ( ; it != itend; ++it ) {
                    	len = it->second.callpath.length();
                        MPI_Pack( (void*) &it->first.a,                1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                        MPI_Pack( (void*) &len,                1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                        MPI_Pack( (void*) &it->second.count.min,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                        MPI_Pack( (void*) &it->second.count.max,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                        MPI_Pack( (void*) &it->second.count.sum,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                        MPI_Pack( (void*) &it->second.count.cnt,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                         MPI_Pack( (void*) &it->second.excl_time.min, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                         MPI_Pack( (void*) &it->second.excl_time.max, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                         MPI_Pack( (void*) &it->second.excl_time.sum, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                         MPI_Pack( (void*) &it->second.excl_time.cnt, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                         MPI_Pack( (void*) &it->second.incl_time.min, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                         MPI_Pack( (void*) &it->second.incl_time.max, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                         MPI_Pack( (void*) &it->second.incl_time.sum, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                         MPI_Pack( (void*) &it->second.incl_time.cnt, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                         MPI_Pack( (void*) it->first.b.c_str(), len, MPI_CHAR, buffer, bytesize, &position, MPI_COMM_WORLD );
                     }
                    alldata.functionCallpathMapGlobal.clear();
                }

                /* extra check that doesn't cost too much */
                MPI_Pack( (void*) &fence, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
        }
        /* pack functionDurationSectionMapGlobal */
        {
            map< Pair, FunctionData, ltPair >::const_iterator it=    alldata.functionDurationSectionMapGlobal.begin();
            map< Pair, FunctionData, ltPair >::const_iterator itend= alldata.functionDurationSectionMapGlobal.end();
            for ( ; it != itend; ++it ) {
                
                MPI_Pack( (void*) &it->first.a,              1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->first.b,              1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                
                MPI_Pack( (void*) &it->second.count.min,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count.max,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count.sum,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count.cnt,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                
                MPI_Pack( (void*) &it->second.excl_time.min, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.excl_time.max, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.excl_time.sum, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.excl_time.cnt, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                
                MPI_Pack( (void*) &it->second.incl_time.min, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.incl_time.max, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.incl_time.sum, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.incl_time.cnt, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            }
            alldata.functionDurationSectionMapGlobal.clear();           
            
        }

        /* extra check that doesn't cost too much */
        MPI_Pack( (void*) &fence, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
        
        /* pack counterMapGlobal */
        {
            map< Pair, CounterData, ltPair >::const_iterator it=    alldata.counterMapGlobal.begin();
            map< Pair, CounterData, ltPair >::const_iterator itend= alldata.counterMapGlobal.end();
            for ( ; it != itend; ++it ) {

                MPI_Pack( (void*) &it->first.a,              1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->first.b,              1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.count.min,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count.max,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count.sum,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count.cnt,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.excl_time.min, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.excl_time.max, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.excl_time.sum, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.excl_time.cnt, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.incl_time.min, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.incl_time.max, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.incl_time.sum, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.incl_time.cnt, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            }
            alldata.counterMapGlobal.clear();
        }

        /* extra check that doesn't cost too much */
        MPI_Pack( (void*) &fence, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

        /* pack messageMapPerGroupPair  */
        {
            map< Pair, MessageData, ltPair >::const_iterator it=    alldata.messageMapPerGroupPair.begin();
            map< Pair, MessageData, ltPair >::const_iterator itend= alldata.messageMapPerGroupPair.end();
            for ( ; it != itend; ++it ) {

                MPI_Pack( (void*) &it->first.a,                  1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->first.b,                  1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.count_send.min,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count_send.max,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count_send.sum,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count_send.cnt,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.count_recv.min,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count_recv.max,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count_recv.sum,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count_recv.cnt,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.bytes_send.min,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.bytes_send.max,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.bytes_send.sum,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.bytes_send.cnt,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.bytes_recv.min,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.bytes_recv.max,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.bytes_recv.sum,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.bytes_recv.cnt,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.duration_send.min, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.duration_send.max, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.duration_send.sum, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.duration_send.cnt, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.duration_recv.min, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.duration_recv.max, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.duration_recv.sum, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.duration_recv.cnt, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            }
            alldata.messageMapPerGroupPair.clear();
        }

        /* extra check that doesn't cost too much */
        MPI_Pack( (void*) &fence, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

        /* pack messageMapPerGroup  */
        {
            map< uint64_t, MessageData >::const_iterator it=    alldata.messageMapPerGroup.begin();
            map< uint64_t, MessageData >::const_iterator itend= alldata.messageMapPerGroup.end();
            for ( ; it != itend; ++it ) {

                MPI_Pack( (void*) &it->first,                    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.count_send.min,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count_send.max,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count_send.sum,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count_send.cnt,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.count_recv.min,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count_recv.max,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count_recv.sum,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count_recv.cnt,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.bytes_send.min,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.bytes_send.max,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.bytes_send.sum,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.bytes_send.cnt,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.bytes_recv.min,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.bytes_recv.max,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.bytes_recv.sum,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.bytes_recv.cnt,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.duration_send.min, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.duration_send.max, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.duration_send.sum, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.duration_send.cnt, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.duration_recv.min, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.duration_recv.max, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.duration_recv.sum, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.duration_recv.cnt, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            }
            alldata.messageMapPerGroup.clear();
        }

        /* extra check that doesn't cost too much */
        MPI_Pack( (void*) &fence, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

        /* pack messageSpeedMapPerLength */
        {
            map< Pair, MessageSpeedData, ltPair >::const_iterator it=    alldata.messageSpeedMapPerLength.begin();
            map< Pair, MessageSpeedData, ltPair >::const_iterator itend= alldata.messageSpeedMapPerLength.end();
            for ( ; it != itend; ++it ) {

                MPI_Pack( (void*) &it->first.a,          1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->first.b,          1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.count.min, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count.max, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count.sum, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count.cnt, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            }
            alldata.messageSpeedMapPerLength.clear();
        }

        /* extra check that doesn't cost too much */
        MPI_Pack( (void*) &fence, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

        /* pack collectiveMapPerGroup */
        {
            map< Pair, CollectiveData, ltPair >::const_iterator it=    alldata.collectiveMapPerGroup.begin();
            map< Pair, CollectiveData, ltPair >::const_iterator itend= alldata.collectiveMapPerGroup.end();
            for ( ; it != itend; ++it ) {

                MPI_Pack( (void*) &it->first.a,                  1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->first.b,                  1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.count_send.min,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count_send.max,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count_send.sum,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count_send.cnt,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.count_recv.min,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count_recv.max,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count_recv.sum,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.count_recv.cnt,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.bytes_send.min,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.bytes_send.max,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.bytes_send.sum,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.bytes_send.cnt,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.bytes_recv.min,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.bytes_recv.max,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.bytes_recv.sum,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.bytes_recv.cnt,    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.duration_send.min, 1, MPI_DOUBLE, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.duration_send.max, 1, MPI_DOUBLE, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.duration_send.sum, 1, MPI_DOUBLE, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.duration_send.cnt, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                MPI_Pack( (void*) &it->second.duration_recv.min, 1, MPI_DOUBLE, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.duration_recv.max, 1, MPI_DOUBLE, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.duration_recv.sum, 1, MPI_DOUBLE, buffer, bytesize, &position, MPI_COMM_WORLD );
                MPI_Pack( (void*) &it->second.duration_recv.cnt, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            }
            alldata.collectiveMapPerGroup.clear();
        }

        /* extra check that doesn't cost too much */
        MPI_Pack( (void*) &fence, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );


        if(alldata.params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
        {
            /* pack functionDurationSectionCallpathMapGlobal*/

            {
                map< TripleCallpath, FunctionData, ltTripleCallpath >::const_iterator it=    alldata.functionDurationSectionCallpathMapGlobal.begin();
                map< TripleCallpath, FunctionData, ltTripleCallpath >::const_iterator itend= alldata.functionDurationSectionCallpathMapGlobal.end();
                uint64_t len = 0;

                for ( ; it != itend; ++it ) {
                    len = it->second.callpath.length();
                    MPI_Pack( (void*) &it->first.a,              1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                    MPI_Pack( (void*) &len,                      1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                    MPI_Pack( (void*) &it->first.c,              1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                    MPI_Pack( (void*) &it->second.count.min,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                    MPI_Pack( (void*) &it->second.count.max,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                    MPI_Pack( (void*) &it->second.count.sum,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                    MPI_Pack( (void*) &it->second.count.cnt,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                    MPI_Pack( (void*) &it->second.excl_time.min, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                    MPI_Pack( (void*) &it->second.excl_time.max, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                    MPI_Pack( (void*) &it->second.excl_time.sum, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                    MPI_Pack( (void*) &it->second.excl_time.cnt, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

                    MPI_Pack( (void*) &it->second.incl_time.min, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                    MPI_Pack( (void*) &it->second.incl_time.max, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                    MPI_Pack( (void*) &it->second.incl_time.sum, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
                    MPI_Pack( (void*) &it->second.incl_time.cnt, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
                    MPI_Pack( (void*) it->second.callpath.c_str(), len, MPI_CHAR, buffer, bytesize, &position, MPI_COMM_WORLD );
                }
                alldata.functionDurationSectionMapGlobal.clear();

            }

            /* extra check that doesn't cost too much */
            MPI_Pack( (void*) &fence, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
        }
    }

    if ( alldata.params.clustering.enabled ) {

        /* pack functionMapPerRank */

        map< Pair, FunctionData, ltPair >::const_iterator it= alldata.functionMapPerRank.begin();
        map< Pair, FunctionData, ltPair >::const_iterator itend= alldata.functionMapPerRank.end();
        for ( ; it != itend; ++it ) {

            MPI_Pack( (void*) &it->first.a,              1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->first.b,              1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

            MPI_Pack( (void*) &it->second.count.min,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.count.max,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.count.sum,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.count.cnt,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

            MPI_Pack( (void*) &it->second.excl_time.min, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.excl_time.max, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.excl_time.sum, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.excl_time.cnt, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

            MPI_Pack( (void*) &it->second.incl_time.min, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.incl_time.max, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.incl_time.sum, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.incl_time.cnt, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
        }

        /* in case of producing CSV output do not clear map because it is
        needed later */
        if ( !alldata.params.create_csv ) {

            alldata.functionMapPerRank.clear();
        }

        /* extra check that doesn't cost too much */
        MPI_Pack( (void*) &fence, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
    }

    if ( alldata.params.clustering.enabled ) {

        /* pack functionCallpathMapPerRank */

        map< TripleCallpath, FunctionData, ltTripleCallpath >::const_iterator it= alldata.functionCallpathMapPerRank.begin();
        map< TripleCallpath, FunctionData, ltTripleCallpath >::const_iterator itend= alldata.functionCallpathMapPerRank.end();
        uint64_t len=0;
        for ( ; it != itend; ++it ) {
        	len = it->first.b.length();
            MPI_Pack( (void*) &it->first.a,              1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &len,              1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->first.c,              1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

            MPI_Pack( (void*) &it->second.count.min,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.count.max,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.count.sum,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.count.cnt,     1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

            MPI_Pack( (void*) &it->second.excl_time.min, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.excl_time.max, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.excl_time.sum, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.excl_time.cnt, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );

            MPI_Pack( (void*) &it->second.incl_time.min, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.incl_time.max, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.incl_time.sum, 1, MPI_DOUBLE,        buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.incl_time.cnt, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) it->first.b.c_str(), len, MPI_CHAR, buffer, bytesize, &position, MPI_COMM_WORLD );

        }

        /* in case of producing CSV output do not clear map because it is
        needed later */
        if ( !alldata.params.create_csv ) {

            alldata.functionCallpathMapPerRank.clear();
        }

        /* extra check that doesn't cost too much */
        MPI_Pack( (void*) &fence, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
    }
    
    if ( alldata.params.dispersion.enabled ) {
        
        /* pack functionMinMaxLocationMap */
        
        map<uint64_t, FunctionMinMaxLocationData>::const_iterator it= alldata.functionMinMaxLocationMap.begin();
        map<uint64_t, FunctionMinMaxLocationData>::const_iterator itend= alldata.functionMinMaxLocationMap.end();
        
        for ( ; it != itend ; ++it ) {
            
            MPI_Pack( (void*) &it->first,                    1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.location.min,      1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.location.max,      1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.location.loc_min,  1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.location.loc_max,  1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.location.time_max, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
            MPI_Pack( (void*) &it->second.location.time_max, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
        }
        
        /* extra check that doesn't cost too much */
        MPI_Pack( (void*) &fence, 1, MPI_LONG_LONG_INT, buffer, bytesize, &position, MPI_COMM_WORLD );
    }

    return buffer;
}


/* prepare alldata for unpack, return buffer of sufficient size */
static char* prepare_worker_data( AllData& alldata, uint32_t sizes[12] ) {

    uint32_t bytesize= sizes[0];

    return alldata.guaranteePackBuffer( bytesize );
}

/* unpack the received worker data and add it to the local alldata */
static void unpack_worker_data( AllData& alldata, uint32_t sizes[12] ) {

    uint64_t fence;

    /* unpack parts */
    int position= 0;
    char* buffer= alldata.getPackBuffer( );

    /* extra check that doesn't cost too much */
    fence= 0;
    MPI_Unpack( buffer, sizes[0], &position, &fence, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
    assert( FENCE == fence );


    /* chararray for unpacking the callpath */
    char* callpath = (char*) malloc(alldata.maxCallpathLength * sizeof(char));

    if ( alldata.params.create_tex ) {

        /* unpack functionMapGlobal */
        for ( uint32_t i= 0; i < sizes[1]; i++ ) {

            uint64_t func;
            FunctionData tmp;

            MPI_Unpack( buffer, sizes[0], &position, &func,              1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position, &tmp.count.min,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.count.max,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.count.sum,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.count.cnt,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.min, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.max, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.sum, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.min, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.max, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.sum, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            alldata.functionMapGlobal[ func ].add( tmp );
        }
        /* extra check that doesn't cost too much */
        fence= 0;
        MPI_Unpack( buffer, sizes[0], &position, &fence, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
        assert( FENCE == fence );
        if(alldata.params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
        {
            /* unpack functionCallpathMapGlobal */
               for ( uint32_t i= 0; i < sizes[11]; i++ ) {

                    uint64_t func;
                    uint64_t len;
                    FunctionData tmp;

                      MPI_Unpack( buffer, sizes[0], &position, &func,              1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
                      MPI_Unpack( buffer, sizes[0], &position, &len,              1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
                      MPI_Unpack( buffer, sizes[0], &position, &tmp.count.min,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
                      MPI_Unpack( buffer, sizes[0], &position, &tmp.count.max,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
                      MPI_Unpack( buffer, sizes[0], &position, &tmp.count.sum,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
                      MPI_Unpack( buffer, sizes[0], &position, &tmp.count.cnt,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

                      MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.min, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
                      MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.max, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
                      MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.sum, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
                      MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

                      MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.min, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
                      MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.max, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
                      MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.sum, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
                      MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

                      MPI_Unpack( buffer, sizes[0], &position, callpath, len, MPI_CHAR, MPI_COMM_WORLD );
                    tmp.callpath = callpath;
                    tmp.callpath = tmp.callpath.substr (0,len);
                    alldata.functionCallpathMapGlobal[ PairCallpath(func,tmp.callpath) ].add( tmp );
                }
                /* extra check that doesn't cost too much */
                fence= 0;
                MPI_Unpack( buffer, sizes[0], &position, &fence, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
                assert( FENCE == fence );
        }
        /* unpack functionDurationSectionMapGlobal */
        for ( uint32_t i= 0; i < sizes[2]; i++ ) {
            
            uint64_t func;
            uint64_t bin;
            FunctionData tmp;
            
            MPI_Unpack( buffer, sizes[0], &position, &func,              1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &bin,               1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            
            MPI_Unpack( buffer, sizes[0], &position, &tmp.count.min,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.count.max,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.count.sum,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.count.cnt,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            
            MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.min, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.max, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.sum, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            
            MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.min, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.max, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.sum, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            
            alldata.functionDurationSectionMapGlobal[ Pair( func, bin ) ].add( tmp );
        }
        
        /* extra check that doesn't cost too much */
        fence= 0;
        MPI_Unpack( buffer, sizes[0], &position, &fence, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
        assert( FENCE == fence );
        
        /* unpack counterMapGlobal */
        for ( uint32_t i= 0; i < sizes[3]; i++ ) {

            uint64_t a;
            uint64_t b;
            CounterData tmp;

            MPI_Unpack( buffer, sizes[0], &position, &a,                 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &b,                 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position, &tmp.count.min,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.count.max,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.count.sum,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.count.cnt,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.min, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.max, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.sum, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.min, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.max, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.sum, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            alldata.counterMapGlobal[ Pair( a, b ) ].add( tmp );
        }

        /* extra check that doesn't cost too much */
        fence= 0;
        MPI_Unpack( buffer, sizes[0], &position, &fence, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
        assert( FENCE == fence );

        /* unpack messageMapPerGroupPair */
        for ( uint32_t i= 0; i < sizes[4]; i++ ) {

            uint64_t a;
            uint64_t b;
            MessageData tmp;

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &a,                     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &b,                     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_send.min,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_send.max,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_send.sum,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_send.cnt,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_recv.min,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_recv.max,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_recv.sum,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_recv.cnt,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_send.min,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_send.max,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_send.sum,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_send.cnt,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_recv.min,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_recv.max,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_recv.sum,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_recv.cnt,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_send.min, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_send.max, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_send.sum, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_send.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_recv.min, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_recv.max, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_recv.sum, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_recv.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            alldata.messageMapPerGroupPair[ Pair(a,b) ].add( tmp );
        }

        /* extra check that doesn't cost too much */
        fence= 0;
        MPI_Unpack( buffer, sizes[0], &position, &fence, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
        assert( FENCE == fence );

        /* unpack messageMapPerGroup */
        for ( uint32_t i= 0; i < sizes[5]; i++ ) {

            uint64_t a;
            MessageData tmp;

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &a,                     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_send.min,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_send.max,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_send.sum,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_send.cnt,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_recv.min,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_recv.max,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_recv.sum,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_recv.cnt,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_send.min,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_send.max,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_send.sum,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_send.cnt,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_recv.min,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_recv.max,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_recv.sum,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_recv.cnt,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_send.min, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_send.max, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_send.sum, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_send.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_recv.min, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_recv.max, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_recv.sum, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_recv.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            alldata.messageMapPerGroup[ a ].add( tmp );
        }

        /* extra check that doesn't cost too much */
        fence= 0;
        MPI_Unpack( buffer, sizes[0], &position, &fence, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
        assert( FENCE == fence );

        /* unpack messageSpeedMapPerLength */
        for ( uint32_t i= 0; i < sizes[6]; i++ ) {

            uint64_t a;
            uint64_t b;
            MessageSpeedData tmp;

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &a,             1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &b,             1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count.min, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count.max, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count.sum, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            alldata.messageSpeedMapPerLength[ Pair(a,b) ].add( tmp );
        }

        /* extra check that doesn't cost too much */
        fence= 0;
        MPI_Unpack( buffer, sizes[0], &position, &fence, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
        assert( FENCE == fence );

        /* unpack collectiveMapPerGroup */
        for ( uint32_t i= 0; i < sizes[7]; i++ ) {

            uint64_t a;
            uint64_t b;
            CollectiveData tmp;

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &a,                     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &b,                     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_send.min,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_send.max,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_send.sum,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_send.cnt,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_recv.min,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_recv.max,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_recv.sum,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.count_recv.cnt,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_send.min,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_send.max,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_send.sum,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_send.cnt,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_recv.min,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_recv.max,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_recv.sum,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.bytes_recv.cnt,    1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_send.min, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_send.max, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_send.sum, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_send.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_recv.min, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_recv.max, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_recv.sum, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &tmp.duration_recv.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            alldata.collectiveMapPerGroup[ Pair(a,b) ].add( tmp );
        }

        /* extra check that doesn't cost too much */
        fence= 0;
        MPI_Unpack( buffer, sizes[0], &position, &fence, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
        assert( FENCE == fence );

        if(alldata.params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
        {
        /* unpack functionDurationSectionCallpathMapGlobal */
                for ( uint32_t i= 0; i < sizes[10]; i++ ) {

                    uint64_t func;
                    uint64_t bin;
                    uint64_t len;
                    FunctionData tmp;

                    MPI_Unpack( buffer, sizes[0], &position, &func,              1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
                    MPI_Unpack( buffer, sizes[0], &position, &len,              1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
                    MPI_Unpack( buffer, sizes[0], &position, &bin,               1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

                    MPI_Unpack( buffer, sizes[0], &position, &tmp.count.min,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
                    MPI_Unpack( buffer, sizes[0], &position, &tmp.count.max,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
                    MPI_Unpack( buffer, sizes[0], &position, &tmp.count.sum,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
                    MPI_Unpack( buffer, sizes[0], &position, &tmp.count.cnt,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

                    MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.min, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
                    MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.max, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
                    MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.sum, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
                    MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

                    MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.min, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
                    MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.max, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
                    MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.sum, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
                    MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

                    MPI_Unpack( buffer, sizes[0], &position, callpath, len, MPI_CHAR, MPI_COMM_WORLD );
                    tmp.callpath = callpath;
                    tmp.callpath = tmp.callpath.substr (0,len);
                    alldata.functionDurationSectionCallpathMapGlobal[ TripleCallpath( func, tmp.callpath,bin ) ].add( tmp );

                }

                fence= 0;
                MPI_Unpack( buffer, sizes[0], &position, &fence, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
                assert( FENCE == fence );
        }
    }

    if ( alldata.params.clustering.enabled ) {

        /* unpack functionMapPerRank */
        for ( uint32_t i= 0; i < sizes[8]; i++ ) {

            uint64_t a;
            uint64_t b;
            FunctionData tmp;

            MPI_Unpack( buffer, sizes[0], &position,  (void*) &a,        1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position,  (void*) &b,        1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position, &tmp.count.min,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.count.max,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.count.sum,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.count.cnt,     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.min, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.max, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.sum, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.excl_time.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.min, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.max, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.sum, 1, MPI_DOUBLE,        MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, &tmp.incl_time.cnt, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );

            alldata.functionMapPerRank[ Pair(a,b) ].add( tmp );
        }

        /* extra check that doesn't cost too much */
        fence= 0;
        MPI_Unpack( buffer, sizes[0], &position, &fence, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
        assert( FENCE == fence );
    }

    if ( alldata.params.dispersion.enabled) {
        
        /* unpack functionMinMaxLocationMap */
        for ( uint32_t i= 0;  i < sizes[9]; i++) {
            
            uint64_t a;
            FunctionMinMaxLocationData tmp;
            
            MPI_Unpack( buffer, sizes[0], &position, (void*) &a,                     1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, (void*) &tmp.location.min,      1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, (void*) &tmp.location.max,      1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, (void*) &tmp.location.loc_min,  1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, (void*) &tmp.location.loc_max,  1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, (void*) &tmp.location.time_min, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            MPI_Unpack( buffer, sizes[0], &position, (void*) &tmp.location.time_max, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
            
            alldata.functionMinMaxLocationMap[ a ].add(tmp);
        }
        
        /* extra check that doesn't cost too much */
        fence= 0;
        MPI_Unpack( buffer, sizes[0], &position, &fence, 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD );
        assert( FENCE == fence );      
    }
        /* free the callpath chararray */
        delete callpath;
}


bool ReduceData( AllData& alldata ) {

    bool error= false;

    assert( 1 < alldata.numRanks );

    /* start runtime measurement for reducing data */
    StartMeasurement( alldata, 1, true, "reduce data" );

    VerbosePrint( alldata, 1, true, "reducing data\n" );

    /* implement reduction myself because MPI and C++ STL don't play with
    each other */

    /* how many rounds until master has all the data? */
    uint32_t num_rounds= Logi( alldata.numRanks ) -1;
    uint32_t round_no= 0;
    uint32_t round= 1;
    while ( round < alldata.numRanks ) {

        round_no++;

        if ( 1 == alldata.params.verbose_level ) {

            VerbosePrint( alldata, 1, true, " round %u / %u\n",
                              round_no, num_rounds );
        }

        uint32_t peer= alldata.myRank ^ round;

        /* if peer rank is not there, do nothing but go on */
        if ( peer >= alldata.numRanks ) {

            round= round << 1;
            continue;
        }

        /* send to smaller peer, receive from larger one */
        uint32_t sizes[12];
        char* buffer;

        if ( alldata.myRank < peer ) {

            MPI_Status status;

            MPI_Recv( sizes, 12, MPI_UNSIGNED, peer, 4, MPI_COMM_WORLD,
                      &status );

            // DEBUG
            //cout << "    round " << round << " recv " << peer << "--> " <<
            //alldata.myRank << " with " <<
            //sizes[0] << " bytes, " <<
            //sizes[1] << ", " <<
            //sizes[2] << ", " <<
            //sizes[3] << ", " <<
            //sizes[4] << "" << endl << flush;

            buffer= prepare_worker_data( alldata, sizes );

            VerbosePrint( alldata, 2, false,
                          "round %u / %u: receiving %u bytes from rank %u\n",
                          round_no, num_rounds, sizes[0], peer );

            MPI_Recv( buffer, sizes[0], MPI_PACKED, peer, 5, MPI_COMM_WORLD,
                      &status );

            unpack_worker_data( alldata, sizes );

        } else {

            buffer= pack_worker_data( alldata, sizes );

            // DEBUG
            //cout << "    round " << round << " send " << alldata.myRank <<
            //" --> " << peer << " with " <<
            //sizes[0] << " bytes, " <<
            //sizes[1] << ", " <<
            //sizes[2] << ", " <<
            //sizes[3] << ", " <<
            //sizes[4] << "" << endl << flush;

            VerbosePrint( alldata, 2, false,
                          "round %u / %u: sending %u bytes to rank %u\n",
                          round_no, num_rounds, sizes[0], peer );

            MPI_Send( sizes, 12, MPI_UNSIGNED, peer, 4, MPI_COMM_WORLD );

            MPI_Send( buffer, sizes[0], MPI_PACKED, peer, 5,
                      MPI_COMM_WORLD );

            /* every work has to send off its data at most once,
            after that, break from the collective reduction operation */
            break;
        }

        round= round << 1;
    }

    alldata.freePackBuffer();

    /* synchronize error indicator with workers */
    /*SyncError( alldata, error );*/

    if ( !error ) {

        /* stop runtime measurement for reducing data */
        StopMeasurement( alldata, true, "reduce data" );
    }

    return !error;
}


bool ReduceDataDispersion( AllData& alldata ) {
    
    bool error= false;

    assert( 1 < alldata.numRanks );
    
    /* start runtime measurement for reducing data */
    StartMeasurement( alldata, 1, true, "reduce data dispersion" );
    
    VerbosePrint( alldata, 1, true, "reducing data dispersion\n" );


    /* implement reduction myself because MPI and C++ STL don't play with
     each other */
    
    /* how many rounds until master has all the data? */
    uint32_t num_rounds= Logi( alldata.numRanks ) -1;
    uint32_t round_no= 0;
    uint32_t round= 1;

    while ( round < alldata.numRanks ) {
        
        round_no++;
        
        if ( 1 == alldata.params.verbose_level ) {
            
            VerbosePrint( alldata, 1, true, " round %u / %u\n",
                         round_no, num_rounds );
        }
        
        uint32_t peer= alldata.myRank ^ round;
        
        /* if peer rank is not there, do nothing but go on */
        if ( peer >= alldata.numRanks ) {
            
            round= round << 1;
            continue;
        }
        
        /* send to smaller peer, receive from larger one */
        uint32_t sizes[12];
        char* buffer;
        
        if ( alldata.myRank < peer ) {
            
            MPI_Status status;
            
            MPI_Recv( sizes, 12, MPI_UNSIGNED, peer, 4, MPI_COMM_WORLD,
                     &status );
            
            // DEBUG
      /*      cout << "    round " << round << " recv " << peer << "--> " <<
            alldata.myRank << " with " <<
            sizes[0] << " bytes, " <<
            sizes[1] << ", " <<
            sizes[2] << ", " <<
            sizes[3] << ", " <<
            sizes[4] << "" << endl << flush;
        */
            buffer= prepare_worker_data( alldata, sizes );
            
            VerbosePrint( alldata, 2, false,
                         "round %u / %u: receiving %u bytes from rank %u\n",
                         round_no, num_rounds, sizes[0], peer );
            
            MPI_Recv( buffer, sizes[0], MPI_PACKED, peer, 5, MPI_COMM_WORLD,
                     &status );
            
            unpack_worker_data( alldata, sizes );
            
        } else {
            
            /* don't reduce function map global twice */ 
            alldata.functionMapGlobal.clear();
            if(alldata.params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
                alldata.functionCallpathMapGlobal.clear();
            
            buffer= pack_worker_data( alldata, sizes );
            
            // DEBUG
            //cout << "    round " << round << " send " << alldata.myRank <<
            //" --> " << peer << " with " <<
            //sizes[0] << " bytes, " <<
            //sizes[1] << ", " <<
            //sizes[2] << ", " <<
            //sizes[3] << ", " <<
            //sizes[4] << "" << endl << flush;
            
            VerbosePrint( alldata, 2, false,
                         "round %u / %u: sending %u bytes to rank %u\n",
                         round_no, num_rounds, sizes[0], peer );
            
            MPI_Send( sizes, 12, MPI_UNSIGNED, peer, 4, MPI_COMM_WORLD );
            
            MPI_Send( buffer, sizes[0], MPI_PACKED, peer, 5,
                     MPI_COMM_WORLD );
            
            /* every work has to send off its data at most once,
             after that, break from the collective reduction operation */
            break;
        }
        
        round= round << 1;
    }
    alldata.freePackBuffer();
    
    /* synchronize error indicator with workers */
    /*SyncError( alldata, error );*/
    
    if ( !error ) {
        
        /* stop runtime measurement for reducing data */
        StopMeasurement( alldata, true, "reduce data dispersion" );
    }
    
    return !error;
}
