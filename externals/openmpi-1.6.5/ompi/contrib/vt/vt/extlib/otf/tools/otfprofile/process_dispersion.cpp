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
#include <algorithm>


#include "otf.h"
#include "otfaux.h"

#include "process_dispersion.h"
#include "otfprofile.h"


using namespace std;


bool ProcessDispersion( AllData& alldata ) {

    bool error= false;
    /* start runtime measurement for process dispersion information */
    StartMeasurement( alldata, 1, true, "process dispersion information" );
    
    VerbosePrint( alldata, 1, true, "process dispersion information\n" );
	
    if ( 0 == alldata.myRank ) {

        int count;
        if (alldata.params.dispersion.enabled) {

            map<Pair, FunctionData, ltPair>::const_iterator it =
                    alldata.functionDurationSectionMapGlobal.begin();
            map<Pair, FunctionData, ltPair>::const_iterator itend =
                    alldata.functionDurationSectionMapGlobal.end();

            uint64_t funcid = it->first.a;

            map<uint64_t, FunctionData>::const_iterator iter_funcMapGlobal;
            iter_funcMapGlobal = alldata.functionMapGlobal.find(funcid);
            assert( iter_funcMapGlobal != alldata.functionMapGlobal.end() );

            uint64_t n = iter_funcMapGlobal->second.count.sum;

            uint64_t n_temp = 0;
            uint64_t n_25 = n / 4;
            uint64_t n_50 = n / 2;
            uint64_t n_75 = (3 * n) / 4;
            uint64_t n_95 = (19 * n) / 20;

            double t_min = iter_funcMapGlobal->second.DISPERSION_OPTION.min;
            double t_max = iter_funcMapGlobal->second.DISPERSION_OPTION.max;
            double t_sum = iter_funcMapGlobal->second.DISPERSION_OPTION.sum;

            double t_25 = 0.0;
            double t_50 = 0.0;
            double t_75 = 0.0;
            double t_95 = 0.0;
            count = 0;

            for (; it != itend; ++it)
            {
                count++;
                //cerr << " funcid " << funcid << endl;

                if (funcid != it->first.a)
                {

                    /*
                     cerr << " function: " << funcid << " , n: " << n <<
                     " , t_sum: " << t_sum << " , t_min: " << t_min <<
                     " , t_25: " << t_25 << " , t_50: " << t_50 <<
                     " , t_75: " << t_75 << " , t_max: " << t_max << endl;
                     */

                    alldata.functionDispersionMap[Pair((uint64_t) ((t_max
                            - t_75)), funcid)] = FunctionDispersionData(n,
                            t_sum, t_min, t_25, t_50, t_75, t_95, t_max);

                    alldata.functionDispersionCallpathMap[TripleCallpath(
                            (uint64_t) ((t_max - t_75)), "", funcid)]
                            = FunctionDispersionData(n, t_sum, t_min, t_25,
                                    t_50, t_75, t_95, t_max);

                    funcid = it->first.a;

                    iter_funcMapGlobal = alldata.functionMapGlobal.find(funcid);
                    assert( iter_funcMapGlobal != alldata.functionMapGlobal.end() );
                    n = iter_funcMapGlobal->second.count.sum;

                    n_temp = 0;
                    n_25 = n / 4;
                    n_50 = n / 2;
                    n_75 = (3 * n) / 4;
                    n_95 = (19 * n) / 20;
                    t_min = iter_funcMapGlobal->second.DISPERSION_OPTION.min;
                    t_max = iter_funcMapGlobal->second.DISPERSION_OPTION.max;
                    t_sum = iter_funcMapGlobal->second.DISPERSION_OPTION.sum;
                    t_25 = 0.0;
                    t_50 = 0.0;
                    t_75 = 0.0;
                    t_95 = 0.0;

                }

                n_temp += it->second.count.sum;

                /* determine lower quartile, median, and upper quartile */
                if (0.0 == t_95)
                {

                    if (n_temp >= n_95)
                    {
                        t_95 = (it->second.DISPERSION_OPTION.max
                                - it->second.DISPERSION_OPTION.min) / 2
                                + it->second.DISPERSION_OPTION.min;
                    }

                    if (0.0 == t_75)
                    {

                        if (n_temp >= n_75)
                        {
                            t_75 = (it->second.DISPERSION_OPTION.max
                                    - it->second.DISPERSION_OPTION.min) / 2
                                    + it->second.DISPERSION_OPTION.min;
                        }

                        if (0.0 == t_50)
                        {

                            if (n_temp >= n_50)
                            {
                                t_50 = (it->second.DISPERSION_OPTION.max
                                        - it->second.DISPERSION_OPTION.min) / 2
                                        + it->second.DISPERSION_OPTION.min;
                            }

                            if (0.0 == t_25)
                            {

                                if (n_temp >= n_25)
                                {
                                    t_25 = (it->second.DISPERSION_OPTION.max
                                            - it->second.DISPERSION_OPTION.min)
                                            / 2
                                            + it->second.DISPERSION_OPTION.min;
                                }

                            }

                        }

                    }
                }

            }

            alldata.functionDispersionMap[Pair((uint64_t) ((t_max - t_75)),
                    funcid)] = FunctionDispersionData(n, t_sum, t_min, t_25,
                    t_50, t_75, t_95, t_max);
            if (alldata.params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
            {
                alldata.functionDispersionCallpathMap[TripleCallpath(
                        (uint64_t) ((t_max - t_75)), "", funcid)]
                        = FunctionDispersionData(n, t_sum, t_min, t_25, t_50,
                                t_75, t_95, t_max);
            }
        }

        if ((alldata.params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
                && (alldata.params.dispersion.enabled))
        {
            map<uint64_t, uint64_t> dispBorder;

            map<TripleCallpath, uint32_t, ltTripleCallpath> function_filter;

            map<TripleCallpath, FunctionData, ltTripleCallpath>::const_iterator
                    it =
                            alldata.functionDurationSectionCallpathMapGlobal.begin();
            map<TripleCallpath, FunctionData, ltTripleCallpath>::const_iterator
                    itend =
                            alldata.functionDurationSectionCallpathMapGlobal.end();

            uint64_t funcid = it->first.a;
            string callpath = it->second.callpath;

            map<PairCallpath, FunctionData, ltPairCallpath>::const_iterator
                    iter_funcCallpathMapGlobal;
            iter_funcCallpathMapGlobal
                    = alldata.functionCallpathMapGlobal.find(PairCallpath(
                            funcid, callpath));
            assert( iter_funcCallpathMapGlobal != alldata.functionCallpathMapGlobal.end() );

            uint64_t n = iter_funcCallpathMapGlobal->second.count.sum;

            uint64_t n_temp = 0;
            uint64_t n_25 = n / 4;
            uint64_t n_50 = n / 2;
            uint64_t n_75 = (3 * n) / 4;
            uint64_t n_95 = (19 * n) / 20;

            double t_min =
                    iter_funcCallpathMapGlobal->second.DISPERSION_OPTION.min;
            double t_max =
                    iter_funcCallpathMapGlobal->second.DISPERSION_OPTION.max;
            double t_sum =
                    iter_funcCallpathMapGlobal->second.DISPERSION_OPTION.sum;

            double t_25 = 0.0;
            double t_50 = 0.0;
            double t_75 = 0.0;
            double t_95 = 0.0;

            count = 0;

            uint64_t callpathcount = 0;
            for (; it != itend; ++it)
            {
                count++;
                //cerr << " funcid " << funcid << endl;
                if (funcid != it->first.a || callpath != it->first.b)
                {

                    alldata.functionDispersionCallpathMap[TripleCallpath(
                            (uint64_t) ((t_max - t_75)), callpath, funcid)]
                            = FunctionDispersionData(n, t_sum, t_min, t_25,
                                    t_50, t_75, t_95, t_max);
                    function_filter[TripleCallpath((uint64_t) ((t_max - t_75)),
                            callpath, funcid)] = n;

                    funcid = it->first.a;
                    callpath = it->first.b;
                    iter_funcCallpathMapGlobal
                            = alldata.functionCallpathMapGlobal.find(
                                    PairCallpath(funcid, callpath));
                    assert( iter_funcCallpathMapGlobal != alldata.functionCallpathMapGlobal.end() );

                    if ( n > MIN_CPATH_COUNT ){

                        callpathcount += n;

                    }

                    n = iter_funcCallpathMapGlobal->second.count.sum;

                    n_temp = 0;
                    n_25 = n / 4;
                    n_50 = n / 2;
                    n_75 = (3 * n) / 4;
                    n_95 = (19 * n) / 20;
                    t_min
                            = iter_funcCallpathMapGlobal->second.DISPERSION_OPTION.min;
                    t_max
                            = iter_funcCallpathMapGlobal->second.DISPERSION_OPTION.max;
                    t_sum
                            = iter_funcCallpathMapGlobal->second.DISPERSION_OPTION.sum;
                    t_25 = 0.0;
                    t_50 = 0.0;
                    t_75 = 0.0;
                    t_95 = 0.0;
                }

                n_temp += it->second.count.sum;

                /* determine lower quartile, median, and upper quartile */
                if (0.0 == t_95)
                {

                    if (n_temp >= n_95)
                    {
                        t_95 = (it->second.DISPERSION_OPTION.max
                                - it->second.DISPERSION_OPTION.min) / 2
                                + it->second.DISPERSION_OPTION.min;
                    }

                    if (0.0 == t_75)
                    {

                        if (n_temp >= n_75)
                        {
                            t_75 = (it->second.DISPERSION_OPTION.max
                                    - it->second.DISPERSION_OPTION.min) / 2
                                    + it->second.DISPERSION_OPTION.min;
                        }

                        if (0.0 == t_50)
                        {

                            if (n_temp >= n_50)
                            {
                                t_50 = (it->second.DISPERSION_OPTION.max
                                        - it->second.DISPERSION_OPTION.min) / 2
                                        + it->second.DISPERSION_OPTION.min;
                            }

                            if (0.0 == t_25)
                            {

                                if (n_temp >= n_25)
                                {
                                    t_25 = (it->second.DISPERSION_OPTION.max
                                            - it->second.DISPERSION_OPTION.min)
                                            / 2
                                            + it->second.DISPERSION_OPTION.min;
                                }

                            }

                        }

                    }
                }

            }

            alldata.functionDispersionCallpathMap[TripleCallpath(
                    (uint64_t) ((t_max - t_75)), callpath, funcid)]
                    = FunctionDispersionData(n, t_sum, t_min, t_25, t_50, t_75,
                            t_95, t_max);

            function_filter[TripleCallpath((uint64_t) ((t_max - t_75)),
                    callpath, funcid)] = n;
            if (n > MIN_CPATH_COUNT){

                callpathcount += n;

            }

            {
                // set filter and dispersion border
                map<TripleCallpath, uint32_t, ltTripleCallpath>::const_iterator
                        iter = function_filter.begin();
                map<TripleCallpath, uint32_t, ltTripleCallpath>::const_iterator
                        iter_end = function_filter.end();


                uint32_t k=0;
                callpathcount = (uint64_t)((double)callpathcount *
                            ((double)alldata.params.dispersion.reduction/100.0));

                for (uint64_t i = 0; iter != iter_end; i++)
                {

                    if (k <= callpathcount && iter->second > MIN_CPATH_COUNT &&
                        ( std::count(iter->first.b.begin(), iter->first.b.end(), ' ')
                                 < RFG_FILTER_MAX_CPATH_SIZE )
                    ){
                   	k += iter->second;
                        alldata.functionDispersionCallpathMap[TripleCallpath(
                                iter->first.a, iter->first.b,
                                iter->first.c)].addFilterRule(0);

                    } else
                    {
                        dispBorder[iter->first.a] = iter->first.c;
                        if (dispBorder.size() > 50)
                            dispBorder.erase(--(dispBorder.end()));

                    }
                    iter++;
                }
            }

            alldata.dispersionMarkerBorder = (--(dispBorder.end()))->first;
        }

        if (alldata.params.create_csv)
        {

            map<Triple, FunctionData, ltTriple>::const_iterator it =
                    alldata.functionDurationSectionMapPerRank.begin();
            map<Triple, FunctionData, ltTriple>::const_iterator itend =
                    alldata.functionDurationSectionMapPerRank.end();

            uint64_t rank = it->first.a;
            uint64_t funcid = it->first.b;

            map<uint64_t, FunctionData>::const_iterator iter_funcMapGlobal;
            iter_funcMapGlobal = alldata.functionMapGlobal.find(funcid);
            assert( iter_funcMapGlobal != alldata.functionMapGlobal.end() );

            uint64_t n = iter_funcMapGlobal->second.count.sum;

            uint64_t n_temp = 0;
            uint64_t n_25 = n / 4;
            uint64_t n_50 = n / 2;
            uint64_t n_75 = (3 * n) / 4;
            uint64_t n_95 = (19 * n) / 20;

            double t_min = iter_funcMapGlobal->second.excl_time.min;
            double t_max = iter_funcMapGlobal->second.excl_time.max;
            double t_sum = iter_funcMapGlobal->second.excl_time.sum;

            double t_25 = 0.0;
            double t_50 = 0.0;
            double t_75 = 0.0;
            double t_95 = 0.0;
            for (; it != itend; ++it)
            {

                //cerr << " funcid " << funcid << endl;
                if (funcid != it->first.a)
                {

                    /*
                     cerr << " function: " << funcid << " , n: " << n <<
                     " , t_sum: " << t_sum << " , t_min: " << t_min <<
                     " , t_25: " << t_25 << " , t_50: " << t_50 <<
                     " , t_75: " << t_75 << " , t_max: " << t_max << endl;
                     */

                    alldata.functionDispersionMapPerRank[Triple(
                            (uint64_t) ((t_max / t_75) * 100), funcid, rank)]
                            = FunctionDispersionData(n, t_sum, t_min, t_25,
                                    t_50, t_75, t_95, t_max);

                    rank = it->first.a;
                    funcid = it->first.b;

                    iter_funcMapGlobal = alldata.functionMapGlobal.find(funcid);
                    assert( iter_funcMapGlobal != alldata.functionMapGlobal.end() );
                    n = iter_funcMapGlobal->second.count.sum;

                    n_temp = 0;
                    n_25 = n / 4;
                    n_50 = n / 2;
                    n_75 = (3 * n) / 4;
                    n_95 = (19 * n) / 20;
                    t_min = iter_funcMapGlobal->second.excl_time.min;
                    t_max = iter_funcMapGlobal->second.excl_time.max;
                    t_sum = iter_funcMapGlobal->second.excl_time.sum;
                    t_25 = 0.0;
                    t_50 = 0.0;
                    t_75 = 0.0;
                    t_95 = 0.0;
                }

                n_temp += it->second.count.sum;

                /* determine lower quartile, median, and upper quartile */
                if (0.0 == t_95)
                {

                    if (n_temp >= n_95)
                    {
                        t_95 = (it->second.excl_time.max
                                - it->second.excl_time.min) / 2
                                + it->second.excl_time.min;
                    }
                    if (0.0 == t_75)
                    {

                        if (n_temp >= n_75)
                        {
                            t_75 = (it->second.excl_time.max
                                    - it->second.excl_time.min) / 2
                                    + it->second.excl_time.min;
                        }

                        if (0.0 == t_50)
                        {

                            if (n_temp >= n_50)
                            {
                                t_50 = (it->second.excl_time.max
                                        - it->second.excl_time.min) / 2
                                        + it->second.excl_time.min;
                            }

                            if (0.0 == t_25)
                            {

                                if (n_temp >= n_25)
                                {
                                    t_25 = (it->second.excl_time.max
                                            - it->second.excl_time.min) / 2
                                            + it->second.excl_time.min;
                                }

                            }

                        }

                    }
                }

            }

            alldata.functionDispersionMapPerRank[Triple((uint64_t) ((t_max
                    / t_75) * 100), funcid, rank)] = FunctionDispersionData(n,
                    t_sum, t_min, t_25, t_50, t_75, t_95, t_max);

        }

    }
    //alldata.functionDurationSectionMapGlobal.clear();
    
    if ( !error ) {
        
        /* stop runtime measurement for process data */
        StopMeasurement( alldata, true, "process dispersion information" );
        
    }
    
    return !error;
}
