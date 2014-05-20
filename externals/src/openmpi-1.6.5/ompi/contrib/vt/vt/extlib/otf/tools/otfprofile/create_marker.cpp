/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
*/

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <unistd.h>

#include "otf.h"
#include "otfaux.h"

#include "create_marker.h"
#include "otfprofile.h"

using namespace std;

static uint32_t markerDispersionId= 0;
static uint32_t markerLastId= 0;

static int handle_def_marker( void* fha, uint32_t stream, uint32_t token,
                             const char* name, uint32_t type, 
                             OTF_KeyValueList* list) {
 
    OTF_WStream* writer= (OTF_WStream*) fha;
    
    if ( 0 == strcmp( "OTF_IRREGULARITY_HINT", name ) ) {
        
        markerDispersionId= token;
    }
    
    if ( token > markerLastId ) markerLastId= token;
    
    if ( 0 == OTF_WStream_writeDefMarkerKV( writer, token, name, type, list ) ) {
     
        cout << "Error while writing Marker Definition " << endl ;
    }
    
    return OTF_RETURN_OK;
}

static int handle_marker( void* fha, uint64_t time, uint32_t process, 
                     uint32_t token, const char* text, OTF_KeyValueList* list ) {
 
    OTF_WStream* writer= (OTF_WStream*) fha;
    
    
    return (markerDispersionId == token ) ? OTF_RETURN_OK : 
        OTF_WStream_writeMarkerKV( writer, time, process, token, text, list);
    
}

static bool read_markerDefinition( OTF_Reader* reader, OTF_WStream* writer ) {
 
    bool error= false;
    
    /* open OTF handler array */
    OTF_HandlerArray* handlers= OTF_HandlerArray_open( );
    assert( handlers );
    
    /* set record handler functions */
    
    OTF_HandlerArray_setHandler( handlers,
                                (OTF_FunctionPointer*) handle_def_marker,
                                OTF_DEFMARKER_RECORD );
    
    /* set record handler's first arguments */
    OTF_HandlerArray_setFirstHandlerArg( handlers, writer,
                                        OTF_DEFMARKER_RECORD );
    
    /* read marker definitions */
    uint64_t read_ret= OTF_Reader_readMarkers( reader, handlers );
    
    if ( OTF_READ_ERROR == read_ret ) {
        
        cerr << "ERROR: Could not read marker definitions." << endl;
        error= true;
        
    }
    
    /* close OTF handler array */
    OTF_HandlerArray_close( handlers );
    
    return !error;
    
}

static bool read_marker( OTF_Reader* reader, OTF_WStream* writer ) {
 
    bool error= false;
    
    /* open OTF handler array */
    OTF_HandlerArray* handlers= OTF_HandlerArray_open( );
    assert( handlers );
    
    /* set record handler functions */
    
    OTF_HandlerArray_setHandler( handlers,
                                (OTF_FunctionPointer*) handle_marker,
                                OTF_MARKER_RECORD );
    
    /* set record handler's first arguments */
    OTF_HandlerArray_setFirstHandlerArg( handlers, writer,
                                        OTF_MARKER_RECORD );
    
    /* read marker */
    uint64_t read_ret= OTF_Reader_readMarkers( reader, handlers );
    
    if ( OTF_READ_ERROR == read_ret ) {
        
        cerr << "ERROR: Could not read marker." << endl;
        error= true;
        
    }
    
    /* close OTF handler array */
    OTF_HandlerArray_close( handlers );
    
    return !error;
    
}

static bool write_markerDefinition( OTF_WStream* writer ) {
 
    int err;
    
    markerDispersionId= markerLastId + 1;
    
    cout << " OTF_IRREGULARITY_HINT ID: " << markerDispersionId << endl;
    
    err= OTF_WStream_writeDefMarker( writer, markerDispersionId, 
                                   "OTF_IRREGULARITY_HINT" , 
                                   OTF_MARKER_TYPE_HINT );
    
    cout << " Return Value of OTF_WStream... : " << err << endl;
    
    return (  err == 1 ? true : false  );
    
}

static bool write_markerDispersion( AllData& alldata, OTF_WStream* writer ) {
    
    
    int i= 0;
    uint64_t timerResolution= alldata.timerResolution;
    
    map< Pair, FunctionDispersionData, gtPair >::const_iterator it= alldata.functionDispersionMap.begin();
    map< Pair, FunctionDispersionData, gtPair >::const_iterator itend= alldata.functionDispersionMap.end();
    
    
    
    while ( itend != it && i < 20 ) {
        
        
        if ( it->second.count ) {
        
            map< uint64_t, FunctionMinMaxLocationData>::const_iterator it_loc=
            alldata.functionMinMaxLocationMap.find(it->first.b);
            map< uint64_t, FunctionMinMaxLocationData>::const_iterator itend_loc=
            alldata.functionMinMaxLocationMap.end();
            
            if ( itend_loc != it_loc ) {
                stringstream oss;
                
                uint64_t time= it_loc->second.location.time_max;
                uint64_t process= it_loc->second.location.loc_max;
                string name= alldata.functionIdNameMap.find(it->first.b)->second;
                
                if ( name.length() > 50) {
                    name.replace(name.find_first_of(",") + 1, name.find_last_of(",")-name.find_first_of(",") - 1, " ... ");
                }
/*
                cerr << "Irregularity function " << name << 
                " id: " << it->first.b << 
                " process: " << process <<
                " tmin: " << it->second.excl_time_minimum <<
                " t_25: " << it->second.excl_time_low_quartile <<
                " tmed: " << it->second.excl_time_median <<
                " t_75: " << it->second.excl_time_top_quartile <<
                " tmax: " << it->second.excl_time_maximum <<
                " tavg: " << it->second.excl_time_sum / it->second.count << 
                " MinMaxLocationInformation: " << 
                " min: " << it_loc->second.location.min << 
                " max: " << it_loc->second.location.max <<
                " lmin: " << it_loc->second.location.loc_min <<
                " lmax: " << it_loc->second.location.loc_max <<
                " tmin: " << it_loc->second.location.time_min << 
                " tmax: " << it_loc->second.location.time_max <<
                " tmin: " << (double) it_loc->second.location.time_min / timerResolution << 
                " tmax: " << (double) it_loc->second.location.time_max / timerResolution << endl;
*/
                
                oss << "Irregularity weight: " << (double) it->first.a / timerResolution
                    << " Function Name: " << name << " Dispersion values: "
                    << " [ " << it->second.excl_time_minimum / timerResolution
                    << " , " <<  it->second.excl_time_low_quartile / timerResolution
                    << " , " << it->second.excl_time_median / timerResolution
                    << " , " <<  it->second.excl_time_top_quartile / timerResolution
                    << " , " << (it->second.excl_time_sum / it->second.count) / timerResolution 
                    << " , " << it->second.excl_time_maximum / timerResolution 
                    << " ] " << endl;
                
                if ( 0 == OTF_WStream_writeMarker( writer, (uint64_t) time, 
                                                 (uint32_t) process, markerDispersionId, 
                                                 oss.str().c_str() ) ) {
                    
                    cout << "Error while writing Marker Spots " << endl ;
                    
                }
                else {
                    /*
                    cout << i << ": "  << oss.str() << endl;
                   */
                            i++;
                }
                
                oss.flush();

            }    

        }
                        
        it++;
    }    
    
    return true;
}

static bool write_markerDispersion_callpath( AllData& alldata, OTF_WStream* writer ) {


    int i= 0;
    uint64_t timerResolution= alldata.timerResolution;

    map< TripleCallpath, FunctionDispersionData, gtTripleCallpathSortByCallpath >::const_iterator it= alldata.functionDispersionCallpathMap.begin();
    map< TripleCallpath, FunctionDispersionData, gtTripleCallpathSortByCallpath >::const_iterator itend= alldata.functionDispersionCallpathMap.end();



    while ( itend != it ) {


        if ( it->second.count && it->first.b != "" && it->first.a > alldata.dispersionMarkerBorder) {

            map< string, FunctionMinMaxLocationData>::const_iterator it_loc=
            alldata.functionMinMaxLocationCallpathMap.find(it->first.b);
            map< string, FunctionMinMaxLocationData>::const_iterator itend_loc=
            alldata.functionMinMaxLocationCallpathMap.end();

            if ( itend_loc != it_loc ) {
                stringstream oss;

                uint64_t time= it_loc->second.location.time_max;
                uint64_t process= it_loc->second.location.loc_max;
                string name= alldata.functionIdNameMap.find(it->first.c)->second;

                if ( name.length() > 50) {
                    name.replace(name.find_first_of(",") + 1, name.find_last_of(",")-name.find_first_of(",") - 1, " ... ");
                }
/*
                cerr << "Irregularity function " << name <<
                " id: " << it->first.b <<
                " process: " << process <<
                " tmin: " << it->second.excl_time_minimum <<
                " t_25: " << it->second.excl_time_low_quartile <<
                " tmed: " << it->second.excl_time_median <<
                " t_75: " << it->second.excl_time_top_quartile <<
                " tmax: " << it->second.excl_time_maximum <<
                " tavg: " << it->second.excl_time_sum / it->second.count <<
                " MinMaxLocationInformation: " <<
                " min: " << it_loc->second.location.min <<
                " max: " << it_loc->second.location.max <<
                " lmin: " << it_loc->second.location.loc_min <<
                " lmax: " << it_loc->second.location.loc_max <<
                " tmin: " << it_loc->second.location.time_min <<
                " tmax: " << it_loc->second.location.time_max <<
                " tmin: " << (double) it_loc->second.location.time_min / timerResolution <<
                " tmax: " << (double) it_loc->second.location.time_max / timerResolution << endl;
*/

                oss << "Irregularity weight: " << (double) it->first.a / timerResolution
                    << " Function Name: " << name << " Dispersion values: "
                    << " [ " << it->second.excl_time_minimum / timerResolution
                    << " , " <<  it->second.excl_time_low_quartile / timerResolution
                    << " , " << it->second.excl_time_median / timerResolution
                    << " , " <<  it->second.excl_time_top_quartile / timerResolution
                    << " , " << (it->second.excl_time_sum / it->second.count) / timerResolution
                    << " , " << it->second.excl_time_maximum / timerResolution
                    << " ] " << endl;

                if ( 0 == OTF_WStream_writeMarker( writer, (uint64_t) time,
                                                 (uint32_t) process, markerDispersionId,
                                                 oss.str().c_str() ) ) {

                    cout << "Error while writing Marker Spots " << endl ;

                }
                else {
                    /*
                    cout << i << ": "  << oss.str() << endl;
                   */
                            i++;
                }

                oss.flush();

            }

        }

        it++;
    }

    return true;
}

bool CreateMarker( AllData& alldata ) {

    bool error= false;

    /* start runtime measurement for creating marker output */
    StartMeasurement( alldata, 1, true, "produce marker output" );

    VerbosePrint( alldata, 1, true, "producing marker output\n" );
    
    if ( 0 == alldata.myRank ) { 
    
        /* open OTF file manager, reader, and writer */
    
        OTF_FileManager* manager=
        OTF_FileManager_open( alldata.params.max_file_handles );
        assert( manager );
    
        OTF_Reader* reader=
        OTF_Reader_open( alldata.params.input_file_prefix.c_str(), manager );
        assert( reader );
        
        OTF_WStream* writer=
        OTF_WStream_open( alldata.params.input_file_prefix.c_str(), 0, manager );
        assert( writer );
        
        do {
     
            /* read and copy existing marker definition */
            
            VerbosePrint( alldata, 1, true, "reading marker definitions\n" );
            
            error= !read_markerDefinition( reader, writer );
            
            /* read and copy existing marker spots */
            
            VerbosePrint( alldata, 1, true, "reading marker spots\n" );
            error= !read_marker( reader, writer );
            
            /* write dispersion marker definition */ 
            
            if ( 0 == markerDispersionId ) {
             
                VerbosePrint( alldata, 1, true, "writing marker irregularity definition \n" );
                error= !write_markerDefinition( writer );
                
            }
            
            /* write dispersion marker */
            
            VerbosePrint( alldata, 1, true, "writing marker irregularity spots \n" );
            error= !write_markerDispersion(alldata, writer );
            if(alldata.params.dispersion.mode == DISPERSION_MODE_PERCALLPATH)
            {
                error= !write_markerDispersion_callpath(alldata, writer );
            }
        
        } while ( false );

        OTF_WStream_close( writer );
        OTF_Reader_close( reader );
        OTF_FileManager_close( manager );
    }
    
    if ( !error ) {

        /* stop runtime measurement for creating marker output */
        StopMeasurement( alldata, false, "produce marker output" );

    }

    return !error;
}
