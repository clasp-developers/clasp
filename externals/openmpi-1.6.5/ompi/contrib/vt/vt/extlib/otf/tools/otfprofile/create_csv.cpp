/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
*/

#include <fstream>
#include <iostream>
#include <sstream>
#include <unistd.h>

#include "create_csv.h"
#include "otfprofile.h"

#include "OTF_Definitions.h"


/* define the following macro to fill-up per rank data to get equal data amounts
for each trace process/thread; this is useful to make the resulting CSV data
better comparable, but consider that it might cause a significant performance
impact */
/*#define FILLUP_DATA*/


using namespace std;


#ifdef FILLUP_DATA
/* fill-up per rank data based on the definitions */
static void fillup_data( AllData& alldata ) {

    /* iterate over process ids/names map */
    for ( map< uint64_t, string >::const_iterator proc_it=
          alldata.processIdNameMap.begin();
          proc_it != alldata.processIdNameMap.end(); proc_it++ ) {

        const uint64_t & proc_id= proc_it->first;

        /* add empty message data for process */
        alldata.messageMapPerRank[ proc_id ].add( MessageData() );

        /* add empty collop. data for process */

        alldata.collectiveMapPerRank[
            Pair( proc_id, OTF_COLLECTIVE_TYPE_BARRIER ) ].
                add( CollectiveData() );
        alldata.collectiveMapPerRank[
            Pair( proc_id, OTF_COLLECTIVE_TYPE_ONE2ALL ) ].
                add( CollectiveData() );
        alldata.collectiveMapPerRank[
            Pair( proc_id, OTF_COLLECTIVE_TYPE_ALL2ONE ) ].
                add( CollectiveData() );
        alldata.collectiveMapPerRank[
            Pair( proc_id, OTF_COLLECTIVE_TYPE_ALL2ALL ) ].
                add( CollectiveData() );

        /* iterate over function ids/names map */
        for ( map< uint64_t, string >::const_iterator func_it=
              alldata.functionIdNameMap.begin();
              func_it != alldata.functionIdNameMap.end(); func_it++ ) {

            const uint64_t & func_id= func_it->first;

            /* add empty function data for process/function */
            alldata.functionMapPerRank[ Pair( proc_id, func_id ) ].
                add( FunctionData() );

            /* iterate over counter ids/names map */
            for ( map< uint64_t, string >::const_iterator cntr_it=
                  alldata.counterIdNameMap.begin();
                  cntr_it != alldata.counterIdNameMap.end(); cntr_it++ ) {

                const uint64_t & cntr_id= cntr_it->first;

                /* add empty counter data for process/function/counter */
                alldata.counterMapPerFunctionRank[
                    Triple( proc_id, func_id, cntr_id ) ].add( CounterData() );

            }

        }

    }
}
#endif /* FILLUP_DATA */


/* append function data to CSV file */
static void write_func_data( AllData& alldata, ofstream& csvFile,
    const string& csvFileName ) {

    assert( csvFile.good() );

    VerbosePrint( alldata, 2, false,
                  " appending function data to file: %s\n",
                  csvFileName.c_str() );

    static const string LINE_PREFIX= "FUNCTION";

    if ( 0 == alldata.myRank ) {

        /* write headline */
        csvFile << LINE_PREFIX << ';'
                << "Process;Function;Invocations;Excl. Time (s);Incl. Time (s)" << endl;

    }

    /* write function data */

    map< Pair, FunctionData, ltPair >::iterator it= alldata.functionMapPerRank.begin();
    map< Pair, FunctionData, ltPair >::iterator itend= alldata.functionMapPerRank.end();
    while ( itend != it ) {

        const uint64_t& proc_id= it->first.a;
        const uint64_t& func_id= it->first.b;
        const uint64_t& count= it->second.count.cnt;
        const double excl_time= it->second.excl_time.sum / alldata.timerResolution;
        const double incl_time= it->second.incl_time.sum / alldata.timerResolution;

        const string& proc_name= alldata.processIdNameMap[ proc_id ];
        assert( 0 != proc_name.length() );

        const string& func_name= alldata.functionIdNameMap[ func_id ];
        assert( 0 != func_name.length() );

        csvFile << LINE_PREFIX << ';'
                << proc_name << ';'
                << func_name << ';'
                << count << ';'
                << excl_time << ';'
                << incl_time << endl;

        it++;

    }
}


/* append counter data to CSV file */
static void write_counter_data( AllData& alldata, ofstream& csvFile,
    const string& csvFileName ) {

    assert( csvFile.good() );

    VerbosePrint( alldata, 2, false,
                  " appending counter data to file: %s\n",
                  csvFileName.c_str() );

    static const string LINE_PREFIX= "COUNTER";

    if ( 0 == alldata.myRank ) {

        /* write headline */
        csvFile << endl << LINE_PREFIX << ';'
                << "Process;Function;Counter;Excl. Rate;Incl. Rate"
                << endl;

    }

    /* write counter data */

    map< Triple, CounterData, ltTriple >::iterator it= alldata.counterMapPerFunctionRank.begin();
    map< Triple, CounterData, ltTriple >::iterator itend= alldata.counterMapPerFunctionRank.end();
    while ( itend != it ) {

        const uint64_t& proc_id= it->first.a;
        const uint64_t& func_id= it->first.b;
        const uint64_t& counter_id= it->first.c;

        const string& proc_name= alldata.processIdNameMap[ proc_id ];
        assert( 0 != proc_name.length() );

        const string& counter_name= alldata.counterIdNameMap[ counter_id ];
        assert( 0 != counter_name.length() );

        const string& func_name= alldata.functionIdNameMap[ func_id ];
        assert( 0 != func_name.length() );

        map< Pair, FunctionData, ltPair >::const_iterator func_it=
            alldata.functionMapPerRank.find( Pair( proc_id, func_id ) );
        assert( alldata.functionMapPerRank.end() != func_it );

        double excl_rate= 0.0;
        if ( 0.0 < func_it->second.excl_time.sum ) {

            excl_rate= it->second.excl_time.sum /
                func_it->second.excl_time.sum * alldata.timerResolution;

        }

        double incl_rate= 0.0;
        if ( 0.0 < func_it->second.incl_time.sum ) {

            incl_rate= it->second.incl_time.sum /
                func_it->second.incl_time.sum * alldata.timerResolution;

        }

        csvFile << LINE_PREFIX << ';'
                << proc_name << ';'
                << func_name << ';'
                << counter_name << ';'
                << excl_rate << ';'
                << incl_rate << endl;

        it++;

    }
}


/* append P2P message data to CSV file */
static void write_p2p_data( AllData& alldata, ofstream& csvFile,
    const string& csvFileName ) {

    assert( csvFile.good() );

    VerbosePrint( alldata, 2, false,
                  " appending P2P message data to file: %s\n",
                  csvFileName.c_str() );

    static const string LINE_PREFIX= "P2P";

    if ( 0 == alldata.myRank ) {

        /* write headline */
        csvFile << endl << LINE_PREFIX << ';'
                << "Process;Send Invocations;Recv. Invocations;Send Bytes;Recv. Bytes;Duration (s)"
                << endl;

    }

    /* write P2P message data */

    map< uint64_t, MessageData >::iterator it= alldata.messageMapPerRank.begin();
    map< uint64_t, MessageData >::iterator itend= alldata.messageMapPerRank.end();
    while ( itend != it ) {

        const uint64_t& proc_id= it->first;

        const uint64_t& count_send= it->second.count_send.cnt;
        const uint64_t& count_recv= it->second.count_recv.cnt;
        const uint64_t& bytes_send= it->second.bytes_send.sum;
        const uint64_t& bytes_recv= it->second.bytes_recv.sum;
        const double duration= it->second.duration_send.sum / alldata.timerResolution;

        const string& proc_name= alldata.processIdNameMap[ proc_id ];
        assert( 0 != proc_name.length() );

        csvFile << LINE_PREFIX << ';'
                << proc_name << ';'
                << count_send << ';'
                << count_recv << ';'
                << bytes_send << ';'
                << bytes_recv << ';'
                << duration << endl;

        it++;

    }
}


/* append collective op. data to CSV file */
static void write_collop_data( AllData& alldata, ofstream& csvFile,
    const string& csvFileName ) {

    assert( csvFile.good() );

    VerbosePrint( alldata, 2, false,
                  " appending collective op. data to file: %s\n",
                  csvFileName.c_str() );

    static const string LINE_PREFIX= "COLLOP";

    static map< uint64_t, string > op_class_names;
    if ( op_class_names.empty() ) {

        op_class_names[ OTF_COLLECTIVE_TYPE_BARRIER ]= "BARRIER";
        op_class_names[ OTF_COLLECTIVE_TYPE_ONE2ALL ]= "ONE2ALL";
        op_class_names[ OTF_COLLECTIVE_TYPE_ALL2ONE ]= "ALL2ONE";
        op_class_names[ OTF_COLLECTIVE_TYPE_ALL2ALL ]= "ALL2ALL";

    }

    if ( 0 == alldata.myRank ) {

        /* write headline */
        csvFile << endl << LINE_PREFIX << ';'
                << "Process;Coll. Op;Send Invocations;Recv. Invocations;Send Bytes;Recv. Bytes;Duration (s)"
                << endl;

    }

    /* write collop. data */

    map< Pair, CollectiveData, ltPair >::iterator it= alldata.collectiveMapPerRank.begin();
    map< Pair, CollectiveData, ltPair >::iterator itend= alldata.collectiveMapPerRank.end();
    while ( itend != it ) {

        const uint64_t& proc_id= it->first.a;
        const uint64_t& op_class= it->first.b;

        const uint64_t& count_send= it->second.count_send.cnt;
        const uint64_t& count_recv= it->second.count_recv.cnt;
        const uint64_t& bytes_send= it->second.bytes_send.sum;
        const uint64_t& bytes_recv= it->second.bytes_recv.sum;
        const double duration= it->second.duration_send.sum / alldata.timerResolution;

        const string& proc_name= alldata.processIdNameMap[ proc_id ];
        assert( 0 != proc_name.length() );

        const string& op_class_name= op_class_names[ op_class ];
        assert( 0 != op_class_name.length() );

        csvFile << LINE_PREFIX << ';'
                << proc_name << ';'
                << op_class_name << ';'
                << count_send << ';'
                << count_recv << ';'
                << bytes_send << ';'
                << bytes_recv << ';'
                << duration << endl;

        it++;

    }
}


bool CreateCSV( AllData& alldata ) {

    bool error= false;

    /* start runtime measurement for creating CSV output */
    StartMeasurement( alldata, 1, true, "produce CSV output" );

    VerbosePrint( alldata, 1, true, "producing CSV output\n" );

    /* compose output file name */
    string csv_file_name= alldata.params.output_file_prefix + ".csv";

    /* remove already existing output file */
    unlink( csv_file_name.c_str() );

#ifdef FILLUP_DATA
    /* fill-up per rank data based on the definitions */
    fillup_data( alldata );
#endif /* FILLUP_DATA */

    /* statistics types */
    enum {
        STAT_TYPE_FUNC,
        STAT_TYPE_COUNTER,
        STAT_TYPE_P2P,
        STAT_TYPE_COLLOP,
        STAT_TYPE_NUM
    };

    for( uint8_t type= 0; type < STAT_TYPE_NUM && !error; type++ ) {

#ifdef OTFPROFILE_MPI
        for( uint32_t rank= 0; rank < alldata.numRanks; rank++ ) {

            if ( alldata.myRank == rank ) {
#endif /* OTFPROFILE_MPI */

                /* open CSV output file */

                ofstream csv_file( csv_file_name.c_str(), ios_base::app );
                if ( !csv_file ) {

                    cerr << "ERROR: Unable to open file '" << csv_file_name
                         << "' for writing." << endl;

                    error= true;

                } else {

                    /* write statistics */

                    csv_file.precision( 9 );

                    switch( type ) {

                        case STAT_TYPE_FUNC:
                            if ( !alldata.params.clustering.enabled ||
                                 0 == alldata.myRank ) {

                                /* in case of additional clustering, the master
                                already has function statistics over *all*
                                processes; only the master writes its data */
                                write_func_data( alldata, csv_file,
                                    csv_file_name );
                            }
                            break;
                        case STAT_TYPE_COUNTER:
                            write_counter_data( alldata, csv_file,
                                csv_file_name );
                            break;
                        case STAT_TYPE_P2P:
                            write_p2p_data( alldata, csv_file, csv_file_name );
                            break;
                        case STAT_TYPE_COLLOP:
                            write_collop_data( alldata, csv_file,
                                csv_file_name );
                            break;

                    }

                }

                /* close CSV output file */
                csv_file.close();

                if ( STAT_TYPE_NUM == type +1 ) {

                    VerbosePrint( alldata, 2, true, " created file: %s\n",
                                  csv_file_name.c_str() );

                }

#ifdef OTFPROFILE_MPI
            }

            /* broadcast error indicator to workers */
            if ( SyncError( alldata, error, rank ) ) {

                break;

            }

            MPI_Barrier( MPI_COMM_WORLD );

        }
#endif /* OTFPROFILE_MPI */

    }

    if ( !error ) {

        /* stop runtime measurement for creating CSV output */
        StopMeasurement( alldata, false, "produce CSV output" );

    }

    return !error;
}
