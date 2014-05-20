/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
*/

#include <fstream>
#include <iostream>
#include <sstream>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "OTF_Platform.h"

#include "clustering.h"
#include "comparison.h"
#include "otfprofile.h"


using namespace std;


bool ProcessClustering( AllData& alldata ) {

    bool error= false;

    do {

        ostringstream map_data;

        /* start runtime measurement for process comparison */
        StartMeasurement( alldata, 1, true, "process comparison" );

        /* compare processes */

        if ( CLUSTER_ALG_CLINKAGE == alldata.params.clustering.alg ) {

            VerbosePrint( alldata, 1, true,
                          "comparing processes using CLINKAGE\n" );

            error= !ProcessComparisonCLINKAGE( alldata, map_data );

        } else { /* CLUSTER_ALG_KMEANS == alldata.params.clustering.alg */

            VerbosePrint( alldata, 1, true,
                          "comparing processes using KMEANS\n" );

            error= !ProcessComparisonKMEANS( alldata, map_data );
        }

#ifdef OTFPROFILE_MPI
        /* synchronize error indicator with workers */
        if ( SyncError( alldata, error ) ) {

            break;
        }
#endif /* OTFPROFILE_MPI */

        /* stop runtime measurement for process comparison */
        StopMeasurement( alldata, true, "process comparison" );

        /* check for process comparison result */

        char have_map_data= (char)( 0 < map_data.str().length() );

#ifdef OTFPROFILE_MPI
        MPI_Bcast( &have_map_data, 1, MPI_CHAR, 0, MPI_COMM_WORLD );
#endif /* OTFPROFILE_MPI */

        if ( !have_map_data ) {

            if ( 0 == alldata.myRank ) {

                cerr << "WARNING: Process comparison did not give any results."
                     << endl;
            }

            break;
        }

        /* the master creates the process mapping file and (if desired) applies
        it to otfshrink */

        if ( 0 == alldata.myRank ) {

            /* open process mapping file */

            ofstream map_file(
               alldata.params.clustering.map_file_name.c_str() );
            if ( !map_file ) {

                cerr << "ERROR: Unable to open file '"
                     << alldata.params.clustering.map_file_name
                     << "' for writing." << endl;

                error= true;

            } else {

                /* write mapping data to file */
                map_file << map_data.str();

                /* close process mapping file */
                map_file.close();

                VerbosePrint( alldata, 2, true, " created file: %s\n",
                              alldata.params.clustering.map_file_name.c_str() );

                if ( alldata.params.clustering.shrink ) {

                    /* call otfshrink with created process mapping file */

                    /* start runtime measurement for shrinking input trace */
                    StartMeasurement( alldata, 1, false, "shrink input trace" );

                    VerbosePrint( alldata, 1, true, "shrinking input trace\n" );

                    /* composing command */

                    char cmd[1024];

                    snprintf( cmd, sizeof( cmd ) - 1,
                        "otfshrink -i %s -o %s -f %s",
                        alldata.params.input_file_prefix.c_str(),
                        alldata.params.clustering.shrink_output_prefix.c_str(),
                        alldata.params.clustering.map_file_name.c_str() );

                    /* run command */

                    VerbosePrint( alldata, 2, true, " running command: %s\n",
                                  cmd );

                    int rc= system( cmd );

                    /* evaluate exit status */

                    int es= ( -1 != rc ) ? WEXITSTATUS( rc ) : 0;

                    /* command could not be executed; print warning message */
                    if ( -1 == rc || 127 == es ) {

                        ostringstream warn_msg;

                        warn_msg << "Warning: Could not execute command '"
                                 << cmd << "'";

                        if ( -1 == rc ) {

                            warn_msg << " (" << strerror( errno ) << ")";
                        }

                        warn_msg << "." << endl
                            << "Try to run this command manually in terminal "
                            << "to shrink the input trace.";

                        cerr << warn_msg.str() << endl;

                    /* command executed, but failed; abort */
                    } else if ( 0 != es ) {

                        cerr << "ERROR: Could not shrink input trace '"
                             << alldata.params.input_file_prefix << "' to '"
                             << alldata.params.clustering.shrink_output_prefix
                             << "'. otfshrink returned with exit code " << es
                             << "." << endl;
                        error= true;

                    /* command executed successfully */
                    } else { /* es == 0 */

                        /* stop runtime measurement for shrinking input trace */
                        StopMeasurement( alldata, false, "shrink input trace" );
                    }
                }
            }
        }

#ifdef OTFPROFILE_MPI
        /* broadcast error indicator to workers */
        SyncError( alldata, error, 0 );
#endif /* OTFPROFILE_MPI */

    } while( false );

    return !error;
}
