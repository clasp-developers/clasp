/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
*/

#include <fstream>
#include <iostream>
#include <limits>
#include <map>
#include <set>
#include <vector>
#include <math.h>

#include "comparison.h"


using namespace std;


struct kmeansquality {

    double min_dist2_vec_to_cent;
    double max_dist2_vec_to_cent;
    double avg_dist2_vec_to_cent;

    double min_dist2_cent_to_cent;
    double max_dist2_cent_to_cent;
    double avg_dist2_cent_to_cent;

    uint32_t iterations_used;
    uint32_t iterations_max;

    uint32_t number_of_clusters;
};


template<class T>
static bool vector_equal( const T* a, const T* b, uint32_t len );


template<class T>
static bool vector_equal( const T* a, const T* b, uint32_t len ) {

    uint32_t index= 0;
    while ( ( len > index ) && ( *a == *b ) ) {

        ++a;
        ++b;
        ++index;
    }

    return ( len == index );
}


static void vector_zero( double* v, uint32_t n ) {

    while ( 0 < n ) {

        *v= 0.0;
        ++v;
        --n;
    }
}


static void vector_add( double* accu, const double* add, uint32_t n ) {

    while ( 0 < n ) {

        *accu += *add;
        ++accu;
        ++add;
        --n;
    }
}


static void vector_div( double* v, double div, uint32_t n ) {

    while ( 0 < n ) {

        *v= *v / div;
        ++v;
        --n;
    }
}


/*
static void vector_print( const double* v, uint32_t n ) {

    uint8_t m= 10;
    while ( ( 0 < n ) && ( 0 < m ) ) {

        cout << *v << " ";
        ++v;
        --n;
        --m;
    }
    cout << endl;
}
*/


/* computes euklidian distance ^2 of the vector distance */
static double vector_dist_2( const double* a, const double* b, uint32_t n ) {

    double ret= 0.0;

    while ( 0 < n ) {

        double tmp= ( *a - *b );
        ret += tmp * tmp;

        ++a;
        ++b;
        --n;
    }

    return ret;
}


/* do K-means clustering for the vectors in 'vectors' of length 'len' considering
only the members 'vectors' with a key contained in 's'. exactly 'n' clusters will
be created and at most 'max' iterations will be done. The result is stored in 'ret'
-- the 's.size()' input IDs are placed into the 'n' sets in the vector. If
'ret_quality' is not NULL, then the contained quality measures are filled for
the produced clustering */
static bool do_kmeans( uint32_t n, uint32_t max, uint32_t len,
    const set< uint64_t >& members, const map< uint64_t, double* >& vectors,
    vector< set< uint64_t > >& ret_vector, kmeansquality* ret_quality ) {

    /* check if enough vectors present */
    if ( n >= members.size() ) {

        cerr << "ERROR: Too many clusters (" << n << ") for " << members.size()
             << " vectors " << endl;
        return false;
    }

    /* initial cluster assignments -- one per cluster, remaining ones to first cluster */

    /* allocate memory for cluster centroid vectors */
    double** centroids= (double**) malloc( n * sizeof(double*) + n * len * sizeof(double) );
    assert( centroids );

    /* for every cluster centroid vector count how many original vectors contribute, 
    this is the number to divde by after all original vectors were added. */
    uint32_t* centroids_n= (uint32_t*) malloc( n * sizeof(uint32_t) );
    assert( centroids_n );

    /* initialize 'centroids' pointer array and 'centroids_n' */
    double* p= (double*) ( centroids + n );
    for ( uint32_t i= 0; i < n; ++i ) {

        centroids[i]= p;
        p += len;

        centroids_n[i]= 0;
    }


    /* for processing, keep for every member (uint64_t) the cluster index (uint32_t).
    Only in the very end, translate this representation to 
    'vector< set< uint64_t > >& ret_vector' */
    map< uint64_t, uint32_t > assignment;
    {
    set< uint64_t >::const_iterator it= members.begin();
    set< uint64_t >::const_iterator itend= members.end();
    uint32_t i= 0;
    for ( ; it != itend; ++it ) {

        assignment[ *it ]= i % n;
        centroids_n[ i % n ] += 1;

        /*
        cout << "    assign " << *it << " --> " << ( i % n ) << 
            " out of " << centroids_n[ i % n ] << " / " << n << endl;
        */

        ++i;
    }
    }

    bool change= true;
    uint32_t m= 0;
    while ( ( m < max ) && change ) {

        change= false;

        /*
        cout << endl << "round " << m << "/" << max << endl << endl;
        */

        /* zeroing centroid vectors */
        for ( uint32_t i= 0; i < n; ++i ) {

            vector_zero( centroids[i], len );
        }

        /* print assignment */
        /* {
        map< uint64_t, uint32_t >::const_iterator kt= assignment.begin();
        map< uint64_t, uint32_t >::const_iterator ktend= assignment.end();
        for ( ; kt != ktend; ++kt ) {

            cout << "    assigned " << kt->first << " --> " << kt->second << 
                " out of " << centroids_n[ kt->second ] << endl;
        }
        } */

        /* compute cluster centroids -- iterate over 'assignment', for every
        assigned cluster, add associated vector to centroid, keep size of clusters */
        map< uint64_t, uint32_t >::const_iterator it= assignment.begin();
        map< uint64_t, uint32_t >::const_iterator itend= assignment.end();
        for ( ; it != itend; ++it ) {

            map< uint64_t, double* >::const_iterator ft= vectors.find( it->first );
            assert( vectors.end() != ft );

            /*
            cout << "before " << it->second << "\t " ; vector_print( centroids[ it->second ], len );
            cout << "add to " << it->second << "\t " ; vector_print( ft->second, len );
            */
            vector_add( centroids[ it->second ], ft->second, len );
            /*
            cout << "after  " << it->second << "\t " ; vector_print( centroids[ it->second ], len );
            cout << endl;
            */
        }

        /* divide sum of vectors by the number of contributing vectors to get real centroids */
        for ( uint32_t i= 0; i < n; ++i ) {

            /*
            cout << "centroid " << i << " sum :" << endl;
            vector_print( centroids[i], len );
            */

            /*
            cout << "div vector " << i << " by " << centroids_n[ i ] << endl;
            */
            vector_div( centroids[i], centroids_n[i], len );

            /*
            cout << "centroid " << i << ":" << endl;
            vector_print( centroids[i], len );
            */
        }


        /* for all vectors */
        map< uint64_t, uint32_t >::iterator jt= assignment.begin();
        map< uint64_t, uint32_t >::iterator jtend= assignment.end();
        for ( ; jt != jtend; ++jt ) {

            /*
            cout << "        check " << jt->first ;
            */

            map< uint64_t, double* >::const_iterator ft= vectors.find( jt->first );
            assert( vectors.end() != ft );
            const double* v= ft->second;

            /* previous assignment */
            uint32_t a= jt->second;
            /*
            cout << " currently assigned to " << a << endl;
            */


            /* previous number of members in this cluster */
            uint32_t b= centroids_n[ a ];
            assert( 0 < b ); /* otherwise this cluster is empty which is illegal */

            /* if only one member left then we cannot re-assign because empty clusters 
            are not allowed*/
            if ( 1 == b ) continue;

            uint32_t aa= (uint32_t) -1; 
            double d= numeric_limits<double>::max( );

            /* for all centroids */
            for ( uint32_t k= 0; k < n ; ++k ) {

                double dd= vector_dist_2( v, centroids[k], len );

                /*
                cout << "            cent " << k << " d^2= " << dd ;
                */
                if ( dd < d ) {
                    aa= k;
                    d= dd;
                    /*
                    cout << " * ";
                    */
                }
                /*
                cout << endl;
                */
            }

            assert( aa != (uint32_t) -1 ); /* there must be a new value */

            if ( aa != a ) {

                /* actual re-assignment */
                /*
                cout << endl << " ### reassign " << jt->first << " from " << a << " to " << aa << " ### " << endl << endl;
                */
                centroids_n[ a ]--;
                centroids_n[ aa ]++;
                jt->second= aa;
                change= true;
            }
        }

        ++m;
    }


    /* final assignment of members to clusters */
    ret_vector.clear();
    ret_vector.resize( n, set<uint64_t>() );
    {
    map< uint64_t, uint32_t >::const_iterator it= assignment.begin();
    map< uint64_t, uint32_t >::const_iterator itend= assignment.end();

    for ( ; it != itend ; ++it ) {

        ret_vector[ it->second ].insert( it->first );
    }
    }

    /* print cluster assignments */

    /*
    cout << "    #" << n << " clusters: { ";
    vector< set< uint64_t > >::const_iterator it= ret_vector.begin();
    vector< set< uint64_t > >::const_iterator itend= ret_vector.end();
    for ( ; it != itend ; ++it ) {

        cout << "{ ";
        set< uint64_t >::const_iterator jt= it->begin();
        set< uint64_t >::const_iterator jtend= it->end();
        for ( ; jt != jtend ; ++jt ) {

            cout << *jt << " ";
        }
        cout << "} ";
    }
    cout << "}"<< endl;
    */

    /* if 'ret_quality' is requested, compute some more things */
    if ( NULL != ret_quality ) {

        uint32_t cnt;

        ret_quality->min_dist2_vec_to_cent= numeric_limits<double>::max( );
        ret_quality->max_dist2_vec_to_cent= numeric_limits<double>::min( );
        ret_quality->avg_dist2_vec_to_cent= 0.0;
        cnt= 0;

        map< uint64_t, uint32_t >::const_iterator it= assignment.begin();
        map< uint64_t, uint32_t >::const_iterator itend= assignment.end();
        for ( ; it != itend; ++it ) {

            map< uint64_t, double* >::const_iterator ft= vectors.find( it->first );
            assert( vectors.end() != ft );
            const double* v= ft->second;

            for ( uint32_t j= 0; j < n; ++j ) {

                double d2= vector_dist_2( v, centroids[j], len );

                ret_quality->min_dist2_vec_to_cent= ( d2 < ret_quality->min_dist2_cent_to_cent ) ? d2 : ret_quality->min_dist2_cent_to_cent;
                ret_quality->max_dist2_vec_to_cent= ( d2 > ret_quality->max_dist2_cent_to_cent ) ? d2 : ret_quality->max_dist2_cent_to_cent;
                ret_quality->avg_dist2_vec_to_cent += d2;
                cnt++;
            }
        }
        if ( 0 < cnt )
            ret_quality->avg_dist2_vec_to_cent= ret_quality->avg_dist2_vec_to_cent / cnt;

        ret_quality->min_dist2_cent_to_cent= numeric_limits<double>::max( );
        ret_quality->max_dist2_cent_to_cent= numeric_limits<double>::min( );
        ret_quality->avg_dist2_cent_to_cent= 0.0;

        cnt= 0;
        for ( uint32_t i= 0; i < n; ++i ) {
            for ( uint32_t j= i+1; j < n; ++j ) {

                if ( i == j ) continue;

                double d2= vector_dist_2( centroids[i], centroids[j], len );

                ret_quality->min_dist2_cent_to_cent= ( d2 < ret_quality->min_dist2_cent_to_cent ) ? d2 : ret_quality->min_dist2_cent_to_cent;
                ret_quality->max_dist2_cent_to_cent= ( d2 > ret_quality->max_dist2_cent_to_cent ) ? d2 : ret_quality->max_dist2_cent_to_cent;
                ret_quality->avg_dist2_cent_to_cent += d2;
                cnt++;
            }
        }
        if ( 0 < cnt )
            ret_quality->avg_dist2_cent_to_cent= ret_quality->avg_dist2_cent_to_cent / cnt;

        ret_quality->iterations_used= m;
        ret_quality->iterations_max= max;

        ret_quality->number_of_clusters= n;
    }

    free( centroids ); centroids= NULL;
    free( centroids_n ); centroids_n= NULL;

    return true;
}


bool ProcessComparisonKMEANS( AllData& alldata, ostringstream& mapdata ) {

    /* only the master performs the process comparison */
    if ( 0 != alldata.myRank ) {

        return true;
    }

    /* get vectors with number-of-calls for all functions in same order */

    /* map process ID to vector of function call counts, 
    the values in the vector ar in same order -- it is sorded after
    'functionMapGlobal' which is guaranteed to contain all occuring function IDs */
    uint32_t vector_length= alldata.functionMapGlobal.size();
    map< uint64_t, uint64_t* > callcountPerRank;
    map< uint64_t, double* >   calltimesPerRank;

    {
    map< uint64_t, string >::const_iterator it= alldata.processIdNameMap.begin();
    map< uint64_t, string >::const_iterator itend= alldata.processIdNameMap.end();
    for ( ; it != itend; ++it ) {

        /* get the next vector to fill in the following loop */
        uint64_t* vc= callcountPerRank[ it->first ]= (uint64_t*) malloc( vector_length * sizeof(uint64_t) );
        assert( vc );
        double*   vt= calltimesPerRank[ it->first ]= (double*) malloc( vector_length * sizeof(double) );
        assert( vt );

        map< Pair, FunctionData, ltPair >::const_iterator ft;
        map< Pair, FunctionData, ltPair >::const_iterator ftend= alldata.functionMapPerRank.end();

        map< uint64_t, FunctionData >::const_iterator jt= alldata.functionMapGlobal.begin();
        map< uint64_t, FunctionData >::const_iterator jtend= alldata.functionMapGlobal.end();
        uint32_t index= 0;
        for ( ; jt != jtend; ++jt ) {

            ft= alldata.functionMapPerRank.find( Pair( it->first, jt->first ) );
            uint64_t c= ( ft != ftend ) ? ft->second.count.sum : 0 ;
            double t= ( ft != ftend ) ? ft->second.excl_time.sum : 0 ;

            vc[index]= c ;
            vt[index]= t ;
            index++;
        }
    }
    }

    /*
    {
    cout << "    count vectors of size " << vector_length << " : " << endl;    
    map< uint64_t, uint64_t* >::const_iterator kt= callcountPerRank.begin();
    map< uint64_t, uint64_t* >::const_iterator ktend= callcountPerRank.end();
    for ( ;kt != ktend; ++kt ) {

        cout << "       " << kt->first << " count : " ;

        uint32_t i= 0;
        uint64_t* p= kt->second;
        for ( ; i < vector_length; ++i, ++p ) {

            cout << *p << " ";
        }
        cout << endl;
    }
    }
    */

    /*
    {
    cout << "    times vectors of size " << vector_length << " : " << endl;    
    map< uint64_t, double* >::const_iterator kt= calltimesPerRank.begin();
    map< uint64_t, double* >::const_iterator ktend= calltimesPerRank.end();
    for ( ;kt != ktend; ++kt ) {
        cout << "       " << kt->first << " times : " ;
        uint32_t i= 0;
        double* p= kt->second;
        for ( ; i < vector_length; ++i, ++p ) {

            cout << *p << " ";
        }
        cout << endl;


    }
    }
    */

    /* extract groups of processes that are identical with respect to hard properties.
    Therefore, make candidate set, compare all processes with remaining candidates, 
    when processes match, create target sets and remove processes from canditates. 
    Target sets are labeled by their first */

    map< uint64_t, set< uint64_t > > hard_groups;

    {

    /* candidate set with all entries */

    set< uint64_t > candidates;
    map< uint64_t, uint64_t* >::const_iterator jt= callcountPerRank.begin();
    map< uint64_t, uint64_t* >::const_iterator jtend= callcountPerRank.end();
    for ( ; jt != jtend; ++jt ) {

        candidates.insert( jt->first );
    }


    set< uint64_t >::const_iterator ctend= candidates.end();
    map< uint64_t, uint64_t* >::const_iterator itend= callcountPerRank.end();
    map< uint64_t, uint64_t* >::const_iterator ita= callcountPerRank.begin();
    map< uint64_t, uint64_t* >::const_iterator itb;

    for ( ; ita != itend; ++ita ) {

        /* if ita->first is not in candidate set, then skip */
        if ( ctend == candidates.find( ita->first ) ) continue;

        itb= ita;
        ++itb;
        if ( itb == itend ) break;
        for ( ; itb != itend; ++itb ) {

            /* if itb->first is not in candidate set, then skip */
            if ( ctend == candidates.find( itb->first ) ) continue;

            /* compare vectors at ita and itb */

            bool e= vector_equal< uint64_t >( ita->second, itb->second, vector_length );

            if ( e ) {

                /*cout << "        " << ita->first << " == " << itb->first << endl;*/
                candidates.erase( ita->first );
                candidates.erase( itb->first );

                hard_groups[ ita->first ].insert( ita->first );
                hard_groups[ ita->first ].insert( itb->first );

            } else {

                /*cout << "        " << ita->first << " != " << itb->first << endl;*/
            }

        }
    }

    }

    /*cout << " compare soft properties " << endl;*/

    {

#ifdef GNUPLOT_OUTPUT 

    /* gnuplot output: scripts and data to examine clustering quality */
    /* generate two output files: gnuplot script and gnuplot input file */
    ofstream gnuplot_script, gnuplot_inputs, gnuplot_cluster;
    gnuplot_script.open( "gnuplot_script.sh" );
    gnuplot_inputs.open( "gnuplot_inputs.txt" );
    gnuplot_cluster.open( "gnuplot_cluster.txt" );

    gnuplot_script << "#!/usr/bin/gnuplot -persist\n\n";
    gnuplot_script << "set terminal pdf\n" ;
    gnuplot_script << "set output \"gnuplot_clusters_quality.pdf\"\n";
    gnuplot_script << "# set terminal postscript eps enhanced color solid 18\n";
    gnuplot_script << "# set output \"gnuplot_clusters_quality.eps\"\n";
    gnuplot_script << "set key top Right\n";
    gnuplot_script << "set title \"cluster quailty: avg cluster radius per dimension\"\n";
    gnuplot_script << "set xlabel \"cluster bins\"\n";
    gnuplot_script << "set ylabel \"radius [ticks]\"\n\n";
    gnuplot_script << "plot [:][:] \\\n";
#endif /* GNUPLOT_OUTPUT */

    map< uint64_t, set< uint64_t > >::const_iterator it= hard_groups.begin();
    map< uint64_t, set< uint64_t > >::const_iterator itend= hard_groups.end();
    for ( ; it != itend ; ++it ) {

        /*
        {
            uint32_t id= it->first;
            cout << "Cluster " << id << " ( " << it->second.size() << " members ) : { " ;

            set< uint64_t >::const_iterator jt= it->second.begin();
            set< uint64_t >::const_iterator jtend= it->second.end();
            for ( ; jt != jtend ; ++jt ) {

                cout << *jt << " ";
            }
            cout << "}" << endl;
        }
        */

        /* do not handle sets with < 6 members */
        if ( 6 > it->second.size() ) {

            /*
            cout << "      skip small collection { ";
            set< uint64_t >::const_iterator jt= it->second.begin();
            set< uint64_t >::const_iterator jtend= it->second.end();
            for ( ; jt != jtend ; ++jt ) {

                cout << *jt << " ";
            }
            cout << "} " << endl;;
            */

            continue;
        }

#ifdef GNUPLOT_OUTPUT
        gnuplot_script << "'gnuplot_inputs.txt' using ($4):( ( " << id << "== $2 ) ? $13/$6 : 1/0 ) t \"" << id << "\" w lp, \\" << endl;

                gnuplot_cluster << endl << " id " << id << " : ";
        set< uint64_t >::const_iterator kt= it->second.begin();
        set< uint64_t >::const_iterator ktend= it->second.end();
        for ( ; kt != ktend ; ++kt ) {

            gnuplot_cluster << *kt << " ";
        }
        gnuplot_cluster << endl;
#endif /* GNUPLOT_OUTPUT */

        vector< set< uint64_t > > result_vector;

        kmeansquality quality;

        /* use min( n/3, 16 ) clusters */
        uint32_t n= it->second.size();
        n= ( n/3 < 16 ) ? n/3 : 16;

        bool ret= do_kmeans( n, 10, vector_length,
            it->second, calltimesPerRank, 
            result_vector, &quality );

        if ( ! ret ) continue;

        /* print cluster assignments and quality */
        {

            vector< set< uint64_t > >::const_iterator it= result_vector.begin();
            vector< set< uint64_t > >::const_iterator itend= result_vector.end();
            for ( ; it != itend ; ++it ) {

                /*cout << " { ";*/
#ifdef GNUPLOT_OUTPUT
                gnuplot_cluster << " { ";
#endif /* GNUPLOT_OUTPUT */
                set< uint64_t >::const_iterator jt= it->begin();
                set< uint64_t >::const_iterator jtend= it->end();
                for ( ; jt != jtend ; ++jt ) {

                    /*cout << *jt << " ";*/
#ifdef GNUPLOT_OUTPUT
                    gnuplot_cluster << *jt << " ";
#endif /* GNUPLOT_OUTPUT */

                    mapdata << *jt << " ";

                }
                /*cout << "}";*/
#ifdef GNUPLOT_OUTPUT
                gnuplot_cluster << "}";
#endif /* GNUPLOT_OUTPUT */

                mapdata << endl;
            }
            /*cout << " } " << endl << endl;*/
#ifdef GNUPLOT_OUTPUT
            gnuplot_cluster << " } " << endl;
#endif /* GNUPLOT_OUTPUT */

            mapdata << endl;

/*
        cout << "        number_of_clusters   " << 
            quality.number_of_clusters << endl;
        cout << "        number_of_iterations " << 
            quality.iterations_used << " <= " <<
            quality.iterations_max << endl;

        cout << "        distance_vector_to_center " << 
            sqrt( quality.min_dist2_vec_to_cent ) << " / " <<
            sqrt( quality.avg_dist2_vec_to_cent ) << " / " <<
            sqrt( quality.max_dist2_vec_to_cent ) << " (min/avg/max)" << endl;

        cout << "        distance_center_to_center " << 
            sqrt( quality.min_dist2_cent_to_cent ) << " / " <<
            sqrt( quality.avg_dist2_cent_to_cent ) << " / " <<
            sqrt( quality.max_dist2_cent_to_cent ) << " (min/avg/max)" << endl;
*/
#ifdef GNUPLOT_OUTPUT
            gnuplot_inputs << " id " << id <<
                " num_clusters "  << quality.number_of_clusters << 
                " vector_length " << vector_length <<
                " iterations " << quality.iterations_used << " max_iterations " << quality.iterations_max <<
                " distance_vector_to_center_min_avg_max " << 
                    sqrt( quality.min_dist2_vec_to_cent ) << " " <<
                    sqrt( quality.avg_dist2_vec_to_cent ) << " " <<
                    sqrt( quality.max_dist2_vec_to_cent ) << " " <<
                " distance_center_to_center_min_avg_max " <<
                    sqrt( quality.min_dist2_cent_to_cent ) << " " << 
                    sqrt( quality.avg_dist2_cent_to_cent ) << " " << 
                    sqrt( quality.max_dist2_cent_to_cent ) << endl;
#endif /* GNUPLOT_OUTPUT */

        }
    }

#ifdef GNUPLOT_OUTPUT

    gnuplot_script << "0 t \"\"" << endl;
    gnuplot_script.close();

    gnuplot_inputs.close();
    gnuplot_cluster.close();

#endif /* GNUPLOT_OUTPUT */

    }

    return true;
}
