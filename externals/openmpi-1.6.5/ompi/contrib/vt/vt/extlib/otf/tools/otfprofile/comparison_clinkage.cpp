/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz, Andre Groetzsch
*/

#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <vector>

#include "comparison.h"


using namespace std;


/* type for pairs in the distance map of CLINKAGE
 * ('Pair' struct in datastructs.h doesnt contain a less operator that's needed for the map) */
typedef pair <uint64_t, uint64_t> StdPairOfRanks;


struct DPair {

    uint64_t a;
    uint64_t b;
    double d;   // distance of a and b

        bool operator<(const DPair& p) const;
        bool operator==(const DPair& r) const;
        DPair(uint64_t a, uint64_t b, double d) : a(a),b(b),d(d) {}
};

bool DPair::operator<(const DPair& p) const
{return d < p.d;}

bool DPair::operator==(const DPair& p) const
{return a==p.a && b==p.b;}


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


/* normalized distance measure for clustering */
static double ndist( const double* a, const double* b, uint32_t n){

        double smin=0, smax=0;
        for ( uint32_t i= 0; i < n; ++i ){

                smin += min(*a, *b);
                smax += max(*a, *b);
                a++;
                b++;
        }
        return 1 - smin / smax; // 1 - sum(pmin(a, b)) / sum(pmax(a, b));
}


/* normalized distance measure for clustering *
static double ndist_calls( const double* a, const double* b, uint32_t n){

        double dmax = 0;

        for ( uint32_t i= 0; i < n; ++i, ++a, ++b ){

                if (max(*a, *b) > 0) dmax = max (dmax, min(*a, *b) / max(*a, *b));
        }
        return 1 - dmax;
}
*/


static bool erasekey(multiset<DPair>& set, const DPair& key )
{
  multiset<DPair>::iterator iter;

  //cout << "try to erase pair (" << key.a << ", " << key.b << ")" << endl;

  pair<multiset<DPair>::iterator, multiset<DPair>::iterator> ret;
  ret = set.equal_range(key);

  for (iter=ret.first; iter!=ret.second && !(*iter == key); ++iter);


  if (iter!=ret.second) // so the dpair is found
  {
        //cout << "erasing pair (" << iter->a << ", " << iter->b << ")" << endl;
    set.erase(iter);
    return true;
  }

  //cout << "           dpair not found!" << endl;
  return false;
}


static void NewPair(map<StdPairOfRanks, double>& dist, multiset<DPair >& dpairs, uint32_t& len, double& qt,
	map< uint64_t, double* >::const_iterator& it1, map< uint64_t, double* >::const_iterator& it2){
	
	/* it1->first is expected to be less than it2->first! */
	
	/* compute distance */
	double d = ndist(it1->second, it2->second, len);

	/* save distance */
	dist[make_pair(it1->first, it2->first)] = d;	

	/* insert pair to dpairs only if quality treshold is satisfied */
	if (d < qt) dpairs.insert(DPair(it1->first, it2->first, d));  
}



static void CLINKAGE( const set< uint64_t >& members, const map< uint64_t, double* >& vectors, uint32_t len,
        vector< set< uint64_t > >& ret_vector, double qt ) {

        //time_t startTime = time(NULL);

        map<StdPairOfRanks, double> dist; // distance map

        multiset<DPair > dpairs; // priority queue of dpairs

        set< uint64_t > unclustered;

        if (members.empty()) {  // consider all ranks

                /* compute distance map, dpairs and set of all processes (all vectors) */
                for (map< uint64_t, double* >::const_iterator it1 = vectors.begin(); it1 != vectors.end(); ++it1 ){

                        unclustered.insert( it1->first );

                        for (map< uint64_t, double* >::const_iterator it2 = vectors.begin(); it2 != it1; ++it2 ){                               
                                
                                NewPair(dist, dpairs, len, qt, it2, it1); // (it2->first < it1->first)
                        }
                }
        }
        else {  // consider members only

                /* compute distance map, dpairs and set of all processes */
                for ( set< uint64_t >::const_iterator it1 = members.begin(); it1 != members.end(); ++it1 ){

                        unclustered.insert( *it1 );

                        /* 'vectors' must not be declared as constant for using the element access operator []
                         * alternative: use 'find! */

                        map< uint64_t, double* >::const_iterator itv1 = vectors.find(*it1);

                        for ( set< uint64_t >::const_iterator it2 = members.begin(); it2 != it1; ++it2 ){

                                map< uint64_t, double* >::const_iterator itv2 = vectors.find(*it2);
                                
								NewPair(dist, dpairs, len, qt, itv2, itv1); // (it2->first < it1->first)
                        }
                }
        }

        /*              ALGORITHM               */

        map< uint64_t, set<uint64_t> > clustered; // map of clusters
//      map< uint64_t, uint64_t> > clustered; // map of clustering

        while (!dpairs.empty()) {

                /* plot current distance matrix *
                cout << "\t \t";
                for (set< uint64_t >::const_iterator it_a = unclustered.begin(); it_a != unclustered.end(); ++it_a ) cout << *it_a << "\t";
                cout << endl;
                for (set< uint64_t >::const_iterator it_a = unclustered.begin(); it_a != unclustered.end(); ++it_a ){

                        cout << "process " << *it_a << "\t ";
                        for (set< uint64_t >::const_iterator it_b = unclustered.begin(); it_b != unclustered.end(); ++it_b ){

                                if (*it_a > *it_b) printf("%.3f \t", dist[make_pair(*it_b, *it_a)]);

                                else cout << "*\t";
                        }
                        cout << endl;
                }
                cout << endl;   */

                multiset<DPair>::iterator it = dpairs.begin();
                DPair topDPair = *it;
                dpairs.erase(it);       // erase at known position -> fast

                uint64_t a = topDPair.a, b = topDPair.b;

                //cout << "closest pair: (" << a << ", " << b << ") with distance: " << topDPair.d << endl;

                /* delete b from 'unclustered' */
                unclustered.erase(b);

                /* copy b and all elements in clustered[b] to clustered[a] */
                clustered[a].insert(b);
                clustered[a].insert(clustered[b].begin(), clustered[b].end());
                /* clear b */ 
                clustered[b].clear();

                //clustered[b] = a; // save the cluster assignment

                /* update distances */
                for (set< uint64_t >::const_iterator c = unclustered.begin(); c != unclustered.end(); ++c) {

                        if (*c == a) continue;

                        uint64_t min_ac = min(a, *c);
                        uint64_t max_ac = max(a, *c);

                        double ac = dist[make_pair(min_ac, max_ac)];
                        double bc = dist[make_pair(min(b, *c), max(b, *c))];

                        if ( ac < bc ) {

                                if (ac < qt) erasekey(dpairs, DPair(min_ac, max_ac, ac));

                                dist[make_pair(min_ac, max_ac)] = bc;

                                if (bc < qt) dpairs.insert(DPair(min_ac, max_ac, bc));
                        }
                }

                //cout << "--> " << b << " clustered in " << a << endl;

                /* erase dpairs a with a.a==b or a.b==b */
                for (set< uint64_t >::const_iterator it_a = unclustered.begin(); it_a != unclustered.end(); ++it_a){

                        if (*it_a == a || *it_a == b) continue;

                        double d = dist[make_pair(min(b, *it_a), max(b, *it_a))];

                        if ( d < qt) erasekey(dpairs, DPair(min(b, *it_a), max(b, *it_a), d));

                        // else: pair doesn't exist!
                }

        }       /* now 'unclustered' contains the representatives of the clusters as result */

        //time_t endTime = time(NULL);

        //cout << "clustering of " ;

        //if (members.empty()) cout << vectors.size();
        //else cout << members.size();

        //cout << " processes with " << len << " functions " << "done in " << endTime-startTime
        //       <<  " seconds." << endl;
        //cout << "determined clusters: " << unclustered.size() << " (quality threshold: " << qt << ")." << endl;

        /* clustering out */
        for ( set< uint64_t >::const_iterator it = unclustered.begin(); it != unclustered.end(); ++it ){

                /* add unclustered *it to its cluster */
                clustered[*it].insert(*it);

                /* function out */
                ret_vector.push_back(clustered[*it]);

                /* on screen *
                cout << "cluster " << *it << ": ";
                for (set< uint64_t >::const_iterator it_b = clustered[*it].begin();
                        it_b != clustered[*it].end(); ++it_b) {
                        cout << *it_b << " ";
                }
                cout << endl;*/
        }


}


/* CLINKAGE signatur without 'members' (no hard groups) */

static void CLINKAGE( const map< uint64_t, double* >& vectors, uint32_t len,
        vector< set< uint64_t > >& ret_vector, double qt){

        set< uint64_t > defaultset;

        CLINKAGE(defaultset, vectors, len, ret_vector, qt );
}


bool ProcessComparisonCLINKAGE( AllData& alldata, ostringstream& mapdata ) {

    /* only the master performs the process comparison */
    if ( 0 != alldata.myRank ) {

        return true;
    }

        /* set quality threshold */
        double qt = alldata.params.clustering.quality_threshold;


    /* get vectors with number-of-calls for all functions in same order */

    /* map process ID to vector of function call counts,
    the values in the vector ar in same order -- it is sorded after
    'functionMapGlobal' which is guaranteed to contain all occuring function IDs */
    uint32_t vector_length= alldata.functionMapGlobal.size();
    map< uint64_t, uint64_t* > callcountPerRank;
    map< uint64_t, double* >   calltimesPerRank;

    if (!alldata.params.clustering.synth_data) {

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

                    ft= alldata.functionMapPerRank.find( Pair( it->first, jt->first ) ); //  iterators swapped!
                    uint64_t c= ( ft != ftend ) ? ft->second.count.sum : 0 ;

                    double t= ( ft != ftend ) ? ft->second.excl_time.sum : 0 ; // in cpu ticks not seconds!
        //            double t= ( ft != ftend ) ? ft->second.excl_time.sum / alldata.timerResolution : 0 ;

                    vc[index]= c ;
                    vt[index]= t ;
                    index++;
                }
            }
    }

    /*{
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
    }*/



    /*{
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
    }*/

        else { // -> alldata.params.synthData == true

                /* generate some synthetic data for testing the performance of the CLINK-algorithm */

                vector_length = alldata.params.clustering.synth_funcs_num; // number of functions

            for ( uint64_t i=1; i <= alldata.params.clustering.synth_ranks_num; i++ ) { // number of processes

                double*   vt= calltimesPerRank[ i ]= (double*) malloc( vector_length * sizeof(double) );
                assert( vt );

                for ( uint64_t j=0; j <vector_length; j++ )  vt[j] = i+j;
            }
        }

        if (!alldata.params.clustering.hard_grouping) { /* clustering without hard groups */

        vector< set< uint64_t > > result_vector;

                CLINKAGE( calltimesPerRank, vector_length, result_vector, qt );

                /* print cluster assignments */
                vector< set< uint64_t > >::const_iterator it= result_vector.begin();
                vector< set< uint64_t > >::const_iterator itend= result_vector.end();
                for ( ; it != itend ; ++it ) {

                        set< uint64_t >::const_iterator jt= it->begin();
                        set< uint64_t >::const_iterator jtend= it->end();
                        for ( ; jt != jtend ; ++jt ) {

                                mapdata << *jt << " ";
                        }
                        mapdata << endl;
                }
                
                
                if (!alldata.params.clustering.synth_data) {
		
					for ( map< uint64_t, uint64_t* >::const_iterator it = callcountPerRank.begin();
						it != callcountPerRank.end(); it++) free(it->second);
				} // didn't allocate memory to callcountPerRank in the synthetic data case
		    
				for ( map< uint64_t, double* >::const_iterator it = calltimesPerRank.begin();
				it != calltimesPerRank.end(); it++) free(it->second);
                
                return true;
        }

        /* else: clustering with hard groups */

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

        //cout << "number of processes: " << alldata.processIdNameMap.size() << endl;
        //cout << "number of hard groups: " << hard_groups.size() << endl;

        /*
        for (map< uint64_t, set< uint64_t > >::const_iterator iter=hard_groups.begin();
                iter!=hard_groups.end(); iter++){
                for (set< uint64_t >::const_iterator iter2= iter->second.begin();
                        iter2!=iter->second.end(); iter2++) cout << *iter2 << " ";
                cout << endl;
        }
        */

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
    for ( ; it != itend; ++it ) {


        /*{
            uint32_t id= it->first;
                        cout << "Cluster " << id << " ( " << it->second.size() << " members ) : { " ;

            set< uint64_t >::const_iterator jt= it->second.begin();
            set< uint64_t >::const_iterator jtend= it->second.end();
            for ( ; jt != jtend ; ++jt ) {

                cout << *jt << " ";
            }
            cout << "}" << endl;
        } */


        /* do not handle sets with < 6 members *
        if ( 6 > it->second.size() ) {

            *
            cout << "      skip small collection { ";
            set< uint64_t >::const_iterator jt= it->second.begin();
            set< uint64_t >::const_iterator jtend= it->second.end();
            for ( ; jt != jtend ; ++jt ) {

                cout << *jt << " ";
            }
            cout << "} " << endl;;
            *

            continue;
        } */

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

        /* kmeansquality quality;

        * use min( n/3, 16 ) clusters *
        uint32_t n= it->second.size();
        n= ( n/3 < 16 ) ? n/3 : 16;

        bool ret= do_kmeans( n, 10, vector_length,
            it->second, calltimesPerRank,
            result_vector, &quality ); */


                CLINKAGE( it->second, calltimesPerRank, vector_length, result_vector, qt );


        // if ( ! ret ) continue;

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
    
    if (!alldata.params.clustering.synth_data) {
		
		for ( map< uint64_t, uint64_t* >::const_iterator it = callcountPerRank.begin();
			it != callcountPerRank.end(); it++) free(it->second);
	} // didn't allocate memory to callcountPerRank in the synthetic data case
    
    for ( map< uint64_t, double* >::const_iterator it = calltimesPerRank.begin();
		it != calltimesPerRank.end(); it++) free(it->second);
		

#ifdef GNUPLOT_OUTPUT

    gnuplot_script << "0 t \"\"" << endl;
    gnuplot_script.close();

    gnuplot_inputs.close();
    gnuplot_cluster.close();

#endif /* GNUPLOT_OUTPUT */

    }

    return true;
}
