/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_COLL_TUNED_TOPO_H_HAS_BEEN_INCLUDED
#define MCA_COLL_TUNED_TOPO_H_HAS_BEEN_INCLUDED

#include "ompi_config.h"

#define MAXTREEFANOUT 32

BEGIN_C_DECLS

    typedef struct ompi_coll_tree_t {
        int32_t tree_root;
        int32_t tree_fanout;
        int32_t tree_bmtree;
        int32_t tree_prev;
        int32_t tree_next[MAXTREEFANOUT];
        int32_t tree_nextsize;
    } ompi_coll_tree_t;

    ompi_coll_tree_t*
    ompi_coll_tuned_topo_build_tree( int fanout,
                                     struct ompi_communicator_t* com,
                                     int root );
    ompi_coll_tree_t*
    ompi_coll_tuned_topo_build_in_order_bintree( struct ompi_communicator_t* comm );

    ompi_coll_tree_t*
    ompi_coll_tuned_topo_build_bmtree( struct ompi_communicator_t* comm,
                                       int root );
    ompi_coll_tree_t*
    ompi_coll_tuned_topo_build_in_order_bmtree( struct ompi_communicator_t* comm,
						int root );
    ompi_coll_tree_t*
    ompi_coll_tuned_topo_build_chain( int fanout,
                                      struct ompi_communicator_t* com,
                                      int root );

    int ompi_coll_tuned_topo_destroy_tree( ompi_coll_tree_t** tree );

    /* debugging stuff, will be removed later */
    int ompi_coll_tuned_topo_dump_tree (ompi_coll_tree_t* tree, int rank);

END_C_DECLS

#endif  /* MCA_COLL_TUNED_TOPO_H_HAS_BEEN_INCLUDED */

