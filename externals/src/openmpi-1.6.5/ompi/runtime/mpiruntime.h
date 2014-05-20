/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      University of Houston.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * Interface into the MPI portion of the Open MPI Run Time Environment
 */

#ifndef OMPI_MPI_MPIRUNTIME_H
#define OMPI_MPI_MPIRUNTIME_H

#include "ompi_config.h"

#include "opal/class/opal_list.h"
#include "opal/class/opal_hash_table.h"

BEGIN_C_DECLS

/** forward type declaration */
struct ompi_communicator_t;
/** forward type declaration */
struct opal_thread_t;

/* Global variables and symbols for the MPI layer */

/** Did mpi start to initialize? */
OMPI_DECLSPEC extern bool ompi_mpi_init_started;
/** Is mpi initialized? */
OMPI_DECLSPEC extern bool ompi_mpi_initialized;
/** Has mpi been finalized? */
OMPI_DECLSPEC extern bool ompi_mpi_finalized;

/** Do we have multiple threads? */
OMPI_DECLSPEC extern bool ompi_mpi_thread_multiple;
/** Thread level requested to \c MPI_Init_thread() */
OMPI_DECLSPEC extern int ompi_mpi_thread_requested;
/** Thread level provided by Open MPI */
OMPI_DECLSPEC extern int ompi_mpi_thread_provided;
/** Identifier of the main thread */
OMPI_DECLSPEC extern struct opal_thread_t *ompi_mpi_main_thread;


/** Bitflags to be used for the modex exchange for the various thread
 *  levels. Required to support heterogeneous environments */
#define OMPI_THREADLEVEL_SINGLE_BF     0x00000001
#define OMPI_THREADLEVEL_FUNNELED_BF   0x00000002
#define OMPI_THREADLEVEL_SERIALIZED_BF 0x00000004
#define OMPI_THREADLEVEL_MULTIPLE_BF   0x00000008

#define OMPI_THREADLEVEL_SET_BITFLAG(threadlevelin,threadlevelout) { \
    if ( MPI_THREAD_SINGLE == threadlevelin ) {                 \
        threadlevelout |= OMPI_THREADLEVEL_SINGLE_BF;           \
    } else if ( MPI_THREAD_FUNNELED == threadlevelin ) {        \
        threadlevelout |= OMPI_THREADLEVEL_FUNNELED_BF;         \
    } else if ( MPI_THREAD_SERIALIZED == threadlevelin ) {      \
        threadlevelout |= OMPI_THREADLEVEL_SERIALIZED_BF;       \
    } else if ( MPI_THREAD_MULTIPLE == threadlevelin ) {       \
        threadlevelout |= OMPI_THREADLEVEL_MULTIPLE_BF;         \
    }}


#define OMPI_THREADLEVEL_IS_MULTIPLE(threadlevel) (threadlevel & OMPI_THREADLEVEL_MULTIPLE_BF)

/** Do we want to be warned on fork or not? */
OMPI_DECLSPEC extern bool ompi_warn_on_fork;

/** In ompi_mpi_init: a list of all memory associated with calling
    MPI_REGISTER_DATAREP so that we can free it during
    MPI_FINALIZE. */
OMPI_DECLSPEC extern opal_list_t ompi_registered_datareps;

/** In ompi_mpi_init: the lists of Fortran 90 mathing datatypes.
 * We need these lists and hashtables in order to satisfy the new
 * requirements introduced in MPI 2-1 Sect. 10.2.5, 
 * MPI_TYPE_CREATE_F90_xxxx, page 295, line 47.
 */
extern opal_hash_table_t ompi_mpi_f90_integer_hashtable;
extern opal_hash_table_t ompi_mpi_f90_real_hashtable;
extern opal_hash_table_t ompi_mpi_f90_complex_hashtable;

/** version string of ompi */
OMPI_DECLSPEC extern const char ompi_version_string[];

OMPI_DECLSPEC void ompi_warn_fork(void);

/**
 * Initialize the Open MPI MPI environment
 *
 * @param argc argc, typically from main() (IN)
 * @param argv argv, typically from main() (IN)
 * @param requested Thread support that is requested (IN)
 * @param provided Thread support that is provided (OUT)
 *
 * @returns MPI_SUCCESS if successful
 * @returns Error code if unsuccessful
 *
 * Intialize all support code needed for MPI applications.  This
 * function should only be called by MPI applications (including
 * singletons).  If this function is called, ompi_init() and
 * ompi_rte_init() should *not* be called.
 *
 * It is permissable to pass in (0, NULL) for (argc, argv).
 */
int ompi_mpi_init(int argc, char **argv, int requested, int *provided);

/**
 * Finalize the Open MPI MPI environment
 *
 * @returns MPI_SUCCESS if successful
 * @returns Error code if unsuccessful
 *
 * Should be called after all MPI functionality is complete (usually
 * during MPI_FINALIZE).
 */
int ompi_mpi_finalize(void);

/**
 * Abort the processes of comm
 */
OMPI_DECLSPEC int ompi_mpi_abort(struct ompi_communicator_t* comm,
                   int errcode, bool kill_remote_of_intercomm);

/**
 * Do a preconnect of MPI connections (i.e., force connections to
 * be made if they will be made).
 */
int ompi_init_preconnect_mpi(void);

END_C_DECLS

#endif /* OMPI_MPI_MPIRUNTIME_H */
