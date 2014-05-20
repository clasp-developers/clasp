/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
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
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2006-2009 University of Houston. All rights reserved.
 * Copyright (c) 2008-2009 Sun Microsystems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */
#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "mpi.h"
#include "opal/class/opal_list.h"
#include "opal/mca/base/base.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/mca/maffinity/base/base.h"
#include "opal/runtime/opal_progress.h"
#include "opal/threads/threads.h"
#include "opal/util/output.h"
#include "opal/util/error.h"
#include "opal/util/stacktrace.h"
#include "opal/util/show_help.h"
#include "opal/runtime/opal.h"
#include "opal/event/event.h"
#include "opal/mca/hwloc/hwloc.h"

#include "orte/util/proc_info.h"
#include "orte/runtime/runtime.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/show_help.h"
#include "orte/mca/ess/ess.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"

#include "orte/mca/notifier/notifier.h"

#include "ompi/constants.h"
#include "ompi/mpi/f77/constants.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/runtime/params.h"
#include "ompi/runtime/ompi_module_exchange.h"
#include "ompi/communicator/communicator.h"
#include "ompi/info/info.h"
#include "ompi/errhandler/errcode.h"
#include "ompi/request/request.h"
#include "ompi/op/op.h"
#include "ompi/mca/op/op.h"
#include "ompi/mca/op/base/base.h"
#include "ompi/file/file.h"
#include "ompi/attribute/attribute.h"
#include "ompi/mca/allocator/base/base.h"
#include "ompi/mca/rcache/base/base.h"
#include "ompi/mca/rcache/rcache.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/io/io.h"
#include "ompi/mca/io/base/base.h"
#include "ompi/debuggers/debuggers.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "ompi/mca/dpm/base/base.h"
#include "ompi/mca/pubsub/base/base.h"

#if OPAL_ENABLE_FT_CR == 1
#include "ompi/mca/crcp/crcp.h"
#include "ompi/mca/crcp/base/base.h"
#endif
#include "ompi/runtime/ompi_cr.h"

#include "orte/runtime/orte_globals.h"

/* This is required for the boundaries of the hash tables used to store
 * the F90 types returned by the MPI_Type_create_f90_XXX functions.
 */
#include <float.h>

#if OPAL_CC_USE_PRAGMA_IDENT
#pragma ident OMPI_IDENT_STRING
#elif OPAL_CC_USE_IDENT
#ident OMPI_IDENT_STRING
#endif
const char ompi_version_string[] = OMPI_IDENT_STRING;

/*
 * Global variables and symbols for the MPI layer
 */

bool ompi_mpi_init_started = false;
bool ompi_mpi_initialized = false;
bool ompi_mpi_finalized = false;

bool ompi_mpi_thread_multiple = false;
int ompi_mpi_thread_requested = MPI_THREAD_SINGLE;
int ompi_mpi_thread_provided = MPI_THREAD_SINGLE;

opal_thread_t *ompi_mpi_main_thread = NULL;

bool ompi_mpi_maffinity_setup = false;

bool ompi_warn_on_fork;

#if OPAL_HAVE_POSIX_THREADS
static bool fork_warning_issued = false;
static bool atfork_called = false;

static void warn_fork_cb(void)
{
    if (ompi_mpi_initialized && !ompi_mpi_finalized && !fork_warning_issued) {
        orte_show_help("help-mpi-runtime.txt", "mpi_init:warn-fork", true,
                       orte_process_info.nodename, getpid(),
                       ompi_mpi_comm_world.comm.c_my_rank);
        fork_warning_issued = true;
    }
}
#endif

void ompi_warn_fork(void)
{
#if OPAL_HAVE_POSIX_THREADS
    if (ompi_warn_on_fork && !atfork_called) {
        pthread_atfork(warn_fork_cb, NULL, NULL);
        atfork_called = true;
    }
#endif
}

/*
 * These variables are here, rather than under ompi/mpi/c/foo.c
 * because it is not sufficient to have a .c file that only contains
 * variables -- you must have a function that is invoked from
 * elsewhere in the code to guarantee that all linkers will pull in
 * the .o file from the library.  Hence, although these are MPI
 * constants, we might as well just define them here (i.e., in a file
 * that already has a function that is guaranteed to be linked in,
 * rather than make a new .c file with the constants and a
 * corresponding dummy function that is invoked from this function).
 *
 * Additionally, there can be/are strange linking paths such that
 * ompi_info needs symbols such as ompi_fortran_status_ignore,
 * which, if they weren't here with a collection of other global
 * symbols that are initialized (which seems to force this .o file to
 * be pulled into the resolution process, because ompi_info certainly
 * does not call ompi_mpi_init()), would not be able to be found by
 * the OSX linker.
 *
 * NOTE: See the big comment in ompi/mpi/f77/constants.h about why we
 * have four symbols for each of the common blocks (e.g., the Fortran
 * equivalent(s) of MPI_STATUS_IGNORE).  Here, we can only have *one*
 * value (not four).  So the only thing we can do is make it equal to
 * the fortran compiler convention that was selected at configure
 * time.  Note that this is also true for the value of .TRUE. from the
 * Fortran compiler, so even though Open MPI supports all four Fortran
 * symbol conventions, it can only support one convention for the two
 * C constants (MPI_FORTRAN_STATUS[ES]_IGNORE) and only support one
 * compiler for the value of .TRUE.  Ugh!!
 *
 * Note that the casts here are ok -- we're *only* comparing pointer
 * values (i.e., they'll never be de-referenced).  The global symbols
 * are actually of type (ompi_fortran_common_t) (for alignment
 * issues), but MPI says that MPI_F_STATUS[ES]_IGNORE must be of type
 * (MPI_Fint*).  Hence, we have to cast to make compilers not
 * complain.
 */
#if OMPI_WANT_F77_BINDINGS
#  if OMPI_F77_CAPS
MPI_Fint *MPI_F_STATUS_IGNORE = (MPI_Fint*) &MPI_FORTRAN_STATUS_IGNORE;
MPI_Fint *MPI_F_STATUSES_IGNORE = (MPI_Fint*) &MPI_FORTRAN_STATUSES_IGNORE;
#  elif OMPI_F77_PLAIN
MPI_Fint *MPI_F_STATUS_IGNORE = (MPI_Fint*) &mpi_fortran_status_ignore;
MPI_Fint *MPI_F_STATUSES_IGNORE = (MPI_Fint*) &mpi_fortran_statuses_ignore;
#  elif OMPI_F77_SINGLE_UNDERSCORE
MPI_Fint *MPI_F_STATUS_IGNORE = (MPI_Fint*) &mpi_fortran_status_ignore_;
MPI_Fint *MPI_F_STATUSES_IGNORE = (MPI_Fint*) &mpi_fortran_statuses_ignore_;
#  elif OMPI_F77_DOUBLE_UNDERSCORE
MPI_Fint *MPI_F_STATUS_IGNORE = (MPI_Fint*) &mpi_fortran_status_ignore__;
MPI_Fint *MPI_F_STATUSES_IGNORE = (MPI_Fint*) &mpi_fortran_statuses_ignore__;
#  else
#    error Unrecognized Fortran 77 name mangling scheme
#  endif
#else
MPI_Fint *MPI_F_STATUS_IGNORE = NULL;
MPI_Fint *MPI_F_STATUSES_IGNORE = NULL;
#endif  /* OMPI_WANT_F77_BINDINGS */


/* Constants for the Fortran layer.  These values are referred to via
   common blocks in the Fortran equivalents.  See
   ompi/mpi/f77/constants.h for a more detailed explanation.

   The values are *NOT* initialized.  We do not use the values of
   these constants; only their addresses (because they're always
   passed by reference by Fortran).  

   Initializing upon instantiation these can reveal size and/or
   alignment differences between Fortran and C (!) which can cause
   warnings or errors upon linking (e.g., making static libraries with
   the intel 9.0 compilers on 64 bit platforms shows alignment
   differences between libmpi.a and the user's application, resulting
   in a linker warning).  FWIW, if you initialize these variables in
   functions (i.e., not at the instantiation in the global scope), the
   linker somehow "figures it all out" (w.r.t. different alignments
   between fortan common blocks and the corresponding C variables) and
   no linker warnings occur.

   Note that the rationale for the types of each of these variables is
   discussed in ompi/include/mpif-common.h.  Do not change the types
   without also modifying ompi/mpi/f77/constants.h and
   ompi/include/mpif-common.h.
 */

#define INST(type, upper_case, lower_case, single_u, double_u)   \
type lower_case; \
type upper_case; \
type single_u;  \
type double_u

INST(int, MPI_FORTRAN_BOTTOM, mpi_fortran_bottom,
     mpi_fortran_bottom_, mpi_fortran_bottom__);
INST(int, MPI_FORTRAN_IN_PLACE, mpi_fortran_in_place,
     mpi_fortran_in_place_, mpi_fortran_in_place__);
INST(char *, MPI_FORTRAN_ARGV_NULL, mpi_fortran_argv_null,
     mpi_fortran_argv_null_, mpi_fortran_argv_null__);
INST(double, MPI_FORTRAN_ARGVS_NULL, mpi_fortran_argvs_null,
     mpi_fortran_argvs_null_, mpi_fortran_argvs_null__);
INST(int *, MPI_FORTRAN_ERRCODES_IGNORE, mpi_fortran_errcodes_ignore,
     mpi_fortran_errcodes_ignore_, mpi_fortran_errcodes_ignore__);
INST(int *, MPI_FORTRAN_STATUS_IGNORE, mpi_fortran_status_ignore,
     mpi_fortran_status_ignore_, mpi_fortran_status_ignore__);
INST (double, MPI_FORTRAN_STATUSES_IGNORE, mpi_fortran_statuses_ignore,
      mpi_fortran_statuses_ignore_, mpi_fortran_statuses_ignore__);

/*
 * Hash tables for MPI_Type_create_f90* functions
 */
opal_hash_table_t ompi_mpi_f90_integer_hashtable;
opal_hash_table_t ompi_mpi_f90_real_hashtable;
opal_hash_table_t ompi_mpi_f90_complex_hashtable;

/*
 * Per MPI-2:9.5.3, MPI_REGISTER_DATAREP is a memory leak.  There is
 * no way to *de*register datareps once they've been registered.  So
 * we have to track all registrations here so that they can be
 * de-registered during MPI_FINALIZE so that memory-tracking debuggers
 * don't show Open MPI as leaking memory.
 */
opal_list_t ompi_registered_datareps;


int ompi_mpi_init(int argc, char **argv, int requested, int *provided)
{
    int ret;
    ompi_proc_t** procs;
    size_t nprocs;
    char *error = NULL;
    bool timing = false;
    int param, value;
    struct timeval ompistart, ompistop;
    char *event_val = NULL;
    opal_paffinity_base_cpu_set_t mask;
    bool proc_bound;
#if 0
    /* see comment below about sched_yield */
    int num_processors;
#endif
    bool orte_setup = false;
    bool paffinity_enabled = false;

    /* bitflag of the thread level support provided. To be used
     * for the modex in order to work in heterogeneous environments. */
    uint8_t threadlevel_bf; 

    /* Indicate that we have *started* MPI_INIT*.  MPI_FINALIZE has
       something sorta similar in a static local variable in
       ompi_mpi_finalize(). */
    ompi_mpi_init_started = true;

    /* Setup enough to check get/set MCA params */

    if (ORTE_SUCCESS != (ret = opal_init_util(&argc, &argv))) {
        error = "ompi_mpi_init: opal_init_util failed";
        goto error;
    }

    /* _After_ opal_init_util() but _before_ orte_init(), we need to
       set an MCA param that tells libevent that it's ok to use any
       mechanism in libevent that is available on this platform (e.g.,
       epoll and friends).  Per opal/event/event.s, we default to
       select/poll -- but we know that MPI processes won't be using
       pty's with the event engine, so it's ok to relax this
       constraint and let any fd-monitoring mechanism be used. */
    ret = mca_base_param_reg_string_name("opal", "event_include",
                                         "Internal orted MCA param: tell opal_init() to use a specific mechanism in libevent",
                                         false, false, "all", &event_val);
    if (ret >= 0) {
        /* We have to explicitly "set" the MCA param value here
           because libevent initialization will re-register the MCA
           param and therefore override the default. Setting the value
           here puts the desired value ("all") in different storage
           that is not overwritten if/when the MCA param is
           re-registered. This is unless the user has specified a different
           value for this MCA parameter. Make sure we check to see if the
           default is specified before forcing "all" in case that is not what
           the user desires. Note that we do *NOT* set this value as an
           environment variable, just so that it won't be inherited by
           any spawned processes and potentially cause unintented
           side-effects with launching ORTE tools... */
        if (0 == strcmp("all", event_val)) {
            mca_base_param_set_string(ret, "all");
        }
    }

    if( NULL != event_val ) {
        free(event_val);
        event_val = NULL;
    }

    /* check to see if we want timing information */
    param = mca_base_param_reg_int_name("ompi", "timing",
                                        "Request that critical timing loops be measured",
                                        false, false, 0, &value);
    if (value != 0) {
        timing = true;
        gettimeofday(&ompistart, NULL);
    }
    
    /* Setup ORTE - note that we are an MPI process  */
    if (ORTE_SUCCESS != (ret = orte_init(NULL, NULL, ORTE_PROC_MPI))) {
        error = "ompi_mpi_init: orte_init failed";
        goto error;
    }
    orte_setup = true;
    
    /* check for timing request - get stop time and report elapsed time if so */
    if (timing && 0 == ORTE_PROC_MY_NAME->vpid) {
        gettimeofday(&ompistop, NULL);
        opal_output(0, "ompi_mpi_init [%ld]: time from start to completion of orte_init %ld usec",
                    (long)ORTE_PROC_MY_NAME->vpid,
                    (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                               (ompistop.tv_usec - ompistart.tv_usec)));
        gettimeofday(&ompistart, NULL);
    }

    /* Figure out the final MPI thread levels.  If we were not
       compiled for support for MPI threads, then don't allow
       MPI_THREAD_MULTIPLE.  Set this stuff up here early in the
       process so that other components can make decisions based on
       this value. */

    ompi_mpi_thread_requested = requested;
    if (OPAL_HAVE_THREAD_SUPPORT == 0) {
        ompi_mpi_thread_provided = *provided = MPI_THREAD_SINGLE;
        ompi_mpi_main_thread = NULL;
    } else if (OMPI_ENABLE_THREAD_MULTIPLE == 1) {
        ompi_mpi_thread_provided = *provided = requested;
        ompi_mpi_main_thread = opal_thread_get_self();
    } else {
        if (MPI_THREAD_MULTIPLE == requested) {
            ompi_mpi_thread_provided = *provided = MPI_THREAD_SERIALIZED;
        } else {
            ompi_mpi_thread_provided = *provided = requested;
        }
        ompi_mpi_main_thread = opal_thread_get_self();
    }

    ompi_mpi_thread_multiple = (ompi_mpi_thread_provided == 
                                MPI_THREAD_MULTIPLE);


    /* determine the bitflag belonging to the threadlevel_support provided */
    memset ( &threadlevel_bf, 0, sizeof(uint8_t));
    OMPI_THREADLEVEL_SET_BITFLAG ( ompi_mpi_thread_provided, threadlevel_bf );

    /* add this bitflag to the modex */
    if ( OMPI_SUCCESS != (ret = ompi_modex_send_string("MPI_THREAD_LEVEL", &threadlevel_bf, sizeof(uint8_t)))) {
	error = "ompi_mpi_init: modex send thread level";
	goto error;
    }

#if OPAL_HAVE_HWLOC
    /* If orte_init() didn't fill in opal_hwloc_topology, then we need
       to go fill it in ourselves. */
    if (NULL == opal_hwloc_topology) {
        if (0 != hwloc_topology_init(&opal_hwloc_topology) ||
            0 != hwloc_topology_load(opal_hwloc_topology)) {
            return OPAL_ERR_NOT_SUPPORTED;
        }
    }
#endif

    /* Once we've joined the RTE, see if any MCA parameters were
       passed to the MPI level */

    if (OMPI_SUCCESS != (ret = ompi_mpi_register_params())) {
        error = "mca_mpi_register_params() failed";
        goto error;
    }

    /* If desired, send a notify message */
    if (ompi_notify_init_finalize) {
        orte_notifier.log(ORTE_NOTIFIER_NOTICE,
                          ORTE_SUCCESS,
                          "MPI_INIT:Starting on host %s, pid %d",
                          orte_process_info.nodename,
                          orte_process_info.pid);
    }

    /* initialize datatypes. This step should be done early as it will
     * create the local convertor and local arch used in the proc
     * init.
     */
    if (OMPI_SUCCESS != (ret = ompi_datatype_init())) {
        error = "ompi_datatype_init() failed";
        goto error;
    }

    /* Initialize OMPI procs */
    if (OMPI_SUCCESS != (ret = ompi_proc_init())) {
        error = "mca_proc_init() failed";
        goto error;
    }

    /* Initialize the op framework. This has to be done *after*
       ddt_init, but befor mca_coll_base_open, since some collective
       modules (e.g., the hierarchical coll component) may need ops in
       their query function. */
    if (OMPI_SUCCESS != (ret = ompi_op_base_open())) {
        error = "ompi_op_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != 
        (ret = ompi_op_base_find_available(OPAL_ENABLE_PROGRESS_THREADS,
                                           OMPI_ENABLE_THREAD_MULTIPLE))) {
        error = "ompi_op_base_find_available() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = ompi_op_init())) {
        error = "ompi_op_init() failed";
        goto error;
    }

    /* Open up MPI-related MCA components */

    if (OMPI_SUCCESS != (ret = mca_allocator_base_open())) {
        error = "mca_allocator_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = mca_rcache_base_open())) {
        error = "mca_rcache_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = mca_mpool_base_open())) {
        error = "mca_mpool_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = mca_pml_base_open())) {
        error = "mca_pml_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = mca_coll_base_open())) {
        error = "mca_coll_base_open() failed";
        goto error;
    }

    if (OMPI_SUCCESS != (ret = ompi_osc_base_open())) {
        error = "ompi_osc_base_open() failed";
        goto error;
    }

#if OPAL_ENABLE_FT_CR == 1
    if (OMPI_SUCCESS != (ret = ompi_crcp_base_open())) {
        error = "ompi_crcp_base_open() failed";
        goto error;
    }
#endif

    /* In order to reduce the common case for MPI apps (where they
       don't use MPI-2 IO or MPI-1 topology functions), the io and
       topo frameworks are initialized lazily, at the first use of
       relevant functions (e.g., MPI_FILE_*, MPI_CART_*, MPI_GRAPH_*),
       so they are not opened here. */

    /* Select which MPI components to use */

    if (OMPI_SUCCESS != 
        (ret = mca_mpool_base_init(OPAL_ENABLE_PROGRESS_THREADS,
                                   OMPI_ENABLE_THREAD_MULTIPLE))) {
        error = "mca_mpool_base_init() failed";
        goto error;
    }

    if (OMPI_SUCCESS != 
        (ret = mca_pml_base_select(OPAL_ENABLE_PROGRESS_THREADS,
                                   OMPI_ENABLE_THREAD_MULTIPLE))) {
        error = "mca_pml_base_select() failed";
        goto error;
    }

    /* check for timing request - get stop time and report elapsed time if so */
    if (timing && 0 == ORTE_PROC_MY_NAME->vpid) {
        gettimeofday(&ompistop, NULL);
        opal_output(0, "ompi_mpi_init[%ld]: time from completion of orte_init to modex %ld usec",
                    (long)ORTE_PROC_MY_NAME->vpid,
                    (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                               (ompistop.tv_usec - ompistart.tv_usec)));
        gettimeofday(&ompistart, NULL);
    }
    
    /* exchange connection info - this function also acts as a barrier
     * as it will not return until the exchange is complete
     */
    if (OMPI_SUCCESS != (ret = orte_grpcomm.modex(NULL))) {
        error = "orte_grpcomm_modex failed";
        goto error;
    }

    if (timing && 0 == ORTE_PROC_MY_NAME->vpid) {
        gettimeofday(&ompistop, NULL);
        opal_output(0, "ompi_mpi_init[%ld]: time to execute modex %ld usec",
                    (long)ORTE_PROC_MY_NAME->vpid,
                    (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                               (ompistop.tv_usec - ompistart.tv_usec)));
        gettimeofday(&ompistart, NULL);
    }
    
    /* identify the architectures of remote procs and setup
     * their datatype convertors, if required
     */
    if (OMPI_SUCCESS != (ret = ompi_proc_set_arch())) {
        error = "ompi_proc_set_arch failed";
        goto error;
    }

    /* if it hasn't already been done, setup process affinity. 
     * First check to see if a slot list was
     * specified.  If so, use it.  If no slot list was specified,
     * that's not an error -- just fall through and try the next
     * paffinity scheme.
     */
    ret = opal_paffinity_base_get(&mask);
    if (OPAL_SUCCESS == ret) {
        /* paffinity is supported - check for binding */
        OPAL_PAFFINITY_PROCESS_IS_BOUND(mask, &proc_bound);
        if (proc_bound || opal_paffinity_base_bound) {
            /* someone external set it - indicate it is set
             * so that we know
             */
            paffinity_enabled = true;
        } else {
            /* the system is capable of doing processor affinity, but it
             * has not yet been set - see if a slot_list was given
             */
            if (NULL != opal_paffinity_base_slot_list) {
                /* It's an error if multiple paffinity schemes were specified */
                if (opal_paffinity_alone) {
                    ret = OMPI_ERR_BAD_PARAM;
                    error = "Multiple processor affinity schemes specified (can only specify one)";
                    goto error;
                }
                ret = opal_paffinity_base_slot_list_set((long)ORTE_PROC_MY_NAME->vpid, opal_paffinity_base_slot_list);
                if (OPAL_ERR_NOT_FOUND != ret) {
                    error = "opal_paffinity_base_slot_list_set() returned an error";
                    goto error;
                }
                paffinity_enabled = true;
            } else if (opal_paffinity_alone) {
                /* no slot_list, but they asked for paffinity */
                int phys_cpu;
                orte_node_rank_t nrank;
                if (ORTE_NODE_RANK_INVALID == (nrank = orte_ess.get_node_rank(ORTE_PROC_MY_NAME))) {
                    error = "Could not get node rank - cannot set processor affinity";
                    goto error;
                }
                OPAL_PAFFINITY_CPU_ZERO(mask);
                phys_cpu = opal_paffinity_base_get_physical_processor_id(nrank);
                if (0 > phys_cpu) {
                    ret = phys_cpu;
                    error = "Could not get physical processor id - cannot set processor affinity";
                    goto error;
                }
                OPAL_PAFFINITY_CPU_SET(phys_cpu, mask);
                ret = opal_paffinity_base_set(mask);
                if (OPAL_SUCCESS != ret) {
                    error = "Setting processor affinity failed";
                    goto error;
                }
                paffinity_enabled = true;
            }
        }
    }
    
    /* If we were able to set processor affinity, try setting up
       memory affinity */
    if (!opal_maffinity_setup && paffinity_enabled) {
        if (OPAL_SUCCESS == opal_maffinity_base_open() &&
            OPAL_SUCCESS == opal_maffinity_base_select()) {
            opal_maffinity_setup = true;
        }
    }
    
    /* select buffered send allocator component to be used */
    ret=mca_pml_base_bsend_init(OMPI_ENABLE_THREAD_MULTIPLE);
    if( OMPI_SUCCESS != ret ) {
        error = "mca_pml_base_bsend_init() failed";
        goto error;
    }

    if (OMPI_SUCCESS != 
        (ret = mca_coll_base_find_available(OPAL_ENABLE_PROGRESS_THREADS,
                                            OMPI_ENABLE_THREAD_MULTIPLE))) {
        error = "mca_coll_base_find_available() failed";
        goto error;
    }

    if (OMPI_SUCCESS != 
        (ret = ompi_osc_base_find_available(OPAL_ENABLE_PROGRESS_THREADS,
                                           OMPI_ENABLE_THREAD_MULTIPLE))) {
        error = "ompi_osc_base_find_available() failed";
        goto error;
    }

#if OPAL_ENABLE_FT_CR == 1
    if (OMPI_SUCCESS != (ret = ompi_crcp_base_select() ) ) {
        error = "ompi_crcp_base_select() failed";
        goto error;
    }
#endif

    /* io and topo components are not selected here -- see comment
       above about the io and topo frameworks being loaded lazily */

    /* Initialize each MPI handle subsystem */
    /* initialize requests */
    if (OMPI_SUCCESS != (ret = ompi_request_init())) {
        error = "ompi_request_init() failed";
        goto error;
    }

    /* initialize info */
    if (OMPI_SUCCESS != (ret = ompi_info_init())) {
        error = "ompi_info_init() failed";
        goto error;
    }

    /* initialize error handlers */
    if (OMPI_SUCCESS != (ret = ompi_errhandler_init())) {
        error = "ompi_errhandler_init() failed";
        goto error;
    }

    /* initialize error codes */
    if (OMPI_SUCCESS != (ret = ompi_mpi_errcode_init())) {
        error = "ompi_mpi_errcode_init() failed";
        goto error;
    }
    
    /* initialize internal error codes */
    if (OMPI_SUCCESS != (ret = ompi_errcode_intern_init())) {
        error = "ompi_errcode_intern_init() failed";
        goto error;
    }
     
    /* initialize groups  */
    if (OMPI_SUCCESS != (ret = ompi_group_init())) {
        error = "ompi_group_init() failed";
        goto error;
    }

    /* initialize communicators */
    if (OMPI_SUCCESS != (ret = ompi_comm_init())) {
        error = "ompi_comm_init() failed";
        goto error;
    }

    /* initialize file handles */
    if (OMPI_SUCCESS != (ret = ompi_file_init())) {
        error = "ompi_file_init() failed";
        goto error;
    }

    /* initialize windows */
    if (OMPI_SUCCESS != (ret = ompi_win_init())) {
        error = "ompi_win_init() failed";
        goto error;
    }

    /* initialize attribute meta-data structure for comm/win/dtype */
    if (OMPI_SUCCESS != (ret = ompi_attr_init())) {
        error = "ompi_attr_init() failed";
        goto error;
    }

    /* If thread support was enabled, then setup OPAL to allow for
       them. */
    if ((OPAL_ENABLE_PROGRESS_THREADS == 1) ||
        (*provided != MPI_THREAD_SINGLE)) {
        opal_set_using_threads(true);
    }

    /* start PML/BTL's */
    ret = MCA_PML_CALL(enable(true));
    if( OMPI_SUCCESS != ret ) {
        error = "PML control failed";
        goto error;
    }

    /* add all ompi_proc_t's to PML */
    if (NULL == (procs = ompi_proc_world(&nprocs))) {
        error = "ompi_proc_world() failed";
        goto error;
    }
    ret = MCA_PML_CALL(add_procs(procs, nprocs));
    free(procs);
    /* If we got "unreachable", then print a specific error message.
       Otherwise, if we got some other failure, fall through to print
       a generic message. */
    if (OMPI_ERR_UNREACH == ret) {
        orte_show_help("help-mpi-runtime",
                       "mpi_init:startup:pml-add-procs-fail", true);
        error = NULL;
        goto error;
    } else if (OMPI_SUCCESS != ret) {
        error = "PML add procs failed";
        goto error;
    }

    MCA_PML_CALL(add_comm(&ompi_mpi_comm_world.comm));
    MCA_PML_CALL(add_comm(&ompi_mpi_comm_self.comm));

    /*
     * Dump all MCA parameters if requested
     */
    if (ompi_mpi_show_mca_params) {
       ompi_show_all_mca_params(ompi_mpi_comm_world.comm.c_my_rank, 
                                nprocs, 
                                orte_process_info.nodename);
    }

    /* Do we need to wait for a debugger? */
    ompi_wait_for_debugger();
    
    /* check for timing request - get stop time and report elapsed
     time if so, then start the clock again */
    if (timing && 0 == ORTE_PROC_MY_NAME->vpid) {
        gettimeofday(&ompistop, NULL);
        opal_output(0, "ompi_mpi_init[%ld]: time from modex to first barrier %ld usec",
                    (long)ORTE_PROC_MY_NAME->vpid,
                    (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                               (ompistop.tv_usec - ompistart.tv_usec)));
        gettimeofday(&ompistart, NULL);
    }
    
    /* wait for everyone to reach this point */
    if (OMPI_SUCCESS != (ret = orte_grpcomm.barrier())) {
        error = "orte_grpcomm_barrier failed";
        goto error;
    }
    
    /* check for timing request - get stop time and report elapsed
       time if so, then start the clock again */
    if (timing && 0 == ORTE_PROC_MY_NAME->vpid) {
        gettimeofday(&ompistop, NULL);
        opal_output(0, "ompi_mpi_init[%ld]: time to execute barrier %ld usec",
                    (long)ORTE_PROC_MY_NAME->vpid,
                    (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                               (ompistop.tv_usec - ompistart.tv_usec)));
        gettimeofday(&ompistart, NULL);
    }

#if OPAL_ENABLE_PROGRESS_THREADS == 0
    /* Start setting up the event engine for MPI operations.  Don't
       block in the event library, so that communications don't take
       forever between procs in the dynamic code.  This will increase
       CPU utilization for the remainder of MPI_INIT when we are
       blocking on ORTE-level events, but may greatly reduce non-TCP
       latency. */
    opal_progress_set_event_flag(OPAL_EVLOOP_NONBLOCK);
#endif
    
    /* wire up the mpi interface, if requested.  Do this after the
       non-block switch for non-TCP performance.  Do before the
       polling change as anyone with a complex wire-up is going to be
       using the oob. */
    if (OMPI_SUCCESS != (ret = ompi_init_preconnect_mpi())) {
        error = "ompi_mpi_do_preconnect_all() failed";
        goto error;
    }

    /* Setup the publish/subscribe (PUBSUB) framework */
    if (OMPI_SUCCESS != (ret = ompi_pubsub_base_open())) {
        error = "ompi_pubsub_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = ompi_pubsub_base_select())) {
        error = "ompi_pubsub_base_select() failed";
        goto error;
    }
    
    /* Setup the dynamic process management (DPM) framework */
    if (OMPI_SUCCESS != (ret = ompi_dpm_base_open())) {
        error = "ompi_dpm_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = ompi_dpm_base_select())) {
        error = "ompi_dpm_base_select() failed";
        goto error;
    }


    /* Determine the overall threadlevel support of all processes 
       in MPI_COMM_WORLD. This has to be done before calling 
       coll_base_comm_select, since some of the collective components
       e.g. hierarch, might create subcommunicators. The threadlevel
       requested by all processes is required in order to know
       which cid allocation algorithm can be used. */
    if ( OMPI_SUCCESS != 
	 ( ret = ompi_comm_cid_init ())) {
	error = "ompi_mpi_init: ompi_comm_cid_init failed";
	goto error;
    }

    /* Init coll for the comms. This has to be after dpm_base_select, 
       (since dpm.mark_dyncomm is not set in the communicator creation
       function else), but before dpm.dyncom_init, since this function
       might require collective for the CID allocation. */
    if (OMPI_SUCCESS !=
        (ret = mca_coll_base_comm_select(MPI_COMM_WORLD))) {
        error = "mca_coll_base_comm_select(MPI_COMM_WORLD) failed";
        goto error;
    }

    if (OMPI_SUCCESS != 
        (ret = mca_coll_base_comm_select(MPI_COMM_SELF))) {
        error = "mca_coll_base_comm_select(MPI_COMM_SELF) failed";
        goto error;
    }


    
    /* Check whether we have been spawned or not.  We introduce that
       at the very end, since we need collectives, datatypes, ptls
       etc. up and running here.... */
    if (OMPI_SUCCESS != (ret = ompi_dpm.dyn_init())) {
        error = "ompi_comm_dyn_init() failed";
        goto error;
    }

    /*
     * Startup the Checkpoint/Restart Mech.
     * Note: Always do this so tools don't hang when
     * in a non-checkpointable build
     */
    if (OMPI_SUCCESS != (ret = ompi_cr_init())) {
        error = "ompi_cr_init";
        goto error;
    }

    /* Undo OPAL calling opal_progress_event_users_increment() during 
       opal_init, to get better latency when not using TCP.  Do 
       this *after* dyn_init, as dyn init uses lots of ORTE 
       communication and we don't want to hinder the performance of 
       that code. */ 
    opal_progress_event_users_decrement(); 

    /* see if yield_when_idle was specified - if so, use it */
    param = mca_base_param_find("mpi", NULL, "yield_when_idle");
    mca_base_param_lookup_int(param, &value);
    if (value < 0) {
        /* if no info is provided, just default to conservative */
        opal_progress_set_yield_when_idle(true);
    } else {
        /* info was provided, so set idle accordingly */
        opal_progress_set_yield_when_idle(value == 0 ? false : true);
    }
    
    param = mca_base_param_find("mpi", NULL, "event_tick_rate");
    mca_base_param_lookup_int(param, &value);
    /* negative value means use default - just don't do anything */
    if (value >= 0) {
        opal_progress_set_event_poll_rate(value);
    }

    /* At this point, we are fully configured and in MPI mode.  Any
       communication calls here will work exactly like they would in
       the user's code.  Setup the connections between procs and warm
       them up with simple sends, if requested */

 error:
    if (ret != OMPI_SUCCESS) {
        /* Only print a message if one was not already printed */
        if (NULL != error) {
            const char *err_msg = opal_strerror(ret);
            /* If ORTE was not setup yet, don't use orte_show_help */
            if (orte_setup) {
                orte_show_help("help-mpi-runtime",
                               "mpi_init:startup:internal-failure", true,
                               "MPI_INIT", "MPI_INIT", error, err_msg, ret);
            } else {
                opal_show_help("help-mpi-runtime",
                               "mpi_init:startup:internal-failure", true,
                               "MPI_INIT", "MPI_INIT", error, err_msg, ret);
            }
        }
        return ret;
    }

    /* Initialize the registered datarep list to be empty */
    OBJ_CONSTRUCT(&ompi_registered_datareps, opal_list_t);

    /* Initialize the arrays used to store the F90 types returned by the
     *  MPI_Type_create_f90_XXX functions.
     */
    OBJ_CONSTRUCT( &ompi_mpi_f90_integer_hashtable, opal_hash_table_t);
    opal_hash_table_init(&ompi_mpi_f90_integer_hashtable, 16 /* why not? */);

    OBJ_CONSTRUCT( &ompi_mpi_f90_real_hashtable, opal_hash_table_t);
    opal_hash_table_init(&ompi_mpi_f90_real_hashtable, FLT_MAX_10_EXP);

    OBJ_CONSTRUCT( &ompi_mpi_f90_complex_hashtable, opal_hash_table_t);
    opal_hash_table_init(&ompi_mpi_f90_complex_hashtable, FLT_MAX_10_EXP);
    
    /* All done.  Wasn't that simple? */

    ompi_mpi_initialized = true;

    /* check for timing request - get stop time and report elapsed time if so */
    if (timing && 0 == ORTE_PROC_MY_NAME->vpid) {
        gettimeofday(&ompistop, NULL);
        opal_output(0, "ompi_mpi_init[%ld]: time from barrier to complete mpi_init %ld usec",
                    (long)ORTE_PROC_MY_NAME->vpid,
                    (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                               (ompistop.tv_usec - ompistart.tv_usec)));
    }

    /* If desired, send a notifier message that we've finished MPI_INIT */
    if (ompi_notify_init_finalize) {
        orte_notifier.log(ORTE_NOTIFIER_NOTICE,
                          ORTE_SUCCESS,
                          "MPI_INIT:Finishing on host %s, pid %d",
                          orte_process_info.nodename,
                          orte_process_info.pid);
    }

    return MPI_SUCCESS;
}
