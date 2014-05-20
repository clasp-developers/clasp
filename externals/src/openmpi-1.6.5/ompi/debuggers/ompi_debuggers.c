/* -*- Mode: C; c-basic-offset:4 ; -*- */
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
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c)      2012 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/*
 * MPI portion of debugger support: initially based on the
 * TotalView/Etnus API for debuggers to attach to MPI jobs.
 *
 * There is a lengthy explanation of how OMPI handles parallel
 * debuggers attaching to MPI jobs in orte/tools/orterun/debuggers.c.
 */

#include "ompi_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/mca/base/base.h"
#include "opal/util/argv.h"
#include "opal/mca/installdirs/installdirs.h"
#include "debuggers.h"
/**
 * BEWARE: The following headers are required by optimized builds in order
 * to get access to the type information. Some compilers remove all type
 * information on optimized build, and as a result we are unable to access
 * the fields structure (i.e. to get their displacement). This file is
 * included in the optimized build just to provide us with this missing
 * informations. Therefore, it always have to be compiled with the -g flag,
 * otherwise the type information will be missing and the parallel
 * debuggers will be unable to initialize the Open MPI debug library.
 */
#include "opal/class/opal_list.h"
#include "ompi/class/ompi_free_list.h"
#include "ompi/request/request.h"
#include "ompi/mca/pml/base/pml_base_request.h"
#include "ompi/mca/pml/base/pml_base_sendreq.h"
#include "ompi/mca/pml/base/pml_base_recvreq.h"
#include "opal/class/opal_pointer_array.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/topo/topo.h"
#include "ompi/group/group.h"
#include "opal/datatype/opal_datatype.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/include/mpi.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/runtime/orte_globals.h"

#if defined(OMPI_MSGQ_DLL)
/* This variable is old/deprecated -- the mpimsgq_dll_locations[]
   method is preferred because it's more flexible */
OMPI_DECLSPEC char MPIR_dll_name[] = OMPI_MSGQ_DLL;
#endif  /* defined(OMPI_MSGQ_DLL) */
OMPI_DECLSPEC char **mpidbg_dll_locations = NULL;
OMPI_DECLSPEC char **mpimsgq_dll_locations = NULL;

OMPI_DECLSPEC int MPIR_debug_typedefs_sizeof[] = {
    sizeof(short),
    sizeof(int),
    sizeof(long),
    sizeof(long long),
    sizeof(void*),
    sizeof(bool),
    sizeof(size_t)
};
    
/*
 * Values defined by the standardized interface; do not change these
 * values
 */
#define MPIR_DEBUG_SPAWNED   1
#define MPIR_DEBUG_ABORTING  2

/**
 * BEWARE: Try to outsmart some compilers. In some cases, when variables
 * are defined but not used, some compilers will optimized them out from
 * the build. As we need to be able to access the structure sizes from the
 * debugged program (in the case where the MPI library and the application
 * is compiled with a different architecture flag than the parallel
 * debugger, 32 vs. 64 bits), we have to have these variables defined.
 */
OMPI_DECLSPEC opal_list_item_t* opal_list_item_t_type_force_inclusion = NULL;
OMPI_DECLSPEC opal_list_t* opal_list_t_type_force_inclusion = NULL;
OMPI_DECLSPEC ompi_free_list_item_t* ompi_free_list_item_t_type_force_inclusion = NULL;
OMPI_DECLSPEC ompi_free_list_t* ompi_free_list_t_type_force_inclusion = NULL;
OMPI_DECLSPEC ompi_request_t* ompi_request_t_type_force_inclusion = NULL;
OMPI_DECLSPEC mca_pml_base_request_t* mca_pml_base_request_t_type_force_inclusion = NULL;
OMPI_DECLSPEC mca_pml_base_send_request_t* mca_pml_base_send_request_t_type_force_inclusion = NULL;
OMPI_DECLSPEC mca_pml_base_recv_request_t* mca_pml_base_recv_request_t_type_force_inclusion = NULL;
OMPI_DECLSPEC opal_pointer_array_t* opal_pointer_array_t_type_force_inclusion = NULL;
OMPI_DECLSPEC ompi_communicator_t* ompi_communicator_t_type_force_inclusion = NULL;
OMPI_DECLSPEC ompi_group_t* ompi_group_t_type_force_inclusion = NULL;
OMPI_DECLSPEC ompi_status_public_t* ompi_status_public_t_type_force_inclusion = NULL;
OMPI_DECLSPEC opal_datatype_t* opal_datatype_t_type_force_inclusion = NULL;
OMPI_DECLSPEC ompi_datatype_t* ompi_datatype_t_type_force_inclusion = NULL;

OMPI_DECLSPEC volatile int MPIR_debug_gate = 0;
ORTE_DECLSPEC volatile int MPIR_being_debugged;
ORTE_DECLSPEC volatile int MPIR_debug_state;
OMPI_DECLSPEC char *MPIR_debug_abort_string = "";

/* Check for a file in few direct ways for portability */
static void check(char *dir, char *file, char **locations) 
{
    char *str;

    asprintf(&str, "%s/%s.so", dir, file);
    
#if defined(HAVE_SYS_STAT_H)
    {
        struct stat buf;
        
        /* Use stat() */
        if (0 == stat(str, &buf)) {
            opal_argv_append_nosize(&locations, file);
        }
    }
#else
    {
        FILE *fp;
        
        /* Just try to open the file */
        if (NULL != (fp = fopen(str, "r"))) {
            fclose(fp);
            opal_argv_append_nosize(&locations, file);
        }
    }
#endif /* defined(HAVE_SYS_STAT_H) */

    free(str);
}


/*
 * Wait for a debugger if asked.  We support two ways of waiting for
 * attaching debuggers -- see big comment in
 * orte/tools/orterun/debuggers.c explaning the two scenarios.
 */
void ompi_wait_for_debugger(void)
{
    int i, debugger, rc;
    char *a, *b, **dirs, **tmp1 = NULL, **tmp2 = NULL;
    opal_buffer_t buf;

    /* See lengthy comment in orte/tools/orterun/orterun.c about
       orte_in_parallel_debugger */
#if ORTE_DISABLE_FULL_SUPPORT
    debugger = 0;
#else
    debugger = orte_in_parallel_debugger;
#endif

    /* Add in environment variables for other launchers, such as yod,
       srun, ...etc. */
    if (1 == MPIR_being_debugged) {
        debugger = 1;
    } else if (NULL != getenv("yod_you_are_being_debugged")) {
        debugger = 1;
    }
    if (1 == MPIR_being_debugged) {
        debugger = 1;
    }
    
    if (!debugger) {
        /* if not, just return */
        return;
    }
    
    /* if we are being debugged, then we need to find
     * the correct plug-ins
     */
    a = strdup(opal_install_dirs.pkglibdir);
    mca_base_param_reg_string_name("ompi",
                                   "debugger_dll_path",
                                   "List of directories where MPI_INIT should search for debugger plugins",
                                   false, false, a, &b);
    free(a);
    
    /* Search the directory for MPI debugger DLLs */
    if (NULL != b) {
        dirs = opal_argv_split(b, ':');
        for (i = 0; dirs[i] != NULL; ++i) {
            check(dirs[i], OMPI_MPIHANDLES_DLL_PREFIX, tmp1);
            check(dirs[i], OMPI_MSGQ_DLL_PREFIX, tmp2);
        }
    }

    /* Now that we have a full list of directories, assign the argv
       arrays to the global variables (since the debugger may read the
       global variables at any time, we want to ensure that they have
       non-NULL values only when the entire array is ready). */
    mpimsgq_dll_locations = tmp1;
    mpidbg_dll_locations = tmp2;

    if (ORTE_DISABLE_FULL_SUPPORT || orte_standalone_operation) {
        /* spin until debugger attaches and releases us */
        while (MPIR_debug_gate == 0) {
#if defined(__WINDOWS__)
            Sleep(100);     /* milliseconds */
#elif defined(HAVE_USLEEP)
            usleep(100000); /* microseconds */
#else
            sleep(1);       /* seconds */
#endif
        }
    } else {
    
        /* only the rank=0 proc waits for either a message from the
         * HNP or for the debugger to attach - everyone else will just
         * spin in * the grpcomm barrier in ompi_mpi_init until rank=0
         * joins them.
         */
        if (0 != ORTE_PROC_MY_NAME->vpid) {
            return;
        }
    
        /* VPID 0 waits for a message from the HNP */
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        rc = orte_rml.recv_buffer(ORTE_NAME_WILDCARD, &buf, 
                                  ORTE_RML_TAG_DEBUGGER_RELEASE, 0);
        OBJ_DESTRUCT(&buf);  /* don't care about contents of message */
        if (rc < 0) {
            /* if it failed for some reason, then we are in trouble -
             * for now, just report the problem and give up waiting
             */
            opal_output(0, "Debugger_attach[rank=%ld]: could not wait for debugger - error %s!",
                        (long)ORTE_PROC_MY_NAME->vpid, ORTE_ERROR_NAME(rc));
        }
    }
}    

/*
 * Tell the debugger that we are about to abort
 */
void ompi_debugger_notify_abort(char *reason)
{
    MPIR_debug_state = MPIR_DEBUG_ABORTING;

    if (NULL != reason && strlen(reason) > 0) {
        MPIR_debug_abort_string = reason;
    } else {
        MPIR_debug_abort_string = "Unknown";
    }

    /* Now tell the debugger */
    MPIR_Breakpoint();
}

/* 
 * Breakpoint function for parallel debuggers.  This function is also 
 * defined in orterun for the starter.  It should never conflict with 
 * this 
 */
void* MPIR_Breakpoint(void)
{
    return NULL;
}
