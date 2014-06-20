/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2006 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 * 
 * One-sided Communication interface
 *
 * Interface for implementing the one-sided communication chapter of
 * the MPI-2 standard.  Similar in scope to the PML for point-to-point
 * communication from MPI-1.
 */

#ifndef OMPI_MCA_OSC_OSC_H
#define OMPI_MCA_OSC_OSC_H

#include "opal_config.h"

#ifdef HAVE_STDDEF_H
#include <stddef.h>
#endif

#include "opal/mca/mca.h"

BEGIN_C_DECLS


/* ******************************************************************** */


struct ompi_win_t;
struct ompi_info_t;
struct ompi_communicator_t;
struct ompi_group_t;
struct ompi_datatype_t;
struct ompi_op_t;


/* ******************************************************************** */


/**
 * OSC component initialization
 *
 * Initialize the given one-sided component.  This function should
 * initialize any component-level data.

 * @note The component framework is not lazily opened, so attempts
 * should be made to minimize the amount of memory allocated during
 * this function.
 *
 * @param[in] enable_progress_threads True if the component needs to
 *                                support progress threads
 * @param[in] enable_mpi_threads  True if the component needs to
 *                                support MPI_THREAD_MULTIPLE
 *
 * @retval ORTE_SUCCESS Component successfully initialized
 * @retval ORTE_ERROR   An unspecified error occurred
 */
typedef int (*ompi_osc_base_component_init_fn_t)(bool enable_progress_threads,
                                                 bool enable_mpi_threads);


/**
 * OSC component finalization
 *
 * Finalize the given one-sided component.  This function should clean
 * up any component-level data allocated during component_init().  It
 * should also clean up any data created during the lifetime of the
 * component, including any modules that are outstanding.
 *
 * @retval ORTE_SUCCESS Component successfully finalized
 * @retval ORTE_ERROR   An unspecified error occurred
 */
typedef int (*ompi_osc_base_component_finalize_fn_t)(void);


/**
 * OSC component query
 *
 * Query whether, given the info and comm, the component can be used
 * for one-sided support.  The ability to use the component for the
 * window does not mean that the component will be selected.  The win
 * argument should not be modified during this call and no memory
 * should be allocated that is associated with this window.
 *
 * @return The selection priority of the component
 *
 * @param[in]  win  The window handle, already filled in by MPI_WIN_CREATE()
 * @param[in]  info An info structure with hints from the user
 *                  regarding the usage of the component
 * @param[in]  comm The communicator specified by the user for the
 *                  basis of the group membership for the Window.
 *
 * @retval -1          The component can not be used for this window
 * @retval >= 0        The priority of the component for this window
 */
typedef int (*ompi_osc_base_component_query_fn_t)(struct ompi_win_t *win,
                                                  struct ompi_info_t *info,
                                                  struct ompi_communicator_t *comm);


/**
 * OSC component select
 *
 * This component has been selected to provide one-sided services for
 * the given window.  The win->w_osc_module field can be updated and
 * memory can be associated with this window.  The module should be
 * ready for use immediately upon return of this function, and the
 * module is responsible for providing any required collective
 * synchronization before the end of the call.  
 *
 * @note The comm is the communicator specified from the user, so
 * normal internal usage rules apply.  In other words, if you need
 * communication for the life of the window, you should call
 * comm_dup() during this function.
 * 
 * @param[in/out]  win  The window handle, already filled in by MPI_WIN_CREATE()
 * @param[in]      info An info structure with hints from the user
 *                      regarding the usage of the component
 * @param[in]      comm The communicator specified by the user for the
 *                      basis of the group membership for the Window.
 *
 * @retval ORTE_SUCCESS Component successfully selected
 * @retval ORTE_ERROR   An unspecified error occurred
 */
typedef int (*ompi_osc_base_component_select_fn_t)(struct ompi_win_t *win,
                                                   struct ompi_info_t *info,
                                                   struct ompi_communicator_t *comm);


/**
 * OSC component interface
 *
 * Component interface for the OSC framework.  A public instance of
 * this structure, called mca_osc_[component_name]_component, must
 * exist in any OSC component.
 */
struct ompi_osc_base_component_2_0_0_t {
    /** Base component description */
    mca_base_component_t osc_version;
    /** Base component data block */
    mca_base_component_data_t osc_data;
    /** Component initialization function */
    ompi_osc_base_component_init_fn_t osc_init;
    /** Query whether component is useable for give comm/info */
    ompi_osc_base_component_query_fn_t osc_query;
    /** Create module for the given window */
    ompi_osc_base_component_select_fn_t osc_select;
    /* Finalize the component infrastructure */
    ompi_osc_base_component_finalize_fn_t osc_finalize;
};
typedef struct ompi_osc_base_component_2_0_0_t ompi_osc_base_component_2_0_0_t;
typedef ompi_osc_base_component_2_0_0_t ompi_osc_base_component_t;


/* ******************************************************************** */


/**
 * Free resources associated with win
 *
 * Free all resources associated with \c win.  The component must
 * provide the barrier semantics required by MPI-2 6.2.1.  The caller
 * will guarantee that no new calls into the module are made after the
 * start of this call.  It is possible that the window is locked by
 * remote processes.  win->w_flags will have OMPI_WIN_FREED set before
 * this function is called.
 *
 * @param[in]  win  Window to free
 *
 * @retval ORTE_SUCCESS Component successfully selected
 * @retval ORTE_ERROR   An unspecified error occurred
 */
typedef int (*ompi_osc_base_module_free_fn_t)(struct ompi_win_t *win);


typedef int (*ompi_osc_base_module_put_fn_t)(void *origin_addr,
                                            int origin_count,
                                            struct ompi_datatype_t *origin_dt,
                                            int target,
                                            OPAL_PTRDIFF_TYPE target_disp,
                                            int target_count,
                                            struct ompi_datatype_t *target_dt,
                                            struct ompi_win_t *win);


typedef int (*ompi_osc_base_module_get_fn_t)(void *origin_addr,
                                            int origin_count,
                                            struct ompi_datatype_t *origin_dt,
                                            int target,
                                            OPAL_PTRDIFF_TYPE target_disp,
                                            int target_count,
                                            struct ompi_datatype_t *target_dt,
                                            struct ompi_win_t *win);


typedef int (*ompi_osc_base_module_accumulate_fn_t)(void *origin_addr,
                                                   int origin_count,
                                                   struct ompi_datatype_t *origin_dt,
                                                   int target,
                                                   OPAL_PTRDIFF_TYPE target_disp,
                                                   int target_count,
                                                   struct ompi_datatype_t *target_dt,
                                                   struct ompi_op_t *op,
                                                   struct ompi_win_t *win);


typedef int (*ompi_osc_base_module_fence_fn_t)(int assert, struct ompi_win_t *win);


typedef int (*ompi_osc_base_module_start_fn_t)(struct ompi_group_t *group,
                                              int assert,
                                              struct ompi_win_t *win);


typedef int (*ompi_osc_base_module_complete_fn_t)(struct ompi_win_t *win);


typedef int (*ompi_osc_base_module_post_fn_t)(struct ompi_group_t *group,
                                             int assert,
                                             struct ompi_win_t *win);


typedef int (*ompi_osc_base_module_wait_fn_t)(struct ompi_win_t *win);


typedef int (*ompi_osc_base_module_test_fn_t)(struct ompi_win_t *win,
                                             int *flag);


typedef int (*ompi_osc_base_module_lock_fn_t)(int lock_type,
                                             int target,
                                             int assert,
                                             struct ompi_win_t *win);


typedef int (*ompi_osc_base_module_unlock_fn_t)(int target,
                                             struct ompi_win_t *win);


/* ******************************************************************** */


/**
 * OSC module instance
 *
 * Module interface to the OSC system.  An instance of this module is
 * attached to each window.  The window contains a pointer to the base
 * module instead of a base module itself so that the component is
 * free to create a structure that inherits this one for use as the
 * module structure.
 */
struct ompi_osc_base_module_1_0_0_t {
    /** Free resources associated with the window */
    ompi_osc_base_module_free_fn_t osc_free;    

    /** Implement MPI_PUT */
    ompi_osc_base_module_put_fn_t osc_put;
    /** Implement MPI_GET */
    ompi_osc_base_module_get_fn_t osc_get;
    /** Implement MPI_ACCUMULATE */
    ompi_osc_base_module_accumulate_fn_t osc_accumulate;

    /** Implement MPI_WIN_FENCE */
    ompi_osc_base_module_fence_fn_t osc_fence;

    /* Implement MPI_WIN_START */
    ompi_osc_base_module_start_fn_t osc_start;
    /* Implement MPI_WIN_COMPLETE */
    ompi_osc_base_module_complete_fn_t osc_complete;
    /* Implement MPI_WIN_POST */
    ompi_osc_base_module_post_fn_t osc_post;
    /* Implement MPI_WIN_WAIT */
    ompi_osc_base_module_wait_fn_t osc_wait;
    /* Implement MPI_WIN_TEST */
    ompi_osc_base_module_test_fn_t osc_test;

    /* Implement MPI_WIN_LOCK */
    ompi_osc_base_module_lock_fn_t osc_lock;
    /* Implement MPI_WIN_UNLOCK */
    ompi_osc_base_module_unlock_fn_t osc_unlock;
};
typedef struct ompi_osc_base_module_1_0_0_t ompi_osc_base_module_1_0_0_t;
typedef ompi_osc_base_module_1_0_0_t ompi_osc_base_module_t;


/* ******************************************************************** */


/** Macro for use in components that are of type osc */
#define OMPI_OSC_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "osc", 2, 0, 0


/* ******************************************************************** */


END_C_DECLS


#endif /* OMPI_OSC_H */
