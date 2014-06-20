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
 *
 */

/** @file
 * MCA coll base framework public interface functions.
 *
 * These functions are normally invoked by the back-ends of:
 *
 * - The back-ends of MPI_Init() and MPI_Finalize()
 * - Communuicactor constructors (e.g., MPI_Comm_split()) and
 *   destructors (e.g., MPI_Comm_free())
 * - The laminfo command
 */

#ifndef MCA_COLL_BASE_H
#define MCA_COLL_BASE_H

#include "ompi_config.h"

#include "mpi.h"
#include "opal/class/opal_list.h"


/*
 * Global functions for MCA overall collective open and close
 */

BEGIN_C_DECLS
/**
 * Initialize the coll MCA framework
 *
 * @retval OMPI_SUCCESS Upon success
 * @retval OMPI_ERROR Upon failure
 *
 * This must be the first function invoked in the coll MCA
 * framework.  It initializes the coll MCA framework, finds and
 * opens coll components, etc.
 *
 * This function is invoked during ompi_mpi_init() and during the
 * initialization of the special case of the laminfo command.
 * 
 * This function fills in the internal global variable
 * mca_coll_base_components_opened, which is a list of all coll components
 * that were successfully opened.  This variable should \em only be
 * used by other coll base functions -- it is not considered a
 * public interface member -- and is only mentioned here for
 * completeness.
 */
OMPI_DECLSPEC int mca_coll_base_open(void);

/**
 * Create list of available coll components.
 *
 * @param allow_multi_user_threads Will be set to true if any of the
 * available components will allow multiple user threads
 * @param have_hidden_threads Will be set to true if any of the
 * available components have hidden threads.
 *
 * @retval OMPI_SUCCESS If one or more coll components are available.
 * @retval OMPI_ERROR If no coll components are found to be available.
 *
 * This function is invoked during ompi_mpi_init() to query all
 * successfully opened coll components and create a list of all
 * available coll components.
 *
 * This function traverses the (internal global variable)
 * mca_coll_base_components_opened list and queries each component to see
 * if it ever might want to run during this MPI process.  It creates
 * another internal global variable list named
 * mca_coll_base_components_available, consisting of a list of components
 * that are available for selection when communicators are created.
 * This variable should \em only be used by other coll base
 * functions -- it is not considered a public interface member --
 * and is only mentioned here for completeness.
 */
int mca_coll_base_find_available(bool enable_progress_threads,
                                 bool enable_mpi_threads);

/**
 * Select an available component for a new communicator.
 *
 * @param comm Communicator that the component will be selected for.
 * @param preferred The component that is preferred for this
 * communicator (or NULL).
 *
 * @return OMPI_SUCCESS Upon success.
 * @return OMPI_ERROR Upon failure.
 *
 * Note that the types of the parameters have "struct" in them
 * (e.g., ompi_communicator_t" vs. a plain "ompi_communicator_t") to
 * avoid an include file loop.  All similar types (e.g., "struct
 * ompi_communicator_t *", "ompi_communicator_t *", and "MPI_Comm")
 * are all typedef'ed to be the same, so the fact that we use struct
 * here in the prototype is ok.
 *
 * This function is invoked when a new communicator is created and a
 * coll component needs to be selected for it.  It should be invoked
 * near the end of the communicator creation process such that
 * almost everything else is functional on the communicator (e.g.,
 * point-to-point communication).  
 *
 * Note that new communicators may be created as a result of
 * invoking this function.  Specifically: this function is called in
 * the depths of communicator creation, but during the execution of
 * this function, new communicators may be created, and therefore
 * communicator creation functions may be re-entered (albiet with
 * different arguments).
 */
int mca_coll_base_comm_select(struct ompi_communicator_t *comm);

/**
 * Finalize a coll component on a specific communicator.
 *
 * @param comm The communicator that is being destroyed.
 *
 * @retval OMPI_SUCCESS Always.
 *
 * Note that the type of the parameter is only a "struct
 * ompi_communicator_t" (vs. a plain "ompi_communicator_t") to avoid
 * an include file loop.  The types "struct ompi_communicator_t *",
 * "ompi_communicator_t *", and "MPI_Comm" are all typedef'ed to be
 * the same, so the fact that we use struct here in the prototype is
 * ok.
 *
 * This function is invoked near the beginning of the destruction of
 * a communicator.  It finalizes the coll component associated with the
 * communicator (e.g., allowing the component to clean up and free any
 * resources allocated for that communicator).  Note that similar to
 * mca_coll_base_select(), as result of this function, other
 * communicators may also be destroyed.
 */
int mca_coll_base_comm_unselect(struct ompi_communicator_t *comm);

/**
 * Shut down the coll MCA framework.
 *
 * @retval OMPI_SUCCESS Always
 *
 * This function shuts down everything in the coll MCA framework,
 * and is called during ompi_mpi_finalize() and the special case of
 * the laminfo command.
 *
 * It must be the last function invoked on the coll MCA framework.
 */
OMPI_DECLSPEC int mca_coll_base_close(void);


/*
 * Globals
 */

/**
 * Coll framework debugging stream ID used with opal_output() and
 * opal_output_verbose().
 */
OMPI_DECLSPEC extern int mca_coll_base_output;

/**
 * Indicator as to whether the list of opened coll components is valid or
 * not.
 */
extern bool mca_coll_base_components_opened_valid;

/**
 * List of all opened components; created when the coll framework is
 * initialized and destroyed when we reduce the list to all available
 * coll components.
 */
OMPI_DECLSPEC extern opal_list_t mca_coll_base_components_opened;

/**
 * Indicator as to whether the list of available coll components is valid
 * or not.
 */
extern bool mca_coll_base_components_available_valid;

/**
 * List of all available components; created by reducing the list of open
 * components to all those who indicate that they may run during this
 * process.
 */
extern opal_list_t mca_coll_base_components_available;

END_C_DECLS
#endif /* MCA_BASE_COLL_H */
