/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007-2008 UT-Battelle, LLC
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * Collective Communication Interface
 *
 * Interface for implementing the collective communication interface
 * of MPI.  The MPI interface provides error checking and error
 * handler invocation, but the collective components provide all other
 * functionality.
 *
 * Component selection is done per commuicator, at Communicator
 * construction time.  mca_coll_base_comm_select() is used to
 * create the list of components available to the compoenent
 * collm_comm_query function, instantiating a module for each
 * component that i usable, and sets the module collective function pointers.
 * mca_coll_base_comm_select() then loops through the list of available
 * components (via the instantiated module), and uses the 
 * module's coll_module_enable() function to enable the modules, and
 * if successful, sets the communicator collective functions to the
 * those supplied by the given module, keeping track of which module it
 * is associated with.
 *
 * The module destructors are called for each module used by the
 * communicator, at communicator desctruction time.
 *
 * This can result in up to N different components being used for a 
 * single communicator, one per needed collective function.
 *
 * The interface is the same for inter- or intra-communicators, and
 * components should be able to handle either style of communicator
 * during initialization (although handling may include indicating the
 * component is not available).
 */

#ifndef OMPI_MCA_COLL_COLL_H
#define OMPI_MCA_COLL_COLL_H

#include "ompi_config.h"

#include "mpi.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

BEGIN_C_DECLS


/* ******************************************************************** */


struct ompi_communicator_t;
struct ompi_datatype_t;
struct ompi_op_t;


/* ******************************************************************** */


/**
 * Collective component initialization
 *
 * Initialize the given collective component.  This function should
 * initialize any component-level. data.  It will be called exactly
 * once during MPI_INIT.
 *
 * @note The component framework is not lazily opened, so attempts
 * should be made to minimze the amount of memory allocated during
 * this function.
 *
 * @param[in] enable_progress_threads True if the component needs to
 *                                support progress threads
 * @param[in] enable_mpi_threads  True if the component needs to
 *                                support MPI_THREAD_MULTIPLE
 *
 * @retval OMPI_SUCCESS Component successfully initialized
 * @retval OMPI_ERROR   An unspecified error occurred
 */
typedef int (*mca_coll_base_component_init_query_fn_t)
     (bool enable_progress_threads, bool enable_mpi_threads);



/**
 * Query whether a component is available for the given communicator
 *
 * Query whether the component is available for the given
 * communicator.  If the component is available, an object should be
 * allocated and returned (with refcount at 1).  The module will not
 * be used for collective operations until module_enable() is called
 * on the module, but may be destroyed (via OBJ_RELEASE) either before
 * or after module_enable() is called.  If the module needs to release
 * resources obtained during query(), it should do so in the module
 * destructor.
 *
 * A component may provide NULL to this function to indicate it does
 * not wish to run or return an error during module_enable().
 *
 * @note The communicator is available for point-to-point
 * communication, but other functionality is not available during this
 * phase of initialization.
 *
 * @param[in] comm        The communicator being created
 * @param[out] priority   Priority setting for component on 
 *                        this communicator
 *
 * @returns An initialized module structure if the component can
 * provide a module with the requested functionality or NULL if the
 * component should not be used on the given communicator.
 */
typedef struct mca_coll_base_module_2_0_0_t *
  (*mca_coll_base_component_comm_query_2_0_0_fn_t)
    (struct ompi_communicator_t *comm, int *priority);


/* ******************************************************************** */


/**
 * Enable module for collective communication
 *
 * Enable the module for collective commuication.  Modules are enabled
 * in order from lowest to highest priority.  At each component,
 * collective functions with priority higher than the existing
 * function are copied into the communicator's function table and the
 * module's reference count is incremented.  Replaced functions have
 * their module's reference count decremented, so a component will go
 * out of scope when it has been examined and is no longer used in any
 * collective functions.
 *
 * Because the function list is built on increasing priority, a
 * component that needs functions from a lower priority component
 * (say, a multi-cast barrier that might need a point-to-point barrier
 * for resource exhaustion issues) can keep the function pointer and
 * module pointer and increase the reference count of the module and
 * use the module during execution.
 *
 * When a module is not used for any interface functions and no
 * higher-priority module has increased its refcount, it will have
 * it's destructor triggered and the module will be destroyed.
 *
 * @note The collective component should not modify the communicator
 * during this operation.  The communicator will be updated with the
 * collective algorithm's function pointers and module (and the ref
 * count increased on the module) by the base selection functionality.
 *
 * @param[in/out] module     Module created during comm_query()
 * @param[in]     comm       Communicator being created
 */
typedef int
(*mca_coll_base_module_enable_1_1_0_fn_t)(struct mca_coll_base_module_2_0_0_t* module,
                                          struct ompi_communicator_t *comm);


typedef int (*mca_coll_base_module_allgather_fn_t)
  (void *sbuf, int scount, struct ompi_datatype_t *sdtype, 
   void *rbuf, int rcount, struct ompi_datatype_t *rdtype, 
   struct ompi_communicator_t *comm, struct mca_coll_base_module_2_0_0_t *module);
typedef int (*mca_coll_base_module_allgatherv_fn_t)
  (void *sbuf, int scount, struct ompi_datatype_t *sdtype, 
   void * rbuf, int *rcounts, int *disps,  struct ompi_datatype_t *rdtype, 
   struct ompi_communicator_t *comm, struct mca_coll_base_module_2_0_0_t *module);
typedef int (*mca_coll_base_module_allreduce_fn_t)
  (void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype, 
   struct ompi_op_t *op, struct ompi_communicator_t *comm, struct mca_coll_base_module_2_0_0_t *module);
typedef int (*mca_coll_base_module_alltoall_fn_t)
  (void *sbuf, int scount, struct ompi_datatype_t *sdtype, 
   void* rbuf, int rcount, struct ompi_datatype_t *rdtype, 
   struct ompi_communicator_t *comm, struct mca_coll_base_module_2_0_0_t *module);
typedef int (*mca_coll_base_module_alltoallv_fn_t)
  (void *sbuf, int *scounts, int *sdisps, struct ompi_datatype_t *sdtype, 
   void *rbuf, int *rcounts, int *rdisps, struct ompi_datatype_t *rdtype, 
   struct ompi_communicator_t *comm, struct mca_coll_base_module_2_0_0_t *module);
typedef int (*mca_coll_base_module_alltoallw_fn_t)
  (void *sbuf, int *scounts, int *sdisps, struct ompi_datatype_t **sdtypes, 
   void *rbuf, int *rcounts, int *rdisps, struct ompi_datatype_t **rdtypes, 
   struct ompi_communicator_t *comm, struct mca_coll_base_module_2_0_0_t *module);
typedef int (*mca_coll_base_module_barrier_fn_t)
  (struct ompi_communicator_t *comm, struct mca_coll_base_module_2_0_0_t *module);
typedef int (*mca_coll_base_module_bcast_fn_t)
  (void *buff, int count, struct ompi_datatype_t *datatype, int root,
   struct ompi_communicator_t *comm, struct mca_coll_base_module_2_0_0_t *module);
typedef int (*mca_coll_base_module_exscan_fn_t)
  (void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype, 
   struct ompi_op_t *op, struct ompi_communicator_t *comm, struct mca_coll_base_module_2_0_0_t *module);
typedef int (*mca_coll_base_module_gather_fn_t)
  (void *sbuf, int scount, struct ompi_datatype_t *sdtype, 
   void *rbuf, int rcount, struct ompi_datatype_t *rdtype, 
   int root, struct ompi_communicator_t *comm, struct mca_coll_base_module_2_0_0_t *module);
typedef int (*mca_coll_base_module_gatherv_fn_t)
  (void *sbuf, int scount, struct ompi_datatype_t *sdtype, 
   void *rbuf, int *rcounts, int *disps, struct ompi_datatype_t *rdtype, 
   int root, struct ompi_communicator_t *comm, struct mca_coll_base_module_2_0_0_t *module);
typedef int (*mca_coll_base_module_reduce_fn_t)
  (void *sbuf, void* rbuf, int count, struct ompi_datatype_t *dtype, 
   struct ompi_op_t *op, int root, struct ompi_communicator_t *comm, struct mca_coll_base_module_2_0_0_t *module);
typedef int (*mca_coll_base_module_reduce_scatter_fn_t)
  (void *sbuf, void *rbuf, int *rcounts, struct ompi_datatype_t *dtype,
   struct ompi_op_t *op, struct ompi_communicator_t *comm, struct mca_coll_base_module_2_0_0_t *module);
typedef int (*mca_coll_base_module_scan_fn_t)
  (void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype, 
   struct ompi_op_t *op, struct ompi_communicator_t *comm, struct mca_coll_base_module_2_0_0_t *module);
typedef int (*mca_coll_base_module_scatter_fn_t)
  (void *sbuf, int scount, struct ompi_datatype_t *sdtype, 
   void *rbuf, int rcount, struct ompi_datatype_t *rdtype, 
   int root, struct ompi_communicator_t *comm, struct mca_coll_base_module_2_0_0_t *module);
typedef int (*mca_coll_base_module_scatterv_fn_t)
  (void *sbuf, int *scounts, int *disps, struct ompi_datatype_t *sdtype, 
   void* rbuf, int rcount, struct ompi_datatype_t *rdtype,
   int root, struct ompi_communicator_t *comm, struct mca_coll_base_module_2_0_0_t *module);


/**
 * Fault Tolerance Awareness function.
 *
 * Fault tolerance function -- called when a process / job state
 * change is noticed.
 *
 * @param[in] state    State change that triggered the function
 *
 * @retval OMPI_SUCCESS Component successfully selected
 * @retval OMPI_ERROR   An unspecified error occurred
 */
typedef int (*mca_coll_base_module_ft_event_fn_t) (int state);


/* ******************************************************************** */


/**
 * Collective component interface
 *
 * Component interface for the collective framework.  A public
 * instance of this structure, called
 * mca_coll_[component_name]_component, must exist in any collective
 * component.
 */
struct mca_coll_base_component_2_0_0_t {
    /** Base component description */
    mca_base_component_t collm_version;
    /** Base component data block */
    mca_base_component_data_t collm_data;

    /** Component initialization function */
    mca_coll_base_component_init_query_fn_t collm_init_query;
    /** Query whether component is useable for given communicator */
    mca_coll_base_component_comm_query_2_0_0_fn_t collm_comm_query;
};
typedef struct mca_coll_base_component_2_0_0_t mca_coll_base_component_2_0_0_t;

/** Per guidence in mca.h, use the unversioned struct name if you just
    want to always keep up with the most recent version of the
    interace. */
typedef struct mca_coll_base_component_2_0_0_t mca_coll_base_component_t;


/**
 * Collective module interface
 *
 * Module interface to the Collective framework.  Modules are
 * reference counted based on the number of functions from the module
 * used on the commuicator.  There is at most one module per component
 * on a given communicator, and there can be many component modules on
 * a given communicator.  
 *
 * @note The collective framework and the
 * communicator functionality only stores a pointer to the module
 * function, so the component is free to create a structure that
 * inherits from this one for use as the module structure.
 */
struct mca_coll_base_module_2_0_0_t {
    /** Collective modules all inherit from opal_object */
    opal_object_t super;

    /** Enable function called when a collective module is (possibly)
        going to be used for the given communicator */
    mca_coll_base_module_enable_1_1_0_fn_t coll_module_enable;

    /* Collective function pointers */
    mca_coll_base_module_allgather_fn_t coll_allgather;
    mca_coll_base_module_allgatherv_fn_t coll_allgatherv;
    mca_coll_base_module_allreduce_fn_t coll_allreduce;
    mca_coll_base_module_alltoall_fn_t coll_alltoall;
    mca_coll_base_module_alltoallv_fn_t coll_alltoallv;
    mca_coll_base_module_alltoallw_fn_t coll_alltoallw;
    mca_coll_base_module_barrier_fn_t coll_barrier;
    mca_coll_base_module_bcast_fn_t coll_bcast;
    mca_coll_base_module_exscan_fn_t coll_exscan;
    mca_coll_base_module_gather_fn_t coll_gather;
    mca_coll_base_module_gatherv_fn_t coll_gatherv;
    mca_coll_base_module_reduce_fn_t coll_reduce;
    mca_coll_base_module_reduce_scatter_fn_t coll_reduce_scatter;
    mca_coll_base_module_scan_fn_t coll_scan;
    mca_coll_base_module_scatter_fn_t coll_scatter;
    mca_coll_base_module_scatterv_fn_t coll_scatterv;

    /** Fault tolerance event trigger function */
    mca_coll_base_module_ft_event_fn_t ft_event;
};
typedef struct mca_coll_base_module_2_0_0_t mca_coll_base_module_2_0_0_t;

/** Per guidence in mca.h, use the unversioned struct name if you just
    want to always keep up with the most recent version of the
    interace. */
typedef struct mca_coll_base_module_2_0_0_t mca_coll_base_module_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_coll_base_module_t);

/**
 * Collectives communicator cache structure
 *
 * Collectives communicator cache structure, used to find functions to
 * implement collective algorithms and their associated modules.  This
 * function may also be used internally by a module if it needs to
 * keep a large number of "backing" functions, such as the demo
 * component.
 */
struct mca_coll_base_comm_coll_t {
    mca_coll_base_module_allgather_fn_t coll_allgather;
    mca_coll_base_module_2_0_0_t *coll_allgather_module;
    mca_coll_base_module_allgatherv_fn_t coll_allgatherv;
    mca_coll_base_module_2_0_0_t *coll_allgatherv_module;
    mca_coll_base_module_allreduce_fn_t coll_allreduce;
    mca_coll_base_module_2_0_0_t *coll_allreduce_module;
    mca_coll_base_module_alltoall_fn_t coll_alltoall;
    mca_coll_base_module_2_0_0_t *coll_alltoall_module;
    mca_coll_base_module_alltoallv_fn_t coll_alltoallv;
    mca_coll_base_module_2_0_0_t *coll_alltoallv_module;
    mca_coll_base_module_alltoallw_fn_t coll_alltoallw;
    mca_coll_base_module_2_0_0_t *coll_alltoallw_module;
    mca_coll_base_module_barrier_fn_t coll_barrier;
    mca_coll_base_module_2_0_0_t *coll_barrier_module;
    mca_coll_base_module_bcast_fn_t coll_bcast;
    mca_coll_base_module_2_0_0_t *coll_bcast_module;
    mca_coll_base_module_exscan_fn_t coll_exscan;
    mca_coll_base_module_2_0_0_t *coll_exscan_module;
    mca_coll_base_module_gather_fn_t coll_gather;
    mca_coll_base_module_2_0_0_t *coll_gather_module;
    mca_coll_base_module_gatherv_fn_t coll_gatherv;
    mca_coll_base_module_2_0_0_t *coll_gatherv_module;
    mca_coll_base_module_reduce_fn_t coll_reduce;
    mca_coll_base_module_2_0_0_t *coll_reduce_module;
    mca_coll_base_module_reduce_scatter_fn_t coll_reduce_scatter;
    mca_coll_base_module_2_0_0_t *coll_reduce_scatter_module;
    mca_coll_base_module_scan_fn_t coll_scan;
    mca_coll_base_module_2_0_0_t *coll_scan_module;
    mca_coll_base_module_scatter_fn_t coll_scatter;
    mca_coll_base_module_2_0_0_t *coll_scatter_module;
    mca_coll_base_module_scatterv_fn_t coll_scatterv;
    mca_coll_base_module_2_0_0_t *coll_scatterv_module;
};
typedef struct mca_coll_base_comm_coll_t mca_coll_base_comm_coll_t;


/* ******************************************************************** */


/*
 * Macro for use in components that are of type coll
 */
#define MCA_COLL_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "coll", 2, 0, 0


/* ******************************************************************** */


END_C_DECLS

#endif /* MCA_COLL_H */
