/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
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
/**
 * @file
 *
 * Dynamic Process Management Interface
 *
 */

#ifndef OMPI_MCA_DPM_H
#define OMPI_MCA_DPM_H

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"


#include "ompi/info/info.h"
#include "ompi/communicator/communicator.h"

BEGIN_C_DECLS

/* OMPI port definitions */
/* carry over the INVALID def */
#define OMPI_RML_TAG_INVALID                        ORTE_RML_TAG_INVALID
/* define a starting point to avoid conflicts */
#define OMPI_RML_TAG_BASE                           ORTE_RML_TAG_MAX

#define OMPI_RML_TAG_UDAPL                          OMPI_RML_TAG_BASE+1
#define OMPI_RML_TAG_OPENIB                         OMPI_RML_TAG_BASE+2
#define OMPI_RML_TAG_XOPENIB                        OMPI_RML_TAG_BASE+3
#define OMPI_RML_TAG_COMM_CID_INTRA                 OMPI_RML_TAG_BASE+4
#define OMPI_RML_TAG_XOOB                           OMPI_RML_TAG_BASE+5
#define OMPI_RML_TAG_SM_BACK_FILE_CREATED           OMPI_RML_TAG_BASE+6
#define OMPI_CRCP_COORD_BOOKMARK_TAG                OMPI_RML_TAG_BASE+7
#define OMPI_COMM_JOIN_TAG                          OMPI_RML_TAG_BASE+8

/* support for shared memory collectives */
#define OMPI_RML_TAG_COLL_SM2_BACK_FILE_CREATED     OMPI_RML_TAG_BASE+9

#define OMPI_RML_TAG_DYNAMIC                        OMPI_RML_TAG_BASE+200




/*
 * Initialize a module
 */
typedef int (*ompi_dpm_base_module_init_fn_t)(void);

/*
 * Connect/accept communications
 */
typedef int (*ompi_dpm_base_module_connect_accept_fn_t)(ompi_communicator_t *comm, int root,
                                                        char *port, bool send_first,
                                                        ompi_communicator_t **newcomm);

/**
 * Executes internally a disconnect on all dynamic communicators
 * in case the user did not disconnect them.
 */
typedef void (*ompi_dpm_base_module_disconnect_fn_t)(ompi_communicator_t *comm);

/*
 * Dynamically spawn processes
 */
typedef int (*ompi_dpm_base_module_spawn_fn_t)(int count, char **array_of_commands,
                                               char ***array_of_argv,
                                               int *array_of_maxprocs,
                                               MPI_Info *array_of_info,
                                               char *port_name);

/*
 * This routine checks, whether an application has been spawned
 * by another MPI application, or has been independently started.
 * If it has been spawned, it establishes the parent communicator.
 * Since the routine has to communicate, it should be among the last
 * steps in MPI_Init, to be sure that everything is already set up.
 */
typedef int (*ompi_dpm_base_module_dyn_init_fn_t)(void);

/*
 * Interface for mpi_finalize to call to ensure dynamically spawned procs
 * collectively finalize
 */
typedef int (*ompi_dpm_base_module_dyn_finalize_fn_t)(void);

/* this routine counts the number of different jobids of the processes
   given in a certain communicator. If there is more than one jobid,
   we mark the communicator as 'dynamic'. This is especially relevant
   for the MPI_Comm_disconnect *and* for MPI_Finalize, where we have
   to wait for all still connected processes.
*/
typedef void (*ompi_dpm_base_module_mark_dyncomm_fn_t)(ompi_communicator_t *comm);

/*
 * Open a port to interface to a dynamically spawned job - if the
 * specified tag is valid, then it will be used to form the port. Otherwise,
 * a dynamically assigned tag that is unique to this request will be provided
 */
typedef int (*ompi_dpm_base_module_open_port_fn_t)(char *port_name, orte_rml_tag_t tag);

/*
 * Converts an opaque port string to a RML process nane and tag.
 */
typedef int (*ompi_dpm_base_module_parse_port_name_t)(char *port_name,
                                                      char **hnp_uri, char **rml_uri,
                                                      orte_rml_tag_t *tag);

/* 
 * Update the routed component to make sure that the RML can send messages to
 * the remote port
 */
typedef int (*ompi_dpm_base_module_route_to_port_t)(char *rml_uri, orte_process_name_t *rproc);


/*
 * Close a port
 */
typedef int (*ompi_dpm_base_module_close_port_fn_t)(char *port_name);

/*
 * Finalize a module
 */
typedef int (*ompi_dpm_base_module_finalize_fn_t)(void);

/**
* Structure for DPM modules
 */
struct ompi_dpm_base_module_1_0_0_t {
    /** Initialization Function */
    ompi_dpm_base_module_init_fn_t              init;
    /* connect/accept */
    ompi_dpm_base_module_connect_accept_fn_t    connect_accept;
    /* disconnect */
    ompi_dpm_base_module_disconnect_fn_t        disconnect;
    /* spawn processes */
    ompi_dpm_base_module_spawn_fn_t             spawn;
    /* dyn_init */
    ompi_dpm_base_module_dyn_init_fn_t          dyn_init;
    /* dyn_finalize */
    ompi_dpm_base_module_dyn_finalize_fn_t      dyn_finalize;
    /* mark dyncomm */
    ompi_dpm_base_module_mark_dyncomm_fn_t      mark_dyncomm;
    /* open port */
    ompi_dpm_base_module_open_port_fn_t         open_port;
    /* parse port string */
    ompi_dpm_base_module_parse_port_name_t      parse_port;
    /* update route to a port */
    ompi_dpm_base_module_route_to_port_t        route_to_port;
    /* close port */
    ompi_dpm_base_module_close_port_fn_t        close_port;
    /* finalize */
    ompi_dpm_base_module_finalize_fn_t          finalize;
};
typedef struct ompi_dpm_base_module_1_0_0_t ompi_dpm_base_module_1_0_0_t;
typedef struct ompi_dpm_base_module_1_0_0_t ompi_dpm_base_module_t;

OMPI_DECLSPEC extern ompi_dpm_base_module_t ompi_dpm;


/**
 * Structure for DPM components.
 */
struct ompi_dpm_base_component_2_0_0_t {
    /** MCA base component */
    mca_base_component_t base_version;
    /** MCA base data */
    mca_base_component_data_t base_data;
};
typedef struct ompi_dpm_base_component_2_0_0_t ompi_dpm_base_component_2_0_0_t;
typedef struct ompi_dpm_base_component_2_0_0_t ompi_dpm_base_component_t;

/**
 * Macro for use in components that are of type DPM
 */
#define OMPI_DPM_BASE_VERSION_2_0_0 \
    MCA_BASE_VERSION_2_0_0, \
    "dpm", 2, 0, 0


END_C_DECLS

#endif /* OMPI_MCA_DPM_H */
