/*
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * This interface is designed to hide the back-end details of how IB
 * RC connections are made from the rest of the openib BTL.  There are
 * module-like instances of the implemented functionality (dlopen and
 * friends are not used, but all the functionality is accessed through
 * struct's of function pointers, so you can swap between multiple
 * different implementations at run time, just like real components).
 * Hence, these entities are referred to as "Connect
 * Pseudo-Components" (CPCs).
 *
 * The CPCs are referenced by their names (e.g., "oob", "rdma_cm").
 *
 * CPCs are split into components and modules, similar to all other
 * MCA frameworks in this code base.
 *
 * Before diving into the CPC interface, let's discuss some
 * terminology and mappings of data structures:
 *
 * - a BTL module represents a network port (in the case of the openib
 *   BTL, a LID)
 * - a CPC module represents one way to make connections to a BTL module 
 * - hence, a BTL module has potentially multiple CPC modules
 *   associated with it
 * - an endpoint represnts a connection between a local BTL module and 
 *   a remote BTL module (in the openib BTL, because of BSRQ, an 
 *   endpoint can contain multiple QPs) 
 * - when an endpoint is created, one of the CPC modules associated
 *   with the local BTL is selected and associated with the endpoint 
 *   (obviously, it is a CPC module that is common between the local
 *   and remote BTL modules) 
 * - endpoints may be created and destroyed during the MPI job 
 * - endpoints are created lazily, during the first communication 
 *   between two peers 
 * - endpoints are destroyed when two MPI processes become 
 *   disconnected (e.g., MPI-2 dynamics or MPI_FINALIZE) 
 * - hence, BTL modules and CPC modules outlive endpoints. 
 *   Specifically, BTL modules and CPC modules live from MPI_INIT to 
 *   MPI_FINALIZE. endpoints come and go as MPI semantics demand it. 
 * - therefore, CPC modules need to cache information on endpoints that 
 *   are specific to that connection. 
 *
 * Component interface:
 *
 * - component_register(): The openib BTL's component_open() function
 * calls the connect_base_register() function, which scans all
 * compiled-in CPC's.  If they have component_register() functions,
 * they are called (component_register() functions are only allowed to
 * register MCA parameters).  
 *
 * NOTE: The connect_base_register() function will process the
 * btl_openib_cpc_include and btl_openib_cpc_exclude MCA parameters
 * and automatically include/exclude CPCs as relevant.  If a CPC is
 * excluded, none of its other interface functions will be invoked for
 * the duration of the process.
 *
 * - component_init(): The openib BTL's component_init() function
 * calls connect_base_init(), which will invoke this query function on
 * each CPC to see if it wants to run at all.  CPCs can gracefully
 * remove themselves from consideration in this process by returning
 * OMPI_ERR_NOT_SUPPORTED.
 *
 * - component_query(): The openib BTL's init_one_port() calls the
 * connect_base_select_for_local_port() function, which, for each LID
 * on that port, calls the component_query() function on every
 * available CPC on that LID.  This function is intended to see if a
 * CPC can run on a sepcific openib BTL module (i.e., LID).  If it
 * can, the CPC is supposed to create a CPC module that is specific to
 * that BTL/LID and return it.  If it cannot, it should return
 * OMPI_ERR_NOT_SUPPORTED and be gracefully skipped for this
 * OpenFabrics port.
 *
 * component_finalize(): The openib BTL's component_close() function
 * calls connect_base_finalize(), which, in turn, calls the
 * component_finalize() function on all available CPCs.  Note that all
 * CPC modules will have been finalized by this point; the CPC
 * component_finalize() function is a chance for the CPC to clean up
 * any component-specific resources.
 *
 * Module interface:
 *
 * cbm_component member: A pointer pointing to the single, global
 * instance of the CPC component.  This member is used for creating a
 * unique index representing the modules' component so that it can be
 * shared with remote peer processes.
 *
 * cbm_priority member: An integer between 0 and 100, inclusive,
 * representing the priority of this CPC.
 *
 * cbm_modex_message member: A pointer to a blob buffer that will be
 * included in the modex message for this port for this CPC (it is
 * assumed that this blob is a) only understandable by the
 * corresponding CPC in the peer process, and b) contains specific
 * addressing/contact information for *this* port's CPC module).
 *
 * cbm_modex_message_len member: The length of the cbm_modex_message
 * blob, in bytes.
 *
 * cbm_endpoint_init(): Called during endpoint creation, allowing a
 * CPC module to cache information on the endpoint.  A pointer to the
 * endpoint's CPC module is already cached on the endpoint.
 *
 * cbm_start_connect(): initiate a connection to a remote peer.  The
 * CPC is responsible for setting itself up for asyncronous operation
 * for progressing the outgoing connection request.
 *
 * cbm_endpoint_finalize(): Called during the endpoint destrouction,
 * allowing the CPC module to destroy anything that it cached on the
 * endpoint.
 *
 * cbm_finalize(): shut down all asynchronous handling and clean up
 * any state that was setup for this CPC module/BTL.  Some CPCs setup
 * asynchronous support on a per-HCA/NIC basis (vs. per-port/LID).  It
 * is the reponsibility of the CPC to figure out such issues (e.g.,
 * via reference counting) -- there is no notification from the
 * upper-level BTL about when an entire HCA/NIC is no longer being
 * used.  There is only this function, which tells when a specific
 * CPC/BTL module is no longer being used.
 *
 * cbm_uses_cts: a bool that indicates whether the CPC will use the
 * CTS protocol or not.
 *   - if true: the CPC will post the fragment on
 *     endpoint->endpoint_cts_frag as a receive buffer and will *not*
 *     call ompi_btl_openib_post_recvs().
 *   - if false: the CPC will call ompi_btl_openib_post_recvs() before
 *     calling ompi_btl_openib_cpc_complete().
 *
 * There are two functions in the main openib BTL that the CPC may
 * call:
 *
 * - ompi_btl_openib_post_recvs(endpoint): once a QP is locally
 * connected to the remote side (but we don't know if the remote side
 * is connected to us yet), this function is invoked to post buffers
 * on the QP, setup credits for the endpoint, etc.  This function is
 * *only* invoked if the CPC's cbm_uses_cts is false.
 *
 * - ompi_btl_openib_cpc_complete(endpoint): once that a CPC knows
 * that a QP is connected on *both* sides, this function is invoked to
 * tell the main openib BTL "ok, you can use this connection now."
 * (e.g., the main openib BTL will either invoke the CTS protocol or
 * start sending out fragments that were queued while the connection
 * was establishing, etc.).
 */
#ifndef BTL_OPENIB_CONNECT_H
#define BTL_OPENIB_CONNECT_H

BEGIN_C_DECLS

#define BCF_MAX_NAME 64

/**
 * Must forward declare these structs to avoid include file loops.
 */
struct mca_btl_openib_hca_t;
struct mca_btl_openib_module_t;
struct mca_btl_base_endpoint_t;

/**
 * This is struct is defined below
 */
struct ompi_btl_openib_connect_base_module_t;

/************************************************************************/

/**
 * Function to register MCA params in the connect functions.  It
 * returns no value, so it cannot fail.
 */
typedef void (*ompi_btl_openib_connect_base_component_register_fn_t)(void);

/**
 * This function is invoked once by the openib BTL component during
 * startup.  It is intended to have CPC component-wide startup.
 *
 * Return value:
 *
 * - OMPI_SUCCESS: this CPC component will be used in selection during
 *   this process.
 *
 * - OMPI_ERR_NOT_SUPPORTED: this CPC component will be silently
 *   ignored in this process.
 *
 * - Other OMPI_ERR_* values: the error will be propagated upwards,
 *   likely causing a fatal error (and/or the openib BTL component
 *   being ignored).
 */
typedef int (*ompi_btl_openib_connect_base_component_init_fn_t)(void);

/**
 * Query the CPC to see if it wants to run on a specific port (i.e., a
 * specific BTL module).  If the component init function previously
 * returned OMPI_SUCCESS, this function is invoked once per BTL module
 * creation (i.e., for each port found by an MPI process).  If this
 * CPC wants to be used on this BTL module, it returns a CPC module
 * that is specific to this BTL module.
 *
 * The BTL module in question is passed to the function; all of its
 * attributes can be used to query to see if it's eligible for this
 * CPC.
 *
 * If it is eligible, the CPC is responsible for creating a
 * corresponding CPC module, filling in all the relevant fields on the
 * modules, and for setting itself up to run (per above) and returning
 * a CPC module (this is effectively the "module_init" function).
 * Note that the module priority must be between 0 and 100
 * (inclusive).  When multiple CPCs are eligible for a single module,
 * the CPC with the highest priority will be used.
 *
 * Return value:
 *
 * - OMPI_SUCCESS if this CPC is eligible for and was able to be setup
 * for this BTL module.  It is assumed that the CPC is now completely
 * setup to run on this openib module (per description above).
 *
 * - OMPI_ERR_NOT_SUPPORTED if this CPC cannot support this BTL
 * module.  This is not an error; it's just the CPC saying "sorry, I
 * cannot support this BTL module."
 *
 * - Other OMPI_ERR_* code: an error occurred.
 */
typedef int (*ompi_btl_openib_connect_base_func_component_query_t)
    (struct mca_btl_openib_module_t *btl, 
     struct ompi_btl_openib_connect_base_module_t **cpc);

/**
 * This function is invoked once by the openib BTL component during
 * shutdown.  It is intended to have CPC component-wide shutdown.
 */
typedef int (*ompi_btl_openib_connect_base_component_finalize_fn_t)(void);

/**
 * CPC component struct
 */
struct ompi_btl_openib_connect_base_component_t {
    /** Name of this set of connection functions */
    char cbc_name[BCF_MAX_NAME];

    /** Register function.  Can be NULL. */
    ompi_btl_openib_connect_base_component_register_fn_t cbc_register;

    /** CPC component init function.  Can be NULL. */
    ompi_btl_openib_connect_base_component_init_fn_t cbc_init;

    /** Query the CPC component to get a CPC module corresponding to
        an openib BTL module.  Cannot be NULL. */
    ompi_btl_openib_connect_base_func_component_query_t cbc_query;

    /** CPC component finalize function.  Can be NULL. */
    ompi_btl_openib_connect_base_component_finalize_fn_t cbc_finalize;
};
/**
 * Convenience typedef
 */
typedef struct ompi_btl_openib_connect_base_component_t ompi_btl_openib_connect_base_component_t;

/************************************************************************/

/**
 * Function called when an endpoint has been created and has been
 * associated with a CPC.
 */
typedef int (*ompi_btl_openib_connect_base_module_endpoint_init_fn_t)
    (struct mca_btl_base_endpoint_t *endpoint);

/**
 * Function to initiate a connection to a remote process.
 */
typedef int (*ompi_btl_openib_connect_base_module_start_connect_fn_t)
    (struct ompi_btl_openib_connect_base_module_t *cpc,
     struct mca_btl_base_endpoint_t *endpoint);

/**
 * Function called when an endpoint is being destroyed.
 */
typedef int (*ompi_btl_openib_connect_base_module_endpoint_finalize_fn_t)
     (struct mca_btl_base_endpoint_t *endpoint);

/**
 * Function to finalize the CPC module.  It is called once when the
 * CPC module's corresponding openib BTL module is being finalized.
 */
typedef int (*ompi_btl_openib_connect_base_module_finalize_fn_t)
    (struct mca_btl_openib_module_t *btl,
     struct ompi_btl_openib_connect_base_module_t *cpc);

/**
 * Meta data about a CPC module.  This is in a standalone struct
 * because it is used in both the CPC module struct and the
 * openib_btl_proc_t struct to hold information received from the
 * modex.
 */
typedef struct ompi_btl_openib_connect_base_module_data_t {
    /** Pointer back to the component.  Used by the base and openib
        btl to calculate this module's index for the modex. */
    ompi_btl_openib_connect_base_component_t *cbm_component;

    /** Priority of the CPC module (must be >=0 and <=100) */
    uint8_t cbm_priority;

    /** Blob that the CPC wants to include in the openib modex message
        for a specific port, or NULL if the CPC does not want to
        include a message in the modex.  */
    void *cbm_modex_message;

    /** Length of the cbm_modex_message blob (0 if
        cbm_modex_message==NULL).  The message is intended to be short
        (because the size of the modex broadcast is a function of
        sum(cbm_modex_message_len[i]) for
        i=(0...total_num_ports_in_MPI_job) -- e.g., IBCM imposes its
        own [very short] limits (per IBTA volume 1, chapter 12). */
    uint8_t cbm_modex_message_len;
} ompi_btl_openib_connect_base_module_data_t;

/**
 * Struct for holding CPC module and associated meta data
 */
typedef struct ompi_btl_openib_connect_base_module_t {
    /** Meta data about the module */
    ompi_btl_openib_connect_base_module_data_t data;

    /** Endpoint initialization function */
    ompi_btl_openib_connect_base_module_endpoint_init_fn_t cbm_endpoint_init;

    /** Connect function */
    ompi_btl_openib_connect_base_module_start_connect_fn_t cbm_start_connect;

    /** Endpoint finalization function */
    ompi_btl_openib_connect_base_module_endpoint_finalize_fn_t cbm_endpoint_finalize;

    /** Finalize the cpc module */
    ompi_btl_openib_connect_base_module_finalize_fn_t cbm_finalize;

    /** Whether this module will use the CTS protocol or not.  This
        directly states whether this module will call
        mca_btl_openib_endpoint_post_recvs() or not: true = this
        module will *not* call _post_recvs() and instead will post the
        receive buffer provided at endpoint->endpoint_cts_frag on qp
        0. */
    bool cbm_uses_cts;
} ompi_btl_openib_connect_base_module_t;

END_C_DECLS

#endif
