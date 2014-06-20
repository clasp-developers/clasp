/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * Copyright (c) 2007-2009 Sun Microsystems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <errno.h> 
#include <string.h>
#include "opal/class/opal_bitmap.h"
#include "opal/util/if.h"
#include "ompi/mca/btl/btl.h"

#include "btl_udapl.h"
#include "btl_udapl_endpoint.h"
#include "btl_udapl_frag.h"
#include "btl_udapl_mca.h"
#include "btl_udapl_proc.h"
#include "opal/datatype/opal_convertor.h" 
#include "ompi/mca/mpool/base/base.h" 
#include "ompi/mca/mpool/rdma/mpool_rdma.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "ompi/proc/proc.h"

static int udapl_reg_mr(void *reg_data, void *base, size_t size,
        mca_mpool_base_registration_t *reg);
static int udapl_dereg_mr(void *reg_data, mca_mpool_base_registration_t *reg);
static int mca_btl_udapl_set_peer_parameters(
        struct mca_btl_udapl_module_t* udapl_btl,
        size_t nprocs);
static int mca_btl_udapl_assign_netmask(mca_btl_udapl_module_t* udapl_btl);

mca_btl_udapl_module_t mca_btl_udapl_module = {
    {
        &mca_btl_udapl_component.super,
        0, /* max size of first fragment */
        0, /* min send fragment size */
        0, /* max send fragment size */
        0, /* btl_rdma_pipeline_send_length */
        0, /* btl_rdma_pipeline_frag_size */
        0, /* btl_min_rdma_pipeline_size */
        0, /* exclusivity */
        0, /* latency */
        0, /* bandwidth */
        MCA_BTL_FLAGS_SEND,
        mca_btl_udapl_add_procs,
        mca_btl_udapl_del_procs,
        NULL, 
        mca_btl_udapl_finalize,
        mca_btl_udapl_alloc, 
        mca_btl_udapl_free, 
        mca_btl_udapl_prepare_src,
        mca_btl_udapl_prepare_dst,
        mca_btl_udapl_send,
        NULL, /* send immediate */
        mca_btl_udapl_put,
        NULL, /* get */ 
        mca_btl_base_dump,
        NULL, /* mpool */
        NULL, /* register error cb */
        mca_btl_udapl_ft_event
    }
};

static int udapl_reg_mr(void *reg_data, void *base, size_t size,
        mca_mpool_base_registration_t *reg)
{
    mca_btl_udapl_module_t *btl = (mca_btl_udapl_module_t*)reg_data;
    mca_btl_udapl_reg_t *udapl_reg = (mca_btl_udapl_reg_t*)reg;
    DAT_REGION_DESCRIPTION region;
    DAT_VLEN dat_size;
    DAT_VADDR dat_addr;
    int rc;
    DAT_MEM_TYPE lmr_mem_type = DAT_MEM_TYPE_VIRTUAL;

    region.for_va = base;
    udapl_reg->lmr_triplet.virtual_address = (DAT_VADDR)(uintptr_t)base;
    udapl_reg->lmr_triplet.segment_length = size;
    udapl_reg->lmr = NULL;

#if HAVE_DAT_MEM_TYPE_SO_VIRTUAL
    if (reg->flags & MCA_MPOOL_FLAGS_SO_MEM) {
        lmr_mem_type = DAT_MEM_TYPE_SO_VIRTUAL;
    }
#endif

    rc = dat_lmr_create(btl->udapl_ia, lmr_mem_type, region, size,
            btl->udapl_pz, DAT_MEM_PRIV_ALL_FLAG, &udapl_reg->lmr,
            &udapl_reg->lmr_triplet.lmr_context, &udapl_reg->rmr_context,
            &dat_size, &dat_addr);

    if(rc != DAT_SUCCESS) {
        BTL_UDAPL_VERBOSE_HELP(VERBOSE_SHOW_HELP, ("help-mpi-btl-udapl.txt",
            "dat_lmr_create DAT_INSUFFICIENT_RESOURCES", true));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    return OMPI_SUCCESS;
}

static int udapl_dereg_mr(void *reg_data, mca_mpool_base_registration_t *reg)
{
    mca_btl_udapl_reg_t *udapl_reg = (mca_btl_udapl_reg_t*)reg;
    int rc;

    if(udapl_reg->lmr != NULL) {
        rc = dat_lmr_free(udapl_reg->lmr);
        if(rc != DAT_SUCCESS) {
            char* major;
            char* minor;

            dat_strerror(rc, (const char**)&major,
                (const char**)&minor);
            BTL_ERROR(("ERROR: %s %s %s\n", "dat_lmr_free",
                major, minor));
            return OMPI_ERROR;
        }
    }

    return OMPI_SUCCESS;
}

/**
 * Initialize module module resources.
 */

int
mca_btl_udapl_init(DAT_NAME_PTR ia_name, mca_btl_udapl_module_t* btl)
{
    mca_mpool_base_resources_t res;
    DAT_CONN_QUAL port;
    DAT_RETURN rc;

    /* open the uDAPL interface */
    btl->udapl_evd_async = DAT_HANDLE_NULL;
    rc = dat_ia_open(ia_name, btl->udapl_async_evd_qlen,
            &btl->udapl_evd_async, &btl->udapl_ia);
    if(DAT_SUCCESS != rc) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major,
            (const char**)&minor);

#if defined(__SVR4) && defined(__sun)
        if (strcmp(major, "DAT_INVALID_PARAMETER") == 0 &&
            strcmp(minor, "DAT_INVALID_RO_COOKIE") == 0) {
            /* Some platforms that Solaris runs on implement the PCI 
	     * standard for relaxed ordering(RO). Using RDMA with 
	     * polling on a memory location as the uDAPL (and openib
	     * by the way) BTL does for short messages with 
	     * relaxed ordering could potentially produce silent data
	     * corruption. For this reason we need to take extra
	     * steps and this is accomplished by setting
	     * "ro_aware_system = 1" and handling as required.
             *
	     * The uDAPL standard does not provide an interface to
	     * inform users of this scenario so Sun has implemented the
	     * following: If a platform supports relaxed ordering
	     * when the interface name is passed into the
	     * dat_ia_open() call, the call will return 
	     * DAT_INVALID_PARAMETER and DAT_INVALID_RO_COOKIE.  
	     * DAT_INVALID_RO_COOKIE is not part of the uDAPL standard
	     * at this time. The only way to open this interface is
	     * to prefix the following cookie "RO_AWARE_" to the ia
	     * name that was retreived from the dat registry.
             *
             * Example: ia_name = "ib0", new expected name will be
             * "RO_AWARE_ib0".
             * 
             * Here, since our first ia open attempt failed in the
             * standard way, add the cookie and try to open again.
             */
            DAT_NAME_PTR ro_ia_name;

            /* prefix relaxed order cookie to ia_name */
            asprintf(&ro_ia_name, "RO_AWARE_%s", ia_name);
            if (NULL == ro_ia_name) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            /* because this is not standard inform user in some way */
            BTL_UDAPL_VERBOSE_HELP(VERBOSE_INFORM,
                ("help-mpi-btl-udapl.txt", "relaxed order support",
                true, ia_name, ro_ia_name));

            /* try and open again */
            btl->udapl_evd_async = DAT_HANDLE_NULL;
            rc = dat_ia_open(ro_ia_name, btl->udapl_async_evd_qlen,
                &btl->udapl_evd_async, &btl->udapl_ia);

	    dat_strerror(rc, (const char**)&major,
		(const char**)&minor);

            if (DAT_SUCCESS == rc) {
                mca_btl_udapl_component.ro_aware_system = 1;
                free(ro_ia_name);
            } else {
                BTL_UDAPL_VERBOSE_HELP(VERBOSE_SHOW_HELP,
                    ("help-mpi-btl-udapl.txt",
                        "dat_ia_open fail RO", true, ro_ia_name,
                        major, minor, ia_name));

                free(ro_ia_name);
                return OMPI_ERROR;
            }
        } else {
#endif
        BTL_UDAPL_VERBOSE_HELP(VERBOSE_SHOW_HELP, ("help-mpi-btl-udapl.txt",
            "dat_ia_open fail", true, ia_name, major, minor));

        return OMPI_ERROR;
#if defined(__SVR4) && defined(__sun)	    
        }
#endif
    }

    /* create a protection zone */
    rc = dat_pz_create(btl->udapl_ia, &btl->udapl_pz);
    if(DAT_SUCCESS != rc) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major,
            (const char**)&minor);
        BTL_ERROR(("ERROR: %s %s %s\n", "dat_pz_create",
            major, minor));
        goto failure;
    }

    /* query to get address information */
    rc = dat_ia_query(btl->udapl_ia, &btl->udapl_evd_async,
            DAT_IA_ALL, &(btl->udapl_ia_attr), 0, NULL);
    if(DAT_SUCCESS != rc) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major,
            (const char**)&minor);
        BTL_ERROR(("ERROR: %s %s %s\n", "dat_ia_query",
            major, minor));
        goto failure;
    }

    memcpy(&btl->udapl_addr.addr, (btl->udapl_ia_attr).ia_address_ptr,
        sizeof(DAT_SOCK_ADDR));

    /* determine netmask */
    mca_btl_udapl_assign_netmask(btl);

    /* check evd qlen against adapter max */
    if (btl->udapl_dto_evd_qlen > (btl->udapl_ia_attr).max_evd_qlen) {
        BTL_UDAPL_VERBOSE_HELP(VERBOSE_SHOW_HELP, ("help-mpi-btl-udapl.txt",
            "evd_qlen adapter max", 
            true,
            "btl_udapl_dto_evd_qlen",
            btl->udapl_dto_evd_qlen,
            (btl->udapl_ia_attr).max_evd_qlen));        
        btl->udapl_dto_evd_qlen = btl->udapl_ia_attr.max_evd_qlen;
    }
    if (btl->udapl_conn_evd_qlen > (btl->udapl_ia_attr).max_evd_qlen) {
        BTL_UDAPL_VERBOSE_HELP(VERBOSE_SHOW_HELP, ("help-mpi-btl-udapl.txt",
            "evd_qlen adapter max", 
            true,
            "btl_udapl_conn_evd_qlen",
            btl->udapl_conn_evd_qlen,
            (btl->udapl_ia_attr).max_evd_qlen));        
        btl->udapl_conn_evd_qlen = btl->udapl_ia_attr.max_evd_qlen;
    }

    /* set up evd's */
    rc = dat_evd_create(btl->udapl_ia,
        btl->udapl_dto_evd_qlen, DAT_HANDLE_NULL,
        DAT_EVD_DTO_FLAG | DAT_EVD_RMR_BIND_FLAG, &btl->udapl_evd_dto);
    if(DAT_SUCCESS != rc) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major,
            (const char**)&minor);
        BTL_ERROR(("ERROR: %s %s %s\n", "dat_evd_create (dto)",
            major, minor));
        goto failure;
    }

    rc = dat_evd_create(btl->udapl_ia,
            btl->udapl_conn_evd_qlen, DAT_HANDLE_NULL,
            DAT_EVD_CR_FLAG | DAT_EVD_CONNECTION_FLAG, &btl->udapl_evd_conn);
    if(DAT_SUCCESS != rc) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major,
            (const char**)&minor);
        BTL_ERROR(("ERROR: %s %s %s\n", "dat_evd_create (conn)",
            major, minor));
        goto failure;
    }

    /* create our public service point */
    rc = dat_psp_create_any(btl->udapl_ia, &port, btl->udapl_evd_conn,
        DAT_PSP_CONSUMER_FLAG, &btl->udapl_psp);
    if(DAT_SUCCESS != rc) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major,
            (const char**)&minor);
        BTL_ERROR(("ERROR: %s %s %s\n", "dat_psp_create_any",
            major, minor));
        goto failure;
    }

    /* establish endpoint parameters */
    rc = mca_btl_udapl_endpoint_get_params(btl, &(btl->udapl_ep_param));
    if(OMPI_SUCCESS != rc) { 
        /* by not erroring out here we can try to continue with
         * the default endpoint parameter values
         */
        BTL_UDAPL_VERBOSE_HELP(VERBOSE_SHOW_HELP, ("help-mpi-btl-udapl.txt",
            "use default endpoint params", 
            true));
    }

    /* Save the port with the address information */
    /* TODO - since we're doing the hack below, do we need our own port? */
    btl->udapl_addr.port = port;

    /* Using dat_ep_query to obtain the remote port would be ideal but
     * since the current udapl implementations don't seem to support
     * this we store the port in udapl_addr and explictly exchange the
     * information later.
     */
    ((struct sockaddr_in*)&btl->udapl_addr.addr)->sin_port = htons(port);

    /* initialize the memory pool */
    res.reg_data = btl;
    res.sizeof_reg = sizeof(mca_btl_udapl_reg_t);
    res.register_mem = udapl_reg_mr;
    res.deregister_mem = udapl_dereg_mr;
    btl->super.btl_mpool = mca_mpool_base_module_create(
            mca_btl_udapl_component.udapl_mpool_name, &btl->super, &res);
    if (NULL == btl->super.btl_mpool) {
        BTL_UDAPL_VERBOSE_OUTPUT(VERBOSE_INFORM,
            ("WARNING: Failed to create mpool."));
        goto failure;
    }
 
    /* initialize objects */
    OBJ_CONSTRUCT(&btl->udapl_frag_eager, ompi_free_list_t);
    OBJ_CONSTRUCT(&btl->udapl_frag_eager_recv, ompi_free_list_t);
    OBJ_CONSTRUCT(&btl->udapl_frag_max, ompi_free_list_t);
    OBJ_CONSTRUCT(&btl->udapl_frag_max_recv, ompi_free_list_t);
    OBJ_CONSTRUCT(&btl->udapl_frag_user, ompi_free_list_t);
    OBJ_CONSTRUCT(&btl->udapl_frag_control, ompi_free_list_t);
    OBJ_CONSTRUCT(&btl->udapl_lock, opal_mutex_t);
    
     /* check buffer alignment against dat library */
    if (mca_btl_udapl_component.udapl_buffer_alignment !=
        DAT_OPTIMAL_ALIGNMENT) {

        BTL_UDAPL_VERBOSE_HELP(VERBOSE_SHOW_HELP, ("help-mpi-btl-udapl.txt",
            "optimal buffer alignment mismatch", 
            true,
            DAT_OPTIMAL_ALIGNMENT,
            mca_btl_udapl_component.udapl_buffer_alignment,
            DAT_OPTIMAL_ALIGNMENT));
    }

    /* initialize free lists */
    ompi_free_list_init_ex_new(&btl->udapl_frag_eager,
        sizeof(mca_btl_udapl_frag_eager_t) +
            mca_btl_udapl_component.udapl_eager_frag_size,
        mca_btl_udapl_component.udapl_buffer_alignment,
        OBJ_CLASS(mca_btl_udapl_frag_eager_t),
        mca_btl_udapl_component.udapl_eager_frag_size,
        mca_btl_udapl_component.udapl_buffer_alignment,
        mca_btl_udapl_component.udapl_free_list_num,
        mca_btl_udapl_component.udapl_free_list_max,
        mca_btl_udapl_component.udapl_free_list_inc,
                           btl->super.btl_mpool,
                           NULL,
                           NULL);

    ompi_free_list_init_ex_new(&btl->udapl_frag_eager_recv,
        sizeof(mca_btl_udapl_frag_eager_t) +
            mca_btl_udapl_component.udapl_eager_frag_size,
        mca_btl_udapl_component.udapl_buffer_alignment,
        OBJ_CLASS(mca_btl_udapl_frag_eager_t),
        mca_btl_udapl_component.udapl_eager_frag_size,
        mca_btl_udapl_component.udapl_buffer_alignment,
        mca_btl_udapl_component.udapl_free_list_num,
        mca_btl_udapl_component.udapl_free_list_max,
        mca_btl_udapl_component.udapl_free_list_inc,
                           btl->super.btl_mpool,
                           NULL,
                           NULL);

    ompi_free_list_init_ex_new(&btl->udapl_frag_max,
        sizeof(mca_btl_udapl_frag_max_t) +
            mca_btl_udapl_component.udapl_max_frag_size,
        mca_btl_udapl_component.udapl_buffer_alignment,
        OBJ_CLASS(mca_btl_udapl_frag_max_t),
        mca_btl_udapl_component.udapl_max_frag_size,
        mca_btl_udapl_component.udapl_buffer_alignment,
        mca_btl_udapl_component.udapl_free_list_num,
        mca_btl_udapl_component.udapl_free_list_max,
        mca_btl_udapl_component.udapl_free_list_inc,
                           btl->super.btl_mpool,
                           NULL,
                           NULL);

    ompi_free_list_init_ex_new(&btl->udapl_frag_max_recv,
        sizeof(mca_btl_udapl_frag_max_t) +
            mca_btl_udapl_component.udapl_max_frag_size,
        mca_btl_udapl_component.udapl_buffer_alignment,
        OBJ_CLASS(mca_btl_udapl_frag_max_t),
        mca_btl_udapl_component.udapl_max_frag_size,
        mca_btl_udapl_component.udapl_buffer_alignment,
        mca_btl_udapl_component.udapl_free_list_num,
        mca_btl_udapl_component.udapl_free_list_max,
        mca_btl_udapl_component.udapl_free_list_inc,
                           btl->super.btl_mpool,
                           NULL,
                           NULL);

    ompi_free_list_init_ex_new(&btl->udapl_frag_user,
        sizeof(mca_btl_udapl_frag_user_t),
        mca_btl_udapl_component.udapl_buffer_alignment,
        OBJ_CLASS(mca_btl_udapl_frag_user_t),
        0,0,
        mca_btl_udapl_component.udapl_free_list_num,
        mca_btl_udapl_component.udapl_free_list_max,
        mca_btl_udapl_component.udapl_free_list_inc,
                           NULL,
                           NULL,
                           NULL);

    ompi_free_list_init_ex_new(&btl->udapl_frag_control,
        sizeof(mca_btl_udapl_frag_eager_t) +
        mca_btl_udapl_component.udapl_eager_frag_size,
        mca_btl_udapl_component.udapl_buffer_alignment,
        OBJ_CLASS(mca_btl_udapl_frag_eager_t),
        mca_btl_udapl_component.udapl_eager_frag_size,
        mca_btl_udapl_component.udapl_buffer_alignment,
        mca_btl_udapl_component.udapl_free_list_num,
        -1,
        mca_btl_udapl_component.udapl_free_list_inc,
                           btl->super.btl_mpool,
                           NULL,
                           NULL);

    /* initialize eager rdma buffer info */
    btl->udapl_eager_rdma_endpoints = OBJ_NEW(opal_pointer_array_t);
    opal_pointer_array_init(btl->udapl_eager_rdma_endpoints, 
        mca_btl_udapl_component.udapl_max_eager_rdma_peers,
        mca_btl_udapl_component.udapl_max_eager_rdma_peers, 
        0);
    btl->udapl_eager_rdma_endpoint_count = 0;
    OBJ_CONSTRUCT(&btl->udapl_eager_rdma_lock, opal_mutex_t);

    /* initialize miscellaneous variables */
    btl->udapl_async_events = 0;
    btl->udapl_connect_inprogress = 0;
    btl->udapl_num_peers = 0;

    /* TODO - Set up SRQ when it is supported */
    return OMPI_SUCCESS;

failure:
    dat_ia_close(btl->udapl_ia, DAT_CLOSE_ABRUPT_FLAG);
    return OMPI_ERROR;
}

/*
 * Cleanup/release module resources.
 */

int mca_btl_udapl_finalize(struct mca_btl_base_module_t* base_btl)
{
    mca_btl_udapl_module_t* udapl_btl = (mca_btl_udapl_module_t*) base_btl; 
    int32_t i;
    
    /*
     * Cleaning up the endpoints here because mca_btl_udapl_del_procs
     * is never called by upper layers.
     * Note: this is only looking at those endpoints which are available
     * off of the btl module rdma list. 
     */
    for (i=0; i < udapl_btl->udapl_eager_rdma_endpoint_count; i++) {
        mca_btl_udapl_endpoint_t* endpoint =
            opal_pointer_array_get_item(udapl_btl->udapl_eager_rdma_endpoints,
                i);

        OBJ_DESTRUCT(endpoint);
    }

    /* release uDAPL resources */
    dat_evd_free(udapl_btl->udapl_evd_dto);
    dat_evd_free(udapl_btl->udapl_evd_conn);
    dat_pz_free(udapl_btl->udapl_pz);
    dat_ia_close(udapl_btl->udapl_ia, DAT_CLOSE_GRACEFUL_FLAG);

    /* destroy objects */
    OBJ_DESTRUCT(&udapl_btl->udapl_lock);
    OBJ_DESTRUCT(&udapl_btl->udapl_frag_eager);
    OBJ_DESTRUCT(&udapl_btl->udapl_frag_eager_recv);
    OBJ_DESTRUCT(&udapl_btl->udapl_frag_max);
    OBJ_DESTRUCT(&udapl_btl->udapl_frag_max_recv);
    OBJ_DESTRUCT(&udapl_btl->udapl_frag_user);
    OBJ_DESTRUCT(&udapl_btl->udapl_frag_control);
    OBJ_DESTRUCT(&udapl_btl->udapl_eager_rdma_lock);
    
    /* destroy mpool */
    if (OMPI_SUCCESS !=
        mca_mpool_base_module_destroy(udapl_btl->super.btl_mpool)) {
        BTL_UDAPL_VERBOSE_OUTPUT(VERBOSE_INFORM,
            ("WARNING: Failed to release mpool"));
        return OMPI_ERROR;
    }

    free(udapl_btl);
    return OMPI_SUCCESS;
}


/*
 * Adjust parameters that are dependent on the number of peers.
 *
 * @param udapl_btl (IN)      BTL module
 * @param nprocs (IN)         number of processes handed into
 *                                mca_btl_udapl_add_procs()
 * @return                    OMPI_SUCCESS or error status on failure
 */

static int mca_btl_udapl_set_peer_parameters(
    struct mca_btl_udapl_module_t* udapl_btl,
    size_t nprocs) 
{
    int rc = OMPI_SUCCESS;
    DAT_RETURN dat_rc = DAT_SUCCESS;
    uint potential_udapl_timeout;
    int first_time_sizing = (udapl_btl->udapl_num_peers == 0 ? 1 : 0);
    DAT_EVD_PARAM evd_param;
    
    /* nprocs includes self so subtract 1 */
    udapl_btl->udapl_num_peers += nprocs - 1; 

    /* resize dto_evd_qlen if not already at its max */
    if (udapl_btl->udapl_dto_evd_qlen !=
        udapl_btl->udapl_ia_attr.max_evd_qlen) {

        int potential_dto_evd_qlen;
        int max_connection_dto_events;
        int eager_connection_dto_events;

        /* eager connection dto events already factored into
         * max_recv/request_dtos but need to calculate max connection dtos;
         * see mca_btl_udapl_get_params() for max_recv/request_dtos 
         */
        eager_connection_dto_events = udapl_btl->udapl_max_recv_dtos +
            udapl_btl->udapl_max_request_dtos;
        max_connection_dto_events = mca_btl_udapl_component.udapl_num_recvs +
            mca_btl_udapl_component.udapl_num_sends +
            (mca_btl_udapl_component.udapl_num_recvs /
                mca_btl_udapl_component.udapl_sr_win) + 1;
        potential_dto_evd_qlen = udapl_btl->udapl_num_peers *
            (eager_connection_dto_events + max_connection_dto_events);
        
        /* here we use what the library calculates as the
         * potential_dto_evd_qlen unless the user has set
         */
        if (first_time_sizing) { 
            if (udapl_btl->udapl_dto_evd_qlen < potential_dto_evd_qlen) {
                if (MCA_BTL_UDAPL_DTO_EVD_QLEN_DEFAULT !=
                    udapl_btl->udapl_dto_evd_qlen) {

                    /* user modified so warn */
                    BTL_UDAPL_VERBOSE_HELP(VERBOSE_SHOW_HELP,
                        ("help-mpi-btl-udapl.txt",
                        "evd_qlen too low", 
                        true,
                        "btl_udapl_dto_evd_qlen",
                        udapl_btl->udapl_dto_evd_qlen,
                        "btl_udapl_dto_evd_qlen",                        
                        potential_dto_evd_qlen));
                } else {
                    udapl_btl->udapl_dto_evd_qlen = potential_dto_evd_qlen;
                }
            }
        } else {
            /* since this is not the first time attempting to resize the
             * evd queue length just use the potential value; this may not
             * be the best solution
             */
            udapl_btl->udapl_dto_evd_qlen = potential_dto_evd_qlen;
        }

        udapl_btl->udapl_dto_evd_qlen = ((udapl_btl->udapl_dto_evd_qlen >
            udapl_btl->udapl_ia_attr.max_evd_qlen) ?
            udapl_btl->udapl_ia_attr.max_evd_qlen :
            udapl_btl->udapl_dto_evd_qlen);
            
        /* OFED stack does not return DAT_INVALID_STATE when
         * the new qlen is less than current value so here we find
         * current value and if greater than what we intend to set
         * it to skip the resize. 
         */
        dat_rc = dat_evd_query(udapl_btl->udapl_evd_dto,
            DAT_EVD_FIELD_EVD_QLEN, &evd_param);
        if(DAT_SUCCESS != dat_rc) {
            char* major;
            char* minor;

            dat_strerror(dat_rc, (const char**)&major,
                (const char**)&minor);
            BTL_ERROR(("ERROR: %s %s %s\n", "dat_evd_query",
                major, minor));
        }

        if (udapl_btl->udapl_dto_evd_qlen > evd_param.evd_qlen) {
            /* resize dto event dispatcher queue length */
            dat_rc = dat_evd_resize(udapl_btl->udapl_evd_dto,
                udapl_btl->udapl_dto_evd_qlen);
            if(DAT_SUCCESS != dat_rc) {
                char* major;
                char* minor;

                dat_strerror(dat_rc, (const char**)&major,
                    (const char**)&minor);
                BTL_ERROR(("ERROR: %s %s %s\n", "dat_evd_resize",
                    major, minor));
                rc = OMPI_ERR_OUT_OF_RESOURCE;
            } 
        }
    }

    /* resize connection evd qlen */
    if (udapl_btl->udapl_conn_evd_qlen !=
        udapl_btl->udapl_ia_attr.max_evd_qlen) {

        int potential_conn_evd_qlen = 2 * udapl_btl->udapl_num_peers;

        if (first_time_sizing) { 
            if (udapl_btl->udapl_conn_evd_qlen < potential_conn_evd_qlen) {
                if (MCA_BTL_UDAPL_CONN_EVD_QLEN_DEFAULT !=
                    udapl_btl->udapl_conn_evd_qlen) {

                    /* user modified so warn */
                    BTL_UDAPL_VERBOSE_HELP(VERBOSE_SHOW_HELP,
                        ("help-mpi-btl-udapl.txt",
                        "evd_qlen too low", 
                        true,
                        "btl_udapl_conn_evd_qlen",
                        udapl_btl->udapl_conn_evd_qlen,
                        "btl_udapl_conn_evd_qlen",
                        potential_conn_evd_qlen));
                } else {
                    udapl_btl->udapl_conn_evd_qlen = potential_conn_evd_qlen;
                }
            }
        } else {
            /* since this is not the first time attempting to resize the
             * evd queue length just use the potential value; this may not
             * be the best solution
             */
            udapl_btl->udapl_conn_evd_qlen = potential_conn_evd_qlen;
        }

        udapl_btl->udapl_conn_evd_qlen = ((udapl_btl->udapl_conn_evd_qlen >
            udapl_btl->udapl_ia_attr.max_evd_qlen) ?
            udapl_btl->udapl_ia_attr.max_evd_qlen :
            udapl_btl->udapl_conn_evd_qlen);
        
        /* OFED stack does not return DAT_INVALID_STATE when
         * the new qlen is less than current value so here we find
         * current value and if greater than what we intend to set
         * it to skip the resize. 
         */
        dat_rc = dat_evd_query(udapl_btl->udapl_evd_conn,
            DAT_EVD_FIELD_EVD_QLEN, &evd_param);
        if(DAT_SUCCESS != dat_rc) {
            char* major;
            char* minor;

            dat_strerror(dat_rc, (const char**)&major,
                (const char**)&minor);
            BTL_ERROR(("ERROR: %s %s %s\n", "dat_evd_query",
                major, minor));
        }

        if (udapl_btl->udapl_conn_evd_qlen > evd_param.evd_qlen) {
            /* resize conn evd queue length */
            dat_rc = dat_evd_resize(udapl_btl->udapl_evd_conn,
                udapl_btl->udapl_conn_evd_qlen);
            if(DAT_SUCCESS != dat_rc) {
                char* major;
                char* minor;

                dat_strerror(dat_rc, (const char**)&major,
                    (const char**)&minor);
                BTL_ERROR(("ERROR: %s %s %s\n", "dat_evd_resize",
                    major, minor));
                rc = OMPI_ERR_OUT_OF_RESOURCE;
            } 
        }
    }
    
    /* adjust connection timeout value, calculated in microseconds */
    potential_udapl_timeout = MCA_BTL_UDAPL_CONN_TIMEOUT_INC *
        udapl_btl->udapl_num_peers;
    
    if (mca_btl_udapl_component.udapl_timeout <
        potential_udapl_timeout) {

        if (MCA_BTL_UDAPL_CONN_TIMEOUT_DEFAULT !=
            mca_btl_udapl_component.udapl_timeout) {

            /* user modified so warn */
            BTL_UDAPL_VERBOSE_HELP(VERBOSE_SHOW_HELP,
                ("help-mpi-btl-udapl.txt",
                "connection timeout low", 
                true,
                "btl_udapl_timeout",
                mca_btl_udapl_component.udapl_timeout,
                "btl_udapl_timeout",                
                potential_udapl_timeout));         
        } else {
            mca_btl_udapl_component.udapl_timeout =
                potential_udapl_timeout;
        }
    }
    mca_btl_udapl_component.udapl_timeout =
        ((mca_btl_udapl_component.udapl_timeout >
            MCA_BTL_UDAPL_CONN_TIMEOUT_MAX) ?
            MCA_BTL_UDAPL_CONN_TIMEOUT_MAX :
            mca_btl_udapl_component.udapl_timeout);

    return rc;
}

/*
 * Find and assign system netmask for the address of the uDAPL BTL
 * module, but only if udapl_if_mask has not been set by the "--mca
 * btl_udapl_if_mask" parameter. This routine will either find
 * the system netmask or set the value to 0.
 *
 * @param udapl_btl (IN)      BTL module
 *
 * @return                    OMPI_SUCCESS or OMPI_ERROR
 */
static int mca_btl_udapl_assign_netmask(mca_btl_udapl_module_t* udapl_btl)
{
    struct sockaddr *saddr;
    struct sockaddr_in *btl_addr;
    char btl_addr_string[INET_ADDRSTRLEN]; 
    char btl_ifname[INET_ADDRSTRLEN];

    /* Setting if_mask to 0 informs future steps to assume all
     * addresses are reachable.
     */
    udapl_btl->udapl_if_mask = 0; 

    if (mca_btl_udapl_component.udapl_compare_subnet) {
        /* go get system netmask value */

        /* use generic address to find address family */
        saddr = (struct sockaddr *)&(udapl_btl->udapl_addr.addr);

        if (saddr->sa_family == AF_INET) {

            btl_addr = (struct sockaddr_in *)saddr;

            /*
             * Retrieve the netmask of the udapl btl address. To
             * accomplish this requires 4 steps and the use of an opal
             * utility. This same utility is used by the tcp oob.
             * Steps:
             *     1. Get string value of known udapl btl module address.
             *     2. Use string value to find the interface name of address.
             *     3. Use interface name to find its index.
             *     4. From the index get the netmask.
             */

            /* retrieve string value of udapl btl address */
            inet_ntop(AF_INET, (void *) &btl_addr->sin_addr,
                btl_addr_string, INET_ADDRSTRLEN);

            /* use address string to retrieve associated interface name */
            if (OPAL_SUCCESS != 
                opal_ifaddrtoname(btl_addr_string,
                    btl_ifname, INET_ADDRSTRLEN)) {

                BTL_UDAPL_VERBOSE_HELP(VERBOSE_SHOW_HELP,
                    ("help-mpi-btl-udapl.txt", "interface not found",
                        true, orte_process_info.nodename, btl_addr_string));

                return OMPI_ERROR;
            }

            /* use interface name to retrieve index;  then
             * use index to retrieve udapl btl address netmask
             */
            if (OPAL_SUCCESS != 
                opal_ifindextomask(opal_ifnametoindex(btl_ifname),
                    &(udapl_btl->udapl_if_mask), sizeof(udapl_btl->udapl_if_mask))) {

                BTL_UDAPL_VERBOSE_HELP(VERBOSE_SHOW_HELP,
                    ("help-mpi-btl-udapl.txt", "netmask not found",
                        true, orte_process_info.nodename, btl_addr_string));

                return OMPI_ERROR;
            }

            /* report if_mask used by address */
            BTL_UDAPL_VERBOSE_OUTPUT(VERBOSE_INFORM,
                ("uDAPL BTL address %s : if_mask = %d",
                btl_addr_string, udapl_btl->udapl_if_mask));

        } else {
            /* current uDAPL BTL does not support IPv6 */
            BTL_UDAPL_VERBOSE_HELP(VERBOSE_SHOW_HELP,
                ("help-mpi-btl-udapl.txt", "IPv4 only",
                    true, orte_process_info.nodename));

            return OMPI_ERROR;
        }
    }

    return OMPI_SUCCESS;
}

/*
 *
 */

int mca_btl_udapl_add_procs(
    struct mca_btl_base_module_t* btl, 
    size_t nprocs, 
    struct ompi_proc_t **ompi_procs, 
    struct mca_btl_base_endpoint_t** peers, 
    opal_bitmap_t* reachable)
{
    mca_btl_udapl_module_t* udapl_btl = (mca_btl_udapl_module_t*)btl;
    int i, rc;

    for(i = 0; i < (int) nprocs; i++) {

        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        mca_btl_udapl_proc_t* udapl_proc;
        mca_btl_base_endpoint_t* udapl_endpoint;

        if(ompi_proc == ompi_proc_local()) 
            continue;

        if(NULL == (udapl_proc = mca_btl_udapl_proc_create(ompi_proc))) {
            continue;
        }

        OPAL_THREAD_LOCK(&udapl_proc->proc_lock);

        /* The btl_proc datastructure is shared by all uDAPL BTL
         * instances that are trying to reach this destination. 
         * Cache the peer instance on the btl_proc.
         */
        udapl_endpoint = OBJ_NEW(mca_btl_udapl_endpoint_t);
        if(NULL == udapl_endpoint) {
            OPAL_THREAD_UNLOCK(&udapl_proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        udapl_endpoint->endpoint_btl = udapl_btl;
        rc = mca_btl_udapl_proc_insert(udapl_proc, udapl_endpoint);
        if(rc != OMPI_SUCCESS) {
            OBJ_RELEASE(udapl_endpoint);
            OPAL_THREAD_UNLOCK(&udapl_proc->proc_lock);
            continue;
        }

        opal_bitmap_set_bit(reachable, i);
        OPAL_THREAD_UNLOCK(&udapl_proc->proc_lock);
        peers[i] = udapl_endpoint;
    }

    /* resize based on number of processes */
    if (OMPI_SUCCESS !=
        mca_btl_udapl_set_peer_parameters(udapl_btl, nprocs)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    return OMPI_SUCCESS;
}


int mca_btl_udapl_del_procs(struct mca_btl_base_module_t* btl, 
        size_t nprocs, 
        struct ompi_proc_t **procs, 
        struct mca_btl_base_endpoint_t ** peers)
{
    /* TODO */
    return OMPI_SUCCESS;
}


/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */

mca_btl_base_descriptor_t* mca_btl_udapl_alloc(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    uint8_t order,
    size_t size,
    uint32_t flags)
{
    mca_btl_udapl_module_t* udapl_btl = (mca_btl_udapl_module_t*) btl; 
    mca_btl_udapl_frag_t* frag;
    int rc;
    int pad = 0;
    
    /* compute pad as needed */
    MCA_BTL_UDAPL_FRAG_CALC_ALIGNMENT_PAD(pad,
        (size + sizeof(mca_btl_udapl_footer_t)));

    if((size + pad) <= btl->btl_eager_limit) { 
        MCA_BTL_UDAPL_FRAG_ALLOC_EAGER(udapl_btl, frag, rc); 
    } else if(size <= btl->btl_max_send_size) {
        MCA_BTL_UDAPL_FRAG_ALLOC_MAX(udapl_btl, frag, rc); 
    } else {
        return NULL;
    }

    if (NULL == frag) {
        return NULL;
    }

    frag->segment.seg_len = size;

    /* Set up the LMR triplet from the frag segment.
     * Note: The triplet.segment_len is set to what is required for
     * actually sending the fragment, if later it is determined
     * that rdma can be used to transfer the fragment the
     * triplet.segment_len will have to change.
     */
    frag->triplet.virtual_address =
        (DAT_VADDR)(uintptr_t)frag->segment.seg_addr.pval;
    frag->triplet.segment_length =
        frag->segment.seg_len + sizeof(mca_btl_udapl_footer_t);
    assert(frag->triplet.lmr_context ==
        frag->registration->lmr_triplet.lmr_context);
    
    frag->btl = udapl_btl;
    frag->base.des_src = &frag->segment;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = flags;
    frag->base.order = MCA_BTL_NO_ORDER;
    return &frag->base;
}


/**
 * Return a segment
 */

int mca_btl_udapl_free(
    struct mca_btl_base_module_t* btl, 
    mca_btl_base_descriptor_t* des) 
{
    mca_btl_udapl_frag_t* frag = (mca_btl_udapl_frag_t*)des;

    if(0 == frag->size) { 
        if (NULL != frag->registration) {
            btl->btl_mpool->mpool_deregister(btl->btl_mpool,
                &(frag->registration->base));
            frag->registration = NULL;
        }
        MCA_BTL_UDAPL_FRAG_RETURN_USER(btl, frag);
    } else if(frag->size == mca_btl_udapl_component.udapl_eager_frag_size) {
        MCA_BTL_UDAPL_FRAG_RETURN_EAGER(btl, frag); 
    } else if(frag->size == mca_btl_udapl_component.udapl_max_frag_size) {
        MCA_BTL_UDAPL_FRAG_RETURN_MAX(btl, frag); 
    } else {
        BTL_UDAPL_VERBOSE_OUTPUT(VERBOSE_DIAGNOSE,
            ("mca_btl_udapl_free: invalid descriptor\n"));
        return OMPI_ERR_BAD_PARAM;
    }
    return OMPI_SUCCESS; 
}

/**
 * Pack data and return a descriptor that can be
 * used for send/put.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
mca_btl_base_descriptor_t* mca_btl_udapl_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct opal_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags
)
{
    mca_btl_udapl_frag_t* frag = NULL;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int rc;
    int pad = 0;

    /* compute pad as needed */
    MCA_BTL_UDAPL_FRAG_CALC_ALIGNMENT_PAD(pad,
        (max_data + reserve + sizeof(mca_btl_udapl_footer_t)));

    if(opal_convertor_need_buffers(convertor) == false && 0 == reserve) {
        if(registration != NULL || max_data > btl->btl_max_send_size) {

            MCA_BTL_UDAPL_FRAG_ALLOC_USER(btl, frag, rc);
            if(NULL == frag){
                return NULL;
            }

            iov.iov_len = max_data;
            iov.iov_base = NULL;

            opal_convertor_pack(convertor, &iov,
                &iov_count, &max_data );

            *size = max_data;
        
            if(NULL == registration) {
                rc = btl->btl_mpool->mpool_register(btl->btl_mpool, iov.iov_base,
                    max_data, 0,
                    &registration);

                if(rc != OMPI_SUCCESS) {
                    MCA_BTL_UDAPL_FRAG_RETURN_USER(btl,frag);
                    return NULL;
                }
                /* keep track of the registration we did */
                frag->registration = (mca_btl_udapl_reg_t*)registration;
            }

            frag->segment.seg_len = max_data;
            frag->segment.seg_addr.pval = iov.iov_base;
            frag->triplet.segment_length = max_data;
            frag->triplet.virtual_address = (DAT_VADDR)(uintptr_t)iov.iov_base;
            frag->triplet.lmr_context =
                ((mca_btl_udapl_reg_t*)registration)->lmr_triplet.lmr_context;

            /* initialize base descriptor */
            frag->base.des_src = &frag->segment;
            frag->base.des_src_cnt = 1;
            frag->base.des_dst = NULL;
            frag->base.des_dst_cnt = 0;
            frag->base.des_flags = flags;
            frag->base.order = MCA_BTL_NO_ORDER;
            return &frag->base;
        }
    }

    if(max_data + pad + reserve <= btl->btl_eager_limit) {
        /* the data is small enough to fit in the eager frag and
         * memory is not prepinned */
        MCA_BTL_UDAPL_FRAG_ALLOC_EAGER(btl, frag, rc);
    }

    if(NULL == frag) {
        /* the data doesn't fit into eager frag or eager frag is
         * not available */
        MCA_BTL_UDAPL_FRAG_ALLOC_MAX(btl, frag, rc);
        if(NULL == frag) {
            return NULL;
        }
        if(max_data + reserve > btl->btl_max_send_size) {
            max_data = btl->btl_max_send_size - reserve;
        }
    }
    
    iov.iov_len = max_data;
    iov.iov_base = (char *) frag->segment.seg_addr.pval + reserve;
    
    rc = opal_convertor_pack(convertor,
        &iov, &iov_count, &max_data );
    if(rc < 0) {
        MCA_BTL_UDAPL_FRAG_RETURN_MAX(btl, frag);
        return NULL;
    }

    *size = max_data;

    /* setup lengths and addresses to send out data */
    frag->segment.seg_len = max_data + reserve;
    frag->triplet.segment_length =
        max_data + reserve + sizeof(mca_btl_udapl_footer_t);
    frag->triplet.virtual_address =
        (DAT_VADDR)(uintptr_t)frag->segment.seg_addr.pval;

    /* initialize base descriptor */
    frag->base.des_src = &frag->segment;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = flags;
    frag->base.order = MCA_BTL_NO_ORDER;
    return &frag->base;
}


/**
 * Prepare a descriptor for send/rdma using the supplied
 * convertor. If the convertor references data that is contiguous,
 * the descriptor may simply point to the user buffer. Otherwise,
 * this routine is responsible for allocating buffer space and
 * packing if required.
 *
 * @param btl (IN)          BTL module
 * @param endpoint (IN)     BTL peer addressing
 * @param convertor (IN)    Data type convertor
 * @param reserve (IN)      Additional bytes requested by upper layer to precede user data
 * @param size (IN/OUT)     Number of bytes to prepare (IN), number of bytes actually prepared (OUT)
 */
mca_btl_base_descriptor_t* mca_btl_udapl_prepare_dst(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct opal_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags)
{
    mca_btl_udapl_frag_t* frag;
    int rc;

    MCA_BTL_UDAPL_FRAG_ALLOC_USER(btl, frag, rc);
    if(NULL == frag) {
        return NULL;
    }

    frag->segment.seg_len = *size;
    opal_convertor_get_current_pointer( convertor, (void**)&(frag->segment.seg_addr.pval) );

    if(NULL == registration) {
        /* didn't get a memory registration passed in, so must
         * register the region now
         */ 
        rc = btl->btl_mpool->mpool_register(btl->btl_mpool,
                                   frag->segment.seg_addr.pval,
                                   frag->segment.seg_len,
                                   0,
                                   &registration);
        if(OMPI_SUCCESS != rc || NULL == registration) {
            MCA_BTL_UDAPL_FRAG_RETURN_USER(btl,frag);
            return NULL;
        }
        frag->registration = (mca_btl_udapl_reg_t*)registration;        
    }

    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst = &frag->segment;
    frag->base.des_dst_cnt = 1;
    frag->base.des_flags = flags;

    frag->segment.seg_key.key32[0] =
        ((mca_btl_udapl_reg_t*)registration)->rmr_context;
    
    frag->base.order = MCA_BTL_NO_ORDER;

    return &frag->base;
}

/**
 * Initiate an asynchronous send.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 * @param tag (IN)         The tag value used to notify the peer.
 */

int mca_btl_udapl_send( 
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* des, 
    mca_btl_base_tag_t tag)
   
{
    mca_btl_udapl_frag_t* frag = (mca_btl_udapl_frag_t*)des;

    frag->endpoint = endpoint;
    frag->ftr = (mca_btl_udapl_footer_t *)
        ((char *)frag->segment.seg_addr.pval + frag->segment.seg_len);
    frag->ftr->tag = tag;
    frag->type = MCA_BTL_UDAPL_SEND;

    /* TODO - will inlining this give worthwhile performance? */
    return mca_btl_udapl_endpoint_send(endpoint, frag);
}



/**
 * Initiate an asynchronous put.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */

int mca_btl_udapl_put( 
    mca_btl_base_module_t* btl,
    mca_btl_base_endpoint_t* endpoint,
    mca_btl_base_descriptor_t* des)
{
    DAT_RMR_TRIPLET remote_buffer;
    DAT_DTO_COOKIE cookie;
    int rc = OMPI_SUCCESS;
    
    mca_btl_udapl_frag_t* frag = (mca_btl_udapl_frag_t*)des;
    mca_btl_base_segment_t *dst_segment = des->des_dst;

    frag->btl = (mca_btl_udapl_module_t *)btl;
    frag->endpoint = endpoint;
    frag->type = MCA_BTL_UDAPL_PUT;

    if (OPAL_THREAD_ADD32(&endpoint->endpoint_lwqe_tokens[BTL_UDAPL_MAX_CONNECTION], -1) < 0) {
        /* no local work queue tokens available */
        OPAL_THREAD_ADD32(&endpoint->endpoint_lwqe_tokens[BTL_UDAPL_MAX_CONNECTION], 1);
        OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
        opal_list_append(&endpoint->endpoint_max_frags,
            (opal_list_item_t*)frag);
        OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
        opal_progress();
    } else {
        /* work queue tokens available, try to send  */

	if(OPAL_THREAD_ADD32(&endpoint->endpoint_sr_tokens[BTL_UDAPL_MAX_CONNECTION], -1) < 0) {
            OPAL_THREAD_ADD32(&endpoint->endpoint_lwqe_tokens[BTL_UDAPL_MAX_CONNECTION], 1);
	    OPAL_THREAD_ADD32(&endpoint->endpoint_sr_tokens[BTL_UDAPL_MAX_CONNECTION], 1);
	    OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
	    opal_list_append(&endpoint->endpoint_max_frags,
		(opal_list_item_t*)frag);
	    OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
	    opal_progress();
	} else {
	    frag->triplet.segment_length = frag->segment.seg_len;
        
	    remote_buffer.rmr_context =
		(DAT_RMR_CONTEXT)dst_segment->seg_key.key32[0];
	    remote_buffer.target_address =
		(DAT_VADDR)(uintptr_t)dst_segment->seg_addr.lval;
	    remote_buffer.segment_length = dst_segment->seg_len;

	    cookie.as_ptr = frag;
	    OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
	    rc = dat_ep_post_rdma_write(endpoint->endpoint_max,
		1,
		&frag->triplet,
		cookie,
		&remote_buffer,	
		DAT_COMPLETION_DEFAULT_FLAG);
	    OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
	    if(DAT_SUCCESS != rc) {
		char* major;
		char* minor;

		dat_strerror(rc, (const char**)&major,
		    (const char**)&minor);
		BTL_ERROR(("ERROR: %s %s %s\n", "dat_ep_post_rdma_write",
		    major, minor));
		rc = OMPI_ERROR;
	    }
	}
    }
    
    return rc;
}



/**
 * Initiate an asynchronous get.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 *
 */

int mca_btl_udapl_get( 
    mca_btl_base_module_t* btl,
    mca_btl_base_endpoint_t* endpoint,
    mca_btl_base_descriptor_t* des)
{
    BTL_UDAPL_VERBOSE_OUTPUT(VERBOSE_DEVELOPER, ("udapl_get\n"));
    return OMPI_ERR_NOT_IMPLEMENTED;
}

int mca_btl_udapl_ft_event(int state) {
    if(OPAL_CRS_CHECKPOINT == state) {
        ;
    }
    else if(OPAL_CRS_CONTINUE == state) {
        ;
    }
    else if(OPAL_CRS_RESTART == state) {
        ;
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }
    
    return OMPI_SUCCESS;
}
