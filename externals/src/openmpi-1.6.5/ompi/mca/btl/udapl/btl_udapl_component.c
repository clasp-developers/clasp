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
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include "ompi/constants.h"
#include "opal/event/event.h"
#include "opal/util/argv.h"
#include "ompi/mca/btl/btl.h"

#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/mpool/base/base.h" 
#include "ompi/mca/mpool/rdma/mpool_rdma.h"
#include "btl_udapl.h"
#include "btl_udapl_frag.h"
#include "btl_udapl_endpoint.h" 
#include "btl_udapl_mca.h"
#include "btl_udapl_proc.h" 
#include "ompi/mca/btl/base/base.h" 
#include "ompi/mca/btl/base/btl_base_error.h"
#include "btl_udapl_endpoint.h"
#include "orte/util/proc_info.h"
#include "ompi/runtime/ompi_module_exchange.h"
#include "ompi/runtime/mpiruntime.h"

/*
 * Local Functions
 */
static inline int mca_btl_udapl_frag_progress_one(mca_btl_udapl_module_t* udapl_btl,
                                                  mca_btl_udapl_frag_t* frag);
void mca_btl_udapl_frag_progress_pending(mca_btl_udapl_module_t* udapl_btl,
                                        mca_btl_base_endpoint_t* endpoint,
                                        const int connection);
static int mca_btl_udapl_modify_ia_list(DAT_COUNT *num_info_entries,
                                        DAT_PROVIDER_INFO* datinfo);
static const char*
mca_btl_udapl_dat_event_to_string(DAT_EVENT_NUMBER event_number);


mca_btl_udapl_component_t mca_btl_udapl_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */
        {
            MCA_BTL_BASE_VERSION_2_0_0,

            "udapl", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_btl_udapl_component_open,  /* component open */
            mca_btl_udapl_component_close  /* component close */
        },
        {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

        mca_btl_udapl_component_init,  
        mca_btl_udapl_component_progress,
    }
};


/*
 * Predefined and fixed size structure containing DAT_EVENT values
 * and associated string as defined in: "uDAPL:User Direct Access
 * Programming Library v1.2 Sept 15, 2004", DAT Collaborative Organization.
 */
static struct mca_btl_udapl_dat_events {
        DAT_EVENT_NUMBER value;
        const char* name;
} mca_btl_udapl_dat_events[] = {
        { DAT_DTO_COMPLETION_EVENT,
            "DAT_DTO_COMPLETION_EVENT" },
        { DAT_RMR_BIND_COMPLETION_EVENT,
            "DAT_RMR_BIND_COMPLETION_EVENT" },
        { DAT_CONNECTION_REQUEST_EVENT,
            "DAT_CONNECTION_REQUEST_EVENT" },
        { DAT_CONNECTION_EVENT_ESTABLISHED,
            "DAT_CONNECTION_EVENT_ESTABLISHED" },
        { DAT_CONNECTION_EVENT_PEER_REJECTED,
            "DAT_CONNECTION_EVENT_PEER_REJECTED" },
        { DAT_CONNECTION_EVENT_NON_PEER_REJECTED,
            "DAT_CONNECTION_EVENT_NON_PEER_REJECTED" },
        { DAT_CONNECTION_EVENT_ACCEPT_COMPLETION_ERROR,
            "DAT_CONNECTION_EVENT_ACCEPT_COMPLETION_ERROR" },
        { DAT_CONNECTION_EVENT_DISCONNECTED,
            "DAT_CONNECTION_EVENT_DISCONNECTED" },
        { DAT_CONNECTION_EVENT_BROKEN,
            "DAT_CONNECTION_EVENT_BROKEN" },
        { DAT_CONNECTION_EVENT_TIMED_OUT,
            "DAT_CONNECTION_EVENT_TIMED_OUT" },
        { DAT_CONNECTION_EVENT_UNREACHABLE,
            "DAT_CONNECTION_EVENT_UNREACHABLE" },
        { DAT_ASYNC_ERROR_EVD_OVERFLOW,
            "DAT_ASYNC_ERROR_EVD_OVERFLOW" },
        { DAT_ASYNC_ERROR_IA_CATASTROPHIC,
            "DAT_ASYNC_ERROR_IA_CATASTROPHIC" },
        { DAT_ASYNC_ERROR_EP_BROKEN,
            "DAT_ASYNC_ERROR_EP_BROKEN" },
        { DAT_ASYNC_ERROR_TIMED_OUT,
            "DAT_ASYNC_ERROR_TIMED_OUT" },
        { DAT_ASYNC_ERROR_PROVIDER_INTERNAL_ERROR,
            "DAT_ASYNC_ERROR_PROVIDER_INTERNAL_ERROR" },
        { DAT_SOFTWARE_EVENT,
            "DAT_SOFTWARE_EVENT" }
};


/*
 * Function to convert DAT_EVENT_NUMBER into a readable string.
 *
 * @param event_number (IN)   DAT_EVENT_NUMBER value 
 *
 * @return                    event string or a string indicating
 *                            event number is invalid
 */
static const char *
mca_btl_udapl_dat_event_to_string(DAT_EVENT_NUMBER event_number)
{
    int i;
    int num_events = (sizeof(mca_btl_udapl_dat_events) /
       sizeof(mca_btl_udapl_dat_events[0]));

   for (i = 0; i < num_events; i++) {
       if (mca_btl_udapl_dat_events[i].value == event_number) {
           return (mca_btl_udapl_dat_events[i].name);
       }
   }
   
   return ("Unknown DAT Event Number");
}


/**
  * Report a uDAPL error - for debugging
  */

#if OPAL_ENABLE_DEBUG
void
mca_btl_udapl_error(DAT_RETURN ret, char* str)
{
    char* major;
    char* minor;

    if(DAT_SUCCESS != dat_strerror(ret,
            (const char**)&major, (const char**)&minor))
    {
        printf("dat_strerror failed! ret is %d\n", ret);
        exit(-1);
    }

    OPAL_OUTPUT((0, "ERROR: %s %s %s\n", str, major, minor));
}
#endif


/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

int mca_btl_udapl_component_open(void)
{
    int rc = OMPI_SUCCESS;

    /* initialize state */
    mca_btl_udapl_component.udapl_num_btls=0;
    mca_btl_udapl_component.udapl_btls=NULL;
    mca_btl_udapl_component.ro_aware_system=0;
    
    /* initialize objects */ 
    OBJ_CONSTRUCT(&mca_btl_udapl_component.udapl_procs, opal_list_t);
    OBJ_CONSTRUCT(&mca_btl_udapl_component.udapl_lock, opal_mutex_t);

    /* register uDAPL MCA parameters */
    rc = mca_btl_udapl_register_mca_params();

    /* compute udapl_eager_frag_size and udapl_max_frag_size */
    mca_btl_udapl_component.udapl_eager_frag_size =
        mca_btl_udapl_module.super.btl_eager_limit;
    mca_btl_udapl_module.super.btl_eager_limit -=
        (sizeof(mca_btl_udapl_footer_t) + sizeof(mca_btl_udapl_rdma_footer_t));
    
    mca_btl_udapl_component.udapl_max_frag_size =
        mca_btl_udapl_module.super.btl_max_send_size;
    mca_btl_udapl_module.super.btl_max_send_size -=
        (sizeof(mca_btl_udapl_footer_t) + sizeof(mca_btl_udapl_rdma_footer_t));

    /* compute udapl_eager_rdma_frag_size */
    mca_btl_udapl_component.udapl_eager_rdma_frag_size =
        sizeof(mca_btl_udapl_frag_eager_rdma_t) +
        mca_btl_udapl_component.udapl_eager_frag_size;

    return rc;
}


/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_btl_udapl_component_close(void)
{
    /* TODO - what needs to be done here? */
    return OMPI_SUCCESS;
}


/*
 *  Register uDAPL component addressing information. The MCA framework
 *  will make this available to all peers.
 */

static int
mca_btl_udapl_modex_send(void)
{
    int         rc;
    size_t      i;
    size_t      size;
    mca_btl_udapl_addr_t *addrs = NULL;

    size = sizeof(mca_btl_udapl_addr_t) *
            mca_btl_udapl_component.udapl_num_btls;

    if (0 != size) {
        addrs = (mca_btl_udapl_addr_t*)malloc(size);
        if (NULL == addrs) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        memset(addrs, 0, size);

        for (i = 0; i < mca_btl_udapl_component.udapl_num_btls; i++) {
            mca_btl_udapl_module_t* btl = mca_btl_udapl_component.udapl_btls[i];
            addrs[i] = btl->udapl_addr;
        }
    }

    rc = ompi_modex_send(
            &mca_btl_udapl_component.super.btl_version, addrs, size);
    if (NULL != addrs) {
        free (addrs);
    }
    return rc;
}


/*
 * Callback function used for udapl btl internal control messages. 
 *
 * @param btl (IN)         BTL module
 * @param tag (IN)         Not used but part of callback interface
 * @param descriptor (IN)  Description of the data that was just transferred
 * @param cbdata (IN)             Data used by call back function. Not used.
 *
 */
static void mca_btl_udapl_receive_control(struct mca_btl_base_module_t* btl,
                               mca_btl_base_tag_t tag,
                               mca_btl_base_descriptor_t* descriptor,
                               void* cbdata)
{
    mca_btl_udapl_frag_t* frag = (mca_btl_udapl_frag_t*)descriptor;
    mca_btl_udapl_endpoint_t* endpoint = frag->endpoint;
    mca_btl_udapl_control_header_t* ctl_hdr =
        frag->segment.seg_addr.pval;
    
    switch (ctl_hdr->type) {
    case MCA_BTL_UDAPL_CONTROL_RDMA_CONNECT:
    {        
        mca_btl_udapl_eager_rdma_connect_t* rdma_connect =
            frag->segment.seg_addr.pval;

        if (endpoint->endpoint_eager_rdma_remote.base.pval) {
            BTL_ERROR(("ERROR: Received RDMA connect twice!"));
            return;
        }
        endpoint->endpoint_eager_rdma_remote.rkey =  rdma_connect->rkey;
        endpoint->endpoint_eager_rdma_remote.base.pval =
            rdma_connect->rdma_start.pval;

        OPAL_THREAD_ADD32(&(endpoint->endpoint_eager_rdma_remote.tokens),
            mca_btl_udapl_component.udapl_eager_rdma_num);

        break;
    }
    case MCA_BTL_UDAPL_CONTROL_RDMA_CREDIT:
    {
        mca_btl_udapl_eager_rdma_credit_t* rdma_credit =
            frag->segment.seg_addr.pval;
        
        /* don't return credits used for rdma credit control message */
        OPAL_THREAD_ADD32(
            &(endpoint->endpoint_sr_credits[BTL_UDAPL_EAGER_CONNECTION]),
            -1);

        OPAL_THREAD_ADD32(&(endpoint->endpoint_eager_rdma_remote.tokens),
            rdma_credit->credits);
        
        break;
    }
    case MCA_BTL_UDAPL_CONTROL_SR_CREDIT:
    {
        mca_btl_udapl_sr_credit_t* sr_credit =
            frag->segment.seg_addr.pval;
        
        /* don't return credits used for sr credit control message */
        OPAL_THREAD_ADD32(
            &(endpoint->endpoint_sr_credits[sr_credit->connection]), -1);

        OPAL_THREAD_ADD32(
            &(endpoint->endpoint_sr_tokens[sr_credit->connection]),
            sr_credit->credits);

        break;
    }
    default:
        BTL_ERROR(("ERROR: Unknown contrl message type received by BTL"));
        break;
    }
}


/*
 * Modify the list of dat entry pointers to include only those entries
 * which it is desired to attempt dat_ia_open on.
 * 
 * @param num_info_entries (IN/OUT) Number of entries in datinfo list
 * @param datinfo (IN/OUT)          List of pointers to dat registry entries
 */

static int mca_btl_udapl_modify_ia_list(DAT_COUNT *num_info_entries,
                               DAT_PROVIDER_INFO* datinfo)
{
    int i,j,k,found;
    DAT_PROVIDER_INFO* tmp_datinfo = NULL;
    DAT_COUNT tmp_num_entries = 0;

    
    tmp_datinfo = malloc((*num_info_entries) * sizeof(DAT_PROVIDER_INFO));
    if(NULL == tmp_datinfo) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (i = 0; i < *num_info_entries; i++) {
        j = 0;
        found = 0;

        /* search for datinfo entry on the if_list list */
        while (mca_btl_udapl_component.if_list[j]) {
            if (0 == strcmp(datinfo[i].ia_name, 
                mca_btl_udapl_component.if_list[j])) {

                found = 1;
                /* remove from if_list */
                k = opal_argv_count(mca_btl_udapl_component.if_list);
                opal_argv_delete(&k, &(mca_btl_udapl_component.if_list),
                    j, 1);

                break;
            }
            j++;
        }

        if (found) {
            if (NULL != mca_btl_udapl_component.if_include_list) {
                /* explicitly include */
                tmp_datinfo[tmp_num_entries] = datinfo[i];
                tmp_num_entries++;
            }

            /* if this is if_exclude case and match found do nothing */
            
        } else {
            /* if this is if_include case and match not found do nothing */

            if (NULL != mca_btl_udapl_component.if_exclude_list) {
                /* not found for exclude case so actually include here */
                tmp_datinfo[tmp_num_entries] = datinfo[i];
                tmp_num_entries++;
            }
        }
    }
    
    /* set new values */
    *num_info_entries = tmp_num_entries;
    for (j = 0; j < *num_info_entries; j++) {
        datinfo[j] = tmp_datinfo[j];
    }


    /* if if_list not NULL, either not found or user error */
    if (opal_argv_count(mca_btl_udapl_component.if_list)) {
        char *str = opal_argv_join(mca_btl_udapl_component.if_list, ',');
        BTL_UDAPL_VERBOSE_HELP(VERBOSE_SHOW_HELP,
            ("help-mpi-btl-udapl.txt", "nonexistent entry",
            true, orte_process_info.nodename,
            ((NULL != mca_btl_udapl_component.if_include) ? 
            "in" : "ex"), str));
        free(str);
    }    

    free(tmp_datinfo);
    return OMPI_SUCCESS;
}


/*
 * Initialize the uDAPL component,
 * check how many interfaces are available and create a btl module for each.
 */

mca_btl_base_module_t **
mca_btl_udapl_component_init (int *num_btl_modules,
                           bool enable_progress_threads,
                           bool enable_mpi_threads)
{
    DAT_PROVIDER_INFO* datinfo;
    DAT_PROVIDER_INFO** datinfoptr;
    mca_btl_base_module_t **btls;
    mca_btl_udapl_module_t *btl;
    DAT_COUNT num_ias;
    int32_t i;

    /* Currently refuse to run if MPI_THREAD_MULTIPLE is enabled */
    if (ompi_mpi_thread_multiple && !mca_btl_base_thread_multiple_override) {
        mca_btl_udapl_component.udapl_num_btls = 0;
        mca_btl_udapl_modex_send();
        return NULL;
    } 

    /* parse the include and exclude lists, checking for errors */
    mca_btl_udapl_component.if_include_list =
        mca_btl_udapl_component.if_exclude_list = 
        mca_btl_udapl_component.if_list = NULL;
    if (NULL != mca_btl_udapl_component.if_include &&
        NULL != mca_btl_udapl_component.if_exclude) {
        BTL_UDAPL_VERBOSE_HELP(VERBOSE_SHOW_HELP, ("help-mpi-btl-udapl.txt",
            "specified include and exclude", true,
            mca_btl_udapl_component.if_include,
            mca_btl_udapl_component.if_exclude));
        mca_btl_udapl_component.udapl_num_btls = 0;
        mca_btl_udapl_modex_send();
        return NULL;
    } else if (NULL != mca_btl_udapl_component.if_include) {
        mca_btl_udapl_component.if_include_list = 
            opal_argv_split(mca_btl_udapl_component.if_include, ',');
        mca_btl_udapl_component.if_list = 
            opal_argv_copy(mca_btl_udapl_component.if_include_list);

    } else if (NULL != mca_btl_udapl_component.if_exclude) {
        mca_btl_udapl_component.if_exclude_list = 
            opal_argv_split(mca_btl_udapl_component.if_exclude, ',');
        mca_btl_udapl_component.if_list = 
            opal_argv_copy(mca_btl_udapl_component.if_exclude_list);
    }
    
    /* enumerate uDAPL interfaces */
    /* Have to do weird pointer stuff to make uDAPL happy -
       just an array of DAT_PROVIDER_INFO isn't good enough. */
    datinfo = malloc(sizeof(DAT_PROVIDER_INFO) *
            mca_btl_udapl_component.udapl_max_btls);
    datinfoptr = malloc(sizeof(DAT_PROVIDER_INFO*) *
            mca_btl_udapl_component.udapl_max_btls);
    if(NULL == datinfo || NULL == datinfoptr) {
        return NULL;
    }

     for(i = 0; i < (int32_t)mca_btl_udapl_component.udapl_max_btls; i++) {
         datinfoptr[i] = &datinfo[i];
     }

    if(DAT_SUCCESS != dat_registry_list_providers(
            mca_btl_udapl_component.udapl_max_btls,
            (DAT_COUNT*)&num_ias, datinfoptr)) {
        free(datinfo);
        free(datinfoptr);
        return NULL;
    }

    free(datinfoptr);

    /* modify list of IA's to be used when if_in[ex]clude set  */
    if (NULL != mca_btl_udapl_component.if_list) {
        mca_btl_udapl_modify_ia_list(&num_ias, datinfo); 
    }

    /* allocate space for the each possible BTL */
    mca_btl_udapl_component.udapl_btls = (mca_btl_udapl_module_t **)
            malloc(num_ias * sizeof(mca_btl_udapl_module_t *));
    if(NULL == mca_btl_udapl_component.udapl_btls) {
        free(datinfo);
        return NULL;
    }

    /* create a BTL module for each interface */
    for(mca_btl_udapl_component.udapl_num_btls = i = 0; i < num_ias; i++) {
        btl = malloc(sizeof(mca_btl_udapl_module_t));
        if(NULL == btl) {
            free(datinfo);
            free(mca_btl_udapl_component.udapl_btls);
            return NULL;
        }

        /* copy default values into the new BTL */
        memcpy(btl, &mca_btl_udapl_module, sizeof(mca_btl_udapl_module_t));

        /* initialize this BTL */
        /* TODO - make use of the thread-safety info in datinfo also */
        if(OMPI_SUCCESS != mca_btl_udapl_init(datinfo[i].ia_name, btl)) {
            free(btl);
            continue;
        }

        /* register internal control message callback */
        mca_btl_base_active_message_trigger[MCA_BTL_TAG_UDAPL].cbfunc = mca_btl_udapl_receive_control; 
        mca_btl_base_active_message_trigger[MCA_BTL_TAG_UDAPL].cbdata = NULL;

        /* successful btl creation */
        mca_btl_udapl_component.udapl_btls[mca_btl_udapl_component.udapl_num_btls] = btl;
        if(++mca_btl_udapl_component.udapl_num_btls >=
                mca_btl_udapl_component.udapl_max_btls) {
            break;
        }
    }

    /* finished with datinfo */
    free(datinfo);

    /* Make sure we have some interfaces */
    if(0 == mca_btl_udapl_component.udapl_num_btls) {
        mca_btl_base_error_no_nics("uDAPL", "NIC");
        free(mca_btl_udapl_component.udapl_btls);
        return NULL;
    }

    /* publish uDAPL parameters with the MCA framework */
    if (OMPI_SUCCESS != mca_btl_udapl_modex_send()) {
        free(mca_btl_udapl_component.udapl_btls);
        return NULL;
    }

    /* Post OOB receive */
    mca_btl_udapl_endpoint_post_oob_recv();

    /* return array of BTLs */
    btls = (mca_btl_base_module_t**) malloc(sizeof(mca_btl_base_module_t *) *
            mca_btl_udapl_component.udapl_num_btls);
    if (NULL == btls) {
        free(mca_btl_udapl_component.udapl_btls);
        return NULL;
    }

    memcpy(btls, mca_btl_udapl_component.udapl_btls,
           mca_btl_udapl_component.udapl_num_btls *
           sizeof(mca_btl_udapl_module_t *));
    *num_btl_modules = mca_btl_udapl_component.udapl_num_btls;
    return btls;
}


static int mca_btl_udapl_accept_connect(mca_btl_udapl_module_t* btl,
                                        DAT_CR_HANDLE cr_handle)
{
    DAT_EP_HANDLE ep;
    int rc;
    mca_btl_base_endpoint_t* proc_ep;
    mca_btl_udapl_addr_t priv_data_in_addr;
    int32_t priv_data_in_conn_type;     /* incoming endpoint type  */

    if (mca_btl_udapl_component.udapl_conn_priv_data) {
        DAT_CR_PARAM cr_param;

        /* query the connection request for incoming private data */
        rc = dat_cr_query(cr_handle,
                    DAT_CR_FIELD_ALL,
                    &cr_param);
        if (rc != DAT_SUCCESS) {
            char* major;
            char* minor;

            dat_strerror(rc, (const char**)&major,
                (const char**)&minor);
            BTL_ERROR(("ERROR: %s %s %s\n", "dat_cr_query",
                major, minor));
            return OMPI_ERROR;
        }

        /* retrieve data from connection request event;
         * cr_param contains remote_port_qual but we need to
         * match on the psp port and address of remote
         * so we get this from the private data.
         */
        memcpy(&priv_data_in_addr,
            (mca_btl_udapl_addr_t *)cr_param.private_data,
            sizeof(mca_btl_udapl_addr_t));
        priv_data_in_conn_type = *(int32_t *)
            ((char *)cr_param.private_data + sizeof(mca_btl_udapl_addr_t));
    }

    /* create the endpoint for the incoming connection */
    rc = mca_btl_udapl_endpoint_create(btl, &ep);
    if(OMPI_SUCCESS != rc) {
        BTL_ERROR(("ERROR: mca_btl_udapl_endpoint_create"));
        return OMPI_ERROR;
    }
    
    /* cr_param no longer valid once dat_cr_accept called */
    rc = dat_cr_accept(cr_handle, ep, 0, NULL);
    if(DAT_SUCCESS != rc) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major,
            (const char**)&minor);
        BTL_ERROR(("ERROR: %s %s %s\n", "dat_cr_accept",
            major, minor));
        return OMPI_ERROR;
    }

    if (mca_btl_udapl_component.udapl_conn_priv_data) {
        /* With accept now in process find a home for the DAT ep by
         * matching against the private data that came in on the
         * connection request event
         */

        /* find the endpoint which matches the address in data received */
        proc_ep = 
            mca_btl_udapl_find_endpoint_address_match(btl, priv_data_in_addr);

        if (proc_ep == NULL) {
            return OMPI_ERROR;
        }

        if (BTL_UDAPL_EAGER_CONNECTION == priv_data_in_conn_type) {
            proc_ep->endpoint_eager = ep;
        } else {
            assert(BTL_UDAPL_MAX_CONNECTION == priv_data_in_conn_type);
            proc_ep->endpoint_max = ep;
        }
    }

    return OMPI_SUCCESS;
}


static inline int mca_btl_udapl_sendrecv(mca_btl_udapl_module_t* btl,
        DAT_EP_HANDLE* endpoint)
{
    int rc;
    mca_btl_udapl_frag_t* frag;
    DAT_DTO_COOKIE cookie;
    static int32_t connection_seq = 1;
    uint32_t flags = 0;
    mca_btl_base_endpoint_t* btl_endpoint = NULL; /* endpoint required by
                                                   * mca_btl_udapl_alloc has not
                                                   * been created at this point
                                                   */
    
    /* Post a receive to get the peer's address data */
    frag = (mca_btl_udapl_frag_t*)
        mca_btl_udapl_alloc(
                            &btl->super,
                            btl_endpoint, 
                            MCA_BTL_NO_ORDER,
                            sizeof(mca_btl_udapl_addr_t) +
                            sizeof(int32_t),
                            flags);
    cookie.as_ptr = frag;

    frag->type = MCA_BTL_UDAPL_CONN_RECV;

    rc = dat_ep_post_recv(endpoint, 1,
            &frag->triplet, cookie, DAT_COMPLETION_DEFAULT_FLAG);
    if(DAT_SUCCESS != rc) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major,
            (const char**)&minor);
        BTL_ERROR(("ERROR: %s %s %s\n", "dat_ep_post_recv",
            major, minor));
        return OMPI_ERROR;
    }


    /* Send our local address data over this EP */
    frag = (mca_btl_udapl_frag_t*)
        mca_btl_udapl_alloc(
                            &btl->super, 
                            btl_endpoint, 
                            MCA_BTL_NO_ORDER,
                            sizeof(mca_btl_udapl_addr_t) +
                            sizeof(int32_t),
                            flags);
    cookie.as_ptr = frag;

    memcpy(frag->segment.seg_addr.pval,
            &btl->udapl_addr, sizeof(mca_btl_udapl_addr_t));
    memcpy((char *)frag->segment.seg_addr.pval + sizeof(mca_btl_udapl_addr_t),
            &connection_seq, sizeof(int32_t));
    connection_seq++;

    frag->type = MCA_BTL_UDAPL_CONN_SEND;

    rc = dat_ep_post_send(endpoint, 1,
            &frag->triplet, cookie, DAT_COMPLETION_DEFAULT_FLAG);
    if(DAT_SUCCESS != rc) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major,
            (const char**)&minor);
        BTL_ERROR(("ERROR: %s %s %s\n", "dat_ep_post_send",
            major, minor));
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

static inline int mca_btl_udapl_frag_progress_one(
        mca_btl_udapl_module_t* udapl_btl,
        mca_btl_udapl_frag_t* frag)
{
    int rc;

    switch(frag->type) {
        case MCA_BTL_UDAPL_SEND:
            rc = mca_btl_udapl_endpoint_send(frag->endpoint, frag);
            break;
        case MCA_BTL_UDAPL_PUT:
            rc = mca_btl_udapl_put(&udapl_btl->super,
                frag->endpoint,
                &frag->base);
            break;
        default:
            rc = OMPI_ERROR; 
            BTL_ERROR(("Error : Progressing pending operation, invalid type %d\n",
                frag->type));
            break;
    }

    return rc;
}

void mca_btl_udapl_frag_progress_pending(mca_btl_udapl_module_t* udapl_btl,
                                        mca_btl_base_endpoint_t* endpoint,
                                        const int connection)
{
    int len;
    int i;
    int token_avail;
    mca_btl_udapl_frag_t* frag;
    
    if (BTL_UDAPL_EAGER_CONNECTION == connection) {
        len = opal_list_get_size(&endpoint->endpoint_eager_frags);

        /* progress eager frag queue as needed */
	BTL_UDAPL_TOKEN_AVAIL(endpoint, connection, token_avail);
	
        for(i = 0; i < len && token_avail > 0; i++) {

            OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
            frag = (mca_btl_udapl_frag_t*)opal_list_remove_first(&(endpoint->endpoint_eager_frags));
            OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
            if(NULL == frag) {
                return;
            }
            if(mca_btl_udapl_frag_progress_one(udapl_btl, frag) !=
                OMPI_SUCCESS) {
                BTL_ERROR(("ERROR: Not able to progress on connection(%d)\n",
                    BTL_UDAPL_EAGER_CONNECTION));
                return;
            }
            BTL_UDAPL_TOKEN_AVAIL(endpoint, connection, token_avail);
        }

    } else if (BTL_UDAPL_MAX_CONNECTION == connection) {
        len = opal_list_get_size(&endpoint->endpoint_max_frags);

        BTL_UDAPL_TOKEN_AVAIL(endpoint, connection, token_avail);
	
        /* progress max frag queue as needed */
        for(i = 0; i < len && token_avail > 0; i++) {

            OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
            frag = (mca_btl_udapl_frag_t*)opal_list_remove_first(&(endpoint->endpoint_max_frags));
            OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
            if(NULL == frag) {
                return;
            }
            if(mca_btl_udapl_frag_progress_one(udapl_btl, frag) !=
                OMPI_SUCCESS) {
                BTL_ERROR(("ERROR: Not able to progress on connection(%d)\n",
                    BTL_UDAPL_MAX_CONNECTION));
                return;
            }
	    BTL_UDAPL_TOKEN_AVAIL(endpoint, connection, token_avail);
        }

    } else {
        BTL_ERROR(("ERROR: Can not progress pending fragment on unknown connection\n"));
    }
    return;
}

/*
 *  uDAPL component progress.
 */

int mca_btl_udapl_component_progress()
{
    mca_btl_udapl_module_t* btl;
    static int32_t inprogress = 0;
    DAT_EVENT event;
    size_t i;
    int32_t j, rdma_ep_count;
    int count = 0, btl_ownership;
    mca_btl_udapl_frag_t* frag;
    mca_btl_base_endpoint_t* endpoint;

    /* prevent deadlock - only one thread should be 'progressing' at a time */
    if(OPAL_THREAD_ADD32(&inprogress, 1) > 1) {
        OPAL_THREAD_ADD32(&inprogress, -1);
        return OMPI_SUCCESS;
    }

    /* check for work to do on each uDAPL btl */
    OPAL_THREAD_LOCK(&mca_btl_udapl_component.udapl_lock);
    for(i = 0; i < mca_btl_udapl_component.udapl_num_btls; i++) {
        btl = mca_btl_udapl_component.udapl_btls[i];

        /* Check DTO EVD */
        while(DAT_SUCCESS ==
                dat_evd_dequeue(btl->udapl_evd_dto, &event)) {
            DAT_DTO_COMPLETION_EVENT_DATA* dto;

            switch(event.event_number) {
            case DAT_DTO_COMPLETION_EVENT:
                dto = &event.event_data.dto_completion_event_data;

                frag = dto->user_cookie.as_ptr;

                /* Was the DTO successful? */
                if(DAT_DTO_SUCCESS != dto->status) {

                    if (DAT_DTO_ERR_FLUSHED == dto->status) {

                        BTL_UDAPL_VERBOSE_OUTPUT(VERBOSE_INFORM,
                            ("DAT_DTO_ERR_FLUSHED: probably OK if occurs during MPI_Finalize().\n"));
                    } else {

                        BTL_UDAPL_VERBOSE_OUTPUT(VERBOSE_CRITICAL,
                            ("ERROR: DAT_DTO_COMPLETION_EVENT: %d %d %lu %p.\n",
                                dto->status, frag->type,
                                (unsigned long)frag->size, dto->ep_handle));
                    }
                    return OMPI_ERROR;		    
                }
                endpoint = frag->endpoint;
                btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);

                switch(frag->type) {
                case MCA_BTL_UDAPL_RDMA_WRITE:
                {
                    assert(frag->base.des_src == &frag->segment);
                    assert(frag->base.des_src_cnt == 1);
                    assert(frag->base.des_dst == NULL);
                    assert(frag->base.des_dst_cnt == 0);
                    assert(frag->type == MCA_BTL_UDAPL_RDMA_WRITE);
    
                    frag->base.des_cbfunc(&btl->super, endpoint,
                        &frag->base, OMPI_SUCCESS);
                    if( btl_ownership ) {
                        mca_btl_udapl_free(&btl->super,
                            &frag->base);
                    }

                    OPAL_THREAD_ADD32(&(endpoint->endpoint_lwqe_tokens[BTL_UDAPL_EAGER_CONNECTION]), 1);

		    mca_btl_udapl_frag_progress_pending(btl,
                        endpoint, BTL_UDAPL_EAGER_CONNECTION);

                    break;
                }
                case MCA_BTL_UDAPL_SEND:
                {
                    int connection = BTL_UDAPL_EAGER_CONNECTION;

                    assert(frag->base.des_src == &frag->segment);
                    assert(frag->base.des_src_cnt == 1);
                    assert(frag->base.des_dst == NULL);
                    assert(frag->base.des_dst_cnt == 0);
                    assert(frag->type == MCA_BTL_UDAPL_SEND);

                    if(frag->size !=
                            mca_btl_udapl_component.udapl_eager_frag_size) {
                        assert(frag->size ==
                            mca_btl_udapl_component.udapl_max_frag_size);

                        connection = BTL_UDAPL_MAX_CONNECTION;
                    }
                    frag->base.des_cbfunc(&btl->super, endpoint,
                            &frag->base, OMPI_SUCCESS);
                    if( btl_ownership ) {
                        mca_btl_udapl_free(&btl->super,
                            &frag->base);
                    }

                    OPAL_THREAD_ADD32(&(endpoint->endpoint_lwqe_tokens[connection]), 1);

                    mca_btl_udapl_frag_progress_pending(btl,
                        endpoint, connection);
                    break;
                }
                case MCA_BTL_UDAPL_RECV:
                {
                    mca_btl_active_message_callback_t* reg;
                    int cntrl_msg = -1;

                    assert(frag->base.des_dst == &frag->segment);
                    assert(frag->base.des_dst_cnt == 1);
                    assert(frag->base.des_src == NULL);
                    assert(frag->base.des_src_cnt == 0);
                    assert(frag->type == MCA_BTL_UDAPL_RECV);
                    assert(frag->triplet.virtual_address ==
                            (DAT_VADDR)(uintptr_t)frag->segment.seg_addr.pval);
                    assert(frag->triplet.segment_length == frag->size);
                    assert(frag->btl == btl);

                    /* setup frag ftr location and do callback */
                    frag->segment.seg_len = dto->transfered_length -
                        sizeof(mca_btl_udapl_footer_t);
                    frag->ftr = (mca_btl_udapl_footer_t *)
                        ((char *)frag->segment.seg_addr.pval + 
                        frag->segment.seg_len);

                    cntrl_msg = frag->ftr->tag;

                    reg = mca_btl_base_active_message_trigger + frag->ftr->tag;
                    OPAL_THREAD_UNLOCK(&mca_btl_udapl_component.udapl_lock);

                    reg->cbfunc(&btl->super,
                            frag->ftr->tag, &frag->base, reg->cbdata);
                    OPAL_THREAD_LOCK(&mca_btl_udapl_component.udapl_lock);

                    /* Repost the frag */
                    frag->ftr = frag->segment.seg_addr.pval;
                    frag->segment.seg_len =
                        (frag->size - sizeof(mca_btl_udapl_footer_t) -
                            sizeof(mca_btl_udapl_rdma_footer_t)); 
                    frag->base.des_flags = 0;

                    if(frag->size ==
                              mca_btl_udapl_component.udapl_eager_frag_size) {

                        OPAL_THREAD_ADD32(&(frag->endpoint->endpoint_sr_credits[BTL_UDAPL_EAGER_CONNECTION]), 1);

                        dat_ep_post_recv(frag->endpoint->endpoint_eager,
                            1, &frag->triplet, dto->user_cookie,
                            DAT_COMPLETION_DEFAULT_FLAG);

                        if (frag->endpoint->endpoint_sr_credits[BTL_UDAPL_EAGER_CONNECTION] >=
                            mca_btl_udapl_component.udapl_sr_win) {
                            mca_btl_udapl_endpoint_send_sr_credits(frag->endpoint,
                                BTL_UDAPL_EAGER_CONNECTION);
                        }

                        if (MCA_BTL_TAG_UDAPL == cntrl_msg) {
                            mca_btl_udapl_frag_progress_pending(btl,
                                frag->endpoint,
                                BTL_UDAPL_EAGER_CONNECTION);
                        }

                    } else {
                        assert(frag->size ==
                            mca_btl_udapl_component.udapl_max_frag_size);

                        OPAL_THREAD_ADD32(&(frag->endpoint->endpoint_sr_credits[BTL_UDAPL_MAX_CONNECTION]), 1);

                        dat_ep_post_recv(frag->endpoint->endpoint_max,
                            1, &frag->triplet, dto->user_cookie,
                            DAT_COMPLETION_DEFAULT_FLAG);

                        if (frag->endpoint->endpoint_sr_credits[BTL_UDAPL_MAX_CONNECTION] >=
                            mca_btl_udapl_component.udapl_sr_win) {
                            mca_btl_udapl_endpoint_send_sr_credits(frag->endpoint,
                                BTL_UDAPL_MAX_CONNECTION);
                        }

                        if (MCA_BTL_TAG_UDAPL == cntrl_msg) {
                            mca_btl_udapl_frag_progress_pending(btl,
                                frag->endpoint,
                                BTL_UDAPL_MAX_CONNECTION);
                        }
                    }

                    break;
                }
                case MCA_BTL_UDAPL_PUT:
                {
                    assert(frag->base.des_src == &frag->segment);
                    assert(frag->base.des_src_cnt == 1);
                    assert(frag->base.des_dst_cnt == 1);
                    assert(frag->type == MCA_BTL_UDAPL_PUT);
                    
                    frag->base.des_cbfunc(&btl->super, endpoint,
                        &frag->base, OMPI_SUCCESS);
                    if( btl_ownership ) {
                        mca_btl_udapl_free(&btl->super,
                            &frag->base);
                    }

                    OPAL_THREAD_ADD32(&(endpoint->endpoint_lwqe_tokens[BTL_UDAPL_MAX_CONNECTION]), 1);
                    OPAL_THREAD_ADD32(&(endpoint->endpoint_sr_tokens[BTL_UDAPL_MAX_CONNECTION]), 1);

                    mca_btl_udapl_frag_progress_pending(btl,
                        endpoint, BTL_UDAPL_MAX_CONNECTION);
         
                    break;
                }                    
                case MCA_BTL_UDAPL_CONN_RECV:
                    mca_btl_udapl_endpoint_finish_connect(btl,
                            frag->segment.seg_addr.pval,
                            (int32_t *)((char *)frag->segment.seg_addr.pval  +
                                sizeof(mca_btl_udapl_addr_t)),
                            event.event_data.connect_event_data.ep_handle);
                    /* No break - fall through to free */
                case MCA_BTL_UDAPL_CONN_SEND:
                    frag->segment.seg_len =
                            mca_btl_udapl_module.super.btl_eager_limit;
                    mca_btl_udapl_free(&btl->super, &frag->base);
                    break;
                default:
                    BTL_UDAPL_VERBOSE_OUTPUT(VERBOSE_DIAGNOSE,
                        ("WARNING: unknown frag type: %d\n",
                        frag->type));
                }
                count++;
                break;
            default:
                BTL_UDAPL_VERBOSE_OUTPUT(VERBOSE_DIAGNOSE,
                    ("WARNING: DTO event: %s (%d)\n",
                    mca_btl_udapl_dat_event_to_string(event.event_number),
                    event.event_number));
            }
        }

        /* Check connection EVD */
        while((btl->udapl_connect_inprogress > 0) && (DAT_SUCCESS ==
            dat_evd_dequeue(btl->udapl_evd_conn, &event))) {

            switch(event.event_number) {
                case DAT_CONNECTION_REQUEST_EVENT:
                    /* Accept a new connection */
                    mca_btl_udapl_accept_connect(btl,
                            event.event_data.cr_arrival_event_data.cr_handle);
                    count++;
                    break;
                case DAT_CONNECTION_EVENT_ESTABLISHED:
                    /* Both the client and server side of a connection generate
                       this event */
                    if (mca_btl_udapl_component.udapl_conn_priv_data) {
                        /* private data is only valid at this point if this 
                         * event is from a dat_ep_connect call, not an accept
                         */
                        mca_btl_udapl_endpoint_pd_established_conn(btl,
                            event.event_data.connect_event_data.ep_handle);
                    } else {
                        /* explicitly exchange process data */
                        mca_btl_udapl_sendrecv(btl,
                            event.event_data.connect_event_data.ep_handle);
                    }
                    count++;
                    break;
                case DAT_CONNECTION_EVENT_PEER_REJECTED:
                case DAT_CONNECTION_EVENT_NON_PEER_REJECTED:
                case DAT_CONNECTION_EVENT_ACCEPT_COMPLETION_ERROR:
                case DAT_CONNECTION_EVENT_DISCONNECTED:
                case DAT_CONNECTION_EVENT_BROKEN:
                case DAT_CONNECTION_EVENT_TIMED_OUT:
                    /* handle this case specially? if we have finite timeout,
                       we might want to try connecting again here. */
                case DAT_CONNECTION_EVENT_UNREACHABLE:
                    /* Need to set the BTL endpoint to MCA_BTL_UDAPL_FAILED
                       See dat_ep_connect documentation pdf pg 198 */
                    BTL_UDAPL_VERBOSE_OUTPUT(VERBOSE_CRITICAL,
                        ("WARNING: connection event not handled : %s (%d)\n",
                        mca_btl_udapl_dat_event_to_string(event.event_number),
                        event.event_number));
                    break;
                default:
                    BTL_ERROR(("ERROR: connection event : %s (%d)",
                        mca_btl_udapl_dat_event_to_string(event.event_number),
                        event.event_number));
            }
        }

        /* Check async EVD */
        if (btl->udapl_async_events == mca_btl_udapl_component.udapl_async_events) {
            btl->udapl_async_events = 0;

            while(DAT_SUCCESS ==
                dat_evd_dequeue(btl->udapl_evd_async, &event)) {

                switch(event.event_number) {
                case DAT_ASYNC_ERROR_EVD_OVERFLOW:
                case DAT_ASYNC_ERROR_IA_CATASTROPHIC:
                case DAT_ASYNC_ERROR_EP_BROKEN:
                case DAT_ASYNC_ERROR_TIMED_OUT:
                case DAT_ASYNC_ERROR_PROVIDER_INTERNAL_ERROR:
                    BTL_UDAPL_VERBOSE_OUTPUT(VERBOSE_CRITICAL,
                        ("WARNING: async event ignored : %s (%d)",
                        mca_btl_udapl_dat_event_to_string(event.event_number),
                        event.event_number));
                    break;
                default:
                    BTL_UDAPL_VERBOSE_OUTPUT(VERBOSE_CRITICAL,
                        ("WARNING: %s (%d)\n",
                        mca_btl_udapl_dat_event_to_string(event.event_number),
                        event.event_number));
                }
            }
        } else {
            btl->udapl_async_events++;
        }

        /*
         * Check eager rdma segments
         */
        
        /* find the number of endpoints with rdma buffers */
        rdma_ep_count = btl->udapl_eager_rdma_endpoint_count;
        
        for (j = 0; j < rdma_ep_count; j++) {
            mca_btl_udapl_endpoint_t* endpoint;
            mca_btl_udapl_frag_t *local_rdma_frag;

            endpoint =
                opal_pointer_array_get_item(btl->udapl_eager_rdma_endpoints, j);

            OPAL_THREAD_LOCK(&endpoint->endpoint_eager_rdma_local.lock);

            local_rdma_frag =             
                MCA_BTL_UDAPL_GET_LOCAL_RDMA_FRAG(endpoint,
                    endpoint->endpoint_eager_rdma_local.head);

            if (local_rdma_frag->rdma_ftr->active == 1) {
                int pad = 0;
                mca_btl_active_message_callback_t* reg;

                MCA_BTL_UDAPL_RDMA_NEXT_INDEX(endpoint->endpoint_eager_rdma_local.head);
                OPAL_THREAD_UNLOCK(&endpoint->endpoint_eager_rdma_local.lock);

                /* compute pad as needed */
                MCA_BTL_UDAPL_FRAG_CALC_ALIGNMENT_PAD(pad,
                    (local_rdma_frag->rdma_ftr->size +
                        sizeof(mca_btl_udapl_footer_t)));
                
                /* set fragment information */
                local_rdma_frag->ftr = (mca_btl_udapl_footer_t *)
                    ((char *)local_rdma_frag->rdma_ftr -
                        pad -
                        sizeof(mca_btl_udapl_footer_t));
                local_rdma_frag->segment.seg_len =
                    local_rdma_frag->rdma_ftr->size;
                local_rdma_frag->segment.seg_addr.pval = (unsigned char *)
                    ((char *)local_rdma_frag->ftr -
                        local_rdma_frag->segment.seg_len);

                /* trigger callback */
                reg = mca_btl_base_active_message_trigger + local_rdma_frag->ftr->tag;
                reg->cbfunc(&btl->super,
                    local_rdma_frag->ftr->tag, &local_rdma_frag->base, reg->cbdata);

                /* repost */
                local_rdma_frag->rdma_ftr->active = 0; 
                local_rdma_frag->segment.seg_len =
                    mca_btl_udapl_module.super.btl_eager_limit;
                local_rdma_frag->base.des_flags = 0;

                /* increment local rdma credits */
                OPAL_THREAD_ADD32(&(endpoint->endpoint_eager_rdma_local.credits),
                    1);

                if (endpoint->endpoint_eager_rdma_local.credits >=
                    mca_btl_udapl_component.udapl_eager_rdma_win) {
                    mca_btl_udapl_endpoint_send_eager_rdma_credits(endpoint);
                }

                count++;

            } else {
                OPAL_THREAD_UNLOCK(&endpoint->endpoint_eager_rdma_local.lock);
            }
        } /* end of rdma_count loop */
    }

    /* unlock and return */
    OPAL_THREAD_UNLOCK(&mca_btl_udapl_component.udapl_lock);
    OPAL_THREAD_ADD32(&inprogress, -1);
    return count;
}

