/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * In windows, many of the socket functions return an EWOULDBLOCK
 * instead of \ things like EAGAIN, EINPROGRESS, etc. It has been
 * verified that this will \ not conflict with other error codes that
 * are returned by these functions \ under UNIX/Linux environments 
 */

#include "ompi_config.h"

#include "opal/opal_socket_errno.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>
#include <fcntl.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif

#include "ompi/constants.h"
#include "opal/event/event.h"
#include "opal/util/if.h"
#include "opal/util/argv.h"
#include "ompi/mca/btl/btl.h"

#include "opal/mca/base/mca_base_param.h"
#include "ompi/runtime/ompi_module_exchange.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/mca/mpool/base/base.h" 
#include "ompi/mca/btl/base/btl_base_error.h"
#include "btl_sctp.h"
#include "btl_sctp_addr.h"
#include "btl_sctp_proc.h"
#include "btl_sctp_frag.h"
#include "btl_sctp_endpoint.h" 
#include "ompi/mca/btl/base/base.h" 

#include <netinet/sctp.h>
#include "btl_sctp_recv_handler.h"
#include "btl_sctp_component.h"

static int mca_btl_sctp_component_register(void);
static int mca_btl_sctp_component_open(void);
static int mca_btl_sctp_component_close(void);


mca_btl_sctp_component_t mca_btl_sctp_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        {
            MCA_BTL_BASE_VERSION_2_0_0,

            "sctp", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_btl_sctp_component_open,  /* component open */
            mca_btl_sctp_component_close,  /* component close */
            NULL,
            mca_btl_sctp_component_register, /* component register */
        },
        {
            /* Whether the component is checkpointable or not */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        mca_btl_sctp_component_init,  
        NULL,
    }
};

#if MCA_BTL_SCTP_DONT_USE_HASH
struct mca_btl_sctp_proc_table_node *recvr_proc_table;
struct mca_btl_sctp_proc_table_node *sender_proc_table;
#endif

/*
 * utility routines for parameter registration
 */

static inline char* mca_btl_sctp_param_register_string(
                                                     const char* param_name, 
                                                     const char* default_value)
{
    char *param_value;
    char *help_string = NULL;    
    mca_base_param_reg_string(&mca_btl_sctp_component.super.btl_version,
                              param_name, help_string, false, false,
                              default_value, &param_value);   
    return param_value;
}

static inline int mca_btl_sctp_param_register_int(
        const char* param_name, 
        int default_value)
{
    int param_value;
    char *help_string = NULL;
    mca_base_param_reg_int(&mca_btl_sctp_component.super.btl_version,
                           param_name, help_string, false, false, 
                           default_value, &param_value);
    return param_value;
}


/*
 * Data structure for accepting connections.
 */

struct mca_btl_sctp_event_t {
    opal_list_item_t item;
    opal_event_t event;
};
typedef struct mca_btl_sctp_event_t mca_btl_sctp_event_t;

static void mca_btl_sctp_event_construct(mca_btl_sctp_event_t* event)
{
    OPAL_THREAD_LOCK(&mca_btl_sctp_component.sctp_lock);
    opal_list_append(&mca_btl_sctp_component.sctp_events, &event->item);
    OPAL_THREAD_UNLOCK(&mca_btl_sctp_component.sctp_lock);
}

static void mca_btl_sctp_event_destruct(mca_btl_sctp_event_t* event)
{
    OPAL_THREAD_LOCK(&mca_btl_sctp_component.sctp_lock);
    opal_list_remove_item(&mca_btl_sctp_component.sctp_events, &event->item);
    OPAL_THREAD_UNLOCK(&mca_btl_sctp_component.sctp_lock);
}

OBJ_CLASS_INSTANCE(
    mca_btl_sctp_event_t,
    opal_list_item_t,
    mca_btl_sctp_event_construct,
    mca_btl_sctp_event_destruct);


/*
 * functions for receiving event callbacks
 */

static void mca_btl_sctp_component_recv_handler(int, short, void*); /* for 1-1 */
/* mca_btl_sctp_recv_handler(int, short, void*) for 1-many is in btl_sctp_recv_handler.h */


static int mca_btl_sctp_component_register(void)
{
    /* register SCTP component parameters */
    /* num links */
    mca_btl_sctp_component.sctp_if_include =
        mca_btl_sctp_param_register_string("if_include", "");
    mca_btl_sctp_component.sctp_if_exclude =
        mca_btl_sctp_param_register_string("if_exclude", "lo");
    mca_btl_sctp_component.sctp_free_list_num =
        mca_btl_sctp_param_register_int ("free_list_num", 8);
    mca_btl_sctp_component.sctp_free_list_max =
        mca_btl_sctp_param_register_int ("free_list_max", -1);
    mca_btl_sctp_component.sctp_free_list_inc =
        mca_btl_sctp_param_register_int ("free_list_inc", 32);
    mca_btl_sctp_component.sctp_sndbuf =
        mca_btl_sctp_param_register_int ("sndbuf", 128*1024);
    mca_btl_sctp_component.sctp_rcvbuf =
        mca_btl_sctp_param_register_int ("rcvbuf", 128*1024);
    mca_btl_sctp_component.sctp_endpoint_cache =
        mca_btl_sctp_param_register_int ("endpoint_cache", 30*1024);
    mca_btl_sctp_component.sctp_use_nodelay =
        !mca_btl_sctp_param_register_int ("use_nagle", 0);
    /* port_min */
    /* port_range */
    /* use a single one-to-many socket by default except in Solaris (see
     *  the configure.m4 file)
     */
    mca_base_param_reg_int(&mca_btl_sctp_component.super.btl_version,
                           "if_11", "If 0, have one SCTP BTL module and let SCTP do multilink scheduling. If non-zero, have an SCTP BTL module per link and let the PML do the scheduling.",
                           false, false,
                           OMPI_MCA_BTL_SCTP_USE_ONE_TO_ONE_SOCKET,
                           &mca_btl_sctp_component.sctp_if_11);

    /* have lower exclusivity than tcp */
    mca_btl_sctp_module.super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_LOW;
    mca_btl_sctp_module.super.btl_eager_limit = 64*1024;
    mca_btl_sctp_module.super.btl_rndv_eager_limit = 64*1024;
    mca_btl_sctp_module.super.btl_max_send_size = 128*1024;
    mca_btl_sctp_module.super.btl_rdma_pipeline_send_length = 128*1024;
    mca_btl_sctp_module.super.btl_rdma_pipeline_frag_size = INT_MAX;
    mca_btl_sctp_module.super.btl_min_rdma_pipeline_size = 0;
    mca_btl_sctp_module.super.btl_flags  = MCA_BTL_FLAGS_PUT |
                                       MCA_BTL_FLAGS_SEND_INPLACE |
                                       MCA_BTL_FLAGS_NEED_CSUM | 
                                       MCA_BTL_FLAGS_NEED_ACK |
                                       MCA_BTL_FLAGS_HETEROGENEOUS_RDMA;
    mca_btl_sctp_module.super.btl_bandwidth = 100;
    mca_btl_sctp_module.super.btl_latency = 100;
    mca_btl_base_param_register(&mca_btl_sctp_component.super.btl_version,
            &mca_btl_sctp_module.super);

    return OMPI_SUCCESS;
}

/*
 *  Called by MCA framework to open the component
 */

static int mca_btl_sctp_component_open(void)
{
#ifdef __WINDOWS__
    WSADATA win_sock_data;
    if (WSAStartup(MAKEWORD(2,2), &win_sock_data) != 0) {
        BTL_ERROR(("failed to initialise windows sockets:%d", WSAGetLastError()));
        return OMPI_ERROR;
    }
#endif

    /* initialize state */
    mca_btl_sctp_component.sctp_listen_sd = -1;
    /* TODO different sd for ipv6 */
    mca_btl_sctp_component.sctp_num_btls=0;
    /* addr_count */
    mca_btl_sctp_component.sctp_btls=NULL;
    
    /* initialize objects */ 
    OBJ_CONSTRUCT(&mca_btl_sctp_component.sctp_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_btl_sctp_component.sctp_procs, opal_hash_table_t);
    OBJ_CONSTRUCT(&mca_btl_sctp_component.sctp_events, opal_list_t);
    OBJ_CONSTRUCT(&mca_btl_sctp_component.sctp_frag_eager, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_sctp_component.sctp_frag_max, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_sctp_component.sctp_frag_user, ompi_free_list_t);
    opal_hash_table_init(&mca_btl_sctp_component.sctp_procs, 256);
#if MCA_BTL_SCTP_DONT_USE_HASH
    /* TODO make this only allocate how much it needs to.  Currently
     *  allocates 256 (to match sctp_procs). recvr_proc_table and
     *  sender_proc_table are malloc'd in mca_btl_sctp_component_init.
     */
    recvr_proc_table = NULL;
    sender_proc_table = NULL;
#else
    OBJ_CONSTRUCT(&mca_btl_sctp_component.sctp_assocID_hash, opal_hash_table_t);
    opal_hash_table_init(&mca_btl_sctp_component.sctp_assocID_hash, 256);
#endif

    /* if_include and if_exclude need to be mutually exclusive */
    if (OPAL_SUCCESS != 
        mca_base_param_check_exclusive_string(
        mca_btl_sctp_component.super.btl_version.mca_type_name,
        mca_btl_sctp_component.super.btl_version.mca_component_name,
        "if_include",
        mca_btl_sctp_component.super.btl_version.mca_type_name,
        mca_btl_sctp_component.super.btl_version.mca_component_name,
        "if_exclude")) {
        /* Return ERR_NOT_AVAILABLE so that a warning message about
           "open" failing is not printed */
        return OMPI_ERR_NOT_AVAILABLE;
    }

    /* setup receive buffer */
    if(0 == mca_btl_sctp_recv_handler_initbuf()) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    return OMPI_SUCCESS;
}


/*
 * module cleanup - sanity checking of queue lengths
 */

int mca_btl_sctp_component_close(void)
{
    opal_list_item_t* item;
    opal_list_item_t* next;

    if(NULL != mca_btl_sctp_component.sctp_if_include) {
        free(mca_btl_sctp_component.sctp_if_include);
    }
    if(NULL != mca_btl_sctp_component.sctp_if_exclude) {
       free(mca_btl_sctp_component.sctp_if_exclude);
    }
    if (NULL != mca_btl_sctp_component.sctp_btls) {
        free(mca_btl_sctp_component.sctp_btls);
    }

    mca_btl_sctp_recv_handler_freebuf();
 
    if (mca_btl_sctp_component.sctp_listen_sd >= 0) {
        opal_event_del(&mca_btl_sctp_component.sctp_recv_event);
        CLOSE_THE_SOCKET(mca_btl_sctp_component.sctp_listen_sd);
        mca_btl_sctp_component.sctp_listen_sd = -1;
    }

    /* cleanup any pending events */
    OPAL_THREAD_LOCK(&mca_btl_sctp_component.sctp_lock);
    for(item =	opal_list_get_first(&mca_btl_sctp_component.sctp_events);
	item != opal_list_get_end(&mca_btl_sctp_component.sctp_events);
	item = next) {
	mca_btl_sctp_event_t* event = (mca_btl_sctp_event_t*)item;
	next = opal_list_get_next(item);
	opal_event_del(&event->event);
	OBJ_RELEASE(event);
    }
    OPAL_THREAD_UNLOCK(&mca_btl_sctp_component.sctp_lock);

    /* release resources */
    OBJ_DESTRUCT(&mca_btl_sctp_component.sctp_procs);
#if MCA_BTL_SCTP_DONT_USE_HASH
    if(NULL != recvr_proc_table) {
        free(recvr_proc_table);
    }
    if(NULL != sender_proc_table) {
        free(sender_proc_table);
    }
#else
    OBJ_DESTRUCT(&mca_btl_sctp_component.sctp_assocID_hash);
#endif
    OBJ_DESTRUCT(&mca_btl_sctp_component.sctp_events);
    OBJ_DESTRUCT(&mca_btl_sctp_component.sctp_frag_eager);
    OBJ_DESTRUCT(&mca_btl_sctp_component.sctp_frag_max);
    OBJ_DESTRUCT(&mca_btl_sctp_component.sctp_frag_user);
    OBJ_DESTRUCT(&mca_btl_sctp_component.sctp_lock);

#ifdef __WINDOWS__
    WSACleanup();
#endif

    return OMPI_SUCCESS;
}


/*
 *  Create a btl instance and add to modules list.
 */

static int mca_btl_sctp_create(int if_index, const char* if_name)
{
    if(mca_btl_sctp_component.sctp_if_11) {

        char param[256];
        struct mca_btl_sctp_module_t* btl = (struct mca_btl_sctp_module_t *)malloc(sizeof(mca_btl_sctp_module_t));
        if(NULL == btl) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        memcpy(btl, &mca_btl_sctp_module, sizeof(mca_btl_sctp_module));
        OBJ_CONSTRUCT(&btl->sctp_endpoints, opal_list_t);
        mca_btl_sctp_component.sctp_btls[mca_btl_sctp_component.sctp_num_btls++] = btl;

        /* initialize the btl */
        btl->sctp_ifindex = if_index;
#if MCA_BTL_SCTP_STATISTICS
        btl->sctp_bytes_recv = 0;
        btl->sctp_bytes_sent = 0;
        btl->sctp_send_handler = 0;
#endif
        opal_ifindextoaddr(if_index, (struct sockaddr*)&btl->sctp_ifaddr, sizeof(btl->sctp_ifaddr));
        /* prepare for bind call later before connect */
        btl->sctp_ifaddr.sin_family = AF_INET;
#ifdef FREEBSD
        btl->sctp_ifaddr.sin_len = sizeof(struct sockaddr);
#endif
        btl->sctp_ifaddr.sin_port = 0;       
        opal_ifindextomask(if_index, (uint32_t *)&btl->sctp_ifmask, sizeof(btl->sctp_ifmask));

        /* allow user to specify interface bandwidth */
        sprintf(param, "bandwidth_%s", if_name);
        btl->super.btl_bandwidth = mca_btl_sctp_param_register_int(param, 0);

        /* allow user to override/specify latency ranking */
        sprintf(param, "latency_%s", if_name);
        btl->super.btl_latency = mca_btl_sctp_param_register_int(param, 0);

#if 0 && OPAL_ENABLE_DEBUG
        BTL_OUTPUT(("interface: %s bandwidth %d latency %d",
                    if_name, btl->super.btl_bandwidth, btl->super.btl_latency));
#endif
        return OMPI_SUCCESS;
    }

    else {
        /* 1 to many */
        struct mca_btl_sctp_module_t* btl;
        char param[256];
        struct sockaddr_in next_ifaddr;
        socklen_t len = sizeof(struct sockaddr_in);
        opal_socklen_t addrlen;
        
        /* check if this is the first time this function is being called */
        if(0 == mca_btl_sctp_component.sctp_num_btls) {

            /* fill in btl struct with first interface's information (arbitary) */

            btl = (struct mca_btl_sctp_module_t *)malloc(sizeof(mca_btl_sctp_module_t));
            if(NULL == btl) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            memcpy(btl, &mca_btl_sctp_module, sizeof(mca_btl_sctp_module));
            OBJ_CONSTRUCT(&btl->sctp_endpoints, opal_list_t);
            mca_btl_sctp_component.sctp_btls[mca_btl_sctp_component.sctp_num_btls++] = btl;

            /* initialize the btl */
            btl->sctp_ifindex = if_index;
#if MCA_BTL_SCTP_STATISTICS
            btl->sctp_bytes_recv = 0;
            btl->sctp_bytes_sent = 0;
            btl->sctp_send_handler = 0;
#endif
            opal_ifindextoaddr(if_index, (struct sockaddr*)&btl->sctp_ifaddr, sizeof(btl->sctp_ifaddr));
            opal_ifindextomask(if_index, (uint32_t *)&btl->sctp_ifmask, sizeof(btl->sctp_ifmask));

            /* allow user to specify interface bandwidth */
            sprintf(param, "bandwidth_%s", if_name);
            btl->super.btl_bandwidth = mca_btl_sctp_param_register_int(param, 0);

            /* allow user to override/specify latency ranking */
            sprintf(param, "latency_%s", if_name);
            btl->super.btl_latency = mca_btl_sctp_param_register_int(param, 0);

#if 0 && OPAL_ENABLE_DEBUG
            BTL_OUTPUT(("interface: %s bandwidth %d latency %d",
                        if_name, btl->super.btl_bandwidth, btl->super.btl_latency));
#endif
            /* call bind to this (initial) addr */
            opal_ifindextoaddr(if_index, (struct sockaddr*)&next_ifaddr, sizeof(next_ifaddr));
            next_ifaddr.sin_family = AF_INET;
#ifdef FREEBSD
            next_ifaddr.sin_len = sizeof(struct sockaddr);
#endif
            next_ifaddr.sin_port = 0;

            if(bind(mca_btl_sctp_component.sctp_listen_sd, (struct sockaddr *) &next_ifaddr, len) < 0) {
                return OMPI_ERR_FATAL;
            }

            /* resolve system assignend port */
            addrlen = sizeof(struct sockaddr_in);
            if(getsockname(mca_btl_sctp_component.sctp_listen_sd, (struct sockaddr*)&next_ifaddr, &addrlen) < 0) {
                BTL_ERROR(("getsockname() failed with errno=%d", opal_socket_errno));
                return OMPI_ERROR;
            }
            /* need to get the port after the first bind call for subsequent
             *  sctp_bindx calls.
             */
            mca_btl_sctp_component.sctp_listen_port = next_ifaddr.sin_port;
            
        } 

        else {
            next_ifaddr.sin_port = htons((unsigned short) mca_btl_sctp_component.sctp_listen_port);

            /* add this addr to bindx */
            opal_ifindextoaddr(if_index, (struct sockaddr*)&next_ifaddr, sizeof(next_ifaddr));
            next_ifaddr.sin_family = AF_INET;
#ifdef FREEBSD
            next_ifaddr.sin_len = sizeof(struct sockaddr);
#endif

            if(sctp_bindx(mca_btl_sctp_component.sctp_listen_sd, (struct sockaddr *) &next_ifaddr,
                          1, SCTP_BINDX_ADD_ADDR) < 0) {
                return OMPI_ERR_FATAL;
            }

        }

        return OMPI_SUCCESS;
    }
}

/*
 * Create SCTP BTL instance(s) using either:
 * (1) all interfaces specified by the user
 * (2) all available interfaces 
 * (3) all available interfaces except for those excluded by the user
 *
 * For 1-1 sockets, have a BTL per interface.
 * For 1-many sockets, use bind for the first interface and sctp_bindx for each interface thereafter.
 *
 */

static int mca_btl_sctp_component_create_instance(void)
{
    int if_count = opal_ifcount();
    int if_index;
    char **include;
    char **exclude;
    char **argv;

    if(if_count <= 0) {
        return OMPI_ERROR;
    }

    /* Allocate memory for btl pointers.  This may be more space then
       we need as some of the interfaces may get filtered out by the
       if_include and if_exclude parameters.  But that is just a few
       unused pointers. */
    mca_btl_sctp_component.sctp_btls = 
        (mca_btl_sctp_module_t **)malloc(if_count * sizeof(mca_btl_sctp_module_t*));
    if(NULL == mca_btl_sctp_component.sctp_btls) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* if the user specified an interface list - use these exclusively */
    argv = include = opal_argv_split(mca_btl_sctp_component.sctp_if_include,',');
    while(argv && *argv) {
        char* if_name = *argv;
        int if_index = opal_ifnametoindex(if_name);
        if(if_index < 0) {
            BTL_ERROR(("invalid interface \"%s\"", if_name));
        } else {
            mca_btl_sctp_create(if_index, if_name);
        }
        argv++;
    }
    opal_argv_free(include);
    if(mca_btl_sctp_component.sctp_num_btls) {
        return OMPI_SUCCESS;
    }

    /* if the interface list was not specified by the user, create 
     * a BTL for each interface that was not excluded.
     */
    exclude = opal_argv_split(mca_btl_sctp_component.sctp_if_exclude,',');
    for(if_index = opal_ifbegin(); if_index >= 0; if_index = opal_ifnext(if_index)) {
        char if_name[32];
        opal_ifindextoname(if_index, if_name, sizeof(if_name));

        /* check to see if this interface exists in the exclude list */
        if(opal_ifcount() > 1) {
            argv = exclude;
            while(argv && *argv) {
                if(strncmp(*argv,if_name,strlen(*argv)) == 0) {
                    break;
                }
                argv++;
            }
            /* if this interface was not found in the excluded list - create a BTL */
            if(argv == 0 || *argv == 0) {
                mca_btl_sctp_create(if_index, if_name);
            }
        } else {
            mca_btl_sctp_create(if_index, if_name);
        }
    }
    opal_argv_free(exclude);
    return OMPI_SUCCESS;
}

/*
 * Create a listen socket and bind to all interfaces
 */

static int mca_btl_sctp_component_create_listen(void)
{
    if(mca_btl_sctp_component.sctp_if_11) {
        /* 1 to 1 */
        int rc;
        struct sockaddr_in inaddr; 
        opal_socklen_t addrlen;

        /* create a listen socket for incoming connections */
        mca_btl_sctp_component.sctp_listen_sd = socket(AF_INET, SOCK_STREAM, IPPROTO_SCTP);
        if(mca_btl_sctp_component.sctp_listen_sd < 0) {
            if(opal_socket_errno != ESOCKTNOSUPPORT) {
                /* may have SCTP API but SCTP itself unable to auto-load into 
                 *  the kernel (like RHEL4U4), so avoid noisy error output
                 */                
                BTL_ERROR(("socket() failed with errno=%d", opal_socket_errno));
            }
            return OMPI_ERROR;
        }
        if((rc = mca_btl_sctp_set_socket_options(mca_btl_sctp_component.sctp_listen_sd)) != OMPI_SUCCESS) {
            return rc;
        }

        /* bind to all addresses and dynamically assigned port */
        memset(&inaddr, 0, sizeof(inaddr));
        inaddr.sin_family = AF_INET;
        inaddr.sin_addr.s_addr = INADDR_ANY;
        inaddr.sin_port = 0;

        if(bind(mca_btl_sctp_component.sctp_listen_sd, (struct sockaddr*)&inaddr, sizeof(inaddr)) < 0) {
            BTL_ERROR(("bind() failed with errno=%d", opal_socket_errno));
            return OMPI_ERROR;
        }

        /* resolve system assignend port */
        addrlen = sizeof(struct sockaddr_in);
        if(getsockname(mca_btl_sctp_component.sctp_listen_sd, (struct sockaddr*)&inaddr, &addrlen) < 0) {
            BTL_ERROR(("getsockname() failed with errno=%d", opal_socket_errno));
            return OMPI_ERROR;
        }
        mca_btl_sctp_component.sctp_listen_port = inaddr.sin_port;

        /* setup listen backlog to maximum allowed by kernel */
        if(listen(mca_btl_sctp_component.sctp_listen_sd, SOMAXCONN) < 0) {
            BTL_ERROR(("listen() failed with errno=%d", opal_socket_errno));
            return OMPI_ERROR;
        }


        /* register listen port */
        opal_event_set(
                &mca_btl_sctp_component.sctp_recv_event,
                mca_btl_sctp_component.sctp_listen_sd, 
                OPAL_EV_READ|OPAL_EV_PERSIST, 
                mca_btl_sctp_component_recv_handler, 
                0);
        opal_event_add(&mca_btl_sctp_component.sctp_recv_event,0);

        return OMPI_SUCCESS;
    }

    else {
        /* 1 to many */
        int rc;

        /* create a one to many listen socket for incoming connections and ALL sent/received messages */
        mca_btl_sctp_component.sctp_listen_sd = socket(AF_INET, SOCK_SEQPACKET, IPPROTO_SCTP);
        if(mca_btl_sctp_component.sctp_listen_sd < 0) {
            if(opal_socket_errno != ESOCKTNOSUPPORT) {
                /* may have SCTP API but SCTP itself unable to auto-load into 
                 *  the kernel (like RHEL4U4), so avoid noisy error output
                 */                
                BTL_ERROR(("socket() failed with errno=%d", opal_socket_errno));
            }
            return OMPI_ERROR;
        }

        if((rc = mca_btl_sctp_set_socket_options(mca_btl_sctp_component.sctp_listen_sd)) != OMPI_SUCCESS) {
            return rc;
        }
        
        /* port set to zero to indicate "unset" in mca_btl_sctp_create */   
        mca_btl_sctp_component.sctp_listen_port = 0;    

        return OMPI_SUCCESS;
    }

}


static int mca_btl_sctp_component_register_listen(void)
{    
    /* setup listen backlog to maximum allowed by kernel */
    if(listen(mca_btl_sctp_component.sctp_listen_sd, SOMAXCONN) < 0) {
        BTL_ERROR(("listen() failed with errno=%d", opal_socket_errno));
        return OMPI_ERROR;
    }


    /* register listen port */
    opal_event_set(
            &mca_btl_sctp_component.sctp_recv_event,
            mca_btl_sctp_component.sctp_listen_sd, 
            OPAL_EV_READ|OPAL_EV_PERSIST, 
            mca_btl_sctp_recv_handler, 
            0);
    opal_event_add(&mca_btl_sctp_component.sctp_recv_event,0);
    return OMPI_SUCCESS;
}

/*
 *  Register SCTP module addressing information. The MCA framework
 *  will make this available to all peers. 
 */

static int mca_btl_sctp_component_exchange(void)
{
    int rc=0;
    size_t i=0;
    size_t size = mca_btl_sctp_component.sctp_num_btls * sizeof(mca_btl_sctp_addr_t);
    if(mca_btl_sctp_component.sctp_num_btls != 0) {
        mca_btl_sctp_addr_t *addrs = (mca_btl_sctp_addr_t *)malloc(size);
        for(i=0; i<mca_btl_sctp_component.sctp_num_btls; i++) {
            struct mca_btl_sctp_module_t* btl = mca_btl_sctp_component.sctp_btls[i];
            addrs[i].addr_inet    = btl->sctp_ifaddr.sin_addr;
            addrs[i].addr_port    = mca_btl_sctp_component.sctp_listen_port;
            addrs[i].addr_inuse   = 0;
        }
        rc =  ompi_modex_send(&mca_btl_sctp_component.super.btl_version, addrs, size);
        free(addrs);
    }
    return rc;
}

/*
 *  SCTP module initialization:
 *  (1) read interface list from kernel and compare against module parameters
 *      then create a BTL instance for selected interfaces
 *  (2) setup SCTP listen socket for incoming connection attempts
 *  (3) register BTL parameters with the MCA
 */
mca_btl_base_module_t** mca_btl_sctp_component_init(int *num_btl_modules, 
        bool enable_progress_threads,
        bool enable_mpi_threads)
{
    /* Currently refuse to run if MPI_THREAD_MULTIPLE is enabled */
    if (ompi_mpi_thread_multiple && !mca_btl_base_thread_multiple_override) {
        return NULL;
    } 

    if(mca_btl_sctp_component.sctp_if_11) {
        /* 1 to 1 */
        mca_btl_base_module_t **btls;
        *num_btl_modules = 0;

        /* initialize free lists */
        ompi_free_list_init( &mca_btl_sctp_component.sctp_frag_eager,
                sizeof (mca_btl_sctp_frag_eager_t) + 
                mca_btl_sctp_module.super.btl_eager_limit,
                OBJ_CLASS (mca_btl_sctp_frag_eager_t),
                mca_btl_sctp_component.sctp_free_list_num,
                mca_btl_sctp_component.sctp_free_list_max,
                mca_btl_sctp_component.sctp_free_list_inc,
                NULL );

        ompi_free_list_init( &mca_btl_sctp_component.sctp_frag_max,
                sizeof (mca_btl_sctp_frag_max_t) + 
                mca_btl_sctp_module.super.btl_max_send_size,
                OBJ_CLASS (mca_btl_sctp_frag_max_t),
                mca_btl_sctp_component.sctp_free_list_num,
                mca_btl_sctp_component.sctp_free_list_max,
                mca_btl_sctp_component.sctp_free_list_inc,
                NULL );

        ompi_free_list_init( &mca_btl_sctp_component.sctp_frag_user,
                sizeof (mca_btl_sctp_frag_user_t),
                OBJ_CLASS (mca_btl_sctp_frag_user_t),
                mca_btl_sctp_component.sctp_free_list_num,
                mca_btl_sctp_component.sctp_free_list_max,
                mca_btl_sctp_component.sctp_free_list_inc,
                NULL );

        /* create a BTL SCTP module for selected interfaces */
        if(mca_btl_sctp_component_create_instance() != OMPI_SUCCESS) {
            return 0;
        }

        /* create a SCTP listen socket for incoming connection attempts */
        if(mca_btl_sctp_component_create_listen() != OMPI_SUCCESS) {
            return 0;
        }

        /* publish SCTP parameters with the MCA framework */
        if(mca_btl_sctp_component_exchange() != OMPI_SUCCESS) {
            return 0;
        }

        btls = (mca_btl_base_module_t **)malloc(mca_btl_sctp_component.sctp_num_btls * 
                sizeof(mca_btl_base_module_t*));
        if(NULL == btls) {
            return NULL;
        }

        memcpy(btls, mca_btl_sctp_component.sctp_btls, mca_btl_sctp_component.sctp_num_btls*sizeof(mca_btl_sctp_module_t*));
        *num_btl_modules = mca_btl_sctp_component.sctp_num_btls;
        return btls;
    }

    else {
        /* 1 to many */
        int i;
        mca_btl_base_module_t **btls;
        *num_btl_modules = 0;

        /* initialize free lists */
        ompi_free_list_init( &mca_btl_sctp_component.sctp_frag_eager,
                sizeof (mca_btl_sctp_frag_eager_t) + 
                mca_btl_sctp_module.super.btl_eager_limit,
                OBJ_CLASS (mca_btl_sctp_frag_eager_t),
                mca_btl_sctp_component.sctp_free_list_num,
                mca_btl_sctp_component.sctp_free_list_max,
                mca_btl_sctp_component.sctp_free_list_inc,
                NULL );

        ompi_free_list_init( &mca_btl_sctp_component.sctp_frag_max,
                sizeof (mca_btl_sctp_frag_max_t) + 
                mca_btl_sctp_module.super.btl_max_send_size,
                OBJ_CLASS (mca_btl_sctp_frag_max_t),
                mca_btl_sctp_component.sctp_free_list_num,
                mca_btl_sctp_component.sctp_free_list_max,
                mca_btl_sctp_component.sctp_free_list_inc,
                NULL );

        ompi_free_list_init( &mca_btl_sctp_component.sctp_frag_user,
                sizeof (mca_btl_sctp_frag_user_t),
                OBJ_CLASS (mca_btl_sctp_frag_user_t),
                mca_btl_sctp_component.sctp_free_list_num,
                mca_btl_sctp_component.sctp_free_list_max,
                mca_btl_sctp_component.sctp_free_list_inc,
                NULL );


        /* create a SCTP listen socket for incoming connection attempts */
        if(mca_btl_sctp_component_create_listen() != OMPI_SUCCESS) {
            return 0;
        }

        /* create a BTL SCTP module for selected interfaces */
        if(mca_btl_sctp_component_create_instance() != OMPI_SUCCESS) {
            return 0;
        }

        /* register the now completed single BTL */
        if(mca_btl_sctp_component_register_listen() != OMPI_SUCCESS) {
            return 0;
        }

        /* publish SCTP parameters with the MCA framework */
        if(mca_btl_sctp_component_exchange() != OMPI_SUCCESS) {
            return 0;
        }

#if MCA_BTL_SCTP_DONT_USE_HASH
        /* Initialize the proc_tables to all negative ones. */
        recvr_proc_table = (mca_btl_sctp_proc_table_node *) malloc(sizeof(mca_btl_sctp_proc_table_node) * MCA_BTL_SCTP_PROC_TABLE_SIZE);
        sender_proc_table = (mca_btl_sctp_proc_table_node *) malloc(sizeof(mca_btl_sctp_proc_table_node) * MCA_BTL_SCTP_PROC_TABLE_SIZE);
        if(NULL == recvr_proc_table || NULL == sender_proc_table) {
            return 0;
        }
        for(i = 0; i < MCA_BTL_SCTP_PROC_TABLE_SIZE; i++) { 
            recvr_proc_table[i].valid = 0;
            recvr_proc_table[i].sctp_assoc_id = 0;
            recvr_proc_table[i].vpid = 0;
            recvr_proc_table[i].proc = NULL;
            sender_proc_table[i].valid = 0;
            sender_proc_table[i].sctp_assoc_id = 0;
            sender_proc_table[i].vpid = 0;
            sender_proc_table[i].proc = NULL;
        }
#endif

        btls = (mca_btl_base_module_t **)malloc(mca_btl_sctp_component.sctp_num_btls * 
                sizeof(mca_btl_base_module_t*));
        if(NULL == btls) {
            return NULL;
        }

        memcpy(btls, mca_btl_sctp_component.sctp_btls, mca_btl_sctp_component.sctp_num_btls*sizeof(mca_btl_sctp_module_t*));
        *num_btl_modules = mca_btl_sctp_component.sctp_num_btls;
        return btls;
    }
}

/*
 *  SCTP module control
 */

int mca_btl_sctp_component_control(int param, void* value, size_t size)
{
    return OMPI_SUCCESS;
}


/*
 *  Called by mca_btl_sctp_component_recv() when the SCTP listen
 *  socket has pending connection requests. Accept incoming
 *  requests and queue for completion of the connection handshake.
 */


void mca_btl_sctp_component_accept(void)
{
    if(mca_btl_sctp_component.sctp_if_11) {
        /* 1 to 1 */
        while(true) {
            opal_socklen_t addrlen = sizeof(struct sockaddr_in);
            struct sockaddr_in addr;
            mca_btl_sctp_event_t *event;
            int rc, sd = accept(mca_btl_sctp_component.sctp_listen_sd, (struct sockaddr*)&addr, &addrlen);
            if(sd < 0) {
                if(opal_socket_errno == EINTR) {
                    continue;
                }
                if(opal_socket_errno == ECONNRESET || opal_socket_errno == EBADF) {
                    /* closed remotely while on listen queue */
                    close(sd);
                }
                else if(opal_socket_errno != EAGAIN && opal_socket_errno != EWOULDBLOCK) {
                    BTL_ERROR(("accept() failed with errno %d.", opal_socket_errno));
                }
                return;
            }

            if((rc = mca_btl_sctp_set_socket_options(sd)) != OMPI_SUCCESS) {
                BTL_ERROR(("failed to set socket options"));
                return;
            }

            /* wait for receipt of peers process identifier to complete this connection */

            event = OBJ_NEW(mca_btl_sctp_event_t);
            opal_event_set(&event->event, sd, OPAL_EV_READ, mca_btl_sctp_component_recv_handler, event);
            opal_event_add(&event->event, 0);
        }
    }

    else {
        /*  1 to many */

        /* Called by mca_btl_sctp_recv_handler to get a valid *user pointer */

        mca_btl_sctp_event_t *event;
        int sd = mca_btl_sctp_component.sctp_listen_sd;

        if(sd < 0) {
            BTL_ERROR(("mca_btl_sctp_component_accept(): Invalid socket descriptor.\n"));
        }

        /* wait for receipt of peers process identifier to complete this connection */

        event = OBJ_NEW(mca_btl_sctp_event_t);
        opal_event_set(&event->event, sd, OPAL_EV_READ, mca_btl_sctp_recv_handler, event);
        opal_event_add(&event->event, 0);
    }
}




/* Used only with one-to-one socket.
 *
 * Event callback when there is data available on the registered 
 * socket to recv.
 */
static void mca_btl_sctp_component_recv_handler(int sd, short flags, void* user)
{
    orte_process_name_t guid;
    struct sockaddr_in addr;
    int retval;
    mca_btl_sctp_proc_t* btl_proc;
    opal_socklen_t addr_len = sizeof(addr);
    mca_btl_sctp_event_t *event = (mca_btl_sctp_event_t *)user;
    int msg_flags=0;
    struct sctp_sndrcvinfo sri;
    
    /* accept new connections on the listen socket */
    if(mca_btl_sctp_component.sctp_listen_sd == sd) {
        mca_btl_sctp_component_accept();
        return;
    }
    OBJ_RELEASE(event);

    retval = sctp_recvmsg(sd, (char *)&guid, sizeof(guid), 0, 0, &sri, &msg_flags);

    if(retval != sizeof(guid)) {
        CLOSE_THE_SOCKET(sd); 
        return; 
    }
    SCTP_BTL_ERROR(("mca_btl_sctp_component_recv_handler() sd=%d, got %d byte guid.\n", sd, retval));

    ORTE_PROCESS_NAME_NTOH(guid);
   
    /* lookup the corresponding process */
    btl_proc = mca_btl_sctp_proc_lookup(&guid);
    if(NULL == btl_proc) {
        BTL_ERROR(("errno=%d",errno));
        CLOSE_THE_SOCKET(sd);
        return;
    }

    /* lookup peer address */
    if(getpeername(sd, (struct sockaddr*)&addr, &addr_len) != 0) {
        if(opal_socket_errno != ECONNRESET && opal_socket_errno != EBADF && opal_socket_errno != ENOTCONN) {
            BTL_ERROR(("getpeername() failed with errno=%d", opal_socket_errno));
        }
        CLOSE_THE_SOCKET(sd);
        return;
    }

    /* are there any existing peer instances will to accept this connection */
    if(mca_btl_sctp_proc_accept(btl_proc, &addr, sd) == false) {
        CLOSE_THE_SOCKET(sd);
        return;
    }
}
