/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "opal/class/opal_hash_table.h"
#include "opal/util/arch.h"
#include "opal/include/opal_stdint.h"

#include "ompi/mca/btl/base/btl_base_error.h"
#include "ompi/runtime/ompi_module_exchange.h"

#include "btl_sctp.h"
#include "btl_sctp_proc.h"

static void mca_btl_sctp_proc_construct(mca_btl_sctp_proc_t* proc);
static void mca_btl_sctp_proc_destruct(mca_btl_sctp_proc_t* proc);
static bool is_private_ipv4(struct in_addr *in);


OBJ_CLASS_INSTANCE(
        mca_btl_sctp_proc_t, 
        opal_list_item_t, 
        mca_btl_sctp_proc_construct, 
        mca_btl_sctp_proc_destruct);


void mca_btl_sctp_proc_construct(mca_btl_sctp_proc_t* stcp_proc)
{
    stcp_proc->proc_ompi = 0;
    stcp_proc->proc_addrs = NULL;
    stcp_proc->proc_addr_count = 0;
    stcp_proc->proc_endpoints = NULL;
    stcp_proc->proc_endpoint_count = 0;
    OBJ_CONSTRUCT(&stcp_proc->proc_lock, opal_mutex_t);
}

/*
 * Cleanup ib proc instance
 */

void mca_btl_sctp_proc_destruct(mca_btl_sctp_proc_t* stcp_proc)
{
    /* remove from list of all proc instances */
    OPAL_THREAD_LOCK(&mca_btl_sctp_component.sctp_lock);
    opal_hash_table_remove_value_uint64(&mca_btl_sctp_component.sctp_procs, 
                                        orte_util_hash_name(&stcp_proc->proc_name));
    OPAL_THREAD_UNLOCK(&mca_btl_sctp_component.sctp_lock);

    /* release resources */
    if(NULL != stcp_proc->proc_endpoints) {
        free(stcp_proc->proc_endpoints);
    }
    OBJ_DESTRUCT(&stcp_proc->proc_lock);
}


/*
 * Check to see if an IPv4 struct in_addr is public or private.  We
 * can only do IPv4 here because some of the SCTP BTL endpoint structs
 * only hold the struct in_addr, not the upper-level sin_family that
 * would indicate if the address is IPv6.
 */
static bool is_private_ipv4(struct in_addr *in)
{
    /* There are definitely ways to do this more efficiently, but
       since this is not performance-critical code, it seems better to
       use clear code (vs. clever code) */

    uint32_t addr = ntohl((uint32_t) in->s_addr);
    unsigned int a = (addr & 0xff000000) >> 24;
    unsigned int b = (addr & 0x00ff0000) >> 16;

    return ((10 == a) ||
            (192 == a && 168 == b) ||
            (172 == a && 16 == b)) ? true : false;
}


/*
 * Create a SCTP process structure. There is a one-to-one correspondence
 * between a ompi_proc_t and a mca_btl_sctp_proc_t instance. We cache
 * additional data (specifically the list of mca_btl_sctp_endpoint_t instances, 
 * and published addresses) associated w/ a given destination on this
 * datastructure.
 */

mca_btl_sctp_proc_t* mca_btl_sctp_proc_create(ompi_proc_t* ompi_proc)
{
    int rc;
    size_t size;
    mca_btl_sctp_proc_t* btl_proc;
    uint64_t hash = orte_util_hash_name(&ompi_proc->proc_name);

    OPAL_THREAD_LOCK(&mca_btl_sctp_component.sctp_lock);
    rc = opal_hash_table_get_value_uint64(&mca_btl_sctp_component.sctp_procs, 
                                          hash, (void**)&btl_proc);
    if(OMPI_SUCCESS == rc) {
        OPAL_THREAD_UNLOCK(&mca_btl_sctp_component.sctp_lock);
        return btl_proc;
    }

    btl_proc = OBJ_NEW(mca_btl_sctp_proc_t);
    if(NULL == btl_proc) {
        return NULL;
    }
    btl_proc->proc_ompi = ompi_proc;
    btl_proc->proc_name = ompi_proc->proc_name;

    /* add to hash table of all proc instance */
    opal_hash_table_set_value_uint64(&mca_btl_sctp_component.sctp_procs,
                                     hash, btl_proc);
    OPAL_THREAD_UNLOCK(&mca_btl_sctp_component.sctp_lock);

    /* lookup sctp parameters exported by this proc */
    rc = ompi_modex_recv( &mca_btl_sctp_component.super.btl_version,
            ompi_proc,
            (void**)&btl_proc->proc_addrs,
            &size );
    if(rc != OMPI_SUCCESS) {
        BTL_ERROR(("mca_base_modex_recv: failed with return value=%d", rc));
        OBJ_RELEASE(btl_proc);
        return NULL;
    }
    if(0 != (size % sizeof(mca_btl_sctp_addr_t))) {
        BTL_ERROR(("mca_base_modex_recv: invalid size %" PRIsize_t "\n", size));
        return NULL;
    }
    btl_proc->proc_addr_count = size / sizeof(mca_btl_sctp_addr_t);

    /* allocate space for endpoint array - one for each exported address */
    btl_proc->proc_endpoints = (mca_btl_base_endpoint_t**)
        malloc(btl_proc->proc_addr_count * sizeof(mca_btl_base_endpoint_t*));
    if(NULL == btl_proc->proc_endpoints) {
        OBJ_RELEASE(btl_proc);
        return NULL;
    }
    if(NULL == mca_btl_sctp_component.sctp_local && ompi_proc == ompi_proc_local()) {
        mca_btl_sctp_component.sctp_local = btl_proc;
    }
    return btl_proc;
}


/*
 * Note that this routine must be called with the lock on the process
 * already held.  Insert a btl instance into the proc array and assign 
 * it an address.
 */
int mca_btl_sctp_proc_insert(
        mca_btl_sctp_proc_t* btl_proc, 
        mca_btl_base_endpoint_t* btl_endpoint)
{
    struct mca_btl_sctp_module_t *btl_sctp = btl_endpoint->endpoint_btl;
    size_t i;
    unsigned long net1;

#ifndef WORDS_BIGENDIAN
    /* if we are little endian and our peer is not so lucky, then we
       need to put all information sent to him in big endian (aka
       Network Byte Order) and expect all information received to
       be in NBO.  Since big endian machines always send and receive
       in NBO, we don't care so much about that case. */
    if (btl_proc->proc_ompi->proc_arch & OPAL_ARCH_ISBIGENDIAN) {
        btl_endpoint->endpoint_nbo = true;
    }
#endif

    /* insert into endpoint array */
    btl_endpoint->endpoint_proc = btl_proc;
    btl_proc->proc_endpoints[btl_proc->proc_endpoint_count++] = btl_endpoint;

    net1 = btl_sctp->sctp_ifaddr.sin_addr.s_addr & btl_sctp->sctp_ifmask.sin_addr.s_addr;

    /*
     * Look through the proc instance for an address that is on the
     * directly attached network. If we don't find one, pick the first
     * unused address.
     */
    for(i=0; i<btl_proc->proc_addr_count; i++) {
        mca_btl_sctp_addr_t* endpoint_addr = btl_proc->proc_addrs + i;
        unsigned long net2 = endpoint_addr->addr_inet.s_addr & btl_sctp->sctp_ifmask.sin_addr.s_addr;
        if(endpoint_addr->addr_inuse != 0) {
            continue;
        }
        /* in the 1-to-many code, we sctp_bindx all addresses to one
         *  association, and only represent this multi-NIC endpoint
         *  internally as one BTL. this is within mca_btl_sctp_create.
         *  this is done so that SCTP itself handles multi-path scenarios
         *  since the transport has multihoming.  as a result, we simply
         *  have to pick one of the addresses in the association (it doesn't
         *  matter to the API which is chosen); we pick the one which bind
         *  is called (NOT sctp_bindx) even if it is a private IP...
         */
        if(net1 == net2 || 0 == mca_btl_sctp_component.sctp_if_11) {
            btl_endpoint->endpoint_addr = endpoint_addr;
            break;
        } else if(btl_endpoint->endpoint_addr != 0) {
            btl_endpoint->endpoint_addr = endpoint_addr;
        }
    }

    /* Make sure there is a common interface */
    if( NULL != btl_endpoint->endpoint_addr ) {
        btl_endpoint->endpoint_addr->addr_inuse++;
        return OMPI_SUCCESS;
    }

    /* There was no common interface.  So what do we do?  For the
       moment, we'll do enough to cover 2 common cases:

       1. Running MPI processes on two computers that are not on the
       same subnet, but still have routable addresses to each
       other.  In this case, the above subnet matching will fail,
       but since the addresses are routable, the
       OS/networking/routers will make it all work ok.  So we need
       to make this function *not* return OMPI_ERR_UNREACH.

       2. Running MPI processes on a typical cluster configuration
       where a head node has 2 SCTP NICs (one public IP address one
       private IP address) and all the back-end compute nodes have
       only private IP addresses.  In this scenario, the MPI
       process on the head node will have 2 SCTP BTL modules (one
       for the public, one for the private).  The module with the
       private IP address will match the subnet and all will work
       fine.  The module with the public IP address will not match
       anything and fall through to here -- we want it to return
       OMPI_ERR_UNREACH so that that module will effectively have
       no peers that it can communicate with.

       To support these two scenarios, do the following:

       - if my address is private (10., 192.168., or 172.16.), return
       UNREACH.
       - if my address is public, return the first public address from
       my peer (and hope for the best), or UNREACH if there are none
       available.

       This does not cover some other scenarios that we'll likely need
       to support in the future, such as:

       - Flat neighborhood networks -- where all the IP's in question
       are private, the subnet masking won't necessarily match, but
       they're routable to each other.
       - Really large, private SCTP-based clusters, such as a 1024 node
       SCTP-based cluster.  Depending on how the subnet masks are set
       by the admins, there may be a subnet mask that effectively
       spans the entire cluster, or (for example) subnet masks may
       be set such that only nodes on the same switches are on the
       same subnet.  This latter scenario will not be supported
       by the above cases.

       To support these kinds of scenarios, we really need "something
       better", such as allowing the user to specify a config file
       indicating which subnets are reachable by which interface, etc.
       */

    else {
        /* If my address is private, return UNREACH */
        if (is_private_ipv4(&(btl_sctp->sctp_ifaddr.sin_addr))) {
            return OMPI_ERR_UNREACH;
        }

        /* Find the first public peer address */
        for (i = 0; i < btl_proc->proc_addr_count; ++i) {
            mca_btl_sctp_addr_t* endpoint_addr = btl_proc->proc_addrs + i;
            if (!is_private_ipv4(&(endpoint_addr->addr_inet))) {
                btl_endpoint->endpoint_addr = endpoint_addr;
                btl_endpoint->endpoint_addr->addr_inuse++;
                return OMPI_SUCCESS;
            }
        }

        /* Didn't find any peer addresses that were public, so return
           UNREACH */
        return OMPI_ERR_UNREACH;
    }
}

/*
 * Remove an endpoint from the proc array and indicate the address is
 * no longer in use.
 */

int mca_btl_sctp_proc_remove(mca_btl_sctp_proc_t* btl_proc, mca_btl_base_endpoint_t* btl_endpoint)
{
    size_t i;
    OPAL_THREAD_LOCK(&btl_proc->proc_lock);
    for(i=0; i<btl_proc->proc_endpoint_count; i++) {
        if(btl_proc->proc_endpoints[i] == btl_endpoint) {
            memmove(btl_proc->proc_endpoints+i, btl_proc->proc_endpoints+i+1,
                    (btl_proc->proc_endpoint_count-i-1)*sizeof(mca_btl_base_endpoint_t*));
            if(--btl_proc->proc_endpoint_count == 0) {
                OPAL_THREAD_UNLOCK(&btl_proc->proc_lock);
                OBJ_RELEASE(btl_proc);
                return OMPI_SUCCESS;
            }
            /* The endpoint_addr may still be NULL if this enpoint is
               being removed early in the wireup sequence (e.g., if it
               is unreachable by all other procs) */
            if (NULL != btl_endpoint->endpoint_addr) {
                btl_endpoint->endpoint_addr->addr_inuse--;
            }
            break;
        }
    }
    OPAL_THREAD_UNLOCK(&btl_proc->proc_lock);
    return OMPI_SUCCESS;
}

/*
 * Look for an existing SCTP process instance based on the globally unique
 * process identifier.
 */
mca_btl_sctp_proc_t* mca_btl_sctp_proc_lookup(const orte_process_name_t *name)
{
    mca_btl_sctp_proc_t* proc = NULL;
    OPAL_THREAD_LOCK(&mca_btl_sctp_component.sctp_lock);
    opal_hash_table_get_value_uint64(&mca_btl_sctp_component.sctp_procs,
                                     orte_util_hash_name(name), (void**)&proc);
    OPAL_THREAD_UNLOCK(&mca_btl_sctp_component.sctp_lock);
    return proc;
}

/*
 * loop through all available PTLs for one matching the source address
 * of the request.
 */
bool mca_btl_sctp_proc_accept(mca_btl_sctp_proc_t* btl_proc, struct sockaddr_in* addr, int sd)
{
    size_t i;
    OPAL_THREAD_LOCK(&btl_proc->proc_lock);
    for(i=0; i<btl_proc->proc_endpoint_count; i++) {
        mca_btl_base_endpoint_t* btl_endpoint = btl_proc->proc_endpoints[i];
        if(mca_btl_sctp_endpoint_accept(btl_endpoint, addr, sd)) {
            OPAL_THREAD_UNLOCK(&btl_proc->proc_lock);
            return true;
        }
    }
    OPAL_THREAD_UNLOCK(&btl_proc->proc_lock);
    return false;
}



/**
 * int mca_btl_sctp_proc_check
 * ----------------------------
 *  This function simply consults a table of procs and checks if the table at
 *  position 'id' is valid or not.
 *
 *  TODO - change this to use a hash for constant time performance
 */
static int mca_btl_sctp_proc_check(int is_vpid, sctp_assoc_t id, orte_vpid_t vpid, struct mca_btl_sctp_proc_table_node *table) {
#if MCA_BTL_SCTP_DONT_USE_HASH
    int i;
    for(i = 0; i < MCA_BTL_SCTP_PROC_TABLE_SIZE; i++) {
        /*  sender_proc_table uses orte_vpid_t.
         *  recvr_proc_table uses sctp_assoc_id.
         *  Calls using this function use one or the other.
         */
        if((0 == is_vpid && table[i].valid && table[i].sctp_assoc_id == id) ||
           (1 == is_vpid && table[i].valid && table[i].vpid == vpid))
        {
            return VALID_ENTRY;
        } else if(table[i].valid == 0) {
            /* once invalid is found, can return INVALID_ENTRY (added incrementally) */
	    break;
        }
    }
    return INVALID_ENTRY;
#else
    mca_btl_sctp_proc_t *val;
    /* TODO fix if sctp_assoc_t is 64 bit (once we change to hash) */
    int rc = opal_hash_table_get_value_uint32(&mca_btl_sctp_component.sctp_assocID_hash, id, &val);
    if(OPAL_SUCCESS == rc) {
        return VALID_ENTRY;
    } else {
        return INVALID_ENTRY;
    }
#endif
}

int mca_btl_sctp_proc_check_vpid(orte_vpid_t vpid, struct mca_btl_sctp_proc_table_node *table) {
    return mca_btl_sctp_proc_check(1, 0, vpid, table);
}

int mca_btl_sctp_proc_check_assoc_id(sctp_assoc_t id, struct mca_btl_sctp_proc_table_node *table) {
    return mca_btl_sctp_proc_check(0, id, 0, table);
}



/**
 * void mca_btl_sctp_proc_add
 * ---------------------------
 *  Add a proc entry to the table indexed by association id.
 *  
 *  TODO change this to a hash table that can expand to eliminate
 *    MCA_BTL_SCTP_PROC_TABLE_SIZE limitation
 */
static void mca_btl_sctp_proc_add(sctp_assoc_t id, orte_vpid_t vpid, struct mca_btl_sctp_proc_t *proc, struct mca_btl_sctp_proc_table_node *table) {
#if MCA_BTL_SCTP_DONT_USE_HASH
    int i;
    for(i = 0; i < MCA_BTL_SCTP_PROC_TABLE_SIZE; i++) {
        if(table[i].sctp_assoc_id == 0 && table[i].vpid == 0 && table[i].valid == 0) {
            table[i].sctp_assoc_id = id;
            table[i].vpid = vpid;
            table[i].proc = proc;
            table[i].valid = 1;
            return;
        }
    }
#else
    /* TODO fix if sctp_assoc_t is 64 bit (once we change to hash) */
    int rc = opal_hash_table_set_value_uint32(&mca_btl_sctp_component.sctp_assocID_hash, id, proc);
    /* TODO handle return code */
#endif    
}

void mca_btl_sctp_proc_add_vpid(orte_vpid_t vpid, struct mca_btl_sctp_proc_t *proc, struct mca_btl_sctp_proc_table_node *table) {
    mca_btl_sctp_proc_add(0, vpid, proc, table);
}

void mca_btl_sctp_proc_add_assoc_id(sctp_assoc_t id, struct mca_btl_sctp_proc_t *proc, struct mca_btl_sctp_proc_table_node *table) {
    mca_btl_sctp_proc_add(id, 0, proc, table);
}


/**
 * mca_btl_sctp_proc_t* mca_btl_sctp_proc_get
 * ------------------------------------------
 *  Returns pointer to a proc that is indexed by the association id.
 */
mca_btl_sctp_proc_t *mca_btl_sctp_proc_get(sctp_assoc_t id, struct mca_btl_sctp_proc_table_node *table) {
#if MCA_BTL_SCTP_DONT_USE_HASH
    int i;
    for(i = 0; i < MCA_BTL_SCTP_PROC_TABLE_SIZE; i++){
        if(table[i].sctp_assoc_id == id) {
            return table[i].proc;
        }
    }
    return NULL;
#else
    mca_btl_sctp_proc_t *val;
    /* TODO fix if sctp_assoc_t is 64 bit (once we change to hash) */
    int rc = opal_hash_table_get_value_uint32(&mca_btl_sctp_component.sctp_assocID_hash, id, &val);
    if(OPAL_SUCCESS == rc) {
        return val;
    } else {
        return NULL;
    }
#endif    
}
