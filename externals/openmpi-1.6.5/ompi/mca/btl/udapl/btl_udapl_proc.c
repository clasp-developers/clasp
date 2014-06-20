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
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/runtime/ompi_module_exchange.h"
#include "opal/util/net.h"
#include "btl_udapl.h"
#include "btl_udapl_endpoint.h"
#include "btl_udapl_proc.h"

static void mca_btl_udapl_proc_construct(mca_btl_udapl_proc_t* proc);
static void mca_btl_udapl_proc_destruct(mca_btl_udapl_proc_t* proc);

OBJ_CLASS_INSTANCE(mca_btl_udapl_proc_t, 
        opal_list_item_t, mca_btl_udapl_proc_construct, 
        mca_btl_udapl_proc_destruct);

void mca_btl_udapl_proc_construct(mca_btl_udapl_proc_t* udapl_proc)
{
    udapl_proc->proc_ompi = 0;
    udapl_proc->proc_addr_count = 0;
    udapl_proc->proc_endpoints = 0;
    udapl_proc->proc_endpoint_count = 0;
    OBJ_CONSTRUCT(&udapl_proc->proc_lock, opal_mutex_t);

    /* add to list of all proc instance */
    OPAL_THREAD_LOCK(&mca_btl_udapl_component.udapl_lock);
    opal_list_append(&mca_btl_udapl_component.udapl_procs, &udapl_proc->super);
    OPAL_THREAD_UNLOCK(&mca_btl_udapl_component.udapl_lock);
}


/*
 * Cleanup uDAPL proc instance
 */

void mca_btl_udapl_proc_destruct(mca_btl_udapl_proc_t* udapl_proc)
{
    /* remove from list of all proc instances */
    OPAL_THREAD_LOCK(&mca_btl_udapl_component.udapl_lock);
    opal_list_remove_item(&mca_btl_udapl_component.udapl_procs, &udapl_proc->super);
    OPAL_THREAD_UNLOCK(&mca_btl_udapl_component.udapl_lock);

    /* release resources */
    if(NULL != udapl_proc->proc_endpoints) {
        free(udapl_proc->proc_endpoints);
    }
    OBJ_DESTRUCT(&udapl_proc->proc_lock);
}


/*
 * Look for an existing uDAPL process instances based on the associated
 * ompi_proc_t instance.
 */
static mca_btl_udapl_proc_t* mca_btl_udapl_proc_lookup_ompi(ompi_proc_t* ompi_proc)
{
    mca_btl_udapl_proc_t* udapl_proc;

    OPAL_THREAD_LOCK(&mca_btl_udapl_component.udapl_lock);

    for(udapl_proc = (mca_btl_udapl_proc_t*)
            opal_list_get_first(&mca_btl_udapl_component.udapl_procs);
            udapl_proc != (mca_btl_udapl_proc_t*)
            opal_list_get_end(&mca_btl_udapl_component.udapl_procs);
            udapl_proc  = (mca_btl_udapl_proc_t*)opal_list_get_next(udapl_proc)) {

        if(udapl_proc->proc_ompi == ompi_proc) {
            OPAL_THREAD_UNLOCK(&mca_btl_udapl_component.udapl_lock);
            return udapl_proc;
        }

    }

    OPAL_THREAD_UNLOCK(&mca_btl_udapl_component.udapl_lock);

    return NULL;
}

/*
 * Create a uDAPL process structure. There is a one-to-one correspondence
 * between a ompi_proc_t and a mca_btl_udapl_proc_t instance. We cache
 * additional data (specifically the list of mca_btl_udapl_endpoint_t instances, 
 * and published addresses) associated w/ a given destination on this
 * datastructure.
 */

mca_btl_udapl_proc_t* mca_btl_udapl_proc_create(ompi_proc_t* ompi_proc)
{
    mca_btl_udapl_proc_t* udapl_proc = NULL;
    size_t size;
    int rc;

    /* Check if we have already created a uDAPL proc
     * structure for this ompi process */
    udapl_proc = mca_btl_udapl_proc_lookup_ompi(ompi_proc);
    if(udapl_proc != NULL) {
        return udapl_proc;
    }

    /* create a new udapl proc out of the ompi_proc ... */
    udapl_proc = OBJ_NEW(mca_btl_udapl_proc_t);
    udapl_proc->proc_endpoint_count = 0;
    udapl_proc->proc_ompi = ompi_proc;
    udapl_proc->proc_guid = ompi_proc->proc_name;

    /* query for the peer address info */
    rc = ompi_modex_recv(
                 &mca_btl_udapl_component.super.btl_version,
                 ompi_proc,
                 (void*)&udapl_proc->proc_addrs,
                 &size); 
    if(OMPI_SUCCESS != rc) {
        BTL_UDAPL_VERBOSE_OUTPUT(VERBOSE_CRITICAL,
            ("ompi_modex_recv failed for peer %s",
            ORTE_NAME_PRINT(&ompi_proc->proc_name)));
        OBJ_RELEASE(udapl_proc);
        return NULL;
    }

    if((size % sizeof(mca_btl_udapl_addr_t)) != 0) {
        BTL_UDAPL_VERBOSE_OUTPUT(VERBOSE_CRITICAL,
            ("invalid udapl address for peer %s",
            ORTE_NAME_PRINT(&ompi_proc->proc_name)));
        OBJ_RELEASE(udapl_proc);
        return NULL;
    }

    udapl_proc->proc_addr_count = size/sizeof(mca_btl_udapl_addr_t);
    if (0 == udapl_proc->proc_addr_count) {
        udapl_proc->proc_endpoints = NULL;
    } else {
        udapl_proc->proc_endpoints = (mca_btl_base_endpoint_t**)
            malloc(udapl_proc->proc_addr_count * sizeof(mca_btl_base_endpoint_t*));
    }
    if(NULL == udapl_proc->proc_endpoints) {
        OBJ_RELEASE(udapl_proc);
        return NULL;
    }
    return udapl_proc;
}


/*
 * Find an address on the peer_process which matches stated criteria
 * to the udapl btl module address information. Return in peer_addr_idx
 * the index to the peer_process address that matches the btl module
 * address. Where match criteria is:
 * - the address in not already in use
 * - compare addresses using netmask, the netmask value can be modified with
 *   "--mca btl_udapl_if_mask"
 *
 * Note: since this is called from mca_btl_udapl_proc_insert() it
 * is assumed that the process lock is locked when entered.
 *
 * @param udapl_btl (IN)        BTL module
 * @param peer_process (IN)     BTL peer process
 * @param peer_addr_idx(IN/OUT) Index of address on peer_process
 *                              which matches the udapl_btl address data.
 *                              On success should be  >= 0.
 * @return                      OMPI_SUCCESS or error status on failure
 */
static int mca_btl_udapl_proc_address_match(
    mca_btl_udapl_module_t* udapl_btl,
    mca_btl_udapl_proc_t* peer_proc,
    int* peer_addr_idx)
{
    int i;
    struct sockaddr *saddr;
    struct sockaddr_in *btl_addr;
    struct sockaddr_in *peer_addr;
    char btl_addr_string[INET_ADDRSTRLEN]; 
    char peer_addr_string[INET_ADDRSTRLEN];

    *peer_addr_idx = MCA_BTL_UDAPL_INVALID_PEER_ADDR_IDX;

    /* use generic address to find address family */
    saddr = (struct sockaddr *)&(udapl_btl->udapl_addr.addr);

    if (saddr->sa_family == AF_INET) {

        btl_addr = (struct sockaddr_in *)saddr;

        /* Loop thru peer process addresses looking for match.
         * Match criteria:
         * - address should not be "inuse"
         * - both udapl btl module and peer address should be on
         *   the same subnet (compare with if_mask value)
         */
        for(i = 0; i < (int) peer_proc->proc_addr_count; i++) {

            peer_addr =
                (struct sockaddr_in *)&(peer_proc->proc_addrs[i].addr);

            if (VERBOSE_INFORM <=
                mca_btl_udapl_component.udapl_verbosity) {

                /*  retrieve udapl btl and peer address string for reporting */
                inet_ntop(AF_INET, (void *) &btl_addr->sin_addr,
                    btl_addr_string, INET_ADDRSTRLEN);
                inet_ntop(AF_INET, (void *) &peer_addr->sin_addr,
                    peer_addr_string, INET_ADDRSTRLEN);
            }

            if ((false == peer_proc->proc_addrs[i].inuse) &&
                (opal_net_samenetwork((struct sockaddr *)btl_addr,
                    (struct sockaddr *)peer_addr, udapl_btl->udapl_if_mask))) {

                /* capture index of remote address where match found */
                *peer_addr_idx = i;

                /* mark this address as now being used */
                peer_proc->proc_addrs[i].inuse = true;

                /* report what address was found to match */
                BTL_UDAPL_VERBOSE_OUTPUT(VERBOSE_INFORM,
                    ("uDAPL BTL module(%s) matched %s",
                    btl_addr_string, peer_addr_string));
                break;
            } else {
                /* peer address already used by another udapl btl
                 * module or netmask check not successful so skip
                 */
                BTL_UDAPL_VERBOSE_OUTPUT(VERBOSE_INFORM,
                    ("uDAPL BTL module(%s) either skipped because it "
                    "is already in use or match criteria not successful "
                    "for peer address %s",
                    btl_addr_string, peer_addr_string));
            }
        }

    } else {
        /* current uDAPL BTL only supports IPv4 */
        BTL_UDAPL_VERBOSE_HELP(VERBOSE_SHOW_HELP,
            ("help-mpi-btl-udapl.txt", "IPv4 only",
            true, orte_process_info.nodename));
        return OMPI_ERROR;
    }

    if (MCA_BTL_UDAPL_INVALID_PEER_ADDR_IDX == *peer_addr_idx) {
        BTL_UDAPL_VERBOSE_HELP(VERBOSE_SHOW_HELP,
            ("help-mpi-btl-udapl.txt", "no network match",
            true, btl_addr_string, orte_process_info.nodename,
            peer_proc->proc_ompi->proc_hostname));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }    

    return OMPI_SUCCESS;
}

    
/*
 * Note that this routine must be called with the lock on the process
 * already held.  Insert a btl instance into the proc array and assign 
 * it an address.
 */
int mca_btl_udapl_proc_insert(
    mca_btl_udapl_proc_t* udapl_proc, 
    mca_btl_base_endpoint_t* udapl_endpoint)
{
    int peer_address_idx;
    mca_btl_udapl_module_t* udapl_btl = udapl_endpoint->endpoint_btl;

    /* Check so as not to create more endpoints than addresses.
     * Example: If one node has 3 btl modules and another only has 2,
     * this check prevents the node with 3 btl modules from
     * overloading the other, i.e. only 2 possible connections will
     * be possible.
     */
    if (udapl_proc->proc_endpoint_count > udapl_proc->proc_addr_count)
        return OMPI_ERR_OUT_OF_RESOURCE;

    /* Find an endpoint on the udapl process of interest that matches
     * the endpoint information of the current udapl btl module
     */
    if (OMPI_SUCCESS !=
        mca_btl_udapl_proc_address_match(udapl_btl, udapl_proc,
            &peer_address_idx)) { 
        /* no address on peer proc met criteria */
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* insert into endpoint array */
    udapl_endpoint->endpoint_proc = udapl_proc;
    udapl_endpoint->endpoint_addr =
        udapl_proc->proc_addrs[peer_address_idx];
   
    udapl_proc->proc_endpoints[udapl_proc->proc_endpoint_count] = udapl_endpoint;
    udapl_proc->proc_endpoint_count++;
    return OMPI_SUCCESS;
}

