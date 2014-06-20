/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2010 Oracle and/or its affiliates.  All rights reserved
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif

#include "opal/class/opal_hash_table.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "ompi/runtime/ompi_module_exchange.h"
#include "opal/util/arch.h"
#include "opal/util/argv.h"
#include "opal/util/if.h"
#include "opal/util/net.h"

#include "btl_tcp.h"
#include "btl_tcp_proc.h"

static void mca_btl_tcp_proc_construct(mca_btl_tcp_proc_t* proc);
static void mca_btl_tcp_proc_destruct(mca_btl_tcp_proc_t* proc);

static mca_btl_tcp_interface_t** local_interfaces = NULL;
static int local_kindex_to_index[MAX_KERNEL_INTERFACE_INDEX];
static size_t num_local_interfaces, max_local_interfaces;
static mca_btl_tcp_interface_t** peer_interfaces = NULL;
static size_t num_peer_interfaces, max_peer_interfaces;
static int peer_kindex_to_index[MAX_KERNEL_INTERFACE_INDEX];
static unsigned int *best_assignment;
static int max_assignment_weight;
static int max_assignment_cardinality;
static enum mca_btl_tcp_connection_quality **weights;
static struct mca_btl_tcp_addr_t ***best_addr;

OBJ_CLASS_INSTANCE( mca_btl_tcp_proc_t, 
                    opal_list_item_t, 
                    mca_btl_tcp_proc_construct, 
                    mca_btl_tcp_proc_destruct );

void mca_btl_tcp_proc_construct(mca_btl_tcp_proc_t* tcp_proc)
{
    tcp_proc->proc_ompi = 0;
    tcp_proc->proc_addrs = NULL;
    tcp_proc->proc_addr_count = 0;
    tcp_proc->proc_endpoints = NULL;
    tcp_proc->proc_endpoint_count = 0;
    OBJ_CONSTRUCT(&tcp_proc->proc_lock, opal_mutex_t);
}

/*
 * Cleanup ib proc instance
 */

void mca_btl_tcp_proc_destruct(mca_btl_tcp_proc_t* tcp_proc)
{
    /* remove from list of all proc instances */
    OPAL_THREAD_LOCK(&mca_btl_tcp_component.tcp_lock);
    opal_hash_table_remove_value_uint64(&mca_btl_tcp_component.tcp_procs, 
                                        orte_util_hash_name(&tcp_proc->proc_ompi->proc_name));
    OPAL_THREAD_UNLOCK(&mca_btl_tcp_component.tcp_lock);

    /* release resources */
    if(NULL != tcp_proc->proc_endpoints) {
        free(tcp_proc->proc_endpoints);
    }
    OBJ_DESTRUCT(&tcp_proc->proc_lock);
}

/*
 * Create a TCP process structure. There is a one-to-one correspondence
 * between a ompi_proc_t and a mca_btl_tcp_proc_t instance. We cache
 * additional data (specifically the list of mca_btl_tcp_endpoint_t instances, 
 * and published addresses) associated w/ a given destination on this
 * datastructure.
 */

mca_btl_tcp_proc_t* mca_btl_tcp_proc_create(ompi_proc_t* ompi_proc)
{
    int rc;
    size_t size;
    mca_btl_tcp_proc_t* btl_proc;
    uint64_t hash = orte_util_hash_name(&ompi_proc->proc_name);

    OPAL_THREAD_LOCK(&mca_btl_tcp_component.tcp_lock);
    rc = opal_hash_table_get_value_uint64(&mca_btl_tcp_component.tcp_procs, 
                                          hash, (void**)&btl_proc);
    if(OMPI_SUCCESS == rc) {
        OPAL_THREAD_UNLOCK(&mca_btl_tcp_component.tcp_lock);
        return btl_proc;
    }

    btl_proc = OBJ_NEW(mca_btl_tcp_proc_t);
    if(NULL == btl_proc)
        return NULL;
    btl_proc->proc_ompi = ompi_proc;
    
    /* add to hash table of all proc instance */
    opal_hash_table_set_value_uint64(&mca_btl_tcp_component.tcp_procs,
                                     hash, btl_proc);
    OPAL_THREAD_UNLOCK(&mca_btl_tcp_component.tcp_lock);

    /* lookup tcp parameters exported by this proc */
    rc = ompi_modex_recv( &mca_btl_tcp_component.super.btl_version,
                                  ompi_proc,
                                  (void**)&btl_proc->proc_addrs,
                                  &size );
    if(rc != OMPI_SUCCESS) {
        BTL_ERROR(("mca_base_modex_recv: failed with return value=%d", rc));
        OBJ_RELEASE(btl_proc);
        return NULL;
    }
    if(0 != (size % sizeof(mca_btl_tcp_addr_t))) {
        BTL_ERROR(("mca_base_modex_recv: invalid size %lu: btl-size: %lu\n",
          (unsigned long) size, (unsigned long)sizeof(mca_btl_tcp_addr_t)));
        return NULL;
    }
    btl_proc->proc_addr_count = size / sizeof(mca_btl_tcp_addr_t);

    /* allocate space for endpoint array - one for each exported address */
    btl_proc->proc_endpoints = (mca_btl_base_endpoint_t**)
        malloc((1 + btl_proc->proc_addr_count) *
                sizeof(mca_btl_base_endpoint_t*));
    if(NULL == btl_proc->proc_endpoints) {
        OBJ_RELEASE(btl_proc);
        return NULL;
    }
    if(NULL == mca_btl_tcp_component.tcp_local && ompi_proc == ompi_proc_local()) {
        mca_btl_tcp_component.tcp_local = btl_proc;
    }
    {
        /* convert the OMPI addr_family field to OS constants,
         * so we can check for AF_INET (or AF_INET6) and don't have
         * to deal with byte ordering anymore.
         */
        unsigned int i;
        for (i = 0; i < btl_proc->proc_addr_count; i++) {
            if (MCA_BTL_TCP_AF_INET == btl_proc->proc_addrs[i].addr_family) {
                btl_proc->proc_addrs[i].addr_family = AF_INET;
            }
#if OPAL_WANT_IPV6
            if (MCA_BTL_TCP_AF_INET6 == btl_proc->proc_addrs[i].addr_family) {
                btl_proc->proc_addrs[i].addr_family = AF_INET6;
            }
#endif
        }
    }
    return btl_proc;
}



static void evaluate_assignment(int *a) {
    size_t i;
    unsigned int max_interfaces = num_local_interfaces;
    int assignment_weight = 0;
    int assignment_cardinality = 0;

    if(max_interfaces < num_peer_interfaces) {
        max_interfaces = num_peer_interfaces;
    }

    for(i = 0; i < max_interfaces; ++i) {
        if(0 < weights[i][a[i]-1]) {
            ++assignment_cardinality;
            assignment_weight += weights[i][a[i]-1];
        }
    }

    /*
     * check wether current solution beats all previous solutions
     */
    if(assignment_cardinality > max_assignment_cardinality
            || (assignment_cardinality == max_assignment_cardinality
                && assignment_weight > max_assignment_weight)) {

        for(i = 0; i < max_interfaces; ++i) {
             best_assignment[i] = a[i]-1;
        }
        max_assignment_weight = assignment_weight;
        max_assignment_cardinality = assignment_cardinality;
    }
}

static void visit(int k, int level, int siz, int *a)
{
    level = level+1; a[k] = level;

    if (level == siz) {
        evaluate_assignment(a);
    } else {
        int i;
        for ( i = 0; i < siz; i++)
            if (a[i] == 0)
                visit(i, level, siz, a);
    }

    level = level-1; a[k] = 0;
}


static void mca_btl_tcp_initialise_interface(mca_btl_tcp_interface_t* interface,
        int ifk_index, int index)
{
    interface->kernel_index = ifk_index;
    interface->peer_interface = -1;
    interface->ipv4_address = NULL;
    interface->ipv6_address =  NULL;
    interface->index = index;
    interface->inuse = 0;
}

static mca_btl_tcp_interface_t** mca_btl_tcp_retrieve_local_interfaces(void)
{
    struct sockaddr_storage local_addr;
    char local_if_name[IF_NAMESIZE];
    char **include, **exclude, **argv;
    int idx;

    if( NULL != local_interfaces )
        return local_interfaces;

    max_local_interfaces = MAX_KERNEL_INTERFACES;
    num_local_interfaces = 0;
    local_interfaces = (mca_btl_tcp_interface_t**)calloc( max_local_interfaces, sizeof(mca_btl_tcp_interface_t*) );
    if( NULL == local_interfaces )
        return NULL;

    memset(local_kindex_to_index, -1, sizeof(int)*MAX_KERNEL_INTERFACE_INDEX);

    /* Collect up the list of included and excluded interfaces, if any */
    include = opal_argv_split(mca_btl_tcp_component.tcp_if_include,',');
    exclude = opal_argv_split(mca_btl_tcp_component.tcp_if_exclude,',');

    /*
     * identify all kernel interfaces and the associated addresses of
     * the local node
     */
    for( idx = opal_ifbegin(); idx >= 0; idx = opal_ifnext (idx) ) {
        int kindex, index;
        bool skip = false;

        opal_ifindextoaddr (idx, (struct sockaddr*) &local_addr, sizeof (local_addr));
        opal_ifindextoname (idx, local_if_name, sizeof (local_if_name));

        /* If we were given a list of included interfaces, then check
         * to see if the current one is a member of this set.  If so,
         * drop down and complete processing.  If not, skip it and
         * continue on to the next one.  Note that providing an include
         * list will override providing an exclude list as the two are
         * mutually exclusive.  This matches how it works in
         * mca_btl_tcp_component_create_instances() which is the function
         * that exports the interfaces.  */
        if(NULL != include) {
            argv = include;
            skip = true;
            while(argv && *argv) {
                /* When comparing included interfaces, we look for exact matches.
                   That is why we are using strcmp() here. */
                if (0 == strcmp(*argv, local_if_name)) {
                    skip = false;
                    break;
                }
                argv++;
            }
        } else if (NULL != exclude) {
            /* If we were given a list of excluded interfaces, then check to see if the
             * current one is a member of this set.  If not, drop down and complete
             * processing.  If so, skip it and continue on to the next one. */
            argv = exclude;
            while(argv && *argv) {
                /* When looking for interfaces to exclude, we only look at
                 * the number of characters equal to what the user provided.
                 * For example, excluding "lo" excludes "lo", "lo0" and
                 * anything that starts with "lo" */
                if(0 == strncmp(*argv, local_if_name, strlen(*argv))) {
                    skip = true;
                    break;
                }
                argv++;
            }
        }
        if (true == skip) {
            /* This interface is not part of the requested set, so skip it */
            continue;
        }

        kindex = opal_ifindextokindex(idx);
        index = local_kindex_to_index[kindex];

        /* create entry for this kernel index previously not seen */
        if(-1 == index) {
            index = num_local_interfaces++;
            local_kindex_to_index[kindex] = index;

            if( num_local_interfaces == max_local_interfaces ) {
                max_local_interfaces <<= 1;
                local_interfaces = (mca_btl_tcp_interface_t**)realloc( local_interfaces,
                                                                       max_local_interfaces * sizeof(mca_btl_tcp_interface_t*) );
                if( NULL == local_interfaces )
                    return NULL;
            }
            local_interfaces[index] = (mca_btl_tcp_interface_t *) malloc(sizeof(mca_btl_tcp_interface_t));
            assert(NULL != local_interfaces[index]);
            mca_btl_tcp_initialise_interface(local_interfaces[index], kindex, index);
        }

        switch(local_addr.ss_family) {
        case AF_INET:
            /* if AF is disabled, skip it completely */
            if (4 == mca_btl_tcp_component.tcp_disable_family) {
                continue;
            }

            local_interfaces[local_kindex_to_index[kindex]]->ipv4_address = 
                (struct sockaddr_storage*) malloc(sizeof(local_addr));
            memcpy(local_interfaces[local_kindex_to_index[kindex]]->ipv4_address, 
                   &local_addr, sizeof(local_addr));
            opal_ifindextomask(idx, 
                               &local_interfaces[local_kindex_to_index[kindex]]->ipv4_netmask, 
                               sizeof(int));
            break;
        case AF_INET6:
            /* if AF is disabled, skip it completely */
            if (6 == mca_btl_tcp_component.tcp_disable_family) {
                continue;
            }

            local_interfaces[local_kindex_to_index[kindex]]->ipv6_address 
                = (struct sockaddr_storage*) malloc(sizeof(local_addr));
            memcpy(local_interfaces[local_kindex_to_index[kindex]]->ipv6_address, 
                   &local_addr, sizeof(local_addr));
            opal_ifindextomask(idx, 
                               &local_interfaces[local_kindex_to_index[kindex]]->ipv6_netmask, 
                               sizeof(int));
            break;
        default:
            opal_output(0, "unknown address family for tcp: %d\n",
                        local_addr.ss_family);
        }
    }
    opal_argv_free(include);
    opal_argv_free(exclude);

    return local_interfaces;
}
/*
 * Note that this routine must be called with the lock on the process
 * already held.  Insert a btl instance into the proc array and assign 
 * it an address.
 */
int mca_btl_tcp_proc_insert( mca_btl_tcp_proc_t* btl_proc, 
                             mca_btl_base_endpoint_t* btl_endpoint )
{
    struct sockaddr_storage endpoint_addr_ss;
    unsigned int perm_size;
    int rc, *a = NULL;
    size_t i, j;

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

    /* sanity checks */
    if( NULL == local_interfaces ) {
        if( NULL == mca_btl_tcp_retrieve_local_interfaces() )
            return OMPI_ERR_OUT_OF_RESOURCE;
    }
    if( 0 == num_local_interfaces ) {
        return OMPI_ERR_UNREACH;
    }

    if( NULL == peer_interfaces ) {
        max_peer_interfaces = max_local_interfaces;
        peer_interfaces = (mca_btl_tcp_interface_t**)malloc( max_peer_interfaces * sizeof(mca_btl_tcp_interface_t*) );
    }
    num_peer_interfaces = 0;
    memset(peer_kindex_to_index, -1, sizeof(int)*MAX_KERNEL_INTERFACE_INDEX);
    memset(peer_interfaces, 0, max_peer_interfaces * sizeof(mca_btl_tcp_interface_t*));

    /*
     * identify all kernel interfaces and the associated addresses of
     * the peer
     */

    for( i = 0; i < btl_proc->proc_addr_count; i++ ) {

        int index;

        mca_btl_tcp_addr_t* endpoint_addr = btl_proc->proc_addrs + i;

        mca_btl_tcp_proc_tosocks (endpoint_addr, &endpoint_addr_ss);

        index = peer_kindex_to_index[endpoint_addr->addr_ifkindex];

        if(-1 == index) {
            index = num_peer_interfaces++;
            peer_kindex_to_index[endpoint_addr->addr_ifkindex] = index;
            if( num_peer_interfaces == max_peer_interfaces ) {
                max_peer_interfaces <<= 1;
                peer_interfaces = (mca_btl_tcp_interface_t**)realloc( peer_interfaces,
                                                                      max_peer_interfaces * sizeof(mca_btl_tcp_interface_t*) );
                if( NULL == peer_interfaces )
                    return OMPI_ERR_OUT_OF_RESOURCE;
            }
            peer_interfaces[index] = (mca_btl_tcp_interface_t *) malloc(sizeof(mca_btl_tcp_interface_t));
            mca_btl_tcp_initialise_interface(peer_interfaces[index], 
                                             endpoint_addr->addr_ifkindex, index);
        }       
        
        /*
         * in case one of the peer addresses is already in use,
         * mark the complete peer interface as 'not available'
         */
        if(endpoint_addr->addr_inuse) {
            peer_interfaces[index]->inuse = 1;
        }

        switch(endpoint_addr_ss.ss_family) {
        case AF_INET:
            peer_interfaces[index]->ipv4_address = (struct sockaddr_storage*) malloc(sizeof(endpoint_addr_ss));
            peer_interfaces[index]->ipv4_endpoint_addr = endpoint_addr;
            memcpy(peer_interfaces[index]->ipv4_address, 
                   &endpoint_addr_ss, sizeof(endpoint_addr_ss));
            break;
        case AF_INET6:
            peer_interfaces[index]->ipv6_address = (struct sockaddr_storage*) malloc(sizeof(endpoint_addr_ss));
            peer_interfaces[index]->ipv6_endpoint_addr = endpoint_addr;
            memcpy(peer_interfaces[index]->ipv6_address, 
                   &endpoint_addr_ss, sizeof(endpoint_addr_ss));
            break;
        default:
            opal_output(0, "unknown address family for tcp: %d\n",
                        endpoint_addr_ss.ss_family);
            /*
             * return OMPI_UNREACH or some error, as this is not
             * good
             */
        }
    }

    /*
     * assign weights to each possible pair of interfaces    
     */

    perm_size = num_local_interfaces;
    if(num_peer_interfaces > perm_size) {
        perm_size = num_peer_interfaces;
    }

    weights = (enum mca_btl_tcp_connection_quality**) malloc(perm_size
                                                             * sizeof(enum mca_btl_tcp_connection_quality*));
    
    best_addr = (mca_btl_tcp_addr_t ***) malloc(perm_size
                                                * sizeof(mca_btl_tcp_addr_t **));
    for(i = 0; i < perm_size; ++i) {
        weights[i] = (enum mca_btl_tcp_connection_quality*) malloc(perm_size *
                                                                   sizeof(enum mca_btl_tcp_connection_quality));
        memset(weights[i], 0, perm_size * sizeof(enum mca_btl_tcp_connection_quality));

        best_addr[i] = (mca_btl_tcp_addr_t **) malloc(perm_size *
                                                      sizeof(mca_btl_tcp_addr_t *));
        memset(best_addr[i], 0, perm_size * sizeof(mca_btl_tcp_addr_t *));
    }
    

    for(i=0; i<num_local_interfaces; ++i) {
        for(j=0; j<num_peer_interfaces; ++j) {

            /*  initially, assume no connection is possible */
            weights[i][j] = CQ_NO_CONNECTION;

            /* check state of ipv4 address pair */
            if(NULL != local_interfaces[i]->ipv4_address &&
               NULL != peer_interfaces[j]->ipv4_address) {

                /*  check for RFC1918 */
                if(opal_net_addr_isipv4public((struct sockaddr*) local_interfaces[i]->ipv4_address)
                   && opal_net_addr_isipv4public((struct sockaddr*) 
                                                 peer_interfaces[j]->ipv4_address)) {
                    if(opal_net_samenetwork((struct sockaddr*) local_interfaces[i]->ipv4_address,
                                            (struct sockaddr*) peer_interfaces[j]->ipv4_address,
                                            local_interfaces[i]->ipv4_netmask)) {
                        weights[i][j] = CQ_PUBLIC_SAME_NETWORK;
                    } else {
                        weights[i][j] = CQ_PUBLIC_DIFFERENT_NETWORK;
                    }
                    best_addr[i][j] = peer_interfaces[j]->ipv4_endpoint_addr;
                    continue;
                } else {
                    if(opal_net_samenetwork((struct sockaddr*) local_interfaces[i]->ipv4_address,
                                            (struct sockaddr*) peer_interfaces[j]->ipv4_address,
                                            local_interfaces[i]->ipv4_netmask)) {
                        weights[i][j] = CQ_PRIVATE_SAME_NETWORK;
                    } else {
                        weights[i][j] = CQ_PRIVATE_DIFFERENT_NETWORK;
                    }
                    best_addr[i][j] = peer_interfaces[j]->ipv4_endpoint_addr;
                }
            }

            /* check state of ipv6 address pair - ipv6 is always public,
             * since link-local addresses are skipped in opal_ifinit()
             */
            if(NULL != local_interfaces[i]->ipv6_address &&
               NULL != peer_interfaces[j]->ipv6_address) {
                if(opal_net_samenetwork((struct sockaddr*) local_interfaces[i]->ipv6_address,
                                        (struct sockaddr*) peer_interfaces[j]->ipv6_address,
                                        local_interfaces[i]->ipv6_netmask)) {
                    weights[i][j] = CQ_PUBLIC_SAME_NETWORK;
                } else {
                    weights[i][j] = CQ_PUBLIC_DIFFERENT_NETWORK;
                }
                best_addr[i][j] = peer_interfaces[j]->ipv6_endpoint_addr;
            } 

        } /* for each peer interface */
    } /* for each local interface */

    /*
     * determine the size of the set to permute (max number of
     * interfaces
     */

    best_assignment = (unsigned int *) malloc (perm_size * sizeof(int));

    a = (int *) malloc(perm_size * sizeof(int));
    if (NULL == a) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Can only find the best set of connections when the number of
     * interfaces is not too big.  When it gets larger, we fall back
     * to a simpler and faster (and not as optimal) algorithm. */
    if (perm_size <= MAX_PERMUTATION_INTERFACES) {
       memset(a, 0, perm_size * sizeof(int));
       max_assignment_cardinality = -1;
       max_assignment_weight = -1;
       visit(0, -1, perm_size, a);

       rc = OMPI_ERR_UNREACH;
       for(i = 0; i < perm_size; ++i) {
	   if(best_assignment[i] > num_peer_interfaces
	      || weights[i][best_assignment[i]] == CQ_NO_CONNECTION
	      || peer_interfaces[best_assignment[i]]->inuse 
	      || NULL == peer_interfaces[best_assignment[i]]) {
	       continue;
	   } 
	   peer_interfaces[best_assignment[i]]->inuse++;
	   btl_endpoint->endpoint_addr = best_addr[i][best_assignment[i]];
	   btl_endpoint->endpoint_addr->addr_inuse++;
	   rc = OMPI_SUCCESS;
	   break;
       }
    } else {
	enum mca_btl_tcp_connection_quality max;
	int i_max = 0, j_max = 0;
	/* Find the best connection that is not in use.  Save away
	 * the indices of the best location. */
	max = CQ_NO_CONNECTION;
	for(i=0; i<num_local_interfaces; ++i) {
	    for(j=0; j<num_peer_interfaces; ++j) {
		if (!peer_interfaces[j]->inuse) {
		    if (weights[i][j] > max) {
			max = weights[i][j];
			i_max = i;
			j_max = j;
		    }
		}
	    }
	}
	/* Now see if there is a some type of connection available. */
	rc = OMPI_ERR_UNREACH;
	if (CQ_NO_CONNECTION != max) {
	    peer_interfaces[j_max]->inuse++;
	    btl_endpoint->endpoint_addr = best_addr[i_max][j_max];
	    btl_endpoint->endpoint_addr->addr_inuse++;
	    rc = OMPI_SUCCESS;
	}
    }

    for(i = 0; i < perm_size; ++i) {
        free(weights[i]);
        free(best_addr[i]);
    }

    for(i = 0; i < num_peer_interfaces; ++i) {
        if(NULL != peer_interfaces[i]->ipv4_address) {
            free(peer_interfaces[i]->ipv4_address);
        }
        if(NULL != peer_interfaces[i]->ipv6_address) {
            free(peer_interfaces[i]->ipv6_address);
        }
        free(peer_interfaces[i]);
    }
    free(peer_interfaces);
    peer_interfaces = NULL;
    max_peer_interfaces = 0;

    for(i = 0; i < num_local_interfaces; ++i) {
        if(NULL != local_interfaces[i]->ipv4_address) {
            free(local_interfaces[i]->ipv4_address);
        }
        if(NULL != local_interfaces[i]->ipv6_address) {
            free(local_interfaces[i]->ipv6_address);
        }
        free(local_interfaces[i]);
    }
    free(local_interfaces);
    local_interfaces = NULL;
    max_local_interfaces = 0;

    free(weights);
    free(best_addr);
    free(best_assignment);
    free(a);

    return rc;
}

/*
 * Remove an endpoint from the proc array and indicate the address is
 * no longer in use.
 */
                                                                                                                 
int mca_btl_tcp_proc_remove(mca_btl_tcp_proc_t* btl_proc, mca_btl_base_endpoint_t* btl_endpoint)
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
 * Look for an existing TCP process instance based on the globally unique
 * process identifier.
 */
mca_btl_tcp_proc_t* mca_btl_tcp_proc_lookup(const orte_process_name_t *name)
{
    mca_btl_tcp_proc_t* proc = NULL;
    OPAL_THREAD_LOCK(&mca_btl_tcp_component.tcp_lock);
    opal_hash_table_get_value_uint64(&mca_btl_tcp_component.tcp_procs, 
                                     orte_util_hash_name(name), (void**)&proc);
    OPAL_THREAD_UNLOCK(&mca_btl_tcp_component.tcp_lock);
    return proc;
}

/*
 * loop through all available BTLs for one matching the source address
 * of the request.
 */
bool mca_btl_tcp_proc_accept(mca_btl_tcp_proc_t* btl_proc, struct sockaddr* addr, int sd)
{
    size_t i;
    OPAL_THREAD_LOCK(&btl_proc->proc_lock);
    for( i = 0; i < btl_proc->proc_endpoint_count; i++ ) {
        mca_btl_base_endpoint_t* btl_endpoint = btl_proc->proc_endpoints[i];
        /* Check all conditions before going to try to accept the connection. */
        if( btl_endpoint->endpoint_addr->addr_family != addr->sa_family ) {
            continue;
        }

        switch (addr->sa_family) {
        case AF_INET:
            if( memcmp( &btl_endpoint->endpoint_addr->addr_inet,
                        &(((struct sockaddr_in*)addr)->sin_addr),
                        sizeof(struct in_addr) ) ) {
                continue;
            }
            break;
#if OPAL_WANT_IPV6
        case AF_INET6:
            if( memcmp( &btl_endpoint->endpoint_addr->addr_inet,
                        &(((struct sockaddr_in6*)addr)->sin6_addr),
                        sizeof(struct in6_addr) ) ) {
                continue;
            }
            break;
#endif
        default:
            ;
        }

        if(mca_btl_tcp_endpoint_accept(btl_endpoint, addr, sd)) {
            OPAL_THREAD_UNLOCK(&btl_proc->proc_lock);
            return true;
        }
    }
    OPAL_THREAD_UNLOCK(&btl_proc->proc_lock);
    return false;
}

/*
 * convert internal data structure (mca_btl_tcp_addr_t) to sockaddr_storage
 *
 */
bool mca_btl_tcp_proc_tosocks(mca_btl_tcp_addr_t* proc_addr,
                              struct sockaddr_storage* output)
{
    memset(output, 0, sizeof (*output));
    switch (proc_addr->addr_family) {
    case AF_INET:
        output->ss_family = AF_INET;
        memcpy(&((struct sockaddr_in*)output)->sin_addr,
               &proc_addr->addr_inet, sizeof(struct in_addr));
        ((struct sockaddr_in*)output)->sin_port = proc_addr->addr_port;
        break;
#if OPAL_WANT_IPV6
    case AF_INET6:
        {
            struct sockaddr_in6* inaddr = (struct sockaddr_in6*)output;
            output->ss_family = AF_INET6;
            memcpy(&inaddr->sin6_addr, &proc_addr->addr_inet,
                   sizeof (proc_addr->addr_inet));
            inaddr->sin6_port = proc_addr->addr_port;
            inaddr->sin6_scope_id = 0;
            inaddr->sin6_flowinfo = 0;
        }
        break;
#endif
    default:
        opal_output( 0, "mca_btl_tcp_proc: unknown af_family received: %d\n",
                     proc_addr->addr_family );
        return false;
    } 
    return true;
}

