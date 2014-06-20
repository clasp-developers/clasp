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
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

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
#include <string.h>

#include "opal/util/if.h"
#include "opal/util/net.h"


#include "oob_tcp.h"
#include "oob_tcp_addr.h"


static void mca_oob_tcp_addr_construct(mca_oob_tcp_addr_t* addr)
{
    memset(&addr->addr_name, 0, sizeof(addr->addr_name));
    addr->addr_count = 0;
    addr->addr_alloc = 0;
    addr->addr_next = 0;
    addr->addr_inet = NULL;
    addr->addr_matched = MCA_OOB_TCP_ADDR_UNCLASSIFIED;
}

static void mca_oob_tcp_addr_destruct(mca_oob_tcp_addr_t* addr)
{
    if(addr->addr_inet != NULL)
        free(addr->addr_inet);
}

OBJ_CLASS_INSTANCE(
    mca_oob_tcp_addr_t,
    opal_object_t,
    mca_oob_tcp_addr_construct,
    mca_oob_tcp_addr_destruct);


int mca_oob_tcp_addr_get_next(mca_oob_tcp_addr_t* addr, struct sockaddr* retval)
{
    static uint32_t i_have = MCA_OOB_TCP_ADDR_UNCLASSIFIED; /* my own capabilities */
    
    if((NULL == addr) || (0 == addr->addr_count)) {
        return ORTE_ERROR;
    }
    
    if(MCA_OOB_TCP_ADDR_UNCLASSIFIED == addr->addr_matched) {
        orte_std_cntr_t i=0;
        for(i=addr->addr_next; i<addr->addr_count; i++) {
            opal_list_item_t *item;
            for (item = opal_list_get_first(&mca_oob_tcp_component.tcp_available_devices) ;
                 item != opal_list_get_end(&mca_oob_tcp_component.tcp_available_devices) ;
                 item = opal_list_get_next(item)) {
                mca_oob_tcp_device_t *dev = (mca_oob_tcp_device_t*) item;
                uint32_t inmask;

                opal_ifindextomask(dev->if_index, &inmask, sizeof(inmask));

                /* Decide which address to try first; note that we're
                    called multiple times and each time we need to
                    present a different address
                    
                    Precedence rules:
                    
                    - IPv4public has the highest priority
                    - when IPv4private + IPv6, use IPv6 (this should
                      be changed when there is something like a CellID)
                    */
                if (true == opal_net_addr_isipv4public ((struct sockaddr*) &dev->if_addr)) {
                    i_have |= MCA_OOB_TCP_ADDR_IPV4public;
                }
                
                if (true == opal_net_addr_isipv4public ((struct sockaddr*)&addr->addr_inet[i])) {
                    addr->addr_matched |= MCA_OOB_TCP_ADDR_IPV4public;
                }
                
                if ((MCA_OOB_TCP_ADDR_IPV4public ==
                     (i_have & MCA_OOB_TCP_ADDR_IPV4public)) &&
                    (MCA_OOB_TCP_ADDR_IPV4public ==
                     (addr->addr_matched & MCA_OOB_TCP_ADDR_IPV4public))) {
                    addr->addr_next = i;
                    goto done;
                }
                
                if (AF_INET6 == dev->if_addr.ss_family) {
                    i_have |= MCA_OOB_TCP_ADDR_IPV6;
                }
                
                if (AF_INET6 ==
                    ((struct sockaddr_in6*)&addr->addr_inet[i])->sin6_family) {
                    addr->addr_matched |= MCA_OOB_TCP_ADDR_IPV6;
                    addr->addr_next = i;
                    goto done;
                }
                
                /* if match on network prefix - start here */
                /* Bug, FIXME: This code is dangerous, it will prefer
                    local addresses even if they point to wrong hosts
                    (the multicluster problem).
                    
                    We need more magic to select the best address
                    
                    adi@2006-09-30
                */
                if(opal_net_samenetwork((struct sockaddr*) &dev->if_addr,
                                        (struct sockaddr*)&addr->addr_inet[i],
                                        inmask)) {
                    addr->addr_matched |= MCA_OOB_TCP_ADDR_MATCHED;
                    addr->addr_next = i;
                    goto done;
                }
            }
        }
done:
        ; /* NOP */
    }

    if (addr->addr_inet[addr->addr_next].ss_family == AF_INET) {
        memcpy(retval, &addr->addr_inet[addr->addr_next],
               sizeof(struct sockaddr_in));
    } else {
        memcpy(retval, &addr->addr_inet[addr->addr_next],
               sizeof(struct sockaddr_in6));
    }

    if(++addr->addr_next >= addr->addr_count)
        addr->addr_next = 0;
    return ORTE_SUCCESS;
}


int
mca_oob_tcp_addr_insert(mca_oob_tcp_addr_t* addr, const struct sockaddr* inaddr)
{
    if(addr->addr_alloc == 0) {
        addr->addr_alloc = 2;
        addr->addr_inet = (struct sockaddr_storage*) malloc(addr->addr_alloc * sizeof(struct sockaddr_storage));
    } else if(addr->addr_count == addr->addr_alloc) {
        addr->addr_alloc <<= 1;
        addr->addr_inet = (struct sockaddr_storage*) realloc(addr->addr_inet, addr->addr_alloc * sizeof(struct sockaddr_storage));
    }
    if(NULL == addr->addr_inet) return ORTE_ERR_OUT_OF_RESOURCE;

    if (inaddr->sa_family == AF_INET) {
        memcpy(addr->addr_inet+addr->addr_count, inaddr, sizeof(struct sockaddr_in));
    } else {
        memcpy(addr->addr_inet+addr->addr_count, inaddr, sizeof(struct sockaddr_in6));
    }
    addr->addr_count++;
    return ORTE_SUCCESS;
}
