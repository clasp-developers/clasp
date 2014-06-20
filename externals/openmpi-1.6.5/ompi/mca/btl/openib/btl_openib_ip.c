/*
 * Copyright (c) 2008      Chelsio, Inc. All rights reserved.
 * Copyright (c) 2008-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * @file
 */

#include "ompi_config.h"

#include <infiniband/verbs.h>

#if OMPI_HAVE_RDMACM
#include <rdma/rdma_cma.h>
#include <stdlib.h>
#include <stdio.h>

#include "opal/util/argv.h"
#include "opal/util/if.h"

#include "orte/util/show_help.h"

#include "connect/connect.h"
#endif
/* Always want to include this file */
#include "btl_openib_endpoint.h"
#include "btl_openib_ip.h"
#if OMPI_HAVE_RDMACM

/* 
 * The cruft below maintains the linked list of rdma ipv4 addresses and their
 * associated rdma device names and device port numbers.  
 */
struct rdma_addr_list {
    opal_list_item_t      super;
    uint32_t              addr;
    uint32_t              subnet;
    char                  addr_str[16];
    char                  dev_name[IBV_SYSFS_NAME_MAX];
    uint8_t               dev_port;
};
typedef struct rdma_addr_list rdma_addr_list_t;

static OBJ_CLASS_INSTANCE(rdma_addr_list_t, opal_list_item_t, 
                          NULL, NULL);
static opal_list_t *myaddrs = NULL;

#if OPAL_ENABLE_DEBUG
static char *stringify(uint32_t addr)
{
    static char line[64];
    memset(line, 0, sizeof(line));
    snprintf(line, sizeof(line) - 1, "%d.%d.%d.%d (0x%x)", 
#if defined(WORDS_BIGENDIAN)
             (addr >> 24),
             (addr >> 16) & 0xff,
             (addr >> 8) & 0xff,
             addr & 0xff,
#else
             addr & 0xff,
             (addr >> 8) & 0xff,
             (addr >> 16) & 0xff,
             (addr >> 24),
#endif
             addr);
    return line;
}
#endif

/* Note that each device port can have multiple IP addresses
 * associated with it (aka IP aliasing).  However, the openib module
 * only knows about (device,port) tuples -- not IP addresses (only the
 * RDMA CM CPC knows which IP addresses are associated with each
 * (device,port) tuple).  Thus, any searching of device list for the
 * IP Address or subnets may not work as one might expect.  The
 * current behavior is to return the IP address (or subnet) of the
 * *first* instance of the device on the list.  This behavior is
 * uniform for subnet and IP addresses and thus should not cause any
 * mismatches.  If this behavior is not preferred by the user, the MCA
 * parameters to include/exclude specific IP addresses can be used to
 * precisely specify which addresses are used (e.g., to effect
 * specific subnet routing).
 */
uint64_t mca_btl_openib_get_ip_subnet_id(struct ibv_device *ib_dev,
                                         uint8_t port)
{
    opal_list_item_t *item;

    /* In the off chance that the user forces a non-RDMACM CPC and an
     * IP-based mechanism, the list will be uninitialized.  Return 0
     * to prevent crashes, and the lack of it actually working will be
     * caught at a later stage.
     */
    if (NULL == myaddrs) {
        return 0;
    }

    for (item = opal_list_get_first(myaddrs);
         item != opal_list_get_end(myaddrs);
         item = opal_list_get_next(item)) {
        struct rdma_addr_list *addr = (struct rdma_addr_list *)item;
        if (!strcmp(addr->dev_name, ib_dev->name) &&
            port == addr->dev_port) {
            return addr->subnet;
        }
    }

    return 0;
}

/* This function should not be necessary, as rdma_get_local_addr would
 * be more correct in returning the IP address given the cm_id (and
 * not necessitate having to do a list look up).  Unfortunately, the
 * subnet and IP address look up needs to match or there could be a
 * mismatch if IP Aliases are being used.  For more information on
 * this, please read comment above mca_btl_openib_get_ip_subnet_id.
 */
uint32_t mca_btl_openib_rdma_get_ipv4addr(struct ibv_context *verbs, 
                                          uint8_t port)
{
    opal_list_item_t *item;

    /* Sanity check */
    if (NULL == myaddrs) {
        return 0;
    }

    BTL_VERBOSE(("Looking for %s:%d in IP address list",
                 ibv_get_device_name(verbs->device), port));
    for (item = opal_list_get_first(myaddrs);
         item != opal_list_get_end(myaddrs);
         item = opal_list_get_next(item)) {
        struct rdma_addr_list *addr = (struct rdma_addr_list *)item;
        if (!strcmp(addr->dev_name, verbs->device->name) && 
            port == addr->dev_port) {
            BTL_VERBOSE(("FOUND: %s:%d is %s",
                         ibv_get_device_name(verbs->device), port,
                         stringify(addr->addr)));
            return addr->addr;
        }
    }
    return 0;
}

static int dev_specified(char *name, int port)
{
    char **list;

    if (NULL != mca_btl_openib_component.if_include) {
        int i;

        list = opal_argv_split(mca_btl_openib_component.if_include, ',');
        for (i = 0; NULL != list[i]; i++) {
            char **temp = opal_argv_split(list[i], ':');
            if (0 == strcmp(name, temp[0]) &&
                (NULL == temp[1] || port == atoi(temp[1]))) {
                return 0;
            }
        }

        return 1;
    }

    if (NULL != mca_btl_openib_component.if_exclude) {
        int i;

        list = opal_argv_split(mca_btl_openib_component.if_exclude, ',');
        for (i = 0; NULL != list[i]; i++) {
            char **temp = opal_argv_split(list[i], ':');
            if (0 == strcmp(name, temp[0]) &&
                (NULL == temp[1] || port == atoi(temp[1]))) {
                return 1;
            }
        }
    }

    return 0;
}

static int ipaddr_specified(struct sockaddr_in *ipaddr, uint32_t netmask)
{
    uint32_t all = ~((uint32_t) 0);

    if (NULL != mca_btl_openib_component.ipaddr_include) {
        char **list;
        int i;

        list = opal_argv_split(mca_btl_openib_component.ipaddr_include, ',');
        for (i = 0; NULL != list[i]; i++) {
            uint32_t subnet, list_subnet;
            struct in_addr ipae;
            char **temp = opal_argv_split(list[i], '/');

            if (NULL == temp || NULL == temp[0] || NULL == temp[1] ||
                NULL != temp[2]) {
                orte_show_help("help-mpi-btl-openib.txt",
                               "invalid ipaddr_inexclude", true, "include",
                               orte_process_info.nodename, list[i],
                               "Invalid specification (missing \"/\")");
                if (NULL != temp) {
                    opal_argv_free(temp);
                }
                continue;
            }

            if (1 != inet_pton(ipaddr->sin_family, temp[0], &ipae)) {
                orte_show_help("help-mpi-btl-openib.txt",
                               "invalid ipaddr_inexclude", true, "include",
                               orte_process_info.nodename, list[i],
                               "Invalid specification (inet_pton() failed)");
                opal_argv_free(temp);
                continue;
            }
            list_subnet = ntohl(ipae.s_addr) & ~(all >> atoi(temp[1]));
            subnet = ntohl(ipaddr->sin_addr.s_addr) & ~(all >> netmask);
            opal_argv_free(temp);

            if (subnet == list_subnet) { 
                return 0;
            }
        }

        return 1;
    }

    if (NULL != mca_btl_openib_component.ipaddr_exclude) {
        char **list;
        int i;

        list = opal_argv_split(mca_btl_openib_component.ipaddr_exclude, ',');
        for (i = 0; NULL != list[i]; i++) {
            uint32_t subnet, list_subnet;
            struct in_addr ipae;
            char **temp = opal_argv_split(list[i], '/');

            if (NULL == temp || NULL == temp[0] || NULL == temp[1] ||
                NULL != temp[2]) {
                orte_show_help("help-mpi-btl-openib.txt",
                               "invalid ipaddr_inexclude", true, "exclude",
                               orte_process_info.nodename, list[i],
                               "Invalid specification (missing \"/\")");
                if (NULL != temp) {
                    opal_argv_free(temp);
                }
                continue;
            }

            if (1 != inet_pton(ipaddr->sin_family, temp[0], &ipae)) {
                orte_show_help("help-mpi-btl-openib.txt",
                               "invalid ipaddr_inexclude", true, "exclude",
                               orte_process_info.nodename, list[i],
                               "Invalid specification (inet_pton() failed)");
                opal_argv_free(temp);
                continue;
            }
            list_subnet = ntohl(ipae.s_addr) & ~(all >> atoi(temp[1]));
            subnet = ntohl(ipaddr->sin_addr.s_addr) & ~(all >> netmask);
            opal_argv_free(temp);

            if (subnet == list_subnet) { 
                return 1;
            }
        }
    }

    return 0;
}

static int add_rdma_addr(struct sockaddr *ipaddr, uint32_t netmask)
{
    struct sockaddr_in *sinp;
    struct rdma_cm_id *cm_id;
    struct rdma_event_channel *ch;
    int rc = OMPI_SUCCESS;
    struct rdma_addr_list *myaddr;
    uint32_t all = ~((uint32_t) 0);

    /* Ensure that this IP address is not in 127.0.0.1/8.  If it is,
       skip it because we never want loopback addresses to be
       considered RDMA devices that remote peers can use to connect
       to.  

       This check is necessary because of a change that almost went
       into RDMA CM in OFED 1.5.1.  We asked for a delay so that we
       could get a release of Open MPI out that includes the
       127-ignoring logic; hence, this change will likely be in a
       future version of OFED (perhaps OFED 1.6?).

       OMPI uses rdma_bind_addr() to determine if a local IP address
       is an RDMA device or not.  If it succeeds and we get a non-NULL
       verbs pointer back in the return, we say that it's a valid RDMA
       device.  Up through OFED 1.5, rdma_bind_addr(127.0.0.1), would
       succeed, but the verbs pointer returned would be NULL.  Hence,
       we knew it was loopback, and therefore we skipped it.

       The proposed RDMA CM change would return a non-NULL/valid verbs
       pointer when binding to 127.0.0.1/8.  This, of course, screws
       up OMPI because we then advertise 127.0.0.1 in the modex as an
       address that remote peers can use to contact this process via
       RDMA.  Hence, we have to specifically exclude 127.0.0.1/8 --
       don't even both trying to rdma_bind_addr() to it because we
       know we don't want loopback addresses at all. */
    sinp = (struct sockaddr_in *)ipaddr;
    if ((sinp->sin_addr.s_addr & htonl(0xff000000)) == htonl(0x7f000000)) {
        rc = OMPI_SUCCESS;
        goto out1;
    }

    ch = rdma_create_event_channel();
    if (NULL == ch) {
        BTL_VERBOSE(("failed creating RDMA CM event channel"));
        rc = OMPI_ERROR;
        goto out1;
    }

    rc = rdma_create_id(ch, &cm_id, NULL, RDMA_PS_TCP);
    if (rc) {
        BTL_VERBOSE(("rdma_create_id returned %d", rc));
        rc = OMPI_ERROR;
        goto out2;
    }

    /* Bind the newly created cm_id to the IP address.  This will,
       amongst other things, verify that the device is verbs
       capable */
    rc = rdma_bind_addr(cm_id, ipaddr);
    if (rc || !cm_id->verbs) {
        rc = OMPI_SUCCESS;
        goto out3;
    }

    /* Verify that the device has not been excluded */
    rc = dev_specified(cm_id->verbs->device->name, cm_id->port_num);
    if (rc) {
        rc = OMPI_SUCCESS;
        goto out3;
    }

    /* Verify that the device has a valid IP address */
    if (0 == ((struct sockaddr_in *)ipaddr)->sin_addr.s_addr ||
	ipaddr_specified((struct sockaddr_in *)ipaddr, netmask)) {
        rc = OMPI_SUCCESS;
        goto out3;
    }

    myaddr = OBJ_NEW(rdma_addr_list_t);
    if (NULL == myaddr) {
        BTL_ERROR(("malloc failed!"));
        rc = OMPI_ERROR;
        goto out3;
    }

    myaddr->addr = sinp->sin_addr.s_addr;
    myaddr->subnet = ntohl(myaddr->addr) & ~(all >> netmask);
    inet_ntop(sinp->sin_family, &sinp->sin_addr, 
              myaddr->addr_str, sizeof(myaddr->addr_str));
    memcpy(myaddr->dev_name, cm_id->verbs->device->name, IBV_SYSFS_NAME_MAX);
    myaddr->dev_port = cm_id->port_num;
    BTL_VERBOSE(("Adding addr %s (0x%x) subnet 0x%x as %s:%d", 
                 myaddr->addr_str, myaddr->addr, myaddr->subnet,
                 myaddr->dev_name, myaddr->dev_port));

    opal_list_append(myaddrs, &(myaddr->super));

out3:
    rdma_destroy_id(cm_id);
out2:
    rdma_destroy_event_channel(ch);
out1:
    return rc;
}

int mca_btl_openib_build_rdma_addr_list(void)
{
    int rc = OMPI_SUCCESS, i;

    myaddrs = OBJ_NEW(opal_list_t);
    if (NULL == myaddrs) {
        BTL_ERROR(("malloc failed!"));
        return OMPI_ERROR;
    }

    for (i = opal_ifbegin(); i >= 0; i = opal_ifnext(i)) {
        struct sockaddr ipaddr;
        uint32_t netmask;

        opal_ifindextoaddr(i, &ipaddr, sizeof(struct sockaddr));
        opal_ifindextomask(i, &netmask, sizeof(uint32_t));

        if (ipaddr.sa_family == AF_INET) {
            rc = add_rdma_addr(&ipaddr, netmask);
            if (OMPI_SUCCESS != rc) {
                break;
            }
        }
    }
    return rc;
}
  
void mca_btl_openib_free_rdma_addr_list(void)
{
    opal_list_item_t *item, *next;

    if (NULL != myaddrs && 0 != opal_list_get_size(myaddrs)) {
        for (item = opal_list_get_first(myaddrs);
             item != opal_list_get_end(myaddrs);
             item = next) {
            struct rdma_addr_list *addr = (struct rdma_addr_list *)item;
            next = opal_list_get_next(item);
            opal_list_remove_item(myaddrs, item);
            OBJ_RELEASE(addr);
        }
       OBJ_RELEASE(myaddrs);
       myaddrs = NULL;
    }
}

#else 
/* !OMPI_HAVE_RDMACM case */

uint64_t mca_btl_openib_get_ip_subnet_id(struct ibv_device *ib_dev,
                                            uint8_t port) 
{
    return 0;
}

uint32_t mca_btl_openib_rdma_get_ipv4addr(struct ibv_context *verbs, 
                                          uint8_t port) 
{
    return 0;
}

int mca_btl_openib_build_rdma_addr_list(void) 
{
    return OMPI_SUCCESS;
}

void mca_btl_openib_free_rdma_addr_list(void) 
{
}
#endif
