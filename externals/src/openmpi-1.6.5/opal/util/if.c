/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_SOCKIO_H
#include <sys/sockio.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_NET_IF_H
#if defined(__APPLE__) && defined(_LP64)
/* Apple engineering suggested using options align=power as a
   workaround for a bug in OS X 10.4 (Tiger) that prevented ioctl(...,
   SIOCGIFCONF, ...) from working properly in 64 bit mode on Power PC.
   It turns out that the underlying issue is the size of struct
   ifconf, which the kernel expects to be 12 and natural 64 bit
   alignment would make 16.  The same bug appears in 64 bit mode on
   Intel macs, but align=power is a no-op there, so instead, use the
   pack pragma to instruct the compiler to pack on 4 byte words, which
   has the same effect as align=power for our needs and works on both
   Intel and Power PC Macs. */
#pragma pack(push,4)
#endif
#include <net/if.h>
#if defined(__APPLE__) && defined(_LP64)
#pragma pack(pop)
#endif
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_IFADDRS_H
#include <ifaddrs.h>
#endif

#include "opal/class/opal_list.h"
#include "opal/util/if.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/constants.h"
#include "opal/mca/base/mca_base_param.h"

#ifdef HAVE_STRUCT_SOCKADDR_IN

#ifndef MIN
#  define MIN(a,b)                ((a) < (b) ? (a) : (b))
#endif
        
/*
 * Define INADDR_NONE if we don't have it.  Solaris is the only system
 * where I have found that it does not exist, and the man page for
 * inet_addr() says that it returns -1 upon failure.  On Linux and
 * other systems with INADDR_NONE, it's just a #define to -1 anyway.
 * So just #define it to -1 here if it doesn't already exist.
 */

#if !defined(INADDR_NONE)
#define INADDR_NONE -1
#endif

typedef struct opal_if_t {
    opal_list_item_t     super;
    char                if_name[IF_NAMESIZE];
    int                 if_index;
    uint16_t            if_kernel_index;
#ifndef __WINDOWS__
    int                 if_flags;
#else
    u_long              if_flags;
#endif
    int                 if_speed;
    struct sockaddr_storage  if_addr;
   uint32_t             if_mask;
#ifdef __WINDOWS__
    struct sockaddr_in  if_bcast;
#endif
    uint32_t            if_bandwidth;
} opal_if_t;

static OBJ_CLASS_INSTANCE(opal_if_t, opal_list_item_t, NULL, NULL);
static opal_list_t opal_if_list;
static bool already_done = false;
static bool do_not_resolve = false;

#define DEFAULT_NUMBER_INTERFACES 10
#define MAX_IFCONF_SIZE 10 * 1024 * 1024

/* convert a netmask (in network byte order) to CIDR notation */
static int prefix (uint32_t netmask)
{
    uint32_t mask = ntohl(netmask);
    int plen = 0;

    if (0 == mask) {
        plen = 32;
    } else {
        while ((mask % 2) == 0) {
            plen += 1;
            mask /= 2;
        }
    }

    return (32 - plen);
}
     
/*
 *  Discover the list of configured interfaces. Don't care about any
 *  interfaces that are not up or are local loopbacks.
 */

static int opal_ifinit(void) 
{
#ifndef __WINDOWS__
    int flag;

    if (already_done) {
        return OPAL_SUCCESS;
    }
    already_done = true;

    mca_base_param_reg_int_name("opal", "if_do_not_resolve",
                                "If nonzero, do not attempt to resolve interfaces",
                                false, false, (int)false, &flag);
    do_not_resolve = OPAL_INT_TO_BOOL(flag);

    OBJ_CONSTRUCT(&opal_if_list, opal_list_t);

#if defined(__NetBSD__) || defined(__FreeBSD__) || \
    defined(__OpenBSD__) || defined(__DragonFly__)
    /* configure using getifaddrs(3) */
    {
        struct ifaddrs **ifadd_list;
        struct ifaddrs *cur_ifaddrs;
        struct sockaddr_in* sin_addr;

        /* 
         * the manpage claims that getifaddrs() allocates the memory,
         * and freeifaddrs() is later used to release the allocated memory.
         * however, without this malloc the call to getifaddrs() segfaults
         */
        ifadd_list = (struct ifaddrs **) malloc(sizeof(struct ifaddrs*));

        /* create the linked list of ifaddrs structs */
        if (getifaddrs(ifadd_list) < 0) {
            opal_output(0, "opal_ifinit: getifaddrs() failed with error=%d\n",
                    errno);
            return OPAL_ERROR;
        }

        for (cur_ifaddrs = *ifadd_list; NULL != cur_ifaddrs; 
                cur_ifaddrs = cur_ifaddrs->ifa_next) {
            opal_if_t *intf;
            struct in_addr a4;

            /* skip non- af_inet interface addresses */
            if (AF_INET != cur_ifaddrs->ifa_addr->sa_family) {
		continue;
	    }

            /* skip interface if it is down (IFF_UP not set) */
            if (0 == (cur_ifaddrs->ifa_flags & IFF_UP)) {
                continue;
            }

            /* skip interface if it is a loopback device (IFF_LOOPBACK set) */
            /* or if it is a point-to-point interface */
            /* TODO: do we really skip p2p? */
            if (0 != (cur_ifaddrs->ifa_flags & IFF_LOOPBACK) ||
                0 != (cur_ifaddrs->ifa_flags & IFF_POINTOPOINT)) {
                continue;
            }

            sin_addr = (struct sockaddr_in *) cur_ifaddrs->ifa_addr;

            intf = OBJ_NEW(opal_if_t);
            if (NULL == intf) {
                opal_output(0, "opal_ifinit: unable to allocate %lu bytes\n",
                            (int) sizeof(opal_if_t));
                return OPAL_ERR_OUT_OF_RESOURCE;
            }

            /* fill values into the opal_if_t */
            memcpy(&a4, &(sin_addr->sin_addr), sizeof(struct in_addr));
            
            strncpy(intf->if_name, cur_ifaddrs->ifa_name, IF_NAMESIZE);
            intf->if_index = opal_list_get_size(&opal_if_list) + 1;
            ((struct sockaddr_in*) &intf->if_addr)->sin_addr = a4;
            ((struct sockaddr_in*) &intf->if_addr)->sin_family = AF_INET;
            ((struct sockaddr_in*) &intf->if_addr)->sin_len =  cur_ifaddrs->ifa_addr->sa_len;

	    intf->if_mask = prefix( sin_addr->sin_addr.s_addr);
            intf->if_flags = cur_ifaddrs->ifa_flags;

            intf->if_kernel_index = 
                (uint16_t) if_nametoindex(cur_ifaddrs->ifa_name);

            opal_list_append(&opal_if_list, &(intf->super));
        }   /*  of for loop over ifaddrs list */
    }
    /* End of various flavors of BSD */
#else
    /* Beginning of !(various flavors of BSD) */
    {
        int sd;
        int lastlen, num, rem;
        char *ptr;
        struct ifconf ifconf;
        int ifc_len;
        bool successful_locate = false;

        /* Create the internet socket to test with.  Must use AF_INET;
           using AF_UNSPEC or AF_INET6 will cause everything to
           fail. */
        if ((sd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
            opal_output(0, "opal_ifinit: socket() failed with errno=%d\n", 
                        errno);
            return OPAL_ERROR;
        }

        /*
         * Get Network Interface configuration 
         *
         * Some notes on the behavior of ioctl(..., SIOCGIFCONF,...)
         * when not enough space is allocated for all the entries.
         *
         * - Solaris returns -1, errno EINVAL if there is not enough
         *   space 
         * - OS X returns 0, sets .ifc_len to the space used by the
         *   by the entries that did fit.
         * - Linux returns 0, sets .ifc_len to the space required to
         *   hold all the entries (although it only writes what will
         *   fit in the buffer of .ifc_len passed to the function).
         * - FreeBSD returns 0, sets .ifc_len to 0.
         *
         * Everyone else seems to do one of the four.
         */
        lastlen = 0;
        ifc_len = sizeof(struct ifreq) * DEFAULT_NUMBER_INTERFACES;
        do {
            ifconf.ifc_len = ifc_len;
            ifconf.ifc_req = malloc(ifc_len);
            if (NULL == ifconf.ifc_req) {
                close(sd);
                return OPAL_ERROR;
            }
            
            /* initialize the memory so valgrind and purify won't
             * complain.  Since this isn't performance critical, just
             * always memset.
             */
            memset(ifconf.ifc_req, 0, ifconf.ifc_len);
            
            if (ioctl(sd, SIOCGIFCONF, &ifconf) < 0) {
                /* if we got an einval, we probably don't have enough
                   space.  so we'll fall down and try to expand our
                   space */
                if (errno != EINVAL && lastlen != 0) {
                    opal_output(0, "opal_ifinit: ioctl(SIOCGIFCONF) \
                            failed with errno=%d", 
                                errno);
                    free(ifconf.ifc_req);
                    close(sd);
                    return OPAL_ERROR;
                }
            } else {
                /* if ifc_len is 0 or different than what we set it to
                   at call to ioctl, try again with a bigger buffer.
                   else stop */
                if (ifconf.ifc_len == lastlen && ifconf.ifc_len > 0) {
                    /* we didn't expand.  we're done */
                    successful_locate = true;
                    break;
                }
                lastlen = ifconf.ifc_len;
            }
            
            /* Yes, we overflowed (or had an EINVAL on the ioctl).
               Loop back around and try again with a bigger buffer */
            free(ifconf.ifc_req);
            ifc_len = (ifc_len == 0) ? 1 : ifc_len * 2;
        } while (ifc_len < MAX_IFCONF_SIZE);
        if (!successful_locate) {
            opal_output(0, "opal_ifinit: unable to find network interfaces.");
            return OPAL_ERR_FATAL;
        }
        
        /* 
         * Setup indexes 
         */
        ptr = (char*) ifconf.ifc_req;
        rem = ifconf.ifc_len;
        num = 0;
        
        /* loop through all interfaces */
        while (rem > 0) {
            struct ifreq* ifr = (struct ifreq*) ptr;
            opal_if_t *intf;
            int length;
            
            intf = OBJ_NEW(opal_if_t);
            if (NULL == intf) {
                opal_output(0, "opal_ifinit: unable to allocated %lu bytes\n", (unsigned long)sizeof(opal_if_t));
                free(ifconf.ifc_req);
                close(sd);
                return OPAL_ERR_OUT_OF_RESOURCE;
            }
            
            /* compute offset for entries */
#ifdef HAVE_STRUCT_SOCKADDR_SA_LEN
            length = sizeof(struct sockaddr);
            
            if (ifr->ifr_addr.sa_len > length) {
                length = ifr->ifr_addr.sa_len;
            }
            
            length += sizeof(ifr->ifr_name);
#else
            length = sizeof(struct ifreq);
#endif
            
            rem -= length;
            ptr += length;
            
            /* see if we like this entry */
            if (AF_INET != ifr->ifr_addr.sa_family) {
                continue;
            }
            
            if (ioctl(sd, SIOCGIFFLAGS, ifr) < 0) {
                opal_output(0, "opal_ifinit: ioctl(SIOCGIFFLAGS) failed with errno=%d", errno);
                continue;
            }
            if ((ifr->ifr_flags & IFF_UP) == 0) {
                continue;
            }
#ifdef IFF_SLAVE
            /* Is this a slave to a load balancer or bonded channel?
               If so, don't use it -- pick up the master instead */
            if ((ifr->ifr_flags & IFF_SLAVE) != 0) {
                continue;
            }
#endif
#if 0
            if ((ifr->ifr_flags & IFF_LOOPBACK) != 0) {
                continue;
            }
#endif
            
            /* copy entry over into our data structure */
            strcpy(intf->if_name, ifr->ifr_name);
            intf->if_flags = ifr->ifr_flags;
            
            /* every new address gets its own internal if_index */
            intf->if_index = opal_list_get_size(&opal_if_list)+1;
            
            /* assign the kernel index to distinguish different NICs */
#ifndef SIOCGIFINDEX
            intf->if_kernel_index = intf->if_index;
#else
            if (ioctl(sd, SIOCGIFINDEX, ifr) < 0) {
                opal_output(0,"opal_ifinit: ioctl(SIOCGIFINDEX) failed with errno=%d", errno);
                continue;
            }
#if defined(ifr_ifindex)
            intf->if_kernel_index = ifr->ifr_ifindex;
#elif defined(ifr_index)
            intf->if_kernel_index = ifr->ifr_index;
#else
            intf->if_kernel_index = -1;
#endif
#endif /* SIOCGIFINDEX */
            
            /* This call returns IPv4 addresses only. Use SIOCGLIFADDR
               instead */
            if (ioctl(sd, SIOCGIFADDR, ifr) < 0) {
                opal_output(0, "opal_ifinit: ioctl(SIOCGIFADDR) failed with errno=%d", errno);
                break;
            }
            if (AF_INET != ifr->ifr_addr.sa_family) {
                continue;
            }
            
            /* based on above, we know this is an IPv4 address... */
            memcpy(&intf->if_addr, &ifr->ifr_addr, sizeof(struct sockaddr_in));
            
            if (ioctl(sd, SIOCGIFNETMASK, ifr) < 0) {
                opal_output(0, "opal_ifinit: ioctl(SIOCGIFNETMASK) failed with errno=%d", errno);
                continue;
            }
            
            /* generate CIDR and assign to netmask */
            intf->if_mask = prefix(((struct sockaddr_in*) &ifr->ifr_addr)->sin_addr.s_addr);
            
            opal_list_append(&opal_if_list, &(intf->super));
        }
        free(ifconf.ifc_req);
        close(sd);
    }        
#endif /* anything other than {Net,Open,Free}BSD and DragonFly */

#if OPAL_WANT_IPV6
#ifdef __linux__ /* Linux does not have SIOCGL*, so parse
                     /proc/net/if_inet6 instead */
    {
        FILE *f;
        if ((f = fopen("/proc/net/if_inet6", "r"))) {
            char ifname[IF_NAMESIZE];
            unsigned int idx, pfxlen, scope, dadstat;
            struct in6_addr a6;
            int iter;
            uint32_t flag;
            unsigned int addrbyte[16];

            while (fscanf(f, "%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x %x %x %x %x %20s\n",
                      &addrbyte[0], &addrbyte[1], &addrbyte[2], &addrbyte[3],
                      &addrbyte[4], &addrbyte[5], &addrbyte[6], &addrbyte[7],
                      &addrbyte[8], &addrbyte[9], &addrbyte[10], &addrbyte[11],
                      &addrbyte[12], &addrbyte[13], &addrbyte[14], &addrbyte[15],
                      &idx, &pfxlen, &scope, &dadstat, ifname) != EOF) {
                opal_if_t *intf;

                /* we don't want any other scope than global */
                if (scope != 0) {
                   continue;
                }

                intf = OBJ_NEW(opal_if_t);
                if (NULL == intf) {
                    opal_output(0, "opal_ifinit: unable to allocate %lu bytes\n",
                                    (unsigned long)sizeof(opal_if_t));
                    fclose(f);
                    return OPAL_ERR_OUT_OF_RESOURCE;
                }
        
                for (iter = 0; iter < 16; iter++) {
                    a6.s6_addr[iter] = addrbyte[iter];
                }

                /* now construct the opal_if_t */
                strncpy(intf->if_name, ifname, IF_NAMESIZE);
                intf->if_index = opal_list_get_size(&opal_if_list)+1;
                intf->if_kernel_index = (uint16_t) idx;
                ((struct sockaddr_in6*) &intf->if_addr)->sin6_addr = a6;
                ((struct sockaddr_in6*) &intf->if_addr)->sin6_family = AF_INET6;
                ((struct sockaddr_in6*) &intf->if_addr)->sin6_scope_id = scope;
                intf->if_mask = pfxlen;
                if (OPAL_SUCCESS == opal_ifindextoflags(opal_ifnametoindex (ifname), &flag)) {
                    intf->if_flags = flag;
                } else {
                    intf->if_flags = IFF_UP;
                }
                
                /* copy new interface information to heap and append
                   to list */
                opal_list_append(&opal_if_list, &(intf->super));
            } /* of while */
            fclose(f);
        }
    }
#endif

/*
 * the bsd ipv6 code below has only been tested on netbsd and linux (glibc>=2.4)
 *
 * - getifaddrs() has been included in glibc-2.4, so we can finally use
 *   it in linux, too.
 * - getifaddrs() should be the prefered way to access interface / address
 *   pairs whenever it is available, for both IPv4 and IPv6
 *   TODO:  - more testing 
 *          - let configure decide whether getifaddrs() is available,
 *            and use it when available, independent of operating system
 */



#if defined( __NetBSD__) || defined(__OpenBSD__) || defined(__FreeBSD__) || \
             defined(__386BSD__) || defined(__bsdi__) || defined(__APPLE__) 
/*           || defined(__linux__)  */
             
    {
        struct ifaddrs **ifadd_list;
        struct ifaddrs *cur_ifaddrs;
        struct sockaddr_in6* sin_addr;

        /* 
         * the manpage claims that getifaddrs() allocates the memory,
         * and freeifaddrs() is later used to release the allocated memory.
         * however, without this malloc the call to getifaddrs() segfaults
         */
        ifadd_list = (struct ifaddrs **) malloc(sizeof(struct ifaddrs*));

        /* create the linked list of ifaddrs structs */
        if (getifaddrs(ifadd_list) < 0) {
            opal_output(0, "opal_ifinit: getifaddrs() failed with error=%d\n",
                    errno);
            return OPAL_ERROR;
        }

        for (cur_ifaddrs = *ifadd_list; NULL != cur_ifaddrs; 
                cur_ifaddrs = cur_ifaddrs->ifa_next) {
            opal_if_t *intf;
            struct in6_addr a6;

            /* skip non-ipv6 interface addresses */
            if (AF_INET6 != cur_ifaddrs->ifa_addr->sa_family) {
#if 0
                printf("skipping non-ipv6 interface %s.\n", cur_ifaddrs->ifa_name);
#endif
                continue;
            }

            /* skip interface if it is down (IFF_UP not set) */
            if (0 == (cur_ifaddrs->ifa_flags & IFF_UP)) {
#if 0
                printf("skipping non-up interface %s.\n", cur_ifaddrs->ifa_name);
#endif
                continue;
            }

            /* skip interface if it is a loopback device (IFF_LOOPBACK set) */
            /* or if it is a point-to-point interface */
            /* TODO: do we really skip p2p? */
            if (0 != (cur_ifaddrs->ifa_flags & IFF_LOOPBACK)
                    || 0!= (cur_ifaddrs->ifa_flags & IFF_POINTOPOINT)) {
#if 0
                printf("skipping loopback interface %s.\n", cur_ifaddrs->ifa_name);
#endif              
                continue;
            }

            sin_addr = (struct sockaddr_in6 *) cur_ifaddrs->ifa_addr;
            intf = OBJ_NEW(opal_if_t);
            if (NULL == intf) {
                opal_output(0, "opal_ifinit: unable to allocate %lu bytes\n",
                            sizeof(opal_if_t));
                return OPAL_ERR_OUT_OF_RESOURCE;
            }

            /* 
             * skip IPv6 address starting with fe80:, as this is supposed to be
             * link-local scope. sockaddr_in6->sin6_scope_id doesn't always work
             * TODO: test whether scope id is set to a sensible value on 
             * linux and/or bsd (including osx)
             *
             * MacOSX: fe80::... has a scope of 0, but ifconfig -a shows
             * a scope of 4 on that particular machine, 
             * so the scope returned by getifaddrs() isn't working properly
             */

            if ((IN6_IS_ADDR_LINKLOCAL (&sin_addr->sin6_addr))) {
#if 0
                printf("skipping link-local ipv6 address on interface \
                        %s with scope %d.\n", 
                        cur_ifaddrs->ifa_name, sin_addr->sin6_scope_id);
#endif
                continue;
            }

#if 0
            char *addr_name = (char *) malloc(48*sizeof(char));
            inet_ntop(AF_INET6, &sin_addr->sin6_addr, addr_name, 48*sizeof(char));
            opal_output(0, "ipv6 capable interface %s discovered, address %s.\n", 
                    cur_ifaddrs->ifa_name, addr_name);
            free(addr_name);
#endif

            /* fill values into the opal_if_t */
            memcpy(&a6, &(sin_addr->sin6_addr), sizeof(struct in6_addr));
            
            strncpy(intf->if_name, cur_ifaddrs->ifa_name, IF_NAMESIZE);
            intf->if_index = opal_list_get_size(&opal_if_list) + 1;
            ((struct sockaddr_in6*) &intf->if_addr)->sin6_addr = a6;
            ((struct sockaddr_in6*) &intf->if_addr)->sin6_family = AF_INET6;

            /* since every scope != 0 is ignored, we just set the scope to 0 */
            ((struct sockaddr_in6*) &intf->if_addr)->sin6_scope_id = 0;

            /*
             * hardcoded netmask, adrian says that's ok
             */
            intf->if_mask = 64;
            intf->if_flags = cur_ifaddrs->ifa_flags;

            /*
             * FIXME: figure out how to gain access to the kernel index
             * (or create our own), getifaddrs() does not contain such
             * data
             */
            intf->if_kernel_index = 
                (uint16_t) if_nametoindex(cur_ifaddrs->ifa_name);
            opal_list_append(&opal_if_list, &(intf->super));
        }   /*  of for loop over ifaddrs list */
    }
#endif  /* bsd,  macosx */


#ifdef __sun__
    /* Solaris IPv6 interface discovery */
    {
        int i;
        int sd;
        int error;
        uint16_t kindex;
        struct lifnum lifnum;
        struct lifconf lifconf;
        struct lifreq *lifreq, lifquery;
        
        sd = socket (AF_INET6, SOCK_DGRAM, 0);
        if (sd < 0) {
            opal_output (0, "opal_ifinit: unable to open IPv6 socket\n");
            return OPAL_ERROR;
        }

        /* we only ask for IPv6; IPv4 discovery has already been done */
        lifnum.lifn_family = AF_INET6;
        lifnum.lifn_flags = 0;
        lifnum.lifn_count = 0;

        /* get the number of interfaces in the system */
        error = ioctl (sd, SIOCGLIFNUM, &lifnum);
        if (error < 0) {
            opal_output (0,
              "opal_ifinit: ioctl SIOCGLIFNUM failed with errno=%d\n", errno);
            return OPAL_ERROR;
        }

        memset (&lifconf, 0, sizeof (struct lifconf));
        memset (&lifquery, 0, sizeof (struct lifreq));
        lifconf.lifc_family = AF_INET6;
        lifconf.lifc_flags = 0;
        lifconf.lifc_len = lifnum.lifn_count * sizeof (struct lifreq) * 2;
        lifconf.lifc_buf = malloc (lifconf.lifc_len);
        if (NULL == lifconf.lifc_buf) {
            opal_output (0, "opal_ifinit: IPv6 discovery: malloc() failed\n");
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        memset (lifconf.lifc_buf, 0, lifconf.lifc_len);

        error = ioctl (sd, SIOCGLIFCONF, &lifconf);
        if (error < 0) {
            opal_output (0,
              "opal_ifinit: IPv6 SIOCGLIFCONF failed with errno=%d\n", errno);
        }

        for (i = 0; i + sizeof (struct lifreq) <= lifconf.lifc_len;
             i += sizeof (*lifreq)) {

            lifreq = (struct lifreq *)((caddr_t)lifconf.lifc_buf + i);
            strncpy (lifquery.lifr_name, lifreq->lifr_name,
                                 sizeof (lifquery.lifr_name));

            /* lookup kernel index */
            error = ioctl (sd, SIOCGLIFINDEX, &lifquery);
            if (error < 0) {
                opal_output (0,
                  "opal_ifinit: SIOCGLIFINDEX failed with errno=%d\n", errno);
                return OPAL_ERROR;
            }
            kindex = lifquery.lifr_index;

            /* lookup interface flags */
            error = ioctl (sd, SIOCGLIFFLAGS, &lifquery);
            if (error < 0) {
                opal_output (0,
                  "opal_ifinit: SIOCGLIFFLAGS failed with errno=%d\n", errno);
                return OPAL_ERROR;
            }

            if (AF_INET6 == lifreq->lifr_addr.ss_family) {
                struct sockaddr_in6* my_addr = (struct sockaddr_in6*) &lifreq->lifr_addr;
                /* we surely want to check for sin6_scope_id, but Solaris
                   does not set it correctly, so we have to look for
                   global scope. For now, global is anything which is
                   neither loopback nor link local.

                   Bug, FIXME: site-local, multicast, ... missing
                   Check for 2000::/3?
                */
                if ( (! IN6_IS_ADDR_LOOPBACK (&my_addr->sin6_addr)) &&
                     (! IN6_IS_ADDR_LINKLOCAL (&my_addr->sin6_addr))) {
                    /* create interface for newly found address */
                    opal_if_t *intf;

                    intf = OBJ_NEW(opal_if_t);
                    if (NULL == intf) {
                        opal_output (0,
                                     "opal_ifinit: unable to allocate %d bytes\n",
                                     sizeof (opal_if_t));
                        return OPAL_ERR_OUT_OF_RESOURCE;
                    }

                    strncpy (intf->if_name, lifreq->lifr_name, IF_NAMESIZE);
                    intf->if_index = opal_list_get_size(&opal_if_list)+1;
                    memcpy(&intf->if_addr, my_addr, sizeof (*my_addr));
                    intf->if_mask = 64;
                    /* lifrq flags are uint64_t */
                    intf->if_flags =
                        (uint32_t)(0x00000000ffffffff) & lifquery.lifr_flags;

                    /* append to list */
                    opal_list_append (&opal_if_list, &(intf->super));
                }
            }
        } /* for */

        if (NULL != lifconf.lifc_buf) {
            free (lifconf.lifc_buf);
        }
    }
#endif /* __sun__ */

#endif /* OPAL_WANT_IPV6 */

#else /* __WINDOWS__ implementation begins */
    {
        /* 
           1. check if the interface info list is already populated. If so, return
           2. get the interface information which is required using WSAIoctl
           3. construct opal_if_list and populate it with the list of interfaces we have
           CAVEAT: Does not support the following options which are supported in SIOCGIFCONF
                - kernel table index
                - interface name
         */

        #define MAX_INTERFACES 10 /* Anju: for now assume there are no more than this */
        SOCKET sd; 
        INTERFACE_INFO if_list[MAX_INTERFACES];
        int num_interfaces;
        unsigned long num_bytes_returned;
        int i;
        unsigned int interface_counter = 0;
        opal_if_t *intf;

        /* return if this has been done before */
        if (already_done) {
            return OPAL_SUCCESS;
        }
        already_done = true;
  
        /* create a socket */
        sd = WSASocket (AF_INET, SOCK_DGRAM, IPPROTO_UDP, NULL, 0, 0);
        if (sd == SOCKET_ERROR) {
            opal_output(0, "opal_ifinit: WSASocket failed with errno=%d\n",WSAGetLastError());
            return OPAL_ERROR;
        }

        /* get the information about the interfaces */
        if (SOCKET_ERROR == WSAIoctl (sd, 
                                      SIO_GET_INTERFACE_LIST, 
                                      NULL, 
                                      0, 
                                      &if_list,
                                      sizeof (if_list), 
                                      &num_bytes_returned, 
                                      0,
                                    0)) {
            opal_output(0, "opal_ifinit: WSAIoctl failed with errno=%d\n",WSAGetLastError());
            return OPAL_ERROR;
        }

        /* create and populate opal_if_list */
        OBJ_CONSTRUCT (&opal_if_list, opal_list_t);

        /* loop through all the interfaces and create the list */
        num_interfaces = num_bytes_returned / sizeof (INTERFACE_INFO);
        for (i = 0; i < num_interfaces; ++i) {
            /* do all this only if the interface is up, and skip loopback interface */
            if (0 != (if_list[i].iiFlags & IFF_UP)
                && 0 == (if_list[i].iiFlags & IFF_LOOPBACK)) {

                intf = OBJ_NEW(opal_if_t);
                if (NULL == intf) {
                    opal_output (0,"opal_ifinit: Unable to malloc %d bytes",sizeof(opal_list_t));
                    return OPAL_ERR_OUT_OF_RESOURCE;
                }
        
                /* fill in the interface address */ 
                memcpy(&intf->if_addr, &(if_list[i].iiAddress),
                       sizeof(intf->if_addr));

                /* fill in the netmask information */
                memcpy(&intf->if_mask, &(if_list[i].iiNetmask),
                       sizeof(intf->if_mask));

                /* fill in the bcast address */
                memcpy(&intf->if_bcast, &(if_list[i].iiBroadcastAddress),
                       sizeof(intf->if_bcast));

                /* fill in the flags */
                intf->if_flags = if_list[i].iiFlags;

                /* fill in the index in the table */
                intf->if_index = opal_list_get_size(&opal_if_list)+1;

                /* fill in the kernel index */
                intf->if_kernel_index = intf->if_index;

                /* generate the interface name, e.g. eth0, eth1, ..... */
                sprintf(intf->if_name, "eth%u", interface_counter++);

                /* copy all this into a persistent form and store it in the list */
                opal_list_append(&opal_if_list, &(intf->super));
            }
        }
    }
#endif
    return OPAL_SUCCESS;
}


/*
 *  Finalize the list of configured interfaces to free malloc'd memory
 */

int opal_iffinalize(void) 
{
    if (already_done) {
#ifndef __WINDOWS__
        opal_if_t *intf_ptr;
    
        while (NULL != 
               (intf_ptr = (opal_if_t*)opal_list_remove_first(&opal_if_list))) {
            OBJ_RELEASE(intf_ptr);
        }
        OBJ_DESTRUCT(&opal_if_list);
#endif
        already_done = false;
    }
    return OPAL_SUCCESS;
}


/*
 *  Look for interface by name and returns its address 
 *  as a dotted decimal formatted string.
 */

int opal_ifnametoaddr(const char* if_name, struct sockaddr* addr, int length)
{
    opal_if_t* intf;
    int rc = opal_ifinit();
    if (rc != OPAL_SUCCESS)
        return rc;

    for (intf =  (opal_if_t*)opal_list_get_first(&opal_if_list);
        intf != (opal_if_t*)opal_list_get_end(&opal_if_list);
        intf =  (opal_if_t*)opal_list_get_next(intf)) {
        if (strcmp(intf->if_name, if_name) == 0) {
            memcpy(addr, &intf->if_addr, length);
            return OPAL_SUCCESS;
        }
    }
    return OPAL_ERROR;
}


/*
 *  Look for interface by name and returns its 
 *  corresponding opal_list index.
 */

int opal_ifnametoindex(const char* if_name)
{
    opal_if_t* intf;
    int rc = opal_ifinit();
    if (rc != OPAL_SUCCESS)
        return rc;

    for (intf =  (opal_if_t*)opal_list_get_first(&opal_if_list);
        intf != (opal_if_t*)opal_list_get_end(&opal_if_list);
        intf =  (opal_if_t*)opal_list_get_next(intf)) {
        if (strcmp(intf->if_name, if_name) == 0) {
            return intf->if_index;
        }
    }
    return -1;
}


/*
 *  Look for interface by name and returns its 
 *  corresponding kernel index.
 */

int16_t opal_ifnametokindex(const char* if_name)
{
    opal_if_t* intf;
    int rc = opal_ifinit();
    if (rc != OPAL_SUCCESS)
        return rc;

    for (intf =  (opal_if_t*)opal_list_get_first(&opal_if_list);
        intf != (opal_if_t*)opal_list_get_end(&opal_if_list);
        intf =  (opal_if_t*)opal_list_get_next(intf)) {
        if (strcmp(intf->if_name, if_name) == 0) {
            return intf->if_kernel_index;
        }
    }
    return -1;
}


/*
 *  Look for interface by opal_list index and returns its 
 *  corresponding kernel index.
 */

int opal_ifindextokindex(int if_index)
{
    opal_if_t* intf;
    int rc = opal_ifinit();
    if (rc != OPAL_SUCCESS)
        return rc;

    for (intf =  (opal_if_t*)opal_list_get_first(&opal_if_list);
        intf != (opal_if_t*)opal_list_get_end(&opal_if_list);
        intf =  (opal_if_t*)opal_list_get_next(intf)) {
        if (if_index == intf->if_index) {
            return intf->if_kernel_index;
        }
    }
    return -1;
}


/*
 *  Attempt to resolve the adddress (given as either IPv4/IPv6 string
 *  or hostname) and lookup corresponding interface.
 */

int opal_ifaddrtoname(const char* if_addr, char* if_name, int length)
{
    opal_if_t* intf;
    int rc;
#if OPAL_WANT_IPV6
    int error;
    struct addrinfo hints, *res = NULL, *r;
#else
#ifndef __WINDOWS__
    in_addr_t inaddr;
#else 
    unsigned long inaddr;
#endif
    struct hostent *h;
#endif

    rc = opal_ifinit();
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    /* if the user asked us not to resolve interfaces, then just return */
    if (do_not_resolve) {
        /* return not found so ifislocal will declare
         * the node to be non-local
         */
        return OPAL_ERR_NOT_FOUND;
    }
    
#if OPAL_WANT_IPV6
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = PF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    error = getaddrinfo(if_addr, NULL, &hints, &res);

    if (error) {
        if (NULL != res) {
            freeaddrinfo (res);
        }
        return OPAL_ERR_NOT_FOUND;
    }

    for (r = res; r != NULL; r = r->ai_next) {
        for (intf =  (opal_if_t*)opal_list_get_first(&opal_if_list);
            intf != (opal_if_t*)opal_list_get_end(&opal_if_list);
            intf =  (opal_if_t*)opal_list_get_next(intf)) {
            
            if (AF_INET == r->ai_family) {
                struct sockaddr_in ipv4;
                struct sockaddr_in *inaddr;

                inaddr = (struct sockaddr_in*) &intf->if_addr;
                memcpy (&ipv4, r->ai_addr, r->ai_addrlen);
                
                if (inaddr->sin_addr.s_addr == ipv4.sin_addr.s_addr) {
                    strncpy(if_name, intf->if_name, length);
                    return OPAL_SUCCESS;
                }
            } else {
                if (IN6_ARE_ADDR_EQUAL(&((struct sockaddr_in6*) &intf->if_addr)->sin6_addr,
                    &((struct sockaddr_in6*) r->ai_addr)->sin6_addr)) {
                    strncpy(if_name, intf->if_name, length);
                    return OPAL_SUCCESS;
                }
            }
        }
    }
    if (NULL != res) {
        freeaddrinfo (res);
    }
#else
    inaddr = inet_addr(if_addr);

    if (INADDR_NONE == inaddr) {
        h = gethostbyname(if_addr);
        if (0 == h) {
            return OPAL_ERR_NOT_FOUND;
        }
        memcpy(&inaddr, h->h_addr, sizeof(inaddr));
    }

    for (intf =  (opal_if_t*)opal_list_get_first(&opal_if_list);
        intf != (opal_if_t*)opal_list_get_end(&opal_if_list);
        intf =  (opal_if_t*)opal_list_get_next(intf)) {
        if (((struct sockaddr_in*) &intf->if_addr)->sin_addr.s_addr == inaddr) {
            strncpy(if_name, intf->if_name, length);
            return OPAL_SUCCESS;
        }
    }
#endif
    return OPAL_ERR_NOT_FOUND;
}

/*
 *  Return the number of discovered interface.
 */

int opal_ifcount(void)
{
    if (opal_ifinit() != OPAL_SUCCESS)
        return (-1);
    return opal_list_get_size(&opal_if_list);
}


/*
 *  Return the opal_list interface index for the first
 *  interface in our list.
 */

int opal_ifbegin(void)
{
    opal_if_t *intf;
    if (opal_ifinit() != OPAL_SUCCESS)
        return (-1);
    intf = (opal_if_t*)opal_list_get_first(&opal_if_list);
    if (NULL != intf)
        return intf->if_index;
    return (-1);
}


/*
 *  Located the current position in the list by if_index and
 *  return the interface index of the next element in our list
 *  (if it exists).
 */

int opal_ifnext(int if_index)
{
    opal_if_t *intf;
    if (opal_ifinit() != OPAL_SUCCESS)
        return (-1);

    for (intf =  (opal_if_t*)opal_list_get_first(&opal_if_list);
        intf != (opal_if_t*)opal_list_get_end(&opal_if_list);
        intf =  (opal_if_t*)opal_list_get_next(intf)) {
        if (intf->if_index == if_index) {
            do {
                opal_if_t* if_next = (opal_if_t*)opal_list_get_next(intf);
                opal_if_t* if_end =  (opal_if_t*)opal_list_get_end(&opal_if_list);
                if (if_next == if_end) {
                    return -1;
                }
                intf = if_next;
            } while(intf->if_index == if_index);
            return intf->if_index;
        }
    }
    return (-1);
}


/* 
 *  Lookup the interface by opal_list index and return the 
 *  primary address assigned to the interface.
 */

int opal_ifindextoaddr(int if_index, struct sockaddr* if_addr, unsigned int length)
{
    opal_if_t* intf;
    int rc = opal_ifinit();
    if (rc != OPAL_SUCCESS)
        return rc;

    for (intf =  (opal_if_t*)opal_list_get_first(&opal_if_list);
        intf != (opal_if_t*)opal_list_get_end(&opal_if_list);
        intf =  (opal_if_t*)opal_list_get_next(intf)) {
        if (intf->if_index == if_index) {
            memcpy(if_addr, &intf->if_addr, MIN(length, sizeof (intf->if_addr)));
            return OPAL_SUCCESS;
        }
    }
    return OPAL_ERROR;
}


/* 
 *  Lookup the interface by opal_list index and return the 
 *  network mask assigned to the interface.
 */

int opal_ifindextomask(int if_index, uint32_t* if_mask, int length)
{
    opal_if_t* intf;
    int rc = opal_ifinit();
    if (rc != OPAL_SUCCESS)
        return rc;

    for (intf =  (opal_if_t*)opal_list_get_first(&opal_if_list);
        intf != (opal_if_t*)opal_list_get_end(&opal_if_list);
        intf =  (opal_if_t*)opal_list_get_next(intf)) {
        if (intf->if_index == if_index) {
            memcpy(if_mask, &intf->if_mask, length);
            return OPAL_SUCCESS;
        }
    }
    return OPAL_ERROR;
}

/* 
 *  Lookup the interface by opal_list index and return the 
 *  flags assigned to the interface.
 *
 *  Bug: Make return type portable (compatible with Windows)
 */

int opal_ifindextoflags(int if_index, uint32_t* if_flags)
{
    opal_if_t* intf;
    int rc = opal_ifinit();
    if (rc != OPAL_SUCCESS)
        return rc;

    for (intf =  (opal_if_t*)opal_list_get_first(&opal_if_list);
        intf != (opal_if_t*)opal_list_get_end(&opal_if_list);
        intf =  (opal_if_t*)opal_list_get_next(intf)) {
        if (intf->if_index == if_index) {
            memcpy(if_flags, &intf->if_flags, sizeof(uint32_t));
            return OPAL_SUCCESS;
        }
    }
    return OPAL_ERROR;
}



/* 
 *  Lookup the interface by opal_list index and return
 *  the associated name.
 */

int opal_ifindextoname(int if_index, char* if_name, int length)
{
    opal_if_t *intf;
    int rc = opal_ifinit();
    if (rc != OPAL_SUCCESS)
        return rc;

    for (intf =  (opal_if_t*)opal_list_get_first(&opal_if_list);
        intf != (opal_if_t*)opal_list_get_end(&opal_if_list);
        intf =  (opal_if_t*)opal_list_get_next(intf)) {
        if (intf->if_index == if_index) {
            strncpy(if_name, intf->if_name, length);
            return OPAL_SUCCESS;
        }
    }
    return OPAL_ERROR;
}


/* 
 *  Lookup the interface by kernel index and return
 *  the associated name.
 */

int opal_ifkindextoname(int if_kindex, char* if_name, int length)
{
    opal_if_t *intf;
    int rc = opal_ifinit();
    if (rc != OPAL_SUCCESS)
        return rc;

    for (intf =  (opal_if_t*)opal_list_get_first(&opal_if_list);
        intf != (opal_if_t*)opal_list_get_end(&opal_if_list);
        intf =  (opal_if_t*)opal_list_get_next(intf)) {
        if (intf->if_kernel_index == if_kindex) {
            strncpy(if_name, intf->if_name, length);
            return OPAL_SUCCESS;
        }
    }
    return OPAL_ERROR;
}


#define ADDRLEN 100
bool
opal_ifislocal(const char *hostname)
{
    int ret;
#if OPAL_WANT_IPV6
    char addrname[NI_MAXHOST]; /* should be larger than ADDRLEN, but I think
                                  they really mean IFNAMESIZE */
#else
    char addrname[ADDRLEN + 1];
#endif

    ret = opal_ifaddrtoname(hostname, addrname, ADDRLEN);

    return (OPAL_SUCCESS == ret) ? true : false;
}

static uint32_t parse_dots(char *addr)
{
    char **tuple;
    uint32_t n[]={0,0,0,0};
    uint32_t net;
    int i;

    tuple = opal_argv_split(addr, '.');
    /* now assemble the address */
    for (i=0; NULL != tuple[i]; i++) {
        n[i] = strtoul(tuple[i], NULL, 10);
    }
    net = OPAL_IF_ASSEMBLE_NETWORK(n[0], n[1], n[2], n[3]);
    opal_argv_free(tuple);
    return net;
}

int
opal_iftupletoaddr(char *inaddr, uint32_t *net, uint32_t *mask)
{
    char *addr;
    char **tuple;
    int pval;
    char *msk, *ptr;
    
    /* if a mask was desired... */
    if (NULL != mask) {
        /* set default */
        *mask = 0xFFFFFFFF;
        /* protect the input */
        addr = strdup(inaddr);
        
        /* if entry includes mask, split that off */
        msk = NULL;
        if (NULL != (ptr = strchr(addr, '/'))) {
            *ptr = '\0';
            msk = ptr + 1;
            /* is the mask a tuple? */
            if (NULL != strchr(msk, '.')) {
                /* yes - extract mask from it */
                *mask = parse_dots(msk);
            } else {
                /* no - must be an int telling us how
                 * much of the addr to use: e.g., /16
                 */
                pval = strtol(msk, NULL, 10);
                if (24 == pval) {
                    *mask = 0xFFFFFF00;
                } else if (16 == pval) {
                    *mask = 0xFFFF0000;
                } else if (8 == pval) {
                    *mask = 0xFF000000;
                } else {
                    opal_output(0, "opal_iftupletoaddr: unknown mask");
                    free(addr);
                    return OPAL_ERROR;
                }
            }
        } else {
            /* use the number of dots to determine it */
            tuple = opal_argv_split(addr, '.');
            pval = opal_argv_count(tuple);
            /* if we have three dots, then we have four
             * fields since it is a full address, so the
             * default netmask is fine
             */
            if (pval < 4) {
                if (3 == pval) {         /* 2 dots */
                    *mask = 0xFFFFFF00;
                } else if (2 == pval) {  /* 1 dot */
                    *mask = 0xFFFF0000;
                } else if (1 == pval) {  /* no dots */
                    *mask = 0xFF000000;
                } else {
                    opal_output(0, "opal_iftupletoaddr: unknown mask");
                    free(addr);
                    return OPAL_ERROR;
                }
            }
            opal_argv_free(tuple);
        }
        free(addr);
    }
    
    /* if network addr is desired... */
    if (NULL != net) {
        /* set default */
        *net = 0;
        /* protect the input */
        addr = strdup(inaddr);
        
        /* if entry includes mask, split that off */
        if (NULL != (ptr = strchr(addr, '/'))) {
            *ptr = '\0';
        }
        /* now assemble the address */
        *net = parse_dots(addr);
        free(addr);
    }
    
    return OPAL_SUCCESS;
}

/* 
 *  Determine if the specified interface is loopback
 */

bool opal_ifisloopback(int if_index)
{
    opal_if_t* intf;
    int rc = opal_ifinit();
    if (rc != OPAL_SUCCESS) {
        return rc;
    }
    
    for (intf =  (opal_if_t*)opal_list_get_first(&opal_if_list);
        intf != (opal_if_t*)opal_list_get_end(&opal_if_list);
        intf =  (opal_if_t*)opal_list_get_next(intf)) {
        if (intf->if_index == if_index) {
            if ((intf->if_flags & IFF_LOOPBACK) != 0) {
                return true;
            }
        }
    }
    return false;
}


#else /* HAVE_STRUCT_SOCKADDR_IN */

/* if we don't have struct sockaddr_in, we don't have traditional
   ethernet devices.  Just make everything a no-op error call (except
   for finalize, which will return successfully) */

int
opal_ifnametoaddr(const char* if_name, 
                  struct sockaddr* if_addr, int size)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

int
opal_ifaddrtoname(const char* if_addr, 
                  char* if_name, int size)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

int
opal_ifnametoindex(const char* if_name)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

int16_t
opal_ifnametokindex(const char* if_name)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

int
opal_ifindextokindex(int if_index)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

int
opal_ifcount(void)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

int
opal_ifbegin(void)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

int
opal_ifnext(int if_index)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

int
opal_ifindextoname(int if_index, char* if_name, int length)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

int
opal_ifkindextoname(int kif_index, char* if_name, int length)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

int
opal_ifindextoaddr(int if_index, struct sockaddr* if_addr, unsigned int length)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

int
opal_ifindextomask(int if_index, uint32_t* if_addr, int length)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

bool
opal_ifislocal(const char *hostname)
{
    return false;
}

int
opal_iffinalize(void)
{
    return OPAL_SUCCESS;
}

int
opal_iftupletoaddr(char *inaddr, uint32_t *net, uint32_t *mask)
{
    return 0;
}

#endif /* HAVE_STRUCT_SOCKADDR_IN */

