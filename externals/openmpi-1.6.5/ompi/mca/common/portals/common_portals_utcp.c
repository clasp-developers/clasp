/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <unistd.h>
#include <stdio.h>
#include <p3api/debug.h>

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/proc/proc.h"
#include "ompi/constants.h"
#include "ompi/runtime/ompi_module_exchange.h"

#ifdef __APPLE__
static char *ptl_ifname = "en0";
FILE *p3_out = stderr;
#else
static char *ptl_ifname = "eth0";
#endif


/* how's this for source code diving? - find private method for
   getting interface */
extern int p3tcp_my_nid(const char *if_str, unsigned int *nid);

static volatile int32_t usage_count = 0;
static volatile int32_t ni_usage_count = 0;
static bool setup_utcp_params = true;
static bool init_called = false;
static ptl_handle_ni_t active_ni_h = PTL_INVALID_HANDLE;
static mca_base_component_t portals_component = {
    MCA_BASE_VERSION_2_0_0,
    "common",
    MCA_BASE_VERSION_2_0_0,
    "portals",
    MCA_BASE_VERSION_2_0_0,
    NULL,
    NULL
};


char *
ompi_common_portals_nodeid(void)
{
    char *ret;
    asprintf(&ret, "%5d", getpid());
    return ret;
}


int
ompi_common_portals_register_mca(void)
{
    mca_base_param_reg_string(&portals_component,
                              "ifname",
                              "Interface name to use for communication",
                              false,
                              false,
                              ptl_ifname,
                              &ptl_ifname);

    return OMPI_SUCCESS;
}


int
ompi_common_portals_initialize(ptl_handle_ni_t *ni_handle, bool *accel)
{
    int ret;
    ptl_process_id_t info;

    if (OPAL_THREAD_ADD32(&usage_count, 1) > 1) return OMPI_SUCCESS;

    /* if the environment variables for the utcp implementation are
       already set, assume the user is running without the full Open
       RTE and is doing RTE testing for a more tightly-coupled
       platform (like, say, Red Storm).  Otherwise, be nice and use
       the modex to setup everything for the user */
    if (NULL == getenv("PTL_MY_RID")) {
        setup_utcp_params = true;
    } else {
        setup_utcp_params = false;
    }

    if (setup_utcp_params) {
        /* Find our contact information and post to registry.  Don't
           initialize Portals until we have everyone's contact
           information. */
        unsigned int nid;

        p3tcp_my_nid(ptl_ifname, &nid);
        info.nid = htonl(nid);
        info.pid = htonl((ptl_pid_t) getpid());

    } else {
        /* Initialize Portals and publish our assigned contact
           information */
        int max_interfaces;
        unsigned int nptl_procs, rank;

        ret = PtlInit(&max_interfaces);
        if (PTL_OK != ret) {
            opal_output(0, "%5d: PtlInit failed, returning %d\n", 
                        getpid(), ret);
            return OMPI_ERR_NOT_AVAILABLE;
        }
        init_called = true;

        /* tell the UTCP runtime code to read the env variables */
        PtlSetRank(PTL_INVALID_HANDLE, -1, -1);

        /* Initialize a network device */
        ret = PtlNIInit(PTL_IFACE_DEFAULT, /* interface to initialize */
                        PTL_PID_ANY,       /* let library assign our pid */
                        NULL,              /* no desired limits */
                        NULL,              /* no need to have limits around */
                        &active_ni_h       /* our interface handle */
                        );
        if (PTL_OK != ret) {
            opal_output(0, "%5d: PtlNIInit failed, returning %d\n", 
                        getpid(), ret);
            return OMPI_ERR_FATAL;
        }

        ret = PtlGetRank(active_ni_h, &rank, &nptl_procs);
        if (ret != PTL_OK) {
            opal_output(0, "%5d, PtlGetRank() returned %d", 
                        getpid(), ret);
            return OMPI_ERR_FATAL;
        }

        ret = PtlGetRankId(active_ni_h, rank, &info);
        if (ret != PTL_OK) {
            opal_output(0, "%5d, PtlGetRank(rank=%d) returned %d", 
                        getpid(), rank, ret);
            return OMPI_ERR_FATAL;
        }
    }

    ret = ompi_modex_send(&portals_component,
                                  &info, sizeof(ptl_process_id_t));
    if (OMPI_SUCCESS != ret) {
        return ret;
    }
        
    return OMPI_SUCCESS;
}


int
ompi_common_portals_ni_initialize(ptl_handle_ni_t *ni_handle, bool *accel)
{
    int ret;

    *accel = false;

    OPAL_THREAD_ADD32(&ni_usage_count, 1);
    if (PTL_INVALID_HANDLE != active_ni_h) {
        *ni_handle = active_ni_h;
        return OMPI_SUCCESS;
    }

    if (setup_utcp_params) {
        ompi_proc_t **procs;
        int my_rid = 0;
        ptl_process_id_t *info;
        char *nidmap = NULL, *pidmap = NULL;
        char *nid_str, *pid_str;
        size_t map_size = 0;
        size_t nprocs, size, i;
        char *tmp;
        ompi_proc_t* proc_self = ompi_proc_local();
        int max_interfaces;

        /* get our world */
        procs = ompi_proc_world(&nprocs);

        map_size = nprocs * 12 + 1; /* 12 is max length of long in decimal */
        nidmap = malloc(map_size);
        pidmap = malloc(map_size);
        nid_str = malloc(12 + 1);
        pid_str = malloc(12 + 1);
        if (NULL == nidmap || NULL == pidmap || 
            NULL == nid_str || NULL == pid_str)
            return OMPI_ERROR;
         
        for (i = 0 ; i < nprocs ; ++i) {
            if (proc_self == procs[i]) my_rid = i;

            ret = ompi_modex_recv(&portals_component,
                                          procs[i], (void**) &info, &size);
            if (OMPI_SUCCESS != ret) {
                opal_output(0, "%5d: ompi_modex_recv failed: %d", 
                            getpid(), ret);
                return ret;
            } else if (sizeof(ptl_process_id_t) != size) {
                opal_output(0, "%5d: ompi_modex_recv returned size %d, expected %d", 
                            getpid(), size, sizeof(ptl_process_id_t));
                return OMPI_ERROR;
            }

            if (i == 0) {
                snprintf(nidmap, map_size, "%u", ntohl(info->nid));
                snprintf(pidmap, map_size, "%u", ntohl(info->pid));
            } else {
                snprintf(nid_str, 12 + 1, ":%u", ntohl(info->nid));
                snprintf(pid_str, 12 + 1, ":%u", ntohl(info->pid));
                strncat(nidmap, nid_str, 12);
                strncat(pidmap, pid_str, 12);
            }

            free(info);
        }

        asprintf(&tmp, "PTL_MY_RID=%u", my_rid);
        putenv(tmp);
        asprintf(&tmp, "PTL_NIDMAP=%s", nidmap);
        putenv(tmp);
        asprintf(&tmp, "PTL_PIDMAP=%s", pidmap);
        putenv(tmp);
        asprintf(&tmp, "PTL_IFACE=%s", ptl_ifname);
        putenv(tmp);

        free(pidmap);
        free(nidmap);
        free(pid_str);
        free(nid_str);

        /*
         * Initialize Portals
         */

        ret = PtlInit(&max_interfaces);
        if (PTL_OK != ret) {
            opal_output(0, "%5d: PtlInit failed, returning %d\n", 
                        getpid(), ret);
            return OMPI_ERR_NOT_AVAILABLE;
        }
        init_called = true;

        /* tell the UTCP runtime code to read the env variables */
        PtlSetRank(PTL_INVALID_HANDLE, -1, -1);

        /* Initialize a network device */
        ret = PtlNIInit(PTL_IFACE_DEFAULT, /* interface to initialize */
                        PTL_PID_ANY,       /* let library assign our pid */
                        NULL,              /* no desired limits */
                        NULL,              /* no need to have limits around */
                        &active_ni_h       /* our interface handle */
                        );
        if (PTL_OK != ret) {
            opal_output(0, "%5d: PtlNIInit failed, returning %d\n", 
                        getpid(), ret);
            return OMPI_ERR_FATAL;
        }

        *ni_handle = active_ni_h;

        return OMPI_SUCCESS;
    }

    /* shouldn't ever be able to get here */
    return OMPI_ERROR;
}


int
ompi_common_portals_get_procs(size_t nprocs,
                              struct ompi_proc_t **procs,
                              ptl_process_id_t *portals_procs)
{
    size_t i, size;
    int ret;
    ptl_process_id_t *info;

    for (i = 0 ; i < nprocs ; ++i) {
        ret = ompi_modex_recv(&portals_component,
                                      procs[i], (void**) &info, &size);
        if (OMPI_SUCCESS != ret) {
            opal_output(0, "%5d: ompi_modex_recv failed: %d", 
                        getpid(), ret);
            return ret;
        } else if (sizeof(ptl_process_id_t) != size) {
            opal_output(0, "%5d: ompi_modex_recv returned size %d, expected %d", 
                        getpid(), size, sizeof(ptl_process_id_t));
            return OMPI_ERROR;
        }

        portals_procs[i].nid = ntohl(info->nid);
        portals_procs[i].pid = ntohl(info->pid);
    }

    return OMPI_SUCCESS;
}


int
ompi_common_portals_ni_finalize(void)
{
    if (OPAL_THREAD_ADD32(&ni_usage_count, -1) <= 0) {
        if (PTL_INVALID_HANDLE != active_ni_h) {
            if (PTL_OK != PtlNIFini(active_ni_h)) {
                active_ni_h = PTL_INVALID_HANDLE;
                return OMPI_ERROR;
            }
            active_ni_h = PTL_INVALID_HANDLE;
        }
    }

    return OMPI_SUCCESS;
}


int
ompi_common_portals_finalize(void)
{
    if (OPAL_THREAD_ADD32(&usage_count, -1) <= 0) {
        if (init_called) {
            PtlFini();
        }
    }

    return OMPI_SUCCESS;
}
