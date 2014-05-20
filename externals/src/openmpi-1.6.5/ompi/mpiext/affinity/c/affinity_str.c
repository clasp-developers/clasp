/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * Simple routine to expose three things to the MPI process:
 *
 * 1. What processor(s) Open MPI bound this process to
 * 2. What processor(s) this process is bound to
 * 3. What processor(s) exist on this host
 * 
 * Note that 1 and 2 may be different!
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>

#include "opal/mca/paffinity/base/base.h"

#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/mpi/c/bindings.h"
#include "ompi/mpiext/affinity/mpiext_affinity_c.h"

static const char FUNC_NAME[] = "OMPI_Affinity";

static int get_rsrc_ompi_bound(char str[OMPI_AFFINITY_STRING_MAX]);
static int get_rsrc_current_binding(char str[OMPI_AFFINITY_STRING_MAX]);
static int get_rsrc_exists(char str[OMPI_AFFINITY_STRING_MAX]);
static int get_layout_ompi_bound(char str[OMPI_AFFINITY_STRING_MAX]);
static int get_layout_current_binding(char str[OMPI_AFFINITY_STRING_MAX]);
static int get_layout_exists(char str[OMPI_AFFINITY_STRING_MAX]);

int OMPI_Affinity_str(ompi_affinity_fmt_t fmt_type,
		      char ompi_bound[OMPI_AFFINITY_STRING_MAX],
                      char current_binding[OMPI_AFFINITY_STRING_MAX],
                      char exists[OMPI_AFFINITY_STRING_MAX])
{
    int ret;

    memset(ompi_bound, 0, sizeof(ompi_bound));
    memset(current_binding, 0, sizeof(current_binding));

    switch(fmt_type) {
    case OMPI_AFFINITY_RSRC_STRING_FMT:
	if (OPAL_SUCCESS != (ret = get_rsrc_ompi_bound(ompi_bound)) ||
	    OPAL_SUCCESS != (ret = get_rsrc_current_binding(current_binding)) ||
	    OPAL_SUCCESS != (ret = get_rsrc_exists(exists))) {
	    return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, ret, FUNC_NAME);
	}
	break;
    case OMPI_AFFINITY_LAYOUT_FMT:
	if (OPAL_SUCCESS != (ret = get_layout_ompi_bound(ompi_bound)) ||
	    OPAL_SUCCESS != (ret = get_layout_current_binding(current_binding)) ||
	    OPAL_SUCCESS != (ret = get_layout_exists(exists))) {
	    return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, ret, FUNC_NAME);
	}
	break;
    default:
	return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
    }

    return MPI_SUCCESS;
}

static int get_rsrc_ompi_bound(char str[OMPI_AFFINITY_STRING_MAX])
{
    int ret;
    opal_paffinity_base_cpu_set_t cset;

    /* If OMPI did not bind, indicate that */
    if (!opal_paffinity_base_bound) {
        const char tmp[] = "Open MPI did not bind this process";
        strncpy(str, tmp, OMPI_AFFINITY_STRING_MAX - 1);
        return OPAL_SUCCESS;
    }

    /* Find out what OMPI bound us to and prettyprint it */
    ret = 
        opal_paffinity_base_parse_binding(opal_paffinity_base_applied_binding,
                                          &cset);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    return opal_paffinity_base_cset2str(str, OMPI_AFFINITY_STRING_MAX, &cset);
}

static int get_rsrc_current_binding(char str[OMPI_AFFINITY_STRING_MAX])
{
    int ret, flag;
    opal_paffinity_base_cpu_set_t cset;

    /* Get our binding */
    ret = opal_paffinity_base_get(&cset);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    /* Are we bound anywhere? */
    OPAL_PAFFINITY_PROCESS_IS_BOUND(cset, &flag);
    if (!flag) {
        const char tmp[] = "Not bound (or bound to all available processors)";
        strncat(str, tmp, OMPI_AFFINITY_STRING_MAX - 1);
        return OPAL_SUCCESS;
    }

    return opal_paffinity_base_cset2str(str, OMPI_AFFINITY_STRING_MAX, &cset);
}


/* Prettyprint a list of all available processors */
static int get_rsrc_exists(char str[OMPI_AFFINITY_STRING_MAX])
{
    int ret, i, num_sockets, num_cores;
    char tmp[BUFSIZ];
    const int stmp = sizeof(tmp) - 1;

    str[0] = tmp[stmp] = '\0';

    /* Loop over the number of sockets in this machine */
    ret = opal_paffinity_base_get_socket_info(&num_sockets);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }
    for (i = 0; i < num_sockets; ++i) {
        if (i > 0) {
            strncat(str, ", ", OMPI_AFFINITY_STRING_MAX - strlen(str));
        }
        snprintf(tmp, stmp, "socket %d has ", i);
        strncat(str, tmp, OMPI_AFFINITY_STRING_MAX - strlen(str));

        /* Loop over the number of cores in this socket */
        ret = opal_paffinity_base_get_core_info(i, &num_cores);
        if (OPAL_SUCCESS != ret) {
            return ret;
        }
        if (1 == num_cores) {
            strncat(str, "1 core", OMPI_AFFINITY_STRING_MAX - strlen(str));
        } else {
            snprintf(tmp, stmp, "%d cores", num_cores);
            strncat(str, tmp, OMPI_AFFINITY_STRING_MAX - strlen(str));
        }
    }

    return OPAL_SUCCESS;
}

static int get_layout_ompi_bound(char str[OMPI_AFFINITY_STRING_MAX])
{
    int ret;
    opal_paffinity_base_cpu_set_t cset;

    /* If OMPI did not bind, indicate that */
    if (!opal_paffinity_base_bound) {
        const char tmp[] = "Open MPI did not bind this process";
        strncpy(str, tmp, OMPI_AFFINITY_STRING_MAX - 1);
        return OPAL_SUCCESS;
    }

    /* Find out what OMPI bound us to and prettyprint it */
    ret = 
        opal_paffinity_base_parse_binding(opal_paffinity_base_applied_binding,
                                          &cset);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    return opal_paffinity_base_cset2mapstr(str, OMPI_AFFINITY_STRING_MAX, &cset);
}

static int get_layout_current_binding(char str[OMPI_AFFINITY_STRING_MAX])
{
    int ret = OPAL_SUCCESS, flag;
    opal_paffinity_base_cpu_set_t cset;

    /* Get our binding */
    ret = opal_paffinity_base_get(&cset);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    /* Are we bound anywhere? */
    OPAL_PAFFINITY_PROCESS_IS_BOUND(cset, &flag);
    if (!flag) {
        const char tmp[] = "Not bound (or bound to all available processors)";
        strncat(str, tmp, OMPI_AFFINITY_STRING_MAX - 1);
        return OPAL_SUCCESS;
    }

    return opal_paffinity_base_cset2mapstr(str, OMPI_AFFINITY_STRING_MAX, &cset);
}

/* Prettyprint a list of all available processors in layout format*/
static int get_layout_exists(char str[OMPI_AFFINITY_STRING_MAX])
{
    int ret, i, j, num_sockets, num_cores;
    int len = OMPI_AFFINITY_STRING_MAX;

    str[0] = '\0';

    /* Loop over the number of sockets in this machine */
    ret = opal_paffinity_base_get_socket_info(&num_sockets);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }
    for (i = 0; i < num_sockets; ++i) {
	strncat(str, "[", len - strlen(str));
        /* Loop over the number of cores in this socket */
        ret = opal_paffinity_base_get_core_info(i, &num_cores);
        if (OPAL_SUCCESS != ret) {
            return ret;
        }
	for (j = 0; j < num_cores; j++) {
	    if (0 < j) {
		/* add space after first core is printed */
		strncat(str, " ", len - strlen(str));
	    }
	    
	    /* mark core exists */
	    strncat(str, ".", len - strlen(str));
	}
	strncat(str, "]", len - strlen(str));
    }

    return OPAL_SUCCESS;
}
