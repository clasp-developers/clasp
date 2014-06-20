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

#include "ompi_config.h"
#include "ompi/constants.h"
#include "common_portals.h"


#if OMPI_PORTALS_UTCP

#include "common_portals_utcp.c"

#elif OMPI_PORTALS_CRAYXT3

#include "common_portals_crayxt3.c"

#elif OMPI_PORTALS_CRAYXT3_MODEX
#include "common_portals_cray_xt_modex.c"

#else

#error "Unknown Portals library configuration"

#endif

int
ompi_common_portals_error_ptl_to_ompi(int ptl_error)
{
    int ret;

    switch (ptl_error) {
    case PTL_OK:
        ret = OMPI_SUCCESS;
        break;
    case PTL_AC_INDEX_INVALID:
        ret = OMPI_ERR_BAD_PARAM;
        break;
    case PTL_EQ_DROPPED:
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        break;
    case PTL_EQ_INVALID:
        ret = OMPI_ERR_BAD_PARAM;
        break;
    case PTL_FAIL:
        ret = OMPI_ERROR;
        break;
    case PTL_HANDLE_INVALID:
        ret = OMPI_ERR_BAD_PARAM;
        break;
    case PTL_IFACE_INVALID:
        ret = OMPI_ERR_BAD_PARAM;
        break;
    case PTL_MD_ILLEGAL:
        ret = OMPI_ERR_BAD_PARAM;
        break;
    case PTL_MD_INVALID:
        ret = OMPI_ERR_BAD_PARAM;
        break;
    case PTL_MD_IN_USE:
        ret = OMPI_ERR_RESOURCE_BUSY;
        break;
    case PTL_ME_INVALID:
        ret = OMPI_ERR_BAD_PARAM;
        break;
    case PTL_ME_IN_USE:
        ret = OMPI_ERR_RESOURCE_BUSY;
        break;
    case PTL_ME_LIST_TOO_LONG:
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        break;
    case PTL_NI_INVALID:
        ret = OMPI_ERR_BAD_PARAM;
        break;
    case PTL_NO_INIT:
        ret = OMPI_ERR_BAD_PARAM;
        break;
    case PTL_NO_SPACE:
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        break;
    case PTL_PID_INVALID:
        ret = OMPI_ERR_BAD_PARAM;
        break;
    case PTL_PROCESS_INVALID:
        ret = OMPI_ERR_BAD_PARAM;
        break;
    case PTL_PT_FULL:
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        break;
    case PTL_PT_INDEX_INVALID:
        ret = OMPI_ERR_BAD_PARAM;
        break;
    case PTL_SEGV:
        ret = OMPI_ERR_VALUE_OUT_OF_BOUNDS;
        break;
    case PTL_SR_INDEX_INVALID:
        ret = OMPI_ERR_BAD_PARAM;
        break;
#if !(OMPI_PORTALS_CRAYXT3 || OMPI_PORTALS_CRAYXT3_MODEX)
    case PTL_UNKNOWN_ERROR:
        ret = OMPI_ERROR;
        break;
#endif
    default:
        ret = OMPI_ERROR;
    }

    return ret;
}
