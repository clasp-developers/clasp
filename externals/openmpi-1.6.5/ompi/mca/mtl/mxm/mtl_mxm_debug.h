/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MTL_MXM_DEBUG_H
#define MTL_MXM_DEBUG_H
#pragma GCC system_header

#ifdef __BASE_FILE__
#define __MXM_FILE__ __BASE_FILE__
#else
#define __MXM_FILE__ __FILE__
#endif

#define MXM_VERBOSE(level, format, ...) \
    opal_output_verbose(level, mca_mtl_mxm_output, "%s:%d - %s() " format, \
                        __MXM_FILE__, __LINE__, __FUNCTION__, ## __VA_ARGS__)

#define MXM_ERROR(format, ... ) \
    opal_output_verbose(0, mca_mtl_mxm_output, "Error: %s:%d - %s() " format, \
                        __MXM_FILE__, __LINE__, __FUNCTION__, ## __VA_ARGS__)


#define MXM_MODULE_VERBOSE(mxm_module, level, format, ...) \
        MXM_VERBOSE(level, "[%d] " format, (mxm_module)->rank, ## __VA_ARGS__)

extern int mca_mtl_mxm_output;

#endif
