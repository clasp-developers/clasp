/**
  Copyright (c) 2011 Mellanox Technologies. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */

#ifndef MCA_COLL_FCA_DEBUG_H
#define MCA_COLL_FCA_DEBUG_H
#pragma GCC system_header

#ifdef __BASE_FILE__
#define __FCA_FILE__ __BASE_FILE__
#else
#define __FCA_FILE__ __FILE__
#endif

#define FCA_VERBOSE(level, format, ...) \
    opal_output_verbose(level, mca_coll_fca_output, "%s:%d - %s() " format, \
                        __FCA_FILE__, __LINE__, __FUNCTION__, ## __VA_ARGS__)

#define FCA_ERROR(format, ... ) \
    opal_output_verbose(0, mca_coll_fca_output, "Error: %s:%d - %s() " format, \
                        __FCA_FILE__, __LINE__, __FUNCTION__, ## __VA_ARGS__)


#define FCA_MODULE_VERBOSE(fca_module, level, format, ...) \
        FCA_VERBOSE(level, "[%p:%d] " format, (void*)(fca_module)->comm, (fca_module)->rank, ## __VA_ARGS__)

extern int mca_coll_fca_output;

#endif
