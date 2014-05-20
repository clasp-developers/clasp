/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 */

#ifndef OPAL_MCA_HWLOC_H
#define OPAL_MCA_HWLOC_H

#include "opal_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

BEGIN_C_DECLS

#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
typedef unsigned char u_char;
typedef unsigned short u_short;
#endif

/**
 * Structure for hwloc components.
 */
struct opal_hwloc_base_component_2_0_0_t {
    /** MCA base component */
    mca_base_component_t base_version;
    /** MCA base data */
    mca_base_component_data_t base_data;
};

/**
 * Convenience typedef
 */
typedef struct opal_hwloc_base_component_2_0_0_t opal_hwloc_base_component_2_0_0_t;
typedef struct opal_hwloc_base_component_2_0_0_t opal_hwloc_component_t;

/**
 * Macro for use in components that are of type hwloc
 */
#define OPAL_HWLOC_BASE_VERSION_2_0_0 \
    MCA_BASE_VERSION_2_0_0, \
    "hwloc", 2, 0, 0

END_C_DECLS

/* include implementation to call */
#if OPAL_HAVE_HWLOC
#include MCA_hwloc_IMPLEMENTATION_HEADER

OPAL_DECLSPEC extern hwloc_topology_t opal_hwloc_topology;
#endif

#endif /* OPAL_HWLOC_H_ */
