/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * System resource info for Posix systems.
 *
 */


#ifndef MCA_SYSINFO_LINUX_H
#define MCA_SYSINFO_LINUX_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/sysinfo/sysinfo.h"


BEGIN_C_DECLS

/**
 * Globally exported variable
 */
OPAL_DECLSPEC extern const opal_sysinfo_base_component_t mca_sysinfo_linux_component;

OPAL_DECLSPEC extern const opal_sysinfo_base_module_t opal_sysinfo_linux_module;

END_C_DECLS
#endif /* MCA_SYSINFO_LINUX_H */
