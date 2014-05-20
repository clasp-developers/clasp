/*
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved. 
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
 * System resource info for Solaris systems.
 *
 */


#ifndef MCA_SYSINFO_SOLARIS_H
#define MCA_SYSINFO_SOLARIS_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/sysinfo/sysinfo.h"


BEGIN_C_DECLS

/**
 * Globally exported variable
 */
OPAL_DECLSPEC extern const opal_sysinfo_base_component_t mca_sysinfo_solaris_component;

OPAL_DECLSPEC extern const opal_sysinfo_base_module_t opal_sysinfo_solaris_module;

END_C_DECLS
#endif /* MCA_SYSINFO_LINUX_H */
