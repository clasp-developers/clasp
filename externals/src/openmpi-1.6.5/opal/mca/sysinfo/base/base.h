/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#ifndef OPAL_SYSINFO_BASE_H
#define OPAL_SYSINFO_BASE_H

#include "opal_config.h"

#include "opal/mca/sysinfo/sysinfo.h"

/*
 * Global functions for MCA overall sysinfo open and close
 */

BEGIN_C_DECLS

/**
 * Initialize the sysinfo MCA framework
 *
 * @retval OPAL_SUCCESS Upon success
 * @retval OPAL_ERROR Upon failure
 *
 * This must be the first function invoked in the sysinfo MCA
 * framework.  It initializes the sysinfo MCA framework, finds
 * and opens sysinfo components, etc.
 *
 * This function is invoked during opal_init().
 */
OPAL_DECLSPEC int opal_sysinfo_base_open(void);

/**
 * Close the sysinfo MCA framework
 *
 * @retval OPAL_SUCCESS Upon success
 * @retval OPAL_ERROR Upon failure
 *
 * This must be the last function invoked in the sysinfo MCA
 * framework.
 *
 * This function is invoked during opal_finalize().
 */
OPAL_DECLSPEC int opal_sysinfo_base_close(void);

/**
 * Select all available components.
 *
 * @return OPAL_SUCCESS Upon success.
 * @return OPAL_ERROR Upon other failure.
 *
 * At the end of this process, we'll have a list of all available
 * components. If the list is empty, that is okay too. All
 * available components will have their init function called.
 */
OPAL_DECLSPEC int opal_sysinfo_base_select(void);

OPAL_DECLSPEC extern int opal_sysinfo_base_output;
OPAL_DECLSPEC extern opal_list_t opal_sysinfo_base_components_opened;
OPAL_DECLSPEC extern opal_list_t opal_sysinfo_avail_modules;
OPAL_DECLSPEC extern bool opal_sysinfo_initialized;
OPAL_DECLSPEC extern bool opal_sysinfo_selected;

END_C_DECLS

#endif /* OPAL_SYSINFO_BASE_H */
