/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008	   Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2010	   Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/** @file **/

#ifndef OPAL_H
#define OPAL_H

#include "opal_config.h"

BEGIN_C_DECLS

/** version string of opal */
OPAL_DECLSPEC extern const char opal_version_string[];

/* profile flag */
OPAL_DECLSPEC extern bool opal_profile;
OPAL_DECLSPEC extern char *opal_profile_file;

/* Size of a cache line.  To be replaced with real hwloc info (in
   trunk/v1.5 and beyond, only), but for the moment, just move it here
   so that we can remove opal/include/sys/cache.h whose only purpose
   is life is to #define CACHE_LINE_SIZE (and has a conflict on
   NetBSD). */
OPAL_DECLSPEC extern int opal_cache_line_size;

/**
 * Initialize the OPAL layer, including the MCA system.
 *
 * @retval OPAL_SUCCESS Upon success.
 * @retval OPAL_ERROR Upon failure.
 *
 * \note If this function is called, opal_init_util() should *not* be
 * called.
 */
OPAL_DECLSPEC int opal_init(int* pargc, char*** pargv);

/**
 * Finalize the OPAL layer, including the MCA system. 
 *
 * @retval OPAL_SUCCESS Upon success.
 * @retval OPAL_ERROR Upon failure.
 *
 * \note If this function is called, opal_finalize_util() should *not*
 * be called.
 */
OPAL_DECLSPEC int opal_finalize(void);

/**
 * Initialize the OPAL layer, excluding the MCA system.
 *
 * @retval OPAL_SUCCESS Upon success.
 * @retval OPAL_ERROR Upon failure.
 *
 * \note If this function is called, opal_init() should *not*
 * be called.
 */
OPAL_DECLSPEC int opal_init_util(int* pargc, char*** pargv);

/**
 * Finalize the OPAL layer, excluding the MCA system. 
 *
 * @retval OPAL_SUCCESS Upon success.
 * @retval OPAL_ERROR Upon failure.
 *
 * \note If this function is called, opal_finalize() should *not*
 * be called.
 */
OPAL_DECLSPEC int opal_finalize_util(void);

/**
 * Internal function.  Do not call.
 */
OPAL_DECLSPEC int opal_register_params(void);

END_C_DECLS

#endif
