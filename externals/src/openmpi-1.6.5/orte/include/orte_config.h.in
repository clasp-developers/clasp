/* -*- c -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Function: - OS, CPU and compiler dependent configuration
 */

#ifndef ORTE_CONFIG_H
#define ORTE_CONFIG_H

#include "opal_config.h"

#define ORTE_IDENT_STRING OPAL_IDENT_STRING

#if defined(__WINDOWS__)

#  if defined(_USRDLL)    /* building shared libraries (.DLL) */
#    if defined(ORTE_EXPORTS)
#      define ORTE_DECLSPEC         __declspec(dllexport)
#      define ORTE_MODULE_DECLSPEC
#    else
#      define ORTE_DECLSPEC         __declspec(dllimport)
#      if defined(ORTE_MODULE_EXPORTS)
#        define ORTE_MODULE_DECLSPEC __declspec(dllexport)
#      else
#        define ORTE_MODULE_DECLSPEC __declspec(dllimport)
#      endif  /* defined(ORTE_MODULE_EXPORTS) */
#    endif  /* defined(ORTE_EXPORTS) */
#  else          /* building static library or external includes */
#    if defined(ORTE_IMPORTS)
#      define ORTE_DECLSPEC        __declspec(dllimport)
#    else
#      define ORTE_DECLSPEC
#    endif  /* defined(ORTE_IMPORTS) */
#    define ORTE_MODULE_DECLSPEC
#  endif

#else

#  if OPAL_C_HAVE_VISIBILITY
#    define ORTE_DECLSPEC           __opal_attribute_visibility__("default")
#    define ORTE_MODULE_DECLSPEC    __opal_attribute_visibility__("default")
#  else
#    define ORTE_DECLSPEC
#    define ORTE_MODULE_DECLSPEC
#  endif
#endif  /* defined(__WINDOWS__) */

#endif
