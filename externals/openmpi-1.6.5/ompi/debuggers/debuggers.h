/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c)      2012 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * MPI portion of debugger support
 */

#ifndef OMPI_DEBUGGERS_H
#define OMPI_DEBUGGERS_H

#include "ompi_config.h"

BEGIN_C_DECLS

/**
 * Wait for a debugger if asked.
 */
extern void ompi_wait_for_debugger(void);

/**
 * Notify a debugger that we're about to abort
 */
extern void ompi_debugger_notify_abort(char *string);

/**
 * Breakpoint function for parallel debuggers.
 * This function is also defined in orterun for the starter.
 * It should never conflict with this one
 */
OMPI_DECLSPEC void *MPIR_Breakpoint(void);

END_C_DECLS

#endif /* OMPI_DEBUGGERS_H */
