/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * @file
 */

#ifndef OPAL_STACKTRACE_H
#define OPAL_STACKTRACE_H

#include "opal_config.h"

/**
 * Output the current stack trace (not including the call to this
 * function) to the stream indicated.
 */
OPAL_DECLSPEC void opal_stackframe_output(int stream);

/**
 * Here we register the opal_show_stackframe function for signals
 * passed to OpenMPI by the mpi_signal-parameter passed to mpirun
 * by the user.
 *
 *  @returnvalue OPAL_SUCCESS
 *  @returnvalue OPAL_ERR_BAD_PARAM if the value in the signal-list
 *    is not a valid signal-number
 *               
 */
OPAL_DECLSPEC int opal_util_register_stackhandlers (void);

#endif /* OPAL_STACKTRACE_H */
