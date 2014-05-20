/* 
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#ifndef ORTE_CONDITION_H
#define ORTE_CONDITION_H

#include "orte_config.h"

#include "opal/threads/condition.h"

BEGIN_C_DECLS

#define ORTE_CONDITION_WAIT(x, y)  opal_condition_wait(x, y)

#define ORTE_CONDITION_SIGNAL(x) opal_condition_signal(x)

#define ORTE_CONDITION_BROADCAST(x) opal_condition_broadcast(x)

END_C_DECLS

#endif

