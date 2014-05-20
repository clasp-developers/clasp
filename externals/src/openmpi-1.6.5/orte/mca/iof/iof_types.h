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
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */

#ifndef ORTE_IOF_TYPES_H
#define ORTE_IOF_TYPES_H

#include "orte_config.h"
#include "orte/types.h"


BEGIN_C_DECLS

/* Predefined tag values */
typedef uint8_t orte_iof_tag_t;
#define ORTE_IOF_TAG_T  OPAL_UINT8

#define ORTE_IOF_STDIN      0x01
#define ORTE_IOF_STDOUT     0x02
#define ORTE_IOF_STDERR     0x04
#define ORTE_IOF_STDDIAG    0x08
#define ORTE_IOF_STDOUTALL  0x0e

/* flow control flags */
#define ORTE_IOF_XON        0x10
#define ORTE_IOF_XOFF       0x20
/* tool requests */
#define ORTE_IOF_PULL       0x40
#define ORTE_IOF_CLOSE      0x80

END_C_DECLS

#endif /* ORTE_IOF_TYPES_H */
