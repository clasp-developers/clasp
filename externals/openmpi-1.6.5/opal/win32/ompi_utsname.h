/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_UTSNAME_H
#define OMPI_UTSNAME_H

#include "opal_config.h"

#define OMPI_UTSNAME_LEN 64

struct utsname {
    char sysname[OMPI_UTSNAME_LEN];
    char nodename[OMPI_UTSNAME_LEN];
    char release[OMPI_UTSNAME_LEN];
    char version[OMPI_UTSNAME_LEN];
    char machine[OMPI_UTSNAME_LEN];
};

BEGIN_C_DECLS
    OPAL_DECLSPEC int uname(struct utsname *un);
END_C_DECLS

#endif /* oMPI_UTSNAME_H */
