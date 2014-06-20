/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_INET_H
#define OMPI_INET_H

#include "opal_config.h"

#ifndef OMPI_WIN_COMPAT_H
#error This file is supposed to be included only from win_compat.h
#endif  /* OMPI_WIN_COMPAT_H */

BEGIN_C_DECLS

OPAL_DECLSPEC int ompi_inet_pton(int af, const char *src, void *dst);

OPAL_DECLSPEC const char *ompi_inet_ntop(int af, const void *src, char *dst, size_t size);

END_C_DECLS

#endif /* OMPI_INET_H */