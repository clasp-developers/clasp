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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/*
 * List of supported architectures
 */

#ifndef OPAL_SYS_ARCHITECTURE_H
#define OPAL_SYS_ARCHITECTURE_H

/* Architectures */
#define OMPI_UNSUPPORTED    0000
#define OMPI_WINDOWS        0001
#define OMPI_IA32           0010
#define OMPI_IA64           0020
#define OMPI_AMD64          0030
#define OMPI_ALPHA          0040
#define OMPI_POWERPC32      0050
#define OMPI_POWERPC64      0051
#define OMPI_SPARC          0060
#define OMPI_SPARCV9_32     0061
#define OMPI_SPARCV9_64     0062
#define OMPI_MIPS           0070
#define OMPI_ARM            0100

/* Formats */
#define OMPI_DEFAULT        1000  /* standard for given architecture */
#define OMPI_DARWIN         1001  /* Darwin / OS X on PowerPC */
#define OMPI_PPC_LINUX      1002  /* Linux on PowerPC */
#define OMPI_AIX            1003  /* AIX on Power / PowerPC */

#endif /* #ifndef OPAL_SYS_ARCHITECTURE_H */
