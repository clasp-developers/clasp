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
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#define OMPI_BUILDING 0
#include "opal_config.h"

#include "opal/sys/atomic.h"

int
main(int argc, char *argv[])
{
#if OPAL_HAVE_ATOMIC_MEM_BARRIER

    /* there really isn't a great way to test that the barriers
       actually barrier, but at least make sure they don't kill the
       machine.*/

    opal_atomic_mb();
    opal_atomic_rmb();
    opal_atomic_wmb();

    return 0;
#else
    return 77;
#endif
}

