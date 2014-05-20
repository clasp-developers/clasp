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

#include "opal_config.h"

#include "opal/sys/atomic.h"
#include "opal/sys/architecture.h"

#if OPAL_ASSEMBLY_ARCH == OMPI_SPARC

#if OPAL_WANT_SMP_LOCKS

#define LOCKS_TABLE_SIZE 8
/* make sure to get into reasonably useful bits (so shift at least 5) */
#define FIND_LOCK(addr) (&(locks_table[(((unsigned long) addr) >> 8) &  \
                                       (LOCKS_TABLE_SIZE - 1)]))

/* have to fix if you change LOCKS_TABLE_SIZE */
static opal_atomic_lock_t locks_table[LOCKS_TABLE_SIZE] = {
    { { OPAL_ATOMIC_UNLOCKED } }, 
    { { OPAL_ATOMIC_UNLOCKED } },
    { { OPAL_ATOMIC_UNLOCKED } },
    { { OPAL_ATOMIC_UNLOCKED } },
    { { OPAL_ATOMIC_UNLOCKED } },
    { { OPAL_ATOMIC_UNLOCKED } },
    { { OPAL_ATOMIC_UNLOCKED } },
    { { OPAL_ATOMIC_UNLOCKED } }
};

# else /* OPAL_WANT_SMP_LOCKS */

#define LOCKS_TABLE_SIZE 1
#define FIND_LOCK(addr) (&(locks_table[0]))

static opal_atomic_lock_t locks_table[1] = { OPAL_ATOMIC_UNLOCKED };

#endif /* OPAL_WANT_SMP_LOCKS */


int32_t
opal_atomic_add_32(volatile int32_t *addr, int delta)
{
    int32_t ret;

    opal_atomic_lock(FIND_LOCK(addr));

    ret = (*addr += delta);

    opal_atomic_unlock(FIND_LOCK(addr));

    return ret;
}


int32_t
opal_atomic_sub_32(volatile int32_t *addr, int delta)
{
    int32_t ret;

    opal_atomic_lock(FIND_LOCK(addr));

    ret = (*addr -= delta);

    opal_atomic_unlock(FIND_LOCK(addr));

    return ret;
}


#endif /* OPAL_ASSEMBLY_ARCH == OMPI_SPARC32 */
