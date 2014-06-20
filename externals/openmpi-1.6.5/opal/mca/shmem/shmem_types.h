/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * Copyright (c) 2010-2011 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * shmem (shared memory backing facility) framework types, convenience macros,
 * etc.
 */

#ifndef OPAL_SHMEM_TYPES_H
#define OPAL_SHMEM_TYPES_H

#include "opal_config.h"

BEGIN_C_DECLS

/* ////////////////////////////////////////////////////////////////////////// */
/**
 * ds_buf: pointer to opal_shmem_ds_t typedef'd struct
 */

/**
 * flag indicating the state (valid/invalid) of the shmem data structure
 * 0x0* - reserved for non-internal flags
 */
#define OPAL_SHMEM_DS_FLAGS_VALID 0x01

/**
 * 0x1* - reserved for internal flags. that is, flags that will NOT be
 * propagated via ds_copy during inter-process information sharing.
 */

/**
 * masks out internal flags
 */
#define OPAL_SHMEM_DS_FLAGS_INTERNAL_MASK 0x0F

/**
 * invalid id value
 */
#define OPAL_SHMEM_DS_ID_INVALID -1

/**
 * macro that sets all bits in flags to 0
 */
#define OPAL_SHMEM_DS_RESET_FLAGS(ds_buf)                                      \
do {                                                                           \
    (ds_buf)->flags = 0x00;                                                    \
} while (0)

/**
 * sets valid bit in flags to 1
 */
#define OPAL_SHMEM_DS_SET_VALID(ds_buf)                                        \
do {                                                                           \
    (ds_buf)->flags |= OPAL_SHMEM_DS_FLAGS_VALID;                              \
} while (0)

#define OPAL_SHMEM_DS_SET_CREATOR(ds_buf)                                      \
do {                                                                           \
    (ds_buf)->flags |= OPAL_SHMEM_DS_FLAGS_CREATOR;                            \
} while (0)

/**
 * sets valid bit in flags to 0
 */
#define OPAL_SHMEM_DS_INVALIDATE(ds_buf)                                       \
do {                                                                           \
    (ds_buf)->flags &= ~OPAL_SHMEM_DS_FLAGS_VALID;                             \
} while (0)

/**
 * evaluates to 1 if the valid bit in flags is set to 1.  evaluates to 0
 * otherwise.
 */
#define OPAL_SHMEM_DS_IS_VALID(ds_buf)                                         \
    ( (ds_buf)->flags & OPAL_SHMEM_DS_FLAGS_VALID )

#define OPAL_SHMEM_DS_IS_CREATOR(ds_buf)                                       \
    ( (ds_buf)->flags & OPAL_SHMEM_DS_FLAGS_CREATOR )

/* ////////////////////////////////////////////////////////////////////////// */
typedef uint8_t opal_shmem_ds_flag_t;

/* shared memory segment header */
struct opal_shmem_seg_hdr_t {
    /* segment lock */
    opal_atomic_lock_t lock;
    /* pid of the segment creator */
    pid_t cpid;
};
typedef struct opal_shmem_seg_hdr_t opal_shmem_seg_hdr_t;

struct opal_shmem_ds_t {
    /* owner pid of the opal_shmem_ds_t */
    pid_t opid;
    /* state flags */
    opal_shmem_ds_flag_t flags;
    /* pid of the shared memory segment creator */
    pid_t seg_cpid;
    /* ds id */
    int seg_id;
    /* size of shared memory segment */
    size_t seg_size;
    /* path to backing store */
    char seg_name[OPAL_PATH_MAX];
    /* base address of shared memory segment */
    unsigned char *seg_base_addr;
};
typedef struct opal_shmem_ds_t opal_shmem_ds_t;

END_C_DECLS

#endif /* OPAL_SHMEM_TYPES_H */
