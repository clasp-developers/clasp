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
/**
 * @file
 *
 * Common types used in the maffinity framework
 */

#ifndef OPAL_MAFFINITY_TYPES_H
#define OPAL_MAFFINITY_TYPES_H

#include "opal_config.h"

#include <sys/types.h>

BEGIN_C_DECLS

/**
 * Struct used with opal_maffinity_base_module_set_fn_t.  It
 * describes a section of memory (starting address and length).
 * This is really the same thing as an iovec, but we include a
 * separate type for it for at least 2 reasons:
 *
 * 1. Some OS's iovec definitions are exceedingly lame (e.g.,
 * Solaris 9 has the length argument as an int, instead of a
 * size_t).
 *
 * 2. We reserve the right to expand/change this struct in the
 * future.
 */
struct opal_maffinity_base_segment_t {
    /** Starting address of segment */
    void *mbs_start_addr;
    /** Length of segment */
    size_t mbs_len;
};
/**
 * Convenience typedef
 */
typedef struct opal_maffinity_base_segment_t opal_maffinity_base_segment_t;

END_C_DECLS

#endif /* OPAL_MAFFINITY_TYPES_H */
