/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stddef.h>
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include <stdlib.h>

#include "opal/prefetch.h"
#include "opal/util/output.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "opal/datatype/opal_datatype_checksum.h"


#if OPAL_ENABLE_DEBUG
extern int opal_copy_debug;
#define DO_DEBUG(INST)  if( opal_copy_debug ) { INST }
#else
#define DO_DEBUG(INST)
#endif  /* OPAL_ENABLE_DEBUG */

static size_t opal_datatype_memop_block_size = 128 * 1024;

/**
 * Non overlapping memory regions
 */
#undef MEM_OP_NAME
#define MEM_OP_NAME  non_overlap
#undef MEM_OP
#define MEM_OP       MEMCPY
#include "opal_datatype_copy.h"

#define MEMMOVE(d, s, l)                                  \
    do {                                                  \
        if( (((d) < (s)) && (((d) + (l)) > (s))) ||       \
            (((s) < (d)) && (((s) + (l)) > (d))) ) {      \
            memmove( (d), (s), (l) );                     \
        } else {                                          \
            MEMCPY( (d), (s), (l) );                      \
        }                                                 \
    } while (0)

/**
 * Overlapping memory regions
 */
#undef MEM_OP_NAME
#define MEM_OP_NAME  overlap
#undef MEM_OP
#define MEM_OP       MEMMOVE
#include "opal_datatype_copy.h"

int32_t opal_datatype_copy_content_same_ddt( const opal_datatype_t* datatype, int32_t count,
                                             char* destination_base, char* source_base )
{
    OPAL_PTRDIFF_TYPE extent;
    size_t iov_len_local;
    int32_t (*fct)( const opal_datatype_t*, int32_t, char*, char*);

    DO_DEBUG( opal_output( 0, "opal_datatype_copy_content_same_ddt( %p, %d, dst %p, src %p )\n",
                           (void*)datatype, count, destination_base, source_base ); );

    /* empty data ? then do nothing. This should normally be trapped
     * at a higher level.
     */
    if( 0 == count ) return 1;

    iov_len_local = count * datatype->size;
    /**
     * see discussion in coll_basic_reduce.c for the computation of extent when
     * count != 1. Short version of the story:
     * (true_extent + ((count - 1) * extent))
     */
    extent = (datatype->true_ub - datatype->true_lb) + (count - 1) * (datatype->ub - datatype->lb);

    fct = non_overlap_copy_content_same_ddt;
    if( destination_base < source_base ) {
        if( (destination_base + extent) > source_base ) {
            /* memmove */
            fct = overlap_copy_content_same_ddt;
        }
    } else {
        if( (source_base + extent) > destination_base ) {
            /* memmove */
            fct = overlap_copy_content_same_ddt;
        }
    }
    return fct( datatype, count, destination_base, source_base );
}

