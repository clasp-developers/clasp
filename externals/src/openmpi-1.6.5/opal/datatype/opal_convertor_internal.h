/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#ifndef OPAL_CONVERTOR_INTERNAL_HAS_BEEN_INCLUDED
#define OPAL_CONVERTOR_INTERNAL_HAS_BEEN_INCLUDED

#include "opal_config.h"

#include <stddef.h>

#include "opal/constants.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_convertor.h"

BEGIN_C_DECLS

typedef int32_t (*conversion_fct_t)( opal_convertor_t* pConvertor, uint32_t count,
                                     const void* from, size_t from_len, OPAL_PTRDIFF_TYPE from_extent,
                                     void* to, size_t to_length, OPAL_PTRDIFF_TYPE to_extent,
                                     OPAL_PTRDIFF_TYPE *advance );

typedef struct opal_convertor_master_t {
    struct opal_convertor_master_t* next;
    uint32_t                        remote_arch;
    uint32_t                        flags;
    uint32_t                        hetero_mask;
    const size_t                    remote_sizes[OPAL_DATATYPE_MAX_PREDEFINED];
    conversion_fct_t*               pFunctions;   /**< the convertor functions pointer */
} opal_convertor_master_t;

/*
 * Find or create a new master convertor based on a specific architecture. The master
 * convertor hold all informations related to a defined architecture, such as the sizes
 * of the predefined data-types, the conversion functions, ...
 */
opal_convertor_master_t* opal_convertor_find_or_create_master( uint32_t remote_arch );

/*
 * Destroy all pending master convertors. This function is usually called when we
 * shutdown the data-type engine, once all convertors have been destroyed.
 */
void opal_convertor_destroy_masters( void );

END_C_DECLS

#endif  /* OPAL_CONVERTOR_INTERNAL_HAS_BEEN_INCLUDED */
