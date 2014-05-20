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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stddef.h>

#include "opal/util/arch.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_convertor_internal.h"
#include "opal/mca/base/mca_base_param.h"

/* by default the debuging is turned off */
int opal_datatype_dfd = -1;
int opal_unpack_debug = 0;
int opal_pack_debug = 0;
int opal_position_debug = 0;
int opal_copy_debug = 0;

uint32_t opal_local_arch = 0xFFFFFFFF;

/* Using this macro implies that at this point _all_ informations needed
 * to fill up the datatype are known.
 * We fill all the static information, the pointer to desc.desc is setup
 * into an array, which is initialized at runtime.
 * Everything is constant.
 */
OPAL_DECLSPEC const opal_datatype_t opal_datatype_empty =       OPAL_DATATYPE_INITIALIZER_EMPTY(0);

OPAL_DECLSPEC const opal_datatype_t opal_datatype_loop =        OPAL_DATATYPE_INITIALIZER_LOOP(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_end_loop =    OPAL_DATATYPE_INITIALIZER_END_LOOP(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_lb =          OPAL_DATATYPE_INITIALIZER_LB(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_ub =          OPAL_DATATYPE_INITIALIZER_UB(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_int1 =        OPAL_DATATYPE_INITIALIZER_INT1(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_int2 =        OPAL_DATATYPE_INITIALIZER_INT2(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_int4 =        OPAL_DATATYPE_INITIALIZER_INT4(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_int8 =        OPAL_DATATYPE_INITIALIZER_INT8(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_int16 =       OPAL_DATATYPE_INITIALIZER_INT16(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_uint1 =       OPAL_DATATYPE_INITIALIZER_UINT1(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_uint2 =       OPAL_DATATYPE_INITIALIZER_UINT2(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_uint4 =       OPAL_DATATYPE_INITIALIZER_UINT4(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_uint8 =       OPAL_DATATYPE_INITIALIZER_UINT8(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_uint16 =      OPAL_DATATYPE_INITIALIZER_UINT16(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_float2 =      OPAL_DATATYPE_INITIALIZER_FLOAT2(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_float4 =      OPAL_DATATYPE_INITIALIZER_FLOAT4(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_float8 =      OPAL_DATATYPE_INITIALIZER_FLOAT8(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_float12 =     OPAL_DATATYPE_INITIALIZER_FLOAT12(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_float16 =     OPAL_DATATYPE_INITIALIZER_FLOAT16(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_complex8 =    OPAL_DATATYPE_INITIALIZER_COMPLEX8(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_complex16 =   OPAL_DATATYPE_INITIALIZER_COMPLEX16(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_complex32 =   OPAL_DATATYPE_INITIALIZER_COMPLEX32(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_bool =        OPAL_DATATYPE_INITIALIZER_BOOL(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_wchar =       OPAL_DATATYPE_INITIALIZER_WCHAR(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_unavailable = OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED(UNAVAILABLE, 0);

OPAL_DECLSPEC dt_elem_desc_t opal_datatype_predefined_elem_desc[2 * OPAL_DATATYPE_MAX_PREDEFINED];

/* NOTE: The order of this array *MUST* match the order in opal_datatype_basicDatatypes */
OPAL_DECLSPEC const size_t opal_datatype_local_sizes[OPAL_DATATYPE_MAX_PREDEFINED] =
{
    0,
    0,
    0,
    0,
    1,   /* sizeof (int8_t) */
    2,   /* sizeof (int16_t) */
    4,   /* sizeof (int32_t) */
    8,   /* sizeof (int64_t) */
    16,  /* sizeof (int128_t) */
    1,   /* sizeof (uint8_t) */
    2,   /* sizeof (uint16_t) */
    4,   /* sizeof (uint32_t) */
    8,   /* sizeof (uint64_t) */
    16,  /* sizeof (uint128_t) */
    2,   /* sizeof (float2) */
    4,   /* sizeof (float4) */
    8,   /* sizeof (float8) */
    12,  /* sizeof (float12) */
    16,  /* sizeof (float16) */
    8,   /* 2 * sizeof(float4) */
    16,  /* 2 * sizeof(float8) */
    32,  /* 2 * sizeof(float16) */
    sizeof (_Bool),
    sizeof (wchar_t),
    0    /* unavailable */
};

/*
 * NOTE: The order of this array *MUST* match what is listed in datatype.h
 */
OPAL_DECLSPEC const opal_datatype_t* opal_datatype_basicDatatypes[OPAL_DATATYPE_MAX_PREDEFINED] = {
    &opal_datatype_loop,
    &opal_datatype_end_loop,
    &opal_datatype_lb,
    &opal_datatype_ub,
    &opal_datatype_int1,
    &opal_datatype_int2,
    &opal_datatype_int4,
    &opal_datatype_int8,
    &opal_datatype_int16,       /* Yes, double-machine word integers are available */
    &opal_datatype_uint1,
    &opal_datatype_uint2,
    &opal_datatype_uint4,
    &opal_datatype_uint8,
    &opal_datatype_uint16,      /* Yes, double-machine word integers are available */
    &opal_datatype_float2,
    &opal_datatype_float4,
    &opal_datatype_float8,
    &opal_datatype_float12,
    &opal_datatype_float16,
    &opal_datatype_complex8,
    &opal_datatype_complex16,
    &opal_datatype_complex32,
    &opal_datatype_bool,
    &opal_datatype_wchar,
    &opal_datatype_unavailable
};


int opal_datatype_register_params(void)
{
#if OPAL_ENABLE_DEBUG
    mca_base_param_reg_int_name( "mpi", "ddt_unpack_debug",
                                 "Whether to output debugging information in the ddt unpack functions (nonzero = enabled)",
                                 false, false,
                                 opal_unpack_debug, &opal_unpack_debug );
    mca_base_param_reg_int_name( "mpi", "ddt_pack_debug",
                                 "Whether to output debugging information in the ddt pack functions (nonzero = enabled)",
                                 false, false,
                                 opal_pack_debug, &opal_pack_debug );
    mca_base_param_reg_int_name( "mpi", "ddt_position_debug",
                                 "Non zero lead to output generated by the datatype position functions",
                                 false, false, 0, &opal_position_debug );

    mca_base_param_reg_int_name( "mpi", "ddt_copy_debug",
                                 "Whether to output debugging information in the ddt copy functions (nonzero = enabled)",
                                 false, false,
                                 opal_copy_debug, &opal_copy_debug );
#endif /* OPAL_ENABLE_DEBUG */
    return OPAL_SUCCESS;
}


int32_t opal_datatype_init( void )
{
    const opal_datatype_t* datatype;
    int32_t i;

    opal_arch_compute_local_id( &opal_local_arch );

    for( i = OPAL_DATATYPE_FIRST_TYPE; i < OPAL_DATATYPE_MAX_PREDEFINED; i++ ) {
        datatype = opal_datatype_basicDatatypes[i];

        /* All of the predefined OPAL types don't have any GAPS! */
        datatype->desc.desc[0].elem.common.flags = OPAL_DATATYPE_FLAG_PREDEFINED |
                                                   OPAL_DATATYPE_FLAG_DATA |
                                                   OPAL_DATATYPE_FLAG_CONTIGUOUS |
                                                   OPAL_DATATYPE_FLAG_NO_GAPS;
        datatype->desc.desc[0].elem.common.type  = i;
        /* datatype->desc.desc[0].elem.blocklen XXX not set at the moment, it will be needed later */
        datatype->desc.desc[0].elem.count        = 1;
        datatype->desc.desc[0].elem.disp         = 0;
        datatype->desc.desc[0].elem.extent       = datatype->size;

        datatype->desc.desc[1].end_loop.common.flags    = 0;
        datatype->desc.desc[1].end_loop.common.type     = OPAL_DATATYPE_END_LOOP;
        datatype->desc.desc[1].end_loop.items           = 1;
        datatype->desc.desc[1].end_loop.first_elem_disp = datatype->desc.desc[0].elem.disp;
        datatype->desc.desc[1].end_loop.size            = datatype->size;
    }

#if !(OPAL_USE_FLOAT__COMPLEX && (SIZEOF_FLOAT__COMPLEX == 8)) && (SIZEOF_FLOAT == 4)
    datatype = opal_datatype_basicDatatypes[OPAL_DATATYPE_COMPLEX8];

    datatype->desc.desc[0].elem.common.type  = OPAL_DATATYPE_FLOAT4;
    datatype->desc.desc[0].elem.count        = 2;
    datatype->desc.desc[0].elem.extent       = SIZEOF_FLOAT;
#endif

#if !(OPAL_USE_DOUBLE__COMPLEX && (SIZEOF_DOUBLE__COMPLEX == 16)) && (SIZEOF_DOUBLE == 8)
    datatype = opal_datatype_basicDatatypes[OPAL_DATATYPE_COMPLEX16];

    datatype->desc.desc[0].elem.common.type  = OPAL_DATATYPE_FLOAT8;
    datatype->desc.desc[0].elem.count        = 2;
    datatype->desc.desc[0].elem.extent       = SIZEOF_DOUBLE;
#endif

#if !(OPAL_USE_LONG_DOUBLE__COMPLEX && (SIZEOF_LONG_DOUBLE__COMPLEX == 32)) && (HAVE_LONG_DOUBLE && (SIZEOF_LONG_DOUBLE == 16))
    datatype = opal_datatype_basicDatatypes[OPAL_DATATYPE_COMPLEX32];

    datatype->desc.desc[0].elem.common.type  = OPAL_DATATYPE_FLOAT16;
    datatype->desc.desc[0].elem.count        = 2;
    datatype->desc.desc[0].elem.extent       = SIZEOF_LONG_DOUBLE;
#endif

    return OPAL_SUCCESS;
}


int32_t opal_datatype_finalize( void )
{
    /* As the synonyms are just copies of the internal data we should not free them.
     * Anyway they are over the limit of OPAL_DATATYPE_MAX_PREDEFINED so they will never get freed.
     */

    /* As they are statically allocated they cannot be released. But we
     * can call OBJ_DESTRUCT, just to free all internally allocated ressources.
     */
#if defined(VERBOSE)
    if( opal_datatype_dfd != -1 )
        opal_output_close( opal_datatype_dfd );
    opal_datatype_dfd = -1;
#endif /* VERBOSE */

    /* clear all master convertors */
    opal_convertor_destroy_masters();

    return OPAL_SUCCESS;
}

#if OPAL_ENABLE_DEBUG
/*
 * Set a breakpoint to this function in your favorite debugger
 * to make it stop on all pack and unpack errors.
 */
int opal_datatype_safeguard_pointer_debug_breakpoint( const void* actual_ptr, int length,
                                                      const void* initial_ptr,
                                                      const opal_datatype_t* pData,
                                                      int count )
{
    return 0;
}
#endif  /* OPAL_ENABLE_DEBUG */
