/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stddef.h>
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

#include "opal/util/arch.h"

#include "opal/types.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "opal/datatype/opal_datatype_checksum.h"
#include "opal/datatype/opal_convertor_internal.h"


/*
 * Long-Term TODO:
 * We need a better way for the upper layer to convert
 * multiple, consecutive struct-types, e.g. float_int.
 * In the current design, the copy_float_heterogeneous and copy_float_heterogeneous
 * functions would be called 2*count times in total.
 * This is a big performance hit for a structure types.
 *
 * A better way would be to have a conversion registration functionality.
 */

static inline void
opal_dt_swap_bytes(void *to_p, const void *from_p, const size_t size)
{
    size_t i;
    size_t back_i = size - 1;
    uint8_t *to = (uint8_t*) to_p;
    uint8_t *from = (uint8_t*) from_p;
    for (i = 0 ; i < size ; i++, back_i--) {
        to[back_i] = from[i];
    }
}


#define COPY_TYPE_HETEROGENEOUS( TYPENAME, TYPE )                                         \
static int32_t                                                                            \
copy_##TYPENAME##_heterogeneous(opal_convertor_t *pConvertor, uint32_t count,             \
                                const char* from, size_t from_len, OPAL_PTRDIFF_TYPE from_extent, \
                                char* to, size_t to_length, OPAL_PTRDIFF_TYPE to_extent,          \
                                OPAL_PTRDIFF_TYPE *advance)             \
{                                                                       \
    uint32_t i;                                                         \
                                                                        \
    datatype_check( #TYPE, sizeof(TYPE), sizeof(TYPE), &count,          \
                   from, from_len, from_extent,                         \
                   to, to_length, to_extent);                           \
                                                                        \
    if ((pConvertor->remoteArch & OPAL_ARCH_ISBIGENDIAN) !=             \
        (opal_local_arch & OPAL_ARCH_ISBIGENDIAN)) {                    \
        for( i = 0; i < count; i++ ) {                                  \
            opal_dt_swap_bytes(to, from, sizeof(TYPE));                 \
            to += to_extent;                                            \
            from += from_extent;                                        \
        }                                                               \
    } else if ((OPAL_PTRDIFF_TYPE)sizeof(TYPE) == to_extent &&          \
               (OPAL_PTRDIFF_TYPE)sizeof(TYPE) == from_extent) {        \
         MEMCPY( to, from, count * sizeof(TYPE) );                      \
    } else {                                                            \
         /* source or destination are non-contigous */                  \
         for( i = 0; i < count; i++ ) {                                 \
             MEMCPY( to, from, sizeof(TYPE) );                          \
             to += to_extent;                                           \
             from += from_extent;                                       \
         }                                                              \
    }                                                                   \
    *advance = count * from_extent;                                     \
    return count;                                                       \
}


#define COPY_2TYPE_HETEROGENEOUS( TYPENAME, TYPE1, TYPE2 )              \
static int32_t                                                          \
copy_##TYPENAME##_heterogeneous(opal_convertor_t *pConvertor, uint32_t count, \
                                const char* from, uint32_t from_len, OPAL_PTRDIFF_TYPE from_extent, \
                                char* to, uint32_t to_length, OPAL_PTRDIFF_TYPE to_extent, \
                                OPAL_PTRDIFF_TYPE *advance)             \
{                                                                       \
    uint32_t i;                                                         \
                                                                        \
    datatype_check( #TYPENAME, sizeof(TYPE1) + sizeof(TYPE2),           \
                   sizeof(TYPE1) + sizeof(TYPE2), &count,               \
                   from, from_len, from_extent,                         \
                   to, to_length, to_extent);                           \
                                                                        \
    if ((pConvertor->remoteArch & OPAL_ARCH_ISBIGENDIAN) !=             \
        (opal_local_arch & OPAL_ARCH_ISBIGENDIAN)) {                    \
        /* source and destination are different endianness */           \
        for( i = 0; i < count; i++ ) {                                  \
            TYPE1* to_1, *from_1;                                       \
            TYPE2* to_2, *from_2;                                       \
            to_1 = (TYPE1*) to; from_1 = (TYPE1*) from;                 \
            opal_dt_swap_bytes(to_1, from_1, sizeof(TYPE1));            \
            to_2 = (TYPE2*) (to_1 + 1); from_2 = (TYPE2*) (from_1 + 1); \
            opal_dt_swap_bytes(to_2, from_2, sizeof(TYPE2));            \
            to += to_extent;                                            \
            from += from_extent;                                        \
        }                                                               \
    } else if ((OPAL_PTRDIFF_TYPE)(sizeof(TYPE1) + sizeof(TYPE2)) == to_extent &&   \
               (OPAL_PTRDIFF_TYPE)(sizeof(TYPE1) + sizeof(TYPE2)) == from_extent) { \
        /* source and destination are contigous */                      \
        MEMCPY( to, from, count * (sizeof(TYPE1) + sizeof(TYPE2)) );    \
    } else {                                                            \
        /* source or destination are non-contigous */                   \
        for( i = 0; i < count; i++ ) {                                  \
            MEMCPY( to, from, sizeof(TYPE1) + sizeof(TYPE2) );          \
            to += to_extent;                                            \
            from += from_extent;                                        \
        }                                                               \
    }                                                                   \
    *advance = count * from_extent;                                     \
    return count;                                                       \
}


static inline void
datatype_check(char *type, size_t local_size, size_t remote_size, uint32_t *count,
               const char* from, size_t from_len, OPAL_PTRDIFF_TYPE from_extent,
               char* to, size_t to_len, OPAL_PTRDIFF_TYPE to_extent)
{
    /* make sure the remote buffer is large enough to hold the data */
    if( (remote_size * *count) > from_len ) {
        *count = (uint32_t)(from_len / remote_size);
        if( (*count * remote_size) != from_len ) {
            DUMP( "oops should I keep this data somewhere (excedent %d bytes)?\n",
                  from_len - (*count * remote_size) );
        }
        DUMP( "correct: copy %s count %d from buffer %p with length %d to %p space %d\n",
              "char", *count, from, from_len, to, to_len );
    } else {
        DUMP( "         copy %s count %d from buffer %p with length %d to %p space %d\n",
              "char", *count, from, from_len, to, to_len );
    }
}

#define CXX_BOOL_COPY_LOOP(TYPE)                        \
    for( i = 0; i < count; i++ ) {                      \
        bool *to_real = (bool*) to;                     \
        *to_real = *((TYPE*) from) == 0 ? false : true; \
        to += to_extent;                                \
        from += from_extent;                            \
    }
static int32_t
copy_cxx_bool_heterogeneous(opal_convertor_t *pConvertor, uint32_t count,
                            const char* from, uint32_t from_len, OPAL_PTRDIFF_TYPE from_extent,
                            char* to, uint32_t to_length, OPAL_PTRDIFF_TYPE to_extent,
                            OPAL_PTRDIFF_TYPE *advance)
{
    uint32_t i;

    /* fix up the from extent */
    if ((pConvertor->remoteArch & OPAL_ARCH_BOOLISxx) !=
        (opal_local_arch & OPAL_ARCH_BOOLISxx)) {
        switch (pConvertor->remoteArch & OPAL_ARCH_BOOLISxx) {
        case OPAL_ARCH_BOOLIS8:
            from_extent = 1;
            break;
        case OPAL_ARCH_BOOLIS16:
            from_extent = 2;
            break;
        case OPAL_ARCH_BOOLIS32:
            from_extent = 4;
            break;
        }
    }

    datatype_check( "bool", sizeof(bool), sizeof(bool), &count,
                   from, from_len, from_extent,
                   to, to_length, to_extent);

    if ((to_extent != sizeof(bool) || from_extent != sizeof(bool)) ||
        ((pConvertor->remoteArch & OPAL_ARCH_BOOLISxx) !=
         (opal_local_arch & OPAL_ARCH_BOOLISxx))) {
        switch (pConvertor->remoteArch & OPAL_ARCH_BOOLISxx) {
        case OPAL_ARCH_BOOLIS8:
            CXX_BOOL_COPY_LOOP(int8_t);
            break;
        case OPAL_ARCH_BOOLIS16:
            CXX_BOOL_COPY_LOOP(int16_t);
            break;
        case OPAL_ARCH_BOOLIS32:
            CXX_BOOL_COPY_LOOP(int32_t);
            break;
        }
    } else {
        MEMCPY( to, from, count * sizeof(bool) );
    }

    *advance = count * from_extent;
    return count;
}


COPY_TYPE_HETEROGENEOUS(int1, int8_t)
COPY_TYPE_HETEROGENEOUS(int2, int16_t)
COPY_TYPE_HETEROGENEOUS(int4, int32_t)
#ifdef HAVE_INT64_T
COPY_TYPE_HETEROGENEOUS(int8, int64_t)
#else
#define copy_int8_heterogeneous NULL
#endif

#ifdef HAVE_INT128_T
COPY_TYPE_HETEROGENEOUS(int16, int128_t)
#else
#define copy_int16_heterogeneous NULL
#endif


#if SIZEOF_FLOAT == 2
COPY_TYPE_HETEROGENEOUS( float2, float )
#elif SIZEOF_DOUBLE == 2
COPY_TYPE_HETEROGENEOUS( float2, double )
#elif HAVE_LONG_DOUBLE && SIZEOF_LONG_DOUBLE == 2
COPY_TYPE_HETEROGENEOUS( float2, long double )
#else
/* #error No basic type for copy function for opal_datatype_float2 found */
#define copy_float2_heterogeneous NULL
#endif

#if SIZEOF_FLOAT == 4
COPY_TYPE_HETEROGENEOUS( float4, float )
#elif SIZEOF_DOUBLE == 4
COPY_TYPE_HETEROGENEOUS( float4, double )
#elif HAVE_LONG_DOUBLE && SIZEOF_LONG_DOUBLE == 4
COPY_TYPE_HETEROGENEOUS( float4, long double )
#else
/* #error No basic type for copy function for opal_datatype_float4 found */
#define copy_float4_heterogeneous NULL
#endif

#if SIZEOF_FLOAT == 8
COPY_TYPE_HETEROGENEOUS( float8, float )
#elif SIZEOF_DOUBLE == 8
COPY_TYPE_HETEROGENEOUS( float8, double )
#elif HAVE_LONG_DOUBLE && SIZEOF_LONG_DOUBLE == 8
COPY_TYPE_HETEROGENEOUS( float8, long double )
#else
/* #error No basic type for copy function for opal_datatype_float8 found */
#define copy_float8_heterogeneous NULL
#endif

#if SIZEOF_FLOAT == 12
COPY_TYPE_HETEROGENEOUS( float12, float )
#elif SIZEOF_DOUBLE == 12
COPY_TYPE_HETEROGENEOUS( float12, double )
#elif HAVE_LONG_DOUBLE && SIZEOF_LONG_DOUBLE == 12
COPY_TYPE_HETEROGENEOUS( float12, long double )
#else
/* #error No basic type for copy function for opal_datatype_float12 found */
#define copy_float12_heterogeneous NULL
#endif

#if SIZEOF_FLOAT == 16
COPY_TYPE_HETEROGENEOUS( float16, float )
#elif SIZEOF_DOUBLE == 8
COPY_TYPE_HETEROGENEOUS( float16, double )
#elif HAVE_LONG_DOUBLE && SIZEOF_LONG_DOUBLE == 16
COPY_TYPE_HETEROGENEOUS( float16, long double )
#else
/* #error No basic type for copy function for opal_datatype_float16 found */
#define copy_float16_heterogeneous NULL
#endif

COPY_TYPE_HETEROGENEOUS (wchar, wchar_t)

/* table of predefined copy functions - one for each MPI type */
conversion_fct_t opal_datatype_heterogeneous_copy_functions[OPAL_DATATYPE_MAX_PREDEFINED] = {
   NULL,                                                     /* OPAL_DATATYPE_LOOP        */
   NULL,                                                     /* OPAL_DATATYPE_END_LOOP    */
   NULL,                                                     /* OPAL_DATATYPE_LB          */
   NULL,                                                     /* OPAL_DATATYPE_UB          */
   (conversion_fct_t) copy_int1_heterogeneous,               /* OPAL_DATATYPE_INT1        */
   (conversion_fct_t) copy_int2_heterogeneous,               /* OPAL_DATATYPE_INT2        */
   (conversion_fct_t) copy_int4_heterogeneous,               /* OPAL_DATATYPE_INT4        */
   (conversion_fct_t) copy_int8_heterogeneous,               /* OPAL_DATATYPE_INT8        */
   (conversion_fct_t) copy_int16_heterogeneous,              /* OPAL_DATATYPE_INT16       */
   (conversion_fct_t) copy_int1_heterogeneous,               /* OPAL_DATATYPE_UINT1       */
   (conversion_fct_t) copy_int2_heterogeneous,               /* OPAL_DATATYPE_UINT2       */
   (conversion_fct_t) copy_int4_heterogeneous,               /* OPAL_DATATYPE_UINT4       */
   (conversion_fct_t) copy_int8_heterogeneous,               /* OPAL_DATATYPE_UINT8       */
   (conversion_fct_t) copy_int16_heterogeneous,              /* OPAL_DATATYPE_UINT16      */
   (conversion_fct_t) copy_float2_heterogeneous,             /* OPAL_DATATYPE_FLOAT2      */
   (conversion_fct_t) copy_float4_heterogeneous,             /* OPAL_DATATYPE_FLOAT4      */
   (conversion_fct_t) copy_float8_heterogeneous,             /* OPAL_DATATYPE_FLOAT8      */
   (conversion_fct_t) copy_float12_heterogeneous,            /* OPAL_DATATYPE_FLOAT12     */
   (conversion_fct_t) copy_float16_heterogeneous,            /* OPAL_DATATYPE_FLOAT16     */
   (conversion_fct_t) copy_cxx_bool_heterogeneous,           /* OPAL_DATATYPE_BOOL        */
   (conversion_fct_t) copy_wchar_heterogeneous,              /* OPAL_DATATYPE_WCHAR       */
   NULL,                                                     /* OPAL_DATATYPE_UNAVAILABLE */
};
