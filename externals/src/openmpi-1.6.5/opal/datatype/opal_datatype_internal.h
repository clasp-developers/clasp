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

#ifndef OPAL_DATATYPE_INTERNAL_H_HAS_BEEN_INCLUDED
#define OPAL_DATATYPE_INTERNAL_H_HAS_BEEN_INCLUDED

#include "opal_config.h"

#include <stddef.h>
#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

#if defined(VERBOSE)
#include "opal/util/output.h"

extern int opal_datatype_dfd;

#  define DDT_DUMP_STACK( PSTACK, STACK_POS, PDESC, NAME ) \
     opal_datatype_dump_stack( (PSTACK), (STACK_POS), (PDESC), (NAME) )
#  if defined(ACCEPT_C99)
#    define DUMP( ARGS... )          opal_output(opal_datatype_dfd, __VA_ARGS__)
#  else
#    if defined(__GNUC__) && !defined(__STDC__)
#      define DUMP(ARGS...)          opal_output( opal_datatype_dfd, ARGS)
#  else
static inline void DUMP( char* fmt, ... )
{
   va_list list;

   va_start( list, fmt );
   opal_output( opal_datatype_dfd, fmt, list );
   va_end( list );
}
#      define DUMP                   printf
#    endif  /* __GNUC__ && !__STDC__ */
#  endif  /* ACCEPT_C99 */
#else
#  define DDT_DUMP_STACK( PSTACK, STACK_POS, PDESC, NAME )
#  if defined(ACCEPT_C99)
#    define DUMP(ARGS...)
#  else
#    if defined(__GNUC__) && !defined(__STDC__)
#      define DUMP(ARGS...)
#    else
       /* If we do not compile with PGI, mark the parameter as unused */
#      if !defined(__PGI)
#        define __opal_attribute_unused_tmp__  __opal_attribute_unused__
#      else
#        define __opal_attribute_unused_tmp__
#      endif  
static inline void DUMP( char* fmt __opal_attribute_unused_tmp__, ... )
{
#if defined(__PGI)
           /* Some compilers complain if we have "..." arguments and no
              corresponding va_start() */
           va_list arglist;
           va_start(arglist, fmt);
           va_end(arglist);
#endif
}
#         undef __opal_attribute_unused_tmp__
#    endif  /* __GNUC__ && !__STDC__ */
#  endif  /* ACCEPT_C99 */
#endif  /* VERBOSE */


/*
 * There 3 types of predefined data types.
 * - the basic one composed by just one basic datatype which are
 *   definitively contiguous
 * - the derived ones where the same basic type is used multiple times.
 *   They should be most of the time contiguous.
 * - and finally the derived one where multiple basic types are used.
 *   Depending on the architecture they can be contiguous or not.
 *
 * At the OPAL-level we do not care from which language the datatype came from
 * (C, C++ or FORTRAN), we only focus on their internal representation in
 * the host memory.
 *
 * NOTE: This (and in ompi_datatype_internal.h) is the id-order to obey
 */
#define OPAL_DATATYPE_LOOP           0
#define OPAL_DATATYPE_END_LOOP       1
#define OPAL_DATATYPE_LB             2
#define OPAL_DATATYPE_UB             3
#define OPAL_DATATYPE_FIRST_TYPE     4 /* Number of first real type */
#define OPAL_DATATYPE_INT1           4
#define OPAL_DATATYPE_INT2           5
#define OPAL_DATATYPE_INT4           6
#define OPAL_DATATYPE_INT8           7
#define OPAL_DATATYPE_INT16          8
#define OPAL_DATATYPE_UINT1          9
#define OPAL_DATATYPE_UINT2          10
#define OPAL_DATATYPE_UINT4          11
#define OPAL_DATATYPE_UINT8          12
#define OPAL_DATATYPE_UINT16         13
#define OPAL_DATATYPE_FLOAT2         14
#define OPAL_DATATYPE_FLOAT4         15
#define OPAL_DATATYPE_FLOAT8         16
#define OPAL_DATATYPE_FLOAT12        17
#define OPAL_DATATYPE_FLOAT16        18
#define OPAL_DATATYPE_COMPLEX8       19
#define OPAL_DATATYPE_COMPLEX16      20
#define OPAL_DATATYPE_COMPLEX32      21
#define OPAL_DATATYPE_BOOL           22
#define OPAL_DATATYPE_WCHAR          23
#define OPAL_DATATYPE_UNAVAILABLE    24

#ifndef OPAL_DATATYPE_MAX_PREDEFINED
#define OPAL_DATATYPE_MAX_PREDEFINED 25
#endif

/*
 * If the number of basic datatype should change update
 * OPAL_DATATYPE_MAX_PREDEFINED in datatype.h
 */
#if OPAL_DATATYPE_MAX_PREDEFINED <= OPAL_DATATYPE_UNAVAILABLE
#error OPAL_DATATYPE_MAX_PREDEFINED should be updated to the next value after the OPAL_DATATYPE_UNAVAILABLE define
#endif

#define DT_INCREASE_STACK     8

BEGIN_C_DECLS

struct ddt_elem_id_description {
    uint16_t   flags;  /**< flags for the record */
    uint16_t   type;   /**< the basic data type id */
};
typedef struct ddt_elem_id_description ddt_elem_id_description;

/* the basic element. A data description is composed
 * by a set of basic elements.
 */
struct ddt_elem_desc {
    ddt_elem_id_description common;           /**< basic data description and flags */
    uint32_t                count;            /**< number of blocks */
    uint32_t                blocklen;         /**< number of elements on each block */
    OPAL_PTRDIFF_TYPE       extent;           /**< extent of each block (in bytes) */
    OPAL_PTRDIFF_TYPE       disp;             /**< displacement of the first block */
};
typedef struct ddt_elem_desc ddt_elem_desc_t;

struct ddt_loop_desc {
    ddt_elem_id_description common;           /**< basic data description and flags */
    uint32_t                loops;            /**< number of elements */
    uint32_t                items;            /**< number of items in the loop */
    size_t                  unused;           /**< not used right now */
    OPAL_PTRDIFF_TYPE       extent;           /**< extent of the whole loop */
};
typedef struct ddt_loop_desc ddt_loop_desc_t;

struct ddt_endloop_desc {
    ddt_elem_id_description common;           /**< basic data description and flags */
    uint32_t                items;            /**< number of elements */
    uint32_t                unused;           /**< not used right now */
    size_t                  size;             /**< real size of the data in the loop */
    OPAL_PTRDIFF_TYPE       first_elem_disp;  /**< the displacement of the first block in the loop */
};
typedef struct ddt_endloop_desc ddt_endloop_desc_t;

union dt_elem_desc {
    ddt_elem_desc_t    elem;
    ddt_loop_desc_t    loop;
    ddt_endloop_desc_t end_loop;
};

#define CREATE_LOOP_START( _place, _count, _items, _extent, _flags )           \
    do {                                                                       \
        (_place)->loop.common.type   = OPAL_DATATYPE_LOOP;                     \
        (_place)->loop.common.flags  = (_flags) & ~OPAL_DATATYPE_FLAG_DATA;    \
        (_place)->loop.loops         = (_count);                               \
        (_place)->loop.items         = (_items);                               \
        (_place)->loop.extent        = (_extent);                              \
        (_place)->loop.unused        = -1;                                     \
    } while(0)

#define CREATE_LOOP_END( _place, _items, _first_item_disp, _size, _flags )     \
    do {                                                                       \
        (_place)->end_loop.common.type = OPAL_DATATYPE_END_LOOP;               \
        (_place)->end_loop.common.flags = (_flags) & ~OPAL_DATATYPE_FLAG_DATA; \
        (_place)->end_loop.items = (_items);                                   \
        (_place)->end_loop.first_elem_disp = (_first_item_disp);               \
        (_place)->end_loop.size = (_size);  /* the size inside the loop */     \
        (_place)->end_loop.unused = -1;                                        \
    } while(0)

#define CREATE_ELEM( _place, _type, _flags, _count, _disp, _extent )           \
    do {                                                                       \
        (_place)->elem.common.flags = (_flags) | OPAL_DATATYPE_FLAG_DATA;      \
        (_place)->elem.common.type  = (_type);                                 \
        (_place)->elem.count        = (_count);                                \
        (_place)->elem.disp         = (_disp);                                 \
        (_place)->elem.extent       = (_extent);                               \
        (_place)->elem.blocklen     = 1;                                       \
    } while(0)
/*
 * This array holds the descriptions desc.desc[2] of the predefined basic datatypes.
 */
OPAL_DECLSPEC extern union dt_elem_desc opal_datatype_predefined_elem_desc[2 * OPAL_DATATYPE_MAX_PREDEFINED];
struct opal_datatype_t;

/* Other fields starting after bdt_used (index of OPAL_DATATYPE_LOOP should be ONE) */
/*
 * NOTE: The order of initialization *MUST* match the order of the OPAL_DATATYPE_-numbers.
 * Unfortunateley, I don't get the preprocessor to replace
 *     OPAL_DATATYPE_INIT_BTYPES_ARRAY_ ## OPAL_DATATYPE ## NAME
 * into
 *     OPAL_DATATYPE_INIT_BTYPES_ARRAY_[0-21], then order and naming would _not_ matter....
 */
#define Z5    0, 0, 0, 0, 0
#define Z10   Z5, Z5
#define Z15   Z10, Z5
#define Z20   Z10, Z10
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_UNAVAILABLE { 0, }
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_LOOP        { 1, }
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_END_LOOP    { 0, 1, }
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_LB          { 0, 0, 1, }
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_UB          { 0, 0, 0, 1, }
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_INT1        { 0, 0, 0, 0, 1, }
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_INT2        { Z5, 1, }              /*5*/
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_INT4        { Z5, 0, 1, }
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_INT8        { Z5, 0, 0, 1, }
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_INT16       { Z5, 0, 0, 0, 1, }
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_UINT1       { Z5, 0, 0, 0, 0, 1, }
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_UINT2       { Z10, 1, }             /*10*/
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_UINT4       { Z10, 0, 1, }
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_UINT8       { Z10, 0, 0, 1, }
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_UINT16      { Z10, 0, 0, 0, 1, }
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_FLOAT2      { Z10, 0, 0, 0, 0, 1, }
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_FLOAT4      { Z15, 1, }             /*15*/
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_FLOAT8      { Z15, 0, 1, }
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_FLOAT12     { Z15, 0, 0, 1, }
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_FLOAT16     { Z15, 0, 0, 0, 1, }
#if HAVE_FLOAT__COMPLEX
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_COMPLEX8    { Z15, 0, 0, 0, 0, 1 }
#else
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_COMPLEX8    { Z15, 2 }    /* two floats */
#endif
#if HAVE_DOUBLE__COMPLEX
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_COMPLEX16   { Z15, 0, 0, 0, 0, 0, 1 }
#else
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_COMPLEX16   { Z15, 0, 2, }
#endif
#if HAVE_LONG_DOUBLE__COMPLEX
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_COMPLEX32   { Z15, 0, 0, 0, 0, 0, 0, 1 }
#else
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_COMPLEX32   { Z15, 0, 0, 0, 2, }
#endif
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_BOOL        { Z20, 0, 1, }          /*22*/
#define OPAL_DATATYPE_INIT_BTYPES_ARRAY_WCHAR       { Z20, 0, 0, 1, }

#define OPAL_DATATYPE_INIT_NAME(NAME) "OPAL_" _OPAL_DATATYPE_INIT_NAME(NAME)
#define _OPAL_DATATYPE_INIT_NAME(NAME)  #NAME

/*
 * Macro to initialize the main description for basic types, setting the pointer
 * into the array opal_datatype_predefined_type_desc, which is initialized at
 * runtime in opal_datatype_init(). Each basic type has two desc-elements....
 */
#define OPAL_DATATYPE_INIT_DESC_PREDEFINED(NAME)                                     \
    {                                                                                \
        1 /*length*/, 1/*used*/,                                                     \
        &(opal_datatype_predefined_elem_desc[2 * OPAL_DATATYPE_ ## NAME]) /*desc*/   \
    }
#define OPAL_DATATYPE_INIT_DESC_NULL  {0 /*length*/, 0/*used*/, NULL /*desc*/}

#define OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED( NAME, FLAGS )                   \
    {                                                                                \
        OPAL_OBJ_STATIC_INIT(opal_datatype_t),                                       \
        OPAL_DATATYPE_FLAG_UNAVAILABLE | OPAL_DATATYPE_FLAG_PREDEFINED | (FLAGS) /*flag*/, \
        OPAL_DATATYPE_ ## NAME /*id*/,                                               \
        0 /*bdt_used*/,                                                              \
        0 /*size*/,                                                                  \
        0 /*true_lb*/, 0 /*true_ub*/, 0 /*lb*/, 0 /*ub*/,                            \
        0 /*align*/,                                                                 \
        1 /*nbElems*/,                                                               \
        OPAL_DATATYPE_INIT_NAME(NAME), /*name*/                                      \
        OPAL_DATATYPE_INIT_DESC_PREDEFINED(UNAVAILABLE), /*desc*/                    \
        OPAL_DATATYPE_INIT_DESC_PREDEFINED(UNAVAILABLE), /*opt_desc*/                \
        OPAL_DATATYPE_INIT_BTYPES_ARRAY_UNAVAILABLE /*btypes*/                       \
    }

#define OPAL_DATATYPE_INITIALIZER_UNAVAILABLE( FLAGS )                               \
    OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED( UNAVAILABLE, (FLAGS) )

#define OPAL_DATATYPE_INITIALIZER_EMPTY( FLAGS )                                     \
    {                                                                                \
        OPAL_OBJ_STATIC_INIT(opal_datatype_t),                                       \
        OPAL_DATATYPE_FLAG_PREDEFINED | (FLAGS) /*flag*/,                            \
        0 /*id*/,                                                                    \
        0 /*bdt_used*/,                                                              \
        0 /*size*/,                                                                  \
        0 /*true_lb*/, 0 /*true_ub*/, 0 /*lb*/, 0 /*ub*/,                            \
        0 /*align*/,                                                                 \
        1 /*nbElems*/,                                                               \
        OPAL_DATATYPE_INIT_NAME(EMPTY), /*name*/                                     \
        OPAL_DATATYPE_INIT_DESC_NULL, /*desc*/                                       \
        OPAL_DATATYPE_INIT_DESC_NULL, /*opt_desc*/                                   \
        OPAL_DATATYPE_INIT_BTYPES_ARRAY_UNAVAILABLE /*btypes*/                       \
    }

#define OPAL_DATATYPE_INIT_BASIC_TYPE( TYPE, NAME, FLAGS )                           \
    {                                                                                \
        OPAL_OBJ_STATIC_INIT(opal_datatype_t),                                       \
        OPAL_DATATYPE_FLAG_PREDEFINED | (FLAGS) /*flag*/,                            \
        TYPE /*id*/,                                                                 \
        (((uint32_t)1)<<(TYPE)) /*bdt_used*/,                                        \
        0 /*size*/,                                                                  \
        0 /*true_lb*/, 0 /*true_ub*/, 0 /*lb*/, 0 /*ub*/,                            \
        0 /*align*/,                                                                 \
        1 /*nbElems*/,                                                               \
        OPAL_DATATYPE_INIT_NAME(NAME), /*name*/                                      \
        OPAL_DATATYPE_INIT_DESC_NULL, /*desc*/                                       \
        OPAL_DATATYPE_INIT_DESC_NULL, /*opt_desc*/                                   \
        OPAL_DATATYPE_INIT_BTYPES_ARRAY_ ## NAME /*btypes*/                          \
    }

#define OPAL_DATATYPE_INIT_BASIC_DATATYPE( TYPE, ALIGN, NAME, FLAGS )                \
    {                                                                                \
        OPAL_OBJ_STATIC_INIT(opal_datatype_t),                                       \
        OPAL_DATATYPE_FLAG_BASIC | (FLAGS) /*flag*/,                                 \
        OPAL_DATATYPE_ ## NAME /*id*/,                                               \
        (((uint32_t)1)<<(OPAL_DATATYPE_ ## NAME)) /*bdt_used*/,                      \
        sizeof(TYPE) /*size*/,                                                       \
        0 /*true_lb*/, sizeof(TYPE) /*true_ub*/, 0 /*lb*/, sizeof(TYPE) /*ub*/,      \
        (ALIGN) /*align*/,                                                           \
        1 /*nbElems*/,                                                               \
        OPAL_DATATYPE_INIT_NAME(NAME) /*name*/,                                      \
        OPAL_DATATYPE_INIT_DESC_PREDEFINED(NAME) /*desc*/,                           \
        OPAL_DATATYPE_INIT_DESC_PREDEFINED(NAME) /*opt_desc*/,                       \
        OPAL_DATATYPE_INIT_BTYPES_ARRAY_ ## NAME /*btypes*/                          \
    }

#define OPAL_DATATYPE_INITIALIZER_LOOP(FLAGS)       OPAL_DATATYPE_INIT_BASIC_TYPE( OPAL_DATATYPE_LOOP, LOOP, FLAGS )
#define OPAL_DATATYPE_INITIALIZER_END_LOOP(FLAGS)   OPAL_DATATYPE_INIT_BASIC_TYPE( OPAL_DATATYPE_END_LOOP, END_LOOP, FLAGS )
#define OPAL_DATATYPE_INITIALIZER_LB(FLAGS)         OPAL_DATATYPE_INIT_BASIC_TYPE( OPAL_DATATYPE_LB, LB, FLAGS )
#define OPAL_DATATYPE_INITIALIZER_UB(FLAGS)         OPAL_DATATYPE_INIT_BASIC_TYPE( OPAL_DATATYPE_UB, UB, FLAGS )
#define OPAL_DATATYPE_INITIALIZER_INT1(FLAGS)       OPAL_DATATYPE_INIT_BASIC_DATATYPE( int8_t, OPAL_ALIGNMENT_INT8, INT1, FLAGS )
#define OPAL_DATATYPE_INITIALIZER_INT2(FLAGS)       OPAL_DATATYPE_INIT_BASIC_DATATYPE( int16_t, OPAL_ALIGNMENT_INT16, INT2, FLAGS )
#define OPAL_DATATYPE_INITIALIZER_INT4(FLAGS)       OPAL_DATATYPE_INIT_BASIC_DATATYPE( int32_t, OPAL_ALIGNMENT_INT32, INT4, FLAGS )
#define OPAL_DATATYPE_INITIALIZER_INT8(FLAGS)       OPAL_DATATYPE_INIT_BASIC_DATATYPE( int64_t, OPAL_ALIGNMENT_INT64, INT8, FLAGS )
#ifdef HAVE_INT128_T
#define OPAL_DATATYPE_INITIALIZER_INT16(FLAGS)      OPAL_DATATYPE_INIT_BASIC_DATATYPE( int128_t, OPAL_ALIGNMENT_INT128, INT16, FLAGS )
#else
#define OPAL_DATATYPE_INITIALIZER_INT16(FLAGS)      OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED( INT16, FLAGS )
#endif
#define OPAL_DATATYPE_INITIALIZER_UINT1(FLAGS)      OPAL_DATATYPE_INIT_BASIC_DATATYPE( uint8_t, OPAL_ALIGNMENT_INT8, UINT1, FLAGS )
#define OPAL_DATATYPE_INITIALIZER_UINT2(FLAGS)      OPAL_DATATYPE_INIT_BASIC_DATATYPE( uint16_t, OPAL_ALIGNMENT_INT16, UINT2, FLAGS )
#define OPAL_DATATYPE_INITIALIZER_UINT4(FLAGS)      OPAL_DATATYPE_INIT_BASIC_DATATYPE( uint32_t, OPAL_ALIGNMENT_INT32, UINT4, FLAGS )
#define OPAL_DATATYPE_INITIALIZER_UINT8(FLAGS)      OPAL_DATATYPE_INIT_BASIC_DATATYPE( uint64_t, OPAL_ALIGNMENT_INT64, UINT8, FLAGS )
#ifdef HAVE_UINT128_T
#define OPAL_DATATYPE_INITIALIZER_UINT16(FLAGS)     OPAL_DATATYPE_INIT_BASIC_DATATYPE( uint128_t, OPAL_ALIGNMENT_INT128, UINT16, FLAGS )
#else
#define OPAL_DATATYPE_INITIALIZER_UINT16(FLAGS)     OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED( INT16, FLAGS )
#endif

#if SIZEOF_FLOAT == 2
#define OPAL_DATATYPE_INITIALIZER_FLOAT2(FLAGS)     OPAL_DATATYPE_INIT_BASIC_DATATYPE( float, OPAL_ALIGNMENT_FLOAT, FLOAT2, FLAGS )
#elif SIZEOF_DOUBLE == 2
#define OPAL_DATATYPE_INITIALIZER_FLOAT2(FLAGS)     OPAL_DATATYPE_INIT_BASIC_DATATYPE( double, OPAL_ALIGNMENT_DOUBLE, FLOAT2, FLAGS )
#elif SIZEOF_LONG_DOUBLE == 2
#define OPAL_DATATYPE_INITIALIZER_FLOAT2(FLAGS)     OPAL_DATATYPE_INIT_BASIC_DATATYPE( long double, OPAL_ALIGNMENT_LONG_DOUBLE, FLOAT2, FLAGS )
#else
#define OPAL_DATATYPE_INITIALIZER_FLOAT2(FLAGS)     OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED( FLOAT2, FLAGS )
#endif

#if SIZEOF_FLOAT == 4
#define OPAL_DATATYPE_INITIALIZER_FLOAT4(FLAGS)     OPAL_DATATYPE_INIT_BASIC_DATATYPE( float, OPAL_ALIGNMENT_FLOAT, FLOAT4, FLAGS )
#elif SIZEOF_DOUBLE == 4
#define OPAL_DATATYPE_INITIALIZER_FLOAT4(FLAGS)     OPAL_DATATYPE_INIT_BASIC_DATATYPE( double, OPAL_ALIGNMENT_DOUBLE, FLOAT4, FLAGS )
#elif SIZEOF_LONG_DOUBLE == 4
#define OPAL_DATATYPE_INITIALIZER_FLOAT4(FLAGS)     OPAL_DATATYPE_INIT_BASIC_DATATYPE( long double, OPAL_ALIGNMENT_LONG_DOUBLE, FLOAT4, FLAGS )
#else
#define OPAL_DATATYPE_INITIALIZER_FLOAT4(FLAGS)     OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED( FLOAT4, FLAGS )
#endif

#if SIZEOF_FLOAT == 8
#define OPAL_DATATYPE_INITIALIZER_FLOAT8(FLAGS)     OPAL_DATATYPE_INIT_BASIC_DATATYPE( float, OPAL_ALIGNMENT_FLOAT, FLOAT8, FLAGS )
#elif SIZEOF_DOUBLE == 8
#define OPAL_DATATYPE_INITIALIZER_FLOAT8(FLAGS)     OPAL_DATATYPE_INIT_BASIC_DATATYPE( double, OPAL_ALIGNMENT_DOUBLE, FLOAT8, FLAGS )
#elif SIZEOF_LONG_DOUBLE == 8
#define OPAL_DATATYPE_INITIALIZER_FLOAT8(FLAGS)     OPAL_DATATYPE_INIT_BASIC_DATATYPE( long double, OPAL_ALIGNMENT_LONG_DOUBLE, FLOAT8, FLAGS )
#else
#define OPAL_DATATYPE_INITIALIZER_FLOAT8(FLAGS)     OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED( FLOAT8, FLAGS )
#endif

#if SIZEOF_FLOAT == 12
#define OPAL_DATATYPE_INITIALIZER_FLOAT12(FLAGS)    OPAL_DATATYPE_INIT_BASIC_DATATYPE( float, OPAL_ALIGNMENT_FLOAT, FLOAT12, FLAGS )
#elif SIZEOF_DOUBLE == 12
#define OPAL_DATATYPE_INITIALIZER_FLOAT12(FLAGS)    OPAL_DATATYPE_INIT_BASIC_DATATYPE( double, OPAL_ALIGNMENT_DOUBLE, FLOAT12, FLAGS )
#elif SIZEOF_LONG_DOUBLE == 12
#define OPAL_DATATYPE_INITIALIZER_FLOAT12(FLAGS)    OPAL_DATATYPE_INIT_BASIC_DATATYPE( long double, OPAL_ALIGNMENT_LONG_DOUBLE, FLOAT12, FLAGS )
#else
#define OPAL_DATATYPE_INITIALIZER_FLOAT12(FLAGS)    OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED( FLOAT12, FLAGS )
#endif

#if SIZEOF_FLOAT == 16
#define OPAL_DATATYPE_INITIALIZER_FLOAT16(FLAGS)    OPAL_DATATYPE_INIT_BASIC_DATATYPE( float, OPAL_ALIGNMENT_FLOAT, FLOAT16, FLAGS )
#elif SIZEOF_DOUBLE == 16
#define OPAL_DATATYPE_INITIALIZER_FLOAT16(FLAGS)    OPAL_DATATYPE_INIT_BASIC_DATATYPE( double, OPAL_ALIGNMENT_DOUBLE, FLOAT16, FLAGS )
#elif SIZEOF_LONG_DOUBLE == 16
#define OPAL_DATATYPE_INITIALIZER_FLOAT16(FLAGS)    OPAL_DATATYPE_INIT_BASIC_DATATYPE( long double, OPAL_ALIGNMENT_LONG_DOUBLE, FLOAT16, FLAGS )
#else
#define OPAL_DATATYPE_INITIALIZER_FLOAT16(FLAGS)    OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED( FLOAT16, FLAGS )
#endif

/*
 * Until we figure out the whole logic behing the _Complex type, disable it. It only exists
 * in C99 (i.e. __STDC_VERSION__ >= 199901L) but [evidently] not all compilers correctly
 * export the __STDC_VERSION__.
 * http://predef.sourceforge.net/prestd.html for more info.
 */

#define OPAL_USE_FLOAT__COMPLEX       (0 && HAVE_FLOAT__COMPLEX)
#define OPAL_USE_DOUBLE__COMPLEX      (0 && HAVE_DOUBLE__COMPLEX)
#define OPAL_USE_LONG_DOUBLE__COMPLEX (0 && HAVE_LONG_DOUBLE__COMPLEX)

#if OPAL_USE_FLOAT__COMPLEX && (SIZEOF_FLOAT__COMPLEX == 8)
#define OPAL_DATATYPE_INITIALIZER_COMPLEX8(FLAGS)   OPAL_DATATYPE_INIT_BASIC_DATATYPE( float _Complex, OPAL_ALIGNMENT_FLOAT_COMPLEX, COMPLEX8, (FLAGS) )
#else

#if SIZEOF_FLOAT == 4
typedef struct {
    float r;
    float i;
} opal_complex_float_t;

#define OPAL_DATATYPE_INITIALIZER_COMPLEX8( FLAGS )                     \
    {                                                                   \
        OPAL_OBJ_STATIC_INIT(opal_datatype_t),                          \
        OPAL_DATATYPE_FLAG_BASIC | (FLAGS) /*flag*/,                    \
        OPAL_DATATYPE_COMPLEX8 /*id*/,                                  \
        (((uint32_t)1)<<(OPAL_DATATYPE_ ## FLOAT4)) /*bdt_used*/,       \
        sizeof(opal_complex_float_t) /*size*/,                          \
        0 /*true_lb*/, sizeof(opal_complex_float_t) /*true_ub*/, 0 /*lb*/, sizeof(opal_complex_float_t) /*ub*/, \
        OPAL_ALIGNMENT_FLOAT /*align*/,                                 \
        2 /*nbElems*/,                                                  \
        OPAL_DATATYPE_INIT_NAME(COMPLEX8) /*name*/,                     \
        OPAL_DATATYPE_INIT_DESC_PREDEFINED(COMPLEX8) /*desc*/,          \
        OPAL_DATATYPE_INIT_DESC_PREDEFINED(COMPLEX8) /*opt_desc*/,      \
        {Z15, 2, } /*btypes*/                                           \
    }
#else
#define OPAL_DATATYPE_INITIALIZER_COMPLEX8(FLAGS)  OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED( COMPLEX8, FLAGS )
#endif

#endif  /* HAVE_FLOAT__COMPLEX */

#if OPAL_USE_DOUBLE__COMPLEX && (SIZEOF_DOUBLE__COMPLEX == 16)
#define OPAL_DATATYPE_INITIALIZER_COMPLEX16(FLAGS)  OPAL_DATATYPE_INIT_BASIC_DATATYPE( double _Complex, OPAL_ALIGNMENT_DOUBLE_COMPLEX, COMPLEX16, (FLAGS) )
#else

#if SIZEOF_DOUBLE == 8
typedef struct {
    double r;
    double i;
} opal_complex_double_t;

#define OPAL_DATATYPE_INITIALIZER_COMPLEX16( FLAGS )                    \
    {                                                                   \
        OPAL_OBJ_STATIC_INIT(opal_datatype_t),                          \
        OPAL_DATATYPE_FLAG_BASIC | (FLAGS) /*flag*/,                    \
        OPAL_DATATYPE_COMPLEX16 /*id*/,                                 \
        (((uint32_t)1)<<(OPAL_DATATYPE_ ## FLOAT8)) /*bdt_used*/,       \
        sizeof(opal_complex_double_t) /*size*/,                         \
        0 /*true_lb*/, sizeof(opal_complex_double_t) /*true_ub*/, 0 /*lb*/, sizeof(opal_complex_double_t) /*ub*/, \
        OPAL_ALIGNMENT_DOUBLE /*align*/,                                \
        2 /*nbElems*/,                                                  \
        OPAL_DATATYPE_INIT_NAME(COMPLEX16) /*name*/,                    \
        OPAL_DATATYPE_INIT_DESC_PREDEFINED(COMPLEX16) /*desc*/,         \
        OPAL_DATATYPE_INIT_DESC_PREDEFINED(COMPLEX16) /*opt_desc*/,     \
        {Z15, 2, } /*btypes*/                                           \
    }
#else
#define OPAL_DATATYPE_INITIALIZER_COMPLEX16(FLAGS) OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED( COMPLEX16, FLAGS )
#endif

#endif  /* HAVE_DOUBLE__COMPLEX */

#if OPAL_USE_LONG_DOUBLE__COMPLEX && (SIZEOF_LONG_DOUBLE__COMPLEX == 32)
#define OPAL_DATATYPE_INITIALIZER_COMPLEX32(FLAGS)   OPAL_DATATYPE_INIT_BASIC_DATATYPE( long double _Complex, OPAL_ALIGNMENT_LONG_DOUBLE_COMPLEX, COMPLEX32, (FLAGS) )
#else

#if HAVE_LONG_DOUBLE && (SIZEOF_LONG_DOUBLE == 16)
typedef struct {
    long double r;
    long double i;
} opal_complex_long_double_t;

#define OPAL_DATATYPE_INITIALIZER_COMPLEX32( FLAGS )                    \
    {                                                                   \
        OPAL_OBJ_STATIC_INIT(opal_datatype_t),                          \
        OPAL_DATATYPE_FLAG_BASIC | (FLAGS) /*flag*/,                    \
        OPAL_DATATYPE_COMPLEX32 /*id*/,                                 \
        (((uint32_t)1)<<(OPAL_DATATYPE_ ## FLOAT16)) /*bdt_used*/,      \
        sizeof(opal_complex_long_double_t) /*size*/,                    \
        0 /*true_lb*/, sizeof(opal_complex_long_double_t) /*true_ub*/, 0 /*lb*/, sizeof(opal_complex_long_double_t) /*ub*/, \
        OPAL_ALIGNMENT_LONG_DOUBLE /*align*/,                           \
        2 /*nbElems*/,                                                  \
        OPAL_DATATYPE_INIT_NAME(COMPLEX32) /*name*/,                    \
        OPAL_DATATYPE_INIT_DESC_PREDEFINED(COMPLEX32) /*desc*/,         \
        OPAL_DATATYPE_INIT_DESC_PREDEFINED(COMPLEX32) /*opt_desc*/,     \
        {Z15, 2, } /*btypes*/                                           \
    }
#else
#define OPAL_DATATYPE_INITIALIZER_COMPLEX32(FLAGS) OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED( COMPLEX32, FLAGS )
#endif

#endif  /* HAVE_LONG_DOUBLE__COMPLEX */

#define OPAL_DATATYPE_INITIALIZER_BOOL(FLAGS)       OPAL_DATATYPE_INIT_BASIC_DATATYPE( _Bool, OPAL_ALIGNMENT_BOOL, BOOL, FLAGS )

#if OPAL_ALIGNMENT_WCHAR != 0
#define OPAL_DATATYPE_INITIALIZER_WCHAR(FLAGS)      OPAL_DATATYPE_INIT_BASIC_DATATYPE( wchar_t, OPAL_ALIGNMENT_WCHAR, WCHAR, FLAGS )
#else
#define OPAL_DATATYPE_INITIALIZER_WCHAR(FLAGS)      OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED( WCHAR, FLAGS )
#endif


#define BASIC_DDT_FROM_ELEM( ELEM ) (opal_datatype_basicDatatypes[(ELEM).elem.common.type])

#define SAVE_STACK( PSTACK, INDEX, TYPE, COUNT, DISP) \
do { \
   (PSTACK)->index    = (INDEX); \
   (PSTACK)->type     = (TYPE); \
   (PSTACK)->count    = (COUNT); \
   (PSTACK)->disp     = (DISP); \
} while(0)

#define PUSH_STACK( PSTACK, STACK_POS, INDEX, TYPE, COUNT, DISP) \
do { \
   dt_stack_t* pTempStack = (PSTACK) + 1; \
   SAVE_STACK( pTempStack, (INDEX), (TYPE), (COUNT), (DISP) );  \
   (STACK_POS)++; \
   (PSTACK) = pTempStack; \
} while(0)

#if OPAL_ENABLE_DEBUG
#define OPAL_DATATYPE_SAFEGUARD_POINTER( ACTPTR, LENGTH, INITPTR, PDATA, COUNT ) \
    {                                                                   \
        unsigned char *__lower_bound = (INITPTR), *__upper_bound;       \
        assert( ((LENGTH) != 0) && ((COUNT) != 0) );                    \
        __lower_bound += (PDATA)->true_lb;                              \
        __upper_bound = (INITPTR) + (PDATA)->true_ub +                  \
            ((PDATA)->ub - (PDATA)->lb) * ((COUNT) - 1);                \
        if( ((ACTPTR) < __lower_bound) || ((ACTPTR) >= __upper_bound) ) { \
            opal_datatype_safeguard_pointer_debug_breakpoint( (ACTPTR), (LENGTH), (INITPTR), (PDATA), (COUNT) ); \
            opal_output( 0, "%s:%d\n\tPointer %p size %lu is outside [%p,%p] for\n\tbase ptr %p count %d and data \n", \
                         __FILE__, __LINE__, (ACTPTR), (unsigned long)(LENGTH), __lower_bound, __upper_bound, \
                         (INITPTR), (COUNT) );                          \
            opal_datatype_dump( (PDATA) );                                   \
        }                                                               \
    }

#else
#define OPAL_DATATYPE_SAFEGUARD_POINTER( ACTPTR, LENGTH, INITPTR, PDATA, COUNT )
#endif  /* OPAL_ENABLE_DEBUG */

static inline int GET_FIRST_NON_LOOP( const union dt_elem_desc* _pElem )
{
    int element_index = 0;

    /* We dont have to check for the end as we always put an END_LOOP
     * at the end of all datatype descriptions.
     */
    while( _pElem->elem.common.type == OPAL_DATATYPE_LOOP ) {
        ++_pElem; element_index++;
    }
    return element_index;
}

#define UPDATE_INTERNAL_COUNTERS( DESCRIPTION, POSITION, ELEMENT, COUNTER ) \
    do {                                                                \
        (ELEMENT) = &((DESCRIPTION)[(POSITION)]);                       \
        (COUNTER) = (ELEMENT)->elem.count;                              \
    } while (0)

OPAL_DECLSPEC int opal_datatype_contain_basic_datatypes( const struct opal_datatype_t* pData, char* ptr, size_t length );
OPAL_DECLSPEC int opal_datatype_dump_data_flags( unsigned short usflags, char* ptr, size_t length );
OPAL_DECLSPEC int opal_datatype_dump_data_desc( union dt_elem_desc* pDesc, int nbElems, char* ptr, size_t length );

END_C_DECLS
#endif  /* OPAL_DATATYPE_INTERNAL_H_HAS_BEEN_INCLUDED */
