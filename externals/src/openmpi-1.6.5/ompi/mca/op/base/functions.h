/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_OP_BASE_FUNCTIONS_H
#define OMPI_OP_BASE_FUNCTIONS_H

#include "ompi_config.h"
#include "ompi/mca/op/op.h"

/*
 * Since we have so many of these, and they're all identical except
 * for the name, use macros to prototype them.
 */
#define OMPI_OP_PROTO (void *in, void *out, int *count, struct ompi_datatype_t **dtype, struct ompi_op_base_module_1_0_0_t *module)

/* C integer */
#define OMPI_OP_HANDLER_C_INTEGER_INTRINSIC(name) \
  void ompi_op_base_##name##_int8_t OMPI_OP_PROTO; \
  void ompi_op_base_##name##_uint8_t OMPI_OP_PROTO; \
  void ompi_op_base_##name##_int16_t OMPI_OP_PROTO; \
  void ompi_op_base_##name##_uint16_t OMPI_OP_PROTO; \
  void ompi_op_base_##name##_int32_t OMPI_OP_PROTO; \
  void ompi_op_base_##name##_uint32_t OMPI_OP_PROTO; \
  void ompi_op_base_##name##_int64_t OMPI_OP_PROTO; \
  void ompi_op_base_##name##_uint64_t OMPI_OP_PROTO;
#define OMPI_OP_HANDLER_C_INTEGER(name) \
  OMPI_OP_HANDLER_C_INTEGER_INTRINSIC(name)

/* Fortran integer */

#define OMPI_OP_HANDLER_FORTRAN_INTEGER_INTRINSIC(name) \
  void ompi_op_base_##name##_fortran_integer OMPI_OP_PROTO;
#if OMPI_HAVE_FORTRAN_INTEGER1
#define OMPI_OP_HANDLER_FORTRAN_INTEGER1(name) \
  void ompi_op_base_##name##_fortran_integer1 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_FORTRAN_INTEGER1(name)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
#define OMPI_OP_HANDLER_FORTRAN_INTEGER2(name) \
  void ompi_op_base_##name##_fortran_integer2 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_FORTRAN_INTEGER2(name)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
#define OMPI_OP_HANDLER_FORTRAN_INTEGER4(name) \
  void ompi_op_base_##name##_fortran_integer4 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_FORTRAN_INTEGER4(name)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
#define OMPI_OP_HANDLER_FORTRAN_INTEGER8(name) \
  void ompi_op_base_##name##_fortran_integer8 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_FORTRAN_INTEGER8(name)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
#define OMPI_OP_HANDLER_FORTRAN_INTEGER16(name) \
  void ompi_op_base_##name##_fortran_integer16 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_FORTRAN_INTEGER16(name)
#endif
#define OMPI_OP_HANDLER_FORTRAN_INTEGER(name) \
  OMPI_OP_HANDLER_FORTRAN_INTEGER_INTRINSIC(name) \
  OMPI_OP_HANDLER_FORTRAN_INTEGER1(name) \
  OMPI_OP_HANDLER_FORTRAN_INTEGER2(name) \
  OMPI_OP_HANDLER_FORTRAN_INTEGER4(name) \
  OMPI_OP_HANDLER_FORTRAN_INTEGER8(name) \
  OMPI_OP_HANDLER_FORTRAN_INTEGER16(name)

/* Floating point */

#define OMPI_OP_HANDLER_FLOATING_POINT_INTRINSIC(name) \
  void ompi_op_base_##name##_float OMPI_OP_PROTO; \
  void ompi_op_base_##name##_double OMPI_OP_PROTO; \
  void ompi_op_base_##name##_fortran_real OMPI_OP_PROTO; \
  void ompi_op_base_##name##_fortran_double_precision OMPI_OP_PROTO; \
  void ompi_op_base_##name##_long_double OMPI_OP_PROTO;
#if OMPI_HAVE_FORTRAN_REAL2
#define OMPI_OP_HANDLER_FLOATING_POINT_REAL2(name) \
  void ompi_op_base_##name##_fortran_real2 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_FLOATING_POINT_REAL2(name)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
#define OMPI_OP_HANDLER_FLOATING_POINT_REAL4(name) \
  void ompi_op_base_##name##_fortran_real4 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_FLOATING_POINT_REAL4(name)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
#define OMPI_OP_HANDLER_FLOATING_POINT_REAL8(name) \
  void ompi_op_base_##name##_fortran_real8 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_FLOATING_POINT_REAL8(name)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
#define OMPI_OP_HANDLER_FLOATING_POINT_REAL16(name) \
  void ompi_op_base_##name##_fortran_real16 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_FLOATING_POINT_REAL16(name)
#endif
#define OMPI_OP_HANDLER_FLOATING_POINT(name) \
  OMPI_OP_HANDLER_FLOATING_POINT_INTRINSIC(name) \
  OMPI_OP_HANDLER_FLOATING_POINT_REAL4(name) \
  OMPI_OP_HANDLER_FLOATING_POINT_REAL8(name) \
  OMPI_OP_HANDLER_FLOATING_POINT_REAL16(name) \

/* Logical */

#define OMPI_OP_HANDLER_LOGICAL(name) \
  void ompi_op_base_##name##_fortran_logical OMPI_OP_PROTO; \
  void ompi_op_base_##name##_bool OMPI_OP_PROTO;

/* Complex */

#if OMPI_HAVE_FORTRAN_REAL
#define OMPI_OP_HANDLER_COMPLEX_INTRINSIC(name) \
  void ompi_op_base_##name##_fortran_complex OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_COMPLEX_INTRINSIC(name)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
#define OMPI_OP_HANDLER_DOUBLE_COMPLEX_INTRINSIC(name) \
  void ompi_op_base_##name##_fortran_double_complex OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_DOUBLE_COMPLEX_INTRINSIC(name)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
#define OMPI_OP_HANDLER_COMPLEX8(name) \
  void ompi_op_base_##name##_fortran_complex8 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_COMPLEX8(name)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
#define OMPI_OP_HANDLER_COMPLEX16(name) \
  void ompi_op_base_##name##_fortran_complex16 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_COMPLEX16(name)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
#define OMPI_OP_HANDLER_COMPLEX32(name) \
  void ompi_op_base_##name##_fortran_complex32 OMPI_OP_PROTO;
#else
#define OMPI_OP_HANDLER_COMPLEX32(name)
#endif
#define OMPI_OP_HANDLER_COMPLEX(name) \
  OMPI_OP_HANDLER_COMPLEX_INTRINSIC(name) \
  OMPI_OP_HANDLER_DOUBLE_COMPLEX_INTRINSIC(name) \
  OMPI_OP_HANDLER_COMPLEX8(name) \
  OMPI_OP_HANDLER_COMPLEX16(name) \
  OMPI_OP_HANDLER_COMPLEX32(name)

/* Byte */

#define OMPI_OP_HANDLER_BYTE(name) \
  void ompi_op_base_##name##_byte OMPI_OP_PROTO;

/* "2 type" */

#define OMPI_OP_HANDLER_2TYPE(name) \
  void ompi_op_base_##name##_2real OMPI_OP_PROTO; \
  void ompi_op_base_##name##_2double_precision OMPI_OP_PROTO; \
  void ompi_op_base_##name##_2integer OMPI_OP_PROTO; \
  void ompi_op_base_##name##_float_int OMPI_OP_PROTO; \
  void ompi_op_base_##name##_double_int OMPI_OP_PROTO; \
  void ompi_op_base_##name##_long_int OMPI_OP_PROTO; \
  void ompi_op_base_##name##_2int OMPI_OP_PROTO; \
  void ompi_op_base_##name##_short_int OMPI_OP_PROTO; \
  void ompi_op_base_##name##_long_double_int OMPI_OP_PROTO;

BEGIN_C_DECLS

/**
 * Handler functions for MPI_MAX
 */
  OMPI_OP_HANDLER_C_INTEGER(max)
  OMPI_OP_HANDLER_FORTRAN_INTEGER(max)
  OMPI_OP_HANDLER_FLOATING_POINT(max)

/**
 * Handler functions for MPI_MIN
 */
  OMPI_OP_HANDLER_C_INTEGER(min)
  OMPI_OP_HANDLER_FORTRAN_INTEGER(min)
  OMPI_OP_HANDLER_FLOATING_POINT(min)

/**
 * Handler functions for MPI_SUM
 */
  OMPI_OP_HANDLER_C_INTEGER(sum)
  OMPI_OP_HANDLER_FORTRAN_INTEGER(sum)
  OMPI_OP_HANDLER_FLOATING_POINT(sum)
  OMPI_OP_HANDLER_COMPLEX(sum)

/**
 * Handler functions for MPI_PROD
 */
  OMPI_OP_HANDLER_C_INTEGER(prod)
  OMPI_OP_HANDLER_FORTRAN_INTEGER(prod)
  OMPI_OP_HANDLER_FLOATING_POINT(prod)
  OMPI_OP_HANDLER_COMPLEX(prod)

/**
 * Handler functions for MPI_LAND
 */
  OMPI_OP_HANDLER_C_INTEGER(land)
  OMPI_OP_HANDLER_LOGICAL(land)

/**
 * Handler functions for MPI_BAND
 */
  OMPI_OP_HANDLER_C_INTEGER(band)
  OMPI_OP_HANDLER_FORTRAN_INTEGER(band)
  OMPI_OP_HANDLER_BYTE(band)

/**
 * Handler functions for MPI_LOR
 */
  OMPI_OP_HANDLER_C_INTEGER(lor)
  OMPI_OP_HANDLER_LOGICAL(lor)

/**
 * Handler functions for MPI_BOR
 */
  OMPI_OP_HANDLER_C_INTEGER(bor)
  OMPI_OP_HANDLER_FORTRAN_INTEGER(bor)
  OMPI_OP_HANDLER_BYTE(bor)

/**
 * Handler functions for MPI_LXOR
 */
  OMPI_OP_HANDLER_C_INTEGER(lxor)
  OMPI_OP_HANDLER_LOGICAL(lxor)

/**
 * Handler functions for MPI_BXOR
 */
  OMPI_OP_HANDLER_C_INTEGER(bxor)
  OMPI_OP_HANDLER_FORTRAN_INTEGER(bxor)
  OMPI_OP_HANDLER_BYTE(bxor)

/**
 * Handler functions for MPI_MAXLOC
 */
  OMPI_OP_HANDLER_2TYPE(maxloc)

/**
 * Handler functions for MPI_MINLOC
 */
  OMPI_OP_HANDLER_2TYPE(minloc)

/*
 * 3 buffer prototypes (two input and one output)
 */
#define OMPI_OP_PROTO_3BUF  \
  ( void * restrict in1, void * restrict in2, void * restrict out, \
   int *count, struct ompi_datatype_t **dtype, \
   struct ompi_op_base_module_1_0_0_t *module)

/* C integer */

#define OMPI_OP_3BUFF_HANDLER_C_INTEGER_INTRINSIC(name) \
  void ompi_op_base_3buff_##name##_int8_t OMPI_OP_PROTO_3BUF; \
  void ompi_op_base_3buff_##name##_uint8_t OMPI_OP_PROTO_3BUF; \
  void ompi_op_base_3buff_##name##_int16_t OMPI_OP_PROTO_3BUF; \
  void ompi_op_base_3buff_##name##_uint16_t OMPI_OP_PROTO_3BUF; \
  void ompi_op_base_3buff_##name##_int32_t OMPI_OP_PROTO_3BUF; \
  void ompi_op_base_3buff_##name##_uint32_t OMPI_OP_PROTO_3BUF; \
  void ompi_op_base_3buff_##name##_int64_t OMPI_OP_PROTO_3BUF; \
  void ompi_op_base_3buff_##name##_uint64_t OMPI_OP_PROTO_3BUF;
#define OMPI_OP_3BUFF_HANDLER_C_INTEGER(name) \
  OMPI_OP_3BUFF_HANDLER_C_INTEGER_INTRINSIC(name)

/* Fortran integer */

#define OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER_INTRINSIC(name) \
  void ompi_op_base_3buff_##name##_fortran_integer OMPI_OP_PROTO_3BUF;
#if OMPI_HAVE_FORTRAN_INTEGER1
#define OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER1(name) \
  void ompi_op_base_3buff_##name##_fortran_integer1 OMPI_OP_PROTO_3BUF;
#else
#define OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER1(name)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
#define OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER2(name) \
  void ompi_op_base_3buff_##name##_fortran_integer2 OMPI_OP_PROTO_3BUF;
#else
#define OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER2(name)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
#define OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER4(name) \
  void ompi_op_base_3buff_##name##_fortran_integer4 OMPI_OP_PROTO_3BUF;
#else
#define OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER4(name)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
#define OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER8(name) \
  void ompi_op_base_3buff_##name##_fortran_integer8 OMPI_OP_PROTO_3BUF;
#else
#define OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER8(name)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
#define OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER16(name) \
  void ompi_op_base_3buff_##name##_fortran_integer16 OMPI_OP_PROTO_3BUF;
#else
#define OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER16(name)
#endif
#define OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER(name) \
  OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER_INTRINSIC(name) \
  OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER1(name) \
  OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER2(name) \
  OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER4(name) \
  OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER8(name) \
  OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER16(name)

/* Floating point */

#define OMPI_OP_3BUFF_HANDLER_FLOATING_POINT_INTRINSIC(name) \
  void ompi_op_base_3buff_##name##_float OMPI_OP_PROTO_3BUF; \
  void ompi_op_base_3buff_##name##_double OMPI_OP_PROTO_3BUF; \
  void ompi_op_base_3buff_##name##_fortran_real OMPI_OP_PROTO_3BUF; \
  void ompi_op_base_3buff_##name##_fortran_double_precision OMPI_OP_PROTO_3BUF; \
  void ompi_op_base_3buff_##name##_long_double OMPI_OP_PROTO_3BUF;
#if OMPI_HAVE_FORTRAN_REAL2
#define OMPI_OP_3BUFF_HANDLER_FLOATING_POINT_REAL2(name) \
  void ompi_op_base_3buff_##name##_fortran_real2 OMPI_OP_PROTO_3BUF;
#else
#define OMPI_OP_3BUFF_HANDLER_FLOATING_POINT_REAL2(name)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
#define OMPI_OP_3BUFF_HANDLER_FLOATING_POINT_REAL4(name) \
  void ompi_op_base_3buff_##name##_fortran_real4 OMPI_OP_PROTO_3BUF;
#else
#define OMPI_OP_3BUFF_HANDLER_FLOATING_POINT_REAL4(name)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
#define OMPI_OP_3BUFF_HANDLER_FLOATING_POINT_REAL8(name) \
  void ompi_op_base_3buff_##name##_fortran_real8 OMPI_OP_PROTO_3BUF;
#else
#define OMPI_OP_3BUFF_HANDLER_FLOATING_POINT_REAL8(name)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
#define OMPI_OP_3BUFF_HANDLER_FLOATING_POINT_REAL16(name) \
  void ompi_op_base_3buff_##name##_fortran_real16 OMPI_OP_PROTO_3BUF;
#else
#define OMPI_OP_3BUFF_HANDLER_FLOATING_POINT_REAL16(name)
#endif
#define OMPI_OP_3BUFF_HANDLER_FLOATING_POINT(name) \
  OMPI_OP_3BUFF_HANDLER_FLOATING_POINT_INTRINSIC(name) \
  OMPI_OP_3BUFF_HANDLER_FLOATING_POINT_REAL4(name) \
  OMPI_OP_3BUFF_HANDLER_FLOATING_POINT_REAL8(name) \
  OMPI_OP_3BUFF_HANDLER_FLOATING_POINT_REAL16(name) \

/* Logical */

#define OMPI_OP_3BUFF_HANDLER_LOGICAL(name) \
  void ompi_op_base_3buff_##name##_fortran_logical OMPI_OP_PROTO_3BUF; \
  void ompi_op_base_3buff_##name##_bool OMPI_OP_PROTO_3BUF;

/* Complex */

#if OMPI_HAVE_FORTRAN_REAL
#define OMPI_OP_3BUFF_HANDLER_COMPLEX_INTRINSIC(name) \
  void ompi_op_base_3buff_##name##_fortran_complex OMPI_OP_PROTO_3BUF;
#else
#define OMPI_OP_3BUFF_HANDLER_COMPLEX_INTRINSIC(name)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
#define OMPI_OP_3BUFF_HANDLER_DOUBLE_COMPLEX_INTRINSIC(name) \
  void ompi_op_base_3buff_##name##_fortran_double_complex OMPI_OP_PROTO_3BUF;
#else
#define OMPI_OP_3BUFF_HANDLER_DOUBLE_COMPLEX_INTRINSIC(name)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
#define OMPI_OP_3BUFF_HANDLER_COMPLEX8(name) \
  void ompi_op_base_3buff_##name##_fortran_complex8 OMPI_OP_PROTO_3BUF;
#else
#define OMPI_OP_3BUFF_HANDLER_COMPLEX8(name)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
#define OMPI_OP_3BUFF_HANDLER_COMPLEX16(name) \
  void ompi_op_base_3buff_##name##_fortran_complex16 OMPI_OP_PROTO_3BUF;
#else
#define OMPI_OP_3BUFF_HANDLER_COMPLEX16(name)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
#define OMPI_OP_3BUFF_HANDLER_COMPLEX32(name) \
  void ompi_op_base_3buff_##name##_fortran_complex32 OMPI_OP_PROTO_3BUF;
#else
#define OMPI_OP_3BUFF_HANDLER_COMPLEX32(name)
#endif
#define OMPI_OP_3BUFF_HANDLER_COMPLEX(name) \
  OMPI_OP_3BUFF_HANDLER_COMPLEX_INTRINSIC(name) \
  OMPI_OP_3BUFF_HANDLER_DOUBLE_COMPLEX_INTRINSIC(name) \
  OMPI_OP_3BUFF_HANDLER_COMPLEX8(name) \
  OMPI_OP_3BUFF_HANDLER_COMPLEX16(name) \
  OMPI_OP_3BUFF_HANDLER_COMPLEX32(name)

/* Byte */

#define OMPI_OP_3BUFF_HANDLER_BYTE(name) \
  void ompi_op_base_3buff_##name##_byte OMPI_OP_PROTO_3BUF;

/* "2 type" */

#define OMPI_OP_3BUFF_HANDLER_2TYPE(name) \
  void ompi_op_base_3buff_##name##_2real OMPI_OP_PROTO_3BUF; \
  void ompi_op_base_3buff_##name##_2double_precision OMPI_OP_PROTO_3BUF; \
  void ompi_op_base_3buff_##name##_2integer OMPI_OP_PROTO_3BUF; \
  void ompi_op_base_3buff_##name##_float_int OMPI_OP_PROTO_3BUF; \
  void ompi_op_base_3buff_##name##_double_int OMPI_OP_PROTO_3BUF; \
  void ompi_op_base_3buff_##name##_long_int OMPI_OP_PROTO_3BUF; \
  void ompi_op_base_3buff_##name##_2int OMPI_OP_PROTO_3BUF; \
  void ompi_op_base_3buff_##name##_short_int OMPI_OP_PROTO_3BUF; \
  void ompi_op_base_3buff_##name##_long_double_int OMPI_OP_PROTO_3BUF;

/**
 * Handler functions for MPI_MAX
 */
  OMPI_OP_3BUFF_HANDLER_C_INTEGER(max)
  OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER(max)
  OMPI_OP_3BUFF_HANDLER_FLOATING_POINT(max)

/**
 * Handler functions for MPI_MIN
 */
  OMPI_OP_3BUFF_HANDLER_C_INTEGER(min)
  OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER(min)
  OMPI_OP_3BUFF_HANDLER_FLOATING_POINT(min)

/**
 * Handler functions for MPI_SUM
 */
  OMPI_OP_3BUFF_HANDLER_C_INTEGER(sum)
  OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER(sum)
  OMPI_OP_3BUFF_HANDLER_FLOATING_POINT(sum)
  OMPI_OP_3BUFF_HANDLER_COMPLEX(sum)

/**
 * Handler functions for MPI_PROD
 */
  OMPI_OP_3BUFF_HANDLER_C_INTEGER(prod)
  OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER(prod)
  OMPI_OP_3BUFF_HANDLER_FLOATING_POINT(prod)
  OMPI_OP_3BUFF_HANDLER_COMPLEX(prod)

/**
 * Handler functions for MPI_LAND
 */
  OMPI_OP_3BUFF_HANDLER_C_INTEGER(land)
  OMPI_OP_3BUFF_HANDLER_LOGICAL(land)

/**
 * Handler functions for MPI_BAND
 */
  OMPI_OP_3BUFF_HANDLER_C_INTEGER(band)
  OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER(band)
  OMPI_OP_3BUFF_HANDLER_BYTE(band)

/**
 * Handler functions for MPI_LOR
 */
  OMPI_OP_3BUFF_HANDLER_C_INTEGER(lor)
  OMPI_OP_3BUFF_HANDLER_LOGICAL(lor)

/**
 * Handler functions for MPI_BOR
 */
  OMPI_OP_3BUFF_HANDLER_C_INTEGER(bor)
  OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER(bor)
  OMPI_OP_3BUFF_HANDLER_BYTE(bor)

/**
 * Handler functions for MPI_LXOR
 */
  OMPI_OP_3BUFF_HANDLER_C_INTEGER(lxor)
  OMPI_OP_3BUFF_HANDLER_LOGICAL(lxor)

/**
 * Handler functions for MPI_BXOR
 */
  OMPI_OP_3BUFF_HANDLER_C_INTEGER(bxor)
  OMPI_OP_3BUFF_HANDLER_FORTRAN_INTEGER(bxor)
  OMPI_OP_3BUFF_HANDLER_BYTE(bxor)

/**
 * Handler functions for MPI_MAXLOC
 */
  OMPI_OP_3BUFF_HANDLER_2TYPE(maxloc)

/**
 * Handler functions for MPI_MINLOC
 */
  OMPI_OP_3BUFF_HANDLER_2TYPE(minloc)

/**
 * Globals holding all the "base" function pointers, indexed by op and
 * datatype.
 */
OMPI_DECLSPEC extern ompi_op_base_handler_fn_t 
    ompi_op_base_functions[OMPI_OP_BASE_FORTRAN_OP_MAX][OMPI_OP_BASE_TYPE_MAX];
OMPI_DECLSPEC extern ompi_op_base_3buff_handler_fn_t 
    ompi_op_base_3buff_functions[OMPI_OP_BASE_FORTRAN_OP_MAX][OMPI_OP_BASE_TYPE_MAX];

END_C_DECLS

#endif /* OMPI_OP_BASE_FUNCTIONS_H */
