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
 * Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "ompi/mca/op/op.h"
#include "ompi/mca/op/base/functions.h"


/*
 * Since all the functions in this file are essentially identical, we
 * use a macro to substitute in names and types.  The core operation
 * in all functions that use this macro is the same.
 *
 * This macro is for (out op in).
 */
#define OP_FUNC(name, type_name, type, op) \
  void ompi_op_base_##name##_##type_name(void *in, void *out, int *count, \
                                        struct ompi_datatype_t **dtype,  \
                                        struct ompi_op_base_module_1_0_0_t *module) \
  {                                                                      \
      int i;                                                             \
      type *a = (type *) in;                                             \
      type *b = (type *) out;                                            \
      for (i = 0; i < *count; ++i) {                                     \
          *(b++) op *(a++);                                              \
      }                                                                  \
  }

#define COMPLEX_OP_FUNC_SUM(type_name, type) \
  void ompi_op_base_sum_##type_name(void *in, void *out, int *count,     \
                                    struct ompi_datatype_t **dtype,      \
                                    struct ompi_op_base_module_1_0_0_t *module)\
  {                                                                      \
      int i;                                                             \
      type *a = (type *) in;                                             \
      type *b = (type *) out;                                            \
      for (i = 0; i < *count; ++i, ++b, ++a) {                           \
          b->real += a->real;                                            \
          b->imag += a->imag;                                            \
      }                                                                  \
  }

#define COMPLEX_OP_FUNC_PROD(type_name, type) \
  void ompi_op_base_prod_##type_name(void *in, void *out, int *count,    \
                                    struct ompi_datatype_t **dtype,      \
                                    struct ompi_op_base_module_1_0_0_t *module)\
  {                                                                      \
      int i;                                                             \
      type *a = (type *) in;                                             \
      type *b = (type *) out;                                            \
      type temp;                                                         \
      for (i = 0; i < *count; ++i, ++b, ++a) {                           \
          temp.real = a->real * b->real - a->imag * b->imag;             \
          temp.imag = a->imag * b->real + a->real * b->imag;             \
          *b = temp;                                                     \
      }                                                                  \
  }


/*
 * Since all the functions in this file are essentially identical, we
 * use a macro to substitute in names and types.  The core operation
 * in all functions that use this macro is the same.
 *
 * This macro is for (out = op(out, in))
 */
#define FUNC_FUNC(name, type_name, type) \
  void ompi_op_base_##name##_##type_name(void *in, void *out, int *count, \
                                         struct ompi_datatype_t **dtype,  \
                                         struct ompi_op_base_module_1_0_0_t *module)\
  {                                                                      \
      int i;                                                             \
      type *a = (type *) in;                                             \
      type *b = (type *) out;                                            \
      for (i = 0; i < *count; ++i) {                                     \
          *(b) = current_func(*(b), *(a));                               \
          ++b;                                                           \
          ++a;                                                           \
      }                                                                  \
  }

/*
 * Since all the functions in this file are essentially identical, we
 * use a macro to substitute in names and types.  The core operation
 * in all functions that use this macro is the same.
 *
 * This macro is for minloc and maxloc
 */
#define LOC_STRUCT(type_name, type1, type2) \
  typedef struct { \
      type1 v; \
      type2 k; \
  } ompi_op_predefined_##type_name##_t;

#define LOC_FUNC(name, type_name, op) \
  void ompi_op_base_##name##_##type_name(void *in, void *out, int *count, \
                                         struct ompi_datatype_t **dtype,  \
                                         struct ompi_op_base_module_1_0_0_t *module)\
  { \
      int i; \
      ompi_op_predefined_##type_name##_t *a = (ompi_op_predefined_##type_name##_t*) in; \
      ompi_op_predefined_##type_name##_t *b = (ompi_op_predefined_##type_name##_t*) out; \
      for (i = 0; i < *count; ++i, ++a, ++b) { \
          if (a->v op b->v) { \
              b->v = a->v; \
              b->k = a->k; \
          } else if (a->v == b->v) { \
              b->k = (b->k < a->k ? b->k : a->k); \
          } \
      } \
  }

/*************************************************************************
 * Max
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) > (b) ? (a) : (b))
/* C integer */
FUNC_FUNC(max,   int8_t,   int8_t)
FUNC_FUNC(max,  uint8_t,  uint8_t)
FUNC_FUNC(max,  int16_t,  int16_t)
FUNC_FUNC(max, uint16_t, uint16_t)
FUNC_FUNC(max,  int32_t,  int32_t)
FUNC_FUNC(max, uint32_t, uint32_t)
FUNC_FUNC(max,  int64_t,  int64_t)
FUNC_FUNC(max, uint64_t, uint64_t)
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC(max, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC(max, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC(max, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC(max, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC(max, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC(max, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Floating point */
FUNC_FUNC(max, float, float)
FUNC_FUNC(max, double, double)
#if HAVE_LONG_DOUBLE
FUNC_FUNC(max, long_double, long double)
#endif
#if OMPI_HAVE_FORTRAN_REAL
FUNC_FUNC(max, fortran_real, ompi_fortran_real_t)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
FUNC_FUNC(max, fortran_double_precision, ompi_fortran_double_precision_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL2
FUNC_FUNC(max, fortran_real2, ompi_fortran_real2_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
FUNC_FUNC(max, fortran_real4, ompi_fortran_real4_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
FUNC_FUNC(max, fortran_real8, ompi_fortran_real8_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
FUNC_FUNC(max, fortran_real16, ompi_fortran_real16_t)
#endif


/*************************************************************************
 * Min
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) < (b) ? (a) : (b))
/* C integer */
FUNC_FUNC(min,   int8_t,   int8_t)
FUNC_FUNC(min,  uint8_t,  uint8_t)
FUNC_FUNC(min,  int16_t,  int16_t)
FUNC_FUNC(min, uint16_t, uint16_t)
FUNC_FUNC(min,  int32_t,  int32_t)
FUNC_FUNC(min, uint32_t, uint32_t)
FUNC_FUNC(min,  int64_t,  int64_t)
FUNC_FUNC(min, uint64_t, uint64_t)
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC(min, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC(min, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC(min, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC(min, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC(min, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC(min, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Floating point */
FUNC_FUNC(min, float, float)
FUNC_FUNC(min, double, double)
#if HAVE_LONG_DOUBLE
FUNC_FUNC(min, long_double, long double)
#endif
#if OMPI_HAVE_FORTRAN_REAL
FUNC_FUNC(min, fortran_real, ompi_fortran_real_t)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
FUNC_FUNC(min, fortran_double_precision, ompi_fortran_double_precision_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL2
FUNC_FUNC(min, fortran_real2, ompi_fortran_real2_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
FUNC_FUNC(min, fortran_real4, ompi_fortran_real4_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
FUNC_FUNC(min, fortran_real8, ompi_fortran_real8_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
FUNC_FUNC(min, fortran_real16, ompi_fortran_real16_t)
#endif

/*************************************************************************
 * Sum
 *************************************************************************/

/* C integer */
OP_FUNC(sum,   int8_t,   int8_t, +=)
OP_FUNC(sum,  uint8_t,  uint8_t, +=)
OP_FUNC(sum,  int16_t,  int16_t, +=)
OP_FUNC(sum, uint16_t, uint16_t, +=)
OP_FUNC(sum,  int32_t,  int32_t, +=)
OP_FUNC(sum, uint32_t, uint32_t, +=)
OP_FUNC(sum,  int64_t,  int64_t, +=)
OP_FUNC(sum, uint64_t, uint64_t, +=)
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
OP_FUNC(sum, fortran_integer, ompi_fortran_integer_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
OP_FUNC(sum, fortran_integer1, ompi_fortran_integer1_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
OP_FUNC(sum, fortran_integer2, ompi_fortran_integer2_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
OP_FUNC(sum, fortran_integer4, ompi_fortran_integer4_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
OP_FUNC(sum, fortran_integer8, ompi_fortran_integer8_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
OP_FUNC(sum, fortran_integer16, ompi_fortran_integer16_t, +=)
#endif
/* Floating point */
OP_FUNC(sum, float, float, +=)
OP_FUNC(sum, double, double, +=)
#if HAVE_LONG_DOUBLE
OP_FUNC(sum, long_double, long double, +=)
#endif
#if OMPI_HAVE_FORTRAN_REAL
OP_FUNC(sum, fortran_real, ompi_fortran_real_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
OP_FUNC(sum, fortran_double_precision, ompi_fortran_double_precision_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_REAL2
OP_FUNC(sum, fortran_real2, ompi_fortran_real2_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
OP_FUNC(sum, fortran_real4, ompi_fortran_real4_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
OP_FUNC(sum, fortran_real8, ompi_fortran_real8_t, +=)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
OP_FUNC(sum, fortran_real16, ompi_fortran_real16_t, +=)
#endif
/* Complex */
#if OMPI_HAVE_FORTRAN_REAL && OMPI_HAVE_FORTRAN_COMPLEX
COMPLEX_OP_FUNC_SUM(fortran_complex, ompi_fortran_complex_t)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION && OMPI_HAVE_FORTRAN_COMPLEX
COMPLEX_OP_FUNC_SUM(fortran_double_complex, ompi_fortran_double_complex_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL4 && OMPI_HAVE_FORTRAN_COMPLEX8
COMPLEX_OP_FUNC_SUM(fortran_complex8, ompi_fortran_complex8_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL8 && OMPI_HAVE_FORTRAN_COMPLEX16
COMPLEX_OP_FUNC_SUM(fortran_complex16, ompi_fortran_complex16_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL16 && OMPI_HAVE_FORTRAN_COMPLEX32
COMPLEX_OP_FUNC_SUM(fortran_complex32, ompi_fortran_complex32_t)
#endif

/*************************************************************************
 * Product
 *************************************************************************/

/* C integer */
OP_FUNC(prod,   int8_t,   int8_t, *=)
OP_FUNC(prod,  uint8_t,  uint8_t, *=)
OP_FUNC(prod,  int16_t,  int16_t, *=)
OP_FUNC(prod, uint16_t, uint16_t, *=)
OP_FUNC(prod,  int32_t,  int32_t, *=)
OP_FUNC(prod, uint32_t, uint32_t, *=)
OP_FUNC(prod,  int64_t,  int64_t, *=)
OP_FUNC(prod, uint64_t, uint64_t, *=)
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
OP_FUNC(prod, fortran_integer, ompi_fortran_integer_t, *=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
OP_FUNC(prod, fortran_integer1, ompi_fortran_integer1_t, *=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
OP_FUNC(prod, fortran_integer2, ompi_fortran_integer2_t, *=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
OP_FUNC(prod, fortran_integer4, ompi_fortran_integer4_t, *=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
OP_FUNC(prod, fortran_integer8, ompi_fortran_integer8_t, *=)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
OP_FUNC(prod, fortran_integer16, ompi_fortran_integer16_t, *=)
#endif
/* Floating point */
OP_FUNC(prod, float, float, *=)
OP_FUNC(prod, double, double, *=)
#if HAVE_LONG_DOUBLE
OP_FUNC(prod, long_double, long double, *=)
#endif
#if OMPI_HAVE_FORTRAN_REAL
OP_FUNC(prod, fortran_real, ompi_fortran_real_t, *=)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
OP_FUNC(prod, fortran_double_precision, ompi_fortran_double_precision_t, *=)
#endif
#if OMPI_HAVE_FORTRAN_REAL2
OP_FUNC(prod, fortran_real2, ompi_fortran_real2_t, *=)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
OP_FUNC(prod, fortran_real4, ompi_fortran_real4_t, *=)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
OP_FUNC(prod, fortran_real8, ompi_fortran_real8_t, *=)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
OP_FUNC(prod, fortran_real16, ompi_fortran_real16_t, *=)
#endif
/* Complex */
#if OMPI_HAVE_FORTRAN_REAL && OMPI_HAVE_FORTRAN_COMPLEX
COMPLEX_OP_FUNC_PROD(fortran_complex, ompi_fortran_complex_t)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION && OMPI_HAVE_FORTRAN_COMPLEX
COMPLEX_OP_FUNC_PROD(fortran_double_complex, ompi_fortran_double_complex_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL4 && OMPI_HAVE_FORTRAN_COMPLEX8
COMPLEX_OP_FUNC_PROD(fortran_complex8, ompi_fortran_complex8_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL8 && OMPI_HAVE_FORTRAN_COMPLEX16
COMPLEX_OP_FUNC_PROD(fortran_complex16, ompi_fortran_complex16_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL16 && OMPI_HAVE_FORTRAN_COMPLEX32
COMPLEX_OP_FUNC_PROD(fortran_complex32, ompi_fortran_complex32_t)
#endif

/*************************************************************************
 * Logical AND
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) && (b))
/* C integer */
FUNC_FUNC(land,   int8_t,   int8_t)
FUNC_FUNC(land,  uint8_t,  uint8_t)
FUNC_FUNC(land,  int16_t,  int16_t)
FUNC_FUNC(land, uint16_t, uint16_t)
FUNC_FUNC(land,  int32_t,  int32_t)
FUNC_FUNC(land, uint32_t, uint32_t)
FUNC_FUNC(land,  int64_t,  int64_t)
FUNC_FUNC(land, uint64_t, uint64_t)
/* Logical */
#if OMPI_HAVE_FORTRAN_LOGICAL
FUNC_FUNC(land, fortran_logical, ompi_fortran_logical_t)
#endif
/* C++ bool */
FUNC_FUNC(land, bool, bool)

/*************************************************************************
 * Logical OR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) || (b))
/* C integer */
FUNC_FUNC(lor,   int8_t,   int8_t)
FUNC_FUNC(lor,  uint8_t,  uint8_t)
FUNC_FUNC(lor,  int16_t,  int16_t)
FUNC_FUNC(lor, uint16_t, uint16_t)
FUNC_FUNC(lor,  int32_t,  int32_t)
FUNC_FUNC(lor, uint32_t, uint32_t)
FUNC_FUNC(lor,  int64_t,  int64_t)
FUNC_FUNC(lor, uint64_t, uint64_t)
/* Logical */
#if OMPI_HAVE_FORTRAN_LOGICAL
FUNC_FUNC(lor, fortran_logical, ompi_fortran_logical_t)
#endif
/* C++ bool */
FUNC_FUNC(lor, bool, bool)

/*************************************************************************
 * Logical XOR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a ? 1 : 0) ^ (b ? 1: 0))
/* C integer */
FUNC_FUNC(lxor,   int8_t,   int8_t)
FUNC_FUNC(lxor,  uint8_t,  uint8_t)
FUNC_FUNC(lxor,  int16_t,  int16_t)
FUNC_FUNC(lxor, uint16_t, uint16_t)
FUNC_FUNC(lxor,  int32_t,  int32_t)
FUNC_FUNC(lxor, uint32_t, uint32_t)
FUNC_FUNC(lxor,  int64_t,  int64_t)
FUNC_FUNC(lxor, uint64_t, uint64_t)
/* Logical */
#if OMPI_HAVE_FORTRAN_LOGICAL
FUNC_FUNC(lxor, fortran_logical, ompi_fortran_logical_t)
#endif
/* C++ bool */
FUNC_FUNC(lxor, bool, bool)

/*************************************************************************
 * Bitwise AND
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) & (b))
/* C integer */
FUNC_FUNC(band,   int8_t,   int8_t)
FUNC_FUNC(band,  uint8_t,  uint8_t)
FUNC_FUNC(band,  int16_t,  int16_t)
FUNC_FUNC(band, uint16_t, uint16_t)
FUNC_FUNC(band,  int32_t,  int32_t)
FUNC_FUNC(band, uint32_t, uint32_t)
FUNC_FUNC(band,  int64_t,  int64_t)
FUNC_FUNC(band, uint64_t, uint64_t)
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC(band, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC(band, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC(band, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC(band, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC(band, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC(band, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Byte */
FUNC_FUNC(band, byte, char)

/*************************************************************************
 * Bitwise OR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) | (b))
/* C integer */
FUNC_FUNC(bor,   int8_t,   int8_t)
FUNC_FUNC(bor,  uint8_t,  uint8_t)
FUNC_FUNC(bor,  int16_t,  int16_t)
FUNC_FUNC(bor, uint16_t, uint16_t)
FUNC_FUNC(bor,  int32_t,  int32_t)
FUNC_FUNC(bor, uint32_t, uint32_t)
FUNC_FUNC(bor,  int64_t,  int64_t)
FUNC_FUNC(bor, uint64_t, uint64_t)
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC(bor, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC(bor, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC(bor, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC(bor, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC(bor, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC(bor, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Byte */
FUNC_FUNC(bor, byte, char)

/*************************************************************************
 * Bitwise XOR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) ^ (b))
/* C integer */
FUNC_FUNC(bxor,   int8_t,   int8_t)
FUNC_FUNC(bxor,  uint8_t,  uint8_t)
FUNC_FUNC(bxor,  int16_t,  int16_t)
FUNC_FUNC(bxor, uint16_t, uint16_t)
FUNC_FUNC(bxor,  int32_t,  int32_t)
FUNC_FUNC(bxor, uint32_t, uint32_t)
FUNC_FUNC(bxor,  int64_t,  int64_t)
FUNC_FUNC(bxor, uint64_t, uint64_t)
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC(bxor, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC(bxor, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC(bxor, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC(bxor, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC(bxor, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC(bxor, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Byte */
FUNC_FUNC(bxor, byte, char)

/*************************************************************************
 * Min and max location "pair" datatypes
 *************************************************************************/

#if OMPI_HAVE_FORTRAN_REAL
LOC_STRUCT(2real, ompi_fortran_real_t, ompi_fortran_real_t)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
LOC_STRUCT(2double_precision, ompi_fortran_double_precision_t, ompi_fortran_double_precision_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER
LOC_STRUCT(2integer, ompi_fortran_integer_t, ompi_fortran_integer_t)
#endif
LOC_STRUCT(float_int, float, int)
LOC_STRUCT(double_int, double, int)
LOC_STRUCT(long_int, long, int)
LOC_STRUCT(2int, int, int)
LOC_STRUCT(short_int, short, int)
#if HAVE_LONG_DOUBLE
LOC_STRUCT(long_double_int, long double, int)
#endif

/*************************************************************************
 * Max location
 *************************************************************************/

#if OMPI_HAVE_FORTRAN_REAL
LOC_FUNC(maxloc, 2real, >)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
LOC_FUNC(maxloc, 2double_precision, >)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER
LOC_FUNC(maxloc, 2integer, >)
#endif
LOC_FUNC(maxloc, float_int, >)
LOC_FUNC(maxloc, double_int, >)
LOC_FUNC(maxloc, long_int, >)
LOC_FUNC(maxloc, 2int, >)
LOC_FUNC(maxloc, short_int, >)
#if HAVE_LONG_DOUBLE
LOC_FUNC(maxloc, long_double_int, >)
#endif

/*************************************************************************
 * Min location
 *************************************************************************/

#if OMPI_HAVE_FORTRAN_REAL
LOC_FUNC(minloc, 2real, <)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
LOC_FUNC(minloc, 2double_precision, <)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER
LOC_FUNC(minloc, 2integer, <)
#endif
LOC_FUNC(minloc, float_int, <)
LOC_FUNC(minloc, double_int, <)
LOC_FUNC(minloc, long_int, <)
LOC_FUNC(minloc, 2int, <)
LOC_FUNC(minloc, short_int, <)
#if HAVE_LONG_DOUBLE
LOC_FUNC(minloc, long_double_int, <)
#endif


/*
 *  This is a three buffer (2 input and 1 output) version of the reduction
 *    routines, needed for some optimizations.
 */
#define OP_FUNC_3BUF(name, type_name, type, op) \
  void ompi_op_base_3buff_##name##_##type_name(void * restrict in1,      \
          void * restrict in2, void * restrict out, int *count,          \
          struct ompi_datatype_t **dtype,                                \
          struct ompi_op_base_module_1_0_0_t *module)                          \
  {                                                                      \
      int i;                                                             \
      type *a1 = (type *) in1;                                           \
      type *a2 = (type *) in2;                                           \
      type *b = (type *) out;                                            \
      for (i = 0; i < *count; ++i) {                                     \
          *(b++) =  *(a1++) op *(a2++);                                  \
      }                                                                  \
  }

#define COMPLEX_OP_FUNC_SUM_3BUF(type_name, type) \
  void ompi_op_base_3buff_sum_##type_name(void * restrict in1,           \
          void * restrict in2, void * restrict out, int *count,          \
          struct ompi_datatype_t **dtype,                                \
          struct ompi_op_base_module_1_0_0_t *module)                          \
  {                                                                      \
      int i;                                                             \
      type *a1 = (type *) in1;                                           \
      type *a2 = (type *) in2;                                           \
      type *b = (type *) out;                                            \
      for (i = 0; i < *count; ++i, ++b, ++a1, ++a2) {                    \
          b->real = a1->real + a2->real;                                 \
          b->imag = a1->imag + a2->imag;                                 \
      }                                                                  \
  }

#define COMPLEX_OP_FUNC_PROD_3BUF(type_name, type) \
  void ompi_op_base_3buff_prod_##type_name(void * restrict in1,          \
          void * restrict in2, void * restrict out, int *count,          \
          struct ompi_datatype_t **dtype,                                \
          struct ompi_op_base_module_1_0_0_t *module)                          \
  {                                                                      \
      int i;                                                             \
      type *a1 = (type *) in1;                                           \
      type *a2 = (type *) in2;                                           \
      type *b = (type *) out;                                            \
      for (i = 0; i < *count; ++i, ++b, ++a1, ++a2) {                    \
          b->real = a1->real * a2->real - a1->imag * a2->imag;           \
          b->imag = a1->imag * a2->real + a1->real * a2->imag;           \
      }                                                                  \
  }


/*
 * Since all the functions in this file are essentially identical, we
 * use a macro to substitute in names and types.  The core operation
 * in all functions that use this macro is the same.
 *
 * This macro is for (out = op(in1, in2))
 */
#define FUNC_FUNC_3BUF(name, type_name, type) \
  void ompi_op_base_3buff_##name##_##type_name(void * restrict in1,      \
          void * restrict in2, void * restrict out, int *count,          \
          struct ompi_datatype_t **dtype,                                \
          struct ompi_op_base_module_1_0_0_t *module)                          \
  {                                                                      \
      int i;                                                             \
      type *a1 = (type *) in1;                                           \
      type *a2 = (type *) in2;                                           \
      type *b = (type *) out;                                            \
      for (i = 0; i < *count; ++i) {                                     \
          *(b) = current_func(*(a1), *(a2));                             \
          ++b;                                                           \
          ++a1;                                                          \
          ++a2;                                                          \
      }                                                                  \
  }

/*
 * Since all the functions in this file are essentially identical, we
 * use a macro to substitute in names and types.  The core operation
 * in all functions that use this macro is the same.
 *
 * This macro is for minloc and maxloc
 */
/*
#define LOC_STRUCT(type_name, type1, type2) \
  typedef struct { \
      type1 v; \
      type2 k; \
  } ompi_op_predefined_##type_name##_t;
*/

#define LOC_FUNC_3BUF(name, type_name, op) \
  void ompi_op_base_3buff_##name##_##type_name(void * restrict in1,      \
          void * restrict in2, void * restrict out, int *count,          \
          struct ompi_datatype_t **dtype,                                \
          struct ompi_op_base_module_1_0_0_t *module)                          \
  { \
      int i; \
      ompi_op_predefined_##type_name##_t *a1 = (ompi_op_predefined_##type_name##_t*) in1; \
      ompi_op_predefined_##type_name##_t *a2 = (ompi_op_predefined_##type_name##_t*) in2; \
      ompi_op_predefined_##type_name##_t *b = (ompi_op_predefined_##type_name##_t*) out; \
      for (i = 0; i < *count; ++i, ++a1, ++a2, ++b ) { \
          if (a1->v op a2->v) { \
             b->v = a1->v; \
             b->k = a1->k; \
          } else if (a1->v == a2->v) { \
             b->v = a1->v; \
             b->k = (a2->k < a1->k ? a2->k : a1->k); \
          } else {  \
             b->v = a2->v; \
             b->k = a2->k; \
          }  \
      } \
  }

/*************************************************************************
 * Max
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) > (b) ? (a) : (b))
/* C integer */
FUNC_FUNC_3BUF(max,   int8_t,   int8_t)
FUNC_FUNC_3BUF(max,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF(max,  int16_t,  int16_t)
FUNC_FUNC_3BUF(max, uint16_t, uint16_t)
FUNC_FUNC_3BUF(max,  int32_t,  int32_t)
FUNC_FUNC_3BUF(max, uint32_t, uint32_t)
FUNC_FUNC_3BUF(max,  int64_t,  int64_t)
FUNC_FUNC_3BUF(max, uint64_t, uint64_t)
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC_3BUF(max, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC_3BUF(max, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC_3BUF(max, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC_3BUF(max, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC_3BUF(max, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC_3BUF(max, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Floating point */
FUNC_FUNC_3BUF(max, float, float)
FUNC_FUNC_3BUF(max, double, double)
#if HAVE_LONG_DOUBLE
FUNC_FUNC_3BUF(max, long_double, long double)
#endif
#if OMPI_HAVE_FORTRAN_REAL
FUNC_FUNC_3BUF(max, fortran_real, ompi_fortran_real_t)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
FUNC_FUNC_3BUF(max, fortran_double_precision, ompi_fortran_double_precision_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL2
FUNC_FUNC_3BUF(max, fortran_real2, ompi_fortran_real2_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
FUNC_FUNC_3BUF(max, fortran_real4, ompi_fortran_real4_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
FUNC_FUNC_3BUF(max, fortran_real8, ompi_fortran_real8_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
FUNC_FUNC_3BUF(max, fortran_real16, ompi_fortran_real16_t)
#endif


/*************************************************************************
 * Min
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) < (b) ? (a) : (b))
/* C integer */
FUNC_FUNC_3BUF(min,   int8_t,   int8_t)
FUNC_FUNC_3BUF(min,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF(min,  int16_t,  int16_t)
FUNC_FUNC_3BUF(min, uint16_t, uint16_t)
FUNC_FUNC_3BUF(min,  int32_t,  int32_t)
FUNC_FUNC_3BUF(min, uint32_t, uint32_t)
FUNC_FUNC_3BUF(min,  int64_t,  int64_t)
FUNC_FUNC_3BUF(min, uint64_t, uint64_t)
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC_3BUF(min, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC_3BUF(min, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC_3BUF(min, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC_3BUF(min, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC_3BUF(min, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC_3BUF(min, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Floating point */
FUNC_FUNC_3BUF(min, float, float)
FUNC_FUNC_3BUF(min, double, double)
#if HAVE_LONG_DOUBLE
FUNC_FUNC_3BUF(min, long_double, long double)
#endif
#if OMPI_HAVE_FORTRAN_REAL
FUNC_FUNC_3BUF(min, fortran_real, ompi_fortran_real_t)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
FUNC_FUNC_3BUF(min, fortran_double_precision, ompi_fortran_double_precision_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL2
FUNC_FUNC_3BUF(min, fortran_real2, ompi_fortran_real2_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
FUNC_FUNC_3BUF(min, fortran_real4, ompi_fortran_real4_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
FUNC_FUNC_3BUF(min, fortran_real8, ompi_fortran_real8_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
FUNC_FUNC_3BUF(min, fortran_real16, ompi_fortran_real16_t)
#endif

/*************************************************************************
 * Sum
 *************************************************************************/

/* C integer */
OP_FUNC_3BUF(sum,   int8_t,   int8_t, +=)
OP_FUNC_3BUF(sum,  uint8_t,  uint8_t, +=)
OP_FUNC_3BUF(sum,  int16_t,  int16_t, +=)
OP_FUNC_3BUF(sum, uint16_t, uint16_t, +=)
OP_FUNC_3BUF(sum,  int32_t,  int32_t, +=)
OP_FUNC_3BUF(sum, uint32_t, uint32_t, +=)
OP_FUNC_3BUF(sum,  int64_t,  int64_t, +=)
OP_FUNC_3BUF(sum, uint64_t, uint64_t, +=)
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
OP_FUNC_3BUF(sum, fortran_integer, ompi_fortran_integer_t, +)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
OP_FUNC_3BUF(sum, fortran_integer1, ompi_fortran_integer1_t, +)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
OP_FUNC_3BUF(sum, fortran_integer2, ompi_fortran_integer2_t, +)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
OP_FUNC_3BUF(sum, fortran_integer4, ompi_fortran_integer4_t, +)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
OP_FUNC_3BUF(sum, fortran_integer8, ompi_fortran_integer8_t, +)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
OP_FUNC_3BUF(sum, fortran_integer16, ompi_fortran_integer16_t, +)
#endif
/* Floating point */
OP_FUNC_3BUF(sum, float, float, +)
OP_FUNC_3BUF(sum, double, double, +)
#if HAVE_LONG_DOUBLE
OP_FUNC_3BUF(sum, long_double, long double, +)
#endif
#if OMPI_HAVE_FORTRAN_REAL
OP_FUNC_3BUF(sum, fortran_real, ompi_fortran_real_t, +)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
OP_FUNC_3BUF(sum, fortran_double_precision, ompi_fortran_double_precision_t, +)
#endif
#if OMPI_HAVE_FORTRAN_REAL2
OP_FUNC_3BUF(sum, fortran_real2, ompi_fortran_real2_t, +)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
OP_FUNC_3BUF(sum, fortran_real4, ompi_fortran_real4_t, +)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
OP_FUNC_3BUF(sum, fortran_real8, ompi_fortran_real8_t, +)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
OP_FUNC_3BUF(sum, fortran_real16, ompi_fortran_real16_t, +)
#endif
/* Complex */
#if OMPI_HAVE_FORTRAN_REAL && OMPI_HAVE_FORTRAN_COMPLEX
COMPLEX_OP_FUNC_SUM_3BUF(fortran_complex, ompi_fortran_complex_t)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION && OMPI_HAVE_FORTRAN_COMPLEX
COMPLEX_OP_FUNC_SUM_3BUF(fortran_double_complex, ompi_fortran_double_complex_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL4 && OMPI_HAVE_FORTRAN_COMPLEX8
COMPLEX_OP_FUNC_SUM_3BUF(fortran_complex8, ompi_fortran_complex8_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL8 && OMPI_HAVE_FORTRAN_COMPLEX16
COMPLEX_OP_FUNC_SUM_3BUF(fortran_complex16, ompi_fortran_complex16_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL16 && OMPI_HAVE_FORTRAN_COMPLEX32
COMPLEX_OP_FUNC_SUM_3BUF(fortran_complex32, ompi_fortran_complex32_t)
#endif

/*************************************************************************
 * Product
 *************************************************************************/

/* C integer */
OP_FUNC_3BUF(prod,   int8_t,   int8_t, *=)
OP_FUNC_3BUF(prod,  uint8_t,  uint8_t, *=)
OP_FUNC_3BUF(prod,  int16_t,  int16_t, *=)
OP_FUNC_3BUF(prod, uint16_t, uint16_t, *=)
OP_FUNC_3BUF(prod,  int32_t,  int32_t, *=)
OP_FUNC_3BUF(prod, uint32_t, uint32_t, *=)
OP_FUNC_3BUF(prod,  int64_t,  int64_t, *=)
OP_FUNC_3BUF(prod, uint64_t, uint64_t, *=)
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
OP_FUNC_3BUF(prod, fortran_integer, ompi_fortran_integer_t, *)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
OP_FUNC_3BUF(prod, fortran_integer1, ompi_fortran_integer1_t, *)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
OP_FUNC_3BUF(prod, fortran_integer2, ompi_fortran_integer2_t, *)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
OP_FUNC_3BUF(prod, fortran_integer4, ompi_fortran_integer4_t, *)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
OP_FUNC_3BUF(prod, fortran_integer8, ompi_fortran_integer8_t, *)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
OP_FUNC_3BUF(prod, fortran_integer16, ompi_fortran_integer16_t, *)
#endif
/* Floating point */
OP_FUNC_3BUF(prod, float, float, *)
OP_FUNC_3BUF(prod, double, double, *)
#if HAVE_LONG_DOUBLE
OP_FUNC_3BUF(prod, long_double, long double, *)
#endif
#if OMPI_HAVE_FORTRAN_REAL
OP_FUNC_3BUF(prod, fortran_real, ompi_fortran_real_t, *)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
OP_FUNC_3BUF(prod, fortran_double_precision, ompi_fortran_double_precision_t, *)
#endif
#if OMPI_HAVE_FORTRAN_REAL2
OP_FUNC_3BUF(prod, fortran_real2, ompi_fortran_real2_t, *)
#endif
#if OMPI_HAVE_FORTRAN_REAL4
OP_FUNC_3BUF(prod, fortran_real4, ompi_fortran_real4_t, *)
#endif
#if OMPI_HAVE_FORTRAN_REAL8
OP_FUNC_3BUF(prod, fortran_real8, ompi_fortran_real8_t, *)
#endif
#if OMPI_HAVE_FORTRAN_REAL16
OP_FUNC_3BUF(prod, fortran_real16, ompi_fortran_real16_t, *)
#endif
/* Complex */
#if OMPI_HAVE_FORTRAN_REAL && OMPI_HAVE_FORTRAN_COMPLEX
COMPLEX_OP_FUNC_PROD_3BUF(fortran_complex, ompi_fortran_complex_t)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION && OMPI_HAVE_FORTRAN_COMPLEX
COMPLEX_OP_FUNC_PROD_3BUF(fortran_double_complex, ompi_fortran_double_complex_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL4 && OMPI_HAVE_FORTRAN_COMPLEX8
COMPLEX_OP_FUNC_PROD_3BUF(fortran_complex8, ompi_fortran_complex8_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL8 && OMPI_HAVE_FORTRAN_COMPLEX16
COMPLEX_OP_FUNC_PROD_3BUF(fortran_complex16, ompi_fortran_complex16_t)
#endif
#if OMPI_HAVE_FORTRAN_REAL16 && OMPI_HAVE_FORTRAN_COMPLEX32
COMPLEX_OP_FUNC_PROD_3BUF(fortran_complex32, ompi_fortran_complex32_t)
#endif

/*************************************************************************
 * Logical AND
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) && (b))
/* C integer */
FUNC_FUNC_3BUF(land,   int8_t,   int8_t)
FUNC_FUNC_3BUF(land,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF(land,  int16_t,  int16_t)
FUNC_FUNC_3BUF(land, uint16_t, uint16_t)
FUNC_FUNC_3BUF(land,  int32_t,  int32_t)
FUNC_FUNC_3BUF(land, uint32_t, uint32_t)
FUNC_FUNC_3BUF(land,  int64_t,  int64_t)
FUNC_FUNC_3BUF(land, uint64_t, uint64_t)
/* Logical */
#if OMPI_HAVE_FORTRAN_LOGICAL
FUNC_FUNC_3BUF(land, fortran_logical, ompi_fortran_logical_t)
#endif
/* C++ bool */
FUNC_FUNC_3BUF(land, bool, bool)

/*************************************************************************
 * Logical OR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) || (b))
/* C integer */
FUNC_FUNC_3BUF(lor,   int8_t,   int8_t)
FUNC_FUNC_3BUF(lor,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF(lor,  int16_t,  int16_t)
FUNC_FUNC_3BUF(lor, uint16_t, uint16_t)
FUNC_FUNC_3BUF(lor,  int32_t,  int32_t)
FUNC_FUNC_3BUF(lor, uint32_t, uint32_t)
FUNC_FUNC_3BUF(lor,  int64_t,  int64_t)
FUNC_FUNC_3BUF(lor, uint64_t, uint64_t)
/* Logical */
#if OMPI_HAVE_FORTRAN_LOGICAL
FUNC_FUNC_3BUF(lor, fortran_logical, ompi_fortran_logical_t)
#endif
/* C++ bool */
FUNC_FUNC_3BUF(lor, bool, bool)

/*************************************************************************
 * Logical XOR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a ? 1 : 0) ^ (b ? 1: 0))
/* C integer */
FUNC_FUNC_3BUF(lxor,   int8_t,   int8_t)
FUNC_FUNC_3BUF(lxor,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF(lxor,  int16_t,  int16_t)
FUNC_FUNC_3BUF(lxor, uint16_t, uint16_t)
FUNC_FUNC_3BUF(lxor,  int32_t,  int32_t)
FUNC_FUNC_3BUF(lxor, uint32_t, uint32_t)
FUNC_FUNC_3BUF(lxor,  int64_t,  int64_t)
FUNC_FUNC_3BUF(lxor, uint64_t, uint64_t)
/* Logical */
#if OMPI_HAVE_FORTRAN_LOGICAL
FUNC_FUNC_3BUF(lxor, fortran_logical, ompi_fortran_logical_t)
#endif
/* C++ bool */
FUNC_FUNC_3BUF(lxor, bool, bool)

/*************************************************************************
 * Bitwise AND
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) & (b))
/* C integer */
FUNC_FUNC_3BUF(band,   int8_t,   int8_t)
FUNC_FUNC_3BUF(band,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF(band,  int16_t,  int16_t)
FUNC_FUNC_3BUF(band, uint16_t, uint16_t)
FUNC_FUNC_3BUF(band,  int32_t,  int32_t)
FUNC_FUNC_3BUF(band, uint32_t, uint32_t)
FUNC_FUNC_3BUF(band,  int64_t,  int64_t)
FUNC_FUNC_3BUF(band, uint64_t, uint64_t)
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC_3BUF(band, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC_3BUF(band, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC_3BUF(band, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC_3BUF(band, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC_3BUF(band, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC_3BUF(band, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Byte */
FUNC_FUNC_3BUF(band, byte, char)

/*************************************************************************
 * Bitwise OR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) | (b))
/* C integer */
FUNC_FUNC_3BUF(bor,   int8_t,   int8_t)
FUNC_FUNC_3BUF(bor,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF(bor,  int16_t,  int16_t)
FUNC_FUNC_3BUF(bor, uint16_t, uint16_t)
FUNC_FUNC_3BUF(bor,  int32_t,  int32_t)
FUNC_FUNC_3BUF(bor, uint32_t, uint32_t)
FUNC_FUNC_3BUF(bor,  int64_t,  int64_t)
FUNC_FUNC_3BUF(bor, uint64_t, uint64_t)
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC_3BUF(bor, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC_3BUF(bor, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC_3BUF(bor, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC_3BUF(bor, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC_3BUF(bor, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC_3BUF(bor, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Byte */
FUNC_FUNC_3BUF(bor, byte, char)

/*************************************************************************
 * Bitwise XOR
 *************************************************************************/

#undef current_func
#define current_func(a, b) ((a) ^ (b))
/* C integer */
FUNC_FUNC_3BUF(bxor,   int8_t,   int8_t)
FUNC_FUNC_3BUF(bxor,  uint8_t,  uint8_t)
FUNC_FUNC_3BUF(bxor,  int16_t,  int16_t)
FUNC_FUNC_3BUF(bxor, uint16_t, uint16_t)
FUNC_FUNC_3BUF(bxor,  int32_t,  int32_t)
FUNC_FUNC_3BUF(bxor, uint32_t, uint32_t)
FUNC_FUNC_3BUF(bxor,  int64_t,  int64_t)
FUNC_FUNC_3BUF(bxor, uint64_t, uint64_t)
/* Fortran integer */
#if OMPI_HAVE_FORTRAN_INTEGER
FUNC_FUNC_3BUF(bxor, fortran_integer, ompi_fortran_integer_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
FUNC_FUNC_3BUF(bxor, fortran_integer1, ompi_fortran_integer1_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
FUNC_FUNC_3BUF(bxor, fortran_integer2, ompi_fortran_integer2_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
FUNC_FUNC_3BUF(bxor, fortran_integer4, ompi_fortran_integer4_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
FUNC_FUNC_3BUF(bxor, fortran_integer8, ompi_fortran_integer8_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
FUNC_FUNC_3BUF(bxor, fortran_integer16, ompi_fortran_integer16_t)
#endif
/* Byte */
FUNC_FUNC_3BUF(bxor, byte, char)

/*************************************************************************
 * Min and max location "pair" datatypes
 *************************************************************************/

/*
#if OMPI_HAVE_FORTRAN_REAL
LOC_STRUCT_3BUF(2real, ompi_fortran_real_t, ompi_fortran_real_t)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
LOC_STRUCT_3BUF(2double_precision, ompi_fortran_double_precision_t, ompi_fortran_double_precision_t)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER
LOC_STRUCT_3BUF(2integer, ompi_fortran_integer_t, ompi_fortran_integer_t)
#endif
LOC_STRUCT_3BUF(float_int, float, int)
LOC_STRUCT_3BUF(double_int, double, int)
LOC_STRUCT_3BUF(long_int, long, int)
LOC_STRUCT_3BUF(2int, int, int)
LOC_STRUCT_3BUF(short_int, short, int)
#if HAVE_LONG_DOUBLE
LOC_STRUCT_3BUF(long_double_int, long double, int)
#endif
*/

/*************************************************************************
 * Max location
 *************************************************************************/

#if OMPI_HAVE_FORTRAN_REAL
LOC_FUNC_3BUF(maxloc, 2real, >)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
LOC_FUNC_3BUF(maxloc, 2double_precision, >)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER
LOC_FUNC_3BUF(maxloc, 2integer, >)
#endif
LOC_FUNC_3BUF(maxloc, float_int, >)
LOC_FUNC_3BUF(maxloc, double_int, >)
LOC_FUNC_3BUF(maxloc, long_int, >)
LOC_FUNC_3BUF(maxloc, 2int, >)
LOC_FUNC_3BUF(maxloc, short_int, >)
#if HAVE_LONG_DOUBLE
LOC_FUNC_3BUF(maxloc, long_double_int, >)
#endif

/*************************************************************************
 * Min location
 *************************************************************************/

#if OMPI_HAVE_FORTRAN_REAL
LOC_FUNC_3BUF(minloc, 2real, <)
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
LOC_FUNC_3BUF(minloc, 2double_precision, <)
#endif
#if OMPI_HAVE_FORTRAN_INTEGER
LOC_FUNC_3BUF(minloc, 2integer, <)
#endif
LOC_FUNC_3BUF(minloc, float_int, <)
LOC_FUNC_3BUF(minloc, double_int, <)
LOC_FUNC_3BUF(minloc, long_int, <)
LOC_FUNC_3BUF(minloc, 2int, <)
LOC_FUNC_3BUF(minloc, short_int, <)
#if HAVE_LONG_DOUBLE
LOC_FUNC_3BUF(minloc, long_double_int, <)
#endif

/*
 * Helpful defines, because there's soooo many names!
 *
 * **NOTE** These #define's are strictly ordered!  A series of macros
 * are built up to assemble a list of function names (or NULLs) that
 * are put into the intrinsict ompi_op_t's in the middle of this file.
 * The order of these function names is critical, and must be the same
 * as the OMPI_OP_BASE_TYPE_* enums in ompi/mca/op/op.h (i.e., the
 * enum's starting with OMPI_OP_BASE_TYPE_UNSIGNED_CHAR).
 */

/** C integer ***********************************************************/
#define C_INTEGER_NULL \
  NULL, /* OMPI_OP_BASE_TYPE_UNSIGNED_CHAR */ \
  NULL, /* OMPI_OP_BASE_TYPE_SIGNED_CHAR */ \
  NULL, /* OMPI_OP_BASE_TYPE_INT */ \
  NULL, /* OMPI_OP_BASE_TYPE_LONG */ \
  NULL, /* OMPI_OP_BASE_TYPE_SHORT */ \
  NULL, /* OMPI_OP_BASE_TYPE_UNSIGNED_SHORT */ \
  NULL, /* OMPI_OP_BASE_TYPE_UNSIGNED */ \
  NULL  /* OMPI_OP_BASE_TYPE_UNSIGNED_LONG */

#define C_INTEGER_NULL_3BUFF \
  NULL, /* OMPI_OP_BASE_TYPE_UNSIGNED_CHAR */ \
  NULL, /* OMPI_OP_BASE_TYPE_SIGNED_CHAR */ \
  NULL, /* OMPI_OP_BASE_TYPE_INT */ \
  NULL, /* OMPI_OP_BASE_TYPE_LONG */ \
  NULL, /* OMPI_OP_BASE_TYPE_SHORT */ \
  NULL, /* OMPI_OP_BASE_TYPE_UNSIGNED_SHORT */ \
  NULL, /* OMPI_OP_BASE_TYPE_UNSIGNED */ \
  NULL  /* OMPI_OP_BASE_TYPE_UNSIGNED_LONG */

#define C_INTEGER(name) \
  ompi_op_base_##name##_int8_t,  /* OMPI_OP_BASE_TYPE_UNSIGNED_CHAR */ \
  ompi_op_base_##name##_uint8_t,    /* OMPI_OP_BASE_TYPE_SIGNED_CHAR */ \
  ompi_op_base_##name##_int16_t,            /* OMPI_OP_BASE_TYPE_INT */ \
  ompi_op_base_##name##_uint16_t,           /* OMPI_OP_BASE_TYPE_LONG */ \
  ompi_op_base_##name##_int32_t,          /* OMPI_OP_BASE_TYPE_SHORT */ \
  ompi_op_base_##name##_uint32_t, /* OMPI_OP_BASE_TYPE_UNSIGNED_SHORT */ \
  ompi_op_base_##name##_int64_t,       /* OMPI_OP_BASE_TYPE_UNSIGNED */ \
  ompi_op_base_##name##_uint64_t  /* OMPI_OP_BASE_TYPE_UNSIGNED_LONG */

#define C_INTEGER_3BUFF(name) \
  ompi_op_base_3buff_##name##_int8_t,  /* OMPI_OP_BASE_TYPE_UNSIGNED_CHAR */ \
  ompi_op_base_3buff_##name##_uint8_t,    /* OMPI_OP_BASE_TYPE_SIGNED_CHAR */ \
  ompi_op_base_3buff_##name##_int16_t,            /* OMPI_OP_BASE_TYPE_INT */ \
  ompi_op_base_3buff_##name##_uint16_t,           /* OMPI_OP_BASE_TYPE_LONG */ \
  ompi_op_base_3buff_##name##_int32_t,          /* OMPI_OP_BASE_TYPE_SHORT */ \
  ompi_op_base_3buff_##name##_uint32_t, /* OMPI_OP_BASE_TYPE_UNSIGNED_SHORT */ \
  ompi_op_base_3buff_##name##_int64_t,       /* OMPI_OP_BASE_TYPE_UNSIGNED */ \
  ompi_op_base_3buff_##name##_uint64_t  /* OMPI_OP_BASE_TYPE_UNSIGNED_LONG */

/** All the Fortran integers ********************************************/

#if OMPI_HAVE_FORTRAN_INTEGER
#define FORTRAN_INTEGER_PLAIN(name) ompi_op_base_##name##_fortran_integer
#define FORTRAN_INTEGER_PLAIN_3BUFF(name) ompi_op_base_3buff_##name##_fortran_integer
#else
#define FORTRAN_INTEGER_PLAIN(name) NULL
#define FORTRAN_INTEGER_PLAIN_3BUFF(name) NULL
#endif
#if OMPI_HAVE_FORTRAN_INTEGER1
#define FORTRAN_INTEGER1(name) ompi_op_base_##name##_fortran_integer1
#define FORTRAN_INTEGER1_3BUFF(name) ompi_op_base_3buff_##name##_fortran_integer1
#else
#define FORTRAN_INTEGER1(name) NULL
#define FORTRAN_INTEGER1_3BUFF(name) NULL
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
#define FORTRAN_INTEGER2(name) ompi_op_base_##name##_fortran_integer2
#define FORTRAN_INTEGER2_3BUFF(name) ompi_op_base_3buff_##name##_fortran_integer2
#else
#define FORTRAN_INTEGER2(name) NULL
#define FORTRAN_INTEGER2_3BUFF(name) NULL
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
#define FORTRAN_INTEGER4(name) ompi_op_base_##name##_fortran_integer4
#define FORTRAN_INTEGER4_3BUFF(name) ompi_op_base_3buff_##name##_fortran_integer4
#else
#define FORTRAN_INTEGER4(name) NULL
#define FORTRAN_INTEGER4_3BUFF(name) NULL
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
#define FORTRAN_INTEGER8(name) ompi_op_base_##name##_fortran_integer8
#define FORTRAN_INTEGER8_3BUFF(name) ompi_op_base_3buff_##name##_fortran_integer8
#else
#define FORTRAN_INTEGER8(name) NULL
#define FORTRAN_INTEGER8_3BUFF(name) NULL
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
#define FORTRAN_INTEGER16(name) ompi_op_base_##name##_fortran_integer16
#define FORTRAN_INTEGER16_3BUFF(name) ompi_op_base_3buff_##name##_fortran_integer16
#else
#define FORTRAN_INTEGER16(name) NULL
#define FORTRAN_INTEGER16_3BUFF(name) NULL
#endif
#define FORTRAN_INTEGER(name) \
  FORTRAN_INTEGER_PLAIN(name),      /* OMPI_OP_BASE_TYPE_INTEGER */ \
  FORTRAN_INTEGER1(name),           /* OMPI_OP_BASE_TYPE_INTEGER1 */ \
  FORTRAN_INTEGER2(name),           /* OMPI_OP_BASE_TYPE_INTEGER2 */ \
  FORTRAN_INTEGER4(name),           /* OMPI_OP_BASE_TYPE_INTEGER4 */ \
  FORTRAN_INTEGER8(name),           /* OMPI_OP_BASE_TYPE_INTEGER8 */ \
  FORTRAN_INTEGER16(name)           /* OMPI_OP_BASE_TYPE_INTEGER16 */

#define FORTRAN_INTEGER_3BUFF(name) \
  FORTRAN_INTEGER_PLAIN_3BUFF(name),      /* OMPI_OP_BASE_TYPE_INTEGER */ \
  FORTRAN_INTEGER1_3BUFF(name),           /* OMPI_OP_BASE_TYPE_INTEGER1 */ \
  FORTRAN_INTEGER2_3BUFF(name),           /* OMPI_OP_BASE_TYPE_INTEGER2 */ \
  FORTRAN_INTEGER4_3BUFF(name),           /* OMPI_OP_BASE_TYPE_INTEGER4 */ \
  FORTRAN_INTEGER8_3BUFF(name),           /* OMPI_OP_BASE_TYPE_INTEGER8 */ \
  FORTRAN_INTEGER16_3BUFF(name)           /* OMPI_OP_BASE_TYPE_INTEGER16 */

#define FORTRAN_INTEGER_NULL \
  NULL,  /* OMPI_OP_BASE_TYPE_INTEGER */ \
  NULL,  /* OMPI_OP_BASE_TYPE_INTEGER1 */ \
  NULL,  /* OMPI_OP_BASE_TYPE_INTEGER2 */ \
  NULL,  /* OMPI_OP_BASE_TYPE_INTEGER4 */ \
  NULL,  /* OMPI_OP_BASE_TYPE_INTEGER8 */ \
  NULL  /* OMPI_OP_BASE_TYPE_INTEGER16 */

#define FORTRAN_INTEGER_NULL_3BUFF \
  NULL,  /* OMPI_OP_BASE_TYPE_INTEGER */ \
  NULL,  /* OMPI_OP_BASE_TYPE_INTEGER1 */ \
  NULL,  /* OMPI_OP_BASE_TYPE_INTEGER2 */ \
  NULL,  /* OMPI_OP_BASE_TYPE_INTEGER4 */ \
  NULL,  /* OMPI_OP_BASE_TYPE_INTEGER8 */ \
  NULL  /* OMPI_OP_BASE_TYPE_INTEGER16 */

/** All the Fortran reals ***********************************************/

#if OMPI_HAVE_FORTRAN_REAL
#define FLOATING_POINT_FORTRAN_REAL_PLAIN(name) ompi_op_base_##name##_fortran_real
#define FLOATING_POINT_FORTRAN_REAL_PLAIN_3BUFF(name) ompi_op_base_3buff_##name##_fortran_real
#else
#define FLOATING_POINT_FORTRAN_REAL_PLAIN(name) NULL
#define FLOATING_POINT_FORTRAN_REAL_PLAIN_3BUFF(name) NULL
#endif
#if OMPI_HAVE_FORTRAN_REAL2
#define FLOATING_POINT_FORTRAN_REAL2(name) ompi_op_base_##name##_fortran_real2
#define FLOATING_POINT_FORTRAN_REAL2_3BUFF(name) ompi_op_base_3buff_##name##_fortran_real2
#else
#define FLOATING_POINT_FORTRAN_REAL2(name) NULL
#define FLOATING_POINT_FORTRAN_REAL2_3BUFF(name) NULL
#endif
#if OMPI_HAVE_FORTRAN_REAL4
#define FLOATING_POINT_FORTRAN_REAL4(name) ompi_op_base_##name##_fortran_real4
#define FLOATING_POINT_FORTRAN_REAL4_3BUFF(name) ompi_op_base_3buff_##name##_fortran_real4
#else
#define FLOATING_POINT_FORTRAN_REAL4(name) NULL
#define FLOATING_POINT_FORTRAN_REAL4_3BUFF(name) NULL
#endif
#if OMPI_HAVE_FORTRAN_REAL8
#define FLOATING_POINT_FORTRAN_REAL8(name) ompi_op_base_##name##_fortran_real8
#define FLOATING_POINT_FORTRAN_REAL8_3BUFF(name) ompi_op_base_3buff_##name##_fortran_real8
#else
#define FLOATING_POINT_FORTRAN_REAL8(name) NULL
#define FLOATING_POINT_FORTRAN_REAL8_3BUFF(name) NULL
#endif
/* If:
   - we have fortran REAL*16, *and*
   - fortran REAL*16 matches the bit representation of the
     corresponding C type
   Only then do we put in function pointers for REAL*16 reductions.
   Otherwise, just put in NULL. */
#if OMPI_HAVE_FORTRAN_REAL16 && OMPI_REAL16_MATCHES_C
#define FLOATING_POINT_FORTRAN_REAL16(name) ompi_op_base_##name##_fortran_real16
#define FLOATING_POINT_FORTRAN_REAL16_3BUFF(name) ompi_op_base_3buff_##name##_fortran_real16
#else
#define FLOATING_POINT_FORTRAN_REAL16(name) NULL
#define FLOATING_POINT_FORTRAN_REAL16_3BUFF(name) NULL
#endif

#define FLOATING_POINT_FORTRAN_REAL(name) \
  FLOATING_POINT_FORTRAN_REAL_PLAIN(name),      /* OMPI_OP_BASE_TYPE_REAL */ \
  FLOATING_POINT_FORTRAN_REAL2(name),           /* OMPI_OP_BASE_TYPE_REAL2 */ \
  FLOATING_POINT_FORTRAN_REAL4(name),           /* OMPI_OP_BASE_TYPE_REAL4 */ \
  FLOATING_POINT_FORTRAN_REAL8(name),           /* OMPI_OP_BASE_TYPE_REAL8 */ \
  FLOATING_POINT_FORTRAN_REAL16(name)           /* OMPI_OP_BASE_TYPE_REAL16 */

#define FLOATING_POINT_FORTRAN_REAL_3BUFF(name) \
  FLOATING_POINT_FORTRAN_REAL_PLAIN_3BUFF(name),      /* OMPI_OP_BASE_TYPE_REAL */ \
  FLOATING_POINT_FORTRAN_REAL2_3BUFF(name),           /* OMPI_OP_BASE_TYPE_REAL2 */ \
  FLOATING_POINT_FORTRAN_REAL4_3BUFF(name),           /* OMPI_OP_BASE_TYPE_REAL4 */ \
  FLOATING_POINT_FORTRAN_REAL8_3BUFF(name),           /* OMPI_OP_BASE_TYPE_REAL8 */ \
  FLOATING_POINT_FORTRAN_REAL16_3BUFF(name)           /* OMPI_OP_BASE_TYPE_REAL16 */

/** Fortran double precision ********************************************/

#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
#define FLOATING_POINT_FORTRAN_DOUBLE_PRECISION(name) \
    ompi_op_base_##name##_fortran_double_precision
#define FLOATING_POINT_FORTRAN_DOUBLE_PRECISION_3BUFF(name) \
    ompi_op_base_3buff_##name##_fortran_double_precision
#else
#define FLOATING_POINT_FORTRAN_DOUBLE_PRECISION(name) NULL
#define FLOATING_POINT_FORTRAN_DOUBLE_PRECISION_3BUFF(name) NULL
#endif

/** Floating point, including all the Fortran reals *********************/

#define FLOATING_POINT(name) \
  ompi_op_base_##name##_float,                    /* OMPI_OP_BASE_TYPE_FLOAT */\
  ompi_op_base_##name##_double,                   /* OMPI_OP_BASE_TYPE_DOUBLE */\
  FLOATING_POINT_FORTRAN_REAL(name),                 /* OMPI_OP_BASE_TYPE_REAL */ \
  FLOATING_POINT_FORTRAN_DOUBLE_PRECISION(name),     /* OMPI_OP_BASE_TYPE_DOUBLE_PRECISION */ \
  ompi_op_base_##name##_long_double               /* OMPI_OP_BASE_TYPE_LONG_DOUBLE */

#define FLOATING_POINT_3BUFF(name) \
  ompi_op_base_3buff_##name##_float,                    /* OMPI_OP_BASE_TYPE_FLOAT */\
  ompi_op_base_3buff_##name##_double,                   /* OMPI_OP_BASE_TYPE_DOUBLE */\
  FLOATING_POINT_FORTRAN_REAL_3BUFF(name),                 /* OMPI_OP_BASE_TYPE_REAL */ \
  FLOATING_POINT_FORTRAN_DOUBLE_PRECISION_3BUFF(name),     /* OMPI_OP_BASE_TYPE_DOUBLE_PRECISION */ \
  ompi_op_base_3buff_##name##_long_double               /* OMPI_OP_BASE_TYPE_LONG_DOUBLE */

#define FLOATING_POINT_NULL \
  NULL, /* OMPI_OP_BASE_TYPE_FLOAT */ \
  NULL, /* OMPI_OP_BASE_TYPE_DOUBLE */ \
  NULL, /* OMPI_OP_BASE_TYPE_REAL */ \
  NULL, /* OMPI_OP_BASE_TYPE_REAL2 */ \
  NULL, /* OMPI_OP_BASE_TYPE_REAL4 */ \
  NULL, /* OMPI_OP_BASE_TYPE_REAL8 */ \
  NULL, /* OMPI_OP_BASE_TYPE_REAL16 */ \
  NULL, /* OMPI_OP_BASE_TYPE_DOUBLE_PRECISION */ \
  NULL  /* OMPI_OP_BASE_TYPE_LONG_DOUBLE */

#define FLOATING_POINT_NULL_3BUFF \
  NULL, /* OMPI_OP_BASE_TYPE_FLOAT */ \
  NULL, /* OMPI_OP_BASE_TYPE_DOUBLE */ \
  NULL, /* OMPI_OP_BASE_TYPE_REAL */ \
  NULL, /* OMPI_OP_BASE_TYPE_REAL2 */ \
  NULL, /* OMPI_OP_BASE_TYPE_REAL4 */ \
  NULL, /* OMPI_OP_BASE_TYPE_REAL8 */ \
  NULL, /* OMPI_OP_BASE_TYPE_REAL16 */ \
  NULL, /* OMPI_OP_BASE_TYPE_DOUBLE_PRECISION */ \
  NULL  /* OMPI_OP_BASE_TYPE_LONG_DOUBLE */

/** Fortran logical *****************************************************/

#if OMPI_HAVE_FORTRAN_LOGICAL
#define FORTRAN_LOGICAL(name) \
  ompi_op_base_##name##_fortran_logical  /* OMPI_OP_BASE_TYPE_LOGICAL */
#define FORTRAN_LOGICAL_3BUFF(name) \
  ompi_op_base_3buff_##name##_fortran_logical  /* OMPI_OP_BASE_TYPE_LOGICAL */
#else
#define FORTRAN_LOGICAL(name) NULL
#define FORTRAN_LOGICAL_3BUFF(name) NULL
#endif
#define LOGICAL(name) \
  FORTRAN_LOGICAL(name), \
  ompi_op_base_##name##_bool  /* OMPI_OP_BASE_TYPE_BOOL */
#define LOGICAL_3BUFF(name) \
  FORTRAN_LOGICAL_3BUFF(name), \
  ompi_op_base_3buff_##name##_bool  /* OMPI_OP_BASE_TYPE_BOOL */

#define LOGICAL_NULL \
  NULL,  /* OMPI_OP_BASE_TYPE_LOGICAL */ \
  NULL   /* OMPI_OP_BASE_TYPE_BOOL */

#define LOGICAL_NULL_3BUFF \
  NULL,  /* OMPI_OP_BASE_TYPE_LOGICAL */ \
  NULL   /* OMPI_OP_BASE_TYPE_BOOL */

/** Fortran complex *****************************************************/

#if OMPI_HAVE_FORTRAN_REAL && OMPI_HAVE_FORTRAN_COMPLEX
#define COMPLEX_PLAIN(name) ompi_op_base_##name##_fortran_complex
#define COMPLEX_PLAIN_3BUFF(name) ompi_op_base_3buff_##name##_fortran_complex
#else
#define COMPLEX_PLAIN(name) NULL
#define COMPLEX_PLAIN_3BUFF(name) NULL
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION && OMPI_HAVE_FORTRAN_COMPLEX
#define COMPLEX_DOUBLE(name) ompi_op_base_##name##_fortran_double_complex
#define COMPLEX_DOUBLE_3BUFF(name) ompi_op_base_3buff_##name##_fortran_double_complex
#else
#define COMPLEX_DOUBLE(name) NULL
#define COMPLEX_DOUBLE_3BUFF(name) NULL
#endif
#if OMPI_HAVE_FORTRAN_REAL4 && OMPI_HAVE_FORTRAN_COMPLEX8
#define COMPLEX8(name) ompi_op_base_##name##_fortran_complex8
#define COMPLEX8_3BUFF(name) ompi_op_base_3buff_##name##_fortran_complex8
#else
#define COMPLEX8(name) NULL
#define COMPLEX8_3BUFF(name) NULL
#endif
#if OMPI_HAVE_FORTRAN_REAL8 && OMPI_HAVE_FORTRAN_COMPLEX16
#define COMPLEX16(name) ompi_op_base_##name##_fortran_complex16
#define COMPLEX16_3BUFF(name) ompi_op_base_3buff_##name##_fortran_complex16
#else
#define COMPLEX16(name) NULL
#define COMPLEX16_3BUFF(name) NULL
#endif
/* If:
   - we have fortran REAL*16, *and*
   - fortran REAL*16 matches the bit representation of the
     corresponding C type, *and*
   - we have fortran COMPILEX*32
   Only then do we put in function pointers for COMPLEX*32 reductions.
   Otherwise, just put in NULL. */
#if OMPI_HAVE_FORTRAN_REAL16 && OMPI_REAL16_MATCHES_C && OMPI_HAVE_FORTRAN_COMPLEX32
#define COMPLEX32(name) ompi_op_base_##name##_fortran_complex32
#define COMPLEX32_3BUFF(name) ompi_op_base_3buff_##name##_fortran_complex32
#else
#define COMPLEX32(name) NULL
#define COMPLEX32_3BUFF(name) NULL
#endif

#define COMPLEX(name) \
  COMPLEX_PLAIN(name),  /* OMPI_OP_BASE_TYPE_COMPLEX */ \
  COMPLEX_DOUBLE(name), /* OMPI_OP_BASE_TYPE_DOUBLE_COMPLEX */ \
  COMPLEX8(name),       /* OMPI_OP_BASE_TYPE_COMPLEX8 */ \
  COMPLEX16(name),      /* OMPI_OP_BASE_TYPE_COMPLEX16 */ \
  COMPLEX32(name)       /* OMPI_OP_BASE_TYPE_COMPLEX32 */

#define COMPLEX_3BUFF(name) \
  COMPLEX_PLAIN_3BUFF(name),  /* OMPI_OP_BASE_TYPE_COMPLEX */ \
  COMPLEX_DOUBLE_3BUFF(name), /* OMPI_OP_BASE_TYPE_DOUBLE_COMPLEX */ \
  COMPLEX8_3BUFF(name),       /* OMPI_OP_BASE_TYPE_COMPLEX8 */ \
  COMPLEX16_3BUFF(name),      /* OMPI_OP_BASE_TYPE_COMPLEX16 */ \
  COMPLEX32_3BUFF(name)       /* OMPI_OP_BASE_TYPE_COMPLEX32 */

#define COMPLEX_NULL \
  NULL,  /* OMPI_OP_BASE_TYPE_COMPLEX */ \
  NULL,  /* OMPI_OP_BASE_TYPE_DOUBLE_COMPLEX */ \
  NULL,  /* OMPI_OP_BASE_TYPE_COMPLEX8 */ \
  NULL,  /* OMPI_OP_BASE_TYPE_COMPLEX16 */ \
  NULL   /* OMPI_OP_BASE_TYPE_COMPLEX32 */

#define COMPLEX_NULL_3BUFF \
  NULL,  /* OMPI_OP_BASE_TYPE_COMPLEX */ \
  NULL,  /* OMPI_OP_BASE_TYPE_DOUBLE_COMPLEX */ \
  NULL,  /* OMPI_OP_BASE_TYPE_COMPLEX8 */ \
  NULL,  /* OMPI_OP_BASE_TYPE_COMPLEX16 */ \
  NULL   /* OMPI_OP_BASE_TYPE_COMPLEX32 */

/** Byte ****************************************************************/

#define BYTE(name) \
  ompi_op_base_##name##_byte  /* OMPI_OP_BASE_TYPE_BYTE */
#define BYTE_3BUFF(name) \
  ompi_op_base_3buff_##name##_byte  /* OMPI_OP_BASE_TYPE_BYTE */

#define BYTE_NULL \
  NULL  /* OMPI_OP_BASE_TYPE_BYTE */

#define BYTE_NULL_3BUFF \
  NULL  /* OMPI_OP_BASE_TYPE_BYTE */

/** Fortran complex *****************************************************/
/** Fortran "2" types ***************************************************/

#if OMPI_HAVE_FORTRAN_REAL
#define TWOLOC_FORTRAN_2REAL(name) ompi_op_base_##name##_2real
#define TWOLOC_FORTRAN_2REAL_3BUFF(name) ompi_op_base_3buff_##name##_2real
#else
#define TWOLOC_FORTRAN_2REAL(name) NULL
#define TWOLOC_FORTRAN_2REAL_3BUFF(name) NULL
#endif
#if OMPI_HAVE_FORTRAN_DOUBLE_PRECISION
#define TWOLOC_FORTRAN_2DOUBLE_PRECISION(name) ompi_op_base_##name##_2double_precision
#define TWOLOC_FORTRAN_2DOUBLE_PRECISION_3BUFF(name) ompi_op_base_3buff_##name##_2double_precision
#else
#define TWOLOC_FORTRAN_2DOUBLE_PRECISION(name) NULL
#define TWOLOC_FORTRAN_2DOUBLE_PRECISION_3BUFF(name) NULL
#endif
#if OMPI_HAVE_FORTRAN_INTEGER
#define TWOLOC_FORTRAN_2INTEGER(name) ompi_op_base_##name##_2integer
#define TWOLOC_FORTRAN_2INTEGER_3BUFF(name) ompi_op_base_3buff_##name##_2integer
#else
#define TWOLOC_FORTRAN_2INTEGER(name) NULL
#define TWOLOC_FORTRAN_2INTEGER_3BUFF(name) NULL
#endif

/** All "2" types *******************************************************/

#define TWOLOC(name) \
  TWOLOC_FORTRAN_2REAL(name),                 /* OMPI_OP_BASE_TYPE_2REAL */ \
  TWOLOC_FORTRAN_2DOUBLE_PRECISION(name),     /* OMPI_OP_BASE_TYPE_2DOUBLE_PRECISION */ \
  TWOLOC_FORTRAN_2INTEGER(name),              /* OMPI_OP_BASE_TYPE_2INTEGER */ \
  ompi_op_base_##name##_float_int,         /* OMPI_OP_BASE_TYPE_FLOAT_INT */ \
  ompi_op_base_##name##_double_int,        /* OMPI_OP_BASE_TYPE_DOUBLE_INT */ \
  ompi_op_base_##name##_long_int,          /* OMPI_OP_BASE_TYPE_LONG_INT */ \
  ompi_op_base_##name##_2int,              /* OMPI_OP_BASE_TYPE_2INT */ \
  ompi_op_base_##name##_short_int,         /* OMPI_OP_BASE_TYPE_SHORT_INT */ \
  ompi_op_base_##name##_long_double_int    /* OMPI_OP_BASE_TYPE_LONG_DOUBLE_INT */

#define TWOLOC_3BUFF(name) \
  TWOLOC_FORTRAN_2REAL_3BUFF(name),                 /* OMPI_OP_BASE_TYPE_2REAL */ \
  TWOLOC_FORTRAN_2DOUBLE_PRECISION_3BUFF(name),     /* OMPI_OP_BASE_TYPE_2DOUBLE_PRECISION */ \
  TWOLOC_FORTRAN_2INTEGER_3BUFF(name),              /* OMPI_OP_BASE_TYPE_2INTEGER */ \
  ompi_op_base_3buff_##name##_float_int,         /* OMPI_OP_BASE_TYPE_FLOAT_INT */ \
  ompi_op_base_3buff_##name##_double_int,        /* OMPI_OP_BASE_TYPE_DOUBLE_INT */ \
  ompi_op_base_3buff_##name##_long_int,          /* OMPI_OP_BASE_TYPE_LONG_INT */ \
  ompi_op_base_3buff_##name##_2int,              /* OMPI_OP_BASE_TYPE_2INT */ \
  ompi_op_base_3buff_##name##_short_int,         /* OMPI_OP_BASE_TYPE_SHORT_INT */ \
  ompi_op_base_3buff_##name##_long_double_int    /* OMPI_OP_BASE_TYPE_LONG_DOUBLE_INT */

#define TWOLOC_NULL \
  NULL, /* OMPI_OP_BASE_TYPE_2REAL */\
  NULL, /* OMPI_OP_BASE_TYPE_2DOUBLE_PRECISION */ \
  NULL, /* OMPI_OP_BASE_TYPE_2INTEGER */ \
  NULL, /* OMPI_OP_BASE_TYPE_FLOAT_INT */ \
  NULL, /* OMPI_OP_BASE_TYPE_DOUBLE_INT */ \
  NULL, /* OMPI_OP_BASE_TYPE_LONG_INT */ \
  NULL, /* OMPI_OP_BASE_TYPE_2INT */ \
  NULL, /* OMPI_OP_BASE_TYPE_SHORT_INT */ \
  NULL  /* OMPI_OP_BASE_TYPE_LONG_DOUBLE_INT */

#define TWOLOC_NULL_3BUFF \
  NULL, /* OMPI_OP_BASE_TYPE_2REAL */\
  NULL, /* OMPI_OP_BASE_TYPE_2DOUBLE_PRECISION */ \
  NULL, /* OMPI_OP_BASE_TYPE_2INTEGER */ \
  NULL, /* OMPI_OP_BASE_TYPE_FLOAT_INT */ \
  NULL, /* OMPI_OP_BASE_TYPE_DOUBLE_INT */ \
  NULL, /* OMPI_OP_BASE_TYPE_LONG_INT */ \
  NULL, /* OMPI_OP_BASE_TYPE_2INT */ \
  NULL, /* OMPI_OP_BASE_TYPE_SHORT_INT */ \
  NULL  /* OMPI_OP_BASE_TYPE_LONG_DOUBLE_INT */


/*
 * MPI_OP_NULL
 * All types
 */
#define FLAGS_NO_FLOAT \
    (OMPI_OP_FLAGS_INTRINSIC | OMPI_OP_FLAGS_ASSOC | OMPI_OP_FLAGS_COMMUTE)
#define FLAGS \
    (OMPI_OP_FLAGS_INTRINSIC | OMPI_OP_FLAGS_ASSOC | \
     OMPI_OP_FLAGS_FLOAT_ASSOC | OMPI_OP_FLAGS_COMMUTE)

ompi_op_base_handler_fn_t ompi_op_base_functions[OMPI_OP_BASE_FORTRAN_OP_MAX][OMPI_OP_BASE_TYPE_MAX] = 
    {
        /* Corresponds to MPI_OP_NULL */
        {
            /* Leaving this empty puts in NULL for all entries */
            NULL,
        },
        /* Corresponds to MPI_MAX */
        {
            C_INTEGER(max),
            FORTRAN_INTEGER(max),
            FLOATING_POINT(max),
            LOGICAL_NULL,
            COMPLEX_NULL,
            BYTE_NULL,
            TWOLOC_NULL
        },
        /* Corresponds to MPI_MIN */
        {
            C_INTEGER(min),
            FORTRAN_INTEGER(min),
            FLOATING_POINT(min),
            LOGICAL_NULL,
            COMPLEX_NULL,
            BYTE_NULL,
            TWOLOC_NULL
        },
        /* Corresponds to MPI_SUM */
        {
            C_INTEGER(sum),
            FORTRAN_INTEGER(sum),
            FLOATING_POINT(sum),
            LOGICAL_NULL,
            COMPLEX(sum),
            BYTE_NULL,
            TWOLOC_NULL
        },
        /* Corresponds to MPI_PROD */
        {
            C_INTEGER(prod),
            FORTRAN_INTEGER(prod),
            FLOATING_POINT(prod),
            LOGICAL_NULL,
            COMPLEX(prod),
            BYTE_NULL,
            TWOLOC_NULL
        },
        /* Corresponds to MPI_LAND */
        {
            C_INTEGER(land),
            FORTRAN_INTEGER_NULL,
            FLOATING_POINT_NULL,
            LOGICAL(land),
            COMPLEX_NULL,
            BYTE_NULL,
            TWOLOC_NULL
        },
        /* Corresponds to MPI_BAND */
        {
            C_INTEGER(band),
            FORTRAN_INTEGER(band),
            FLOATING_POINT_NULL,
            LOGICAL_NULL,
            COMPLEX_NULL,
            BYTE(band),
            TWOLOC_NULL
        },
        /* Corresponds to MPI_LOR */
        {
            C_INTEGER(lor),
            FORTRAN_INTEGER_NULL,
            FLOATING_POINT_NULL,
            LOGICAL(lor),
            COMPLEX_NULL,
            BYTE_NULL,
            TWOLOC_NULL
        },
        /* Corresponds to MPI_BOR */
        {
            C_INTEGER(bor),
            FORTRAN_INTEGER(bor),
            FLOATING_POINT_NULL,
            LOGICAL_NULL,
            COMPLEX_NULL,
            BYTE(bor),
            TWOLOC_NULL
        },
        /* Corresponds to MPI_LXOR */
        {
            C_INTEGER(lxor),
            FORTRAN_INTEGER_NULL,
            FLOATING_POINT_NULL,
            LOGICAL(lxor),
            COMPLEX_NULL,
            BYTE_NULL,
            TWOLOC_NULL
        },
        /* Corresponds to MPI_BXOR */
        {
            C_INTEGER(bxor),
            FORTRAN_INTEGER(bxor),
            FLOATING_POINT_NULL,
            LOGICAL_NULL,
            COMPLEX_NULL,
            BYTE(bxor),
            TWOLOC_NULL
        },
        /* Corresponds to MPI_MAXLOC */
        {
            C_INTEGER_NULL,
            FORTRAN_INTEGER_NULL,
            FLOATING_POINT_NULL,
            LOGICAL_NULL,
            COMPLEX_NULL,
            BYTE_NULL,
            TWOLOC(maxloc),
        },
        /* Corresponds to MPI_MINLOC */
        {
            C_INTEGER_NULL,
            FORTRAN_INTEGER_NULL,
            FLOATING_POINT_NULL,
            LOGICAL_NULL,
            COMPLEX_NULL,
            BYTE_NULL,
            TWOLOC(minloc),
        },
        /* Corresponds to MPI_REPLACE */
        {
            /* (MPI_ACCUMULATE is handled differently than the other
               reductions, so just zero out its function
               impementations here to ensure that users don't invoke
               MPI_REPLACE with any reduction operations other than
               ACCUMULATE) */
            NULL,
        },

    };


ompi_op_base_3buff_handler_fn_t ompi_op_base_3buff_functions[OMPI_OP_BASE_FORTRAN_OP_MAX][OMPI_OP_BASE_TYPE_MAX] = 
    {
        /* Corresponds to MPI_OP_NULL */
        {
            /* Leaving this empty puts in NULL for all entries */
            NULL,
        },
        /* Corresponds to MPI_MAX */
        {
            C_INTEGER_3BUFF(max),
            FORTRAN_INTEGER_3BUFF(max),
            FLOATING_POINT_3BUFF(max),
            LOGICAL_NULL_3BUFF,
            COMPLEX_NULL_3BUFF,
            BYTE_NULL_3BUFF,
            TWOLOC_NULL_3BUFF
        },
        /* Corresponds to MPI_MIN */
        {
            C_INTEGER_3BUFF(min),
            FORTRAN_INTEGER_3BUFF(min),
            FLOATING_POINT_3BUFF(min),
            LOGICAL_NULL_3BUFF,
            COMPLEX_NULL_3BUFF,
            BYTE_NULL_3BUFF,
            TWOLOC_NULL_3BUFF
        },
        /* Corresponds to MPI_SUM */
        {
            C_INTEGER_3BUFF(sum),
            FORTRAN_INTEGER_3BUFF(sum),
            FLOATING_POINT_3BUFF(sum),
            LOGICAL_NULL_3BUFF,
            COMPLEX_3BUFF(sum),
            BYTE_NULL_3BUFF,
            TWOLOC_NULL_3BUFF
        },
        /* Corresponds to MPI_PROD */
        {
            C_INTEGER_3BUFF(prod),
            FORTRAN_INTEGER_3BUFF(prod),
            FLOATING_POINT_3BUFF(prod),
            LOGICAL_NULL_3BUFF,
            COMPLEX_3BUFF(prod),
            BYTE_NULL_3BUFF,
            TWOLOC_NULL_3BUFF
        },
        /* Corresponds to MPI_LAND */
        {
            C_INTEGER_3BUFF(land),
            FORTRAN_INTEGER_NULL_3BUFF,
            FLOATING_POINT_NULL_3BUFF,
            LOGICAL_3BUFF(land),
            COMPLEX_NULL_3BUFF,
            BYTE_NULL_3BUFF,
            TWOLOC_NULL_3BUFF
        },
        /* Corresponds to MPI_BAND */
        {
            C_INTEGER_3BUFF(band),
            FORTRAN_INTEGER_3BUFF(band),
            FLOATING_POINT_NULL_3BUFF,
            LOGICAL_NULL_3BUFF,
            COMPLEX_NULL_3BUFF,
            BYTE_3BUFF(band),
            TWOLOC_NULL_3BUFF
        },
        /* Corresponds to MPI_LOR */
        {
            C_INTEGER_3BUFF(lor),
            FORTRAN_INTEGER_NULL_3BUFF,
            FLOATING_POINT_NULL_3BUFF,
            LOGICAL_3BUFF(lor),
            COMPLEX_NULL_3BUFF,
            BYTE_NULL_3BUFF,
            TWOLOC_NULL_3BUFF
        },
        /* Corresponds to MPI_BOR */
        {
            C_INTEGER_3BUFF(bor),
            FORTRAN_INTEGER_3BUFF(bor),
            FLOATING_POINT_NULL_3BUFF,
            LOGICAL_NULL_3BUFF,
            COMPLEX_NULL_3BUFF,
            BYTE_3BUFF(bor),
            TWOLOC_NULL_3BUFF
        },
        /* Corresponds to MPI_LXOR */
        {
            C_INTEGER_3BUFF(lxor),
            FORTRAN_INTEGER_NULL_3BUFF,
            FLOATING_POINT_NULL_3BUFF,
            LOGICAL_3BUFF(lxor),
            COMPLEX_NULL_3BUFF,
            BYTE_NULL_3BUFF,
            TWOLOC_NULL_3BUFF
        },
        /* Corresponds to MPI_BXOR */
        {
            C_INTEGER_3BUFF(bxor),
            FORTRAN_INTEGER_3BUFF(bxor),
            FLOATING_POINT_NULL_3BUFF,
            LOGICAL_NULL_3BUFF,
            COMPLEX_NULL_3BUFF,
            BYTE_3BUFF(bxor),
            TWOLOC_NULL_3BUFF
        },
        /* Corresponds to MPI_MAXLOC */
        {
            C_INTEGER_NULL_3BUFF,
            FORTRAN_INTEGER_NULL_3BUFF,
            FLOATING_POINT_NULL_3BUFF,
            LOGICAL_NULL_3BUFF,
            COMPLEX_NULL_3BUFF,
            BYTE_NULL_3BUFF,
            TWOLOC_3BUFF(maxloc),
        },
        /* Corresponds to MPI_MINLOC */
        {
            C_INTEGER_NULL_3BUFF,
            FORTRAN_INTEGER_NULL_3BUFF,
            FLOATING_POINT_NULL_3BUFF,
            LOGICAL_NULL_3BUFF,
            COMPLEX_NULL_3BUFF,
            BYTE_NULL_3BUFF,
            TWOLOC_3BUFF(minloc),
        },
        /* Corresponds to MPI_REPLACE */
        {
            /* MPI_ACCUMULATE is handled differently than the other
               reductions, so just zero out its function
               impementations here to ensure that users don't invoke
               MPI_REPLACE with any reduction operations other than
               ACCUMULATE */
            NULL,
        },
    };

