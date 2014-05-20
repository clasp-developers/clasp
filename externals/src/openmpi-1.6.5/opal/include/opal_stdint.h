/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
 *
 * This file includes the C99 stdint.h file if available, and otherwise
 * defines fixed-width types according to the SIZEOF information
 * gathered by configure.
 */

#ifndef OPAL_STDINT_H
#define OPAL_STDINT_H 1

/*
 * Include what we can and define what is missing.
 */
#include <limits.h>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

/* 8-bit */

#if SIZEOF_CHAR == 1

#ifndef HAVE_INT8_T
typedef signed char int8_t;
#endif

#ifndef HAVE_UINT8_T
typedef unsigned char uint8_t;
#endif

#else

#error Failed to define 8-bit types

#endif

/* 16-bit */

#if SIZEOF_SHORT == 2

#ifndef HAVE_INT16_T
typedef signed short int16_t;
#endif

#ifndef HAVE_UINT16_T
typedef unsigned short uint16_t;
#endif

#else

#error Failed to define 16-bit types

#endif

/* 32-bit */

#if SIZEOF_INT == 4

#ifndef HAVE_INT32_T
typedef signed int int32_t;
#endif

#ifndef HAVE_UINT32_T
typedef unsigned int uint32_t;
#endif

#elif SIZEOF_LONG == 4

#ifndef HAVE_INT32_T
typedef signed long int32_t;
#endif

#ifndef HAVE_UINT32_T
typedef unsigned long uint32_t;
#endif

#else

#error Failed to define 32-bit types

#endif

/* 64-bit */

#if SIZEOF_INT == 8

#ifndef HAVE_INT64_T
typedef signed int int64_t;
#endif

#ifndef HAVE_UINT64_T
typedef unsigned int uint64_t;
#endif

#elif SIZEOF_LONG == 8

#ifndef HAVE_INT64_T
typedef signed long int64_t;
#endif

#ifndef HAVE_UINT64_T
typedef unsigned long uint64_t;
#endif

#elif HAVE_LONG_LONG && SIZEOF_LONG_LONG == 8

#ifndef HAVE_INT64_T
typedef signed long long int64_t;
#endif

#ifndef HAVE_UINT64_T
typedef unsigned long long uint64_t;
#endif

#else

#error Failed to define 64-bit types

#endif

/* Pointers */

#if SIZEOF_VOID_P == SIZEOF_INT

#ifndef HAVE_INTPTR_T
typedef signed int intptr_t;
#endif

#ifndef HAVE_UINTPTR_T
typedef unsigned int uintptr_t;
#endif

#elif SIZEOF_VOID_P == SIZEOF_LONG

#ifndef HAVE_INTPTR_T
typedef signed long intptr_t;
#endif

#ifndef HAVE_UINTPTR_T
typedef unsigned long uintptr_t;
#endif

#elif HAVE_LONG_LONG && SIZEOF_VOID_P == SIZEOF_LONG_LONG

#ifndef HAVE_INTPTR_T
typedef signed long long intptr_t;
#endif
#ifndef HAVE_UINTPTR_T
typedef unsigned long long uintptr_t;
#endif

#else

#error Failed to define pointer-sized integer types

#endif

/* fix up some constants that may be missing */
#ifndef SIZE_MAX
# if SIZEOF_VOID_P == SIZEOF_INT
#   define SIZE_MAX UINT_MAX
# elif SIZEOF_VOID_P == SIZEOF_LONG
#   define SIZE_MAX ULONG_MAX
# else
#   error Failed to find value for SIZE_MAX
# endif
#endif /* ifndef SIZE_MAX */


/* inttypes.h printf specifiers */
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>
#else 

# if SIZEOF_LONG == 8
#  define __PRI64_PREFIX	"l"
#  define __PRIPTR_PREFIX	"l"
# else
#  define __PRI64_PREFIX	"ll"
#  define __PRIPTR_PREFIX
# endif

/* Decimal notation.  */
# define PRId8		"d"
# define PRId16		"d"
# define PRId32		"d"
# define PRId64		__PRI64_PREFIX "d"

# define PRIdLEAST8	"d"
# define PRIdLEAST16	"d"
# define PRIdLEAST32	"d"
# define PRIdLEAST64	__PRI64_PREFIX "d"

# define PRIdFAST8	"d"
# define PRIdFAST16	__PRIPTR_PREFIX "d"
# define PRIdFAST32	__PRIPTR_PREFIX "d"
# define PRIdFAST64	__PRI64_PREFIX "d"

# define PRIi8		"i"
# define PRIi16		"i"
# define PRIi32		"i"
# define PRIi64		__PRI64_PREFIX "i"

# define PRIiLEAST8	"i"
# define PRIiLEAST16	"i"
# define PRIiLEAST32	"i"
# define PRIiLEAST64	__PRI64_PREFIX "i"

# define PRIiFAST8	"i"
# define PRIiFAST16	__PRIPTR_PREFIX "i"
# define PRIiFAST32	__PRIPTR_PREFIX "i"
# define PRIiFAST64	__PRI64_PREFIX "i"

/* Octal notation.  */
# define PRIo8		"o"
# define PRIo16		"o"
# define PRIo32		"o"
# define PRIo64		__PRI64_PREFIX "o"

# define PRIoLEAST8	"o"
# define PRIoLEAST16	"o"
# define PRIoLEAST32	"o"
# define PRIoLEAST64	__PRI64_PREFIX "o"

# define PRIoFAST8	"o"
# define PRIoFAST16	__PRIPTR_PREFIX "o"
# define PRIoFAST32	__PRIPTR_PREFIX "o"
# define PRIoFAST64	__PRI64_PREFIX "o"

/* Unsigned integers.  */
# define PRIu8		"u"
# define PRIu16		"u"
# define PRIu32		"u"
# define PRIu64		__PRI64_PREFIX "u"

# define PRIuLEAST8	"u"
# define PRIuLEAST16	"u"
# define PRIuLEAST32	"u"
# define PRIuLEAST64	__PRI64_PREFIX "u"

# define PRIuFAST8	"u"
# define PRIuFAST16	__PRIPTR_PREFIX "u"
# define PRIuFAST32	__PRIPTR_PREFIX "u"
# define PRIuFAST64	__PRI64_PREFIX "u"

/* lowercase hexadecimal notation.  */
# define PRIx8		"x"
# define PRIx16		"x"
# define PRIx32		"x"
# define PRIx64		__PRI64_PREFIX "x"

# define PRIxLEAST8	"x"
# define PRIxLEAST16	"x"
# define PRIxLEAST32	"x"
# define PRIxLEAST64	__PRI64_PREFIX "x"

# define PRIxFAST8	"x"
# define PRIxFAST16	__PRIPTR_PREFIX "x"
# define PRIxFAST32	__PRIPTR_PREFIX "x"
# define PRIxFAST64	__PRI64_PREFIX "x"

/* UPPERCASE hexadecimal notation.  */
# define PRIX8		"X"
# define PRIX16		"X"
# define PRIX32		"X"
# define PRIX64		__PRI64_PREFIX "X"

# define PRIXLEAST8	"X"
# define PRIXLEAST16	"X"
# define PRIXLEAST32	"X"
# define PRIXLEAST64	__PRI64_PREFIX "X"

# define PRIXFAST8	"X"
# define PRIXFAST16	__PRIPTR_PREFIX "X"
# define PRIXFAST32	__PRIPTR_PREFIX "X"
# define PRIXFAST64	__PRI64_PREFIX "X"

/* Macros for printing `intmax_t' and `uintmax_t'.  */
# define PRIdMAX	__PRI64_PREFIX "d"
# define PRIiMAX	__PRI64_PREFIX "i"
# define PRIoMAX	__PRI64_PREFIX "o"
# define PRIuMAX	__PRI64_PREFIX "u"
# define PRIxMAX	__PRI64_PREFIX "x"
# define PRIXMAX	__PRI64_PREFIX "X"

/* Macros for printing `intptr_t' and `uintptr_t'.  */
# define PRIdPTR	__PRIPTR_PREFIX "d"
# define PRIiPTR	__PRIPTR_PREFIX "i"
# define PRIoPTR	__PRIPTR_PREFIX "o"
# define PRIuPTR	__PRIPTR_PREFIX "u"
# define PRIxPTR	__PRIPTR_PREFIX "x"
# define PRIXPTR	__PRIPTR_PREFIX "X" 

#endif

#ifndef PRIsize_t
# if defined(ACCEPT_C99)
#   define PRIsize_t "zu"
# elif SIZEOF_SIZE_T == SIZEOF_LONG
#   define PRIsize_t "lu"
# elif SIZEOF_SIZE_T == SIZEOF_LONG_LONG
#   define PRIsize_t "llu"
# else
#   define PRIsize_t "u"
# endif
#endif

#endif /* OPAL_STDINT_H */

