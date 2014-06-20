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

/** @file
 *
 * Cycle counter reading instructions.  Do not use directly - see the
 * timer interface instead
 */

#ifndef OPAL_SYS_TIMER_H
#define OPAL_SYS_TIMER_H 1

#include "opal_config.h"

#include "opal/sys/architecture.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

/* do some quick #define cleanup in cases where we are doing
   testing... */
#ifdef OMPI_DISABLE_INLINE_ASM
#undef OPAL_C_GCC_INLINE_ASSEMBLY
#define OPAL_C_GCC_INLINE_ASSEMBLY 0
#undef OMPI_CXX_GCC_INLINE_ASSEMBLY
#define OMPI_CXX_GCC_INLINE_ASSEMBLY 0
#undef OPAL_C_DEC_INLINE_ASSEMBLY
#define OPAL_C_DEC_INLINE_ASSEMBLY 0
#undef OMPI_CXX_DEC_INLINE_ASSEMBLY
#define OMPI_CXX_DEC_INLINE_ASSEMBLY 0
#undef OPAL_C_XLC_INLINE_ASSEMBLY
#define OPAL_C_XLC_INLINE_ASSEMBLY 0
#undef OMPI_CXX_XLC_INLINE_ASSEMBLY
#define OMPI_CXX_XLC_INLINE_ASSEMBLY 0
#endif

/* define OMPI_{GCC,DEC,XLC}_INLINE_ASSEMBLY based on the
   OMPI_{C,CXX}_{GCC,DEC,XLC}_INLINE_ASSEMBLY defines and whether we
   are in C or C++ */
#if defined(c_plusplus) || defined(__cplusplus)
#define OMPI_GCC_INLINE_ASSEMBLY OMPI_CXX_GCC_INLINE_ASSEMBLY
#define OMPI_DEC_INLINE_ASSEMBLY OMPI_CXX_DEC_INLINE_ASSEMBLY
#define OMPI_XLC_INLINE_ASSEMBLY OMPI_CXX_XLC_INLINE_ASSEMBLY
#else
#define OMPI_GCC_INLINE_ASSEMBLY OPAL_C_GCC_INLINE_ASSEMBLY
#define OMPI_DEC_INLINE_ASSEMBLY OPAL_C_DEC_INLINE_ASSEMBLY
#define OMPI_XLC_INLINE_ASSEMBLY OPAL_C_XLC_INLINE_ASSEMBLY
#endif

/**********************************************************************
 *
 * Load the appropriate architecture files and set some reasonable
 * default values for our support
 *
 *********************************************************************/

BEGIN_C_DECLS

/* If you update this list, you probably also want to update
   opal/mca/timer/linux/configure.m4.  Or not. */

#if defined(DOXYGEN)
/* don't include system-level gorp when generating doxygen files */ 
#elif OPAL_ASSEMBLY_ARCH == OMPI_AMD64
#include "opal/sys/amd64/timer.h"
#elif OPAL_ASSEMBLY_ARCH == OMPI_ARM
#include "opal/sys/arm/timer.h"
#elif OPAL_ASSEMBLY_ARCH == OMPI_IA32
#include "opal/sys/ia32/timer.h"
#elif OPAL_ASSEMBLY_ARCH == OMPI_IA64
#include "opal/sys/ia64/timer.h"
#elif OPAL_ASSEMBLY_ARCH == OMPI_POWERPC32
#include "opal/sys/powerpc/timer.h"
#elif OPAL_ASSEMBLY_ARCH == OMPI_POWERPC64
#include "opal/sys/powerpc/timer.h"
#elif OPAL_ASSEMBLY_ARCH == OMPI_SPARCV9_32
#include "opal/sys/sparcv9/timer.h"
#elif OPAL_ASSEMBLY_ARCH == OMPI_SPARCV9_64
#include "opal/sys/sparcv9/timer.h"
#elif OPAL_ASSEMBLY_ARCH == OMPI_WINDOWS
#include "opal/sys/win32/timer.h"
#elif OPAL_ASSEMBLY_ARCH == OMPI_MIPS
#include "opal/sys/mips/timer.h"
#endif

#ifndef DOXYGEN
#ifndef OPAL_HAVE_SYS_TIMER_GET_CYCLES
#define OPAL_HAVE_SYS_TIMER_GET_CYCLES 0

typedef int opal_timer_t;
#endif
#endif

END_C_DECLS

#endif /* OPAL_SYS_TIMER_H */
