/*============================================================================
  KWSys - Kitware System Library
  Copyright 2000-2009 Kitware, Inc., Insight Software Consortium

  Distributed under the OSI-approved BSD License (the "License");
  see accompanying file Copyright.txt for details.

  This software is distributed WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the License for more information.
============================================================================*/
#ifndef cmsys_CPU_h
#define cmsys_CPU_h

#include <cmsys/Configure.h>

/* Identify possible endian cases.  The macro
   cmsys_CPU_ENDIAN_ID will be defined to one of these, or
   0 if unknown.  */
#define cmsys_CPU_ENDIAN_ID_BIG    4321
#define cmsys_CPU_ENDIAN_ID_LITTLE 1234

/* Apple always defines one of these.  */
#if defined(__LITTLE_ENDIAN__)
# define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_LITTLE
#elif defined(__BIG_ENDIAN__)
# define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_BIG

/* Alpha */
#elif defined(__alpha) || defined(__alpha__) || defined(_M_ALPHA)
# define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_LITTLE

/* Arm */
#elif defined(__arm__)
# if !defined(__ARMEB__)
#  define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_LITTLE
# else
#  define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_BIG
# endif

/* Intel x86 */
#elif defined(__i386) || defined(__i386__) || defined(_M_IX86)
# define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_LITTLE
#elif defined(_X86_) || defined(__THW_INTEL__) || defined(__I86__)
# define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_LITTLE
#elif defined(__MWERKS__) && defined(__INTEL__)
# define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_LITTLE

/* Intel x86-64 */
#elif defined(__x86_64) || defined(__x86_64__) || defined(_M_X64)
# define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_LITTLE
#elif defined(__amd64) || defined(__amd64__)
# define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_LITTLE

/* Intel Architecture-64 (Itanium) */
#elif defined(__ia64) || defined(__ia64__)
# define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_LITTLE
#elif defined(_IA64) || defined(__IA64__) || defined(_M_IA64)
# define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_LITTLE

/* PowerPC */
#elif defined(__powerpc) || defined(__powerpc__)
# define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_BIG
#elif defined(__ppc) || defined(__ppc__) || defined(__POWERPC__)
# define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_BIG

/* SPARC */
#elif defined(__sparc) || defined(__sparc__)
# define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_BIG

/* HP/PA RISC */
#elif defined(__hppa) || defined(__hppa__)
# define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_BIG

/* Motorola 68k */
#elif defined(__m68k__) || defined(M68000)
# define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_BIG

/* MIPS */
#elif defined(__mips) || defined(__mips__) || defined(__MIPS__)
# define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_BIG

/* RS/6000 */
#elif defined(__THW_RS600) || defined(_IBMR2) || defined(_POWER)
# define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_BIG
#elif defined(_ARCH_PWR) || defined(_ARCH_PWR2)
# define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_BIG

/* System/370 */
#elif defined(__370__) || defined(__THW_370__)
# define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_BIG

/* System/390 */
#elif defined(__s390__) || defined(__s390x__)
# define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_BIG

/* z/Architecture */
#elif defined(__SYSC_ZARCH__)
# define cmsys_CPU_ENDIAN_ID cmsys_CPU_ENDIAN_ID_BIG

/* Unknown CPU */
#else
# define cmsys_CPU_ENDIAN_ID 0
# if !defined(cmsys_CPU_UNKNOWN_OKAY)
#  error "The target CPU architecture is not known."
# endif
#endif

/* If building a C or C++ file in kwsys itself, give the source file
   access to the macros without a configured namespace.  */
#if defined(KWSYS_NAMESPACE)
# define KWSYS_CPU_ENDIAN_ID        cmsys_CPU_ENDIAN_ID
# define KWSYS_CPU_ENDIAN_ID_BIG    cmsys_CPU_ENDIAN_ID_BIG
# define KWSYS_CPU_ENDIAN_ID_LITTLE cmsys_CPU_ENDIAN_ID_LITTLE
#endif

#endif
