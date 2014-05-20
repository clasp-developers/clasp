/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_INTTYPES_H
#define _VT_INTTYPES_H

#ifdef HAVE_CONFIG_H
# include "config.h"
#else /* HAVE_CONFIG_H */
# define HAVE_STDINT_H   1
# define HAVE_INTTYPES_H 1
# define SIZEOF_LONG     8
#endif /* HAVE_CONFIG_H */

#if defined(HAVE_STDINT_H) && HAVE_STDINT_H && !defined(__sgi)
# include <stdint.h>
#elif defined(HAVE_INTTYPES_H) && HAVE_INTTYPES_H
# include <inttypes.h>
#else /* HAVE_INTTYPES_H || HAVE_STDINT_H */

  /* Signed. */
  typedef signed char               int8_t;
  typedef signed short int          int16_t;
  typedef signed int                int32_t;

#if SIZEOF_LONG == 8
    typedef signed long int         int64_t;
#else /* SIZEOF_LONG */
    typedef signed long long int    int64_t;
#endif /* SIZEOF_LONG */

/* Unsigned. */
  typedef unsigned char             uint8_t;
  typedef unsigned short int        uint16_t;
  typedef unsigned int              uint32_t;

#if SIZEOF_LONG == 8
    typedef unsigned long int       uint64_t;
#else /* SIZEOF_LONG */
    typedef unsigned long long int  uint64_t;
#endif /* SIZEOF_LONG */

#endif /* HAVE_INTTYPES_H || HAVE_STDINT_H */

#endif /* _VT_INTTYPES_H */
