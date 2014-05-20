/* gmp_obstack_printf -- formatted output to an obstack.

Copyright 2001, 2002 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
MA 02110-1301, USA. */

#include "config.h"

#if HAVE_OBSTACK_VPRINTF

#if HAVE_STDARG
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include <obstack.h>
#include <string.h>

#include "gmp.h"
#include "gmp-impl.h"


int
#if HAVE_STDARG
gmp_obstack_printf (struct obstack *ob, const char *fmt, ...)
#else
gmp_obstack_printf (va_alist)
     va_dcl
#endif
{
  va_list  ap;
  int      ret;

#if HAVE_STDARG
  va_start (ap, fmt);
#else
  struct obstack *ob;
  const char     *fmt;
  va_start (ap);
  ob = va_arg (ap, struct obstack *);
  fmt = va_arg (ap, const char *);
#endif

  ASSERT (! MEM_OVERLAP_P (obstack_base(ob), obstack_object_size(ob),
                           fmt, strlen(fmt)+1));

  ret = __gmp_doprnt (&__gmp_obstack_printf_funs, ob, fmt, ap);
  va_end (ap);
  return ret;
}

#endif /* HAVE_OBSTACK_VPRINTF */
