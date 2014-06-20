/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2004-2005, The Trustees of Indiana University and Indiana
 *                          University Research and Technology
 *
 * Copyright (c) 2004-2006, The University of Tennessee and The University
 *                          of Tennessee Research Foundation
 *
 * Copyright (c) 2004-2005, High Performance Computing Center Stuttgart,
 *                          University of Stuttgart
 *
 * Copyright (c) 2004-2005, The Regents of the University of California
 *
 * Copyright (c) 2007,      Cisco Systems, Inc.
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "util.h"

static int guess_strlen(const char* fmt, va_list ap)
{
  char* sarg;
  double darg;
  float farg;
  size_t i;
  int iarg;
  int len;
  long larg;

  /* Start off with a fudge factor of 128 to handle the % escapes that
     we aren't calculating here */

  len = (int)strlen(fmt) + 128;
  for (i = 0; i < strlen(fmt); ++i) {
    if ('%' == fmt[i] && i + 1 < strlen(fmt) && '%' != fmt[i + 1]) {
      ++i;
      switch (fmt[i]) {
        case 'c':
	  (void)va_arg(ap, int);
	  len += 1;  /* let's suppose it's a printable char */
	  break;
        case 's':
	  sarg = va_arg(ap, char*);

	  /* If there's an arg, get the strlen, otherwise we'll
	   * use (null) */

	  if (NULL != sarg) {
	    len += (int)strlen(sarg);
	  } else {
	    len += 5;
	  }
	  break;

        case 'd':
        case 'i':
	  iarg = va_arg(ap, int);
	  /* Alloc for minus sign */
	  if (iarg < 0)
	    ++len;
	  /* Now get the log10 */
	  do {
	    ++len;
	    iarg /= 10;
	  } while (0 != iarg);
	  break;

        case 'x':
        case 'X':
	  iarg = va_arg(ap, int);
	  /* Now get the log16 */
	  do {
	    ++len;
	    iarg /= 16;
	  } while (0 != iarg);
	  break;

        case 'f':
	  farg = (float)va_arg(ap, int);
	  /* Alloc for minus sign */
	  if (farg < 0) {
	    ++len;
	    farg = -farg;
	  }
	  /* Alloc for 3 decimal places + '.' */
	  len += 4;
	  /* Now get the log10 */
	  do {
	    ++len;
	    farg /= 10.0;
	  } while (0.0 != farg);
	  break;

        case 'g':
	  darg = va_arg(ap, int);
	  /* Alloc for minus sign */
	  if (darg < 0) {
	    ++len;
	    darg = -darg;
	  }
	  /* Alloc for 3 decimal places + '.' */
	  len += 4;
	  /* Now get the log10 */
	  do {
	    ++len;
	    darg /= 10.0;
	  } while (0.0 != darg);
	  break;

        case 'l':
	  /* Get %ld %lx %lX %lf */
	   if (i + 1 < strlen(fmt)) {
	      ++i;
	      switch (fmt[i]) {
	        case 'x':
	        case 'X':
		  larg = va_arg(ap, int);
		  /* Now get the log16 */
		  do {
		    ++len;
		    larg /= 16;
		  } while (0 != larg);
		  break;

	        case 'f':
		  darg = va_arg(ap, int);
		  /* Alloc for minus sign */
		  if (darg < 0) {
		    ++len;
		    darg = -darg;
		  }
		  /* Alloc for 3 decimal places + '.' */
		  len += 4;
		  /* Now get the log10 */
		  do {
		    ++len;
		    darg /= 10.0;
		  } while (0.0 != darg);
		  break;

	        case 'd':
	        default:
		  larg = va_arg(ap, int);
		  /* Now get the log10 */
		  do {
		    ++len;
		    larg /= 10;
		  } while (0 != larg);
		  break;
	      }
	   }

        default:
	  break;
      }
    }
  }

  return len;
}


void vt_assert_fail(const char* expr, const char* file, int line)
{
  fprintf(stderr, "%s:%d: Assertion `%s' failed.", file, line, expr);
  abort();
}

int vt_asprintf(char** ptr, const char* fmt, ...)
{
  int length;
  va_list ap;

  va_start(ap, fmt);
  length = vt_vasprintf(ptr, fmt, ap);
  va_end(ap);

  return length;
}

int vt_vasprintf(char** ptr, const char* fmt, va_list ap)
{
  int length;
  va_list ap2;

  /* va_list might have pointer to internal state and using
     it twice is a bad idea.  So make a copy for the second
     use.  Copy order taken from Autoconf docs. */
#if defined(va_copy)
  va_copy(ap2, ap);
#elif defined(__va_copy)
  __va_copy(ap2, ap);
#else
  memcpy (&ap2, &ap, sizeof(va_list));
#endif

  /* guess the size */
  length = guess_strlen(fmt, ap);

  /* allocate a buffer */
  *ptr = (char*) malloc((size_t) length + 1);
  if (NULL == *ptr) {
    errno = ENOMEM;
    va_end(ap2);
    return -1;
  }

  /* fill the buffer */
  length = vsprintf(*ptr, fmt, ap2);
#if defined(va_copy) || defined(__va_copy)
  va_end(ap2);
#endif /* va_copy || __va_copy */

  /* realloc */
  *ptr = (char*)realloc(*ptr, (size_t)length + 1);
  if (NULL == *ptr) {
    errno = ENOMEM;
    return -1;
  }

  return length;
}

int vt_snprintf(char* str, size_t size, const char* fmt, ...)
{
  int length;
  va_list ap;

  va_start(ap, fmt);
  length = vt_vsnprintf(str, size, fmt, ap);
  va_end(ap);

  return length;
}

int vt_vsnprintf(char* str, size_t size, const char* fmt, va_list ap)
{
  int length;
  char* buf;

  length = vt_vasprintf(&buf, fmt, ap);
  if (length < 0) {
    return length;
  }

  /* return the length when given a null buffer (C99) */
  if (str) {
    if ((size_t) length < size) {
      strcpy(str, buf);
    } else {
      memcpy(str, buf, size - 1);
      str[size] = '\0';
    }
  }

  /* free allocated buffer */
  free(buf);

  return length;
}

char* vt_strdup(const char* s)
{
  char* c;

  if (s == NULL || (c = (char*)malloc(strlen(s)+1)) == NULL)
  {
    errno = ENOMEM;
    return NULL;
  }

  strcpy(c, s) ;

  return c;
}

char* vt_strtrim(char* s)
{
  int trim_start_idx = 0;
  int trim_stop_idx = strlen(s);
  int i, j;

  if (trim_stop_idx > 0)
  {
    for ( i = 0; i < trim_stop_idx && s[i] == ' '; i++ ) trim_start_idx++;
    for ( i = trim_stop_idx - 1; i >= 0 && s[i] == ' '; i-- ) trim_stop_idx--;
    for ( j = 0, i = trim_start_idx; i < trim_stop_idx; i++, j++ ) s[j] = s[i];
    s[j] = '\0';
  }

  return s;
}

void* vt_memmove(void* dest, const void* src, size_t n)
{
  char *d = (char*)dest;
  char *s = (char*)src;
  
  if(src>dest)
  {
    while(n-- != 0) *(d++) = *(s++);
  }
  else if(src<dest)
  {
    d += n;
    s += n;
    while(n-- != 0) *(--d) = *(--s);
  }
  
  return dest;
}
