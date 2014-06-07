/*
 * Compatibility functions for those systems who are missing them.
 *
 * Copyright 2000 by Gray Watson
 *
 * This file is part of the dmalloc package.
 *
 * Permission to use, copy, modify, and distribute this software for
 * any purpose and without fee is hereby granted, provided that the
 * above copyright notice and this permission notice appear in all
 * copies, and that the name of Gray Watson not be used in advertising
 * or publicity pertaining to distribution of the document or software
 * without specific, written prior permission.
 *
 * Gray Watson makes no representations about the suitability of the
 * software described herein for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * The author may be contacted via http://dmalloc.com/
 *
 * $Id: compat.c,v 1.57 2007/05/14 15:53:11 gray Exp $
 */

/*
 * This file holds the compatibility routines necessary for the library to
 * function just in case your system does not have them.
 */

#include <stdio.h>				/* for vsprintf */

#if HAVE_STDARG_H
# include <stdarg.h>				/* for ... */
#endif

#define DMALLOC_DISABLE

#include "conf.h"
#include "dmalloc.h"

#include "compat.h"
#include "dmalloc_loc.h"

#if HAVE_ATOI == 0
/*
 * Turn a ascii-string into an integer which is returned
 */
int	atoi(const char *str)
{
  const char	*str_p;
  int		sign = 1, result = 0;
  
  /* skip opening white space */
  for (str_p = str; *str_p == ' ' || *str_p == '\t'; str_p++) {
  }
  
  /* handle sign */
  if (*str_p == '-') {
    sign = -1;
    str_p++;
  }
  else if (*str_p == '+') {
    /* sign is already 1 */
    str_p++;
  }
  
  /* skip more white space */
  for (; *str_p == ' ' || *str_p == '\t'; str_p++) {
  }
  
  /* now add up all digits */
  for (; *str_p >= '0' && *str_p <= '9'; str_p++) {
    result = result * 10 + *str_p - '0';
  }
  
  return result * sign;
}
#endif /* HAVE_ATOI == 0 */

#if HAVE_ATOL == 0
/*
 * Turn a ascii-string into an integer which is returned
 */
long	atol(const char *str)
{
  const char	*str_p;
  long		sign = 1, result = 0;
  
  /* skip opening white space */
  for (str_p = str; *str_p == ' ' || *str_p == '\t'; str_p++) {
  }
  
  /* handle sign */
  if (*str_p == '-') {
    sign = -1;
    str_p++;
  }
  else if (*str_p == '+') {
    /* sign is already 1 */
    str_p++;
  }
  
  /* skip more white space */
  for (; *str_p == ' ' || *str_p == '\t'; str_p++) {
  }
  
  /* now add up all digits */
  for (; *str_p >= '0' && *str_p <= '9'; str_p++) {
    result = result * (long)10 + (long)(*str_p - '0');
  }
  
  return result * sign;
}
#endif /* HAVE_ATOL == 0 */

/*
 * Local ascii to unsigned long function
 */
unsigned long	loc_atoul(const char *str)
{
  const char	*str_p;
  unsigned long	result = 0;
  
  /* skip opening white space */
  for (str_p = str; *str_p == ' ' || *str_p == '\t'; str_p++) {
  }
  
  /* now add up all digits */
  for (; *str_p >= '0' && *str_p <= '9'; str_p++) {
    result = result * (unsigned long)10 + (unsigned long)(*str_p - '0');
  }
  
  return result;
}

/*
 * Local vsnprintf which handles the buffer-size or not.  Returns the
 * number of characters copied into BUF.
 */
int	loc_vsnprintf(char *buf, const int buf_size, const char *format,
		      va_list args)
{
  char	*buf_p;
  
#if HAVE_VSNPRINTF
  (void)vsnprintf(buf, buf_size, format, args);
#else
#if HAVE_VPRINTF
  (void)vsprintf(buf, format, args);
#else
  /* Oh well.  Just do a strcpy of the format */
  (void)strncpy(buf, format, buf_size - 1);
  buf[buf_size - 1] = '\0';
#endif
#endif
  
  /* now find the end of the buffer */
  for (buf_p = buf; *buf_p != '\0'; buf_p++) {
  }
  
  return buf_p - buf;
}

/*
 * Local snprintf which handles the buf-size not.  Returns the number
 * of characters copied into BUF.
 */
int	loc_snprintf(char *buf, const int buf_size, const char *format, ...)
{
  va_list	args;  
  int		len;
  
  va_start(args, format);
  len = loc_vsnprintf(buf, buf_size, format, args);
  va_end(args);
  
  return len;
}

#if HAVE_MEMCMP == 0
/*
 * Compare LEN characters, return -1,0,1 if STR1 is <,==,> STR2
 */
int	memcmp(const void *str1, const void *str2, DMALLOC_SIZE len)
{
  const unsigned char	*str1_p, *str2_p;
  
  for (str1_p = str1, str2_p = str2; len > 0; len--, str1_p++, str2_p++) {
    if (*str1_p != *str2_p) {
      return *str1_p - *str2_p;
    }
  }
  
  return 0;
}
#endif /* HAVE_MEMCMP == 0 */

#if HAVE_MEMCPY == 0
/*
 * Copy LEN characters from SRC to DEST
 */
void	*memcpy(void *dest, const void *src, DMALLOC_SIZE len)
{
  unsigned char		*dest_p;
  const	unsigned char	*src_p;
  int			byte_c;
  
  if (len <= 0) {
    return;
  }
  
  /*
   * NOTE: should we check to make sure that it's not overlapped but
   * there may be times where people want this, albeit bizarre,
   * behavior.
   */
  
  src_p = src;
  dest_p = dest;
  
  if (src_p <= dest_p && src_p + (len - 1) >= dest_p) {
    /* overlap, must copy right-to-left. */
    src_p += len - 1;
    dest_p += len - 1;
    for (byte_c = 0; byte_c < len; byte_c++) {
      *dest_p-- = *src_p--;
    }
  } else {
    for (byte_c = 0; byte_c < len; byte_c++) {
      *dest_p++ = *src_p++;
    }
  }
  
  return dest;
}
#endif /* HAVE_MEMCPY == 0 */

#if HAVE_MEMMOVE == 0
/*
 * Copy LEN characters from SRC to DEST
 */
void	*memmove(void *dest, const void *src, DMALLOC_SIZE len)
{
  unsigned char		*dest_p;
  const	unsigned char	*src_p;
  int			byte_c;
  
  if (len <= 0) {
    return;
  }
  
  src_p = src;
  dest_p = dest;
  
  if (src_p <= dest_p && src_p + (len - 1) >= dest_p) {
    /* overlap, must copy right-to-left. */
    src_p += len - 1;
    dest_p += len - 1;
    for (byte_c = 0; byte_c < len; byte_c++) {
      *dest_p-- = *src_p--;
    }
  } else {
    for (byte_c = 0; byte_c < len; byte_c++) {
      *dest_p++ = *src_p++;
    }
  }
  
  return dest;
}
#endif /* HAVE_MEMMOVE == 0 */

#if HAVE_MEMSET == 0
/*
 * Set LEN characters in STR to character CH
 */
void	*memset(void *str, const int ch, DMALLOC_SIZE len)
{
  unsigned char	*str_p = str;
  
  for (; len > 0; len--, str_p++) {
    *(unsigned char *)str_p = (unsigned char)ch;
  }
  
  return str;
}
#endif /* HAVE_MEMSET == 0 */

#if HAVE_STRCHR == 0
/*
 * Find CH in STR by searching backwards through the string
 */
char	*strchr(const char *str, const int ch)
{
  const char	*str_p;
  
  for (str_p = str; *str_p != '\0'; str_p++) {
    if (*str_p == (char)ch) {
      return (char *)str_p;
    }
  }
  
  if (ch == '\0') {
    return (char *)str_p;
  }
  else {
    return NULL;
  }
}
#endif /* HAVE_STRCHR == 0 */

#if HAVE_STRCMP == 0
/*
 * Returns -1,0,1 on whether STR1 is <,==,> STR2
 */
int	strcmp(const char *str1, const char *str2)
{
  for (; *str1 != '\0' && *str1 == *str2; str1++, str2++) {
  }
  return *str1 - *str2;
}
#endif /* HAVE_STRCMP == 0 */

#if HAVE_STRCPY == 0
/*
 * Copies STR2 to STR1.  Returns STR1.
 */
char	*strcpy(char *str1, const char *str2)
{
  char	*str_p;
  
  for (str_p = str1; *str2 != '\0'; str_p++, str2++) {
    *str_p = *str2;
  }
  *str_p = '\0';
  
  return str1;
}
#endif /* HAVE_STRCPY == 0 */

#if HAVE_STRLEN == 0
/*
 * Return the length in characters of STR
 */
int	strlen(const char *str)
{
  int	len;
  
  for (len = 0; *str != '\0'; str++, len++) {
  }
  
  return len;
}
#endif /* HAVE_STRLEN == 0 */

#if HAVE_STRNCMP == 0
/*
 * Compare at most LEN chars in STR1 and STR2 and return -1,0,1 or
 * STR1 - STR2
 */
int	strncmp(const char *str1, const char *str2, const int len)
{
  int	len_c;
  
  for (len_c = 0; len_c < len; len_c++, str1++, str2++) {
    if (*str1 != *str2 || *str1 == '\0') {
      return *str1 - *str2;
    }
  }
  
  return 0;
}
#endif /* HAVE_STRNCMP == 0 */

#if HAVE_STRNCPY == 0
/*
 * Copy STR2 to STR1 until LEN or null
 */
char	*strncpy(char *str1, const char *str2, const int len)
{
  char		*str1_p, null_reached_b = 0;
  int		len_c;
  
  for (len_c = 0, str1_p = str1; len_c < len; len_c++, str1_p++, str2++) {
    if (null_reached || *str2 == '\0') {
      null_reached = 1;
      *str1_p = '\0';
    }
    else {
      *str1_p = *str2;
    }
  }
  
  return str1;
}
#endif /* HAVE_STRNCPY == 0 */

#if HAVE_STRRCHR == 0
/*
 * Find CH in STR by searching backwards through the string
 */
char	*strrchr(const char *str, const int ch)
{
  const char	*str_p, *pnt = NULL;
  
  for (str_p = str; *str_p != '\0'; str_p++) {
    if (*str_p == (char)ch) {
      pnt = str_p;
    }
  }
  
  if (ch == '\0') {
    return (char *)str_p;
  }
  else {
    return (char *)pnt_p;
  }
}
#endif /* HAVE_STRRCHR == 0 */

#if HAVE_STRSEP == 0
/*
 * char *strsep
 *
 * DESCRIPTION:
 *
 * This is a function which should be in libc in every Unix.  Grumble.
 * It basically replaces the strtok function because it is reentrant.
 * This tokenizes a string by returning the next token in a string and
 * punching a \0 on the first delimiter character past the token.  The
 * difference from strtok is that you pass in the address of a string
 * pointer which will be shifted allong the buffer being processed.
 * With strtok you passed in a 0L for subsequant calls.  Yeach.
 *
 * This will count the true number of delimiter characters in the string
 * and will return an empty token (one with \0 in the zeroth position)
 * if there are two delimiter characters in a row.
 *
 * Consider the following example:
 *
 * char *tok, *str_p = "1,2,3, hello there ";
 *
 * while (1) { tok = strsep(&str_p, " ,"); if (tok == 0L) { break; } }
 *
 * strsep will return as tokens: "1", "2", "3", "", "hello", "there", "".
 * Notice the two empty "" tokens where there were two delimiter
 * characters in a row ", " and at the end of the string where there
 * was an extra delimiter character.  If you want to ignore these
 * tokens then add a test to see if the first character of the token
 * is \0.
 *
 * RETURNS:
 *
 * Success - Pointer to the next delimited token in the string.
 *
 * Failure - 0L if there are no more tokens.
 *
 * ARGUMENTS:
 *
 * string_p - Pointer to a string pointer which will be searched for
 * delimiters.  \0's will be added to this buffer.
 *
 * delim - List of delimiter characters which separate our tokens.  It
 * does not have to remain constant through all calls across the same
 * string.
 */
char	*strsep(char **string_p, const char *delim)
{
  char		*str_p, *tok;
  const char	*delim_p;
  
  /* no tokens left? */
  str_p = *string_p;
  if (str_p == 0L) {
    return 0L;
  }
  
  /* now find end of token */
  tok = str_p;
  for (; *str_p != '\0'; str_p++) {
    
    for (delim_p = delim; *delim_p != '\0'; delim_p++) {
      if (*delim_p == *str_p) {
	/* punch the '\0' */
	*str_p = '\0';
	*string_p = str_p + 1;
	return tok;
      }
    }
  }
  
  /* there are no more delimiter characters */
  *string_p = 0L;
  return tok;
}
#endif /* HAVE_STRSEP == 0 */
