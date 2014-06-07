/*
 * Specific test program for dmalloc function checking code
 *
 * Copyright 2007 by Gray Watson
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
 * $Id: dmalloc_fc_t.c,v 1.1 2007/03/23 16:20:42 gray Exp $
 */

#include <stdio.h>				/* for stdin */

#if HAVE_STDLIB_H
# include <stdlib.h>				/* for exit... */
#endif
#if HAVE_STRING_H
# include <string.h>				/* for string funcs */
#endif
#if HAVE_UNISTD_H
# include <unistd.h>				/* for getpid */
#endif

#define DMALLOC_FUNC_CHECK

#include "conf.h"

#if HAVE_TIME
# ifdef TIME_INCLUDE
#  include TIME_INCLUDE
# endif
#endif

#include "dmalloc.h"
#include "dmalloc_argv.h"
#include "dmalloc_rand.h"

#include "debug_tok.h"
#include "error_val.h"

/* argument variables */
static	char		*env_string = NULL;		/* env options */
static	int		log_trans_b = ARGV_FALSE;	/* log transactions */
static	int		random_debug_b = ARGV_FALSE;	/* random flag */
static	int		silent_b = ARGV_FALSE;		/* silent flag */
static	unsigned int	seed_random = 0;		/* random seed */
static	int		verbose_b = ARGV_FALSE;		/* verbose flag */

static	argv_t		arg_list[] = {
  { 'e',	"env-string",		ARGV_CHAR_P,		&env_string,
    "string",			"string of env commands to set" },
  { 'l',	"log-trans",		ARGV_BOOL_INT,		&log_trans_b,
    NULL,			"log transactions via tracking-func" },
  { 'r',	"random-debug",		ARGV_BOOL_INT,	       &random_debug_b,
    NULL,			"randomly change debug flag" },
  { 's',	"silent",		ARGV_BOOL_INT,		&silent_b,
    NULL,			"do not display messages" },
  { 'S',	"seed-random",		ARGV_U_INT,		&seed_random,
    "number",			"seed for random function" },
  { 'v',	"verbose",		ARGV_BOOL_INT,		&verbose_b,
    NULL,			"enables verbose messages" },
  { ARGV_LAST }
};

/*
 * Make sure that the last call did not fail.
 */
static	int	check_ok(const char *what) {
  if (dmalloc_errno == ERROR_NONE) {
    return 1;
  } else {
    if (! silent_b) {
      (void)printf("   ERROR: %s failed: %s\n",
		   what, dmalloc_strerror(dmalloc_errno));
    }
    dmalloc_message("   ERROR: %s failed: %s\n",
		    what, dmalloc_strerror(dmalloc_errno));
    return 0;
  }
}

/*
 * Make sure that the last call failed.
 */
static	int	check_fail(const char *what, const int expected_errno) {
  if (dmalloc_errno == ERROR_NONE) {
    if (! silent_b) {
      (void)printf("   ERROR: %s succeeded but should have failed with %s\n",
		   what, dmalloc_strerror(expected_errno));
    }
    dmalloc_message("   ERROR: %s succeeded but should have failed with %s\n",
		    what, dmalloc_strerror(expected_errno));
    return 0;
  } else if (dmalloc_errno == expected_errno) {
    return 1;
  } else {
    if (! silent_b) {
      (void)printf("   ERROR: %s failed with %s, expected %s\n",
		   what, dmalloc_strerror(dmalloc_errno),
		   dmalloc_strerror(expected_errno));
    }
    dmalloc_message("   ERROR: %s failed with %s, expected %s\n",
		   what, dmalloc_strerror(dmalloc_errno),
		   dmalloc_strerror(expected_errno));
    return 0;
  }
}

#if HAVE_ATOI
static	int	do_atoi(void) {
  void	*pnt;
  char	*func_name = "atoi";
  int	val, ret;
  
  pnt = malloc(5);
  val = 1234;
  strcpy(pnt, "1234");
  
  ret = atoi(pnt);
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (ret != val) {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have gotten %d but got %d\n",
		   func_name, val, ret);
    }
    return 0;
  }
  
  memmove(pnt, "12345", 5);
  atoi(pnt);
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_ATOL
static	int	do_atol(void) {
  void	*pnt;
  char	*func_name = "atol";
  long	val, ret;
  
  pnt = malloc(5);
  val = 1234L;
  strcpy(pnt, "1234");
  
  ret = atol(pnt);
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (ret != val) {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have gotten %ld but got %ld\n",
		   func_name, val, ret);
    }
    return 0;
  }
  
  memmove(pnt, "12345", 5);
  atol(pnt);
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_BCMP
static	int	do_bcmp(void) {
  char	*pnt, *val;
  char	*func_name = "bcmp";
  int	ret;
  
  pnt = malloc(5);
  val = "12345";
  memmove(pnt, val, strlen(val));
  
  ret = bcmp(pnt, val, 5);
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (ret != 0) {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have gotten 0 got %d\n",
		   func_name, ret);
    }
    return 0;
  }
  
  val = "123456";
  memmove(pnt, val, strlen(val));
  bcmp(pnt, val, 6);
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_BCOPY
static	int	do_bcopy(void) {
  char	*pnt, *val;
  char	*func_name = "bcopy";
  
  pnt = malloc(5);
  val = "12345";
  
  bcopy(val, pnt, strlen(val));
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (memcmp(pnt, val, 5) != 0) {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have copied the bytes\n", func_name);
    }
    return 0;
  }
  
  val = "123456";
  bcopy(val, pnt, strlen(val));
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_BZERO
static	int	do_bzero(void) {
  char	*pnt;
  char	*func_name = "bzero";
  char	*zeros = "\000\000\000\000\000";
  int	size = 5;
  
  pnt = malloc(size);
  strcpy(pnt, "foo");
  
  if (memcmp(pnt, zeros, size) == 0) {
    if (! silent_b) {
      (void)printf("   ERROR: %s pnt should not have zeros initially\n",
		   func_name);
    }
    return 0;
  }
  
  bzero(pnt, size);
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (memcmp(pnt, zeros, size) != 0) {
    if (! silent_b) {
      (void)printf("   ERROR: %s pnt should have zeros\n", func_name);
    }
    return 0;
  }
  
  bzero(pnt, size + 1);
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_INDEX
static	int	do_index(void) {
  char	*pnt, *val, *ret;
  char	*func_name = "index";
  
  pnt = malloc(5);
  val = "foot";
  strcpy(pnt, val);
  
  ret = index(pnt, 't');
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (*ret != 't') {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have found the t\n", func_name);
    }
    return 0;
  }
  
  val = "footy";
  memmove(pnt, val, strlen(val));
  index(pnt, 'u');
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_MEMCCPY
static	int	do_memccpy(void) {
  char	*pnt, *val, *ret;
  char	*func_name = "memccpy";
  
  pnt = malloc(5);
  val = "footy";
  
  ret = memccpy(pnt, val, 'z', strlen(val));
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (ret != 0L) {
    if (! silent_b) {
      (void)printf("   ERROR: %s pointer should have returned 0L\n",
		   func_name);
    }
    return 0;
  }
  if (memcmp(pnt, val, strlen(val)) != 0) {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have copied the string\n", func_name);
    }
    return 0;
  }
  
  val = "footies";
  memccpy(pnt, val, 'z', strlen(val));
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_MEMCHR
static	int	do_memchr(void) {
  char	*pnt, *val, *ret;
  char	*func_name = "memchr";
  
  pnt = malloc(5);
  val = "footy";
  memmove(pnt, val, strlen(val));
  
  ret = memchr(pnt, 't', strlen(val));
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (*ret != 't') {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have found the t\n", func_name);
    }
    return 0;
  }
  
  memchr(pnt, 'u', strlen(val) + 1);
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_MEMCMP
static	int	do_memcmp(void) {
  char	*pnt, *val;
  char	*func_name = "memcmp";
  int	ret;
  
  pnt = malloc(5);
  val = "12345";
  memmove(pnt, val, strlen(val));
  
  ret = memcmp(pnt, val, strlen(val));
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (ret != 0) {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have gotten 0 got %d\n",
		   func_name, ret);
    }
    return 0;
  }
  
  val = "123456";
  memmove(pnt, val, strlen(val));
  memcmp(pnt, val, 6);
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_MEMCPY
static	int	do_memcpy(void) {
  char	*pnt, *val;
  char	*func_name = "memcpy";
  
  pnt = malloc(5);
  val = "12345";
  
  memcpy(pnt, val, strlen(val));
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (memcmp(pnt, val, strlen(val)) != 0) {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have copied the bytes\n", func_name);
    }
    return 0;
  }
  
  val = "123456";
  memcpy(pnt, val, strlen(val));
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_MEMMOVE
static	int	do_memmove(void) {
  char	*pnt, *val;
  char	*func_name = "memmove";
  
  pnt = malloc(5);
  val = "12345";
  
  memmove(pnt, val, strlen(val));
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (memcmp(pnt, val, strlen(val)) != 0) {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have copied the bytes\n", func_name);
    }
    return 0;
  }
  
  val = "123456";
  memmove(pnt, val, strlen(val));
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_MEMSET
static	int	do_memset(void) {
  char	*pnt;
  char	*func_name = "memset";
  char	*zeros = "\000\000\000\000\000";
  int	size = 5;
  
  pnt = malloc(size);
  strcpy(pnt, "foo");
  
  if (memcmp(pnt, zeros, size) == 0) {
    if (! silent_b) {
      (void)printf("   ERROR: %s pnt should not have zeros initially\n",
		   func_name);
    }
    return 0;
  }
  
  memset(pnt, 0, size);
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (memcmp(pnt, zeros, size) != 0) {
    if (! silent_b) {
      (void)printf("   ERROR: %s pnt should have zeros\n", func_name);
    }
    return 0;
  }
  
  memset(pnt, 0, size + 1);
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_RINDEX
static	int	do_rindex(void) {
  char	*pnt, *val, *ret;
  char	*func_name = "rindex";
  
  pnt = malloc(5);
  val = "foot";
  strcpy(pnt, val);
  
  ret = index(pnt, *val);
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (ret != pnt) {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have found the f\n", func_name);
    }
    return 0;
  }
  
  val = "footy";
  memmove(pnt, val, strlen(val));
  rindex(pnt, *val);
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_STRCASECMP
static	int	do_strcasecmp(void) {
  char	*pnt, *val, *big_val;
  char	*func_name = "strcasecmp";
  int	ret;
  
  pnt = malloc(5);
  val = "abcd";
  big_val = "ABCD";
  strcpy(pnt, big_val);
  
  ret = strcasecmp(pnt, val);
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (ret != 0) {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have gotten 0 got %d\n",
		   func_name, ret);
    }
    return 0;
  }
  
  val = "abcdef";
  big_val = "ABCDEF";
  memmove(pnt, big_val, strlen(big_val));
  strcasecmp(pnt, val);
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_STRCAT
static	int	do_strcat(void) {
  char	*pnt, *val;
  char	*func_name = "strcat";
  
  pnt = malloc(5);
  val = "ab";
  strcpy(pnt, val);
  
  strcat(pnt, val);
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (memcmp(pnt, val, strlen(val)) != 0) {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have copied string in\n", func_name);
    }
    return 0;
  }
  if (memcmp(pnt + strlen(val), val, strlen(val)) != 0) {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have appending string\n", func_name);
    }
    return 0;
  }
  
  strcat(pnt, val);
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_STRCHR
static	int	do_strchr(void) {
  char	*pnt, *val, *ret;
  char	*func_name = "strchr";
  
  pnt = malloc(5);
  val = "foot";
  strcpy(pnt, val);

  ret = strchr(pnt, 't');
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (*ret != 't') {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have found the t\n", func_name);
    }
    return 0;
  }
  
  val = "footy";
  memmove(pnt, val, strlen(val));
  strchr(pnt, 'y');
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_STRCMP
static	int	do_strcmp(void) {
  char	*pnt, *val;
  char	*func_name = "strcmp";
  int	ret;
  
  pnt = malloc(5);
  val = "foot";
  strcpy(pnt, val);
  
  ret = strcmp(pnt, val);
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (ret != 0) {
    if (! silent_b) {
      (void)printf("   ERROR: %s strings should have been the same\n",
		   func_name);
    }
    return 0;
  }
  
  val = "footy";
  memmove(pnt, val, strlen(val));
  strcmp(pnt, val);
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_STRCPY
static	int	do_strcpy(void) {
  char	*pnt, *val;
  char	*func_name = "strcpy";
  
  pnt = malloc(5);
  val = "1234";
  
  strcpy(pnt, val);
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (memcmp(pnt, val, strlen(val)) != 0) {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have copied the bytes\n", func_name);
    }
    return 0;
  }
  
  val = "12345";
  strcpy(pnt, val);
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_STRCSPN
static	int	do_strcspn(void) {
  char	*pnt, *val;
  char	*func_name = "strcspn";
  int	ret;
  
  pnt = malloc(5);
  val = "1234";
  
  ret = strcspn(pnt, val);
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (ret != 0) {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have found all chars\n", func_name);
    }
    return 0;
  }
  
  /* can't make this fail */
  return 1;
}
#endif

#if HAVE_STRLEN
static	int	do_strlen(void) {
  char	*pnt, *val;
  char	*func_name = "strlen";
  int	ret;
  
  pnt = malloc(5);
  val = "1234";
  strcpy(pnt, val);
  
  ret = strlen(pnt);
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (ret != _dmalloc_strlen(__FILE__, __LINE__, val)) {
    if (! silent_b) {
      (void)printf("   ERROR: %s got improper string length\n", func_name);
    }
    return 0;
  }
  
  val = "12345";
  memmove(pnt, val, strlen(val));
  
  strlen(pnt);
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_STRNCASECMP
static	int	do_strncasecmp(void) {
  char	*pnt, *val, *big_val;
  char	*func_name = "strncasecmp";
  int	ret;
  
  pnt = malloc(5);
  val = "abcde";
  big_val = "ABCDE";
  memmove(pnt, big_val, strlen(big_val));
  
  ret = strncasecmp(pnt, val, strlen(val));
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (ret != 0) {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have gotten 0 got %d\n",
		   func_name, ret);
    }
    return 0;
  }
  
  big_val = "ABCDEF";
  strncasecmp(pnt, big_val, strlen(big_val));
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_STRNCAT
static	int	do_strncat(void) {
  char	*pnt, *val;
  char	*func_name = "strncat";
  
  pnt = malloc(5);
  val = "ab";
  strcpy(pnt, val);
  
  strncat(pnt, val, strlen(val));
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (memcmp(pnt, val, strlen(val)) != 0) {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have copied string in\n", func_name);
    }
    return 0;
  }
  if (memcmp(pnt + strlen(val), val, strlen(val)) != 0) {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have appending string\n", func_name);
    }
    return 0;
  }
  
  strncat(pnt, val, strlen(val));
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_STRNCMP
static	int	do_strncmp(void) {
  char	*pnt, *val;
  char	*func_name = "strncmp";
  int	ret;
  
  pnt = malloc(5);
  val = "footy";
  memmove(pnt, val, strlen(val));
  
  ret = strncmp(pnt, val, strlen(val));
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (ret != 0) {
    if (! silent_b) {
      (void)printf("   ERROR: %s strings should have been the same\n",
		   func_name);
    }
    return 0;
  }
  
  strncmp(pnt, val, strlen(val) + 1);
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_STRNCPY
static	int	do_strncpy(void) {
  char	*pnt, *val;
  char	*func_name = "strncpy";
  
  pnt = malloc(5);
  val = "12345";
  
  strncpy(pnt, val, strlen(val));
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (memcmp(pnt, val, strlen(val)) != 0) {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have copied the bytes\n", func_name);
    }
    return 0;
  }
  
  val = "123456";
  strncpy(pnt, val, strlen(val));
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_STRPBRK
static	int	do_strpbrk(void) {
  char	*pnt, *val, *ret;
  char	*func_name = "strpbrk";
  
  pnt = malloc(5);
  val = "foot";
  strcpy(pnt, val);

  ret = strpbrk(pnt, "t");
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (*ret != 't') {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have found the t\n", func_name);
    }
    return 0;
  }
  
  val = "footy";
  memmove(pnt, val, strlen(val));
  strpbrk(pnt, "y");
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_STRRCHR
static	int	do_strrchr(void) {
  char	*pnt, *val, *ret;
  char	*func_name = "rindex";
  
  pnt = malloc(5);
  val = "foot";
  strcpy(pnt, val);
  
  ret = strrchr(pnt, *val);
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (ret != pnt) {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have found the f\n", func_name);
    }
    return 0;
  }
  
  val = "footy";
  memmove(pnt, val, strlen(val));
  strrchr(pnt, *val);
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_STRSPN
static	int	do_strspn(void) {
  char	*pnt, *val;
  char	*func_name = "strspn";
  int	ret;
  
  pnt = malloc(5);
  val = "foot";
  strcpy(pnt, val);

  ret = strspn(pnt, val);
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (ret != strlen(val)) {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have counted all chars\n", func_name);
    }
    return 0;
  }
  
  val = "footy";
  memmove(pnt, val, strlen(val));
  strspn(pnt, val);
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_STRSTR
static	int	do_strstr(void) {
  char	*pnt, *val, *ret;
  char	*func_name = "strstr";
  
  pnt = malloc(5);
  val = "foot";
  strcpy(pnt, val);

  ret = strstr(pnt, "ot");
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (ret - pnt != 2) {
    if (! silent_b) {
      (void)printf("   ERROR: %s should have found the ot\n", func_name);
    }
    return 0;
  }
  
  val = "footy";
  memmove(pnt, val, strlen(val));
  strstr(pnt, "none");
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

#if HAVE_STRTOK
static	int	do_strtok(void) {
  char	*pnt, *val, *ret;
  char	*func_name = "strtok";
  
  pnt = malloc(5);
  val = "a,b,";
  strcpy(pnt, val);

  ret = strtok(pnt, ",");
  if (! check_ok(func_name)) {
    return 0;
  }
  
  if (ret != pnt) {
    if (! silent_b) {
      (void)printf("   ERROR: %s first token should be start of pnt\n",
		   func_name);
    }
    return 0;
  }
  
  val = "a,b,c";
  memmove(pnt, val, strlen(val));
  strtok(pnt, ",");
  return check_fail(func_name, ERROR_WOULD_OVERWRITE);
}
#endif

static	int	(*test_funcs[])() = {
#if HAVE_ATOI
  do_atoi,
#endif
#if HAVE_ATOL
  do_atol,
#endif
#if HAVE_BCMP
  do_bcmp,
#endif
#if HAVE_BCOPY
  do_bcopy,
#endif
#if HAVE_BZERO
  do_bzero,
#endif
#if HAVE_INDEX
  do_index,
#endif
#if HAVE_MEMCCPY
  do_memccpy,
#endif
#if HAVE_MEMCHR
  do_memchr,
#endif
#if HAVE_MEMCMP
  do_memcmp,
#endif
#if HAVE_MEMCPY
  do_memcpy,
#endif
#if HAVE_MEMMOVE
  do_memmove,
#endif
#if HAVE_MEMSET
  do_memset,
#endif
#if HAVE_RINDEX
  do_rindex,
#endif
#if HAVE_STRCASECMP
  do_strcasecmp,
#endif
#if HAVE_STRCAT
  do_strcat,
#endif
#if HAVE_STRCHR
  do_strchr,
#endif
#if HAVE_STRCMP
  do_strcmp,
#endif
#if HAVE_STRCPY
  do_strcpy,
#endif
#if HAVE_STRCSPN
  do_strcspn,
#endif
#if HAVE_STRLEN
  do_strlen,
#endif
#if HAVE_STRNCASECMP
  do_strncasecmp,
#endif
#if HAVE_STRNCAT
  do_strncat,
#endif
#if HAVE_STRNCMP
  do_strncmp,
#endif
#if HAVE_STRNCPY
  do_strncpy,
#endif
#if HAVE_STRPBRK
  do_strpbrk,
#endif
#if HAVE_STRRCHR
  do_strrchr,
#endif
#if HAVE_STRSPN
  do_strspn,
#endif
#if HAVE_STRSTR
  do_strstr,
#endif
#if HAVE_STRTOK
  do_strtok,
#endif
  0L,
};

static	int	do_tests(void) {
  int		final = 1, prev_errno, test_c;
  unsigned int	old_flags;
  
  old_flags = dmalloc_debug_current();
  dmalloc_debug(old_flags | DEBUG_CHECK_FUNCS);
  
  for (test_c = 0; test_funcs[test_c] != 0L; test_c++) {
    prev_errno = dmalloc_errno;
    dmalloc_errno = ERROR_NONE;
    final &= test_funcs[test_c]();
    if (prev_errno != ERROR_NONE) {
      dmalloc_errno = prev_errno;
    }
  }
  
  dmalloc_debug(old_flags);
  return final;
}

int	main(int argc, char **argv) {
  int	ret, final = 0;
  
  argv_process(arg_list, argc, argv);
  
  if (silent_b && verbose_b) {
    silent_b = ARGV_FALSE;
  }
  
  if (env_string != NULL) {
    dmalloc_debug_setup(env_string);
    if (! silent_b) {
      (void)printf("Set dmalloc environment to: %s\n", env_string);
    }
  }
  
  /* repeat until we get a non 0 seed */
  while (seed_random == 0) {
#ifdef HAVE_TIME
#ifdef HAVE_GETPID
    seed_random = time(0) ^ getpid();
#else /* ! HAVE_GETPID */
    seed_random = time(0) ^ 0xDEADBEEF;
#endif /* ! HAVE_GETPID */
#else /* ! HAVE_TIME */
#ifdef HAVE_GETPID
    seed_random = getpid();
#else /* ! HAVE_GETPID */
    /* okay, I give up */
    seed_random = 0xDEADBEEF;
#endif /* ! HAVE_GETPID */
#endif /* ! HAVE_TIME */
  }
  _dmalloc_srand(seed_random);
  
  if (! silent_b) {
    (void)printf("Random seed is %u\n", seed_random);
  }
  
  dmalloc_message("random seed is %u\n", seed_random);
  
  /*************************************************/
  
  if (! silent_b) {
    (void)printf("Running tests...\n");
  }
  (void)fflush(stdout);
  
  if (do_tests()) {
    if (! silent_b) {
      (void)printf("  Succeeded.\n");
    }
  }
  else {
    if (silent_b) {
      (void)printf("ERROR: Random tests failed.  Last dmalloc error: %s (err %d)\n",
		   dmalloc_strerror(dmalloc_errno), dmalloc_errno);
    }
    else {
      (void)printf("  Failed.  Last dmalloc error: %s (err %d)\n",
		   dmalloc_strerror(dmalloc_errno), dmalloc_errno);
    }
    final = 1;
  }
  
  /*************************************************/
  
  
  if (final != 0) {
    /*
     * Even if we are silent, we must give the random seed which is
     * the only way we can reproduce the problem.
     */
    if (silent_b) {
      (void)printf("Random seed is %u.  Final dmalloc error: %s (err %d)\n",
		   seed_random, dmalloc_strerror(dmalloc_errno),
		   dmalloc_errno);
    }
    else {
      (void)printf("Final dmalloc error: %s (err %d)\n",
		   dmalloc_strerror(dmalloc_errno), dmalloc_errno);
    }
  }
  
  argv_cleanup(arg_list);
  
  /* last thing is to verify the heap */
  ret = dmalloc_verify(NULL /* check all heap */);
  if (ret != DMALLOC_NOERROR) {
    (void)printf("Final dmalloc_verify returned failure: %s (err %d)\n",
		 dmalloc_strerror(dmalloc_errno), dmalloc_errno);
  }
  
  /* you will need this if you can't auto-shutdown */
#if HAVE_ATEXIT == 0 && HAVE_ON_EXIT == 0 && FINI_DMALLOC == 0
  /* shutdown the alloc routines */
  dmalloc_shutdown();
#endif
  
  exit(final);
}
