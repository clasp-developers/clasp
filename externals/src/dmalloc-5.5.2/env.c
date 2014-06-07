/*
 * Environment handling routines
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
 * $Id: env.c,v 1.37 2007/05/14 17:09:20 gray Exp $
 */

/*
 * This file contains short routine(s) to process the environment
 * variable(s) used by the library to get the runtime option(s).
 */

#define DMALLOC_DISABLE

#if HAVE_STDLIB_H
# include <stdlib.h>
#endif
#if HAVE_STRING_H
# include <string.h>
#endif
#if HAVE_UNISTD_H
# include <unistd.h>				/* for getpid */
#endif

#include "conf.h"
#include "dmalloc.h"

#include "compat.h"
#include "dmalloc_loc.h"
#include "debug_tok.h"
#include "env.h"
#include "error.h"

/*
 * Environmental labels.
 *
 * NOTE: the decision has been made _not_ to do the sizeof() hack for
 * portability reasons.
 */
#define ADDRESS_LABEL		"addr"
#define DEBUG_LABEL		"debug"
#define INTERVAL_LABEL		"inter"
#define LOCK_ON_LABEL		"lockon"
#define LOGFILE_LABEL		"log"
#define START_LABEL		"start"
#define LIMIT_LABEL		"limit"

#define ASSIGNMENT_CHAR		'='

/* local variables */
static	char		log_path[512]	= { '\0' }; /* storage for env path */
static	char		start_file[512] = { '\0' }; /* file to start at */

/****************************** local utilities ******************************/

/*
 * Hexadecimal STR to int translation
 */
static	long	hex_to_long(const char *str)
{
  long		ret;
  
  /* strip off spaces */
  for (; *str == ' ' || *str == '\t'; str++) {
  }
  
  /* skip a leading 0[xX] */
  if (*str == '0' && (*(str + 1) == 'x' || *(str + 1) == 'X')) {
    str += 2;
  }
  
  for (ret = 0;; str++) {
    if (*str >= '0' && *str <= '9') {
      ret = ret * 16 + (*str - '0');
    }
    else if (*str >= 'a' && *str <= 'f') {
      ret = ret * 16 + (*str - 'a' + 10);
    }
    else if (*str >= 'A' && *str <= 'F') {
      ret = ret * 16 + (*str - 'A' + 10);
    }
    else {
      break;
    }
  }
  
  return ret;
}

/***************************** exported routines *****************************/

/*
 * Break up ADDR_ALL into ADDR_P and ADDR_COUNT_P
 */
void	_dmalloc_address_break(const char *addr_all, DMALLOC_PNT *addr_p,
			       unsigned long *addr_count_p)
{
  char	*colon_p;
  
  SET_POINTER(addr_p, (DMALLOC_PNT)hex_to_long(addr_all));
  if (addr_count_p != NULL) {
    colon_p = strchr(addr_all, ':');
    if (colon_p != NULL) {
      *addr_count_p = loc_atoul(colon_p + 1);
    }
  }
}

/*
 * Break up START_ALL into SFILE_P, SLINE_P, and SCOUNT_P
 */
void	_dmalloc_start_break(char *start_all, char **start_file_p,
			     int *start_line_p, unsigned long *start_iter_p,
			     unsigned long *start_size_p)
{
  char	*start_p;
  
  start_p = strchr(start_all, ':');
  if (start_p != NULL) {
    (void)strncpy(start_file, start_all, sizeof(start_file));
    start_file[sizeof(start_file) - 1] = '\0';
    SET_POINTER(start_file_p, start_file);
    start_p = start_file + (start_p - start_all);
    *start_p = '\0';
    SET_POINTER(start_line_p, atoi(start_p + 1));
    SET_POINTER(start_iter_p, 0);
    SET_POINTER(start_size_p, 0);
  }
  else if (start_all[0] == 's') {
    SET_POINTER(start_file_p, NULL);
    SET_POINTER(start_line_p, 0);
    SET_POINTER(start_iter_p, 0);
    SET_POINTER(start_size_p, loc_atoul(start_all + 1));
  }
  else {
    SET_POINTER(start_file_p, NULL);
    SET_POINTER(start_line_p, 0);
    if (start_all[0] == 'c') {
      SET_POINTER(start_iter_p, loc_atoul(start_all + 1));
    }
    else {
      SET_POINTER(start_iter_p, loc_atoul(start_all));
    }
    SET_POINTER(start_size_p, 0);
  }
}

/*
 * Process the values of dmalloc environ variable(s) from ENVIRON
 * string.
 */
void	_dmalloc_environ_process(const char *env_str, DMALLOC_PNT *addr_p,
				 unsigned long *addr_count_p,
				 unsigned int *debug_p,
				 unsigned long *interval_p, int *lock_on_p,
				 char **logpath_p, char **start_file_p,
				 int *start_line_p,
				 unsigned long *start_iter_p,
				 unsigned long *start_size_p,
				 unsigned long *limit_p)
{
  char		*env_p, *this_p;
  char		buf[1024];
  int		len, done_b = 0;
  unsigned int	flags = 0;
  attr_t	*attr_p;
  
  SET_POINTER(addr_p, NULL);
  SET_POINTER(addr_count_p, 0);
  SET_POINTER(debug_p, 0);
  SET_POINTER(interval_p, 0);
  SET_POINTER(lock_on_p, 0);
  SET_POINTER(logpath_p, NULL);
  SET_POINTER(start_file_p, NULL);
  SET_POINTER(start_line_p, 0);
  SET_POINTER(start_iter_p, 0);
  SET_POINTER(start_size_p, 0);
  SET_POINTER(limit_p, 0);
  
  /* make a copy */
  (void)strncpy(buf, env_str, sizeof(buf));
  buf[sizeof(buf) - 1] = '\0';
  
  /* handle each of tokens, in turn */
  for (env_p = buf, this_p = buf; ! done_b; env_p++, this_p = env_p) {
    
    /* find the comma of end */
    for (;; env_p++) {
      if (*env_p == '\0') {
	done_b = 1;
	break;
      }
      if (*env_p == ',' && (env_p == buf || *(env_p - 1) != '\\')) {
	break;
      }
    }
    
    /* should we strip ' ' or '\t' here? */
    
    if (this_p == env_p) {
      continue;
    }
    
    *env_p = '\0';
    
    len = strlen(ADDRESS_LABEL);
    if (strncmp(this_p, ADDRESS_LABEL, len) == 0
	&& *(this_p + len) == ASSIGNMENT_CHAR) {
      this_p += len + 1;
      _dmalloc_address_break(this_p, addr_p, addr_count_p);
      continue;
    }
    
    len = strlen(DEBUG_LABEL);
    if (strncmp(this_p, DEBUG_LABEL, len) == 0
	&& *(this_p + len) == ASSIGNMENT_CHAR) {
      this_p += len + 1;
      SET_POINTER(debug_p, hex_to_long(this_p));
      continue;
    }
    
    len = strlen(INTERVAL_LABEL);
    if (strncmp(this_p, INTERVAL_LABEL, len) == 0
	&& *(this_p + len) == ASSIGNMENT_CHAR) {
      this_p += len + 1;
      SET_POINTER(interval_p, loc_atoul(this_p));
      continue;
    }
    
    len = strlen(LOCK_ON_LABEL);
    if (strncmp(this_p, LOCK_ON_LABEL, len) == 0
	&& *(this_p + len) == ASSIGNMENT_CHAR) {
      this_p += len + 1;
      SET_POINTER(lock_on_p, atoi(this_p));
      continue;
    }
    
    /* get the dmalloc logfile name into a holding variable */
    len = strlen(LOGFILE_LABEL);
    if (strncmp(this_p, LOGFILE_LABEL, len) == 0
	&& *(this_p + len) == ASSIGNMENT_CHAR) {
      this_p += len + 1;
      (void)strncpy(log_path, this_p, sizeof(log_path));
      log_path[sizeof(log_path) - 1] = '\0';
      SET_POINTER(logpath_p, log_path);
      continue;
    }
    
    /*
     * start checking the heap after X iterations OR
     * start at a file:line combination
     */
    len = strlen(START_LABEL);
    if (strncmp(this_p, START_LABEL, len) == 0
	&& *(this_p + len) == ASSIGNMENT_CHAR) {
      this_p += len + 1;
      _dmalloc_start_break(this_p, start_file_p, start_line_p, start_iter_p,
			   start_size_p);
      continue;
    }
    
    /* set the memory limit to the library */
    len = strlen(LIMIT_LABEL);
    if (strncmp(this_p, LIMIT_LABEL, len) == 0
	&& *(this_p + len) == ASSIGNMENT_CHAR) {
      this_p += len + 1;
      SET_POINTER(limit_p, loc_atoul(this_p));
      continue;
    }
    
    /* need to check the short/long debug options */
    for (attr_p = attributes; attr_p->at_string != NULL; attr_p++) {
      if (strcmp(this_p, attr_p->at_string) == 0) {
	flags |= attr_p->at_value;
	break;
      }
    }
    if (attr_p->at_string != NULL) {
      continue;
    }
  }
  
  /* append the token settings to the debug setting */
  if (debug_p != NULL) {
    if (*debug_p == 0) {
      *debug_p = flags;
    }
    else {
      *debug_p |= flags;
    }
  }
}

/*
 * Set dmalloc environ variable(s) with the values (maybe SHORT debug
 * info) into BUF.
 */
void	_dmalloc_environ_set(char *buf, const int buf_size,
			     const int long_tokens_b,
			     const DMALLOC_PNT address,
			     const unsigned long addr_count,
			     const unsigned int debug,
			     const unsigned long interval, const int lock_on,
			     const char *logpath, const char *start_file_p,
			     const int start_line,
			     const unsigned long start_iter,
			     const unsigned long start_size,
			     const unsigned long limit_val)
{
  char	*buf_p = buf, *bounds_p = buf + buf_size;
  
  if (debug > 0) {
    if (long_tokens_b) {
      attr_t	*attr_p;
      
      for (attr_p = attributes; attr_p->at_string != NULL; attr_p++) {
	if (debug & attr_p->at_value) {
	  buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "%s,",
				attr_p->at_string);
	}
      }
    }
    else {
      buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "%s%c%#x,",
			    DEBUG_LABEL, ASSIGNMENT_CHAR, debug);
    }
  }
  if (address != NULL) {
    if (addr_count > 0) {
      buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "%s%c%#lx:%lu,",
			    ADDRESS_LABEL, ASSIGNMENT_CHAR, (long)address,
			    addr_count);
    }
    else {
      buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "%s%c%#lx,",
			    ADDRESS_LABEL, ASSIGNMENT_CHAR, (long)address);
    }
  }
  if (interval > 0) {
    buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "%s%c%lu,",
			  INTERVAL_LABEL, ASSIGNMENT_CHAR, interval);
  }
  if (lock_on > 0) {
    buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "%s%c%d,",
			  LOCK_ON_LABEL, ASSIGNMENT_CHAR, lock_on);
  }
  if (logpath != NULL) {
    buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "%s%c%s,",
			  LOGFILE_LABEL, ASSIGNMENT_CHAR, logpath);
  }
  if (start_file_p != NULL) {
    if (start_line > 0) {
      buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "%s%c%s:%d,",
			    START_LABEL, ASSIGNMENT_CHAR, start_file_p,
			    start_line);
    }
    else {
      buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "%s%c%s,",
			    START_LABEL, ASSIGNMENT_CHAR, start_file_p);
    }
  }
  else if (start_iter > 0) {
    /* NOTE: there is an 'c' (for count) before the iter variable here */
    buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "%s%cc%lu,",
			  START_LABEL, ASSIGNMENT_CHAR, start_iter);
  }
  else if (start_size > 0) {
    /* NOTE: there is an 's' before the size variable here */
    buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "%s%cs%lu,",
			  START_LABEL, ASSIGNMENT_CHAR, start_size);
  }
  if (limit_val > 0) {
    buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "%s%c%lu,",
			  LIMIT_LABEL, ASSIGNMENT_CHAR, limit_val);
  }
  
  /* cut off the last comma */
  if (buf_p > buf) {
    buf_p--;
  }
  
  *buf_p = '\0';
}
