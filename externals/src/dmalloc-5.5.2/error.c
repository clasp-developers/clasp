/*
 * Error and message routines
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
 * $Id: error.c,v 1.114 2004/08/13 21:26:27 gray Exp $
 */

/*
 * This file contains the routines needed for processing error codes
 * produced by the library.
 */

#include <fcntl.h>				/* for O_WRONLY, etc. */
#include <stdio.h>

#if HAVE_STDARG_H
# include <stdarg.h>				/* for message vsprintf */
#endif
#if HAVE_STDLIB_H
# include <stdlib.h>				/* for abort */
#endif
#if HAVE_UNISTD_H
# include <unistd.h>				/* for write */
#endif

#include "conf.h"				/* up here for _INCLUDE */

/* for KILL_PROCESS define */
#if USE_ABORT == 0
#ifdef KILL_INCLUDE
#include KILL_INCLUDE				/* for kill signals */
#endif
#endif

#if LOG_PNT_TIMEVAL
# ifdef TIMEVAL_INCLUDE
#  include TIMEVAL_INCLUDE
# endif
#else
# if HAVE_TIME
#  ifdef TIME_INCLUDE
#   include TIME_INCLUDE
#  endif
# endif
#endif

#define DMALLOC_DISABLE

#include "dmalloc.h"

#include "chunk.h"				/* for _dmalloc_memory_limit */
#include "compat.h"
#include "debug_tok.h"
#include "env.h"				/* for LOGPATH_INIT */
#include "error.h"
#include "error_val.h"
#include "dmalloc_loc.h"
#include "version.h"

#if LOCK_THREADS
#if IDENT_WORKS
#ident "@(#) $Information: lock-threads is enabled $"
#else
static char *information = "@(#) $Information: lock-threads is enabled $";
#endif
#endif

#define MINS_IN_HOUR	60
#define SECS_IN_MIN	60
#define SECS_IN_HOUR	(MINS_IN_HOUR * SECS_IN_MIN)

/* external routines */
extern	const char	*dmalloc_strerror(const int errnum);

/*
 * exported variables
 */

/* address to look for.  when discovered call dmalloc_error() */
DMALLOC_PNT	_dmalloc_address = NULL;
/* when to stop at an address */
unsigned long	_dmalloc_address_seen_n = 0;

/* global debug flags that are set my DMALLOC_DEBUG environ variable */
unsigned int	_dmalloc_flags = 0;

/* global iteration counter for activities */
unsigned long	_dmalloc_iter_c = 0;

/* how often to check the heap */
unsigned long	_dmalloc_check_interval = 0;

#if LOG_PNT_TIMEVAL
/* overhead information storing when the library started up for elapsed time */
TIMEVAL_TYPE	_dmalloc_start;
#endif

/* NOTE: we do the ifdef this way for fillproto */
#if LOG_PNT_TIMEVAL == 0
#if HAVE_TIME
TIME_TYPE	_dmalloc_start = 0;
#endif
#endif

/* when we are going to startup our locking subsystem */
int		_dmalloc_lock_on = 0;

/* global flag which indicates when we are aborting */
int		_dmalloc_aborting_b = 0;

/* local variables */
static	int	outfile_fd = -1;		/* output file descriptor */
/* the following are here to reduce stack overhead */
static	char	error_str[1024];		/* error string buffer */
static	char	message_str[1024];		/* message string buffer */

/*
 * void _dmalloc_open_log
 *
 * DESCRIPTION:
 *
 * Open up our log file and write some version of settings
 * information.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * None.
 */
static	void	build_logfile_path(char *buf, const int buf_len)
{
  char	*bounds_p;
  char	*path_p, *buf_p, *start_p;
  int	len;
  
  if (dmalloc_logpath == NULL) {
    buf[0] = '\0';
    return;
  }
  
  buf_p = buf;
  bounds_p = buf + buf_len;
  
  start_p = dmalloc_logpath;
  for (path_p = dmalloc_logpath; *path_p != '\0'; path_p++) {
    
    /* if we don't have to do anything special then just continue */
    if (*path_p != '%' || *(path_p + 1) == '\0') {
      if (buf_p < bounds_p) {
	*buf_p++ = *path_p;
      }
      continue;
    }
    
    /* skip over the % */
    path_p++;
    
    /* dump the hostname */
    if (*path_p == 'h') {
#if HAVE_GETHOSTNAME
      char	our_host[128];
      gethostname(our_host, sizeof(our_host));
      buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "%s", our_host);
#else
      buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "not-gethostname");
#endif
    }
    /* dump the thread-id */
    if (*path_p == 'i') {
#if LOG_PNT_THREAD_ID
      char		id_str[256];
      THREAD_TYPE	id;
      
      id = THREAD_GET_ID();
      THREAD_ID_TO_STRING(id_str, sizeof(id_str), id);
      buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "%s", id_str);
#else
      buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "no-thread-id");
#endif
    }
    /* dump the pid -- also support backwards compatibility with %d */
    if (*path_p == 'p' || *path_p == 'd') {
#if HAVE_GETPID
      /* we make it long in case it's big and we hope it will promote if not */
      long	our_pid = getpid();
      buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "%ld", our_pid);
#else
      buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "no-getpid");
#endif
    }
    /* dump the time value */
    if (*path_p == 't') {
#if HAVE_TIME
      /* we make time a long here so it will promote */
      long	now;
      now = time(NULL);
      buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "%ld", now);
#else
      buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "no-time");
#endif
    }
    /* dump the user-id */
    if (*path_p == 'u') {
#if HAVE_GETUID
      /* we make it long in case it's big and we hope it will promote if not */
      long	our_uid = getuid();
      buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "%ld", our_uid);
#else
      buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "no-uid");
#endif
    }
  }
  
  if (buf_p >= bounds_p - 1) {
    /* NOTE: we can't use dmalloc_message of course so do it the hard way */
    len = loc_snprintf(error_str, sizeof(error_str),
		       "debug-malloc library: logfile path too large '%s'\r\n",
		       dmalloc_logpath);
    (void)write(STDERR, error_str, len);
  }
  
  *buf_p = '\0';
}

/*
 * void _dmalloc_open_log
 *
 * DESCRIPTION:
 *
 * Open up our log file and write some version of settings
 * information.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * None.
 */
void	_dmalloc_open_log(void)
{
  char	log_path[1024];
  int	len;
  
  /* if it's already open or if we don't have a log file configured */
  if (outfile_fd >= 0
      || dmalloc_logpath == NULL) {
    return;
  }
  
  build_logfile_path(log_path, sizeof(log_path));
  
  /* open our logfile */
  outfile_fd = open(log_path, O_WRONLY | O_CREAT | O_TRUNC, 0666);
  if (outfile_fd < 0) {
    /* NOTE: we can't use dmalloc_message of course so do it the hardway */
    len = loc_snprintf(error_str, sizeof(error_str),
		       "debug-malloc library: could not open '%s'\r\n",
		       log_path);
    (void)write(STDERR, error_str, len);
    /* disable log_path */
    dmalloc_logpath = NULL;
    return;
  }
  
  /*
   * NOTE: this makes it go recursive here, but it will never enter
   * this section of code.
   */
  
  dmalloc_message("Dmalloc version '%s' from '%s'",
		  dmalloc_version, DMALLOC_HOME);
  dmalloc_message("flags = %#x, logfile '%s'", _dmalloc_flags, log_path);
  dmalloc_message("interval = %lu, addr = %#lx, seen # = %ld, limit = %ld",
		  _dmalloc_check_interval, (unsigned long)_dmalloc_address,
		  _dmalloc_address_seen_n, _dmalloc_memory_limit);
#if LOCK_THREADS
  dmalloc_message("threads enabled, lock-on = %d, lock-init = %d",
		  _dmalloc_lock_on, THREAD_INIT_LOCK);
#endif
    
#if LOG_PNT_TIMEVAL
  {
    char	time_buf[64];
    dmalloc_message("starting time = %s",
		     _dmalloc_ptimeval(&_dmalloc_start, time_buf,
				       sizeof(time_buf), 0));
  }
#else
#if HAVE_TIME /* NOT LOG_PNT_TIME */
  {
    char	time_buf[64];
    dmalloc_message("starting time = %s",
		     _dmalloc_ptime(&_dmalloc_start, time_buf,
				    sizeof(time_buf), 0));
  }
#endif
#endif
  
#if HAVE_GETPID
  {
    /* we make it long in case it's big and we hope it will promote if not */
    long	our_pid = getpid();
    
    dmalloc_message("process pid = %ld", our_pid);
  }
#endif
}

/*
 * void _dmalloc_reopen_log
 *
 * DESCRIPTION:
 *
 * Re-open our log file which basically calls close() on the
 * logfile-fd.  If we change the name of the log-file then we will
 * re-open the file.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * None.
 */
void	_dmalloc_reopen_log(void)
{
  /* no need to reopen it if it hasn't been reopened yet */
  if (outfile_fd < 0) {
    return;
  }
  
  if (dmalloc_logpath == NULL) {
    dmalloc_message("Closing logfile to not be reopened");
  }
  else {
    dmalloc_message("Closing logfile to be reopened as '%s'",
		     dmalloc_logpath);
  }
  
  (void)close(outfile_fd);
  outfile_fd = -1;
  /* we don't call open here, we'll let the next message do it */
}

#if LOG_PNT_TIMEVAL
/*
 * char *_dmalloc_ptimeval
 *
 * DESCRIPTION:
 *
 * Print the time into local buffer.
 *
 * RETURNS:
 *
 * Poiner to the buf argument.
 *
 * ARGUMENTS:
 *
 * timeval_p -> Pointer to a time value.
 *
 * buf -> Internal buffer into which we are writing the time.
 *
 * buf_size -> Size of the buffer.
 *
 * elapsed_b -> Set to 1 to dump the elapsed instead of global time.
 */
char	*_dmalloc_ptimeval(const TIMEVAL_TYPE *timeval_p, char *buf,
			   const int buf_size, const int elapsed_b)
{
  unsigned long	hrs, mins, secs, usecs;
  
  secs = timeval_p->tv_sec;
  usecs = timeval_p->tv_usec;
  
  if (elapsed_b) {
    if (usecs >= _dmalloc_start.tv_usec) {
      usecs -= _dmalloc_start.tv_usec;
    }
    else {
      usecs = _dmalloc_start.tv_usec - usecs;
      secs--;
    }
    secs -= _dmalloc_start.tv_sec;
    
    hrs = secs / SECS_IN_HOUR;
    mins = (secs / SECS_IN_MIN) % MINS_IN_HOUR;
    secs %= SECS_IN_MIN;
    
    (void)loc_snprintf(buf, buf_size, "%lu:%02lu:%02lu.%06lu",
		       hrs, mins, secs, usecs);
  }
  else {
    (void)loc_snprintf(buf, buf_size, "%lu.%06lu",
		       secs, usecs);
  }
  
  return buf;
}
#endif

/* NOTE: we do the ifdef this way for fillproto */
#if LOG_PNT_TIMEVAL == 0 && HAVE_TIME
/*
 * char *_dmalloc_ptime
 *
 * DESCRIPTION:
 *
 * Print the time into local buffer.
 *
 * RETURNS:
 *
 * Poiner to the buf argument.
 *
 * ARGUMENTS:
 *
 * time_p -> Pointer to a time value.
 *
 * buf -> Internal buffer into which we are writing the time.
 *
 * buf_size -> Size of the buffer.
 *
 * elapsed_b -> Set to 1 to dump the elapsed instead of global time.
 */
char	*_dmalloc_ptime(const TIME_TYPE *time_p, char *buf, const int buf_size,
			const int elapsed_b)
{
  unsigned long	hrs, mins, secs;
  
  secs = *time_p;
  
  if (elapsed_b) {
    secs -= _dmalloc_start;
    
    hrs = secs / SECS_IN_HOUR;
    mins = (secs / SECS_IN_MIN) % MINS_IN_HOUR;
    secs %= SECS_IN_MIN;
    
    (void)loc_snprintf(buf, buf_size, "%lu:%02lu:%02lu", hrs, mins, secs);
  }
  else {
    (void)loc_snprintf(buf, buf_size, "%lu", secs);
  }
  
  return buf;
}
#endif

/*
 * void _dmalloc_vmessage
 *
 * DESCRIPTION:
 *
 * Message writer with vprintf like arguments which adds a line to the
 * dmalloc logfile.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * format -> Printf-style format statement.
 *
 * args -> Already converted pointer to a stdarg list.
 */
void	_dmalloc_vmessage(const char *format, va_list args)
{
  char	*str_p, *bounds_p;
  int	len;
  
  str_p = message_str;
  bounds_p = str_p + sizeof(message_str);
  
  /* no logpath and no print then no workie */
  if (dmalloc_logpath == NULL
      && ! BIT_IS_SET(_dmalloc_flags, DEBUG_PRINT_MESSAGES)) {
    return;
  }
  
#if HAVE_GETPID && LOG_REOPEN
  if (dmalloc_logpath != NULL) {
    char	*log_p;
    
    /*
     * This static pid will be checked to make sure it doesn't change.
     * We make it long in case it's big and we hope it will promote if
     * not.
     */
    static long		current_pid = -1;
    long		new_pid;
    
    new_pid = getpid();
    if (new_pid != current_pid) {
      /* NOTE: we need to do this _before_ the reopen otherwise we recurse */
      current_pid = new_pid;
      
      /* if the new pid doesn't match the old one then reopen it */
      if (current_pid >= 0) {
	
	/* this only works if there is a %p in the logpath */
	for (log_p = dmalloc_logpath; *log_p != '\0'; log_p++) {
	  if (*log_p == '%' && *(log_p + 1) == 'p') {
	    _dmalloc_reopen_log();
	    break;
	  }
	}
      }
    }
  }
#endif
  
  /* do we need to open the logfile? */
  if (dmalloc_logpath != NULL && outfile_fd < 0) {
    _dmalloc_open_log();
  }
  
#if HAVE_TIME
#if LOG_TIME_NUMBER
  {
    long	now;
    now = time(NULL);
    str_p += loc_snprintf(str_p, bounds_p - str_p, "%ld: ", now);
  }
#endif /* LOG_TIME_NUMBER */
#if HAVE_CTIME
#if LOG_CTIME_STRING
  {
    TIME_TYPE	now;
    now = time(NULL);
    str_p += loc_snprintf(str_p, bounds_p - str_p, "%.24s: ", ctime(&now));
  }
#endif /* LOG_CTIME_STRING */
#endif /* HAVE_CTIME */
#endif /* HAVE_TIME */
  
#if LOG_ITERATION
  /* add the iteration number */
  str_p += loc_snprintf(str_p, bounds_p - str_p, "%lu: ", _dmalloc_iter_c);
#endif
#if LOG_PID && HAVE_GETPID
  {
    /* we make it long in case it's big and we hope it will promote if not */
    long	our_pid = getpid();
    
    /* add the pid to the log file */
    str_p += loc_snprintf(str_p, bounds_p - str_p, "p%ld: ", our_pid);
  }
#endif
  
  /*
   * NOTE: the following code, as well as the function definition
   * above, would need to be altered to conform to non-ANSI-C
   * specifications if necessary.
   */
  
  /* write the format + info into str */
  len = loc_vsnprintf(str_p, bounds_p - str_p, format, args);
  
  /* was it an empty format? */
  if (len == 0) {
    return;
  }
  str_p += len;
  
  /* tack on a '\n' if necessary */
  if (*(str_p - 1) != '\n') {
    *str_p++ = '\n';
    *str_p = '\0';
  }
  len = str_p - message_str;
  
  /* do we need to write the message to the logfile */
  if (dmalloc_logpath != NULL) {
    (void)write(outfile_fd, message_str, len);
  }
  
  /* do we need to print the message? */
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_PRINT_MESSAGES)) {
    (void)write(STDERR, message_str, len);
  }
}

/*
 * void _dmalloc_die
 *
 * DESCRIPTION:
 *
 * Kill the program because of an internal malloc error.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * silent_b -> Set to 1 to not drop log entries.
 */
void	_dmalloc_die(const int silent_b)
{
  char	*stop_str;
  int	len;
  
  if (! silent_b) {
    if (BIT_IS_SET(_dmalloc_flags, DEBUG_ERROR_ABORT)) {
      stop_str = "dumping";
    }
    else {
      stop_str = "halting";
    }
    
    /* print a message that we are going down */
    len = loc_snprintf(error_str, sizeof(error_str),
		       "debug-malloc library: %s program, fatal error\r\n",
		       stop_str);
    (void)write(STDERR, error_str, len);
    if (dmalloc_errno != ERROR_NONE) {
      len = loc_snprintf(error_str, sizeof(error_str),
			 "   Error: %s (err %d)\r\n",
			 dmalloc_strerror(dmalloc_errno), dmalloc_errno);
      (void)write(STDERR, error_str, len);
    }
  }
  
  /*
   * set this in case the following generates a recursive call for
   * some dumb reason
   */
  _dmalloc_aborting_b = 1;
  
  /* do I need to drop core? */
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_ERROR_ABORT)
      || BIT_IS_SET(_dmalloc_flags, DEBUG_ERROR_DUMP)) {
#if USE_ABORT
    abort();
#else
#ifdef KILL_PROCESS
    KILL_PROCESS;
#endif
#endif
  }
  
  /*
   * NOTE: this should not be exit() because fclose will free, etc
   */
  _exit(1);
}

/*
 * void dmalloc_error
 *
 * DESCRIPTION:
 *
 * Handler of error codes.  The caller should have set the errno already
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * func -> Function name for the logs.
 */
void	dmalloc_error(const char *func)
{
  /* do we need to log or print the error? */
  if (dmalloc_logpath != NULL
      || BIT_IS_SET(_dmalloc_flags, DEBUG_PRINT_MESSAGES)) {
    
    /* default str value */
    if (func == NULL) {
      func = "_malloc_error";
    }
    
    /* print the malloc error message */
    dmalloc_message("ERROR: %s: %s (err %d)",
		    func, dmalloc_strerror(dmalloc_errno), dmalloc_errno);
  }
  
  /* do I need to abort? */
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_ERROR_ABORT)) {
    _dmalloc_die(0);
  }
  
#if HAVE_FORK
  /* how about just drop core? */
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_ERROR_DUMP)) {
    if (fork() == 0) {
      _dmalloc_die(1);
    }
  }
#endif
}
