/*
 * user-level memory-allocation routines
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
 * $Id: malloc.c,v 1.189 2007/03/25 18:53:41 gray Exp $
 */

/*
 * This file contains the user-level calls to the memory allocation
 * routines.  It handles a lot of the miscellaneous support garbage for
 * chunk.c which is the real heap manager.
 */

#include <stdio.h>				/* for sprintf sometimes */
#if HAVE_STDLIB_H
# include <stdlib.h>				/* for atexit */
#endif
#if HAVE_STRING_H
# include <string.h>				/* for strlen */
#endif
#if HAVE_UNISTD_H
# include <unistd.h>				/* for write */
#endif

/*
 * cygwin includes
 */
#if HAVE_SYS_CYGWIN_H
# include <sys/cygwin.h>
#endif
#if HAVE_STDARG_H
# include <stdarg.h>
#endif
#if HAVE_W32API_WINDEF_H
# include <w32api/windef.h>
#endif
#if HAVE_W32API_WINBASE_H
# include <w32api/winbase.h>
#endif

#include "conf.h"				/* up here for _INCLUDE */

#if LOG_PNT_TIMEVAL
# ifdef TIMEVAL_INCLUDE
#  include TIMEVAL_INCLUDE
# endif
#else
# if HAVE_TIME /* NOT LOG_PNT_TIME */
#  ifdef TIME_INCLUDE
#   include TIME_INCLUDE
#  endif
# endif
#endif

#if LOCK_THREADS
#if HAVE_PTHREAD_H
#include <pthread.h>
#endif
#if HAVE_PTHREADS_H
#include <pthreads.h>
#endif
#endif

#if SIGNAL_OKAY && HAVE_SIGNAL_H
#include <signal.h>
#endif

#define DMALLOC_DISABLE

#include "dmalloc.h"

#include "chunk.h"
#include "compat.h"
#include "debug_tok.h"
#include "env.h"
#include "error.h"
#include "error_val.h"
#include "heap.h"
#include "dmalloc_loc.h"
#include "malloc_funcs.h"
#include "return.h"

#if LOCK_THREADS
#if IDENT_WORKS
#ident "@(#) $Information: lock-threads is enabled $"
#else
static char *information = "@(#) $Information: lock-threads is enabled $";
#endif
#endif

/* exported variables */

/* internal dmalloc error number for reference purposes only */
int		dmalloc_errno = ERROR_NONE;

/* logfile for dumping dmalloc info, DMALLOC_LOGFILE env var overrides this */
char		*dmalloc_logpath = NULL;

/* local variables */
static	int		enabled_b = 0;		/* have we started yet? */
static	int		in_alloc_b = 0;		/* can't be here twice */
static	int		do_shutdown_b = 0;	/* execute shutdown soon */
static	int		memalign_warn_b = 0;	/* memalign warning printed?*/
static	dmalloc_track_t	tracking_func = NULL;	/* memory trxn tracking func */

/* debug variables */
static	char		*start_file = NULL;	/* file to start at */
static	int		start_line = 0;		/* line to start */
static	unsigned long	start_iter = 0;		/* start after X iterations */
static	unsigned long	start_size = 0;		/* start after X bytes */
static	int		thread_lock_c = 0;	/* lock counter */

/****************************** thread locking *******************************/

#if LOCK_THREADS
#ifdef THREAD_MUTEX_T
/*
 * Define a global variable we use as a lock counter.
 *
 * NOTE: we do not use the PTHREAD_MUTEX_INITIALIZER since this
 * basically delays the pthread_mutex_init call to when
 * pthread_mutex_lock is called for the first time (at least on
 * freebsd).  Since we don't want to go recursive into the pthread
 * library when we go to lock our mutex variable, we want to force the
 * initialization to happen beforehand with a call to
 * pthread_mute_init.
 */
static THREAD_MUTEX_T dmalloc_mutex;
#else
#error We need to have THREAD_MUTEX_T defined by the configure script
#endif
#endif

/*
 * THREADS LOCKING:
 *
 * Because we need to protect for multiple threads making calls into
 * the dmalloc library at the same time, we need to initialize and use
 * a thread mutex variable.  The problem is that most thread libraries
 * uses malloc itself and do not like to go recursive.
 *
 * There are two places where we may have this problem.  One is when
 * we try to use our mutex-lock variable when pthreads is starting up
 * in a shaky state.  The thread library allocates some space, the
 * dmalloc library needs to lock its mutex variable so calls back into
 * the pthread library before it is ready for a call, and a core dump
 * is probably the result.
 *
 * We hopefully solve this by having the dmalloc library not lock
 * during the first couple of memory transactions.  The number is
 * controlled by lock-on dmalloc program environmental setting (set
 * with ``dmalloc -o X'').  You will have to play with the value.  Too
 * many will cause two threads to march into the dmalloc code at the
 * same time generating a ERROR_IN_TWICE error.  Too few and you will
 * get a core dump in the pthreads initialization code.
 *
 * The second place where we might go recursive is when we go to
 * actually initialize our mutex-lock before we can use it.  The
 * THREAD_INIT_LOCK variable (in settings.h) defines that the
 * initialization happens 2 memory transactions before the library
 * begins to use the mutex (lock-on - 2).  It we waited to initialize
 * the variable right before we used it, the pthread library might
 * want to allocate some memory for the variable causing a recursive
 * call and probably a seg-fault -- at least in OSF.  If people need
 * to have this variable also be runtime configurable or would like to
 * present an alternative default, please let me know.
 */

#if LOCK_THREADS
/*
 * mutex lock the malloc library
 */
static	void	lock_thread(void)
{
  /* we only lock if the lock-on counter has reached 0 */
  if (thread_lock_c == 0) {
#if HAVE_PTHREAD_MUTEX_LOCK
    pthread_mutex_lock(&dmalloc_mutex);
#endif
  }
}

/*
 * mutex unlock the malloc library
 */
static	void	unlock_thread(void)
{
  /* if the lock-on counter has not reached 0 then count it down */
  if (thread_lock_c > 0) {
    thread_lock_c--;
    /*
     * As we approach the time when we start mutex locking the
     * library, we need to init the mutex variable.  This sets how
     * many times before we start locking should we init the variable
     * taking in account that the init itself might generate a call
     * into the library.  Ugh.
     */
    if (thread_lock_c == THREAD_INIT_LOCK) {
#if HAVE_PTHREAD_MUTEX_INIT
      /*
       * NOTE: we do not use the PTHREAD_MUTEX_INITIALIZER since this
       * basically delays the pthread_mutex_init call to when
       * pthread_mutex_lock is called for the first time (at least on
       * freebsd).  Since we don't want to go recursive into the
       * pthread library when we go to lock our mutex variable, we
       * want to force the initialization to happen beforehand with a
       * call to pthread_mute_init.
       */
      pthread_mutex_init(&dmalloc_mutex, THREAD_LOCK_INIT_VAL);
#endif
    }
  }
  else if (thread_lock_c == 0) {
#if HAVE_PTHREAD_MUTEX_UNLOCK
    pthread_mutex_unlock(&dmalloc_mutex);
#endif
  }
}
#endif

/****************************** local utilities ******************************/

/*
 * check out a pointer to see if we were looking for it.  this should
 * be re-entrant and it may not return.
 */
static	void	check_pnt(const char *file, const int line, const void *pnt,
			  const char *label)
{
  static unsigned long	addr_c = 0;
  char			where_buf[64];
  
  if (_dmalloc_address == NULL || pnt != _dmalloc_address) {
    return;
  }
  
  addr_c++;
  dmalloc_message("address '%#lx' found in '%s' at pass %ld from '%s'",
		  (unsigned long)pnt, label, addr_c,
		  _dmalloc_chunk_desc_pnt(where_buf, sizeof(where_buf), file,
					  line));
  
  /* NOTE: if address_seen_n == 0 then never quit */
  if (_dmalloc_address_seen_n > 0 && addr_c >= _dmalloc_address_seen_n) {
    dmalloc_errno = ERROR_IS_FOUND;
    dmalloc_error("check_pnt");
  }
}

/*
 * process the values of dmalloc environ variables
 */
static	void	process_environ(const char *option_str)
{
  /*
   * we have a static here so we can store the string without getting
   * into problems
   */
  static char	options[1024];
  
  /* process the options flag */
  if (option_str == NULL) {
    options[0] = '\0';
  }
  else {
    strncpy(options, option_str, sizeof(options));
    options[sizeof(options) - 1] = '\0';
  }
  
  _dmalloc_environ_process(options, &_dmalloc_address,
			   (long *)&_dmalloc_address_seen_n, &_dmalloc_flags,
			   &_dmalloc_check_interval, &_dmalloc_lock_on,
			   &dmalloc_logpath, &start_file, &start_line,
			   &start_iter, &start_size, &_dmalloc_memory_limit);
  thread_lock_c = _dmalloc_lock_on;
  
  /* if we set the start stuff, then check-heap comes on later */
  if (start_iter > 0 || start_size > 0) {
    BIT_CLEAR(_dmalloc_flags, DEBUG_CHECK_HEAP);
  }
  
  /* indicate that we should reopen the logfile if we need to */
  _dmalloc_reopen_log();
  
#if LOCK_THREADS == 0
  /* was thread-lock-on specified but not configured? */
  if (_dmalloc_lock_on > 0) {
    dmalloc_errno = ERROR_LOCK_NOT_CONFIG;
    _dmalloc_die(0);
  }
#endif
}

/************************** startup/shutdown calls ***************************/

#if SIGNAL_OKAY
/*
 * signal catcher
 */
static	RETSIGTYPE	signal_handler(const int sig)
{
  dmalloc_message("caught signal %d", sig);
  /* if we are already inside malloc then do the shutdown later */
  if (in_alloc_b) {
    do_shutdown_b = 1;
  }
  else {
    dmalloc_shutdown();
  }
}
#endif

/*
 * startup the memory-allocation module
 */
static	int	dmalloc_startup(const char *debug_str)
{
  static int	some_up_b = 0;
  const char	*env_str;
#ifdef __CYGWIN__
  char		env_buf[256];
#endif
  
  /* have we started already? */
  if (enabled_b) {
    return 0;
  }
  
  if (! some_up_b) {
    /* set this up here so if an error occurs below, it will not try again */
    some_up_b = 1;
    
#if LOG_PNT_TIMEVAL
    GET_TIMEVAL(_dmalloc_start);
#else
#if HAVE_TIME /* NOT LOG_PNT_TIME */
    _dmalloc_start = time(NULL);
#endif
#endif
    
    /*
     * If we are running under Cygwin then getenv may not be safe.  We
     * try to use the GetEnvironmentVariableA function instead.
     */
#if defined(__CYGWIN__) && HAVE_GETENVIRONMENTVARIABLEA
    /* use this function instead of getenv */
    GetEnvironmentVariableA(OPTIONS_ENVIRON, env_buf, sizeof(env_buf));
    env_str = env_buf;
#else /* ! __CYGWIN__ */
#if GETENV_SAFE
    /* get the options flag */
    if (debug_str == NULL) {
      env_str = getenv(OPTIONS_ENVIRON);
    }
    else {
      env_str = debug_str;
    }
#else
    /* oh, well.  no idea how to get the environmental variables */
    env_str = "";
#endif /* GETENV_SAFE */
#endif /* ! __CYGWIN__ */
    /* process the environmental variable(s) */
    process_environ(env_str);
    
    /*
     * Tune the environment here.  If we have a start-file,
     * start-count, or interval enabled then make sure the check-heap
     * flag is cleared.
     */ 
    if (start_file != NULL
	|| start_iter > 0
	|| start_size > 0
	|| _dmalloc_check_interval > 0) {
      BIT_CLEAR(_dmalloc_flags, DEBUG_CHECK_HEAP);
    }
    
    /* startup heap code */
    if (! _dmalloc_heap_startup()) {
      return 0;
    }
    
    /* startup the chunk lower-level code */
    if (! _dmalloc_chunk_startup()) {
      return 0;
    }
  }
  
#if LOCK_THREADS
  if (thread_lock_c > 0) {
    return 1;
  }
#endif
  
  /*
   * We have initialized all of our code.
   *
   * NOTE: set this up here so if an error occurs below, it will not
   * try again
   */
  enabled_b = 1;
  
  /*
   * NOTE: we may go recursive below here becasue atexit or on_exit
   * may ask for memory to be allocated.  We won't worry about it and
   * will just give it to them.  We hope that atexit didn't start the
   * allocating.  Ugh.
   */
#if AUTO_SHUTDOWN
  /* NOTE: I use the else here in case some dumb systems has both */
#if HAVE_ATEXIT
  (void)atexit(dmalloc_shutdown);
#else
#if HAVE_ON_EXIT
  (void)on_exit(dmalloc_shutdown, NULL);
#endif /* HAVE_ON_EXIT */
#endif /* ! HAVE_ATEXIT */
#endif /* AUTO_SHUTDOWN */
  
#if SIGNAL_OKAY
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_CATCH_SIGNALS)) {
#ifdef SIGNAL1
    (void)signal(SIGNAL1, signal_handler);
#endif
#ifdef SIGNAL2
    (void)signal(SIGNAL2, signal_handler);
#endif
#ifdef SIGNAL3
    (void)signal(SIGNAL3, signal_handler);
#endif
#ifdef SIGNAL4
    (void)signal(SIGNAL4, signal_handler);
#endif
#ifdef SIGNAL5
    (void)signal(SIGNAL5, signal_handler);
#endif
#ifdef SIGNAL6
    (void)signal(SIGNAL6, signal_handler);
#endif
  }
#endif /* SIGNAL_OKAY */
  
  return 1;
}

/*
 * static int dmalloc_in
 *
 * DESCRIPTION:
 *
 * Call to the alloc routines has been made.  Do some initialization,
 * locking, and check some debug variables.
 *
 * RETURNS:
 *
 * Success - 1
 *
 * Failure - 0
 *
 * ARGUMENTS:
 *
 * file -> File-name or return-address of the caller.
 *
 * line -> Line-number of the caller.
 *
 * check_heap_b -> Set to 1 if it is okay to check the heap.  If set
 * to 0 then the caller will check it itself or it is a non-invasive
 * call.
 */
static	int	dmalloc_in(const char *file, const int line,
			   const int check_heap_b)
{
  if (_dmalloc_aborting_b) {
    return 0;
  }
  
  /*
   * NOTE: we need to do this outside of lock to get env vars
   * otherwise our _dmalloc_lock_on variable won't be initialized and
   * the THREAD_LOCK will flip.
   */
  if (! enabled_b) {
    if (! dmalloc_startup(NULL /* no options string */)) {
      return 0;
    }
  }
  
#if LOCK_THREADS
  lock_thread();
#endif
  
  if (in_alloc_b) {
    dmalloc_errno = ERROR_IN_TWICE;
    dmalloc_error("dmalloc_in");
    /* NOTE: dmalloc_error may die already */
    _dmalloc_die(0);
    /*NOTREACHED*/
  }
  
  in_alloc_b = 1;
  
  /* increment our interval */
  _dmalloc_iter_c++;
  
  /* check start file/line specifications */
  if ((! BIT_IS_SET(_dmalloc_flags, DEBUG_CHECK_HEAP))
      && start_file != NULL
      && file != DMALLOC_DEFAULT_FILE
      && line != DMALLOC_DEFAULT_LINE
      && strcmp(start_file, file) == 0
      && (start_line == 0 || start_line == line)) {
    BIT_SET(_dmalloc_flags, DEBUG_CHECK_HEAP);
    /*
     * we disable the start file so we won't check this again and the
     * interval can go on/off
     */
    start_file = NULL;
  }
  
  /* start checking heap after X times */
  else if (start_iter > 0) {
    if (--start_iter == 0) {
      BIT_SET(_dmalloc_flags, DEBUG_CHECK_HEAP);
      /*
       * this is automatically disabled since it goes to 0 so the
       * interval can go on/off
       */
    }
  }
  
  else if (start_size > 0 && start_size >= _dmalloc_alloc_total) {
    BIT_SET(_dmalloc_flags, DEBUG_CHECK_HEAP);
    start_size = 0;
    /* disable this check so the interval can go on/off */
  }
  
  /* checking heap every X times */
  else if (_dmalloc_check_interval > 0) {
    if (_dmalloc_iter_c % _dmalloc_check_interval == 0) {
      BIT_SET(_dmalloc_flags, DEBUG_CHECK_HEAP);
    }
    else { 
      BIT_CLEAR(_dmalloc_flags, DEBUG_CHECK_HEAP);
    }
  }
  
  /* after all that, do we need to check the heap? */
  if (check_heap_b && BIT_IS_SET(_dmalloc_flags, DEBUG_CHECK_HEAP)) {
    (void)_dmalloc_chunk_heap_check();
  }
  
  return 1;
}

/*
 * Going out of the alloc routines back to user space.
 */
static	void	dmalloc_out(void)
{
  in_alloc_b = 0;
  
#if LOCK_THREADS
  unlock_thread();
#endif
  
  if (do_shutdown_b) {
    dmalloc_shutdown();
  }
}

/***************************** exported routines *****************************/

/*
 * void dmalloc_shutdown
 *
 * DESCRIPTION:
 *
 * Shutdown the dmalloc library and provide statistics if necessary.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * None.
 */
void	dmalloc_shutdown(void)
{
  /* NOTE: do not generate errors for IN_TWICE here */
  
  /* if we're already in die mode leave fast and quietly */
  if (_dmalloc_aborting_b) {
    return;
  }
  
  /*
   * Make sure that the log file is open.  We do this here because we
   * might cause an allocation in the open() and don't want to go
   * recursive.
   */
  _dmalloc_open_log();
  
  /* if we've died in dmalloc somewhere then leave fast and quietly */
  if (in_alloc_b) {
    return;
  }
  
#if LOCK_THREADS
  lock_thread();
#endif
  
  /* we do it again in case the lock synced the flag to true now */
  if (in_alloc_b) {
#if LOCK_THREADS
    unlock_thread();
#endif
    return;
  }
  
  in_alloc_b = 1;
  
  /*
   * Check the heap since we are dumping info from it.  We check it
   * when check-blank is enabled do make sure all of the areas have
   * not been overwritten.  Thanks Randell.
   */
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_CHECK_HEAP)
      || BIT_IS_SET(_dmalloc_flags, DEBUG_CHECK_BLANK)
      || BIT_IS_SET(_dmalloc_flags, DEBUG_CHECK_SHUTDOWN)) {
    (void)_dmalloc_chunk_heap_check();
  }
  
  /* dump some statistics to the logfile */
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_LOG_STATS)) {
    _dmalloc_chunk_log_stats();
  }
  
  /* report on non-freed pointers */
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_LOG_NONFREE)) {
    _dmalloc_chunk_log_changed(0, 1, 0,
#if DUMP_UNFREED_SUMMARY_ONLY
		       0
#else
		       1
#endif
		       );
  }
  
#if LOG_PNT_TIMEVAL
  {
    TIMEVAL_TYPE	now;
    char		time_buf1[64], time_buf2[64];
    GET_TIMEVAL(now);
    dmalloc_message("ending time = %s, elapsed since start = %s",
		    _dmalloc_ptimeval(&now, time_buf1, sizeof(time_buf1), 0),
		    _dmalloc_ptimeval(&now, time_buf2, sizeof(time_buf2), 1));
  }
#else
#if HAVE_TIME /* NOT LOG_PNT_TIME */
  {
    TIME_TYPE	now;
    char	time_buf1[64], time_buf2[64];
    now = time(NULL);
    dmalloc_message("ending time = %s, elapsed since start = %s",
		    _dmalloc_ptime(&now, time_buf1, sizeof(time_buf1), 0),
		    _dmalloc_ptime(&now, time_buf2, sizeof(time_buf2), 1));
  }
#endif
#endif
  
  in_alloc_b = 0;
  
#if LOCK_THREADS
  unlock_thread();
#endif
  
  /* NOTE: do not set enabled_b to false here */
}

#if FINI_DMALLOC
/*
 * void __fini_dmalloc
 *
 * DESCRIPTION:
 *
 * Automatic function to close dmalloc supported by some operating
 * systems.  Pretty cool OS/compiler hack.  By default it is not
 * necessary because we use atexit() and on_exit() to register the
 * close functions which are more portable.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * None.
 */
void	__fini_dmalloc(void)
{
  dmalloc_shutdown();
}
#endif

/*
 * DMALLOC_PNT dmalloc_malloc
 *
 * DESCRIPTION:
 *
 * Allocate and return a memory block of a certain size.
 *
 * RETURNS:
 *
 * Success - Valid pointer.
 *
 * Failure - 0L
 *
 * ARGUMENTS:
 *
 * file -> File-name or return-address of the caller.
 *
 * line -> Line-number of the caller.
 *
 * size -> Number of bytes requested.
 *
 * func_id -> Function-id to identify the type of call.  See
 * dmalloc.h.
 *
 * alignment -> To align the new block to a certain number of bytes,
 * set this to a value greater than 0.
 *
 * xalloc_b -> If set to 1 then print an error and exit if we run out
 * of memory.
 */
DMALLOC_PNT	dmalloc_malloc(const char *file, const int line,
			       const DMALLOC_SIZE size, const int func_id,
			       const DMALLOC_SIZE alignment,
			       const int xalloc_b)
{
  void		*new_p;
  DMALLOC_SIZE	align;
  
#if DMALLOC_SIZE_UNSIGNED == 0
  if (size < 0) {
    dmalloc_errno = ERROR_BAD_SIZE;
    dmalloc_error("malloc");
    if (tracking_func != NULL) {
      tracking_func(file, line, func_id, size, alignment, NULL, NULL);
    }
    return MALLOC_ERROR;
  }
#endif
  
  if (! dmalloc_in(file, line, 1)) {
    if (tracking_func != NULL) {
      tracking_func(file, line, func_id, size, alignment, NULL, NULL);
    }
    return MALLOC_ERROR;
  }
  
  if (alignment == 0) {
    if (func_id == DMALLOC_FUNC_VALLOC) {
      align = BLOCK_SIZE;
    }
    else {
      align = 0;
    }
  }
  else if (alignment >= BLOCK_SIZE) {
    align = BLOCK_SIZE;
  }
  else {
    /*
     * NOTE: Currently, there is no support in the library for
     * memalign on less than block boundaries.  It will be non-trivial
     * to support valloc with fence-post checking and the lack of the
     * flag width for dblock allocations.
     */
    if (! memalign_warn_b) {
      dmalloc_message("WARNING: memalign called without library support");
      memalign_warn_b = 1;
    }
    align = 0;
    /* align = alignment */
  }
  
  new_p = _dmalloc_chunk_malloc(file, line, size, func_id, align);
  
  check_pnt(file, line, new_p, "malloc");
  
  dmalloc_out();
  
  if (tracking_func != NULL) {
    tracking_func(file, line, func_id, size, alignment, NULL, new_p);
  }
  
  if (xalloc_b && new_p == NULL) {
    char	mess[1024], desc[128];
    (void)loc_snprintf(mess, sizeof(mess),
		       "Out of memory while allocating %d bytes from '%s'\n",
		       size,
		       _dmalloc_chunk_desc_pnt(desc, sizeof(desc),
					       file, line));
    (void)write(STDERR, mess, strlen(mess));
    _exit(1);
  }
  
  return new_p;
}

/*
 * DMALLOC_PNT dmalloc_realloc
 *
 * DESCRIPTION:
 *
 * Resizes and old pointer to a new number of bytes.
 *
 * RETURNS:
 *
 * Success - Valid pointer.
 *
 * Failure - 0L
 *
 * ARGUMENTS:
 *
 * file -> File-name or return-address of the caller.
 *
 * line -> Line-number of the caller.
 *
 * old_pnt -> Pointer to an existing memory chunk that we are
 * resizing.  If this is NULL then it basically does a malloc.
 *
 * new_size -> New number of bytes requested for the old pointer.
 *
 * func_id -> Function-id to identify the type of call.  See
 * dmalloc.h.
 *
 * xalloc_b -> If set to 1 then print an error and exit if we run out
 * of memory.
 */
DMALLOC_PNT	dmalloc_realloc(const char *file, const int line,
				DMALLOC_PNT old_pnt, DMALLOC_SIZE new_size,
				const int func_id, const int xalloc_b)
{
  void		*new_p;
  
#if DMALLOC_SIZE_UNSIGNED == 0
  if (new_size < 0) {
    dmalloc_errno = ERROR_BAD_SIZE;
    dmalloc_error("realloc");
    if (tracking_func != NULL) {
      tracking_func(file, line, func_id, new_size, 0, old_pnt, NULL);
    }
    return MALLOC_ERROR;
  }
#endif
  
  if (! dmalloc_in(file, line, 1)) {
    if (tracking_func != NULL) {
      tracking_func(file, line, func_id, new_size, 0, old_pnt, NULL);
    }
    return REALLOC_ERROR;
  }
  
  check_pnt(file, line, old_pnt, "realloc-in");
  
#if ALLOW_REALLOC_NULL
  if (old_pnt == NULL) {
    int		new_func_id;
    /* shift the function id to be calloc or malloc */
    if (func_id == DMALLOC_FUNC_RECALLOC) {
      new_func_id = DMALLOC_FUNC_CALLOC;
    }
    else {
      new_func_id = DMALLOC_FUNC_MALLOC;
    }
    new_p = _dmalloc_chunk_malloc(file, line, new_size, new_func_id, 0);
  }
  else
#endif
#if ALLOW_REALLOC_SIZE_ZERO
    if (new_size == 0) {
      /*
       * If realloc(old_pnt, 0) then free(old_pnt).  Thanks to Stefan
       * Froehlich for patiently pointing that the realloc in just
       * about every Unix has this functionality.
       */
      (void)_dmalloc_chunk_free(file, line, old_pnt, func_id);
      new_p = NULL;
    }
    else
#endif
      new_p = _dmalloc_chunk_realloc(file, line, old_pnt, new_size, func_id);
  
  if (new_p != NULL) {
    check_pnt(file, line, new_p, "realloc-out");
  }
  
  dmalloc_out();
  
  if (tracking_func != NULL) {
    tracking_func(file, line, func_id, new_size, 0, old_pnt, new_p);
  }
  
  if (xalloc_b && new_p == NULL) {
    char	mess[1024], desc[128];
    (void)loc_snprintf(mess, sizeof(mess),
		       "Out of memory while reallocating %d bytes from '%s'\n",
		       new_size, _dmalloc_chunk_desc_pnt(desc, sizeof(desc),
							 file, line));
    (void)write(STDERR, mess, strlen(mess));
    _exit(1);
  }
  
  return new_p;
}

/*
 * int dmalloc_free
 *
 * DESCRIPTION:
 *
 * Release a pointer back into the heap.
 *
 * RETURNS:
 *
 * Success - FREE_NOERROR
 *
 * Failure - FREE_ERROR
 *
 * Note: many operating systems define free to return (void) so this
 * return value may be filtered.  Dumb.
 *
 * ARGUMENTS:
 *
 * file -> File-name or return-address of the caller.
 *
 * line -> Line-number of the caller.
 *
 * pnt -> Existing pointer we are freeing.
 *
 * func_id -> Function-id to identify the type of call.  See
 * dmalloc.h.
 */
int	dmalloc_free(const char *file, const int line, DMALLOC_PNT pnt,
		     const int func_id)
{
  int		ret;
  
  if (! dmalloc_in(file, line, 1)) {
    if (tracking_func != NULL) {
      tracking_func(file, line, func_id, 0, 0, pnt, NULL);
    }
    return FREE_ERROR;
  }
  
  check_pnt(file, line, pnt, "free");
  
  ret = _dmalloc_chunk_free(file, line, pnt, func_id);
  
  dmalloc_out();
  
  if (tracking_func != NULL) {
    tracking_func(file, line, DMALLOC_FUNC_FREE, 0, 0, pnt, NULL);
  }
  
  return ret;
}

/*
 * DMALLOC_PNT dmalloc_strndup
 *
 * DESCRIPTION:
 *
 * Allocate and return an allocated block of memory holding a copy of
 * a string of a certain number of characters.
 *
 * RETURNS:
 *
 * Success - Valid pointer.
 *
 * Failure - 0L
 *
 * ARGUMENTS:
 *
 * file -> File-name or return-address of the caller.
 *
 * line -> Line-number of the caller.
 *
 * string -> String we are duplicating.
 *
 * len -> Length of the string we are duplicating.
 *
 * xalloc_b -> If set to 1 then print an error and exit if we run out
 * of memory.
 */
char	*dmalloc_strndup(const char *file, const int line,
			 const char *string, const int len,
			 const int xalloc_b)
{
  DMALLOC_SIZE	size;
  char		*new_string;
  const char	*string_p;
  
  /* check the arguments */
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_CHECK_FUNCS)) {
    /* we check for pointer plus \0 */
    if (! dmalloc_verify_pnt_strsize(file, line, "strdup", string,
				     0 /* not exact */, 1 /* strlen */, len)) {
      dmalloc_message("bad pointer argument found in strdup");
    }
  }
  
  /* so we have to figure out the max length of the string directly */
  if (len < 0) {
    size = strlen(string);
  }
  else {
    for (string_p = string; string_p < string + len; string_p++) {
      if (*string_p == '\0') {
	break;
      }
    }
    size = string_p - string;
  }
  
  /* allocate space for the \0 */
  new_string = dmalloc_malloc(file, line, size + 1, DMALLOC_FUNC_STRDUP,
			      0 /* no alignment */, xalloc_b);
  if (new_string != NULL) {
    strncpy(new_string, string, size);
    new_string[size] = '\0';
  }
  
  return new_string;
}

/*************************** external memory calls ***************************/

/*
 * DMALLOC_PNT malloc
 *
 * DESCRIPTION:
 *
 * Overloading the malloc(3) function.  Allocate and return a memory
 * block of a certain size.
 *
 * RETURNS:
 *
 * Success - Valid pointer.
 *
 * Failure - 0L
 *
 * ARGUMENTS:
 *
 * size -> Number of bytes requested.
 */
#undef malloc
DMALLOC_PNT	malloc(DMALLOC_SIZE size)
{
  char	*file;
  
  GET_RET_ADDR(file);
  return dmalloc_malloc(file, DMALLOC_DEFAULT_LINE, size, DMALLOC_FUNC_MALLOC,
			0 /* no alignment */, 0 /* no xalloc messages */);
}

/*
 * DMALLOC_PNT calloc
 *
 * DESCRIPTION:
 *
 * Overloading the calloc(3) function.  Returns a block of zeroed memory.
 *
 * RETURNS:
 *
 * Success - Valid pointer.
 *
 * Failure - 0L
 *
 * ARGUMENTS:
 *
 * num_elements -> Number of elements being allocated.
 *
 * size -> The number of bytes in each element.
 */
#undef calloc
DMALLOC_PNT	calloc(DMALLOC_SIZE num_elements, DMALLOC_SIZE size)
{
  DMALLOC_SIZE	len = num_elements * size;
  char		*file;
  
  GET_RET_ADDR(file);
  return dmalloc_malloc(file, DMALLOC_DEFAULT_LINE, len, DMALLOC_FUNC_CALLOC,
			0 /* no alignment */, 0 /* no xalloc messages */);
}

/*
 * DMALLOC_PNT realloc
 *
 * DESCRIPTION:
 *
 * Overload of realloc(3).  Resizes and old pointer to a new number of
 * bytes.
 *
 * RETURNS:
 *
 * Success - Valid pointer.
 *
 * Failure - 0L
 *
 * ARGUMENTS:
 *
 * old_pnt -> Pointer to an existing memory chunk that we are
 * resizing.  If this is NULL then it basically does a malloc.
 *
 * new_size -> New number of bytes requested for the old pointer.
 */
#undef realloc
DMALLOC_PNT	realloc(DMALLOC_PNT old_pnt, DMALLOC_SIZE new_size)
{
  char	*file;
  
  GET_RET_ADDR(file);
  return dmalloc_realloc(file, DMALLOC_DEFAULT_LINE, old_pnt, new_size,
			 DMALLOC_FUNC_REALLOC, 0 /* no xalloc messages */);
}

/*
 * DMALLOC_PNT recalloc
 *
 * DESCRIPTION:
 *
 * Overload of recalloc(3) which exists on some systems.  Resizes and
 * old pointer to a new number of bytes.  If we are expanding, then
 * any new bytes will be zeroed.
 *
 * RETURNS:
 *
 * Success - Valid pointer.
 *
 * Failure - 0L
 *
 * ARGUMENTS:
 *
 * old_pnt -> Pointer to an existing memory chunk that we are
 * resizing.
 *
 * new_size -> New number of bytes requested for the old pointer.
 */
#undef recalloc
DMALLOC_PNT	recalloc(DMALLOC_PNT old_pnt, DMALLOC_SIZE new_size)
{
  char	*file;
  
  GET_RET_ADDR(file);
  return dmalloc_realloc(file, DMALLOC_DEFAULT_LINE, old_pnt, new_size,
			 DMALLOC_FUNC_RECALLOC, 0 /* no xalloc messages */);
}

/*
 * DMALLOC_PNT memalign
 *
 * DESCRIPTION:
 *
 * Overloading the memalign(3) function.  Allocate and return a memory
 * block of a certain size which have been aligned to a certain
 * alignment.
 *
 * RETURNS:
 *
 * Success - Valid pointer.
 *
 * Failure - 0L
 *
 * ARGUMENTS:
 *
 * alignment -> Value to which the allocation must be aligned.  This
 * should probably be a multiple of 2 with a maximum value equivalent
 * to the block-size which is often 1k or 4k.
 *
 * size -> Number of bytes requested.
 */
#undef memalign
DMALLOC_PNT	memalign(DMALLOC_SIZE alignment, DMALLOC_SIZE size)
{
  char		*file;
  
  GET_RET_ADDR(file);
  return dmalloc_malloc(file, DMALLOC_DEFAULT_LINE, size,
			DMALLOC_FUNC_MEMALIGN, alignment,
			0 /* no xalloc messages */);
}

/*
 * DMALLOC_PNT valloc
 *
 * DESCRIPTION:
 *
 * Overloading the valloc(3) function.  Allocate and return a memory
 * block of a certain size which have been aligned to page boundaries
 * which are often 1k or 4k.
 *
 * RETURNS:
 *
 * Success - Valid pointer.
 *
 * Failure - 0L
 *
 * ARGUMENTS:
 *
 * size -> Number of bytes requested.
 */
#undef valloc
DMALLOC_PNT	valloc(DMALLOC_SIZE size)
{
  char	*file;
  
  GET_RET_ADDR(file);
  return dmalloc_malloc(file, DMALLOC_DEFAULT_LINE, size, DMALLOC_FUNC_VALLOC,
			BLOCK_SIZE, 0 /* no xalloc messages */);
}

#ifndef DMALLOC_STRDUP_MACRO
/*
 * DMALLOC_PNT strdup
 *
 * DESCRIPTION:
 *
 * Overload of strdup(3).  Allocate and return an allocated block of
 * memory holding a copy of a string.
 *
 * RETURNS:
 *
 * Success - Valid pointer.
 *
 * Failure - 0L
 *
 * ARGUMENTS:
 *
 * string -> String we are duplicating.
 */
#undef strdup
char	*strdup(const char *string)
{
  int	len;
  char	*buf, *file;
  
  GET_RET_ADDR(file);
  
  /* check the arguments */
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_CHECK_FUNCS)) {
    if (! dmalloc_verify_pnt(file, 0 /* no line */, "strdup", string,
			     0 /* not exact */, -1)) {
      dmalloc_message("bad pointer argument found in strdup");
    }
  }
  
  /* len + \0 */
  len = strlen(string) + 1;
  
  buf = dmalloc_malloc(file, DMALLOC_DEFAULT_LINE, len, DMALLOC_FUNC_STRDUP,
		       0 /* no alignment */, 0 /* no xalloc messages */);
  if (buf != NULL) {
    (void)memcpy(buf, string, len);
  }
  
  return buf;
}
#endif

/*
 * DMALLOC_PNT strndup
 *
 * DESCRIPTION:
 *
 * Overload of strndup(3).  Allocate and return an allocated block of
 * memory holding a copy of a string with a maximum length.
 *
 * RETURNS:
 *
 * Success - Valid pointer.
 *
 * Failure - 0L
 *
 * ARGUMENTS:
 *
 * string -> String we are duplicating.
 *
 * len -> Length of the string to duplicate.
 */
#undef strndup
char	*strndup(const char *string, const DMALLOC_SIZE len)
{
  int		size;
  char		*buf, *file;
  const char	*string_p;
  
  GET_RET_ADDR(file);
  
  /* check the arguments */
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_CHECK_FUNCS)) {
    if (! dmalloc_verify_pnt_strsize(file, 0 /* no line */, "strdup", string,
				     0 /* not exact */, 1 /* strlen */,
				     size)) {
      dmalloc_message("bad pointer argument found in strdup");
    }
  }
  
  /* so we have to figure out the max length of the string directly */
  for (string_p = string; string_p < string + len; string_p++) {
    if (*string_p == '\0') {
      break;
    }
  }
  size = string_p - string;
  
  /* at 1 for null */
  buf = dmalloc_malloc(file, DMALLOC_DEFAULT_LINE, size + 1,
		       DMALLOC_FUNC_STRDUP, 0 /* no alignment */,
		       0 /* no xalloc messages */);
  if (buf != NULL) {
    (void)memcpy(buf, string, size);
    buf[size] = '\0';
  }
  
  return buf;
}

/*
 * DMALLOC_FREE_RET free
 *
 * DESCRIPTION:
 *
 * Release a pointer back into the heap.
 *
 * RETURNS:
 *
 * Returns FREE_ERROR, FREE_NOERROR or void depending on whether STDC
 * is defined by your compiler.
 *
 * ARGUMENTS:
 *
 * pnt -> Existing pointer we are freeing.
 */
#undef free
DMALLOC_FREE_RET	free(DMALLOC_PNT pnt)
{
  char	*file;
  int	ret;
  
  GET_RET_ADDR(file);
  ret = dmalloc_free(file, DMALLOC_DEFAULT_LINE, pnt, DMALLOC_FUNC_FREE);
  
#if (defined(__STDC__) && __STDC__ == 1) || defined(__cplusplus) || defined(STDC_HEADERS)
#else
  return ret;
#endif
}

/*
 * DMALLOC_FREE_RET cfree
 *
 * DESCRIPTION:
 *
 * Same as free.
 *
 * RETURNS:
 *
 * Returns FREE_ERROR, FREE_NOERROR or void depending on whether STDC
 * is defined by your compiler.
 *
 * ARGUMENTS:
 *
 * pnt -> Existing pointer we are freeing.
 */
#undef cfree
DMALLOC_FREE_RET	cfree(DMALLOC_PNT pnt)
{
  char	*file;
  int	ret;
  
  GET_RET_ADDR(file);
  ret = dmalloc_free(file, DMALLOC_DEFAULT_LINE, pnt, DMALLOC_FUNC_CFREE);
  
#if (defined(__STDC__) && __STDC__ == 1) || defined(__cplusplus) || defined(STDC_HEADERS)
#else
  return ret;
#endif
}

/******************************* utility calls *******************************/

/*
 * int dmalloc_verify
 *
 * DESCRIPTION:
 *
 * Verify a pointer which has previously been allocated by the
 * library or check the entire heap.
 *
 * RETURNS:
 *
 * Success - MALLOC_VERIFY_NOERROR
 *
 * Failure - MALLOC_VERIFY_ERROR
 *
 * ARGUMENTS:
 *
 * pnt -> Pointer we are verifying.  If 0L then check the entire heap.
 */
int	dmalloc_verify(const DMALLOC_PNT pnt)
{
  int	ret;
  
  if (! dmalloc_in(DMALLOC_DEFAULT_FILE, DMALLOC_DEFAULT_LINE, 0)) {
    return MALLOC_VERIFY_NOERROR;
  }
  
  /* should not check heap here because we will be doing it below */
  
  if (pnt == NULL) {
    ret = _dmalloc_chunk_heap_check();
  }
  else {
    ret = _dmalloc_chunk_pnt_check("dmalloc_verify", pnt,
				   1 /* exact pointer */, 0 /* no strlen */,
				   0 /* no min size */);
  }
  
  dmalloc_out();
  
  if (ret) {
    return MALLOC_VERIFY_NOERROR;
  }
  else {
    return MALLOC_VERIFY_ERROR;
  }
}

/*
 * int malloc_verify
 *
 * DESCRIPTION:
 *
 * Verify a pointer which has previously been allocated by the
 * library.  Same as dmalloc_verify.
 *
 * RETURNS:
 *
 * Success - MALLOC_VERIFY_NOERROR
 *
 * Failure - MALLOC_VERIFY_ERROR
 *
 * ARGUMENTS:
 *
 * pnt -> Pointer we are verifying.  If 0L then check the entire heap.
 */
int	malloc_verify(const DMALLOC_PNT pnt)
{
  return dmalloc_verify(pnt);
}

/*
 * int dmalloc_verify_pnt
 *
 * DESCRIPTION:
 *
 * This function is mainly used by the arg_check.c functions to verify
 * specific pointers.  This can be used by users to provide more fine
 * grained tests on pointers.
 *
 * RETURNS:
 *
 * Success - MALLOC_VERIFY_NOERROR
 *
 * Failure - MALLOC_VERIFY_ERROR
 *
 * ARGUMENTS:
 *
 * file -> File-name or return-address of the caller.  You can use
 * __FILE__ for this argument or 0L for none.
 *
 * line -> Line-number of the caller.  You can use __LINE__ for this
 * argument or 0 for none.
 *
 * func -> Function string which is checking the pointer.  0L if none.
 *
 * pnt -> Pointer we are checking.
 *
 * exact_b -> Set to 1 if this pointer was definitely handed back from
 * a memory allocation.  If set to 0 then this pointer can be inside
 * another allocation or outside the heap altogether.
 *
 * strlen_b -> Set to 1 to make sure that this pointer can handle
 * strlen(pnt) + 1 bytes up to the maximum specified by min_size.  If
 * this is 1 and min_size > 0 then it is in effect a strnlen.
 *
 * min_size -> Make sure that pointer can hold at least that many
 * bytes if inside of the heap.  If 0 then don't check the size.
 */
int	dmalloc_verify_pnt_strsize(const char *file, const int line,
				   const char *func, const void *pnt,
				   const int exact_b, const int strlen_b,
				   const int min_size)
{
  int	ret;
  
  if (! dmalloc_in(file, line, 0)) {
    return MALLOC_VERIFY_NOERROR;
  }
  
  /* call the pnt checking chunk code */
  ret = _dmalloc_chunk_pnt_check(func, pnt, exact_b, strlen_b, min_size);
  dmalloc_out();
  
  if (ret) {
    return MALLOC_VERIFY_NOERROR;
  }
  else {
    return MALLOC_VERIFY_ERROR;
  }
}

/*
 * int dmalloc_verify_pnt
 *
 * DESCRIPTION:
 *
 * This function is mainly used by the arg_check.c functions to verify
 * specific pointers.  This can be used by users to provide more fine
 * grained tests on pointers.
 *
 * RETURNS:
 *
 * Success - MALLOC_VERIFY_NOERROR
 *
 * Failure - MALLOC_VERIFY_ERROR
 *
 * ARGUMENTS:
 *
 * file -> File-name or return-address of the caller.  You can use
 * __FILE__ for this argument or 0L for none.
 *
 * line -> Line-number of the caller.  You can use __LINE__ for this
 * argument or 0 for none.
 *
 * func -> Function string which is checking the pointer.  0L if none.
 *
 * pnt -> Pointer we are checking.
 *
 * exact_b -> Set to 1 if this pointer was definitely handed back from
 * a memory allocation.  If set to 0 then this pointer can be inside
 * another allocation or outside the heap altogether.
 *
 * min_size -> Make sure that pointer can hold at least that many
 * bytes if inside of the heap.  If -1 then make sure it can handle
 * strlen(pnt) + 1 bytes (+1 for the \0).  If 0 then don't check the
 * size.  If you need strnlen functionality with a maximum on the
 * strlen, see dmalloc_verify_pnt_strsize.
 */
int	dmalloc_verify_pnt(const char *file, const int line, const char *func,
			   const void *pnt, const int exact_b,
			   const int min_size)
{
  if (min_size < 0) {
    return dmalloc_verify_pnt_strsize(file, line, func, pnt, exact_b,
				      1 /* strlen */, 0 /* no min-size */);
  } else {
    return dmalloc_verify_pnt_strsize(file, line, func, pnt, exact_b,
				      0 /* no strlen */, min_size);
  }
}

/*
 * unsigned int dmalloc_debug
 *
 * DESCRIPTION:
 *
 * Set the global debug functionality flags.  You can also use
 * dmalloc_debug_setup.
 *
 * Note: you cannot add or remove certain flags such as signal
 * handlers since they are setup at initialization time only.
 *
 * RETURNS:
 *
 * The old debug flag value.
 *
 * ARGUMENTS:
 *
 * flags -> Flag value to set.  Pass in 0 to disable all debugging.
 */
unsigned int	dmalloc_debug(const unsigned int flags)
{
  unsigned int	old_flags;
  
  if (! enabled_b) {
    (void)dmalloc_startup(NULL /* no options string */);
  }
  
  old_flags = _dmalloc_flags;
  
  /* add the new flags */
  _dmalloc_flags = flags;
  
  return old_flags;
}

/*
 * unsigned int dmalloc_debug_current
 *
 * DESCRIPTION:
 *
 * Returns the current debug functionality flags.  This allows you to
 * save a dmalloc library state to be restored later.
 *
 * RETURNS:
 *
 * Current debug flags.
 *
 * ARGUMENTS:
 *
 * None.
 */
unsigned int	dmalloc_debug_current(void)
{
  if (! enabled_b) {
    (void)dmalloc_startup(NULL /* no options string */);
  }
  
  /* should not check the heap here since we are dumping the debug variable */
  return _dmalloc_flags;
}

/*
 * void dmalloc_debug_setup
 *
 * DESCRIPTION:
 *
 * Set the global debugging functionality as an option string.
 * Normally this would be pased in in the DMALLOC_OPTIONS
 * environmental variable.  This is here to override the env or for
 * circumstances where modifying the environment is not possible or
 * does not apply such as servers or cgi-bin programs.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * options_str -> Options string to set the library flags.
 */
void	dmalloc_debug_setup(const char *options_str)
{
  if (! enabled_b) {
    (void)dmalloc_startup(options_str);
    /* if we just started up then we don't have to do anything else */
    return;
  }
  
  /* we need to lock */
  if (! dmalloc_in(NULL /* no file-name */, 0 /* no line-number */,
		   0 /* don't-check-heap */)) {
    return;
  }
  
  process_environ(options_str);
  dmalloc_out();
}

/*
 * int dmalloc_examine
 *
 * DESCRIPTION:
 *
 * Examine a pointer and pass back information on its allocation size
 * as well as the file and line-number where it was allocated.  If the
 * file and line number is not available, then it will pass back the
 * allocation location's return-address if available.
 *
 * RETURNS:
 *
 * Success - DMALLOC_NOERROR
 *
 * Failure - DMALLOC_ERROR
 *
 * ARGUMENTS:
 *
 * pnt -> Pointer we are checking.
 *
 * user_size_p <- Pointer to a DMALLOC_SIZE type variable which, if
 * not NULL, will be set to the size of bytes from the pointer.
 *
 * total_size_p <- Poiner to a DMALLOC_SIZE type variable which, if
 * not NULL, will be set to the total size given for this allocation
 * including administrative overhead.
 *
 * file_p <- Pointer to a character pointer which, if not NULL, will
 * be set to the file where the pointer was allocated.
 *
 * line_p <- Pointer to an unsigned integer which, if not NULL, will
 * be set to the line-number where the pointer was allocated.
 *
 * ret_attr_p <- Pointer to a void pointer, if not NULL, will be set
 * to the return-address where the pointer was allocated.
 *
 * used_mark_p <- Poiner to an unsigned integer which, if not NULL,
 * will be set to the mark of when the pointer was last "used".  This
 * could be when it was allocated, reallocated, or freed.
 *
 * seen_p <- Poiner to an unsigned long which, if not NULL, will be
 * set to the number of times that this pointer has been allocated,
 * realloced, or freed.  NOTE: LOG_PNT_SEEN_COUNT must be set to 1
 * otherwise no seen information is available and it will be set to 0.
 */
int	dmalloc_examine(const DMALLOC_PNT pnt, DMALLOC_SIZE *user_size_p,
			DMALLOC_SIZE *total_size_p, char **file_p,
			unsigned int *line_p, DMALLOC_PNT *ret_attr_p,
			unsigned long *used_mark_p, unsigned long *seen_p)
{
  int		ret;
  unsigned int	user_size_map, tot_size_map;
  unsigned long	*loc_seen_p;
  
  /*
   * NOTE: we use the size maps because we use a unsigned int size
   * type internally but may use some size_t externally.
   */
  
  /* need to check the heap here since we are geting info from it below */
  if (! dmalloc_in(DMALLOC_DEFAULT_FILE, DMALLOC_DEFAULT_LINE, 1)) {
    return DMALLOC_ERROR;
  }
  
  /* NOTE: we do not need the alloc-size info */
  ret = _dmalloc_chunk_read_info(pnt, "dmalloc_examine", &user_size_map,
				 &tot_size_map, file_p, line_p, ret_attr_p,
				 &loc_seen_p, used_mark_p, NULL, NULL);
  
  dmalloc_out();
  
  if (ret) {
    SET_POINTER(user_size_p, user_size_map);
    SET_POINTER(total_size_p, tot_size_map);
    if (loc_seen_p == NULL) {
      SET_POINTER(seen_p, 0);
    } else {
      SET_POINTER(seen_p, *loc_seen_p);
    }
    return DMALLOC_NOERROR;
  }
  else {
    return DMALLOC_ERROR;
  }
}

/*
 * void dmalloc_track
 *
 * DESCRIPTION:
 *
 * Register an allocation tracking function which will be called each
 * time an allocation occurs.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * track_func -> Function to register as the tracking function.  Set
 * to NULL to disable.
 */
void	dmalloc_track(const dmalloc_track_t track_func)
{
  tracking_func = track_func;
}

/*
 * unsigned long dmalloc_mark
 *
 * DESCRIPTION:
 *
 * Return to the caller the current "mark" which can be used later by
 * dmalloc_log_changed to log the changed pointers since this point.
 * Multiple marks can be saved and used.
 *
 * This is also the iteration number and can be logged at the front of
 * each memory transaction in the logfile with the LOG_ITERATION
 * define in settings.h and can be logged with each pointer with the
 * LOG_PNT_ITERATION define in settings.h.
 *
 * RETURNS:
 *
 * Current mark value
 *
 * ARGUMENTS:
 *
 * None.
 */
unsigned long	dmalloc_mark(void)
{
  if (! enabled_b) {
    (void)dmalloc_startup(NULL /* no options string */);
  }
  
  return _dmalloc_iter_c;
}

/*
 * unsigned long dmalloc_memory_allocated
 *
 * DESCRIPTION:
 *
 * Return the total number of bytes allocated by the program so far.
 *
 * RETURNS:
 *
 * Total number of bytes allocated by the program so far.
 *
 * ARGUMENTS:
 *
 * None.
 */
unsigned long	dmalloc_memory_allocated(void)
{
  if (! enabled_b) {
    (void)dmalloc_startup(NULL /* no options string */);
  }
  
  return _dmalloc_alloc_total;
}

/*
 * unsigned int dmalloc_page_size
 *
 * DESCRIPTION:
 *
 * Get the page-size being used by dmalloc.
 *
 * RETURNS:
 *
 * Page size.
 *
 * ARGUMENTS:
 *
 * None.
 */
unsigned int	dmalloc_page_size(void)
{
  if (! enabled_b) {
    (void)dmalloc_startup(NULL /* no options string */);
  }
  
  return BLOCK_SIZE;
}

/*
 * unsigned long dmalloc_count_changed
 *
 * DESCRIPTION:
 *
 * Count the changed memory bytes since a particular mark.
 *
 * RETURNS:
 *
 * Number of bytes since mark.
 *
 * ARGUMENTS:
 *
 * mark -> Sets the point from which to count the changed memory.  You
 * can use dmalloc_mark to get the current mark value which can later
 * be passed in here.  Pass in 0 to report on the unfreed memory since
 * the program started.
 *
 * not_freed_b -> Set to 1 to count the new pointers that are non-freed.
 *
 * free_b -> Set to 1 to count the new pointers that are freed.
 */
unsigned long	dmalloc_count_changed(const unsigned long mark,
				      const int not_freed_b, const int free_b)
{
  unsigned long	mem_count;
  
  if (! dmalloc_in(DMALLOC_DEFAULT_FILE, DMALLOC_DEFAULT_LINE, 1)) {
    return 0;
  }
  
  if (! BIT_IS_SET(_dmalloc_flags, DEBUG_LOG_TRANS)) {
    dmalloc_message("counting the unfreed memory since mark %lu", mark);
  }
  
  mem_count = _dmalloc_chunk_count_changed(mark, not_freed_b, free_b);
  
  dmalloc_out();
  
  return mem_count;
}

/*
 * void dmalloc_log_status
 *
 * DESCRIPTION:
 *
 * Dump dmalloc statistics to logfile.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * None.
 */
void	dmalloc_log_stats(void)
{
  if (! dmalloc_in(DMALLOC_DEFAULT_FILE, DMALLOC_DEFAULT_LINE, 1)) {
    return;
  }
  
  _dmalloc_chunk_log_stats();
  
  dmalloc_out();
}

/*
 * void dmalloc_log_unfreed
 *
 * DESCRIPTION:
 *
 * Dump unfreed-memory info to logfile.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * None.
 */
void	dmalloc_log_unfreed(void)
{
  if (! dmalloc_in(DMALLOC_DEFAULT_FILE, DMALLOC_DEFAULT_LINE, 1)) {
    return;
  }
  
  if (! BIT_IS_SET(_dmalloc_flags, DEBUG_LOG_TRANS)) {
    dmalloc_message("dumping the unfreed pointers");
  }
  
  /*
   * to log the non-free we are interested in the pointers currently
   * being used
   */
  _dmalloc_chunk_log_changed(0, 1, 0,
#if DUMP_UNFREED_SUMMARY_ONLY
			0
#else
			1
#endif
			);
  
  dmalloc_out();
}

/*
 * void dmalloc_log_changed
 *
 * DESCRIPTION:
 *
 * Dump the pointers that have changed since a point in time.
 *
 * RETURNS:
 *
 * mark -> Sets the point to compare against.  You can use
 * dmalloc_mark to get the current mark value which can later be
 * passed in here.  Pass in 0 to log what has changed since the
 * program started.
 *
 * not_freed_b -> Set to 1 to log the new pointers that are non-freed.
 *
 * free_b -> Set to 1 to log the new pointers that are freed.
 *
 * details_b -> Set to 1 to dump the individual pointers that have
 * changed otherwise the summaries will be logged.
 */
void	dmalloc_log_changed(const unsigned long mark, const int not_freed_b,
			    const int free_b, const int details_b)
{
  if (! dmalloc_in(DMALLOC_DEFAULT_FILE, DMALLOC_DEFAULT_LINE, 1)) {
    return;
  }
  _dmalloc_chunk_log_changed(mark, not_freed_b, free_b, details_b);
  
  dmalloc_out();
}

/*
 * void dmalloc_vmessage
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
void	dmalloc_vmessage(const char *format, va_list args)
{
  _dmalloc_vmessage(format, args);
}

/*
 * void dmalloc_message
 *
 * DESCRIPTION:
 *
 * Message writer with printf like arguments which adds a line to the
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
 * ... -> Variable argument list.
 */
void	dmalloc_message(const char *format, ...)
  /* __attribute__ ((format (printf, 1, 2))) */
{
  va_list	args;
  
  va_start(args, format);
  _dmalloc_vmessage(format, args);
  va_end(args);
}

/*
 * void dmalloc_get_stats
 *
 * DESCRIPTION:
 *
 * Return a number of statistics about the current heap.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * heap_low_p <- Pointer to pointer which, if not 0L, will be set to
 * the low address in the heap.
 *
 * heap_high_p <- Pointer to pointer which, if not 0L, will be set to
 * the high address in the heap.
 *
 * total_space_p <- Pointer to an unsigned long which, if not 0L, will
 * be set to the total space managed by the library including user
 * space, administrative space, and overhead.
 *
 * user_space_p <- Pointer to an unsigned long which, if not 0L, will
 * be set to the space given to the user process (allocated and free).
 *
 * current_allocated_p <- Pointer to an unsigned long which, if not
 * 0L, will be set to the current allocated space given to the user
 * process.
 *
 * current_pnt_np <- Pointer to an unsigned long which, if not 0L,
 * will be set to the current number of pointers allocated by the user
 * process.
 *
 * max_allocated_p <- Pointer to an unsigned long which, if not 0L,
 * will be set to the maximum allocated space given to the user
 * process.
 *
 * max_pnt_np <- Pointer to an unsigned long which, if not 0L, will be
 * set to the maximum number of pointers allocated by the user
 * process.
 *
 * max_one_p <- Pointer to an unsigned long which, if not 0L, will be
 * set to the maximum allocated with 1 call by the user process.
 */
void	dmalloc_get_stats(DMALLOC_PNT *heap_low_p,
			  DMALLOC_PNT *heap_high_p,
			  unsigned long *total_space_p,
			  unsigned long *user_space_p,
			  unsigned long *current_allocated_p,
			  unsigned long *current_pnt_np,
			  unsigned long *max_allocated_p,
			  unsigned long *max_pnt_np,
			  unsigned long *max_one_p)
{
  _dmalloc_chunk_get_stats(heap_low_p, heap_high_p, total_space_p,
			   user_space_p, current_allocated_p, current_pnt_np,
			   max_allocated_p, max_pnt_np, max_one_p);
}

/*
 * const char *dmalloc_strerror
 *
 * DESCRIPTION:
 *
 * Convert a dmalloc error code into its string equivalent.
 *
 * RETURNS:
 *
 * Success - String version of the error
 *
 * Failure - The string "unknown error"
 *
 * ARGUMENTS:
 *
 * error_num -> Error number we are converting.
 */
const char	*dmalloc_strerror(const int error_num)
{
  error_str_t	*err_p;
  
  /* should not dmalloc_in here because _dmalloc_error calls this */
  
  for (err_p = error_list; err_p->es_error != 0; err_p++) {
    if (err_p->es_error == error_num) {
      return err_p->es_string;
    }
  }
  
  return INVALID_ERROR;
}
