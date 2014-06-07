/*
 * Token attributes for the dmalloc program and _dmalloc_flags
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
 * $Id: debug_tok.h,v 1.41 2004/09/13 05:13:29 gray Exp $
 */

#ifndef __DEBUG_TOK_H__
#define __DEBUG_TOK_H__

#include "dmalloc_loc.h"		/* for BIT_FLAG */

/*
 * special debug codes which detail what debug features are enabled
 * NOTE: need to change debug_tok.h, mallocrc, and malloc.texi if any
 * capabilities are added/removed/updated
 */

/* logging */
#define DEBUG_LOG_STATS		BIT_FLAG(0)	/* generally log statistics */
#define DEBUG_LOG_NONFREE	BIT_FLAG(1)	/* report non-freed pointers */
#define DEBUG_LOG_KNOWN		BIT_FLAG(2)	/* report only known nonfreed*/
#define DEBUG_LOG_TRANS		BIT_FLAG(3)	/* log memory transactions */
/* 4 available - 20001107 */
#define DEBUG_LOG_ADMIN		BIT_FLAG(5)	/* log background admin info */
/* 6 available 20030508 */
/* 7 available - 20001107 */
#define DEBUG_LOG_BAD_SPACE	BIT_FLAG(8)	/* dump space from bad pnt */
#define DEBUG_LOG_NONFREE_SPACE	BIT_FLAG(9)	/* dump space from non-freed */

#define DEBUG_LOG_ELAPSED_TIME	BIT_FLAG(18)	/* log pnt elapsed time info */
#define DEBUG_LOG_CURRENT_TIME	BIT_FLAG(19)	/* log pnt current time info */

/* checking */
#define DEBUG_CHECK_FENCE	BIT_FLAG(10)	/* check fence-post errors  */
#define DEBUG_CHECK_HEAP	BIT_FLAG(11)	/* examine heap adm structs */
/* 12 available - 20030608 */
#define DEBUG_CHECK_BLANK	BIT_FLAG(13)	/* check blank sections */
#define DEBUG_CHECK_FUNCS	BIT_FLAG(14)	/* check functions */
#define DEBUG_CHECK_SHUTDOWN	BIT_FLAG(15)	/* check pointers on shutdown*/

/* misc */
/* 16 available - 20040709 */
#define DEBUG_CATCH_SIGNALS	BIT_FLAG(17)	/* catch HUP, INT, and TERM */
/* 18,19 used above */
#define DEBUG_REALLOC_COPY	BIT_FLAG(20)	/* copy all reallocations */
#define DEBUG_FREE_BLANK	BIT_FLAG(21)	/* write over free'd memory */
#define DEBUG_ERROR_ABORT	BIT_FLAG(22)	/* abort on error else exit */
#define DEBUG_ALLOC_BLANK	BIT_FLAG(23)	/* write over to-be-alloced */
/* 24 available - 20030508 */
#define DEBUG_PRINT_MESSAGES	BIT_FLAG(25)	/* write messages to STDERR */
#define DEBUG_CATCH_NULL	BIT_FLAG(26)	/* quit before return null */
#define DEBUG_NEVER_REUSE	BIT_FLAG(27)	/* never reuse memory */
#define DEBUG_ERROR_FREE_NULL	BIT_FLAG(28)	/* catch free(0) */
/* 29 available - 20011130 */
#define DEBUG_ERROR_DUMP	BIT_FLAG(30)	/* dump core on error */
/* 31 is the high bit and off-limits */

/*
 * structure for mapping the string to the numerical token
 */
typedef struct {
  char		*at_string;		/* attribute string */
  unsigned long	at_value;		/* value for the item */
  char		*at_desc;		/* description string */
} attr_t;

static	attr_t		attributes[]
#ifdef __GNUC__
__attribute__ ((unused))
#endif
= {
  { "none",		0,
      "no functionality" },
  
  { "log-stats",	DEBUG_LOG_STATS,	"log general statistics" },
  { "log-non-free",	DEBUG_LOG_NONFREE,	"log non-freed pointers" },
  { "log-known",	DEBUG_LOG_KNOWN,	"log only known non-freed" },
  { "log-trans",	DEBUG_LOG_TRANS,	"log memory transactions" },
  { "log-admin",	DEBUG_LOG_ADMIN,	"log administrative info" },
  { "log-bad-space",	DEBUG_LOG_BAD_SPACE,	"dump space from bad pnt" },
  { "log-nonfree-space",DEBUG_LOG_NONFREE_SPACE,
    "dump space from non-freed pointers" },
  
  { "log-elapsed-time",	DEBUG_LOG_ELAPSED_TIME,
    "log elapsed-time for allocated pointer" },
  { "log-current-time",	DEBUG_LOG_CURRENT_TIME,
    "log current-time for allocated pointer" },
  
  { "check-fence",	DEBUG_CHECK_FENCE,	"check fence-post errors" },
  { "check-heap",	DEBUG_CHECK_HEAP,	"check heap adm structs" },
  { "check-blank",	DEBUG_CHECK_BLANK,
    "check mem overwritten by alloc-blank, free-blank" },
  { "check-funcs",	DEBUG_CHECK_FUNCS,	"check functions" },
  { "check-shutdown",	DEBUG_CHECK_SHUTDOWN,	"check heap on shutdown" },
  
  { "catch-signals",	DEBUG_CATCH_SIGNALS,
    "shutdown program on SIGHUP, SIGINT, SIGTERM" },
  { "realloc-copy",	DEBUG_REALLOC_COPY,	"copy all re-allocations" },
  { "free-blank",	DEBUG_FREE_BLANK,
    "overwrite freed memory with \\0337 byte (0xdf)" },
  { "error-abort",	DEBUG_ERROR_ABORT,	"abort immediately on error" },
  { "alloc-blank",	DEBUG_ALLOC_BLANK,
    "overwrite allocated memory with \\0332 byte (0xda)" },
  { "print-messages",	DEBUG_PRINT_MESSAGES,	"write messages to stderr" },
  { "catch-null",	DEBUG_CATCH_NULL,      "abort if no memory available"},
  { "never-reuse",	DEBUG_NEVER_REUSE,	"never re-use freed memory" },
  { "error-dump",	DEBUG_ERROR_DUMP,
    "dump core on error, then continue" },
  { "error-free-null",	DEBUG_ERROR_FREE_NULL,
    "generate error when a 0L pointer is freed" },
  
  /*
   * The following are here for backwards compatibility
   */
  
  /* this is the default now and log-known is the opposite */
  { "log-unknown",	0,
    "Disabled -- this is the default now, log-known is opposite" },
  { "log-blocks",	0,
    "Disabled because of new heap organization" },
  /* this is the default and force-linear is opposite */
  { "allow-nonlinear",	0,
    "Disabled -- this is the default now" },
  /* this is the default and error-free-null opposite */
  { "allow-free-null",	0,
    "Disabled -- this is the default now, error-free-null is opposite" },
  { "heap-check-map",	0,
    "Disabled because of new heap organization" },
  { "check-lists",	0,
    "Disabled -- removed with new heap organization" },
  { "force-linear",	0,
    "Disabled -- removed with new mmap usage" },
  
  { NULL }
};

#endif /* ! __DEBUG_TOK_H__ */
