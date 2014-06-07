/*
 * global error codes for chunk allocation problems
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
 * $Id: error_val.h,v 1.35 2005/06/05 05:22:28 gray Exp $
 */

#ifndef __ERROR_VAL_H__
#define __ERROR_VAL_H__

/*
 * Dmalloc error codes.
 *
 * NOTE: these are here instead of error.h because the dmalloc utility
 * needs them as well as the dmalloc library.
 */
#define ERROR_NONE			1	/* no error */
/* 2 is reserved for invalid error */

/* administrative errors */
#define ERROR_BAD_SETUP			10	/* bad setup value */
#define ERROR_IN_TWICE			11	/* in malloc domain twice */
/* 12 unused */
#define ERROR_LOCK_NOT_CONFIG		13	/* thread locking not config */

/* pointer verification errors */
#define ERROR_IS_NULL			20	/* pointer is NULL */
#define ERROR_NOT_IN_HEAP		21	/* pointer is not in heap */
#define ERROR_NOT_FOUND			22	/* pointer not-found */
#define ERROR_IS_FOUND			23	/* found special pointer */
#define ERROR_BAD_FILE			24	/* bad file-name */
#define ERROR_BAD_LINE			25	/* bad line-number */
#define ERROR_UNDER_FENCE		26	/* failed picket fence lower */
#define ERROR_OVER_FENCE		27	/* failed picket fence upper */
#define ERROR_WOULD_OVERWRITE		28	/* would overwrite fence */
/* 29 unused */
#define ERROR_NOT_START_BLOCK		30	/* pointer not to start mem */

/* allocation errors */
#define ERROR_BAD_SIZE			40	/* bad size value */
#define ERROR_TOO_BIG			41	/* allocation too large */
/* 42 unused */
#define ERROR_ALLOC_FAILED		43	/* could not get more space */
/* 44 unused */
#define ERROR_OVER_LIMIT		45	/* over allocation limit */

/* free errors */
#define ERROR_NOT_ON_BLOCK		60	/* not on block boundary */
#define ERROR_ALREADY_FREE		61	/* already in free list */
/* 62-66 unused */
#define ERROR_FREE_OVERWRITTEN		67	/* free space overwritten */

/* administrative errors */
#define ERROR_ADMIN_LIST		70	/* list pnt out of bounds */
/* 71 unused */
#define ERROR_ADDRESS_LIST		72	/* invalid address list */
#define ERROR_SLOT_CORRUPT		73	/* memory slot corruption */

#define INVALID_ERROR		"errno value is not valid"

typedef struct {
  int		es_error;		/* error number */
  char		*es_string;		/* associated string */
} error_str_t;

/* string error codes which apply to error codes in error_val.h */
static	error_str_t	error_list[]
#ifdef __GNUC__
__attribute__ ((unused))
#endif
= {
  { ERROR_NONE,			"no error" },
  
  /* administrative errors */
  { ERROR_BAD_SETUP,		"dmalloc initialization and setup failed" },
  { ERROR_IN_TWICE,		"dmalloc library has gone recursive" },
  { ERROR_LOCK_NOT_CONFIG,  "dmalloc thread locking has not been configured" },
  
  /* pointer verification errors */
  { ERROR_IS_NULL,		"pointer is null" },
  { ERROR_NOT_IN_HEAP,		"pointer is not pointing to heap data space" },
  { ERROR_NOT_FOUND,		"cannot locate pointer in heap" },
  { ERROR_IS_FOUND,		"found pointer the user was looking for" },
  { ERROR_BAD_FILE,		"possibly bad .c filename pointer" },
  { ERROR_BAD_LINE,		"possibly bad .c file line-number" },
  { ERROR_UNDER_FENCE,	       "failed UNDER picket-fence magic-number check"},
  { ERROR_OVER_FENCE,		"failed OVER picket-fence magic-number check"},
  { ERROR_WOULD_OVERWRITE,	"use of pointer would exceed allocation" },
  { ERROR_NOT_START_BLOCK,	"pointer is not to start of memory block" },
  
  /* allocation errors */
  { ERROR_BAD_SIZE,		"invalid allocation size" },
  { ERROR_TOO_BIG,		"largest maximum allocation size exceeded" },
  { ERROR_ALLOC_FAILED,		"could not grow heap by allocating memory" },
  { ERROR_OVER_LIMIT,		"over user specified allocation limit" },
  
  /* free errors */
  { ERROR_NOT_ON_BLOCK,	 	"pointer is not on block boundary" },
  { ERROR_ALREADY_FREE,		"tried to free previously freed pointer" },
  { ERROR_FREE_OVERWRITTEN,	"free space has been overwritten" },
  
  /* administrative errors */
  { ERROR_ADMIN_LIST,		"dmalloc bad admin structure list" },
  { ERROR_ADDRESS_LIST,		"dmalloc internal address list corruption" },
  { ERROR_SLOT_CORRUPT,		"dmalloc internal memory slot corruption" },
  
  { 0 }
};

#endif /* ! __ERROR_VAL_H__ */
