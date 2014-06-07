/*
 * Local defines for the low level memory routines
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
 * $Id: chunk_loc.h,v 1.70 2003/06/09 23:14:17 gray Exp $
 */

#ifndef __CHUNK_LOC_H__
#define __CHUNK_LOC_H__

#include "conf.h"				/* up here for _INCLUDE */
#include "dmalloc_loc.h"			/* for DMALLOC_SIZE */

/* for thread-id types -- see conf.h */
#if LOG_THREAD_ID
#ifdef THREAD_INCLUDE
#include THREAD_INCLUDE
#endif
#endif

/* for time type -- see settings.h */
#if LOG_PNT_TIMEVAL
# ifdef TIMEVAL_INCLUDE
#  include TIMEVAL_INCLUDE
# endif
#else
# if LOG_PNT_TIME
#  ifdef TIME_INCLUDE
#   include TIME_INCLUDE
#  endif
# endif
#endif

/* log-bad-space info */
#define SPECIAL_CHARS		"\"\"''\\\\n\nr\rt\tb\bf\fa\007"

/*
 * Maximum level in the skip list.  This implies that we can only
 * store 2^32 entries optimally.  Needless to say this is plenty.
 */
#define MAX_SKIP_LEVEL		32

/* memory table settings */
#define MEM_ALLOC_ENTRIES	(MEMORY_TABLE_SIZE * 2)
#define MEM_CHANGED_ENTRIES	(MEMORY_TABLE_SIZE * 2)

/* NOTE: FENCE_BOTTOM_SIZE and FENCE_TOP_SIZE defined in settings.h */
#define FENCE_OVERHEAD_SIZE	(FENCE_BOTTOM_SIZE + FENCE_TOP_SIZE)
#define FENCE_MAGIC_BOTTOM	0xC0C0AB1B
#define FENCE_MAGIC_TOP		0xFACADE69
/* smallest allocated block */
#define CHUNK_SMALLEST_BLOCK	\
	(FENCE_BOTTOM_SIZE + DEFAULT_SMALLEST_ALLOCATION)

/* flags associated with the skip_alloc_t type's sa_flags field */
#define ALLOC_FLAG_USER		BIT_FLAG(0)	/* slot is user allocated */
#define ALLOC_FLAG_FREE		BIT_FLAG(1)	/* slot is free */
#define ALLOC_FLAG_EXTERN	BIT_FLAG(2)	/* slot allocated externally */
#define ALLOC_FLAG_ADMIN	BIT_FLAG(3)	/* administrative space */
#define ALLOC_FLAG_BLANK	BIT_FLAG(4)	/* slot has been blanked */
#define ALLOC_FLAG_FENCE	BIT_FLAG(5)	/* slot is fence posted */
#define ALLOC_FLAG_VALLOC	BIT_FLAG(6)	/* slot is block aligned */

/*
 * Below defines an allocation structure either on the free or used
 * list.  It tracks allocations that fit in partial, one, or many
 * basic-blocks.  It stores some optional fields for recording
 * information about the pointer.
 */
typedef struct skip_alloc_st {
  
  unsigned char		sa_flags;	/* what it is */
  
  /* some small data types up front to save on space */
  unsigned char		sa_level_n;	/* how tall our node is */
  unsigned short	sa_line;	/* line where it was allocated */
  
  unsigned int		sa_user_size;	/* size requested by user (wo fence) */
  unsigned int		sa_total_size;	/* total size of the block */
  
  void			*sa_mem;	/* pointer to the memory in question */
  const char		*sa_file;	/* .c filename where allocated */
  unsigned long		sa_use_iter;	/* when last ``used'' */
  
#if LOG_PNT_SEEN_COUNT
  unsigned long		sa_seen_c;	/* times pointer was seen */
#endif
#if LOG_PNT_ITERATION
  unsigned long		sa_iteration;	/* interation when pointer alloced */
#endif
#if LOG_PNT_TIMEVAL
  TIMEVAL_TYPE 		sa_timeval;	/* time when pointer alloced */
#else
#if LOG_PNT_TIME
  TIME_TYPE		sa_time;	/* time when pointer alloced */
#endif
#endif
#if LOG_PNT_THREAD_ID
  THREAD_TYPE		sa_thread_id;	/* thread id which allocaed pnt */
#endif
  
  /*
   * Array of next pointers.  This may extend past the end of the
   * function if we allocate for space larger than the structure.
   */
  struct skip_alloc_st	*sa_next_p[1];
  
} skip_alloc_t;

/*
 * This macro helps us determine how much memory we need to store to
 * hold all of the next pointers in the skip-list entry.  So if we are
 * at level 0 then this will have no extra next pointers since there
 * already is one inside of skip_alloc_t.
 */
#define SKIP_SLOT_SIZE(next_n)	\
	(sizeof(skip_alloc_t) + sizeof(skip_alloc_t *) * (next_n))

/* entry block magic numbers */
#define ENTRY_BLOCK_MAGIC1	0xEBEB1111	/* for the eb_magic1 field */
#define ENTRY_BLOCK_MAGIC2	0xEBEB2222	/* for the eb_magic2 field */
#define ENTRY_BLOCK_MAGIC3	0xEBEB3333	/* written at end of eb block*/

/*
 * The following structure is written at the front of a skip-list
 * entry administrative block.  
 */
typedef struct entry_block_st {
  unsigned int		eb_magic1;	/* magic number */
  unsigned int		eb_level_n;	/* the levels which are stored here */
  struct entry_block_st	*eb_next_p;	/* pointer to next block */
  unsigned int		eb_magic2;	/* magic number */
  
  skip_alloc_t		eb_first_slot;	/* first slot in the block */
  
  /*
   * the rest are after this one but we don't really know the size
   * because it is based on the skip-level.
   */
  
  /*
   * At the end of the block is the MAGIC3 value but we can't define
   * it in a structure.
   */
} entry_block_t;

/*
 * The following structure is used to figure out a number of bits of
 * information about a user allocation.
 */
typedef struct {
  int		pi_fence_b;		/* fence-posts are on for pointer */
  int		pi_valloc_b;		/* pointer is valloc-aligned */
  int		pi_blanked_b;		/* pointer was blanked */
  void		*pi_alloc_start;	/* pnt to start of allocation */
  void		*pi_fence_bottom;	/* pnt to the bottom fence area */
  void		*pi_user_start;		/* pnt to start of user allocation */
  void		*pi_user_bounds;	/* pnt past end of user allocation */
  void		*pi_fence_top;		/* pnt to the top fence area */
  void		*pi_upper_bounds;	/* pnt to highest available user area*/
  void		*pi_alloc_bounds;	/* pnt past end of total allocation */
} pnt_info_t;

#endif /* ! __CHUNK_LOC_H__ */
