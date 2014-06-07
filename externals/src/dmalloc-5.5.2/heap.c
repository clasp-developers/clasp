/*
 * system specific memory routines
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
 * $Id: heap.c,v 1.69 2005/01/11 18:27:05 gray Exp $
 */

/*
 * These are the system/machine specific routines for allocating space on the
 * heap as well as reporting the current position of the heap.
 */

#if HAVE_UNISTD_H
# include <unistd.h>				/* for write */
#endif
#if HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
#if HAVE_SYS_MMAN_H
#  include <sys/mman.h>				/* for mmap stuff */
#endif

#define DMALLOC_DISABLE

#include "conf.h"
#include "dmalloc.h"

#include "chunk.h"
#include "compat.h"
#include "debug_tok.h"
#include "error.h"
#include "error_val.h"
#include "heap.h"
#include "dmalloc_loc.h"

#define SBRK_ERROR	((char *)-1)		/* sbrk error code */

/* exported variables */
void		*_dmalloc_heap_low = NULL;	/* base of our heap */
void		*_dmalloc_heap_high = NULL;	/* end of our heap */

/****************************** local functions ******************************/

/*
 * static void *heap_extend
 *
 * DESCRIPTION:
 *
 * Get more bytes from the system functions.
 *
 * RETURNS:
 *
 * Success - Valid pointer.
 *
 * Failure - NULL
 *
 * ARGUMENTS:
 *
 * incr -> Number of bytes we need.
 */
static	void	*heap_extend(const int incr)
{
  void	*ret = SBRK_ERROR;
  char	*high;
  
#if INTERNAL_MEMORY_SPACE
  {
    static char	block_o_bytes[INTERNAL_MEMORY_SPACE];
    static char *bounds_p = block_o_bytes + sizeof(block_o_bytes);
    static char *block_p = block_o_bytes;
    
    if (block_p + incr >= bounds_p) {
      ret = SBRK_ERROR;
    }
    else {
      ret = block_p;
      block_p += incr;
    }
  }
#else
#if HAVE_MMAP && USE_MMAP
#if MAP_ANON
  /* if we have and can use mmap, then do so */
  ret = mmap(0L, incr, PROT_READ | PROT_WRITE | PROT_EXEC,
	     MAP_PRIVATE | MAP_ANON, -1 /* no fd */, 0 /* no offset */);
#else
#endif
  if (ret == MAP_FAILED) {
    ret = SBRK_ERROR;
  }
#else
#if HAVE_SBRK
  ret = sbrk(incr);
#endif /* if HAVE_SBRK */
#endif /* if not HAVE_MMAP && USE_MMAP */
#endif /* if not INTERNAL_MEMORY_SPACE */
  
  if (ret == SBRK_ERROR) {
    if (BIT_IS_SET(_dmalloc_flags, DEBUG_CATCH_NULL)) {
      char	str[128];
      int	len;
      len = loc_snprintf(str, sizeof(str),
			 "\r\ndmalloc: critical error: could not extend heap %u more bytes\r\n", incr);
      (void)write(STDERR, str, len);
      _dmalloc_die(0);
    }
    dmalloc_errno = ERROR_ALLOC_FAILED;
    dmalloc_error("heap_extend");
  }
  
  if (_dmalloc_heap_low == NULL || (char *)ret < (char *)_dmalloc_heap_low) {
    _dmalloc_heap_low = ret;
  }
  high = (char *)ret + incr;
  if (high > (char *)_dmalloc_heap_high) {
    _dmalloc_heap_high = high;
  }
  
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_LOG_ADMIN)) {
    dmalloc_message("extended heap space by %d bytes [%#lx, %#lx]",
		    incr, (unsigned long)_dmalloc_heap_low,
		    (unsigned long)_dmalloc_heap_high);
  }
  
  return ret;
}

/**************************** exported functions *****************************/

/*
 * int _heap_startup
 *
 * DESCRIPTION:
 *
 * Initialize heap pointers.
 *
 * RETURNS:
 *
 * Success - 1
 *
 * Failure - 0
 *
 * ARGUMENTS:
 *
 * None.
 */
int	_dmalloc_heap_startup(void)
{
  return 1;
}

/*
 * void *_dmalloc_heap_alloc
 *
 * DESCRIPTION:
 *
 * Function to get memory bytes from the heap.
 *
 * RETURNS:
 *
 * Success - Valid pointer.
 *
 * Failure - NULL
 *
 * ARGUMENTS:
 *
 * size -> Number of bytes we need.
 */
void	*_dmalloc_heap_alloc(const unsigned int size)
{
  void	*heap_new, *heap_diff;
  long	diff;
  
  if (size == 0) {
    dmalloc_errno = ERROR_BAD_SIZE;
    dmalloc_error("_dmalloc_heap_alloc");
    return HEAP_ALLOC_ERROR;
  }
  
  while (1) {
    
    /* extend the heap by our size */
    heap_new = heap_extend(size);
    if (heap_new == SBRK_ERROR) {
      return HEAP_ALLOC_ERROR;
    }
    
    /* calculate bytes needed to align to block boundary */
    diff = (long)heap_new % BLOCK_SIZE;
    if (diff == 0) {
      /* if we are already aligned then we are all set */
      break;
    }
    diff = BLOCK_SIZE - diff;
    
    /* shift the heap a bit to account for non block alignment */
    heap_diff = heap_extend(diff);
    if (heap_diff == SBRK_ERROR) {
      return HEAP_ALLOC_ERROR;
    }
    
    /* if heap-diff went down then our stack grows down */
    if ((char *)heap_diff + diff == (char *)heap_new) {
      heap_new = heap_diff;
      break;
    }
    else if ((char *)heap_new + size == (char *)heap_diff) {
      /* shift up our heap to align with the block */
      heap_new = (char *)heap_new + diff;
      break;
    }
    
    /*
     * We may have a wierd sbrk race condition here.  We hope that we
     * are not just majorly confused which may mean that we sbrk till
     * the cows come home and die from lack of memory.
     */
    
    /* start over again */
  }
  
  return heap_new;
}
