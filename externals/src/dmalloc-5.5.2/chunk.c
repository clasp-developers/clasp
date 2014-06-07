/*
 * Memory chunk low-level allocation routines
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
 * $Id: chunk.c,v 1.217 2007/03/25 18:53:41 gray Exp $
 */

/*
 * This file contains algorithm level routine for the heap.  They handle the
 * manipulation and administration of chunks of memory.
 */

#include <ctype.h>

#if HAVE_STRING_H
# include <string.h>
#endif
#if HAVE_STDLIB_H
# include <stdlib.h>
#endif

#define DMALLOC_DISABLE

#include "conf.h"

#if LOG_PNT_TIMEVAL
#ifdef TIMEVAL_INCLUDE
# include TIMEVAL_INCLUDE
#endif
#else
# if LOG_PNT_TIME
#  ifdef TIME_INCLUDE
#   include TIME_INCLUDE
#  endif
# endif
#endif

#include "dmalloc.h"

#include "chunk.h"
#include "chunk_loc.h"
#include "compat.h"
#include "debug_tok.h"
#include "dmalloc_loc.h"
#include "dmalloc_rand.h"
#include "dmalloc_tab.h"
#include "error.h"
#include "error_val.h"
#include "heap.h"

/*
 * Library Copyright and URL information for ident and what programs
 */
#if IDENT_WORKS
#ident "@(#) $Copyright: Dmalloc package Copyright 2007 by Gray Watson $"
#ident "@(#) $URL: Source for dmalloc available from http://dmalloc.com/ $"
#else
static	char	*copyright =
  "@(#) $Copyright: Dmalloc package Copyright 2007 by Gray Watson $";
static	char	*source_url =
  "@(#) $URL: Source for dmalloc available from http://dmalloc.com/ $";
#endif

#if LOCK_THREADS
#if IDENT_WORKS
#ident "@(#) $Information: lock-threads is enabled $"
#else
static char *information = "@(#) $Information: lock-threads is enabled $";
#endif
#endif

/*
 * exported variables
 */
/* limit in how much memory we are allowed to allocate */
unsigned long		_dmalloc_memory_limit = 0;

/* total number of bytes that the heap has allocated */
unsigned long		_dmalloc_alloc_total = 0;

/*
 * local variables
 */

/*
 * Skip list of our free list sorted by size in bytes.  Bit of a hack
 * here.  Basically we cannot do a alloc for the structure and we'd
 * like it to be static storage so we allocate an array of them to
 * make sure we have enough forward pointers, when all we need is
 * SKIP_SLOT_SIZE(MAX_SKIP_LEVEL + 1) bytes.
 */
static	skip_alloc_t	skip_free_alloc[MAX_SKIP_LEVEL /* read note ^^ */];
static	skip_alloc_t	*skip_free_list = skip_free_alloc;

/* skip list of all of our allocated blocks sorted by address */
static	skip_alloc_t	skip_address_alloc[MAX_SKIP_LEVEL /* read note ^^ */];
static	skip_alloc_t	*skip_address_list = skip_address_alloc;

/* update slots which we use to update the skip lists */
static	skip_alloc_t	skip_update[MAX_SKIP_LEVEL /* read note ^^ */];

/* linked list of slots of various sizes */
static	skip_alloc_t	*entry_free_list[MAX_SKIP_LEVEL];
/* linked list of blocks of the sizes */
static	entry_block_t	*entry_blocks[MAX_SKIP_LEVEL];
/* linked list of freed blocks on hold waiting for the FREED_POINTER_DELAY */
static	skip_alloc_t	*free_wait_list_head = NULL;
static	skip_alloc_t	*free_wait_list_tail = NULL;

/* administrative structures */
static	char		fence_bottom[FENCE_BOTTOM_SIZE];
static	char		fence_top[FENCE_TOP_SIZE];
static	int		bit_sizes[BASIC_BLOCK]; /* number bits for div-blocks*/

/* memory tables */
static	mem_table_t	mem_table_alloc[MEM_ALLOC_ENTRIES];
static	int		mem_table_alloc_c = 0;
static	mem_table_t	mem_table_changed[MEM_CHANGED_ENTRIES];
static	int		mem_table_changed_c = 0;

/* memory stats */
static	unsigned long	alloc_current = 0;	/* current memory usage */
static	unsigned long	alloc_maximum = 0;	/* maximum memory usage  */
static	unsigned long	alloc_cur_given = 0;	/* current mem given */
static	unsigned long	alloc_max_given = 0;	/* maximum mem given  */
static	unsigned long	alloc_one_max = 0;	/* maximum at once */
static	unsigned long	free_space_bytes = 0;	/* count the free bytes */

/* pointer stats */
static	unsigned long	alloc_cur_pnts = 0;	/* current pointers */
static	unsigned long	alloc_max_pnts = 0;	/* maximum pointers */
static	unsigned long	alloc_tot_pnts = 0;	/* total pointers */

/* admin counts */
static	unsigned long	heap_check_c = 0;	/* count of heap-checks */
static	unsigned long	user_block_c = 0;	/* count of blocks */
static	unsigned long	admin_block_c = 0;	/* count of admin blocks */

/* alloc counts */
static	unsigned long	func_malloc_c = 0;	/* count the mallocs */
static	unsigned long	func_calloc_c = 0;	/* # callocs, done in alloc */
static	unsigned long	func_realloc_c = 0;	/* count the reallocs */
static	unsigned long	func_recalloc_c = 0;	/* count the reallocs */
static	unsigned long	func_memalign_c = 0;	/* count the memaligns */
static	unsigned long	func_valloc_c = 0;	/* count the veallocs */
static	unsigned long	func_new_c = 0;		/* count the news */
static	unsigned long	func_free_c = 0;	/* count the frees */
static	unsigned long	func_delete_c = 0;	/* count the deletes */

/**************************** skip list routines *****************************/

/*
 * static int random_level
 *
 * DESCRIPTION:
 *
 * Return a random level to be associated with a new free-list entry.
 *
 * RETURNS:
 *
 * Random level from 0 to max_level - 1.
 *
 * ARGUMENTS:
 *
 * max_level -> Maximum level of the free-list.
 */
static	int	random_level(const int max_level)
{
  int	level_c;
  
  for (level_c = 0; level_c < max_level; level_c++) {
    /*
     * Basically we count the number of times that the random number
     * generator returns an odd number in a row.  On average this
     * should return 0 1/2 the time, 1 1/4 of the time, 2 1/8 of a
     * time, and N 1/(2^(N - 1)) of the time.  This is what we want.
     * We could test for this in the configure scripts.
     *
     * Since many machines return random numbers which aren't that
     * random, there may be better ways of doing this.  In the past I
     * had (_dmalloc_rand() % 10000 >= 5000) or something but I'd
     * rather not have the % overhead here.
     */
    if (_dmalloc_rand() & 1) {
      break;
    }
  }
  
  return level_c;
}

/*
 * static skip_alloc_t *find_address
 *
 * DESCRIPTION:
 *
 * Look for a specific address in the skip list.  If it exist then a
 * pointer to the matching slot is returned otherwise NULL.  Either
 * way, the links that were traversed to get there are set in the
 * update slot which has the maximum number of levels.
 *
 * RETURNS:
 *
 * Success - Pointer to the slot which matches the block-num and size
 * pair.
 *
 * Failure - NULL and this will not set dmalloc_errno
 *
 * ARGUMENTS:
 *
 * address -> Address we are looking for.
 *
 * free_b -> Look on the free list otherwise look on the used list.
 *
 * exact_b -> Set to 1 to find the exact pointer.  If 0 then the
 * address could be inside a block.
 *
 * update_p -> Pointer to the skip_alloc entry we are using to hold
 * the update pointers.
 */
static	skip_alloc_t	*find_address(const void *address, const int free_b,
				      const int exact_b,
				      skip_alloc_t *update_p)
{
  int		level_c;
  skip_alloc_t 	*slot_p, *found_p = NULL, *next_p;
  
  /* skip_address_max_level */
  level_c = MAX_SKIP_LEVEL - 1;
  if (free_b) {
    slot_p = skip_free_list;
  }
  else {
    slot_p = skip_address_list;
  }
  
  /* traverse list to smallest entry */
  while (1) {
    
    /* next on we are looking for */
    next_p = slot_p->sa_next_p[level_c];
    
    /*
     * sort by address
     */
    
    /* are we are at the end of a row? */
    if (next_p == NULL) {
      /* just go down a level */
    }
    else if (next_p == found_p
	     || (char *)next_p->sa_mem > (char *)address) {
      /* just go down a level */
    }
    else if ((char *)next_p->sa_mem == (char *)address) {
      /* found it and go down a level */
      found_p = next_p;
    }
    /*
     * (char *)next_p->sa_mem < (char *)address
     */
    else if ((! exact_b)
	     && ((char *)next_p->sa_mem + next_p->sa_total_size >
		 (char *)address)) {
      /*
       * if we are doing loose searches and this block contains this
       * pointer then we have a match
       */
      found_p = next_p;
    }
    else {
      /* next slot is less, go right */
      slot_p = next_p;
      continue;
    }
    
    /* we are lowering the level */
    
    update_p->sa_next_p[level_c] = slot_p;
    if (level_c == 0) {
      break;
    }
    level_c--;
  }
  
  return found_p;
}

/*
 * static skip_alloc_t *find_free_size
 *
 * DESCRIPTION:
 *
 * Look for a specific size in the free skip list.  If it exist then a
 * pointer to the matching slot is returned otherwise NULL.  Either
 * way, the links that were traversed to get there are set in the
 * update slot which has the maximum number of levels.
 *
 * RETURNS:
 *
 * Success - Pointer to the slot which matches the size pair.
 *
 * Failure - NULL
 *
 * ARGUMENTS:
 *
 * address -> Address we are looking for.
 *
 * update_p -> Pointer to the skip_alloc entry we are using to hold
 * the update pointers.
 */
static	skip_alloc_t	*find_free_size(const unsigned int size,
					skip_alloc_t *update_p)
{
  int		level_c, cmp;
  skip_alloc_t 	*slot_p, *found_p = NULL, *next_p;
  
  /* skip_free_max_level */
  level_c = MAX_SKIP_LEVEL - 1;
  slot_p = skip_free_list;
  
  /* traverse list to smallest entry */
  while (1) {
    
    /* next on we are looking for */
    next_p = slot_p->sa_next_p[level_c];
    
    /* are we are at the end of a row? */
    if (next_p == NULL
	|| next_p == found_p) {
      /* just go down a level */
    }
    else {
      cmp = next_p->sa_total_size - size;
      if (cmp < 0) {
	/* next slot is less, go right */
	slot_p = next_p;
	continue;
      }
      else if (cmp == 0) {
	/*
	 * we found a match but it may not be the first slot with this
	 * size and we want the first match
	 */
	found_p = next_p;
      }
    }
    
    /* we are lowering the level */
    
    update_p->sa_next_p[level_c] = slot_p;
    if (level_c == 0) {
      break;
    }
    level_c--;
  }
  
  /* space should be free */
  if (found_p != NULL && (! BIT_IS_SET(found_p->sa_flags, ALLOC_FLAG_FREE))) {
    /* sanity check */
    dmalloc_errno = ERROR_ADDRESS_LIST;
    dmalloc_error("find_free_size");
    return NULL;
  }
  
  return found_p;
}

/*
 * static int insert_slot
 *
 * DESCRIPTION:
 *
 * Insert an address entry into a skip list.
 *
 * RETURNS:
 *
 * Success - 1
 *
 * Failure - 0
 *
 * ARGUMENTS:
 *
 * slot_p <-> Slot that we are inserting into the skip list.
 *
 * free_b -> Insert a free address in the free-size list otherwise it
 * will go into the used address list.
 */
static	int	insert_slot(skip_alloc_t *slot_p, const int free_b)
{
  skip_alloc_t	*adjust_p, *update_p;
  int		level_c;
  
  update_p = skip_update;
  
  if (free_b) {
    (void)find_free_size(slot_p->sa_total_size, update_p);
    /*
     * NOTE: we can get a new_p because there might be other blocks of
     * the same size which we will be inserting before.
     */
  }
  else if (find_address(slot_p->sa_mem, 0 /* used list */, 1 /* exact */,
			update_p) != NULL) {
    /*
     * Sanity check.  We should not have found it since that means
     * that someone has the same size and block-num.
     */
    dmalloc_errno = ERROR_ADDRESS_LIST;
    dmalloc_error("insert_slot");
    return 0;
  }
  
  /* update the block skip list */
  for (level_c = 0; level_c <= slot_p->sa_level_n; level_c++) {
    /*
     * We are inserting our new slot after each of the slots in the
     * update array.  So for each level, we get the slot we are
     * adjusting, we take it's next pointers and set them in the new
     * slot, and we point its next pointers to the new slot.
     */
    adjust_p = update_p->sa_next_p[level_c];
    slot_p->sa_next_p[level_c] = adjust_p->sa_next_p[level_c];
    adjust_p->sa_next_p[level_c] = slot_p;
  }
  
  return 1;
}

/*
 * static int alloc_slots
 *
 * DESCRIPTION:
 *
 * Allocate a block of new slots of a certain size and add them to the
 * free list.  If there are none in the linked list then we will
 * allocate a block of the size.
 *
 * RETURNS:
 *
 * Success - Valid pointer to a single block that was allocated for
 * the slots.
 *
 * Failure - NULL
 *
 * ARGUMENTS:
 *
 * level_n -> Number of the level we are looking for.  Set to 0 to
 * have it be chosen at random.
 */
static	void	*alloc_slots(const int level_n)
{
  skip_alloc_t	*new_p;
  entry_block_t	*block_p;
  unsigned int	*magic3_p, magic3;
  int		size, new_c;
  
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_LOG_ADMIN)) {
    dmalloc_message("need a block of slots for level %d", level_n);
  }
  
  /* we need to allocate a new block of the slots of this level */
  block_p = _dmalloc_heap_alloc(BLOCK_SIZE);
  if (block_p == NULL) {
    /*
     * Sanity check.  Out of heap memory.  Error code set in
     * _dmalloc_heap_alloc().
     */
    return NULL;
  }
  memset(block_p, 0, BLOCK_SIZE);
  admin_block_c++;
  
  /* intialize the block structure */
  block_p->eb_magic1 = ENTRY_BLOCK_MAGIC1;
  block_p->eb_level_n = level_n;
  block_p->eb_magic2 = ENTRY_BLOCK_MAGIC2;
  
  /* add the block on the entry block linked list */
  block_p->eb_next_p = entry_blocks[level_n];
  entry_blocks[level_n] = block_p;
  
  /* put the magic3 at the end of the block */
  magic3_p = (unsigned int *)((char *)block_p + BLOCK_SIZE -
			      sizeof(*magic3_p));
  magic3 = ENTRY_BLOCK_MAGIC3;
  memcpy(magic3_p, &magic3, sizeof(*magic3_p));
  
  /* get the size of the slot */
  size = SKIP_SLOT_SIZE(level_n);
  
  /* add in all of the unused slots to the linked list */
  new_c = 1;
  for (new_p = &block_p->eb_first_slot;
       (char *)new_p + size < (char *)magic3_p;
       new_p = (skip_alloc_t *)((char *)new_p + size)) {
    new_p->sa_level_n = level_n;
    new_p->sa_next_p[0] = entry_free_list[level_n];
    entry_free_list[level_n] = new_p;
    new_c++;
  }
  
  /* extern pointer information set in _dmalloc_heap_alloc */
  return block_p;
}

/*
 * static int remove_slot
 *
 * DESCRIPTION:
 *
 * Remove a slot from the skip list.
 *
 * RETURNS:
 *
 * Success - 1
 *
 * Failure - 0
 *
 * ARGUMENTS:
 *
 * delete_p -> Pointer to the block we are deleting from the list.
 *
 * update_p -> Pointer to the skip_alloc entry we are using to hold
 * the update pointers.
 */
static	int	remove_slot(skip_alloc_t *delete_p, skip_alloc_t *update_p)
{
  skip_alloc_t	*adjust_p;
  int		level_c;
  
  /* update the block skip list */
  for (level_c = 0; level_c <= MAX_SKIP_LEVEL; level_c++) {
    
    /*
     * The update node holds pointers to the slots which are pointing
     * to the one we want since we need to update those pointers
     * ahead.
     */
    adjust_p = update_p->sa_next_p[level_c];
    
    /*
     * If the pointer in question is not pointing to the deleted slot
     * then the deleted slot is shorter than this level and we are
     * done.  This is guaranteed if we have a proper skip list.
     */
    if (adjust_p->sa_next_p[level_c] != delete_p) {
      break;
    }
    
    /*
     * We are deleting a slot after each of the slots in the update
     * array.  So for each level, we get the slot we are adjusting, we
     * set it's next pointers to the next pointers at the same level
     * from the deleted slot.
     */
    adjust_p->sa_next_p[level_c] = delete_p->sa_next_p[level_c];
  }
  
  /*
   * Sanity check here, we should always have at least 1 pointer to
   * the found node that we are deleting.
   */
  if (level_c == 0) {
    dmalloc_errno = ERROR_ADDRESS_LIST;
    dmalloc_error("remove_slot");
    return 0;
  }
  
  return 1;
}

/*
 * static skip_alloc_t *get_slot
 *
 * DESCRIPTION:
 *
 * Get a new slot of a certain size.  This calls alloc_slot and then
 * does a whole bunch of things if alloc_slots generates the need for
 * two new slots.  Jumping through hoops to get this right.
 *
 * RETURNS:
 *
 * Success - Valid skip-alloc pointer.
 *
 * Failure - NULL
 *
 * ARGUMENTS:
 *
 * None.
 */
static	skip_alloc_t	*get_slot(void)
{
  skip_alloc_t	*new_p;
  int		level_n, slot_size;
  void		*admin_mem;
  
  /* generate the level for our new slot */
  level_n = random_level(MAX_SKIP_LEVEL);
  slot_size = SKIP_SLOT_SIZE(level_n);
  
  /* get an extry from the free list */
  new_p = entry_free_list[level_n];
  if (new_p != NULL) {
    /* shift the linked list over */
    entry_free_list[level_n] = new_p->sa_next_p[0];
    /* zero our slot entry */
    memset(new_p, 0, slot_size);
    new_p->sa_level_n = level_n;
    return new_p;
  }
  
  /*
   * Okay, this is a little wild.  Holding on?
   *
   * So we are trying to get a slot of a certain size to store
   * something in a skip list.  We didn't have any on the free-list so
   * now we will allocate a block.  We allocate a block of memory to
   * hold the slots meaning that we may need 1 new slot to account for
   * the admin and external memory in addition to the 1 requested.
   *
   * To do it right, would take a recursive call to get_slot which I
   * am not willing to do so we will have 2 blocks in a row which have
   * the same height.  This is less than efficient but oh well.
   */
  
  /* add in all of the unused slots to the linked list */
  admin_mem = alloc_slots(level_n);
  if (admin_mem == NULL) {
    /* Sanity check.  Error code set in alloc_slots(). */
    return NULL;
  }
  
  /* get one for the admin memory */
  new_p = entry_free_list[level_n];
  if (new_p == NULL) {
    /*
     * Sanity check. We should have created a whole bunch of
     * addresses.
     */
    dmalloc_errno = ERROR_ADDRESS_LIST;
    dmalloc_error("get_slot");
    return NULL;
  }
  entry_free_list[level_n] = new_p->sa_next_p[0];
  memset(new_p, 0, slot_size);
  new_p->sa_flags = ALLOC_FLAG_ADMIN;
  new_p->sa_mem = admin_mem;
  new_p->sa_total_size = BLOCK_SIZE;
  new_p->sa_level_n = level_n;
  
  /* now put it in the used list */
  if (! insert_slot(new_p, 0 /* used list */)) {
    /* Sanity check.  error code set in insert_slot(). */
    return NULL;
  }
  
  /* now get one for the user */
  new_p = entry_free_list[level_n];
  if (new_p == NULL) {
    /*
     * Sanity check.  We should have created a whole bunch of
     * addresses.
     */
    dmalloc_errno = ERROR_ADDRESS_LIST;
    dmalloc_error("get_slot");
    return NULL;
  }
  entry_free_list[level_n] = new_p->sa_next_p[0];
  memset(new_p, 0, slot_size);
  new_p->sa_level_n = level_n;
  
  /* level_np set up top */
  return new_p;
}

/*
 * static skip_alloc_t *insert_address
 *
 * DESCRIPTION:
 *
 * Insert an address entry into a skip list.
 *
 * RETURNS:
 *
 * Success - Valid slot pointer.
 *
 * Failure - NULL
 *
 * ARGUMENTS:
 *
 * address -> Address we are inserting into the address list.
 *
 * free_b -> Insert a free address in the free-size list otherwise it
 * will go into the used address list.
 *
 * tot_size -> Total size of the chunk that we are inserting into the
 * list.
 */
static	skip_alloc_t	*insert_address(void *address, const int free_b,
					const unsigned int tot_size)
{
  skip_alloc_t	*new_p;
  
  /* get a new entry */
  new_p = get_slot();
  if (new_p == NULL) {
    /* error code set in get_slot */
    return NULL;
  }
  if (free_b) {
    new_p->sa_flags = ALLOC_FLAG_FREE;
  }
  else {
    new_p->sa_flags = ALLOC_FLAG_USER;
  }
  new_p->sa_mem = address;
  new_p->sa_total_size = tot_size;
  
  /* now try and insert the slot into the skip-list */
  if (! insert_slot(new_p, free_b)) {
    /* Sanity check.  error code set in insert_slot(). */
    return NULL;
  }
  
  return new_p;
}

/******************************* misc routines *******************************/

/*
 * static int expand_chars
 *
 * DESCRIPTION:
 *
 * Copies a buffer into a output buffer while translates
 * non-printables into %03o octal values.  If it can, it will also
 * translate certain \ characters (\r, \n, etc.) into \\%c.  The
 * routine is useful for printing out binary values.
 *
 * Note: It does _not_ add a \0 at the end of the output buffer.
 *
 * RETURNS:
 *
 * Returns the number of characters added to the output buffer.
 *
 * ARGUMENTS:
 *
 * buf - the buffer to convert.
 *
 * buf_size - size of the buffer.  If < 0 then it will expand till it
 * sees a \0 character.
 *
 * out - destination buffer for the convertion.
 *
 * out_size - size of the output buffer.
 */
static	int	expand_chars(const void *buf, const int buf_size,
			     char *out, const int out_size)
{
  const unsigned char	*buf_p, *spec_p;
  char	 		*out_p = out, *bounds_p;
  
  /* setup our max pointer */
  bounds_p = out + out_size;
  
  /* run through the input buffer, counting the characters as we go */
  for (buf_p = (const unsigned char *)buf;
       buf_p < (const unsigned char *)buf + buf_size;
       buf_p++) {
    
    /* search for special characters */
    for (spec_p = (unsigned char *)SPECIAL_CHARS + 1;
	 *(spec_p - 1) != '\0';
	 spec_p += 2) {
      if (*spec_p == *buf_p) {
	break;
      }
    }
    
    /* did we find one? */
    if (*(spec_p - 1) != '\0') {
      if (out_p + 2 >= bounds_p) {
	break;
      }
      out_p += loc_snprintf(out_p, bounds_p - out_p, "\\%c", *(spec_p - 1));
      continue;
    }
    
    /* print out any 7-bit printable characters */
    if (*buf_p < 128 && isprint(*buf_p)) {
      if (out_p + 1 >= bounds_p) {
	break;
      }
      *out_p = *(char *)buf_p;
      out_p += 1;
    }
    else {
      if (out_p + 4 >= bounds_p) {
	break;
      }
      out_p += loc_snprintf(out_p, bounds_p - out_p, "\\%03o", *buf_p);
    }
  }
  /* try to punch the null if we have space in case the %.*s doesn't work */
  if (out_p < bounds_p) {
    *out_p = '\0';
  }
  
  return out_p - out;
}

/*
 * static void get_pnt_info
 *
 * DESCRIPTION:
 *
 * With a slot, set a number of pointers to places within the block.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * slot_p -> Pointer to a slot structure that we are getting info on.
 *
 * info_p <-> Pointer to an info structure that we are filling with
 * information.
 */
static	void	get_pnt_info(const skip_alloc_t *slot_p, pnt_info_t *info_p)
{
  info_p->pi_fence_b = BIT_IS_SET(slot_p->sa_flags, ALLOC_FLAG_FENCE);
  info_p->pi_valloc_b = BIT_IS_SET(slot_p->sa_flags, ALLOC_FLAG_VALLOC);
  info_p->pi_blanked_b = BIT_IS_SET(slot_p->sa_flags, ALLOC_FLAG_BLANK);
  
  info_p->pi_alloc_start = slot_p->sa_mem;
  
  if (info_p->pi_fence_b) {
    if (info_p->pi_valloc_b) {
      info_p->pi_user_start = (char *)info_p->pi_alloc_start + BLOCK_SIZE;
      info_p->pi_fence_bottom = (char *)info_p->pi_user_start -
	FENCE_BOTTOM_SIZE;
    }
    else {
      info_p->pi_fence_bottom = info_p->pi_alloc_start;
      info_p->pi_user_start = (char *)info_p->pi_alloc_start +
	FENCE_BOTTOM_SIZE;
    }
  }
  else {
    info_p->pi_fence_bottom = NULL;
    info_p->pi_user_start = info_p->pi_alloc_start;
  }
  
  info_p->pi_user_bounds = (char *)info_p->pi_user_start +
    slot_p->sa_user_size;
  
  info_p->pi_alloc_bounds = (char *)slot_p->sa_mem + slot_p->sa_total_size;
  
  if (info_p->pi_fence_b) {
    info_p->pi_fence_top = info_p->pi_user_bounds;
    info_p->pi_upper_bounds = (char *)info_p->pi_alloc_bounds - FENCE_TOP_SIZE;
  }
  else {
    info_p->pi_fence_top = NULL;
    info_p->pi_upper_bounds = info_p->pi_alloc_bounds;
  }
}

/*
 * static char *display_pnt
 *
 * DESCRIPTION:
 *
 * Write into a buffer a discription of a pointer.
 *
 * RETURNS:
 *
 * Pointer to buffer 1st argument.
 *
 * ARGUMENTS:
 *
 * user_pnt -> Pointer that we are displaying.
 *
 * alloc_p -> Pointer to the skip slot which we are displaying.
 *
 * buf <-> Passed in buffer which will be filled with a description of
 * the pointer.
 *
 * buf_size -> Size of the buffer in bytes.
 */
static	char	*display_pnt(const void *user_pnt, const skip_alloc_t *alloc_p,
			     char *buf, const int buf_size)
{
  char	*buf_p, *bounds_p;
  int	elapsed_b;
  
  buf_p = buf;
  bounds_p = buf_p + buf_size;
  
  buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "%#lx",
			(unsigned long)user_pnt);
  
#if LOG_PNT_SEEN_COUNT
  buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "|s%lu", alloc_p->sa_seen_c);
#endif
  
#if LOG_PNT_ITERATION
  buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "|i%lu",
			alloc_p->sa_iteration);
#endif
  
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_LOG_ELAPSED_TIME)) {
    elapsed_b = 1;
  }
  else {
    elapsed_b = 0;
  }
  if (elapsed_b || BIT_IS_SET(_dmalloc_flags, DEBUG_LOG_CURRENT_TIME)) {
#if LOG_PNT_TIMEVAL
    {
      char	time_buf[64];
      buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "|w%s",
			    _dmalloc_ptimeval(&alloc_p->sa_timeval, time_buf,
					      sizeof(time_buf), elapsed_b));
    }
#else
#if LOG_PNT_TIME
    {
      char	time_buf[64];
      buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "|w%s",
			    _dmalloc_ptime(&alloc_p->sa_time, time_buf,
					   sizeof(time_buf), elapsed_b));
    }
#endif
#endif
  }
  
#if LOG_PNT_THREAD_ID
  {
    char	thread_id[256];
    
    buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "|t");
    THREAD_ID_TO_STRING(thread_id, sizeof(thread_id), alloc_p->sa_thread_id);
    buf_p += loc_snprintf(buf_p, bounds_p - buf_p, "%s", thread_id);
  }
#endif
  
  return buf;
}

/*
 * static void log_error_info
 *
 * DESCRIPTION:
 *
 * Logging information about a pointer, usually during an error
 * condition.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * now_file -> File from where we generated the error.
 *
 * now_line -> Line number from where we generated the error.
 *
 * user_pnt -> Pointer in question.  This can be 0L then it will use
 * the slot_p memory pointer.
 *
 * slot_p -> Pointer to the slot associated with the user_pnt or 0L.
 *
 * reason -> Reason string why something happened.
 *
 * where -> What routine is calling log_error_info.  For instance
 * malloc or chunk_check.
 */
static	void	log_error_info(const char *now_file,
			       const unsigned int now_line,
			       const void *user_pnt,
			       const skip_alloc_t *slot_p,
			       const char *reason, const char *where)
{
  static int	dump_bottom_b = 0, dump_top_b = 0;
  char		out[(DUMP_SPACE + FENCE_BOTTOM_SIZE + FENCE_TOP_SIZE) * 4];
  char		where_buf[MAX_FILE_LENGTH + 64];
  char		where_buf2[MAX_FILE_LENGTH + 64];
  const char	*prev_file;
  const void	*dump_pnt = user_pnt;
  const void	*start_user;
  unsigned int	prev_line, user_size;
  skip_alloc_t	*other_p;
  pnt_info_t	pnt_info;
  int		out_len, dump_size, offset;
  
  if (slot_p == NULL) {
    prev_file = NULL;
    prev_line = 0;
    user_size = 0;
    start_user = user_pnt;
  }
  else {
    prev_file = slot_p->sa_file;
    prev_line = slot_p->sa_line;
    user_size = slot_p->sa_user_size;
    if (user_pnt == NULL) {
      get_pnt_info(slot_p, &pnt_info);
      start_user = pnt_info.pi_user_start;
    }
    else {
      start_user = user_pnt;
    }
  }
  
  /* get a proper reason string */
  if (reason != NULL) {
    dmalloc_message("  error details: %s", reason);
  }
  
  /* dump the pointer information */
  if (start_user == NULL) {
    dmalloc_message("  from '%s' prev access '%s'",
		    _dmalloc_chunk_desc_pnt(where_buf, sizeof(where_buf),
					    now_file, now_line),
		    _dmalloc_chunk_desc_pnt(where_buf2, sizeof(where_buf2),
					    prev_file, prev_line));
  }
  else {
    dmalloc_message("  pointer '%#lx' from '%s' prev access '%s'",
		    (unsigned long)start_user,
		    _dmalloc_chunk_desc_pnt(where_buf, sizeof(where_buf),
					    now_file, now_line),
		    _dmalloc_chunk_desc_pnt(where_buf2, sizeof(where_buf2),
					    prev_file, prev_line));
  }
  
  /*
   * If we aren't logging bad space or we didn't error with an
   * overwrite error then don't log the bad bytes.
   */
  if ((! BIT_IS_SET(_dmalloc_flags, DEBUG_LOG_BAD_SPACE))
      || (dmalloc_errno != ERROR_UNDER_FENCE
	  && dmalloc_errno != ERROR_OVER_FENCE
	  && dmalloc_errno != ERROR_FREE_OVERWRITTEN)) {
    /* we call the error function after writing more info to the logfile */
    dmalloc_error(where);
    return;
  }
  
  /* NOTE: display memory like this has the potential for generating a core */
  if (dmalloc_errno == ERROR_UNDER_FENCE) {
    /* NOTE: only dump out the proper fence-post area once */
    if (! dump_bottom_b) {
      out_len = expand_chars(fence_bottom, FENCE_BOTTOM_SIZE, out,
			     sizeof(out));
      dmalloc_message("  dump of proper fence-bottom bytes: '%.*s'",
		      out_len, out);
      dump_bottom_b = 1;
    }
    offset = -FENCE_BOTTOM_SIZE;
    dump_size = DUMP_SPACE + FENCE_BOTTOM_SIZE;
    if (dump_size > user_size + FENCE_OVERHEAD_SIZE) {
      dump_size = user_size + FENCE_OVERHEAD_SIZE;
    }
  }
  else if (dmalloc_errno == ERROR_OVER_FENCE
	   && user_size > 0) {
    /* NOTE: only dump out the proper fence-post area once */
    if (! dump_top_b) {
      out_len = expand_chars(fence_top, FENCE_TOP_SIZE, out, sizeof(out));
      dmalloc_message("  dump of proper fence-top bytes: '%.*s'",
		      out_len, out);
      dump_top_b = 1;
    }
    /*
     * The size includes the bottom fence post area.  We want it to
     * align with the start of the top fence post area.
     */
    if (DUMP_SPACE > user_size + FENCE_OVERHEAD_SIZE) {
      dump_size = user_size + FENCE_OVERHEAD_SIZE;
      offset = -FENCE_BOTTOM_SIZE;
    }
    else {
      dump_size = DUMP_SPACE;
      /* we will go backwards possibly up to FENCE_BOTTOM_SIZE offset */
      offset = user_size + FENCE_TOP_SIZE - DUMP_SPACE;
    }
  }
  else {
    dump_size = DUMP_SPACE;
    offset = 0;
    if (user_size > 0 && dump_size > user_size) {
      dump_size = user_size;
    }
  }
  
  dump_pnt = (char *)start_user + offset;
  if (IS_IN_HEAP(dump_pnt)) {
    out_len = expand_chars(dump_pnt, dump_size, out, sizeof(out));
    dmalloc_message("  dump of '%#lx'%+d: '%.*s'",
		    (unsigned long)start_user, offset, out_len, out);
  }
  else {
    dmalloc_message("  dump of '%#lx'%+d failed: not in heap",
		    (unsigned long)start_user, offset);
  }
  
  /* find the previous pointer in case it ran over */
  if (dmalloc_errno == ERROR_UNDER_FENCE && start_user != NULL) {
    other_p = find_address((char *)start_user - FENCE_BOTTOM_SIZE - 1,
			   0 /* used list */, 1 /* not exact pointer */,
			   skip_update);
    if (other_p != NULL) {
      dmalloc_message("  prev pointer '%#lx' (size %u) may have run over from '%s'",
		      (unsigned long)other_p->sa_mem, other_p->sa_user_size,
		      _dmalloc_chunk_desc_pnt(where_buf, sizeof(where_buf),
					      other_p->sa_file,
					      other_p->sa_line));
    }
  }
  /* find the next pointer in case it ran under */
  else if (dmalloc_errno == ERROR_OVER_FENCE
	   && start_user != NULL
	   && slot_p != NULL) {
    other_p = find_address((char *)slot_p->sa_mem + slot_p->sa_total_size,
			   0 /* used list */, 1 /* not exact pointer */,
			   skip_update);
    if (other_p != NULL) {
      dmalloc_message("  next pointer '%#lx' (size %u) may have run under from '%s'",
		      (unsigned long)other_p->sa_mem, other_p->sa_user_size,
		      _dmalloc_chunk_desc_pnt(where_buf, sizeof(where_buf),
					      other_p->sa_file,
					      other_p->sa_line));
    }
  }
  
  /* we call the error function after writing more info to the logfile */
  dmalloc_error(where);
}

/*
 * static int fence_read
 *
 * DESCRIPTION
 *
 * Check a pointer for fence-post magic numbers.
 *
 * RETURNS:
 *
 * Success - 1 if the fence posts are good.
 *
 * Failure - 0 if they are not.
 *
 * ARGUMENTS:
 *
 * info_p -> Pointer information that we are checking.
 */
static	int	fence_read(const pnt_info_t *info_p)
{
  /* check magic numbers in bottom of allocation block */
  if (memcmp(fence_bottom, info_p->pi_fence_bottom, FENCE_BOTTOM_SIZE) != 0) {
    dmalloc_errno = ERROR_UNDER_FENCE;
    return 0;
  }
  
  /* check numbers at top of allocation block */
  if (memcmp(fence_top, info_p->pi_fence_top, FENCE_TOP_SIZE) != 0) {
    dmalloc_errno = ERROR_OVER_FENCE;
    return 0;
  }
  
  return 1;
}

/*
 * static void clear_alloc
 *
 * DESCRIPTION
 *
 * Setup allocations by writing fence post and doing any necessary
 * clearing of memory.
 *
 * RETURNS:
 *
 * Success - 1 if the fence posts are good.
 *
 * Failure - 0 if they are not.
 *
 * ARGUMENTS:
 *
 * slot_p <-> Slot we are clearing.
 *
 * info_p -> Pointer to information about the allocation.
 *
 * old_size -> If there was an old-size that we have copied into the
 * new pointer then set this.  If 0 then it will clear the entire
 * allocation.
 *
 * func_id -> ID of the function which is doing the allocation.  Used
 * to determine if we should 0 memory for [re]calloc.
 */
static	void	clear_alloc(skip_alloc_t *slot_p, pnt_info_t *info_p,
			    const unsigned int old_size, const int func_id)
{
  char	*start_p;
  int	num;
  
  /*
   * NOTE: The alloc blank flag is set so we blank a slot when it is
   * allocated.  It used to be that the allocated spaces were blanked
   * and the free spaces of the allocated chunk were blanked only if
   * the FREE_BLANK flag was enabled.  Wrong!
   */
  
  /*
   * Set our slot blank flag if the flags are set now.  This will
   * carry over with a realloc.
   */
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_ALLOC_BLANK)
      || BIT_IS_SET(_dmalloc_flags, DEBUG_CHECK_BLANK)) {
    BIT_SET(slot_p->sa_flags, ALLOC_FLAG_BLANK);
  }
  
  /*
   * If we have a fence post protected valloc then there is almost a
   * full block at the front what is "free".  Set it with blank chars.
   */
  if (info_p->pi_fence_b) {
    num = (char *)info_p->pi_fence_bottom - (char *)info_p->pi_alloc_start;
    /* alloc-blank NOT free-blank */
    if (num > 0 && BIT_IS_SET(slot_p->sa_flags, ALLOC_FLAG_BLANK)) {
      memset(info_p->pi_alloc_start, ALLOC_BLANK_CHAR, num);
    }
  }
  
  /*
   * If we are allocating or extending memory, write in our alloc
   * chars.
   */
  start_p = (char *)info_p->pi_user_start + old_size;
  
  num = (char *)info_p->pi_user_bounds - start_p;
  if (num > 0) {
    if (func_id == DMALLOC_FUNC_CALLOC || func_id == DMALLOC_FUNC_RECALLOC) {
      memset(start_p, 0, num);
    }
    else if (BIT_IS_SET(slot_p->sa_flags, ALLOC_FLAG_BLANK)) {
      memset(start_p, ALLOC_BLANK_CHAR, num);
    }
  }
  
  /* write in fence-post info */
  if (info_p->pi_fence_b) {
    memcpy(info_p->pi_fence_bottom, fence_bottom, FENCE_BOTTOM_SIZE);
    memcpy(info_p->pi_fence_top, fence_top, FENCE_TOP_SIZE);
  }
  
  /*
   * Now clear the rest of the block above any fence post space with
   * free characters.
   *
   * NOTE alloc-blank NOT free-blank
   */
  if (BIT_IS_SET(slot_p->sa_flags, ALLOC_FLAG_BLANK)) {
    
    if (info_p->pi_fence_b) {
      start_p = (char *)info_p->pi_fence_top + FENCE_TOP_SIZE;
    }
    else {
      start_p = info_p->pi_user_bounds;
    }
    
    num = (char *)info_p->pi_alloc_bounds - start_p;
    if (num > 0) {
      memset(start_p, ALLOC_BLANK_CHAR, num);
    }
  }
}

/************************** administration functions *************************/

/*
 * static int create_divided_chunks
 *
 * DESCRIPTION:
 *
 * Get a divided-block from the free list or heap allocation.
 *
 * RETURNS:
 *
 * Success - 1
 *
 * Failure - 0
 *
 * ARGUMENTS:
 *
 * div_size -> Size of the divided block that we are allocating.
 */
static	int	create_divided_chunks(const unsigned int div_size)
{
  void		*mem, *bounds_p;
  
  /* allocate a 1 block chunk that we will cut up into pieces */
  mem = _dmalloc_heap_alloc(BLOCK_SIZE);
  if (mem == HEAP_ALLOC_ERROR) {
    /* error code set in _dmalloc_heap_alloc */
    return 0;
  }
  user_block_c++;
  
  /*
   * now run through the block and add the the locations to the
   * free-list
   */
  
  /* end of the block */
  bounds_p = (char *)mem + BLOCK_SIZE - div_size;
  
  for (; mem <= bounds_p; mem = (char *)mem + div_size) {
    /* insert the rest of the blocks into the free-list */
    if (insert_address(mem, 1 /* free list */, div_size) == NULL) {
      /* error set in insert_address */
      return 0;
    }
    free_space_bytes += div_size;
  }
  
  return 1;
}

/*
 * static skip_alloc_t *use_free_memory
 *
 * DESCRIPTION:
 *
 * Find a free memory chunk and remove it from the free list and put
 * it on the used list if available.
 *
 * RETURNS:
 *
 * Success - Valid slot pointer
 *
 * Failure - NULL
 *
 * ARGUMENTS:
 *
 * size -> Size of the block that we are looking for.
 *
 * update_p -> Pointer to the skip_alloc entry we are using to hold
 * the update pointers.
 */
static	skip_alloc_t	*use_free_memory(const unsigned int size,
					 skip_alloc_t *update_p)
{
  skip_alloc_t	*slot_p;
  
#if FREED_POINTER_DELAY
  /*
   * check the free wait list to see if any of the waiting pointers
   * need to be moved off and inserted into the free list
   */
  for (slot_p = free_wait_list_head; slot_p != NULL; ) {
    skip_alloc_t	*next_p;
    
    /* we are done if we find a pointer delay in the future */
    if (slot_p->sa_use_iter + FREED_POINTER_DELAY > _dmalloc_iter_c) {
      break;
    }
    
    /* put slot on free list */
    next_p = slot_p->sa_next_p[0];
    if (! insert_slot(slot_p, 1 /* free list */)) {
      /* error dumped in insert_slot */
      return NULL;
    }
    
    /* adjust our linked list */
    slot_p = next_p;
    free_wait_list_head = slot_p;
    if (slot_p == NULL) {
      free_wait_list_tail = NULL;
    }
  }
#endif
  
  /* find a free block which matches the size */ 
  slot_p = find_free_size(size, update_p);
  if (slot_p == NULL) {
    return NULL;
  }
  
  /* sanity check */
  if (slot_p->sa_total_size != size) {
    dmalloc_errno = ERROR_ADDRESS_LIST;
    dmalloc_error("use_free_memory");
    return NULL;
  }
  
  /* remove from free list */
  if (! remove_slot(slot_p, update_p)) {
    /* error reported in remove_slot */
    return NULL;
  }
  
  /* set to user allocated space */
  slot_p->sa_flags = ALLOC_FLAG_USER;
  
  /* insert it into our address list */
  if (! insert_slot(slot_p, 0 /* used list */)) {
    /* error set in insert_slot */
    return NULL;
  }
  
  free_space_bytes -= slot_p->sa_total_size;
  
  return slot_p;
}

/*
 * static skip_alloc_t *get_divided_memory
 *
 * DESCRIPTION:
 *
 * Get a divided memory block from the free list or heap allocation.
 *
 * RETURNS:
 *
 * Success - Valid skip slot pointer.
 *
 * Failure - NULL
 *
 * ARGUMENTS:
 *
 * size -> Size of the block that we are allocating.
 */
static	skip_alloc_t	*get_divided_memory(const unsigned int size)
{
  skip_alloc_t	*slot_p;
  unsigned int	need_size;
  int		*bits_p;
  
  for (bits_p = bit_sizes;; bits_p++) {
    if (*bits_p >= size) {
      break;
    }
  }
  need_size = *bits_p;
  
  /* find a free block which matches the size */ 
  slot_p = use_free_memory(need_size, skip_update);
  if (slot_p != NULL) {
    return slot_p;
  }
  
  /* need to get more slots */
  if (! create_divided_chunks(need_size)) {
    /* errors dumped in  create_divided_chunks */
    return NULL;
  }
  
  /* now we ask again for the free memory */
  slot_p = use_free_memory(need_size, skip_update);
  if (slot_p == NULL) {
    /* huh?  This isn't right. */
    dmalloc_errno = ERROR_ADDRESS_LIST;
    dmalloc_error("get_divided_memory");
    return NULL;
  }
  
  return slot_p;
}

/*
 * static skip_alloc_t *get_memory
 *
 * DESCRIPTION:
 *
 * Get a block from the free list or heap allocation.
 *
 * RETURNS:
 *
 * Success - Valid skip slot pointer.
 *
 * Failure - NULL
 *
 * ARGUMENTS:
 *
 * size -> Size of the block that we are allocating.
 */
static	skip_alloc_t	*get_memory(const unsigned int size)
{
  skip_alloc_t	*slot_p, *update_p;
  void		*mem;
  unsigned int	need_size, block_n;
  
  /* do we need to print admin info? */
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_LOG_ADMIN)) {
    dmalloc_message("need %d bytes", size);
  }
  
  /* will this allocate put us over the limit? */
  if (_dmalloc_memory_limit > 0
      && alloc_cur_given + size > _dmalloc_memory_limit) {
    dmalloc_errno = ERROR_OVER_LIMIT;
    dmalloc_error("get_memory");
    return NULL;
  }
  
  /* do we have a divided block here? */
  if (size <= BLOCK_SIZE / 2) {
    return get_divided_memory(size);
  }
  
  /* round up to the nearest block size */
  need_size = size + BLOCK_SIZE - 1;
  block_n = need_size / BLOCK_SIZE;
  need_size = block_n * BLOCK_SIZE;
  
  update_p = skip_update;
  
  /* find a free block which matches the size */ 
  slot_p = use_free_memory(need_size, update_p);
  if (slot_p != NULL) {
    return slot_p;
  }
  
  /* if there are blocks that are larger than this */
  slot_p = update_p->sa_next_p[0];
  if (slot_p != NULL && slot_p->sa_total_size > size) {
    
    /*
     * now we ask again for the memory because we need to reset the
     * update pointer list
     */
    slot_p = use_free_memory(need_size, update_p);
    if (slot_p != NULL) {
      /* huh?  This isn't right. */
      dmalloc_errno = ERROR_ADDRESS_LIST;
      dmalloc_error("get_memory");
      return NULL;
    }
  }
  
  /* allocate the memory necessary for the new blocks */
  mem = _dmalloc_heap_alloc(need_size);
  if (mem == HEAP_ALLOC_ERROR) {
    /* error code set in _dmalloc_heap_alloc */
    return NULL;
  }
  user_block_c += block_n;
  
  /* create our slot */
  slot_p = insert_address(mem, 0 /* used list */, need_size);
  if (slot_p == NULL) {
    /* error set in insert_address */
    return NULL;
  }
  
  return slot_p;
}

/*
 * static int check_used_slot
 *
 * Check out the pointer in a allocated slot to make sure it is good.
 *
 * RETURNS:
 *
 * Success - 1
 *
 * Failure - 0
 *
 * ARGUMENTS:
 *
 * slot_p -> Slot that we are checking.
 *
 * user_pnt -> User pointer which was used to get the slot or NULL.
 *
 * exact_b -> Set to 1 to find the pointer specifically.  Otherwise we
 * can find the pointer inside of an allocation.
 *
 * strlen_b -> Make sure that pnt can hold at least a strlen + 1
 * bytes.  If 0 then ignore.
 *
 * min_size -> Make sure that pnt can hold at least that many bytes.
 * If 0 then ignore.
 */
static	int	check_used_slot(const skip_alloc_t *slot_p,
				const void *user_pnt, const int exact_b,
				const int strlen_b, const int min_size)
{
  const char	*file, *name_p, *bounds_p, *mem_p;
  unsigned int	line, num;
  pnt_info_t	pnt_info;
  
  if (! (BIT_IS_SET(slot_p->sa_flags, ALLOC_FLAG_USER)
	 || BIT_IS_SET(slot_p->sa_flags, ALLOC_FLAG_EXTERN)
	 || BIT_IS_SET(slot_p->sa_flags, ALLOC_FLAG_ADMIN))) {
    dmalloc_errno = ERROR_SLOT_CORRUPT;
    return 0;
  }
  
  /* get pointer info */
  get_pnt_info(slot_p, &pnt_info);
  
  /* the user pointer needs to be within the user space */
  if (user_pnt != NULL && (char *)user_pnt < (char *)pnt_info.pi_user_start) {
    dmalloc_errno = ERROR_WOULD_OVERWRITE;
    return 0;
  }
  
  /* if we need the exact pointer, make sure that the user_pnt agrees */
  if (exact_b && user_pnt != pnt_info.pi_user_start) {
    dmalloc_errno = ERROR_NOT_START_BLOCK;
    return 0;
  }
  
#if LARGEST_ALLOCATION
  /* have we exceeded the upper bounds */
  if (slot_p->sa_user_size > LARGEST_ALLOCATION) {
    dmalloc_errno = ERROR_BAD_SIZE;
    return 0;
  }
#endif
  
  /* check our total block size */
  if (slot_p->sa_total_size > BLOCK_SIZE / 2
      && slot_p->sa_total_size % BLOCK_SIZE != 0) {
    dmalloc_errno = ERROR_BAD_SIZE;
    return 0;
  }
  
  /*
   * If we have a valloc allocation then the _user_ pnt should be
   * block aligned otherwise the chunk_pnt should be.
   */
  if (pnt_info.pi_valloc_b) {
    
    if ((long)pnt_info.pi_user_start % BLOCK_SIZE != 0) {
      dmalloc_errno = ERROR_NOT_ON_BLOCK;
      return 0;
    }
    if (slot_p->sa_total_size < BLOCK_SIZE) {
      dmalloc_errno = ERROR_SLOT_CORRUPT;
      return 0;
    }
    
    /* now check the below space to make sure it is still clear */
    if (pnt_info.pi_fence_b && pnt_info.pi_blanked_b) {
      num = (char *)pnt_info.pi_fence_bottom - (char *)pnt_info.pi_alloc_start;
      if (num > 0) {
	for (mem_p = pnt_info.pi_alloc_start;
	     mem_p < (char *)pnt_info.pi_fence_bottom;
	     mem_p++) {
	  if (*mem_p != ALLOC_BLANK_CHAR) {
	    dmalloc_errno = ERROR_FREE_OVERWRITTEN;
	    return 0;
	  }
	}
      }
    }
  }
  
  /* check out the fence-posts */
  if (pnt_info.pi_fence_b && (! fence_read(&pnt_info))) {
    /* errno set in fence_read */
    return 0;
  }
  
  /* check above the allocation to see if it's been overwritten */
  if (pnt_info.pi_blanked_b) {
    
    if (pnt_info.pi_fence_b) {
      mem_p = (char *)pnt_info.pi_fence_top + FENCE_TOP_SIZE;
    }
    else {
      mem_p = pnt_info.pi_user_bounds;
    }
    
    for (; mem_p < (char *)pnt_info.pi_alloc_bounds; mem_p++) {
      if (*mem_p != ALLOC_BLANK_CHAR) {
	dmalloc_errno = ERROR_FREE_OVERWRITTEN;
	return 0;
      }
    }
  }

  file = slot_p->sa_file;
  line = slot_p->sa_line;
  
  /* check line number */
#if MAX_LINE_NUMBER
  if (line > MAX_LINE_NUMBER) {
    dmalloc_errno = ERROR_BAD_LINE;
    return 0;
  }
#endif
  
  /*
   * Check file pointer only if file is not NULL and line is not 0
   * which implies that file is a return-addr.
   */
#if MAX_FILE_LENGTH
  if (file != DMALLOC_DEFAULT_FILE && line != DMALLOC_DEFAULT_LINE) {
    /* NOTE: we don't use strlen here because we might check too far */
    bounds_p = file + MAX_FILE_LENGTH;
    for (name_p = file; name_p <= bounds_p && *name_p != '\0'; name_p++) {
    }
    if (name_p > bounds_p
	|| name_p < file + MIN_FILE_LENGTH) {
      dmalloc_errno = ERROR_BAD_FILE;
      return 0;
    }
  }
#endif
  
#if LOG_PNT_SEEN_COUNT
  /*
   * We divide by 2 here because realloc which returns the same
   * pointer will seen_c += 2.  However, it will never be more than
   * twice the iteration value.  We divide by two to not overflow
   * iter_c * 2.
   */
  if (slot_p->sa_seen_c / 2 > _dmalloc_iter_c) {
    dmalloc_errno = ERROR_SLOT_CORRUPT;
    return 0;
  }
#endif
  
  if (strlen_b) {
    int	equals_okay_b = 0;
    mem_p = (char *)user_pnt;
    if (min_size > 0) {
      bounds_p = mem_p + min_size;
      /* min_size can be out of bounds as long as we find a \0 beforehand */
      if (bounds_p > (char *)pnt_info.pi_user_bounds) {
	bounds_p = (char *)pnt_info.pi_user_bounds;
      } else {
	/* we can equals our boundary if our min_size <= user_bounds */
	equals_okay_b = 1;
      }
    } else {
      bounds_p = (char *)pnt_info.pi_user_bounds;
    }
    for (; mem_p < bounds_p; mem_p++) {
      if (*mem_p == '\0') {
	break;
      }
    }
    /* mem_p can == bounds_p if we hit the min_size but can't >= user_bounds*/ 
    if (mem_p > (char *)pnt_info.pi_user_bounds
	|| ((! equals_okay_b) && mem_p == (char *)pnt_info.pi_user_bounds)) {
      dmalloc_errno = ERROR_WOULD_OVERWRITE;
      return 0;
    }
  } else if (min_size > 0) {
    if ((char *)user_pnt + min_size > (char *)pnt_info.pi_user_bounds) {
      dmalloc_errno = ERROR_WOULD_OVERWRITE;
      return 0;
    }
  }
  
  return 1;
}

/*
 * static int check_free_slot
 *
 * Check out the pointer in a slot to make sure it is good.
 *
 * RETURNS:
 *
 * Success - 1
 *
 * Failure - 0
 *
 * ARGUMENTS:
 *
 * slot_p -> Slot that we are checking.
 */
static	int	check_free_slot(const skip_alloc_t *slot_p)
{
  char	*check_p;
  
  if (! BIT_IS_SET(slot_p->sa_flags, ALLOC_FLAG_FREE)) {
    dmalloc_errno = ERROR_SLOT_CORRUPT;
    return 0;
  }
  
  if (BIT_IS_SET(slot_p->sa_flags, ALLOC_FLAG_BLANK)) {
    for (check_p = (char *)slot_p->sa_mem;
	 check_p < (char *)slot_p->sa_mem + slot_p->sa_total_size;
	 check_p++) {
      if (*check_p != FREE_BLANK_CHAR) {
	dmalloc_errno = ERROR_FREE_OVERWRITTEN;
	return 0;
      }
    }
  }
  
#if LOG_PNT_SEEN_COUNT
  /*
   * We divide by 2 here because realloc which returns the same
   * pointer will seen_c += 2.  However, it will never be more than
   * twice the iteration value.  We divide by two to not overflow
   * iter_c * 2.
   */
  if (slot_p->sa_seen_c / 2 > _dmalloc_iter_c) {
    dmalloc_errno = ERROR_SLOT_CORRUPT;
    return 0;
  }
#endif
  
  return 1;
}

/***************************** exported routines *****************************/

/*
 * int _dmalloc_chunk_startup
 * 
 * DESCRIPTION:
 *
 * Startup the low level malloc routines.
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
int	_dmalloc_chunk_startup(void)
{
  unsigned int	value;
  char		*pos_p, *max_p;
  int		bit_c, *bits_p;
  
  value = FENCE_MAGIC_BOTTOM;
  max_p = fence_bottom + FENCE_BOTTOM_SIZE;
  for (pos_p = fence_bottom;
       pos_p < max_p;
       pos_p += sizeof(value)) {
    if (pos_p + sizeof(value) <= max_p) {
      memcpy(pos_p, (char *)&value, sizeof(value));
    }
    else {
      memcpy(pos_p, (char *)&value, max_p - pos_p);
    }
  }
  
  value = FENCE_MAGIC_TOP;
  max_p = fence_top + FENCE_TOP_SIZE;
  for (pos_p = fence_top; pos_p < max_p; pos_p += sizeof(value)) {
    if (pos_p + sizeof(value) <= max_p) {
      memcpy(pos_p, (char *)&value, sizeof(value));
    }
    else {
      memcpy(pos_p, (char *)&value, max_p - pos_p);
    }
  }
  
  /* initialize the bits array */
  bits_p = bit_sizes;
  for (bit_c = 0; bit_c < BASIC_BLOCK; bit_c++) {
    if ((1 << bit_c) >= CHUNK_SMALLEST_BLOCK) {
      *bits_p++ = 1 << bit_c;
    }
  }
  
  /* set the admin flags on the two statically allocated slots */
  skip_free_list->sa_flags = ALLOC_FLAG_ADMIN;
  skip_address_list->sa_flags = ALLOC_FLAG_ADMIN;
  
  return 1;
}

/*
 * char *_dmalloc_chunk_desc_pnt
 *
 * DESCRIPTION:
 *
 * Write into a buffer a pointer description with file and
 * line-number.
 *
 * RETURNS:
 *
 * Pointer to buffer 1st argument.
 *
 * ARGUMENTS:
 *
 * buf <-> Passed in buffer which will be filled with a description of
 * the pointer.
 *
 * buf_size -> Size of the buffer in bytes.
 *
 * file -> File name, return address, or NULL.
 *
 * line -> Line number or 0.
 */
char	*_dmalloc_chunk_desc_pnt(char *buf, const int buf_size,
				 const char *file, const unsigned int line)
{
  if (file == DMALLOC_DEFAULT_FILE && line == DMALLOC_DEFAULT_LINE) {
    (void)loc_snprintf(buf, buf_size, "unknown");
  }
  else if (line == DMALLOC_DEFAULT_LINE) {
    (void)loc_snprintf(buf, buf_size, "ra=%#lx", (unsigned long)file);
  }
  else if (file == DMALLOC_DEFAULT_FILE) {
    (void)loc_snprintf(buf, buf_size, "ra=ERROR(line=%u)", line);
  }
  else {
    (void)loc_snprintf(buf, buf_size, "%.*s:%u", MAX_FILE_LENGTH, file, line);
  }
  
  return buf;
}

/*
 * int _dmalloc_chunk_read_info
 *
 * DESCRIPTION:
 *
 * Return some information associated with a pointer.
 *
 * RETURNS:
 *
 * Success - 1 pointer is okay
 *
 * Failure - 0 problem with pointer
 *
 * ARGUMENTS:
 *
 * user_pnt -> Pointer we are checking.
 *
 * where <- Where the check is being made from.
 *
 * user_size_p <- Pointer to an unsigned int which, if not NULL, will
 * be set to the size of bytes that the user requested.
 *
 * alloc_size_p <- Pointer to an unsigned int which, if not NULL, will
 * be set to the total given size of bytes including block overhead.
 *
 * file_p <- Pointer to a character pointer which, if not NULL, will
 * be set to the file where the pointer was allocated.
 *
 * line_p <- Pointer to a character pointer which, if not NULL, will
 * be set to the line-number where the pointer was allocated.
 *
 * ret_attr_p <- Pointer to a void pointer, if not NULL, will be set
 * to the return-address where the pointer was allocated.
 *
 * seen_cp <- Pointer to an unsigned long which, if not NULL, will be
 * set to the number of times the pointer has been "seen".
 *
 * used_p <- Pointer to an unsigned long which, if not NULL, will be
 * set to the last time the pointer was "used".
 *
 * valloc_bp <- Pointer to an integer which, if not NULL, will be set
 * to 1 if the pointer was allocated with valloc otherwise 0.
 *
 * fence_bp <- Pointer to an integer which, if not NULL, will be set
 * to 1 if the pointer has the fence bit set otherwise 0.
 */
int	_dmalloc_chunk_read_info(const void *user_pnt, const char *where,
				 unsigned int *user_size_p,
				 unsigned int *alloc_size_p, char **file_p,
				 unsigned int *line_p, void **ret_attr_p,
				 unsigned long **seen_cp,
				 unsigned long *used_p, int *valloc_bp,
				 int *fence_bp)
{
  skip_alloc_t	*slot_p;
  
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_LOG_TRANS)) {
    dmalloc_message("reading info about pointer '%#lx'",
		    (unsigned long)user_pnt);
  }
  
  /* find the pointer with loose checking for fence */
  slot_p = find_address(user_pnt, 0 /* used list */, 0 /* not exact pointer */,
			skip_update);
  if (slot_p == NULL) {
    dmalloc_errno = ERROR_NOT_FOUND;
    log_error_info(NULL, 0, user_pnt, NULL, "finding address in heap", where);
    return 0;
  }
  
  /* might as well check the pointer now */
  if (! check_used_slot(slot_p, user_pnt, 1 /* exact */, 0 /* no strlen */,
			0 /* no min-size */)) {
    /* errno set in check_slot */
    log_error_info(NULL, 0, user_pnt, slot_p, "checking pointer admin", where);
    return 0;
  }
  
  /* write info back to user space */
  SET_POINTER(user_size_p, slot_p->sa_user_size);
  SET_POINTER(alloc_size_p, slot_p->sa_total_size);
  if (slot_p->sa_file == DMALLOC_DEFAULT_FILE) {
    SET_POINTER(file_p, NULL);
  }
  else {
    SET_POINTER(file_p, (char *)slot_p->sa_file);
  }
  SET_POINTER(line_p, slot_p->sa_line);
  /* if the line is blank then the file will be 0 or the return address */
  if (slot_p->sa_line == DMALLOC_DEFAULT_LINE) {
    SET_POINTER(ret_attr_p, (char *)slot_p->sa_file);
  }
  else {
    SET_POINTER(ret_attr_p, NULL);
  }
#if LOG_PNT_SEEN_COUNT
  SET_POINTER(seen_cp, &slot_p->sa_seen_c);
#else
  SET_POINTER(seen_cp, NULL);
#endif
  SET_POINTER(used_p, slot_p->sa_use_iter);
  SET_POINTER(valloc_bp, BIT_IS_SET(slot_p->sa_flags, ALLOC_FLAG_VALLOC));
  SET_POINTER(fence_bp, BIT_IS_SET(slot_p->sa_flags, ALLOC_FLAG_FENCE));
  
  return 1;
}

/******************************* heap checking *******************************/

/*
 * int _dmalloc_chunk_heap_check
 *
 * DESCRIPTION:
 *
 * Run extensive tests on the entire heap.
 *
 * RETURNS:
 *
 * Success - 1 if the heap is okay
 *
 * Failure - 0 if a problem was detected
 *
 * ARGUMENTS:
 *
 * None.
 */
int	_dmalloc_chunk_heap_check(void)
{
  skip_alloc_t	*slot_p;
  entry_block_t	*block_p;
  int		ret, level_c, checking_list_c = 0;
  int		final = 1;
  
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_LOG_TRANS)) {
    dmalloc_message("checking heap");
  }
  
  heap_check_c++;
  
  /*
   * first, run through all of the admin structures and check for
   * validity
   */
  for (level_c = 0; level_c < MAX_SKIP_LEVEL; level_c++) {
    unsigned int	*magic3_p, magic3;
    
    /* run through the blocks and test them */
    for (block_p = entry_blocks[level_c];
	 block_p != NULL;
	 block_p = block_p->eb_next_p) {
      
      /* better be in the heap */
      if (! IS_IN_HEAP(block_p)) {
	dmalloc_errno = ERROR_ADMIN_LIST;
	dmalloc_error("_dmalloc_chunk_heap_check");
	return 0;
      }
      
      /* get the magic3 at the end of the block */
      magic3_p = (unsigned int *)((char *)block_p + BLOCK_SIZE -
				  sizeof(*magic3_p));
      memcpy(&magic3, magic3_p, sizeof(magic3));
      
      /* check magics */
      if (block_p->eb_magic1 != ENTRY_BLOCK_MAGIC1
	  || block_p->eb_magic2 != ENTRY_BLOCK_MAGIC2
	  || magic3 != ENTRY_BLOCK_MAGIC3) {
	dmalloc_errno = ERROR_ADMIN_LIST;
	dmalloc_error("_dmalloc_chunk_heap_check");
	return 0;
      }
      
      /* check for a valid level */
      if (block_p->eb_level_n != level_c) {
	dmalloc_errno = ERROR_ADMIN_LIST;
	dmalloc_error("_dmalloc_chunk_heap_check");
	return 0;
      }
      
      /* now we look up the block and make sure it exists and is valid */
      slot_p = find_address(block_p, 0 /* used list */, 1 /* exact */,
			    skip_update);
      if (slot_p == NULL) {
	dmalloc_errno = ERROR_ADMIN_LIST;
	dmalloc_error("_dmalloc_chunk_heap_check");
	return 0;
      }
      if ((! BIT_IS_SET(slot_p->sa_flags, ALLOC_FLAG_ADMIN))
	  || slot_p->sa_mem != block_p
	  || slot_p->sa_total_size != BLOCK_SIZE
	  || slot_p->sa_level_n != level_c) {
	dmalloc_errno = ERROR_ADMIN_LIST;
	dmalloc_error("_dmalloc_chunk_heap_check");
	return 0;
      }
      
      /*
       * NOTE: we could now check each of the entries in the block to
       * make sure that they are valid and on the used or free list
       */
    }
  }
  
  /*
   * Now run through the used pointers and check each one.
   */
  for (slot_p = skip_address_list->sa_next_p[0];
       ;
       slot_p = slot_p->sa_next_p[0]) {
    skip_alloc_t	*block_slot_p;
    
    /*
     * switch to the free list in the middle after we've checked the
     * used pointer slots
     */
    if (slot_p == NULL) {
      checking_list_c++;
      if (checking_list_c == 1) {
	slot_p = skip_free_list->sa_next_p[0];
      }
#if FREED_POINTER_DELAY
      else if (checking_list_c == 2) {
	slot_p = free_wait_list_head;
      }
#endif
      else {
	/* we are done */
	break;
      }
      if (slot_p == NULL) {
	break;
      }
    }
    
    /* better be in the heap */
    if (! IS_IN_HEAP(slot_p)) {
      dmalloc_errno = ERROR_ADDRESS_LIST;
      dmalloc_error("_dmalloc_chunk_heap_check");
      return 0;
    }
    
    /*
     * now we look up the slot pointer itself and make sure it exists
     * in a valid block
     */
    block_slot_p = find_address(slot_p, 0 /* used list */,
				0 /* not exact pointer */, skip_update);
    if (block_slot_p == NULL) {
      dmalloc_errno = ERROR_ADMIN_LIST;
      dmalloc_error("_dmalloc_chunk_heap_check");
      return 0;
    }
    
    /* point at the block */
    block_p = block_slot_p->sa_mem;
    
    /* check block magic */
    if (block_p->eb_magic1 != ENTRY_BLOCK_MAGIC1) {
      dmalloc_errno = ERROR_ADDRESS_LIST;
      dmalloc_error("_dmalloc_chunk_heap_check");
      return 0;
    }
    
    /* make sure the slot level matches */
    if (slot_p->sa_level_n != block_p->eb_level_n) {
      dmalloc_errno = ERROR_ADDRESS_LIST;
      dmalloc_error("_dmalloc_chunk_heap_check");
      return 0;
    }
    
    /* now check the allocation */
    if (checking_list_c == 0) {
      ret = check_used_slot(slot_p, NULL /* no user pnt */,
			    0 /* loose pnt checking */, 0 /* no strlen */,
			    0 /* no min-size */);
      if (! ret) {
	/* error set in check_slot */
	log_error_info(NULL, 0, NULL, slot_p, "checking user pointer",
		       "_dmalloc_chunk_heap_check");
	/* not a critical error */
	final = 0;
      }
    }
    else {
      ret = check_free_slot(slot_p);
      if (! ret) {
	/* error set in check_slot */
	log_error_info(NULL, 0, NULL, slot_p, "checking free pointer",
		       "_dmalloc_chunk_heap_check");
	/* not a critical error */
	final = 0;
      }
    }
  }
  
  return final;
}

/*
 * int _dmalloc_chunk_pnt_check
 *
 * DESCRIPTION:
 *
 * Run extensive tests on a pointer.
 *
 * RETURNS:
 *
 * Success - 1 if the pointer is okay
 *
 * Failure - 0 if not
 *
 * ARGUMENTS:
 *
 * func -> Function string which is checking the pointer.
 *
 * user_pnt -> Pointer we are checking.
 *
 * exact_b -> Set to 1 to find the pointer specifically.  Otherwise we
 * can find the pointer inside of an allocation.
 *
 * strlen_b -> Make sure that pnt can hold at least a strlen + 1
 * bytes.  If 0 then ignore.
 *
 * min_size -> Make sure that pnt can hold at least that many bytes.
 * If 0 then ignore.
 */
int	_dmalloc_chunk_pnt_check(const char *func, const void *user_pnt,
				 const int exact_b, const int strlen_b,
				 const int min_size)
{
  skip_alloc_t	*slot_p;
  
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_LOG_TRANS)) {
    if (func == NULL) {
      dmalloc_message("checking pointer '%#lx'", (unsigned long)user_pnt);
    }
    else {
      dmalloc_message("checking func '%s' pointer '%#lx'",
		      func, (unsigned long)user_pnt);
    }
  }
  
  /* try to find the address */
  slot_p = find_address(user_pnt, 0 /* used list */, 0 /* not exact pointer */,
			skip_update);
  if (slot_p == NULL) {
    if (exact_b) {
      dmalloc_errno = ERROR_NOT_FOUND;
      log_error_info(NULL, 0, user_pnt, NULL, "pointer-check", func);
      return 0;
    }
    else {
      return 1;
    }
  }
  
  /* now make sure that the user slot is valid */
  if (! check_used_slot(slot_p, user_pnt, exact_b, strlen_b, min_size)) {
    /* dmalloc_error set in check_used_slot */
    log_error_info(NULL, 0, user_pnt, slot_p, "pointer-check", func);
    return 0;
  }
  
  return 1;
}

/************************** low-level user functions *************************/

/*
 * void *_dmalloc_chunk_malloc
 *
 * DESCRIPTION:
 *
 * Allocate a chunk of memory.
 *
 * RETURNS:
 *
 * Success - Valid pointer.
 *
 * Failure - NULL
 *
 * ARGUMENTS:
 *
 * file -> File-name or return-address location of the allocation.
 *
 * line -> Line-number location of the allocation.
 *
 * size -> Number of bytes to allocate.
 *
 * func_id -> Calling function-id as defined in dmalloc.h.
 *
 * alignment -> If greater than 0 then try to align the returned
 * block.
 */
void	*_dmalloc_chunk_malloc(const char *file, const unsigned int line,
			       const unsigned long size, const int func_id,
			       const unsigned int alignment)
{
  unsigned long	needed_size;
  int		valloc_b = 0, memalign_b = 0, fence_b = 0;
  char		where_buf[MAX_FILE_LENGTH + 64], disp_buf[64];
  skip_alloc_t	*slot_p;
  pnt_info_t	pnt_info;
  const char	*trans_log;
  
  /* counts calls to malloc */
  if (func_id == DMALLOC_FUNC_CALLOC) {
    func_calloc_c++;
  }
  else if (alignment == BLOCK_SIZE) {
    func_valloc_c++;
    valloc_b = 1;
  }
  else if (alignment > 0) {
    func_memalign_c++;
    memalign_b = 1;
  }
  else if (func_id == DMALLOC_FUNC_NEW) {
    func_new_c++;
  }
  else if (func_id != DMALLOC_FUNC_REALLOC
	   && func_id != DMALLOC_FUNC_RECALLOC) {
    func_malloc_c++;
  }
  
#if ALLOW_ALLOC_ZERO_SIZE == 0
  if (size == 0) {
    dmalloc_errno = ERROR_BAD_SIZE;
    log_error_info(file, line, NULL, NULL, "bad zero byte allocation request",
		   "malloc");
    return MALLOC_ERROR;
  }
#endif
  
#if LARGEST_ALLOCATION
  /* have we exceeded the upper bounds */
  if (size > LARGEST_ALLOCATION) {
    dmalloc_errno = ERROR_TOO_BIG;
    log_error_info(file, line, NULL, NULL, "allocation too big", "malloc");
    return MALLOC_ERROR;
  }
#endif
  
  needed_size = size;
  
  /* adjust the size */
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_CHECK_FENCE)) {
    needed_size += FENCE_OVERHEAD_SIZE;
    fence_b = 1;
    
    /*
     * If the user is requesting a page-aligned block of data then we
     * will need another block below the allocation just for the fence
     * information.  Ugh.
     */
    if (valloc_b) {
      needed_size += BLOCK_SIZE;
    }
  }
  else if (valloc_b && needed_size <= BLOCK_SIZE / 2) {
    /*
     * If we are valloc-ing, make sure that we get a blocksized chunk
     * because they are always block aligned.  We know here that fence
     * posting is not on otherwise it would have been set above.
     */
    needed_size = BLOCK_SIZE;
  }
  
  /* get some space for our memory */
  slot_p = get_memory(needed_size);
  if (slot_p == NULL) {
    /* errno set in get_slot */
    return MALLOC_ERROR;
  }
  if (fence_b) {
    BIT_SET(slot_p->sa_flags, ALLOC_FLAG_FENCE);
  }
  if (valloc_b) {
    BIT_SET(slot_p->sa_flags, ALLOC_FLAG_VALLOC);
  }
  slot_p->sa_user_size = size;
  
  /* initialize the bblocks */
  alloc_cur_given += slot_p->sa_total_size;
  alloc_max_given = MAX(alloc_max_given, alloc_cur_given);
  
  get_pnt_info(slot_p, &pnt_info);
  
  /* clear the allocation */
  clear_alloc(slot_p, &pnt_info, 0 /* no old-size */, func_id);
  
  slot_p->sa_file = file;
  slot_p->sa_line = line;
  slot_p->sa_use_iter = _dmalloc_iter_c;
#if LOG_PNT_SEEN_COUNT
  slot_p->sa_seen_c++;
#endif
#if LOG_PNT_ITERATION
  slot_p->sa_iteration = _dmalloc_iter_c;
#endif
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_LOG_ELAPSED_TIME)
      || BIT_IS_SET(_dmalloc_flags, DEBUG_LOG_CURRENT_TIME)) {
#if LOG_PNT_TIMEVAL
    GET_TIMEVAL(slot_p->sa_timeval);
#else
#if LOG_PNT_TIME
    slot_p->sa_time = time(NULL);
#endif
#endif
  }
  
#if LOG_PNT_THREAD_ID
  slot_p->sa_thread_id = THREAD_GET_ID();
#endif
  
  /* do we need to print transaction info? */
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_LOG_TRANS)) {
    switch (func_id) {
    case DMALLOC_FUNC_CALLOC:
      trans_log = "calloc";
      break;
    case DMALLOC_FUNC_MEMALIGN:
      trans_log = "memalign";
      break;
    case DMALLOC_FUNC_VALLOC:
      trans_log = "valloc";
      break;
    default:
      trans_log = "alloc";
      break;
    }
    dmalloc_message("*** %s: at '%s' for %ld bytes, got '%s'",
		    trans_log,
		    _dmalloc_chunk_desc_pnt(where_buf, sizeof(where_buf),
					    file, line),
		    size, display_pnt(pnt_info.pi_user_start, slot_p, disp_buf,
				      sizeof(disp_buf)));
  }
  
#if MEMORY_TABLE_TOP_LOG
  _dmalloc_table_insert(mem_table_alloc, MEM_ALLOC_ENTRIES, file, line,
			size, &mem_table_alloc_c);
#endif
  
  /* monitor current allocation level */
  alloc_current += size;
  alloc_maximum = MAX(alloc_maximum, alloc_current);
  _dmalloc_alloc_total += size;
  alloc_one_max = MAX(alloc_one_max, size);
  
  /* monitor pointer usage */
  alloc_cur_pnts++;
  alloc_max_pnts = MAX(alloc_max_pnts, alloc_cur_pnts);
  alloc_tot_pnts++;
  
  return pnt_info.pi_user_start;
}

/*
 * int _dmalloc_chunk_free
 *
 * DESCRIPTION:
 *
 * Free a user pointer from the heap.
 *
 * RETURNS:
 *
 * Success - FREE_NOERROR
 *
 * Failure - FREE_ERROR
 *
 * ARGUMENTS:
 *
 * file -> File-name or return-address location of the allocation.
 *
 * line -> Line-number location of the allocation.
 *
 * user_pnt -> Pointer we are freeing.
 *
 * func_id -> Function ID
 */
int	_dmalloc_chunk_free(const char *file, const unsigned int line,
			    void *user_pnt, const int func_id)
{
  char		where_buf[MAX_FILE_LENGTH + 64];
  char		where_buf2[MAX_FILE_LENGTH + 64], disp_buf[64];
  skip_alloc_t	*slot_p, *update_p;
  
  /* counts calls to free */
  if (func_id == DMALLOC_FUNC_DELETE) {
    func_delete_c++;
  }
  else if (func_id == DMALLOC_FUNC_REALLOC
	   || func_id == DMALLOC_FUNC_RECALLOC) {
    /* ignore these because they will alredy be accounted for in realloc */
  }
  else {
    func_free_c++;
  }
  
  if (user_pnt == NULL) {
    
#if ALLOW_FREE_NULL_MESSAGE
    /* does the user want a specific message? */
    dmalloc_message("WARNING: tried to free(0) from '%s'",
		    _dmalloc_chunk_desc_pnt(where_buf, sizeof(where_buf),
					    file, line));
#endif
    
    /*
     * NOTE: we have here both a default in the settings.h file and a
     * runtime token in case people want to turn it on or off at
     * runtime.
     */
    if (BIT_IS_SET(_dmalloc_flags, DEBUG_ERROR_FREE_NULL)) {
      dmalloc_errno = ERROR_IS_NULL;
      log_error_info(file, line, user_pnt, NULL, "invalid 0L pointer", "free");
      return FREE_ERROR;
    }
    
#if ALLOW_FREE_NULL == 0
    dmalloc_errno = ERROR_IS_NULL;
#endif
    return FREE_ERROR;
  }
  
  update_p = skip_update;
  
  /* try to find the address with loose match */
  slot_p = find_address(user_pnt, 0 /* used list */, 0 /* not exact pointer */,
			skip_update);
  if (slot_p == NULL) {
#if FREED_POINTER_DELAY
    skip_alloc_t	*del_p;
    
    /* search the delay list */
    for (del_p = free_wait_list_head;
	 del_p != NULL;
	 del_p = del_p->sa_next_p[0]) {
      if (del_p->sa_mem <= user_pnt
	  && (char *)del_p->sa_mem + del_p->sa_total_size > (char *)user_pnt) {
	pnt_info_t	info;
	get_pnt_info(del_p, &info);
	if (info.pi_user_start == user_pnt) {
	  dmalloc_errno = ERROR_ALREADY_FREE;
	}
	else {
	  dmalloc_errno = ERROR_NOT_FOUND;
	}
	break;
      }
    }
    if (del_p == NULL) {
#endif
      /* not in the used list so check the free list */
      if (find_address(user_pnt, 1 /* free list */, 0 /* not exact pointer */,
		       skip_update) == NULL) {
	dmalloc_errno = ERROR_NOT_FOUND;
      }
      else {
	dmalloc_errno = ERROR_ALREADY_FREE;
      }
#if FREED_POINTER_DELAY
    }
#endif
    log_error_info(file, line, user_pnt, NULL, "finding address in heap",
		   "free");
    return FREE_ERROR;
  }
  
  if (! check_used_slot(slot_p, user_pnt, 1 /* exact pnt */, 0 /* no strlen */,
			0 /* no min-size */)) {
    /* error set in check slot */
    log_error_info(file, line, user_pnt, slot_p, "checking pointer admin",
		   "free");
    return FREE_ERROR;
  }
  
  if (! remove_slot(slot_p, update_p)) {
    /* error set and dumped in remove_slot */
    return FREE_ERROR;
  }
  if (BIT_IS_SET(slot_p->sa_flags, ALLOC_FLAG_FENCE)) {
    /*
     * We need to preserve the fence-post flag because we may need to
     * properly check for previously freed pointers in the future.
     */
    slot_p->sa_flags = ALLOC_FLAG_FREE | ALLOC_FLAG_FENCE;
  }
  else {
    slot_p->sa_flags = ALLOC_FLAG_FREE;
  }
  
  alloc_cur_pnts--;
  
  slot_p->sa_use_iter = _dmalloc_iter_c;
#if LOG_PNT_SEEN_COUNT
  slot_p->sa_seen_c++;
#endif
  
  /* do we need to print transaction info? */
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_LOG_TRANS)) {
    dmalloc_message("*** free: at '%s' pnt '%s': size %u, alloced at '%s'",
		    _dmalloc_chunk_desc_pnt(where_buf, sizeof(where_buf), file,
					    line),
		    display_pnt(user_pnt, slot_p, disp_buf, sizeof(disp_buf)),
		    slot_p->sa_user_size,
		    _dmalloc_chunk_desc_pnt(where_buf2, sizeof(where_buf2),
					    slot_p->sa_file, slot_p->sa_line));
  }
  
#if MEMORY_TABLE_TOP_LOG
  _dmalloc_table_delete(mem_table_alloc, MEM_ALLOC_ENTRIES, slot_p->sa_file,
			slot_p->sa_line, slot_p->sa_user_size);
#endif
  
  /* update the file/line -- must be after _dmalloc_table_delete */
  slot_p->sa_file = file;
  slot_p->sa_line = line;
  
  /* monitor current allocation level */
  alloc_current -= slot_p->sa_user_size;
  alloc_cur_given -= slot_p->sa_total_size;
  free_space_bytes += slot_p->sa_total_size;
  
  /* clear the memory */
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_FREE_BLANK)
      || BIT_IS_SET(_dmalloc_flags, DEBUG_CHECK_BLANK)) {
    memset(slot_p->sa_mem, FREE_BLANK_CHAR, slot_p->sa_total_size);
    /* set our slot blank flag */
    BIT_SET(slot_p->sa_flags, ALLOC_FLAG_BLANK);
  }
  
  /*
   * The question is should we combine multiple free chunks together
   * into one.  This would help we with fragmentation but it would
   * screwup the seen counter.
   *
   * Check above and below the free bblock looking for neighbors that
   * are free so we can add them together and put them in a different
   * free slot.
   *
   * NOTE: all of these block's reuse-iter count will be moved ahead
   * because we are encorporating in this newly freed block.
   */
  
  if (! BIT_IS_SET(_dmalloc_flags, DEBUG_NEVER_REUSE)) {
#if FREED_POINTER_DELAY
    slot_p->sa_next_p[0] = NULL;
    if (free_wait_list_head == NULL) {
      free_wait_list_head = slot_p;
    }
    else {
      free_wait_list_tail->sa_next_p[0] = slot_p;
    }
    free_wait_list_tail = slot_p;
#else
    /* put slot on free list */
    if (! insert_slot(slot_p, 1 /* free list */)) {
      /* error dumped in insert_slot */
      return FREE_ERROR;
    }
#endif
  }
  
  return FREE_NOERROR;
}

/*
 * void *_dmalloc_chunk_realloc
 *
 * DESCRIPTION:
 *
 * Re-allocate a chunk of memory either shrinking or expanding it.
 *
 * RETURNS:
 *
 * Success - Valid pointer.
 *
 * Failure - NULL
 *
 * ARGUMENTS:
 *
 * file -> File-name or return-address location of the allocation.
 *
 * line -> Line-number location of the allocation.
 *
 * old_user_pnt -> Old user pointer that we are reallocating.
 *
 * new_size -> New-size to change the pointer.
 *
 * func_id -> Calling function-id as defined in dmalloc.h.
 */
void	*_dmalloc_chunk_realloc(const char *file, const unsigned int line,
				void *old_user_pnt,
				const unsigned long new_size,
				const int func_id)
{
  const char	*old_file;
  skip_alloc_t	*slot_p;
  pnt_info_t	pnt_info;
  void		*new_user_pnt;
  unsigned int	old_size, old_line;
  
  /* counts calls to realloc */
  if (func_id == DMALLOC_FUNC_RECALLOC) {
    func_recalloc_c++;
  }
  else {
    func_realloc_c++;
  }
  
#if ALLOW_ALLOC_ZERO_SIZE == 0
  if (new_size == 0) {
    dmalloc_errno = ERROR_BAD_SIZE;
    log_error_info(file, line, NULL, NULL, "bad zero byte allocation request",
		   "realloc");
    return REALLOC_ERROR;
  }
#endif
  
  /* by now malloc.c should have taken care of the realloc(NULL) case */
  if (old_user_pnt == NULL) {
    dmalloc_errno = ERROR_IS_NULL;
    log_error_info(file, line, old_user_pnt, NULL, "invalid pointer",
		   "realloc");
    return REALLOC_ERROR;
  }
  
  /* find the old pointer with loose checking for fence post stuff */
  slot_p = find_address(old_user_pnt, 0 /* used list */,
			0 /* not exact pointer */, skip_update);
  if (slot_p == NULL) {
    dmalloc_errno = ERROR_NOT_FOUND;
    log_error_info(file, line, old_user_pnt, NULL, "finding address in heap",
		   "realloc");
    return 0;
  }
  
  /* get info about the pointer */
  get_pnt_info(slot_p, &pnt_info);
  old_file = slot_p->sa_file;
  old_line = slot_p->sa_line;
  old_size = slot_p->sa_user_size;
  
  /* if we are not realloc copying and the size is the same */
  if ((char *)pnt_info.pi_user_start + new_size >
      (char *)pnt_info.pi_upper_bounds
      || BIT_IS_SET(_dmalloc_flags, DEBUG_REALLOC_COPY)
      || BIT_IS_SET(_dmalloc_flags, DEBUG_NEVER_REUSE)) {
    int	min_size;
    
    /* allocate space for new chunk */
    new_user_pnt = _dmalloc_chunk_malloc(file, line, new_size, func_id,
				    0 /* no align */);
    if (new_user_pnt == MALLOC_ERROR) {
      return REALLOC_ERROR;
    }
    
    /*
     * NOTE: _chunk_malloc() already took care of the fence stuff and
     * zeroing of memory.
     */
    
    /* copy stuff into new section of memory */
    min_size = MIN(new_size, old_size);
    if (min_size > 0) {
      memcpy(new_user_pnt, pnt_info.pi_user_start, min_size);
    }
    
    /* free old pointer */
    if (_dmalloc_chunk_free(file, line, old_user_pnt,
			    func_id) != FREE_NOERROR) {
      return REALLOC_ERROR;
    }
  }
  else {
    /* new pointer is the same as the old one */
    new_user_pnt = pnt_info.pi_user_start;
    
    /*
     * monitor current allocation level
     *
     * NOTE: we do this here since the malloc/free used above take care
     * on if in that section
     */
    alloc_current += new_size - old_size;
    alloc_maximum = MAX(alloc_maximum, alloc_current);
    _dmalloc_alloc_total += new_size;
    alloc_one_max = MAX(alloc_one_max, new_size);
    
    /* monitor pointer usage */
    alloc_tot_pnts++;
    
    /* change the slot information */
    slot_p->sa_user_size = new_size;
    get_pnt_info(slot_p, &pnt_info);
    
    clear_alloc(slot_p, &pnt_info, old_size, func_id);
    
    slot_p->sa_use_iter = _dmalloc_iter_c;
#if LOG_PNT_SEEN_COUNT
    /* we see in inbound and outbound so we need to increment by 2 */
    slot_p->sa_seen_c += 2;
#endif
    
#if MEMORY_TABLE_TOP_LOG
    _dmalloc_table_delete(mem_table_alloc, MEM_ALLOC_ENTRIES,
			  slot_p->sa_file, slot_p->sa_line, old_size);
    _dmalloc_table_insert(mem_table_alloc, MEM_ALLOC_ENTRIES, file, line,
			  new_size, &mem_table_alloc_c);
#endif
  
    /*
     * finally, we update the file/line info -- must be after
     * _dmalloc_table functions
     */
    slot_p->sa_file = file;
    slot_p->sa_line = line;
  }
  
  if (BIT_IS_SET(_dmalloc_flags, DEBUG_LOG_TRANS)) {
    const char	*trans_log;
    char	where_buf[MAX_FILE_LENGTH + 64];
    char	where_buf2[MAX_FILE_LENGTH + 64];
    
    if (func_id == DMALLOC_FUNC_RECALLOC) {
      trans_log = "recalloc";
    }
    else {
      trans_log = "realloc";
    }
    dmalloc_message("*** %s: at '%s' from '%#lx' (%u bytes) file '%s' to '%#lx' (%lu bytes)",
		    trans_log,
		    _dmalloc_chunk_desc_pnt(where_buf, sizeof(where_buf),
					    file, line),
		    (unsigned long)old_user_pnt, old_size,
		    _dmalloc_chunk_desc_pnt(where_buf2, sizeof(where_buf2),
					    old_file, old_line),
		    (unsigned long)new_user_pnt, new_size);
  }
  
  return new_user_pnt;
}

/***************************** diagnostic routines ***************************/

/*
 * void _dmalloc_chunk_log_stats
 *
 * DESCRIPTION:
 *
 * Log general statistics from the heap to the logfile.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * None.
 */
void	_dmalloc_chunk_log_stats(void)
{
  unsigned long	overhead, user_space, tot_space;
  
  dmalloc_message("Dumping Chunk Statistics:");
  
  tot_space = (user_block_c + admin_block_c) * BLOCK_SIZE;
  user_space = alloc_current + free_space_bytes;
  overhead = admin_block_c * BLOCK_SIZE;
  
  /* version information */
  dmalloc_message("basic-block %d bytes, alignment %d bytes",
		  BLOCK_SIZE, ALLOCATION_ALIGNMENT);
  
  /* general heap information with blocks */
  dmalloc_message("heap address range: %#lx to %#lx, %ld bytes",
		  (unsigned long)_dmalloc_heap_low,
		  (unsigned long)_dmalloc_heap_high,
		  (unsigned long)_dmalloc_heap_high -
		  (unsigned long)_dmalloc_heap_low);
  dmalloc_message("    user blocks: %ld blocks, %ld bytes (%ld%%)",
		  user_block_c, user_space,
		  (tot_space < 100 ? 0 : user_space / (tot_space / 100)));
  dmalloc_message("   admin blocks: %ld blocks, %ld bytes (%ld%%)",
		  admin_block_c, overhead,
		  (tot_space < 100 ? 0 : overhead / (tot_space / 100)));
  dmalloc_message("   total blocks: %ld blocks, %ld bytes",
		  user_block_c + admin_block_c, tot_space);
  
  dmalloc_message("heap checked %ld", heap_check_c);
  
  /* log user allocation information */
  dmalloc_message("alloc calls: malloc %lu, calloc %lu, realloc %lu, free %lu",
		  func_malloc_c, func_calloc_c, func_realloc_c, func_free_c);
  dmalloc_message("alloc calls: recalloc %lu, memalign %lu, valloc %lu",
		  func_recalloc_c, func_memalign_c, func_valloc_c);
  dmalloc_message("alloc calls: new %lu, delete %lu",
		  func_new_c, func_delete_c);
  dmalloc_message("  current memory in use: %lu bytes (%lu pnts)",
		  alloc_current, alloc_cur_pnts);
  dmalloc_message(" total memory allocated: %lu bytes (%lu pnts)",
		  _dmalloc_alloc_total, alloc_tot_pnts);
  
  /* maximum stats */
  dmalloc_message(" max in use at one time: %lu bytes (%lu pnts)",
		  alloc_maximum, alloc_max_pnts);
  dmalloc_message("max alloced with 1 call: %lu bytes",
		  alloc_one_max);
  dmalloc_message("max unused memory space: %lu bytes (%lu%%)",
		  alloc_max_given - alloc_maximum,
		  (alloc_max_given == 0 ? 0 :
		   ((alloc_max_given - alloc_maximum) * 100) /
		   alloc_max_given));
  
#if MEMORY_TABLE_TOP_LOG
  dmalloc_message("top %d allocations:", MEMORY_TABLE_TOP_LOG);
  _dmalloc_table_log_info(mem_table_alloc, mem_table_alloc_c,
			  MEM_ALLOC_ENTRIES, MEMORY_TABLE_TOP_LOG,
			  1 /* have in-use column */);
#endif
}

/*
 * void _dmalloc_chunk_log_changed
 *
 * DESCRIPTION:
 *
 * Log the pointers that has changed since a pointer in time.
 *
 * RETURNS:
 *
 * None.
 *
 * ARGUMENTS:
 *
 * mark -> Dmalloc counter used to mark a specific time so that
 * servers can check on the changed pointers.
 *
 * log_non_free_b -> If set to 1 then log the new not-freed
 * (i.e. used) pointers.
 *
 * log_free_b -> If set to 1 then log the new freed pointers.
 *
 * details_b -> If set to 1 then dump the individual pointer entries
 * instead of just the summary.
 */
void	_dmalloc_chunk_log_changed(const unsigned long mark,
				   const int log_not_freed_b,
				   const int log_freed_b, const int details_b)
{
  skip_alloc_t	*slot_p;
  pnt_info_t	pnt_info;
  int		known_b, freed_b, used_b;
  char		out[DUMP_SPACE * 4], *which_str;
  char		where_buf[MAX_FILE_LENGTH + 64], disp_buf[64];
  int		unknown_size_c = 0, unknown_block_c = 0, out_len;
  int		size_c = 0, block_c = 0, checking_list_c = 0;
  
  if (log_not_freed_b && log_freed_b) {
    which_str = "Not-Freed and Freed";
  }
  else if (log_not_freed_b) {
    which_str = "Not-Freed";
  }
  else if (log_freed_b) {
    which_str = "Freed";
  }
  else {
    return;
  }
  
  if (mark == 0) {
    dmalloc_message("Dumping %s Pointers Changed Since Start:", which_str);
  }
  else {
    dmalloc_message("Dumping %s Pointers Changed Since Mark %lu:",
		    which_str, mark);
  }
  
  /* clear out our memory table so we can fill it with pointer info */
  _dmalloc_table_clear(mem_table_changed, MEM_CHANGED_ENTRIES,
		       &mem_table_changed_c);
  
  /* run through the blocks */
  for (slot_p = skip_address_list->sa_next_p[0];
       ;
       slot_p = slot_p->sa_next_p[0]) {
    
    /*
     * switch to the free list in the middle after we've checked the
     * used pointer slots
     */
    if (slot_p == NULL) {
      checking_list_c++;
      if (checking_list_c == 1) {
	slot_p = skip_free_list->sa_next_p[0];
      }
#if FREED_POINTER_DELAY
      else if (checking_list_c == 2) {
	slot_p = free_wait_list_head;
      }
#endif
      else {
	/* we are done */
	break;
      }
      if (slot_p == NULL) {
	break;
      }
    }
    
    freed_b = BIT_IS_SET(slot_p->sa_flags, ALLOC_FLAG_FREE);
    used_b = BIT_IS_SET(slot_p->sa_flags, ALLOC_FLAG_USER);
    
    /*
     * check for different types
     */
    if (! (freed_b || used_b)) {
      continue;
    }
    
    /* do we want to dump this one? */
    if (! ((log_not_freed_b && used_b) || (log_freed_b && freed_b))) {
      continue;
    }    
    /* is it too long ago? */
    if (slot_p->sa_use_iter <= mark) {
      continue;
    }
    
    /* unknown pointer? */
    if (slot_p->sa_file == DMALLOC_DEFAULT_FILE
	|| slot_p->sa_line == DMALLOC_DEFAULT_LINE) {
      unknown_block_c++;
      unknown_size_c += slot_p->sa_user_size;
      known_b = 0;
    }
    else {
      known_b = 1;
    }
    
    get_pnt_info(slot_p, &pnt_info);
    
    if (known_b || (! BIT_IS_SET(_dmalloc_flags, DEBUG_LOG_KNOWN))) {
      if (details_b) {
	dmalloc_message(" %s freed: '%s' (%u bytes) from '%s'",
			(freed_b ? "   " : "not"),
			display_pnt(pnt_info.pi_user_start, slot_p, disp_buf,
				    sizeof(disp_buf)),
			slot_p->sa_user_size,
			_dmalloc_chunk_desc_pnt(where_buf, sizeof(where_buf),
						slot_p->sa_file,
						slot_p->sa_line));
	
	if ((! freed_b)
	    && BIT_IS_SET(_dmalloc_flags, DEBUG_LOG_NONFREE_SPACE)) {
	  out_len = expand_chars((char *)pnt_info.pi_user_start, DUMP_SPACE,
				 out, sizeof(out));
	  dmalloc_message("  dump of '%#lx': '%.*s'",
			  (unsigned long)pnt_info.pi_user_start, out_len, out);
	}
      }
      _dmalloc_table_insert(mem_table_changed, MEM_CHANGED_ENTRIES,
			    slot_p->sa_file, slot_p->sa_line,
			    slot_p->sa_user_size, &mem_table_changed_c);
    }
  }
  
  /* dump the summary from the table table */
  _dmalloc_table_log_info(mem_table_changed, mem_table_changed_c,
			  MEM_CHANGED_ENTRIES, 0 /* log all entries */,
			  0 /* no in-use column */);
  
  /* copy out size of pointers */
  if (block_c > 0) {
    if (block_c - unknown_block_c > 0) {
      dmalloc_message(" known memory: %d pointer%s, %d bytes",
		      block_c - unknown_block_c,
		      (block_c - unknown_block_c == 1 ? "" : "s"),
		      size_c - unknown_size_c);
    }
    if (unknown_block_c > 0) {
      dmalloc_message(" unknown memory: %d pointer%s, %d bytes",
		      unknown_block_c, (unknown_block_c == 1 ? "" : "s"),
		      unknown_size_c);
    }
  }
}

/*
 * unsigned long _dmalloc_chunk_count_changed
 *
 * DESCRIPTION:
 *
 * Return the pointers that has changed since a pointer in time.
 *
 * RETURNS:
 *
 * Number of bytes changed since mark.
 *
 * ARGUMENTS:
 *
 * mark -> Dmalloc counter used to mark a specific time so that
 * servers can check on the changed pointers.
 *
 * count_non_free_b -> If set to 1 then count the new not-freed
 * (i.e. used) pointers.
 *
 * count_free_b -> If set to 1 then count the new freed pointers.
 */
unsigned long	_dmalloc_chunk_count_changed(const unsigned long mark,
					     const int count_not_freed_b,
					     const int count_freed_b)
{
  skip_alloc_t	*slot_p;
  int		freed_b, used_b;
  int		checking_list_c = 0;
  unsigned int	mem_count = 0;
  
  /* run through the blocks */
  for (slot_p = skip_address_list->sa_next_p[0];
       ;
       slot_p = slot_p->sa_next_p[0]) {
    
    /*
     * switch to the free list in the middle after we've checked the
     * used pointer slots
     */
    if (slot_p == NULL) {
      checking_list_c++;
      if (checking_list_c == 1) {
	slot_p = skip_free_list->sa_next_p[0];
      }
#if FREED_POINTER_DELAY
      else if (checking_list_c == 2) {
	slot_p = free_wait_list_head;
      }
#endif
      else {
	/* we are done */
	break;
      }
      if (slot_p == NULL) {
	break;
      }
    }
    
    freed_b = BIT_IS_SET(slot_p->sa_flags, ALLOC_FLAG_FREE);
    used_b = BIT_IS_SET(slot_p->sa_flags, ALLOC_FLAG_USER);
    
    /*
     * check for different types
     */
    if (! (freed_b || used_b)) {
      continue;
    }
    /* is it too long ago? */
    if (slot_p->sa_use_iter <= mark) {
      continue;
    }
    
    /* count the memory */
    if (count_not_freed_b && used_b) {
      mem_count += slot_p->sa_user_size;
    }
    else if (count_freed_b && freed_b) {
      mem_count += slot_p->sa_user_size;
    }
  }
  
  return mem_count;
}

/*
 * void _dmalloc_chunk_get_stats
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
void	_dmalloc_chunk_get_stats(void **heap_low_p, void **heap_high_p,
				 unsigned long *total_space_p,
				 unsigned long *user_space_p,
				 unsigned long *current_allocated_p,
				 unsigned long *current_pnt_np,
				 unsigned long *max_allocated_p,
				 unsigned long *max_pnt_np,
				 unsigned long *max_one_p)
{
  SET_POINTER(heap_low_p, _dmalloc_heap_low);
  SET_POINTER(heap_high_p, _dmalloc_heap_high);
  SET_POINTER(total_space_p, (user_block_c + admin_block_c) * BLOCK_SIZE);
  SET_POINTER(user_space_p, alloc_current + free_space_bytes);
  SET_POINTER(current_allocated_p, alloc_current);
  SET_POINTER(current_pnt_np, alloc_cur_pnts);
  SET_POINTER(max_allocated_p, alloc_maximum);
  SET_POINTER(max_pnt_np, alloc_max_pnts);
  SET_POINTER(max_one_p, alloc_one_max);
}
