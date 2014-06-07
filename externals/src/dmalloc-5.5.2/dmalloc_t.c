/*
 * Test program for malloc code
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
 * $Id: dmalloc_t.c,v 1.129 2007/03/25 18:53:41 gray Exp $
 */

/*
 * Test program for the malloc library.  Current it is interactive although
 * should be script based.
 */

#include <stdio.h>				/* for stdin */

#if HAVE_STDLIB_H
# include <stdlib.h>				/* for atoi + */
#endif
#if HAVE_STRING_H
# include <string.h>
#endif
#if HAVE_UNISTD_H
# include <unistd.h>
#endif

#include "conf.h"
#include "compat.h"				/* for loc_snprintf */

#if HAVE_TIME
# ifdef TIME_INCLUDE
#  include TIME_INCLUDE
# endif
#endif

#include "dmalloc.h"
#include "dmalloc_argv.h"
#include "dmalloc_rand.h"
#include "arg_check.h"

/*
 * NOTE: these are only needed to test certain features of the library.
 */
#include "debug_tok.h"
#include "error_val.h"
#include "heap.h"				/* for external testing */

#define INTER_CHAR		'i'
#define DEFAULT_ITERATIONS	10000
#define MAX_POINTERS		1024
#if HAVE_SBRK == 0 && HAVE_MMAP == 0
/* if we have a small memory area then just take 1/10 of the internal space */
#define MAX_ALLOC		(INTERNAL_MEMORY_SPACE / 10)
#else
/* otherwise allocate a megabyte */
#define MAX_ALLOC		(1024 * 1024)
#endif
#define MIN_AVAIL		10

/* pointer tracking structure */
typedef struct pnt_info_st {
  long			pi_crc;			/* crc of storage */
  int			pi_size;		/* size of storage */
  void			*pi_pnt;		/* pnt to storage */
  struct pnt_info_st	*pi_next;		/* pnt to next */
} pnt_info_t;

static	pnt_info_t	*pointer_grid;

/* argument variables */
static	long		default_iter_n = DEFAULT_ITERATIONS; /* # of iters */
static	char		*env_string = NULL;		/* env options */
static	int		interactive_b = ARGV_FALSE;	/* interactive flag */
static	int		log_trans_b = ARGV_FALSE;	/* log transactions */
static	int		no_special_b = ARGV_FALSE;	/* no-special flag */
static	long		max_alloc = MAX_ALLOC;		/* amt of mem to use */
static	long		max_pointers = MAX_POINTERS;	/* # of pnts to use */
static	int		random_debug_b = ARGV_FALSE;	/* random flag */
static	int		silent_b = ARGV_FALSE;		/* silent flag */
static	unsigned int	seed_random = 0;		/* random seed */
static	int		verbose_b = ARGV_FALSE;		/* verbose flag */

static	argv_t		arg_list[] = {
  { INTER_CHAR,	"interactive",		ARGV_BOOL_INT,		&interactive_b,
    NULL,			"turn on interactive mode" },
  { 'e',	"env-string",		ARGV_CHAR_P,		&env_string,
    "string",			"string of env commands to set" },
  { 'l',	"log-trans",		ARGV_BOOL_INT,		&log_trans_b,
    NULL,			"log transactions via tracking-func" },
  { 'm',	"max-alloc",		ARGV_SIZE,		&max_alloc,
    "bytes",			"maximum allocation to test" },
  { 'n',	"no-special",		ARGV_BOOL_INT,		&no_special_b,
    NULL,			"do not run special tests" },
  { 'p',	"max-pointers",		ARGV_SIZE,		&max_pointers,
    "pointers",		"number of pointers to test" },
  { 'r',	"random-debug",		ARGV_BOOL_INT,	       &random_debug_b,
    NULL,			"randomly change debug flag" },
  { 's',	"silent",		ARGV_BOOL_INT,		&silent_b,
    NULL,			"do not display messages" },
  { 'S',	"seed-random",		ARGV_U_INT,		&seed_random,
    "number",			"seed for random function" },
  { 't',	"times",		ARGV_SIZE,	       &default_iter_n,
    "number",			"number of iterations to run" },
  { 'v',	"verbose",		ARGV_BOOL_INT,		&verbose_b,
    NULL,			"enables verbose messages" },
  { ARGV_LAST }
};

/*
 * Hexadecimal STR to integer translation
 */
static	long	hex_to_long(char *str)
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

/*
 * Read an address from the user
 */
static	void	*get_address(void)
{
  char	line[80];
  void	*pnt;
  
  do {
    (void)printf("Enter a hex address: ");
    if (fgets(line, sizeof(line), stdin) == NULL) {
      return NULL;
    }
  } while (line[0] == '\0');
  
  pnt = (void *)hex_to_long(line);
  
  return pnt;
}

/*
 * Free a slot from the used_p list and put it on the free list
 */
static	void	free_slot(const int iter_c, pnt_info_t *slot_p,
			  pnt_info_t **used_pp, pnt_info_t **free_pp)
{
  pnt_info_t	*this_p, *prev_p;
  
  if (verbose_b) {
    (void)printf("%d: free'd %d bytes from slot %d (%#lx)\n",
		 iter_c + 1, slot_p->pi_size, slot_p - pointer_grid,
		 (long)slot_p->pi_pnt);
  }
  
  slot_p->pi_pnt = NULL;
  
  /* find pnt in the used list */
  for (this_p = *used_pp, prev_p = NULL;
       this_p != NULL;
       prev_p = this_p, this_p = this_p->pi_next) {
    if (this_p == slot_p) {
      break;
    }
  }
  if (prev_p == NULL) {
    *used_pp = slot_p->pi_next;
  }
  else {
    prev_p->pi_next = slot_p->pi_next;
  }
  
  slot_p->pi_next = *free_pp;
  *free_pp = slot_p;
}

/*
 * Try ITER_N random program iterations, returns 1 on success else 0
 */
static	int	do_random(const int iter_n)
{
  unsigned int	old_flags = dmalloc_debug_current();
  unsigned int	flags;
  int		iter_c, prev_errno, amount, max_avail, free_c;
  int		final = 1;
  char		*chunk_p;
  pnt_info_t	*free_p, *used_p = NULL;
  pnt_info_t	*pnt_p;
  
  max_avail = max_alloc;
  
  flags = dmalloc_debug_current();
  
  pointer_grid = (pnt_info_t *)malloc(sizeof(pnt_info_t) * max_pointers);
  if (pointer_grid == NULL) {
    (void)printf("%s: problems allocating space for %ld pointer slots.\n",
		 argv_program, max_pointers);
    return 0;
  }
  
  /* initialize free list */
  free_p = pointer_grid;
  for (pnt_p = pointer_grid; pnt_p < pointer_grid + max_pointers; pnt_p++) {
    pnt_p->pi_size = 0;
    pnt_p->pi_pnt = NULL;
    pnt_p->pi_next = pnt_p + 1;
  }
  /* redo the last next pointer */
  (pnt_p - 1)->pi_next = NULL;
  free_c = max_pointers;
  
  prev_errno = ERROR_NONE;
  for (iter_c = 0; iter_c < iter_n;) {
    int		which_func, which;
    
    if (dmalloc_errno != prev_errno && ! silent_b) {
      (void)printf("ERROR: iter %d, %s (err %d)\n",
		   iter_c, dmalloc_strerror(dmalloc_errno), dmalloc_errno);
      prev_errno = dmalloc_errno;
      final = 0;
    }
    
    /* special case when doing non-linear stuff, sbrk took all memory */
    if (max_avail < MIN_AVAIL && free_p == NULL) {
      break;
    }
    
    if (random_debug_b) {
      unsigned int	new_flag;
      which = _dmalloc_rand() % (sizeof(int) * 8);
      new_flag = 1 << which;
      flags ^= new_flag;
      if (verbose_b) {
	(void)printf("%d: debug flags = %#x\n", iter_c + 1, flags);
      }
      dmalloc_debug(flags);
    }
    
    /* decide whether to malloc a new pointer or free/realloc an existing */
    which = _dmalloc_rand() % 4;
    
    if ((free_p == NULL
	 || which == 3
	 || max_avail < MIN_AVAIL
	 || free_c == max_pointers)
	&& used_p != NULL) {
      
      /* choose a random slot to free */
      which = _dmalloc_rand() % (max_pointers - free_c);
      for (pnt_p = used_p; which > 0; which--) {
	pnt_p = pnt_p->pi_next;
      }
      
      free(pnt_p->pi_pnt);
      free_slot(iter_c, pnt_p, &used_p, &free_p);
      free_c++;
      
      if (verbose_b) {
	(void)printf("%d: free'd %d bytes from slot %d (%#lx)\n",
		     iter_c + 1, pnt_p->pi_size, pnt_p - pointer_grid,
		     (long)pnt_p->pi_pnt);
      }
      
      max_avail += pnt_p->pi_size;
      iter_c++;
      continue;
    }
    
    /* sanity check */
    if (free_p == NULL) {
      (void)fprintf(stderr, "%s: problem with test program free list\n",
		    argv_program);
      exit(1);
    }
    
    /* rest are allocations */
    amount = _dmalloc_rand() % (max_avail / 2);
#if ALLOW_ALLOC_ZERO_SIZE == 0
    if (amount == 0) {
      amount = 1;
    }
#endif
    
    which_func = _dmalloc_rand() % 9;
    
    switch (which_func) {
      
      /* malloc */
    case 0:
      pnt_p = free_p;
      pnt_p->pi_pnt = malloc(amount);
      
      if (verbose_b) {
	(void)printf("%d: malloc %d of max %d into slot %d.  got %#lx\n",
		     iter_c + 1, amount, max_avail, pnt_p - pointer_grid,
		     (long)pnt_p->pi_pnt);
      }
      break;
      
      /* calloc */
    case 1:
      pnt_p = free_p;
      pnt_p->pi_pnt = calloc(amount, sizeof(char));
      
      if (verbose_b) {
	(void)printf("%d: calloc %d of max %d into slot %d.  got %#lx\n",
		     iter_c + 1, amount, max_avail, pnt_p - pointer_grid,
		     (long)pnt_p->pi_pnt);
      }
      
      /* test the returned block to make sure that is has been cleared */
      if (pnt_p->pi_pnt != NULL) {
	for (chunk_p = pnt_p->pi_pnt;
	     chunk_p < (char *)pnt_p->pi_pnt + amount;
	     chunk_p++) {
	  if (*chunk_p != '\0') {
	    if (! silent_b) {
	      (void)printf("calloc of %d was not fully zeroed on iteration #%d\n",
			   amount, iter_c + 1);
	    }
	    break;
	  }
	}
      }
      break;
      
      /* realloc */
    case 2:
      if (free_c == max_pointers) {
	continue;
      }

      which = _dmalloc_rand() % (max_pointers - free_c);
      for (pnt_p = used_p; which > 0; which--) {
	pnt_p = pnt_p->pi_next;
      }
      
      pnt_p->pi_pnt = realloc(pnt_p->pi_pnt, amount);
      /*
       * note that we've free the old size, we'll account for the
       * alloc below
       */
      max_avail += pnt_p->pi_size;
      
      if (verbose_b) {
	(void)printf("%d: realloc %d from %d of max %d slot %d.  got %#lx\n",
		     iter_c + 1, amount, pnt_p->pi_size, max_avail,
		     pnt_p - pointer_grid, (long)pnt_p->pi_pnt);
      }
      
      if (amount == 0) {
	free_slot(iter_c, pnt_p, &used_p, &free_p);
	free_c++;
	pnt_p = NULL;
	continue;
      }
      break;
      
      /* recalloc */
    case 3:
      if (free_c == max_pointers) {
	continue;
      }
      
      which = _dmalloc_rand() % (max_pointers - free_c);
      for (pnt_p = used_p; which > 0; which--) {
	pnt_p = pnt_p->pi_next;
      }
      
      pnt_p->pi_pnt = recalloc(pnt_p->pi_pnt, amount);
      /*
       * note that we've free the old size, we'll account for the
       * alloc below
       */
      max_avail += pnt_p->pi_size;
      
      if (verbose_b) {
	(void)printf("%d: recalloc %d from %d of max %d slot %d.  got %#lx\n",
		     iter_c + 1, amount, pnt_p->pi_size, max_avail,
		     pnt_p - pointer_grid, (long)pnt_p->pi_pnt);
      }
      
      /* test the returned block to make sure that is has been cleared */
      if (pnt_p->pi_pnt != NULL && amount > pnt_p->pi_size) {
	for (chunk_p = (char *)pnt_p->pi_pnt + pnt_p->pi_size;
	     chunk_p < (char *)pnt_p->pi_pnt + amount;
	     chunk_p++) {
	  if (*chunk_p != '\0') {
	    if (! silent_b) {
	      (void)printf("recalloc %d from %d was not fully zeroed on iteration #%d\n",
			   amount, pnt_p->pi_size, iter_c + 1);
	    }
	    break;
	  }
	}
      }
      
      if (amount == 0) {
	free_slot(iter_c, pnt_p, &used_p, &free_p);
	free_c++;
	pnt_p = NULL;
	continue;
      }
      break;
      
      /* valloc */
    case 4:
      pnt_p = free_p;
      pnt_p->pi_pnt = valloc(amount);
      
      if (verbose_b) {
	(void)printf("%d: valloc %d of max %d into slot %d.  got %#lx\n",
		     iter_c + 1, amount, max_avail, pnt_p - pointer_grid,
		     (long)pnt_p->pi_pnt);
      }
      break;
      
      /* sbrk */
    case 5:
      /* do it less often then the other functions */
      which = _dmalloc_rand() % 5;
      if (which == 3 && amount > 0) {
	void	*mem;
	
	mem = _dmalloc_heap_alloc(amount);
	if (verbose_b) {
	  (void)printf("%d: heap alloc %d of max %d bytes.  got %#lx\n",
		       iter_c + 1, amount, max_avail, (long)mem);
	}
	iter_c++;
      }
      /* don't store the memory */
      continue;
      break;
      
      /* heap check in the middle */
    case 6:
      /* do it less often then the other functions */
      which = _dmalloc_rand() % 20;
      if (which == 7) {
	if (dmalloc_verify(NULL /* check all heap */) != DMALLOC_NOERROR) {
	  if (! silent_b) {
	    (void)printf("%d: ERROR dmalloc_verify failed\n", iter_c + 1);
	  }
	  final = 0;
	}
	iter_c++;
      }
      continue;
      break;
      
#if HAVE_STRDUP
      /* strdup */
    case 7:
      {
	char	str[] = "this is a test of the emergency broadcasting system, the broadcasters in your area would like you to know that this system has really never been fully tested so we have no idea if it would actually work in the advent of a real disaster.";
	
	amount = _dmalloc_rand() % strlen(str);
	str[amount] = '\0';
	
	pnt_p = free_p;
	pnt_p->pi_pnt = strdup(str);
	
	if (verbose_b) {
	  /* the amount includes the \0 */
	  (void)printf("%d: strdup %d of max %d into slot %d.  got %#lx\n",
		       iter_c + 1, amount + 1, max_avail, pnt_p - pointer_grid,
		       (long)pnt_p->pi_pnt);
	}
      }
      break;
#endif
      
#if HAVE_STRNDUP
      /* strndup */
    case 8:
      {
	char	str[] = "this is a test of the emergency broadcasting system, the broadcasters in your area would like you to know that this system has really never been fully tested so we have no idea if it would actually work in the advent of a real disaster.";
	
	amount = _dmalloc_rand() % strlen(str);
	
	pnt_p = free_p;
	pnt_p->pi_pnt = strndup(str, amount);
	
	if (verbose_b) {
	  (void)printf("%d: strdup %d of max %d into slot %d.  got %#lx\n",
		       iter_c + 1, amount, max_avail, pnt_p - pointer_grid,
		       (long)pnt_p->pi_pnt);
	}
      }
      break;
#endif      
      
    default:
      continue;
      break;
    }
    
    if (pnt_p->pi_pnt == NULL) {
      if (! silent_b) {
	(void)printf("%d: ERROR allocation of %d returned error\n",
		     iter_c + 1, amount);
      }
      final = 0;
      iter_c++;
      continue;
    }
    
    /* set the size and take it off the free-list and put on used list */
    pnt_p->pi_size = amount;
    
    if (pnt_p == free_p) {
      free_p = pnt_p->pi_next;
      pnt_p->pi_next = used_p;
      used_p = pnt_p;
      free_c--;
    }
    
    max_avail -= amount;
    iter_c++;
    continue;
  }
  
  /* free used pointers */
  for (pnt_p = pointer_grid; pnt_p < pointer_grid + max_pointers; pnt_p++) {
    if (pnt_p->pi_pnt != NULL) {
      free(pnt_p->pi_pnt);
    }
  }
  
  free(pointer_grid);
  dmalloc_debug(old_flags);
  
  return final;
}

/*
 * Do some special tests as soon as we run the test program.  Returns
 * 1 on success else 0.
 */
static	int	check_initial_special(void)
{
  void	*pnt;
  int	final = 1, iter_c;
  
  /********************/
  
  do {
    int		amount;
    
    /*
     * So I ran across a bad check for the seen value versus the
     * iteration count.  It was only seen when there were a greater
     * number of reallocs to the same pointer at the start of the
     * program running.
     */
    
    if (! silent_b) {
      (void)printf("  Checking realloc(malloc) seen count\n");
    }
    
    do {
      /* NOTE: must be less than 1024 because below depends on this */
      amount = _dmalloc_rand() % 10;
    } while (amount == 0);
    pnt = malloc(amount);
    if (pnt == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not allocate %d bytes.\n", amount);
      }
      final = 0;
      break;
    }
    
    for (iter_c = 0; iter_c < 100; iter_c++) {
      
      /* change the amount */
      pnt = realloc(pnt, amount);
      if (pnt == NULL) {
	if (! silent_b) {
	  (void)printf("   ERROR: could not reallocate %d bytes.\n", amount);
	}
	final = 0;
	break;
      }
    }
    
    if (pnt == NULL) {
      break;
    }
    
    /*
     * now try freeing the pointer with a free that provides a return
     * value
     */
    if (dmalloc_free(__FILE__, __LINE__, pnt,
		     DMALLOC_FUNC_FREE) != FREE_NOERROR) {
      if (! silent_b) {
	(void)printf("   ERROR: free of realloc(malloc) pointer %lx failed.\n",
		     (unsigned long)pnt);
      }
      final = 0;
    }
    
    /* now check the heap to verify tha the freed slot is good */
    if (dmalloc_verify(NULL /* check all heap */) != DMALLOC_NOERROR) {
      if (! silent_b) {
	(void)printf("   ERROR: dmalloc_verify failed\n");
      }
      final = 0;
    }
  } while(0);
  
  /********************/
  
#define NEVER_REUSE_ITERS	20

  {
    void		*new_pnt, *pnts[NEVER_REUSE_ITERS];
    unsigned int	old_flags, amount, check_c;
    
    if (! silent_b) {
      (void)printf("  Checking never-reuse token\n");
    }
    
    old_flags = dmalloc_debug_current();
    dmalloc_debug(old_flags | DEBUG_NEVER_REUSE);
    
    for (iter_c = 0; iter_c < NEVER_REUSE_ITERS; iter_c++) {
      amount = 1024;
      pnts[iter_c] = malloc(amount);
      if (pnts[iter_c] == NULL) {
	if (! silent_b) {
	  (void)printf("   ERROR: could not allocate %d bytes.\n", amount);
	}
	final = 0;
	break;
      }
    }
    
    /* now free them */
    for (iter_c = 0; iter_c < NEVER_REUSE_ITERS; iter_c++) {
      if (dmalloc_free(__FILE__, __LINE__, pnts[iter_c],
		       DMALLOC_FUNC_FREE) != FREE_NOERROR) {
	if (! silent_b) {
	  (void)printf("   ERROR: free of pointer %lx failed.\n",
		       (unsigned long)pnts[iter_c]);
	}
	final = 0;
      }
    }
    
    /*
     * now allocate them again and make sure we don't get the same
     * pointers
     */
    for (iter_c = 0; iter_c < NEVER_REUSE_ITERS; iter_c++) {
      amount = 1024;
      new_pnt = malloc(amount);
      if (new_pnt == NULL) {
	if (! silent_b) {
	  (void)printf("   ERROR: could not allocate %d bytes.\n", amount);
	}
	final = 0;
      }
      
      /* did we get a previous pointer? */
      for (check_c = 0; check_c < NEVER_REUSE_ITERS; check_c++) {
	if (new_pnt == pnts[check_c]) {
	  if (! silent_b) {
	    (void)printf("   ERROR: pointer %lx was improperly reused.\n",
			 (unsigned long)new_pnt);
	  }
	  final = 0;
	  break;
	}
      }
      
      if (dmalloc_free(__FILE__, __LINE__, new_pnt,
		       DMALLOC_FUNC_FREE) != FREE_NOERROR) {
	if (! silent_b) {
	  (void)printf("   ERROR: free of pointer %lx failed.\n",
		       (unsigned long)new_pnt);
	}
	final = 0;
      }
    }
    
    dmalloc_debug(old_flags);
  }

  /********************/

  return final;
}

/*
 * Make sure that some of the arg check stuff works.
 */
static	int	check_arg_check(void)
{
  unsigned int	old_flags = dmalloc_debug_current();
  char		*func;
  int		our_errno_hold = dmalloc_errno;
  int		size, final = 1;
  char		*pnt, *pnt2, hold_ch;
  
  if (! silent_b) {
    (void)printf("  Checking arg-check functions\n");
  }
  
  /*
   * enable function checking and remove check-fence which caused
   * extra errors
   */
  dmalloc_debug((old_flags | DEBUG_CHECK_FUNCS) & (~DEBUG_CHECK_FENCE));
  
  size = 5;
  pnt = malloc(size);
  if (pnt == NULL) {
    if (! silent_b) {
      (void)printf("     ERROR: could not malloc %d bytes.\n", size);
    }
    return 0;
  }
  pnt2 = malloc(size * 2);
  if (pnt2 == NULL) {
    if (! silent_b) {
      (void)printf("     ERROR: could not malloc %d bytes.\n", size * 2);
    }
    return 0;
  }
  
  /*********/
  
#if HAVE_ATOI
  func = "atoi";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* they should be the same */
  dmalloc_errno = ERROR_NONE;
  memcpy(pnt, "1234", size);
  if (_dmalloc_atoi(__FILE__, __LINE__, pnt) != 1234) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  
  /* they should be different */
  dmalloc_errno = ERROR_NONE;
  memcpy(pnt, "12345", size);
  if (_dmalloc_atoi(__FILE__, __LINE__, pnt) != 12345) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should get error\n", func);
    }
    final = 0;
  }
#endif
  
  /*********/
  
#if HAVE_ATOL
  func = "atol";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* they should be the same */
  dmalloc_errno = ERROR_NONE;
  memcpy(pnt, "1234", size);
  if (_dmalloc_atol(__FILE__, __LINE__, pnt) != 1234) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  
  /* they should be different */
  dmalloc_errno = ERROR_NONE;
  memcpy(pnt, "12345", size);
  if (_dmalloc_atol(__FILE__, __LINE__, pnt) != 12345) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should get error\n", func);
    }
    final = 0;
  }
#endif
  
  /*********/
  
#if HAVE_BCMP
  func = "bcmp";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* they should be the same */
  dmalloc_errno = ERROR_NONE;
  memset(pnt, 1, size);
  memset(pnt2, 1, size);
  if (_dmalloc_bcmp(__FILE__, __LINE__, pnt, pnt2, size) != 0) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  
  /* they should be different */
  dmalloc_errno = ERROR_NONE;
  memset(pnt, 1, size);
  memset(pnt2, 2, size);
  if (_dmalloc_bcmp(__FILE__, __LINE__, pnt, pnt2, size) == 0) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  
  /* this should cause an error */
  dmalloc_errno = ERROR_NONE;
  _dmalloc_bcmp(__FILE__, __LINE__, pnt, pnt2, size + 1);
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload did not register overwrite\n",
		   func);
    }
    final = 0;
  }
#endif
  
  /*********/
  
#if HAVE_BCOPY
  func = "bcopy";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* this copies the right number of characters into buffer */
  dmalloc_errno = ERROR_NONE;
  memset(pnt, 2, size);
  _dmalloc_bcopy(__FILE__, __LINE__, pnt, pnt2, size);
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  /* check to see if it worked */
  if (memcmp(pnt, pnt2, size) != 0) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  
  /* this copies too many characters into buffer */
  dmalloc_errno = ERROR_NONE;
  memset(pnt, 2, size);
  _dmalloc_bcopy(__FILE__, __LINE__, pnt, pnt2, size + 1);
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should get error\n", func);
    }
    final = 0;
  }
#endif
  
  /*********/
  
#if HAVE_BZERO
  func = "bzero";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* this copies enough characters into buffer */
  dmalloc_errno = ERROR_NONE;
  _dmalloc_bzero(__FILE__, __LINE__, pnt, size);
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  
  /* this copies too many characters into buffer */
  dmalloc_errno = ERROR_NONE;
  hold_ch = *(pnt + size);
  _dmalloc_bzero(__FILE__, __LINE__, pnt, size + 1);
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should get error\n", func);
    }
    final = 0;
  }
  *(pnt + size) = hold_ch;
#endif
  
  /*********/
  
#if HAVE_INDEX
  func = "index";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* they should be the same */
  dmalloc_errno = ERROR_NONE;
  memcpy(pnt, "1234", size);
  if (_dmalloc_index(__FILE__, __LINE__, pnt, '4') != pnt + 3) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  
  /* they should be different */
  dmalloc_errno = ERROR_NONE;
  hold_ch = *(pnt + size);
  memcpy(pnt, "12345", size);
  if (_dmalloc_index(__FILE__, __LINE__, pnt, '5') != pnt + 4) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should get error\n", func);
    }
    final = 0;
  }
  *(pnt + size) = hold_ch;
#endif
  
  /*********/
  
#if HAVE_MEMCCPY
  func = "memccpy";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* this copies the right number of characters into buffer */
  dmalloc_errno = ERROR_NONE;
  memset(pnt, 3, size);
  _dmalloc_memccpy(__FILE__, __LINE__, pnt2, pnt, 0, size);
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  
  /* this copies too many characters into buffer */
  dmalloc_errno = ERROR_NONE;
  hold_ch = *(pnt + size);
  _dmalloc_memccpy(__FILE__, __LINE__, pnt2, pnt, 0, size + 1);
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should get error\n", func);
    }
    final = 0;
  }
  *(pnt + size) = hold_ch;
#endif
  
  /*********/
  
#if HAVE_MEMCHR
  func = "memchr";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* this looks at the right number of characters in buffer */
  dmalloc_errno = ERROR_NONE;
  memset(pnt, 4, size);
  if (_dmalloc_memchr(__FILE__, __LINE__, pnt, 0, size) != NULL) {
    if (! silent_b) {
      (void)printf("     ERROR: %s should have failed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  
  /* this looks at too many characters in buffer */
  dmalloc_errno = ERROR_NONE;
  (void)_dmalloc_memchr(__FILE__, __LINE__, pnt, 0, size + 1);
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should get error\n", func);
    }
    final = 0;
  }
#endif
  
  /*********/
  
#if HAVE_MEMCMP
  func = "memcmp";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* this checks the right number of characters in buffer */
  dmalloc_errno = ERROR_NONE;
  memset(pnt, 5, size);
  memset(pnt2, 5, size);
  if (_dmalloc_memcmp(__FILE__, __LINE__, pnt, pnt2, size) != 0) {
    if (! silent_b) {
      (void)printf("     ERROR: %s should have passed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  
  /* this checks too many characters from buffer */
  dmalloc_errno = ERROR_NONE;
  (void)_dmalloc_memcmp(__FILE__, __LINE__, pnt, pnt2, size + 1);
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should get error\n", func);
    }
    final = 0;
  }
#endif
  
  /*********/
  
#if HAVE_MEMCPY
  func = "memcpy";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* this copies enough characters into buffer */
  dmalloc_errno = ERROR_NONE;
  memset(pnt, 6, size);
  _dmalloc_memcpy(__FILE__, __LINE__, pnt2, pnt, size);
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  if (memcmp(pnt, pnt2, size) != 0) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  
  /* this copies too many characters into buffer */
  dmalloc_errno = ERROR_NONE;
  hold_ch = *(pnt + size);
  (void)_dmalloc_memcpy(__FILE__, __LINE__, pnt, pnt2, size + 1);
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should get error\n", func);
    }
    final = 0;
  }
  *(pnt + size) = hold_ch;
#endif
  
  /*********/
  
#if HAVE_MEMMOVE
  func = "memmove";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* this copies enough characters into buffer */
  dmalloc_errno = ERROR_NONE;
  memset(pnt, 7, size);
  _dmalloc_memmove(__FILE__, __LINE__, pnt2, pnt, size);
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  if (memcmp(pnt, pnt2, size) != 0) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  
  /* this copies too many characters into buffer */
  dmalloc_errno = ERROR_NONE;
  hold_ch = *(pnt + size);
  (void)_dmalloc_memmove(__FILE__, __LINE__, pnt, pnt2, size + 1);
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should get error\n", func);
    }
    final = 0;
  }
  *(pnt + size) = hold_ch;
#endif
  
  /*********/
  
#if HAVE_MEMSET
  func = "memset";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* this sets the right number of characters in buffer */
  dmalloc_errno = ERROR_NONE;
  _dmalloc_memset(__FILE__, __LINE__, pnt, 0, size);
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  
  /* this sets too many characters in buffer */
  dmalloc_errno = ERROR_NONE;
  hold_ch = *(pnt + size);
  _dmalloc_memset(__FILE__, __LINE__, pnt, 0, size + 1);
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should get error\n", func);
    }
    final = 0;
  }
  *(pnt + size) = hold_ch;
#endif
  
  /*********/
  
#if HAVE_RINDEX
  func = "rindex";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* they should be the same */
  dmalloc_errno = ERROR_NONE;
  memcpy(pnt, "1234", size);
  if (_dmalloc_rindex(__FILE__, __LINE__, pnt, '1') != pnt) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  
  /* they should be different */
  dmalloc_errno = ERROR_NONE;
  memcpy(pnt, "12345", size);
  if (_dmalloc_rindex(__FILE__, __LINE__, pnt, '1') != pnt) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should get error\n", func);
    }
    final = 0;
  }
#endif
  
  /*********/
  
#if HAVE_STRCASECMP
  func = "strcasecmp";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* they should be the same */
  dmalloc_errno = ERROR_NONE;
  memcpy(pnt, "abcd", size);
  memcpy(pnt2, "ABCD", size);
  if (_dmalloc_strcasecmp(__FILE__, __LINE__, pnt, pnt2) != 0) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  
  /* they should be different */
  dmalloc_errno = ERROR_NONE;
  memcpy(pnt, "abcde", size);
  memcpy(pnt2, "ABCDE", size);
  /* unknown results */
  (void)_dmalloc_strcasecmp(__FILE__, __LINE__, pnt, pnt2);
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should get error\n", func);
    }
    final = 0;
  }
#endif
  
  /*********/
  
#if HAVE_STRCAT
  func = "strcat";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* they should be the same */
  dmalloc_errno = ERROR_NONE;
  memcpy(pnt, "ab", 3);
  memcpy(pnt2, "cd", 3);
  if (_dmalloc_strcat(__FILE__, __LINE__, pnt, pnt2) != pnt) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  
  /* they should be different */
  dmalloc_errno = ERROR_NONE;
  memcpy(pnt, "ab", 3);
  memcpy(pnt2, "abc", 4);
  hold_ch = *(pnt + size);
  if (_dmalloc_strcat(__FILE__, __LINE__, pnt, pnt2) != pnt) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should get error\n", func);
    }
    final = 0;
  }
  *(pnt + size) = hold_ch;
#endif
  
  /*********/
  
#if HAVE_STRCHR
  func = "strchr";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* they should be the same */
  dmalloc_errno = ERROR_NONE;
  memcpy(pnt, "1234", size);
  if (_dmalloc_strchr(__FILE__, __LINE__, pnt, '4') != pnt + 3) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  
  /* they should be different */
  dmalloc_errno = ERROR_NONE;
  memcpy(pnt, "12345", size);
  if (_dmalloc_strchr(__FILE__, __LINE__, pnt, '5') != pnt + 4) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should get error\n", func);
    }
    final = 0;
  }
#endif
  
  /*********/
  
#if HAVE_STRCMP
  func = "strcmp";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* they should be the same */
  dmalloc_errno = ERROR_NONE;
  memcpy(pnt, "abcd", size);
  memcpy(pnt2, "abcd", size);
  if (_dmalloc_strcmp(__FILE__, __LINE__, pnt, pnt2) != 0) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  
  /* they should be different */
  dmalloc_errno = ERROR_NONE;
  memcpy(pnt, "abcde", size);
  memcpy(pnt2, "abcde", size);
  /* unknown results */
  (void)_dmalloc_strcmp(__FILE__, __LINE__, pnt, pnt2);
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should get error\n", func);
    }
    final = 0;
  }
#endif
  
  /*********/
  
#if HAVE_STRCPY
  func = "strcpy";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* they should be the same */
  dmalloc_errno = ERROR_NONE;
  memcpy(pnt2, "abcd", size);
  _dmalloc_strcpy(__FILE__, __LINE__, pnt, pnt2);
  if (memcmp(pnt, pnt2, size) != 0) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  
  /* they should be different */
  dmalloc_errno = ERROR_NONE;
  hold_ch = *(pnt + size);
  memcpy(pnt2, "abcde", size + 1);
  _dmalloc_strcpy(__FILE__, __LINE__, pnt, pnt2);
  if (memcmp(pnt, pnt2, size) != 0) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should get error\n", func);
    }
    final = 0;
  }
  *(pnt + size) = hold_ch;
#endif
  
  /*********/
  
#if HAVE_STRCSPN
  func = "strcspn";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* they should be the same */
  dmalloc_errno = ERROR_NONE;
  memcpy(pnt, "abcd", size);
  if (_dmalloc_strcspn(__FILE__, __LINE__, pnt, ".") != 4) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  
  /* they should be different */
  dmalloc_errno = ERROR_NONE;
  hold_ch = *(pnt + size);
  memcpy(pnt, "abcde", size);
  /* unknown results */
  (void)_dmalloc_strcspn(__FILE__, __LINE__, pnt, ".");
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should get error\n", func);
    }
    final = 0;
  }
  *(pnt + size) = hold_ch;
#endif
  
  /*********/
  
#if HAVE_STRDUP
#ifdef DMALLOC_STRDUP_MACRO
  func = "strdup";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  {
    char	*new_pnt;
    
    /* this copies characters into buffer */
    dmalloc_errno = ERROR_NONE;
    memset(pnt, 3, size);
    memset(pnt + size - 1, 0, 1);
    new_pnt = strdup(pnt);
    if (new_pnt == NULL) {
      if (! silent_b) {
	(void)printf("     ERROR: %s overload failed\n", func);
      }
      final = 0;
    }
    if (dmalloc_errno != ERROR_NONE) {
      if (! silent_b) {
	(void)printf("     ERROR: %s overload should not get error\n", func);
      }
      final = 0;
    }
    
    /* this looks at too many characters from buffer */
    dmalloc_errno = ERROR_NONE;
    memset(pnt, 3, size);
    new_pnt = strdup(pnt);
    if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
      if (! silent_b) {
	(void)printf("     ERROR: %s overload should get error\n", func);
      }
      final = 0;
    }
  }
#endif
#endif
  
  /*********/
  
#if HAVE_STRNCASECMP
  func = "strncasecmp";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* this compares enough characters from buffer */
  dmalloc_errno = ERROR_NONE;
  memset(pnt, 'a', size);
  memset(pnt2, 'A', size);
  if (_dmalloc_strncasecmp(__FILE__, __LINE__, pnt, pnt2, size) != 0) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  
  /* this compares too many characters from buffer */
  dmalloc_errno = ERROR_NONE;
  (void)_dmalloc_strncasecmp(__FILE__, __LINE__, pnt, pnt2, size + 1);
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should get error\n", func);
    }
    final = 0;
  }
#endif
  
  /*********/
  
#if HAVE_STRNCAT
  func = "strncat";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* this copies enough characters into buffer */
  dmalloc_errno = ERROR_NONE;
  /* sanity check */
  if (size <= 2) {
    abort();
  }
  memset(pnt, 8, size);
  /* remove 2 chars from end of pnt to fit 1 from pnt2 and \0 */
  pnt[size - 2] = '\0';
  memset(pnt2, 8, size);
  pnt2[1] = '\0';
  _dmalloc_strncat(__FILE__, __LINE__, pnt, pnt2, size);
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  
  /* this copies too many characters into buffer */
  dmalloc_errno = ERROR_NONE;
  memset(pnt, 8, size);
  /* now just remove one so the \0 would overwrite */
  pnt[size - 1] = '\0';
  hold_ch = *(pnt + size);
  _dmalloc_strncat(__FILE__, __LINE__, pnt, pnt2, size);
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should get error\n", func);
    }
    final = 0;
  }
  *(pnt + size) = hold_ch;
#endif
  
  /*********/
  
#if HAVE_STRNCMP
  func = "strncat";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* this compares too many characters from buffer */
  dmalloc_errno = ERROR_NONE;
  memset(pnt, 9, size);
  memset(pnt2, 9, size);
  if (_dmalloc_strncmp(__FILE__, __LINE__, pnt, pnt2, size) != 0) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  
  /* this compares too many characters from buffer */
  dmalloc_errno = ERROR_NONE;
  (void)_dmalloc_strncmp(__FILE__, __LINE__, pnt, pnt2, size + 1);
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should get error\n", func);
    }
    final = 0;
  }
#endif
  
  /*********/
  
#if HAVE_STRNCPY
  func = "strncpy";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  /* this looks at enough characters into buffer */
  dmalloc_errno = ERROR_NONE;
  memset(pnt, 9, size);
  _dmalloc_strncpy(__FILE__, __LINE__, pnt, pnt2, size);
  if (dmalloc_errno != ERROR_NONE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should not get error\n", func);
    }
    final = 0;
  }
  if (memcmp(pnt, pnt2, size) != 0) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload failed\n", func);
    }
    final = 0;
  }
  
  /* this copies too many characters into buffer */
  dmalloc_errno = ERROR_NONE;
  hold_ch = *(pnt + size);
  _dmalloc_strncpy(__FILE__, __LINE__, pnt, pnt2, size + 1);
  if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
    if (! silent_b) {
      (void)printf("     ERROR: %s overload should get error\n", func);
    }
    final = 0;
  }
  *(pnt + size) = hold_ch;
#endif
  
  /*********/
  
#if HAVE_STRNDUP
  func = "strndup";
  if (! silent_b) {
    (void)printf("    Checking %s\n", func);
  }
  
  {
    void	*new_pnt;
    
    /* this looks at enough characters in buffer */
    dmalloc_errno = ERROR_NONE;
    new_pnt = strndup(pnt, size);
    if (new_pnt == NULL) {
      if (! silent_b) {
	(void)printf("     ERROR: %s overload failed\n", func);
      }
      final = 0;
    }
    if (dmalloc_errno != ERROR_NONE) {
      if (! silent_b) {
	(void)printf("     ERROR: %s overload should not get error\n", func);
      }
      final = 0;
    }
    free(new_pnt);
      
    dmalloc_errno = ERROR_NONE;
    memset(pnt, 1, size);
    new_pnt = strndup(pnt, size + 1);
    if (new_pnt == NULL) {
      if (! silent_b) {
	(void)printf("     ERROR: strndup failed\n");
      }
      final = 0;
    }
    if (dmalloc_errno != ERROR_NONE) {
      if (! silent_b) {
	(void)printf("     ERROR: %s overload should not get error\n", func);
      }
      final = 0;
    }
    free(new_pnt);
  }
#endif
  
  /*********/
    
  free(pnt);
  free(pnt2);

  /* restore flags */
  dmalloc_debug(old_flags);
  dmalloc_errno = our_errno_hold;
  
  return final;
}

/*
 * Do some special tests, returns 1 on success else 0
 */
static	int	check_special(void)
{
  void	*pnt;
  int	page_size;
  int	final = 1;
  
  /* get our page size */
  page_size = dmalloc_page_size();
  
  dmalloc_message("-------------------------------------------------------\n");
  dmalloc_message("NOTE: ignore any errors until the next ------\n");
  
  /********************/

  /*
   * Check to make sure that we are handling free(0L) correctly.
   */
  {
    int	errno_hold = dmalloc_errno;
    
    if (! silent_b) {
      (void)printf("  Trying to free 0L pointer.\n");
    }
    free(NULL);
#if ALLOW_FREE_NULL
    if (dmalloc_errno != ERROR_NONE) {
      if (! silent_b) {
	(void)printf("   ERROR: free of 0L returned error.\n");
      }
      final = 0;
    }
#else
    if (dmalloc_errno == ERROR_NONE) {
      if (! silent_b) {
	(void)printf("   ERROR: free of 0L did not return error.\n");
      }
      final = 0;
    }
#endif
    
    /* now test the dmalloc_free function */
    if (dmalloc_free(__FILE__, __LINE__, NULL,
		     DMALLOC_FUNC_FREE) != FREE_ERROR) {
      if (! silent_b) {
	(void)printf("   ERROR: free of NULL should have failed.\n");
      }
      final = 0;
    }
    
    dmalloc_errno = errno_hold;
  }

  /********************/

  /*
   * Check to make sure that large mallocs are handled correctly.
   */
  
#if LARGEST_ALLOCATION
  {
    int	errno_hold = dmalloc_errno;
    
    if (! silent_b) {
      (void)printf("  Allocating a block of too-many bytes.\n");
    }
    pnt = malloc(LARGEST_ALLOCATION + 1);
    if (pnt == NULL) {
      dmalloc_errno = ERROR_NONE;
    }
    else {
      if (! silent_b) {
	(void)printf("   ERROR: allocation of > largest allowed size did not return error.\n");
      }
      free(pnt);
      final = 0;
    }
    
    dmalloc_errno = errno_hold;
  }
#endif
  
  /********************/
  
  /*
   * Check to see if overwritten freed memory is detected.
   */
  
  if (dmalloc_verify(NULL /* check all heap */) == DMALLOC_NOERROR) {
    int			iter_c, amount, where;
    int			errno_hold = dmalloc_errno;
    unsigned int	old_flags = dmalloc_debug_current();
    unsigned char	ch_hold;
    
    dmalloc_debug(old_flags | DEBUG_FREE_BLANK);
    
    if (! silent_b) {
      (void)printf("  Overwriting free memory.\n");
    }
    
    for (iter_c = 0; iter_c < 20; iter_c++) {
      do {
	amount = _dmalloc_rand() % (page_size * 3);
      } while (amount == 0);
      pnt = malloc(amount);
      if (pnt == NULL) {
	if (! silent_b) {
	  (void)printf("   ERROR: could not allocate %d bytes.\n", amount);
	}
	final = 0;
	continue;
      }
      free(pnt);
      
      /* find out where overwrite inside of the pointer */
      where = _dmalloc_rand() % amount;
      ch_hold = *((char *)pnt + where);
      *((char *)pnt + where) = 'h';
      
      /* now verify that the pnt and the whole heap register errors */
      dmalloc_errno = ERROR_NONE;      
      if (dmalloc_verify(pnt) == DMALLOC_NOERROR
	  || dmalloc_verify(NULL /* check all heap */) == DMALLOC_NOERROR) {
	if (! silent_b) {
	  (void)printf("   ERROR: overwriting free memory not detected.\n");
	}
	final = 0;
      }
      else if (dmalloc_errno == ERROR_FREE_OVERWRITTEN) {
      }
      else {
	if (! silent_b) {
	  (void)printf("   ERROR: verify of overwritten memory returned: %s (err %d)\n",
		       dmalloc_strerror(dmalloc_errno), dmalloc_errno);
	}
	final = 0;
      }
      *((char *)pnt + where) = ch_hold;
    }
    
    dmalloc_debug(old_flags);
    dmalloc_errno = errno_hold;
  }
  
  /********************/
  
  /*
   * Check to see if the space above an allocated pnt is detected.
   */
  
  {
    int			iter_c, amount, where;
    int			errno_hold = dmalloc_errno;
    unsigned int	old_flags;
    DMALLOC_SIZE	tot_size;
    unsigned char	ch_hold;
    
    old_flags = dmalloc_debug_current();
    
    /* sure on free-blank on and check-fence off */
    dmalloc_debug((old_flags | DEBUG_ALLOC_BLANK) & (~DEBUG_CHECK_FENCE));
    
    if (! silent_b) {
      (void)printf("  Overwriting memory above allocation.\n");
    }
    
    for (iter_c = 0; iter_c < 20; /* iter_c ++ below */) {
      do {
	amount = _dmalloc_rand() % (page_size * 3);
      } while (amount == 0);
      pnt = malloc(amount);
      if (pnt == NULL) {
	if (! silent_b) {
	  (void)printf("   ERROR: could not allocate %d bytes.\n", amount);
	}
	final = 0;
	continue;
      }
      /*
       * check out the pointer now to make sure that we have some
       * space above the pointer
       */
      if (dmalloc_examine(pnt, NULL /* now user size */, &tot_size,
			  NULL /* no file */, NULL /* no line */,
			  NULL /* no return address */, NULL /* no mark */,
			  NULL /* no seen */) != DMALLOC_NOERROR) {
	if (! silent_b) {
	  (void)printf("   ERROR: examining pointer %lx failed.\n",
		       (unsigned long)pnt);
	}
	final = 0;
	break;
      }
      if (tot_size == amount) {
	/* we need some space to overwrite */
	free(pnt);
	continue;
      }
      /* now we can increment */ 
      iter_c++;
      
      /* where to overwrite is then a random from 0 to the remainder-1 */
      where = _dmalloc_rand() % (tot_size - amount);
      ch_hold = *((char *)pnt + amount + where);
      *((char *)pnt + amount + where) = 'h';
      
      /* now verify that the pnt and the whole heap register errors */
      dmalloc_errno = ERROR_NONE;
      if (dmalloc_verify(pnt) == DMALLOC_NOERROR
	  || dmalloc_verify(NULL /* check all heap */) == DMALLOC_NOERROR) {
	if (! silent_b) {
	  (void)printf("   ERROR: overwriting above allocated memory not detected.\n");
	}
	final = 0;
      }
      else if (dmalloc_errno == ERROR_FREE_OVERWRITTEN) {
      }
      else {
	if (! silent_b) {
	  (void)printf("   ERROR: verify of overwritten above allocated memory returned: %s (err %d)\n",
		       dmalloc_strerror(dmalloc_errno), dmalloc_errno);
	}
	final = 0;
      }
      *((char *)pnt + amount + where) = ch_hold;
      free(pnt);
    }
    
    dmalloc_debug(old_flags);
    dmalloc_errno = errno_hold;
  }
  
  /********************/
  
  /*
   * See if we can free invalid pointers and get the appropriate errors
   */
  
  {
    int	errno_hold = dmalloc_errno;
    int	iter_c, amount, wrong;
    
    if (! silent_b) {
      (void)printf("  Freeing invalid pointers\n");
    }
    
    for (iter_c = 0; iter_c < 20; iter_c++) {
      do {
	amount = _dmalloc_rand() % (page_size * 3);
      } while (amount == 0);
      pnt = malloc(amount);
      if (pnt == NULL) {
	if (! silent_b) {
	  (void)printf("   ERROR: could not allocate %d bytes.\n", amount);
	}
	final = 0;
	continue;
      }
      wrong = _dmalloc_rand() % amount;
      if (wrong == 0) {
	wrong = 1;
      }
      if (dmalloc_free(__FILE__, __LINE__, (char *)pnt + wrong,
		       DMALLOC_FUNC_FREE) != FREE_NOERROR) {
	if (dmalloc_errno == ERROR_NOT_START_BLOCK) {
	}
	else {
	  if (! silent_b) {
	    (void)printf("   ERROR: free bad pointer produced: %s (err %d)\n",
			 dmalloc_strerror(dmalloc_errno), dmalloc_errno);
	  }
	  final = 0;
	}
      }
      else {
	if (! silent_b) {
	  (void)printf("   ERROR: no problem freeing bad pointer.\n");
	}
	final = 0;
      }
      free(pnt);
    }
    dmalloc_errno = errno_hold;
  }
  
  /********************/

  /*
   * Saw a number of problems where the used_iter value was not being
   * set and the proper flags were not being set on the slots.
   */
  
  {
    char		*loc_file, *ex_file;
    void		*new_pnt;
    int			errno_hold = dmalloc_errno;
    unsigned int	amount, loc_line, ex_line, old_flags;
    unsigned long	loc_mark, ex_mark, old_seen, ex_seen;
    DMALLOC_SIZE	ex_user_size, ex_tot_size;
    int			iter_c;
    
    if (! silent_b) {
      (void)printf("  Checking dmalloc_examine information\n");
    }
    
    old_flags = dmalloc_debug_current();
    /*
     * We have to turn off the realloc-copy and new-reuse flags
     * otherwise this won't work.
     */
    dmalloc_debug(old_flags & ~DEBUG_REALLOC_COPY & ~DEBUG_NEVER_REUSE);
    
    for (iter_c = 0; iter_c < 20; iter_c++) {
      do {
	amount = _dmalloc_rand() % (page_size * 3);
	/* we need 2 because we are doing a 2-1 below */
      } while (amount < 2);
      pnt = malloc(amount); loc_file = __FILE__; loc_line = __LINE__;
      if (pnt == NULL) {
	if (! silent_b) {
	  (void)printf("   ERROR: could not allocate %d bytes.\n", amount);
	}
	final = 0;
	continue;
      }
      /* record the mark */
      loc_mark = dmalloc_mark();
      
      /* check out the pointer */
      if (dmalloc_examine(pnt, &ex_user_size, &ex_tot_size, &ex_file, &ex_line,
			  NULL /* no return address */, &ex_mark,
			  &ex_seen) != DMALLOC_NOERROR) {
	if (! silent_b) {
	  (void)printf("   ERROR: examining pointer %lx failed.\n",
		       (unsigned long)pnt);
	}
	final = 0;
      }
      else if (ex_user_size != amount
	       || ex_file == NULL
	       || strcmp(ex_file, loc_file) != 0
	       || ex_line != loc_line
	       || ex_mark <= 0
	       || ex_mark != loc_mark
	       || ex_tot_size < ex_user_size
#if LOG_PNT_SEEN_COUNT
	       || ex_seen < 1
#endif
	       ) {
	if (! silent_b) {
	  (void)printf("   ERROR: examined pointer info invalid.\n");
	}
	final = 0;
      }
      old_seen = ex_seen;
      
      /*
       * Now realloc the pointer again and make sure that the mark and
       * the seen increment by 1.  We decrement instead of
       * incrementing because the library will never reposition an
       * allocation if shrinking.
       */
      new_pnt = realloc(pnt, amount - 1);
      if (new_pnt == NULL) {
	if (! silent_b) {
	  (void)printf("   ERROR: could not reallocate %d bytes.\n",
		       amount + 1);
	}
	final = 0;
	continue;
      }
      
      /* should always get a new pointer */
      if (new_pnt != pnt) {
	if (! silent_b) {
	  (void)printf("   ERROR: could not reallocate %d bytes.\n",
		       amount + 1);
	}
	final = 0;
	continue;
      }
      
      /* check out the pointer */
      if (dmalloc_examine(pnt, &ex_user_size, NULL, NULL, NULL,
			  NULL, &ex_mark, &ex_seen) != DMALLOC_NOERROR) {
	if (! silent_b) {
	  (void)printf("   ERROR: examining pointer %lx failed.\n",
		       (unsigned long)pnt);
	}
	final = 0;
      }
      else if (ex_user_size != amount - 1
	       /* +2 on the mark because of the examine */
	       || ex_mark != loc_mark + 2
#if LOG_PNT_SEEN_COUNT
	       /* +2 on seen because realloc counts it in and out */
	       || ex_seen != old_seen + 2
#endif
	       ) {
	if (! silent_b) {
	  (void)printf("   ERROR: examined realloced pointer info invalid.\n");
	}
	final = 0;
      }
      
      free(pnt);
      
      /* this should fail now that we freed the pointer */
      if (dmalloc_examine(pnt, NULL, NULL, NULL, NULL, NULL, NULL,
			  NULL) != DMALLOC_ERROR) {
	if (! silent_b) {
	  (void)printf("   ERROR: examining freed pointer %lx did not fail.\n",
		       (unsigned long)pnt);
	}
	final = 0;
      }
    }
    
    dmalloc_debug(old_flags);
    dmalloc_errno = errno_hold;
  }
  
  /********************/
  
  /*
   * Make sure that the start file:line works.
   */
  {
    unsigned int	old_flags = dmalloc_debug_current();
    int			errno_hold = dmalloc_errno;
    char		*loc_file, save_ch;
    int			iter_c, loc_line;
    void		*pnts[2];
    char		setup[128];
    
    /* turn on fence post checking */
    dmalloc_debug(DEBUG_CHECK_FENCE);
    dmalloc_errno = ERROR_NONE;
    
    if (! silent_b) {
      (void)printf("  Checking heap check start at file:line\n");
    }
    
#define BUF_SIZE	64
    
    /* make an allocation */
    pnts[0] = malloc(BUF_SIZE);
    if (pnts[0] == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not malloc %d bytes.\n", BLOCK_SIZE);
      }
      return 0;
    }
    
    /* save the character but then overwrite the high fence post */
    save_ch = *((char *)pnts[0] + BUF_SIZE);
    *((char *)pnts[0] + BUF_SIZE) = '\0';
    
    /* make another allocation */
    pnts[1] = malloc(BUF_SIZE);
    if (pnts[1] == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not malloc %d bytes.\n", BLOCK_SIZE);
      }
      return 0;
    }
    
    /* it shouldn't generate an error */
    if (dmalloc_errno != ERROR_NONE) {
      if (! silent_b) {
	(void)printf("   ERROR: should not have gotten an error with no heap checking enabled.\n");
      }
      return 0;
    }
    
    /* restore the overwritten character otherwise we can't free pointer */
    *((char *)pnts[0] + BUF_SIZE) = save_ch;
    free(pnts[0]);
    free(pnts[1]);
    
    for (iter_c = 0; iter_c < 2; iter_c++) {
      /*
       * we have to do this loop hack here because we need to know the
       * __FILE__ and __LINE__ of a certain location to set the
       * variable so then we need to run it again.
       */
      
      /*
       * Make an allocation recording where we did it.
       *
       * NOTE: This all needs to be on the same line.
       */
      loc_file= __FILE__; loc_line = __LINE__; pnts[iter_c] = malloc(BUF_SIZE);
      if (pnts[iter_c] == NULL) {
	if (! silent_b) {
	  (void)printf("   ERROR: could not malloc %d bytes.\n", BLOCK_SIZE);
	}
	return 0;
      }
      
      /* first time through the loop? */
      if (iter_c == 0) {
	/* save the character and then overwrite the high fence post */
	save_ch = *((char *)pnts[0] + BUF_SIZE);
	*((char *)pnts[0] + BUF_SIZE) = '\0';
	
	/*
	 * build and enable an options string turning on checking at
	 * the above allocation
	 */
	(void)loc_snprintf(setup, sizeof(setup), "debug=%#x,start=%s:%d",
			   DEBUG_CHECK_FENCE, loc_file, loc_line);
	dmalloc_debug_setup(setup);	
	continue;
      }
      
      /*
       * now the 2nd time through the loop we should have seen an
       * error because heap checking should have been enabled at the
       * 2nd allocation so the heap should have been checked and the
       * problem with the 1st allocation detected.
       */
      if (dmalloc_errno != ERROR_OVER_FENCE) {
	if (! silent_b) {
	  (void)printf("   ERROR: should have gotten over fence-post error after checking started.\n");
	}
	return 0;
      }
      
      /*
       * restore the overwritten character otherwise we can't free
       * the pointer
       */
      *((char *)pnts[0] + BUF_SIZE) = save_ch;
    }
    
    free(pnts[0]);
    free(pnts[1]);
    
    /* reset the debug flags and errno */
    dmalloc_debug(old_flags);
    dmalloc_errno = errno_hold;
  }
  
  /********************/
  
  /*
   * Make sure that the start iteration count works.
   */
  {
    unsigned int	old_flags = dmalloc_debug_current();
    int			errno_hold = dmalloc_errno;
    char		save_ch;
    void		*pnt2;
    char		setup[128];
    
    /* turn on fence post checking */
    dmalloc_debug(DEBUG_CHECK_FENCE);
    dmalloc_errno = ERROR_NONE;
    
    if (! silent_b) {
      (void)printf("  Checking heap check start at iteration count\n");
    }
    
#define BUF_SIZE	64
    
    /* make an allocation */
    pnt = malloc(BUF_SIZE);
    if (pnt == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not malloc %d bytes.\n", BLOCK_SIZE);
      }
      return 0;
    }
    
    /* overwrite the high fence post */
    save_ch = *((char *)pnt + BUF_SIZE);
    *((char *)pnt + BUF_SIZE) = '\0';
    
    /* make another allocation */
    pnt2 = malloc(BUF_SIZE);
    if (pnt2 == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not malloc %d bytes.\n", BLOCK_SIZE);
      }
      return 0;
    }
    
    /* it shouldn't generate an error */
    if (dmalloc_errno != ERROR_NONE) {
      if (! silent_b) {
	(void)printf("   ERROR: should not have gotten an error with no heap checking enabled.\n");
      }
      return 0;
    }
    
    /* free the 2nd pointer */
    free(pnt2);
    
    /*
     * build and enable an options string turning on checking at the
     * next transaction
     */
    (void)loc_snprintf(setup, sizeof(setup), "debug=%#x,start=c1",
		       DEBUG_CHECK_FENCE);
    dmalloc_debug_setup(setup);	
    
    /*
     * make another allocation which should enable heap checking and
     * notice the above pointer overwrite
     */
    pnt2 = malloc(BUF_SIZE);
    if (pnt2 == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not malloc %d bytes.\n", BLOCK_SIZE);
      }
      return 0;
    }
    
    /* now we should see the error */
    if (dmalloc_errno != ERROR_OVER_FENCE) {
      if (! silent_b) {
	(void)printf("   ERROR: should have gotten over fence-post error after checking started.\n");
      }
      return 0;
    }
    
    /* restore the overwritten character otherwise we can't free the pointer */
    *((char *)pnt + BUF_SIZE) = save_ch;
    free(pnt);
    free(pnt2);
    
    /* reset the debug flags and errno */
    dmalloc_debug(old_flags);
    dmalloc_errno = errno_hold;
  }
  
  /********************/
  
  /*
   * Make sure that the start after memory size allocated.
   */
  {
    unsigned int	old_flags = dmalloc_debug_current();
    int			errno_hold = dmalloc_errno;
    char		save_ch;
    void		*pnt2;
    char		setup[128];
    
    /* turn on fence post checking */
    dmalloc_debug(DEBUG_CHECK_FENCE);
    dmalloc_errno = ERROR_NONE;
    
    if (! silent_b) {
      (void)printf("  Checking heap check start at memory size\n");
    }
    
#define BUF_SIZE	64
    
    /* make an allocation */
    pnt = malloc(BUF_SIZE);
    if (pnt == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not malloc %d bytes.\n", BLOCK_SIZE);
      }
      return 0;
    }
    
    /* overwrite the high fence post */
    save_ch = *((char *)pnt + BUF_SIZE);
    *((char *)pnt + BUF_SIZE) = '\0';
    
    /* make another allocation */
    pnt2 = malloc(BUF_SIZE);
    if (pnt2 == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not malloc %d bytes.\n", BLOCK_SIZE);
      }
      return 0;
    }
    
    /* it shouldn't generate an error */
    if (dmalloc_errno != ERROR_NONE) {
      if (! silent_b) {
	(void)printf("   ERROR: should not have gotten an error with no heap checking enabled.\n");
      }
      return 0;
    }
    
    free(pnt2);
    
    /*
     * build and enable an options string turning on checking at the
     * next transaction
     */
    (void)loc_snprintf(setup, sizeof(setup), "debug=%#x,start=s%lu",
		       DEBUG_CHECK_FENCE, dmalloc_memory_allocated());
    dmalloc_debug_setup(setup);	
    
    /*
     * make another allocation which should enable heap checking and
     * notice the above pointer overwrite
     */
    pnt2 = malloc(BUF_SIZE);
    if (pnt2 == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not malloc %d bytes.\n", BLOCK_SIZE);
      }
      return 0;
    }
    
    /* now we should see the error */
    if (dmalloc_errno != ERROR_OVER_FENCE) {
      if (! silent_b) {
	(void)printf("   ERROR: should have gotten over fence-post error after checking started.\n");
      }
      return 0;
    }
    
    /* restore the overwritten character otherwise we can't free the pointer */
    *((char *)pnt + BUF_SIZE) = save_ch;
    free(pnt);
    free(pnt2);
    
    /* reset the debug flags and errno */
    dmalloc_debug(old_flags);
    dmalloc_errno = errno_hold;
  }
  
  /********************/
 
  /*
   * Make sure per-pointer blanking flags work.
   */
  {
    int			errno_hold = dmalloc_errno;
    unsigned long	size;
    unsigned int	old_flags = dmalloc_debug_current();
    char		save_ch;
    
    if (! silent_b) {
      (void)printf("  Checking per-pointer blanking flags\n");
    }
    
    /* disable alloc and check blanking */ 
    dmalloc_debug(old_flags & (~(DEBUG_ALLOC_BLANK | DEBUG_CHECK_BLANK)));
    
    /* allocate a pointer */
    size = _dmalloc_rand() % MAX_ALLOC + 10;
    pnt = malloc(size);
    if (pnt == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not malloc %lu bytes.\n", size);
      }
      return 0;
    }
    
    /* now enable alloc and check blanking */ 
    dmalloc_debug(old_flags | DEBUG_ALLOC_BLANK | DEBUG_CHECK_BLANK);
    
    if (dmalloc_free(__FILE__, __LINE__, pnt,
		     DMALLOC_FUNC_FREE) != FREE_NOERROR) {
      if (! silent_b) {
	(void)printf("   ERROR: per-pointer blanking flags failed: %s (err %d)\n",
		     dmalloc_strerror(dmalloc_errno), dmalloc_errno);
      }
      final = 0;
    }
    
    /********/
    
    /* now, enable alloc and check blanking */ 
    dmalloc_debug((old_flags | DEBUG_ALLOC_BLANK | DEBUG_CHECK_BLANK)
		  & (~DEBUG_CHECK_FENCE));
    
    /* allocate a pointer */
    size = BLOCK_SIZE / 2 + 1;
    pnt = malloc(size);
    if (pnt == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not malloc %lu bytes.\n", size);
      }
      return 0;
    }
    
    /* now disable alloc blanking */ 
    dmalloc_debug(old_flags | DEBUG_CHECK_BLANK);
    
    /* overwrite one of the top chars */
    save_ch = *((char *)pnt + size);
    *((char *)pnt + size) = '\0';
    
    /* free the pointer should still see the error */
    if (dmalloc_free(__FILE__, __LINE__, pnt,
		     DMALLOC_FUNC_FREE) == FREE_NOERROR) {
      if (! silent_b) {
	(void)printf("   ERROR: per-pointer blanking flags should have failed: %s (err %d)\n",
		     dmalloc_strerror(dmalloc_errno), dmalloc_errno);
      }
      final = 0;
    }
    
    /* restore the overwrite */
    *((char *)pnt + size) = save_ch;
    
    /* restore flags */
    dmalloc_debug(old_flags);
    dmalloc_errno = errno_hold;
  }
  
  /********************/
  
  /*
   * Make sure that a pointer reallocating, get's the per-pointer
   * alloc flags enabled.
   */
  {
    int			errno_hold = dmalloc_errno;
    unsigned long	size;
    unsigned int	old_flags = dmalloc_debug_current();
    char		save_ch;
    
    if (! silent_b) {
      (void)printf("  Checking per-pointer alloc flags and realloc\n");
    }
    
    /* enable alloc blanking without fence-posts */
    dmalloc_debug((old_flags | DEBUG_ALLOC_BLANK | DEBUG_FREE_BLANK)
		  & (~(DEBUG_CHECK_FENCE | DEBUG_CHECK_BLANK)));
    
    /* allocate a pointer that should fill the block */
    size = BLOCK_SIZE;
    pnt = malloc(size);
    if (pnt == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not malloc %lu bytes.\n", size);
      }
      return 0;
    }
    
    /* now disable all checking */
    dmalloc_debug((old_flags | DEBUG_CHECK_BLANK)
		  & (~(DEBUG_CHECK_FENCE
		       | DEBUG_ALLOC_BLANK | DEBUG_FREE_BLANK)));
    
    pnt = realloc(pnt, size - 1);
    if (pnt == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not realloc %#lx to %lu bytes.\n",
		     (long)pnt, size - 1);
      }
      return 0;
    }
    
    /* overwrite one of the top chars */
    save_ch = *((char *)pnt + size - 1);
    *((char *)pnt + size - 1) = '\0';
    
    /* we should notice the overwrite */
    if (dmalloc_free(__FILE__, __LINE__, pnt,
		     DMALLOC_FUNC_FREE) == FREE_NOERROR) {
      if (! silent_b) {
	(void)printf("   ERROR: per-pointer alloc flags should have failed: %s (err %d)\n",
		     dmalloc_strerror(dmalloc_errno), dmalloc_errno);
      }
      final = 0;
    }
    
    /* restore the overwrite */
    *((char *)pnt + size - 1) = save_ch;
    
    /* restore flags */
    dmalloc_debug(old_flags);
    dmalloc_errno = errno_hold;
  }
  
  /********************/
  
  /*
   * Make sure that a free of an existing pointer gets the right error.
   */
  {
    int		errno_hold = dmalloc_errno;
    int		size = 10;
    
    if (! silent_b) {
      (void)printf("  Checking double free error\n");
    }
    
    pnt = malloc(size);
    if (pnt == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not malloc %d bytes.\n", size);
      }
      return 0;
    }
    
    if (dmalloc_free(__FILE__, __LINE__, pnt,
		     DMALLOC_FUNC_FREE) != FREE_NOERROR) {
      if (! silent_b) {
	(void)printf("   ERROR: 1st of double free should not fail: %s (err %d)\n",
		     dmalloc_strerror(dmalloc_errno), dmalloc_errno);
      }
      final = 0;
    }
    
    dmalloc_errno = ERROR_NONE;
    if (dmalloc_free(__FILE__, __LINE__, pnt,
		     DMALLOC_FUNC_FREE) == FREE_NOERROR) {
      if (! silent_b) {
	(void)printf("   ERROR: 2nd of double free should have failed: %s (err %d)\n",
		     dmalloc_strerror(dmalloc_errno), dmalloc_errno);
      }
      final = 0;
    }
    else if (dmalloc_errno != ERROR_ALREADY_FREE) {
      if (! silent_b) {
	(void)printf("   ERROR: 2nd of double free should get ERROR_ALREADY_FREE not: %s (err %d)\n",
		     dmalloc_strerror(dmalloc_errno), dmalloc_errno);
      }
      final = 0;
    }
    
    dmalloc_errno = errno_hold;
  }
  
  /********************/
  
  /*
   * Coverage tests
   */
#if FREED_POINTER_DELAY > 0
  {
    int		errno_hold = dmalloc_errno;
    int		size = 10, pnt_c;
    
    if (! silent_b) {
      (void)printf("  Checking freed pointer delay\n");
    }
    
    pnt = malloc(size);
    if (pnt == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not malloc %d bytes.\n", size);
      }
      return 0;
    }
    for (pnt_c = 0; pnt_c < FREED_POINTER_DELAY; pnt_c++) {
      void	*pnt2 = malloc(size);
      if (pnt2 == NULL) {
	if (! silent_b) {
	  (void)printf("   ERROR: could not malloc %d bytes.\n", size);
	}
	return 0;
      }
      free(pnt2);
    }
    
    /* now double free the first one */
    dmalloc_errno = ERROR_NONE;
    if (dmalloc_free(__FILE__, __LINE__, pnt,
		     DMALLOC_FUNC_FREE) != FREE_NOERROR) {
      if (! silent_b) {
	(void)printf("   ERROR: 1st of double free should not fail: %s (err %d)\n",
		     dmalloc_strerror(dmalloc_errno), dmalloc_errno);
      }
      final = 0;
    }
    
    if (dmalloc_free(__FILE__, __LINE__, pnt,
		     DMALLOC_FUNC_FREE) == FREE_NOERROR) {
      if (! silent_b) {
	(void)printf("   ERROR: 2nd of double free should have failed: %s (err %d)\n",
		     dmalloc_strerror(dmalloc_errno), dmalloc_errno);
      }
      final = 0;
    }
    else if (dmalloc_errno != ERROR_ALREADY_FREE) {
      if (! silent_b) {
	(void)printf("   ERROR: 2nd of double free should get ERROR_ALREADY_FREE not: %s (err %d)\n",
		     dmalloc_strerror(dmalloc_errno), dmalloc_errno);
      }
      final = 0;
    }
    
    dmalloc_errno = errno_hold;
  }
#endif
  
  /********************/

  /*
   * Test strndup.
   */
  {
    int		errno_hold = dmalloc_errno;
    int		size = 5;
    char	*str, *ret;
    unsigned int old_flags = dmalloc_debug_current();
  
    if (! silent_b) {
      (void)printf("  Checking strndup\n");
    }
    
    dmalloc_debug(old_flags | DEBUG_CHECK_FUNCS);
    
    pnt = malloc(size);
    if (pnt == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not malloc %d bytes.\n", size);
      }
      return 0;
    }
    str = "1234";
    memmove(pnt, str, size);
    
    dmalloc_errno = ERROR_NONE;
    ret = strndup(pnt, size);
    
    if (dmalloc_errno != ERROR_NONE) {
      if (! silent_b) {
	(void)printf("   ERROR: strndup shouldn't produce error: %s (err %d)\n",
		     dmalloc_strerror(dmalloc_errno), dmalloc_errno);
      }
      final = 0;
    } else if (strcmp(pnt, str) != 0) {
      if (! silent_b) {
	(void)printf("   ERROR: strndup should have copied string\n");
      }
      final = 0;
    }
    
    dmalloc_errno = ERROR_NONE;
    ret = strndup(pnt, size + 1);
    
    if (dmalloc_errno != ERROR_NONE) {
      if (! silent_b) {
	(void)printf("   ERROR: strndup shouldn't produce error: %s (err %d)\n",
		     dmalloc_strerror(dmalloc_errno), dmalloc_errno);
      }
      final = 0;
    } else if (strcmp(pnt, str) != 0) {
      if (! silent_b) {
	(void)printf("   ERROR: strndup should have copied string\n");
      }
      final = 0;
    }
    
    str = "12345";
    memmove(pnt, str, size);
    
    dmalloc_errno = ERROR_NONE;
    strndup(pnt, size);
    
    if (dmalloc_errno != ERROR_NONE) {
      if (! silent_b) {
	(void)printf("   ERROR: strndup shouldn't produce error: %s (err %d)\n",
		     dmalloc_strerror(dmalloc_errno), dmalloc_errno);
      }
      final = 0;
    } else if (strcmp(pnt, str) != 0) {
      if (! silent_b) {
	(void)printf("   ERROR: strndup should have copied string\n");
      }
      final = 0;
    }

    dmalloc_errno = ERROR_NONE;
    strndup(pnt, size + 1);
    
    if (dmalloc_errno != ERROR_WOULD_OVERWRITE) {
      if (! silent_b) {
	(void)printf("   ERROR: strndup should produce overwrite error not: %s (err %d)\n",
		     dmalloc_strerror(dmalloc_errno), dmalloc_errno);
      }
      final = 0;
    }
    
    dmalloc_debug(old_flags);
    dmalloc_errno = errno_hold;
  }
  
  /********************/
  
  /* check all of the arg check routines */
  if (! check_arg_check()) {
    final = 0;
  }
  
  /********************/
  
  dmalloc_message("NOTE: ignore the errors from the above ----- to here.\n");
  dmalloc_message("-------------------------------------------------------\n");
  
  /*
   * The following errors whould not generate a dmalloc_errno nor any
   * error messages
   */
  
  /********************/
  
  /*
   * Make sure that realloc of NULL pointers either works or doesn't
   * depending on the proper ALLOW_REALLOC_NULL setting.
   */
  
  {
    if (! silent_b) {
      (void)printf("  Trying to realloc a 0L pointer.\n");
    }
    pnt = realloc(NULL, 10);
#if ALLOW_REALLOC_NULL
    if (pnt == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: re-allocation of 0L returned error.\n");
      }
      final = 0;
    }
    else {
      free(pnt);
    }
#else
    if (pnt == NULL) {
      dmalloc_errno = ERROR_NONE;
    }
    else {
      if (! silent_b) {
	(void)printf("   ERROR: re-allocation of 0L did not return error.\n");
      }
      free(pnt);
      final = 0;
    }
#endif
  }
  
  /********************/
  
  /*
   * Make sure that valloc returns properly page-aligned memory.
   */
  
  {
    int			iter_c, amount;
    unsigned int	old_flags = dmalloc_debug_current();
    
    if (! silent_b) {
      (void)printf("  Testing valloc()\n");
    }
    
    /*
     * First check without frence posts
     */
    dmalloc_debug(old_flags & ~DEBUG_CHECK_FENCE);
    
    for (iter_c = 0; iter_c < 20; iter_c++) {
      do {
	amount = _dmalloc_rand() % (page_size * 3);
      } while (amount == 0);
      pnt = valloc(amount);
      if (pnt == NULL) {
	if (! silent_b) {
	  (void)printf("   ERROR: could not valloc %d bytes.\n", amount);
	}
	final = 0;
	continue;
      }
      if ((unsigned long)pnt % page_size != 0) {
	if (! silent_b) {
	  (void)printf("   ERROR: valloc got %lx which is not page aligned.\n",
		       (unsigned long)pnt);
	}
	final = 0;
      }
      free(pnt);
    }
    
    /*
     * Now test with fence posts enabled
     */
    dmalloc_debug(old_flags | DEBUG_CHECK_FENCE);
    
    for (iter_c = 0; iter_c < 20; iter_c++) {
      do {
	amount = _dmalloc_rand() % (page_size * 3);
      } while (amount == 0);
      pnt = valloc(amount);
      if (pnt == NULL) {
	if (! silent_b) {
	  (void)printf("   ERROR: could not valloc %d bytes.\n", amount);
	}
	final = 0;
	continue;
      }
      if ((unsigned long)pnt % page_size != 0) {
	if (! silent_b) {
	  (void)printf("   ERROR: valloc got %lx which is not page aligned.\n",
		       (unsigned long)pnt);
	}
	final = 0;
      }
      free(pnt);
    }
    
    dmalloc_debug(old_flags);
  }
  
  /********************/
  
  /*
   * Make sure that the blanking flags actually blank all of the
   * allocated pointer space.
   */
  
  {
    char		*alloc_p;
    unsigned int	old_flags, iter_c, amount;
    
    if (! silent_b) {
      (void)printf("  Checking alloc blanking\n");
    }
    
    old_flags = dmalloc_debug_current();
    /* turn on alloc blanking without fence posts */
    dmalloc_debug((old_flags | DEBUG_ALLOC_BLANK) & (~DEBUG_CHECK_FENCE));
    
    for (iter_c = 0; iter_c < 20; iter_c++) {
      do {
	amount = _dmalloc_rand() % (page_size * 3);
      } while (amount == 0);
      pnt = malloc(amount);
      if (pnt == NULL) {
	if (! silent_b) {
	  (void)printf("   ERROR: could not allocate %d bytes.\n", amount);
	}
	final = 0;
	continue;
      }
      for (alloc_p = pnt; alloc_p < (char *)pnt + amount; alloc_p++) {
	if (*alloc_p != ALLOC_BLANK_CHAR) {
	  if (! silent_b) {
	    (void)printf("   ERROR: allocation not fully blanked.\n");
	  }
	  final = 0;
	}
      }
      free(pnt);
    }
    
    /*
     * Now test with fence posts enabled
     */
    dmalloc_debug(old_flags | DEBUG_CHECK_FENCE);
    
    for (iter_c = 0; iter_c < 20; iter_c++) {
      do {
	amount = _dmalloc_rand() % (page_size * 3);
      } while (amount == 0);
      pnt = valloc(amount);
      if (pnt == NULL) {
	if (! silent_b) {
	  (void)printf("   ERROR: could not valloc %d bytes.\n", amount);
	}
	final = 0;
	continue;
      }
      if ((unsigned long)pnt % page_size != 0) {
	if (! silent_b) {
	  (void)printf("   ERROR: valloc got %lx which is not page aligned.\n",
		       (unsigned long)pnt);
	}
	final = 0;
      }
      free(pnt);
    }
    
    dmalloc_debug(old_flags);
  }
  
  /********************/
  
  /*
   * Make sure realloc copy works right.
   */
  
  {
    void		*new_pnt;
    unsigned int	amount, old_flags;
    int			iter_c;
    
    if (! silent_b) {
      (void)printf("  Checking realloc_copy token\n");
    }
    
    old_flags = dmalloc_debug_current();
    dmalloc_debug(old_flags | DEBUG_REALLOC_COPY);
    
    for (iter_c = 0; iter_c < 20; iter_c++) {
      do {
	amount = _dmalloc_rand() % (page_size * 3);
      } while (amount == 0);
      pnt = malloc(amount);
      if (pnt == NULL) {
	if (! silent_b) {
	  (void)printf("   ERROR: could not allocate %d bytes.\n", amount);
	}
	final = 0;
	continue;
      }
      
      /* we should get the same pointer */
      new_pnt = realloc(pnt, amount);
      if (new_pnt == NULL) {
	if (! silent_b) {
	  (void)printf("   ERROR: could not reallocate %d bytes.\n",
		       amount + 10);
	}
	final = 0;
	continue;
      }
      
      if (new_pnt == pnt) {
	if (! silent_b) {
	  (void)printf("   ERROR: realloc produced same pointer.\n");
	}
	final = 0;
	continue;
      }
      
      if (dmalloc_free(__FILE__, __LINE__, new_pnt,
		       DMALLOC_FUNC_FREE) != FREE_NOERROR) {
	if (! silent_b) {
	  (void)printf("   ERROR: free bad pointer produced: %s (err %d)\n",
		       dmalloc_strerror(dmalloc_errno), dmalloc_errno);
	}
	final = 0;
      }
    }
    
    dmalloc_debug(old_flags);
  }
  
  /********************/
  
  /*
   * Make sure realloc copy works right.
   */
  
  {
    void		*new_pnt;
    unsigned int	amount, old_flags;
    int			iter_c;
    
    if (! silent_b) {
      (void)printf("  Checking never-reuse token\n");
    }
    
    old_flags = dmalloc_debug_current();
    dmalloc_debug(old_flags | DEBUG_NEVER_REUSE);
    
    for (iter_c = 0; iter_c < 20; iter_c++) {
      do {
	amount = _dmalloc_rand() % (page_size * 3);
      } while (amount == 0);
      pnt = malloc(amount);
      if (pnt == NULL) {
	if (! silent_b) {
	  (void)printf("   ERROR: could not allocate %d bytes.\n", amount);
	}
	final = 0;
	continue;
      }
      
      /* we should get the same pointer */
      new_pnt = realloc(pnt, amount);
      if (new_pnt == NULL) {
	if (! silent_b) {
	  (void)printf("   ERROR: could not reallocate %d bytes.\n",
		       amount + 10);
	}
	final = 0;
	continue;
      }
      
      if (new_pnt == pnt) {
	if (! silent_b) {
	  (void)printf("   ERROR: realloc produced same pointer.\n");
	}
	final = 0;
	continue;
      }
      
      if (dmalloc_free(__FILE__, __LINE__, new_pnt,
		       DMALLOC_FUNC_FREE) != FREE_NOERROR) {
	if (! silent_b) {
	  (void)printf("   ERROR: free bad pointer produced: %s (err %d)\n",
		       dmalloc_strerror(dmalloc_errno), dmalloc_errno);
	}
	final = 0;
      }
    }
    
    dmalloc_debug(old_flags);
  }

  /********************/
  
  /*
   * Make sure that the dmalloc function checking allows external
   * pointers.
   */
  {
    int			errno_hold = dmalloc_errno;
    unsigned int	old_flags;
    char		buf[20];
    
    if (! silent_b) {
      (void)printf("  Checking function checking of non-heap pointers\n");
    }
    
    old_flags = dmalloc_debug_current();
    dmalloc_debug(old_flags | DEBUG_CHECK_FUNCS);
    
    dmalloc_errno = ERROR_NONE;
    _dmalloc_memset(__FILE__, __LINE__, buf, 0, sizeof(buf));
    if (dmalloc_errno != ERROR_NONE) {
      if (! silent_b) {
	(void)printf("   ERROR: dmalloc_memset of non-heap pointer failed: %s (err %d)\n",
		     dmalloc_strerror(dmalloc_errno), dmalloc_errno);
      }
      final = 0;
    }
    
    dmalloc_debug(old_flags);
    dmalloc_errno = errno_hold;
  }
  
  /********************/
  
  /*
   * Make sure that string tokens work in the processing program.
   */
  {
    unsigned int	old_flags, new_flags;
    
    if (! silent_b) {
      (void)printf("  Checking string tokens in dmalloc_debug_setup\n");
    }
    
    old_flags = dmalloc_debug_current();
    dmalloc_debug(0);
    dmalloc_debug_setup("log-stats,log-non-free,log-bad-space");
    new_flags = dmalloc_debug_current();
    
    if (! (new_flags & DEBUG_LOG_STATS
	   && new_flags & DEBUG_LOG_NONFREE
	   && new_flags & DEBUG_LOG_BAD_SPACE)) {
      if (! silent_b) {
	(void)printf("   ERROR: dmalloc_debug_setup didn't process comma separated tokens.\n");
      }
      final = 0;
    }
    
    dmalloc_debug(old_flags);
  }
  
  /********************/
  
  /*
   * Make sure that the calloc macro is valid.
   */
  {
    DMALLOC_SIZE	user_size;
  
    if (! silent_b) {
      (void)printf("  Checking calloc macro arguments\n");
    }
    
    /* notice that we do not have a () around this operation */
#define SIZE_ARG	10 + 1
#define COUNT_ARG	2
    
    /*
     * Initially I was not putting parens around macro arguments in
     * dmalloc.h.  Rookie mistake.  This should allocate (10 + 1) * 2
     * bytes (22) not 10 + 1 * 2 bytes (12).
     */
    pnt = calloc(SIZE_ARG, COUNT_ARG);
    if (pnt == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not calloc %d bytes.\n",
		     (SIZE_ARG) * (COUNT_ARG));
      }
      final = 0;
    }
    
    else if (dmalloc_examine(pnt, &user_size, NULL, NULL, NULL, NULL, NULL,
			     NULL) != DMALLOC_NOERROR) {
      if (! silent_b) {
	(void)printf("   ERROR: examining pointer %lx failed.\n",
		     (unsigned long)pnt);
      }
      final = 0;
    }
    
    else if (user_size != (SIZE_ARG) * (COUNT_ARG)) {
      if (! silent_b) {
	(void)printf("   ERROR: calloc size should be %d but was %d.\n",
		     (SIZE_ARG) * (COUNT_ARG), user_size);
      }
      final = 0;
    }
  }
  
  /********************/
  
  /*
   * Verify that the check-funcs work with check-fence.  Thanks to
   * John Hetherington for reporting this.
   */
  {
    unsigned int	old_flags = dmalloc_debug_current();
    int			errno_hold = dmalloc_errno;
    
    dmalloc_debug(DEBUG_CHECK_FUNCS);
    
    if (! silent_b) {
      (void)printf("  Checking check-funcs with check-fence\n");
    }
    
#define BUF_SIZE	64
    
    pnt = malloc(BUF_SIZE);
    if (pnt == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not malloc %d bytes.\n", BLOCK_SIZE);
      }
      return 0;
    }
    
    dmalloc_errno = ERROR_NONE;
    _dmalloc_memset(__FILE__, __LINE__, pnt, 0, BUF_SIZE);
    if (dmalloc_errno != ERROR_NONE) {
      if (! silent_b) {
	(void)printf("   ERROR: memset on buf of %d bytes failed.\n",
		     BUF_SIZE);
      }
      final = 0;
    }
    
    free(pnt);
    
    /* now turn on the check-fence token */
    dmalloc_debug(DEBUG_CHECK_FENCE | DEBUG_CHECK_FUNCS);
    
    pnt = malloc(BUF_SIZE);
    if (pnt == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not malloc %d bytes.\n", BLOCK_SIZE);
      }
      return 0;
    }
    
    dmalloc_errno = ERROR_NONE;
    _dmalloc_memset(__FILE__, __LINE__, pnt, 0, BUF_SIZE);
    if (dmalloc_errno != ERROR_NONE) {
      if (! silent_b) {
	(void)printf("   ERROR: memset of %d bytes with check-fence failed.\n",
		     BUF_SIZE);
      }
      final = 0;
    }
    
    free(pnt);
    dmalloc_debug(old_flags);
    
    /* recover the errno if necessary */
    if (dmalloc_errno == ERROR_NONE) {
      dmalloc_errno = errno_hold;
    }
  }
  
  /********************/
  
  /*
   * Verify the dmalloc_count_changed function.
   */
  {
    unsigned long	size, mem_count, loc_mark;
    
    if (! silent_b) {
      (void)printf("  Checking dmalloc_count_changed function\n");
    }
    
    /* get a mark */
    loc_mark = dmalloc_mark();
    
    /* make sure that nothing has changed */
    mem_count = dmalloc_count_changed(loc_mark, 1 /* not-freed */,
				      1 /* freed */);
    if (mem_count != 0) {
      if (! silent_b) {
	(void)printf("   ERROR: count-changed reported %lu bytes changed.\n",
		     mem_count);
      }
      return 0;
    }
    
    /* allocate a pointer */
    size = _dmalloc_rand() % MAX_ALLOC + 10;
    pnt = malloc(size);
    if (pnt == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not malloc %lu bytes.\n", size);
      }
      return 0;
    }
    
    /* make sure that we see the non-freed pointer */
    mem_count = dmalloc_count_changed(loc_mark, 1 /* not-freed */,
				      0 /* no freed */);
    if (mem_count != size) {
      if (! silent_b) {
	(void)printf("   ERROR: count-changed reported %lu bytes changed not %lu.\n",
		     mem_count, size);
      }
      return 0;
    }
    /* make sure that we see non-freed pointer with non-fred + freed flags */
    mem_count = dmalloc_count_changed(loc_mark, 1 /* not-freed */,
				      1 /* no freed */);
    if (mem_count != size) {
      if (! silent_b) {
	(void)printf("   ERROR: count-changed reported %lu bytes changed not %lu.\n",
		     mem_count, size);
      }
      return 0;
    }
    
    /* now free it */
    free(pnt);
    
    /* make sure that everything is freed */
    mem_count = dmalloc_count_changed(loc_mark, 1 /* not-freed */,
				      0 /* no freed */);
    if (mem_count != 0) {
      if (! silent_b) {
	(void)printf("   ERROR: count-changed reported %lu bytes changed.\n",
		     mem_count);
      }
      return 0;
    }
    
    /* make sure that we see the freed pointer */
    mem_count = dmalloc_count_changed(loc_mark, 0 /* no not-freed */,
				      1 /* no freed */);
    if (mem_count != size) {
      if (! silent_b) {
	(void)printf("   ERROR: count-changed report %lu bytes changed not %lu.\n",
		     mem_count, size);
      }
      return 0;
    }
    /* make sure that we see the freed pointer with both flags */
    mem_count = dmalloc_count_changed(loc_mark, 1 /* no not-freed */,
				      1 /* no freed */);
    if (mem_count != size) {
      if (! silent_b) {
	(void)printf("   ERROR: count-changed report %lu bytes changed not %lu.\n",
		     mem_count, size);
      }
      return 0;
    }
  }
 
  /********************/
  
  /*
   * Check block rounding by allocator.
   */
  {
    unsigned long	size;
    unsigned int	old_flags = dmalloc_debug_current();
    
    if (! silent_b) {
      (void)printf("  Checking block-size alignment rounding\n");
    }
    
    /* enable never-reuse and disable fence checking */
    dmalloc_debug((old_flags | DEBUG_NEVER_REUSE) & (~DEBUG_CHECK_FENCE));
    
    size = BLOCK_SIZE / 2 + 1;
    pnt = malloc(size);
    if (pnt == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not malloc %lu bytes.\n", size);
      }
      return 0;
    }
    if ((unsigned long)pnt % BLOCK_SIZE != 0) {
      if (! silent_b) {
	(void)printf("   ERROR: alloc of %lu bytes was not block aligned.\n",
		     size);
      }
      return 0;
    }
    free(pnt);
    
    /*
     * Now we'll allocate some pitifully small chunk from the heap
     * allocator and it should return a good pointer.  In the future
     * it should re-align the heap.
     */

    size = 10;
    pnt = _dmalloc_heap_alloc(size);
    if (pnt == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not heap-alloc %lu bytes.\n", size);
      }
      return 0;
    }
    
    size = BLOCK_SIZE / 2 + 1;
    pnt = malloc(size);
    if (pnt == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not malloc %lu bytes.\n", size);
      }
      return 0;
    }
    if ((unsigned long)pnt % BLOCK_SIZE != 0) {
      if (! silent_b) {
	(void)printf("   ERROR: alloc of %lu bytes was not block aligned.\n",
		     size);
      }
      return 0;
    }
    free(pnt);
    
    /* restore flags */
    dmalloc_debug(old_flags);
  }
  
  /********************/
  
  /*
   * Make sure free-blank doesn't imply alloc-blank
   */
  {
    unsigned long	size;
    unsigned int	old_flags = dmalloc_debug_current();
    
    if (! silent_b) {
      (void)printf("  Checking free versus alloc blank\n");
    }
    
    /* enable free blank only without fence-post, alloc, or check blank */ 
    dmalloc_debug((old_flags | DEBUG_FREE_BLANK)
		  & (~(DEBUG_CHECK_FENCE | DEBUG_CHECK_BLANK
		       | DEBUG_ALLOC_BLANK)));
    
    /* allocate a pointer */
    size = BLOCK_SIZE / 2 + 1;
    pnt = malloc(size);
    if (pnt == NULL) {
      if (! silent_b) {
	(void)printf("   ERROR: could not malloc %lu bytes.\n", size);
      }
      return 0;
    }
    
    /* overwrite one of the top chars */
    *((char *)pnt + size) = '\0';
    
    /* this shouldn't notice */
    if (dmalloc_free(__FILE__, __LINE__, pnt,
		     DMALLOC_FUNC_FREE) != FREE_NOERROR) {
      if (! silent_b) {
	(void)printf("   ERROR: free versus alloc flags failed: %s (err %d)\n",
		     dmalloc_strerror(dmalloc_errno), dmalloc_errno);
      }
      final = 0;
    }
    
    /* NOTE: no restoring of overwritten chars here because of free-blank */
    
    /* restore flags */
    dmalloc_debug(old_flags);
  }
  
  /********************/
  
  /*
   * Check the dmalloc_get_stats function
   */
  {
    unsigned long	current_allocated, current_pnt_n;
    unsigned long	current_allocated2, current_pnt_n2;
    int			amount;
    
    if (! silent_b) {
      (void)printf("  Checking the dmalloc_get_stats function\n");
    }
    
    dmalloc_get_stats(NULL, NULL, NULL, NULL, &current_allocated,
		      &current_pnt_n, NULL, NULL, NULL);
    
    amount = 12;
    pnt = malloc(amount);
    
    dmalloc_get_stats(NULL, NULL, NULL, NULL, &current_allocated2,
		      &current_pnt_n2, NULL, NULL, NULL);
    
    if (current_allocated2 != current_allocated + amount) {
      if (! silent_b) {
	(void)printf("   ERROR: dmalloc_get_stats did not count alloc of %d\n",
		     amount);
      }
      final = 0;
    }
    if (current_pnt_n2 != current_pnt_n + 1) {
      if (! silent_b) {
	(void)printf("   ERROR: dmalloc_get_stats did not count pnt alloc\n");
      }
      final = 0;
    }
    
    free(pnt);
    
    dmalloc_get_stats(NULL, NULL, NULL, NULL, &current_allocated2,
		      &current_pnt_n2, NULL, NULL, NULL);
    
    if (current_allocated2 != current_allocated) {
      if (! silent_b) {
	(void)printf("   ERROR: dmalloc_get_stats did not count free of %d\n",
		     amount);
      }
      final = 0;
    }
    if (current_pnt_n2 != current_pnt_n) {
      if (! silent_b) {
	(void)printf("   ERROR: dmalloc_get_stats did not count pnt free\n");
      }
      final = 0;
    }
  }
  
  /********************/
  
  /*
   * NOTE: add tests which should result in errors before the -------
   * message above
   */
  
  return final;
}

/*
 * Run the interactive section of the program
 */
static	void	do_interactive(void)
{
  int		len;
  char		line[128], *line_p;
  void		*pnt;
  
  (void)printf("Malloc test program.  Type 'help' for assistance.\n");
  
  while (1) {
    (void)printf("> ");
    if (fgets(line, sizeof(line), stdin) == NULL) {
      break;
    }
    line_p = strchr(line, '\n');
    if (line_p != NULL) {
      *line_p = '\0';
    }
    
    len = strlen(line);
    if (len == 0) {
      continue;
    }
    
    if (strncmp(line, "?", len) == 0
	|| strncmp(line, "help", len) == 0) {
      (void)printf("\thelp      - print this message\n\n");
      
      (void)printf("\tmalloc    - allocate memory\n");
      (void)printf("\tcalloc    - allocate/clear memory\n");
      (void)printf("\trealloc   - reallocate memory\n");
      (void)printf("\trecalloc  - reallocate cleared memory\n");
      (void)printf("\tmemalign  - allocate aligned memory\n");
      (void)printf("\tvalloc    - allocate page-aligned memory\n");
      (void)printf("\tstrdup    - allocate a string\n");
      (void)printf("\tfree      - deallocate memory\n\n");
      
      (void)printf("\tstats     - dump heap stats to the logfile\n");
      (void)printf("\tunfreed   - list the unfree memory to the logfile\n");
      (void)printf("\tmark      - display the current mark value\n");
      (void)printf("\tchanged   - display what pointers have changed\n\n");
      
      (void)printf("\tverify    - check out a memory address (or all heap)\n");
      (void)printf("\toverwrite - overwrite some memory to test errors\n");
#if HAVE_SBRK
      (void)printf("\tsbrk       - call sbrk to test external areas\n\n");
#endif
      
      (void)printf("\trandom    - randomly execute a number of [de] allocs\n");
      (void)printf("\tspecial   - run some special tests\n\n");
      
      (void)printf("\tquit      - quit this test program\n");
      continue;
    }
    
    if (strncmp(line, "quit", len) == 0) {
      break;
    }
    
    if (strncmp(line, "malloc", len) == 0) {
      int	size;
      
      (void)printf("How much to malloc: ");
      if (fgets(line, sizeof(line), stdin) == NULL) {
	break;
      }
      size = atoi(line);
      (void)printf("malloc(%d) returned '%#lx'\n", size, (long)malloc(size));
      continue;
    }
    
    if (strncmp(line, "calloc", len) == 0) {
      int	size;
      
      (void)printf("How much to calloc: ");
      if (fgets(line, sizeof(line), stdin) == NULL) {
	break;
      }
      size = atoi(line);
      (void)printf("calloc(%d) returned '%#lx'\n",
		   size, (long)calloc(size, sizeof(char)));
      continue;
    }
    
    if (strncmp(line, "realloc", len) == 0) {
      int	size;
      
      pnt = get_address();
      
      (void)printf("How much to realloc: ");
      if (fgets(line, sizeof(line), stdin) == NULL) {
	break;
      }
      size = atoi(line);
      
      (void)printf("realloc(%#lx, %d) returned '%#lx'\n",
		   (long)pnt, size, (long)realloc(pnt, size));
      
      continue;
    }
    
    if (strncmp(line, "recalloc", len) == 0) {
      int	size;
      
      pnt = get_address();
      
      (void)printf("How much to recalloc: ");
      if (fgets(line, sizeof(line), stdin) == NULL) {
	break;
      }
      size = atoi(line);
      
      (void)printf("realloc(%#lx, %d) returned '%#lx'\n",
		   (long)pnt, size, (long)recalloc(pnt, size));
      
      continue;
    }
    
    if (strncmp(line, "memalign", len) == 0) {
      int	alignment, size;
      
      (void)printf("Alignment in bytes: ");
      if (fgets(line, sizeof(line), stdin) == NULL) {
	break;
      }
      alignment = atoi(line);
      (void)printf("How much to memalign: ");
      if (fgets(line, sizeof(line), stdin) == NULL) {
	break;
      }
      size = atoi(line);
      (void)printf("memalign(%d, %d) returned '%#lx'\n",
		   alignment, size, (long)memalign(alignment, size));
      continue;
    }
    
    if (strncmp(line, "valloc", len) == 0) {
      int	size;
      
      (void)printf("How much to valloc: ");
      if (fgets(line, sizeof(line), stdin) == NULL) {
	break;
      }
      size = atoi(line);
      (void)printf("valloc(%d) returned '%#lx'\n", size, (long)valloc(size));
      continue;
    }
    
    if (strncmp(line, "strdup", len) == 0) {
      (void)printf("Enter a string to strdup: ");
      if (fgets(line, sizeof(line), stdin) == NULL) {
	break;
      }
      (void)printf("strdup returned '%#lx'\n", (long)strdup(line));
      continue;
    }
    
    if (strncmp(line, "free", len) == 0) {
      pnt = get_address();
      free(pnt);
      continue;
    }
    
    if (strncmp(line, "stats", len) == 0) {
      dmalloc_log_stats();
      (void)printf("Done.\n");
      continue;
    }
    
    if (strncmp(line, "unfreed", len) == 0) {
      dmalloc_log_unfreed();
      (void)printf("Done.\n");
      continue;
    }
    
    if (strncmp(line, "mark", len) == 0) {
      (void)printf("Mark is %lu.\n", dmalloc_mark());
      continue;
    }
    
    if (strncmp(line, "changed", len) == 0) {
      (void)printf("Enter the mark: ");
      if (fgets(line, sizeof(line), stdin) == NULL) {
	break;
      }
      dmalloc_log_changed(atoi(line), 1, 1, 1);
      (void)printf("Done.\n");
      continue;
    }
    
    if (strncmp(line, "overwrite", len) == 0) {
      char	*overwrite = "OVERWRITTEN";
      
      pnt = get_address();
      memcpy((char *)pnt, overwrite, strlen(overwrite));
      (void)printf("Done.\n");
      continue;
    }
    
#if HAVE_SBRK
    /* call sbrk directly */
    if (strncmp(line, "sbrk", len) == 0) {
      int	size;
      
      (void)printf("How much to sbrk: ");
      if (fgets(line, sizeof(line), stdin) == NULL) {
	break;
      }
      size = atoi(line);
      (void)printf("sbrk(%d) returned '%#lx'\n", size, (long)sbrk(size));
      continue;
    }
#endif
    
    /* do random heap hits */
    if (strncmp(line, "random", len) == 0) {
      int	iter_n;
      
      (void)printf("How many iterations[%ld]: ", default_iter_n);
      if (fgets(line, sizeof(line), stdin) == NULL) {
	break;
      }
      if (line[0] == '\0' || line[0] == '\n') {
	iter_n = default_iter_n;
      }
      else {
	iter_n = atoi(line);
      }
      
      if (do_random(iter_n)) {
	(void)printf("It succeeded.\n");
      }
      else {
	(void)printf("It failed.\n");
      }
      
      continue;
    }
    
    /* do special checks */
    if (strncmp(line, "special", len) == 0) {
      if (check_special()) {
	(void)printf("It succeeded.\n");
      }
      else {
	(void)printf("It failed.\n");
      }
      
      continue;
    }
    
    if (strncmp(line, "verify", len) == 0) {
      int	ret;
      
      (void)printf("If the address is 0, verify will check the whole heap.\n");
      pnt = get_address();
      ret = dmalloc_verify(pnt);
      (void)printf("dmalloc_verify(%#lx) returned '%s'\n",
		   (long)pnt,
		   (ret == DMALLOC_NOERROR ? "success" : "failure"));
      continue;
    }
    
    (void)printf("Unknown command '%s'.  Type 'help' for assistance.\n", line);
  }
}

/*
 * Allocation tracking function called each time an allocation occurs.
 * FILE may be a return address if LINE is 0.  FUNC_ID is one of the
 * above DMALLOC_FUNC_ defines.  BYTE_SIZE is how many bytes were
 * requested with a possible ALIGNMENT.  OLD_ADDR is for realloc and
 * free functions.  NEW_ADDR is the pointer returned by the allocation
 * functions.
 */
static	void	track_alloc_trxn(const char *file, const unsigned int line,
				 const int func_id,
				 const DMALLOC_SIZE byte_size,
				 const DMALLOC_SIZE alignment,
				 const DMALLOC_PNT old_addr,
				 const DMALLOC_PNT new_addr)
{
  char	file_line[64];
  
  if (file == NULL && line == 0) {
    strcpy(file_line, "unknown");
  }
  else if (line == 0) {
    (void)loc_snprintf(file_line, sizeof(file_line), "ra=%#lx", (long)file);
  }
  else {
    (void)loc_snprintf(file_line, sizeof(file_line), "%s:%d", file, line);
  }
  
  switch (func_id) {
  case DMALLOC_FUNC_MALLOC:
    (void)printf("%s malloc %d bytes got %#lx\n",
		 file_line, byte_size, (long)new_addr);
    break;
  case DMALLOC_FUNC_CALLOC:
    (void)printf("%s calloc %d bytes got %#lx\n",
		 file_line, byte_size, (long)new_addr);
    break;
  case DMALLOC_FUNC_REALLOC:
    (void)printf("%s realloc %d bytes from %#lx got %#lx\n",
		 file_line, byte_size, (long)old_addr, (long)new_addr);
    break;
  case DMALLOC_FUNC_RECALLOC:
    (void)printf("%s recalloc %d bytes from %#lx got %#lx\n",
		 file_line, byte_size, (long)old_addr, (long)new_addr);
    break;
  case DMALLOC_FUNC_MEMALIGN:
    (void)printf("%s memalign %d bytes alignment %d got %#lx\n",
		 file_line, byte_size, alignment, (long)new_addr);
    break;
  case DMALLOC_FUNC_VALLOC:
    (void)printf("%s valloc %d bytes alignment %d got %#lx\n",
		 file_line, byte_size, alignment, (long)new_addr);
    break;
  case DMALLOC_FUNC_STRDUP:
    (void)printf("%s strdup %d bytes ot %#lx\n",
		 file_line, byte_size, (long)new_addr);
    break;
  case DMALLOC_FUNC_FREE:
    (void)printf("%s free %#lx\n", file_line, (long)old_addr);
    break;
  case DMALLOC_FUNC_NEW:
    (void)printf("%s new %d bytes got %#lx\n",
		 file_line, byte_size, (long)new_addr);
    break;
  case DMALLOC_FUNC_NEW_ARRAY:
    (void)printf("%s new[] %d bytes got %#lx\n",
		 file_line, byte_size, (long)new_addr);
    break;
  case DMALLOC_FUNC_DELETE:
    (void)printf("%s delete %#lx\n", file_line, (long)old_addr);
    break;
  case DMALLOC_FUNC_DELETE_ARRAY:
    (void)printf("%s delete[] %#lx\n", file_line, (long)old_addr);
    break;
  default:
    (void)printf("%s unknown function %d bytes, %d alignment, %#lx old-addr "
		 "%#lx new-addr\n",
		 file_line, byte_size, alignment, (long)old_addr,
		 (long)new_addr);
    break;
  }
}

int	main(int argc, char **argv)
{
  unsigned int	store_flags;
  int		ret, final = 0;
  
  argv_process(arg_list, argc, argv);
  
  if (silent_b && (verbose_b || interactive_b)) {
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
  
  if (log_trans_b) {
    dmalloc_track(track_alloc_trxn);
  }
  
  store_flags = dmalloc_debug_current();
  
  /*************************************************/
  
  if (! no_special_b) {
    
    /* some special tests to run first thing */
    if (! silent_b) {
      (void)printf("Running initial tests...\n");
    }
    if (check_initial_special()) {
      if (! silent_b) {
	(void)printf("  Succeeded.\n");
      }
    }
    else {
      if (silent_b) {
	(void)printf("ERROR: Initial tests failed.  Last dmalloc error: %s (err %d)\n",
		     dmalloc_strerror(dmalloc_errno), dmalloc_errno);
      }
      else {
	(void)printf("  Failed.  Last dmalloc error: %s (err %d)\n",
		     dmalloc_strerror(dmalloc_errno), dmalloc_errno);
      }
      final = 1;
    }
  }
  
  /*************************************************/
  
  if (interactive_b) {
    do_interactive();
  }
  else {
    if (! silent_b) {
      (void)printf("Running %ld tests (use -%c for interactive)...\n",
		   default_iter_n, INTER_CHAR);
    }
    (void)fflush(stdout);
    
    if (do_random(default_iter_n)) {
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
  }
  
  /*************************************************/
  
  /* check the special malloc functions but don't allow silent dumps */
  if ((! no_special_b)
      && (! (silent_b
	     && (dmalloc_debug_current() & DEBUG_ERROR_ABORT)))) {
    if (! silent_b) {
      (void)printf("Running special tests...\n");
    }
    if (check_special()) {
      if (! silent_b) {
	(void)printf("  Succeeded.\n");
      }
    }
    else {
      if (silent_b) {
	(void)printf("ERROR: Running special tests failed.  Last dmalloc error: %s (err %d)\n",
		     dmalloc_strerror(dmalloc_errno), dmalloc_errno);
      }
      else {
	(void)printf("  Failed.  Last dmalloc error: %s (err %d)\n",
		     dmalloc_strerror(dmalloc_errno), dmalloc_errno);
      }
      final = 1;
    }
  }
  
  /*************************************************/
  
  dmalloc_debug(store_flags);
  
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
  malloc_shutdown();
#endif
  
  exit(final);
}
