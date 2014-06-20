/*
 * Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
 *
 * Additional copyrights may follow.
 */
/* Malloc implementation for multiple threads without lock contention.
   Copyright (C) 2001, 2002, 2003 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Wolfram Gloger <wg@malloc.de>, 2001.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* $Id: hooks.c,v 1.12 2004/11/05 14:42:32 wg Exp $ */

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/memory/memory.h"
#include "opal/util/show_help.h"
#include "opal/constants.h"

#include "opal/mca/memory/linux/memory_linux.h"

#ifndef DEFAULT_CHECK_ACTION
#define DEFAULT_CHECK_ACTION 1
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>  /* for stat */
#endif  /* HAVE_SYS_STAT_H */


/* What to do if the standard debugging hooks are in place and a
   corrupt pointer is detected: do nothing (0), print an error message
   (1), or call abort() (2). */

/* Hooks for debugging versions.  The initial hooks just call the
   initialization routine, then do the normal work. */

#if !(USE_STARTER & 2)

/* OMPI change: these aren't used (avoid a compiler warning by if
   0'ing them out */
#if 0
static Void_t*
#if __STD_C
malloc_hook_ini(size_t sz, const __malloc_ptr_t caller)
#else
malloc_hook_ini(sz, caller)
     size_t sz; const __malloc_ptr_t caller;
#endif
{
  __malloc_hook = NULL;
  ptmalloc_init();
  return public_mALLOc(sz);
}
#endif

/* OMPI change: these aren't used (avoid a compiler warning by if
   0'ing them out */
#if 0
static Void_t*
#if __STD_C
realloc_hook_ini(Void_t* ptr, size_t sz, const __malloc_ptr_t caller)
#else
realloc_hook_ini(ptr, sz, caller)
     Void_t* ptr; size_t sz; const __malloc_ptr_t caller;
#endif
{
  __malloc_hook = NULL;
  __realloc_hook = NULL;
  ptmalloc_init();
  return public_rEALLOc(ptr, sz);
}
#endif

/* OMPI change: these aren't used (avoid a compiler warning by if
   0'ing them out */
#if 0
static Void_t*
#if __STD_C
memalign_hook_ini(size_t alignment, size_t sz, const __malloc_ptr_t caller)
#else
memalign_hook_ini(alignment, sz, caller)
     size_t alignment; size_t sz; const __malloc_ptr_t caller;
#endif
{
  __memalign_hook = NULL;
  ptmalloc_init();
  return public_mEMALIGn(alignment, sz);
}
#endif

#endif /* !(USE_STARTER & 2) */

static int check_action = DEFAULT_CHECK_ACTION;

/* Whether we are using malloc checking.  */
static int using_malloc_checking;

/* A flag that is set by malloc_set_state, to signal that malloc checking
   must not be enabled on the request from the user (via the MALLOC_CHECK_
   environment variable).  It is reset by __malloc_check_init to tell
   malloc_set_state that the user has requested malloc checking.

   The purpose of this flag is to make sure that malloc checking is not
   enabled when the heap to be restored was constructed without malloc
   checking, and thus does not contain the required magic bytes.
   Otherwise the heap would be corrupted by calls to free and realloc.  If
   it turns out that the heap was created with malloc checking and the
   user has requested it malloc_set_state just calls __malloc_check_init
   again to enable it.  On the other hand, reusing such a heap without
   further malloc checking is safe.  */
static int disallow_malloc_check;

/* Activate a standard set of debugging hooks. */
void
__malloc_check_init()
{
  if (disallow_malloc_check) {
    disallow_malloc_check = 0;
    return;
  }
  using_malloc_checking = 1;
  __malloc_hook = malloc_check;
  __free_hook = free_check;
  __realloc_hook = realloc_check;
  __memalign_hook = memalign_check;
  if(check_action & 1)
    fprintf(stderr, "malloc: using debugging hooks\n");
}

/* A simple, standard set of debugging hooks.  Overhead is `only' one
   byte per chunk; still this will catch most cases of double frees or
   overruns.  The goal here is to avoid obscure crashes due to invalid
   usage, unlike in the MALLOC_DEBUG code. */

#define MAGICBYTE(p) ( ( ((size_t)p >> 3) ^ ((size_t)p >> 11)) & 0xFF )

/* Instrument a chunk with overrun detector byte(s) and convert it
   into a user pointer with requested size sz. */

static Void_t*
internal_function
#if __STD_C
mem2mem_check(Void_t *ptr, size_t sz)
#else
mem2mem_check(ptr, sz) Void_t *ptr; size_t sz;
#endif
{
  mchunkptr p;
  unsigned char* m_ptr = (unsigned char*)BOUNDED_N(ptr, sz);
  size_t i;

  if (!ptr)
    return ptr;
  p = mem2chunk(ptr);
  for(i = chunksize(p) - (chunk_is_mmapped(p) ? 2*SIZE_SZ+1 : SIZE_SZ+1);
      i > sz;
      i -= 0xFF) {
    if(i-sz < 0x100) {
      m_ptr[i] = (unsigned char)(i-sz);
      break;
    }
    m_ptr[i] = 0xFF;
  }
  m_ptr[sz] = MAGICBYTE(p);
  return (Void_t*)m_ptr;
}

/* Convert a pointer to be free()d or realloc()ed to a valid chunk
   pointer.  If the provided pointer is not valid, return NULL. */

static mchunkptr
internal_function
#if __STD_C
mem2chunk_check(Void_t* mem)
#else
mem2chunk_check(mem) Void_t* mem;
#endif
{
  mchunkptr p;
  INTERNAL_SIZE_T sz, c;
  unsigned char magic;

  if(!aligned_OK(mem)) return NULL;
  p = mem2chunk(mem);
  if (!chunk_is_mmapped(p)) {
    /* Must be a chunk in conventional heap memory. */
    int contig = contiguous(&main_arena);
    sz = chunksize(p);
    if((contig &&
	((char*)p<mp_.sbrk_base ||
	 ((char*)p + sz)>=(mp_.sbrk_base+main_arena.system_mem) )) ||
       sz<MINSIZE || sz&MALLOC_ALIGN_MASK || !inuse(p) ||
       ( !prev_inuse(p) && (p->prev_size&MALLOC_ALIGN_MASK ||
                            (contig && (char*)prev_chunk(p)<mp_.sbrk_base) ||
                            next_chunk(prev_chunk(p))!=p) ))
      return NULL;
    magic = MAGICBYTE(p);
    for(sz += SIZE_SZ-1; (c = ((unsigned char*)p)[sz]) != magic; sz -= c) {
      if(c<=0 || sz<(c+2*SIZE_SZ)) return NULL;
    }
    ((unsigned char*)p)[sz] ^= 0xFF;
  } else {
    unsigned long offset, page_mask = malloc_getpagesize-1;

    /* mmap()ed chunks have MALLOC_ALIGNMENT or higher power-of-two
       alignment relative to the beginning of a page.  Check this
       first. */
    offset = (unsigned long)mem & page_mask;
    if((offset!=MALLOC_ALIGNMENT && offset!=0 && offset!=0x10 &&
        offset!=0x20 && offset!=0x40 && offset!=0x80 && offset!=0x100 &&
        offset!=0x200 && offset!=0x400 && offset!=0x800 && offset!=0x1000 &&
        offset<0x2000) ||
       !chunk_is_mmapped(p) || (p->size & PREV_INUSE) ||
       ( (((unsigned long)p - p->prev_size) & page_mask) != 0 ) ||
       ( (sz = chunksize(p)), ((p->prev_size + sz) & page_mask) != 0 ) )
      return NULL;
    magic = MAGICBYTE(p);
    for(sz -= 1; (c = ((unsigned char*)p)[sz]) != magic; sz -= c) {
      if(c<=0 || sz<(c+2*SIZE_SZ)) return NULL;
    }
    ((unsigned char*)p)[sz] ^= 0xFF;
  }
  return p;
}

/* Check for corruption of the top chunk, and try to recover if
   necessary. */

static int
internal_function
#if __STD_C
top_check(void)
#else
top_check()
#endif
{
  mchunkptr t = top(&main_arena);
  char* brk, * new_brk;
  INTERNAL_SIZE_T front_misalign, sbrk_size;
  unsigned long pagesz = malloc_getpagesize;

  if (t == initial_top(&main_arena) ||
      (!chunk_is_mmapped(t) &&
       chunksize(t)>=MINSIZE &&
       prev_inuse(t) &&
       (!contiguous(&main_arena) ||
	(char*)t + chunksize(t) == mp_.sbrk_base + main_arena.system_mem)))
    return 0;

  if(check_action & 1)
    fprintf(stderr, "malloc: top chunk is corrupt\n");
  if(check_action & 2)
    abort();

  /* Try to set up a new top chunk. */
  brk = MORECORE(0);
  front_misalign = (unsigned long)chunk2mem(brk) & MALLOC_ALIGN_MASK;
  if (front_misalign > 0)
    front_misalign = MALLOC_ALIGNMENT - front_misalign;
  sbrk_size = front_misalign + mp_.top_pad + MINSIZE;
  sbrk_size += pagesz - ((unsigned long)(brk + sbrk_size) & (pagesz - 1));
  new_brk = (char*)(MORECORE (sbrk_size));
  if (new_brk == (char*)(MORECORE_FAILURE)) return -1;
  /* Call the `morecore' hook if necessary.  */
  if (__after_morecore_hook)
    (*__after_morecore_hook) ();
  main_arena.system_mem = (new_brk - mp_.sbrk_base) + sbrk_size;

  top(&main_arena) = (mchunkptr)(brk + front_misalign);
  set_head(top(&main_arena), (sbrk_size - front_misalign) | PREV_INUSE);

  return 0;
}

static Void_t*
#if __STD_C
malloc_check(size_t sz, const Void_t *caller)
#else
malloc_check(sz, caller) size_t sz; const Void_t *caller;
#endif
{
  Void_t *victim;

  (void)mutex_lock(&main_arena.mutex);
  victim = (top_check() >= 0) ? _int_malloc(&main_arena, sz+1) : NULL;
  (void)mutex_unlock(&main_arena.mutex);
  return mem2mem_check(victim, sz);
}

static void
#if __STD_C
free_check(Void_t* mem, const Void_t *caller)
#else
free_check(mem, caller) Void_t* mem; const Void_t *caller;
#endif
{
  mchunkptr p;

  if(!mem) return;
  (void)mutex_lock(&main_arena.mutex);
  p = mem2chunk_check(mem);
  if(!p) {
    (void)mutex_unlock(&main_arena.mutex);
    if(check_action & 1)
      fprintf(stderr, "free(): invalid pointer %p!\n", mem);
    if(check_action & 2)
      abort();
    return;
  }
#if HAVE_MMAP
  if (chunk_is_mmapped(p)) {
    (void)mutex_unlock(&main_arena.mutex);
    munmap_chunk(p);
    return;
  }
#endif
#if 0 /* Erase freed memory. */
  memset(mem, 0, chunksize(p) - (SIZE_SZ+1));
#endif
  _int_free(&main_arena, mem);
  (void)mutex_unlock(&main_arena.mutex);
}

static Void_t*
#if __STD_C
realloc_check(Void_t* oldmem, size_t bytes, const Void_t *caller)
#else
realloc_check(oldmem, bytes, caller)
     Void_t* oldmem; size_t bytes; const Void_t *caller;
#endif
{
  mchunkptr oldp;
  INTERNAL_SIZE_T nb, oldsize;
  Void_t* newmem = 0;

  if (oldmem == 0) return malloc_check(bytes, NULL);
  (void)mutex_lock(&main_arena.mutex);
  oldp = mem2chunk_check(oldmem);
  (void)mutex_unlock(&main_arena.mutex);
  if(!oldp) {
    if(check_action & 1)
      fprintf(stderr, "realloc(): invalid pointer %p!\n", oldmem);
    if(check_action & 2)
      abort();
    return malloc_check(bytes, NULL);
  }
  oldsize = chunksize(oldp);

  checked_request2size(bytes+1, nb);
  (void)mutex_lock(&main_arena.mutex);

#if HAVE_MMAP
  if (chunk_is_mmapped(oldp)) {
#if HAVE_MREMAP
    mchunkptr newp = mremap_chunk(oldp, nb);
    if(newp)
      newmem = chunk2mem(newp);
    else
#endif
    {
      /* Note the extra SIZE_SZ overhead. */
      if(oldsize - SIZE_SZ >= nb)
	newmem = oldmem; /* do nothing */
      else {
        /* Must alloc, copy, free. */
        if (top_check() >= 0)
	  newmem = _int_malloc(&main_arena, bytes+1);
        if (newmem) {
          MALLOC_COPY(BOUNDED_N(newmem, bytes+1), oldmem, oldsize - 2*SIZE_SZ);
          munmap_chunk(oldp);
        }
      }
    }
  } else {
#endif /* HAVE_MMAP */
    if (top_check() >= 0)
      newmem = _int_realloc(&main_arena, oldmem, bytes+1);
#if 0 /* Erase freed memory. */
    if(newmem)
      newp = mem2chunk(newmem);
    nb = chunksize(newp);
    if(oldp<newp || oldp>=chunk_at_offset(newp, nb)) {
      memset((char*)oldmem + 2*sizeof(mbinptr), 0,
             oldsize - (2*sizeof(mbinptr)+2*SIZE_SZ+1));
    } else if(nb > oldsize+SIZE_SZ) {
      memset((char*)BOUNDED_N(chunk2mem(newp), bytes) + oldsize,
	     0, nb - (oldsize+SIZE_SZ));
    }
#endif
#if HAVE_MMAP
  }
#endif
  (void)mutex_unlock(&main_arena.mutex);

  return mem2mem_check(newmem, bytes);
}

static Void_t*
#if __STD_C
memalign_check(size_t alignment, size_t bytes, const Void_t *caller)
#else
memalign_check(alignment, bytes, caller)
     size_t alignment; size_t bytes; const Void_t *caller;
#endif
{
  INTERNAL_SIZE_T nb;
  Void_t* mem;

  if (alignment <= MALLOC_ALIGNMENT) return malloc_check(bytes, NULL);
  if (alignment <  MINSIZE) alignment = MINSIZE;

  checked_request2size(bytes+1, nb);
  (void)mutex_lock(&main_arena.mutex);
  mem = (top_check() >= 0) ? _int_memalign(&main_arena, alignment, bytes+1) :
    NULL;
  (void)mutex_unlock(&main_arena.mutex);
  return mem2mem_check(mem, bytes);
}

#if !defined NO_THREADS && USE_STARTER

/* The following hooks are used when the global initialization in
   ptmalloc_init() hasn't completed yet. */

static Void_t*
#if __STD_C
malloc_starter(size_t sz, const Void_t *caller)
#else
malloc_starter(sz, caller) size_t sz; const Void_t *caller;
#endif
{
  Void_t* victim;

  ptmalloc_init_minimal();
  victim = _int_malloc(&main_arena, sz);

  return victim ? BOUNDED_N(victim, sz) : 0;
}

static Void_t*
#if __STD_C
memalign_starter(size_t align, size_t sz, const Void_t *caller)
#else
memalign_starter(align, sz, caller) size_t align, sz; const Void_t *caller;
#endif
{
  Void_t* victim;

  ptmalloc_init_minimal();
  victim = _int_memalign(&main_arena, align, sz);

  return victim ? BOUNDED_N(victim, sz) : 0;
}

static void
#if __STD_C
free_starter(Void_t* mem, const Void_t *caller)
#else
free_starter(mem, caller) Void_t* mem; const Void_t *caller;
#endif
{
  mchunkptr p;

  if(!mem) return;
  p = mem2chunk(mem);
#if HAVE_MMAP
  if (chunk_is_mmapped(p)) {
    munmap_chunk(p);
    return;
  }
#endif
  _int_free(&main_arena, mem);
}

#endif /* !defined NO_THREADS && USE_STARTER */


/* Get/set state: malloc_get_state() records the current state of all
   malloc variables (_except_ for the actual heap contents and `hook'
   function pointers) in a system dependent, opaque data structure.
   This data structure is dynamically allocated and can be free()d
   after use.  malloc_set_state() restores the state of all malloc
   variables to the previously obtained state.  This is especially
   useful when using this malloc as part of a shared library, and when
   the heap contents are saved/restored via some other method.  The
   primary example for this is GNU Emacs with its `dumping' procedure.
   `Hook' function pointers are never saved or restored by these
   functions, with two exceptions: If malloc checking was in use when
   malloc_get_state() was called, then malloc_set_state() calls
   __malloc_check_init() if possible; if malloc checking was not in
   use in the recorded state but the user requested malloc checking,
   then the hooks are reset to 0.  */

#define MALLOC_STATE_MAGIC   0x444c4541l
#define MALLOC_STATE_VERSION (0*0x100l + 2l) /* major*0x100 + minor */

struct malloc_save_state {
  long          magic;
  long          version;
  mbinptr       av[NBINS * 2 + 2];
  char*         sbrk_base;
  int           sbrked_mem_bytes;
  unsigned long trim_threshold;
  unsigned long top_pad;
  unsigned int  n_mmaps_max;
  unsigned long mmap_threshold;
  int           check_action;
  unsigned long max_sbrked_mem;
  unsigned long max_total_mem;
  unsigned int  n_mmaps;
  unsigned int  max_n_mmaps;
  unsigned long mmapped_mem;
  unsigned long max_mmapped_mem;
  int           using_malloc_checking;
};

Void_t*
public_gET_STATe(void)
{
  struct malloc_save_state* ms;
  int i;
  mbinptr b;

  ms = (struct malloc_save_state*)public_mALLOc(sizeof(*ms));
  if (!ms)
    return 0;
  (void)mutex_lock(&main_arena.mutex);
  malloc_consolidate(&main_arena);
  ms->magic = MALLOC_STATE_MAGIC;
  ms->version = MALLOC_STATE_VERSION;
  ms->av[0] = 0;
  ms->av[1] = 0; /* used to be binblocks, now no longer used */
  ms->av[2] = top(&main_arena);
  ms->av[3] = 0; /* used to be undefined */
  for(i=1; i<NBINS; i++) {
    b = bin_at(&main_arena, i);
    if(first(b) == b)
      ms->av[2*i+2] = ms->av[2*i+3] = 0; /* empty bin */
    else {
      ms->av[2*i+2] = first(b);
      ms->av[2*i+3] = last(b);
    }
  }
  ms->sbrk_base = mp_.sbrk_base;
  ms->sbrked_mem_bytes = main_arena.system_mem;
  ms->trim_threshold = mp_.trim_threshold;
  ms->top_pad = mp_.top_pad;
  ms->n_mmaps_max = mp_.n_mmaps_max;
  ms->mmap_threshold = mp_.mmap_threshold;
  ms->check_action = check_action;
  ms->max_sbrked_mem = main_arena.max_system_mem;
#ifdef NO_THREADS
  ms->max_total_mem = mp_.max_total_mem;
#else
  ms->max_total_mem = 0;
#endif
  ms->n_mmaps = mp_.n_mmaps;
  ms->max_n_mmaps = mp_.max_n_mmaps;
  ms->mmapped_mem = mp_.mmapped_mem;
  ms->max_mmapped_mem = mp_.max_mmapped_mem;
  ms->using_malloc_checking = using_malloc_checking;
  (void)mutex_unlock(&main_arena.mutex);
  return (Void_t*)ms;
}

int
public_sET_STATe(Void_t* msptr)
{
  struct malloc_save_state* ms = (struct malloc_save_state*)msptr;
  int i;
  mbinptr b;

  disallow_malloc_check = 1;
  ptmalloc_init();
  if(ms->magic != MALLOC_STATE_MAGIC) return -1;
  /* Must fail if the major version is too high. */
  if((ms->version & ~0xffl) > (MALLOC_STATE_VERSION & ~0xffl)) return -2;
  (void)mutex_lock(&main_arena.mutex);
  /* There are no fastchunks.  */
  clear_fastchunks(&main_arena);
  set_max_fast(&main_arena, DEFAULT_MXFAST);
  for (i=0; i<(int)NFASTBINS; ++i)
    main_arena.fastbins[i] = 0;
  for (i=0; i<(int)BINMAPSIZE; ++i)
    main_arena.binmap[i] = 0;
  top(&main_arena) = ms->av[2];
  main_arena.last_remainder = 0;
  for(i=1; i<NBINS; i++) {
    b = bin_at(&main_arena, i);
    if(ms->av[2*i+2] == 0) {
      assert(ms->av[2*i+3] == 0);
      first(b) = last(b) = b;
    } else {
      if(i<(int)NSMALLBINS || ((int)largebin_index(chunksize(ms->av[2*i+2]))==i &&
			  (int)largebin_index(chunksize(ms->av[2*i+3]))==i)) {
	first(b) = ms->av[2*i+2];
	last(b) = ms->av[2*i+3];
	/* Make sure the links to the bins within the heap are correct.  */
	first(b)->bk = b;
	last(b)->fd = b;
	/* Set bit in binblocks.  */
	mark_bin(&main_arena, i);
      } else {
	/* Oops, index computation from chunksize must have changed.
           Link the whole list into unsorted_chunks.  */
	first(b) = last(b) = b;
	b = unsorted_chunks(&main_arena);
	ms->av[2*i+2]->bk = b;
	ms->av[2*i+3]->fd = b->fd;
	b->fd->bk = ms->av[2*i+3];
	b->fd = ms->av[2*i+2];
      }
    }
  }
  mp_.sbrk_base = ms->sbrk_base;
  main_arena.system_mem = ms->sbrked_mem_bytes;
  mp_.trim_threshold = ms->trim_threshold;
  mp_.top_pad = ms->top_pad;
  mp_.n_mmaps_max = ms->n_mmaps_max;
  mp_.mmap_threshold = ms->mmap_threshold;
  check_action = ms->check_action;
  main_arena.max_system_mem = ms->max_sbrked_mem;
#ifdef NO_THREADS
  mp_.max_total_mem = ms->max_total_mem;
#endif
  mp_.n_mmaps = ms->n_mmaps;
  mp_.max_n_mmaps = ms->max_n_mmaps;
  mp_.mmapped_mem = ms->mmapped_mem;
  mp_.max_mmapped_mem = ms->max_mmapped_mem;
  /* add version-dependent code here */
  if (ms->version >= 1) {
    /* Check whether it is safe to enable malloc checking, or whether
       it is necessary to disable it.  */
    if (ms->using_malloc_checking && !using_malloc_checking &&
        !disallow_malloc_check)
      __malloc_check_init ();
    else if (!ms->using_malloc_checking && using_malloc_checking) {
      __malloc_hook = 0;
      __free_hook = 0;
      __realloc_hook = 0;
      __memalign_hook = 0;
      using_malloc_checking = 0;
    }
  }
  check_malloc_state(&main_arena);

  (void)mutex_unlock(&main_arena.mutex);
  return 0;
}


/*-------------------------------------------------------------------------
   OMPI change: Per
   http://www.gnu.org/software/libc/manual/html_mono/libc.html#Hooks-for-Malloc,
   we can define the __malloc_initialize_hook variable to be a
   function that is invoked before the first allocation is ever
   performed.  We use this hook to wholly replace the underlying
   allocator to our own allocator if a few conditions are met.

   Remember that this hook is called probably at the very very very
   beginning of the process.  MCA parameters haven't been setup yet --
   darn near nothing has been setup yet.  Indeed, we're effectively in
   signal context because we can't call anything that calls malloc.
   So we can basically have some hard-coded tests for things to see if
   we want to setup to use our internal ptmalloc2 or not. */

static void *opal_memory_linux_malloc_hook(size_t sz, 
                                           const __malloc_ptr_t caller)
{
    return public_mALLOc(sz);
}

static void *opal_memory_linux_realloc_hook(Void_t* ptr, size_t sz, 
                                            const __malloc_ptr_t caller)
{
    return public_rEALLOc(ptr, sz);
}

static void *opal_memory_linux_memalign_hook(size_t alignment, size_t sz, 
                                             const __malloc_ptr_t caller)
{
    return public_mEMALIGn(alignment, sz);
}

static void opal_memory_linux_free_hook(__malloc_ptr_t __ptr,
                                        const __malloc_ptr_t caller)
{
    public_fREe(__ptr);
}

typedef enum {
    RESULT_NO,
    RESULT_YES,
    RESULT_RUNTIME,
    RESULT_NOT_FOUND
} check_result_t;

static check_result_t check(const char *name)
{
    char *s = getenv(name);
    if (NULL == s) {
        return RESULT_NOT_FOUND;
    }

    if ('0' == s[0] && '\0' == s[1]) {
        /* A value of 0 means "don't use!" */
        return RESULT_NO;
    } else if ('-' == s[0] && '1' == s[1] && '\0' == s[2]) {
        /* A value of -1 means "use it if it would be advantageous */
        return RESULT_RUNTIME;
    } else {
        /* Any other value means "use the hooks, Luke!" */
        return RESULT_YES;
    }
}

/* OMPI's init function */
static void opal_memory_linux_malloc_init_hook(void)
{
    check_result_t r1, lp, lpp;
    bool want_rcache = false, found_driver = false;

    /* First, check for a FAKEROOT environment.  If we're in a
       fakeroot, then access() (and likely others) have been replaced
       and are not safe to call here in this pre-main environment.  So
       check for the environment markers that we're in a FAKEROOT.
       And if so, return immediately.

       Note that this check was inspired by a problem with Debian's
       "fakeroot" build environment that allocates memory during
       stat() (see http://bugs.debian.org/531522).  It may not be
       necessary any more since we're using access(), not stat().  But
       we'll leave the check, anyway. */
    if (getenv("FAKEROOTKEY") != NULL ||
        getenv("FAKED_MODE") != NULL) {
        return;
    }

    /* Next, check if ummunotify is present on the system. If it is,
       then we don't need to do the following ptmalloc2 hacks.
       open/mmap on the device may fail during init, but if /dev/ummunotify
       exists, we assume that the user/administrator *wants* to use
       ummunotify. */
    if (access("/dev/ummunotify", F_OK) == 0) {
        return;
    }

    /* Yes, checking for an MPI MCA parameter here is an abstraction
       violation.  Cope.  Yes, even checking for *any* MCA parameter
       here (without going through the MCA param API) is an
       abstraction violation.  Fricken' cope, will ya?
       (unfortunately, there's really no good way to do this other
       than this abstraction violation :-( ) */
    lp = check("OMPI_MCA_mpi_leave_pinned");
    lpp = check("OMPI_MCA_mpi_leave_pinned_pipeline");

    /* See if we want to disable this component.  */
    r1 = check("OMPI_MCA_memory_linux_disable");
    if (RESULT_NOT_FOUND != r1 && RESULT_NO != r1) {
        return;
    }

    /* Look for sentinel files (directories) to see if various network
       drivers are loaded (yes, I know, further abstraction
       violations...).

       * All OpenFabrics devices will have files in
         /sys/class/infiniband (even iWARP)
       * Open-MX doesn't currently use a reg cache, but it might
         someday.  So be conservative and check for /dev/open-mx.
       * MX will have one or more of /dev/myri[0-9].  Yuck.
     */
    if (0 == access("/sys/class/infiniband", F_OK) ||
        0 == access("/dev/open-mx", F_OK) ||
        0 == access("/dev/myri0", F_OK) ||
        0 == access("/dev/myri1", F_OK) ||
        0 == access("/dev/myri2", F_OK) ||
        0 == access("/dev/myri3", F_OK) ||
        0 == access("/dev/myri4", F_OK) ||
        0 == access("/dev/myri5", F_OK) ||
        0 == access("/dev/myri6", F_OK) ||
        0 == access("/dev/myri7", F_OK) ||
        0 == access("/dev/myri8", F_OK) ||
        0 == access("/dev/myri9", F_OK) ||
        0 == access("/dev/ipath", F_OK) ||
        0 == access("/dev/kgni0", F_OK)) {
        found_driver = true;
    }
    
    /* Simple combination of the results of these two environment
       variables (if both "yes" and "no" are specified, then be
       conservative and assume "yes"):

       lp / lpp   yes   no   runtime   not found       
       yes        yes   yes  yes       yes
       no         yes   no   no        no
       runtime    yes   no   runtime   runtime
       not found  yes   no   runtime   runtime
     */
    if (RESULT_YES == lp || RESULT_YES == lpp) {
        want_rcache = true;
    } else if (RESULT_NO == lp || RESULT_NO == lpp) {
        want_rcache = false;
    } else {
        want_rcache = found_driver;
    }

    if (want_rcache) {
        /* Initialize ptmalloc */
        ptmalloc_init();

        /* Now set the hooks to point to our functions */
        __free_hook = opal_memory_linux_free_hook;
        __malloc_hook = opal_memory_linux_malloc_hook;
        __memalign_hook = opal_memory_linux_memalign_hook;
        __realloc_hook = opal_memory_linux_realloc_hook;
    }
}


/* OMPI change: prototype the function below, otherwise we'll get
   warnings about it not being declared (at least in developer/debug
   builds).  This function is not DECLSPEC'ed because we don't want it
   visible outside of this component (i.e., libopen-pal, since this
   component is never built as a DSO; it's always slurped into
   libopen-pal).  This declaration is not in malloc.h because this
   function only exists as a horrid workaround to force linkers to
   pull in this .o file (see explanation below).  */
void opal_memory_linux_hook_pull(bool *want_hooks);

/* OMPI change: add a dummy function here that will be called by the
   linux component open() function.  This dummy function is
   necessary for when OMPI is built as --disable-shared
   --enable-static --disable-dlopen, because we won't use
   -Wl,--export-dynamic when building OMPI.  So we need to ensure that
   not only that all the symbols in this file end up in libopen-pal.a,
   but they also end up in the final exectuable (so that
   __malloc_initialize_hook is there, overrides the weak symbol in
   glibc, ....etc.). */
void opal_memory_linux_hook_pull(bool *want_hooks)
{
    int val;

    /* Make this slightly less than a dummy function -- register the
       MCA parameter here (that way we keep the name of this MCA
       parameter here within this one, single file).  Register solely
       so that it shows up in ompi_info -- by the time we register it,
       the _malloc_init_hook() has almost certainly already fired, so
       whatever value was set via normal MCA mechanisms likely won't
       be see if it wasn't already see by the getenv() in the
       _malloc_init_hook(). */
    mca_base_param_source_t source;
    char *file;
    int p = mca_base_param_reg_int(&mca_memory_linux_component.super.memoryc_version,
                                   "disable",
                                   "If this MCA parameter is set to 1 **VIA ENVIRONMENT VARIABLE ONLY*** (this MCA parameter *CANNOT* be set in a file or on the mpirun command line!), this component will be disabled and will not attempt to use either ummunotify or memory hook support",
                                   false, false, 0, &val);

    /* We can at least warn if someone tried to set this in a file */
    if (p >= 0) {
        if (OPAL_SUCCESS == mca_base_param_lookup_source(p, &source, &file) &&
            (MCA_BASE_PARAM_SOURCE_DEFAULT != source &&
             MCA_BASE_PARAM_SOURCE_ENV != source)) {
            opal_show_help("help-opal-memory-linux.txt",
                           "disable incorrectly set", true,
                           "opal_linux_disable",
                           "opal_linux_disable", val,
                           MCA_BASE_PARAM_SOURCE_FILE == source ?
                           file : "override");
        } else {
            *want_hooks = OPAL_INT_TO_BOOL(!val);
        }
    }
}



/* OMPI change: This is the symbol to override to make the above
   function get fired during malloc initialization time. */
void (*__malloc_initialize_hook) (void) = 
    opal_memory_linux_malloc_init_hook;

/*
 * Local variables:
 * c-basic-offset: 4
 * End:
 */
