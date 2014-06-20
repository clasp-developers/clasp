/*
 * Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
 *
 * Additional copyrights may follow.
 */
/* Malloc implementation for multiple threads; statistics printing.
   Copyright (C) 2004 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Wolfram Gloger <wg@malloc.de>, 2004.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

/* $Id: $ */

/* OMPI change: Name-shift all the internal symbols */
#include "opal/mca/memory/linux/rename.h"

#include <stdio.h>    /* needed for malloc_stats */

#include <malloc-machine.h>

#include "malloc.h"

/*
  Define HAVE_MMAP as true to optionally make malloc() use mmap() to
  allocate very large blocks.  These will be returned to the
  operating system immediately after a free(). Also, if mmap
  is available, it is used as a backup strategy in cases where
  MORECORE fails to provide space from system.

  This malloc is best tuned to work with mmap for large requests.
  If you do not have mmap, operations involving very large chunks (1MB
  or so) may be slower than you'd like.
*/

#ifndef HAVE_MMAP
#define HAVE_MMAP 1
#endif

#ifdef USE_DL_PREFIX

#define public_mSTATs    dlmalloc_stats

#else /* USE_DL_PREFIX */
#ifdef _LIBC

#define public_mSTATs    __malloc_stats

#else /* !_LIBC */

#define public_mSTATs    malloc_stats

#endif /* _LIBC */
#endif /* USE_DL_PREFIX */

/*
  malloc_stats();
  Prints on stderr the amount of space obtained from the system (both
  via sbrk and mmap), the maximum amount (which may be more than
  current if malloc_trim and/or munmap got called), and the current
  number of bytes allocated via malloc (or realloc, etc) but not yet
  freed. Note that this is the number of bytes allocated, not the
  number requested. It will be larger than the number requested
  because of alignment and bookkeeping overhead. Because it includes
  alignment wastage as being in use, this figure may be greater than
  zero even when no user-level chunks are allocated.

  The reported current and maximum system memory can be inaccurate if
  a program makes other calls to system memory allocation functions
  (normally sbrk) outside of malloc.

  malloc_stats prints only the most commonly interesting statistics.
  More information can be obtained by calling mallinfo.

*/
void     public_mSTATs __MALLOC_P((void));

/*
  ------------------------------ malloc_stats ------------------------------
*/

void public_mSTATs()
{
  int i;
  mstate ar_ptr;
  struct malloc_global_info mgi;
  struct malloc_arena_info mai;
  unsigned long in_use_b, system_b, avail_b;
#if defined(THREAD_STATS) && THREAD_STATS
  long stat_lock_direct = 0, stat_lock_loop = 0, stat_lock_wait = 0;
#endif

#if 0
  if(__malloc_initialized < 0)
    ptmalloc_init ();
#endif
  _int_get_global_info(&mgi);
  system_b = in_use_b = mgi.mmapped_mem;
#ifdef _LIBC
  _IO_flockfile (stderr);
  int old_flags2 = ((_IO_FILE *) stderr)->_flags2;
  ((_IO_FILE *) stderr)->_flags2 |= _IO_FLAGS2_NOTCANCEL;
#endif
  for (i=0; (ar_ptr = _int_get_arena(i)); i++) {
    _int_get_arena_info(ar_ptr, &mai);
    avail_b = mai.fastavail + mai.binavail + mai.top_size;
    fprintf(stderr, "Arena %d:\n", i);
    fprintf(stderr, "system bytes     = %10lu\n",
	    (unsigned long)mai.system_mem);
    fprintf(stderr, "in use bytes     = %10lu\n",
	    (unsigned long)(mai.system_mem - avail_b));
#if MALLOC_DEBUG > 1
    if (i > 0)
      dump_heap(heap_for_ptr(top(ar_ptr)));
#endif
    system_b += mai.system_mem;
    in_use_b += mai.system_mem - avail_b;
#if defined(THREAD_STATS) && THREAD_STATS
    stat_lock_direct += mai.stat_lock_direct;
    stat_lock_loop += mai.stat_lock_loop;
    stat_lock_wait += mai.stat_lock_wait;
#endif
  }
#if HAVE_MMAP
  fprintf(stderr, "Total (incl. mmap):\n");
#else
  fprintf(stderr, "Total:\n");
#endif
  fprintf(stderr, "system bytes     = %10lu\n", system_b);
  fprintf(stderr, "in use bytes     = %10lu\n", in_use_b);
#ifdef NO_THREADS
  fprintf(stderr, "max system bytes = %10lu\n",
	  (unsigned long)mgi.max_total_mem);
#endif
#if HAVE_MMAP
  fprintf(stderr, "max mmap regions = %10u\n", (unsigned int)mgi.max_n_mmaps);
  fprintf(stderr, "max mmap bytes   = %10lu\n",
	  (unsigned long)mgi.max_mmapped_mem);
#endif
#if defined(THREAD_STATS) && THREAD_STATS
  fprintf(stderr, "heaps created    = %10d\n",  mgi.stat_n_heaps);
  fprintf(stderr, "locked directly  = %10ld\n", stat_lock_direct);
  fprintf(stderr, "locked in loop   = %10ld\n", stat_lock_loop);
  fprintf(stderr, "locked waiting   = %10ld\n", stat_lock_wait);
  fprintf(stderr, "locked total     = %10ld\n",
          stat_lock_direct + stat_lock_loop + stat_lock_wait);
#endif
#ifdef _LIBC
  ((_IO_FILE *) stderr)->_flags2 |= old_flags2;
  _IO_funlockfile (stderr);
#endif
}

#ifdef _LIBC
weak_alias (__malloc_stats, malloc_stats)
#endif
