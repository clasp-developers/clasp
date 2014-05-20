/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/* Name-shift all the internal ptmalloc22 symbols to guarantee to not
   conflict / confuse / override the internal glibc symbols. */

#define __default_morecore opal_memory_ptmalloc2_default_morecore

#define _int_malloc opal_memory_ptmalloc2_int_malloc
#define _int_free opal_memory_ptmalloc2_int_free
#define _int_realloc opal_memory_ptmalloc2_int_realloc
#define _int_memalign opal_memory_ptmalloc2_int_memalign
#define _int_valloc opal_memory_ptmalloc2_int_valloc
#define _int_pvalloc opal_memory_ptmalloc2_int_pvalloc
#define _int_icalloc opal_memory_ptmalloc2_int_icalloc
#define _int_icomalloc opal_memory_ptmalloc2_int_icomalloc

#define mTRIm opal_memory_ptmalloc2_mTRIm
#define mUSABLe opal_memory_ptmalloc2_mUSABLe
#define mALLOPt opal_memory_ptmalloc2_mALLOPt

#define mem2mem_check opal_memory_ptmalloc2_mem2mem_check
#define top_check opal_memory_ptmalloc2_top_check
#define munmap_chunk opal_memory_ptmalloc2_munmap_chunk
#define mremap_chunk opal_memory_ptmalloc2_mremap_chunk

#define malloc_check opal_memory_ptmalloc2_malloc_check
#define free_check opal_memory_ptmalloc2_free_check
#define realloc_check opal_memory_ptmalloc2_realloc_check
#define memalign_check opal_memory_ptmalloc2_memalign_check

#define malloc_starter opal_memory_ptmalloc2_malloc_starter
#define memalign_starter opal_memory_ptmalloc2_memalign_starter
#define free_starter opal_memory_ptmalloc2_free_starter

#define malloc_atfork opal_memory_ptmalloc2_malloc_atfork
#define free_atfork opal_memory_ptmalloc2_free_atfork

#define _int_get_arena opal_memory_ptmalloc2_int_get_arena
#define _int_get_arena_info opal_memory_ptmalloc2_int_get_arena_info
#define _int_get_global_info opal_memory_ptmalloc2_int_get_global_info
#define _int_new_arena opal_memory_ptmalloc2_int_new_arena
#define __malloc_check_init opal_memory_ptmalloc2_malloc_check_init
#define malloc_stats opal_memory_ptmalloc2_malloc_stats

#define posix_memalign opal_memory_ptmalloc2_posix_memalign
