// Compile mps here
//
//
// There are some configuration settings that can be set in the
// top level Jamroot.jam file
// <define>CONFIG_VAR_COOL  - detailed, slow MPS debugging
// <define>CONFIG_PF_ANSI    - use the generic "ANSI" platform
// <define>CONFIG_THREAD_SINGLE - single threaded
// Don't do static analysis on the garbage collector code
#ifndef RUNNING_GC_BUILDER
#include <config.h>
#include <clasp/gctools/configure_memory.h>

#ifdef USE_MPS
#include <clasp/mps/code/mps.c>

extern mps_arena_t global_arena;

/* David Lovemore suggests (Oct 2017) that PoolOfAddr may not return a pool.
    If PoolOfAddr returns TRUE then it will set pool_return
    if it returns FALSE, we set pool_return to NULL and check it in the caller.
 */
mps_pool_t clasp_pool_of_addr(void* vaddr)
{
  Pool pool_return;
  Arena arena = (Arena)global_arena;
  Addr addr = (Addr)vaddr;
  ArenaEnter(arena);
  int success = PoolOfAddr(&pool_return,arena,addr);
  if (!success) pool_return = NULL;
  ArenaLeave(arena);
  return (mps_pool_t)pool_return;
}


mps_res_t clasp_scan_area_tagged(mps_ss_t ss,
                                 void* base, void* limit,
                                 void* closure)
{
  mps_scan_tag_t tag = closure;
  mps_word_t mask = tag->mask;
  mps_word_t pattern = tag->pattern;
  // gctools::pointer_tag_mask is #b101
  // gctools::pointer_tag_eq is   #b001
  //MPS_SCAN_AREA((tag_bits&gctools::pointer_tag_mask) == gctools::pointer_tag_eq);
  MPS_SCAN_AREA((tag_bits&POINTER_TAG_MASK) == POINTER_TAG_EQ);
  return MPS_RES_OK;
}



#endif
#endif // #ifndef RUNNING_GC_BUILDER
