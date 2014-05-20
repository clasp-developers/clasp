/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "vt_comp.h"
#include "vt_defs.h"
#include "vt_env.h"
#include "vt_mallocwrap.h"
#include "vt_pform.h"
#include "vt_thrd.h"
#include "vt_trc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 *-----------------------------------------------------------------------------
 * macro for getting id of calling thread
 *-----------------------------------------------------------------------------
 */

#define GET_THREAD_ID(tid) \
  VT_CHECK_THREAD;         \
  (tid) = VT_MY_THREAD

struct s1 {
  long l1;
  long l2;
  double d1;
  double d2;
  long isseen;
  char *c;
  void *p1;
  long lineno;
  void *p2;
  struct s1 *p3;
  int fid;
  int rid;
  char *file;
  char *rout;
};

static int rou_init = 1;       /* is initialization needed? */

/* With PGI 8 series compilers, a pragma is required to indicate that the
   compiler should save the registers, which was previously done explicitly.
   #pragma save_all_regs saves all registers, however, fewer registers can
   be saved with save_all_gp_regs, saved_used_gp_regs, etc.
 */

/*
 *-----------------------------------------------------------------------------
 * called during program initialization
 *-----------------------------------------------------------------------------
 */

#pragma save_all_regs
void __rouinit() {
}

/*
 *-----------------------------------------------------------------------------
 * called during program termination
 *-----------------------------------------------------------------------------
 */

#pragma save_all_regs
void __rouexit() {
}

/*
 *-----------------------------------------------------------------------------
 * called at the beginning of each profiled routine
 *-----------------------------------------------------------------------------
 */

#pragma save_all_regs
void ___rouent2(struct s1 *p) {
  uint32_t tid;
  uint64_t time;

  /* -- if not yet initialized, initialize VampirTrace -- */
  if (rou_init)
    {
      rou_init = 0;
      vt_open();
    }

  /* -- if VampirTrace already finalized, return -- */
  if ( !vt_is_alive ) return;

  /* -- get calling thread id -- */
  GET_THREAD_ID(tid);

  VT_SUSPEND_MALLOC_TRACING(tid);

  time = vt_pform_wtime();

  if (!p->isseen)
    {
      char* rname =  p->rout;
      char* modpos;

      /* fix opari output file names */
      if ( (modpos = strstr(p->file, ".mod.")) != NULL )
        {
          strcpy(modpos, modpos+4);
        }

#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_LOCK_IDS();
      if (!p->isseen)
        {
          p->fid = vt_def_scl_file(tid, p->file);
          p->rid = vt_def_region(tid, rname, p->fid, p->lineno,
                                 VT_NO_LNO, NULL, VT_FUNCTION);
          p->isseen = 1;
        }
      VTTHRD_UNLOCK_IDS();
#else /* VT_MT || VT_HYB */
      p->fid = vt_def_scl_file(tid, p->file);
      p->rid = vt_def_region(tid, rname, p->fid, p->lineno,
                             VT_NO_LNO, NULL, VT_FUNCTION);
      p->isseen = 1;
#endif /* VT_MT || VT_HYB */
    }

  /* write enter trace record */
  vt_enter(tid, &time, p->rid);

  VT_RESUME_MALLOC_TRACING(tid);
}

/*
 *-----------------------------------------------------------------------------
 * called at the end of each profiled routine
 *-----------------------------------------------------------------------------
 */

#pragma save_all_regs
void ___rouret2(void) {
  uint32_t tid;
  uint64_t time;

  /* -- if VampirTrace already finalized, return -- */
  if ( !vt_is_alive ) return;

  /* -- get calling thread id -- */
  GET_THREAD_ID(tid);

  VT_SUSPEND_MALLOC_TRACING(tid);

  time = vt_pform_wtime();
  vt_exit(tid, &time);

  VT_RESUME_MALLOC_TRACING(tid);
}

#pragma save_used_gp_regs
void ___linent2(void *l) {
}
