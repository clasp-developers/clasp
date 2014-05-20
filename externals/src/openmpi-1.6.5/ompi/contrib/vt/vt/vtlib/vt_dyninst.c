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
#include "vt_dyninst.h"
#include "vt_env.h"
#include "vt_error.h"
#include "vt_mallocwrap.h"
#include "vt_pform.h"
#include "vt_thrd.h"
#include "vt_trc.h"

#include "util/installdirs.h"

#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>

/* macro for getting id of calling thread */
#define GET_THREAD_ID(tid) \
  VT_CHECK_THREAD;         \
  (tid) = VT_MY_THREAD

/* flag: attaching the Dyninst mutator to the current process? */
uint8_t vt_dyn_attaching = 0;

/* flag: is initialization needed? */
static int dyn_init = 1;

/* flag: continue execution of mutatee? (set by cont_sighandler) */
static volatile int mutatee_cont = 0;
/* flag: mutator exited erroneously? (set by error_sighandler) */
static volatile int mutator_error = 0;

/* region id lookup table */
static uint32_t* rtab = NULL;

/* signal handler for continuing execution of mutatee */
static void cont_sighandler(int signum)
{
  mutatee_cont = 1;
}

/* signal handler for errors occurring during instrumentation */
static void error_sighandler(int signum)
{
  mutator_error = 1;
}

/* register new region (function or loop) */
static uint32_t register_region(uint32_t tid, const char* name,
                                const char* file, uint32_t lno, uint32_t loop)
{
  uint32_t fid;

  /* register file if available */
  if( file[0] )
  {
    fid = vt_def_scl_file(tid, file);
  }
  else
  {
    fid = VT_NO_ID;
    lno = VT_NO_LNO;
  }

  /* register region and store its identifier */
  return vt_def_region(tid, name, fid, lno, VT_NO_LNO, NULL,
                       loop ? VT_LOOP : VT_FUNCTION);
}

/* finalize Dyninst instrumentation interface */
static void dyn_finalize(void)
{
  if( dyn_init )
    return;

  /* free region id table */
  free(rtab);
  rtab = NULL;

  dyn_init = 1;
}

void vt_dyn_attach()
{
  uint32_t mutatee_pid;

  /* set flag that we're attaching the Dyninst mutator */
  vt_dyn_attaching = 1;

  /* get mutatee's PID */
  mutatee_pid = (uint32_t)getpid();

  vt_cntl_msg(1, "[%i]: Attaching instrumentor", mutatee_pid);

  /* VampirTrace must be not initialized at this point */
  vt_libassert(!vt_is_alive);

  /* install signal handler for continuing execution and abort */
  if( signal(VT_DYNINST_CONT_SIGNUM, cont_sighandler) == SIG_ERR )
    vt_error_msg("Could not install handler for signal "
                 "VT_DYNINST_CONT_SIGNUM");

  if( signal(VT_DYNINST_ERROR_SIGNUM, error_sighandler) == SIG_ERR )
    vt_error_msg("Could not install handler for signal "
                 "VT_DYNINST_ERROR_SIGNUM");

  /* the Dyninst attach library (libvt-dynatt) could be set by LD_PRELOAD;
     unset this environment variable to avoid recursion */
  putenv((char*)"LD_PRELOAD=");
  putenv((char*)"DYLD_INSERT_LIBRARIES="); /* equivalent on MacOS */

  /* attach Dyninst instrumentor on running executable */
  switch( fork() )
  {
    case -1:
    {
      vt_error_msg("Could not attach Dyninst instrumentor");
      break;
    }
    case 0:
    {
      int rc;
      char cmd[1024];
      char* filter = vt_env_filter_spec();
      char* shlibs = vt_env_dyn_shlibs();
      char* shlibs_arg = NULL;
      char* mutatee_path = NULL;

      /* restore original signal handler */
      signal(VT_DYNINST_CONT_SIGNUM, SIG_DFL);
      signal(VT_DYNINST_ERROR_SIGNUM, SIG_DFL);

      /* try to get pathname of the mutatee */
      vt_pform_init();
      mutatee_path = vt_env_apppath();

      /* replace all colons by commas in the list of shared libraries to
         be instrumented */
      if ( shlibs && strlen(shlibs) > 0 )
      {
        char* tk;
        shlibs_arg = (char*)calloc(strlen(shlibs)+2, sizeof(char));
        tk = strtok( shlibs, ":" );
        do
        {
           strcat(shlibs_arg, tk);
           strcat(shlibs_arg, ",");
        } while( (tk = strtok( 0, ":" )) );
        shlibs_arg[strlen(shlibs_arg)-1] = '\0';
      }

      /* compose mutator command */
      snprintf(cmd,
              sizeof(cmd)-1, "%s/vtdyn %s %s %s %s %s %s %s %s %s %s %s "
                             "-p %i %s",
              vt_installdirs_get(VT_INSTALLDIR_BINDIR),
              (vt_env_verbose() == 0) ? "-q" : "",
              (vt_env_verbose() >= 2) ? "-v" : "",
              filter ? "-f" : "", filter ? filter : "",
              shlibs_arg ? "-s" : "", shlibs_arg ? shlibs_arg : "",
              (vt_env_dyn_outer_loops()) ? "--outer-loops" : "",
              (vt_env_dyn_inner_loops()) ? "--inner-loops" : "",
              (vt_env_dyn_loop_iters()) ? "--loop-iters" : "",
              (vt_env_dyn_ignore_nodbg()) ? "--ignore-nodbg" : "",
              (vt_env_dyn_detach()) ? "" : "--nodetach",
              mutatee_pid,
              mutatee_path ? mutatee_path : "");

      if( shlibs_arg )
        free(shlibs_arg);

      /* start mutator */
      vt_cntl_msg(2, "[%i]: Executing %s", mutatee_pid, cmd);
      rc = system(cmd);

      /* kill mutatee, if an error occurred during instrumentation */
      if(rc != 0)
        kill(mutatee_pid, VT_DYNINST_ERROR_SIGNUM);

      exit(rc);

      break;
    }
    default:
    {
      /* wait until mutator sends the signal to continue execution */
      vt_cntl_msg(1, "[%i]: Waiting until instrumentation is done",
                  mutatee_pid);

      do { usleep(1000); } while(mutatee_cont == 0);

      if( !mutator_error )
      {
        /* restore original signal handler */
        signal(VT_DYNINST_CONT_SIGNUM, SIG_DFL);
        signal(VT_DYNINST_ERROR_SIGNUM, SIG_DFL);
      }
      else
      {
        vt_error_msg("An error occurred during instrumenting");
      }

      break;
    }
  }

  /* unset flag for attaching the Dyninst mutator */
  vt_dyn_attaching = 0;
}

void vt_dyn_start(uint32_t index, const char* name, const char* fname,
                  uint32_t lno, uint32_t loop)
{
  uint32_t tid;
  uint64_t time;
  uint32_t* rid;

  vt_libassert(index < VT_MAX_DYNINST_REGIONS);

  /* ignore events if VT is initializing */
  if( !dyn_init && !vt_is_alive )
    return;

  /* if not yet initialized, initialize Dyninst instrumentation interface */
  if( dyn_init )
  {
    dyn_init = 0;
    vt_open();
    vt_comp_finalize = dyn_finalize;
    VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);
    rtab = (uint32_t*)calloc(VT_MAX_DYNINST_REGIONS, sizeof(uint32_t));
    if( rtab == NULL )
      vt_error();
    VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
  }

  /* if VampirTrace is already finalized, return */
  if( !vt_is_alive )
    return;

  /* get calling thread id */
  GET_THREAD_ID(tid);

  VT_SUSPEND_MALLOC_TRACING(tid);

  time = vt_pform_wtime();

  /* get region identifier */
  rid = &(rtab[index]);
  if( *rid == 0 )
  {
    /* if region entered the first time, register region */
#if (defined(VT_MT) || defined(VT_HYB))
    VTTHRD_LOCK_IDS();
    if( *rid == 0 )
      *rid = register_region(tid, name, fname, lno, loop);
    VTTHRD_UNLOCK_IDS();
#else /* VT_MT || VT_HYB */
    *rid = register_region(tid, name, fname, lno, loop);
#endif /* VT_MT || VT_HYB */
  }

  /* write enter record */
  vt_enter(tid, &time, *rid);

  VT_RESUME_MALLOC_TRACING(tid);
}

void vt_dyn_end(uint32_t index)
{
  uint32_t tid;
  uint64_t time;

  vt_libassert(index < VT_MAX_DYNINST_REGIONS);

  /* if VampirTrace is already finalized, return */
  if( !vt_is_alive )
    return;

  /* if region id isn't present (enter not recognized) , return */
  if( rtab[index] == 0 )
    return;

  /* get calling thread id */
  GET_THREAD_ID(tid);

  VT_SUSPEND_MALLOC_TRACING(tid);

  time = vt_pform_wtime();

  /* write exit record */
  vt_exit(tid, &time);

  VT_RESUME_MALLOC_TRACING(tid);
}
