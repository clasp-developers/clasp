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

#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "vt_comp.h"
#include "vt_defs.h"
#include "vt_error.h"
#include "vt_mallocwrap.h"
#include "vt_pform.h"
#include "vt_thrd.h"
#include "vt_trc.h"

/*
 * Macro for getting id of calling thread
 */

#define GET_THREAD_ID(tid) \
  VT_CHECK_THREAD;         \
  (tid) = VT_MY_THREAD

/*
 * Subtypes for loops.
 * A loop can be a 'DO-LOOP' in Fortran or 'while-do'/'do-while' in C/C++.
 */

typedef enum
{
  DO_LOOP_SUBTYPE  = 1,
  WHILE_DO_SUBTYPE = 2,
  DO_WHILE_SUBTYPE = 3
} LoopSubType;

/*
 * Data structures for context information of the API calls.
 */

struct profile_init_struct
{
  char*   output_filename;
  int32_t phase_num;
  int32_t unique_name;
};

struct profile_gen_struct
{
  int64_t pu_handle;
  char*   file_name;
  char*   pu_name;
  int32_t id;
  int32_t linenum;
  int32_t endline;
  int32_t subtype;
  int32_t taken;
  char*   callee;
  int32_t target;
  int32_t num_target;
  void*   called_fun_address;
  void*   data;
};

/*
 * Data structure for list of pointers to stored data.
 * e.g. assigned region identifiers
 */

struct data_list_struct
{
  void** ptr;
  struct data_list_struct* next;
};

/* flag: is initialization needed? */
static int openuh_init = 1;

/* list of pointers to stored data */
static struct data_list_struct* data_list = NULL;

/*
 * Registers a new region.
 */

static void register_region(uint32_t tid, struct profile_gen_struct* d,
                            uint8_t rtype)
{
  struct data_list_struct* data;
  char* rname = d->pu_name;
  uint32_t fid = VT_NO_ID;
  uint32_t begln = VT_NO_LNO;
  uint32_t endln = VT_NO_LNO;

  /* -- register file, if available -- */
  if ( d->file_name != NULL )
  {
    fid = vt_def_scl_file(tid, d->file_name);
    begln = d->linenum;
    endln = d->endline;
  }

  /* -- generate region name, if region is a loop -- */
  if ( rtype == VT_LOOP )
  {
    char stname[10];

    rname = (char*)malloc(1024 * sizeof(char));
    if ( rname == NULL ) vt_error();

    switch ( d->subtype )
    {
      case DO_LOOP_SUBTYPE:
        strncpy(stname, "do_loop", 7+1);
        break;
      case WHILE_DO_SUBTYPE:
        strncpy(stname, "while_do", 8+1);
        break;
      case DO_WHILE_SUBTYPE:
        strncpy(stname, "do_while", 8+1);
        break;
      default:
        vt_libassert(0);
        break;
    }

    /* -- add source location to name, if available -- */
    if ( d->file_name != NULL )
    {
      snprintf(rname, 1023, "%s @%s:%i-%i",
               stname, basename(d->file_name), begln, endln);
    }
    /* -- otherwise, add the internal compiler id to name -- */
    else
    {
      snprintf(rname, 1023, "%s%i", stname, d->id);
    }
  }

  /* -- register region -- */
  d->data = (uint32_t*)malloc(sizeof(uint32_t));
  if ( d->data == NULL ) vt_error();
  *((uint32_t*)(d->data)) =
    vt_def_region(tid, rname, fid, begln, endln, NULL, rtype);

  /* -- free generated region name -- */
  if ( rtype == VT_LOOP )
    free( rname );

  /* -- add pointer to stored data (region identifier) to list -- */
  data = (struct data_list_struct*)malloc(sizeof(struct data_list_struct));
  if ( data == NULL ) vt_error();

  data->ptr = &(d->data);
  if ( data_list == NULL )
  {
    data->next = NULL;
    data_list = data;
  }
  else
  {
    data->next = data_list->next;
    data_list->next = data;
  }
}

/*
 * Finalizes the performance library.
 */

VT_DECLDEF(void __profile_finish(void))
{
  struct data_list_struct* tmp;

  /* -- free all stored data -- */
  while( data_list )
  {
    tmp = data_list->next;

    free(*(data_list->ptr));  /* stored data (=profile_gen_struct.data) */
    *(data_list->ptr) = NULL;
    free(data_list);          /* list entry */

    data_list = tmp;
  }
}

/*
 * Initializes the performance library.
 * It should be invoked once in the entire application.
 */

VT_DECLDEF(void __profile_init(struct profile_init_struct* d))
{
  (void)d;

  if ( openuh_init )
  {
    openuh_init = 0;
    vt_open();
    vt_comp_finalize = &__profile_finish;
  }
}

/*
 * Instrumented at the beginning of each procedure, method, function. 
 */

VT_DECLDEF(void __profile_invoke(struct profile_gen_struct* d))
{
  uint32_t tid;
  uint64_t time;

  /* -- if VampirTrace already finalized, return -- */
  if ( !vt_is_alive ) return;

  /* -- get calling thread id -- */
  GET_THREAD_ID(tid);

  VT_SUSPEND_MALLOC_TRACING(tid);

  time = vt_pform_wtime();

  /* -- get region identifier -- */
  if ( d->data == NULL )
  {
    /* -- region entered the first time, register region -- */
#if (defined(VT_MT) || defined(VT_HYB))
    VTTHRD_LOCK_IDS();
    if ( d->data == NULL )
      register_region(tid, d, VT_FUNCTION);
    VTTHRD_UNLOCK_IDS();
#else /* VT_MT || VT_HYB */
      register_region(tid, d, VT_FUNCTION);
#endif /* VT_MT || VT_HYB */
  }

  /* -- write enter record -- */
  vt_enter(tid, &time, *((uint32_t*)(d->data)));

  VT_RESUME_MALLOC_TRACING(tid);
}

/*
 * Instrumentation at the end of each procedure, method, function.
 * A procedure can have multiple exits.
 */

VT_DECLDEF(void __profile_invoke_exit(struct profile_gen_struct* d))
{
  uint32_t tid;
  uint64_t time;

  /* -- if VampirTrace already finalized, return -- */
  if ( !vt_is_alive ) return;

  /* -- get calling thread id -- */
  GET_THREAD_ID(tid);

  VT_SUSPEND_MALLOC_TRACING(tid);

  time = vt_pform_wtime();

  /* -- write exit record -- */
  if ( d->data != NULL ) {
    vt_exit(tid, &time);
  }

  VT_RESUME_MALLOC_TRACING(tid);
}

/*
 * Instrumentation at the beginning of a branch.
 * A branch can be of a subBranchType.
 */

VT_DECLDEF(void __profile_branch(struct profile_gen_struct* d))
{
  (void)d;
}

/*
 * Instrumentation at the beginning of a loop.
 * A loop can be subLoopType.
 */

VT_DECLDEF(void __profile_loop(struct profile_gen_struct* d))
{
  uint32_t tid;
  uint64_t time;

  /* -- if VampirTrace already finalized, return -- */
  if ( !vt_is_alive ) return;

  /* -- get calling thread id -- */
  GET_THREAD_ID(tid);

  VT_SUSPEND_MALLOC_TRACING(tid);

  time = vt_pform_wtime();

  /* -- get region identifier -- */
  if ( d->data == NULL )
  {
    /* -- loop entered the first time, register region -- */
#if (defined(VT_MT) || defined(VT_HYB))
    VTTHRD_LOCK_IDS();
    if ( d->data == NULL )
      register_region(tid, d, VT_LOOP);
    VTTHRD_UNLOCK_IDS();
#else /* VT_MT || VT_HYB */
    register_region(tid, d, VT_LOOP);
#endif /* VT_MT || VT_HYB */
  }

  /* -- write enter record -- */
  vt_enter(tid, &time, *((uint32_t*)(d->data)));

  VT_RESUME_MALLOC_TRACING(tid);
}

/*
 * Instrumentation inside the body of a loop.
 */

VT_DECLDEF(void __profile_loop_iter(struct profile_gen_struct* d))
{
  (void)d;
}

/*
 * Instrumentation at the end of each exit of the loop.
 * A loop can have several exits points, such stops or returns.
 */

VT_DECLDEF(void  __profile_loop_exit(struct profile_gen_struct* d))
{
  uint32_t tid;
  uint64_t time;

  /* -- if VampirTrace already finalized, return -- */
  if ( !vt_is_alive ) return;

  /* -- get calling thread id -- */
  GET_THREAD_ID(tid);

  VT_SUSPEND_MALLOC_TRACING(tid);

  time = vt_pform_wtime();

  /* -- write exit record -- */
  if ( d->data != NULL ) {
    vt_exit(tid, &time);
  }

  VT_RESUME_MALLOC_TRACING(tid);
}

/*
 * Instrumentation before a callsite.
 */

VT_DECLDEF(void __profile_call_entry(struct profile_gen_struct* d))
{
  (void)d;
}

/*
 * Instrumentation at the end of a call site.
 */

VT_DECLDEF(void __profile_call_exit(struct profile_gen_struct* d))
{
  (void)d;
}

/*
 * Instrumentation before an indirect call, such as pointer call.
 */

VT_DECLDEF(void __profile_icall_entry(struct profile_gen_struct* d))
{
  (void)d;
}

/*
 * Instrumentation after an indirect call.
 */

VT_DECLDEF(void __profile_icall_exit(struct profile_gen_struct* d))
{
  (void)d;
}

/*
 * Instrumentation before a computed GOTO in Fortran.
 */

VT_DECLDEF(void __profile_compgoto(struct profile_gen_struct* d))
{
  (void)d;
}

/*
 * Instrumentation at the beginning of a switch statement.
 */

VT_DECLDEF(void __profile_switch(struct profile_gen_struct* d))
{
  (void)d;
}

/*
 * Instrumentation at the end of the switch statement.
 */

VT_DECLDEF(void __profile_switch_exit(struct profile_gen_struct* d))
{
  (void)d;
}

/*
 * Instrumentation for each expression in the conditional statement evaluation.
 */

VT_DECLDEF(void __profile_short_circuit(struct profile_gen_struct* d))
{
  (void)d;
}

/*
 * Instrumentation for each floating point multiplication, division, modulus.
 */

VT_DECLDEF(void __profile_value(struct profile_gen_struct* d))
{
  (void)d;
}
