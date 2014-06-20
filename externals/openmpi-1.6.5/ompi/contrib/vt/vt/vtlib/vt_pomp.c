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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libgen.h>

#include "pomp_lib.h"

#include "vt_defs.h"
#include "vt_env.h"
#include "vt_fbindings.h"
#include "vt_inttypes.h"
#include "vt_pform.h"
#include "vt_pomp.h"
#include "vt_ompreg.h"
#include "vt_trc.h"
#define VT_MT /* necessary to have VTThrd_registerThread() declared */
#include "vt_thrd.h"

/*
 * Global variables
 */

int pomp_initialized = 0;
int pomp_tracing = 0;

void POMP_Finalize() {
  static int pomp_finalize_called = 0;

  if ( ! pomp_finalize_called ) {
    pomp_finalize_called = 1;
    vt_close();
  }
}

void POMP_Init() {
  static int pomp_init_called = 0;
  static struct VTRegDescr rd_data_table[1000];
  char anno_rname[256]; /* annotated region name */
  int max_threads = omp_get_max_threads();
  int i;
  uint8_t rtype = VT_UNKNOWN;
  char* rname = "";
  const char* rdesc;

  if ( ! pomp_init_called ) {
    pomp_init_called = 1;

    vt_open();
    atexit(POMP_Finalize);

    /* register all threads in sequential order to make the VT thread ids equal
       to the OpenMP thread ids */
#   pragma omp parallel for ordered private(i)
    for(i = 0; i < max_threads; i++) {
#     pragma omp ordered
      VT_CHECK_THREAD;
    }

    /* register wrapper functions for OpenMP API */
    vt_omp_register();

    for(i = 0; i < POMP_MAX_ID; ++i) {
      if ( pomp_rd_table[i] ) {
        struct VTRegDescr* data = &rd_data_table[i];
        struct ompregdescr* r    = pomp_rd_table[i];      
        r->data = data;
	rdesc   = "OMP";

        /* -- register file --*/
        data->fid   = vt_def_scl_file(VT_CURRENT_THREAD, r->file_name);
        data->begln = r->begin_first_line;
        data->endln = r->end_last_line;
	data->sbrid = VT_NO_ID;

        if (strcmp(r->name, "region") == 0)  {
          rtype = VT_USER_REGION;
          rname = (char*)(r->sub_name);
	  rdesc   = VT_DEFAULT_REGION_GROUP;
        } else if (strcmp(r->name, "atomic") == 0) {
          rtype = VT_OMP_ATOMIC;
          rname = "!$omp atomic";
        } else if (strcmp(r->name, "barrier") == 0) {
          rtype = VT_OMP_BARRIER;
          rname = "!$omp barrier";
        } else if (strcmp(r->name, "critical") == 0) {
          rtype = VT_OMP_CRITICAL;
          rname = "!$omp critical";
          sprintf(anno_rname, "%s @%s:%d", "!$omp critical sblock",
                basename((char*)(r->file_name)), r->begin_first_line+1);
          data->sbrid =
            vt_def_region(VT_CURRENT_THREAD, anno_rname, data->fid,
                          r->begin_last_line+1, r->end_first_line-1, NULL,
                          VT_OMP_CRITICAL_SBLOCK);
        } else if (strcmp(r->name, "do") == 0) {
          rtype = VT_OMP_LOOP;
          rname = "!$omp do";
        } else if (strcmp(r->name, "flush") == 0) {
          rtype = VT_OMP_FLUSH;
          rname = "!$omp flush";
        } else if (strcmp(r->name, "for") == 0) {
          rtype = VT_OMP_LOOP;
          rname = "!$omp for";
        } else if (strcmp(r->name, "function") == 0)  {
          rtype = VT_FUNCTION;
          rname = (char*)(r->sub_name);
	  rdesc   = VT_DEFAULT_REGION_GROUP;
        } else if (strcmp(r->name, "master") == 0) {
          rtype = VT_OMP_MASTER;
          rname = "!$omp master";
        } else if (strcmp(r->name, "parallel") == 0)  {
          rtype = VT_OMP_PARALLEL;
          rname = "!$omp parallel";
        } else if (strcmp(r->name, "paralleldo") == 0)  {
          rtype = VT_OMP_PARALLEL;
          rname = "!$omp parallel";
          sprintf(anno_rname, "%s @%s:%d", "!$omp do",
                basename((char*)(r->file_name)), r->begin_first_line);
          data->sbrid =
            vt_def_region(VT_CURRENT_THREAD, anno_rname, data->fid, data->begln,
                          data->endln, NULL, VT_OMP_LOOP);
        } else if (strcmp(r->name, "parallelfor") == 0)  {
          rtype = VT_OMP_PARALLEL;
          rname = "!$omp parallel";
          sprintf(anno_rname, "%s @%s:%d", "!$omp for",
                basename((char*)(r->file_name)), r->begin_first_line);
          data->sbrid =
            vt_def_region(VT_CURRENT_THREAD, anno_rname, data->fid, data->begln,
                          data->endln, NULL, VT_OMP_LOOP);
        } else if (strcmp(r->name, "parallelsections") == 0)  {
          rtype = VT_OMP_PARALLEL;
          rname = "!$omp parallel";
          sprintf(anno_rname, "%s @%s:%d", "!$omp sections",
                basename((char*)(r->file_name)), r->begin_first_line);
          data->sbrid =
            vt_def_region(VT_CURRENT_THREAD, anno_rname, data->fid, data->begln,
                          data->endln, NULL, VT_OMP_SECTIONS);
        } else if (strcmp(r->name, "parallelworkshare") == 0)  {
          rtype = VT_OMP_PARALLEL;
          rname = "!$omp parallel";
          sprintf(anno_rname, "%s @%s:%d", "!$omp workshare",
                basename((char*)(r->file_name)), r->begin_first_line);
          data->sbrid =
            vt_def_region(VT_CURRENT_THREAD, anno_rname, data->fid, data->begln,
                          data->endln, NULL, VT_OMP_WORKSHARE);
        } else if (strcmp(r->name, "sections") == 0) {
          rtype = VT_OMP_SECTIONS;
          rname = "!$omp sections";
          sprintf(anno_rname, "%s @%s:%d", "!$omp section",
                basename((char*)(r->file_name)), r->begin_last_line);
          data->sbrid =
            vt_def_region(VT_CURRENT_THREAD, anno_rname, data->fid,
                          r->begin_last_line, r->end_first_line, NULL,
                          VT_OMP_SECTION);
        } else if (strcmp(r->name, "section") == 0) {
          /* NOT DEFINED BY POMP YET */
          /* rtype = VT_OMP_SECTION; */
          /* rname = "!$omp section"; */
        } else if (strcmp(r->name, "single") == 0) {
          rtype = VT_OMP_SINGLE;
          rname = "!$omp single";
          sprintf(anno_rname, "%s @%s:%d", "!$omp single sblock",
                basename((char*)(r->file_name)), r->begin_last_line+1);
          data->sbrid =
            vt_def_region(VT_CURRENT_THREAD, anno_rname, data->fid,
                          r->begin_last_line+1, r->end_first_line-1, NULL,
                          VT_OMP_SINGLE_SBLOCK);
        } else if (strcmp(r->name, "workshare") == 0) {
          rtype = VT_OMP_WORKSHARE;
          rname = "!$omp workshare";
        } else {
          rtype = VT_UNKNOWN;
          rname = (char*)(r->name);
        }

        if (strcmp(rdesc, "OMP") == 0) {
            sprintf(anno_rname, "%s @%s:%d", rname,
                basename((char*)(r->file_name)), r->begin_first_line);
            rname = anno_rname;
        }

        /* -- register region -- */
        data->rid = vt_def_region(VT_CURRENT_THREAD, rname, data->fid,
                                  data->begln, data->endln, NULL, rtype);

        if (rtype == VT_OMP_PARALLEL ||
            rtype == VT_OMP_LOOP     ||
            rtype == VT_OMP_SECTIONS ||
            rtype == VT_OMP_SINGLE   ||
            rtype == VT_OMP_WORKSHARE) {
          /* -- register implicit barrier -- */
          rname = "!$omp ibarrier";
          sprintf(anno_rname, "%s @%s:%d", rname,
                basename((char*)(r->file_name)), r->end_last_line);
          data->brid =
            vt_def_region(VT_CURRENT_THREAD, anno_rname, data->fid, data->endln,
                          data->endln, NULL, VT_OMP_IBARRIER);
        } else
          data->brid = VT_NO_ID;
      }
    }
    pomp_initialized = 1;
    pomp_tracing = vt_env_omptrace();
  }
}

void POMP_Off() {
  pomp_tracing = 0;
}

void POMP_On() {
  pomp_tracing = 1;
}

static uint32_t main_rid = VT_NO_ID;

void POMP_Begin(struct ompregdescr* r) {
  struct VTRegDescr* data = (struct VTRegDescr*)(r->data);

  if ( main_rid == VT_NO_ID ) main_rid = data->rid;
  if ( IS_POMP_TRACE_ON )
  {
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, data->rid);
  }
}

void POMP_End(struct ompregdescr* r) {
  struct VTRegDescr* data = (struct VTRegDescr*)(r->data);

  if ( IS_POMP_TRACE_ON )
  {
    uint64_t time;
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
  }
  if ( data->rid == main_rid ) POMP_Finalize();
}

void POMP_Atomic_enter(struct ompregdescr* r) {
  GUARDED_ENTER(rid);
}

void POMP_Atomic_exit(struct ompregdescr* r) {
  GUARDED_EXIT();
}

void POMP_Barrier_enter(struct ompregdescr* r) {
  GUARDED_COLL_ENTER_2('b', rid, brid);
}

void POMP_Barrier_exit(struct ompregdescr* r) {
  GUARDED_COLL_EXIT();
}

void POMP_Flush_enter(struct ompregdescr* r) {
  GUARDED_ENTER(rid);
}

void POMP_Flush_exit(struct ompregdescr* r) {
  GUARDED_EXIT();
}

void POMP_Critical_begin(struct ompregdescr* r) {
  GUARDED_ENTER(sbrid);
}

void POMP_Critical_end(struct ompregdescr* r) {
  GUARDED_EXIT();
}

void POMP_Critical_enter(struct ompregdescr* r) {
  GUARDED_ENTER(rid);
}

void POMP_Critical_exit(struct ompregdescr* r) {
  GUARDED_EXIT();
}

void POMP_For_enter(struct ompregdescr* r) {
  GUARDED_ENTER_2('p', sbrid, rid);
}

void POMP_For_exit(struct ompregdescr* r) {
  GUARDED_EXIT();
}

void POMP_Master_begin(struct ompregdescr* r) {
  GUARDED_ENTER(rid);
}

void POMP_Master_end(struct ompregdescr* r) {
  GUARDED_EXIT();
}

void POMP_Parallel_begin(struct ompregdescr* r) {
  vt_omp_parallel_begin(VT_CURRENT_THREAD);

  GUARDED_ENTER(rid);
}

void POMP_Parallel_begin2(struct ompregdescr* r, int* p) {
  /* actually, the parent thread ID should never be -1 */
  if( *p == -1 )
    vt_omp_parallel_begin(VT_CURRENT_THREAD);
  else
    vt_omp_parallel_begin2(VT_CURRENT_THREAD, (uint32_t)*p);

  GUARDED_ENTER(rid);
}

void POMP_Parallel_end(struct ompregdescr* r) {
  GUARDED_EXIT();

  vt_omp_parallel_end(VT_CURRENT_THREAD);
}

void POMP_Parallel_fork(struct ompregdescr* r) {
  if ( !pomp_initialized ) POMP_Init();

  vt_omp_fork(VT_CURRENT_THREAD);

  if ( IS_POMP_TRACE_ON ) {
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, vt_trc_regid[VT__TRC_OMPPREG]);
  }
}

void POMP_Parallel_fork2(struct ompregdescr* r, int* p) {
  if ( !pomp_initialized ) POMP_Init();

  vt_omp_fork2(VT_CURRENT_THREAD, (uint32_t*)p);

  if ( IS_POMP_TRACE_ON ) {
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, vt_trc_regid[VT__TRC_OMPPREG]);
  }
}

void POMP_Parallel_join(struct ompregdescr* r) {
  vt_omp_join(VT_CURRENT_THREAD);

  if ( IS_POMP_TRACE_ON ) {
    uint64_t time;
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
  }
}

void POMP_Section_begin(struct ompregdescr* r) {
  GUARDED_ENTER(sbrid);
}

void POMP_Section_end(struct ompregdescr* r) {
  GUARDED_EXIT();
}

void POMP_Sections_enter(struct ompregdescr* r) {
  GUARDED_ENTER_2('p', sbrid, rid);
}

void POMP_Sections_exit(struct ompregdescr* r) {
  GUARDED_EXIT();
}

void POMP_Single_begin(struct ompregdescr* r) {
  GUARDED_ENTER(sbrid);
}

void POMP_Single_end(struct ompregdescr* r) {
  GUARDED_EXIT();
}

void POMP_Single_enter(struct ompregdescr* r) {
  GUARDED_ENTER(rid);
}

void POMP_Single_exit(struct ompregdescr* r) {
  GUARDED_EXIT();
}

void POMP_Workshare_enter(struct ompregdescr* r) {
  GUARDED_ENTER_2('p', sbrid, rid);
}

void POMP_Workshare_exit(struct ompregdescr* r) {
  GUARDED_EXIT();
}

/*
 *----------------------------------------------------------------
 * C Wrapper for OpenMP API
 *----------------------------------------------------------------
 */

void POMP_Init_lock(omp_lock_t *s) {
  if ( !pomp_initialized ) POMP_Init();

  if ( IS_POMP_TRACE_ON ) {
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, vt_omp_regid[VT__OMP_INIT_LOCK]);
    omp_init_lock(s);
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
  } else {
    omp_init_lock(s);
  }
}

void POMP_Destroy_lock(omp_lock_t *s) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, vt_omp_regid[VT__OMP_DESTROY_LOCK]);
    omp_destroy_lock(s);
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
  } else {
    omp_destroy_lock(s);
  }
}

void POMP_Set_lock(omp_lock_t *s) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, vt_omp_regid[VT__OMP_SET_LOCK]);
    omp_set_lock(s);
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
  } else {
    omp_set_lock(s);
  }
}

void POMP_Unset_lock(omp_lock_t *s) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, vt_omp_regid[VT__OMP_UNSET_LOCK]);
    omp_unset_lock(s);
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
  } else {
    omp_unset_lock(s);
  }
}

int  POMP_Test_lock(omp_lock_t *s) {
  if ( IS_POMP_TRACE_ON ) {
    int result;
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, vt_omp_regid[VT__OMP_TEST_LOCK]);
    result = omp_test_lock(s);
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
    return result;     
  } else {
    return omp_test_lock(s);
  }
}

void POMP_Init_nest_lock(omp_nest_lock_t *s) {
  if ( !pomp_initialized ) POMP_Init();

  if ( IS_POMP_TRACE_ON ) {
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, vt_omp_regid[VT__OMP_INIT_NEST_LOCK]);
    omp_init_nest_lock(s);
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
  } else {
    omp_init_nest_lock(s);
  }
}

void POMP_Destroy_nest_lock(omp_nest_lock_t *s) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, vt_omp_regid[VT__OMP_DESTROY_NEST_LOCK]);
    omp_destroy_nest_lock(s);
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
  } else {
    omp_destroy_nest_lock(s);
  }
}

void POMP_Set_nest_lock(omp_nest_lock_t *s) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, vt_omp_regid[VT__OMP_SET_NEST_LOCK]);
    omp_set_nest_lock(s);
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
  } else {
    omp_set_nest_lock(s);
  }
}

void POMP_Unset_nest_lock(omp_nest_lock_t *s) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, vt_omp_regid[VT__OMP_UNSET_NEST_LOCK]);
    omp_unset_nest_lock(s);
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
  } else {
    omp_unset_nest_lock(s);
  }
}

int POMP_Test_nest_lock(omp_nest_lock_t *s) {
  if ( IS_POMP_TRACE_ON ) {
    int result;
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, vt_omp_regid[VT__OMP_TEST_NEST_LOCK]);
    result = omp_test_nest_lock(s);
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
    return result;
  } else {
    return omp_test_nest_lock(s);
  }
}

/*
 *----------------------------------------------------------------
 * Fortran Wrapper for OpenMP API
 *----------------------------------------------------------------
 */

VT_DECLDEF(void POMP_Init_lock_f(omp_lock_t *s)) {
  if ( !pomp_initialized ) POMP_Init();

  if ( IS_POMP_TRACE_ON ) {
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, vt_omp_regid[VT__OMP_INIT_LOCK]);
    omp_init_lock(s);
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
  } else {
    omp_init_lock(s);
  }
} VT_GENERATE_F77_BINDINGS(pomp_init_lock, POMP_INIT_LOCK,
			   POMP_Init_lock_f,
			   (omp_lock_t *s),
			   (s))

VT_DECLDEF(void POMP_Destroy_lock_f(omp_lock_t *s)) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, vt_omp_regid[VT__OMP_DESTROY_LOCK]);
    omp_destroy_lock(s);
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
  } else {
    omp_destroy_lock(s);
  }
} VT_GENERATE_F77_BINDINGS(pomp_destroy_lock, POMP_DESTROY_LOCK,
			   POMP_Destroy_lock_f,
			   (omp_lock_t *s),
			   (s))

VT_DECLDEF(void POMP_Set_lock_f(omp_lock_t *s)) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, vt_omp_regid[VT__OMP_SET_LOCK]);
    omp_set_lock(s);
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
  } else {
    omp_set_lock(s);
  }
} VT_GENERATE_F77_BINDINGS(pomp_set_lock, POMP_SET_LOCK,
			   POMP_Set_lock_f,
			   (omp_lock_t *s),
			   (s))

VT_DECLDEF(void POMP_Unset_lock_f(omp_lock_t *s)) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, vt_omp_regid[VT__OMP_UNSET_LOCK]);
    omp_unset_lock(s);
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
  } else {
    omp_unset_lock(s);
  }
} VT_GENERATE_F77_BINDINGS(pomp_unset_lock, POMP_UNSET_LOCK,
			   POMP_Unset_lock_f,
			   (omp_lock_t *s),
			   (s))

VT_DECLDEF(int POMP_Test_lock_f(omp_lock_t *s)) {
  if ( IS_POMP_TRACE_ON ) {
    int result;
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, vt_omp_regid[VT__OMP_TEST_LOCK]);
    result = omp_test_lock(s);
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
    return result;     
  } else {
    return omp_test_lock(s);
  }
} VT_GENERATE_F77_BINDINGS(pomp_test_lock, POMP_TEST_LOCK,
			   POMP_Test_lock_f,
			   (omp_lock_t *s),
			   (s))

#ifndef __osf__
VT_DECLDEF(void POMP_Init_nest_lock_f(omp_nest_lock_t *s)) {
  if ( !pomp_initialized ) POMP_Init();

  if ( IS_POMP_TRACE_ON ) {
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, vt_omp_regid[VT__OMP_INIT_NEST_LOCK]);
    omp_init_nest_lock(s);
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
  } else {
    omp_init_nest_lock(s);
  }
} VT_GENERATE_F77_BINDINGS(pomp_init_nest_lock, POMP_INIT_NEST_LOCK,
			   POMP_Init_nest_lock_f,
			   (omp_nest_lock_t *s),
			   (s))

VT_DECLDEF(void POMP_Destroy_nest_lock_f(omp_nest_lock_t *s)) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, vt_omp_regid[VT__OMP_DESTROY_NEST_LOCK]);
    omp_destroy_nest_lock(s);
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
  } else {
    omp_destroy_nest_lock(s);
  }
} VT_GENERATE_F77_BINDINGS(pomp_destroy_nest_lock, POMP_DESTROY_NEST_LOCK,
			   POMP_Destroy_nest_lock_f,
			   (omp_nest_lock_t *s),
			   (s))

VT_DECLDEF(void POMP_Set_nest_lock_f(omp_nest_lock_t *s)) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, vt_omp_regid[VT__OMP_SET_NEST_LOCK]);
    omp_set_nest_lock(s);
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
  } else {
    omp_set_nest_lock(s);
  }
} VT_GENERATE_F77_BINDINGS(pomp_set_nest_lock, POMP_SET_NEST_LOCK,
			   POMP_Set_nest_lock_f,
			   (omp_nest_lock_t *s),
			   (s))

VT_DECLDEF(void POMP_Unset_nest_lock_f(omp_nest_lock_t *s)) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, vt_omp_regid[VT__OMP_UNSET_NEST_LOCK]);
    omp_unset_nest_lock(s);
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
  } else {
    omp_unset_nest_lock(s);
  }
} VT_GENERATE_F77_BINDINGS(pomp_unset_nest_lock, POMP_UNSET_NEST_LOCK,
			   POMP_Unset_nest_lock_f,
			   (omp_nest_lock_t *s),
			   (s))

VT_DECLDEF(int POMP_Test_nest_lock_f(omp_nest_lock_t *s)) {
  if ( IS_POMP_TRACE_ON ) {
    int result;
    uint64_t time;
    time = vt_pform_wtime();
    vt_enter(VT_CURRENT_THREAD, &time, vt_omp_regid[VT__OMP_TEST_NEST_LOCK]);
    result = omp_test_nest_lock(s);
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
    return result;
  } else {
    return omp_test_nest_lock(s);
  }
} VT_GENERATE_F77_BINDINGS(pomp_test_nest_lock, POMP_TEST_NEST_LOCK,
			   POMP_Test_nest_lock_f,
			   (omp_nest_lock_t *s),
			   (s))

#endif
