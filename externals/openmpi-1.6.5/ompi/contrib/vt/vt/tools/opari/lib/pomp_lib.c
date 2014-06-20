/*************************************************************************/
/* OPARI Version 1.1                                                     */
/* Copyright (c) 2001-2005                                                    */
/* Forschungszentrum Juelich, Zentralinstitut fuer Angewandte Mathematik */
/*************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include "pomp_lib.h"
#include "vt_fbindings.h"

/*
 * Global variables
 */

int pomp_initialized = 0;
int pomp_tracing = 0;

/*
 * C pomp function library
 */

void POMP_Finalize() {
  static int pomp_finalize_called = 0;

  if ( ! pomp_finalize_called ) {
    pomp_finalize_called = 1;

    fprintf(stderr, "  0: finalize\n");
  }
}

void POMP_Init() {
  int i;
  static int pomp_init_called = 0;

  if ( ! pomp_init_called ) {
    pomp_init_called = 1;

    atexit(POMP_Finalize);
    fprintf(stderr, "  0: init\n");

    for(i=0; i<POMP_MAX_ID; ++i) {
      if ( pomp_rd_table[i] ) {
        pomp_rd_table[i]->data = 0;   /* <-- allocate space for
					     performance data here */
      }
    }
    pomp_tracing = 1;
  }
}

void POMP_Off() {
  pomp_tracing = 0;
}

void POMP_On() {
  pomp_tracing = 1;
}

void POMP_Atomic_enter(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: enter atomic\n", omp_get_thread_num());
  }
}

void POMP_Atomic_exit(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: exit  atomic\n", omp_get_thread_num());
  }
}

void POMP_Barrier_enter(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    if ( r->name[0] == 'b' )
      fprintf(stderr, "%3d: enter barrier\n", omp_get_thread_num());
    else
      fprintf(stderr, "%3d: enter implicit barrier of %s\n",
	      omp_get_thread_num(), r->name);
  }
}

void POMP_Barrier_exit(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    if ( r->name[0] == 'b' )
      fprintf(stderr, "%3d: exit  barrier\n", omp_get_thread_num());
    else
      fprintf(stderr, "%3d: exit  implicit barrier of %s\n",
	      omp_get_thread_num(), r->name);
  }
}

void POMP_Flush_enter(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: enter flush\n", omp_get_thread_num());
  }
}

void POMP_Flush_exit(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: exit  flush\n", omp_get_thread_num());
  }
}

void POMP_Critical_begin(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: begin critical %s\n",
            omp_get_thread_num(), r->sub_name);
  }
}

void POMP_Critical_end(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: end   critical %s\n",
            omp_get_thread_num(), r->sub_name);
  }
}

void POMP_Critical_enter(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: enter critical %s\n",
            omp_get_thread_num(), r->sub_name);
  }
}

void POMP_Critical_exit(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: exit  critical %s\n",
            omp_get_thread_num(), r->sub_name);
  }
}

void POMP_For_enter(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: enter for\n", omp_get_thread_num());
  }
}

void POMP_For_exit(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: exit  for\n", omp_get_thread_num());
  }
}

void POMP_Master_begin(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: begin master\n", omp_get_thread_num());
  }
}

void POMP_Master_end(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: end   master\n", omp_get_thread_num());
  }
}

void POMP_Parallel_begin(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: begin parallel\n", omp_get_thread_num());
  }
}

void POMP_Parallel_end(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: end   parallel\n", omp_get_thread_num());
  }
}

void POMP_Parallel_fork(struct ompregdescr* r) {
  if ( !pomp_initialized ) POMP_Init();
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: fork  parallel\n", omp_get_thread_num());
  }
}

void POMP_Parallel_join(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: join  parallel\n", omp_get_thread_num());
  }
}

void POMP_Section_begin(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: begin section\n", omp_get_thread_num());
  }
}

void POMP_Section_end(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: end   section\n", omp_get_thread_num());
  }
}

void POMP_Sections_enter(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: enter sections\n", omp_get_thread_num());
  }
}

void POMP_Sections_exit(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: exit  sections\n", omp_get_thread_num());
  }
}

void POMP_Single_begin(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: begin single\n", omp_get_thread_num());
  }
}

void POMP_Single_end(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: end   single\n", omp_get_thread_num());
  }
}

void POMP_Single_enter(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: enter single\n", omp_get_thread_num());
  }
}

void POMP_Single_exit(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: exit  single\n", omp_get_thread_num());
  }
}

void POMP_Workshare_enter(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: enter workshare\n", omp_get_thread_num());
  }
}

void POMP_Workshare_exit(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: exit  workshare\n", omp_get_thread_num());
  }
}

void POMP_Begin(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: begin region %s\n",
            omp_get_thread_num(), r->sub_name);
  }
}

void POMP_End(struct ompregdescr* r) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: end   region %s\n",
            omp_get_thread_num(), r->sub_name);
  }
}

/*
 *----------------------------------------------------------------
 * C Wrapper for OpenMP API
 *----------------------------------------------------------------
 */

void POMP_Init_lock(omp_lock_t *s) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: init lock\n", omp_get_thread_num());
  }
  omp_init_lock(s);
}

void POMP_Destroy_lock(omp_lock_t *s) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: destroy lock\n", omp_get_thread_num());
  }
  omp_destroy_lock(s);
}

void POMP_Set_lock(omp_lock_t *s) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: set lock\n", omp_get_thread_num());
  }
  omp_set_lock(s);
}

void POMP_Unset_lock(omp_lock_t *s) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: unset lock\n", omp_get_thread_num());
  }
  omp_unset_lock(s);
}

int  POMP_Test_lock(omp_lock_t *s) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: test lock\n", omp_get_thread_num());
  }
  return omp_test_lock(s);
}

void POMP_Init_nest_lock(omp_nest_lock_t *s) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: init nestlock\n", omp_get_thread_num());
  }
  omp_init_nest_lock(s);
}

void POMP_Destroy_nest_lock(omp_nest_lock_t *s) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: destroy nestlock\n", omp_get_thread_num());
  }
  omp_destroy_nest_lock(s);
}

void POMP_Set_nest_lock(omp_nest_lock_t *s) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: set nestlock\n", omp_get_thread_num());
  }
  omp_set_nest_lock(s);
}

void POMP_Unset_nest_lock(omp_nest_lock_t *s) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: unset nestlock\n", omp_get_thread_num());
  }
  omp_unset_nest_lock(s);
}

int  POMP_Test_nest_lock(omp_nest_lock_t *s) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: test nestlock\n", omp_get_thread_num());
  }
  return omp_test_nest_lock(s);
}

/*
 *----------------------------------------------------------------
 * Fortran  Wrapper for OpenMP API
 *----------------------------------------------------------------
 */

void POMP_Init_lock_f(omp_lock_t *s) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: init lock\n", omp_get_thread_num());
  }
  omp_init_lock(s);
} VT_GENERATE_F77_BINDINGS(pomp_init_lock, POMP_INIT_LOCK,
			   POMP_Init_lock_f,
			   (omp_lock_t *s),
			   ())

void POMP_Destroy_lock_f(omp_lock_t *s) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: destroy lock\n", omp_get_thread_num());
  }
  omp_destroy_lock(s);
} VT_GENERATE_F77_BINDINGS(pomp_destroy_lock, POMP_DESTROY_LOCK,
			   POMP_Destroy_lock_f,
			   (omp_lock_t *s),
			   ())

void POMP_Set_lock_f(omp_lock_t *s) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: set lock\n", omp_get_thread_num());
  }
  omp_set_lock(s);
} VT_GENERATE_F77_BINDINGS(pomp_set_lock, POMP_SET_LOCK,
			   POMP_Set_lock_f,
			   (omp_lock_t *s),
			   ())

void POMP_Unset_lock_f(omp_lock_t *s) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: unset lock\n", omp_get_thread_num());
  }
  omp_unset_lock(s);
} VT_GENERATE_F77_BINDINGS(pomp_unset_lock, POMP_UNSET_LOCK,
			   POMP_Unset_lock_f,
			   (omp_lock_t *s),
			   ())

int POMP_Test_lock_f(omp_lock_t *s) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: test lock\n", omp_get_thread_num());
  }
  return omp_test_lock(s);
} VT_GENERATE_F77_BINDINGS(pomp_test_lock, POMP_TEST_LOCK,
			   POMP_Test_lock_f,
			   (omp_lock_t *s),
			   ())

#ifndef __osf__
void POMP_Init_nest_lock_f(omp_nest_lock_t *s) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: init nestlock\n", omp_get_thread_num());
  }
  omp_init_nest_lock(s);
} VT_GENERATE_F77_BINDINGS(pomp_init_nest_lock, POMP_INIT_NEST_LOCK,
			   POMP_Init_nest_lock_f,
			   (omp_nest_lock_t *s),
			   ())

void POMP_Destroy_nest_lock_f(omp_nest_lock_t *s) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: destroy nestlock\n", omp_get_thread_num());
  }
  omp_destroy_nest_lock_f(s);
} VT_GENERATE_F77_BINDINGS(pomp_destroy_nest_lock, POMP_DESTROY_NEST_LOCK,
			   POMP_Destroy_nest_lock_f,
			   (omp_nest_lock_t *s),
			   ())

void POMP_Set_nest_lock_f(omp_nest_lock_t *s) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: set nestlock\n", omp_get_thread_num());
  }
  omp_set_nest_lock_f(s);
} VT_GENERATE_F77_BINDINGS(pomp_set_nest_lock, POMP_SET_NEST_LOCK,
			   POMP_Set_nest_lock_f,
			   (omp_nest_lock_t *s),
			   ())

void POMP_Unset_nest_lock_f(omp_nest_lock_t *s) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: unset nestlock\n", omp_get_thread_num());
  }
  omp_unset_nest_lock(s);
} VT_GENERATE_F77_BINDINGS(pomp_unset_nest_lock, POMP_UNSET_NEST_LOCK,
			   POMP_Unset_nest_lock_f,
			   (omp_nest_lock_t *s),
			   ())

int POMP_Test_nest_lock_f(omp_nest_lock_t *s) {
  if ( pomp_tracing ) {
    fprintf(stderr, "%3d: test nestlock\n", omp_get_thread_num());
  }
  return omp_test_nest_lock(s);
} VT_GENERATE_F77_BINDINGS(pomp_test_nest_lock, POMP_TEST_NEST_LOCK,
			   POMP_Test_nest_lock_f,
			   (omp_nest_lock_t *s),
			   ())
#endif

