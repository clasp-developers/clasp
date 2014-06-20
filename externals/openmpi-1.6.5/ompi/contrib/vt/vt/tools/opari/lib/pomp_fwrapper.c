/*************************************************************************/
/* OPARI Version 1.1                                                     */
/* Copyright (c) 2001-2005                                                    */
/* Forschungszentrum Juelich, Zentralinstitut fuer Angewandte Mathematik */
/*************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include "pomp_lib.h"
#include "vt_fbindings.h"

/* macro for one-step declaration and definition of functions */
#define DEF_FPOMP_FUNC(function)  \
function; /* declaration */ \
function  /* definition */

/*
 * Fortran wrappers calling the C versions
 */

DEF_FPOMP_FUNC(void POMP_Atomic_enter_f(int* id)) {
  if ( pomp_tracing ) POMP_Atomic_enter(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_atomic_enter, POMP_ATOMIC_ENTER,
			   POMP_Atomic_enter_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Atomic_exit_f(int* id)) {
  if ( pomp_tracing ) POMP_Atomic_exit(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_atomic_exit, POMP_ATOMIC_EXIT,
			   POMP_Atomic_exit_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Barrier_enter_f(int* id)) {
  if ( pomp_tracing ) POMP_Barrier_enter(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_barrier_enter, POMP_BARRIER_ENTER,
			   POMP_Barrier_enter_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Barrier_exit_f(int* id)) {
  if ( pomp_tracing ) POMP_Barrier_exit(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_barrier_exit, POMP_BARRIER_EXIT,
			   POMP_Barrier_exit_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Flush_enter_f(int* id)) {
  if ( pomp_tracing ) POMP_Flush_enter(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_flush_enter, POMP_FLUSH_ENTER,
			   POMP_Flush_enter_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Flush_exit_f(int* id)) {
  if ( pomp_tracing ) POMP_Flush_exit(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_flush_exit, POMP_FLUSH_EXIT,
			   POMP_Flush_exit_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Critical_begin_f(int* id)) {
  if ( pomp_tracing ) POMP_Critical_begin(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_critical_begin, POMP_CRITICAL_BEGIN,
			   POMP_Critical_begin_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Critical_end_f(int* id)) {
  if ( pomp_tracing ) POMP_Critical_end(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_critical_end, POMP_CRITICAL_END,
			   POMP_Critical_end_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Critical_enter_f(int* id)) {
  if ( pomp_tracing ) POMP_Critical_enter(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_critical_enter, POMP_CRITICAL_ENTER,
			   POMP_Critical_enter_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Critical_exit_f(int* id)) {
  if ( pomp_tracing ) POMP_Critical_exit(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_critical_exit, POMP_CRITICAL_EXIT,
			   POMP_Critical_exit_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Do_enter_f(int* id)) {
  if ( pomp_tracing ) POMP_For_enter(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_do_enter, POMP_DO_ENTER,
			   POMP_Do_enter_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Do_exit_f(int* id)) {
  if ( pomp_tracing ) POMP_For_exit(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_do_exit, POMP_DO_EXIT,
			   POMP_Do_exit_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Master_begin_f(int* id)) {
  if ( pomp_tracing ) POMP_Master_begin(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_master_begin, POMP_MASTER_BEGIN,
			   POMP_Master_begin_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Master_end_f(int* id)) {
  if ( pomp_tracing ) POMP_Master_end(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_master_end, POMP_MASTER_END,
			   POMP_Master_end_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Parallel_begin_f(int* id)) {
  if ( pomp_tracing ) POMP_Parallel_begin(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_parallel_begin, POMP_PARALLEL_BEGIN,
			   POMP_Parallel_begin_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Parallel_end_f(int* id)) {
  if ( pomp_tracing ) POMP_Parallel_end(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_parallel_end, POMP_PARALLEL_END,
			   POMP_Parallel_end_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Parallel_fork_f(int* id)) {
  if ( !pomp_initialized ) POMP_Init();
  if ( pomp_tracing ) POMP_Parallel_fork(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_parallel_fork, POMP_PARALLEL_FORK,
			   POMP_Parallel_fork_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Parallel_join_f(int* id)) {
  if ( pomp_tracing ) POMP_Parallel_join(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_parallel_join, POMP_PARALLEL_JOIN,
			   POMP_Parallel_join_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Section_begin_f(int* id)) {
  if ( pomp_tracing ) POMP_Section_begin(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_section_begin, POMP_SECTION_BEGIN,
			   POMP_Section_begin_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Section_end_f(int* id)) {
  if ( pomp_tracing ) POMP_Section_end(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_section_end, POMP_SECTION_END,
			   POMP_Section_end_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Sections_enter_f(int* id)) {
  if ( pomp_tracing ) POMP_Sections_enter(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_sections_enter, POMP_SECTIONS_ENTER,
			   POMP_Sections_enter_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Sections_exit_f(int* id)) {
  if ( pomp_tracing ) POMP_Sections_exit(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_sections_exit, POMP_SECTIONS_EXIT,
			   POMP_Sections_exit_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Single_begin_f(int* id)) {
  if ( pomp_tracing ) POMP_Single_begin(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_single_begin, POMP_SINGLE_BEGIN,
			   POMP_Single_begin_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Single_end_f(int* id)) {
  if ( pomp_tracing ) POMP_Single_end(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_single_end, POMP_SINGLE_END,
			   POMP_Single_end_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Single_enter_f(int* id)) {
  if ( pomp_tracing ) POMP_Single_enter(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_single_enter, POMP_SINGLE_ENTER,
			   POMP_Single_enter_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Single_exit_f(int* id)) {
  if ( pomp_tracing ) POMP_Single_exit(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_single_exit, POMP_SINGLE_EXIT,
			   POMP_Single_exit_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Workshare_enter_f(int* id)) {
  if ( pomp_tracing ) POMP_Workshare_enter(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_workshare_enter, POMP_WORKSHARE_ENTER,
			   POMP_Workshare_enter_f, (int* id), (id))

DEF_FPOMP_FUNC(void POMP_Workshare_exit_f(int* id)) {
  if ( pomp_tracing ) POMP_Workshare_exit(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_workshare_exit, POMP_WORKSHARE_EXIT,
			   POMP_Workshare_exit_f, (int* id), (id))
