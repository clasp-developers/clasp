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
 * Basic Fortran wrappers calling the C versions
 */

void POMP_Finalize_f(void);
void POMP_Init_f(void);
void POMP_Off_f(void);
void POMP_On_f(void);
void POMP_Begin_f(int* id);
void POMP_End_f(int* id);

void POMP_Finalize_f() {
  POMP_Finalize();
} VT_GENERATE_F77_BINDINGS(pomp_finalize, POMP_FINALIZE,
			   POMP_Finalize_f, (void), ())

void POMP_Init_f() {
  POMP_Init();
} VT_GENERATE_F77_BINDINGS(pomp_init, POMP_INIT,
			   POMP_Init_f, (void), ())

void POMP_Off_f() {
  pomp_tracing = 0;
} VT_GENERATE_F77_BINDINGS(pomp_off, POMP_OFF,
			   POMP_Off_f, (void), ())

void POMP_On_f() {
  pomp_tracing = 1;
} VT_GENERATE_F77_BINDINGS(pomp_on, POMP_ON,
			   POMP_On_f, (void), ())

void POMP_Begin_f(int* id) {
  if ( pomp_tracing ) POMP_Begin(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_begin, POMP_BEGIN,
			   POMP_Begin_f, (int* id), (id))  

void POMP_End_f(int* id) {
  if ( pomp_tracing ) POMP_End(pomp_rd_table[*id]);
} VT_GENERATE_F77_BINDINGS(pomp_end, POMP_END,
			   POMP_End_f, (int* id), (id))
