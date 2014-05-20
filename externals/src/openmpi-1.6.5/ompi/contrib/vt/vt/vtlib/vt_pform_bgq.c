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

#include "config.h"

#include <stdio.h>
#include <firmware/include/personality.h>
#include <hwi/include/common/uci.h>

#include "vt_pform.h"
#include "vt_defs.h"

#ifndef TIMER_PAPI_REAL_CYC
#  define TIMER_PAPI_REAL_CYC 10
#endif
#ifndef TIMER_PAPI_REAL_USEC
#  define TIMER_PAPI_REAL_USEC 11
#endif

#if TIMER != TIMER_GET_TIMEBASE && \
    TIMER != TIMER_PAPI_REAL_CYC && \
    TIMER != TIMER_PAPI_REAL_USEC
# error Unknown timer specified! Check the timer configuration in 'config.h'.
#endif

#if TIMER == TIMER_GET_TIMEBASE
# include <hwi/include/bqc/A2_inlines.h>
  static uint64_t vt_ticks_per_sec = 1;
#elif TIMER == TIMER_PAPI_REAL_CYC
  extern uint64_t vt_metric_clckrt(void);
  extern uint64_t vt_metric_real_cyc(void);
#elif TIMER == TIMER_PAPI_REAL_USEC
  extern uint64_t vt_metric_real_usec(void);
  static uint64_t vt_time_base = 0;
#endif

static int torus_coord[6];

static Personality_t mybgq;

/* platform specific initialization */
void vt_pform_init() {
  Kernel_GetPersonality(&mybgq, sizeof(Personality_t));
#if TIMER == TIMER_GET_TIMEBASE
  vt_ticks_per_sec = (uint64_t)mybgq.Kernel_Config.FreqMHz * 1000000LL;
#elif TIMER == TIMER_PAPI_REAL_USEC
  vt_time_base = vt_metric_real_usec();
#endif

  torus_coord[0] = mybgq.Network_Config.Acoord;
  torus_coord[1] = mybgq.Network_Config.Bcoord;
  torus_coord[2] = mybgq.Network_Config.Ccoord;
  torus_coord[3] = mybgq.Network_Config.Dcoord;
  torus_coord[4] = mybgq.Network_Config.Ecoord;
  torus_coord[5] = Kernel_ProcessorID();
}

/* directory of global file system  */
char* vt_pform_gdir() {
  return ".";
}

/* directory of local file system  */
char* vt_pform_ldir() {
#ifdef DEFAULT_PFORM_LDIR
  return DEFAULT_PFORM_LDIR;
#else
  return ".";
#endif
}

/* full path of executable  */
char* vt_pform_exec()
{
  return NULL;
}

/* clock resolution */
uint64_t vt_pform_clockres() {
#if TIMER == TIMER_GET_TIMEBASE
  return vt_ticks_per_sec;
#elif TIMER == TIMER_PAPI_REAL_CYC
  return vt_metric_clckrt();
#elif TIMER == TIMER_PAPI_REAL_USEC
  return 1000000LL;
#endif
}

/* local or global wall-clock time in seconds */
uint64_t vt_pform_wtime() {
#if TIMER == TIMER_GET_TIMEBASE
  return (uint64_t)GetTimeBase();
#elif TIMER == TIMER_PAPI_REAL_CYC
  return vt_metric_real_cyc();
#elif TIMER == TIMER_PAPI_REAL_USEC
  return vt_metric_real_usec() - vt_time_base;
#endif
}

/* unique numeric SMP-node identifier */
long vt_pform_node_id() {
  BG_UniversalComponentIdentifier uci = mybgq.Kernel_Config.UCI;
  /* use upper part of UCI (26bit; upto ComputeCard; ignore lower 38bit)
   * but only use the 20 bits (FFFFF) that describe row,col,mp,nb,cc */
  return ((uci>>38)&0xFFFFF);
}

/* unique string SMP-node identifier */
char* vt_pform_node_name() {
  static char buf[48];
  BG_UniversalComponentIdentifier uci = mybgq.Kernel_Config.UCI;
  unsigned int row, col, mp, nb, cc;
  bg_decodeComputeCardOnNodeBoardUCI(uci, &row, &col, &mp, &nb, &cc);
  sprintf(buf, "R%x%x-M%d-N%02x-J%02x <%d,%d,%d,%d,%d>", row, col, mp, nb, cc,
          torus_coord[0], torus_coord[1], torus_coord[2],
          torus_coord[3], torus_coord[4]);
  return buf;
}

/* number of CPUs */
int vt_pform_num_cpus() {
  return 64;
}

