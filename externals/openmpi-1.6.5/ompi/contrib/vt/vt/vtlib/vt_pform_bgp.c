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
#include <common/bgp_personality.h>
#include <common/bgp_personality_inlines.h>
#include <spi/kernel_interface.h>

#include "vt_pform.h"
#include "vt_defs.h"

#define BGP_GROUP_ON_NODEBOARD

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
  static uint64_t vt_ticks_per_sec = 1;
#elif TIMER == TIMER_PAPI_REAL_CYC
  extern uint64_t vt_metric_clckrt(void);
  extern uint64_t vt_metric_real_cyc(void);
#elif TIMER == TIMER_PAPI_REAL_USEC
  extern uint64_t vt_metric_real_usec(void);
  static uint64_t vt_time_base = 0;
#endif

static _BGP_Personality_t mybgp;

/* platform specific initialization */
void vt_pform_init() {
  Kernel_GetPersonality(&mybgp, sizeof(_BGP_Personality_t));
#if TIMER == TIMER_GET_TIMEBASE
  vt_ticks_per_sec = (uint64_t)BGP_Personality_clockMHz(&mybgp) * 1000000LL;
#elif TIMER == TIMER_PAPI_REAL_USEC
  vt_time_base = vt_metric_real_usec();
#endif
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
  return (uint64_t)_bgp_GetTimeBase();
#elif TIMER == TIMER_PAPI_REAL_CYC
  return vt_metric_real_cyc();
#elif TIMER == TIMER_PAPI_REAL_USEC
  return vt_metric_real_usec() - vt_time_base;
#endif
}

/* unique numeric SMP-node identifier */
long vt_pform_node_id() {
#ifdef BGP_GROUP_ON_NODEBOARD
  _BGP_UniversalComponentIdentifier uci;
  uci.UCI = mybgp.Kernel_Config.UniversalComponentIdentifier;
  /* use upper part of UCI (upto NodeCard, ignore lower 14bits)
   * but only use the 13 bits (1FFF) that describe row,col,mp,nc */
  return ((uci.UCI>>14)&0x1FFF);
#else
  return ( BGP_Personality_psetNum(&mybgp) *
           BGP_Personality_psetSize(&mybgp) +
           BGP_Personality_rankInPset(&mybgp)) * Kernel_ProcessCount()
           + Kernel_PhysicalProcessorID();
#endif
}

static void bgp_getNodeidString(const _BGP_Personality_t *p, char *buf) {
   _BGP_UniversalComponentIdentifier uci;
  uci.UCI = p->Kernel_Config.UniversalComponentIdentifier;

  if ((uci.ComputeCard.Component == _BGP_UCI_Component_ComputeCard) ||
      (uci.IOCard.Component == _BGP_UCI_Component_IOCard)) {

    unsigned row = uci.ComputeCard.RackRow;
    unsigned col = uci.ComputeCard.RackColumn;
    unsigned mp  = uci.ComputeCard.Midplane;
    unsigned nc  = uci.ComputeCard.NodeCard;
    
    if (row == 0xff)
      sprintf(buf, "Rxx-Mx-N%x", nc);
    else
      sprintf(buf, "R%x%x-M%d-N%x", row, col, mp, nc);
  } else {
    sprintf(buf, "R?\?-M?-N?");
  }
}

/* unique string SMP-node identifier */
char* vt_pform_node_name() {
#ifdef BGP_GROUP_ON_NODEBOARD
  static char buf[BGPPERSONALITY_MAX_LOCATION];
  bgp_getNodeidString(&mybgp, buf);
  return buf;
#else
  static char node[128];
  unsigned x = BGP_Personality_xCoord(&mybgp);
  unsigned y = BGP_Personality_yCoord(&mybgp);
  unsigned z = BGP_Personality_zCoord(&mybgp);

  sprintf(node, "node-%03d-%03d-%03d-%d", x, y, z, Kernel_PhysicalProcessorID());
  return node;
#endif
}

/* number of CPUs */
int vt_pform_num_cpus() {
#ifdef BGP_GROUP_ON_NODEBOARD
  return 32 * Kernel_ProcessCount();
#else
  return 1;
#endif
}
