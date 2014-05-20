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
#include <bglpersonality.h>

#include "vt_pform.h"
#include "vt_defs.h"

#define BGL_GROUP_ON_NODEBOARD

#ifndef TIMER_PAPI_REAL_CYC
#  define TIMER_PAPI_REAL_CYC 10
#endif
#ifndef TIMER_PAPI_REAL_USEC
#  define TIMER_PAPI_REAL_USEC 11
#endif

#if TIMER != TIMER_RTS_GET_TIMEBASE && \
    TIMER != TIMER_PAPI_REAL_CYC && \
    TIMER != TIMER_PAPI_REAL_USEC
# error Unknown timer specified! Check the timer configuration in 'config.h'.
#endif

#if TIMER == TIMER_RTS_GET_TIMEBASE
# include <rts.h>
  static uint64_t vt_ticks_per_sec = 1;
#elif TIMER == TIMER_PAPI_REAL_CYC
  extern uint64_t vt_metric_clckrt(void);
  extern uint64_t vt_metric_real_cyc(void);
#elif TIMER == TIMER_PAPI_REAL_USEC
  extern uint64_t vt_metric_real_usec(void);
  static uint64_t vt_time_base = 0;
#endif

static BGLPersonality mybgl;

/* platform specific initialization */
void vt_pform_init() {
  rts_get_personality(&mybgl, sizeof(BGLPersonality));
#if TIMER == TIMER_RTS_GET_TIMEBASE
  vt_ticks_per_sec = (uint64_t)BGLPersonality_clockHz(&mybgl);
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
#if TIMER == TIMER_RTS_GET_TIMEBASE
  return vt_ticks_per_sec;
#elif TIMER == TIMER_PAPI_REAL_CYC
  return vt_metric_clckrt();
#elif TIMER == TIMER_PAPI_REAL_USEC
  return 1000000LL;
#endif
}

/* local or global wall-clock time */
uint64_t vt_pform_wtime() {
#if TIMER == TIMER_RTS_GET_TIMEBASE
  return (uint64_t)rts_get_timebase();
#elif TIMER == TIMER_PAPI_REAL_CYC
  return vt_metric_real_cyc();
#elif TIMER == TIMER_PAPI_REAL_USEC
  return vt_metric_real_usec() - vt_time_base;
#endif
}

/* unique numeric SMP-node identifier */
long vt_pform_node_id() {
#ifdef BGL_GROUP_ON_NODEBOARD
  return ((mybgl.location >> 6) & 0x1fff);
#else
  if ( BGLPersonality_virtualNodeMode(&mybgl) )
    return ( BGLPersonality_psetNum(&mybgl) *
           BGLPersonality_numNodesInPset(&mybgl) +
           BGLPersonality_rankInPset(&mybgl)) * 2
           + rts_get_processor_id();
  else
    return BGLPersonality_psetNum(&mybgl) *
           BGLPersonality_numNodesInPset(&mybgl) +
           BGLPersonality_rankInPset(&mybgl);
#endif
}

static void bgl_getLocString(const BGLPersonality *p, char *buf) {
  unsigned row = (p->location >> 15) & 0xf;
  unsigned col = (p->location >> 11) & 0xf;
  unsigned mp  = (p->location >> 10) & 1;
  unsigned nc  = (p->location >> 6) & 0xf;
  unsigned pc  = (p->location >> 1) & 0x1f;
  unsigned asic = p->location & 1;
  const char *asicname = (asic ? "U01" : "U11");
  if (row == 0xff)
    sprintf(buf, "Rxx-Mx-N%x-J%02d-%s", nc, pc, asicname);
  else
    sprintf(buf, "R%x%x-M%d-N%x-J%02d-%s", row, col, mp, nc, pc, asicname);
}

static void bgl_getNodeidString(const BGLPersonality *p, char *buf) {
  unsigned row = (p->location >> 15) & 0xf;
  unsigned col = (p->location >> 11) & 0xf;
  unsigned mp  = (p->location >> 10) & 1;
  unsigned nc  = (p->location >> 6) & 0xf;
  if (row == 0xff)
    sprintf(buf, "Rxx-Mx-N%x", nc);
  else
    sprintf(buf, "R%x%x-M%d-N%x", row, col, mp, nc);
}

/* unique string SMP-node identifier */
char* vt_pform_node_name() {
#ifdef BGL_GROUP_ON_NODEBOARD
  static char buf[BGLPERSONALITY_MAX_LOCATION];
  bgl_getNodeidString(&mybgl, buf);
  return buf;
#else
  static char node[128];
  unsigned x = BGLPersonality_xCoord(&mybgl);
  unsigned y = BGLPersonality_yCoord(&mybgl);
  unsigned z = BGLPersonality_zCoord(&mybgl);

  sprintf(node, "node-%03d-%03d-%03d-%d", x, y, z, rts_get_processor_id());

  /* -- BGL internal location string
  static char buf[BGLPERSONALITY_MAX_LOCATION];
  BGLPersonality_getLocationString(&mybgl, buf);
  -- */
  return node;              
#endif
}

/* number of CPUs */
int vt_pform_num_cpus() {
#ifdef BGL_GROUP_ON_NODEBOARD
  if ( BGLPersonality_virtualNodeMode(&mybgl) )
    return 64;
  else
    return 32;
#else
  return 1;
#endif
}
