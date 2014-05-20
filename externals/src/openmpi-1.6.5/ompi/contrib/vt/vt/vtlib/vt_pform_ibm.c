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

#include "vt_defs.h"
#include "vt_error.h"
#include "vt_pform.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>
#include <unistd.h>

#ifndef VT_PROCDIR 
#  define VT_PROCDIR "/proc/"
#endif

#ifndef TIMER_PAPI_REAL_CYC
#  define TIMER_PAPI_REAL_CYC 10
#endif
#ifndef TIMER_PAPI_REAL_USEC
#  define TIMER_PAPI_REAL_USEC 11
#endif

#if TIMER != TIMER_POWER_REALTIME && \
    TIMER != TIMER_SWITCH_CLOCK && \
    TIMER != TIMER_PAPI_REAL_CYC && \
    TIMER != TIMER_PAPI_REAL_USEC
#  error Unknown timer specified! Check the timer configuration in 'config.h'.
#endif

#if TIMER == TIMER_SWITCH_CLOCK
# include <swclock.h>
  static swclock_handle_t vt_swclk;
# define NUMRETRY 100
#elif TIMER == TIMER_POWER_REALTIME
  static uint64_t vt_time_base = 0;
#elif TIMER == TIMER_PAPI_REAL_CYC
  extern uint64_t vt_metric_clckrt(void);
  extern uint64_t vt_metric_real_cyc(void);
#elif TIMER == TIMER_PAPI_REAL_USEC
  extern uint64_t vt_metric_real_usec(void);
  static uint64_t vt_time_base = 0;
#endif

static char* vt_exec = NULL;
static long vt_node_id = 0;

/* platform specific initialization */
void vt_pform_init() {
  int  pid = getpid();
  char exec_proc[512];
  int  hostid_retries;

#if TIMER == TIMER_SWITCH_CLOCK
  int i;
  for (i=0; i<NUMRETRY; i++) {
    if ( (vt_swclk = swclockInit()) != 0 ) break;
  }
#elif TIMER == TIMER_POWER_REALTIME
  timebasestruct_t t;
  read_real_time(&t, TIMEBASE_SZ);
  time_base_to_time(&t, TIMEBASE_SZ);
  vt_time_base = t.tb_high - (t.tb_high & 0xFFFF);
#elif TIMER == TIMER_PAPI_REAL_USEC
  vt_time_base = vt_metric_real_usec();
#endif

  /* get full path of executable */
  snprintf(exec_proc, sizeof (exec_proc), VT_PROCDIR"%d/object/a.out", pid);
  vt_exec = strdup(exec_proc);

  /* get unique numeric SMP-node identifier */
  hostid_retries = 0;
  while( !vt_node_id && (hostid_retries++ < VT_MAX_GETHOSTID_RETRIES) ) {
    vt_node_id = gethostid();
  }
  if (!vt_node_id)
    vt_error_msg("Maximum retries (%i) for gethostid exceeded!",
		 VT_MAX_GETHOSTID_RETRIES);
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
  return "/tmp";
#endif
}

/* full path of executable  */
char* vt_pform_exec() {
  return vt_exec;
}

/* clock resolution */
uint64_t vt_pform_clockres() {
#if TIMER == TIMER_SWITCH_CLOCK
  int i, incr;
  for(i=0; i<NUMRETRY; i++) {
    if ( (incr = swclockGetIncrement(vt_swclk)) != -1 ) return incr;
  }
  return 1;
#elif TIMER == TIMER_POWER_REALTIME
  return 1000000000LL;
#elif TIMER == TIMER_PAPI_REAL_CYC
  return vt_metric_clckrt();
#elif TIMER == TIMER_PAPI_REAL_USEC
  return 1000000LL;
#endif
}

/* local or global wall-clock time */
uint64_t vt_pform_wtime() {
#if TIMER == TIMER_SWITCH_CLOCK
  int i;
  int64_t t;
  for (i=0; i<NUMRETRY; i++) {
    if ( (t = swclockRead(vt_swclk)) != -1 ) return t;
  }
  return 0;
#elif TIMER == TIMER_POWER_REALTIME
  timebasestruct_t t;
  read_real_time(&t, TIMEBASE_SZ);
  time_base_to_time(&t, TIMEBASE_SZ);
  return ((t.tb_high - vt_time_base) * 1000000000LL) + t.tb_low;
#elif TIMER == TIMER_PAPI_REAL_CYC
  return vt_metric_real_cyc();
#elif TIMER == TIMER_PAPI_REAL_USEC
  return vt_metric_real_usec() - vt_time_base;
#endif
}

/* unique numeric SMP-node identifier */
long vt_pform_node_id() {
  return vt_node_id;
}

/* unique string SMP-node identifier */
char* vt_pform_node_name() {
  static char host_name[20];
  gethostname(host_name, 20);
  return host_name;              
}

/* number of CPUs */
int vt_pform_num_cpus() {
  return sysconf(_SC_NPROCESSORS_CONF);
}
