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

#include "vt_pform.h"
#include "vt_error.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifndef VT_PROCDIR 
#  define VT_PROCDIR "/proc/"
#endif

#ifndef __LIBCATAMOUNT__
#  include <sys/types.h>
#  include <sys/stat.h>
#  include <fcntl.h>
#endif

#ifndef TIMER_PAPI_REAL_CYC
#  define TIMER_PAPI_REAL_CYC 10
#endif
#ifndef TIMER_PAPI_REAL_USEC
#  define TIMER_PAPI_REAL_USEC 11
#endif

#if TIMER != TIMER_DCLOCK && \
    TIMER != TIMER_CYCLE_COUNTER && \
    TIMER != TIMER_GETTIMEOFDAY && \
    TIMER != TIMER_CLOCK_GETTIME && \
    TIMER != TIMER_PAPI_REAL_CYC && \
    TIMER != TIMER_PAPI_REAL_USEC
# error Unknown timer specified! Check the timer configuration in 'config.h'.
#endif

#if TIMER == TIMER_DCLOCK
# include <catamount/dclock.h>
# include <catamount/data.h>
#elif TIMER == TIMER_CYCLE_COUNTER
  static uint64_t vt_ticks_per_sec = 1;
#elif TIMER == TIMER_CLOCK_GETTIME || TIMER == TIMER_GETTIMEOFDAY
# include <time.h>
# include <sys/time.h>
  static uint64_t vt_time_base = 0;
#elif TIMER == TIMER_PAPI_REAL_CYC
  extern uint64_t vt_metric_clckrt(void);
  extern uint64_t vt_metric_real_cyc(void);
#elif TIMER == TIMER_PAPI_REAL_USEC
  extern uint64_t vt_metric_real_usec(void);
  static uint64_t vt_time_base = 0;
#endif

static long vt_node_id = 0;
static char* vt_node_name = NULL;
static char* vt_exec = NULL;

/* platform specific initialization */
void vt_pform_init() {
  int  pid = getpid();
  char exec_proc[VT_PATH_MAX];
  char exec[VT_PATH_MAX];
  int  exec_len;

#if TIMER == TIMER_CYCLE_COUNTER
  FILE *cpuinfofp;
  char line[1024];
  if ((cpuinfofp = fopen(VT_PROCDIR "cpuinfo", "r")) == NULL)
    vt_error_msg("Cannot open file %s: %s", VT_PROCDIR"cpuinfo",
		 strerror(errno));
  while (fgets(line, sizeof (line), cpuinfofp))
  {
    if (!strncmp("cpu MHz", line, 7))
    {
      strtok(line, ":");

      vt_ticks_per_sec =
	strtol((char*) strtok(NULL, " \n"), (char**) NULL, 0) * 1000000LL;
    }
    else if (!strncmp("timebase", line, 8))
    {
      strtok(line, ":");

      vt_ticks_per_sec =
	strtol((char*) strtok(NULL, " \n"), (char**) NULL, 0);
    }
  }
  fclose(cpuinfofp);
#elif TIMER == TIMER_CLOCK_GETTIME
  struct timespec tp;
  clock_gettime(CLOCK_REALTIME, &tp);
  vt_time_base = tp.tv_sec - (tp.tv_sec & 0xFF);
#elif TIMER == TIMER_GETTIMEOFDAY
  struct timeval tp;
  gettimeofday(&tp, 0);
  vt_time_base = tp.tv_sec - (tp.tv_sec & 0xFFFF);
#elif TIMER == TIMER_PAPI_REAL_USEC
  vt_time_base = vt_metric_real_usec();
#endif

  /* get unique numeric/string SMP-node identifier */
#ifdef __LIBCATAMOUNT__
  vt_node_id = (long)_my_pnid;
  if (asprintf(&vt_node_name, "node%ld", vt_node_id) == -1)
    vt_error();
#else
  {
    char buf[256];
    ssize_t bytes;
    int fd;

    /* get numeric identifier */
    fd = open(VT_PROCDIR"cray_xt/nid", O_RDONLY);
    if (fd < 0) vt_error_msg("Cannot open file "VT_PROCDIR"cray_xt/nid: %s",
      strerror(errno));
    bytes = read(fd, buf, 256);
    if (bytes <= 0) vt_error_msg("Cannot read file "VT_PROCDIR"cray_xt/nid: %s",
      strerror(errno));
    close(fd);
    vt_node_id = atol(buf);
    
    /* get string identifier */
    fd = open(VT_PROCDIR"cray_xt/cname", O_RDONLY);
    if (fd < 0) vt_error_msg("Cannot open file "VT_PROCDIR"cray_xt/cname: %s",
      strerror(errno));
    bytes = read(fd, buf, 256);
    if (bytes <= 0) vt_error_msg("Cannot read file "VT_PROCDIR"cray_xt/cname: %s",
      strerror(errno));
    close(fd);
    vt_node_name = strdup(buf);
  }
#endif

  /* get full path of executable */
  snprintf(exec_proc, sizeof (exec_proc), VT_PROCDIR"%d/exe", pid);
  exec_len = readlink(exec_proc, exec, sizeof (exec)-1);
  if(exec_len != -1)
  {
    exec[exec_len] = '\0';
    vt_exec = strdup(exec);
  }
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
  return vt_exec;
}

/* clock resolution */
uint64_t vt_pform_clockres() {
#if TIMER == TIMER_DCLOCK
  return 1000000000000000LL;
#elif TIMER == TIMER_CYCLE_COUNTER
  return vt_ticks_per_sec;
#elif TIMER == TIMER_CLOCK_GETTIME
  return 1000000000LL;
#elif TIMER == TIMER_GETTIMEOFDAY
  return 1000000LL;
#elif TIMER == TIMER_PAPI_REAL_CYC
  return vt_metric_clckrt();
#elif TIMER == TIMER_PAPI_REAL_USEC
  return 1000000LL;
#endif
}

/* local or global wall-clock time */
uint64_t vt_pform_wtime() {
#if TIMER == TIMER_DCLOCK
  return (uint64_t)(dclock() * 1.0e15);
#elif TIMER == TIMER_CYCLE_COUNTER
  uint64_t clock_value;
  uint32_t low = 0;
  uint32_t high = 0;
  asm volatile ("rdtsc" : "=a" (low), "=d" (high));
  clock_value = ((uint64_t)high << 32) | (uint64_t)low;
  return clock_value;
#elif TIMER == TIMER_CLOCK_GETTIME
  struct timespec tp;
  clock_gettime(CLOCK_REALTIME, &tp);
  return ((tp.tv_sec - vt_time_base) * 1000000000LL) + tp.tv_nsec;
#elif TIMER == TIMER_GETTIMEOFDAY
  struct timeval tp;
  gettimeofday(&tp, 0);
  return ((tp.tv_sec - vt_time_base) * 1000000LL) + tp.tv_usec;
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
  return vt_node_name;
}

/* number of CPUs */
int vt_pform_num_cpus() {
  return 1;
}
