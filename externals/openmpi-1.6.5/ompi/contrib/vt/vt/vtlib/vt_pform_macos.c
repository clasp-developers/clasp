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
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/sysctl.h>

#ifndef TIMER_PAPI_REAL_CYC
#  define TIMER_PAPI_REAL_CYC 10
#endif
#ifndef TIMER_PAPI_REAL_USEC
#  define TIMER_PAPI_REAL_USEC 11
#endif

#if TIMER != TIMER_CYCLE_COUNTER && \
    TIMER != TIMER_GETTIMEOFDAY && \
    TIMER != TIMER_PAPI_REAL_CYC && \
    TIMER != TIMER_PAPI_REAL_USEC
# error Unknown timer specified! Check the timer configuration in 'config.h'.
#endif

#if TIMER == TIMER_GETTIMEOFDAY
# include <time.h>
# include <sys/time.h>
  static uint64_t vt_time_base = 0;
#elif TIMER == TIMER_CYCLE_COUNTER
  static uint64_t vt_ticks_per_sec = 0;
#elif TIMER == TIMER_PAPI_REAL_CYC
  extern uint64_t vt_metric_clckrt(void);
  extern uint64_t vt_metric_real_cyc(void);
#elif TIMER == TIMER_PAPI_REAL_USEC
  extern uint64_t vt_metric_real_usec(void);
  static uint64_t vt_time_base = 0;
#endif

static char* vt_exec = NULL;
static long vt_node_id = 0;
static uint32_t vt_cpu_count = 0;

/* platform specific initialization */
void vt_pform_init()
{
  int    mib[4];
  int    argmax;
  char*  procargs;
  size_t len;

  /* get timer resolution */
#if TIMER == TIMER_GETTIMEOFDAY
  struct timeval tp;
  gettimeofday(&tp, 0);
  vt_time_base = tp.tv_sec - (tp.tv_sec & 0xFFFF);
#elif TIMER == TIMER_CYCLE_COUNTER
# if defined(__powerpc64__) || defined(__powerpc__) || defined(__POWERPC__)
    struct clockinfo clk_info;
    mib[0] = CTL_KERN;
    mib[1] = KERN_CLOCKRATE;
    len = sizeof(clk_info);
    if (sysctl(mib, 2, &clk_info, &len, NULL, 0) == -1)
      vt_error_msg("sysctl[KERN_CLOCKRATE] failed: %s", strerror(errno));
    vt_ticks_per_sec = clk_info.hz * 1000000;
# else
    mib[0] = CTL_HW;
    mib[1] = HW_CPU_FREQ;
    len = sizeof(vt_ticks_per_sec);
    if (sysctl(mib, 2, &vt_ticks_per_sec, &len, NULL, 0) == -1)
      vt_error_msg("sysctl[HW_CPU_FREQ] failed: %s", strerror(errno));
# endif
#elif TIMER == TIMER_PAPI_REAL_USEC
  vt_time_base = vt_metric_real_usec();
#endif

  /* get number of CPUs */
  mib[0] = CTL_HW;
  mib[1] = HW_NCPU;
  len = sizeof(vt_cpu_count);
  if (sysctl(mib, 2, &vt_cpu_count, &len, NULL, 0) == -1)
    vt_error_msg("sysctl[HW_NCPU] failed: %s", strerror(errno));

  /* get maximum arguments of executable */
  mib[0] = CTL_KERN;
  mib[1] = KERN_ARGMAX;
  len = sizeof(argmax);
  if (sysctl(mib, 2, &argmax, &len, NULL, 0) == -1)
    vt_error_msg("sysctl[KERN_ARGMAX] failed: %s", strerror(errno));

  /* get full path of executable */
  mib[0] = CTL_KERN;
  mib[1] = KERN_PROCARGS;
  mib[2] = (int)getpid();
  len = (size_t)argmax;
  procargs = (char*)malloc(argmax);
  if (sysctl(mib, 3, procargs, &len, NULL, 0) == -1)
    vt_error_msg("sysctl[KERN_PROCARGS] failed: %s", strerror(errno));

  /* relative path? */
  if (procargs[0] != '/')
  {
    /* prepend CWD to relative path */
    char cwd[1024];
    if (getcwd( cwd, sizeof(cwd)) == NULL)
      vt_error_msg("getcwd failed: %s", strerror(errno));

    vt_exec = (char*)malloc(strlen(cwd)+strlen(procargs)+1);
    sprintf(vt_exec, "%s/%s", cwd, procargs);
  }
  else
  {
    vt_exec = strdup(procargs); 
  }

  free(procargs);

  /* get unique numeric SMP-node identifier */
  mib[0] = CTL_KERN;
  mib[1] = KERN_HOSTID;
  len = sizeof(vt_node_id);
  if (sysctl(mib, 2, &vt_node_id, &len, NULL, 0) == -1)
    vt_error_msg("sysctl[KERN_HOSTID] failed: %s", strerror(errno));
}

/* directory of global file system  */
char* vt_pform_gdir()
{
  return ".";
}

/* directory of local file system  */
char* vt_pform_ldir()
{
#ifdef DEFAULT_PFORM_LDIR
  return DEFAULT_PFORM_LDIR;
#else
  return "/tmp";
#endif
}

/* full path of executable  */
char* vt_pform_exec()
{
  return vt_exec;
}

/* clock resolution */
uint64_t vt_pform_clockres()
{
#if TIMER == TIMER_GETTIMEOFDAY
  return 1000000LL;
#elif TIMER == TIMER_CYCLE_COUNTER
  return vt_ticks_per_sec;
#elif TIMER == TIMER_PAPI_REAL_CYC
  return vt_metric_clckrt();
#elif TIMER == TIMER_PAPI_REAL_USEC
  return 1000000LL;
#endif
}

/* local or global wall-clock time */
uint64_t vt_pform_wtime()
{
#if TIMER == TIMER_GETTIMEOFDAY
  struct timeval tp;
  gettimeofday(&tp, 0);
  return ((tp.tv_sec - vt_time_base) * 1000000LL) + tp.tv_usec;
#elif TIMER == TIMER_CYCLE_COUNTER
  uint64_t clock_value;

# ifdef __powerpc64__
    /* ... PPC64 */
    asm volatile("mftb %0" : "=r" (clock_value));
# elif defined(__powerpc__) || defined(__POWERPC__)
    /* ... PPC32 */
    {
      uint32_t low = 0;
      uint32_t higha = 0;
      uint32_t highb = 0;
      
      do {
	asm volatile ("mftbu %0" : "=r"(highb));
	asm volatile ("mftb %0" : "=r"(low));
	asm volatile ("mftbu %0" : "=r"(higha));
      } while (highb != higha);
      clock_value = ((uint64_t)higha << 32) | (uint64_t)low;
    }
# else
    /* ... TSC */
    {
      uint32_t low = 0;
      uint32_t high = 0;
       
      asm volatile ("rdtsc" : "=a" (low), "=d" (high));
  
      clock_value = ((uint64_t)high << 32) | (uint64_t)low;
    }
# endif

  return clock_value;
#elif TIMER == TIMER_PAPI_REAL_CYC
  return vt_metric_real_cyc();
#elif TIMER == TIMER_PAPI_REAL_USEC
  return vt_metric_real_usec() - vt_time_base;
#endif
}

/* unique numeric SMP-node identifier */
long vt_pform_node_id()
{
  return vt_node_id;
}

/* unique string SMP-node identifier */
char* vt_pform_node_name()
{
  static char host_name[20];

  gethostname(host_name, 20);

  return host_name;
}

/* number of CPUs */
int vt_pform_num_cpus()
{
  return vt_cpu_count;
}
