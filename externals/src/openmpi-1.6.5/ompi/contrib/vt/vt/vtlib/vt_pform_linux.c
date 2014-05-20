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
#include <errno.h>
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

#if TIMER != TIMER_CYCLE_COUNTER && \
    TIMER != TIMER_GETTIMEOFDAY && \
    TIMER != TIMER_CLOCK_GETTIME && \
    TIMER != TIMER_PAPI_REAL_CYC && \
    TIMER != TIMER_PAPI_REAL_USEC
# error Unknown timer specified! Check the timer configuration in 'config.h'.
#endif


#if TIMER == TIMER_CYCLE_COUNTER
#include <sys/time.h>
# if defined(__ia64__)
#   include <asm/intrinsics.h>
# endif
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

#if TIMER == TIMER_CYCLE_COUNTER
/* to have the declaration befor the implementation */
uint64_t cylce_counter_frequency(long usleep_time);

uint64_t cylce_counter_frequency(long usleep_time)
{
    uint64_t start1_cylce_counter, start2_cylce_counter;
    uint64_t end1_cylce_counter, end2_cylce_counter;
    uint64_t start_time, end_time;
    uint64_t start_time_cylce_counter, end_time_cylce_counter;
    struct timeval timestamp;

    /* start timestamp */
    start1_cylce_counter = vt_pform_wtime();
    gettimeofday(&timestamp,NULL);
    start2_cylce_counter = vt_pform_wtime();
  
    start_time=timestamp.tv_sec*1000000+timestamp.tv_usec;
    
    usleep( usleep_time );
  
    /* end timestamp */
    end1_cylce_counter = vt_pform_wtime();
    gettimeofday(&timestamp,NULL);
    end2_cylce_counter = vt_pform_wtime();
    
    end_time=timestamp.tv_sec*1000000+timestamp.tv_usec;

    start_time_cylce_counter = (start1_cylce_counter+start2_cylce_counter)/2;
    end_time_cylce_counter   = (  end1_cylce_counter+  end2_cylce_counter)/2;

    /* freq is 1e6 * cylce_counter_time_diff/gettimeofday_time_diff */
    return (uint64_t)
             (1e6*(double)(end_time_cylce_counter-start_time_cylce_counter)/
             (double)(end_time-start_time));
}
#endif /* TIMER == TIMER_CYCLE_COUNTER */

static char* vt_exec = NULL;
static long vt_node_id = 0;
static uint32_t vt_cpu_count = 0;

/* platform specific initialization */
void vt_pform_init()
{
  int  pid = getpid();
  char exec_proc[VT_PATH_MAX];
  char exec[VT_PATH_MAX];
  int  exec_len;
  FILE *cpuinfofp;
  char line[1024];
  int  hostid_retries;

#if TIMER == TIMER_CYCLE_COUNTER
    int num_measurements=0, loop;
    int done=0;
    uint64_t value, test_value, diff;
#endif 

#if TIMER == TIMER_CLOCK_GETTIME
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

  if ((cpuinfofp = fopen (VT_PROCDIR "cpuinfo", "r")) == NULL) 
    vt_error_msg("Cannot open file %s: %s", VT_PROCDIR "cpuinfo",
                  strerror(errno));
  
  while (fgets(line, sizeof (line), cpuinfofp))
  {
    if (!strncmp("processor", line, 9))
      vt_cpu_count++;
#if TIMER == TIMER_CYCLE_COUNTER
    {
# if defined(__ia64__)
      if (!strncmp("itc MHz", line, 7))
# else
      if (!strncmp("cpu MHz", line, 7))
# endif
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
#endif
  }
  
  fclose(cpuinfofp);

/* try to something better on ia32 by doing timing measurements 
 * on the TSC 
 */
#if TIMER == TIMER_CYCLE_COUNTER
    do /* ~100 milli sec sleeps until we have a stable value */
    {
        value = cylce_counter_frequency(100000);
        /* printf("cylce_counter freq initial at: %llu\n", value); */
        /* at max two test against this value to see if it is stable;
         * or to see if we are able to read the get the same value 
         * (hopefully stable means good) again 
         */
        for( loop=0; loop<2; loop++)
        {
            test_value = cylce_counter_frequency(100000);
            /* printf("cylce_counter freq (t %d) at: %llu\n", loop, test_value); */
            diff = ( test_value>value ) ? test_value-value :
                                          value-test_value;
            /* stable value is here defined as not more than 0.01% difference */
            if( ((double) diff) < ((double) 0.0001 * value) )
            {
                /* printf("updating cylce_counter freq to: %llu\n", value); */
                vt_ticks_per_sec = value;
                done=1;
                break;
            }
        }
        num_measurements++;
    } while( done==0 && num_measurements<3 );

    /*
    if( done==0 )
    {
        printf("unable to get a stable cycle counter frequency");
    }
    */

#endif /* TIMER == TIMER_CYCLE_COUNTER */

  /* get full path of executable */
  snprintf(exec_proc, sizeof (exec_proc), VT_PROCDIR"%d/exe", pid);
  exec_len = readlink(exec_proc, exec, sizeof (exec)-1);
  if(exec_len > 0 )
  {
    exec[exec_len] = '\0';

    /* if the result of readlink isn't accessable it could be that we are on an
       unionfs. In this case crop the first directory (/cow) from the pathname
       of the executable. */
    if(access(exec, F_OK) != 0)
    {
      char* root = strchr(exec+1, '/');
      if(root)
        vt_exec = strdup(root);
    }
    else
    {
      vt_exec = strdup(exec);
    }
  }

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
#if TIMER == TIMER_CYCLE_COUNTER
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
uint64_t vt_pform_wtime()
{
#if TIMER == TIMER_CYCLE_COUNTER
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
# elif defined(__ia64__)
    /* ... IA64 */
    clock_value = __getReg(_IA64_REG_AR_ITC);
# elif defined(__alpha__)
    /* ... Alpha */
    asm volatile ("rpcc %0" : "=r"(clock_value));
# elif defined(__sparc__)
    /* ... Sparc */
    asm ("rd %%tick, %0" : "=r"(clock_value));
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
