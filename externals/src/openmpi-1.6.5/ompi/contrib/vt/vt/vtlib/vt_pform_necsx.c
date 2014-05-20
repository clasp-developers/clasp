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
#include <stdlib.h>
#include <unistd.h>
#include <sys/rsg.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#if TIMER != TIMER_SYSSX_HGTIME
# error Unknown timer specified! Check the timer configuration in 'config.h'.
#endif

#include <sys/syssx.h>

static uint64_t vt_time_base = 0;
static long vt_node_id = 0;

/* platform specific initialization */
void vt_pform_init() {
  unsigned long long val;
  int hostid_retries;
  syssx(HGTIME, &val);
  vt_time_base = val - (val % 10000000000);

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
  return NULL;
}

/* clock resolution */
uint64_t vt_pform_clockres() {
  return 1000000LL;
}

/* local or global wall-clock time */
uint64_t vt_pform_wtime() {
  unsigned long long val;
  syssx(HGTIME, &val);
  return (uint64_t)val - vt_time_base;
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
  rsg_info_t data;
  int id;

  id=open("/dev/rsg/own", O_RDONLY);
  ioctl(id, RSG_INFO, &data);
  close(id);
  return data.cprb.maxi_cpu;
}
