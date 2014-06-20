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

#ifndef _VT_ENV_H
#define _VT_ENV_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#include "vt_defs.h"

#include <stdio.h>

EXTERN char*  vt_env_apppath(void);
EXTERN char*  vt_env_dyn_shlibs(void);
EXTERN int    vt_env_dyn_outer_loops(void);
EXTERN int    vt_env_dyn_inner_loops(void);
EXTERN int    vt_env_dyn_loop_iters(void);
EXTERN int    vt_env_dyn_ignore_nodbg(void);
EXTERN int    vt_env_dyn_detach(void);
EXTERN char*  vt_env_gnu_nm(void);
EXTERN char*  vt_env_gnu_nmfile(void);
EXTERN char*  vt_env_gdir(void);
EXTERN char*  vt_env_ldir(void);
EXTERN int    vt_env_gdir_check(void);
EXTERN int    vt_env_ldir_check(void);
EXTERN char*  vt_env_fprefix(void);
EXTERN int    vt_env_funique(void);
EXTERN size_t vt_env_bsize(void);
EXTERN size_t vt_env_thread_bsize(void);
EXTERN int    vt_env_pthread_reuse(void);
EXTERN int    vt_env_mode(void);
EXTERN int    vt_env_stat_intv(void);
EXTERN int    vt_env_stat_props(void);
EXTERN int    vt_env_stat_msg_dtls(void);
EXTERN int    vt_env_stat_collop_dtls(void);
EXTERN int    vt_env_snapshots(void);
EXTERN int    vt_env_max_snapshots(void);
EXTERN int    vt_env_verbose(void);
EXTERN int    vt_env_do_unify(void);
EXTERN int    vt_env_do_clean(void);
EXTERN int    vt_env_cpuidtrace(void);
EXTERN int    vt_env_iotrace(void);
EXTERN int    vt_env_iotrace_extended(void);
EXTERN char*  vt_env_iolibpathname(void);
EXTERN int    vt_env_exectrace(void);
EXTERN int    vt_env_memtrace(void);
EXTERN int    vt_env_memtrace_marker(void);
EXTERN int    vt_env_omptrace(void);
EXTERN int    vt_env_mpitrace(void);
EXTERN int    vt_env_mpi_ignore_filter(void);
EXTERN int    vt_env_mpicheck(void);
EXTERN int    vt_env_mpicheck_errexit(void);
EXTERN char*  vt_env_rusage(void);
EXTERN int    vt_env_rusage_intv(void);
EXTERN char*  vt_env_metrics(void);
EXTERN char*  vt_env_metrics_sep(void);
EXTERN char*  vt_env_metrics_spec(void);
EXTERN int    vt_env_sync_flush(void);
EXTERN int    vt_env_sync_flush_skip(void);
EXTERN int    vt_env_sync_flush_level(void);
EXTERN int    vt_env_onoff_check_stack_balance(void);
EXTERN int    vt_env_max_stack_depth(void);
EXTERN int    vt_env_max_flushes(void);
EXTERN int    vt_env_max_threads(void);
EXTERN int    vt_env_compression(void);
EXTERN size_t vt_env_otf_bsize(void);
EXTERN size_t vt_env_compression_bsize(void);
EXTERN int    vt_env_java_native(void);
EXTERN int    vt_env_java_synthetic(void);
EXTERN int    vt_env_java_group_classes(void);
EXTERN char*  vt_env_java_filter_spec(void);
EXTERN char*  vt_env_filter_spec(void);
EXTERN char*  vt_env_groups_spec(void);
EXTERN int    vt_env_etimesync(void);
EXTERN int    vt_env_etimesync_intv(void);
EXTERN void   vt_env_cudatrace(void);
EXTERN size_t vt_env_cudatrace_bsize(void);
EXTERN char*  vt_env_cupti_events(void);
EXTERN int    vt_env_cupti_sampling(void);
EXTERN char*  vt_env_gputrace(void);
EXTERN int    vt_env_gputrace_kernel(void);
EXTERN int    vt_env_gputrace_memusage(void);
EXTERN int    vt_env_gputrace_sync(void);
EXTERN char*  vt_env_iofsl_servers(void);
EXTERN int    vt_env_iofsl_mode(void);
EXTERN int    vt_env_iofsl_async_io(void);

#endif /* _VT_ENV_H */



















