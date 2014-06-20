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

#if defined(VT_PLUGIN_CNTR)

#ifndef _VT_PLUGIN_CNTR_INT_H
#define _VT_PLUGIN_CNTR_INT_H

#include "vt_plugin_cntr.h"
#include "vt_thrd.h"

#define VT_PLUGIN_PROCESS_GROUP_ALL_STRING "All Processes"
#define VT_PLUGIN_PROCESS_GROUP_HOST_STRING "Processes on Host "
#define VT_PLUGIN_PROCESS_GROUP_PROCESS_STRING "Threads on Process "

#define VT_PLUGIN_CNTR_WRITING_POST_MORTEM(tid, flag) \
  { \
    VTThrd * thrd; \
    if (tid == VT_CURRENT_THREAD) \
    { \
      VT_CHECK_THREAD; \
      thrd = VTTHRD_MY_VTTHRD; \
    } \
    else \
    { \
      thrd = VTThrdv[tid]; \
    } \
    flag = VTTHRD_PLUGIN_CNTR_WRITING_POST_MORTEM(thrd); \
  }

enum cntr_group{
	VT_PLUGIN_PROCESS_GROUP_ALL = 0,
	VT_PLUGIN_PROCESS_GROUP_HOST,
	VT_PLUGIN_PROCESS_GROUP_PROCESS,
	VT_PLUGIN_PROCESS_GROUP_MAX
};

EXTERN uint8_t vt_plugin_cntr_used;

/**
 * VampirTrace internal functions, which may change in later releases
 */

/**
 * get the number of synch metrics for the current thread
 */
uint32_t vt_plugin_cntr_get_num_synch_metrics(VTThrd * thrd);
/**
 * get the current value of the synch counter nr for the current thread
 */
uint64_t vt_plugin_cntr_get_synch_value(VTThrd * thrd, int nr, uint32_t * cid,
    uint64_t * value);

/**
 * write all callback data for threadID, which occured between the
 * last call of this function and time
 */
void vt_plugin_cntr_write_callback_data(uint64_t * time, uint32_t tid);

/**
 * stores all collected asynch event plugins data
 */
void vt_plugin_cntr_write_asynch_event_data(uint64_t * time, uint32_t tid);

/**
 * This should read the environment, and map the libraries
 */
void vt_plugin_cntr_init(void);
/**
 * This should set the correct counters for the current vampir trace thread
 */
void vt_plugin_cntr_thread_init(VTThrd * thrd, uint32_t tid);
/**
 * enable counters before tracing
 */
void vt_plugin_cntr_thread_enable_counters(VTThrd * thrd);
/**
 * disable counters after tracing
 */
void vt_plugin_cntr_thread_disable_counters(VTThrd * thrd);

/**
 * This should free all per thread ressources
 */
void vt_plugin_cntr_thread_exit(VTThrd * thrd);

/**
 * This should free all general ressources
 * nr_threads is the number of threads the process generated
 */
void vt_plugin_cntr_finalize(uint32_t nr_threads);
/**
 * This should be used to check whether the current thread is
 * a monitor thread of a callback function.
 * Monitor threads should not be traced.
 */
int vt_plugin_cntr_is_registered_monitor_thread(void);

/**
 * writes all post_mortem events to the trace files
 * thrd is the thread for which the values shall be collected
 * This should be called when closing all threads
 */
void vt_plugin_cntr_write_post_mortem(VTThrd * thrd);

/* Set Group IDs */
void vt_plugin_cntr_set_all_group(uint32_t group_id);
void vt_plugin_cntr_set_host_group(uint32_t group_id);
void vt_plugin_cntr_set_process_group(uint32_t group_id);

#endif /* _VT_PLUGIN_CNTR_INT_H */

#endif /* VT_PLUGIN_CNTR */
