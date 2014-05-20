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

#ifndef _VT_PLUGIN_CNTR_H
#define _VT_PLUGIN_CNTR_H

#include "vt_inttypes.h"



/* use this enum to define the synch type of your plugin */
enum vt_plugin_cntr_synch {
  VT_PLUGIN_CNTR_SYNCH = 0,
  VT_PLUGIN_CNTR_ASYNCH_EVENT,
  VT_PLUGIN_CNTR_ASYNCH_POST_MORTEM,
  VT_PLUGIN_CNTR_ASYNCH_CALLBACK,
  VT_PLUGIN_CNTR_SYNCH_TYPE_MAX /* NON-ABI, don't use it!*/
};

/* use this enum to define how often a metric should be measured*/

enum vt_plugin_cntr_per {
  VT_PLUGIN_CNTR_PER_THREAD = 0,
  VT_PLUGIN_CNTR_PER_PROCESS,
  VT_PLUGIN_CNTR_PER_HOST,
  VT_PLUGIN_CNTR_ONCE,
  VT_PLUGIN_CNTR_RUN_PER_MAX /* NON-ABI, don't use it!*/
};

/* use this enum to check your callback function's return value
 * (only use it with VT_PLUGIN_CNTR_ASYNCH_CALLBACK plugins) */
enum vt_plugin_callback_return {
  VT_PLUGIN_CNTR_CALLBACK_OK = 0,
  VT_PLUGIN_CNTR_CALLBACK_BUFFER_FULL = 1,
  VT_PLUGIN_CNTR_CALLBACK_TRACE_OFF_PERMANENT = 2
};

/* Use these definitions to define the counter properties */

/* The counter has an increasing value */
#define VT_PLUGIN_CNTR_ACC       1<<0
/* The counter has an absolute value */
#define VT_PLUGIN_CNTR_ABS       1<<1
/* The counter values belong to the time interval since
 *  the beginning of the measurement*/
#define VT_PLUGIN_CNTR_START     1<<2
/* value is only valid at a point in time but
not necessarily for any interval of time */
#define VT_PLUGIN_CNTR_POINT     1<<3
/* The counter values are related to the time interval since the last counter
 * sample of the same counter, i.e. the immediate past */
#define VT_PLUGIN_CNTR_LAST      1<<4
/* The counter values are valid from now until the next counter sample,
 *  i.e. the future right ahead. */
#define VT_PLUGIN_CNTR_NEXT      1<<5

/* For integer values (which have to have a length of 64 bit) */
#define VT_PLUGIN_CNTR_SIGNED    1<<6
#define VT_PLUGIN_CNTR_UNSIGNED  1<<7
/* For floating point values */
#define VT_PLUGIN_CNTR_FLOAT     1<<8
#define VT_PLUGIN_CNTR_DOUBLE    1<<9

#define VT_PLUGIN_CNTR_VERSION 2;

/* used for add_counter */
typedef struct vt_plugin_cntr_metric_info_struct {
  /* generated id by plugin */
  char * name;
  /* name of the function */
  char * unit;
  /* name of the function */
  uint32_t cntr_property;
} vt_plugin_cntr_metric_info;

/* used for get_results */
typedef struct vt_plugin_cntr_timevalue_struct {
  /* in vampir trace time! */
  uint64_t timestamp;
  /* current value */
  uint64_t value;
} vt_plugin_cntr_timevalue;

typedef struct vt_plugin_cntr_info_struct {
  /* for all */

  /* should be set to VT_PLUGIN_VERSION (needed for back- and forward
   * compatiblity)
   */
  uint32_t vt_plugin_cntr_version;

  /* this is called once per process
   * should return 0 if successful
   */
  int32_t(*init)(void);

  /* this is called once per process */
  vt_plugin_cntr_metric_info * (*get_event_info)(char *);

  /* add counter, returns ID
   * is called PER THREAD
   */
  int32_t (*add_counter)(char * event_name);

  /* enable counter with ID
   * is called PER THREAD
   * can be set to NULL
   */
  int32_t (*enable_counter)(int32_t ID);

  /* disable counter with ID
   * is called PER THREAD
   * can be set to NULL
   */
  int32_t (*disable_counter)(int32_t ID);

  /* register a asynchronous plugin's own threads within the plugin!
   * the threads may ask whether they are registered.
   * (use pthread_self to get the tid)
   */
  int32_t (*is_thread_registered)(void);

  /* runs per host / per thread / ... */
  int32_t run_per;
  /* runs synchronous/asynch. */
  int32_t synch;

  /* for synchronous plugins */
  uint64_t (*get_current_value)(int32_t ID);

  /* for asynchronous plugins */
  void (*set_pform_wtime_function)(uint64_t(*pform_wtime)(void));

  /* asynch post_mortem/event
  * Input 1: ID
  * Input 2: used as output: pointer to list with return values
  * return : length of list with return values
  */
  uint64_t (*get_all_values)(int32_t, vt_plugin_cntr_timevalue **);

  /* asynch_callback */
  /* Input 1: an ID, which should be used as first argument when calling the
   *          callback function
   * Input 2: is a counter id the plugin provided previously
   * Input 3: is the function to be called by the plugin
   *      whenever there is new data, the plugin should call
   *      callback_function(Input1,currentValue)
   * return 0 if successful
   */
  int32_t (*set_callback_function)(void *, int32_t,
      int32_t(*callback_function)(void *, vt_plugin_cntr_timevalue));

  /* called once per process */
  void (*finalize)(void);

  /* some space for future stuff, should be zeroed */
  uint64_t reserved[100];

} vt_plugin_cntr_info;


/**
 * should be implemented by a plugin
 */
vt_plugin_cntr_info get_info(void);

#endif /* _VT_PLUGIN_CNTR_H */
