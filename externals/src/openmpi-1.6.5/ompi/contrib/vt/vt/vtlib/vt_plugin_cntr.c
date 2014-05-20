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

#include "vt_plugin_cntr.h"
#include "vt_plugin_cntr_int.h"
#include "vt_pform.h"
#include "vt_thrd.h"
#include "vt_trc.h"

#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* This should not be exceeded */
#define VT_PLUGIN_COUNTERS_PER_THREAD 256

/* per plugin library information */
struct vt_plugin {
  /* info from get_info() */
  vt_plugin_cntr_info info;
  /* handle should be closed when finalize */
  void * dlfcn_handle;
  /* counter group for vt*/
  uint32_t counter_group;
  /* selected event_names */
  int num_selected_events;
  /* selected event_names */
  char * name;
  /* selected event_names */
  char ** selected_events;
  /* vt counter ids */
  uint32_t * vt_counter_ids;
  /* the asynch key value ids assigned by vt */
  uint32_t * vt_asynch_keys;
};

/* a number of plugin libraries, [synch_type][library_nr] */
static struct vt_plugin** vt_plugin_handles = NULL;

/* number of used plugins per synch type*/
static uint32_t * nr_plugins = NULL;

/* the maximal number of events a callback may produce */
static uint32_t max_values_callback;

#define INVALID_GROUP_NUMBER 0xFFFFFFFF

static uint32_t all_group = INVALID_GROUP_NUMBER;
static uint32_t host_group = INVALID_GROUP_NUMBER;
static uint32_t thread_group = INVALID_GROUP_NUMBER;

/* whether plugins are used or not*/
uint8_t vt_plugin_cntr_used = 0;

/* per thread values */
/* short cut for measuring */
struct vt_plugin_single_counter {
  /* the id, which was produced by the plugin */
  int32_t from_plugin_id;
  /* the id assigned by vt */
  uint32_t vt_counter_id;
  /* process group or thread id to write the results to */
  uint32_t tid;
  /* the asynch key value id assigned by vt */
  uint32_t vt_asynch_key;
  /* functions for en- and disabling */
  int32_t (*enable_counter)(int32_t);
  int32_t (*disable_counter)(int32_t);
  /* short cuts for getting values */
  uint64_t (*getValue)(int32_t);
  uint64_t (*getAllValues)(int32_t, vt_plugin_cntr_timevalue **);

  /* current position in callback_values to write, int per thread */
  uint32_t current_callback_write_position;
  /* callback stuff */
  vt_plugin_cntr_timevalue * callback_values;
  void * callback_mutex;
};

/* used for per thread variables in VTThrd */
struct vt_plugin_cntr_defines {
  /* per synch type */
  uint32_t * size_of_counters;
  /* per synch type, size_of ...*/
  struct vt_plugin_single_counter ** counters;
};

/* called when activating events */
static void maybe_register_new_thread(VTThrd * thrd, uint32_t tid);
/* called when activating events */
static void add_events(const struct vt_plugin * current_plugin, VTThrd * thrd);

static uint32_t post_mortem_asynch_key(void);

/* called by watchdog thread in a plugin */
int callback_function(void * ID, vt_plugin_cntr_timevalue tv);

void vt_plugin_cntr_init() {
  char ** plugins;
  int nr_selected_plugins = 0;
  char * plugin_read_start;
  int read_plugin;
  char * current_plugin;

  char * env_vt_plugin_metrics;
  char current_plugin_metric[255];
  char * next_plugin_metric;

  char * env_vt_callback_buffer;

  char buffer[512];

  char * dl_lib_error;

  void * handle;
  vt_plugin_cntr_info info;

  /* used union to get rid of compiler warning */
  union {
    void * vp;
    vt_plugin_cntr_info (* function)(void);

  } get_info;

  int index;
  int i;
  int found = 0;

  struct vt_plugin * current;

  /* set some internal variables to zero */
  vt_plugin_handles = calloc(VT_PLUGIN_CNTR_SYNCH_TYPE_MAX,
      sizeof(struct vt_plugin *));
  nr_plugins = calloc(VT_PLUGIN_CNTR_SYNCH_TYPE_MAX, sizeof(uint32_t));

  /* check whether plugins are activated */
  env_vt_plugin_metrics = getenv("VT_PLUGIN_CNTR_METRICS");
  if (env_vt_plugin_metrics == NULL)
    return;

  env_vt_callback_buffer = getenv("VT_PLUGIN_CNTR_CALLBACK_BUFFER");
  /* default: 1 M elements (16 MiB memory) per thread and callback counter */
  if (env_vt_callback_buffer == NULL)
    max_values_callback = 1024 * 1024;
  else
    max_values_callback = atoi(env_vt_callback_buffer);

  /* extract the plugin names */
  plugin_read_start = env_vt_plugin_metrics;
  read_plugin = 1;
  current_plugin = plugin_read_start;
  plugins = NULL;
  /* go through the plugin env. variable */
  for (; *current_plugin != '\0'; current_plugin++) {
    if (read_plugin) {
      if (*current_plugin == '_') {
        /* do not use the same plugin twice! */
        memcpy(buffer, plugin_read_start,
            ((current_plugin - plugin_read_start)) * sizeof(char));
        buffer[(current_plugin - plugin_read_start)] = '\0';
        found = 0;
        for (i = 0; i < nr_selected_plugins; i++) {
          if (strcmp(buffer, plugins[i]) == 0)
            found = 1;
        }
        if (found) {
          read_plugin = 0;
          continue;
        } else {
          nr_selected_plugins++;
          /* allocate the plugin name buffer */
          plugins = realloc(plugins, nr_selected_plugins * sizeof(char*));
          plugins[nr_selected_plugins - 1] = malloc(
              (current_plugin - plugin_read_start + 1) * sizeof(char));
          /* copy the content to the buffer */
          memcpy(plugins[nr_selected_plugins - 1], plugin_read_start,
              ((current_plugin - plugin_read_start)) * sizeof(char));
          /* finish with null */
          plugins[nr_selected_plugins - 1]
                 [(current_plugin - plugin_read_start)] = '\0';
          read_plugin = 0;
        }
      }
    } else {
      /* a new plugin/counter starts after the ':' */
      if (*current_plugin == ':') {
        read_plugin = 1;
        plugin_read_start = current_plugin + 1;
      }
    }
  }
  /*go through all plugins:*/
  for (i = 0; i < nr_selected_plugins; i++) {
    uint32_t group = 0;
    current_plugin = plugins[i];
    vt_cntl_msg(2, "Loading plugin counter library: lib%s.so", current_plugin);
    /* next one is stored in next_plugin,
     / * current is stored in current_plugin_buffer */
    /* load it from LD_LIBRARY_PATH*/
    sprintf(buffer, "lib%s.so", current_plugin);

    /* now dlopen it */
    handle = dlopen(buffer, RTLD_NOW);

    /* if it is not valid */
    if ((dl_lib_error = dlerror()) != NULL) {
      vt_error_msg("Error loading plugin: %s\n", dl_lib_error);
      /* try loading next */
      continue;
    }

    /* now get the info */
    get_info.vp = dlsym(handle, "get_info");
    if ((dl_lib_error = dlerror()) != NULL) {
      vt_error_msg("Error getting info from plugin: %s\n", dl_lib_error);
      dlclose(handle);
      /* try loading next */
      continue;
    }

    /* now store it */

    /* get the info */
    info = get_info.function();

    /* check the run per type */

    if (info.run_per == VT_PLUGIN_CNTR_PER_PROCESS) {
      if (thread_group == INVALID_GROUP_NUMBER){
        vt_cntl_msg(3, "No process group defined, using master thread for %s",
            current_plugin);
      }
      else{
# if (defined(VT_MT) || defined(VT_HYB))
        /* only called per process */
        group = vt_get_curid();
        thread_group = group;
# else
        /* not multithreaded -> keep information on local process */
#endif
      }
    }

    if (info.run_per == VT_PLUGIN_CNTR_PER_HOST) {
      if (!vt_my_trace_is_master)
        continue;
      else if (host_group == INVALID_GROUP_NUMBER){
        host_group = vt_node_pgid;
        vt_def_procgrp_attributes(VT_MY_THREAD ,vt_node_pgid,
            VT_PROCGRP_HASCOUNTERS);
      }
      group = host_group;
    }

    if (info.run_per == VT_PLUGIN_CNTR_ONCE) {
      if (vt_my_trace != 0)
        continue;
      else if (all_group == INVALID_GROUP_NUMBER){
        all_group = vt_all_pgid;
        vt_def_procgrp_attributes(VT_MY_THREAD ,vt_all_pgid,
            VT_PROCGRP_HASCOUNTERS);
      }
      group = all_group;
    }

    if (info.init == NULL) {
      vt_error_msg(
          "Init not implemented in plugin %s\n",
          current_plugin);
      /* try loading next */
      continue;
    }

    if (info.add_counter == NULL) {
      vt_error_msg(
          "Add counter not implemented in plugin %s\n",
          current_plugin);
      /* try loading next */
      continue;
    }

    if (info.get_event_info == NULL) {
      vt_error_msg("Get event info not implemented in plugin %s\n",
          current_plugin);
      /* try loading next */
      continue;
    }

    /* check the type of plugin */
    switch (info.synch) {
    case VT_PLUGIN_CNTR_SYNCH:
      nr_plugins[VT_PLUGIN_CNTR_SYNCH]++;
      vt_plugin_handles[VT_PLUGIN_CNTR_SYNCH] = realloc(
          vt_plugin_handles[VT_PLUGIN_CNTR_SYNCH],
          nr_plugins[VT_PLUGIN_CNTR_SYNCH] * sizeof(struct vt_plugin));
      current
          = &vt_plugin_handles[VT_PLUGIN_CNTR_SYNCH]
                              [nr_plugins[VT_PLUGIN_CNTR_SYNCH] - 1];
      if (info.get_current_value == NULL) {
        nr_plugins[VT_PLUGIN_CNTR_SYNCH]--;
        vt_error_msg("Get current results not implemented in plugin %s\n",
            current_plugin);
        /* try loading next */
        continue;
      }
      break;
    case VT_PLUGIN_CNTR_ASYNCH_CALLBACK:
      nr_plugins[VT_PLUGIN_CNTR_ASYNCH_CALLBACK]++;
      vt_plugin_handles[VT_PLUGIN_CNTR_ASYNCH_CALLBACK]
          = realloc(
              vt_plugin_handles[VT_PLUGIN_CNTR_ASYNCH_CALLBACK],
              nr_plugins[VT_PLUGIN_CNTR_ASYNCH_CALLBACK]
                  * sizeof(struct vt_plugin));
      current
          = &vt_plugin_handles[VT_PLUGIN_CNTR_ASYNCH_CALLBACK]
                              [nr_plugins[VT_PLUGIN_CNTR_ASYNCH_CALLBACK] - 1];
      if (info.set_callback_function == NULL) {
        nr_plugins[VT_PLUGIN_CNTR_ASYNCH_CALLBACK]--;
        vt_error_msg("set callback not implemented in plugin %s\n",
            current_plugin);
        /* try loading next */
        continue;
      }
      if (info.set_pform_wtime_function == NULL) {
        nr_plugins[VT_PLUGIN_CNTR_ASYNCH_CALLBACK]--;
        vt_error_msg(
            "set wtime not implemented in plugin %s\n",
            current_plugin);
        /* try loading next */
        continue;
      }
      break;
    case VT_PLUGIN_CNTR_ASYNCH_EVENT:
      nr_plugins[VT_PLUGIN_CNTR_ASYNCH_EVENT]++;
      vt_plugin_handles[VT_PLUGIN_CNTR_ASYNCH_EVENT] = realloc(
          vt_plugin_handles[VT_PLUGIN_CNTR_ASYNCH_EVENT],
          nr_plugins[VT_PLUGIN_CNTR_ASYNCH_EVENT] * sizeof(struct vt_plugin));
      current
          = &vt_plugin_handles[VT_PLUGIN_CNTR_ASYNCH_EVENT]
                               [nr_plugins[VT_PLUGIN_CNTR_ASYNCH_EVENT] - 1];
      if (info.get_all_values == NULL) {
        nr_plugins[VT_PLUGIN_CNTR_ASYNCH_EVENT]--;
        vt_error_msg("get all values not implemented in plugin %s\n",
            current_plugin);
        /* try loading next */
        continue;
      }
      if (info.set_pform_wtime_function == NULL) {
        nr_plugins[VT_PLUGIN_CNTR_ASYNCH_EVENT]--;
        vt_error_msg(
            "set wtime not implemented in plugin %s\n",
            current_plugin);
        /* try loading next */
        continue;
      }
      break;
    case VT_PLUGIN_CNTR_ASYNCH_POST_MORTEM:
      nr_plugins[VT_PLUGIN_CNTR_ASYNCH_POST_MORTEM]++;
      vt_plugin_handles[VT_PLUGIN_CNTR_ASYNCH_POST_MORTEM] = realloc(
          vt_plugin_handles[VT_PLUGIN_CNTR_ASYNCH_POST_MORTEM],
          nr_plugins[VT_PLUGIN_CNTR_ASYNCH_POST_MORTEM]
              * sizeof(struct vt_plugin));
      current
          = &vt_plugin_handles[VT_PLUGIN_CNTR_ASYNCH_POST_MORTEM]
                           [nr_plugins[VT_PLUGIN_CNTR_ASYNCH_POST_MORTEM] - 1];
      if (info.get_all_values == NULL) {
        vt_error_msg("get all values not implemented in plugin %s\n",
            current_plugin);
        nr_plugins[VT_PLUGIN_CNTR_ASYNCH_POST_MORTEM]--;
        /* try loading next */
        continue;
      }
      if (info.set_pform_wtime_function == NULL) {
        vt_error_msg(
            "set wtime not implemented in plugin %s\n",
            current_plugin);
        nr_plugins[VT_PLUGIN_CNTR_ASYNCH_POST_MORTEM]--;
        /* try loading next */
        continue;
      }
      break;
    default:
      vt_error_msg(
          "Error getting synch type from plugin (invalid synch type)\n");
      continue;
    }

    /* clear out current plugin */
    memset(current, 0, sizeof(struct vt_plugin));

    /* add handle (should be closed in the end) */
    current->dlfcn_handle = handle;

    /* store the info object of the plugin */
    current->info = info;

    /* store the name of the plugin */
    current->name = current_plugin;

    /* give plugin the wtime function to make it possible to convert times */
    if (current->info.set_pform_wtime_function != NULL) {
      current->info.set_pform_wtime_function(vt_pform_wtime);
    }
    vt_cntl_msg(3, "Initializing plugin counter library: lib%s.so",
        current_plugin);
    /* initialize plugin */
    if (current->info.init()) {
      vt_error_msg("Error initializing plugin %s, init returned != 0\n",
          current_plugin);
      continue;
    }
    /* define a counter group for every plugin*/
    current->counter_group = vt_def_counter_group(VT_MY_THREAD, current_plugin);

    /* now search for all available events on that plugin */
    next_plugin_metric = env_vt_plugin_metrics;
    while (next_plugin_metric[0] != 0) {
      /* shall contain current index in environment VT_PLUGIN_METRICS */
      index = 0;

      /* copy metric to current_plugin_metric char by char */
      while ((next_plugin_metric[index] != ':') && (next_plugin_metric[index]
          != '\0')) {
        current_plugin_metric[index] = next_plugin_metric[index];
        index++;
      }
      current_plugin_metric[index] = 0;
      if (next_plugin_metric[index] == ':')
        next_plugin_metric = &next_plugin_metric[index + 1];
      else
        next_plugin_metric = &next_plugin_metric[index];
      /* If the plugin metric belongs to the current plugin */
      if (strstr(current_plugin_metric, current_plugin)
          == current_plugin_metric) {
        /* some meta data*/
        char * unit = NULL;
        uint32_t otf_prop = 0;

        /* This will be needed to iterate over the infos */
        vt_plugin_cntr_metric_info * current_event_info;

        /* check the event name from plugin */
        /* it could contain wildcards and other stuff */
        vt_plugin_cntr_metric_info * event_infos = info.get_event_info(
            &current_plugin_metric[strlen(current_plugin) + 1]);

        vt_cntl_msg(3, "Adding metric %s for plugin counter library: lib%s.so",
            current_plugin_metric, current_plugin);

        /* check the event name from plugin */
        /* it could contain wildcards and other stuff */
        if (event_infos == NULL) {
          vt_error_msg(
              "Error initializing plugin metric %s, no info returned\n",
              current_plugin_metric);
          continue;
        }
        /* now for all events which are present in the struct */
        current_event_info = event_infos;
        for (current_event_info = event_infos;
            current_event_info->name != NULL;
            current_event_info++) {

          vt_cntl_msg(3,
              "Retrieved metric %s for plugin counter library: lib%s.so."
                " Initializing data structures", current_event_info->name,
              current_plugin);

          /* event is part of this plugin */
          current->num_selected_events++;

          /* allocate space for events */
          current->selected_events = realloc(current->selected_events,
              current->num_selected_events * sizeof(char*));

          /*the metric is everything after "plugin_"*/
          current->selected_events[current->num_selected_events - 1]
              = current_event_info->name;

          current->vt_counter_ids = realloc(current->vt_counter_ids,
              current->num_selected_events * sizeof(uint32_t));

          current->vt_asynch_keys = realloc(current->vt_asynch_keys,
              current->num_selected_events * sizeof(uint32_t));

          /* if a unit is provided, use it */
          unit = current_event_info->unit == NULL ? "#"
              : current_event_info->unit;

          /* if otf properties are provided, use them */
          otf_prop = current_event_info->cntr_property;
          /* define new counter */
          current->vt_counter_ids[current->num_selected_events - 1]
              = vt_def_counter(VT_MY_THREAD,
                  current->selected_events[current->num_selected_events - 1],
                  unit, otf_prop, current->counter_group, group);

          switch (current->info.synch) {
          case VT_PLUGIN_CNTR_SYNCH:
          	/* no asynch_keys at all */
        	  break;
          case VT_PLUGIN_CNTR_ASYNCH_POST_MORTEM:
        	  /* One asynch_key for all */
              current->vt_asynch_keys[current->num_selected_events - 1] = post_mortem_asynch_key();
        	  break;
          default:
            {
              char buffer[512];
              sprintf(buffer, "%s_%s", current_plugin, current_event_info->name);
              current->vt_asynch_keys[current->num_selected_events - 1]
                 = vt_def_async_source(VT_MY_THREAD, buffer);
            }
            break;
          }
          /* enable plugin counters */
          vt_plugin_cntr_used = 1;
        } /* end of: for all metrics related to the metric string */
        if (event_infos != NULL)
          free(event_infos);
      } /* end of if metric belongs to this plugin */
    } /* end of: for all plugin metrics */
  } /* end of: for all plugins */

  /* free temporary variables */
  if (plugins != NULL)
    free(plugins);

}

/**
 * this should be done for every thread
 * vt_plugin.c decides which plugin to enable
 */
void vt_plugin_cntr_thread_init(VTThrd * thrd, uint32_t tid) {
  uint32_t i = 0;
  uint32_t j = 0;

  /* for all plugin types*/
  for (i = 0; i < VT_PLUGIN_CNTR_SYNCH_TYPE_MAX; i++) {
    /* all plugins by this type */
    for (j = 0; j < nr_plugins[i]; j++) {
      /* first create groups or join them */

      /* then enable the counter if this thread has to */
      if (vt_plugin_handles[i][j].info.run_per == VT_PLUGIN_CNTR_ONCE) {
        if ((vt_my_trace != 0) || (thrd != VTThrdv[0])){
          continue;
        }
      }
      if (vt_plugin_handles[i][j].info.run_per == VT_PLUGIN_CNTR_PER_HOST)
        if ((!vt_my_trace_is_master) || (thrd != VTThrdv[0]))
          continue;
      if (vt_plugin_handles[i][j].info.run_per == VT_PLUGIN_CNTR_PER_PROCESS)
        /* for all threads != 0 dont trace*/
        if (thrd != VTThrdv[0])
          continue;
      vt_cntl_msg(3,
          "Process %i Thread %s%s (%i) adds own plugin metrics for plugin %s:",
          vt_my_ptrace, thrd->name, thrd->name_suffix, tid,
          vt_plugin_handles[i][j].name);
      maybe_register_new_thread(thrd, tid);

      /* now, that the thread is registered, register the counters */
      add_events(&(vt_plugin_handles[i][j]), thrd);
    }
  }

}

/**
 * enable the counters, that were added before
 * this should be done for every thread. vt_plugin.c decides
 * which plugins/counters to enable
 */
void vt_plugin_cntr_thread_enable_counters(VTThrd * thrd) {
  uint32_t i, j;
  struct vt_plugin_cntr_defines * plugin_cntr_defines;

  vt_cntl_msg(3, "Process %i Thread %s-%s enables own plugin metrics",
      vt_my_ptrace, thrd->name, thrd->name_suffix);

  /* check whether thread is ok */
  if (thrd == NULL)
    return;
  if (thrd->plugin_cntr_defines == NULL)
    return;
  plugin_cntr_defines
      = (struct vt_plugin_cntr_defines *) thrd->plugin_cntr_defines;
  /* for all plugin types*/
  for (i = 0; i < VT_PLUGIN_CNTR_SYNCH_TYPE_MAX; i++) {
    /* all plugins by this type */
    for (j = 0; j < plugin_cntr_defines->size_of_counters[i]; j++) {
      struct vt_plugin_single_counter vts
          = plugin_cntr_defines->counters[i][j];
      if (vts.enable_counter != NULL) {
        vts.enable_counter(vts.from_plugin_id);
      }
    }
  }
}

/**
 * disable the counters, that were added before
 * this should be done for every thread. vt_plugin.c decides
 * which plugins/counters to disabled
 */
void vt_plugin_cntr_thread_disable_counters(VTThrd * thrd) {
  uint32_t i, j;

  struct vt_plugin_cntr_defines * plugin_cntr_defines;

  vt_cntl_msg(3, "Process %i Thread %s%s disables own plugin metrics",
      vt_my_ptrace, thrd->name, thrd->name_suffix);

  /* check whether thread is ok */
  if (thrd == NULL)
    return;
  if (thrd->plugin_cntr_defines == NULL)
    return;

  plugin_cntr_defines
      = (struct vt_plugin_cntr_defines *) thrd->plugin_cntr_defines;

  /* for all plugin types*/
  for (i = 0; i < VT_PLUGIN_CNTR_SYNCH_TYPE_MAX; i++) {
    /* all plugins by this type */
    for (j = 0; j < plugin_cntr_defines->size_of_counters[i]; j++) {
      struct vt_plugin_single_counter vts
          = plugin_cntr_defines->counters[i][j];
      if (vts.disable_counter != NULL) {
        vts.disable_counter(vts.from_plugin_id);
      }
    }
  }

}
/**
 * This should be called after the last thread exited.
 * It should free all ressources used by vt_plugin
 */
void vt_plugin_cntr_finalize(uint32_t tnum) {
  uint32_t i, j;
  int k;

  vt_cntl_msg(3, "Process %i exits plugins", vt_my_ptrace);


# if (defined(VT_MT) || defined(VT_HYB))
  if ( thread_group != INVALID_GROUP_NUMBER )
  /* write thread process group definition */
  {
    uint32_t* grpv;
    char tmp_char[128];

    /* get member array */

    grpv = (uint32_t*)malloc(tnum * sizeof(uint32_t));
    if ( grpv == NULL )
      vt_error();

    for (i = 0; i < tnum; i++)
      grpv[i] = VT_PROCESS_ID(vt_my_trace, i);

    /* prepend thread process group identifier to name */
    snprintf(tmp_char, sizeof(tmp_char) - 1,
             "Threads of Process %d",vt_my_trace);
    /* write thread process group definition */
    vt_def_procgrp(VT_MASTER_THREAD, tmp_char, 0, tnum, grpv, thread_group);

    free(grpv);
  }
#endif

  /* free all ressources */
  for (i = 0; i < VT_PLUGIN_CNTR_SYNCH_TYPE_MAX; i++) {

    /* data per plugin */
    for (j = 0; j < nr_plugins[i]; j++) {
      vt_cntl_msg(3, "Process %i finalizes %s", vt_my_ptrace,
          vt_plugin_handles[i][j].name);
      vt_plugin_handles[i][j].info.finalize();
      vt_cntl_msg(3, "Process %i finalize %s done", vt_my_ptrace,
          vt_plugin_handles[i][j].name);
      if (vt_plugin_handles[i][j].vt_counter_ids != NULL)
        free(vt_plugin_handles[i][j].vt_counter_ids);
      if (vt_plugin_handles[i][j].vt_asynch_keys != NULL)
        free(vt_plugin_handles[i][j].vt_asynch_keys);
      if (vt_plugin_handles[i][j].selected_events != NULL) {
        for (k = 0; k < vt_plugin_handles[i][j].num_selected_events; k++)
          if (vt_plugin_handles[i][j].selected_events[k])
            free(vt_plugin_handles[i][j].selected_events[k]);
        free(vt_plugin_handles[i][j].selected_events);
      }
      if (vt_plugin_handles[i][j].name != NULL)
        free(vt_plugin_handles[i][j].name);
      if (vt_plugin_handles[i][j].dlfcn_handle != NULL)
        dlclose(vt_plugin_handles[i][j].dlfcn_handle);
    }
    free(vt_plugin_handles[i]);
  }
  free(vt_plugin_handles);
  if (nr_plugins)
    free(nr_plugins);

  vt_cntl_msg(3, "Process %i exits plugins done", vt_my_ptrace);
}

static uint32_t post_mortem_asynch_key(void) {
  static uint32_t post_mortem_asynch_key = (uint32_t)-1;
  if (post_mortem_asynch_key == (uint32_t)-1) {
    post_mortem_asynch_key = vt_def_async_source(VT_MY_THREAD, "plugin_post_mortem_all");
  }
  return post_mortem_asynch_key;
}

static void maybe_register_new_thread(VTThrd * thrd, uint32_t tid) {
  struct vt_plugin_cntr_defines * plugin_cntr_defines;
  vt_libassert(thrd!=NULL);
  /* "register" a thread */
  if (thrd->plugin_cntr_defines == NULL) {
    thrd->plugin_cntr_defines
        = calloc(1, sizeof(struct vt_plugin_cntr_defines));
    vt_libassert(thrd->plugin_cntr_defines!=NULL);
    plugin_cntr_defines
        = (struct vt_plugin_cntr_defines *) thrd->plugin_cntr_defines;
    plugin_cntr_defines->counters = calloc(VT_PLUGIN_CNTR_SYNCH_TYPE_MAX,
        sizeof(struct vt_plugin_single_counter *));
    /* number of counters of type */
    plugin_cntr_defines->size_of_counters = calloc(
        VT_PLUGIN_CNTR_SYNCH_TYPE_MAX, sizeof(uint32_t));
    /* single elements in vectors */
  }
}

static void add_events(const struct vt_plugin * current_plugin, VTThrd * thrd) {
  int j;
  struct vt_plugin_single_counter * current;
  uint32_t * current_size;
  struct vt_plugin_cntr_defines * plugin_cntr_defines =
      (struct vt_plugin_cntr_defines *) thrd->plugin_cntr_defines;
  /* get the current counters for this thread and synch type*/
  current = plugin_cntr_defines->counters[current_plugin->info.synch];
  if (current == NULL) {
    plugin_cntr_defines->counters[current_plugin->info.synch] =
	 calloc(VT_PLUGIN_COUNTERS_PER_THREAD, sizeof(struct vt_plugin_single_counter));
    current = plugin_cntr_defines->counters[current_plugin->info.synch];
  }
  /* get the number of counters for this thread and synch type*/
  current_size
      = &(plugin_cntr_defines->size_of_counters[current_plugin->info.synch]);

  vt_cntl_msg(3, "Process %i Thread %s-%s adds own plugin metrics",
      vt_my_ptrace, thrd->name, thrd->name_suffix);

  for (j = 0; j < current_plugin->num_selected_events; j++) {
    if (*current_size >= VT_PLUGIN_COUNTERS_PER_THREAD) {
      vt_error_msg("You're about to add more then %i plugin counters,"
        "which is impossible\n", VT_PLUGIN_COUNTERS_PER_THREAD);
      continue;
    }
    if (current_plugin->info.synch == VT_PLUGIN_CNTR_ASYNCH_CALLBACK) {
      if (*current_size == 0) {
      }
    }
    /* add counter */
    current[*current_size].from_plugin_id = current_plugin->info.add_counter(
        current_plugin->selected_events[j]);

    /* add successfully? */
    if (current[*current_size].from_plugin_id < 0) {
      vt_error_msg(
          "Error while adding plugin counter \"%s\" to thread \"%s%s\"\n",
          current_plugin->selected_events[j], thrd->name, thrd->name_suffix);
      continue;
    }
    /* get the vampir trace id for the counter */
    current[*current_size].vt_counter_id = current_plugin->vt_counter_ids[j];
    current[*current_size].vt_asynch_key = current_plugin->vt_asynch_keys[j];
    current[*current_size].enable_counter = current_plugin->info.enable_counter;
    current[*current_size].disable_counter
        = current_plugin->info.disable_counter;

    /* per type stuff */
    if (current_plugin->info.synch == VT_PLUGIN_CNTR_SYNCH)
      /* synch counters have to implement getValue */
      current[*current_size].getValue = current_plugin->info.get_current_value;
    if ((current_plugin->info.synch == VT_PLUGIN_CNTR_ASYNCH_EVENT)
        || (current_plugin->info.synch == VT_PLUGIN_CNTR_ASYNCH_POST_MORTEM)) {
      /* these have to implement getAllValues */
      current[*current_size].getAllValues = current_plugin->info.get_all_values;
    }
    if (current_plugin->info.synch == VT_PLUGIN_CNTR_ASYNCH_CALLBACK) {
      /* callback should set the callback function */
      /* allocate resources */
#if (defined(VT_MT) || defined (VT_HYB) || defined(VT_JAVA))
      VTThrd_createMutex(
          (VTThrdMutex **) &(current[*current_size].callback_mutex)
      );
      /* try to set callback function */
      if (current_plugin->info.set_callback_function(&current[*current_size],
          current[*current_size].from_plugin_id, callback_function)) {
        vt_error_msg("Asynchronous callback plugin %s failed "
          "to set callback function for counter %s.\n", current_plugin->name,
            current_plugin->selected_events[j]);
      }

      current[*current_size].callback_values = malloc(
          max_values_callback * sizeof(vt_plugin_cntr_timevalue));
      if (current[*current_size].callback_values == NULL) {
        vt_error_msg("Failed to allocate memory for callback buffer\n");
      }
#else
      vt_error_msg(
          "callback events need thread support, you might use"
          " -vt:mt or -vt:hyb\n");
      continue;
#endif  /* VT_MT || VT_HYB || VT_JAVA */
    }

    current[*current_size].tid = VT_MY_THREAD;/*
    switch (current_plugin->info.run_per) {
    case VT_PLUGIN_CNTR_PER_PROCESS:
      if (thread_group != INVALID_GROUP_NUMBER)
        current[*current_size].tid = thread_group;
      break;
    case VT_PLUGIN_CNTR_PER_HOST:
      if (current_plugin->info.run_per == VT_PLUGIN_CNTR_PER_HOST)
        if (host_group != INVALID_GROUP_NUMBER)
          current[*current_size].tid = host_group;
      break;
    case VT_PLUGIN_CNTR_ONCE:
      if (current_plugin->info.run_per == VT_PLUGIN_CNTR_ONCE)
        if (all_group != INVALID_GROUP_NUMBER)
          current[*current_size].tid = all_group;
      break;
    }*/
    /* Next counter */
    (*current_size)++;
  }
}

uint32_t vt_plugin_cntr_get_num_synch_metrics(VTThrd * thrd) {
  uint32_t num = 0;
  vt_libassert(thrd != NULL);
  if (thrd->plugin_cntr_defines == NULL)
    return 0;
  num = ((struct vt_plugin_cntr_defines *)(thrd->plugin_cntr_defines))
      ->size_of_counters[VT_PLUGIN_CNTR_SYNCH];
  return num;
}

uint64_t vt_plugin_cntr_get_synch_value(VTThrd * thrd,
    int threadIncrementingID, uint32_t * cid, uint64_t * value) {
  *cid = ((struct vt_plugin_cntr_defines *) (thrd->plugin_cntr_defines))
      ->counters[VT_PLUGIN_CNTR_SYNCH][threadIncrementingID].vt_counter_id;
  *value = ((struct vt_plugin_cntr_defines *) (thrd->plugin_cntr_defines))
    ->counters[VT_PLUGIN_CNTR_SYNCH][threadIncrementingID].getValue(
        ((struct vt_plugin_cntr_defines *) (thrd->plugin_cntr_defines))
          ->counters[VT_PLUGIN_CNTR_SYNCH][threadIncrementingID].from_plugin_id
     );
  return *value;
}

void vt_plugin_cntr_thread_exit(VTThrd * thrd) {
  uint32_t i, j;

  struct vt_plugin_cntr_defines * defines =
      (struct vt_plugin_cntr_defines *) thrd->plugin_cntr_defines;

  vt_cntl_msg(3, "Process %i Thread %s-%s exits plugin counters ...",
      vt_my_ptrace, thrd->name, thrd->name_suffix);

  /* make sure that we can process */
  if (defines == NULL)
    return;

  vt_plugin_cntr_thread_disable_counters(thrd);
  /* free per thread resources */
  if (defines->counters != NULL) {
    for (i = 0; i < VT_PLUGIN_CNTR_SYNCH_TYPE_MAX; i++) {
      if (defines->counters[i] != NULL) {
        if (defines->size_of_counters != NULL) {
          for (j = 0; j < defines->size_of_counters[i]; j++) {
#if (defined(VT_MT) || defined (VT_HYB) || defined(VT_JAVA))
            if (defines->counters[i][j].callback_mutex!=NULL)
              VTThrd_lock((VTThrdMutex **) &defines->counters[i][j].callback_mutex);
            if (defines->counters[i][j].callback_values != NULL) {
              free(defines->counters[i][j].callback_values);
              defines->counters[i][j].callback_values = NULL;
              vt_trace_off(thrd->tid, 0, 1);
            }
            if (defines->counters[i][j].callback_mutex != NULL) {
              VTThrd_unlock((VTThrdMutex **) &defines->counters[i][j].callback_mutex);
              VTThrd_deleteMutex((VTThrdMutex **) &(defines->counters[i][j].callback_mutex));
            }
#endif /* VT_MT || VT_HYB || VT_JAVA */
          }
        }
        free(defines->counters[i]);
      }
    }
    free(defines->counters);
  }
  if (defines->size_of_counters != NULL)
    free(defines->size_of_counters);
  free(defines);

  vt_cntl_msg(3, "Process %i Thread %s-%s exits plugin counters ... done",
      vt_my_ptrace, thrd->name, thrd->name_suffix);
}

/**
 * This is called by a callback plugin
 * It writes data to a data buffer, which is later "collected"
 * with write_callback_data by vampir_trace
 */
int32_t callback_function(void * ID, vt_plugin_cntr_timevalue tv) {
  vt_plugin_cntr_timevalue * timeList;
  struct vt_plugin_single_counter * defines =
      (struct vt_plugin_single_counter *) ID;
  if (defines == NULL)
    return -1;

  if (defines->callback_values == NULL)
    return -1;

  if (defines->current_callback_write_position >= max_values_callback)
    return VT_PLUGIN_CNTR_CALLBACK_BUFFER_FULL;

#if (defined(VT_MT) || defined (VT_HYB) || defined(VT_JAVA))
  VTThrd_lock((VTThrdMutex **) &defines->callback_mutex);
#endif  /* VT_MT || VT_HYB || VT_JAVA */

  timeList = defines->callback_values;
  if (timeList == NULL) {
#if (defined(VT_MT) || defined (VT_HYB) || defined(VT_JAVA))  
    VTThrd_unlock((VTThrdMutex **) &defines->callback_mutex);
#endif  /* VT_MT || VT_HYB || VT_JAVA */
    return -1;
  }

  if (defines->current_callback_write_position >= max_values_callback) {
#if (defined(VT_MT) || defined (VT_HYB) || defined(VT_JAVA))
    VTThrd_unlock((VTThrdMutex **) &defines->callback_mutex);
#endif  /* VT_MT || VT_HYB || VT_JAVA */
    return VT_PLUGIN_CNTR_CALLBACK_BUFFER_FULL;
  }

  if (VTTHRD_TRACE_STATUS(VTThrdv[defines->tid]) == VT_TRACE_OFF_PERMANENT) {
#if (defined(VT_MT) || defined (VT_HYB) || defined(VT_JAVA))
    VTThrd_unlock((VTThrdMutex **) &defines->callback_mutex);
#endif  /* VT_MT || VT_HYB || VT_JAVA */
    return VT_PLUGIN_CNTR_CALLBACK_TRACE_OFF_PERMANENT;
  }

  timeList[defines->current_callback_write_position++] = tv;

#if (defined(VT_MT) || defined (VT_HYB) || defined(VT_JAVA))
  VTThrd_unlock((VTThrdMutex **) &defines->callback_mutex);
#endif  /* VT_MT || VT_HYB || VT_JAVA */

  return VT_PLUGIN_CNTR_CALLBACK_OK;
}

/**
 * This should be used when writing asynch data
 * counter is a struct singlecounter for the current plugin counter
 * timevalue is the timevalue to write
 * dummy_time could be a pointer to the current wtime
 */

#define VTGEN_ALIGN_LENGTH(bytes)                                   \
  ( ( (bytes) % SIZEOF_VOIDP ) ?                                    \
    ( (bytes) / SIZEOF_VOIDP + 1 ) * SIZEOF_VOIDP : (bytes) )

#define WRITE_ASYNCH_DATA(thrd, tid, counter, timevalue, dummy_time)   \
	if (VTTHRD_TRACE_STATUS(thrd) == VT_TRACE_ON){                     \
	if ((timevalue).timestamp > 0){                                    \
		vt_guarantee_buffer((tid), (dummy_time),                       \
				 VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_KeyValue))      \
               + VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_Counter)));     \
		vt_next_async_time((tid),                                      \
		    		(counter).vt_asynch_key,                           \
		    		(timevalue).timestamp);                            \
	    vt_count( (tid),                                               \
		        (dummy_time),                                          \
		        (counter).vt_counter_id,                               \
		        (timevalue).value);                                    \
	}}

/**
 * write collected callback data to otf
 * this has to be done in a mutex. currently the mutex is per thread,
 * later it might be done per counter of a thread?
 * May possibly alter the provided time to accomodate a buffer flush
 */
void vt_plugin_cntr_write_callback_data(uint64_t *time, uint32_t tid) {
  uint32_t i, k;
  struct vt_plugin_single_counter * current_counter;
  vt_plugin_cntr_timevalue * values;
  struct vt_plugin_cntr_defines * plugin_cntr_defines =
      (struct vt_plugin_cntr_defines *) VTThrdv[tid]->plugin_cntr_defines;

  if (plugin_cntr_defines == NULL)
    return;
  if (plugin_cntr_defines->size_of_counters[VT_PLUGIN_CNTR_ASYNCH_CALLBACK]
      == 0)
    return;
  for (i = 0;
    i < plugin_cntr_defines->size_of_counters[VT_PLUGIN_CNTR_ASYNCH_CALLBACK];
    i++) {
    current_counter
        = &plugin_cntr_defines->counters[VT_PLUGIN_CNTR_ASYNCH_CALLBACK][i];
#if (defined(VT_MT) || defined (VT_HYB) || defined(VT_JAVA))
    VTThrd_lock((VTThrdMutex **) &(current_counter->callback_mutex));
#endif  /* VT_MT || VT_HYB || VT_JAVA */
    values = current_counter->callback_values;
    for (k = 0; k < current_counter->current_callback_write_position; k++) {
      WRITE_ASYNCH_DATA(VTThrdv[tid], tid, (*current_counter), values[k], time);
    }
    current_counter->current_callback_write_position = 0;
#if (defined(VT_MT) || defined (VT_HYB) || defined(VT_JAVA))
    VTThrd_unlock((VTThrdMutex **) &(current_counter->callback_mutex));
#endif  /* VT_MT || VT_HYB || VT_JAVA */
  }
}

/* to be called per thread
 * May possibly alter the provided time to accomodate a buffer flush
 */
void vt_plugin_cntr_write_asynch_event_data(uint64_t *time, uint32_t tid) {
  uint32_t counter_index;
  vt_plugin_cntr_timevalue * time_values = NULL;
  uint32_t number_of_counters;
  uint64_t number_of_values = 0;
  uint64_t i;
  struct vt_plugin_single_counter current_counter;
  struct vt_plugin_cntr_defines * plugin_cntr_defines =
      (struct vt_plugin_cntr_defines *) VTThrdv[tid]->plugin_cntr_defines;
  if (plugin_cntr_defines == NULL)
    return;

  if (plugin_cntr_defines->size_of_counters[VT_PLUGIN_CNTR_ASYNCH_EVENT] == 0)
    return;
  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON)
    return;
  /* for all post_mortem counters */
  number_of_counters
      = plugin_cntr_defines->size_of_counters[VT_PLUGIN_CNTR_ASYNCH_EVENT];
  /* we assume that for each counter (not plugin),
   * the data is monotonically increasing */
  /* for all counters of this thread */
  for (counter_index = 0; counter_index < number_of_counters; counter_index++) {
    current_counter
        = plugin_cntr_defines->counters[VT_PLUGIN_CNTR_ASYNCH_EVENT]
                                       [counter_index];
    /* get data */
    number_of_values = current_counter.getAllValues(
        current_counter.from_plugin_id, &time_values);
    if (time_values == NULL)
      return;
    for (i = 0; i < number_of_values; i++) {
      WRITE_ASYNCH_DATA(VTThrdv[tid], tid, current_counter, time_values[i], time);
    }
  }
}

/* may be called per thread */
void vt_plugin_cntr_write_post_mortem(VTThrd * thrd) {
  uint32_t counter_index;
  uint64_t * counter_current_indices;
  vt_plugin_cntr_timevalue ** time_values_by_counter = NULL;
  uint32_t number_of_counters;
  uint64_t * number_of_values_by_counter;
  uint64_t dummy_time;
  uint32_t tid;
  struct vt_plugin_single_counter current_counter;
  struct vt_plugin_cntr_defines * plugin_cntr_defines =
      (struct vt_plugin_cntr_defines *) thrd->plugin_cntr_defines;
  if (plugin_cntr_defines == NULL)
    return;

  if (plugin_cntr_defines->size_of_counters[VT_PLUGIN_CNTR_ASYNCH_POST_MORTEM]
      == 0)
    return;
  if (VTTHRD_TRACE_STATUS(thrd) != VT_TRACE_ON)
    return;
  for (tid=0;tid<VTThrdn;tid++)
    if ( VTThrdv[tid] == thrd )
      break;
  if ( tid == VTThrdn ){
    vt_warning("Can not determine internal TID when gathering post-mortem counters");
    return;
  }
  /* for all post_mortem counters */
  number_of_counters
    = plugin_cntr_defines->size_of_counters[VT_PLUGIN_CNTR_ASYNCH_POST_MORTEM];
  dummy_time = vt_pform_wtime();
  /* set flag for writing post mortem counters; prevents writing of flush
   * enter/exit event when flushing */
  thrd->plugin_cntr_writing_post_mortem = 1;
  /* we assume that for each counter (not plugin),
   * the data is monotonically increasing */
  /* for all counters of this thread */

  time_values_by_counter = calloc(number_of_counters, sizeof(*time_values_by_counter));
  vt_libassert(time_values_by_counter);
  number_of_values_by_counter = calloc(number_of_counters, sizeof(*number_of_values_by_counter));
  vt_libassert(number_of_values_by_counter);
  for (counter_index = 0;
       counter_index < number_of_counters;
       counter_index++) {
    current_counter
        = plugin_cntr_defines->counters[VT_PLUGIN_CNTR_ASYNCH_POST_MORTEM][counter_index];
    /* get data */
    number_of_values_by_counter[counter_index] = current_counter.getAllValues(
        current_counter.from_plugin_id, &(time_values_by_counter[counter_index]));
    if (time_values_by_counter[counter_index] == NULL) {
      free(time_values_by_counter);
      free(number_of_values_by_counter);
      return;
    }
  }
  /* initialized with 0! */
  counter_current_indices = calloc(number_of_counters, sizeof(*counter_current_indices));
  vt_libassert(counter_current_indices);
  while (1) {
	vt_plugin_cntr_timevalue *min_tvp = NULL;
	uint32_t min_counter;
    for (counter_index = 0;
	     counter_index < number_of_counters;
	     counter_index++)
    {
      /*
       * TODO optimize for "nice" plugins by looking if the "next" counter has the
       * _same_ timestamp (so there cannot be anyone else with a smaller one
       */
	  vt_plugin_cntr_timevalue *tvp;
	  if (counter_current_indices[counter_index] >= number_of_values_by_counter[counter_index]) {
		continue;
	  }
	  tvp = &(time_values_by_counter[counter_index][counter_current_indices[counter_index]]);
      if (!min_tvp || tvp->timestamp < min_tvp->timestamp) {
        min_tvp = tvp;
        min_counter = counter_index;
      }
	}
    if (min_tvp == NULL) {
      /* we are done */
      break;
    }
    current_counter
        = plugin_cntr_defines->counters[VT_PLUGIN_CNTR_ASYNCH_POST_MORTEM][min_counter];
    WRITE_ASYNCH_DATA(thrd, tid, current_counter, *min_tvp, &dummy_time);
    counter_current_indices[min_counter]++;
  }
  free(time_values_by_counter);
  free(counter_current_indices);
  free(number_of_values_by_counter);
  /* unset flag for writing post mortem counters */
  thrd->plugin_cntr_writing_post_mortem = 0;
}

int vt_plugin_cntr_is_registered_monitor_thread() {
  uint32_t i, j;
  /* check whether tracing this thread is allowed */
  for (j = 0; j < VT_PLUGIN_CNTR_SYNCH_TYPE_MAX; j++)
    for (i = 0; i < nr_plugins[j]; i++)
      /* function implemented? */
      if (vt_plugin_handles[j][i].info.is_thread_registered)
        /* does it return != 0 ? */
        if (vt_plugin_handles[j][i].info.is_thread_registered())
          return 1;
  return 0;
}

void vt_plugin_cntr_set_all_group(uint32_t group_id) {
  all_group = group_id;
}
void vt_plugin_cntr_set_host_group(uint32_t group_id) {
  host_group = group_id;
}
void vt_plugin_cntr_set_process_group(uint32_t group_id) {
  thread_group = group_id;
}
