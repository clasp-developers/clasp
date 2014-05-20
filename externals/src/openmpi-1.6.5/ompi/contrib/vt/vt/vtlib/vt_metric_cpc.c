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
#include <string.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#include "libcpc.h"

#include "vt_defs.h"
#include "vt_env.h"
#include "vt_error.h"
#include "vt_inttypes.h"
#include "vt_metric.h"

struct metric
{
  char* name;
  char* descr;
};

struct vt_metv
{
  cpc_set_t* set;
  cpc_buf_t* buffer;
  int* indices;
};

/*
 * Global variables
 */
static struct metric* metricv[VT_METRIC_MAXNUM];
static int nmetrics = 0;
static cpc_t* cpc = NULL;

static void metricv_add(char* name)
{
  if ( nmetrics >= VT_METRIC_MAXNUM )
  {
    vt_error_msg("Number of counters exceeds VampirTrace allowed maximum "
		 "of %d", VT_METRIC_MAXNUM);
  }
  else
  {
    metricv[nmetrics] = (struct metric*)malloc(sizeof(struct metric));
    metricv[nmetrics]->name = strdup(name);
    metricv[nmetrics]->descr = NULL;
    nmetrics++;
  }
}

int vt_metric_open()
{
  char* env;
  char* env_sep;
  char* var;
  char* token;

  /* read environment variable "VT_METRICS" */
  if ( ( env = vt_env_metrics() ) == NULL )
    return 0;

  env_sep = vt_env_metrics_sep();

  var = strdup(env);
  vt_cntl_msg(2, "VT_METRICS=%s", var);

  /* initialize CPC */
  if ( ( cpc = cpc_open(CPC_VER_CURRENT) ) == NULL )
    vt_error_msg("cpc_open: %s", strerror(errno));

  /* read metrics from specification string */
  token = strtok(var, env_sep);
  while ( token && (nmetrics < VT_METRIC_MAXNUM) )
  {
    metricv_add( token );
    token = strtok(NULL, env_sep);
  }

  free(var);

  return nmetrics;
}

void vt_metric_close()
{
  int i;

  for ( i = 0; i < nmetrics; i++ )
  {
    free(metricv[i]->name);
    free(metricv[i]);
  }

  cpc_close(cpc);
}

struct vt_metv* vt_metric_create()
{
  struct vt_metv* metv;
  int i;

  if ( nmetrics == 0 )
    return NULL;

  metv = (struct vt_metv*)malloc(sizeof(struct vt_metv));
  if ( metv == NULL )
    vt_error();

  /* create CPC set */
  metv->set = NULL;
  if ( ( metv->set = cpc_set_create(cpc) ) == NULL )
    vt_error_msg("cpc_set_create: %s", strerror(errno));

  metv->indices = (int*)calloc(nmetrics, sizeof(int));
  for ( i = 0; i < nmetrics; i++ )
  {
    /* add request to set and store the corresponding index */
    metv->indices[i] = cpc_set_add_request(cpc, metv->set,
					   metricv[i]->name, 0,
					   CPC_COUNT_USER, 0, NULL);
    if ( metv->indices[i] == -1 )
      vt_error_msg("cpc_set_add_request (%s): %s", metricv[i]->name,
		   strerror(errno));
  }

  /* create CPC buffer */
  if ( ( metv->buffer = cpc_buf_create(cpc, metv->set) ) == NULL )
    vt_error_msg("cpc_buf_create: %s", strerror(errno));

  /* bind set to the calling LWP */
  if ( cpc_bind_curlwp(cpc, metv->set, 0) == -1 )
    vt_error_msg("cpc_bind_curlwp: %s",  strerror(errno));

  return metv;
}

void vt_metric_free(struct vt_metv* metv, uint32_t tid)
{
  (void)tid;
  
  if ( metv == NULL )
    return;

  if ( cpc_buf_destroy(cpc, metv->buffer) == -1 )
    vt_error_msg("cpc_buf_destroy: %s", strerror(errno));
  if ( cpc_set_destroy(cpc, metv->set) == -1 )
    vt_error_msg("cpc_set_destroy: %s", strerror(errno));

  free(metv);
}

void vt_metric_thread_init(long (*id_fn)(void))
{
  (void)id_fn;
}

void vt_metric_thread_fini()
{
}

void vt_metric_read(struct vt_metv* metv, uint64_t offsets[],
                    uint64_t values[])
{
  int i;

  if ( metv == NULL )
    return;

  /* read counter values of set */
  if ( cpc_set_sample(cpc, metv->set, metv->buffer) == -1 )
    vt_error_msg("cpc_set_sample: %s", strerror(errno));

  for ( i = 0; i < nmetrics; i++ )
  {
    /* get 64-bit counter values from CPC buffer */
    if ( cpc_buf_get(cpc, metv->buffer, metv->indices[i], &(values[i])) == -1 )
      break;
  }
  if ( i != nmetrics )
    vt_error_msg("cpc_buf_get: %s", strerror(errno));

  /* add offsets to values, if necessary */
  if ( offsets != NULL )
  {
    for ( i = 0; i < nmetrics; i++ )
      values[i] += offsets[i];
  }
}

int vt_metric_num()
{
  return nmetrics;
}

const char* vt_metric_name(int i)
{
  return metricv[i]->name;
}

const char* vt_metric_descr(int i)
{
  return metricv[i]->descr;
}

const char* vt_metric_unit(int i)
{
  (void)i;
  return "#";
}

uint32_t vt_metric_props(int i)
{
  (void)i;
  return VT_CNTR_ACC;
}
