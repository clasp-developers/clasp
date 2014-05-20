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

#include "papi.h"

#include "vt_defs.h"
#include "vt_env.h"
#include "vt_error.h"
#include "vt_inttypes.h"
#include "vt_iowrap.h"
#include "vt_metric.h"

#if !(defined(HAVE_LONG_LONG) && HAVE_LONG_LONG)
# define long_long long long
#endif /* HAVE_LONG_LONG */

#if PAPI_VER_CURRENT >= PAPI_VERSION_NUMBER(3,9,0,0)
# define PAPIC
#endif /* PAPI_VER_CURRENT >= 3.9.0.0 */
#if PAPI_VER_CURRENT >= PAPI_VERSION_NUMBER(5,0,0,0)
# define PAPIV
#endif /* PAPI_VER_CURRENT >= 5.0.0.0 */

#ifndef TIMER_PAPI_REAL_CYC
# define TIMER_PAPI_REAL_CYC 10
#endif /* TIMER_PAPI_REAL_CYC */
#ifndef TIMER_PAPI_REAL_USEC
# define TIMER_PAPI_REAL_USEC 11
#endif /* TIMER_PAPI_REAL_USEC */

typedef enum {
  METMAP_UNKNOWN=0x0,
  METMAP_MEASURE=0x1,
  METMAP_AGGROUP=0x2,
  METMAP_COMPOSE=0x4,
  METMAP_COMPUTE=0x8,
  METMAP_INVALID=0x10
} metmap_t;

typedef struct metricmap_t {
  metmap_t            type;
  char*               event_name;
  char*               alias_name;
  struct metricmap_t* next;
} metricmap_t;

struct metric
{
  char* name;
  char  descr[PAPI_HUGE_STR_LEN];
  int   papi_code;
  uint32_t props;
};

typedef struct eventmap_t   /* we need an eventset for each component */
{
  int EventId; /* eventset id */
  long_long Values[VT_METRIC_MAXNUM]; /* return values for the eventsets */
  int nEvents; /* number of recorded events in this set */
  int ComponentId;
}eventmap_t;

struct vt_metv
{
  struct eventmap_t * EventSet[VT_METRIC_MAXNUM]; /* a struct for each active component */
  long_long * Values[VT_METRIC_MAXNUM]; /* for each counter a pointer, that points to the eventsets return values */
};

/*
 * Global variables
 */
static struct metric* metricv[VT_METRIC_MAXNUM];
static int    nmetrics = 0;

static metricmap_t* metricmap_append(metricmap_t* map,
                                     metmap_t type,
                                     char* event, char* alias)
{
  /*printf("Def 0x%X %s = <%s>\n", type, event, alias);*/

  if (map == NULL) {
    map = (metricmap_t*)calloc(1, sizeof(metricmap_t));
    if (map == NULL) {
      vt_cntl_msg(2, "Metricmap creation failed!");
      return NULL;
    }
    /*printf("Created new metricmap head @0x%p\n", map);*/
  } else {
    while (map->next != NULL) map = map->next;
    map->next = (metricmap_t*)calloc(1, sizeof(metricmap_t));
    if (map->next == NULL) {
      vt_cntl_msg(2, "Metricmap append failed!");
      return NULL;
    }
    map = map->next;
    /*printf("Created new metricmap node @0x%p\n", map);*/
  }

  map->type = type;
  map->event_name = strdup(event);
  map->alias_name = strdup(alias);
  map->next = NULL;

  return map;
}

static void metricmap_dump(metricmap_t* map)
{
  unsigned i = 0;

  if (map == NULL || vt_env_verbose() < 3)
    return;

  vt_cntl_msg(3, "Metricmap dump (head=0x%p):", (void*)map);
  while (map != NULL) {
    vt_cntl_msg(3, "m[%3u] 0x%X %s = %s", i, map->type,
                map->event_name, map->alias_name);
    i++;
    map = map->next;
  }
  vt_cntl_msg(3, "Metricmap dumped %u maps", i);
}

static void metricmap_free(metricmap_t* map)
{
  if (map == NULL)
    return;

  vt_cntl_msg(3, "Metricmap free (head=0x%p):", map);
  while (map != NULL) {
    metricmap_t* next = map->next;
    if (map->event_name != NULL) free(map->event_name);
    if (map->alias_name != NULL) free(map->alias_name);
    free(map);
    map = next;
  }
}

/* METRICS.SPEC entry format: */
/* measure MEASURE_DEF = NAME                // no operators */
/* aggroup AGGROUP_DEF = NAME1 NAME2 ...     // no operators */
/* compose COMPOSE_DEF = NAME1 + NAME2 + ... // one or more "+" */
/* compute COMPUTE_DEF = NAME1 & NAME2 & ... // "&" is "+-* /" */

/* Initialize metric map */
static metricmap_t* vt_metricmap_init(metmap_t match)
{
  metricmap_t* mapv = NULL, *map = NULL;
  char* specfile = vt_env_metrics_spec();
  unsigned lineno=0, defs=0;
  unsigned invalid_defs=0, unknown_defs=0;
  unsigned measure_defs=0, aggroup_defs=0, compose_defs=0, compute_defs=0;
  char line[1024];
  FILE *fp;

  if (!specfile) return NULL;

  fp = fopen(specfile, "r");
  if (fp == NULL) {
    vt_cntl_msg(2, "Failed to open metric specification %s: %s",
                specfile, strerror(errno));
    return NULL;
  }

  /*printf("specfile=%s match=0x%X\n", specfile, match);*/

  while (fgets(line, sizeof(line), fp)) {
    metmap_t type=METMAP_UNKNOWN;
    char* def_name, *def_args;
    int len = strcspn(line, "#\n"); /* length of non-comment string */
    while (len && ((line[len-1] == ' ') || (line[len-1] == '\t'))) len--;
    line[len] = '\0'; /* chop comment and return */
    lineno++;
    if (len <= 1) continue;
    defs++;
    if      (!strncmp("measure", line, 7)) type=METMAP_MEASURE;
    else if (!strncmp("compose", line, 7)) type=METMAP_COMPOSE;
    else if (!strncmp("compute", line, 7)) type=METMAP_COMPUTE;
    else if (!strncmp("aggroup", line, 7)) type=METMAP_AGGROUP;
    /*printf("%3d:%2d %d-[%2d] %s\n", lineno, defs, type, len, line);*/
    if (type == METMAP_UNKNOWN) {
      unknown_defs++;
      vt_cntl_msg(2, "Failed to parse metric definition line %d: %s", lineno, line);
      continue;
    }
    line[7] = '\0'; /* terminate definition type */
    def_name = line + 8;
    def_name += strspn(def_name, " \t"); /* get start of definition name */
    len = strcspn(def_name, "= \t"); /* length of definition name */
    *(def_name+len)='\0'; /* terminate definition name */
    def_args = line + 8 + len + 1;
    def_args += strspn(def_args, "= \t"); /* get start of def argument */
    /*printf("Def %2d:<%s> %s <%s>\n", defs, def_name, line, def_args);*/
    len = strlen(def_args); /* length of definition arguments */
    if (((type == METMAP_MEASURE) && (match & METMAP_MEASURE)) ||
        ((type == METMAP_AGGROUP) && (match & METMAP_AGGROUP))) {
      if (((int)strcspn(def_args, "=+") != len) ||
          (((int)strcspn(def_args, "=+-*/ \t") != len)
           && (type == METMAP_MEASURE))) {
        type = METMAP_INVALID;
        invalid_defs++;
        vt_cntl_msg(2, "XXXX Def %d:%s <%s> invalid!", lineno, line, def_name);
      } else {
        map = metricmap_append(map, type, def_name, def_args);
        measure_defs++;
      }
    } else if ((type == METMAP_COMPOSE) && (match & METMAP_COMPOSE)) {
       map = metricmap_append(map, type, def_name, def_args);
       compose_defs++;
    } else if ((type == METMAP_COMPUTE) && (match & METMAP_COMPUTE)) {
       map = metricmap_append(map, type, def_name, def_args);
       compute_defs++;
    }
    if (mapv == NULL) mapv = map; /* initialise head of vector */
  }
  vt_cntl_msg(2, "Mapped %d/%d defs from \"%s\"",
              measure_defs+aggroup_defs+compose_defs+compute_defs, defs, specfile);
#if 0
  printf("measure %d aggroup %d compose %d compute %d unknown %d invalid %d\n",
         measure_defs, aggroup_defs, compose_defs, compute_defs,
         unknown_defs, invalid_defs);
#endif
  fclose(fp);
  return mapv;
}

static void metricv_add(char* name, int code, uint32_t props)
{
  if (nmetrics >= VT_METRIC_MAXNUM) {
    vt_error_msg("Number of counters exceeds VampirTrace allowed maximum "
                 "of %d", VT_METRIC_MAXNUM);
  } else {
    metricv[nmetrics] = (struct metric*)malloc(sizeof(struct metric));
    metricv[nmetrics]->name = strdup(name);
    metricv[nmetrics]->descr[0] = '\0';
    metricv[nmetrics]->props = props;
    metricv[nmetrics]->papi_code = code;
    nmetrics++;
  }
}

/* PAPI-specific error message */

static void metric_error(int errcode, char *note)
{
  char errstring[PAPI_MAX_STR_LEN];

#ifdef PAPIV
  PAPI_perror(errstring);
#else
  PAPI_perror(errcode, errstring, PAPI_MAX_STR_LEN);
#endif
  if (errcode == PAPI_ESYS) {
    strncat(errstring, ": ", PAPI_MAX_STR_LEN-strlen(errstring));
    strncat(errstring, strerror(errno), PAPI_MAX_STR_LEN-strlen(errstring));
  }
  vt_error_msg("%s: %s (fatal)\n", note?note:"PAPI", errstring);
}

/* PAPI-specific warning message */

static void metric_warning(int errcode, char *note)
{
  char errstring[PAPI_MAX_STR_LEN];

#ifdef PAPIV
  PAPI_perror(errstring);
#else
  PAPI_perror(errcode, errstring, PAPI_MAX_STR_LEN);
#endif
  if (errcode == PAPI_ESYS) {
    strncat(errstring, ": ", PAPI_MAX_STR_LEN-strlen(errstring));
    strncat(errstring, strerror(errno), PAPI_MAX_STR_LEN-strlen(errstring));
  }
  vt_warning("%s: %s (ignored)\n", note?note:"PAPI", errstring);
}

/* Get metric descriptions */
static void metric_descriptions(void)
{
  int i, j, k, retval;
  PAPI_event_info_t info;

  for (i=0; i < nmetrics; i++) {
    memset(&info, 0, sizeof(PAPI_event_info_t));
    retval = PAPI_get_event_info(metricv[i]->papi_code, &info);
    if (retval != PAPI_OK)
      metric_error(retval, "PAPI_get_event_info");

    if (strcmp(info.long_descr, metricv[i]->name) != 0) {
      strncpy(metricv[i]->descr, info.long_descr, sizeof(metricv[i]->descr));

      /* tidy description if necessary */
      j=strlen(metricv[i]->descr)-1;
      if (metricv[i]->descr[j] == '\n') metricv[i]->descr[j]='\0';
      j=strlen(metricv[i]->descr)-1;
      if (metricv[i]->descr[j] != '.')
        strncat(metricv[i]->descr, ".",
                sizeof(metricv[i]->descr)-strlen(metricv[i]->descr));
    }

    if (metricv[i]->papi_code & PAPI_PRESET_MASK) { /* PAPI preset */
      char *postfix_chp = info.postfix;
      char derive_ch = strcmp(info.derived,"DERIVED_SUB")?'+':'-';
      strncat(metricv[i]->descr, " [ ",
              sizeof(metricv[i]->descr)-strlen(metricv[i]->descr));
      strncat(metricv[i]->descr, info.name[0],
              sizeof(metricv[i]->descr)-strlen(metricv[i]->descr));
      for (k=1; k<(int)info.count; k++) {
        char op[4];
        postfix_chp = postfix_chp?strpbrk(++postfix_chp, "+-*/"):NULL;
        sprintf(op, " %c ", (postfix_chp?*postfix_chp:derive_ch));
        strncat(metricv[i]->descr, op,
                sizeof(metricv[i]->descr)-strlen(metricv[i]->descr));
        strncat(metricv[i]->descr, info.name[k],
                sizeof(metricv[i]->descr)-strlen(metricv[i]->descr));
      }
      strncat(metricv[i]->descr, " ]",
              sizeof(metricv[i]->descr)-strlen(metricv[i]->descr));
      if (strcmp(info.symbol, metricv[i]->name) != 0) { /* add preset name */
        strncat(metricv[i]->descr, " = ",
                sizeof(metricv[i]->descr)-strlen(metricv[i]->descr));
        strncat(metricv[i]->descr, info.symbol,
                sizeof(metricv[i]->descr)-strlen(metricv[i]->descr));
      }
    }

    /*printf("Metric %d: <%s>\n<<%s>>\n", i, metricv[i]->name, metricv[i]->descr);*/
  }
}

/* Test whether requested event combination valid */
static void metric_test(void)
{
  int i, j;
  int retval;

  int component;
  struct eventmap_t * EventSet[VT_METRIC_MAXNUM];
  for (i=0; i<VT_METRIC_MAXNUM; i++)
    EventSet[i] = NULL;
  for (i=0; i < nmetrics; i++) {
#ifdef PAPIC
    /* Preset-counter belong to Component 0! */
    component = PAPI_COMPONENT_INDEX(metricv[i]->papi_code);
#else
    component = 0;
#endif
    /* search for the eventset that matches the counter */
    j=0;
    while (EventSet[j]!=NULL && j < VT_METRIC_MAXNUM && EventSet[j]->ComponentId!=component){
      j++;
    }
    if (EventSet[j]==NULL) /* create eventset, if no matching found */
    {
      EventSet[j] = malloc(sizeof(eventmap_t));
      EventSet[j]->EventId=PAPI_NULL;
      retval = PAPI_create_eventset(&(EventSet[j]->EventId));
      if ( retval != PAPI_OK)
        metric_error(retval, "PAPI_create_eventset");
      EventSet[j]->ComponentId=component;
    }
    /* add event to event set */
    retval = PAPI_add_event(EventSet[j]->EventId, metricv[i]->papi_code);
    if ( retval != PAPI_OK ) {
      char errstring[PAPI_MAX_STR_LEN];
      sprintf(errstring, "PAPI_add_event(%d:\"%s\")", i, metricv[i]->name);
      metric_error(retval, errstring);
    }
    vt_cntl_msg(2, "Event %s added to event set", metricv[i]->name);
  }
  /* foreach used eventset */
  for (i=0; i < VT_METRIC_MAXNUM && EventSet[i]!=NULL; i++)
  {
    retval = PAPI_cleanup_eventset(EventSet[i]->EventId);
    if ( retval != PAPI_OK )
      metric_error(retval, "PAPI_cleanup_eventset");

    retval = PAPI_destroy_eventset(&(EventSet[i]->EventId));
    if ( retval != PAPI_OK )
      metric_error(retval, "PAPI_destroy_eventset");
    free(EventSet[i]);
  }

  vt_cntl_msg(2, "Event set tested OK");
}

int vt_metric_open()
{
  int retval;
  char* env;
  char* env_sep;
  char* var;
  char* token;
  char* saveptr;
  PAPI_event_info_t info;
  metricmap_t* mapv = NULL;
  metricmap_t* map;

  /* read environment variable "VT_METRICS". Return if
     uset and no PAPI timer used. */
  env = vt_env_metrics();
  if( env == NULL )
  {
#if TIMER != TIMER_PAPI_REAL_CYC && TIMER != TIMER_PAPI_REAL_USEC
    return nmetrics;
#endif
  }

  env_sep = vt_env_metrics_sep();

  mapv = vt_metricmap_init(
    (metmap_t)(METMAP_MEASURE|METMAP_AGGROUP));
  metricmap_dump(mapv);

  /* initialize PAPI */
  retval = PAPI_library_init(PAPI_VER_CURRENT);
  if ( retval != PAPI_VER_CURRENT )
    metric_error(retval, "PAPI_library_init");

  /* return if environment variable is unset */
  if ( env == NULL )
    return nmetrics;

  var = strdup(env);
  vt_cntl_msg(2, "VT_METRICS=%s", var);

  /* read metrics from specification string */
  token = strtok_r(var, env_sep, &saveptr);
  while ( token && (nmetrics < VT_METRIC_MAXNUM) ) {
    /* set counter properties */
    uint32_t props;
    if (token[0]=='!')
    {
      props = VT_CNTR_ABS | VT_CNTR_NEXT;
      token++;
    }
    else
    {
      props = VT_CNTR_ACC;
    }
    /* search metricmap for a suitable definition */
    map = mapv;
    /*printf("Token%d: <%s>\n", nmetrics, token);*/
    while (map != NULL) {
      if ( strcmp(map->event_name, token) == 0 ) {
        /*printf("Definition %s = <%s>\n", token, map->alias_name);*/
        /* expand definition and set components */
        char* c_token = map->alias_name;
        int len = strcspn(c_token, " \t"); /* first token */
        int got_valid_match = 1; /* to be verified */
        int k = 0;
        do { /* verify each component of definition is available */
          char component[64];
          int code = -1;
          strncpy(component, c_token, len);
          component[len] = '\0';
          /*printf("Comp[%d] <%s>\n", k, component);*/
          c_token += len + strspn(c_token+len, " \t");
          len = strcspn(c_token, " \t"); /* next token */

          PAPI_event_name_to_code(component, &code);
          memset(&info, 0, sizeof(PAPI_event_info_t));
          retval = PAPI_get_event_info(code, &info);
          /*printf("v[%d] %s [0x%X] %d\n", k, component, code, info.count);*/

          if (info.count == 0) {
            /*printf("Event %s *N/A*\n", component);*/
            got_valid_match = 0;
          } else if ((k==0) && (len==0)) { /* use provided event name */
            metricv_add(token, code, props);
          } else { /* use alias component name */
            metricv_add(component, code, props);
          }
          k++;
        } while (got_valid_match && (len > 0));
        if (got_valid_match) {
          /*printf("Definition %s = <%s> OK\n", map->event_name, map->alias_name);*/
          break; /* accept this event definition */
        }
      }
      map = map->next;
    }

    if (map == NULL) { /* no map match, so try given name */
      int code = -1;
      char* component = token;
      /*printf("Comp[X] <%s>\n", component);*/
      retval = PAPI_event_name_to_code(component, &code);
      if (retval != PAPI_OK || code == -1)
        vt_error_msg("Metric <%s> not supported\n", component);

      memset(&info, 0, sizeof(PAPI_event_info_t));
      retval = PAPI_get_event_info(code, &info);
      /*printf("v[%d] %s [0x%X] %d\n", nmetrics, component, code, info.count);*/
      if (retval != PAPI_OK)
        vt_error_msg("Metric <%s> not available\n", component);

      metricv_add(component, code, props);
    }

    token = strtok_r(NULL, env_sep, &saveptr);
  }

  /*printf("nmetrics=%d\n", nmetrics);*/

  /* clean up */
  metricmap_free(mapv);
  free(var);

  /* Check whether event combination is valid. This is done here to
     avoid errors when creating the event set for each thread, which
     would multiply the error messages. */
  metric_test();

  metric_descriptions();

  return nmetrics;
}

void vt_metric_close()
{
  int i;

  for ( i = 0; i < nmetrics; i++ ) {
    free (metricv[i]->name);
    free(metricv[i]);
  }
  if ( nmetrics > 0 )
    PAPI_shutdown();
}

struct vt_metv* vt_metric_create()
{
  struct vt_metv* metv;
  int retval, i,j;
  int component;

  if ( nmetrics == 0 )
    return NULL;

  metv = (struct vt_metv*)malloc(sizeof(struct vt_metv));
  if ( metv == NULL )
    vt_error();

  /* create event set */
  for (i=0; i<VT_METRIC_MAXNUM; i++)
    metv->EventSet[i] = NULL;

  for (i=0; i < nmetrics; i++)
  {
    struct eventmap_t *eventset;

#ifdef PAPIC
    component = PAPI_COMPONENT_INDEX(metricv[i]->papi_code);
#else
    component = 0;
#endif
    /* search for the eventset that matches the counter */
    j=0;
    while (metv->EventSet[j]!=NULL && j < VT_METRIC_MAXNUM && metv->EventSet[j]->ComponentId!=component){
      j++;
    }
    if (metv->EventSet[j]==NULL) /* no event of this component yet! */
    {
      metv->EventSet[j] = (struct eventmap_t*)malloc(sizeof(eventmap_t));
      metv->EventSet[j]->EventId=PAPI_NULL;
      metv->EventSet[j]->nEvents = 0;
      retval = PAPI_create_eventset(&(metv->EventSet[j]->EventId));
      if ( retval != PAPI_OK)
        metric_error(retval, "PAPI_create_eventset");
      metv->EventSet[j]->ComponentId=component;
    }
    eventset = metv->EventSet[j];

    /* add event to event set */
    retval = PAPI_add_event(eventset->EventId, metricv[i]->papi_code);
    if ( retval != PAPI_OK )
      metric_error(retval, "PAPI_add_event");
    /* for demux the values from eventset -> returnvector */
    metv->Values[i] = &(eventset->Values[eventset->nEvents]);
    eventset->nEvents++;
  }

  /* foreach used eventset */
  for (i=0; i < VT_METRIC_MAXNUM && metv->EventSet[i]!=NULL; i++)
  {
    retval = PAPI_start(metv->EventSet[i]->EventId);
    if ( retval != PAPI_OK )
      metric_error(retval, "PAPI_start");
  }

  return metv;
}

void vt_metric_free(struct vt_metv* metv, uint32_t tid)
{
  int retval, i;
  long_long papi_vals[VT_METRIC_MAXNUM];

  if ( metv == NULL )
    return;

  /* treat PAPI failures at this point as non-fatal */

  VT_SUSPEND_IO_TRACING(tid);

  /* foreach used eventset */
  for (i=0; i < VT_METRIC_MAXNUM && metv->EventSet[i]!=NULL; i++)
  {
    retval = PAPI_stop(metv->EventSet[i]->EventId, papi_vals);
    if ( retval != PAPI_OK ) {
      metric_warning(retval, "PAPI_stop");
    } else { /* cleanup/destroy require successful PAPI_stop */
      retval = PAPI_cleanup_eventset(metv->EventSet[i]->EventId);
      if ( retval != PAPI_OK )
        metric_warning(retval, "PAPI_cleanup_eventset");
      retval = PAPI_destroy_eventset(&metv->EventSet[i]->EventId);
      if ( retval != PAPI_OK )
        metric_warning(retval, "PAPI_destroy_eventset");
    }
    free(metv->EventSet[i]);
  }

  VT_RESUME_IO_TRACING(tid);

  free(metv);
}

void vt_metric_thread_init(long (*id_fn)(void))
{
  int retval;

  if ( nmetrics == 0 )
    return;

  retval = PAPI_thread_init((unsigned long (*)(void))(id_fn));
  if ( retval != PAPI_OK)
    metric_error(retval, "PAPI_thread_init");
  vt_cntl_msg(2, "PAPI thread support initialized");
}

void vt_metric_thread_fini()
{
  if ( nmetrics == 0 )
    return;

  /* ignore return value */
  (void)PAPI_unregister_thread();
}

void vt_metric_read(struct vt_metv* metv, uint64_t offsets[],
                    uint64_t values[])
{
  int retval;
  int i;

  if ( metv == NULL )
    return;

  VT_SUSPEND_IO_TRACING(VT_CURRENT_THREAD);

  /* foreach used eventset */
  for (i=0; i < VT_METRIC_MAXNUM && metv->EventSet[i]!=NULL; i++)
  {
    retval = PAPI_read(metv->EventSet[i]->EventId, metv->EventSet[i]->Values );
    if ( retval != PAPI_OK )
      metric_error(retval, "PAPI_read");
  }

  if ( offsets != NULL )
    for ( i = 0; i < nmetrics; i++ )
      values[i] = (uint64_t) *metv->Values[i] + offsets[i];
  else
    for ( i = 0; i < nmetrics; i++ )
      values[i] = (uint64_t) *metv->Values[i];

  VT_RESUME_IO_TRACING(VT_CURRENT_THREAD);
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
  return metricv[i]->props;
}

#if TIMER == TIMER_PAPI_REAL_CYC

uint64_t vt_metric_clckrt(void)
{
  const PAPI_hw_info_t* hwinfo = NULL;
  double hertz;

  if (!PAPI_is_initialized()) {
    /* initialize PAPI, since it hasn't already been initialized */
    int retval = PAPI_library_init(PAPI_VER_CURRENT);
    if ( retval != PAPI_VER_CURRENT )
      metric_error(retval, "PAPI_library_init");
  }

  hwinfo = PAPI_get_hardware_info();
  if ( hwinfo == NULL)
    vt_error_msg("Failed to access PAPI hardware info\n");
  vt_cntl_msg(2, "Clock rate: %f MHz", hwinfo->mhz);

  hertz = hwinfo->mhz * 1000000.0;

  return (uint64_t)hertz;
}

uint64_t vt_metric_real_cyc(void)
{
  return (uint64_t)PAPI_get_real_cyc();
}

#elif TIMER == TIMER_PAPI_REAL_USEC

uint64_t vt_metric_real_usec(void)
{
  return (uint64_t)PAPI_get_real_usec();
}

#endif /* TIMER */
