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

#include <stdlib.h>
#include <string.h>

#include <pthread.h>

#include "vt_env.h"
#include "vt_error.h"
#include "vt_inttypes.h"
#include "vt_metric.h"
#include "vt_pform.h"
#include "vt_thrd.h"
#include "vt_trc.h"

#if defined(VT_PLUGIN_CNTR)
# include "vt_plugin_cntr_int.h"
#endif /* VT_PLUGIN_CNTR */

#define MAX_MUTEXES 16

/* data structure which hold the actual Pthread mutex */
struct VTThrdMutex_struct
{
  pthread_mutex_t m;
};

/* data structure for list of idle thread-IDs */
typedef struct IdleThreadIdListEntryS
{
  uint32_t tid;
  struct IdleThreadIdListEntryS* next;
} IdleThreadIdListEntryT;

/* data structure that holds the list of idle thread-IDs per thread */
typedef struct IdleThreadIdListS
{
  IdleThreadIdListEntryT* first;
  IdleThreadIdListEntryT* last;
  uint32_t size;
} IdleThreadIdListT;

static pthread_key_t pthreadKey;

static pthread_mutex_t threadReuseMutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mutexInitMutex = PTHREAD_MUTEX_INITIALIZER;

static VTThrdMutex mutexes[MAX_MUTEXES];
static uint32_t    mutexesNum = 0;

static IdleThreadIdListT* idleThreadIds = NULL;

static uint8_t reuseThreadIds = 0;
static uint8_t masterThreadTerminated = 0;

static uint32_t idle_tid_list_size(uint32_t ptid)
{
  vt_libassert(ptid < VTThrdMaxNum);
  return idleThreadIds[ptid].size;
}

static void idle_tid_list_push_back(uint32_t ptid, uint32_t tid)
{
  IdleThreadIdListEntryT* idle_tid;

  vt_libassert(ptid < VTThrdMaxNum);

  /* create new list entry */
  idle_tid = (IdleThreadIdListEntryT*)calloc(1,
               sizeof(IdleThreadIdListEntryT));
  if (idle_tid == NULL)
    vt_error();

  idle_tid->tid = tid;

  /* append new entry to list */
  if (idleThreadIds[ptid].last)
  {
    idleThreadIds[ptid].last->next = idle_tid;
    idleThreadIds[ptid].last = idle_tid;
  }
  else
  {
    idleThreadIds[ptid].first = idleThreadIds[ptid].last = idle_tid;
  }

  /* increment size of list */
  idleThreadIds[ptid].size++;
}

static uint32_t idle_tid_list_pop_front(uint32_t ptid)
{
  uint32_t tid;
  IdleThreadIdListEntryT* tmp;

  vt_libassert(ptid < VTThrdMaxNum);
  vt_libassert(idleThreadIds[ptid].size > 0);

  /* get thread-ID from the first list entry */
  tid = idleThreadIds[ptid].first->tid;

  /* remove first list entry */
  tmp = idleThreadIds[ptid].first;
  if (idleThreadIds[ptid].first == idleThreadIds[ptid].last)
    idleThreadIds[ptid].first = idleThreadIds[ptid].last = NULL;
  else
    idleThreadIds[ptid].first = tmp->next;
  free(tmp);

  /* decrement size of list */
  idleThreadIds[ptid].size--;

  return tid;
}

static void pthread_key_destructor(void* data)
{
  uint32_t tid = *((uint32_t*)data);

#if defined(VT_METR)
  if (vt_is_alive && vt_metric_num() > 0 && VTThrdv[tid]->metv)
  {
    /* shut down metrics */
    vt_metric_free(VTThrdv[tid]->metv, tid);
    VTThrdv[tid]->metv = NULL;
  }
#endif /* VT_METR */

  if (tid == 0)
  {
    /* register termination of main() thread (*tid==0) */
    masterThreadTerminated = 1;

    /* TODO: It's unclear whether/where the lists of idle thread-IDs should
             be freed. Maybe at this point? */
  }
  else if (vt_is_alive && reuseThreadIds)
  {
    /* store thread-ID for reuse */
    pthread_mutex_lock(&threadReuseMutex);
    idle_tid_list_push_back(VTThrdv[tid]->parent_tid, tid);
    pthread_mutex_unlock(&threadReuseMutex);

    /* store last metric values */
#if defined(VT_METR)
    if (vt_metric_num() > 0 && VTThrdv[tid]->offv && VTThrdv[tid]->valv)
      memcpy(VTThrdv[tid]->offv, VTThrdv[tid]->valv,
             vt_metric_num() * sizeof(uint64_t));
#endif /* VT_METR */

    /* only disable plugin counters, so they can be reused */
#if defined(VT_PLUGIN_CNTR)
    /* if we really use plugins and this thread also uses some */
    if (vt_plugin_cntr_used && VTThrdv[tid]->plugin_cntr_defines)
      vt_plugin_cntr_thread_disable_counters(VTThrdv[tid]);
#endif /* VT_PLUGIN_CNTR */
  }

  /* free thread-specific data */
  free(data);
}

void VTThrd_initPthread()
{
  static uint8_t initflag = 1;

  if (initflag)
  {
    uint32_t* master_tid;
    initflag = 0;

    /* reuse thread IDs of terminated threads? */
    if ((reuseThreadIds = (uint8_t)vt_env_pthread_reuse()))
    {
      /* create lists for idle thread-IDs */
      idleThreadIds = (IdleThreadIdListT*)calloc(VTThrdMaxNum,
                        sizeof(IdleThreadIdListT));
      if (idleThreadIds == NULL)
        vt_error();
    }

    /* create thread-specific data key for all threads */
    if (pthread_key_create(&pthreadKey, pthread_key_destructor) != 0)
      vt_error();

    /* create ID for master thread (=0) */
    master_tid = (uint32_t*)calloc(1, sizeof(uint32_t));
    if (master_tid == NULL)
      vt_error();

    /* put master thread-ID to thread-specific data */
    if (pthread_setspecific(pthreadKey, master_tid) != 0)
      vt_error();

#if defined(VT_METR)
    if (vt_metric_num() > 0)
      vt_metric_thread_init((long (*)(void))(pthread_self));
#endif /* VT_METR */
  }
}

void VTThrd_registerThread(uint32_t ptid)
{
  uint32_t *tid;
  uint8_t tid_reuse = 0;

  if (!vt_is_alive) return;

  /* check whether an ID is already created for this thread */
  tid = (uint32_t*)pthread_getspecific(pthreadKey);
  if (tid == NULL)
  {
    tid = (uint32_t*)malloc(sizeof(uint32_t));
    if (tid == NULL) vt_error();

    /* try to get idle thread-ID for reuse, if desired */
    if (reuseThreadIds)
    {
      pthread_mutex_lock(&threadReuseMutex);
      if (idle_tid_list_size(ptid) > 0)
      {
        *tid = idle_tid_list_pop_front(ptid);
        tid_reuse = 1;
      }
      pthread_mutex_unlock(&threadReuseMutex);
    }

    /* create new thread-ID, if not reusing */
    if (!tid_reuse)
      *tid = VTThrd_create(NULL, ptid, 0);

    /* put (new) thread-ID to thread-specific data */
    pthread_setspecific(pthreadKey, tid);

    /* open thread associated trace file, if new thread object was created */
    if (!tid_reuse )
    {
      VTThrd_open(*tid);
    }
    /* otherwise, re-create metrics for reused thread object */
    else
    {
#if defined(VT_METR)
      if (vt_metric_num() > 0 && !VTThrdv[*tid]->metv)
        VTThrdv[*tid]->metv = vt_metric_create();
#endif /* VT_METR */

#if defined(VT_PLUGIN_CNTR)
      /* if we really use plugins and this thread also uses some */
      if (vt_plugin_cntr_used && VTThrdv[*tid]->plugin_cntr_defines)
        vt_plugin_cntr_thread_enable_counters(VTThrdv[*tid]);
#endif /* VT_PLUGIN_CNTR */
    }
  }
}

uint8_t VTThrd_isAlive()
{
  uint32_t *tid;

  /* get thread-ID from thread-specific data */
  tid = (uint32_t*)pthread_getspecific(pthreadKey);

  /* by calling pthread_exit() in main() the thread-specific data for
     the master thread can already be destroyed at this point; if this
     happens return 1 (current thread is alive) */

  if (tid || masterThreadTerminated)
    return 1;
  else
    return 0;
}

uint32_t VTThrd_getThreadId()
{
  uint32_t *tid;

  /* get thread-ID from thread-specific data */
  tid = (uint32_t*)pthread_getspecific(pthreadKey);

  /* by calling pthread_exit() in main() the thread-specific data for
     the master thread can already be destroyed at this point; if this
     happens return zero (thread-ID of the master thread) */
  if (tid == NULL && masterThreadTerminated)
    return 0;
  else
    vt_libassert(tid != NULL);

  return *tid;
}

void VTThrd_createMutex(VTThrdMutex** mutex)
{
  pthread_mutex_lock(&mutexInitMutex);
  if (*mutex == NULL)
  {
    if (mutexesNum + 1 >= MAX_MUTEXES)
    {
      vt_error_msg("Number of thread mutexes exceeds maximum of %d",
                   MAX_MUTEXES);
    }
    else
    {
      *mutex = &(mutexes[mutexesNum++]);

      pthread_mutex_init(&((*mutex)->m), NULL);
    }
  }
  pthread_mutex_unlock(&mutexInitMutex);
}

void VTThrd_deleteMutex(VTThrdMutex** mutex)
{
  if (*mutex == NULL) return;

  pthread_mutex_lock(&mutexInitMutex);
  if (*mutex != NULL )
  {
    pthread_mutex_destroy(&((*mutex)->m));
    *mutex = NULL;
  }
  pthread_mutex_unlock(&mutexInitMutex);
}

void VTThrd_lock(VTThrdMutex** mutex)
{
  if (*mutex == NULL)
    VTThrd_createMutex(mutex);

  pthread_mutex_lock(&((*mutex)->m));
}

void VTThrd_unlock(VTThrdMutex** mutex)
{
  vt_libassert(*mutex != NULL);

  pthread_mutex_unlock(&((*mutex)->m));
}
