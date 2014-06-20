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
#include "vt_defs.h"        /* VampirTrace constants */
#include "vt_env.h"         /* get environment variables */
#include "vt_pform.h"       /* VampirTrace time measurement */
#include "vt_trc.h"         /* VampirTrace events */
#include "vt_mallocwrap.h"  /* wrapping of malloc and free */
#include "vt_cupti_events.h"
#include "vt_gpu.h"

#include <string.h>

#define PRINT_CUPTI_ERROR(err, _msg){                    \
    const char *errstr;                                  \
    cuptiGetResultString(err, &errstr);                  \
    vt_warning("[CUPTI Events] %s:%d:%s:'%s'",           \
                 __FILE__, __LINE__, _msg, errstr);      \
  }

/* some of CUPTI API functions have changed */
#if (defined(CUPTI_API_VERSION) && (CUPTI_API_VERSION >= 2))

# define VTCUPTIEVENTGETATTRIBUTE(_cuDev, _cuptiEvtID, _cuptiAttr, _valueSize, \
                                 _value) \
  VT_CUPTI_CALL(\
    cuptiEventGetAttribute(_cuptiEvtID, _cuptiAttr, _valueSize, _value), \
    "cuptiEventGetAttribute")

# define VTCUPTIEVENTDOMAINGETNUMEVENTS(_cuDev, _cuptiDomain, _numEvts) \
  VT_CUPTI_CALL(\
    cuptiEventDomainGetNumEvents(_cuptiDomain, _numEvts), \
    "cuptiEventDomainGetNumEvents")

# define VTCUPTIEVENTDOMAINENUMEVENTS(_cuDev, _cuptiDomain, _valueSize, _value)\
  VT_CUPTI_CALL(\
    cuptiEventDomainEnumEvents(_cuptiDomain, _valueSize, _value), \
    "cuptiEventDomainEnumEvents")

#else

# define VTCUPTIEVENTGETATTRIBUTE(_cuDev, _cuptiEvtID, _cuptiAttr, _valueSize, \
                                 _value) \
  VT_CUPTI_CALL(\
    cuptiEventGetAttribute(_cuDev, _cuptiEvtID, _cuptiAttr, _valueSize,_value),\
    "cuptiEventGetAttribute")

# define VTCUPTIEVENTDOMAINGETNUMEVENTS(_cuDev, _cuptiDomain, _numEvts) \
  VT_CUPTI_CALL(\
    cuptiEventDomainGetNumEvents(_cuDev, _cuptiDomain, _numEvts), \
    "cuptiEventDomainGetNumEvents")

# define VTCUPTIEVENTDOMAINENUMEVENTS(_cuDev, _cuptiDomain, _valueSize, _value)\
  VT_CUPTI_CALL(\
    cuptiEventDomainEnumEvents(_cuDev, _cuptiDomain, _valueSize, _value), \
    "cuptiEventDomainEnumEvents")

#endif

uint8_t vt_cupti_events_enabled = 0;
uint8_t vt_cupti_events_sampling = 0;

static uint32_t vt_cuptievt_rid_init;
static uint8_t vt_cuptievt_initialized = 0;
static uint8_t vt_cuptievt_finalized = 0;

/* VampirTrace counter group ID */
static uint32_t vt_cuptievt_cgid;

static vt_cupti_device_t *vtcuptievtCapList = NULL;

/***** --- Declaration of internally used functions --- *****/

/*
 * Enables the recording of CUPTI counters. Either thread if or pointer to the
 * host thread structure has to be given.
 * 
 * @param vtcuptiCtx pointer to the VampirTrace CUPTI context
 */
static void vt_cuptievt_start(vt_cupti_events_t *vtcuptiEvtCtx);

/*
 * Disables recording of CUPTI counters.
 * 
 * @param vtcuptiCtx pointer to the VampirTrace CUPTI context
 */
static void vt_cuptievt_stop(vt_cupti_events_t *vtcuptiEvtCtx);

/*
 * Get to current VampirTrace CUPTI context or create a new one, if CUDA context
 * is not registered yet.
 *
 * @param cuCtx the CUDA context to lookup the VampirTrace CUPTI context
 * @param ptid the VampirTrace thread id of current running thread
 *
 * @return the corresponding VampirTrace host thread structure.
 */
static vt_cupti_ctx_t* vt_cuptievt_getOrCreateCtx(CUcontext cuCtx, uint32_t ptid);

/*
 * Free the memory allocated for the given VampirTrace CUPTI events context.
 * 
 * @param vtcuptiEvtCtx pointer to the VampirTrace CUPTI events context
 */
static void vt_cuptievt_freeEventCtx(vt_cupti_events_t *vtcuptiEvtCtx);

/*
 * Create a VampirTrace CUPTI event group.
 * 
 * @param vtcuptiCtx pointer to the VampirTrace CUPTI context
 * 
 * @return the created VampirTrace CUPTI event group
 */
static vt_cupti_evtgrp_t* vt_cuptievt_createEvtGrp(vt_cupti_ctx_t *vtcuptiCtx);

/*
 * Setup a list of devices with different device capabilities and add the 
 * metrics, which are specified by the user.
 * 
 * @return a list of CUDA devices with different device capabilities
 */
static vt_cupti_device_t* vt_cuptievt_setupMetricList(void);

/*
 * Parse the environment variable for CUPTI metrics (including CUDA device
 * capabilities) and fill the capability metric list.
 *
 * @param capList points to the first element of the capability metric list
 */
static void vt_cupti_fillMetricList(vt_cupti_device_t *capList);

/*
 * Check whether the CUDA device capability is already listed.
 *
 * @param capList IN: list containing the CUDA device capabilities
 * @param major the major CUDA device capability
 * @param minor the minor CUDA device capability
 *
 * @return pointer to the list entry (NULL if not found)
 */
static vt_cupti_device_t* vt_cupti_checkMetricList(vt_cupti_device_t *capList,
                                                   int major, int minor);

/*
 * Print all available counters to stdout.
 *
 * @param capList list of CUDA devices with different capabilities
 */
static void vt_cupti_showAllCounters(vt_cupti_device_t *capList);

/*
 * Print all events for a given CUDA device and CUPTI event domain with name 
 * and ID.
 * 
 * @param cuDev the CUDA device
 * @param domainId the CUPTI event domain ID
 */
static void vt_cuptievt_enumEvents(CUdevice cuDev, CUpti_EventDomainID domainId);
/* ------ */

/* ----------------------- internally used functions ----------------------- */

static vt_cupti_evtgrp_t* vt_cuptievt_createEvtGrp(vt_cupti_ctx_t *vtcuptiCtx)
{
  CUptiResult cuptiErr = CUPTI_SUCCESS;
  vt_cupti_evtgrp_t *vtcuptiGrp = NULL;

  vtcuptiGrp = (vt_cupti_evtgrp_t*)malloc(sizeof(vt_cupti_evtgrp_t));
  vtcuptiGrp->evtNum = 0;
  vtcuptiGrp->enabled = 0;
  vtcuptiGrp->next = NULL;

  /* create initial CUPTI counter group */
  cuptiErr = cuptiEventGroupCreate(vtcuptiCtx->cuCtx, &(vtcuptiGrp->evtGrp), 0);
  VT_CUPTI_CALL(cuptiErr, "cuptiEventGroupCreate");

  {
    size_t evtNum = vtcuptiCtx->events->vtDevCap->evtNum;
    
    vtcuptiGrp->cuptiEvtIDs = 
            (CUpti_EventID *)malloc(evtNum*sizeof(CUpti_EventID));
    
    vtcuptiGrp->vtCIDs = 
            (uint32_t *)malloc(evtNum*sizeof(uint32_t));
  }

  return vtcuptiGrp;
}

static void vt_cupti_addEvtGrpsToCtx(vt_cupti_ctx_t *vtcuptiCtx)
{
    CUptiResult cuptiErr = CUPTI_SUCCESS;
    vt_cupti_evtgrp_t *vtcuptiGrp = vt_cuptievt_createEvtGrp(vtcuptiCtx);
    vt_cupti_events_t *vtcuptiEvents = vtcuptiCtx->events;
    vt_cupti_evtctr_t *vtcuptiEvt = vtcuptiEvents->vtDevCap->vtcuptiEvtList;

    /* try to add all events for current context/device */
    while(vtcuptiEvt != NULL && vtcuptiGrp->evtNum < vtcuptiEvents->vtDevCap->evtNum){
      cuptiErr = cuptiEventGroupAddEvent(vtcuptiGrp->evtGrp,
                                         vtcuptiEvt->cuptiEvtID);

      /* everything is fine */
      if(cuptiErr == CUPTI_SUCCESS){
        vtcuptiGrp->cuptiEvtIDs[vtcuptiGrp->evtNum] = vtcuptiEvt->cuptiEvtID;
        vtcuptiGrp->vtCIDs[vtcuptiGrp->evtNum] = vtcuptiEvt->vtCID;
        vtcuptiGrp->evtNum++;
      }else{
        /* we can at least try to put the event in another group */

        /* too many events in this group or
           event is in different domain or device limitation*/
        if(cuptiErr == CUPTI_ERROR_MAX_LIMIT_REACHED ||
           cuptiErr == CUPTI_ERROR_NOT_COMPATIBLE){

          vt_cntl_msg(2, "[CUPTI Events] Create new event group for event %d",
                         vtcuptiEvt->cuptiEvtID);

          /* prepend last group to list, if it is not empty */
          if(vtcuptiGrp->evtNum > 0){
            vtcuptiGrp->next = vtcuptiEvents->vtGrpList;
            vtcuptiEvents->vtGrpList = vtcuptiGrp;
          }

          /* create new VampirTrace CUPTI event group */
          vtcuptiGrp = vt_cuptievt_createEvtGrp(vtcuptiCtx);

          /* try to add the same event to the just created group */
          continue;
        }

        PRINT_CUPTI_ERROR(cuptiErr, "cuptiEventGroupAddEvent");
      }

      vtcuptiEvt = vtcuptiEvt->next;
    }

    /* prepend last group to list, if it is not empty */
    if(vtcuptiGrp->evtNum > 0){
      vtcuptiGrp->next = vtcuptiEvents->vtGrpList;
      vtcuptiEvents->vtGrpList = vtcuptiGrp;
    }
}

/*
 * Initialize the CUPTI events data of the given VampirTrace CUPTI context.
 * 
 * @param vtCtx pointer to the VampirTrace CUPTI context
 */
void vt_cupti_events_initContext(vt_cupti_ctx_t *vtcuptiCtx)
{
  vt_cupti_events_t *vtcuptiEvtCtx = NULL;
  
  vt_cntl_msg(2, "[CUPTI Events] Initializing VampirTrace CUPTI events context");

  /* get a pointer to eventIDArray */
  {
    CUresult cuErr = CUDA_SUCCESS;
    int dev_major, dev_minor;
    vt_cupti_device_t *cuptiDev;

    /* TODO: do not trace this driver API function call */
    cuErr = cuDeviceComputeCapability(&dev_major, &dev_minor, vtcuptiCtx->cuDev);
    VT_CUDRV_CALL(cuErr, "cuDeviceComputeCapability");

    /* check if device capability already listed */
    VT_CUPTI_LOCK();
      cuptiDev = vtcuptievtCapList;
    VT_CUPTI_UNLOCK();
    
    cuptiDev = vt_cupti_checkMetricList(cuptiDev, dev_major, dev_minor);
    if(cuptiDev){
      /* allocate the VampirTrace CUPTI events context */
      vtcuptiEvtCtx = (vt_cupti_events_t *)malloc(sizeof(vt_cupti_events_t));
      if(vtcuptiEvtCtx == NULL)
        vt_error_msg("[CUPTI Events] malloc(sizeof(vt_cupti_events_t)) failed!");
      
      vtcuptiEvtCtx->vtDevCap = cuptiDev;
      vtcuptiEvtCtx->vtGrpList = NULL;
      vtcuptiEvtCtx->counterData = NULL;
      vtcuptiEvtCtx->cuptiEvtIDs = NULL;
      
      vtcuptiCtx->events = vtcuptiEvtCtx;
    }else{
      return;
    }
  }

  /* create and add the VampirTrace CUPTI groups to the context */
  vt_cupti_addEvtGrpsToCtx(vtcuptiCtx);

  /* allocate memory for CUPTI counter reads */
  {
    size_t allocSize = vtcuptiEvtCtx->vtGrpList->evtNum;
    
    vtcuptiEvtCtx->counterData = 
            (uint64_t *)malloc(allocSize*sizeof(uint64_t));
    vtcuptiEvtCtx->cuptiEvtIDs = 
            (CUpti_EventID *)malloc(allocSize*sizeof(CUpti_EventID));
  }
  
  vt_cuptievt_start(vtcuptiEvtCtx);
}

static void vt_cuptievt_freeEventCtx(vt_cupti_events_t *vtcuptiEvtCtx)
{
  vt_cupti_evtgrp_t *vtcuptiGrp = NULL;
  
  if(vtcuptiEvtCtx == NULL)
    return;
  
  vtcuptiGrp = vtcuptiEvtCtx->vtGrpList;

  while(vtcuptiGrp != NULL){
    free(vtcuptiGrp->cuptiEvtIDs);
    free(vtcuptiGrp->vtCIDs);
    
    vtcuptiGrp = vtcuptiGrp->next;
  }

  /* free memory for CUPTI counter reads */
  free(vtcuptiEvtCtx->counterData);
  free(vtcuptiEvtCtx->cuptiEvtIDs);
}

/*
 * Retrieve the VampirTrace CUPTI context from the CUDA context.
 * 
 * @param cuCtx the CUDA context
 * @param ptid the active VampirTrace thread id
 * 
 * @return VampirTrace CUPTI context
 */
static vt_cupti_ctx_t* vt_cuptievt_getOrCreateCtx(CUcontext cuCtx, uint32_t ptid)
{
  vt_cupti_ctx_t *vtcuptiCtx = NULL;
  
  uint64_t time;

  /* check, if the current VampirTrace thread is enabled for GPU counters */
  if((vt_gpu_prop[ptid] & VTGPU_NO_PC) == VTGPU_NO_PC)
    return NULL;
  
  time = vt_pform_wtime();
  vt_enter(ptid, &time, vt_cuptievt_rid_init);
  
  /* retrieve a global VampirTrace CUPTI context */
  vtcuptiCtx = vt_cupti_getCreateCtx(cuCtx);
  
  /* if the event context is not available yet, then create it */
  if(NULL == vtcuptiCtx->events){
    vt_cupti_events_initContext(vtcuptiCtx);
  }
  
  time = vt_pform_wtime();
  vt_exit(ptid, &time);
  
  return vtcuptiCtx;
}

/*
 * Parse the environment variable for CUPTI metrics (including CUDA device
 * capabilities) and fill the capability metric list.
 *
 * @param capList points to the first element of the capability metric list
 */
static void vt_cupti_fillMetricList(vt_cupti_device_t *capList)
{
  char *metricString = vt_env_cupti_events();
  char *metric_sep = vt_env_metrics_sep();
  char *metric, *metric_cap;

  metric = strtok(metricString, metric_sep);

  while (metric != NULL){
    CUptiResult cuptiErr = CUPTI_SUCCESS;
    vt_cupti_device_t *cuptiDev = NULL;
    vt_cupti_evtctr_t *vtcuptiEvt = NULL;
    int metr_major = 0;
    int metr_minor = 0;

    /* try to get CUDA device capability parsed from metric */
    metr_major = atoi(metric);
    metric_cap = strchr(metric+1, '.');
    if(metric_cap){
      metr_minor = atoi(metric_cap+1);
      metric_cap = strchr(metric_cap+1, '_');
    }
    
    /* check whether device capability is given or not */
    if(metric_cap){
      metric = metric_cap + 1;

      vt_cntl_msg(2, "Metric '%s', %d.%d", metric, metr_major, metr_minor);

      cuptiDev = vt_cupti_checkMetricList(capList, metr_major, metr_minor);
      if(cuptiDev == NULL){
        metric = strtok(NULL, metric_sep);
        continue;
      }
      
      vtcuptiEvt = (vt_cupti_evtctr_t*)malloc(sizeof(vt_cupti_evtctr_t));
      cuptiErr = cuptiEventGetIdFromName(cuptiDev->cuDev, metric,
                                         &vtcuptiEvt->cuptiEvtID);
      if(cuptiErr != CUPTI_SUCCESS){
        if(!strncmp(metric, "help", 4)) vt_cupti_showAllCounters(capList);
        vt_warning("[CUPTI Events] Skipping invalid event '%s' for device %d",
                   metric, cuptiDev->cuDev);
        metric = strtok(NULL, metric_sep);
        continue;
      }

      /* create VampirTrace counter ID */
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_LOCK_IDS();
#endif
      vtcuptiEvt->vtCID = vt_def_counter(VT_MASTER_THREAD, metric, "#",
            VT_CNTR_ABS | VT_CNTR_LAST | VT_CNTR_UNSIGNED, vt_cuptievt_cgid, 0);
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_UNLOCK_IDS();
#endif

      cuptiDev->evtNum++;
      vtcuptiEvt->next = cuptiDev->vtcuptiEvtList;
      cuptiDev->vtcuptiEvtList = vtcuptiEvt;
    }else{ 
      /* device capability is not given. Try to add metric to all devices */
      uint32_t cid_metric = VT_NO_ID;

      cuptiDev = capList;
      while(cuptiDev != NULL){
        vtcuptiEvt = (vt_cupti_evtctr_t*)malloc(sizeof(vt_cupti_evtctr_t));
        cuptiErr = cuptiEventGetIdFromName(cuptiDev->cuDev, metric,
                                           &vtcuptiEvt->cuptiEvtID);

        if(cuptiErr != CUPTI_SUCCESS){
          if(!strncmp(metric, "help", 4)) vt_cupti_showAllCounters(capList);
          vt_warning("[CUPTI Events] Skipping invalid event '%s' for device %d",
                     metric, cuptiDev->cuDev);
        }else{
          /* create VampirTrace counter ID, if not yet done for other device */
          if(cid_metric == VT_NO_ID){
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_LOCK_IDS();
#endif
      cid_metric = vt_def_counter(VT_MASTER_THREAD, metric, "#", 
            VT_CNTR_ABS | VT_CNTR_LAST | VT_CNTR_UNSIGNED, vt_cuptievt_cgid, 0);
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_UNLOCK_IDS();
#endif
          }
          
          cuptiDev->evtNum++;
          vtcuptiEvt->vtCID = cid_metric;
          vtcuptiEvt->next = cuptiDev->vtcuptiEvtList;
          cuptiDev->vtcuptiEvtList = vtcuptiEvt;
        }

        cuptiDev = cuptiDev->next;
      }
    }

    metric = strtok(NULL, metric_sep);
  }
}

/*
 * Check whether the CUDA device capability is already listed.
 *
 * @param capList IN: list containing the CUDA device capabilities
 * @param major the major CUDA device capability
 * @param minor the minor CUDA device capability
 *
 * @return pointer to the list entry (NULL if not found)
 */
static vt_cupti_device_t* vt_cupti_checkMetricList(vt_cupti_device_t *capList,
                                                int major, int minor)
{
  vt_cupti_device_t *cuptiDev;

  /* check if device capability is already listed and return it if found */
  cuptiDev = capList;
  while(cuptiDev != NULL){
    if(cuptiDev->dev_major == major && cuptiDev->dev_minor == minor){
      return cuptiDev;
    }
    cuptiDev = cuptiDev->next;
  }

  return NULL;
}

/*
 * Setup a list of devices with different device capabilities and add the 
 * metrics, which are specified by the user.
 * 
 * @return a list of CUDA devices with different device capabilities
 */
static vt_cupti_device_t* vt_cuptievt_setupMetricList(void)
{
  CUresult err;
  int deviceCount, id;
  vt_cupti_device_t *capList = NULL;
  
  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);
  
  /* CUDA initialization */
	VT_CUDRV_CALL(cuInit(0), "cuInit");
  
  /* How many GPGPU devices do we have? */
	err = cuDeviceGetCount( &deviceCount );
	VT_CUDRV_CALL(err, "cuDeviceGetCount");
	if(deviceCount == 0){
		vt_error_msg("[CUPTI Events] There is no device supporting CUDA available.");
	}

  /* create list with available compute capabilities */
  for(id = 0; id < deviceCount; id++){
    CUdevice cuDev;
    vt_cupti_device_t *cuptiDev;
    int dev_major, dev_minor;

    err = cuDeviceGet(&cuDev, id);
		VT_CUDRV_CALL(err, "cuDeviceGet");

    err = cuDeviceComputeCapability(&dev_major, &dev_minor, cuDev);
    VT_CUDRV_CALL(err, "cuDeviceComputeCapability");

    /* check if device capability already listed */
    cuptiDev = vt_cupti_checkMetricList(capList, dev_major, dev_minor);

    if(cuptiDev == NULL){
      /* allocate memory for device list entry */
      cuptiDev = (vt_cupti_device_t *)malloc(sizeof(vt_cupti_device_t));
      cuptiDev->dev_major = dev_major;
      cuptiDev->dev_minor = dev_minor;
      cuptiDev->cuDev = cuDev;
      cuptiDev->vtcuptiEvtList = NULL;
      cuptiDev->evtNum = 0;
      cuptiDev->next = NULL;

      /* prepend to list */
      cuptiDev->next = capList;
      capList = cuptiDev;
    }
  }

  vt_cupti_fillMetricList(capList);

  /* cleanup list: remove entries, which don't have metrics */
  {
    vt_cupti_device_t *curr = capList;
    vt_cupti_device_t *last = capList;

    while(curr != NULL){
      vt_cupti_device_t *freeDev = curr;
      curr = curr->next;

      if(freeDev->evtNum == 0){
        /* first element */
        if(freeDev == capList){
          capList = capList->next;
        }else{
          last->next = freeDev->next;
        }
        free(freeDev);
      }else last = freeDev;
    }
  }
  
  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);

  return capList;
}

/*
 * Print all events for a given CUDA device and CUPTI event domain with name 
 * and ID.
 * 
 * @param cuDev the CUDA device
 * @param domainId the CUPTI event domain ID
 */
static void vt_cuptievt_enumEvents(CUdevice cuDev, CUpti_EventDomainID domainId)
{
  CUpti_EventID *eventId = NULL;
  uint32_t maxEvents = 0;
  uint32_t i = 0;
  size_t size = 0;
  uint8_t desc_on = 0;
  char *help = vt_env_cupti_events();
  
  if(!strncmp(&help[4], "_l", 2)) desc_on = 1;
  
  /*vt_cntl_msg(1, "############ %s", &help[5]);*/

  /* query num of events available in the domain */
  VTCUPTIEVENTDOMAINGETNUMEVENTS(cuDev,
                                 (CUpti_EventDomainID)domainId,
                                 &maxEvents);

  size = sizeof(CUpti_EventID) * maxEvents;
  eventId = (CUpti_EventID*)malloc(size);
  if(eventId == NULL) 
    vt_error_msg("[CUPTI Events] Failed to allocate memory for event ID");
  
  memset(eventId, 0, size);

  VTCUPTIEVENTDOMAINENUMEVENTS(cuDev,
                               (CUpti_EventDomainID)domainId,
                               &size,
                               eventId);

  /* query event info */
  {
    size_t NAME_SHORT = 32;
    size_t DESC_SHORT = 2048;
    char *eventname = (char*)malloc(NAME_SHORT*sizeof(char)); /* event name */
    char *shortdesc = NULL; /* short desc of the event */
    
    if(desc_on) shortdesc = malloc(DESC_SHORT*sizeof(char));
    
    for(i = 0; i < maxEvents; i++){
      NAME_SHORT = 32;
      DESC_SHORT = 2048;
      VTCUPTIEVENTGETATTRIBUTE(cuDev,
                               eventId[i],
                               CUPTI_EVENT_ATTR_NAME,
                               &NAME_SHORT,
                               eventname);

      if(desc_on){
        VTCUPTIEVENTGETATTRIBUTE(cuDev,
                                 eventId[i],
                                 CUPTI_EVENT_ATTR_LONG_DESCRIPTION,
                                 &DESC_SHORT,
                                 (uint8_t*)shortdesc);
      }

      vt_cntl_msg(1, "%d:%s", eventId[i], eventname);
      if(desc_on) vt_cntl_msg(1, "%s\n", shortdesc);
    }

    free(eventname);
    if(desc_on) free(shortdesc);
  }

  free(eventId);
}

/*
 * Print all available counters to stdout.
 *
 * @param capList list of CUDA devices with different capabilities
 */
static void vt_cupti_showAllCounters(vt_cupti_device_t *capList)
{
  CUptiResult cuptiErr = CUPTI_SUCCESS;
  CUpti_EventDomainID *domainId = NULL;
  uint32_t maxDomains = 0;
  uint32_t i;
  size_t size = 0;
  
  while(capList != NULL){
    CUdevice cuDev = capList->cuDev;
    vt_cntl_msg(1, "[CUPTI Events] Available events for device %d (SM %d.%d):", 
                   cuDev, capList->dev_major, capList->dev_minor);
    vt_cntl_msg(1, "Id:Name");
    vt_cntl_msg(1, "Description\n"
         "-------------------------------------------------------------------");
    
    cuptiErr = cuptiDeviceGetNumEventDomains(cuDev, &maxDomains);
    VT_CUPTI_CALL(cuptiErr, "cuptiDeviceGetNumEventDomains");

    if(maxDomains == 0){
      vt_warning("[CUPTI Events] No domain is exposed by dev = %d\n", cuDev);
      return;
    }

    size = sizeof(CUpti_EventDomainID) * maxDomains;
    domainId = (CUpti_EventDomainID*)malloc(size);
    if(domainId == NULL){
      vt_warning("[CUPTI Events] Failed to allocate memory to domain ID");
      return;
    }
    memset(domainId, 0, size);

    cuptiErr = cuptiDeviceEnumEventDomains(cuDev, &size, domainId);
    VT_CUPTI_CALL(cuptiErr, "cuptiDeviceEnumEventDomains");

    /* enum domains */
    for(i = 0; i < maxDomains; i++) vt_cuptievt_enumEvents(cuDev, domainId[i]);

    vt_cntl_msg(1, "------------------------------------------------------");
    
    free(domainId);
    
    capList = capList->next;
  }
  
  /* as this function is in the call-path of the initialize functions
   * -> vt_cupti_setupMetrics 
   * -> vt_cupti_fillMetricList 
   * -> vt_cupti_showAllCounters
   */
  vt_cuptievt_initialized = 1;
  VT_CUPTI_UNLOCK();
  exit(0);
}


static void vt_cuptievt_start(vt_cupti_events_t *vtcuptiEvtCtx)
{
  CUptiResult cuptiErr = CUPTI_SUCCESS;
  vt_cupti_evtgrp_t *vtcuptiGrp = NULL;
  vt_cupti_evtgrp_t *lastGrp = NULL;

  /* start gathering counter values, if context was successfully initialized */
  if(NULL == vtcuptiEvtCtx){
    /* no performance counters for this thread available */
    VT_CHECK_THREAD;
    vt_gpu_prop[VT_MY_THREAD] |= VTGPU_NO_PC;
    vt_cntl_msg(2, "[CUPTI Events] Context not initialized!");
    return;
  }

  /* start all groups */
  vtcuptiGrp = vtcuptiEvtCtx->vtGrpList;
  lastGrp = vtcuptiEvtCtx->vtGrpList;
  while(vtcuptiGrp != NULL){
    cuptiErr = cuptiEventGroupEnable(vtcuptiGrp->evtGrp);
    
    /* if the event group could not be enabled, remove it */
    if(cuptiErr != CUPTI_SUCCESS){
      size_t i;
      vt_cupti_evtgrp_t *freeGrp = vtcuptiGrp;
      size_t valueSize = 32;
      char name[32];

      vtcuptiGrp = vtcuptiGrp->next;

      /* give user information about the group, which cannot be enabled */
      for(i = 0; i < freeGrp->evtNum; i++){
        VTCUPTIEVENTGETATTRIBUTE(vtcuptiEvtCtx->vtDevCap->cuDev,
                                 *(freeGrp->cuptiEvtIDs)+i,
                                 CUPTI_EVENT_ATTR_NAME,
                                 &valueSize, (char*)name);
        vt_warning("[CUPTI Events] Event '%s' (%d) cannot be enabled",
                   name, *(freeGrp->cuptiEvtIDs)+i);
      }

      /* group is first element in linked list */
      if(vtcuptiEvtCtx->vtGrpList == freeGrp){
        vtcuptiEvtCtx->vtGrpList = vtcuptiEvtCtx->vtGrpList->next;
      }else{/* has to be at least the second group in linked list */
        lastGrp->next = freeGrp->next;
      }

      free(freeGrp);
      freeGrp = NULL;
    }else{
      vtcuptiGrp->enabled = 1;
      lastGrp= vtcuptiGrp;
      vtcuptiGrp = vtcuptiGrp->next;
    }
  }
  
}

/*
 * Stop CUPTI counter capturing by disabling the CUPTI event groups.
 * 
 * @param vtcuptiEvtCtx pointer to the VampirTrace CUPTI events context
 */
static void vt_cuptievt_stop(vt_cupti_events_t *vtcuptiEvtCtx)
{
  vt_cupti_evtgrp_t *vtcuptiGrp = NULL;

  if(vtcuptiEvtCtx == NULL || vt_gpu_debug) 
    return;

  /* stop counter reading for all groups */
  vtcuptiGrp = vtcuptiEvtCtx->vtGrpList;
  while(vtcuptiGrp != NULL){
    if(vtcuptiGrp->enabled){
      CUptiResult cuptiErr = CUPTI_SUCCESS;
      
      cuptiErr = cuptiEventGroupDisable(vtcuptiGrp->evtGrp);
      VT_CUPTI_CALL(cuptiErr, "cuptiEventGroupDisable");

      vtcuptiGrp->enabled = 0;
    }

    vtcuptiGrp = vtcuptiGrp->next;
  }
}

/* -------------START: Implementation of public functions ------------------ */
/* ------------------------------------------------------------------------- */

/*
 * Initialize VampirTrace IDs and registers the finalize function.
 * This may be done implicitly by vt_cupti_count().
 */
void vt_cupti_events_init()
{
  if(!vt_cuptievt_initialized){ /* fast check without lock */
    vt_cupti_init();
    VT_CUPTI_LOCK();
    if(!vt_cuptievt_initialized){
      vt_cntl_msg(2, "[CUPTI Events] Initializing ... ");

      /* create VampirTrace counter group ID only once */
  #if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_LOCK_IDS();
  #endif
      vt_cuptievt_rid_init = vt_def_region(VT_MASTER_THREAD, "vtcuptiHostThreadInit",
                      VT_NO_ID, VT_NO_LNO, VT_NO_LNO, "VT_CUPTI", VT_FUNCTION);

      vt_cuptievt_cgid = vt_def_counter_group(VT_MASTER_THREAD, "CUPTI");
  #if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_UNLOCK_IDS();
  #endif

      vt_cupti_events_sampling = (uint8_t)vt_env_cupti_sampling();

      vtcuptievtCapList = vt_cuptievt_setupMetricList();

      if(NULL == vtcuptievtCapList){
        vt_cupti_events_enabled = 0;
      }else{
        /* register the finalize function of VampirTrace CUPTI to be called before
         * the program exits */
        atexit(vt_cupti_events_finalize);
      }

      vt_cuptievt_initialized = 1;
      VT_CUPTI_UNLOCK();
    }
  }
}

/*
 * Finalizes the VampirTrace CUPTI events interface.
 */
void vt_cupti_events_finalize()
{
  if(!vt_cuptievt_finalized && vt_cuptievt_initialized){ /* fast check without lock */
    VT_CUPTI_LOCK();
    if(!vt_cuptievt_finalized && vt_cuptievt_initialized){
      vt_cupti_ctx_t *vtcuptiCtxList =  vt_cupti_ctxList;
      
      /* needed because of the atexit in vt_cupti_events_init() */
      VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

      vt_cntl_msg(2, "[CUPTI Events] Finalizing ...");

      /* free VampirTrace CUPTI events context structures */
      while(vtcuptiCtxList != NULL){
        if(vtcuptiCtxList->events != NULL){
          vt_cupti_events_finalizeContext(vtcuptiCtxList);
          free(vtcuptiCtxList->events);
          vtcuptiCtxList->events = NULL;
        }

        vtcuptiCtxList = vtcuptiCtxList->next;
      }

      /* free capability metric list */
      while(vtcuptievtCapList != NULL){
        vt_cupti_device_t *tmp = vtcuptievtCapList;
        vtcuptievtCapList = vtcuptievtCapList->next;
        
        /* free VampirTrace CUPTI events */
        while(tmp->vtcuptiEvtList != NULL){
          vt_cupti_evtctr_t *tmpEvt = tmp->vtcuptiEvtList;
          tmp->vtcuptiEvtList = tmp->vtcuptiEvtList->next;
          free(tmpEvt);
          tmpEvt = NULL;
        }

        free(tmp);
        tmp = NULL;
      }
      
      VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);

      vt_cuptievt_finalized = 1;
      VT_CUPTI_UNLOCK();
    }
  }
}

void vt_cupti_events_finalizeContext(vt_cupti_ctx_t *vtCtx)
{
  uint64_t time = vt_pform_wtime();
  vt_cupti_strm_t *curStrm = NULL;
  vt_cupti_evtgrp_t *vtcuptiGrp = NULL;
  
  if(vtCtx == NULL || vtCtx->events == NULL)
    return;
  
  /* These CUPTI calls may fail, as CUPTI has implicitly destroyed something */
  if(vt_gpu_debug == 0){
    curStrm = vtCtx->strmList;


    /* for all streams of this context */
    while(curStrm != NULL){

      /* ensure increasing time stamps */
      if(time < curStrm->vtLastTime){
        curStrm = curStrm->next;
        continue;
      }

      vt_cuptievt_resetCounter(vtCtx->events, curStrm->vtThrdID, &time);

      curStrm = curStrm->next;
    }

    /* stop CUPTI counter capturing */
    vt_cuptievt_stop(vtCtx->events);

    /* destroy all CUPTI event groups, which have been created */
    vtcuptiGrp = vtCtx->events->vtGrpList;

    while(vtcuptiGrp != NULL){
      VT_CUPTI_CALL(cuptiEventGroupRemoveAllEvents(vtcuptiGrp->evtGrp), 
                    "cuptiEventGroupRemoveAllEvents");

      VT_CUPTI_CALL(cuptiEventGroupDestroy(vtcuptiGrp->evtGrp), 
                    "cuptiEventGroupDestroy");

      vtcuptiGrp = vtcuptiGrp->next;
    }
  }else{
    /* set at least the VampirTrace counter to zero */
    curStrm = vtCtx->strmList;

    /* for all streams of this context */
    while(curStrm != NULL){

      /* ensure increasing time stamps */
      if(time < curStrm->vtLastTime){
        curStrm = curStrm->next;
        continue;
      }
      
      vtcuptiGrp = vtCtx->events->vtGrpList;

      while(vtcuptiGrp != NULL){
        size_t i;
        
        for(i = 0; i < vtcuptiGrp->evtNum; i++){
          vt_count(curStrm->vtThrdID, &time, *(vtcuptiGrp->vtCIDs+i), 0);
        }

        vtcuptiGrp = vtcuptiGrp->next;
      }
      
      curStrm = curStrm->next;
    }
  }
  
  /* free previously allocated memory */
  vt_cuptievt_freeEventCtx(vtCtx->events);
}

/*
 * Retrieves the VampirTrace CUPTI context for the CUDA context associated with
 * the calling host thread. Initiates context creation, if it is not available 
 * yet.
 *
 * @param ptid the VampirTrace thread id of the calling host thread
 * 
 * @return VampirTrace CUPTI context
 */
vt_cupti_ctx_t* vt_cuptievt_getOrCreateCurrentCtx(uint32_t ptid)
{
  CUcontext cuCtx = NULL;
  
  if(!vt_cuptievt_initialized) vt_cupti_events_init();

# if (defined(CUDA_VERSION) && (CUDA_VERSION < 4000))
  VT_CUDRV_CALL(cuCtxPopCurrent(&cuCtx), "cuCtxPopCurrent");
  VT_CUDRV_CALL(cuCtxPushCurrent(cuCtx), "cuCtxPushCurrent");
# else
  VT_CUDRV_CALL(cuCtxGetCurrent(&cuCtx), "cuCtxGetCurrent");
# endif
  
  if(cuCtx == NULL){
    vt_cntl_msg(2, "[CUPTI Events] No context is bound to the calling CPU thread!");
    return NULL;
  }
  
  return vt_cuptievt_getOrCreateCtx(cuCtx, ptid);
}

/*
 * Request the CUTPI counter values and write it to the given VampirTrace
 * stream with the given timestamps.
 *
 * @param vtcuptiEvtCtx pointer to the VampirTrace CUPTI events context
 * @param strmid the stream id for the counter values
 * @param time the VampirTrace timestamps
 */
void vt_cuptievt_writeCounter(vt_cupti_events_t *vtcuptiEvtCtx, uint32_t strmid,
                              uint64_t *time)
{
  CUptiResult cuptiErr = CUPTI_SUCCESS;
  vt_cupti_evtgrp_t *vtcuptiGrp = NULL;

  size_t bufferSizeBytes;
  size_t arraySizeBytes;
  size_t numCountersRead;

  if(vtcuptiEvtCtx == NULL){
    VT_CHECK_THREAD;
    vtcuptiEvtCtx = vt_cuptievt_getOrCreateCurrentCtx(VT_MY_THREAD)->events;
    if(vtcuptiEvtCtx == NULL) return;
  }

  vtcuptiGrp = vtcuptiEvtCtx->vtGrpList;
  while(vtcuptiGrp != NULL){
    /* read events only, if the event group is enabled */
    if(vtcuptiGrp->enabled){

      bufferSizeBytes = vtcuptiGrp->evtNum * sizeof(uint64_t);
      arraySizeBytes = vtcuptiGrp->evtNum * sizeof(CUpti_EventID);

      /* read events */
      cuptiErr = cuptiEventGroupReadAllEvents(vtcuptiGrp->evtGrp,
                                              CUPTI_EVENT_READ_FLAG_NONE,
                                              &bufferSizeBytes, vtcuptiEvtCtx->counterData,
                                              &arraySizeBytes, vtcuptiEvtCtx->cuptiEvtIDs,
                                              &numCountersRead);
      VT_CUPTI_CALL(cuptiErr, "cuptiEventGroupReadAllEvents");
      
      if(vtcuptiGrp->evtNum != numCountersRead){
        vt_error_msg("[CUPTI Events] %d counter reads, %d metrics specified in "
                     "VT_CUPTI_METRICS!", numCountersRead, vtcuptiGrp->evtNum);
      }

      /* For all events of the event group: map added event IDs to just read event
       * IDs, as the order may not be the same. For small numbers of counter reads
       * this simple mapping should be fast enough.
       */
      {
        size_t j;

        for(j = 0; j < numCountersRead; j++){
          size_t i;
          for(i = 0; i < vtcuptiGrp->evtNum; i++){
            if(vtcuptiEvtCtx->cuptiEvtIDs[j] == *(vtcuptiGrp->cuptiEvtIDs+i)){
              /* write the counter value as VampirTrace counter */
              vt_count(strmid, time, *(vtcuptiGrp->vtCIDs+i), vtcuptiEvtCtx->counterData[i]);
            }
          }
        }
      }

    }

    vtcuptiGrp = vtcuptiGrp->next;
  }
  
}

/*
 * Reset the VampirTrace counter values (to zero) for active CUPTI counters.
 *
 * @param vtcuptiEvtCtx pointer to the VampirTrace CUPTI events context
 * @param strmid the stream id for the counter values
 * @param time the VampirTrace timestamps
 */
void vt_cuptievt_resetCounter(vt_cupti_events_t *vtcuptiEvtCtx, uint32_t strmid,
                              uint64_t *time)
{
  size_t i;
  vt_cupti_evtgrp_t *vtcuptiGrp = NULL;

  /* create a VampirTrace CUPTI events context, if it is not available */
  if(vtcuptiEvtCtx == NULL){
    VT_CHECK_THREAD;
    vtcuptiEvtCtx = vt_cuptievt_getOrCreateCurrentCtx(VT_MY_THREAD)->events;
    if(vtcuptiEvtCtx == NULL) return;
  }

  vtcuptiGrp = vtcuptiEvtCtx->vtGrpList;
  
  while(vtcuptiGrp != NULL){
    for(i = 0; i < vtcuptiGrp->evtNum; i++){
      vt_count(strmid, time, *(vtcuptiGrp->vtCIDs+i), 0);
    }
    
    /* reset counter values of this group */
    VT_CUPTI_CALL(cuptiEventGroupResetAllEvents(vtcuptiGrp->evtGrp),
                      "cuptiEventGroupResetAllEvents");
    
    vtcuptiGrp = vtcuptiGrp->next;
  }
}

/*
 * Finalizes CUPTI device.
 * 
 * @param ptid VampirTrace process/thread id
 * @param cleanExit 1 to cleanup CUPTI event group, otherwise 0
 */
void vt_cuptievt_finalize_device(uint8_t cleanExit){
  CUptiResult cuptiErr = CUPTI_SUCCESS;
  vt_cupti_ctx_t *vtcuptiCtx = NULL;

  vt_cntl_msg(2, "[CUPTI Events] Finalize device ... ");

  {
    CUcontext cuCtx;
    
#if (defined(CUDA_VERSION) && (CUDA_VERSION < 4000))
    VT_CUDRV_CALL(cuCtxPopCurrent(&cuCtx), "cuCtxPopCurrent");
    VT_CUDRV_CALL(cuCtxPushCurrent(cuCtx), "cuCtxPushCurrent");
#else
    VT_CUDRV_CALL(cuCtxGetCurrent(&cuCtx), "cuCtxGetCurrent");
#endif

    vtcuptiCtx = vt_cupti_removeCtx(&cuCtx);
    if(vtcuptiCtx == NULL) 
      return;
  }
  
  if(vtcuptiCtx->events == NULL)
    return;

  if(cleanExit && vt_gpu_debug != 0){
    /*uint64_t time = vt_pform_wtime();

    vt_cupti_resetCounter(vtcuptiCtx, 0, &time);*/

    /* stop CUPTI counter capturing */
    vt_cuptievt_stop(vtcuptiCtx->events);

    /* destroy all CUPTI event groups, which have been created */
    {
      vt_cupti_evtgrp_t *vtcuptiGrp = vtcuptiCtx->events->vtGrpList;

      while(vtcuptiGrp != NULL){
        cuptiErr = cuptiEventGroupRemoveAllEvents(vtcuptiGrp->evtGrp);
        VT_CUPTI_CALL(cuptiErr, "cuptiEventGroupRemoveAllEvents");

        cuptiErr = cuptiEventGroupDestroy(vtcuptiGrp->evtGrp);
        VT_CUPTI_CALL(cuptiErr, "cuptiEventGroupDestroy");

        vtcuptiGrp = vtcuptiGrp->next;
      }
    }
  }

  /* free VampirTrace CUPTI event context */
  vt_cuptievt_freeEventCtx(vtcuptiCtx->events);
}

/* ------------------------------------------------------------------------- */
/* -------------- END: Implementation of public functions ------------------ */
