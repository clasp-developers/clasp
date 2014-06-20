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

#include "stdio.h"

#include "vt_thrd.h"            /* thread creation for GPU kernels */
#include "vt_gpu.h"             /* common for GPU */
#include "vt_mallocwrap.h"      /* wrapping of malloc and free */
#include "vt_pform.h"           /* VampirTrace time measurement */
#include "vt_cupti.h"           /* Support for CUPTI */
#include "vt_cupti_common.h"    /* CUPTI common structures, functions, etc. */

/* mutex for locking the CUPTI environment */
#if (defined(VT_MT) || defined(VT_HYB))
VTThrdMutex* VTThrdMutexCupti = NULL;
#endif /* VT_MT || VT_HYB */

/* set the list of CUPTI contexts to 'empty' */
vt_cupti_ctx_t *vt_cupti_ctxList = NULL;

/* CUPTI global CUDA kernel counter group ID */
uint32_t vt_cupti_cgid_cuda_kernel = VT_NO_ID;

/* global kernel counter IDs */
uint32_t vt_cupti_cid_blocksPerGrid = VT_NO_ID;
uint32_t vt_cupti_cid_threadsPerBlock = VT_NO_ID;
uint32_t vt_cupti_cid_threadsPerKernel = VT_NO_ID;

static uint8_t vt_cupti_initialized = 0;
static uint8_t vt_cupti_finalized   = 0;

void vt_cupti_init()
{
  if(!vt_cupti_initialized){
#if (defined(VT_MT) || defined(VT_HYB))
    VTThrd_createMutex(&VTThrdMutexCupti);
#endif
    VT_CUPTI_LOCK();
    if(!vt_cupti_initialized){
      vt_cntl_msg(2, "[CUPTI] Initializing ... ");
      
      /* register the finalize function of VampirTrace CUPTI to be called before
       * the program exits */
      atexit(vt_cupti_finalize);
      
      vt_cupti_initialized = 1;
      VT_CUPTI_UNLOCK();
    }
  }
}

/*
 * Finalize the CUPTI common interface.
 * - free the VampirTrace CUPTI context list
 */
void vt_cupti_finalize()
{
  if(!vt_cupti_finalized && vt_cupti_initialized){
    VT_CUPTI_LOCK();
    if(!vt_cupti_finalized && vt_cupti_initialized){
      vt_cntl_msg(2, "[CUPTI] Finalizing ... ");
      
      /* free VampirTrace CUPTI context structures */
      while(vt_cupti_ctxList != NULL){
        vt_cupti_ctx_t *tmp =  vt_cupti_ctxList;

        vt_cupti_ctxList = vt_cupti_ctxList->next;

        vt_cupti_finalizeCtx(tmp);
        tmp = NULL;
      }
      
      vt_cupti_finalized = 1;
      VT_CUPTI_UNLOCK();

#if (defined(VT_MT) || defined (VT_HYB))
      VTTHRD_LOCK_ENV();
      VTThrd_deleteMutex(&VTThrdMutexCupti);
      VTTHRD_UNLOCK_ENV();
#endif /* VT_MT || VT_HYB */
    }
  }
}

#if (defined(VT_CUPTI_ACTIVITY) || defined(VT_CUPTI_CALLBACKS))
/*
 * Create a VampirTrace CUPTI stream.
 * 
 * @param vtCtx VampirTrace CUPTI context
 * @param cuStrm CUDA stream
 * @param strmID ID of the CUDA stream
 * 
 * @return pointer to created VampirTrace CUPTI stream
 */
vt_cupti_strm_t* vt_cupti_createStream(vt_cupti_ctx_t *vtCtx, 
                                       CUstream cuStrm, uint32_t strmID)
{
  vt_cupti_strm_t *vtStrm = NULL;
  
  if(vtCtx == NULL){
    vt_warning("[CUPTI] Cannot create stream without VampirTrace CUPTI context");
    return NULL;
  }
  
  vtStrm = (vt_cupti_strm_t *)malloc(sizeof(vt_cupti_strm_t));
  if(vtStrm == NULL)
    vt_error_msg("[CUPTI] Could not allocate memory for stream!");
  vtStrm->cuStrm = cuStrm;
  vtStrm->vtLastTime = vt_gpu_init_time;
  vtStrm->destroyed = 0;
  vtStrm->next = NULL;
  
#if defined(VT_CUPTI_ACTIVITY)
  /* create stream by VT CUPTI callbacks implementation (CUstream is given) */
  if(strmID == VT_CUPTI_NO_STREAM_ID){
    if(cuStrm != VT_CUPTI_NO_STREAM){
      VT_CUPTI_CALL(cuptiGetStreamId(vtCtx->cuCtx, cuStrm, &strmID), 
                                     "cuptiGetStreamId");
    }else{
      vt_warning("[CUPTI] Neither CUDA stream nor stream ID given!");
      free(vtStrm);
      return NULL;
    }
  }
#else /* only VT_CUPTI_CALLBACKS is defined */
  if(vtCtx->callbacks != NULL){
    strmID = vtCtx->callbacks->streamsCreated;
    vtCtx->callbacks->streamsCreated++;
  }
#endif

  vtStrm->cuStrmID = strmID;
  
  /* create VampirTrace thread */
  {
    char thread_name[16] = "CUDA";

    if(vt_gpu_stream_reuse){
      if(vtCtx->devID != VT_NO_ID){
        if(-1 == snprintf(thread_name+4, 12, "[%d]", vtCtx->devID))
          vt_cntl_msg(1, "Could not create thread name for CUDA thread!");
      }
    }else{
      if(vtCtx->devID == VT_NO_ID){
        if(-1 == snprintf(thread_name+4, 12, "[?:%d]", strmID))
          vt_cntl_msg(1, "Could not create thread name for CUDA thread!");
      }else{
        if(-1 == snprintf(thread_name+4, 12, "[%d:%d]", vtCtx->devID, strmID))
          vt_cntl_msg(1, "Could not create thread name for CUDA thread!");
      }
    }
    
    VT_CHECK_THREAD;
    vt_gpu_registerThread(thread_name, VT_MY_THREAD, &(vtStrm->vtThrdID));
  }
  
  if(vt_gpu_init_time < vt_start_time)
      vt_gpu_init_time = vt_start_time;
  
  /* for the first stream created for this context */
  if(vtCtx->strmList == NULL){
    if(vt_gpu_trace_idle > 0){      
      /* write enter event for GPU_IDLE on first stream */
      vt_enter(vtStrm->vtThrdID, &vt_gpu_init_time, vt_gpu_rid_idle);
      /*vt_warning("IDLEente: %llu (%d)", vt_gpu_init_time, vtStrm->vtThrdID);*/
      
#if defined(VT_CUPTI_ACTIVITY)
      if(vtCtx->activity != NULL)
        vtCtx->activity->gpuIdleOn = 1;
#endif
    }
    
    /* set the counter value for cudaMalloc to 0 on first stream */
    if(vt_gpu_trace_memusage > 0)
      vt_count(vtStrm->vtThrdID, &vt_gpu_init_time, vt_gpu_cid_memusage, 0);
  }

  if(vt_gpu_trace_kernels > 1){
    /* set count values to zero */
    vt_count(vtStrm->vtThrdID, &vt_gpu_init_time, vt_cupti_cid_blocksPerGrid, 0);
    vt_count(vtStrm->vtThrdID, &vt_gpu_init_time, vt_cupti_cid_threadsPerBlock, 0);
    vt_count(vtStrm->vtThrdID, &vt_gpu_init_time, vt_cupti_cid_threadsPerKernel, 0);
  }
  
  /* prepend the stream 
  vtStrm->next = vtCtx->strmList;
  vtCtx->strmList = vtStrm;*/
  
  return vtStrm;
}

/*
 * Retrieve a VampirTrace CUPTI stream object. This function will lookup, if 
 * the stream is already available, a stream is reusable or if it has to be
 * created and will return the VampirTrace CUPTI stream object.
 * 
 * @param vtCtx VampirTrace CUPTI Activity context
 * @param cuStrm CUDA stream
 * @param strmID the CUDA stream ID provided by CUPTI callback API
 * 
 * @return the VampirTrace CUPTI stream
 */
vt_cupti_strm_t* vt_cupti_getCreateStream(vt_cupti_ctx_t* vtCtx, 
                                          CUstream cuStrm, uint32_t strmID)
{
  vt_cupti_strm_t *currStrm = NULL;
  vt_cupti_strm_t *lastStrm = NULL;
  vt_cupti_strm_t *reusableStrm = NULL;
  
  if(vtCtx == NULL){
    vt_error_msg("[CUPTI] No context given in vt_cupti_checkStream()!");
    return NULL;
  }
  
  if(strmID == VT_CUPTI_NO_STREAM_ID && cuStrm == VT_CUPTI_NO_STREAM){
    vt_error_msg("[CUPTI] No stream information given!");
    return NULL;
  }
  
  /*** lookup stream ***/
  /*VT_CUPTI_LOCK();*/
  currStrm = vtCtx->strmList;
  lastStrm = vtCtx->strmList;
  while(currStrm != NULL){
    
    /* check for existing stream */
    if( (strmID != VT_CUPTI_NO_STREAM_ID && currStrm->cuStrmID == strmID) || 
        (cuStrm != VT_CUPTI_NO_STREAM && currStrm->cuStrm == cuStrm) ){
      /*VT_CUPTI_UNLOCK();*/
      return currStrm;
    }
    
    /* check for reusable stream */
    if(vt_gpu_stream_reuse && reusableStrm == NULL && currStrm->destroyed == 1){
      reusableStrm = currStrm;
    }
    
    /* remember last stream to append new created stream later */
    lastStrm = currStrm;
    
    /* check next stream */
    currStrm = currStrm->next;
  }
  
  /* reuse a destroyed stream, if there is any available */
  if(vt_gpu_stream_reuse && reusableStrm){
    vt_cntl_msg(2, "[CUPTI] Reusing CUDA stream %d with stream %d",
                   reusableStrm->cuStrmID, strmID);
    reusableStrm->destroyed = 0;
    reusableStrm->cuStrmID = strmID;
    reusableStrm->cuStrm = cuStrm;

    return reusableStrm;
  }
  
#if defined(VT_CUPTI_ACTIVITY)
  /* 
   * If stream list is empty, the stream to be created is not the default
   * stream and GPU idle and memory copy tracing is enabled, then create
   * a default stream.
   * This is necessary to preserve increasing event time stamps!
   */
  if(vtCtx->strmList == NULL && vtCtx->activity != NULL && 
     strmID != vtCtx->activity->defaultStrmID && 
     vt_gpu_trace_idle > 0 && vt_gpu_trace_mcpy){
    vtCtx->strmList = vt_cupti_createStream(vtCtx, cuStrm, 
                                               vtCtx->activity->defaultStrmID);
    lastStrm = vtCtx->strmList;
  }
#endif /* VT_CUPTI_ACTIVITY */
  
  /* create the stream, which has not been created yet */
  currStrm = vt_cupti_createStream(vtCtx, cuStrm, strmID);
  
  /* append the newly created stream */
  if(NULL != lastStrm) lastStrm->next = currStrm;
  else vtCtx->strmList = currStrm;
  
  /*VT_CUPTI_UNLOCK();*/
  return currStrm;
}

/*
 * Get a VampirTrace CUPTI stream by CUDA stream without locking.
 * 
 * @param vtCtx pointer to the VampirTrace CUPTI context, containing the stream
 * @param strmID the CUPTI stream ID
 * 
 * @return VampirTrace CUPTI stream
 */
vt_cupti_strm_t* vt_cupti_getStreamByID(vt_cupti_ctx_t *vtCtx,
                                        uint32_t strmID)
{
  vt_cupti_strm_t* vtStrm = NULL;

  vtStrm = vtCtx->strmList;
  while(vtStrm != NULL){
    if(vtStrm->cuStrmID == strmID){
      return vtStrm;
    }
    vtStrm = vtStrm->next;
  }
  
  return NULL;
}
#endif /* VT_CUPTI_ACTIVITY || VT_CUPTI_CALLBACKS */



/*
 * Create a VampirTrace CUPTI context. If the CUDA context is not given, the 
 * current context will be requested and used.
 * 
 * @param cuCtx CUDA context
 * @param cuDev CUDA device
 * @param ctxID ID of the CUDA context
 * @param devID ID of the CUDA device
 * 
 * @return pointer to created VampirTrace CUPTI context
 */
vt_cupti_ctx_t* vt_cupti_createCtx(CUcontext cuCtx, CUdevice cuDev,
                                   uint32_t cuCtxID, uint32_t cuDevID)
{
  vt_cupti_ctx_t* vtCtx = NULL;
  
  /* create new context */
  vtCtx = (vt_cupti_ctx_t *)malloc(sizeof(vt_cupti_ctx_t));
  if(vtCtx == NULL) 
    vt_error_msg("[CUPTI] Could not allocate memory for VT CUPTI context!");
  vtCtx->ctxID = cuCtxID;
#if (defined(VT_CUPTI_ACTIVITY) || defined(VT_CUPTI_CALLBACKS))
  vtCtx->gpuMemAllocated = 0;
  vtCtx->gpuMemList = NULL;
  vtCtx->strmList = NULL;
#endif
  vtCtx->next = NULL;
  
  VT_CHECK_THREAD;
  vtCtx->ptid = VT_MY_THREAD;
  
  /* try to get CUDA device (ID), if they are not given */
  if(cuDevID == VT_CUPTI_NO_DEVICE_ID){
    if(cuDev == VT_CUPTI_NO_CUDA_DEVICE){
      CUcontext cuCurrCtx;
      
      if(cuCtx != NULL){
        cuCtxGetCurrent(&cuCurrCtx);
      
        /* if given context does not match the current one, get the device for 
           the given one */
        if(cuCtx != cuCurrCtx)
          VT_CUDRV_CALL(cuCtxSetCurrent(cuCtx), NULL);
      }
      
      if(CUDA_SUCCESS == cuCtxGetDevice(&cuDev))
        cuDevID = (uint32_t)cuDev;
      
      /* reset the active context */
      if(cuCtx != NULL && cuCtx != cuCurrCtx)
        VT_CUDRV_CALL(cuCtxSetCurrent(cuCurrCtx), NULL);
      
    }else{
      /* no device ID, but CUDA device is given */
      cuDevID = (uint32_t)cuDev;
    }
  }
  
  vtCtx->devID = cuDevID;
  vtCtx->cuDev = cuDev;
  
  /* get the current CUDA context, if it is not given */
  if(cuCtx == NULL) 
    VT_CUDRV_CALL(cuCtxGetCurrent(&cuCtx), NULL);
  
  /* set the CUDA context */
  vtCtx->cuCtx = cuCtx;
  
#if defined(VT_CUPTI_ACTIVITY)
  vtCtx->activity = NULL;
#endif

#if defined(VT_CUPTI_CALLBACKS)
  vtCtx->callbacks = NULL;
#endif
  
#if defined(VT_CUPTI_EVENTS)
  vtCtx->events = NULL;
#endif

  vt_cntl_msg(2, "[CUPTI] Created context for CUcontext %d, CUdevice %d", 
              cuCtx, cuDev);
  
  return vtCtx;
}

/*
 * Prepend the given VampirTrace CUPTI context to the global context list.
 * 
 * @param vtCtx pointer to the VampirTrace CUPTI context to be prepended
 */
void vt_cupti_prependCtx(vt_cupti_ctx_t *vtCtx)
{
  VT_CUPTI_LOCK();
  vtCtx->next = vt_cupti_ctxList;
  vt_cupti_ctxList = vtCtx;
  VT_CUPTI_UNLOCK();
}

/*
 * Get a VampirTrace CUPTI context by CUDA context
 * 
 * @param cuCtx the CUDA context
 * 
 * @return VampirTrace CUPTI context
 */
vt_cupti_ctx_t* vt_cupti_getCtx(CUcontext cuCtx)
{
  vt_cupti_ctx_t* vtCtx = NULL;
  
  /* lookup context */
  VT_CUPTI_LOCK();
  vtCtx = vt_cupti_ctxList;
  while(vtCtx != NULL){
    if(vtCtx->cuCtx == cuCtx){
      
      /* workaround to set the correct device number 
      if(vtCtx->devID == VT_CUPTI_NO_DEVICE_ID){
        CUdevice cuDev;
        
        if(CUDA_SUCCESS != cuCtxGetDevice(&cuDev)){
          vt_warning("[CUPTI] Could not get CUdevice from context");
        }
        
        vtCtx->devID = (uint32_t)cuDev;
        vtCtx->cuDev = cuDev;
      }*/
      
      VT_CUPTI_UNLOCK();
      return vtCtx;
    }
    vtCtx = vtCtx->next;
  }
  VT_CUPTI_UNLOCK();
  
  return NULL;
}

/*
 * Get a VampirTrace CUPTI context by CUDA context without locking.
 * 
 * @param cuCtx the CUDA context
 * 
 * @return VampirTrace CUPTI context
 */
vt_cupti_ctx_t* vt_cupti_getCtxNoLock(CUcontext cuCtx)
{
  vt_cupti_ctx_t* vtCtx = NULL;

  vtCtx = vt_cupti_ctxList;
  while(vtCtx != NULL){
    if(vtCtx->cuCtx == cuCtx){
      return vtCtx;
    }
    vtCtx = vtCtx->next;
  }
  
  return NULL;
}

/*
 * Get or if not available create a VampirTrace CUPTI context by CUDA context.
 * 
 * @param cuCtx the CUDA context
 * 
 * @return VampirTrace CUPTI context
 */
vt_cupti_ctx_t* vt_cupti_getCreateCtx(CUcontext cuCtx)
{
  vt_cupti_ctx_t* vtCtx = vt_cupti_getCtx(cuCtx);
  
  if(vtCtx == NULL){
    VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);
    vtCtx = vt_cupti_createCtx(cuCtx, VT_CUPTI_NO_CUDA_DEVICE,
                               VT_CUPTI_NO_CONTEXT_ID, VT_CUPTI_NO_DEVICE_ID);
    VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);

    vt_cupti_prependCtx(vtCtx);
  }
  
  return vtCtx;
}

/*
 * Remove a context from the global context list and return it.
 * 
 * @param cuCtx pointer to the CUDA context
 * @return the VampirTrace CUPTI context, which has been removed 
 */
vt_cupti_ctx_t* vt_cupti_removeCtx(CUcontext *cuCtx)
{
  vt_cupti_ctx_t *currCtx = NULL;
  vt_cupti_ctx_t *lastCtx = NULL;

  VT_CUPTI_LOCK();
  currCtx = vt_cupti_ctxList;
  lastCtx = vt_cupti_ctxList;
  while(currCtx != NULL){
    if(currCtx->cuCtx == *cuCtx){
      /* if first element in list */
      if(currCtx == vt_cupti_ctxList){
        vt_cupti_ctxList = vt_cupti_ctxList->next;
      }else{
        lastCtx->next = currCtx->next;
      }
      VT_CUPTI_UNLOCK();
      return currCtx;
    }
    lastCtx = currCtx;
    currCtx = currCtx->next;
  }
  VT_CUPTI_UNLOCK();

  vt_cntl_msg(2, "[CUPTI] Could not remove context (CUDA Context not found)!");
  return NULL;
}

/*
 * Finalize the VampirTrace CUPTI context and free all memory allocated with it.
 * 
 * @param vtCtx pointer to the VampirTrace CUPTI context
 */
void vt_cupti_finalizeCtx(vt_cupti_ctx_t *vtCtx)
{
  if(vtCtx == NULL)
    return;
  
  /* write exit event for GPU idle time */
  if(vt_gpu_trace_idle > 0 && vtCtx->strmList != NULL 
#if defined(VT_CUPTI_ACTIVITY)
     && vtCtx->activity != NULL && vtCtx->activity->gpuIdleOn == 1
#endif
    ){
    uint64_t idle_end = vt_pform_wtime();
    
    /*vt_warning("IDLEexit: %llu (%d)", idle_end, vtCtx->strmList->vtThrdID);*/
    vt_exit(vtCtx->strmList->vtThrdID, &idle_end);
  }
  
  /* cleanup stream list */
  while(vtCtx->strmList != NULL){
    vt_cupti_strm_t *vtStrm = vtCtx->strmList;
    
    vtCtx->strmList = vtCtx->strmList->next;
    
    free(vtStrm);
    vtStrm = NULL;
  }
  
  /* free CUDA malloc entries, if user application has memory leaks */
  while(vtCtx->gpuMemList != NULL){
    vt_cupti_gpumem_t *vtMem =  vtCtx->gpuMemList;
    
    if(vt_gpu_trace_memusage > 1)
      vt_cntl_msg(1, "[CUPTI] Free of %d bytes GPU memory missing!", 
                     vtMem->size);
    
    vtCtx->gpuMemList = vtMem->next;
    free(vtMem);
    vtMem = NULL;
  }
  
#if defined(VT_CUPTI_ACTIVITY)
  if(vtCtx->activity != NULL)
    free(vtCtx->activity);
#endif

#if (defined(VT_CUPTI_CALLBACKS) && !defined(VT_CUPTI_ACTIVITY))
  if(vtCtx->callbacks != NULL)
    free(vtCtx->callbacks);
#endif
  
#if defined(VT_CUPTI_EVENTS)
  if(vtCtx->events != NULL)
    free(vtCtx->events);
#endif
  
  free(vtCtx);
  vtCtx = NULL;
}

/*
 * Handles errors returned from CUPTI function calls.
 * 
 * @param ecode the CUDA driver API error code
 * @param msg a message to get more detailed information about the error
 * @param the corresponding file
 * @param the line the error occurred
 */
void vt_cupti_handleError(CUptiResult err, const char* msg,
                          const char *file, const int line)
{
  const char *errstr;
  
  if(msg != NULL) vt_cntl_msg(1, msg);
  
  cuptiGetResultString(err, &errstr);
  
  if(vt_gpu_error){
    vt_error_msg("[CUPTI] %s:%d:'%s'", file, line, errstr);
  }else{
    vt_warning("[CUPTI] %s:%d:'%s'", file, line, errstr);
  }
}
