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

#include "config.h"         /* snprintf */

#include "vt_env.h"         /* get environment variables */
#include "vt_pform.h"       /* VampirTrace time measurement */
#include "vt_defs.h"        /* VampirTrace constants */
#include "vt_error.h"       /* VampirTrace warning and error messages */
#include "vt_mallocwrap.h"  /* wrapping of malloc and free */
#include "vt_gpu.h"         /* common for GPU */
#include "util/hash.h"

#include "vt_cupti.h"           /* CUPTI header */
#include "vt_cupti_common.h"    /* CUPTI common structures, functions, etc. */

#include "vt_cupti_callback.h"
#if defined(VT_CUPTI_EVENTS)
#include "vt_cupti_events.h"    /* Support for CUPTI events */
#endif

#if defined(VT_CUPTI_ACTIVITY)
#include "vt_cupti_activity.h"  /* Support for CUPTI activity */
#endif

#include <stdio.h>
#include <string.h>

#define VT_CUPTI_ENABLE_CALLBACK(_domain, _cbid)                               \
  {                                                                            \
    VT_CUPTI_CALL(cuptiEnableCallback(1, vt_cupticb_subscriber,                \
                                      _domain, _cbid),                         \
                  "[CUPTI Callbacks] Enable CUPTI callback failed!");          \
  }

#define VT_CUPTI_DISABLE_CALLBACK(_domain, _cbid)                              \
  {                                                                            \
    VT_CUPTI_CALL(cuptiEnableCallback(0, vt_cupticb_subscriber,                \
                                      _domain, _cbid),                         \
                  "[CUPTI Callbacks] Disable CUPTI callback failed!");         \
  }

#define VT_CUPTI_ENABLE_CALLBACK_DOMAIN(_domain)                               \
  {                                                                            \
    VT_CUPTI_CALL(cuptiEnableDomain(1, vt_cupticb_subscriber, _domain),        \
                  "[CUPTI Callbacks] Enable domain failed!");                  \
  }

#define VT_CUPTI_DISABLE_CALLBACK_DOMAIN(_domain)                              \
  {                                                                            \
    VT_CUPTI_CALL(cuptiEnableDomain(0, vt_cupticb_subscriber, _domain),        \
                  "[CUPTI Callbacks] Disable domain failed!");                 \
  }

#define ENABLE_CUDART_CALLBACKS() \
  if(vt_cupticb_trace_runtimeAPI) \
    VT_CUPTI_ENABLE_CALLBACK_DOMAIN(CUPTI_CB_DOMAIN_RUNTIME_API)

#define DISABLE_CUDART_CALLBACKS() \
  if(vt_cupticb_trace_runtimeAPI) \
    VT_CUPTI_DISABLE_CALLBACK_DOMAIN(CUPTI_CB_DOMAIN_RUNTIME_API)

#define ENABLE_CUDRV_CALLBACKS() \
  if(vt_cupticb_trace_driverAPI)\
    VT_CUPTI_ENABLE_CALLBACK_DOMAIN(CUPTI_CB_DOMAIN_DRIVER_API)

#define DISABLE_CUDRV_CALLBACKS() \
  if(vt_cupticb_trace_driverAPI)\
    VT_CUPTI_DISABLE_CALLBACK_DOMAIN(CUPTI_CB_DOMAIN_DRIVER_API)

#define SUSPEND_CALLBACKS(_vtCbCtx) _vtCbCtx->callbacks_enabled = 0;
#define RESUME_CALLBACKS(_vtCbCtx) _vtCbCtx->callbacks_enabled = 1;

/*
 * Handle CUDA runtime memory copy calls.
 *
 * @param _cbInfo information about the callback
 * @param _kind the direction of the transfer
 * @param _src source memory pointer
 * @param _dst destination memory pointer
 * @param _bytes the number of transfered bytes
 * @param _time execution time stamp
 */
#define VT_CUPTICB_MEMCPY_CUDART(_cbInfo, _kind, _src, _dst, _bytes, _time) \
{\
  if(_kind == cudaMemcpyDefault){\
    vt_cupti_handle_memcpyDefault(_cbInfo, (CUdeviceptr)_src, \
                                  (CUdeviceptr)_dst, _bytes, _time);\
  }else\
    vt_cupticb_handle_memcpy(_cbInfo, _kind, _bytes, _time);\
}

/* global subscriber handles */
static CUpti_SubscriberHandle vt_cupticb_subscriber;

/* flag: tracing of CUDA runtime API enabled? */
static uint8_t vt_cupticb_trace_runtimeAPI = 0;

/* flag: tracing of CUDA driver API enabled? */
static uint8_t vt_cupticb_trace_driverAPI = 0;

/* initialization and finalization flags */
static uint8_t vt_cupticb_initialized = 0;
static uint8_t vt_cupticb_finalized = 0;

/**************** The callback functions to be registered *********************/

/* CUDA runtime API callback function */
/* some of CUPTI API functions have changed */
#if defined(VT_CUPTI_ACTIVITY)
void CUPTIAPI vt_cupticb_all(void *, CUpti_CallbackDomain,
                                CUpti_CallbackId, const void *);
void (*vt_cupticb_all_ptr)(void *, CUpti_CallbackDomain, 
                              CUpti_CallbackId, const void *)
      = vt_cupticb_all;

void CUPTIAPI vt_cupticb_cudart(void *, CUpti_CallbackDomain,
                                CUpti_CallbackId, const CUpti_CallbackData *);

void vt_cupticb_driverAPI(CUpti_CallbackId, const CUpti_CallbackData *);

void vt_cupticb_resource(CUpti_CallbackId, const CUpti_ResourceData *);

void vt_cupticb_sync(CUpti_CallbackId, const CUpti_SynchronizeData *);

#else
void CUPTIAPI vt_cupticb_cudart(void *, CUpti_CallbackDomain,
                                CUpti_CallbackId, const CUpti_CallbackData *);
void (*vt_cupticb_cudart_ptr)(void *, CUpti_CallbackDomain, 
                              CUpti_CallbackId, const CUpti_CallbackData *)
      = vt_cupticb_cudart;

/******************************************************************************/
#endif /* VT_CUPTI_ACTIVITY */

/*********************** Internal function declarations ***********************/
static enum cudaMemcpyKind vt_cupticb_getMemcpyKind(CUmemorytype, CUmemorytype);
static void vt_cupticb_handle_memcpy(const CUpti_CallbackData *, 
                                       enum cudaMemcpyKind, uint64_t, uint64_t);
static void vt_cupticb_handle_memcpyP2P(const CUpti_CallbackData *cbInfo,
                                             CUcontext cuSrcCtx,
                                             CUcontext cuDstCtx,
                                             uint64_t bytes, uint64_t time);
static void vt_cupti_handle_memcpyDefault(const CUpti_CallbackData *cbInfo, 
                                          CUdeviceptr cuSrcDevPtr,
                                          CUdeviceptr cuDstDevPtr, 
                                          uint64_t bytes, uint64_t time);
static void vt_cupticb_handle_cudart_mcpyAsync(const CUpti_CallbackData *cbInfo,
                 enum cudaMemcpyKind kind, uint64_t bytes, cudaStream_t cuStrm);

static void vt_cupticb_handle_cudart_knconf(const CUpti_CallbackData *);
static void vt_cupticb_handle_cuda_kernel(const CUpti_CallbackData *, CUstream, 
                                          uint64_t blocks);

static void vt_cupticb_handle_malloc(CUcontext, uint64_t, size_t);
static void vt_cupticb_handle_free(CUcontext, uint64_t);
/******************************************************************************/

/* hash table to map CUpti_CallbackIds to VampirTrace rids */
#define VT_CUPTICB_CUDA_API_FUNC_MAX 1024
static uint32_t vt_cupticb_cudaApiFuncTab[VT_CUPTICB_CUDA_API_FUNC_MAX];

static uint32_t vt_cupticb_cudaApiHashFunc(CUpti_CallbackDomain domain,
                                           CUpti_CallbackId cid)
{
  uint32_t idx = 0;

  /* Use an offset for the driver API functions, if CUDA runtime and driver
      API recording is enabled (uncommon case) */
  if( vt_cupticb_trace_driverAPI && vt_cupticb_trace_runtimeAPI ){
    uint16_t offset = 0;

    if( domain == CUPTI_CB_DOMAIN_DRIVER_API ){
      offset = VT_CUPTICB_CUDA_API_FUNC_MAX/2;
    }

    idx = offset + ( uint32_t )cid;

    if( ( domain == CUPTI_CB_DOMAIN_RUNTIME_API ) &&
          ( idx >= (uint32_t)(VT_CUPTICB_CUDA_API_FUNC_MAX - offset) )){
      idx = 0;

      vt_error_msg("[CUPTI Callbacks] Hash table for CUDA runtime API "
                    "function %d is to small!", cid );
    }
  }else{
    idx = ( uint32_t )cid;
  }

  if( idx >= VT_CUPTICB_CUDA_API_FUNC_MAX ){
    idx = 0;

    vt_error_msg("[CUPTI Callbacks] Hash table for CUDA API "
                 "function %d is to small!", cid );
  }
  
  return (uint32_t)idx;
}

static void vt_cupticb_cudaApiFuncPut(CUpti_CallbackDomain domain, 
                                    CUpti_CallbackId cid, uint32_t rid)
{  
  vt_cupticb_cudaApiFuncTab[vt_cupticb_cudaApiHashFunc(domain, cid)] = rid;
}

static uint32_t vt_cupticb_cudaApiFuncGet(CUpti_CallbackDomain domain, 
                                          CUpti_CallbackId cid)
{
  return vt_cupticb_cudaApiFuncTab[vt_cupticb_cudaApiHashFunc(domain, cid)];
}

/*
 * Set a CUPTI callback function for a specific CUDA runtime or driver function 
 * or for a whole domain (runtime or driver API)
 * 
 * @param subscriber handle to the initialize subscriber
 * @param callback the callback function
 * @param domain The domain of the callback
 * @param cbid The ID of the API function associated with this callback, if it
 *             is not valid, the whole domain will be enabled
 */
static void vt_cupti_set_callback(CUpti_CallbackFunc callback,
                                  CUpti_CallbackDomain domain,
                                  CUpti_CallbackId cbid)
{
  CUptiResult cuptiErr;
  static uint8_t initflag = 1;

  if(initflag){
    initflag = 0;
    
    VT_CUDRV_CALL(cuInit(0), "cuInit");
    
    /* only one subscriber allowed at a time */
    cuptiErr = cuptiSubscribe(&vt_cupticb_subscriber, callback, NULL);
    VT_CUPTI_CALL(cuptiErr, "cuptiSubscribe");
  }
  
  if(CUPTI_CB_DOMAIN_INVALID == domain){
    cuptiEnableAllDomains(1, vt_cupticb_subscriber);
  }else{
    if((cbid == CUPTI_RUNTIME_TRACE_CBID_INVALID) || 
       (cbid == CUPTI_DRIVER_TRACE_CBID_INVALID)){
      cuptiErr = cuptiEnableDomain(1, vt_cupticb_subscriber, domain);
      VT_CUPTI_CALL(cuptiErr, "cuptiEnableDomain");
    }else{
      cuptiErr = cuptiEnableCallback(1, vt_cupticb_subscriber, domain, cbid);
      VT_CUPTI_CALL(cuptiErr, "cuptiEnableCallback");
    }
  }
}

/*
 * Creates a VampirTrace CUPTI callbacks context.
 * 
 * @param vtCtx the VampirTrace CUPTI context
 * @param cuStrm the CUDA stream
 * 
 * @return the VampirTrace CUPTI callbacks context
 */
static vt_cupti_callbacks_t* vt_cupticb_createCbCtx(vt_cupti_ctx_t *vtCtx
#if !defined(VT_CUPTI_ACTIVITY)
        , CUstream cuStrm
#endif
)
{
  vt_cupti_callbacks_t *vtCbCtx = NULL;
  
  if(vtCtx == NULL) return NULL;
  
  /* create new context, as it is not listed */
  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);
  vtCbCtx = (vt_cupti_callbacks_t *)malloc(sizeof(vt_cupti_callbacks_t));
  if(vtCbCtx == NULL) 
    vt_error_msg("[CUPTI Callbacks] Could not allocate memory for callbacks context!");
  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
  
  vtCbCtx->kernelData = NULL;
    
#if (defined(CUPTI_API_VERSION) && (CUPTI_API_VERSION >= 3))
  if((vt_gpu_config & VT_GPU_TRACE_CONCURRENT_KERNEL) 
           == VT_GPU_TRACE_CONCURRENT_KERNEL){
    vtCbCtx->concurrentKernels = 1;
  }else{
    int tmp_ck;

    /* check for concurrent kernel support */
    VT_CUDRV_CALL(cuDeviceGetAttribute(&tmp_ck, 
                   CU_DEVICE_ATTRIBUTE_CONCURRENT_KERNELS, vtCtx->cuDev), 
                   "cuDeviceGetAttribute");

    vtCbCtx->concurrentKernels = (uint8_t)tmp_ck;
  }
  
  vtCbCtx->streamsCreated = 0;
#endif
  
#if !defined(VT_CUPTI_ACTIVITY)
  /* enable handling of callbacks */
  vtCbCtx->callbacks_enabled = 1;  
  
  /* create first empty CUDA stream */
  vtCbCtx->streamsCreated = 2;
  vtCbCtx->strmList = NULL;
  vtCbCtx->strmList = vt_cupticb_createStream(cuStrm, vtCtx);
#endif
  
  /* set the callback context */
  vtCtx->callbacks = vtCbCtx;
  
  return vtCbCtx;
}

/*
 * Finalize the VampirTrace CUPTI Callbacks context.
 * 
 * @param vtCtx pointer to VampirTrace CUPTI context
 */
static void vt_cupti_callbacks_finalizeContext(vt_cupti_ctx_t *vtCtx)
{
  vt_cupti_kernel_t *vtKn = NULL;
  
  if(vtCtx == NULL || vtCtx->callbacks == NULL || 
     vtCtx->callbacks->kernelData == NULL)
    return;
  
  vtKn = vtCtx->callbacks->kernelData;
  
  if(vtCtx->callbacks->kernelData->down != NULL)
    vt_warning("[CUPTI Callbacks] Not all configured kernels have been executed!");
  
  /* free the allocated memory for kernel parameters */
  while(vtKn != NULL){
    vt_cupti_kernel_t *tmp = vtKn;
    
    vtKn = vtKn->up;
    free(tmp);
  }
  
  vtCtx->callbacks->kernelData = NULL;
}

#if (defined(CUPTI_API_VERSION) && (CUPTI_API_VERSION >= 2))
/*
 * This CUPTI callback function chooses the CUPTI domain.
 *
 * @param userdata pointer to the user data
 * @param domain the callback domain (runtime or driver API)
 * @param cbid the ID of the callback function in the given domain
 * @param cbInfo information about the callback
 */
void CUPTIAPI vt_cupticb_all(void *userdata, 
                             CUpti_CallbackDomain domain,
                             CUpti_CallbackId cbid, 
                             const void *cbInfo)
{
  if(CUPTI_CB_DOMAIN_RUNTIME_API == domain)
    vt_cupticb_cudart(userdata, domain, cbid, (CUpti_CallbackData *)cbInfo);
  
  if(CUPTI_CB_DOMAIN_DRIVER_API == domain)
    vt_cupticb_driverAPI(cbid, (CUpti_CallbackData *)cbInfo);
  
  if(CUPTI_CB_DOMAIN_RESOURCE == domain)
    vt_cupticb_resource(cbid, (CUpti_ResourceData *)cbInfo);
  
  if(CUPTI_CB_DOMAIN_SYNCHRONIZE == domain)
    vt_cupticb_sync(cbid, (CUpti_SynchronizeData *)cbInfo);
}
#endif

/*
 * This callback function is used to trace the CUDA runtime API.
 *
 * @param userdata pointer to the user data
 * @param domain the callback domain (runtime API)
 * @param cbid the ID of the callback function in the given domain
 * @param cbInfo information about the callback
 */
void CUPTIAPI vt_cupticb_cudart(void *userdata, 
                                CUpti_CallbackDomain domain,
                                CUpti_CallbackId cbid, 
                                const CUpti_CallbackData *cbInfo)
{
  uint32_t ptid;
  uint64_t time;
  uint32_t rid_func = VT_NO_ID;
  uint32_t hash_api_rid = VT_NO_ID;
  
  if(cbid == CUPTI_RUNTIME_TRACE_CBID_INVALID) return;
  
  /* record cuCtxSynchronize in an extra function group */
  if(cbid == CUPTI_RUNTIME_TRACE_CBID_cudaDeviceSynchronize_v3020){
    if(vt_gpu_sync_level > 1){
      uint64_t time = vt_pform_wtime();
      
      VT_CHECK_THREAD;
      ptid = VT_MY_THREAD;

      if(cbInfo->callbackSite == CUPTI_API_ENTER){
        vt_enter(ptid, &time, vt_gpu_rid_sync);
      }else if(cbInfo->callbackSite == CUPTI_API_EXIT){
        vt_exit(ptid, &time);
      }
      
      return;
    }
  }
  
  VT_CHECK_THREAD;
  ptid = VT_MY_THREAD;
  
  /* get the VampirTrace region ID for the API function */
  hash_api_rid = vt_cupticb_cudaApiFuncGet(CUPTI_CB_DOMAIN_RUNTIME_API, cbid);
  if(hash_api_rid != VT_NO_ID){
    rid_func = hash_api_rid;
  }else{
    rid_func = vt_def_region(VT_MASTER_THREAD, cbInfo->functionName, VT_NO_ID,
                             VT_NO_LNO, VT_NO_LNO, "CUDART_API", VT_FUNCTION);
    
    vt_cupticb_cudaApiFuncPut(CUPTI_CB_DOMAIN_RUNTIME_API, cbid, rid_func);
  }
  
  /*********** write enter and exit records for CUDA runtime API **************/
  time = vt_pform_wtime();
  if(cbInfo->callbackSite == CUPTI_API_ENTER){
    (void)vt_enter(ptid, &time, rid_func);
  }
  
  if(cbInfo->callbackSite == CUPTI_API_EXIT){
    vt_exit(ptid, &time);
  }

  /*
   ************** Semantic function instrumentation *********************
   */
  
  /* Memory allocation and deallocation tracing */
  if(vt_gpu_trace_memusage > 0 && !vt_cupticb_trace_driverAPI){
    switch(cbid){
    /********************** CUDA memory allocation ******************************/
      case CUPTI_RUNTIME_TRACE_CBID_cudaMalloc_v3020: {
        if(cbInfo->callbackSite == CUPTI_API_EXIT){
          cudaMalloc_v3020_params *params = 
                              (cudaMalloc_v3020_params *)cbInfo->functionParams;

          vt_cupticb_handle_malloc(cbInfo->context,
                                   (uint64_t)*(params->devPtr), 
                                   params->size);
        }

        return;
      }

      case CUPTI_RUNTIME_TRACE_CBID_cudaMallocPitch_v3020: {
        if(cbInfo->callbackSite == CUPTI_API_EXIT){
          cudaMallocPitch_v3020_params *params = 
                           (cudaMallocPitch_v3020_params *)cbInfo->functionParams;

          vt_cupticb_handle_malloc(cbInfo->context, 
                                   (uint64_t)*(params->devPtr), 
                                   params->height * (*(params->pitch)));
        }

        return;
      }

      case CUPTI_RUNTIME_TRACE_CBID_cudaMallocArray_v3020: {
        if(cbInfo->callbackSite == CUPTI_API_EXIT){
          cudaMallocArray_v3020_params *params = 
                           (cudaMallocArray_v3020_params *)cbInfo->functionParams;

          vt_cupticb_handle_malloc(cbInfo->context,
                                   (uint64_t)*(params->array), 
                                   params->height * params->width);
        }

        return;
      }

      case CUPTI_RUNTIME_TRACE_CBID_cudaMalloc3D_v3020: {
        if(cbInfo->callbackSite == CUPTI_API_EXIT){
          cudaMalloc3D_v3020_params *params = 
                           (cudaMalloc3D_v3020_params *)cbInfo->functionParams;

          vt_cupticb_handle_malloc(cbInfo->context,
                                   (uint64_t)params->pitchedDevPtr->ptr, 
     params->pitchedDevPtr->pitch * params->extent.height * params->extent.depth);
        }

        return;
      }

      case CUPTI_RUNTIME_TRACE_CBID_cudaMalloc3DArray_v3020: {
        if(cbInfo->callbackSite == CUPTI_API_EXIT){
          cudaMalloc3DArray_v3020_params *params = 
                         (cudaMalloc3DArray_v3020_params *)cbInfo->functionParams;

          vt_cupticb_handle_malloc(cbInfo->context,
                                   (uint64_t)*(params->array), 
            params->extent.width * params->extent.height * params->extent.depth);
        }

        return;
      }

      case CUPTI_RUNTIME_TRACE_CBID_cudaFree_v3020: {
        if(cbInfo->callbackSite == CUPTI_API_ENTER){

          vt_cupticb_handle_free(cbInfo->context,
           (uint64_t)((cudaFree_v3020_params *)cbInfo->functionParams)->devPtr);
        }

        return;
      }

      case CUPTI_RUNTIME_TRACE_CBID_cudaFreeArray_v3020: {
        if(cbInfo->callbackSite == CUPTI_API_ENTER){

          vt_cupticb_handle_free(cbInfo->context,
         (uint64_t)((cudaFreeArray_v3020_params *)cbInfo->functionParams)->array);
        }

        return;
      }

      default: break;
    }
  }
  
  if(vt_gpu_trace_mcpy && !vt_cupticb_trace_driverAPI){
#if defined(VT_CUPTI_ACTIVITY)
    if(vt_cupti_events_enabled || (vt_gpu_sync_level > 2 &&
     (vt_gpu_config & VT_GPU_TRACE_SYNC) == VT_GPU_TRACE_SYNC))
#endif
    {
      switch(cbid){
      /****************** synchronous CUDA memory copies **************************/
        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy_v3020: {
          cudaMemcpy_v3020_params *params = 
                (cudaMemcpy_v3020_params *)cbInfo->functionParams;

          VT_CUPTICB_MEMCPY_CUDART(cbInfo, params->kind, 
                                   params->src, params->dst,
                                   params->count, time);
          
          return;
        }

        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy2D_v3020: {
          cudaMemcpy2D_v3020_params *params = 
                  (cudaMemcpy2D_v3020_params *)cbInfo->functionParams;

          VT_CUPTICB_MEMCPY_CUDART(cbInfo, params->kind, 
                                   params->src, params->dst,
                                   (uint64_t)(params->height * params->width), 
                                   time);
          return;
        }

        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyToArray_v3020: {
          cudaMemcpyToArray_v3020_params *params = 
                  (cudaMemcpyToArray_v3020_params *)cbInfo->functionParams;

          VT_CUPTICB_MEMCPY_CUDART(cbInfo, params->kind, 
                                   params->src, params->dst,
                                   (uint64_t)params->count, time);
          return;
        }

        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy2DToArray_v3020: {
          cudaMemcpy2DToArray_v3020_params *params = 
                  (cudaMemcpy2DToArray_v3020_params *)cbInfo->functionParams;

          VT_CUPTICB_MEMCPY_CUDART(cbInfo, params->kind, 
                                   params->src, params->dst,
                                   (uint64_t)(params->height * params->width), 
                                   time);
          return;
        }

        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyFromArray_v3020: {
          cudaMemcpyFromArray_v3020_params *params = 
                  (cudaMemcpyFromArray_v3020_params *)cbInfo->functionParams;

          VT_CUPTICB_MEMCPY_CUDART(cbInfo, params->kind, 
                                   params->src, params->dst,
                                   (uint64_t)params->count, time);
          return;
        }

        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy2DFromArray_v3020: {
          cudaMemcpy2DFromArray_v3020_params *params = 
                  (cudaMemcpy2DFromArray_v3020_params *)cbInfo->functionParams;

          VT_CUPTICB_MEMCPY_CUDART(cbInfo, params->kind, 
                                   params->src, params->dst,
                                   (uint64_t)(params->height * params->width), 
                                   time);
          return;
        }

        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyArrayToArray_v3020: {
          cudaMemcpyArrayToArray_v3020_params *params = 
                  (cudaMemcpyArrayToArray_v3020_params *)cbInfo->functionParams;

          VT_CUPTICB_MEMCPY_CUDART(cbInfo, params->kind, 
                                   params->src, params->dst,
                                   (uint64_t)params->count, time);
          return;
        }

        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy2DArrayToArray_v3020: {
          cudaMemcpy2DArrayToArray_v3020_params *params = 
                  (cudaMemcpy2DArrayToArray_v3020_params *)cbInfo->functionParams;

          VT_CUPTICB_MEMCPY_CUDART(cbInfo, params->kind, 
                                   params->src, params->dst,
                                   (uint64_t)(params->height * params->width), 
                                   time);
          return;
        }

        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyToSymbol_v3020: {
          cudaMemcpyToSymbol_v3020_params *params = 
                  (cudaMemcpyToSymbol_v3020_params *)cbInfo->functionParams;

          VT_CUPTICB_MEMCPY_CUDART(cbInfo, params->kind, 
                                   params->src, params->symbol,
                                   (uint64_t)params->count, time);
          return;
        }

        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyFromSymbol_v3020: {
          cudaMemcpyFromSymbol_v3020_params *params = 
                  (cudaMemcpyFromSymbol_v3020_params *)cbInfo->functionParams;

          VT_CUPTICB_MEMCPY_CUDART(cbInfo, params->kind, 
                                   params->symbol, params->dst,
                                   (uint64_t)params->count, time);
          return;
        }

        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy3D_v3020: {
          cudaMemcpy3D_v3020_params *params = 
                  (cudaMemcpy3D_v3020_params *)cbInfo->functionParams;

          VT_CUPTICB_MEMCPY_CUDART(cbInfo, params->p->kind, 
                                   params->p->srcArray, params->p->dstArray,
                 (uint64_t)(params->p->extent.height * params->p->extent.width * 
                            params->p->extent.depth), 
                                   time);
          return;
        }

        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyPeer_v4000: {
          cudaMemcpyPeer_v4000_params *params = 
                (cudaMemcpyPeer_v4000_params *)cbInfo->functionParams;

          CUcontext cuSrcCtx;
          CUcontext cuDstCtx;
          int cudaCurrDev = 0;
          
          DISABLE_CUDART_CALLBACKS();
          DISABLE_CUDRV_CALLBACKS();
          
          /* get the currently active device */
          cudaGetDevice(&cudaCurrDev);
          
          /* get the source and destination context */
          if(params->srcDevice == cudaCurrDev){
            cuSrcCtx = cbInfo->context;
            if(cudaSuccess != cudaSetDevice(params->dstDevice)){
              vt_warning("[CUPTI Callbacks] Could not set device in P2P mcyp!");
              return;
            }
            VT_CUDRV_CALL(cuCtxGetCurrent(&cuDstCtx), NULL);
          }else if(params->dstDevice == cudaCurrDev){
            cuDstCtx = cbInfo->context;
            if(cudaSuccess != cudaSetDevice(params->srcDevice)){
              vt_warning("[CUPTI Callbacks] Could not set device in P2P mcyp!");
              return;
            }
            VT_CUDRV_CALL(cuCtxGetCurrent(&cuSrcCtx), NULL);
          }else{
            vt_warning("[CUPTI Callbacks] Could not identify P2P memcpy!");
            return;
          }
          
          /* reset the original active device */
          if(cudaSuccess != cudaSetDevice(cudaCurrDev)){
            vt_error_msg("[CUPTI Callbacks] Could not reset device in P2P mcyp!");
          }
          
          ENABLE_CUDART_CALLBACKS();
          ENABLE_CUDRV_CALLBACKS();
          
          vt_cupticb_handle_memcpyP2P(cbInfo, cuSrcCtx, cuDstCtx, 
                                      (uint64_t)params->count, time);
          
          return;
        }

        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy3DPeer_v4000: {
          cudaMemcpy3DPeer_v4000_params *params = 
                  (cudaMemcpy3DPeer_v4000_params *)cbInfo->functionParams;
          
          CUcontext cuSrcCtx;
          CUcontext cuDstCtx;
          int cudaCurrDev = 0;
          
          DISABLE_CUDART_CALLBACKS();
          DISABLE_CUDRV_CALLBACKS();
          
          /* get the currently active device */
          cudaGetDevice(&cudaCurrDev);
          
          /* get the source and destination context */
          if(params->p->srcDevice == cudaCurrDev){
            cuSrcCtx = cbInfo->context;
            if(cudaSuccess != cudaSetDevice(params->p->dstDevice)){
              vt_warning("[CUPTI Callbacks] Could not set device in P2P mcyp!");
              return;
            }
            VT_CUDRV_CALL(cuCtxGetCurrent(&cuDstCtx), NULL);
          }else if(params->p->dstDevice == cudaCurrDev){
            cuDstCtx = cbInfo->context;
            if(cudaSuccess != cudaSetDevice(params->p->srcDevice)){
              vt_warning("[CUPTI Callbacks] Could not set device in P2P mcyp!");
              return;
            }
            VT_CUDRV_CALL(cuCtxGetCurrent(&cuSrcCtx), NULL);
          }else{
            vt_warning("[CUPTI Callbacks] Could not identify P2P memcpy 3D!");
            return;
          }
          
          /* reset the original active device */
          if(cudaSuccess != cudaSetDevice(cudaCurrDev)){
            vt_error_msg("[CUPTI Callbacks] Could not reset device in P2P mcyp!");
          }
          
          ENABLE_CUDART_CALLBACKS();
          ENABLE_CUDRV_CALLBACKS();

          vt_cupticb_handle_memcpyP2P(cbInfo, cuSrcCtx, cuDstCtx,
                    (uint64_t)(params->p->extent.height * params->p->extent.width * 
                               params->p->extent.depth), 
                                          time);
          return;
        }
        
        default: break;
      } /* switch(cbid) */
    } /* synchronization recording enabled */
  } /* if(vt_gpu_trace_memcpy) */
  
#if defined(VT_CUPTI_ACTIVITY)
  if(vt_cupti_events_enabled)
#endif
  {
    if(vt_gpu_trace_kernels){
      switch(cbid){

        /************* the CUDA runtime kernel configure call ************/
        case CUPTI_RUNTIME_TRACE_CBID_cudaConfigureCall_v3020: {
          if(vt_cupti_events_enabled && vt_gpu_trace_kernels && 
             cbInfo->callbackSite == CUPTI_API_EXIT)
            vt_cupticb_handle_cudart_knconf(cbInfo);

          return;
        }

        /***** the CUDA runtime kernel launch ******/
        case CUPTI_RUNTIME_TRACE_CBID_cudaLaunch_v3020: {
          if(vt_cupti_events_enabled && vt_gpu_trace_kernels)
            vt_cupticb_handle_cuda_kernel(cbInfo, NULL, 0);

          return;
        }
        
        default: break;
      }
    }
    /****************************************************************************/
      
    if(vt_gpu_trace_mcpy){
      switch(cbid){
        /******************** asynchronous memory copies **************************/
        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyAsync_v3020: {
          cudaMemcpyAsync_v3020_params *params = 
                (cudaMemcpyAsync_v3020_params *)cbInfo->functionParams;

          vt_cupticb_handle_cudart_mcpyAsync(cbInfo, params->kind, 
                                             (uint64_t)params->count, 
                                             params->stream);
          break;
        }

        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyToArrayAsync_v3020: {
          cudaMemcpyToArrayAsync_v3020_params *params = 
                  (cudaMemcpyToArrayAsync_v3020_params *)cbInfo->functionParams;

          vt_cupticb_handle_cudart_mcpyAsync(cbInfo, params->kind, 
                                             (uint64_t)params->count, 
                                             params->stream);
          break;
        }

        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyFromArrayAsync_v3020: {
          cudaMemcpyFromArrayAsync_v3020_params *params = 
                  (cudaMemcpyFromArrayAsync_v3020_params *)cbInfo->functionParams;

          vt_cupticb_handle_cudart_mcpyAsync(cbInfo, params->kind, 
                                          (uint64_t)params->count, 
                                          params->stream);
          break;
        }

        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy2DAsync_v3020: {
          cudaMemcpy2DAsync_v3020_params *params = 
                  (cudaMemcpy2DAsync_v3020_params *)cbInfo->functionParams;

          vt_cupticb_handle_cudart_mcpyAsync(cbInfo, params->kind, 
                                          (uint64_t)(params->height * params->width), 
                                          params->stream);
          break;
        }

        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy2DToArrayAsync_v3020: {
          cudaMemcpy2DToArrayAsync_v3020_params *params = 
                  (cudaMemcpy2DToArrayAsync_v3020_params *)cbInfo->functionParams;

          vt_cupticb_handle_cudart_mcpyAsync(cbInfo, params->kind, 
                                          (uint64_t)(params->height * params->width), 
                                          params->stream);
          break;
        }

        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy2DFromArrayAsync_v3020: {
          cudaMemcpy2DFromArrayAsync_v3020_params *params = 
                  (cudaMemcpy2DFromArrayAsync_v3020_params *)cbInfo->functionParams;

          vt_cupticb_handle_cudart_mcpyAsync(cbInfo, params->kind, 
                                          (uint64_t)(params->height * params->width), 
                                          params->stream);
          break;
        }

        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyToSymbolAsync_v3020: {
          cudaMemcpyToSymbolAsync_v3020_params *params = 
                  (cudaMemcpyToSymbolAsync_v3020_params *)cbInfo->functionParams;

          vt_cupticb_handle_cudart_mcpyAsync(cbInfo, params->kind, 
                                          (uint64_t)params->count, 
                                          params->stream);
          break;
        }

        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyFromSymbolAsync_v3020: {
          cudaMemcpyFromSymbolAsync_v3020_params *params = 
                  (cudaMemcpyFromSymbolAsync_v3020_params *)cbInfo->functionParams;

          vt_cupticb_handle_cudart_mcpyAsync(cbInfo, params->kind, 
                                          (uint64_t)params->count, 
                                          params->stream);
          break;
        }

        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy3DAsync_v3020: {
          cudaMemcpy3DAsync_v3020_params *params = 
                  (cudaMemcpy3DAsync_v3020_params *)cbInfo->functionParams;

          vt_cupticb_handle_cudart_mcpyAsync(cbInfo, params->p->kind, 
                    (uint64_t)(params->p->extent.height * params->p->extent.width * 
                               params->p->extent.depth), 
                                          params->stream);
          break;
        }
        /*
        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyPeerAsync_v4000: {
          cudaMemcpyPeerAsync_v4000_params *params = 
                (cudaMemcpyPeerAsync_v4000_params *)cbInfo->functionParams;

          vt_cupticb_handle_cudart_mcpyAsync(cbInfo, cudaMemcpyDeviceToDevice, 
                                          (uint64_t)params->count, 
                                          params->stream);
          break;
        }

        case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy3DPeerAsync_v4000: {
          cudaMemcpy3DPeerAsync_v4000_params *params = 
                  (cudaMemcpy3DPeerAsync_v4000_params *)cbInfo->functionParams;

          vt_cupticb_handle_cudart_mcpyAsync(cbInfo, cudaMemcpyDeviceToDevice, 
                    (uint64_t)(params->p->extent.height * params->p->extent.width * 
                               params->p->extent.depth), 
                                          params->stream);
          break;
        }*/
        /**************************************************************************/

        default: break;
      } /* switch(cbid) */
    } /* if(vt_gpu_trace_memcpy) */
  }
  /****************************************************************************/
}


#if (defined(CUPTI_API_VERSION) && (CUPTI_API_VERSION >= 2))
/*
 * This callback function is used to trace the CUDA runtime API.
 *
 * @param cbid the ID of the callback function in the given domain
 * @param cbInfo information about the callback
 */
void CUPTIAPI vt_cupticb_driverAPI(CUpti_CallbackId cbid, 
                                   const CUpti_CallbackData *cbInfo)
{
  uint32_t ptid;
  uint64_t time;
  uint32_t rid_func = VT_NO_ID;
  uint32_t hash_api_rid = VT_NO_ID;
  
  if(cbid == CUPTI_DRIVER_TRACE_CBID_INVALID) return;
  
  /* record cuCtxSynchronize in an extra function group */
  if(cbid == CUPTI_DRIVER_TRACE_CBID_cuCtxSynchronize){
    if(vt_gpu_sync_level > 1){
      uint64_t time = vt_pform_wtime();
      
      VT_CHECK_THREAD;
      ptid = VT_MY_THREAD;

      if(cbInfo->callbackSite == CUPTI_API_ENTER){
        vt_enter(ptid, &time, vt_gpu_rid_sync);
      }else if(cbInfo->callbackSite == CUPTI_API_EXIT){
        vt_exit(ptid, &time);
      }
      
      return;
    }
  }
  
  if(vt_cupticb_trace_driverAPI){
    VT_CHECK_THREAD;
    ptid = VT_MY_THREAD;

    /* get the VampirTrace region ID for the API function */
    hash_api_rid = vt_cupticb_cudaApiFuncGet(CUPTI_CB_DOMAIN_DRIVER_API, cbid);
    if(hash_api_rid != VT_NO_ID){
      rid_func = hash_api_rid;
    }else{
      rid_func = vt_def_region(VT_MASTER_THREAD, cbInfo->functionName, VT_NO_ID,
                               VT_NO_LNO, VT_NO_LNO, "CUDRV_API", VT_FUNCTION);

      vt_cupticb_cudaApiFuncPut(CUPTI_CB_DOMAIN_DRIVER_API, cbid, rid_func);
    }

    /*********** write enter and exit records for CUDA runtime API ************/
    time = vt_pform_wtime();
    if(cbInfo->callbackSite == CUPTI_API_ENTER){
      (void)vt_enter(ptid, &time, rid_func);
    }

    if(cbInfo->callbackSite == CUPTI_API_EXIT){
      vt_exit(ptid, &time);
    }  
  }
  
  if(vt_gpu_trace_mcpy){
#if defined(VT_CUPTI_ACTIVITY)
    if(vt_cupti_events_enabled || (vt_gpu_sync_level > 2 &&
     (vt_gpu_config & VT_GPU_TRACE_SYNC) == VT_GPU_TRACE_SYNC))
#endif
    {
      if(!vt_cupticb_trace_driverAPI)
        time = vt_pform_wtime();
      
      /****************** synchronous CUDA memory copies **********************/
      switch(cbid){
        case CUPTI_DRIVER_TRACE_CBID_cuMemcpy: {
          cuMemcpy_params *params = 
                (cuMemcpy_params *)cbInfo->functionParams;
          
          vt_cupti_handle_memcpyDefault(cbInfo, params->src, params->dst, 
                                        params->ByteCount, time);

          return;
        }
        
        case CUPTI_DRIVER_TRACE_CBID_cuMemcpy2D_v2: {
          cuMemcpy2D_v2_params *params = 
                (cuMemcpy2D_v2_params *)cbInfo->functionParams;
          
          size_t bytes = params->pCopy->WidthInBytes * params->pCopy->Height;
          
          vt_cupti_handle_memcpyDefault(cbInfo, params->pCopy->srcDevice,
                                        params->pCopy->dstDevice, time, bytes);
          
          return;
        }
        
        case CUPTI_DRIVER_TRACE_CBID_cuMemcpy3D_v2: {
          cuMemcpy3D_v2_params *params = 
                (cuMemcpy3D_v2_params *)cbInfo->functionParams;
          
          size_t bytes = params->pCopy->WidthInBytes * params->pCopy->Height
                       * params->pCopy->Depth;
          
          vt_cupti_handle_memcpyDefault(cbInfo, params->pCopy->srcDevice,
                                        params->pCopy->dstDevice, time, bytes);
          
          return;
        }
        
        case CUPTI_DRIVER_TRACE_CBID_cuMemcpyPeer: {
          cuMemcpyPeer_params *params = 
                (cuMemcpyPeer_params *)cbInfo->functionParams;
          
          size_t bytes = params->ByteCount;
          
          vt_cupticb_handle_memcpyP2P(cbInfo, 
                                      params->srcContext, params->dstContext,
                                      bytes, time);
          
          return;
        }
        
        case CUPTI_DRIVER_TRACE_CBID_cuMemcpy3DPeer: {
          cuMemcpy3DPeer_params *params = 
                (cuMemcpy3DPeer_params *)cbInfo->functionParams;
          
          size_t bytes = params->pCopy->WidthInBytes 
                       * params->pCopy->Depth * params->pCopy->Height;
          
          vt_cupticb_handle_memcpyP2P(cbInfo, params->pCopy->srcContext, 
                                      params->pCopy->dstContext,
                                      bytes, time);
          
          return;
        }

        case CUPTI_DRIVER_TRACE_CBID_cuMemcpyHtoD: {
          cuMemcpyHtoD_params *params = 
                (cuMemcpyHtoD_params *)cbInfo->functionParams;

          vt_cupticb_handle_memcpy(cbInfo, cudaMemcpyHostToDevice, 
                                   params->ByteCount, time);

          return;
        }

        case CUPTI_DRIVER_TRACE_CBID_cuMemcpyHtoD_v2: {
          cuMemcpyHtoD_v2_params *params = 
                (cuMemcpyHtoD_v2_params *)cbInfo->functionParams;

          vt_cupticb_handle_memcpy(cbInfo, cudaMemcpyHostToDevice, 
                                   params->ByteCount, time);

          return;
        }

        case CUPTI_DRIVER_TRACE_CBID_cuMemcpyDtoH: {
          cuMemcpyDtoH_params *params = 
                (cuMemcpyDtoH_params *)cbInfo->functionParams;

          vt_cupticb_handle_memcpy(cbInfo, cudaMemcpyDeviceToHost, 
                                   params->ByteCount, time);

          return;
        }

        case CUPTI_DRIVER_TRACE_CBID_cuMemcpyDtoH_v2: {
          cuMemcpyDtoH_v2_params *params = 
                (cuMemcpyDtoH_v2_params *)cbInfo->functionParams;

          vt_cupticb_handle_memcpy(cbInfo, cudaMemcpyDeviceToHost, 
                                   params->ByteCount, time);

          return;
        }

        case CUPTI_DRIVER_TRACE_CBID_cuMemcpyDtoD: {
          cuMemcpyDtoD_params *params = 
                (cuMemcpyDtoD_params *)cbInfo->functionParams;

          vt_cupticb_handle_memcpy(cbInfo, cudaMemcpyDeviceToDevice, 
                                   params->ByteCount, time);

          return;
        }

        case CUPTI_DRIVER_TRACE_CBID_cuMemcpyDtoD_v2: {
          cuMemcpyDtoD_v2_params *params = 
                (cuMemcpyDtoD_v2_params *)cbInfo->functionParams;

          vt_cupticb_handle_memcpy(cbInfo, cudaMemcpyDeviceToDevice, 
                                   params->ByteCount, time);

          return;
        }

        case CUPTI_DRIVER_TRACE_CBID_cuMemcpyHtoA: {
          cuMemcpyHtoA_params *params = 
                (cuMemcpyHtoA_params *)cbInfo->functionParams;

          vt_cupticb_handle_memcpy(cbInfo, cudaMemcpyHostToDevice, 
                                   params->ByteCount, time);

          return;
        }

        case CUPTI_DRIVER_TRACE_CBID_cuMemcpyHtoA_v2: {
          cuMemcpyHtoA_v2_params *params = 
                (cuMemcpyHtoA_v2_params *)cbInfo->functionParams;

          vt_cupticb_handle_memcpy(cbInfo, cudaMemcpyHostToDevice, 
                                   params->ByteCount, time);

          return;
        }

        case CUPTI_DRIVER_TRACE_CBID_cuMemcpyAtoH: {
          cuMemcpyAtoH_params *params = 
                (cuMemcpyAtoH_params *)cbInfo->functionParams;

          vt_cupticb_handle_memcpy(cbInfo, cudaMemcpyDeviceToHost, 
                                   params->ByteCount, time);

          return;
        }

        case CUPTI_DRIVER_TRACE_CBID_cuMemcpyAtoH_v2: {
          cuMemcpyAtoH_v2_params *params = 
                (cuMemcpyAtoH_v2_params *)cbInfo->functionParams;

          vt_cupticb_handle_memcpy(cbInfo, cudaMemcpyDeviceToHost, 
                                   params->ByteCount, time);

          return;
        }

        case CUPTI_DRIVER_TRACE_CBID_cuMemcpyAtoA: {
          cuMemcpyAtoA_params *params = 
                (cuMemcpyAtoA_params *)cbInfo->functionParams;

          vt_cupticb_handle_memcpy(cbInfo, cudaMemcpyDeviceToDevice, 
                                   params->ByteCount, time);

          return;
        }

        case CUPTI_DRIVER_TRACE_CBID_cuMemcpyAtoA_v2: {
          cuMemcpyAtoA_v2_params *params = 
                (cuMemcpyAtoA_v2_params *)cbInfo->functionParams;

          vt_cupticb_handle_memcpy(cbInfo, cudaMemcpyDeviceToDevice, 
                                   params->ByteCount, time);

          return;
        }

        default: break;
      } /* switch(cbid) */
    }
  }/* if(vt_gpu_trace_mcpy)*/
  
  /************* CUDA kernel launches *************/
#if defined(VT_CUPTI_ACTIVITY)
  if(vt_cupti_events_enabled)
#endif
  {
    if(vt_gpu_trace_kernels){
      switch(cbid){
        /*case CUPTI_DRIVER_TRACE_CBID_cuLaunch: {

            vt_cupticb_handle_cuda_kernel(cbInfo, NULL);

          break;
        }*/

        case CUPTI_DRIVER_TRACE_CBID_cuLaunchGrid: {
          cuLaunchGrid_params *params = 
                               (cuLaunchGrid_params *)cbInfo->functionParams;

          uint64_t blocks = params->grid_width;

          if(params->grid_height != 0) 
            blocks *= params->grid_height;

          vt_cupticb_handle_cuda_kernel(cbInfo, NULL, blocks);

          return;
        }

        case CUPTI_DRIVER_TRACE_CBID_cuLaunchGridAsync: {
          cuLaunchGridAsync_params *params = 
                               (cuLaunchGridAsync_params *)cbInfo->functionParams;

          uint64_t blocks = params->grid_width;

          if(params->grid_height != 0) 
            blocks *= params->grid_height;

          vt_cupticb_handle_cuda_kernel(cbInfo, params->hStream, blocks);

          return;
        }

        case CUPTI_DRIVER_TRACE_CBID_cuLaunchKernel: {
          cuLaunchKernel_params *params = 
                                  (cuLaunchKernel_params *)cbInfo->functionParams;

          uint64_t blocks = params->gridDimX;

          if(params->gridDimY != 0) 
            blocks *= params->gridDimY;

          if(params->gridDimZ != 0) 
            blocks *= params->gridDimZ;

          vt_cupticb_handle_cuda_kernel(cbInfo, params->hStream, blocks);

          return;
        }

        default: break;
      }
    }/* vt_gpu_trace_kernels */
  }
  
  if(vt_gpu_trace_memusage > 0){
    switch(cbid){
      /********************** CUDA memory allocation ****************************/
      case CUPTI_DRIVER_TRACE_CBID_cuMemAlloc: {
        if(cbInfo->callbackSite == CUPTI_API_EXIT){
          cuMemAlloc_params *params = 
                              (cuMemAlloc_params *)cbInfo->functionParams;

          vt_cupticb_handle_malloc(cbInfo->context,
                                   (uint64_t)*(params->dptr), 
                                   (size_t)params->bytesize);
        }

        break;
      }

      case CUPTI_DRIVER_TRACE_CBID_cuMemAlloc_v2: {
        if(cbInfo->callbackSite == CUPTI_API_EXIT){
          cuMemAlloc_v2_params *params = 
                              (cuMemAlloc_v2_params *)cbInfo->functionParams;

          vt_cupticb_handle_malloc(cbInfo->context,
                                   (uint64_t)*(params->dptr), 
                                   (size_t)params->bytesize);
        }

        break;
      }

      case CUPTI_DRIVER_TRACE_CBID_cuMemAllocPitch: {
        if(cbInfo->callbackSite == CUPTI_API_EXIT){
          cuMemAllocPitch_params *params = 
                              (cuMemAllocPitch_params *)cbInfo->functionParams;

          vt_cupticb_handle_malloc(cbInfo->context,
                                   (uint64_t)*(params->dptr), 
                                   (size_t)(params->Height * (*(params->pPitch))));
        }

        break;
      }

      case CUPTI_DRIVER_TRACE_CBID_cuMemAllocPitch_v2: {
        if(cbInfo->callbackSite == CUPTI_API_EXIT){
          cuMemAllocPitch_v2_params *params = 
                              (cuMemAllocPitch_v2_params *)cbInfo->functionParams;

          vt_cupticb_handle_malloc(cbInfo->context,
                                   (uint64_t)*(params->dptr), 
                                   (size_t)(params->Height * (*(params->pPitch))));
        }

        break;
      }

      case CUPTI_DRIVER_TRACE_CBID_cuArrayCreate: {
        if(cbInfo->callbackSite == CUPTI_API_EXIT){
          cuArrayCreate_params *params = 
                              (cuArrayCreate_params *)cbInfo->functionParams;
          size_t sizeInBytes = (size_t)(params->pAllocateArray->dummy);

          vt_cupticb_handle_malloc(cbInfo->context,
                                   (uint64_t)*(params->pHandle), 
                                   sizeInBytes);
        }

        break;
      }

      case CUPTI_DRIVER_TRACE_CBID_cuArrayCreate_v2: {
        if(cbInfo->callbackSite == CUPTI_API_EXIT){
          cuArrayCreate_v2_params *params = 
                              (cuArrayCreate_v2_params *)cbInfo->functionParams;
          size_t sizeInBytes = (size_t)(params->pAllocateArray->Width * 
                                        params->pAllocateArray->NumChannels);

          if(params->pAllocateArray->Height != 0)
            sizeInBytes *= params->pAllocateArray->Height;

          if(params->pAllocateArray->Format == CU_AD_FORMAT_UNSIGNED_INT16 ||
             params->pAllocateArray->Format == CU_AD_FORMAT_SIGNED_INT16 ||
             params->pAllocateArray->Format == CU_AD_FORMAT_HALF)
            sizeInBytes *= 2;

          if(params->pAllocateArray->Format == CU_AD_FORMAT_UNSIGNED_INT32 ||
             params->pAllocateArray->Format == CU_AD_FORMAT_SIGNED_INT32 ||
             params->pAllocateArray->Format == CU_AD_FORMAT_FLOAT)
            sizeInBytes *= 4;

          vt_cupticb_handle_malloc(cbInfo->context,
                                   (uint64_t)*(params->pHandle), 
                                   sizeInBytes);
        }

        break;
      }

      case CUPTI_DRIVER_TRACE_CBID_cuArray3DCreate: {
        if(cbInfo->callbackSite == CUPTI_API_EXIT){
          cuArray3DCreate_params *params = 
                              (cuArray3DCreate_params *)cbInfo->functionParams;
          size_t sizeInBytes = (size_t)(params->pAllocateArray->dummy);

          vt_cupticb_handle_malloc(cbInfo->context,
                                   (uint64_t)*(params->pHandle), 
                                   sizeInBytes);
        }

        break;
      }

      case CUPTI_DRIVER_TRACE_CBID_cuArray3DCreate_v2: {
        if(cbInfo->callbackSite == CUPTI_API_EXIT){
          cuArray3DCreate_v2_params *params = 
                              (cuArray3DCreate_v2_params *)cbInfo->functionParams;
          size_t sizeInBytes = (size_t)(params->pAllocateArray->Width *
                                        params->pAllocateArray->NumChannels);

          if(params->pAllocateArray->Height != 0) 
            sizeInBytes *= params->pAllocateArray->Height;

          if(params->pAllocateArray->Depth != 0)
            sizeInBytes *= params->pAllocateArray->Depth;

          if(params->pAllocateArray->Format == CU_AD_FORMAT_UNSIGNED_INT16 ||
             params->pAllocateArray->Format == CU_AD_FORMAT_SIGNED_INT16 ||
             params->pAllocateArray->Format == CU_AD_FORMAT_HALF)
            sizeInBytes *= 2;

          if(params->pAllocateArray->Format == CU_AD_FORMAT_UNSIGNED_INT32 ||
             params->pAllocateArray->Format == CU_AD_FORMAT_SIGNED_INT32 ||
             params->pAllocateArray->Format == CU_AD_FORMAT_FLOAT)
            sizeInBytes *= 4;

          vt_cupticb_handle_malloc(cbInfo->context,
                                   (uint64_t)*(params->pHandle), 
                                   sizeInBytes);
        }

        break;
      }

      case CUPTI_DRIVER_TRACE_CBID_cuMemFree: {
        if(cbInfo->callbackSite == CUPTI_API_ENTER){
          cuMemFree_params *params = 
                              (cuMemFree_params *)cbInfo->functionParams;

          vt_cupticb_handle_free(cbInfo->context, (uint64_t)params->dptr);
        }

        break;
      }

      case CUPTI_DRIVER_TRACE_CBID_cuMemFree_v2: {
        if(cbInfo->callbackSite == CUPTI_API_ENTER){
          cuMemFree_v2_params *params = 
                              (cuMemFree_v2_params *)cbInfo->functionParams;

          vt_cupticb_handle_free(cbInfo->context, (uint64_t)params->dptr);
        }

        break;
      }

      case CUPTI_DRIVER_TRACE_CBID_cuArrayDestroy: {
        if(cbInfo->callbackSite == CUPTI_API_ENTER){
          cuArrayDestroy_params *params = 
                              (cuArrayDestroy_params *)cbInfo->functionParams;

          vt_cupticb_handle_free(cbInfo->context, (uint64_t)params->hArray);
        }

        break;
      }

      default: break;
    }
  }
}

/*
 * This callback function is used to handle synchronization calls.
 *
 * @param cbid the ID of the callback function in the given domain
 * @param syncData synchronization data (CUDA context, CUDA stream)
 */
void vt_cupticb_sync(CUpti_CallbackId cbid, 
                     const CUpti_SynchronizeData *syncData)
{
  if(CUPTI_CBID_SYNCHRONIZE_CONTEXT_SYNCHRONIZED == cbid){
    vt_cntl_msg(3, "[CUPTI Callbacks] Synchronize called for CUDA context %d", 
                syncData->context);
    
#if defined(VT_CUPTI_ACTIVITY)
    if(!vt_cupti_events_enabled){
      VT_CUPTI_LOCK();
      vt_cuptiact_flushCtxActivities(vt_cupti_getCtxNoLock(syncData->context));
      VT_CUPTI_UNLOCK();
    }
#endif
  }
  
  /*if(CUPTI_CBID_SYNCHRONIZE_STREAM_SYNCHRONIZED == cbid){    
    vt_cntl_msg(2, "[CUPTI Callbacks] Stream synchronize called");
  }*/
}

/*
 * This callback function is used to handle resource usage.
 *
 * @param cbid the ID of the callback function in the given domain
 * @param resData resource information (CUDA context, CUDA stream)
 */
void vt_cupticb_resource(CUpti_CallbackId cbid, 
                         const CUpti_ResourceData *resData)
{  
  switch(cbid){
  /********************** CUDA memory allocation ******************************/
    case CUPTI_CBID_RESOURCE_CONTEXT_CREATED: {
      vt_cupti_ctx_t *vtCtx = NULL;
      CUcontext cuCtx = resData->context;
      
      vtCtx = vt_cupti_getCreateCtx(cuCtx);
      
      /* 
       * Create the VampirTrace CUPTI callbacks context for CUPTI >3
       * (needed for concurrent kernel tracing)
       */
#if (defined(CUPTI_API_VERSION) && (CUPTI_API_VERSION >= 3))
      if(vtCtx->callbacks == NULL)
        vtCtx->callbacks = vt_cupticb_createCbCtx(vtCtx);
#endif

      if(!vt_cupti_events_enabled){
        /* add the context without tracing CUDA driver API calls, if enabled */
        DISABLE_CUDRV_CALLBACKS();
        vt_cuptiact_setupActivityContext(vtCtx);
        ENABLE_CUDRV_CALLBACKS();
      }  
      
      break;
    }
    
    case CUPTI_CBID_RESOURCE_CONTEXT_DESTROY_STARTING: {
      vt_cntl_msg(2, "[CUPTI Callbacks] Destroying context ...");

      if(!vt_cupti_events_enabled){
        /* Only flush the activities of the context. The user code has to ensure, 
           that the context is synchronized */
        VT_CUPTI_LOCK();
        vt_cuptiact_flushCtxActivities(vt_cupti_getCtxNoLock(resData->context));
        VT_CUPTI_UNLOCK();
      }
      
      break;
    }
    
    case CUPTI_CBID_RESOURCE_STREAM_CREATED: {
      /*if(vt_gpu_stream_reuse){
        uint32_t strmID;
        
        VT_CUPTI_CALL(cuptiGetStreamId(resData->context, 
                                       resData->resourceHandle.stream, 
                                       &strmID), 
                      "cuptiGetStreamId");
        
        vt_cntl_msg(2, "[CUPTI Callbacks] Creating stream %d (context %d)", 
                       strmID, resData->context);
      }*/
      
#if (defined(CUPTI_API_VERSION) && (CUPTI_API_VERSION >= 3))
        if(vt_gpu_trace_kernels > 0 &&
           ((vt_gpu_config & VT_GPU_TRACE_CONCURRENT_KERNEL) 
            != VT_GPU_TRACE_CONCURRENT_KERNEL)){
          vt_cupti_ctx_t *vtCtx = vt_cupti_getCreateCtx(resData->context);
          
          /* if the device is capable of concurrent kernels */
          if(vtCtx->callbacks->concurrentKernels){
            /**/
            vtCtx->callbacks->streamsCreated++;

            vt_cntl_msg(2, "[CUPTI Callbacks] Creating stream %d (context %d)", 
                          vtCtx->callbacks->streamsCreated, resData->context);

            if(!vt_cupti_events_enabled && vtCtx->callbacks->streamsCreated > 1)
              vt_cuptiact_enableConcurrentKernel(vtCtx);
          }
        }
#endif
      
      break;
    }
    
    case CUPTI_CBID_RESOURCE_STREAM_DESTROY_STARTING: {
      if(vt_gpu_stream_reuse){
        uint32_t strmID;

#if (defined(CUDA_VERSION) && (CUDA_VERSION < 5000))
        /* implicitly flush context activities via cuCtxSynchronize() */
        {
          uint32_t ptid;
          uint64_t time;
          
          VT_CHECK_THREAD;
          ptid = VT_MY_THREAD;

          time = vt_pform_wtime();
          vt_enter(ptid, &time, vt_gpu_rid_sync);

          DISABLE_CUDRV_CALLBACKS();
          VT_CUDRV_CALL(cuCtxSynchronize(), NULL);
          ENABLE_CUDRV_CALLBACKS();

          time = vt_pform_wtime();
          vt_exit(ptid, &time);
        }
#else
        /* TODO: NVIDIA bug??? */
        /* cuCtxSynchronize() runs into a lock here, therefore just flush */
        if(!vt_cupti_events_enabled){
          VT_CUPTI_LOCK();
          vt_cuptiact_flushCtxActivities(vt_cupti_getCtx(resData->context));
          VT_CUPTI_UNLOCK();
        }
#endif
        if(!vt_cupti_events_enabled){
          /* get the stream id from stream type */
          VT_CUPTI_CALL(cuptiGetStreamId(resData->context, 
                                         resData->resourceHandle.stream, 
                                         &strmID), 
                        "cuptiGetStreamId");

          /* mark the stream as destroyed to be available for reuse */
          vt_cuptiact_markStreamAsDestroyed(resData->context, 
                                            strmID);
        }
        
        vt_cntl_msg(2, "[CUPTI Callbacks] Destroying stream %d (context %d)", 
                       strmID, resData->context);
      }
      
      break;
    }
    
    default: break;
  }
}
#endif

/*
 * Synchronize the current CUDA context and record the synchronization as 
 * necessary.
 * 
 * @param ptid the VampirTrace process/thread ID
 * 
 * @return VampirTrace time stamp after synchronization
 */
static uint64_t vt_cupticb_synchronizeCtx(uint32_t ptid)
{
  uint64_t time;
  
  /* check, if CUDA synchronize for activity flush is necessary */
  
  
  if(vt_cupticb_trace_driverAPI){
    VT_CUDRV_CALL(cuCtxSynchronize(), NULL);
    time = vt_pform_wtime();
  }else{
    if(vt_gpu_sync_level > 1){
      time = vt_pform_wtime();
      vt_enter(ptid, &time, vt_gpu_rid_sync);
    }

    VT_CUDRV_CALL(cuCtxSynchronize(), NULL);
    time = vt_pform_wtime();
    
    if(vt_gpu_sync_level > 1){
      vt_exit(ptid, &time);
    }
  }
  return time;
}

/*
 * This function handles the cudaConfigureCall callback.
 * Kernel configuration data are written on the kernel configure stack.
 * 
 * @param cbInfo information about the callback
 */
static void vt_cupticb_handle_cudart_knconf(const CUpti_CallbackData *cbInfo)
{
  /* configure call parameter have to be saved for kernel launch on a per 
   * thread basis. */
  vt_cupti_kernel_t *vtParams = NULL;
  cudaConfigureCall_v3020_params *params = 
         (cudaConfigureCall_v3020_params *) cbInfo->functionParams;
  vt_cupti_ctx_t *vtCtx = vt_cupti_getCreateCtx(cbInfo->context);
  vt_cupti_callbacks_t *vtCbCtx = 
#if defined(VT_CUPTI_ACTIVITY)
    vt_cupticb_createCbCtx(vtCtx);
#else
    vt_cupticb_createCbCtx(vtCtx, (CUstream)(params->stream));
#endif

  /* Is their already a kernel configured? */
  if(NULL == vtCbCtx->kernelData){ /* NO */
    /* allocate parameter memory for first kernel, if not yet done */
    vtParams = (vt_cupti_kernel_t *)malloc(sizeof(vt_cupti_kernel_t));
    if(vtParams == NULL){
      vt_error_msg("[CUPTI Callbacks] Could not allocate memory for "
                   "kernel parameter!");
    }
    vtParams->up = NULL;
    vtParams->down = NULL;
    
    vtCbCtx->kernelData = vtParams;
  }else{ /* YES */
    /* for the bottom element, which has been invalidated (reuse it) */
    if(vtCbCtx->kernelData->threadsPerBlock == VT_NO_ID){
      vtParams = vtCbCtx->kernelData;
    }else{
    
      /* there may have been some kernels configured (allocated) */
      if(NULL == vtCbCtx->kernelData->up){
        vtParams = (vt_cupti_kernel_t *)malloc(sizeof(vt_cupti_kernel_t));
        if(vtParams == NULL){
          vt_error_msg("[CUPTI Callbacks] Could not allocate memory for "
                       "kernel parameter!");
        }
        vtParams->up = NULL;
        vtParams->down = vtCbCtx->kernelData;
      }else{
        /* just use the already allocated kernel element */
        vtParams = vtCbCtx->kernelData->up;
      }

      /* set the current kernel */
      vtCbCtx->kernelData = vtParams;
    }
  }

  vtParams->blocksPerGrid = params->gridDim.x * params->gridDim.y
                          * params->gridDim.z;
  vtParams->threadsPerBlock = params->blockDim.x * params->blockDim.y 
                           * params->blockDim.z;

  vtParams->stream = params->stream;
}

/*
 * This function can be called at the beginning and end of a CUDA kernel launch.
 * Time stamps will be written to the corresponding CUDA stream.
 * !!! The kernel has to be configured (cudaConfigureCall) !!!

 * @param cbInfo information about the callback
 * @param cuStrm the CUDA stream
 * @param blocks number of blocks executed with this kernel
 */
static void vt_cupticb_handle_cuda_kernel(const CUpti_CallbackData *cbInfo, 
                                          CUstream cuStrm, uint64_t blocks)
{
  uint64_t time;
  
  if(cbInfo->callbackSite == CUPTI_API_ENTER){
    uint32_t knRID = VT_NO_ID;
    const char *symName = cbInfo->symbolName;
    vt_cupti_strm_t *vtStrm = NULL;
    vt_gpu_hn_string_t *hn = NULL;
    vt_cupti_ctx_t *vtCtx = NULL;
    vt_cupti_callbacks_t *vtCbCtx = NULL;
    
    if(cbInfo->symbolName == NULL) symName = "_Z7noSymbolName";

    /* get the VampirTrace region ID for the kernel */
    hn = vt_gpu_stringHashGet(symName);

    if(hn){
      knRID = hn->rid;
    }else{
      char *knName = NULL;

      knName = vt_cuda_demangleKernel(symName);
      
      if(knName == NULL || *knName == '\0') {
        knName = (char *)symName;

        if(knName == NULL) knName = "unknownKernel";
      }
      
      VT_CUPTI_LOCK();
      knRID = vt_def_region(VT_MASTER_THREAD, knName, VT_NO_ID,
                            VT_NO_LNO, VT_NO_LNO, "CUDA_KERNEL", VT_FUNCTION);
      VT_CUPTI_UNLOCK();

      hn = vt_gpu_stringHashPut(symName, knRID);
      /*hn->fname = knName;*/
    }

    /* get the VampirTrace CUPTI context the kernel is running on */
    if(blocks != 0){ /* if called from driver API launch */
      vtCtx = vt_cupti_getCreateCtx(cbInfo->context);
    }else{
      vtCtx = vt_cupti_getCtx(cbInfo->context);
    }
    
    if(NULL == vtCtx) {
      vt_warning("[CUPTI Callbacks] No context available!");
      return;
    }
    
    /* check if current host thread is the same as the context host thread */
    VT_CHECK_THREAD;
    if(vtCtx->ptid != VT_MY_THREAD){
      vt_warning("[CUPTI Callbacks] Host thread of context changed!");
      return;
    }
    
    vtCbCtx = vtCtx->callbacks;
    
    /* if called from driver API launch, the callback context may not be created */
    if(blocks != 0 && NULL == vtCbCtx){
      vtCbCtx = 
#if defined(VT_CUPTI_ACTIVITY)
        vt_cupticb_createCbCtx(vtCtx);
#else
        vt_cupticb_createCbCtx(vtCtx, cuStrm);
#endif
    }
    
    if(NULL == vtCbCtx){
      vt_warning("[CUPTI Callbacks] No callbacks context available!");
      return;
    }
    
    /* called from CUDA runtime API */
    if(NULL == vtCbCtx->kernelData && blocks == 0){
      vt_warning("[CUPTI Callbacks] No kernel parameter set! "
                 "cudaConfigureCall() failed?");
      return;
    }
    
    {
      uint32_t cuStrmID = VT_CUPTI_NO_STREAM_ID;

      if(blocks == 0){
        cuStrm = vtCbCtx->kernelData->stream;
      }
        
#if defined(VT_CUPTI_ACTIVITY)
        VT_CUPTI_CALL(cuptiGetStreamId(cbInfo->context, cuStrm, &cuStrmID), 
                      "cuptiGetStreamId");
#endif
        
        vtStrm = vt_cupti_getCreateStream(vtCtx, cuStrm, cuStrmID);
    }

    /* save address into 64 Bit correlation value for exit callback */
    *cbInfo->correlationData = (uint64_t)vtStrm;

#if defined(VT_CUPTI_EVENTS)
    if(vt_cupti_events_enabled && !vt_cupti_activity_isBufferEmpty(vtCtx->cuCtx)){
      /* write the event records */
      time = vt_cupticb_synchronizeCtx(vtCtx->ptid);
    }else
#endif
    {
      time = vt_pform_wtime();
    }
    
    /* write VT kernel start events */
    if(vt_gpu_trace_idle) vt_exit(vtCtx->strmList->vtThrdID, &time);
    vt_enter(vtStrm->vtThrdID, &time, knRID);

    if(vt_gpu_trace_kernels > 1){
      if(blocks == 0){
        vt_count(vtStrm->vtThrdID, &time, vt_cupti_cid_blocksPerGrid, 
                 vtCbCtx->kernelData->blocksPerGrid);
        vt_count(vtStrm->vtThrdID, &time, vt_cupti_cid_threadsPerBlock, 
                 vtCbCtx->kernelData->threadsPerBlock);
        vt_count(vtStrm->vtThrdID, &time, vt_cupti_cid_threadsPerKernel,
                 vtCbCtx->kernelData->threadsPerBlock * 
                 vtCbCtx->kernelData->blocksPerGrid);
      }else{
        vt_count(vtStrm->vtThrdID, &time, vt_cupti_cid_blocksPerGrid, blocks);
      }
    }
    
#if defined(VT_CUPTI_EVENTS)
    if(vt_cupti_events_enabled){
      vt_cupti_events_t *vtEvtCtx = vtCtx->events;
      
      if(NULL == vtEvtCtx) {
        vt_cupti_events_initContext(vtCtx);
        vtEvtCtx = vtCtx->events;
      }
      
      vt_cuptievt_resetCounter(vtEvtCtx, vtStrm->vtThrdID, &time);
    }
#endif

    /* Only for CUDA runtime API */
    if(blocks == 0){
      /* take the configure parameters from stack or invalidate it */
      if(NULL != vtCbCtx->kernelData->down)
        vtCbCtx->kernelData = vtCbCtx->kernelData->down;
      else
        /* use this parameter for invalidation */
        vtCbCtx->kernelData->threadsPerBlock = VT_NO_ID;
    }
  }

  if(cbInfo->callbackSite == CUPTI_API_EXIT){
    vt_cupti_strm_t *vtStrm = (vt_cupti_strm_t *)(*cbInfo->correlationData);
    uint32_t tid = vtStrm->vtThrdID;
    vt_cupti_ctx_t *vtCtx = vt_cupti_getCtx(cbInfo->context);
    
    if(NULL == vtCtx){
      vt_warning("[CUPTI Callbacks] No CUPTI context available!");
      return;
    }
    
    /* check if current host thread is the same as the context host thread */
    VT_CHECK_THREAD;
    if(vtCtx->ptid != VT_MY_THREAD){
      vt_warning("[CUPTI Callbacks] Host thread of context changed!");
      return;
    }
    
#if defined(VT_CUPTI_EVENTS)
    if(vt_cupti_events_enabled){
      if(NULL == vtCtx->events){
        vt_warning("[CUPTI Callbacks] No events context available!");
      }else{
        time = vt_pform_wtime();
        vt_enter(vtCtx->ptid, &time, vt_gpu_rid_sync);

        if(vt_cupti_events_sampling){
          CUresult ret = CUDA_SUCCESS;
          /* sampling of CUPTI counter values */
          do{
            time = vt_pform_wtime();
            vt_cuptievt_writeCounter(vtCtx->events, tid, &time);
            ret = cuStreamQuery(vtStrm->cuStrm);
          }while(ret != CUDA_SUCCESS);
        }else{
          /* synchronize context before 
             (assume that the given context is the current one) */
          time = vt_cupticb_synchronizeCtx(vtCtx->ptid);
        }

        vt_cuptievt_writeCounter(vtCtx->events, tid, &time);
        vt_exit(vtCtx->ptid, &time);
      } /* NULL != vtCtx->events */
    }else
#endif /* VT_CUPTI_EVENTS */
    {
      if(vt_gpu_sync_level > 0){
        time = vt_cupticb_synchronizeCtx(vtCtx->ptid);
      }
    }

    /* write VT kernel stop events */
    if(vt_gpu_trace_kernels > 1){
      vt_count(tid, &time, vt_cupti_cid_blocksPerGrid, 0);
      
      if(blocks == 0){
        vt_count(tid, &time, vt_cupti_cid_threadsPerBlock, 0);
        vt_count(tid, &time, vt_cupti_cid_threadsPerKernel, 0);
      }
    }

    vt_exit(tid, &time);

    if(vt_gpu_trace_idle){
      vt_enter(vtCtx->strmList->vtThrdID, &time, vt_gpu_rid_idle);
    }
  }
}

/* 
 * Create and add the default stream to the given VampirTrace CUPTI context 
 * 
 * @param vtCtx pointer to the VampirTrace CUPTI context
 */
static void vt_cupticb_createDefaultStream(vt_cupti_ctx_t *vtCtx)
{
  uint32_t cuStrmID = VT_CUPTI_NO_STREAM_ID;
    
  if(vt_gpu_init_time < vt_start_time)
    vt_gpu_init_time = vt_start_time;

#if defined(VT_CUPTI_ACTIVITY)        
  /* create a VampirTrace CUPTI stream */
  if(vtCtx->activity == NULL){
    VT_CUPTI_CALL(cuptiGetStreamId(vtCtx->cuCtx, NULL, &cuStrmID), 
                                   "cuptiGetStreamId");
  }else{
    cuStrmID = vtCtx->activity->defaultStrmID;
  }
#endif /* VT_CUPTI_ACTIVITY */

  /* this will create a valid VT stream object if !defined(VT_CUPTI_ACTIVITY) */
  vtCtx->strmList = vt_cupti_createStream(vtCtx, VT_CUPTI_NO_STREAM, 
                                          cuStrmID);
}

/*
 * Increases the "Allocated CUDA memory" counter.
 * 
 * @param cuCtx CUDA context
 * @param devPtr pointer to the allocated memory (needed for vtcudaFree())
 * @param size the number of bytes allocated
 */
static void vt_cupticb_handle_malloc(CUcontext cuCtx, 
                                     uint64_t address, size_t size)
{
  vt_cupti_gpumem_t *vtMalloc = NULL;
  vt_cupti_ctx_t *vtCtx = NULL;
  
  if(address == (uint64_t)NULL) 
    return;
  
  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);
  
  vtMalloc = (vt_cupti_gpumem_t*)malloc(sizeof(vt_cupti_gpumem_t));
  if(vtMalloc == NULL) 
    vt_error_msg("[CUPTI Callbacks] Could not allocate memory for GPU memory pointer!");
  
  /* set address and size of the allocated GPU memory */
  vtMalloc->address = address;
  vtMalloc->size = size;
  
  /* lock the work on the context */
  VT_CUPTI_LOCK();
  
  /* get the context without additional locks or malloc tracing checks */
  vtCtx = vt_cupti_getCtxNoLock(cuCtx);
  if(vtCtx == NULL){
    vtCtx = vt_cupti_createCtx(cuCtx, VT_CUPTI_NO_CUDA_DEVICE,
                               VT_CUPTI_NO_CONTEXT_ID, VT_CUPTI_NO_DEVICE_ID);
    vt_cupti_prependCtx(vtCtx);
  }
  
  /* add malloc entry to list */
  vtMalloc->next = vtCtx->gpuMemList;
  vtCtx->gpuMemList = vtMalloc;
  
  /* increase the context global allocated memory counter */
  vtCtx->gpuMemAllocated += size;

  /* check if first CUDA stream is available */
  if(vtCtx->strmList == NULL){
    vt_cupticb_createDefaultStream(vtCtx);
    vt_count(vtCtx->strmList->vtThrdID, &vt_gpu_init_time, vt_gpu_cid_memusage, 0);
  }
  
  VT_CUPTI_UNLOCK();
  
#if defined(VT_CUPTI_ACTIVITY)
# if defined(VT_CUPTI_EVENTS)
  if(!vt_cupti_events_enabled)
# endif
  {
    /* synchronize context before (implicit activity buffer flush)
       (assume that the given context is the current one) */
    if(!(vt_gpu_trace_kernels && vt_gpu_trace_mcpy) ||
       ((vt_gpu_trace_kernels || vt_gpu_trace_mcpy) && 
                                !vt_cupti_activity_isBufferEmpty(vtCtx->cuCtx)))
      vt_cupticb_synchronizeCtx(vtCtx->ptid);
  }
#endif /* VT_CUPTI_ACTIVITY */

  /* write counter value */
  {
    uint64_t vtTime = vt_pform_wtime();

    vt_count(vtCtx->strmList->vtThrdID, &vtTime, vt_gpu_cid_memusage, 
             (uint64_t)(vtCtx->gpuMemAllocated));
  }
  
  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
}

/*
 * Decreases the "Allocated CUDA memory" counter.
 *
 * @param cuCtx CUDA context
 * @param devPtr pointer to the allocated memory
 */
static void vt_cupticb_handle_free(CUcontext cuCtx, uint64_t devPtr)
{
  uint64_t vtTime;
  vt_cupti_ctx_t *vtCtx = NULL;
  vt_cupti_gpumem_t *curMalloc = NULL;
  vt_cupti_gpumem_t *lastMalloc = NULL;

  if(devPtr == (uint64_t)NULL) return;
  
  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);
  
  /* lock the work on the context */
  VT_CUPTI_LOCK();
  
  /* get the context without additional locks or malloc tracing checks */
  vtCtx = vt_cupti_getCtxNoLock(cuCtx);
  if(vtCtx == NULL){
    vtCtx = vt_cupti_createCtx(cuCtx, VT_CUPTI_NO_CUDA_DEVICE,
                               VT_CUPTI_NO_CONTEXT_ID, VT_CUPTI_NO_DEVICE_ID);
    vt_cupti_prependCtx(vtCtx);
  }
  
  VT_CUPTI_UNLOCK();
  
#if defined(VT_CUPTI_ACTIVITY)
# if defined(VT_CUPTI_EVENTS)
  if(!vt_cupti_events_enabled)
# endif
  {
    /* synchronize context before 
       (assume that the given context is the current one) */
    if(!(vt_gpu_trace_kernels && vt_gpu_trace_mcpy) ||
       ((vt_gpu_trace_kernels || vt_gpu_trace_mcpy) && 
                                !vt_cupti_activity_isBufferEmpty(vtCtx->cuCtx)))
      vt_cupticb_synchronizeCtx(vtCtx->ptid);
  }
#endif /* VT_CUPTI_ACTIVITY */

  VT_CUPTI_LOCK();
  curMalloc = vtCtx->gpuMemList;
  lastMalloc = vtCtx->gpuMemList;
  while(curMalloc != NULL){
    if(devPtr == curMalloc->address){

      /* decrease allocated counter value and write it */
      vtTime = vt_pform_wtime();
      vtCtx->gpuMemAllocated -= curMalloc->size;
      vt_count(vtCtx->strmList->vtThrdID, &vtTime, vt_gpu_cid_memusage,
               (uint64_t)(vtCtx->gpuMemAllocated));


      /* set pointer over current element to next one */
      lastMalloc->next = curMalloc->next;

      /* if current element is the first list entry, set the list entry */
      if(curMalloc == vtCtx->gpuMemList){
        vtCtx->gpuMemList = curMalloc->next;
      }

      /* free VT memory of CUDA malloc */
      curMalloc->next = NULL;
      free(curMalloc);
      curMalloc = NULL;

      /* set mallocList to NULL, if last element freed */
      if(vtCtx->gpuMemAllocated == 0) {
        vtCtx->gpuMemList = NULL;
      }
      
      VT_CUPTI_UNLOCK();
      VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
      return;
    }

    lastMalloc = curMalloc;
    curMalloc = curMalloc->next;
  }
  
  VT_CUPTI_UNLOCK();
  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);

  vt_warning("[CUPTI Callbacks] Free CUDA memory, which has not been allocated!");
}

/*
 * Retrieve the direction of CUDA memory copies, based on the source and 
 * destination memory type. 
 * (source: host, destination: device -> cudaMemcpyHostToDevice)
 * 
 * @param srcMemType memory type of source
 * @param dstMemType memory type of destination
 * 
 * @return kind the cudaMemcpyKind
 */
static enum cudaMemcpyKind vt_cupticb_getMemcpyKind(CUmemorytype srcMemType,
                                                    CUmemorytype dstMemType)
{  
  if(CU_MEMORYTYPE_HOST == srcMemType){
    if(CU_MEMORYTYPE_DEVICE == dstMemType || 
       CU_MEMORYTYPE_ARRAY == dstMemType){
      return cudaMemcpyHostToDevice;
    }else if(CU_MEMORYTYPE_HOST == dstMemType){
      return cudaMemcpyHostToHost;
    }
  }else{
    if(CU_MEMORYTYPE_DEVICE == srcMemType || 
       CU_MEMORYTYPE_ARRAY == srcMemType){
      if(CU_MEMORYTYPE_DEVICE == dstMemType || 
         CU_MEMORYTYPE_ARRAY == dstMemType){
        return cudaMemcpyDeviceToDevice;
      }else{
        if(CU_MEMORYTYPE_HOST == dstMemType){
          return cudaMemcpyDeviceToHost;
        }
      }
    }
  }
  
  return cudaMemcpyDefault;
}

/*
 * Handle synchronous CUDA memory copy calls.
 *
 * @param cbInfo information about the callback
 * @param kind direction of the data transfer
 * @param bytes number of bytes transfered
 * @param time the start/stop time of the synchronous transfer
 */
static void vt_cupticb_handle_memcpy(const CUpti_CallbackData *cbInfo,
                                     enum cudaMemcpyKind kind,
                                     uint64_t bytes, uint64_t time)
{
  uint32_t strmID;
  uint32_t ptid;
  
  VT_CHECK_THREAD;
  ptid = VT_MY_THREAD;

  if(cbInfo->callbackSite == CUPTI_API_ENTER){
    
#if defined(VT_CUPTI_ACTIVITY)
    /* disable activity API for memory copy recording */
    if(!vt_cupti_events_enabled){
      VT_CUPTI_CALL(cuptiActivityDisable(CUPTI_ACTIVITY_KIND_MEMCPY), 
                    "cuptiActivityDisable");
    }
#endif /* VT_CUPTI_ACTIVITY */
        
    /* get the VampirTrace thread ID the kernel is running on */
    {
      vt_cupti_ctx_t *vtCtx = vt_cupti_getCreateCtx(cbInfo->context);
      vt_cupti_strm_t *vtStrm = NULL;
      
      if(vtCtx->strmList == NULL){
        vt_cupticb_createDefaultStream(vtCtx);
      }
      
      vtStrm = vtCtx->strmList;
      strmID = vtStrm->vtThrdID;

      /* save address into 64 Bit correlation value for exit callback */
      *cbInfo->correlationData = (uint64_t)vtStrm;
      
      /* check if current host thread is the same as the context host thread */
      if(vtCtx->ptid != ptid){
        vt_warning("[CUPTI Callbacks] Host thread of context changed! "
                   "Skipping memory copy!");
        return;
      }
      
      /* synchronize to get host waiting time */
      if(vt_gpu_sync_level > 0){
        if( !vt_gpu_trace_kernels || 
            !vt_cupti_activity_isBufferEmpty(vtCtx->cuCtx) )
          time = vt_cupticb_synchronizeCtx(vtCtx->ptid);
      }
      
      /* pure idle time */
      if(vt_gpu_trace_idle == 2){
#if defined(VT_CUPTI_ACTIVITY)
        if(NULL != vtCtx->activity){
          if(vtCtx->activity->gpuIdleOn){
            vt_exit(vtCtx->strmList->vtThrdID, &time);
            vtCtx->activity->gpuIdleOn = 0;
          }
        }else
#endif /* VT_CUPTI_ACTIVITY */
        {
          vt_exit(vtCtx->strmList->vtThrdID, &time);
        }
      }
    }

    VT_CUPTI_LOCK();
    if(kind != cudaMemcpyDeviceToDevice) vt_gpu_prop[ptid] |= VTGPU_GPU_COMM;
    vt_gpu_prop[strmID] |= VTGPU_GPU_COMM;
    VT_CUPTI_UNLOCK();

    /*time = vt_pform_wtime();*/
    if(kind == cudaMemcpyHostToDevice){
      vt_mpi_rma_put(ptid, &time, VT_GPU_RANK_ID(strmID),
                     vt_gpu_commCID, 0, bytes);
    }else if(kind == cudaMemcpyDeviceToHost){
      vt_mpi_rma_get(ptid, &time, VT_GPU_RANK_ID(strmID),
                     vt_gpu_commCID, 0, bytes);
    }else if(kind == cudaMemcpyDeviceToDevice){
      vt_mpi_rma_get(strmID, &time, VT_GPU_RANK_ID(strmID),
                     vt_gpu_commCID, 0, bytes);
    }
  }

  if(cbInfo->callbackSite == CUPTI_API_EXIT){
    strmID = ((vt_cupti_strm_t *)(*cbInfo->correlationData))->vtThrdID;

    /*time = vt_pform_wtime(); */
    if(kind == cudaMemcpyDeviceToDevice){
      vt_mpi_rma_end(strmID, &time, vt_gpu_commCID, 0);
    }else if(kind != cudaMemcpyHostToHost){
      vt_mpi_rma_end(ptid, &time, vt_gpu_commCID, 0);
    }
    
    /* pure idle time */
    if(vt_gpu_trace_idle == 2){
      vt_cupti_ctx_t *vtCtx = vt_cupti_getCreateCtx(cbInfo->context);
      
      if(vtCtx->strmList != NULL){
#if defined(VT_CUPTI_ACTIVITY)
        if(NULL != vtCtx->activity){
          if(!vtCtx->activity->gpuIdleOn){
            vt_enter(vtCtx->strmList->vtThrdID, &time, vt_gpu_rid_idle);
            vtCtx->activity->gpuIdleOn = 1;
          }
        }else
  #endif /* VT_CUPTI_ACTIVITY */
        {
          vt_enter(vtCtx->strmList->vtThrdID, &time, vt_gpu_rid_idle);
        }
      }
    }
    
#if defined(VT_CUPTI_ACTIVITY)
    /* enable activity API for memory copy recording */
    if(!vt_cupti_events_enabled){
      VT_CUPTI_CALL(cuptiActivityEnable(CUPTI_ACTIVITY_KIND_MEMCPY), 
                    "cuptiActivityEnable");
    }
#endif
  }
}

/*
 * Handle synchronous Peer-toPeer CUDA memory copy calls.
 *
 * @param cbInfo information about the callback
 * @param cuSrcCtx CUDA context of source memory
 * @param cuDstCtx CUDA context of destination memory
 * @param bytes number of bytes transfered
 * @param time the start/stop time of the synchronous transfer
 */
static void vt_cupticb_handle_memcpyP2P(const CUpti_CallbackData *cbInfo,
                                        CUcontext cuSrcCtx,
                                        CUcontext cuDstCtx,
                                        uint64_t bytes, uint64_t time)
{
  if(cbInfo->callbackSite == CUPTI_API_ENTER){
    uint64_t time;
    vt_cupti_ctx_t *vtSrcCtx = vt_cupti_getCreateCtx(cuSrcCtx);
    vt_cupti_ctx_t *vtDstCtx = vt_cupti_getCreateCtx(cuDstCtx);
    
#if defined(VT_CUPTI_ACTIVITY)
    /* disable activity API for memory copy recording */
    if(!vt_cupti_events_enabled){
      VT_CUPTI_CALL(cuptiActivityDisable(CUPTI_ACTIVITY_KIND_MEMCPY), 
                    "cuptiActivityDisable");
    }
#endif /* VT_CUPTI_ACTIVITY */
    
    if(vtSrcCtx->strmList == NULL){
      vt_cupticb_createDefaultStream(vtSrcCtx);
    }
    
    if(vtDstCtx->strmList == NULL){
      vt_cupticb_createDefaultStream(vtDstCtx);
    }

    *(cbInfo->correlationData) = VT_NO_ID;

    if(cbInfo->context == cuSrcCtx){
      *(cbInfo->correlationData) = vtSrcCtx->strmList->vtThrdID;

      if(!vt_cupti_activity_isBufferEmpty(cuSrcCtx))
        VT_CUDRV_CALL(cuCtxSynchronize(), NULL);
      
      DISABLE_CUDRV_CALLBACKS();
      VT_CUDRV_CALL(cuCtxSetCurrent(cuDstCtx), NULL);
      ENABLE_CUDRV_CALLBACKS();
      
      if(!vt_cupti_activity_isBufferEmpty(cuDstCtx))
        VT_CUDRV_CALL(cuCtxSynchronize(), NULL);

      DISABLE_CUDRV_CALLBACKS();
      VT_CUDRV_CALL(cuCtxSetCurrent(cuSrcCtx), NULL);
      ENABLE_CUDRV_CALLBACKS();
      
      time = vt_pform_wtime();
      
      /* write put in source context */
      vt_mpi_rma_put(vtSrcCtx->strmList->vtThrdID, &time, 
                     VT_GPU_RANK_ID(vtDstCtx->strmList->vtThrdID),
                     vt_gpu_commCID, 0, bytes);
    }else if(cbInfo->context == cuDstCtx){
      *(cbInfo->correlationData) = vtDstCtx->strmList->vtThrdID;

      if(!vt_cupti_activity_isBufferEmpty(cuDstCtx))
        VT_CUDRV_CALL(cuCtxSynchronize(), NULL);
      
      DISABLE_CUDRV_CALLBACKS();
      VT_CUDRV_CALL(cuCtxSetCurrent(cuSrcCtx), NULL);
      ENABLE_CUDRV_CALLBACKS();
      
      if(!vt_cupti_activity_isBufferEmpty(cuSrcCtx))
        VT_CUDRV_CALL(cuCtxSynchronize(), NULL);
      
      DISABLE_CUDRV_CALLBACKS();
      VT_CUDRV_CALL(cuCtxSetCurrent(cuDstCtx), NULL);
      ENABLE_CUDRV_CALLBACKS();

      time = vt_pform_wtime();
      
      /* write get in destination context */
      vt_mpi_rma_get(vtDstCtx->strmList->vtThrdID, &time, 
                     VT_GPU_RANK_ID(vtSrcCtx->strmList->vtThrdID),
                     vt_gpu_commCID, 0, bytes);
    }else
      return;
    
    /* pure idle time */
    if(vt_gpu_trace_idle == 2){
#if defined(VT_CUPTI_ACTIVITY)
      if(NULL != vtSrcCtx->activity){
        if(vtSrcCtx->activity->gpuIdleOn){
          vt_exit(vtSrcCtx->strmList->vtThrdID, &time);
          vtSrcCtx->activity->gpuIdleOn = 0;
        }
      }else
#endif /* VT_CUPTI_ACTIVITY */
      {
        vt_exit(vtSrcCtx->strmList->vtThrdID, &time);
      }
#if defined(VT_CUPTI_ACTIVITY)
      if(NULL != vtDstCtx->activity){
        if(vtDstCtx->activity->gpuIdleOn){
          vt_exit(vtDstCtx->strmList->vtThrdID, &time);
          vtDstCtx->activity->gpuIdleOn = 0;
        }
      }else
#endif /* VT_CUPTI_ACTIVITY */
      {
        vt_exit(vtDstCtx->strmList->vtThrdID, &time);
      }
    }
  }else if(cbInfo->callbackSite == CUPTI_API_EXIT){
    /* if communication put/get has been written, then write the 
       rma_end on current context */
    if(*(cbInfo->correlationData) != VT_NO_ID){
      vt_mpi_rma_end(*(cbInfo->correlationData), 
                     &time, vt_gpu_commCID, 0);
    
    
      /* pure idle time */
      if(vt_gpu_trace_idle == 2){
        vt_cupti_ctx_t *vtSrcCtx = vt_cupti_getCreateCtx(cuSrcCtx);
        vt_cupti_ctx_t *vtDstCtx = vt_cupti_getCreateCtx(cuDstCtx);
        
        /* source context */
#if defined(VT_CUPTI_ACTIVITY)
        if(vtSrcCtx->strmList != NULL){
          if(NULL != vtSrcCtx->activity){
            if(!vtSrcCtx->activity->gpuIdleOn){
              vt_enter(vtSrcCtx->strmList->vtThrdID, &time, vt_gpu_rid_idle);
              vtSrcCtx->activity->gpuIdleOn = 1;
            }
          }else
#endif /* VT_CUPTI_ACTIVITY */
          {
            vt_enter(vtSrcCtx->strmList->vtThrdID, &time, vt_gpu_rid_idle);
          }
        }
        
        /* destination context */
#if defined(VT_CUPTI_ACTIVITY)
        if(vtDstCtx->strmList != NULL){
          if(NULL != vtDstCtx->activity){
            if(!vtDstCtx->activity->gpuIdleOn){
              vt_enter(vtDstCtx->strmList->vtThrdID, &time, vt_gpu_rid_idle);
              vtDstCtx->activity->gpuIdleOn = 1;
            }
          }else
#endif /* VT_CUPTI_ACTIVITY */
          {
            vt_enter(vtDstCtx->strmList->vtThrdID, &time, vt_gpu_rid_idle);
          }
        }
        
      }
    }
    
#if defined(VT_CUPTI_ACTIVITY)
    /* enable activity API for memory copy recording */
    if(!vt_cupti_events_enabled){
      VT_CUPTI_CALL(cuptiActivityEnable(CUPTI_ACTIVITY_KIND_MEMCPY), 
                    "cuptiActivityEnable");
    }
#endif
  }
}

/*
 * Handle CUDA memory copies with the flag cudaMemcpyDefault
 * 
 * @param cbInfo information about the callback
 * @param cuSrcDevPtr CUDA source device pointer
 * @param cuDstDevPtr CUDA destination device pointer
 * @param bytes number of bytes to be transfered
 * @param time the enter or exit timestamp of the memory copy API function
 */
static void vt_cupti_handle_memcpyDefault(const CUpti_CallbackData *cbInfo, 
                                          CUdeviceptr cuSrcDevPtr,
                                          CUdeviceptr cuDstDevPtr, 
                                          uint64_t bytes, uint64_t time)
{
  CUcontext cuSrcCtx;
  CUcontext cuDstCtx;
  CUmemorytype srcMemType;
  CUmemorytype dstMemType;
  enum cudaMemcpyKind kind = cudaMemcpyDefault;

  /* do not trace these CUDA driver API function calls */
  DISABLE_CUDRV_CALLBACKS();
  
  cuPointerGetAttribute(&cuSrcCtx, CU_POINTER_ATTRIBUTE_CONTEXT, 
                        cuSrcDevPtr);
  cuPointerGetAttribute(&cuDstCtx, CU_POINTER_ATTRIBUTE_CONTEXT, 
                        cuDstDevPtr);

  cuPointerGetAttribute(&srcMemType, CU_POINTER_ATTRIBUTE_MEMORY_TYPE, 
                        cuSrcDevPtr);
  cuPointerGetAttribute(&dstMemType, CU_POINTER_ATTRIBUTE_MEMORY_TYPE, 
                        cuDstDevPtr);
  
  ENABLE_CUDRV_CALLBACKS();
  /* reset time, due to tracing of cuPointerGetAttribute() 
  time = vt_pform_wtime();*/

  /* get memory copy direction */
  kind = vt_cupticb_getMemcpyKind(srcMemType, dstMemType);

  if(kind == cudaMemcpyDefault){
    vt_warning("[CUPTI Callbacks] Could not determine memory copy kind! "
               "Skipping this memory copy!");
    return;
  }

  /* if device<->host the context should be the current one */

  /* check for peer-to-peer memory copy */
  if(cuSrcCtx != cuDstCtx){
    if(kind == cudaMemcpyDeviceToDevice){
      vt_cupticb_handle_memcpyP2P(cbInfo, cuSrcCtx, cuDstCtx,
                                       bytes, time);
    }else{
      /* device <-> host memory copies can be written on the current context, 
       * even if the device memory in a different context, than the host memory */
      /*if(cbInfo->context == cuSrcCtx)*/
        vt_cupticb_handle_memcpy(cbInfo, kind, bytes, time);
      /*else if(cbInfo->context == cuDstCtx)
        vt_cupticb_handle_cuda_memcpy(cbInfo, kind, bytes, time);*/
    }
  }else{
    if(cbInfo->context == cuSrcCtx){
      vt_cupticb_handle_memcpy(cbInfo, kind, bytes, time);
    }else if(cbInfo->context == cuDstCtx){
      /* switch memory copy direction*/
      if(kind == cudaMemcpyDeviceToHost){
        kind = cudaMemcpyHostToDevice;
      }else if(kind == cudaMemcpyHostToDevice){
        kind = cudaMemcpyDeviceToHost;
      }
        
      vt_cupticb_handle_memcpy(cbInfo, kind, bytes, time);
    }else{
      vt_warning("[CUPTI Callbacks] Memory copy within context skipped! "
                 "(kind=%d)", kind);
    }
  }
}

/*
 * Handle asynchronous CUDA runtime memory copy calls.
 *
 * @param cbInfo information about the callback
 * @param kind the direction of the transfer
 * @param bytes the number of transfered bytes
 * @param cuStrm the CUDA stream
 */
static void vt_cupticb_handle_cudart_mcpyAsync(const CUpti_CallbackData *cbInfo,
                                               enum cudaMemcpyKind kind,
                                               uint64_t bytes,
                                               cudaStream_t cuStrm)
{
  uint32_t strmID;
  uint32_t ptid;
  uint64_t time;
  
  VT_CHECK_THREAD;
  ptid = VT_MY_THREAD;

  if(cbInfo->callbackSite == CUPTI_API_ENTER){
    /* get the VampirTrace thread ID the kernel is running on */
    {
      vt_cupti_ctx_t *vtCtx = vt_cupti_getCreateCtx(cbInfo->context);
      vt_cupti_strm_t *vtStrm = NULL;
      uint32_t cuStrmID = VT_CUPTI_NO_STREAM_ID;
    
      if(vt_gpu_init_time < vt_start_time)
        vt_gpu_init_time = vt_start_time;

#if defined(VT_CUPTI_ACTIVITY)
      if(vtCtx->activity == NULL){
        VT_CUPTI_CALL(cuptiGetStreamId(cbInfo->context, cuStrm, &cuStrmID), 
                                       "cuptiGetStreamId");
      }else{
        cuStrmID = vtCtx->activity->defaultStrmID;
      }
#endif /* VT_CUPTI_ACTIVITY */

      vtStrm = vt_cupti_getCreateStream(vtCtx, cuStrm, cuStrmID);
      
      strmID = vtStrm->vtThrdID;

      /* save address into 64 Bit correlation value for exit callback */
      *cbInfo->correlationData = (uint64_t)vtStrm;
    }

    VT_CUPTI_LOCK();
    if(kind != cudaMemcpyDeviceToDevice) vt_gpu_prop[ptid] |= VTGPU_GPU_COMM;
    vt_gpu_prop[strmID] |= VTGPU_GPU_COMM;
    VT_CUPTI_UNLOCK();

    time = vt_pform_wtime();
    if(kind == cudaMemcpyHostToDevice){
      vt_mpi_rma_put(ptid, &time, VT_GPU_RANK_ID(strmID),
                     vt_gpu_commCID, 0, bytes);
    }else if(kind == cudaMemcpyDeviceToHost){
      vt_mpi_rma_get(ptid, &time, VT_GPU_RANK_ID(strmID),
                     vt_gpu_commCID, 0, bytes);
    }else if(kind == cudaMemcpyDeviceToDevice){
      vt_mpi_rma_get(strmID, &time, VT_GPU_RANK_ID(strmID),
                     vt_gpu_commCID, 0, bytes);
    }
  }

  if(cbInfo->callbackSite == CUPTI_API_EXIT){
    strmID = ((vt_cupti_strm_t *)(*cbInfo->correlationData))->vtThrdID;

    /* synchronize to get host waiting time */
    if(vt_gpu_sync_level > 0){
      time = vt_cupticb_synchronizeCtx(ptid);
    }else{
      time = vt_pform_wtime();
    }

    if(kind == cudaMemcpyDeviceToDevice){
      vt_mpi_rma_end(strmID, &time, vt_gpu_commCID, 0);
    }else if(kind != cudaMemcpyHostToHost){
      vt_mpi_rma_end(ptid, &time, vt_gpu_commCID, 0);
    }
  }
}

/* -------------START: Implementation of public functions ------------------ */
/* ------------------------------------------------------------------------- */

/**
 * Initialize the VampirTrace CUPTI callback implementation.
 */
void vt_cupti_callback_init()
{
  if(!vt_cupticb_initialized){
    vt_cupti_init();
    VT_CUPTI_LOCK();
    if(!vt_cupticb_initialized){
      
      vt_cntl_msg(2, "[CUPTI Callbacks] Initializing ... ");
      
      /* check the CUDA APIs to be traced */
      vt_cupticb_trace_driverAPI = 0;
      vt_cupticb_trace_runtimeAPI = 0;
      
      /* check whether VT_GPUTRACE is set */
      if(vt_gpu_get_config() != 0){
        
        /* check for CUDA runtime API */
        if((vt_gpu_config & VT_GPU_TRACE_RUNTIME_API) == VT_GPU_TRACE_RUNTIME_API){
          vt_cupticb_trace_runtimeAPI = 1;
        }
        
        /* check for CUDA driver API */
        if((vt_gpu_config & VT_GPU_TRACE_DRIVER_API) == VT_GPU_TRACE_DRIVER_API){
          vt_cupticb_trace_driverAPI = 1;
        }
      
#if defined(VT_CUPTI_EVENTS)
        /* check for CUPTI events */
        if(vt_gpu_trace_kernels  && vt_env_cupti_events() != NULL){
          vt_cupti_events_enabled = 1;
        }else{
          vt_cupti_events_enabled = 0;
        }
#endif

        /* set callback for CUDA API functions */
#if defined(VT_CUPTI_ACTIVITY)  
        if(vt_cupticb_trace_runtimeAPI){
          vt_cupti_set_callback(vt_cupticb_all_ptr, 
                                CUPTI_CB_DOMAIN_RUNTIME_API,
                                CUPTI_RUNTIME_TRACE_CBID_INVALID);
        }

        if(vt_cupticb_trace_driverAPI || 
           (!vt_cupticb_trace_runtimeAPI && vt_gpu_trace_memusage) || 
           (!vt_cupticb_trace_runtimeAPI && vt_cupti_events_enabled) ||
           (!vt_cupticb_trace_runtimeAPI && vt_gpu_trace_mcpy && 
            vt_gpu_sync_level > 2)){
          vt_cupti_set_callback(vt_cupticb_all_ptr, 
                                CUPTI_CB_DOMAIN_DRIVER_API,
                                CUPTI_DRIVER_TRACE_CBID_INVALID);
        }
#else
        if(vt_cupticb_trace_runtimeAPI){
          vt_cupti_set_callback(vt_cupticb_cudart_ptr, 
                                CUPTI_CB_DOMAIN_RUNTIME_API,
                                CUPTI_RUNTIME_TRACE_CBID_INVALID);
        }
#endif

        /* reset the hash table for CUDA API functions */
        memset(vt_cupticb_cudaApiFuncTab, VT_NO_ID, 
               VT_CUPTICB_CUDA_API_FUNC_MAX * sizeof(uint32_t));

        /* if GPU streams are necessary */
        if(vt_gpu_trace_kernels > 0 || vt_gpu_trace_mcpy || vt_gpu_trace_memusage > 0){
#if (defined(VT_MT) || defined(VT_HYB))
          VTTHRD_LOCK_IDS();
#endif
          /* initialize GPU common stuff */
          vt_gpu_init();

          /* get global counter group IDs */
          if(vt_gpu_trace_kernels > 1){
            vt_cupti_cgid_cuda_kernel = 
                              vt_def_counter_group(VT_MASTER_THREAD, "CUDA_KERNEL");

            vt_cupti_cid_blocksPerGrid = vt_def_counter(VT_MASTER_THREAD, 
                          "blocks_per_grid", "#",
                          VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED, 
                          vt_cupti_cgid_cuda_kernel, 0);
            vt_cupti_cid_threadsPerBlock = vt_def_counter(VT_MASTER_THREAD, 
                          "threads_per_block", "#",
                          VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED, 
                          vt_cupti_cgid_cuda_kernel, 0);
            vt_cupti_cid_threadsPerKernel = vt_def_counter(VT_MASTER_THREAD, 
                          "threads_per_kernel", "#",
                          VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED, 
                          vt_cupti_cgid_cuda_kernel, 0);
          }

          vt_gpu_rid_sync = vt_def_region(VT_MASTER_THREAD, "cudaSynchronize", 
                        VT_NO_ID, VT_NO_LNO, VT_NO_LNO, "CUDA_SYNC", VT_FUNCTION);

#if (defined(VT_MT) || defined(VT_HYB))
          VTTHRD_UNLOCK_IDS();
#endif

#if defined(VT_CUPTI_ACTIVITY)
          vt_cupti_set_callback(vt_cupticb_all_ptr, 
                                CUPTI_CB_DOMAIN_RESOURCE,
                                CUPTI_RUNTIME_TRACE_CBID_INVALID);

#if defined(VT_CUPTI_EVENTS)
          if(!vt_cupti_events_enabled)
#endif
          {
            vt_cupti_activity_init();
          }

          if(vt_gpu_trace_kernels > 0 || vt_gpu_trace_mcpy){
            vt_cupti_set_callback(vt_cupticb_all_ptr, 
                                  CUPTI_CB_DOMAIN_SYNCHRONIZE,
                                  CUPTI_RUNTIME_TRACE_CBID_INVALID);
          }
#endif

          /* reset the GPU idle start time */
          if(vt_gpu_trace_idle) 
            vt_gpu_init_time = vt_pform_wtime();

        }/* vt_gpu_trace_kernels || vt_gpu_trace_mcpy || vt_gpu_trace_memusage */
      } /* vt_gpu_get_config() != 0 */
      
      /* register the finalize function of VampirTrace CUPTI to be called before
        * the program exits */
      atexit(vt_cupti_callback_finalize);

      vt_cupticb_initialized = 1;        
    } /* !vt_cupticb_initialized */
    
    VT_CUPTI_UNLOCK();
    
    /* initialize CUPTI events (uses CUPTI locks)*/
#if defined(VT_CUPTI_EVENTS)
    if(vt_cupti_events_enabled){
      vt_cupti_events_init();

      /* TODO: check exit handler problems with CUPTI events */
      vt_gpu_debug = 1;
    }
#endif
    
  } /* !vt_cupticb_initialized */
}

/**
 * Finalize the VampirTrace CUPTI callback implementation.
 */
void vt_cupti_callback_finalize()
{
  if(!vt_cupticb_finalized && vt_cupticb_initialized){
    VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

    VT_CUPTI_LOCK();
    if(!vt_cupticb_finalized && vt_cupticb_initialized){

      vt_cntl_msg(2, "[CUPTI Callbacks] Finalizing ... ");
      
      if(vt_cupticb_trace_runtimeAPI || vt_cupticb_trace_driverAPI ||
         vt_gpu_trace_kernels > 0 || vt_gpu_trace_mcpy || 
         vt_gpu_trace_memusage > 0){
        VT_CUPTI_CALL(cuptiUnsubscribe(vt_cupticb_subscriber), 
                      "cuptiUnsubscribe");
      }

      /* clean up the VampirTrace CUPTI context list */
      while(vt_cupti_ctxList != NULL){
        vt_cupti_ctx_t *vtCtx = vt_cupti_ctxList;

        vt_cupti_ctxList = vt_cupti_ctxList->next;
        
#if defined(VT_CUPTI_EVENTS)
        if(vt_cupti_events_enabled)
          vt_cupti_events_finalizeContext(vtCtx);
#endif

#if defined(VT_CUPTI_ACTIVITY)
        if(vt_gpu_trace_kernels > 0 || vt_gpu_trace_mcpy || 
           vt_gpu_trace_memusage > 0){
          vt_cupti_activity_finalizeContext(vtCtx);
        }
#endif
        
        vt_cupti_callbacks_finalizeContext(vtCtx);
        
        /* this will free the allocated memory of the context as well */
        vt_cupti_finalizeCtx(vtCtx);

        vtCtx = NULL;
      }
      
      vt_gpu_finalize();
      
      if(vt_gpu_trace_kernels > 0)
        vt_gpu_stringhashClear();
      
      vt_cupticb_finalized = 1;
      VT_CUPTI_UNLOCK();
      
#if defined(VT_CUPTI_EVENTS)
      if(vt_cupti_events_enabled) 
        vt_cupti_events_finalize();
#endif
      
#if defined(VT_CUPTI_ACTIVITY)
      if(vt_gpu_trace_kernels > 0 || vt_gpu_trace_mcpy || 
           vt_gpu_trace_memusage > 0){
        vt_cupti_activity_finalize();
      }
#endif
      
      vt_cupti_finalize();
    }
    
    VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
  }
}
