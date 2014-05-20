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

#include "vt_gpu.h"
#include "vt_env.h"         /* get environment variables */
#include "vt_pform.h"       /* VampirTrace time measurement */
#include "vt_mallocwrap.h"  /* wrapping of malloc and free */

#include <string.h> /* needed for hashing and manual CUDA kernel demangling */

uint32_t vt_gpu_groupCID;
uint32_t vt_gpu_commCID;
uint8_t *vt_gpu_prop;

uint32_t vt_gpu_config = 0;

uint8_t vt_gpu_trace_kernels = 0;

uint8_t vt_gpu_trace_idle = 0;

uint8_t vt_gpu_trace_mcpy = 0;

uint8_t vt_gpu_sync_level = 0;

uint8_t vt_gpu_stream_reuse = 0;

uint8_t vt_gpu_trace_memusage = 0;

uint8_t vt_gpu_debug = 0;

uint8_t vt_gpu_error = 0;

uint32_t vt_gpu_rid_idle = VT_NO_ID;

uint32_t vt_gpu_rid_sync = VT_NO_ID;

uint32_t vt_gpu_cid_memusage = VT_NO_ID;

uint64_t vt_gpu_init_time = 0;

static uint8_t vt_gpu_initialized = 0;
static uint8_t vt_gpu_finalized = 0;

/* declaration of internal functions */
static void vt_gpu_createGroups(void);

/*
 * Common initialization for GPU tracing.
 * Has to be in between VTTHRD_LOCK_IDS()!!!
 */
void vt_gpu_init(void)
{
  if(!vt_gpu_initialized){
    
    /* create group property list for threads */
    VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);
    vt_gpu_prop = (uint8_t*)calloc(VTThrdMaxNum, sizeof(uint8_t));
    VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);

    /* get a communicator id for GPU communication */
    vt_gpu_commCID = vt_get_curid();
    vt_gpu_groupCID = vt_get_curid();
    
    /* make sure that the GPU environment is configured */
    vt_gpu_get_config();

    /* GPU idle time */
    if(vt_gpu_trace_idle > 0){
      if(vt_gpu_trace_idle == 2 && vt_gpu_trace_mcpy){
        vt_gpu_rid_idle = vt_def_region(VT_MASTER_THREAD, "gpu_idle", 
                      VT_NO_ID, VT_NO_LNO, VT_NO_LNO, "GPU_IDLE", VT_FUNCTION);
      }else if(vt_gpu_trace_kernels > 0){
        vt_gpu_rid_idle = vt_def_region(VT_MASTER_THREAD, "compute_idle", 
                      VT_NO_ID, VT_NO_LNO, VT_NO_LNO, "GPU_IDLE", VT_FUNCTION);
      }else{
        vt_gpu_trace_idle = 0;
      }
      
      vt_gpu_init_time = vt_pform_wtime();
    }
    
    /* GPU memory usage */
    if(vt_gpu_trace_memusage > 0){
      vt_gpu_cid_memusage = vt_def_counter(VT_MASTER_THREAD, "gpu_mem_usage", "Bytes",
                      VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED,
                      vt_def_counter_group(VT_MASTER_THREAD, "GPU_MEMORY_USAGE"),
                      0);
    }
    
    /* disable stream reuse if neither kernels nor memory copies are enabled */
    if(vt_gpu_stream_reuse && !(vt_gpu_trace_kernels > 0 || vt_gpu_trace_mcpy))
      vt_gpu_stream_reuse = 0;

    vt_gpu_initialized = 1;
  }
}

void vt_gpu_finalize(void)
{
  if(!vt_gpu_finalized && vt_gpu_initialized){
#if (defined(VT_MT) || defined(VT_HYB))
    VTTHRD_LOCK_IDS();
#endif
    if(!vt_gpu_finalized && vt_gpu_initialized){
      vt_gpu_createGroups();

      vt_cntl_msg(2, "[GPU] vt_gpu_finalize() done");
      
      vt_gpu_finalized = 1;
    }
#if (defined(VT_MT) || defined(VT_HYB))
    VTTHRD_UNLOCK_IDS();
#endif
  }
}

uint32_t vt_gpu_get_config(void)
{
  static uint8_t init_config = 0;
  
  if(init_config == 0){
    char *args = vt_env_gputrace();
    const char *sep = ",:";
    char *feature = NULL;

    init_config = 1;
    
    /* check, if the old environment variable has been set */
    (void)vt_env_cudatrace();
    
    /* check for disable CUDA measurement first */
    if(args == NULL || strcmp(args, "no") == 0){
      vt_gpu_trace_kernels = 0;
      vt_gpu_trace_mcpy = 0;
      return vt_gpu_config;
    }
    
    /* check for individual features */
    feature = strtok(args, sep);
    while (feature != NULL){
      if(strcmp(feature, "yes") == 0 || strcmp(feature, "default") == 0) {
        vt_gpu_config |= VT_GPU_TRACE_DEFAULT;
        vt_gpu_trace_kernels = 1;
        vt_gpu_trace_mcpy = 1;
      }else if(strcmp(feature, "cuda") == 0){
        vt_gpu_config |= VT_GPU_TRACE_CUDA;
      }else if(strcmp(feature, "cupti") == 0){
        vt_gpu_config |= VT_GPU_TRACE_CUPTI;
      }else if(strcmp(feature, "opencl") == 0){
        vt_gpu_config |= VT_GPU_TRACE_OPENCL;
      }else if(strcmp(feature, "runtime") == 0){
        vt_gpu_config |= VT_GPU_TRACE_RUNTIME_API;
      }else if(strcmp(feature, "driver") == 0){
        vt_gpu_config |= VT_GPU_TRACE_DRIVER_API;
      }else if(strcmp(feature, "kernel") == 0){
        vt_gpu_config |= VT_GPU_TRACE_KERNEL;
        vt_gpu_trace_kernels = 1;
      }else if(strcmp(feature, "concurrent") == 0){
        vt_gpu_config |= VT_GPU_TRACE_CONCURRENT_KERNEL;
        vt_gpu_trace_kernels = 1;
      }else if(strcmp(feature, "idle") == 0){
        vt_gpu_config |= VT_GPU_TRACE_IDLE;
        vt_gpu_trace_idle = 1;
      }else if(strcmp(feature, "pure_idle") == 0){
        vt_gpu_config |= VT_GPU_TRACE_IDLE;
        vt_gpu_trace_idle = 2;
      }else if(strcmp(feature, "memcpy") == 0){
        vt_gpu_config |= VT_GPU_TRACE_MEMCPY;
        vt_gpu_trace_mcpy = 1;
      }else if(strcmp(feature, "memusage") == 0){
        vt_gpu_config |= VT_GPU_TRACE_MEMUSAGE;
        vt_gpu_trace_memusage = 1;
      }else if(strcmp(feature, "sync") == 0){
        vt_gpu_config |= VT_GPU_TRACE_SYNC;
        vt_gpu_sync_level = (uint8_t)vt_env_gputrace_sync();
      }else if(strcmp(feature, "stream_reuse") == 0){
        vt_gpu_config |= VT_GPU_STREAM_REUSE;
        vt_gpu_stream_reuse = 1;
      }else if(strcmp(feature, "debug") == 0){
        vt_gpu_config |= VT_GPU_DEBUG;
        vt_gpu_debug = 1;
      }else if(strcmp(feature, "error") == 0){
        vt_gpu_config |= VT_GPU_ERROR;
        vt_gpu_error = 1;
      }else{
        vt_warning("[GPU] Unknown GPU tracing option: '%s'", feature);
      }
      
      feature = strtok(NULL, sep);
    }
    
    /* environment variables for further refinement */
    if(vt_env_gputrace_kernel() > 1)
      vt_gpu_trace_kernels = (uint8_t)vt_env_gputrace_kernel();

    if(vt_env_gputrace_memusage() > 1)
      vt_gpu_trace_memusage = (uint8_t)vt_env_gputrace_memusage();
  }

  return vt_gpu_config;
}

/*
 * Creates process groups for all GPU threads in trace and groups for threads,
 * which participate in GPU communication.
 */
static void vt_gpu_createGroups()
{
  uint32_t i, ctrGPUGroup, ctrGPUComm;

  ctrGPUGroup = 0;
  ctrGPUComm = 0;

  /* get number of GPU communication threads and GPU threads to determine
     array size */
  for(i = 0; i < VTThrdn; i++){
    if((vt_gpu_prop[i] & VTGPU_GPU_COMM) == VTGPU_GPU_COMM) ctrGPUComm++;
    if((vt_gpu_prop[i] & VTGPU_GPU) == VTGPU_GPU) ctrGPUGroup++;
  }

  /* create array of GPU communication threads and define group */
  if(ctrGPUComm > 0){
    uint32_t *gpu_comm_array = (uint32_t*)malloc(ctrGPUComm*sizeof(uint32_t));
    int j = 0;
    
    for(i = 0; i < VTThrdn; i++){
      if((vt_gpu_prop[i] & VTGPU_GPU_COMM) == VTGPU_GPU_COMM){
        gpu_comm_array[j++] = VT_PROCESS_ID(vt_my_trace, i);
      }
    }
    
    vt_def_procgrp(VT_CURRENT_THREAD, "GPU_COMM_GLOBAL",
                   VT_PROCGRP_ISCOMMUNICATOR, ctrGPUComm, gpu_comm_array,
                   vt_gpu_commCID);
    
    free(gpu_comm_array);
  }

  /* create array of GPU threads and define group */
  if(ctrGPUGroup > 0){
    uint32_t *gpu_group_array = (uint32_t*)malloc(ctrGPUGroup*sizeof(uint32_t));
    int j = 0;
    
    for(i = 0; i < VTThrdn; i++){
      if((vt_gpu_prop[i] & VTGPU_GPU) == VTGPU_GPU){
        gpu_group_array[j++] = VT_PROCESS_ID(vt_my_trace, i);
      }
    }

    vt_def_procgrp(VT_CURRENT_THREAD, "GPU_GROUP", 0, ctrGPUGroup,
                   gpu_group_array, vt_gpu_groupCID);
    
    free(gpu_group_array);
  }
}

/* 
 * Uses VampirTrace Thread API to create a GPU thread.
 * 
 * @param tname the name of the thread to be registered
 * @param the parent thread id
 * @param vt_tid pointer to the thread id of the thread to be registered
 */
void vt_gpu_registerThread(const char* tname, uint32_t ptid, uint32_t *vt_tid)
{
  if(!vt_is_alive){
    vt_cntl_msg(2, "VampirTrace is not alive. No GPU thread created.\n "
                   "Writing events on master thread (0)");
    return;
  }

  /* create new thread object */
  *vt_tid = VTThrd_create(tname, ptid, 1);
  /* open thread associated trace file */
  VTThrd_open(*vt_tid);

  vt_cntl_msg(2, "[GPU] Created thread '%s' with id: %d", tname, *vt_tid);
  
  /* set the threads property to GPU */
  vt_gpu_prop[*vt_tid] = VTGPU_GPU;
}

#if (defined(VT_CUDA) && defined(VT_CUPTI))

/*
 * Handles errors returned from CUDA driver API calls.
 * 
 * @param ecode the CUDA driver API error code
 * @param msg a message to get more detailed information about the error
 * @param the corresponding file
 * @param the line the error occurred
 */
void vt_gpu_handleCuError(CUresult ecode, const char* msg,
                          const char *file, const int line)
{
  if(msg != NULL) vt_cntl_msg(1, "[CUDA] %s", msg);
  VT_CHECK_THREAD;
  if(vt_gpu_error){
    vt_error_msg("[CUDA Error %d in <%s>:%i] (ptid %d)", ecode, file, line, VT_MY_THREAD);
  }else{
    vt_warning("[CUDA Error %d in <%s>:%i] (ptid %d)", ecode, file, line, VT_MY_THREAD);
  }
}

#endif /* defined(VT_CUDA) && defined(VT_CUPTI) */

#if (defined(VT_CUDARTWRAP) || defined(VT_CUPTI))

#if !defined(VT_LIBERTY)
#include <stdio.h>
char vt_gpu_kernel_name[VTGPU_KERNEL_STRING_SIZE];

/*
 * Parse the device function name:
 * "_Z<kernel_length><kernel_name><templates>..." (no name space)
 * "_ZN<ns_length><ns_name>...<ns_length><ns_name><kernel_length>..." (with name space)
 *
 * @param kname the extracted kernel name
 * @param devFunc the CUDA internal kernel function name
 */
char* vt_cuda_demangleKernel(const char* mangled)
{
  int i = 0;       /* position in device function (source string) */
  int nlength = 0; /* length of name space or kernel */
  int ePos = 0;    /* position in final kernel string */
  char *curr_elem, kn_templates[VTGPU_KERNEL_STRING_SIZE];
  char *tmpEnd, *tmpElemEnd;

  /*vt_cntl_msg(1,"[CUDART] device function name: %s'", devFunc);*/

  /* init for both cases: name space available or not */
  if(mangled[2] == 'N'){
    nlength = atoi(&mangled[3]); /* get length of first name space */
    i = 4;
  }else{
    nlength = atoi(&mangled[2]); /* get length of kernel */
    i = 3;
  }

  /* unless string null termination */
  while(mangled[i] != '\0'){
    /* found either name space or kernel name (no digits) */
    if(mangled[i] < '0' || mangled[i] > '9'){
      /* copy name to kernel function */
      if((ePos + nlength) < VTGPU_KERNEL_STRING_SIZE){
        (void)strncpy(&vt_gpu_kernel_name[ePos], &mangled[i], nlength);
        ePos += nlength; /* set next position to write */
      }else{
        nlength = VTGPU_KERNEL_STRING_SIZE - ePos;
        (void)strncpy(&vt_gpu_kernel_name[ePos], &mangled[i], nlength);
        vt_cntl_msg(1,"[CUDART]: kernel name '%s' contains more than %d chars!",
                      mangled, VTGPU_KERNEL_STRING_SIZE);
        return vt_gpu_kernel_name;
      }

      i += nlength; /* jump over name */
      nlength = atoi(&mangled[i]); /* get length of next name space or kernel */

      /* finish if no digit after name space or kernel */
      if(nlength == 0){
        vt_gpu_kernel_name[ePos] = '\0'; /* set string termination */
        break;
      }else{
        if((ePos + 3) < VTGPU_KERNEL_STRING_SIZE){
          (void)strncpy(&vt_gpu_kernel_name[ePos], "::\0", 3);
          ePos += 2;
        }else{
          vt_cntl_msg(1,"[CUDART]: kernel name '%s' contains more than %d chars!",
                        mangled, VTGPU_KERNEL_STRING_SIZE);
          return vt_gpu_kernel_name;
        }
      }
    }else i++;
  }

  /* copy the end of the kernel name string to extract templates */
  if(-1 == snprintf(kn_templates, VTGPU_KERNEL_STRING_SIZE, "%s", &mangled[i+1]))
    vt_cntl_msg(1, "[CUDART]: Error parsing kernel '%s'", mangled);
  curr_elem = kn_templates; /* should be 'L' */

  /* search templates (e.g. "_Z10cptCurrentILb1ELi10EEv6SField8SParListifff") */
  tmpEnd=strstr(curr_elem,"EE");
  /* check for templates: curr_elem[0] points to 'L' AND string contains "EE" */
  if(tmpEnd != NULL && curr_elem[0]=='L'){ /* templates exist */
    tmpEnd[1] = '\0'; /* set 2nd 'E' to \0 as string end marker */

    /* write at position 'I' with '<' */
    /* elem->name[ePos]='<'; */
    if(-1 == snprintf(&(vt_gpu_kernel_name[ePos]),VTGPU_KERNEL_STRING_SIZE-ePos,"<"))
      vt_cntl_msg(1,"[CUDART] Parsing templates of kernel '%s' failed!", mangled);
    ePos++; /* continue with next character */

    do{
      int res;
      curr_elem++; /* set pointer to template type length or template type */
      /* find end of template element */
      tmpElemEnd = strchr(curr_elem + atoi(curr_elem), 'E');
      if(tmpElemEnd == NULL) continue;
      tmpElemEnd[0] = '\0'; /* set termination char after template element */
      /* find next non-digit char */
      while(*curr_elem >= '0' && *curr_elem <= '9') curr_elem++;
      /* append template value to kernel name */
      if(-1 == (res = snprintf(&(vt_gpu_kernel_name[ePos]),
                               VTGPU_KERNEL_STRING_SIZE-ePos,"%s,",curr_elem)))
        vt_cntl_msg(1,"[CUDART]: Parsing templates of kernel '%s' crashed!", mangled);
      ePos += res; /* continue after template value */
      curr_elem =tmpElemEnd + 1; /* set current element to begin of next template */
    }while(tmpElemEnd < tmpEnd);
    if((ePos-1) < VTGPU_KERNEL_STRING_SIZE) (void)strncpy(&vt_gpu_kernel_name[ePos-1], ">\0", 2);
    else vt_cntl_msg(1,"[CUDART]: Templates of '%s' too long for internal buffer!", mangled);
  } /* else: kernel has no templates */
  /*vt_cntl_msg(1,"[CUDART] function name: %s'",e->name);*/
  
  return vt_gpu_kernel_name;
}

#endif /* defined(VT_LIBERTY) */
#endif /* defined(VT_CUDARTWRAP) || defined(VT_CUPTI) */

/***************************** hashing of strings *****************************/
#include "util/hash.h"

/* size of hash table (must be a power of two!) */
#define VT_GPU_HASHTABLE_SIZE 1024

static vt_gpu_hn_string_t* vt_gpu_string_htab[VT_GPU_HASHTABLE_SIZE];

void* vt_gpu_stringHashPut(const char* n, uint32_t rid)
{
  uint32_t id = vt_hash(n, strlen(n), 0) & (VT_GPU_HASHTABLE_SIZE - 1);
  vt_gpu_hn_string_t *add = NULL;
  
  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);
  
  add = (vt_gpu_hn_string_t*)malloc(sizeof(vt_gpu_hn_string_t));
  add->sname = strdup(n);  
  add->rid = rid;
  add->next = vt_gpu_string_htab[id];
  vt_gpu_string_htab[id] = add;
  
  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
  
  return add;
}

void* vt_gpu_stringHashGet(const char* n)
{
  uint32_t id = vt_hash(n, strlen(n), 0) & (VT_GPU_HASHTABLE_SIZE - 1);
  vt_gpu_hn_string_t *curr = vt_gpu_string_htab[id];
  
  while ( curr ) {
    if ( strcmp( curr->sname, n ) == 0 )
      return curr;

    curr = curr->next;
  }
  
  return NULL;
}

void vt_gpu_stringhashClear()
{
  int i;
  vt_gpu_hn_string_t* tmp_node;
  
  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  for ( i = 0; i < VT_GPU_HASHTABLE_SIZE; i++ )
  {
    while( vt_gpu_string_htab[i] )
    {
      tmp_node = vt_gpu_string_htab[i]->next;
      free( vt_gpu_string_htab[i]->sname );
      free( vt_gpu_string_htab[i] );
      vt_gpu_string_htab[i] = tmp_node;
    }
  }
  
  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
}
