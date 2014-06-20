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

#ifndef _VT_GPU_H_
#define _VT_GPU_H_

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

#include "vt_defs.h"
#include "vt_inttypes.h"    /* VampirTrace integer types */
#include "vt_thrd.h"        /* thread creation for GPU kernels */
#include "vt_trc.h"         /* VampirTrace events */
#include "vt_error.h"       /* VampirTrace warning and error messages */

/* definition for the GPU tracing features/configuration */
#define VT_GPU_TRACE_CUDA               (1 << 0)
#define VT_GPU_TRACE_CUPTI              (1 << 1)
#define VT_GPU_TRACE_OPENCL             (1 << 2)
#define VT_GPU_TRACE_RUNTIME_API        (1 << 3)
#define VT_GPU_TRACE_DRIVER_API         (1 << 4)
#define VT_GPU_TRACE_KERNEL             (1 << 5)
#define VT_GPU_TRACE_CONCURRENT_KERNEL  (1 << 6)
#define VT_GPU_TRACE_IDLE               (1 << 7)
#define VT_GPU_TRACE_MEMCPY             (1 << 8)
#define VT_GPU_TRACE_MEMUSAGE           (1 << 9)
#define VT_GPU_TRACE_SYNC               (1 << 10)
#define VT_GPU_STREAM_REUSE             (1 << 11)
#define VT_GPU_DEBUG                    (1 << 12)
#define VT_GPU_ERROR                    (1 << 13)


/* set the default tracing configuration */
#if defined(VT_CUDARTWRAP)
# define VT_GPU_TRACE_DEFAULT \
  (VT_GPU_TRACE_CUDA | VT_GPU_TRACE_RUNTIME_API | VT_GPU_TRACE_OPENCL | \
   VT_GPU_TRACE_KERNEL | VT_GPU_TRACE_MEMCPY)
#else
# define VT_GPU_TRACE_DEFAULT \
  (VT_GPU_TRACE_CUPTI | VT_GPU_TRACE_RUNTIME_API | VT_GPU_TRACE_OPENCL | \
   VT_GPU_TRACE_KERNEL | VT_GPU_TRACE_MEMCPY)
#endif

/* defines the maximum string length of a function/kernel executed on GPU */
#define VTGPU_KERNEL_STRING_SIZE 256

/* default and maximum buffer size for asynchronous on-device tasks (in bytes) */
#define VTGPU_DEFAULT_BSIZE 8192
#define VTGPU_MAX_BSIZE     2097152 /* 8192^8 bytes */

/* default buffer size for CUPTI activities */
#define VT_CUPTI_ACT_DEFAULT_BSIZE 65536

/* defines for GPU GROUP and GPU COMM (8 bit only!!!) */
#define VTGPU_NO_GPU   0x00 /* thread is no GPU and does no GPU communication */
#define VTGPU_GPU      0x01 /* thread is a GPU thread */
#define VTGPU_GPU_COMM 0x02 /* thread does GPU communication (CPU or GPU) */

/* performance counter available? */
#define VTGPU_NO_PC    0x04 /* no performance counter for this thread available */

/* 
 * Get the rank ID for a given VampirTrace thread ID.
 * The MPI RMA functions take the rank ID instead of the VampirTrace process ID!
 */
#define VT_GPU_RANK_ID(thread_id) \
  (VT_PROCESS_ID(vt_my_trace, thread_id)-1)

#if (defined(VT_CUDARTWRAP) || defined(VT_CUPTI))

#if defined(VT_LIBERTY)
#include "vt_demangle.h"

#define vt_cuda_demangleKernel(mangled) \
  cplus_demangle(mangled, 0)

#else

EXTERN char vt_gpu_kernel_name[VTGPU_KERNEL_STRING_SIZE];

/*
 * Parse the device function name:
 * "_Z<kernel_length><kernel_name><templates>..." (no name space)
 * "_ZN<ns_length><ns_name>...<ns_length><ns_name><kernel_length>..." (with name space)
 *
 * @param kname the extracted kernel name
 * @param devFunc the CUDA internal kernel function name
 */
EXTERN char* vt_cuda_demangleKernel(const char* mangled);
#endif /* defined(VT_DEMANGLE) */

#endif /* defined(VT_CUDARTWRAP) || defined(VT_CUPTI) */


#if (defined(VT_CUDA) && defined(VT_CUPTI))

#include "vt_cuda_driver_api.h"

# define VT_CUDRV_CALL(_err, _msg) \
  if(_err != CUDA_SUCCESS){ \
    vt_gpu_handleCuError(_err, _msg, __FILE__,__LINE__); \
  }

/*
 * Handles errors returned from CUDA driver API calls.
 * 
 * @param ecode the CUDA driver API error code
 * @param msg a message to get more detailed information about the error
 * @param the corresponding file
 * @param the line the error occurred
 */
EXTERN void vt_gpu_handleCuError(CUresult ecode, const char* msg,
                                 const char *file, const int line);

#else /* defined(VT_CUDA) && defined(VT_CUPTI) */

# define VT_CUDRV_CALL(_err, _msg)

#endif /* defined(VT_CUDA) && defined(VT_CUPTI) */


/* device/host communication directions */
typedef enum {
  VT_GPU_DEV2HOST  = 0x00, /* device to host copy */
  VT_GPU_HOST2DEV  = 0x01, /* host to device copy */
  VT_GPU_DEV2DEV   = 0x02, /* device to device copy */
  VT_GPU_HOST2HOST = 0x04,  /* host to host copy */
  VT_GPU_COPYDIRECTION_UNKNOWN = 0x08  /* unknown */
} vt_gpu_copy_kind_t;

/* 
 * global communicator id for all GPU threads
 */
EXTERN uint32_t vt_gpu_groupCID;

/* 
 * communicator for all node local threads communicating with GPU
 */
EXTERN uint32_t vt_gpu_commCID;

/*
 * Process/Thread IDs, which participate in GPU communication.
 * Index of the list is the thread ID (VTThrd...)
 */
EXTERN uint8_t *vt_gpu_prop;

/*
 * bit mask holding the GPU tracing configuration
 */
EXTERN uint32_t vt_gpu_config;

/*
 * flag: trace GPU kernels?
 */
EXTERN uint8_t vt_gpu_trace_kernels;

/*
 * flag: write GPU idle time as region into first GPU stream/queue?
 */
EXTERN uint8_t vt_gpu_trace_idle;

/*
 * flag: trace GPU data transfers (memory copies)?
 */
EXTERN uint8_t vt_gpu_trace_mcpy;

/* 
 * Synchronization Level:
 * 0 no extra synchronization
 * 1 synchronize before synchronous memory copy or synchronization - correct
 *   data transfer rates for communication
 * 2 show synchronization in extra region group to get host wait time
 */
EXTERN uint8_t vt_gpu_sync_level;

/*
 * flag: Reuse destroyed GPU streams?
 */
EXTERN uint8_t vt_gpu_stream_reuse;

/*
 * flag: trace GPU memory usage (allocation, free)?
 */
EXTERN uint8_t vt_gpu_trace_memusage;

/*
 * flag: Is debugging on? (yes: do not call CUDA functions in finalize)
 */
EXTERN uint8_t vt_gpu_debug;

/* 
 * flag: abort program on GPU error, if enabled 
 */
EXTERN uint8_t vt_gpu_error;

/*
 * VampirTrace timestamp during vt_gpu_init(). Is only set if kernel and idle 
 * time measurement is enabled.
 */
EXTERN uint64_t vt_gpu_init_time;

/* 
 * VampirTrace region ID for GPU idle time 
 */
EXTERN uint32_t vt_gpu_rid_idle;

/*
 * VampirTrace region ID for synchronization of host and CUDA device
 */
EXTERN uint32_t vt_gpu_rid_sync;

/*
 * VampirTrace GPU memory allocation counter
 */
EXTERN uint32_t vt_gpu_cid_memusage;

/*
 * Initialization for all GPU API wrappers.
 * VampirTrace IDS have to be locked, before calling this function.
 */
EXTERN void vt_gpu_init(void);

/*
 * Finalization for all GPU API wrappers.
 * VampirTrace IDS have to be locked, before calling this function.
 */
EXTERN void vt_gpu_finalize(void);

/*
 * Get GPU tracing configuration.
 */
EXTERN uint32_t vt_gpu_get_config(void);

/* 
 * Uses VampirTrace Thread API to create a GPU thread
 *
 * @param tname the name of the thread to be registered
 * @param ptid the parent thread id
 * @param vt_tid pointer to the thread id of the thread to be registered
 */
EXTERN void vt_gpu_registerThread(const char* tname, uint32_t ptid,
                                  uint32_t *vt_tid);

/***************************** hashing of strings *****************************/

/* The key of the hash node is a string and the value an unsigned 32bit integer. 
   It is used to store region names with its corresponding region IDs. */
typedef struct vt_gpu_hnString_st {
  char                      *sname; /**< name of the symbol */
  uint32_t                  rid;    /**< associated region group identifier */
  struct vt_gpu_hnString_st *next;  /**< bucket for collision */
} vt_gpu_hn_string_t;

/*
 * Stores a hash value in the hash table.
 * 
 * @param n pointer to a char (string) - the hash nodes key
 * @param rid integer - the hash nodes value
 * 
 * @return pointer to the hash node
 */
EXTERN void* vt_gpu_stringHashPut(const char* n, uint32_t rid);

/*
 * Retrieves the hash node for a given key.
 * 
 * @param n pointer to a char (string) - the hash nodes key
 * 
 * @return pointer to the hash node
 */
EXTERN void* vt_gpu_stringHashGet(const char* n);

/*
 * Clears the hash table. Frees all allocated hash nodes.
 */
EXTERN void vt_gpu_stringhashClear(void);

#endif /* _VT_GPU_H_ */
