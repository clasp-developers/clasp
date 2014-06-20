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

#ifndef _VT_CUDARTWRAP_H_
#define _VT_CUDARTWRAP_H_

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

#include "vt_gpu.h"         /* common for GPU */
#include "vt_libwrap.h"     /* wrapping of CUDA Runtime API functions */
#include "vt_cuda_runtime_api.h"       /* includes CUDA runtime API functions */

/*#if (defined(VT_CUDARTWRAP))*/

#if (defined(CUDART_VERSION) && (CUDART_VERSION < 5000))
#define VT_CUDARTWRAP_COMPAT_PTR const char *
#else
#define VT_CUDARTWRAP_COMPAT_PTR const void *
#endif

/* library wrapper object */
EXTERN VTLibwrap* vt_cudart_lw;

/* library wrapper attributes declaration */
EXTERN VTLibwrapAttr vt_cudart_lw_attr;

/* internal initialization functions for the CUDA runtime library */
EXTERN void vt_cudartwrap_lw_attr_init(VTLibwrapAttr* attr);
EXTERN void vt_cudartwrap_init(void);

/* flag: tracing of CUDA API enabled? */
EXTERN uint8_t vt_cudart_trace_enabled;

/* flag: Is the CUDA runtime initialized? CUDA runtime function called? */
EXTERN uint8_t vt_cudart_initialized;

EXTERN void vt_cudartwrap_finalize(void);

/* Mutex for locking the CUDA runtime wrap environment */
#if (defined(VT_MT) || defined(VT_HYB))
EXTERN VTThrdMutex* VTThrdMutexCudart;
# define CUDARTWRAP_LOCK() VTThrd_lock(&VTThrdMutexCudart)
# define CUDARTWRAP_UNLOCK() VTThrd_unlock(&VTThrdMutexCudart)
#else /* VT_MT || VT_HYB */
# define CUDARTWRAP_LOCK()
# define CUDARTWRAP_UNLOCK()
#endif /* VT_MT || VT_HYB */

/* do initialization before calling the first CUDA function
 * no CUDA context creation */
#define CUDARTWRAP_FUNC_INIT(_lw, _lwattr, _func, _rettype, _argtypes, _file, \
                             _line)                                           \
  VT_LIBWRAP_FUNC_INIT(_lw, _lwattr, _func, _rettype, _argtypes, _file,       \
                       _line);                                                \
                                                                              \
  if(!vt_cudart_initialized){                                                 \
    CUDARTWRAP_LOCK();                                                        \
    if(!vt_cudart_initialized){                                               \
      vt_cudartwrap_init();                                                   \
    }                                                                         \
    CUDARTWRAP_UNLOCK();                                                      \
  }

#define CUDARTWRAP_FUNC_START(_lw) \
  if(vt_cudart_trace_enabled){     \
    VT_LIBWRAP_FUNC_START(_lw);    \
  }

#define CUDARTWRAP_FUNC_END(_lw) \
  if(vt_cudart_trace_enabled) VT_LIBWRAP_FUNC_END(_lw)

/*#endif*/

#endif /* _VT_CUDARTWRAP_H_ */
