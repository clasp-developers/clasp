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

#ifndef VT_CUPTI_ACTIVITY_H
#define	VT_CUPTI_ACTIVITY_H

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

#include "vt_cupti_common.h"    /* CUPTI common structures, functions, etc. */

/* 
 * Initialize the VampirTrace CUPTI Activity implementation.
 */
EXTERN void vt_cupti_activity_init(void);

/* 
 * Finalize the VampirTrace CUPTI Activity implementation.
 */
EXTERN void vt_cupti_activity_finalize(void);

/*
 * Finalize the VampirTrace CUPTI activity context.
 * 
 * @param vtCtx the VampirTrace CUPTI context, which contains the activities
 */
EXTERN void vt_cupti_activity_finalizeContext(vt_cupti_ctx_t *vtCtx);

/*
 * Setup a the VampirTrace CUPTI activity context. Trigger initialization and 
 * enqueuing of the CUPTI activity buffer for the given context.
 * 
 * @param vtCtx the VampirTrace CUPTI context
 */
EXTERN void vt_cuptiact_setupActivityContext(vt_cupti_ctx_t *vtCtx);

/*
 * Check for empty activity buffer.
 * 
 * @param cuCtx CUDA context
 * 
 * @return 1 for empty, 0 for non-empty buffer
 */
EXTERN uint8_t vt_cupti_activity_isBufferEmpty(CUcontext cuCtx);

/*
 * Handle activities buffered by CUPTI. Lock a call to this routine!!!
 * 
 * NVIDIA:
 * "Global Queue: The global queue collects all activity records that
 * are not associated with a valid context. All API activity records
 * are collected in the global queue. A buffer is enqueued in the
 * global queue by specifying \p context == NULL.
 *
 * Context Queue: Each context queue collects activity records
 * associated with that context that are not associated with a
 * specific stream or that are associated with the default stream
 * (stream ID 0). A buffer is enqueued in a context queue by
 * specifying the context and a stream ID of 0.
 *
 * Stream Queue: Each stream queue collects memcpy, memset, and kernel
 * activity records associated with the stream. A buffer is enqueued
 * in a stream queue by specifying a context and a non-zero stream ID."
 * 
 * @param vtCtx VampirTrace CUPTI context, NULL to handle globally buffered 
 * activities
 */
EXTERN void vt_cuptiact_flushCtxActivities(vt_cupti_ctx_t *vtCtx);

/*
 * Mark a CUDA stream as destroyed, so that it can be reused afterwards.
 * 
 * @param cuCtx CUDA context, which contains the stream
 * @param strmID the CUDA stream ID to be marked as destroyed
 * 
 */
EXTERN void vt_cuptiact_markStreamAsDestroyed(CUcontext cuCtx, uint32_t strmID);


EXTERN void vt_cuptiact_writeMalloc(uint32_t ctxID, CUcontext cuCtx, 
                                    void *devPtr, size_t size);

EXTERN void vt_cuptiact_writeFree(uint32_t ctxID, CUcontext cuCtx, 
                                  void *devPtr);

/*
 * To provide correlation data between API call and activity.
 * 
 * @param ctxID the CUDA context identifier
 * @param correlationID correlation between memory copy and API call
 */
EXTERN void vt_cuptiact_addCorrelation(uint32_t ctxID, uint32_t correlationID, 
                                       uint32_t ptid);

#if (defined(CUPTI_API_VERSION) && (CUPTI_API_VERSION >= 3))
/*
 * Enable tracing of concurrent kernels. Disable normal kernel tracing, if 
 * necessary.
 * 
 * @param vtCtx pointer to the VampirTrace CUPTI context.
 */
EXTERN void vt_cuptiact_enableConcurrentKernel(vt_cupti_ctx_t *vtCtx);
#endif

#endif	/* VT_CUPTI_ACTIVITY_H */

