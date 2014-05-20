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

#ifndef VT_CUPTI_EVENTS_H
#define	VT_CUPTI_EVENTS_H

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

#include "vt_inttypes.h"        /* VampirTrace integer types */

#include "vt_cupti_common.h"    /* CUPTI common structures, functions, etc. */

EXTERN uint8_t vt_cupti_events_enabled;

EXTERN uint8_t vt_cupti_events_sampling;

/*
 * Initialize VampirTrace IDs and registers the finalize function.
 * This may be done implicitly by vt_cuptievt_count().
 */
EXTERN void vt_cupti_events_init(void);

/*
 * Finalizes the VampirTrace CUPTI implementation.
 */
EXTERN void vt_cupti_events_finalize(void);

/*
 * Initialize the CUPTI events data of the given VampirTrace CUPTI context.
 * 
 * @param vtCtx pointer to the VampirTrace CUPTI context
 */
EXTERN void vt_cupti_events_initContext(vt_cupti_ctx_t *vtCtx);

/*
 * Finalizes the given VampirTrace CUPTI events context.
 * 
 * @param vtCtx pointer to the VampirTrace CUPTI context
 */
EXTERN void vt_cupti_events_finalizeContext(vt_cupti_ctx_t *vtCtx);

/*
 * Finalizes CUPTI device.
 * 
 * @param ptid the VampirTrace process/thread id
 * @param cleanExit 1 to cleanup CUPTI event group, otherwise 0
 */
EXTERN void vt_cuptievt_finalize_device(uint8_t cleanExit);

/*
 * Returns the VampirTrace CUPTI context for the CUDA context associated with
 * the calling host thread. Makes sure to also create a VampirTrace CUPTI event 
 * context.
 *
 * @param ptid the VampirTrace thread id of the calling host thread
 */
EXTERN vt_cupti_ctx_t* vt_cuptievt_getOrCreateCurrentCtx(uint32_t ptid);

/*
 * Request the CUPTI counter values and write it to the given VampirTrace
 * stream with the given timestamps.
 *
 * @param vtcuptiCtx pointer to the VampirTrace CUPTI context
 * @param strmid the stream id for the counter values
 * @param time the VampirTrace timestamps
 */
EXTERN void vt_cuptievt_writeCounter(vt_cupti_events_t *vtcuptiEvtCtx, uint32_t strmid, 
                              uint64_t *time);

/*
 * Reset the VampirTrace counter values (to zero) for active CUPTI counters.
 *
 * @param vtcuptiCtx pointer to the VampirTrace CUPTI context
 * @param strmid the stream id for the counter values
 * @param time the VampirTrace timestamps
 */
EXTERN void vt_cuptievt_resetCounter(vt_cupti_events_t *vtcuptiEvtCtx, uint32_t strmid, 
                              uint64_t *time);

#endif	/* VT_CUPTI_EVENTS_H */

