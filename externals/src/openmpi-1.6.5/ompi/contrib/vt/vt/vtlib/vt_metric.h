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

#ifndef _VT_METRIC_H
#define _VT_METRIC_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

/* maximum number of counters */
#define VT_METRIC_MAXNUM 20

/* define and set metrics based on VT_METRICS specification */
EXTERN int             vt_metric_open(void);
EXTERN void            vt_metric_close(void);

/* create per-thread counter sets */
EXTERN struct vt_metv* vt_metric_create(void);
/* free per-thread counter sets */
EXTERN void            vt_metric_free(struct vt_metv* metv, uint32_t tid);

/* register thread (supply pthread_self() / omp_get_thread_num() as argument) */
EXTERN void            vt_metric_thread_init(long (*id_fn)(void));
/* unregister thread */
EXTERN void            vt_metric_thread_fini(void);

/* reads values of counters relative to the time of vt_metric_open() */
EXTERN void            vt_metric_read(struct vt_metv* metv, uint64_t offsets[],
                                      uint64_t values[]);

/* returns number of counters */
EXTERN int             vt_metric_num(void);
/* returns name of counter i */
EXTERN const char*     vt_metric_name(int i);
/* returns description of counter i */
EXTERN const char*     vt_metric_descr(int i);
/* returns unit of counter i */
EXTERN const char*     vt_metric_unit(int i);
/* returns OTF properties of counter i */
EXTERN uint32_t vt_metric_props(int i);

#endif /* _VT_METRIC_H */
