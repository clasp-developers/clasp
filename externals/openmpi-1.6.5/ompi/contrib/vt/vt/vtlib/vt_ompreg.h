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

#ifndef _VT_OMPREG_H
#define _VT_OMPREG_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#define VT__OMP_DESTROY_LOCK                     0
#define VT__OMP_DESTROY_NEST_LOCK                1
#define VT__OMP_GET_DYNAMIC                      2
#define VT__OMP_GET_MAX_THREADS                  3
#define VT__OMP_GET_NESTED                       4
#define VT__OMP_GET_NUM_PROCS                    5
#define VT__OMP_GET_NUM_THREADS                  6
#define VT__OMP_GET_THREAD_NUM                   7
#define VT__OMP_IN_PARALLEL                      8
#define VT__OMP_INIT_LOCK                        9
#define VT__OMP_INIT_NEST_LOCK                  10
#define VT__OMP_SET_DYNAMIC                     11
#define VT__OMP_SET_LOCK                        12
#define VT__OMP_SET_NEST_LOCK                   13
#define VT__OMP_SET_NESTED                      14
#define VT__OMP_SET_NUM_THREADS                 15  
#define VT__OMP_TEST_LOCK                       16
#define VT__OMP_TEST_NEST_LOCK                  17 
#define VT__OMP_UNSET_LOCK                      18
#define VT__OMP_UNSET_NEST_LOCK                 19
#define VT__OMP_REGID_NUM                       20

extern int     vt_omp_regid[VT__OMP_REGID_NUM];

EXTERN void    vt_omp_register(void);

#endif /* _VT_OMPREG_H */









