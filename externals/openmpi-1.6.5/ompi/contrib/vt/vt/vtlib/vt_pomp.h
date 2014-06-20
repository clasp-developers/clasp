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

#include "vt_inttypes.h"

#define IS_POMP_TRACE_ON ( pomp_tracing )

extern int pomp_initialized;
extern int pomp_tracing;

struct VTRegDescr {
  uint32_t rid;     /* region id */
  uint32_t brid;    /* region id of implicit barrier */
                    /* also: lockid for critical     */
  uint32_t sbrid;   /* region id of enclosed structured block */
                    /* also: inner construct for combined constructs */
  uint32_t fid;
  uint32_t begln;
  uint32_t endln;
};

#define GUARDED_ENTER(id) \
  if ( IS_POMP_TRACE_ON ) { \
    struct VTRegDescr* data = (struct VTRegDescr*)(r->data); \
    uint64_t time = vt_pform_wtime(); \
    vt_enter(VT_CURRENT_THREAD, &time, data->id); \
  }

#define GUARDED_ENTER_2(ch,id1,id2) \
  if ( IS_POMP_TRACE_ON ) { \
    struct VTRegDescr* data; \
    uint64_t time; \
    data = (struct VTRegDescr*)(r->data); \
    time = vt_pform_wtime(); \
    if ( r->name[0] == ch ) \
      vt_enter(VT_CURRENT_THREAD, &time, data->id1); \
    else \
      vt_enter(VT_CURRENT_THREAD, &time, data->id2); \
  }

#define GUARDED_EXIT() \
  if ( IS_POMP_TRACE_ON ) { \
    uint64_t time; \
    time = vt_pform_wtime(); \
    vt_exit(VT_CURRENT_THREAD, &time); \
  }

#define GUARDED_COLL_ENTER(id) \
  if ( IS_POMP_TRACE_ON ) { \
    struct VTRegDescr* data; \
    uint64_t time; \
    data = (struct VTRegDescr*)(r->data); \
    time = vt_pform_wtime(); \
    /* vt_omp_collenter(VT_CURRENT_THREAD, &time, data->id); */ \
    vt_enter(VT_CURRENT_THREAD, &time, data->id); \
  }

#define GUARDED_COLL_ENTER_2(ch,id1,id2) \
  if ( IS_POMP_TRACE_ON ) { \
    struct VTRegDescr* data; \
    uint64_t time; \
    data = (struct VTRegDescr*)(r->data); \
    time = vt_pform_wtime(); \
    if ( r->name[0] == ch ) { \
      /* vt_omp_collenter(VT_CURRENT_THREAD, &time, data->id1); */ \
      vt_enter(VT_CURRENT_THREAD, &time, data->id1); \
    } else { \
      /* vt_omp_collenter(VT_CURRENT_THREAD, &time, data->id2); */ \
      vt_enter(VT_CURRENT_THREAD, &time, data->id2); \
    } \
  }

#define GUARDED_COLL_EXIT() \
  if ( IS_POMP_TRACE_ON ) { \
    uint64_t time; \
    time = vt_pform_wtime(); \
    /* vt_omp_collexit(VT_CURRENT_THREAD, &time); */ \
    vt_exit(VT_CURRENT_THREAD, &time); \
  }
