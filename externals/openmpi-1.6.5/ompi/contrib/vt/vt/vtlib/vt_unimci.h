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

#ifndef _VT_UNIMCI_H
#define _VT_UNIMCI_H

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

#ifdef VT_UNIMCI

# include "config.h"

# include "vt_defs.h"
# include "vt_inttypes.h"
# include "vt_thrd.h"
# include "vt_trc.h"

/* if all MPI2 parts are available define 'UNIMCI_MPI2' before
   including 'unimci.h' */
# if (defined(HAVE_MPI2_1SIDED)          && HAVE_MPI2_1SIDED)  &&      \
     (defined(HAVE_MPI2_EXTCOLL)         && HAVE_MPI2_EXTCOLL) &&      \
     (defined(HAVE_MPI2_THREAD)          && HAVE_MPI2_THREAD)  &&      \
     (defined(HAVE_MPI2_IO)              && HAVE_MPI2_IO)      &&      \
     (defined(HAVE_MPI_REGISTER_DATAREP) && HAVE_MPI_REGISTER_DATAREP)
#   define UNIMCI_MPI2
# endif /* HAVE_MPI2_* */
# include "unimci.h"

# define VT_UNIMCI_DO_CHECK()                                                 \
  (vt_is_alive &&                                                             \
   VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) != VT_TRACE_OFF_PERMANENT &&    \
   vt_unimci_is_initialized)

# define VT_UNIMCI_SET_BINDING_LANGUAGE_C()                                   \
  if (vt_unimci_is_initialized)                                               \
    UNIMCI_set_binding_language(UNIMCI_LANGUAGE_C)

# define VT_UNIMCI_SET_BINDING_LANGUAGE_FORTRAN()                             \
  if (vt_unimci_is_initialized)                                               \
    UNIMCI_set_binding_language(UNIMCI_LANGUAGE_FORTRAN)

# define VT_UNIMCI_CHECK_PRE(_call, _args, _record, _time)                    \
  if (VT_UNIMCI_DO_CHECK()) {                                                 \
    UNIMCI_check_pre__##_call _args;                                          \
    vt_unimci_check_msg(_record, _time);                                      \
  }

# define VT_UNIMCI_CHECK_POST(_call, _args, _record, _time)                   \
  if (VT_UNIMCI_DO_CHECK()) {                                                 \
    UNIMCI_check_post__##_call _args;                                         \
    vt_unimci_check_msg(_record, _time);                                      \
  }

EXTERN void vt_unimci_init(void);

EXTERN void vt_unimci_finalize(void);

EXTERN void vt_unimci_check_msg(uint8_t record, uint64_t* time);

EXTERN uint8_t vt_unimci_is_initialized;

#else /* VT_UNIMCI */

# define VT_UNIMCI_SET_BINDING_LANGUAGE_C()
# define VT_UNIMCI_SET_BINDING_LANGUAGE_FORTRAN()
# define VT_UNIMCI_CHECK_PRE(_call, _args, _record, _time)                    \
  (void)(_record) /* needed to avoid compiler warnings (unused but set) */
# define VT_UNIMCI_CHECK_POST(_call, _args, _record, _time)

#endif /* VT_UNIMCI */

#endif /* _VT_UNIMCI_H */
