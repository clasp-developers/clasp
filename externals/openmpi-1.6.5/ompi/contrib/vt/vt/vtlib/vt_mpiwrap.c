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

/* if compiling inside Open MPI, suppress warnings about usage of
   deprecated MPI functions (e.g. MPI_Address) */
#ifdef INSIDE_OPENMPI
# undef OMPI_WANT_MPI_INTERFACE_WARNING
# define OMPI_WANT_MPI_INTERFACE_WARNING 0
#endif /* INSIDE_OPENMPI */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vt_defs.h"
#include "vt_env.h"
#include "vt_error.h"
#include "vt_inttypes.h"
#include "vt_mallocwrap.h"
#include "vt_mpicom.h"
#include "vt_mpireg.h"
#include "vt_mpireq.h"
#include "vt_mpiwrap.h"
#include "vt_pform.h"
#include "vt_thrd.h"
#include "vt_trc.h"
#include "vt_unimci.h"
#if defined (HAVE_MPI2_IO) && HAVE_MPI2_IO
# include "vt_mpifile.h"
# include "vt_iowrap.h"
#endif /* HAVE_MPI2_IO */

#include "mpi.h"

/* since MPI-3 the C keyword "const" is added to all relevant MPI API parameters
   (e.g. MPI_Send(void* sendbuf, ...) -> MPI_Send(const void* sendbuf, ...));
   prepend CONST to these parameters which is defined either to "const"
   (if MPI-3) or to nothing (if MPI-1/2) */
#if defined(MPI_VERSION) && MPI_VERSION >= 3
# define CONST const
#else /* MPI_VERSION */
# define CONST
#endif /* MPI_VERSION */

/* get calling thread id */
#ifdef VT_HYB
# if defined(HAVE_MPI2_THREAD) &&  HAVE_MPI2_THREAD
#   define CHECK_THREAD(tid) ((tid) == 0 || is_mpi_multithreaded)
# else /* HAVE_MPI2_THREAD */
#   define CHECK_THREAD(tid) ((tid) == 0)
# endif /* HAVE_MPI2_THREAD */
# define GET_THREAD_ID(tid) \
    VT_CHECK_THREAD; \
    (tid) = VT_MY_THREAD; \
    if (mpi_init_called && !CHECK_THREAD((tid))) \
      vt_error_msg("%s called from a non-master thread. " \
                   "The provided MPI thread support level does not " \
                   "allow that.", __func__)
#else /* VT_HYB */
# define GET_THREAD_ID(tid) (tid) = 0
#endif /* VT_HYB */

/* check whether MPI tracing is currently enabled */
#define IS_MPI_TRACE_ON(tid) \
  (vt_is_alive && VTTHRD_MPI_TRACING_ENABLED(VTThrdv[tid]))

/* temporary switch off MPI tracing */
#define MPI_TRACE_OFF(tid) \
  VT_SUSPEND_MALLOC_TRACING(tid); \
  VTTHRD_MPI_TRACING_ENABLED(VTThrdv[tid]) = (env_mpitrace && MPI_TRACE_INSIDE)
#define MPI_TRACE_ON(tid) \
  VT_RESUME_MALLOC_TRACING(tid); \
  VTTHRD_MPI_TRACING_ENABLED(VTThrdv[tid]) = env_mpitrace

/* flag: MPI tracing enabled (env. VT_MPITRACE)? */
static uint8_t env_mpitrace = 1;

/* flag: trace MPI communication events although its corresponding functions
   are filtered (env. VT_MPI_IGNORE_FILTER)? */
static uint8_t env_mpi_ignore_filter = 0;

/* dummy main function ("user") entered */
static uint8_t dummy_main_entered = 0;

/* thread id where the dummy main function was entered */
static uint32_t dummy_main_tid = 0;

/* flag: MPI_Init[_thread] called? */
static uint8_t mpi_init_called = 0;

#if defined(HAVE_MPI_FINALIZED) && HAVE_MPI_FINALIZED
  /* flag: MPI_Finalize called? */
  static uint8_t mpi_finalize_called = 0;
#endif /* HAVE_MPI_FINALIZED */

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
  /* flag: MPI initialized with multithreading? */
  static uint8_t is_mpi_multithreaded = 0;
#endif /* HAVE_MPI2_THREAD */

#if defined(HAVE_MPI2_1SIDED) && HAVE_MPI2_1SIDED
  static uint8_t is_rma_putre = 1;
#endif /* HAVE_MPI2_1SIDED */

static MPI_Status* my_status_array = 0;
static VT_MPI_INT my_status_array_size = 0;

static MPI_Status* get_status_array(VT_MPI_INT size)
{
  if (my_status_array_size == 0)
    {
      /* -- never used: initialize -- */
      my_status_array = (MPI_Status*)malloc(size * sizeof(MPI_Status));
      if ( my_status_array == NULL ) vt_error();
      my_status_array_size = size;
    }
  else if (size > my_status_array_size)
    {
      /* -- not enough room: expand -- */
      my_status_array =
        (MPI_Status*)realloc(my_status_array, size * sizeof(MPI_Status));
      if ( my_status_array == NULL ) vt_error();
      my_status_array_size = size;
    }

  return my_status_array;
}

void vt_mpiwrap_init()
{
  /* MPI tracing enabled? */
  env_mpitrace = vt_env_mpitrace();

  /* trace MPI communication events although its corresponding functions
     are filtered? */
  env_mpi_ignore_filter = vt_env_mpi_ignore_filter();
}

void vt_mpiwrap_finalize()
{
  /* exit the dummy main function, if necessary */
  if (dummy_main_entered)
    {
      uint64_t time = vt_pform_wtime();
      vt_exit_user(dummy_main_tid, &time);
    }
}

/*
 *-----------------------------------------------------------------------------
 *
 * Init and finalize
 *
 *-----------------------------------------------------------------------------
 */

/* -- MPI_Init -- */

VT_MPI_INT MPI_Init(VT_MPI_INT* argc, char*** argv)
{
  VT_MPI_INT result;
  uint64_t time;
  uint32_t tid;

  /* first event? */
  if (!vt_is_alive)
    {
      /* initialize VT */
      vt_open();

      /* get calling thread id */
      GET_THREAD_ID(tid);

      /* enter the dummy main function */
      time = vt_pform_wtime();
      vt_enter_user(tid, &time);

      dummy_main_entered = 1;
      dummy_main_tid = tid;
    }
  else
    {
      /* get calling thread id */
      GET_THREAD_ID(tid);
    }

  if (IS_MPI_TRACE_ON(tid))
    {
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded =
        vt_enter(tid, &time, vt_mpi_regid[VT__MPI_INIT]);

      VT_UNIMCI_CHECK_PRE(MPI_Init, (argc, argv, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Init(argc, argv);

      VT_UNIMCI_CHECK_POST(MPI_Init, (argc, argv, "", 0, 0),
        was_recorded, &time);

      /* initialize mpi event handling */
      vt_mpi_init(0);

      /* initialize communicator management */
      vt_comm_init();

#if defined (HAVE_MPI2_IO) && HAVE_MPI2_IO
      /* initialize file management */
      vt_mpifile_init();
#endif /* HAVE_MPI2_IO */

      mpi_init_called = 1;

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Init(argc, argv);

      /* initialize mpi event handling */
      vt_mpi_init(0);

      mpi_init_called = 1;
    }

  return result;
}

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD

/* -- MPI_Init_thread -- */

VT_MPI_INT MPI_Init_thread(VT_MPI_INT* argc, char*** argv, VT_MPI_INT required,
                           VT_MPI_INT* provided)
{
  VT_MPI_INT result;
  uint64_t time;
  uint32_t tid;

  /* first event? */
  if (!vt_is_alive)
    {
      /* initialize VT */
      vt_open();

      /* get calling thread id */
      GET_THREAD_ID(tid);

      /* enter the dummy main function */
      time = vt_pform_wtime();
      vt_enter_user(tid, &time);

      dummy_main_entered = 1;
      dummy_main_tid = tid;
    }
  else
    {
      /* get calling thread id */
      GET_THREAD_ID(tid);
    }

  if (IS_MPI_TRACE_ON(tid))
    {
      uint8_t was_recorded;

      VT_MPI_INT me;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded =
        vt_enter(tid, &time, vt_mpi_regid[VT__MPI_INIT_THREAD]);

      VT_UNIMCI_CHECK_PRE(MPI_Init_thread,
        (argc, argv, required, provided, "", 0, 0), was_recorded, &time);

      result = PMPI_Init_thread(argc, argv, required, provided);

      VT_UNIMCI_CHECK_POST(MPI_Init_thread,
        (argc, argv, required, provided, "", 0, 0), was_recorded, &time);

      PMPI_Comm_rank(MPI_COMM_WORLD, &me);

      switch (required)
        {
          case MPI_THREAD_SINGLE:
            break;
          case MPI_THREAD_FUNNELED:
            if (*provided == MPI_THREAD_FUNNELED && tid != 0)
              {
                if (me == 0)
                  {
                    vt_warning("The MPI thread support level "
                               "MPI_THREAD_FUNNELED is not yet fully "
                               "supported. MPI_Init_thread must be called from "
                               "the master thread. No MPI communication events "
                               "will be recorded. Continuing.");

                  }
                is_mpi_multithreaded = 1;
              }
            break;
          case MPI_THREAD_SERIALIZED:
          case MPI_THREAD_MULTIPLE:
            if (*provided == MPI_THREAD_SERIALIZED ||
                *provided == MPI_THREAD_MULTIPLE)
              {
                if (me == 0)
                  {
                    vt_warning("The MPI thread support levels "
                               "MPI_THREAD_SERIALIZED and MPI_THREAD_MULTIPLE "
                               "are not yet supported. No MPI communication "
                               "events will be recorded. Continuing.");
                  }
                is_mpi_multithreaded = 1;
              }
            break;
          default:
            vt_error_msg("Unknown level of MPI thread support requested");
            break;
        }

      /* initialize mpi event handling */
      vt_mpi_init(is_mpi_multithreaded);

      if (!is_mpi_multithreaded)
        {
          /* initialize communicator management */
          vt_comm_init();

#if defined (HAVE_MPI2_IO) && HAVE_MPI2_IO
          /* initialize file management */
          vt_mpifile_init();
#endif /* HAVE_MPI2_IO */
        }

      mpi_init_called = 1;

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Init_thread(argc, argv, required, provided);

      /* initialize mpi event handling */
      vt_mpi_init(0);

      mpi_init_called = 1;
    }

  return result;
}

#endif /* HAVE_MPI2_THREAD */

/* -- MPI_Initialized -- */

VT_MPI_INT MPI_Initialized(VT_MPI_INT* flag)
{
  VT_MPI_INT result;
  uint64_t time;
  uint32_t tid;

  /* first event? */
  if (!vt_is_alive)
    {
      /* initialize VT */
      vt_open();

      /* get calling thread id */
      GET_THREAD_ID(tid);

      /* enter the dummy main function */
      time = vt_pform_wtime();
      vt_enter_user(tid, &time);

      dummy_main_entered = 1;
      dummy_main_tid = tid;
    }
  else
    {
      /* get calling thread id */
      GET_THREAD_ID(tid);
    }

  if (IS_MPI_TRACE_ON(tid))
    {
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_INITIALIZED]);

      VT_UNIMCI_CHECK_PRE(MPI_Initialized,
        (flag, "", 0, 0), was_recorded, &time);

      result = PMPI_Initialized(flag);

      VT_UNIMCI_CHECK_POST(MPI_Initialized,
        (flag, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Initialized(flag);
    }

  return result;
}

/* -- MPI_Finalize -- */

VT_MPI_INT MPI_Finalize(void)
{
  VT_MPI_INT result;
  uint64_t time;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      vt_enter(tid, &time, vt_mpi_regid[VT__MPI_FINALIZE]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          /* finalize communicator, request, and file management */
          vt_comm_finalize();
          vt_request_finalize();
#if defined (HAVE_MPI2_IO) && HAVE_MPI2_IO
          vt_mpifile_finalize();
#endif /* HAVE_MPI2_IO */
        }

      /* finalize mpi event handling */
      vt_mpi_finalize();

#if defined(HAVE_MPI_FINALIZED) && HAVE_MPI_FINALIZED
      mpi_finalize_called = 1;
#endif /* HAVE_MPI_FINALIZED */
      result = MPI_SUCCESS;

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      /* finalize mpi event handling */
      vt_mpi_finalize();

#if defined(HAVE_MPI_FINALIZED) && HAVE_MPI_FINALIZED
      mpi_finalize_called = 1;
#endif /* HAVE_MPI_FINALIZED */

      result = MPI_SUCCESS;
    }

  /* close VampirTrace, if necessary */
  if (vt_close_on_mpi_finalize)
    vt_close();

  return result;
}

#if defined(HAVE_MPI_FINALIZED) && HAVE_MPI_FINALIZED

/* -- MPI_Finalized -- */

VT_MPI_INT MPI_Finalized(VT_MPI_INT* flag)
{
  *flag = (VT_MPI_INT)mpi_finalize_called;

  return MPI_SUCCESS;
}

#endif /* HAVE_MPI_FINALIZED */

/*
 *-----------------------------------------------------------------------------
 *
 * Communicator management
 *
 *-----------------------------------------------------------------------------
 */

/* ------- Constructors ------- */

/* -- MPI_Comm_dup -- */

VT_MPI_INT MPI_Comm_dup(MPI_Comm comm, MPI_Comm* newcomm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_COMM_DUP]);

      VT_UNIMCI_CHECK_PRE(MPI_Comm_dup,
        (comm, newcomm, "", 0, 0), was_recorded, &time);

      result = PMPI_Comm_dup(comm, newcomm);

      VT_UNIMCI_CHECK_POST(MPI_Comm_dup,
        (comm, newcomm, "", 0, 0), was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
      vt_comm_create(*newcomm);

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Comm_dup(comm, newcomm);
    }

  return result;
}

/* -- MPI_Comm_create -- */

VT_MPI_INT MPI_Comm_create(MPI_Comm comm, MPI_Group group, MPI_Comm* newcomm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_COMM_CREATE]);

      VT_UNIMCI_CHECK_PRE(MPI_Comm_create,
        (comm, group, newcomm, "", 0, 0), was_recorded, &time);

      result = PMPI_Comm_create(comm, group, newcomm);

      VT_UNIMCI_CHECK_POST(MPI_Comm_create,
        (comm, group, newcomm, "", 0, 0), was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (*newcomm != MPI_COMM_NULL)
            vt_comm_create(*newcomm);
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Comm_create(comm, group, newcomm);
    }

  return result;
}

/* -- MPI_Comm_group -- */

VT_MPI_INT MPI_Comm_group( MPI_Comm comm, MPI_Group* group )
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_COMM_GROUP]);

      VT_UNIMCI_CHECK_PRE(MPI_Comm_group,
        (comm, group, "", 0, 0), was_recorded, &time);

      result = PMPI_Comm_group(comm, group);

      VT_UNIMCI_CHECK_POST(MPI_Comm_group,
         (comm, group, "", 0, 0), was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (*group != MPI_GROUP_NULL)
            vt_group_create(*group);
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Comm_group(comm, group);
    }

  return result;
}

/* -- MPI_Comm_remote_group -- */

VT_MPI_INT MPI_Comm_remote_group(MPI_Comm comm, MPI_Group* group)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_COMM_REMOTE_GROUP]);

      VT_UNIMCI_CHECK_PRE(MPI_Comm_remote_group,
        (comm, group, "", 0, 0), was_recorded, &time);

      result = PMPI_Comm_remote_group(comm, group);

      VT_UNIMCI_CHECK_POST(MPI_Comm_remote_group,
        (comm, group, "", 0, 0), was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (*group != MPI_GROUP_NULL)
            vt_group_create(*group);
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Comm_remote_group(comm, group);
    }

  return result;
}

/* -- MPI_Comm_split -- */

VT_MPI_INT MPI_Comm_split(MPI_Comm comm, VT_MPI_INT color, VT_MPI_INT key,
                          MPI_Comm* newcomm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_COMM_SPLIT]);

      VT_UNIMCI_CHECK_PRE(MPI_Comm_split,
        (comm, color, key, newcomm, "", 0, 0), was_recorded, &time);

      result = PMPI_Comm_split(comm, color, key, newcomm);

      VT_UNIMCI_CHECK_POST(MPI_Comm_split,
        (comm, color, key, newcomm, "", 0, 0), was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (*newcomm != MPI_COMM_NULL)
            vt_comm_create(*newcomm);
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Comm_split(comm, color, key, newcomm);
    }

  return result;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Group management
 *
 *-----------------------------------------------------------------------------
 */

/* -- MPI_Group_union -- */

VT_MPI_INT MPI_Group_union(MPI_Group group1, MPI_Group group2,
                           MPI_Group* newgroup)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_GROUP_UNION]);

      VT_UNIMCI_CHECK_PRE(MPI_Group_union,
        (group1, group2, newgroup, "", 0, 0), was_recorded, &time);

      result = PMPI_Group_union(group1, group2, newgroup);

      VT_UNIMCI_CHECK_POST(MPI_Group_union,
        (group1, group2, newgroup, "", 0, 0), was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (*newgroup != MPI_GROUP_NULL && *newgroup != MPI_GROUP_EMPTY)
            vt_group_create(*newgroup);
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Group_union(group1, group2, newgroup);
    }

  return result;
}

/* -- MPI_Group_intersection -- */

VT_MPI_INT MPI_Group_intersection(MPI_Group group1, MPI_Group group2,
                                  MPI_Group* newgroup)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_GROUP_INTERSECTION]);

      VT_UNIMCI_CHECK_PRE(MPI_Group_intersection,
        (group1, group2, newgroup, "", 0, 0), was_recorded, &time);

      result = PMPI_Group_intersection(group1, group2, newgroup);

      VT_UNIMCI_CHECK_POST(MPI_Group_intersection,
        (group1, group2, newgroup, "", 0, 0), was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (*newgroup != MPI_GROUP_NULL && *newgroup != MPI_GROUP_EMPTY)
            vt_group_create(*newgroup);
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Group_intersection(group1, group2, newgroup);
    }

  return result;
}

/* -- MPI_Group_difference -- */

VT_MPI_INT MPI_Group_difference(MPI_Group group1, MPI_Group group2,
                                MPI_Group* newgroup)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_GROUP_DIFFERENCE]);

      VT_UNIMCI_CHECK_PRE(MPI_Group_difference,
        (group1, group2, newgroup, "", 0, 0), was_recorded, &time);

      result = PMPI_Group_difference(group1, group2, newgroup);

      VT_UNIMCI_CHECK_POST(MPI_Group_difference,
        (group1, group2, newgroup, "", 0, 0), was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (*newgroup != MPI_GROUP_NULL && *newgroup != MPI_GROUP_EMPTY)
            vt_group_create(*newgroup);
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Group_difference(group1, group2, newgroup);
    }

  return result;
}

/* -- MPI_Group_incl -- */

VT_MPI_INT MPI_Group_incl(MPI_Group group, VT_MPI_INT n,
                          CONST VT_MPI_INT* ranks, MPI_Group* newgroup)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_GROUP_INCL]);

      VT_UNIMCI_CHECK_PRE(MPI_Group_incl,
        (group, n, ranks, newgroup, "", 0, 0), was_recorded, &time);

      result = PMPI_Group_incl(group, n, ranks, newgroup);

      VT_UNIMCI_CHECK_POST(MPI_Group_incl,
        (group, n, ranks, newgroup, "", 0, 0), was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (*newgroup != MPI_GROUP_NULL)
            vt_group_create(*newgroup);
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Group_incl(group, n, ranks, newgroup);
    }

  return result;
}

/* -- MPI_Group_excl -- */

VT_MPI_INT MPI_Group_excl(MPI_Group group, VT_MPI_INT n,
                          CONST VT_MPI_INT* ranks, MPI_Group* newgroup)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_GROUP_EXCL]);

      VT_UNIMCI_CHECK_PRE(MPI_Group_excl,
        (group, n, ranks, newgroup, "", 0, 0), was_recorded, &time);

      result = PMPI_Group_excl(group, n, ranks, newgroup);

      VT_UNIMCI_CHECK_POST(MPI_Group_excl,
        (group, n, ranks, newgroup, "", 0, 0), was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (*newgroup != MPI_GROUP_NULL && *newgroup != MPI_GROUP_EMPTY)
            vt_group_create(*newgroup);
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Group_excl(group, n, ranks, newgroup);
    }

  return result;
}

/* -- MPI_Group_range_incl -- */

VT_MPI_INT MPI_Group_range_incl(MPI_Group group, VT_MPI_INT n,
                                VT_MPI_INT ranges[][3], MPI_Group* newgroup)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_GROUP_RANGE_INCL]);

      VT_UNIMCI_CHECK_PRE(MPI_Group_range_incl,
        (group, n, ranges, newgroup, "", 0, 0), was_recorded, &time);

      result = PMPI_Group_range_incl(group, n, ranges, newgroup);

      VT_UNIMCI_CHECK_POST(MPI_Group_range_incl,
        (group, n, ranges, newgroup, "", 0, 0), was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (*newgroup != MPI_GROUP_NULL)
            vt_group_create(*newgroup);
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Group_range_incl(group, n, ranges, newgroup);
    }

  return result;
}

/* -- MPI_Group_range_excl --*/

VT_MPI_INT MPI_Group_range_excl(MPI_Group group, VT_MPI_INT n,
                                VT_MPI_INT ranges[][3], MPI_Group* newgroup)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_GROUP_RANGE_EXCL]);

      VT_UNIMCI_CHECK_PRE(MPI_Group_range_excl,
        (group, n, ranges, newgroup, "", 0, 0), was_recorded, &time);

      result = PMPI_Group_range_excl(group, n, ranges, newgroup);

      VT_UNIMCI_CHECK_POST(MPI_Group_range_excl,
        (group, n, ranges, newgroup, "", 0, 0), was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (*newgroup != MPI_GROUP_NULL && *newgroup != MPI_GROUP_EMPTY)
            vt_group_create(*newgroup);
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Group_range_excl(group, n, ranges, newgroup);
    }

  return result;
}

/* -- MPI_Group_free -- */

VT_MPI_INT MPI_Group_free(MPI_Group* group)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_GROUP_FREE]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          vt_group_free(*group);
        }

      VT_UNIMCI_CHECK_PRE(MPI_Group_free,
        (group, "", 0, 0), was_recorded, &time);

      result = PMPI_Group_free(group);

      VT_UNIMCI_CHECK_POST(MPI_Group_free,
        (group, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Group_free(group);
    }

  return result;
}

#if defined(HAVE_MPI2_1SIDED) && HAVE_MPI2_1SIDED

/*
 *-----------------------------------------------------------------------------
 *
 * Window management
 *
 *-----------------------------------------------------------------------------
 */

/* ------- Constructor ------- */

/* -- MPI_Win_create -- */

VT_MPI_INT MPI_Win_create(void* base, MPI_Aint size, VT_MPI_INT disp_unit,
                          MPI_Info info, MPI_Comm comm, MPI_Win* win)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_WIN_CREATE]);

      VT_UNIMCI_CHECK_PRE(MPI_Win_create,
        (base, size, disp_unit, info, comm, win, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Win_create(base, size, disp_unit, info, comm, win);

      VT_UNIMCI_CHECK_POST(MPI_Win_create,
        (base, size, disp_unit, info, comm, win, "", 0, 0),
        was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (*win != MPI_WIN_NULL)
            vt_win_create(*win, comm);
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Win_create(base, size, disp_unit, info, comm, win);
    }

  return result;
}

/* ------- Destructor ------- */

/* -- MPI_Win_free -- */

VT_MPI_INT MPI_Win_free(MPI_Win* win)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_WIN_FREE]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          vt_win_free(*win);
        }

      VT_UNIMCI_CHECK_PRE(MPI_Win_free,
        (win, "", 0, 0), was_recorded, &time);

      result = PMPI_Win_free(win);

      VT_UNIMCI_CHECK_POST(MPI_Win_free,
        (win, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Win_free(win);
    }

  return result;
}

/* -- MPI_Win_get_group -- */

VT_MPI_INT MPI_Win_get_group(MPI_Win win, MPI_Group* group)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      /*uint8_t was_recorded;*/

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      /*was_recorded =*/ vt_enter(tid, &time, vt_mpi_regid[VT__MPI_WIN_GET_GROUP]);

      /* UNIMCI_check_<pre|post>__MPI_Win_get_group not yet available;
         call PMPI function directly */
      result = PMPI_Win_get_group(win, group);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (*group != MPI_GROUP_NULL)
            vt_group_create(*group);
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Win_get_group(win, group);
    }

  return result;
}

#endif /* HAVE_MPI2_1SIDED */

/*
 *-----------------------------------------------------------------------------
 *
 * Cartesian Toplogy functions
 *
 *-----------------------------------------------------------------------------
 */

/* -- MPI_Cart_create -- */

VT_MPI_INT MPI_Cart_create(MPI_Comm comm_old, VT_MPI_INT ndims,
                           CONST VT_MPI_INT* dims, CONST VT_MPI_INT* periodv,
                           VT_MPI_INT reorder, MPI_Comm* comm_cart)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_CART_CREATE]);

      VT_UNIMCI_CHECK_PRE(MPI_Cart_create,
        (comm_old, ndims, dims, periodv, reorder, comm_cart, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Cart_create(comm_old, ndims, dims, periodv,
                                reorder, comm_cart);

      VT_UNIMCI_CHECK_POST(MPI_Cart_create,
        (comm_old, ndims, dims, periodv, reorder, comm_cart, "", 0, 0),
        was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (*comm_cart != MPI_COMM_NULL)
            vt_comm_create(*comm_cart);
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Cart_create(comm_old, ndims, dims, periodv,
                                      reorder, comm_cart);
    }

  return result;
}

/* -- MPI_Cart_sub -- */

VT_MPI_INT MPI_Cart_sub(MPI_Comm comm, CONST VT_MPI_INT* rem_dims, MPI_Comm* newcomm )
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_CART_SUB]);

      VT_UNIMCI_CHECK_PRE(MPI_Cart_sub,
        (comm, rem_dims, newcomm, "", 0, 0), was_recorded, &time);

      result = PMPI_Cart_sub(comm, rem_dims, newcomm);

      VT_UNIMCI_CHECK_POST(MPI_Cart_sub,
        (comm, rem_dims, newcomm, "", 0, 0), was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (*newcomm != MPI_COMM_NULL)
            vt_comm_create(*newcomm);
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Cart_sub(comm, rem_dims, newcomm);
    }

  return result;
}

/* -- MPI_Graph_create -- */

VT_MPI_INT MPI_Graph_create(MPI_Comm comm_old, VT_MPI_INT nnodes,
                            CONST VT_MPI_INT* index, CONST VT_MPI_INT* edges,
                            VT_MPI_INT reorder, MPI_Comm* comm_graph)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_GRAPH_CREATE]);

      VT_UNIMCI_CHECK_PRE(MPI_Graph_create,
        (comm_old, nnodes, index, edges, reorder, comm_graph, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Graph_create(comm_old, nnodes, index, edges, reorder,
                                 comm_graph);

      VT_UNIMCI_CHECK_POST(MPI_Graph_create,
        (comm_old, nnodes, index, edges, reorder, comm_graph, "", 0, 0),
        was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (*comm_graph != MPI_COMM_NULL)
            vt_comm_create(*comm_graph);
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Graph_create(comm_old, nnodes, index, edges, reorder,
                                 comm_graph);
    }

  return result;
}

/* -- MPI_Intercomm_create -- */

VT_MPI_INT MPI_Intercomm_create(MPI_Comm local_comm, VT_MPI_INT local_leader,
                                 MPI_Comm peer_comm, VT_MPI_INT remote_leader,
                                 VT_MPI_INT tag, MPI_Comm* newintercomm)

{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_INTERCOMM_CREATE]);

      VT_UNIMCI_CHECK_PRE(MPI_Intercomm_create,
        (local_comm, local_leader, peer_comm, remote_leader, tag, newintercomm,
         "", 0, 0), was_recorded, &time);

      result = PMPI_Intercomm_create(local_comm, local_leader, peer_comm,
                                     remote_leader, tag, newintercomm);

      VT_UNIMCI_CHECK_POST(MPI_Intercomm_create,
        (local_comm, local_leader, peer_comm, remote_leader, tag, newintercomm,
         "", 0, 0), was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (*newintercomm != MPI_COMM_NULL)
            vt_comm_create(*newintercomm);
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Intercomm_create(local_comm, local_leader, peer_comm,
                                     remote_leader, tag, newintercomm);
    }

  return result;
}

/* -- MPI_Intercomm_merge -- */

VT_MPI_INT MPI_Intercomm_merge(MPI_Comm intercomm, VT_MPI_INT high,
                               MPI_Comm* newcomm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_INTERCOMM_MERGE]);

      VT_UNIMCI_CHECK_PRE(MPI_Intercomm_merge,
        (intercomm, high, newcomm, "", 0, 0), was_recorded, &time);

      result = PMPI_Intercomm_merge(intercomm, high, newcomm);

      VT_UNIMCI_CHECK_POST(MPI_Intercomm_merge,
        (intercomm, high, newcomm, "", 0, 0), was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (*newcomm != MPI_COMM_NULL)
            vt_comm_create(*newcomm);
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Intercomm_merge(intercomm, high, newcomm);
    }

  return result;
}


/* ------- Destructors ------- */

/* -- MPI_Comm_free -- */

VT_MPI_INT MPI_Comm_free(MPI_Comm* comm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_COMM_FREE]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          vt_comm_free(*comm);
        }

      VT_UNIMCI_CHECK_PRE(MPI_Comm_free,
        (comm, "", 0, 0), was_recorded, &time);

      result = PMPI_Comm_free(comm);

      VT_UNIMCI_CHECK_POST(MPI_Comm_free,
        (comm, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Comm_free(comm);
    }

  return result;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Point-to-point communication
 *
 *-----------------------------------------------------------------------------
 */

/* ------- Synchronous ------- */


/* -- MPI_Send -- */

VT_MPI_INT MPI_Send(CONST void* buf, VT_MPI_INT count, MPI_Datatype datatype,
                    VT_MPI_INT dest, VT_MPI_INT tag, MPI_Comm comm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_SEND]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (dest != MPI_PROC_NULL && (was_recorded || env_mpi_ignore_filter))
            {
              VT_MPI_INT sz;
              PMPI_Type_size(datatype, &sz);
              vt_mpi_send(tid, &time, VT_RANK_TO_PE(dest, comm),
                          VT_COMM_ID(comm), tag, count * sz);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Send,
        (buf, count, datatype, dest, tag, comm, "", 0, 0), was_recorded, &time);

      result = PMPI_Send(buf, count, datatype, dest, tag, comm);

      VT_UNIMCI_CHECK_POST(MPI_Send,
        (buf, count, datatype, dest, tag, comm, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Send(buf, count, datatype, dest, tag, comm);
    }

  return result;
}

/* -- MPI_Bsend -- */

VT_MPI_INT MPI_Bsend(CONST void* buf, VT_MPI_INT count, MPI_Datatype datatype,
                     VT_MPI_INT dest, VT_MPI_INT tag, MPI_Comm comm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_BSEND]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (dest != MPI_PROC_NULL && (was_recorded || env_mpi_ignore_filter))
            {
              VT_MPI_INT sz;
              PMPI_Type_size(datatype, &sz);
              vt_mpi_send(tid, &time, VT_RANK_TO_PE(dest, comm),
                          VT_COMM_ID(comm), tag, count * sz);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Bsend,
        (buf, count, datatype, dest, tag, comm, "", 0, 0), was_recorded, &time);

      result = PMPI_Bsend(buf, count, datatype, dest, tag, comm);

      VT_UNIMCI_CHECK_POST(MPI_Bsend,
        (buf, count, datatype, dest, tag, comm, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Bsend(buf, count, datatype, dest, tag, comm);
    }

  return result;
}

/* -- MPI_Rsend -- */

VT_MPI_INT MPI_Rsend(CONST void* buf, VT_MPI_INT count, MPI_Datatype datatype,
                     VT_MPI_INT dest, VT_MPI_INT tag, MPI_Comm comm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_RSEND]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (dest != MPI_PROC_NULL && (was_recorded || env_mpi_ignore_filter))
            {
              VT_MPI_INT sz;
              PMPI_Type_size(datatype, &sz);
              vt_mpi_send(tid, &time, VT_RANK_TO_PE(dest, comm),
                          VT_COMM_ID(comm), tag, count * sz);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Rsend,
        (buf, count, datatype, dest, tag, comm, "", 0, 0), was_recorded, &time);

      result = PMPI_Rsend(buf, count, datatype, dest, tag, comm);

      VT_UNIMCI_CHECK_POST(MPI_Rsend,
        (buf, count, datatype, dest, tag, comm, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Rsend(buf, count, datatype, dest, tag, comm);
    }

  return result;
}

/* -- MPI_Ssend -- */

VT_MPI_INT MPI_Ssend(CONST void* buf, VT_MPI_INT count, MPI_Datatype datatype,
                     VT_MPI_INT dest, VT_MPI_INT tag, MPI_Comm comm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_SSEND]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (dest != MPI_PROC_NULL && (was_recorded || env_mpi_ignore_filter))
            {
              VT_MPI_INT sz;
              PMPI_Type_size(datatype, &sz);
              vt_mpi_send(tid, &time, VT_RANK_TO_PE(dest, comm),
                          VT_COMM_ID(comm), tag, count * sz);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Ssend,
        (buf, count, datatype, dest, tag, comm, "", 0, 0), was_recorded, &time);

      result = PMPI_Ssend(buf, count, datatype, dest, tag, comm);

      VT_UNIMCI_CHECK_POST(MPI_Ssend,
        (buf, count, datatype, dest, tag, comm, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Ssend(buf, count, datatype, dest, tag, comm);
    }

  return result;
}

/* -- MPI_Recv -- */

VT_MPI_INT MPI_Recv(void* buf, VT_MPI_INT count, MPI_Datatype datatype,
                    VT_MPI_INT source, VT_MPI_INT tag, MPI_Comm comm,
                    MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_Status mystatus;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_RECV]);

      if (status == MPI_STATUS_IGNORE) status = &mystatus;

      VT_UNIMCI_CHECK_PRE(MPI_Recv,
        (buf, count, datatype, source, tag, comm, status, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Recv(buf, count, datatype, source, tag, comm, status);

      VT_UNIMCI_CHECK_POST(MPI_Recv,
        (buf, count, datatype, source, tag, comm, status, "", 0, 0),
        was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (source != MPI_PROC_NULL && result == MPI_SUCCESS &&
              (was_recorded || env_mpi_ignore_filter))
            {
              VT_MPI_INT sz;
              PMPI_Type_size(datatype, &sz);
              PMPI_Get_count(status, datatype, &count);
              if (count == MPI_UNDEFINED)
                count = 0;
              vt_mpi_recv(tid, &time, VT_RANK_TO_PE(status->MPI_SOURCE, comm),
                          VT_COMM_ID(comm), status->MPI_TAG, count * sz);
            }
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Recv(buf, count, datatype, source, tag, comm, status);
    }

  return result;
}

/* -- MPI_Sendrecv -- */

VT_MPI_INT MPI_Sendrecv(CONST void* sendbuf, VT_MPI_INT sendcount,
                        MPI_Datatype sendtype, VT_MPI_INT dest,
                        VT_MPI_INT sendtag, void* recvbuf, VT_MPI_INT recvcount,
                        MPI_Datatype recvtype, VT_MPI_INT source,
                        VT_MPI_INT recvtag, MPI_Comm comm, MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_Status mystatus;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_SENDRECV]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (dest != MPI_PROC_NULL && (was_recorded || env_mpi_ignore_filter))
            {
              VT_MPI_INT sz;
              PMPI_Type_size(sendtype, &sz);
              vt_mpi_send(tid, &time, VT_RANK_TO_PE(dest, comm),
                          VT_COMM_ID(comm), sendtag, sendcount * sz);
            }

          if (status == MPI_STATUS_IGNORE) status = &mystatus;
        }

      VT_UNIMCI_CHECK_PRE(MPI_Sendrecv,
        (sendbuf, sendcount, sendtype, dest, sendtag, recvbuf, recvcount,
        recvtype, source, recvtag, comm, status, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Sendrecv(sendbuf, sendcount, sendtype, dest, sendtag,
                             recvbuf, recvcount, recvtype, source, recvtag,
                             comm, status);

      VT_UNIMCI_CHECK_POST(MPI_Sendrecv,
        (sendbuf, sendcount, sendtype, dest, sendtag, recvbuf, recvcount,
        recvtype, source, recvtag, comm, status, "", 0, 0),
        was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (source != MPI_PROC_NULL && result == MPI_SUCCESS &&
              (was_recorded || env_mpi_ignore_filter))
            {
              VT_MPI_INT sz;
              PMPI_Type_size(recvtype, &sz);
              PMPI_Get_count(status, recvtype, &recvcount);
              if (recvcount == MPI_UNDEFINED)
                recvcount = 0;
              vt_mpi_recv(tid, &time, VT_RANK_TO_PE(status->MPI_SOURCE, comm),
                          VT_COMM_ID(comm), status->MPI_TAG,
                          recvcount * sz);
            }
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Sendrecv(sendbuf, sendcount, sendtype, dest, sendtag,
                             recvbuf, recvcount, recvtype, source, recvtag,
                             comm, status);
    }

  return result;
}

/* -- MPI_Sendrecv_replace -- */

VT_MPI_INT MPI_Sendrecv_replace(void* buf, VT_MPI_INT count,
                                MPI_Datatype datatype, VT_MPI_INT dest,
                                VT_MPI_INT sendtag, VT_MPI_INT source,
                                VT_MPI_INT recvtag, MPI_Comm comm,
                                MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_Status mystatus;
      VT_MPI_INT sz;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_SENDRECV_REPLACE]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          PMPI_Type_size(datatype, &sz);
          if (dest != MPI_PROC_NULL && (was_recorded || env_mpi_ignore_filter))
            {
              vt_mpi_send(tid, &time, VT_RANK_TO_PE(dest, comm),
                          VT_COMM_ID(comm), sendtag, count * sz);
            }

          if (status == MPI_STATUS_IGNORE) status = &mystatus;
        }

      VT_UNIMCI_CHECK_PRE(MPI_Sendrecv_replace,
        (buf, count, datatype, dest, sendtag, source, recvtag, comm, status,
         "", 0, 0), was_recorded, &time);

      result = PMPI_Sendrecv_replace(buf, count, datatype, dest, sendtag,
                                     source, recvtag, comm, status);

      VT_UNIMCI_CHECK_POST(MPI_Sendrecv_replace,
        (buf, count, datatype, dest, sendtag, source, recvtag, comm, status,
         "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (source != MPI_PROC_NULL && result == MPI_SUCCESS)
            {
              vt_mpi_recv(tid, &time, VT_RANK_TO_PE(status->MPI_SOURCE, comm),
                          VT_COMM_ID(comm), status->MPI_TAG, count * sz);
            }
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Sendrecv_replace(buf, count, datatype, dest, sendtag,
                                     source, recvtag, comm, status);
    }

  return result;
}

/* ------- Aynchronous ------- */

/* -- MPI_Isend -- */

VT_MPI_INT MPI_Isend(CONST void* buf, VT_MPI_INT count, MPI_Datatype datatype,
                     VT_MPI_INT dest, VT_MPI_INT tag, MPI_Comm comm,
                     MPI_Request* request)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_ISEND]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (dest != MPI_PROC_NULL && (was_recorded || env_mpi_ignore_filter))
            {
              VT_MPI_INT sz;
              PMPI_Type_size(datatype, &sz);
              vt_mpi_send(tid, &time, VT_RANK_TO_PE(dest, comm),
                          VT_COMM_ID(comm), tag, count * sz);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Isend,
        (buf, count, datatype, dest, tag, comm, request, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Isend(buf, count, datatype, dest, tag, comm, request);

      VT_UNIMCI_CHECK_POST(MPI_Isend,
        (buf, count, datatype, dest, tag, comm, request, "", 0, 0),
        was_recorded, &time);

      /* no need to save send request as we already created send event,
         so why saving request, and then have all kinds of trouble handling
         it correctly
#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif
        {
          if (dest != MPI_PROC_NULL && result == MPI_SUCCESS)
            vt_request_create(*request, ERF_SEND, tag, dest, count * sz, comm);
        }
       */

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Isend(buf, count, datatype, dest, tag, comm, request);
    }

  return result;
}

/* -- MPI_Irecv -- */

VT_MPI_INT MPI_Irecv(void* buf, VT_MPI_INT count, MPI_Datatype datatype,
                     VT_MPI_INT source, VT_MPI_INT tag, MPI_Comm comm,
                     MPI_Request* request)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_IRECV]);

      VT_UNIMCI_CHECK_PRE(MPI_Irecv,
        (buf, count, datatype, source, tag, comm, request, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Irecv(buf, count, datatype, source, tag, comm, request);

      VT_UNIMCI_CHECK_POST(MPI_Irecv,
        (buf, count, datatype, source, tag, comm, request, "", 0, 0),
        was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (source != MPI_PROC_NULL && result == MPI_SUCCESS)
            {
              VT_MPI_INT sz;
              PMPI_Type_size(datatype, &sz);
              vt_request_create(*request, ERF_RECV,
                                tag, 0, count * sz, datatype, comm);
            }
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Irecv(buf, count, datatype, source, tag, comm, request);
    }

  return result;
}

/* -- MPI_Ibsend -- */

VT_MPI_INT MPI_Ibsend(CONST void* buf, VT_MPI_INT count, MPI_Datatype datatype,
                      VT_MPI_INT dest, VT_MPI_INT tag, MPI_Comm comm,
                      MPI_Request* request)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_IBSEND]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (dest != MPI_PROC_NULL && (was_recorded || env_mpi_ignore_filter))
            {
              VT_MPI_INT sz;
              PMPI_Type_size(datatype, &sz);
              vt_mpi_send(tid, &time, VT_RANK_TO_PE(dest, comm),
                          VT_COMM_ID(comm), tag, count * sz);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Ibsend,
        (buf, count, datatype, dest, tag, comm, request, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Ibsend(buf, count, datatype, dest, tag, comm, request);

      VT_UNIMCI_CHECK_POST(MPI_Ibsend,
        (buf, count, datatype, dest, tag, comm, request, "", 0, 0),
        was_recorded, &time);

      /* no need to save send request as we already created send event,
         so why saving request, and then have all kinds of trouble handling
         it correctly
#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif
        {
          if (dest != MPI_PROC_NULL && result == MPI_SUCCESS)
            vt_request_create(*request, ERF_SEND, tag, dest, count * sz, comm);
        }
       */

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Ibsend(buf, count, datatype, dest, tag, comm, request);
    }

  return result;
}

/* -- MPI_Issend -- */

VT_MPI_INT MPI_Issend(CONST void* buf, VT_MPI_INT count, MPI_Datatype datatype,
                      VT_MPI_INT dest, VT_MPI_INT tag, MPI_Comm comm,
                      MPI_Request* request)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_ISSEND]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (dest != MPI_PROC_NULL && (was_recorded || env_mpi_ignore_filter))
            {
              VT_MPI_INT sz;
              PMPI_Type_size(datatype, &sz);
              vt_mpi_send(tid, &time, VT_RANK_TO_PE(dest, comm),
                          VT_COMM_ID(comm), tag, count * sz);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Issend,
        (buf, count, datatype, dest, tag, comm, request, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Issend(buf, count, datatype, dest, tag, comm, request);

      VT_UNIMCI_CHECK_POST(MPI_Issend,
        (buf, count, datatype, dest, tag, comm, request, "", 0, 0),
        was_recorded, &time);

      /* no need to save send request as we already created send event,
         so why saving request, and then have all kinds of trouble handling
         it correctly
#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif
        {
          if (dest != MPI_PROC_NULL && result == MPI_SUCCESS)
            vt_request_create(*request, ERF_SEND, tag, dest, count * sz, comm);
        }
       */

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Issend(buf, count, datatype, dest, tag, comm, request);
    }

  return result;
}

/* -- MPI_Irsend -- */

VT_MPI_INT MPI_Irsend(CONST void* buf, VT_MPI_INT count, MPI_Datatype datatype,
                      VT_MPI_INT dest, VT_MPI_INT tag, MPI_Comm comm,
                      MPI_Request* request )
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_IRSEND]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (dest != MPI_PROC_NULL && (was_recorded || env_mpi_ignore_filter))
            {
              VT_MPI_INT sz;
              PMPI_Type_size(datatype, &sz);
              vt_mpi_send(tid, &time, VT_RANK_TO_PE(dest, comm),
                          VT_COMM_ID(comm), tag, count * sz);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Irsend,
        (buf, count, datatype, dest, tag, comm, request, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Irsend(buf, count, datatype, dest, tag, comm, request);

      VT_UNIMCI_CHECK_POST(MPI_Irsend,
        (buf, count, datatype, dest, tag, comm, request, "", 0, 0),
        was_recorded, &time);

      /* no need to save send request as we already created send event,
         so why saving request, and then have all kinds of trouble handling
         it correctly
#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif
        {
          if (dest != MPI_PROC_NULL && result == MPI_SUCCESS)
            vt_request_create(*request, ERF_SEND, tag, dest, count * sz, comm);
        }
       */

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Irsend(buf, count, datatype, dest, tag, comm, request);
    }

  return result;
}

/* -- MPI_Wait -- */

VT_MPI_INT MPI_Wait(MPI_Request* request, MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_Status mystatus;
      struct VTRequest* orig_req = NULL;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_WAIT]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (status == MPI_STATUS_IGNORE) status = &mystatus;
          orig_req = vt_request_get(*request);
        }

      VT_UNIMCI_CHECK_PRE(MPI_Wait,
        (request, status, "", 0, 0), was_recorded, &time);

      result = PMPI_Wait(request, status);

      VT_UNIMCI_CHECK_POST(MPI_Wait,
        (request, status, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          vt_check_request(tid, &time, orig_req, status,
                           (was_recorded || env_mpi_ignore_filter));
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Wait(request, status);
    }

  return result;
}

/* -- MPI_Waitall -- */

VT_MPI_INT MPI_Waitall(VT_MPI_INT count, MPI_Request* requests,
                       MPI_Status* array_of_statuses)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_WAITALL]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (array_of_statuses == MPI_STATUSES_IGNORE)
            array_of_statuses = get_status_array(count);
          vt_save_request_array(requests, count);
        }

      VT_UNIMCI_CHECK_PRE(MPI_Waitall,
        (count, requests, array_of_statuses, "", 0, 0), was_recorded, &time);

      result = PMPI_Waitall(count, requests, array_of_statuses);

      VT_UNIMCI_CHECK_POST(MPI_Waitall,
        (count, requests, array_of_statuses, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
      {
        struct VTRequest* orig_req;
        VT_MPI_INT i;
        for (i = 0; i < count; i++)
          {
            orig_req = vt_saved_request_get(i);
            vt_check_request(tid, &time, orig_req, &(array_of_statuses[i]),
                             (was_recorded || env_mpi_ignore_filter));
          }
      }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Waitall(count, requests, array_of_statuses);
    }

  return result;
}

/* -- MPI_Waitany -- */

VT_MPI_INT MPI_Waitany(VT_MPI_INT count, MPI_Request* requests,
                       VT_MPI_INT* index, MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_Status mystatus;
      struct VTRequest* orig_req;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_WAITANY]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (status == MPI_STATUS_IGNORE) status = &mystatus;
          vt_save_request_array(requests, count);
        }

      VT_UNIMCI_CHECK_PRE(MPI_Waitany,
        (count, requests, index, status, "", 0, 0), was_recorded, &time);

      result = PMPI_Waitany(count, requests, index, status);

      VT_UNIMCI_CHECK_POST(MPI_Waitany,
        (count, requests, index, status, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          orig_req = vt_saved_request_get(*index);
          vt_check_request(tid, &time, orig_req, status,
                           (was_recorded || env_mpi_ignore_filter));
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Waitany(count, requests, index, status);
    }

  return result;
}

/* -- MPI_Waitsome -- */

VT_MPI_INT MPI_Waitsome(VT_MPI_INT incount, MPI_Request* array_of_requests,
                        VT_MPI_INT* outcount, VT_MPI_INT* array_of_indices,
                        MPI_Status* array_of_statuses)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_WAITSOME]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (array_of_statuses == MPI_STATUSES_IGNORE)
            array_of_statuses = get_status_array(incount);
          vt_save_request_array(array_of_requests, incount);
        }

      VT_UNIMCI_CHECK_PRE(MPI_Waitsome,
        (incount, array_of_requests, outcount, array_of_indices,
         array_of_statuses, "", 0, 0), was_recorded, &time);

      result = PMPI_Waitsome(incount, array_of_requests, outcount,
                             array_of_indices, array_of_statuses);

      VT_UNIMCI_CHECK_POST(MPI_Waitsome,
        (incount, array_of_requests, outcount, array_of_indices,
         array_of_statuses, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          struct VTRequest* orig_req;
          VT_MPI_INT i;
          for (i = 0; i < *outcount; i++)
            {
              orig_req = vt_saved_request_get(array_of_indices[i]);
              vt_check_request(tid, &time, orig_req, &(array_of_statuses[i]),
                               (was_recorded || env_mpi_ignore_filter));
            }
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Waitsome(incount, array_of_requests, outcount,
                             array_of_indices, array_of_statuses);
    }

  return result;
}

/* -- MPI_Test -- */

VT_MPI_INT MPI_Test(MPI_Request* request, VT_MPI_INT* flag, MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_Status mystatus;
      struct VTRequest* orig_req = NULL;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_TEST]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (status == MPI_STATUS_IGNORE) status = &mystatus;
          orig_req = vt_request_get(*request);
        }

      VT_UNIMCI_CHECK_PRE(MPI_Test,
        (request, flag, status, "", 0, 0), was_recorded, &time);

      result = PMPI_Test(request, flag, status);

      VT_UNIMCI_CHECK_POST(MPI_Test,
        (request, flag, status, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (*flag)
            {
              vt_check_request(tid, &time, orig_req, status,
                               (was_recorded || env_mpi_ignore_filter));
            }
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Test(request, flag, status);
    }

  return result;
}

/* -- MPI_Testany -- */

VT_MPI_INT MPI_Testany(VT_MPI_INT count, MPI_Request* array_of_requests,
                       VT_MPI_INT* index, VT_MPI_INT* flag, MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_Status mystatus;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_TESTANY]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (status == MPI_STATUS_IGNORE) status = &mystatus;
          vt_save_request_array(array_of_requests, count);
        }

      VT_UNIMCI_CHECK_PRE(MPI_Testany,
        (count, array_of_requests, index, flag, status, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Testany(count, array_of_requests, index, flag, status);

      VT_UNIMCI_CHECK_POST(MPI_Testany,
        (count, array_of_requests, index, flag, status, "", 0, 0),
        was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          struct VTRequest* orig_req;
          if (*flag && *index != MPI_UNDEFINED)
            {
              orig_req = vt_saved_request_get(*index);
              vt_check_request(tid, &time, orig_req, status,
                               (was_recorded || env_mpi_ignore_filter));
            }
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Testany(count, array_of_requests, index, flag, status);
    }

  return result;
}

/* -- MPI_Testall -- */

VT_MPI_INT MPI_Testall(VT_MPI_INT count, MPI_Request* array_of_requests,
                       VT_MPI_INT* flag, MPI_Status* array_of_statuses)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_TESTALL]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (array_of_statuses == MPI_STATUSES_IGNORE)
            array_of_statuses = get_status_array(count);
          vt_save_request_array(array_of_requests, count);
        }

      VT_UNIMCI_CHECK_PRE(MPI_Testall,
        (count, array_of_requests, flag, array_of_statuses, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Testall(count, array_of_requests, flag, array_of_statuses);

      VT_UNIMCI_CHECK_POST(MPI_Testall,
        (count, array_of_requests, flag, array_of_statuses, "", 0, 0),
        was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (*flag)
            {
              struct VTRequest* orig_req;
              VT_MPI_INT i;
              for (i = 0; i < count; i++)
                {
                  orig_req = vt_saved_request_get(i);
                  vt_check_request(tid, &time, orig_req,
                                   &(array_of_statuses[i]),
                                   (was_recorded || env_mpi_ignore_filter));
                }
            }
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Testall(count, array_of_requests, flag, array_of_statuses);
    }

  return result;
}

/* -- MPI_Testsome -- */

VT_MPI_INT MPI_Testsome(VT_MPI_INT incount, MPI_Request* array_of_requests,
                        VT_MPI_INT* outcount, VT_MPI_INT* array_of_indices,
                        MPI_Status* array_of_statuses)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_TESTSOME]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (array_of_statuses == MPI_STATUSES_IGNORE)
            array_of_statuses = get_status_array(incount);
          vt_save_request_array(array_of_requests, incount);
        }

      VT_UNIMCI_CHECK_PRE(MPI_Testsome,
        (incount, array_of_requests, outcount, array_of_indices,
         array_of_statuses, "", 0, 0), was_recorded, &time);

      result = PMPI_Testsome(incount, array_of_requests, outcount,
                             array_of_indices, array_of_statuses);

      VT_UNIMCI_CHECK_POST(MPI_Testsome,
        (incount, array_of_requests, outcount, array_of_indices,
         array_of_statuses, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          struct VTRequest* orig_req;
          VT_MPI_INT i;
          for (i = 0; i < *outcount; i++)
            {
              orig_req = vt_saved_request_get(array_of_indices[i]);
              vt_check_request(tid, &time, orig_req, &(array_of_statuses[i]),
                               (was_recorded || env_mpi_ignore_filter));
            }
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Testsome(incount, array_of_requests, outcount,
                             array_of_indices, array_of_statuses);
    }

  return result;
}

/* ------- Persistent requests ------- */

/* -- MPI_Send_init -- */

VT_MPI_INT MPI_Send_init(CONST void* buf, VT_MPI_INT count,
                         MPI_Datatype datatype, VT_MPI_INT dest,
                         VT_MPI_INT tag, MPI_Comm comm, MPI_Request* request)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_SEND_INIT]);

      VT_UNIMCI_CHECK_PRE(MPI_Send_init,
        (buf, count, datatype, dest, tag, comm, request, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Send_init(buf, count, datatype, dest, tag, comm, request);

      VT_UNIMCI_CHECK_POST(MPI_Send_init,
        (buf, count, datatype, dest, tag, comm, request, "", 0, 0),
        was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (dest != MPI_PROC_NULL && result == MPI_SUCCESS)
            {
              VT_MPI_INT sz;
              PMPI_Type_size(datatype, &sz);
              vt_request_create(*request, (ERF_SEND | ERF_IS_PERSISTENT),
                                tag, dest, count*sz, datatype, comm);
            }
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Send_init(buf, count, datatype, dest, tag, comm, request);
    }

  return result;
}

/* -- MPI_Recv_init -- */

VT_MPI_INT MPI_Recv_init(void* buf, VT_MPI_INT count, MPI_Datatype datatype,
                         VT_MPI_INT source, VT_MPI_INT tag, MPI_Comm comm,
                         MPI_Request* request)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_RECV_INIT]);

      VT_UNIMCI_CHECK_PRE(MPI_Recv_init,
        (buf, count, datatype, source, tag, comm, request, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Recv_init(buf, count, datatype, source, tag, comm, request);

      VT_UNIMCI_CHECK_POST(MPI_Recv_init,
        (buf, count, datatype, source, tag, comm, request, "", 0, 0),
        was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (source != MPI_PROC_NULL && result == MPI_SUCCESS)
            {
              VT_MPI_INT sz;
              PMPI_Type_size(datatype, &sz);
              vt_request_create(*request, (ERF_RECV | ERF_IS_PERSISTENT),
                                tag, source, count * sz, datatype, comm);
            }
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Recv_init(buf, count, datatype, source, tag, comm, request);
    }

  return result;
}

/* -- MPI_Bsend_init -- */

VT_MPI_INT MPI_Bsend_init(CONST void* buf, VT_MPI_INT count,
                          MPI_Datatype datatype, VT_MPI_INT dest,
                          VT_MPI_INT tag, MPI_Comm comm, MPI_Request* request)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_BSEND_INIT]);

      VT_UNIMCI_CHECK_PRE(MPI_Bsend_init,
        (buf, count, datatype, dest, tag, comm, request, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Bsend_init(buf, count, datatype, dest, tag, comm, request);

      VT_UNIMCI_CHECK_POST(MPI_Bsend_init,
        (buf, count, datatype, dest, tag, comm, request, "", 0, 0),
        was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (dest != MPI_PROC_NULL && result == MPI_SUCCESS)
            {
              VT_MPI_INT sz;
              PMPI_Type_size(datatype, &sz);
              vt_request_create(*request, (ERF_SEND | ERF_IS_PERSISTENT),
                                tag, dest, count*sz, datatype, comm);
            }
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Bsend_init(buf, count, datatype, dest, tag, comm, request);
    }

  return result;
}

/* -- MPI_Ssend_init -- */

VT_MPI_INT MPI_Ssend_init(CONST void* buf, VT_MPI_INT count,
                          MPI_Datatype datatype, VT_MPI_INT dest,
                          VT_MPI_INT tag, MPI_Comm comm, MPI_Request* request)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_SSEND_INIT]);

      VT_UNIMCI_CHECK_PRE(MPI_Ssend_init,
        (buf, count, datatype, dest, tag, comm, request, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Ssend_init(buf, count, datatype, dest, tag, comm, request);

      VT_UNIMCI_CHECK_POST(MPI_Ssend_init,
        (buf, count, datatype, dest, tag, comm, request, "", 0, 0),
        was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (dest != MPI_PROC_NULL && result == MPI_SUCCESS)
            {
              VT_MPI_INT sz;
              PMPI_Type_size(datatype, &sz);
              vt_request_create(*request, (ERF_SEND | ERF_IS_PERSISTENT),
                                tag, dest, count*sz, datatype, comm);
            }
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Ssend_init(buf, count, datatype, dest, tag, comm, request);
    }

  return result;
}

/* -- MPI_Rsend_init -- */

VT_MPI_INT MPI_Rsend_init(CONST void* buf, VT_MPI_INT count,
                          MPI_Datatype datatype, VT_MPI_INT dest,
                          VT_MPI_INT tag, MPI_Comm comm, MPI_Request* request)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_RSEND_INIT]);

      VT_UNIMCI_CHECK_PRE(MPI_Rsend_init,
        (buf, count, datatype, dest, tag, comm, request, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Rsend_init(buf, count, datatype, dest, tag, comm, request);

      VT_UNIMCI_CHECK_POST(MPI_Rsend_init,
        (buf, count, datatype, dest, tag, comm, request, "", 0, 0),
        was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (dest != MPI_PROC_NULL && result == MPI_SUCCESS)
            {
              VT_MPI_INT sz;
              PMPI_Type_size(datatype, &sz);
              vt_request_create(*request, (ERF_SEND | ERF_IS_PERSISTENT),
                                tag, dest, count*sz, datatype, comm);
            }
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Rsend_init(buf, count, datatype, dest, tag, comm, request);
    }

  return result;
}

/* -- MPI_Start -- */

VT_MPI_INT MPI_Start(MPI_Request* request)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_START]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          struct VTRequest* req;

          req = vt_request_get(*request);
          if (req)
            {
              if (req->flags & ERF_IS_PERSISTENT )
                {
                  req->flags |= ERF_IS_ACTIVE;
                  if ((req->flags & ERF_SEND) && (req->dest != MPI_PROC_NULL) &&
                      (was_recorded || env_mpi_ignore_filter))
                    {
                      vt_mpi_send(tid, &time,
                                  VT_RANK_TO_PE_BY_GROUP(req->dest, req->group),
                                  req->cid, req->tag,  req->bytes);
                    }
                }
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Start,
        (request, "", 0, 0), was_recorded, &time);

      result = PMPI_Start(request);

      VT_UNIMCI_CHECK_POST(MPI_Start,
        (request, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Start(request);
    }

  return result;
}

/* -- MPI_Startall -- */

VT_MPI_INT MPI_Startall(VT_MPI_INT count, MPI_Request* array_of_requests)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_STARTALL]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          MPI_Request* request;
          struct VTRequest* req;
          VT_MPI_INT i;

          for (i = 0; i < count; i++)
            {
              request = &array_of_requests[i];
              req = vt_request_get(*request);
              if (req)
                {
                  if (req->flags & ERF_IS_PERSISTENT )
                    {
                      req->flags |= ERF_IS_ACTIVE;
                      if ((req->flags & ERF_SEND) &&
                          (req->dest != MPI_PROC_NULL) &&
                          (was_recorded || env_mpi_ignore_filter))
                        {
                          vt_mpi_send(tid, &time,
                            VT_RANK_TO_PE_BY_GROUP(req->dest, req->group),
                            req->cid, req->tag, req->bytes);
                        }
                    }
                }
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Startall,
        (count, array_of_requests, "", 0, 0), was_recorded, &time);

      result = PMPI_Startall(count, array_of_requests);

      VT_UNIMCI_CHECK_POST(MPI_Startall,
        (count, array_of_requests, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Startall(count, array_of_requests);
    }

  return result;
}

/* -- MPI_Request_free -- */

VT_MPI_INT MPI_Request_free(MPI_Request* request)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_REQUEST_FREE]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          struct VTRequest* req;

          req = vt_request_get(*request);
          if (req && (req->flags & ERF_IS_PERSISTENT))
            {
              if (req->flags & ERF_IS_ACTIVE )
                /* mark active requests for deallocation */
                req->flags |= ERF_DEALLOCATE;
              else
                /* deallocate inactive requests -*/
                vt_request_free(req);
            }
          /* -- else non-persistent requests:
                + we don't track non-persistent sends
                + MPI standard strongly suggests to deallocate non-persistent
                  recv's only by waot or test
                ==> nothing to do here
           */
        }

      VT_UNIMCI_CHECK_PRE(MPI_Request_free,
        (request, "", 0, 0), was_recorded, &time);

      result = PMPI_Request_free(request);

      VT_UNIMCI_CHECK_POST(MPI_Request_free,
        (request, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Request_free(request);
    }

  return result;
}

/* -- MPI_Cancel -- */

VT_MPI_INT MPI_Cancel(MPI_Request* request)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_CANCEL]);

      /* -- do not really know what to do here ?!?
            would need to find out if canceled communcation completed
            sucessfully or was canceled sucessfully (probably possible
            by using PMPI_Test_cancelled) but whatever we do here,
            we end up by an invalid trace as there we cannot remove the
            send events already put in the trace buffer, and so the
            message matching in the analysis will fail in any case
       */

      VT_UNIMCI_CHECK_PRE(MPI_Cancel,
        (request, "", 0, 0), was_recorded, &time);

      result = PMPI_Cancel(request);

      VT_UNIMCI_CHECK_POST(MPI_Cancel,
        (request, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Cancel(request);
    }

  return result;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Collective communication
 *
 *-----------------------------------------------------------------------------
 */

/* -- MPI_Allreduce -- */

VT_MPI_INT MPI_Allreduce(CONST void* sendbuf, void* recvbuf, VT_MPI_INT count,
                         MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      uint64_t matchid = 0;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_ALLREDUCE]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (was_recorded || env_mpi_ignore_filter)
            {
              VT_MPI_INT sz;
              matchid = VTTHRD_MPICOLLOP_NEXT_MATCHINGID(VTThrdv[tid]);
              PMPI_Type_size(datatype, &sz);
              vt_mpi_collbegin(tid, &time, vt_mpi_regid[VT__MPI_ALLREDUCE],
                               matchid, VT_NO_ID, VT_COMM_ID(comm),
                               count * sz, count * sz);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Allreduce,
        (sendbuf, recvbuf, count, datatype, op, comm, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Allreduce(sendbuf, recvbuf, count, datatype, op, comm);

      VT_UNIMCI_CHECK_POST(MPI_Allreduce,
        (sendbuf, recvbuf, count, datatype, op, comm, "", 0, 0),
        was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          vt_mpi_collend(tid, &time, matchid, &comm,
                         (was_recorded || env_mpi_ignore_filter));
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Allreduce(sendbuf, recvbuf, count, datatype, op, comm);
    }

  return result;
}

/* -- MPI_Barrier -- */

VT_MPI_INT MPI_Barrier(MPI_Comm comm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      uint64_t matchid = 0;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_BARRIER]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (was_recorded || env_mpi_ignore_filter)
            {
              matchid = VTTHRD_MPICOLLOP_NEXT_MATCHINGID(VTThrdv[tid]);
              vt_mpi_collbegin(tid, &time, vt_mpi_regid[VT__MPI_BARRIER],
                               matchid, VT_NO_ID, VT_COMM_ID(comm), 0, 0);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Barrier,
        (comm, "", 0, 0), was_recorded, &time);

      result = PMPI_Barrier(comm);

      VT_UNIMCI_CHECK_POST(MPI_Barrier,
        (comm, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          vt_mpi_collend(tid, &time, matchid, &comm,
                         (was_recorded || env_mpi_ignore_filter));
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Barrier(comm);
    }

  return result;
}

/* -- MPI_Bcast -- */

VT_MPI_INT MPI_Bcast(void* buf, VT_MPI_INT count, MPI_Datatype datatype,
                     VT_MPI_INT root, MPI_Comm comm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      uint64_t matchid = 0;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_BCAST]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (root != MPI_PROC_NULL && (was_recorded || env_mpi_ignore_filter))
            {
              VT_MPI_INT me, sendcount, sz;
              uint8_t iam_root;

#if defined(HAVE_DECL_MPI_ROOT) && HAVE_DECL_MPI_ROOT
              VT_MPI_INT inter;
              PMPI_Comm_test_inter(comm, &inter);
              if (inter)
                {
                  iam_root = (root == MPI_ROOT);
                }
              else
#endif /* HAVE_DECL_MPI_ROOT */
                {
                  PMPI_Comm_rank(comm, &me);
                  iam_root = (root == me);
                }

              PMPI_Type_size(datatype, &sz);
              if (iam_root)
                sendcount = count;
              else
                sendcount = 0;

              matchid = VTTHRD_MPICOLLOP_NEXT_MATCHINGID(VTThrdv[tid]);

              vt_mpi_collbegin(tid, &time, vt_mpi_regid[VT__MPI_BCAST], matchid,
                               VT_RANK_TO_PE(root, comm), VT_COMM_ID(comm),
                               sendcount * sz, count * sz);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Bcast,
        (buf, count, datatype, root, comm, "", 0, 0), was_recorded, &time);

      result = PMPI_Bcast(buf, count, datatype, root, comm);

      VT_UNIMCI_CHECK_POST(MPI_Bcast,
        (buf, count, datatype, root, comm, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          vt_mpi_collend(tid, &time, matchid, &comm,
                         (root != MPI_PROC_NULL &&
                          (was_recorded || env_mpi_ignore_filter)));
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Bcast(buf, count, datatype, root, comm);
    }

  return result;
}

/* -- MPI_Gather -- */

VT_MPI_INT MPI_Gather(CONST void* sendbuf, VT_MPI_INT sendcount,
                      MPI_Datatype sendtype, void* recvbuf,
                      VT_MPI_INT recvcount, MPI_Datatype recvtype,
                      VT_MPI_INT root, MPI_Comm comm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      uint64_t matchid = 0;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_GATHER]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (root != MPI_PROC_NULL && (was_recorded || env_mpi_ignore_filter))
            {
              VT_MPI_INT me, N, ssz, rsz;
              uint8_t iam_root;

#if defined(HAVE_DECL_MPI_ROOT) && HAVE_DECL_MPI_ROOT
              VT_MPI_INT inter;
              PMPI_Comm_test_inter(comm, &inter);
              if (inter)
                {
                  iam_root = (root == MPI_ROOT);
                }
              else
#endif /* HAVE_DECL_MPI_ROOT */
                {
                  PMPI_Comm_rank(comm, &me);
                  iam_root = (root == me);
                }

#if defined(HAVE_DECL_MPI_IN_PLACE) && HAVE_DECL_MPI_IN_PLACE
              if (sendbuf == MPI_IN_PLACE)
                {
                  sendtype = recvtype;
                  sendcount = recvcount;
                }
#endif /* HAVE_DECL_MPI_IN_PLACE */

              PMPI_Type_size(sendtype, &ssz);

              if (iam_root)
                {
                  PMPI_Comm_size(comm, &N);
                  PMPI_Type_size(recvtype, &rsz);
                }
              else
                {
                  N = rsz = 0;
                }

              matchid = VTTHRD_MPICOLLOP_NEXT_MATCHINGID(VTThrdv[tid]);

              vt_mpi_collbegin(tid, &time, vt_mpi_regid[VT__MPI_GATHER],
                               matchid, VT_RANK_TO_PE(root, comm),
                               VT_COMM_ID(comm), sendcount * ssz,
                               N * recvcount * rsz);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Gather,
        (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root,
         comm, "", 0, 0), was_recorded, &time);

      result = PMPI_Gather(sendbuf, sendcount, sendtype, recvbuf, recvcount,
                           recvtype, root, comm);

      VT_UNIMCI_CHECK_POST(MPI_Gather,
        (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root,
         comm, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          vt_mpi_collend(tid, &time, matchid, &comm,
                         (root != MPI_PROC_NULL &&
                          (was_recorded || env_mpi_ignore_filter)));
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Gather(sendbuf, sendcount, sendtype, recvbuf, recvcount,
                           recvtype, root, comm);
    }

  return result;
}

/* -- MPI_Reduce -- */

VT_MPI_INT MPI_Reduce(CONST void* sendbuf, void* recvbuf, VT_MPI_INT count,
                      MPI_Datatype datatype, MPI_Op op, VT_MPI_INT root,
                      MPI_Comm comm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      uint64_t matchid = 0;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_REDUCE]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (root != MPI_PROC_NULL && (was_recorded || env_mpi_ignore_filter))
            {
              VT_MPI_INT me, recvcount, sz;
              uint8_t iam_root;

#if defined(HAVE_DECL_MPI_ROOT) && HAVE_DECL_MPI_ROOT
              VT_MPI_INT inter;
              PMPI_Comm_test_inter(comm, &inter);
              if (inter)
                {
                  iam_root = (root == MPI_ROOT);
                }
              else
#endif /* HAVE_DECL_MPI_ROOT */
                {
                  PMPI_Comm_rank(comm, &me);
                  iam_root = (root == me);
                }

              if (iam_root)
                recvcount = count;
              else
                recvcount = 0;

              PMPI_Type_size(datatype, &sz);

              matchid = VTTHRD_MPICOLLOP_NEXT_MATCHINGID(VTThrdv[tid]);

              vt_mpi_collbegin(tid, &time, vt_mpi_regid[VT__MPI_REDUCE],
                               matchid, VT_RANK_TO_PE(root, comm),
                               VT_COMM_ID(comm), count * sz, recvcount * sz);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Reduce,
        (sendbuf, recvbuf, count, datatype, op, root, comm, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Reduce(sendbuf, recvbuf, count, datatype, op, root, comm);

      VT_UNIMCI_CHECK_POST(MPI_Reduce,
        (sendbuf, recvbuf, count, datatype, op, root, comm, "", 0, 0),
        was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          vt_mpi_collend(tid, &time, matchid, &comm,
                         (root != MPI_PROC_NULL && was_recorded));
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Reduce(sendbuf, recvbuf, count, datatype, op, root, comm);
    }

  return result;
}

/* -- MPI_Gatherv -- */

VT_MPI_INT MPI_Gatherv(CONST void* sendbuf, VT_MPI_INT sendcount,
                       MPI_Datatype sendtype, void* recvbuf,
                       CONST VT_MPI_INT *recvcounts, CONST VT_MPI_INT *displs,
                       MPI_Datatype recvtype, VT_MPI_INT root, MPI_Comm comm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      uint64_t matchid = 0;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_GATHERV]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (root != MPI_PROC_NULL && (was_recorded || env_mpi_ignore_filter))
            {
              VT_MPI_INT me, N, recvcount, sendsz, recvsz, i;
              uint8_t iam_root;

#if defined(HAVE_DECL_MPI_ROOT) && HAVE_DECL_MPI_ROOT
              VT_MPI_INT inter;
              PMPI_Comm_test_inter(comm, &inter);
              if (inter)
                {
                  iam_root = (root == MPI_ROOT);
                }
              else
#endif /* HAVE_DECL_MPI_ROOT */
                {
                  PMPI_Comm_rank(comm, &me);
                  iam_root = (root == me);
                }

              recvcount = recvsz = 0;
              if (iam_root)
                {
                  PMPI_Comm_size(comm, &N);
                  PMPI_Type_size(recvtype, &recvsz);
                  for (i = 0; i < N; i++) recvcount += recvcounts[i];
                }

#if defined(HAVE_DECL_MPI_IN_PLACE) && HAVE_DECL_MPI_IN_PLACE
              if (sendbuf == MPI_IN_PLACE)
                {
                  sendtype = recvtype;
                  sendcount = recvcount;
                }
#endif /* HAVE_DECL_MPI_IN_PLACE */

              PMPI_Type_size(sendtype, &sendsz);

              matchid = VTTHRD_MPICOLLOP_NEXT_MATCHINGID(VTThrdv[tid]);

              vt_mpi_collbegin(tid, &time, vt_mpi_regid[VT__MPI_GATHERV],
                               matchid, VT_RANK_TO_PE(root, comm),
                               VT_COMM_ID(comm), sendcount * sendsz,
                               recvcount * recvsz);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Gatherv,
        (sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype,
         root, comm, "", 0, 0), was_recorded, &time);

      result = PMPI_Gatherv(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype,
                            root, comm);

      VT_UNIMCI_CHECK_POST(MPI_Gatherv,
        (sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype,
         root, comm, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          vt_mpi_collend(tid, &time, matchid, &comm,
                         (root != MPI_PROC_NULL &&
                          (was_recorded || env_mpi_ignore_filter)));
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Gatherv(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype,
                            root, comm);
    }

  return result;
}

/* -- MPI_Allgather -- */

VT_MPI_INT MPI_Allgather(CONST void* sendbuf, VT_MPI_INT sendcount,
                         MPI_Datatype sendtype, void* recvbuf,
                         VT_MPI_INT recvcount, MPI_Datatype recvtype,
                         MPI_Comm comm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      uint64_t matchid = 0;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_ALLGATHER]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (was_recorded || env_mpi_ignore_filter)
            {
              VT_MPI_INT N, sendsz, recvsz;
              matchid = VTTHRD_MPICOLLOP_NEXT_MATCHINGID(VTThrdv[tid]);

#if defined(HAVE_DECL_MPI_IN_PLACE) && HAVE_DECL_MPI_IN_PLACE
              if (sendbuf == MPI_IN_PLACE)
                {
                  sendtype = recvtype;
                  sendcount = recvcount;
                }
#endif /* HAVE_DECL_MPI_IN_PLACE */

              PMPI_Type_size(recvtype, &recvsz);
              PMPI_Type_size(sendtype, &sendsz);
              PMPI_Comm_size(comm, &N);

              vt_mpi_collbegin(tid, &time, vt_mpi_regid[VT__MPI_ALLGATHER],
                               matchid, VT_NO_ID, VT_COMM_ID(comm),
                               sendcount * sendsz, N * recvcount * recvsz);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Allgather,
        (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype,
         comm, "", 0, 0), was_recorded, &time);

      result = PMPI_Allgather(sendbuf, sendcount, sendtype, recvbuf, recvcount,
                              recvtype, comm);

      VT_UNIMCI_CHECK_POST(MPI_Allgather,
        (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype,
         comm, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          vt_mpi_collend(tid, &time, matchid, &comm,
                         (was_recorded || env_mpi_ignore_filter));
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Allgather(sendbuf, sendcount, sendtype, recvbuf, recvcount,
                              recvtype, comm);
    }

  return result;
}

/* -- MPI_Allgatherv -- */

VT_MPI_INT MPI_Allgatherv(CONST void* sendbuf, VT_MPI_INT sendcount,
                          MPI_Datatype sendtype, void* recvbuf,
                          CONST VT_MPI_INT* recvcounts,
                          CONST VT_MPI_INT* displs, MPI_Datatype recvtype,
                          MPI_Comm comm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      uint64_t matchid = 0;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_ALLGATHERV]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (was_recorded || env_mpi_ignore_filter)
            {
              VT_MPI_INT N, recvcount, sendsz, recvsz, i;

              matchid = VTTHRD_MPICOLLOP_NEXT_MATCHINGID(VTThrdv[tid]);

              PMPI_Comm_size(comm, &N);

              recvcount = 0;
              for (i = 0; i < N; i++) recvcount += recvcounts[i];

#if defined(HAVE_DECL_MPI_IN_PLACE) && HAVE_DECL_MPI_IN_PLACE
              if (sendbuf == MPI_IN_PLACE)
                {
                  sendtype = recvtype;
                  sendcount = recvcount;
                }
#endif /* HAVE_DECL_MPI_IN_PLACE */

              PMPI_Type_size(recvtype, &recvsz);
              PMPI_Type_size(sendtype, &sendsz);

              vt_mpi_collbegin(tid, &time, vt_mpi_regid[VT__MPI_ALLGATHERV],
                               matchid, VT_NO_ID, VT_COMM_ID(comm),
                               sendcount * sendsz, recvcount * recvsz);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Allgatherv,
        (sendbuf, sendcount, sendtype, recvbuf,
         recvcounts, displs, recvtype, comm, "", 0, 0), was_recorded, &time);

      result = PMPI_Allgatherv(sendbuf, sendcount, sendtype, recvbuf,
                               recvcounts, displs, recvtype, comm);

      VT_UNIMCI_CHECK_POST(MPI_Allgatherv,
        (sendbuf, sendcount, sendtype, recvbuf,
         recvcounts, displs, recvtype, comm, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          vt_mpi_collend(tid, &time, matchid, &comm,
                         (was_recorded || env_mpi_ignore_filter));
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Allgatherv(sendbuf, sendcount, sendtype, recvbuf,
                               recvcounts, displs, recvtype, comm);
    }

  return result;
}

/* -- MPI_Alltoall -- */

VT_MPI_INT MPI_Alltoall(CONST void* sendbuf, VT_MPI_INT sendcount,
                        MPI_Datatype sendtype, void* recvbuf,
                        VT_MPI_INT recvcount, MPI_Datatype recvtype,
                        MPI_Comm comm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      uint64_t matchid = 0;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_ALLTOALL]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (was_recorded || env_mpi_ignore_filter)
            {
              VT_MPI_INT N, sendsz, recvsz;

              matchid = VTTHRD_MPICOLLOP_NEXT_MATCHINGID(VTThrdv[tid]);

              PMPI_Type_size(recvtype, &recvsz);
              PMPI_Type_size(sendtype, &sendsz);
              PMPI_Comm_size(comm, &N);

              vt_mpi_collbegin(tid, &time, vt_mpi_regid[VT__MPI_ALLTOALL],
                               matchid, VT_NO_ID, VT_COMM_ID(comm),
                               sendsz * sendcount * N, recvsz * recvcount * N);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Alltoall,
        (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype,
         comm, "", 0, 0), was_recorded, &time);

      result = PMPI_Alltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount,
                             recvtype, comm);

      VT_UNIMCI_CHECK_POST(MPI_Alltoall,
        (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype,
         comm, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          vt_mpi_collend(tid, &time, matchid, &comm,
                         (was_recorded || env_mpi_ignore_filter));
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Alltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount,
                             recvtype, comm);
    }

  return result;
}

/* -- MPI_Alltoallv -- */

VT_MPI_INT MPI_Alltoallv(CONST void* sendbuf, CONST VT_MPI_INT* sendcounts,
                         CONST VT_MPI_INT* sdispls, MPI_Datatype sendtype,
                         void* recvbuf, CONST VT_MPI_INT* recvcounts,
                         CONST VT_MPI_INT* rdispls, MPI_Datatype recvtype,
                         MPI_Comm comm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      uint64_t matchid = 0;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_ALLTOALLV]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (was_recorded || env_mpi_ignore_filter)
            {
              VT_MPI_INT N, sendcount = 0, recvcount = 0, sendsz, recvsz, i;
              matchid = VTTHRD_MPICOLLOP_NEXT_MATCHINGID(VTThrdv[tid]);

              PMPI_Type_size(recvtype, &recvsz);
              PMPI_Type_size(sendtype, &sendsz);
              PMPI_Comm_size(comm, &N);
              for (i = 0; i < N; i++)
                {
                  recvcount += recvcounts[i];
                  sendcount += sendcounts[i];
                }

              vt_mpi_collbegin(tid, &time, vt_mpi_regid[VT__MPI_ALLTOALLV],
                               matchid, VT_NO_ID, VT_COMM_ID(comm),
                               sendsz * sendcount, recvsz * recvcount);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Alltoallv,
        (sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls,
         recvtype, comm, "", 0, 0), was_recorded, &time);

      result = PMPI_Alltoallv(sendbuf, sendcounts, sdispls, sendtype, recvbuf,
                              recvcounts, rdispls, recvtype, comm);

      VT_UNIMCI_CHECK_POST(MPI_Alltoallv,
        (sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls,
         recvtype, comm, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          vt_mpi_collend(tid, &time, matchid, &comm,
                         (was_recorded || env_mpi_ignore_filter));
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Alltoallv(sendbuf, sendcounts, sdispls, sendtype, recvbuf,
                              recvcounts, rdispls, recvtype, comm);
    }

  return result;
}

/* -- MPI_Scan -- */

VT_MPI_INT MPI_Scan(CONST void* sendbuf, void* recvbuf, VT_MPI_INT count,
                    MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      uint64_t matchid = 0;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_SCAN]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (was_recorded || env_mpi_ignore_filter)
            {
              VT_MPI_INT me, sz;
              matchid = VTTHRD_MPICOLLOP_NEXT_MATCHINGID(VTThrdv[tid]);

              PMPI_Type_size(datatype, &sz);
              PMPI_Comm_rank(comm, &me);

              vt_mpi_collbegin(tid, &time, vt_mpi_regid[VT__MPI_SCAN], matchid,
                               VT_NO_ID, VT_COMM_ID(comm), count * sz,
                               count * sz);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Scan,
        (sendbuf, recvbuf, count, datatype, op, comm, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Scan(sendbuf, recvbuf, count, datatype, op, comm);

      VT_UNIMCI_CHECK_POST(MPI_Scan,
        (sendbuf, recvbuf, count, datatype, op, comm, "", 0, 0),
        was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          vt_mpi_collend(tid, &time, matchid, &comm,
                         (was_recorded || env_mpi_ignore_filter));
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Scan(sendbuf, recvbuf, count, datatype, op, comm);
    }

  return result;
}

/* -- MPI_Scatter -- */

VT_MPI_INT MPI_Scatter(CONST void* sendbuf, VT_MPI_INT sendcount,
                       MPI_Datatype sendtype, void* recvbuf,
                       VT_MPI_INT recvcount, MPI_Datatype recvtype,
                       VT_MPI_INT root, MPI_Comm comm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      uint64_t matchid = 0;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_SCATTER]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (root != MPI_PROC_NULL && (was_recorded || env_mpi_ignore_filter))
            {
              VT_MPI_INT me, N, sendsz, recvsz;
              uint8_t iam_root;

#if defined(HAVE_DECL_MPI_ROOT) && HAVE_DECL_MPI_ROOT
              VT_MPI_INT inter;
              PMPI_Comm_test_inter(comm, &inter);
              if (inter)
                {
                  iam_root = (root == MPI_ROOT);
                }
              else
#endif /* HAVE_DECL_MPI_ROOT */
                {
                  PMPI_Comm_rank(comm, &me);
                  iam_root = (root == me);
                }

#if defined(HAVE_DECL_MPI_IN_PLACE) && HAVE_DECL_MPI_IN_PLACE
              if (sendbuf == MPI_IN_PLACE)
                {
                  sendtype = recvtype;
                  sendcount = recvcount;
                }
#endif /* HAVE_DECL_MPI_IN_PLACE */

              PMPI_Type_size(recvtype, &recvsz);
              if (iam_root)
                {
                  PMPI_Comm_size(comm, &N);
                  PMPI_Type_size(sendtype, &sendsz);
                }
              else
                {
                  N = sendsz = 0;
                }

              matchid = VTTHRD_MPICOLLOP_NEXT_MATCHINGID(VTThrdv[tid]);

              vt_mpi_collbegin(tid, &time, vt_mpi_regid[VT__MPI_SCATTER],
                               matchid, VT_RANK_TO_PE(root, comm),
                               VT_COMM_ID(comm), N * sendcount * sendsz,
                               recvcount * recvsz);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Scatter,
        (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root,
         comm, "", 0, 0), was_recorded, &time);

      result = PMPI_Scatter(sendbuf, sendcount, sendtype, recvbuf, recvcount,
                            recvtype, root, comm);

      VT_UNIMCI_CHECK_POST(MPI_Scatter,
        (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root,
         comm, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          vt_mpi_collend(tid, &time, matchid, &comm,
                         (root != MPI_PROC_NULL &&
                          (was_recorded || env_mpi_ignore_filter)));
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Scatter(sendbuf, sendcount, sendtype, recvbuf, recvcount,
                            recvtype, root, comm);
    }

  return result;
}


/* -- MPI_Scatterv -- */

VT_MPI_INT MPI_Scatterv(CONST void* sendbuf, CONST VT_MPI_INT* sendcounts,
                        CONST VT_MPI_INT* displs, MPI_Datatype sendtype,
                        void* recvbuf, VT_MPI_INT recvcount,
                        MPI_Datatype recvtype, VT_MPI_INT root, MPI_Comm comm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      uint64_t matchid = 0;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_SCATTERV]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (root != MPI_PROC_NULL && (was_recorded || env_mpi_ignore_filter))
            {
              VT_MPI_INT me, N, sendcount, sendsz, recvsz, i;
              uint8_t iam_root;

#if defined(HAVE_DECL_MPI_ROOT) && HAVE_DECL_MPI_ROOT
              VT_MPI_INT inter;
              PMPI_Comm_test_inter(comm, &inter);
              if (inter)
                {
                  iam_root = (root == MPI_ROOT);
                }
              else
#endif /* HAVE_DECL_MPI_ROOT */
                {
                  PMPI_Comm_rank(comm, &me);
                  iam_root = (root == me);
                }

              sendcount = sendsz = 0;
              if (iam_root)
                {
                  PMPI_Comm_size(comm, &N);
                  PMPI_Type_size(sendtype, &sendsz);
                  for (i = 0; i < N; i++) sendcount += sendcounts[i];
                }

#if defined(HAVE_DECL_MPI_IN_PLACE) && HAVE_DECL_MPI_IN_PLACE
              if (recvbuf == MPI_IN_PLACE)
                {
                  recvtype = sendtype;
                  recvcount = sendcount;
                }
#endif /* HAVE_DECL_MPI_IN_PLACE */

              PMPI_Type_size(recvtype, &recvsz);

              matchid = VTTHRD_MPICOLLOP_NEXT_MATCHINGID(VTThrdv[tid]);

              vt_mpi_collbegin(tid, &time, vt_mpi_regid[VT__MPI_SCATTERV],
                               matchid, VT_RANK_TO_PE(root, comm),
                               VT_COMM_ID(comm), sendcount * sendsz,
                               recvcount * recvsz);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Scatterv,
        (sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype,
         root, comm, "", 0, 0), was_recorded, &time);

      result = PMPI_Scatterv(sendbuf, sendcounts, displs, sendtype, recvbuf,
                             recvcount, recvtype, root, comm);

      VT_UNIMCI_CHECK_POST(MPI_Scatterv,
        (sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype,
         root, comm, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          vt_mpi_collend(tid, &time, matchid, &comm,
                         (root != MPI_PROC_NULL &&
                          (was_recorded || env_mpi_ignore_filter)));
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Scatterv(sendbuf, sendcounts, displs, sendtype, recvbuf,
                             recvcount, recvtype, root, comm);
    }

  return result;
}

/* -- MPI_Reduce_scatter -- */

VT_MPI_INT MPI_Reduce_scatter(CONST void* sendbuf, void* recvbuf,
                              CONST VT_MPI_INT* recvcounts,
                              MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      uint64_t matchid = 0;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_REDUCE_SCATTER]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (was_recorded || env_mpi_ignore_filter)
            {
              VT_MPI_INT me, N, recvcount, sz, i;
              matchid = VTTHRD_MPICOLLOP_NEXT_MATCHINGID(VTThrdv[tid]);

              PMPI_Type_size(datatype, &sz);
              PMPI_Comm_size(comm, &N);
              PMPI_Comm_rank(comm, &me);
              recvcount = 0;
              for (i = 0; i < N; i++) recvcount += recvcounts[i];

              vt_mpi_collbegin(tid, &time, vt_mpi_regid[VT__MPI_REDUCE_SCATTER],
                               matchid, VT_NO_ID, VT_COMM_ID(comm),
                               recvcount * sz, recvcounts[me] * sz);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Reduce_scatter,
        (sendbuf, recvbuf, recvcounts, datatype, op, comm, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Reduce_scatter(sendbuf, recvbuf, recvcounts, datatype, op,
                                   comm);

      VT_UNIMCI_CHECK_POST(MPI_Reduce_scatter,
        (sendbuf, recvbuf, recvcounts, datatype, op, comm, "", 0, 0),
        was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          vt_mpi_collend(tid, &time, matchid, &comm,
                         (was_recorded || env_mpi_ignore_filter));
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Reduce_scatter(sendbuf, recvbuf, recvcounts, datatype, op,
                                   comm);
    }

  return result;
}

/*
 *-----------------------------------------------------------------------------
 *
 * MPI-2 One-sided communications
 *
 *-----------------------------------------------------------------------------
 */

#if defined(HAVE_MPI2_1SIDED) && HAVE_MPI2_1SIDED

/* -- MPI_Put -- */

VT_MPI_INT MPI_Put(CONST void* origin_addr, VT_MPI_INT origin_count,
                   MPI_Datatype origin_datatype, VT_MPI_INT target_rank,
                   MPI_Aint target_disp, VT_MPI_INT target_count,
                   MPI_Datatype target_datatype, MPI_Win win)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);
      
      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_PUT]);

      VT_UNIMCI_CHECK_PRE(MPI_Put,
        (origin_addr, origin_count, origin_datatype, target_rank, target_disp,
         target_count, target_datatype, win, "", 0, 0), was_recorded, &time);

      result = PMPI_Put(origin_addr, origin_count, origin_datatype, target_rank,
                        target_disp, target_count, target_datatype, win);

      VT_UNIMCI_CHECK_POST(MPI_Put,
        (origin_addr, origin_count, origin_datatype, target_rank, target_disp,
         target_count, target_datatype, win, "", 0, 0), was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (target_rank != MPI_PROC_NULL &&
              (was_recorded || env_mpi_ignore_filter))
            {
              MPI_Comm comm;
              VT_MPI_INT sz;
              uint32_t gid, wid;

              PMPI_Type_size(origin_datatype, &sz);
              vt_win_id(win, &comm, &gid, &wid);
              if (is_rma_putre)
                vt_mpi_rma_putre(tid, &time, VT_RANK_TO_PE(target_rank, comm),
                                 gid, wid, sz * origin_count );
              else
                vt_mpi_rma_put(tid, &time, VT_RANK_TO_PE(target_rank, comm),
                               gid, wid, sz * origin_count );
            }
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Put(origin_addr, origin_count, origin_datatype, target_rank,
                        target_disp, target_count, target_datatype, win);
    }

  return result;
}

/* -- MPI_Get -- */

VT_MPI_INT MPI_Get(void* origin_addr, VT_MPI_INT origin_count,
                   MPI_Datatype origin_datatype, VT_MPI_INT target_rank,
                   MPI_Aint target_disp, VT_MPI_INT target_count,
                   MPI_Datatype target_datatype, MPI_Win win)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_GET]);

      VT_UNIMCI_CHECK_PRE(MPI_Get,
        (origin_addr, origin_count, origin_datatype, target_rank, target_disp,
         target_count, target_datatype, win, "", 0, 0), was_recorded, &time);

      result = PMPI_Get(origin_addr, origin_count, origin_datatype, target_rank, target_disp,
                        target_count, target_datatype, win);

      VT_UNIMCI_CHECK_POST(MPI_Get,
        (origin_addr, origin_count, origin_datatype, target_rank, target_disp,
         target_count, target_datatype, win, "", 0, 0), was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (target_rank != MPI_PROC_NULL &&
              (was_recorded || env_mpi_ignore_filter))
            {
              MPI_Comm comm;
              VT_MPI_INT sz;
              uint32_t gid, wid;

              PMPI_Type_size(target_datatype, &sz);
              vt_win_id(win, &comm, &gid, &wid);
              vt_mpi_rma_get(tid, &time, VT_RANK_TO_PE(target_rank, comm), gid,
                             wid, target_count * sz);
            }
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Get(origin_addr, origin_count, origin_datatype, target_rank, target_disp,
                        target_count, target_datatype, win);
    }

  return result;
}

/* -- MPI_Accumulate -- */

VT_MPI_INT MPI_Accumulate(CONST void* origin_addr, VT_MPI_INT origin_count,
                          MPI_Datatype origin_datatype, VT_MPI_INT target_rank,
                          MPI_Aint target_disp, VT_MPI_INT target_count,
                          MPI_Datatype target_datatype, MPI_Op op,
                          MPI_Win win)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);
 
      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_ACCUMULATE]);

      VT_UNIMCI_CHECK_PRE(MPI_Accumulate,
        (origin_addr, origin_count, origin_datatype, target_rank, target_disp,
         target_count, target_datatype, op, win, "", 0, 0),
         was_recorded, &time);

      result = PMPI_Accumulate(origin_addr, origin_count, origin_datatype,
                               target_rank, target_disp, target_count,
                               target_datatype, op, win);

      VT_UNIMCI_CHECK_POST(MPI_Accumulate,
        (origin_addr, origin_count, origin_datatype, target_rank, target_disp,
         target_count, target_datatype, op, win, "", 0, 0),
         was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (target_rank != MPI_PROC_NULL &&
              (was_recorded || env_mpi_ignore_filter))
            {
              MPI_Comm comm;
              VT_MPI_INT sz;
              uint32_t gid, wid;

              PMPI_Type_size(origin_datatype, &sz);
              vt_win_id(win, &comm, &gid, &wid);
              if (is_rma_putre)
                vt_mpi_rma_putre(tid, &time, VT_RANK_TO_PE(target_rank, comm),
                                 gid, wid, sz * origin_count );
              else
                vt_mpi_rma_put(tid, &time, VT_RANK_TO_PE(target_rank, comm),
                               gid, wid, sz * origin_count );
            }
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Accumulate(origin_addr, origin_count, origin_datatype,
                               target_rank, target_disp, target_count,
                               target_datatype, op, win);
    }

  return result;
}

/* -- MPI_Win_fence -- */

VT_MPI_INT MPI_Win_fence(VT_MPI_INT assert, MPI_Win win)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_WIN_FENCE]);

      VT_UNIMCI_CHECK_PRE(MPI_Win_fence,
        (assert, win, "", 0, 0), was_recorded, &time);

      result = PMPI_Win_fence(assert, win);

      VT_UNIMCI_CHECK_POST(MPI_Win_fence,
        (assert, win, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (was_recorded || env_mpi_ignore_filter)
            {
              MPI_Comm comm;
              uint32_t gid, wid;

              vt_win_id(win, &comm, &gid, &wid);
              vt_mpi_rma_end(tid, &time, gid, wid);
            }
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Win_fence(assert, win);
    }

  return result;
}

/* -- MPI_Win_start -- */

VT_MPI_INT MPI_Win_start(MPI_Group group, VT_MPI_INT assert, MPI_Win win)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_WIN_START]);

      VT_UNIMCI_CHECK_PRE(MPI_Win_start,
        (group, assert, win, "", 0, 0), was_recorded, &time);

      result = PMPI_Win_start(group, assert, win);

      VT_UNIMCI_CHECK_POST(MPI_Win_start,
        (group, assert, win, "", 0, 0), was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          vt_win_set_gid(win, vt_group_id(group));
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Win_start(group, assert, win);
    }

  return result;
}

/* -- MPI_Win_complete -- */

VT_MPI_INT MPI_Win_complete(MPI_Win win)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_WIN_COMPLETE]);

      VT_UNIMCI_CHECK_PRE(MPI_Win_complete,
        (win, "", 0, 0), was_recorded, &time);

      result = PMPI_Win_complete(win);

      VT_UNIMCI_CHECK_POST(MPI_Win_complete,
        (win, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          MPI_Comm comm;
          uint32_t gid, wid;

          vt_win_id(win, &comm, &gid, &wid);

          if (was_recorded || env_mpi_ignore_filter)
            {
              vt_comment(tid, &time, "__RMASPECIALGROUP__");
              vt_mpi_rma_end(tid, &time, gid, wid);
            }

          vt_win_set_gid(win, VT_COMM_ID(comm));
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Win_complete(win);
    }

  return result;
}

/* -- MPI_Win_post -- */

VT_MPI_INT MPI_Win_post(MPI_Group group, VT_MPI_INT assert, MPI_Win win)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_WIN_POST]);

      VT_UNIMCI_CHECK_PRE(MPI_Win_post,
        (group, assert, win, "", 0, 0), was_recorded, &time);

      result = PMPI_Win_post(group, assert, win);

      VT_UNIMCI_CHECK_POST(MPI_Win_post,
        (group, assert, win, "", 0, 0), was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          vt_win_set_gid(win, vt_group_id(group));
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Win_post(group, assert, win);
    }

  return result;
}

/* -- MPI_Win_wait -- */

VT_MPI_INT MPI_Win_wait(MPI_Win win)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_WIN_WAIT]);

      VT_UNIMCI_CHECK_PRE(MPI_Win_wait,
        (win, "", 0, 0), was_recorded, &time);

      result = PMPI_Win_wait(win);

      VT_UNIMCI_CHECK_POST(MPI_Win_wait,
        (win, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          MPI_Comm comm;
          uint32_t gid, wid;

          vt_win_id(win, &comm, &gid, &wid);

          if (was_recorded || env_mpi_ignore_filter)
            vt_mpi_rma_end(tid, &time, gid, wid);

          vt_win_set_gid(win, VT_COMM_ID(comm));
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Win_wait(win);
    }

  return result;
}

#if defined(HAVE_PMPI_WIN_TEST) && HAVE_PMPI_WIN_TEST

/* -- MPI_Win_test -- */

VT_MPI_INT MPI_Win_test(MPI_Win win, VT_MPI_INT* flag)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_WIN_TEST]);

      VT_UNIMCI_CHECK_PRE(MPI_Win_test,
        (win, flag, "", 0, 0), was_recorded, &time);

      result = PMPI_Win_test(win, flag);

      VT_UNIMCI_CHECK_POST(MPI_Win_test,
        (win, flag, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          MPI_Comm comm;
          uint32_t gid, wid;

          vt_win_id(win, &comm, &gid, &wid);

          if (*flag && (was_recorded || env_mpi_ignore_filter))
            vt_mpi_rma_end(tid, &time, gid, wid);

          if (*flag)
            vt_win_set_gid(win, VT_COMM_ID(comm));
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Win_test(win, flag);
    }

  return result;
}

#endif /* HAVE_PMPI_WIN_TEST */

#if defined(HAVE_PMPI_WIN_LOCK) && HAVE_PMPI_WIN_LOCK

/* -- MPI_Win_lock -- */

VT_MPI_INT MPI_Win_lock(VT_MPI_INT lock_type, VT_MPI_INT rank,
                        VT_MPI_INT assert, MPI_Win win)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_WIN_LOCK]);

      VT_UNIMCI_CHECK_PRE(MPI_Win_lock,
        (lock_type, rank, assert, win, "", 0, 0), was_recorded, &time);

      result = PMPI_Win_lock(lock_type, rank, assert, win);

      VT_UNIMCI_CHECK_POST(MPI_Win_lock,
        (lock_type, rank, assert, win, "", 0, 0), was_recorded, &time);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          is_rma_putre = 0;
        }

      time = vt_pform_wtime();
      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Win_lock(lock_type, rank, assert, win);
    }

  return result;
}

#endif /* HAVE_PMPI_WIN_LOCK */

#if defined(HAVE_PMPI_WIN_UNLOCK) && HAVE_PMPI_WIN_UNLOCK

/* -- MPI_Win_unlock -- */

VT_MPI_INT MPI_Win_unlock(VT_MPI_INT rank, MPI_Win win)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_WIN_UNLOCK]);

      VT_UNIMCI_CHECK_PRE(MPI_Win_unlock,
        (rank, win, "", 0, 0), was_recorded, &time);

      result = PMPI_Win_unlock(rank, win);

      VT_UNIMCI_CHECK_POST(MPI_Win_unlock,
        (rank, win, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (was_recorded || env_mpi_ignore_filter)
            {
              MPI_Comm comm;
              uint32_t gid, wid;

              vt_win_id(win, &comm, &gid, &wid);
              vt_mpi_rma_end(tid, &time, gid, wid);
              is_rma_putre = 1;
            }
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Win_unlock(rank, win);
    }

  return result;
}

#endif /* HAVE_PMPI_WIN_UNLOCK */

#endif /* HAVE_MPI2_1SIDED */

/*
 *-----------------------------------------------------------------------------
 *
 * MPI-2 Extended collective communication
 *
 *-----------------------------------------------------------------------------
 */

#if defined(HAVE_MPI2_EXTCOLL) && HAVE_MPI2_EXTCOLL

/* -- MPI_Alltoallw -- */

VT_MPI_INT MPI_Alltoallw(CONST void* sendbuf, CONST VT_MPI_INT* sendcounts,
                         CONST VT_MPI_INT* sdispls,
                         CONST MPI_Datatype* sendtypes, void* recvbuf,
                         CONST VT_MPI_INT* recvcounts,
                         CONST VT_MPI_INT* rdispls, CONST MPI_Datatype* recvtypes,
                         MPI_Comm comm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      uint64_t matchid = 0;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_ALLTOALLW]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (was_recorded || env_mpi_ignore_filter)
            {
              VT_MPI_INT N, sendcount = 0, recvcount = 0, sendsz, recvsz, i;
              matchid = VTTHRD_MPICOLLOP_NEXT_MATCHINGID(VTThrdv[tid]);

              PMPI_Comm_size(comm, &N);
              for (i = 0; i < N; i++)
                {
                  PMPI_Type_size(recvtypes[i], &recvsz);
                  PMPI_Type_size(sendtypes[i], &sendsz);
                  recvcount += recvsz * recvcounts[i];
                  sendcount += sendsz * sendcounts[i];
                }

              vt_mpi_collbegin(tid, &time, vt_mpi_regid[VT__MPI_ALLTOALLW],
                               matchid, VT_NO_ID, VT_COMM_ID(comm),
                               sendcount, recvcount);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Alltoallw,
        (sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts,
         rdispls, recvtypes, comm, "", 0, 0), was_recorded, &time);

      result = PMPI_Alltoallw(sendbuf, sendcounts, sdispls, sendtypes,
                              recvbuf, recvcounts, rdispls, recvtypes, comm);

      VT_UNIMCI_CHECK_POST(MPI_Alltoallw,
        (sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts,
         rdispls, recvtypes, comm, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          vt_mpi_collend(tid, &time, matchid, &comm,
                         (was_recorded || env_mpi_ignore_filter));
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Alltoallw(sendbuf, sendcounts, sdispls, sendtypes,
                              recvbuf, recvcounts, rdispls, recvtypes, comm);
    }

  return result;
}

/* -- MPI_Exscan -- */

VT_MPI_INT MPI_Exscan(CONST void* sendbuf, void* recvbuf, VT_MPI_INT count,
                      MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      uint64_t time;
      uint8_t was_recorded;

      uint64_t matchid = 0;

      MPI_TRACE_OFF(tid);

      time = vt_pform_wtime();
      was_recorded = vt_enter(tid, &time, vt_mpi_regid[VT__MPI_EXSCAN]);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          if (was_recorded || env_mpi_ignore_filter)
            {
              VT_MPI_INT me, sz;

              matchid = VTTHRD_MPICOLLOP_NEXT_MATCHINGID(VTThrdv[tid]);

              PMPI_Type_size(datatype, &sz);
              PMPI_Comm_rank(comm, &me);

              vt_mpi_collbegin(tid, &time, vt_mpi_regid[VT__MPI_EXSCAN],
                               matchid, VT_NO_ID, VT_COMM_ID(comm),
                               count * sz, count * sz);
            }
        }

      VT_UNIMCI_CHECK_PRE(MPI_Exscan,
        (sendbuf, recvbuf, count, datatype, op, comm, "", 0, 0),
        was_recorded, &time);

      result = PMPI_Exscan(sendbuf, recvbuf, count, datatype, op, comm);

      VT_UNIMCI_CHECK_POST(MPI_Exscan,
        (sendbuf, recvbuf, count, datatype, op, comm, "", 0, 0),
        was_recorded, &time);

      time = vt_pform_wtime();

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD
      if (!is_mpi_multithreaded)
#endif /* HAVE_MPI2_THREAD */
        {
          vt_mpi_collend(tid, &time, matchid, &comm,
                         (was_recorded || env_mpi_ignore_filter));
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_Exscan(sendbuf, recvbuf, count, datatype, op, comm);
    }

  return result;
}

#endif /* HAVE_MPI2_EXTCOLL */

/*
 *-----------------------------------------------------------------------------
 *
 * MPI-2 I/O
 *
 *-----------------------------------------------------------------------------
 */

#if defined(HAVE_MPI2_IO) && HAVE_MPI2_IO

/* Some macros, to ease the programming, and because MaJu likes them so much ;-)
 */ 

#if !(defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD)
  static const uint8_t is_mpi_multithreaded = 0;
#endif /* HAVE_MPI2_THREAD */

/**
 * Write function enter record and make MPI_Status available.
 */
#define MPIIO_ENTER_IO_W_STATUS(REGIONID) \
  uint64_t time; \
  MPI_Status mystatus; \
  uint8_t was_recorded; \
  MPI_TRACE_OFF(tid); \
  time = vt_pform_wtime(); \
  was_recorded = vt_enter(tid, &time, vt_mpi_regid[REGIONID]); \
  if (!is_mpi_multithreaded && was_recorded && (status == MPI_STATUS_IGNORE)) \
    { \
      status = &mystatus; \
    }

/**
 * Write function enter record, I/O begin record, make matchingid available.
 */
#define MPIIO_ENTER_IO_W_MATCHINGID(REGIONID) \
  uint64_t time; \
  uint64_t matchingid = 0; \
  uint8_t was_recorded; \
  MPI_TRACE_OFF(tid); \
  time = vt_pform_wtime(); \
  was_recorded = vt_enter(tid, &time, vt_mpi_regid[REGIONID]); \
  if (!is_mpi_multithreaded && was_recorded) \
    { \
      matchingid = VTTHRD_IO_NEXT_MATCHINGID(VTThrdv[tid]); \
      vt_iobegin(tid, &time, matchingid); \
    }

/**
 * Write function enter record, I/O begin record, save matchingid and datatype
 * with the associated MPI_File fh.
 */
#define MPIIO_ENTER_IO_SPLITCOLL(REGIONID) \
  uint64_t time; \
  uint8_t was_recorded; \
  MPI_TRACE_OFF(tid); \
  time = vt_pform_wtime(); \
  was_recorded = vt_enter(tid, &time, vt_mpi_regid[REGIONID]); \
  if (!is_mpi_multithreaded && was_recorded) \
    { \
      uint64_t matchingid; \
      vt_mpifile_data *fdata; \
      matchingid = VTTHRD_IO_NEXT_MATCHINGID(VTThrdv[tid]); \
      vt_iobegin(tid, &time, matchingid); \
      fdata = vt_mpifile_get_data(fh); \
      fdata->split_collective_id = matchingid; \
      fdata->datatype = datatype; \
    }

/**
 * Write function enter record, I/O begin record, make MPI_Status and matchingid
 * available.
 */
#define MPIIO_ENTER_IO_W_MATCHINGID_STATUS(REGIONID) \
  uint64_t time; \
  uint64_t matchingid = 0; \
  MPI_Status mystatus; \
  uint8_t was_recorded; \
  MPI_TRACE_OFF(tid); \
  time = vt_pform_wtime(); \
  was_recorded = vt_enter(tid, &time, vt_mpi_regid[REGIONID]); \
  if (!is_mpi_multithreaded && was_recorded) \
    { \
      matchingid = VTTHRD_IO_NEXT_MATCHINGID(VTThrdv[tid]); \
      vt_iobegin(tid, &time, matchingid); \
      if (status == MPI_STATUS_IGNORE) \
        status = &mystatus; \
    }

/**
 * Just write function leave record.
 */
#define MPIIO_LEAVE_IO() \
  time = vt_pform_wtime(); \
  vt_exit(tid, &time); \
  MPI_TRACE_ON(tid)

/**
 * Write I/O end record, leave record (Used for 0-byte operations like open,
 * close, seek). Needs matchingid.
 */
#define MPIIO_LEAVE_IO_W_MATCHINGID(IOOP) \
  time = vt_pform_wtime(); \
  if (!is_mpi_multithreaded && was_recorded) \
    { \
      vt_mpifile_data *fdata = vt_mpifile_get_data(fh); \
      uint32_t fileop = IOOP; \
      if (result != MPI_SUCCESS) \
        { \
          fileop |= VT_IOFLAG_IOFAILED; \
        } \
      vt_ioend(tid, &time, fdata->fid, matchingid, fdata->handle, fileop, 0); \
    } \
  vt_exit(tid, &time); \
  MPI_TRACE_ON(tid)

/**
 * If nonblocking function was successful, create vt_request object; if not,
 * write I/O end record indicating failure; write function leave record in any
 * case.
 */
#define MPIIO_LEAVE_IO_W_REQ(IOOP) \
  time = vt_pform_wtime(); \
  if (!is_mpi_multithreaded && was_recorded) \
    { \
      vt_mpifile_data *fdata = vt_mpifile_get_data(fh); \
      if (result == MPI_SUCCESS) \
        { \
          vt_iorequest_create(*request, datatype, matchingid, fdata->handle, \
                              fdata->fid, IOOP); \
        } \
      else \
        { \
          vt_ioend(tid, &time, fdata->fid, matchingid, fdata->handle, IOOP | \
                   VT_IOFLAG_IOFAILED, 0); \
        } \
    } \
  vt_exit(tid, &time); \
  MPI_TRACE_ON(tid)

/**
 * Write I/O end record, function leave record for this _splitcollective_
 * operation. Retrieves all needed information from the associated
 * vt_mpifile_data object.
 */
#define MPIIO_LEAVE_IO_SPLITCOLL(IOOP) \
  time = vt_pform_wtime(); \
  if (!is_mpi_multithreaded && was_recorded) \
    { \
      vt_mpifile_data *fdata = vt_mpifile_get_data(fh); \
      uint64_t nbytes = 0; \
      uint32_t fileop = IOOP; \
      if (result == MPI_SUCCESS) \
        { \
          VT_MPI_INT sz, cnt; \
          PMPI_Type_size(fdata->datatype, &sz); \
          PMPI_Get_count(status, fdata->datatype, &cnt); \
          if (cnt != MPI_UNDEFINED) \
            nbytes = (uint64_t)cnt * (uint64_t)sz; \
        } \
      else \
        { \
          fileop |= VT_IOFLAG_IOFAILED; \
        } \
      vt_ioend(tid, &time, fdata->fid, fdata->split_collective_id, \
               fdata->handle, fileop, nbytes); \
    } \
  vt_exit(tid, &time); \
  MPI_TRACE_ON(tid)

/**
 * Write I/O end record, function leave record for this operation (usually
 * simple read or write). Needs matchingid and status object.
 */
#define MPIIO_LEAVE_IO_W_MATCHINGID_STATUS(IOOP) \
  time = vt_pform_wtime(); \
  if (!is_mpi_multithreaded && was_recorded) \
    { \
      vt_mpifile_data *fdata = vt_mpifile_get_data(fh); \
      if (result == MPI_SUCCESS) \
        { \
          VT_MPI_INT sz, cnt; \
          PMPI_Type_size(datatype, &sz); \
          PMPI_Get_count(status, datatype, &cnt); \
          if (cnt == MPI_UNDEFINED) \
            cnt = 0; \
          vt_ioend(tid, &time, fdata->fid, matchingid, fdata->handle, IOOP, \
                   (uint64_t)cnt * (uint64_t)sz); \
        } \
      else \
        { \
          vt_ioend(tid, &time, fdata->fid, matchingid, fdata->handle, IOOP | \
                   VT_IOFLAG_IOFAILED, 0); \
        } \
    } \
  vt_exit(tid, &time); \
  MPI_TRACE_ON(tid)

/*
 *-----------------------------------------------------------------------------
 *
 * File access
 *
 *-----------------------------------------------------------------------------
 */

/* -- MPI_File_close -- */

VT_MPI_INT MPI_File_close(MPI_File* fh)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPI_File bak = *fh;
      MPIIO_ENTER_IO_W_MATCHINGID(VT__MPI_FILE_CLOSE);

      VT_UNIMCI_CHECK_PRE(MPI_File_close,
        (fh, "", 0, 0), was_recorded, &time);

      result = PMPI_File_close(fh);

      VT_UNIMCI_CHECK_POST(MPI_File_close,
        (fh, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

      if (!is_mpi_multithreaded)
        {
          if (was_recorded)
            {
              vt_mpifile_data* fdata = vt_mpifile_get_data(bak);
              if (result == MPI_SUCCESS)
                {
                  vt_ioend(tid, &time, fdata->fid, matchingid, fdata->handle,
                           VT_IOOP_CLOSE | VT_IOFLAG_COLL, 0);
                }
              else
                {
                  vt_ioend(tid, &time, fdata->fid, matchingid, fdata->handle,
                           VT_IOOP_CLOSE | VT_IOFLAG_COLL | VT_IOFLAG_IOFAILED,
                           0);
                }
            }
          vt_mpifile_free(bak);
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_File_close(fh);
    }

  return result;
}

/* -- MPI_File_open -- */

VT_MPI_INT MPI_File_open(MPI_Comm comm, CONST char* filename, VT_MPI_INT amode,
                         MPI_Info info, MPI_File* fh)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID(VT__MPI_FILE_OPEN);

      VT_UNIMCI_CHECK_PRE(MPI_File_open,
        (comm, filename, amode, info, fh, "", 0, 0), was_recorded, &time);

      result = PMPI_File_open(comm, filename, amode, info, fh);

      VT_UNIMCI_CHECK_POST(MPI_File_open,
        (comm, filename, amode, info, fh, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

      if (!is_mpi_multithreaded)
        {
          vt_mpifile_data* fdata = vt_mpifile_create(*fh, filename);
          if (was_recorded)
            {
              uint32_t fileop = VT_IOOP_OPEN | VT_IOFLAG_COLL;
              if (result != MPI_SUCCESS)
                {
                  fileop |= VT_IOFLAG_IOFAILED;
                }
              vt_ioend(tid, &time, fdata->fid, matchingid, fdata->handle,
                       fileop, 0);
            }
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_File_open(comm, filename, amode, info, fh);
    }

  return result;
}

/* -- MPI_File_delete -- */

VT_MPI_INT MPI_File_delete(CONST char* filename, MPI_Info info)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID(VT__MPI_FILE_DELETE);

      VT_UNIMCI_CHECK_PRE(MPI_File_delete,
        (filename, info, "", 0, 0), was_recorded, &time);

      result = PMPI_File_delete(filename, info);

      VT_UNIMCI_CHECK_POST(MPI_File_delete,
        (filename, info, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

      if (!is_mpi_multithreaded && was_recorded)
        {
          uint32_t fid = vt_mpifilename_get_id(filename);
          uint32_t fileop = VT_IOOP_UNLINK;
          if (result != MPI_SUCCESS)
            {
              fileop |= VT_IOFLAG_IOFAILED;
            }
          vt_ioend(tid, &time, fid, matchingid, 0, fileop, 0);
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_File_delete(filename, info);
    }

  return result;
}

/* -- MPI_File_sync -- */

VT_MPI_INT MPI_File_sync(MPI_File fh)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID(VT__MPI_FILE_SYNC);

      VT_UNIMCI_CHECK_PRE(MPI_File_sync,
        (fh, "", 0, 0), was_recorded, &time);

      result = PMPI_File_sync(fh);

      VT_UNIMCI_CHECK_POST(MPI_File_sync,
        (fh, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO_W_MATCHINGID(VT_IOOP_SYNC);
    }
  else
    {
      result = PMPI_File_sync(fh);
    }

  return result;
}

/* -- MPI_File_preallocate -- */

VT_MPI_INT MPI_File_preallocate(MPI_File fh, MPI_Offset size)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID(VT__MPI_FILE_PREALLOCATE);

      VT_UNIMCI_CHECK_PRE(MPI_File_preallocate,
        (fh, size, "", 0, 0), was_recorded, &time);

      result = PMPI_File_preallocate(fh, size);

      VT_UNIMCI_CHECK_POST(MPI_File_preallocate,
        (fh, size, "", 0, 0), was_recorded, &time);

      time = vt_pform_wtime();

      if (!is_mpi_multithreaded && was_recorded)
        {
          vt_mpifile_data* fdata = vt_mpifile_get_data(fh);
          uint32_t fileop = VT_IOOP_WRITE;
          if (result != MPI_SUCCESS)
            {
              fileop |= VT_IOFLAG_IOFAILED;
            }
          vt_ioend(tid, &time, fdata->fid, matchingid, fdata->handle, fileop,
                   (uint64_t)size);
        }

      vt_exit(tid, &time);

      MPI_TRACE_ON(tid);
    }
  else
    {
      result = PMPI_File_preallocate(fh, size);
    }

  return result;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Data access with individual file pointers
 *
 *-----------------------------------------------------------------------------
 */

/* -- MPI_File_iread -- */
VT_MPI_INT MPI_File_iread(MPI_File fh, void* buf, VT_MPI_INT count,
                          MPI_Datatype datatype, MPI_Request* request)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID(VT__MPI_FILE_IREAD);

      VT_UNIMCI_CHECK_PRE(MPI_File_iread,
        (fh, buf, count, datatype, request, "", 0, 0), was_recorded, &time);

      result = PMPI_File_iread(fh, buf, count, datatype, request);

      VT_UNIMCI_CHECK_POST(MPI_File_iread,
        (fh, buf, count, datatype, request, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO_W_REQ(VT_IOOP_READ | VT_IOFLAG_ASYNC);
    }
  else
    {
      result = PMPI_File_iread(fh, buf, count, datatype, request);
    }

  return result;
}

/* -- MPI_File_iwrite -- */

VT_MPI_INT MPI_File_iwrite(MPI_File fh, CONST void* buf, VT_MPI_INT count,
                           MPI_Datatype datatype, MPI_Request* request)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID(VT__MPI_FILE_IWRITE);

      VT_UNIMCI_CHECK_PRE(MPI_File_iwrite,
        (fh, buf, count, datatype, request, "", 0, 0), was_recorded, &time);

      result = PMPI_File_iwrite(fh, buf, count, datatype, request);

      VT_UNIMCI_CHECK_POST(MPI_File_iwrite,
        (fh, buf, count, datatype, request, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO_W_REQ(VT_IOOP_WRITE | VT_IOFLAG_ASYNC);
    }
  else
    {
      result = PMPI_File_iwrite(fh, buf, count, datatype, request);
    }

  return result;
}

/* -- MPI_File_read -- */

VT_MPI_INT MPI_File_read(MPI_File fh, void* buf, VT_MPI_INT count,
                         MPI_Datatype datatype, MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID_STATUS(VT__MPI_FILE_READ);

      VT_UNIMCI_CHECK_PRE(MPI_File_read,
        (fh, buf, count, datatype, status, "", 0, 0), was_recorded, &time);

      result = PMPI_File_read(fh, buf, count, datatype, status);

      VT_UNIMCI_CHECK_POST(MPI_File_read,
        (fh, buf, count, datatype, status, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO_W_MATCHINGID_STATUS(VT_IOOP_READ);
    }
  else
    {
      result = PMPI_File_read(fh, buf, count, datatype, status);
    }

  return result;
}

/* -- MPI_File_read_all -- */

VT_MPI_INT MPI_File_read_all(MPI_File fh, void* buf, VT_MPI_INT count,
                             MPI_Datatype datatype, MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID_STATUS(VT__MPI_FILE_READ_ALL);

      VT_UNIMCI_CHECK_PRE(MPI_File_read_all,
        (fh, buf, count, datatype, status, "", 0, 0), was_recorded, &time);

      result = PMPI_File_read_all(fh, buf, count, datatype, status);

      VT_UNIMCI_CHECK_POST(MPI_File_read_all,
        (fh, buf, count, datatype, status, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO_W_MATCHINGID_STATUS(VT_IOOP_READ | VT_IOFLAG_COLL);
    }
  else
    {
      result = PMPI_File_read_all(fh, buf, count, datatype, status);
    }

  return result;
}

/* -- MPI_File_seek -- */

VT_MPI_INT MPI_File_seek(MPI_File fh, MPI_Offset offset, VT_MPI_INT whence)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID(VT__MPI_FILE_SEEK);

      VT_UNIMCI_CHECK_PRE(MPI_File_seek,
        (fh, offset, whence, "", 0, 0), was_recorded, &time);

      result = PMPI_File_seek(fh, offset, whence);

      VT_UNIMCI_CHECK_POST(MPI_File_seek,
        (fh, offset, whence, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO_W_MATCHINGID(VT_IOOP_SEEK);
    }
  else
    {
      result = PMPI_File_seek(fh, offset, whence);
    }

  return result;
}

/* -- MPI_File_write -- */

VT_MPI_INT MPI_File_write(MPI_File fh, CONST void* buf, VT_MPI_INT count,
                          MPI_Datatype datatype, MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID_STATUS(VT__MPI_FILE_WRITE);

      VT_UNIMCI_CHECK_PRE(MPI_File_write,
        (fh, buf, count, datatype, status, "", 0, 0), was_recorded, &time);

      result = PMPI_File_write(fh, buf, count, datatype, status);

      VT_UNIMCI_CHECK_POST(MPI_File_write,
        (fh, buf, count, datatype, status, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO_W_MATCHINGID_STATUS(VT_IOOP_WRITE);
    }
  else
    {
      result = PMPI_File_write(fh, buf, count, datatype, status);
    }

  return result;
}

/* -- MPI_File_write_all -- */

VT_MPI_INT MPI_File_write_all(MPI_File fh, CONST void* buf, VT_MPI_INT count,
                              MPI_Datatype datatype, MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID_STATUS(VT__MPI_FILE_WRITE_ALL);

      VT_UNIMCI_CHECK_PRE(MPI_File_write_all,
        (fh, buf, count, datatype, status, "", 0, 0), was_recorded, &time);

      result = PMPI_File_write_all(fh, buf, count, datatype, status);

      VT_UNIMCI_CHECK_POST(MPI_File_write_all,
        (fh, buf, count, datatype, status, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO_W_MATCHINGID_STATUS(VT_IOOP_WRITE | VT_IOFLAG_COLL);
    }
  else
    {
      result = PMPI_File_write_all(fh, buf, count, datatype, status);
    }

  return result;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Split collective data access routines
 *
 *-----------------------------------------------------------------------------
 */

/* -- MPI_File_read_all_begin -- */

VT_MPI_INT MPI_File_read_all_begin(MPI_File fh, void* buf,
                                   VT_MPI_INT count, MPI_Datatype datatype)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_SPLITCOLL(VT__MPI_FILE_READ_ALL_BEGIN);

      VT_UNIMCI_CHECK_PRE(MPI_File_read_all_begin,
        (fh, buf, count, datatype, "", 0, 0), was_recorded, &time);

      result = PMPI_File_read_all_begin(fh, buf, count, datatype);

      VT_UNIMCI_CHECK_POST(MPI_File_read_all_begin,
        (fh, buf, count, datatype, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO();
    }
  else
    {
      result = PMPI_File_read_all_begin(fh, buf, count, datatype);
    }

  return result;
}

/* -- MPI_File_read_all_end -- */

VT_MPI_INT MPI_File_read_all_end(MPI_File fh, void* buf, MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_STATUS(VT__MPI_FILE_READ_ALL_END);

      VT_UNIMCI_CHECK_PRE(MPI_File_read_all_end,
        (fh, buf, status, "", 0, 0), was_recorded, &time);

      result = PMPI_File_read_all_end(fh, buf, status);

      VT_UNIMCI_CHECK_POST(MPI_File_read_all_end,
        (fh, buf, status, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO_SPLITCOLL(VT_IOOP_READ | VT_IOFLAG_ASYNC | VT_IOFLAG_COLL);
    }
  else
    {
      result = PMPI_File_read_all_end(fh, buf, status);
    }

  return result;
}

/* -- MPI_File_read_at_all_begin -- */

VT_MPI_INT MPI_File_read_at_all_begin(MPI_File fh, MPI_Offset offset,
                                      void* buf, VT_MPI_INT count,
                                      MPI_Datatype datatype )
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_SPLITCOLL(VT__MPI_FILE_READ_AT_ALL_BEGIN);

      VT_UNIMCI_CHECK_PRE(MPI_File_read_at_all_begin,
        (fh, offset, buf, count, datatype, "", 0, 0), was_recorded, &time);

      result = PMPI_File_read_at_all_begin(fh, offset, buf, count, datatype);

      VT_UNIMCI_CHECK_POST(MPI_File_read_at_all_begin,
        (fh, offset, buf, count, datatype, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO();
    }
  else
    {
      result = PMPI_File_read_at_all_begin(fh, offset, buf, count, datatype);
    }

  return result;
}

/* -- MPI_File_read_at_all_end -- */

VT_MPI_INT MPI_File_read_at_all_end(MPI_File fh, void* buf, MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_STATUS(VT__MPI_FILE_READ_AT_ALL_END);

      VT_UNIMCI_CHECK_PRE(MPI_File_read_at_all_end,
        (fh, buf, status, "", 0, 0), was_recorded, &time);

      result = PMPI_File_read_at_all_end(fh, buf, status);

      VT_UNIMCI_CHECK_POST(MPI_File_read_at_all_end,
        (fh, buf, status, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO_SPLITCOLL(VT_IOOP_READ | VT_IOFLAG_ASYNC | VT_IOFLAG_COLL);
    }
  else
    {
      result = PMPI_File_read_at_all_end(fh, buf, status);
    }

  return result;
}

#if defined(HAVE_PMPI_FILE_READ_ORDERED_BEGIN) && HAVE_PMPI_FILE_READ_ORDERED_BEGIN

/* -- MPI_File_read_ordered_begin -- */

VT_MPI_INT MPI_File_read_ordered_begin(MPI_File fh, void* buf, VT_MPI_INT count,
                                       MPI_Datatype datatype)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_SPLITCOLL(VT__MPI_FILE_READ_ORDERED_BEGIN);

      VT_UNIMCI_CHECK_PRE(MPI_File_read_ordered_begin,
        (fh, buf, count, datatype, "", 0, 0), was_recorded, &time);

      result = PMPI_File_read_ordered_begin(fh, buf, count, datatype);

      VT_UNIMCI_CHECK_POST(MPI_File_read_ordered_begin,
        (fh, buf, count, datatype, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO();
    }
  else
    {
      result = PMPI_File_read_ordered_begin(fh, buf, count, datatype);
    }

  return result;
}

/* -- MPI_File_read_ordered_end -- */

VT_MPI_INT MPI_File_read_ordered_end(MPI_File fh, void* buf, MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_STATUS(VT__MPI_FILE_READ_ORDERED_END);

      VT_UNIMCI_CHECK_PRE(MPI_File_read_ordered_end,
        (fh, buf, status, "", 0, 0), was_recorded, &time);

      result = PMPI_File_read_ordered_end(fh, buf, status);

      VT_UNIMCI_CHECK_POST(MPI_File_read_ordered_end,
        (fh, buf, status, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO_SPLITCOLL(VT_IOOP_READ | VT_IOFLAG_ASYNC | VT_IOFLAG_COLL);
    }
  else
    {
      result = PMPI_File_read_ordered_end(fh, buf, status);
    }

  return result;
}

#endif /* HAVE_PMPI_FILE_READ_ORDERED_BEGIN */

/* -- MPI_File_write_all_begin -- */

VT_MPI_INT MPI_File_write_all_begin(MPI_File fh, CONST void* buf, VT_MPI_INT count,
                                    MPI_Datatype datatype)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_SPLITCOLL(VT__MPI_FILE_WRITE_ALL_BEGIN);

      VT_UNIMCI_CHECK_PRE(MPI_File_write_all_begin,
        (fh, buf, count, datatype, "", 0, 0), was_recorded, &time);

      result = PMPI_File_write_all_begin(fh, buf, count, datatype);

      VT_UNIMCI_CHECK_POST(MPI_File_write_all_begin,
        (fh, buf, count, datatype, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO();
    }
  else
    {
      result = PMPI_File_write_all_begin(fh, buf, count, datatype);
    }

  return result;
}

/* -- MPI_File_write_all_end -- */

VT_MPI_INT MPI_File_write_all_end(MPI_File fh, CONST void* buf,
                                  MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_STATUS(VT__MPI_FILE_WRITE_ALL_END);

      VT_UNIMCI_CHECK_PRE(MPI_File_write_all_end,
        (fh, buf, status, "", 0, 0), was_recorded, &time);

      result = PMPI_File_write_all_end(fh, buf, status);

      VT_UNIMCI_CHECK_POST(MPI_File_write_all_end,
        (fh, buf, status, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO_SPLITCOLL(VT_IOOP_WRITE | VT_IOFLAG_ASYNC | VT_IOFLAG_COLL);
    }
  else
    {
      result = PMPI_File_write_all_end(fh, buf, status);
    }

  return result;
}

/* -- MPI_File_write_at_all_begin -- */

VT_MPI_INT MPI_File_write_at_all_begin(MPI_File fh, MPI_Offset offset,
                                       CONST void* buf, VT_MPI_INT count,
                                       MPI_Datatype datatype)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_SPLITCOLL(VT__MPI_FILE_WRITE_AT_ALL_BEGIN);

      VT_UNIMCI_CHECK_PRE(MPI_File_write_at_all_begin,
        (fh, offset, buf, count, datatype, "", 0, 0), was_recorded, &time);

      result = PMPI_File_write_at_all_begin(fh, offset, buf, count, datatype);

      VT_UNIMCI_CHECK_POST(MPI_File_write_at_all_begin,
        (fh, offset, buf, count, datatype, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO();
    }
  else
    {
      result = PMPI_File_write_at_all_begin(fh, offset, buf, count, datatype);
    }

  return result;
}

/* -- MPI_File_write_at_all_end -- */

VT_MPI_INT MPI_File_write_at_all_end(MPI_File fh, CONST void* buf,
                                     MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_STATUS(VT__MPI_FILE_WRITE_AT_ALL_END);

      VT_UNIMCI_CHECK_PRE(MPI_File_write_at_all_end,
        (fh, buf, status, "", 0, 0), was_recorded, &time);

      result = PMPI_File_write_at_all_end(fh, buf, status);

      VT_UNIMCI_CHECK_POST(MPI_File_write_at_all_end,
        (fh, buf, status, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO_SPLITCOLL(VT_IOOP_WRITE | VT_IOFLAG_ASYNC | VT_IOFLAG_COLL);
    }
  else
    {
      result = PMPI_File_write_at_all_end(fh, buf, status);
    }

  return result;
}

#if defined(HAVE_PMPI_FILE_WRITE_ORDERED_BEGIN) && HAVE_PMPI_FILE_WRITE_ORDERED_BEGIN

/* -- MPI_File_write_ordered_begin -- */

VT_MPI_INT MPI_File_write_ordered_begin(MPI_File fh, CONST void* buf,
                                        VT_MPI_INT count, MPI_Datatype datatype)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_SPLITCOLL(VT__MPI_FILE_WRITE_ORDERED_BEGIN);

      VT_UNIMCI_CHECK_PRE(MPI_File_write_ordered_begin,
        (fh, buf, count, datatype, "", 0, 0), was_recorded, &time);

      result = PMPI_File_write_ordered_begin(fh, buf, count, datatype);

      VT_UNIMCI_CHECK_POST(MPI_File_write_ordered_begin,
        (fh, buf, count, datatype, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO();
    }
  else
    {
      result = PMPI_File_write_ordered_begin(fh, buf, count, datatype);
    }

  return result;
}

/* -- MPI_File_write_ordered_end -- */

VT_MPI_INT MPI_File_write_ordered_end(MPI_File fh, CONST void* buf,
                                      MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_STATUS(VT__MPI_FILE_WRITE_ORDERED_END);

      VT_UNIMCI_CHECK_PRE(MPI_File_write_ordered_end,
        (fh, buf, status, "", 0, 0), was_recorded, &time);

      result = PMPI_File_write_ordered_end(fh, buf, status);

      VT_UNIMCI_CHECK_POST(MPI_File_write_ordered_end,
        (fh, buf, status, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO_SPLITCOLL(VT_IOOP_WRITE | VT_IOFLAG_ASYNC | VT_IOFLAG_COLL);
    }
  else
    {
      result = PMPI_File_write_ordered_end(fh, buf, status);
    }

  return result;
}

#endif /* HAVE_PMPI_FILE_WRITE_ORDERED_BEGIN */

/*
 *-----------------------------------------------------------------------------
 *
 * Data access with explicit offsets
 *
 *-----------------------------------------------------------------------------
 */

/* -- MPI_File_iread_at -- */

VT_MPI_INT MPI_File_iread_at(MPI_File fh, MPI_Offset offset, void* buf,
                             VT_MPI_INT count, MPI_Datatype datatype,
                             MPI_Request* request)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID(VT__MPI_FILE_IREAD_AT);

      VT_UNIMCI_CHECK_PRE(MPI_File_iread_at,
        (fh, offset, buf, count, datatype, request, "", 0, 0),
        was_recorded, &time);

      result = PMPI_File_iread_at(fh, offset, buf, count, datatype, request);

      VT_UNIMCI_CHECK_POST(MPI_File_iread_at,
        (fh, offset, buf, count, datatype, request, "", 0, 0),
        was_recorded, &time);

      MPIIO_LEAVE_IO_W_REQ(VT_IOOP_WRITE | VT_IOFLAG_ASYNC);
    }
  else
    {
      result = PMPI_File_iread_at(fh, offset, buf, count, datatype, request);
    }

  return result;
}

/* -- MPI_File_iwrite_at -- */

VT_MPI_INT MPI_File_iwrite_at(MPI_File fh, MPI_Offset offset, CONST void* buf,
                              VT_MPI_INT count, MPI_Datatype datatype,
                              MPI_Request* request)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID(VT__MPI_FILE_IWRITE_AT);

      VT_UNIMCI_CHECK_PRE(MPI_File_iwrite_at,
        (fh, offset, buf, count, datatype, request, "", 0, 0),
        was_recorded, &time);

      result = PMPI_File_iwrite_at(fh, offset, buf, count, datatype, request);

      VT_UNIMCI_CHECK_POST(MPI_File_iwrite_at,
        (fh, offset, buf, count, datatype, request, "", 0, 0),
        was_recorded, &time);

      MPIIO_LEAVE_IO_W_REQ(VT_IOOP_WRITE | VT_IOFLAG_ASYNC);
    }
  else
    {
      result = PMPI_File_iwrite_at(fh, offset, buf, count, datatype, request);
    }

  return result;
}

/* -- MPI_File_read_at -- */

VT_MPI_INT MPI_File_read_at(MPI_File fh, MPI_Offset offset, void* buf,
                            VT_MPI_INT count, MPI_Datatype datatype,
                            MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID_STATUS(VT__MPI_FILE_READ_AT);

      VT_UNIMCI_CHECK_PRE(MPI_File_read_at,
        (fh, offset, buf, count, datatype, status, "", 0, 0),
        was_recorded, &time);

      result = PMPI_File_read_at(fh, offset, buf, count, datatype, status);

      VT_UNIMCI_CHECK_POST(MPI_File_read_at,
        (fh, offset, buf, count, datatype, status, "", 0, 0),
        was_recorded, &time);

      MPIIO_LEAVE_IO_W_MATCHINGID_STATUS(VT_IOOP_READ);
    }
  else
    {
      result = PMPI_File_read_at(fh, offset, buf, count, datatype, status);
    }

  return result;
}

/* -- MPI_File_read_at_all -- */

VT_MPI_INT MPI_File_read_at_all(MPI_File fh, MPI_Offset offset, void* buf,
                                VT_MPI_INT count, MPI_Datatype datatype,
                                MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID_STATUS(VT__MPI_FILE_READ_AT_ALL);

      VT_UNIMCI_CHECK_PRE(MPI_File_read_at_all,
        (fh, offset, buf, count, datatype, status, "", 0, 0),
        was_recorded, &time);

      result = PMPI_File_read_at_all(fh, offset, buf, count, datatype, status);

      VT_UNIMCI_CHECK_POST(MPI_File_read_at_all,
        (fh, offset, buf, count, datatype, status, "", 0, 0),
        was_recorded, &time);

      MPIIO_LEAVE_IO_W_MATCHINGID_STATUS(VT_IOOP_READ | VT_IOFLAG_COLL);
    }
  else
    {
      result = PMPI_File_read_at_all(fh, offset, buf, count, datatype, status);
    }

  return result;
}

/* -- MPI_File_write_at -- */

VT_MPI_INT MPI_File_write_at(MPI_File fh, MPI_Offset offset, CONST void* buf,
                             VT_MPI_INT count, MPI_Datatype datatype,
                             MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID_STATUS(VT__MPI_FILE_WRITE_AT);

      VT_UNIMCI_CHECK_PRE(MPI_File_write_at,
        (fh, offset, buf, count, datatype, status, "", 0, 0),
        was_recorded, &time);

      result = PMPI_File_write_at(fh, offset, buf, count, datatype, status);

      VT_UNIMCI_CHECK_POST(MPI_File_write_at,
        (fh, offset, buf, count, datatype, status, "", 0, 0),
        was_recorded, &time);

      MPIIO_LEAVE_IO_W_MATCHINGID_STATUS(VT_IOOP_WRITE);
    }
  else
    {
      result = PMPI_File_write_at(fh, offset, buf, count, datatype, status);
    }

  return result;
}

/* -- MPI_File_write_at_all -- */

VT_MPI_INT MPI_File_write_at_all(MPI_File fh, MPI_Offset offset,
                                 CONST void* buf, VT_MPI_INT count,
                                 MPI_Datatype datatype, MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID_STATUS(VT__MPI_FILE_WRITE_AT_ALL);

      VT_UNIMCI_CHECK_PRE(MPI_File_write_at_all,
        (fh, offset, buf, count, datatype, status, "", 0, 0),
        was_recorded, &time);

      result = PMPI_File_write_at_all(fh, offset, buf, count, datatype, status);

      VT_UNIMCI_CHECK_POST(MPI_File_write_at_all,
        (fh, offset, buf, count, datatype, status, "", 0, 0),
        was_recorded, &time);

      MPIIO_LEAVE_IO_W_MATCHINGID_STATUS(VT_IOOP_WRITE | VT_IOFLAG_COLL);
    }
  else
    {
      result = PMPI_File_write_at_all(fh, offset, buf, count, datatype, status);
    }

  return result;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Data access with shared file pointers
 *
 *-----------------------------------------------------------------------------
 */

/* -- MPI_File_iread_shared -- */

VT_MPI_INT MPI_File_iread_shared(MPI_File fh, void* buf, VT_MPI_INT count,
                                 MPI_Datatype datatype, MPI_Request* request)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID(VT__MPI_FILE_IREAD_SHARED);

      VT_UNIMCI_CHECK_PRE(MPI_File_iread_shared,
        (fh, buf, count, datatype, request, "", 0, 0), was_recorded, &time);

      result = PMPI_File_iread_shared(fh, buf, count, datatype, request);

      VT_UNIMCI_CHECK_POST(MPI_File_iread_shared,
        (fh, buf, count, datatype, request, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO_W_REQ(VT_IOOP_READ | VT_IOFLAG_ASYNC);
    }
  else
    {
      result = PMPI_File_iread_shared(fh, buf, count, datatype, request);
    }

  return result;
}

/* -- MPI_File_iwrite_shared -- */

VT_MPI_INT MPI_File_iwrite_shared(MPI_File fh, CONST void* buf,
                                  VT_MPI_INT count, MPI_Datatype datatype,
                                  MPI_Request* request)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID(VT__MPI_FILE_IWRITE_SHARED);

      VT_UNIMCI_CHECK_PRE(MPI_File_iwrite_shared,
        (fh, buf, count, datatype, request, "", 0, 0), was_recorded, &time);

      result = PMPI_File_iwrite_shared(fh, buf, count, datatype, request);

      VT_UNIMCI_CHECK_POST(MPI_File_iwrite_shared,
        (fh, buf, count, datatype, request, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO_W_REQ(VT_IOOP_WRITE | VT_IOFLAG_ASYNC);
    }
  else
    {
      result = PMPI_File_iwrite_shared(fh, buf, count, datatype, request);
    }

  return result;
}

#if defined(HAVE_PMPI_FILE_READ_ORDERED) && HAVE_PMPI_FILE_READ_ORDERED

/* -- MPI_File_read_ordered -- */

VT_MPI_INT MPI_File_read_ordered(MPI_File fh, void* buf, VT_MPI_INT count,
                                 MPI_Datatype datatype, MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID_STATUS(VT__MPI_FILE_READ_ORDERED);

      VT_UNIMCI_CHECK_PRE(MPI_File_read_ordered,
        (fh, buf, count, datatype, status, "", 0, 0), was_recorded, &time);

      result = PMPI_File_read_ordered(fh, buf, count, datatype, status);

      VT_UNIMCI_CHECK_POST(MPI_File_read_ordered,
        (fh, buf, count, datatype, status, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO_W_MATCHINGID_STATUS(VT_IOOP_READ | VT_IOFLAG_COLL);
    }
  else
    {
      result = PMPI_File_read_ordered(fh, buf, count, datatype, status);
    }

  return result;
}

#endif /* HAVE_PMPI_FILE_READ_ORDERED */

/* -- MPI_File_read_shared -- */

VT_MPI_INT MPI_File_read_shared(MPI_File fh, void* buf, VT_MPI_INT count,
                                MPI_Datatype datatype, MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID_STATUS(VT__MPI_FILE_READ_SHARED);

      VT_UNIMCI_CHECK_PRE(MPI_File_read_shared,
        (fh, buf, count, datatype, status, "", 0, 0), was_recorded, &time);

      result = PMPI_File_read_shared(fh, buf, count, datatype, status);

      VT_UNIMCI_CHECK_PRE(MPI_File_read_shared,
        (fh, buf, count, datatype, status, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO_W_MATCHINGID_STATUS(VT_IOOP_READ);
    }
  else
    {
      result = PMPI_File_read_shared(fh, buf, count, datatype, status);
    }

  return result;
}

/* -- MPI_File_seek_shared -- */

VT_MPI_INT MPI_File_seek_shared(MPI_File fh, MPI_Offset offset,
                                VT_MPI_INT whence)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID(VT__MPI_FILE_SEEK_SHARED);

      VT_UNIMCI_CHECK_PRE(MPI_File_seek_shared,
        (fh, offset, whence, "", 0, 0), was_recorded, &time);

      result = PMPI_File_seek_shared(fh, offset, whence);

      VT_UNIMCI_CHECK_POST(MPI_File_seek_shared,
        (fh, offset, whence, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO_W_MATCHINGID(VT_IOOP_SEEK);
    }
  else
    {
      result = PMPI_File_seek_shared(fh, offset, whence);
    }

  return result;
}

#if defined(HAVE_PMPI_FILE_WRITE_ORDERED) && HAVE_PMPI_FILE_WRITE_ORDERED

/* -- MPI_File_write_ordered -- */

VT_MPI_INT MPI_File_write_ordered(MPI_File fh, CONST void* buf, VT_MPI_INT count,
                                  MPI_Datatype datatype, MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID_STATUS(VT__MPI_FILE_WRITE_ORDERED);

      VT_UNIMCI_CHECK_PRE(MPI_File_write_ordered,
        (fh, buf, count, datatype, status, "", 0, 0), was_recorded, &time);

      result = PMPI_File_write_ordered(fh, buf, count, datatype, status);

      VT_UNIMCI_CHECK_POST(MPI_File_write_ordered,
        (fh, buf, count, datatype, status, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO_W_MATCHINGID_STATUS(VT_IOOP_WRITE | VT_IOFLAG_COLL);
    }
  else
    {
      result = PMPI_File_write_ordered(fh, buf, count, datatype, status);
    }

  return result;
}

#endif /* HAVE_PMPI_FILE_WRITE_ORDERED */

/* -- MPI_File_write_shared -- */

VT_MPI_INT MPI_File_write_shared(MPI_File fh, CONST void* buf, VT_MPI_INT count,
                                 MPI_Datatype datatype, MPI_Status* status)
{
  VT_MPI_INT result;
  uint32_t tid;

  GET_THREAD_ID(tid);

  if (IS_MPI_TRACE_ON(tid))
    {
      MPIIO_ENTER_IO_W_MATCHINGID_STATUS(VT__MPI_FILE_WRITE_SHARED);

      VT_UNIMCI_CHECK_PRE(MPI_File_write_shared,
        (fh, buf, count, datatype, status, "", 0, 0), was_recorded, &time);

      result = PMPI_File_write_shared(fh, buf, count, datatype, status);

      VT_UNIMCI_CHECK_POST(MPI_File_write_shared,
        (fh, buf, count, datatype, status, "", 0, 0), was_recorded, &time);

      MPIIO_LEAVE_IO_W_MATCHINGID_STATUS(VT_IOOP_WRITE);
    }
  else
    {
      result = PMPI_File_write_shared(fh, buf, count, datatype, status);
    }

  return result;
}

#undef MPIIO_ENTER_IO_SPLITCOLL
#undef MPIIO_ENTER_IO_W_MATCHINGID
#undef MPIIO_ENTER_IO_W_MATCHINGID_STATUS
#undef MPIIO_ENTER_IO_W_STATUS
#undef MPIIO_LEAVE_IO
#undef MPIIO_LEAVE_IO_SPLITCOLL
#undef MPIIO_LEAVE_IO_W_MATCHINGID
#undef MPIIO_LEAVE_IO_W_MATCHINGID_STATUS
#undef MPIIO_LEAVE_IO_W_REQ

#endif /* HAVE_MPI2_IO */

/* include generated wrapper functions */
#include "vt_mpiwrap.gen.c"
