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

#ifndef _VT_THRD_H
#define _VT_THRD_H

#ifdef __cplusplus
#   define EXTERN extern "C"
#else
#   define EXTERN extern
#endif

#include "config.h"

#include "vt_defs.h"
#include "vt_error.h"
#include "vt_inttypes.h"
#include "vt_otf_gen.h"

#if defined(VT_JAVA)
# include "vt_java.h"
#endif /* VT_JAVA */

#if defined(VT_RUSAGE)
# include "vt_rusage.h"
#endif /* VT_RUSAGE */


#include "rfg.h"

#if (defined(VT_MT) || defined(VT_HYB))
# define VT_MY_THREAD_IS_ALIVE VTThrd_isAlive()
# define VT_MY_THREAD          VTThrd_getThreadId()
# define VT_CHECK_THREAD       VTThrd_registerThread(0)
#elif defined(VT_JAVA)
# define VT_MY_THREAD_IS_ALIVE VTThrd_isAlive()
# define VT_MY_THREAD          VTThrd_getThreadId()
# define VT_CHECK_THREAD
#else
# define VT_MY_THREAD_IS_ALIVE 1
# define VT_MY_THREAD          0
# define VT_CHECK_THREAD
#endif



/** Maximum number of threads to be created */
EXTERN uint32_t VTThrdMaxNum;

/** VTThrd struct holds all thread-specific data:
 * - Trace buffer and file including file name
 * - Event sets and value vector
 * - ...
 *-----------------------------------------------------------------------------
 */
typedef struct
{
  VTGen* gen;                       /**< trace file and buffer */

  char name[512];                   /**< thread name */
  char name_suffix[128];            /**< suffix of thread name */

  int stack_level;                  /**< current call stack level */
  int stack_level_at_off;           /**< call stack level at trace off */
  int stack_level_at_rewind_mark;   /**< call stack level at rewind mark */

  int8_t trace_status;              /**< trace status:
                                      VT_TRACE_ON,
                                      VT_TRACE_OFF, or
                                      VT_TRACE_OFF_PERMANENT */

  uint32_t tid;                     /**< associated thread id */
  uint32_t parent_tid;              /**< parent thread id */
  uint32_t child_num;               /**< number of child threads */

  uint8_t  is_virtual;              /**< flag: is virtual thread? (e.g. GPU) */

#if !defined(VT_DISABLE_RFG)

  RFG_Regions* rfg_regions;         /**< RFG regions object */
  int stack_level_at_recfilt_enabled; /**< call stack level at recursive
                                           filtering enabled */

#endif /* VT_DISABLE_RFG */

#if (defined (VT_MPI) || defined (VT_HYB))

  uint8_t mpi_tracing_enabled;       /**< actual mode of MPI tracing
                                          operation */
  uint64_t mpicoll_next_matchingid;  /**< matching id counter for MPI collective
                                          operations */

#endif /* VT_MPI || VT_HYB */

#if defined(VT_IOWRAP)

  uint8_t io_tracing_state;         /**< save value of enabled flag during
                                         suspend */
  uint8_t io_tracing_suspend_cnt;   /**< save how often suspend was called */
  uint8_t io_tracing_enabled;       /**< actual mode of I/O tracing operation */

#endif /* VT_IOWRAP */

#if (defined (VT_IOWRAP) || (defined(HAVE_MPI2_IO) && HAVE_MPI2_IO))

  uint64_t io_next_matchingid;      /**< matching id counter for I/O
                                         operations */
  uint64_t io_next_handle;          /**< handle id counter for I/O operations */

#endif

#if defined(VT_EXECWRAP)

  uint8_t exec_tracing_state;       /**< save value of enabled flag during
                                         suspend */
  uint8_t exec_tracing_suspend_cnt; /**< save how often suspend was called */
  uint8_t exec_tracing_enabled;     /**< actual mode of EXEC tracing
                                         operation */

#endif /* VT_EXECWRAP */

#if defined(VT_MALLOCWRAP)

  uint8_t  malloc_tracing_state;       /**< save value of enabled flag during
                                            suspend */
  uint8_t  malloc_tracing_suspend_cnt; /**< save how often suspend was called */
  uint8_t  malloc_tracing_enabled;     /**< actual mode of memory allocation
                                            tracing operation */
  uint64_t malloc_tracing_counter_val; /**< memory allocation counter value */

#endif /* VT_MALLOCWRAP */

#if defined(VT_GETCPU)

  uint32_t cpuid_val;               /**< cpu id counter value */

#endif /* VT_GETCPU */

#if defined(VT_RUSAGE)

  uint64_t          ru_next_read;   /**< next timestamp for reading rusage
                                         counters */
  uint64_t*         ru_valv;        /**< vector of rusage values */
  struct vt_rusage* ru_obj;         /**< rusage object */

#endif /* VT_RUSAGE */

#if defined(VT_METR)

  uint64_t*       offv;             /**< vector of counter offsets */
  uint64_t*       valv;             /**< vector of counter values */
  struct vt_metv* metv;             /**< vector of metric objects
                                         (i.e.the event sets) */

#endif /* VT_METR */


#if defined(VT_PLUGIN_CNTR)

  void* plugin_cntr_defines;               /**< plugin cntr handle */

  uint8_t plugin_cntr_writing_post_mortem; /**< flag: writing post mortem
                                                counter? */

#endif /* VT_PLUGIN_CNTR */

} VTThrd;

/* Accessor macros */

#define VTTHRD_MY_VTTHRD            (VTThrdv[VT_MY_THREAD])

/* flag: is tracing enabled? */
#define VTTHRD_TRACE_STATUS(thrd)   (thrd->trace_status)

/* trace file and buffer */
#define VTTHRD_GEN(thrd)            (thrd->gen)

/* prefix of thread's name */
#define VTTHRD_NAME_PREFIX(thrd)    (thrd->name_prefix)

/* suffix of thread's name */
#define VTTHRD_NAME_SUFFIX(thrd)    (thrd->name_suffix)

/* external name of thread */
#define VTTHRD_NAME_EXTERNAL(thrd)  (thrd->name_extern)

/* parent thread id */
#define VTTHRD_PARENT_TID(thrd)     (thrd->parent_tid)

/* number of child threads */
#define VTTHRD_CHILD_NUM(thrd)      (thrd->child_num)

/* flag: is virtual thread? */
#define VTTHRD_IS_VIRTUAL(thrd)     (thrd->is_virtual)

/* current call stack level */
#define VTTHRD_STACK_LEVEL(thrd)    (thrd->stack_level)

/* call stack level at trace off */
#define VTTHRD_STACK_LEVEL_AT_OFF(thrd) \
                                    (thrd->stack_level_at_off)

/* call stack level at rewind mark */
#define VTTHRD_STACK_LEVEL_AT_REWIND_MARK(thrd) \
                                    (thrd->stack_level_at_rewind_mark)

/* push the call stack */
#define VTTHRD_STACK_PUSH(thrd)     (thrd->stack_level)++

/* pop the call stack */
#define VTTHRD_STACK_POP(thrd)      if(--(thrd->stack_level) < 0) \
                                      vt_error_msg("Stack underflow")

#if !defined(VT_DISABLE_RFG)

/* RFG regions object */
#define VTTHRD_RFGREGIONS(thrd)     (thrd->rfg_regions)

/* flag: recursive filtering currently enabled? */
#define VTTHRD_RECFILT_ENABLED(thrd) \
                                    (thrd->stack_level_at_recfilt_enabled > -1)

/* call stack level at recursive filtering enabled */
#define VTTHRD_STACK_LEVEL_AT_RECFILT_ENABLED(thrd) \
                                    (thrd->stack_level_at_recfilt_enabled)

#endif /* VT_DISABLE_RFG */

#if (defined (VT_MPI) || defined (VT_HYB))

/* actual mode of MPI tracing operation */
#define VTTHRD_MPI_TRACING_ENABLED(thrd) \
                                    (thrd->mpi_tracing_enabled)

/* increment matching id counter for MPI collective operations */
#define VTTHRD_MPICOLLOP_NEXT_MATCHINGID(thrd) \
                                    (thrd->mpicoll_next_matchingid++)

#endif /* VT_MPI || VT_HYB */

#if (defined (VT_IOWRAP))

/* save value of enabled flag during suspend */
#define VTTHRD_IO_TRACING_STATE(thrd) \
                                    (thrd->io_tracing_state)

/* save how often suspend was called */
#define VTTHRD_IO_TRACING_SUSPEND_CNT(thrd) \
                                    (thrd->io_tracing_suspend_cnt)

/* actual mode of I/O tracing operation */
#define VTTHRD_IO_TRACING_ENABLED(thrd) \
                                    (thrd->io_tracing_enabled)

#endif /* VT_IOWRAP */

#if (defined (VT_IOWRAP) || (defined(HAVE_MPI2_IO) && HAVE_MPI2_IO))

/* increment matching id counter for I/O operations */
#define VTTHRD_IO_NEXT_MATCHINGID(thrd) \
                                    (thrd->io_next_matchingid++)

/* increment handle id counter for I/O operations */
#define VTTHRD_IO_NEXT_HANDLE(thrd) \
                                    (thrd->io_next_handle++)

#endif /* VT_IOWRAP || (HAVE_MPI2_IO && HAVE_MPI2_IO) */

#if (defined (VT_EXECWRAP))

/* save value of enabled flag during suspend */
#define VTTHRD_EXEC_TRACING_STATE(thrd) \
                                    (thrd->exec_tracing_state)

/* save how often suspend was called */
#define VTTHRD_EXEC_TRACING_SUSPEND_CNT(thrd) \
                                    (thrd->exec_tracing_suspend_cnt)

/* actual mode of EXEC tracing operation */
#define VTTHRD_EXEC_TRACING_ENABLED(thrd) \
                                    (thrd->exec_tracing_enabled)

#endif /* VT_EXECWRAP */

#if (defined (VT_MALLOCWRAP))

/* save value of enabled flag during suspend */
#define VTTHRD_MALLOC_TRACING_STATE(thrd) \
                                    (thrd->malloc_tracing_state)

/* save how often suspend was called */
#define VTTHRD_MALLOC_TRACING_SUSPEND_CNT(thrd) \
                                    (thrd->malloc_tracing_suspend_cnt)

/* actual mode of memory allocation tracing operation */
#define VTTHRD_MALLOC_TRACING_ENABLED(thrd) \
                                    (thrd->malloc_tracing_enabled)

/* memory allocation counter value */
#define VTTHRD_MALLOC_TRACING_COUNTER_VAL(thrd) \
                                    (thrd->malloc_tracing_counter_val)

#endif /* VT_MALLOCWRAP */

#if (defined (VT_GETCPU))

/* cpu id counter value */
#define VTTHRD_CPUID_VAL(thrd)      (thrd->cpuid_val)

#endif /* VT_GETCPU */

#if (defined (VT_RUSAGE))

/* next timestamp for reading rusage counters */
#define VTTHRD_RU_NEXT_READ(thrd)   (thrd->ru_next_read)

/* rusage values */
#define VTTHRD_RU_VALV(thrd)        (thrd->ru_valv)

/* rusage object */
#define VTTHRD_RU_OBJ(thrd)         (thrd->ru_obj)

#endif /* VT_RUSAGE */

#if (defined (VT_METR))

/* vector of metric offsets */
#define VTTHRD_OFFV(thrd)           (thrd->offv)

/* vector of metric values */
#define VTTHRD_VALV(thrd)           (thrd->valv)

/* vector of metric objects (i.e., event sets) */
#define VTTHRD_METV(thrd)           (thrd->metv)

#endif /* VT_METR */

#if defined(VT_PLUGIN_CNTR)

/* plugin cntr handle */
#define VTTHRD_PLUGIN_CNTR_DEFINES(thrd) \
                                    (thrd->plugin_cntr_defines)

/* flag: writing post mortem counter? */
#define VTTHRD_PLUGIN_CNTR_WRITING_POST_MORTEM(thrd) \
                                    (thrd->plugin_cntr_writing_post_mortem)

#endif /* VT_PLUGIN_CNTR */


/**
 * Initialize thread object management.
 */
EXTERN void VTThrd_init( void );

/**
 * Finalize thread object management.
 */
EXTERN void VTThrd_finalize( void );

/**
 * Creates a new thread object.
 *
 * @param tname       thread name (optional)
 * @param ptid        the ID of the parent thread/process
 * @param is_virtual  flag: is the thread a virtual thread? (e.g. GPU)
 *
 * @return            thread ID associated with the new thread object
 */
EXTERN uint32_t VTThrd_create(const char* tname, uint32_t ptid,
                              uint8_t is_virtual);

/**
 * Free thread object.
 *
 * @param thrd  thread object
 * @param tid   thread ID
 */
EXTERN void VTThrd_delete(VTThrd* thrd, uint32_t tid);

/**
 * Destroy thread object.
 *
 * @param thrd  thread object
 * @param tid   thread ID
 */
EXTERN void VTThrd_destroy(VTThrd* thrd, uint32_t tid);

/**
 * Open associated trace file.
 *
 * @param tid  thread ID
 */
EXTERN void VTThrd_open(uint32_t tid);

/**
 * Close associated trace file.
 *
 * @param thrd  pointer to the thread structure
 */
EXTERN void VTThrd_close(VTThrd* thrd);

#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))

/* macros for (un)locking predefined mutexes */
#define VTTHRD_LOCK_ENV() VTThrd_lock(&VTThrdMutexEnv)
#define VTTHRD_UNLOCK_ENV() VTThrd_unlock(&VTThrdMutexEnv)
#define VTTHRD_LOCK_IDS() VTThrd_lock(&VTThrdMutexIds)
#define VTTHRD_UNLOCK_IDS() VTThrd_unlock(&VTThrdMutexIds)

typedef struct VTThrdMutex_struct VTThrdMutex;

#if !defined(VT_JAVA)
# if defined(VT_THRD_PTHREAD)
    EXTERN void VTThrd_initPthread(void);
# elif defined(VT_THRD_OMP)
    EXTERN void VTThrd_initOmp(void);
# endif /* VT_THRD_[PTHREAD|OMP] */
  EXTERN void VTThrd_registerThread( uint32_t ptid );
#else /* VT_JAVA */
  EXTERN void VTThrd_initJava(void);
  EXTERN void VTThrd_registerThread(jthread thread, const char* tname);
#endif /* VT_JAVA */

/**
 * Check whether current thread is alive.
 *
 * @return  1 if alive, otherwise 0
 */
EXTERN uint8_t VTThrd_isAlive(void);

/**
 * Get ID of current thread.
 *
 * @return  thread ID
 */
EXTERN uint32_t VTThrd_getThreadId(void);

/**
 * Create a mutex for locking (*mutex must be NULL).
 *
 * @param  mutex the generic VampirTrace thread mutex
 */
EXTERN void VTThrd_createMutex(VTThrdMutex** mutex);

/**
 * Delete a mutex for locking.
 *
 * @param  mutex the generic VampirTrace thread mutex
 */
EXTERN void VTThrd_deleteMutex(VTThrdMutex** mutex);

/**
 * Lock a mutex (*mutex will be initialized, if NULL).
 *
 * @param  mutex the generic VampirTrace thread mutex
 */
EXTERN void VTThrd_lock(VTThrdMutex** mutex);

/**
 * Unlock a mutex.
 *
 * @param  mutex the generic VampirTrace thread mutex
 */
EXTERN void VTThrd_unlock(VTThrdMutex** mutex);

/* predefined mutexes for locking ... */
EXTERN VTThrdMutex* VTThrdMutexEnv;  /* ... VT Thread environment */
EXTERN VTThrdMutex* VTThrdMutexIds;  /* ... VT IDs */

#endif /* VT_MT || VT_HYB || VT_JAVA */

/** vector of the thread objects */
EXTERN VTThrd** VTThrdv;

/** number of thread objects */
EXTERN uint32_t VTThrdn;

#endif /* _VT_THRD_H */
