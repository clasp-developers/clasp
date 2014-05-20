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

#ifndef _VT_TRC_H
#define _VT_TRC_H

#ifdef __cplusplus
#   define EXTERN extern "C"
#else
#   define EXTERN extern
#endif

#include "vt_inttypes.h"

#include <stdlib.h>

/* id indices for internal regions */
#define VT__TRC_USER       0
#define VT__TRC_SYNC       1
#define VT__TRC_SYNCTIME   2
#define VT__TRC_FLUSH      3
#define VT__TRC_STAT       4
#define VT__TRC_OFF        5
#define VT__TRC_REWIND     6
#define VT__TRC_OMPPREG    7
#define VT__TRC_REGID_NUM  8

/* id indices for internal markers */
#define VT__TRC_MARKER_ERROR    0
#define VT__TRC_MARKER_WARNING  1
#define VT__TRC_MARKER_HINT     2

/**
 * TODO: Description
 */
EXTERN void vt_open(void);

/**
 * TODO: Description
 */
EXTERN void vt_reset(void);
/**
 * TODO: Description
 *
 * @param signum  signal number
 */
EXTERN void vt_close_by_signal(int signum);

/**
 * TODO: Description
 */
EXTERN void vt_close(void);

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param mark  flag: mark trace status as function enter/exit?
 *              (0 - no, 1 - yes)
 */
EXTERN void vt_trace_on(uint32_t tid, uint8_t mark);

/**
 * TODO: Description
 *
 * @param tid        thread id
 * @param mark       flag: mark trace status as function enter/exit?
 *                   (0 - no, 1 - yes)
 * @param permanent  flag: trace switched off permanently?
 *                   (e.g. if max. buffer flushes reached)
 *                   (0 - no, 1 - yes)
 */
EXTERN void vt_trace_off(uint32_t tid, uint8_t mark, uint8_t permanent);

/**
 * TODO: Description
 *
 * @param tid  thread id
 *
 * @return     current trace status
 *             (0 - off, 1 - on)
 */
EXTERN uint8_t vt_is_trace_on(uint32_t tid);

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp (optional)
 * @param size  buffer size to be guaranteed
 */
EXTERN void vt_guarantee_buffer(uint32_t tid, uint64_t* time, size_t size);

/**
 * TODO: Description
 *
 * @param tid  thread id
 */
EXTERN void vt_buffer_flush(uint32_t tid);

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_update_counter(uint32_t tid, uint64_t* time);

/**
 * TODO: Description
 * This function have to be called immediately after initializing the
 * communication middle-ware, e.g. atfer MPI_Init().
 */
EXTERN void vt_mpi_init(uint8_t multithreaded);

/**
 * TODO: Description
 * This function have to be called immediately before finalizing the
 * communication middle-ware, e.g. before MPI_Finalize().
 */
EXTERN void vt_mpi_finalize(void);

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param comm  MPI communicator
 */
EXTERN void vt_mpi_sync(uint32_t tid, uint64_t* time, void* comm);

/**
 * Retrieve and increment current id counter.
 *
 * @return  new id
 */
EXTERN uint32_t vt_get_curid(void);

/*
 *-----------------------------------------------------------------------------
 * Definition records
 *-----------------------------------------------------------------------------
 */

/**
 * TODO: Description
 *
 * @param tid  thread id
 * @param fmt  comment as format string like printf
 */
EXTERN void vt_def_comment(uint32_t tid, const char* fmt, ...);

/**
 * TODO: Description
 *
 * @param tid    thread id
 * @param fname  source file name
 *
 * @return       source file id
 */
EXTERN uint32_t vt_def_scl_file(uint32_t tid, const char* fname);

/**
 * TODO: Description
 *
 * @param tid    thread id
 * @param fid    source file id (created by vt_def_scl_file)
 * @param begln  begin line number
 * @param endln  end line number
 * @param fname  source file name
 *
 * @return       source code location id
 */
EXTERN uint32_t vt_def_scl(uint32_t tid, uint32_t fid, uint32_t begln,
                           uint32_t endln);

/**
 * TODO: Description
 *
 * @param tid    thread id
 * @param gname  file group name
 *
 * @return       file group id
 */
EXTERN uint32_t vt_def_file_group(uint32_t tid, const char* gname);

/**
 * TODO: Description
 *
 * @param tid    thread id
 * @param fname  file name
 * @param gid    file group id (created by vt_def_file_group)
 *
 * @return       file id
 */
EXTERN uint32_t vt_def_file(uint32_t tid, const char* fname, uint32_t gid);

/**
 * TODO: Description
 *
 * @param tid    thread id
 * @param gname  region group name
 *
 * @return       region group id
 */
EXTERN uint32_t vt_def_region_group(uint32_t tid, const char* gname);

/**
 * TODO: Description
 *
 * @param tid     thread id
 * @param rname   region name
 * @param fid     source file id (created by vt_def_scl_file)
 * @param begln   begin line number
 * @param endln   end line number
 * @param rgroup  region group name
 * @param rtype   region type
 *
 * @return        region id
 */
EXTERN uint32_t vt_def_region(uint32_t tid, const char* rname, uint32_t fid,
                              uint32_t begln, uint32_t endln,
                              const char* rgroup, uint8_t rtype);

/**
 * TODO: Description
 *
 * @param tid    thread id
 * @param gname  counter group name
 *
 * @return       counter group id
 */
EXTERN uint32_t vt_def_counter_group(uint32_t tid, const char* gname);

/**
 * TODO: Description
 *
 * @param tid    thread id
 * @param cname  counter name
 * @param cunit  counter unit
 * @param cprop  counter properties bitmask
 * @param cgid   counter group id (created by vt_def_counter_group)
 * @param pgid   process group id (created by vt_def_procgrp if group counter,
 *                                 otherwise 0)
 *
 * @return       counter id
 */
EXTERN uint32_t vt_def_counter(uint32_t tid, const char* cname,
                               const char* cunit, uint32_t cprop, uint32_t gid,
                               uint32_t pgid);

/**
 * TODO: Description
 *
 * @param tid    thread id
 * @param gname  process group name
 * @param grpc   number of entries in @grpv array
 * @param grpv   array of member process/thread ids
 * @param gid    previous created process group id (if 0, create a new one)
 *
 * @return       process group id
 */
EXTERN uint32_t vt_def_procgrp(uint32_t tid, const char* gname, uint32_t gattr,
                               uint32_t grpc, uint32_t grpv[], uint32_t gid);

/**
 * TODO: Description
 *
 * @param tid    thread id
 * @param gid    process group id (created by vt_def_procgrp)
 * @param gattr  process group attributes bitmask
 */
EXTERN void vt_def_procgrp_attributes(uint32_t tid, uint32_t gid,
                                      uint32_t gattr);

/**
 * TODO: Description
 *
 * @param tid    thread id
 * @param mname  marker name
 * @param mtype  marker type
 *
 * @return       marker id
 */
EXTERN uint32_t vt_def_marker(uint32_t tid, const char* mname,
                              uint32_t mtype);

/**
 * TODO: Description
 *
 * @param tid    thread id
 * @param ctype  MPI communicator type
 *               (VT_MPI_COMM_WORLD, VT_MPI_COMM_SELF, or VT_MPI_COMM_OTHER)
 * @param grpc   number of members
 * @param grpv   bit-vector of members
 *
 * @return       MPI communicator id
 */
EXTERN uint32_t vt_def_mpi_comm(uint32_t tid, uint8_t ctype, uint32_t grpc,
                                uint8_t grpv[]);

/**
 * TODO: Description
 *
 * @param tid    thread id
 * @param cname  communicator name
 *
 * @return       User communicator id
 */
EXTERN uint32_t vt_def_user_comm(uint32_t tid, const char* cname);

/**
 * TODO: Description
 *
 * @param tid    thread id
 * @param vtype  value type
 * @param kname  key name
 *
 * @return       key id
 */
EXTERN uint32_t vt_def_keyval(uint32_t tid, uint8_t vtype, const char* kname);

/**
 * TODO: Description
 *
 * @param tid    thread id
 * @param sname  unique async. source name
 *
 * @return       async. source key id
 */
EXTERN uint32_t vt_def_async_source(uint32_t tid, const char* sname);

/*
 *-----------------------------------------------------------------------------
 * Event records
 *-----------------------------------------------------------------------------
 */

/* -- Region -- */

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param rid   region id (created by vt_def_region)
 *
 * @return      flag: enter was recorded?
 *              (0 - no, 1 - yes)
 */
EXTERN uint8_t vt_enter(uint32_t tid, uint64_t* time, uint32_t rid);

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_exit(uint32_t tid, uint64_t* time);

/* -- File I/O -- */

/**
 * DEPRECATED
 * TODO: Description
 *
 * @param tid    thread id
 * @param time   begin timestamp
 * @param etime  end timestamp
 * @param fid    file id (created by vt_def_file)
 * @param hid    handle id
 * @param op     file operation
 * @param bytes  read/wrote bytes
 */
EXTERN void vt_ioexit(uint32_t tid, uint64_t* time, uint64_t* etime,
                      uint32_t fid, uint64_t hid, uint32_t op, uint64_t bytes );

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param mid   matching id
 */
EXTERN void vt_iobegin( uint32_t tid, uint64_t* time, uint64_t mid );

/**
 * TODO: Description
 *
 * @param tid    thread id
 * @param time   timestamp
 * @param fid    file id (created by vt_def_file)
 * @param mid    matching id
 * @param hid    handle id
 * @param op     file operation
 * @param bytes  read/wrote bytes
 */
EXTERN void vt_ioend(uint32_t tid, uint64_t* time, uint32_t fid, uint64_t mid,
                     uint64_t hid, uint32_t op, uint64_t bytes);

/* -- Counter -- */

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param hid   counter id (created by vt_def_counter)
 * @param cval  counter value
 */
EXTERN void vt_count(uint32_t tid, uint64_t* time, uint32_t cid, uint64_t cval);

/* -- Comment -- */

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param fmt   comment as format string like printf
 */
EXTERN void vt_comment(uint32_t tid, uint64_t* time, const char* fmt, ... );

/* -- Rewind -- */

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_rewind(uint32_t tid, uint64_t* time);

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_set_rewind_mark(uint32_t tid, uint64_t* time);

/* -- Marker -- */

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param mid   marker id (created by vt_def_marker)
 * @param fmt   marker text as format string like printf
 */
EXTERN void vt_marker(uint32_t tid, uint64_t* time, uint32_t mid,
                      const char* fmt, ...);

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param fmt   marker text as format string like printf
 */
EXTERN void vt_marker_error(uint32_t tid, uint64_t* time,
                            const char* fmt, ...);

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param fmt   marker text as format string like printf
 */
EXTERN void vt_marker_warning(uint32_t tid, uint64_t* time,
                              const char* fmt, ...);

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param fmt   marker text as format string like printf
 */
EXTERN void vt_marker_hint(uint32_t tid, uint64_t* time,
                           const char* fmt, ...);

/* -- Key-Value -- */

/**
 * TODO: Description
 *
 * @param tid    thread id
 * @param time   timestamp
 * @param kid    key id
 * @param vtype  value type
 * @param kval   pointer to value
 */
EXTERN void vt_keyval(uint32_t tid, uint32_t kid, uint8_t vtype,
                      const void* kvalue);

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param kid   async. source key id
 * @param time  actual time of next async. event
 */
EXTERN void vt_next_async_time(uint32_t tid, uint32_t kid, uint64_t atime);

/* -- MPI-1 -- */

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param dpid  destination process id
 * @param cid   communicator id (created by vt_def_mpi_comm)
 * @param tag   message tag
 * @param sent  sent bytes
 */
EXTERN void vt_mpi_send(uint32_t tid, uint64_t* time, uint32_t dpid,
                        uint32_t cid, uint32_t tag, uint32_t sent);

/**
 * TODO: Description
 *
 * @param tid    thread id
 * @param time   timestamp
 * @param spid   source process id
 * @param cid    communicator id (created by vt_def_mpi_comm)
 * @param tag    message tag
 * @param recvd  received bytes
 */
EXTERN void vt_mpi_recv(uint32_t tid, uint64_t* time, uint32_t spid,
                        uint32_t cid, uint32_t tag, uint32_t recvd);


/**
 * DEPRECATED
 * TODO: Description
 *
 * @param tid    thread id
 * @param time   timestamp
 * @param etime  end timestamp
 * @param rid    region id of coll. operation (created by vt_def_region)
 * @param rpid   root process id
 * @param cid    communicator id (created by vt_def_mpi_comm)
 * @param comm   MPI communicator
 * @param sent   sent bytes
 * @param recvd  received bytes
 */
EXTERN void vt_mpi_collexit(uint32_t tid, uint64_t* time, uint64_t* etime,
                            uint32_t rid, uint32_t rpid, uint32_t cid,
                            void* comm, uint32_t sent, uint32_t recvd);

/**
 * TODO: Description
 *
 * @param tid    thread id
 * @param time   timestamp
 * @param rid    region id of coll. operation (created by vt_def_region)
 * @param mid    matching id
 * @param rpid   root process id
 * @param cid    communicator id (created by vt_def_mpi_comm)
 * @param sent   sent bytes
 * @param recvd  received bytes
 */
EXTERN void vt_mpi_collbegin(uint32_t tid, uint64_t* time, uint32_t rid,
                             uint64_t mid, uint32_t rpid, uint32_t cid,
                             uint64_t sent, uint64_t recvd);

/**
 * TODO: Description
 *
 * @param tid    thread id
 * @param time   timestamp
 * @param mid    matching id
 * @param comm   MPI communicator
 */
EXTERN void vt_mpi_collend(uint32_t tid, uint64_t* time, uint64_t mid,
                           void* comm, uint8_t was_recorded);

/* -- MPI2 - 1sided -- */

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param tpid  process id
 * @param cid   communicator id (created by vt_def_mpi_comm)
 * @param tag   message tag
 * @param sent  sent bytes
 */
EXTERN void vt_mpi_rma_put(uint32_t tid, uint64_t* time, uint32_t tpid,
                           uint32_t cid, uint32_t tag, uint64_t sent);

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param tpid  process id
 * @param cid   communicator id (created by vt_def_mpi_comm)
 * @param tag   message tag
 * @param sent  sent bytes
 */
EXTERN void vt_mpi_rma_putre(uint32_t tid, uint64_t* time, uint32_t tpid,
                             uint32_t cid, uint32_t tag, uint64_t sent);

/**
 * TODO: Description
 *
 * @param tid    thread id
 * @param time   timestamp
 * @param tpid   process id
 * @param cid    communicator id (created by vt_def_mpi_comm)
 * @param tag    message tag
 * @param recvd  received bytes
 */
EXTERN void vt_mpi_rma_get(uint32_t tid, uint64_t* time, uint32_t tpid,
                           uint32_t cid, uint32_t tag, uint64_t recvd);

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param cid   communicator id (created by vt_def_mpi_comm)
 * @param tag   message tag
 */
EXTERN void vt_mpi_rma_end(uint32_t tid, uint64_t* time, uint32_t cid,
                           uint32_t tag);

/* -- OpenMP -- */

/**
 * TODO: Description
 *
 * @param tid  thread id
 */
EXTERN void vt_omp_fork(uint32_t tid);

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param ptid  parent thread id
 */
EXTERN void vt_omp_fork2(uint32_t tid, uint32_t* ptid);

/**
 * TODO: Description
 *
 * @param tid  thread id
 */
EXTERN void vt_omp_join(uint32_t tid);

/**
 * TODO: Description
 *
 * @param tid  thread id
 */
EXTERN void vt_omp_parallel_begin(uint32_t tid);

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param ptid  parent thread id
 */
EXTERN void vt_omp_parallel_begin2(uint32_t tid, uint32_t ptid);

/**
 * TODO: Description
 *
 * @param tid  thread id
 */
EXTERN void vt_omp_parallel_end(uint32_t tid);

/* -- User Point-to-Point Communication -- */

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param cid   communicator id (created by vt_def_user_comm)
 * @param tag   message tag (has to be unique per communication pair)
 * @param sent  sent bytes
 */

EXTERN void vt_user_send(uint32_t tid, uint64_t* time, uint32_t cid,
                         uint32_t tag, uint32_t sent);

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param cid   communicator id (created by vt_def_user_comm)
 * @param tag   message tag (has to be unique per communication pair)
 * @param sent  received bytes
 */

EXTERN void vt_user_recv(uint32_t tid, uint64_t* time, uint32_t cid,
                         uint32_t tag, uint32_t recvd);

/* -- VampirTrace Internal -- */

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_enter_user(uint32_t tid, uint64_t* time);

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_exit_user(uint32_t tid, uint64_t* time);

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_enter_stat(uint32_t tid, uint64_t* time);

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_exit_stat(uint32_t tid, uint64_t* time);

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_enter_flush(uint32_t tid, uint64_t* time);

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_exit_flush(uint32_t tid, uint64_t* time);

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_enter_rewind(uint32_t tid, uint64_t* time);

/**
 * TODO: Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_exit_rewind(uint32_t tid, uint64_t* time);

/*
 *-----------------------------------------------------------------------------
 * Global variables
 *-----------------------------------------------------------------------------
 */

EXTERN int vt_num_traces; /** number of processes */
EXTERN int vt_my_trace;   /** current process id (i.e. MPI-rank) */
EXTERN int vt_my_ptrace;  /** parent process id */
EXTERN uint8_t vt_my_trace_is_master; /** 1st process on local node? */
EXTERN uint8_t vt_my_trace_is_disabled; /** process disabled? */
EXTERN uint8_t vt_my_trace_is_first_avail; /** 1st not disabled process? */

/** unique file id */
EXTERN int vt_my_funique;

/** array of indices for internal regions */
EXTERN uint32_t vt_trc_regid[VT__TRC_REGID_NUM];

/** array of indices for internal markers (error, warnings, hints) */
EXTERN uint32_t vt_trc_mid[3];

/** id of process group containing all processes */
EXTERN uint32_t vt_all_pgid;

/** node process group id */
EXTERN uint32_t vt_node_pgid;

/** counter group id for miscellaneous counters (e.g. cpu id) */
EXTERN uint32_t vt_misc_cgid;

/** flag: indicates whether VampirTrace is initialized and ready to trace */
EXTERN uint8_t vt_is_alive;

/** flag: indicates whether VampirTrace aborted by a fatal error
 * (i.e. vt_error_msg); return immediately from vt_close, if it's the case */
EXTERN uint8_t vt_failure;

/** flag: indicates whether VampirTrace shall be closed if MPI_Finalize is
          called */
EXTERN uint8_t vt_close_on_mpi_finalize;

/** start time (set during vt_open() with vt_pform_wtime() */
EXTERN uint64_t vt_start_time;

/** start time (µs after 00:00:00 UTC 1 January 1970) */
EXTERN uint64_t vt_start_time_epoch;

#endif /* _VT_TRC_H */
