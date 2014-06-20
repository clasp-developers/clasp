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

#ifndef _VT_GEN_OTF_H
#define _VT_GEN_OTF_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#include "vt_inttypes.h"

#include <stdlib.h>

/*
 *-----------------------------------------------------------------------------
 * Typedefs
 *-----------------------------------------------------------------------------
 */

/* -- Trace buffer entries -- */


/* - Types - */

typedef enum
{
  VTBUF_ENTRY_TYPE__DefinitionComment,
  VTBUF_ENTRY_TYPE__DefSclFile,
  VTBUF_ENTRY_TYPE__DefScl,
  VTBUF_ENTRY_TYPE__DefFileGroup,
  VTBUF_ENTRY_TYPE__DefFile,
  VTBUF_ENTRY_TYPE__DefFunctionGroup,
  VTBUF_ENTRY_TYPE__DefFunction,
  VTBUF_ENTRY_TYPE__DefCollectiveOperation,
  VTBUF_ENTRY_TYPE__DefCounterGroup,
  VTBUF_ENTRY_TYPE__DefCounter,
  VTBUF_ENTRY_TYPE__DefProcessGroup,
  VTBUF_ENTRY_TYPE__DefProcessGroupAttributes,
  VTBUF_ENTRY_TYPE__DefMarker,
  VTBUF_ENTRY_TYPE__DefKeyValue,
  VTBUF_ENTRY_TYPE__KeyValue,
  /* time-bound records */
  VTBUF_ENTRY_TYPE__Enter,
  VTBUF_ENTRY_TYPE__Leave,
  VTBUF_ENTRY_TYPE__FileOperation,
  VTBUF_ENTRY_TYPE__BeginFileOperation,
  VTBUF_ENTRY_TYPE__EndFileOperation,
  VTBUF_ENTRY_TYPE__Counter,
  VTBUF_ENTRY_TYPE__Comment,
  VTBUF_ENTRY_TYPE__Marker,
  VTBUF_ENTRY_TYPE__SendMsg,
  VTBUF_ENTRY_TYPE__RecvMsg,
  VTBUF_ENTRY_TYPE__RMAPut,
  VTBUF_ENTRY_TYPE__RMAPutRE,
  VTBUF_ENTRY_TYPE__RMAGet,
  VTBUF_ENTRY_TYPE__RMAEnd,
  VTBUF_ENTRY_TYPE__CollectiveOperation,
  VTBUF_ENTRY_TYPE__BeginCollectiveOperation,
  VTBUF_ENTRY_TYPE__EndCollectiveOperation,
  VTBUF_ENTRY_TYPE__FunctionSummary,
  VTBUF_ENTRY_TYPE__MessageSummary,
  VTBUF_ENTRY_TYPE__CollectiveOperationSummary,
  VTBUF_ENTRY_TYPE__FileOperationSummary
} VTBuf_EntryTypes;


/* -- Data structures -- */


/* - Base - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;
} VTBuf_Entry_Base;

/* - VTBUF_ENTRY_TYPE__DefinitionComment - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  char comment[1];
} VTBuf_Entry_DefinitionComment;

/* - VTBUF_ENTRY_TYPE__DefSclFile - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t fid;
  char     fname[1];
} VTBuf_Entry_DefSclFile;

/* - VTBUF_ENTRY_TYPE__DefScl - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t sid;
  uint32_t fid;
  uint32_t ln;
} VTBuf_Entry_DefScl;

/* - VTBUF_ENTRY_TYPE__DefFileGroup - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t gid;
  char     gname[1];
} VTBuf_Entry_DefFileGroup;

/* - VTBUF_ENTRY_TYPE__DefFile - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t fid;
  uint32_t gid;
  char     fname[1];
} VTBuf_Entry_DefFile;

/* - VTBUF_ENTRY_TYPE__DefFunctionGroup - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t rdid;
  char     rdesc[1];
} VTBuf_Entry_DefFunctionGroup;

/* - VTBUF_ENTRY_TYPE__DefFunction - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t rid;
  uint32_t rdid;
  uint32_t sid;
  char     rname[1];
} VTBuf_Entry_DefFunction;

/* - VTBUF_ENTRY_TYPE__DefCollectiveOperation - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t cid;
  uint32_t ctype;
  char     cname[1];
} VTBuf_Entry_DefCollectiveOperation;

/* - VTBUF_ENTRY_TYPE__DefCounterGroup - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t gid;
  char     gname[1];
} VTBuf_Entry_DefCounterGroup;

/* - VTBUF_ENTRY_TYPE__DefCounter - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t cid;
  uint32_t cprop;
  uint32_t gid;
  uint32_t pgid;
  char     cunit[128];
  char     cname[1];
} VTBuf_Entry_DefCounter;

/* - VTBUF_ENTRY_TYPE__DefProcessGroup - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t gid;
  char     grpn[128];
  uint32_t grpc;
  uint32_t grpv[1];
} VTBuf_Entry_DefProcessGroup;

/* - VTBUF_ENTRY_TYPE__DefProcessGroupAttributes - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t gid;
  uint32_t gattr;
} VTBuf_Entry_DefProcessGroupAttributes;

/* - VTBUF_ENTRY_TYPE__DefMarker - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t mid;
  uint32_t mtype;
  char     mname[1];
} VTBuf_Entry_DefMarker;

/* - VTBUF_ENTRY_TYPE__DefKeyValue - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t kid;
  uint8_t  vtype;
  char     kname[1];
} VTBuf_Entry_DefKeyValue;

/* - VTBUF_ENTRY_TYPE__Enter / VTBUF_ENTRY_TYPE__Leave - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint32_t rid;
  uint32_t sid;
} VTBuf_Entry_EnterLeave;

/* - VTBUF_ENTRY_TYPE__FileOperation - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint64_t etime;
  uint32_t fid;
  uint64_t hid;
  uint32_t op;
  uint32_t bytes;
  uint32_t sid;
} VTBuf_Entry_FileOperation;

/* - VTBUF_ENTRY_TYPE__BeginFileOperation - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint64_t mid;
  uint32_t sid;
} VTBuf_Entry_BeginFileOperation;

/* - VTBUF_ENTRY_TYPE__EndFileOperation - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint32_t fid;
  uint64_t mid;
  uint64_t hid;
  uint32_t op;
  uint32_t bytes;
  uint32_t sid;
} VTBuf_Entry_EndFileOperation;

/* - VTBUF_ENTRY_TYPE__Counter - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint32_t cid;
  uint64_t cval;
} VTBuf_Entry_Counter;

/* - VTBUF_ENTRY_TYPE__Comment - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  char     comment[1];
} VTBuf_Entry_Comment;

/* - VTBUF_ENTRY_TYPE__Marker - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint32_t mid;
  char     mtext[1];
} VTBuf_Entry_Marker;

/* - VTBUF_ENTRY_TYPE__KeyValue - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint32_t kid;
  uint8_t  vtype;

  union {
    char     c;
    int32_t  i32;
    uint32_t u32;
    int64_t  i64;
    uint64_t u64;
    float    f;
    double   d;
    char*    s;
  } kvalue;

} VTBuf_Entry_KeyValue;

/* - VTBUF_ENTRY_TYPE__SendMsg / VTBUF_ENTRY_TYPE__RecvMsg - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint32_t pid;
  uint32_t cid;
  uint32_t tag;
  uint32_t len;
  uint32_t sid;
} VTBuf_Entry_SendRecvMsg;

/* - VTBUF_ENTRY_TYPE__CollectiveOperation - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint64_t etime;
  uint32_t rid;
  uint32_t cid;
  uint32_t rpid;
  uint32_t sent;
  uint32_t recvd;
  uint32_t sid;
} VTBuf_Entry_CollectiveOperation;

/* - VTBUF_ENTRY_TYPE__BeginCollectiveOperation - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint32_t rid;
  uint64_t mid;
  uint32_t rpid;
  uint32_t cid;
  uint64_t sent;
  uint64_t recvd;
  uint32_t sid;
} VTBuf_Entry_BeginCollectiveOperation;

/* - VTBUF_ENTRY_TYPE__EndCollectiveOperation - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint64_t mid;
} VTBuf_Entry_EndCollectiveOperation;

/* - VTBUF_ENTRY_TYPE__RMAPut / VTBUF_ENTRY_TYPE__RMAPutRE / VTBUF_ENTRY_TYPE__RMAGet - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time; 
  uint32_t opid; 
  uint32_t tpid; 
  uint32_t cid;
  uint32_t tag; 
  uint64_t len; 
  uint32_t sid;
}VTBuf_Entry_RMAPutGet;

/* - VTBUF_ENTRY_TYPE__RMAEnd - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint32_t rpid;
  uint32_t cid;
  uint32_t tag;
  uint32_t sid;
}VTBuf_Entry_RMAEnd;

/* - VTBUF_ENTRY_TYPE__FunctionSummary - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint32_t rid;
  uint64_t cnt;
  uint64_t excl;
  uint64_t incl;
} VTBuf_Entry_FunctionSummary;

/* - VTBUF_ENTRY_TYPE__MessageSummary - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint32_t peer;
  uint32_t cid;
  uint32_t tag;
  uint64_t scnt;
  uint64_t rcnt;
  uint64_t sent;
  uint64_t recvd;
} VTBuf_Entry_MessageSummary;

/* - VTBUF_ENTRY_TYPE__CollectiveOperationSummary - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint32_t cid;
  uint32_t rid;
  uint64_t scnt;
  uint64_t rcnt;
  uint64_t sent;
  uint64_t recvd;
} VTBuf_Entry_CollectiveOperationSummary;

/* - VTBUF_ENTRY_TYPE__FileOperationSummary - */

typedef struct
{
  VTBuf_EntryTypes type;
  uint32_t length;

  uint64_t time;
  uint32_t fid;
  uint64_t nopen;
  uint64_t nclose;
  uint64_t nread;
  uint64_t nwrite;
  uint64_t nseek;
  uint64_t read;
  uint64_t wrote;
} VTBuf_Entry_FileOperationSummary;


/* -- Opaque data types -- */


/* - Trace file generated at run time - */
typedef struct VTGen_struct VTGen;

/*
 *-----------------------------------------------------------------------------
 * VTGen
 *-----------------------------------------------------------------------------
 */

EXTERN VTGen* VTGen_open(const char* tname, const char* tnamesuffix,
                         uint32_t ptid, uint32_t tid, size_t buffer_size);

EXTERN void VTGen_guarantee(VTGen* gen, uint64_t* time, size_t size);

EXTERN void VTGen_flush(VTGen* gen, uint8_t lastFlush,
                        uint64_t flushBTime, uint64_t* flushETime);

EXTERN void VTGen_close(VTGen* gen);

EXTERN void VTGen_delete(VTGen* gen);

EXTERN void VTGen_destroy(VTGen* gen);

EXTERN uint8_t VTGen_get_buflevel(VTGen* gen);


/* -- Writing trace records -- */


/* - Definition records - */

EXTERN void VTGen_write_DEFINITION_COMMENT(VTGen* gen, const char* comment);

EXTERN void VTGen_write_DEF_SCL_FILE(VTGen* gen, uint32_t fid,
                                     const char* fname);

EXTERN void VTGen_write_DEF_SCL(VTGen* gen, uint32_t sid, uint32_t fid,
                                uint32_t ln );

EXTERN void VTGen_write_DEF_FILE_GROUP(VTGen* gen, uint32_t gid,
                                       const char* gname);

EXTERN void VTGen_write_DEF_FILE(VTGen* gen, uint32_t fid, const char* fname,
                                 uint32_t gid);

EXTERN void VTGen_write_DEF_FUNCTION_GROUP(VTGen* gen, uint32_t rdid,
                                           const char* rdesc);

EXTERN void VTGen_write_DEF_FUNCTION(VTGen* gen, uint32_t rid,
                                     const char* rname, uint32_t rdid,
                                     uint32_t sid );

EXTERN void VTGen_write_DEF_COLLECTIVE_OPERATION(VTGen* gen, uint32_t cid,
                                                 const char* cname,
                                                 uint32_t ctype );

EXTERN void VTGen_write_DEF_COUNTER_GROUP(VTGen* gen, uint32_t gid,
                                          const char* gname);

EXTERN void VTGen_write_DEF_COUNTER(VTGen* gen, uint32_t cid,
                                    const char* cname, const char* cunit,
                                    uint32_t cprop, uint32_t gid,
                                    uint32_t pgid);

EXTERN void VTGen_write_DEF_PROCESS_GROUP(VTGen* gen, uint32_t gid,
                                          const char* grpn, uint32_t grpc,
                                          uint32_t grpv[]);

EXTERN void VTGen_write_DEF_PROCESS_GROUP_ATTRIBUTES(VTGen* gen, uint32_t gid,
                                                     uint32_t gattr);

EXTERN void VTGen_write_DEF_KEYVAL(VTGen* gen, uint32_t kid, uint8_t vtype,
                                   const char* kname);

/* -- Marker -- */

EXTERN void VTGen_write_DEF_MARKER(VTGen* gen, uint32_t mid, const char* mname,
                                   uint32_t mtype);


/* - Event records - */


/* -- Region -- */

EXTERN void VTGen_write_ENTER(VTGen* gen, uint64_t* time, uint32_t rid,
                              uint32_t sid);

EXTERN void VTGen_write_LEAVE(VTGen* gen, uint64_t* time, uint32_t rid,
                              uint32_t sid);

/* -- File I/O -- */

EXTERN void VTGen_write_FILE_OPERATION(VTGen* gen, uint64_t* time,
                                       uint64_t* etime, uint32_t fid,
                                       uint64_t hid, uint32_t op,
                                       uint64_t bytes, uint32_t sid);

EXTERN void VTGen_write_BEGIN_FILE_OPERATION(VTGen* gen, uint64_t* time,
                                             uint64_t mid, uint32_t sid);

EXTERN void VTGen_write_END_FILE_OPERATION(VTGen* gen, uint64_t* time,
                                           uint32_t fid, uint64_t mid,
                                           uint64_t hid, uint32_t op,
                                           uint64_t bytes, uint32_t sid);

/* -- Counter -- */

EXTERN void VTGen_write_COUNTER(VTGen* gen, uint64_t* time, uint32_t cid,
                                uint64_t cval);

/* -- Comment -- */

EXTERN void VTGen_write_COMMENT(VTGen* gen, uint64_t* time,
                                const char* comment);

/* -- Marker -- */

EXTERN void VTGen_write_MARKER(VTGen* gen, uint64_t* time, uint32_t mid,
                               const char* mtext);

/* -- Key-Value -- */

EXTERN void VTGen_write_KEYVAL(VTGen* gen, uint32_t kid, uint8_t vtype,
                               const void* kvalue);

/* -- MPI-1 -- */

EXTERN void VTGen_write_SEND_MSG(VTGen* gen, uint64_t* time, uint32_t dpid,
                                 uint32_t cid, uint32_t tag, uint32_t sent,
                                 uint32_t sid);

EXTERN void VTGen_write_RECV_MSG(VTGen* gen, uint64_t* time, uint32_t spid,
                                 uint32_t cid, uint32_t tag, uint32_t recvd,
                                 uint32_t sid);

EXTERN void VTGen_write_COLLECTIVE_OPERATION(VTGen* gen, uint64_t* time,
                                             uint64_t* etime, uint32_t rid,
                                             uint32_t cid, uint32_t rpid,
                                             uint32_t sent, uint32_t recvd,
                                             uint32_t sid);

EXTERN void VTGen_write_BEGIN_COLLECTIVE_OPERATION(VTGen* gen, uint64_t* time,
                                                   uint32_t rid, uint64_t mid,
                                                   uint32_t rpid, uint32_t cid,
                                                   uint64_t sent,
                                                   uint64_t recvd,
                                                   uint32_t sid);

EXTERN void VTGen_write_END_COLLECTIVE_OPERATION(VTGen* gen, uint64_t* time,
                                                 uint64_t mid);

/* -- MPI2 - 1sided -- */

EXTERN void VTGen_write_RMA_PUT(VTGen* gen, uint64_t* time, uint32_t opid,
                                uint32_t tpid, uint32_t cid, uint32_t tag,
                                uint32_t len, uint32_t sid);

EXTERN void VTGen_write_RMA_PUTRE(VTGen* gen, uint64_t* time, uint32_t opid,
                                  uint32_t tpid, uint32_t cid, uint32_t tag,
                                  uint64_t len, uint32_t sid);

EXTERN void VTGen_write_RMA_GET(VTGen* gen, uint64_t* time, uint32_t opid,
                                uint32_t tpid, uint32_t cid, uint32_t tag,
                                uint64_t len, uint32_t sid);

EXTERN void VTGen_write_RMA_END(VTGen* gen, uint64_t* time, uint32_t rpid,
                                uint32_t cid, uint32_t tag, uint32_t sid);

/* -- VampirTrace Internal -- */

EXTERN void VTGen_write_ENTER_FLUSH(VTGen* gen, uint64_t* time);

EXTERN void VTGen_write_LEAVE_FLUSH(VTGen* gen, uint64_t* time);

EXTERN void VTGen_write_ENTER_STAT(VTGen* gen, uint64_t* time);

EXTERN void VTGen_write_LEAVE_STAT(VTGen* gen, uint64_t* time);

/* - Summary records - */

EXTERN void VTGen_write_FUNCTION_SUMMARY(VTGen* gen, uint64_t* time,
                                         uint32_t rid, uint64_t cnt,
                                         uint64_t excl, uint64_t incl);

EXTERN void VTGen_write_MESSAGE_SUMMARY(VTGen* gen, uint64_t* time,
                                        uint32_t peer, uint32_t cid,
                                        uint32_t tag, uint64_t scnt,
                                        uint64_t rcnt, uint64_t sent,
                                        uint64_t recvd);

EXTERN void VTGen_write_COLLECTIVE_OPERATION_SUMMARY(VTGen* gen,
                                                     uint64_t* time,
                                                     uint32_t cid,
                                                     uint32_t rid,
                                                     uint64_t scnt,
                                                     uint64_t rcnt,
                                                     uint64_t sent,
                                                     uint64_t recvd);

EXTERN void VTGen_write_FILE_OPERATION_SUMMARY(VTGen* gen, uint64_t* time,
                                               uint32_t fid, uint64_t nopen,
                                               uint64_t nclose, uint64_t nread,
                                               uint64_t nwrite, uint64_t nseek,
                                               uint64_t read, uint64_t wrote);

/* - Rewind - */

EXTERN void VTGen_set_rewind_mark(VTGen* gen, uint64_t* time);

EXTERN void VTGen_rewind(VTGen* gen, uint64_t* time);

EXTERN uint8_t VTGen_is_rewind_mark_present(VTGen* gen);

#endif /* _VT_GEN_OTF_H */
