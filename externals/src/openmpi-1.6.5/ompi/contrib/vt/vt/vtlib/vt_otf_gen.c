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

#include "config.h"

#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "vt_otf_gen.h"
#include "vt_otf_sum.h"
#include "vt_defs.h"
#include "vt_env.h"
#include "vt_error.h"
#include "vt_iowrap.h"
#include "vt_inttypes.h"
#include "vt_pform.h"
#include "vt_trc.h"

#if defined(VT_IOFSL)
# include "vt_iofsl.h"
#endif /* VT_IOFSL */

#if defined(VT_PLUGIN_CNTR)
# include "vt_plugin_cntr_int.h"
#endif /* VT_PLUGIN_CNTR */

#include "otf.h"

/*
 *-----------------------------------------------------------------------------
 * Macro functions
 *-----------------------------------------------------------------------------
 */

#define VTGEN_CHECK(gen)                                            \
  if (gen == NULL) vt_error_msg("Abort: Uninitialized trace buffer")

#define VTGEN_ALLOC(gen, bytes)                                     \
  if ((uint64_t)((gen)->buf->pos - (gen)->buf->mem) >               \
      (uint64_t)((gen)->buf->size - (bytes)))                       \
    VTGen_flush((gen), 0, vt_pform_wtime(), NULL);

#define VTGEN_ALLOC_EVENT(gen, bytes, time)                         \
  if ((uint64_t)((gen)->buf->pos - (gen)->buf->mem) >               \
      (uint64_t)((gen)->buf->size - (bytes))) {                     \
    VTGen_flush((gen), 0, *(time), (time));                         \
    if((gen)->flushcntr == 0) return;                               \
  }

#define VTGEN_ALIGN_LENGTH(bytes)                                   \
  ( ( (bytes) % SIZEOF_VOIDP ) ?                                    \
    ( (bytes) / SIZEOF_VOIDP + 1 ) * SIZEOF_VOIDP : (bytes) )

#define VTGEN_JUMP(gen, bytes)                                      \
  gen->buf->pos += (bytes)

#define VTGEN_IS_TRACE_ON(gen) ((gen)->mode & VT_MODE_TRACE) != 0
#define VTGEN_IS_SUM_ON(gen) ((gen)->mode & VT_MODE_STAT) != 0
#define VTGEN_IS_SUM_PROP_ON(gen, prop) \
  (VTGEN_IS_SUM_ON((gen)) && ((gen)->sum_props & (prop)) != 0)

/*
 *-----------------------------------------------------------------------------
 * VTGen
 *-----------------------------------------------------------------------------
 */

typedef struct
{
  buffer_t  mem;
  buffer_t  pos;
  size_t    size;
} VTBuf;

typedef struct
{
  buffer_t  pos;
  uint64_t  time;
} VTRewindMark;

typedef struct
{
  uint64_t min;
  uint64_t max;
} VTTimeRange;

struct VTGen_struct
{
  OTF_FileManager*    filemanager;
  OTF_WStream*        filestream;
  OTF_FileCompression filecomp;
  char*               fileprefix;
  const char*         ldir;
  const char*         gdir;
  const char*         tname;
  const char*         tnamesuffix;
  uint32_t            ptid;
  uint32_t            tid;
  uint32_t            flushcntr;
  uint8_t             isfirstflush;
  uint8_t             hasdata;
  uint8_t             mode;
  uint8_t             sum_props;
  uint8_t             same_ldir_gdir;
  VTRewindMark        rewindmark;
  VTTimeRange         timerange;
  VTSum*              sum;
  VTBuf*              buf;
};

VTGen* VTGen_open(const char* tname, const char* tnamesuffix,
                  uint32_t ptid, uint32_t tid, size_t buffer_size)
{
  VTGen* gen;

  struct stat stat_ldir;
  struct stat stat_gdir;

  /* allocate VTGen record */
  gen = (VTGen*)calloc(1, sizeof(VTGen));
  if (gen == NULL)
    vt_error();

  /* get trace directory paths from environment variables */
  gen->ldir = vt_env_ldir();
  gen->gdir = vt_env_gdir();

  /* check write permissions */

  if (vt_env_ldir_check())
  {
    if (access(gen->ldir, W_OK) == -1)
    {
      vt_error_msg("Could not access %s for writing: %s", gen->ldir,
                   strerror(errno));
    }
  }
  if (vt_env_gdir_check())
  {
    if (access(gen->gdir, W_OK) == -1)
    {
      vt_error_msg("Could not access %s for writing: %s", gen->gdir,
                   strerror(errno));
    }
  }

  /* get the device id and inode number of the trace directories to check
     whether they're the same or not */

  if (stat(gen->ldir, &stat_ldir) == -1)
    vt_error_msg("Could not stat %s: %s", gen->ldir, strerror(errno));
  if (stat(gen->gdir, &stat_gdir) == -1)
    vt_error_msg("Could not stat %s: %s", gen->gdir, strerror(errno));

  if (stat_ldir.st_dev == stat_gdir.st_dev &&
      stat_ldir.st_ino == stat_gdir.st_ino)
  {
    gen->same_ldir_gdir = 1;
  }

#if defined(VT_IOFSL)
  /* local and global trace directory *must* be the same in IOFSL mode */
  vt_libassert( !vt_iofsl_enabled || gen->same_ldir_gdir );
#endif /* VT_IOFSL */

  /* store thread name */
  gen->tname = tname;

  /* store thread name suffix */
  gen->tnamesuffix = tnamesuffix;

  /* store parent thread id */
  gen->ptid = ptid;

  /* store thread id */
  gen->tid = tid;

  /* initialize flush counter */
  gen->flushcntr = vt_env_max_flushes();
  if( gen->flushcntr == 0 ) gen->flushcntr = (uint32_t)-1;

  /* initialize first flush flag */
  gen->isfirstflush = 1;

  /* initialize has data flag */
  gen->hasdata = 0;

  /* initialize trace mode flags */
  gen->mode = (uint8_t)vt_env_mode();

  /* initialize statistics properties */
  gen->sum_props = (uint8_t)vt_env_stat_props();

  /* allocate VTSum record */
  gen->sum = NULL;
  if (VTGEN_IS_SUM_ON(gen))
    gen->sum = VTSum_open(gen, tid);

  /* allocate buffer record */

  gen->buf = (VTBuf*)malloc(sizeof(VTBuf));
  if (gen->buf == NULL) 
    vt_error();

  /* allocate buffer */

  gen->buf->mem = malloc(buffer_size);
  if (gen->buf->mem == NULL) 
    vt_error();

  /* initialize buffer */

  gen->buf->pos  = gen->buf->mem;
  /* subtraction leaves space for size of FLUSH record */
  gen->buf->size =
    buffer_size - (2 * VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_EnterLeave)));

  /* initialize rewind mark */
  gen->rewindmark.pos = (buffer_t)-1;

  /* initialize time range */
  gen->timerange.min = (uint64_t)-1;
  gen->timerange.max = 0;

  /* return */
  return gen;
}

void VTGen_guarantee(VTGen* gen, uint64_t* time, size_t size)
{
  if (!time)
  {
    VTGEN_ALLOC(gen, VTGEN_ALIGN_LENGTH(size));
  }
  else
  {
    VTGEN_ALLOC_EVENT(gen, VTGEN_ALIGN_LENGTH(size), time);
  }
}

void VTGen_flush(VTGen* gen, uint8_t lastFlush,
                 uint64_t flushBTime, uint64_t* flushETime )
{
  uint8_t end_flush_marked = 0;
  uint32_t pid;
  buffer_t p;

  /* intermediate flush and max. buffer flushes reached? */
  if (!lastFlush && gen->flushcntr == 0) return;

  /* reset buffer, if rank is disabled */
  if (vt_my_trace_is_disabled)
  {
    gen->buf->pos = gen->buf->mem;
    return;
  }

  /* disable I/O tracing */
  VT_SUSPEND_IO_TRACING(gen->tid);

  /* mark begin of flush */
  if (!lastFlush)
  {
#if defined(VT_PLUGIN_CNTR)
    /* ... but not when writing post mortem counters */
    uint8_t flag;
    VT_PLUGIN_CNTR_WRITING_POST_MORTEM(gen->tid, flag);
    if (!flag)
#endif /* VT_PLUGIN_CNTR */
    vt_enter_flush(gen->tid, &flushBTime);
  }

  /* get process id */
  pid = VT_PROCESS_ID(vt_my_trace, gen->tid);

  if (gen->isfirstflush)
  {
    /* set base name of the temporary files (basename includes local path
       but neither thread identifier nor suffix) */

    gen->fileprefix = (char*)calloc(VT_PATH_MAX + 1, sizeof(char));
    if (gen->fileprefix == NULL)
      vt_error();

    /* if the local and global trace directory are the same (i.e. in IOFSL mode)
       set the temporary base file name to the final one to prevent
       copying/renaming before starting the unification */
    if (gen->same_ldir_gdir)
    {
      if (vt_my_funique > 0)
      {
        snprintf(gen->fileprefix, VT_PATH_MAX, "%s/%s_%u",
                 gen->gdir, vt_env_fprefix(), vt_my_funique);
      }
      else
      {
        snprintf(gen->fileprefix, VT_PATH_MAX, "%s/%s",
                 gen->gdir, vt_env_fprefix());
      }
    }
    /* otherwise, compose an unique temporary base file name by adding the
       node- and process id */
    else
    {
      snprintf(gen->fileprefix, VT_PATH_MAX, "%s/%s.%lx.%u",
               gen->ldir, vt_env_fprefix(), vt_pform_node_id(), getpid());
    }

    /* open file manager for writer stream */

    gen->filemanager = OTF_FileManager_open(4);
    if (gen->filemanager == NULL)
      vt_error_msg("OTF_FileManager_open failed:\n %s", otf_strerr);

#if defined(VT_IOFSL)
    if (vt_iofsl_enabled)
    {
       /* initialize file manager for IOFSL mode */

       OTF_IofslMode otf_iofsl_mode =
         (vt_iofsl_mode == VT_IOFSL_MODE_MULTIFILE) ?
           OTF_IOFSL_MULTIFILE : OTF_IOFSL_MULTIFILE_SPLIT;

       uint32_t otf_iofsl_flags = 0;
       if ((vt_iofsl_flags & VT_IOFSL_FLAG_ASYNC_IO) != 0)
         otf_iofsl_flags |= OTF_IOFSL_FLAG_NONBLOCKING;

       OTF_FileManager_setIofsl(gen->filemanager, vt_iofsl_servers_num,
         vt_iofsl_servers_list, otf_iofsl_mode, otf_iofsl_flags, 0,
         VT_TRACEID_BITMASK);
    }
#endif /* VT_IOFSL */

    /* open writer stream */

    gen->filestream =
      OTF_WStream_open(gen->fileprefix, VT_PROCESS_ID(vt_my_trace, gen->tid),
        gen->filemanager);
    if (gen->filestream == NULL)
      vt_error_msg("OTF_WStream_open failed:\n %s", otf_strerr);

    vt_cntl_msg(2, "Opened OTF writer stream [namestub %s id %x] for "
                "generation [buffer %llu bytes]",
                gen->fileprefix, VT_PROCESS_ID(vt_my_trace, gen->tid),
                (unsigned long long)gen->buf->size);

    /* set writer stream's buffer size */
    {
      size_t bsize = vt_env_otf_bsize();
      if (bsize > 0)
      {
        OTF_WStream_setBufferSizes(gen->filestream, bsize);
        /* no return value; check otf_errno for error */
        if (otf_errno != OTF_NO_ERROR)
        {
          vt_error_msg("OTF_WStream_setBufferSizes failed:\n %s",
                       otf_strerr);
        }
      }
    }

    /* set file compression and buffer size */

    gen->filecomp = OTF_FILECOMPRESSION_UNCOMPRESSED;
    if (vt_env_compression() &&
        (OTF_WStream_setCompression(gen->filestream,
           OTF_FILECOMPRESSION_COMPRESSED) == 1))
    {
      size_t bsize = vt_env_compression_bsize();
      gen->filecomp = OTF_FILECOMPRESSION_COMPRESSED;

      if (bsize > 0)
      {
        OTF_WStream_setZBufferSizes(gen->filestream, bsize);
        /* no return value; check otf_errno for error */
        if (otf_errno != OTF_NO_ERROR)
        {
          vt_error_msg("OTF_WStream_setZBufferSizes failed:\n %s",
                       otf_strerr);
        }
      }
    }

    if (gen->tid == 0)
    {
      char creator[100];

      /* write OTF version record */

      if (OTF_WStream_writeOtfVersion(gen->filestream) == 0)
        vt_error_msg("OTF_WStream_writeOtfVersion failed:\n %s", otf_strerr);

      /* write creator record */

      snprintf(creator, sizeof(creator) - 1, "%s", PACKAGE_STRING);
      if (OTF_WStream_writeDefCreator(gen->filestream, creator) == 0)
        vt_error_msg("OTF_WStream_writeDefCreator failed:\n %s", otf_strerr);

      /* write timer resolution record */

      if (OTF_WStream_writeDefTimerResolution(gen->filestream,
            vt_pform_clockres()) == 0)
      {
        vt_error_msg("OTF_WStream_writeDefTimerResolution failed:\n %s",
                     otf_strerr);
      }
    }

    /* write process definition record */
    {
      uint32_t parent_pid = 0;
      char pname[1024];

      if (gen->tid != 0)
        parent_pid = VT_PROCESS_ID(vt_my_trace, gen->ptid);

      snprintf(pname, sizeof(pname) - 1, "%s %d%s",
               gen->tname, vt_my_trace, gen->tnamesuffix);

      if (OTF_WStream_writeDefProcess(gen->filestream, pid, pname,
            parent_pid) == 0)
      {
        vt_error_msg("OTF_WStream_writeDefProcess failed:\n %s",
                     otf_strerr);
      }
    }

    gen->isfirstflush = 0;
  }

  /* walk through the buffer and write records */

  p = gen->buf->mem;

  while(p < gen->buf->pos)
  {
    /* time-bound record? */
    if (((VTBuf_Entry_Base*)p)->type >= VTBUF_ENTRY_TYPE__Enter)
    {
      VTBuf_Entry_EnterLeave* entry = (VTBuf_Entry_EnterLeave*)p;

      /* update time range */
      if (gen->timerange.min == (uint64_t)-1)
        gen->timerange.min = entry->time;
      gen->timerange.max = entry->time;

      /* set indicator for having events/statistics,
         if it's not a marker record */
      if (!gen->hasdata && entry->type != VTBUF_ENTRY_TYPE__Marker)
        gen->hasdata = 1;
    }

    /* write record */
    switch(((VTBuf_Entry_Base*)p)->type)
    {
      case VTBUF_ENTRY_TYPE__DefinitionComment:
      {
        VTBuf_Entry_DefinitionComment* entry =
          (VTBuf_Entry_DefinitionComment*)p;

        if (OTF_WStream_writeDefinitionComment(gen->filestream,
              entry->comment) == 0)
        {
          vt_error_msg("OTF_WStream_writeDefinitionComment failed:\n %s",
                       otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__DefSclFile:
      {
        VTBuf_Entry_DefSclFile* entry = (VTBuf_Entry_DefSclFile*)p;

        if (OTF_WStream_writeDefSclFile(gen->filestream, entry->fid,
              entry->fname) == 0)
        {
          vt_error_msg("OTF_WStream_writeDefSclFile failed:\n %s", otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__DefScl:
      {
        VTBuf_Entry_DefScl* entry = (VTBuf_Entry_DefScl*)p;

        if (OTF_WStream_writeDefScl(gen->filestream, entry->sid, entry->fid,
              entry->ln) == 0)
        {
          vt_error_msg("OTF_WStream_writeDefScl failed:\n %s", otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__DefFileGroup:
      {
        VTBuf_Entry_DefFileGroup* entry = (VTBuf_Entry_DefFileGroup*)p;

        if (OTF_WStream_writeDefFileGroup(gen->filestream, entry->gid,
              entry->gname) == 0)
        {
          vt_error_msg("OTF_WStream_writeDefFileGroup failed:\n %s",
                       otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__DefFile:
      {
        VTBuf_Entry_DefFile* entry = (VTBuf_Entry_DefFile*)p;

        if (OTF_WStream_writeDefFile(gen->filestream, entry->fid, entry->fname,
              entry->gid) == 0)
        {
          vt_error_msg("OTF_WStream_writeDefFile failed:\n %s", otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__DefFunctionGroup:
      {
        VTBuf_Entry_DefFunctionGroup* entry = (VTBuf_Entry_DefFunctionGroup*)p;

        if (OTF_WStream_writeDefFunctionGroup(gen->filestream, entry->rdid,
              entry->rdesc) == 0)
        {
          vt_error_msg("OTF_WStream_writeDefFunctionGroup failed:\n %s",
                       otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__DefFunction:
      {
        VTBuf_Entry_DefFunction* entry = (VTBuf_Entry_DefFunction*)p;

        if (OTF_WStream_writeDefFunction(gen->filestream, entry->rid,
              entry->rname, entry->rdid, entry->sid) == 0)
        {
          vt_error_msg("OTF_WStream_writeDefFunction failed:\n %s", otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__DefCollectiveOperation:
      {
        VTBuf_Entry_DefCollectiveOperation* entry =
          (VTBuf_Entry_DefCollectiveOperation*)p;

        uint32_t ctype = OTF_COLLECTIVE_TYPE_UNKNOWN;
        switch(entry->ctype)
        {
          case VT_MPI_COLL_ALL2ALL:
            ctype = OTF_COLLECTIVE_TYPE_ALL2ALL;
            break;
          case VT_MPI_COLL_ALL2ONE:
            ctype = OTF_COLLECTIVE_TYPE_ALL2ONE;
            break;
          case VT_MPI_COLL_BARRIER:
            ctype = OTF_COLLECTIVE_TYPE_BARRIER;
            break;
          case VT_MPI_COLL_ONE2ALL:
            ctype = OTF_COLLECTIVE_TYPE_ONE2ALL;
            break;
          default:
            vt_libassert(0);
        }

        if (OTF_WStream_writeDefCollectiveOperation(gen->filestream, entry->cid,
              entry->cname, ctype) == 0)
        {
          vt_error_msg("OTF_WStream_writeDefCollectiveOperation failed:\n %s",
                       otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__DefCounterGroup:
      {
        VTBuf_Entry_DefCounterGroup* entry = (VTBuf_Entry_DefCounterGroup*)p;

        if (OTF_WStream_writeDefCounterGroup(gen->filestream, entry->gid,
              entry->gname) == 0)
        {
          vt_error_msg("OTF_WStream_writeDefCounterGroup failed:\n %s",
                       otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__DefCounter:
      {
        VTBuf_Entry_DefCounter* entry = (VTBuf_Entry_DefCounter*)p;

        uint32_t cprop = 0;
        if ((entry->cprop & VT_CNTR_ACC) != 0)
          cprop |= OTF_COUNTER_TYPE_ACC;
        if ((entry->cprop & VT_CNTR_ABS) != 0)
          cprop |= OTF_COUNTER_TYPE_ABS;
        if ((entry->cprop & VT_CNTR_START) != 0)
          cprop |= OTF_COUNTER_SCOPE_START;
        if ((entry->cprop & VT_CNTR_POINT) != 0)
          cprop |= OTF_COUNTER_SCOPE_POINT;
        if ((entry->cprop & VT_CNTR_LAST) != 0)
          cprop |= OTF_COUNTER_SCOPE_LAST;
        if ((entry->cprop & VT_CNTR_NEXT) != 0)
          cprop |= OTF_COUNTER_SCOPE_NEXT;
        if ((entry->cprop & VT_CNTR_SIGNED) != 0)
          cprop |= OTF_COUNTER_VARTYPE_SIGNED8;
        if ((entry->cprop & VT_CNTR_UNSIGNED) != 0)
          cprop |= OTF_COUNTER_VARTYPE_UNSIGNED8;
        if ((entry->cprop & VT_CNTR_FLOAT) != 0)
          cprop |= OTF_COUNTER_VARTYPE_FLOAT;
        if ((entry->cprop & VT_CNTR_DOUBLE) != 0)
          cprop |= OTF_COUNTER_VARTYPE_DOUBLE;

        if (OTF_WStream_writeDefCounter(gen->filestream, entry->cid,
              entry->cname, cprop, entry->gid, entry->cunit) == 0)
        {
          vt_error_msg("OTF_WStream_writeDefCounter failed:\n %s", otf_strerr);
        }

        if (entry->pgid != 0)
        {
          if (OTF_WStream_writeDefCounterAssignments(gen->filestream,
                entry->cid, 1, &(entry->pgid), NULL) == 0)
          {
            vt_error_msg("OTF_WStream_writeDefCounterAssignments failed:\n %s",
                         otf_strerr);
          }
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__DefProcessGroup:
      {
        VTBuf_Entry_DefProcessGroup* entry = (VTBuf_Entry_DefProcessGroup*)p;

        if (OTF_WStream_writeDefProcessGroup(gen->filestream, entry->gid,
              entry->grpn, entry->grpc, entry->grpv) == 0)
        {
          vt_error_msg("OTF_WStream_writeDefProcessGroup failed:\n %s",
                       otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__DefProcessGroupAttributes:
      {
        VTBuf_Entry_DefProcessGroupAttributes* entry =
          (VTBuf_Entry_DefProcessGroupAttributes*)p;

        uint32_t gattr = 0;
        if ((entry->gattr & VT_PROCGRP_ISCOMMUNICATOR) != 0)
          gattr |= (1<<OTF_ATTR_IsCommunicator);
        if ((entry->gattr & VT_PROCGRP_HASCOUNTERS) != 0)
          gattr |= (1<<OTF_ATTR_hasGroupCounters);

        if (OTF_WStream_writeDefProcessOrGroupAttributes(gen->filestream,
              entry->gid, gattr) == 0)
        {
          vt_error_msg("OTF_WStream_writeDefProcessOrGroupAttributes failed:\n %s",
                       otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__DefMarker:
      {
        VTBuf_Entry_DefMarker* entry = (VTBuf_Entry_DefMarker*)p;

        uint32_t mtype = OTF_MARKER_TYPE_UNKNOWN;
        switch(entry->mtype)
        {
          case VT_MARKER_ERROR:
            mtype = OTF_MARKER_TYPE_ERROR;
            break;
          case VT_MARKER_WARNING:
            mtype = OTF_MARKER_TYPE_WARNING;
            break;
          case VT_MARKER_HINT:
            mtype = OTF_MARKER_TYPE_HINT;
            break;
          default:
            vt_libassert(0);
        }

        if (OTF_WStream_writeDefMarker(gen->filestream, entry->mid,
              entry->mname, mtype) == 0)
        {
          vt_error_msg("OTF_WStream_writeDefMarker failed:\n %s", otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__DefKeyValue:
      {
        VTBuf_Entry_DefKeyValue* entry = (VTBuf_Entry_DefKeyValue*)p;

        OTF_Type vtype = OTF_UNKNOWN;
        switch(entry->vtype)
        {
          case VT_KEYVAL_TYPE_CHAR:
            vtype = OTF_CHAR;
            break;
          case VT_KEYVAL_TYPE_INT32:
            vtype = OTF_INT32;
            break;
          case VT_KEYVAL_TYPE_UINT32:
            vtype = OTF_UINT32;
            break;
          case VT_KEYVAL_TYPE_INT64:
            vtype = OTF_INT64;
            break;
          case VT_KEYVAL_TYPE_UINT64:
            vtype = OTF_UINT64;
            break;
          case VT_KEYVAL_TYPE_FLOAT:
            vtype = OTF_FLOAT;
            break;
          case VT_KEYVAL_TYPE_DOUBLE:
            vtype = OTF_DOUBLE;
            break;
          case VT_KEYVAL_TYPE_STRING:
            vtype = OTF_BYTE_ARRAY;
            break;
          default:
            vt_libassert(0);
        }

        if (OTF_WStream_writeDefKeyValue(gen->filestream, entry->kid, vtype,
              entry->kname, NULL) == 0)
        {
          vt_error_msg("OTF_WStream_writeDefKeyValue failed:\n %s", otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__Enter:
      {
        VTBuf_Entry_EnterLeave* entry = (VTBuf_Entry_EnterLeave*)p;

        if (OTF_WStream_writeEnter(gen->filestream, entry->time, entry->rid,
              pid, entry->sid) == 0)
        {
          vt_error_msg("OTF_WStream_writeEnter failed:\n %s", otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__Leave:
      {
        VTBuf_Entry_EnterLeave* entry = (VTBuf_Entry_EnterLeave*)p;

        if (OTF_WStream_writeLeave(gen->filestream, entry->time, entry->rid,
              pid, entry->sid) == 0)
        {
          vt_error_msg("OTF_WStream_writeLeave failed:\n %s", otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__FileOperation:
      {
        VTBuf_Entry_FileOperation* entry = (VTBuf_Entry_FileOperation*)p;

        if (OTF_WStream_writeFileOperation(gen->filestream, entry->time,
              entry->fid, pid, entry->hid, entry->op, entry->bytes,
              entry->etime - entry->time, entry->sid) == 0)
        {
          vt_error_msg("OTF_WStream_writeFileOperation failed:\n %s",
                       otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__BeginFileOperation:
      {
        VTBuf_Entry_BeginFileOperation* entry =
          (VTBuf_Entry_BeginFileOperation*)p;

        if (OTF_WStream_writeBeginFileOperation(gen->filestream, entry->time,
              pid, entry->mid, entry->sid) == 0)
        {
          vt_error_msg("OTF_WStream_writeBeginFileOperation failed:\n %s",
                       otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__EndFileOperation:
      {
        VTBuf_Entry_EndFileOperation* entry = (VTBuf_Entry_EndFileOperation*)p;

        if (OTF_WStream_writeEndFileOperation(gen->filestream, entry->time, pid,
              entry->fid, entry->mid, entry->hid, entry->op, entry->bytes,
              entry->sid) == 0)
        {
          vt_error_msg("OTF_WStream_writeEndFileOperation failed:\n %s",
                       otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__Counter:
      {
        VTBuf_Entry_Counter* entry = (VTBuf_Entry_Counter*)p;

        if (OTF_WStream_writeCounter(gen->filestream, entry->time, pid,
               entry->cid, entry->cval) == 0)
        {
          vt_error_msg("OTF_WStream_writeCounter failed:\n %s", otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__Comment:
      {
        VTBuf_Entry_Comment* entry = (VTBuf_Entry_Comment*)p;

        if (OTF_WStream_writeEventComment(gen->filestream, entry->time, pid,
              entry->comment) == 0)
        {
          vt_error_msg("OTF_WStream_writeEventComment failed:\n %s",
                       otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__Marker:
      {
        VTBuf_Entry_Marker* entry = (VTBuf_Entry_Marker*)p;

        if (OTF_WStream_writeMarker(gen->filestream, entry->time, pid,
              entry->mid, entry->mtext) == 0)
        {
          vt_error_msg("OTF_WStream_writeMarker failed:\n %s", otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__KeyValue:
      {
        VTBuf_Entry_KeyValue* entry = (VTBuf_Entry_KeyValue*)p;

        OTF_WBuffer* filestream_buffer;
        OTF_KeyValuePair kvpair;

        filestream_buffer = OTF_WStream_getEventBuffer(gen->filestream);
        if (filestream_buffer == NULL)
          vt_error_msg("OTF_WStream_getEventBuffer failed:\n %s", otf_strerr);

        kvpair.key = entry->kid;

        switch(entry->vtype)
        {
          case VT_KEYVAL_TYPE_CHAR:
            kvpair.type = OTF_CHAR;
            kvpair.value.otf_char = entry->kvalue.c;
            break;
          case VT_KEYVAL_TYPE_INT32:
            kvpair.type = OTF_INT32;
            kvpair.value.otf_int32 = entry->kvalue.i32;
            break;
          case VT_KEYVAL_TYPE_UINT32:
            kvpair.type = OTF_UINT32;
            kvpair.value.otf_uint32 = entry->kvalue.u32;
            break;
          case VT_KEYVAL_TYPE_INT64:
            kvpair.type = OTF_INT64;
            kvpair.value.otf_int64 = entry->kvalue.i64;
            break;
          case VT_KEYVAL_TYPE_UINT64:
            kvpair.type = OTF_UINT64;
            kvpair.value.otf_uint64 = entry->kvalue.u64;
            break;
          case VT_KEYVAL_TYPE_FLOAT:
            kvpair.type = OTF_FLOAT;
            kvpair.value.otf_float = entry->kvalue.f;
            break;
          case VT_KEYVAL_TYPE_DOUBLE:
            kvpair.type = OTF_DOUBLE;
            kvpair.value.otf_double = entry->kvalue.d;
            break;
          case VT_KEYVAL_TYPE_STRING:
            kvpair.type = OTF_BYTE_ARRAY;
            kvpair.value.otf_byte_array.len = strlen(entry->kvalue.s);
            memcpy(kvpair.value.otf_byte_array.array, entry->kvalue.s,
                   kvpair.value.otf_byte_array.len * sizeof(uint8_t));
            free(entry->kvalue.s);
            break;
          default:
            vt_libassert(0);
        }

        if (OTF_WBuffer_writeKeyValuePair_short(filestream_buffer,
              &kvpair) == 0)
        {
          vt_error_msg("OTF_WBuffer_writeKeyValuePair_short failed:\n %s",
                       otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__SendMsg:
      {
        VTBuf_Entry_SendRecvMsg* entry = (VTBuf_Entry_SendRecvMsg*)p;

        if (OTF_WStream_writeSendMsg(gen->filestream, entry->time, pid,
              entry->pid, entry->cid, entry->tag, entry->len, entry->sid) == 0)
        {
          vt_error_msg("OTF_WStream_writeSendMsg failed:\n %s", otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__RecvMsg:
      {
        VTBuf_Entry_SendRecvMsg* entry = (VTBuf_Entry_SendRecvMsg*)p;

        if (OTF_WStream_writeRecvMsg(gen->filestream, entry->time, pid,
              entry->pid, entry->cid, entry->tag, entry->len, entry->sid) == 0)
        {
          vt_error_msg("OTF_WStream_writeRecvMsg failed:\n %s", otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__CollectiveOperation:
      {
        VTBuf_Entry_CollectiveOperation* entry =
          (VTBuf_Entry_CollectiveOperation*)p;

        if (OTF_WStream_writeCollectiveOperation(gen->filestream,
              entry->time, pid, entry->rid, entry->cid, entry->rpid,
              entry->sent, entry->recvd, entry->etime - entry->time,
             entry->sid) == 0)
        {
          vt_error_msg("OTF_WStream_writeCollectiveOperation failed:\n %s",
                       otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__BeginCollectiveOperation:
      {
        VTBuf_Entry_BeginCollectiveOperation* entry =
          (VTBuf_Entry_BeginCollectiveOperation*)p;

        if (OTF_WStream_writeBeginCollectiveOperation(gen->filestream,
              entry->time, pid, entry->rid, entry->mid, entry->cid,
              entry->rpid, entry->sent, entry->recvd, entry->sid) == 0)
        {
          vt_error_msg("OTF_WStream_writeBeginCollectiveOperation failed:\n %s",
                       otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__EndCollectiveOperation:
      {
        VTBuf_Entry_EndCollectiveOperation* entry =
          (VTBuf_Entry_EndCollectiveOperation*)p;

        if (OTF_WStream_writeEndCollectiveOperation(gen->filestream,
              entry->time, pid, entry->mid) == 0)
        {
          vt_error_msg("OTF_WStream_writeEndCollectiveOperation failed:\n %s",
                       otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__RMAPut:
      {
        VTBuf_Entry_RMAPutGet* entry = (VTBuf_Entry_RMAPutGet*)p;

        if (OTF_WStream_writeRMAPut(gen->filestream, entry->time, pid,
              entry->opid, entry->tpid, entry->cid, entry->tag, entry->len,
              entry->sid) == 0)
        {
          vt_error_msg("OTF_WStream_writeRMAPut failed:\n %s", otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__RMAPutRE:
      {
        VTBuf_Entry_RMAPutGet* entry = (VTBuf_Entry_RMAPutGet*)p;

        if (OTF_WStream_writeRMAPutRemoteEnd(gen->filestream, entry->time,
              pid, entry->opid, entry->tpid, entry->cid, entry->tag,
              entry->len, entry->sid) == 0)
        {
          vt_error_msg("OTF_WStream_writeRMAPutRemoteEnd failed:\n %s",
                       otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__RMAGet:
      {
        VTBuf_Entry_RMAPutGet* entry = (VTBuf_Entry_RMAPutGet*)p;

        if (OTF_WStream_writeRMAGet(gen->filestream, entry->time, pid,
              entry->opid, entry->tpid, entry->cid, entry->tag, entry->len,
              entry->sid) == 0)
        {
          vt_error_msg("OTF_WStream_writeRMAGet failed:\n %s", otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__RMAEnd:
      {
        VTBuf_Entry_RMAEnd* entry = (VTBuf_Entry_RMAEnd*)p;

        if (OTF_WStream_writeRMAEnd(gen->filestream, entry->time, pid,
              entry->rpid, entry->cid, entry->tag, entry->sid) == 0)
        {
          vt_error_msg("OTF_WStream_writeRMAEnd failed:\n %s", otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__FunctionSummary:
      {
        VTBuf_Entry_FunctionSummary* entry = (VTBuf_Entry_FunctionSummary*)p;

        if (OTF_WStream_writeFunctionSummary(gen->filestream, entry->time,
              entry->rid, pid, entry->cnt, entry->excl, entry->incl) == 0)
        {
          vt_error_msg("OTF_WStream_writeFunctionSummary failed:\n %s",
                       otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__MessageSummary:
      {
        VTBuf_Entry_MessageSummary* entry = (VTBuf_Entry_MessageSummary*)p;

        if (OTF_WStream_writeMessageSummary(gen->filestream, entry->time,
              pid, entry->peer, entry->cid, entry->tag, entry->scnt,
              entry->rcnt, entry->sent, entry->recvd) == 0)
        {
          vt_error_msg("OTF_WStream_writeMessageSummary failed:\n %s",
                       otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__CollectiveOperationSummary:
      {
        VTBuf_Entry_CollectiveOperationSummary* entry =
          (VTBuf_Entry_CollectiveOperationSummary*)p;

        if (OTF_WStream_writeCollopSummary(gen->filestream, entry->time,
              pid, entry->cid, entry->rid, entry->scnt, entry->rcnt,
              entry->sent, entry->recvd) == 0)
        {
          vt_error_msg("OTF_WStream_writeCollopSummary failed:\n %s",
                       otf_strerr);
        }

        break;
      }
      case VTBUF_ENTRY_TYPE__FileOperationSummary:
      {
        VTBuf_Entry_FileOperationSummary* entry =
          (VTBuf_Entry_FileOperationSummary*)p;

        if (OTF_WStream_writeFileOperationSummary(gen->filestream,
              entry->time, entry->fid, pid, entry->nopen, entry->nclose,
              entry->nread, entry->nwrite, entry->nseek, entry->read,
              entry->wrote) == 0)
        {
          vt_error_msg("OTF_WStream_writeFileOperationSummary failed:\n %s",
                       otf_strerr);
        }

        break;
      }
      default:
      {
        vt_libassert(0);
      }
    }

    /* last buffer entry and end flush not marked ? */
    if (!end_flush_marked &&
        p + ((VTBuf_Entry_Base*)p)->length >= gen->buf->pos)
    {
      /* mark end of flush, if it's not the last (invisible) flush and
         max flushes not reached */
      if (!lastFlush && gen->flushcntr > 1)
      {
        uint64_t flush_etime;
#if defined(VT_PLUGIN_CNTR)
        /* ... but not when writing post mortem counters */
        uint8_t flag;
        VT_PLUGIN_CNTR_WRITING_POST_MORTEM(gen->tid, flag);
        if (flag)
        {
          flush_etime = flushBTime;
        }
        else
#endif /* VT_PLUGIN_CNTR */
        {
          flush_etime = vt_pform_wtime();
          vt_exit_flush(gen->tid, &flush_etime);
        }
        if (flushETime != NULL) *flushETime = flush_etime;
      }

      end_flush_marked = 1;
    }

    p += ((VTBuf_Entry_Base*)p)->length;
  }

  if (lastFlush)
  {
    /* if no events or statistics recorded, write event/summary comment record
       in order that all event/summary files will exist */
    if (!gen->hasdata)
    {
      uint64_t time = vt_pform_wtime();

      if (VTGEN_IS_TRACE_ON(gen))
      {
        if (OTF_WStream_writeEventComment(gen->filestream, time, pid, "") == 0)
        {
          vt_error_msg("OTF_WStream_writeEventComment failed:\n %s",
                       otf_strerr);
        }
      }

      if (VTGEN_IS_SUM_ON(gen))
      {
        if (OTF_WStream_writeSummaryComment(gen->filestream, time, pid, "") == 0)
        {
          vt_error_msg("OTF_WStream_writeSummaryComment failed:\n %s",
                       otf_strerr);
        }
      }

      /* set time range */
      if (gen->timerange.min == (uint64_t)-1)
        gen->timerange.min = time;
      gen->timerange.max = time;
    }

    /* write time range record */
    if (OTF_WStream_writeDefTimeRange(gen->filestream, gen->timerange.min,
          gen->timerange.max, NULL) == 0)
    {
      vt_error_msg("OTF_WStream_writeDefTimeRange failed:\n %s",
                   otf_strerr);
    }
  }

  /* reset buffer */
  gen->buf->pos = gen->buf->mem;

  vt_cntl_msg(2, "Flushed OTF writer stream [namestub %s id %x]",
              gen->fileprefix, VT_PROCESS_ID(vt_my_trace, gen->tid));

  /* decrement flush counter */
  if (gen->flushcntr > 0) gen->flushcntr--;

  /* switch tracing off, if number of max flushes reached */
  if (!lastFlush && gen->flushcntr == 0)
  {
    int max_flushes = vt_env_max_flushes();

    vt_cntl_msg(1, "Maximum number of buffer flushes reached "
                "(VT_MAX_FLUSHES=%d)", max_flushes);

    vt_trace_off(gen->tid, 1, 1);

    vt_def_comment(gen->tid,
                   VT_UNIFY_STRID_VT_COMMENT"Warning: This trace is "
                   "incomplete due to reached maximum number of buffer "
                   "flushes. (VT_MAX_FLUSHES=%d)", max_flushes);
  }

  /* reset rewind mark and time */
  gen->rewindmark.time = 0;
  gen->rewindmark.pos = (buffer_t)-1;

  /* enable I/O tracing again */
  VT_RESUME_IO_TRACING(gen->tid);
}

void VTGen_close(VTGen* gen)
{
  /* close summary */
  if (VTGEN_IS_SUM_ON(gen))
    VTSum_close(gen->sum);

  /* flush buffer if necessary */
  VTGen_flush(gen, 1, 0, NULL);

  if (gen->fileprefix)
  {
    /* close writer stream */
    if (OTF_WStream_close(gen->filestream) == 0)
      vt_error_msg("OTF_WStream_close failed:\n %s", otf_strerr);

    /* close file manager of writer stream */
    OTF_FileManager_close(gen->filemanager);
    /* no return value; check otf_errno for error */
    if (otf_errno != OTF_NO_ERROR)
      vt_error_msg("OTF_FileManager_close failed:\n %s", otf_strerr);

    vt_cntl_msg(2, "Closed OTF writer stream [namestub %s id %x]",
                gen->fileprefix, VT_PROCESS_ID(vt_my_trace, gen->tid));
  }

  /* free buffer memory */
  free(gen->buf->mem);

  /* free buffer record */
  free(gen->buf);
}

void VTGen_delete(VTGen* gen)
{
  if (gen->fileprefix)
  {
    /* copy/move temporary trace files to global trace directory, if necessary */
    if (!gen->same_ldir_gdir)
    {
      char* tmp_namev[5];
      char* global_name;
      uint32_t global_name_len;
      char* suffix;

      char* fprefix = vt_env_fprefix();
      int do_rename = 1;

      uint8_t i;

      /* compose temporary file names for removal */

      tmp_namev[0] =
        OTF_getFilename(gen->fileprefix, VT_PROCESS_ID(vt_my_trace, gen->tid),
                        OTF_FILETYPE_DEF | gen->filecomp,
                        0, NULL);
      vt_libassert(tmp_namev[0]);

      tmp_namev[1] =
        OTF_getFilename(gen->fileprefix, VT_PROCESS_ID(vt_my_trace, gen->tid),
                        OTF_FILETYPE_EVENT | gen->filecomp,
                        0, NULL);
      vt_libassert(tmp_namev[1]);

      tmp_namev[2] =
        OTF_getFilename(gen->fileprefix, VT_PROCESS_ID(vt_my_trace, gen->tid),
                        OTF_FILETYPE_STATS | gen->filecomp,
                        0, NULL);
      vt_libassert(tmp_namev[2]);

      tmp_namev[3] =
        OTF_getFilename(gen->fileprefix, VT_PROCESS_ID(vt_my_trace, gen->tid),
                        OTF_FILETYPE_MARKER | gen->filecomp,
                        0, NULL);
      vt_libassert(tmp_namev[3]);

      tmp_namev[4] = NULL;

      i = 0;
      while(tmp_namev[i] != NULL)
      {
        /* local temp. trace file exists? */
        if (access(tmp_namev[i], R_OK) != 0)
        {
          free(tmp_namev[i++]);
          continue;
        }

        /* determine file suffix */
        suffix = strchr(tmp_namev[i]+strlen(gen->fileprefix)+1, '.');

        /* build global file name */
        global_name_len = strlen(gen->gdir) + strlen(fprefix) + 32;
        global_name = (char*)calloc(global_name_len+1, sizeof(char));

        if (vt_my_funique > 0)
        {
          snprintf(global_name, global_name_len, "%s/%s_%u.%x%s",
                   gen->gdir, fprefix, vt_my_funique,
                   VT_PROCESS_ID(vt_my_trace, gen->tid), suffix);
        }
        else
        {
          snprintf(global_name, global_name_len, "%s/%s.%x%s",
                   gen->gdir, fprefix, VT_PROCESS_ID(vt_my_trace, gen->tid),
                   suffix);
        }

        /* rename file, if possible */
        if (do_rename)
        {
          if (rename(tmp_namev[i], global_name) == 0)
          {
            vt_cntl_msg(2, "Moved trace file %s to %s", tmp_namev[i],
                        global_name);
          }
          else
          {
            do_rename = 0;
            free(global_name);
            continue;
          }
        }
        /* otherwise, copy file */
        else
        {
          size_t bytes_read;
          void *buffer;
          size_t buflen;
          FILE* infile;
          FILE* outfile;

          /* allocate buffer */
          buflen = VT_FILE_COPY_BUFFER_SIZE;
          buffer = malloc( buflen );
          if( !buffer )
            vt_error_msg( "Cannot allocate %u bytes for copy buffer", buflen );

          /* open files */
          if ((infile = fopen(tmp_namev[i], "rb")) == NULL )
            vt_error_msg("Cannot open trace file %s for reading", tmp_namev[i]);
          if ((outfile = fopen(global_name, "wb")) == NULL)
            vt_error_msg("Cannot open trace file %s for writing", global_name);

          /* copy file */
          while((bytes_read = fread(buffer, 1, buflen, infile)))
          {
            if( bytes_read > fwrite(buffer, 1, bytes_read, outfile) )
            {
              fclose( infile );
              fclose( outfile );
              free( buffer );
              vt_error_msg("Failed to write to file %s", global_name);
            }
          }

          /* close files */
          fclose(infile);
          fclose(outfile);

          /* free buffer */
          free( buffer );

          vt_cntl_msg(2, "Copied trace file %s to %s", tmp_namev[i], global_name);

          /* remove local temp. trace file */
          if (remove(tmp_namev[i]) == 0 )
            vt_cntl_msg(2, "Removed trace file %s", tmp_namev[i]);
          else
            vt_error_msg("Cannot remove trace file %s", tmp_namev[i]);
        }

        free(global_name);
        free(tmp_namev[i]);

        i++;
      }
    }

    free(gen->fileprefix);
  }

  /* delete sum record */
  if (VTGEN_IS_SUM_ON(gen)) VTSum_delete(gen->sum);

  /* free gen record */
  free(gen);
}

void VTGen_destroy(VTGen* gen)
{
  if(gen->fileprefix)
  {
    /* close writer stream */
    if (OTF_WStream_close(gen->filestream) == 0)
      vt_error_msg("OTF_WStream_close failed:\n %s", otf_strerr);

    /* close file manager of writer stream */
    OTF_FileManager_close(gen->filemanager);
    /* no return value; check otf_errno for error */
    if (otf_errno != OTF_NO_ERROR)
      vt_error_msg("OTF_FileManager_close failed:\n %s", otf_strerr);

    free(gen->fileprefix);
  }

  /* destroy sum record */
  if (VTGEN_IS_SUM_ON(gen)) VTSum_destroy(gen->sum);

  /* free buffer memory */
  free(gen->buf->mem);

  /* free buffer record */
  free(gen->buf);

  /* free gen record */
  free(gen);
}

uint8_t VTGen_get_buflevel(VTGen* gen)
{
  VTGEN_CHECK(gen);

  return (uint8_t)(((gen->buf->pos - gen->buf->mem) * 100) / gen->buf->size);
}


/* -- Writing trace records -- */


/* - Definition records - */

void VTGen_write_DEFINITION_COMMENT(VTGen* gen, const char* comment)
{
  VTBuf_Entry_DefinitionComment* new_entry;

  const uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefinitionComment) +
                        (strlen(comment) * sizeof(char))));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC(gen, length);

  new_entry = ((VTBuf_Entry_DefinitionComment*)gen->buf->pos);

  new_entry->type    = VTBUF_ENTRY_TYPE__DefinitionComment;
  new_entry->length  = length;
  strcpy(new_entry->comment, comment);

  VTGEN_JUMP(gen, length);
}

void VTGen_write_DEF_SCL_FILE(VTGen* gen, uint32_t fid, const char* fname)
{
  VTBuf_Entry_DefSclFile* new_entry;

  const uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefSclFile) +
                        (strlen(fname) * sizeof(char))));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC(gen, length);

  new_entry = ((VTBuf_Entry_DefSclFile*)gen->buf->pos);

  new_entry->type   = VTBUF_ENTRY_TYPE__DefSclFile;
  new_entry->length = length;
  new_entry->fid    = fid;
  strcpy(new_entry->fname, fname);

  VTGEN_JUMP(gen, length);
}

void VTGen_write_DEF_SCL(VTGen* gen, uint32_t sid, uint32_t fid, uint32_t ln)
{
  VTBuf_Entry_DefScl* new_entry;

  static const uint32_t length =
    VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_DefScl));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC(gen, length);

  new_entry = ((VTBuf_Entry_DefScl*)gen->buf->pos);

  new_entry->type   = VTBUF_ENTRY_TYPE__DefScl;
  new_entry->length = length;
  new_entry->sid    = sid;
  new_entry->fid    = fid;
  new_entry->ln     = ln;

  VTGEN_JUMP(gen, length);
}

void VTGen_write_DEF_FILE_GROUP(VTGen* gen, uint32_t gid, const char* gname)
{
  VTBuf_Entry_DefFileGroup* new_entry;

  const uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefFileGroup) +
                        (strlen(gname) * sizeof(char))));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC(gen, length);

  new_entry = ((VTBuf_Entry_DefFileGroup*)gen->buf->pos);

  new_entry->type   = VTBUF_ENTRY_TYPE__DefFileGroup;
  new_entry->length = length;
  new_entry->gid    = gid;
  strcpy(new_entry->gname, gname);

  VTGEN_JUMP(gen, length);
}

void VTGen_write_DEF_FILE(VTGen* gen, uint32_t fid, const char* fname,
                          uint32_t gid)
{
  VTBuf_Entry_DefFile* new_entry;

  const uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefFile) +
                        (strlen(fname) * sizeof(char))));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC(gen, length);

  new_entry = ((VTBuf_Entry_DefFile*)gen->buf->pos);

  new_entry->type   = VTBUF_ENTRY_TYPE__DefFile;
  new_entry->length = length;
  new_entry->fid    = fid;
  new_entry->gid    = gid;
  strcpy(new_entry->fname, fname);

  VTGEN_JUMP(gen, length);
}

void VTGen_write_DEF_FUNCTION_GROUP(VTGen* gen, uint32_t rdid,
                                    const char* rdesc)
{
  VTBuf_Entry_DefFunctionGroup* new_entry;

  const uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefFunctionGroup) +
                        (strlen(rdesc) * sizeof(char))));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC(gen, length);

  new_entry = ((VTBuf_Entry_DefFunctionGroup*)gen->buf->pos);

  new_entry->type   = VTBUF_ENTRY_TYPE__DefFunctionGroup;
  new_entry->length = length;
  new_entry->rdid   = rdid;
  strcpy(new_entry->rdesc, rdesc);

  VTGEN_JUMP(gen, length);
}

void VTGen_write_DEF_FUNCTION(VTGen* gen, uint32_t rid, const char* rname,
                              uint32_t rdid, uint32_t sid)
{
  VTBuf_Entry_DefFunction* new_entry;

  const uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefFunction) +
                        (strlen(rname) * sizeof(char))));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC(gen, length);

  new_entry = ((VTBuf_Entry_DefFunction*)gen->buf->pos);

  new_entry->type   = VTBUF_ENTRY_TYPE__DefFunction;
  new_entry->length = length;
  new_entry->rid    = rid;
  new_entry->rdid   = rdid;
  new_entry->sid    = sid;
  strcpy(new_entry->rname, rname);

  VTGEN_JUMP(gen, length);
}

void VTGen_write_DEF_COLLECTIVE_OPERATION(VTGen* gen, uint32_t cid,
                                          const char* cname, uint32_t ctype)
{
  VTBuf_Entry_DefCollectiveOperation* new_entry;

  const uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefCollectiveOperation) +
                        (strlen(cname) * sizeof(char))));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC(gen, length);

  new_entry = ((VTBuf_Entry_DefCollectiveOperation*)gen->buf->pos);

  new_entry->type   = VTBUF_ENTRY_TYPE__DefCollectiveOperation;
  new_entry->length = length;
  new_entry->cid    = cid;
  new_entry->ctype  = ctype;
  strcpy(new_entry->cname, cname);

  VTGEN_JUMP(gen, length);
}

void VTGen_write_DEF_COUNTER_GROUP(VTGen* gen, uint32_t gid, const char* gname)
{
  VTBuf_Entry_DefCounterGroup* new_entry;

  const uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefCounterGroup) +
                        (strlen(gname) * sizeof(char))));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC(gen, length);

  new_entry = ((VTBuf_Entry_DefCounterGroup*)gen->buf->pos);

  new_entry->type   = VTBUF_ENTRY_TYPE__DefCounterGroup;
  new_entry->length = length;
  new_entry->gid    = gid;
  strcpy(new_entry->gname, gname);

  VTGEN_JUMP(gen, length);
}

void VTGen_write_DEF_COUNTER(VTGen* gen, uint32_t cid, const char* cname,
                             const char* cunit, uint32_t cprop, uint32_t gid,
                             uint32_t pgid)
{
  VTBuf_Entry_DefCounter* new_entry;

  const uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefCounter) +
                        (strlen(cname) * sizeof(char))));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC(gen, length);

  new_entry = ((VTBuf_Entry_DefCounter*)gen->buf->pos);

  new_entry->type   = VTBUF_ENTRY_TYPE__DefCounter;
  new_entry->length = length;
  new_entry->cid    = cid;
  new_entry->cprop  = cprop;
  new_entry->gid    = gid;
  new_entry->pgid   = pgid;
  strncpy(new_entry->cunit, cunit, sizeof(new_entry->cunit)-1);
  new_entry->cunit[sizeof(new_entry->cunit)-1] = '\0';
  strcpy(new_entry->cname, cname);

  VTGEN_JUMP(gen, length);
}

void VTGen_write_DEF_PROCESS_GROUP(VTGen* gen, uint32_t gid, const char* grpn,
                                   uint32_t grpc, uint32_t grpv[])
{
  VTBuf_Entry_DefProcessGroup* new_entry;

  const uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefProcessGroup) +
                        (grpc > 0 ? (grpc - 1) * sizeof(uint32_t) : 0 )));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC(gen, length);

  new_entry = ((VTBuf_Entry_DefProcessGroup*)gen->buf->pos);

  new_entry->type   = VTBUF_ENTRY_TYPE__DefProcessGroup;
  new_entry->length = length;
  new_entry->gid    = gid;
  strncpy(new_entry->grpn, grpn, sizeof(new_entry->grpn)-1);
  new_entry->grpn[sizeof(new_entry->grpn)-1] = '\0';
  new_entry->grpc   = grpc;
  if( grpc > 0 )
    memcpy(new_entry->grpv, grpv, grpc * sizeof(uint32_t));

  VTGEN_JUMP(gen, length);
}

void VTGen_write_DEF_PROCESS_GROUP_ATTRIBUTES(VTGen* gen, uint32_t gid,
                                              uint32_t gattr)
{
  VTBuf_Entry_DefProcessGroupAttributes* new_entry;

  static const uint32_t length =
    VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_DefProcessGroupAttributes));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC(gen, length);

  new_entry = ((VTBuf_Entry_DefProcessGroupAttributes*)gen->buf->pos);

  new_entry->type   = VTBUF_ENTRY_TYPE__DefProcessGroupAttributes;
  new_entry->length = length;
  new_entry->gid    = gid;
  new_entry->gattr  = gattr;

  VTGEN_JUMP(gen, length);
}

void VTGen_write_DEF_KEYVAL(VTGen* gen, uint32_t kid, uint8_t vtype,
                            const char* kname)
{
  VTBuf_Entry_DefKeyValue* new_entry;

  const uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefKeyValue) +
                        (strlen(kname) * sizeof(char))));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC(gen, length);

  new_entry = ((VTBuf_Entry_DefKeyValue*)gen->buf->pos);

  new_entry->type   = VTBUF_ENTRY_TYPE__DefKeyValue;
  new_entry->length = length;
  new_entry->kid    = kid;
  new_entry->vtype  = vtype;
  strcpy(new_entry->kname, kname);

  VTGEN_JUMP(gen, length);
}

/* -- Marker -- */

void VTGen_write_DEF_MARKER(VTGen* gen, uint32_t mid, const char* mname,
                            uint32_t mtype )
{
  VTBuf_Entry_DefMarker* new_entry;

  const uint32_t length =
    VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_DefMarker) +
                        (strlen(mname) * sizeof(char))));

  VTGEN_CHECK(gen);

  VTGEN_ALLOC(gen, length);

  new_entry = ((VTBuf_Entry_DefMarker*)gen->buf->pos);

  new_entry->type   = VTBUF_ENTRY_TYPE__DefMarker;
  new_entry->length = length;
  new_entry->mid    = mid;
  new_entry->mtype  = mtype;
  strcpy(new_entry->mname, mname);

  VTGEN_JUMP(gen, length);
}


/* - Event records - */


/* -- Region -- */

void VTGen_write_ENTER(VTGen* gen, uint64_t* time, uint32_t rid, uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_EnterLeave* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_EnterLeave));

    VTGEN_ALLOC_EVENT(gen, length, time);

    new_entry = ((VTBuf_Entry_EnterLeave*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__Enter;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->rid    = rid;
    new_entry->sid    = sid;

    VTGEN_JUMP(gen, length);
  }

  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_FUNC))
    VTSum_enter(gen->sum, time, rid);
}

void VTGen_write_LEAVE(VTGen* gen, uint64_t* time, uint32_t rid, uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_EnterLeave* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_EnterLeave));

    VTGEN_ALLOC_EVENT(gen, length, time);

    new_entry = ((VTBuf_Entry_EnterLeave*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__Leave;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->rid    = rid;
    new_entry->sid    = sid;

    VTGEN_JUMP(gen, length);
  }

  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_FUNC))
    VTSum_exit(gen->sum, time, rid);
}

/* -- File I/O -- */

void VTGen_write_FILE_OPERATION(VTGen* gen, uint64_t* time,
                                uint64_t* etime, uint32_t fid, uint64_t hid,
                                uint32_t op, uint64_t bytes, uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_FileOperation* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_FileOperation));

    *etime -= *time;
    VTGEN_ALLOC_EVENT(gen, length, time);
    *etime += *time;

    new_entry = ((VTBuf_Entry_FileOperation*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__FileOperation;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->etime  = *etime;
    new_entry->fid    = fid;
    new_entry->hid    = hid;
    new_entry->op     = (op == VT_IOOP_DUP) ? OTF_FILEOP_OPEN : op;
    new_entry->bytes  = bytes;
    new_entry->sid    = sid;

    VTGEN_JUMP(gen, length);
  }

  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_FILEOP))
  {
    switch( op )
    {
      case OTF_FILEOP_OPEN:
      {
        VTSum_fileop_open(gen->sum, time, fid);
        break;
      }
      case OTF_FILEOP_CLOSE:
      {
        VTSum_fileop_close(gen->sum, time, fid);
        break;
      }
      case OTF_FILEOP_READ:
      {
        VTSum_fileop_read(gen->sum, time, fid, bytes);
        break;
      }
      case OTF_FILEOP_WRITE:
      {
        VTSum_fileop_write(gen->sum, time, fid, bytes);
        break;
      }
      case OTF_FILEOP_SEEK:
      {
        VTSum_fileop_seek(gen->sum, time, fid);
        break;
      }
    }
  }
}

void VTGen_write_BEGIN_FILE_OPERATION(VTGen* gen, uint64_t* time,
                                      uint64_t mid, uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_BeginFileOperation* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_BeginFileOperation));

    VTGEN_ALLOC_EVENT(gen, length, time);

    new_entry = ((VTBuf_Entry_BeginFileOperation*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__BeginFileOperation;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->mid    = mid;
    new_entry->sid    = sid;

    VTGEN_JUMP(gen, length);
  }
}

void VTGen_write_END_FILE_OPERATION(VTGen* gen, uint64_t* time,
                                    uint32_t fid, uint64_t mid, uint64_t hid,
                                    uint32_t op, uint64_t bytes, uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_EndFileOperation* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_EndFileOperation));

    VTGEN_ALLOC_EVENT(gen, length, time);

    new_entry = ((VTBuf_Entry_EndFileOperation*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__EndFileOperation;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->fid    = fid;
    new_entry->mid    = mid;
    new_entry->hid    = hid;
    new_entry->op     = op;
    new_entry->bytes  = bytes;
    new_entry->sid    = sid;

    VTGEN_JUMP(gen, length);
  }

/* TODO: Summary records for begin/end I/O records */
#if 0
  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_FILEOP))
  {
    switch( op )
    {
      case OTF_FILEOP_OPEN:
      {
        VTSum_fileop_open(gen->sum, time, fid);
        break;
      }
      case OTF_FILEOP_CLOSE:
      {
        VTSum_fileop_close(gen->sum, time, fid);
        break;
      }
      case OTF_FILEOP_READ:
      {
        VTSum_fileop_read(gen->sum, time, fid, bytes);
        break;
      }
      case OTF_FILEOP_WRITE:
      {
        VTSum_fileop_write(gen->sum, time, fid, bytes);
        break;
      }
      case OTF_FILEOP_SEEK:
      {
        VTSum_fileop_seek(gen->sum, time, fid);
        break;
      }
    }
  }
#endif
}

/* -- Counter -- */

void VTGen_write_COUNTER(VTGen* gen, uint64_t* time, uint32_t cid,
                         uint64_t cval)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_Counter* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_Counter));

    VTGEN_ALLOC_EVENT(gen, length, time);

    new_entry = ((VTBuf_Entry_Counter*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__Counter;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->cid    = cid;
    new_entry->cval   = cval;

    VTGEN_JUMP(gen, length);
  }
}

/* -- Comment -- */

void VTGen_write_COMMENT(VTGen* gen, uint64_t* time, const char* comment)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_Comment* new_entry;

    const uint32_t length =
      VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_Comment) +
                          (strlen(comment) * sizeof(char))));

    VTGEN_ALLOC_EVENT(gen, length, time);

    new_entry = ((VTBuf_Entry_Comment*)gen->buf->pos);

    new_entry->type    = VTBUF_ENTRY_TYPE__Comment;
    new_entry->length  = length;
    new_entry->time    = *time;
    strcpy(new_entry->comment, comment);

    VTGEN_JUMP(gen, length);
  }
}

/* -- Marker -- */

void VTGen_write_MARKER(VTGen* gen, uint64_t* time, uint32_t mid,
                        const char* mtext)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_Marker* new_entry;

    const uint32_t length =
      VTGEN_ALIGN_LENGTH((sizeof(VTBuf_Entry_Marker) +
                          (strlen(mtext) * sizeof(char))));

    VTGEN_ALLOC_EVENT(gen, length, time);

    new_entry = ((VTBuf_Entry_Marker*)gen->buf->pos);

    new_entry->type    = VTBUF_ENTRY_TYPE__Marker;
    new_entry->length  = length;
    new_entry->time    = *time;
    new_entry->mid     = mid;
    strcpy(new_entry->mtext, mtext);

    VTGEN_JUMP(gen, length);
  }
}

/* -- Key-Value -- */

void VTGen_write_KEYVAL(VTGen* gen, uint32_t kid, uint8_t vtype,
                        const void* kvalue)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_KeyValue* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_KeyValue));

    /* No VTGEN_ALLOC_EVENT since space must be guaranteed */
    vt_libassert( (uint64_t)((gen)->buf->pos - (gen)->buf->mem) <=
               (uint64_t)((gen)->buf->size - length) );

    new_entry = ((VTBuf_Entry_KeyValue*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__KeyValue;
    new_entry->length = length;
    new_entry->kid    = kid;
    new_entry->vtype  = vtype;

    switch(vtype)
    {
      case VT_KEYVAL_TYPE_CHAR:
        new_entry->kvalue.c = *((char*)kvalue);
        break;
      case VT_KEYVAL_TYPE_INT32:
        new_entry->kvalue.i32 = *((int32_t*)kvalue);
        break;
      case VT_KEYVAL_TYPE_UINT32:
        new_entry->kvalue.u32 = *((uint32_t*)kvalue);
        break;
      case VT_KEYVAL_TYPE_INT64:
        new_entry->kvalue.i64 = *((int64_t*)kvalue);
        break;
      case VT_KEYVAL_TYPE_UINT64:
        new_entry->kvalue.u64 = *((uint64_t*)kvalue);
        break;
      case VT_KEYVAL_TYPE_FLOAT:
        new_entry->kvalue.f = *((float*)kvalue);
        break;
      case VT_KEYVAL_TYPE_DOUBLE:
        new_entry->kvalue.d = *((double*)kvalue);
        break;
      case VT_KEYVAL_TYPE_STRING:
      {
        size_t kvalue_len = strlen((const char*)kvalue);
        vt_libassert(kvalue_len > 0);
        vt_libassert(kvalue_len <= OTF_KEYVALUE_MAX_ARRAY_LEN);
        new_entry->kvalue.s = strdup((const char*)kvalue);
        if (new_entry->kvalue.s == NULL)
          vt_error();
        break;
      }
      default:
        vt_libassert(0);
    }

    VTGEN_JUMP(gen, length);
  }
}

/* -- MPI-1 -- */

void VTGen_write_SEND_MSG(VTGen* gen, uint64_t* time, uint32_t pid,
                          uint32_t cid, uint32_t tag, uint32_t sent,
                          uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_SendRecvMsg* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_SendRecvMsg));

    VTGEN_ALLOC_EVENT(gen, length, time);

    new_entry = ((VTBuf_Entry_SendRecvMsg*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__SendMsg;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->pid    = pid;
    new_entry->cid    = cid;
    new_entry->tag    = tag;
    new_entry->len    = sent;
    new_entry->sid    = sid;

    VTGEN_JUMP(gen, length);
  }

  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_MSG))
    VTSum_msg_send(gen->sum, time, pid, cid, tag, (uint64_t)sent);
}

void VTGen_write_RECV_MSG(VTGen* gen, uint64_t* time, uint32_t pid,
                          uint32_t cid, uint32_t tag, uint32_t recvd,
                          uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_SendRecvMsg* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_SendRecvMsg));

    VTGEN_ALLOC_EVENT(gen, length, time);

    new_entry = ((VTBuf_Entry_SendRecvMsg*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__RecvMsg;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->pid    = pid;
    new_entry->cid    = cid;
    new_entry->tag    = tag;
    new_entry->len    = recvd;
    new_entry->sid    = sid;
    
    VTGEN_JUMP(gen, length);
  }

  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_MSG))
    VTSum_msg_recv(gen->sum, time, pid, cid, tag, (uint64_t)recvd);
}

void VTGen_write_COLLECTIVE_OPERATION(VTGen* gen, uint64_t* time,
                                      uint64_t* etime, uint32_t rid,
                                      uint32_t cid, uint32_t rpid,
                                      uint32_t sent, uint32_t recvd,
                                      uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_CollectiveOperation* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_CollectiveOperation));

    *etime -= *time;
    VTGEN_ALLOC_EVENT(gen, length, time);
    *etime += *time;

    new_entry = ((VTBuf_Entry_CollectiveOperation*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__CollectiveOperation;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->etime  = *etime;
    new_entry->rid    = rid;
    new_entry->cid    = cid;
    new_entry->rpid   = rpid;
    new_entry->sent   = sent;
    new_entry->recvd  = recvd;
    new_entry->sid    = sid;

    VTGEN_JUMP(gen, length);
  }

  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_COLLOP) && (sent > 0 || recvd > 0))
    VTSum_collop(gen->sum, time, rid, cid, (uint64_t)sent, (uint64_t)recvd);
}

void VTGen_write_BEGIN_COLLECTIVE_OPERATION(VTGen* gen, uint64_t* time,
                                            uint32_t rid, uint64_t mid,
                                            uint32_t rpid, uint32_t cid,
                                            uint64_t sent, uint64_t recvd,
                                            uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_BeginCollectiveOperation* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_BeginCollectiveOperation));

    VTGEN_ALLOC_EVENT(gen, length, time);

    new_entry = ((VTBuf_Entry_BeginCollectiveOperation*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__BeginCollectiveOperation;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->rid    = rid;
    new_entry->mid    = mid;
    new_entry->rpid   = rpid;
    new_entry->cid    = cid;
    new_entry->sent   = sent;
    new_entry->recvd  = recvd;
    new_entry->sid    = sid;

    VTGEN_JUMP(gen, length);
  }

  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_COLLOP) && (sent > 0 || recvd > 0))
    VTSum_collop(gen->sum, time, rid, cid, sent, recvd);
}

void VTGen_write_END_COLLECTIVE_OPERATION(VTGen* gen, uint64_t* time,
                                          uint64_t mid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_EndCollectiveOperation* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_EndCollectiveOperation));

    VTGEN_ALLOC_EVENT(gen, length, time);

    new_entry = ((VTBuf_Entry_EndCollectiveOperation*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__EndCollectiveOperation;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->mid    = mid;

    VTGEN_JUMP(gen, length);
  }
}

/* -- RMA - 1sided --*/

void VTGen_write_RMA_PUT(VTGen* gen, uint64_t* time, uint32_t opid,
                         uint32_t tpid, uint32_t cid, uint32_t tag,
                         uint32_t len, uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_RMAPutGet* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_RMAPutGet));

    VTGEN_ALLOC_EVENT(gen, length, time);

    new_entry = ((VTBuf_Entry_RMAPutGet*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__RMAPut;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->opid   = opid;
    new_entry->tpid   = tpid;
    new_entry->cid    = cid;
    new_entry->tag    = tag;
    new_entry->len    = len;
    new_entry->sid    = sid;

    VTGEN_JUMP(gen, length);
  }
}

void VTGen_write_RMA_PUTRE(VTGen* gen, uint64_t* time, uint32_t opid,
                           uint32_t tpid, uint32_t cid, uint32_t tag,
                           uint64_t len, uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_RMAPutGet* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_RMAPutGet));

    VTGEN_ALLOC_EVENT(gen, length, time);

    new_entry = ((VTBuf_Entry_RMAPutGet*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__RMAPutRE;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->opid   = opid;
    new_entry->tpid   = tpid;
    new_entry->cid    = cid;
    new_entry->tag    = tag;
    new_entry->len    = len;
    new_entry->sid    = sid;

    VTGEN_JUMP(gen, length);
  }
}

void VTGen_write_RMA_GET(VTGen* gen, uint64_t* time, uint32_t opid,
                         uint32_t tpid, uint32_t cid, uint32_t tag,
                         uint64_t len, uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_RMAPutGet* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_RMAPutGet));

    VTGEN_ALLOC_EVENT(gen, length, time);

    new_entry = ((VTBuf_Entry_RMAPutGet*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__RMAGet;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->opid   = opid;
    new_entry->tpid   = tpid;
    new_entry->cid    = cid;
    new_entry->tag    = tag;
    new_entry->len    = len;
    new_entry->sid    = sid;

    VTGEN_JUMP(gen, length);
  }
}

void VTGen_write_RMA_END(VTGen* gen, uint64_t* time, uint32_t rpid,
                         uint32_t cid, uint32_t tag, uint32_t sid)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_RMAEnd* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_RMAEnd));

    VTGEN_ALLOC_EVENT(gen, length, time);

    new_entry = ((VTBuf_Entry_RMAEnd*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__RMAEnd;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->rpid   = rpid;
    new_entry->cid    = cid;
    new_entry->tag    = tag;
    new_entry->sid    = sid;

    VTGEN_JUMP(gen, length);
  }
}

/* -- VampirTrace Internal -- */

void VTGen_write_ENTER_FLUSH(VTGen* gen, uint64_t* time)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_EnterLeave* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_EnterLeave));

    /* No VTGEN_ALLOC_EVENT since space reserved at buffer creation */

    new_entry = ((VTBuf_Entry_EnterLeave*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__Enter;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->rid    = vt_trc_regid[VT__TRC_FLUSH];
    new_entry->sid    = 0;

    VTGEN_JUMP(gen, length);
  }
}

void VTGen_write_LEAVE_FLUSH(VTGen* gen, uint64_t* time)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_EnterLeave* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_EnterLeave));

    /* No VTGEN_ALLOC_EVENT since space reserved at buffer creation */

    new_entry = ((VTBuf_Entry_EnterLeave*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__Leave;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->rid    = 0;
    new_entry->sid    = 0;

    VTGEN_JUMP(gen, length);
  }
}

void VTGen_write_ENTER_STAT(VTGen* gen, uint64_t* time)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_EnterLeave* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_EnterLeave)); 

    VTGEN_ALLOC_EVENT(gen, length, time);

    new_entry = ((VTBuf_Entry_EnterLeave*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__Enter;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->rid    = vt_trc_regid[VT__TRC_STAT];
    new_entry->sid    = 0;

    VTGEN_JUMP(gen, length);
  }
}

void VTGen_write_LEAVE_STAT(VTGen* gen, uint64_t* time)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_TRACE_ON(gen))
  {
    VTBuf_Entry_EnterLeave* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_EnterLeave));

    VTGEN_ALLOC_EVENT(gen, length, time);

    new_entry = ((VTBuf_Entry_EnterLeave*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__Leave;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->rid    = 0;
    new_entry->sid    = 0;

    VTGEN_JUMP(gen, length);
  }
}


/* - Summary records - */


void VTGen_write_FUNCTION_SUMMARY(VTGen* gen, uint64_t* time,
                                  uint32_t rid, uint64_t cnt, uint64_t excl,
                                  uint64_t incl)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_FUNC))
  {
    VTBuf_Entry_FunctionSummary* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_FunctionSummary));

    VTGEN_ALLOC_EVENT(gen, length, time);

    new_entry = ((VTBuf_Entry_FunctionSummary*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__FunctionSummary;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->rid    = rid;
    new_entry->cnt    = cnt;
    new_entry->excl   = excl;
    new_entry->incl   = incl;

    VTGEN_JUMP(gen, length);
  }
}

void VTGen_write_MESSAGE_SUMMARY(VTGen* gen, uint64_t* time,
                                 uint32_t peer, uint32_t cid, uint32_t tag,
                                 uint64_t scnt, uint64_t rcnt, uint64_t sent,
                                 uint64_t recvd)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_MSG))
  {
    VTBuf_Entry_MessageSummary* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_MessageSummary));

    VTGEN_ALLOC_EVENT(gen, length, time);

    new_entry = ((VTBuf_Entry_MessageSummary*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__MessageSummary;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->peer   = peer;
    new_entry->cid    = cid;
    new_entry->tag    = tag;
    new_entry->scnt   = scnt;
    new_entry->rcnt   = rcnt;
    new_entry->sent   = sent;
    new_entry->recvd  = recvd;

    VTGEN_JUMP(gen, length);
  }
}

void VTGen_write_COLLECTIVE_OPERATION_SUMMARY(VTGen* gen, uint64_t* time,
                                              uint32_t cid, uint32_t rid,
                                              uint64_t scnt, uint64_t rcnt,
                                              uint64_t sent, uint64_t recvd)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_COLLOP))
  {
    VTBuf_Entry_CollectiveOperationSummary* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_CollectiveOperationSummary));

    VTGEN_ALLOC_EVENT(gen, length, time);

    new_entry = ((VTBuf_Entry_CollectiveOperationSummary*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__CollectiveOperationSummary;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->cid    = cid;
    new_entry->rid    = rid;
    new_entry->scnt   = scnt;
    new_entry->rcnt   = rcnt;
    new_entry->sent   = sent;
    new_entry->recvd  = recvd;

    VTGEN_JUMP(gen, length);
  }
}

void VTGen_write_FILE_OPERATION_SUMMARY(VTGen* gen, uint64_t* time,
                                        uint32_t fid, uint64_t nopen,
                                        uint64_t nclose, uint64_t nread,
                                        uint64_t nwrite, uint64_t nseek,
                                        uint64_t read, uint64_t wrote)
{
  VTGEN_CHECK(gen);

  if (VTGEN_IS_SUM_PROP_ON(gen, VT_SUM_PROP_FILEOP))
  {
    VTBuf_Entry_FileOperationSummary* new_entry;

    static const uint32_t length =
      VTGEN_ALIGN_LENGTH(sizeof(VTBuf_Entry_FileOperationSummary));

    VTGEN_ALLOC_EVENT(gen, length, time);

    new_entry = ((VTBuf_Entry_FileOperationSummary*)gen->buf->pos);

    new_entry->type   = VTBUF_ENTRY_TYPE__FileOperationSummary;
    new_entry->length = length;
    new_entry->time   = *time;
    new_entry->fid    = fid;
    new_entry->nopen  = nopen;
    new_entry->nclose = nclose;
    new_entry->nread  = nread;
    new_entry->nwrite = nwrite;
    new_entry->nseek  = nseek;
    new_entry->read   = read;
    new_entry->wrote  = wrote;

    VTGEN_JUMP(gen, length);
  }
}

void VTGen_set_rewind_mark(VTGen* gen, uint64_t *time)
{
  VTGEN_CHECK(gen);

  gen->rewindmark.pos  = gen->buf->pos;
  gen->rewindmark.time = *time;
}

void VTGen_rewind(VTGen* gen, uint64_t *time)
{
  buffer_t p;
  uint32_t length;

  VTGEN_CHECK(gen);

  p = gen->rewindmark.pos;

  /* run over trace buffer and shift definition records to
     the rewind mark, other records will be dropped */
  while(p < gen->buf->pos)
  {
    length = ((VTBuf_Entry_Base*)p)->length;

    switch(((VTBuf_Entry_Base*)p)->type)
    {
      case VTBUF_ENTRY_TYPE__DefinitionComment:
      case VTBUF_ENTRY_TYPE__DefSclFile:
      case VTBUF_ENTRY_TYPE__DefScl:
      case VTBUF_ENTRY_TYPE__DefFileGroup:
      case VTBUF_ENTRY_TYPE__DefFile:
      case VTBUF_ENTRY_TYPE__DefFunctionGroup:
      case VTBUF_ENTRY_TYPE__DefFunction:
      case VTBUF_ENTRY_TYPE__DefCollectiveOperation:
      case VTBUF_ENTRY_TYPE__DefCounterGroup:
      case VTBUF_ENTRY_TYPE__DefCounter:
      case VTBUF_ENTRY_TYPE__DefProcessGroup:
      case VTBUF_ENTRY_TYPE__DefProcessGroupAttributes:
      case VTBUF_ENTRY_TYPE__DefMarker:
      case VTBUF_ENTRY_TYPE__DefKeyValue:
      {
        if(p != gen->rewindmark.pos)
          memmove(gen->rewindmark.pos, p, length);
        /* move rewind mark behind definition record */
        gen->rewindmark.pos += length;
        break;
      }
      default:
        break;
    }

    p += length;
  }

  /* reset current buffer position */
  gen->buf->pos = gen->rewindmark.pos;
  *time = gen->rewindmark.time;
}

uint8_t VTGen_is_rewind_mark_present(VTGen* gen)
{
  VTGEN_CHECK(gen);

  return (uint8_t)( gen->rewindmark.pos != (buffer_t)-1 );
}

