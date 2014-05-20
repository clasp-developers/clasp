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

#include <stdio.h>
#include <stdlib.h>

#include "vt_otf_gen.h"
#include "vt_otf_sum.h"
#include "vt_env.h"
#include "vt_error.h"
#include "vt_inttypes.h"
#include "vt_pform.h"
#include "vt_trc.h"

#include "util/hash.h"

#include "otf.h"

#define VTSUM_STACK_BSIZE  100
#define VTSUM_STAT_BSIZE   500
#define VTSUM_HASH_MAX    1024

/*
 *-----------------------------------------------------------------------------
 * Macro functions
 *-----------------------------------------------------------------------------
 */

#define VTSUM_CHECK(sum) \
  if (sum == NULL) vt_error_msg("Abort: Uninitialized summary generator")

#define VTSUM_FUNC_STAT_ADD(_sum, _rid, _stat_idx)                    \
{                                                                     \
  if (_sum->func_stat_num == _sum->func_stat_size)                    \
  {                                                                   \
    _sum->func_stat = (VTSum_funcStat*)realloc(_sum->func_stat,       \
		        (_sum->func_stat_size                         \
		         + VTSUM_STAT_BSIZE)                          \
		        * sizeof(VTSum_funcStat));                    \
    _sum->func_stat_size += VTSUM_STAT_BSIZE;                         \
  }                                                                   \
                                                                      \
  _stat_idx = _sum->func_stat_num++;                                  \
                                                                      \
  _sum->func_stat[_stat_idx].rid  = _rid;                             \
  _sum->func_stat[_stat_idx].cnt  = 0;                                \
  _sum->func_stat[_stat_idx].excl = 0;                                \
  _sum->func_stat[_stat_idx].incl = 0;                                \
}

#define VTSUM_MSG_STAT_ADD(_sum, _peer, _cid, _tag, _stat_idx)        \
{                                                                     \
  if (_sum->msg_stat_num == _sum->msg_stat_size)                      \
  {                                                                   \
    _sum->msg_stat = (VTSum_msgStat*)realloc(_sum->msg_stat,          \
		       (_sum->msg_stat_size                           \
		        + VTSUM_STAT_BSIZE)                           \
		       * sizeof(VTSum_msgStat));                      \
    _sum->msg_stat_size += VTSUM_STAT_BSIZE;                          \
  }                                                                   \
                                                                      \
  _stat_idx = _sum->msg_stat_num++;                                   \
                                                                      \
  _sum->msg_stat[_stat_idx].peer  = _peer;                            \
  _sum->msg_stat[_stat_idx].cid   = _cid;                             \
  _sum->msg_stat[_stat_idx].tag   = _tag;                             \
  _sum->msg_stat[_stat_idx].scnt  = 0;                                \
  _sum->msg_stat[_stat_idx].rcnt  = 0;                                \
  _sum->msg_stat[_stat_idx].sent  = 0;                                \
  _sum->msg_stat[_stat_idx].recvd = 0;                                \
}

#define VTSUM_COLLOP_STAT_ADD(_sum, _rid, _cid, _stat_idx)            \
{                                                                     \
  if (_sum->collop_stat_num == _sum->collop_stat_size)                \
  {                                                                   \
    _sum->collop_stat = (VTSum_collopStat*)realloc(_sum->collop_stat, \
			  (_sum->collop_stat_size                     \
			   + VTSUM_STAT_BSIZE)                        \
			  * sizeof(VTSum_collopStat));                \
    _sum->collop_stat_size += VTSUM_STAT_BSIZE;                       \
  }						                      \
                                                                      \
  _stat_idx = _sum->collop_stat_num++;                                \
                                                                      \
  _sum->collop_stat[_stat_idx].rid   = _rid;                          \
  _sum->collop_stat[_stat_idx].cid   = _cid;                          \
  _sum->collop_stat[_stat_idx].scnt  = 0;                             \
  _sum->collop_stat[_stat_idx].rcnt  = 0;                             \
  _sum->collop_stat[_stat_idx].sent  = 0;                             \
  _sum->collop_stat[_stat_idx].recvd = 0;                             \
}

#define VTSUM_FILEOP_STAT_ADD(_sum, _fid, _stat_idx)                  \
{                                                                     \
  if (_sum->fileop_stat_num == _sum->fileop_stat_size)                \
  {                                                                   \
    _sum->fileop_stat = (VTSum_fileopStat*)realloc(_sum->fileop_stat, \
			  (_sum->fileop_stat_size                     \
			   + VTSUM_STAT_BSIZE)                        \
			  * sizeof(VTSum_fileopStat));                \
    _sum->fileop_stat_size += VTSUM_STAT_BSIZE;                       \
  }                                                                   \
                                                                      \
  _stat_idx = _sum->fileop_stat_num++;                                \
                                                                      \
  _sum->fileop_stat[_stat_idx].fid    = _fid;                         \
  _sum->fileop_stat[_stat_idx].nopen  = 0;                            \
  _sum->fileop_stat[_stat_idx].nclose = 0;                            \
  _sum->fileop_stat[_stat_idx].nread  = 0;                            \
  _sum->fileop_stat[_stat_idx].nwrite = 0;                            \
  _sum->fileop_stat[_stat_idx].nseek  = 0;                            \
  _sum->fileop_stat[_stat_idx].read   = 0;                            \
  _sum->fileop_stat[_stat_idx].wrote  = 0;                            \
}

#define VTSUM_STACK_PUSH(_sum, _stat_idx, _time)                      \
{                                                                     \
  if (_sum->stack_pos+1 == (int32_t)_sum->stack_size)                 \
  {                                                                   \
    _sum->stack = (VTSum_stack*)realloc(_sum->stack,                  \
			       (_sum->stack_size                      \
				+ VTSUM_STACK_BSIZE)                  \
			       * sizeof(VTSum_stack));                \
    _sum->stack_size += VTSUM_STACK_BSIZE;                            \
  }                                                                   \
                                                                      \
  _sum->func_stat[_stat_idx].cnt++;                                   \
                                                                      \
  _sum->stack_pos++;                                                  \
  _sum->stack[_sum->stack_pos].stat_idx = _stat_idx;                  \
  _sum->stack[_sum->stack_pos].hexcl = *_time;                        \
  _sum->stack[_sum->stack_pos].hincl = *_time;                        \
                                                                      \
  if (_sum->stack_pos > 0)                                            \
  {                                                                   \
    _sum->func_stat[_sum->stack[_sum->stack_pos-1].stat_idx].excl +=  \
       (*_time - _sum->stack[_sum->stack_pos-1].hexcl);               \
  }                                                                   \
}

#define VTSUM_STACK_POP(_sum, _time)                                  \
{                                                                     \
  if (_sum->stack_pos == -1)                                          \
    vt_error_msg("Abort: Stack underflow");                           \
                                                                      \
  _sum->func_stat[_sum->stack[_sum->stack_pos].stat_idx].excl +=      \
     (*_time - _sum->stack[_sum->stack_pos].hexcl);                   \
  _sum->func_stat[_sum->stack[_sum->stack_pos].stat_idx].incl +=      \
     (*_time - _sum->stack[_sum->stack_pos].hincl);                   \
                                                                      \
  _sum->stack_pos--;                                                  \
  if (_sum->stack_pos != -1)                                          \
     _sum->stack[_sum->stack_pos].hexcl = *_time;                     \
}

#define VT_CHECK_DUMP(_sum, _time)                                    \
  if (*_time >= _sum->next_dump) VTSum_dump(_sum, _time, 1);

#define VTSUM_IS_PROP_ON(_sum, _prop) \
  ((_sum->props & _prop) != 0)
#define VTSUM_IS_MSG_DTL_ON(_sum, _dtl) \
  ((_sum->msg_stat_dtls & _dtl) != 0)
#define VTSUM_IS_COLLOP_DTL_ON(_sum, _dtl) \
  ((_sum->collop_stat_dtls & _dtl) != 0)

/*
 *-----------------------------------------------------------------------------
 * VTSum
 *-----------------------------------------------------------------------------
 */

/* Data structure for function statistic */

typedef struct
{
  uint32_t         rid;
  uint64_t         cnt;
  uint64_t         excl;
  uint64_t         incl;
} VTSum_funcStat;

/* Data structure for message statistic */

typedef struct
{
  uint32_t         peer;
  uint32_t         cid;
  uint32_t         tag;
  uint64_t         scnt;
  uint64_t         rcnt;
  uint64_t         sent;
  uint64_t         recvd;
} VTSum_msgStat;

/* Data structure for collective operation statistics */

typedef struct
{
  uint32_t         rid;
  uint32_t         cid;
  uint64_t         scnt;
  uint64_t         rcnt;
  uint64_t         sent; 
  uint64_t         recvd;
} VTSum_collopStat;

/* Data structure for file operation statistic */

typedef struct
{
  uint32_t         fid;
  uint64_t         nopen;
  uint64_t         nclose;
  uint64_t         nread;
  uint64_t         nwrite;
  uint64_t         nseek;
  uint64_t         read;
  uint64_t         wrote;
} VTSum_fileopStat;

/* Data structure for call stack */

typedef struct
{
  uint64_t         hexcl;
  uint64_t         hincl;
  uint64_t         stat_idx;
} VTSum_stack;

/* Hash table to map function ids to statistic */

typedef struct HN_func
{
  uint32_t id;              /* hash code (identifier of region) */
  uint64_t stat_idx;        /* index of associated statistic    */
  struct HN_func* next;
} VTSum_funcHashNode;

/* Hash table to map message peer, comm, and tag to statistic */

typedef struct HN_msg
{
  uint32_t peer, cid, tag;  /* peer, comm, tag of message    */
  uint64_t stat_idx;        /* index of associated statistic */
  struct HN_msg* next;
} VTSum_msgHashNode;

/* Hash table to map collop and comm to statistic */

typedef struct HN_collop
{
  uint32_t rid, cid;        /* op, comm of collective op.    */
  uint64_t stat_idx;        /* index of associated statistic */
  struct HN_collop* next;
} VTSum_collopHashNode;

/* Hash table to map file op. ids to statistic */

typedef struct HN_fileop
{
  uint32_t id;              /* hash code (identifier of file op.) */
  uint64_t stat_idx;        /* index of associated statistic      */
  struct HN_fileop* next;
} VTSum_fileopHashNode;

/* VTSum record */

struct VTSum_struct
{
  VTGen*                 gen;

  VTSum_funcStat*        func_stat;
  VTSum_funcHashNode**   func_stat_htab;
  VTSum_stack*           stack;
  uint64_t               func_stat_size;
  uint64_t               func_stat_num;
  uint32_t               stack_size;
  int32_t                stack_pos;

  VTSum_msgStat*         msg_stat;
  VTSum_msgHashNode**    msg_stat_htab;
  uint64_t               msg_stat_size;
  uint64_t               msg_stat_num;
  uint8_t                msg_stat_dtls;

  VTSum_collopStat*      collop_stat;
  VTSum_collopHashNode** collop_stat_htab;
  uint64_t               collop_stat_size;
  uint64_t               collop_stat_num;
  uint8_t                collop_stat_dtls;

  VTSum_fileopStat*      fileop_stat;
  VTSum_fileopHashNode** fileop_stat_htab;
  uint64_t               fileop_stat_size;
  uint64_t               fileop_stat_num;

  uint32_t               tid;
  uint64_t               intv;
  uint64_t               next_dump;
  uint8_t                props;
};

/* Summary interval */
static uint64_t SumIntv = 0;

/* Stores index of function statistic `stat_idx' under hash code `h' */

static void hash_put_func(VTSum* sum, uint32_t h, uint64_t stat_idx) {
  uint32_t id = h & (VTSUM_HASH_MAX - 1);
  VTSum_funcHashNode* add =
    (VTSum_funcHashNode*)malloc(sizeof(VTSum_funcHashNode));
  add->id = h;
  add->stat_idx = stat_idx;
  add->next = sum->func_stat_htab[id];
  sum->func_stat_htab[id] = add;
}

/* Lookup hash code `h'
 * Returns hash table entry if already stored, otherwise NULL */

static VTSum_funcHashNode* hash_get_func(VTSum* sum, uint32_t h) {
  uint32_t id = h & (VTSUM_HASH_MAX - 1);
  VTSum_funcHashNode* curr = sum->func_stat_htab[id];
  while ( curr ) {
    if ( curr->id == h ) {
      return curr;
    }
    curr = curr->next;
  }
  return NULL;
}

/* Clear hash table for function statistics */

static void hash_clear_func(VTSum* sum) {
  int i;
  VTSum_funcHashNode* tmp;

  if (!sum->func_stat_htab) return;

  for (i = 0; i < VTSUM_HASH_MAX; i++)
  {
    while( sum->func_stat_htab[i] )
    {
      tmp = sum->func_stat_htab[i]->next;
      free(sum->func_stat_htab[i]);
      sum->func_stat_htab[i] = tmp;
    }
  }

  free(sum->func_stat_htab);
}

/* Stores index of message statistic `stat_idx' under hash code
   input `peer',`cid',`tag' */

static void hash_put_msg(VTSum* sum, uint32_t peer, uint32_t cid, uint32_t tag,
			 uint64_t stat_idx)
{
  uint32_t id;
  VTSum_msgHashNode* add;

  id = vt_hashtriple(peer, cid, tag, 0) & (VTSUM_HASH_MAX - 1);

  add = (VTSum_msgHashNode*)malloc(sizeof(VTSum_msgHashNode));
  add->peer     = peer;
  add->cid      = cid;
  add->tag      = tag;
  add->stat_idx = stat_idx;
  add->next     = sum->msg_stat_htab[id];
  sum->msg_stat_htab[id] = add;
}

/* Lookup hash code input `peer',`cid',`tag'
 * Returns hash table entry if already stored, otherwise NULL */

static VTSum_msgHashNode* hash_get_msg(VTSum* sum, uint32_t peer, uint32_t cid,
				       uint32_t tag) {
  uint32_t id;
  VTSum_msgHashNode* curr;

  id = vt_hashtriple(peer, cid, tag, 0) & (VTSUM_HASH_MAX - 1);

  curr = sum->msg_stat_htab[id];
  while ( curr ) {
    if ( curr->peer == peer &&
	 curr->cid == cid &&
	 curr->tag == tag ) {
       return curr;
    }
    curr = curr->next;
  }
  return NULL;
}

/* Clear hash table for message statistics */

static void hash_clear_msg(VTSum* sum) {
  int i;
  VTSum_msgHashNode* tmp;

  if (!sum->msg_stat_htab) return;

  for (i = 0; i < VTSUM_HASH_MAX; i++)
  {
    while( sum->msg_stat_htab[i] )
    {
      tmp = sum->msg_stat_htab[i]->next;
      free(sum->msg_stat_htab[i]);
      sum->msg_stat_htab[i] = tmp;
    }
  }

  free(sum->msg_stat_htab);
}

/* Stores index of collective operation statistic `stat_idx' under hash code
   input `rid',`cid', */

static void hash_put_collop(VTSum* sum, uint32_t rid, uint32_t cid,
			    uint64_t stat_idx) {
  uint32_t id;
  VTSum_collopHashNode* add;

  id = vt_hashtriple(rid, cid, 0, 0) & (VTSUM_HASH_MAX - 1);

  add = (VTSum_collopHashNode*)malloc(sizeof(VTSum_collopHashNode));
  add->rid        = rid;
  add->cid        = cid;
  add->stat_idx   = stat_idx;
  add->next       = sum->collop_stat_htab[id];
  sum->collop_stat_htab[id] = add;
}

/* Lookup hash code `h'
 * Returns hash table entry if already stored, otherwise NULL */

static VTSum_collopHashNode* hash_get_collop(VTSum* sum, uint32_t rid,
					     uint32_t cid) {
  uint32_t id;
  VTSum_collopHashNode* curr;

  id = vt_hashtriple(rid, cid, 0, 0) & (VTSUM_HASH_MAX - 1);

  curr = sum->collop_stat_htab[id];
  while ( curr ) {
    if ( curr->rid == rid &&
	 curr->cid == cid ) {
       return curr;
    }
    curr = curr->next;
  }
  return NULL;
}

/* Clear hash table for collective operation statistics */

static void hash_clear_collop(VTSum* sum) {
  int i;
  VTSum_collopHashNode* tmp;

  if (!sum->collop_stat_htab) return;

  for (i = 0; i < VTSUM_HASH_MAX; i++)
  {
    while( sum->collop_stat_htab[i] )
    {
      tmp = sum->collop_stat_htab[i]->next;
      free(sum->collop_stat_htab[i]);
      sum->collop_stat_htab[i] = tmp;
    }
  }

  free(sum->collop_stat_htab);
}

/* Stores index of file operation statistic `stat_idx' under hash code `h' */

static void hash_put_fileop(VTSum* sum, uint32_t h, uint64_t stat_idx) {
  uint32_t id = h & (VTSUM_HASH_MAX - 1);
  VTSum_fileopHashNode* add =
    (VTSum_fileopHashNode*)malloc(sizeof(VTSum_fileopHashNode));
  add->id       = h;
  add->stat_idx = stat_idx;
  add->next     = sum->fileop_stat_htab[id];
  sum->fileop_stat_htab[id] = add;
}

/* Lookup hash code `h'
 * Returns hash table entry if already stored, otherwise NULL */

static VTSum_fileopHashNode* hash_get_fileop(VTSum* sum, uint32_t h) {
  uint32_t id = h & (VTSUM_HASH_MAX - 1);
  VTSum_fileopHashNode* curr = sum->fileop_stat_htab[id];
  while ( curr ) {
    if ( curr->id == h ) {
      return curr;
    }
    curr = curr->next;
  }
  return NULL;
}

/* Clear hash table for file operation statistics */

static void hash_clear_fileop(VTSum* sum) {
  int i;
  VTSum_fileopHashNode* tmp;

  if (!sum->fileop_stat_htab) return;

  for (i = 0; i < VTSUM_HASH_MAX; i++)
  {
    while( sum->fileop_stat_htab[i] )
    {
      tmp = sum->fileop_stat_htab[i]->next;
      free(sum->fileop_stat_htab[i]);
      sum->fileop_stat_htab[i] = tmp;
    }
  }

  free(sum->fileop_stat_htab);
}

VTSum* VTSum_open(VTGen* gen, uint32_t tid)
{
  VTSum* sum;
  uint32_t intv = (uint32_t)vt_env_stat_intv();

  /* allocate VTSum record */

  sum = (VTSum*)malloc(sizeof(VTSum));
  if (sum == NULL) 
    vt_error();

  /* set pointer to corresponding VTGen record */
  sum->gen = gen;

  /* initialize statistics properties */
  sum->props = (uint8_t)vt_env_stat_props();

  if (VTSUM_IS_PROP_ON(sum, VT_SUM_PROP_FUNC))
  {
    /* initialize function statistics */

    sum->func_stat = (VTSum_funcStat*)malloc(VTSUM_STAT_BSIZE
					     * sizeof(VTSum_funcStat));
    if (sum->func_stat == NULL)
      vt_error();
    sum->func_stat_size = VTSUM_STAT_BSIZE;
    sum->func_stat_num = 0;

    /* initialize hash table for function statistics */

    sum->func_stat_htab =
      (VTSum_funcHashNode**)calloc(VTSUM_HASH_MAX,
				   sizeof(VTSum_funcHashNode*));
    if (sum->func_stat_htab == NULL)
      vt_error();

    /* initialize call stack */

    sum->stack = (VTSum_stack*)malloc(VTSUM_STACK_BSIZE * sizeof(VTSum_stack));
    if (sum->stack == NULL)
      vt_error();
    sum->stack_size = VTSUM_STACK_BSIZE;
    sum->stack_pos = -1;
  }

  if (VTSUM_IS_PROP_ON(sum, VT_SUM_PROP_MSG))
  {
    /* initialize message statistics */
     
    sum->msg_stat = (VTSum_msgStat*)malloc(VTSUM_STAT_BSIZE
					   * sizeof(VTSum_msgStat));
    if (sum->msg_stat == NULL)
      vt_error();
    sum->msg_stat_size = VTSUM_STAT_BSIZE;
    sum->msg_stat_num = 0;

    /* initialize hash table for message statistics */
    
    sum->msg_stat_htab =
      (VTSum_msgHashNode**)calloc(VTSUM_HASH_MAX,
				  sizeof(VTSum_msgHashNode*));
    if (sum->msg_stat_htab == NULL)
      vt_error();

    /* initialize message statistics details */

    sum->msg_stat_dtls = (uint8_t)vt_env_stat_msg_dtls();
  }

  if (VTSUM_IS_PROP_ON(sum, VT_SUM_PROP_COLLOP))
  {
    /* initialize collective operation statistics */
  
    sum->collop_stat = (VTSum_collopStat*)malloc(VTSUM_STAT_BSIZE
						 * sizeof(VTSum_collopStat));
    if (sum->collop_stat == NULL)
      vt_error();
    sum->collop_stat_size = VTSUM_STAT_BSIZE;
    sum->collop_stat_num = 0;

    /* initialize hash table for collective operation statistics */
    
    sum->collop_stat_htab =
      (VTSum_collopHashNode**)calloc(VTSUM_HASH_MAX,
				     sizeof(VTSum_collopHashNode*));
    if (sum->collop_stat_htab == NULL)
      vt_error();

    /* initialize collective operation statistics details */

    sum->collop_stat_dtls = (uint8_t)vt_env_stat_collop_dtls();
  }

  if (VTSUM_IS_PROP_ON(sum, VT_SUM_PROP_FILEOP))
  {
    /* initialize file operation statistics */

    sum->fileop_stat = (VTSum_fileopStat*)malloc(VTSUM_STAT_BSIZE
						 * sizeof(VTSum_fileopStat));
    if (sum->fileop_stat == NULL)
      vt_error();
    sum->fileop_stat_size = VTSUM_STAT_BSIZE;
    sum->fileop_stat_num = 0;

    /* initialize hash table for file operation statistics */

    sum->fileop_stat_htab =
      (VTSum_fileopHashNode**)calloc(VTSUM_HASH_MAX,
				     sizeof(VTSum_fileopHashNode*));
    if (sum->fileop_stat_htab == NULL)
      vt_error();
  }

  /* set thread id */
  sum->tid = tid;

  /* set summary interval */

  if (intv > 0)
  {
    SumIntv = (vt_pform_clockres() * intv) / 1000;
    sum->next_dump = vt_pform_wtime() + SumIntv;
  }
  else
  {
    sum->next_dump = (uint64_t)-1;
  }

  /* return */
  return sum;
}

void VTSum_dump(VTSum* sum, uint64_t* time, uint8_t markDump)
{
  uint32_t i;

  /* mark begin of statistics dump */
  if (markDump)
    vt_enter_stat(sum->tid, time);

  /* dump function statistics */

  if (VTSUM_IS_PROP_ON(sum, VT_SUM_PROP_FUNC))
  {
    for(i = 0; i < sum->func_stat_num; i++)
    {
      VTGen_write_FUNCTION_SUMMARY(sum->gen, time,
	sum->func_stat[i].rid,
	sum->func_stat[i].cnt,
	sum->func_stat[i].excl,
	sum->func_stat[i].incl);
    }
  }

  /* dump message statistics */

  if (VTSUM_IS_PROP_ON(sum, VT_SUM_PROP_MSG))
  {
    for(i = 0; i < sum->msg_stat_num; i++)
    {
      VTGen_write_MESSAGE_SUMMARY(sum->gen, time,
	sum->msg_stat[i].peer,
	sum->msg_stat[i].cid,
	sum->msg_stat[i].tag,
	sum->msg_stat[i].scnt,
	sum->msg_stat[i].rcnt,
	sum->msg_stat[i].sent,
	sum->msg_stat[i].recvd);
    }
  }

  /* dump collective operation statistics */

  if (VTSUM_IS_PROP_ON(sum, VT_SUM_PROP_COLLOP))
  {
    for(i = 0; i < sum->collop_stat_num; i++)
    {
      VTGen_write_COLLECTIVE_OPERATION_SUMMARY(sum->gen, time,
	sum->collop_stat[i].cid,
	sum->collop_stat[i].rid,
	sum->collop_stat[i].scnt,
	sum->collop_stat[i].rcnt,
	sum->collop_stat[i].sent,
	sum->collop_stat[i].recvd);
    }
  }

  /* dump file operation statistics */

  if (VTSUM_IS_PROP_ON(sum, VT_SUM_PROP_FILEOP))
  {
    for(i = 0; i < sum->fileop_stat_num; i++)
    {
      VTGen_write_FILE_OPERATION_SUMMARY(sum->gen, time,
	sum->fileop_stat[i].fid,
        sum->fileop_stat[i].nopen,
        sum->fileop_stat[i].nclose,
	sum->fileop_stat[i].nread,
	sum->fileop_stat[i].nwrite,
	sum->fileop_stat[i].nseek,
	sum->fileop_stat[i].read,
	sum->fileop_stat[i].wrote);
    }
  }

  *time = vt_pform_wtime();

  /* mark end of statistics dump */
  if (markDump)
    vt_exit_stat(sum->tid, time);

  if (sum->next_dump != (uint64_t)-1)
    sum->next_dump = *time + SumIntv;
}

void VTSum_close(VTSum* sum)
{
  /* dump statistics */
  uint64_t time = vt_pform_wtime();
  VTSum_dump(sum, &time, 0);
}

void VTSum_delete(VTSum* sum)
{
  if (VTSUM_IS_PROP_ON(sum, VT_SUM_PROP_FUNC))
  {
    /* free function statistics */
    free(sum->func_stat);

    /* free hash table for function statistics */
    hash_clear_func(sum);

    /* free call stack */
    free(sum->stack);
  }

  if (VTSUM_IS_PROP_ON(sum, VT_SUM_PROP_MSG))
  {
    /* free message statistics */
    free(sum->msg_stat);

    /* free hash table for message statistics */
    hash_clear_msg(sum);
  }

  if (VTSUM_IS_PROP_ON(sum, VT_SUM_PROP_COLLOP))
  {
    /* free collective operation statistics */
    free(sum->collop_stat);

    /* free hash table for collective operation statistics */
    hash_clear_collop(sum);
  }

  if (VTSUM_IS_PROP_ON(sum, VT_SUM_PROP_FILEOP))
  {
    /* free file operation statistics */
    free(sum->fileop_stat);

    /* free hash table for file operation statistics */
    hash_clear_fileop(sum);
  }

  /* free sum record */
  free(sum);
}


/* -- Region -- */

void VTSum_enter(VTSum* sum, uint64_t* time, uint32_t rid)
{
  uint64_t stat_idx;
  VTSum_funcHashNode* hn;

  VTSUM_CHECK(sum);

  if ( (hn = hash_get_func(sum, rid)) )
  {
    stat_idx = hn->stat_idx;
  }
  else
  {
    VTSUM_FUNC_STAT_ADD(sum, rid, stat_idx);
    hash_put_func(sum, rid, stat_idx);
  }

  VTSUM_STACK_PUSH(sum, stat_idx, time);

  VT_CHECK_DUMP(sum, time);
}

void VTSum_exit(VTSum* sum, uint64_t* time, uint32_t rid)
{
  VTSUM_CHECK(sum);

  VTSUM_STACK_POP(sum, time);

  VT_CHECK_DUMP(sum, time);
}


/* -- Message -- */

#define VTSum_msg(_sum, _peer, _cid, _tag, _stat_idx)       \
{                                                           \
  VTSum_msgHashNode* hn;                                    \
                                                            \
  if ( (hn = hash_get_msg(_sum, _peer, _cid, _tag)) )       \
  {                                                         \
    _stat_idx = hn->stat_idx;                               \
  }                                                         \
  else                                                      \
  {                                                         \
    VTSUM_MSG_STAT_ADD(_sum, _peer, _cid, _tag, _stat_idx); \
    hash_put_msg(_sum, _peer, _cid, _tag, _stat_idx);       \
  }                                                         \
}

void VTSum_msg_send(VTSum* sum, uint64_t* time, uint32_t dpid, uint32_t cid,
		    uint32_t tag, uint64_t sent)
{
  uint64_t stat_idx;
  uint32_t ldpid;
  uint32_t lcid;
  uint32_t ltag;

  VTSUM_CHECK(sum);
  
  ldpid = VTSUM_IS_MSG_DTL_ON(sum, VT_SUM_MSG_DTL_PEER) ? dpid : 0;
  lcid  = VTSUM_IS_MSG_DTL_ON(sum, VT_SUM_MSG_DTL_COMM) ? cid  : 0;
  ltag  = VTSUM_IS_MSG_DTL_ON(sum, VT_SUM_MSG_DTL_TAG)  ? tag  : 0;

  VTSum_msg(sum, ldpid, lcid, ltag, stat_idx);

  sum->msg_stat[stat_idx].scnt++;
  sum->msg_stat[stat_idx].sent += sent;
  
  VT_CHECK_DUMP(sum, time);
}

void VTSum_msg_recv(VTSum* sum, uint64_t* time, uint32_t spid, uint32_t cid,
		    uint32_t tag, uint64_t recvd)
{
  uint64_t stat_idx;
  uint32_t lspid;
  uint32_t lcid;
  uint32_t ltag;

  VTSUM_CHECK(sum);
  
  lspid = VTSUM_IS_MSG_DTL_ON(sum, VT_SUM_MSG_DTL_PEER) ? spid : 0;
  lcid  = VTSUM_IS_MSG_DTL_ON(sum, VT_SUM_MSG_DTL_COMM) ? cid  : 0;
  ltag  = VTSUM_IS_MSG_DTL_ON(sum, VT_SUM_MSG_DTL_TAG)  ? tag  : 0;

  VTSum_msg(sum, lspid, lcid, ltag, stat_idx);

  sum->msg_stat[stat_idx].rcnt++;
  sum->msg_stat[stat_idx].recvd += recvd;
  
  VT_CHECK_DUMP(sum, time);
}


/* -- Collop -- */

void VTSum_collop(VTSum* sum, uint64_t* time, uint32_t rid, uint32_t cid,
		  uint64_t sent, uint64_t recvd)
{
  uint64_t stat_idx;
  uint32_t lrid;
  uint32_t lcid;
  VTSum_collopHashNode* hn;

  VTSUM_CHECK(sum);

  lrid = VTSUM_IS_COLLOP_DTL_ON(sum, VT_SUM_COLLOP_DTL_OP)   ? rid : 0;
  lcid = VTSUM_IS_COLLOP_DTL_ON(sum, VT_SUM_COLLOP_DTL_COMM) ? cid : 0;

  if ( (hn = hash_get_collop(sum, lrid, lcid)) )
  {
    stat_idx = hn->stat_idx;
  }
  else
  {
    VTSUM_COLLOP_STAT_ADD(sum, lrid, lcid, stat_idx);
    hash_put_collop(sum, lrid, lcid, stat_idx);
  }
 
  if ( sent == 0 && recvd == 0 ) /* no bytes => barrier */
  {
    sum->collop_stat[stat_idx].scnt++;
    sum->collop_stat[stat_idx].rcnt++;
  }
  else
  {
    if ( sent > 0 )
    {
      sum->collop_stat[stat_idx].scnt++;
      sum->collop_stat[stat_idx].sent += sent;
    }
    if ( recvd > 0 )
    {
      sum->collop_stat[stat_idx].rcnt++;
      sum->collop_stat[stat_idx].recvd += recvd;
    }
  }

  VT_CHECK_DUMP(sum, time);
}


/* -- File I/O -- */

#define VTSum_fileop(_sum, _fid, _stat_idx)                 \
{                                                           \
  VTSum_fileopHashNode* hn;                                 \
                                                            \
  if ( (hn = hash_get_fileop(_sum, _fid)) )                 \
  {                                                         \
    _stat_idx = hn->stat_idx;                               \
  }                                                         \
  else                                                      \
  {                                                         \
    VTSUM_FILEOP_STAT_ADD(_sum, _fid, _stat_idx);           \
    hash_put_fileop(_sum, _fid, _stat_idx);                 \
  }                                                         \
}

void VTSum_fileop_open(VTSum* sum, uint64_t* time, uint32_t fid)
{
  uint64_t stat_idx;

  VTSUM_CHECK(sum);

  VTSum_fileop(sum, fid, stat_idx);

  sum->fileop_stat[stat_idx].nopen++;

  VT_CHECK_DUMP(sum, time);
}

void VTSum_fileop_close(VTSum* sum, uint64_t* time, uint32_t fid)
{
  uint64_t stat_idx;

  VTSUM_CHECK(sum);

  VTSum_fileop(sum, fid, stat_idx);

  sum->fileop_stat[stat_idx].nclose++;

  VT_CHECK_DUMP(sum, time);
}

void VTSum_fileop_read(VTSum* sum, uint64_t* time, uint32_t fid, uint64_t read)
{
  uint64_t stat_idx;

  VTSUM_CHECK(sum);

  VTSum_fileop(sum, fid, stat_idx);

  sum->fileop_stat[stat_idx].nread++;
  sum->fileop_stat[stat_idx].read += read;

  VT_CHECK_DUMP(sum, time);
}

void VTSum_fileop_write(VTSum* sum, uint64_t* time, uint32_t fid, uint64_t wrote)
{
  uint64_t stat_idx;

  VTSUM_CHECK(sum);

  VTSum_fileop(sum, fid, stat_idx);

  sum->fileop_stat[stat_idx].nwrite++;
  sum->fileop_stat[stat_idx].wrote += wrote;

  VT_CHECK_DUMP(sum, time);
}

void VTSum_fileop_seek(VTSum* sum, uint64_t* time, uint32_t fid)
{
  uint64_t stat_idx;

  VTSUM_CHECK(sum);

  VTSum_fileop(sum, fid, stat_idx);

  sum->fileop_stat[stat_idx].nseek++;

  VT_CHECK_DUMP(sum, time);
}
