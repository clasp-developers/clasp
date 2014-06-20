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

#include "vt_defs.h"
#include "vt_error.h"
#include "vt_inttypes.h"
#include "vt_mpireq.h"
#include "vt_mpicom.h"
#include "vt_trc.h"

/* 
 *-----------------------------------------------------------------------------
 *
 * Request management
 *
 *-----------------------------------------------------------------------------
 */

#define VT_REQBLK_SIZE 10

struct VTRequestBlock {
  struct VTRequest req[VT_REQBLK_SIZE];
  struct VTRequestBlock *next;
  struct VTRequestBlock *prev;
};

static struct VTRequestBlock *head_block = 0;
static struct VTRequestBlock *last_block = 0;
static struct VTRequest *lastreq = 0;
static int lastidx = VT_REQBLK_SIZE;

void vt_request_finalize()
{
  struct VTRequestBlock *block;

  /* free request blocks */

  while (head_block) {
    block = head_block;
    head_block = head_block->next;
    free(block);
  }
}

void vt_request_create(MPI_Request request, 
			unsigned flags,
			int tag, 
			int dest, 
			int bytes, 
			MPI_Datatype datatype,
			MPI_Comm comm)
{
  struct VTRequestBlock *new_block;

  MPI_Datatype type;
  MPI_Group group;
  VT_MPI_INT intercomm;

  lastidx++;
  if (lastidx >= VT_REQBLK_SIZE) 
    {
      if (head_block == 0 ) 
	{
	  /* first time: allocate and initialize first block */
	  new_block = (struct VTRequestBlock*)malloc(sizeof(struct VTRequestBlock));
	  new_block->next = 0;
	  new_block->prev = 0;
	  head_block = last_block = new_block;
	} 
      else if (last_block == 0 ) 
	{
	  /* request list empty: re-initialize */
	  last_block = head_block;
	} 
      else 
	{
	  if (last_block->next == 0 ) 
	    {
	      /* request list full: expand */
	      new_block = (struct VTRequestBlock*)malloc(sizeof(struct VTRequestBlock));
	      new_block->next = 0;
	      new_block->prev = last_block;
	      last_block->next = new_block;
	    }
	  /* use next available block */
	  last_block = last_block->next;
	}
      lastreq = &(last_block->req[0]);
      lastidx  = 0;
    } 
  else 
    {
      lastreq++;
    }

  /* ask for group of comm */
  PMPI_Comm_test_inter(comm, &intercomm);
  if (intercomm)
    PMPI_Comm_remote_group(comm, &group);
  else
    PMPI_Comm_group(comm, &group);

  /* duplicate data type due to it could be freed before the communication
     is completed */
#if defined(HAVE_MPI_TYPE_DUP) && HAVE_MPI_TYPE_DUP
  PMPI_Type_dup(datatype, &type);
#else /* HAVE_MPI_TYPE_DUP */
  type = datatype;
#endif /* HAVE_MPI_TYPE_DUP */

  /* store request information */
  lastreq->request  = request;
  lastreq->flags    = ERF_NONE;
  lastreq->flags   |= flags;
  lastreq->tag      = tag;
  lastreq->dest     = dest;
  lastreq->bytes    = bytes;
  lastreq->datatype = type;
  lastreq->group    = group;
  lastreq->cid      = VT_COMM_ID(comm);
}

void vt_iorequest_create( MPI_Request request,
                          MPI_Datatype datatype,
                          uint64_t matchingid,
			  uint64_t handleid,
			  uint32_t fileid,
			  uint32_t fileop )
{
  struct VTRequestBlock *new_block;

  MPI_Datatype type;

  lastidx++;
  if (lastidx >= VT_REQBLK_SIZE) 
    {
      if (head_block == 0 ) 
	{
	  /* first time: allocate and initialize first block */
	  new_block = (struct VTRequestBlock*)malloc(sizeof(struct VTRequestBlock));
	  new_block->next = 0;
	  new_block->prev = 0;
	  head_block = last_block = new_block;
	} 
      else if (last_block == 0 ) 
	{
	  /* request list empty: re-initialize */
	  last_block = head_block;
	} 
      else 
	{
	  if (last_block->next == 0 ) 
	    {
	      /* request list full: expand */
	      new_block = (struct VTRequestBlock*)malloc(sizeof(struct VTRequestBlock));
	      new_block->next = 0;
	      new_block->prev = last_block;
	      last_block->next = new_block;
	    }
	  /* use next available block */
	  last_block = last_block->next;
	}
      lastreq = &(last_block->req[0]);
      lastidx  = 0;
    } 
  else 
    {
      lastreq++;
    }

  /* duplicate data type due to it could be freed before the I/O operation
     is completed */
#if defined(HAVE_MPI_TYPE_DUP) && HAVE_MPI_TYPE_DUP
  PMPI_Type_dup(datatype, &type);
#else /* HAVE_MPI_TYPE_DUP */
  type = datatype;
#endif /* HAVE_MPI_TYPE_DUP */

  /* store request information */
  lastreq->request  = request;
  lastreq->datatype = type;
  lastreq->flags    = ERF_IO;
  lastreq->matchingid = matchingid;
  lastreq->handleid = handleid;
  lastreq->fileid   = fileid;
  lastreq->fileop   = fileop;
}

struct VTRequest* vt_request_get(MPI_Request request)
{
  int i;
  struct VTRequestBlock *block;
  struct VTRequest *curr;

  /* list empty */
  if (!lastreq) return 0;

  /* search all requests in all blocks */
  block = head_block;
  while (block) {
    curr = &(block->req[0]);
    for (i = 0; i < VT_REQBLK_SIZE; ++i) 
      {
	/* found? */
	if (curr->request == request) 
	  return curr;

	/* end of list? */
	if (curr == lastreq)
	  return 0;

	curr++;
      }
    block = block->next;
  }
  return 0;
}

void vt_request_free(struct VTRequest* req)
{
#if defined(HAVE_MPI_TYPE_DUP) && HAVE_MPI_TYPE_DUP
  /* since the stored data type was duplicated on request creation, free them */
  PMPI_Type_free(&(req->datatype));
#endif /* HAVE_MPI_TYPE_DUP */

  /* delete request by copying last request in place of req */ 
  if (!lastreq) {
    vt_error_msg("INTERNAL ERROR in request handling - no last request");
  }
  *req = *lastreq;
  lastreq->flags = ERF_NONE;
  lastreq->request = 0;

  /* adjust pointer to last request */
  lastidx--;
  if (lastidx < 0) 
    {
      /* reached low end of block */
      if (last_block->prev) 
	{
	  /* goto previous block if existing */
	  lastidx = VT_REQBLK_SIZE-1;
	  lastreq = &(last_block->prev->req[lastidx]);
	} 
      else 
	{
	  /* no previous block: re-initialize */
	  lastidx = VT_REQBLK_SIZE;
	  lastreq = 0;
	}
      last_block = last_block->prev;
    } 
  else 
    {
      lastreq--;
    }  
}

void vt_check_request(uint32_t tid, uint64_t* time, struct VTRequest* req,
                      MPI_Status *status, uint8_t record_event)
{
  if (!req ||
      ((req->flags & ERF_IS_PERSISTENT) && !(req->flags & ERF_IS_ACTIVE)))
    return;

  /* if receive request, write receive trace record */
  if (record_event &&
      (req->flags & ERF_RECV) &&
      (status->MPI_SOURCE != MPI_PROC_NULL) && 
      (status->MPI_SOURCE != MPI_ANY_SOURCE))
  {
    VT_MPI_INT count, sz;
    PMPI_Type_size(req->datatype, &sz);
    PMPI_Get_count(status, req->datatype, &count);
    vt_mpi_recv(tid, time,
                VT_RANK_TO_PE_BY_GROUP(status->MPI_SOURCE, req->group),
                req->cid, status->MPI_TAG, count * sz);
  }

  if (record_event && (req->flags & ERF_IO))
  {
    VT_MPI_INT count, sz;
    PMPI_Type_size(req->datatype, &sz);
    PMPI_Get_count(status, req->datatype, &count);
    if (count == MPI_UNDEFINED)
      count = 0;
    vt_ioend(tid, time, req->fileid, req->matchingid, req->handleid, req->fileop,
             (uint64_t)count*(uint64_t)sz);
  }

  if (req->flags & ERF_IS_PERSISTENT)
    {
      /* if persistent request, set to inactive,
         and, if requested delete request */
      req->flags &= ~ERF_IS_ACTIVE;
      if (req->flags & ERF_DEALLOCATE) vt_request_free(req);
    }
  else
    {
      /* if non-persistent request, delete always request */
      vt_request_free(req);
    }
}

static MPI_Request *orig_req_arr = 0;
static int orig_req_arr_size = 0;

void vt_save_request_array(MPI_Request *arr_req, int arr_req_size)
{
  int i;

  if (orig_req_arr_size == 0) {
    /* -- never used: initialize -- */
    orig_req_arr = (MPI_Request*)malloc(arr_req_size * sizeof(MPI_Request));
    orig_req_arr_size = arr_req_size;
  } else if (arr_req_size > orig_req_arr_size) {
    /* -- not enough room: expand -- */
    orig_req_arr = (MPI_Request*)realloc(orig_req_arr, arr_req_size * sizeof(MPI_Request));
    orig_req_arr_size = arr_req_size;
  }

  /* -- copy array -- */
  for (i=0; i<arr_req_size; ++i) orig_req_arr[i] = arr_req[i];
}

struct VTRequest* vt_saved_request_get(int i)
{
  return vt_request_get(orig_req_arr[i]);
}
