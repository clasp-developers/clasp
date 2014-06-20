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

#ifndef _VT_MPIREQ_H
#define _VT_MPIREQ_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#include "vt_defs.h"
#include "mpi.h"

enum VTReqFlags {
  ERF_NONE = 0x00,
  ERF_SEND = 0x01,
  ERF_RECV = 0x02,
  ERF_IO   = 0x04,
  ERF_IS_PERSISTENT = 0x10,
  ERF_DEALLOCATE = 0x20,
  ERF_IS_ACTIVE  = 0x40
};

struct VTRequest {
  MPI_Request request;
  unsigned flags;
  int tag;
  int dest;
  int bytes;
  MPI_Datatype datatype;
  MPI_Group group;
  uint32_t cid;

  uint64_t matchingid;
  uint64_t handleid;
  uint32_t fileid;
  uint32_t fileop;
  uint32_t ioflags;
};

EXTERN void vt_request_finalize(void);
EXTERN void vt_request_create(MPI_Request request, 
			      unsigned flags, int tag, int dest, int bytes,
			      MPI_Datatype datatype, MPI_Comm comm);
EXTERN void vt_iorequest_create( MPI_Request request,
                                 MPI_Datatype datatype,
				 uint64_t matchingid,
				 uint64_t handleid,
				 uint32_t fileid,
				 uint32_t flags );
EXTERN struct VTRequest* vt_request_get(MPI_Request request);
EXTERN void vt_request_free(struct VTRequest* req);
EXTERN void vt_check_request(uint32_t tid, uint64_t* time,
                             struct VTRequest* req, MPI_Status *status,
                             uint8_t record_event);
EXTERN void vt_save_request_array(MPI_Request *arr_req, int arr_req_size);
EXTERN struct VTRequest* vt_saved_request_get(int i);

#endif
