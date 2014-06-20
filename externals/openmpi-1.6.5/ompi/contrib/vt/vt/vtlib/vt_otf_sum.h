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

#ifndef _VT_OTF_SUM_H
#define _VT_OTF_SUM_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#include <stdlib.h>
#include <stdio.h>

#include "vt_defs.h"

/*
 *-----------------------------------------------------------------------------
 * Typedefs (to make data types opaque)
 *-----------------------------------------------------------------------------
 */

/* Trace summary generated at run time */
typedef struct VTSum_struct VTSum;

/*
 *-----------------------------------------------------------------------------
 * VTSum
 *-----------------------------------------------------------------------------
 */

EXTERN VTSum* VTSum_open              ( VTGen* gen, uint32_t tid );

EXTERN void   VTSum_dump              ( VTSum* sum, uint64_t* time,
                                        uint8_t markDump );

EXTERN void   VTSum_close             ( VTSum* sum );

EXTERN void   VTSum_delete            ( VTSum* sum );

#define       VTSum_destroy(sum)      VTSum_delete( sum )

/* -- Region -- */

EXTERN void   VTSum_enter             ( VTSum* sum, uint64_t* time,
					uint32_t rid );

EXTERN void   VTSum_exit              ( VTSum* sum, uint64_t* time,
					uint32_t rid );

/* -- Message -- */

EXTERN void   VTSum_msg_send          ( VTSum* sum, uint64_t* time,
					uint32_t dpid, uint32_t cid,
					uint32_t tag, uint64_t sent );

EXTERN void   VTSum_msg_recv          ( VTSum* sum, uint64_t* time,
					uint32_t spid, uint32_t cid,
					uint32_t tag, uint64_t recvd );

/* -- Collop -- */

EXTERN void   VTSum_collop            ( VTSum*sum, uint64_t* time,
					uint32_t rid, uint32_t cid,
					uint64_t sent, uint64_t recvd );

/* -- File I/O -- */

EXTERN void   VTSum_fileop_open       ( VTSum* sum, uint64_t* time,
					uint32_t fid );

EXTERN void   VTSum_fileop_close      ( VTSum* sum, uint64_t* time,
					uint32_t fid );

EXTERN void   VTSum_fileop_read       ( VTSum* sum, uint64_t* time,
					uint32_t fid, uint64_t read );

EXTERN void   VTSum_fileop_write      ( VTSum* sum, uint64_t* time,
					uint32_t fid, uint64_t wrote );

EXTERN void   VTSum_fileop_seek       ( VTSum* sum, uint64_t* time,
					uint32_t fid );

#endif /* _VT_OTF_SUM_H */
