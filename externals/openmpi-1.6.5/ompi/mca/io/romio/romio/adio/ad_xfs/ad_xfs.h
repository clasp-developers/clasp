/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#ifndef AD_XFS_INCLUDE
#define AD_XFS_INCLUDE

#define _XOPEN_SOURCE 500
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include "adio.h"

#if defined(MPISGI)
#include "xfs/xfs_fs.h"
#ifndef  __USE_LARGEFILE64
#define  __USE_LARGEFILE64
#endif
typedef struct aiocb64 aiocb64_t;
#endif

void ADIOI_XFS_Open(ADIO_File fd, int *error_code);
void ADIOI_XFS_Close(ADIO_File fd, int *error_code);
void ADIOI_XFS_ReadContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                     ADIO_Offset offset, ADIO_Status *status, int
		     *error_code);
void ADIOI_XFS_WriteContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Status *status, int
		      *error_code);   
void ADIOI_XFS_Fcntl(ADIO_File fd, int flag, ADIO_Fcntl_t *fcntl_struct, int
		*error_code);
void ADIOI_XFS_Resize(ADIO_File fd, ADIO_Offset size, int *error_code);
void ADIOI_XFS_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code);

#endif
