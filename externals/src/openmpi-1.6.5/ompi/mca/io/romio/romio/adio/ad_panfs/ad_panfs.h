/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   ad_panfs.h
 *
 *   Copyright (C) 2001 University of Chicago.
 *   See COPYRIGHT notice in top-level directory.
 */

#ifndef AD_PANFS_INCLUDE
#define AD_PANFS_INCLUDE

#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include "adio.h"

#ifndef NO_AIO
#ifdef AIO_SUN
#include <sys/asynch.h>
#else
#include <aio.h>
#ifdef NEEDS_ADIOCB_T
typedef struct adiocb adiocb_t;
#endif
#endif
#endif

void ADIOI_PANFS_Open(ADIO_File fd, int *error_code);
void ADIOI_PANFS_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code);
void ADIOI_PANFS_ReadContig(ADIO_File fd, void *buf, int count, 
			  MPI_Datatype datatype, int file_ptr_type,
			  ADIO_Offset offset, ADIO_Status *status,
			  int *error_code);
void ADIOI_PANFS_Resize(ADIO_File fd, ADIO_Offset size, int *error_code);
void ADIOI_PANFS_WriteContig(ADIO_File fd, void *buf, int count, 
			   MPI_Datatype datatype, int file_ptr_type,
			   ADIO_Offset offset, ADIO_Status *status,
			   int *error_code);

/* Delay 1 ms */
#define AD_PANFS_RETRY_DELAY 1000

#define AD_PANFS_RETRY(_op_,_rc_) \
{ \
    _rc_ = (_op_); \
    while(_rc_ == -1 && errno == EAGAIN) \
    { \
        if(usleep(AD_PANFS_RETRY_DELAY) == -1) \
        { \
            break; \
        } \
        _rc_ = (_op_); \
    } \
}

#endif
