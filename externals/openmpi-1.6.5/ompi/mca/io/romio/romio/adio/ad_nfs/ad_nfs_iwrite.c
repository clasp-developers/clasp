/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_nfs.h"

#include "../../mpi-io/mpioimpl.h"
#include "../../mpi-io/mpioprof.h"
#include "mpiu_greq.h"

#include <string.h>

#ifdef ROMIO_HAVE_WORKING_AIO
static MPIX_Grequest_class ADIOI_GEN_greq_class = 0;
/* this routine is nearly identical to ADIOI_GEN_IwriteContig, except we lock
 * around I/O */
void ADIOI_NFS_IwriteContig(ADIO_File fd, void *buf, int count, 
                MPI_Datatype datatype, int file_ptr_type,
                ADIO_Offset offset, ADIO_Request *request, int *error_code)  
{
    int len, typesize;
    int aio_errno = 0;
    static char myname[] = "ADIOI_NFS_IWRITECONTIG";

    MPI_Type_size(datatype, &typesize);
    len = count * typesize;

    if (file_ptr_type == ADIO_INDIVIDUAL) offset = fd->fp_ind;
    aio_errno = ADIOI_NFS_aio(fd, buf, len, offset, 1, request);
    if (file_ptr_type == ADIO_INDIVIDUAL) fd->fp_ind += len;

    fd->fp_sys_posn = -1;

    if (aio_errno != 0) {
	/* --BEGIN ERROR HANDLING-- */
	MPIO_ERR_CREATE_CODE_ERRNO(myname, aio_errno, error_code);
	return;
	/* --END ERROR HANDLING-- */
    }
    else *error_code = MPI_SUCCESS;
    return;
}
#endif

/* This function is for implementation convenience. It is not user-visible.
 * It takes care of the differences in the interface for nonblocking I/O
 * on various Unix machines! If wr==1 write, wr==0 read.
 *
 * Returns 0 on success, -errno on failure.
 */
#ifdef ROMIO_HAVE_WORKING_AIO
int ADIOI_NFS_aio(ADIO_File fd, void *buf, int len, ADIO_Offset offset,
		  int wr, MPI_Request *request)
{
    int err=-1, fd_sys;
    int error_code, this_errno;

    struct aiocb *aiocbp;
    ADIOI_AIO_Request *aio_req;
    MPI_Status status;

    fd_sys = fd->fd_sys;

    aio_req = (ADIOI_AIO_Request*)ADIOI_Calloc(sizeof(ADIOI_AIO_Request), 1);
    aiocbp = (struct aiocb *) ADIOI_Calloc(sizeof(struct aiocb), 1);
    aiocbp->aio_offset = offset;
    aiocbp->aio_buf    = buf;
    aiocbp->aio_nbytes = len;

#ifdef ROMIO_HAVE_STRUCT_AIOCB_WITH_AIO_WHENCE
    aiocbp->aio_whence = SEEK_SET;
#endif
#ifdef ROMIO_HAVE_STRUCT_AIOCB_WITH_AIO_FILDES
    aiocbp->aio_fildes = fd_sys;
#endif
#ifdef ROMIO_HAVE_STRUCT_AIOCB_WITH_AIO_SIGEVENT
# ifdef AIO_SIGNOTIFY_NONE
    aiocbp->aio_sigevent.sigev_notify = SIGEV_NONE;
# endif
    aiocbp->aio_sigevent.sigev_signo = 0;
#endif
#ifdef ROMIO_HAVE_STRUCT_AIOCB_WITH_AIO_REQPRIO
# ifdef AIO_PRIO_DFL
    aiocbp->aio_reqprio = AIO_PRIO_DFL;   /* not needed in DEC Unix 4.0 */
# else
    aiocbp->aio_reqprio = 0;
# endif
#endif

    if (wr) ADIOI_WRITE_LOCK(fd, offset, SEEK_SET, len);
    else ADIOI_READ_LOCK(fd, offset, SEEK_SET, len);

#ifndef ROMIO_HAVE_AIO_CALLS_NEED_FILEDES
    if (wr) err = aio_write(aiocbp);
    else err = aio_read(aiocbp);
#else
    /* Broken IBM interface */
    if (wr) err = aio_write(fd_sys, aiocbp);
    else err = aio_read(fd_sys, aiocbp);
#endif

    this_errno = errno;
    ADIOI_UNLOCK(fd, offset, SEEK_SET, len);

    if (err == -1) {
	if (this_errno == EAGAIN) {
        /* exceeded the max. no. of outstanding requests.
           complete all previous async. requests and try again. */
	    ADIO_WriteContig(fd, buf, len, MPI_BYTE, ADIO_EXPLICIT_OFFSET,
			    offset, &status, &error_code);
	    MPIO_Completed_request_create(&fd, len, &error_code, request);
	    return 0;
	} else {
	    return -this_errno;
	}
    }
    aio_req->aiocbp = aiocbp;
    if (ADIOI_GEN_greq_class == 0) {
	    MPIX_Grequest_class_create(ADIOI_GEN_aio_query_fn, 
			    ADIOI_GEN_aio_free_fn, MPIU_Greq_cancel_fn, 
			    ADIOI_GEN_aio_poll_fn, ADIOI_GEN_aio_wait_fn, 
			    &ADIOI_GEN_greq_class);
    }
    MPIX_Grequest_class_allocate(ADIOI_GEN_greq_class, aio_req, request);
    memcpy(&(aio_req->req), request, sizeof(MPI_Request));
    return 0;
}
#endif
