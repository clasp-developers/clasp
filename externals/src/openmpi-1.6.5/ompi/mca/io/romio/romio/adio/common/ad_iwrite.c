/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 2004 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_AIO_H
#include <aio.h>
#endif
#ifdef HAVE_SYS_AIO_H
#include <sys/aio.h>
#endif
#include <time.h>

#include "../../mpi-io/mpioimpl.h"
#include "../../mpi-io/mpioprof.h"
#include "mpiu_greq.h"
/* Workaround for incomplete set of definitions if __REDIRECT is not 
   defined and large file support is used in aio.h */
#if !defined(__REDIRECT) && defined(__USE_FILE_OFFSET64)
#define aiocb aiocb64
#endif

#ifdef ROMIO_HAVE_WORKING_AIO

static MPIX_Grequest_class ADIOI_GEN_greq_class = 0;

/* ADIOI_GEN_IwriteContig
 *
 * This code handles only the case where ROMIO_HAVE_WORKING_AIO is 
 * defined. We post an asynchronous I/O operations using the appropriate aio
 * routines.  Otherwise, the ADIOI_Fns_struct will point to the FAKE
 * version.
 */
void ADIOI_GEN_IwriteContig(ADIO_File fd, void *buf, int count, 
			    MPI_Datatype datatype, int file_ptr_type,
			    ADIO_Offset offset, ADIO_Request *request,
			    int *error_code)  
{
    int len, typesize;
    int aio_errno = 0;
    static char myname[] = "ADIOI_GEN_IWRITECONTIG";

    MPI_Type_size(datatype, &typesize);
    len = count * typesize;
    ADIOI_Assert(len == (int)((ADIO_Offset)count * (ADIO_Offset)typesize)); /* the count is an int parm */

    if (file_ptr_type == ADIO_INDIVIDUAL) offset = fd->fp_ind;
    aio_errno = ADIOI_GEN_aio(fd, buf, len, offset, 1, request);
    if (file_ptr_type == ADIO_INDIVIDUAL) fd->fp_ind += len;

    fd->fp_sys_posn = -1;

    /* --BEGIN ERROR HANDLING-- */
    if (aio_errno != 0) {
	MPIO_ERR_CREATE_CODE_ERRNO(myname, aio_errno, error_code);
	return;
    }
    /* --END ERROR HANDLING-- */

    *error_code = MPI_SUCCESS;
}
/* This function is for implementation convenience.
 * It takes care of the differences in the interface for nonblocking I/O
 * on various Unix machines! If wr==1 write, wr==0 read.
 *
 * Returns 0 on success, -errno on failure.
 */
int ADIOI_GEN_aio(ADIO_File fd, void *buf, int len, ADIO_Offset offset,
		  int wr, MPI_Request *request)
{
    int err=-1, fd_sys;

    int error_code;
    struct aiocb *aiocbp;
    ADIOI_AIO_Request *aio_req;
    MPI_Status status;
#if defined(ROMIO_XFS)
    unsigned maxiosz = wr ? fd->hints->fs_hints.xfs.write_chunk_sz :
	    fd->hints->fs_hints.xfs.read_chunk_sz;
#endif /* ROMIO_XFS */

    fd_sys = fd->fd_sys;

#if defined(ROMIO_XFS)
    /* Use Direct I/O if desired and properly aligned */
    if (fd->fns == &ADIO_XFS_operations &&
	 ((wr && fd->direct_write) || (!wr && fd->direct_read)) &&
	 !(((long) buf) % fd->d_mem) && !(offset % fd->d_miniosz) && 
	 !(len % fd->d_miniosz) && (len >= fd->d_miniosz) && 
	 (len <= maxiosz)) {
	    fd_sys = fd->fd_direct;
    }
#endif /* ROMIO_XFS */

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

#ifndef ROMIO_HAVE_AIO_CALLS_NEED_FILEDES
#ifndef ROMIO_HAVE_STRUCT_AIOCB_WITH_AIO_FILDES
#error 'No fildes set for aio structure'
#endif
    if (wr) err = aio_write(aiocbp);
    else err = aio_read(aiocbp);
#else
    /* Broken IBM interface */
    if (wr) err = aio_write(fd_sys, aiocbp);
    else err = aio_read(fd_sys, aiocbp);
#endif

    if (err == -1) {
	if (errno == EAGAIN) {
	    /* exceeded the max. no. of outstanding requests.
	    treat this as a blocking request and return.  */
	    if (wr) 
		ADIO_WriteContig(fd, buf, len, MPI_BYTE, 
			    ADIO_EXPLICIT_OFFSET, offset, &status, &error_code);  
	    else
		ADIO_ReadContig(fd, buf, len, MPI_BYTE,
			    ADIO_EXPLICIT_OFFSET, offset, &status, &error_code);  
		    
	    MPIO_Completed_request_create(&fd, len, &error_code, request);
	    return 0;
	} else {
	    return -errno;
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


/* Generic implementation of IwriteStrided calls the blocking WriteStrided
 * immediately.
 */
void ADIOI_GEN_IwriteStrided(ADIO_File fd, void *buf, int count, 
			     MPI_Datatype datatype, int file_ptr_type,
			     ADIO_Offset offset, MPI_Request *request,
			     int *error_code)
{
    ADIO_Status status;
    int typesize;
    MPI_Offset nbytes=0;

    /* Call the blocking function.  It will create an error code 
     * if necessary.
     */
    ADIO_WriteStrided(fd, buf, count, datatype, file_ptr_type, 
		      offset, &status, error_code);  

    if (*error_code == MPI_SUCCESS) {
	MPI_Type_size(datatype, &typesize);
	nbytes = (MPI_Offset)count * (MPI_Offset)typesize;
    }
    MPIO_Completed_request_create(&fd, nbytes, error_code, request);
}

#ifdef ROMIO_HAVE_WORKING_AIO
/* generic POSIX aio completion test routine */
int ADIOI_GEN_aio_poll_fn(void *extra_state, MPI_Status *status)
{
    ADIOI_AIO_Request *aio_req;
    int errcode=MPI_SUCCESS;

    aio_req = (ADIOI_AIO_Request *)extra_state;

    /* aio_error returns an ERRNO value */
    errno = aio_error(aio_req->aiocbp);
    if (errno == EINPROGRESS) {
	    /* TODO: need to diddle with status somehow */
    }
    else if (errno == ECANCELED) {
	    /* TODO: unsure how to handle this */
    } else if (errno == 0) {
	    int n = aio_return(aio_req->aiocbp);
	    aio_req->nbytes = n;
	    errcode = MPI_Grequest_complete(aio_req->req);
	    /* --BEGIN ERROR HANDLING-- */
	    if (errcode != MPI_SUCCESS) {
		    errcode = MPIO_Err_create_code(MPI_SUCCESS,
				    MPIR_ERR_RECOVERABLE,
				    "ADIOI_GEN_aio_poll_fn", __LINE__,
				    MPI_ERR_IO, "**mpi_grequest_complete",
				    0);
	    }
	    /* --END ERROR HANDLING-- */
    }
    return errcode;
}

/* wait for multiple requests to complete */
int ADIOI_GEN_aio_wait_fn(int count, void ** array_of_states, 
		double timeout, MPI_Status *status)
{
	const struct aiocb **cblist;
	int err, errcode=MPI_SUCCESS;
	int nr_complete=0;
	double starttime;
	struct timespec aio_timer;
	struct timespec *aio_timer_p = NULL;

	ADIOI_AIO_Request **aio_reqlist;
	int i;

	aio_reqlist = (ADIOI_AIO_Request **)array_of_states;

	cblist = (const struct aiocb**) ADIOI_Calloc(count, sizeof(struct aiocb*));

	starttime = MPI_Wtime();
	if (timeout >0) {
	    aio_timer.tv_sec = (time_t)timeout;
	    aio_timer.tv_nsec = timeout - aio_timer.tv_sec;
	    aio_timer_p = &aio_timer;
	}
	for (i=0; i< count; i++)
	{
		cblist[i] = aio_reqlist[i]->aiocbp;
	}

	while(nr_complete < count) {
	    do {
		err = aio_suspend(cblist, count, aio_timer_p);
	    } while (err < 0 && errno == EINTR);
	    if (err == 0) 
	    { /* run through the list of requests, and mark all the completed
		 ones as done */
		for (i=0; i< count; i++)
		{
		    /* aio_error returns an ERRNO value */
		    if (aio_reqlist[i]->aiocbp == NULL) 
			continue;
		    errno = aio_error(aio_reqlist[i]->aiocbp);
		    if (errno == 0) {
			int n = aio_return(aio_reqlist[i]->aiocbp);
			aio_reqlist[i]->nbytes = n;
			errcode = MPI_Grequest_complete(aio_reqlist[i]->req);
			if (errcode != MPI_SUCCESS) {
			    errcode = MPIO_Err_create_code(MPI_SUCCESS,
				    MPIR_ERR_RECOVERABLE,
				    "ADIOI_GEN_aio_wait_fn", 
				    __LINE__, MPI_ERR_IO, 
				    "**mpi_grequest_complete", 0);
			}
			ADIOI_Free(aio_reqlist[i]->aiocbp);
			aio_reqlist[i]->aiocbp = NULL;
			cblist[i] = NULL;
			nr_complete++;
		    } 
		    /* TODO: need to handle error conditions somehow*/
		}
	    } /* TODO: also need to handle errors here  */
	    if ( (timeout > 0) && (timeout < (MPI_Wtime() - starttime) ))
		break;
	}

	if (cblist != NULL) ADIOI_Free(cblist);
        return errcode;
}

int ADIOI_GEN_aio_free_fn(void *extra_state)
{
	ADIOI_AIO_Request *aio_req;
	aio_req = (ADIOI_AIO_Request*)extra_state;

	if (aio_req->aiocbp != NULL)
		ADIOI_Free(aio_req->aiocbp);
	ADIOI_Free(aio_req);

	return MPI_SUCCESS;
}
#endif /* working AIO */

int ADIOI_GEN_aio_query_fn(void *extra_state, MPI_Status *status) 
{
	ADIOI_AIO_Request *aio_req;

	aio_req = (ADIOI_AIO_Request *)extra_state;


	MPI_Status_set_elements(status, MPI_BYTE, aio_req->nbytes); 

	/* can never cancel so always true */ 
	MPI_Status_set_cancelled(status, 0); 

	/* choose not to return a value for this */ 
	status->MPI_SOURCE = MPI_UNDEFINED; 
	/* tag has no meaning for this generalized request */ 
	status->MPI_TAG = MPI_UNDEFINED; 
	/* this generalized request never fails */ 
	return MPI_SUCCESS; 
}
/* 
 * vim: ts=8 sts=4 sw=4 noexpandtab 
 */
