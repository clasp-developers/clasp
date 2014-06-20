/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_ntfs.h"

#include "../../mpi-io/mpioimpl.h"
#include "../../mpi-io/mpioprof.h"
#include "mpiu_greq.h"

static MPIX_Grequest_class ADIOI_NTFS_greq_class = 0;

/* Fills the input buffer, errMsg, with the error message 
   corresponding to error code, error */
void ADIOI_NTFS_Strerror(int error, char *errMsg, int errMsgLen)
{
    LPTSTR str;
    int num_bytes;
    num_bytes = FormatMessage(
	FORMAT_MESSAGE_FROM_SYSTEM |
	FORMAT_MESSAGE_ALLOCATE_BUFFER,
	NULL,
	error,
	0,
	&str,
	FORMAT_MESSAGE_MIN_SIZE,
    0);
    if (num_bytes == 0)
    {
	strncpy(errMsg, "\0", errMsgLen);
    }
    else
    {
	strncpy(errMsg, str, errMsgLen);
	LocalFree(str);
    }
}

/* poll for completion of a single outstanding AIO request */
int ADIOI_NTFS_aio_poll_fn(void *extra_state, MPI_Status *status)
{
    ADIOI_AIO_Request *aio_req;
    int mpi_errno = MPI_SUCCESS;

    /* FIXME: Validate the args -- has it already been done by the 
       caller ? */

    aio_req = (ADIOI_AIO_Request *)extra_state;
    
    /* XXX: test for AIO completion here */
    if(!GetOverlappedResult( aio_req->fd, aio_req->lpOvl, 
                            &(aio_req->nbytes), FALSE)){
        if(GetLastError() == ERROR_IO_INCOMPLETE){
        /* IO in progress */
	    /* TODO: need to diddle with status somehow */
        }else{
        /* Error occured */
        /* TODO: unsure how to handle this */    
        }
    }else{
        mpi_errno = MPI_Grequest_complete(aio_req->req);
	    if (mpi_errno != MPI_SUCCESS) {
		    mpi_errno = MPIO_Err_create_code(MPI_SUCCESS,
				    MPIR_ERR_RECOVERABLE,
				    "ADIOI_NTFS_aio_poll_fn", __LINE__,
				    MPI_ERR_IO, "**mpi_grequest_complete",
				    0);
	    }
    }
    return mpi_errno;
}


/* Wait for completion of one of the outstanding AIO requests */
int ADIOI_NTFS_aio_wait_fn(int count, void **array_of_states,
		double timeout, MPI_Status *status)
{
	int i, mpi_errno = MPI_SUCCESS;
	ADIOI_AIO_Request **aio_reqlist;
    LPHANDLE lpHandles;
    DWORD retObject=0;

    /* FIXME: Validate the args -- has it already been done by the 
       caller ? */
	aio_reqlist = (ADIOI_AIO_Request **)array_of_states;
    lpHandles = (LPHANDLE) ADIOI_Calloc(count, sizeof(HANDLE));
    if (lpHandles == NULL)
    {
	mpi_errno = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
	    "ADIOI_NTFS_aio_wait_fn", __LINE__, MPI_ERR_IO,
	    "**nomem", "**nomem %s", "Event handles");
	return mpi_errno;
    }
	/* XXX: set-up arrays of outstanding requests */
    for(i=0; i<count; i++){
        lpHandles[i] = (aio_reqlist[i])->lpOvl->hEvent;
    }

	/* XXX: wait for one request to complete */
    /* FIXME: Is the timeout in seconds ? */
    timeout = (timeout <= 0) ? INFINITE : (timeout * 1000);
    
    if((retObject = WaitForMultipleObjects(count, lpHandles,
                    FALSE, timeout)) != WAIT_FAILED){
        retObject = retObject - WAIT_OBJECT_0;
        if(GetOverlappedResult( aio_reqlist[retObject]->fd, 
                aio_reqlist[retObject]->lpOvl, &(aio_reqlist[retObject]->nbytes), 
                FALSE)){
        	/* XXX: mark completed requests as 'done'*/
            mpi_errno = MPI_Grequest_complete(aio_reqlist[retObject]->req);
    	    if (mpi_errno != MPI_SUCCESS) {
	    	    mpi_errno = MPIO_Err_create_code(MPI_SUCCESS,
				    MPIR_ERR_RECOVERABLE,
				    "ADIOI_NTFS_aio_wait_fn", __LINE__,
				    MPI_ERR_IO, "**mpi_grequest_complete",
				    0);
            }
        }else{
            if(GetLastError() == ERROR_IO_INCOMPLETE){
            /* IO in progress */
	        /* TODO: need to diddle with status somehow */
            }else{
            /* Error occured */
            /* TODO: not sure how to handle this */    
            }
        }
    }else{
        /* TODO: How to handle error while waiting ? */
    }
    ADIOI_Free(lpHandles);
	return mpi_errno;
}

int ADIOI_NTFS_aio_query_fn(void *extra_state, MPI_Status *status) 
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


int ADIOI_NTFS_aio_free_fn(void *extra_state)
{
	ADIOI_AIO_Request *aio_req;
    /* FIXME: Validate the args -- has it already been done by the 
       caller ? */
	aio_req = (ADIOI_AIO_Request*)extra_state;
    CloseHandle(aio_req->lpOvl->hEvent);
    ADIOI_Free(aio_req->lpOvl);
    ADIOI_Free(aio_req);
	return MPI_SUCCESS;    
}

void ADIOI_NTFS_IwriteContig(ADIO_File fd, void *buf, int count, 
			     MPI_Datatype datatype, int file_ptr_type,
			     ADIO_Offset offset, ADIO_Request *request,
			     int *error_code)  
{
    int len, typesize;
    int err;
    static char myname[] = "ADIOI_NTFS_IwriteContig";

    MPI_Type_size(datatype, &typesize);
    len = count * typesize;

    if (file_ptr_type == ADIO_INDIVIDUAL)
    {
	offset = fd->fp_ind;
    }
    err = ADIOI_NTFS_aio(fd, buf, len, offset, 1, request);
    if (file_ptr_type == ADIO_INDIVIDUAL)
    {
	fd->fp_ind += len;
    }

    /* --BEGIN ERROR HANDLING-- */
    if (err != MPI_SUCCESS)
    {
	*error_code = MPIO_Err_create_code(err, MPIR_ERR_RECOVERABLE,
					   myname, __LINE__, MPI_ERR_IO,
					   "**io", 0);
	return;
    }
    /* --END ERROR HANDLING-- */
    *error_code = MPI_SUCCESS;

    fd->fp_sys_posn = -1;   /* set it to null. */
}


/* This function is for implementation convenience. It is not user-visible.
 * If wr==1 write, wr==0 read.
 *
 * Returns MPI_SUCCESS on success, mpi_errno on failure.
 */
int ADIOI_NTFS_aio(ADIO_File fd, void *buf, int len, ADIO_Offset offset,
		   int wr, MPI_Request *request)
{
    static char myname[] = "ADIOI_NTFS_aio";

    ADIOI_AIO_Request *aio_req;
    static DWORD dwNumWritten, dwNumRead;
    BOOL ret_val = FALSE;
    FDTYPE fd_sys;
    int mpi_errno = MPI_SUCCESS;
    DWORD err;

    fd_sys = fd->fd_sys;

    aio_req = (ADIOI_AIO_Request *)ADIOI_Calloc(sizeof(ADIOI_AIO_Request), 1);
    if (aio_req == NULL)
    {
	mpi_errno = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
	    myname, __LINE__, MPI_ERR_IO,
	    "**nomem", "**nomem %s", "AIO_REQ");
	return mpi_errno;
    }
    aio_req->lpOvl = (LPOVERLAPPED ) ADIOI_Calloc(sizeof(OVERLAPPED), 1);
    if (aio_req->lpOvl == NULL)
    {
	mpi_errno = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
	    myname, __LINE__, MPI_ERR_IO,
	    "**nomem", "**nomem %s", "OVERLAPPED");
    ADIOI_Free(aio_req);
	return mpi_errno;
    }
    aio_req->lpOvl->hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
    if (aio_req->lpOvl->hEvent == NULL)
    {
    char errMsg[ADIOI_NTFS_ERR_MSG_MAX];
	err = GetLastError();
    ADIOI_NTFS_Strerror(err, errMsg, ADIOI_NTFS_ERR_MSG_MAX);
	mpi_errno = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
	    myname, __LINE__, MPI_ERR_IO,
	    "**io", "**io %s", errMsg);
    ADIOI_Free(aio_req->lpOvl);
    ADIOI_Free(aio_req);
	return mpi_errno;
    }
    aio_req->lpOvl->Offset = DWORDLOW(offset);
    aio_req->lpOvl->OffsetHigh = DWORDHIGH(offset);
    aio_req->fd = fd_sys;
    
    /* XXX: initiate async I/O  */
    if (wr)
    {
	ret_val = WriteFile(fd_sys, buf, len, &dwNumWritten, aio_req->lpOvl);
    }
    else
    {
	ret_val = ReadFile(fd_sys, buf, len, &dwNumRead, aio_req->lpOvl);
    }

    /* --BEGIN ERROR HANDLING-- */
    if (ret_val == FALSE) 
    {
	mpi_errno = GetLastError();
	if (mpi_errno != ERROR_IO_PENDING)
	{
        char errMsg[ADIOI_NTFS_ERR_MSG_MAX];
        ADIOI_NTFS_Strerror(mpi_errno, errMsg, ADIOI_NTFS_ERR_MSG_MAX);
	    mpi_errno = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
		myname, __LINE__, MPI_ERR_IO,
		"**io",
		"**io %s", errMsg);
	    return mpi_errno;
	}
	mpi_errno = MPI_SUCCESS;
    }
    /* --END ERROR HANDLING-- */

    /* XXX: set up generalized request class and request */
    if (ADIOI_NTFS_greq_class == 0) {
	    mpi_errno = MPIX_Grequest_class_create(ADIOI_NTFS_aio_query_fn,
			    ADIOI_NTFS_aio_free_fn, MPIU_Greq_cancel_fn,
			    ADIOI_NTFS_aio_poll_fn, ADIOI_NTFS_aio_wait_fn,
			    &ADIOI_NTFS_greq_class);
        if(mpi_errno != MPI_SUCCESS){
        /* FIXME: Pass appropriate error code to user */
        }
    }
    mpi_errno = MPIX_Grequest_class_allocate(ADIOI_NTFS_greq_class, aio_req, request);
    if(mpi_errno != MPI_SUCCESS){
    /* FIXME: Pass appropriate error code to user */
    }
    memcpy(&(aio_req->req), request, sizeof(MPI_Request));
    return mpi_errno;
}
