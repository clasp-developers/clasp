/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_iwrite = PMPI_File_iwrite
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_iwrite MPI_File_iwrite
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_iwrite as PMPI_File_iwrite
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_iwrite - Nonblocking write using individual file pointer

Input Parameters:
. fh - file handle (handle)
. buf - initial address of buffer (choice)
. count - number of elements in buffer (nonnegative integer)
. datatype - datatype of each buffer element (handle)

Output Parameters:
. request - request object (handle)

.N fortran
@*/
#ifdef HAVE_MPI_GREQUEST
#include "mpiu_greq.h"
#endif

int MPI_File_iwrite(MPI_File mpi_fh, void *buf, int count, 
		    MPI_Datatype datatype, MPI_Request *request)
{
    int error_code=MPI_SUCCESS;
    static char myname[] = "MPI_FILE_IWRITE";
#ifdef MPI_hpux
    int fl_xmpi;

    HPMP_IO_START(fl_xmpi, BLKMPIFILEIWRITE, TRDTSYSTEM, mpi_fh, datatype,
		  count);
#endif /* MPI_hpux */

    MPIU_THREAD_CS_ENTER(ALLFUNC,);

    error_code = MPIOI_File_iwrite(mpi_fh, (MPI_Offset) 0, ADIO_INDIVIDUAL,
				   buf, count, datatype, myname, request);

    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS)
	error_code = MPIO_Err_return_file(mpi_fh, error_code);
    /* --END ERROR HANDLING-- */

#ifdef MPI_hpux
    HPMP_IO_END(fl_xmpi, mpi_fh, datatype, count);
#endif /* MPI_hpux */
    MPIU_THREAD_CS_EXIT(ALLFUNC,);

    return error_code;
}

/* prevent multiple definitions of this routine */
#ifdef MPIO_BUILD_PROFILING
int MPIOI_File_iwrite(MPI_File mpi_fh,
		      MPI_Offset offset,
		      int file_ptr_type,
		      void *buf,
		      int count,
		      MPI_Datatype datatype,
		      char *myname,
		      MPI_Request *request)
{
    int error_code, bufsize, buftype_is_contig, filetype_is_contig;
    int datatype_size;
    ADIO_Status status;
    ADIO_Offset off;
    ADIO_File fh;
    MPI_Offset nbytes=0;

    fh = MPIO_File_resolve(mpi_fh);

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_FILE_HANDLE(fh, myname, error_code);
    MPIO_CHECK_COUNT(fh, count, myname, error_code);
    MPIO_CHECK_DATATYPE(fh, datatype, myname, error_code);

    if (file_ptr_type == ADIO_EXPLICIT_OFFSET && offset < 0) {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_ARG,
					  "**iobadoffset", 0);
	error_code = MPIO_Err_return_file(fh, error_code);
	goto fn_exit;
    }
    /* --END ERROR HANDLING-- */

    MPI_Type_size(datatype, &datatype_size);

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_INTEGRAL_ETYPE(fh, count, datatype_size, myname, error_code);
    MPIO_CHECK_WRITABLE(fh, myname, error_code);
    MPIO_CHECK_NOT_SEQUENTIAL_MODE(fh, myname, error_code);
    MPIO_CHECK_COUNT_SIZE(fh, count, datatype_size, myname, error_code);
    /* --END ERROR HANDLING-- */

    ADIOI_Datatype_iscontig(datatype, &buftype_is_contig);
    ADIOI_Datatype_iscontig(fh->filetype, &filetype_is_contig);
    
    ADIOI_TEST_DEFERRED(fh, myname, &error_code);

    if (buftype_is_contig && filetype_is_contig) {
	/* convert sizes to bytes */
	bufsize = datatype_size * count;
	if (file_ptr_type == ADIO_EXPLICIT_OFFSET) {
	    off = fh->disp + fh->etype_size * offset;
	}
	else {
	    off = fh->fp_ind;
	}

        if (!(fh->atomicity)) {
	    ADIO_IwriteContig(fh, buf, count, datatype, file_ptr_type,
			      off, request, &error_code);
	}
	else {
            /* to maintain strict atomicity semantics with other concurrent
              operations, lock (exclusive) and call blocking routine */
	    if (ADIO_Feature(fh, ADIO_LOCKS) )
	    {
                ADIOI_WRITE_LOCK(fh, off, SEEK_SET, bufsize);
	    }

            ADIO_WriteContig(fh, buf, count, datatype, file_ptr_type, off, 
			     &status, &error_code);  

	    if (ADIO_Feature(fh, ADIO_LOCKS) )
	    {
                ADIOI_UNLOCK(fh, off, SEEK_SET, bufsize);
	    }
	    if (error_code == MPI_SUCCESS) {
		nbytes = count * datatype_size;
	    }
	    
	    MPIO_Completed_request_create(&fh, nbytes, &error_code, request);
	}
    }
    else {
	ADIO_IwriteStrided(fh, buf, count, datatype, file_ptr_type,
			   offset, request, &error_code);
    }
fn_exit:
    return error_code;
}
#endif
