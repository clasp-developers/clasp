/*
 *  Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                          University Research and Technology
 *                          Corporation.  All rights reserved.
 *  Copyright (c) 2004-2005 The University of Tennessee and The University
 *                          of Tennessee Research Foundation.  All rights
 *                          reserved.
 *  Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                          University of Stuttgart.  All rights reserved.
 *  Copyright (c) 2004-2005 The Regents of the University of California.
 *                          All rights reserved.
 *  $COPYRIGHT$
 *  
 *  Additional copyrights may follow
 *  
 *  $HEADER$
 */

#include "ompi_config.h"

#include "ompi/communicator/communicator.h"
#include "ompi/info/info.h"
#include "ompi/file/file.h"

#include "io_romio.h"


int
mca_io_romio_file_open (ompi_communicator_t *comm,
                        char *filename,
                        int amode,
                        ompi_info_t *info,
                        ompi_file_t *fh)
{
    int ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_open)(comm, filename, amode, info, 
                                      &data->romio_fh);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_close (ompi_file_t *fh)
{
    int ret;
    mca_io_romio_data_t *data;

    /* If we've already started MPI_Finalize by this point, then just
       give up (because ROMIO's file close routine calls MPI_Barrier,
       which we obviously can't do if we've started to MPI_Finalize).
       The user didn't close the file, so they should expect
       unexpected behavior. */
    if (ompi_mpi_finalized) {
        return OMPI_SUCCESS;
    }

    /* Because ROMIO expects the MPI library to provide error handler
     * management routines but it doesn't ever participate in
     * MPI_File_close, we have to somehow inform the MPI library that
     * we no longer hold a reference to any user defined error
     * handler.  We do this by setting the errhandler at this point to
     * MPI_ERRORS_RETURN. */
    if (fh->error_handler != &ompi_mpi_errors_return.eh) {
        OBJ_RELEASE(fh->error_handler);
        fh->error_handler = &ompi_mpi_errors_return.eh;
        OBJ_RETAIN(fh->error_handler);
    }

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;

    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_close) (&data->romio_fh);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_set_size (ompi_file_t *fh,
                            MPI_Offset size)
{
    int ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_set_size) (data->romio_fh, size);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}

int
mca_io_romio_file_preallocate (ompi_file_t *fh,
                               MPI_Offset size)
{
    int ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_preallocate) (data->romio_fh, size);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_get_size (ompi_file_t *fh,
                            MPI_Offset * size)
{
    int ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_get_size) (data->romio_fh, size);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_get_amode (ompi_file_t *fh,
                             int *amode)
{
    int ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_get_amode) (data->romio_fh, amode);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_set_info (ompi_file_t *fh,
                            ompi_info_t *info)
{
    int ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_set_info) (data->romio_fh, info);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_get_info (ompi_file_t *fh,
                            ompi_info_t ** info_used)
{
    int ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_get_info) (data->romio_fh, info_used);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_set_view (ompi_file_t *fh,
                            MPI_Offset disp,
                            struct ompi_datatype_t *etype,
                            struct ompi_datatype_t *filetype,
                            char *datarep,
                            ompi_info_t *info)
{
    int ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_set_view) (data->romio_fh, disp, etype, filetype,
                                        datarep, info);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_get_view (ompi_file_t *fh,
                            MPI_Offset * disp,
                            struct ompi_datatype_t ** etype,
                            struct ompi_datatype_t ** filetype,
                            char *datarep)
{
    int ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_get_view) (data->romio_fh, disp, etype, filetype,
                                        datarep);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;

}


int
mca_io_romio_file_get_type_extent (ompi_file_t *fh,
                                   struct ompi_datatype_t *datatype,
                                   MPI_Aint * extent)
{
    int ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_get_type_extent) (data->romio_fh, datatype, extent);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_set_atomicity (ompi_file_t *fh,
                                 int flag)
{
    int ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_set_atomicity) (data->romio_fh, flag);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}

int
mca_io_romio_file_get_atomicity (ompi_file_t *fh,
                                 int *flag)
{
    int ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_get_atomicity) (data->romio_fh, flag);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}

int
mca_io_romio_file_sync (ompi_file_t *fh)
{
    int ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_sync) (data->romio_fh);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_seek_shared (ompi_file_t *fh,
                               MPI_Offset offset,
                               int whence)
{
    int ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_seek_shared) (data->romio_fh, offset, whence);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_get_position_shared (ompi_file_t *fh,
                                       MPI_Offset * offset)
{
    int ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_get_position_shared) (data->romio_fh, offset);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_seek (ompi_file_t *fh,
                        MPI_Offset offset,
                        int whence)
{
    int ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_seek) (data->romio_fh, offset, whence);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_get_position (ompi_file_t *fh,
                                MPI_Offset * offset)
{
    int ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_get_position) (data->romio_fh, offset);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_get_byte_offset (ompi_file_t *fh,
                                   MPI_Offset offset,
                                   MPI_Offset * disp)
{
    int ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_get_byte_offset) (data->romio_fh, offset, disp);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}
