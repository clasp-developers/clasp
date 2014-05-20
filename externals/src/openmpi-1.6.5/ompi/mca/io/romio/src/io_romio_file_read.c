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
#include "mpi.h"
#include "ompi/file/file.h"
#include "io_romio.h"


int
mca_io_romio_file_read_at (ompi_file_t *fh,
                           MPI_Offset offset,
                           void *buf,
                           int count,
                           struct ompi_datatype_t *datatype,
                           ompi_status_public_t * status)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_read_at) (data->romio_fh, offset, buf, count,
                                       datatype, status);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_read_at_all (ompi_file_t *fh,
                               MPI_Offset offset,
                               void *buf,
                               int count,
                               struct ompi_datatype_t *datatype,
                               ompi_status_public_t * status)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_read_at_all) (data->romio_fh, offset, buf, count,
                                           datatype, status);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_iread_at (ompi_file_t *fh,
                            MPI_Offset offset,
                            void *buf,
                            int count,
                            struct ompi_datatype_t *datatype,
                            ompi_request_t **request)
{
    int ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_iread_at) (data->romio_fh, offset, buf, count,
                                         datatype, request);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_read (ompi_file_t *fh,
                        void *buf,
                        int count,
                        struct ompi_datatype_t *datatype,
                        ompi_status_public_t * status)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_read) (data->romio_fh, buf, count, datatype,
                                    status);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_read_all (ompi_file_t *fh,
                            void *buf,
                            int count,
                            struct ompi_datatype_t *datatype,
                            ompi_status_public_t * status)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_read_all) (data->romio_fh, buf, count, datatype,
                                        status);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_iread (ompi_file_t *fh,
                         void *buf,
                         int count,
                         struct ompi_datatype_t *datatype,
                         ompi_request_t **request)
{
    int ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_iread) (data->romio_fh, buf, count, datatype,
                                      request);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_read_shared (ompi_file_t *fh,
                               void *buf,
                               int count,
                               struct ompi_datatype_t *datatype,
                               ompi_status_public_t * status)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_read_shared) (data->romio_fh, buf, count, 
                                            datatype, status);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_iread_shared (ompi_file_t *fh,
                                void *buf,
                                int count,
                                struct ompi_datatype_t *datatype,
                                ompi_request_t **request)
{
    int ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_iread_shared) (data->romio_fh, buf, count, 
                                             datatype, request);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_read_ordered (ompi_file_t *fh,
                                void *buf,
                                int count,
                                struct ompi_datatype_t *datatype,
                                ompi_status_public_t * status)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_read_ordered) (data->romio_fh, buf, count,
                                             datatype, status);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_read_at_all_begin (ompi_file_t *fh,
                                     MPI_Offset offset,
                                     void *buf,
                                     int count,
                                     struct ompi_datatype_t *datatype)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_read_at_all_begin) (data->romio_fh, offset, buf,
                                                 count, datatype);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_read_at_all_end (ompi_file_t *fh,
                                   void *buf,
                                   ompi_status_public_t * status)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_read_at_all_end) (data->romio_fh, buf, status);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_read_all_begin (ompi_file_t *fh,
                                  void *buf,
                                  int count,
                                  struct ompi_datatype_t *datatype)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_read_all_begin) (data->romio_fh, buf, count,
                                              datatype);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_read_all_end (ompi_file_t *fh,
                                void *buf,
                                ompi_status_public_t * status)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_read_all_end) (data->romio_fh, buf, status);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_read_ordered_begin (ompi_file_t *fh,
                                      void *buf,
                                      int count,
                                      struct ompi_datatype_t *datatype)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_read_ordered_begin) (data->romio_fh, buf, count,
                                                  datatype);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_read_ordered_end (ompi_file_t *fh,
                                    void *buf,
                                    ompi_status_public_t * status)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_read_ordered_end) (data->romio_fh, buf, 
                                                   status);
    OPAL_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}
