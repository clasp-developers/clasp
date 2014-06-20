/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>
#include "mpi.h"

#include "ompi/errhandler/errcode-internal.h"

/* Table holding all error codes */
opal_pointer_array_t ompi_errcodes_intern;
int ompi_errcode_intern_lastused=0;

static ompi_errcode_intern_t ompi_success_intern;
static ompi_errcode_intern_t ompi_error;
static ompi_errcode_intern_t ompi_err_out_of_resource;
static ompi_errcode_intern_t ompi_err_temp_out_of_resource;
static ompi_errcode_intern_t ompi_err_resource_busy;
static ompi_errcode_intern_t ompi_err_bad_param;
static ompi_errcode_intern_t ompi_err_recv_less_than_posted;
static ompi_errcode_intern_t ompi_err_recv_more_than_posted;
static ompi_errcode_intern_t ompi_err_no_match_yet;
static ompi_errcode_intern_t ompi_err_fatal;
static ompi_errcode_intern_t ompi_err_not_implemented;
static ompi_errcode_intern_t ompi_err_not_supported;
static ompi_errcode_intern_t ompi_err_interupted;
static ompi_errcode_intern_t ompi_err_would_block;
static ompi_errcode_intern_t ompi_err_in_errno;
static ompi_errcode_intern_t ompi_err_unreach;
static ompi_errcode_intern_t ompi_err_not_found;
static ompi_errcode_intern_t ompi_err_request;
static ompi_errcode_intern_t ompi_err_buffer;

static void ompi_errcode_intern_construct(ompi_errcode_intern_t* errcode);
static void ompi_errcode_intern_destruct(ompi_errcode_intern_t* errcode);

OBJ_CLASS_INSTANCE(ompi_errcode_intern_t,opal_object_t,ompi_errcode_intern_construct, ompi_errcode_intern_destruct);

int ompi_errcode_intern_init (void)
{
    int pos=0;
    /* Initialize the pointer array, which will hold the references to
       the error objects */
    OBJ_CONSTRUCT(&ompi_errcodes_intern, opal_pointer_array_t);
    if( OPAL_SUCCESS != opal_pointer_array_init(&ompi_errcodes_intern,
                                                0, OMPI_FORTRAN_HANDLE_MAX, 64) ) {
        return OMPI_ERROR;
    }

    /* Initialize now each predefined error code and register
       it in the pointer-array. */
    OBJ_CONSTRUCT(&ompi_success_intern, ompi_errcode_intern_t);
    ompi_success_intern.code = OMPI_SUCCESS;
    ompi_success_intern.mpi_code = MPI_SUCCESS;
    ompi_success_intern.index = pos++;
    strncpy(ompi_success_intern.errstring, "OMPI_SUCCESS", OMPI_MAX_ERROR_STRING);
    opal_pointer_array_set_item(&ompi_errcodes_intern, ompi_success_intern.index,  
                                &ompi_success_intern);

    OBJ_CONSTRUCT(&ompi_error, ompi_errcode_intern_t);
    ompi_error.code = OMPI_ERROR;
    ompi_error.mpi_code = MPI_ERR_OTHER;
    ompi_error.index = pos++;
    strncpy(ompi_error.errstring, "OMPI_ERROR", OMPI_MAX_ERROR_STRING);
    opal_pointer_array_set_item(&ompi_errcodes_intern, ompi_error.index, 
                                &ompi_error);

    OBJ_CONSTRUCT(&ompi_err_out_of_resource, ompi_errcode_intern_t);
    ompi_err_out_of_resource.code = OMPI_ERR_OUT_OF_RESOURCE;
    ompi_err_out_of_resource.mpi_code = MPI_ERR_INTERN;
    ompi_err_out_of_resource.index = pos++;
    strncpy(ompi_err_out_of_resource.errstring, "OMPI_ERR_OUT_OF_RESOURCE", OMPI_MAX_ERROR_STRING);
    opal_pointer_array_set_item(&ompi_errcodes_intern, ompi_err_out_of_resource.index, 
                                &ompi_err_out_of_resource);

    OBJ_CONSTRUCT(&ompi_err_temp_out_of_resource, ompi_errcode_intern_t);
    ompi_err_temp_out_of_resource.code = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    ompi_err_temp_out_of_resource.mpi_code = MPI_ERR_INTERN;
    ompi_err_temp_out_of_resource.index = pos++;
    strncpy(ompi_err_temp_out_of_resource.errstring, "OMPI_ERR_TEMP_OUT_OF_RESOURCE", OMPI_MAX_ERROR_STRING);
    opal_pointer_array_set_item(&ompi_errcodes_intern, ompi_err_temp_out_of_resource.index, 
                                &ompi_err_temp_out_of_resource);

    OBJ_CONSTRUCT(&ompi_err_resource_busy, ompi_errcode_intern_t);
    ompi_err_resource_busy.code = OMPI_ERR_RESOURCE_BUSY;
    ompi_err_resource_busy.mpi_code = MPI_ERR_INTERN;
    ompi_err_resource_busy.index = pos++;
    strncpy(ompi_err_resource_busy.errstring, "OMPI_ERR_RESOURCE_BUSY", OMPI_MAX_ERROR_STRING);
    opal_pointer_array_set_item(&ompi_errcodes_intern, ompi_err_resource_busy.index, 
                                &ompi_err_resource_busy);

    OBJ_CONSTRUCT(&ompi_err_bad_param, ompi_errcode_intern_t);
    ompi_err_bad_param.code = OMPI_ERR_BAD_PARAM;
    ompi_err_bad_param.mpi_code = MPI_ERR_ARG;
    ompi_err_bad_param.index = pos++;
    strncpy(ompi_err_bad_param.errstring, "OMPI_ERR_BAD_PARAM", OMPI_MAX_ERROR_STRING);
    opal_pointer_array_set_item(&ompi_errcodes_intern, ompi_err_bad_param.index, 
                                &ompi_err_bad_param);

    OBJ_CONSTRUCT(&ompi_err_recv_less_than_posted, ompi_errcode_intern_t);
    ompi_err_recv_less_than_posted.code = OMPI_ERR_RECV_LESS_THAN_POSTED;
    ompi_err_recv_less_than_posted.mpi_code = MPI_SUCCESS;
    ompi_err_recv_less_than_posted.index = pos++;
    strncpy(ompi_err_recv_less_than_posted.errstring, "OMPI_ERR_RECV_LESS_THAN_POSTED", OMPI_MAX_ERROR_STRING);
    opal_pointer_array_set_item(&ompi_errcodes_intern, ompi_err_recv_less_than_posted.index, 
                                &ompi_err_recv_less_than_posted);

    OBJ_CONSTRUCT(&ompi_err_recv_more_than_posted, ompi_errcode_intern_t);
    ompi_err_recv_more_than_posted.code = OMPI_ERR_RECV_MORE_THAN_POSTED;
    ompi_err_recv_more_than_posted.mpi_code = MPI_ERR_TRUNCATE;
    ompi_err_recv_more_than_posted.index = pos++;
    strncpy(ompi_err_recv_more_than_posted.errstring, "OMPI_ERR_RECV_MORE_THAN_POSTED", OMPI_MAX_ERROR_STRING);
    opal_pointer_array_set_item(&ompi_errcodes_intern, ompi_err_recv_more_than_posted.index, 
                                &ompi_err_recv_more_than_posted);

    OBJ_CONSTRUCT(&ompi_err_no_match_yet, ompi_errcode_intern_t);
    ompi_err_no_match_yet.code = OMPI_ERR_NO_MATCH_YET;
    ompi_err_no_match_yet.mpi_code = MPI_ERR_PENDING;
    ompi_err_no_match_yet.index = pos++;
    strncpy(ompi_err_no_match_yet.errstring, "OMPI_ERR_NO_MATCH_YET", OMPI_MAX_ERROR_STRING);
    opal_pointer_array_set_item(&ompi_errcodes_intern, ompi_err_no_match_yet.index, 
                                &ompi_err_no_match_yet);

    OBJ_CONSTRUCT(&ompi_err_fatal, ompi_errcode_intern_t);
    ompi_err_fatal.code = OMPI_ERR_FATAL;
    ompi_err_fatal.mpi_code = MPI_ERR_INTERN;
    ompi_err_fatal.index = pos++;
    strncpy(ompi_err_fatal.errstring, "OMPI_ERR_FATAL", OMPI_MAX_ERROR_STRING);
    opal_pointer_array_set_item(&ompi_errcodes_intern, ompi_err_fatal.index, 
                                &ompi_err_fatal);

    OBJ_CONSTRUCT(&ompi_err_not_implemented, ompi_errcode_intern_t);
    ompi_err_not_implemented.code = OMPI_ERR_NOT_IMPLEMENTED;
    ompi_err_not_implemented.mpi_code = MPI_ERR_INTERN;
    ompi_err_not_implemented.index = pos++;
    strncpy(ompi_err_not_implemented.errstring, "OMPI_ERR_NOT_IMPLEMENTED", OMPI_MAX_ERROR_STRING);
    opal_pointer_array_set_item(&ompi_errcodes_intern, ompi_err_not_implemented.index, 
                                &ompi_err_not_implemented);

    OBJ_CONSTRUCT(&ompi_err_not_supported, ompi_errcode_intern_t);
    ompi_err_not_supported.code = OMPI_ERR_NOT_SUPPORTED;
    ompi_err_not_supported.mpi_code = MPI_ERR_INTERN;
    ompi_err_not_supported.index = pos++;
    strncpy(ompi_err_not_supported.errstring, "OMPI_ERR_NOT_SUPPORTED", OMPI_MAX_ERROR_STRING);
    opal_pointer_array_set_item(&ompi_errcodes_intern, ompi_err_not_supported.index, 
                                &ompi_err_not_supported);

    OBJ_CONSTRUCT(&ompi_err_interupted, ompi_errcode_intern_t);
    ompi_err_interupted.code = OMPI_ERR_INTERUPTED;
    ompi_err_interupted.mpi_code = MPI_ERR_INTERN;
    ompi_err_interupted.index = pos++;
    strncpy(ompi_err_interupted.errstring, "OMPI_ERR_INTERUPTED", OMPI_MAX_ERROR_STRING);
    opal_pointer_array_set_item(&ompi_errcodes_intern, ompi_err_interupted.index, 
                                &ompi_err_interupted);

    OBJ_CONSTRUCT(&ompi_err_would_block, ompi_errcode_intern_t);
    ompi_err_would_block.code = OMPI_ERR_WOULD_BLOCK;
    ompi_err_would_block.mpi_code = MPI_ERR_INTERN;
    ompi_err_would_block.index = pos++;
    strncpy(ompi_err_would_block.errstring, "OMPI_ERR_WOULD_BLOCK", OMPI_MAX_ERROR_STRING);
    opal_pointer_array_set_item(&ompi_errcodes_intern, ompi_err_would_block.index, 
                                &ompi_err_would_block);

    OBJ_CONSTRUCT(&ompi_err_in_errno, ompi_errcode_intern_t);
    ompi_err_in_errno.code = OMPI_ERR_IN_ERRNO;
    ompi_err_in_errno.mpi_code = MPI_ERR_INTERN;
    ompi_err_in_errno.index = pos++;
    strncpy(ompi_err_in_errno.errstring, "OMPI_ERR_IN_ERRNO", OMPI_MAX_ERROR_STRING);
    opal_pointer_array_set_item(&ompi_errcodes_intern, ompi_err_in_errno.index, 
                                &ompi_err_in_errno);

    OBJ_CONSTRUCT(&ompi_err_unreach, ompi_errcode_intern_t);
    ompi_err_unreach.code = OMPI_ERR_UNREACH;
    ompi_err_unreach.mpi_code = MPI_ERR_INTERN;
    ompi_err_unreach.index = pos++;
    strncpy(ompi_err_unreach.errstring, "OMPI_ERR_UNREACH", OMPI_MAX_ERROR_STRING);
    opal_pointer_array_set_item(&ompi_errcodes_intern, ompi_err_unreach.index, 
                                &ompi_err_unreach);

    OBJ_CONSTRUCT(&ompi_err_not_found, ompi_errcode_intern_t);
    ompi_err_not_found.code = OMPI_ERR_NOT_FOUND;
    ompi_err_not_found.mpi_code = MPI_ERR_INTERN;
    ompi_err_not_found.index = pos++;
    strncpy(ompi_err_not_found.errstring, "OMPI_ERR_NOT_FOUND", OMPI_MAX_ERROR_STRING);
    opal_pointer_array_set_item(&ompi_errcodes_intern, ompi_err_not_found.index, 
                                &ompi_err_not_found);

    OBJ_CONSTRUCT(&ompi_err_buffer, ompi_errcode_intern_t);
    ompi_err_buffer.code = OMPI_ERR_BUFFER;
    ompi_err_buffer.mpi_code = MPI_ERR_BUFFER;
    ompi_err_buffer.index = pos++;
    strncpy(ompi_err_buffer.errstring, "OMPI_ERR_BUFFER", OMPI_MAX_ERROR_STRING);
    opal_pointer_array_set_item(&ompi_errcodes_intern, ompi_err_buffer.index, 
                                &ompi_err_buffer);

    OBJ_CONSTRUCT(&ompi_err_request, ompi_errcode_intern_t);
    ompi_err_request.code = OMPI_ERR_REQUEST;
    ompi_err_request.mpi_code = MPI_ERR_REQUEST;
    ompi_err_request.index = pos++;
    strncpy(ompi_err_request.errstring, "OMPI_ERR_REQUEST", OMPI_MAX_ERROR_STRING);
    opal_pointer_array_set_item(&ompi_errcodes_intern, ompi_err_request.index, 
                                &ompi_err_request);

    ompi_errcode_intern_lastused=pos;
    return OMPI_SUCCESS;
}

int ompi_errcode_intern_finalize(void)
{

    OBJ_DESTRUCT(&ompi_success_intern);
    OBJ_DESTRUCT(&ompi_error);
    OBJ_DESTRUCT(&ompi_err_out_of_resource);
    OBJ_DESTRUCT(&ompi_err_temp_out_of_resource);
    OBJ_DESTRUCT(&ompi_err_resource_busy);
    OBJ_DESTRUCT(&ompi_err_bad_param);
    OBJ_DESTRUCT(&ompi_err_recv_less_than_posted);
    OBJ_DESTRUCT(&ompi_err_recv_more_than_posted);
    OBJ_DESTRUCT(&ompi_err_no_match_yet);
    OBJ_DESTRUCT(&ompi_err_fatal);
    OBJ_DESTRUCT(&ompi_err_not_implemented);
    OBJ_DESTRUCT(&ompi_err_not_supported);
    OBJ_DESTRUCT(&ompi_err_interupted);
    OBJ_DESTRUCT(&ompi_err_would_block);
    OBJ_DESTRUCT(&ompi_err_in_errno);
    OBJ_DESTRUCT(&ompi_err_unreach);
    OBJ_DESTRUCT(&ompi_err_not_found);
    OBJ_DESTRUCT(&ompi_err_buffer);
    OBJ_DESTRUCT(&ompi_err_request);

    OBJ_DESTRUCT(&ompi_errcodes_intern);
    return OMPI_SUCCESS;
}

static void ompi_errcode_intern_construct(ompi_errcode_intern_t *errcode)
{
    errcode->code     = MPI_UNDEFINED;
    errcode->mpi_code = MPI_UNDEFINED;
    errcode->index    = MPI_UNDEFINED;
    memset ( errcode->errstring, 0, OMPI_MAX_ERROR_STRING);
    return;
}

static void ompi_errcode_intern_destruct(ompi_errcode_intern_t *errcode)
{
    opal_pointer_array_set_item(&ompi_errcodes_intern, errcode->index, NULL);
    return;
}
