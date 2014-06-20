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
 * Copyright (c) 2006      University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "mpi.h"

#include "ompi/errhandler/errcode.h"
#include "ompi/constants.h"

/* Table holding all error codes */
opal_pointer_array_t ompi_mpi_errcodes;
int ompi_mpi_errcode_lastused=0;
int ompi_mpi_errcode_lastpredefined=0;

ompi_mpi_errcode_t ompi_success;
ompi_mpi_errcode_t ompi_err_buffer;
ompi_mpi_errcode_t ompi_err_count;
ompi_mpi_errcode_t ompi_err_type;
ompi_mpi_errcode_t ompi_err_tag;
ompi_mpi_errcode_t ompi_err_comm;
ompi_mpi_errcode_t ompi_err_rank;
ompi_mpi_errcode_t ompi_err_request;
ompi_mpi_errcode_t ompi_err_root;
ompi_mpi_errcode_t ompi_err_group;
ompi_mpi_errcode_t ompi_err_op;
ompi_mpi_errcode_t ompi_err_topology;
ompi_mpi_errcode_t ompi_err_dims;
ompi_mpi_errcode_t ompi_err_arg;
ompi_mpi_errcode_t ompi_err_unknown;
ompi_mpi_errcode_t ompi_err_truncate;
ompi_mpi_errcode_t ompi_err_other;
ompi_mpi_errcode_t ompi_err_intern;
ompi_mpi_errcode_t ompi_err_in_status;
ompi_mpi_errcode_t ompi_err_pending;

ompi_mpi_errcode_t ompi_err_access;
ompi_mpi_errcode_t ompi_err_amode;
ompi_mpi_errcode_t ompi_err_assert;
ompi_mpi_errcode_t ompi_err_bad_file;
ompi_mpi_errcode_t ompi_err_base;
ompi_mpi_errcode_t ompi_err_conversion;
ompi_mpi_errcode_t ompi_err_disp;
ompi_mpi_errcode_t ompi_err_dup_datarep;
ompi_mpi_errcode_t ompi_err_file_exists;
ompi_mpi_errcode_t ompi_err_file_in_use;
ompi_mpi_errcode_t ompi_err_file;
ompi_mpi_errcode_t ompi_err_info_key;
ompi_mpi_errcode_t ompi_err_info_nokey;
ompi_mpi_errcode_t ompi_err_info_value;
ompi_mpi_errcode_t ompi_err_info;
ompi_mpi_errcode_t ompi_err_io;
ompi_mpi_errcode_t ompi_err_keyval;
ompi_mpi_errcode_t ompi_err_locktype;
ompi_mpi_errcode_t ompi_err_name;
ompi_mpi_errcode_t ompi_err_no_mem;
ompi_mpi_errcode_t ompi_err_not_same;
ompi_mpi_errcode_t ompi_err_no_space;
ompi_mpi_errcode_t ompi_err_no_such_file;
ompi_mpi_errcode_t ompi_err_port;
ompi_mpi_errcode_t ompi_err_quota;
ompi_mpi_errcode_t ompi_err_read_only;
ompi_mpi_errcode_t ompi_err_rma_conflict;
ompi_mpi_errcode_t ompi_err_rma_sync;
ompi_mpi_errcode_t ompi_err_service;
ompi_mpi_errcode_t ompi_err_size;
ompi_mpi_errcode_t ompi_err_spawn;
ompi_mpi_errcode_t ompi_err_unsupported_datarep;
ompi_mpi_errcode_t ompi_err_unsupported_operation;
ompi_mpi_errcode_t ompi_err_win;

static void ompi_mpi_errcode_construct(ompi_mpi_errcode_t* errcode);
static void ompi_mpi_errcode_destruct(ompi_mpi_errcode_t* errcode);

OBJ_CLASS_INSTANCE(ompi_mpi_errcode_t,opal_object_t,ompi_mpi_errcode_construct, ompi_mpi_errcode_destruct);

#define CONSTRUCT_ERRCODE(VAR, ERRCODE, TXT )                             \
do {                                                                      \
    OBJ_CONSTRUCT(&(VAR), ompi_mpi_errcode_t);                            \
    (VAR).code = (ERRCODE);                                               \
    (VAR).cls = (ERRCODE);                                                \
    strncpy((VAR).errstring, (TXT), MPI_MAX_ERROR_STRING);                \
    opal_pointer_array_set_item(&ompi_mpi_errcodes, (ERRCODE), &(VAR));   \
} while (0)

int ompi_mpi_errcode_init (void)
{
    /* Initialize the pointer array, which will hold the references to
       the error objects */
    OBJ_CONSTRUCT(&ompi_mpi_errcodes, opal_pointer_array_t);
    if( OPAL_SUCCESS != opal_pointer_array_init(&ompi_mpi_errcodes, 0,
                                                OMPI_FORTRAN_HANDLE_MAX, 64) ) {
        return OMPI_ERROR;
    }

    /* Initialize now each predefined error code and register
       it in the pointer-array. */
    CONSTRUCT_ERRCODE( ompi_success, MPI_SUCCESS, "MPI_SUCCESS: no errors" );
    CONSTRUCT_ERRCODE( ompi_err_buffer, MPI_ERR_BUFFER, "MPI_ERR_BUFFER: invalid buffer pointer");
    CONSTRUCT_ERRCODE( ompi_err_count, MPI_ERR_COUNT, "MPI_ERR_COUNT: invalid count argument" );
    CONSTRUCT_ERRCODE( ompi_err_type, MPI_ERR_TYPE, "MPI_ERR_TYPE: invalid datatype" );
    CONSTRUCT_ERRCODE( ompi_err_tag, MPI_ERR_TAG, "MPI_ERR_TAG: invalid tag" );
    CONSTRUCT_ERRCODE( ompi_err_comm, MPI_ERR_COMM, "MPI_ERR_COMM: invalid communicator" );
    CONSTRUCT_ERRCODE( ompi_err_rank, MPI_ERR_RANK, "MPI_ERR_RANK: invalid rank" );
    CONSTRUCT_ERRCODE( ompi_err_request, MPI_ERR_REQUEST, "MPI_ERR_REQUEST: invalid request" );
    CONSTRUCT_ERRCODE( ompi_err_root, MPI_ERR_ROOT, "MPI_ERR_ROOT: invalid root" );
    CONSTRUCT_ERRCODE( ompi_err_group, MPI_ERR_GROUP, "MPI_ERR_GROUP: invalid group" );
    CONSTRUCT_ERRCODE( ompi_err_op, MPI_ERR_OP, "MPI_ERR_OP: invalid reduce operation" );
    CONSTRUCT_ERRCODE( ompi_err_topology, MPI_ERR_TOPOLOGY, "MPI_ERR_TOPOLOGY: invalid communicator topology" );
    CONSTRUCT_ERRCODE( ompi_err_dims, MPI_ERR_DIMS, "MPI_ERR_DIMS: invalid topology dimension" );
    CONSTRUCT_ERRCODE( ompi_err_arg, MPI_ERR_ARG, "MPI_ERR_ARG: invalid argument of some other kind" );
    CONSTRUCT_ERRCODE( ompi_err_unknown, MPI_ERR_UNKNOWN, "MPI_ERR_UNKNOWN: unknown error" );
    CONSTRUCT_ERRCODE( ompi_err_truncate, MPI_ERR_TRUNCATE, "MPI_ERR_TRUNCATE: message truncated" );
    CONSTRUCT_ERRCODE( ompi_err_other, MPI_ERR_OTHER, "MPI_ERR_OTHER: known error not in list" );
    CONSTRUCT_ERRCODE( ompi_err_intern, MPI_ERR_INTERN, "MPI_ERR_INTERN: internal error" );
    CONSTRUCT_ERRCODE( ompi_err_in_status, MPI_ERR_IN_STATUS, "MPI_ERR_IN_STATUS: error code in status" );
    CONSTRUCT_ERRCODE( ompi_err_pending, MPI_ERR_PENDING, "MPI_ERR_PENDING: pending request" );
    CONSTRUCT_ERRCODE( ompi_err_access, MPI_ERR_ACCESS, "MPI_ERR_ACCESS: invalid access mode" );
    CONSTRUCT_ERRCODE( ompi_err_amode, MPI_ERR_AMODE, "MPI_ERR_AMODE: invalid amode argument" );
    CONSTRUCT_ERRCODE( ompi_err_assert, MPI_ERR_ASSERT, "MPI_ERR_ASSERT: invalid assert argument" );
    CONSTRUCT_ERRCODE( ompi_err_bad_file, MPI_ERR_BAD_FILE, "MPI_ERR_BAD_FILE: bad file" );
    CONSTRUCT_ERRCODE( ompi_err_base, MPI_ERR_BASE, "MPI_ERR_BASE: invalid base" );
    CONSTRUCT_ERRCODE( ompi_err_conversion, MPI_ERR_CONVERSION, "MPI_ERR_CONVERSION: error in data conversion" );
    CONSTRUCT_ERRCODE( ompi_err_disp, MPI_ERR_DISP, "MPI_ERR_DISP: invalid displacement" );
    CONSTRUCT_ERRCODE( ompi_err_dup_datarep, MPI_ERR_DUP_DATAREP, "MPI_ERR_DUP_DATAREP: error duplicating data representation" );
    CONSTRUCT_ERRCODE( ompi_err_file_exists, MPI_ERR_FILE_EXISTS, "MPI_ERR_FILE_EXISTS: file exists alreay" );
    CONSTRUCT_ERRCODE( ompi_err_file_in_use, MPI_ERR_FILE_IN_USE, "MPI_ERR_FILE_IN_USE: file already in use" );
    CONSTRUCT_ERRCODE( ompi_err_file, MPI_ERR_FILE, "MPI_ERR_FILE: invalid file" );
    CONSTRUCT_ERRCODE( ompi_err_info_key, MPI_ERR_INFO_KEY, "MPI_ERR_INFO_KEY: invalid key argument for info object" );
    CONSTRUCT_ERRCODE( ompi_err_info_nokey, MPI_ERR_INFO_NOKEY, "MPI_ERR_INFO_NOKEY: unknown key for given info object" );
    CONSTRUCT_ERRCODE( ompi_err_info_value, MPI_ERR_INFO_VALUE, "MPI_ERR_INFO_VALUE: invalid value argument for info object" );
    CONSTRUCT_ERRCODE( ompi_err_info, MPI_ERR_INFO, "MPI_ERR_INFO: invalid info object" );
    CONSTRUCT_ERRCODE( ompi_err_io, MPI_ERR_IO, "MPI_ERR_IO: input/output error" );
    CONSTRUCT_ERRCODE( ompi_err_keyval, MPI_ERR_KEYVAL, "MPI_ERR_KEYVAL: invalid key value" );
    CONSTRUCT_ERRCODE( ompi_err_locktype, MPI_ERR_LOCKTYPE, "MPI_ERR_LOCKTYPE: invalid lock" );
    CONSTRUCT_ERRCODE( ompi_err_name, MPI_ERR_NAME, "MPI_ERR_NAME: invalid name argument" );
    CONSTRUCT_ERRCODE( ompi_err_no_mem, MPI_ERR_NO_MEM, "MPI_ERR_NO_MEM: out of memory" );
    CONSTRUCT_ERRCODE( ompi_err_not_same, MPI_ERR_NOT_SAME, "MPI_ERR_NOT_SAME: objects are not identical");
    CONSTRUCT_ERRCODE( ompi_err_no_space, MPI_ERR_NO_SPACE, "MPI_ERR_NO_SPACE: no space left on device" );
    CONSTRUCT_ERRCODE( ompi_err_no_such_file, MPI_ERR_NO_SUCH_FILE, "MPI_ERR_NO_SUCH_FILE: no such file or directory" );
    CONSTRUCT_ERRCODE( ompi_err_port, MPI_ERR_PORT, "MPI_ERR_PORT: invalid port" );
    CONSTRUCT_ERRCODE( ompi_err_quota, MPI_ERR_QUOTA, "MPI_ERR_QUOTA: out of quota" );
    CONSTRUCT_ERRCODE( ompi_err_read_only, MPI_ERR_READ_ONLY, "MPI_ERR_READ_ONLY: file is read only" );
    CONSTRUCT_ERRCODE( ompi_err_rma_conflict, MPI_ERR_RMA_CONFLICT, "MPI_ERR_RMA_CONFLICT: rma conflict during operation" );
    CONSTRUCT_ERRCODE( ompi_err_rma_sync, MPI_ERR_RMA_SYNC, "MPI_ERR_RMA_SYNC: error executing rma sync" );
    CONSTRUCT_ERRCODE( ompi_err_service, MPI_ERR_SERVICE, "MPI_ERR_SERVICE: unknown service name" );
    CONSTRUCT_ERRCODE( ompi_err_size, MPI_ERR_SIZE, "MPI_ERR_SIZE: invalid size" );
    CONSTRUCT_ERRCODE( ompi_err_spawn, MPI_ERR_SPAWN, "MPI_ERR_SPAWN: could not spawn processes" );
    CONSTRUCT_ERRCODE( ompi_err_unsupported_datarep, MPI_ERR_UNSUPPORTED_DATAREP, "MPI_ERR_UNSUPPORTED_DATAREP: data representation not supported" );
    CONSTRUCT_ERRCODE( ompi_err_unsupported_operation, MPI_ERR_UNSUPPORTED_OPERATION, "MPI_ERR_UNSUPPORTED_OPERATION: operation not supported" );
    CONSTRUCT_ERRCODE( ompi_err_win, MPI_ERR_WIN, "MPI_ERR_WIN: invalid window" );

    ompi_mpi_errcode_lastused = MPI_ERR_WIN;
    ompi_mpi_errcode_lastpredefined = MPI_ERR_WIN;
    return OMPI_SUCCESS;
}

int ompi_mpi_errcode_finalize(void)
{
    int i;
    ompi_mpi_errcode_t *errc;
    
    for (i=ompi_mpi_errcode_lastpredefined+1; i<=ompi_mpi_errcode_lastused; i++) {
        /* 
         * there are some user defined error-codes, which
         * we have to free.
         */
        errc = (ompi_mpi_errcode_t *)opal_pointer_array_get_item(&ompi_mpi_errcodes, i);
        OBJ_RELEASE (errc);
    }

    OBJ_DESTRUCT(&ompi_success);
    OBJ_DESTRUCT(&ompi_err_buffer);
    OBJ_DESTRUCT(&ompi_err_count);
    OBJ_DESTRUCT(&ompi_err_type);
    OBJ_DESTRUCT(&ompi_err_tag);
    OBJ_DESTRUCT(&ompi_err_comm);
    OBJ_DESTRUCT(&ompi_err_rank);
    OBJ_DESTRUCT(&ompi_err_request);
    OBJ_DESTRUCT(&ompi_err_root);
    OBJ_DESTRUCT(&ompi_err_group);
    OBJ_DESTRUCT(&ompi_err_op);
    OBJ_DESTRUCT(&ompi_err_topology);
    OBJ_DESTRUCT(&ompi_err_dims);
    OBJ_DESTRUCT(&ompi_err_arg);
    OBJ_DESTRUCT(&ompi_err_unknown);
    OBJ_DESTRUCT(&ompi_err_truncate);
    OBJ_DESTRUCT(&ompi_err_other);
    OBJ_DESTRUCT(&ompi_err_intern);
    OBJ_DESTRUCT(&ompi_err_in_status);
    OBJ_DESTRUCT(&ompi_err_pending);
    OBJ_DESTRUCT(&ompi_err_access);
    OBJ_DESTRUCT(&ompi_err_amode);
    OBJ_DESTRUCT(&ompi_err_assert);
    OBJ_DESTRUCT(&ompi_err_bad_file);
    OBJ_DESTRUCT(&ompi_err_base);
    OBJ_DESTRUCT(&ompi_err_conversion);
    OBJ_DESTRUCT(&ompi_err_disp);
    OBJ_DESTRUCT(&ompi_err_dup_datarep);
    OBJ_DESTRUCT(&ompi_err_file_exists);
    OBJ_DESTRUCT(&ompi_err_file_in_use);
    OBJ_DESTRUCT(&ompi_err_file);
    OBJ_DESTRUCT(&ompi_err_info_key);
    OBJ_DESTRUCT(&ompi_err_info_nokey);
    OBJ_DESTRUCT(&ompi_err_info_value);
    OBJ_DESTRUCT(&ompi_err_info);
    OBJ_DESTRUCT(&ompi_err_io);
    OBJ_DESTRUCT(&ompi_err_keyval);
    OBJ_DESTRUCT(&ompi_err_locktype);
    OBJ_DESTRUCT(&ompi_err_name);
    OBJ_DESTRUCT(&ompi_err_no_mem);
    OBJ_DESTRUCT(&ompi_err_not_same);
    OBJ_DESTRUCT(&ompi_err_no_space);
    OBJ_DESTRUCT(&ompi_err_no_such_file);
    OBJ_DESTRUCT(&ompi_err_port);
    OBJ_DESTRUCT(&ompi_err_quota);
    OBJ_DESTRUCT(&ompi_err_read_only);
    OBJ_DESTRUCT(&ompi_err_rma_conflict);
    OBJ_DESTRUCT(&ompi_err_rma_sync);
    OBJ_DESTRUCT(&ompi_err_service);
    OBJ_DESTRUCT(&ompi_err_size);
    OBJ_DESTRUCT(&ompi_err_spawn);
    OBJ_DESTRUCT(&ompi_err_unsupported_datarep);
    OBJ_DESTRUCT(&ompi_err_unsupported_operation);
    OBJ_DESTRUCT(&ompi_err_win);

    OBJ_DESTRUCT(&ompi_mpi_errcodes);
    return OMPI_SUCCESS;
}

int ompi_mpi_errcode_add(int errclass )
{
    ompi_mpi_errcode_t *newerrcode;

    newerrcode = OBJ_NEW(ompi_mpi_errcode_t);
    newerrcode->code = (ompi_mpi_errcode_lastused+1);
    newerrcode->cls = errclass;
    opal_pointer_array_set_item(&ompi_mpi_errcodes, newerrcode->code, newerrcode);
    
    ompi_mpi_errcode_lastused++;
    return newerrcode->code;
}

int ompi_mpi_errclass_add(void)
{
    ompi_mpi_errcode_t *newerrcode;

    newerrcode = OBJ_NEW(ompi_mpi_errcode_t);
    newerrcode->cls = ( ompi_mpi_errcode_lastused+1);
    opal_pointer_array_set_item(&ompi_mpi_errcodes, newerrcode->cls, newerrcode);
    
    ompi_mpi_errcode_lastused++;
    return newerrcode->cls;
}

int ompi_mpi_errnum_add_string(int errnum, char *errstring, int len)
{
    ompi_mpi_errcode_t *errcodep;

    errcodep = (ompi_mpi_errcode_t *)opal_pointer_array_get_item(&ompi_mpi_errcodes, errnum);
    if ( NULL == errcodep ) { 
        return OMPI_ERROR;
    }

    if ( MPI_MAX_ERROR_STRING > len ) {
        len = MPI_MAX_ERROR_STRING;
    }
    
    strncpy ( errcodep->errstring, errstring, len );
    return OMPI_SUCCESS;
}

static void ompi_mpi_errcode_construct(ompi_mpi_errcode_t *errcode)
{
    errcode->code = MPI_UNDEFINED;
    errcode->cls = MPI_UNDEFINED;
    memset ( errcode->errstring, 0, MPI_MAX_ERROR_STRING);
    return;
}

static void ompi_mpi_errcode_destruct(ompi_mpi_errcode_t *errcode)
{
    if (MPI_UNDEFINED != errcode->code) {
        opal_pointer_array_set_item(&ompi_mpi_errcodes, errcode->code, NULL);
    } else if (MPI_UNDEFINED != errcode->cls) {
        opal_pointer_array_set_item(&ompi_mpi_errcodes, errcode->cls, NULL);
    }
}
