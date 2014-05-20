/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#ifndef ORTE_CONSTANTS_H
#define ORTE_CONSTANTS_H

#include "opal/constants.h"
#include "orte_config.h"

BEGIN_C_DECLS

#define ORTE_ERR_BASE            OPAL_ERR_MAX


enum {
    /* Error codes inherited from OPAL.  Still enum values so that we
       get the nice debugger help. */

    ORTE_SUCCESS                  = OPAL_SUCCESS,

    ORTE_ERROR                    = OPAL_ERROR,
    ORTE_ERR_OUT_OF_RESOURCE      = OPAL_ERR_OUT_OF_RESOURCE,
    ORTE_ERR_TEMP_OUT_OF_RESOURCE = OPAL_ERR_TEMP_OUT_OF_RESOURCE,
    ORTE_ERR_RESOURCE_BUSY        = OPAL_ERR_RESOURCE_BUSY,
    ORTE_ERR_BAD_PARAM            = OPAL_ERR_BAD_PARAM,
    ORTE_ERR_FATAL                = OPAL_ERR_FATAL,
    ORTE_ERR_NOT_IMPLEMENTED      = OPAL_ERR_NOT_IMPLEMENTED,
    ORTE_ERR_NOT_SUPPORTED        = OPAL_ERR_NOT_SUPPORTED,
    ORTE_ERR_INTERUPTED           = OPAL_ERR_INTERUPTED,
    ORTE_ERR_WOULD_BLOCK          = OPAL_ERR_WOULD_BLOCK,
    ORTE_ERR_IN_ERRNO             = OPAL_ERR_IN_ERRNO,
    ORTE_ERR_UNREACH              = OPAL_ERR_UNREACH,
    ORTE_ERR_NOT_FOUND            = OPAL_ERR_NOT_FOUND,
    ORTE_EXISTS                   = OPAL_EXISTS,
    ORTE_ERR_TIMEOUT              = OPAL_ERR_TIMEOUT,
    ORTE_ERR_NOT_AVAILABLE        = OPAL_ERR_NOT_AVAILABLE,
    ORTE_ERR_PERM                 = OPAL_ERR_PERM,
    ORTE_ERR_VALUE_OUT_OF_BOUNDS  = OPAL_ERR_VALUE_OUT_OF_BOUNDS,
    ORTE_ERR_FILE_READ_FAILURE    = OPAL_ERR_FILE_READ_FAILURE,
    ORTE_ERR_FILE_WRITE_FAILURE   = OPAL_ERR_FILE_WRITE_FAILURE,
    ORTE_ERR_FILE_OPEN_FAILURE    = OPAL_ERR_FILE_OPEN_FAILURE,
    ORTE_ERR_PACK_MISMATCH        = OPAL_ERR_PACK_MISMATCH,
    ORTE_ERR_PACK_FAILURE         = OPAL_ERR_PACK_FAILURE,
    ORTE_ERR_UNPACK_FAILURE       = OPAL_ERR_UNPACK_FAILURE,
    ORTE_ERR_UNPACK_INADEQUATE_SPACE = OPAL_ERR_UNPACK_INADEQUATE_SPACE,
    ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER = OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER,
    ORTE_ERR_TYPE_MISMATCH        = OPAL_ERR_TYPE_MISMATCH,
    ORTE_ERR_OPERATION_UNSUPPORTED = OPAL_ERR_OPERATION_UNSUPPORTED, 
    ORTE_ERR_UNKNOWN_DATA_TYPE    = OPAL_ERR_UNKNOWN_DATA_TYPE,
    ORTE_ERR_BUFFER               = OPAL_ERR_BUFFER,
    ORTE_ERR_DATA_TYPE_REDEF      = OPAL_ERR_DATA_TYPE_REDEF,
    ORTE_ERR_DATA_OVERWRITE_ATTEMPT = OPAL_ERR_DATA_OVERWRITE_ATTEMPT,
/* error codes specific to ORTE - don't forget to update
    orte/util/error_strings.c when adding new error codes!!
    Otherwise, the error reporting system will potentially crash,
    or at the least not be able to report the new error correctly.
 */
    ORTE_ERR_RECV_LESS_THAN_POSTED          = (ORTE_ERR_BASE -  1),
    ORTE_ERR_RECV_MORE_THAN_POSTED          = (ORTE_ERR_BASE -  2),
    ORTE_ERR_NO_MATCH_YET                   = (ORTE_ERR_BASE -  3),
    ORTE_ERR_REQUEST                        = (ORTE_ERR_BASE -  4),
    ORTE_ERR_NO_CONNECTION_ALLOWED          = (ORTE_ERR_BASE -  5),
    ORTE_ERR_CONNECTION_REFUSED             = (ORTE_ERR_BASE -  6),
    ORTE_ERR_CONNECTION_FAILED              = (ORTE_ERR_BASE -  7),
    ORTE_ERR_COMM_FAILURE                   = (ORTE_ERR_BASE -  8),
    ORTE_ERR_COMPARE_FAILURE                = (ORTE_ERR_BASE -  9),
    ORTE_ERR_COPY_FAILURE                   = (ORTE_ERR_BASE - 10),
    ORTE_ERR_PROC_STATE_MISSING             = (ORTE_ERR_BASE - 11),
    ORTE_ERR_PROC_EXIT_STATUS_MISSING       = (ORTE_ERR_BASE - 12),
    ORTE_ERR_INDETERMINATE_STATE_INFO       = (ORTE_ERR_BASE - 13),
    ORTE_ERR_NODE_FULLY_USED                = (ORTE_ERR_BASE - 14),
    ORTE_ERR_INVALID_NUM_PROCS              = (ORTE_ERR_BASE - 15),
    ORTE_ERR_SILENT                         = (ORTE_ERR_BASE - 16),
    ORTE_ERR_ADDRESSEE_UNKNOWN              = (ORTE_ERR_BASE - 17),
    ORTE_ERR_SYS_LIMITS_PIPES               = (ORTE_ERR_BASE - 18),
    ORTE_ERR_PIPE_SETUP_FAILURE             = (ORTE_ERR_BASE - 19),
    ORTE_ERR_SYS_LIMITS_CHILDREN            = (ORTE_ERR_BASE - 20),
    ORTE_ERR_FAILED_GET_TERM_ATTRS          = (ORTE_ERR_BASE - 21),
    ORTE_ERR_WDIR_NOT_FOUND                 = (ORTE_ERR_BASE - 22),
    ORTE_ERR_EXE_NOT_FOUND                  = (ORTE_ERR_BASE - 23),
    ORTE_ERR_PIPE_READ_FAILURE              = (ORTE_ERR_BASE - 24),
    ORTE_ERR_EXE_NOT_ACCESSIBLE             = (ORTE_ERR_BASE - 25),
    ORTE_ERR_FAILED_TO_START                = (ORTE_ERR_BASE - 26),
    ORTE_ERR_FILE_NOT_EXECUTABLE            = (ORTE_ERR_BASE - 27),
    ORTE_ERR_HNP_COULD_NOT_START            = (ORTE_ERR_BASE - 28),
    ORTE_ERR_SYS_LIMITS_SOCKETS             = (ORTE_ERR_BASE - 29),
    ORTE_ERR_SOCKET_NOT_AVAILABLE           = (ORTE_ERR_BASE - 30),
    ORTE_ERR_SYSTEM_WILL_BOOTSTRAP          = (ORTE_ERR_BASE - 31),
    ORTE_ERR_COMM_DISABLED                  = (ORTE_ERR_BASE - 32)
};

#define ORTE_ERR_MAX                      (ORTE_ERR_BASE - 100)

/* include the prototype for the error-to-string converter */
ORTE_DECLSPEC const char* orte_err2str(int errnum);

END_C_DECLS

#endif /* ORTE_CONSTANTS_H */

