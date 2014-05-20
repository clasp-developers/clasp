/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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

#ifndef OPAL_CONSTANTS_H
#define OPAL_CONSTANTS_H
 
/* error codes - don't forget to update opal/rutime/opal_init.c when 
   adding to this list */
#define OPAL_ERR_BASE             0 /* internal use only */
 
enum {
    OPAL_SUCCESS                  = (OPAL_ERR_BASE),

    OPAL_ERROR                    = (OPAL_ERR_BASE -  1),
    OPAL_ERR_OUT_OF_RESOURCE      = (OPAL_ERR_BASE -  2), /* fatal error */
    OPAL_ERR_TEMP_OUT_OF_RESOURCE = (OPAL_ERR_BASE -  3), /* try again later */
    OPAL_ERR_RESOURCE_BUSY        = (OPAL_ERR_BASE -  4),
    OPAL_ERR_BAD_PARAM            = (OPAL_ERR_BASE -  5),  /* equivalent to MPI_ERR_ARG error code */
    OPAL_ERR_FATAL                = (OPAL_ERR_BASE -  6),
    OPAL_ERR_NOT_IMPLEMENTED      = (OPAL_ERR_BASE -  7),
    OPAL_ERR_NOT_SUPPORTED        = (OPAL_ERR_BASE -  8),
    OPAL_ERR_INTERUPTED           = (OPAL_ERR_BASE -  9),
    OPAL_ERR_WOULD_BLOCK          = (OPAL_ERR_BASE - 10),
    OPAL_ERR_IN_ERRNO             = (OPAL_ERR_BASE - 11),
    OPAL_ERR_UNREACH              = (OPAL_ERR_BASE - 12),
    OPAL_ERR_NOT_FOUND            = (OPAL_ERR_BASE - 13),
    OPAL_EXISTS                   = (OPAL_ERR_BASE - 14), /* indicates that the specified object already exists */
    OPAL_ERR_TIMEOUT              = (OPAL_ERR_BASE - 15),
    OPAL_ERR_NOT_AVAILABLE        = (OPAL_ERR_BASE - 16),
    OPAL_ERR_PERM                 = (OPAL_ERR_BASE - 17), /* no permission */
    OPAL_ERR_VALUE_OUT_OF_BOUNDS  = (OPAL_ERR_BASE - 18),
    OPAL_ERR_FILE_READ_FAILURE    = (OPAL_ERR_BASE - 19),
    OPAL_ERR_FILE_WRITE_FAILURE   = (OPAL_ERR_BASE - 20),
    OPAL_ERR_FILE_OPEN_FAILURE    = (OPAL_ERR_BASE - 21),
    OPAL_ERR_PACK_MISMATCH        = (OPAL_ERR_BASE - 22),
    OPAL_ERR_PACK_FAILURE         = (OPAL_ERR_BASE - 23),
    OPAL_ERR_UNPACK_FAILURE       = (OPAL_ERR_BASE - 24),
    OPAL_ERR_UNPACK_INADEQUATE_SPACE        = (OPAL_ERR_BASE - 25),
    OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER = (OPAL_ERR_BASE - 26),
    OPAL_ERR_TYPE_MISMATCH                  = (OPAL_ERR_BASE - 27),
    OPAL_ERR_OPERATION_UNSUPPORTED          = (OPAL_ERR_BASE - 28),
    OPAL_ERR_UNKNOWN_DATA_TYPE              = (OPAL_ERR_BASE - 29),
    OPAL_ERR_BUFFER                         = (OPAL_ERR_BASE - 30),
    OPAL_ERR_DATA_TYPE_REDEF                = (OPAL_ERR_BASE - 31),
    OPAL_ERR_DATA_OVERWRITE_ATTEMPT         = (OPAL_ERR_BASE - 32)
};

#define OPAL_ERR_MAX                (OPAL_ERR_BASE - 100)

#endif /* OPAL_CONSTANTS_H */

