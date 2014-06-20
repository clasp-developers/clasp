/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_zoidfs.h"

void ADIOI_ZOIDFS_Close(ADIO_File fd, int *error_code)
{
    ADIOI_Free(fd->fs_ptr);
    fd->fs_ptr = NULL;

    /* At some point or another it was decided that ROMIO would not
     * explicitly flush (other than any local cache) on close, because
     * there is no way to *avoid* that overhead if you implement it here
     * and don't actually want it.
     */

    *error_code = MPI_SUCCESS;
}
/* 
 * vim: ts=8 sts=4 sw=4 noexpandtab 
 */
