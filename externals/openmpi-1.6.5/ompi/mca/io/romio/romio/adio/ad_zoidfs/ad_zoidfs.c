/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 2003 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_zoidfs.h"

/* adioi.h has the ADIOI_Fns_struct define */
#include "adioi.h"

struct ADIOI_Fns_struct ADIO_ZOIDFS_operations = {
    ADIOI_ZOIDFS_Open, /* Open */
    ADIOI_SCALEABLE_OpenColl, /* OpenColl */
    ADIOI_ZOIDFS_ReadContig, /* ReadContig */
    ADIOI_ZOIDFS_WriteContig, /* WriteContig */
    ADIOI_GEN_ReadStridedColl, /* ReadStridedColl */
    ADIOI_GEN_WriteStridedColl, /* WriteStridedColl */
    ADIOI_GEN_SeekIndividual, /* SeekIndividual */
    ADIOI_ZOIDFS_Fcntl, /* Fcntl */
    ADIOI_GEN_SetInfo, /* SetInfo */
    ADIOI_GEN_ReadStrided, /* ReadStrided */
    ADIOI_ZOIDFS_WriteStrided, /* WriteStrided */
    ADIOI_ZOIDFS_Close, /* Close */
    ADIOI_FAKE_IreadContig, /* IreadContig */
    ADIOI_FAKE_IwriteContig, /* IwriteContig */
    ADIOI_FAKE_IODone, /* ReadDone */
    ADIOI_FAKE_IODone, /* WriteDone */
    ADIOI_FAKE_IOComplete, /* ReadComplete */
    ADIOI_FAKE_IOComplete, /* WriteComplete */
    ADIOI_FAKE_IreadStrided, /* IreadStrided */
    ADIOI_FAKE_IwriteStrided, /* IwriteStrided */
    ADIOI_ZOIDFS_Flush, /* Flush */
    ADIOI_ZOIDFS_Resize, /* Resize */
    ADIOI_ZOIDFS_Delete, /* Delete */
    ADIOI_ZOIDFS_Feature,
};

/* 
 * vim: ts=8 sts=4 sw=4 noexpandtab 
 */
