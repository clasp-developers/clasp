/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_bgl.c
 * \brief ???
 */

/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 2001 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_bgl.h"

/* adioi.h has the ADIOI_Fns_struct define */
#include "adioi.h"

struct ADIOI_Fns_struct ADIO_BGL_operations = {
    ADIOI_BGL_Open, /* Open */
    ADIOI_GEN_OpenColl, /* Collective open */
    ADIOI_BGL_ReadContig, /* ReadContig */
    ADIOI_BGL_WriteContig, /* WriteContig */
#if BGL_OPTIM_STEP1_2
    ADIOI_BGL_ReadStridedColl, /* ReadStridedColl */
    ADIOI_BGL_WriteStridedColl, /* WriteStridedColl */
#else
    ADIOI_GEN_ReadStridedColl, /* ReadStridedColl */
    ADIOI_GEN_WriteStridedColl, /* WriteStridedColl */
#endif
    ADIOI_GEN_SeekIndividual, /* SeekIndividual */
    ADIOI_BGL_Fcntl, /* Fcntl */
#if BGL_OPTIM_STEP1_1
    ADIOI_BGL_SetInfo, /* SetInfo */
#else
    ADIOI_GEN_SetInfo, /* SetInfo */
#endif
    ADIOI_BGL_ReadStrided, /* ReadStrided */
    ADIOI_BGL_WriteStrided, /* WriteStrided */
    ADIOI_BGL_Close, /* Close */
#ifdef ROMIO_HAVE_WORKING_AIO
#warning Consider BG support for NFS before enabling this.
    ADIOI_GEN_IreadContig, /* IreadContig */
    ADIOI_GEN_IwriteContig, /* IwriteContig */
#else
    ADIOI_FAKE_IreadContig, /* IreadContig */
    ADIOI_FAKE_IwriteContig, /* IwriteContig */
#endif
    ADIOI_GEN_IODone, /* ReadDone */
    ADIOI_GEN_IODone, /* WriteDone */
    ADIOI_GEN_IOComplete, /* ReadComplete */
    ADIOI_GEN_IOComplete, /* WriteComplete */
    ADIOI_GEN_IreadStrided, /* IreadStrided */
    ADIOI_GEN_IwriteStrided, /* IwriteStrided */
    ADIOI_BGL_Flush, /* Flush */
    ADIOI_GEN_Resize, /* Resize */
    ADIOI_GEN_Delete, /* Delete */
    ADIOI_GEN_Feature, /* Features */
};
