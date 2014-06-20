/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_bgl_pset.c
 * \brief Definition of functions associated to structs ADIOI_BGL_ProcInfo_t and ADIOI_BGL_ConfInfo_t 
 */

/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include <stdlib.h>
#include "ad_bgl.h"
#include "ad_bgl_pset.h"
#include "mpidimpl.h"

ADIOI_BGL_ProcInfo_t *
ADIOI_BGL_ProcInfo_new()
{
    ADIOI_BGL_ProcInfo_t *p = (ADIOI_BGL_ProcInfo_t *) ADIOI_Malloc (sizeof(ADIOI_BGL_ProcInfo_t));
    AD_BGL_assert ((p != NULL));
    return p;
}

ADIOI_BGL_ProcInfo_t *
ADIOI_BGL_ProcInfo_new_n( int n )
{
    ADIOI_BGL_ProcInfo_t *p = (ADIOI_BGL_ProcInfo_t *) ADIOI_Malloc (n * sizeof(ADIOI_BGL_ProcInfo_t));
    AD_BGL_assert ((p != NULL));
    return p;
}

void
ADIOI_BGL_ProcInfo_free( ADIOI_BGL_ProcInfo_t *info )
{
    if (info != NULL) ADIOI_Free (info);
}

static
void 
ADIOI_BGL_ProcInfo_set(ADIOI_BGL_ProcInfo_t *info, const DCMF_Hardware_t *hw, int r)
{
    info->psetNum    = hw->idOfPset;
    info->xInPset    = hw->xCoord;
    info->yInPset    = hw->yCoord;
    info->zInPset    = hw->zCoord;
    info->cpuid      = hw->tCoord;
    info->rank       = r;
    info->rankInPset = hw->rankInPset;
}


ADIOI_BGL_ConfInfo_t *
ADIOI_BGL_ConfInfo_new ()
{
    ADIOI_BGL_ConfInfo_t *p = (ADIOI_BGL_ConfInfo_t *) ADIOI_Malloc (sizeof(ADIOI_BGL_ConfInfo_t));
    AD_BGL_assert ((p != NULL));
    return p;
}

static
void
ADIOI_BGL_ConfInfo_set(ADIOI_BGL_ConfInfo_t *info, const DCMF_Hardware_t *hw, int s, int n_aggrs)
{
    info->PsetSize        = hw->sizeOfPset;
    info->numPsets        = (hw->xSize * hw->ySize *
					hw->zSize) / hw->sizeOfPset;
    info->isVNM           = (hw->tSize != 1);
    info->cpuidSize       = hw->tSize;
    info->virtualPsetSize = hw->sizeOfPset * hw->tSize;
    info->nProcs          = s;

    /* More complicated logic maybe needed for nAggrs specification */
    info->nAggrs          = n_aggrs;
    if ( info->nAggrs <=0 || MIN(info->nProcs, info->virtualPsetSize) < info->nAggrs ) 
        info->nAggrs      = ADIOI_BGL_NAGG_PSET_DFLT;
    if ( info->nAggrs > info->virtualPsetSize ) info->nAggrs = info->virtualPsetSize;

    info->aggRatio        = 1. * info->nAggrs / info->virtualPsetSize;
    if (info->aggRatio > 1) info->aggRatio = 1.;
}

void
ADIOI_BGL_ConfInfo_free( ADIOI_BGL_ConfInfo_t *info )
{
    if (info != NULL) ADIOI_Free (info);
}

void 
ADIOI_BGL_persInfo_init(ADIOI_BGL_ConfInfo_t *conf, 
			ADIOI_BGL_ProcInfo_t *proc, 
			int s, int r, int n_aggrs)
{
    DCMF_Hardware_t hw;
    DCMF_Hardware(&hw);

    ADIOI_BGL_ConfInfo_set (conf, &hw, s, n_aggrs);
    ADIOI_BGL_ProcInfo_set (proc, &hw, r);
}

void 
ADIOI_BGL_persInfo_free( ADIOI_BGL_ConfInfo_t *conf, ADIOI_BGL_ProcInfo_t *proc )
{
    ADIOI_BGL_ConfInfo_free( conf );
    ADIOI_BGL_ProcInfo_free( proc );
}
