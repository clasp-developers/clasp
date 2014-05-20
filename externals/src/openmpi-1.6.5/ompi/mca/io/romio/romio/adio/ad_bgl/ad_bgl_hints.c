/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_bgl_hints.c
 * \brief ???
 */

/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "adio_extern.h"

#include "ad_bgl.h"
#include "ad_bgl_pset.h"
#include "ad_bgl_aggrs.h"

#define   ADIOI_BGL_CB_BUFFER_SIZE_DFLT      	"16777216"
#define	  ADIOI_BGL_IND_RD_BUFFER_SIZE_DFLT	"4194304"
#define   ADIOI_BGL_IND_WR_BUFFER_SIZE_DFLT	"4194304"
#define   ADIOI_BGL_NAGG_IN_PSET_HINT_NAME	"bgl_nodes_pset"

/* Compute the aggregator-related parameters that are required in 2-phase collective IO of ADIO. */
extern int 
ADIOI_BGL_gen_agg_ranklist(ADIO_File fd, int n_proxy_per_pset);

void ADIOI_BGL_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code)
{
/* if fd->info is null, create a new info object. 
   Initialize fd->info to default values.
   Initialize fd->hints to default values.
   Examine the info object passed by the user. If it contains values that
   ROMIO understands, override the default. */

    MPI_Info info;
    char *value;
    int flag, intval, tmp_val, nprocs=0, nprocs_is_valid = 0;
    static char myname[] = "ADIOI_BGL_SETINFO";

    int did_anything = 0;

    if (fd->info == MPI_INFO_NULL) MPI_Info_create(&(fd->info));
    info = fd->info;

    /* Note that fd->hints is allocated at file open time; thus it is
     * not necessary to allocate it, or check for allocation, here.
     */

    value = (char *) ADIOI_Malloc((MPI_MAX_INFO_VAL+1)*sizeof(char));
    AD_BGL_assert ((value != NULL));

    /* initialize info and hints to default values if they haven't been
     * previously initialized
     */
    if (!fd->hints->initialized) {

	did_anything = 1;

	/* buffer size for collective I/O */
	ADIOI_Info_set(info, "cb_buffer_size", ADIOI_BGL_CB_BUFFER_SIZE_DFLT); 
	fd->hints->cb_buffer_size = atoi(ADIOI_BGL_CB_BUFFER_SIZE_DFLT);

	/* default is to let romio automatically decide when to use
	 * collective buffering
	 */
	ADIOI_Info_set(info, "romio_cb_read", "enable"); 
	fd->hints->cb_read = ADIOI_HINT_ENABLE;
	ADIOI_Info_set(info, "romio_cb_write", "enable"); 
	fd->hints->cb_write = ADIOI_HINT_ENABLE;

   	if ( fd->hints->cb_config_list != NULL ) ADIOI_Free (fd->hints->cb_config_list);
	fd->hints->cb_config_list = NULL;

	/* number of processes that perform I/O in collective I/O */
	MPI_Comm_size(fd->comm, &nprocs);
	nprocs_is_valid = 1;
	ADIOI_Snprintf(value, MPI_MAX_INFO_VAL+1, "%d", nprocs);
	ADIOI_Info_set(info, "cb_nodes", value);
	fd->hints->cb_nodes = -1;

	/* hint indicating that no indep. I/O will be performed on this file */
	ADIOI_Info_set(info, "romio_no_indep_rw", "false");
	fd->hints->no_indep_rw = 0;

	/* bgl is not implementing file realms (ADIOI_IOStridedColl),
	   initialize to disabled it. 	   */
	/* hint instructing the use of persistent file realms */
	ADIOI_Info_set(info, "romio_cb_pfr", "disable");
	fd->hints->cb_pfr = ADIOI_HINT_DISABLE;
	
	/* hint guiding the assignment of persistent file realms */
	ADIOI_Info_set(info, "romio_cb_fr_types", "aar");
	fd->hints->cb_fr_type = ADIOI_FR_AAR;

	/* hint to align file realms with a certain byte value */
	ADIOI_Info_set(info, "romio_cb_fr_alignment", "1");
	fd->hints->cb_fr_alignment = 1;

	/* hint to set a threshold percentage for a datatype's size/extent at
	 * which data sieving should be done in collective I/O */
	ADIOI_Info_set(info, "romio_cb_ds_threshold", "0");
	fd->hints->cb_ds_threshold = 0;

	/* hint to switch between point-to-point or all-to-all for two-phase */
	ADIOI_Info_set(info, "romio_cb_alltoall", "automatic");
	fd->hints->cb_alltoall = ADIOI_HINT_AUTO;

	 /* deferred_open derived from no_indep_rw and cb_{read,write} */
	fd->hints->deferred_open = 0;

	/* buffer size for data sieving in independent reads */
	ADIOI_Info_set(info, "ind_rd_buffer_size", ADIOI_BGL_IND_RD_BUFFER_SIZE_DFLT);
	fd->hints->ind_rd_buffer_size = atoi(ADIOI_BGL_IND_RD_BUFFER_SIZE_DFLT);

	/* buffer size for data sieving in independent writes */
	ADIOI_Info_set(info, "ind_wr_buffer_size", ADIOI_BGL_IND_WR_BUFFER_SIZE_DFLT);
	fd->hints->ind_wr_buffer_size = atoi(ADIOI_BGL_IND_WR_BUFFER_SIZE_DFLT);

  if(fd->file_system == ADIO_UFS)
  {
    /* default for ufs/pvfs is to disable data sieving  */
    ADIOI_Info_set(info, "romio_ds_read", "disable"); 
    fd->hints->ds_read = ADIOI_HINT_DISABLE;
    ADIOI_Info_set(info, "romio_ds_write", "disable"); 
    fd->hints->ds_write = ADIOI_HINT_DISABLE;
  }
  else
  {
    /* default is to let romio automatically decide when to use data
     * sieving
     */
    ADIOI_Info_set(info, "romio_ds_read", "automatic"); 
    fd->hints->ds_read = ADIOI_HINT_AUTO;
    ADIOI_Info_set(info, "romio_ds_write", "automatic"); 
    fd->hints->ds_write = ADIOI_HINT_AUTO;
  }

    /* still to do: tune this a bit for a variety of file systems. there's
	 * no good default value so just leave it unset */
    fd->hints->min_fdomain_size = 0;
    fd->hints->striping_unit = 0;

    fd->hints->initialized = 1;
    }

    /* add in user's info if supplied */
    if (users_info != MPI_INFO_NULL) {
	ADIOI_Info_get(users_info, "cb_buffer_size", MPI_MAX_INFO_VAL, 
		     value, &flag);
	if (flag && ((intval=atoi(value)) > 0)) {
	    tmp_val = intval;

	    MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
	    /* --BEGIN ERROR HANDLING-- */
	    if (tmp_val != intval) {
		MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
						   "cb_buffer_size",
						   error_code);
		return;
	    }
	    /* --END ERROR HANDLING-- */

	    ADIOI_Info_set(info, "cb_buffer_size", value);
	    fd->hints->cb_buffer_size = intval;

	}
#if 0
	/* bgl is not implementing file realms (ADIOI_IOStridedColl) ... */
	/* aligning file realms to certain sizes (e.g. stripe sizes)
	 * may benefit I/O performance */
	ADIOI_Info_get(users_info, "romio_cb_fr_alignment", MPI_MAX_INFO_VAL, 
		     value, &flag);
	if (flag && ((intval=atoi(value)) > 0)) {
	    tmp_val = intval;

	    MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
	    /* --BEGIN ERROR HANDLING-- */
	    if (tmp_val != intval) {
		MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
						   "romio_cb_fr_alignment",
						   error_code);
		return;
	    }
	    /* --END ERROR HANDLING-- */

	    ADIOI_Info_set(info, "romio_cb_fr_alignment", value);
	    fd->hints->cb_fr_alignment = intval;

	}

	/* for collective I/O, try to be smarter about when to do data sieving
	 * using a specific threshold for the datatype size/extent
	 * (percentage 0-100%) */
	ADIOI_Info_get(users_info, "romio_cb_ds_threshold", MPI_MAX_INFO_VAL, 
		     value, &flag);
	if (flag && ((intval=atoi(value)) > 0)) {
	    tmp_val = intval;

	    MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
	    /* --BEGIN ERROR HANDLING-- */
	    if (tmp_val != intval) {
		MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
						   "romio_cb_ds_threshold",
						   error_code);
		return;
	    }
	    /* --END ERROR HANDLING-- */

	    ADIOI_Info_set(info, "romio_cb_ds_threshold", value);
	    fd->hints->cb_ds_threshold = intval;

	}
	ADIOI_Info_get(users_info, "romio_cb_alltoall", MPI_MAX_INFO_VAL, value,
		     &flag);
	if (flag) {
	    if (!strcmp(value, "enable") || !strcmp(value, "ENABLE")) {
		ADIOI_Info_set(info, "romio_cb_alltoall", value);
		fd->hints->cb_read = ADIOI_HINT_ENABLE;
	    }
	    else if (!strcmp(value, "disable") || !strcmp(value, "DISABLE")) {
		ADIOI_Info_set(info, "romio_cb_alltoall", value);
		fd->hints->cb_read = ADIOI_HINT_DISABLE;
	    }
	    else if (!strcmp(value, "automatic") || !strcmp(value, "AUTOMATIC"))
	    {
		ADIOI_Info_set(info, "romio_cb_alltoall", value);
		fd->hints->cb_read = ADIOI_HINT_AUTO;
	    }

	    tmp_val = fd->hints->cb_alltoall;

	    MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
	    /* --BEGIN ERROR HANDLING-- */
	    if (tmp_val != fd->hints->cb_alltoall) {
		MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
						   "romio_cb_alltoall",
						   error_code);
		return;
	    }
	    /* --END ERROR HANDLING-- */
	}
#endif
	/* new hints for enabling/disabling coll. buffering on
	 * reads/writes
	 */
	ADIOI_Info_get(users_info, "romio_cb_read", MPI_MAX_INFO_VAL, value,
		     &flag);
	if (flag) {
	    if (!strcmp(value, "enable") || !strcmp(value, "ENABLE")) {
		ADIOI_Info_set(info, "romio_cb_read", value);
		fd->hints->cb_read = ADIOI_HINT_ENABLE;
	    }
	    else if (!strcmp(value, "disable") || !strcmp(value, "DISABLE")) {
		    /* romio_cb_read overrides no_indep_rw */
		ADIOI_Info_set(info, "romio_cb_read", value);
		ADIOI_Info_set(info, "romio_no_indep_rw", "false");
		fd->hints->cb_read = ADIOI_HINT_DISABLE;
		fd->hints->no_indep_rw = ADIOI_HINT_DISABLE;
	    }
	    else if (!strcmp(value, "automatic") || !strcmp(value, "AUTOMATIC"))
	    {
		ADIOI_Info_set(info, "romio_cb_read", value);
		fd->hints->cb_read = ADIOI_HINT_AUTO;
	    }

	    tmp_val = fd->hints->cb_read;

	    MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
	    /* --BEGIN ERROR HANDLING-- */
	    if (tmp_val != fd->hints->cb_read) {
		MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
						   "romio_cb_read",
						   error_code);
		return;
	    }
	    /* --END ERROR HANDLING-- */
	}
	ADIOI_Info_get(users_info, "romio_cb_write", MPI_MAX_INFO_VAL, value,
		     &flag);
	if (flag) {
	    if (!strcmp(value, "enable") || !strcmp(value, "ENABLE")) {
		ADIOI_Info_set(info, "romio_cb_write", value);
		fd->hints->cb_write = ADIOI_HINT_ENABLE;
	    }
	    else if (!strcmp(value, "disable") || !strcmp(value, "DISABLE"))
	    {
		/* romio_cb_write overrides no_indep_rw, too */
		ADIOI_Info_set(info, "romio_cb_write", value);
		ADIOI_Info_set(info, "romio_no_indep_rw", "false");
		fd->hints->cb_write = ADIOI_HINT_DISABLE;
		fd->hints->no_indep_rw = ADIOI_HINT_DISABLE;
	    }
	    else if (!strcmp(value, "automatic") ||
		     !strcmp(value, "AUTOMATIC"))
	    {
		ADIOI_Info_set(info, "romio_cb_write", value);
		fd->hints->cb_write = ADIOI_HINT_AUTO;
	    }
	
	    tmp_val = fd->hints->cb_write;

	    MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
	    /* --BEGIN ERROR HANDLING-- */
	    if (tmp_val != fd->hints->cb_write) {
		MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
						   "romio_cb_write",
						   error_code);
		return;
	    }
	    /* --END ERROR HANDLING-- */
	}

#if 0
	/* bgl is not implementing file realms (ADIOI_IOStridedColl) ... */
	/* enable/disable persistent file realms for collective I/O */
	/* may want to check for no_indep_rdwr hint as well */
	ADIOI_Info_get(users_info, "romio_cb_pfr", MPI_MAX_INFO_VAL, value,
		     &flag);
	if (flag) {
	    if (!strcmp(value, "enable") || !strcmp(value, "ENABLE")) {
		ADIOI_Info_set(info, "romio_cb_pfr", value);
		fd->hints->cb_pfr = ADIOI_HINT_ENABLE;
	    }
	    else if (!strcmp(value, "disable") || !strcmp(value, "DISABLE")) {
		ADIOI_Info_set(info, "romio_cb_pfr", value);
		fd->hints->cb_pfr = ADIOI_HINT_DISABLE;
	    }
	    else if (!strcmp(value, "automatic") || !strcmp(value, "AUTOMATIC"))
	    {
		ADIOI_Info_set(info, "romio_cb_pfr", value);
		fd->hints->cb_pfr = ADIOI_HINT_AUTO;
	    }

	    tmp_val = fd->hints->cb_pfr;

	    MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
	    /* --BEGIN ERROR HANDLING-- */
	    if (tmp_val != fd->hints->cb_pfr) {
		MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
						   "romio_cb_pfr",
						   error_code);
		return;
	    }
	    /* --END ERROR HANDLING-- */
	}

	/* file realm assignment types ADIOI_FR_AAR(0),
	 ADIOI_FR_FSZ(-1), ADIOI_FR_USR_REALMS(-2), all others specify
	 a regular fr size in bytes. probably not the best way... */
	ADIOI_Info_get(users_info, "romio_cb_fr_type", MPI_MAX_INFO_VAL, 
		     value, &flag);
	if (flag && ((intval=atoi(value)) >= -2)) {
	    tmp_val = intval;

	    MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
	    /* --BEGIN ERROR HANDLING-- */
	    if (tmp_val != intval) {
		MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
						   "romio_cb_fr_type",
						   error_code);
		return;
	    }
	    /* --END ERROR HANDLING-- */

	    ADIOI_Info_set(info, "romio_cb_fr_type", value);
	    fd->hints->cb_fr_type = intval;

	}
#endif
	/* new hint for specifying no indep. read/write will be performed */
	ADIOI_Info_get(users_info, "romio_no_indep_rw", MPI_MAX_INFO_VAL, value,
		     &flag);
	if (flag) {
	    if (!strcmp(value, "true") || !strcmp(value, "TRUE")) {
		    /* if 'no_indep_rw' set, also hint that we will do
		     * collective buffering: if we aren't doing independent io,
		     * then we have to do collective  */
		ADIOI_Info_set(info, "romio_no_indep_rw", value);
		ADIOI_Info_set(info, "romio_cb_write", "enable");
		ADIOI_Info_set(info, "romio_cb_read", "enable");
		fd->hints->no_indep_rw = 1;
		fd->hints->cb_read = 1;
		fd->hints->cb_write = 1;
		tmp_val = 1;
	    }
	    else if (!strcmp(value, "false") || !strcmp(value, "FALSE")) {
		ADIOI_Info_set(info, "romio_no_indep_rw", value);
		fd->hints->no_indep_rw = 0;
		tmp_val = 0;
	    }
	    else {
		/* default is above */
		tmp_val = 0;
	    }

	    MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
	    /* --BEGIN ERROR HANDLING-- */
	    if (tmp_val != fd->hints->no_indep_rw) {
		MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
						   "romio_no_indep_rw",
						   error_code);
		return;
	    }
	    /* --END ERROR HANDLING-- */
	}
	/* new hints for enabling/disabling data sieving on
	 * reads/writes
	 */
	ADIOI_Info_get(users_info, "romio_ds_read", MPI_MAX_INFO_VAL, value, 
		     &flag);
	if (flag) {
	    if (!strcmp(value, "enable") || !strcmp(value, "ENABLE")) {
		ADIOI_Info_set(info, "romio_ds_read", value);
		fd->hints->ds_read = ADIOI_HINT_ENABLE;
	    }
	    else if (!strcmp(value, "disable") || !strcmp(value, "DISABLE")) {
		ADIOI_Info_set(info, "romio_ds_read", value);
		fd->hints->ds_read = ADIOI_HINT_DISABLE;
	    }
	    else if (!strcmp(value, "automatic") || !strcmp(value, "AUTOMATIC"))
	    {
		ADIOI_Info_set(info, "romio_ds_read", value);
		fd->hints->ds_read = ADIOI_HINT_AUTO;
	    }
	    /* otherwise ignore */
	}
	ADIOI_Info_get(users_info, "romio_ds_write", MPI_MAX_INFO_VAL, value, 
		     &flag);
	if (flag) {
	    if (!strcmp(value, "enable") || !strcmp(value, "ENABLE")) {
		ADIOI_Info_set(info, "romio_ds_write", value);
		fd->hints->ds_write = ADIOI_HINT_ENABLE;
	    }
	    else if (!strcmp(value, "disable") || !strcmp(value, "DISABLE")) {
		ADIOI_Info_set(info, "romio_ds_write", value);
		fd->hints->ds_write = ADIOI_HINT_DISABLE;
	    }
	    else if (!strcmp(value, "automatic") || !strcmp(value, "AUTOMATIC"))
	    {
		ADIOI_Info_set(info, "romio_ds_write", value);
		fd->hints->ds_write = ADIOI_HINT_AUTO;
	    }
	    /* otherwise ignore */
	}

	ADIOI_Info_get(users_info, "ind_wr_buffer_size", MPI_MAX_INFO_VAL, 
		     value, &flag);
	if (flag && ((intval = atoi(value)) > 0)) {
	    ADIOI_Info_set(info, "ind_wr_buffer_size", value);
	    fd->hints->ind_wr_buffer_size = intval;
	}

	ADIOI_Info_get(users_info, "ind_rd_buffer_size", MPI_MAX_INFO_VAL, 
		     value, &flag);
	if (flag && ((intval = atoi(value)) > 0)) {
	    ADIOI_Info_set(info, "ind_rd_buffer_size", value);
	    fd->hints->ind_rd_buffer_size = intval;
	}

	memset( value, 0, MPI_MAX_INFO_VAL+1 );
	ADIOI_Info_get(users_info, "romio_min_fdomain_size", MPI_MAX_INFO_VAL,
			value, &flag);
	if ( flag && ((intval = atoi(value)) > 0) ) {
		ADIOI_Info_set(info, "romio_min_fdomain_size", value);
		fd->hints->min_fdomain_size = intval;
	}
  /* Now we use striping unit in common code so we should
     process hints for it. */
	ADIOI_Info_get(users_info, "striping_unit", MPI_MAX_INFO_VAL,
			value, &flag);
	if ( flag && ((intval = atoi(value)) > 0) ) {
		ADIOI_Info_set(info, "striping_unit", value);
		fd->hints->striping_unit = intval;
	}

	memset( value, 0, MPI_MAX_INFO_VAL+1 );
        ADIOI_Info_get(users_info, ADIOI_BGL_NAGG_IN_PSET_HINT_NAME, MPI_MAX_INFO_VAL,
		     value, &flag);
	if (flag && ((intval = atoi(value)) > 0)) {

	    did_anything = 1;
	    ADIOI_Info_set(info, ADIOI_BGL_NAGG_IN_PSET_HINT_NAME, value);
	    fd->hints->cb_nodes = intval;
	}
    }

    /* associate CB aggregators to certain CNs in every involved PSET */
    if (did_anything) {
	ADIOI_BGL_gen_agg_ranklist(fd, fd->hints->cb_nodes);
    }
    /* ignore defered open hints and do not enable it for bluegene: need all
     * processors in the open path so we can stat-and-broadcast the blocksize
     */
    ADIOI_Info_set(info, "romio_no_indep_rw", "false");
    fd->hints->no_indep_rw = 0;
    fd->hints->deferred_open = 0;

    /* BobC commented this out, but since hint processing runs on both bgl and
     * bglockless, we need to keep DS writes enabled on gpfs and disabled on
     * PVFS */
    if (ADIO_Feature(fd, ADIO_DATA_SIEVING_WRITES) == 0) {
    /* disable data sieving for fs that do not
       support file locking */
       	ADIOI_Info_get(info, "ind_wr_buffer_size", MPI_MAX_INFO_VAL,
		     value, &flag);
	if (flag) {
	    /* get rid of this value if it is set */
	    ADIOI_Info_delete(info, "ind_wr_buffer_size");
	}
	/* note: leave ind_wr_buffer_size alone; used for other cases
	 * as well. -- Rob Ross, 04/22/2003
	 */
	ADIOI_Info_set(info, "romio_ds_write", "disable");
	fd->hints->ds_write = ADIOI_HINT_DISABLE;
    }

    ADIOI_Free(value);

    *error_code = MPI_SUCCESS;
}
