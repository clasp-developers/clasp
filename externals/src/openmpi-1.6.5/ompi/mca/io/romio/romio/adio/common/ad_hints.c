/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "adio_extern.h"

void ADIOI_GEN_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code)
{
/* if fd->info is null, create a new info object. 
   Initialize fd->info to default values.
   Initialize fd->hints to default values.
   Examine the info object passed by the user. If it contains values that
   ROMIO understands, override the default. */

    MPI_Info info;
    char *value;
    int flag, intval, tmp_val, nprocs=0, nprocs_is_valid = 0, len;
    int ok_to_override_cb_nodes=0;
    static char myname[] = "ADIOI_GEN_SETINFO";


    /* if we've already set up default hints and the user has not asked us to
     * process any hints (MPI_INFO_NULL), then we can short-circuit hint
     * processing */
    if (fd->hints->initialized && fd->info == MPI_INFO_NULL) {
	    *error_code = MPI_SUCCESS;
	    return;
    }

    if (fd->info == MPI_INFO_NULL) MPI_Info_create(&(fd->info));
    info = fd->info;

    /* Note that fd->hints is allocated at file open time; thus it is
     * not necessary to allocate it, or check for allocation, here.
     */

    value = (char *) ADIOI_Malloc((MPI_MAX_INFO_VAL+1)*sizeof(char));
    if (value == NULL) {
	/* NEED TO HANDLE ENOMEM */
    }

    /* initialize info and hints to default values if they haven't been
     * previously initialized
     */
    if (!fd->hints->initialized) {

	/* buffer size for collective I/O */
	ADIOI_Info_set(info, "cb_buffer_size", ADIOI_CB_BUFFER_SIZE_DFLT); 
	fd->hints->cb_buffer_size = atoi(ADIOI_CB_BUFFER_SIZE_DFLT);

	/* default is to let romio automatically decide when to use
	 * collective buffering
	 */
	ADIOI_Info_set(info, "romio_cb_read", "automatic"); 
	fd->hints->cb_read = ADIOI_HINT_AUTO;
	ADIOI_Info_set(info, "romio_cb_write", "automatic"); 
	fd->hints->cb_write = ADIOI_HINT_AUTO;

	fd->hints->cb_config_list = NULL;

	/* number of processes that perform I/O in collective I/O */
	MPI_Comm_size(fd->comm, &nprocs);
	nprocs_is_valid = 1;
	ADIOI_Snprintf(value, MPI_MAX_INFO_VAL+1, "%d", nprocs);
	ADIOI_Info_set(info, "cb_nodes", value);
	fd->hints->cb_nodes = nprocs;

	/* hint indicating that no indep. I/O will be performed on this file */
	ADIOI_Info_set(info, "romio_no_indep_rw", "false");
	fd->hints->no_indep_rw = 0;

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
	ADIOI_Info_set(info, "ind_rd_buffer_size", ADIOI_IND_RD_BUFFER_SIZE_DFLT);
	fd->hints->ind_rd_buffer_size = atoi(ADIOI_IND_RD_BUFFER_SIZE_DFLT);

	/* buffer size for data sieving in independent writes */
	ADIOI_Info_set(info, "ind_wr_buffer_size", ADIOI_IND_WR_BUFFER_SIZE_DFLT);
	fd->hints->ind_wr_buffer_size = atoi(ADIOI_IND_WR_BUFFER_SIZE_DFLT);

	/* default is to let romio automatically decide when to use data
	 * sieving
	 */
	ADIOI_Info_set(info, "romio_ds_read", "automatic"); 
	fd->hints->ds_read = ADIOI_HINT_AUTO;
	ADIOI_Info_set(info, "romio_ds_write", "automatic"); 
	fd->hints->ds_write = ADIOI_HINT_AUTO;

	/* still to do: tune this a bit for a variety of file systems. there's
	 * no good default value so just leave it unset */
	fd->hints->min_fdomain_size = 0;
  fd->hints->striping_unit = 0;

	fd->hints->initialized = 1;

	/* ADIO_Open sets up collective buffering arrays.  If we are in this
	 * path from say set_file_view, then we've don't want to adjust the
	 * array: we'll get a segfault during collective i/o.  We only want to
	 * look at the users cb_nodes if it's open time  */
	ok_to_override_cb_nodes = 1;

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

	if (ok_to_override_cb_nodes) {
		/* MPI_File_open path sets up some data structrues that don't
		 * get resized in the MPI_File_set_view path, so ignore
		 * cb_nodes in the set_view case */
	    ADIOI_Info_get(users_info, "cb_nodes", MPI_MAX_INFO_VAL, 
	  	     value, &flag);
	    if (flag && ((intval=atoi(value)) > 0)) {
	        tmp_val = intval;

	        MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
	       /* --BEGIN ERROR HANDLING-- */
	       if (tmp_val != intval) {
		    MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
						       "cb_nodes",
						       error_code);
		    return;
	       }
	       /* --END ERROR HANDLING-- */

	       if (!nprocs_is_valid) {
		   /* if hints were already initialized, we might not
		    * have already gotten this?
		    */
		   MPI_Comm_size(fd->comm, &nprocs);
		   nprocs_is_valid = 1;
	       }
	       if (intval <= nprocs) {
		   ADIOI_Info_set(info, "cb_nodes", value);
		   fd->hints->cb_nodes = intval;
	       }
	   }
	} /* if (ok_to_override_cb_nodes) */

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

	ADIOI_Info_get(users_info, "cb_config_list", MPI_MAX_INFO_VAL,
		     value, &flag);
	if (flag) {
	    if (fd->hints->cb_config_list == NULL) {
		/* only set cb_config_list if it isn't already set.
		 * Note that since we set it below, this ensures that
		 * the cb_config_list hint will be set at file open time
		 * either by the user or to the default
		 */
	    	ADIOI_Info_set(info, "cb_config_list", value);
		len = (strlen(value)+1) * sizeof(char);
		fd->hints->cb_config_list = ADIOI_Malloc(len);
		if (fd->hints->cb_config_list == NULL) {
		    /* NEED TO HANDLE ENOMEM */
		}
		ADIOI_Strncpy(fd->hints->cb_config_list, value, len);
	    }
	    /* if it has been set already, we ignore it the second time. 
	     * otherwise we would get an error if someone used the same
	     * info value with a cb_config_list value in it in a couple
	     * of calls, which would be irritating. */
	}
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
    }

    /* handle cb_config_list default value here; avoids an extra
     * free/alloc and insures it is always set
     */
    if (fd->hints->cb_config_list == NULL) {
	ADIOI_Info_set(info, "cb_config_list", ADIOI_CB_CONFIG_LIST_DFLT);
	len = (strlen(ADIOI_CB_CONFIG_LIST_DFLT)+1) * sizeof(char);
	fd->hints->cb_config_list = ADIOI_Malloc(len);
	if (fd->hints->cb_config_list == NULL) {
	    /* NEED TO HANDLE ENOMEM */
	}
	ADIOI_Strncpy(fd->hints->cb_config_list, ADIOI_CB_CONFIG_LIST_DFLT, len);
    }
    /* deferred_open won't be set by callers, but if the user doesn't
     * explicitly disable collecitve buffering (two-phase) and does hint that
     * io w/o independent io is going on, we'll set this internal hint as a
     * convenience */
    if ( ( (fd->hints->cb_read != ADIOI_HINT_DISABLE) \
			    && (fd->hints->cb_write != ADIOI_HINT_DISABLE)\
			    && fd->hints->no_indep_rw ) ) {
	    fd->hints->deferred_open = 1;
    } else {
	    /* setting romio_no_indep_rw enable and romio_cb_{read,write}
	     * disable at the same time doesn't make sense. honor
	     * romio_cb_{read,write} and force the no_indep_rw hint to
	     * 'disable' */
	    ADIOI_Info_set(info, "romio_no_indep_rw", "false");
	    fd->hints->no_indep_rw = 0;
	    fd->hints->deferred_open = 0;
    }

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
