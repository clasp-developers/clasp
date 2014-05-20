/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include <stdlib.h>
#include "ad_pvfs2.h"

void ADIOI_PVFS2_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code)
{
    char *value;
    int flag, tmp_value;
    static char myname[] = "ADIOI_PVFS_SETINFO";

    if ((fd->info) == MPI_INFO_NULL) {
	/* part of the open call */
	MPI_Info_create(&(fd->info));
	ADIOI_Info_set(fd->info, "romio_pvfs2_debugmask", "0");
	fd->hints->fs_hints.pvfs2.debugmask = 0;

	ADIOI_Info_set(fd->info, "striping_factor", "0");
	fd->hints->striping_factor = 0;

	ADIOI_Info_set(fd->info, "striping_unit", "0");
	fd->hints->striping_unit = 0;

	/* disable the aggressive strided optimizations by default */
        ADIOI_Info_set(fd->info, "romio_pvfs2_posix_read", "disable");
        ADIOI_Info_set(fd->info, "romio_pvfs2_posix_write", "disable");
        fd->hints->fs_hints.pvfs2.posix_read = ADIOI_HINT_DISABLE;
        fd->hints->fs_hints.pvfs2.posix_write = ADIOI_HINT_DISABLE;

        ADIOI_Info_set(fd->info, "romio_pvfs2_dtype_read", "disable");
        ADIOI_Info_set(fd->info, "romio_pvfs2_dtype_write", "disable");
        fd->hints->fs_hints.pvfs2.dtype_read = ADIOI_HINT_DISABLE;
        fd->hints->fs_hints.pvfs2.dtype_write = ADIOI_HINT_DISABLE;

        ADIOI_Info_set(fd->info, "romio_pvfs2_listio_read", "disable");
        ADIOI_Info_set(fd->info, "romio_pvfs2_listio_write", "disable");
        fd->hints->fs_hints.pvfs2.listio_read = ADIOI_HINT_DISABLE;
        fd->hints->fs_hints.pvfs2.listio_write = ADIOI_HINT_DISABLE;

	
	/* any user-provided hints? */
	if (users_info != MPI_INFO_NULL) {
	    /* pvfs2 debugging */
	    value = (char *) ADIOI_Malloc( (MPI_MAX_INFO_VAL+1)*sizeof(char));
	    ADIOI_Info_get(users_info, "romio_pvfs2_debugmask", 
		    MPI_MAX_INFO_VAL, value, &flag);
	    if (flag) {
		tmp_value = fd->hints->fs_hints.pvfs2.debugmask = 
		    PVFS_debug_eventlog_to_mask(value);

		MPI_Bcast(&tmp_value, 1, MPI_INT, 0, fd->comm);
		/* --BEGIN ERROR HANDLING-- */
		if (tmp_value != fd->hints->fs_hints.pvfs2.debugmask) {
		    MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
						       "romio_pvfs2_debugmask",
						       error_code);
		    return;
		}
		/* --END ERROR HANDLING-- */
		
		ADIOI_Info_set(fd->info, "romio_pvfs2_debugmask", value);
	    }

	    /* the striping factor */
	    ADIOI_Info_get(users_info, "striping_factor", 
		    MPI_MAX_INFO_VAL, value, &flag);
	    if (flag) {
		tmp_value = fd->hints->striping_factor =  atoi(value);

		MPI_Bcast(&tmp_value, 1, MPI_INT, 0, fd->comm);
		/* --BEGIN ERROR HANDLING-- */
		if (tmp_value != fd->hints->striping_factor) {
		    MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
						       "striping_factor",
						       error_code);
		    return;
		}
		/* --END ERROR HANDLING-- */
		
		ADIOI_Info_set(fd->info, "striping_factor", value);
	    }

	    /* the striping unit */
	    ADIOI_Info_get(users_info, "striping_unit",
		    MPI_MAX_INFO_VAL, value, &flag);
	    if (flag) {
		tmp_value = fd->hints->striping_unit = atoi(value);
		MPI_Bcast(&tmp_value, 1, MPI_INT, 0, fd->comm);
		/* --BEGIN ERROR HANDLING-- */
		if (tmp_value != fd->hints->striping_unit) {
		    MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname, 
			                               "striping_unit",
			                                error_code);
		    return;
		}
		/* --END ERROR HANDLING-- */

		ADIOI_Info_set(fd->info, "striping_unit", value);
	    }

	    /* distribution name */
	    ADIOI_Info_get(users_info, "romio_pvfs2_distribution_name",
		    MPI_MAX_INFO_VAL, value, &flag);
	    if (flag) {
	    }


	    /* POSIX read */
            ADIOI_Info_get(users_info, "romio_pvfs2_posix_read",
                         MPI_MAX_INFO_VAL, value, &flag);
            if (flag) {
                if ( !strcmp(value, "enable") || !strcmp(value, "ENABLE"))
                {
                    ADIOI_Info_set(fd->info, "romio_pvfs2_posix_read", value);
                    fd->hints->fs_hints.pvfs2.posix_read = ADIOI_HINT_ENABLE;
                }
                else if ( !strcmp(value, "disable") ||
                          !strcmp(value, "DISABLE"))
                {
                    ADIOI_Info_set(fd->info , "romio_pvfs2_posix_read", value);
                    fd->hints->fs_hints.pvfs2.posix_read = ADIOI_HINT_DISABLE;
                }
                tmp_value = fd->hints->fs_hints.pvfs2.posix_read;
                MPI_Bcast(&tmp_value, 1, MPI_INT, 0, fd->comm);
                if (tmp_value != fd->hints->fs_hints.pvfs2.posix_read) {
                    MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
                                                       "posix_read",
                                                       error_code);
                    return;
                }
            }

            /* POSIX write */
            ADIOI_Info_get(users_info, "romio_pvfs2_posix_write",
                         MPI_MAX_INFO_VAL, value, &flag);
            if (flag) {
                if ( !strcmp(value, "enable") || !strcmp(value, "ENABLE"))
                {
                    ADIOI_Info_set(fd->info, "romio_pvfs2_posix_write", value);
                    fd->hints->fs_hints.pvfs2.posix_write = ADIOI_HINT_ENABLE;
                }
                else if ( !strcmp(value, "disable") ||
                          !strcmp(value, "DISABLE"))
                {
                    ADIOI_Info_set(fd->info , "romio_pvfs2_posix_write", value);
                    fd->hints->fs_hints.pvfs2.posix_write = ADIOI_HINT_DISABLE;
                }
                tmp_value = fd->hints->fs_hints.pvfs2.posix_write;
                MPI_Bcast(&tmp_value, 1, MPI_INT, 0, fd->comm);
                if (tmp_value != fd->hints->fs_hints.pvfs2.posix_write) {
                    MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
                                                       "posix_write",
                                                       error_code);
                    return;
                }
            }

	    /* Datatype read */
            ADIOI_Info_get(users_info, "romio_pvfs2_dtype_read",
                         MPI_MAX_INFO_VAL, value, &flag);
            if (flag) {
                if ( !strcmp(value, "enable") || !strcmp(value, "ENABLE"))
                {
                    ADIOI_Info_set(fd->info, "romio_pvfs2_dtype_read", value);
                    fd->hints->fs_hints.pvfs2.dtype_read = ADIOI_HINT_ENABLE;
                }
                else if ( !strcmp(value, "disable") ||
                          !strcmp(value, "DISABLE"))
                {
                    ADIOI_Info_set(fd->info , "romio_pvfs2_dtype_read", value);
                    fd->hints->fs_hints.pvfs2.dtype_read = ADIOI_HINT_DISABLE;
                }
                tmp_value = fd->hints->fs_hints.pvfs2.dtype_read;
                MPI_Bcast(&tmp_value, 1, MPI_INT, 0, fd->comm);
                if (tmp_value != fd->hints->fs_hints.pvfs2.dtype_read) {
                    MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
                                                       "dtype_read",
                                                       error_code);
                    return;
                }
            }

            /* Datatype write */
            ADIOI_Info_get(users_info, "romio_pvfs2_dtype_write",
                         MPI_MAX_INFO_VAL, value, &flag);
            if (flag) {
                if ( !strcmp(value, "enable") || !strcmp(value, "ENABLE"))
                {
                    ADIOI_Info_set(fd->info, "romio_pvfs2_dtype_write", value);
                    fd->hints->fs_hints.pvfs2.dtype_write = ADIOI_HINT_ENABLE;
                }
                else if ( !strcmp(value, "disable") ||
                          !strcmp(value, "DISABLE"))
                {
                    ADIOI_Info_set(fd->info , "romio_pvfs2_dtype_write", value);
                    fd->hints->fs_hints.pvfs2.dtype_write = ADIOI_HINT_DISABLE;
                }
                tmp_value = fd->hints->fs_hints.pvfs2.dtype_write;
                MPI_Bcast(&tmp_value, 1, MPI_INT, 0, fd->comm);
                if (tmp_value != fd->hints->fs_hints.pvfs2.dtype_write) {
                    MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
                                                       "dtype_write",
                                                       error_code);
                    return;
                }
            }

	    /* Listio read */
            ADIOI_Info_get(users_info, "romio_pvfs2_listio_read",
                         MPI_MAX_INFO_VAL, value, &flag);
            if (flag) {
                if ( !strcmp(value, "enable") || !strcmp(value, "ENABLE"))
                {
                    ADIOI_Info_set(fd->info, "romio_pvfs2_listio_read", value);
                    fd->hints->fs_hints.pvfs2.listio_read = ADIOI_HINT_ENABLE;
                }
                else if ( !strcmp(value, "disable") ||
                          !strcmp(value, "DISABLE"))
                {
                    ADIOI_Info_set(fd->info , "romio_pvfs2_listio_read", value);
                    fd->hints->fs_hints.pvfs2.listio_read = ADIOI_HINT_DISABLE;
                }
                tmp_value = fd->hints->fs_hints.pvfs2.listio_read;
                MPI_Bcast(&tmp_value, 1, MPI_INT, 0, fd->comm);
                if (tmp_value != fd->hints->fs_hints.pvfs2.listio_read) {
                    MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
                                                       "listio_read",
                                                       error_code);
                    return;
                }
            }

            /* Datatype write */
            ADIOI_Info_get(users_info, "romio_pvfs2_listio_write",
                         MPI_MAX_INFO_VAL, value, &flag);
            if (flag) {
                if ( !strcmp(value, "enable") || !strcmp(value, "ENABLE"))
                {
                    ADIOI_Info_set(fd->info, "romio_pvfs2_listio_write", value);
                    fd->hints->fs_hints.pvfs2.listio_write = ADIOI_HINT_ENABLE;
                }
                else if ( !strcmp(value, "disable") ||
                          !strcmp(value, "DISABLE"))
                {
                    ADIOI_Info_set(fd->info , "romio_pvfs2_listio_write", value);
                    fd->hints->fs_hints.pvfs2.listio_write = ADIOI_HINT_DISABLE;
                }
                tmp_value = fd->hints->fs_hints.pvfs2.listio_write;
                MPI_Bcast(&tmp_value, 1, MPI_INT, 0, fd->comm);
                if (tmp_value != fd->hints->fs_hints.pvfs2.listio_write) {
                    MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
                                                       "listio_write",
                                                       error_code);
                    return;
                }
            }
            ADIOI_Free(value);

	}
    }
    /* set the values for collective I/O and data sieving parameters */
    ADIOI_GEN_SetInfo(fd, users_info, error_code);

    *error_code = MPI_SUCCESS;
}

/*
 * vim: ts=8 sts=4 sw=4 noexpandtab
 */
