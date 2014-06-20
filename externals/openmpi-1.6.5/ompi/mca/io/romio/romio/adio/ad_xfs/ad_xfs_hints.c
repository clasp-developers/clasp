/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_xfs.h"
#include "adio_extern.h"

static unsigned xfs_direct_read_chunk_size;
static unsigned xfs_direct_write_chunk_size;

void ADIOI_XFS_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code)
{
    char *value, * c;
    int flag;
    static char xfs_initialized = 0;

    if (fd->info == MPI_INFO_NULL) MPI_Info_create(&(fd->info));

    ADIOI_Info_set(fd->info, "direct_read", "false");
    ADIOI_Info_set(fd->info, "direct_write", "false");
    fd->direct_read = fd->direct_write = 0;

	if (!xfs_initialized) {
		xfs_initialized = 1;
		c = getenv("MPIO_DIRECT_READ_CHUNK_SIZE");
		if (c) {
			int io;
			io = atoi(c);
			if (io <= 0) {
				fprintf(stderr,
"MPI: Ignoring an invalid setting for MPIO_DIRECT_READ_CHUNK_SIZE.\n"
"     It must be set to a positive integer value.\n");
			} else {
				xfs_direct_read_chunk_size = io;
			}
		} else {
			xfs_direct_read_chunk_size = 0;
		}

		c = getenv("MPIO_DIRECT_WRITE_CHUNK_SIZE");
		if (c) {
			int io;
			io = atoi(c);
			if (io <= 0) {
				fprintf(stderr,
"MPI: Ignoring an invalid setting for MPIO_DIRECT_WRITE_CHUNK_SIZE.\n"
"     It must be set to a positive integer value.\n");
			} else {
				xfs_direct_write_chunk_size = io;
			}
		} else {
			xfs_direct_write_chunk_size = 0;
		}
	}

	if (!fd->hints->initialized) {
		fd->hints->fs_hints.xfs.read_chunk_sz =
			xfs_direct_read_chunk_size;
		fd->hints->fs_hints.xfs.write_chunk_sz =
			xfs_direct_write_chunk_size;
	}

    /* has user specified values for keys "direct_read" and "direct write"? */
    if (users_info != MPI_INFO_NULL) {
	value = (char *) ADIOI_Malloc((MPI_MAX_INFO_VAL+1)*sizeof(char));

	ADIOI_Info_get(users_info, "direct_read", MPI_MAX_INFO_VAL, 
			 value, &flag);
	if (flag && !strcmp(value, "true")) {
	    ADIOI_Info_set(fd->info, "direct_read", "true");
	    fd->direct_read = 1;
	}

	ADIOI_Info_get(users_info, "direct_write", MPI_MAX_INFO_VAL, 
			 value, &flag);
	if (flag && !strcmp(value, "true")) {
	    ADIOI_Info_set(fd->info, "direct_write", "true");
	    fd->direct_write = 1;
	}

	ADIOI_Free(value);
    }
    
    /* set the values for collective I/O and data sieving parameters */
    ADIOI_GEN_SetInfo(fd, users_info, error_code);

    /* Environment variables override MPI_Info hints */
    if (ADIOI_Direct_read) fd->direct_read = 1;
    if (ADIOI_Direct_write) fd->direct_write = 1;

    /* environment variables checked in ADIO_Init */

    *error_code = MPI_SUCCESS;
}
