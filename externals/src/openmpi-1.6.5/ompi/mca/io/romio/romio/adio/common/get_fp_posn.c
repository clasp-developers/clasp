/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "adio_extern.h"
#include "adioi.h"

/* returns the current position of the individual file pointer
   in etype units relative to the current view. */

void ADIOI_Get_position(ADIO_File fd, ADIO_Offset *offset)
{
    ADIOI_Flatlist_node *flat_file;
    int i, flag;
    unsigned filetype_size;
    int etype_size, filetype_is_contig;
    MPI_Aint filetype_extent;
    ADIO_Offset disp, byte_offset, sum=0, size_in_file, n_filetypes, frd_size;
    
    ADIOI_Datatype_iscontig(fd->filetype, &filetype_is_contig);
    etype_size = fd->etype_size;

    if (filetype_is_contig) *offset = (fd->fp_ind - fd->disp)/etype_size;
    else {
/* filetype already flattened in ADIO_Open */
        flat_file = ADIOI_Flatlist;
        while (flat_file->type != fd->filetype) flat_file = flat_file->next;

	MPI_Type_size(fd->filetype, (int*)&filetype_size);
	MPI_Type_extent(fd->filetype, &filetype_extent);

	disp = fd->disp;
	byte_offset = fd->fp_ind;
	n_filetypes = -1;
	flag = 0;
	while (!flag) {
	    sum = 0;
	    n_filetypes++;
	    for (i=0; i<flat_file->count; i++) {
		sum += flat_file->blocklens[i];
		if (disp + flat_file->indices[i] + 
	     	    n_filetypes* ADIOI_AINT_CAST_TO_OFFSET filetype_extent + flat_file->blocklens[i] 
		    >= byte_offset) {
		    frd_size = disp + flat_file->indices[i] + 
			n_filetypes * ADIOI_AINT_CAST_TO_OFFSET filetype_extent
			+ flat_file->blocklens[i] - byte_offset;
		    sum -= frd_size;
		    flag = 1;
		    break;
		}
	    }
	}
	size_in_file = n_filetypes * (ADIO_Offset)filetype_size + sum;
	*offset = size_in_file/etype_size;
    }
}
