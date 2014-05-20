/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 2008 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "adio_extern.h"
#ifdef AGGREGATION_PROFILE
#include "mpe.h"
#endif
#include <assert.h>
/* #define DEBUG */

void ADIOI_Calc_file_realms_user_size (ADIO_File fd, int fr_size,
				       int nprocs_for_coll,
				       ADIO_Offset *file_realm_st_offs,
				       MPI_Datatype *file_realm_types);
void ADIOI_Calc_file_realms_aar (ADIO_File fd, int nprocs_for_coll,
				 int pfr_enabled,
				 ADIO_Offset min_st_offset,
				 ADIO_Offset max_end_offset,
				 ADIO_Offset *file_realm_st_offs,
				 MPI_Datatype *file_realm_types);
void ADIOI_Calc_file_realms_fsize (ADIO_File fd,
				   int nprocs_for_coll,
				   ADIO_Offset max_end_offset,
				   ADIO_Offset *file_realm_st_offs,
				   MPI_Datatype *file_realm_types);
void ADIOI_Create_fr_simpletype (int size, int nprocs_for_coll,
			      MPI_Datatype *simpletype);
static void align_fr (int fr_size, ADIO_Offset fr_off, int alignment,
	       int *aligned_fr_size, ADIO_Offset *aligned_fr_off);
void ADIOI_Verify_fr (int nprocs_for_coll, ADIO_Offset *file_realm_st_offs,
		      MPI_Datatype *file_realm_types);

void ADIOI_Calc_file_realms (ADIO_File fd, ADIO_Offset min_st_offset,
			     ADIO_Offset max_end_offset)
{
    int nprocs_for_coll;
    int file_realm_calc_type;
    
    MPI_Datatype *file_realm_types = NULL;
    ADIO_Offset *file_realm_st_offs = NULL;

#ifdef AGGREGATION_PROFILE
    MPE_Log_event (5004, 0, NULL);
#endif
#ifdef DEBUG
    printf ("ADIOI_Calc_file_realms\n");
#endif
    
    nprocs_for_coll = fd->hints->cb_nodes;    
    file_realm_calc_type = fd->hints->cb_fr_type;

    /* If PFRs are disabled we know these pointers are not allocated */
    if (fd->hints->cb_pfr != ADIOI_HINT_ENABLE) {
	fd->file_realm_st_offs = NULL;
	fd->file_realm_types = NULL;
    }

    if (nprocs_for_coll == 1) {
	/* if there's only one aggregator, we can reset the file
	 * realms every single time */
	if (fd->file_realm_st_offs == NULL)
	{
	    file_realm_st_offs = (ADIO_Offset *)
		ADIOI_Malloc (sizeof(ADIO_Offset));
	    file_realm_types   = (MPI_Datatype *)
		ADIOI_Malloc (sizeof(MPI_Datatype));
	}
	else
	{
	    file_realm_st_offs = fd->file_realm_st_offs;
	    file_realm_types   = fd->file_realm_types;
	}
	*file_realm_st_offs = min_st_offset;
	MPI_Type_contiguous ((max_end_offset - min_st_offset + 1), MPI_BYTE,
			     file_realm_types);
	MPI_Type_commit (file_realm_types);
	ADIOI_Add_contig_flattened (*file_realm_types);
    }
    else if (fd->file_realm_st_offs == NULL) {
	file_realm_st_offs = (ADIO_Offset *)
	    ADIOI_Malloc (nprocs_for_coll * sizeof(ADIO_Offset));
	file_realm_types   = (MPI_Datatype *)
	    ADIOI_Malloc (nprocs_for_coll * sizeof(MPI_Datatype));
	
	if (file_realm_calc_type == ADIOI_FR_AAR) {
	    ADIOI_Calc_file_realms_aar (fd, nprocs_for_coll,
					fd->hints->cb_pfr,
					min_st_offset, max_end_offset,
					file_realm_st_offs, file_realm_types);
	    /* flatten file realm datatype for future use - only one
	     * because all are the same*/
	    ADIOI_Flatten_datatype (file_realm_types[0]);
	}
	else if (file_realm_calc_type == ADIOI_FR_FSZ) {
	    ADIOI_Calc_file_realms_fsize (fd, nprocs_for_coll, max_end_offset,
					  file_realm_st_offs,
					  file_realm_types);
	    /* flatten file realm datatype for future use - only one
	     * because all are the same*/
	    ADIOI_Flatten_datatype (file_realm_types[0]);
	}
	else if (file_realm_calc_type == ADIOI_FR_USR_REALMS) {
	    /* copy user provided realm datatypes and realm offsets in
	     * hints to file descriptor. may also want to verify that
	     * the provided file realms are covering (for pfr at
	     * least) and non-overlapping */
	}
	else if (file_realm_calc_type > 0) {
	    ADIOI_Calc_file_realms_user_size (fd, file_realm_calc_type,
					      nprocs_for_coll,
					      file_realm_st_offs,
					      file_realm_types);
	    /* flatten file realm datatype for future use - only one
	     * because all are the same */
	    ADIOI_Flatten_datatype (file_realm_types[0]);
	}
    }
    fd->file_realm_st_offs = file_realm_st_offs;
    fd->file_realm_types   = file_realm_types;
#ifdef AGGREGATION_PROFILE
    MPE_Log_event (5005, 0, NULL);
#endif
}

void ADIOI_Calc_file_realms_user_size (ADIO_File fd, int fr_size,
				       int nprocs_for_coll,
				       ADIO_Offset *file_realm_st_offs,
				       MPI_Datatype *file_realm_types)
{
    int i;
    int aligned_fr_size;
    ADIO_Offset aligned_fr_off;
    MPI_Datatype simpletype;

    align_fr(fr_size, 0, fd->hints->cb_fr_alignment, &aligned_fr_size,
	     &aligned_fr_off);
    fr_size = aligned_fr_size;
    ADIOI_Create_fr_simpletype (fr_size, nprocs_for_coll, &simpletype);

    if (fd->hints->cb_pfr == ADIOI_HINT_ENABLE)
	file_realm_st_offs[0] = 0;
    else
	file_realm_st_offs[0] = aligned_fr_off;
    file_realm_types[0]   = simpletype;
#ifdef DEBUG
    printf ("file_realm[0] = (%lld, %d)\n", file_realm_st_offs[0],
	    fr_size);
#endif

    for (i=1; i < nprocs_for_coll; i++)
    {
	file_realm_st_offs[i] = file_realm_st_offs[i-1] + fr_size;
	file_realm_types[i]   = simpletype;
#ifdef DEBUG
	printf ("file_realm[%d] = (%lld, %d)\n", i, file_realm_st_offs[i],
		aligned_fr_size);
#endif
    }
}

/* takes an extra romio_cb_pfr param to decide whether file realms
 * should start at byte 0 of the file*/
void ADIOI_Calc_file_realms_aar (ADIO_File fd, int nprocs_for_coll, int cb_pfr,
				 ADIO_Offset min_st_offset,
				 ADIO_Offset max_end_offset,
				 ADIO_Offset *file_realm_st_offs,
				 MPI_Datatype *file_realm_types)
{
    int fr_size, aligned_fr_size, i;
    MPI_Datatype simpletype;
    ADIO_Offset aligned_start_off;
    char value[9];

    fr_size = (max_end_offset - min_st_offset + nprocs_for_coll) /
	nprocs_for_coll;
    align_fr(fr_size, min_st_offset, fd->hints->cb_fr_alignment,
	     &aligned_fr_size, &aligned_start_off);
    fr_size = aligned_fr_size;
    ADIOI_Create_fr_simpletype (fr_size, nprocs_for_coll, &simpletype);
    if (cb_pfr == ADIOI_HINT_ENABLE)
	file_realm_st_offs[0] = 0;
    else
	file_realm_st_offs[0] = aligned_start_off;
    file_realm_types[0]   = simpletype;

#ifdef DEBUG
    printf ("file_realm[0] = (%lld, %d)\n", file_realm_st_offs[0],
	    fr_size);
#endif
    for (i=1; i < nprocs_for_coll; i++)
    {
	file_realm_st_offs[i] = file_realm_st_offs[i-1] + fr_size;
	file_realm_types[i]   = simpletype;
#ifdef DEBUG
	printf ("file_realm[%d] = (%lld, %d)\n", i, file_realm_st_offs[i],
		fr_size);
#endif
    }
    if (fd->hints->cb_pfr == ADIOI_HINT_ENABLE) {
	sprintf (value, "%d", fr_size);
	ADIOI_Info_set (fd->info, "romio_cb_fr_type", value);
    }
}

void ADIOI_Calc_file_realms_fsize (ADIO_File fd, int nprocs_for_coll,
				   ADIO_Offset max_end_offset,
				   ADIO_Offset *file_realm_st_offs,
				   MPI_Datatype *file_realm_types)
{
    int fr_size, aligned_fr_size, error_code, i;
    int fsize;
    ADIO_Offset aligned_fr_off;
    ADIO_Fcntl_t fcntl_struct;
    MPI_Datatype simpletype;

    ADIO_Fcntl (fd, ADIO_FCNTL_GET_FSIZE, &fcntl_struct, &error_code);
    
    /* use impending file size since a write call may lengthen the file */
    fsize = ADIOI_MAX (fcntl_struct.fsize, max_end_offset+1);
    fr_size = (fsize + nprocs_for_coll - 1) / nprocs_for_coll;
    align_fr(fr_size, 0, fd->hints->cb_fr_alignment,
	     &aligned_fr_size, &aligned_fr_off);
    ADIOI_Create_fr_simpletype (fr_size, nprocs_for_coll, &simpletype);

    for (i=0; i < nprocs_for_coll; i++)
    {
	file_realm_st_offs[i] = fr_size * i;
	file_realm_types[i]   = simpletype;
    }
}

/* creates a datatype with an empty trailing edge */
void ADIOI_Create_fr_simpletype (int size, int nprocs_for_coll,
			      MPI_Datatype *simpletype)
{
    int count=2, blocklens[2];
    MPI_Aint indices[2];
    MPI_Datatype old_types[2];

    blocklens[0] = size;
    blocklens[1] = 1;
    indices[0]   = 0;
    indices[1]   = size*nprocs_for_coll;
    old_types[0] = MPI_BYTE;
    old_types[1] = MPI_UB;

    MPI_Type_struct (count, blocklens, indices, old_types, simpletype);

    MPI_Type_commit (simpletype);
}

/* Verify that file realms are covering (PFRs) and non-overlapping */
void ADIOI_Verify_fr (int nprocs_for_coll, ADIO_Offset *file_realm_st_offs,
		      MPI_Datatype *file_realm_types)
{
}

int ADIOI_Agg_idx (int rank, ADIO_File fd) {
    int i, cb_nodes, *ranklist;
    cb_nodes = fd->hints->cb_nodes;
    ranklist = fd->hints->ranklist;

    for (i=0; i<cb_nodes; i++) {
	if (ranklist[i])
	    return i;
    }
    return -1;
}

static void align_fr (int fr_size, ADIO_Offset fr_off, int alignment,
	       int *aligned_fr_size, ADIO_Offset *aligned_fr_off) {
    *aligned_fr_off = fr_off - (fr_off % alignment);
    *aligned_fr_size = ((fr_off + fr_size) / alignment) * alignment - 
	*aligned_fr_off;
    if ((fr_off + fr_size) % alignment)
	*aligned_fr_size += alignment;

    assert(!((*aligned_fr_off % alignment) ||
	     (*aligned_fr_size % alignment)));
}
