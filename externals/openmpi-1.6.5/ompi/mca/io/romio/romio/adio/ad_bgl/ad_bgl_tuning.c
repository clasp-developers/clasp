/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_bgl_tuning.c
 * \brief defines ad_bgl performance tuning
 */

/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 2008 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

/*---------------------------------------------------------------------
 * ad_bgl_tuning.c
 *
 * defines global variables and functions for performance tuning and 
 * functional debugging.
 *---------------------------------------------------------------------*/

#include "ad_bgl_tuning.h"
#include "mpi.h"

int 	bglmpio_timing;
int 	bglmpio_timing2;
int 	bglmpio_comm;
int 	bglmpio_tunegather;
int 	bglmpio_tuneblocking;

double	bglmpio_prof_cw    [BGLMPIO_CIO_LAST];
double	bglmpio_prof_cr    [BGLMPIO_CIO_LAST];

/* set internal variables for tuning environment variables */
/** \page env_vars Environment Variables
 * - BGLMPIO_COMM - Define how data is exchanged on collective
 *   reads and writes.  Possible values:
 *   - 0 - Use MPI_Alltoallv.
 *   - 1 - Use MPI_Isend/MPI_Irecv.
 *   - Default is 0.
 *
 * - BGLMPIO_TIMING - collect timing breakdown for MPI I/O collective calls.
 *   Must also compile the library with BGL_PROFILE defined. Possible values:
 *   - 0 - Do not collect/report timing.
 *   - 1 - Collect/report timing.
 *   - Default is 0.
 *
 * - BGLMPIO_TIMING2 - collect additional averages for MPI I/O collective calls.
 *   Must also compile the library with BGL_PROFILE defined. Possible values:
 *   - 0 - Do not collect/report averages.
 *   - 1 - Collect/report averages.
 *   - Default is 0.
 *
 * - BGLMPIO_TUNEGATHER - Tune how starting and ending offsets are communicated
 *   for aggregator collective i/o.  Possible values:
 *   - 0 - Use two MPI_Allgather's to collect starting and ending offsets.
 *   - 1 - Use MPI_Allreduce(MPI_MAX) to collect starting and ending offsets.
 *   - Default is 1.
 *
 * - BGLMPIO_TUNEBLOCKING - Tune how aggregate file domains are 
 *   calculated (block size).  Possible values:
 *   - 0 - Evenly calculate file domains across aggregators.  Also use 
 *   MPI_Isend/MPI_Irecv to exchange domain information.
 *   - 1 - Align file domains with the underlying file system's block size.  Also use 
 *   MPI_Alltoallv to exchange domain information.
 *   - Default is 1.
 *
*/
void ad_bgl_get_env_vars() {
    char *x;

    bglmpio_comm   = 0;
	x = getenv( "BGLMPIO_COMM"         ); 
	if (x) bglmpio_comm         = atoi(x);
    bglmpio_timing = 0;
	x = getenv( "BGLMPIO_TIMING"       ); 
	if (x) bglmpio_timing       = atoi(x);
    bglmpio_timing2 = 0;
	x = getenv( "BGLMPIO_TIMING2"      ); 
	if (x) bglmpio_timing2      = atoi(x);
    bglmpio_tunegather = 1;
	x = getenv( "BGLMPIO_TUNEGATHER"   ); 
	if (x) bglmpio_tunegather   = atoi(x);
    bglmpio_tuneblocking = 1;
	x = getenv( "BGLMPIO_TUNEBLOCKING" ); 
	if (x) bglmpio_tuneblocking = atoi(x);
}

/* report timing breakdown for MPI I/O collective call */
void ad_bgl_wr_timing_report( int rw, ADIO_File fd, int myrank, int nprocs )
{
    int i;

    if (bglmpio_timing) {

	double *bglmpio_prof_org = bglmpio_prof_cr;
	if (rw) bglmpio_prof_org = bglmpio_prof_cw;

	double bglmpio_prof_avg[ BGLMPIO_CIO_LAST ];
	double bglmpio_prof_max[ BGLMPIO_CIO_LAST ];
	
	MPI_Reduce( bglmpio_prof_org, bglmpio_prof_avg, BGLMPIO_CIO_LAST, MPI_DOUBLE, MPI_SUM, 0, fd->comm );
	MPI_Reduce( bglmpio_prof_org, bglmpio_prof_max, BGLMPIO_CIO_LAST, MPI_DOUBLE, MPI_MAX, 0, fd->comm );

	if (myrank == 0) {

	    for (i=0; i<BGLMPIO_CIO_LAST; i++) bglmpio_prof_avg[i] /= nprocs;

	    if (bglmpio_timing2) {
		bglmpio_prof_avg[ BGLMPIO_CIO_B_POSI_RW  ] = bglmpio_prof_avg[ BGLMPIO_CIO_DATA_SIZE ] * nprocs / 
							     bglmpio_prof_max[ BGLMPIO_CIO_T_POSI_RW  ];
		bglmpio_prof_avg[ BGLMPIO_CIO_B_MPIO_RW  ] = bglmpio_prof_avg[ BGLMPIO_CIO_DATA_SIZE ] * nprocs / 
							     bglmpio_prof_max[ BGLMPIO_CIO_T_MPIO_RW  ];
	    } else {

		bglmpio_prof_avg[ BGLMPIO_CIO_B_POSI_RW  ] = 0;
		bglmpio_prof_avg[ BGLMPIO_CIO_B_MPIO_RW  ] = 0;
	    }

		bglmpio_prof_avg[ BGLMPIO_CIO_B_MPIO_CRW ] = bglmpio_prof_avg[ BGLMPIO_CIO_DATA_SIZE ] * nprocs / 
							     bglmpio_prof_max[ BGLMPIO_CIO_T_MPIO_CRW ];

	    printf("\tTIMING-1 %1s , ", (rw ? "W" : "R") );
	    printf(    "SZ: %12.4f , ", bglmpio_prof_avg[ BGLMPIO_CIO_DATA_SIZE ] * nprocs );
	    printf(  "SK-a: %10.3f , ", bglmpio_prof_avg[ BGLMPIO_CIO_T_SEEK ]     );
	    printf(  "SK-m: %10.3f , ", bglmpio_prof_max[ BGLMPIO_CIO_T_SEEK ]     );
	    printf(  "LC-a: %10.3f , ", bglmpio_prof_avg[ BGLMPIO_CIO_T_LCOMP ]    );
	    printf(  "GA-m: %10.3f , ", bglmpio_prof_max[ BGLMPIO_CIO_T_GATHER ]   );
	    printf(  "AN-a: %10.3f , ", bglmpio_prof_avg[ BGLMPIO_CIO_T_PATANA ]   );
	    printf(  "FD-a: %10.3f , ", bglmpio_prof_avg[ BGLMPIO_CIO_T_FD_PART ]  );
	    printf(  "MY-a: %10.3f , ", bglmpio_prof_avg[ BGLMPIO_CIO_T_MYREQ ]    );
	    printf(  "OT-m: %10.3f , ", bglmpio_prof_max[ BGLMPIO_CIO_T_OTHREQ ]   );
	    printf(  "EX-m: %10.3f , ", bglmpio_prof_max[ BGLMPIO_CIO_T_DEXCH ]    );
	    printf("\tTIMING-2 %1s , ", (rw ? "W" : "R") );
	    printf( "PXT-m: %10.3f , ", bglmpio_prof_avg[ BGLMPIO_CIO_T_POSI_RW ]  );
	    printf( "MPT-m: %10.3f , ", bglmpio_prof_avg[ BGLMPIO_CIO_T_MPIO_RW ]  );
	    printf("MPTC-m: %10.3f , ", bglmpio_prof_avg[ BGLMPIO_CIO_T_MPIO_CRW ] );
	    printf(   "PXB: %10.3f , ", bglmpio_prof_avg[ BGLMPIO_CIO_B_POSI_RW ]  );
	    printf(   "MPB: %10.3f , ", bglmpio_prof_avg[ BGLMPIO_CIO_B_MPIO_RW ]  );
	    printf(  "MPBC: %10.3f , ", bglmpio_prof_avg[ BGLMPIO_CIO_B_MPIO_CRW ] );
	}
    }

}
