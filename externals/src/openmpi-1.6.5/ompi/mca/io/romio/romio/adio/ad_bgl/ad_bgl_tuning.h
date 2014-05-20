/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_bgl_tuning.h
 * \brief ???
 */

/*---------------------------------------------------------------------
 * ad_bgl_tuning.h
 *
 * declares global variables and macros for performance tuning and 
 * functional debugging.
 *---------------------------------------------------------------------*/

#ifndef AD_BGL_TUNING_H_
#define AD_BGL_TUNING_H_

#include "adio.h"

#define AD_BGL_assert( a ) if (!(a)) { \
                                fprintf( stderr, "AD_BGL_assert, file=%s, line=%d\n", __FILE__, __LINE__ ); \
                                MPI_Abort( MPI_COMM_WORLD, 1 ); \
                           }

/*-----------------------------------------
 *  Global variables for the control of
 *  1.  timing
 *  2.  select specific optimizations
 *-----------------------------------------*/

/* timing fields */
enum {
    BGLMPIO_CIO_DATA_SIZE=0,	
    BGLMPIO_CIO_T_SEEK,		
    BGLMPIO_CIO_T_LCOMP,	/* time for ADIOI_Calc_my_off_len(), local */
    BGLMPIO_CIO_T_GATHER,	/* time for previous MPI_Allgather, now Allreduce */
    BGLMPIO_CIO_T_PATANA,	/* time for a quick test if access is contiguous or not, local */
    BGLMPIO_CIO_T_FD_PART,	/* time for file domain partitioning, local */
    BGLMPIO_CIO_T_MYREQ,	/* time for ADIOI_BGL_Calc_my_req(), local */
    BGLMPIO_CIO_T_OTHREQ,	/* time for ADIOI_Calc_others_req(), short Alltoall */
    BGLMPIO_CIO_T_DEXCH,	/* time for I/O data exchange */
    BGLMPIO_CIO_T_POSI_RW,
    BGLMPIO_CIO_B_POSI_RW,
    BGLMPIO_CIO_T_MPIO_RW,	/* time for ADIOI_BGL_WriteContig() */
    BGLMPIO_CIO_B_MPIO_RW,
    BGLMPIO_CIO_T_MPIO_CRW,	/* time for ADIOI_BGL_WriteStridedColl() */
    BGLMPIO_CIO_B_MPIO_CRW,
    BGLMPIO_CIO_LAST
};

extern double 	bglmpio_prof_cw    [BGLMPIO_CIO_LAST];
extern double 	bglmpio_prof_cr    [BGLMPIO_CIO_LAST];


/* corresponds to environment variables to select optimizations and timing level */
extern int 	bglmpio_timing;
extern int 	bglmpio_timing2;
extern int 	bglmpio_comm;
extern int 	bglmpio_tunegather;
extern int 	bglmpio_tuneblocking;


/* set internal variables for tuning environment variables */
void ad_bgl_get_env_vars();

/* report timing breakdown for MPI I/O collective call */
void ad_bgl_timing_crw_report( int rw, ADIO_File fd, int myrank, int nprocs );

/* note: 	
 *   T := timing; 
 * CIO := collective I/O 
 */
#define BGLMPIO_T_CIO_RESET( LEVEL, RW ) \
	if (bglmpio_timing_cw_level >= LEVEL) { \
	  int i; \
	  for ( i = 0; i < BGLMPIO_T_LAST; i ++ ) \
	    bglmpio_prof_c##RW [ i ] = 0; \
	}

#define BGLMPIO_T_CIO_REPORT( LEVEL, RW, FD, MYRANK, NPROCS ) \
	if (bglmpio_timing_cw_level >= LEVEL) { \
	  ad_bgl_timing_crw_report ( RW, FD, MYRANK, NPROCS ); \
   	}

#define BGLMPIO_T_CIO_SET_GET( LEVEL, RW, DOBAR, ISSET, ISGET, VAR1, VAR2 ) \
	if (bglmpio_timing_cw_level >= LEVEL) { \
	  if ( DOBAR ) MPI_Barrier( fd->comm ); \
	  double temp = MPI_Wtime(); \
	  if ( ISSET ) bglmpio_prof_c##RW [ VAR1 ] = temp; \
	  if ( ISGET ) bglmpio_prof_c##RW [ VAR2 ] = temp - bglmpio_prof_c##RW [ VAR2 ] ; \
	}

#endif  /* AD_BGL_TUNING_H_ */
