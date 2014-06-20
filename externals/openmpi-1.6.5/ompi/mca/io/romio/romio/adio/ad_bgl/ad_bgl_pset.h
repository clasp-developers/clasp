/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_bgl_pset.h
 * \brief ???
 */

/* File: ad_bgl_pset.h
 * 
 * Defines two structures that keep BG/L PSET specific information and their public interfaces:
 * 	. ADIOI_BGL_ProcInfo_t object keeps specific information to each process
 * 	. ADIOI_BGL_ConfInfo_t object keeps general information for the whole communicator, only kept
 *	  on process 0.
 */

#ifndef AD_BGL_PSET_H_
#define AD_BGL_PSET_H_

/* Keeps specific information to each process, will be exchanged among processes */
typedef struct {

    int psetNum;	/* which PSET I am in */
    int rank;		/* my rank */
    int xInPset;	/* my relative coordinates in my PSET */
    int yInPset;
    int zInPset;
    int cpuid;		/* my CPU id -- for virtual node mode (t coord)*/
    int rankInPset;	/* my relative rank in my PSET */

} ADIOI_BGL_ProcInfo_t __attribute__((aligned(16)));


/* Keeps general information for the whole communicator, only on process 0 */
typedef struct {

    int PsetSize;
    int nAggrs;
    int numPsets;
    int isVNM;
    int virtualPsetSize;
    int nProcs;
    float aggRatio;
    int cpuidSize;       /* how many cpu ids? (t size) */

} ADIOI_BGL_ConfInfo_t __attribute__((aligned(16)));


#undef MIN
#define MIN(a,b) ((a)<(b) ? (a) : (b))


/* Default is to choose 8 aggregator nodes in each 32 CN pset. 
   Also defines default ratio of aggregator nodes in each a pset.
   For Virtual Node Mode, the ratio is 8/64 */
#define ADIOI_BGL_NAGG_PSET_MIN  1
#define ADIOI_BGL_NAGG_PSET_DFLT 8
#define ADIOI_BGL_PSET_SIZE_DFLT 32


/* public funcs for ADIOI_BGL_ProcInfo_t objects */
    ADIOI_BGL_ProcInfo_t * ADIOI_BGL_ProcInfo_new();
    ADIOI_BGL_ProcInfo_t * ADIOI_BGL_ProcInfo_new_n( int n );
    void ADIOI_BGL_ProcInfo_free( ADIOI_BGL_ProcInfo_t *info );


/* public funcs for ADIOI_BGL_ConfInfo_t objects */
    ADIOI_BGL_ConfInfo_t * ADIOI_BGL_ConfInfo_new ();
    void ADIOI_BGL_ConfInfo_free( ADIOI_BGL_ConfInfo_t *info );


/* public funcs for a pair of ADIOI_BGL_ConfInfo_t and ADIOI_BGL_ProcInfo_t objects */
    void ADIOI_BGL_persInfo_init( ADIOI_BGL_ConfInfo_t *conf, 
				  ADIOI_BGL_ProcInfo_t *proc, 
				  int s, int r, int n_aggrs );
    void ADIOI_BGL_persInfo_free( ADIOI_BGL_ConfInfo_t *conf, 
				  ADIOI_BGL_ProcInfo_t *proc );


#endif  /* AD_BGL_PSET_H_ */
