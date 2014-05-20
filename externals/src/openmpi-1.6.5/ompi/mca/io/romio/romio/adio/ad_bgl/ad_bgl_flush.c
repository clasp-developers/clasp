/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_bgl_flush.c
 * \brief Scalable flush based on underlying filesystem and psets
 */

/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_bgl.h"
#include "ad_bgl_aggrs.h"

void ADIOI_BGL_Flush(ADIO_File fd, int *error_code)
{
  int err=0;
  static char myname[] = "ADIOI_BGL_FLUSH";


  if(((ADIOI_BGL_fs*)fd->fs_ptr)->fsync_aggr & ADIOI_BGL_FSYNC_AGGREGATION_ENABLED)
  {
    int rank;
 
    /* Barrier so we can collectively do fewer fsync's */
    MPI_Barrier(fd->comm);
  
    MPI_Comm_rank(fd->comm, &rank);
  
    /* All ranks marked as "fsync aggregators" should fsync. 
       (We currently only do one fsync on rank 0 but this is general 
       enough to support >1 aggregator using allreduce to get the
       results instead of simply bcast'ing the results from rank 0.)*/
    if(((ADIOI_BGL_fs*)fd->fs_ptr)->fsync_aggr & ADIOI_BGL_FSYNC_AGGREGATOR)
    {
      err = fsync(fd->fd_sys);
      DBG_FPRINTF(stderr,"aggregation:fsync %s, err=%#X, errno=%#X\n",fd->filename, err, errno);
      /* We want errno, not the return code if it failed */
      if (err == -1) err = errno;
      else err = 0;
    }
    /* Just pick an errno (using unsigned MPI_MAX) from any failures */
    MPI_Allreduce( MPI_IN_PLACE, (unsigned*)&err, 1, MPI_UNSIGNED, MPI_MAX, fd->comm);
    DBGV_FPRINTF(stderr,"aggregation result:fsync %s, errno %#X,\n",fd->filename, err);

    if (err) /* if it's non-zero, it must be an errno */
    {
      errno = err;
      err = -1;
    }
  }
  else /* Non-aggregated fsync */
  {
#ifdef USE_DBG_LOGGING
    int rank;
#endif
    err = fsync(fd->fd_sys);
#ifdef USE_DBG_LOGGING
    MPI_Comm_rank(fd->comm, &rank);

    if(rank == 0)
    {
        DBG_FPRINTF(stderr,"no aggregation:fsync %s, err=%#X, errno=%#X\n",fd->filename, err, errno);
    }
    else
    {
        DBGV_FPRINTF(stderr,"no aggregation:fsync %s, err=%#X, errno=%#X\n",fd->filename, err, errno);
    }
#endif
  }

  /* --BEGIN ERROR HANDLING-- */
  if (err == -1)
  {
    *error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
                                       myname, __LINE__, MPI_ERR_IO,
                                       "**io",
                                       "**io %s", strerror(errno));
    DBGT_FPRINTF(stderr,"fsync %s, err=%#X, errno=%#X\n",fd->filename, err, errno);
    return;
  }
  /* --END ERROR HANDLING-- */

  *error_code = MPI_SUCCESS;
}

