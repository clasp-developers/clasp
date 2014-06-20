/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_bgl_close.c
 * \brief ???
 */

/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_bgl.h"
#include "ad_bgl_aggrs.h"

void ADIOI_BGL_Close(ADIO_File fd, int *error_code)
{
  int err, derr=0;
  static char myname[] = "ADIOI_BGL_CLOSE";

#ifdef PROFILE
  MPE_Log_event(9, 0, "start close");
#endif

  err = close(fd->fd_sys);
  if (fd->fd_direct >= 0)
  {
    derr = close(fd->fd_direct);
  }

#ifdef PROFILE
  MPE_Log_event(10, 0, "end close");
#endif

/*  FPRINTF(stderr,"%s(%d):'%s'. Free %#X\n",myname,__LINE__,fd->filename,(int)fd->fs_ptr);*/
  if (fd->fs_ptr != NULL) {
	  ADIOI_Free(fd->fs_ptr);
	  fd->fs_ptr = NULL;
  }
  fd->fd_sys    = -1;
  fd->fd_direct = -1;

  if (err == -1 || derr == -1)
  {
    *error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
                                       myname, __LINE__, MPI_ERR_IO,
                                       "**io",
                                       "**io %s", strerror(errno));
  }
  else *error_code = MPI_SUCCESS;
}
