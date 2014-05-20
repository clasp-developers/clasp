/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_UIO_H
#define OMPI_UIO_H

#include "opal_config.h"

#ifndef OMPI_WIN_COMPAT_H
#error This file is supposed to be included only from win_compat.h
#endif  /* OMPI_WIN_COMPAT_H */

/* define the iovec structure */
struct iovec {
  WSABUF data;
};
#define iov_base data.buf
#define iov_len data.len

BEGIN_C_DECLS
/*
 * writev:
   writev  writes  data  to  file  descriptor  fd,  and  from  the buffers
   described by iov. The number of buffers is specified by  cnt.  The
   buffers  are  used  in  the  order specified.  Operates just like write
   except that data is taken from iov instead of a contiguous buffer.
 */
OPAL_DECLSPEC int writev (int fd, struct iovec *iov, int cnt);

/* 
   readv  reads  data  from file descriptor fd, and puts the result in the
   buffers described by iov. The number  of  buffers  is  specified  by
   cnt.  The  buffers  are filled in the order specified.  Operates just
   like read except that data is put in iov  instead  of  a  contiguous
   buffer.
 */
OPAL_DECLSPEC int readv (int fd, struct iovec *iov, int cnt);

END_C_DECLS

#endif /* OMPI_UIO_H */
