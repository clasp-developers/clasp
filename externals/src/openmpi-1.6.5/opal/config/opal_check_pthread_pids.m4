dnl
dnl Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OMPI_CHECK_PTHREAD_PIDS],[
#
# Arguments: none
#
# Dependencies: None
#
# Sets:
#  OPAL_THREADS_HAVE_DIFFERENT_PIDS (variable)
#
# Test for Linux-like threads in the system. We will need to handle things like
# getpid() differently in the case of a Linux-like threads model.
#

AC_MSG_CHECKING([if threads have different pids (pthreads on linux)])
CFLAGS_save="$CFLAGS"
CFLAGS="$CFLAGS $THREAD_CFLAGS"
CPPFLAGS_save="$CPPFLAGS"
CPPFLAGS="$CPPFLAGS $THREAD_CPPFLAGS"
LDFLAGS_save="$LDFLAGS"
LDFLAGS="$LDFLAGS $THREAD_LDFLAGS"
LIBS_save="$LIBS"
LIBS="$LIBS $THREAD_LIBS"
AC_TRY_RUN([#include <pthread.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
void *checkpid(void *arg);
int main() {
  pthread_t thr;
  int pid, *retval;
  pid = getpid();
  pthread_create(&thr, NULL, checkpid, &pid);
  pthread_join(thr, (void **) &retval);
  exit(*retval);
}
static int ret;
void *checkpid(void *arg) {
   int ppid = *((int *) arg);
   if (ppid == getpid())
     ret = 0;
   else
     ret = 1;
   pthread_exit((void *) &ret);
}], 
[MSG=no OPAL_THREADS_HAVE_DIFFERENT_PIDS=0], 
[MSG=yes OPAL_THREADS_HAVE_DIFFERENT_PIDS=1],
[case $host in
     *-linux*)
         MSG="cross compiling - assuming yes"
         OPAL_THREADS_HAVE_DIFFERENT_PIDS=1
         ;;
     *)
         MSG="cross compiling - assuming no"
         OPAL_THREADS_HAVE_DIFFERENT_PIDS=0
         ;;
 esac
])

CFLAGS="$CFLAGS_save"
CPPFLAGS="$CPPFLAGS_save"
LDFLAGS="$LDFLAGS_save"
LIBS="$LIBS_save"

AC_MSG_RESULT([$MSG])
AC_DEFINE_UNQUOTED(OPAL_THREADS_HAVE_DIFFERENT_PIDS, $OPAL_THREADS_HAVE_DIFFERENT_PIDS)

#
# if pthreads is not available, then the system does not have an insane threads
# model
#
unset MSG])dnl
