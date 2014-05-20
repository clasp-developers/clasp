/*
 Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
                         University Research and Technology
                         Corporation.  All rights reserved.
 Copyright (c) 2004-2005 The University of Tennessee and The University
                         of Tennessee Research Foundation.  All rights
                         reserved.
 Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
                         University of Stuttgart.  All rights reserved.
 Copyright (c) 2004-2005 The Regents of the University of California.
                         All rights reserved.
 $COPYRIGHT$
 
 Additional copyrights may follow
 
 $HEADER$
 */

#include "opal_config.h"
#include "win32/ompi_process.h"

pid_t waitpid(pid_t pid, int *status, int options) {
   return _cwait(status, pid, _WAIT_CHILD);
}

int kill(pid_t pid, int sig) {
   /* XXX fill this in */
   /* Need to connect to the child process
      Then raise the signal since Windows doesn;t
      have the ability to 'send a signal' to a 
      process, a la the kill command in UNIX
      
      MSVC functions to look at:
       - OpenProcess
       - TerminateProcess
       - raise
   */
   
   return 0;
}
