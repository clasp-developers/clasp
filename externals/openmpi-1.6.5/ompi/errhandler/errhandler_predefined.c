/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      University of Houston. All rights reserved.
 * Copyright (c) 2008-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <stdlib.h>
#include <stdarg.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"
#include "ompi/errhandler/errhandler_predefined.h"
#include "ompi/errhandler/errcode.h"
#include "ompi/communicator/communicator.h"
#include "ompi/file/file.h"
#include "ompi/win/win.h"
#include "opal/util/printf.h"
#include "opal/util/output.h"

/*
 * Local functions
 */
static void backend_fatal(char *type, struct ompi_communicator_t *comm, 
                          char *name, int *error_code, va_list arglist);
static void out(char *str, char *arg);


void ompi_mpi_errors_are_fatal_comm_handler(struct ompi_communicator_t **comm,
					    int *error_code, ...)
{
  char *name;
  struct ompi_communicator_t *abort_comm;
  va_list arglist;

  va_start(arglist, error_code);

  if (NULL != comm) {
      name = (*comm)->c_name;
      abort_comm = *comm;
  } else {
      name = NULL;
      abort_comm = NULL;
  }
  backend_fatal("communicator", abort_comm, name, error_code, arglist);
  va_end(arglist);
}


void ompi_mpi_errors_are_fatal_file_handler(struct ompi_file_t **file,
					    int *error_code, ...)
{
  char *name;
  struct ompi_communicator_t *abort_comm;
  va_list arglist;

  va_start(arglist, error_code);

  if (NULL != file) {
      name = (*file)->f_filename;
      abort_comm = (*file)->f_comm;
  } else {
      name = NULL;
      abort_comm = NULL;
  }
  backend_fatal("file", abort_comm, name, error_code, arglist);
  va_end(arglist);
}


void ompi_mpi_errors_are_fatal_win_handler(struct ompi_win_t **win,
					   int *error_code, ...)
{
  char *name;
  struct ompi_communicator_t *abort_comm = NULL;
  va_list arglist;

  va_start(arglist, error_code);

  if (NULL != win) {
      name = (*win)->w_name;
  } else {
      name = NULL;
  }
  backend_fatal("win", abort_comm, name, error_code, arglist);
  va_end(arglist);
}

void ompi_mpi_errors_return_comm_handler(struct ompi_communicator_t **comm,
					 int *error_code, ...)
{
    /* Don't need anything more -- just need this function to exist */
    /* Silence some compiler warnings */
    
    va_list arglist;
    va_start(arglist, error_code);
    va_end(arglist);
}


void ompi_mpi_errors_return_file_handler(struct ompi_file_t **file,
					 int *error_code, ...)
{
    /* Don't need anything more -- just need this function to exist */
    /* Silence some compiler warnings */
    
    va_list arglist;
    va_start(arglist, error_code);
    va_end(arglist);
}


void ompi_mpi_errors_return_win_handler(struct ompi_win_t **win,
					int *error_code, ...)
{
    /* Don't need anything more -- just need this function to exist */
    /* Silence some compiler warnings */
    
    va_list arglist;
    va_start(arglist, error_code);
    va_end(arglist);
}


static void out(char *str, char *arg)
{
    if (ompi_mpi_initialized && !ompi_mpi_finalized) {
        if (NULL != arg) {
            opal_output(0, str, arg);
        } else {
            opal_output(0, "%s", str);
        }
    } else {
        if (NULL != arg) {
            fprintf(stderr, str, arg);
        } else {
            fprintf(stderr, "%s", str);
        }
    }
}

/*
 * Use orte_show_help() to aggregate the error messages (i.e., show it
 * once rather than N times).  
 *
 * Note that this function will only be invoked for errors during the
 * MPI application (i.e., after MPI_INIT and before MPI_FINALIZE).  So
 * there's no need to handle the pre-MPI_INIT and post-MPI_FINALIZE
 * errors here.
 */
static void backend_fatal_aggregate(char *type, 
                                    struct ompi_communicator_t *comm,
                                    char *name, int *error_code, 
                                    va_list arglist)
{
    char *arg, *prefix, *err_msg = "Unknown error";
    bool err_msg_need_free = false;

    arg = va_arg(arglist, char*);
    va_end(arglist);

    asprintf(&prefix, "[%s:%d]", orte_process_info.nodename,
             (int) orte_process_info.pid);

    if (NULL != error_code) {
        err_msg = ompi_mpi_errnum_get_string(*error_code);
        if (NULL == err_msg) {
            err_msg_need_free = true;
            asprintf(&err_msg, "Error code: %d (no associated error message)",
                     *error_code);
        }
    }

    if (NULL != name && ompi_mpi_initialized && !ompi_mpi_finalized) {
        orte_show_help("help-mpi-errors.txt", 
                       "mpi_errors_are_fatal", false,
                       prefix, (NULL == arg) ? "" : "in",
                       (NULL == arg) ? "" : arg,
                       prefix, type, name, prefix, err_msg, prefix);
    } else if (NULL == name) {
        orte_show_help("help-mpi-errors.txt", 
                       "mpi_errors_are_fatal unknown handle", false,
                       prefix, (NULL == arg) ? "" : "in",
                       (NULL == arg) ? "" : arg,
                       prefix, type, prefix, err_msg, prefix);
    }

    if (err_msg_need_free) {
        free(err_msg);
    }
}

/* 
 * Note that this function has to handle pre-MPI_INIT and
 * post-MPI_FINALIZE errors, which backend_fatal_aggregate() does not
 * have to handle.
 */
static void backend_fatal_no_aggregate(char *type, 
                                       struct ompi_communicator_t *comm,
                                       char *name, int *error_code, 
                                       va_list arglist)
{
    char *arg;

    fflush(stdout);
    fflush(stderr);

    arg = va_arg(arglist, char*);

    /* Per #2152, print out in plain english if something was invoked
       before MPI_INIT* or after MPI_FINALIZE */
    if (!ompi_mpi_init_started && !ompi_mpi_initialized) {
        if (NULL != arg) {
            out("*** The %s() function was called before MPI_INIT was invoked.\n"
                "*** This is disallowed by the MPI standard.\n", arg);
        } else {
            out("*** An MPI function was called before MPI_INIT was invoked.\n"
                "*** This is disallowed by the MPI standard.\n"
                "*** Unfortunately, no further information is available on *which* MPI\n"
                "*** function was invoked, sorry.  :-(\n", NULL);
        }
        out("*** Your MPI job will now abort.\n", NULL);
    } else if (ompi_mpi_finalized) {
        if (NULL != arg) {
            out("*** The %s() function was called after MPI_FINALIZE was invoked.\n"
                "*** This is disallowed by the MPI standard.\n", arg);
        } else {
            out("*** An MPI function was called after MPI_FINALIZE was invoked.\n"
                "*** This is disallowed by the MPI standard.\n"
                "*** Unfortunately, no further information is available on *which* MPI\n"
                "*** function was invoked, sorry.  :-(\n", NULL);
        }
        out("*** Your MPI job will now abort.\n", NULL);
    }

    else {
        int len;
        char str[MPI_MAX_PROCESSOR_NAME * 2];
        
        /* THESE MESSAGES ARE COORDINATED WITH FIXED STRINGS IN
           help-mpi-errors.txt!  Do not change these messages without
           also changing help-mpi-errors.txt! */

        /* This is after MPI_INIT* and before MPI_FINALIZE, so print
           the error message normally */
        if (NULL != arg) {
            out("*** An error occurred in %s\n", arg);
        } else {
            out("*** An error occurred\n", NULL);
        }

        if (NULL != name) {
            /* Don't use asprintf() here because there may be stack /
               heap corruption by the time we're invoked, so just do
               it on the stack */
            str[0] = '\0';
            len = sizeof(str) - 1;
            strncat(str, type, len);
            
            len -= strlen(type);
            if (len > 0) {
                strncat(str, " ", len);
                
                --len;
                if (len > 0) {
                    strncat(str, name, len);
                }
            }
            out("*** on %s", str);
        } else if (NULL == name) {
            out("*** on a NULL %s\n", type);
        }

        if (NULL != error_code) {
            char *tmp = ompi_mpi_errnum_get_string(*error_code);
            if (NULL != tmp) {
                out("*** %s\n", tmp);
            } else {
                char intbuf[32];
                snprintf(intbuf, 32, "%d", *error_code);
                out("*** Error code: %d (no associated error message)\n", intbuf);
            }
        }
        out("*** MPI_ERRORS_ARE_FATAL: your MPI job will now abort\n", NULL);
    }
    va_end(arglist);
}

static void backend_fatal(char *type, struct ompi_communicator_t *comm,
                          char *name, int *error_code, 
                          va_list arglist)
{
    /* Do we want help message aggregation?  Usually yes, but it uses
       malloc(), which may cause further errors if we're exiting due
       to a memory problem.  So we also have the option to *not*
       aggregate (which doesn't use malloc during its call stack,
       meaning that there is a better chance that the error message
       will actually get printed).  Note that we can only do
       aggregation after MPI_INIT and before MPI_FINALIZE. */
    if (orte_help_want_aggregate && orte_show_help_is_available()) {
        backend_fatal_aggregate(type, comm, name, error_code, arglist);
    } else {
        backend_fatal_no_aggregate(type, comm, name, error_code, arglist);
    }

    /* Should we do something more intelligent than just using
       COMM_SELF? */
    if (comm == NULL) {
        comm = &ompi_mpi_comm_self.comm;
    }

    if (NULL != error_code) {
        ompi_mpi_abort(comm, *error_code, false);
    } else {
        ompi_mpi_abort(comm, 1, false);
    }
}
