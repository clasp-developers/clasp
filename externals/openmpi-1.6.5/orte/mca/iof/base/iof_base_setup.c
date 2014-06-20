/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#include <sys/types.h>
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include <signal.h>
#ifdef HAVE_UTIL_H
#include <util.h>
#endif
#ifdef HAVE_PTY_H
#include <pty.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_TERMIOS_H
#include <termios.h>
# ifdef HAVE_TERMIO_H
#  include <termio.h>
# endif
#endif
#ifdef HAVE_LIBUTIL_H
#include <libutil.h>
#endif

#include "opal/util/opal_pty.h"
#include "opal/util/opal_environ.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/iof_base_setup.h"

int
orte_iof_base_setup_prefork(orte_iof_base_io_conf_t *opts)
{
    int ret = -1;

    fflush(stdout);

    /* first check to make sure we can do ptys */
#if OPAL_ENABLE_PTY_SUPPORT
    if (opts->usepty) {
        /**
         * It has been reported that on MAC OS X 10.4 and prior one cannot
         * safely close the writing side of a pty before completly reading
         * all data inside.
         * There seems to be two issues: first all pending data is
         * discarded, and second it randomly generate kernel panics.
         * Apparently this issue was fixed in 10.5 so by now we use the
         * pty exactly as we use the pipes.
         * This comment is here as a reminder.
         */
        ret = opal_openpty(&(opts->p_stdout[0]), &(opts->p_stdout[1]),
                           (char*)NULL, (struct termios*)NULL, (struct winsize*)NULL);
    }
#else
    opts->usepty = 0;
#endif

#if defined(__WINDOWS__)
    /* Windows doesn't have a 'pipe' function.
     * So we need to do something a bit more complex */
    /* 
     * http://www-106.ibm.com/developerworks/linux/library/l-rt4/?open&t=grl,l=252,p=pipes
     */
#else
    if (ret < 0) {
        if (pipe(opts->p_stdout) < 0) {
            ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_PIPES);
            return ORTE_ERR_SYS_LIMITS_PIPES;
        }
    }
    if (pipe(opts->p_stdin) < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_PIPES);
        return ORTE_ERR_SYS_LIMITS_PIPES;
    }
    if (pipe(opts->p_stderr) < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_PIPES);
        return ORTE_ERR_SYS_LIMITS_PIPES;
    }
    if (pipe(opts->p_internal) < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_PIPES);
        return ORTE_ERR_SYS_LIMITS_PIPES;
    }
#endif

    return ORTE_SUCCESS;
}


int
orte_iof_base_setup_child(orte_iof_base_io_conf_t *opts, char ***env)
{
    int ret;
    char *str;

    close(opts->p_stdin[1]);
    close(opts->p_stdout[0]);
    close(opts->p_stderr[0]);
    close(opts->p_internal[0]);

    if (opts->usepty) {
#ifndef __WINDOWS__
            /* disable echo */
            struct termios term_attrs;
            if (tcgetattr(opts->p_stdout[1], &term_attrs) < 0) {
                return ORTE_ERR_PIPE_SETUP_FAILURE;
            }
            term_attrs.c_lflag &= ~ (ECHO | ECHOE | ECHOK |
                                     ECHOCTL | ECHOKE | ECHONL);
            term_attrs.c_iflag &= ~ (ICRNL | INLCR | ISTRIP | INPCK | IXON);
            term_attrs.c_oflag &= ~ (
#ifdef OCRNL
                                     /* OS X 10.3 does not have this
                                        value defined */
                                     OCRNL | 
#endif
                                     ONLCR);
            if (tcsetattr(opts->p_stdout[1], TCSANOW, &term_attrs) == -1) {
                return ORTE_ERR_PIPE_SETUP_FAILURE;
            }
#endif
        ret = dup2(opts->p_stdout[1], fileno(stdout));
        if (ret < 0) return ORTE_ERR_PIPE_SETUP_FAILURE;
        close(opts->p_stdout[1]);
    } else {
        if(opts->p_stdout[1] != fileno(stdout)) {
            ret = dup2(opts->p_stdout[1], fileno(stdout));
            if (ret < 0) return ORTE_ERR_PIPE_SETUP_FAILURE;
            close(opts->p_stdout[1]); 
        }
    }
    if (opts->connect_stdin) {
        if(opts->p_stdin[0] != fileno(stdin)) {
            ret = dup2(opts->p_stdin[0], fileno(stdin));
            if (ret < 0) return ORTE_ERR_PIPE_SETUP_FAILURE;
            close(opts->p_stdin[0]); 
        }
    } else {
        int fd;

        close(opts->p_stdin[0]);
        /* connect input to /dev/null */
        fd = open("/dev/null", O_RDONLY, 0);
        if(fd > fileno(stdin)) {
            dup2(fd, fileno(stdin));
            close(fd);
        }
    }
    if(opts->p_stderr[1] != fileno(stderr)) {
        ret = dup2(opts->p_stderr[1], fileno(stderr));
        if (ret < 0) return ORTE_ERR_PIPE_SETUP_FAILURE;
        close(opts->p_stderr[1]);
    }

    /* Set an environment variable that the new child process can use
       to get the fd of the pipe connected to the INTERNAL IOF tag. */
    asprintf(&str, "%d", opts->p_internal[1]);
    if (NULL != str) {
        opal_setenv("OPAL_OUTPUT_STDERR_FD", str, true, env);
        free(str);
    }

    return ORTE_SUCCESS;
}


int
orte_iof_base_setup_parent(const orte_process_name_t* name,
                           orte_iof_base_io_conf_t *opts)
{
    int ret;

    close(opts->p_stdin[0]);
    close(opts->p_stdout[1]);
    close(opts->p_stderr[1]);
    close(opts->p_internal[1]);

    /* connect stdin endpoint */
    if (opts->connect_stdin) {
        /* and connect the pty to stdin */
        ret = orte_iof.pull(name, ORTE_IOF_STDIN, opts->p_stdin[1]);
        if(ORTE_SUCCESS != ret) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    } else {
        close(opts->p_stdin[1]);
    }

    /* connect read ends to IOF */
    ret = orte_iof.push(name, ORTE_IOF_STDOUT, opts->p_stdout[0]);
    if(ORTE_SUCCESS != ret) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    ret = orte_iof.push(name, ORTE_IOF_STDERR, opts->p_stderr[0]);
    if(ORTE_SUCCESS != ret) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    ret = orte_iof.push(name, ORTE_IOF_STDDIAG, opts->p_internal[0]);
    if(ORTE_SUCCESS != ret) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    return ORTE_SUCCESS;
}
