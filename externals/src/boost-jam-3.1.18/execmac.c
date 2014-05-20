/*
 * Copyright 1993, 1995 Christopher Seiwald.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

#include "jam.h"
#include "lists.h"
#include "execcmd.h"
#include <errno.h>

#ifdef OS_MAC

/*
 * execunix.c - execute a shell script on UNIX
 *
 * If $(JAMSHELL) is defined, uses that to formulate execvp().
 * The default is:
 *
 *  /bin/sh -c %
 *
 * Each word must be an individual element in a jam variable value.
 *
 * In $(JAMSHELL), % expands to the command string and ! expands to
 * the slot number (starting at 1) for multiprocess (-j) invocations.
 * If $(JAMSHELL) doesn't include a %, it is tacked on as the last
 * argument.
 *
 * Don't just set JAMSHELL to /bin/sh - it won't work!
 *
 * External routines:
 *  exec_cmd()  - launch an async command execution.
 *  exec_wait() - wait and drive at most one execution completion.
 *
 * Internal routines:
 *  onintr() - bump intr to note command interruption.
 *
 * 04/08/94 (seiwald) - Coherent/386 support added.
 * 05/04/94 (seiwald) - async multiprocess interface
 * 01/22/95 (seiwald) - $(JAMSHELL) support
 */


/*
 * exec_cmd() - launch an async command execution.
 */

void exec_cmd
(
    char * string,
    void (* func)( void * closure, int status, timing_info *, char *, char * ),
    void * closure,
    LIST * shell
)
{
    printf( "%s", string );
    (*func)( closure, EXEC_CMD_OK );
}

/*
 * exec_wait() - wait and drive at most one execution completion.
 */

int exec_wait()
{
    return 0;
}

#endif /* OS_MAC */
