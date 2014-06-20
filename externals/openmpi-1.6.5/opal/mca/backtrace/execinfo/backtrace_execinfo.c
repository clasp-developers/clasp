/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>
#ifdef HAVE_EXECINFO_H
#include <execinfo.h>
#endif

#include "opal/constants.h"
#include "opal/mca/backtrace/backtrace.h"

void
opal_backtrace_print(FILE *file)
{
    int i;
    int trace_size;
    void * trace[32];
    char ** messages = (char **)NULL;

    trace_size = backtrace (trace, 32);
    messages = backtrace_symbols (trace, trace_size);

    for (i = 0; i < trace_size; i++) {
        fprintf(file, "[%d] func:%s\n", i, messages[i]);
        fflush(file);
    }

    free(messages);
}


int
opal_backtrace_buffer(char ***message_out, int *len_out) 
{
    int trace_size;
    void * trace[32];
    char ** funcs = (char **)NULL;

    trace_size = backtrace (trace, 32);
    funcs = backtrace_symbols (trace, trace_size);

    *message_out = funcs;
    *len_out = trace_size;

    return OPAL_SUCCESS;
}
