/* @file */
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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/util/trace.h"

int opal_trace_handle;

/*
 * Local state
 */
#if OPAL_ENABLE_TRACE
static opal_output_stream_t tracer;
#endif


void opal_trace_init(void)
{
#if OPAL_ENABLE_TRACE
    int param, value;

    param = mca_base_param_reg_int_name("trace", "verbose",
                                        "Verbosity level for opal trace system",
                                        false, false, 0, &value);

    OBJ_CONSTRUCT(&tracer, opal_output_stream_t);

    /* if the value is < 0, then we want the output to go to the screen */
    if (0 > value) {
        tracer.lds_want_file = false;
        tracer.lds_want_stderr = true;
        value = -1 * value;
    } else if (0 == value) { /* don't provide any output */
        opal_trace_handle = -1;
        return;
    } else {
        /* get a file setup for opal_output to use for the trace */
        tracer.lds_file_suffix = "trace";
        tracer.lds_want_file = true;
    }

    tracer.lds_verbose_level = value;

    opal_trace_handle = opal_output_open(&tracer);
#endif
}

void opal_trace_finalize(void)
{
#if OPAL_ENABLE_TRACE
    opal_output_close(opal_trace_handle);
    OBJ_DESTRUCT(&tracer);
#endif
}
