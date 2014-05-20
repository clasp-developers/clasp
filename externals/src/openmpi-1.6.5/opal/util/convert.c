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

#include "opal_config.h"

#include <stdio.h>
#include <stdlib.h>

#include "opal/util/convert.h"
#include "opal/constants.h"

#if SIZEOF_SIZE_T <= SIZEOF_INT
/*
 * This is the [short] case where we can just cast and we're all good
 */
int opal_size2int(size_t in, int *out, bool want_check)
{
    *out = (int)in;
    return OPAL_SUCCESS;
}

#else
/*
 * The rest of the file handles the case where
 * sizeof(size_t)>sizeof(int).
 */

static bool init_done = false;
static unsigned int int_pos = 0;


static void opal_size2int_init(void);
static void warn(void);


int opal_size2int(size_t in, int *out, bool want_check)
{
    int *pos = (int *) &in;
    unsigned int i;

    if (!init_done) {
        opal_size2int_init();
    }

    *out = pos[int_pos];
    if (want_check) {
        /* Remember that size_t is unsigned, so we don't need to check
           for when in < 0 (in which case the internal checks would be
           slightly different) */
        for (i = 0; i < (sizeof(in) / sizeof(*out)); ++i) {
            if (i != int_pos) {
                if (pos[i] != 0) {
                    warn();
                    return OPAL_ERR_NOT_IMPLEMENTED;
                }
            }
        }
    }

    return OPAL_SUCCESS;
}


static void opal_size2int_init(void)
{
    size_t bogus = 1;
    int *i = (int *) &bogus;

    for (int_pos = 0; int_pos < (sizeof(bogus) / sizeof(int)); ++int_pos) {
        if (i[int_pos] == 1) {
            break;
        }
    }

    init_done = true;
}


static void warn(void)
{
#if OPAL_ENABLE_DEBUG
    /* Developer builds */
    fprintf(stderr, "WARNING: A size_t value was attempted to be cast to an int (sizeof(size_t) == %ld, sizeof(int) == %ld), but data was lost in the conversion.  This should never happen (i.e., we should never try to convert a value that will be 'too big').  Since this is a developer build, I'm going to abort, and you can check the corefile.  Enjoy.\n", (long) sizeof(size_t), (long) sizeof(int));
    abort();
#else
    static bool warned = false;

    if (!warned) {
        fprintf(stderr, "Open MPI WARNING: A bad cast (size_t->int) occurred.\n");
        fprintf(stderr, "Please inform the Open MPI developers.  This message will not repeat.\n");
        fprintf(stderr, "Attempting to continue (no guarantees about correctness...\n");
        warned = true;
    }
#endif
}

#endif
