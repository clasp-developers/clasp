/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <sys/mman.h>
#include <stdlib.h>
#include <malloc.h>

#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/memoryhooks/memory.h"
#include "opal/memoryhooks/memory_internal.h"

#include "opal/mca/memory/linux/memory_linux.h"


/* Need to call a function in hooks.c to ensure that all those symbols
   get pulled in at link time (e.g., when building libmpi.a, so that
   those symbols end up in the final executable -- especially if we
   use --disable-dlopen and therefore -Wl,--export-dynamic isn't used
   when we build OMPI). */
extern void opal_memory_linux_hook_pull(bool *want_hooks);


/* 
 * Try to initialize ptmalloc2 
 */
int opal_memory_linux_ptmalloc2_open(void)
{
    int val = 0;
    void *p;
    bool want_hooks = true;

    /* Call a [somewhat] dummy function in hooks.c.  ***Do not remove
       this call!*** See comment at the beginning of this file
       explaining why it is here.  It will also check to see if an
       environment variable has been set to disable this component
       (note that OPAL_ERR_NOT_AVAILABLE is a special return value
       that will silently fail the open component call; all others
       will issue an error). */
    opal_memory_linux_hook_pull(&want_hooks);
    if (!want_hooks) {
        return OPAL_ERR_NOT_AVAILABLE;
    }

    /* We will also provide malloc/free support if we've been
       activated.  We don't exclusively rely on the
       __malloc_initialize_hook() previously being called because it's
       possible that our hook was called, but then someone else reset
       the hooks to point to something else (i.e., before MPI_INIT).
       So explicitly test here if our hooks are still in place.  If
       they are, then enable FREE|CHUNK_SUPPORT.  If not, then don't
       enable that support -- just leave it at MUNMAP_SUPPORT.

       (Look in hooks.c for the __malloc_initialize_hook setup) */

    /* Do a simple set of tests to see if our hooks are still the ones
       installed.  Explicitly reset the flags indicating that our
       functions were invoked */
    p = malloc(1024 * 1024 * 4);
    if (NULL == p) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    p = realloc(p, 1024 * 1024 * 4 + 32);
    if (NULL == p) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    free(p);
    p = memalign(4, 1024 * 1024);
    if (NULL == p) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    free(p);

#if HAVE_POSIX_MEMALIGN
    /* Double check for posix_memalign, too */
    if (mca_memory_linux_component.memalign_invoked) {
        mca_memory_linux_component.memalign_invoked = false;
        if (0 != posix_memalign(&p, sizeof(void*), 1024 * 1024)) {
            return OPAL_ERR_IN_ERRNO;
        }
        free(p);
    }
#endif

    if (mca_memory_linux_component.malloc_invoked &&
        mca_memory_linux_component.realloc_invoked &&
        mca_memory_linux_component.memalign_invoked &&
        mca_memory_linux_component.free_invoked) {
        /* Happiness; our functions were invoked */
        val |= OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_CHUNK_SUPPORT;
    }

    /* Check if our mmap layering is working */
    p = mmap(NULL, 4096, PROT_READ, (MAP_ANONYMOUS | MAP_PRIVATE), -1, 0);
    if (MAP_FAILED == p) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    munmap(p, 4096);
    if (mca_memory_linux_component.munmap_invoked) {
        val |= OPAL_MEMORY_MUNMAP_SUPPORT;
    }

    /* All done */
    if (val > 0) {
        opal_mem_hooks_set_support(val);
        return OPAL_SUCCESS;
    }
    return OPAL_ERR_NOT_AVAILABLE;
}


int opal_memory_linux_ptmalloc2_close(void)
{
    /* Nothing to do, really.  This function exists just for
       symmetry. */

    return OPAL_SUCCESS;
}
