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
 * Copyright (c) 2007-2011 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2009-2011 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/mca/memory/memory.h"
#include "opal/mca/memory/base/empty.h"
#include "opal/memoryhooks/memory_internal.h"
#include "opal/constants.h"

#include <sys/types.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <dlfcn.h>
#if defined(HAVE___MUNMAP)
/* here so we only include others if we absolutely have to */
#elif defined(HAVE_SYSCALL)
#include <sys/syscall.h>
#include <unistd.h>
#endif


#if defined(HAVE___MUNMAP)
int  __munmap(caddr_t addr, size_t len);
#endif

static int opal_memory_malloc_open(void);

const opal_memory_base_component_2_0_0_t mca_memory_malloc_solaris_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        OPAL_MEMORY_BASE_VERSION_2_0_0,

        /* Component name and version */
        "malloc_solaris",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        opal_memory_malloc_open,
        NULL
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    /* This component doesn't need these functions, but need to
       provide safe/empty register/deregister functions to call */
    NULL,
    opal_memory_base_component_register_empty,
    opal_memory_base_component_deregister_empty,
};

/*
 * This component exists to establish the memory hook support
 * level available on Solaris. By default Solaris does not
 * return memory to the system, i.e. does not unmap memory
 * from the process space, when a user calls free(). This allows
 * us to declare OPAL_MEMORY_FREE_SUPPORT. Additionally, by
 * intercepting munmap we can declare OPAL_MEMORY_MUNMAP_SUPPORT.
 *
 * NOTE: Not releasing memory back to the system when calling
 * free() may be unique to Solaris which is why this component
 * was created.
 */
static int
opal_memory_malloc_open(void)
{
    opal_mem_hooks_set_support(
        (OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT));
    return OPAL_SUCCESS;
}


/*
 * Three ways to call munmap.  Prefered is to call __munmap, which
 * will exist if munmap is a weak symbol.  If not available next try
 * the syscall, and if that doesn't work, try looking in the dynamic
 * libc.
 */
#if USE_SOLARIS_LEGACY_MUNMAP_PROTOTYPE
/* We are compiling using S10 so use its munmap prototype */
int
munmap(caddr_t addr, size_t len)
#else
/* From S11 on forward munmap's addr is void * */
int
munmap(void *addr, size_t len)
#endif
{
#if !defined(HAVE___MUNMAP) && \
    !defined(HAVE_SYSCALL) && defined(HAVE_DLSYM)
    static int (*realmunmap)(void*, size_t);
#endif

    opal_mem_hooks_release_hook(addr, len, 0);

#if defined(HAVE___MUNMAP)
    return __munmap(addr, len);
#elif defined(HAVE_SYSCALL)
    return syscall(SYS_munmap, addr, len);
#elif defined(HAVE_DLSYM)
    if (NULL == realmunmap) {
        union { 
            int (*munmap_fp)(void*, size_t);
            void *munmap_p;
        } tmp;

        tmp.munmap_p = dlsym(RTLD_NEXT, "munmap");
        realmunmap = tmp.munmap_fp;
    }
    return realmunmap(addr, len);
#else
    #error "Can not determine how to call munmap"
#endif
}
