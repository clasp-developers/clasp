/*
 * Copyright (c) 2010 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_MEMORY_LINUX_H
#define OPAL_MEMORY_LINUX_H

#include "opal_config.h"

#include "opal/mca/memory/memory.h"

BEGIN_C_DECLS

/* Component structure */

typedef struct opal_memory_linux_component_t {
    opal_memory_base_component_2_0_0_t super;

    /* Component data */
    int verbose_level;
    int enable_ummunotify;
    int enable_ptmalloc2;

#if MEMORY_LINUX_UMMUNOTIFY
    /* Ummunotify-specific data */
    int ummunotify_fd;
#endif

#if MEMORY_LINUX_PTMALLOC2
    /* Ptmalloc2-specific data */
    bool free_invoked;
    bool malloc_invoked;
    bool realloc_invoked;
    bool memalign_invoked;
    bool munmap_invoked;
#endif
} opal_memory_linux_component_t;

/* memory_linux_component.c */

extern opal_memory_linux_component_t mca_memory_linux_component;


#if MEMORY_LINUX_UMMUNOTIFY
/* memory_linux_ummunotify.c */
int opal_memory_linux_ummunotify_open(void);
int opal_memory_linux_ummunotify_close(void);
#endif /* MEMORY_LINUX_UMMUNOTIFY */

#if MEMORY_LINUX_PTMALLOC2
/* memory_linux_ptmalloc2.c */
int opal_memory_linux_ptmalloc2_open(void);
int opal_memory_linux_ptmalloc2_close(void);

/* memory_linux_munmap.c */
OPAL_DECLSPEC int opal_memory_linux_free_ptmalloc2_munmap(void *start, size_t length, int from_alloc);
OPAL_DECLSPEC int munmap(void* addr, size_t len);
#endif /* !MEMORY_LINUX_PTMALLOC2 */

END_C_DECLS

#endif
