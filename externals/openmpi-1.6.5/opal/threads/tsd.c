/* 
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "opal_config.h"

#include <stdio.h>
# if HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <errno.h>

#include "opal/threads/tsd.h"

#if !OPAL_HAVE_POSIX_THREADS && !OPAL_HAVE_SOLARIS_THREADS && !defined(__WINDOWS__)

#define TSD_ENTRIES 32

struct tsd_entry_t {
    bool used;
    void *value;
    opal_tsd_destructor_t destructor;
};
typedef struct tsd_entry_t tsd_entry_t;

static tsd_entry_t entries[TSD_ENTRIES];
static bool atexit_registered = false;

static void
run_destructors(void)
{
    int i;

    for (i = 0; i < TSD_ENTRIES ; ++i) {
        opal_tsd_destructor_t destructor;
        void *value;

        if (entries[i].used) {
            destructor = entries[i].destructor;
            value = entries[i].value;
            
            entries[i].used = false;
            entries[i].destructor = NULL;
            entries[i].value = NULL;
            
            destructor(value);
        }
    }
}

int
opal_tsd_key_create(opal_tsd_key_t *key, 
                    opal_tsd_destructor_t destructor)
{
    int i;

    if (!atexit_registered) {
        atexit_registered = true;
        if (0 != atexit(run_destructors)) {
            return OPAL_ERR_TEMP_OUT_OF_RESOURCE;
        }
    }

    for (i = 0 ; i < TSD_ENTRIES ; ++i) {
        if (entries[i].used == false) {
            entries[i].used = true;
            entries[i].value = NULL;
            entries[i].destructor = destructor;
            *key = i;
            break;
        }
    }
    if (i == TSD_ENTRIES) return ENOMEM;

    return OPAL_SUCCESS;
}


int
opal_tsd_key_delete(opal_tsd_key_t key)
{
    if (!entries[key].used) return OPAL_ERR_BAD_PARAM;

    entries[key].used = false;
    entries[key].value = NULL;
    entries[key].destructor = NULL;

    return OPAL_SUCCESS;
}


int
opal_tsd_setspecific(opal_tsd_key_t key, void *value)
{
    if (!entries[key].used) return OPAL_ERR_BAD_PARAM;
    entries[key].value = value;
    return OPAL_SUCCESS;
}


int
opal_tsd_getspecific(opal_tsd_key_t key, void **valuep)
{
    if (!entries[key].used) return OPAL_ERR_BAD_PARAM;
    *valuep = entries[key].value;
    return OPAL_SUCCESS;
}

#endif
