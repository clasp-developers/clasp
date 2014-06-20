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
 * Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <string.h>
#include <stdio.h>

#include "opal/constants.h"
#include "opal/mca/mca.h"
#include "opal/libltdl/ltdl.h"

#include "components.h"


/*
 * Local functions
 */
static bool try_dlopen(const char *dir, const char *fw, const char *comp,
                       test_component_handle_t *comp_handle,
                       test_component_sym_t *comp_symbol);
static char *dir_concat(const char *a, const char *b);


int test_component_open(const char *framework, const char *component,
                        test_component_handle_t *comp_handle,
                        mca_base_component_t **mca)
{
    test_component_sym_t sym;

    if (NULL == framework || NULL == component || NULL == comp_handle ||
        NULL == mca) {
        return OPAL_ERR_BAD_PARAM;
    }
    comp_handle->tch_handle = NULL;
    sym.tcs_variable = NULL;

    /* It's ok to call lt_dlinit() multiple times; it ref counts for
       lt_dlexit() */

    lt_dlinit();

    /* Try to open */

    if (try_dlopen(".", framework, component, comp_handle, &sym) ||
        try_dlopen(dir_concat(BUILDDIR, "src/mca"), framework, 
                   component, comp_handle, &sym) ||
        try_dlopen(dir_concat(SRCDIR, "src/mca"), framework, 
                   component, comp_handle, &sym)) {

        /* Ok, dlopen'ed it.  Now call the component's open
           function. */

        *mca = (mca_base_component_t*) sym.tcs_variable;
        if (NULL == (*mca)->mca_open_component ||
            (NULL != (*mca)->mca_open_component &&
             OPAL_SUCCESS == (*mca)->mca_open_component())) {
            return OPAL_SUCCESS;
        }

        /* Badness occurred, so dlclose the component */

        test_component_close(comp_handle);
    }

    /* Didn't find it / unable to open it */

    return OPAL_ERROR;
}


int test_component_find_symbol(const char *name, 
                               test_component_handle_t *handle,
                               test_component_sym_t *sym)
{
    /* Use a union to avoid pesky compilers that complain [rightfully,
       unfortunately] about converting (void*) to a function pointer
       -- need to wait until the ltdl library has a lt_dlsymfunc()
       function for a real fix... */

    union {
        void *vvalue;
        void (*fvalue)(void);
    } value;

    if (NULL == handle || NULL == sym) {
        return OPAL_ERR_BAD_PARAM;
    }

    value.vvalue = lt_dlsym(handle->tch_handle, name);
    sym->tcs_function = value.fvalue;
    return OPAL_SUCCESS;
}


int test_component_close(test_component_handle_t *handle)
{
    /* Note that lt_dlinit() refcounts, so ld_dlexit() only actually
       shuts down if we've called it as many times as we've called
       lt_dlinit() */

    if (NULL != handle && NULL != handle->tch_handle) {
        lt_dlclose(handle->tch_handle);
        lt_dlexit();
        handle->tch_handle = NULL;
    }

    return OPAL_SUCCESS;
}


static bool try_dlopen(const char *dir, const char *fw, const char *comp,
                       test_component_handle_t *comp_handle,
                       test_component_sym_t *comp_symbol)
{
    char component_name[BUFSIZ];
    char file_name[BUFSIZ];
#ifdef __WINDOWS__
    char dirsep = '\\';
#else
    char dirsep = '/';
#endif

    component_name[BUFSIZ - 1] = '\0';
    snprintf(component_name, BUFSIZ - 1, "mca_%s_%s_component", fw, comp);
    if ('\0' != component_name[BUFSIZ - 1]) {
        return false;
    }

    /* First, look for the component symbol statically */

    comp_handle->tch_handle = lt_dlopen(NULL);
    if (NULL != comp_handle->tch_handle) {
        comp_symbol->tcs_variable = 
            lt_dlsym(comp_handle->tch_handle, component_name);
        if (NULL != comp_symbol->tcs_variable) {
            return true;
        }
        lt_dlclose(comp_handle->tch_handle);
    }

    /* Not in self -- look for a real component */

    file_name[BUFSIZ - 1] = '\0';
    snprintf(file_name, BUFSIZ - 1, "%s%c%s%c%s%cmca_%s_%s", dir, dirsep,
             fw, dirsep, comp, dirsep, fw, comp);
    if ('\0' != file_name[BUFSIZ - 1]) {
        return false;
    }
    comp_handle->tch_handle = lt_dlopenext(file_name);
    if (NULL != comp_handle->tch_handle) {
        comp_symbol->tcs_variable = 
            lt_dlsym(comp_handle->tch_handle, component_name);
        if (NULL != comp_symbol->tcs_variable) {
            return true;
        }
        lt_dlclose(comp_handle->tch_handle);
    }

    /* Nope, didn't find it */

    return false;
}


static char *dir_concat(const char *a, const char *b)
{
    static char name[BUFSIZ];
    name[0] = '\0';
    if (NULL != a) {
        strcat(name, a);
#ifdef __WINDOWS__
        strcat(name, "\\");
#else
        strcat(name, "/");
#endif        
    }
    if (NULL != b) {
        strcat(name, b);
    }

    return name;
}
