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
/** @file */

#ifndef OMPI_SUPPORT_COMPONENTS_H
#define OMPI_SUPPORT_COMPONENTS_H

#include "opal/libltdl/ltdl.h"
#include "opal/mca/mca.h"

BEGIN_C_DECLS

    /**
     * Generic function pointer, suitable for casting
     */
    typedef void (*test_component_fn_t)(void);

    /**
     * Type to hold both a variable and a function pointer, especially
     * for platforms where they are different sizes.
     */
    union test_component_sym_t {
        void *tcs_variable;
        test_component_fn_t tcs_function;
    };
    /** Convenience typedef */
    typedef union test_component_sym_t test_component_sym_t;

    /**
     * Type for masking the real/underlying type of opened components
     */
    struct test_component_handle_t {
        lt_dlhandle tch_handle;
    };
    /**
     * Convenience typedef
     */
    typedef struct test_component_handle_t test_component_handle_t;

    /**
     * Open a specific MCA component in the build tree
     *
     * void *test_component_open()
     */
    int test_component_open(const char *framework, const char *component,
                            test_component_handle_t *comp_handle,
                            mca_base_component_t **mca);

    /**
     * Find a symbol in an MCA component
     *
     * ompi_test_component_sym_t test_component_find_symbol()
     */
    int test_component_find_symbol(const char *name, 
                                   test_component_handle_t *handle,
                                   test_component_sym_t *sym);

    /**
     * Close a specific MCA component
     *
     * int test_component_close()
     */
    int test_component_close(test_component_handle_t *handle);


END_C_DECLS

#endif /* OMPI_SUPPORT_COMPONENTS_H */
