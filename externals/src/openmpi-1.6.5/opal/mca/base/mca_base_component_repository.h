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

#ifndef MCA_BASE_COMPONENT_REPOSITORY_H
#define MCA_BASE_COMPONENT_REPOSITORY_H

#include "opal_config.h"

BEGIN_C_DECLS

    OPAL_DECLSPEC int mca_base_component_repository_init(void);

/* This file provide the external interface to our base component
 * module.  Most of the components that depend on it, will use the
 * retain_component() function to increase the reference count on a
 * particular component (as opposed to the retain() function, which is
 * internal to the opal/mca/base).  But it's convenient to have all
 * the functions exported from one header file rather than to separate
 * retain_component() and retain() into two separate header files
 * (i.e., have a separate header file just for retain()).
 *
 * Note that internal to opal/mca/base, <ltdl.h> will *always* be
 * included before this file, and <ltdl.h> is not included anywhere
 * else in the OMPI tree.  So checking for the LTDL_H preprocessor
 * macro is a good indicator as to whether this file is being included
 * from an opal/mca/base source file or not.  If we are, then we need
 * already have a real definition of lt_dlhandle.  If we are being
 * included from elsewhere, then <ltdl.h> will not previously have
 * been included, LTDL_H will not be defined, and we need a fake
 * definition of lt_dlhandle (or we'll get compile errors).  So just
 * typedef it to (void*).
 *
 * One more case that this handles is the --disable-dlopen case.  In
 * that case, even in opal/mca/base, we *won't* be including <ltdl.h>.
 * Hence, LTDL_H won't be defined, so we'll end up typedefing
 * lt_dlhandle to (void *).  "But why does that matter?" you ask, "If
 * we configure with --disable-dlopen, then lt_dlhandle shouldn't be
 * used anywhere."  Incorrect, Grasshopper.  A small number of places
 * (like the retain() function) are still prototyped, but have 99% of
 * their innards #if'ed out -- i.e., they just return
 * OPAL_ERR_NOT_SUPPORTED.  Why was it coded up this way?  I'm not
 * entirely sure -- there may be a reason (and that reason may just be
 * conservative coding), but I'm not really too inspired to look into
 * it any further at this point.  :-)
 */
#if !defined(LTDL_H)
    typedef void *lt_dlhandle;
#endif

    OPAL_DECLSPEC int mca_base_component_repository_retain(char *type, 
                              lt_dlhandle component_handle, 
                              const mca_base_component_t *component_struct);

    OPAL_DECLSPEC int mca_base_component_repository_retain_component(const char *type, 
                              const char *name);
    OPAL_DECLSPEC int mca_base_component_repository_link(const char *src_type, 
                              const char *src_name,
                              const char *depend_type,
                              const char *depend_name);
    OPAL_DECLSPEC void mca_base_component_repository_release(const mca_base_component_t *component);
    OPAL_DECLSPEC void mca_base_component_repository_finalize(void);
    
END_C_DECLS

#endif /* MCA_BASE_COMPONENT_REPOSITORY_H */
