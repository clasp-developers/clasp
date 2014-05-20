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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/** 
 * @file 
 *
 * Checkpoint/Restart Functionality for the OMPI layer
 */

#ifndef OMPI_CR_H
#define OMPI_CR_H

#include "ompi_config.h"

BEGIN_C_DECLS

    /*
     * Initialization called in ompi_init()
     */
    OMPI_DECLSPEC int ompi_cr_init(void);

    /*
     * Finalization called in ompi_finalize()
     */
    OMPI_DECLSPEC int ompi_cr_finalize(void);

    /*
     * Interlayer Coodination Callback
     */
    OMPI_DECLSPEC int ompi_cr_coord(int state);

    /*
     * A general output handle to use for FT related messages
     */
    OMPI_DECLSPEC extern int ompi_cr_output;

    /*
     * If one of the BTLs that shutdown require a full, clean rebuild of the
     * point-to-point stack on 'continue' as well as 'restart'.
     */
    OMPI_DECLSPEC extern bool ompi_cr_continue_like_restart;

END_C_DECLS

#endif /* OMPI_CR_H */
