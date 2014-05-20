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
 *
 */

#ifndef OPAL_TIMER_BASE_H
#define OPAL_TIMER_BASE_H

#include "opal_config.h"

#include "opal/mca/timer/timer.h"


/*
 * Global functions for MCA overall timer open and close
 */

BEGIN_C_DECLS

    /**
     * Initialize the timer MCA framework
     *
     * @retval OPAL_SUCCESS Upon success
     * @retval OPAL_ERROR Upon failure
     *
     * This must be the first function invoked in the timer MCA
     * framework.  It initializes the timer MCA framework, finds
     * and opens timer components, etc.
     *
     * This function is invoked during opal_init() and during the
     * initialization of the special case of the laminfo command.
     * 
     * This function fills in the internal global variable
     * opal_timer_base_components_opened, which is a list of all
     * timer components that were successfully opened.  This
     * variable should \em only be used by other timer base
     * functions -- it is not considered a public interface member --
     * and is only mentioned here for completeness.
     */
    OPAL_DECLSPEC int opal_timer_base_open(void);
    

    /**
     * Shut down the timer MCA framework.
     *
     * @retval OPAL_SUCCESS Always
     *
     * This function shuts down everything in the timer MCA
     * framework, and is called during opal_finalize() and the
     * special case of the laminfo command.
     *
     * It must be the last function invoked on the timer MCA framework.
     */
    OPAL_DECLSPEC int opal_timer_base_close(void);

    OPAL_DECLSPEC extern opal_list_t opal_timer_base_components_opened;
    
END_C_DECLS

/* include implementation to call */
#include MCA_timer_IMPLEMENTATION_HEADER

#endif /* OPAL_BASE_TIMER_H */
