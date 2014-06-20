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
 * Copyright (c) 2011 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#ifndef OPAL_MAFFINITY_BASE_H
#define OPAL_MAFFINITY_BASE_H

#include "opal_config.h"

#include "opal/mca/maffinity/maffinity.h"


/*
 * Global functions for MCA overall maffinity open and close
 */

BEGIN_C_DECLS

/**
 * Initialize the maffinity MCA framework
 *
 * @retval OPAL_SUCCESS Upon success
 * @retval OPAL_ERROR Upon failure
 *
 * This must be the first function invoked in the maffinity MCA
 * framework.  It initializes the maffinity MCA framework, finds
 * and opens maffinity components, etc.
 *
 * This function fills in the internal global variable
 * opal_maffinity_base_components_opened, which is a list of all
 * maffinity components that were successfully opened.  This
 * variable should \em only be used by other maffinity base
 * functions -- it is not considered a public interface member --
 * and is only mentioned here for completeness.
 */
OPAL_DECLSPEC int opal_maffinity_base_open(void);
    
/**
 * Select an available component.
 *
 * @return OPAL_SUCCESS Upon success.
 * @return OPAL_NOT_FOUND If no component can be selected.
 * @return OPAL_ERROR Upon other failure.
 *
 * This function invokes the selection process for maffinity
 * components, which works as follows:
 *
 * - If the \em maffinity MCA parameter is not specified, the
 *   selection set is all available maffinity components.
 * - If the \em maffinity MCA parameter is specified, the
 *   selection set is just that component.
 * - All components in the selection set are queried to see if
 *   they want to run.  All components that want to run are ranked
 *   by their priority and the highest priority component is
 *   selected.  All non-selected components have their "close"
 *   function invoked to let them know that they were not selected.
 * - The selected component will have its "init" function invoked to
 *   let it know that it was selected.
 *
 * If we fall through this entire process and no component is
 * selected, then return OPAL_NOT_FOUND (this is not a fatal
 * error).
 *
 * At the end of this process, we'll either have a single
 * component that is selected and initialized, or no component was
 * selected.  If no component was selected, subsequent invocation
 * of the maffinity wrapper functions will return an error.
 */
OPAL_DECLSPEC int opal_maffinity_base_select(void);

/**
 * Set memory affinity.
 *
 * @param segments Array describing segments and what process they
 * belong to
 * @param num_segments Length of the segments array
 * @param am_allocator True if this process created the shared
 * memory block
 *
 * @retval OPAL_SUCCESS upon success
 * @retval OPAL_NOT_FOUND if no maffinity components are available.
 * @retval OPAL_ERROR upon other error.
 *
 * Set the affinity of the memory segments described in the \em
 * segments array.
 *
 * If no maffinity components were available, or if the
 * opal_maffinity_base_select() was never invoked, OPAL_NOT_FOUND
 * is returned.
 */
OPAL_DECLSPEC int opal_maffinity_base_set(opal_maffinity_base_segment_t *segments, size_t num_segments);

OPAL_DECLSPEC int opal_maffinity_base_node_name_to_id(char *, int *);
OPAL_DECLSPEC int opal_maffinity_base_bind(opal_maffinity_base_segment_t *, size_t, int);

/**
 * Shut down the maffinity MCA framework.
 *
 * @retval OPAL_SUCCESS Always
 *
 * This function shuts down everything in the maffinity MCA
 * framework.
 *
 * It must be the last function invoked on the maffinity MCA
 * framework.
 */
OPAL_DECLSPEC int opal_maffinity_base_close(void);

/**
 * Indication of whether a component was successfully selected or
 * not
 */
extern bool opal_maffinity_base_selected;

/**
 * Global component struct for the selected component
 */
OPAL_DECLSPEC extern const opal_maffinity_base_component_2_0_0_t 
*opal_maffinity_base_component;
/**
 * Global module struct for the selected module
 */
OPAL_DECLSPEC extern const opal_maffinity_base_module_1_0_0_t 
*opal_maffinity_base_module;

/**
 * Indicator as to whether the list of opened maffinity components
 * is valid or not.
 */
extern bool opal_maffinity_base_components_opened_valid;
/**
 * List of all opened components; created when the maffinity
 * framework is initialized and destroyed when we reduce the list
 * to all available maffinity components.
 */
OPAL_DECLSPEC extern opal_list_t opal_maffinity_base_components_opened;

/**
 * Debugging output stream
 */
extern int opal_maffinity_base_output;

/**
 * Flag to indicate whether or not maffinity was setup
 */
OPAL_DECLSPEC extern bool opal_maffinity_setup;

END_C_DECLS
    
#endif /* OPAL_MAFFINITY_BASE_H */
