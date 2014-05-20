/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * Interface for manipulating how the RML receives contact information
 *
 * Interface for manipulating how the RML receives contact
 * information.  These functions are generally used during orte_init
 * and orte_finalize.
 */


#include "orte_config.h"
#include "orte/types.h"

#include "opal/dss/dss_types.h"

BEGIN_C_DECLS

/**
 * Create packed RML contact information for the given process names
 *
 * Create packed RML contact information for a given job.
 * The information is provided packed in an opal_buffer_t
 * structure.
 *
 * @param[in] jobid     Job whose contact information is needed
 * @param[out] data     Contact information packed in buffer for
 *                      \c name.
 *
 * @retval ORTE_SUCCESS Successfully found contact information
 * @retval ORTE_ERROR   Contact information could not be found or shared
 */
ORTE_DECLSPEC int orte_rml_base_get_contact_info(orte_jobid_t job, 
                                                 opal_buffer_t *data);


/**
 * Update the RML with contact information
 *
 * Update the RML with contact information provided from a call to
 * orte_rml_base_get_contact_info(), likely on another host.
 *
 * @param[in] data      Contact information in a packed buffer,
 *                      obtained by call to orte_rml_base_get_contact_info()
 *
 * @retval ORTE_SUCCESS Successfully updated contact information
 */
ORTE_DECLSPEC int orte_rml_base_update_contact_info(opal_buffer_t* data);

/**
 * Parse a contact information string
 *
 * Parse a contact infromation string, such as that returned by
 * orte_rml.get_contact_info().  Generally used to extract the peer
 * name from a contact information string.  It can also be used to
 * extract the contact URI strings, although this is slightly less
 * useful as the URIs may be RML componenent specific and not have
 * general meaning.
 *
 * @param[in] contact_info  Contact information string for peer
 * @param[out] peer         Peer name in contact_info
 * @param[out] uris         URI strings for peer.  May be NULL if
 *                          information is not needed
 *
 * @retval ORTE_SUCCESS     Information successfully extraced
 * @retval ORTE_ERR_BAD_PARAM The contact_info was not a valid string
 * @retval ORTE_ERROR       An unspecified error occurred
 */
ORTE_DECLSPEC int orte_rml_base_parse_uris(const char* contact_inf,
                                           orte_process_name_t* peer, 
                                           char*** uris);

END_C_DECLS
