/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
 * RSH FILEM component
 *
 */

#ifndef MCA_FILEM_RSH_EXPORT_H
#define MCA_FILEM_RSH_EXPORT_H

#include "orte_config.h"

#include "opal/mca/mca.h"
#include "orte/mca/filem/filem.h"

BEGIN_C_DECLS

#define ORTE_FILEM_RSH_ASK   0
#define ORTE_FILEM_RSH_ALLOW 1
#define ORTE_FILEM_RSH_DONE  2

    /*
     * Local Component structures
     */
    struct orte_filem_rsh_component_t {
        /** Base FILEM component */
        orte_filem_base_component_t super;

        /** RSH cp command: rsh = rcp, ssh = scp */
        char * cp_command;

        /** Unix cp command */
        char * cp_local_command;

        /** SSH remote login command */	
        char * remote_sh_command;
    };
    typedef struct orte_filem_rsh_component_t orte_filem_rsh_component_t;
    ORTE_MODULE_DECLSPEC extern orte_filem_rsh_component_t mca_filem_rsh_component;

    extern int orte_filem_rsh_max_incomming;
    extern int orte_filem_rsh_max_outgoing;

    int orte_filem_rsh_component_query(mca_base_module_t **module, int *priority);

    /*
     * Module functions
     */
    int orte_filem_rsh_module_init(void);
    int orte_filem_rsh_module_finalize(void);

    int orte_filem_rsh_put(orte_filem_base_request_t *request);
    int orte_filem_rsh_put_nb(orte_filem_base_request_t *request);

    int orte_filem_rsh_get(orte_filem_base_request_t *request);
    int orte_filem_rsh_get_nb(orte_filem_base_request_t *request);

    int orte_filem_rsh_rm( orte_filem_base_request_t *request);
    int orte_filem_rsh_rm_nb( orte_filem_base_request_t *request);

    int orte_filem_rsh_wait( orte_filem_base_request_t *request);
    int orte_filem_rsh_wait_all( opal_list_t *request_list);

END_C_DECLS

#endif /* MCA_FILEM_RSH_EXPORT_H */
