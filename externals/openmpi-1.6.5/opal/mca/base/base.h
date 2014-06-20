/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BASE_H
#define MCA_BASE_H

#include "opal_config.h"

#include "opal/class/opal_object.h"
#include "opal/class/opal_list.h"

/*
 * These units are large enough to warrant their own .h files
 */
#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/cmd_line.h"

BEGIN_C_DECLS

/*
 * Structure for making plain lists of components
 */
struct mca_base_component_list_item_t {
    opal_list_item_t super;
    const mca_base_component_t *cli_component;
};
typedef struct mca_base_component_list_item_t mca_base_component_list_item_t;
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(mca_base_component_list_item_t);

/*
 * Structure for making priority lists of components
 */
struct mca_base_component_priority_list_item_t {
    mca_base_component_list_item_t super;
    int cpli_priority;
};
typedef struct mca_base_component_priority_list_item_t 
    mca_base_component_priority_list_item_t;

OPAL_DECLSPEC OBJ_CLASS_DECLARATION(mca_base_component_priority_list_item_t);

/*
 * Public variables
 */
OPAL_DECLSPEC extern int mca_base_param_component_path;
OPAL_DECLSPEC extern char *mca_base_system_default_path;
OPAL_DECLSPEC extern char *mca_base_user_default_path;

/*
 * Public functions
 */

/**
 * First function called in the MCA.
 *
 * @return OPAL_SUCCESS Upon success
 * @return OPAL_ERROR Upon failure
 * 
 * This function starts up the entire MCA.  It initializes a bunch
 * of built-in MCA parameters, and initialized the MCA component
 * repository.
 *
 * It must be the first MCA function invoked.  It is normally
 * invoked during ompi_mpi_init() and specifically invoked in the
 * special case of the laminfo command.
 */
OPAL_DECLSPEC int mca_base_open(void);

/**
 * Last function called in the MCA
 *
 * @return OPAL_SUCCESS Upon success
 * @return OPAL_ERROR Upon failure
 *
 * This function closes down the entire MCA.  It clears all MCA
 * parameters and closes down the MCA component respository.  
 *
 * It must be the last MCA function invoked.  It is normally invoked
 * during ompi_mpi_finalize() and specifically invoked during the
 * special case of the laminfo command.
 */
OPAL_DECLSPEC int mca_base_close(void);

/**
 * A generic select function
 *
 */
OPAL_DECLSPEC int mca_base_select(const char *type_name, int output_id,
                                  opal_list_t *components_available,
                                  mca_base_module_t **best_module,
                                  mca_base_component_t **best_component);

/**
 * A function for component query functions to discover if they have
 * been explicitly required to or requested to be selected.
 *
 * exclusive: If the specified component is the only component that is
 *            available for selection.
 *
 */
OPAL_DECLSPEC int mca_base_is_component_required(opal_list_t *components_available,
                                                 mca_base_component_t *component,
                                                 bool exclusive,
                                                 bool *is_required);

/* mca_base_cmd_line.c */

OPAL_DECLSPEC int mca_base_cmd_line_setup(opal_cmd_line_t *cmd);
OPAL_DECLSPEC int mca_base_cmd_line_process_args(opal_cmd_line_t *cmd,
                                                 char ***app_env,
                                                 char ***global_env);

/* mca_base_component_compare.c */

OPAL_DECLSPEC int mca_base_component_compare_priority(mca_base_component_priority_list_item_t *a,
                                                      mca_base_component_priority_list_item_t *b);
OPAL_DECLSPEC int mca_base_component_compare(const mca_base_component_t *a,
                                             const mca_base_component_t *b);
OPAL_DECLSPEC int mca_base_component_compatible(const mca_base_component_t *a,
                                                const mca_base_component_t *b);
OPAL_DECLSPEC char * mca_base_component_to_string(const mca_base_component_t *a);

/* mca_base_component_find.c */

OPAL_DECLSPEC int mca_base_component_find(const char *directory, const char *type,
                                          const mca_base_component_t *static_components[],
                                          char **requested_component_names,
                                          bool include_mode,
                                          opal_list_t *found_components,
                                          bool open_dso_components);

/* Safely release some memory allocated by mca_base_component_find()
   (i.e., is safe to call even if you never called
   mca_base_component_find()). */
OPAL_DECLSPEC int mca_base_component_find_finalize(void);

/* mca_base_components_open.c */

OPAL_DECLSPEC int mca_base_components_open(const char *type_name, int output_id,
                                           const mca_base_component_t **static_components,
                                           opal_list_t *components_available,
                                           bool open_dso_components);

/* mca_base_components_close.c */

OPAL_DECLSPEC int mca_base_components_close(int output_id, opal_list_t *components_available, 
                                            const mca_base_component_t *skip,
                                            bool close_stream);

#if 0
/* JMS Not implemented yet */
int mca_base_init_callback(mca_base_init_cb_t func);
int mca_base_init_callbacks_invoke(void);
int mca_base_component_select(int requested);

int mca_base_param_associate(int index, int keyval);
int mca_base_param_lookup_int(int index, MPI_Comm comm);
char *mca_base_param_lookup_string(int index, MPI_Comm comm);
#endif

END_C_DECLS

#endif /* MCA_BASE_H */
