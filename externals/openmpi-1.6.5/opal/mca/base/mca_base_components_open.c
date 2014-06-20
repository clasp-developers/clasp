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
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "opal/class/opal_list.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "opal/constants.h"

struct component_name_t {
  opal_list_item_t super;

  char mn_name[MCA_BASE_MAX_COMPONENT_NAME_LEN];
};
typedef struct component_name_t component_name_t;

/*
 * Dummy structure for casting for open_only logic
 */
struct mca_base_open_only_dummy_component_t {
    /** MCA base component */
    mca_base_component_t version;
    /** MCA base data */
    mca_base_component_data_t data;
};
typedef struct mca_base_open_only_dummy_component_t mca_base_open_only_dummy_component_t;

/*
 * Local variables
 */
static bool show_errors = false;
static const char negate = '^';


/*
 * Local functions
 */
static int parse_requested(int mca_param, bool *include_mode,
                           char ***requested_component_names);
static int open_components(const char *type_name, int output_id, 
                           opal_list_t *src, opal_list_t *dest);

/**
 * Function for finding and opening either all MCA components, or the
 * one that was specifically requested via a MCA parameter.
 */
int mca_base_components_open(const char *type_name, int output_id,
                             const mca_base_component_t **static_components,
                             opal_list_t *components_available,
                             bool open_dso_components)
{
    int ret, param;
    opal_list_item_t *item;
    opal_list_t components_found;
    char **requested_component_names;
    int param_verbose = -1;
    int param_type = -1;
    int verbose_level;
    char *str;
    bool include_mode;
#if (OPAL_ENABLE_FT == 1) && (OPAL_ENABLE_FT_CR == 1)
    opal_list_item_t *next;
    uint32_t open_only_flags = MCA_BASE_METADATA_PARAM_NONE;
    const mca_base_component_t *component;
#endif
 
    /* Register MCA parameters */
    /* Check to see if it exists first */
    if( 0 > (param_type = mca_base_param_find(type_name, NULL, NULL) ) ) {
        asprintf(&str, "Default selection set of components for the %s framework (<none>"
                 " means use all components that can be found)", type_name);
        param_type = 
            mca_base_param_reg_string_name(type_name, NULL, str, 
                                           false, false, NULL, NULL);
        free(str);
    }

    param = mca_base_param_find("mca", NULL, "component_show_load_errors");
    mca_base_param_lookup_int(param, &ret);
    show_errors = OPAL_INT_TO_BOOL(ret);

    /* Setup verbosity for this MCA type */
    asprintf(&str, "Verbosity level for the %s framework (0 = no verbosity)", type_name);
    param_verbose = 
        mca_base_param_reg_int_name(type_name, "base_verbose",
                                    str, false, false, 0, NULL);
    free(str);
    mca_base_param_lookup_int(param_verbose, &verbose_level);
    if (output_id != 0) {
        opal_output_set_verbosity(output_id, verbose_level);
    }
    opal_output_verbose(10, output_id, 
                        "mca: base: components_open: Looking for %s components",
                        type_name);

    ret = parse_requested(param_type, &include_mode, &requested_component_names);
    if( OPAL_SUCCESS != ret ) {
        return ret;
    }

    /* Find and load requested components */
    if (OPAL_SUCCESS != (ret = 
        mca_base_component_find(NULL, type_name, static_components,
                                requested_component_names, include_mode, 
                                &components_found, open_dso_components)) ) {
        return ret;
    }

#if (OPAL_ENABLE_FT == 1) && (OPAL_ENABLE_FT_CR == 1)
    {
        int param_id = -1;
        int param_val = 0;
        /*
         * Extract supported mca parameters for selection contraints
         * Supported Options:
         *   - mca_base_component_distill_checkpoint_ready = Checkpoint Ready
         */
        param_id = mca_base_param_reg_int_name("mca", "base_component_distill_checkpoint_ready",
                                               "Distill only those components that are Checkpoint Ready", 
                                               false, false,
                                               0, &param_val);
        if( 0 != param_val ) { /* Select Checkpoint Ready */
            open_only_flags |= MCA_BASE_METADATA_PARAM_CHECKPOINT;
        }
    }
#endif  /* (OPAL_ENABLE_FT == 1) && (OPAL_ENABLE_FT_CR == 1) */

    /*
     * Pre-process the list with parameter constraints
     * e.g., If requested to select only CR enabled components
     *       then only make available those components.
     *
     * JJH Note: Currently checkpoint/restart is the only user of this
     *           functionality. If other component constraint options are
     *           added, then this logic can be used for all contraint
     *           options.
     */
#if (OPAL_ENABLE_FT == 1) && (OPAL_ENABLE_FT_CR == 1)
    if( !(MCA_BASE_METADATA_PARAM_NONE & open_only_flags) ) {
        if( MCA_BASE_METADATA_PARAM_CHECKPOINT & open_only_flags) {
            opal_output_verbose(10, output_id,
                                "mca: base: components_open: "
                                "including only %s components that are checkpoint enabled", type_name);
        }

        /*
         * Check all the components to make sure they adhere to the user
         * expressed requirements.
         */
        for(item  = opal_list_get_first(&components_found);
            item != opal_list_get_end(&components_found);
            item  = next ) {
            mca_base_open_only_dummy_component_t *dummy;
            mca_base_component_list_item_t *cli = (mca_base_component_list_item_t *) item;
            dummy = (mca_base_open_only_dummy_component_t*) cli->cli_component;
            component = cli->cli_component;
      
            next = opal_list_get_next(item);
      
            /*
             * If the user asked for a checkpoint enabled run
             * then only load checkpoint enabled components.
             */
            if( MCA_BASE_METADATA_PARAM_CHECKPOINT & open_only_flags) {
                if( MCA_BASE_METADATA_PARAM_CHECKPOINT & dummy->data.param_field) {
                    opal_output_verbose(10, output_id,
                                        "mca: base: components_open: "
                                        "(%s) Component %s is Checkpointable",
                                        type_name,
                                        dummy->version.mca_component_name);
                }
                else {
                    opal_output_verbose(10, output_id,
                                        "mca: base: components_open: "
                                        "(%s) Component %s is *NOT* Checkpointable - Disabled",
                                        type_name,
                                        dummy->version.mca_component_name);
                    opal_list_remove_item(&components_found, item);
                    /* Make sure to release the component since we are not
                     * opening it */
                    mca_base_component_repository_release(component);
                }
            }
        }
    }
#endif  /* (OPAL_ENABLE_FT == 1) && (OPAL_ENABLE_FT_CR == 1) */

    /* Open all remaining components */
    ret = open_components(type_name, output_id,
                          &components_found, components_available);

    /* Free resources */
    for (item = opal_list_remove_first(&components_found); NULL != item;
         item = opal_list_remove_first(&components_found)) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&components_found);

    if (NULL != requested_component_names) {
        opal_argv_free(requested_component_names);
    }

    /* All done */
    return ret;
}

int mca_base_is_component_required(opal_list_t *components_available,
                                   mca_base_component_t *component,
                                   bool exclusive,
                                   bool *is_required)
{
    opal_list_item_t *item = NULL;
    mca_base_component_list_item_t *cli = NULL;
    mca_base_component_t *comp = NULL;

    /* Sanity check */
    if( NULL == components_available ||
        NULL == component) {
        return OPAL_ERR_BAD_PARAM;
    }

    *is_required = false;

    /*
     * Look through the components available for opening
     */
    if( exclusive ) {
        /* Must be the -only- component in the list */
        if( 1 == opal_list_get_size(components_available) ) {
            item  = opal_list_get_first(components_available);
            cli   = (mca_base_component_list_item_t *) item;
            comp  = (mca_base_component_t *) cli->cli_component;

            if( 0 == strncmp(comp->mca_component_name,
                             component->mca_component_name,
                             strlen(component->mca_component_name)) ) {
                *is_required = true;
                return OPAL_SUCCESS;
            }
        }
    }
    else {
        /* Must be one of the components in the list */
        for (item  = opal_list_get_first(components_available);
             item != opal_list_get_end(components_available);
             item  = opal_list_get_next(item) ) {
            cli  = (mca_base_component_list_item_t *) item;
            comp = (mca_base_component_t *) cli->cli_component;

            if( 0 == strncmp(comp->mca_component_name,
                             component->mca_component_name,
                             strlen(component->mca_component_name)) ) {
                *is_required = true;
                return OPAL_SUCCESS;
            }
        }
    }

    return OPAL_SUCCESS;
}


static int parse_requested(int mca_param, bool *include_mode,
                           char ***requested_component_names)
{
  int i;
  char *requested, *requested_orig;

  *requested_component_names = NULL;
  *include_mode = true;

  /* See if the user requested anything */

  if (OPAL_ERROR == mca_base_param_lookup_string(mca_param, &requested)) {
    return OPAL_ERROR;
  }
  if (NULL == requested || 0 == strlen(requested)) {
    return OPAL_SUCCESS;
  }
  requested_orig = requested;

  /* Are we including or excluding?  We only allow the negate
     character to be the *first* character of the value (but be nice
     and allow any number of negate characters in the beginning). */

  while (negate == requested[0] && '\0' != requested[0]) {
      *include_mode = false;
      ++requested;
  }

  /* Double check to ensure that the user did not specify the negate
     character anywhere else in the value. */

  i = 0;
  while ('\0' != requested[i]) {
      if (negate == requested[i]) {
          opal_show_help("help-mca-base.txt", 
                         "framework-param:too-many-negates",
                         true, requested_orig);
          free(requested_orig);
          return OPAL_ERROR;
      }
      ++i;
  }

  /* Split up the value into individual component names */

  *requested_component_names = opal_argv_split(requested, ',');

  /* All done */

  free(requested_orig);
  return OPAL_SUCCESS;
}


/*
 * Traverse the entire list of found components (a list of
 * mca_base_component_t instances).  If the requested_component_names
 * array is empty, or the name of each component in the list of found
 * components is in the requested_components_array, try to open it.
 * If it opens, add it to the components_available list.
 */
static int open_components(const char *type_name, int output_id, 
                           opal_list_t *src, opal_list_t *dest)
{
    int ret;
    opal_list_item_t *item;
    const mca_base_component_t *component;
    mca_base_component_list_item_t *cli;
    bool called_open;
    bool opened, registered;
    
    /* Announce */
    
    opal_output_verbose(10, output_id,
                        "mca: base: components_open: opening %s components",
                        type_name);
    
    /* Traverse the list of found components */
    
    OBJ_CONSTRUCT(dest, opal_list_t);
    for (item = opal_list_get_first(src);
         opal_list_get_end(src) != item;
         item = opal_list_get_next(item)) {
        cli = (mca_base_component_list_item_t *) item;
        component = cli->cli_component;
        
        registered = opened = called_open = false;
        opal_output_verbose(10, output_id, 
                            "mca: base: components_open: found loaded component %s",
                            component->mca_component_name);

        /* Call the component's MCA parameter registration function */
        if (NULL == component->mca_register_component_params) {
            registered = true;
            opal_output_verbose(10, output_id, 
                                "mca: base: components_open: "
                                "component %s has no register function",
                                component->mca_component_name);
        } else {
            ret = component->mca_register_component_params();
            if (MCA_SUCCESS == ret) {
                registered = true;
                opal_output_verbose(10, output_id, 
                                    "mca: base: components_open: "
                                    "component %s register function successful",
                                    component->mca_component_name);
            } else if (OPAL_ERR_NOT_AVAILABLE != ret) {
                /* If the component returns OPAL_ERR_NOT_AVAILABLE,
                   it's a cue to "silently ignore me" -- it's not a
                   failure, it's just a way for the component to say
                   "nope!".  

                   Otherwise, however, display an error.  We may end
                   up displaying this twice, but it may go to separate
                   streams.  So better to be redundant than to not
                   display the error in the stream where it was
                   expected. */
                
                if (show_errors) {
                    opal_output(0, "mca: base: components_open: "
                                "component %s / %s register function failed",
                                component->mca_type_name,
                                component->mca_component_name);
                }
                opal_output_verbose(10, output_id, 
                                    "mca: base: components_open: "
                                    "component %s register function failed",
                                    component->mca_component_name);
            }
        }

        if (NULL == component->mca_open_component) {
            opened = true; 
            opal_output_verbose(10, output_id, 
                                "mca: base: components_open: "
                                "component %s has no open function",
                                component->mca_component_name);
        } else {
            called_open = true;
            ret = component->mca_open_component();
            if (MCA_SUCCESS == ret) {
                opened = true;
                opal_output_verbose(10, output_id, 
                                    "mca: base: components_open: "
                                    "component %s open function successful",
                                    component->mca_component_name);
            } else if (OPAL_ERR_NOT_AVAILABLE != ret) {
                /* If the component returns OPAL_ERR_NOT_AVAILABLE,
                   it's a cue to "silently ignore me" -- it's not a
                   failure, it's just a way for the component to say
                   "nope!".  

                   Otherwise, however, display an error.  We may end
                   up displaying this twice, but it may go to separate
                   streams.  So better to be redundant than to not
                   display the error in the stream where it was
                   expected. */
                
                if (show_errors) {
                    opal_output(0, "mca: base: components_open: "
                                "component %s / %s open function failed",
                                component->mca_type_name,
                                component->mca_component_name);
                }
                opal_output_verbose(10, output_id, 
                                    "mca: base: components_open: "
                                    "component %s open function failed",
                                    component->mca_component_name);
            }
        }
        
        /* If it didn't open, close it out and get rid of it */
        
        if (!opened) {
            char *name;
            if (called_open) {
                if (NULL != component->mca_close_component) {
                    component->mca_close_component();
                }
                opal_output_verbose(10, output_id, 
                                    "mca: base: components_open: component %s closed",
                                    component->mca_component_name);
                called_open = false;
            }
            name = strdup(component->mca_component_name);
            mca_base_component_repository_release(component);
            opal_output_verbose(10, output_id, 
                                "mca: base: components_open: component %s unloaded", 
                                name);
            free(name);
        }
        
        /* If it did open, register its "priority" MCA parameter (if
           it doesn't already have one) and save it in the
           opened_components list */
        
        else {
            if (OPAL_ERROR == mca_base_param_find(type_name, 
                                                  component->mca_component_name,
                                                  "priority")) {
                mca_base_param_register_int(type_name,
                                            component->mca_component_name,
                                            "priority", NULL, 0);
            }
            
            cli = OBJ_NEW(mca_base_component_list_item_t);
            if (NULL == cli) {
                return OPAL_ERR_OUT_OF_RESOURCE;
            }
            cli->cli_component = component;
            opal_list_append(dest, (opal_list_item_t *) cli);
        }
    }
    
    /* All done */
    
    return OPAL_SUCCESS;
}
