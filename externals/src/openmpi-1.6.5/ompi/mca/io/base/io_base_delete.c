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
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "mpi.h"
#include "opal/class/opal_list.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "ompi/mca/io/io.h"
#include "ompi/mca/io/base/base.h"

/*
 * Local types
 */
struct avail_io_t {
    opal_list_item_t super;

    mca_io_base_version_t ai_version;

    int ai_priority;
    mca_io_base_components_t ai_component;
    struct mca_io_base_delete_t *ai_private_data;
};
typedef struct avail_io_t avail_io_t;

/*
 * Local functions
 */
static opal_list_t *check_components(opal_list_t *components, 
                                     char *filename, struct ompi_info_t *info,
                                     char **names, int num_names);
static avail_io_t *check_one_component(const mca_base_component_t *component,
                                       char *filename, struct ompi_info_t *info);

static avail_io_t *query(const mca_base_component_t *component,
                         char *filename, struct ompi_info_t *info);
static avail_io_t *query_2_0_0(const mca_io_base_component_2_0_0_t *io_component, 
                               char *filename, struct ompi_info_t *info);

static void unquery(avail_io_t *avail, char *filename, struct ompi_info_t *info);

static int delete_file(avail_io_t *avail, char *filename, struct ompi_info_t *info);


/*
 * Stuff for the OBJ interface
 */
static OBJ_CLASS_INSTANCE(avail_io_t, opal_list_item_t, NULL, NULL);


/*
 */
int mca_io_base_delete(char *filename, struct ompi_info_t *info)
{
    int err, num_names;
    char *names, **name_array;
    opal_list_t *selectable;
    opal_list_item_t *item;
    avail_io_t *avail, selected;

    /* Announce */

    opal_output_verbose(10, mca_io_base_output,
                        "io:base:delete: deleting file: %s", 
                        filename);
  
    /* See if a set of component was requested by the MCA parameter.
       Don't check for error. */

    names = NULL;
    mca_base_param_lookup_string(mca_io_base_param, &names);

    /* Compute the intersection of all of my available components with
       the components from all the other processes in this file */

    /* JMS CONTINUE HERE */

    /* See if there were any listed in the MCA parameter; parse them
       and check them all */

    err = OMPI_ERROR;
    if (NULL != names && 0 < strlen(names)) {
        name_array = opal_argv_split(names, ',');
        num_names = opal_argv_count(name_array);
        
        opal_output_verbose(10, mca_io_base_output, 
                            "io:base:delete: Checking specific modules: %s",
                            names);
        selectable = check_components(&mca_io_base_components_available, 
                                      filename, info, name_array, num_names);
        opal_argv_free(name_array);
    }

    /* Nope -- a specific [set of] component[s] was not requested.  Go
       check them all. */
  
    else {
        opal_output_verbose(10, mca_io_base_output, 
                            "io:base:delete: Checking all available modules");
        selectable = check_components(&mca_io_base_components_available, 
                                      filename, info, NULL, 0);
    }

    /* Upon return from the above, the modules list will contain the
       list of modules that returned (priority >= 0).  If we have no
       io modules available, it's an error */

    if (NULL == selectable) {
        /* There's no modules available.  Doh! */
        /* show_help */
        return OMPI_ERROR;
    }

    /* Do some kind of collective operation to find a module that
       everyone has available */

#if 1
    /* For the moment, just take the top module off the list */

    item = opal_list_remove_first(selectable);
    avail = (avail_io_t *) item;
    selected = *avail;
    OBJ_RELEASE(avail);
#else
    /* JMS CONTINUE HERE */
#endif

    /* Everything left in the selectable list is therefore unwanted,
       and we call their unquery() method (because they all had
       query() invoked, but will never have init() invoked in this
       scope). */

    for (item = opal_list_remove_first(selectable); item != NULL;
         item = opal_list_remove_first(selectable)) {
        avail = (avail_io_t *) item;
        unquery(avail, filename, info);
        OBJ_RELEASE(item);
    }
    OBJ_RELEASE(selectable);

    /* Finally -- delete the file with the selected component */
        
    if (OMPI_SUCCESS != (err = delete_file(&selected, filename, info))) {
        return err;
    }
    
    /* Announce the winner */
  
    opal_output_verbose(10, mca_io_base_output,
                        "io:base:delete: Selected io component %s", 
                        selected.ai_component.v2_0_0.io_version.mca_component_name);
  
    return OMPI_SUCCESS;
}


/*
 * For each module in the list, if it is in the list of names (or the
 * list of names is NULL), then check and see if it wants to run, and
 * do the resulting priority comparison.  Make a list of components to
 * be only those who returned that they want to run, and put them in
 * priority order.
 */
static opal_list_t *check_components(opal_list_t *components, 
                                     char *filename, struct ompi_info_t *info,
                                     char **names, int num_names)
{
    int i;
    const mca_base_component_t *component;
    opal_list_item_t *item, *item2;
    bool want_to_check;
    opal_list_t *selectable;
    avail_io_t *avail, *avail2;

    /* Make a list of the components that query successfully */

    selectable = OBJ_NEW(opal_list_t);

    /* Scan through the list of components.  This nested loop is
       O(N^2), but we should never have too many components and/or
       names, so this *hopefully* shouldn't matter... */
  
    for (item = opal_list_get_first(components); 
         item != opal_list_get_end(components); 
         item = opal_list_get_next(item)) {
        component = ((mca_base_component_priority_list_item_t *) 
                     item)->super.cli_component;

        /* If we have a list of names, scan through it */

        if (0 == num_names) {
            want_to_check = true;
        } else {
            want_to_check = false;
            for (i = 0; i < num_names; ++i) {
                if (0 == strcmp(names[i], component->mca_component_name)) {
                    want_to_check = true;
                }
            }
        }

        /* If we determined that we want to check this component, then
           do so */

        if (want_to_check) {
            avail = check_one_component(component, filename, info);
            if (NULL != avail) {

                /* Put this item on the list in priority order
                   (highest priority first).  Should it go first? */

                item2 = opal_list_get_first(selectable); 
                avail2 = (avail_io_t *) item2;
                if (opal_list_get_end(selectable) == item2 ||
                    avail->ai_priority > avail2->ai_priority) {
                    opal_list_prepend(selectable, (opal_list_item_t*) avail);
                } else {
                    for (i = 1; item2 != opal_list_get_end(selectable); 
                         item2 = opal_list_get_next(selectable), ++i) {
                        avail2 = (avail_io_t *) item2;
                        if (avail->ai_priority > avail2->ai_priority) {
                            opal_list_insert(selectable,
                                             (opal_list_item_t *) avail, i);
                            break;
                        }
                    }

                    /* If we didn't find a place to put it in the
                       list, then append it (because it has the lowest
                       priority found so far) */

                    if (opal_list_get_end(selectable) == item2) {
                        opal_list_append(selectable, 
                                         (opal_list_item_t *) avail);
                    }
                }
            }
        }
    }
    
    /* If we didn't find any available components, return an error */
    
    if (0 == opal_list_get_size(selectable)) {
        OBJ_RELEASE(selectable);
        return NULL;
    }

    /* All done */

    return selectable;
}


/*
 * Check a single component
 */
static avail_io_t *check_one_component(const mca_base_component_t *component,
                                       char *filename, struct ompi_info_t *info)
{
    avail_io_t *avail;

    avail = query(component, filename, info);
    if (NULL != avail) {
        avail->ai_priority = (avail->ai_priority < 100) ? 
            avail->ai_priority : 100;
        avail->ai_priority = (avail->ai_priority < 0) ?
            0 : avail->ai_priority;
        opal_output_verbose(10, mca_io_base_output, 
                            "io:base:delete: component available: %s, priority: %d", 
                            component->mca_component_name, 
                            avail->ai_priority);
    } else {
        opal_output_verbose(10, mca_io_base_output, 
                            "io:base:delete: component not available: %s",
                            component->mca_component_name);
    }
    
    return avail;
}


/**************************************************************************
 * Query functions
 **************************************************************************/

/*
 * Take any version of a io module, query it, and return the right
 * module struct
 */
static avail_io_t *query(const mca_base_component_t *component, 
                         char *filename, struct ompi_info_t *info)
{
    const mca_io_base_component_2_0_0_t *ioc_200;

    /* io v2.0.0 */

    if (2 == component->mca_major_version &&
        0 == component->mca_minor_version &&
        0 == component->mca_release_version) {
        ioc_200 = (mca_io_base_component_2_0_0_t *) component;
        
        return query_2_0_0(ioc_200, filename, info);
    }

    /* Unknown io API version -- return error */

    return NULL;
}


static avail_io_t *query_2_0_0(const mca_io_base_component_2_0_0_t *component,
                               char *filename, struct ompi_info_t *info)
{
    bool usable;
    int priority, ret;
    avail_io_t *avail;
    struct mca_io_base_delete_t *private_data;

    /* Query v2.0.0 */

    avail = NULL;
    private_data = NULL;
    usable = false;
    ret = component->io_delete_query(filename, info, &private_data, &usable, 
                                     &priority);
    if (OMPI_SUCCESS == ret && usable) {
        avail = OBJ_NEW(avail_io_t);
        avail->ai_version = MCA_IO_BASE_V_2_0_0;
        avail->ai_priority = priority;
        avail->ai_component.v2_0_0 = *component;
        avail->ai_private_data = private_data;
    }

    return avail;
}


/**************************************************************************
 * Unquery functions
 **************************************************************************/

static void unquery(avail_io_t *avail, char *filename, struct ompi_info_t *info)
{
    const mca_io_base_component_2_0_0_t *ioc_200;

    switch(avail->ai_version) {
    case MCA_IO_BASE_V_2_0_0:
        ioc_200 = &(avail->ai_component.v2_0_0);
        if (NULL != ioc_200->io_delete_unquery) {
            ioc_200->io_delete_unquery(filename, info, avail->ai_private_data);
        }
        break;

    default:
        break;
    }
}


/**************************************************************************
 * File delete functions
 **************************************************************************/

/*
 * Invoke the component's delete function
 */
static int delete_file(avail_io_t *avail, char *filename, struct ompi_info_t *info)
{
    const mca_io_base_component_2_0_0_t *ioc_200;

    switch(avail->ai_version) {
    case MCA_IO_BASE_V_2_0_0:
        ioc_200 = &(avail->ai_component.v2_0_0);
        return ioc_200->io_delete_select(filename, info,
                                         avail->ai_private_data);
        break;

    default:
        return OMPI_ERROR;
        break;
    }

    /* No way to reach here */
}
