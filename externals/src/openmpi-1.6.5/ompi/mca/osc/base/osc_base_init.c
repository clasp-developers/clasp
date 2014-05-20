/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
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

#include "ompi_config.h"

#include "ompi/constants.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/osc/osc.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/info/info.h"
#include "ompi/communicator/communicator.h"
#include "ompi/win/win.h"

int
ompi_osc_base_select(ompi_win_t *win,
                    ompi_info_t *info,
                    ompi_communicator_t *comm)
{
    opal_list_item_t *item;
    ompi_osc_base_component_t *best_component = NULL;
    int best_priority = -1, priority;

    if (opal_list_get_size(&ompi_osc_base_avail_components) <= 0) {
        /* we don't have any components to support us... */
        return OMPI_ERR_NOT_SUPPORTED;
    }

    for (item = opal_list_get_first(&ompi_osc_base_avail_components) ;
         item != opal_list_get_end(&ompi_osc_base_avail_components) ;
         item = opal_list_get_next(item)) {
        ompi_osc_base_component_t *component = (ompi_osc_base_component_t*)
            ((mca_base_component_list_item_t*) item)->cli_component;

        priority = component->osc_query(win, info, comm);
        if (priority < 0) continue;
        if (priority > best_priority) {
            best_component = component;
            best_priority = priority;
        }
    }

    if (NULL == best_component) return OMPI_ERR_NOT_SUPPORTED;

    return best_component->osc_select(win, info, comm);
}
