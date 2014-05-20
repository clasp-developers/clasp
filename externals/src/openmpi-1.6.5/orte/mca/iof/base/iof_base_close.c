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
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdio.h>

#include "opal/event/event.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/util/proc_info.h"

#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"


int orte_iof_base_close(void)
{
    bool dump;
    opal_list_item_t *item;
    orte_iof_write_output_t *output;
    orte_iof_write_event_t *wev;
    int num_written;
    
    /* shutdown any remaining opened components */
    if (0 != opal_list_get_size(&orte_iof_base.iof_components_opened)) {
        mca_base_components_close(orte_iof_base.iof_output, 
                                  &orte_iof_base.iof_components_opened, 
                                  NULL, true);
    }
    OBJ_DESTRUCT(&orte_iof_base.iof_components_opened);

    OPAL_THREAD_LOCK(&orte_iof_base.iof_write_output_lock);
    if (!ORTE_PROC_IS_DAEMON) {
        /* check if anything is still trying to be written out */
        wev = orte_iof_base.iof_write_stdout->wev;
        if (!opal_list_is_empty(&wev->outputs)) {
            dump = false;
            /* make one last attempt to write this out */
            while (NULL != (item = opal_list_remove_first(&wev->outputs))) {
                output = (orte_iof_write_output_t*)item;
                if (!dump) {
                    num_written = write(wev->fd, output->data, output->numbytes);
                    if (num_written < output->numbytes) {
                        /* don't retry - just cleanout the list and dump it */
                        dump = true;
                    }
                }
                OBJ_RELEASE(output);
            }
        }
        OBJ_RELEASE(orte_iof_base.iof_write_stdout);
        if (!orte_xml_output) {
            /* we only opened stderr channel if we are NOT doing xml output */
            wev = orte_iof_base.iof_write_stderr->wev;
            if (!opal_list_is_empty(&wev->outputs)) {
                dump = false;
                /* make one last attempt to write this out */
                while (NULL != (item = opal_list_remove_first(&wev->outputs))) {
                    output = (orte_iof_write_output_t*)item;
                    if (!dump) {
                        num_written = write(wev->fd, output->data, output->numbytes);
                        if (num_written < output->numbytes) {
                            /* don't retry - just cleanout the list and dump it */
                            dump = true;
                        }
                    }
                    OBJ_RELEASE(output);
                }
            }
            OBJ_RELEASE(orte_iof_base.iof_write_stderr);
        }
    }
    OPAL_THREAD_UNLOCK(&orte_iof_base.iof_write_output_lock);

    OBJ_DESTRUCT(&orte_iof_base.iof_write_output_lock);


    return ORTE_SUCCESS;
}

