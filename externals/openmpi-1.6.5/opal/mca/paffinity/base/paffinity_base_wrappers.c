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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "opal_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"

int opal_paffinity_base_set(opal_paffinity_base_cpu_set_t cpumask)
{
    int rc;

    if (!opal_paffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    if (OPAL_SUCCESS == (rc = opal_paffinity_base_module->paff_module_set(cpumask))){
        opal_paffinity_base_bound = true;
    }
    return rc;
}

int opal_paffinity_base_get(opal_paffinity_base_cpu_set_t *cpumask)
{
    /* zero the cpumask so we start with a clean slate - do
     * it here so that any error returns no info
     */
    if (NULL != cpumask) {
        OPAL_PAFFINITY_CPU_ZERO(*cpumask);
    }
    
    if (!opal_paffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    if(NULL == cpumask) {
        return OPAL_ERR_BAD_PARAM;
    }
    return opal_paffinity_base_module->paff_module_get(cpumask);
}

int opal_paffinity_base_get_map_to_processor_id(int socket, int core, int *processor_id)
{
    if (!opal_paffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_paffinity_base_module->paff_get_map_to_processor_id(socket, core, processor_id);
}

int opal_paffinity_base_get_map_to_socket_core(int processor_id, int *socket, int *core)
{
    if (!opal_paffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_paffinity_base_module->paff_get_map_to_socket_core(processor_id, socket, core);
}


int opal_paffinity_base_get_processor_info(int *num_processors)
{
    if (!opal_paffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_paffinity_base_module->paff_get_processor_info(num_processors);
}

int opal_paffinity_base_get_socket_info(int *num_sockets)
{
    if (!opal_paffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_paffinity_base_module->paff_get_socket_info(num_sockets);
}

int opal_paffinity_base_get_core_info(int socket, int *num_cores)
{
    if (!opal_paffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_paffinity_base_module->paff_get_core_info(socket, num_cores);
}

int opal_paffinity_base_get_physical_processor_id(int logical_processor_id)
{
    if (!opal_paffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_paffinity_base_module->paff_get_physical_processor_id(logical_processor_id);
}

int opal_paffinity_base_get_physical_socket_id(int logical_socket_id)
{
    if (!opal_paffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_paffinity_base_module->paff_get_physical_socket_id(logical_socket_id);
}

int opal_paffinity_base_get_physical_core_id(int physical_socket_id, int logical_core_id)
{
    if (!opal_paffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_paffinity_base_module->paff_get_physical_core_id(physical_socket_id, logical_core_id);
}

char *opal_paffinity_base_print_binding(opal_paffinity_base_cpu_set_t cpumask)
{
    char *tmp;
    size_t i, j, masksize, save;
    
    /* get space for element separators and trailing NULL */
    masksize = (2*OPAL_PAFFINITY_CPU_SET_NUM_BYTES)+OPAL_PAFFINITY_BITMASK_NUM_ELEMENTS + 1;
    tmp = (char*)malloc(masksize);
    if (NULL == tmp) {
        return NULL;
    }
    memset(tmp, 0, masksize);
    masksize = sizeof(opal_paffinity_base_bitmask_t);

    /* Save the position of the last : before there are all zeros to
       the right -- we don't need to print all zeros to the right;
       we'll chop them off, below. */
    save = 0;
    if (4 == masksize) {
        for (i=0, j=0; i < OPAL_PAFFINITY_BITMASK_NUM_ELEMENTS; i++) {
            sprintf(&tmp[j], "%08lx", cpumask.bitmask[i]);
            j += 8;
            tmp[j] = ':';
            if (cpumask.bitmask[i] > 0) {
                save = j;
            }
            j++;
        }
    } else if (8 == masksize) {
        for (i=0, j=0; i < OPAL_PAFFINITY_BITMASK_NUM_ELEMENTS; i++) {
            sprintf(&tmp[j], "%016lx", cpumask.bitmask[i]);
            j += 16;
            tmp[j] = ':';
            j++;
            if (cpumask.bitmask[i] > 0) {
                save = j;
            }
        }
    }

    /* If there were no non-zero values, then ensure to print one
       field of zeros */
    if (0 == save) {
        tmp[2 * masksize] = '\0';
    } else {
        tmp[save] = '\0';
    }

    return tmp;
}

int opal_paffinity_base_parse_binding(char *binding, opal_paffinity_base_cpu_set_t *cpumask)
{
    size_t i;
    char *tmp, *save;
    
    if (NULL == binding || 0 == strlen(binding)) {
        return OPAL_SUCCESS;
    }
    
    OPAL_PAFFINITY_CPU_ZERO(*cpumask);
    tmp = binding;
    for (i=0; i < OPAL_PAFFINITY_BITMASK_NUM_ELEMENTS; i++) {
        cpumask->bitmask[i] = strtoul(tmp, &save, 16);
        tmp = save;
        if (NULL == tmp) {
            /* end of the line */
            break;
        }
        tmp++;
        if (NULL == tmp || 0 == strlen(tmp)) {
            return OPAL_SUCCESS;
        }
    }
    
    return OPAL_SUCCESS;
}

