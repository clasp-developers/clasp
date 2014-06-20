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
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved.
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
#include "opal/dss/dss.h"

#include "orte/mca/grpcomm/grpcomm.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "ompi/proc/proc.h"
#include "ompi/runtime/ompi_module_exchange.h"


int
ompi_modex_send(mca_base_component_t * source_component,
                const void *data, size_t size)
{
    int rc;
    char * name = mca_base_component_to_string(source_component);
    
    if(NULL == name) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    rc = orte_grpcomm.set_proc_attr(name, data, size);
    free(name);
    return rc;
}


int
ompi_modex_recv(mca_base_component_t * component,
                ompi_proc_t * proc,
                void **buffer,
                size_t * size)
{
    int rc;
    char * name = mca_base_component_to_string(component);
    
    if(NULL == name) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    rc = orte_grpcomm.get_proc_attr(proc->proc_name, name, buffer, size);
    free(name);
    return rc;
}

int
ompi_modex_send_string(const char* key,
                       const void *buffer, size_t size)
{
    return orte_grpcomm.set_proc_attr(key, buffer, size);
}


int
ompi_modex_recv_string(const char* key,
                       struct ompi_proc_t *source_proc,
                       void **buffer, size_t *size)
{
    return orte_grpcomm.get_proc_attr(source_proc->proc_name, key, buffer, size);
}

int
ompi_modex_send_key_value(const char* key,
                          const void *value,
                          opal_data_type_t dtype)
{
    int rc;
    opal_buffer_t buf;
    opal_byte_object_t bo;
    
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf, value, 1, dtype))) {
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    if (OPAL_SUCCESS != (rc = opal_dss.unload(&buf, (void**)&bo.bytes, &bo.size))) {
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    OBJ_DESTRUCT(&buf);
    
    return orte_grpcomm.set_proc_attr(key, bo.bytes, bo.size);
}


int
ompi_modex_recv_key_value(const char* key,
                          struct ompi_proc_t *source_proc,
                          void *value, opal_data_type_t dtype)
{
    int rc;
    opal_buffer_t buf;
    opal_byte_object_t bo;
    int32_t n;
    size_t bsize;
    
    bo.bytes = NULL;
    bo.size = 0;
    if (OMPI_SUCCESS != (rc = orte_grpcomm.get_proc_attr(source_proc->proc_name, key,
                                                         (void**)&bo.bytes, &bsize))) {
        return rc;
    }
    bo.size = bsize;
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    if (OMPI_SUCCESS != (rc = opal_dss.load(&buf, bo.bytes, bo.size))) {
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    n = 1;
    rc = opal_dss.unpack(&buf, value, &n, dtype);
    OBJ_DESTRUCT(&buf);
    return rc;
}
