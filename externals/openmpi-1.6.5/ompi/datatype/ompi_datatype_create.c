/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stddef.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "ompi/constants.h"
#include "opal/class/opal_pointer_array.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/attribute/attribute.h"


static void __ompi_datatype_allocate( ompi_datatype_t* datatype )
{
    datatype->args               = NULL;
    datatype->d_f_to_c_index     = opal_pointer_array_add(&ompi_datatype_f_to_c_table, datatype);
    /* Later generated datatypes will have their id according to the Fortran ID, as ALL types are registered */
    datatype->id                 = datatype->d_f_to_c_index;
    datatype->d_keyhash          = NULL;
    datatype->name[0]            = '\0';
    datatype->packed_description = NULL;
}

static void __ompi_datatype_release(ompi_datatype_t * datatype)
{
    if( NULL != datatype->args ) {
        ompi_datatype_release_args( datatype );
        datatype->args = NULL;
    }
    if( NULL != datatype->packed_description ) {
        free( datatype->packed_description );
        datatype->packed_description = NULL;
    }
    if( NULL != opal_pointer_array_get_item(&ompi_datatype_f_to_c_table, datatype->d_f_to_c_index) ){
        opal_pointer_array_set_item( &ompi_datatype_f_to_c_table, datatype->d_f_to_c_index, NULL );
    }
    /* any pending attributes ? */
    if (NULL != datatype->d_keyhash) {
        ompi_attr_delete_all( TYPE_ATTR, datatype, datatype->d_keyhash );
        OBJ_RELEASE( datatype->d_keyhash );
    }
    /* make sure the name is set to empty */
    datatype->name[0] = '\0';
}

OBJ_CLASS_INSTANCE(ompi_datatype_t, opal_datatype_t, __ompi_datatype_allocate, __ompi_datatype_release);

ompi_datatype_t * ompi_datatype_create( int32_t expectedSize )
{
    int ret;
    ompi_datatype_t * datatype = (ompi_datatype_t*)OBJ_NEW(ompi_datatype_t);

    ret = opal_datatype_create_desc ( &(datatype->super), expectedSize);
    if (OPAL_SUCCESS != ret)
        return NULL;

    return datatype;
}
