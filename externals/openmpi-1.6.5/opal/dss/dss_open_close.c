/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
/** @file:
 *
 */
#include "opal_config.h"

#include "opal/mca/base/mca_base_param.h"

#include "opal/dss/dss_internal.h"

/**
 * globals
 */
bool opal_dss_initialized = false;
int opal_dss_verbose = -1;  /* by default disabled */
int opal_dss_initial_size;
int opal_dss_threshold_size;
opal_pointer_array_t opal_dss_types;
opal_data_type_t opal_dss_num_reg_types;
opal_dss_buffer_type_t default_buf_type;

opal_dss_t opal_dss = {
    opal_dss_set,
    opal_dss_get,
    opal_dss_set_buffer_type,
    opal_dss_pack,
    opal_dss_unpack,
    opal_dss_copy,
    opal_dss_compare,
    opal_dss_size,
    opal_dss_print,
    opal_dss_release,
    opal_dss_peek,
    opal_dss_unload,
    opal_dss_load,
    opal_dss_copy_payload,
    opal_dss_register,
    opal_dss_lookup_data_type,
    opal_dss_dump_data_types,
    opal_dss_dump
};

/**
 * Object constructors, destructors, and instantiations
 */
/** Data Value **/
/* constructor - used to initialize state of data value instance */
static void opal_data_value_construct(opal_dss_value_t* ptr)
{
    ptr->type = OPAL_UNDEF;
    ptr->data = NULL;
}
/* destructor - used to release data value instance */
static void opal_data_value_destruct(opal_dss_value_t* ptr)
{
    if (NULL != ptr->data) {
        opal_dss.release(ptr);
    }
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
    opal_dss_value_t,              /* type name */
    opal_object_t,                  /* parent "class" name */
    opal_data_value_construct,      /* constructor */
    opal_data_value_destruct);      /* destructor */


static void opal_buffer_construct (opal_buffer_t* buffer)
{
    /** set the default buffer type */
    buffer->type = default_buf_type;

    /* Make everything NULL to begin with */

    buffer->base_ptr = buffer->pack_ptr = buffer->unpack_ptr = NULL;
    buffer->bytes_allocated = buffer->bytes_used = 0;
}

static void opal_buffer_destruct (opal_buffer_t* buffer)
{
    if (NULL != buffer->base_ptr) {
        free (buffer->base_ptr);
    }
}

OBJ_CLASS_INSTANCE(opal_buffer_t,
                   opal_object_t,
                   opal_buffer_construct,
                   opal_buffer_destruct);


static void opal_dss_type_info_construct(opal_dss_type_info_t *obj)
{
    obj->odti_name = NULL;
    obj->odti_pack_fn = NULL;
    obj->odti_unpack_fn = NULL;
    obj->odti_copy_fn = NULL;
    obj->odti_compare_fn = NULL;
    obj->odti_size_fn = NULL;
    obj->odti_print_fn = NULL;
    obj->odti_release_fn = NULL;
    obj->odti_structured = false;
}

static void opal_dss_type_info_destruct(opal_dss_type_info_t *obj)
{
    if (NULL != obj->odti_name) {
        free(obj->odti_name);
    }
}

OBJ_CLASS_INSTANCE(opal_dss_type_info_t, opal_object_t,
                   opal_dss_type_info_construct,
                   opal_dss_type_info_destruct);


static void opal_pstat_construct(opal_pstats_t *obj)
{
    memset(obj->node, 0, sizeof(obj->node));
    memset(obj->cmd, 0, sizeof(obj->cmd));
    obj->state = 'U';
    obj->time = 0;
    obj->priority = -1;
    obj->num_threads = -1;
    obj->vsize = 0;
    obj->rss = 0;
    obj->peak_vsize = 0;
    obj->shared_size = 0;
    obj->processor = -1;
}

OBJ_CLASS_INSTANCE(opal_pstats_t, opal_list_item_t,
                   opal_pstat_construct,
                   NULL);


int opal_dss_open(void)
{
    char *enviro_val;
    int id, rc;
    opal_data_type_t tmp;
    int def_type;

    if (opal_dss_initialized) {
        return OPAL_SUCCESS;
    }

    enviro_val = getenv("OPAL_dss_debug");
    if (NULL != enviro_val) {  /* debug requested */
        opal_dss_verbose = 0;
    }

    /** set the default buffer type. If we are in debug mode, then we default
     * to fully described buffers. Otherwise, we default to non-described for brevity
     * and performance
     */
#if OPAL_ENABLE_DEBUG
    def_type = OPAL_DSS_BUFFER_FULLY_DESC;
#else
    def_type = OPAL_DSS_BUFFER_NON_DESC;
#endif

    id = mca_base_param_register_int("dss", "buffer", "type",
                                     "Set the default mode for OpenRTE buffers (0=non-described, 1=described)",
                                     def_type);
    mca_base_param_lookup_int(id, &rc);
    default_buf_type = rc;

    /* setup the initial size of the buffer. */
    id = mca_base_param_register_int("dss", "buffer_initial", "size", NULL, 
                                     OPAL_DSS_DEFAULT_INITIAL_SIZE);
    mca_base_param_lookup_int(id, &opal_dss_initial_size);

    /* the threshold as to where to stop doubling the size of the buffer 
     * allocated memory and start doing additive increases */
    id = mca_base_param_register_int("dss", "buffer_threshold", "size", NULL, 
                                     OPAL_DSS_DEFAULT_THRESHOLD_SIZE);
    mca_base_param_lookup_int(id, &opal_dss_threshold_size);

    /* Setup the types array */
    OBJ_CONSTRUCT(&opal_dss_types, opal_pointer_array_t);
    if (OPAL_SUCCESS != (rc = opal_pointer_array_init(&opal_dss_types,
                                                      OPAL_DSS_ID_DYNAMIC,
                                                      OPAL_DSS_ID_MAX,
                                                      OPAL_DSS_ID_MAX))) {
        return rc;
    }
    opal_dss_num_reg_types = 0;

    /* Register all the intrinsic types */

    tmp = OPAL_NULL;
    if (OPAL_SUCCESS != (rc = opal_dss.register_type(opal_dss_pack_null,
                                          opal_dss_unpack_null,
                                          (opal_dss_copy_fn_t)opal_dss_copy_null,
                                          (opal_dss_compare_fn_t)opal_dss_compare_null,
                                          (opal_dss_size_fn_t)opal_dss_std_size,
                                          (opal_dss_print_fn_t)opal_dss_print_null,
                                          (opal_dss_release_fn_t)opal_dss_std_release,
                                          OPAL_DSS_UNSTRUCTURED,
                                          "OPAL_NULL", &tmp))) {
        return rc;
    }
    tmp = OPAL_BYTE;
    if (OPAL_SUCCESS != (rc = opal_dss.register_type(opal_dss_pack_byte,
                                          opal_dss_unpack_byte,
                                          (opal_dss_copy_fn_t)opal_dss_std_copy,
                                          (opal_dss_compare_fn_t)opal_dss_compare_byte,
                                          (opal_dss_size_fn_t)opal_dss_std_size,
                                          (opal_dss_print_fn_t)opal_dss_print_byte,
                                          (opal_dss_release_fn_t)opal_dss_std_release,
                                          OPAL_DSS_UNSTRUCTURED,
                                          "OPAL_BYTE", &tmp))) {
        return rc;
    }
    tmp = OPAL_BOOL;
    if (OPAL_SUCCESS != (rc = opal_dss.register_type(opal_dss_pack_bool,
                                          opal_dss_unpack_bool,
                                          (opal_dss_copy_fn_t)opal_dss_std_copy,
                                          (opal_dss_compare_fn_t)opal_dss_compare_bool,
                                          (opal_dss_size_fn_t)opal_dss_std_size,
                                          (opal_dss_print_fn_t)opal_dss_print_bool,
                                          (opal_dss_release_fn_t)opal_dss_std_release,
                                          OPAL_DSS_UNSTRUCTURED,
                                          "OPAL_BOOL", &tmp))) {
        return rc;
    }
    tmp = OPAL_INT;
    if (OPAL_SUCCESS != (rc = opal_dss.register_type(opal_dss_pack_int,
                                          opal_dss_unpack_int,
                                          (opal_dss_copy_fn_t)opal_dss_std_copy,
                                          (opal_dss_compare_fn_t)opal_dss_compare_int,
                                          (opal_dss_size_fn_t)opal_dss_std_size,
                                          (opal_dss_print_fn_t)opal_dss_print_int,
                                          (opal_dss_release_fn_t)opal_dss_std_release,
                                          OPAL_DSS_UNSTRUCTURED,
                                          "OPAL_INT", &tmp))) {
        return rc;
    }
    tmp = OPAL_UINT;
    if (OPAL_SUCCESS != (rc = opal_dss.register_type(opal_dss_pack_int,
                                          opal_dss_unpack_int,
                                          (opal_dss_copy_fn_t)opal_dss_std_copy,
                                          (opal_dss_compare_fn_t)opal_dss_compare_uint,
                                          (opal_dss_size_fn_t)opal_dss_std_size,
                                          (opal_dss_print_fn_t)opal_dss_print_uint,
                                          (opal_dss_release_fn_t)opal_dss_std_release,
                                          OPAL_DSS_UNSTRUCTURED,
                                          "OPAL_UINT", &tmp))) {
        return rc;
    }
    tmp = OPAL_INT8;
    if (OPAL_SUCCESS != (rc = opal_dss.register_type(opal_dss_pack_byte,
                                          opal_dss_unpack_byte,
                                          (opal_dss_copy_fn_t)opal_dss_std_copy,
                                          (opal_dss_compare_fn_t)opal_dss_compare_int8,
                                          (opal_dss_size_fn_t)opal_dss_std_size,
                                          (opal_dss_print_fn_t)opal_dss_print_int8,
                                          (opal_dss_release_fn_t)opal_dss_std_release,
                                          OPAL_DSS_UNSTRUCTURED,
                                          "OPAL_INT8", &tmp))) {
        return rc;
    }
    tmp = OPAL_UINT8;
    if (OPAL_SUCCESS != (rc = opal_dss.register_type(opal_dss_pack_byte,
                                          opal_dss_unpack_byte,
                                          (opal_dss_copy_fn_t)opal_dss_std_copy,
                                          (opal_dss_compare_fn_t)opal_dss_compare_uint8,
                                          (opal_dss_size_fn_t)opal_dss_std_size,
                                          (opal_dss_print_fn_t)opal_dss_print_uint8,
                                          (opal_dss_release_fn_t)opal_dss_std_release,
                                          OPAL_DSS_UNSTRUCTURED,
                                          "OPAL_UINT8", &tmp))) {
        return rc;
    }
    tmp = OPAL_INT16;
    if (OPAL_SUCCESS != (rc = opal_dss.register_type(opal_dss_pack_int16,
                                          opal_dss_unpack_int16,
                                          (opal_dss_copy_fn_t)opal_dss_std_copy,
                                          (opal_dss_compare_fn_t)opal_dss_compare_int16,
                                          (opal_dss_size_fn_t)opal_dss_std_size,
                                          (opal_dss_print_fn_t)opal_dss_print_int16,
                                          (opal_dss_release_fn_t)opal_dss_std_release,
                                          OPAL_DSS_UNSTRUCTURED,
                                          "OPAL_INT16", &tmp))) {
        return rc;
    }
    tmp = OPAL_UINT16;
    if (OPAL_SUCCESS != (rc = opal_dss.register_type(opal_dss_pack_int16,
                                          opal_dss_unpack_int16,
                                          (opal_dss_copy_fn_t)opal_dss_std_copy,
                                          (opal_dss_compare_fn_t)opal_dss_compare_uint16,
                                          (opal_dss_size_fn_t)opal_dss_std_size,
                                          (opal_dss_print_fn_t)opal_dss_print_uint16,
                                          (opal_dss_release_fn_t)opal_dss_std_release,
                                          OPAL_DSS_UNSTRUCTURED,
                                          "OPAL_UINT16", &tmp))) {
        return rc;
    }
    tmp = OPAL_INT32;
    if (OPAL_SUCCESS != (rc = opal_dss.register_type(opal_dss_pack_int32,
                                          opal_dss_unpack_int32,
                                          (opal_dss_copy_fn_t)opal_dss_std_copy,
                                          (opal_dss_compare_fn_t)opal_dss_compare_int32,
                                          (opal_dss_size_fn_t)opal_dss_std_size,
                                          (opal_dss_print_fn_t)opal_dss_print_int32,
                                          (opal_dss_release_fn_t)opal_dss_std_release,
                                          OPAL_DSS_UNSTRUCTURED,
                                          "OPAL_INT32", &tmp))) {
        return rc;
    }
    tmp = OPAL_UINT32;
    if (OPAL_SUCCESS != (rc = opal_dss.register_type(opal_dss_pack_int32,
                                          opal_dss_unpack_int32,
                                          (opal_dss_copy_fn_t)opal_dss_std_copy,
                                          (opal_dss_compare_fn_t)opal_dss_compare_uint32,
                                          (opal_dss_size_fn_t)opal_dss_std_size,
                                          (opal_dss_print_fn_t)opal_dss_print_uint32,
                                          (opal_dss_release_fn_t)opal_dss_std_release,
                                          OPAL_DSS_UNSTRUCTURED,
                                          "OPAL_UINT32", &tmp))) {
        return rc;
    }
    tmp = OPAL_INT64;
    if (OPAL_SUCCESS != (rc = opal_dss.register_type(opal_dss_pack_int64,
                                          opal_dss_unpack_int64,
                                          (opal_dss_copy_fn_t)opal_dss_std_copy,
                                          (opal_dss_compare_fn_t)opal_dss_compare_int64,
                                          (opal_dss_size_fn_t)opal_dss_std_size,
                                          (opal_dss_print_fn_t)opal_dss_print_int64,
                                          (opal_dss_release_fn_t)opal_dss_std_release,
                                          OPAL_DSS_UNSTRUCTURED,
                                          "OPAL_INT64", &tmp))) {
        return rc;
    }
    tmp = OPAL_UINT64;
    if (OPAL_SUCCESS != (rc = opal_dss.register_type(opal_dss_pack_int64,
                                          opal_dss_unpack_int64,
                                          (opal_dss_copy_fn_t)opal_dss_std_copy,
                                          (opal_dss_compare_fn_t)opal_dss_compare_uint64,
                                          (opal_dss_size_fn_t)opal_dss_std_size,
                                          (opal_dss_print_fn_t)opal_dss_print_uint64,
                                          (opal_dss_release_fn_t)opal_dss_std_release,
                                          OPAL_DSS_UNSTRUCTURED,
                                          "OPAL_UINT64", &tmp))) {
        return rc;
    }
    tmp = OPAL_SIZE;
    if (OPAL_SUCCESS != (rc = opal_dss.register_type(opal_dss_pack_sizet,
                                          opal_dss_unpack_sizet,
                                          (opal_dss_copy_fn_t)opal_dss_std_copy,
                                          (opal_dss_compare_fn_t)opal_dss_compare_size,
                                          (opal_dss_size_fn_t)opal_dss_std_size,
                                          (opal_dss_print_fn_t)opal_dss_print_size,
                                          (opal_dss_release_fn_t)opal_dss_std_release,
                                          OPAL_DSS_UNSTRUCTURED,
                                          "OPAL_SIZE", &tmp))) {
        return rc;
    }
    tmp = OPAL_PID;
    if (OPAL_SUCCESS != (rc = opal_dss.register_type(opal_dss_pack_pid,
                                          opal_dss_unpack_pid,
                                          (opal_dss_copy_fn_t)opal_dss_std_copy,
                                          (opal_dss_compare_fn_t)opal_dss_compare_pid,
                                          (opal_dss_size_fn_t)opal_dss_std_size,
                                          (opal_dss_print_fn_t)opal_dss_print_pid,
                                          (opal_dss_release_fn_t)opal_dss_std_release,
                                          OPAL_DSS_UNSTRUCTURED,
                                          "OPAL_PID", &tmp))) {
        return rc;
    }
    tmp = OPAL_STRING;
    if (OPAL_SUCCESS != (rc = opal_dss.register_type(opal_dss_pack_string,
                                          opal_dss_unpack_string,
                                          (opal_dss_copy_fn_t)opal_dss_copy_string,
                                          (opal_dss_compare_fn_t)opal_dss_compare_string,
                                          (opal_dss_size_fn_t)opal_dss_size_string,
                                          (opal_dss_print_fn_t)opal_dss_print_string,
                                          (opal_dss_release_fn_t)opal_dss_std_release,
                                          OPAL_DSS_STRUCTURED,
                                          "OPAL_STRING", &tmp))) {
        return rc;
    }
    tmp = OPAL_DATA_TYPE;
    if (OPAL_SUCCESS != (rc = opal_dss.register_type(opal_dss_pack_data_type,
                                          opal_dss_unpack_data_type,
                                          (opal_dss_copy_fn_t)opal_dss_std_copy,
                                          (opal_dss_compare_fn_t)opal_dss_compare_dt,
                                          (opal_dss_size_fn_t)opal_dss_std_size,
                                          (opal_dss_print_fn_t)opal_dss_print_data_type,
                                          (opal_dss_release_fn_t)opal_dss_std_release,
                                          OPAL_DSS_UNSTRUCTURED,
                                          "OPAL_DATA_TYPE", &tmp))) {
        return rc;
    }
    tmp = OPAL_DATA_VALUE;
    if (OPAL_SUCCESS != (rc = opal_dss.register_type(opal_dss_pack_data_value,
                                          opal_dss_unpack_data_value,
                                          (opal_dss_copy_fn_t)opal_dss_copy_data_value,
                                          (opal_dss_compare_fn_t)opal_dss_compare_data_value,
                                          (opal_dss_size_fn_t)opal_dss_size_data_value,
                                          (opal_dss_print_fn_t)opal_dss_print_data_value,
                                          (opal_dss_release_fn_t)opal_dss_std_obj_release,
                                          OPAL_DSS_STRUCTURED,
                                          "OPAL_DATA_VALUE", &tmp))) {
        return rc;
    }

    tmp = OPAL_BYTE_OBJECT;
    if (OPAL_SUCCESS != (rc = opal_dss.register_type(opal_dss_pack_byte_object,
                                          opal_dss_unpack_byte_object,
                                          (opal_dss_copy_fn_t)opal_dss_copy_byte_object,
                                          (opal_dss_compare_fn_t)opal_dss_compare_byte_object,
                                          (opal_dss_size_fn_t)opal_dss_size_byte_object,
                                          (opal_dss_print_fn_t)opal_dss_print_byte_object,
                                          (opal_dss_release_fn_t)opal_dss_release_byte_object,
                                          OPAL_DSS_STRUCTURED,
                                          "OPAL_BYTE_OBJECT", &tmp))) {
        return rc;
    }

    tmp = OPAL_PSTAT;
    if (OPAL_SUCCESS != (rc = opal_dss.register_type(opal_dss_pack_pstat,
                                                     opal_dss_unpack_pstat,
                                                     (opal_dss_copy_fn_t)opal_dss_copy_pstat,
                                                     (opal_dss_compare_fn_t)opal_dss_compare_pstat,
                                                     (opal_dss_size_fn_t)opal_dss_size_pstat,
                                                     (opal_dss_print_fn_t)opal_dss_print_pstat,
                                                     (opal_dss_release_fn_t)opal_dss_std_obj_release,
                                                     OPAL_DSS_STRUCTURED,
                                                     "OPAL_PSTAT", &tmp))) {
        return rc;
    }
    /* All done */

    return OPAL_SUCCESS;
}


int opal_dss_close(void)
{
    int32_t i;

    opal_dss_initialized = false;

    for (i = 0 ; i < opal_pointer_array_get_size(&opal_dss_types) ; ++i) {
        opal_dss_type_info_t *info = (opal_dss_type_info_t*)opal_pointer_array_get_item(&opal_dss_types, i);
        if (NULL != info) {
            opal_pointer_array_set_item(&opal_dss_types, i, NULL);
            OBJ_RELEASE(info);
        }
    }

    OBJ_DESTRUCT(&opal_dss_types);

    return OPAL_SUCCESS;
}
