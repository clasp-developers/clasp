/* -*- C -*-
 *
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
 *
 */
#ifndef OPAL_DSS_INTERNAL_H_
#define OPAL_DSS_INTERNAL_H_

#include "opal_config.h"
#include "opal/constants.h"

#include "opal/class/opal_pointer_array.h"

#include "opal/dss/dss.h"

#ifdef HAVE_STRING_H
#    if !defined(STDC_HEADERS) && HAVE_MEMORY_H
#        include <memory.h>
#    endif
#    include <string.h>
#endif

BEGIN_C_DECLS

/*
 * The default starting chunk size
 */
#define OPAL_DSS_DEFAULT_INITIAL_SIZE  128
/*
 * The default threshold size when we switch from doubling the 
 * buffer size to addatively increasing it
 */
#define OPAL_DSS_DEFAULT_THRESHOLD_SIZE 1024

/*
 * Internal type corresponding to size_t.  Do not use this in
 * interface calls - use OPAL_SIZE instead.
 */
#if SIZEOF_SIZE_T == 1
#define DSS_TYPE_SIZE_T OPAL_UINT8
#elif SIZEOF_SIZE_T == 2
#define DSS_TYPE_SIZE_T OPAL_UINT16
#elif SIZEOF_SIZE_T == 4
#define DSS_TYPE_SIZE_T OPAL_UINT32
#elif SIZEOF_SIZE_T == 8
#define DSS_TYPE_SIZE_T OPAL_UINT64
#else
#error Unsupported size_t size!
#endif

/*
 * Internal type corresponding to bool.  Do not use this in interface
 * calls - use OPAL_BOOL instead.
 */
#if SIZEOF_BOOL == 1
#define DSS_TYPE_BOOL OPAL_UINT8
#elif SIZEOF_BOOL == 2
#define DSS_TYPE_BOOL OPAL_UINT16
#elif SIZEOF_BOOL == 4
#define DSS_TYPE_BOOL OPAL_UINT32
#elif SIZEOF_BOOL == 8
#define DSS_TYPE_BOOL OPAL_UINT64
#else
#error Unsupported bool size!
#endif

/*
 * Internal type corresponding to int and unsigned int.  Do not use
 * this in interface calls - use OPAL_INT / OPAL_UINT instead.
 */
#if SIZEOF_INT == 1
#define DSS_TYPE_INT OPAL_INT8
#define DSS_TYPE_UINT OPAL_UINT8
#elif SIZEOF_INT == 2
#define DSS_TYPE_INT OPAL_INT16
#define DSS_TYPE_UINT OPAL_UINT16
#elif SIZEOF_INT == 4
#define DSS_TYPE_INT OPAL_INT32
#define DSS_TYPE_UINT OPAL_UINT32
#elif SIZEOF_INT == 8
#define DSS_TYPE_INT OPAL_INT64
#define DSS_TYPE_UINT OPAL_UINT64
#else
#error Unsupported int size!
#endif

/*
 * Internal type corresponding to pid_t.  Do not use this in interface
 * calls - use OPAL_PID instead.
 */
#if SIZEOF_PID_T == 1
#define DSS_TYPE_PID_T OPAL_UINT8
#elif SIZEOF_PID_T == 2
#define DSS_TYPE_PID_T OPAL_UINT16
#elif SIZEOF_PID_T == 4
#define DSS_TYPE_PID_T OPAL_UINT32
#elif SIZEOF_PID_T == 8
#define DSS_TYPE_PID_T OPAL_UINT64
#else
#error Unsupported pid_t size!
#endif

/* Unpack generic size macros */
#define UNPACK_SIZE_MISMATCH(unpack_type, remote_type, ret)      \
do { \
    switch(remote_type) { \
        case OPAL_UINT8: \
            UNPACK_SIZE_MISMATCH_FOUND(unpack_type, uint8_t, remote_type); \
            break; \
        case OPAL_INT8: \
            UNPACK_SIZE_MISMATCH_FOUND(unpack_type, int8_t, remote_type); \
            break; \
        case OPAL_UINT16: \
            UNPACK_SIZE_MISMATCH_FOUND(unpack_type, uint16_t, remote_type); \
            break; \
        case OPAL_INT16: \
            UNPACK_SIZE_MISMATCH_FOUND(unpack_type, int16_t, remote_type); \
            break; \
        case OPAL_UINT32: \
            UNPACK_SIZE_MISMATCH_FOUND(unpack_type, uint32_t, remote_type); \
            break; \
        case OPAL_INT32: \
            UNPACK_SIZE_MISMATCH_FOUND(unpack_type, int32_t, remote_type); \
            break; \
        case OPAL_UINT64: \
            UNPACK_SIZE_MISMATCH_FOUND(unpack_type, uint64_t, remote_type); \
            break; \
        case OPAL_INT64: \
            UNPACK_SIZE_MISMATCH_FOUND(unpack_type, int64_t, remote_type); \
            break; \
        default: \
            ret = OPAL_ERR_NOT_FOUND; \
    } \
} while (0)
        
/* NOTE: do not need to deal with endianness here, as the unpacking of
the underling sender-side type will do that for us.  Repeat: the
data in tmpbuf[] is already in host byte order. */
#define UNPACK_SIZE_MISMATCH_FOUND(unpack_type, tmptype, tmpdsstype)        \
do {                                                                    \
    int32_t i;                                                  \
    tmptype *tmpbuf = (tmptype*)malloc(sizeof(tmptype) * (*num_vals));  \
    ret = opal_dss_unpack_buffer(buffer, tmpbuf, num_vals, tmpdsstype); \
    for (i = 0 ; i < *num_vals ; ++i) {                                 \
        ((unpack_type*) dest)[i] = (unpack_type)(tmpbuf[i]);            \
    }                                                                   \
    free(tmpbuf);                                                       \
} while (0)
            
            
/**
 * Internal struct used for holding registered dss functions
 */
struct opal_dss_type_info_t {
    opal_object_t super;
    /* type identifier */
    opal_data_type_t odti_type;
    /** Debugging string name */
    char *odti_name;
    /** Pack function */
    opal_dss_pack_fn_t odti_pack_fn;
    /** Unpack function */
    opal_dss_unpack_fn_t odti_unpack_fn;
    /** copy function */
    opal_dss_copy_fn_t odti_copy_fn;
    /** compare function */
    opal_dss_compare_fn_t odti_compare_fn;
    /** size function */
    opal_dss_size_fn_t odti_size_fn;
    /** print function */
    opal_dss_print_fn_t odti_print_fn;
    /** Release function */
    opal_dss_release_fn_t odti_release_fn;
    /** flag to indicate structured data */
    bool odti_structured;
};
/**
 * Convenience typedef
 */
typedef struct opal_dss_type_info_t opal_dss_type_info_t;
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_dss_type_info_t);

/*
 * globals needed within dss
 */
extern bool opal_dss_initialized;
extern bool opal_dss_debug;
extern int opal_dss_verbose;
extern int opal_dss_initial_size;
extern int opal_dss_threshold_size;
extern opal_pointer_array_t opal_dss_types;
extern opal_data_type_t opal_dss_num_reg_types;

    /*
     * Implementations of API functions
     */

    int opal_dss_set(opal_dss_value_t *value, void *new_value, opal_data_type_t type);

    int opal_dss_get(void **data, opal_dss_value_t *value, opal_data_type_t type);

    int opal_dss_set_buffer_type(opal_buffer_t *buffer, opal_dss_buffer_type_t type);

    int opal_dss_pack(opal_buffer_t *buffer, const void *src,
                      int32_t num_vals,
                      opal_data_type_t type);
    int opal_dss_unpack(opal_buffer_t *buffer, void *dest,
                        int32_t *max_num_vals,
                        opal_data_type_t type);

    int opal_dss_copy(void **dest, void *src, opal_data_type_t type);

    int opal_dss_compare(const void *value1, const void *value2,
                         opal_data_type_t type);

    int opal_dss_print(char **output, char *prefix, void *src, opal_data_type_t type);

    int opal_dss_dump(int output_stream, void *src, opal_data_type_t type);

    int opal_dss_size(size_t *size, void *src, opal_data_type_t type);

    int opal_dss_peek(opal_buffer_t *buffer, opal_data_type_t *type,
                      int32_t *number);

    int opal_dss_peek_type(opal_buffer_t *buffer, opal_data_type_t *type);

    int opal_dss_unload(opal_buffer_t *buffer, void **payload,
                        int32_t *bytes_used);
    int opal_dss_load(opal_buffer_t *buffer, void *payload, int32_t bytes_used);

    int opal_dss_copy_payload(opal_buffer_t *dest, opal_buffer_t *src);

    int opal_dss_register(opal_dss_pack_fn_t pack_fn,
                          opal_dss_unpack_fn_t unpack_fn,
                          opal_dss_copy_fn_t copy_fn,
                          opal_dss_compare_fn_t compare_fn,
                          opal_dss_size_fn_t size_fn,
                          opal_dss_print_fn_t print_fn,
                          opal_dss_release_fn_t release_fn,
                          bool structured,
                          const char *name, opal_data_type_t *type);

    void opal_dss_release(opal_dss_value_t *value);

    char *opal_dss_lookup_data_type(opal_data_type_t type);

    void opal_dss_dump_data_types(int output);

    /*
     * Specialized functions
     */
OPAL_DECLSPEC    int opal_dss_pack_buffer(opal_buffer_t *buffer, const void *src, 
                             int32_t num_vals, opal_data_type_t type);

OPAL_DECLSPEC    int opal_dss_unpack_buffer(opal_buffer_t *buffer, void *dst, 
                               int32_t *num_vals, opal_data_type_t type);

    /*
     * Internal pack functions
     */

    int opal_dss_pack_null(opal_buffer_t *buffer, const void *src,
                           int32_t num_vals, opal_data_type_t type);
    int opal_dss_pack_byte(opal_buffer_t *buffer, const void *src,
                           int32_t num_vals, opal_data_type_t type);

    int opal_dss_pack_bool(opal_buffer_t *buffer, const void *src,
                           int32_t num_vals, opal_data_type_t type);

    int opal_dss_pack_int(opal_buffer_t *buffer, const void *src,
                          int32_t num_vals, opal_data_type_t type);
    int opal_dss_pack_int16(opal_buffer_t *buffer, const void *src,
                            int32_t num_vals, opal_data_type_t type);
    int opal_dss_pack_int32(opal_buffer_t *buffer, const void *src,
                            int32_t num_vals, opal_data_type_t type);
    int opal_dss_pack_int64(opal_buffer_t *buffer, const void *src,
                            int32_t num_vals, opal_data_type_t type);

    int opal_dss_pack_sizet(opal_buffer_t *buffer, const void *src,
                            int32_t num_vals, opal_data_type_t type);

    int opal_dss_pack_pid(opal_buffer_t *buffer, const void *src,
                          int32_t num_vals, opal_data_type_t type);

    int opal_dss_pack_string(opal_buffer_t *buffer, const void *src,
                             int32_t num_vals, opal_data_type_t type);

    int opal_dss_pack_data_type(opal_buffer_t *buffer, const void *src,
                           int32_t num_vals, opal_data_type_t type);

    int opal_dss_pack_data_value(opal_buffer_t *buffer, const void *src,
                           int32_t num_vals, opal_data_type_t type);

    int opal_dss_pack_byte_object(opal_buffer_t *buffer, const void *src,
                           int32_t num_vals, opal_data_type_t type);

    int opal_dss_pack_pstat(opal_buffer_t *buffer, const void *src,
                            int32_t num_vals, opal_data_type_t type);

    /*
     * Internal unpack functions
     */

    int opal_dss_unpack_null(opal_buffer_t *buffer, void *dest,
                             int32_t *num_vals, opal_data_type_t type);
    int opal_dss_unpack_byte(opal_buffer_t *buffer, void *dest,
                             int32_t *num_vals, opal_data_type_t type);

    int opal_dss_unpack_bool(opal_buffer_t *buffer, void *dest,
                             int32_t *num_vals, opal_data_type_t type);

    int opal_dss_unpack_int(opal_buffer_t *buffer, void *dest,
                            int32_t *num_vals, opal_data_type_t type);
    int opal_dss_unpack_int16(opal_buffer_t *buffer, void *dest,
                              int32_t *num_vals, opal_data_type_t type);
    int opal_dss_unpack_int32(opal_buffer_t *buffer, void *dest,
                              int32_t *num_vals, opal_data_type_t type);
    int opal_dss_unpack_int64(opal_buffer_t *buffer, void *dest,
                              int32_t *num_vals, opal_data_type_t type);

    int opal_dss_unpack_sizet(opal_buffer_t *buffer, void *dest,
                              int32_t *num_vals, opal_data_type_t type);

    int opal_dss_unpack_pid(opal_buffer_t *buffer, void *dest,
                            int32_t *num_vals, opal_data_type_t type);

    int opal_dss_unpack_string(opal_buffer_t *buffer, void *dest,
                               int32_t *num_vals, opal_data_type_t type);

    int opal_dss_unpack_data_type(opal_buffer_t *buffer, void *dest,
                             int32_t *num_vals, opal_data_type_t type);

    int opal_dss_unpack_data_value(opal_buffer_t *buffer, void *dest,
                             int32_t *num_vals, opal_data_type_t type);

    int opal_dss_unpack_byte_object(opal_buffer_t *buffer, void *dest,
                             int32_t *num_vals, opal_data_type_t type);

    int opal_dss_unpack_pstat(opal_buffer_t *buffer, void *dest,
                              int32_t *num_vals, opal_data_type_t type);

    /*
     * Internal copy functions
     */

    int opal_dss_std_copy(void **dest, void *src, opal_data_type_t type);

    int opal_dss_copy_null(char **dest, char *src, opal_data_type_t type);

    int opal_dss_copy_string(char **dest, char *src, opal_data_type_t type);

    int opal_dss_copy_byte_object(opal_byte_object_t **dest, opal_byte_object_t *src,
                                  opal_data_type_t type);

    int opal_dss_copy_data_value(opal_dss_value_t **dest, opal_dss_value_t *src,
                                  opal_data_type_t type);

    int opal_dss_copy_pstat(opal_pstats_t **dest, opal_pstats_t *src,
                            opal_data_type_t type);

    /*
     * Internal compare functions
     */

    int opal_dss_compare_bool(bool *value1, bool *value2, opal_data_type_t type);

    int opal_dss_compare_int(int *value1, int *value2, opal_data_type_t type);
    int opal_dss_compare_uint(unsigned int *value1, unsigned int *value2, opal_data_type_t type);

    int opal_dss_compare_size(size_t *value1, size_t *value2, opal_data_type_t type);

    int opal_dss_compare_pid(pid_t *value1, pid_t *value2, opal_data_type_t type);

    int opal_dss_compare_byte(char *value1, char *value2, opal_data_type_t type);
    int opal_dss_compare_char(char *value1, char *value2, opal_data_type_t type);
    int opal_dss_compare_int8(int8_t *value1, int8_t *value2, opal_data_type_t type);
    int opal_dss_compare_uint8(uint8_t *value1, uint8_t *value2, opal_data_type_t type);

    int opal_dss_compare_int16(int16_t *value1, int16_t *value2, opal_data_type_t type);
    int opal_dss_compare_uint16(uint16_t *value1, uint16_t *value2, opal_data_type_t type);

    int opal_dss_compare_int32(int32_t *value1, int32_t *value2, opal_data_type_t type);
    int opal_dss_compare_uint32(uint32_t *value1, uint32_t *value2, opal_data_type_t type);

    int opal_dss_compare_int64(int64_t *value1, int64_t *value2, opal_data_type_t type);
    int opal_dss_compare_uint64(uint64_t *value1, uint64_t *value2, opal_data_type_t type);

    int opal_dss_compare_null(char *value1, char *value2, opal_data_type_t type);

    int opal_dss_compare_string(char *value1, char *value2, opal_data_type_t type);

    int opal_dss_compare_dt(opal_data_type_t *value1, opal_data_type_t *value2, opal_data_type_t type);

    int opal_dss_compare_data_value(opal_dss_value_t *value1, opal_dss_value_t *value2, opal_data_type_t type);

    int opal_dss_compare_byte_object(opal_byte_object_t *value1, opal_byte_object_t *value2, opal_data_type_t type);

    int opal_dss_compare_pstat(opal_pstats_t *value1, opal_pstats_t *value2, opal_data_type_t type);

    /*
    * Internal size functions
    */
    int opal_dss_std_size(size_t *size, void *src, opal_data_type_t type);

    int opal_dss_size_string(size_t *size, char *src, opal_data_type_t type);

    int opal_dss_size_data_value(size_t *size, opal_dss_value_t *src, opal_data_type_t type);

    int opal_dss_size_byte_object(size_t *size, opal_byte_object_t *src, opal_data_type_t type);

    int opal_dss_size_pstat(size_t *size, opal_pstats_t *src, opal_data_type_t type);

    /*
    * Internal print functions
    */
    int opal_dss_print_byte(char **output, char *prefix, uint8_t *src, opal_data_type_t type);

    int opal_dss_print_string(char **output, char *prefix, char *src, opal_data_type_t type);

    int opal_dss_print_size(char **output, char *prefix, size_t *src, opal_data_type_t type);
    int opal_dss_print_pid(char **output, char *prefix, pid_t *src, opal_data_type_t type);
    int opal_dss_print_bool(char **output, char *prefix, bool *src, opal_data_type_t type);
    int opal_dss_print_int(char **output, char *prefix, int *src, opal_data_type_t type);
    int opal_dss_print_uint(char **output, char *prefix, int *src, opal_data_type_t type);
    int opal_dss_print_uint8(char **output, char *prefix, uint8_t *src, opal_data_type_t type);
    int opal_dss_print_uint16(char **output, char *prefix, uint16_t *src, opal_data_type_t type);
    int opal_dss_print_uint32(char **output, char *prefix, uint32_t *src, opal_data_type_t type);
    int opal_dss_print_int8(char **output, char *prefix, int8_t *src, opal_data_type_t type);
    int opal_dss_print_int16(char **output, char *prefix, int16_t *src, opal_data_type_t type);
    int opal_dss_print_int32(char **output, char *prefix, int32_t *src, opal_data_type_t type);
#ifdef HAVE_INT64_T
    int opal_dss_print_uint64(char **output, char *prefix, uint64_t *src, opal_data_type_t type);
    int opal_dss_print_int64(char **output, char *prefix, int64_t *src, opal_data_type_t type);
#else
    int opal_dss_print_uint64(char **output, char *prefix, void *src, opal_data_type_t type);
    int opal_dss_print_int64(char **output, char *prefix, void *src, opal_data_type_t type);
#endif
    int opal_dss_print_null(char **output, char *prefix, void *src, opal_data_type_t type);
    int opal_dss_print_data_type(char **output, char *prefix, opal_data_type_t *src, opal_data_type_t type);
    int opal_dss_print_data_value(char **output, char *prefix, opal_dss_value_t *src, opal_data_type_t type);
    int opal_dss_print_byte_object(char **output, char *prefix, opal_byte_object_t *src, opal_data_type_t type);
    int opal_dss_print_pstat(char **output, char *prefix, opal_pstats_t *src, opal_data_type_t type);


    /*
    * Internal release functions
    */
    void opal_dss_std_release(opal_dss_value_t *value);

    void opal_dss_std_obj_release(opal_dss_value_t *value);

    void opal_dss_release_byte_object(opal_dss_value_t *value);

    /*
     * Internal helper functions
     */

    char* opal_dss_buffer_extend(opal_buffer_t *bptr, size_t bytes_to_add);

    bool opal_dss_too_small(opal_buffer_t *buffer, size_t bytes_reqd);

    opal_dss_type_info_t* opal_dss_find_type(opal_data_type_t type);

    int opal_dss_store_data_type(opal_buffer_t *buffer, opal_data_type_t type);

    int opal_dss_get_data_type(opal_buffer_t *buffer, opal_data_type_t *type);

END_C_DECLS

#endif
