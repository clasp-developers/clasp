/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_INFO_H
#define OMPI_INFO_H

#include "ompi_config.h"
#include <string.h>

#include "mpi.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/threads/mutex.h"


/**
 * \internal
 * ompi_info_t structure. MPI_Info is a pointer to this structure
 */
struct ompi_info_t {
  opal_list_t super; 
  /**< generic list pointer which is the container for (key,value)
       pairs */
  int i_f_to_c_index; 
  /**< fortran handle for info. This is needed for translation from
       fortran to C and vice versa */
  opal_mutex_t *i_lock;
  /**< Mutex for thread safety */
  bool i_freed;
  /**< Whether this info has been freed or not */
};
/**
 * \internal
 * Convenience typedef
 */
typedef struct ompi_info_t ompi_info_t;

/**
 * Padded struct to maintain back compatibiltiy.
 * See ompi/communicator/communicator.h comments with struct ompi_communicator_t
 * for full explanation why we chose the following padding construct for predefines.
 */
#define PREDEFINED_INFO_PAD (sizeof(void*) * 32)

struct ompi_predefined_info_t {
    struct ompi_info_t info;
    char padding[PREDEFINED_INFO_PAD - sizeof(ompi_info_t)];
};
typedef struct ompi_predefined_info_t ompi_predefined_info_t;


/**
 * \internal
 *
 * ompi_info_entry_t object. Each item in ompi_info_list is of this
 * type. It contains (key,value) pairs
 */
struct ompi_info_entry_t {
    opal_list_item_t super; /**< required for opal_list_t type */
    char *ie_value; /**< value part of the (key, value) pair.
                  * Maximum length is MPI_MAX_INFO_VAL */
    char ie_key[MPI_MAX_INFO_KEY + 1]; /**< "key" part of the (key, value)
                                     * pair */ 
};
/**
 * \internal
 * Convenience typedef
 */
typedef struct ompi_info_entry_t ompi_info_entry_t;

BEGIN_C_DECLS

/**
 * Table for Fortran <-> C translation table
 */ 
extern opal_pointer_array_t ompi_info_f_to_c_table;

/**
 * Global instance for MPI_INFO_NULL
 */
OMPI_DECLSPEC extern ompi_predefined_info_t ompi_mpi_info_null;

/**
 * \internal
 * Some declarations needed to use OBJ_NEW and OBJ_DESTRUCT macros
 */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_info_t);

/**
 * \internal
 * Some declarations needed to use OBJ_NEW and OBJ_DESTRUCT macros
 */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_info_entry_t);

/**
 * This function is invoked during ompi_mpi_init() and sets up
 * MPI_Info handling.
 */
int ompi_info_init(void);

/**
 * This functions is called during ompi_mpi_finalize() and shuts
 * down MPI_Info handling.
 */
int ompi_info_finalize(void);

/**
 *   ompi_info_dup - Duplicate an 'MPI_Info' object
 *
 *   @param info source info object (handle)
 *   @param newinfo pointer to the new info object (handle)
 *
 *   @retval MPI_SUCCESS upon success
 *   @retval MPI_ERR_NO_MEM if out of memory
 *
 *   Not only will the (key, value) pairs be duplicated, the order
 *   of keys will be the same in 'newinfo' as it is in 'info'.  When
 *   an info object is no longer being used, it should be freed with
 *   'MPI_Info_free'.
 */
int ompi_info_dup (ompi_info_t *info, ompi_info_t **newinfo);

/*
 * Set a new key,value pair on info.
 *
 * @param info pointer to ompi_info_t object
 * @param key pointer to the new key object
 * @param value pointer to the new value object
 *
 * @retval MPI_SUCCESS upon success
 * @retval MPI_ERR_NO_MEM if out of memory
 */
int ompi_info_set (ompi_info_t *info, char *key, char *value);

/**
 * ompi_info_free - Free an 'MPI_Info' object.
 *
 *   @param info pointer to info (ompi_info_t *) object to be freed (handle)
 *
 *   @retval MPI_SUCCESS
 *   @retval MPI_ERR_ARG
 *
 *   Upon successful completion, 'info' will be set to
 *   'MPI_INFO_NULL'.  Free the info handle and all of its keys and
 *   values.
 */
int ompi_info_free (ompi_info_t **info);

  /**
   *   Get a (key, value) pair from an 'MPI_Info' object and assign it
   *   into a boolen output.
   *
   *   @param info Pointer to ompi_info_t object
   *   @param key null-terminated character string of the index key
   *   @param value Boolean output value
   *   @param flag true (1) if 'key' defined on 'info', false (0) if not
   *               (logical)
   *
   *   @retval MPI_SUCCESS
   *
   *   If found, the string value will be cast to the boolen output in
   *   the following manner:
   *
   *   - If the string value is digits, the return value is "(bool)
   *     atoi(value)"
   *   - If the string value is (case-insensitive) "yes" or "true", the
   *     result is true
   *   - If the string value is (case-insensitive) "no" or "false", the
   *     result is false
   *   - All other values are false
   */
OMPI_DECLSPEC int ompi_info_get_bool (ompi_info_t *info, char *key, bool *value,
                                      int *flag);

/**
 *   Get a (key, value) pair from an 'MPI_Info' object
 *
 *   @param info Pointer to ompi_info_t object
 *   @param key null-terminated character string of the index key
 *   @param valuelen maximum length of 'value' (integer)
 *   @param value null-terminated character string of the value
 *   @param flag true (1) if 'key' defined on 'info', false (0) if not
 *               (logical)
 *
 *   @retval MPI_SUCCESS
 *
 *   In C and C++, 'valuelen' should be one less than the allocated
 *   space to allow for for the null terminator.
 */
OMPI_DECLSPEC int ompi_info_get (ompi_info_t *info, char *key, int valuelen,
                                 char *value, int *flag);

/**
 * Delete a (key,value) pair from "info"
 *
 * @param info ompi_info_t pointer on which we need to operate
 * @param key The key portion of the (key,value) pair that
 *            needs to be deleted
 *
 * @retval MPI_SUCCESS
 * @retval MPI_ERR_NOKEY
 */
int ompi_info_delete (ompi_info_t *info, char *key);

/**
 *   @param info - ompi_info_t pointer object (handle)
 *   @param key - null-terminated character string of the index key
 *   @param valuelen - length of the value associated with 'key' (integer)
 *   @param flag - true (1) if 'key' defined on 'info', false (0) if not
 *   (logical)
 *
 *   @retval MPI_SUCCESS
 *   @retval MPI_ERR_ARG
 *   @retval MPI_ERR_INFO_KEY
 *
 *   The length returned in C and C++ does not include the end-of-string
 *   character.  If the 'key' is not found on 'info', 'valuelen' is left
 *   alone.
 */
OMPI_DECLSPEC int ompi_info_get_valuelen (ompi_info_t *info, char *key, int *valuelen,
                              int *flag);

/**
 *   ompi_info_get_nthkey - Get a key indexed by integer from an 'MPI_Info' o
 *
 *   @param info Pointer to ompi_info_t object
 *   @param n index of key to retrieve (integer)
 *   @param key character string of at least 'MPI_MAX_INFO_KEY' characters
 *
 *   @retval MPI_SUCCESS
 *   @retval MPI_ERR_ARG
 */
int ompi_info_get_nthkey (ompi_info_t *info, int n, char *key);

/**
 * Convert value string to boolean
 *
 * Convert value string \c value into a boolean, using the
 * interpretation rules specified in MPI-2 Section 4.10.  The
 * strings "true", "false", and integer numbers can be converted
 * into booleans.  All others will return \c OMPI_ERR_BAD_PARAM
 *
 * @param value Value string for info key to interpret
 * @param interp returned interpretation of the value key
 *
 * @retval OMPI_SUCCESS string was successfully interpreted
 * @retval OMPI_ERR_BAD_PARAM string was not able to be interpreted
 */
OMPI_DECLSPEC int ompi_info_value_to_bool(char *value, bool *interp);

/**
 * Convert value string to integer
 *
 * Convert value string \c value into a integer, using the
 * interpretation rules specified in MPI-2 Section 4.10.  
 * All others will return \c OMPI_ERR_BAD_PARAM
 *
 * @param value Value string for info key to interpret
 * @param interp returned interpretation of the value key
 *
 * @retval OMPI_SUCCESS string was successfully interpreted
 * @retval OMPI_ERR_BAD_PARAM string was not able to be interpreted
 */
int ompi_info_value_to_int(char *value, int *interp);

END_C_DECLS

/**
 * Return whether this info has been freed already or not.
 *
 * @param info Pointer to ompi_info_t object.
 *
 * @retval true If the info has already been freed
 * @retval false If the info has not yet been freed
 *
 * If the info has been freed, return true.  This will likely only
 * happen in a reliable manner if ompi_debug_handle_never_free is
 * true, in which case an extra OBJ_RETAIN is set on the object during
 * OBJ_NEW, meaning that the user will never be able to actually free
 * the underlying object.  It's a good way to find out if a process is
 * unintentionally using a freed handle.
 */
static inline bool ompi_info_is_freed(ompi_info_t *info)
{
  return info->i_freed;
}


/**
 * Get the number of keys defined on on an MPI_Info object
 * @param info Pointer to ompi_info_t object.
 * @param nkeys Pointer to nkeys, which needs to be filled up.
 *
 * @retval The number of keys defined on info
 */
static inline int 
ompi_info_get_nkeys(ompi_info_t *info, int *nkeys) 
{
    *nkeys = (int) opal_list_get_size(&(info->super));
    return MPI_SUCCESS;
}

#endif /* OMPI_INFO_H */
