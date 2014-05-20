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

#include "ompi_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <errno.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#include <limits.h>
#include <ctype.h>

#include "opal/util/output.h"
#include "opal/util/strncpy.h"
#include "ompi/constants.h"
#include "ompi/info/info.h"
#include "ompi/runtime/params.h"


/*
 * Global variables
 */
ompi_predefined_info_t ompi_mpi_info_null;


/*
 * Local functions
 */
static void info_constructor(ompi_info_t *info);
static void info_destructor(ompi_info_t *info);
static void info_entry_constructor(ompi_info_entry_t *entry);
static void info_entry_destructor(ompi_info_entry_t *entry);
static ompi_info_entry_t *info_find_key (ompi_info_t *info, char *key);


/*
 * ompi_info_t classes
 */
OBJ_CLASS_INSTANCE(ompi_info_t,
                   opal_list_t,
                   info_constructor,
                   info_destructor);

/*
 * ompi_info_entry_t classes
 */
OBJ_CLASS_INSTANCE(ompi_info_entry_t,
                   opal_list_item_t,
                   info_entry_constructor,
                   info_entry_destructor);

/*
 * The global fortran <-> C translation table
 */
opal_pointer_array_t ompi_info_f_to_c_table;

/*
 * This function is called during ompi_init and initializes the
 * fortran to C translation table.
 */ 
int ompi_info_init(void) 
{
    /* initialize table */

    OBJ_CONSTRUCT(&ompi_info_f_to_c_table, opal_pointer_array_t);
    if( OPAL_SUCCESS != opal_pointer_array_init(&ompi_info_f_to_c_table, 0,
                                                OMPI_FORTRAN_HANDLE_MAX, 64) ) {
        return OMPI_ERROR;
    }

    /* Create MPI_INFO_NULL */

    OBJ_CONSTRUCT(&ompi_mpi_info_null.info, ompi_info_t);
    ompi_mpi_info_null.info.i_f_to_c_index = 0;

    /* All done */

    return OMPI_SUCCESS;
}


/*
 * Duplicate an info
 */
int ompi_info_dup (ompi_info_t *info, ompi_info_t **newinfo) 
{
    int err;
    opal_list_item_t *item;
    ompi_info_entry_t *iterator;

    OPAL_THREAD_LOCK(info->i_lock);
    for (item = opal_list_get_first(&(info->super));
         item != opal_list_get_end(&(info->super));
         item = opal_list_get_next(iterator)) {
         iterator = (ompi_info_entry_t *) item;
         err = ompi_info_set(*newinfo, iterator->ie_key, iterator->ie_value);
         if (MPI_SUCCESS != err) {
            OPAL_THREAD_UNLOCK(info->i_lock);
            return err;
         }
     }
    OPAL_THREAD_UNLOCK(info->i_lock);
     return MPI_SUCCESS;
}


/*
 * Set a value on the info
 */
int ompi_info_set (ompi_info_t *info, char *key, char *value) 
{
    char *new_value;
    ompi_info_entry_t *new_info;
    ompi_info_entry_t *old_info;

    new_value = strdup(value);
    if (NULL == new_value) {
      return MPI_ERR_NO_MEM;
    }

    OPAL_THREAD_LOCK(info->i_lock);
    old_info = info_find_key (info, key);
    if (NULL != old_info) {
        /*
         * key already exists. remove the value associated with it
         */
        free(old_info->ie_value);
        old_info->ie_value = new_value;
    } else {
        new_info = OBJ_NEW(ompi_info_entry_t);
        if (NULL == new_info) {
            OPAL_THREAD_UNLOCK(info->i_lock);
            return MPI_ERR_NO_MEM;
        }
        strncpy (new_info->ie_key, key, MPI_MAX_INFO_KEY);
        new_info->ie_value = new_value;
        opal_list_append (&(info->super), (opal_list_item_t *) new_info);
    }
    OPAL_THREAD_UNLOCK(info->i_lock);
    return MPI_SUCCESS;
}


/*
 * Free an info handle and all of its keys and values.
 */
int ompi_info_free (ompi_info_t **info) 
{
    (*info)->i_freed = true;
    OBJ_RELEASE(*info);
    *info = MPI_INFO_NULL;
    return MPI_SUCCESS;
}


/*
 * Get a value from an info
 */
int ompi_info_get (ompi_info_t *info, char *key, int valuelen,
                   char *value, int *flag) 
{
    ompi_info_entry_t *search;
    int value_length;

    OPAL_THREAD_LOCK(info->i_lock);
    search = info_find_key (info, key);
    if (NULL == search){
        *flag = 0;
    } else {
        /*
         * We have found the element, so we can return the value
         * Set the flag, value_length and value
         */
         *flag = 1;
         value_length = strlen(search->ie_value);
         /*
          * If the stored value is shorter than valuelen, then
          * we can copy the entire value out. Else, we have to
          * copy ONLY valuelen bytes out
          */
          if (value_length < valuelen ) {
               strcpy(value, search->ie_value);
          } else {
               opal_strncpy(value, search->ie_value, valuelen);
               value[valuelen] = 0;
          }
    }
    OPAL_THREAD_UNLOCK(info->i_lock);
    return MPI_SUCCESS;
}


/*
 * Similar to ompi_info_get(), but cast the result into a boolean
 * using some well-defined rules.
 */
int ompi_info_get_bool(ompi_info_t *info, char *key, bool *value, int *flag)
{
    char *ptr;
    char str[256];

    str[sizeof(str) - 1] = '\0';
    ompi_info_get(info, key, sizeof(str) - 1, str, flag);
    if (*flag) {
        *value = false;

        /* Trim whitespace */
        ptr = str + sizeof(str) - 1;
        while (ptr >= str && isspace(*ptr)) {
            *ptr = '\0';
            --ptr;
        }
        ptr = str;
        while (ptr < str + sizeof(str) - 1 && *ptr != '\0' && 
               isspace(*ptr)) {
            ++ptr;
        }
        if ('\0' != *ptr) {
            if (isdigit(*ptr)) {
                *value = (bool) atoi(ptr);
            } else if (0 == strcasecmp(ptr, "yes") || 
                       0 == strcasecmp(ptr, "true")) {
                *value = true;
            } else if (0 != strcasecmp(ptr, "no") &&
                       0 != strcasecmp(ptr, "false")) {
                /* RHC unrecognized value -- print a warning? */
            }
        }
    }
    return MPI_SUCCESS;
}

/*
 * Delete a key from an info
 */
int ompi_info_delete (ompi_info_t *info, char *key) 
{
    ompi_info_entry_t *search;

    OPAL_THREAD_LOCK(info->i_lock);
    search = info_find_key (info, key);
    if (NULL == search){
         OPAL_THREAD_UNLOCK(info->i_lock);
         return MPI_ERR_INFO_NOKEY;
    } else {
         /*
          * An entry with this key value was found. Remove the item
          * and free the memory allocated to it.
          * As this key *must* be available, we do not check for errors.
          */
          opal_list_remove_item (&(info->super),
                                 (opal_list_item_t *)search);
          OBJ_RELEASE(search);
    }
    OPAL_THREAD_UNLOCK(info->i_lock);
    return MPI_SUCCESS;
}


/*
 * Return the length of a value
 */
int ompi_info_get_valuelen (ompi_info_t *info, char *key, int *valuelen,
                            int *flag) 
{
    ompi_info_entry_t *search;

    OPAL_THREAD_LOCK(info->i_lock);
    search = info_find_key (info, key);
    if (NULL == search){
        *flag = 0;
    } else {
        /*
         * We have found the element, so we can return the value
         * Set the flag, value_length and value
         */
         *flag = 1;
         *valuelen = strlen(search->ie_value);
    }
    OPAL_THREAD_UNLOCK(info->i_lock);
    return MPI_SUCCESS;
}


/*
 * Get the nth key
 */
int ompi_info_get_nthkey (ompi_info_t *info, int n, char *key)
{
    ompi_info_entry_t *iterator;

    /*
     * Iterate over and over till we get to the nth key
     */
    OPAL_THREAD_LOCK(info->i_lock);
    for (iterator = (ompi_info_entry_t *)opal_list_get_first(&(info->super));
         n > 0;
         --n) {
         iterator = (ompi_info_entry_t *)opal_list_get_next(iterator);
         if (opal_list_get_end(&(info->super)) == 
             (opal_list_item_t *) iterator) {
             OPAL_THREAD_UNLOCK(info->i_lock);
             return MPI_ERR_ARG;
         }
    }
    /*
     * iterator is of the type opal_list_item_t. We have to
     * cast it to ompi_info_entry_t before we can use it to
     * access the value
     */
    strncpy(key, iterator->ie_key, MPI_MAX_INFO_KEY);
    OPAL_THREAD_UNLOCK(info->i_lock);
    return MPI_SUCCESS;
}


/*
 * Shut down MPI_Info handling
 */
int ompi_info_finalize(void) 
{
    size_t i, max;
    ompi_info_t *info;
    opal_list_item_t *item;
    ompi_info_entry_t *entry;
    bool found = false;
    
    /* Release MPI_INFO_NULL.  Do this so that we don't get a bogus
       leak report on it.  Plus, it's statically allocated, so we
       don't want to call OBJ_RELEASE on it. */
    
    OBJ_DESTRUCT(&ompi_mpi_info_null.info);
    opal_pointer_array_set_item(&ompi_info_f_to_c_table, 0, NULL);
    
    /* Go through the f2c table and see if anything is left.  Free them
       all. */
    
    max = opal_pointer_array_get_size(&ompi_info_f_to_c_table);
    for (i = 0; i < max; ++i) {
        info = (ompi_info_t *)opal_pointer_array_get_item(&ompi_info_f_to_c_table, i);
        
        /* If the info was freed but still exists because the user
           told us to never free handles, then do an OBJ_RELEASE it
           and all is well.  Then get the value again and see if it's
           actually been freed. */
        
        if (NULL != info && ompi_debug_no_free_handles && info->i_freed) {
            OBJ_RELEASE(info);
            info = (ompi_info_t *)opal_pointer_array_get_item(&ompi_info_f_to_c_table, i);
        } 
        
        /* If it still exists here and was never freed, then it's an
           orphan */
        
        if (NULL != info) {
            
            /* If the user wanted warnings about MPI object leaks, print out
               a message */
            
            if (!info->i_freed && ompi_debug_show_handle_leaks) {
                if (ompi_debug_show_handle_leaks) {
                    opal_output(0, "WARNING: MPI_Info still allocated at MPI_FINALIZE");
                    for (item = opal_list_get_first(&(info->super));
                         opal_list_get_end(&(info->super)) != item;
                         item = opal_list_get_next(item)) {
                        entry = (ompi_info_entry_t *) item;
                        opal_output(0, "WARNING:   key=\"%s\", value=\"%s\"", 
                                    entry->ie_key,
                                    NULL != entry->ie_value ? entry->ie_value : "(null)");
                        found = true;
                    }
                }
                OBJ_RELEASE(info);
            }
            
            /* Don't bother setting each element back down to NULL; it
               would just take a lot of thread locks / unlocks and
               since we're destroying everything, it isn't worth it */

            if (!found && ompi_debug_show_handle_leaks) {
                opal_output(0, "WARNING:   (no keys)");
            }
        }
    }
  
    /* All done -- destroy the table */

    OBJ_DESTRUCT(&ompi_info_f_to_c_table);
    return OMPI_SUCCESS;
}


/*
 * This function is invoked when OBJ_NEW() is called. Here, we add this
 * info pointer to the table and then store its index as the handle
 */
static void info_constructor(ompi_info_t *info) 
{
    info->i_f_to_c_index = opal_pointer_array_add(&ompi_info_f_to_c_table, 
                                                  info);
    info->i_lock = OBJ_NEW(opal_mutex_t);
    info->i_freed = false;

    /* If the user doesn't want us to ever free it, then add an extra
       RETAIN here */

    if (ompi_debug_no_free_handles) {
        OBJ_RETAIN(&(info->super));
    }
}


/*
 * This function is called during OBJ_DESTRUCT of "info". When this 
 * done, we need to remove the entry from the ompi fortran to C 
 * translation table
 */ 
static void info_destructor(ompi_info_t *info) 
{
    opal_list_item_t *item;
    ompi_info_entry_t *iterator;

    /* Remove every key in the list */
  
    for (item = opal_list_remove_first(&(info->super));
         NULL != item;
         item = opal_list_remove_first(&(info->super))) {
        iterator = (ompi_info_entry_t *) item;
        OBJ_RELEASE(iterator);
    }

    /* reset the &ompi_info_f_to_c_table entry - make sure that the
       entry is in the table */
    
    if (MPI_UNDEFINED != info->i_f_to_c_index &&
        NULL != opal_pointer_array_get_item(&ompi_info_f_to_c_table, 
                                            info->i_f_to_c_index)){
        opal_pointer_array_set_item(&ompi_info_f_to_c_table, 
                                    info->i_f_to_c_index, NULL);
    }

    /* Release the lock */

    OBJ_RELEASE(info->i_lock);
}


/*
 * ompi_info_entry_t interface functions
 */
static void info_entry_constructor(ompi_info_entry_t *entry) 
{
    memset(entry->ie_key, 0, sizeof(entry->ie_key));
    entry->ie_key[MPI_MAX_INFO_KEY] = 0;
}


static void info_entry_destructor(ompi_info_entry_t *entry) 
{
    if (NULL != entry->ie_value) {
        free(entry->ie_value);
    }
}


/*
 * Find a key
 *
 * Do NOT thread lock in here -- the calling function is responsible
 * for that.
 */
static ompi_info_entry_t *info_find_key (ompi_info_t *info, char *key)
{
    ompi_info_entry_t *iterator;

    /* No thread locking in here! */

    /* Iterate over all the entries. If the key is found, then 
     * return immediately. Else, the loop will fall of the edge
     * and NULL is returned
     */
    for (iterator = (ompi_info_entry_t *)opal_list_get_first(&(info->super));
         opal_list_get_end(&(info->super)) != (opal_list_item_t*) iterator;
         iterator = (ompi_info_entry_t *)opal_list_get_next(iterator)) {
        if (0 == strcmp(key, iterator->ie_key)) {
            return iterator;
        }
    }
    return NULL;
}


int
ompi_info_value_to_int(char *value, int *interp)
{
    long tmp;
    char *endp;

    if (NULL == value || '\0' == value[0]) return OMPI_ERR_BAD_PARAM;

    errno = 0;
    tmp = strtol(value, &endp, 10);
    /* we found something not a number */
    if (*endp != '\0') return OMPI_ERR_BAD_PARAM;
    /* underflow */
    if (tmp == 0 && errno == EINVAL) return OMPI_ERR_BAD_PARAM;

    *interp = (int) tmp;

    return OMPI_SUCCESS;
}


int
ompi_info_value_to_bool(char *value, bool *interp)
{
    int tmp;

    /* idiot case */
    if (NULL == value || NULL == interp) return OMPI_ERR_BAD_PARAM;

    /* is it true / false? */
    if (0 == strcmp(value, "true")) {
        *interp = true;
        return OMPI_SUCCESS;
    } else if (0 == strcmp(value, "false")) {
        *interp = false;
        return OMPI_SUCCESS;

    /* is it a number? */
    } else if (OMPI_SUCCESS == ompi_info_value_to_int(value, &tmp)) {
        if (tmp == 0) {
            *interp = false;
        } else {
            *interp = true;
        } 
        return OMPI_SUCCESS;
    }

    return OMPI_ERR_BAD_PARAM;
}

