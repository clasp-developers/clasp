/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/os_path.h"
#include "opal/util/path.h"
#include "opal/class/opal_value_array.h"
#include "opal/util/show_help.h"
#include "opal/class/opal_hash_table.h"
#include "opal/util/printf.h"
#include "opal/util/argv.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/base/mca_base_param_internal.h"
#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"

/*
 * Local types
 */

typedef struct {
    /* Base class */
    opal_list_item_t super;
    
    /* String of the type name or NULL */
    char *si_type_name;
    /* String of the component name */
    char *si_component_name;
    /* String of the param name */
    char *si_param_name;
    /* Full name of the synonym */
    char *si_full_name;
    /* Name of the synonym's corresponding environment variable */
    char *si_env_var_name;

    /* Whether this synonym is a deprecated name or not */
    bool si_deprecated;
    /* Whether we've shown a warning that this synonym has been
       displayed or not */
    bool si_deprecated_warning_shown;
} syn_info_t;


/*
 * Public variables
 *
 * This variable is public, but not advertised in mca_base_param.h.
 * It's only public so that the file parser can see it.
 */
opal_list_t mca_base_param_file_values;

/*
 * local variables
 */
static opal_value_array_t mca_base_params;
static const char *mca_prefix = "OMPI_MCA_";
static char *home = NULL;
static char *cwd  = NULL;
static bool initialized = false;
static char * force_agg_path = NULL;

/*
 * local functions
 */
#if defined(__WINDOWS__)
static int read_keys_from_registry(HKEY hKey, char *sub_key, char *current_name);
#endif  /* defined(__WINDOWS__) */
static int fixup_files(char **file_list, char * path, bool rel_path_search);
static int read_files(char *file_list);
static int param_register(const char *type_name,
                          const char *component_name,
                          const char *param_name,
                          const char *help_msg,
                          mca_base_param_type_t type,
                          bool internal,
                          bool read_only,
                          mca_base_param_storage_t *default_value,
                          mca_base_param_storage_t *file_value,
                          mca_base_param_storage_t *override_value,
                          mca_base_param_storage_t *current_value);
static int syn_register(int index_orig, const char *syn_type_name,
                        const char *syn_component_name,
                        const char *syn_param_name, bool deprecated);
static bool param_lookup(size_t index, mca_base_param_storage_t *storage,
                         opal_hash_table_t *attrs,
                         mca_base_param_source_t *source,
                         char **source_file);
static bool param_set_override(size_t index, 
                               mca_base_param_storage_t *storage,
                               mca_base_param_type_t type);
static bool lookup_override(mca_base_param_t *param,
                            mca_base_param_storage_t *storage);
static bool lookup_env(mca_base_param_t *param,
                       mca_base_param_storage_t *storage);
static bool lookup_file(mca_base_param_t *param,
                        mca_base_param_storage_t *storage,
                        char **source_file);
static bool lookup_default(mca_base_param_t *param,
                           mca_base_param_storage_t *storage);
static bool set(mca_base_param_type_t type,
                mca_base_param_storage_t *dest, mca_base_param_storage_t *src);
static void param_constructor(mca_base_param_t *p);
static void param_destructor(mca_base_param_t *p);
static void fv_constructor(mca_base_param_file_value_t *p);
static void fv_destructor(mca_base_param_file_value_t *p);
static void info_constructor(mca_base_param_info_t *p);
static void info_destructor(mca_base_param_info_t *p);
static void syn_info_constructor(syn_info_t *si);
static void syn_info_destructor(syn_info_t *si);

/*
 * Make the class instance for mca_base_param_t
 */
OBJ_CLASS_INSTANCE(mca_base_param_t, opal_object_t, 
                   param_constructor, param_destructor);
OBJ_CLASS_INSTANCE(mca_base_param_file_value_t, opal_list_item_t,
                   fv_constructor, fv_destructor);
OBJ_CLASS_INSTANCE(mca_base_param_info_t, opal_list_item_t,
                   info_constructor, info_destructor);
OBJ_CLASS_INSTANCE(syn_info_t, opal_list_item_t,
                   syn_info_constructor, syn_info_destructor);

/*
 * Set it up
 */
int mca_base_param_init(void)
{
    if (!initialized) {

        /* Init the value array for the param storage */

        OBJ_CONSTRUCT(&mca_base_params, opal_value_array_t);
        opal_value_array_init(&mca_base_params, sizeof(mca_base_param_t));

        /* Init the file param value list */

        OBJ_CONSTRUCT(&mca_base_param_file_values, opal_list_t);

        /* Set this before we register the parameter, below */

        initialized = true; 

        mca_base_param_recache_files(false);
    }

    return OPAL_SUCCESS;
}

int mca_base_param_recache_files(bool rel_path_search)
{
    int id;
    char *files, *new_files = NULL, *new_agg_files = NULL;
    char * new_agg_path = NULL, *agg_default_path = NULL;

    /* We may need this later */
    home = (char*)opal_home_directory();
    
    if(NULL == cwd) {
        cwd = (char *) malloc(sizeof(char) * MAXPATHLEN);
        if( NULL == (cwd = getcwd(cwd, MAXPATHLEN) )) {
            opal_output(0, "Error: Unable to get the current working directory\n");
            cwd = strdup(".");
        }
    }

#if OPAL_WANT_HOME_CONFIG_FILES
    asprintf(&files,
             "%s"OPAL_PATH_SEP".openmpi"OPAL_PATH_SEP"mca-params.conf%c%s"OPAL_PATH_SEP"openmpi-mca-params.conf",
             home, OPAL_ENV_SEP, opal_install_dirs.sysconfdir);
#else
    asprintf(&files,
             "%s"OPAL_PATH_SEP"openmpi-mca-params.conf",
             opal_install_dirs.sysconfdir);
#endif

    /* Initialize a parameter that says where MCA param files can
       be found */

    id = mca_base_param_reg_string_name("mca", "param_files",
                                        "Path for MCA configuration files containing default parameter values",
                                        false, false, files, &new_files);

    /* Aggregate MCA parameter files
     * A prefix search path to look up aggregate MCA parameter file
     * requests that do not specify an absolute path
     */
    id = mca_base_param_reg_string_name("mca", "base_param_file_prefix",
                                        "Aggregate MCA parameter file sets",
                                        false, false, NULL, &new_agg_files);
    
    asprintf(&agg_default_path,
             "%s"OPAL_PATH_SEP"amca-param-sets%c%s",
             opal_install_dirs.pkgdatadir, OPAL_ENV_SEP, cwd);
    id = mca_base_param_reg_string_name("mca", "base_param_file_path",
                                        "Aggregate MCA parameter Search path",
                                        false, false, agg_default_path, &new_agg_path);

    id = mca_base_param_reg_string_name("mca", "base_param_file_path_force",
                                        "Forced Aggregate MCA parameter Search path",
                                        false, false, NULL, &force_agg_path);

    if( NULL != force_agg_path ) {
        char *tmp_str = NULL;
        if( NULL == new_agg_path ) {
            new_agg_path = strdup(force_agg_path);
        }
        else {
            tmp_str = strdup(new_agg_path);
            free(new_agg_path);
            asprintf(&new_agg_path, "%s%c%s", force_agg_path, OPAL_ENV_SEP, tmp_str);
            free(tmp_str);
        }
    }

    if( NULL != new_agg_files ) {
        char *tmp_str = NULL;
        
        /*
         * Resolve all relative paths.
         * the file list returned will contain only absolute paths
         */
        if( OPAL_SUCCESS != fixup_files(&new_agg_files, new_agg_path, rel_path_search) ) {
#if 0
            /* JJH We need to die! */
            abort();
#else
            ;
#endif
        }
        else {
            /* Prepend the files to the search list */
            asprintf(&tmp_str, "%s%c%s", new_agg_files, OPAL_ENV_SEP, new_files);
            free(new_files);
            new_files = strdup(tmp_str);
            free(tmp_str);
        }
    }
    
    read_files(new_files);
#if defined(__WINDOWS__)
    read_keys_from_registry(HKEY_LOCAL_MACHINE, "SOFTWARE\\Open MPI", NULL);
    read_keys_from_registry(HKEY_CURRENT_USER, "SOFTWARE\\Open MPI", NULL);
#endif  /* defined(__WINDOWS__) */
    
    free(files);
    free(new_files);
    if( NULL != new_agg_files ) {
        free(new_agg_files);
        new_agg_files = NULL;
    }
    if( NULL != agg_default_path ) {
        free(agg_default_path);
        agg_default_path = NULL;
    }
    if( NULL != new_agg_path ) {
        free(new_agg_path);
        new_agg_path = NULL;
    }

    return OPAL_SUCCESS;
}


/*
 * Register an integer MCA parameter 
 */
int mca_base_param_reg_int(const mca_base_component_t *component,
                           const char *param_name, 
                           const char *help_msg,
                           bool internal,
                           bool read_only,
                           int default_value,
                           int *current_value)
{
    int ret;
    mca_base_param_storage_t storage;
    mca_base_param_storage_t lookup;

    storage.intval = default_value;
    ret = param_register(component->mca_type_name, 
                         component->mca_component_name, 
                         param_name, help_msg, 
                         MCA_BASE_PARAM_TYPE_INT, internal, read_only,
                         &storage, NULL, NULL, &lookup);
    if (ret >= 0 && NULL != current_value) {
        *current_value = lookup.intval;
    }
    return ret;
}


/*
 * Register an integer MCA parameter that is not associated with a
 * component
 */
int mca_base_param_reg_int_name(const char *type,
                                const char *param_name, 
                                const char *help_msg,
                                bool internal,
                                bool read_only,
                                int default_value,
                                int *current_value)
{
    int ret;
    mca_base_param_storage_t storage;
    mca_base_param_storage_t lookup;

    storage.intval = default_value;
    ret = param_register(type, NULL, param_name, help_msg, 
                         MCA_BASE_PARAM_TYPE_INT, internal, read_only,
                         &storage, NULL, NULL, &lookup);
    if (ret >= 0 && NULL != current_value) {
        *current_value = lookup.intval;
    }
    return ret;
}


/*
 * Register a string MCA parameter.
 */
int mca_base_param_reg_string(const mca_base_component_t *component,
                              const char *param_name, 
                              const char *help_msg,
                              bool internal,
                              bool read_only,
                              const char *default_value,
                              char **current_value)
{
    int ret;
    mca_base_param_storage_t storage;
    mca_base_param_storage_t lookup;
    
    if (NULL != default_value) {
        storage.stringval = (char *) default_value;
    } else {
        storage.stringval = NULL;
    }
    ret = param_register(component->mca_type_name,
                         component->mca_component_name,
                         param_name, help_msg, 
                         MCA_BASE_PARAM_TYPE_STRING, internal, read_only,
                         &storage, NULL, NULL, 
                         (NULL != current_value) ? &lookup : NULL);
    if (ret >= 0 && NULL != current_value) {
        *current_value = lookup.stringval;
    }
    return ret;
}


/*
 * Register a string MCA parameter that is not associated with a
 * component
 */
int mca_base_param_reg_string_name(const char *type,
                                   const char *param_name, 
                                   const char *help_msg,
                                   bool internal,
                                   bool read_only,
                                   const char *default_value,
                                   char **current_value)
{
    int ret;
    mca_base_param_storage_t storage;
    mca_base_param_storage_t lookup;
    
    if (NULL != default_value) {
        storage.stringval = (char *) default_value;
    } else {
        storage.stringval = NULL;
    }
    ret = param_register(type, NULL, param_name, help_msg, 
                         MCA_BASE_PARAM_TYPE_STRING, internal, read_only,
                         &storage, NULL, NULL, 
                         (NULL != current_value) ? &lookup : NULL);
    if (ret >= 0 && NULL != current_value) {
        *current_value = lookup.stringval;
    }
    return ret;
}


/*
 * Register an integer MCA parameter 
 * (deprecated)
 */
int mca_base_param_register_int(const char *type_name, 
                                const char *component_name,
                                const char *param_name, 
                                const char *mca_param_name, 
                                int default_value)
{
    int ret;
    mca_base_param_storage_t storage;

    storage.intval = default_value;
    ret = param_register(type_name, component_name, param_name, mca_param_name,
                         MCA_BASE_PARAM_TYPE_INT, false, false,
                         &storage, NULL, NULL, NULL);
    return ret;
}


/*
 * Register a string MCA parameter.
 * (deprecated)
 */
int mca_base_param_register_string(const char *type_name, 
                                   const char *component_name,
                                   const char *param_name, 
                                   const char *mca_param_name,
                                   const char *default_value)
{
    int ret;
    mca_base_param_storage_t storage;

    if (NULL != default_value) {
        storage.stringval = (char *) default_value;
    } else {
        storage.stringval = NULL;
    }
    ret = param_register(type_name, component_name, param_name, mca_param_name,
                         MCA_BASE_PARAM_TYPE_STRING, false, false,
                         &storage, NULL, NULL, NULL);
    return ret;
}


/*
 * Register a synonym name for an existing MCA parameter
 */
int mca_base_param_reg_syn(int index_orig,
                           const mca_base_component_t *syn_component,
                           const char *syn_param_name, bool deprecated)
{
    return syn_register(index_orig,
                        syn_component->mca_type_name,
                        syn_component->mca_component_name,
                        syn_param_name, deprecated);
}

/*
 * Register a synonym name for an existing MCA parameter
 */
int mca_base_param_reg_syn_name(int index_orig,
                                const char *syn_type_name,
                                const char *syn_param_name, bool deprecated)
{
    return syn_register(index_orig, syn_type_name, NULL,
                        syn_param_name, deprecated);
}

/*
 * Look up an integer MCA parameter.
 */
int mca_base_param_lookup_int(int index, int *value)
{
  mca_base_param_storage_t storage;
  
  if (param_lookup(index, &storage, NULL, NULL, NULL)) {
    *value = storage.intval;
    return OPAL_SUCCESS;
  }
  return OPAL_ERROR;
}


/*
 * Set an integer parameter
 */
int mca_base_param_set_int(int index, int value)
{
    mca_base_param_storage_t storage;

    mca_base_param_unset(index);
    storage.intval = value;
    param_set_override(index, &storage, MCA_BASE_PARAM_TYPE_INT);
    return OPAL_SUCCESS;
}

/*
 * Deregister a parameter
 */
int mca_base_param_deregister(int index)
{
    size_t size;

    /* Lookup the index and see if it's valid */
    size = opal_value_array_get_size(&mca_base_params);
    if (index < 0 || ((size_t) index) > size) {
        return OPAL_ERROR;
    }

    return opal_value_array_remove_item(&mca_base_params, index);
}

/*
 * Look up a string MCA parameter.
 */
int mca_base_param_lookup_string(int index, char **value)
{
  mca_base_param_storage_t storage;
  
  if (param_lookup(index, &storage, NULL, NULL, NULL)) {
    *value = storage.stringval;
    return OPAL_SUCCESS;
  }
  return OPAL_ERROR;
}


/*
 * Set an string parameter
 */
int mca_base_param_set_string(int index, char *value)
{
    mca_base_param_storage_t storage;

    mca_base_param_unset(index);
    storage.stringval = value;
    param_set_override(index, &storage, MCA_BASE_PARAM_TYPE_STRING);
    return OPAL_SUCCESS;
}


/*
 * Lookup the source of an MCA param's value
 */
int mca_base_param_lookup_source(int index, mca_base_param_source_t *source, char **source_file)
{
    mca_base_param_storage_t storage;
  
    if (param_lookup(index, &storage, NULL, source, source_file)) {
        return OPAL_SUCCESS;
    }
    return OPAL_ERROR;
}

/*
 * Unset a parameter
 */
int mca_base_param_unset(int index)
{
    size_t len;
    mca_base_param_t *array;

    if (!initialized) {
        return OPAL_ERROR;
    }

    len = opal_value_array_get_size(&mca_base_params);
    if (index < 0 || ((size_t) index) > len) {
        return OPAL_ERROR;
    }

    /* We have a valid entry (remember that we never delete MCA
       parameters, so if the index is >0 and <len, it must be good),
       so save the internal flag */

    array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
    if (array[index].mbp_override_value_set) {
        if (MCA_BASE_PARAM_TYPE_STRING == array[index].mbp_type &&
            NULL != array[index].mbp_override_value.stringval) {
            free(array[index].mbp_override_value.stringval);
            array[index].mbp_override_value.stringval = NULL;
        }
    }
    array[index].mbp_override_value_set = false;
  
    /* All done */

    return OPAL_SUCCESS;
}


char *mca_base_param_env_var(const char *param_name)
{
    char *name;

    asprintf(&name, "%s%s", mca_prefix, param_name);

    return name;
}

/*
 * Make a string suitable for the environment, setting an MCA param
 */
char *mca_base_param_environ_variable(const char *type,
                                      const char *component,
                                      const char *param)
{
    size_t len;
    int id;
    char *ret = NULL, *name;
    mca_base_param_t *array;

    if (NULL == type) {
        return NULL;
    }

    id = mca_base_param_find(type, component, param);
    if (OPAL_ERROR != id) {
        array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
        ret = strdup(array[id].mbp_env_var_name);
    } else {
        len = strlen(mca_prefix) + strlen(type) + 16;
        if (NULL != component) {
            len += strlen(component);
        }
        if (NULL != param) {
            len += strlen(param);
        }
        name = (char*)malloc(len);
        if (NULL == name) {
            return NULL;
        }
        name[0] = '\0';
        snprintf(name, len, "%s%s", mca_prefix, type);
        if (NULL != component) {
            strcat(name, "_");
            strcat(name, component);
        }
        if (NULL != param) {
            strcat(name, "_");
            strcat(name, param);
        }
        ret = name;
    }

    /* All done */

    return ret;
}


/*
 * Find the index for an MCA parameter based on its names.
 */
int mca_base_param_find(const char *type_name, const char *component_name, 
                        const char *param_name) 
{
  size_t i, size;
  mca_base_param_t *array;

  /* Check for bozo cases */

  if (!initialized) {
    return OPAL_ERROR;
  }

  /* Loop through looking for a parameter of a given
     type/component/param */

  size = opal_value_array_get_size(&mca_base_params);
  array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
  for (i = 0; i < size; ++i) {
    if (((NULL == type_name && NULL == array[i].mbp_type_name) ||
         (NULL != type_name && NULL != array[i].mbp_type_name &&
          (0 == strcmp(type_name, array[i].mbp_type_name)))) &&
        ((NULL == component_name && NULL == array[i].mbp_component_name) ||
         (NULL != component_name && NULL != array[i].mbp_component_name &&
          0 == strcmp(component_name, array[i].mbp_component_name))) &&
        ((NULL == param_name && NULL == array[i].mbp_param_name) ||
         (NULL != param_name && NULL != array[i].mbp_param_name &&
          0 == strcmp(param_name, array[i].mbp_param_name)))) {
      return (int)i;
    }
  }

  /* Didn't find it */

  return OPAL_ERROR;
}


int mca_base_param_set_internal(int index, bool internal)
{
    size_t len;
    mca_base_param_t *array;

    /* Check for bozo cases */
    
    if (!initialized) {
        return OPAL_ERROR;
    }

    len = opal_value_array_get_size(&mca_base_params);
    if (((size_t) index) > len) {
        return OPAL_ERROR;
    }

    /* We have a valid entry (remember that we never delete MCA
       parameters, so if the index is >0 and <len, it must be good),
       so save the internal flag */

    array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
    array[index].mbp_internal = internal;
  
    /* All done */

    return OPAL_SUCCESS;
}


/*
 * Return a list of info of all currently registered parameters
 */
int mca_base_param_dump(opal_list_t **info, bool internal)
{
    size_t i, j, len;
    mca_base_param_info_t *p, *q;
    mca_base_param_t *array;
    opal_list_item_t *item;
    syn_info_t *si;

    /* Check for bozo cases */
    
    if (!initialized) {
        return OPAL_ERROR;
    }

    if (NULL == info) {
        return OPAL_ERROR;
    }
    *info = OBJ_NEW(opal_list_t);

    /* Iterate through all the registered parameters */

    len = opal_value_array_get_size(&mca_base_params);
    array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
    for (i = 0; i < len; ++i) {
        if (array[i].mbp_internal == internal || internal) {
            p = OBJ_NEW(mca_base_param_info_t);
            if (NULL == p) {
                return OPAL_ERR_OUT_OF_RESOURCE;
            }
            p->mbpp_index = (int)i;
            p->mbpp_type_name = array[i].mbp_type_name;
            p->mbpp_component_name = array[i].mbp_component_name;
            p->mbpp_param_name = array[i].mbp_param_name;
            p->mbpp_full_name = array[i].mbp_full_name;
            p->mbpp_deprecated = array[i].mbp_deprecated;
            p->mbpp_internal = array[i].mbp_internal;
            p->mbpp_read_only = array[i].mbp_read_only;
            p->mbpp_type = array[i].mbp_type;
            p->mbpp_help_msg = array[i].mbp_help_msg;

            /* Save this entry to the list */
            opal_list_append(*info, (opal_list_item_t*) p);

            /* If this param has synonyms, add them too */
            if (NULL != array[i].mbp_synonyms &&
                !opal_list_is_empty(array[i].mbp_synonyms)) {
                p->mbpp_synonyms_len = 
                    (int) opal_list_get_size(array[i].mbp_synonyms);
                p->mbpp_synonyms = malloc(sizeof(mca_base_param_info_t*) *
                                          p->mbpp_synonyms_len);
                if (NULL == p->mbpp_synonyms) {
                    p->mbpp_synonyms_len = 0;
                    return OPAL_ERR_OUT_OF_RESOURCE;
                }
                
                for (j = 0, item = opal_list_get_first(array[i].mbp_synonyms);
                     opal_list_get_end(array[i].mbp_synonyms) != item;
                     ++j, item = opal_list_get_next(item)) {
                    si = (syn_info_t*) item;
                    q = OBJ_NEW(mca_base_param_info_t);
                    if (NULL == q) {
                        return OPAL_ERR_OUT_OF_RESOURCE;
                    }
                    q->mbpp_index = (int)i;
                    q->mbpp_type_name = si->si_type_name;
                    q->mbpp_component_name = si->si_component_name;
                    q->mbpp_param_name = si->si_param_name;
                    q->mbpp_full_name = si->si_full_name;
                    q->mbpp_deprecated = si->si_deprecated ||
                        array[i].mbp_deprecated;
                    q->mbpp_internal = array[i].mbp_internal;
                    q->mbpp_read_only = array[i].mbp_read_only;
                    q->mbpp_type = array[i].mbp_type;
                    q->mbpp_help_msg = array[i].mbp_help_msg;

                    /* Let this one point to the original */
                    q->mbpp_synonym_parent = p;

                    /* Let the original point to this one */
                    p->mbpp_synonyms[j] = q;

                    /* Save this entry to the list */
                    opal_list_append(*info, (opal_list_item_t*) q);
                }
            }
        }
    }

    /* All done */

    return OPAL_SUCCESS;
}


/*
 * Make an argv-style list of strings suitable for an environment
 */
int mca_base_param_build_env(char ***env, int *num_env, bool internal)
{
    size_t i, len;
    mca_base_param_t *array;
    char *str;
    mca_base_param_storage_t storage;

    /* Check for bozo cases */
    
    if (!initialized) {
        return OPAL_ERROR;
    }

    /* Iterate through all the registered parameters */

    len = opal_value_array_get_size(&mca_base_params);
    array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
    for (i = 0; i < len; ++i) {
        /* Don't output read-only values */
        if (array[i].mbp_read_only) {
            continue;
        }

        if (array[i].mbp_internal == internal || internal) {
            if (param_lookup(i, &storage, NULL, NULL, NULL)) {
                if (MCA_BASE_PARAM_TYPE_INT == array[i].mbp_type) {
                    asprintf(&str, "%s=%d", array[i].mbp_env_var_name, 
                             storage.intval);
                    opal_argv_append(num_env, env, str);
                    free(str);
                } else if (MCA_BASE_PARAM_TYPE_STRING == array[i].mbp_type) {
                    if (NULL != storage.stringval) {
                        asprintf(&str, "%s=%s", array[i].mbp_env_var_name, 
                                 storage.stringval);
                        free(storage.stringval);
                        opal_argv_append(num_env, env, str);
                        free(str);
                    } 
                } else {
                    goto cleanup;
                }
            } else {
                goto cleanup;
            }
        }
    }

    /* All done */

    return OPAL_SUCCESS;

    /* Error condition */

 cleanup:
    if (*num_env > 0) {
        opal_argv_free(*env);
        *num_env = 0;
        *env = NULL;
    }
    return OPAL_ERR_NOT_FOUND;
}


/*
 * Free a list -- and all associated memory -- that was previously
 * returned from mca_base_param_dump()
 */
int mca_base_param_dump_release(opal_list_t *info)
{
    opal_list_item_t *item;

    for (item = opal_list_remove_first(info); NULL != item;
         item = opal_list_remove_first(info)) {
        OBJ_RELEASE(item);
    }
    OBJ_RELEASE(info);

    return OPAL_SUCCESS;
}


/*
 * Shut down the MCA parameter system (normally only invoked by the
 * MCA framework itself).
 */
int mca_base_param_finalize(void)
{
    opal_list_item_t *item;
    mca_base_param_t *array;

    if (initialized) {

        /* This is slow, but effective :-) */

        array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
        while (opal_value_array_get_size(&mca_base_params) > 0) {
            OBJ_DESTRUCT(&array[0]);
            opal_value_array_remove_item(&mca_base_params, 0);
        }
        OBJ_DESTRUCT(&mca_base_params);

        for (item = opal_list_remove_first(&mca_base_param_file_values);
             NULL != item;
             item = opal_list_remove_first(&mca_base_param_file_values)) {
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&mca_base_param_file_values);

        if( NULL != cwd ) {
            free(cwd);
            cwd = NULL;
        }

        if( NULL != force_agg_path ) {
            free(force_agg_path);
            force_agg_path = NULL;
        }

        initialized = false;
    }

    /* All done */

    return OPAL_SUCCESS;
}


/*************************************************************************/
static int fixup_files(char **file_list, char * path, bool rel_path_search) {
    int exit_status = OPAL_SUCCESS;
    char **files = NULL;
    char **search_path = NULL;
    char * tmp_file = NULL;
    char **argv = NULL;
    int mode = R_OK; /* The file exists, and we can read it */
    int count, i, argc = 0;

    search_path = opal_argv_split(path, OPAL_ENV_SEP);
    files = opal_argv_split(*file_list, OPAL_ENV_SEP);
    count = opal_argv_count(files);

    /* Read in reverse order, so we can preserve the original ordering */
    for (i = 0 ; i < count; ++i) {
        /* Absolute paths preserved */
        if ( opal_path_is_absolute(files[i]) ) {
            if( NULL == opal_path_access(files[i], NULL, mode) ) {
                opal_show_help("help-mca-param.txt", "missing-param-file",
                               true, getpid(), files[i], path);
                exit_status = OPAL_ERROR;
                goto cleanup;
            }
            else {
                opal_argv_append(&argc, &argv, files[i]);
            }
        }
        /* Resolve all relative paths:
         *  - If filename contains a "/" (e.g., "./foo" or "foo/bar")
         *    - look for it relative to cwd
         *    - if exists, use it
         *    - ow warn/error
         */
        else if (!rel_path_search && NULL != strchr(files[i], OPAL_PATH_SEP[0]) ) {
            if( NULL != force_agg_path ) {
                tmp_file = opal_path_access(files[i], force_agg_path, mode);
            }
            else {
                tmp_file = opal_path_access(files[i], cwd, mode);
            }

            if( NULL == tmp_file ) {
                opal_show_help("help-mca-param.txt", "missing-param-file",
                               true, getpid(), files[i], cwd);
                exit_status = OPAL_ERROR;
                goto cleanup;
            }
            else {
                opal_argv_append(&argc, &argv, tmp_file);
            }
        }
        /* Resolve all relative paths:
         * - Use path resolution
         *    - if found and readable, use it
         *    - otherwise, warn/error
         */
        else {
            if( NULL != (tmp_file = opal_path_find(files[i], search_path, mode, NULL)) ) {
                opal_argv_append(&argc, &argv, tmp_file);
                free(tmp_file);
                tmp_file = NULL;
            }
            else {
                opal_show_help("help-mca-param.txt", "missing-param-file",
                               true, getpid(), files[i], path);
                exit_status = OPAL_ERROR;
                goto cleanup;
            }
        }
    }

    free(*file_list);
    *file_list = opal_argv_join(argv, OPAL_ENV_SEP);

 cleanup:
    if( NULL != files ) {
        opal_argv_free(files);
        files = NULL;
    }
    if( NULL != argv ) {
        opal_argv_free(argv);
        argv = NULL;
    }
    if( NULL != search_path ) {
        opal_argv_free(search_path);
        search_path = NULL;
    }
    if( NULL != tmp_file ) {
        free(tmp_file);
        tmp_file = NULL;
    }

    return exit_status;
}

static int read_files(char *file_list)
{
    int i, count;
    char **files;

    /* Iterate through all the files passed in -- read them in reverse
       order so that we preserve unix/shell path-like semantics (i.e.,
       the entries farthest to the left get precedence) */

    files = opal_argv_split(file_list, OPAL_ENV_SEP);
    count = opal_argv_count(files);

    for (i = count - 1; i >= 0; --i) {
        mca_base_parse_paramfile(files[i]);
    }
    opal_argv_free(files);

    return OPAL_SUCCESS;
}

/**
 *
 */
#if defined(__WINDOWS__)
#define MAX_KEY_LENGTH 255
#define MAX_VALUE_NAME 16383

static int read_keys_from_registry(HKEY hKey, char *sub_key, char *current_name)
{       
    TCHAR   achKey[MAX_KEY_LENGTH];        /* buffer for subkey name */
    DWORD   cbName;                        /* size of name string */
    TCHAR   achClass[MAX_PATH] = TEXT(""); /* buffer for class name */
    DWORD   cchClassName = MAX_PATH;       /* size of class string */
    DWORD   cSubKeys=0;                    /* number of subkeys */
    DWORD   cbMaxSubKey;                   /* longest subkey size */
    DWORD   cchMaxClass;                   /* longest class string */
    DWORD   cValues;                       /* number of values for key */
    DWORD   cchMaxValue;                   /* longest value name */
    DWORD   cbMaxValueData;                /* longest value data */
    DWORD   cbSecurityDescriptor;          /* size of security descriptor */

    LPDWORD lpType;
    LPDWORD word_lpData;
    TCHAR   str_lpData[MAX_VALUE_NAME];
    TCHAR   *str_key_name, *type_name, *next_name;
    DWORD   dwSize, i, retCode, type_len, param_type;
    TCHAR achValue[MAX_VALUE_NAME];
    DWORD cchValue = MAX_VALUE_NAME;
    HKEY hTestKey; 
    char *sub_sub_key;
    mca_base_param_storage_t storage, override, lookup;
      
    if( !RegOpenKeyEx( hKey, sub_key, 0, KEY_READ, &hTestKey) == ERROR_SUCCESS )
        return OPAL_ERROR;

    /* Get the class name and the value count. */
    retCode = RegQueryInfoKey( hTestKey,                /* key handle */
                               achClass,                /* buffer for class name */
                               &cchClassName,           /* size of class string */
                               NULL,                    /* reserved */
                               &cSubKeys,               /* number of subkeys */
                               &cbMaxSubKey,            /* longest subkey size */
                               &cchMaxClass,            /* longest class string */
                               &cValues,                /* number of values for this key */
                               &cchMaxValue,            /* longest value name */
                               &cbMaxValueData,         /* longest value data */
                               &cbSecurityDescriptor,   /* security descriptor */
                               NULL );

    /* Enumerate the subkeys, until RegEnumKeyEx fails. */
    for (i = 0; i < cSubKeys; i++) { 
        cbName = MAX_KEY_LENGTH;
        retCode = RegEnumKeyEx(hTestKey, i, achKey, &cbName, NULL, NULL, NULL, NULL); 
        if (retCode != ERROR_SUCCESS) continue;
        asprintf(&sub_sub_key, "%s\\%s", sub_key, achKey);
        if( NULL != current_name ) {
            asprintf(&next_name, "%s_%s", current_name, achKey);
        } else {
            asprintf(&next_name, "%s", achKey);
        }
        read_keys_from_registry(hKey, sub_sub_key, next_name);
        free(next_name);
        free(sub_sub_key);
    }
    
    /* Enumerate the key values. */
    for( i = 0; i < cValues; i++ ) { 
        cchValue = MAX_VALUE_NAME; 
        achValue[0] = '\0'; 
        retCode = RegEnumValue(hTestKey, i, achValue, &cchValue, NULL, NULL, NULL, NULL);
        if (retCode != ERROR_SUCCESS ) continue; 
       
        /* lpType - get the type of the value
         * dwSize - get the size of the buffer to hold the value
         */
        retCode = RegQueryValueEx(hTestKey, achValue, NULL, (LPDWORD)&lpType, NULL, &dwSize);

        if (strcmp(achValue,"")) {
            if (current_name!=NULL)
                asprintf(&type_name, "%s_%s", current_name, achValue);
            else
                asprintf(&type_name, "%s", achValue);
        } else {
            if (current_name!=NULL)
                asprintf(&type_name, "%s", current_name);
            else
                asprintf(&type_name, "%s", achValue);
        } 
        
        type_len = strcspn(type_name, "_");
        str_key_name = type_name + type_len + 1;
        if( type_len == strlen(type_name) )
            str_key_name = NULL;
        type_name[type_len] = '\0';

        retCode = 1;
        if( lpType == (LPDWORD)REG_SZ ) { /* REG_SZ = 1 */
            retCode = RegQueryValueEx(hTestKey, achValue, NULL, NULL, (LPBYTE)&str_lpData, &dwSize);
            storage.stringval = (char*)str_lpData;
            override.stringval = (char*)str_lpData;
            param_type = MCA_BASE_PARAM_TYPE_STRING;
        } else if( lpType == (LPDWORD)REG_DWORD ) { /* REG_DWORD = 4 */
            retCode = RegQueryValueEx(hTestKey, achValue, NULL, NULL, (LPBYTE)&word_lpData, &dwSize);
            storage.intval  = (int)word_lpData;
            override.intval = (int)word_lpData;
            param_type = MCA_BASE_PARAM_TYPE_INT;
        }
        if( !retCode ) {
            (void)param_register( type_name, NULL, str_key_name, NULL, 
                                  param_type, false, false,
                                  &storage, NULL, &override, &lookup );
        } else {
            opal_output( 0, "error reading value of param_name: %s with %d error.\n",
                         str_key_name, retCode);
        }
        
        free(type_name);
    }

    RegCloseKey( hKey );

    return OPAL_SUCCESS;
}
#endif  /* defined(__WINDOWS__) */

/******************************************************************************/

   
static int param_register(const char *type_name,
                          const char *component_name,
                          const char *param_name,
                          const char *help_msg,
                          mca_base_param_type_t type,
                          bool internal,
                          bool read_only,
                          mca_base_param_storage_t *default_value,
                          mca_base_param_storage_t *file_value,
                          mca_base_param_storage_t *override_value,
                          mca_base_param_storage_t *current_value)
{
  int ret;
  size_t i, len;
  mca_base_param_t param, *array;

  /* There are data holes in the param struct */
  OPAL_DEBUG_ZERO(param);

  /* Initialize the array if it has never been initialized */

  if (!initialized) {
      mca_base_param_init();
  }

  /* Create a parameter entry */

  OBJ_CONSTRUCT(&param, mca_base_param_t);
  param.mbp_type = type;
  param.mbp_internal = internal;
  param.mbp_read_only = read_only;
  if (NULL != help_msg) {
      param.mbp_help_msg = strdup(help_msg);
  }

  if (NULL != type_name) {
      param.mbp_type_name = strdup(type_name);
      if (NULL == param.mbp_type_name) {
          OBJ_DESTRUCT(&param);
          return OPAL_ERR_OUT_OF_RESOURCE;
      }
  }
  if (NULL != component_name) {
      param.mbp_component_name = strdup(component_name);
      if (NULL == param.mbp_component_name) {
          OBJ_DESTRUCT(&param);
          return OPAL_ERR_OUT_OF_RESOURCE;
      }
  }
  param.mbp_param_name = NULL;
  if (NULL != param_name) {
      param.mbp_param_name = strdup(param_name);
      if (NULL == param.mbp_param_name) {
          OBJ_DESTRUCT(&param);
          return OPAL_ERR_OUT_OF_RESOURCE;
      }
  }

  /* Build up the full name */
  len = 16;
  if (NULL != type_name) {
      len += strlen(type_name);
  }
  if (NULL != param.mbp_component_name) {
      len += strlen(param.mbp_component_name);
  }
  if (NULL != param.mbp_param_name) {
      len += strlen(param.mbp_param_name);
  }
  
  param.mbp_full_name = (char*)malloc(len);
  if (NULL == param.mbp_full_name) {
      OBJ_DESTRUCT(&param);
      return OPAL_ERROR;
  }
  
  /* Copy the name over in parts */
  
  param.mbp_full_name[0] = '\0';
  if (NULL != type_name) {
      strncat(param.mbp_full_name, type_name, len);
  }
  if (NULL != component_name) {
      if ('\0' != param.mbp_full_name[0]) {
          strcat(param.mbp_full_name, "_");
      }
      strcat(param.mbp_full_name, component_name);
  }
  if (NULL != param_name) {
      if ('\0' != param.mbp_full_name[0]) {
          strcat(param.mbp_full_name, "_");
      }
      strcat(param.mbp_full_name, param_name);
  }

  /* Create the environment name */

  len = strlen(param.mbp_full_name) + strlen(mca_prefix) + 16;
  param.mbp_env_var_name = (char*)malloc(len);
  if (NULL == param.mbp_env_var_name) {
    OBJ_DESTRUCT(&param);
    return OPAL_ERROR;
  }
  snprintf(param.mbp_env_var_name, len, "%s%s", mca_prefix, 
           param.mbp_full_name);

  /* Figure out the default value; zero it out if a default is not
     provided */

  if (NULL != default_value) {
    if (MCA_BASE_PARAM_TYPE_STRING == param.mbp_type &&
        NULL != default_value->stringval) {
      param.mbp_default_value.stringval = strdup(default_value->stringval);
    } else {
      param.mbp_default_value = *default_value;
    }
  } else {
    memset(&param.mbp_default_value, 0, sizeof(param.mbp_default_value));
  }

  /* Figure out the file value; zero it out if a file is not
     provided */

  if (NULL != file_value) {
    if (MCA_BASE_PARAM_TYPE_STRING == param.mbp_type &&
        NULL != file_value->stringval) {
      param.mbp_file_value.stringval = strdup(file_value->stringval);
    } else {
      param.mbp_file_value = *file_value;
    }
    param.mbp_file_value_set = true;
  } else {
    memset(&param.mbp_file_value, 0, sizeof(param.mbp_file_value));
    param.mbp_file_value_set = false;
  }

  /* Figure out the override value; zero it out if a override is not
     provided */

  if (NULL != override_value) {
    if (MCA_BASE_PARAM_TYPE_STRING == param.mbp_type &&
        NULL != override_value->stringval) {
      param.mbp_override_value.stringval = strdup(override_value->stringval);
    } else {
      param.mbp_override_value = *override_value;
    }
    param.mbp_override_value_set = true;
  } else {
    memset(&param.mbp_override_value, 0, sizeof(param.mbp_override_value));
    param.mbp_override_value_set = false;
  }

  /* See if this entry is already in the array */

  len = opal_value_array_get_size(&mca_base_params);
  array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
  for (i = 0; i < len; ++i) {
    if (0 == strcmp(param.mbp_full_name, array[i].mbp_full_name)) {

        /* We found an entry with the same param name.  Check to see
           if we're changing types */
        /* Easy case: both are INT */

      if (MCA_BASE_PARAM_TYPE_INT == array[i].mbp_type &&
          MCA_BASE_PARAM_TYPE_INT == param.mbp_type) {
          if (NULL != default_value) {
              array[i].mbp_default_value.intval =
                  param.mbp_default_value.intval;
          }
          if (NULL != file_value) {
              array[i].mbp_file_value.intval =
                  param.mbp_file_value.intval;
              array[i].mbp_file_value_set = true;
          }
          if (NULL != override_value) {
              array[i].mbp_override_value.intval =
                  param.mbp_override_value.intval;
              array[i].mbp_override_value_set = true;
          }
      } 

      /* Both are STRING */

      else if (MCA_BASE_PARAM_TYPE_STRING == array[i].mbp_type &&
               MCA_BASE_PARAM_TYPE_STRING == param.mbp_type) {
          if (NULL != default_value) {
              if (NULL != array[i].mbp_default_value.stringval) {
                  free(array[i].mbp_default_value.stringval);
                  array[i].mbp_default_value.stringval = NULL;
              }
              if (NULL != param.mbp_default_value.stringval) {
                  array[i].mbp_default_value.stringval =
                      strdup(param.mbp_default_value.stringval);
              }
          }

          if (NULL != file_value) {
              if (NULL != array[i].mbp_file_value.stringval) {
                  free(array[i].mbp_file_value.stringval);
                  array[i].mbp_file_value.stringval = NULL;
              }
              if (NULL != param.mbp_file_value.stringval) {
                  array[i].mbp_file_value.stringval =
                      strdup(param.mbp_file_value.stringval);
              }
              array[i].mbp_file_value_set = true;
          }

          if (NULL != override_value) {
              if (NULL != array[i].mbp_override_value.stringval) {
                  free(array[i].mbp_override_value.stringval);
                  array[i].mbp_override_value.stringval = NULL;
              }
              if (NULL != param.mbp_override_value.stringval) {
                  array[i].mbp_override_value.stringval =
                      strdup(param.mbp_override_value.stringval);
              }
              array[i].mbp_override_value_set = true;
          }
      } 

      /* Original is INT, new is STRING */

      else if (MCA_BASE_PARAM_TYPE_INT == array[i].mbp_type &&
               MCA_BASE_PARAM_TYPE_STRING == param.mbp_type) {
          if (NULL != default_value && 
              NULL != param.mbp_default_value.stringval) {
              array[i].mbp_default_value.stringval =
                  strdup(param.mbp_default_value.stringval);
          } else {
              /* If the new STRING doesn't have a default value, we
                 must set the default value to "NULL" because it still
                 contains the old INT default value (which may not
                 compare equally to NULL) */
              array[i].mbp_default_value.stringval = NULL;
          }

          if (NULL != file_value &&
              NULL != param.mbp_file_value.stringval) {
              array[i].mbp_file_value.stringval =
                  strdup(param.mbp_file_value.stringval);
              array[i].mbp_file_value_set = true;
          } else {
              /* Similar to above, be sure to set the file default
                 value to NULL to ensure that it's not still set to a
                 non-NULL value from the prior INT default value */
              array[i].mbp_file_value.stringval = NULL;
              array[i].mbp_file_value_set = false;
          }

          if (NULL != override_value &&
              NULL != param.mbp_override_value.stringval) {
              array[i].mbp_override_value.stringval =
                  strdup(param.mbp_override_value.stringval);
              array[i].mbp_override_value_set = true;
          } else {
              /* Similar to above, be sure to set the file default
                 value to NULL to ensure that it's not still set to a
                 non-NULL value from the prior INT default value */
              array[i].mbp_file_value.stringval = NULL;
              array[i].mbp_file_value_set = false;
          }

          array[i].mbp_type = param.mbp_type;
      } 

      /* Original is STRING, new is INT */

      else if (MCA_BASE_PARAM_TYPE_STRING == array[i].mbp_type &&
                 MCA_BASE_PARAM_TYPE_INT == param.mbp_type) {
          /* Free the old STRING default value, if it exists */
          if (NULL != array[i].mbp_default_value.stringval) {
              free(array[i].mbp_default_value.stringval);
          }

          /* Set the new default value, or 0 if one wasn't provided */
          if (NULL != default_value) {
              array[i].mbp_default_value.intval =
                  param.mbp_default_value.intval;
          } else {
              array[i].mbp_default_value.intval = 0;
          }

          if (NULL != file_value) {
              if (NULL != array[i].mbp_file_value.stringval) {
                  free(array[i].mbp_file_value.stringval);
              }
              array[i].mbp_file_value.intval =
                  param.mbp_file_value.intval;
              array[i].mbp_file_value_set = true;
          } else {
              array[i].mbp_file_value.intval = 0;
              array[i].mbp_file_value_set = false;
          }

          if (NULL != override_value) {
              if (NULL != array[i].mbp_override_value.stringval) {
                  free(array[i].mbp_override_value.stringval);
              }
              array[i].mbp_override_value.intval =
                  param.mbp_override_value.intval;
              array[i].mbp_override_value_set = true;
          } else {
              array[i].mbp_file_value.intval = 0;
              array[i].mbp_file_value_set = false;
          }

          array[i].mbp_type = param.mbp_type;
      }

      /* Now delete the newly-created entry (since we just saved the
         value in the old entry) */

      OBJ_DESTRUCT(&param);

      /* Finally, if we have a lookup value, look it up */
      
      if (NULL != current_value) {
          if (!param_lookup(i, current_value, NULL, NULL, NULL)) {
              return OPAL_ERR_NOT_FOUND;
          }
      }

      /* Return the new index */

      return (int)i;
    }
  }

  /* Add it to the array.  Note that we copy the mca_param_t by value,
     so the entire contents of the struct is copied.  The synonym list
     will always be empty at this point, so there's no need for an
     extra RETAIN or RELEASE. */
  if (OPAL_SUCCESS != 
      (ret = opal_value_array_append_item(&mca_base_params, &param))) {
    return ret;
  }
  ret = (int)opal_value_array_get_size(&mca_base_params) - 1;

  /* Finally, if we have a lookup value, look it up */

  if (NULL != current_value) {
      if (!param_lookup(ret, current_value, NULL, NULL, NULL)) {
          return OPAL_ERR_NOT_FOUND;
      }
  }

  /* All done */

  return ret;
}


/*
 * Back-end for registering a synonym
 */
static int syn_register(int index_orig, const char *syn_type_name,
                        const char *syn_component_name,
                        const char *syn_param_name, bool deprecated)
{
    size_t len;
    syn_info_t *si;
    mca_base_param_t *array;

    if (!initialized) {
        return OPAL_ERROR;
    }

    /* Sanity check index param */
    len = opal_value_array_get_size(&mca_base_params);
    if (index_orig < 0 || ((size_t) index_orig) > len) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* Make the synonym info object */
    si = OBJ_NEW(syn_info_t);
    if (NULL == si) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Note that the following logic likely could have been combined
       into more compact code.  However, keeping it separate made it
       much easier to read / maintain (IMHO).  This is not a high
       performance section of the code, so a premium was placed on
       future readability / maintenance. */

    /* Save the function parameters */
    si->si_deprecated = deprecated;
    if (NULL != syn_type_name) {
        si->si_type_name = strdup(syn_type_name);
        if (NULL == si->si_type_name) {
            OBJ_RELEASE(si);
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }

    if (NULL != syn_component_name) {
        si->si_component_name = strdup(syn_component_name);
        if (NULL == si->si_component_name) {
            OBJ_RELEASE(si);
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }

    if (NULL != syn_param_name) {
        si->si_param_name = strdup(syn_param_name);
        if (NULL == si->si_param_name) {
            OBJ_RELEASE(si);
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }

    /* Build up the full name */
    len = 16;
    if (NULL != syn_type_name) {
        len += strlen(syn_type_name);
    }
    if (NULL != syn_component_name) {
        len += strlen(syn_component_name);
    }
    if (NULL != syn_param_name) {
        len += strlen(syn_param_name);
    }
    si->si_full_name = (char*) malloc(len);
    if (NULL == si->si_full_name) {
        OBJ_RELEASE(si);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    
    /* Copy the name over in parts */
    si->si_full_name[0] = '\0';
    if (NULL != syn_type_name) {
        strncat(si->si_full_name, syn_type_name, len);
    }
    if (NULL != syn_component_name) {
        if ('\0' != si->si_full_name[0]) {
            strcat(si->si_full_name, "_");
        }
        strcat(si->si_full_name, syn_component_name);
    }
    if (NULL != syn_param_name) {
        if ('\0' != si->si_full_name[0]) {
            strcat(si->si_full_name, "_");
        }
        strcat(si->si_full_name, syn_param_name);
    }
    
    /* Create the environment name */
    len = strlen(si->si_full_name) + strlen(mca_prefix) + 16;
    si->si_env_var_name = (char*) malloc(len);
    if (NULL == si->si_env_var_name) {
        OBJ_RELEASE(si);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    snprintf(si->si_env_var_name, len, "%s%s", mca_prefix, 
             si->si_full_name);
    
    /* Find the param entry; add this syn_info to its list of
       synonyms */
    array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
    if (NULL == array[index_orig].mbp_synonyms) {
        array[index_orig].mbp_synonyms = OBJ_NEW(opal_list_t);
    }
    opal_list_append(array[index_orig].mbp_synonyms, &(si->super));
  
    /* All done */

    return OPAL_SUCCESS;
}


/*
 * Set an override
 */
static bool param_set_override(size_t index, 
                               mca_base_param_storage_t *storage,
                               mca_base_param_type_t type)
{
    size_t size;
    mca_base_param_t *array;

    /* Lookup the index and see if it's valid */

    if (!initialized) {
        return false;
    }
    size = opal_value_array_get_size(&mca_base_params);
    if (index > size) {
        return false;
    }

    array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
    if (MCA_BASE_PARAM_TYPE_INT == type) {
        array[index].mbp_override_value.intval = storage->intval;
    } else if (MCA_BASE_PARAM_TYPE_STRING == type) {
        if (NULL != storage->stringval) {
            array[index].mbp_override_value.stringval = 
                strdup(storage->stringval);
        } else {
            array[index].mbp_override_value.stringval = NULL;
        }
    }
    array[index].mbp_override_value_set = true;

    return true;
}


/*
 * Lookup a parameter in multiple places
 */
static bool param_lookup(size_t index, mca_base_param_storage_t *storage,
                         opal_hash_table_t *attrs,
                         mca_base_param_source_t *source_param,
                         char **source_file)
{
    size_t size;
    mca_base_param_t *array;
    char *p, *q;
    mca_base_param_source_t source = MCA_BASE_PARAM_SOURCE_MAX;

    /* default the value */
    if (NULL != source_file) {
        *source_file = NULL;
    }
    
    /* Lookup the index and see if it's valid */

    if (!initialized) {
        return false;
    }
    size = opal_value_array_get_size(&mca_base_params);
    if (index > size) {
        return false;
    }
    array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);

    /* Ensure that MCA param has a good type */

    if (MCA_BASE_PARAM_TYPE_INT != array[index].mbp_type &&
        MCA_BASE_PARAM_TYPE_STRING != array[index].mbp_type) {
        return false;
    }

    /* Check all the places that the param may be hiding, in priority
       order -- but if read_only is true, then only look at the
       default location. */

    if (array[index].mbp_read_only) {
        if (lookup_override(&array[index], storage) ||
             lookup_env(&array[index], storage) ||
             lookup_file(&array[index], storage, source_file)) {
            opal_show_help("help-mca-param.txt", "read-only-param-set",
                           true, array[index].mbp_full_name);
        }

        /* First look at the "real" name of this param */
        if (lookup_default(&array[index], storage)) {
            source = MCA_BASE_PARAM_SOURCE_DEFAULT;
        }
    } else {
        if (lookup_override(&array[index], storage)) {
            source = MCA_BASE_PARAM_SOURCE_OVERRIDE;
        } else if (lookup_env(&array[index], storage)) {
            source = MCA_BASE_PARAM_SOURCE_ENV;
        } else if (lookup_file(&array[index], storage, source_file)) {
            source = MCA_BASE_PARAM_SOURCE_FILE;
        } else if (lookup_default(&array[index], storage)) {
            source = MCA_BASE_PARAM_SOURCE_DEFAULT;
        }
    }
    if (MCA_BASE_PARAM_SOURCE_MAX != source) {
        if (NULL != source_param) {
            *source_param = source;
        }
        
        /* If we're returning a string, replace all instances of "~/"
           with the user's home directory */

        if (MCA_BASE_PARAM_TYPE_STRING == array[index].mbp_type &&
            NULL != storage->stringval) {
            if (0 == strncmp(storage->stringval, "~/", 2)) {
                if( NULL == home ) {
                    asprintf(&p, "%s", storage->stringval + 2);
                } else {
                    p = opal_os_path( false, home, storage->stringval + 2, NULL );
                }
                free(storage->stringval);
                storage->stringval = p;
            }

            p = strstr(storage->stringval, ":~/");
            while (NULL != p) {
                *p = '\0';
                if( NULL == home ) {
                    asprintf(&q, "%s:%s", storage->stringval, p + 2);
                } else {
                    asprintf(&q, "%s:%s%s", storage->stringval, home, p + 2);
                }
                free(storage->stringval);
                storage->stringval = q;
                p = strstr(storage->stringval, ":~/");
            }
        }

        return true;
    }

    /* Didn't find it.  Doh! */
  
    return false;
}


/*
 * Lookup a param in the overrides section
 */
static bool lookup_override(mca_base_param_t *param,
                            mca_base_param_storage_t *storage)
{
    if (param->mbp_override_value_set) {
        if (MCA_BASE_PARAM_TYPE_INT == param->mbp_type) {
            storage->intval = param->mbp_override_value.intval;
        } else if (MCA_BASE_PARAM_TYPE_STRING == param->mbp_type) {
            storage->stringval = strdup(param->mbp_override_value.stringval);
        }

        return true;
    }

    /* Don't have an override */

    return false;
}


/*
 * Lookup a param in the environment
 */
static bool lookup_env(mca_base_param_t *param,
                       mca_base_param_storage_t *storage)
{
    char *env = NULL;
    opal_list_item_t *item;
    syn_info_t *si;
    char *deprecated_name = NULL;
    bool print_deprecated_warning = false;

    /* Look for the primary param name */
    if (NULL != param->mbp_env_var_name) {
        env = getenv(param->mbp_env_var_name);
        print_deprecated_warning = 
            param->mbp_deprecated & !param->mbp_deprecated_warning_shown;
        deprecated_name = param->mbp_full_name;
        /* Regardless of whether we want to show the deprecated
           warning or not, we can skip this check the next time
           through on this parameter */
        param->mbp_deprecated_warning_shown = true;
    }
    
    /* If we didn't find the primary name, look in all the synonyms */
    if (NULL == env && NULL != param->mbp_synonyms && 
        !opal_list_is_empty(param->mbp_synonyms)) {
        for (item = opal_list_get_first(param->mbp_synonyms);
             NULL == env && opal_list_get_end(param->mbp_synonyms) != item;
             item = opal_list_get_next(item)) {
            si = (syn_info_t*) item;
            env = getenv(si->si_env_var_name);
            if (NULL != env && 
                ((si->si_deprecated && 
                  !si->si_deprecated_warning_shown) ||
                 (param->mbp_deprecated &&
                  !param->mbp_deprecated_warning_shown))) {
                print_deprecated_warning = 
                    si->si_deprecated_warning_shown = 
                    param->mbp_deprecated_warning_shown = true;
                deprecated_name = si->si_full_name;
            }
        }
    }

    /* If we found it, react */
    if (NULL != env) {
        if (MCA_BASE_PARAM_TYPE_INT == param->mbp_type) {
            storage->intval = (int)strtol(env,(char**)NULL,0);
        } else if (MCA_BASE_PARAM_TYPE_STRING == param->mbp_type) {
            storage->stringval = strdup(env);
        }

        if (print_deprecated_warning) {
            opal_show_help("help-mca-param.txt", "deprecated mca param env",
                           true, deprecated_name);
        }
        return true;
    }
    
    /* Didn't find it */
    return false;
}


/*
 * Lookup a param in the files
 */
static bool lookup_file(mca_base_param_t *param,
                        mca_base_param_storage_t *storage,
                        char **source_file)
{
    bool found = false;
    syn_info_t *si;
    char *deprecated_name = NULL;
    opal_list_item_t *item, *in_item;
    mca_base_param_file_value_t *fv;
    bool print_deprecated_warning = false;

    /* See if we previously found a match from a file.  If so, just
       return that */

    if (param->mbp_file_value_set) {
        if (NULL != source_file) {
            *source_file = param->mbp_source_file;
        }
        return set(param->mbp_type, storage, &param->mbp_file_value);
    }

    /* Scan through the list of values read in from files and try to
       find a match.  If we do, cache it on the param (for future
       lookups) and save it in the storage. */

    for (item = opal_list_get_first(&mca_base_param_file_values);
         opal_list_get_end(&mca_base_param_file_values) != item;
         item = opal_list_get_next(item)) {
        fv = (mca_base_param_file_value_t *) item;
        /* If it doesn't match the parameter's real name, check its
           synonyms */
        if (0 == strcmp(fv->mbpfv_param, param->mbp_full_name)) {
            found = true;
            print_deprecated_warning = 
                param->mbp_deprecated & !param->mbp_deprecated_warning_shown;
            deprecated_name = param->mbp_full_name;
            /* Regardless of whether we want to show the deprecated
               warning or not, we can skip this check the next time
               through on this parameter */
            param->mbp_deprecated_warning_shown = true;
        } else if (NULL != param->mbp_synonyms && 
                   !opal_list_is_empty(param->mbp_synonyms)) {
            /* Check all the synonyms on this parameter and see if the
               file value matches */
            for (in_item = opal_list_get_first(param->mbp_synonyms);
                 opal_list_get_end(param->mbp_synonyms) != in_item;
                 in_item = opal_list_get_next(in_item)) {
                si = (syn_info_t*) in_item;
                if (0 == strcmp(fv->mbpfv_param, si->si_full_name)) {
                    found = true;
                    if ((si->si_deprecated && 
                         !si->si_deprecated_warning_shown) ||
                        (param->mbp_deprecated && 
                         !param->mbp_deprecated_warning_shown)) {
                        print_deprecated_warning = 
                            si->si_deprecated_warning_shown = 
                            param->mbp_deprecated_warning_shown = true;
                        deprecated_name = si->si_full_name;
                    }
                }
            }
        }

        /* Did we find it? */
        if (found) {
            if (MCA_BASE_PARAM_TYPE_INT == param->mbp_type) {
                if (NULL != fv->mbpfv_value) {
                    param->mbp_file_value.intval = 
                        (int)strtol(fv->mbpfv_value,(char**)NULL,0);
                } else {
                    param->mbp_file_value.intval = 0;
                }
            } else {
                param->mbp_file_value.stringval = fv->mbpfv_value;
                fv->mbpfv_value = NULL;
            }
            if (NULL != fv->mbpfv_file) {
                param->mbp_source_file = strdup(fv->mbpfv_file);
            }
            param->mbp_file_value_set = true;

            /* If the caller requested to know what file we found the
               value in, give them a copy of the filename pointer */
            if (NULL != source_file) {
                *source_file = param->mbp_source_file;
            }

            /* Since this is now cached on the param, we might as well
               remove it from the list and make future file lookups
               faster */

            opal_list_remove_item(&mca_base_param_file_values, 
                                  (opal_list_item_t *) fv);
            OBJ_RELEASE(fv);

            /* Print the deprecated warning, if applicable */
            if (print_deprecated_warning) {
                opal_show_help("help-mca-param.txt",
                               "deprecated mca param file",
                               true, deprecated_name);
            }

            return set(param->mbp_type, storage, &param->mbp_file_value);
        }
    }

    return false;
}


/*
 * Return the default value for a param
 */
static bool lookup_default(mca_base_param_t *param,
                           mca_base_param_storage_t *storage)
{
    return set(param->mbp_type, storage, &param->mbp_default_value);
}


static bool set(mca_base_param_type_t type,
                mca_base_param_storage_t *dest, mca_base_param_storage_t *src)
{
    switch (type) {
    case MCA_BASE_PARAM_TYPE_INT:
        dest->intval = src->intval;
        break;
        
    case MCA_BASE_PARAM_TYPE_STRING:
        if (NULL != src->stringval) {
            dest->stringval = strdup(src->stringval);
        } else {
            dest->stringval = NULL;
        }
        break;
        
    default:
        return false;
        break;
    }

    return true;
}


/*
 * Create an empty param container
 */
static void param_constructor(mca_base_param_t *p)
{
    p->mbp_type = MCA_BASE_PARAM_TYPE_MAX;
    p->mbp_internal = false;
    p->mbp_read_only = false;
    p->mbp_deprecated = false;
    p->mbp_deprecated_warning_shown = false;

    p->mbp_type_name = NULL;
    p->mbp_component_name = NULL;
    p->mbp_param_name = NULL;
    p->mbp_full_name = NULL;
    p->mbp_help_msg = NULL;

    p->mbp_env_var_name = NULL;

    p->mbp_default_value.stringval = NULL;
    p->mbp_file_value_set = false;
    p->mbp_file_value.stringval = NULL;
    p->mbp_source_file = NULL;
    p->mbp_override_value_set = false;
    p->mbp_override_value.stringval = NULL;

    p->mbp_synonyms = NULL;
}


/*
 * Free all the contents of a param container
 */
static void param_destructor(mca_base_param_t *p)
{
    opal_list_item_t *item;

    if (NULL != p->mbp_type_name) {
        free(p->mbp_type_name);
    }
    if (NULL != p->mbp_component_name) {
        free(p->mbp_component_name);
    }
    if (NULL != p->mbp_param_name) {
        free(p->mbp_param_name);
    }
    if (NULL != p->mbp_env_var_name) {
        free(p->mbp_env_var_name);
    }
    if (NULL != p->mbp_full_name) {
        free(p->mbp_full_name);
    }
    if (NULL != p->mbp_help_msg) {
        free(p->mbp_help_msg);
    }
    if (MCA_BASE_PARAM_TYPE_STRING == p->mbp_type) {
        if (NULL != p->mbp_default_value.stringval) {
            free(p->mbp_default_value.stringval);
        }
        if (p->mbp_file_value_set) {
            if (NULL != p->mbp_file_value.stringval) {
                free(p->mbp_file_value.stringval);
            }
            if (NULL != p->mbp_source_file) {
                free(p->mbp_source_file);
            }
        }
        if (p->mbp_override_value_set && 
            NULL != p->mbp_override_value.stringval) {
            free(p->mbp_override_value.stringval);
        }
    }

    /* Destroy any synonyms that are on the list */
    if (NULL != p->mbp_synonyms) {
        for (item = opal_list_remove_first(p->mbp_synonyms);
             NULL != item; item = opal_list_remove_first(p->mbp_synonyms)) {
            OBJ_RELEASE(item);
        }
        OBJ_RELEASE(p->mbp_synonyms);
    }

#if OPAL_ENABLE_DEBUG
    /* Cheap trick to reset everything to NULL */
    param_constructor(p);
#endif
}


static void fv_constructor(mca_base_param_file_value_t *f)
{
    f->mbpfv_param = NULL;
    f->mbpfv_value = NULL;
    f->mbpfv_file = NULL;
}


static void fv_destructor(mca_base_param_file_value_t *f)
{
    if (NULL != f->mbpfv_param) {
        free(f->mbpfv_param);
    }
    if (NULL != f->mbpfv_value) {
        free(f->mbpfv_value);
    }
    if (NULL != f->mbpfv_file) {
        free(f->mbpfv_file);
    }
    fv_constructor(f);
}

static void info_constructor(mca_base_param_info_t *p)
{
    p->mbpp_index = -1;
    p->mbpp_type = MCA_BASE_PARAM_TYPE_MAX;

    p->mbpp_type_name = NULL;
    p->mbpp_component_name = NULL;
    p->mbpp_param_name = NULL;
    p->mbpp_full_name = NULL;

    p->mbpp_deprecated = false;

    p->mbpp_synonyms = NULL;
    p->mbpp_synonyms_len = 0;
    p->mbpp_synonym_parent = NULL;

    p->mbpp_read_only = false;
    p->mbpp_help_msg = NULL;
}

static void info_destructor(mca_base_param_info_t *p)
{
    if (NULL != p->mbpp_synonyms) {
        free(p->mbpp_synonyms);
    }
    /* No need to free any of the strings -- the pointers were copied
       by value from their corresponding parameter registration */

    info_constructor(p);
}

static void syn_info_constructor(syn_info_t *si)
{
    si->si_type_name = si->si_component_name = si->si_param_name =
        si->si_full_name = si->si_env_var_name = NULL;
    si->si_deprecated = si->si_deprecated_warning_shown = false;
}

static void syn_info_destructor(syn_info_t *si)
{
    if (NULL != si->si_type_name) {
        free(si->si_type_name);
    }
    if (NULL != si->si_component_name) {
        free(si->si_component_name);
    }
    if (NULL != si->si_param_name) {
        free(si->si_param_name);
    }
    if (NULL != si->si_full_name) {
        free(si->si_full_name);
    }
    if (NULL != si->si_env_var_name) {
        free(si->si_env_var_name);
    }

    syn_info_constructor(si);
}

int mca_base_param_find_int(const mca_base_component_t *component,
                            const char *param_name,
                            char **env,
                            int *current_value)
{
    char *tmp, *ptr;
    int len, i;
    int rc=OPAL_ERR_NOT_FOUND;
    
    if (NULL == env) {
        return OPAL_ERR_NOT_FOUND;
    }
    
    asprintf(&tmp, "%s%s_%s_%s", mca_prefix, component->mca_type_name,
             component->mca_component_name, param_name);
    len = strlen(tmp);
    for (i=0; NULL != env[i]; i++) {
        if (0 == strncmp(tmp, env[i], len)) {
            ptr = strchr(env[i], '=');
            ptr++;
            *current_value = strtol(ptr, NULL, 10);
            rc = OPAL_SUCCESS;
            break;
        }
    }
    free(tmp);
    return rc;
}

int mca_base_param_find_int_name(const char *type,
                                 const char *param_name,
                                 char **env,
                                 int *current_value)
{
    char *tmp, *ptr;
    int len, i;
    int rc=OPAL_ERR_NOT_FOUND;
    
    if (NULL == env) {
        return OPAL_ERR_NOT_FOUND;
    }
    
    asprintf(&tmp, "%s%s_%s", mca_prefix, type, param_name);
    len = strlen(tmp);
    for (i=0; NULL != env[i]; i++) {
        if (0 == strncmp(tmp, env[i], len)) {
            ptr = strchr(env[i], '=');
            ptr++;
            *current_value = strtol(ptr, NULL, 10);
            rc = OPAL_SUCCESS;
            break;
        }
    }
    free(tmp);
    return rc;
}

int mca_base_param_find_string(const mca_base_component_t *component,
                               const char *param_name,
                               char **env,
                               char **current_value)
{
    char *tmp, *ptr;
    int len, i;
    int rc=OPAL_ERR_NOT_FOUND;
    
    if (NULL == env) {
        return OPAL_ERR_NOT_FOUND;
    }
    
    asprintf(&tmp, "%s%s_%s_%s", mca_prefix, component->mca_type_name,
             component->mca_component_name, param_name);
    len = strlen(tmp);
    for (i=0; NULL != env[i]; i++) {
        if (0 == strncmp(tmp, env[i], len)) {
            ptr = strchr(env[i], '=');
            ptr++;
            *current_value = ptr;
            rc = OPAL_SUCCESS;
            break;
        }
    }
    free(tmp);
    return rc;
}

int mca_base_param_find_string_name(const char *type,
                                    const char *param_name,
                                    char **env,
                                    char **current_value)
{
    char *tmp, *ptr;
    int len, i;
    int rc=OPAL_ERR_NOT_FOUND;
    
    if (NULL == env) {
        return OPAL_ERR_NOT_FOUND;
    }
    
    asprintf(&tmp, "%s%s_%s", mca_prefix, type, param_name);
    len = strlen(tmp);
    for (i=0; NULL != env[i]; i++) {
        if (0 == strncmp(tmp, env[i], len)) {
            ptr = strchr(env[i], '=');
            ptr++;
            *current_value = ptr;
            rc = OPAL_SUCCESS;
            break;
        }
    }
    free(tmp);
    return rc;
}

static char *source_name(mca_base_param_source_t source, 
                               const char *filename)
{
    char *ret;

    switch (source) {
    case MCA_BASE_PARAM_SOURCE_DEFAULT:
        return strdup("default value");
        break;

    case MCA_BASE_PARAM_SOURCE_ENV:
        return strdup("command line or environment variable");
        break;

    case MCA_BASE_PARAM_SOURCE_FILE:
        asprintf(&ret, "file (%s)", filename);
        return ret;
        break;

    case MCA_BASE_PARAM_SOURCE_OVERRIDE:
        return strdup("internal override");
        break;

    default:
        return strdup("unknown (!)");
        break;
    }
}

int mca_base_param_check_exclusive_string(const char *type_a,
                                          const char *component_a,
                                          const char *param_a,
                                          const char *type_b,
                                          const char *component_b,
                                          const char *param_b)
{
    int i, ret;
    mca_base_param_source_t source_a, source_b;
    char *filename_a, *filename_b;

    i = mca_base_param_find(type_a, component_a, param_a);
    if (i < 0) {
        return OPAL_ERR_NOT_FOUND;
    }
    ret = mca_base_param_lookup_source(i, &source_a, &filename_a);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    i = mca_base_param_find(type_b, component_b, param_b);
    if (i < 0) {
        return OPAL_ERR_NOT_FOUND;
    }
    ret = mca_base_param_lookup_source(i, &source_b, &filename_b);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    if (MCA_BASE_PARAM_SOURCE_DEFAULT != source_a &&
        MCA_BASE_PARAM_SOURCE_DEFAULT != source_b) {
        size_t len;
        char *str_a, *str_b, *name_a, *name_b;

        /* Form cosmetic string names for A */
        str_a = source_name(source_a, filename_a);
        len = 5;
        if (NULL != type_a) len += strlen(type_a);
        if (NULL != component_a) len += strlen(component_a);
        if (NULL != param_a) len += strlen(param_a);
        name_a = calloc(1, len);
        if (NULL == name_a) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        if (NULL != type_a) {
            strncat(name_a, type_a, len);
            strncat(name_a, "_", len);
        }
        if (NULL != component_a) strncat(name_a, component_a, len);
        strncat(name_a, "_", len);
        strncat(name_a, param_a, len);

        /* Form cosmetic string names for B */
        str_b = source_name(source_b, filename_b);
        len = 5;
        if (NULL != type_b) len += strlen(type_b);
        if (NULL != component_b) len += strlen(component_b);
        if (NULL != param_b) len += strlen(param_b);
        name_b = calloc(1, len);
        if (NULL == name_b) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        if (NULL != type_b) {
            strncat(name_b, type_b, len);
            strncat(name_b, "_", len);
        }
        if (NULL != component_b) strncat(name_b, component_b, len);
        strncat(name_b, "_", len);
        strncat(name_b, param_b, len);

        /* Print it all out */
        opal_show_help("help-mca-param.txt", 
                       "mutually exclusive params",
                       true, name_a, str_a, name_b, str_b);

        /* Free the temp strings */
        free(str_a);
        free(name_a);
        free(str_b);
        free(name_b);
        return OPAL_ERR_BAD_PARAM;
    }

    return OPAL_SUCCESS;
}
