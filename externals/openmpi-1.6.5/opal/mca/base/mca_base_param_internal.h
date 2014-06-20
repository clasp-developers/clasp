/*
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
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/** 
 * @file 
 *
 * This is the private declarations for the MCA parameter system.
 * This file is internal to the MCA parameter system and should not
 * need to be used by any other elements in Open MPI except the
 * special case of the ompi_info command.
 *
 * All the rest of the doxygen documentation in this file is marked as
 * "internal" and won't show up unless you specifically tell doxygen
 * to generate internal documentation (by default, it is skipped).
 */

#ifndef OPAL_MCA_BASE_PARAM_INTERNAL_H
#define OPAL_MCA_BASE_PARAM_INTERNAL_H

#include "opal_config.h"

#include "opal/class/opal_object.h"
#include "opal/class/opal_list.h"
#include "opal/mca/base/mca_base_param.h"

BEGIN_C_DECLS

/**
 * \internal
 *
 * Types for MCA parameters.
 */
typedef union {
    /** Integer value */
    int intval;
    /** String value */
    char *stringval;
} mca_base_param_storage_t;


/**
 * \internal
 *
 * Entry for holding the information about an MCA parameter and its
 * default value.
 */
struct mca_base_param_t {
    /** Allow this to be an OPAL OBJ */
    opal_object_t mbp_super;

    /** Enum indicating the type of the parameter (integer or string) */
    mca_base_param_type_t mbp_type;
    /** String of the type name, or NULL */
    char *mbp_type_name;
    /** String of the component name */
    char *mbp_component_name;
    /** String of the parameter name */
    char *mbp_param_name;
    /** Full parameter name, in case it is not
       <type>_<component>_<param> */
    char *mbp_full_name;

    /** List of synonym names for this parameter.  This *must* be a
        pointer (vs. a plain opal_list_t) because we copy this whole
        struct into a new param for permanent storage
        (opal_vale_array_append_item()), and the internal pointers in
        the opal_list_t will be invalid when that happens.  Hence, we
        simply keep a pointer to an external opal_list_t.  Synonyms
        are uncommon enough that this is not a big performance hit. */
    opal_list_t *mbp_synonyms;
    
    /** Whether this is internal (not meant to be seen / modified by
        users) or not */
    bool mbp_internal;
    /** Whether this value is changable from the default value that
        was registered (e.g., when true, useful for reporting values,
        like the value of the GM library that was linked against) */
    bool mbp_read_only;
    /** Whether this MCA parameter (*and* all of its synonyms) is
        deprecated or not */
    bool mbp_deprecated;
    /** Whether the warning message for the deprecated MCA param has
        been shown already or not */
    bool mbp_deprecated_warning_shown;
    /** Help message associated with this parameter */
    char *mbp_help_msg;

    /** Environment variable name */
    char *mbp_env_var_name;

    /** Default value of the parameter */
    mca_base_param_storage_t mbp_default_value;

    /** Whether or not we have a file value */
    bool mbp_file_value_set;
    /** Value of the parameter found in a file */
    mca_base_param_storage_t mbp_file_value;
    /** File the value came from */
    char *mbp_source_file;

    /** Whether or not we have an override value */
    bool mbp_override_value_set;
    /** Value of the parameter override set via API */
    mca_base_param_storage_t mbp_override_value;
};
/**
 * \internal
 *
 * Convenience typedef.
 */
typedef struct mca_base_param_t mca_base_param_t;

/**
 * \internal
 *
 * Object delcataion for mca_base_param_t
 */
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(mca_base_param_t);


/**
 * \internal
 *
 * Structure for holding param names and values read in from files.
 */
struct mca_base_param_file_value_t {
    /** Allow this to be an OPAL OBJ */
    opal_list_item_t super;
    
    /** Parameter name */
    char *mbpfv_param;
    /** Parameter value */
    char *mbpfv_value;
    /** File it came from */
    char *mbpfv_file;
};
/**
 * \internal
 *
 * Convenience typedef
 */
typedef struct mca_base_param_file_value_t mca_base_param_file_value_t;

/**
 * Object declaration for mca_base_param_file_value_t
 */
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(mca_base_param_file_value_t);


/**
 * \internal
 *
 * Global list of params and values read in from MCA parameter files
 */
OPAL_DECLSPEC extern opal_list_t mca_base_param_file_values;

/**
 * \internal
 *
 * Parse a parameter file.
 */
OPAL_DECLSPEC int mca_base_parse_paramfile(const char *paramfile);

END_C_DECLS
    
#endif /* OPAL_MCA_BASE_PARAM_INTERNAL_H */
