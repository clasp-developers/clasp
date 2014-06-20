/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2009 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_INFO_TOOL_H
#define OMPI_INFO_TOOL_H
#include "ompi_config.h"

#include "opal/class/opal_list.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/util/cmd_line.h"
#include "opal/mca/mca.h"

BEGIN_C_DECLS

/*
 * Globals
 */

extern bool ompi_info_pretty;
extern opal_cmd_line_t *ompi_info_cmd_line;

extern const char *ompi_info_type_all;
extern const char *ompi_info_type_ompi;
extern const char *ompi_info_type_orte;
extern const char *ompi_info_type_opal;
extern const char *ompi_info_type_base;

extern opal_pointer_array_t mca_types;


/*
 * Version-related strings and functions
 */

extern const char *ompi_info_ver_full;
extern const char *ompi_info_ver_major;
extern const char *ompi_info_ver_minor;
extern const char *ompi_info_ver_release;
extern const char *ompi_info_ver_greek;
extern const char *ompi_info_ver_svn;

void ompi_info_do_version(bool want_all, opal_cmd_line_t *cmd_line);
void ompi_info_show_ompi_version(const char *scope);
void ompi_info_show_component_version(const char *type_name, 
                                      const char *component_name,
                                      const char *scope, 
                                      const char *ver_type);

/*
 * Parameter/configuration-related functions
 */

extern const char *ompi_info_component_all;
extern const char *ompi_info_param_all;

extern const char *ompi_info_path_prefix;
extern const char *ompi_info_path_bindir;
extern const char *ompi_info_path_libdir;
extern const char *ompi_info_path_incdir;
extern const char *ompi_info_path_mandir;
extern const char *ompi_info_path_pkglibdir;
extern const char *ompi_info_path_sysconfdir;
extern const char *ompi_info_path_exec_prefix;
extern const char *ompi_info_path_sbindir;
extern const char *ompi_info_path_libexecdir;
extern const char *ompi_info_path_datarootdir;
extern const char *ompi_info_path_datadir;
extern const char *ompi_info_path_sharedstatedir;
extern const char *ompi_info_path_localstatedir;
extern const char *ompi_info_path_infodir;
extern const char *ompi_info_path_pkgdatadir;
extern const char *ompi_info_path_pkgincludedir;

void ompi_info_do_params(bool want_all, bool want_internal);
void ompi_info_show_mca_params(opal_list_t *info,
                               const char *type, const char *component, 
                               bool want_internal);

void ompi_info_do_path(bool want_all, opal_cmd_line_t *cmd_line);
void ompi_info_show_path(const char *type, const char *value);

void ompi_info_do_arch(void);
void ompi_info_do_hostname(void);
void ompi_info_do_config(bool want_all);

/*
 * Output-related functions
 */
void ompi_info_out(const char *pretty_message, 
                   const char *plain_message, 
                   const char *value);
void ompi_info_out_int(const char *pretty_message, 
                       const char *plain_message, 
                       int value);
/*
 * Component-related functions
 */
typedef struct {
    opal_list_item_t super;
    char *type;
    opal_list_t *components;
} ompi_info_component_map_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_info_component_map_t);

extern opal_pointer_array_t component_map;

void ompi_info_open_components(void);
void ompi_info_close_components(void);

END_C_DECLS

#endif /* OMPI_INFO_H */
