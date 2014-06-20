/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2011      Oracle and/or its affiliates. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "mpi.h"

#include <string.h>
#include <ctype.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif

#include MCA_timer_IMPLEMENTATION_HEADER
#include "opal/mca/installdirs/installdirs.h"
#include "opal/class/opal_value_array.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/util/printf.h"
#include "opal/memoryhooks/memory.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/show_help.h"

#include "ompi/tools/ompi_info/ompi_info.h"
#include "ompi/include/mpi_portable_platform.h"


/*
 * Public variables
 */

const char *ompi_info_component_all = "all";
const char *ompi_info_param_all = "all";

const char *ompi_info_path_prefix = "prefix";
const char *ompi_info_path_bindir = "bindir";
const char *ompi_info_path_libdir = "libdir";
const char *ompi_info_path_incdir = "incdir";
const char *ompi_info_path_mandir = "mandir";
const char *ompi_info_path_pkglibdir = "pkglibdir";
const char *ompi_info_path_sysconfdir = "sysconfdir";
const char *ompi_info_path_exec_prefix = "exec_prefix";
const char *ompi_info_path_sbindir = "sbindir";
const char *ompi_info_path_libexecdir = "libexecdir";
const char *ompi_info_path_datarootdir = "datarootdir";
const char *ompi_info_path_datadir = "datadir";
const char *ompi_info_path_sharedstatedir = "sharedstatedir";
const char *ompi_info_path_localstatedir = "localstatedir";
const char *ompi_info_path_infodir = "infodir";
const char *ompi_info_path_pkgdatadir = "pkgdatadir";
const char *ompi_info_path_pkgincludedir = "pkgincludedir";

/*
 * External variables
 *
 * This exists in mca/base/mca_base_param.c.  It's not extern'ed
 * in mca_base_param.h so that no one else will use it.
 */

extern opal_value_array_t mca_base_params;


void ompi_info_do_params(bool want_all_in, bool want_internal)
{
    int count;
    char *type, *component, *str;
    bool found;
    int i;
    bool want_all = false;
    opal_list_t *info;
    
    ompi_info_open_components();
    
    if (want_all_in) {
        want_all = true;
    } else {
        /* See if the special param "all" was givin to --param; that
         * superceeds any individual type
         */
        count = opal_cmd_line_get_ninsts(ompi_info_cmd_line, "param");
        for (i = 0; i < count; ++i) {
            type = opal_cmd_line_get_param(ompi_info_cmd_line, "param", (int)i, 0);
            if (0 == strcmp(ompi_info_type_all, type)) {
                want_all = true;
                break;
            }
        }
    }
    
    /* Get a dump of all the MCA params */
    mca_base_param_dump(&info, want_internal);
    
    /* Show the params */
    
    if (want_all) {
        for (i = 0; i < mca_types.size; ++i) {
            if (NULL == (type = (char *)opal_pointer_array_get_item(&mca_types, i))) {
                continue;
            }
            ompi_info_show_mca_params(info, type, ompi_info_component_all, want_internal);
        }
    } else {
        for (i = 0; i < count; ++i) {
            type = opal_cmd_line_get_param(ompi_info_cmd_line, "param", (int)i, 0);
            component = opal_cmd_line_get_param(ompi_info_cmd_line, "param", (int)i, 1);
            
            for (found = false, i = 0; i < mca_types.size; ++i) {
                if (NULL == (str = (char *)opal_pointer_array_get_item(&mca_types, i))) {
                    continue;
                }
                if (0 == strcmp(str, type)) {
                    found = true;
                    break;
                }
            }
            
            if (!found) {
                char *usage = opal_cmd_line_get_usage_msg(ompi_info_cmd_line);
                orte_show_help("help-ompi_info.txt", "not-found", true, type);
                free(usage);
                exit(1);
            }
            
            ompi_info_show_mca_params(info, type, component, want_internal);
        }
    }
    
    /* Release all the MCA param memory */
    mca_base_param_dump_release(info);
}


void ompi_info_show_mca_params(opal_list_t *info,
                               const char *type, const char *component, 
                               bool want_internal)
{
    opal_list_item_t *i;
    mca_base_param_info_t *p;
    char *value_string, *empty = "";
    char *message, *content, *tmp;
    int value_int, j;
    mca_base_param_source_t source;
    char *src_file;
    
    for (i = opal_list_get_first(info); i != opal_list_get_last(info);
         i = opal_list_get_next(i)) {
        p = (mca_base_param_info_t*) i;
        
        if (NULL != p->mbpp_type_name && 0 == strcmp(type, p->mbpp_type_name)) {
            if (0 == strcmp(component, ompi_info_component_all) || 
                NULL == p->mbpp_component_name ||
                (NULL != p->mbpp_component_name &&
                 0 == strcmp(component, p->mbpp_component_name))) {
                
                /* Find the source of the value */
                if (OPAL_SUCCESS != 
                    mca_base_param_lookup_source(p->mbpp_index, &source, &src_file)) {
                    continue;
                }
                
                /* Make a char *for the default value.  Invoke a
                 * lookup because it may transform the char *("~/" ->
                 * "<home dir>/") or get the value from the
                 * environment, a file, etc.
                 */
                if (MCA_BASE_PARAM_TYPE_STRING == p->mbpp_type) {
                    mca_base_param_lookup_string(p->mbpp_index,
                                                 &value_string);
                    
                    /* Can't let the char *be NULL because we
                     * assign it to a std::string, below
                     */
                    if (NULL == value_string) {
                        value_string = strdup(empty);
                    }
                } else {
                    mca_base_param_lookup_int(p->mbpp_index, &value_int);
                    asprintf(&value_string, "%d", value_int);
                }
                
                /* Build up the strings to ompi_info_output. */
                
                if (ompi_info_pretty) {
                    asprintf(&message, "MCA %s", p->mbpp_type_name);
                    
                    /* Put in the real, full name (which may be
                     * different than the categorization).
                     */
                    asprintf(&content, "%s \"%s\" (%s: <%s>, data source: ",
                             p->mbpp_read_only ? "information" : "parameter",
                             p->mbpp_full_name,
                             p->mbpp_read_only ? "value" : "current value",
                             (0 == strlen(value_string)) ? "none" : value_string);
                    
                    /* Indicate where the param was set from */
                    switch(source) {
                        case MCA_BASE_PARAM_SOURCE_DEFAULT:
                            asprintf(&tmp, "%sdefault value", content);
                            free(content);
                            content = tmp;
                            break;
                        case MCA_BASE_PARAM_SOURCE_ENV:
                            asprintf(&tmp, "%senvironment or cmdline", content);
                            free(content);
                            content = tmp;
                            break;
                        case MCA_BASE_PARAM_SOURCE_FILE:
                            asprintf(&tmp, "%sfile [%s]", content, src_file);
                            free(content);
                            content = tmp;
                            break;
                        case MCA_BASE_PARAM_SOURCE_OVERRIDE:
                            asprintf(&tmp, "%sAPI override", content);
                            free(content);
                            content = tmp;
                            break;
                        default:
                            break;
                    }
                    
                    /* Is this parameter deprecated? */
                    if (p->mbpp_deprecated) {
                        asprintf(&tmp, "%s, deprecated", content);
                        free(content);
                        content = tmp;
                    }
                    
                    /* Does this parameter have any synonyms? */
                    if (p->mbpp_synonyms_len > 0) {
                        asprintf(&tmp, "%s, synonyms: ", content);
                        free(content);
                        content = tmp;
                        for (j = 0; j < p->mbpp_synonyms_len; ++j) {
                            if (j > 0) {
                                asprintf(&tmp, "%s, %s", content, p->mbpp_synonyms[j]->mbpp_full_name);
                                free(content);
                                content = tmp;
                            } else {
                                asprintf(&tmp, "%s%s", content, p->mbpp_synonyms[j]->mbpp_full_name);
                                free(content);
                                content = tmp;
                            }
                        }
                    }
                    
                    /* Is this parameter a synonym of something else? */
                    else if (NULL != p->mbpp_synonym_parent) {
                        asprintf(&tmp, "%s, synonym of: %s", content, p->mbpp_synonym_parent->mbpp_full_name);
                        free(content);
                        content = tmp;
                    }
                    asprintf(&tmp, "%s)", content);
                    free(content);
                    content = tmp;
                    ompi_info_out(message, message, content);
                    free(message);
                    free(content);
                    
                    /* If we have a help message, ompi_info_output it */
                    if (NULL != p->mbpp_help_msg) {
                        ompi_info_out("", "", p->mbpp_help_msg);
                    }
                } else {
                    /* build the message*/
                    asprintf(&tmp, "mca:%s:%s:param:%s:", p->mbpp_type_name,
                             (NULL == p->mbpp_component_name) ? "base" : p->mbpp_component_name,
                             p->mbpp_full_name);

                    /* Output the value */
                    asprintf(&message, "%svalue", tmp);
                    ompi_info_out(message, message, value_string);
                    free(message);
                    
                    /* Indicate where the param was set from */
                    
                    asprintf(&message, "%sdata_source", tmp);
                    switch(source) {
                        case MCA_BASE_PARAM_SOURCE_DEFAULT:
                            content = strdup("default value");
                            break;
                        case MCA_BASE_PARAM_SOURCE_ENV:
                            content = strdup("environment-cmdline");
                            break;
                        case MCA_BASE_PARAM_SOURCE_FILE:
                            asprintf(&content, "file: %s", src_file);
                            break;
                        case MCA_BASE_PARAM_SOURCE_OVERRIDE:
                            content = strdup("API override");
                            break;
                        default:
                            break;
                    }
                    ompi_info_out(message, message, content);
                    free(message);
                    free(content);
                    
                    /* Output whether it's read only or writable */
                    
                    asprintf(&message, "%sstatus", tmp);
                    content = p->mbpp_read_only ? "read-only" : "writable";
                    ompi_info_out(message, message, content);
                    free(message);
                    
                    /* If it has a help message, ompi_info_output that */
                    
                    if (NULL != p->mbpp_help_msg) {
                        asprintf(&message, "%shelp", tmp);
                        content = p->mbpp_help_msg;
                        ompi_info_out(message, message, content);
                        free(message);
                    }
                    
                    /* Is this parameter deprecated? */
                    asprintf(&message, "%sdeprecated", tmp);
                    content = p->mbpp_deprecated ? "yes" : "no";
                    ompi_info_out(message, message, content);
                    free(message);
                    
                    /* Does this parameter have any synonyms? */
                    if (p->mbpp_synonyms_len > 0) {
                        for (j = 0; j < p->mbpp_synonyms_len; ++j) {
                            asprintf(&message, "%ssynonym:name", tmp);
                            content = p->mbpp_synonyms[j]->mbpp_full_name;
                            ompi_info_out(message, message, content);
                            free(message);
                        }
                    }
                    
                    /* Is this parameter a synonym of something else? */
                    else if (NULL != p->mbpp_synonym_parent) {
                        asprintf(&message, "%ssynonym_of:name", tmp);
                        content = p->mbpp_synonym_parent->mbpp_full_name;
                        ompi_info_out(message, message, content);
                        free(message);
                    }
                }
                
                /* If we allocated the string, then free it */
                
                if (NULL != value_string) {
                    free(value_string);
                }
            }
        }
    }
}


void ompi_info_do_path(bool want_all, opal_cmd_line_t *cmd_line)
{
    int i, count;
    char *scope;
    
    /* Check bozo case */
    count = opal_cmd_line_get_ninsts(cmd_line, "path");
    for (i = 0; i < count; ++i) {
        scope = opal_cmd_line_get_param(cmd_line, "path", i, 0);
        if (0 == strcmp("all", scope)) {
            want_all = true;
            break;
        }
    }
    
    if (want_all) {
        ompi_info_show_path(ompi_info_path_prefix, opal_install_dirs.prefix);
        ompi_info_show_path(ompi_info_path_exec_prefix, opal_install_dirs.exec_prefix);
        ompi_info_show_path(ompi_info_path_bindir, opal_install_dirs.bindir);
        ompi_info_show_path(ompi_info_path_sbindir, opal_install_dirs.sbindir);
        ompi_info_show_path(ompi_info_path_libdir, opal_install_dirs.libdir);
        ompi_info_show_path(ompi_info_path_incdir, opal_install_dirs.includedir);
        ompi_info_show_path(ompi_info_path_mandir, opal_install_dirs.mandir);
        ompi_info_show_path(ompi_info_path_pkglibdir, opal_install_dirs.pkglibdir);
        ompi_info_show_path(ompi_info_path_libexecdir, opal_install_dirs.libexecdir);
        ompi_info_show_path(ompi_info_path_datarootdir, opal_install_dirs.datarootdir);
        ompi_info_show_path(ompi_info_path_datadir, opal_install_dirs.datadir);
        ompi_info_show_path(ompi_info_path_sysconfdir, opal_install_dirs.sysconfdir);
        ompi_info_show_path(ompi_info_path_sharedstatedir, opal_install_dirs.sharedstatedir);
        ompi_info_show_path(ompi_info_path_localstatedir, opal_install_dirs.localstatedir);
        ompi_info_show_path(ompi_info_path_infodir, opal_install_dirs.infodir);
        ompi_info_show_path(ompi_info_path_pkgdatadir, opal_install_dirs.pkgdatadir);
        ompi_info_show_path(ompi_info_path_pkglibdir, opal_install_dirs.pkglibdir);
        ompi_info_show_path(ompi_info_path_pkgincludedir, opal_install_dirs.pkgincludedir);
    } else {
        count = opal_cmd_line_get_ninsts(cmd_line, "path");
        for (i = 0; i < count; ++i) {
            scope = opal_cmd_line_get_param(cmd_line, "path", i, 0);
            
            if (0 == strcmp(ompi_info_path_prefix, scope)) {
                ompi_info_show_path(ompi_info_path_prefix, opal_install_dirs.prefix);
            } else if (0 == strcmp(ompi_info_path_bindir, scope)) {
                ompi_info_show_path(ompi_info_path_bindir, opal_install_dirs.bindir);
            } else if (0 == strcmp(ompi_info_path_libdir, scope)) {
                ompi_info_show_path(ompi_info_path_libdir, opal_install_dirs.libdir);
            } else if (0 == strcmp(ompi_info_path_incdir, scope)) {
                ompi_info_show_path(ompi_info_path_incdir, opal_install_dirs.includedir);
            } else if (0 == strcmp(ompi_info_path_mandir, scope)) {
                ompi_info_show_path(ompi_info_path_mandir, opal_install_dirs.mandir);
            } else if (0 == strcmp(ompi_info_path_pkglibdir, scope)) {
                ompi_info_show_path(ompi_info_path_pkglibdir, opal_install_dirs.pkglibdir);
            } else if (0 == strcmp(ompi_info_path_sysconfdir, scope)) {
                ompi_info_show_path(ompi_info_path_sysconfdir, opal_install_dirs.sysconfdir);
            } else if (0 == strcmp(ompi_info_path_exec_prefix, scope)) {
                ompi_info_show_path(ompi_info_path_exec_prefix, opal_install_dirs.exec_prefix);
            } else if (0 == strcmp(ompi_info_path_sbindir, scope)) {
                ompi_info_show_path(ompi_info_path_sbindir, opal_install_dirs.sbindir);
            } else if (0 == strcmp(ompi_info_path_libexecdir, scope)) {
                ompi_info_show_path(ompi_info_path_libexecdir, opal_install_dirs.libexecdir);
            } else if (0 == strcmp(ompi_info_path_datarootdir, scope)) {
                ompi_info_show_path(ompi_info_path_datarootdir, opal_install_dirs.datarootdir);
            } else if (0 == strcmp(ompi_info_path_datadir, scope)) {
                ompi_info_show_path(ompi_info_path_datadir, opal_install_dirs.datadir);
            } else if (0 == strcmp(ompi_info_path_sharedstatedir, scope)) {
                ompi_info_show_path(ompi_info_path_sharedstatedir, opal_install_dirs.sharedstatedir);
            } else if (0 == strcmp(ompi_info_path_localstatedir, scope)) {
                ompi_info_show_path(ompi_info_path_localstatedir, opal_install_dirs.localstatedir);
            } else if (0 == strcmp(ompi_info_path_infodir, scope)) {
                ompi_info_show_path(ompi_info_path_infodir, opal_install_dirs.infodir);
            } else if (0 == strcmp(ompi_info_path_pkgdatadir, scope)) {
                ompi_info_show_path(ompi_info_path_pkgdatadir, opal_install_dirs.pkgdatadir);
            } else if (0 == strcmp(ompi_info_path_pkgincludedir, scope)) {
                ompi_info_show_path(ompi_info_path_pkgincludedir, opal_install_dirs.pkgincludedir);
            } else {
                char *usage = opal_cmd_line_get_usage_msg(cmd_line);
                orte_show_help("help-ompi_info.txt", "usage", true, usage);
                free(usage);
                exit(1);
            }
        }
    }
}


void ompi_info_show_path(const char *type, const char *value)
{
    char *pretty, *path;
    
    pretty = strdup(type);
    pretty[0] = toupper(pretty[0]);
    
    asprintf(&path, "path:%s", type);
    ompi_info_out(pretty, path, value);
    free(pretty);
    free(path);
}


void ompi_info_do_arch()
{
    ompi_info_out("Configured architecture", "config:arch", OPAL_ARCH);
}


void ompi_info_do_hostname()
{
    ompi_info_out("Configure host", "config:host", OMPI_CONFIGURE_HOST);
}


/*
 * do_config
 * Accepts:
 *	- want_all: boolean flag; TRUE -> display all options
 *				  FALSE -> display selected options
 *
 * This function displays all the options with which the current
 * installation of ompi was configured. There are many options here 
 * that are carried forward from OMPI-7 and are not mca parameters 
 * in OMPI-10. I have to dig through the invalid options and replace
 * them with OMPI-10 options.
 */
void ompi_info_do_config(bool want_all)
{
    char *cxx;
    char *f77;
    char *f90;
    char *f90_size;
    char *heterogeneous;
    char *memprofile;
    char *memdebug;
    char *debug;
    char *mpi_interface_warning;
    char *cprofiling;
    char *cxxprofiling;
    char *f77profiling;
    char *f90profiling;
    char *cxxexceptions;
    char *threads;
    char *want_libltdl;
    char *mpirun_prefix_by_default;
    char *sparse_groups;
    char *have_mpi_io;
    char *wtime_support;
    char *symbol_visibility;
    char *ft_support;
    char *topology_support;
    char *vt_support;
    /* Do a little preprocessor trickery here to figure ompi_info_out the
     * tri-state of MPI_PARAM_CHECK (which will be either 0, 1, or
     * ompi_mpi_param_check).  The preprocessor will only allow
     * comparisons against constants, so you'll get a warning if you
     * check MPI_PARAM_CHECK against 0 or 1, but its real value is the
     * char *ompi_mpi_param_check.  So define ompi_mpi_param_check to
     * be a constant, and then all the preprocessor comparisons work ompi_info_out
     * ok.  Note that we chose the preprocessor comparison rompi_info_oute because
     * it is not sufficient to simply set the variable
     * ompi_mpi_param_check to a non-0/non-1 value.  This is because the
     * compiler will generate a warning that that C variable is unused
     * when MPI_PARAM_CHECK is hard-coded to 0 or 1.
     */
    char *paramcheck;
#define ompi_mpi_param_check 999
#if 0 == MPI_PARAM_CHECK
    paramcheck = "never";
#elif 1 == MPI_PARAM_CHECK
    paramcheck = "always";
#else
    paramcheck = "runtime";
#endif
    
    /* setup the strings that don't require allocations*/
    cxx = OMPI_WANT_CXX_BINDINGS ? "yes" : "no";
    f90 = OMPI_WANT_F90_BINDINGS ? "yes" : "no";
    f90_size = OMPI_F90_BUILD_SIZE;
    heterogeneous = OPAL_ENABLE_HETEROGENEOUS_SUPPORT ? "yes" : "no";
    memprofile = OPAL_ENABLE_MEM_PROFILE ? "yes" : "no";
    memdebug = OPAL_ENABLE_MEM_DEBUG ? "yes" : "no";
    debug = OPAL_ENABLE_DEBUG ? "yes" : "no";
    mpi_interface_warning = OMPI_WANT_MPI_INTERFACE_WARNING ? "yes" : "no";
    cprofiling = OMPI_ENABLE_MPI_PROFILING ? "yes" : "no";
    cxxprofiling = (OMPI_WANT_CXX_BINDINGS && OMPI_ENABLE_MPI_PROFILING) ? "yes" : "no";
    cxxexceptions = (OMPI_WANT_CXX_BINDINGS && OMPI_HAVE_CXX_EXCEPTION_SUPPORT) ? "yes" : "no";
    f77profiling = (OMPI_ENABLE_MPI_PROFILING && OMPI_WANT_F77_BINDINGS) ? "yes" : "no";
    f90profiling = (OMPI_ENABLE_MPI_PROFILING && OMPI_WANT_F90_BINDINGS) ? "yes" : "no";
    want_libltdl = OPAL_WANT_LIBLTDL ? "yes" : "no";
    mpirun_prefix_by_default = ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT ? "yes" : "no";
    sparse_groups = OMPI_GROUP_SPARSE ? "yes" : "no";
    have_mpi_io = OMPI_PROVIDE_MPI_FILE_INTERFACE ? "yes" : "no";
    wtime_support = OPAL_TIMER_USEC_NATIVE ? "native" : "gettimeofday";
    symbol_visibility = OPAL_C_HAVE_VISIBILITY ? "yes" : "no";
    topology_support = OPAL_HAVE_HWLOC ? "yes" : "no";
    vt_support = OMPI_ENABLE_CONTRIB_vt ? "yes" : "no";
    
    /* setup strings that require allocation */
    if (OMPI_WANT_F77_BINDINGS) {
        asprintf(&f77, "yes (%s)",
                 (OPAL_HAVE_WEAK_SYMBOLS ? "all" :
                  (OMPI_F77_CAPS ? "caps" :
                   (OMPI_F77_PLAIN ? "lower case" :
                    (OMPI_F77_SINGLE_UNDERSCORE ? "single underscore" : "double underscore")))));
    } else {
        f77 = strdup("no");
    }
    
    if (OPAL_HAVE_SOLARIS_THREADS || OPAL_HAVE_POSIX_THREADS) {
        asprintf(&threads, "%s (MPI_THREAD_MULTIPLE: %s, progress: %s)", OPAL_HAVE_SOLARIS_THREADS ? "solaris" :
                 (OPAL_HAVE_POSIX_THREADS ? "posix" : "type unknown"),
                 OMPI_ENABLE_THREAD_MULTIPLE ? "yes" : "no",
                 OPAL_ENABLE_PROGRESS_THREADS ? "yes" : "no");
    } else {
        threads = strdup("no");
    }
    
    asprintf(&ft_support, "%s (checkpoint thread: %s)", 
             OPAL_ENABLE_FT ? "yes" : "no", OPAL_ENABLE_FT_THREAD ? "yes" : "no");;
    
    /* output values */
    ompi_info_out("Configured by", "config:user", OMPI_CONFIGURE_USER);
    ompi_info_out("Configured on", "config:timestamp", OMPI_CONFIGURE_DATE);
    ompi_info_out("Configure host", "config:host", OMPI_CONFIGURE_HOST);
    
    ompi_info_out("Built by", "build:user", OMPI_BUILD_USER);
    ompi_info_out("Built on", "build:timestamp", OMPI_BUILD_DATE);
    ompi_info_out("Built host", "build:host", OMPI_BUILD_HOST);
    
    ompi_info_out("C bindings", "bindings:c", "yes");
    ompi_info_out("C++ bindings", "bindings:cxx", cxx);
    ompi_info_out("Fortran77 bindings", "bindings:f77", f77);
    free(f77);
    ompi_info_out("Fortran90 bindings", "bindings:f90", f90);
    ompi_info_out("Fortran90 bindings size", "bindings:f90:size", 
                  OMPI_WANT_F90_BINDINGS ? f90_size : "na");
    
    ompi_info_out("C compiler", "compiler:c:command", OPAL_CC);
    ompi_info_out("C compiler absolute", "compiler:c:absolute", OPAL_CC_ABSOLUTE);
    ompi_info_out("C compiler family name", "compiler:c:familyname", _STRINGIFY(OPAL_BUILD_PLATFORM_COMPILER_FAMILYNAME));
    ompi_info_out("C compiler version", "compiler:c:version", _STRINGIFY(OPAL_BUILD_PLATFORM_COMPILER_VERSION_STR));
    
    if (want_all) {
        ompi_info_out_int("C char size", "compiler:c:sizeof:char", sizeof(char));
        /* JMS: should be fixed in MPI-2.2 to differentiate between C
           _Bool and C++ bool.  For the moment, the code base assumes
           that they are the same.  Because of opal_config_bottom.h,
           we can sizeof(bool) here, so we might as well -- even
           though this technically isn't right.  This should be fixed
           when we update to MPI-2.2.  See below for note about C++
           bool alignment. */
        ompi_info_out_int("C bool size", "compiler:c:sizeof:bool", sizeof(bool));
        ompi_info_out_int("C short size", "compiler:c:sizeof:short", sizeof(short));
        ompi_info_out_int("C int size", "compiler:c:sizeof:int", sizeof(int));
        ompi_info_out_int("C long size", "compiler:c:sizeof:long", sizeof(long));
        ompi_info_out_int("C float size", "compiler:c:sizeof:float", sizeof(float));
        ompi_info_out_int("C double size", "compiler:c:sizeof:double", sizeof(double));
        ompi_info_out_int("C pointer size", "compiler:c:sizeof:pointer", sizeof(void *));
        ompi_info_out_int("C char align", "compiler:c:align:char", OPAL_ALIGNMENT_CHAR);
#if OMPI_WANT_CXX_BINDINGS
        /* JMS: See above for note about C++ bool size.  We don't have
           the bool alignment the way configure currently runs -- need
           to clean this up when we update for MPI-2.2. */
        ompi_info_out_int("C bool align", "compiler:c:align:bool", OPAL_ALIGNMENT_CXX_BOOL);
#else
        ompi_info_out("C bool align", "compiler:c:align:bool", "skipped");
#endif
        ompi_info_out_int("C int align", "compiler:c:align:int", OPAL_ALIGNMENT_INT);
        ompi_info_out_int("C float align", "compiler:c:align:float", OPAL_ALIGNMENT_FLOAT);
        ompi_info_out_int("C double align", "compiler:c:align:double", OPAL_ALIGNMENT_DOUBLE);
    }

    ompi_info_out("C++ compiler", "compiler:cxx:command", OMPI_CXX);
    ompi_info_out("C++ compiler absolute", "compiler:cxx:absolute", OMPI_CXX_ABSOLUTE);
    ompi_info_out("Fortran77 compiler", "compiler:f77:command", OMPI_F77);
    ompi_info_out("Fortran77 compiler abs", "compiler:f77:absolute", 
                  OMPI_F77_ABSOLUTE);
    ompi_info_out("Fortran90 compiler", "compiler:f90:command", OMPI_F90);
    ompi_info_out("Fortran90 compiler abs", "compiler:f90:absolute", 
                  OMPI_F90_ABSOLUTE);
    
    if (want_all) {
        
        /* Will always have the size of Fortran integer */
        
        ompi_info_out_int("Fort integer size", "compiler:fortran:sizeof:integer", 
                      OMPI_SIZEOF_FORTRAN_INTEGER);
        
        ompi_info_out_int("Fort logical size", "compiler:fortran:sizeof:logical", 
                      OMPI_SIZEOF_FORTRAN_LOGICAL);
        ompi_info_out_int("Fort logical value true", "compiler:fortran:value:true",
                      OMPI_FORTRAN_VALUE_TRUE);
        
        
        /* May or may not have the other Fortran sizes */
        
        if (OMPI_WANT_F77_BINDINGS || OMPI_WANT_F90_BINDINGS) {
            ompi_info_out("Fort have integer1", "compiler:fortran:have:integer1", 
                          OMPI_HAVE_FORTRAN_INTEGER1 ? "yes" : "no");
            ompi_info_out("Fort have integer2", "compiler:fortran:have:integer2", 
                          OMPI_HAVE_FORTRAN_INTEGER2 ? "yes" : "no");
            ompi_info_out("Fort have integer4", "compiler:fortran:have:integer4", 
                          OMPI_HAVE_FORTRAN_INTEGER4 ? "yes" : "no");
            ompi_info_out("Fort have integer8", "compiler:fortran:have:integer8", 
                          OMPI_HAVE_FORTRAN_INTEGER8 ? "yes" : "no");
            ompi_info_out("Fort have integer16", "compiler:fortran:have:integer16", 
                          OMPI_HAVE_FORTRAN_INTEGER16 ? "yes" : "no");
            
            ompi_info_out("Fort have real4", "compiler:fortran:have:real4", 
                          OMPI_HAVE_FORTRAN_REAL4 ? "yes" : "no");
            ompi_info_out("Fort have real8", "compiler:fortran:have:real8", 
                          OMPI_HAVE_FORTRAN_REAL8 ? "yes" : "no");
            ompi_info_out("Fort have real16", "compiler:fortran:have:real16", 
                          OMPI_HAVE_FORTRAN_REAL16 && OMPI_REAL16_MATCHES_C ? "yes" : "no");
            
            ompi_info_out("Fort have complex8", "compiler:fortran:have:complex8", 
                          OMPI_HAVE_FORTRAN_COMPLEX8 ? "yes" : "no");
            ompi_info_out("Fort have complex16", "compiler:fortran:have:complex16", 
                          OMPI_HAVE_FORTRAN_COMPLEX16 ? "yes" : "no");
            ompi_info_out("Fort have complex32", "compiler:fortran:have:complex32", 
                          OMPI_HAVE_FORTRAN_COMPLEX32 && OMPI_REAL16_MATCHES_C ? "yes" : "no");
            
            ompi_info_out_int("Fort integer1 size", "compiler:fortran:sizeof:integer1", 
                          OMPI_HAVE_FORTRAN_INTEGER1 ? OMPI_SIZEOF_FORTRAN_INTEGER1 : -1);
            ompi_info_out_int("Fort integer2 size", "compiler:fortran:sizeof:integer2", 
                          OMPI_HAVE_FORTRAN_INTEGER2 ? OMPI_SIZEOF_FORTRAN_INTEGER2 : -1);
            ompi_info_out_int("Fort integer4 size", "compiler:fortran:sizeof:integer4", 
                          OMPI_HAVE_FORTRAN_INTEGER4 ? OMPI_SIZEOF_FORTRAN_INTEGER4 : -1);
            ompi_info_out_int("Fort integer8 size", "compiler:fortran:sizeof:integer8", 
                          OMPI_HAVE_FORTRAN_INTEGER8 ? OMPI_SIZEOF_FORTRAN_INTEGER8 : -1);
            ompi_info_out_int("Fort integer16 size", "compiler:fortran:sizeof:integer16", 
                          OMPI_HAVE_FORTRAN_INTEGER16 ? OMPI_SIZEOF_FORTRAN_INTEGER16 : -1);
            
            ompi_info_out_int("Fort real size", "compiler:fortran:sizeof:real", 
                          OMPI_SIZEOF_FORTRAN_REAL);
            ompi_info_out_int("Fort real4 size", "compiler:fortran:sizeof:real4", 
                          OMPI_HAVE_FORTRAN_REAL4 ? OMPI_SIZEOF_FORTRAN_REAL4 : -1);
            ompi_info_out_int("Fort real8 size", "compiler:fortran:sizeof:real8", 
                          OMPI_HAVE_FORTRAN_REAL8 ? OMPI_SIZEOF_FORTRAN_REAL8 : -1);
            ompi_info_out_int("Fort real16 size", "compiler:fortran:sizeof:real17", 
                          OMPI_HAVE_FORTRAN_REAL16 ? OMPI_SIZEOF_FORTRAN_REAL16 : -1);
            
            ompi_info_out_int("Fort dbl prec size", 
                          "compiler:fortran:sizeof:double_precision",
                          OMPI_SIZEOF_FORTRAN_DOUBLE_PRECISION);
            
            ompi_info_out_int("Fort cplx size", "compiler:fortran:sizeof:complex", 
                          OMPI_SIZEOF_FORTRAN_COMPLEX);
            ompi_info_out_int("Fort dbl cplx size",
                          "compiler:fortran:sizeof:double_complex", 
                          OMPI_HAVE_FORTRAN_DOUBLE_COMPLEX ? OMPI_SIZEOF_FORTRAN_DOUBLE_COMPLEX : -1);
            ompi_info_out_int("Fort cplx8 size", "compiler:fortran:sizeof:complex8", 
                          OMPI_HAVE_FORTRAN_COMPLEX8 ? OMPI_SIZEOF_FORTRAN_COMPLEX8 : -1);
            ompi_info_out_int("Fort cplx16 size", "compiler:fortran:sizeof:complex16", 
                          OMPI_HAVE_FORTRAN_COMPLEX16 ? OMPI_SIZEOF_FORTRAN_COMPLEX16 : -1);
            ompi_info_out_int("Fort cplx32 size", "compiler:fortran:sizeof:complex32", 
                          OMPI_HAVE_FORTRAN_COMPLEX32 ? OMPI_SIZEOF_FORTRAN_COMPLEX32 : -1);
            
            ompi_info_out_int("Fort integer align", "compiler:fortran:align:integer", 
                          OMPI_ALIGNMENT_FORTRAN_INTEGER);
            ompi_info_out_int("Fort integer1 align", "compiler:fortran:align:integer1", 
                          OMPI_HAVE_FORTRAN_INTEGER1 ? OMPI_ALIGNMENT_FORTRAN_INTEGER1 : -1);
            ompi_info_out_int("Fort integer2 align", "compiler:fortran:align:integer2", 
                          OMPI_HAVE_FORTRAN_INTEGER2 ? OMPI_ALIGNMENT_FORTRAN_INTEGER2 : -1);
            ompi_info_out_int("Fort integer4 align", "compiler:fortran:align:integer4", 
                          OMPI_HAVE_FORTRAN_INTEGER4 ? OMPI_ALIGNMENT_FORTRAN_INTEGER4 : -1);
            ompi_info_out_int("Fort integer8 align", "compiler:fortran:align:integer8", 
                          OMPI_HAVE_FORTRAN_INTEGER8 ? OMPI_ALIGNMENT_FORTRAN_INTEGER8 : -1);
            ompi_info_out_int("Fort integer16 align", "compiler:fortran:align:integer16", 
                          OMPI_HAVE_FORTRAN_INTEGER16 ? OMPI_ALIGNMENT_FORTRAN_INTEGER16 : -1);
            
            ompi_info_out_int("Fort real align", "compiler:fortran:align:real", 
                          OMPI_ALIGNMENT_FORTRAN_REAL);
            ompi_info_out_int("Fort real4 align", "compiler:fortran:align:real4", 
                          OMPI_HAVE_FORTRAN_REAL4 ? OMPI_ALIGNMENT_FORTRAN_REAL4 : -1);
            ompi_info_out_int("Fort real8 align", "compiler:fortran:align:real8", 
                          OMPI_HAVE_FORTRAN_REAL8 ? OMPI_ALIGNMENT_FORTRAN_REAL8 : -1);
            ompi_info_out_int("Fort real16 align", "compiler:fortran:align:real16", 
                          OMPI_HAVE_FORTRAN_REAL16 ? OMPI_ALIGNMENT_FORTRAN_REAL16 : -1);
            
            ompi_info_out_int("Fort dbl prec align", 
                          "compiler:fortran:align:double_precision",
                          OMPI_ALIGNMENT_FORTRAN_DOUBLE_PRECISION);
            
            ompi_info_out_int("Fort cplx align", "compiler:fortran:align:complex", 
                          OMPI_ALIGNMENT_FORTRAN_COMPLEX);
            ompi_info_out_int("Fort dbl cplx align",
                          "compiler:fortran:align:double_complex", 
                          OMPI_HAVE_FORTRAN_DOUBLE_COMPLEX ? OMPI_ALIGNMENT_FORTRAN_DOUBLE_COMPLEX : -1);
            ompi_info_out_int("Fort cplx8 align", "compiler:fortran:align:complex8", 
                          OMPI_HAVE_FORTRAN_COMPLEX8 ? OMPI_ALIGNMENT_FORTRAN_COMPLEX8 : -1);
            ompi_info_out_int("Fort cplx16 align", "compiler:fortran:align:complex16", 
                          OMPI_HAVE_FORTRAN_COMPLEX16 ? OMPI_ALIGNMENT_FORTRAN_COMPLEX16 : -1);
            ompi_info_out_int("Fort cplx32 align", "compiler:fortran:align:complex32", 
                          OMPI_HAVE_FORTRAN_COMPLEX32 ? OMPI_ALIGNMENT_FORTRAN_COMPLEX32 : -1);
            
        } else {
            ompi_info_out("Fort real size", "compiler:fortran:sizeof:real", "skipped");
            ompi_info_out("Fort dbl prec size",
                          "compiler:fortran:sizeof:double_precision", "skipped");
            ompi_info_out("Fort cplx size", "compiler:fortran:sizeof:complex", "skipped");
            ompi_info_out("Fort dbl cplx size",
                          "compiler:fortran:sizeof:double_complex", "skipped");
            
            ompi_info_out("Fort integer align", "compiler:fortran:align:integer", "skipped");
            ompi_info_out("Fort real align", "compiler:fortran:align:real", "skipped");
            ompi_info_out("Fort dbl prec align", 
                          "compiler:fortran:align:double_precision","skipped");
            ompi_info_out("Fort cplx align", "compiler:fortran:align:complex", "skipped");
            ompi_info_out("Fort dbl cplx align",
                          "compiler:fortran:align:double_complex", "skipped");
        }
    }
    
    ompi_info_out("C profiling", "option:profiling:c", cprofiling);
    ompi_info_out("C++ profiling", "option:profiling:cxx", cxxprofiling);
    ompi_info_out("Fortran77 profiling", "option:profiling:f77", f77profiling);
    ompi_info_out("Fortran90 profiling", "option:profiling:f90", f90profiling);
    
    ompi_info_out("C++ exceptions", "option:cxx_exceptions", cxxexceptions);
    ompi_info_out("Thread support", "option:threads", threads);
    free(threads);
    ompi_info_out("Sparse Groups", "option:sparse:groups", sparse_groups);
    
    if (want_all) {
        
        /* Don't display the build CPPFLAGS or CXXCPPFLAGS because they're
         * just -I$(top_srcdir)/include, etc.  Hence, they're a) boring,
         * and c) specific for ompi_info.
         */
        
        ompi_info_out("Build CFLAGS", "option:build:cflags", OMPI_BUILD_CFLAGS);
        ompi_info_out("Build CXXFLAGS", "option:build:cxxflags", OMPI_BUILD_CXXFLAGS);
        ompi_info_out("Build FFLAGS", "option:build:fflags", OMPI_BUILD_FFLAGS);
        ompi_info_out("Build FCFLAGS", "option:build:fcflags", OMPI_BUILD_FCFLAGS);
        ompi_info_out("Build LDFLAGS", "option:build:ldflags", OMPI_BUILD_LDFLAGS);
        ompi_info_out("Build LIBS", "option:build:libs", OMPI_BUILD_LIBS);
        
        ompi_info_out("Wrapper extra CFLAGS", "option:wrapper:extra_cflags", 
                      WRAPPER_EXTRA_CFLAGS);
        ompi_info_out("Wrapper extra CXXFLAGS", "option:wrapper:extra_cxxflags", 
                      WRAPPER_EXTRA_CXXFLAGS);
        ompi_info_out("Wrapper extra FFLAGS", "option:wrapper:extra_fflags", 
                      WRAPPER_EXTRA_FFLAGS);
        ompi_info_out("Wrapper extra FCFLAGS", "option:wrapper:extra_fcflags", 
                      WRAPPER_EXTRA_FCFLAGS);
        ompi_info_out("Wrapper extra LDFLAGS", "option:wrapper:extra_ldflags", 
                      WRAPPER_EXTRA_LDFLAGS);
        ompi_info_out("Wrapper extra LIBS", "option:wrapper:extra_libs",
                      WRAPPER_EXTRA_LIBS);
    }
    
    ompi_info_out("Internal debug support", "option:debug", debug);
    ompi_info_out("MPI interface warnings", "option:mpi-interface-warning", mpi_interface_warning);
    ompi_info_out("MPI parameter check", "option:mpi-param-check", paramcheck);
    ompi_info_out("Memory profiling support", "option:mem-profile", memprofile);
    ompi_info_out("Memory debugging support", "option:mem-debug", memdebug);
    ompi_info_out("libltdl support", "option:dlopen", want_libltdl);
    ompi_info_out("Heterogeneous support", "options:heterogeneous", heterogeneous);
    ompi_info_out("mpirun default --prefix", "mpirun:prefix_by_default", 
                  mpirun_prefix_by_default);
    ompi_info_out("MPI I/O support", "options:mpi-io", have_mpi_io);
    ompi_info_out("MPI_WTIME support", "options:mpi-wtime", wtime_support);
    ompi_info_out("Symbol vis. support", "options:visibility", symbol_visibility);
    ompi_info_out("Host topology support", "options:host-topology", 
                  topology_support);
    
    ompi_info_out("MPI extensions", "options:mpi_ext", OMPI_EXT_COMPONENTS);
    
    ompi_info_out("FT Checkpoint support", "options:ft_support", ft_support);
    free(ft_support);
    
    ompi_info_out("VampirTrace support", "options:vt", vt_support);

    ompi_info_out_int("MPI_MAX_PROCESSOR_NAME", "options:mpi-max-processor-name", 
                  MPI_MAX_PROCESSOR_NAME);
    ompi_info_out_int("MPI_MAX_ERROR_STRING",   "options:mpi-max-error-string",   
                  MPI_MAX_ERROR_STRING);
    ompi_info_out_int("MPI_MAX_OBJECT_NAME",    "options:mpi-max-object-name",    
                  MPI_MAX_OBJECT_NAME);
    ompi_info_out_int("MPI_MAX_INFO_KEY",       "options:mpi-max-info-key",       
                  MPI_MAX_INFO_KEY);
    ompi_info_out_int("MPI_MAX_INFO_VAL",       "options:mpi-max-info-val",       
                  MPI_MAX_INFO_VAL);
    ompi_info_out_int("MPI_MAX_PORT_NAME",      "options:mpi-max-port-name",      
                  MPI_MAX_PORT_NAME);
#if OMPI_PROVIDE_MPI_FILE_INTERFACE
    ompi_info_out_int("MPI_MAX_DATAREP_STRING", "options:mpi-max-datarep-string", 
                  MPI_MAX_DATAREP_STRING);
#else
    ompi_info_out("MPI_MAX_DATAREP_STRING", "options:mpi-max-datarep-string", 
                  "IO interface not provided");
#endif
    
}
