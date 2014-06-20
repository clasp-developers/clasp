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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>
#include <errno.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif  /*  HAVE_STDLIB_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif  /* HAVE_SYS_STAT_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_REGEX_H
#include <regex.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif  /* HAVE_SYS_WAIT_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#include "opal/mca/installdirs/installdirs.h"
#include "opal/runtime/opal.h"
#include "opal/constants.h"
#include "opal/util/argv.h"
#include "opal/util/error.h"
#include "opal/util/keyval_parse.h"
#include "opal/util/opal_environ.h"
#include "opal/util/show_help.h"
#include "opal/util/path.h"
#include "opal/util/few.h"
#include "opal/util/basename.h"
#include "opal/util/os_path.h"

#if defined(__WINDOWS__) && defined(_MSC_VER)
#define OPAL_INCLUDE_FLAG  "/I"
#define OPAL_LIBDIR_FLAG   "/LIBPATH:"
#else
#define OPAL_INCLUDE_FLAG  "-I"
#define OPAL_LIBDIR_FLAG   "-L"
#endif  /* !defined(__WINDOWS__) && defined(_MSC_VER) */

struct options_data_t {
    char **compiler_args;
    char *language;
    char *project;
    char *project_short;
    char *version;
    char *compiler_env;
    char *compiler_flags_env;
    char *compiler;
    char *module_option;
    char **preproc_flags;
    char **comp_flags;
    char **link_flags;
    char **libs;
    char *req_file;
    char *path_includedir;
    char *path_libdir;
    char *extra_includes;
};

static struct options_data_t *options_data = NULL;
/* index used by parser */
static int parse_options_idx = -1;
/* index of options specified by user */
static int user_data_idx = -1;
/* index of options to use by default */
static int default_data_idx = -1;

#define COMP_DRY_RUN       0x001
#define COMP_SHOW_ERROR    0x002
#define COMP_WANT_COMMAND  0x004
#define COMP_WANT_PREPROC  0x008
#define COMP_WANT_COMPILE  0x010
#define COMP_WANT_LINK     0x020
#define COMP_WANT_PMPI     0x040

static void
options_data_init(struct options_data_t *data)
{
    data->compiler_args = (char **) malloc(sizeof(char*));
    data->compiler_args[0] = NULL;
    data->language = NULL;
    data->compiler = NULL;
    data->project = NULL;
    data->project_short = NULL;
    data->version = NULL;
    data->compiler_env = NULL;
    data->compiler_flags_env = NULL;
    data->module_option = NULL;
    data->preproc_flags = (char **) malloc(sizeof(char*));
    data->preproc_flags[0] = NULL;
    data->comp_flags = (char **) malloc(sizeof(char*));
    data->comp_flags[0] = NULL;
    data->link_flags = (char **) malloc(sizeof(char*));
    data->link_flags[0] = NULL;
    data->libs = (char **) malloc(sizeof(char*));
    data->libs[0] = NULL;
    data->req_file = NULL;
    data->path_includedir = NULL;
    data->path_libdir = NULL;
    data->extra_includes = NULL;
}

static void
options_data_free(struct options_data_t *data)
{
    if (NULL != data->compiler_args) {
        opal_argv_free(data->compiler_args);
    }
    if (NULL != data->language) free(data->language);
    if (NULL != data->compiler) free(data->compiler);
    if (NULL != data->project) free(data->project);
    if (NULL != data->project_short) free(data->project_short);
    if (NULL != data->version) free(data->version);
    if (NULL != data->compiler_env) free(data->compiler_env);
    if (NULL != data->compiler_flags_env) free(data->compiler_flags_env);
    if (NULL != data->module_option) free(data->module_option);
    opal_argv_free(data->preproc_flags);
    opal_argv_free(data->comp_flags);
    opal_argv_free(data->link_flags);
    opal_argv_free(data->libs);
    if (NULL != data->req_file) free(data->req_file);
    if (NULL != data->path_includedir) free(data->path_includedir);
    if (NULL != data->path_libdir) free(data->path_libdir);
    if (NULL != data->extra_includes) free(data->extra_includes);
}

static void
options_data_expand(const char *value)
{
    /* make space for the new set of args */
    parse_options_idx++;
    options_data = (struct options_data_t *) realloc(options_data, sizeof(struct options_data_t) * (parse_options_idx + 1));
    options_data_init(&(options_data[parse_options_idx]));

    /* if there are values, this is not the default case.
       Otherwise, it's the default case... */
    if (NULL != value && 0 != strcmp(value, "")) {
        char **values = opal_argv_split(value, ';');
        opal_argv_insert(&(options_data[parse_options_idx].compiler_args),
                         opal_argv_count(options_data[parse_options_idx].compiler_args),
                         values);
        opal_argv_free(values);
    } else {
        free(options_data[parse_options_idx].compiler_args);
        options_data[parse_options_idx].compiler_args = NULL;
        /* this is a default */
        default_data_idx = parse_options_idx;
    }
}


static int
find_options_index(const char *arg)
{
    int i, j;
#ifdef HAVE_REGEXEC
    int args_count;
    regex_t res;
#endif

    for (i = 0 ; i <= parse_options_idx ; ++i) {
        if (NULL == options_data[i].compiler_args) {
            continue;
        }

#ifdef HAVE_REGEXEC
        args_count = opal_argv_count(options_data[i].compiler_args);
        for (j = 0 ; j < args_count ; ++j) {
            if (0 != regcomp(&res, options_data[i].compiler_args[j], REG_NOSUB)) {
                return -1;
            }

            if (0 == regexec(&res, arg, (size_t) 0, NULL, 0)) {
                regfree(&res);
                return i;
            }

            regfree(&res);
        }
#else
        for (j = 0 ; j < opal_argv_count(options_data[i].compiler_args) ; ++j) {
            if (0 == strcmp(arg, options_data[i].compiler_args[j])) {
                return i;
            }
        }
#endif
    }

    return -1;
}


static void
add_extra_includes(const char *includes, const char* includedir)
{
    int i;
    char **values = opal_argv_split(includes, ' ');

    for (i = 0 ; i < opal_argv_count(values) ; ++i) {
        char *line, *include_directory;

        include_directory = opal_os_path(false, includedir, values[i], NULL);

#if defined(__WINDOWS__)
        asprintf(&line, OPAL_INCLUDE_FLAG"\"%s\"", include_directory);
#else
        asprintf(&line, OPAL_INCLUDE_FLAG"%s", include_directory);
#endif  /* defined(__WINDOWS__) */

        opal_argv_append_nosize(&options_data[parse_options_idx].preproc_flags, line);
        free(include_directory);
        free(line);
    }
}


static void
expand_flags(char **argv)
{
    int i;
    char *tmp;

    for (i = 0 ; argv[i] != NULL ; ++i) {
        tmp = opal_install_dirs_expand(argv[i]);
        if (tmp != argv[i]) {
            free(argv[i]);
            argv[i] = tmp;
        }
    }
}


static void
data_callback(const char *key, const char *value)
{
    /* handle case where text file does not contain any special
       compiler options field */
    if (parse_options_idx < 0 && 0 != strcmp(key, "compiler_args")) {
        options_data_expand(NULL);
    }

    if (0 == strcmp(key, "compiler_args")) {
        options_data_expand(value);
    } else if (0 == strcmp(key, "language")) {
        if (NULL != value) options_data[parse_options_idx].language = strdup(value);
    } else if (0 == strcmp(key, "compiler")) {
        if (NULL != value) options_data[parse_options_idx].compiler = strdup(value);
    } else if (0 == strcmp(key, "project")) {
        if (NULL != value) options_data[parse_options_idx].project = strdup(value);
    } else if (0 == strcmp(key, "version")) {
        if (NULL != value) options_data[parse_options_idx].version = strdup(value);
    } else if (0 == strcmp(key, "module_option")) {
        if (NULL != value) options_data[parse_options_idx].module_option = strdup(value);
    } else if (0 == strcmp(key, "extra_includes")) {
        if (NULL != value) options_data[parse_options_idx].extra_includes = strdup(value);
        if (NULL != value && NULL != options_data[parse_options_idx].path_includedir) {
            /* includedir already found -- we now have both pieces of information, and the
               includedir code didn't do this because it didn't have the extra includes */
            add_extra_includes(value, options_data[parse_options_idx].path_includedir);
        }
    } else if (0 == strcmp(key, "preprocessor_flags")) {
        char **values = opal_argv_split(value, ' ');
        opal_argv_insert(&options_data[parse_options_idx].preproc_flags, 
                         opal_argv_count(options_data[parse_options_idx].preproc_flags),
                         values);
        expand_flags(options_data[parse_options_idx].preproc_flags);
        opal_argv_free(values);
    } else if (0 == strcmp(key, "compiler_flags")) {
        char **values = opal_argv_split(value, ' ');
        opal_argv_insert(&options_data[parse_options_idx].comp_flags,
                         opal_argv_count(options_data[parse_options_idx].comp_flags),
                         values);
        expand_flags(options_data[parse_options_idx].comp_flags);
        opal_argv_free(values);
    } else if (0 == strcmp(key, "linker_flags")) {
        char **values = opal_argv_split(value, ' ');
        opal_argv_insert(&options_data[parse_options_idx].link_flags,
                         opal_argv_count(options_data[parse_options_idx].link_flags),
                         values);
        expand_flags(options_data[parse_options_idx].link_flags);
        opal_argv_free(values);
    } else if (0 == strcmp(key, "libs")) {
        char **values = opal_argv_split(value, ' ');
        opal_argv_insert(&options_data[parse_options_idx].libs,
                         opal_argv_count(options_data[parse_options_idx].libs),
                         values);
        opal_argv_free(values);
    } else if (0 == strcmp(key, "required_file")) {
        if (NULL != value) options_data[parse_options_idx].req_file = strdup(value);
    } else if (0 == strcmp(key, "project_short")) {
        if (NULL != value) options_data[parse_options_idx].project_short = strdup(value);
    } else if (0 == strcmp(key, "compiler_env")) {
        if (NULL != value) options_data[parse_options_idx].compiler_env = strdup(value);
    } else if (0 == strcmp(key, "compiler_flags_env")) {
        if (NULL != value) options_data[parse_options_idx].compiler_flags_env = strdup(value);
    } else if (0 == strcmp(key, "includedir")) {
        if (NULL != value) {
            options_data[parse_options_idx].path_includedir =
                opal_install_dirs_expand(value);
            if (0 != strcmp(options_data[parse_options_idx].path_includedir, "/usr/include") ||
                0 == strncmp(options_data[parse_options_idx].language, "Fortran", strlen("Fortran"))) {
                char *line;
#if defined(__WINDOWS__)
                asprintf(&line, OPAL_INCLUDE_FLAG"\"%s\"", 
                         options_data[parse_options_idx].path_includedir);
#else
                asprintf(&line, OPAL_INCLUDE_FLAG"%s", 
                         options_data[parse_options_idx].path_includedir);
#endif  /* defined(__WINDOWS__) */
                opal_argv_append_nosize(&options_data[parse_options_idx].preproc_flags, line);
                free(line);
            }
            /* Now that we have an include dir, see if we already have
               the extra includes, so that we can post them as well */
            if (NULL != options_data[parse_options_idx].extra_includes) {
                add_extra_includes(options_data[parse_options_idx].extra_includes,
                                   options_data[parse_options_idx].path_includedir);
            }
        }
    } else if (0 == strcmp(key, "libdir")) {
        if (NULL != value) options_data[parse_options_idx].path_libdir = 
                               opal_install_dirs_expand(value);
#if defined(__WINDOWS__) && defined(_MSC_VER)
        opal_argv_append_nosize( &options_data[parse_options_idx].link_flags, "/link" );
#endif  /* defined(__WINDOWS__) && defined(_MSC_VER) */
        if (0 != strcmp(options_data[parse_options_idx].path_libdir, "/usr/lib")) {
            char *line;
#if defined(__WINDOWS__)
#  if defined(_MSC_VER)
            asprintf(&line, OPAL_LIBDIR_FLAG"\"%s\"", 
                     options_data[parse_options_idx].path_libdir);
#  else
            /* linked DLLs are in bin for MinGW build*/
            asprintf(&line, OPAL_LIBDIR_FLAG"\"%s/../bin\"", 
                     options_data[parse_options_idx].path_libdir);
#  endif /* defined(_MSC_VER) */
#else
            asprintf(&line, OPAL_LIBDIR_FLAG"%s", 
                     options_data[parse_options_idx].path_libdir);
#endif  /* defined(__WINDOWS__) */
            opal_argv_append_nosize(&options_data[parse_options_idx].link_flags, line);
            free(line);
        }
    }
}


static int
data_init(const char *appname) 
{
    int ret;
    char *datafile;

    /* now load the data */
    asprintf(&datafile, "%s%s%s-wrapper-data.txt", 
             opal_install_dirs.pkgdatadir, OPAL_PATH_SEP, appname);
    if (NULL == datafile) return OPAL_ERR_TEMP_OUT_OF_RESOURCE;

    ret = opal_util_keyval_parse(datafile, data_callback);
	if( OPAL_SUCCESS != ret ) {
		fprintf(stderr, "Cannot open configuration file %s\n", datafile );
	}
    free(datafile);

    return ret;
}


static int
data_finalize(void)
{
    int i;

    for (i = 0 ; i <= parse_options_idx ; ++i) {
        options_data_free(&(options_data[i]));
    }
    free(options_data);

    return OPAL_SUCCESS;
}


static void
print_flags(char **args, char *pattern)
{
    int i;
    bool found = false;

    for (i = 0 ; args[i] != NULL ; ++i) {
        if (0 == strncmp(args[i], pattern, strlen(pattern))) {
            if (found)  printf(" ");
            printf("%s", args[i] + strlen(pattern));
            found = true;
        }
    }

    if (found) printf("\n");
}


static void
load_env_data(const char *project, const char *flag, char **data)
{
    char *envname;
    char *envvalue;

    if (NULL == project || NULL == flag) return;

    asprintf(&envname, "%s_MPI%s", project, flag);
    if (NULL == (envvalue = getenv(envname))) {
        free(envname);
        asprintf(&envname, "%s_%s", project, flag);
        if (NULL == (envvalue = getenv(envname))) {
            free(envname);
            return;
        }
    } 
    free(envname);

    if (NULL != *data) free(*data);
    *data = strdup(envvalue);
}


static void
load_env_data_argv(const char *project, const char *flag, char ***data)
{
    char *envname;
    char *envvalue;

    if (NULL == project || NULL == flag) return;

    asprintf(&envname, "%s_MPI%s", project, flag);
    if (NULL == (envvalue = getenv(envname))) {
        free(envname);
        asprintf(&envname, "%s_%s", project, flag);
        if (NULL == (envvalue = getenv(envname))) {
            free(envname);
            return;
        }
    } 
    free(envname);

    if (NULL != *data) opal_argv_free(*data);

    *data = opal_argv_split(envvalue, ' ');
}


int
main(int argc, char *argv[])
{
    int exit_status = 0, ret, flags = 0, i;
    int exec_argc = 0, user_argc = 0;
    char **exec_argv = NULL, **user_argv = NULL;
    char *exec_command, *base_argv0 = NULL;
    bool disable_flags = true;
    bool real_flag = false;

    if (OPAL_SUCCESS != (ret = opal_init_util(&argc, &argv))) {
        return ret;
    }

    /****************************************************
     *
     * Setup compiler information
     *
     ****************************************************/

    base_argv0 = opal_basename(argv[0]);
#if defined(EXEEXT)
    if( 0 != strlen(EXEEXT) ) {
        char extension[] = EXEEXT;
        char* temp = strstr( base_argv0, extension );
        char* old_match = temp;
        while( NULL != temp ) {
            old_match = temp;
            temp = strstr( temp + 1, extension );
        }
        /* Only if there was a match of .exe, erase the last occurence of .exe */
        if ( NULL != old_match ) {
            *old_match = '\0';
        }
    }
#endif  /* defined(EXEEXT) */

    if (OPAL_SUCCESS != (ret = data_init(base_argv0))) {
        fprintf(stderr, "Error parsing data file %s: %s\n", base_argv0, opal_strerror(ret));
        return ret;
    }

    for (i = 1 ; i < argc && user_data_idx < 0 ; ++i) {
        user_data_idx = find_options_index(argv[i]);
    }
    /* if we didn't find a match, look for the NULL (base case) options */
    if (user_data_idx < 0) {
        user_data_idx = default_data_idx;
    }
    /* if we still didn't find a match, abort */
    if (user_data_idx < 0) {
        char *flat = opal_argv_join(argv, ' ');
        opal_show_help("help-opal-wrapper.txt", "no-options-support", true,
                       base_argv0, flat, NULL);
        free(flat);
        exit(1);
    }

    /* compiler */
    load_env_data(options_data[user_data_idx].project_short, options_data[user_data_idx].compiler_env, &options_data[user_data_idx].compiler);

    /* preprocessor flags */
    load_env_data_argv(options_data[user_data_idx].project_short, "CPPFLAGS", &options_data[user_data_idx].preproc_flags);

    /* compiler flags */
    load_env_data_argv(options_data[user_data_idx].project_short, options_data[user_data_idx].compiler_flags_env,
                       &options_data[user_data_idx].comp_flags);

    /* linker flags */
    load_env_data_argv(options_data[user_data_idx].project_short, "LDFLAGS", &options_data[user_data_idx].link_flags);

    /* libs */
    load_env_data_argv(options_data[user_data_idx].project_short, "LIBS", &options_data[user_data_idx].libs);


    /****************************************************
     *
     * Sanity Checks
     *
     ****************************************************/
    
    if (NULL != options_data[user_data_idx].req_file) {
        /* make sure the language is supported */
        if (0 == strcmp(options_data[user_data_idx].req_file, "not supported")) {
            opal_show_help("help-opal-wrapper.txt", "no-language-support", true,
                           options_data[user_data_idx].language, base_argv0, NULL);
            exit_status = 1;
            goto cleanup;
        }

        if (options_data[user_data_idx].req_file[0] != '\0') {
            char *filename;
            struct stat buf;
            filename = opal_os_path( false, options_data[user_data_idx].path_libdir, options_data[user_data_idx].req_file, NULL );
            if (0 != stat(filename, &buf)) {
                opal_show_help("help-opal-wrapper.txt", "file-not-found", true,
                               base_argv0, options_data[user_data_idx].req_file, options_data[user_data_idx].language, NULL);
            }
        }
    }

    /****************************************************
     *
     * Parse user flags
     *
     ****************************************************/
    flags = COMP_WANT_COMMAND|COMP_WANT_PREPROC|
        COMP_WANT_COMPILE|COMP_WANT_LINK;

    user_argv = opal_argv_copy(argv + 1);
    user_argc = opal_argv_count(user_argv);

    for (i = 0 ; i < user_argc ; ++i) {
        if (0 == strncmp(user_argv[i], "-showme", strlen("-showme")) ||
            0 == strncmp(user_argv[i], "--showme", strlen("--showme")) ||
            0 == strncmp(user_argv[i], "-show", strlen("-show")) ||
            0 == strncmp(user_argv[i], "--show", strlen("--show"))) {
            bool done_now = false;

            /* check for specific things we want to see.  First three
               still invoke all the building routines.  Last set want
               to parse out certain flags, so we don't go through the
               normal build routine - skip to cleanup. */
            if (0 == strncmp(user_argv[i], "-showme:command", strlen("-showme:command")) ||
                0 == strncmp(user_argv[i], "--showme:command", strlen("--showme:command"))) {
                flags = COMP_WANT_COMMAND;
                /* we know what we want, so don't process any more args */
                done_now = true;
            } else if (0 == strncmp(user_argv[i], "-showme:compile", strlen("-showme:compile")) ||
                0 == strncmp(user_argv[i], "--showme:compile", strlen("--showme:compile"))) {
                flags = COMP_WANT_PREPROC|COMP_WANT_COMPILE;
                /* we know what we want, so don't process any more args */
                done_now = true;
            } else if (0 == strncmp(user_argv[i], "-showme:link", strlen("-showme:link")) ||
                       0 == strncmp(user_argv[i], "--showme:link", strlen("--showme:link"))) {
                flags = COMP_WANT_COMPILE|COMP_WANT_LINK;
                /* we know what we want, so don't process any more args */
                done_now = true;
            } else if (0 == strncmp(user_argv[i], "-showme:incdirs", strlen("-showme:incdirs")) ||
                       0 == strncmp(user_argv[i], "--showme:incdirs", strlen("--showme:incdirs"))) {
                print_flags(options_data[user_data_idx].preproc_flags, OPAL_INCLUDE_FLAG);
                goto cleanup;
            } else if (0 == strncmp(user_argv[i], "-showme:libdirs", strlen("-showme:libdirs")) ||
                       0 == strncmp(user_argv[i], "--showme:libdirs", strlen("--showme:libdirs"))) {
                print_flags(options_data[user_data_idx].link_flags, OPAL_LIBDIR_FLAG);
                goto cleanup;
            } else if (0 == strncmp(user_argv[i], "-showme:libs", strlen("-showme:libs")) ||
                       0 == strncmp(user_argv[i], "--showme:libs", strlen("--showme:libs"))) {
                print_flags(options_data[user_data_idx].libs, "-l");
                goto cleanup;
            } else if (0 == strncmp(user_argv[i], "-showme:version", strlen("-showme:version")) ||
                       0 == strncmp(user_argv[i], "--showme:version", strlen("--showme:version"))) {
                opal_show_help("help-opal-wrapper.txt", "version", false,
                               argv[0], options_data[user_data_idx].project, options_data[user_data_idx].version, options_data[user_data_idx].language, NULL);
                goto cleanup;
            } else if (0 == strncmp(user_argv[i], "-showme:", strlen("-showme:")) ||
                       0 == strncmp(user_argv[i], "--showme:", strlen("--showme:"))) {
                opal_show_help("help-opal-wrapper.txt", "usage", true,
                               argv[0], options_data[user_data_idx].project, NULL);
                goto cleanup;
            }

            flags |= (COMP_DRY_RUN|COMP_SHOW_ERROR);
            /* remove element from user_argv */
            opal_argv_delete(&user_argc, &user_argv, i, 1);
            --i;

            if (done_now) {
                disable_flags = false;
                break;
            }

        } else if (0 == strcmp(user_argv[i], "-c")) {
            flags &= ~COMP_WANT_LINK;
            real_flag = true;
        } else if (0 == strcmp(user_argv[i], "-E") || 
                   0 == strcmp(user_argv[i], "-M")) {
            flags &= ~(COMP_WANT_COMPILE | COMP_WANT_LINK);
            real_flag = true;
        } else if (0 == strcmp(user_argv[i], "-S")) {
            flags &= ~COMP_WANT_LINK;
            real_flag = true;
        } else if (0 == strcmp(user_argv[i], "-lpmpi")) {
            flags |= COMP_WANT_PMPI;

            /* remove element from user_argv */
            opal_argv_delete(&user_argc, &user_argv, i, 1);
            --i;
        } else if ('-' != user_argv[i][0]) {
            disable_flags = false;
            flags |= COMP_SHOW_ERROR;
            real_flag = true;
        } else { 
            /* if the option flag is one that we use to determine
               which set of compiler data to use, don't count it as a
               real option */
            if (find_options_index(user_argv[i]) < 0) {
                real_flag = true;
            }
        }
    }

    /* clear out the want_flags if we got no arguments not starting
       with a - (dash) and -showme wasn't given OR -showme was given
       and we had at least one more non-showme argument that started
       with a - (dash) and no other non-dash arguments.  Some examples:

       opal_wrapper                : clear our flags
       opal_wrapper -v             : clear our flags
       opal_wrapper -E a.c         : don't clear our flags
       opal_wrapper a.c            : don't clear our flags
       opal_wrapper -showme        : don't clear our flags
       opal_wrapper -showme -v     : clear our flags
       opal_wrapper -showme -E a.c : don't clear our flags
       opal_wrapper -showme a.c    : don't clear our flags
    */
    if (disable_flags && !((flags & COMP_DRY_RUN) && !real_flag)) {
        flags &= ~(COMP_WANT_PREPROC|COMP_WANT_COMPILE|COMP_WANT_LINK);
    }

#if !OMPI_ENABLE_MPI_PROFILING
    /* sanity check */
    if (flags & COMP_WANT_PMPI) {
	    opal_show_help("help-opal-wrapper.txt", "no-profiling-support", true,
		               argv[0], NULL);
    }
#endif


    /****************************************************
     *
     * Assemble the command line
     *
     ****************************************************/

    /* compiler (may be multiple arguments, so split) */
    if (flags & COMP_WANT_COMMAND) {
        exec_argv = opal_argv_split(options_data[user_data_idx].compiler, ' ');
        exec_argc = opal_argv_count(exec_argv);
    } else {
        exec_argv = (char **) malloc(sizeof(char*));
        exec_argv[0] = NULL;
        exec_argc = 0;
    }

    /* Per https://svn.open-mpi.org/trac/ompi/ticket/2201, add all the
       user arguments before anything else. */
    opal_argv_insert(&exec_argv, exec_argc, user_argv);
    exec_argc = opal_argv_count(exec_argv);

    /* preproc flags */
    if (flags & COMP_WANT_PREPROC) {
        opal_argv_insert(&exec_argv, exec_argc, options_data[user_data_idx].preproc_flags);
        exec_argc = opal_argv_count(exec_argv);
    }

    /* compiler flags */
    if (flags & COMP_WANT_COMPILE) {
        opal_argv_insert(&exec_argv, exec_argc, options_data[user_data_idx].comp_flags);
        /* Deal with languages like Fortran 90 that have special
           places and flags for modules or whatever */
        if (options_data[user_data_idx].module_option != NULL) {
            char *line;
            asprintf(&line, "%s%s", options_data[user_data_idx].module_option, options_data[user_data_idx].path_libdir);
            opal_argv_append_nosize(&exec_argv, line);
            free(line);
        }
        exec_argc = opal_argv_count(exec_argv);
    }

    /* link flags and libs */
    if (flags & COMP_WANT_LINK) {
        opal_argv_insert(&exec_argv, exec_argc, options_data[user_data_idx].link_flags);
        exec_argc = opal_argv_count(exec_argv);

        opal_argv_insert(&exec_argv, exec_argc, options_data[user_data_idx].libs);
        exec_argc = opal_argv_count(exec_argv);
    }


    /****************************************************
     *
     * Execute the command
     *
     ****************************************************/

    if (flags & COMP_DRY_RUN) {
        exec_command = opal_argv_join(exec_argv, ' ');
        printf("%s\n", exec_command);
    } else {
        char *tmp;

#if 0
        exec_command = opal_argv_join(exec_argv, ' ');
        printf("command: %s\n", exec_command);
#endif

        tmp = opal_path_findv(exec_argv[0], 0, environ, NULL);
        if (NULL == tmp) {
            opal_show_help("help-opal-wrapper.txt", "no-compiler-found", true,
                           exec_argv[0], NULL);
            errno = 0;
            exit_status = 1;
        }  else {
            int status;

            free(exec_argv[0]);
            exec_argv[0] = tmp;
            ret = opal_few(exec_argv, &status);
            exit_status = WIFEXITED(status) ? WEXITSTATUS(status) :
                              (WIFSIGNALED(status) ? WTERMSIG(status) :
                                  (WIFSTOPPED(status) ? WSTOPSIG(status) : 255));
            if( (OPAL_SUCCESS != ret) || ((0 != exit_status) && (flags & COMP_SHOW_ERROR)) ) {
                char* exec_command = opal_argv_join(exec_argv, ' ');
                if( OPAL_SUCCESS != ret ) {
                    opal_show_help("help-opal-wrapper.txt", "spawn-failed", true,
                                   exec_argv[0], strerror(status), exec_command, NULL);
                } else {
#if 0
                    opal_show_help("help-opal-wrapper.txt", "compiler-failed", true,
                                   exec_argv[0], exit_status, exec_command, NULL);
#endif
                }
                free(exec_command);
            }
        }
    }

    /****************************************************
     *
     * Cleanup
     *
     ****************************************************/
 cleanup:

    opal_argv_free(exec_argv);
    opal_argv_free(user_argv);
    if (NULL != base_argv0) free(base_argv0);

    if (OPAL_SUCCESS != (ret = data_finalize())) {
        return ret;
    }

    if (OPAL_SUCCESS != (ret = opal_finalize_util())) {
        return ret;
    }

    return exit_status;
}
