/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Evergrid, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/constants.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"

#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

/******************
 * Local Functions
 ******************/
static int metadata_extract_next_token(FILE *file, char **token, char **value);
static int opal_crs_base_metadata_open(FILE ** meta_data, char * location, char * mode);

static char *last_metadata_file = NULL;
static char **cleanup_file_argv = NULL;
static char **cleanup_dir_argv = NULL;

/******************
 * Object stuff
 ******************/
static void opal_crs_base_construct(opal_crs_base_snapshot_t *snapshot)
{
    snapshot->component_name  = NULL;
    snapshot->reference_name  = opal_crs_base_unique_snapshot_name(getpid());
    snapshot->local_location  = opal_crs_base_get_snapshot_directory(snapshot->reference_name);
    snapshot->remote_location = strdup(snapshot->local_location);
    snapshot->cold_start      = false;
}

static void opal_crs_base_destruct( opal_crs_base_snapshot_t *snapshot)
{
    if(NULL != snapshot->reference_name) {
        free(snapshot->reference_name);
        snapshot->reference_name = NULL;
    }
    if(NULL != snapshot->local_location) {
        free(snapshot->local_location);
        snapshot->local_location = NULL;
    }
    if(NULL != snapshot->remote_location) {
       free(snapshot->remote_location);
       snapshot->remote_location = NULL;
    }
    if(NULL != snapshot->component_name) {
        free(snapshot->component_name);
        snapshot->component_name = NULL;
    }
}

OBJ_CLASS_INSTANCE(opal_crs_base_snapshot_t,
                   opal_list_item_t,
                   opal_crs_base_construct,
                   opal_crs_base_destruct);

static void opal_crs_base_ckpt_options_construct(opal_crs_base_ckpt_options_t *opts) {
    opal_crs_base_clear_options(opts);
}

static void opal_crs_base_ckpt_options_destruct(opal_crs_base_ckpt_options_t *opts) {
    opal_crs_base_clear_options(opts);
}

OBJ_CLASS_INSTANCE(opal_crs_base_ckpt_options_t,
                   opal_object_t,
                   opal_crs_base_ckpt_options_construct,
                   opal_crs_base_ckpt_options_destruct);

/*
 * Utility functions
 */
char * opal_crs_base_unique_snapshot_name(pid_t pid)
{
    char * loc_str = NULL;
    
    asprintf(&loc_str, "opal_snapshot_%d.ckpt", pid);
    
    return loc_str;
}

int opal_crs_base_metadata_read_token(char *snapshot_loc, char * token, char ***value) {
    int ret, exit_status = OPAL_SUCCESS;
    FILE * meta_data = NULL;
    char * loc_token = NULL;
    char * loc_value = NULL;
    int argc = 0;

    /* Dummy check */
    if( NULL == token ) {
        goto cleanup;
    }

    /*
     * Open the metadata file
     */
    if( OPAL_SUCCESS != (ret = opal_crs_base_metadata_open(&meta_data, snapshot_loc, "r")) ) {
        opal_output(opal_crs_base_output,
                    "opal:crs:base: opal_crs_base_metadata_read_token: Error: Unable to open the metadata file\n");
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Extract each token and make the records
     */
    do {
        /* Get next token */
        if( OPAL_SUCCESS != metadata_extract_next_token(meta_data, &loc_token, &loc_value) ) {
            break;
        }

        /* Check token to see if it matches */
        if(0 == strncmp(token, loc_token, strlen(loc_token)) ) {
            opal_argv_append(&argc, value, loc_value);
        }
    } while(0 == feof(meta_data) );
    
 cleanup:
    if(NULL != meta_data) {
        fclose(meta_data);
        meta_data = NULL;
    }

    return exit_status;
}

int opal_crs_base_metadata_write_token(char *snapshot_loc, char * token, char *value) {
    int ret, exit_status = OPAL_SUCCESS;
    FILE * meta_data = NULL;

    /* Dummy check */
    if( NULL == token || NULL == value) {
        goto cleanup;
    }

    /*
     * Open the metadata file
     */
    if( OPAL_SUCCESS != (ret = opal_crs_base_metadata_open(&meta_data, snapshot_loc, "a")) ) {
        opal_output(opal_crs_base_output,
                    "opal:crs:base: opal_crs_base_metadata_write_token: Error: Unable to open the metadata file\n");
        exit_status = ret;
        goto cleanup;
    }

    fprintf(meta_data, "%s%s\n", token, value);

 cleanup:
    if(NULL != meta_data) {
        fclose(meta_data);
        meta_data = NULL;
    }

    return exit_status;
}

int opal_crs_base_extract_expected_component(char *snapshot_loc, char ** component_name, int *prev_pid)
{
    int exit_status = OPAL_SUCCESS;
    char **pid_argv = NULL;
    char **name_argv = NULL;

    opal_crs_base_metadata_read_token(snapshot_loc, CRS_METADATA_PID, &pid_argv);
    if( NULL != pid_argv && NULL != pid_argv[0] ) {
        *prev_pid = atoi(pid_argv[0]);
    } else {
        opal_output(0, "Error: expected_component: PID information unavailable!");
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    opal_crs_base_metadata_read_token(snapshot_loc, CRS_METADATA_COMP, &name_argv);
    if( NULL != name_argv && NULL != name_argv[0] ) {
        *component_name = strdup(name_argv[0]);
    } else {
        opal_output(0, "Error: expected_component: Component Name information unavailable!");
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

 cleanup:
    if( NULL != pid_argv ) {
        opal_argv_free(pid_argv);
        pid_argv = NULL;
    }

    if( NULL != name_argv ) {
        opal_argv_free(name_argv);
        name_argv = NULL;
    }

    return exit_status;
}

char * opal_crs_base_get_snapshot_directory(char *uniq_snapshot_name)
{
    char * dir_name = NULL;

    asprintf(&dir_name, "%s/%s", opal_crs_base_snapshot_dir, uniq_snapshot_name);

    return dir_name;
}

int    opal_crs_base_init_snapshot_directory(opal_crs_base_snapshot_t *snapshot)
{
    int ret, exit_status = OPAL_SUCCESS;
    mode_t my_mode = S_IRWXU; 
    char * pid_str = NULL;

    /*
     * Make the snapshot directory from the uniq_snapshot_name
     */
    if(OPAL_SUCCESS != (ret = opal_os_dirpath_create(snapshot->local_location, my_mode)) ) {
        opal_output(opal_crs_base_output,
                    "opal:crs:base: init_snapshot_directory: Error: Unable to create directory (%s)\n",
                    snapshot->local_location);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Initialize the metadata file at the top of that directory.
     * Add 'BASE' and 'PID'
     */
    if( NULL != last_metadata_file ) {
        free(last_metadata_file);
        last_metadata_file = NULL;
    }
    last_metadata_file = strdup(snapshot->local_location);

    if( OPAL_SUCCESS != (ret = opal_crs_base_metadata_write_token(NULL, CRS_METADATA_BASE, "") ) ) {
        opal_output(opal_crs_base_output,
                    "opal:crs:base: init_snapshot_directory: Error: Unable to write BASE to the file (%s/%s)\n",
                    snapshot->local_location, opal_crs_base_metadata_filename);
        exit_status = ret;
        goto cleanup;
    }

    asprintf(&pid_str, "%d", getpid());
    if( OPAL_SUCCESS != (ret = opal_crs_base_metadata_write_token(NULL, CRS_METADATA_PID, pid_str) ) ) {
        opal_output(opal_crs_base_output,
                    "opal:crs:base: init_snapshot_directory: Error: Unable to write PID (%s) to the file (%s/%s)\n",
                    pid_str, snapshot->local_location, opal_crs_base_metadata_filename);
        exit_status = ret;
        goto cleanup;
    }   

 cleanup:
    if( NULL != pid_str) {
        free(pid_str);
        pid_str = NULL;
    }

    return OPAL_SUCCESS;
}

int opal_crs_base_cleanup_append(char* filename, bool is_dir)
{
    if( NULL == filename ) {
        return OPAL_SUCCESS;
    }

    if( is_dir ) {
        opal_output_verbose(15, opal_crs_base_output,
                            "opal:crs: cleanup_append: Append Dir  <%s>\n",
                            filename);
        opal_argv_append_nosize(&cleanup_dir_argv, filename);
    } else {
        opal_output_verbose(15, opal_crs_base_output,
                            "opal:crs: cleanup_append: Append File <%s>\n",
                            filename);
        opal_argv_append_nosize(&cleanup_file_argv, filename);
    }

    return OPAL_SUCCESS;
}

int opal_crs_base_cleanup_flush(void)
{
    int argc, i;

    /*
     * Cleanup files first
     */
    if( NULL != cleanup_file_argv ) {
        argc = opal_argv_count(cleanup_file_argv);
        for( i = 0; i < argc; ++i) {
            opal_output_verbose(15, opal_crs_base_output,
                                "opal:crs: cleanup_flush: Remove File <%s>\n", cleanup_file_argv[i]);
            unlink(cleanup_file_argv[i]);
        }

        opal_argv_free(cleanup_file_argv);
        cleanup_file_argv = NULL;
    }

    /*
     * Try to cleanup directories next
     */
    if( NULL != cleanup_dir_argv ) {
        argc = opal_argv_count(cleanup_dir_argv);
        for( i = 0; i < argc; ++i) {
            opal_output_verbose(15, opal_crs_base_output,
                                "opal:crs: cleanup_flush: Remove Dir  <%s>\n", cleanup_dir_argv[i]);
            opal_os_dirpath_destroy(cleanup_dir_argv[i], true, NULL);
        }

        opal_argv_free(cleanup_dir_argv);
        cleanup_dir_argv = NULL;
    }

    return OPAL_SUCCESS;
}

char * opal_crs_base_state_str(opal_crs_state_type_t state)
{
    char *str = NULL;

    switch(state) {
    case OPAL_CRS_CHECKPOINT:
        str = strdup("Checkpoint");
        break;
    case OPAL_CRS_RESTART:
        str = strdup("Restart");
        break;
    case OPAL_CRS_CONTINUE:
        str = strdup("Continue");
        break;
    case OPAL_CRS_TERM:
        str = strdup("Terminate");
        break;
    case OPAL_CRS_RUNNING:
        str = strdup("Running");
        break;
    case OPAL_CRS_ERROR:
        str = strdup("Error");
        break;
    default:
        str = strdup("Unknown");
        break;
    }
    
    return str;
}

int opal_crs_base_copy_options(opal_crs_base_ckpt_options_t *from,
                                 opal_crs_base_ckpt_options_t *to)
{
    if( NULL == from ) {
        opal_output(opal_crs_base_output,
                    "opal:crs:base: copy_options: Error: from value is NULL\n");
        return OPAL_ERROR;
    }

    if( NULL == to ) {
        opal_output(opal_crs_base_output,
                    "opal:crs:base: copy_options: Error: to value is NULL\n");
        return OPAL_ERROR;
    }

    to->term = from->term;
    to->stop = from->stop;

    return OPAL_SUCCESS;
}

int opal_crs_base_clear_options(opal_crs_base_ckpt_options_t *target)
{
    if( NULL == target ) {
        opal_output(opal_crs_base_output,
                    "opal:crs:base: copy_options: Error: target value is NULL\n");
        return OPAL_ERROR;
    }

    target->term = false;
    target->stop = false;

    return OPAL_SUCCESS;
}


/******************
 * Local Functions
 ******************/
static int opal_crs_base_metadata_open(FILE **meta_data, char * location, char * mode)
{
    int exit_status = OPAL_SUCCESS;
    char * dir_name = NULL;

    if( NULL == location ) {
        if( NULL == last_metadata_file ) {
            opal_output(0, "Error: No metadata filename specified!");
            exit_status = OPAL_ERROR;
            goto cleanup;
        } else {
            location = last_metadata_file;
        }
    }

    /*
     * Find the snapshot directory, read the metadata file
     */
    asprintf(&dir_name, "%s/%s", location, opal_crs_base_metadata_filename);
    if (NULL == (*meta_data = fopen(dir_name, mode)) ) {
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

 cleanup:
    if( NULL != dir_name ) {
        free(dir_name);
        dir_name = NULL;
    }
    return exit_status;
}

static int metadata_extract_next_token(FILE *file, char **token, char **value)
{
    int exit_status = OPAL_SUCCESS;
    int max_len = 256;
    char * line = NULL;
    int line_len = 0;
    int c = 0, s = 0, v = 0;
    char *local_token = NULL;
    char *local_value = NULL;
    bool end_of_line = false;

    line = (char *) malloc(sizeof(char) * max_len);

 try_again:
    /*
     * If we are at the end of the file, then just return
     */
    if(0 != feof(file) ) {
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /*
     * Other wise grab the next token/value pair
     */
    if (NULL == fgets(line, max_len, file) ) {
        exit_status = OPAL_ERROR;
        goto cleanup;
    }
    line_len = strlen(line);
    /* Strip off the new line if it it there */
    if('\n' == line[line_len-1]) {
        line[line_len-1] = '\0';
        line_len--;
        end_of_line = true;
    }
    else {
        end_of_line = false;
    }

    /* Ignore lines with just '#' too */
    if(2 >= line_len)
        goto try_again;
    
    /*
     * Extract the token from the set
     */
    for(c = 0; 
        line[c] != ':' && 
            c < line_len;
        ++c) {
        ;
    }
    c += 2; /* For the ' ' and the '\0' */
    local_token = (char *)malloc(sizeof(char) * (c + 1));

    for(s = 0; s < c; ++s) {
        local_token[s] = line[s];
    }

    local_token[s] = '\0';
    *token = strdup(local_token);

    if( NULL != local_token) {
        free(local_token);
        local_token = NULL;
    }

    /*
     * Extract the value from the set
     */
    local_value = (char *)malloc(sizeof(char) * (line_len - c + 1));
    for(v = 0, s = c; 
        s < line_len;
        ++s, ++v) {
        local_value[v] = line[s];
    }

    while(!end_of_line) {
        if (NULL == fgets(line, max_len, file) ) {
            exit_status = OPAL_ERROR;
            goto cleanup;
        }
        line_len = strlen(line);
        /* Strip off the new line if it it there */
        if('\n' == line[line_len-1]) {
            line[line_len-1] = '\0';
            line_len--;
            end_of_line = true;
        }
        else {
            end_of_line = false;
        }
        
        local_value = (char *)realloc(local_value, sizeof(char) * line_len);
        for(s = 0;
            s < line_len;
            ++s, ++v) {
            local_value[v] = line[s];
        }
    }

    local_value[v] = '\0';
    *value = strdup(local_value);

 cleanup:
    if( NULL != local_token)
        free(local_token);
    if( NULL != local_value)
        free(local_value);
    if( NULL != line)
        free(line);

    return exit_status;
}
