/*
 * 
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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * $Id: orte_universe_setup_file I/O functions $
 * 
 */
#include "orte_config.h"
#include "orte/constants.h"

#include <stdio.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <stdarg.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif  /* HAVE_DIRENT_H */

#include "opal/util/os_path.h"
#include "opal/util/output.h"
#include "opal/util/os_dirpath.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/routed/routed.h"

#include "orte/util/proc_info.h"
#include "orte/util/hnp_contact.h"

#define ORTE_HNP_CONTACT_FILE_MAX_LINE_LENGTH 1024

/* instantiate the hnp_contact object */
static void orte_hnp_contact_construct(orte_hnp_contact_t *ptr)
{
    ptr->name.jobid = ORTE_JOBID_INVALID;
    ptr->name.vpid = ORTE_VPID_INVALID;
    ptr->rml_uri = NULL;
}
static void orte_hnp_contact_destruct(orte_hnp_contact_t *ptr)
{
    if (NULL != ptr->rml_uri) free(ptr->rml_uri);
}
OBJ_CLASS_INSTANCE(orte_hnp_contact_t,
                   opal_list_item_t,
                   orte_hnp_contact_construct,
                   orte_hnp_contact_destruct);


static char *orte_getline(FILE *fp);

int orte_write_hnp_contact_file(char *filename)
{
    FILE *fp;
    char *my_uri;

    fp = fopen(filename, "w");
    if (NULL == fp) {
        opal_output( 0, "Impossible to open the file %s in write mode\n",
                     filename );
        ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
        return ORTE_ERR_FILE_OPEN_FAILURE;
    }

    my_uri = orte_rml.get_contact_info();
    if (NULL == my_uri) {
        return ORTE_ERROR;
    }
    fprintf(fp, "%s\n", my_uri);
    free(my_uri);

    fprintf(fp, "%lu\n", (unsigned long)orte_process_info.pid);
    fclose(fp);

    return ORTE_SUCCESS;
}

int orte_read_hnp_contact_file(char *filename, orte_hnp_contact_t *hnp, bool connect)
{
    char *hnp_uri, *pidstr;
    FILE *fp;
    int rc;

    fp = fopen(filename, "r");
    if (NULL == fp) { /* failed on first read - wait and try again */
        fp = fopen(filename, "r");
        if (NULL == fp) { /* failed twice - give up */
            return ORTE_ERR_FILE_OPEN_FAILURE;
        }
    }

    hnp_uri = orte_getline(fp);
    if (NULL == hnp_uri) {
        ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
        fclose(fp);
        return ORTE_ERR_FILE_READ_FAILURE;
    }

    /* get the pid */
    pidstr = orte_getline(fp);
    if (NULL == pidstr) {
        ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
        fclose(fp);
        return ORTE_ERR_FILE_READ_FAILURE;
    }
    hnp->pid = (pid_t)atol(pidstr);
    fclose(fp);

    if (connect) {
        /* set the contact info into the comm hash tables*/
        if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(hnp_uri))) {
            ORTE_ERROR_LOG(rc);
            return(rc);
        }
        
        /* extract the HNP's name and store it */
        if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(hnp_uri, &hnp->name, NULL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* set the route to be direct */
        if (ORTE_SUCCESS != (rc = orte_routed.update_route(&hnp->name, &hnp->name))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    hnp->rml_uri = hnp_uri;
    
    return ORTE_SUCCESS;
}

static char *orte_getline(FILE *fp)
{
    char *ret, *buff;
    char input[ORTE_HNP_CONTACT_FILE_MAX_LINE_LENGTH];

    ret = fgets(input, ORTE_HNP_CONTACT_FILE_MAX_LINE_LENGTH, fp);
    if (NULL != ret) {
	   input[strlen(input)-1] = '\0';  /* remove newline */
	   buff = strdup(input);
	   return buff;
    }
    
    return NULL;
}


int orte_list_local_hnps(opal_list_t *hnps, bool connect)
{
    int ret;
#ifndef __WINDOWS__
    DIR *cur_dirp = NULL;
    struct dirent * dir_entry;
#else
    HANDLE hFind = INVALID_HANDLE_VALUE;
    WIN32_FIND_DATA file_data;
#endif  /* __WINDOWS__ */
    char *contact_filename = NULL;
    orte_hnp_contact_t *hnp;
    char *headdir;
    
#if !defined(__WINDOWS__)

    /*
     * Check to make sure we have access to the top-level directory
     */
    headdir = opal_os_path(false, orte_process_info.tmpdir_base, orte_process_info.top_session_dir, NULL);
    
    if( ORTE_SUCCESS != (ret = opal_os_dirpath_access(headdir, 0) )) {
        /* it is okay not to find this as there may not be any
         * HNP's present, and we don't write our own session dir
         */
        if (ORTE_ERR_NOT_FOUND != ret) {
            ORTE_ERROR_LOG(ret);
        }
        goto cleanup;
    }
    
    /*
     * Open up the base directory so we can get a listing
     */
    if( NULL == (cur_dirp = opendir(headdir)) ) {
        goto cleanup;
    }
    /*
     * For each directory
     */
    while( NULL != (dir_entry = readdir(cur_dirp)) ) {
        
        /*
         * Skip the obvious
         */
        if( 0 == strncmp(dir_entry->d_name, ".", strlen(".")) ||
            0 == strncmp(dir_entry->d_name, "..", strlen("..")) ) {
            continue;
        }
        
        /*
         * See if a contact file exists in this directory and read it
         */
        contact_filename = opal_os_path( false, headdir,
                                         dir_entry->d_name, "contact.txt", NULL );
        
        hnp = OBJ_NEW(orte_hnp_contact_t);
        if (ORTE_SUCCESS == (ret = orte_read_hnp_contact_file(contact_filename, hnp, connect))) {
            opal_list_append(hnps, &(hnp->super));
        } else {
            OBJ_RELEASE(hnp);
        }
     }
#else
    /*
     * Open up the base directory so we can get a listing.
     *
     * On Windows if we want to parse the content of a directory the filename
     * should end with the "*". Otherwise we will only open the directory
     * structure (and not the content).
     */
    char *subdirs = opal_os_path(false, orte_process_info.tmpdir_base, orte_process_info.top_session_dir, "*", NULL);
    headdir = opal_os_path(false, orte_process_info.tmpdir_base, orte_process_info.top_session_dir, NULL);

    hFind = FindFirstFile( subdirs, &file_data );
    if( INVALID_HANDLE_VALUE == hFind ) {
        goto cleanup;
    }
    
    /*
     * For each directory
     */
    do {
        /*
         * Skip the obvious
         */
        if( 0 == strncmp(file_data.cFileName, ".", strlen(".")) ||
            0 == strncmp(file_data.cFileName, "..", strlen("..")) ) {
            continue;
        }
        
        /*
         * See if a contact file exists in this directory and read it
         */
        contact_filename = opal_os_path( false, headdir,
                                         file_data.cFileName, "contact.txt", NULL );
        
        hnp = OBJ_NEW(orte_hnp_contact_t);
        if (ORTE_SUCCESS == (ret = orte_read_hnp_contact_file(contact_filename, hnp, connect))) {
            opal_list_append(hnps, &(hnp->super));
        } else {
            OBJ_RELEASE(hnp);
        }
    } while( 0 != FindNextFile( hFind, &file_data ) );
    
#endif  /* !defined(__WINDOWS__) */
    
cleanup:
#ifndef __WINDOWS__
    if( NULL != cur_dirp )
        closedir(cur_dirp);
#else
    FindClose(hFind);
#endif  /* __WINDOWS__ */
    free(headdir);
    if( NULL != contact_filename)
        free(contact_filename);
    
    return (opal_list_is_empty(hnps) ? ORTE_ERR_NOT_FOUND : ORTE_SUCCESS);
}
