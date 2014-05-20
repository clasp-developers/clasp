/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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

#include "orte_config.h"
#include "orte/constants.h"

#include <stdio.h>
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif  /* HAVE_SYS_PARAM_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include <errno.h>
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif  /* HAVE_DIRENT_H */
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif  /* HAVE_PWD_H */

#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/os_path.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/basename.h"
#include "opal/util/opal_environ.h"

#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"

#include "orte/util/session_dir.h"

/*******************************
 * Local function Declarations
 *******************************/
static int orte_create_dir(char *directory);

static bool orte_dir_check_file(const char *root, const char *path);
static bool orte_dir_check_file_output(const char *root, const char *path);

static char *orte_build_job_session_dir(char *top_dir,
                                        orte_process_name_t *proc,
                                        orte_jobid_t jobid);

#define OMPI_PRINTF_FIX_STRING(a) ((NULL == a) ? "(null)" : a)

/****************************
 * Funcationality
 ****************************/
/*
 * Check and create the directory requested
 */
static int orte_create_dir(char *directory)
{
    mode_t my_mode = S_IRWXU;  /* I'm looking for full rights */
    int ret;

    /* Sanity check before creating the directory with the proper mode,
     * Make sure it doesn't exist already */
    if( ORTE_ERR_NOT_FOUND != (ret = opal_os_dirpath_access(directory, my_mode)) ) {
        /* Failure because opal_os_dirpath_access() indicated that either:
         * - The directory exists and we can access it (no need to create it again), 
         *    return OPAL_SUCCESS, or
         * - don't have access rights, return OPAL_ERROR
         */
        if (ORTE_SUCCESS != ret) {
            ORTE_ERROR_LOG(ret);
        }
        return(ret);
    }
    
    /* Get here if the directory doesn't exist, so create it */
    if (ORTE_SUCCESS != (ret = opal_os_dirpath_create(directory, my_mode))) {
        ORTE_ERROR_LOG(ret);
    }
    return ret;
}

/*
 * Construct the fullpath to the session directory
 */
int
orte_session_dir_get_name(char **fulldirpath,
                          char **return_prefix,  /* This will come back as the valid tmp dir */
                          char **return_frontend,
                          char *hostid,
                          char *batchid, 
                          orte_process_name_t *proc) {
    char *hostname  = NULL, 
        *batchname = NULL,
        *sessions  = NULL, 
        *user      = NULL, 
        *prefix = NULL,
        *frontend = NULL,
        *jobfam = NULL,
        *job = NULL,
        *vpidstr = NULL;
    bool prefix_provided = false;
    int exit_status = ORTE_SUCCESS;
#ifndef __WINDOWS__
    int uid;
	struct passwd *pwdent;
#else
#define INFO_BUF_SIZE 256
    TCHAR info_buf[INFO_BUF_SIZE];
    DWORD info_buf_length = INFO_BUF_SIZE;
#endif
    
    /* Ensure that system info is set */
    orte_proc_info();

     /* get the name of the user */
#ifndef __WINDOWS__
    uid = getuid();
#ifdef HAVE_GETPWUID
    pwdent = getpwuid(uid);
#else
    pwdent = NULL;
#endif
    if (NULL != pwdent) {
        user = strdup(pwdent->pw_name);
    } else {
        orte_show_help("help-orte-runtime.txt",
                       "orte:session:dir:nopwname", true);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
#else 
    if (!GetUserName(info_buf, &info_buf_length)) {
        user = strdup("unknown");
    } else {
        user = strdup(info_buf);
    }
#endif
    
    /*
     * set the 'hostname'
     */
    if( NULL != hostid) { /* User specified version */
        hostname = strdup(hostid);
    }
    else {            /* check if it is set elsewhere */
        if( NULL != orte_process_info.nodename)
            hostname = strdup(orte_process_info.nodename);
        else {
            /* Couldn't find it, so fail */
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            exit_status = ORTE_ERR_BAD_PARAM;
            goto cleanup;
        }
    }
    
    /*
     * set the 'batchid'
     */
    if (NULL != batchid)
        batchname = strdup(batchid);
    else 
        batchname = strdup("0");

    /*
     * get the front part of the session directory
     * Will look something like:
     *    openmpi-sessions-USERNAME@HOSTNAME_BATCHID
     */
    if (NULL != orte_process_info.top_session_dir) {
        frontend = strdup(orte_process_info.top_session_dir);
    }
    else { /* If not set then construct it */
        if (0 > asprintf(&frontend, "openmpi-sessions-%s@%s_%s", user, hostname, batchname)) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            exit_status = ORTE_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        }
    }

    /*
     * Construct the session directory
     */
    /* If we were given a valid vpid then we can construct it fully into:
     *   openmpi-sessions-USERNAME@HOSTNAME_BATCHID/JOB-FAMILY/JOBID/VPID
     */
    if( NULL != proc) {
        if (ORTE_VPID_INVALID != proc->vpid) {
            
            if (0 > asprintf(&jobfam, "%d", ORTE_JOB_FAMILY(proc->jobid))) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                exit_status = ORTE_ERR_OUT_OF_RESOURCE;
                goto cleanup;
            }
            
            if (0 > asprintf(&job, "%d", ORTE_LOCAL_JOBID(proc->jobid))) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                exit_status = ORTE_ERR_OUT_OF_RESOURCE;
                goto cleanup;
            }
            
            if (ORTE_SUCCESS != orte_util_convert_vpid_to_string(&vpidstr, proc->vpid)) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                exit_status = ORTE_ERR_OUT_OF_RESOURCE;
                goto cleanup;
            }
            
            sessions = opal_os_path( false, frontend, jobfam, job, vpidstr, NULL );
            if( NULL == sessions ) {
                ORTE_ERROR_LOG(ORTE_ERROR);
                exit_status = ORTE_ERROR;
                goto cleanup;
            }
        }
        /* If we were given a valid jobid then we can construct it partially into:
         *   openmpi-sessions-USERNAME@HOSTNAME_BATCHID/JOB-FAMILY/JOBID
         */
        else if (ORTE_JOBID_INVALID != proc->jobid) {
            if (0 > asprintf(&jobfam, "%d", ORTE_JOB_FAMILY(proc->jobid))) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                exit_status = ORTE_ERR_OUT_OF_RESOURCE;
                goto cleanup;
            }
            
            if (0 > asprintf(&job, "%d", ORTE_LOCAL_JOBID(proc->jobid))) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                exit_status = ORTE_ERR_OUT_OF_RESOURCE;
                goto cleanup;
            }
            
            sessions = opal_os_path( false, frontend, jobfam, job, NULL );
            if( NULL == sessions ) {
                ORTE_ERROR_LOG(ORTE_ERROR);
                exit_status = ORTE_ERROR;
                goto cleanup;
            }
        } /* if both are invalid */
        else {
            sessions = strdup(frontend); /* must dup this to avoid double-free later */
        }
        
    }    /* If we were not given a proc at all, then we just set it to frontend
     */
    else {
        sessions = strdup(frontend); /* must dup this to avoid double-free later */
    }
    
    /*
     * If the user specified an invalid prefix, or no prefix at all
     * we need to keep looking
     */
    if( NULL != fulldirpath && NULL != *fulldirpath) {
        free(*fulldirpath);
        *fulldirpath = NULL;
    }

    if( NULL != return_prefix && NULL != *return_prefix) { /* use the user specified one, if available */ 
        prefix = strdup(*return_prefix); 
        prefix_provided = true;
    }
    /* Try to find a proper alternative prefix */
    else if (NULL != orte_process_info.tmpdir_base) { /* stored value */
        prefix = strdup(orte_process_info.tmpdir_base);
    }
    else if( NULL != getenv("OMPI_PREFIX_ENV") ) { /* OMPI Environment var */
        prefix = strdup(getenv("OMPI_PREFIX_ENV"));
    }
    else { /* General Environment var */
        prefix = strdup(opal_tmp_directory());
    }
    
    /* BEFORE doing anything else, check to see if this prefix is
     * allowed by the system
     */
    if (NULL != orte_prohibited_session_dirs) {
        char **list;
        int i, len;
        /* break the string into tokens - it should be
         * separated by ','
         */
        list = opal_argv_split(orte_prohibited_session_dirs, ',');
        len = opal_argv_count(list);
        /* cycle through the list */
        for (i=0; i < len; i++) {
            /* check if prefix matches */
            if (0 == strncmp(prefix, list[i], strlen(list[i]))) {
                /* this is a prohibited location */
                orte_show_help("help-orte-runtime.txt",
                               "orte:session:dir:prohibited",
                               true, prefix, orte_prohibited_session_dirs);
                return ORTE_ERR_FATAL;
            }
        }
        opal_argv_free(list);  /* done with this */
    }
    /*
     * Construct the absolute final path, if requested
     */
    if (NULL != fulldirpath) {
        *fulldirpath = opal_os_path(false, prefix, sessions, NULL);
    }

    /* 
     * Return the frontend and prefix, if user requested we do so 
     */ 
    if (NULL != return_frontend) { 
        *return_frontend = strdup(frontend); 
    } 
    if (!prefix_provided && NULL != return_prefix) { 
        *return_prefix = strdup(prefix); 
    } 

 cleanup:
    if(NULL != hostname)
        free(hostname);
    if(NULL != batchname)
        free(batchname);
    if(NULL != sessions)
        free(sessions);
    if(NULL != user)
        free(user);
    if (NULL != prefix) free(prefix);
    if (NULL != frontend) free(frontend);
    if (NULL != jobfam) free(jobfam);
    if (NULL != job) free(job);
    if (NULL != vpidstr) free(vpidstr);

    return exit_status;
}

/*
 * Construct the session directory and create it if necessary
 */
int orte_session_dir(bool create, 
                     char *prefix, char *hostid,
                     char *batchid, orte_process_name_t *proc)
{
    char *fulldirpath = NULL,
    *frontend     = NULL,
    *sav          = NULL;
    int rc = ORTE_SUCCESS;
    char *local_prefix = NULL;
    
    /* use the specified prefix, if one was given */
    if (NULL != prefix) {
        local_prefix = strdup(prefix);
    }
    
    /*
     * Get the session directory full name
     */
    if( ORTE_SUCCESS != ( rc = orte_session_dir_get_name(&fulldirpath, 
                                                         &local_prefix,
                                                         &frontend,
                                                         hostid, 
                                                         batchid, proc) ) ) {
        if (ORTE_ERR_FATAL == rc) {
            /* this indicates we should abort quietly */
            rc = ORTE_ERR_SILENT;
            goto cleanup;
        }
        /* otherwise, bark a little first */
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /*
     * Now that we have the full path, go ahead and create it if necessary
     */
    if( create ) {
        if( ORTE_SUCCESS != (rc = orte_create_dir(fulldirpath) ) ) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    /*
     * if we are not creating, then just verify that the path is OK
     */
    else {
        if( ORTE_SUCCESS != (rc = opal_os_dirpath_access(fulldirpath, 0) )) {
            /* it is okay for the path not to be found - don't error
             * log that case, but do error log others
             */
            if (ORTE_ERR_NOT_FOUND != rc) {
                ORTE_ERROR_LOG(rc);
            }
            goto cleanup;
        }
    }
    
    /*
     * If we are creating the directory tree, then force overwrite of the
     * global structure fields
     */
    if (create) {
    	if (NULL != orte_process_info.tmpdir_base) {
    	    free(orte_process_info.tmpdir_base);
    	    orte_process_info.tmpdir_base = NULL;
    	}
    	if (NULL != orte_process_info.top_session_dir) {
    	    free(orte_process_info.top_session_dir);
    	    orte_process_info.top_session_dir = NULL;
    	}
    }
    
    /* 
     * Update some of the global structures if they are empty
     */
    if (NULL == orte_process_info.tmpdir_base)
        orte_process_info.tmpdir_base = strdup(local_prefix);
    
    if (NULL == orte_process_info.top_session_dir)
        orte_process_info.top_session_dir = strdup(frontend);

    /*
     * Set the process session directory
     */
    if (ORTE_VPID_INVALID != proc->vpid) {
    	if (create) { /* overwrite if creating */
    	    if (NULL != orte_process_info.proc_session_dir) {
                free(orte_process_info.proc_session_dir);
                orte_process_info.proc_session_dir = NULL;
    	    }
    	}
    	if (NULL == orte_process_info.proc_session_dir) {
    	    orte_process_info.proc_session_dir = strdup(fulldirpath);
    	}
        
        /* Strip off last part of directory structure */
        sav = opal_dirname(fulldirpath);
        free(fulldirpath);
        fulldirpath = sav;
        sav = NULL;
    }
    
    /*
     * Set the job session directory
     */
    if (ORTE_JOBID_INVALID != proc->jobid) {
    	if (create) { /* overwrite if creating */
    	    if (NULL != orte_process_info.job_session_dir) {
                free(orte_process_info.job_session_dir);
                orte_process_info.job_session_dir = NULL;
    	    }
    	}
    	if (NULL == orte_process_info.job_session_dir) {
    	    orte_process_info.job_session_dir = strdup(fulldirpath);
    	}
    }
    
    if (orte_debug_flag) {
    	opal_output(0, "procdir: %s", 
                    OMPI_PRINTF_FIX_STRING(orte_process_info.proc_session_dir));
    	opal_output(0, "jobdir: %s", 
                    OMPI_PRINTF_FIX_STRING(orte_process_info.job_session_dir));
    	opal_output(0, "top: %s", 
                    OMPI_PRINTF_FIX_STRING(orte_process_info.top_session_dir));
    	opal_output(0, "tmp: %s", 
                    OMPI_PRINTF_FIX_STRING(orte_process_info.tmpdir_base));
    }
    
cleanup:
    if(NULL != fulldirpath)
        free(fulldirpath);
    if(NULL != frontend)
        free(frontend);
    
    return rc;
}

/*
 * A job has aborted - so force cleanup of the session directory
 */
int
orte_session_dir_cleanup(orte_jobid_t jobid)
{
    int rc = ORTE_SUCCESS;
    char *tmp;
    char *job_session_dir=NULL;

    /* need to setup the top_session_dir with the prefix */
    tmp = opal_os_path(false,
                       orte_process_info.tmpdir_base,
                       orte_process_info.top_session_dir, NULL);

    /* we can only blow away session directories for our job family */
    job_session_dir = orte_build_job_session_dir(tmp, ORTE_PROC_MY_NAME, jobid);
    if (NULL == job_session_dir) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }
    
    if (ORTE_JOBID_WILDCARD != jobid) {
        opal_os_dirpath_destroy(job_session_dir, true, orte_dir_check_file);
    } else {
        /* if we want the session_dir removed for ALL jobids, then
         * just recursively blow the whole session away for our job family,
         * saving only output files
         */
        opal_os_dirpath_destroy(job_session_dir, true, orte_dir_check_file_output);
    }
    
    /* now attempt to eliminate the top level directory itself - this
     * will fail if anything is present, but ensures we cleanup if
     * we are the last one out
     */
    opal_os_dirpath_destroy(tmp, false, orte_dir_check_file);

    if (NULL != job_session_dir && opal_os_dirpath_is_empty(job_session_dir)) {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: found job session dir empty - deleting");
    	}
    	rmdir(job_session_dir);
    } else {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: job session dir not empty - leaving");
    	}
        goto CLEANUP;
    }

    if (opal_os_dirpath_is_empty(tmp)) {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: found top session dir empty - deleting");
    	}
    	rmdir(tmp);
    } else {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: top session dir not empty - leaving");
    	}
    }

CLEANUP:
    free(tmp);
    if (NULL != job_session_dir) free(job_session_dir);
    return rc;
}


int
orte_session_dir_finalize(orte_process_name_t *proc)
{
    int rc;
    char *tmp;
    char *job_session_dir, *vpid, *proc_session_dir;

    /* need to setup the top_session_dir with the prefix */
    tmp = opal_os_path(false,
                       orte_process_info.tmpdir_base,
                       orte_process_info.top_session_dir, NULL);
    
    /* define the proc and job session directories for this process */
    if (ORTE_SUCCESS != (rc = orte_util_convert_vpid_to_string(&vpid, proc->vpid))) {
        ORTE_ERROR_LOG(rc);
        free(tmp);
        return rc;
    }
    job_session_dir = orte_build_job_session_dir(tmp, proc, proc->jobid);
    if( NULL == job_session_dir) {
        free(tmp);
        free(vpid);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    proc_session_dir = opal_os_path( false, job_session_dir, vpid, NULL );
    if( NULL == proc_session_dir ) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        free(tmp);
        free(vpid);
        free(job_session_dir);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    opal_os_dirpath_destroy(proc_session_dir,
                            false, orte_dir_check_file);
    opal_os_dirpath_destroy(job_session_dir,
                            false, orte_dir_check_file);
    opal_os_dirpath_destroy(tmp,
                            false, orte_dir_check_file);

    if (opal_os_dirpath_is_empty(proc_session_dir)) {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: found proc session dir empty - deleting");
    	}
    	rmdir(proc_session_dir);
    } else {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: proc session dir not empty - leaving");
    	}
        goto CLEANUP;
    }

    if (opal_os_dirpath_is_empty(job_session_dir)) {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: found job session dir empty - deleting");
    	}
    	rmdir(job_session_dir);
    } else {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: job session dir not empty - leaving");
    	}
        goto CLEANUP;
    }

    if (opal_os_dirpath_is_empty(tmp)) {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: found top session dir empty - deleting");
    	}
    	rmdir(tmp);
    } else {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: top session dir not empty - leaving");
    	}
    }

CLEANUP:
    free(tmp);
    free(vpid);
    free(job_session_dir);
    free(proc_session_dir);
    return ORTE_SUCCESS;
}

static bool 
orte_dir_check_file(const char *root, const char *path)
{
    /*
     * Keep:
     *  - files starting with "output-"
     *  - files that indicate abort
     */
    if( (0 == strncmp(path, "output-", strlen("output-"))) ||
        (0 == strcmp(path,  "abort"))) {
        return false;
    }

    return true;
}

static bool 
orte_dir_check_file_output(const char *root, const char *path)
{
    /*
     * Keep:
     *  - files starting with "output-"
     */
    if( 0 == strncmp(path, "output-", strlen("output-"))) {
        return false;
    }
    
    return true;
}

static char *orte_build_job_session_dir(char *top_dir,
                                        orte_process_name_t *proc,
                                        orte_jobid_t jobid)
{
    char *jobfam = NULL;
    char *job_session_dir;

    if (0 > asprintf(&jobfam, "%d", ORTE_JOB_FAMILY(proc->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }

    if (ORTE_JOBID_WILDCARD != jobid) {
        char *job = NULL;

        if (0 > asprintf(&job, "%d", ORTE_LOCAL_JOBID(jobid))) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            job_session_dir = NULL;
            goto out;
        }
        job_session_dir = opal_os_path(false, top_dir, jobfam, job, NULL);
        free(job);
        if (NULL == job_session_dir) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        }
    } else {
        job_session_dir = opal_os_path(false, top_dir, jobfam, NULL);
        if( NULL == job_session_dir) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        }
    }

out:
    free(jobfam);
    return job_session_dir;
}
