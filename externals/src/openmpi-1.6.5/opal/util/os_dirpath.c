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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "opal_config.h"

#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include <stdlib.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif  /* HAVE_DIRENT_H */

#include "opal/util/output.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/argv.h"
#include "opal/util/os_path.h"
#include "opal/constants.h"

static const char path_sep[] = OPAL_PATH_SEP;

int opal_os_dirpath_create(const char *path, const mode_t mode)
{
    struct stat buf;
    char **parts, *tmp;
    int i, len;
    int ret;

    if (NULL == path) { /* protect ourselves from errors */
        return(OPAL_ERROR);
    }

    if (0 == (ret = stat(path, &buf))) { /* already exists */
        if (mode == (mode & buf.st_mode)) { /* has correct mode */
            return(OPAL_SUCCESS);
        }
        if (0 == (ret = chmod(path, (buf.st_mode | mode)))) { /* successfully change mode */
            return(OPAL_SUCCESS);
        }
        opal_output(0,
                    "opal_os_dirpath_create: "
                    "Error: Unable to create directory (%s), unable to set the correct mode [%d]\n",
                    path, ret);
        return(OPAL_ERROR); /* can't set correct mode */
    }

    /* quick -- try to make directory */
    if (0 == mkdir(path, mode)) {
        return(OPAL_SUCCESS);
    }

    /* didnt work, so now have to build our way down the tree */
    /* Split the requested path up into its individual parts */

    parts = opal_argv_split(path, path_sep[0]);

    /* Ensure to allocate enough space for tmp: the strlen of the
       incoming path + 1 (for \0) */

    tmp = (char*)malloc(strlen(path) + 1);
    tmp[0] = '\0';

    /* Iterate through all the subdirectory names in the path,
       building up a directory name.  Check to see if that dirname
       exists.  If it doesn't, create it. */

    /* Notes about stat(): Windows has funny definitions of what will
       return 0 from stat().  "C:" will return failure, while "C:\"
       will return success.  Similarly, "C:\foo" will return success,
       while "C:\foo\" will return failure (assuming that a folder
       named "foo" exists under C:\).

       POSIX implementations of stat() are generally a bit more
       forgiving; most will return true for "/foo" and "/foo/"
       (assuming /foo exists).  But we might as well abide by the same
       rules as Windows and generally disallow checking for names
       ending with path_sep (the only possible allowable one is
       checking for "/", which is the root directory, and is
       guaranteed to exist on valid POSIX filesystems, and is
       therefore not worth checking for). */

    len = opal_argv_count(parts);
    for (i = 0; i < len; ++i) {
        if (i == 0) {

#ifdef __WINDOWS__
            /* In the Windows case, check for "<drive>:" case (i.e.,
               an absolute pathname).  If this is the case, ensure
               that it ends in a path_sep. */

            if (2 == strlen(parts[0]) && isalpha(parts[0][0]) &&
                ':' == parts[0][1]) {
                strcat(tmp, parts[i]);
                strcat(tmp, path_sep);
            }
            
            /* Otherwise, it's a relative path.  Per the comment
               above, we don't want a '\' at the end, so just append
               this part. */

            else {
                strcat(tmp, parts[i]);
            }
#else
            /* If in POSIX-land, ensure that we never end a directory
               name with path_sep */

            if ('/' == path[0]) {
                strcat(tmp, path_sep);
            }
            strcat(tmp, parts[i]);
#endif
        }

        /* If it's not the first part, ensure that there's a
           preceeding path_sep and then append this part */

        else {
            if (path_sep[0] != tmp[strlen(tmp) - 1]) {
                strcat(tmp, path_sep);
            }
            strcat(tmp, parts[i]);
        }

        /* Now that we finally have the name to check, check it.
           Create it if it doesn't exist. */
        if (0 != (ret = stat(tmp, &buf)) ) {
            if (0 != (ret = mkdir(tmp, mode) && 0 != stat(tmp, &buf))) { 
                opal_output(0,
                            "opal_os_dirpath_create: "
                            "Error: Unable to create the sub-directory (%s) of (%s), mkdir failed [%d]\n",
                            tmp, path, ret);
                opal_argv_free(parts);
                free(tmp);
                return OPAL_ERROR;
            }
        }
    }

    /* All done */

    opal_argv_free(parts);
    free(tmp);
    return OPAL_SUCCESS;
}

/** 
 * This function attempts to remove a directory along with all the
 * files in it.  If the recursive variable is non-zero, then it will
 * try to recursively remove all directories.  If provided, the
 * callback function is executed prior to the directory or file being
 * removed.  If the callback returns non-zero, then no removal is
 * done.
 */
int opal_os_dirpath_destroy(const char *path,
                            bool recursive,
                            opal_os_dirpath_destroy_callback_fn_t cbfunc)
{
    int rc, exit_status = OPAL_SUCCESS;
    bool is_dir = false;

    if (NULL == path) {  /* protect against error */
        return OPAL_ERROR;
    }

    /*
     * Make sure we have access to the the base directory
     */
    if( OPAL_SUCCESS != (rc = opal_os_dirpath_access(path, 0) ) ) {
        exit_status = rc;
        goto cleanup;
    }

#ifndef __WINDOWS__
    {
        DIR *dp;
        struct dirent *ep;
        char *filenm;
#ifndef HAVE_STRUCT_DIRENT_D_TYPE 
        struct stat buf;
#endif

        /* Open up the directory */
        dp = opendir(path);
        if (NULL == dp) {
            return OPAL_ERROR;
        }

        while (NULL != (ep = readdir(dp)) ) {
            /* skip:
            *  - . and ..
            */
            if ((0 == strcmp(ep->d_name, ".")) ||
                (0 == strcmp(ep->d_name, "..")) ) {
                    continue;
            }
        
            /* Check to see if it is a directory */
            is_dir = false;

            /* Create a pathname.  This is not always needed, but it makes
             * for cleaner code just to create it here.  Note that we are
             * allocating memory here, so we need to free it later on. 
             */
            filenm = opal_os_path(false, path, ep->d_name, NULL);
#ifdef HAVE_STRUCT_DIRENT_D_TYPE
            if (DT_DIR == ep->d_type) {
                is_dir = true;
            }
#else /* have dirent.d_type */
            rc = stat(filenm, &buf);
            if (rc < 0 || S_ISDIR(buf.st_mode)) {
                is_dir = true;
            }
#endif /* have dirent.d_type */        

            /*
              * If not recursively decending, then if we find a directory then fail
              * since we were not told to remove it.
              */
            if( is_dir && !recursive) {
                /* Set the error indicating that we found a directory,
                 * but continue removing files
                 */
                exit_status = OPAL_ERROR;
                free(filenm);
                continue;
            }

            /* Will the caller allow us to remove this file/directory? */
            if(NULL != cbfunc) {
                /*
                 * Caller does not wish to remove this file/directory,
                 * continue with the rest of the entries
                 */
                if( ! (cbfunc(path, ep->d_name)) ) {
                    free(filenm);
                    continue;
                }
            }
            /* Directories are recursively destroyed */
            if(is_dir) {
                rc = opal_os_dirpath_destroy(filenm, recursive, cbfunc);
                free(filenm);
                if (OPAL_SUCCESS != rc) {
                    exit_status = rc;
                    closedir(dp);
                    goto cleanup;
                }
            }
            /* Files are removed right here */
            else {
                if( 0 != (rc = unlink(filenm) ) ) {
                    exit_status = OPAL_ERROR;
                }
                free(filenm);
            }
        }
    
        /* Done with this directory */
        closedir(dp);
    }
#else
    {
        char search_path[MAX_PATH];
        HANDLE file;
        WIN32_FIND_DATA file_data;
        TCHAR *file_name;
    
        strncpy(search_path, path, strlen(path)+1);
        strncat (search_path, OPAL_PATH_SEP"*", 3);
        file = FindFirstFile(search_path, &file_data);
    
        if (INVALID_HANDLE_VALUE == file) {
            FindClose(file);
            return OPAL_ERROR;
        } 
    
        do {
            /* Skip . and .. */
            if ((0 == strcmp(file_data.cFileName, ".")) ||
                (0 == strcmp(file_data.cFileName, "..")) ) {
                    continue;
            }
        
            is_dir = false;
            if(file_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
                is_dir = true;
            } 
            /*
             * If not recursively decending, then if we find a directory then fail
             * since we were not told to remove it.
             */
            if( is_dir && !recursive) {
                /* Set the error indicating that we found a directory,
                 * but continue removing files
                 */
                exit_status = OPAL_ERROR;
                continue;
            }
        
            /* Will the caller allow us to remove this file/directory */
            if( NULL != cbfunc) {
                /*
                 * Caller does not wish to remove this file/directory,
                 * continue with the rest of the entries
                 */
                if( !cbfunc(path, file_data.cFileName) ) {
                    continue;
                }
            }

            file_name = opal_os_path(false, path, file_data.cFileName, NULL);
            if( is_dir ) {
                if( OPAL_SUCCESS != (rc = opal_os_dirpath_destroy(file_name,
                                                                  recursive,
                                                                  cbfunc) ) ) {
                    exit_status = rc;
                    free(file_name);
                    FindClose(file);
                    goto cleanup;
                }
            } else {
                DeleteFile(file_name);
            }
            free(file_name);
        } while( 0 != FindNextFile(file, &file_data) );
    
        FindClose(file);
    }
#endif
    
 cleanup:
    
    /*
     * If the directory is empty, them remove it
     */
    if(opal_os_dirpath_is_empty(path)) {
        rmdir(path);
    }

    return exit_status;
}

bool opal_os_dirpath_is_empty(const char *path ) {
#ifndef __WINDOWS__
    DIR *dp;
    struct dirent *ep;
    
    if (NULL != path) {  /* protect against error */
    	dp = opendir(path);
    	if (NULL != dp) {
    	    while ((ep = readdir(dp))) {
        		if ((0 != strcmp(ep->d_name, ".")) &&
        		    (0 != strcmp(ep->d_name, ".."))) {
                            closedir(dp);
        		    return false;
        		}
    	    }
    	    closedir(dp);
    	    return true;
    	}
    	return false;
    }

    return true;
#else 
    char search_path[MAX_PATH];
    HANDLE file;
    WIN32_FIND_DATA file_data;
    bool found = false;

    if (NULL != path) {
        strncpy(search_path, path, strlen(path)+1);
        strncat (search_path, "\\*", 3);

        file = FindFirstFile(search_path, &file_data);
        if (INVALID_HANDLE_VALUE == file) {
            goto cleanup;
        }

        do {
            if (0 != strcmp(file_data.cFileName, ".") || 0 != strcmp(file_data.cFileName, "..")) {
                found = true;
                goto cleanup;
            }
        } while (0 != FindNextFile(file, &file_data));
    }
  cleanup:
    FindClose(file);
    return found;
#endif /* ifndef __WINDOWS__ */
}

int opal_os_dirpath_access(const char *path, const mode_t in_mode ) {
#ifndef __WINDOWS__
    struct stat buf;
#else
#  ifndef _MSC_VER
    struct stat buf;
#  else
    struct __stat64 buf;
#  endif
#endif
    mode_t loc_mode = S_IRWXU;  /* looking for full rights */

    /*
     * If there was no mode specified, use the default mode
     */
    if( 0 != in_mode ) {
        loc_mode = in_mode;
    }

#ifndef __WINDOWS__
    if (0 == stat(path, &buf)) { /* exists - check access */
#else
    if (0 == _stat64(path, &buf)) { /* exist -- check */
#endif
        if ((buf.st_mode & loc_mode) == loc_mode) { /* okay, I can work here */
            return(OPAL_SUCCESS);
        }
        else {
            /* Don't have access rights to the existing path */
            return(OPAL_ERROR);
        }
    }
    else {
        /* We could not find the path */
        return( OPAL_ERR_NOT_FOUND );
    }
}

