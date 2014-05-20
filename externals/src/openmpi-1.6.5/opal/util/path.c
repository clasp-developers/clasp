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
 * Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SHLWAPI_H
#include <shlwapi.h>
#endif
#ifdef HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_SYS_VFS_H
#include <sys/vfs.h>
#endif
#ifdef HAVE_SYS_STATFS_H
#include <sys/statfs.h>
#endif
#ifdef HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif

#include "opal/util/output.h"
#include "opal/util/path.h"
#include "opal/util/os_path.h"
#include "opal/util/argv.h"

static void path_env_load(char *path, int *pargc, char ***pargv);
static char *list_env_get(char *var, char **list);

bool opal_path_is_absolute( const char *path )
{
#if defined(__WINDOWS__)
    /* On Windows an absolute path always start with [a-z]:\ or with \\ */
    if( (isalpha(path[0]) && (':' == path[1])) ||
        ('\\' == path[0]) && ('\\' == path[1]) ) return true;
#else
    if( OPAL_PATH_SEP[0] == *path ) {
        return true;
    }
#endif  /* defined(__WINDOWS__) */
    return false;
}

/**
 *  Locates a file with certain permissions
 */
char *opal_path_find(char *fname, char **pathv, int mode, char **envv)
{
    char *fullpath;  
    char *delimit;  
    char *env;     
    char *pfix;   
    int i;

    /* If absolute path is given, return it without searching. */
    if( opal_path_is_absolute(fname) ) {
        return opal_path_access(fname, NULL, mode);
    }

    /* Initialize. */

    fullpath = NULL;
    i = 0;

    /* Consider each directory until the file is found.  Thus, the
       order of directories is important. */

    while (pathv[i] && NULL == fullpath) {

        /* Replace environment variable at the head of the string. */
        if ('$' == *pathv[i]) {
            delimit = strchr(pathv[i], OPAL_PATH_SEP[0]);
            if (delimit) {
                *delimit = '\0';
            }
            env = list_env_get(pathv[i]+1, envv);
            if (delimit) {
                *delimit = OPAL_PATH_SEP[0];
            }
            if (NULL != env) {
                if (!delimit) {
                    fullpath = opal_path_access(fname, env, mode);
                } else {
                    pfix = (char*) malloc(strlen(env) + strlen(delimit) + 1);
                    if (NULL == pfix) {
                        return NULL;
                    }
                    strcpy(pfix, env);
                    strcat(pfix, delimit);
                    fullpath = opal_path_access(fname, pfix, mode);
                    free(pfix);
                }
            }
        }
        else {
            fullpath = opal_path_access(fname, pathv[i], mode);
        }
        i++;
    }
    return opal_make_filename_os_friendly(fullpath);
}

/*
 * Locates a file with certain permissions from a list of search paths
 */
char *opal_path_findv(char *fname, int mode, char **envv, char *wrkdir)
{
    char **dirv;     
    char *fullpath; 
    char *path;    
    int dirc;    
    int i;
    bool found_dot = false;

    /* Set the local search paths. */

    dirc = 0;
    dirv = NULL;

    if (NULL != (path = list_env_get("PATH", envv))) {
        path_env_load(path, &dirc, &dirv);
    }

    /* Replace the "." path by the working directory. */

    if (NULL != wrkdir) { 
        for (i = 0; i < dirc; ++i) {
            if (0 == strcmp(dirv[i], ".")) {
                found_dot = true;
                free(dirv[i]);
                dirv[i] = strdup(wrkdir);
                if (NULL == dirv[i]){
                    return NULL;
                }
            }
        }
    }

    /* If we didn't find "." in the path and we have a wrkdir, append
       the wrkdir to the end of the path */

    if (!found_dot && NULL != wrkdir) {
        opal_argv_append(&dirc, &dirv, wrkdir);
    }

    if(NULL == dirv)
        return NULL;
    fullpath = opal_path_find(fname, dirv, mode, envv);
    opal_argv_free(dirv);
    return fullpath;
}


/**
 *  Forms a complete pathname and checks it for existance and
 *  permissions
 *            
 *  Accepts:
 *      -fname File name
 *      -path  Path prefix 
 *      -mode  Target permissions which must be satisfied
 *
 *  Returns:
 *      -Full pathname of located file Success
 *      -NULL Failure
 */
char *opal_path_access(char *fname, char *path, int mode)
{
    char *fullpath = NULL;
    struct stat buf;
    
    /* Allocate space for the full pathname. */
    if (NULL == path) {
        fullpath = opal_os_path(false, fname, NULL);
    } else {
        fullpath = opal_os_path(false, path, fname, NULL);
    }
    if (NULL == fullpath)
        return NULL;
    
    /* first check to see - is this a file or a directory? We
     * only want files
     */
    if (0 != stat(fullpath, &buf)) {
        /* couldn't stat the path - obviously, this also meets the
         * existence check, if that was requested
         */
        free(fullpath);
        return NULL;
    }
    
    if (!(S_IFREG & buf.st_mode) &&
        !(S_IFLNK & buf.st_mode)) {
        /* this isn't a regular file or a symbolic link, so
         * ignore it 
         */
        free(fullpath);
        return NULL;
    }
    
    /* check the permissions */
    if ((X_OK & mode) && !(S_IXUSR & buf.st_mode)) {
        /* if they asked us to check executable permission,
         * and that isn't set, then return NULL
         */
        free(fullpath);
        return NULL;
    }
    if ((R_OK & mode) && !(S_IRUSR & buf.st_mode)) {
        /* if they asked us to check read permission,
         * and that isn't set, then return NULL
         */
        free(fullpath);
        return NULL;
    }
    if ((W_OK & mode) && !(S_IWUSR & buf.st_mode)) {
        /* if they asked us to check write permission,
         * and that isn't set, then return NULL
         */
        free(fullpath);
        return NULL;
    }
    
    /* must have met all criteria! */
    return fullpath;
}


/**
 *
 *  Loads argument array with $PATH env var.
 *
 *  Accepts
 *      -path String contiaing the $PATH
 *      -argc Pointer to argc
 *      -argv Pointer to list of argv
 */
static void path_env_load(char *path, int *pargc, char ***pargv)
{
    char *p;
    char saved;

    if (NULL == path) {
        *pargc = 0;
        return;
    }

    /* Loop through the paths (delimited by PATHENVSEP), adding each
       one to argv. */

    while ('\0' != *path) {

        /* Locate the delimiter. */

        for (p = path; *p && (*p != OPAL_ENV_SEP); ++p) {
            continue;
        }

        /* Add the path. */

        if (p != path) {
            saved = *p;
            *p = '\0';
            opal_argv_append(pargc, pargv, path);
            *p = saved;
            path = p;
        }

        /* Skip past the delimiter, if present. */

        if (*path) {
            ++path;
        }
    }
}


/**
 *  Gets value of variable in list or environment. Looks in the list first
 *
 *  Accepts:
 *      -var  String variable
 *      -list Pointer to environment list
 *
 *  Returns:
 *      -List Pointer to environment list Success
 *      -NULL Failure
 */
static char *list_env_get(char *var, char **list)
{
    size_t n;

    if (NULL != list) {
        n = strlen(var);

        while (NULL != *list) {
            if ((0 == strncmp(var, *list, n)) && ('=' == (*list)[n])) {
                return (*list + n + 1);
            }
            ++list;
        }
    }
    return getenv(var);
}

/**
 * Try to figure out the absolute path based on the application name
 * (usually argv[0]). If the path is already absolute return a copy, if
 * it start with . look into the current directory, if not dig into
 * the $PATH.
 * In case of error or if executable was not found (as an example if
 * the application did a cwd between the start and this call), the
 * function will return NULL. Otherwise, an newly allocated string
 * will be returned.
 */
char* opal_find_absolute_path( char* app_name )
{
    char* abs_app_name;
    char cwd[OPAL_PATH_MAX], *pcwd;

    if( opal_path_is_absolute(app_name) ) { /* already absolute path */
        abs_app_name = app_name;
    } else if ( '.' == app_name[0] ||
               NULL != strchr(app_name, OPAL_PATH_SEP[0])) {
        /* the app is in the current directory or below it */
        pcwd = getcwd( cwd, OPAL_PATH_MAX );
        if( NULL == pcwd ) {
            /* too bad there is no way we can get the app absolute name */
            return NULL;
        }
        abs_app_name = opal_os_path( false, pcwd, app_name, NULL );
    } else {
        /* Otherwise try to search for the application in the PATH ... */
        abs_app_name = opal_path_findv( app_name, X_OK, NULL, NULL );
    }
    
    if( NULL != abs_app_name ) {
        char* resolved_path = (char*)malloc(OPAL_PATH_MAX);
#if !defined(__WINDOWS__)
        realpath( abs_app_name, resolved_path );
#else
#ifdef HAVE_SHLWAPI_H
		PathCanonicalize(resolved_path, abs_app_name);
#endif
#endif  /* !defined(__WINDOWS__) */
        if( abs_app_name != app_name ) free(abs_app_name);
        return resolved_path;
    }
    return NULL;
}


/**
 * @brief Figure out, whether fname is on network file system
 *
 * Try to figure out, whether the file name specified through fname is
 * on any network file system (currently NFS, Lustre, Panasas and GPFS).
 *
 * If the file is not created, the parent directory is checked.
 * This allows checking for NFS prior to opening the file.
 *
 * @param[in]     fname        File name to check
 *
 * @retval true                If fname is on NFS, Lustre, Panasas or GPFS
 * @retval false               otherwise
 *
 *
 * Linux:
 *   statfs(const char *path, struct statfs *buf);
 *          with fsid_t  f_fsid;  (in kernel struct{ int val[2] };)
 *          return 0 success, -1 on failure with errno set.
 *   statvfs (const char *path, struct statvfs *buf);
 *          with unsigned long  f_fsid;   -- returns wrong info
 *          return 0 success, -1 on failure with errno set.
 * Solaris:
 *   statvfs (const char *path, struct statvfs *buf);
 *          with f_basetype, contains a string of length FSTYPSZ
 *          return 0 success, -1 on failure with errno set.
 * FreeBSD:
 *   statfs(const char *path, struct statfs *buf);
 *          with f_fstypename, contains a string of length MFSNAMELEN
 *          return 0 success, -1 on failure with errno set.
 *          compliant with: 4.4BSD.
 * Mac OSX (10.6.2):
 *   statvfs(const char * restrict path, struct statvfs * restrict buf);
 *          with fsid    Not meaningful in this implementation.
 *          is just a wrapper around statfs()
 *   statfs(const char *path, struct statfs *buf);
 *          with f_fstypename, contains a string of length MFSTYPENAMELEN
 *          return 0 success, -1 on failure with errno set.
 * Windows (interix):
 *      statvfs(const char *path, struct statvfs *buf);
 *          with unsigned long f_fsid
 *          return 0 success, -1 on failure with errno set.
 */
#ifndef LL_SUPER_MAGIC
#define LL_SUPER_MAGIC                    0x0BD00BD0     /* Lustre magic number */
#endif
#ifndef NFS_SUPER_MAGIC
#define NFS_SUPER_MAGIC                   0x6969
#endif
#ifndef PAN_KERNEL_FS_CLIENT_SUPER_MAGIC
#define PAN_KERNEL_FS_CLIENT_SUPER_MAGIC  0xAAD7AAEA     /* Panasas FS */
#endif
#ifndef GPFS_SUPER_MAGIC
#define GPFS_SUPER_MAGIC  0x47504653    /* Thats GPFS in ASCII */
#endif

#define MASK2        0xffff
#define MASK4    0xffffffff

bool opal_path_nfs(char *fname)
{
#if !defined(__WINDOWS__)
    int i;
    int rc;
    int trials;
    char * file = strdup (fname);
#if defined(__SVR4) && defined(__sun)
    struct statvfs buf;
#elif defined(__linux__) || defined (__BSD) || (defined(__APPLE__) && defined(__MACH__))
    struct statfs buf;
#endif
    /*
     * Be sure to update the test (test/util/opal_path_nfs.c) 
     * while adding a new Network/Cluster Filesystem here
     */
    static struct fs_types_t {
        unsigned long long f_fsid;
        unsigned long long f_mask;
        const char * f_fsname;
    } fs_types[] = {
        {LL_SUPER_MAGIC,                   MASK4, "lustre"},
        {NFS_SUPER_MAGIC,                  MASK2, "nfs"},
        {PAN_KERNEL_FS_CLIENT_SUPER_MAGIC, MASK4, "panfs"},
        {GPFS_SUPER_MAGIC, MASK4, "gpfs"}
    };
#define FS_TYPES_NUM (int)(sizeof (fs_types)/sizeof (fs_types[0]))

    /*
     * First, get the OS-dependent struct stat(v)fs buf
     * This may return the ESTALE error on NFS, if the underlying file/path has changed
     */
again:
    trials = 5;
    do {
#if defined(__SVR4) && defined(__sun)
        rc = statvfs (file, &buf);
#elif defined(__linux__) || defined (__BSD) || (defined(__APPLE__) && defined(__MACH__))
        rc = statfs (file, &buf);
#endif
    } while (-1 == rc && ESTALE == errno && (0 < --trials));

    /* In case some error with the current filename, try the directory */
    if (-1 == rc) {
        char * last_sep;

        OPAL_OUTPUT_VERBOSE((10, 0, "opal_path_nfs: stat(v)fs on file:%s failed errno:%d directory:%s\n",
                             fname, errno, file));

        last_sep = strrchr(file, OPAL_PATH_SEP[0]);
        /* Stop the search, when we have searched past root '/' */
        if (NULL == last_sep || (1 == strlen(last_sep) && 
            OPAL_PATH_SEP[0] == *last_sep)) {
            free (file); 
            return false;
        }
        *last_sep = '\0';

        goto again;
    }

    /* Next, extract the magic value */
#if defined(__SVR4) && defined(__sun)
    for (i = 0; i < FS_TYPES_NUM; i++) 
        if (0 == strncasecmp (fs_types[i].f_fsname, buf.f_basetype, FSTYPSZ))
            goto found;
#elif (defined(__APPLE__) && defined(__MACH__))
    for (i = 0; i < FS_TYPES_NUM; i++)
        if (0 == strncasecmp (fs_types[i].f_fsname, buf.f_fstypename, MFSTYPENAMELEN))
            goto found;
#elif defined(__BSD)
    for (i = 0; i < FS_TYPES_NUM; i++)
        if (0 == strncasecmp (fs_types[i].f_fsname, buf.f_fstypename, MFSNAMELEN))
            goto found;
#elif defined(__linux__)
    for (i = 0; i < FS_TYPES_NUM; i++)
        if (fs_types[i].f_fsid == (buf.f_type & fs_types[i].f_mask))
            goto found;
#endif

    free (file);
    return false;

found:
    OPAL_OUTPUT_VERBOSE((10, 0, "opal_path_nfs: file:%s on fs:%s\n",
                         fname, fs_types[i].f_fsname));
    free (file);
    return true;

#undef FS_TYPES_NUM

#else
    return false;
#endif /* __WINDOWS__ */
}

