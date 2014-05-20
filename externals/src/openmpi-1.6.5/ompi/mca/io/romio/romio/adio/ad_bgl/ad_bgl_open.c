/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_bgl_open.c
 * \brief ???
 */

/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_bgl.h"
#include "ad_bgl_aggrs.h"

#include <sys/statfs.h>
#include <sys/vfs.h>

/* COPIED FROM ad_fstype.c since it is static in that file

 ADIO_FileSysType_parentdir - determines a string pathname for the
 parent directory of a given filename.

Input Parameters:
. filename - pointer to file name character array

Output Parameters:
. dirnamep - pointer to location in which to store a pointer to a string

 Note that the caller should free the memory located at the pointer returned
 after the string is no longer needed.
*/

#ifndef PATH_MAX
#define PATH_MAX 65535
#endif

/* In a strict ANSI environment, S_ISLNK may not be defined.  Fix that
   here.  We assume that S_ISLNK is *always* defined as a macro.  If
   that is not universally true, then add a test to the romio
   configure that trys to link a program that references S_ISLNK */
#if !defined(S_ISLNK) 
#    if defined(S_IFLNK)
     /* Check for the link bit */
#    define S_ISLNK(mode) ((mode) & S_IFLNK)
#    else
     /* no way to check if it is a link, so say false */
#    define S_ISLNK(mode) 0   
#    endif
#endif /* !(S_ISLNK) */

/* ADIO_FileSysType_parentdir
 *
 * Returns pointer to string in dirnamep; that string is allocated with
 * strdup and must be free()'d.
 */
static void ADIO_FileSysType_parentdir(char *filename, char **dirnamep)
{
    int err;
    char *dir = NULL, *slash;
    struct stat statbuf;
    
    err = lstat(filename, &statbuf);

    if (err || (!S_ISLNK(statbuf.st_mode))) {
	/* no such file, or file is not a link; these are the "normal"
	 * cases where we can just return the parent directory.
	 */
	dir = ADIOI_Strdup(filename);
    }
    else {
	/* filename is a symlink.  we've presumably already tried
	 * to stat it and found it to be missing (dangling link),
	 * but this code doesn't care if the target is really there
	 * or not.
	 */
	int namelen;
	char *linkbuf;

	linkbuf = ADIOI_Malloc(PATH_MAX+1);
	namelen = readlink(filename, linkbuf, PATH_MAX+1);
	if (namelen == -1) {
	    /* something strange has happened between the time that
	     * we determined that this was a link and the time that
	     * we attempted to read it; punt and use the old name.
	     */
	    dir = ADIOI_Strdup(filename);
	}
	else {
	    /* successfully read the link */
	    linkbuf[namelen] = '\0'; /* readlink doesn't null terminate */
	    dir = ADIOI_Strdup(linkbuf);
	    ADIOI_Free(linkbuf);
	}
    }

    slash = strrchr(dir, '/');
    if (!slash) ADIOI_Strncpy(dir, ".", 2);
    else {
	if (slash == dir) *(dir + 1) = '\0';
	else *slash = '\0';
    }

    *dirnamep = dir;
    return;
}

static void scaleable_stat(ADIO_File fd)
{
    struct stat64 bgl_stat;
    struct statfs bgl_statfs;
    int rank, rc;
    char * dir;
    long buf[2];
    MPI_Comm_rank(fd->comm, &rank);

    if (rank == 0) {
	/* Get the (real) underlying file system block size */
	rc = stat64(fd->filename, &bgl_stat);
	if (rc >= 0)
	{
	    buf[0] = bgl_stat.st_blksize;
	    DBGV_FPRINTF(stderr,"Successful stat '%s'.  Blocksize=%ld\n",
		    fd->filename,bgl_stat.st_blksize);
	}
	else
	{
	    DBGV_FPRINTF(stderr,"Stat '%s' failed with rc=%d, errno=%d\n",
		    fd->filename,rc,errno);
	}
	/* Get the (real) underlying file system type so we can 
	 * plan our fsync scaling strategy */
	rc = statfs(fd->filename,&bgl_statfs);
	if (rc >= 0)
	{
	    DBGV_FPRINTF(stderr,"Successful statfs '%s'.  Magic number=%#X\n",
		    fd->filename,bgl_statfs.f_type);
	    buf[1] = bgl_statfs.f_type;
	}
	else
	{
	    DBGV_FPRINTF(stderr,"Statfs '%s' failed with rc=%d, errno=%d\n",
		    fd->filename,rc,errno);
	    ADIO_FileSysType_parentdir(fd->filename, &dir);
	    rc = statfs(dir,&bgl_statfs);
	    if (rc >= 0)
	    {
		DBGV_FPRINTF(stderr,"Successful statfs '%s'.  Magic number=%#X\n",dir,bgl_statfs.f_type);
		buf[1] = bgl_statfs.f_type;
	    }
	    else
	    {
		/* Hmm.  Guess we'll assume the worst-case, that it's not GPFS
		 * or PVFS2 below */
		buf[1] = -1; /* bogus magic number */
		DBGV_FPRINTF(stderr,"Statfs '%s' failed with rc=%d, errno=%d\n",dir,rc,errno);
	    }
	    free(dir);
	}
    }
    /* now we can broadcast the stat/statfs data to everyone else */
    MPI_Bcast(buf, 2, MPI_LONG, 0, fd->comm);
    bgl_stat.st_blksize = buf[0];
    bgl_statfs.f_type = buf[1];

    /* data from stat64 */
    /* store the blksize in the file system specific storage */
    ((ADIOI_BGL_fs*)fd->fs_ptr)->blksize = bgl_stat.st_blksize;

    /* data from statfs */
    if ((bgl_statfs.f_type == GPFS_SUPER_MAGIC) ||
	    (bgl_statfs.f_type == PVFS2_SUPER_MAGIC))
    {
	((ADIOI_BGL_fs*)fd->fs_ptr)->fsync_aggr = 
	    ADIOI_BGL_FSYNC_AGGREGATION_ENABLED;

	/* Only one rank is an "fsync aggregator" because only one 
	 * fsync is needed */
	if (rank == 0)
	{
	    ((ADIOI_BGL_fs*)fd->fs_ptr)->fsync_aggr |= 
		ADIOI_BGL_FSYNC_AGGREGATOR;
	    DBG_FPRINTF(stderr,"fsync aggregator %d\n",rank);
	}
	else ; /* aggregation enabled but this rank is not an aggregator*/
    }
    else; /* Other filesystems default to no fsync aggregation */
}


void ADIOI_BGL_Open(ADIO_File fd, int *error_code)
{
    int perm, old_mask, amode;
    static char myname[] = "ADIOI_BGL_OPEN";

    /* set internal variables for tuning environment variables */
    ad_bgl_get_env_vars();		

    if (fd->perm == ADIO_PERM_NULL) {
	old_mask = umask(022);
	umask(old_mask);
	perm = old_mask ^ 0666;
    }
    else perm = fd->perm;

    amode = 0;
    if (fd->access_mode & ADIO_CREATE)
	amode = amode | O_CREAT;
    if (fd->access_mode & ADIO_RDONLY)
	amode = amode | O_RDONLY;
    if (fd->access_mode & ADIO_WRONLY)
	amode = amode | O_WRONLY;
    if (fd->access_mode & ADIO_RDWR)
	amode = amode | O_RDWR;
    if (fd->access_mode & ADIO_EXCL)
	amode = amode | O_EXCL;
#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event(ADIOI_MPE_open_a, 0, NULL);
#endif
    fd->fd_sys = open(fd->filename, amode, perm);
#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event(ADIOI_MPE_open_b, 0, NULL);
#endif
  DBG_FPRINTF(stderr,"open('%s',%#X,%#X) rc=%d, errno=%d\n",fd->filename,amode,perm,fd->fd_sys,errno);
    fd->fd_direct = -1;

    if ((fd->fd_sys != -1) && (fd->access_mode & ADIO_APPEND))
	fd->fp_ind = fd->fp_sys_posn = lseek(fd->fd_sys, 0, SEEK_END);

    if(fd->fd_sys != -1)
    {
        struct stat64 bgl_stat;
        struct statfs bgl_statfs;
        char* dir;
        int rc;

        /* Initialize the ad_bgl file system specific information */
        AD_BGL_assert(fd->fs_ptr == NULL);
        fd->fs_ptr = (ADIOI_BGL_fs*) ADIOI_Malloc(sizeof(ADIOI_BGL_fs));

        ((ADIOI_BGL_fs*)fd->fs_ptr)->blksize = 1048576; /* default to 1M */

        /* default is no fsync aggregation */
        ((ADIOI_BGL_fs*)fd->fs_ptr)->fsync_aggr = 
	    ADIOI_BGL_FSYNC_AGGREGATION_DISABLED; 


#ifdef ADIOI_MPE_LOGGING
        MPE_Log_event(ADIOI_MPE_stat_a, 0, NULL);
#endif
        scaleable_stat(fd);
#ifdef ADIOI_MPE_LOGGING
        MPE_Log_event(ADIOI_MPE_stat_b, 0, NULL);
#endif
    }

    if (fd->fd_sys == -1) {
	if (errno == ENAMETOOLONG)
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS,
					       MPIR_ERR_RECOVERABLE, myname,
					       __LINE__, MPI_ERR_BAD_FILE,
					       "**filenamelong",
					       "**filenamelong %s %d",
					       fd->filename,
					       strlen(fd->filename));
	else if (errno == ENOENT)
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS,
					       MPIR_ERR_RECOVERABLE, myname,
					       __LINE__, MPI_ERR_NO_SUCH_FILE,
					       "**filenoexist",
					       "**filenoexist %s",
					       fd->filename);
	else if (errno == ENOTDIR || errno == ELOOP)
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS,
					       MPIR_ERR_RECOVERABLE,
					       myname, __LINE__,
					       MPI_ERR_BAD_FILE,
					       "**filenamedir",
					       "**filenamedir %s",
					       fd->filename);
	else if (errno == EACCES) {
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS,
					       MPIR_ERR_RECOVERABLE, myname,
					       __LINE__, MPI_ERR_ACCESS,
					       "**fileaccess",
					       "**fileaccess %s", 
					       fd->filename );
	}
	else if (errno == EROFS) {
	    /* Read only file or file system and write access requested */
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS,
					       MPIR_ERR_RECOVERABLE, myname,
					       __LINE__, MPI_ERR_READ_ONLY,
					       "**ioneedrd", 0 );
	}
	else {
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS,
					       MPIR_ERR_RECOVERABLE, myname,
					       __LINE__, MPI_ERR_IO, "**io",
					       "**io %s", strerror(errno));
	}
    }
    else *error_code = MPI_SUCCESS;
}
/* 
 *vim: ts=8 sts=4 sw=4 noexpandtab 
 */
