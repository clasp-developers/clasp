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

#ifndef OMPI_MISC_H
#define OMPI_MISC_H

#include <stdio.h>
#include <stdlib.h>

#define _SC_PAGESIZE 0
#define _SC_OPEN_MAX 1

#if 0
/* currently, this is a memory leak */
static __inline char* getenv (const char *name)
{
    int ret;
    char *buffer;
    DWORD length = GetEnvironmentVariable( (LPCSTR)name, NULL, 0 );

    if( 0 == length ) return NULL;
    buffer = (char *)malloc(sizeof(char) * length);
    ret = GetEnvironmentVariable((LPCSTR)name, (LPSTR)buffer, length);
    return (ret > 0) ? buffer: NULL;
}


static __inline int setenv (const char *name, const char *value, int rewrite)
{
    int ret;
    if( 0 == rewrite  ) {
        DWORD length = 0;
        if( 0 == (length = GetEnvironmentVariable( (LPCSTR)name, NULL, length )) ) {
            if( ERROR_ENVVAR_NOT_FOUND == GetLastError() ) {  /* do not exist */
                return 0;
            }
        }
    }
    /* just push it back to the windows thingy */
    ret = SetEnvironmentVariable ((LPCSTR)name, (LPCSTR)value);
    return (0 != ret)? 1: 0;
}
#endif

static __inline unsigned int sleep(unsigned int seconds) {

    /* Allow interruptions */
    SleepEx(seconds * 1000, TRUE);
    return 0;
}

/* this function can currently ONLY return the page size. for it to 
   do the entire sysconf range it needs to be extended */
static __inline size_t sysconf(int option) {
    
    SYSTEM_INFO sys_info;

    if( _SC_OPEN_MAX == option ) {
        return _getmaxstdio();
    }

    GetSystemInfo(&sys_info);
    if (_SC_PAGESIZE == option){
        return (size_t)sys_info.dwPageSize;
    }
    printf( "This functionality is not supported: line: %d\tfile: %s\n",
            __LINE__, __FILE__ );
    abort();
    return 0;
}

#define F_GETFL 0
#define F_SETFL 1
#define O_NONBLOCK 0
/*
 * this function is currently defined only for setting the socket to be 
 * in the non-blocking mode. Else this function returns error not implemented.
 * This calls ioctlsocket in the winsock library
 */
static __inline int fcntl (int fildes, int cmd, ...) {
    int ret;
    int mode;

    switch (cmd) {
        case F_SETFL: mode = 1; ret = ioctlsocket ((SOCKET)fildes, FIONBIO, (u_long FAR*) &mode);
                      break;
        case F_GETFL: ret = 0;
                      break;
        default: printf("Option not supported: %d %s\n", __LINE__, __FILE__);
                      abort();
    };

    return ret;
}

#endif /* OMPI_MISC_H */
