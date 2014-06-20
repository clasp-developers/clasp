/*
 Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
                         University Research and Technology
                         Corporation.  All rights reserved.
 Copyright (c) 2004-2005 The University of Tennessee and The University
                         of Tennessee Research Foundation.  All rights
                         reserved.
 Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
                         University of Stuttgart.  All rights reserved.
 Copyright (c) 2004-2005 The Regents of the University of California.
                         All rights reserved.
 $COPYRIGHT$
 
 Additional copyrights may follow
 
 $HEADER$
 */

#include "opal_config.h"
#include "opal/win32/ompi_utsname.h"

/*
    This has to fill in the following information

    1. sysname: name of the operating system -- 
    2. nodename: GetComputerName
    3. release: GetVersionEx
    4. version: GetVersionEx
    5. machine: GetSystemInfo
*/
    
int uname( struct utsname *un )
{
    TCHAR env_variable[] = "OS=%OS%";
    DWORD info_buf_count;
    OSVERSIONINFO version_info;
    SYSTEM_INFO sys_info;
    TCHAR info_buf[OMPI_UTSNAME_LEN];

    info_buf_count = ExpandEnvironmentStrings( env_variable, info_buf, OMPI_UTSNAME_LEN); 
    if (0 == info_buf_count) {
        snprintf( un->sysname, OMPI_UTSNAME_LEN, "Unknown" );
    } else {
        /* remove the "OS=" from the beginning of the string */
        strncpy( un->sysname, info_buf + 3, OMPI_UTSNAME_LEN );
    }
    info_buf_count = OMPI_UTSNAME_LEN;
    if (!GetComputerName( un->nodename, &info_buf_count)) {
        snprintf(un->nodename, OMPI_UTSNAME_LEN, "undefined"); 
    }
    
    version_info.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    if (!GetVersionEx(&version_info)) {
        snprintf(un->release, OMPI_UTSNAME_LEN, "undefined"); 
        snprintf(un->version, OMPI_UTSNAME_LEN, "undefined"); 
    } else {
        /* fill in both release and version information */
        snprintf( un->release, OMPI_UTSNAME_LEN, "%d.%d.%d",
                  version_info.dwMajorVersion,
                  version_info.dwMinorVersion,
                  version_info.dwBuildNumber);
        snprintf( un->version, OMPI_UTSNAME_LEN, "%s", version_info.szCSDVersion );
    }

    /* get machine information */
    GetSystemInfo(&sys_info);
    switch( sys_info.wProcessorArchitecture ) {
        case PROCESSOR_ARCHITECTURE_UNKNOWN:
            snprintf( un->machine, OMPI_UTSNAME_LEN, "Unknown %d", sys_info.wProcessorLevel );
            break;
        case PROCESSOR_ARCHITECTURE_INTEL:
            snprintf( un->machine, OMPI_UTSNAME_LEN, "Intel %d", sys_info.wProcessorLevel );
            break;
        case PROCESSOR_ARCHITECTURE_IA64:
            snprintf( un->machine, OMPI_UTSNAME_LEN, "IA64 %d", sys_info.wProcessorLevel );
            break;
        case PROCESSOR_ARCHITECTURE_AMD64:
            snprintf( un->machine, OMPI_UTSNAME_LEN, "AMD %d", sys_info.wProcessorLevel );
            break;
        default:
            snprintf( un->machine, OMPI_UTSNAME_LEN, "UFO hardware %d", sys_info.wProcessorLevel );
            break;
    }

    return 0;
}
