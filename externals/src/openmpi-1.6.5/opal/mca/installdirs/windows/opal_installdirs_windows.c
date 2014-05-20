/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2009      High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <stdlib.h>
#include <string.h>

#include "opal/mca/installdirs/installdirs.h"
#include "opal/constants.h"

static int installdirs_windows_open(void);

opal_installdirs_base_component_t mca_installdirs_windows_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        OPAL_INSTALLDIRS_BASE_VERSION_2_0_0,

        /* Component name and version */
        "windows",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        installdirs_windows_open,
        NULL
    },
    {
        /* This component is checkpointable */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};

char *openmpi_home = NULL;

#define SET_FIELD(field, base_dir)                                             \
    do {                                                                       \
         if (NULL != openmpi_home && 0 != strlen(openmpi_home)) {              \
             char *path =                                                      \
                 (char *) malloc(strlen(openmpi_home) + strlen(base_dir) + 1); \
             strcpy(path, openmpi_home);                                       \
             strcat(path, base_dir);                                           \
             mca_installdirs_windows_component.install_dirs_data.field = path; \
         }                                                                     \
    } while (0)



static int
installdirs_windows_open(void)
{
    /* check the env first */
    openmpi_home = getenv("OPENMPI_HOME");

    /* if OPENMPI_HOME is not set, check the registry */
    if(NULL == openmpi_home) {
        HKEY ompi_key;
        int i;
        DWORD cbData, valueLength, keyType;
        char valueName[1024], vData[1024];

        /* The OPENMPI_HOME is the only one which is required to be in the registry.
         * All others can be composed starting from OPAL_PREFIX.
         *
         * On 32 bit Windows, we write in HKEY_LOCAL_MACHINE\Software\Open MPI,
         * but on 64 bit Windows, we always use HKEY_LOCAL_MACHINE\Software\Wow6432Node\Open MPI
         * for both 32 and 64 bit OMPI, because we only have 32 bit installer, and Windows will
         * always consider OMPI as 32 bit application.
         */
        if( ERROR_SUCCESS == RegOpenKeyEx(HKEY_LOCAL_MACHINE, "Software\\Open MPI", 0, KEY_READ, &ompi_key) ||
            ERROR_SUCCESS == RegOpenKeyEx(HKEY_LOCAL_MACHINE, "Software\\Wow6432Node\\Open MPI", 0, KEY_READ, &ompi_key) ) {    
                for( i = 0; true; i++) {
                valueLength = 1024;
                valueName[0] = '\0';
                cbData = 1024;
                valueLength = 1024;
                if( ERROR_SUCCESS == RegEnumValue( (ompi_key), i, valueName, &valueLength,
                                                    NULL, &keyType, (LPBYTE) vData, &cbData ) ) {
                    if( ((REG_EXPAND_SZ == keyType) || (REG_SZ == keyType)) &&
                        (0 == strncasecmp( valueName, ("OPENMPI_HOME"), strlen(("OPENMPI_HOME")) )) ) {
                        openmpi_home = strdup(vData);
                        break;
                    }
                } else
                    break;
            }
        }

        RegCloseKey(ompi_key);
    }

#ifdef OMPI_RELEASE_BUILD
    /* the last try, check the executable path.
     * only used for binary releases, so that we 
     * don't bother with the configured paths in mca_installdirs_config
     */
    if(NULL == openmpi_home) {
        int ch, i;
        static TCHAR szPath[MAX_PATH+1];

        szPath[MAX_PATH] = 0;
        ch = GetModuleFileName(NULL, szPath, MAX_PATH);
        if (ch != 0) {
            for (i=ch; i>0; i--) {
                if ((szPath[i] != '\\') && (szPath[i] != '/')) {
                  szPath[i] = 0;
                } else {
                  szPath[i] = 0;
                  break;
                }
            }
            strcat(szPath, "/..");
            openmpi_home = szPath;
        }
    }
#endif

    SET_FIELD(prefix, "");
    SET_FIELD(exec_prefix, "/bin");
    SET_FIELD(bindir, "/bin");
    SET_FIELD(sbindir, "/sbin");
    SET_FIELD(libexecdir, "/libexec");
    SET_FIELD(datarootdir, "/share");
    SET_FIELD(datadir, "/share");
    SET_FIELD(sysconfdir, "/etc");
    SET_FIELD(sharedstatedir, "/com");
    SET_FIELD(localstatedir, "/var");
    SET_FIELD(libdir, "/lib");
    SET_FIELD(includedir, "/include");
    SET_FIELD(infodir, "/share/info");
    SET_FIELD(mandir, "/share/man");
    SET_FIELD(pkgdatadir, "/share/openmpi");
    SET_FIELD(pkglibdir, "/lib/openmpi");
    SET_FIELD(pkgincludedir, "/include/openmpi");

    return OPAL_SUCCESS;
}
