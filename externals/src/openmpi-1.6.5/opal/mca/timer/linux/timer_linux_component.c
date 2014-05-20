/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "opal/mca/timer/timer.h"
#include "opal/mca/timer/linux/timer_linux.h"
#include "opal/constants.h"

opal_timer_t opal_timer_linux_freq;

static int opal_timer_linux_open(void);

const opal_timer_base_component_2_0_0_t mca_timer_linux_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        OPAL_TIMER_BASE_VERSION_2_0_0,

        /* Component name and version */
        "linux",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        opal_timer_linux_open,
        NULL
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};

static char *
find_info(FILE* fp, char *str, char *buf, size_t buflen)
{
    char *tmp;

    rewind(fp);
    while (NULL != fgets(buf, buflen, fp)) {
        if (strncmp(buf, str, strlen(str)) == 0) {
            /* we found the line.  Now eat everything up to,
               including, and one past the : */
            for (tmp = buf ; (*tmp != '\0') && (*tmp != ':') ; ++tmp) ;
            for ( ++tmp ; *tmp == ' ' ; ++tmp);
            if ('\0' != *tmp) {
                return tmp;
            }
        }
    }

    return NULL;
}

int
opal_timer_linux_open(void)
{
    FILE *fp;
    char *loc;
    float cpu_f;
    int ret;
    char buf[1024];

    fp = fopen("/proc/cpuinfo", "r");
    if (NULL == fp) {
        return OPAL_ERR_IN_ERRNO;
    }

    opal_timer_linux_freq = 0;

    if (0 == opal_timer_linux_freq) {
        /* first, look for a timebase field.  probably only on PPC,
           but one never knows */
        loc = find_info(fp, "timebase", buf, 1024);
        if (NULL != loc) {
            int freq;
            ret = sscanf(loc, "%d", &freq);
            if (1 == ret) {
                opal_timer_linux_freq = freq;
            }
        }
    }

    if (0 == opal_timer_linux_freq) {
        /* find the CPU speed - most timers are 1:1 with CPU speed */
        loc = find_info(fp, "cpu MHz", buf, 1024);
        if (NULL != loc) {
            ret = sscanf(loc, "%f", &cpu_f);
            if (1 == ret) {
                /* numer is in MHz - convert to Hz and make an integer */
                opal_timer_linux_freq = (opal_timer_t) cpu_f * 1000000;
            }
        }
    }

    if (0 == opal_timer_linux_freq) {
        /* look for the sparc way of getting cpu frequency */
        loc = find_info(fp, "Cpu0ClkTck", buf, 1024);
        if (NULL != loc) {
            unsigned int freq;
            ret = sscanf(loc, "%x", &freq);
            if (1 == ret) {
                opal_timer_linux_freq = freq;
            }
        }
    }

    fclose(fp);

    return OPAL_SUCCESS;
}
