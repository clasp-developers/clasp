/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"

/* This component will only be compiled on Linux, where we are
   guaranteed to have <unistd.h> and friends */
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>

#include <sys/param.h>  /* for HZ to convert jiffies to actual time */

#include "opal/mca/base/mca_base_param.h"
#include "opal/dss/dss_types.h"
#include "opal/util/printf.h"

#include "pstat_linux.h"

/*
 * Local functions
 */
static int linux_module_init(void);
static int query(pid_t pid, opal_pstats_t *stats);
static int linux_module_fini(void);

/*
 * Linux pstat module
 */
const opal_pstat_base_module_t opal_pstat_linux_module = {
    /* Initialization function */
    linux_module_init,
    query,
    linux_module_fini
};

static int linux_module_init(void)
{
    return OPAL_SUCCESS;
}

static int linux_module_fini(void)
{
    return OPAL_SUCCESS;
}

static char *next_field(char *ptr, int barrier)
{
    int i=0;
    
    /* we are probably pointing to the last char
     * of the current field, so look for whitespace
     */
    while (!isspace(*ptr) && i < barrier) {
        ptr++;  /* step over the current char */
        i++;
    }
    
    /* now look for the next field */
    while (isspace(*ptr) && i < barrier) {
        ptr++;
        i++;
    }
    
    return ptr;
}


static int query(pid_t pid, opal_pstats_t *stats)
{
    char data[4096];
    int fd;
    size_t numchars;
    char *ptr, *eptr;
    int i;
    int len;
    
    /* create the stat filename for this proc */
    numchars = snprintf(data, sizeof(data), "/proc/%d/stat", pid);
    if (numchars >= sizeof(data)) {
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }
    
    if (0 > (fd = open(data, O_RDONLY))) {
        /* can't access this file - most likely, this means we
         * aren't really on a supported system, or the proc no
         * longer exists. Just return an error
         */
        return OPAL_ERR_FILE_OPEN_FAILURE;
    }
    
    /* absorb all of the file's contents in one gulp - we'll process
     * it once it is in memory for speed
     */
    memset(data, 0, sizeof(data));
    len = read(fd, data, sizeof(data)-1);
    if (len < 0) {
        /* This shouldn't happen! */
        return OPAL_ERR_FILE_OPEN_FAILURE;
    }
    close(fd);
    
    /* remove newline at end */
    data[len] = '\0';
    
    /* the stat file consists of a single line in a carefully formatted
     * form. Parse it field by field as per proc(3) to get the ones we want
     */
    
    /* we don't need to read the pid from the file - we already know it! */
    stats->pid = pid;
    
    /* the cmd is surrounded by parentheses - find the start */
    if (NULL == (ptr = strchr(data, '('))) {
        /* no cmd => something wrong with data, return error */
        return OPAL_ERR_BAD_PARAM;
    }
    /* step over the paren */
    ptr++;
    
    /* find the ending paren */
    if (NULL == (eptr = strchr(ptr, ')'))) {
        /* no end to cmd => something wrong with data, return error */
        return OPAL_ERR_BAD_PARAM;
    }
    
    /* save the cmd name, up to the limit of the array */
    i = 0;
    while (ptr < eptr && i < OPAL_PSTAT_MAX_STRING_LEN) {
        stats->cmd[i++] = *ptr++;
    }
    
    /* move to the next field in the data */
    ptr = next_field(eptr, len);
    
    /* next is the process state - a single character */
    stats->state = *ptr;
    /* move to next field */
    ptr = next_field(ptr, len);
    
    /* skip fields until we get to the times */
    ptr = next_field(ptr, len); /* ppid */
    ptr = next_field(ptr, len); /* pgrp */
    ptr = next_field(ptr, len); /* session */
    ptr = next_field(ptr, len); /* tty_nr */
    ptr = next_field(ptr, len); /* tpgid */
    ptr = next_field(ptr, len); /* flags */
    ptr = next_field(ptr, len); /* minflt */
    ptr = next_field(ptr, len); /* cminflt */
    ptr = next_field(ptr, len); /* majflt */
    ptr = next_field(ptr, len); /* cmajflt */
    
    /* grab the process time usage fields */
    stats->time = strtoul(ptr, &ptr, 10);    /* utime */
    stats->time += strtoul(ptr, &ptr, 10);   /* add the stime */
    stats->time = stats->time / HZ;  /* convert to time */
    /* move to next field */
    ptr = next_field(ptr, len);
    
    /* skip fields until we get to priority */
    ptr = next_field(ptr, len); /* cutime */
    ptr = next_field(ptr, len); /* cstime */
    
    /* save the priority */
    stats->priority = strtol(ptr, &ptr, 10);
    
    /* that's all we care about from this data - ignore the rest */
    
    /* now create the status filename for this proc */
    memset(data, 0, sizeof(data));
    numchars = snprintf(data, sizeof(data), "/proc/%d/status", pid);
    if (numchars >= sizeof(data)) {
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }
    
    if (0 > (fd = open(data, O_RDONLY))) {
        /* can't access this file - most likely, this means we
         * aren't really on a supported system, or the proc no
         * longer exists. Just return an error
         */
        return OPAL_ERR_FILE_OPEN_FAILURE;
    }
    
    /* absorb all of the file's contents in one gulp - we'll process
     * it once it is in memory for speed
     */
    memset(data, 0, sizeof(data));
    len = read(fd, data, sizeof(data)-1);
    close(fd);
    
    /* remove newline at end */
    data[len] = '\0';
    
    /* parse it according to proc(3) */
    eptr = data;
    /* look for VmPeak */
    if (NULL != (ptr = strstr(data, "VmPeak:"))) {
        /* found it - step past colon */
        ptr += 8;
        eptr = strchr(ptr, 'k');
        *eptr = '\0';
        stats->peak_vsize = strtoul(ptr, NULL, 10);  /* already in kB */
        eptr++;
    }
     /* look for VmSize */
    if (NULL != (ptr = strstr(eptr, "VmSize:"))) {
        /* found it - step past colon */
        ptr += 8;
        eptr = strchr(ptr, 'k');
        *eptr = '\0';
        stats->vsize = strtoul(ptr, NULL, 10);  /* already in kB */
        eptr++;
    }
    
    /* look for RSS */
    if (NULL != (ptr = strstr(eptr, "VmRSS:"))) {
        /* found it - step past colon */
        ptr += 8;
        eptr = strchr(ptr, 'k');
        *eptr = '\0';
        stats->rss = strtoul(ptr, NULL, 10);  /* already in kB */
        eptr++;
    }

    /* look for Libraries */
    if (NULL != (ptr = strstr(eptr, "VmLib:"))) {
        /* found it - step past colon */
        ptr += 8;
        eptr = strchr(ptr, 'k');
        *eptr = '\0';
        stats->shared_size = strtoul(ptr, NULL, 10);  /* already in kB */
        eptr++;
    }

    /* look for threads */
    if (NULL != (ptr = strstr(eptr, "Threads:"))) {
        /* found it - step past colon */
        ptr += 8;
        stats->num_threads = strtoul(ptr, NULL, 10);
    }
    
    return OPAL_SUCCESS;
}
