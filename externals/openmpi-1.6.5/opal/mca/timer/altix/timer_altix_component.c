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

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sn/mmtimer.h>
#include <unistd.h>
#include <errno.h>

#include "opal/mca/timer/timer.h"
#include "opal/mca/timer/altix/timer_altix.h"
#include "opal/constants.h"

opal_timer_t opal_timer_altix_freq;
opal_timer_t opal_timer_altix_usec_conv;
volatile unsigned long *opal_timer_altix_mmdev_timer_addr;
static unsigned long *mmdev_map;

static int opal_timer_altix_open(void);
static int opal_timer_altix_close(void);

const opal_timer_base_component_2_0_0_t mca_timer_altix_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        OPAL_TIMER_BASE_VERSION_2_0_0,

        /* Component name and version */
        "altix",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        opal_timer_altix_open,
        opal_timer_altix_close
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};


static int
opal_timer_altix_open(void)
{
    int fd, ret;
    unsigned long val;
    long offset;

    fd = open(MMTIMER_FULLNAME, O_RDONLY);
    if (fd < 0) return OPAL_ERR_NOT_FOUND;

    /* make sure we can map the timer */
    ret = ioctl(fd, MMTIMER_MMAPAVAIL, 0);
    if (1 != ret) return OPAL_ERR_NOT_SUPPORTED;

    /* find the frequency */
    ret = ioctl(fd, MMTIMER_GETFREQ, &val);
    if (ret == -ENOSYS) return OPAL_ERR_NOT_SUPPORTED;
    opal_timer_altix_freq = val;
    opal_timer_altix_usec_conv = opal_timer_altix_freq / 1000000;

    /* find the address of the counter */
    ret = ioctl(fd, MMTIMER_GETOFFSET, 0);
    if (ret == -ENOSYS) return OPAL_ERR_NOT_SUPPORTED;
    offset = ret;

    mmdev_map = mmap(0, getpagesize(), PROT_READ, MAP_SHARED, fd, 0);
    if (NULL == mmdev_map) return OPAL_ERR_NOT_SUPPORTED;
    opal_timer_altix_mmdev_timer_addr = mmdev_map + offset;
    close(fd);

    return OPAL_SUCCESS;
}


static int
opal_timer_altix_close(void)
{
    if (NULL != mmdev_map) {
        munmap(mmdev_map, getpagesize());
    }

    return OPAL_SUCCESS;
}
