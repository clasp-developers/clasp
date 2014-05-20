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

#include "opal/mca/timer/timer.h"
#include "opal/mca/timer/aix/timer_aix.h"

#include <sys/time.h>
#ifdef HAVE_PMAPI_H
#include <pmapi.h>
#endif

opal_timer_t opal_timer_aix_freq_mhz;
opal_timer_t opal_timer_aix_freq;

static int opal_timer_aix_open(void);

const opal_timer_base_component_2_0_0_t mca_timer_aix_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        OPAL_TIMER_BASE_VERSION_2_0_0,

        /* Component name and version */
        "aix",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        opal_timer_aix_open,
        NULL
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};


static int
opal_timer_aix_open(void)
{
#ifdef HAVE_PM_CYCLES
  opal_timer_aix_freq = pm_cycles();
  opal_timer_aix_freq_mhz = opal_timer_aix_freq / 1000000;
#else
  opal_timer_aix_freq_mhz = 0;
  opal_timer_aix_freq = 0;
#endif

  return OPAL_SUCCESS;
}
