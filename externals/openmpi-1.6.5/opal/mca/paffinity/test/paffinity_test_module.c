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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"

#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"

#include "paffinity_test.h"

/*
 * Local functions
 */
static int init(void);
static int set(opal_paffinity_base_cpu_set_t cpumask);
static int get(opal_paffinity_base_cpu_set_t *cpumask);
static int finalize(void);
static int map_to_processor_id(int socket, int core, int *processor_id);
static int map_to_socket_core(int processor_id, int *socket, int *core);
static int get_processor_info(int *num_processors);
static int get_socket_info(int *num_sockets);
static int get_core_info(int socket, int *num_cores);
static int get_physical_processor_id(int logical_processor_id);
static int get_physical_socket_id(int logical_socket_id);
static int get_physical_core_id(int physical_socket_id, int logical_core_id);

/*
 * Test paffinity module
 */
opal_paffinity_base_module_t opal_paffinity_test_module = {
    /* Initialization function */
    init,

    /* Module function pointers */
    set,
    get,
    map_to_processor_id,
    map_to_socket_core,
    get_processor_info,
    get_socket_info,
    get_core_info,
    get_physical_processor_id,
    get_physical_socket_id,
    get_physical_core_id,
    finalize
};

/* nothing to init here */
static int init(void)
{
    return OPAL_SUCCESS;
}

/* this gives us a cpumask which tells which CPU to bind */
static int set(opal_paffinity_base_cpu_set_t cpumask)
{
    return OPAL_SUCCESS;
}

/* This get function returns the CPU id that's currently binded,
 * and then sets the cpumask. */
static int get(opal_paffinity_base_cpu_set_t *cpumask) 
{
    int i;
    
    OPAL_PAFFINITY_CPU_ZERO(*cpumask);
    if (mca_paffinity_test_component.bound) {
        for (i=0; i < mca_paffinity_test_component.num_sockets*mca_paffinity_test_component.num_cores; i+=2) {
            OPAL_PAFFINITY_CPU_SET(i, *cpumask);
        }
        /* assign all cores in the 2nd socket, if it exists */
        if (mca_paffinity_test_component.num_sockets >= 2) {
            for (i=mca_paffinity_test_component.num_cores; i < 2*mca_paffinity_test_component.num_cores; i++) {
                OPAL_PAFFINITY_CPU_SET(i, *cpumask);
            }
        }
    } else {
        for (i=0; i < mca_paffinity_test_component.num_sockets*mca_paffinity_test_component.num_cores; i++) {
            OPAL_PAFFINITY_CPU_SET(i, *cpumask);
        }
    }
    return OPAL_SUCCESS;
}

static int map_to_processor_id(int socket, int core, int *processor_id)
{
    *processor_id = socket*mca_paffinity_test_component.num_cores + core;
    return OPAL_SUCCESS;
}

static int map_to_socket_core(int processor_id, int *socket, int *core)
{
    *socket = processor_id / mca_paffinity_test_component.num_cores;
    *core = processor_id % mca_paffinity_test_component.num_cores;
    return OPAL_SUCCESS;
}

static int get_processor_info(int *num_processors)
{
    *num_processors = mca_paffinity_test_component.num_sockets * mca_paffinity_test_component.num_cores;
    return OPAL_SUCCESS;
}

static int get_socket_info(int *num_sockets)
{
    *num_sockets = mca_paffinity_test_component.num_sockets;
    return OPAL_SUCCESS;
}

static int get_core_info(int socket, int *num_cores)
{
    *num_cores = mca_paffinity_test_component.num_cores;
    return OPAL_SUCCESS;
}

static int get_physical_processor_id(int logical_processor_id)
{
    return logical_processor_id;
}

static int get_physical_socket_id(int logical_socket_id)
{
    return logical_socket_id;
}

static int get_physical_core_id(int physical_socket_id, int logical_core_id)
{
    if (mca_paffinity_test_component.num_cores < logical_core_id) {
        return OPAL_ERROR;
    }
    return logical_core_id;
}

static int finalize(void)
{
    return OPAL_SUCCESS;
}

