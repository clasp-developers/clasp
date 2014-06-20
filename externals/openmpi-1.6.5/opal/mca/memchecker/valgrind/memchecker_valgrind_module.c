/*
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * memchecker (memory checker) valgrind framework component interface.
 *
 * Intent
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/memchecker/memchecker.h"
#include "opal/mca/memchecker/base/base.h"
#include "memchecker_valgrind.h"
#include "valgrind/valgrind.h"
#include "valgrind/memcheck.h"


/*
 * Local functions
 */
static int valgrind_module_init(void);
static int valgrind_module_runindebugger(void);
static int valgrind_module_isaddressable(void * p, size_t len);
static int valgrind_module_isdefined(void * p, size_t len);
static int valgrind_module_mem_noaccess(void * p, size_t len);
static int valgrind_module_mem_undefined(void * p, size_t len);
static int valgrind_module_mem_defined(void * p, size_t len);
static int valgrind_module_mem_defined_if_addressable(void * p, size_t len);
static int valgrind_module_create_block(void * p, size_t len, char * description);
static int valgrind_module_discard_block(void * p); /* Here, we need to do some mapping for valgrind */
static int valgrind_module_leakcheck(void);
#if 0
static int valgrind_module_get_vbits(void * p, char * vbits, size_t len);
static int valgrind_module_set_vbits(void * p, char * vbits, size_t len);
#endif

/*
 * Valgrind memchecker module
 */
static const opal_memchecker_base_module_1_0_0_t loc_module = {
    /* Initialization function */
    valgrind_module_init,

    /* Module function pointers */
    valgrind_module_runindebugger,
    valgrind_module_isaddressable,
    valgrind_module_isdefined,
    valgrind_module_mem_noaccess,
    valgrind_module_mem_undefined,
    valgrind_module_mem_defined,
    valgrind_module_mem_defined_if_addressable,
    valgrind_module_create_block,
    valgrind_module_discard_block,
    valgrind_module_leakcheck
};


int opal_memchecker_valgrind_component_query(mca_base_module_t **module, int *priority)
{
    int param;

    param = mca_base_param_find("memchecker", "valgrind", "priority");
    mca_base_param_lookup_int(param, priority);

    *module = (mca_base_module_t *)&loc_module;

    return OPAL_SUCCESS;
}


static int valgrind_module_init(void)
{
    /* Nothing to do yet, possibly update the amount of memory blocks. */

    return OPAL_SUCCESS;
}


static int valgrind_module_runindebugger(void)
{
    return RUNNING_ON_VALGRIND;
}


static int valgrind_module_isaddressable(void * p, size_t len)
{
    if (len > 0) {
        VALGRIND_CHECK_MEM_IS_ADDRESSABLE(p, len);
    }

    return OPAL_SUCCESS;
}


static int valgrind_module_isdefined(void * p, size_t len)
{
    if (len > 0) {
        VALGRIND_CHECK_MEM_IS_DEFINED(p, len);
    }

    return OPAL_SUCCESS;
}


static int valgrind_module_mem_noaccess(void * p, size_t len)
{
    if (len > 0) {
        VALGRIND_MAKE_MEM_NOACCESS(p, len);
    }

    return OPAL_SUCCESS;
}


static int valgrind_module_mem_undefined(void * p, size_t len)
{
    if (len > 0) {
        VALGRIND_MAKE_MEM_UNDEFINED(p, len);
    }

    return OPAL_SUCCESS;
}


static int valgrind_module_mem_defined(void * p, size_t len)
{
    if (len > 0) {
        VALGRIND_MAKE_MEM_DEFINED(p, len);
    }

    return OPAL_SUCCESS;
}


static int valgrind_module_mem_defined_if_addressable(void * p, size_t len)
{
    if (len > 0) {
        VALGRIND_MAKE_MEM_DEFINED_IF_ADDRESSABLE(p, len);
    }

    return OPAL_SUCCESS;
}


static int valgrind_module_create_block(void * p, size_t len, char * description)
{
    if (len > 0) {
        VALGRIND_CREATE_BLOCK (p, len, description);
        /*
         * Add p to some list atomically
         */
    }
    return OPAL_SUCCESS;
}


static int valgrind_module_discard_block(void * p)
{
    /* Here, we need to do some mapping for valgrind */
    /*
     * If p in list, then get rid of name
     */
    return OPAL_SUCCESS;
}


static int valgrind_module_leakcheck(void)
{
    VALGRIND_DO_LEAK_CHECK;
    return OPAL_SUCCESS;
}


#if 0
static int valgrind_module_get_vbits(void * p, char * vbits, size_t len)
{
    if (len > 0) {
        VALGRIND_GET_VBITS(p, vbits, len);
    }

    return OPAL_SUCCESS;
}


static int valgrind_module_set_vbits(void * p, char * vbits, size_t len)
{
    if (len > 0) {
        VALGRIND_SET_VBITS(p, vbits, len);
    }

    return OPAL_SUCCESS;
}
#endif

