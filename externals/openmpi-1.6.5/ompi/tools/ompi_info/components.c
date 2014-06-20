/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2011      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdlib.h>
#include <string.h>

#include "ompi/runtime/params.h"
#include "orte/runtime/runtime.h"

#include "opal/util/argv.h"

#include "opal/event/event.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/backtrace/backtrace.h"
#include "opal/mca/backtrace/base/base.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/mca/carto/carto.h"
#include "opal/mca/carto/base/base.h"
#include "opal/mca/shmem/shmem.h"
#include "opal/mca/shmem/base/base.h"
#include "opal/mca/maffinity/maffinity.h"
#include "opal/mca/maffinity/base/base.h"
#include "opal/mca/memory/memory.h"
#include "opal/mca/memory/base/base.h"
#include "opal/mca/memchecker/memchecker.h"
#include "opal/mca/memchecker/base/base.h"
#include "opal/mca/timer/timer.h"
#include "opal/mca/timer/base/base.h"
#include "opal/mca/installdirs/installdirs.h"
#include "opal/mca/installdirs/base/base.h"
#include "opal/mca/sysinfo/sysinfo.h"
#include "opal/mca/sysinfo/base/base.h"
#include "opal/mca/hwloc/base/base.h"
#if OPAL_ENABLE_FT_CR == 1
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"
#endif
#include "opal/runtime/opal.h"
#include "opal/dss/dss.h"

#include "ompi/mca/allocator/base/base.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/io/io.h"
#include "ompi/mca/io/base/base.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/rcache/rcache.h"
#include "ompi/mca/rcache/base/base.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"
#include "ompi/mca/topo/base/base.h"
#include "ompi/mca/osc/osc.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/pubsub/base/base.h"
#include "ompi/mca/dpm/base/base.h"
#include "ompi/mca/op/base/base.h"

#if OPAL_ENABLE_FT_CR == 1
#include "ompi/mca/crcp/crcp.h"
#include "ompi/mca/crcp/base/base.h"
#endif

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/grpcomm/base/base.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/notifier/notifier.h"
#include "orte/mca/notifier/base/base.h"
#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#if !ORTE_DISABLE_FULL_SUPPORT
#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/oob/oob.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/ras/ras.h"
#include "orte/mca/ras/base/ras_private.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/base.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/routed/base/base.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/base.h"
#if OPAL_ENABLE_FT_CR == 1
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"
#endif
#include "orte/mca/filem/filem.h"
#include "orte/mca/filem/base/base.h"
#endif

#include "ompi/tools/ompi_info/ompi_info.h"
/*
 * Public variables
 */

static void component_map_construct(ompi_info_component_map_t *map)
{
    map->type = NULL;
}
static void component_map_destruct(ompi_info_component_map_t *map)
{
    if (NULL != map->type) {
        free(map->type);
    }
    /* the type close functions will release the
     * list of components
     */
}
OBJ_CLASS_INSTANCE(ompi_info_component_map_t,
                   opal_list_item_t,
                   component_map_construct,
                   component_map_destruct);

opal_pointer_array_t component_map;

/*
 * Private variables
 */

static bool opened_components = false;


/*
 * Open all MCA components so that they can register their MCA
 * parameters.  Take a shotgun approach here and indiscriminately open
 * all components -- don't be selective.  To this end, we need to clear
 * out the environment of all OMPI_MCA_<type> variables to ensure
 * that the open algorithms don't try to only open one component.
 */
void ompi_info_open_components(void)
{
    int i;
    char *env, *str;
    char *target, *save, *type;
    char **env_save=NULL;
    bool need_close_components = false;
    ompi_info_component_map_t *map;
    
    if (opened_components) {
        return;
    }
    
    /* init the map */
    OBJ_CONSTRUCT(&component_map, opal_pointer_array_t);
    opal_pointer_array_init(&component_map, 256, INT_MAX, 128);
    
    /* Clear out the environment.  Use strdup() to orphan the resulting
     * strings because items are placed in the environment by reference,
     * not by value.
     */
    
    for (i = 0; i < mca_types.size; ++i) {
        if (NULL == (type = (char*)opal_pointer_array_get_item(&mca_types, i))) {
            continue;
        }
        asprintf(&env, "OMPI_MCA_%s", type);
        if (NULL != (save = getenv(env))) {
            /* save this param so it can later be restored */
            asprintf(&str, "%s=%s", env, save);
            opal_argv_append_nosize(&env_save, str);
            free(str);
            /* can't manipulate it directly, so make a copy first */
            asprintf(&target, "%s=", env);
            putenv(target);
            free(target);
        }
    }
    
    /* some components require the event library be active, so activate it */
    if (OPAL_SUCCESS != opal_event_init()) {
        str = "opal_event_init failed";
        goto error;
    }
    
    /* Open the DSS */
    
    if (ORTE_SUCCESS != opal_dss_open()) {
        str = "Unable to initialize the DSS";
        goto error;
    }
    
    /* Open up the MCA */
    
    if (OPAL_SUCCESS != mca_base_open()) {
        str = "mca_base_open failed";
        goto error;
    }
    
    /* Register the OPAL layer's MCA parameters */
    
    if (OPAL_SUCCESS != opal_register_params()) {
        str = "opal_register_params failed";
        goto error;
    }
    
    /* Register the ORTE layer's MCA parameters */
    
    if (ORTE_SUCCESS != orte_register_params()) {
        str = "orte_register_params failed";
        goto error;
    }
    
    /* Initialize the opal_output system */
    if (!opal_output_init()) {
        str = "opal_output_init failed";
        goto error;
    }
    
    /* Register the MPI layer's MCA parameters */
    
    if (OMPI_SUCCESS != ompi_mpi_register_params()) {
        str = "ompi_mpi_Register_params failed";
        goto error;
    }
    
    /* Find / open all components */
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("base");
    opal_pointer_array_add(&component_map, map);
    
    /* set default error message from here forward */
    str = "A component framework failed to open properly.";
    
    /* OPAL frameworks */
    
    if (OPAL_SUCCESS != opal_backtrace_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("backtrace");
    map->components = &opal_backtrace_base_components_opened;
    opal_pointer_array_add(&component_map, map);
    
    if (OPAL_SUCCESS != opal_memory_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("memory");
    map->components = &opal_memory_base_components_opened;
    opal_pointer_array_add(&component_map, map);
    
    if (OPAL_SUCCESS != opal_memchecker_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("memchecker");
    map->components = &opal_memchecker_base_components_opened;
    opal_pointer_array_add(&component_map, map);
    
    if (OPAL_SUCCESS != opal_paffinity_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("paffinity");
    map->components = &opal_paffinity_base_components_opened;
    opal_pointer_array_add(&component_map, map);
    
    if (OPAL_SUCCESS != opal_carto_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("carto");
    map->components = &opal_carto_base_components_opened;
    opal_pointer_array_add(&component_map, map);

    if (OPAL_SUCCESS != opal_shmem_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("shmem");
    map->components = &opal_shmem_base_components_opened;
    opal_pointer_array_add(&component_map, map);
    
    if (OPAL_SUCCESS != opal_maffinity_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("maffinity");
    map->components = &opal_maffinity_base_components_opened;
    opal_pointer_array_add(&component_map, map);
    
    if (OPAL_SUCCESS != opal_sysinfo_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("sysinfo");
    map->components = &opal_sysinfo_base_components_opened;
    opal_pointer_array_add(&component_map, map);

#if OPAL_HAVE_HWLOC
    if (OPAL_SUCCESS != opal_hwloc_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("hwloc");
    map->components = &opal_hwloc_base_components;
    opal_pointer_array_add(&component_map, map);
#endif

    if (OPAL_SUCCESS != opal_timer_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("timer");
    map->components = &opal_timer_base_components_opened;
    opal_pointer_array_add(&component_map, map);
    
#if OPAL_ENABLE_FT_CR == 1
    if (OPAL_SUCCESS != opal_crs_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("crs");
    map->components = &opal_crs_base_components_available;
    opal_pointer_array_add(&component_map, map);
#endif
    
    /* OPAL's installdirs base open has already been called as part of
     * opal_init_util() back in main().
     */
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("installdirs");
    map->components = &opal_installdirs_components;
    opal_pointer_array_add(&component_map, map);
    
    /* ORTE frameworks
     * Set orte_process_info.proc_type to HNP to force all frameworks to
     * open components
     */
    orte_process_info.proc_type = ORTE_PROC_HNP;
    
    if (ORTE_SUCCESS != orte_errmgr_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("errmgr");
    map->components = &orte_errmgr_base_components_available;
    opal_pointer_array_add(&component_map, map);
    
    if (ORTE_SUCCESS != orte_grpcomm_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("grpcomm");
    map->components = &mca_grpcomm_base_components_available;
    opal_pointer_array_add(&component_map, map);
    
    if (ORTE_SUCCESS != orte_ess_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("ess");
    map->components = &orte_ess_base_components_available;
    opal_pointer_array_add(&component_map, map);
    
    if (ORTE_SUCCESS != orte_notifier_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("notifier");
    map->components = &mca_notifier_base_components_available;
    opal_pointer_array_add(&component_map, map);
    
#if !ORTE_DISABLE_FULL_SUPPORT
    if (ORTE_SUCCESS != mca_oob_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("oob");
    map->components = &mca_oob_base_components;
    opal_pointer_array_add(&component_map, map);
    
    if (ORTE_SUCCESS != orte_odls_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("odls");
    map->components = &orte_odls_base.available_components;
    opal_pointer_array_add(&component_map, map);
    
    if (ORTE_SUCCESS != orte_iof_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("iof");
    map->components = &orte_iof_base.iof_components_opened;
    opal_pointer_array_add(&component_map, map);
    
    if (ORTE_SUCCESS != orte_ras_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("ras");
    map->components = &orte_ras_base.ras_opened;
    opal_pointer_array_add(&component_map, map);
    
    if (ORTE_SUCCESS != orte_rmaps_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("rmaps");
    map->components = &orte_rmaps_base.available_components;
    opal_pointer_array_add(&component_map, map);
    
    if (ORTE_SUCCESS != orte_rml_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("rml");
    map->components = &orte_rml_base_components;
    opal_pointer_array_add(&component_map, map);
    
    if (ORTE_SUCCESS != orte_routed_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("routed");
    map->components = &orte_routed_base_components;
    opal_pointer_array_add(&component_map, map);
    
    if (ORTE_SUCCESS != orte_plm_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("plm");
    map->components = &orte_plm_base.available_components;
    opal_pointer_array_add(&component_map, map);

#if OPAL_ENABLE_FT_CR == 1
    if (ORTE_SUCCESS != orte_snapc_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("snapc");
    map->components = &orte_snapc_base_components_available;
    opal_pointer_array_add(&component_map, map);
#endif
    
    if (ORTE_SUCCESS != orte_filem_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("filem");
    map->components = &orte_filem_base_components_available;
    opal_pointer_array_add(&component_map, map);
#endif
    
    /* MPI frameworks */
    
    if (OMPI_SUCCESS != mca_allocator_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("allocator");
    map->components = &mca_allocator_base_components;
    opal_pointer_array_add(&component_map, map);
    
    if (OMPI_SUCCESS != mca_coll_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("coll");
    map->components = &mca_coll_base_components_opened;
    opal_pointer_array_add(&component_map, map);
    
    if (OMPI_SUCCESS != mca_io_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("io");
    map->components = &mca_io_base_components_opened;
    opal_pointer_array_add(&component_map, map);
    
    if (OMPI_SUCCESS != mca_rcache_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("rcache");
    map->components = &mca_rcache_base_components;
    opal_pointer_array_add(&component_map, map);
    
    if (OMPI_SUCCESS != mca_mpool_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("mpool");
    map->components = &mca_mpool_base_components;
    opal_pointer_array_add(&component_map, map);
    
    if (OMPI_SUCCESS != mca_pml_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("pml");
    map->components = &mca_pml_base_components_available;
    opal_pointer_array_add(&component_map, map);
    
    /* No need to call the bml_base_open() because the ob1 pml calls it.
     * mca_bml_base_open();
     */
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("bml");
    map->components = &mca_bml_base_components_available;
    opal_pointer_array_add(&component_map, map);
    
    if (OMPI_SUCCESS != ompi_osc_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("osc");
    map->components = &ompi_osc_base_open_components;
    opal_pointer_array_add(&component_map, map);
    
    if (OMPI_SUCCESS != mca_btl_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("btl");
    map->components = &mca_btl_base_components_opened;
    opal_pointer_array_add(&component_map, map);
    
    if (OMPI_SUCCESS != ompi_mtl_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("mtl");
    map->components = &ompi_mtl_base_components_opened;
    opal_pointer_array_add(&component_map, map);
    
    if (OMPI_SUCCESS != mca_topo_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("topo");
    map->components = &mca_topo_base_components_opened;
    opal_pointer_array_add(&component_map, map);
    
    if (OMPI_SUCCESS != ompi_pubsub_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("pubsub");
    map->components = &ompi_pubsub_base_components_available;
    opal_pointer_array_add(&component_map, map);
    
    if (OMPI_SUCCESS != ompi_dpm_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("dpm");
    map->components = &ompi_dpm_base_components_available;
    opal_pointer_array_add(&component_map, map);
    
    ompi_op_base_open();
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("op");
    map->components = &ompi_op_base_components_opened;
    opal_pointer_array_add(&component_map, map);
    
#if OPAL_ENABLE_FT_CR == 1
    if (OMPI_SUCCESS != ompi_crcp_base_open()) {
        goto error;
    }
    map = OBJ_NEW(ompi_info_component_map_t);
    map->type = strdup("crcp");
    map->components = &ompi_crcp_base_components_available;
    opal_pointer_array_add(&component_map, map);
#endif
    
    /* flag that we need to close components */
    need_close_components = true;

    /* Restore the environment to what it was before we started so that
     * if users setenv OMPI_MCA_<framework name> to some value, they'll
     * see that value when it is shown via --param output.
     */
    
    if (NULL != env_save) {
        for (i = 0; i < opal_argv_count(env_save); ++i) {
            putenv(env_save[i]);
        }
    }
    
    /* All done */
    
    opened_components = true;
    return;
    
error:
    fprintf(stderr, "%s\n", str);
    fprintf(stderr, "ompi_info will likely not display all configuration information\n");
    if (need_close_components) {
        opened_components = true;
        ompi_info_close_components();
    }
}


void ompi_info_close_components()
{
    int i;
    ompi_info_component_map_t *map;
    
    if (opened_components) {
        
        /* Note that the order of shutdown here doesn't matter because
         * we aren't *using* any components -- none were selected, so
         * there are no dependencies between the frameworks.  We list
         * them generally "in order", but it doesn't really matter.
         
         * We also explicitly ignore the return values from the
         * close() functions -- what would we do if there was an
         * error?
         */
        
#if OPAL_ENABLE_FT_CR == 1
        (void) ompi_crcp_base_close();
#endif
        (void) ompi_op_base_close();
        (void) ompi_dpm_base_close();
        (void) ompi_pubsub_base_close();
        (void) mca_topo_base_close();
        (void) mca_btl_base_close();
        (void) ompi_mtl_base_close();
        (void) mca_pml_base_close();
        (void) mca_mpool_base_close();
        (void) mca_rcache_base_close();
        (void) mca_io_base_close();
        (void) mca_coll_base_close();
        (void) mca_allocator_base_close();
        (void) ompi_osc_base_close();
        (void) orte_grpcomm_base_close();
        (void) orte_notifier_base_close();
        (void) orte_ess_base_close();
        (void) orte_show_help_finalize();
#if !ORTE_DISABLE_FULL_SUPPORT
#if OPAL_ENABLE_FT_CR == 1
        (void) orte_snapc_base_close();
#endif
        (void) orte_filem_base_close();
        (void) orte_iof_base_close();
        (void) orte_plm_base_close();
        (void) orte_odls_base_close();
        (void) orte_rmaps_base_close();
        (void) orte_ras_base_close();
        (void) orte_rml_base_close();
        (void) orte_routed_base_close();
        (void) mca_oob_base_close();
#endif
        (void) orte_errmgr_base_close();
        
        (void) opal_backtrace_base_close();
        (void) opal_memory_base_close();
        (void) opal_memchecker_base_close();
        (void) opal_paffinity_base_close();
        (void) opal_carto_base_close();
        (void) opal_maffinity_base_close();
        (void) opal_timer_base_close();
        (void) opal_sysinfo_base_close();
#if OPAL_HAVE_HWLOC
        (void) opal_hwloc_base_close();
#endif
#if OPAL_ENABLE_FT_CR == 1
        (void) opal_crs_base_close();
#endif
        
        /* Do not call OPAL's installdirs close; it will be handled in
         * opal_finalize_util().
         */
        for (i=0; i < component_map.size; i++) {
            if (NULL != (map = (ompi_info_component_map_t*)opal_pointer_array_get_item(&component_map, i))) {
                OBJ_RELEASE(map);
            }
        }
        OBJ_DESTRUCT(&component_map);
    }
    
    opened_components = false;
}
