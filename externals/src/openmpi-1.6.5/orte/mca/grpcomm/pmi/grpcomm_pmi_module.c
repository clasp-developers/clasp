/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2007      The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Los Alamos National Security, LLC. All
 *                         rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <string.h>
#include <pmi.h>
#if WANT_CRAY_PMI2_EXT
#include <pmi2.h>
#endif

#include "opal/dss/dss.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"

#include "orte/mca/grpcomm/base/base.h"
#include "grpcomm_pmi.h"


/* Static API's */
static int init(void);
static void finalize(void);
static int xcast(orte_jobid_t job,
                 opal_buffer_t *buffer,
                 orte_rml_tag_t tag);
static int pmi_allgather(opal_buffer_t *sbuf, opal_buffer_t *rbuf);
static int pmi_allgather_list(opal_list_t *names,
                              opal_buffer_t *sbuf, opal_buffer_t *rbuf);
static int pmi_barrier(void);
static int pmi_set_proc_attr(const char* attr_name, 
                             const void *buffer, size_t size);
static int pmi_get_proc_attr(const orte_process_name_t name,
                             const char* attr_name,
                             void **buffer, size_t *size);
static int modex(opal_list_t *procs);
static int purge_proc_attrs(void);

/* Module def */
orte_grpcomm_base_module_t orte_grpcomm_pmi_module = {
    init,
    finalize,
    xcast,
    pmi_allgather,
    pmi_allgather_list,
    pmi_barrier,
    NULL,
    pmi_set_proc_attr,
    pmi_get_proc_attr,
    modex,
    purge_proc_attrs
};

static int pmi_encode(const void *val, size_t vallen);
static void* pmi_decode(size_t *retlen);
static int setup_pmi(void);
static int setup_key(const orte_process_name_t *name, const char *key);

/* Local variables */
static char *pmi_kvs_name = NULL;
static char *pmi_kvs_key = NULL;
static char *pmi_attr_val = NULL;
static int pmi_vallen_max = -1;
static int pmi_keylen_max = -1;

/* Because Cray uses PMI2 extensions for some, but not all,
 * PMI functions, we define a set of wrappers for those
 * common functions we will use
 */
static int kvs_put(const char *key, const char *value)
{
#if WANT_CRAY_PMI2_EXT
    return PMI2_KVS_Put(key, value);
#else
    return PMI_KVS_Put(pmi_kvs_name, key, value);
#endif
}

static int kvs_get(const char *key, char *value, int valuelen)
{
#if WANT_CRAY_PMI2_EXT
    int len;

    return PMI2_KVS_Get(pmi_kvs_name, PMI2_ID_NULL, key, value, valuelen, &len);
#else
    return PMI_KVS_Get(pmi_kvs_name, key, value, valuelen);
#endif
}

static int kvs_commit(void)
{
#if WANT_CRAY_PMI2_EXT
    return PMI2_KVS_Fence();
#else
    int rc;

    if (PMI_SUCCESS != (rc = PMI_KVS_Commit(pmi_kvs_name))) {
        return rc;
    }
    /* Barrier here to ensure all other procs have committed */
    return PMI_Barrier();
#endif
}

/**
 * Initialize the module
 */
static int init(void)
{
    int rc;

    if (NULL == pmi_kvs_name) {
        if (ORTE_SUCCESS != (rc = setup_pmi())) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    return ORTE_SUCCESS;
}

/**
 * Finalize the module
 */
static void finalize(void)
{
    if (NULL != pmi_kvs_name) {
        free(pmi_kvs_name);
        pmi_kvs_name = NULL;
    }
    if (NULL != pmi_kvs_key) {
        free(pmi_kvs_key);
        pmi_kvs_key = NULL;
    }
    if (NULL != pmi_attr_val) {
        free(pmi_attr_val);
        pmi_attr_val = NULL;
    }
    return;
}

/**
 *  A "broadcast-like" function to a job's processes.
 *  @param  jobid   The job whose processes are to receive the message
 *  @param  buffer  The data to broadcast
 */

static int xcast(orte_jobid_t job,
                 opal_buffer_t *buffer,
                 orte_rml_tag_t tag)
{
    /* not used in this module */
    return ORTE_ERR_NOT_SUPPORTED;
}

static int pmi_barrier(void)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:pmi entering barrier",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if I am alone, just return */
    if (1 == orte_process_info.num_procs) {
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                             "%s grpcomm:pmi:barrier only one proc",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        return ORTE_SUCCESS;
    }
    
#if WANT_CRAY_PMI2_EXT
    /* Cray doesn't provide a barrier, so use the Fence function here */
    if (PMI_SUCCESS != (rc = PMI2_KVS_Fence())) {
        ORTE_PMI_ERROR(rc, "PMI2_KVS_Fence");
        return ORTE_ERROR;
    }
#else
    /* use the PMI barrier function */
    if (PMI_SUCCESS != (rc = PMI_Barrier())) {
        ORTE_PMI_ERROR(rc, "PMI_Barrier");
        return ORTE_ERROR;
    }
#endif

    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s grpcomm:pmi barrier complete",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    return ORTE_SUCCESS;
}

static int pmi_allgather(opal_buffer_t *sbuf, opal_buffer_t *rbuf)
{
    /* not used in this implementation */
    return ORTE_ERR_NOT_SUPPORTED;
}

static int pmi_allgather_list(opal_list_t *names,
                              opal_buffer_t *sbuf, opal_buffer_t *rbuf)
{
    /* no idea how to do this - only occurs for comm_spawn,
     * which this module doesn't support
     */
    return ORTE_ERR_NOT_SUPPORTED;
}

static int pmi_set_proc_attr(const char* attr_name, 
                             const void *buffer, size_t size)
{
    int rc;

     if (NULL == pmi_kvs_name) {
         if (ORTE_SUCCESS != (rc = setup_pmi())) {
             ORTE_ERROR_LOG(rc);
             return rc;
         }
    }

     if (ORTE_SUCCESS != (rc = setup_key(ORTE_PROC_MY_NAME, attr_name))) {
         ORTE_ERROR_LOG(rc);
         return rc;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:pmi: set attr %s of size %lu in KVS %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), attr_name,
                         (unsigned long)size, pmi_kvs_name));
    
    if (ORTE_SUCCESS != (rc = pmi_encode(buffer, size))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    rc = kvs_put(pmi_kvs_key, pmi_attr_val);
    if (PMI_SUCCESS != rc) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Put");
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}

static int pmi_get_proc_attr(const orte_process_name_t name,
                             const char* attr_name,
                             void **buffer, size_t *size)
{
    int rc;

    /* set default */
    *size = 0;
    *buffer = NULL;

    if (NULL == pmi_kvs_name) {
         if (ORTE_SUCCESS != (rc = setup_pmi())) {
             ORTE_ERROR_LOG(rc);
             return rc;
         }
    }

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:pmi: get attr %s for proc %s in KVS %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), attr_name,
                         ORTE_NAME_PRINT(&name), pmi_kvs_name));
    
    if (ORTE_SUCCESS != (rc = setup_key(&name, attr_name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    rc = kvs_get(pmi_kvs_key, pmi_attr_val, pmi_vallen_max);
    if (PMI_SUCCESS != rc) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Get");
        return ORTE_ERROR;
    }
    *buffer = pmi_decode(size);
    if (NULL == *buffer) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:pmi: got attr %s of size %lu",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         attr_name, (unsigned long)(*size)));
    
    return ORTE_SUCCESS;
}

/***   MODEX SECTION ***/
static int modex(opal_list_t *procs)
{
    int rc, i;
    size_t len;
    char *rml_uri, val[64];
    orte_vpid_t v;
    orte_process_name_t name;
    orte_jmap_t *jmap;
    orte_nid_t *nid, *loc;
    orte_pmap_t *pmap;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:pmi: modex entered",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (NULL == pmi_kvs_name) {
        if (ORTE_SUCCESS != (rc = setup_pmi())) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* provide our hostname so others can know our location */
    if (strlen(orte_process_info.nodename) > (size_t)pmi_vallen_max) {
        ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
        return ORTE_ERR_VALUE_OUT_OF_BOUNDS;
    }
    if (ORTE_SUCCESS != (rc = setup_key(ORTE_PROC_MY_NAME, "HOSTNAME"))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    rc = kvs_put(pmi_kvs_key, orte_process_info.nodename);
    if (PMI_SUCCESS != rc) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Put");
        return ORTE_ERROR;
    }

    /* add our oob endpoint info so that oob communications
     * can be supported
     */
    rml_uri = orte_rml.get_contact_info();
    if (strlen(rml_uri) > (size_t)pmi_vallen_max) {
        ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
        return ORTE_ERROR;
    }
    if (ORTE_SUCCESS != (rc = setup_key(ORTE_PROC_MY_NAME, "RMLURI"))) {
        ORTE_ERROR_LOG(rc);
        free(rml_uri);
        return rc;
    }
    /* NTH: some characters are not allowed in pmi2 land so we need to encode */
    if (ORTE_SUCCESS != (rc = pmi_encode(rml_uri, strlen(rml_uri)))) {
        ORTE_ERROR_LOG(rc);
        free(rml_uri);
        return rc;
    }
    /* encoding puts the encoded value in pmi_attr_val */
    rc = kvs_put(pmi_kvs_key, pmi_attr_val);
    if (PMI_SUCCESS != rc) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Put");
        free(rml_uri);
        return ORTE_ERROR;
    }
    free(rml_uri);

    /* get the job map for this job */
    jmap = (orte_jmap_t*)opal_pointer_array_get_item(&orte_jobmap, 0);
    /* get my pidmap entry */
    pmap = (orte_pmap_t*)opal_pointer_array_get_item(&jmap->pmap, ORTE_PROC_MY_NAME->vpid);

    /* add our local/node rank info */
    if (ORTE_SUCCESS != (rc = setup_key(ORTE_PROC_MY_NAME, "LOCALRANK"))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    snprintf(val, 64, "%lu", (unsigned long)pmap->local_rank);
    rc = kvs_put(pmi_kvs_key, val);
    if (PMI_SUCCESS != rc) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Put");
        return ORTE_ERROR;
    }
    if (ORTE_SUCCESS != (rc = setup_key(ORTE_PROC_MY_NAME, "NODERANK"))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    snprintf(val, 64, "%lu", (unsigned long)pmap->node_rank);
    rc = kvs_put(pmi_kvs_key, val);
    if (PMI_SUCCESS != rc) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Put");
        return ORTE_ERROR;
    }

    /* commit our modex info */
    if (PMI_SUCCESS != (rc = kvs_commit())) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Commit failed");
        return ORTE_ERROR;
    }

    /* harvest the oob endpoint info and hostname for all other procs
     * in our job so oob wireup can be completed and we
     * can setup their nidmap/pidmap
     */
    name.jobid = ORTE_PROC_MY_NAME->jobid;
    orte_process_info.num_nodes = 1; /* have to account for mine! */
    for (v=0; v < orte_process_info.num_procs; v++) {
        if (v == ORTE_PROC_MY_NAME->vpid) {
            continue;
        }
        name.vpid = v;

        if (ORTE_SUCCESS != (rc = setup_key(&name, "RMLURI"))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        rc = kvs_get(pmi_kvs_key, pmi_attr_val, pmi_vallen_max);
        if (PMI_SUCCESS != rc) {
            ORTE_PMI_ERROR(rc, "PMI_KVS_Get");
            return ORTE_ERROR;
        }
        /* Had to encode to protect against pmi2-prohibited chars */
	rml_uri = pmi_decode(&len);
	if (NULL == rml_uri) {
	    return ORTE_ERROR;
	}
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                             "%s grpcomm:pmi: proc %s oob endpoint %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&name), rml_uri));
        /* set the contact info into the hash table */
        if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(rml_uri))) {
            free(rml_uri);
            return rc;
        }
        free(rml_uri);

        if (ORTE_SUCCESS != (rc = setup_key(&name, "HOSTNAME"))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        rc = kvs_get(pmi_kvs_key, pmi_attr_val, pmi_vallen_max);
        if (PMI_SUCCESS != rc) {
            ORTE_PMI_ERROR(rc, "PMI_KVS_Get");
            return ORTE_ERROR;
        }
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                             "%s grpcomm:pmi: proc %s location %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&name), pmi_attr_val));
        /* see if this node is already in nidmap */
        loc = NULL;
        for (i=0; i < orte_nidmap.size; i++) {
            if (NULL == (nid = (orte_nid_t*)opal_pointer_array_get_item(&orte_nidmap, i))) {
                continue;
            }
            if (0 == strcmp(pmi_attr_val, nid->name)) {
                /* found it */
                loc = nid;
                break;
            }
        }
        if (NULL == loc) {
            /* new node - save it */
            loc = OBJ_NEW(orte_nid_t);
            loc->name = strdup(pmi_attr_val);
            loc->index = opal_pointer_array_add(&orte_nidmap, loc);
            loc->daemon = loc->index;
            /* keep track */
            orte_process_info.num_nodes++;
        }
        /* see if this proc is already in the pidmap */
        if (NULL == (pmap = (orte_pmap_t*)opal_pointer_array_get_item(&jmap->pmap, v))) {
            /* nope - add it */
            pmap = OBJ_NEW(orte_pmap_t);
            pmap->node = loc->index;
            if (ORTE_SUCCESS != (rc = opal_pointer_array_set_item(&jmap->pmap, v, pmap))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        /* get the proc's local/node rank info */
        if (ORTE_SUCCESS != (rc = setup_key(&name, "LOCALRANK"))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        rc = kvs_get(pmi_kvs_key, pmi_attr_val, pmi_vallen_max);
        if (PMI_SUCCESS != rc) {
            ORTE_PMI_ERROR(rc, "PMI_KVS_Get");
            return ORTE_ERROR;
        }
        pmap->local_rank = (orte_local_rank_t)strtoul(pmi_attr_val, NULL, 10);
        if (ORTE_SUCCESS != (rc = setup_key(&name, "NODERANK"))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        rc = kvs_get(pmi_kvs_key, pmi_attr_val, pmi_vallen_max);
        if (PMI_SUCCESS != rc) {
            ORTE_PMI_ERROR(rc, "PMI_KVS_Get");
            return ORTE_ERROR;
        }
        pmap->node_rank = (orte_node_rank_t)strtoul(pmi_attr_val, NULL, 10);
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                             "%s grpcomm:pmi: proc %s lrank %u nrank %u",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&name),
                             (unsigned int)pmap->local_rank,
                             (unsigned int)pmap->node_rank));
    }

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm:pmi: modex completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return rc;
}

static int purge_proc_attrs(void)
{
    /* nothing to do here */
    return ORTE_SUCCESS;
}

/* PMI only supports strings. For now, do a simple base16 
 * encoding. Should do something smarter, both with the 
 * algorith used and its implementation. */
static int pmi_encode(const void *val, size_t vallen) {
    static unsigned char encodings[] = {'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'};
    size_t i;

    /* check for size */
    if ((size_t)pmi_vallen_max < ((vallen * 2) + 1)) {
        return ORTE_ERR_VALUE_OUT_OF_BOUNDS;
    }
    for (i = 0; i < vallen; i++) {
        pmi_attr_val[2 * i] = encodings[((unsigned char *)val)[i] & 0xf];
        pmi_attr_val[2 * i + 1] = encodings[((unsigned char *)val)[i] >> 4];
    }
    pmi_attr_val[vallen * 2] = '\0';
    return ORTE_SUCCESS;
}

static void* pmi_decode(size_t *retlen) {
    unsigned char *ret, *val;
    size_t i;
    *retlen = strlen(pmi_attr_val)/2;

    ret = calloc(1, *retlen + 1);
    if (NULL == ret) {
        return ret;
    }

    val = (unsigned char*)pmi_attr_val;
    for (i = 0; i < *retlen; i++) {
        if (*val >= '0' && *val <= '9') {
            ret[i] = *val - '0';
        } else {
            ret[i] = *val - 'a' + 10;
        }
        val++;
        if (*val >= '0' && *val <= '9') {
            ret[i] |= ((*val - '0') << 4);
        } else {
            ret[i] |= ((*val - 'a' + 10) << 4);
        }
        val++;
    }
    return ret;
}

static int setup_pmi(void)
{
    int max_length, rc;

#if WANT_CRAY_PMI2_EXT
    pmi_vallen_max = PMI2_MAX_VALLEN;
#else
    rc = PMI_KVS_Get_value_length_max(&pmi_vallen_max);
    if (PMI_SUCCESS != rc) {
        ORTE_PMI_ERROR(rc, "PMI_Get_value_length_max");
        return ORTE_ERROR;
    }
#endif
    pmi_attr_val = malloc(pmi_vallen_max);
    if (NULL == pmi_attr_val) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

#if WANT_CRAY_PMI2_EXT
    /* TODO -- is this ok */
    max_length = 1024;
#else
    if (PMI_SUCCESS != (rc = PMI_KVS_Get_name_length_max(&max_length))) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Get_name_length_max");
        return ORTE_ERROR;
    }
#endif
    pmi_kvs_name = malloc(max_length);
    if (NULL == pmi_kvs_name) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

#if WANT_CRAY_PMI2_EXT
    rc = PMI2_Job_GetId(pmi_kvs_name, max_length);
#else
    rc = PMI_KVS_Get_my_name(pmi_kvs_name,max_length);
#endif
    if (PMI_SUCCESS != rc) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Get_my_name");
        return ORTE_ERROR;
    }

#if WANT_CRAY_PMI2_EXT
    pmi_keylen_max = PMI2_MAX_KEYLEN;
#else
    if (PMI_SUCCESS != (rc = PMI_KVS_Get_key_length_max(&pmi_keylen_max))) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Get_key_length_max");
        return ORTE_ERROR;
    }
#endif
    pmi_kvs_key = malloc(pmi_keylen_max);

    return ORTE_SUCCESS;
}

static int setup_key(const orte_process_name_t *name, const char *key)
{
    if (pmi_keylen_max <= snprintf(pmi_kvs_key, pmi_keylen_max,
                                   "%s-%s", ORTE_NAME_PRINT(name), key)) {
        return ORTE_ERR_VALUE_OUT_OF_BOUNDS;
    }

    return ORTE_SUCCESS;
}

