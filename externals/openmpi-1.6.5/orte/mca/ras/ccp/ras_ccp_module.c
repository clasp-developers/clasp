/*
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"
#include "orte/constants.h"

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <comutil.h>

#include <errno.h>
#include <string.h>

#include "opal/util/output.h"
#include "orte/util/show_help.h"

#include "orte/runtime/orte_globals.h"

#include "orte/mca/ras/base/ras_private.h"
#include "ras_ccp.h"

/* Import the Windows CCP API. */
#import "ccpapi.tlb" named_guids no_namespace raw_interfaces_only   \
    rename("SetEnvironmentVariable","SetEnvVar")                    \
    rename("GetJob", "GetSingleJob")                                \
    rename("AddJob", "AddSingleJob")

/* Include the library for ::ConvertBSTRToString */
#pragma comment(lib, "comsuppw.lib")

/*
 * Local functions
 */
static int orte_ras_ccp_allocate(opal_list_t *nodes);
static int orte_ras_ccp_finalize(void);
static int discover(opal_list_t* nodelist, ICluster* pCluster);
void ras_get_cluster_message(ICluster* pCluster);


/*
 * Local variables
 */
orte_ras_base_module_t orte_ras_ccp_module = {
    orte_ras_ccp_allocate,
    orte_ras_ccp_finalize
};


/**
 * Discover available (pre-allocated) nodes.  Allocate the
 * requested number of nodes/process slots to the job.
 */
static int orte_ras_ccp_allocate(opal_list_t *nodes)
{
    int ret, i;
    size_t len;
    char *cluster_head = NULL;
    HRESULT hr = S_OK;
    ICluster* pCluster = NULL;

    /* CCP is not thread safe. Use the apartment model. */
    CoInitializeEx(NULL, COINIT_APARTMENTTHREADED);

    /* Create the Cluster object. */
    hr = CoCreateInstance( __uuidof(Cluster),
                           NULL,
                           CLSCTX_INPROC_SERVER,
                           __uuidof(ICluster),
                           reinterpret_cast<void **> (&pCluster) );

    if (FAILED(hr)) {
        OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                            "ras:ccp:allocate: failed to create cluster object!"));
        return ORTE_ERROR;
    }
    
    if(NULL == orte_ccp_headnode) {
        /* Get the cluster head nodes name */
        _dupenv_s(&cluster_head, &len, "LOGONSERVER");

        if(cluster_head == NULL) {
            OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                                "ras:ccp:allocate: connot find cluster head node!"));
            return ORTE_ERROR;
        }

        /* Get rid of the beginning '//'. */
        for( i = 0; i < len - 2; i++){
            cluster_head[i] = cluster_head[i+2];
            cluster_head[i+2] = '\0';
        }
    } else {
        cluster_head = orte_ccp_headnode;
    }

    /* Connect to the cluster's head node */
    hr = pCluster->Connect(_bstr_t(cluster_head));
    if (FAILED(hr)) {
        ras_get_cluster_message(pCluster);
        OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                            "ras:ccp:allocate: connection failed!"));
        return ORTE_ERROR;
    }
   
    if (ORTE_SUCCESS != (ret = discover(nodes, pCluster))) {
        OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                            "ras:ccp:allocate: discover failed!"));
        return ret;
    }
        
    /* in the CCP world, if we didn't find anything, then this
     * is an unrecoverable error - report it
     */
    if (opal_list_is_empty(nodes)) {
        orte_show_help("help-ras-ccp.txt", "no-nodes-found", true);
        return ORTE_ERR_NOT_FOUND;
    }

    /* All finished, release cluster object*/
    pCluster->Release();
    CoUninitialize();

    return ret;
}

/*
 * There's really nothing to do here
 */
static int orte_ras_ccp_finalize(void)
{
    OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                        "ras:ccp:finalize: success (nothing to do)"));
    return ORTE_SUCCESS;
}


/**
 * Discover the available resources.  Obtain directly from head node
 *
 *  - validate any Windows Cluster nodes
 *  - check for additional nodes that have already been allocated
 */

static int discover(opal_list_t* nodelist, ICluster* pCluster)
{
    int ret = ORTE_ERROR;
    int32_t nodeid;
    orte_node_t *node;
    opal_list_item_t* item;
    opal_list_t new_nodes;
    struct timeval start, stop;

    HRESULT hr = S_OK;  
    long idle_processors = 0;
    IClusterEnumerable* pNodesCollection = NULL;
    IEnumVARIANT* pNodes = NULL;
    INode* pNode = NULL;
    BSTR node_name = NULL, node_arch = NULL;
    VARIANT var;
    NodeStatus Status;
    size_t len;

    /* check for timing request - get start time if so */
    if (orte_timing) {
        gettimeofday(&start, NULL);
    }
    
    /* Get the collection of nodes. */
    hr = pCluster->get_ComputeNodes(&pNodesCollection);
    if (FAILED(hr)) {
        ras_get_cluster_message(pCluster);
        OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                            "ras:ccp:pCluster->get_ComputeNodes failed."));
        return ORTE_ERROR;
    }

    /* Get the enumerator used to iterate through the collection. */
    hr = pNodesCollection->GetEnumerator(&pNodes);
    if (FAILED(hr)) {
        ras_get_cluster_message(pCluster);
        OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                            "ras:ccp:pNodesCollection->GetEnumerator failed."));
        return ORTE_ERROR;
    }

    VariantInit(&var);

    /* Construct new node list. */
    OBJ_CONSTRUCT(&new_nodes, opal_list_t);
    nodeid=0;

    /* Loop through the collection. */
    while (hr = pNodes->Next(1, &var, NULL) == S_OK) {
        var.pdispVal->QueryInterface(IID_INode, reinterpret_cast<void **> (&pNode));

        /* Check wether the node is ready. 
         * There are four states:
         *          NodeStatus_Ready = 0,
         *          NodeStatus_Paused = 1,
         *          NodeStatus_Unreachable = 2, probably not a windows cluster node.
         *          NodeStatus_PendingApproval = 3
         */
        hr = pNode->get_Status(&Status);
        if (FAILED(hr)) {
            OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                                "ras:ccp:pNode->get_Status failed."));
            ret = ORTE_ERROR;
            goto cleanup;
        }

        /* Get available number of processors on each node. */
        hr = pNode->get_NumberOfIdleProcessors(&idle_processors);
        if (FAILED(hr)) {
            OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                                "ras:ccp:pNode->get_NumberOfIdleProcessors failed."));
            ret = ORTE_ERROR;
            goto cleanup;
        }

        /* Do we have enough processors on the available nodes? 
         * Question: How do we get the required number of processors?
         */
        if ( (Status == NodeStatus_Ready) && (idle_processors > 0) ) {

            /* Get node name. */
            hr = pNode->get_Name(&node_name);
            if (FAILED(hr)) {
                OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                                    "ras:ccp:pNode->get_Name failed."));
                ret = ORTE_ERROR;
                goto cleanup;
            }

            /* Get node processor architecture. */
            hr = pNode->get_ProcessorArchitecture(&node_arch);
            if (FAILED(hr)) {
                OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                                    "ras:ccp:pNode->get_ProcessorArchitecture failed."));
                ret = ORTE_ERROR;
                goto cleanup;
            }

            /* Prevent duplicated nodes in the list*/
            for (item = opal_list_get_first(&new_nodes);
                 opal_list_get_end(&new_nodes) != item;
                 item = opal_list_get_next(item)) {

                node = (orte_node_t*) item;
                if (0 == strcmp(node->name, (char *)node_name)) {
                    ++node->slots;
                    OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                                        "ras:ccp:allocate:discover: found -- bumped slots to %d",
                                        node->slots));
                    break;
                }
            }
            /* Did we find it? */

            if (opal_list_get_end(&new_nodes) == item) {

                /* Nope -- didn't find it, so add a new item to the list */

                OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                                    "ras:ccp:allocate:discover: not found -- added to list"));

                node = OBJ_NEW(orte_node_t);

                /* The function _dupenv_s is much safer than getenv on Windows. */
                _dupenv_s(&node->username, &len, "username");

                node->name = _com_util::ConvertBSTRToString(node_name);
                node->launch_id = nodeid;
                node->slots_inuse = 0;
                node->slots_max = 0;
                node->slots = 1;
                opal_list_append(nodelist, &node->super);
            } 
            /* up the nodeid */
            nodeid++;
        }

        pNode->Release();
        VariantClear(&var);
    }

    pNodes->Release();

    if (nodeid > 0) ret = ORTE_SUCCESS;

    /* All done */
cleanup:

    if (ORTE_SUCCESS == ret) {
        OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                            "ras:ccp:allocate:discover: success"));
    } else {
        OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                            "ras:ccp:allocate:discover: failed (rc=%d)", ret));
    }

    OBJ_DESTRUCT(&new_nodes);
    SysFreeString(node_name);
    SysFreeString(node_arch);

    /* check for timing request - get stop time and report elapsed time if so */
    if (orte_timing) {
        gettimeofday(&stop, NULL);
        opal_output(0, "ras_ccp: time to allocate is %ld usec",
                    (long int)((stop.tv_sec - start.tv_sec)*1000000 +
                               (stop.tv_usec - start.tv_usec)));
        gettimeofday(&start, NULL);
    }
    
    return ret;
}

void ras_get_cluster_message(ICluster* pCluster)
{
    HRESULT hr = S_OK;
    BSTR message = NULL;

    hr = pCluster->get_ErrorMessage(&message);
    if (SUCCEEDED(hr)) {
        OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                            _com_util::ConvertBSTRToString(message)));
        SysFreeString(message);
    }
    else {
        OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                            "pCluster->get_ErrorMessage failed.\n"));
    }
}
