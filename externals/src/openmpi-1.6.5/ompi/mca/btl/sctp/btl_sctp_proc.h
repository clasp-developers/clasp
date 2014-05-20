/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#ifndef MCA_BTL_SCTP_PROC_H
#define MCA_BTL_SCTP_PROC_H

#include "opal/class/opal_object.h"
#include "ompi/proc/proc.h"
#include "btl_sctp.h"
#include "btl_sctp_addr.h"
#include "btl_sctp_endpoint.h"
#include <netinet/sctp.h>

BEGIN_C_DECLS

/**
 * Represents the state of a remote process and the set of addresses
 * that it exports. Also cache an instance of mca_btl_base_endpoint_t for
 * each
 * BTL instance that attempts to open a connection to the process.
 */
struct mca_btl_sctp_proc_t {
    opal_list_item_t super;                  
    /**< allow proc to be placed on a list */

    ompi_proc_t *proc_ompi;                  
    /**< pointer to corresponding ompi_proc_t */

    orte_process_name_t proc_name;           
    /**< globally unique identifier for the process */

    struct mca_btl_sctp_addr_t* proc_addrs;
    /**< array of addresses exported by peer */

    size_t proc_addr_count;                  
    /**< number of addresses published by endpoint */

    struct mca_btl_base_endpoint_t **proc_endpoints; 
    /**< array of endpoints that have been created to access this proc */    

    size_t proc_endpoint_count;                  
    /**< number of endpoints */

    opal_mutex_t proc_lock;                  
    /**< lock to protect against concurrent access to proc state */
};
typedef struct mca_btl_sctp_proc_t mca_btl_sctp_proc_t;
OBJ_CLASS_DECLARATION(mca_btl_sctp_proc_t);

mca_btl_sctp_proc_t* mca_btl_sctp_proc_create(ompi_proc_t* ompi_proc);
mca_btl_sctp_proc_t* mca_btl_sctp_proc_lookup(const orte_process_name_t* name);
int  mca_btl_sctp_proc_insert(mca_btl_sctp_proc_t*, mca_btl_base_endpoint_t*);
int  mca_btl_sctp_proc_remove(mca_btl_sctp_proc_t*, mca_btl_base_endpoint_t*);
bool mca_btl_sctp_proc_accept(mca_btl_sctp_proc_t*, struct sockaddr_in*, int);


/**
 * Inlined function to return local SCTP proc instance.
 */

static inline mca_btl_sctp_proc_t* mca_btl_sctp_proc_local(void)
{
    if(NULL == mca_btl_sctp_component.sctp_local) {
        mca_btl_sctp_component.sctp_local = mca_btl_sctp_proc_create(ompi_proc_local());
    }
    return mca_btl_sctp_component.sctp_local;
}

enum {
    INVALID_ENTRY = 0,
    VALID_ENTRY = 1
};
/* Table of procs indexed by SCTP association id used by the receiver to
 * identify senders. It is initialized to 0 in mca_btl_sctp_component_init().
 * NOTE: 256 matches the size of the proc hash_table
 */
#define MCA_BTL_SCTP_PROC_TABLE_SIZE 256
struct mca_btl_sctp_proc_table_node {
    int valid;
    sctp_assoc_t sctp_assoc_id;
    orte_vpid_t vpid;
    struct mca_btl_sctp_proc_t *proc;
};
typedef struct mca_btl_sctp_proc_table_node mca_btl_sctp_proc_table_node;

extern struct mca_btl_sctp_proc_table_node *recvr_proc_table;
extern struct mca_btl_sctp_proc_table_node *sender_proc_table;

int mca_btl_sctp_proc_check_vpid(orte_vpid_t vpid, struct mca_btl_sctp_proc_table_node *table);
int mca_btl_sctp_proc_check_assoc_id(sctp_assoc_t id, struct mca_btl_sctp_proc_table_node *table);
void mca_btl_sctp_proc_add_vpid(orte_vpid_t vpid, struct mca_btl_sctp_proc_t *proc, struct mca_btl_sctp_proc_table_node *table);
void mca_btl_sctp_proc_add_assoc_id(sctp_assoc_t id, struct mca_btl_sctp_proc_t *proc, struct mca_btl_sctp_proc_table_node *table);
mca_btl_sctp_proc_t *mca_btl_sctp_proc_get(sctp_assoc_t id, struct mca_btl_sctp_proc_table_node *table);

END_C_DECLS
#endif
