/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MTL_MX_TYPES_H_HAS_BEEN_INCLUDED
#define MTL_MX_TYPES_H_HAS_BEEN_INCLUDED

#include "ompi_config.h"
#include "mtl_mx.h"

#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"
#include "mtl_mx_endpoint.h" 

#include "myriexpress.h"


BEGIN_C_DECLS

/** 
 * MTL Module Interface
 */
struct mca_mtl_mx_module_t { 
    mca_mtl_base_module_t super; /**< base MTL interface */
    int32_t  mx_unexp_queue_max; /**< maximium size of the MX unexpected message queue */ 
    int32_t  mx_filter; /**< user assigned value used to filter incomming messages */
    int32_t  mx_timeout;
    int32_t  mx_retries;
    int32_t  mx_support_sharedmem;
    int mx_board_num;
    int mx_endpoint_num;
    mx_endpoint_t mx_endpoint; /**< mx data structure for local endpoint */
    mx_endpoint_addr_t mx_endpoint_addr; /**< mx data structure for local endpoint address */
    mca_mtl_mx_addr_t mx_addr;
}; 
typedef struct mca_mtl_mx_module_t mca_mtl_mx_module_t;

extern mca_mtl_mx_module_t ompi_mtl_mx;

struct mca_mtl_mx_component_t{ 
    mca_mtl_base_component_2_0_0_t          super;  /**< base MTL component */ 
};
typedef struct mca_mtl_mx_component_t mca_mtl_mx_component_t;

OMPI_MODULE_DECLSPEC extern mca_mtl_mx_component_t mca_mtl_mx_component;
    

/* match/ignore bit manipulation
 *
 * 01234567 01234567 01234567 01234567 01234567 01234567 01234567 01234567
 *                  |                 |
 *      context id  |      source     |            message tag
 *                  |                 |
 */

#define MX_SOURCE_MASK   0x0000FFFF00000000ULL
#define MX_TAG_MASK      0x00000000FFFFFFFFULL

#define MX_SOURCE_IGNR   ~MX_SOURCE_MASK
    /* we need to keep top bit (sign bit) of the tag 
       collectives use this to distinguish the message */
#define MX_TAG_IGNR      0xFFFFFFFF80000000ULL

/* get the tag from the bits */ 
#define MX_GET_TAG(match_bits, tag)                 \
{                                                   \
    tag = (int) (match_bits & MX_TAG_MASK);         \
}


/* get the tag from the bits */ 
#define MX_GET_SRC(match_bits, src)                     \
{                                                       \
    src = (int) ((match_bits & MX_SOURCE_MASK) >> 32);  \
}

/* send posting */
#define MX_SET_SEND_BITS(match_bits, contextid, source, tag)            \
{                                                                       \
    match_bits = contextid;                                             \
    match_bits = (match_bits << 16);                                    \
    match_bits |= source;                                               \
    match_bits = (match_bits << 32);                                    \
    match_bits |= (MX_TAG_MASK & tag);                                  \
}

/* receive posting */
#define MX_SET_RECV_BITS(match_bits, mask_bits, contextid, source, tag) \
{                                                                       \
    match_bits = contextid;                                             \
    match_bits = (match_bits << 16);                                    \
                                                                        \
    if (MPI_ANY_SOURCE == source) {                                     \
        mask_bits = MX_SOURCE_IGNR;                                     \
    } else {                                                            \
        mask_bits = ~0;                                                 \
        match_bits |= source;                                           \
    }                                                                   \
    match_bits = (match_bits << 32);                                    \
                                                                        \
    if (MPI_ANY_TAG == tag) {                                           \
        mask_bits &= MX_TAG_IGNR;                                       \
    } else {                                                            \
        match_bits |= (MX_TAG_MASK & tag);                              \
    }                                                                   \
}
    

   
END_C_DECLS

#endif  /* MTL_MX_TYPES_H_HAS_BEEN_INCLUDED */

