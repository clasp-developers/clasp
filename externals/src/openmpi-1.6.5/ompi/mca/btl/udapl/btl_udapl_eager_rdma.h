/*
 * Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BTL_UDAPL_EAGER_RDMA_H
#define MCA_BTL_UDAPL_EAGER_RDMA_H

/* Open MPI includes */
#include "ompi/mca/btl/udapl/btl_udapl_endpoint.h" 


BEGIN_C_DECLS

/*
 * Describe endpoint local memory region.
 */
struct mca_btl_udapl_eager_rdma_local_t {
    ompi_ptr_t		base;		/**< points to fragment structures */
    struct mca_btl_udapl_reg_t* reg;
    uint8_t 		head; 		/**< RDMA buffer to poll */
    int32_t 		credits; 	/**< number of local rdma buffers ready to be reclaimed,
					   reused. Initially equal to 0. */
    opal_mutex_t 	lock; /**< protect access to RDMA buffer */    
};
typedef struct mca_btl_udapl_eager_rdma_local_t mca_btl_udapl_eager_rdma_local_t;

/*
 * Describe endpoint remote memory region.
 */
struct mca_btl_udapl_eager_rdma_remote_t {
    ompi_ptr_t		base;   /**< points to start of data region, not
				   fragment structures */
    DAT_RMR_CONTEXT	rkey; 	/**< key required to access remote memory */ 
    uint8_t 		head; 	/**< RDMA buffer to use */
    int32_t 		tokens; /**< number of available rdma buffers, initially equal
				   to mca parameter eager_rdma_num  */
    opal_mutex_t 	lock; /**< protect access to RDMA buffer */    
};
typedef struct mca_btl_udapl_eager_rdma_remote_t mca_btl_udapl_eager_rdma_remote_t;

/*
 * Encapsulate data that describes a remote memory region. 
 */
struct mca_btl_udapl_eager_rdma_connect_t {
	mca_btl_udapl_control_header_t control;
	uint32_t 	rkey;
        ompi_ptr_t 	rdma_start;
};
typedef struct mca_btl_udapl_eager_rdma_connect_t mca_btl_udapl_eager_rdma_connect_t;
    
/*
 * Encapsulate data that describes rdma credit information.
 */
struct mca_btl_udapl_eager_rdma_credit_t {
	mca_btl_udapl_control_header_t control;
        uint32_t 	credits;
};
typedef struct mca_btl_udapl_eager_rdma_credit_t mca_btl_udapl_eager_rdma_credit_t;
    
#define EAGER_RDMA_BUFFER_AVAILABLE (0)
#define EAGER_RDMA_BUFFER_IN_USE (0xff)

#define MCA_BTL_UDAPL_RDMA_FRAG_IN_USE(F) do {               \
    *(volatile uint8_t*) ((char*)(F) + 				\
    (mca_btl_udapl_component.udapl_eager_rdma_frag_size -		\
    (sizeof(mca_btl_udapl_footer_t)))); 			\
    } while (0) 

#define MCA_BTL_UDAPL_RDMA_FRAG_ASSIGN_IN_USE(F) do {           	\
    *(volatile uint8_t*) ((char*)(F) + 					\
    (mca_btl_udapl_component.udapl_eager_rdma_frag_size-			\
    (sizeof(mca_btl_udapl_footer_t)))) = EAGER_RDMA_BUFFER_IN_USE; \
    } while (0) 

#define MCA_BTL_UDAPL_RDMA_FRAG_ASSIGN_AVAILABLE(F) do {                	\
    *(volatile uint8_t*) ((char*)(F) + 						\
    (mca_btl_udapl_component.udapl_eager_rdma_frag_size -				\
    (sizeof(mca_btl_udapl_footer_t)))) = EAGER_RDMA_BUFFER_AVAILABLE; 	\
    } while (0) 
    
/* Retrieve the rdma fragment at location I */
#define MCA_BTL_UDAPL_GET_LOCAL_RDMA_FRAG(E, I)                         \
            (mca_btl_udapl_frag_t*)                                     \
            ((char*)(E)->endpoint_eager_rdma_local.base.pval +          \
            (I) * sizeof(mca_btl_udapl_frag_eager_rdma_t))

/*
 * Increment the index I by one while not exceeding the total number of
 * available eager rdma fragments
 */
#define MCA_BTL_UDAPL_RDMA_NEXT_INDEX(I) do {                                   \
                            (I) = ((I) + 1);                                    \
                            if((I) ==                                           \
                                mca_btl_udapl_component.udapl_eager_rdma_num)         \
                                (I) = 0;                                            \
                        } while (0)
    
END_C_DECLS
#endif
