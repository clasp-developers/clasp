/*
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BTL_UDAPL_MCA_H
#define MCA_BTL_UDAPL_MCA_H

BEGIN_C_DECLS

/* Define Integer Boundaries */
#define REGINT_NEG_ONE_OK 0x01            /* value = -1 is valid */
#define REGINT_GE_ZERO 0x02               /* value >= 0 is valid */
#define REGINT_GE_ONE 0x04                /* value > 1  is valid */
#define REGINT_NONZERO 0x08               /* value != 0 is valid */

/* Define String Boundaries */
#define REGSTR_EMPTY_OK 0x01              /* empty string is valid */
#define REGSTR_EMPTY_NOT_OK 0x02          /* empty string is not valid */

/* Define default parameter values that need to be known beyond the
 * initial setting; for example, if a parameter is tuned dynamically
 * by the BTL it would not be advisable to do so if the user has
 * modified the default. 
 */
#define MCA_BTL_UDAPL_ASYNC_EVD_QLEN_DEFAULT 256
#define MCA_BTL_UDAPL_CONN_EVD_QLEN_DEFAULT 256
#define MCA_BTL_UDAPL_DTO_EVD_QLEN_DEFAULT 256
#define MCA_BTL_UDAPL_CONN_TIMEOUT_DEFAULT 10000000
#define MCA_BTL_UDAPL_CONN_TIMEOUT_INC 200000	/* connection timeout
						 * is in microseconds;
						 * this incremental
						 * value is equivalent
						 * to .2 seconds
						 */
#define MCA_BTL_UDAPL_CONN_TIMEOUT_MAX 2147483647 
#define MCA_BTL_UDAPL_MAX_RECV_DTOS_DEFAULT 8
#define MCA_BTL_UDAPL_MAX_REQUEST_DTOS_DEFAULT 76
#define MCA_BTL_UDAPL_NUM_RECVS_DEFAULT 8

    
#define CHECK_PARAM_REGISTER_RETURN_VALUE(expr, tmp_rc, rc) \
{                     \
    tmp_rc = (expr); \
    if (OMPI_SUCCESS != tmp_rc) { \
        rc = tmp_rc;     \
    }                 \
}

/**
 * Function to register MCA params and check for sane values 
 */

int mca_btl_udapl_register_mca_params(void);

    
END_C_DECLS
#endif
