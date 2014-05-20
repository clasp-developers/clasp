/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2006 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2009      Sun Microsystems, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/*
 * utility functions for dealing with remote datatype and op structures
 */

#include "ompi_config.h"

#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_convertor_internal.h"
#include "opal/datatype/opal_datatype_prototypes.h"

#include "ompi/op/op.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/datatype/ompi_datatype_internal.h"

#include "osc_base_obj_convert.h"
#include "ompi/memchecker.h"

int
ompi_osc_base_get_primitive_type_info(ompi_datatype_t *datatype,
                                      ompi_datatype_t **prim_datatype, 
                                      uint32_t *prim_count)
{
    ompi_datatype_t *primitive_datatype = NULL;
    size_t datatype_size, primitive_size, primitive_count;

    primitive_datatype = ompi_datatype_get_single_predefined_type_from_args(datatype);
    if( NULL == primitive_datatype ) {
        *prim_count = 0;
        return OMPI_SUCCESS;
    }
    ompi_datatype_type_size( datatype, &datatype_size );
    ompi_datatype_type_size( primitive_datatype, &primitive_size );
    primitive_count = datatype_size / primitive_size;
#if OPAL_ENABLE_DEBUG
    assert( 0 == (datatype_size % primitive_size) );
#endif  /* OPAL_ENABLE_DEBUG */

    /* We now have the count as a size_t, convert it to an uint32_t */
    *prim_datatype = primitive_datatype;
    *prim_count = (uint32_t)primitive_count;

    return OMPI_SUCCESS;
}


struct ompi_osc_base_convertor_t {
    opal_convertor_t convertor;
    ompi_op_t *op;
    ompi_datatype_t *datatype;
};
typedef struct ompi_osc_base_convertor_t ompi_osc_base_convertor_t;
static OBJ_CLASS_INSTANCE(ompi_osc_base_convertor_t, opal_convertor_t, NULL, NULL);

#define COPY_TYPE( TYPENAME, TYPE, COUNT )                              \
static int copy_##TYPENAME( opal_convertor_t *pConvertor, uint32_t count, \
                            char* from, size_t from_len, ptrdiff_t from_extent, \
                            char* to, size_t to_len, ptrdiff_t to_extent, \
                            ptrdiff_t *advance)                         \
{                                                                       \
    size_t remote_TYPE_size = sizeof(TYPE) * (COUNT); /* TODO */        \
    size_t local_TYPE_size = (COUNT) * sizeof(TYPE);                    \
    ompi_osc_base_convertor_t *osc_convertor =                          \
        (ompi_osc_base_convertor_t*) pConvertor;                        \
                                                                        \
    if( (from_extent == (ptrdiff_t)local_TYPE_size) &&                  \
        (to_extent == (ptrdiff_t)remote_TYPE_size) ) {                  \
        ompi_op_reduce(osc_convertor->op, from, to, count, osc_convertor->datatype); \
    } else {                                                            \
        uint32_t i;                                                     \
        for( i = 0; i < count; i++ ) {                                  \
            ompi_op_reduce(osc_convertor->op, from, to, 1, osc_convertor->datatype); \
            to += to_extent;                                            \
            from += from_extent;                                        \
        }                                                               \
    }                                                                   \
    *advance = count * from_extent;                                     \
    return count;                                                       \
}

/* set up copy functions for the basic C MPI data types */
COPY_TYPE( char, char, 1 )
COPY_TYPE( short, short, 1 )
COPY_TYPE( int, int, 1 )
COPY_TYPE( long, long, 1 )
COPY_TYPE( long_long, long long, 1 )
COPY_TYPE( float, float, 1 )
COPY_TYPE( double, double, 1 )
COPY_TYPE( long_double, long double, 1 )
COPY_TYPE( complex_float, ompi_mpi_cxx_cplex, 1 )
COPY_TYPE( complex_double, ompi_mpi_cxx_dblcplex, 1 )
COPY_TYPE( complex_long_double, ompi_mpi_cxx_ldblcplex, 1 )

/* table of predefined copy functions - one for each MPI type */
/* XXX TODO Adapt to new layout */
static conversion_fct_t ompi_osc_base_copy_functions[OMPI_DATATYPE_MAX_PREDEFINED] = {
   (conversion_fct_t)NULL,                      /* DT_LOOP                */
   (conversion_fct_t)NULL,                      /* DT_END_LOOP            */
   (conversion_fct_t)NULL,                      /* DT_LB                  */
   (conversion_fct_t)NULL,                      /* DT_UB                  */
   (conversion_fct_t)copy_char,                 /* DT_CHAR                */
   (conversion_fct_t)copy_char,                 /* DT_CHARACTER           */
   (conversion_fct_t)copy_char,                 /* DT_UNSIGNED_CHAR       */
   (conversion_fct_t)copy_char,                 /* DT_SIGNED_CHAR       */
   (conversion_fct_t)copy_char,                 /* DT_BYTE                */
   (conversion_fct_t)copy_short,                /* DT_SHORT               */
   (conversion_fct_t)copy_short,                /* DT_UNSIGNED_SHORT      */
   (conversion_fct_t)copy_int,                  /* DT_INT                 */
   (conversion_fct_t)copy_int,                  /* DT_UNSIGNED            */
   (conversion_fct_t)copy_long,                 /* DT_LONG                */
   (conversion_fct_t)copy_long,                 /* DT_UNSIGNED_LONG       */
   (conversion_fct_t)copy_long_long,            /* DT_LONG_LONG           */
   (conversion_fct_t)copy_long_long,            /* DT_UNSIGNED_LONG_LONG  */
   (conversion_fct_t)copy_float,                /* DT_FLOAT               */
   (conversion_fct_t)copy_double,               /* DT_DOUBLE              */
   (conversion_fct_t)copy_long_double,          /* DT_LONG_DOUBLE         */
   (conversion_fct_t)NULL,                      /* DT_PACKED              */
   (conversion_fct_t)NULL,                      /* DT_WCHAR               */
#if SIZEOF_BOOL == SIZEOF_CHAR
   (conversion_fct_t)copy_char,                 /* DT_CXX_BOOL            */
#elif SIZEOF_BOOL == SIZEOF_SHORT
   (conversion_fct_t)copy_short,                /* DT_CXX_BOOL            */
#elif SIZEOF_BOOL == SIZEOF_INT
   (conversion_fct_t)copy_int,                  /* DT_CXX_BOOL            */
#elif SIZEOF_BOOL == SIZEOF_LONG
   (conversion_fct_t)copy_long,                 /* DT_CXX_BOOL            */
#else
   (conversion_fct_t)NULL,                      /* DT_CXX_BOOL            */
#endif
#if OMPI_SIZEOF_FORTRAN_LOGICAL == SIZEOF_CHAR
   (conversion_fct_t)copy_char,                 /* DT_LOGIC               */
#elif OMPI_SIZEOF_FORTRAN_LOGICAL == SIZEOF_SHORT
   (conversion_fct_t)copy_short,                /* DT_LOGIC               */
#elif OMPI_SIZEOF_FORTRAN_LOGICAL == SIZEOF_INT
   (conversion_fct_t)copy_int,                  /* DT_LOGIC               */
#elif OMPI_SIZEOF_FORTRAN_LOGICAL == SIZEOF_LONG
   (conversion_fct_t)copy_long,                 /* DT_LOGIC               */
#else
   (conversion_fct_t)NULL,                      /* DT_LOGIC               */
#endif
   (conversion_fct_t)copy_int,                  /* DT_INTEGER             */
   (conversion_fct_t)copy_float,                /* DT_REAL                */
   (conversion_fct_t)copy_double,               /* DT_DBLPREC             */
   (conversion_fct_t)copy_complex_float,        /* DT_COMPLEX_FLOAT       */
   (conversion_fct_t)copy_complex_double,       /* DT_COMPLEX_DOUBLE      */
   (conversion_fct_t)copy_complex_long_double,  /* DT_COMPLEX_LONG_DOUBLE */
   (conversion_fct_t)NULL,                      /* DT_2INT                */
   (conversion_fct_t)NULL,                      /* DT_2INTEGER            */
   (conversion_fct_t)NULL,                      /* DT_2REAL               */
   (conversion_fct_t)NULL,                      /* DT_2DBLPREC            */
   (conversion_fct_t)NULL,                      /* DT_2COMPLEX            */
   (conversion_fct_t)NULL,                      /* DT_2DOUBLE_COMPLEX     */
   (conversion_fct_t)NULL,                      /* DT_FLOAT_INT           */
   (conversion_fct_t)NULL,                      /* DT_DOUBLE_INT          */
   (conversion_fct_t)NULL,                      /* DT_LONG_DOUBLE_INT     */
   (conversion_fct_t)NULL,                      /* DT_LONG_INT            */
   (conversion_fct_t)NULL,                      /* DT_SHORT_INT           */
   (conversion_fct_t)NULL,                      /* DT_UNAVAILABLE         */
};

int
ompi_osc_base_process_op(void *outbuf,
                         void *inbuf,
                         size_t inbuflen,
                         struct ompi_datatype_t *datatype,
                         int count,
                         ompi_op_t *op)
{
    if (op == &ompi_mpi_op_replace.op) {
        return OMPI_ERR_NOT_SUPPORTED;
    }

    if (ompi_datatype_is_predefined(datatype)) {
        ompi_op_reduce(op, inbuf, outbuf, count, datatype);
    } else {
        struct ompi_datatype_t *primitive_datatype = NULL;
        ompi_osc_base_convertor_t convertor;
        struct iovec iov;
        uint32_t iov_count = 1;
        size_t max_data;
        struct opal_convertor_master_t master = {NULL, 0, 0, 0, {0, }, NULL};

        primitive_datatype = ompi_datatype_get_single_predefined_type_from_args(datatype);

        /* create convertor */
        OBJ_CONSTRUCT(&convertor, ompi_osc_base_convertor_t);
        convertor.op = op;
        convertor.datatype = primitive_datatype;

        /* initialize convertor */
        opal_convertor_copy_and_prepare_for_recv(ompi_proc_local()->proc_convertor,
                                                 &(datatype->super),
                                                 count,
                                                 outbuf,
                                                 0,
                                                 &convertor.convertor);

        memcpy(&master, convertor.convertor.master, sizeof(struct opal_convertor_master_t));
        master.next = convertor.convertor.master;
        master.pFunctions = (conversion_fct_t*) &ompi_osc_base_copy_functions;
        convertor.convertor.master = &master;
        convertor.convertor.fAdvance = opal_unpack_general;

        iov.iov_len  = inbuflen;
        iov.iov_base = (IOVBASE_TYPE*) inbuf;
        max_data     = iov.iov_len;
        MEMCHECKER(
            memchecker_convertor_call(&opal_memchecker_base_mem_defined,
                                      &convertor.convertor);
        );
        opal_convertor_unpack(&convertor.convertor, 
                              &iov,
                              &iov_count,
                              &max_data);
        MEMCHECKER(
            memchecker_convertor_call(&opal_memchecker_base_mem_noaccess,
                                      &convertor.convertor);
        );
        OBJ_DESTRUCT(&convertor);
    }

    return OMPI_SUCCESS;
}
