/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sun Microsystems Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ddt_lib.h"
#include "opal/datatype/opal_convertor.h"
#include <time.h>
#include <stdlib.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <stdio.h>

/* Compile with:
mpicc -DHAVE_CONFIG_H -I. -I../../include -I../../../ompi-trunk/include  -I../.. -I../../include -I../../../ompi-trunk/opal -I../../../ompi-trunk/orte -I../../../ompi-trunk/ompi -g ddt_test.c -o ddt_test
*/

#define TIMER_DATA_TYPE struct timeval
#define GET_TIME(TV)   gettimeofday( &(TV), NULL )
#define ELAPSED_TIME(TSTART, TEND)  (((TEND).tv_sec - (TSTART).tv_sec) * 1000000 + ((TEND).tv_usec - (TSTART).tv_usec))

#define DUMP_DATA_AFTER_COMMIT 0x00000001
#define CHECK_PACK_UNPACK      0x00000002

uint32_t remote_arch;

static int test_upper( unsigned int length )
{
    ompi_datatype_t *pdt;
    opal_convertor_t * pConv;
    int rc;
    unsigned int i, iov_count, split_chunk, total_length;
    size_t max_data;
    struct iovec iov[5];
    TIMER_DATA_TYPE start, end;
    long total_time;

    printf( "test upper matrix\n" );
    pdt = upper_matrix( length );
    /*dt_dump( pdt );*/

    total_length = length * (length + 1) * ( sizeof(double) / 2);

    pConv = opal_convertor_create( remote_arch, 0 );
    if( OMPI_SUCCESS != opal_convertor_prepare_for_send( pConv, &(pdt->super), 1, NULL ) ) {
        printf( "Cannot attach the datatype to a convertor\n" );
        return OMPI_ERROR;
    }

    GET_TIME( start );
    split_chunk = (length + 1) * sizeof(double);
    /*    split_chunk = (total_length + 1) * sizeof(double); */
    for( i = total_length; i > 0; ) {
        iov_count = 5;
        max_data = 0;
        opal_convertor_raw( pConv, iov, &iov_count, &max_data );
	i -= max_data;
    }
    GET_TIME( end );
    total_time = ELAPSED_TIME( start, end );
    printf( "complete raw in %ld microsec\n", total_time );

    /* test the automatic destruction pf the data */
    ompi_datatype_destroy( &pdt ); assert( pdt == NULL );

    OBJ_RELEASE( pConv );
    return rc;
}

/**
 *  Conversion function. They deal with data-types in 3 ways, always making local copies.
 * In order to allow performance testings, there are 3 functions:
 *  - one copying directly from one memory location to another one using the
 *    data-type copy function.
 *  - one which use a 2 convertors created with the same data-type
 *  - and one using 2 convertors created from different data-types.
 *
 */
static int local_copy_ddt_raw( ompi_datatype_t* pdt, int count, int iov_num )
{
    struct iovec* iov;
    opal_convertor_t* convertor;
    TIMER_DATA_TYPE start, end;
    long total_time;
    int i;
    uint32_t iov_count = iov_num;
    size_t max_data = 0, remaining_length;

    iov = (struct iovec*)malloc(iov_num * sizeof(struct iovec));

    convertor = opal_convertor_create( remote_arch, 0 );
    if( OMPI_SUCCESS != opal_convertor_prepare_for_send( convertor, &(pdt->super), count, NULL ) ) {
        printf( "Cannot attach the datatype to a convertor\n" );
        return OMPI_ERROR;
    }

    remaining_length = count * pdt->super.size;
    GET_TIME( start );
    while( 0 == opal_convertor_raw(convertor, iov, &iov_count, &max_data) ) {
#if 0
	printf( "New raw extraction (iov_count = %d, max_data = %zu)\n",
		iov_count, max_data );
	for( i = 0; i < iov_count; i++ ) {
	    printf( "\t{%p, %d}\n", iov[i].iov_base, iov[i].iov_len );
	}
#endif
	remaining_length -= max_data;
        iov_count = iov_num;
    }
    remaining_length -= max_data;
    GET_TIME( end );
    total_time = ELAPSED_TIME( start, end );
    printf( "raw extraction in %ld microsec\n", total_time );
    OBJ_RELEASE( convertor );
    if( remaining_length != 0 ) {
	printf( "Not all raw description was been extracted (%lu bytes missing)\n",
		(unsigned long) remaining_length );
    }
    return OMPI_SUCCESS;
}

/**
 * Main function. Call several tests and print-out the results. It try to stress the convertor
 * using difficult data-type constructions as well as strange segment sizes for the conversion.
 * Usually, it is able to detect most of the data-type and convertor problems. Any modifications
 * on the data-type engine should first pass all the tests from this file, before going into other
 * tests.
 */
int main( int argc, char* argv[] )
{
    ompi_datatype_t *pdt, *pdt1, *pdt2, *pdt3;
    int rc, length = 500, iov_num = 5;

    ompi_datatype_init();

    /**
     * By default simulate homogeneous architectures.
     */
    remote_arch = opal_local_arch;
    printf( "\n\n#\n * TEST INVERSED VECTOR\n #\n\n" );
    pdt = create_inversed_vector( &ompi_mpi_int.dt, 10 );
    if( outputFlags & CHECK_PACK_UNPACK ) {
        local_copy_ddt_raw(pdt, 100, iov_num);
    }
    OBJ_RELEASE( pdt ); assert( pdt == NULL );
    printf( "\n\n#\n * TEST STRANGE DATATYPE\n #\n\n" );
    pdt = create_strange_dt();
    if( outputFlags & CHECK_PACK_UNPACK ) {
        local_copy_ddt_raw(pdt, 1, iov_num);
    }
    OBJ_RELEASE( pdt ); assert( pdt == NULL );
    
    printf( "\n\n#\n * TEST UPPER TRIANGULAR MATRIX (size 100)\n #\n\n" );
    pdt = upper_matrix(100);
    if( outputFlags & CHECK_PACK_UNPACK ) {
        local_copy_ddt_raw(pdt, 1, iov_num);
    }
    OBJ_RELEASE( pdt ); assert( pdt == NULL );
    
    mpich_typeub();
    mpich_typeub2();
    mpich_typeub3();
    
    printf( "\n\n#\n * TEST UPPER MATRIX\n #\n\n" );
    rc = test_upper( length );
    if( rc == 0 )
        printf( "decode [PASSED]\n" );
    else
        printf( "decode [NOT PASSED]\n" );
    
    printf( "\n\n#\n * TEST MATRIX BORDERS\n #\n\n" );
    pdt = test_matrix_borders( length, 100 );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt );
    }
    OBJ_RELEASE( pdt ); assert( pdt == NULL );
    
    printf( "\n\n#\n * TEST CONTIGUOUS\n #\n\n" );
    pdt = test_contiguous();
    OBJ_RELEASE( pdt ); assert( pdt == NULL );
    printf( "\n\n#\n * TEST STRUCT\n #\n\n" );
    pdt = test_struct();
    OBJ_RELEASE( pdt ); assert( pdt == NULL );
    
    ompi_datatype_create_contiguous(0, &ompi_mpi_datatype_null.dt, &pdt1);
    ompi_datatype_create_contiguous(0, &ompi_mpi_datatype_null.dt, &pdt2);
    ompi_datatype_create_contiguous(0, &ompi_mpi_datatype_null.dt, &pdt3);

    ompi_datatype_add( pdt3, &ompi_mpi_int.dt, 10, 0, -1 );
    ompi_datatype_add( pdt3, &ompi_mpi_float.dt, 5, 10 * sizeof(int), -1 );
    
    ompi_datatype_add( pdt2, &ompi_mpi_float.dt, 1, 0, -1 );
    ompi_datatype_add( pdt2, pdt3, 3, sizeof(int) * 1, -1 );
    
    ompi_datatype_add( pdt1, &ompi_mpi_long_long_int.dt, 5, 0, -1 );
    ompi_datatype_add( pdt1, &ompi_mpi_long_double.dt, 2, sizeof(long long) * 5, -1 );
    
    printf( ">>--------------------------------------------<<\n" );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt1 );
    }
    printf( ">>--------------------------------------------<<\n" );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt2 );
    }
    printf( ">>--------------------------------------------<<\n" );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt3 );
    }
    
    OBJ_RELEASE( pdt1 ); assert( pdt1 == NULL );
    OBJ_RELEASE( pdt2 ); assert( pdt2 == NULL );
    OBJ_RELEASE( pdt3 ); assert( pdt3 == NULL );

    printf( ">>--------------------------------------------<<\n" );
    printf( " Contiguous data-type (MPI_DOUBLE)\n" );
    pdt = MPI_DOUBLE;
    if( outputFlags & CHECK_PACK_UNPACK ) {
        local_copy_ddt_raw(pdt, 4500, iov_num);
    }
    printf( ">>--------------------------------------------<<\n" );
    
    printf( ">>--------------------------------------------<<\n" );
    if( outputFlags & CHECK_PACK_UNPACK ) {
        printf( "Contiguous multiple data-type (4500*1)\n" );
        pdt = create_contiguous_type( MPI_DOUBLE, 4500 );
        local_copy_ddt_raw(pdt, 1, iov_num);
        OBJ_RELEASE( pdt ); assert( pdt == NULL );
        printf( "Contiguous multiple data-type (450*10)\n" );
        pdt = create_contiguous_type( MPI_DOUBLE, 450 );
        local_copy_ddt_raw(pdt, 10, iov_num);
        OBJ_RELEASE( pdt ); assert( pdt == NULL );
        printf( "Contiguous multiple data-type (45*100)\n" );
        pdt = create_contiguous_type( MPI_DOUBLE, 45 );
        local_copy_ddt_raw(pdt, 100, iov_num);
        OBJ_RELEASE( pdt ); assert( pdt == NULL );
        printf( "Contiguous multiple data-type (100*45)\n" );
        pdt = create_contiguous_type( MPI_DOUBLE, 100 );
        local_copy_ddt_raw(pdt, 45, iov_num);
        OBJ_RELEASE( pdt ); assert( pdt == NULL );
        printf( "Contiguous multiple data-type (10*450)\n" );
        pdt = create_contiguous_type( MPI_DOUBLE, 10 );
        local_copy_ddt_raw(pdt, 450, iov_num);
        OBJ_RELEASE( pdt ); assert( pdt == NULL );
        printf( "Contiguous multiple data-type (1*4500)\n" );
        pdt = create_contiguous_type( MPI_DOUBLE, 1 );
        local_copy_ddt_raw(pdt, 4500, iov_num);
        OBJ_RELEASE( pdt ); assert( pdt == NULL );
    }
    printf( ">>--------------------------------------------<<\n" );
    printf( ">>--------------------------------------------<<\n" );
    printf( "Vector data-type (450 times 10 double stride 11)\n" );
    pdt = create_vector_type( MPI_DOUBLE, 450, 10, 11 );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
	ompi_datatype_dump( pdt );
    }
    if( outputFlags & CHECK_PACK_UNPACK ) {
        local_copy_ddt_raw(pdt, 1, iov_num);
    }
    printf( ">>--------------------------------------------<<\n" );
    OBJ_RELEASE( pdt ); assert( pdt == NULL );
    
    printf( ">>--------------------------------------------<<\n" );
    pdt = test_struct_char_double();
    if( outputFlags & CHECK_PACK_UNPACK ) {
        local_copy_ddt_raw(pdt, 4500, iov_num);
    }
    printf( ">>--------------------------------------------<<\n" );
    OBJ_RELEASE( pdt ); assert( pdt == NULL );
    
    printf( ">>--------------------------------------------<<\n" );
    pdt = test_create_twice_two_doubles();
    if( outputFlags & CHECK_PACK_UNPACK ) {
        local_copy_ddt_raw(pdt, 4500, iov_num);
    }
    printf( ">>--------------------------------------------<<\n" );
    OBJ_RELEASE( pdt ); assert( pdt == NULL );

    printf( ">>--------------------------------------------<<\n" );
    pdt = test_create_blacs_type();
    if( outputFlags & CHECK_PACK_UNPACK ) {
	if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
	    ompi_datatype_dump( pdt );
	}
        local_copy_ddt_raw(pdt, 4500, iov_num);
    }
    printf( ">>--------------------------------------------<<\n" );
    OBJ_RELEASE( pdt ); assert( pdt == NULL );

    printf( ">>--------------------------------------------<<\n" );
    pdt1 = test_create_blacs_type1( &ompi_mpi_int.dt );
    if( outputFlags & CHECK_PACK_UNPACK ) {
        local_copy_ddt_raw( pdt1, 1, iov_num );
    }
    printf( ">>--------------------------------------------<<\n" );
    OBJ_RELEASE( pdt1 ); assert( pdt1 == NULL );

    /* clean-ups all data allocations */
    ompi_datatype_finalize();

    return OMPI_SUCCESS;
}
