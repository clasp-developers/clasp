/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2009      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef TEST_OPAL_DDT_LIB_H
#define TEST_OPAL_DDT_LIB_H

#include "opal_config.h"

#include "opal/datatype/opal_datatype.h"


#define TIMER_DATA_TYPE struct timeval
#define GET_TIME(TV)   gettimeofday( &(TV), NULL )
#define ELAPSED_TIME(TSTART, TEND)  (((TEND).tv_sec - (TSTART).tv_sec) * 1000000 + ((TEND).tv_usec - (TSTART).tv_usec))

#define DUMP_DATA_AFTER_COMMIT 0x00000001
#define CHECK_PACK_UNPACK      0x00000002

extern uint32_t outputFlags;

extern void cache_trash( void );
extern opal_datatype_t* create_contiguous_type( const opal_datatype_t* type, int length );
extern opal_datatype_t* create_vector_type( const opal_datatype_t* data, int count, int length, int stride );
extern opal_datatype_t* create_strange_dt( void );
extern opal_datatype_t* upper_matrix( unsigned int mat_size );
extern opal_datatype_t* lower_matrix( unsigned int mat_size );
extern opal_datatype_t* test_matrix_borders( unsigned int size, unsigned int width );
extern int init_random_upper_matrix( unsigned int N, double* mat );
extern int check_diag_matrix( unsigned int N, double* mat1, double* mat2 );
extern opal_datatype_t* test_contiguous( void );
extern opal_datatype_t* test_struct_char_double( void );
extern opal_datatype_t* test_create_twice_two_doubles( void );
extern opal_datatype_t* test_struct( void );
extern opal_datatype_t* test_create_blacs_type( void );
extern opal_datatype_t* test_create_blacs_type1( opal_datatype_t* base_type );
extern opal_datatype_t* test_create_blacs_type2( opal_datatype_t* base_type );

extern int mpich_typeub( void );
extern int mpich_typeub2( void );
extern int mpich_typeub3( void );

#endif /* TEST_OPAL_DDT_LIB_H */