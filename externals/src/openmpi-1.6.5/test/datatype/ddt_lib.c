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
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ddt_lib.h"
#include "ompi/constants.h"
#include <time.h>
#include <stdlib.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <stdio.h>
#include <string.h>

#define TIMER_DATA_TYPE struct timeval
#define GET_TIME(TV)   gettimeofday( &(TV), NULL )
#define ELAPSED_TIME(TSTART, TEND)  (((TEND).tv_sec - (TSTART).tv_sec) * 1000000 + ((TEND).tv_usec - (TSTART).tv_usec))

#define DUMP_DATA_AFTER_COMMIT 0x00000001
#define CHECK_PACK_UNPACK      0x00000002

uint32_t outputFlags = CHECK_PACK_UNPACK;

/**
 * Cache cleanup.
 */
#define CACHE_SIZE (4*1024*1024)
void cache_trash( void )
{
    char* buffer;

    buffer = (char*)malloc( sizeof(char) * CACHE_SIZE );
    memset( buffer, 1, CACHE_SIZE );
    memset( buffer, 0xff, CACHE_SIZE );
    free( buffer );
}

/**
 * Data-type functions.
 */
ompi_datatype_t* create_inversed_vector( ompi_datatype_t* type, int length )
{
   ompi_datatype_t* type1;

   ompi_datatype_create_vector( length, 1, 2, type, &type1 );

   ompi_datatype_commit( &type1 );
   return type1;
}

int mpich_typeub( void )
{
   int errs = 0;
   MPI_Aint extent, lb, extent1, extent2, extent3;
   MPI_Aint displ[2];
   int blens[2];
   ompi_datatype_t *type1, *type2, *type3, *types[2];

   ompi_datatype_create_vector( 2, 1, 4, &ompi_mpi_int.dt, &type1 );
   ompi_datatype_commit( &type1 );
   ompi_datatype_get_extent( type1, &lb, &extent );
   extent1 = 5 * sizeof(int);
   if (extent != extent1) {
      printf("EXTENT 1 %ld != %ld\n", (long)extent, (long)extent1);
      errs++;
      printf("extent(type1)=%ld\n",(long)extent);
   }

   blens[0] = 1;
   blens[1] = 1;
   displ[0] = 0;
   displ[1] = sizeof(int)*4;
   types[0] = type1;
   types[1] = &ompi_mpi_ub.dt;  /*ompi_datatype_basicDatatypes[DT_UB];*/
   extent2  = displ[1];

   /*    using MPI_UB and Type_struct, monkey with the extent, making it 16
    */
   ompi_datatype_create_struct( 2, blens, displ, types, &type2 );
   ompi_datatype_commit( &type2 );
   ompi_datatype_get_extent( type2, &lb, &extent );
   if (extent != extent2) {
      printf("EXTENT 2 %ld != %ld\n", (long)extent, (long)extent2);
      errs++;
      printf("extent(type2)=%ld\n",(long)extent);
   }

   /*    monkey with the extent again, making it 4
    *     ===> MPICH gives 4
    *     ===> MPIF gives 16, the old extent
    */
   displ[1] = sizeof(int);
   types[0] = type2;
   types[1] = &ompi_mpi_ub.dt;  /*ompi_datatype_basicDatatypes[DT_UB];*/
   extent3  = extent2;

   ompi_datatype_create_struct( 2, blens, displ, types, &type3 );
   ompi_datatype_commit( &type3 );

   ompi_datatype_get_extent( type3, &lb, &extent );
   if (extent != extent3) {
      printf("EXTENT 3 %ld != %ld\n", (long)extent, (long)extent3);
      errs++;
      printf("extent(type3)=%ld\n",(long)extent);
   }

   OBJ_RELEASE( type1 ); /*assert( type1 == NULL );*/
   OBJ_RELEASE( type2 ); /*assert( type2 == NULL );*/
   OBJ_RELEASE( type3 ); assert( type3 == NULL );
   return errs;
}

int mpich_typeub2( void )
{
   int blocklen[3], err = 0;
   size_t sz1, sz2, sz3;
   MPI_Aint disp[3], lb, ub, ex1, ex2, ex3;
   ompi_datatype_t *types[3], *dt1, *dt2, *dt3;

   blocklen[0] = 1;
   blocklen[1] = 1;
   blocklen[2] = 1;
   disp[0] = -3;
   disp[1] = 0;
   disp[2] = 6;
   types[0] = &ompi_mpi_lb.dt;  /* ompi_datatype_basicDatatypes[DT_LB]; */
   types[1] = &ompi_mpi_int.dt;  /* ompi_datatype_basicDatatypes[DT_INT]; */
   types[2] = &ompi_mpi_ub.dt;  /* ompi_datatype_basicDatatypes[DT_UB]; */

   ompi_datatype_create_struct(3,blocklen,disp, types,&dt1);
   ompi_datatype_commit(&dt1);

   ompi_datatype_type_lb(dt1, &lb);          ompi_datatype_type_ub(dt1, &ub);
   ompi_datatype_type_extent(dt1,&ex1);      ompi_datatype_type_size(dt1,&sz1);

   /* Values should be lb = -3, ub = 6 extent 9; size depends on implementation */
   if (lb != -3 || ub != 6 || ex1 != 9) {
      printf("Example 3.26 type1 lb %d ub %d extent %d size %d\n", (int)lb, (int)ub, (int)ex1, (int)sz1);
      err++;
   }
   else
      printf("Example 3.26 type1 correct\n" );

   ompi_datatype_create_contiguous(2,dt1,&dt2);
   ompi_datatype_type_lb(dt2, &lb);          ompi_datatype_type_ub(dt2, &ub);
   ompi_datatype_type_extent(dt2,&ex2);      ompi_datatype_type_size(dt2,&sz2);
   /* Values should be lb = -3, ub = 15, extent = 18, size depends on implementation */
   if (lb != -3 || ub != 15 || ex2 != 18) {
      printf("Example 3.26 type2 lb %d ub %d extent %d size %d\n", (int)-3, (int)15, (int)18, 8);
      printf("Example 3.26 type2 lb %d ub %d extent %d size %d\n", (int)lb, (int)ub, (int)ex2, (int)sz2);
      err++;
   }
   else
      printf("Example 3.26 type1 correct\n" );
   OBJ_RELEASE( dt2 ); assert( dt2 == NULL );
   ompi_datatype_create_contiguous(2,dt1,&dt2);
   ompi_datatype_type_lb(dt2, &lb);          ompi_datatype_type_ub(dt2, &ub);
   ompi_datatype_type_extent(dt2,&ex2);      ompi_datatype_type_size(dt2,&sz2);
   /* Values should be lb = -3, ub = 15, extent = 18, size depends on implementation */
   if (lb != -3 || ub != 15 || ex2 != 18) {
      printf("Example 3.26 type2 lb %d ub %d extent %d size %d\n", (int)-3, (int)15, (int)18, 8);
      printf("Example 3.26 type2 lb %d ub %d extent %d size %d\n", (int)lb, (int)ub, (int)ex2, (int)sz2);
      err++;
   }
   else
      printf( "Example 3.26 type2 correct\n" );

   types[0]=dt1;               types[1]=dt1;
   blocklen[0]=1;              blocklen[1]=1;
   disp[0]=0;                  disp[1]=ex1;

   ompi_datatype_create_struct(2, blocklen, disp, types, &dt3);
   ompi_datatype_commit(&dt3);

   ompi_datatype_type_lb(dt3, &lb);          ompi_datatype_type_ub(dt3, &ub);
   ompi_datatype_type_extent(dt3,&ex3);      ompi_datatype_type_size(dt3,&sz3);
   /* Another way to express type2 */
   if (lb != -3 || ub != 15 || ex3 != 18) {
      printf("type3 lb %d ub %d extent %d size %d\n", (int)-3, (int)15, (int)18, 8);
      printf("type3 lb %d ub %d extent %d size %d\n", (int)lb, (int)ub, (int)ex3, (int)sz2);
      err++;
   }
   else
      printf( "type3 correct\n" );

   OBJ_RELEASE( dt1 ); /*assert( dt1 == NULL );*/
   OBJ_RELEASE( dt2 ); /*assert( dt2 == NULL );*/
   OBJ_RELEASE( dt3 ); assert( dt3 == NULL );
   return err;
}

int mpich_typeub3( void )
{
   int blocklen[2], err = 0, idisp[3];
   size_t sz;
   MPI_Aint disp[3], lb, ub, ex;
   ompi_datatype_t *types[3], *dt1, *dt2, *dt3, *dt4, *dt5;

   /* Create a datatype with explicit LB and UB */
   blocklen[0] = 1;
   blocklen[1] = 1;
   blocklen[2] = 1;
   disp[0] = -3;
   disp[1] = 0; 
   disp[2] = 6;
   types[0] = &ompi_mpi_lb.dt;  /* ompi_datatype_basicDatatypes[DT_LB]; */
   types[1] = &ompi_mpi_int.dt;  /* ompi_datatype_basicDatatypes[DT_INT]; */
   types[2] = &ompi_mpi_ub.dt;  /* ompi_datatype_basicDatatypes[DT_UB]; */
   
   /* Generate samples for contiguous, hindexed, hvector, indexed, and vector (struct and contiguous tested in typeub2) */                                                                                                                         
   ompi_datatype_create_struct(3,blocklen,disp, types,&dt1);
   ompi_datatype_commit(&dt1);

   /* This type is the same as in typeub2, and is tested there */
   types[0]=dt1;               types[1]=dt1;
   blocklen[0]=1;              blocklen[1]=1;
   disp[0]=-4;                 disp[1]=7;
   idisp[0]=-4;                idisp[1]=7;

   ompi_datatype_create_hindexed( 2, blocklen, disp, dt1, &dt2 );
   ompi_datatype_commit( &dt2 );

   ompi_datatype_type_lb( dt2, &lb );       ompi_datatype_type_ub( dt2, &ub );
   ompi_datatype_type_extent( dt2, &ex );   ompi_datatype_type_size( dt2, &sz );

   if (lb != -7 || ub != 13 || ex != 20) {
      printf("hindexed lb %d ub %d extent %d size %d\n", (int)-7, (int)13, (int)20, (int)sz);
      printf("hindexed lb %d ub %d extent %d size %d\n", (int)lb, (int)ub, (int)ex, (int)sz);
      err++;
   }
   else
      printf( "hindexed ok\n" );

   ompi_datatype_create_indexed( 2, blocklen, idisp, dt1, &dt3 );
   ompi_datatype_commit( &dt3 );

   ompi_datatype_type_lb( dt3, &lb );       ompi_datatype_type_ub( dt3, &ub );
   ompi_datatype_type_extent( dt3, &ex );   ompi_datatype_type_size( dt3, &sz );

   if (lb != -39 || ub != 69 || ex != 108) {
      printf("indexed lb %d ub %d extent %d size %d\n", (int)-39, (int)69, (int)108, (int)sz);
      printf("indexed lb %d ub %d extent %d size %d\n", (int)lb, (int)ub, (int)ex, (int)sz);
      err++;
   }
   else
      printf( "indexed ok\n" );

   ompi_datatype_create_hvector( 2, 1, 14, dt1, &dt4 );
   ompi_datatype_commit( &dt4 );

   ompi_datatype_type_lb( dt4, &lb );       ompi_datatype_type_ub( dt4, &ub );
   ompi_datatype_type_extent( dt4, &ex );   ompi_datatype_type_size( dt4, &sz );

   if (lb != -3 || ub != 20 || ex != 23) {
      printf("hvector lb %d ub %d extent %d size %d\n", (int)-3, (int)20, (int)23, (int)sz);
      printf("hvector lb %d ub %d extent %d size %d\n", (int)lb, (int)ub, (int)ex, (int)sz);
      err++;
   }
   else
      printf( "hvector ok\n" );

   ompi_datatype_create_vector( 2, 1, 14, dt1, &dt5 );
   ompi_datatype_commit( &dt5 );

   ompi_datatype_type_lb( dt5, &lb );       ompi_datatype_type_ub( dt5, &ub );
   ompi_datatype_type_extent( dt5, &ex );   ompi_datatype_type_size( dt5, &sz );

   if (lb != -3 || ub != 132 || ex != 135) {
      printf("vector lb %d ub %d extent %d size %d\n", (int)-3, (int)132, (int)135, (int)sz);
      printf("vector lb %d ub %d extent %d size %d\n", (int)lb, (int)ub, (int)ex, (int)sz);
      err++;
   }
   else
      printf( "vector ok\n" );

   OBJ_RELEASE( dt1 ); /*assert( dt1 == NULL );*/
   OBJ_RELEASE( dt2 ); /*assert( dt2 == NULL );*/
   OBJ_RELEASE( dt3 ); /*assert( dt3 == NULL );*/
   OBJ_RELEASE( dt4 ); /*assert( dt4 == NULL );*/
   OBJ_RELEASE( dt5 ); assert( dt5 == NULL );
   return err;
}

void print_double_mat( unsigned int N, double* mat )
{
   unsigned int i, j;
   double* pMat;

   for( i = 0; i < N; i++ ) {
      printf( "(%4d) :", i * N * (int)sizeof(double) );
      pMat = mat + i * N;
      for( j = 0; j < N; j++ ) {
         printf( "%5.1f ", *pMat );
         pMat++;
      }
      printf( "\n" );
   }
}

int init_random_upper_matrix( unsigned int N, double* mat )
{
    unsigned int i, j;

    srand( time(NULL) );
    for( i = 0; i < N; i++ ) {
        mat += i;
        for( j = i; j < N; j++ ) {
            *mat = (double)random();
            mat++;
        }
    }
    return OMPI_SUCCESS;  
}

int check_diag_matrix( unsigned int N, double* mat1, double* mat2 )
{
   unsigned int i, j;

   for( i = 0; i < N; i++ ) {
      mat1 += i;
      mat2 += i;
      for( j = i; j < N; j++ ) {
         if( *mat1 != *mat2 ) {
            printf( "error in position (%d, %d) expect %f and find %f\n",
                    i, j, *mat1, *mat2 );
            printf( "hex %lx != %lx\n", *(long*)mat1, *(long*)mat2 );
            return OMPI_ERROR;
         }
         mat1++; mat2++;
      }
   }
   return OMPI_SUCCESS;
}

ompi_datatype_t* upper_matrix( unsigned int mat_size )
{
    int *disp, *blocklen;
    unsigned int i;
    ompi_datatype_t* upper;

    disp = (int*)malloc( sizeof(int) * mat_size );
    blocklen = (int*)malloc( sizeof(int) * mat_size );

    for( i = 0; i < mat_size; i++ ) {
        disp[i] = i * mat_size + i;
        blocklen[i] = mat_size - i;
    }

    ompi_datatype_create_indexed( mat_size, blocklen, disp, &ompi_mpi_double.dt,
                             &upper );
    ompi_datatype_commit( &upper );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( upper );
    }
    free( disp );
    free( blocklen );
    return upper;
}

ompi_datatype_t* lower_matrix( unsigned int mat_size )
{
    int *disp, *blocklen;
    unsigned int i;
    ompi_datatype_t* upper;

    disp = (int*)malloc( sizeof(int) * mat_size );
    blocklen = (int*)malloc( sizeof(int) * mat_size );

    for( i = 0; i < mat_size; i++ ) {
        disp[i] = i * mat_size;
        blocklen[i] = i;
    }

    ompi_datatype_create_indexed( mat_size, blocklen, disp, &ompi_mpi_double.dt,
                             &upper );
    free( disp );
    free( blocklen );
    return upper;
}

ompi_datatype_t* test_matrix_borders( unsigned int size, unsigned int width )
{
   ompi_datatype_t *pdt, *pdt_line;
   int disp[2];
   int blocklen[2];
   
   disp[0] = 0;
   blocklen[0] = width;
   disp[1] = (size - width) * sizeof(double);
   blocklen[1] = width;

   ompi_datatype_create_indexed( 2, blocklen, disp, &ompi_mpi_double.dt,
                            &pdt_line );
   ompi_datatype_create_contiguous( size, pdt_line, &pdt );
   OBJ_RELEASE( pdt_line ); /*assert( pdt_line == NULL );*/
   return pdt;
}

ompi_datatype_t* test_contiguous( void )
{
    ompi_datatype_t *pdt, *pdt1, *pdt2;

    printf( "test contiguous (alignement)\n" );
    ompi_datatype_create_contiguous(0, &ompi_mpi_datatype_null.dt, &pdt1);
    ompi_datatype_add( pdt1, &ompi_mpi_double.dt, 1, 0, -1 );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt1 );
    }
    ompi_datatype_add( pdt1, &ompi_mpi_char.dt, 1, 8, -1 );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt1 );
    }
    ompi_datatype_create_contiguous( 4, pdt1, &pdt2 );
    OBJ_RELEASE( pdt1 ); /*assert( pdt1 == NULL );*/
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt2 );
    }
    ompi_datatype_create_contiguous( 2, pdt2, &pdt );
    OBJ_RELEASE( pdt2 ); /*assert( pdt2 == NULL );*/
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt );
    }
    return pdt;
}

typedef struct __struct_char_double {
    char c;
    double d;
} char_double_t;

ompi_datatype_t* test_struct_char_double( void )
{
    char_double_t data;
    int lengths[] = {1, 1};
    MPI_Aint displ[] = {0, 0};
    ompi_datatype_t *pdt;
    ompi_datatype_t* types[] = { &ompi_mpi_char.dt, &ompi_mpi_double.dt};

    displ[0] = (char*)&(data.c) - (char*)&(data);
    displ[1] = (char*)&(data.d) - (char*)&(data);

    ompi_datatype_create_struct( 2, lengths, displ, types, &pdt );
    ompi_datatype_commit( &pdt );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt );
    }
    return pdt;
}

ompi_datatype_t* test_create_twice_two_doubles( void )
{
    ompi_datatype_t* pdt;

    ompi_datatype_create_vector( 2, 2, 5, &ompi_mpi_double.dt, &pdt );
    ompi_datatype_commit( &pdt );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt );
    }
    return pdt;
}

/*
  Datatype 0x832cf28 size 0 align 1 id 0 length 4 used 0
  true_lb 0 true_ub 0 (true_extent 0) lb 0 ub 0 (extent 0)
  nbElems 0 loops 0 flags 6 (commited contiguous )-cC--------[---][---]
  contain 13 disp 0x420 (1056) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 13 disp 0x478 (1144) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 13 disp 0x4d0 (1232) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 13 disp 0x528 (1320) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 13 disp 0x580 (1408) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 13 disp 0x5d8 (1496) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 13 disp 0x630 (1584) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 12 disp 0x68c (1676) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 11 disp 0x6e8 (1768) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 10 disp 0x744 (1860) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 9 disp 0x7a0 (1952) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 8 disp 0x7fc (2044) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 7 disp 0x858 (2136) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 6 disp 0x8b4 (2228) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 5 disp 0x910 (2320) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 4 disp 0x96c (2412) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 3 disp 0x9c8 (2504) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 2 disp 0xa24 (2596) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 1 disp 0xa80 (2688) extent 4
*/
static int blacs_length[] = { 13, 13, 13, 13, 13, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1 };
static int blacs_indices[] = { 1144/4, 1232/4, 1320/4, 1408/4, 1496/4, 1584/4, 1676/4, 1768/4,
                               1860/4, 1952/4, 2044/4, 2136/4, 2228/4, 2320/4, 2412/4, 2504/4,
                               2596/4, 2688/4 };
ompi_datatype_t* test_create_blacs_type( void )
{
    ompi_datatype_t *pdt;

    ompi_datatype_create_indexed( 18, blacs_length, blacs_indices, &ompi_mpi_int.dt, &pdt );
    ompi_datatype_commit( &pdt );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt );
    }
    return pdt;
}

ompi_datatype_t* test_create_blacs_type1( ompi_datatype_t* base_type )
{
    ompi_datatype_t *pdt;

    ompi_datatype_create_vector( 7, 1, 3, base_type, &pdt );
    ompi_datatype_commit( &pdt );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt );
    }
    return pdt;
}

ompi_datatype_t* test_create_blacs_type2( ompi_datatype_t* base_type )
{
    ompi_datatype_t *pdt;

    ompi_datatype_create_vector( 7, 1, 2, base_type, &pdt );
    ompi_datatype_commit( &pdt );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt );
    }
    return pdt;
}

ompi_datatype_t* test_struct( void )
{
    ompi_datatype_t* types[] = { &ompi_mpi_float.dt  /* ompi_datatype_basicDatatypes[DT_FLOAT] */,
                                 NULL, 
                                 &ompi_mpi_char.dt  /* ompi_datatype_basicDatatypes[DT_CHAR] */ };
    int lengths[] = { 2, 1, 3 };
    MPI_Aint disp[] = { 0, 16, 26 };
    ompi_datatype_t* pdt, *pdt1;
   
    printf( "test struct\n" );
    ompi_datatype_create_contiguous(0, &ompi_mpi_datatype_null.dt, &pdt1);
    ompi_datatype_add( pdt1, &ompi_mpi_double.dt, 1, 0, -1 );
    ompi_datatype_add( pdt1, &ompi_mpi_char.dt, 1, 8, -1 );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt1 );
    }

    types[1] = pdt1;

    ompi_datatype_create_struct( 3, lengths, disp, types, &pdt );
    OBJ_RELEASE( pdt1 ); /*assert( pdt1 == NULL );*/
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt );
    }
    return pdt;
}

typedef struct {
    int i1;
    int gap;
    int i2;
} sdata_intern;

typedef struct {
    int counter;
    sdata_intern v[10];
    int last;
} sstrange;

#define SSTRANGE_CNT 10
#define USE_RESIZED

ompi_datatype_t* create_strange_dt( void )
{
    sdata_intern v[2];
    MPI_Aint displ[3];
    ompi_datatype_t* types[3] = { &ompi_mpi_int.dt };
    sstrange t[2];
    int pBlock[3] = {1, 10, 1}, dispi[3];
    ompi_datatype_t *pdt, *pdt1, *pdt2, *pdtTemp;

    dispi[0] = (int)((char*)&(v[0].i1) - (char*)&(v[0]));  /* 0 */
    dispi[1] = (int)(((char*)(&(v[0].i2)) - (char*)&(v[0])) / sizeof(int));  /* 2 */
    ompi_datatype_create_indexed_block( 2, 1, dispi, &ompi_mpi_int.dt, &pdtTemp );
#ifdef USE_RESIZED
    /* optional */
    displ[0] = 0;
    displ[1] = (char*)&(v[1]) - (char*)&(v[0]);
    ompi_datatype_create_resized( pdtTemp, displ[0], displ[1], &pdt1 );
    OBJ_RELEASE( pdtTemp ); assert( pdtTemp == NULL );
#else
    pdt1 = pdtTemp;
#endif  /* USE_RESIZED */

    types[1] = pdt1;
    types[2] = &ompi_mpi_int.dt;
    displ[0] = 0;
    displ[1] = (long)((char*)&(t[0].v[0]) - (char*)&(t[0]));
    displ[2] = (long)((char*)&(t[0].last) - (char*)&(t[0]));
    ompi_datatype_create_struct( 3, pBlock, displ, types, &pdtTemp );
#ifdef USE_RESIZED
    /* optional */
    displ[1] = (char*)&(t[1]) - (char*)&(t[0]);
    ompi_datatype_create_resized( pdtTemp, displ[0], displ[1], &pdt2 );
    OBJ_RELEASE( pdtTemp ); assert( pdtTemp == NULL );
#else
    pdt2 = pdtTemp;
#endif  /* USE_RESIZED */

    ompi_datatype_create_contiguous( SSTRANGE_CNT, pdt2, &pdt );

    OBJ_RELEASE( pdt1 );
    OBJ_RELEASE( pdt2 );
    printf( "\nStrange datatype BEFORE COMMIT\n" );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt );
    }
    ompi_datatype_commit( &pdt );
    printf( "\nStrange datatype AFTER COMMIT\n" );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt );
    }
    return pdt;
}

ompi_datatype_t* create_contiguous_type( const ompi_datatype_t* data, int count )
{
    ompi_datatype_t* contiguous;

    ompi_datatype_create_contiguous( count, data, &contiguous );
    ompi_datatype_commit( &contiguous );
    return contiguous;
}

ompi_datatype_t* create_vector_type( const ompi_datatype_t* data, int count, int length, int stride )
{
    ompi_datatype_t* vector;

    ompi_datatype_create_vector( count, length, stride, data, &vector );
    ompi_datatype_commit( &vector );
    return vector;
}

