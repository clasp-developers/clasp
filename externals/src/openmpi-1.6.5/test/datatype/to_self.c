/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "mpi.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#if OPEN_MPI && 0
extern void ompi_datatype_dump( MPI_Datatype ddt );
#define MPI_DDT_DUMP(ddt) ompi_datatype_dump( (ddt) )
#else
#define MPI_DDT_DUMP(ddt)
#endif  /* OPEN_MPI */

/* Create a datatype similar to the one use by HPL */
static MPI_Datatype
create_indexed_constant_gap_ddt( int number,  /* number of repetitions */
                                 int contig_size,  /* number of elements in a contiguous chunk */
                                 int gap_size )    /* number of elements in a gap */
{
    MPI_Datatype dt, *types;
    int i, *bLength;
    MPI_Aint* displ;

    types = (MPI_Datatype*)malloc( sizeof(MPI_Datatype) * number );
    bLength = (int*)malloc( sizeof(int) * number );
    displ = (MPI_Aint*)malloc( sizeof(MPI_Aint) * number );

    types[0] = MPI_DOUBLE;
    bLength[0] = contig_size;
    displ[0] = 0;
    for( i = 1; i < number; i++ ) {
        types[i] = MPI_DOUBLE;
        bLength[i] = contig_size;
        displ[i] = displ[i-1] + sizeof(double) * (contig_size + gap_size);
    }
    MPI_Type_struct( number, bLength, displ, types, &dt );
    MPI_DDT_DUMP( dt );
    free(types);
    free(bLength);
    free(displ);
    MPI_Type_commit( &dt );
    return dt;
}

static MPI_Datatype
create_optimized_indexed_constant_gap_ddt( int number,  /* number of repetitions */
                                           int contig_size,  /* number of elements in a contiguous chunk */
                                           int gap_size )    /* number of elements in a gap */
{
    MPI_Datatype dt;

    MPI_Type_vector( number, contig_size, (contig_size + gap_size), MPI_DOUBLE, &dt );
    MPI_Type_commit( &dt );
    MPI_DDT_DUMP( dt );
    return dt;
}

typedef struct {
   int i[2];
   float f;
} internal_struct;
typedef struct {
   int v1;
   int gap1;
   internal_struct is[3];
} ddt_gap;

static MPI_Datatype
create_indexed_gap_ddt( void )
{
    ddt_gap dt[2];
    MPI_Datatype dt1, dt2, dt3;
    int bLength[2] = { 2, 1 };
    MPI_Datatype types[2] = { MPI_INT, MPI_FLOAT };
    MPI_Aint displ[2];

    MPI_Address( &(dt[0].is[0].i[0]), &(displ[0]) );
    MPI_Address( &(dt[0].is[0].f), &(displ[1]) );
    displ[1] -= displ[0];
    displ[0] -= displ[0];
    MPI_Type_struct( 2, bLength, displ, types, &dt1 );
    /*MPI_DDT_DUMP( dt1 );*/
    MPI_Type_contiguous( 3, dt1, &dt2 );
    /*MPI_DDT_DUMP( dt2 );*/
    bLength[0] = 1;
    bLength[1] = 1;
    MPI_Address( &(dt[0].v1), &(displ[0]) );
    MPI_Address( &(dt[0].is[0]), &(displ[1]) );
    displ[1] -= displ[0];
    displ[0] -= displ[0];
    types[0] = MPI_INT;
    types[1] = dt2;
    MPI_Type_struct( 2, bLength, displ, types, &dt3 );
    /*MPI_DDT_DUMP( dt3 );*/
    MPI_Type_free( &dt1 );
    MPI_Type_free( &dt2 );
    MPI_Type_contiguous( 10, dt3, &dt1 );
    MPI_DDT_DUMP( dt1 );
    MPI_Type_free( &dt3 );
    MPI_Type_commit( &dt1 );
    return dt1;
}

static MPI_Datatype
create_indexed_gap_optimized_ddt( void )
{
    MPI_Datatype dt1, dt2, dt3;
    int bLength[3];
    MPI_Datatype types[3];
    MPI_Aint displ[3];
   
    MPI_Type_contiguous( 40, MPI_BYTE, &dt1 );
    MPI_Type_create_resized( dt1, 0, 44, &dt2 );
   
    bLength[0] = 4;
    bLength[1] = 9;
    bLength[2] = 36;
   
    types[0] = MPI_BYTE;
    types[1] = dt2;
    types[2] = MPI_BYTE;

    displ[0] = 0;
    displ[1] = 8;
    displ[2] = 44 * 9 + 8;
   
    MPI_Type_struct( 3, bLength, displ, types, &dt3 );
   
    MPI_Type_free( &dt1 );
    MPI_Type_free( &dt2 );
    MPI_DDT_DUMP( dt3 );
    MPI_Type_commit( &dt3 );
    return dt3;
}

static void print_result( int length, int cycles, double time )
{
    double bandwidth, clock_prec;

    clock_prec = MPI_Wtick();
    bandwidth = (length * clock_prec * cycles) / (1024.0 * 1024.0) / (time * clock_prec);
    printf( "%8d\t%.6f\t%.4f MB/s\n", length, time / cycles, bandwidth );
}

static int isend_recv( int cycles,
                       MPI_Datatype sdt, int scount, void* sbuf,
                       MPI_Datatype rdt, int rcount, void* rbuf )
{
    int sres, rres, myself, tag = 0, i, slength, rlength;
    MPI_Status status;
    MPI_Request req;
    double tstart, tend;
   
    MPI_Type_size( sdt, &slength );
    slength *= scount;
    MPI_Type_size( rdt, &rlength );
    rlength *= rcount;

    MPI_Comm_rank( MPI_COMM_WORLD, &myself );

    tstart = MPI_Wtime();
    for( i = 0; i < cycles; i++ ) {
#ifndef FAST
        sres = MPI_Isend( sbuf, scount, sdt, myself, tag, MPI_COMM_WORLD, &req );
        rres = MPI_Recv( rbuf, rcount, rdt, myself, tag, MPI_COMM_WORLD, &status );
        MPI_Wait( &req, &status );
        /*MPI_Request_free( &req );*/
#else
        sres = ftmpi_mpi_isend( sbuf, scount, sdt, myself, tag, MPI_COMM_WORLD, &req );
        rres = ftmpi_mpi_recv( rbuf, rcount, rdt, myself, tag, MPI_COMM_WORLD, &status );
        ftmpi_request_free( &req );
#endif
    }
    tend = MPI_Wtime();
    print_result( rlength, cycles, tend - tstart );
    return 0;
}

static int irecv_send( int cycles,
                       MPI_Datatype sdt, int scount, void* sbuf,
                       MPI_Datatype rdt, int rcount, void* rbuf )
{
    int sres, rres, myself, tag = 0, i, slength, rlength;
    MPI_Request req;
    MPI_Status status;
    double tstart, tend;
   
    MPI_Type_size( sdt, &slength );
    slength *= scount;
    MPI_Type_size( rdt, &rlength );
    rlength *= rcount;

    MPI_Comm_rank( MPI_COMM_WORLD, &myself );

    tstart = MPI_Wtime();
    for( i = 0; i < cycles; i++ ) {
#ifndef FAST
        rres = MPI_Irecv( rbuf, rcount, rdt, myself, tag, MPI_COMM_WORLD, &req );
        sres = MPI_Send( sbuf, scount, sdt, myself, tag, MPI_COMM_WORLD );
        MPI_Wait( &req, &status );
        /*MPI_Request_free( &req );*/
#else
        rres = ftmpi_mpi_irecv( rbuf, rcount, rdt, myself, tag, MPI_COMM_WORLD, &req );
        sres = ftmpi_mpi_send( sbuf, scount, sdt, myself, tag, MPI_COMM_WORLD );
        ftmpi_request_free( &req );
#endif
    }
    tend = MPI_Wtime();
    print_result( rlength, cycles, tend - tstart );
    return 0;
}

static int isend_irecv_wait( int cycles,
                             MPI_Datatype sdt, int scount, void* sbuf,
                             MPI_Datatype rdt, int rcount, void* rbuf )
{
    int sres, rres, myself, tag = 0, i, slength, rlength;
    MPI_Request sreq, rreq;
    MPI_Status status;
    double tstart, tend;
   
    MPI_Type_size( sdt, &slength );
    slength *= scount;
    MPI_Type_size( rdt, &rlength );
    rlength *= rcount;

    MPI_Comm_rank( MPI_COMM_WORLD, &myself );

    tstart = MPI_Wtime();
    for( i = 0; i < cycles; i++ ) {
#ifndef FAST
        sres = MPI_Isend( sbuf, scount, sdt, myself, tag, MPI_COMM_WORLD, &sreq );
        rres = MPI_Irecv( rbuf, rcount, rdt, myself, tag, MPI_COMM_WORLD, &rreq );
        MPI_Wait( &sreq, &status );
        MPI_Wait( &rreq, &status );
        /*MPI_Request_free( &sreq );*/
        /*MPI_Request_free( &rreq );*/
#else
        sres = ftmpi_mpi_isend( sbuf, scount, sdt, myself, tag, MPI_COMM_WORLD, &sreq );
        rres = ftmpi_mpi_irecv( rbuf, rcount, rdt, myself, tag, MPI_COMM_WORLD, &rreq );
        ftmpi_wait( &sreq, &status );
        ftmpi_request_free( &sreq );
        ftmpi_request_free( &rreq );
#endif
    }
    tend = MPI_Wtime();
    print_result( rlength, cycles, tend - tstart );
    return 0;
}

static int irecv_isend_wait( int cycles,
                             MPI_Datatype sdt, int scount, void* sbuf,
                             MPI_Datatype rdt, int rcount, void* rbuf )
{
    int sres, rres, myself, tag = 0, i, slength, rlength;
    MPI_Request sreq, rreq;
    MPI_Status status;
    double tstart, tend;
   
    MPI_Type_size( sdt, &slength );
    slength *= scount;
    MPI_Type_size( rdt, &rlength );
    rlength *= rcount;

    MPI_Comm_rank( MPI_COMM_WORLD, &myself );

    tstart = MPI_Wtime();
    for( i = 0; i < cycles; i++ ) {
#ifndef FAST
        rres = MPI_Irecv( rbuf, rcount, rdt, myself, tag, MPI_COMM_WORLD, &rreq );
        sres = MPI_Isend( sbuf, scount, sdt, myself, tag, MPI_COMM_WORLD, &sreq );
        MPI_Wait( &sreq, &status );
        MPI_Wait( &rreq, &status );
        /*MPI_Request_free( &sreq );*/
        /*MPI_Request_free( &rreq );*/
#else
        rres = ftmpi_mpi_irecv( rbuf, rcount, rdt, myself, tag, MPI_COMM_WORLD, &rreq );
        sres = ftmpi_mpi_isend( sbuf, scount, sdt, myself, tag, MPI_COMM_WORLD, &sreq );
        ftmpi_wait( &sreq, &status );
        ftmpi_request_free( &sreq );
        ftmpi_request_free( &rreq );
#endif
    }
    tend = MPI_Wtime();
    print_result( rlength, cycles, tend - tstart );
    return 0;
}

static int do_test_for_ddt( MPI_Datatype sddt, MPI_Datatype rddt, int length )
{
    int i;
    MPI_Aint extent;
    char *sbuf, *rbuf;

    MPI_Type_extent( sddt, &extent );
    sbuf = (char*)malloc( length );
    rbuf = (char*)malloc( length );
    printf( "# Isend recv\n" );
    for( i = 1; i <= (length/extent); i *= 2 ) {
        isend_recv( 10, sddt, i, sbuf, rddt, i, rbuf );
    }
    printf( "# Isend Irecv Wait\n" );
    for( i = 1; i <= (length/extent); i *= 2 ) {
        isend_irecv_wait( 10, sddt, i, sbuf, rddt, i, rbuf );
    }
    printf( "# Irecv send\n" );
    for( i = 1; i <= (length/extent); i *= 2 ) {
        irecv_send( 10, sddt, i, sbuf, rddt, i, rbuf );
    }
    printf( "# Irecv Isend Wait\n" );
    for( i = 1; i <= (length/extent); i *= 2 ) {
        irecv_isend_wait( 10, sddt, i, sbuf, rddt, i, rbuf );
    }
    free( sbuf );
    free( rbuf );
    return 0;
}

#define DO_CONTIG                 0x01
#define DO_CONSTANT_GAP           0x02
#define DO_INDEXED_GAP            0x04
#define DO_OPTIMIZED_INDEXED_GAP  0x08

int main( int argc, char* argv[] )
{
    int rc;
    int length = 1024 * 1024;
    int rank, size;
    MPI_Datatype ddt;
    int run_tests = DO_CONTIG | DO_CONSTANT_GAP | DO_INDEXED_GAP | DO_OPTIMIZED_INDEXED_GAP;
    /*int run_tests = DO_CONSTANT_GAP;*/

    rc = MPI_Init (&argc, &argv);

    rc = MPI_Comm_rank (MPI_COMM_WORLD, &rank);
    rc = MPI_Comm_size (MPI_COMM_WORLD, &size);

    if( rank != 0 ) {
        MPI_Finalize();
        exit(0);
    }

    if( run_tests & DO_CONTIG ) {
        printf( "\ncontiguous datatype\n\n" );
        do_test_for_ddt( MPI_INT, MPI_INT, length );
    }

    if( run_tests & DO_INDEXED_GAP ) {
        printf( "\nindexed gap\n\n" );
        ddt = create_indexed_gap_ddt();
        MPI_DDT_DUMP( ddt );
        do_test_for_ddt( ddt, ddt, length );
        MPI_Type_free( &ddt );
    }

    if( run_tests & DO_OPTIMIZED_INDEXED_GAP ) {
        printf( "\noptimized indexed gap\n\n" );
        ddt = create_indexed_gap_optimized_ddt();
        MPI_DDT_DUMP( ddt );
        do_test_for_ddt( ddt, ddt, length );
        MPI_Type_free( &ddt );
    }

    if( run_tests & DO_CONSTANT_GAP ) {
        printf( "\nconstant indexed gap\n\n" );
        ddt = create_indexed_constant_gap_ddt( 80, 100, 1 );
        MPI_DDT_DUMP( ddt );
        do_test_for_ddt( ddt, ddt, length );
        MPI_Type_free( &ddt );
    }

    if( run_tests & DO_CONSTANT_GAP ) {
        printf( "\noptimized constant indexed gap\n\n" );
        ddt = create_optimized_indexed_constant_gap_ddt( 80, 100, 1 );
        MPI_DDT_DUMP( ddt );
        do_test_for_ddt( ddt, ddt, length );
        MPI_Type_free( &ddt );
    }

    MPI_Finalize ();
    exit(0);
}

