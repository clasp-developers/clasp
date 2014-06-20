/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <stdio.h>
#include <string.h>
#include "opal/datatype/opal_convertor.h"
#include "ompi/datatype/ompi_datatype.h"
#include "opal/util/output.h"

/**
 * The purpose of this test is to simulate the multi-network packing and
 * unpacking process. The pack operation will happens in-order while the
 * will be done randomly. Therefore, before each unpack the correct
 * position in the user buffer has to be set.
 */

static int fragment_size = 113;
static int data_count = 2048;

typedef struct {
    size_t position;
    size_t size;
    void* buffer;
} ddt_segment_t;

static int
create_segments( ompi_datatype_t* datatype, int count,
                 size_t segment_length,
                 ddt_segment_t** segments, int* seg_count )
{
    size_t data_size, total_length, position;
    opal_convertor_t* convertor;
    int i;
    ddt_segment_t* segment;

    ompi_datatype_type_size( datatype, &data_size );
    data_size *= count;
    *seg_count = data_size / segment_length;
    if( ((*seg_count) * segment_length) != data_size )
        *seg_count += 1;
 allocate_segments:
    *segments = (ddt_segment_t*)malloc( (*seg_count) * sizeof(ddt_segment_t) );

    convertor = opal_convertor_create( opal_local_arch, 0 );
    opal_convertor_prepare_for_send( convertor, &(datatype->super), count, NULL );

    position = 0;
    total_length = 0;
    for( i = 0; i < (*seg_count); i++ ) {
        segment = &((*segments)[i]);
        segment->buffer = malloc(segment_length);
        segment->position = position;

        /* Find the end of the segment */
        position += segment_length;
        opal_convertor_set_position( convertor, &position );
        segment->size = position - segment->position;
        total_length += segment->size;
    }
    OBJ_RELEASE(convertor);
    if( total_length != data_size ) {
        for( i = 0; i < (*seg_count); i++ ) {
            segment = &((*segments)[i]);
            free(segment->buffer);
        }
        free( *segments );
        (*seg_count) += 1;
        goto allocate_segments;
    }
    return 0;
}

static int
shuffle_segments( ddt_segment_t* segments, int seg_count )
{
    ddt_segment_t temporary;
    int i;

    for( i = 0; i < (seg_count/2); i += 2 ) {
        temporary = segments[i];
        segments[i] = segments[seg_count - i - 1];
        segments[seg_count - i - 1] = temporary;
    }
    return 0;
}

static int
pack_segments( ompi_datatype_t* datatype, int count,
               size_t segment_size,
               ddt_segment_t* segments, int seg_count,
               void* buffer )
{
    size_t max_size, position;
    opal_convertor_t* convertor;
    struct iovec iov;
    int i;
    uint32_t iov_count;

    convertor = opal_convertor_create( opal_local_arch, 0 );
    opal_convertor_prepare_for_send( convertor, &(datatype->super), count, buffer );

    for( i = 0; i < seg_count; i++ ) {
        iov.iov_len  = segments[i].size;
        iov.iov_base = segments[i].buffer;
        max_size = iov.iov_len;
        position = segments[i].position;
        opal_convertor_set_position( convertor, &position );
        if( position != segments[i].position ) {
            opal_output( 0, "Setting position failed (%lu != %lu)\n",
                         (unsigned long)segments[i].position, (unsigned long)position );
            break;
        }

        iov_count = 1;
        opal_convertor_pack( convertor, &iov, &iov_count, &max_size );
        if( max_size != segments[i].size ) {
            opal_output( 0, "Amount of packed data do not match (%lu != %lu)\n",
                         (unsigned long)max_size, (unsigned long)segments[i].size );
            opal_output( 0, "Segment %d position %lu size %lu\n", i,
                         (unsigned long)segments[i].position, segments[i].size );
        }
    }
    OBJ_RELEASE(convertor);
    return i;
}

static int
unpack_segments( ompi_datatype_t* datatype, int count,
                 size_t segment_size,
                 ddt_segment_t* segments, int seg_count,
                 void* buffer )
{
    opal_convertor_t* convertor;
    size_t max_size, position;
    int i;
    uint32_t iov_count;
    struct iovec iov;

    convertor = opal_convertor_create( opal_local_arch, 0 );
    opal_convertor_prepare_for_recv( convertor, &(datatype->super), count, buffer );

    for( i = 0; i < seg_count; i++ ) {
        iov.iov_len = segments[i].size;
        iov.iov_base = segments[i].buffer;
        max_size = iov.iov_len;

        position = segments[i].position;
        opal_convertor_set_position( convertor, &position );
        if( position != segments[i].position ) {
            opal_output( 0, "Setting position failed (%lu != %lu)\n",
                         (unsigned long)segments[i].position, (unsigned long)position );
            break;
        }

        iov_count = 1;
        opal_convertor_unpack( convertor, &iov, &iov_count, &max_size );
        if( max_size != segments[i].size ) {
            opal_output( 0, "Amount of unpacked data do not match (%lu != %lu)\n",
                         (unsigned long)max_size, (unsigned long)segments[i].size );
            opal_output( 0, "Segment %d position %lu size %lu\n", i,
                         (unsigned long)segments[i].position, segments[i].size );
        }
    }
    OBJ_RELEASE(convertor);
    return 0;
}

typedef struct {
    long double ld;
    int         i;
} ddt_ldi_t;

static void dump_ldi( ddt_ldi_t* buffer, int start_pos, int end_pos )
{
    int i;

    for( i = start_pos; i < end_pos; i++ ) {
        printf( "buffer[%d] = (%Lf, %d)\n", i, buffer[i].ld, buffer[i].i );

    }
}

#if (OPAL_ENABLE_DEBUG == 1) && (OPAL_C_HAVE_VISIBILITY == 0)
extern int opal_unpack_debug;
extern int opal_pack_debug;
extern int opal_position_debug ;
#endif  /* OPAL_ENABLE_DEBUG */

static char* bytes_dump( void* src, size_t cnt )
{
    static char text[1024];
    int index, i;

    index = sprintf( text, "0x" );
    for( i = 0; i < (int)cnt; i++ )
        index += sprintf( text + index, "%x", (int)(((char*)src)[i]) );
    *(text + index) = '\0';
    return text;
}

int main( int argc, char* argv[] )
{
    ddt_segment_t* segments;
    ddt_ldi_t *send_buffer, *recv_buffer;
    int i, seg_count, errors;
    int show_only_first_error = 1;
    ompi_datatype_t* datatype = MPI_LONG_DOUBLE_INT;

    send_buffer = malloc( sizeof(ddt_ldi_t) * data_count );
    recv_buffer = malloc( sizeof(ddt_ldi_t) * data_count );

    for( i = 0; i < data_count; i++ ) {
        send_buffer[i].ld = (long double)i + (long double)i / 100000.0;
        send_buffer[i].i  = i;
    }
    memcpy(recv_buffer, send_buffer, sizeof(ddt_ldi_t) * data_count );

    opal_datatype_init();
    ompi_datatype_init();

#if (OPAL_ENABLE_DEBUG == 1) && (OPAL_C_HAVE_VISIBILITY == 0)
    opal_unpack_debug   = 0;
    opal_pack_debug     = 0;
    opal_position_debug = 0;
#endif  /* OPAL_ENABLE_DEBUG */

    create_segments( datatype, data_count, fragment_size,
                     &segments, &seg_count );

    /* shuffle the segments */
    shuffle_segments( segments, seg_count );

    /* pack the data */
    pack_segments( datatype, data_count, fragment_size, segments, seg_count,
                   send_buffer );

    /* unpack the data back in the user space (recv buffer) */
    unpack_segments( datatype, data_count, fragment_size, segments, seg_count,
                     recv_buffer );

    /* And now check the data */
    for( errors = i = 0; i < data_count; i++ ) {
        /*if( !bytes_equal(&send_buffer[i].ld, &recv_buffer[i].ld, sizeof(long double)) ||*/
        if( (send_buffer[i].ld != recv_buffer[i].ld) ||
            (send_buffer[i].i != recv_buffer[i].i) ) {
            if( (show_only_first_error && (0 == errors)) ||
                !show_only_first_error ) {
                printf( "error at %4d [*(%s,%d)\n"
                        "             != (%s,%d)\n", i,
                        bytes_dump( &send_buffer[i].ld, sizeof(long double)), send_buffer[i].i,
                        bytes_dump( &recv_buffer[i].ld, sizeof(long double)), recv_buffer[i].i );
            }
            errors++;
        }
    }
    printf( "Found %d errors\n", errors );
    free(send_buffer); free(recv_buffer);

    for( i = 0; i < seg_count; i++ ) {
        free( segments[i].buffer );
    }
    free(segments);

    ompi_datatype_finalize();

    return (0 == errors ? 0 : -1);
}
