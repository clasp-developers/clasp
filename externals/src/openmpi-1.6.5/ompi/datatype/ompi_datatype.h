/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2009      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * ompi_datatype_t interface for OMPI internal data type representation
 *
 * ompi_datatype_t is a class which represents contiguous or
 * non-contiguous data together with constituent type-related
 * information.
 */

#ifndef OMPI_DATATYPE_H_HAS_BEEN_INCLUDED
#define OMPI_DATATYPE_H_HAS_BEEN_INCLUDED

#include "ompi_config.h"

#include <stddef.h>
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "ompi/constants.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/class/opal_hash_table.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype.h"
#include "mpi.h"

BEGIN_C_DECLS

/* These flags are on top of the flags in opal_datatype.h */
/* Is the datatype predefined as MPI type (not necessarily as OPAL type, e.g. struct/block types) */
#define OMPI_DATATYPE_FLAG_PREDEFINED    0x0200
/* Keep trace of the type of the predefined datatypes */
#define OMPI_DATATYPE_FLAG_DATA_INT      0x1000
#define OMPI_DATATYPE_FLAG_DATA_FLOAT    0x2000
#define OMPI_DATATYPE_FLAG_DATA_COMPLEX  0x3000
#define OMPI_DATATYPE_FLAG_DATA_TYPE     0x3000
/* In which language the datatype is intended for to be used */
#define OMPI_DATATYPE_FLAG_DATA_C        0x4000
#define OMPI_DATATYPE_FLAG_DATA_CPP      0x8000
#define OMPI_DATATYPE_FLAG_DATA_FORTRAN  0xC000
#define OMPI_DATATYPE_FLAG_DATA_LANGUAGE 0xC000

#define OMPI_DATATYPE_MAX_PREDEFINED 45

#if OMPI_DATATYPE_MAX_PREDEFINED > OPAL_DATATYPE_MAX_SUPPORTED
#error Need to increase the number of supported dataypes by OPAL (value OPAL_DATATYPE_MAX_SUPPORTED).
#endif


/* the data description.
 */
struct ompi_datatype_t {
    opal_datatype_t    super;                    /**< Base opal_datatype_t superclass */
    /* --- cacheline 5 boundary (320 bytes) was 32 bytes ago --- */

    int32_t            id;                       /**< OMPI-layers unique id of the type */
    int32_t            d_f_to_c_index;           /**< Fortran index for this datatype */
    struct opal_hash_table_t *d_keyhash;         /**< Attribute fields */

    void*              args;                     /**< Data description for the user */
    void*              packed_description;       /**< Packed description of the datatype */
    /* --- cacheline 6 boundary (384 bytes) --- */
    char               name[MPI_MAX_OBJECT_NAME];/**< Externally visible name */
    /* --- cacheline 7 boundary (448 bytes) --- */

    /* size: 448, cachelines: 7, members: 7 */
};

typedef struct ompi_datatype_t ompi_datatype_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_datatype_t);

/**
 * Padded struct to maintain back compatibiltiy.
 * See opal/communicator/communicator.h comments with struct opal_communicator_t
 * for full explanation why we chose the following padding construct for predefines.
 */

/* Using set constant for padding of the DATATYPE handles because the size of
 * base structure is very close to being the same no matter the bitness.
 */
#define PREDEFINED_DATATYPE_PAD (512)

struct ompi_predefined_datatype_t {
    struct ompi_datatype_t dt;
    char padding[PREDEFINED_DATATYPE_PAD - sizeof(ompi_datatype_t)];
};

typedef struct ompi_predefined_datatype_t ompi_predefined_datatype_t;

/*
 * The list of predefined datatypes is specified in ompi/include/mpi.h.in
 */

/* Base convertor for all external32 operations */
OMPI_DECLSPEC extern opal_convertor_t* ompi_mpi_external32_convertor;
OMPI_DECLSPEC extern opal_convertor_t* ompi_mpi_local_convertor;
extern struct opal_pointer_array_t ompi_datatype_f_to_c_table;

OMPI_DECLSPEC int32_t ompi_datatype_init( void );
OMPI_DECLSPEC int32_t ompi_datatype_finalize( void );

OMPI_DECLSPEC int32_t ompi_datatype_default_convertors_init( void );
OMPI_DECLSPEC int32_t ompi_datatype_default_convertors_fini( void );

OMPI_DECLSPEC void ompi_datatype_dump (const ompi_datatype_t* pData);
OMPI_DECLSPEC ompi_datatype_t* ompi_datatype_create( int32_t expectedSize );

static inline int32_t
ompi_datatype_is_committed( const ompi_datatype_t* type )
{
    return opal_datatype_is_committed(&type->super);
}

static inline int32_t
ompi_datatype_is_overlapped( const ompi_datatype_t* type )
{
    return opal_datatype_is_overlapped(&type->super);
}

static inline int32_t
ompi_datatype_is_valid( const ompi_datatype_t* type )
{
    return opal_datatype_is_valid(&type->super);
}

static inline int32_t
ompi_datatype_is_predefined( const ompi_datatype_t* type )
{
    return (type->super.flags & OMPI_DATATYPE_FLAG_PREDEFINED);
}

static inline int32_t
ompi_datatype_is_contiguous_memory_layout( const ompi_datatype_t* type, int32_t count )
{
    return opal_datatype_is_contiguous_memory_layout(&type->super, count);
}

static inline int32_t
ompi_datatype_commit( ompi_datatype_t ** type )
{
    return opal_datatype_commit ( (opal_datatype_t*)*type );
}


static inline int32_t
ompi_datatype_destroy( ompi_datatype_t** type)
{
    ompi_datatype_t* pData = *type;

    if( ompi_datatype_is_predefined(pData) && (pData->super.super.obj_reference_count <= 1) )
        return OMPI_ERROR;

    OBJ_RELEASE( pData );
    *type = NULL;
    return OMPI_SUCCESS;
}


/*
 * Datatype creation functions
 */
static inline int32_t
ompi_datatype_add( ompi_datatype_t* pdtBase, const ompi_datatype_t* pdtAdd, uint32_t count,
                   OPAL_PTRDIFF_TYPE disp, OPAL_PTRDIFF_TYPE extent )
{
    return opal_datatype_add( &pdtBase->super, &pdtAdd->super, count, disp, extent );
}


static inline int32_t
ompi_datatype_duplicate( const ompi_datatype_t* oldType, ompi_datatype_t** newType )
{
    ompi_datatype_t * new_ompi_datatype = ompi_datatype_create( oldType->super.desc.used + 2 );

    *newType = new_ompi_datatype;
    if( NULL == new_ompi_datatype ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    opal_datatype_clone ( &oldType->super, &new_ompi_datatype->super);
    /* Strip the predefined flag at the OMPI level. */
    new_ompi_datatype->super.flags &= ~OMPI_DATATYPE_FLAG_PREDEFINED;
    /* By default maintain the relationships related to the old data (such as ops) */
    new_ompi_datatype->id = oldType->id;

    /* Set the keyhash to NULL -- copying attributes is *only* done at
       the top level (specifically, MPI_TYPE_DUP). */
    new_ompi_datatype->d_keyhash = NULL;
    new_ompi_datatype->args = NULL;
    snprintf (new_ompi_datatype->name, MPI_MAX_OBJECT_NAME, "Dup %s",
              oldType->name);

    return OMPI_SUCCESS;
}

OMPI_DECLSPEC int32_t ompi_datatype_create_contiguous( int count, const ompi_datatype_t* oldType, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_datatype_create_vector( int count, int bLength, int stride,
                                                   const ompi_datatype_t* oldType, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_datatype_create_hvector( int count, int bLength, OPAL_PTRDIFF_TYPE stride,
                                                    const ompi_datatype_t* oldType, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_datatype_create_indexed( int count, const int* pBlockLength, const int* pDisp,
                                                    const ompi_datatype_t* oldType, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_datatype_create_hindexed( int count, const int* pBlockLength, const OPAL_PTRDIFF_TYPE* pDisp,
                                                     const ompi_datatype_t* oldType, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_datatype_create_indexed_block( int count, int bLength, const int* pDisp,
                                                          const ompi_datatype_t* oldType, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_datatype_create_struct( int count, const int* pBlockLength, const OPAL_PTRDIFF_TYPE* pDisp,
                                                   ompi_datatype_t* const* pTypes, ompi_datatype_t** newType );
static inline int32_t
ompi_datatype_create_resized( const ompi_datatype_t* oldType, OPAL_PTRDIFF_TYPE lb, OPAL_PTRDIFF_TYPE extent, ompi_datatype_t** newType )
{
    ompi_datatype_t * type;
    ompi_datatype_duplicate( oldType, &type );
    if ( NULL == type) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    opal_datatype_resize ( &type->super, lb, extent );
    *newType = type;
    return OMPI_SUCCESS;
}

static inline int32_t
ompi_datatype_type_lb( const ompi_datatype_t* type, OPAL_PTRDIFF_TYPE* disp )
{
    return opal_datatype_type_lb(&type->super, disp);
}

static inline int32_t
ompi_datatype_type_ub( const ompi_datatype_t* type, OPAL_PTRDIFF_TYPE* disp )
{
    return opal_datatype_type_ub( &type->super, disp);
}

static inline int32_t
ompi_datatype_type_size ( const ompi_datatype_t* type, size_t *size )
{
    return opal_datatype_type_size( &type->super, size);
}

static inline int32_t
ompi_datatype_type_extent( const ompi_datatype_t* type, OPAL_PTRDIFF_TYPE* extent )
{
    return opal_datatype_type_extent( &type->super, extent);
}

static inline int32_t
ompi_datatype_get_extent( const ompi_datatype_t* type, OPAL_PTRDIFF_TYPE* lb, OPAL_PTRDIFF_TYPE* extent)
{
    return opal_datatype_get_extent( &type->super, lb, extent);
}

static inline int32_t
ompi_datatype_get_true_extent( const ompi_datatype_t* type, OPAL_PTRDIFF_TYPE* true_lb, OPAL_PTRDIFF_TYPE* true_extent)
{
    return opal_datatype_get_true_extent( &type->super, true_lb, true_extent);
}

static inline int32_t
ompi_datatype_get_element_count( const ompi_datatype_t* type, size_t iSize )
{
    return opal_datatype_get_element_count( &type->super, iSize );
}

static inline int32_t
ompi_datatype_set_element_count( const ompi_datatype_t* type, uint32_t count, size_t* length )
{
    return opal_datatype_set_element_count( &type->super, count, length );
}

static inline int32_t
ompi_datatype_copy_content_same_ddt( const ompi_datatype_t* type, int32_t count,
                                     char* pDestBuf, char* pSrcBuf )
{
    return opal_datatype_copy_content_same_ddt( &type->super, count, pDestBuf, pSrcBuf );
}

OMPI_DECLSPEC const ompi_datatype_t* ompi_datatype_match_size( int size, uint16_t datakind, uint16_t datalang );

/*
 *
 */
OMPI_DECLSPEC int32_t ompi_datatype_sndrcv( void *sbuf, int32_t scount, const ompi_datatype_t* sdtype,
                                            void *rbuf, int32_t rcount, const ompi_datatype_t* rdtype);

/*
 *
 */
OMPI_DECLSPEC int32_t ompi_datatype_get_args( const ompi_datatype_t* pData, int32_t which,
                                              int32_t * ci, int32_t * i,
                                              int32_t * ca, OPAL_PTRDIFF_TYPE* a,
                                              int32_t * cd, ompi_datatype_t** d, int32_t * type);
OMPI_DECLSPEC int32_t ompi_datatype_set_args( ompi_datatype_t* pData,
                                              int32_t ci, int32_t ** i,
                                              int32_t ca, OPAL_PTRDIFF_TYPE* a,
                                              int32_t cd, ompi_datatype_t** d,int32_t type);
OMPI_DECLSPEC int32_t ompi_datatype_copy_args( const ompi_datatype_t* source_data,
                                               ompi_datatype_t* dest_data );
OMPI_DECLSPEC int32_t ompi_datatype_release_args( ompi_datatype_t* pData );
OMPI_DECLSPEC ompi_datatype_t* ompi_datatype_get_single_predefined_type_from_args( ompi_datatype_t* type );

/*
 *
 */
OMPI_DECLSPEC size_t ompi_datatype_pack_description_length( const ompi_datatype_t* datatype );

/*
 *
 */
OMPI_DECLSPEC int ompi_datatype_get_pack_description( ompi_datatype_t* datatype,
                                                      const void** packed_buffer );

/*
 *
 */
struct ompi_proc_t;
OMPI_DECLSPEC ompi_datatype_t* ompi_datatype_create_from_packed_description( void** packed_buffer,
                                                                             struct ompi_proc_t* remote_processor );

OMPI_DECLSPEC int32_t ompi_datatype_print_args( const ompi_datatype_t* pData );

#if OPAL_ENABLE_DEBUG
/*
 * Set a breakpoint to this function in your favorite debugger
 * to make it stop on all pack and unpack errors.
 */
OMPI_DECLSPEC int ompi_datatype_safeguard_pointer_debug_breakpoint( const void* actual_ptr, int length,
                                                                    const void* initial_ptr,
                                                                    const ompi_datatype_t* pData,
                                                                    int count );
#endif  /* OPAL_ENABLE_DEBUG */

END_C_DECLS
#endif  /* OMPI_DATATYPE_H_HAS_BEEN_INCLUDED */
