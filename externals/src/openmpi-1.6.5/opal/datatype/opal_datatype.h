/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * opal_datatype_t interface for OPAL internal data type representation
 *
 * opal_datatype_t is a class which represents contiguous or
 * non-contiguous data together with constituent type-related
 * information.
 */

#ifndef OPAL_DATATYPE_H_HAS_BEEN_INCLUDED
#define OPAL_DATATYPE_H_HAS_BEEN_INCLUDED

#include "opal_config.h"

#include <stddef.h>
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

#include "opal/class/opal_object.h"

BEGIN_C_DECLS

/*
 * If there are more basic datatypes than the number of bytes in the int type
 * the bdt_used field of the data description struct should be changed to long.
 *
 * This must match the same definition as in opal_datatype_internal.h
 */
#define OPAL_DATATYPE_MAX_PREDEFINED 25
/*
 * No more than this number of _Basic_ datatypes in C/CPP or Fortran
 * are supported (in order to not change setup and usage of btypes).
 *
 * XXX TODO Adapt to whatever the OMPI-layer needs
 */
#define OPAL_DATATYPE_MAX_SUPPORTED  46


/* flags for the datatypes. */
#define OPAL_DATATYPE_FLAG_UNAVAILABLE   0x0001  /**< datatypes unavailable on the build (OS or compiler dependant) */
#define OPAL_DATATYPE_FLAG_PREDEFINED    0x0002  /**< cannot be removed: initial and predefined datatypes */
#define OPAL_DATATYPE_FLAG_COMMITED      0x0004  /**< ready to be used for a send/recv operation */
#define OPAL_DATATYPE_FLAG_OVERLAP       0x0008  /**< datatype is unpropper for a recv operation */
#define OPAL_DATATYPE_FLAG_CONTIGUOUS    0x0010  /**< contiguous datatype */
#define OPAL_DATATYPE_FLAG_NO_GAPS       0x0020  /**< no gaps around the datatype, aka OPAL_DATATYPE_FLAG_CONTIGUOUS and extent == size */
#define OPAL_DATATYPE_FLAG_USER_LB       0x0040  /**< has a user defined LB */
#define OPAL_DATATYPE_FLAG_USER_UB       0x0080  /**< has a user defined UB */
#define OPAL_DATATYPE_FLAG_DATA          0x0100  /**< data or control structure */
/*
 * We should make the difference here between the predefined contiguous and non contiguous
 * datatypes. The OPAL_DATATYPE_FLAG_BASIC is held by all predefined contiguous datatypes.
 */
#define OPAL_DATATYPE_FLAG_BASIC         (OPAL_DATATYPE_FLAG_PREDEFINED | \
                                          OPAL_DATATYPE_FLAG_CONTIGUOUS | \
                                          OPAL_DATATYPE_FLAG_NO_GAPS |    \
                                          OPAL_DATATYPE_FLAG_DATA |       \
                                          OPAL_DATATYPE_FLAG_COMMITED)


/**
 * The number of supported entries in the data-type definition and the
 * associated type.
 */
#define MAX_DT_COMPONENT_COUNT UINT_MAX
typedef uint32_t opal_datatype_count_t;

typedef union dt_elem_desc dt_elem_desc_t;

struct dt_type_desc_t {
    opal_datatype_count_t  length;  /**< the maximum number of elements in the description array */
    opal_datatype_count_t  used;    /**< the number of used elements in the description array */
    dt_elem_desc_t*        desc;
};
typedef struct dt_type_desc_t dt_type_desc_t;


/*
 * The datatype description.
 */
struct opal_datatype_t {
    opal_object_t      super;    /**< basic superclass */
    uint16_t           flags;    /**< the flags */
    uint16_t           id;       /**< data id, normally the index in the data array. */
    uint32_t           bdt_used; /**< bitset of which basic datatypes are used in the data description */
    size_t             size;     /**< total size in bytes of the memory used by the data if
                                      the data is put on a contiguous buffer */
    OPAL_PTRDIFF_TYPE  true_lb;  /**< the true lb of the data without user defined lb and ub */
    OPAL_PTRDIFF_TYPE  true_ub;  /**< the true ub of the data without user defined lb and ub */
    OPAL_PTRDIFF_TYPE  lb;       /**< lower bound in memory */
    OPAL_PTRDIFF_TYPE  ub;       /**< upper bound in memory */
    /* --- cacheline 1 boundary (64 bytes) --- */
    uint32_t           align;    /**< data should be aligned to */
    uint32_t           nbElems;  /**< total number of elements inside the datatype */

    /* Attribute fields */
    char               name[OPAL_MAX_OBJECT_NAME];  /* TODO: Can this be deleted??? It's just sugar...*/
    /* --- cacheline 2 boundary (128 bytes) was 8 bytes ago --- */
    dt_type_desc_t     desc;     /**< the data description */
    dt_type_desc_t     opt_desc; /**< short description of the data used when conversion is useless
                                      or in the send case (without conversion) */

    uint32_t           btypes[OPAL_DATATYPE_MAX_SUPPORTED];
                                 /**< basic elements count used to compute the size of the
                                      datatype for remote nodes. The length of the array is dependent on
                                      the maximum number of datatypes of all top layers.
                                      Reason being is that Fortran is not at the OPAL layer. */
    /* --- cacheline 5 boundary (320 bytes) was 32 bytes ago --- */

    /* size: 352, cachelines: 6, members: 15 */
    /* last cacheline: 32 bytes */
};

typedef struct opal_datatype_t opal_datatype_t;

OPAL_DECLSPEC OBJ_CLASS_DECLARATION( opal_datatype_t );

OPAL_DECLSPEC extern const opal_datatype_t* opal_datatype_basicDatatypes[OPAL_DATATYPE_MAX_PREDEFINED];
OPAL_DECLSPEC extern const size_t opal_datatype_local_sizes[OPAL_DATATYPE_MAX_PREDEFINED];

/* Local Architecture as provided by opal_arch_compute_local_id() */
OPAL_DECLSPEC extern uint32_t opal_local_arch;

/*
 * The OPAL-layer's Basic datatypes themselves.
 */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_empty;
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_loop;
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_end_loop;
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_lb;
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_ub;
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_int1;       /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_int2;       /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_int4;       /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_int8;       /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_int16;      /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_uint1;      /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_uint2;      /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_uint4;      /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_uint8;      /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_uint16;     /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_float2;     /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_float4;     /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_float8;     /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_float12;    /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_float16;    /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_complex8;   /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_complex16;  /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_complex32;  /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_bool;
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_wchar;


/*
 * Functions exported externally
 */
int opal_datatype_register_params(void);
OPAL_DECLSPEC int32_t opal_datatype_init( void );
OPAL_DECLSPEC int32_t opal_datatype_finalize( void );
OPAL_DECLSPEC opal_datatype_t* opal_datatype_create( int32_t expectedSize );
OPAL_DECLSPEC int32_t opal_datatype_create_desc( opal_datatype_t * datatype, int32_t expectedSize );
OPAL_DECLSPEC int32_t opal_datatype_commit( opal_datatype_t * pData );
OPAL_DECLSPEC int32_t opal_datatype_destroy( opal_datatype_t** );

static inline int32_t
opal_datatype_is_committed( const opal_datatype_t* type )
{
    return ((type->flags & OPAL_DATATYPE_FLAG_COMMITED) == OPAL_DATATYPE_FLAG_COMMITED);
}

static inline int32_t
opal_datatype_is_overlapped( const opal_datatype_t* type )
{
    return ((type->flags & OPAL_DATATYPE_FLAG_OVERLAP) == OPAL_DATATYPE_FLAG_OVERLAP);
}

static inline int32_t
opal_datatype_is_valid( const opal_datatype_t* type )
{
    return !((type->flags & OPAL_DATATYPE_FLAG_UNAVAILABLE) == OPAL_DATATYPE_FLAG_UNAVAILABLE);
}

static inline int32_t
opal_datatype_is_predefined( const opal_datatype_t* type )
{
    return (type->flags & OPAL_DATATYPE_FLAG_PREDEFINED);
}

/*
 * This function return true (1) if the datatype representation depending on the count
 * is contiguous in the memory. And false (0) otherwise.
 */
static inline int32_t
opal_datatype_is_contiguous_memory_layout( const opal_datatype_t* datatype, int32_t count )
{
    if( !(datatype->flags & OPAL_DATATYPE_FLAG_CONTIGUOUS) ) return 0;
    if( (count == 1) || (datatype->flags & OPAL_DATATYPE_FLAG_NO_GAPS) ) return 1;
    return 0;
}


OPAL_DECLSPEC void opal_datatype_dump( const opal_datatype_t* pData );
/* data creation functions */
OPAL_DECLSPEC int32_t opal_datatype_clone( const opal_datatype_t * src_type, opal_datatype_t * dest_type );
OPAL_DECLSPEC int32_t opal_datatype_create_contiguous( int count, const opal_datatype_t* oldType, opal_datatype_t** newType );
OPAL_DECLSPEC int32_t opal_datatype_resize( opal_datatype_t* type, OPAL_PTRDIFF_TYPE lb, OPAL_PTRDIFF_TYPE extent );
OPAL_DECLSPEC int32_t opal_datatype_add( opal_datatype_t* pdtBase, const opal_datatype_t* pdtAdd, uint32_t count,
                                         OPAL_PTRDIFF_TYPE disp, OPAL_PTRDIFF_TYPE extent );

static inline int32_t
opal_datatype_type_lb( const opal_datatype_t* pData, OPAL_PTRDIFF_TYPE* disp )
{
    *disp = pData->lb;
    return 0;
}

static inline int32_t
opal_datatype_type_ub( const opal_datatype_t* pData, OPAL_PTRDIFF_TYPE* disp )
{
    *disp = pData->ub;
    return 0;
}

static inline int32_t
opal_datatype_type_size( const opal_datatype_t* pData, size_t *size )
{
    *size = pData->size;
    return 0;
}

static inline int32_t
opal_datatype_type_extent( const opal_datatype_t* pData, OPAL_PTRDIFF_TYPE* extent )
{
    *extent = pData->ub - pData->lb;
    return 0;
}

static inline int32_t
opal_datatype_get_extent( const opal_datatype_t* pData, OPAL_PTRDIFF_TYPE* lb, OPAL_PTRDIFF_TYPE* extent)
{
    *lb = pData->lb; *extent = pData->ub - pData->lb;
    return 0;
}

static inline int32_t
opal_datatype_get_true_extent( const opal_datatype_t* pData, OPAL_PTRDIFF_TYPE* true_lb, OPAL_PTRDIFF_TYPE* true_extent)
{
    *true_lb = pData->true_lb;
    *true_extent = (pData->true_ub - pData->true_lb);
    return 0;
}

OPAL_DECLSPEC int32_t
opal_datatype_get_element_count( const opal_datatype_t* pData, size_t iSize );
OPAL_DECLSPEC int32_t
opal_datatype_set_element_count( const opal_datatype_t* pData, uint32_t count, size_t* length );
OPAL_DECLSPEC int32_t
opal_datatype_copy_content_same_ddt( const opal_datatype_t* pData, int32_t count,
                                     char* pDestBuf, char* pSrcBuf );

OPAL_DECLSPEC const opal_datatype_t*
opal_datatype_match_size( int size, uint16_t datakind, uint16_t datalang );

/*
 *
 */
OPAL_DECLSPEC int32_t
opal_datatype_sndrcv( void *sbuf, int32_t scount, const opal_datatype_t* sdtype, void *rbuf,
                      int32_t rcount, const opal_datatype_t* rdtype);

/*
 *
 */
OPAL_DECLSPEC int32_t
opal_datatype_get_args( const opal_datatype_t* pData, int32_t which,
                        int32_t * ci, int32_t * i,
                        int32_t * ca, OPAL_PTRDIFF_TYPE* a,
                        int32_t * cd, opal_datatype_t** d, int32_t * type);
OPAL_DECLSPEC int32_t
opal_datatype_set_args( opal_datatype_t* pData,
                        int32_t ci, int32_t ** i,
                        int32_t ca, OPAL_PTRDIFF_TYPE* a,
                        int32_t cd, opal_datatype_t** d,int32_t type);
OPAL_DECLSPEC int32_t
opal_datatype_copy_args( const opal_datatype_t* source_data,
                         opal_datatype_t* dest_data );
OPAL_DECLSPEC int32_t
opal_datatype_release_args( opal_datatype_t* pData );

/*
 *
 */
OPAL_DECLSPEC size_t
opal_datatype_pack_description_length( const opal_datatype_t* datatype );

/*
 *
 */
OPAL_DECLSPEC int
opal_datatype_get_pack_description( opal_datatype_t* datatype,
                                    const void** packed_buffer );

/*
 *
 */
struct opal_proc_t;
OPAL_DECLSPEC opal_datatype_t*
opal_datatype_create_from_packed_description( void** packed_buffer,
                                              struct opal_proc_t* remote_processor );

#if OPAL_ENABLE_DEBUG
/*
 * Set a breakpoint to this function in your favorite debugger
 * to make it stop on all pack and unpack errors.
 */
OPAL_DECLSPEC int
opal_datatype_safeguard_pointer_debug_breakpoint( const void* actual_ptr, int length,
                                                  const void* initial_ptr,
                                                  const opal_datatype_t* pData,
                                                  int count );
#endif  /* OPAL_ENABLE_DEBUG */

END_C_DECLS
#endif  /* OPAL_DATATYPE_H_HAS_BEEN_INCLUDED */
