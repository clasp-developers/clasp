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
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stddef.h>
#include <stdlib.h>

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype_internal.h"

#if OPAL_ENABLE_DEBUG
#include "opal/util/output.h"

extern int opal_position_debug;
#define DO_DEBUG(INST)  if( opal_position_debug ) { INST }
#else
#define DO_DEBUG(INST)
#endif  /* OPAL_ENABLE_DEBUG */

/* The pack/unpack functions need a cleanup. I have to create a proper interface to access
 * all basic functionalities, hence using them as basic blocks for all conversion functions.
 *
 * But first let's make some global assumptions:
 * - a datatype (with the flag DT_DATA set) will have the contiguous flags set if and only if
 *   the data is really contiguous (extent equal with size)
 * - for the OPAL_DATATYPE_LOOP type the DT_CONTIGUOUS flag set means that the content of the loop is
 *   contiguous but with a gap in the begining or at the end.
 * - the DT_CONTIGUOUS flag for the type OPAL_DATATYPE_END_LOOP is meaningless.
 */

static inline void position_predefined_data( opal_convertor_t* CONVERTOR,
                                             dt_elem_desc_t* ELEM,
                                             uint32_t* COUNT,
                                             unsigned char** POINTER,
                                             size_t* SPACE )
{
    uint32_t _copy_count = *(COUNT);
    size_t _copy_blength;
    ddt_elem_desc_t* _elem = &((ELEM)->elem);

    _copy_blength =  opal_datatype_basicDatatypes[_elem->common.type]->size;
    if( (_copy_count * _copy_blength) > *(SPACE) ) {
        _copy_count = (uint32_t)(*(SPACE) / _copy_blength);
        if( 0 == _copy_count ) return;  /* nothing to do */
    }
    _copy_blength *= _copy_count;

    OPAL_DATATYPE_SAFEGUARD_POINTER( *(POINTER) + _elem->disp, _copy_blength, (CONVERTOR)->pBaseBuf,
                                (CONVERTOR)->pDesc, (CONVERTOR)->count );
    *(POINTER) += (_copy_count * _elem->extent);
    *(SPACE)   -= _copy_blength;
    *(COUNT)   -= _copy_count;
}

static inline void position_contiguous_loop( opal_convertor_t* CONVERTOR,
                                             dt_elem_desc_t* ELEM,
                                             uint32_t* COUNT,
                                             unsigned char** POINTER,
                                             size_t* SPACE )
{
    ddt_loop_desc_t *_loop = (ddt_loop_desc_t*)(ELEM);
    ddt_endloop_desc_t* _end_loop = (ddt_endloop_desc_t*)((ELEM) + (ELEM)->loop.items);
    uint32_t _copy_loops = *(COUNT);

    if( (_copy_loops * _end_loop->size) > *(SPACE) )
        _copy_loops = (uint32_t)(*(SPACE) / _end_loop->size);
    OPAL_DATATYPE_SAFEGUARD_POINTER( *(POINTER) + _end_loop->first_elem_disp,
                                (_copy_loops - 1) * _loop->extent + _end_loop->size,
                                (CONVERTOR)->pBaseBuf, (CONVERTOR)->pDesc, (CONVERTOR)->count );
    *(POINTER) += _copy_loops * _loop->extent;
    *(SPACE)   -= _copy_loops * _end_loop->size;
    *(COUNT)   -= _copy_loops;
}

#define POSITION_PREDEFINED_DATATYPE( CONVERTOR, ELEM, COUNT, POSITION, SPACE ) \
    position_predefined_data( (CONVERTOR), (ELEM), &(COUNT), &(POSITION), &(SPACE) )

#define POSITION_CONTIGUOUS_LOOP( CONVERTOR, ELEM, COUNT, POSITION, SPACE ) \
    position_contiguous_loop( (CONVERTOR), (ELEM), &(COUNT), &(POSITION), &(SPACE) )

int opal_convertor_generic_simple_position( opal_convertor_t* pConvertor,
                                            size_t* position )
{
    dt_stack_t* pStack;       /* pointer to the position on the stack */
    uint32_t pos_desc;        /* actual position in the description of the derived datatype */
    uint32_t count_desc;      /* the number of items already done in the actual pos_desc */
    uint16_t type;            /* type at current position */
    dt_elem_desc_t* description = pConvertor->use_desc->desc;
    dt_elem_desc_t* pElem;
    unsigned char *base_pointer = pConvertor->pBaseBuf;
    size_t iov_len_local;
    OPAL_PTRDIFF_TYPE extent = pConvertor->pDesc->ub - pConvertor->pDesc->lb;

    DUMP( "opal_convertor_generic_simple_position( %p, &%ld )\n", (void*)pConvertor, (long)*position );

    /* We dont want to have to parse the datatype multiple times. What we are interested in
     * here is to compute the number of completed datatypes that we can move forward, update
     * the the counters and finally compute the position taking in account only the remaining
     * elements. The only problem is that we have to modify all the elements on the stack.
     */
    iov_len_local = *position - pConvertor->bConverted;
    if( iov_len_local > pConvertor->pDesc->size ) {
        pStack = pConvertor->pStack;  /* we're working with the full stack */
        count_desc = (uint32_t)(iov_len_local / pConvertor->pDesc->size);
        DO_DEBUG( opal_output( 0, "position before %lu asked %lu data size %lu"
                               " iov_len_local %lu count_desc %d\n",
                               (unsigned long)pConvertor->bConverted, (unsigned long)*position, (unsigned long)pConvertor->pDesc->size,
                               (unsigned long)iov_len_local, count_desc ); );
        /**
         * Update all the stack except the last one which is supposed to be for
         * the last partial element description.
         */
        for( type = 0; type < pConvertor->stack_pos; type++ )
            pStack[type].disp += count_desc * extent;
        pConvertor->bConverted += count_desc * pConvertor->pDesc->size;
        iov_len_local = *position - pConvertor->bConverted;
        pStack[0].count -= count_desc;
        DO_DEBUG( opal_output( 0, "after bConverted %lu remaining count %lu iov_len_local %lu\n",
                               (unsigned long)pConvertor->bConverted, (unsigned long)pStack[0].count, (unsigned long)iov_len_local ); );
    }

    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc      = pStack->index;
    base_pointer += pStack->disp;
    count_desc    = (uint32_t)pStack->count;
    pStack--;
    pConvertor->stack_pos--;
    pElem = &(description[pos_desc]);
    base_pointer += pStack->disp;

    DO_DEBUG( opal_output( 0, "position start pos_desc %d count_desc %d disp %llx\n"
                           "stack_pos %d pos_desc %d count_desc %d disp %llx\n",
                           pos_desc, count_desc, (unsigned long long)(base_pointer - pConvertor->pBaseBuf),
                           pConvertor->stack_pos, pStack->index, (int)pStack->count, (unsigned long long)pStack->disp ); );

    while( 1 ) {
        if( OPAL_DATATYPE_END_LOOP == pElem->elem.common.type ) { /* end of the current loop */
            DO_DEBUG( opal_output( 0, "position end_loop count %d stack_pos %d pos_desc %d disp %llx space %lu\n",
                                   (int)pStack->count, pConvertor->stack_pos, pos_desc,
                                   (unsigned long long)pStack->disp, (unsigned long)iov_len_local ); );
            if( --(pStack->count) == 0 ) { /* end of loop */
                if( pConvertor->stack_pos == 0 ) {
                    pConvertor->flags |= CONVERTOR_COMPLETED;
                    pConvertor->partial_length = 0;
                    goto complete_loop;  /* completed */
                }
                pConvertor->stack_pos--;
                pStack--;
                pos_desc++;
            } else {
                if( pStack->index == -1 ) {
                    pStack->disp += extent;
                } else {
                    assert( OPAL_DATATYPE_LOOP == description[pStack->index].loop.common.type );
                    pStack->disp += description[pStack->index].loop.extent;
                }
                pos_desc = pStack->index + 1;
            }
            base_pointer = pConvertor->pBaseBuf + pStack->disp;
            UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
            DO_DEBUG( opal_output( 0, "position new_loop count %d stack_pos %d pos_desc %d disp %llx space %lu\n",
                                   (int)pStack->count, pConvertor->stack_pos, pos_desc,
                                   (unsigned long long)pStack->disp, (unsigned long)iov_len_local ); );
        }
        if( OPAL_DATATYPE_LOOP == pElem->elem.common.type ) {
            OPAL_PTRDIFF_TYPE local_disp = (OPAL_PTRDIFF_TYPE)base_pointer;
            if( pElem->loop.common.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS ) {
                POSITION_CONTIGUOUS_LOOP( pConvertor, pElem, count_desc,
                                          base_pointer, iov_len_local );
                if( 0 == count_desc ) {  /* completed */
                    pos_desc += pElem->loop.items + 1;
                    goto update_loop_description;
                }
                /* Save the stack with the correct last_count value. */
            }
            local_disp = (OPAL_PTRDIFF_TYPE)base_pointer - local_disp;
            PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, OPAL_DATATYPE_LOOP, count_desc,
                        pStack->disp + local_disp );
            pos_desc++;
        update_loop_description:  /* update the current state */
            base_pointer = pConvertor->pBaseBuf + pStack->disp;
            UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
            DDT_DUMP_STACK( pConvertor->pStack, pConvertor->stack_pos, pElem, "advance loop" );
            DO_DEBUG( opal_output( 0, "position set loop count %d stack_pos %d pos_desc %d disp %llx space %lu\n",
                                   (int)pStack->count, pConvertor->stack_pos, pos_desc,
                                   (unsigned long long)pStack->disp, (unsigned long)iov_len_local ); );
            continue;
        }
        while( pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA ) {
            /* now here we have a basic datatype */
            POSITION_PREDEFINED_DATATYPE( pConvertor, pElem, count_desc,
                                          base_pointer, iov_len_local );
            if( 0 != count_desc ) {  /* completed */
                type = pElem->elem.common.type;
                pConvertor->partial_length = (uint32_t)iov_len_local;
                goto complete_loop;
            }
            base_pointer = pConvertor->pBaseBuf + pStack->disp;
            pos_desc++;  /* advance to the next data */
            UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
            DO_DEBUG( opal_output( 0, "position set loop count %d stack_pos %d pos_desc %d disp %llx space %lu\n",
                                   (int)pStack->count, pConvertor->stack_pos, pos_desc,
                                   (unsigned long long)pStack->disp, (unsigned long)iov_len_local ); );
        }
    }
 complete_loop:
    pConvertor->bConverted = *position;  /* update the already converted bytes */

    if( !(pConvertor->flags & CONVERTOR_COMPLETED) ) {
        /* I complete an element, next step I should go to the next one */
        PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, OPAL_DATATYPE_UINT1, count_desc,
                    base_pointer - pStack->disp - pConvertor->pBaseBuf );
        DO_DEBUG( opal_output( 0, "position save stack stack_pos %d pos_desc %d count_desc %d disp %llx\n",
                               pConvertor->stack_pos, pStack->index, (int)pStack->count, (unsigned long long)pStack->disp ); );
        return 0;
    }
    return 1;
}
