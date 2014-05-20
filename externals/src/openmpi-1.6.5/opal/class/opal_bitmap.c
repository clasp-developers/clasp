/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <limits.h>

#include "opal/constants.h"
#include "opal/class/opal_bitmap.h"


#define SIZE_OF_CHAR ((int) (sizeof(char) * 8))

static void opal_bitmap_construct(opal_bitmap_t *bm);
static void opal_bitmap_destruct(opal_bitmap_t *bm);

OBJ_CLASS_INSTANCE(opal_bitmap_t, opal_object_t, 
                   opal_bitmap_construct, opal_bitmap_destruct);


static void 
opal_bitmap_construct(opal_bitmap_t *bm) 
{
    bm->bitmap = NULL;
    bm->array_size = 0;
    bm->max_size = INT_MAX;
}


static void
opal_bitmap_destruct(opal_bitmap_t *bm)
{
    if (NULL != bm->bitmap) {
        free(bm->bitmap);
    }
}


int opal_bitmap_set_max_size (opal_bitmap_t *bm, int max_size)
{
    int actual_size;

    if (NULL == bm) {
        return OPAL_ERR_BAD_PARAM;
    }

    /*
     * Only if the caller wants to set the maximum size,
     * we set it (in numbers of bits!), otherwise it is
     * set to INT_MAX in the constructor.
     */
    actual_size = max_size / SIZE_OF_CHAR;
    actual_size += (max_size % SIZE_OF_CHAR == 0) ? 0 : 1;

    bm->max_size = actual_size;

    return OPAL_SUCCESS;
}


int
opal_bitmap_init(opal_bitmap_t *bm, int size)
{
    int actual_size;

    /*
     * Only if the caller set the maximum size before initializing,
     * we test here (in numbers of bits!)
     * By default, the max size is INT_MAX, set in the constructor.
     */
    if ((size <= 0) || (NULL == bm) || (size > bm->max_size)) {
        return OPAL_ERR_BAD_PARAM;
    }
    
    actual_size = size / SIZE_OF_CHAR;
    actual_size += (size % SIZE_OF_CHAR == 0) ? 0 : 1;
    bm->array_size = actual_size;
    bm->bitmap = (unsigned char *) malloc(actual_size);
    if (NULL == bm->bitmap) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /*
     * Leave bm->max_size untouched: it is initialized to INT_MAX in the constructor
     */

    opal_bitmap_clear_all_bits(bm);
    return OPAL_SUCCESS;
}


int
opal_bitmap_set_bit(opal_bitmap_t *bm, int bit)
{
    int index, offset, new_size;
    size_t new_size_large;
    
    if ((bit < 0) || (NULL == bm) || (bit > bm->max_size)) {
        return OPAL_ERR_BAD_PARAM;
    }
    
    index = bit / SIZE_OF_CHAR; 
    offset = bit % SIZE_OF_CHAR;
    
    if (index >= bm->array_size) {
        
        /* We need to allocate more space for the bitmap, since we are
         out of range. We don't throw any error here, because this is
         valid and we simply expand the bitmap */
        
        new_size_large = (index / bm->array_size + 1 ) * bm->array_size;
        
        /* Note that new_size is guaranteed to be <=
         INT_MAX, which is guaranteed to fit in a
         [signed] int. */
        
        new_size = (int) new_size_large;

        /*
         * No further tests against max_size (or OMPI_FORTRAN_HANDLE_MAX) are
         * necessary, since we validated above, that the bit already is contained!
         */
        
        /* New size is just a multiple of the original size to fit in
         the index. */
        
        bm->bitmap = (unsigned char *) realloc(bm->bitmap, (int) new_size);
        if (NULL == bm->bitmap) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        
        /* zero out the new elements */
        memset(&bm->bitmap[bm->array_size], 0, new_size - bm->array_size);
        
        /* Update the array_size */
        bm->array_size = new_size;
    }
    
    /* Now set the bit */
    bm->bitmap[index] |= (1 << offset);
    
    return OPAL_SUCCESS;
}


int
opal_bitmap_clear_bit(opal_bitmap_t *bm, int bit)
{
    int index, offset;
    
    if ((bit < 0) || NULL == bm || (bit >= (bm->array_size * SIZE_OF_CHAR))) {
        return OPAL_ERR_BAD_PARAM;
    }
    
    index = bit / SIZE_OF_CHAR; 
    offset = bit % SIZE_OF_CHAR;
    
    if (index >= bm->array_size) {
        return OPAL_ERR_BAD_PARAM;
    }
    
    bm->bitmap[index] &= ~(1 << offset);
    return OPAL_SUCCESS;
}


bool
opal_bitmap_is_set_bit(opal_bitmap_t *bm, int bit)
{
    int index, offset;
    
    if ((bit < 0) || NULL == bm || (bit >= (bm->array_size * SIZE_OF_CHAR))) {
        return false;
    }
    
    index = bit / SIZE_OF_CHAR; 
    offset = bit % SIZE_OF_CHAR;
    
    if (index >= bm->array_size) {
        return false;
    }
    
    if (0 != (bm->bitmap[index] & (1 << offset))) {
        return true;
    }
    
    return false;
}


int
opal_bitmap_clear_all_bits(opal_bitmap_t *bm)
{
    if (NULL == bm) {
        return OPAL_ERR_BAD_PARAM;
    }
    
    memset(bm->bitmap, 0, bm->array_size);
    return OPAL_SUCCESS;
}


int
opal_bitmap_set_all_bits(opal_bitmap_t *bm)
{
    if (NULL == bm) {
        return OPAL_ERR_BAD_PARAM;
    }
    
    memset(bm->bitmap, 0xff, bm->array_size);
    
    return OPAL_SUCCESS;
}


int
opal_bitmap_find_and_set_first_unset_bit(opal_bitmap_t *bm, int *position)
{
    int i = 0;
    unsigned char temp;
    unsigned char all_ones = 0xff;
    
    if (NULL == bm) {
        return OPAL_ERR_BAD_PARAM;
    }
    
    /* Neglect all which don't have an unset bit */
    *position = 0;
    while((i < bm->array_size) && (bm->bitmap[i] == all_ones)) {
        ++i;
    }
    
    if (i == bm->array_size) {
        /* increase the bitmap size then */
        *position = bm->array_size * SIZE_OF_CHAR;
        return opal_bitmap_set_bit(bm, *position);
    }
    
    /* This one has an unset bit, find its bit number */
    
    temp = bm->bitmap[i];
    while (temp & 0x1) {
        ++(*position);
        temp >>= 1;
    }
    
    /* Now set the bit number */
    bm->bitmap[i] |= (bm->bitmap[i] + 1);
    
    (*position) += i * SIZE_OF_CHAR;
    return OPAL_SUCCESS;
}
