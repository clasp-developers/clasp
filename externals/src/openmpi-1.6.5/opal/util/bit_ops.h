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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_BIT_OPS_H
#define OPAL_BIT_OPS_H

/**
 * Calculates the highest bit in an integer
 *
 * @param value The integer value to examine
 * @param start Position to start looking
 *
 * @returns pos Position of highest-set integer or -1 if none are set.
 *
 * Look at the integer "value" starting at position "start", and move
 * to the right.  Return the index of the highest bit that is set to
 * 1.
 *
 * WARNING: *NO* error checking is performed.  This is meant to be a
 * fast inline function.
 */
static inline int opal_hibit(int value, int start)
{
  unsigned int mask;

  --start;
  mask = 1 << start;

  for (; start >= 0; --start, mask >>= 1) {
    if (value & mask) {
      break;
    }
  }
  
  return start;
}


/**
 * Returns the cube dimension of a given value.
 *
 * @param value The integer value to examine
 *
 * @returns cubedim The smallest cube dimension containing that value
 *
 * Look at the integer "value" and calculate the smallest power of two
 * dimension that contains that value.
 *
 * WARNING: *NO* error checking is performed.  This is meant to be a
 * fast inline function.
 */
static inline int opal_cube_dim(int value) 
{
    int dim, size;

    for (dim = 0, size = 1; size < value; ++dim, size <<= 1) {
        continue;
    }

    return dim;
}

#endif /* OPAL_BIT_OPS_H */
