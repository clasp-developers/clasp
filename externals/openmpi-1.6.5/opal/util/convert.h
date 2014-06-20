/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
/** 
 * @file
 *
 * This file will hopefully not last long in the tree, but it's
 * unfortunately necessary for now.
 *
 * There are multiple places in the code base where we need to safely
 * convert from a size_t to an int.  However, on some platforms,
 * sizeof(size_t) is larger than sizeof(int), so casting from size_t
 * -> int will result in a compiler warning and potentially data
 * truncation.
 *
 * But, unfortunately, we still need to do it.  But we definitely do
 * not want compiler warnings.  So when sizeof(size_t)>sizeof(int),
 * the solution is the treat the size_t value like an array and
 * dereference the appropriate nibble and cast that to an int (which
 * accounts for both big and little endian machines).
 *
 * Most places in the code where this casting must occur are because
 * collision of APIs (e.g., one API requires a size_t and another API
 * requires an int.  And in most places, we're not going to overflow
 * the int when casting down into it (e.g., it's the result of a
 * strlen, or the length of the buffer in an ompi_buffer_t -- if that
 * buffer is larger than MAX_INT, we've got other problems!).
 *
 * BUT -- the whole premise of casting down to an int is dangerous.
 * So we provide extra protection here to detect overflow situations
 * and print out appropriate warnings.  So if this situation ever
 * occurs, we'll still overflow, but we'll have a good indication that
 * it's happening, and where.
 */

#ifndef OPAL_CONVERT_H
#define OPAL_CONVERT_H

#include "opal_config.h"

BEGIN_C_DECLS

/**
 * Convert a size_t to an int.
 *
 * @param in The size_t value to be converted
 * @param out The output int value.
 * @param want_check Whether to check for truncation or not
 *
 * @returns OPAL_SUCESS If all went well
 * @returns OPAL_NOT_SUPPORTED if the size_t value was truncated
 *
 * The conversion will always occur.  However, if the size_t value was
 * truncated (i.e., sizeof(size_t) > sizeof(int), and the cast down to
 * the int actually changed the value), OPAL_NOT_SUPPORTED will be
 * returned.
 *
 * On platforms where sizeof(size_t) <= sizeof(int), this function
 * will aways return OPAL_SUCCESS.
 */
OPAL_DECLSPEC int opal_size2int(size_t in, int *out, bool want_check) __opal_attribute_nonnull__(2);

END_C_DECLS

#endif
