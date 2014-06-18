/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    ecl_atomics.h -- alternative definitions for atomic operations
*/
/*
    Copyright (c) 2012, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#ifndef ECL_ATOMICS_H
#define AO_ASSUME_WINDOWS98
#include <ecl/internal.h>

#if !defined(AO_HAVE_compare_and_swap_full)
# error "ECL needs AO_compare_and_swap_full or an equivalent"
#endif
#if !defined(AO_HAVE_compare_and_swap)
# error "ECL needs AO_compare_and_swap or an equivalent"
#endif
#if !defined(AO_HAVE_fetch_and_add1)
# error "Cannot implement mailboxs without AO_fetch_and_add1"
#endif

#endif /* ECL_ATOMICS_H */
