/*
 * File that facilitates C++ program debugging.
 *
 * Copyright 2000 by Gray Watson
 *
 * This file is part of the dmalloc package.
 *
 * Permission to use, copy, modify, and distribute this software for
 * any purpose and without fee is hereby granted, provided that the
 * above copyright notice and this permission notice appear in all
 * copies, and that the name of Gray Watson not be used in advertising
 * or publicity pertaining to distribution of the document or software
 * without specific, written prior permission.
 *
 * Gray Watson makes no representations about the suitability of the
 * software described herein for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * The author may be contacted via http://dmalloc.com/
 *
 * $Id: dmallocc.cc,v 1.3 2003/05/15 20:08:46 gray Exp $
 */

/*
 * This file is used to effectively redirect new to the more familiar
 * malloc and delete to the more familiar free so they can be debugged
 * with the debug malloc library..  They also give the known error
 * behavior, too.
 *
 * Compile and link this in with the C++ program you want to debug.
 *
 * NOTE: I am not a C++ hacker so feedback in the form of other hints
 * and ideas for C++ users would be much appreciated.
 */
 
extern "C" {
#include <stdlib.h>

#define DMALLOC_DISABLE

#include "dmalloc.h"
#include "return.h"
}

/*
 * An overload function for the C++ new.
 */
void *
operator new(size_t size)
{
  char	*file;
  GET_RET_ADDR(file);
  return dmalloc_malloc(file, 0, size, DMALLOC_FUNC_NEW,
			0 /* no alignment */, 0 /* no xalloc messages */);
}

/*
 * An overload function for the C++ new[].
 */
void *
operator new[](size_t size)
{
  char	*file;
  GET_RET_ADDR(file);
  return dmalloc_malloc(file, 0, size, DMALLOC_FUNC_NEW_ARRAY,
			0 /* no alignment */, 0 /* no xalloc messages */);
}

/*
 * An overload function for the C++ delete.
 */
void
operator delete(void *pnt)
{
  char	*file;
  GET_RET_ADDR(file);
  dmalloc_free(file, 0, pnt, DMALLOC_FUNC_DELETE);
}

/*
 * An overload function for the C++ delete[].  Thanks to Jens Krinke.
 */
void
operator delete[](void *pnt)
{
  char	*file;
  GET_RET_ADDR(file);
  dmalloc_free(file, 0, pnt, DMALLOC_FUNC_DELETE_ARRAY);
}
