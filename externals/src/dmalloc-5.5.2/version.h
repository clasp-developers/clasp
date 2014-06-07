/*
 * Version string for the library
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
 * $Id: version.h,v 1.132 2007/05/14 17:23:37 gray Exp $
 */

#ifndef __VERSION_H__
#define __VERSION_H__

/*
 * NOTE to gray: whenever this is changed, a corresponding entry
 * should be entered in:
 *
 *	Changlog
 *	NEWS
 *	RELEASE.html
 *	configure.ac (in AC_INIT)
 *	dmalloc.spec
 *	dmalloc.texi
 *	dmalloc.h.4 (DMALLOC_VERSION defines at top of file)
 *      ports/.../Makefile and ports/.../distfile files
 *
 * Make sure to also cvs tag the release.  dmalloc_release_X_X_X
 */
static	char	*dmalloc_version = "5.5.2" ;

/* Version Date: $Date: 2007/05/14 17:23:37 $ */

#endif /* ! __VERSION_H__ */
