/*
 * Local definitions for the user allocation level
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
 * $Id: dmalloc_loc.h,v 1.46 2000/05/16 18:35:57 gray Exp $
 */

#ifndef __DMALLOC_LOC_H__
#define __DMALLOC_LOC_H__

#include "conf.h"			/* for HAVE_BCMP and BASIC_BLOCK */

/*
 * env variable(s)
 */
#define OPTIONS_ENVIRON		"DMALLOC_OPTIONS"

/*
 * web home directory
 */
#define DMALLOC_HOME		"http://dmalloc.com/"

/*
 * generic constants
 */
/* special consideration for NULL.  some compilers bitch if I redefine it */
#ifndef NULL
#define NULL		0L
#endif

/*
 * standard i/o file descriptors
 */
#undef	STDIN
#define	STDIN		0		/* fileno(stdin) */

#undef	STDOUT
#define	STDOUT		1		/* fileno(stdout) */

#undef	STDERR
#define	STDERR		2		/* fileno(stderr) */

/*
 * Default values for the file and line variables.
 */
#define DMALLOC_DEFAULT_FILE	0L
#define DMALLOC_DEFAULT_LINE	0

/*
 * Min/max macros
 *
 * WARNING: these use their arguments multiple times which may be bad
 */
#undef MAX
#define MAX(a,b)	(((a) > (b)) ? (a) : (b))
#undef MIN
#define MIN(a,b)	(((a) < (b)) ? (a) : (b))

/*
 * bitflag tools for Variable and a Flag
 */
#undef BIT_FLAG
#define BIT_FLAG(x)		(1 << (x))
#undef BIT_SET
#define BIT_SET(v,f)		(v) |= (f)
#undef BIT_CLEAR
#define BIT_CLEAR(v,f)		(v) &= ~(f)
#undef BIT_IS_SET
#define BIT_IS_SET(v,f)		((v) & (f))

/*
 * set pointer macro
 */
#define SET_POINTER(pnt, val) \
	do { \
	  if ((pnt) != NULL) { \
	    (*(pnt)) = (val); \
          } \
        } while(0)

/*
 * Global malloc defines
 */
#define BLOCK_SIZE		(int)(1 << BASIC_BLOCK)	/* size of a block */

#endif /* ! __DMALLOC_LOC_H__ */
