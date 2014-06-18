/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    page.h  -- Page macros.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.

    ECoLisp is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#ifdef __cplusplus
extern "C" {
#endif

/*****************************
 * BOEHM's GARBAGE COLLECTOR *
 *****************************/

#ifdef GBC_BOEHM
#ifdef ECL_THREADS
#define GC_THREADS
#endif

extern struct typemanager {
	const char *tm_name;
	cl_index tm_size;
} tm_table[(int)t_end];

#define	tm_of(t)	(&tm_table[(int)(t)])
#endif


/****************************************
 * ECOLISP's ORIGINAL GARBAGE COLLECTOR *
 ****************************************/

#if !defined(GBC_BOEHM)
/* THREADS: If you make it bigger, the bug is less frequent */
#ifdef SYSV
#define	HOLEPAGE	32
#else
#define	HOLEPAGE	128
#endif
#define	INIT_HOLEPAGE	150
#define	CBMINSIZE	64	/*  contiguous block minimal size  */

typedef char *cl_ptr;
#define ptr2int(p)	((cl_ptr)(p) - (cl_ptr)0)
#define int2ptr(n)	((cl_ptr)0 + (n))
#define page(p)		(((cl_ptr)(p) - heap_start)/LISP_PAGESIZE)
#define	pagetochar(x)	(heap_start + (x) * LISP_PAGESIZE)
#define round_to_page(x) (((x) + LISP_PAGESIZE - 1) / LISP_PAGESIZE)
#define	round_up(n)	(((n) + 03) & ~03)
#define	available_pages() ((cl_index)(real_maxpage-page(heap_end)-new_holepage-real_maxpage/32))

extern cl_index real_maxpage;
extern cl_index new_holepage;

/*
	The struct of free lists.
*/
struct freelist {
	HEADER;
	cl_object	f_link;
};

/*
	Type map.

	enum type type_map[MAXPAGE];
*/
extern char type_map[MAXPAGE];

/*
	Storage manager for each type.
*/
struct typemanager {
	cl_type		tm_type;	/*  type  */
	cl_index	tm_size;	/*  element size in bytes  */
	cl_index	tm_nppage;	/*  number per page  */
	cl_object	tm_free;	/*  free list  */
					/*  Note that it is of type object.  */
	cl_index	tm_nfree;	/*  number of free elements  */
	cl_index	tm_nused;	/*  number of elements used  */
	cl_index	tm_npage;	/*  number of pages  */
	cl_index	tm_maxpage;	/*  maximum number of pages  */
	char		*tm_name;	/*  type name  */
	cl_index	tm_gccount;	/*  GC count  */
};

/*
	The table of type managers.
*/
extern struct typemanager tm_table[(int)t_end];

#define	tm_of(t)	(&(tm_table[(int)tm_table[(int)(t)].tm_type]))

/*
	Contiguous block header.
*/
struct contblock {			/*  contiguous block header  */
	cl_index cb_size;		/*  size in bytes  */
	struct contblock *cb_link;	/*  contiguous block link  */
};

/*
	The pointer to the contiguous blocks.
*/
extern struct contblock *cb_pointer;	/*  contblock pointer  */

/*
	Variables for memory management.
*/
extern cl_index ncb;		/*  number of contblocks  */
extern cl_index ncbpage;	/*  number of contblock pages  */
extern cl_index maxcbpage;	/*  maximum number of contblock pages  */
extern cl_index cbgccount;	/*  contblock gc count  */
extern cl_index holepage;	/*  hole pages  */

extern char *heap_start;	/*  heap start  */
extern char *heap_end;		/*  heap end  */
extern char *data_end;		/*  core end  */

#endif /* !GBC_BOEHM */

/*******************************
 * SYMBOLS & KEYWORDS DATABASE *
 *******************************/

#ifdef __cplusplus
}
#endif
