/* -*- mode: c; c-basic-offset: 8 -*- */

/*
    alloc.c --	Memory allocation.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

/********************************************************************************
 ***                                                                          ***
 ***  IMPORTANT: This is obsolete code. The current garbage collector of ECL  ***
 ***  is the Boehm-Weiser garbage collector and it is dealt with in           ***
 ***                           alloc_2.d                                      ***
 ***  This file is kept here because of historical purposes, but also because ***
 ***  it might be useful in the future to implement another garbage collector ***
 ***                                                                          ***
 ********************************************************************************/

/*
  			Heap and Relocatable Area

	                 heap_end    data_end
    +------+--------------------+ - - - + - - --------+
    | text |        heap        | hole  |      stack  |
    +------+--------------------+ - - - + - - --------+

   The type_map array covers all pages of memory: those not used for objects
   are marked as type t_other.

   The tm_table array holds a struct typemanager for each type, which contains
   the first element of the free list for the type, and other bookkeeping
   information.
*/

#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <ecl/ecl.h>
#include <ecl/internal.h>
#include <ecl/page.h>

#define USE_MMAP
#if defined(USE_MMAP)
#include <sys/types.h>
#include <sys/mman.h>
#elif defined(HAVE_ULIMIT_H)
#include <ulimit.h>
#else
#include <sys/resource.h>
#endif

#ifdef ECL_SMALL_CONS
#error "Internal error: ECL cannot be built with --disable-boehm and --enable-smallcons"
#endif

/******************************* EXPORTS ******************************/

cl_index real_maxpage;
cl_index new_holepage;
char type_map[MAXPAGE];
struct typemanager tm_table[(int)t_end];
struct contblock *cb_pointer = NULL;

cl_index ncb;			/*  number of contblocks  */
cl_index ncbpage;		/*  number of contblock pages  */
cl_index maxcbpage;		/*  maximum number of contblock pages  */
cl_index cbgccount;		/*  contblock gc count  */
cl_index holepage;		/*  hole pages  */

cl_ptr heap_end;		/*  heap end  */
cl_ptr heap_start;		/*  heap start  */
cl_ptr data_end;		/*  end of data space  */

/******************************* ------- ******************************/

static bool ignore_maximum_pages = TRUE;

#ifdef NEED_MALLOC
static cl_object malloc_list;
#endif

/*
   Ensure that the hole is at least "n" pages large. If it is not,
   allocate space from the operating system.
*/

#if defined(USE_MMAP)
void
cl_resize_hole(cl_index n)
{
#define PAGESIZE 8192
	cl_index m, bytes;
	cl_ptr result, last_addr;
	bytes = n * LISP_PAGESIZE;
	bytes = (bytes + PAGESIZE-1) / PAGESIZE;
	bytes = bytes * PAGESIZE;
	if (heap_start == NULL) {
		/* First time use. We allocate the memory and keep the first
		 * address in heap_start.
		 */
		result = mmap(0x2E000000, bytes, PROT_READ | PROT_WRITE,
			      MAP_ANON | MAP_FIXED | MAP_PRIVATE, -1 ,0);
		if (result == MAP_FAILED)
			ecl_internal_error("Cannot allocate memory. Good-bye!");
		data_end = heap_end = heap_start = result;
		last_addr = heap_start + bytes;
		holepage = n;
	} else {
		/* Next time use. We extend the region of memory that we had
		 * mapped before.
		 */
		m = (data_end - heap_end)/LISP_PAGESIZE;
		if (n <= m)
			return;
		result = mmap(data_end, bytes, PROT_READ | PROT_WRITE,
			      MAP_ANON | MAP_FIXED | MAP_PRIVATE, -1, 0);
		if (result == MAP_FAILED)
			ecl_internal_error("Cannot resize memory pool. Good-bye!");
		last_addr = result + bytes;
		if (result != data_end) {
			cl_dealloc(heap_end, data_end - heap_end);
			while (heap_end < result) {
				cl_index p = page(heap_end);
				if (p > real_maxpage)
					ecl_internal_error("Memory limit exceeded.");
				type_map[p] = t_other;
				heap_end += LISP_PAGESIZE;
			}
		}
		holepage = (last_addr - heap_end) / LISP_PAGESIZE;
	}
	while (data_end < last_addr) {
		type_map[page(data_end)] = t_other;
		data_end += LISP_PAGESIZE;
	}
}
#else
void
cl_resize_hole(cl_index n)
{
	cl_ptr e;
	cl_index m;
	m = (data_end - heap_end)/LISP_PAGESIZE;
	if (n <= m)
		return;

	/* Create the hole */
	e = sbrk(0);
	if (data_end == e) {
		e = sbrk((n -= m) * LISP_PAGESIZE);
	} else {
		cl_dealloc(heap_end, data_end - heap_end);
		/* FIXME! Horrible hack! */
		/* mark as t_other pages not allocated by us */
		heap_end = e;
		while (data_end < heap_end) {
			type_map[page(data_end)] = t_other;
			data_end += LISP_PAGESIZE;
		}
		holepage = 0;
		e = sbrk(n * LISP_PAGESIZE + (data_end - e));
	}
	if ((cl_fixnum)e < 0)
		ecl_internal_error("Can't allocate.  Good-bye!");
	data_end = e;
	holepage += n;
}
#endif

/* Allocates n pages from the hole.  */
static void *
alloc_page(cl_index n)
{
	cl_ptr e = heap_end;
	if (n >= holepage) {
		ecl_gc(t_contiguous);
		cl_resize_hole(new_holepage+n);
	}
	holepage -= n;
	heap_end += LISP_PAGESIZE*n;
	return e;
}

/*
 * We have to mark all objects within the page as FREE. However, at
 * the end of the page there might be extra bytes, which have to be
 * tagged as useless. Since these bytes are at least 4, x->m points to
 * data within the page and we can mark this object setting x->m=FREE.
 */
static void
add_page_to_freelist(cl_ptr p, struct typemanager *tm)
{
	cl_type t;
	cl_object x, f;
	cl_index i;
	t = tm->tm_type;
	type_map[page(p)] = t;
	f = tm->tm_free;
	for (i = tm->tm_nppage; i > 0; --i, p += tm->tm_size) {
		x = (cl_object)p;
		((struct freelist *)x)->t = (short)t;
		((struct freelist *)x)->m = FREE;
		((struct freelist *)x)->f_link = f;
		f = x;
	}
	/* Mark the extra bytes which cannot be used. */
	if (tm->tm_size * tm->tm_nppage < LISP_PAGESIZE) {
		x = (cl_object)p;
		x->d.m = FREE;
	}
	tm->tm_free = f;
	tm->tm_nfree += tm->tm_nppage;
	tm->tm_npage++;
}

cl_object
ecl_alloc_object(cl_type t)
{
	register cl_object obj;
	register struct typemanager *tm;
	register cl_ptr p;

	switch (t) {
	case t_fixnum:
	  return MAKE_FIXNUM(0); /* Immediate fixnum */
	case t_character:
	  return ECL_CODE_CHAR('\0'); /* Immediate character */
	default:;
	}

	ecl_disable_interrupts();
	tm = tm_of(t);
ONCE_MORE:
	obj = tm->tm_free;
	if (obj == OBJNULL) {
		cl_index available = available_pages();
		if (tm->tm_npage >= tm->tm_maxpage)
			goto CALL_GC;
		if (available < 1) {
			ignore_maximum_pages = FALSE;
			goto CALL_GC;
		}
		p = alloc_page(1);
		add_page_to_freelist(p, tm);
		obj = tm->tm_free;
		/* why this? Beppe
		if (tm->tm_npage >= tm->tm_maxpage)
			goto CALL_GC; */
	}
	tm->tm_free = ((struct freelist *)obj)->f_link;
	--(tm->tm_nfree);
	(tm->tm_nused)++;
	obj->d.t = (short)t;
	obj->d.m = FALSE;
	/* Now initialize the object so that it can be correctly marked
	 * by the GC
	 */
	switch (t) {
	case t_bignum:
	  ECL_BIGNUM_DIM(obj) = ECL_BIGNUM_SIZE(obj) = 0;
	  ECL_BIGNUM_LIMBS(obj) = NULL;
	  break;
	case t_ratio:
	  obj->ratio.num = OBJNULL;
	  obj->ratio.den = OBJNULL;
	  break;
#ifdef ECL_SSE2
	case t_sse_pack:
#endif
	case t_singlefloat:
	case t_doublefloat:
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
#endif
	  break;
	case t_complex:
	  obj->complex.imag = OBJNULL;
	  obj->complex.real = OBJNULL;
	  break;
	case t_symbol:
	  obj->symbol.plist = OBJNULL;
	  obj->symbol.gfdef = OBJNULL;
	  obj->symbol.value = OBJNULL;
	  obj->symbol.name = OBJNULL;
	  obj->symbol.hpack = OBJNULL;
	  break;
	case t_package:
	  obj->pack.name = OBJNULL;
	  obj->pack.nicknames = OBJNULL;
	  obj->pack.shadowings = OBJNULL;
	  obj->pack.uses = OBJNULL;
	  obj->pack.usedby = OBJNULL;
	  obj->pack.internal = OBJNULL;
	  obj->pack.external = OBJNULL;
	  break;
	case t_cons:
#error "FIXME"	  
	  obj->cons.car = OBJNULL;
	  obj->cons.cdr = OBJNULL;
	  break;
	case t_hashtable:
	  obj->hash.rehash_size = OBJNULL;
	  obj->hash.threshold = OBJNULL;
	  obj->hash.data = NULL;
	  break;
	case t_array:
	  obj->array.dims = NULL;
	  obj->array.displaced = ECL_NIL;
	  obj->array.elttype = (short)ecl_aet_object;
	  obj->array.self.t = NULL;
	  break;
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_vector:
	  obj->array.displaced = ECL_NIL;
	  obj->array.elttype = (short)ecl_aet_object;
	  obj->array.self.t = NULL;
	  break;
	case t_base_string:
	  obj->base_string.displaced = ECL_NIL;
	  obj->base_string.self = NULL;
	  break;
	case t_bitvector:
	  obj->vector.displaced = ECL_NIL;
	  obj->vector.self.bit = NULL;
	  break;
#ifndef CLOS
	case t_structure:
	  obj->str.name = OBJNULL;
	  obj->str.self = NULL;
	  break;
#endif /* CLOS */
	case t_stream:
	  obj->stream.mode = (short)ecl_smm_broadcast;
	  obj->stream.file.descriptor = -1;
	  obj->stream.object0 = OBJNULL;
	  obj->stream.object1 = OBJNULL;
	  obj->stream.buffer = NULL;
	  break;
	case t_random:
	  break;
	case t_readtable:
	  obj->readtable.table = NULL;
	  break;
	case t_pathname:
	  obj->pathname.host = OBJNULL;
	  obj->pathname.device = OBJNULL;
	  obj->pathname.directory = OBJNULL;
	  obj->pathname.name = OBJNULL;
	  obj->pathname.type = OBJNULL;
	  obj->pathname.version = OBJNULL;
	  break;
	case t_bytecodes:
	  obj->bytecodes.lex = ECL_NIL;
	  obj->bytecodes.name = ECL_NIL;
	  obj->bytecodes.definition = ECL_NIL;
	  obj->bytecodes.specials = ECL_NIL;
	  obj->bytecodes.code_size = 0;
	  obj->bytecodes.code = NULL;
	  obj->bytecodes.data = NULL;
	  break;
	case t_bclosure:
	  obj->bclosure.code =
	  obj->bclosure.lex = ECL_NIL;
	  break;
	case t_cfun:
	case t_cfunfixed:
	  obj->cfun.name = OBJNULL;
	  obj->cfun.block = NULL;
	  break;
	case t_cclosure:
	  obj->cclosure.env = OBJNULL;
	  obj->cclosure.block = NULL;
	  break;
/*
	case t_spice:
	  break;
*/
#ifdef ECL_THREADS
	case t_process:
	  obj->process.name = OBJNULL;
	  obj->process.function = OBJNULL;
	  obj->process.args = OBJNULL;
	  obj->process.env = NULL;
	  obj->process.interrupt = OBJNULL;
	  break;
	case t_lock:
	  obj->lock.mutex = OBJNULL;
	case t_condition_variable:
	  obj->condition_variable.cv = OBJNULL;
	  break;
#endif
#ifdef ECL_SEMAPHORES
	case t_semaphore:
          obj->semaphore.handle = NULL;
	  break;
#endif
#ifdef CLOS
	case t_instance:
	  obj->instance.length = 0;
	  ECL_CLASS_OF(obj) = OBJNULL;
	  obj->instance.sig = ECL_NIL;
	  obj->instance.isgf = 0;
	  obj->instance.slots = NULL;
	  break;
#endif /* CLOS */
	case t_codeblock:
	  obj->cblock.locked = 0;
	  obj->cblock.name = ECL_NIL;
	  obj->cblock.handle = NULL;
	  obj->cblock.entry = NULL;
	  obj->cblock.data = NULL;
	  obj->cblock.data_size = 0;
	  obj->cblock.data_text = NULL;
	  obj->cblock.data_text_size = 0;
	  obj->cblock.links = ECL_NIL;
	  obj->cblock.next = ECL_NIL;
	  break;
	case t_foreign:
	  obj->foreign.tag = ECL_NIL;
	  obj->foreign.size = 0;
	  obj->foreign.data = NULL;
	  break;
	default:
	  printf("\ttype = %d\n", t);
	  ecl_internal_error("alloc botch.");
	}
	ecl_enable_interrupts();
	return(obj);
CALL_GC:
	ecl_gc(tm->tm_type);
	if (tm->tm_nfree != 0 &&
		(float)tm->tm_nfree * 10.0 >= (float)tm->tm_nused)
		goto ONCE_MORE;

/*	EXHAUSTED:	*/
	if (ignore_maximum_pages) {
		if (tm->tm_maxpage/2 <= 0)
			tm->tm_maxpage += 1;
		else
			tm->tm_maxpage += tm->tm_maxpage/2;
		goto ONCE_MORE;
	}
	GC_disable();
	{ cl_object s = ecl_make_simple_base_string(tm_table[(int)t].tm_name+1, -1);
	GC_enable();
	CEerror(ECL_T, "The storage for ~A is exhausted.~%\
Currently, ~D pages are allocated.~%\
Use ALLOCATE to expand the space.",
		2, s, MAKE_FIXNUM(tm->tm_npage));
	}
	goto ONCE_MORE;
}

cl_object
ecl_cons(cl_object a, cl_object d)
{
	register cl_object obj;
	register cl_ptr p;
	struct typemanager *tm=(&tm_table[(int)t_cons]);

	ecl_disable_interrupts(); 

ONCE_MORE:
	obj = tm->tm_free;
	if (obj == OBJNULL) {
		if (tm->tm_npage >= tm->tm_maxpage)
			goto CALL_GC;
		if (available_pages() < 1) {
			ignore_maximum_pages = FALSE;
			goto CALL_GC;
		}
		p = alloc_page(1);
		add_page_to_freelist(p,tm);
		obj = tm->tm_free;
		if (tm->tm_npage >= tm->tm_maxpage)
			goto CALL_GC;
	}
	tm->tm_free = ((struct freelist *)obj)->f_link;
	--(tm->tm_nfree);
	(tm->tm_nused)++;
	obj->d.t = (short)t_cons;
	obj->d.m = FALSE;
	obj->cons.car = a;
	obj->cons.cdr = d;

	ecl_enable_interrupts();
	return(obj);

CALL_GC:
	ecl_gc(t_cons);
	if ((tm->tm_nfree != 0) && (tm->tm_nfree * 10.0 >= tm->tm_nused))
		goto ONCE_MORE;

/*	EXHAUSTED:	*/
	if (ignore_maximum_pages) {
		if (tm->tm_maxpage/2 <= 0)
			tm->tm_maxpage += 1;
		else
			tm->tm_maxpage += tm->tm_maxpage/2;
		goto ONCE_MORE;
	}
	CEerror(ECL_T, "The storage for CONS is exhausted.~%\
Currently, ~D pages are allocated.~%\
Use ALLOCATE to expand the space.",
		1, MAKE_FIXNUM(tm->tm_npage));
	goto ONCE_MORE;
#undef	tm
}

cl_object
ecl_alloc_instance(cl_index slots)
{
	cl_object i = ecl_alloc_object(t_instance);
	if (slots >= ECL_SLOTS_LIMIT)
		FEerror("Limit on instance size exceeded: ~S slots requested.",
			1, MAKE_FIXNUM(slots));
	/* INV: slots > 0 */
	i->instance.slots = (cl_object*)ecl_alloc(sizeof(cl_object) * slots);
	i->instance.length = slots;
	return i;
}

void *
ecl_alloc(cl_index n)
{
	volatile cl_ptr p;
	struct contblock **cbpp;
	cl_index i, m;
	bool g;

	g = FALSE;
	n = round_up(n);

	ecl_disable_interrupts(); 
ONCE_MORE:
	/* Use extra indirection so that cb_pointer can be updated */
	for (cbpp = &cb_pointer; (*cbpp) != NULL; cbpp = &(*cbpp)->cb_link) 
		if ((*cbpp)->cb_size >= n) {
			p = (cl_ptr)(*cbpp);
			i = (*cbpp)->cb_size - n;
			*cbpp = (*cbpp)->cb_link;
			--ncb;
			cl_dealloc(p+n, i);

			ecl_enable_interrupts();
			return(p);
		}
	m = round_to_page(n);
	if (ncbpage + m > maxcbpage || available_pages() < m) {
		if (available_pages() < m)
			ignore_maximum_pages = FALSE;
		if (!g) {
			ecl_gc(t_contiguous);
			g = TRUE;
			goto ONCE_MORE;
		}
		if (ignore_maximum_pages) {
			if (maxcbpage/2 <= 0)
				maxcbpage += 1;
			else
				maxcbpage += maxcbpage/2;
			g = FALSE;
			goto ONCE_MORE;
		}
		CEerror(ECL_T, "Contiguous blocks exhausted.~%\
Currently, ~D pages are allocated.~%\
Use ALLOCATE-CONTIGUOUS-PAGES to expand the space.",
			1, MAKE_FIXNUM(ncbpage));
		g = FALSE;
		goto ONCE_MORE;
	}
	p = alloc_page(m);

	for (i = 0;  i < m;  i++)
		type_map[page(p) + i] = (char)t_contiguous;
	ncbpage += m;
	cl_dealloc(p+n, LISP_PAGESIZE*m - n);

	ecl_enable_interrupts();
	return memset(p, 0, n);
}

/*
 * adds a contblock to the list of available ones, pointed by cb_pointer,
 * sorted by increasing size.
 */
void
cl_dealloc(void *p, cl_index s)
{
	struct contblock **cbpp, *cbp;

	if (s < CBMINSIZE)
		return;
	ncb++;
	cbp = (struct contblock *)p;
	cbp->cb_size = s;
	for (cbpp = &cb_pointer; *cbpp != NULL; cbpp = &((*cbpp)->cb_link))
		if ((*cbpp)->cb_size >= s) {
			cbp->cb_link = *cbpp;
			*cbpp = cbp;
			return;
		}
	cbp->cb_link = NULL;
	*cbpp = cbp;
}

/*
 * align must be a power of 2 representing the alignment boundary
 * required for the block.
 */
void *
ecl_alloc_align(cl_index size, cl_index align)
{
	void *output;
	ecl_disable_interrupts();
	align--;
	if (align)
	  output = (void*)(((cl_index)ecl_alloc(size + align) + align - 1) & ~align);
	else
	  output = ecl_alloc(size);
	ecl_enable_interrupts();
	return output;
}

static void
init_tm(cl_type t, const char *name, cl_index elsize, cl_index maxpage)
{
	int i, j;
	struct typemanager *tm = &tm_table[(int)t];

	if (elsize < 2*sizeof(cl_index)) {
		// A free list cell does not fit into this type
		elsize = 2*sizeof(cl_index);
	}

	tm->tm_name = name;
	for (i = (int)t_start, j = i-1;  i < (int)t_end;  i++)
	  if (tm_table[i].tm_size >= elsize &&
	      (j < (int)t_start || tm_table[j].tm_size > tm_table[i].tm_size))
	    j = i;
	if (j >= (int)t_start) {
		tm->tm_type = (cl_type)j;
		tm_table[j].tm_maxpage += maxpage;
		return;
	}
	tm->tm_type = t;
	tm->tm_size = round_up(elsize);
	tm->tm_nppage = LISP_PAGESIZE/round_up(elsize);
	tm->tm_free = OBJNULL;
	tm->tm_nfree = 0;
	tm->tm_nused = 0;
	tm->tm_npage = 0;
	tm->tm_maxpage = maxpage;
	tm->tm_gccount = 0;
}

static int alloc_initialized = FALSE;

void
init_alloc(void)
{
	cl_index i;

	if (alloc_initialized) return;
	alloc_initialized = TRUE;

	holepage = 0;
	new_holepage = HOLEPAGE;

#ifdef USE_MMAP
	real_maxpage = MAXPAGE;
#elif defined(MSDOS) || defined(__CYGWIN__)
	real_maxpage = MAXPAGE;
#elif !defined(HAVE_ULIMIT_H)
	{
	  struct rlimit data_rlimit;
# ifdef __MACH__
	  sbrk(0);
	  getrlimit(RLIMIT_DATA, &data_rlimit);
	  real_maxpage = ((unsigned)get_etext() +
			  (unsigned)data_rlimit.rlim_cur)/LISP_PAGESIZE;
# else
	  extern etext;

	  getrlimit(RLIMIT_DATA, &data_rlimit);
	  real_maxpage = ((unsigned int)&etext +
			  (unsigned)data_rlimit.rlim_cur)/LISP_PAGESIZE;
# endif
	  if (real_maxpage > MAXPAGE) real_maxpage = MAXPAGE;
	}
#else /* HAVE_ULIMIT */
	real_maxpage= ulimit(UL_GMEMLIM)/LISP_PAGESIZE;
	if (real_maxpage > MAXPAGE) real_maxpage = MAXPAGE;
#endif /* USE_MMAP, MSDOS, or HAVE_ULIMIT */

#ifdef USE_MMAP
	heap_start = NULL;
#else
	heap_end = sbrk(0);
	i = ((cl_index)heap_end) % LISP_PAGESIZE;
	if (i)
	  sbrk(LISP_PAGESIZE - i);
	heap_end = heap_start = data_end = sbrk(0);
#endif
	cl_resize_hole(INIT_HOLEPAGE);
	for (i = 0;  i < MAXPAGE;  i++)
		type_map[i] = (char)t_other;

/*	Initialization must be done in increasing size order:	*/
	init_tm(t_singlefloat, "FSINGLE-FLOAT", /* 8 */
		sizeof(struct ecl_singlefloat), 1);
	init_tm(t_cons, ".CONS", sizeof(struct ecl_cons), 384); /* 12 */
	init_tm(t_doublefloat, "LDOUBLE-FLOAT", /* 16 */
		sizeof(struct ecl_doublefloat), 1);
	init_tm(t_bytecodes, "bBYTECODES", sizeof(struct ecl_bytecodes), 64);
	init_tm(t_bytecodes, "bBCLOSURE", sizeof(struct ecl_bclosure), 64);
	init_tm(t_base_string, "\"BASE-STRING", sizeof(struct ecl_base_string), 64); /* 20 */
#ifdef ECL_UNICODE
	init_tm(t_string, "\"STRING", sizeof(struct ecl_string), 64);
#endif
	init_tm(t_array, "aARRAY", sizeof(struct ecl_array), 64); /* 24 */
	init_tm(t_pathname, "pPATHNAME", sizeof(struct ecl_pathname), 1); /* 28 */
	init_tm(t_symbol, "|SYMBOL", sizeof(struct ecl_symbol), 64); /* 32 */
	init_tm(t_package, ":PACKAGE", sizeof(struct ecl_package), 1); /* 36 */
	init_tm(t_codeblock, "#CODEBLOCK", sizeof(struct ecl_codeblock), 1);
	init_tm(t_bignum, "BBIGNUM", sizeof(struct ecl_bignum), 16);
	init_tm(t_ratio, "RRATIO", sizeof(struct ecl_ratio), 1);
	init_tm(t_complex, "CCOMPLEX", sizeof(struct ecl_complex), 1);
	init_tm(t_hashtable, "hHASH-TABLE", sizeof(struct ecl_hashtable), 1);
	init_tm(t_vector, "vVECTOR", sizeof(struct ecl_vector), 2);
	init_tm(t_bitvector, "bBIT-VECTOR", sizeof(struct ecl_vector), 1);
	init_tm(t_stream, "sSTREAM", sizeof(struct ecl_stream), 1);
	init_tm(t_random, "$RANDOM-STATE", sizeof(struct ecl_random), 1);
	init_tm(t_readtable, "rREADTABLE", sizeof(struct ecl_readtable), 1);
	init_tm(t_cfun, "fCFUN", sizeof(struct ecl_cfun), 32);
	init_tm(t_cfunfixed, "fCFUN", sizeof(struct ecl_cfun), 32);
	init_tm(t_cclosure, "cCCLOSURE", sizeof(struct ecl_cclosure), 1);
#ifndef CLOS
	init_tm(t_structure, "SSTRUCTURE", sizeof(struct ecl_structure), 32);
#else
	init_tm(t_instance, "IINSTANCE", sizeof(struct ecl_instance), 32);
#endif /* CLOS */
	init_tm(t_foreign, "LFOREIGN", sizeof(struct ecl_foreign), 1);
#ifdef ECL_THREADS
	init_tm(t_process, "tPROCESS", sizeof(struct ecl_process), 2);
	init_tm(t_lock, "tLOCK", sizeof(struct ecl_lock), 2);
	init_tm(t_condition_variable, "tCONDITION-VARIABLE",
                sizeof(struct ecl_condition_variable), 2);
#endif /* THREADS */
#ifdef ECL_SEMAPHORES
	init_tm(t_semaphore, "tSEMAPHORE",
                sizeof(struct ecl_semaphore), 2);
#endif
#ifdef ECL_LONG_FLOAT
	init_tm(t_longfloat, "tLONGFLOAT", sizeof(struct ecl_long_float), 2);
#endif

	ncb = 0;
	ncbpage = 0;
	maxcbpage = 2048;

#ifdef NEED_MALLOC
	malloc_list = ECL_NIL;
	ecl_register_static_root(&malloc_list);
#endif
}

static int
t_from_type(cl_object type)
{  int t;

   type = cl_string(type);
   for (t = (int)t_start ; t < (int)t_end ; t++) {
     struct typemanager *tm = &tm_table[t];
     if (tm->tm_name &&
	 strncmp((tm->tm_name)+1, type->base_string.self, type->base_string.fillp) == 0)
       return(t);
   }
   FEerror("Unrecognized type", 0);
}

@(defun si::allocate (type qty &optional (now ECL_NIL))
	struct typemanager *tm;
	cl_ptr pp;
	cl_index i;
@
	tm = tm_of(t_from_type(type));
	i = ecl_to_size(qty);
	if (tm->tm_npage > i) i = tm->tm_npage;
	tm->tm_maxpage = i;
	if (now == ECL_NIL || tm->tm_maxpage <= tm->tm_npage)
	  @(return ECL_T)
	if (available_pages() < tm->tm_maxpage - tm->tm_npage ||
	    (pp = alloc_page(tm->tm_maxpage - tm->tm_npage)) == NULL)
	  FEerror("Can't allocate ~D pages for ~A.", 2, type,
		  make_constant_base_string(tm->tm_name+1));
	for (;  tm->tm_npage < tm->tm_maxpage;  pp += LISP_PAGESIZE)
	  add_page_to_freelist(pp, tm);
	@(return ECL_T)
@)

@(defun si::maximum-allocatable-pages (type)
@
	@(return MAKE_FIXNUM(tm_of(t_from_type(type))->tm_maxpage))
@)

@(defun si::allocated-pages (type)
@
	@(return MAKE_FIXNUM(tm_of(t_from_type(type))->tm_npage))
@)

@(defun si::allocate-contiguous-pages (qty &optional (now ECL_NIL))
	cl_index i, m;
	cl_ptr p;
@
	i = ecl_to_size(qty);
	if (ncbpage > i)
	  FEerror("Can't set the limit for contiguous blocks to ~D,~%\
since ~D pages are already allocated.",
			2, qty, MAKE_FIXNUM(ncbpage));
	maxcbpage = i;
	if (Null(now))
	  @(return ECL_T)
	m = maxcbpage - ncbpage;
	if (available_pages() < m || (p = alloc_page(m)) == NULL)
		FEerror("Can't allocate ~D pages for contiguous blocks.",
			1, qty);
	for (i = 0;  i < m;  i++)
		type_map[page(p + LISP_PAGESIZE*i)] = (char)t_contiguous;
	ncbpage += m;
	cl_dealloc(p, LISP_PAGESIZE*m);
	@(return ECL_T)
@)

@(defun si::allocated-contiguous-pages ()
@
	@(return MAKE_FIXNUM(ncbpage))
@)

@(defun si::maximum-contiguous-pages ()
@
	@(return MAKE_FIXNUM(maxcbpage))
@)

@(defun si::get-hole-size ()
@
	@(return MAKE_FIXNUM(new_holepage))
@)

@(defun si::set-hole-size (size)
	cl_index i;
@
	i = ecl_to_size(size);
	if (i == 0 || i > available_pages() + new_holepage)
	  FEerror("Illegal value for the hole size.", 0);
	new_holepage = i;
	@(return size)
@)

@(defun si::ignore-maximum-pages (&optional (flag OBJNULL))
@
	if (flag == OBJNULL)
		@(return (ignore_maximum_pages? ECL_T : ECL_NIL))
	ignore_maximum_pages = Null(flag);
	@(return flag)
@)

#ifdef NEED_MALLOC
/*
	UNIX malloc simulator.

	Used by
		getwd, popen, etc.
*/

#undef malloc
#undef calloc
#undef free
#undef cfree
#undef realloc

void *
malloc(size_t size)
{
  cl_object x;

  if (!GC_enabled() && !alloc_initialized)
    init_alloc();

  x = alloc_simple_base_string(size-1);
  x->base_string.self = (char *)ecl_alloc(size);
  malloc_list = ecl_cons(x, malloc_list);
  return(x->base_string.self);
}

void
free(void *ptr)
{
  cl_object *p;

  if (ptr) {
    for (p = &malloc_list;  !ecl_endp(*p);  p = &(CDR((*p))))
      if ((CAR((*p)))->base_string.self == ptr) {
	cl_dealloc(CAR((*p))->base_string.self, CAR((*p))->base_string.dim+1);
	CAR((*p))->base_string.self = NULL;
	*p = CDR((*p));
	return;
      }
    FEerror("free(3) error.", 0);
  }
}

void *
realloc(void *ptr, size_t size)
{
  cl_object x;
  size_t i, j;

  if (ptr == NULL)
    return malloc(size);
  for (x = malloc_list;  !ecl_endp(x);  x = CDR(x))
    if (CAR(x)->base_string.self == ptr) {
      x = CAR(x);
      if (x->base_string.dim >= size) {
	x->base_string.fillp = size;
	return(ptr);
      } else {
	j = x->base_string.dim;
	x->base_string.self = (char *)ecl_alloc(size);
	x->base_string.fillp = x->base_string.dim = size;
	memcpy(x->base_string.self, ptr, j);
	cl_dealloc(ptr, j);
	return(x->base_string.self);
      }
    }
  FEerror("realloc(3) error.", 0);
}

void *
calloc(size_t nelem, size_t elsize)
{
  char *ptr;
  size_t i = nelem*elsize;
  ptr = malloc(i);
  memset(ptr, 0 , i);
  return(ptr);
}

void cfree(void *ptr)
{
  free(ptr);
}

/* make f allocate enough extra, so that we can round
   up, the address given to an even multiple.   Special
   case of size == 0 , in which case we just want an aligned
   number in the address range
   */

#define ALLOC_ALIGNED(f, size, align) \
	((align) <= 4 ? (int)(f)(size) : \
	   ((align) * (((unsigned)(f)(size + (size ? (align) - 1 : 0)) + (align) - 1)/(align))))

void *
memalign(size_t align, size_t size)
{ cl_object x = alloc_simple_base_string(size);
  malloc_list = ecl_cons(x, malloc_list);
  return x->base_string.self;
}

# ifdef WANT_VALLOC
char *
valloc(size_t size)
{ return memalign(getpagesize(), size);}
# endif /* WANT_VALLOC */
#endif /* NEED_MALLOC */
