/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    gbc.c -- Garbage collector.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi and William F. Schelter.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


#include "ecl.h"
#include "page.h"

/******************************* EXPORTS ******************************/

bool GC_enable;
int gc_time;			/* Beppe */

/******************************* ------- ******************************/

/*
   mark_table[m]:i represents word w = 128*m + 4*i, where m = addr-DATA_START.
   Therefore m = w >> 7, i = (w / 4) % 32 = (w >> 2) & 0x1f.
*/

static int	*mark_table;

static void inline
set_mark_bit(void *x) {
	int w = (int)x;
	int m = (w - DATA_START) >> 7;
	int i = (w >> 2) & 0x1f;
	mark_table[m] |= (1 << i);
}
static int inline
get_mark_bit(void *x) {
	int w = (int)x;
	int m = (w - DATA_START) >> 7;
	int i = (w >> 2) & 0x1f;
	return (mark_table[m] >> i) & 1;
}

#define	inheap(pp)	((unsigned long)(pp) < (unsigned long)heap_end)
#define VALID_DATA_ADDRESS(pp) \
  !ECL_IMMEDIATE(pp) && (cl_index)DATA_START <= (cl_index)(pp) && (cl_index)(pp) < (cl_index)heap_end

cl_object siVgc_verbose;
cl_object siVgc_message;

static bool	debug = FALSE;
static int	maxpage;

#define	GC_ROOT_MAX		200
static cl_object	*gc_root[GC_ROOT_MAX];
static int	gc_roots;

static bool	collect_blocks;

/*
   We must register location, since value may be reassigned (e.g. malloc_list)
 */

static void _mark_object (cl_object x);
static void _mark_contblock (void *p, size_t s);
extern void sigint (void);

void
register_root(cl_object *p)
{
	if (gc_roots >= GC_ROOT_MAX)
		error("too many roots");
	gc_root[gc_roots++] = p;
}

@(defun gc (area)
@
	if (!GC_enabled())
		error("GC is not enabled");
	if (Null(area))
		gc(t_cons);
	else
		gc(t_contiguous);
	@(return)
@)

/*----------------------------------------------------------------------
 * Mark phase
 *----------------------------------------------------------------------
 */

/* Whenever two arrays are linked together by displacement,
   if one is live, the other will be made live */
#define mark_displaced(ar) mark_object(ar)
#define mark_contblock(x,s) {if (collect_blocks) _mark_contblock(x,s); }
#if 1
#define mark_object(x) if ((x != OBJNULL) && !ECL_IMMEDIATE(x)) _mark_object(x)
#define mark_next(a) if ((a != OBJNULL) && !ECL_IMMEDIATE(a)) { x=(a); goto BEGIN; }
#else
#define mark_object(x) _mark_object(x)
#define mark_next(a) x=(a); goto BEGIN
#endif

/* We make bitvectors multiple of sizeof(int) in size allocated
 Assume 8 = number of bits in char */
#define W_SIZE (8*sizeof(int))

static void
_mark_object(cl_object x)
{
	size_t i, j;
	cl_object *p, y;
	char *cp;

	cs_check(x);
BEGIN:
#if 0
	/* We cannot get here because mark_object() and mark_next() already check this */
	if (ECL_IMMEDIATE(x)) return;	/* fixnum, character or locative */
	if (x == OBJNULL)
		return;
#endif
	if (get_mark_bit(x))
		return;
	set_mark_bit(x);

	switch (ecl_t_of(x)) {

	case t_bignum:
#ifdef WITH_GMP
		if (collect_blocks) {
		  /* GMP may set num.alloc before actually allocating anything.
		     With these checks we make sure we do not move anything
		     we don't have to. Besides, we use big_dim as the size
		     of the object, because big_size might even be smaller.
		  */
		  char *limbs = (char *)x->big.big_limbs;
		  size_t size = x->big.big_dim * sizeof(mp_limb_t);
		  if (size) mark_contblock(limbs, size);
		}
#endif /* WITH_GMP */
		break;

	case t_ratio:
		mark_object(x->ratio.num);
		mark_next(x->ratio.den);
		break;

#ifdef ECL_SSE2
	case t_sse_pack:
#endif
	case t_singlefloat:
	case t_doublefloat:
		break;

	case t_complex:
		mark_object(x->complex.imag);
		mark_next(x->complex.real);
		break;

	case t_character:
		break;

	case t_symbol:
		mark_object(x->symbol.name);
		mark_object(x->symbol.plist);
		mark_object(ECL_SYM_FUN(x));
		mark_next(SYM_VAL(x));
		break;

	case t_package:
		mark_object(x->pack.name);
		mark_object(x->pack.nicknames);
		mark_object(x->pack.shadowings);
		mark_object(x->pack.uses);
		mark_object(x->pack.usedby);
		mark_object(x->pack.internal);
		mark_next(x->pack.external);
		break;

	case t_cons:
		mark_object(CAR(x));
		mark_next(CDR(x));
		break;

	case t_hashtable:
		mark_object(x->hash.rehash_size);
		mark_object(x->hash.threshold);
		if (x->hash.data == NULL)
			break;
		for (i = 0, j = x->hash.size;  i < j;  i++) {
			mark_object(x->hash.data[i].key);
			mark_object(x->hash.data[i].value);
		}
		mark_contblock(x->hash.data, j * sizeof(struct hashtable_entry));
		break;

	case t_array:
		mark_contblock(x->array.dims, sizeof(x->array.dims[0])*x->array.rank);
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_vector:
		if ((y = x->array.displaced) != ECL_NIL)
			mark_displaced(y);
		cp = (char *)x->array.self.t;
		if (cp == NULL)
			break;
		switch ((enum aelttype)x->array.elttype) {
#ifdef ECL_UNICODE
		case ecl_aet_ch:
#endif
		case ecl_aet_object:
			if (x->array.displaced == ECL_NIL || CAR(x->array.displaced) == ECL_NIL) {
				cl_object *p = x->array.self.t;
				cl_index i;
				if (x->array.t == t_vector && x->vector.hasfillp)
					i = x->vector.fillp;
				else
					i = x->vector.dim;
				while (i-- > 0)
					mark_object(p[i]);
			}
			j = sizeof(cl_object)*x->array.dim;
			break;
		case ecl_aet_bc:
			j = x->array.dim;
			break;
		case ecl_aet_bit:
			j = sizeof(int) * ((x->vector.offset + x->vector.dim + W_SIZE -1)/W_SIZE);
			break;
		case ecl_aet_fix:
			j = x->array.dim * sizeof(cl_fixnum);
			break;
		case ecl_aet_sf:
			j = x->array.dim * sizeof(float);
			break;
		case ecl_aet_df:
			j = x->array.dim * sizeof(double);
			break;
		default:
			error("Allocation botch: unknown array element type");
		}
		goto COPY_ARRAY;
	case t_base_string:
		if ((y = x->base_string.displaced) != ECL_NIL)
			mark_displaced(y);
		cp = x->base_string.self;
		if (cp == NULL)
			break;
		j = x->base_string.dim;
	COPY_ARRAY:
		mark_contblock(cp, j);
		break;
	case t_bitvector:
		if ((y = x->vector.displaced) != ECL_NIL)
			mark_displaced(y);
		cp = x->vector.self.bit;
		if (cp == NULL)
			break;
		j= sizeof(int) * ((x->vector.offset + x->vector.dim + W_SIZE -1)/W_SIZE);
		goto COPY_ARRAY;

#ifndef CLOS
	case t_structure:
		mark_object(x->str.name);
		p = x->str.self;
		if (p == NULL)
			break;
		for (i = 0, j = x->str.length;  i < j;  i++)
			mark_object(p[i]);
		mark_contblock(p, j*sizeof(cl_object));
		break;
#endif CLOS

	case t_stream:
		switch ((enum smmode)x->stream.mode) {
		case ecl_smm_closed:
			/* Rest of fields are NULL */
			mark_next(x->stream.object1);
			break;
		case ecl_smm_input:
		case ecl_smm_output:
		case ecl_smm_io:
		case ecl_smm_probe:
			mark_object(x->stream.object0);
			mark_object(x->stream.object1);
			mark_contblock(x->stream.buffer, BUFSIZ);
			break;

		case ecl_smm_synonym:
			mark_next(x->stream.object0);
			break;

		case ecl_smm_broadcast:
		case ecl_smm_concatenated:
			mark_next(x->stream.object0);
			break;

		case ecl_smm_two_way:
		case ecl_smm_echo:
			mark_object(x->stream.object0);
			mark_next(x->stream.object1);
			break;

		case ecl_smm_string_input:
		case ecl_smm_string_output:
			mark_next(x->stream.object0);
			break;

		default:
			error("mark stream botch");
		}
		break;

	case t_random:
		break;

	case t_readtable:
		if (x->readtable.table == NULL)
			break;
		mark_contblock((char *)(x->readtable.table), RTABSIZE*sizeof(struct readtable_entry));
		for (i = 0;  i < RTABSIZE;  i++) {
			cl_object *p = x->readtable.table[i].dispatch_table;
			mark_object(x->readtable.table[i].macro);
			if (p != NULL) {
			  mark_contblock(p, RTABSIZE*sizeof(cl_object));
			  for (j = 0;  j < RTABSIZE;  j++)
			    mark_object(p[j]);
			}
		}
		break;

	case t_pathname:
		mark_object(x->pathname.host);
		mark_object(x->pathname.device);
		mark_object(x->pathname.directory);
		mark_object(x->pathname.name);
		mark_object(x->pathname.type);
		mark_object(x->pathname.version);
		break;

	case t_bytecodes: {
		cl_index i, size;
		size = x->bytecodes.size;
		mark_object(x->bytecodes.lex);
		mark_contblock(x->bytecodes.data, size * sizeof(cl_object));
		for (i=0; i<size; i++)
			mark_object(x->bytecodes.data[i]);
		break;
	}
	case t_cfun:
		mark_object(x->cfun.block);
		mark_object(x->cfun.name);
		break;

	case t_cclosure:
		mark_object(x->cfun.block);
		mark_object(x->cclosure.env);
		break;

#ifdef THREADS
	case t_cont:
		mark_next(x->cn.cn_thread);
		break;

	case t_thread:
/* Already marked by malloc
 		mark_contblock(x->thread.data, x->thread.size);
 */
		mark_next(x->thread.entry);
		break;
#endif THREADS
#ifdef CLOS
	case t_instance:
		mark_object(x->instance.class);
		p = x->instance.slots;
		if (p == NULL)
			break;
		for (i = 0, j = x->instance.length;  i < j;  i++)
			mark_object(p[i]);
		mark_contblock(p, j*sizeof(cl_object));
		break;

	case t_gfun:
		mark_object(x->gfun.name);
		mark_object(x->gfun.method_hash);
		mark_object(x->gfun.instance);
		p = x->gfun.specializers;
		if (p == NULL)
			break;
		for (i = 0, j = x->gfun.arg_no;  i < j;  i++)
			mark_object(p[i]);
		mark_contblock(p, j*sizeof(cl_object));
		break;
#endif CLOS
	case t_codeblock:
		mark_object(x->cblock.name);
		mark_contblock(x->cblock.start, x->cblock.size);
		if (x->cblock.data) {
			cl_index i = x->cblock.data_size;
			cl_object *p = x->cblock.data;
			while (i--)
				mark_object(p[i]);
		}
		break;
	default:
		if (debug)
			printf("\ttype = %d\n", ecl_t_of(x));
		error("mark botch");
	}
}

static void
mark_stack_conservative(int *top, int *bottom)
{
  int p, m;
  cl_object x;
  struct typemanager *tm;
  register int *j;

  if (debug) { printf("Traversing C stack .."); fflush(stdout); }

  /* On machines which align local pointers on multiple of 2 rather
     than 4 we need to mark twice

  if (offset) mark_stack_conservative(bottom, ((char *) top) + offset, 0);
     */
  for (j = top ; j >= bottom ; j--) {
    /* improved Beppe: */
    if (VALID_DATA_ADDRESS(*j) && type_map[p = page(*j)] < (char)t_end) {
      tm = tm_of((enum type)type_map[p]);
      x = (cl_object)(*j - (*j - (int)pagetochar(p)) % tm->tm_size);
      if (!get_mark_bit(x))
	  mark_object(x);
    }
  }
  if (debug) {printf(". done.\n"); fflush(stdout); }
}

static void
mark_phase(void)
{
	register int i;
	register struct package *pp;
	register ecl_bds_ptr bdp;
	register ecl_frame_ptr frp;
	register ecl_ihs_ptr ihsp;

	mark_object(ECL_NIL);
	mark_object(ECL_T);

#ifdef THREADS
	{
	  pd *pdp;
	  lpd *old_clwp = clwp;

	  for (pdp = running_head; pdp != (pd *)NULL; pdp = pdp->pd_next) {

	    clwp = pdp->pd_lpd;
#endif THREADS
	    
	    for (i=0; i<NValues; i++)
	      mark_object(VALUES(i));

	    for (bdp = bds_org;  bdp <= bds_top;  bdp++) {
	      mark_object(bdp->bds_sym);
	      mark_object(bdp->bds_val);
	    }
	    
	    for (frp = frs_org;  frp <= frs_top;  frp++) {
	      mark_object(frp->frs_val);
	      mark_object(frp->frs_lex);
	    }
	    
	    for (ihsp = ihs_org;  ihsp <= ihs_top;  ihsp++) {
	      mark_object(ihsp->ihs_function);
	      mark_object(ihsp->ihs_base);
	    }

	    mark_object(lex_env);

#ifdef THREADS	      
	    /* added to mark newly allocated objects */
	    mark_object(clwp->lwp_alloc_temporary);
	    mark_object(clwp->lwp_fmt_temporary_stream);
	    mark_object(clwp->lwp_PRINTstream);
	    mark_object(clwp->lwp_PRINTcase);
	    mark_object(clwp->lwp_READtable);
	    mark_object(clwp->lwp_delimiting_char);
	    mark_object(clwp->lwp_token);

	    /* (current-thread) can return it at any time
	     */
	    mark_object(clwp->lwp_thread);
#endif THREADS	      
	    
	    /* now collect from the c-stack of the thread ... */
	    
	    { int *where;
	      volatile jmp_buf buf;

	      /* ensure flushing of register caches */
	      if (ecl_setjmp(buf) == 0) ecl_longjmp(buf, 1);

#ifdef THREADS
	      if (clwp != old_clwp) /* is not the executing stack */
# ifdef __linux
		where = (int *)pdp->pd_env[0].__jmpbuf[0].__sp;
# else
		where = (int *)pdp->pd_env[JB_SP];
# endif
	      else
#endif THREADS
		where = (int *)&where ;
	      
	      /* If the locals of type object in a C function could be
		 aligned other than on multiples of sizeof (char *)
		 we would have to mark twice */
	      
	      if (where > cs_org)
		mark_stack_conservative(where, cs_org);
	      else
		mark_stack_conservative(cs_org, where);
	    }
#ifdef THREADS
	  }
	  clwp = old_clwp;
	}
#endif THREADS

	/* mark roots */
	for (i = 0; i < gc_roots;  i++)
		mark_object(*gc_root[i]);

	/* mark registered symbols & keywords */
	{
	const struct keyword_info *k;
	const struct symbol_info *s;
	for (k = all_keywords; k->loc != NULL; k++)
		mark_object(*(k->loc));
	for (s = all_symbols; s->loc != NULL; s++)
		mark_object(*(s->loc));
	}

	if (debug) {
		printf("symbol navigation\n");
		fflush(stdout);
	}
}

static void
sweep_phase(void)
{
	register int i, j, k;
	register cl_object x;
	register char *p;
	register struct typemanager *tm;
	register cl_object f;

	ECL_NIL->symbol.m = FALSE;
	ECL_T->symbol.m = FALSE;

	if (debug)
		printf("type map\n");

	for (i = 0;  i < maxpage;  i++) {
		if (type_map[i] == (int)t_contiguous) {
			if (debug) {
				printf("-");
				continue;
			}
		}
		if (type_map[i] >= (int)t_end)
			continue;

		tm = tm_of((enum type)type_map[i]);

	/*
		general sweeper
	*/

		if (debug)
			printf("%c", tm->tm_name[0]);

		p = pagetochar(i);
		f = tm->tm_free;
		k = 0;
		for (j = tm->tm_nppage; j > 0; --j, p += tm->tm_size) {
			x = (cl_object)p;
			if (!get_mark_bit(x)) {
			  ((struct freelist *)x)->f_link = f;
			  f = x;
			  k++;
			}
		}
		tm->tm_free = f;
		tm->tm_nfree += k;
		tm->tm_nused -= k;
	}

	if (debug) {
		putchar('\n');
		fflush(stdout);
	}
}

static void
contblock_sweep_phase(void)
{
	register int i, j;
	register char *s, *e, *p, *q;
	register struct contblock *cbp;

	cb_pointer = NULL;
	ncb = 0;
	for (i = 0;  i < maxpage;) {
		if (type_map[i] != (int)t_contiguous) {
			i++;
			continue;
		}
		for (j = i+1;
		     j < maxpage && type_map[j] == (int)t_contiguous;
		     j++)
			;	
		s = pagetochar(i);
		e = pagetochar(j);
		for (p = s;  p < e;) {
			if (get_mark_bit((int *)p)) {
				p += 4;
				continue;
			}
			q = p + 4;
			while (q < e && !get_mark_bit((int *)q))
				q += 4;
			dealloc(p, q - p);
			p = q + 4;
		}
		i = j + 1;
	}

	if (debug) {
		for (cbp = cb_pointer; cbp != NULL; cbp = cbp->cb_link)
			printf("0x%p %d\n", cbp, cbp->cb_size);
		fflush(stdout);
	}
}

cl_object (*GC_enter_hook)() = NULL;
cl_object (*GC_exit_hook)() = NULL;


#ifdef THREADS
/* 
 * We execute the GC routine in the main stack.
 * The idea is to switch over the main stack that is stopped in the intha
 * and to call the GC from there on garbage_parameter. Then you can switch 
 * back after.
 * In addition the interrupt is disabled.
 */
static int i, j;
static sigjmp_buf old_env;
static int val;
static lpd *old_clwp;
static enum type t;
static bool stack_switched = FALSE;

static enum type garbage_parameter;

void
gc(enum type new_name)
{
	int tm;
	int gc_start = runtime();

	start_critical_section();
	t = new_name;
        garbage_parameter = new_name;
#else

void
gc(enum type t)
{
  int i, j;
  int tm;
  int gc_start = runtime();
#endif THREADS

  if (!GC_enabled())
    return;

  if (SYM_VAL(siVgc_verbose) != ECL_NIL) {
    printf("\n[GC ..");
    /* To use this should add entries in tm_table for reloc and contig.
       fprintf(stdout, "\n[GC for %d %s pages ..",
       tm_of(t)->tm_npage,
       tm_table[(int)t].tm_name + 1); */
    fflush(stdout);
  }

  debug = symbol_value(siVgc_message) != ECL_NIL;

#ifdef THREADS
  if (clwp != &main_lpd)  {
    if (debug) {
      printf("*STACK SWITCH*\n");
      fflush (stdout);
    }

    stack_switched = TRUE;
    val = sigsetjmp(old_env, 1);
    if (val == 0) {
      /* informations used by the garbage collector need to be updated */
# ifdef __linux
      running_head->pd_env[0].__jmpbuf[0].__sp = old_env[0].__jmpbuf[0].__sp;
# else
      running_head->pd_env[JB_SP] = old_env[JB_SP];
# endif
      old_clwp = clwp;
      Values = main_lpd.lwp_Values;
      clwp = &main_lpd;
      siglongjmp(main_pd.pd_env, 2); /* new line */
    }
  }

  else val = 1;

  if (val == 1) {

#endif THREADS

    if (GC_enter_hook != NULL)
      (*GC_enter_hook)(0);

    interrupt_enable = FALSE;

    collect_blocks = t > t_end;
    if (collect_blocks)
      cbgccount++;
    else
      tm_table[(int)t].tm_gccount++;

    if (debug) {
      if (collect_blocks)
	printf("GC entered for collecting blocks\n");
      else
	printf("GC entered for collecting %s\n", tm_table[(int)t].tm_name);
      fflush(stdout);
    }

    maxpage = page(heap_end);

    if (collect_blocks) {
      /*
	1 page = 512 word
	512 bit = 16 word
      */
      int mark_table_size = maxpage * (LISP_PAGESIZE / 32);
      extern void resize_hole(size_t);

      if (holepage < mark_table_size*sizeof(int)/LISP_PAGESIZE + 1)
	new_holepage = mark_table_size*sizeof(int)/LISP_PAGESIZE + 1;
      if (new_holepage < HOLEPAGE)
	new_holepage = HOLEPAGE;
      resize_hole(new_holepage);

      mark_table = (int*)heap_end;
      for (i = 0;  i < mark_table_size; i++)
	mark_table[i] = 0;
    }

    if (debug) {
      printf("mark phase\n");
      fflush(stdout);
      tm = runtime();
    }
    mark_phase();
    if (debug) {
      printf("mark ended (%d)\n", runtime() - tm);
      printf("sweep phase\n");
      fflush(stdout);
      tm = runtime();
    }
    sweep_phase();
    if (debug) {
      printf("sweep ended (%d)\n", runtime() - tm);
      fflush(stdout);
    }

    if (t == t_contiguous) {
      if (debug) {
	printf("contblock sweep phase\n");
	fflush(stdout);
	tm = runtime();
      }
      contblock_sweep_phase();
      if (debug)
	printf("contblock sweep ended (%d)\n", runtime() - tm);
    }

    if (debug) {
      for (i = 0, j = 0;  i < (int)t_end;  i++) {
	if (tm_table[i].tm_type == (enum type)i) {
	  printf("%13s: %8d used %8d free %4d/%d pages\n",
		 tm_table[i].tm_name,
		 tm_table[i].tm_nused,
		 tm_table[i].tm_nfree,
		 tm_table[i].tm_npage,
		 tm_table[i].tm_maxpage);
	  j += tm_table[i].tm_npage;
	} else
	  printf("%13s: linked to %s\n",
		 tm_table[i].tm_name,
		 tm_table[(int)tm_table[i].tm_type].tm_name);
      }
      printf("contblock: %d blocks %d pages\n", ncb, ncbpage);
      printf("hole: %d pages\n", holepage);
      printf("GC ended\n");
      fflush(stdout);
    }

    interrupt_enable = TRUE;

    if (GC_exit_hook != NULL)
      (*GC_exit_hook)();

#ifdef THREADS

    /* 
     * Back in the right stack
     */

    if (stack_switched) {
      if (debug) {
	printf("*STACK BACK*\n");
	fflush (stdout);
      }

      stack_switched = FALSE;

      end_critical_section();	/* we get here from the GC call in scheduler */
	  
      clwp = old_clwp;
      Values = clwp->lwp_Values;
      siglongjmp(old_env, 2);
    }
  }
#endif THREADS

  gc_time += (gc_start = runtime() - gc_start);

  if (SYM_VAL(siVgc_verbose) != ECL_NIL) {
    /* Don't use fprintf since on Linux it calls malloc() */
    printf(". finished in %.2f\"]", gc_start/60.0);
    fflush(stdout);
  }

#ifdef unix
  if (interrupt_flag) sigint();
#endif unix

#ifdef THREADS
  end_critical_section();
#endif THREADS
}

/*
 *----------------------------------------------------------------------
 *
 * mark_contblock --
 *     sets the mark bit for words from address p to address p+s.
 *     Both p and p+s are rounded to word boundaries.
 *
 * Results:
 *	none.
 *
 * Side effects:
 *	mark_table
 *
 *----------------------------------------------------------------------
 */

static void
_mark_contblock(void *x, size_t s)
{
	register char *p = x, *q;
	register ptrdiff_t pg = page(p);

	if (pg < 0 || (enum type)type_map[pg] != t_contiguous)
		return;
#if 1
	q = p + s;
	p = (char *)((int)p&~3);
	q = (char *)(((int)q+3)&~3);
	for (;  p < q;  p+= 4)
	  set_mark_bit(p);
#elif 0
	{
	int bit_start = ((int)p - DATA_START) >> 2;
	int bit_end = ((int)p + s + 3 - DATA_START) >> 2;
	int *w = &mark_table[bit_start >> 5];
	int b = bit_start & (32 - 1);
	int mask = ~0 << b;
	int bits = b + bit_end - bit_start;
	while (bits >= 32) {
	  *w |= mask;
	  w++;
	  bits -= 32;
	  mask = ~0;
	}
	mask &= ~(~0 << bits);
	*w |= mask;
	}
#else
	{
	int bit_start = ((int)p - DATA_START) >> 2;
	int bits = ((int)p + s + 3 - DATA_START) >> 2 - bit_start;
	int mask = 1 << bit_start & (32 - 1);
	int *w = &mark_table[bit_start >> 5];
	while (bits) {
	  *w |= mask;
	  mask <<= 1;
	  if (!mask) {
	    mask = 1;
	    w++;
	  }
	}
	}
#endif
}

/*----------------------------------------------------------------------
 * Utilities
 *----------------------------------------------------------------------
 */

@(defun si::room-report ()
	int i;
	cl_object *tl;
@
	NValues = 8;
	VALUES(0) = ecl_make_fixnum(real_maxpage);
	VALUES(1) = ecl_make_fixnum(available_pages());
	VALUES(2) = ecl_make_fixnum(ncbpage);
	VALUES(3) = ecl_make_fixnum(maxcbpage);
	VALUES(4) = ecl_make_fixnum(ncb);
	VALUES(5) = ecl_make_fixnum(cbgccount);
	VALUES(6) = ecl_make_fixnum(holepage);
	VALUES(7) = ECL_NIL;
	tl = &VALUES(7);
	for (i = 0;  i < (int)t_end;  i++) {
	  if (tm_table[i].tm_type == (enum type)i) {
	    tl = &CDR(*tl = CONS(ecl_make_fixnum(tm_table[i].tm_nused), ECL_NIL));
	    tl = &CDR(*tl = CONS(ecl_make_fixnum(tm_table[i].tm_nfree), ECL_NIL));
	    tl = &CDR(*tl = CONS(ecl_make_fixnum(tm_table[i].tm_npage), ECL_NIL));
	    tl = &CDR(*tl = CONS(ecl_make_fixnum(tm_table[i].tm_maxpage), ECL_NIL));
	    tl = &CDR(*tl = CONS(ecl_make_fixnum(tm_table[i].tm_gccount), ECL_NIL));
	  } else {
	    tl = &CDR(*tl = CONS(ECL_NIL, ECL_NIL));
	    tl = &CDR(*tl = CONS(ecl_make_fixnum(tm_table[i].tm_type), ECL_NIL));
	    tl = &CDR(*tl = CONS(ECL_NIL, ECL_NIL));
	    tl = &CDR(*tl = CONS(ECL_NIL, ECL_NIL));
	    tl = &CDR(*tl = CONS(ECL_NIL, ECL_NIL));
	  }
	}
	return VALUES(0);
@)

@(defun si::reset-gc-count ()
	int i;
@
	cbgccount = 0;
	for (i = 0;  i < (int)t_end;  i++)
		tm_table[i].tm_gccount = 0;
	@(return)
@)

@(defun si::gc-time ()
@
	@(return ecl_make_fixnum(gc_time))
@)

void
init_GC(void)
{
	register_root(&siVgc_verbose);
	register_root(&siVgc_message);
	siVgc_verbose = make_si_special("*GC-VERBOSE*", ECL_NIL);
	siVgc_message = make_si_special("*GC-MESSAGE*", ECL_NIL);
	GC_enable();
	gc_time = 0;
}
