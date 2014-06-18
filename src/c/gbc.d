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

#ifdef ECL_THREADS
#include <pthread.h>
#endif
#include <stdio.h>
#include <ecl/ecl.h>
#include <ecl/page.h>
#include <ecl/internal.h>
#include <ecl/bytecodes.h>

/******************************* EXPORTS ******************************/

bool GC_enable;

/******************************* ------- ******************************/

/*
   mark_table[m]:i represents word w = 128*m + 4*i, where m = addr-DATA_START.
   Therefore m = w >> 7, i = (w / 4) % 32 = (w >> 2) & 0x1f.
*/

static int	*mark_table;

#define MTbit(x)	((ptr2int(x) >> 2) & 0x1f)
#define MTword(x)	mark_table[((cl_ptr)x - heap_start) >> 7]
#define get_mark_bit(x) (MTword(x) >> MTbit(x) & 1)
#define set_mark_bit(x) (MTword(x) |= (1 << MTbit(x)))
#define clear_mark_bit(x) (MTword(x) ~= (~1 << MTbit(x)))

#define VALID_DATA_ADDRESS(pp) \
  (!ECL_IMMEDIATE(pp) && (heap_start <= (cl_ptr)(pp)) && ((cl_ptr)(pp) < heap_end))

static bool	debug = FALSE;
static int	maxpage;

#define	GC_ROOT_MAX		200
static cl_object	*gc_root[GC_ROOT_MAX];
static int	gc_roots;

static bool	collect_blocks;

static int gc_time;			/* Beppe */

/*
   We must register location, since value may be reassigned (e.g. malloc_list)
 */

static void _mark_object(cl_object x);
static void _mark_contblock(void *p, cl_index s);
static void mark_cl_env(struct cl_env_struct *env);
extern void sigint (void);

void
ecl_register_root(cl_object *p)
{
	if (gc_roots >= GC_ROOT_MAX)
		ecl_internal_error("too many roots");
	gc_root[gc_roots++] = p;
}

cl_object
si_gc(cl_object area)
{
	if (!GC_enabled())
		ecl_internal_error("GC is not enabled");
	if (Null(area))
		ecl_gc(t_cons);
	else
		ecl_gc(t_contiguous);
	@(return)
}

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
	cl_index i, j;
	cl_object *p, y;
	cl_ptr cp;
BEGIN:
#if 0
	/* We cannot get here because mark_object() and mark_next() already check this */
	if (ECL_IMMEDIATE(x)) return;	/* fixnum, character or locative */
	if (x == OBJNULL)
		return;
#endif
	/* We need this, because sometimes we arrive to data structures
	 * which have been created in the C stack (t_frame in gfun.d,
	 * for instance) */
	if (!VALID_DATA_ADDRESS(x))
		return;
	if (x->d.m) {
		if (x->d.m == FREE)
			ecl_internal_error("mark_object: pointer to free object.");
		else
			return;
	}
	x->d.m = TRUE;

	switch (ecl_t_of(x)) {

	case t_bignum: {
		/* GMP may set num.alloc before actually allocating anything.
		   With these checks we make sure we do not move anything
		   we don't have to. Besides, we use big_dim as the size
		   of the object, because big_size might even be smaller.
		*/
		cl_ptr limbs = (cl_ptr)x->big.big_limbs;
		cl_index size = x->big.big_dim * sizeof(mp_limb_t);
		if (size) mark_contblock(limbs, size);
		break;
	}
	case t_ratio:
		mark_object(x->ratio.num);
		mark_next(x->ratio.den);
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
		mark_object(x->complex.imag);
		mark_next(x->complex.real);
		break;

	case t_character:
		break;

	case t_symbol:
		mark_object(x->symbol.hpack);
		mark_object(x->symbol.name);
		mark_object(x->symbol.plist);
		mark_object(x->symbol.gfdef);
		mark_next(x->symbol.value);
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
		mark_contblock(x->hash.data, j * sizeof(struct ecl_hashtable_entry));
		break;

	case t_array:
		mark_contblock(x->array.dims, sizeof(x->array.dims[0])*x->array.rank);
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_vector:
		if ((y = x->array.displaced) != ECL_NIL)
			mark_displaced(y);
		cp = (cl_ptr)x->array.self.t;
		if (cp == NULL)
			break;
		switch ((cl_elttype)x->array.elttype) {
#ifdef ECL_UNICODE
		case ecl_aet_ch:
#endif
		case ecl_aet_object:
			if (x->array.displaced == ECL_NIL || CAR(x->array.displaced) == ECL_NIL) {
				i = x->vector.dim;
				p = x->array.self.t;
				goto MARK_DATA;
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
		case ecl_aet_index:
			j = x->array.dim * sizeof(cl_index);
			break;
		case ecl_aet_sf:
			j = x->array.dim * sizeof(float);
			break;
		case ecl_aet_df:
			j = x->array.dim * sizeof(double);
			break;
		case ecl_aet_b8:
			j = x->array.dim * sizeof(uint8_t);
			break;
		case ecl_aet_i8:
			j = x->array.dim * sizeof(int8_t);
			break;
		default:
			ecl_internal_error("Allocation botch: unknown array element type");
		}
		goto COPY_ARRAY;
	case t_base_string:
		if ((y = x->base_string.displaced) != ECL_NIL)
			mark_displaced(y);
		cp = x->base_string.self;
		if (cp == NULL)
			break;
		j = x->base_string.dim+1;
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
		i = x->str.length;
		goto MARK_DATA;
#endif /* CLOS */

	case t_stream:
		switch ((enum ecl_smmode)x->stream.mode) {
		case ecl_smm_input:
		case ecl_smm_output:
		case ecl_smm_io:
		case ecl_smm_probe:
			mark_contblock(x->stream.buffer, BUFSIZ);
			mark_object(x->stream.object0);
			mark_next(x->stream.object1);
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
			ecl_internal_error("mark stream botch");
		}
		break;

	case t_random:
		break;

	case t_readtable:
		if (x->readtable.table == NULL)
			break;
		mark_contblock((cl_ptr)(x->readtable.table),
			       RTABSIZE*sizeof(struct ecl_readtable_entry));
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
		mark_object(x->pathname.version);
		mark_object(x->pathname.name);
		mark_object(x->pathname.type);
		mark_next(x->pathname.directory);
		break;

	case t_bytecodes:
		mark_object(x->bytecodes.name);
		mark_object(x->bytecodes.lex);
		mark_object(x->bytecodes.specials);
		mark_object(x->bytecodes.definition);
		mark_contblock(x->bytecodes.code, x->bytecodes.code_size * sizeof(cl_opcode));
		mark_next(x->bytecodes.data);
		break;

	case t_bclosure:
		mark_object(x->bclosure.code);
		mark_next(x->bclosure.lex);
		break;

	case t_cfun:
	case t_cfunfixed:
		mark_object(x->cfun.block);
		mark_next(x->cfun.name);
		break;

	case t_cclosure:
		mark_object(x->cfun.block);
		mark_next(x->cclosure.env);
		break;

#ifdef ECL_THREADS
	case t_process:
/* Already marked by malloc: x->process.env
 */
		mark_object(x->process.name);
		mark_object(x->process.interrupt);
		mark_object(x->process.function);
		mark_cl_env(x->process.env);
		mark_next(x->process.args);
		break;
	case t_lock:
		mark_next(x->lock.name);
		mark_next(x->lock.holder);
		break;
	case t_condition_variable:
                break;
#endif /* THREADS */
#ifdef ECL_SEMAPHORES
	case t_semaphore:
                break;
#endif
#ifdef CLOS
	case t_instance:
		mark_object(x->instance.clas);
		mark_object(x->instance.sig);
		p = x->instance.slots;
		i = x->instance.length;
		goto MARK_DATA;
#endif /* CLOS */
	case t_codeblock:
		mark_object(x->cblock.name);
		mark_object(x->cblock.next);
		mark_object(x->cblock.links);
		p = x->cblock.temp_data;
		if (p) {
			i = x->cblock.temp_data_size;
			mark_contblock(p, i * sizeof(cl_object));
			while (i-- > 0)
				mark_object(p[i]);
		}
		i = x->cblock.data_size;
		p = x->cblock.data;
		goto MARK_DATA;
	case t_foreign:
		if (x->foreign.size)
			mark_contblock(x->foreign.data, x->foreign.size);
		mark_next(x->foreign.tag);
		break;
	MARK_DATA:
		if (p) {
			mark_contblock(p, i * sizeof(cl_object));
			while (i-- > 0)
				mark_object(p[i]);
		}
		return;
	default:
		if (debug)
			printf("\ttype = %d\n", ecl_t_of(x));
		ecl_internal_error("mark botch");
	}
}

static void
mark_stack_conservative(cl_ptr bottom, cl_ptr top)
{
	int p, m;
	cl_object x;
	struct typemanager *tm;
	cl_ptr j;

	if (debug) { printf("Traversing C stack .."); fflush(stdout); }

	/* On machines which align local pointers on multiple of 2 rather
	   than 4 we need to mark twice

	   if (offset) mark_stack_conservative(bottom, ((char *) top) + offset, 0);
	*/
	for (j = bottom ; j < top ; j+=sizeof(cl_ptr)) {
		cl_ptr aux = *((cl_ptr*)j);
		/* improved Beppe: */
		if (VALID_DATA_ADDRESS(aux) && type_map[p = page(aux)] < (char)t_end) {
			tm = tm_of((cl_type)type_map[p]);
			x = (cl_object)(aux - (aux - pagetochar(p)) % tm->tm_size);
			m = x->d.m;
			if (m != FREE && m != TRUE) {
				if (m) {
					fprintf(stderr,
						"** bad value %d of d.m in gc page %d skipping mark **",
						m, p); fflush(stderr);
				} else {
					mark_object(x);
				}
			}
		}
	}
	if (debug) {
		printf(". done.\n"); fflush(stdout);
	}
}

static void
mark_cl_env(struct cl_env_struct *env)
{
	int i = 0;
	cl_object where = 0;
	ecl_bds_ptr bdp = 0;
	ecl_frame_ptr frp = 0;
	ecl_ihs_ptr ihs = 0;

	mark_contblock(env, sizeof(*env));

	mark_object(env->lex_env);

	mark_contblock(env->stack, env->stack_size * sizeof(cl_object));
	mark_stack_conservative((cl_ptr)env->stack, (cl_ptr)env->stack_top);

	if ((bdp = env->bds_org)) {
		mark_contblock(bdp, env->bds_size * sizeof(*bdp));
		for (;  bdp <= env->bds_top;  bdp++) {
			mark_object(bdp->symbol);
			mark_object(bdp->value);
		}
	}
	mark_object(env->bindings_hash);

	if ((frp = env->frs_org)) {
		mark_contblock(frp, env->frs_size * sizeof(*frp));
		for (;  frp <= env->frs_top;  frp++) {
			mark_object(frp->frs_val);
		}
	}

	for (ihs = env->ihs_top; ihs; ihs = ihs->next) {
		mark_object(ihs->function);
		mark_object(ihs->lex_env);
	}

	for (i=0; i<env->nvalues; i++)
		mark_object(env->values[i]);

	mark_object(env->string_pool);

	if (env->c_env) {
		mark_object(env->c_env->variables);
		mark_object(env->c_env->macros);
		mark_object(env->c_env->constants);
	}

	mark_object(env->fmt_aux_stream);

	mark_contblock(env->queue, sizeof(short) * ECL_PPRINT_QUEUE_SIZE);
	mark_contblock(env->indent_stack, sizeof(short) * ECL_PPRINT_INDENTATION_STACK_SIZE);

	mark_object(env->big_register[0]);
	mark_object(env->big_register[1]);
	mark_object(env->big_register[2]);

#ifdef CLOS
#ifdef ECL_THREADS
	mark_object(env->method_hash_clear_list);
#endif
	mark_object(env->method_hash);
	mark_object(env->method_spec_vector);
#endif

#ifdef ECL_THREADS
/* We should mark the stacks of the threads somehow!!! */
#error "The old garbage collector does not support threads"
#else
# ifdef ECL_DOWN_STACK
	mark_stack_conservative((cl_ptr)(&where), (cl_ptr)env->cs_org);
# else
	mark_stack_conservative((cl_ptr)env->cs_org, (cl_ptr)(&where));
# endif /* ECL_DOWN_STACK */
#endif /* THREADS */

#ifdef ECL_FFICALL
	mark_contblock(env->fficall, sizeof(struct ecl_fficall));
	mark_object(((struct ecl_fficall*)env->fficall)->cstring);
#endif
}

static void
mark_phase(void)
{
	int i;
	cl_object s;

	/* save registers on the stack */
	jmp_buf volatile registers;
	ecl_setjmp(registers);

	/* mark registered symbols & keywords */
	for (i=0; i<cl_num_symbols_in_core; i++) {
		s = (cl_object)(cl_symbols + i);
		s->symbol.m = FALSE;
	}
	for (i=0; i<cl_num_symbols_in_core; i++) {
		s = (cl_object)(cl_symbols + i);
		mark_object(s);
	}

	/* We mark everything, but we do not want to get the loaded
	 * libraries to be marked unless they are referenced somewhere
	 * else (function definition. etc) */
	s = cl_core.libraries;
	if (s) {
		for (i = 0; i < s->vector.fillp; i++) {
			cl_object dll = s->vector.self.t[i];
			if (dll->cblock.locked) {
				mark_object(dll);
			}
		}
		s->vector.elttype = ecl_aet_fix;
		mark_object(s);
		s->vector.elttype = ecl_aet_object;
	}
	mark_stack_conservative((cl_ptr)&cl_core, (cl_ptr)(&cl_core + 1));
	/* mark roots */
	for (i = 0; i < gc_roots;  i++)
		mark_object(*gc_root[i]);

#ifdef ECL_THREADS
	mark_object(cl_core.processes);
#else
	mark_cl_env(&cl_env);
#endif
}

static void
sweep_phase(void)
{
	register int i, j, k;
	register cl_object x;
	register cl_ptr p;
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

		tm = tm_of((cl_type)type_map[i]);

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
			if (x->d.m == FREE)
				continue;
			else if (x->d.m) {
				x->d.m = FALSE;
				continue;
			}
			/* INV: Make sure this is the same as in alloc_2.d */
			switch (x->d.t) {
#ifdef ENABLE_DLOPEN
			case t_codeblock:
				ecl_library_close(x);
				break;
#endif
			case t_stream:
                                if (!x->stream.closed)
					cl_close(1, x);
                                break;
#ifdef ECL_THREADS
			case t_lock:
#if defined(ECL_MS_WINDOWS_HOST)
				CloseHandle(x->lock.mutex);
#else
				pthread_mutex_destroy(&x->lock.mutex);
#endif
				break;
			case t_condition_variable:
#if defined(ECL_MS_WINDOWS_HOST)
				CloseHandle(x->condition_variable.cv);
#else
				pthread_cond_destroy(&x->condition_variable.cv);
#endif
				break;
#endif
#ifdef ECL_SEMAPHORES
			case t_semaphore:
#error "Unfinished"
				break;
#endif
			default:;
			}
			((struct freelist *)x)->f_link = f;
			x->d.m = FREE;
			f = x;
			k++;
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
	register cl_ptr s, e, p, q;
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
			ecl_dealloc(p);
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

void
ecl_gc(cl_type t)
{
	const cl_env_ptr env = ecl_process_env();
	int i, j;
	int tm;
	int gc_start = ecl_runtime();
	bool interrupts;

	if (!GC_enabled())
		return;

	GC_disable();

	CL_NEWENV_BEGIN {
	if (SYM_VAL(@'si::*gc-verbose*') != ECL_NIL) {
		printf("\n[GC ..");
		/* To use this should add entries in tm_table for reloc and contig.
		   fprintf(stdout, "\n[GC for %d %s pages ..",
		   tm_of(t)->tm_npage,
		   tm_table[(int)t].tm_name + 1); */
		fflush(stdout);
	}

	debug = ecl_symbol_value(@'si::*gc-message*') != ECL_NIL;

	if (GC_enter_hook != NULL)
		(*GC_enter_hook)();

#ifdef THREADS
#error "We need to stop all other threads"
#endif /* THREADS */

	interrupts = env->disable_interrupts;
	env->disable_interrupts = 1;

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
		extern void cl_resize_hole(cl_index);

		if (holepage < mark_table_size*sizeof(int)/LISP_PAGESIZE + 1)
			new_holepage = mark_table_size*sizeof(int)/LISP_PAGESIZE + 1;
		if (new_holepage < HOLEPAGE)
			new_holepage = HOLEPAGE;
		cl_resize_hole(new_holepage);

		mark_table = (int*)heap_end;
		for (i = 0;  i < mark_table_size; i++)
			mark_table[i] = 0;
	}

	if (debug) {
		printf("mark phase\n");
		fflush(stdout);
		tm = ecl_runtime();
	}
	mark_phase();
	if (debug) {
		printf("mark ended (%d)\n", ecl_runtime() - tm);
		printf("sweep phase\n");
		fflush(stdout);
		tm = ecl_runtime();
	}
	sweep_phase();
	if (debug) {
		printf("sweep ended (%d)\n", ecl_runtime() - tm);
		fflush(stdout);
	}

	if (t == t_contiguous) {
		if (debug) {
			printf("contblock sweep phase\n");
			fflush(stdout);
			tm = ecl_runtime();
		}
		contblock_sweep_phase();
		if (debug)
			printf("contblock sweep ended (%d)\n", ecl_runtime() - tm);
	}

	if (debug) {
		for (i = 0, j = 0;  i < (int)t_end;  i++) {
			if (tm_table[i].tm_type == (cl_type)i) {
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

	env->disable_interrupts = interrupts;

	if (GC_exit_hook != NULL)
		(*GC_exit_hook)();

	} CL_NEWENV_END;

	GC_enable();

#ifdef THREADS
#error "We need to activate all other threads again"
#endif /* THREADS */

	gc_time += (gc_start = ecl_runtime() - gc_start);

	if (SYM_VAL(@'si::*gc-verbose*') != ECL_NIL) {
		/* Don't use fprintf since on Linux it calls malloc() */
		printf(". finished in %.2f\"]", gc_start/60.0);
		fflush(stdout);
	}

	if (env->interrupt_pending) ecl_check_pending_interrupts();
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
_mark_contblock(void *x, cl_index s)
{
	cl_ptr p = x;
	if (p >= heap_start && p < data_end) {
		ptrdiff_t pg = page(p);
		if ((cl_type)type_map[pg] == t_contiguous) {
			cl_ptr q = p + s;
			p = int2ptr(ptr2int(p) & ~3);
			q = int2ptr(ptr2int(q + 3) & ~3);
			for (;  p < q;  p+= 4)
				set_mark_bit(p);
		}
	}
}

/*----------------------------------------------------------------------
 * Utilities
 *----------------------------------------------------------------------
 */

@(defun si::room-report ()
	int i;
	cl_object *tl;
@
	the_env->nvalues = 8;
	the_env->values[0] = ecl_make_fixnum(real_maxpage);
	the_env->values[1] = ecl_make_fixnum(available_pages());
	the_env->values[2] = ecl_make_fixnum(ncbpage);
	the_env->values[3] = ecl_make_fixnum(maxcbpage);
	the_env->values[4] = ecl_make_fixnum(ncb);
	the_env->values[5] = ecl_make_fixnum(cbgccount);
	the_env->values[6] = ecl_make_fixnum(holepage);
	the_env->values[7] = ECL_NIL;
	tl = &the_env->values[7];
	for (i = 0;  i < (int)t_end;  i++) {
	  if (tm_table[i].tm_type == (cl_type)i) {
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
	return the_env->values[0];
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

cl_object
si_get_finalizer(cl_object o)
{
	@(return ECL_NIL)
}

cl_object
si_set_finalizer(cl_object o, cl_object finalizer)
{
	@(return)
}

void
init_GC(void)
{
	GC_enable();
	gc_time = 0;
}
