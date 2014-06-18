/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    serialize.d -- Serialize a bunch of lisp data.
*/
/*
    Copyright (c) 2010, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <ecl/ecl.h>
#include <string.h>
#define ECL_DEFINE_AET_SIZE
#include <ecl/internal.h>

struct fake_package {
        _ECL_HDR;
        cl_object name;
};

struct fake_symbol {
        _ECL_HDR;
        cl_object name;
        cl_object pack;
};

#define ROUND_TO_WORD(int) \
        ((int + sizeof(cl_fixnum) - 1) & ~(sizeof(cl_fixnum) - 1))
#define ROUNDED_SIZE(name) \
        ROUND_TO_WORD(sizeof(struct name))

static cl_index object_size[] = {
        0, /* t_start */
	ROUNDED_SIZE(ecl_cons), /* t_list */
	0, /* t_character = 2 */
	0, /* t_fixnum = 3 */
	ROUNDED_SIZE(ecl_bignum), /* t_bignum = 4 */
	ROUNDED_SIZE(ecl_ratio), /* t_ratio */
	ROUNDED_SIZE(ecl_singlefloat), /* t_singlefloat */
	ROUNDED_SIZE(ecl_doublefloat), /* t_doublefloat */
#ifdef ECL_LONG_FLOAT
	ROUNDED_SIZE(ecl_long_float), /* t_longfloat */
#endif
	ROUNDED_SIZE(ecl_complex), /* t_complex */
	ROUNDED_SIZE(fake_symbol), /* t_symbol */
	ROUNDED_SIZE(fake_package), /* t_package */
	ROUNDED_SIZE(ecl_hashtable), /* t_hashtable */
	ROUNDED_SIZE(ecl_array), /* t_array */
	ROUNDED_SIZE(ecl_vector), /* t_vector */
#ifdef ECL_UNICODE
	ROUNDED_SIZE(ecl_string), /* t_string */
#endif
	ROUNDED_SIZE(ecl_base_string), /* t_base_string */
	ROUNDED_SIZE(ecl_vector), /* t_bitvector */
	ROUNDED_SIZE(ecl_stream), /* t_stream */
	ROUNDED_SIZE(ecl_random), /* t_random */
	ROUNDED_SIZE(ecl_readtable), /* t_readtable */
	ROUNDED_SIZE(ecl_pathname), /* t_pathname */
	ROUNDED_SIZE(ecl_bytecodes), /* t_bytecodes */
	ROUNDED_SIZE(ecl_bclosure), /* t_bclosure */
	ROUNDED_SIZE(ecl_cfun), /* t_cfun */
	ROUNDED_SIZE(ecl_cfunfixed), /* t_cfunfixed */
	ROUNDED_SIZE(ecl_cclosure), /* t_cclosure */
#ifdef CLOS
	ROUNDED_SIZE(ecl_instance), /* t_instance */
#else
	ROUNDED_SIZE(ecl_structure), /* t_structure */
#endif /* CLOS */
#ifdef ECL_THREADS
	ROUNDED_SIZE(ecl_process), /* t_process */
	ROUNDED_SIZE(ecl_lock), /* t_lock */
	ROUNDED_SIZE(ecl_rwlock), /* t_rwlock */
	ROUNDED_SIZE(ecl_condition_variable), /* t_condition_variable */
        ROUNDED_SIZE(ecl_semaphore), /* t_semaphore */
        ROUNDED_SIZE(ecl_barrier), /* t_barrier */
        ROUNDED_SIZE(ecl_mailbox), /* t_mailbox */
#endif
	ROUNDED_SIZE(ecl_codeblock), /* t_codeblock */
	ROUNDED_SIZE(ecl_foreign), /* t_foreign */
	ROUNDED_SIZE(ecl_frame), /* t_frame */
	ROUNDED_SIZE(ecl_weak_pointer) /* t_weak_pointer */
#ifdef ECL_SSE2
	, ROUNDED_SIZE(ecl_sse_pack) /* t_sse_pack */
#endif
};

typedef struct pool {
        cl_object data;
        cl_object hash;
        cl_object queue;
        cl_object last;
} *pool_t;

static cl_index
alloc(pool_t pool, cl_index size)
{
        cl_index bytes = ROUND_TO_WORD(size);
        cl_index fillp = pool->data->vector.fillp;
        cl_index next_fillp = fillp + bytes;
        if (next_fillp >= pool->data->vector.dim) {
                cl_index new_dim = next_fillp + next_fillp / 2;
                pool->data = _ecl_funcall3(@'adjust-array', pool->data,
					   ecl_make_fixnum(new_dim));
        }
        pool->data->vector.fillp = next_fillp;
        return fillp;
}

static cl_object
fix_to_ptr(cl_object ptr)
{
        cl_fixnum i = (cl_fixnum)ptr;
        return (cl_object)(i & ~ECL_IMMEDIATE_TAG);
}

static cl_object
enqueue(pool_t pool, cl_object what)
{
        cl_object record, index;
        if (ECL_FIXNUMP(what) || ECL_CHARACTERP(what) || what == OBJNULL) {
                return what;
        }
#ifdef ECL_SMALL_CONS
        if (Null(what))
                return what;
#endif
        index = ecl_gethash_safe(what, pool->hash, OBJNULL);
        if (index == OBJNULL) {
                cl_object cons;
                index = ecl_make_fixnum(pool->hash->hash.entries);
                ecl_sethash(what, pool->hash, index);
                cons = ecl_cons(what, ECL_NIL);
                ECL_RPLACD(pool->last, cons);
                pool->last = cons;
        }
        return fix_to_ptr(index);
}

#ifdef ECL_SMALL_CONS
typedef struct {
        _ECL_HDR;
        cl_object car, cdr;
} large_cons;
typedef large_cons *large_cons_ptr;
#endif

static cl_index
serialize_bits(pool_t pool, void *data, cl_index size)
{
        cl_index index = alloc(pool, size);
        memcpy(pool->data->vector.self.b8 + index, data, size);
        return index;
}

static void
serialize_object_ptr(pool_t pool, cl_object *ptr, cl_index dim)
{
        cl_index index = serialize_bits(pool, ptr, dim*sizeof(cl_object));
        for (; dim; dim--, index += sizeof(cl_object)) {
                cl_object *p = (cl_object *)(pool->data->vector.self.b8 + index);
                *p = enqueue(pool, *p);
                p++;
        }
}

static void serialize_vector(pool_t pool, cl_object v);

static void
serialize_displaced_vector(pool_t pool, cl_object v)
{
        cl_object disp = v->vector.displaced;
        cl_object to = ECL_CONS_CAR(disp);
        if (Null(to)) {
                v->vector.displaced = ECL_NIL;
                serialize_vector(pool, v);
        } else {
                cl_index index = v->vector.self.b8 - to->vector.self.b8;
                v->vector.displaced = enqueue(pool, to);
                v->vector.self.b8 = (uint8_t*)index;
        }
}

static void
serialize_vector(pool_t pool, cl_object v)
{
        if (!Null(v->vector.displaced)) {
                serialize_displaced_vector(pool, v);
        } else if (v->vector.elttype == ecl_aet_object) {
                serialize_object_ptr(pool, v->vector.self.t, v->vector.dim);
        } else {
                serialize_bits(pool, v->vector.self.b8,
                               v->vector.dim * ecl_aet_size[v->vector.elttype]);
        }
}

static void
serialize_array(pool_t pool, cl_object a)
{
        serialize_bits(pool, a->array.dims, sizeof(cl_index) * a->array.rank);
        serialize_vector(pool, a);
}

static void
serialize_one(pool_t pool, cl_object what)
{
        cl_index bytes, index;
        cl_object buffer;
#ifdef ECL_SMALL_CONS
        if (ECL_LISTP(what)) {
                cl_index bytes = ROUND_TO_WORD(sizeof(large_cons));
                cl_index index = alloc(pool, bytes);
                large_cons_ptr cons =
                        (large_cons_ptr)(pool->data->vector.self.b8 + index);
                memset(cons, 0, bytes);
                cons->t = t_list;
                cons->car = enqueue(pool, ECL_CONS_CAR(what));
                cons->cdr = enqueue(pool, ECL_CONS_CDR(what));
                return;
        }
#endif
        bytes = object_size[what->d.t];
        index = alloc(pool, bytes);
        buffer = (cl_object)(pool->data->vector.self.b8 + index);
        memcpy(buffer, what, bytes);
        switch (buffer->d.t) {
        case t_singlefloat:
        case t_doublefloat:
#ifdef ECL_LONG_FLOAT
        case t_longfloat:
#endif
                break;
#ifndef ECL_SMALL_CONS
        case t_list:
                buffer->cons.car = enqueue(pool, buffer->cons.car);
                buffer->cons.cdr = enqueue(pool, buffer->cons.car);
                break;
#endif
        case t_bignum: {
                cl_fixnum size = ECL_BIGNUM_SIZE(buffer);
                cl_index dim = ((size < 0) ? (-size) : size);
                cl_index bytes = dim * sizeof(mp_limb_t);
                serialize_bits(pool, ECL_BIGNUM_LIMBS(buffer), bytes);
                break;
        }
        case t_ratio: {
                buffer->ratio.den = enqueue(pool, buffer->ratio.den);
                buffer->ratio.num = enqueue(pool, buffer->ratio.num);
                break;
        }
        case t_complex: {
                buffer->complex.real = enqueue(pool, buffer->complex.real);
                buffer->complex.imag = enqueue(pool, buffer->complex.imag);
                break;
        }
#ifdef ECL_UNICODE
        case t_string:
#endif
        case t_vector:
        case t_bitvector:
        case t_base_string: {
                serialize_vector(pool, buffer);
                break;
        }
        case t_array: {
                cl_index bytes = ROUND_TO_WORD(buffer->array.rank *
                                               sizeof(cl_index));
                serialize_bits(pool, buffer->array.dims, bytes);
                serialize_vector(pool, buffer);
                break;
        }
        case t_package: {
                struct fake_package *p = (struct fake_package *)buffer;
                p->name = enqueue(pool, what->pack.name);
                break;
        }
        case t_symbol: {
                struct fake_symbol *p = (struct fake_symbol *)buffer;
                p->name = enqueue(pool, what->symbol.name);
                p->pack = enqueue(pool, what->symbol.hpack);
                break;
        }
        case t_pathname:
                buffer->pathname.host =
                        enqueue(pool, buffer->pathname.host);
                buffer->pathname.device =
                        enqueue(pool, buffer->pathname.device);
                buffer->pathname.directory =
                        enqueue(pool, buffer->pathname.directory);
                buffer->pathname.name = enqueue(pool, buffer->pathname.name);
                buffer->pathname.type = enqueue(pool, buffer->pathname.type);
                buffer->pathname.version =
                        enqueue(pool, buffer->pathname.version);
                break;
	case t_random: {
		buffer->random.value = enqueue(pool, buffer->random.value);
		break;
	}
	case t_bclosure: {
		buffer->bclosure.code = enqueue(pool, buffer->bclosure.code);
		buffer->bclosure.lex = enqueue(pool, buffer->bclosure.lex);
	}
	case t_bytecodes: {
		buffer->bytecodes.name = enqueue(pool, buffer->bytecodes.name);
		buffer->bytecodes.definition = enqueue(pool, buffer->bytecodes.definition);
		buffer->bytecodes.data = enqueue(pool, buffer->bytecodes.data);
		buffer->bytecodes.file = enqueue(pool, buffer->bytecodes.file);
		buffer->bytecodes.file_position = enqueue(pool, buffer->bytecodes.file_position);
		buffer->bytecodes.code = serialize_bits(pool, buffer->bytecodes.code,
							buffer->bytecodes.code_size);
	}
        default:
                FEerror("Unable to serialize object ~A", 1, what);
        }
}

static void
init_pool(pool_t pool, cl_object root)
{
        pool->data = si_make_vector(@'ext::byte8',
                                    ecl_make_fixnum(1024),
                                    ECL_T,
                                    ecl_make_fixnum(2 * sizeof(cl_index)),
                                    ECL_NIL,
                                    ecl_make_fixnum(0));
        pool->hash = cl__make_hash_table(@'eql', ecl_make_fixnum(256),
                                         cl_core.rehash_size,
                                         cl_core.rehash_threshold);
        ecl_sethash(root, pool->hash, ecl_make_fixnum(0));
        pool->queue = ecl_list1(root);
        pool->last = pool->queue;
}

static cl_object
close_pool(pool_t pool)
{
        pool->data->vector.self.index[0] = pool->data->vector.fillp;
        pool->data->vector.self.index[1] = pool->hash->hash.entries;
        return pool->data;
}

cl_object
si_serialize(cl_object root)
{
        struct pool pool[1];
        init_pool(pool, root);
        while (!Null(pool->queue)) {
                cl_object what = ECL_CONS_CAR(pool->queue);
                serialize_one(pool, what);
                pool->queue = ECL_CONS_CDR(pool->queue);
        }
        @(return close_pool(pool));
}

static void *
reconstruct_bits(uint8_t *data, cl_index bytes)
{
        void *output = ecl_alloc_atomic(bytes);
        memcpy(output, data, bytes);
        return output;
}

static void *
reconstruct_object_ptr(uint8_t *data, cl_index bytes)
{
        void *output = ecl_alloc(bytes);
        memcpy(output, data, bytes);
        return output;
}

static uint8_t *
reconstruct_bytecodes(cl_object o, uint8_t *data)
{
	o->bytecodes.code = reconstruct_bits(data, o->bytecodes.code_size);
	data += o->bytecodes.code_size;
	return data;
}

static uint8_t *
reconstruct_vector(cl_object v, uint8_t *data)
{
        if (v->vector.displaced == ECL_NIL) {
                cl_type t = v->vector.elttype;
                cl_index size = v->vector.dim * ecl_aet_size[t];
                cl_index bytes = ROUND_TO_WORD(size);
                if (t == ecl_aet_object) {
                        v->vector.self.t = reconstruct_object_ptr(data, bytes);
                } else {
                        v->vector.self.t = reconstruct_bits(data, size);
                }
                data += bytes;
        }
        return data;
}

static uint8_t *
reconstruct_array(cl_object a, uint8_t *data)
{
        cl_index bytes = ROUND_TO_WORD(a->array.rank * sizeof(cl_index));
        a->array.dims = reconstruct_bits(data, bytes);
        return reconstruct_vector(a, data + bytes);
}

static uint8_t *
duplicate_object(uint8_t *data, cl_object *output)
{
        cl_type t = ((cl_object)data)->d.t;
        cl_object o = ecl_alloc_object(t);
        cl_index bytes = object_size[t];
        memcpy(o, data, bytes);
        *output = o;
        return data + bytes;
}

static uint8_t *
reconstruct_one(uint8_t *data, cl_object *output)
{
        cl_object o = (cl_object)data;
        switch (o->d.t) {
#ifdef ECL_SMALL_CONS
        case t_list: {
                large_cons_ptr c = (large_cons_ptr)data;
                *output = ecl_cons(c->car, c->cdr);
                data += ROUND_TO_WORD(sizeof(large_cons));
                break;
        }
#endif
#ifdef ECL_UNICODE
        case t_string:
#endif
        case t_base_string:
        case t_vector:
        case t_bitvector:
                data = duplicate_object(data, output);
                data = reconstruct_vector(*output, data);
                break;
        case t_array:
                data = duplicate_object(data, output);
                data = reconstruct_array(*output, data);
                break;
        case t_package:
                *output = (cl_object)data;
                data += ROUND_TO_WORD(sizeof(struct fake_package));
                break;
        case t_symbol:
                *output = (cl_object)data;
                data += ROUND_TO_WORD(sizeof(struct fake_symbol));
                break;
	case t_bytecodes:
		data = duplicate_object(data, output);
		data = reconstruct_bytecodes(*output, data);
        default:
                data = duplicate_object(data, output);
        }
        return data;
}

static cl_object
get_object(cl_object o_or_index, cl_object *o_list)
{
        if (ECL_IMMEDIATE(o_or_index)) {
                return o_or_index;
        } else {
                cl_index i = (cl_index)o_or_index >> 2;
                return o_list[i];
        }
}

static void
fixup_vector(cl_object v, cl_object *o_list)
{
        if (!ECL_IMMEDIATE(v->vector.displaced)) {
                cl_object disp = get_object(v->vector.displaced, o_list);
                cl_object to = ECL_CONS_CAR(disp);
                if (to != ECL_NIL) {
                        cl_index offset = (cl_index)v->vector.self.b8;
                        v->vector.displaced = ECL_NIL;
                        ecl_displace(v, to, ecl_make_fixnum(offset));
                        return;
                }
        }
        if (v->vector.elttype == ecl_aet_object) {
                cl_index i;
                cl_object *p = v->vector.self.t;
                for (i = v->vector.dim; i; i--, p++) {
                        *p = get_object(*p, o_list);
                }
        }
}

static void
fixup(cl_object o, cl_object *o_list)
{
#ifdef ECL_SMALL_CONS
        if (ECL_LISTP(o)) {
                ECL_RPLACA(o, get_object(ECL_CONS_CAR(o), o_list));
                ECL_RPLACD(o, get_object(ECL_CONS_CDR(o), o_list));
                return;
        }
#endif
        switch (o->d.t) {
        case t_ratio:
                o->ratio.den = get_object(o->ratio.den, o_list);
                o->ratio.num = get_object(o->ratio.num, o_list);
                break;
        case t_complex:
                o->complex.real = get_object(o->complex.real, o_list);
                o->complex.imag = get_object(o->complex.imag, o_list);
                break;
#ifdef ECL_UNICODE
        case t_string:
#endif
        case t_base_string:
        case t_vector:
        case t_bitvector:
        case t_array:
                fixup_vector(o, o_list);
                break;
        case t_pathname:
                o->pathname.host = get_object(o->pathname.host, o_list);
                o->pathname.device =
                        get_object(o->pathname.device, o_list);
                o->pathname.directory =
                        get_object(o->pathname.directory, o_list);
                o->pathname.name = get_object(o->pathname.name, o_list);
                o->pathname.type = get_object(o->pathname.type, o_list);
                o->pathname.version =
                        get_object(o->pathname.version, o_list);
                break;
        case t_random:
                o->random.value = get_object(o->random.value, o_list);
                break;
        case t_bclosure:
                o->bclosure.code = get_object(o->bclosure.code, o_list);
                o->bclosure.lex = get_object(o->bclosure.lex, o_list);
		o->bclosure.entry = _ecl_bclosure_dispatch_vararg;
                break;
	case t_bytecodes:
		o->bytecodes.name = get_object(o->bytecodes.name, o_list);
		o->bytecodes.definition = get_object(o->bytecodes.definition, o_list);
		o->bytecodes.data = get_object(o->bytecodes.data, o_list);
		o->bytecodes.file = get_object(o->bytecodes.file, o_list);
		o->bytecodes.file_position = get_object(o->bytecodes.file_position, o_list);
		o->bytecodes.entry = _ecl_bytecodes_dispatch_vararg;
		break;
        default:
                break;
        }
}

cl_object
ecl_deserialize(uint8_t *raw)
{
        cl_index *data = (cl_index*)raw;
        cl_index i, num_el = data[1];
        cl_object *output = ecl_alloc(sizeof(cl_object) * num_el);
        raw += 2*sizeof(cl_index);
        for (i = 0; i < num_el; i++) {
                raw = reconstruct_one(raw, output+i);
        }
        for (i = 0; i < num_el; i++) {
                cl_object package = output[i];
                if (!ECL_IMMEDIATE(package) && package->d.t == t_package) {
                        cl_object name = get_object(package->pack.name,
                                                    output);
                        output[i] = ecl_find_package_nolock(name);
                }
        }
        for (i = 0; i < num_el; i++) {
                cl_object symbol = output[i];
                if (!ECL_IMMEDIATE(symbol) && symbol->d.t == t_symbol) {
                        struct fake_symbol *s = (struct fake_symbol *)symbol;
                        cl_object name = get_object(s->name, output);
                        cl_object pack = get_object(s->pack, output);
                        int flag;
                        output[i] = ecl_intern(name, pack, &flag);
                }
        }
        for (i = 0; i < num_el; i++) {
                fixup(output[i], output);
        }
        return output[0];
}


cl_object
si_deserialize(cl_object data)
{
        @(return ecl_deserialize(data->vector.self.b8))
}
