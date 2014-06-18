/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    list.d -- List manipulating routines.
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

#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>

struct cl_test {
	bool (*test_c_function)(struct cl_test *, cl_object);
	cl_object (*key_c_function)(struct cl_test *, cl_object);
	cl_env_ptr env;
	cl_object key_function;
	cl_objectfn key_fn;
	cl_object test_function;
	cl_objectfn test_fn;
	cl_object item_compared;
};

static cl_object subst(struct cl_test *t, cl_object new_obj, cl_object tree);
static cl_object nsubst(struct cl_test *t, cl_object new_obj, cl_object tree);
static cl_object sublis(struct cl_test *t, cl_object alist, cl_object tree);
static cl_object nsublis(struct cl_test *t, cl_object alist, cl_object tree);
static cl_object do_assoc(struct cl_test *t, cl_object alist);

#define TEST(t,k) ((t)->test_c_function)((t),(k))
#define KEY(t,x) ((t)->key_c_function)((t),(x))
#define close_test(t) (void)0

static bool
test_compare(struct cl_test *t, cl_object x)
{
	x = KEY(t,x);
	t->env->function = t->test_function;
	return t->test_fn(2, t->item_compared, x) != ECL_NIL;
}

static bool
test_compare_not(struct cl_test *t, cl_object x)
{
	x = KEY(t,x);
	t->env->function = t->test_function;
	return t->test_fn(2, t->item_compared, x) == ECL_NIL;
}

static bool
test_eq(struct cl_test *t, cl_object x)
{
	return (t->item_compared == KEY(t,x));
}

static bool
test_eql(struct cl_test *t, cl_object x)
{
	return ecl_eql(t->item_compared, KEY(t,x));
}

static bool
test_equal(struct cl_test *t, cl_object x)
{
	return ecl_equal(t->item_compared, KEY(t,x));
}

static bool
test_equalp(struct cl_test *t, cl_object x)
{
	return ecl_equalp(t->item_compared, KEY(t,x));
}

static cl_object
key_function(struct cl_test *t, cl_object x)
{
	t->env->function = t->key_function;
	return t->key_fn(1,x);
}

static cl_object
key_identity(struct cl_test *t, cl_object x)
{
	return x;
}

static void
setup_test(struct cl_test *t, cl_object item, cl_object test,
	   cl_object test_not, cl_object key)
{
	cl_env_ptr env = t->env = ecl_process_env();
	t->item_compared = item;
	if (test != ECL_NIL) {
		if (test_not != ECL_NIL)
		    FEerror("Both :TEST and :TEST-NOT are specified.", 0);
		t->test_function = test = si_coerce_to_function(test);
		if (test == ECL_SYM_FUN(@'eq')) {
			t->test_c_function = test_eq;
		} else if (test == ECL_SYM_FUN(@'eql')) {
			t->test_c_function = test_eql;
		} else if (test == ECL_SYM_FUN(@'equal')) {
			t->test_c_function = test_equal;
		} else if (test == ECL_SYM_FUN(@'equalp')) {
			t->test_c_function = test_equalp;
		} else {
			t->test_c_function = test_compare;
			t->test_fn = ecl_function_dispatch(env, test);
			t->test_function = env->function;
		}
	} else if (test_not != ECL_NIL) {
		t->test_c_function = test_compare_not;
		test_not = si_coerce_to_function(test_not);
		t->test_fn = ecl_function_dispatch(env, test_not);
		t->test_function = env->function;
	} else {
		t->test_c_function = test_eql;
	}
	if (key != ECL_NIL) {
		key = si_coerce_to_function(key);
		t->key_fn = ecl_function_dispatch(env, key);
		t->key_function = env->function;
		t->key_c_function = key_function;
	} else {
		t->key_c_function = key_identity;
	}
}

@(defun list (&rest args)
	cl_object head = ECL_NIL;
@
	if (narg--) {
		cl_object tail = head = ecl_list1(ecl_va_arg(args));
		while (narg--) {
			cl_object cons = ecl_list1(ecl_va_arg(args));
			ECL_RPLACD(tail, cons);
			tail = cons;
		}
	}
	@(return head)
@)

@(defun list* (&rest args)
	cl_object head;
@
	if (narg == 0)
		FEwrong_num_arguments(@[list*]);
	head = ecl_va_arg(args);
	if (--narg) {
		cl_object tail = head = ecl_list1(head);
		while (--narg) {
			cl_object cons = ecl_list1(ecl_va_arg(args));
			ECL_RPLACD(tail, cons);
			tail = cons;
		}
		ECL_RPLACD(tail, ecl_va_arg(args));
	}
	@(return head)
@)

static cl_object *
append_into(cl_object head, cl_object *tail, cl_object l)
{
	if (!Null(*tail)) {
		/* (APPEND '(1 . 2) 3) */
		FEtype_error_proper_list(head);
	}
	while (CONSP(l)) {
		cl_object cons = ecl_list1(ECL_CONS_CAR(l));
		*tail = cons;
		tail = &ECL_CONS_CDR(cons);
		l = ECL_CONS_CDR(l);
	}
        *tail = l;
	return tail;
}

@(defun append (&rest rest)
	cl_object head = ECL_NIL, *tail = &head;
@
	for (; narg > 1; narg--) {
		cl_object other = ecl_va_arg(rest);
                tail = append_into(head, tail, other);
	}
        if (narg) {
                if (!Null(*tail)) {
                        /* (APPEND '(1 . 2) 3) */
                        FEtype_error_proper_list(head);
                }
                *tail = ecl_va_arg(rest);
        }
	@(return head)
@)

cl_object
ecl_append(cl_object x, cl_object y)
{
	cl_object head = ECL_NIL;
        cl_object *tail = &head;
	if (!Null(x)) {
                tail = append_into(head, tail, x);
        }
        if (!Null(*tail)) {
                /* (APPEND '(1 . 2) 3) */
                FEtype_error_proper_list(head);
        }
        *tail = y;
	return head;
}

#define LENTH(n) (cl_object x) {				\
		const cl_env_ptr the_env = ecl_process_env();	\
		ecl_return1(the_env, ecl_nth(n, x));		\
	}
cl_object @fifth	LENTH(4)
cl_object @sixth	LENTH(5)
cl_object @seventh	LENTH(6)
cl_object @eighth	LENTH(7)
cl_object @ninth	LENTH(8)
cl_object @tenth	LENTH(9)
#undef LENTH

static bool
tree_equal(struct cl_test *t, cl_object x, cl_object y)
{
BEGIN:
	if (CONSP(x)) {
		if (CONSP(y)) {
			if (tree_equal(t, ECL_CONS_CAR(x), ECL_CONS_CAR(y))) {
				x = ECL_CONS_CDR(x);
				y = ECL_CONS_CDR(y);
				goto BEGIN;
			} else {
				return(FALSE);
			}
		} else {
			return(FALSE);
		}
	} else {
		t->item_compared = x;
		if (TEST(t, y))
			return(TRUE);
		else
			return(FALSE);
	}
}

@(defun tree_equal (x y &key test test_not)
	struct cl_test t;
	cl_object output;
@
	setup_test(&t, ECL_NIL, test, test_not, ECL_NIL);
	output = tree_equal(&t, x, y)? ECL_T : ECL_NIL;
	close_test(&t);
	@(return output)
@)

cl_object
cl_endp(cl_object x)
{
        cl_object output = ECL_NIL;
	if (Null(x)) {
                output = ECL_T;
        } else if (ecl_unlikely(!LISTP(x))) {
                FEwrong_type_only_arg(@[endp], x, @[list]);
        }
        @(return output);
}

bool
ecl_endp(cl_object x)
{
	if (Null(x)) {
                return TRUE;
        } else if (ecl_unlikely(!LISTP(x))) {
                FEwrong_type_only_arg(@[endp], x, @[list]);
        }
        return FALSE;
}

cl_object
cl_list_length(cl_object x)
{
	cl_fixnum n;
	cl_object fast, slow;
	/* INV: A list's length always fits in a fixnum */
	fast = slow = x;
	for (n = 0; !Null(fast); n++, fast = ECL_CONS_CDR(fast)) {
		if (ecl_unlikely(!LISTP(fast))) {
			FEtype_error_list(fast);
		}
		if (n & 1) {
			/* Circular list? */
			if (slow == fast) @(return ECL_NIL);
			slow = ECL_CONS_CDR(slow);
		}
	}
	@(return ecl_make_fixnum(n));
}

cl_object
si_proper_list_p(cl_object x)
{
	cl_fixnum n;
	cl_object fast, slow, test = ECL_T;
	/* INV: A list's length always fits in a fixnum */
	fast = slow = x;
	for (n = 0; !Null(fast); n++, fast = ECL_CONS_CDR(fast)) {
		if (!LISTP(fast)) {
                        test = ECL_NIL;
                        break;
		}
		if (n & 1) {
			/* Circular list? */
			if (slow == fast) {
                                test = ECL_NIL;
                                break;
                        }
			slow = ECL_CONS_CDR(slow);
		}
	}
	@(return test);
}

cl_object
cl_nth(cl_object n, cl_object x)
{
	@(return ecl_nth(ecl_to_size(n), x))
}

cl_object
ecl_nth(cl_fixnum n, cl_object x)
{
	if (n < 0)
		FEtype_error_index(x, n);
	/* INV: No need to check for circularity since we visit
	   at most `n' conses */
	for (; n > 0 && CONSP(x); n--)
		x = ECL_CONS_CDR(x);
	if (Null(x))
		return ECL_NIL;
	if (!LISTP(x))
		FEtype_error_list(x);
	return ECL_CONS_CAR(x);
}

cl_object
cl_nthcdr(cl_object n, cl_object x)
{
	@(return ecl_nthcdr(ecl_to_size(n), x))
}

cl_object
ecl_nthcdr(cl_fixnum n, cl_object x)
{
	if (n < 0)
		FEtype_error_index(x, n);
	while (n-- > 0 && !Null(x)) {
		if (LISTP(x)) {
			x = ECL_CONS_CDR(x);
		} else {
			FEtype_error_list(x);
		}
	}
	return x;
}

cl_object
ecl_last(cl_object l, cl_index n)
{
	/* The algorithm is very simple. We run over the list with
	 * two pointers, "l" and "r". The separation between both
	 * must be "n", so that when "l" finds no more conses, "r"
	 * contains the output. */
	cl_object r;
	for (r = l; n && CONSP(r); n--, r = ECL_CONS_CDR(r))
		;
	/* If "l" has not moved, we have to ensure that it is a list */
	if (r == l) {
		if (!LISTP(r)) FEtype_error_list(l);
		while (CONSP(r)) {
			r = ECL_CONS_CDR(r);
		}
		return r;
	} else if (n == 0) {
		while (CONSP(r)) {
			r = ECL_CONS_CDR(r);
			l = ECL_CONS_CDR(l);
		}
		return l;
	} else {
		return l;
	}
}

@(defun last (l &optional (k ecl_make_fixnum(1)))
@
	if (ecl_t_of(k) == t_bignum)
		@(return l)
	@(return ecl_last(l, ecl_to_size(k)))
@)

@(defun make_list (size &key initial_element &aux x)
	cl_fixnum i;
@
	/* INV: ecl_to_size() signals a type-error if SIZE is not a integer >=0 */
	i = ecl_to_size(size);
	while (i-- > 0)
		x = CONS(initial_element, x);
	@(return x)
@)

cl_object
cl_copy_list(cl_object x)
{
	cl_object copy;
	if (ecl_unlikely(!LISTP(x))) {
                FEwrong_type_only_arg(@[copy-list], x, @[list]);
	}
	copy = ECL_NIL;
	if (!Null(x)) {
		cl_object tail = copy = ecl_list1(CAR(x));
		while (x = ECL_CONS_CDR(x), CONSP(x)) {
			cl_object cons = ecl_list1(ECL_CONS_CAR(x));
			ECL_RPLACD(tail, cons);
			tail = cons;
		}
		ECL_RPLACD(tail, x);
	}
	@(return copy);
}

static cl_object
duplicate_pairs(cl_object x)
{
	cl_object p = ECL_CONS_CAR(x);
	if (CONSP(p))
		p = CONS(ECL_CONS_CAR(p), ECL_CONS_CDR(p));
	return ecl_list1(p);
}

cl_object
cl_copy_alist(cl_object x)
{
	cl_object copy;
	if (ecl_unlikely(!LISTP(x))) {
                FEwrong_type_only_arg(@[copy-alist], x, @[list]);
	}
	copy = ECL_NIL;
	if (!Null(x)) {
		cl_object tail = copy = duplicate_pairs(x);
		while (x = ECL_CONS_CDR(x), !Null(x)) {
			if (!LISTP(x)) {
				FEtype_error_list(x);
			} else {
				cl_object cons = duplicate_pairs(x);
				tail = ECL_RPLACD(tail, cons);
				tail = cons;
			}
		}
	}
	@(return copy);
}

static cl_object
do_copy_tree(cl_object x)
{
	if (CONSP(x)) {
		x = CONS(do_copy_tree(ECL_CONS_CAR(x)),
			 do_copy_tree(ECL_CONS_CDR(x)));
	}
	return x;
}

cl_object
cl_copy_tree(cl_object x)
{
	@(return do_copy_tree(x))
}

cl_object
cl_revappend(cl_object x, cl_object y)
{
	loop_for_in(x) {
		y = CONS(ECL_CONS_CAR(x),y);
	} end_loop_for_in;
	@(return y)
}

@(defun nconc (&rest lists)
	cl_object head = ECL_NIL, tail = ECL_NIL;
@	
	while (narg--) {
		cl_object new_tail, other = ecl_va_arg(lists);
		if (Null(other)) {
			new_tail = tail;
		} else if (CONSP(other)) {
			new_tail = ecl_last(other, 1);
		} else {
			if (narg) FEtype_error_list(other);
			new_tail = tail;
		}
		if (Null(head)) {
			head = other;
		} else {
			ECL_RPLACD(tail, other);
		}
		tail = new_tail;
	}
	@(return head)
@)

cl_object
ecl_nconc(cl_object l, cl_object y)
{
	if (Null(l)) {
		return y;
	} else {
		ECL_RPLACD(ecl_last(l, 1), y);
		return l;
	}
}

cl_object
cl_nreconc(cl_object l, cl_object y)
{
	cl_object x, z;
	/* INV: when a circular list is "reconc'ed", the pointer ends
	   up at the beginning of the original list, hence we need no
	   slow pointer */
	for (x = l; !Null(x); ) {
                if (!LISTP(x)) FEtype_error_list(x);
		z = x;
		x = ECL_CONS_CDR(x);
		if (x == l) FEcircular_list(l);
		ECL_RPLACD(z, y);
		y = z;
	}
	@(return y)
}

cl_object
ecl_butlast(cl_object l, cl_index n)
{
	/* See LAST for details on this algorithm */
	cl_object r;
	for (r = l; n && CONSP(r); n--, r = ECL_CONS_CDR(r))
		;
	if (Null(r)) {
		return ECL_NIL;
	} else if (!LISTP(r)) {
		/* We reach here either because l is shorter than n conses,
		 * or because it is not a list */
		if (r == l) FEtype_error_list(r);
		return ECL_NIL;
	} else {
		/* We reach here because l has at least n conses and
		 * thus we can take CAR(l) */
		cl_object head, tail;
		head = tail = ecl_list1(CAR(l));
		while (l = ECL_CONS_CDR(l), r = ECL_CONS_CDR(r), CONSP(r)) {
			cl_object cons = ecl_list1(ECL_CONS_CAR(l));
			ECL_RPLACD(tail, cons);
			tail = cons;
		}
		return head;
	}
}

@(defun butlast (lis &optional (nn ecl_make_fixnum(1)))
@
	/* INV: No list has more than MOST_POSITIVE_FIXNUM elements */
	if (ecl_t_of(nn) == t_bignum)
		@(return ECL_NIL);
	/* INV: ecl_to_size() signals a type-error if NN is not an integer >=0 */
	@(return ecl_butlast(lis, ecl_to_size(nn)))
@)

cl_object
ecl_nbutlast(cl_object l, cl_index n)
{
	cl_object r;
	if (ecl_unlikely(!LISTP(l)))
                FEwrong_type_only_arg(@[nbutlast], l, @[list]);
	for (n++, r = l; n && CONSP(r); n--, r = ECL_CONS_CDR(r))
		;
	if (n == 0) {
		cl_object tail = l;
		while (CONSP(r)) {
			tail = ECL_CONS_CDR(tail);
			r = ECL_CONS_CDR(r);
		}
		ECL_RPLACD(tail, ECL_NIL);
		return l;
	}
	return ECL_NIL;
}

@(defun nbutlast (lis &optional (nn ecl_make_fixnum(1)))
@
	/* INV: No list has more than MOST_POSITIVE_FIXNUM elements */
	if (ecl_t_of(nn) == t_bignum)
		@(return ECL_NIL)
	/* INV: ecl_to_size() signas a type-error if NN is not an integer >=0 */
	@(return ecl_nbutlast(lis, ecl_to_size(nn)))
@)

cl_object
cl_ldiff(cl_object x, cl_object y)
{
	cl_object head = ECL_NIL;
	if (ecl_unlikely(!LISTP(x))) {
                FEwrong_type_only_arg(@[ldiff], x, @[list]);
	}
	/* Here we use that, if X or Y are CONS, then (EQL X Y)
	 * only when X == Y */
	if (!Null(x) && (x != y)) {
		cl_object tail = head = ecl_list1(ECL_CONS_CAR(x));
		while (1) {
			x = ECL_CONS_CDR(x);
			if (!CONSP(x)) {
				if (!ecl_eql(x, y)) {
					ECL_RPLACD(tail, x);
				}
				break;
			} else if (x == y) {
				break;
			} else {
				cl_object cons = ecl_list1(ECL_CONS_CAR(x));
				ECL_RPLACD(tail, cons);
				tail = cons;
			}
		}
	}
	@(return head)
}

cl_object
cl_rplaca(cl_object x, cl_object v)
{
        if (ecl_unlikely(!CONSP(x)))
                FEwrong_type_nth_arg(@[rplaca], 1, x, @[cons]);
	ECL_RPLACA(x, v);
	@(return x)
}

cl_object
cl_rplacd(cl_object x, cl_object v)
{
        if (ecl_unlikely(!CONSP(x)))
                FEwrong_type_nth_arg(@[rplacd], 1, x, @[cons]);
	ECL_RPLACD(x, v);
	@(return x)
}

@(defun subst (new_obj old_obj tree &key test test_not key)
	struct cl_test t;
	cl_object output;
@
	setup_test(&t, old_obj, test, test_not, key);
	output = subst(&t, new_obj, tree);
	close_test(&t);
	@(return output)
@)


static cl_object
subst(struct cl_test *t, cl_object new_obj, cl_object tree)
{
	if (TEST(t, tree)) {
		return new_obj;
	} else if (ECL_ATOM(tree)) {
		return tree;
	} else {
		cl_object head, tail = ECL_NIL;
		do {
			cl_object cons = subst(t, new_obj, ECL_CONS_CAR(tree));
			cons = ecl_cons(cons, tree = ECL_CONS_CDR(tree));
			if (Null(tail)) {
				head = cons;
			} else {
				ECL_RPLACD(tail, cons);
			}
			tail = cons;
			if (TEST(t, tree)) {
				ECL_RPLACD(tail, new_obj);
				return head;
			}
		} while (CONSP(tree));
		return head;
	}
}

@(defun nsubst (new_obj old_obj tree &key test test_not key)
	struct cl_test t;
@
	setup_test(&t, old_obj, test, test_not, key);
	tree = nsubst(&t, new_obj, tree);
	close_test(&t);
	@(return tree)
@)

static cl_object
nsubst_cons(struct cl_test *t, cl_object new_obj, cl_object tree)
{
	cl_object l = tree;
	do {
		cl_object o = ECL_CONS_CAR(l);
		if (TEST(t, o)) {
			ECL_RPLACA(l, new_obj);
		} else if (CONSP(o)) {
			nsubst_cons(t, new_obj, o);
		}
		o = ECL_CONS_CDR(l);
		if (TEST(t, o)) {
			ECL_RPLACD(l, new_obj);
			return tree;
		}
		l = o;
	} while (CONSP(l));
	return tree;
}

static cl_object
nsubst(struct cl_test *t, cl_object new_obj, cl_object tree)
{
	if (TEST(t, tree))
		return new_obj;
	if (CONSP(tree))
		return nsubst_cons(t, new_obj, tree);
	return tree;
}

@(defun sublis (alist tree &key test test_not key)
	/* t[0] is the test for the objects in the tree, configured
	   with test, test_not and key. t[1] is the test for searching
	   in the association list.
	 */
	struct cl_test t[2];
@
	setup_test(t, ECL_NIL, ECL_NIL, ECL_NIL, key);
	setup_test(t+1, ECL_NIL, test, test_not, ECL_NIL);
	tree = sublis(t, alist, tree);
	close_test(t+1);
	close_test(t);
	@(return tree)
@)

/*
	Sublis(alist, tree) returns
	result of substituting tree by alist.
*/
static cl_object
sublis(struct cl_test *t, cl_object alist, cl_object tree)
{
	cl_object node;
	t[1].item_compared = KEY(t, tree);
	node = do_assoc(t+1, alist);
	if (!Null(node)) {
		return ECL_CONS_CDR(node);
	}
	if (CONSP(tree)) {
		tree = CONS(sublis(t, alist, ECL_CONS_CAR(tree)),
			    sublis(t, alist, ECL_CONS_CDR(tree)));
	}
	return tree;
}

@(defun nsublis (alist tree &key test test_not key)
	/* t[0] is the test for the objects in the tree, configured
	   with test, test_not and key. t[1] is the test for searching
	   in the association list.
	 */
	struct cl_test t[2];
@
	setup_test(t, ECL_NIL, ECL_NIL, ECL_NIL, key);
	setup_test(t+1, ECL_NIL, test, test_not, ECL_NIL);
	tree = nsublis(t, alist, tree);
	close_test(t+1);
	close_test(t);
	@(return tree)
@)

/*
	Nsublis(alist, treep) stores
	the result of substiting *treep by alist
	to *treep.
*/
static cl_object
nsublis(struct cl_test *t, cl_object alist, cl_object tree)
{
	cl_object node;
	t[1].item_compared = KEY(t, tree);
	node = do_assoc(t+1, alist);
	if (!Null(node)) {
		return ECL_CONS_CDR(node);
	}
	if (CONSP(tree)) {
		ECL_RPLACA(tree, nsublis(t, alist, ECL_CONS_CAR(tree)));
		ECL_RPLACD(tree, nsublis(t, alist, ECL_CONS_CDR(tree)));
	}
	return tree;
}

@(defun member (item list &key test test_not key)
	struct cl_test t;
@
	setup_test(&t, item, test, test_not, key);
	loop_for_in(list) {
		if (TEST(&t, ECL_CONS_CAR(list)))
			break;
	} end_loop_for_in;
	close_test(&t);
	@(return list)
@)

bool
ecl_member_eq(cl_object x, cl_object l)
{
	loop_for_in(l) {
		if (x == ECL_CONS_CAR(l))
			return(TRUE);
	} end_loop_for_in;
	return(FALSE);
}

cl_object
si_memq(cl_object x, cl_object l)
{
	loop_for_in(l) {
		if (x == ECL_CONS_CAR(l))
			@(return l)
	} end_loop_for_in;
	@(return ECL_NIL)
}

/* Added for use by the compiler, instead of open coding them. Beppe */
cl_object
ecl_memql(cl_object x, cl_object l)
{
	loop_for_in(l) {
		if (ecl_eql(x, ECL_CONS_CAR(l)))
			return(l);
	} end_loop_for_in;
	return(ECL_NIL);
}

cl_object
ecl_member(cl_object x, cl_object l)
{
	loop_for_in(l) {
		if (ecl_equal(x, ECL_CONS_CAR(l)))
			return(l);
	} end_loop_for_in;
	return(ECL_NIL);
}
/* End of addition. Beppe */

cl_object
si_member1(cl_object item, cl_object list, cl_object test, cl_object test_not, cl_object key)
{
	struct cl_test t;

	if (key != ECL_NIL)
		item = funcall(2, key, item);
	setup_test(&t, item, test, test_not, key);
	loop_for_in(list) {
		if (TEST(&t, ECL_CONS_CAR(list)))
			break;
	} end_loop_for_in;
	close_test(&t);
	@(return list)
}

cl_object
cl_tailp(cl_object y, cl_object x)
{
	loop_for_on(x) {
		if (ecl_eql(x, y)) @(return ECL_T);
	} end_loop_for_on(x);
	return cl_eql(x, y);
}

@(defun adjoin (item list &key test test_not key)
	cl_object output;
@
	if (narg < 2)
		FEwrong_num_arguments(@[adjoin]);
	output = @si::member1(item, list, test, test_not, key);
	if (Null(output))
		output = CONS(item, list);
	else
		output = list;
	@(return output)
@)

cl_object
cl_cons(cl_object x, cl_object y)
{
	@(return CONS(x, y))
}

cl_object
cl_acons(cl_object x, cl_object y, cl_object z)
{
	@(return CONS(CONS(x, y), z))
}

@(defun pairlis (keys data &optional a_list)
	cl_object k, d;
@
	k = keys;
	d = data;
	loop_for_in(k) {
		if (ecl_endp(d))
			goto error;
		a_list = CONS(CONS(ECL_CONS_CAR(k), ECL_CONS_CAR(d)), a_list);
		d = CDR(d);
	} end_loop_for_in;
	if (!ecl_endp(d))
error:	    FEerror("The keys ~S and the data ~S are not of the same length",
		    2, keys, data);
	@(return a_list)
@)


@(defun assoc (item a_list &key test test_not key)
	struct cl_test t;
@
	setup_test(&t, item, test, test_not, key);
	a_list = do_assoc(&t, a_list);
	close_test(&t);
	@(return a_list)
@)

static cl_object
do_assoc(struct cl_test *t, cl_object a_list)
{
	loop_for_in(a_list) {
		cl_object pair = ECL_CONS_CAR(a_list);
		if (!Null(pair)) {
			if (!LISTP(pair))
				FEtype_error_list(pair);
			if (TEST(t, ECL_CONS_CAR(pair)))
				return pair;
		}
	} end_loop_for_in;
	return ECL_NIL;
}

@(defun rassoc (item a_list &key test test_not key)
	struct cl_test t;
@
	setup_test(&t, item, test, test_not, key);
	loop_for_in(a_list) {
		cl_object pair = ECL_CONS_CAR(a_list);
		if (!Null(pair)) {
			if (!LISTP(pair))
				FEtype_error_list(pair);
			if (TEST(&t, ECL_CONS_CDR(pair))) {
				a_list = pair;
				break;
			}
		}
	} end_loop_for_in;
	close_test(&t);
	@(return a_list)
@)

cl_object
ecl_remove_eq(cl_object x, cl_object l)
{
	cl_object head = ECL_NIL, tail = ECL_NIL;
	loop_for_on_unsafe(l) {
		if (ECL_CONS_CAR(l) != x) {
			cl_object cons = ecl_list1(ECL_CONS_CAR(l));
			if (Null(tail)) {
				head = tail = cons;
			} else {
				ECL_RPLACD(tail, cons);
				tail = cons;
			}
		}
	} end_loop_for_on_unsafe(l);
	return head;
}

cl_object
ecl_delete_eq(cl_object x, cl_object l)
{
	cl_object head = l;
        cl_object *p = &head;
        while (!ECL_ATOM(l)) {
                if (ECL_CONS_CAR(l) == x) {
                        *p = l = ECL_CONS_CDR(l);
                } else {
                        p = &ECL_CONS_CDR(l);
                        l = *p;
                }
        }
	return head;
}

/* Added for use by the compiler, instead of open coding them. Beppe */
cl_object
ecl_assq(cl_object x, cl_object l)
{
	loop_for_in(l) {
		cl_object pair = ECL_CONS_CAR(l);
		if (x == CAR(pair))
			return pair;
	} end_loop_for_in;
	return(ECL_NIL);
}

cl_object
ecl_assql(cl_object x, cl_object l)
{
	loop_for_in(l) {
		cl_object pair = ECL_CONS_CAR(l);
		if (ecl_eql(x, CAR(pair)))
			return pair;
	} end_loop_for_in;
	return(ECL_NIL);
}

cl_object
ecl_assoc(cl_object x, cl_object l)
{
	loop_for_in(l) {
		cl_object pair = ECL_CONS_CAR(l);
		if (ecl_equal(x, CAR(pair)))
			return pair;
	} end_loop_for_in;
	return(ECL_NIL);
}

cl_object
ecl_assqlp(cl_object x, cl_object l)
{
	loop_for_in(l) {
		cl_object pair = ECL_CONS_CAR(l);
		if (ecl_equalp(x, CAR(pair)))
			return pair;
	} end_loop_for_in;
	return(ECL_NIL);
}
/* End of addition. Beppe */
