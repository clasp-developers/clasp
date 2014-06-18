/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    unify.d -- Support for unification.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


#include "ecl.h"
#include "unify.h"

object *slot;			/* scanning pointer within object */
int (*slotf)();			/* read/write mode accessor */

/* -------------------- Trail Instructions -------------------- */

object *trail[VSSIZE];
object **trail_top = trail;

#define BIND(loc, val)		{loc = val; trail_push(&loc);}

@(defun trail_mark ()
@
  trail_mark;
@)

@(defun trail_restore ()
@
  trail_restore;
  @(return ECL_NIL)
@)

@(defun trail_unmark ()
@
  trail_unmark;
  @(return ECL_NIL)
@)

/* -------------------- Mode Operators -------------------- */

bool get_slot(object x)		/* read mode */
{
  if (x == *slot || unify(x, *slot))
    if (*slot == OBJNULL)
      return((bool)MAKE_LOCATIVE(slot++));
    else
      return((bool)*slot++);	/* dereference */
  else
    return(FALSE);
}

bool set_slot(object x)		/* write mode */
{
  /* NOTE: slot contains OBJNULL */
  *slot = x;
  return((bool)MAKE_LOCATIVE(slot++));
}


/* -------------------- Get Instructions -------------------- */

/* get_variable is just setq */

@(defun get_value (v x)
@
	@(return (get_value(v, x)?ECL_T:ECL_NIL))
@)

@(defun get_constant (c x)
@
	@(return (get_constant(c, x)?ECL_T:ECL_NIL))
@)

@(defun get_nil (arg)
@
	@(return (get_nil(arg)?ECL_T:ECL_NIL))
@)

bool
get_cons(object x)
{

RETRY:	switch (ecl_t_of(x)) {
	case t_cons:
	  slot = &CDR(x);	/* cdr slot is first in struct cons */
	  slotf = get_slot;
	  return(TRUE);

	case t_locative:
	  if (UNBOUNDP(x)) {
	    object new = CONS(OBJNULL, OBJNULL);
	    BIND(DEREF(x), new);
	    slot = &CDR(new);
	    slotf = set_slot;
	    return(TRUE);
	  }
	  else {
	    x = DEREF(x);
	    goto RETRY;
	  }

	default: return(FALSE);
	}
	    
}

@(defun get_cons (arg)
@
	@(return (get_cons(arg)?ECL_T:ECL_NIL))
@)

bool
get_instance(object x, object class, int arity)
{
RETRY:	switch (ecl_t_of(x)) {
	case t_instance: 
	if (ECL_CLASS_OF(x) == class) {
	  slot = x->instance.slots;
	  slotf = get_slot;
	  return(TRUE);
	} else
	  return(FALSE);

	case t_locative:
	  if (UNBOUNDP(x)) {
	    object new = allocate_instance(class, arity);
	    BIND(DEREF(x), new);
	    slot = new->instance.slots;
	    slotf = set_slot;
	    return(TRUE);
	  }
	  else {
	    x = DEREF(x);
	    goto RETRY;
	  }
	default: return(FALSE);
	}
}

@(defun get_instance (x class arity)
@
	@(return (get_instance(x, class, ecl_fixnum(arity))?ECL_T:ECL_NIL))
@)


/* -------------------- Unify Instructions --------------------  */

#define UNIFY_LOCATIVE(x, y, L)	{object *p = &DEREF(x); \
				   if (*p == OBJNULL) { \
				     BIND(*p, y); return(TRUE); } \
				       else { x = *p; goto L;}}
/*
#define UNIFY_LOCATIVE(x, y, L) {if (UNBOUNDP(x)) { \
				     BIND(DEREF(x), y); return(TRUE); } \
				       else { x = DEREF(x); goto L;}}
*/

bool
unify(object x, object y)
{
  /* NOTE: x <- y */

  L: switch (ecl_t_of(x)) {

  case t_locative: UNIFY_LOCATIVE(x, y, L);

  case t_cons:
       L1: switch (ecl_t_of(y)) {

       case t_cons: return(unify(CAR(x), CAR(y)) &&
			   unify(CDR(x), CDR(y)));

       case t_locative: UNIFY_LOCATIVE(y, x, L1);

       default: return(FALSE);
       }

  case t_instance:
       L2: switch (ecl_t_of(y)) {

       case t_instance:
	 if (ECL_CLASS_OF(x) == ECL_CLASS_OF(y)) {
	   int l = x->instance.length; int i;
	   object *slotx = x->instance.slots;
	   object *sloty = y->instance.slots;
	   for (i = 0; i < l; i++) {
	     if (!unify(*slotx++, *sloty++))
	       return(FALSE);
	   }
	   return(TRUE);
	 } else
	   return(FALSE);

       case t_locative: UNIFY_LOCATIVE(y, x, L2);

       default: return(FALSE);
       }

  default:
    L3: if (LOCATIVEP(y))
           UNIFY_LOCATIVE(y, x, L3)
        else if (equal(x,y))
	  return(TRUE);
	else
	  return(FALSE);
  }
}

/* Internal function. One should use unify_variable, which always returns T */

@(defun unify_slot ()
@
	@(return ((object)unify_slot))
@)


@(defun unify_value (loc)
  object x;
@
	x = (object)unify_value(loc);
	@(return ((x == ECL_NIL || x)?ECL_T:ECL_NIL))
@)

@(defun unify_constant (c)
  object x;
@
	x = (object)unify_constant(c);
	@(return ((x == ECL_NIL || x)?ECL_T:ECL_NIL))
@)

@(defun unify_nil ()
  object x;
@
	x = (object)unify_nil;
	@(return ((x == ECL_NIL || x)?ECL_T:ECL_NIL))
@)

/* -------------------- Test Functions -------------------- */

@(defun make_locative (&optional (n 0))
@
	@(return (MAKE_LOCATIVE(ecl_fixnum(n))))
@)

@(defun locativep (obje)
@
	@(return (LOCATIVEP(obje)?ECL_T:ECL_NIL))
@)

@(defun unboundp (loc)
@
	@(return (UNBOUNDP(loc)?ECL_T:ECL_NIL))
@)

@(defun dereference (x)
  extern object Slocative;
@
	while (ecl_t_of(x) != t_locative)
	  x = wrong_type_argument(Slocative, x);
	@(return (DEREF(x)))
@)

@(defun make_variable (name)
@
	@(return (CONS(name, OBJNULL)))
@)

/* (defmacro unify-variable (v) `(progn (setq ,v (si:unify-slot)) t) */

object Ssetq, Sunify_slot;

@(defun unify_variable (object var)
@
	@(return list(3, Sprogn,
			 list(3, Ssetq, CADR(var),
				 CONS(Sunify_slot, ECL_NIL)),
			 ECL_T))
@)

#define make_si_macro(name, cfun)	\
	{object x = make_si_ordinary(name); \
	   ECL_SYM_FUN(x) = make_cfun(cfun, ECL_NIL, NULL); \
	   x->symbol.mflag = TRUE; \
	 }

void
init_unify(void)
{
	make_si_macro("UNIFY-VARIABLE", Lunify_variable);
}
