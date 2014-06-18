/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    unify.h -- Unification macros
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.

    ECoLisp is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


#define	trail_push(loc)		(*trail_top++ = (loc))
#define	trail_pop		(**--trail_top = OBJNULL)
#define trail_mark		trail_push((object *)NULL)
#define trail_restore		{while (trail_top[-1] != (object *)NULL) \
				   trail_pop;}
#define trail_unmark		{trail_restore; trail_top--;}
#define BIND(loc, val)		{loc = val; trail_push(&loc);}

#define get_value(v, x)		unify(x, v)
#define get_constant(c, x)	(c == x || unify(x, c))
#define get_nil(x)		(ECL_NIL == x || unify(x, ECL_NIL))

#define unify_slot		(*slotf)(*slot)
#define unify_value(loc)	(*slotf)(loc)
#define unify_constant(c)	(*slotf)(c)
#define unify_nil		(*slotf)(ECL_NIL)
