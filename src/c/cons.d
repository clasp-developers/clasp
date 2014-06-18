/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    cons.d -- list manipulation macros & functions
*/
/*
    Copyright (c) 2011, Juan Jose Garcia-Ripoll

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <ecl/ecl.h>
#include <ecl/cons.h>

/* BEGIN-GENERATED (gen-cons-d) */

#if !ECL_CAN_INLINE
cl_object _ecl_car(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object _ecl_cdr(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object _ecl_caar(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object _ecl_cdar(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object _ecl_cadr(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object _ecl_cddr(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object _ecl_caaar(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object _ecl_cdaar(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object _ecl_cadar(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object _ecl_cddar(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object _ecl_caadr(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object _ecl_cdadr(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object _ecl_caddr(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object _ecl_cdddr(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object _ecl_caaaar(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object _ecl_cdaaar(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object _ecl_cadaar(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object _ecl_cddaar(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object _ecl_caadar(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object _ecl_cdadar(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object _ecl_caddar(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object _ecl_cdddar(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object _ecl_caaadr(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object _ecl_cdaadr(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object _ecl_cadadr(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object _ecl_cddadr(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object _ecl_caaddr(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object _ecl_cdaddr(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object _ecl_cadddr(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object _ecl_cddddr(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

#endif /* !ECL_CAN_INLINE */

cl_object ecl_car(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object ecl_cdr(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object ecl_caar(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object ecl_cdar(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object ecl_cadr(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object ecl_cddr(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object ecl_caaar(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object ecl_cdaar(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object ecl_cadar(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object ecl_cddar(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object ecl_caadr(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object ecl_cdadr(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object ecl_caddr(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object ecl_cdddr(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object ecl_caaaar(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object ecl_cdaaar(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object ecl_cadaar(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object ecl_cddaar(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object ecl_caadar(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object ecl_cdadar(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object ecl_caddar(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object ecl_cdddar(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object ecl_caaadr(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object ecl_cdaadr(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object ecl_cadadr(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object ecl_cddadr(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object ecl_caaddr(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object ecl_cdaddr(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

cl_object ecl_cadddr(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

cl_object ecl_cddddr(cl_object x)
{
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (ecl_unlikely(!ECL_LISTP(x))) FEwrong_type_nth_arg(@[car], 1, x, @[list]);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}


cl_object cl_car(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_car(x));
}

cl_object cl_cdr(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_cdr(x));
}

cl_object cl_caar(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_caar(x));
}

cl_object cl_cdar(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_cdar(x));
}

cl_object cl_cadr(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_cadr(x));
}

cl_object cl_cddr(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_cddr(x));
}

cl_object cl_caaar(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_caaar(x));
}

cl_object cl_cdaar(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_cdaar(x));
}

cl_object cl_cadar(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_cadar(x));
}

cl_object cl_cddar(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_cddar(x));
}

cl_object cl_caadr(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_caadr(x));
}

cl_object cl_cdadr(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_cdadr(x));
}

cl_object cl_caddr(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_caddr(x));
}

cl_object cl_cdddr(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_cdddr(x));
}

cl_object cl_caaaar(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_caaaar(x));
}

cl_object cl_cdaaar(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_cdaaar(x));
}

cl_object cl_cadaar(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_cadaar(x));
}

cl_object cl_cddaar(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_cddaar(x));
}

cl_object cl_caadar(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_caadar(x));
}

cl_object cl_cdadar(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_cdadar(x));
}

cl_object cl_caddar(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_caddar(x));
}

cl_object cl_cdddar(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_cdddar(x));
}

cl_object cl_caaadr(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_caaadr(x));
}

cl_object cl_cdaadr(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_cdaadr(x));
}

cl_object cl_cadadr(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_cadadr(x));
}

cl_object cl_cddadr(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_cddadr(x));
}

cl_object cl_caaddr(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_caaddr(x));
}

cl_object cl_cdaddr(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_cdaddr(x));
}

cl_object cl_cadddr(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_cadddr(x));
}

cl_object cl_cddddr(cl_object x)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_return1(the_env, ecl_cddddr(x));
}

/* END-GENERATED */
