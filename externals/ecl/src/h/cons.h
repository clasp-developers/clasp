/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    cons.h -- list manipulation macros & functions
*/
/*
    Copyright (c) 2011, Juan Jose Garcia-Ripoll

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#ifndef ECL_CONS_H
#define ECL_CONS_H

#include <ecl/ecl.h>

#ifdef __cplusplus
extern "C" {
#endif

#define Null(x)		((x)==ECL_NIL)
#define CONS(a,d)	ecl_cons((a),(d))
#define ACONS(a,b,c)	ecl_cons(ecl_cons((a),(b)),(c))

/* BEGIN-GENERATED (gen-cons-h) */

#if ECL_CAN_INLINE
static ECL_INLINE cl_object _ecl_car(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

static ECL_INLINE cl_object _ecl_cdr(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

static ECL_INLINE cl_object _ecl_caar(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

static ECL_INLINE cl_object _ecl_cdar(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

static ECL_INLINE cl_object _ecl_cadr(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

static ECL_INLINE cl_object _ecl_cddr(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

static ECL_INLINE cl_object _ecl_caaar(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

static ECL_INLINE cl_object _ecl_cdaar(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

static ECL_INLINE cl_object _ecl_cadar(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

static ECL_INLINE cl_object _ecl_cddar(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

static ECL_INLINE cl_object _ecl_caadr(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

static ECL_INLINE cl_object _ecl_cdadr(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

static ECL_INLINE cl_object _ecl_caddr(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CAR(x);
  return x;
}

static ECL_INLINE cl_object _ecl_cdddr(cl_object x)
{
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  if (Null(x)) return x;
  x = ECL_CONS_CDR(x);
  return x;
}

static ECL_INLINE cl_object _ecl_caaaar(cl_object x)
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

static ECL_INLINE cl_object _ecl_cdaaar(cl_object x)
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

static ECL_INLINE cl_object _ecl_cadaar(cl_object x)
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

static ECL_INLINE cl_object _ecl_cddaar(cl_object x)
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

static ECL_INLINE cl_object _ecl_caadar(cl_object x)
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

static ECL_INLINE cl_object _ecl_cdadar(cl_object x)
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

static ECL_INLINE cl_object _ecl_caddar(cl_object x)
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

static ECL_INLINE cl_object _ecl_cdddar(cl_object x)
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

static ECL_INLINE cl_object _ecl_caaadr(cl_object x)
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

static ECL_INLINE cl_object _ecl_cdaadr(cl_object x)
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

static ECL_INLINE cl_object _ecl_cadadr(cl_object x)
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

static ECL_INLINE cl_object _ecl_cddadr(cl_object x)
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

static ECL_INLINE cl_object _ecl_caaddr(cl_object x)
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

static ECL_INLINE cl_object _ecl_cdaddr(cl_object x)
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

static ECL_INLINE cl_object _ecl_cadddr(cl_object x)
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

static ECL_INLINE cl_object _ecl_cddddr(cl_object x)
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

#else
extern ECL_API cl_object _ecl_car(cl_object);
extern ECL_API cl_object _ecl_cdr(cl_object);
extern ECL_API cl_object _ecl_caar(cl_object);
extern ECL_API cl_object _ecl_cdar(cl_object);
extern ECL_API cl_object _ecl_cadr(cl_object);
extern ECL_API cl_object _ecl_cddr(cl_object);
extern ECL_API cl_object _ecl_caaar(cl_object);
extern ECL_API cl_object _ecl_cdaar(cl_object);
extern ECL_API cl_object _ecl_cadar(cl_object);
extern ECL_API cl_object _ecl_cddar(cl_object);
extern ECL_API cl_object _ecl_caadr(cl_object);
extern ECL_API cl_object _ecl_cdadr(cl_object);
extern ECL_API cl_object _ecl_caddr(cl_object);
extern ECL_API cl_object _ecl_cdddr(cl_object);
extern ECL_API cl_object _ecl_caaaar(cl_object);
extern ECL_API cl_object _ecl_cdaaar(cl_object);
extern ECL_API cl_object _ecl_cadaar(cl_object);
extern ECL_API cl_object _ecl_cddaar(cl_object);
extern ECL_API cl_object _ecl_caadar(cl_object);
extern ECL_API cl_object _ecl_cdadar(cl_object);
extern ECL_API cl_object _ecl_caddar(cl_object);
extern ECL_API cl_object _ecl_cdddar(cl_object);
extern ECL_API cl_object _ecl_caaadr(cl_object);
extern ECL_API cl_object _ecl_cdaadr(cl_object);
extern ECL_API cl_object _ecl_cadadr(cl_object);
extern ECL_API cl_object _ecl_cddadr(cl_object);
extern ECL_API cl_object _ecl_caaddr(cl_object);
extern ECL_API cl_object _ecl_cdaddr(cl_object);
extern ECL_API cl_object _ecl_cadddr(cl_object);
extern ECL_API cl_object _ecl_cddddr(cl_object);
#endif /* !ECL_CAN_INLINE */

extern ECL_API cl_object ecl_car(cl_object);
extern ECL_API cl_object ecl_cdr(cl_object);
extern ECL_API cl_object ecl_caar(cl_object);
extern ECL_API cl_object ecl_cdar(cl_object);
extern ECL_API cl_object ecl_cadr(cl_object);
extern ECL_API cl_object ecl_cddr(cl_object);
extern ECL_API cl_object ecl_caaar(cl_object);
extern ECL_API cl_object ecl_cdaar(cl_object);
extern ECL_API cl_object ecl_cadar(cl_object);
extern ECL_API cl_object ecl_cddar(cl_object);
extern ECL_API cl_object ecl_caadr(cl_object);
extern ECL_API cl_object ecl_cdadr(cl_object);
extern ECL_API cl_object ecl_caddr(cl_object);
extern ECL_API cl_object ecl_cdddr(cl_object);
extern ECL_API cl_object ecl_caaaar(cl_object);
extern ECL_API cl_object ecl_cdaaar(cl_object);
extern ECL_API cl_object ecl_cadaar(cl_object);
extern ECL_API cl_object ecl_cddaar(cl_object);
extern ECL_API cl_object ecl_caadar(cl_object);
extern ECL_API cl_object ecl_cdadar(cl_object);
extern ECL_API cl_object ecl_caddar(cl_object);
extern ECL_API cl_object ecl_cdddar(cl_object);
extern ECL_API cl_object ecl_caaadr(cl_object);
extern ECL_API cl_object ecl_cdaadr(cl_object);
extern ECL_API cl_object ecl_cadadr(cl_object);
extern ECL_API cl_object ecl_cddadr(cl_object);
extern ECL_API cl_object ecl_caaddr(cl_object);
extern ECL_API cl_object ecl_cdaddr(cl_object);
extern ECL_API cl_object ecl_cadddr(cl_object);
extern ECL_API cl_object ecl_cddddr(cl_object);

#define CAR(x) _ecl_car(x)
#define CDR(x) _ecl_cdr(x)
#define CAAR(x) _ecl_caar(x)
#define CDAR(x) _ecl_cdar(x)
#define CADR(x) _ecl_cadr(x)
#define CDDR(x) _ecl_cddr(x)
#define CAAAR(x) _ecl_caaar(x)
#define CDAAR(x) _ecl_cdaar(x)
#define CADAR(x) _ecl_cadar(x)
#define CDDAR(x) _ecl_cddar(x)
#define CAADR(x) _ecl_caadr(x)
#define CDADR(x) _ecl_cdadr(x)
#define CADDR(x) _ecl_caddr(x)
#define CDDDR(x) _ecl_cdddr(x)
#define CAAAAR(x) _ecl_caaaar(x)
#define CDAAAR(x) _ecl_cdaaar(x)
#define CADAAR(x) _ecl_cadaar(x)
#define CDDAAR(x) _ecl_cddaar(x)
#define CAADAR(x) _ecl_caadar(x)
#define CDADAR(x) _ecl_cdadar(x)
#define CADDAR(x) _ecl_caddar(x)
#define CDDDAR(x) _ecl_cdddar(x)
#define CAAADR(x) _ecl_caaadr(x)
#define CDAADR(x) _ecl_cdaadr(x)
#define CADADR(x) _ecl_cadadr(x)
#define CDDADR(x) _ecl_cddadr(x)
#define CAADDR(x) _ecl_caaddr(x)
#define CDADDR(x) _ecl_cdaddr(x)
#define CADDDR(x) _ecl_cadddr(x)
#define CDDDDR(x) _ecl_cddddr(x)

extern ECL_API cl_object cl_car(cl_object);
extern ECL_API cl_object cl_cdr(cl_object);
extern ECL_API cl_object cl_caar(cl_object);
extern ECL_API cl_object cl_cdar(cl_object);
extern ECL_API cl_object cl_cadr(cl_object);
extern ECL_API cl_object cl_cddr(cl_object);
extern ECL_API cl_object cl_caaar(cl_object);
extern ECL_API cl_object cl_cdaar(cl_object);
extern ECL_API cl_object cl_cadar(cl_object);
extern ECL_API cl_object cl_cddar(cl_object);
extern ECL_API cl_object cl_caadr(cl_object);
extern ECL_API cl_object cl_cdadr(cl_object);
extern ECL_API cl_object cl_caddr(cl_object);
extern ECL_API cl_object cl_cdddr(cl_object);
extern ECL_API cl_object cl_caaaar(cl_object);
extern ECL_API cl_object cl_cdaaar(cl_object);
extern ECL_API cl_object cl_cadaar(cl_object);
extern ECL_API cl_object cl_cddaar(cl_object);
extern ECL_API cl_object cl_caadar(cl_object);
extern ECL_API cl_object cl_cdadar(cl_object);
extern ECL_API cl_object cl_caddar(cl_object);
extern ECL_API cl_object cl_cdddar(cl_object);
extern ECL_API cl_object cl_caaadr(cl_object);
extern ECL_API cl_object cl_cdaadr(cl_object);
extern ECL_API cl_object cl_cadadr(cl_object);
extern ECL_API cl_object cl_cddadr(cl_object);
extern ECL_API cl_object cl_caaddr(cl_object);
extern ECL_API cl_object cl_cdaddr(cl_object);
extern ECL_API cl_object cl_cadddr(cl_object);
extern ECL_API cl_object cl_cddddr(cl_object);
/* END-GENERATED */

#ifdef __cplusplus
}
#endif

#endif /* !ECL_CONS_H */
