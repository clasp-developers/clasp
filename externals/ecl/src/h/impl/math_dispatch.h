/* -*- mode: c; c-basic-offset: 4 -*- */
/*
    math_dispatch.h -- fast dispatch for math functions
*/
/*
    Copyright (c) 2010, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#ifndef ECL_MATH_DISPATCH_H
#define ECL_MATH_DISPATCH_H

#include <ecl/internal.h> /* for unlikely_if */
#include <ecl/impl/math_fenv.h>

typedef cl_object (*math_one_arg_fn)(cl_object);

#ifdef ECL_LONG_FLOAT
#define MATH_LONG_DOUBLE(opt) opt,
#else
#define MATH_LONG_DOUBLE(opt)
#endif
#define MATH_DEF_DISPATCH1_NE(name,id,type,fix,big,ratio,single_float,double_float,long_float,complex) \
    static cl_object name##failed(cl_object x) {                        \
        FEwrong_type_only_arg(id, x, type);                             \
    }                                                                   \
    static const math_one_arg_fn name##dispatch[t_complex+1]= {         \
        name##failed, /* t_start */                                     \
        name##failed, /* t_list */                                      \
        name##failed, /* t_character */                                 \
        fix, big, ratio, /* t_fixnum, bignum, ratio */                  \
        single_float, double_float, /* t_singlefloat, t_doublefloat */  \
        MATH_LONG_DOUBLE(long_float) /* t_longfloat, optional */        \
        complex };                                                      \
    cl_object ecl_##name(cl_object arg)                                 \
    {                                                                   \
        int t = ECL_IMMEDIATE(arg);                                         \
        if (t == 0) {                                                   \
            t = arg->d.t;                                               \
            unlikely_if (t > t_complex) return name##failed(arg);       \
        }                                                               \
        return name##dispatch[t](arg);                                  \
    }
#define MATH_DEF_DISPATCH1(name,id,type,fix,big,ratio,single_float,double_float,long_float,complex) \
    MATH_DEF_DISPATCH1_NE(name##_ne,id,type,fix,big,ratio,single_float,double_float,long_float,complex) \
    cl_object ecl_##name(cl_object arg)                                 \
    {                                                                   \
        cl_object out;                                                  \
        ECL_MATHERR_CLEAR;                                              \
        out = ecl_##name##_ne(arg);                                     \
        ECL_MATHERR_TEST;                                               \
        return out;                                                     \
    }

typedef int (*math_one_arg_bool_fn)(cl_object);
#define MATH_DEF_DISPATCH1_BOOL(name,id,type,fix,big,ratio,single_float,double_float,long_float,complex) \
    static int name##failed(cl_object x) {                              \
        FEwrong_type_only_arg(id, x, type);                             \
    }                                                                   \
    static const math_one_arg_bool_fn name##dispatch[t_complex+1]= {    \
        name##failed, /* t_start */                                     \
        name##failed, /* t_list */                                      \
        name##failed, /* t_character */                                 \
        fix, big, ratio, /* t_fixnum, bignum, ratio */                  \
        single_float, double_float, /* t_singlefloat, t_doublefloat */  \
        MATH_LONG_DOUBLE(long_float) /* t_longfloat, optional */        \
        complex };                                                      \
    int ecl_##name(cl_object arg)                                       \
    {                                                                   \
        int t = ECL_IMMEDIATE(arg);                                         \
        if (t == 0) {                                                   \
            t = arg->d.t;                                               \
            unlikely_if (t > t_complex) return name##failed(arg);       \
        }                                                               \
        return name##dispatch[t](arg);                                  \
    }
#endif /* ECL_MATH_DISPATCH_H */
