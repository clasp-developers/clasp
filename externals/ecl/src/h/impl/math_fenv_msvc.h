/* -*- mode: c; c-basic-offset: 4 -*- */
/*
    math_msvc_fenv.h -- fake fenv.h using Microsoft Visual C++
*/
/*
    Copyright (c) 2010, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#ifndef ECL_MATH_FENV_MSVC_H
#define ECL_MATH_FENV_MSVC_H

#define HAVE_FEENABLEEXCEPT
#define HAVE_FENV_H

#include <float.h>

#if defined(_MSC_VER)
# define FE_DIVBYZERO _EM_ZERODIVIDE
# define FE_OVERFLOW  _EM_OVERFLOW
# define FE_UNDERFLOW _EM_UNDERFLOW
# define FE_INVALID   _EM_INVALID
# define FE_INEXACT   _EM_INEXACT
typedef int fenv_t;
#else
# ifdef _MCW_EM
#  define MCW_EM _MCW_EM
# else
#  define MCW_EM 0x0008001F
# endif
# define fenv_t int
#endif

#define feenableexcept(bits) do { \
    int cw = _controlfp(0,0); cw &= ~(bits); _controlfp(cw,MCW_EM); } while(0)
#define fedisableexcept(bits) do { \
    int cw = _controlfp(0,0); cw |= (bits); _controlfp(cw,MCW_EM); } while(0)
#define feholdexcept(bits) do { \
    *(bits) = _controlfp(0,0); _controlfp(0xffffffff, MCW_EM); } while(0)
#define fesetenv(bits) do { _controlfp(*(bits), MCW_EM); } while (0)
#define feupdateenv(bits) fesetenv(bits)
#define feclearexcept(bits) _clearfp()
#define fetestexcept(bits) (_clearfp() & (bits))

#endif /* !ECL_MATH_FENV_MSVC_H */
