/*
    File: config.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister

CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

See directory 'clasp/licenses' for full details.

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
// Copyright (c) 2003 Daniel Wallin and Arvid Norberg

// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
// ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
// TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
// SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
// ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
// OR OTHER DEALINGS IN THE SOFTWARE.

#ifndef CLBIND_CONFIG_HPP_INCLUDED
#define CLBIND_CONFIG_HPP_INCLUDED

#include <boost/config.hpp>

#ifdef BOOST_MSVC
#define CLBIND_ANONYMOUS_FIX static
#else
#define CLBIND_ANONYMOUS_FIX
#endif

#if defined(BOOST_MSVC) && (BOOST_MSVC <= 1200)

#define for if (false){} else for

#include <cstring>

namespace std {
using ::strlen;
using ::strcmp;
using ::type_info;
}

#endif

#if defined(BOOST_MSVC) && (BOOST_MSVC <= 1300)
#define CLBIND_MSVC_TYPENAME
#else
#define CLBIND_MSVC_TYPENAME typename
#endif

// the maximum number of arguments of functions that's
// registered. Must at least be 2
#ifndef CLBIND_MAX_ARITY
#define CLBIND_MAX_ARITY 10
#elif CLBIND_MAX_ARITY <= 1
#undef CLBIND_MAX_ARITY
#define CLBIND_MAX_ARITY 2
#endif

// the maximum number of classes one class
// can derive from
// max bases must at least be 1
#ifndef CLBIND_MAX_BASES
#define CLBIND_MAX_BASES 4
#elif CLBIND_MAX_BASES <= 0
#undef CLBIND_MAX_BASES
#define CLBIND_MAX_BASES 1
#endif

// CLBIND_NO_ERROR_CHECKING
// define this to remove all error checks
// this will improve performance and memory
// footprint.
// if it is defined matchers will only be called on
// overloaded functions, functions that's
// not overloaded will be called directly. The
// parameters on the lua stack are assumed
// to match those of the function.
// exceptions will still be catched when there's
// no error checking.

// CLBIND_NOT_THREADSAFE
// this define will make clbind non-thread safe. That is,
// it will rely on a static variable. You can still have
// multiple lua states and use coroutines, but only
// one of your real threads may run lua code.

// CLBIND_NO_EXCEPTIONS
// this define will disable all usage of try, catch and throw in
// clbind. This will in many cases disable runtime-errors, such
// as invalid casts, when calling lua-functions that fails or
// returns values that cannot be converted by the given policy.
// Clbind requires that no function called directly or indirectly
// by clbind throws an exception (throwing exceptions through
// C code has undefined behavior, lua is written in C).
#ifdef CLBIND_DYNAMIC_LINK
#ifdef BOOST_WINDOWS
#ifdef CLBIND_BUILDING
#define CLBIND_API __declspec(dllexport)
#else
#define CLBIND_API __declspec(dllimport)
#endif
#else
#if defined(_GNUC_) && _GNUC_ >= 4
#define CLBIND_API __attribute__((visibility("default")))
#else
#define CLBIND_API __attribute__((visibility("default")))
#endif
#endif
#endif

#ifndef CLBIND_API
#define CLBIND_API
#endif

namespace clbind {

CLBIND_API void disable_super_deprecation();

} // namespace clbind

#endif // CLBIND_CONFIG_HPP_INCLUDED
