/*
    File: primitives.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
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


#ifndef CLBIND_PRIMITIVES_HPP_INCLUDED
#define CLBIND_PRIMITIVES_HPP_INCLUDED

#include <algorithm>
#include <cstring>

#include "clbind/config.h"
//#include <clbind/detail/yes_no.hpp>

namespace clbind {
    namespace detail
{
	template<class T>
	struct identity
	{
		typedef T type;
	};

	template<class T>
    struct type_ {};

/*	typedef char yes_t;
	typedef double no_t;*/

	struct cl_to_cpp {};
	struct cpp_to_cl {};


	template<class T> struct by_value {};
	template<class T> struct by_reference {};
	template<class T> struct by_const_reference {};
	template<class T> struct by_pointer {};
	template<class T> struct by_const_pointer {};

	struct converter_policy_tag {};

	struct ltstr
	{
		bool operator()(const char* s1, const char* s2) const { return std::strcmp(s1, s2) < 0; }
	};

	template<int N>
	struct aligned 
	{
		char storage[N];
	};

	// returns the offset added to a Derived* when cast to a Base*
	// TODO: return ptrdiff
	template<class Derived, class Base>
	int ptr_offset(type_<Derived>, type_<Base>)
	{
		aligned<sizeof(Derived)> obj;
		Derived* ptr = reinterpret_cast<Derived*>(&obj);

		return int(static_cast<char*>(static_cast<void*>(static_cast<Base*>(ptr)))
		- static_cast<char*>(static_cast<void*>(ptr)));
	}

}}

namespace clbind {
    template <class T> class move_if_unique_ptr_and_forward_otherwise {
    public:
        static T&& doit(T&& t) {
            return std::forward<T>(t);
        };
    };

    template <class U> class move_if_unique_ptr_and_forward_otherwise<std::unique_ptr<U>> {
    public:
        static std::unique_ptr<U>&& doit(std::unique_ptr<U>&& t) {
            return std::move(t);
        }
    };
};

#endif // CLBIND_PRIMITIVES_HPP_INCLUDED
