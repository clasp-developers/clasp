/*
    File: primitives.h
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

#ifndef CLBIND_PRIMITIVES_HPP_INCLUDED
#define CLBIND_PRIMITIVES_HPP_INCLUDED

#include <algorithm>
#include <cstring>

#include <clasp/clbind/config.h>
//#include <clasp/clbind/detail/yes_no.hpp>

namespace clbind {
namespace detail {
template <class T>
struct identity {
  typedef T type;
};

template <class T>
struct type_ {};

/*	typedef char yes_t;
	typedef double no_t;*/

struct cl_to_cpp {};
struct cpp_to_cl {};

template <class T>
struct by_value {};
template <class T>
struct by_reference {};
template <class T>
struct by_const_reference {};
template <class T>
struct by_pointer {};
template <class T>
struct by_const_pointer {};

struct converter_policy_tag {};

struct ltstr {
  bool operator()(const char *s1, const char *s2) const { return std::strcmp(s1, s2) < 0; }
};

template <int N>
struct aligned {
  char storage[N];
};

// returns the offset added to a Derived* when cast to a Base*
// TODO: return ptrdiff
template <class Derived, class Base>
int ptr_offset(type_<Derived>, type_<Base>) {
  aligned<sizeof(Derived)> obj;
  Derived *ptr = reinterpret_cast<Derived *>(&obj);

  return int(static_cast<char *>(static_cast<void *>(static_cast<Base *>(ptr))) - static_cast<char *>(static_cast<void *>(ptr)));
}
}
}

namespace clbind {
template <class T>
class move_if_unique_ptr_and_forward_otherwise {
public:
  static T &&doit(T &&t) {
    return std::forward<T>(t);
  };
};

template <class U>
class move_if_unique_ptr_and_forward_otherwise<std::unique_ptr<U>> {
public:
  static std::unique_ptr<U> &&doit(std::unique_ptr<U> &&t) {
    return std::move(t);
  }
};




template <typename T>
struct format_descriptor {
  static std::string format() { SIMPLE_ERROR(BF("subclass for the type T")); };
};

template <>
struct format_descriptor<double> {
    static std::string format() { return std::string("d"); }
};

template <>
struct format_descriptor<float> {
    static std::string format() { return std::string("f"); }
};

#if 0
struct buffer_info {
    void *ptr = nullptr;          // Pointer to the underlying storage
    ssize_t itemsize = 0;         // Size of individual items in bytes
    ssize_t size = 0;             // Total number of entries
    std::string format;           // For homogeneous buffers, this should be set to format_descriptor<T>::format()
    ssize_t ndim = 0;             // Number of dimensions
    std::vector<ssize_t> shape;   // Shape of the tensor (1 entry per dimension)
    std::vector<ssize_t> strides; // Number of bytes between adjacent entries (for each per dimension)
    bool readonly = false;        // flag to indicate if the underlying storage may be written to

    buffer_info() { }

    buffer_info(void *ptr, ssize_t itemsize, const std::string &format, ssize_t ndim,
                detail::any_container<ssize_t> shape_in, detail::any_container<ssize_t> strides_in, bool readonly=false)
    : ptr(ptr), itemsize(itemsize), size(1), format(format), ndim(ndim),
      shape(std::move(shape_in)), strides(std::move(strides_in)), readonly(readonly) {
        if (ndim != (ssize_t) shape.size() || ndim != (ssize_t) strides.size())
          SIMPLE_ERROR(BF("buffer_info: ndim doesn't match shape and/or strides length"));
        for (size_t i = 0; i < (size_t) ndim; ++i)
            size *= shape[i];
    }

    template <typename T>
    buffer_info(T *ptr, detail::any_container<ssize_t> shape_in, detail::any_container<ssize_t> strides_in, bool readonly=false)
    : buffer_info(private_ctr_tag(), ptr, sizeof(T), format_descriptor<T>::format(), static_cast<ssize_t>(shape_in->size()), std::move(shape_in), std::move(strides_in), readonly) { }

    buffer_info(void *ptr, ssize_t itemsize, const std::string &format, ssize_t size, bool readonly=false)
    : buffer_info(ptr, itemsize, format, 1, {size}, {itemsize}, readonly) { }

    template <typename T>
    buffer_info(T *ptr, ssize_t size, bool readonly=false)
    : buffer_info(ptr, sizeof(T), format_descriptor<T>::format(), size, readonly) { }

    template <typename T>
    buffer_info(const T *ptr, ssize_t size, bool readonly=true)
    : buffer_info(const_cast<T*>(ptr), sizeof(T), format_descriptor<T>::format(), size, readonly) { }

#if 0
  explicit buffer_info(Py_buffer *view, bool ownview = true)
    : buffer_info(view->buf, view->itemsize, view->format, view->ndim,
            {view->shape, view->shape + view->ndim}, {view->strides, view->strides + view->ndim}, view->readonly) {
        this->view = view;
        this->ownview = ownview;
    }
#endif
    buffer_info(const buffer_info &) = delete;
    buffer_info& operator=(const buffer_info &) = delete;

    buffer_info(buffer_info &&other) {
        (*this) = std::move(other);
    }
#if 0
    buffer_info& operator=(buffer_info &&rhs) {
        ptr = rhs.ptr;
        itemsize = rhs.itemsize;
        size = rhs.size;
        format = std::move(rhs.format);
        ndim = rhs.ndim;
        shape = std::move(rhs.shape);
        strides = std::move(rhs.strides);
        std::swap(view, rhs.view);
        std::swap(ownview, rhs.ownview);
        readonly = rhs.readonly;
        return *this;
    }

    ~buffer_info() {
        if (view && ownview) { PyBuffer_Release(view); delete view; }
    }
#endif
private:
    struct private_ctr_tag { };

    buffer_info(private_ctr_tag, void *ptr, ssize_t itemsize, const std::string &format, ssize_t ndim,
                detail::any_container<ssize_t> &&shape_in, detail::any_container<ssize_t> &&strides_in, bool readonly)
    : buffer_info(ptr, itemsize, format, ndim, std::move(shape_in), std::move(strides_in), readonly) { }

#if 0
  Py_buffer *view = nullptr;
  bool ownview = false;
#endif
};
#endif



};

#endif // CLBIND_PRIMITIVES_HPP_INCLUDED
