/*
    File: mainBoostPython.h
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

//
// (C) 2004 Christian E. Schafmeister
//

#ifndef BOOSTPYTHON_H
#define BOOSTPYTHON_H

#pragma clang diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
#undef _POSIX_C_SOURCE
#undef _XOPEN_SOURCE
#include <Python.h>
#undef isspace
#undef isupper
#undef islower
#undef isalpha
#undef isalnum
#undef toupper
#undef tolower
#include <boost/python.hpp>
#pragma clang diagnostic pop

namespace bp = boost::python;

#ifndef WIN32
#include <Python.h>
#else

#ifdef _DEBUG
#undef _DEBUG
#include <Python.h>
#define _DEBUG
#else
#include <Python.h>
#endif

#endif


inline void IndexError() {
  PyErr_SetString(PyExc_IndexError, "Index too large");
  boost::python::throw_error_already_set();
}

template <class T>
struct std_item {
  typedef typename T::value_type Value;

  //  static Value const& get(T& x, int n) {
  static Value get(T &x, unsigned int n) {
    //    if( n<0 ) n+=x.size();
    if (n >= x.size()) {
      IndexError();
    }
    return x[n];
  }

  static void set(T &x, unsigned int n, const Value &val) {
    //    if( n<0 ) n+=x.size();
    if (n < x.size())
      x[n] = val;
    else
      IndexError();
  }

  static void del(T &x, unsigned int n) {
    //    if( n<0 ) n+=x.size();
    if (n < x.size())
      x.erase((x.begin() + n));
    else
      IndexError();
  }
};

#endif
