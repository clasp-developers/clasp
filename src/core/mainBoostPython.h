       
       
//
// (C) 2004 Christian E. Schafmeister
//



#ifndef	BOOSTPYTHON_H
#define	BOOSTPYTHON_H

#pragma clang diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
#undef _POSIX_C_SOURCE
#undef _XOPEN_SOURCE
#include "Python.h"
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

#ifndef	WIN32
#include <Python.h>
#else

#ifdef	_DEBUG
#undef	_DEBUG
#include <Python.h>
#define	_DEBUG
#else
#include <Python.h>
#endif

#endif

#include "foundation.h"


inline void IndexError() {
    PyErr_SetString(PyExc_IndexError,"Index too large");
    boost::python::throw_error_already_set();
}

template<class T>
struct std_item
{
  typedef typename T::value_type Value;

//  static Value const& get(T& x, int n) {
static Value get(T& x, unsigned int n) 
{
//    if( n<0 ) n+=x.size();
    if ( n >= x.size() )
    { 
	IndexError(); 
    }
    return x[n];
}

  static void set(T& x, unsigned int n, const Value& val) {
//    if( n<0 ) n+=x.size();
    if( n<x.size() ) x[n]=val;
    else IndexError(); 
  }

  static void del(T& x, unsigned int n) {
//    if( n<0 ) n+=x.size();
    if( n<x.size() ) x.erase((x.begin()+n));
    else IndexError(); }
};




#endif

