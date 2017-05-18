/*
    File: ql.h
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
#ifndef _core_quickList_H
#define _core_quickList_H

#include <clasp/core/sourceFileInfo.h>

/*! Handle template class for creating Lisp Cons's on the fly from within C++ and
  applying them to primitive functions */
namespace ql {

/*! list is a class for constructing free-form Cons lists
  from within C++.
  
  eg:
  C++: (ql::list(lisp) , x , y , z , _lisp->symbol(kw::_sym_color) , objBlue ).cons() 
  returns --> (list X Y Z :color BLUE )   where X,Y,Z,BLUE are the objects contained by the
  C++ values x, y, z, objBlue which are of type T_sp
*/

class list {
 private:
  core::T_sp _Head;
  core::T_sp* _Tail;  // ROOT
 public:
  /*! ctor sets up _Lisp and the first element of the Cons */
  list() {
    this->_Head = _Nil<core::T_O>();
    this->_Tail = &this->_Head;
  }

#if 0
  /*! Point _First of this list to the _Tail of another list */
  void point_to_tail(ql::list const &other) {
    _G();
    this->_First = other._Tail;
    this->_Tail = this->_First;
  }

  void clear() {
    _G();
    this->_First = core::Cons_O::create(_Nil<core::T_O>());
    this->_Tail = this->_First;
  }
#endif
  
  void create_from_cons(core::List_sp other) {
    for (auto cur : other) {
      this->operator<<(CONS_CAR(cur));
    }
  }

#if 0
  core::Cons_sp tail() const {
    return this->_Tail;
  }
#endif

  int length() const {
    LIKELY_if (this->_Head.consp()) return this->_Head.unsafe_cons()->proper_list_length();
    return 0;
  };

  inline list &operator<<(core::T_sp const &obj) {
    core::Cons_sp one = core::Cons_O::create(obj);
    if (!this->_Head.consp()) {
      this->_Head = one;
      this->_Tail = &CONS_CDR(this->_Head);
    } else {
      (*this->_Tail) = one;
      this->_Tail = &CONS_CDR(one);
    }
    return *this;
  }

  /*! Insert list into list - should I copy or append (which will modify the argument)? */
  inline list &operator&(core::List_sp l) {
    for (auto cur : l) {
      (*this) << CONS_CAR(cur);
    }
    return *this;
  }

  /*! dot the list argument to the end of the list */
  inline list &dot(core::T_sp arg) {
    (*this->_Tail) = arg;
    return *this;
  }

  inline core::List_sp cons() const {
    return this->_Head;
  }

  /*! Return all of the list including the (usually) dummy first element */
  inline core::List_sp all() const {
    return this->cons();
  }

};

};

#endif // _core_quickList_H
