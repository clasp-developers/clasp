/*
    File: arrayObjects.h
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
#ifndef _core_ArrayObjects_H
#define _core_ArrayObjects_H

#include <clasp/core/foundation.h>
#include <clasp/core/array.h>
#include <clasp/core/corePackage.fwd.h>

namespace core {

FORWARD(ArrayObjects);
class ArrayObjects_O : public Array_O {
  LISP_BASE1(Array_O);
  LISP_CLASS(core, CorePkg, ArrayObjects_O, "ArrayObjects");
#if defined(XML_ARCHIVE)
  DECLARE_ARCHIVE();
#endif // defined(XML_ARCHIVE)
#if defined(OLD_SERIALIZE)
  DECLARE_SERIALIZE();
#endif // defined(OLD_SERIALIZE)
public:
  explicit ArrayObjects_O() : Base(){};
  virtual ~ArrayObjects_O(){};
  friend void write_array_inner(bool, T_sp, T_sp);

public:
  void initialize();

private: // instance variables here
  vector<cl_index> _Dimensions;
  T_sp _ElementType;
  gctools::Vec0<T_sp> _Values;

public: // Functions here
  static ArrayObjects_sp make(T_sp dim, T_sp elementType, T_sp initialElement, T_sp adjustable);

public:
  virtual bool equalp(T_sp other) const;
  virtual T_sp aset_unsafe(int j, T_sp val);
  T_sp elementType() const { return this->_ElementType; };

  virtual void rowMajorAset(cl_index idx, T_sp value);
  virtual T_sp rowMajorAref(cl_index idx) const;

  virtual gc::Fixnum rank() const { return this->_Dimensions.size(); };

  /* Copy the dimensions for printing */
  virtual std::vector<cl_index> dimensions() const { return this->_Dimensions; };
  virtual gc::Fixnum arrayDimension(gc::Fixnum axisNumber) const;
  virtual T_sp aref_unsafe(cl_index index) const { return this->_Values[index]; };

  LongLongInt setDimensions(List_sp dims, T_sp initialElement);

  void setElementType(T_sp et) { this->_ElementType = et; };
  /*! Return the value at the indices */
  virtual T_sp aref(List_sp indices) const;

  /*! Return the value at the indices */
  virtual T_sp setf_aref(List_sp indices_val);

  /*! Return a shallow copy of this object */
  virtual T_sp shallowCopy() const;

  /*! Return the value at the indices */
  virtual void arrayFill(T_sp val);

  /*! Return a deepCopy of the ArrayObjects */
  virtual T_sp deepCopy() const;

  virtual T_sp svref(int index) const;
  virtual T_sp setf_svref(int index, T_sp value);
};

}; /* core */

TRANSLATE(core::ArrayObjects_O);

#endif /* _core_ArrayObjects_H */
