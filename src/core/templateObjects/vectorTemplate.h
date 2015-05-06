/*
    File: vectorTemplate.h
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
#ifndef _core_Vector_template_O_H
#define _core_Vector_template_O_H

#include "core/object.h"
#include "core/lispVector.h"
#include "corePackage.fwd.h"

namespace core {

template <class T>
class Vector_template_O : public Vector_O {
  LISP_BASE1(Vector_O);
  LISP_TEMPLATE_CLASS(Vector_template_O);

public:
  typedef T valueType;
  typedef gctools::smart_ptr<T> sharedValueType;
  typedef Vector_template_O<T> vectorType;
  typedef gctools::smart_ptr<vectorType> sharedVectorType;

public:
  Vector_template_O() : T_O(), Vector_O(){};
  virtual ~Vector_template_O(){};

protected: // instance variables here
  Vector0<T> _Values;

public:
  void setup(sharedValueType initialElement, Cons_sp initialContents, int dimension) {
    _G();
    if (initialElement->notNil() && initialContents->notNil()) {
      THROW(_lisp->error(BF("You can only specify one of initial-element or initialContents")));
    }
    if (initialContents->notNil()) {
      this->_Values.resize(dimension);
      int i = 0;
      for (Cons_sp cur = initialContents; cur->notNil(); cur = cur->cdr(), ++i) {
        if (i >= dimension)
          break;
        this->_Values[i] = cur->ocar()->as<valueType>();
      }
      for (int j = i; j < dimension; j++) {
        this->_Values[j] = valueType::_nil;
      }
    } else {
      this->_Values.resize(dimension, initialElement);
    }
  }

  //	void setElementType(T_sp elementType) { this->_ElementType = elementType;};
  //	T_sp elementType() const { return this->_ElementType;};
public: // Functions here
  // Add predicates here
  // bool arraySymbolsP() const { return this->isObject();};
  // bool vectorSymbolsP() const { return this->isObject();};

  int dimension() const { return this->_Values.size(); };

  virtual void setf_rowMajorAref(int idx, T_sp value) {
    _G();
    ASSERTF(idx < this->length(), BF("Index %d is out of range (<%d)") % idx % this->length());
    this->_Values[idx] = value->as<valueType>();
  }

  virtual void swapElements(uint i1, uint i2) {
    sharedValueType t = this->_Values[i2];
    this->_Values[i2] = this->_Values[i1];
    this->_Values[i1] = t;
  }

  virtual T_sp rowMajorAref(int idx) const {
    _G();
    ASSERTF(idx < this->length(), BF("Index %d is out of range (<%d)") % idx % this->length());
    return this->_Values[idx];
  }

  virtual int arrayRowMajorIndex(Cons_sp indices) const {
    ASSERTF(indices->length() == 1, BF("Vectors have only one dimension - you passed indices %s") % indices->__repr__());
    return indices->ocar()->as<Fixnum_O>()->get();
  }

  virtual T_sp aref(Cons_sp indices) const {
    _G();
    ASSERTF(indices->length() == 1, BF("Vectors only support one index - passed: %s") % indices->__repr__());
    return this->elt(indices->ocar()->as<Integer_O>()->as_int());
  }

  virtual T_sp setf_aref(Cons_sp indices_val) {
    _G();
    ASSERTF(indices_val->length() == 2, BF("Vectors only support one index followed by a value - passed: %s") % indices_val->__repr__());
    return this->setf_elt(indices_val->ocar()->as<Integer_O>()->as_int(), indices_val->ocadr());
  }

  virtual T_sp elt(int index) const {
    _G();
    return this->_Values[index];
  }

  virtual T_sp setf_elt(int index, T_sp value) {
    _G();
    this->_Values[index] = value->as<valueType>();
    return value;
  }

  virtual T_sp svref(int index) const { return this->elt(index); };
  virtual T_sp setf_svref(int index, T_sp value) { return this->setf_elt(index, value); };

  virtual void fillArrayWithElt(T_sp element, Fixnum_sp start, T_sp end) {
    _G();
    uint istart = start->get();
    uint last = this->_Values.size();
    uint iend = last - 1;
    if (end->notNil())
      iend = end->as<Fixnum_O>()->get();
    ASSERTF(iend >= istart, BF("Illegal fill range istart=%d iend=%d") % istart % iend);
    ASSERTF(iend < last, BF("Illegal value for end[%d] - must be between istart[%d] and less than %d") % iend % istart % last);
    ASSERTF(istart >= 0 <= iend, BF("Illegal value for start[%d] - must be between 0 and %d") % istart % iend);
    for (uint i = istart; i < iend; i++) {
      this->_Values[i] = element->as<valueType>();
    }
  }

  uint length() const { return this->_Values.size(); };
  string __repr__() const {
    _G();
    stringstream ss;
    ss << "#( ";
    for (int i = 0; i < this->_Values.size(); i++) {
      ss << this->_Values[i]->__repr__() << " ";
    }
    ss << ")";
    return ss.str();
  };

  virtual Sequence_sp subseq(int istart, T_sp end) const {
    _G();
    int iend = (end->isNil()) ? this->length() : end->as<Fixnum_O>()->get();
    if (istart < 0 || iend > this->length()) {
      THROW(_lisp->error(BF("out of bounds for subseq")));
    }
    sharedVectorType result(RP_Create<vectorType>(_lisp));
    int isize = iend - istart;
    result->_Values.resize(isize);
    for (int i = 0; i < isize; ++i) {
      result->_Values[i] = this->_Values[istart];
      istart++;
    }
    return result;
  }

  virtual Sequence_sp setf_subseq(int start, T_sp end, Sequence_sp new_subseq) {
    _G();
    IMPLEMENT_ME();
  };
};

}; /* core */

#endif /* _core_Vector_template_O_H */
