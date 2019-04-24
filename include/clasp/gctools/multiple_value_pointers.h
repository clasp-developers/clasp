/*
    File: multiple_value_pointers.h
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
#ifndef gctools_multiple_value_pointers_H
#define gctools_multiple_value_pointers_H

namespace gctools {

template <class T>
class multiple_values : public smart_ptr<T> {
private:
  mutable size_t _number_of_values;

public:
  multiple_values() : smart_ptr<T>(), _number_of_values(0){};
  multiple_values(void *p, size_t num) : smart_ptr<T>((gc::Tagged)p), _number_of_values(num){};

  multiple_values(const smart_ptr<T> &v, int num) : smart_ptr<T>(v), _number_of_values(num){};

  multiple_values(const smart_ptr<T> &v) : smart_ptr<T>(v), _number_of_values(1){};

  multiple_values(const return_type &v) : smart_ptr<T>((Tagged)v.ret0[0]), _number_of_values(v.nvals){};

  template <class Y>
  multiple_values(const multiple_values<Y> &yy) : smart_ptr<T>(yy), _number_of_values(yy.number_of_values()){};

  static multiple_values<T> createFromValues() {
    core::MultipleValues &mv = core::lisp_multipleValues();
    multiple_values<T> result(mv.getSize() == 0 ? _Nil<core::T_O>() : mv.valueGet(0, mv.getSize()), mv.getSize());
    return result;
  }

  // Save this T_mv's primary value to the multiple value vector,
  // probably so we can restore it later.
  // With our multiple value protocol, the primary value is not
  // written into the vector by default.
  void saveToMultipleValue0() const {
    core::MultipleValues &mv = core::lisp_multipleValues();
    mv.setSize(0);
    mv.valueSet(0, *this);
    mv.setSize(this->number_of_values());
  };

  return_type as_return_type() const {
    return return_type(this->raw_(), this->_number_of_values);
  }

  // Read this T_mv from the multiple value vector.
  void readFromMultipleValue0() {
    core::MultipleValues &mv = core::lisp_multipleValues();
    this->setRaw_(reinterpret_cast<gc::Tagged>(mv[0]));
    this->_number_of_values = mv.getSize();
  };

  operator bool() const {
    return this->theObject != NULL;
  }

#ifdef POLYMORPHIC_SMART_PTR
  virtual
#endif
      size_t
      number_of_values() const {
    return this->_number_of_values;
  };

  inline void valueSet_(int idx, core::T_sp val) {
    core::MultipleValues &mv = core::lisp_multipleValues();
    mv.valueSet(idx, val);
  }

  inline core::T_sp valueGet_(int idx) const {
    core::MultipleValues &mv = core::lisp_multipleValues();
    return mv.valueGet(idx, this->_number_of_values);
  };

  core::T_sp second() const {
    return this->valueGet_(1);
  }
  core::T_sp third() const {
    return this->valueGet_(2);
  }

  void dump() {
    if (this->_number_of_values > 0) {
      string ts = (*this)->__repr__();
      printf(" %s\n", ts.c_str());
      for (int i(1); i < this->_number_of_values; ++i) {
        string ts = _rep_(this->valueGet_(i));
        printf(" %s\n", ts.c_str());
      }
    } else {
      printf("---No values---\n");
    }
  }
};


};


namespace core {
 typedef gctools::multiple_values<T_O> T_mv;

 /* Save a return_type (in the form of a primary value and number of values)
  * into an array of T_O*. Values past the first are taken from lisp_multipleValues.
  * This is intended to be used in a pattern like the following:
  * { T_mv result = whatever;
  *   size_t nvals = whatever.number_of_values();
  *   T_O* mv_temp[nvals];
  *   returnTypeSaveToTemp(nvals, result.raw_(), mv_temp);
  *   ... stuff that messes with values ...
  *   return returnTypeLoadFromTemp(nvals, mv_temp);
  * }
  * FIXME: Formalize with a macro or templates or something? */
 inline void returnTypeSaveToTemp(size_t nvals, T_O* primary, T_O** temp) {
   if (nvals > 0) { // don't store even the primary unless the space actually exists
     core::MultipleValues& mv = core::lisp_multipleValues();
     temp[0] = primary;
     for (size_t i = 1; i < nvals; ++i) {
       temp[i] = mv._Values[i];
     }
   }
 }
 // Build and return a return_type from a temporary vector. See above.
 inline gctools::return_type returnTypeLoadFromTemp(size_t nvals, T_O** temp) {
   core::MultipleValues& mv = core::lisp_multipleValues();
   for (size_t i = 1; i < nvals; ++i) {
     mv._Values[i] = temp[i];
   }
   return gctools::return_type(temp[0], nvals);
 }

 // Similar to returnTypeSaveToTemp, but saves only from lisp_multipleValues.
 inline void multipleValuesSaveToTemp(T_O** temp) {
   core::MultipleValues& mv = core::lisp_multipleValues();
   size_t nvals = mv.getSize();
   for (size_t i = 0; i < nvals; ++i) {
     temp[i] = mv._Values[i];
   }
 }
 // Similar to returnTypeLoadFromTemp, but just writes into lisp_multipleValues.
 inline void multipleValuesLoadFromTemp(size_t nvals, T_O** temp) {
   core::MultipleValues& mv = core::lisp_multipleValues();
   mv.setSize(nvals);
   for (size_t i = 0; i < nvals; ++i) {
     mv._Values[i] = temp[i];
   }
 }
};

extern core::T_mv ValuesFromCons(core::List_sp vals);

namespace gctools { 

#if defined(USE_MPS)
  template <class TO, class FROM>
    multiple_values<TO> dynamic_pointer_cast(const multiple_values<FROM> &ptr) {
    smart_ptr<FROM> sp = ptr;
//  return multiple_values<TO>(gctools::dynamic_pointer_cast<TO>(sp), ptr.number_of_values());
    return multiple_values<TO>(dynamic_cast<TO>(sp), ptr.number_of_values());
  };
#else
  template <class TO, class FROM>
    multiple_values<TO> dynamic_pointer_cast(const multiple_values<FROM> &ptr) {
    smart_ptr<FROM> sp = ptr;
//  return multiple_values<TO>(boost::dynamic_pointer_cast<TO>(sp), ptr.number_of_values());
    return multiple_values<TO>(dynamic_cast<TO>(sp), ptr.number_of_values());
  };
#endif
};

#endif
