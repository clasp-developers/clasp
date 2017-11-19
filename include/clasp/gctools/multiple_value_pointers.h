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

  void saveToMultipleValue0() const {
    core::MultipleValues &mv = core::lisp_multipleValues();
    mv.setSize(0);
    mv.valueSet(0, *this);
    mv.setSize(this->number_of_values());
  };

  return_type as_return_type() const {
    return return_type(this->raw_(), this->_number_of_values);
  }

  void readFromMultipleValue0() {
    core::MultipleValues &mv = core::lisp_multipleValues();
    this->setRaw_(reinterpret_cast<gc::Tagged>(mv[0]));
    this->_number_of_values = mv.getSize();
  };

  void saveToVec0(::gctools::Vec0<core::T_sp> &values) {
#if 0
    if ( this->_number_of_values < 0 || this->_number_of_values >= CALL_ARGUMENTS_LIMIT ) {
      printf("%s:%d  Illegal number of return values: %zu\n", __FILE__, __LINE__, this->_number_of_values);
    }
#endif
    values.resize(this->_number_of_values);
    values[0] = *this;
    core::MultipleValues &mv = core::lisp_multipleValues();
    for (int i(1); i < this->_number_of_values; ++i) {
#ifdef TRAP
      if ( (((uintptr_clasp_t)mv._Values[i])&gctools::tag_mask) == unused0_tag) {
        printf("%s:%d Caught bad tagged pointer\n", __FILE__, __LINE__ );
        abort();
      }
#endif
      core::T_sp val((gctools::Tagged)mv._Values[i]);
      values[i] = val;
#ifdef TRAP
      if ( (((uintptr_clasp_t)values[i].raw_())&gctools::tag_mask) == unused0_tag) {
        printf("%s:%d Caught bad tagged pointer\n", __FILE__, __LINE__ );
        abort();
      }
#endif
    }
  }

  void loadFromVec0(const ::gctools::Vec0<core::T_sp> &values) {
    core::MultipleValues &mv = core::lisp_multipleValues();
    for (size_t i(1), iEnd(values.size()); i < iEnd; ++i) {
#ifdef TRAP
      if ( (((uintptr_clasp_t)values[i].raw_())&gctools::tag_mask) == unused0_tag) {
        printf("%s:%d Caught bad tagged pointer\n", __FILE__, __LINE__ );
        abort();
      }
#endif
      mv._Values[i] = values[i].raw_();
#ifdef TRAP
      if ( (((uintptr_clasp_t)mv._Values[i])&gctools::tag_mask) == unused0_tag) {
        printf("%s:%d Caught bad tagged pointer\n", __FILE__, __LINE__ );
        abort();
      }
#endif
    }
    this->_number_of_values = values.size();
    this->setRaw_(reinterpret_cast<gc::Tagged>(values[0].raw_()));
    //	    GCTOOLS_ASSERT(this->valid());
  }

  operator bool() const {
    return this->theObject != NULL;
  }

#ifdef POLYMORPHIC_SMART_PTR
  virtual
#endif
      int
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
 
 // The SimpleVector needs to be created with SimpleVector_O::create_for_multiple_values
 void multipleValuesSaveToVector(T_mv values, SimpleVector_sp save);
 inline size_t multipleValuesLength(SimpleVector_sp values);
 core::T_mv multipleValuesLoadFromVector(SimpleVector_sp load);

 inline void multipleValuesSaveToMultipleValues(T_mv values, MultipleValues* destmv)
 {
   core::MultipleValues& mv = core::lisp_multipleValues();
   destmv->_Values[0] = values.raw_();
   for ( int i(1); i<values.number_of_values(); ++i ) {
     destmv->_Values[i] = mv._Values[i];
   }
   destmv->_Size = values.number_of_values();;
 }
 inline gctools::return_type multipleValuesLoadFromMultipleValues(MultipleValues* sourcemv)
 {
   core::MultipleValues& mv = core::lisp_multipleValues();
   for ( int i(1); i<sourcemv->_Size; ++i ) {
//     printf("%s:%d    multipleValuesLoadFromMultipleValues[%d] --> %s\n", __FILE__, __LINE__, i, _rep_(val));
     mv._Values[i] = sourcemv->_Values[i];
   }
//   printf("%s:%d    multipleValuesLoadFromMultipleValues[%d] --> %s\n", __FILE__, __LINE__, i, _rep_(val0));
   return gctools::return_type(sourcemv->_Values[0],sourcemv->_Size);
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
