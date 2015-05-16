/*
    File: multipleValues.h
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
#ifndef _core_MultipleValues_H
#define _core_MultipleValues_H

#include <utility>

namespace core {

#pragma GCC visibility push(default)
class MultipleValues {
public: // ctor
  static const int MultipleValuesLimit = CALL_ARGUMENTS_LIMIT;

public: // instance variables here
  size_t _Size;
  T_O *_Values[MultipleValuesLimit];
  /*! Allocate the GCVector in the NonMoveable memory.
         This needs to stay pinned or things will go very bad if functions are called with arguments in
        this array (those beyond the arguments that can be passed in registers) or multiple values are returned
        and this array moved in memory */
public:
  void initialize();

public: // Functions here
  /*! Return the indexed multiple value or nil */
  ATTR_WEAK T_sp valueGet(int idx, int number_of_arguments) const;

  //        GC_RESULT scanGCRoots(GC_SCAN_ARGS_PROTOTYPE);

  /*! When calling functions with more arguments than can be passed in registers 
	  the remaining arguments are written into and read out of a MultipleValues array.
	callingArgs returns a pointer to the first value for when the values passed in registers
	need to be written into the array for easier parsing*/
  T_O **callingArgsStart() { return &this->_Values[0]; }
  /*! Return the address of the first array entry where extra arguments beyond the 
	  number that can be passed in registers would be written */
  T_O **callingArgsExtraArgStart() { return &this->_Values[LCC_ARGS_IN_REGISTERS]; }

  T_sp setFromConsSkipFirst(List_sp values);

  //        void setMaxSize() { this->_Size = MultipleValuesLimit;};
  void setSize(size_t sz) { this->_Size = sz; };
  size_t getSize() const { return this->_Size; };
  void emplace_back(T_sp a) {
    this->_Values[this->_Size] = a.raw_() /*std::forward<T>(a)*/;
    ++this->_Size;
  };
  /*! Set the value */

  T_O *&operator[](size_t i) { return this->_Values[i]; };

  void valueSet(int i, T_sp val) {
    this->_Values[i] = val.raw_();
  }

  /*! Return a Cons of elements 1 up to but not including iend */
  List_sp asCons(int iend) const;

#if 0
	void saveToVec0(T_mv& result, gctools::Vec0<core::T_sp>& vec)
        {
            vec.resize(result.this->getSize());
            for ( int i(0), iEnd(this->getSize()); i<iEnd; ++i ) {
                vec[i] = T_sp(this->_Values[i]);
            }
        }
        void loadFromVec0(const gctools::Vec0<core::T_sp>& vec)
        {
            this->setSize(vec.size());
            for ( int i(0), iEnd(vec.size()); i<iEnd; ++i ) {
                this->_Values[i] = vec[i].px;
            };
        };

        void saveToVec0(gctools::Vec0<core::T_sp>& vec)
        {
            vec.resize(this->getSize());
            for ( int i(0), iEnd(this->getSize()); i<iEnd; ++i ) {
                vec[i] = T_sp(this->_Values[i]);
            }
        }
        void loadFromVec0(const gctools::Vec0<core::T_sp>& vec)
        {
            this->setSize(vec.size());
            for ( int i(0), iEnd(vec.size()); i<iEnd; ++i ) {
                this->_Values[i] = vec[i].px;
            };
        };
#endif
};
#pragma GCC visibility pop
};

#include <clasp/gctools/multiple_value_pointers.h>

namespace core {

typedef gctools::multiple_values<T_O> T_mv;
void multipleValuesSaveToVector(T_mv values, VectorObjects_sp save);

core::T_mv multipleValuesLoadFromVector(core::VectorObjects_sp load);
};

extern core::T_mv ValuesFromCons(core::List_sp vals);

template <class T0, class T1, class T2, class T3, class T4, class T5, class T6, class T7, class T8, class T9>
static gctools::multiple_values<T0> Values(const gctools::smart_ptr<T0> &v0,
                                           const gctools::smart_ptr<T1> &v1,
                                           const gctools::smart_ptr<T2> &v2,
                                           const gctools::smart_ptr<T3> &v3,
                                           const gctools::smart_ptr<T4> &v4,
                                           const gctools::smart_ptr<T5> &v5,
                                           const gctools::smart_ptr<T6> &v6,
                                           const gctools::smart_ptr<T7> &v7,
                                           const gctools::smart_ptr<T8> &v8,
                                           const gctools::smart_ptr<T9> &v9) {
  core::MultipleValues &me = (core::lisp_multipleValues());
  me.setSize(0);
  me.emplace_back(v0);
  me.emplace_back(v1);
  me.emplace_back(v2);
  me.emplace_back(v3);
  me.emplace_back(v4);
  me.emplace_back(v5);
  me.emplace_back(v6);
  me.emplace_back(v7);
  me.emplace_back(v8);
  me.emplace_back(v9);
  return gctools::multiple_values<T0>(v0, 10);
}

template <class T0, class T1, class T2, class T3, class T4, class T5, class T6, class T7, class T8>
static gctools::multiple_values<T0> Values(const gctools::smart_ptr<T0> &v0,
                                           const gctools::smart_ptr<T1> &v1,
                                           const gctools::smart_ptr<T2> &v2,
                                           const gctools::smart_ptr<T3> &v3,
                                           const gctools::smart_ptr<T4> &v4,
                                           const gctools::smart_ptr<T5> &v5,
                                           const gctools::smart_ptr<T6> &v6,
                                           const gctools::smart_ptr<T7> &v7,
                                           const gctools::smart_ptr<T8> &v8) {
  core::MultipleValues &me = (core::lisp_multipleValues());
  me.setSize(0);
  me.emplace_back(v0);
  me.emplace_back(v1);
  me.emplace_back(v2);
  me.emplace_back(v3);
  me.emplace_back(v4);
  me.emplace_back(v5);
  me.emplace_back(v6);
  me.emplace_back(v7);
  me.emplace_back(v8);
  return gctools::multiple_values<T0>(v0, 9);
}

template <class T0, class T1, class T2, class T3, class T4, class T5, class T6, class T7>
static gctools::multiple_values<T0> Values(const gctools::smart_ptr<T0> &v0,
                                           const gctools::smart_ptr<T1> &v1,
                                           const gctools::smart_ptr<T2> &v2,
                                           const gctools::smart_ptr<T3> &v3,
                                           const gctools::smart_ptr<T4> &v4,
                                           const gctools::smart_ptr<T5> &v5,
                                           const gctools::smart_ptr<T6> &v6,
                                           const gctools::smart_ptr<T7> &v7) {
  core::MultipleValues &me = (core::lisp_multipleValues());
  me.setSize(0);
  me.emplace_back(v0);
  me.emplace_back(v1);
  me.emplace_back(v2);
  me.emplace_back(v3);
  me.emplace_back(v4);
  me.emplace_back(v5);
  me.emplace_back(v6);
  me.emplace_back(v7);
  return gctools::multiple_values<T0>(v0, 8);
}

template <class T0, class T1, class T2, class T3, class T4, class T5, class T6>
static gctools::multiple_values<T0> Values(const gctools::smart_ptr<T0> &v0,
                                           const gctools::smart_ptr<T1> &v1,
                                           const gctools::smart_ptr<T2> &v2,
                                           const gctools::smart_ptr<T3> &v3,
                                           const gctools::smart_ptr<T4> &v4,
                                           const gctools::smart_ptr<T5> &v5,
                                           const gctools::smart_ptr<T6> &v6) {
  core::MultipleValues &me = (core::lisp_multipleValues());
  me.setSize(0);
  me.emplace_back(v0);
  me.emplace_back(v1);
  me.emplace_back(v2);
  me.emplace_back(v3);
  me.emplace_back(v4);
  me.emplace_back(v5);
  me.emplace_back(v6);
  return gctools::multiple_values<T0>(v0, 7);
}

template <class T0, class T1, class T2, class T3, class T4, class T5>
static gctools::multiple_values<T0> Values(const gctools::smart_ptr<T0> &v0,
                                           const gctools::smart_ptr<T1> &v1,
                                           const gctools::smart_ptr<T2> &v2,
                                           const gctools::smart_ptr<T3> &v3,
                                           const gctools::smart_ptr<T4> &v4,
                                           const gctools::smart_ptr<T5> &v5) {
  core::MultipleValues &me = (core::lisp_multipleValues());
  me.setSize(0);
  me.emplace_back(v0);
  me.emplace_back(v1);
  me.emplace_back(v2);
  me.emplace_back(v3);
  me.emplace_back(v4);
  me.emplace_back(v5);
  return gctools::multiple_values<T0>(v0, 6);
}

template <class T0, class T1, class T2, class T3, class T4>
static gctools::multiple_values<T0> Values(const gctools::smart_ptr<T0> &v0,
                                           const gctools::smart_ptr<T1> &v1,
                                           const gctools::smart_ptr<T2> &v2,
                                           const gctools::smart_ptr<T3> &v3,
                                           const gctools::smart_ptr<T4> &v4) {
  core::MultipleValues &me = (core::lisp_multipleValues());
  me.setSize(0);
  me.emplace_back(v0);
  me.emplace_back(v1);
  me.emplace_back(v2);
  me.emplace_back(v3);
  me.emplace_back(v4);
  return gctools::multiple_values<T0>(v0, 5);
}

template <class T0, class T1, class T2, class T3>
static gctools::multiple_values<T0> Values(const gctools::smart_ptr<T0> &v0,
                                           const gctools::smart_ptr<T1> &v1,
                                           const gctools::smart_ptr<T2> &v2,
                                           const gctools::smart_ptr<T3> &v3) {
  core::MultipleValues &me = (core::lisp_multipleValues());
  me.setSize(0);
  me.emplace_back(v0);
  me.emplace_back(v1);
  me.emplace_back(v2);
  me.emplace_back(v3);
  return gctools::multiple_values<T0>(v0, 4);
}

template <class T0, class T1, class T2>
static gctools::multiple_values<T0> Values(const gctools::smart_ptr<T0> &v0,
                                           const gctools::smart_ptr<T1> &v1,
                                           const gctools::smart_ptr<T2> &v2) {
  core::MultipleValues &me = (core::lisp_multipleValues());
  me.setSize(0);
  me.emplace_back(v0);
  me.emplace_back(v1);
  me.emplace_back(v2);
  return gctools::multiple_values<T0>(v0, 3);
}

template <class T0, class T1>
static gctools::multiple_values<T0> Values(const gctools::smart_ptr<T0> &v0,
                                           const gctools::smart_ptr<T1> &v1) {
  core::MultipleValues &me = (core::lisp_multipleValues());
  me.setSize(0);
  me.emplace_back(v0);
  me.emplace_back(v1);
  return gctools::multiple_values<T0>(v0, 2);
}

template <class T0>
static gctools::multiple_values<T0> Values(const gctools::smart_ptr<T0> &v0) {
  core::MultipleValues &me = (core::lisp_multipleValues());
  me.setSize(0);
  me.emplace_back(v0);
  return gctools::multiple_values<T0>(v0, 1);
}

template <class T0>
static gctools::multiple_values<T0> Values0() {
  core::MultipleValues &me = (core::lisp_multipleValues());
  me.setSize(0);
  return gctools::multiple_values<T0>(_Nil<T0>(), 0);
}

#if 0
#define DEFINE_RETURN_VALUE_TYPE(_ty_) \
  template <>                          \
  inline _ty_ Values1(_ty_ v0) { return v0; }
#else
#define DEFINE_RETURN_VALUE_TYPE(_ty_)
#endif

#define MULTIPLE_VALUES_RETURN()
/*! Does nothing, just notes that a call returns multiple values */

#define MULTIPLE_VALUES_CONTEXT()
//#define MULTIPLE_VALUES_SPECIAL_ACCESS(_var_) core::MultipleValues* _var_ = core::lisp_multipleValues()

#define RET(_x_x_x_) return (Values1(_x_x_x_))

#define RET_POD(_x_x_x_) return (_x_x_x_)

#define RET_REF(_x_x_x_) return (_x_x_x_)

#define RET_PASS_THROUGH(_t_t_t_) return (Values_pass_through<_t_t_t_>())

#endif /* _core_MultipleValues_H */
