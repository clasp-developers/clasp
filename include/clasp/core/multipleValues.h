#pragma once
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

#include <utility>

namespace core {

#pragma GCC visibility push(default)
class MultipleValues {
public: // ctor
  static const int MultipleValuesLimit = MULTIPLE_VALUES_LIMIT;

public: // instance variables here
  size_t _Size;
  T_O* _Values[MultipleValuesLimit];
  /*! Allocate the GCVector in the NonMoveable memory.
         This needs to stay pinned or things will go very bad if functions are called with arguments in
        this array (those beyond the arguments that can be passed in registers) or multiple values are returned
        and this array moved in memory */
public:
  void initialize();

public: // Functions here
  /*! Return the indexed multiple value or nil */
  inline ATTR_WEAK T_sp valueGet(int idx, int number_of_values) const {
    if (idx < number_of_values)
      return T_sp((gctools::Tagged)this->_Values[idx]);
    return nil<T_O>();
  }

  inline T_sp second(int number_of_values) const { return this->valueGet(1, number_of_values); }

  inline T_sp third(int number_of_values) const { return this->valueGet(2, number_of_values); }

  inline T_sp fourth(int number_of_values) const { return this->valueGet(3, number_of_values); }

  void set1(T_sp val) {
    this->_Values[0] = val.raw_();
    this->_Size = 1;
  }

  void setN(T_O* val, size_t vals) {
    this->_Values[0] = val;
    this->_Size = vals;
  }

  T_O** returnValues(size_t start = 0) { return &this->_Values[start]; }

  //        void setMaxSize() { this->_Size = MultipleValuesLimit;};
  void setSize(size_t sz) { this->_Size = sz; };
  size_t getSize() const { return this->_Size; };
  void emplace_back(T_sp a) {
    this->_Values[this->_Size] = a.raw_() /*std::forward<T>(a)*/;
    ++this->_Size;
  };
  /*! Set the value */

  T_O*& operator[](size_t i) { return this->_Values[i]; };

  T_mv readFromMultipleValue0(size_t num) {
    T_O* val0 = this->valueGet(0, num).raw_();
    T_mv res((void*)val0, num);
    return res;
  }

  void saveToMultipleValue0(T_mv val) {
    this->_Values[0] = val.raw_();
    this->setSize(val.number_of_values());
  }

  // saveToTemp and loadFromTemp are like returnTypeSaveToTemp below,
  // but only use the MultipleValues structure.
  inline void saveToTemp(size_t nvals, T_O** temp) {
    for (size_t i = 0; i < nvals; ++i)
      temp[i] = this->_Values[i];
  }

  inline void loadFromTemp(size_t nvals, T_O** temp) {
    this->setSize(nvals);
    for (size_t i = 0; i < nvals; ++i)
      this->_Values[i] = temp[i];
  }

  void valueSet(int i, T_sp val) { this->_Values[i] = val.raw_(); }

  /*! Return a Cons of elements 1 up to but not including iend */
  //  List_sp asCons(int iend) const;
};
#pragma GCC visibility pop
}; // namespace core

#ifdef DEBUG_VALUES
namespace core {
// dump values
extern void dump_values_pos(core::T_sp v, const char* name, int n);
}; // namespace core
#define DUMP_VALUES_POS(v, n) core::dump_values_pos(v, #v, n);
#else
#define DUMP_VALUES_POS(v, n)
#endif

template <class T0, class T1, class T2, class T3, class T4, class T5, class T6, class T7, class T8, class T9, class T10>
inline static gctools::return_type Values(const gctools::smart_ptr<T0>& v0, const gctools::smart_ptr<T1>& v1,
                                          const gctools::smart_ptr<T2>& v2, const gctools::smart_ptr<T3>& v3,
                                          const gctools::smart_ptr<T4>& v4, const gctools::smart_ptr<T5>& v5,
                                          const gctools::smart_ptr<T6>& v6, const gctools::smart_ptr<T7>& v7,
                                          const gctools::smart_ptr<T8>& v8, const gctools::smart_ptr<T9>& v9,
                                          const gctools::smart_ptr<T10>& v10 ) {
  DUMP_VALUES_POS(v0,  11);
  DUMP_VALUES_POS(v1,  11);
  DUMP_VALUES_POS(v2,  11);
  DUMP_VALUES_POS(v3,  11);
  DUMP_VALUES_POS(v4,  11);
  DUMP_VALUES_POS(v5,  11);
  DUMP_VALUES_POS(v6,  11);
  DUMP_VALUES_POS(v7,  11);
  DUMP_VALUES_POS(v8,  11);
  DUMP_VALUES_POS(v9,  11);
  DUMP_VALUES_POS(v10, 11);
  core::MultipleValues& me = (core::lisp_multipleValues());
  me.valueSet(0,   v0);
  me.valueSet(1,   v1);
  me.valueSet(2,   v2);
  me.valueSet(3,   v3);
  me.valueSet(4,   v4);
  me.valueSet(5,   v5);
  me.valueSet(6,   v6);
  me.valueSet(7,   v7);
  me.valueSet(8,   v8);
  me.valueSet(9,   v9);
  me.valueSet(10, v10);
  return gctools::return_type(v0.raw_(), 11);
}
template <class T0, class T1, class T2, class T3, class T4, class T5, class T6, class T7, class T8, class T9>
inline static gctools::return_type Values(const gctools::smart_ptr<T0>& v0, const gctools::smart_ptr<T1>& v1,
                                          const gctools::smart_ptr<T2>& v2, const gctools::smart_ptr<T3>& v3,
                                          const gctools::smart_ptr<T4>& v4, const gctools::smart_ptr<T5>& v5,
                                          const gctools::smart_ptr<T6>& v6, const gctools::smart_ptr<T7>& v7,
                                          const gctools::smart_ptr<T8>& v8, const gctools::smart_ptr<T9>& v9) {
  DUMP_VALUES_POS(v0, 10);
  DUMP_VALUES_POS(v1, 10);
  DUMP_VALUES_POS(v2, 10);
  DUMP_VALUES_POS(v3, 10);
  DUMP_VALUES_POS(v4, 10);
  DUMP_VALUES_POS(v5, 10);
  DUMP_VALUES_POS(v6, 10);
  DUMP_VALUES_POS(v7, 10);
  DUMP_VALUES_POS(v8, 10);
  DUMP_VALUES_POS(v9, 10);
  core::MultipleValues& me = (core::lisp_multipleValues());
  me.valueSet(0, v0);
  me.valueSet(1, v1);
  me.valueSet(2, v2);
  me.valueSet(3, v3);
  me.valueSet(4, v4);
  me.valueSet(5, v5);
  me.valueSet(6, v6);
  me.valueSet(7, v7);
  me.valueSet(8, v8);
  me.valueSet(9, v9);
  return gctools::return_type(v0.raw_(), 10);
}

template <class T0, class T1, class T2, class T3, class T4, class T5, class T6, class T7, class T8>
inline static gctools::return_type
Values(const gctools::smart_ptr<T0>& v0, const gctools::smart_ptr<T1>& v1, const gctools::smart_ptr<T2>& v2,
       const gctools::smart_ptr<T3>& v3, const gctools::smart_ptr<T4>& v4, const gctools::smart_ptr<T5>& v5,
       const gctools::smart_ptr<T6>& v6, const gctools::smart_ptr<T7>& v7, const gctools::smart_ptr<T8>& v8) {
  DUMP_VALUES_POS(v0, 9);
  DUMP_VALUES_POS(v1, 9);
  DUMP_VALUES_POS(v2, 9);
  DUMP_VALUES_POS(v3, 9);
  DUMP_VALUES_POS(v4, 9);
  DUMP_VALUES_POS(v5, 9);
  DUMP_VALUES_POS(v6, 9);
  DUMP_VALUES_POS(v7, 9);
  DUMP_VALUES_POS(v8, 9);
  core::MultipleValues& me = (core::lisp_multipleValues());
  me.valueSet(0, v0);
  me.valueSet(1, v1);
  me.valueSet(2, v2);
  me.valueSet(3, v3);
  me.valueSet(4, v4);
  me.valueSet(5, v5);
  me.valueSet(6, v6);
  me.valueSet(7, v7);
  me.valueSet(8, v8);
  return gctools::return_type(v0.raw_(), 9);
}

template <class T0, class T1, class T2, class T3, class T4, class T5, class T6, class T7>
inline static gctools::return_type Values(const gctools::smart_ptr<T0>& v0, const gctools::smart_ptr<T1>& v1,
                                          const gctools::smart_ptr<T2>& v2, const gctools::smart_ptr<T3>& v3,
                                          const gctools::smart_ptr<T4>& v4, const gctools::smart_ptr<T5>& v5,
                                          const gctools::smart_ptr<T6>& v6, const gctools::smart_ptr<T7>& v7) {
  DUMP_VALUES_POS(v0, 8);
  DUMP_VALUES_POS(v1, 8);
  DUMP_VALUES_POS(v2, 8);
  DUMP_VALUES_POS(v3, 8);
  DUMP_VALUES_POS(v4, 8);
  DUMP_VALUES_POS(v5, 8);
  DUMP_VALUES_POS(v6, 8);
  DUMP_VALUES_POS(v7, 8);
  core::MultipleValues& me = (core::lisp_multipleValues());
  me.valueSet(0, v0);
  me.valueSet(1, v1);
  me.valueSet(2, v2);
  me.valueSet(3, v3);
  me.valueSet(4, v4);
  me.valueSet(5, v5);
  me.valueSet(6, v6);
  me.valueSet(7, v7);
  return gctools::return_type(v0.raw_(), 8);
}

template <class T0, class T1, class T2, class T3, class T4, class T5, class T6>
inline static gctools::return_type Values(const gctools::smart_ptr<T0>& v0, const gctools::smart_ptr<T1>& v1,
                                          const gctools::smart_ptr<T2>& v2, const gctools::smart_ptr<T3>& v3,
                                          const gctools::smart_ptr<T4>& v4, const gctools::smart_ptr<T5>& v5,
                                          const gctools::smart_ptr<T6>& v6) {
  DUMP_VALUES_POS(v0, 7);
  DUMP_VALUES_POS(v1, 7);
  DUMP_VALUES_POS(v2, 7);
  DUMP_VALUES_POS(v3, 7);
  DUMP_VALUES_POS(v4, 7);
  DUMP_VALUES_POS(v5, 7);
  DUMP_VALUES_POS(v6, 7);
  core::MultipleValues& me = (core::lisp_multipleValues());
  me.valueSet(0, v0);
  me.valueSet(1, v1);
  me.valueSet(2, v2);
  me.valueSet(3, v3);
  me.valueSet(4, v4);
  me.valueSet(5, v5);
  me.valueSet(6, v6);
  return gctools::return_type(v0.raw_(), 7);
}

template <class T0, class T1, class T2, class T3, class T4, class T5>
inline static gctools::return_type Values(const gctools::smart_ptr<T0>& v0, const gctools::smart_ptr<T1>& v1,
                                          const gctools::smart_ptr<T2>& v2, const gctools::smart_ptr<T3>& v3,
                                          const gctools::smart_ptr<T4>& v4, const gctools::smart_ptr<T5>& v5) {
  DUMP_VALUES_POS(v0, 6);
  DUMP_VALUES_POS(v1, 6);
  DUMP_VALUES_POS(v2, 6);
  DUMP_VALUES_POS(v3, 6);
  DUMP_VALUES_POS(v4, 6);
  DUMP_VALUES_POS(v5, 6);
  core::MultipleValues& me = (core::lisp_multipleValues());
  me.valueSet(0, v0);
  me.valueSet(1, v1);
  me.valueSet(2, v2);
  me.valueSet(3, v3);
  me.valueSet(4, v4);
  me.valueSet(5, v5);
  return gctools::return_type(v0.raw_(), 6);
}

template <class T0, class T1, class T2, class T3, class T4>
inline static gctools::return_type Values(const gctools::smart_ptr<T0>& v0, const gctools::smart_ptr<T1>& v1,
                                          const gctools::smart_ptr<T2>& v2, const gctools::smart_ptr<T3>& v3,
                                          const gctools::smart_ptr<T4>& v4) {
  DUMP_VALUES_POS(v0, 5);
  DUMP_VALUES_POS(v1, 5);
  DUMP_VALUES_POS(v2, 5);
  DUMP_VALUES_POS(v3, 5);
  DUMP_VALUES_POS(v4, 5);
  core::MultipleValues& me = (core::lisp_multipleValues());
  me.valueSet(0, v0);
  me.valueSet(1, v1);
  me.valueSet(2, v2);
  me.valueSet(3, v3);
  me.valueSet(4, v4);
  return gctools::return_type(v0.raw_(), 5);
}

template <class T0, class T1, class T2, class T3>
inline static gctools::return_type Values(const gctools::smart_ptr<T0>& v0, const gctools::smart_ptr<T1>& v1,
                                          const gctools::smart_ptr<T2>& v2, const gctools::smart_ptr<T3>& v3) {
  DUMP_VALUES_POS(v0, 4);
  DUMP_VALUES_POS(v1, 4);
  DUMP_VALUES_POS(v2, 4);
  DUMP_VALUES_POS(v3, 4);
  core::MultipleValues& me = (core::lisp_multipleValues());
  me.valueSet(0, v0);
  me.valueSet(1, v1);
  me.valueSet(2, v2);
  me.valueSet(3, v3);
  return gctools::return_type(v0.raw_(), 4);
}

template <class T0, class T1, class T2>
inline static gctools::return_type Values(const gctools::smart_ptr<T0>& v0, const gctools::smart_ptr<T1>& v1,
                                          const gctools::smart_ptr<T2>& v2) {
  DUMP_VALUES_POS(v0, 3);
  DUMP_VALUES_POS(v1, 3);
  DUMP_VALUES_POS(v2, 3);
  core::MultipleValues& me = (core::lisp_multipleValues());
  me.valueSet(0, v0);
  me.valueSet(1, v1);
  me.valueSet(2, v2);
  return gctools::return_type(v0.raw_(), 3);
}

template <class T0, class T1>
inline static gctools::return_type Values(const gctools::smart_ptr<T0>& v0, const gctools::smart_ptr<T1>& v1) {
  DUMP_VALUES_POS(v0, 2);
  DUMP_VALUES_POS(v1, 2);
  core::MultipleValues& me = (core::lisp_multipleValues());
  me.valueSet(0, v0);
  me.valueSet(1, v1);
  return gctools::return_type(v0.raw_(), 2);
}

template <class T0> inline static gctools::return_type Values(const gctools::smart_ptr<T0>& v0) {
  DUMP_VALUES_POS(v0, 1);
  return gctools::return_type(v0.raw_(), 1);
}

template <class T0> inline static gctools::return_type Values0() { return gctools::return_type(nil<T0>().raw_(), 0); }

namespace gctools {

inline void fill_frame_multiple_value_return(Frame* frame, size_t& idx, gctools::return_type ret) {
#if LCC_RETURN_VALUES_IN_REGISTERS() == 1
  if (ret.nvals > 0) {
    fill_frame_one(frame, idx, (core::T_O*)ret.ret0[0]);
    core::MultipleValues& mvThreadLocal = core::lisp_multipleValues();
    if (ret.nvals > 1) {
      gctools::fill_frame_nargs_args(frame, idx, ret.nvals - 1, &mvThreadLocal._Values[1]);
    }
  }
#else
#error "Code below should work for more than one register return value"
  if (ret.nvals > 0) {
    gctools::fill_frame_nargs_args(frame, idx, LCC_RETURN_VALUES_IN_REGISTERS(), (core::T_O**)&ret.ret0[0]);
    if (ret.nvals > LCC_RETURN_VALUES_IN_REGISTERS()) {
      core::MultipleValues& mvThreadLocal = core::lisp_multipleValues();
      gctools::fill_frame_nargs_args(frame, idx, ret.nvals - LCC_RETURN_VALUES_IN_REGISTERS(),
                                     &mvThreadLocal._Values[LCC_RETURN_VALUES_IN_REGISTERS()]);
    }
  }
#endif
}

}; // namespace gctools

namespace core {

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
    temp[0] = primary;
    if (nvals > 1) {
      // Only grab this when we really have to - thread local variable
      // access is pretty expensive.
      core::MultipleValues& mv = core::lisp_multipleValues();
      for (size_t i = 1; i < nvals; ++i)
        temp[i] = mv._Values[i];
    }
  }
}
// Build and return a return_type from a temporary vector. See above.
inline gctools::return_type returnTypeLoadFromTemp(size_t nvals, T_O** temp) {
  if (nvals > 1) {
    core::MultipleValues& mv = core::lisp_multipleValues();
    for (size_t i = 1; i < nvals; ++i) {
      mv._Values[i] = temp[i];
    }
  }
  return gctools::return_type(nvals == 0 ? nil<core::T_O>().raw_() : temp[0], nvals);
}
}; // namespace core
