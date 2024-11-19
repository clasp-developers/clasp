#pragma once

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

namespace gctools {

template <class T> class multiple_values : public smart_ptr<T> {
private:
  mutable size_t _number_of_values;

public:
  multiple_values() : smart_ptr<T>(), _number_of_values(0){};
  multiple_values(void* p, size_t num) : smart_ptr<T>((gc::Tagged)p), _number_of_values(num){};

  multiple_values(const smart_ptr<T>& v, int num) : smart_ptr<T>(v), _number_of_values(num){};

  multiple_values(const smart_ptr<T>& v) : smart_ptr<T>(v), _number_of_values(1){};

  multiple_values(const return_type& v) : smart_ptr<T>((Tagged)v.ret0[0]), _number_of_values(v.nvals){};

  template <class Y> multiple_values(const multiple_values<Y>& yy) : smart_ptr<T>(yy), _number_of_values(yy.number_of_values()){};

#if 0
  static multiple_values<T> createFromValues() {
    core::MultipleValues &mv = core::lisp_multipleValues();
    multiple_values<T> result(mv.getSize() == 0 ? nil<core::T_O>() : mv.valueGet(0, mv.getSize()), mv.getSize());
    return result;
  }

  // Save this T_mv's primary value to the multiple value vector,
  // probably so we can restore it later.
  // With our multiple value protocol, the primary value is not
  // written into the vector by default; this function does so
  // explicitly.
  void saveToMultipleValue0() const {
    core::MultipleValues &mv = core::lisp_multipleValues();
    mv.valueSet(0, *this);
    mv.setSize(this->number_of_values());
  };

  // Read this T_mv from the multiple value vector.
  void readFromMultipleValue0() {
    core::MultipleValues &mv = core::lisp_multipleValues();
    this->setRaw_(reinterpret_cast<gc::Tagged>(mv[0]));
    this->_number_of_values = mv.getSize();
  };
#endif

  void set_number_of_values(size_t num) { this->_number_of_values = num; };
  return_type as_return_type() const { return return_type(this->raw_(), this->_number_of_values); }

  operator bool() const { return this->theObject != NULL; }

#ifdef POLYMORPHIC_SMART_PTR
  virtual
#endif
      size_t
      number_of_values() const {
    return this->_number_of_values;
  };

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

}; // namespace gctools

namespace core {
typedef gctools::multiple_values<T_O> T_mv;
static_assert(std::is_trivially_copyable_v<T_mv>);
};

extern core::T_mv ValuesFromCons(core::List_sp vals);
