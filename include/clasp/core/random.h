#pragma once
/*
    File: random.h
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

#include <random>

#include <clasp/core/clasp_gmpxx.h>
#include <clasp/core/object.h>
#include <clasp/core/numbers.h>

template <> struct gctools::GCInfo<core::RandomState_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};

namespace core {

SMART(RandomState);

class RandomState_O : public General_O {
  LISP_CLASS(core, ClPkg, RandomState_O, "random-state", General_O);
  //    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
  typedef std::mt19937 Generator;
  dont_expose<Generator> _Producer;

public: // ctor/dtor for classes with shared virtual base
  explicit RandomState_O(bool random = false) {
    if (random) {
      clock_t currentTime;
#ifdef darwin
      currentTime = mach_absolute_time();
#else
      currentTime = clock();
#endif
      uint tt = currentTime;
      tt = currentTime % 32768;
      Generator temp_gen(static_cast<uint>(tt));
      this->_Producer._value = temp_gen; // this->_Producer.seed(tt);
    } else {
      Generator temp_gen(0);
      this->_Producer._value = temp_gen; // this->_Producer.seed(0);
    }
  };
  explicit RandomState_O(const RandomState_O& state) { this->_Producer._value = state._Producer._value; };

  CL_DEFMETHOD std::string random_state_get() const {
    stringstream ss;
    ss << this->_Producer._value;
    return ss.str();
  }
  CL_DEFMETHOD RandomState_sp random_state_set(const std::string& s) {
    stringstream ss(s);
    ss >> this->_Producer._value;
    return this->asSmartPtr();
  }

public: // Functions here
  static RandomState_sp make(T_sp state);
  static RandomState_sp create(RandomState_sp other) {
    auto b = gctools::GC<RandomState_O>::allocate(*other);
    return b;
  };
  static RandomState_sp create_random() {
    auto b = gctools::GC<RandomState_O>::allocate(true);
    return b;
  }

  virtual void __write__(T_sp strm) const;
  virtual void __writeReadable__(T_sp strm) const;

}; // RandomState class

}; // namespace core
