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
#ifndef _core_random_H_
#define _core_random_H_

#include <boost/multiprecision/gmp.hpp>
// bug from lack of sync sync between GMP and boost versions
#define _mp_size mp11::mp_size
#include <boost/multiprecision/random.hpp>
#include <boost/random.hpp>

#include <clasp/core/clasp_gmpxx.h>
#include <clasp/core/object.h>
#include <clasp/core/numbers.h>

namespace core {
  namespace bmp = boost::multiprecision;
  
SMART(RandomState);

class RandomState_O : public General_O {
  LISP_CLASS(core, ClPkg, RandomState_O, "random-state",General_O);
  //    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
  typedef boost::mt19937 Generator;
  Generator _Producer;
//  boost::mt11213b _Producer;

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
      this->_Producer = temp_gen; // this->_Producer.seed(tt);
    } else {
      Generator temp_gen(0);
      this->_Producer = temp_gen; //this->_Producer.seed(0);
    }
  };
  explicit RandomState_O(const RandomState_O &state) {
    this->_Producer = state._Producer;
  };
  virtual ~RandomState_O() {}

  CL_DEFMETHOD std::string random_state_get() {
    stringstream ss;
    ss << this->_Producer;
    return ss.str();
  }
  CL_DEFMETHOD void random_state_set(const std::string& s) {
    stringstream ss(s);
    ss >> this->_Producer;
  }

 public: // Functions here
  static RandomState_sp make(T_sp state);
  static RandomState_sp create(RandomState_sp other) {
    GC_ALLOCATE_VARIADIC(RandomState_O, b, *other);
    return b;
  };
  static RandomState_sp create_random() {
    GC_ALLOCATE_VARIADIC(RandomState_O, b, true );
    return b;
  }

}; // RandomState class

}; // core namespace

#endif /* _random_H_ */
