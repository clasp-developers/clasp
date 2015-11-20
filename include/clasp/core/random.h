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

#include <clasp/core/clasp_gmpxx.h>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/numbers.h>

namespace core {

SMART(RandomState);

class RandomState_O : public T_O {
  LISP_BASE1(T_O);
  LISP_CLASS(core, ClPkg, RandomState_O, "random-state");
  //	DECLARE_INIT();
  //    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
  boost::mt11213b _Producer;

public: // ctor/dtor for classes with shared virtual base
  explicit RandomState_O() {
    clock_t currentTime;
    int tt;
#ifdef darwin
    currentTime = mach_absolute_time();
#else
    currentTime = clock();
#endif
    tt = currentTime % 32768;
    this->_Producer.seed(static_cast<uint>(tt));
  };
  explicit RandomState_O(const RandomState_O &state) {
    this->_Producer = state._Producer;
  };
  virtual ~RandomState_O() {}

public: // Functions here
  static RandomState_sp make(T_sp state);
  static RandomState_sp create(RandomState_sp other) {
    GC_ALLOCATE_VARIADIC(RandomState_O, b, *other);
    return b;
  };

}; // RandomState class

}; // core namespace
TRANSLATE(core::RandomState_O);

#endif /* _random_H_ */
