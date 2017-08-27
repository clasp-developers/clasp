/*
    File: gctoolsPackage.h
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

#ifndef gctoolsCando_H
#define gctoolsCando_H

#include <clasp/core/common.h>
#include <clasp/gctools/gctoolsPackage.fwd.h>

namespace gctools {

extern bool _GlobalDebugAllocations;

class GcToolsExposer_O : public core::Exposer_O {
private:
public:
  GcToolsExposer_O(core::Lisp_sp lisp) : Exposer_O(lisp, GcToolsPkg){};
  virtual void expose(core::Lisp_sp lisp, WhatToExpose what) const;
};

class TestingClass {
public:
  int x;
  TestingClass() : x(0){};
  TestingClass(int i) : x(i){};
  void dump() const { printf("%d ", x); };
};

void gctools__cleanup(bool verbose);

void initialize_bootstrap_kinds();
};
#endif
