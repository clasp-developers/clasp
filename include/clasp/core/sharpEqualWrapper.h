#pragma once
/*
    File: sharpEqualWrapper.h
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

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/object.h>
#include <clasp/core/symbol.h>
#include <clasp/core/array.h>
// #include "stringSet.fwd.h"
#include <clasp/core/environment.fwd.h>
#include <clasp/core/cons.h>

namespace core {

SYMBOL_EXPORT_SC_(CorePkg, _PLUS_sharp_marker_PLUS_);

SMART(SharpEqualWrapper);
class SharpEqualWrapper_O : public General_O {
  LISP_CLASS(core, CorePkg, SharpEqualWrapper_O, "SharpEqualWrapper", General_O);

private:
  T_sp _Value;
  T_sp _Label;

public:
  CL_DEFMETHOD T_sp sharp_equal_wrapper_value() const { return this->_Value; };
  void set_sharp_equal_wrapper_value(T_sp v) { this->_Value = v;};
  string __repr__() const;

  SharpEqualWrapper_O(T_sp label) : _Value(_sym__PLUS_sharp_marker_PLUS_), _Label(label){};
};

DOCGROUP(clasp)
inline CL_DEFUN SharpEqualWrapper_sp make_sharp_equal_wrapper(T_sp label) {
  auto sew = gctools::GC<SharpEqualWrapper_O>::allocate(label);
  return sew;
}

CL_DOCSTRING("Return true if sharp-tag")
DOCGROUP(clasp)
CL_DEFUN inline bool core__sharp_equal_wrapper_p(T_sp o) { return gc::IsA<SharpEqualWrapper_sp>(o); }

}; // namespace core
