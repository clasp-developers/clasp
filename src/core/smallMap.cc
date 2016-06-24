/*
    File: smallMap.cc
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
#define DEBUG_LEVEL_FULL

//
// (C) 2004 Christian E. Schafmeister
//

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/smallMap.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/environment.h>
#include <clasp/core/cons.h>
#include <clasp/core/numbers.h>
#include <clasp/core/wrappers.h>

namespace core {

LAMBDA();
DECLARE();
DOCSTRING("makeSmallMap");
CL_DEFUN SmallMap_sp core__make_small_map() {
  _G();
  GC_ALLOCATE(SmallMap_O, sm);
  return sm;
};

T_sp SmallMap_O::find(T_sp key, T_sp defval) {
  map_type::iterator it = this->map.find(key);
  if (it == this->map.end()) {
    return defval;
  }
  return (*it).second;
}

void SmallMap_O::setf(T_sp key, T_sp val) {
  pair<map_type::iterator, bool> found = this->map.insert(std::make_pair(key, val));
  found.first->second = val;
}

void SmallMap_O::exposeCando(Lisp_sp lisp) {
  class_<SmallMap_O>()
      .def("map_find", &SmallMap_O::find)
      .def("map_setf", &SmallMap_O::setf)
      .def("map_size", &SmallMap_O::size)
      .def("map_capacity", &SmallMap_O::capacity);
}

void SmallMap_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, SmallMap, "", "", _lisp);
#endif
}

EXPOSE_CLASS(core, SmallMap_O);
};
