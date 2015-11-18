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
#include <clasp/core/smallMultimap.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/environment.h>
#include <clasp/core/cons.h>
#include <clasp/core/numbers.h>
#include <clasp/core/wrappers.h>

namespace core {

#define ARGS_core_makeSmallMultimap "()"
#define DECL_core_makeSmallMultimap ""
#define DOCS_core_makeSmallMultimap "makeSmallMultimap"
SmallMultimap_sp core_makeSmallMultimap() {
  _G();
  GC_ALLOCATE(SmallMultimap_O, sm);
  return sm;
};

void SmallMultimap_O::describe() {
  for (auto it = this->map.begin(); it != this->map.end(); ++it) {
    printf("%s:%d  key: %s   value: %s\n", __FILE__, __LINE__, _rep_(it->first).c_str(), _rep_(it->second).c_str());
  }
}

void SmallMultimap_O::describeRange(T_sp key) {
  pair<map_type::iterator, map_type::iterator> range = this->map.equal_range(key);
  for (auto it = range.first; it != range.second; ++it) {
    printf("%s:%d  key: %s   value: %s\n", __FILE__, __LINE__, _rep_(it->first).c_str(), _rep_(it->second).c_str());
  }
}

void SmallMultimap_O::insert(T_sp key, T_sp val) {
  pair<map_type::iterator, bool> found = this->map.insert(std::make_pair(key, val));
  (void)found;
}

void SmallMultimap_O::exposeCando(Lisp_sp lisp) {
  class_<SmallMultimap_O>()
      .def("small_multimap_describe", &SmallMultimap_O::describe)
      .def("small_multimap_describe_range", &SmallMultimap_O::describeRange)
      .def("small_multimap_insert", &SmallMultimap_O::insert)
      .def("small_multimap_size", &SmallMultimap_O::size);
  CoreDefun(makeSmallMultimap);
}

void SmallMultimap_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, SmallMultimap, "", "", _lisp);
#endif
}

EXPOSE_CLASS(core, SmallMultimap_O);
};
