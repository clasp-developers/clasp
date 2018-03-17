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
//#define DEBUG_LEVEL_FULL

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

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("makeSmallMap");
CL_DEFUN SmallMap_sp core__make_small_map() {
  GC_ALLOCATE(SmallMap_O, sm);
  return sm;
};

void SmallMap_O::fields(Record_sp node) {
  // this->Base::fields(node);
  switch (node->stage()) {
  case Record_O::initializing:
  case Record_O::loading: {
    Vector_sp keyValueVec;
    node->field(INTERN_(core, data), keyValueVec);
    this->map.clear();
    for (size_t i(0), iEnd(this->size()); i < iEnd; ++++i) {
      T_sp key = keyValueVec->rowMajorAref(i + 0);
      T_sp val = keyValueVec->rowMajorAref(i + 1);
      this->setf(key, val);
    };
  } break;
  case Record_O::saving: {
    Vector_sp keyValueVec = core__make_vector(cl::_sym_T_O, 2 * this->size());
    size_t idx = 0;
    for ( auto it : this->map ) {
      T_sp key = it.first;
      T_sp val = it.second;
      keyValueVec->rowMajorAset(idx++,key);
      keyValueVec->rowMajorAset(idx++,val);
    }
    node->field(INTERN_(core, data), keyValueVec);
  } break;
  case Record_O::patching: {
    IMPLEMENT_MEF("Add support to patch SmallMap");
  } break;
  }
}



CL_LISPIFY_NAME("map_find");
CL_DEFMETHOD T_sp SmallMap_O::find(T_sp key, T_sp defval) {
  map_type::iterator it = this->map.find(key);
  if (it == this->map.end()) {
    return defval;
  }
  return (*it).second;
}

CL_LISPIFY_NAME("map_setf");
CL_DEFMETHOD void SmallMap_O::setf(T_sp key, T_sp val) {
  pair<map_type::iterator, bool> found = this->map.insert(std::make_pair(key, val));
  found.first->second = val;
}





};
