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
#include <clasp/core/bformat.h>
#include <clasp/core/smallMultimap.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/environment.h>
#include <clasp/core/cons.h>
#include <clasp/core/numbers.h>
#include <clasp/core/wrappers.h>

namespace core {

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("makeSmallMultimap");
CL_DEFUN SmallMultimap_sp core__make_small_multimap() {
  GC_ALLOCATE(SmallMultimap_O, sm);
  return sm;
};

CL_LISPIFY_NAME("small_multimap_describe");
CL_DEFMETHOD void SmallMultimap_O::describe() {
  for (auto it = this->map.begin(); it != this->map.end(); ++it) {
    write_bf_stream(BF("%s:%d  key: %s   value: %s\n") % __FILE__ % __LINE__ % _rep_(it->first) % _rep_(it->second));
  }
}

CL_LISPIFY_NAME("small_multimap_describe_range");
CL_DEFMETHOD void SmallMultimap_O::describeRange(T_sp key) {
  pair<map_type::iterator, map_type::iterator> range = this->map.equal_range(key);
  for (auto it = range.first; it != range.second; ++it) {
    write_bf_stream(BF("%s:%d  key: %s   value: %s\n") % __FILE__ % __LINE__ % _rep_(it->first) % _rep_(it->second));
  }
}

CL_LISPIFY_NAME("small_multimap_insert");
CL_DEFMETHOD void SmallMultimap_O::insert(T_sp key, T_sp val) {
  pair<map_type::iterator, bool> found = this->map.insert(std::make_pair(key, val));
  (void)found;
}

CL_LISPIFY_NAME("small_multimap_contains");
CL_DEFMETHOD bool SmallMultimap_O::contains(T_sp key) {
  return this->map.contains(key);
}



};
