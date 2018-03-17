/*
    File: intArray.cc
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

#define DEBUG_LEVEL_NONE

#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/intArray.h>
#include <clasp/core/wrappers.h>

namespace core {

//
// Constructor
//

void IntArray_O::initialize() {
  this->Base::initialize();
  this->_Ints.clear();
}

#if defined(XML_ARCHIVE)
void IntArray_O::archive(core::ArchiveP node) {
  node->archiveVectorInt("values", this->_Ints);
}
#endif // defined(XML_ARCHIVE)

IntArray_sp IntArray_O::create(uint sz) {
  IntArray_sp ir = IntArray_O::create();
  ir->resize(sz);
  return ir;
}

void IntArray_O::clear() {
  this->_Ints.clear();
}

void IntArray_O::resize(uint sz) {
  ASSERT(sz > 0);
  this->_Ints.resize(sz);
}

void IntArray_O::append(int val) {
  this->_Ints.push_back(val);
}

void IntArray_O::put(uint idx, int val) {
  ASSERT_lessThan(idx, this->_Ints.size());
  this->_Ints[idx] = val;
}

int IntArray_O::get(uint idx) {
  ASSERT_lessThan(idx, this->_Ints.size());
  return this->_Ints[idx];
}
};
