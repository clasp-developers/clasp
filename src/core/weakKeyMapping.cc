/*
    File: weakKeyMapping.cc
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

#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/weakKeyMapping.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//

CL_LISPIFY_NAME(make-weak-key-mapping);
CL_DEFUN WeakKeyMapping_sp WeakKeyMapping_O::make(T_sp key, T_sp tval) {
  T_sp val(tval);
  if (key == tval) {
    val = gctools::smart_ptr<T_O>(gctools::tag_sameAsKey<T_O *>());
  }
  GC_ALLOCATE_VARIADIC(WeakKeyMapping_O, me, key, val);
  return me;
};






#if defined(OLD_SERIALIZE)
void WeakKeyMapping_O::serialize(serialize::SNode snode) {
  CR_HINT(snode, false);
  snode->archiveWeakKeyMapping("weakObject", this->_WeakObject);
  CR_HINT(snode, false);
}
#endif // defined(OLD_SERIALIZE)

#if defined(XML_ARCHIVE)
void WeakKeyMapping_O::archiveBase(ArchiveP node) {
  this->Base::archiveBase(node);
  node->archiveWeakKeyMapping("weakObject", this->_WeakObject);
}
#endif // defined(XML_ARCHIVE)

CL_LISPIFY_NAME("weakKeyMappingValid");
CL_DEFMETHOD bool WeakKeyMapping_O::valid() const {
  return this->_WeakObject.valid();
}

/*! Return (values key value t) or (values nil nil nil) */
CL_LISPIFY_NAME("weakKeyMappingKeyValue");
CL_DEFMETHOD T_mv WeakKeyMapping_O::keyValue() const {
  _OF();
  return this->_WeakObject.keyValue();
}

}; /* core */
