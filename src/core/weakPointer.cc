/*
    File: weakPointer.cc
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
#include <clasp/core/weakPointer.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//

CL_LISPIFY_NAME(make-weak-pointer);
CL_DEFUN WeakPointer_sp WeakPointer_O::make(T_sp obj) {
  GC_ALLOCATE_VARIADIC(WeakPointer_O, me, obj);
  me->_WeakObject.pointer = gctools::WeakPointerManager::AllocatorType::allocate(obj);
#ifdef USE_BOEHM
  GCTOOLS_ASSERT(me->_WeakObject.pointer->value.objectp());
  if (!unboundOrDeletedOrSplatted(me->_WeakObject.pointer->value)) {
    GC_general_register_disappearing_link(reinterpret_cast<void **>(&me->_WeakObject.pointer->value.rawRef_()), reinterpret_cast<void *>(me->_WeakObject.pointer->value.rawRef_()));
  } else {
    GCTOOLS_ASSERT(false); // ERROR("value can never contain anything but a pointer - if it does then when it gets set to NULL by the BoehmGC it will be interpreted as a Fixnum 0!!!!!");
  }
#endif
  return me;
};






#if defined(OLD_SERIALIZE)
void WeakPointer_O::serialize(serialize::SNode snode) {
  CR_HINT(snode, false);
  snode->archiveWeakPointer("weakObject", this->_WeakObject);
  CR_HINT(snode, false);
}
#endif // defined(OLD_SERIALIZE)

#if defined(XML_ARCHIVE)
void WeakPointer_O::archiveBase(ArchiveP node) {
  this->Base::archiveBase(node);
  node->archiveWeakPointer("weakObject", this->_WeakObject);
}
#endif // defined(XML_ARCHIVE)

CL_LISPIFY_NAME("weakPointerValid");
CL_DEFMETHOD bool WeakPointer_O::valid() const {
  return this->_WeakObject.valid();
}

/*! Return (values value t) or (values nil nil) */
CL_LISPIFY_NAME("weakPointerValue");
CL_DEFMETHOD T_mv WeakPointer_O::value() const {
  return this->_WeakObject.value();
}

}; /* core */
