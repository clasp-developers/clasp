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
// #define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/weakPointer.h>
#include <clasp/core/referenceQueue.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//

WeakPointer_sp WeakPointer_O::make(T_sp obj) {
  return gctools::GC<WeakPointer_O>::allocate(obj);
}

CL_LISPIFY_NAME(make-weak-pointer);
CL_LAMBDA(referent &optional queue);
DOCGROUP(clasp);
CL_DEFUN T_sp core__make_weak_pointer(T_sp referent, T_sp queue) {
  if (queue.nilp()) return WeakPointer_O::make(referent);
  if (queue.isA<ReferenceQueue_O>())
    return QueueableWeakReference_O::make(referent,
                                          queue.as_unsafe<ReferenceQueue_O>(),
                                          false);
  else TYPE_ERROR(queue, Cons_O::createList(cl::_sym_or, cl::_sym_null,
                                            core::_sym_referenceQueue));
};

CL_LISPIFY_NAME("weakPointerValid");
CL_DEFUN bool core__weak_pointer_valid(T_sp reference) {
  if (reference.isA<WeakPointer_O>())
    return reference.as_unsafe<WeakPointer_O>()->valid();
  else if (reference.isA<QueueableWeakReference_O>())
    return reference.as_unsafe<QueueableWeakReference_O>()->valid();
  else TYPE_ERROR(reference,
                  Cons_O::createList(cl::_sym_or, core::_sym_weakPointer,
                                     core::_sym_queueableWeakReference));
}

/*! Return the value if it's live, or NIL if it's dead. */
CL_LISPIFY_NAME("weakPointerValue");
CL_DEFUN T_sp core__weak_pointer_value(T_sp reference) {
  if (reference.isA<WeakPointer_O>())
    return reference.as_unsafe<WeakPointer_O>()->value();
  else if (reference.isA<QueueableWeakReference_O>())
    return reference.as_unsafe<QueueableWeakReference_O>()->value();
  else TYPE_ERROR(reference,
                  Cons_O::createList(cl::_sym_or, core::_sym_weakPointer,
                                     core::_sym_queueableWeakReference));
}

CL_LISPIFY_NAME(make-ephemeron);
CL_DEFUN Ephemeron_sp Ephemeron_O::make(T_sp key, T_sp value) {
  return gctools::GC<Ephemeron_O>::allocate(key, value);
}

CL_LISPIFY_NAME("ephemeron/key");
CL_DEFMETHOD T_sp Ephemeron_O::key() const {
  auto r = _ephemeron.key();
  if (r) return *r;
  else return nil<T_O>();
}
CL_LISPIFY_NAME("ephemeron/value");
CL_DEFMETHOD T_sp Ephemeron_O::value() const {
  auto r = _ephemeron.value();
  if (r) return *r;
  else return nil<T_O>();
}

CL_LISPIFY_NAME("ephemeron/validp");
CL_DEFMETHOD bool Ephemeron_O::valid() const {
  return _ephemeron.key().has_value();
}

}; // namespace core
