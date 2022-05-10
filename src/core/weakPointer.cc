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
DOCGROUP(clasp)
CL_DEFUN WeakPointer_sp WeakPointer_O::make(T_sp obj) {
  if (obj.objectp()) {
    auto  me = gctools::GC<WeakPointer_O>::allocate( obj);
    return me;
  }
  SIMPLE_ERROR(("You cannot make a weak pointer to an immediate"));
};


CL_LISPIFY_NAME("weakPointerValid");
CL_DEFMETHOD bool WeakPointer_O::valid() const {
#if defined(USE_BOEHM)
  return this->_Link!=NULL;
#else
  SIMPLE_ERROR(("WeakPointer_O not supported in this GC"));
#endif
}

/*! Return (values value t) or (values nil nil) */
CL_LISPIFY_NAME("weakPointerValue");
CL_DEFMETHOD T_sp WeakPointer_O::value() const {
#if defined(USE_BOEHM)
  if (this->_Link!=NULL) {
    T_sp obj((gctools::Tagged)this->_Object);
    return obj;
  }
  return nil<core::T_O>();
#else
  SIMPLE_ERROR(("WeakPointer_O not supported by this GC"));
#endif
}

}; /* core */
