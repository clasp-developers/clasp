/*
    File: vectorObjectsWithFillPtr.h
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
#ifndef _core_VectorObjectsWithFillPtr_H
#define _core_VectorObjectsWithFillPtr_H
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/vectorObjects.h>
#include <clasp/core/corePackage.fwd.h>

namespace core {
FORWARD(VectorObjectsWithFillPtr);
class VectorObjectsWithFillPtr_O : public VectorObjects_O {
  LISP_BASE1(VectorObjects_O);
  LISP_CLASS(core, CorePkg, VectorObjectsWithFillPtr_O, "VectorObjectsWithFillPtr");
  void archiveBase(SNode_sp node);

public:
  VectorObjectsWithFillPtr_O();
  virtual ~VectorObjectsWithFillPtr_O(){};

private: // instance variables here
  cl_index _FillPtr;

public:
  static VectorObjectsWithFillPtr_sp make(T_sp initial_element, T_sp initial_values, int dimension, cl_index fillPtr, bool adjustable, T_sp elementType);

public: // Functions here
  gc::Fixnum length() const { return this->_FillPtr; };

  virtual bool arrayHasFillPointerP() const { return true; };
  virtual T_sp &operator[](uint index);

  virtual T_sp elt(int index) const;
  virtual T_sp setf_elt(int index, T_sp value);

  string __repr__() const;

  cl_index fillPointer() const { return this->_FillPtr; };
  void setf_fillPointer(cl_index fp);
  void unsafe_setf_fill_pointer(Fixnum fp) { this->_FillPtr = fp; };
  T_sp vectorPush(T_sp newElement);
  Fixnum_sp vectorPushExtend(T_sp newElement, int extension = 16);
};

}; /* core */

TRANSLATE(core::VectorObjectsWithFillPtr_O);

template <>
struct gctools::GCInfo<core::VectorObjectsWithFillPtr_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

#endif /* _core_VectorObjectsWithFillPtr_H */
