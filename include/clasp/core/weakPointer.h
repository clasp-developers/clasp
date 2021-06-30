/*
    File: weakPointer.h
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
#ifndef _core_WeakPointer_H
#define _core_WeakPointer_H

#include <clasp/core/object.h>
#include <clasp/core/corePackage.fwd.h>

template <>
struct gctools::GCInfo<core::WeakPointer_O> {
  static bool const NeedsInitialization = false;
  static bool const NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = atomic;
};

namespace core {
FORWARD(WeakPointer);
class WeakPointer_O : public General_O {
  LISP_CLASS(core, CorePkg, WeakPointer_O, "WeakPointer",General_O);
#if defined(OLD_SERIALIZE)
  DECLARE_SERIALIZE();
#endif // defined(OLD_SERIALIZE)
#if defined(XML_ARCHIVE)
  DECLARE_ARCHIVE();
#endif // defined(XML_ARCHIVE)
  WeakPointer_O() : _Link(NULL), _Object(NULL) {};
  WeakPointer_O(T_sp ptr) : _Link(ptr.raw_()), _Object(ptr.raw_()) {
#ifdef USE_BOEHM
    GC_general_register_disappearing_link((void**)&this->_Link, &*ptr);
#else
    SIMPLE_ERROR(BF("WeakPointer_O not supported"));
#endif
  };
  ~WeakPointer_O() {
#ifdef USE_BOEHM
    GC_unregister_disappearing_link((void**)&this->_Link);
#else
    SIMPLE_ERROR(BF("WeakPointer_O not supported"));
#endif
  }
  

public:
  static WeakPointer_sp make(T_sp obj);
public:
  void*    _Link; // Use a boehm disappearing link
  void*    _Object;
public: // Functions here
  /*! Value of the reference to the object. If the object was destroyed then return nil. */
  T_sp value() const;

  /*! Return true if the object referenced by this still exists, otherwise return false */
  bool valid() const;
};

}; /* core */


#endif /* _core_WeakPointer_H */
