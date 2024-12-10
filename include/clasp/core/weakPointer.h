#pragma once
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

#include <clasp/core/object.h>
#include <clasp/core/corePackage.fwd.h>
#include <clasp/gctools/gcweak.h>

template <> struct gctools::GCInfo<core::WeakPointer_O> {
  static bool const NeedsInitialization = false;
  static bool const NeedsFinalization = false;
  // the atomic policy means this object is not scanned by the GC, which is the
  // actual reason the pointer is weak!
  static GCInfo_policy constexpr Policy = atomic;
};

namespace core {
FORWARD(WeakPointer);
class WeakPointer_O : public General_O {
  LISP_CLASS(core, CorePkg, WeakPointer_O, "WeakPointer", General_O);
  WeakPointer_O(T_sp ptr) : _Link(ptr) {}

public:
  static WeakPointer_sp make(T_sp obj);

public:
  gctools::WeakPointer _Link; // Use a boehm disappearing link

public: // Functions here
  /*! Value of the reference to the object. If the object was destroyed then return nil. */
  T_sp value() const;

  /*! Return true if the object referenced by this still exists, otherwise return false */
  bool valid() const;
};

}; // namespace core
