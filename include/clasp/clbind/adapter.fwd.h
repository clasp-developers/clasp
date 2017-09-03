/*
    File: adapter.fwd.h
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
#ifndef clbind_adapter_fwd_H
#define clbind_adapter_fwd_H

namespace clbind {

inline void throwSlotsNotSupported() {
  SIMPLE_ERROR_SPRINTF("Slots are not supported by this class");
}

/*! Add the enable_derivable type to Adapter or Derivable
      to allow Common Lisp to derive other classes from it.
    eg: See derivable.h 
    class Derivable : public ... {
    public:
        typedef int enable_derivable;
    };
    */
template <class T>
bool isDerivableCxxClass(typename T::enable_derivable adapter) {
  return true;
}

template <class T>
bool isDerivableCxxClass(...) {
  return false;
}

template <class T>
void support_initializeSlots(int slots, typename T::enable_slots adapter) {
  adapter->initializeSlots(slots);
}

template <class T>
void support_initializeSlots(int slots, ...) {
  throwSlotsNotSupported();
}

template <class T>
void *support_adapterAddress(typename T::enable_slots adapter) {
  return adapter->address();
}

template <class T>
void *support_adapterAddress(...) {
  return NULL;
}

template <class T>
core::T_sp support_instanceSigSet(typename T::enable_slots adapter) {
  return adapter->instanceSigSet();
}

template <class T>
core::T_sp support_instanceSigSet(...) {
  throwSlotsNotSupported();
  return _Nil<core::T_O>();
}

template <class T>
core::T_sp support_instanceSig(typename T::enable_slots adapter) {
  return adapter->instanceSig();
}

template <class T>
core::T_sp support_instanceSig(...) {
  throwSlotsNotSupported();
  return _Nil<core::T_O>();
}

template <class T>
core::T_sp support_instanceRef(size_t idx, typename T::enable_slots adapter) {
  return adapter->instanceRef(idx);
}

template <class T>
core::T_sp support_instanceRef(size_t idx, ...) {
  throwSlotsNotSupported();
  return _Nil<core::T_O>();
}

template <class T>
core::T_sp support_instanceSet(int idx, core::T_sp val, typename T::enable_slots adapter) {
  return adapter->instanceSet(idx, val);
}

template <class T>
core::T_sp support_instanceSet(int idx, core::T_sp val, ...) {
  throwSlotsNotSupported();
  return _Nil<core::T_O>();
}
};
#endif
