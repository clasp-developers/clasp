/*
    File: gcweak.cc
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
#include <clasp/core/foundation.h>
#include <clasp/gctools/memoryManagement.h>
#include <clasp/gctools/gcweak.h>
#include <clasp/core/object.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/mpPackage.h>

namespace gctools {

#ifdef USE_BOEHM
WeakPointer::WeakPointer(core::T_sp o) : _value(o) {
  if (o.objectp()) { // pointer, so we're actually weak
    _splattablep = true;
    // note: deregistered automatically if the weak pointer itself is dealloc'd
    GC_general_register_disappearing_link((void**)&_value, &*o);
  }
}

void* WeakPointer::value_helper(void* data) {
  value_helper_s* vhsp = (value_helper_s*)data;
  if (vhsp->wp->_value || !vhsp->wp->_splattablep) // not splatted
    // put a T_sp in the result
    vhsp->result = vhsp->wp->_value;
  // otherwise, leave the result default constructed (no T_sp)
  return nullptr; // unused
}

std::optional<core::T_sp> WeakPointer::value() const {
  value_helper_s vhs(this);
  // TODO: Use GC_call_with_reader_lock, but it's too new
  GC_call_with_alloc_lock(value_helper, &vhs);
  return vhs.result;
}
std::optional<core::T_sp> WeakPointer::value_no_lock() const {
  if (_value || !_splattablep)
    return _value;
  else return std::nullopt;
}
void WeakPointer::store_no_lock(core::T_sp o) {
  _value = o;
  _splattablep = o.objectp();
  // links set up in fixupInternals below.
}
void WeakPointer::store(core::T_sp o) {
  if (o.objectp()) { // pointer, so we're actually weak
    _splattablep = true;
    // note: deregistered automatically if the weak pointer itself is dealloc'd
    GC_general_register_disappearing_link((void**)&_value, &*o);
  } else {
    _splattablep = false;
    GC_unregister_disappearing_link((void**)&_value);
  }
  _value = o;
}  

void WeakPointer::fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup) {
  if (snapshotSaveLoad::operation(fixup) == snapshotSaveLoad::LoadOp) {
    // We do this later rather than in store_no_lock because register/deregister
    // unconditionally grab a lock. This is a bit of a KLUDGE.
    if (_value.objectp()) {
      _splattablep = true;
      // Implicitly removes any previous registration, per boehm docs.
      GC_general_register_disappearing_link((void**)&_value, &*_value);
    } else {
      _splattablep = false;
      GC_unregister_disappearing_link((void**)&_value);
    }
  }
}
#else // not-actually-weak pointers - TODO for your other GC!
WeakPointer::WeakPointer(core::T_sp o) : _value(o) {}

// always valid
std::optional<core::T_sp> WeakPointer::value() const { return _value; }
std::optional<core::T_sp> WeakPointer::value_no_lock() const { return value(); }
void WeakPointer::store_no_lock(core::T_sp o) { _value = o; }
void WeakPointer::store(core::T_sp o) { store_no_lock(o); }
void WeakPointer::fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup*) {}
#endif

#ifdef USE_BOEHM
Ephemeron::Ephemeron(core::T_sp k, core::T_sp v)
  : _key(GC_HIDE_POINTER(k.tagged_())), _value(v) {
  GCTOOLS_ASSERT(_key); // basically asserts that ~0 is never passed in,
  // since if it was there'd be no way to tell if it's splatted
  if (k.objectp()) {
    GC_general_register_disappearing_link((void**)&_key, &*k);
    GC_general_register_disappearing_link((void**)&_value, &*k);
  }
}

void Ephemeron::reinit(core::T_sp k, core::T_sp v) {
  if (k.objectp()) {
    // These will implicitly undo any previously registered
    // disappearing link, according to Boehm docs.
    // We undo the old registrations first so that it is not possible
    // for the new _key to get wiped by the old key being collected,
    // in the brief window before the _value is also put in.
    // But FIXME: Do we need to put in a fence or something to ensure
    // the key stays alive within reinit?
    GC_general_register_disappearing_link((void**)&_key, &*k);
    GC_general_register_disappearing_link((void**)&_value, &*k);
  } else {
    GC_unregister_disappearing_link((void**)&_key);
    GC_unregister_disappearing_link((void**)&_value);
  }
  _value = v;
  _key = GC_HIDE_POINTER(k.tagged_());
}

void* Ephemeron::get_helper(void* data) {
  result_helper_s* rhsp = (result_helper_s*)data;
  if (rhsp->eph->_key) { // not splatted
    rhsp->result.key = core::T_sp((Tagged)GC_REVEAL_POINTER(rhsp->eph->_key));
    rhsp->result.value = rhsp->eph->_value;
  } else {
    rhsp->result.key = rhsp->result.value = deleted<core::T_O>();
  }
  return nullptr;
}

KVPair Ephemeron::get() const {
  result_helper_s rhs(this);
  // same TODO with GC_call_with_reader_lock.
  GC_call_with_alloc_lock(get_helper, &rhs);
  return rhs.result;
}

KVPair Ephemeron::get_no_lock() const {
  return KVPair(core::T_sp((Tagged)GC_REVEAL_POINTER(_key)), _value);
}
void Ephemeron::reinit_no_lock(core::T_sp k, core::T_sp v) {
  _key = GC_HIDE_POINTER(k.tagged_());
  _value = v;
}
void Ephemeron::fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup) {
  if (snapshotSaveLoad::operation(fixup) == snapshotSaveLoad::LoadOp) {
    core::T_sp k((Tagged)GC_REVEAL_POINTER(_key));
    if (k.objectp()) {
      GC_general_register_disappearing_link((void**)&_key, &*k);
      GC_general_register_disappearing_link((void**)&_value, &*k);
    } else {
      GC_unregister_disappearing_link((void**)&_key);
      GC_unregister_disappearing_link((void**)&_value);
    }
  }
}
#else // not-actually-weak ephemeron default - FIXME for your GC!
Ephemeron::Ephemeron(core::T_sp key, core::T_sp value) : _key(key), _value(value) {}

void Ephemeron::reinit(core::T_sp k, core::T_sp v) {
  _key = k; _value = v;
}
void Ephemeron::reinit_no_lock(core::T_sp k, core::T_sp v) { reinit(k, v); }

KVPair Ephemeron::get() const { return KVPair(_key, _value); }
KVPair Ephemeron::get_no_lock() const { return get(); }
void Ephemeron::fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup*) {}
#endif

// Clang says this definition has to be out-of-line. Sure whatever.
const KVPair StrongMapping::initKV = {.key = core::T_sp(tag_no_key<Tagged>()),
    .value = core::T_sp(tag_no_key<Tagged>())};

// note that Ephemeron's constructor doesn't have to do anything
// interesting for non-objects, so we shouldn't need to worry about
// static constructor ordering.
const Ephemeron EphemeronMapping::initEph{no_key<core::T_O>(), no_key<core::T_O>()};

const WeakAnd WeakAndMapping::initKV{no_key<core::T_O>(), no_key<core::T_O>()};
const DoubleEphemeron DoubleEphMapping::initEph{no_key<core::T_O>(), no_key<core::T_O>()};

} // namespace gctools
