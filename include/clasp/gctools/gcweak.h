#pragma once

/*
    File: gcweak.h
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

/* Derived from scheme-advanced.c by ravenbrook */

/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <functional>
#include <optional>

// Caution: This file is included by obj_scan.cc which is pretty low level.
// Don't put complicated includes in here.

namespace gctools {

// This structure is meant to be included directly (not as a pointer) in
// a weak pointer object, e.g. WeakPointer_O. In order to ensure the pointer
// is not scanned, this should be allocated with the "atomic" GC policy.
struct WeakPointer {
public:
  WeakPointer(core::T_sp o);
  std::optional<core::T_sp> value() const;
  std::optional<core::T_sp> value_no_lock() const; // used by scanner
  void store_no_lock(core::T_sp); // ditto.
  void fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup*);
public: // has to be public for precise GC reasons even though it's not scanned?
  // This is a Tagged rather than a T_sp because something in gc_boot seems to
  // check for T_sps in atomic (pointerless) objects. Rather than lie harder we
  // can just do this and build a T_sp from it as required.
  Tagged _value;
  // flag needed to disambiguate fixnum 0 from splatted pointer
  // not sure if other GCs need this, but i'd like to keep the structure
  // consistent regardless of build parameters if possible.
  bool _splattablep = false;
#ifdef USE_BOEHM
private:
  // private stuff needed to get everything done within a callback
  struct value_helper_s {
    value_helper_s(const WeakPointer* w) : wp(w), result() {}
    const WeakPointer* wp;
    std::optional<core::T_sp> result;
  };
  static void* value_helper(void*);
#endif // lacking real support, we have not-actually-weak pointers.
};

// Used below in hash maps and ephemerons.
struct KVPair {
  core::T_sp key;
  core::T_sp value;
};

// On Boehm this is not a real ephemeron - it's a weak pointer to the key,
// and a strong pointer to the value that happens to get wiped with the key.
// To see the difference, imagine having two inverse ephemerons {V1, V2} and
// {V2, V1}, where V1 and V2 are some otherwise inaccessible objects. With real
// ephemerons, the ephemeron values (V2 and V1) will not be scanned unless the
// keys (V1 and V2) are otherwise inaccessible, which they are not, and so both
// ephemerons can be wiped by the GC. With these boehm "ephemerons" V1 and V2
// will be kept alive by the strong pointers and so both will be alive forever.
// Another issue comes up when an ephemeron's value contains the only strong
// references to the ephemeron's key; in a real ephemeron this will not keep the
// ephemeron alive, but it will in these.

// TL;DR: The Boehm interface does not seem to allow real ephemerons.
// These pseudo ephemerons can at least handle some basic cases of weak hash
// tables without entailing too bad of a memory leak.
struct Ephemeron {
public:
  Ephemeron(core::T_sp key, core::T_sp value);
  // If the ephemeron is valid, return its key and value.
  // Otherwise return (deleted, deleted).
  KVPair get() const;
  std::optional<core::T_sp> key() const {
    auto p = get();
    if (p.key.deletedp()) return std::nullopt;
    else return p.key;
  }
  std::optional<core::T_sp> value() const {
    auto p = get();
    if (p.key.deletedp()) return std::nullopt;
    else return p.value;
  }
  // Caller must ensure that the key is otherwise live,
  // or else the value could remain while the key dies (memory leak)
  void setValue(core::T_sp v) { _value = v; }
  void reinit(core::T_sp k, core::T_sp v);
public:
#ifdef USE_BOEHM
  GC_hidden_pointer _key;
#else // FIXME for other GCs!
  core::T_sp _key;
#endif
  core::T_sp _value;
#ifdef USE_BOEHM
private:
  struct result_helper_s {
    result_helper_s(const Ephemeron* e) : eph(e) {}
    const Ephemeron* eph;
    KVPair result;
  };
  static void* get_helper(void*);
#endif
};

// These Mapping objects are used in hash tables.

// A strong mapping isn't weak at all, obviously, but
// this is the degenerate case.
struct StrongMapping {
public:
  typedef GCArray_moveable<KVPair> vector_type;
  // for e.g. sizeof_container
  typedef typename vector_type::value_type value_type;
private:
  static const KVPair initKV;
public:
  StrongMapping(size_t size) : _Data(size, initKV) {}
public:
  vector_type _Data;
public:
  size_t size() const { return _Data.length(); }
  KVPair get(size_t i) const { return _Data[i]; }
  void setValue(size_t i, core::T_sp v) { _Data[i].value = v; }
  void newEntry(size_t i, core::T_sp k, core::T_sp v) {
    _Data[i].key = k;
    _Data[i].value = v;
  }
  void remove(size_t i) {
    _Data[i].key = core::T_sp(tag_deleted<Tagged>());
    _Data[i].value = core::T_sp(tag_deleted<Tagged>());
  }
};

struct EphemeronMapping {
public:
  typedef GCArray_moveable<Ephemeron> vector_type;
  typedef typename vector_type::value_type value_type;
private:
  static const Ephemeron initEph;
public:
  EphemeronMapping(size_t size) : _Data(size, initEph) {}
public:
  vector_type _Data;
public:
  size_t size() const { return _Data.length(); }
  KVPair get(size_t i) const { return _Data[i].get(); }
  void setValue(size_t i, core::T_sp v) { _Data[i].setValue(v); }
  void newEntry(size_t i, core::T_sp k, core::T_sp v) { _Data[i].reinit(k, v); }
  void remove(size_t i) { _Data[i].reinit(deleted<core::T_O>(), deleted<core::T_O>()); }
};

}; // namespace gctools
