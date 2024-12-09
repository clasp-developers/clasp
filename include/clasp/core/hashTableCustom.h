#pragma once
/*
    File: hashTableCustom.h
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
#include <clasp/core/hashTable.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/corePackage.fwd.h>

namespace core {

FORWARD(HashTableCustom);
class HashTableCustom_O : public HashTable_O {
  LISP_CLASS(core, CorePkg, HashTableCustom_O, "HashTableCustom", HashTable_O);
  DEFAULT_CTOR_DTOR(HashTableCustom_O);

public:
  static HashTableCustom_sp create(uint sz, Number_sp rehashSize, double rehashThreshold, Function_sp comparator,
                                   Function_sp hasher);

public:
  Function_sp comparator;
  Function_sp hasher;

public: // Functions here
  virtual T_sp hashTableTest() const { return comparator; };

  bool keyTest(T_sp entryKey, T_sp searchKey) const;

  void sxhashEffect(T_sp key, HashGenerator& hg) const;
};

}; // namespace core
template <> struct gctools::GCInfo<core::HashTableCustom_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};
