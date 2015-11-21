/*
    File: hashTableEql.h
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
#ifndef _core_HashTableEql_H
#define _core_HashTableEql_H

#include <clasp/core/object.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/corePackage.fwd.h>

namespace core {

FORWARD(HashTableEql);
class HashTableEql_O : public HashTable_O {
  LISP_BASE1(HashTable_O);
  LISP_CLASS(core, CorePkg, HashTableEql_O, "HashTableEql");
#if defined(XML_ARCHIVE)
  DECLARE_ARCHIVE();
#endif // defined(XML_ARCHIVE)
  DEFAULT_CTOR_DTOR(HashTableEql_O);

private: // instance variables here
public:
  static HashTableEql_sp create(uint sz, Number_sp rehashSize, double rehashThreshold);
  static HashTableEql_sp create_default();

public:
  //	static int sxhash_eql(T_sp obj);
public: // Functions here
  virtual T_sp hashTableTest() const { return cl::_sym_eql; };

  bool keyTest(T_sp entryKey, T_sp searchKey) const;

  gc::Fixnum sxhashKey(T_sp key, gc::Fixnum bound, bool willAddKey) const;
};

}; /* core */
template <>
struct gctools::GCInfo<core::HashTableEql_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

TRANSLATE(core::HashTableEql_O);

#endif /* _core_HashTableEql_H */
