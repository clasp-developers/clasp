/*
    File: hashTableEql.cc
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
#include <clasp/core/symbolTable.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//

HashTableEql_sp HashTableEql_O::create(Mapping_sp mapping, Number_sp rehashSize, double rehashThreshold) {
  return gctools::GC<HashTableEql_O>::allocate(mapping, rehashSize, rehashThreshold);
}

HashTableEql_sp HashTableEql_O::create(uint sz, Number_sp rehashSize, double rehashThreshold) {
  return create(StrongMapping_O::make(sz), rehashSize, rehashThreshold);
}

SYMBOL_EXPORT_SC_(ClPkg, eql);
HashTableEql_sp HashTableEql_O::create_default() {
  DoubleFloat_sp rhs = DoubleFloat_O::create(2.0);
  HashTableEql_sp ht = create(16, rhs, DEFAULT_REHASH_THRESHOLD);
  return ht;
}

bool HashTableEql_O::keyTest(T_sp entryKey, T_sp searchKey) const { return cl__eql(entryKey, searchKey); }

void HashTableEql_O::sxhashEffect(T_sp obj, HashGenerator& hg) const {
  if (obj.fixnump()) hg.addValue0(obj.unsafe_fixnum());
  else if (obj.single_floatp()) hg.addValue0(float_convert<float>::float_to_bits(obj.unsafe_single_float()));
  else if (obj.characterp()) hg.addValue0(obj.unsafe_character());
  else if (obj.generalp()) {
    if (cl__numberp(obj)) hg.hashObject(obj);
    else hg.addGeneralAddress(obj.as_unsafe<General_O>());
  } else if (obj.consp()) hg.addConsAddress(obj.as_unsafe<Cons_O>());
  else SIMPLE_ERROR("Illegal object (object.raw_() = {}) for eql hash {}", (void*)obj.raw_(), _rep_(obj));
}

}; // namespace core
