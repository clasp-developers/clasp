/*
    File: sysprop.cc
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
#include <clasp/core/hashTableEql.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/symbol.h>
#include <clasp/core/sysprop.h>
#include <clasp/core/wrappers.h>

namespace core {

CL_LAMBDA(key area value);
CL_DECLARE();
CL_DOCSTRING("put_sysprop - returns value");
CL_DEFUN T_sp core__put_sysprop(T_sp key, T_sp area, T_sp value) {
  ASSERT(_lisp->_Roots._SystemProperties);
  if (_lisp->_Roots._SystemProperties.nilp()) {
    _lisp->_Roots._SystemProperties = HashTableEql_O::create_default();
  }
  bool foundHashTable = false;
  const T_mv &values = gc::As<HashTable_sp>(_lisp->_Roots._SystemProperties)->gethash(area);
  T_sp area_hash_table = values;
  foundHashTable = gc::As<T_sp>(values.valueGet_(1)).isTrue();
  T_sp retval;
  if (foundHashTable) {
    retval = gc::As<HashTable_sp>(area_hash_table)->hash_table_setf_gethash(key, value);
  } else {
    HashTable_sp new_hash_table = HashTableEql_O::create_default();
    new_hash_table->hash_table_setf_gethash(key, value);
    retval = gc::As<HashTable_sp>(_lisp->_Roots._SystemProperties)->hash_table_setf_gethash(area, new_hash_table);
  }
  return (retval);
}

CL_LAMBDA(key area);
CL_DECLARE();
CL_DOCSTRING("get_sysprop - returns (values val foundp)");
CL_DEFUN T_mv core__get_sysprop(T_sp key, T_sp area) {
  if (_lisp->_Roots._SystemProperties.notnilp()) {
    T_mv values = gc::As<HashTable_sp>(_lisp->_Roots._SystemProperties)->gethash(area, _Nil<T_O>());
    T_sp hashTable = values;
    bool foundHashTable = gc::As<T_sp>(values.valueGet_(1)).isTrue();
    if (foundHashTable) {
      return gc::As<HashTable_sp>(hashTable)->gethash(key, _Nil<T_O>());
    }
  }
  return (Values(_Nil<T_O>(), _Nil<T_O>()));
}

CL_LAMBDA(key area);
CL_DECLARE();
CL_DOCSTRING("rem_sysprop");
CL_DEFUN T_sp core__rem_sysprop(T_sp key, T_sp area) {
  T_mv mv_values = gc::As<HashTable_sp>(_lisp->_Roots._SystemProperties)->gethash(area, _Nil<T_O>());
  HashTable_sp hashTable = gc::As<HashTable_sp>(mv_values);
  bool foundHashTable = gc::As<T_sp>(mv_values.valueGet_(1)).isTrue();
  if (foundHashTable) {
    bool found = hashTable->remhash(key);
    return _lisp->_boolean(found);
  }
  return _Nil<T_O>();
}

  SYMBOL_SC_(CorePkg, put_sysprop);

  SYMBOL_SC_(CorePkg, get_sysprop);

  SYMBOL_SC_(CorePkg, rem_sysprop);

};
