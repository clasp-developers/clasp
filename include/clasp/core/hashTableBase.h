/*
    File: hashTable.h
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
#ifndef _core_HashTableBase_H
#define _core_HashTableBase_H

#include <clasp/core/object.h>

namespace core {
  FORWARD(HashTableBase);
  class HashTableBase_O : public General_O {
    struct metadata_bootstrap_class {};
    LISP_CLASS(core, ClPkg, HashTableBase_O, "HashTableBase",core::General_O);
  HashTableBase_O() {};
  public:
    virtual T_sp hash_table_setf_gethash(T_sp key, T_sp value) = 0;
    virtual T_mv gethash(T_sp key, T_sp default_value) = 0;
    virtual bool remhash(T_sp key) = 0;
    virtual Number_sp rehash_size() = 0;
    virtual double rehash_threshold() = 0;
    virtual T_sp hash_table_test() = 0;
    virtual T_mv maphash(T_sp function_desig) = 0;
    virtual T_sp clrhash() = 0;
    virtual size_t hashTableCount() const = 0 ;
  };

};

#endif
