/*
    File: holder.h
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

#ifndef Holder_H //(
#define Holder_H

#include <vector>
#include <map>
#include <set>
#include <unordered_set>
#include <stack>
//#include "conditions.h"
#include <clasp/core/cons.h>

namespace core {

template <class OType /*, class GCStamp=gctools::GCHolder */>
class DepreciatedSymbolDict :
#if defined(USE_MPS) || defined(USE_REFCOUNT)
    public std::map<Symbol_sp, gctools::smart_ptr<OType>>
#endif
#ifdef USE_BOEHM
    public std::map<Symbol_sp, gctools::smart_ptr<OType>, std::less<Symbol_sp>, gc_allocator<Symbol_sp>>
#endif

    {
public:
  typedef typename gctools::smart_ptr<OType> value_type;
  typedef typename map<Symbol_sp, value_type>::iterator iterator;
  typedef typename map<Symbol_sp, value_type>::const_iterator const_iterator;

public:
#if 0
	StringSet_sp getKeysAsStringSet(Lisp_sp e) const
	{
	    typename SymbolDict::const_iterator ie;
	    StringSet_sp ss = StringSet_O::create(e);
	    for ( ie=this->begin(); ie!=this->end(); ie++ )
	    {
	        ss->insert(ie->first->__repr__());
	    }
	    return ss;
	}
#endif
  // getKeysAsSymbolSet --> StringSet_O::create_getKeysAsSymbolSet

  bool contains(Symbol_sp c) const { return this->count(c) > 0; };
  void remove(Symbol_sp c) { this->erase(c); };
  void addUnique(Symbol_sp s, value_type obj) {
    _OF();
    ASSERTP(!this->contains(s), "The SymbolDict already contains key(" + symbol_fullName(s) + ")");
    this->operator[](s) = obj;
  };
  void set(Symbol_sp s, value_type obj) {
    this->operator[](s) = obj;
  };
  value_type get(Symbol_sp name, const Lisp_sp &lisp) const {
    _G();
    typename DepreciatedSymbolDict::const_iterator found = this->find(name);
    if (found == this->end()) {
      KEY_NOT_FOUND_ERROR(name);
    };
    return found->second;
  };

  value_type getDefaultNil(Symbol_sp name, const Lisp_sp &lisp) {
    if (this->count(name) == 0) {
      return _Nil<OType>();
    };
    return (*this)[name];
  };
};
};

#endif //)
