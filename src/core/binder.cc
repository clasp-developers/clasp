/*
    File: binder.cc
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
#define DEBUG_LEVEL_FULL

#include <string.h>
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/serialize.h>
#include <clasp/core/binder.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/standardObject.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/activationFrame.h>
#include <clasp/core/evaluator.h>
//#i n c l u d e "render.h"
#include <clasp/core/hashTableEq.h>
#include <clasp/core/wrappers.h>

#define MAX_CONS_CHARS 1024

namespace core {






void Binder_O::initialize() {
  this->_Bindings = HashTableEq_O::create_default();
  this->_Values = VectorObjects_O::create();
}

void Binder_O::archiveBase(ArchiveP node) {
  node->attribute("bindings", this->_Bindings);
  node->attribute("values", this->_Values);
}

#if 0
    Render_sp Binder_O::rendered(Cons_sp kargs)
    {
	DisplayT_sp dl = _lisp->create<DisplayList_O>();
	Binder_O::iterator	oi;
	for ( oi=this->_Bindings.begin(); oi!=this->_Bindings.end(); oi++ )
	{
	    if ( this->_Bindings.indexed_value(oi->second)->canRender() )
	    {
		Render_sp part = this->_Bindings.indexed_value(oi->second)->rendered(kargs);
		part->setName(oi->first);
		dl->append(part);
	    }
	}
	return((dl));
    }
#endif

void Binder_O::erase() {
  this->_Bindings->clrhash();
  VectorObjects_sp vo = VectorObjects_O::create();
  this->_Values->swap(vo);
}

#if 0
/*! Currently, if the symbol is unbound then it will be created
 * in the local namespace just like updateOrDefine.
 * In the future we should remove the ability to create variables
 * and leave that to updateOrDefine
 */
    void Binder_O::update(Symbol_sp sym, T_sp val)
    {
	Binder_O::iterator it = this->_Bindings.find(sym);
	if ( it==this->_Bindings.end() )
	{
	    SIMPLE_ERROR(BF("In update, could not find variable binding for symbol: "+sym->fullName()));
	}
	this->_Bindings.update_indexed_value(it->second,val);
    }
#endif

T_sp Binder_O::extend(Symbol_sp sym, T_sp val) {
  this->_Bindings->setf_gethash(sym, val);
  return (val);
}

T_sp Binder_O::lookup(Symbol_sp sym) const {
  LOG(BF("Looking for symbol(%s)") % _rep_(sym));
  T_sp val = this->_Bindings->gethash(sym, _Unbound<T_O>());
  if (val.unboundp()) {
    SIMPLE_ERROR(BF("Could not find variable binding for %s") % _rep_(sym));
  }
  return ((this->_Values->operator[](unbox_fixnum(gc::As<Fixnum_sp>(val)))));
}

bool Binder_O::contains(Symbol_sp sym) const {
  TESTING();
  return this->_Bindings->gethash(sym, _Nil<T_O>()).notnilp();
}

bool Binder_O::containsSymbolFromString(const string &str) {
  Symbol_sp sym = _lisp->findSymbol(str);
  if (sym.nilp())
    return ((false));
  return ((this->contains(sym)));
}

#if 0
    T_sp Binder_O::value(const string& str)
    {
	Symbol_sp sym = _lisp->findSymbol(str);
	return((this->value(sym)));
    }

#endif

int Binder_O::intValueOrDefault(Symbol_sp sym, int defVal) {
  if (!this->contains(sym)) {
    return ((defVal));
  }
  return (unbox_fixnum(gc::As<Fixnum_sp>(this->lookup(sym))));
}

#if 0
    string Binder_O::allKeysAsString() const
    {
	stringstream ss;
	uint lineLen = 0;
	for ( Binder_O::const_iterator it=this->_Bindings.begin(); it!=this->_Bindings.end(); it++ )
	{
	    string one = it->first->fullName();
	    ss << one << " ";
	    lineLen += one.size()+1;
	    if ( lineLen > 60 )
	    {
		ss << std::endl;
		lineLen = 0;
	    }
	}
	return((ss.str()));
    }

    StringSet_sp Binder_O::allKeysAsStringSet() const
    {
	StringSet_sp result = StringSet_O::create();
	for ( Binder_O::const_iterator it=this->_Bindings.begin(); it!=this->_Bindings.end(); it++ )
	{
	    result->insert(it->first->currentName());
	}
	return((result));
    }


    Cons_sp Binder_O::allKeysAsCons() const
    {
	Cons_sp first = Cons_O::create();
	Cons_sp cur = first;
	for ( Binder_O::const_iterator it=this->_Bindings.begin(); it!=this->_Bindings.end(); it++ )
	{
	    Cons_sp one = Cons_O::create(it->first,_Nil<T_O>());
	    cur->setCdr(one);
	    cur = one;
	}
	return((cCdr(first)));
    }



    Cons_sp Binder_O::allValuesAsCons() const
    {
	Cons_sp first = Cons_O::create();
	Cons_sp cur = first;
	for ( Binder_O::const_iterator it=this->_Bindings.begin(); it!=this->_Bindings.end(); it++ )
	{
	    Cons_sp one = Cons_O::create(this->_Bindings.indexed_value(it->second),_Nil<T_O>());
	    cur->setCdr(one);
	    cur = one;
	}
	return((cCdr(first)));
    }



    Cons_sp Binder_O::allKeysAsConsOfKeywordSymbols() const
    {
	IMPLEMENT_ME();
    }

#endif

}; // namespace core
