/*
    File: symbolSet.cc
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

//
// (C) 2004 Christian E. Schafmeister
//

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/symbolSet.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/stringList.h>
#include <clasp/core/objectSet.h>
#include <clasp/core/environment.h>
#include <clasp/core/cons.h>
#include <clasp/core/symbolList.h>
#include <clasp/core/numbers.h>
#include <clasp/core/wrappers.h>

namespace core {

extern Symbol_sp _sym_entries;

/*
  __BEGIN_DOC(classes.SymbolSet.!class.SymbolSet)
  \requiredKeyed{entries:}{Cons::listOfStrings}

  Create a SymbolSet containing the strings in \sa{listOfStrings}.
  __END_DOC
*/
SymbolSet_sp SymbolSet_O::make(Cons_sp entries) {
  _G();
  SymbolSet_sp me = SymbolSet_O::create();
  me->insertConsSymbols(entries);
  return me;
}

SymbolSet_sp SymbolSet_O::insertConsSymbols(Cons_sp vals) {
  for (Cons_sp p = vals; p.notnilp(); p = cCdr(p)) {
    Symbol_sp t = oCar(p).as<Symbol_O>();
    this->insert(t);
  }
  return this->sharedThis<SymbolSet_O>();
}

void SymbolSet_O::insertSymbolSet(SymbolSet_sp s) {
  s->map([this](Symbol_sp k) {this->insert(k); });
}

void SymbolSet_O::insertSymbolList(SymbolList_sp slist) {
  slist->map([this](Symbol_sp e) {this->insert(e); });
}

SymbolSet_sp SymbolSet_O::copy() {
  _G();
  GC_COPY(SymbolSet_O, snew, *this);
  snew->clear();
  this->map([&snew](Symbol_sp k) {
                snew->insert(k);
  });
  return snew;
}

string SymbolSet_O::__repr__() const {
  stringstream ss;
  string nm;
  ss.str("");
  ss << "(" << this->_instanceClass()->classNameAsString() << " ";
  this->map([&ss](Symbol_sp s) {
                ss << _rep_(s) << " ";
  });
  ss << " )";
  return ss.str();
}
string SymbolSet_O::asString() {
  stringstream ss;
  string nm;
  ss.str("");
  this->map([&ss](Symbol_sp s) {
                ss << _rep_(s) << " ";
  });
  return ss.str();
}

#if defined(XML_ARCHIVE)
void SymbolSet_O::archiveBase(ArchiveP node) {
  this->Base::archiveBase(node);
  if (node->loading()) {
    this->clear();
    VectorStrings vstrs;
    node->getDataAsVectorOfStrings(vstrs);
    for (VectorStrings::iterator it = vstrs.begin(); it != vstrs.end(); it++) {
      Symbol_sp sym = _lisp->intern(*it);
      this->insert(sym);
    }
  } else {
    node->setCharacters(this->asString());
  }
}
#endif // defined(XML_ARCHIVE)

void SymbolSet_O::insertVectorStrings(const VectorStrings &symbolsAsStrings) {
  for (VectorStrings::const_iterator it = symbolsAsStrings.begin(); it != symbolsAsStrings.end(); it++) {
    Symbol_sp sym = _lisp->intern(*it);
    this->insert(sym);
  }
}

void SymbolSet_O::insertVector(Vector_sp vec) {
  for (int i = 0, iEnd(vec->length()); i < iEnd; ++i) {
    Symbol_sp sym = vec->elt(i).as<Symbol_O>();
    this->insert(sym);
  }
}

bool SymbolSet_O::contains(Symbol_sp s) {
  _G();
  return this->_Symbols->contains(s);
}

bool SymbolSet_O::containsSubset(SymbolSet_sp sub) {
  bool missed(false);
  this->map([&missed, this](Symbol_sp s) {
                if ( !this->contains(s) ) {
                    missed = true;
                    return;
                }
  });
  return !missed;
}

void SymbolSet_O::clear() {
  _OF();
  this->_Symbols->clrhash();
}

/*! Check if the SymbolSet contains the same strings as this
 */
bool SymbolSet_O::equal(T_sp obj) const {
  if (this->eq(obj))
    return true;
  if (obj.isA<SymbolSet_O>()) {
    SymbolSet_sp ss = obj.as<SymbolSet_O>();
    if (this->size() != ss->size())
      return false;
    bool missed(false);
    this->map([&missed, &ss](Symbol_sp s) {
                    if ( !ss->contains(s) ) 
                    {
                        missed = true;
                        return;
                    }
    });
    return !missed;
  }
  return false;
}

void SymbolSet_O::remove(Symbol_sp s) {
  _OF();
#ifdef DEBUG_ON
  if (this->strs.count(s) != 1) {
    SIMPLE_ERROR(BF("The string: %s was not found in SymbolSet") % s);
  }
#endif
  this->_Symbols->remhash(s);
}

void SymbolSet_O::initialize() {
  this->Base::initialize();
  this->_Symbols = HashTableEq_O::create_default();
}

SymbolSet_O::SymbolSet_O(const SymbolSet_O &ss) : T_O(ss) {
  this->_Symbols = ss._Symbols;
}

SymbolSet_sp SymbolSet_O::setUnion(SymbolSet_sp b) {
  _G();
  SymbolSet_sp nset;
  nset = SymbolSet_O::create();
  this->map([&nset](Symbol_sp s) { nset->insert(s); });
  b->map([&nset](Symbol_sp s) { nset->insert(s); });
  return nset;
}

SymbolSet_sp SymbolSet_O::intersection(SymbolSet_sp b) {
  _G();
  SymbolSet_sp nset = SymbolSet_O::create();
  this->map([&b, &nset](Symbol_sp s) {
            LOG(BF("Looking for(%s)") % _rep_((*si)) );
            if ( b->contains(s) )
            {
                LOG(BF("Found it!!!") );
                nset->insert(s);
            } else {
                LOG(BF("Not found") );
            }
  });
  return nset;
}

SymbolSet_sp SymbolSet_O::relativeComplement(SymbolSet_sp b) {
  SymbolSet_sp nset;
  nset = SymbolSet_O::create();
  this->map([&nset, &b](Symbol_sp si) {
                if ( !b->contains(si) )
                {
                    nset->insert(si);
                }
  });
  return nset;
}

/*! Return a new set that takes every element of (this) in combination
  with every element in b separated by a comma
*/
ObjectSet_sp SymbolSet_O::cartesianProduct(SymbolSet_sp b) {
  _G();
  ObjectSet_sp nset = ObjectSet_O::create();
  this->map([&b, &nset](Symbol_sp si) {
                b->map( [&si,&nset] (Symbol_sp bi) {
                        Cons_sp p = Cons_O::create(si,bi);
                        nset->insert(p);
                    } );
  });
  return nset;
}

/*! Return a new set that takes every element of (this) in combination
  with every element in b separated by a comma
*/
ObjectSet_sp SymbolSet_O::cartesianProductWrapped(SymbolSet_sp b, const SymbolSetCartesianProductWrapper &wrapper) {
  _G();
  ObjectSet_sp nset;
  nset = ObjectSet_O::create();
  this->map([&b, &nset, &wrapper](Symbol_sp si) {
                b->map( [&si,&nset,&wrapper] (Symbol_sp bi) {
                        T_sp p = wrapper(si,bi);
                        nset->insert(p);
                    });
  });
  return nset;
}

Symbol_mv SymbolSet_O::first() {
  T_sp found = _Nil<T_O>();
  Symbol_sp result = _Nil<Symbol_O>();
  this->_Symbols->terminatingMapHash([&found, &result](T_sp key, T_sp val) -> bool {
                result = key;
                found = _lisp->_true();
                return false;
  });
  return Values(result, found);
}

Cons_sp SymbolSet_O::asCons() {
  _G();
  Cons_sp cur = _Nil<Cons_O>();
  this->map([&cur](Symbol_sp si) {
                cur = Cons_O::create(si,cur);
  });
  return cur;
}

void SymbolSet_O::map(std::function<void(Symbol_sp)> const &fn) {
  this->_Symbols->mapHash([&fn](T_sp key, T_sp val) {
                fn(key.as<Symbol_O>());
  });
}

void SymbolSet_O::map(std::function<void(Symbol_sp)> const &fn) const {
  this->_Symbols->mapHash([&fn](T_sp key, T_sp val) {
                fn(key.as<Symbol_O>());
  });
}

void SymbolSet_O::exposeCando(Lisp_sp lisp) {
  class_<SymbolSet_O>()
      .def("size", &SymbolSet_O::size)
      .def("insertSymbolSet", &SymbolSet_O::insertSymbolSet)
      .def("insertConsSymbols", &SymbolSet_O::insertConsSymbols)
      .def("insert", &SymbolSet_O::insert)
      .def("contains", &SymbolSet_O::contains)
      .def("containsSubset", &SymbolSet_O::containsSubset)
      //            .def("remove",&SymbolSet_O::remove)
      .def("clear", &SymbolSet_O::clear)
      .def("asString", &SymbolSet_O::asString)
      .def("core:SymbolSetUnion", &SymbolSet_O::setUnion)
      .def("core:SymbolSetIntersection", &SymbolSet_O::intersection)
      .def("relativeComplement", &SymbolSet_O::relativeComplement)
      .def("removeAll", &SymbolSet_O::removeAll)
      .def("cartesianProduct", &SymbolSet_O::cartesianProduct)
      .def("asCons", &SymbolSet_O::asCons);
}

void SymbolSet_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, SymbolSet, "", "", _lisp)
      .def("size", &SymbolSet_O::size)
      .def("insertSymbolSet", &SymbolSet_O::insertSymbolSet)
      .def("insertConsSymbols", &SymbolSet_O::insertConsSymbols)
      .def("insert", &SymbolSet_O::insert)
      .def("contains", &SymbolSet_O::contains)
      .def("containsSubset", &SymbolSet_O::containsSubset)
      //            .def("remove",&SymbolSet_O::remove)
      .def("clear", &SymbolSet_O::clear)
      .def("asString", &SymbolSet_O::asString)
      .def("core:SymbolSetUnion", &SymbolSet_O::setUnion)
      .def("core:SymbolSetIntersection", &SymbolSet_O::intersection)
      .def("relativeComplement", &SymbolSet_O::relativeComplement)
      .def("removeAll", &SymbolSet_O::removeAll)
      .def("cartesianProduct", &SymbolSet_O::cartesianProduct)
      .def("asCons", &SymbolSet_O::asCons);
#endif
}

EXPOSE_CLASS(core, SymbolSet_O);
};
