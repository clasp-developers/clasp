/*
    File: symbolToEnumConverter.cc
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

#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/symbolToEnumConverter.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/environment.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/binder.h>
#include <clasp/core/lisp.h>

// last include is wrappers.h
#include <clasp/core/wrappers.h>

namespace core {

void SymbolToEnumConverter_O::exposeCando(Lisp_sp e) {
  class_<SymbolToEnumConverter_O>()
      .def("enumIndexForSymbol", &SymbolToEnumConverter_O::enumIndexForSymbol);
}
void SymbolToEnumConverter_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON //[
  PYTHON_CLASS(CorePkg, SymbolToEnumConverter, "", "", _lisp);
#endif //]
}

SymbolToEnumConverter_sp SymbolToEnumConverter_O::create(const string &whatDoesEnumRepresent) {
  _G();
  SymbolToEnumConverter_sp c = SymbolToEnumConverter_O::create();
  c->setWhatTheEnumsRepresent(whatDoesEnumRepresent);
  return c;
}

SymbolToEnumConverter_sp SymbolToEnumConverter_O::create(const string &whatDoesEnumRepresent, string const &packageName, NullTerminatedEnumAssociation assoc[], bool exportSymbols) {
  SymbolToEnumConverter_sp c = SymbolToEnumConverter_O::create(whatDoesEnumRepresent);
  for (int i = 0; assoc[i]._Key != ""; ++i) {
    Symbol_sp sym = _lisp->internWithPackageName(packageName, assoc[i]._Key);
    if (exportSymbols)
      sym->exportYourself();
    c->addSymbolEnumPair(sym, sym, assoc[i]._Enum);
  }
  return c;
}

void SymbolToEnumConverter_O::setWhatTheEnumsRepresent(const string &what) {
  _OF();
  this->_WhatTheEnumsRepresent = what;
}

Symbol_sp SymbolToEnumConverter_O::addSymbolEnumPair(Symbol_sp asym, Symbol_sp const &archiveSym, int enumIndex) {
  _OF();
  Symbol_sp sym = asym;
  if (sym.nilp()) {
    sym = archiveSym;
  }
  Fixnum_sp enumIndexKey = make_fixnum(enumIndex);
  this->_EnumToSymbol->setf_gethash(enumIndexKey, sym); // [enumIndex] = sym;
  this->_ArchiveSymbolToEnum->setf_gethash(archiveSym, enumIndexKey);
  this->_EnumToArchiveSymbol->setf_gethash(enumIndexKey, archiveSym);
  this->_SymbolToEnum->setf_gethash(sym, enumIndexKey);
  return sym;
}

int SymbolToEnumConverter_O::enumIndexForSymbol(Symbol_sp sym) {
  _OF();
  if (!this->_SymbolToEnum->contains(sym)) {
    SIMPLE_ERROR(BF("Could not find %s in symbol-to-enum-converter: %s") % _rep_(sym) % _rep_(this->sharedThis<SymbolToEnumConverter_O>()));
  }
  return unbox_fixnum(gc::As<Fixnum_sp>(this->_SymbolToEnum->gethash(sym)));
}

Symbol_sp SymbolToEnumConverter_O::symbolForEnumIndex(int index) {
  _OF();
  Fixnum_sp indexKey = make_fixnum(index);
  ASSERTF(this->_EnumToSymbol->contains(indexKey), BF("Could not find symbol for EnumIndex(%d) in SymbolToEnumConverter(%s)") % index % this->_WhatTheEnumsRepresent.c_str());
  return gc::As<Symbol_sp>(this->_EnumToSymbol->gethash(indexKey));
}

#if 0
string SymbolToEnumConverter_O::symbolStringForEnumIndex(int index)
{_OF();
    ASSERT(this->_EnumToArchiveString.count(index)>0);
    return this->_EnumToArchiveString[index];
}
#endif

void SymbolToEnumConverter_O::throwIfUnrecognizedEnumIndex(int ei) {
  _OF();
  if (!this->recognizesEnumIndex(ei)) {
    stringstream ss;
    ss << (BF("I do not recognize the enum value(%d)") % ei).str() << std::endl;
    ss << "Legal enum values/symbols are: " << std::endl;
    ss << this->legalEnumValuesAndSymbols() << std::endl;
    SIMPLE_ERROR(BF(ss.str()));
  }
}

string SymbolToEnumConverter_O::legalEnumValuesAndSymbols() {
  _OF();
  stringstream ss;
  this->_EnumToArchiveSymbol->mapHash([&ss](T_sp key, T_sp val) {
            ss << "(" << _rep_(key) << "/" << _rep_(val) << ") ";
  });
  return ss.str();
}

#if 0
bool SymbolToEnumConverter_O::recognizesSymbolString(const string& enumStr)
{_OF();
    return this->_ArchiveStringToEnum.count(enumStr)>0;
}
#endif

bool SymbolToEnumConverter_O::recognizesEnumIndex(int ei) {
  _OF();
  Fixnum_sp eif = make_fixnum(ei);
  return this->_EnumToSymbol->contains(eif);
}

bool SymbolToEnumConverter_O::recognizesSymbol(Symbol_sp sym) {
  _OF();
  return this->_SymbolToEnum->contains(sym);
}

string SymbolToEnumConverter_O::__repr__() const {
  _G();
  stringstream ss;
  ss << "#<" << this->_instanceClass()->classNameAsString() << " ";
  ss << " :info " << this->_WhatTheEnumsRepresent.c_str() << " ";
  this->_EnumToSymbol->mapHash([&ss](T_sp k, T_sp v) {
                ss << "#<entry " << _rep_(k) << " " <<_rep_(v) << "> ";
  });
  ss << " > ";
  return ss.str();
}

void SymbolToEnumConverter_O::initialize() {
  this->Base::initialize();
  this->_EnumToSymbol = HashTableEql_O::create_default();
  this->_ArchiveSymbolToEnum = HashTableEq_O::create_default();
  this->_EnumToArchiveSymbol = HashTableEql_O::create_default();
  this->_SymbolToEnum = HashTableEq_O::create_default();
}
#if 0
void	SymbolToEnumConverter_O::archiveBase(ArchiveP node)
{
    this->Base::archiveBase(node);
    IMPLEMENT_ME();
}
#endif

EXPOSE_CLASS(core, SymbolToEnumConverter_O);
};
