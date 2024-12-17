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
// #define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/symbolToEnumConverter.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/lisp.h>

// last include is wrappers.h
#include <clasp/core/wrappers.h>

namespace core {

List_sp SymbolToEnumConverter_O::enumSymbolsAsList() const {
  List_sp symbols = nil<T_O>();
  this->_SymbolToEnum->maphash([&symbols](T_sp key, T_sp val) { symbols = Cons_O::create(key, symbols); });
  return symbols;
}

SymbolToEnumConverter_sp SymbolToEnumConverter_O::create(const string& whatDoesEnumRepresent) {
  SymbolToEnumConverter_sp c = SymbolToEnumConverter_O::create();
  c->setWhatTheEnumsRepresent(whatDoesEnumRepresent);
  return c;
}

SymbolToEnumConverter_sp SymbolToEnumConverter_O::create(const string& whatDoesEnumRepresent, string const& packageName,
                                                         NullTerminatedEnumAssociation assoc[], bool exportSymbols) {
  SymbolToEnumConverter_sp c = SymbolToEnumConverter_O::create(whatDoesEnumRepresent);
  for (int i = 0; assoc[i]._Key != ""; ++i) {
    Symbol_sp sym = _lisp->internWithPackageName(packageName, assoc[i]._Key);
    if (exportSymbols)
      sym->exportYourself();
    c->addSymbolEnumPair(sym, sym, assoc[i]._Enum);
  }
  return c;
}

void SymbolToEnumConverter_O::setWhatTheEnumsRepresent(const string& what) {
  this->_WhatTheEnumsRepresent = SimpleBaseString_O::make(what);
}

Symbol_sp SymbolToEnumConverter_O::addSymbolEnumPair(Symbol_sp asym, Symbol_sp const& archiveSym, int enumIndex) {
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

CL_LISPIFY_NAME("enumIndexForSymbol");
CL_DEFMETHOD int SymbolToEnumConverter_O::enumIndexForSymbol(T_sp obj) {
  T_mv match_mv = this->_SymbolToEnum->gethash(obj);
  MultipleValues& mv = core::lisp_multipleValues();
  if (mv.second(match_mv.number_of_values()).nilp())
    TYPE_ERROR(obj, Cons_O::create(cl::_sym_member, this->enumSymbolsAsList()));
  return unbox_fixnum(match_mv);
}

Symbol_sp SymbolToEnumConverter_O::symbolForEnumIndex(int index) {
  Fixnum_sp indexKey = make_fixnum(index);
  ASSERTF(this->_EnumToSymbol->contains(indexKey), "Could not find symbol for EnumIndex({}) in SymbolToEnumConverter({})", index,
          this->_WhatTheEnumsRepresent->get_std_string());
  return gc::As<Symbol_sp>(this->_EnumToSymbol->gethash(indexKey));
}

#if 0
string SymbolToEnumConverter_O::symbolStringForEnumIndex(int index)
{
    ASSERT(this->_EnumToArchiveString.count(index)>0);
    return this->_EnumToArchiveString[index];
}
#endif

void SymbolToEnumConverter_O::throwIfUnrecognizedEnumIndex(int ei) {
  if (!this->recognizesEnumIndex(ei)) {
    SIMPLE_ERROR("I do not recognize the enum value({})\nLegal enum values/symbols are: {}", ei, this->legalEnumValuesAndSymbols());
  }
}

string SymbolToEnumConverter_O::legalEnumValuesAndSymbols() {

  stringstream ss;
  this->_EnumToArchiveSymbol->mapHash([&ss](T_sp key, T_sp val) { ss << "(" << _rep_(key) << "/" << _rep_(val) << ") "; });
  return ss.str();
}

bool SymbolToEnumConverter_O::recognizesEnumIndex(int ei) {

  Fixnum_sp eif = make_fixnum(ei);
  return this->_EnumToSymbol->contains(eif);
}

bool SymbolToEnumConverter_O::recognizesSymbol(Symbol_sp sym) { return this->_SymbolToEnum->contains(sym); }

string SymbolToEnumConverter_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString() << " ";
  ss << " :info " << this->_WhatTheEnumsRepresent->get_std_string() << " ";
  this->_EnumToSymbol->mapHash([&ss](T_sp k, T_sp v) { ss << "#<entry " << _rep_(k) << " " << _rep_(v) << "> "; });
  ss << " > ";
  return ss.str();
}

CL_LAMBDA("converter symbols");
DOCGROUP(clasp);
CL_DEFUN Fixnum core__enum_logical_or(SymbolToEnumConverter_sp converter, List_sp symbols) {
  Fixnum flags = 0;
  for (auto cur : symbols) {
    Symbol_sp sym = gctools::As<Symbol_sp>(oCar(cur));
    Fixnum one_enum = unbox_fixnum(gc::As<Fixnum_sp>(converter->_SymbolToEnum->gethash(sym)));
    flags |= one_enum;
  }
  return flags;
}

void SymbolToEnumConverter_O::initialize() {
  this->Base::initialize();
  this->_EnumToSymbol = HashTable_O::createEql();
  this->_ArchiveSymbolToEnum = HashTable_O::createEq();
  this->_EnumToArchiveSymbol = HashTable_O::createEql();
  this->_SymbolToEnum = HashTable_O::createEq();
}
#if 0
void	SymbolToEnumConverter_O::archiveBase(ArchiveP node)
{
    this->Base::archiveBase(node);
    IMPLEMENT_ME();
}
#endif

}; // namespace core
