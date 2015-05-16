/*
    File: symbolList.h
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

//
// (C) 2004 Christian E. Schafmeister
//

#ifndef SymbolList_H
#define SymbolList_H
#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>

namespace core {

SMART(Residue);
SMART(SymbolSet);
SMART(SymbolList);

/*!
	A class that stores a set of strings
*/
SMART(SymbolList);
class SymbolList_O : public T_O {
  LISP_BASE1(T_O);
  LISP_CLASS(core, CorePkg, SymbolList_O, "SymbolList");

public:
#if defined(XML_ARCHIVE)
  void archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
GCPRIVATE:
  gctools::Vec0<Symbol_sp> _Contents;

public:
  typedef gctools::Vec0<Symbol_sp>::iterator iterator;
  typedef gctools::Vec0<Symbol_sp>::const_iterator const_iterator;

  iterator begin() { return this->_Contents.begin(); };
  iterator end() { return this->_Contents.end(); };
  const_iterator begin() const { return this->_Contents.begin(); };
  const_iterator end() const { return this->_Contents.end(); };

  int size() { return this->_Contents.size(); };
  void prepend(Symbol_sp s);
  void append(Symbol_sp s);
  void appendSymbolList(SymbolList_sp strings);
  void appendConsOfStrings(List_sp strings);

  bool contains(Symbol_sp nm);
  int indexOf(Symbol_sp nm);
  Symbol_sp get(uint i) {
    _G();
    ASSERT_lt(i, this->_Contents.size());
    return this->_Contents[i];
  };
  void clear();

  void setFromVectorStrings(VectorStrings vs);
  string asString();

  SymbolSet_sp asStringSet();

  void map(std::function<void(Symbol_sp)> const &fn);

  SymbolList_O(const SymbolList_O &ss); //!< Copy constructor

  DEFAULT_CTOR_DTOR(SymbolList_O);
};
};

TRANSLATE(core::SymbolList_O);
#endif
