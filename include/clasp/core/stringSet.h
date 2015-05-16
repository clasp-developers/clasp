/*
    File: stringSet.h
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

#ifndef StringSet_H
#define StringSet_H
#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/holder.h>
#include <clasp/core/lispVector.h>
#include <clasp/core/cons.h>

namespace core {

SMART(Residue);
SMART(StringSet);
SMART(StringList);

/*!
	A class that stores a set of strings
*/
SMART(StringSet);
class StringSet_O : public T_O {
  LISP_BASE1(T_O);
  LISP_CLASS(core, CorePkg, StringSet_O, "StringSet");
  DECLARE_INIT();

public:
  void initialize();
  void archiveBase(ArchiveP node);

private:
  set<string> strs;
  bool rest;

public:
public:
  typedef set<string>::iterator iterator;

  iterator begin() { return this->strs.begin(); };
  iterator end() { return this->strs.end(); };

  StringSet_sp copy();

  string first() { return *(this->strs.begin()); };
  uint size() { return this->strs.size(); };
  void remove(const string &s);
  bool contains(const string &s);
  bool containsSubset(StringSet_sp sub);
  void insert(const string &s) { this->strs.insert(s); };
  void insertVectorStrings(VectorStrings s);
  void insertStringSet(StringSet_sp ss);
  void insertStringList(StringList_sp ss);
  StringSet_sp insertConsStrings(List_sp list);
  void clear();

  List_sp asCons() const;
  Vector_sp asVector() const;

  bool equal(T_sp ss) const;

  // Set theory operations

  //! A setUnion B = (x:x E A || x E B)
  StringSet_sp setUnion(StringSet_sp b);

  //! A intersection B = (x:x E A && x E B)
  StringSet_sp intersection(StringSet_sp b);

  //! A-B = (x: x E A && not x E B )
  StringSet_sp relativeComplement(StringSet_sp b);

  StringSet_sp removeAll(StringSet_sp b) { return this->relativeComplement(b); };

  //! AxB = ("x,y": x E A ; y E B )
  StringSet_sp cartesianProduct(StringSet_sp b) {
    return this->cartesianProductInsert("->", b);
  };
  StringSet_sp cartesianProductInsert(string ins, StringSet_sp b);

  void setFromString(const string &s);

#if 0
	template <class oclass>
	void	setFromMapKeys(const StringMap<oclass>& map)
	{
	    this->clear;
	    StringMap<oclass>::iterator it;
	    for ( it=map.begin(); it!=map.end(); map++ )
	    {
		this->insert(it->first);
	    }
	}
#endif
  string asString();
  std::ostream &dumpToStream(std::ostream &o);

  string __repr__() const;

  StringSet_O(const StringSet_O &ss); //!< Copy constructor

  DEFAULT_CTOR(StringSet_O);
  virtual ~StringSet_O();
};
};

TRANSLATE(core::StringSet_O);
#endif
