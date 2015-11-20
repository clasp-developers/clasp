/*
    File: smallMap.h
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

#ifndef SmallMultimap_H
#define SmallMultimap_H
#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lispVector.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/cons.h>
#include <clasp/core/symbol.h>
#include <clasp/core/corePackage.fwd.h>

namespace core {
using namespace core;

SMART(SmallMultimap);
class SmallMultimap_O : public T_O {
  LISP_BASE1(T_O);
  LISP_CLASS(core, CorePkg, SmallMultimap_O, "SmallMultimap");
GCPRIVATE:
  typedef gctools::SmallMultimap<Symbol_sp, T_sp, SymbolComparer> map_type;
  map_type map;

public:
  void insert(T_sp key, T_sp val);
  int size() const { return this->map.size(); };
  void erase(T_sp key);
  void describe();
  void describeRange(T_sp key);

  DEFAULT_CTOR_DTOR(SmallMultimap_O);
};
};

TRANSLATE(core::SmallMultimap_O);
#endif
