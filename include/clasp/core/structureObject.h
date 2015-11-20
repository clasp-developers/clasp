/*
    File: structureObject.h
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
#ifndef StructureObject_H //[
#define StructureObject_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/structureClass.fwd.h>
//#include "model.h"
#include <clasp/core/environment.h>

namespace core {

SMART(StandardClass);

// Set up this class differently

SMART(StructureObject);
class StructureObject_O : public T_O {
  LISP_META_CLASS(StructureClass);
  LISP_BASE1(T_O);
  LISP_CLASS(core, ClPkg, StructureObject_O, "structure-object");

public:
#if defined(OLD_SERIALIZE)
  void serialize(serialize::SNode node);
#endif
  void archiveBase(ArchiveP node);
  void initialize();
GCPRIVATE:
  T_sp _Type;
  gctools::Vec0<T_sp> _Slots;

public:
  static StructureObject_sp create(T_sp type, List_sp slotNames);

public:
  T_sp structureRef(int index) const;
  T_sp structureSet(int index, T_sp value);

  /*! Convert this structure to a list (type {slot-name slot-value}*) */
  T_sp structureAsList() const;
#if 0
	static bool static_supportsSlots() {return true;};
	T_sp& slot_ref( Symbol_sp sym ) throw(SlotRefFailed);
#endif

  T_sp structureType() const { return this->_Type; };

  void allocate_slot_storage(uint numberOfSlots, T_sp initialValue);

  void __write__(T_sp stream) const;

  string __repr__() const;

  //	void setInstanceVariableValue(Symbol_sp sym, T_sp obj);
  //	T_sp getInstanceVariableValue(Symbol_sp sym);

  /*! Return number of slots if not nil otherwise nil */
  virtual T_sp oinstancepSTAR() const;

  virtual T_sp copyStructure() const;

  explicit StructureObject_O() : Base(){};
  virtual ~StructureObject_O(){};
};

T_sp af_makeStructure(T_sp type, List_sp slot_values);
T_sp af_copyStructure(T_sp arg);
T_sp af_structureRef(T_sp obj, Symbol_sp type, int idx);
T_sp af_structureSet(T_sp obj, Symbol_sp type, int idx, T_sp val);
bool af_structurep(T_sp arg);
bool af_structureSubtypep(T_sp x, Symbol_sp y);
};
TRANSLATE(core::StructureObject_O);
#endif //]
