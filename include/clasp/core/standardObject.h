/*
    File: standardObject.h
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
#ifndef StandardObject_H //[
#define StandardObject_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
//#include "model.h"
#include <clasp/core/environment.h>

namespace core {

SMART(StandardClass);

// Set up this class differently

SMART(StandardObject);
class StandardObject_O : public T_O {
  LISP_META_CLASS(StandardClass);
  LISP_BASE1(T_O);
  LISP_CLASS(core, ClPkg, StandardObject_O, "standard-object");
  DECLARE_INIT();

public:
#if defined(OLD_SERIALIZE)
  void serialize(serialize::SNode node);
#endif
#if defined(XML_ARCHIVE)
  void archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
  void initialize();

private:
  //	StandardClass_sp	_InstanceClass;
  //	Vector0<T_O>		_Slots;
public:
  static StandardObject_sp create(StandardClass_sp instanceClass);

public:
  //	static bool static_supportsSlots() {return true;};
  //	T_sp& slot_ref( Symbol_sp sym ) throw(SlotRefFailed);

  //	void allocate_slot_storage(uint numberOfSlots, T_sp initialValue);

  string __repr__() const;

  //	void setInstanceVariableValue(Symbol_sp sym, T_sp obj);
  //	T_sp getInstanceVariableValue(Symbol_sp sym);

  explicit StandardObject_O() : Base(){};
  virtual ~StandardObject_O(){};
};
};
TRANSLATE(core::StandardObject_O);
#endif //]
