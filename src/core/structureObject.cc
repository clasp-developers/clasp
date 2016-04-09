/*
    File: structureObject.cc
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

#include <clasp/core/lisp.h>
#include <clasp/core/structureObject.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/numbers.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/str.h>
#include <clasp/core/builtInClass.h>
#include <clasp/core/sysprop.h>
#include <clasp/core/instance.h>
#include <clasp/core/structureClass.h>
#include <clasp/core/primitives.h>
#include <clasp/core/wrappers.h>

namespace core {

#define USE_INSTANCES_FOR_STRUCTURES

CL_LAMBDA(type &rest slot_values);
CL_DECLARE();
CL_DOCSTRING("makeStructure");
CL_DEFUN T_sp core__make_structure(T_sp type, List_sp slot_values) {
  if (type.nilp()) {
    SIMPLE_ERROR(BF("You cannot makeStructure of type nil"));
  }
#ifdef CLOS
  if (Class_sp ctype = type.asOrNull<Class_O>()) {
    ASSERTF(!type.nilp(), BF("Tried to make-structure with type = nil"));
    //	printf("%s:%d  core__make_structure of %s  slot_values: %s\n",
    //	       __FILE__, __LINE__, _rep_(type).c_str(), _rep_(slot_values).c_str());
    Instance_sp so = gc::As<Instance_sp>(Instance_O::allocateInstance(ctype, cl__length(slot_values)));
    int idx = 0;
    for (auto slot : slot_values) {
      so->instanceSet(idx, oCar(slot));
      ++idx;
    }
    return so;
  }
#endif // CLOS
  StructureObject_sp so = StructureObject_O::create(type, slot_values);
  return so;
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("copyStructure");
CL_DEFUN T_sp cl__copy_structure(T_sp arg) {
  if (arg.nilp()) {
    SIMPLE_ERROR(BF("You cannot copyStructure nil"));
  }
#ifdef CLOS
  if (Instance_sp iarg = arg.asOrNull<Instance_O>()) {
    return iarg->copyInstance();
  }
#endif
  if (StructureObject_sp so = arg.asOrNull<StructureObject_O>()) {
    return so->copyStructure();
  }
  SIMPLE_ERROR(BF("You cannot copy-structure a %s") % _rep_(arg));
};

CL_LAMBDA(obj name idx);
CL_DECLARE();
CL_DOCSTRING("structureRef");
CL_DEFUN T_sp core__structure_ref(T_sp obj, Symbol_sp type, int idx) {
  if (obj.nilp()) {
    TYPE_ERROR(obj, type);
  }
#ifdef CLOS
  if (Instance_sp so = obj.asOrNull<Instance_O>()) {
    if (!core__structure_subtypep(cl__class_of(so), type)) {
      QERROR_WRONG_TYPE_NTH_ARG(1, obj, type);
    }
    return so->instanceRef(idx);
  }
#endif
  if (StructureObject_sp so = obj.asOrNull<StructureObject_O>()) {
    T_sp soclass = cl__type_of(so);
    if (!core__structure_subtypep(soclass, type)) {
      QERROR_WRONG_TYPE_NTH_ARG(1, obj, type);
    }
    return so->structureRef(idx);
  }
  TYPE_ERROR(obj, type);
};

CL_LAMBDA(struct type idx val);
CL_DECLARE();
CL_DOCSTRING("structureSet");
CL_DEFUN T_sp core__structure_set(T_sp obj, Symbol_sp type, int idx, T_sp val) {
  if (obj.nilp()) {
    TYPE_ERROR(obj, type);
  }
#ifdef CLOS
  if (Instance_sp so = obj.asOrNull<Instance_O>()) {
    if (!core__structure_subtypep(cl__class_of(so), type)) {
      QERROR_WRONG_TYPE_NTH_ARG(1, obj, type);
    }
    return so->instanceSet(idx, val);
  }
#endif
  if (StructureObject_sp so = obj.asOrNull<StructureObject_O>()) {
    T_sp sotype = cl__type_of(so);
    if (!core__structure_subtypep(sotype, type)) {
      QERROR_WRONG_TYPE_NTH_ARG(1, obj, type);
    }
    return so->structureSet(idx, val);
  }
  TYPE_ERROR(obj, type);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("structurep");
CL_DEFUN bool core__structurep(T_sp arg) {
  if (arg.nilp()) {
    return false;
  }
#ifdef CLOS
  if (Instance_sp io = arg.asOrNull<Instance_O>()) {
    if (core__structure_subtypep(io->_instanceClass(), cl::_sym_StructureObject_O))
      return true;
  }
#endif
  if (StructureObject_sp so = arg.asOrNull<StructureObject_O>()) {
    (void)so;
    return true;
  }
  return false;
};

CL_LAMBDA(x y);
CL_DECLARE();
CL_DOCSTRING("structureSubtypep checks if the structure type Y is a subtype of X");
CL_DEFUN bool core__structure_subtypep(T_sp x, Symbol_sp y) {
  if (x.nilp())
    return false;
#ifdef CLOS
  if (Class_sp cx = x.asOrNull<Class_O>()) {
    if (cx->className() == y) {
      return true;
    } else {
      for (auto sup : cx->directSuperclasses()) {
        if (core__structure_subtypep(gc::As<Class_sp>(oCar(sup)), y))
          return true;
      }
      return false;
    }
  }
#endif
  if (Symbol_sp sx = x.asOrNull<Symbol_O>()) {
    do {
      if (sx == y)
        return true;
      SYMBOL_EXPORT_SC_(CorePkg, structure_include);
      sx = gc::As<Symbol_sp>(core__get_sysprop(sx, _sym_structure_include));
    } while (!sx.nilp());
    return false;
  }
  return false;
}

StructureObject_sp StructureObject_O::create(T_sp type, List_sp slot_values) {
  StructureObject_sp co = StructureObject_O::create();
  // This better work or there will be trouble
  co->_Type = gctools::As<Class_sp>(eval::funcall(cl::_sym_findClass,type));
  co->_Slots.resize(cl__length(slot_values));
  int i = 0;
  for (auto cur : slot_values) {
    T_sp val = oCar(cur);
    co->_Slots[i++] = val;
  }
  return co;
}

void StructureObject_O::initialize() {
  LOG(BF("Initializing StructureObject"));
  this->Base::initialize();
  this->_Slots.clear();
}

T_sp StructureObject_O::oinstancepSTAR() const {
  return make_fixnum((int)(this->_Slots.size()));
}

#if defined(OLD_SERIALIZE)
void StructureObject_O::serialize(serialize::SNode node) {
  IMPLEMENT_ME(); // handle slots properly so they are indexed by name
#if 0
	this->Base::serialize(node); 
	node->archiveObject("slots",this->_SlotBinder);
#endif
}
#endif

T_sp StructureObject_O::structureAsList() const {
  Cons_O::CdrType_sp first(_Nil<Cons_O::CdrType_O>());
  Cons_O::CdrType_sp *curP = &first;
  //        gctools::StackRootedPointerToSmartPtr<Cons_O::CdrType_O> cur(&first);
  Cons_sp head = Cons_O::create(this->_Type);
  *curP = head;          // cur.setPointee(head); // *cur = head;
  curP = head->cdrPtr(); // cur.setPointer(head->cdrPtr()); // cur = head->cdrPtr();
  SYMBOL_EXPORT_SC_(CorePkg, structure_slot_descriptions);
  List_sp slots = core__get_sysprop(this->_Type->name(), _sym_structure_slot_descriptions);
  for (; slots.notnilp(); slots = oCdr(slots)) {
    List_sp slotDesc = oCar(slots);
    //    printf("%s:%d slots: %s\n", __FILE__, __LINE__, _rep_(slots).c_str());
    Cons_sp slotNameCons = Cons_O::create(_lisp->internKeyword(gc::As<Symbol_sp>(oCar(slotDesc))->symbolName()->get()));
    *curP = slotNameCons;          // cur.setPointee(slotNameCons); // *cur = slotNameCons;
    curP = slotNameCons->cdrPtr(); // cur.setPointer(slotNameCons->cdrPtr()); // cur = slotNameCons->cdrPtr();
    int idx = unbox_fixnum(gc::As<Fixnum_sp>(oFifth(slotDesc)));
    T_sp val = this->structureRef(idx);
    Cons_sp slotValueCons = Cons_O::create(val);
    *curP = slotValueCons;          // cur.setPointee(slotValueCons); // *cur = slotValueCons;
    curP = slotValueCons->cdrPtr(); // cur.setPointer(slotValueCons->cdrPtr()); // cur = slotValueCons->cdrPtr();
  }
  return first;
}

T_sp StructureObject_O::structureRef(int index) const {
  _OF();
  ASSERTF(index >= 0 && index < this->_Slots.size(), BF("Illegal slot index[%d] - must be less than %d") % index % this->_Slots.size());
  return this->_Slots[index];
}

T_sp StructureObject_O::structureSet(int index, T_sp value) {
  _OF();
  ASSERTF(index >= 0 && index < this->_Slots.size(), BF("Illegal slot index[%d] - must be less than %d") % index % this->_Slots.size());
  this->_Slots[index] = value;
  return value;
}

void StructureObject_O::archiveBase(ArchiveP node) {
  // Call out to core:serialize
  IMPLEMENT_MEF(BF("Call out to core::serialize me node")); // handle slots properly so that they are indexed by name
}

T_sp StructureObject_O::copyStructure() const {
  StructureObject_sp copy = gctools::GC<StructureObject_O>::copy(*this);
  //GC_COPY(StructureObject_O,copy,*this);
  return copy;
}

string StructureObject_O::__repr__() const {
  stringstream ss;
  ss << "#< ";
  ss << this->_instanceClass()->classNameAsString() << " ";
  ASSERT(this->_Type);
  ss << ":type " << _rep_(this->_Type) << std::endl;
  ss << "[slots ";
  for (int i = 0; i < this->_Slots.size(); i++) {
    if (this->_Slots[i].nilp()) {
      ss << "NIL ";
    } else if (this->_Slots[i].get() == this) {
      ss << "SELF_REFERENCE!!!! ";
    } else {
      ss << _rep_(this->_Slots[i]) << " ";
    }
  }
  ss << "]";
  ss << " >";
  return ss.str();
}

#if 0
    T_sp& StructureObject_O::slot_ref(Symbol_sp slot_name) throw(SlotRefFailed)
    {_OF();
	StructureClass_sp cc = this->_Type;
	ASSERT(cc.notnilp());
	// TODO: What about class slots? slot_location returns the index of an instance slot
	// but where do we store class slots and how do we return a reference to one of them
	LOG(BF("This object is of class: %s") % cc->__repr__() );
	uint location = cc->slot_location(slot_name); // Can throw SlotRefFailed if slot_name not found
	LOG(BF("Found the slot with name: %s")% slot_name->__repr__() );
	LOG(BF("   the number of slots in this object are: %d")% this->_Slots.size());
	return this->_Slots[location];
    }
#endif

#if 0
    void StructureObject_O::allocate_slot_storage(uint numSlots, T_sp initialValue )
    {
	this->_Slots.resize(numSlots,initialValue);
    }
#endif

  SYMBOL_EXPORT_SC_(CorePkg, structureRef);
  SYMBOL_EXPORT_SC_(CorePkg, structureSet);
  SYMBOL_EXPORT_SC_(CorePkg, makeStructure);
  SYMBOL_EXPORT_SC_(ClPkg, copyStructure);
  SYMBOL_EXPORT_SC_(CorePkg, structurep);
  SYMBOL_EXPORT_SC_(CorePkg, structureSubtypep);
};
