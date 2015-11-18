/*
    File: instance.cc
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
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/serialize.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/standardClass.h>
#include <clasp/core/genericFunction.h>
#include <clasp/core/instance.h>
#include <clasp/core/wrappers.h>

namespace core {

T_sp InstanceClosure::lambdaList() const {
  printf("%s:%d InstanceClosure::lambdaList returning NIL\n", __FILE__, __LINE__);
  return _Nil<T_O>();
}

LCC_RETURN InstanceClosure::LISP_CALLING_CONVENTION() {
// Copy the arguments passed in registers into the multiple_values array and those
// will be processed by the generic function
#ifdef _DEBUG_BUILD
  VaList_S saved_args(*reinterpret_cast<VaList_S *>(untag_valist(lcc_arglist)));
#endif
  VaList_sp gfargs((gc::Tagged)lcc_arglist);
  //  LCC_SKIP_ARG(gfargs);
  return (this->entryPoint)(this->instance, gfargs);
}

#define ARGS_clos_setFuncallableInstanceFunction "(instance func)"
#define DECL_clos_setFuncallableInstanceFunction ""
#define DOCS_clos_setFuncallableInstanceFunction "setFuncallableInstanceFunction"
T_sp clos_setFuncallableInstanceFunction(T_sp obj, T_sp func) {
  _G();
  if (Instance_sp iobj = obj.asOrNull<Instance_O>()) {
    return iobj->setFuncallableInstanceFunction(func);
  }
  SIMPLE_ERROR(BF("You can only setFuncallableInstanceFunction on instances - you tried to set it on a: %s") % _rep_(obj));
};

#define ARGS_af_instanceClassSet "(instance func)"
#define DECL_af_instanceClassSet ""
#define DOCS_af_instanceClassSet "instanceClassSet"
T_sp af_instanceClassSet(T_sp obj, Class_sp mc) {
  _G();
  if (Instance_sp iobj = obj.asOrNull<Instance_O>()) {
    return iobj->instanceClassSet(mc);
  } else if (Class_sp cobj = obj.asOrNull<Class_O>()) {
    return cobj->instanceClassSet(mc);
  }
  SIMPLE_ERROR(BF("You can only instanceClassSet on Instance_O or Class_O - you tried to set it on a: %s") % _rep_(mc));
};

#define ARGS_core_copyInstance "(obj)"
#define DECL_core_copyInstance ""
#define DOCS_core_copyInstance "copy-instance returns a shallow copy of the instance"
Instance_sp core_copyInstance(Instance_sp obj) {
  _G();
  Instance_sp cp = obj->copyInstance();
  return cp;
};

void Instance_O::setKind(Symbol_sp k) {
  if (k == kw::_sym_macro) {
    SIMPLE_ERROR(BF("You cannot set a generic-function (instance) to macro"));
  }
}

void Instance_O::initializeSlots(int numberOfSlots) {
  this->_Slots.resize(numberOfSlots, _Unbound<T_O>());
}

T_sp Instance_O::oinstancep() const {
  return make_fixnum((gctools::Fixnum)(this->_Slots.size()));
}

T_sp Instance_O::oinstancepSTAR() const {
  return make_fixnum((gctools::Fixnum)(this->_Slots.size()));
}

/*! See ECL>>instance.d>>si_allocate_instance */
T_sp Instance_O::allocateInstance(T_sp theClass, int numberOfSlots) {
  Class_sp cl = gc::As<Class_sp>(theClass);
  if (!cl->hasCreator()) {
    IMPLEMENT_MEF(BF("Handle no allocator class: %s slots: %d") % _rep_(theClass) % numberOfSlots);
  }
  gc::tagged_pointer<core::Creator> allocatorP = (cl->getCreator());
  T_sp obj = allocatorP->allocate();
  ASSERT(obj);
  ASSERT(obj.notnilp());
  obj->instanceClassSet(gc::As<Class_sp>(theClass));
  obj->initializeSlots(numberOfSlots);
  return (obj);
}

/*! See ECL>>instance.d>>si_allocate_raw_instance */
T_sp Instance_O::allocateRawInstance(T_sp orig, T_sp theClass, int numberOfSlots) {
  T_sp toutput = Instance_O::allocateInstance(theClass, numberOfSlots);
  Instance_sp output = toutput.asOrNull<Instance_O>();
  if (!output) {
    SIMPLE_ERROR(BF("Could not convert a newly allocated instance of %s to Instance_sp - this going to require implementing the new Instance_O derived Kinds") % _rep_(theClass));
  }
  if (orig.nilp()) {
    orig = output;
  } else if (Instance_sp iorig = orig.asOrNull<Instance_O>()) {
    iorig->instanceClassSet(gc::As<Class_sp>(theClass));
    iorig->_Slots = output->_Slots; // orig->adoptSlots(output);
  }
  return (orig);
}

void Instance_O::archiveBase(ArchiveP node) {
  if (node->saving()) {
    if (this->_isgf || this->isCallable()) {
      SIMPLE_ERROR(BF("You cannot archive FUNCALLABLE instances or generic-functions"));
    }
    SYMBOL_EXPORT_SC_(KeywordPkg, iclass);
    //	    Symbol_sp className = this->_Class->name();
    //	    node->attribute(kw::_sym_iclass,className);
    for (int i(0); i < this->_Slots.size(); ++i) {
      node->pushVector(this->_Slots[i]);
    }
  } else {
    this->_isgf = false;
    this->closure.reset_();
#if 1
    Symbol_sp className = node->getKind();
    //	    node->attribute(kw::_sym_iclass,className);
    Class_sp cl = gc::As<Class_sp>(eval::funcall(cl::_sym_findClass, className, _lisp->_true()));
    this->_Class = cl;
    this->initializeSlots(node->vectorSize());
#endif
    int idx(0);
    if (node->vectorSize() != this->_Slots.size()) {
      SIMPLE_ERROR(BF("While loading archive class %s mismatch in number of slots - expected %d - loaded %d slots") % _rep_(this->_Class) % this->_Slots.size() % node->vectorSize());
    }
    node->mapVector([this, &idx](T_sp value) {
		    this->_Slots[idx++] = value;
    });
    this->instanceSigSet();
  }
}

T_sp Instance_O::instanceSigSet() {
  T_sp classSlots(_Nil<T_O>());
  Class_sp mc = this->_instanceClass();
  classSlots = mc->slots();
  this->_Sig = classSlots;
  return ((classSlots));
}

T_sp Instance_O::instanceSig() const {
#if DEBUG_CLOS >= 2
  stringstream ssig;
  if (this->_Sig) {
    ssig << this->_Sig->__repr__();
  } else {
    ssig << "UNDEFINED ";
  }
  printf("\nMLOG INSTANCE-SIG of Instance %p \n", (void *)(this));
#endif
  return ((this->_Sig));
}

EXPOSE_CLASS(core, Instance_O);

void Instance_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<Instance_O>();

  af_def(CorePkg, "allocateRawInstance", &Instance_O::allocateRawInstance);
  SYMBOL_EXPORT_SC_(ClosPkg, setFuncallableInstanceFunction);
  ClosDefun(setFuncallableInstanceFunction);
  SYMBOL_EXPORT_SC_(CorePkg, instanceClassSet);
  Defun(instanceClassSet);
  CoreDefun(copyInstance);
}

void Instance_O::exposePython(core::Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, Instance, "", "", _lisp);
#endif
}

T_sp Instance_O::instanceClassSet(Class_sp mc) {
  _G();
  this->_Class = mc;
  return (this->sharedThis<Instance_O>());
}

T_sp Instance_O::instanceRef(int idx) const {
#if DEBUG_CLOS >= 2
  printf("\nMLOG INSTANCE-REF[%d] of Instance %p --->%s\n", idx, (void *)(this), this->_Slots[idx]->__repr__().c_str());
#endif
  return ((this->_Slots[idx]));
}
T_sp Instance_O::instanceSet(int idx, T_sp val) {
#if DEBUG_CLOS >= 2
  printf("\nMLOG SI-INSTANCE-SET[%d] of Instance %p to val: %s\n", idx, (void *)(this), val->__repr__().c_str());
#endif
  this->_Slots[idx] = val;
  return ((val));
}

string Instance_O::__repr__() const {
  stringstream ss;
  ss << "#S(";
  if (Class_sp mc = this->_Class.asOrNull<Class_O>()) {
    ss << _rep_(mc) << " ";
  } else {
    ss << "<ADD SUPPORT FOR INSTANCE _CLASS=" << _rep_(this->_Class) << " >";
  }
  {
    ss << " #slots[" << this->_Slots.size() << "]" << std::endl;
#if 1
    for (int i(0); i < this->_Slots.size(); ++i) {
      T_sp obj = this->_Slots[i];
      ss << "        :slot" << i << " ";
      if (obj) {
        stringstream sslot;
        if (cl_consp(obj)) {
          sslot << "CONS...";
          ss << sslot.str() << std::endl;
        } else if (Instance_sp inst = obj.asOrNull<Instance_O>()) {
          (void)inst; // turn off warning
          sslot << "INSTANCE...";
          ss << sslot.str() << std::endl;
        } else {
          sslot << _rep_(obj);
          if (sslot.str().size() > 80) {
            ss << sslot.str().substr(0, 80) << "...";
          } else {
            ss << sslot.str();
          }
          ss << " " << std::endl;
        }
      } else {
        ss << "UNDEFINED " << std::endl;
      }
    }
#endif
  }
  ss << ")" << std::endl;
  return ((ss.str()));
}

T_sp Instance_O::copyInstance() const {
  Instance_sp iobj = gc::As<Instance_sp>(Instance_O::allocateInstance(this->_Class));
  iobj->_isgf = this->_isgf;
  iobj->_Slots = this->_Slots;
  if ((bool)(this->closure)) {
    auto ic = this->closure.as<InstanceClosure>();
    iobj->closure = gctools::ClassAllocator<InstanceClosure>::allocateClass(*ic);
  } else {
    iobj->closure.reset_();
  }
  iobj->_Sig = this->_Sig;
  return iobj;
}

void Instance_O::reshapeInstance(int delta) {
  _G();
  int size = this->_Slots.size() + delta;
  this->_Slots.resize(size, _Unbound<T_O>());
}
/*
  memcpy(aux->instance.slots, x->instance.slots,
  (delta < 0 ? aux->instance.length : x->instance.length) *
  sizeof(cl_object));
  x->instance = aux->instance;
*/

SYMBOL_SC_(ClosPkg, standardOptimizedReaderMethod);
SYMBOL_SC_(ClosPkg, standardOptimizedWriterMethod);

void Instance_O::ensureClosure(GenericFunctionPtr entryPoint) {
  if (!(bool)(this->closure)) {
    this->closure = gctools::ClassAllocator<InstanceClosure>::allocateClass(this->GFUN_NAME(), entryPoint, this->asSmartPtr());
  } else {
    auto ic = this->closure.as<InstanceClosure>();
    ic->entryPoint = entryPoint;
  }
};

T_sp Instance_O::setFuncallableInstanceFunction(T_sp functionOrT) {
  _G();
  if (this->_isgf == ECL_USER_DISPATCH) {
    this->reshapeInstance(-1);
    this->_isgf = ECL_NOT_FUNCALLABLE;
  }
  SYMBOL_EXPORT_SC_(ClPkg, standardGenericFunction);
  SYMBOL_SC_(ClosPkg, standardOptimizedReaderFunction);
  SYMBOL_SC_(ClosPkg, standardOptimizedWriterFunction);
  if (functionOrT == _lisp->_true()) {
    this->_isgf = ECL_STANDARD_DISPATCH;
    Instance_O::ensureClosure(&generic_function_dispatch);
  } else if (functionOrT == cl::_sym_standardGenericFunction) {
    this->_isgf = ECL_RESTRICTED_DISPATCH;
    Instance_O::ensureClosure(&generic_function_dispatch);
  } else if (functionOrT.nilp()) {
    this->_isgf = ECL_NOT_FUNCALLABLE;
    Instance_O::ensureClosure(&notFuncallableDispatch);
  } else if (functionOrT == clos::_sym_standardOptimizedReaderMethod) {
    /* WARNING: We assume that f(a,...) behaves as f(a,b) */
    this->_isgf = ECL_READER_DISPATCH;
    // TODO: Switch to using slotReaderDispatch like ECL for improved performace
    //	    this->_Entry = &slotReaderDispatch;
    //Instance_O::ensureClosure(&generic_function_dispatch);
    Instance_O::ensureClosure(&slotReaderDispatch);
  } else if (functionOrT == clos::_sym_standardOptimizedWriterMethod) {
    /* WARNING: We assume that f(a,...) behaves as f(a,b) */
    this->_isgf = ECL_WRITER_DISPATCH;
    // TODO: Switch to using slotWriterDispatch like ECL for improved performace
    //	    this->_Entry = &slotWriterDispatch;
    //Instance_O::ensureClosure(&generic_function_dispatch);
    Instance_O::ensureClosure(&slotReaderDispatch);
  } else if (!cl_functionp(functionOrT)) {
    TYPE_ERROR(functionOrT, cl::_sym_function);
    //SIMPLE_ERROR(BF("Wrong type argument: %s") % functionOrT->__repr__());
  } else {
    this->reshapeInstance(+1);
    this->_Slots[this->_Slots.size() - 1] = functionOrT;
    this->_isgf = ECL_USER_DISPATCH;
    // TODO: Switch to using userFunctionDispatch like ECL for improved performace
    //	    this->_Entry = &userFunctionDispatch;
    Instance_O::ensureClosure(&generic_function_dispatch);
  }
  return ((this->sharedThis<Instance_O>()));
}

bool Instance_O::genericFunctionP() const {
  return (this->_isgf);
}

bool Instance_O::equalp(T_sp obj) const {
  if (obj.nilp())
    return false;
  if (this->eq(obj))
    return true;
  if (Instance_sp iobj = obj.asOrNull<Instance_O>()) {
    if (this->_Class != iobj->_Class)
      return false;
    for (int i(0), iEnd(this->_Slots.size()); i < iEnd; ++i) {
      if (!cl_equalp(this->_Slots[i], iobj->_Slots[i])) {
        return false;
      }
    }
    return true;
  }
  return false;
}

void Instance_O::sxhash_(HashGenerator &hg) const {
  if (hg.isFilling())
    hg.hashObject(this->_Class);
  for (int i(0), iEnd(this->_Slots.size()); i < iEnd; ++i) {
    if (!this->_Slots[i].unboundp() && hg.isFilling())
      hg.hashObject(this->_Slots[i]);
    else
      break;
  }
}

void Instance_O::LISP_INVOKE() {
  IMPLEMENT_ME();
#if 0
	ASSERT(this->_Entry!=NULL);
	LispCompiledFunctionIHF _frame(_lisp->invocationHistoryStack(),this->asSmartPtr());
	return(( (this->_Entry)(*this,nargs,args)));
#endif
}

void Instance_O::describe(T_sp stream) {
  stringstream ss;
  ss << (BF("Instance\n")).str();
  ss << (BF("isgf %d\n") % this->_isgf).str();
  ss << (BF("_Class: %s\n") % _rep_(this->_Class).c_str()).str();
  for (int i(0); i < this->_Slots.size(); ++i) {
    ss << (BF("_Slots[%d]: %s\n") % i % _rep_(this->_Slots[i]).c_str()).str();
  }
  clasp_write_string(ss.str(), stream);
}
};
