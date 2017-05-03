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
//#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/symbolTable.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/core/serialize.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/standardClass.h>
#include <clasp/core/genericFunction.h>
#include <clasp/core/accessor.h>
#include <clasp/core/instance.h>
#include <clasp/core/wrappers.h>

namespace core {

CL_DEFUN T_mv clos__getFuncallableInstanceFunction(T_sp obj) {
  if (Instance_sp iobj = obj.asOrNull<Instance_O>()) {
    switch (iobj->_isgf) {
    case CLASP_STANDARD_DISPATCH:
        return Values(_lisp->_true(),Pointer_O::create((void*)iobj->_entryPoint));
    case CLASP_RESTRICTED_DISPATCH:
        return Values(cl::_sym_standardGenericFunction,Pointer_O::create((void*)iobj->_entryPoint));
    case CLASP_READER_DISPATCH:
        return Values(clos::_sym_standardOptimizedReaderMethod,Pointer_O::create((void*)iobj->_entryPoint));
    case CLASP_WRITER_DISPATCH:
        return Values(clos::_sym_standardOptimizedWriterMethod,Pointer_O::create((void*)iobj->_entryPoint));
    case CLASP_USER_DISPATCH:
        return Values(iobj->userFuncallableInstanceFunction(),Pointer_O::create((void*)iobj->_entryPoint));
    case CLASP_STRANDH_DISPATCH:
        return Values(iobj->GFUN_DISPATCHER(),Pointer_O::create((void*)iobj->_entryPoint));
    case CLASP_INVALIDATED_DISPATCH:
        return Values(clos::_sym_invalidated_dispatch_function,Pointer_O::create((void*)iobj->_entryPoint));
    case CLASP_NOT_FUNCALLABLE:
        return Values(clos::_sym_not_funcallable);
    }
    return Values(clasp_make_fixnum(iobj->_isgf),_Nil<T_O>());
  }
  return Values(_Nil<T_O>(),_Nil<T_O>());
};

CL_DEFUN T_sp clos__setFuncallableInstanceFunction(T_sp obj, T_sp func) {
  if (Instance_sp iobj = obj.asOrNull<Instance_O>()) {
    return iobj->setFuncallableInstanceFunction(func);
  }
  SIMPLE_ERROR(BF("You can only setFuncallableInstanceFunction on instances - you tried to set it on a: %s") % _rep_(obj));
};

CL_LAMBDA(instance func);
CL_DECLARE();
CL_DOCSTRING("instanceClassSet");
CL_DEFUN T_sp core__instance_class_set(T_sp obj, Class_sp mc) {
  if (Instance_sp iobj = obj.asOrNull<Instance_O>()) {
    return iobj->instanceClassSet(mc);
  } else if (Class_sp cobj = obj.asOrNull<Class_O>()) {
    return cobj->instanceClassSet(mc);
  }
  SIMPLE_ERROR(BF("You can only instanceClassSet on Instance_O or Class_O - you tried to set it on a: %s") % _rep_(mc));
};

CL_LAMBDA(obj);
CL_DECLARE();
CL_DOCSTRING("copy-instance returns a shallow copy of the instance");
CL_DEFUN T_sp core__copy_instance(T_sp obj) {
  if (gc::IsA<Instance_sp>(obj)) {
    Instance_sp iobj = gc::As_unsafe<Instance_sp>(obj);
    Instance_sp cp = iobj->copyInstance();
    return cp;
  } else if (gc::IsA<Class_sp>(obj)) {
    Class_sp cobj = gc::As_unsafe<Class_sp>(obj);
    Class_sp cp = cobj->copyInstance();
    return cp;
  }
  SIMPLE_ERROR(BF("copy-instance doesn't support copying %s") % _rep_(obj));
};

void Instance_O::GFUN_CALL_HISTORY_set(T_sp h) {
#ifdef DEBUG_GFDISPATCH
  if (_sym_STARdebug_dispatchSTAR->symbolValue().notnilp()) {
    printf("%s:%d   GFUN_CALL_HISTORY_set gf: %s\n", __FILE__, __LINE__, this->__repr__().c_str());
    printf("%s:%d                      history: %s\n", __FILE__, __LINE__, _rep_(h).c_str());
  }
#endif
  this->instanceSet(4,h);
}

void Instance_O::set_kind(Symbol_sp k) {
  if (k == kw::_sym_macro) {
    SIMPLE_ERROR(BF("You cannot set a generic-function (instance) to macro"));
  }
}

void Instance_O::initializeSlots(Fixnum stamp, size_t numberOfSlots) {
  this->_Rack = SimpleVector_O::make(numberOfSlots+1,_Unbound<T_O>(),true);
  this->stamp_set(stamp);
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(this->_Rack);
#endif
}

T_sp Instance_O::oinstancep() const {
  return make_fixnum((gctools::Fixnum)(this->numberOfSlots()));
}

T_sp Instance_O::oinstancepSTAR() const {
    return make_fixnum((gctools::Fixnum)(this->numberOfSlots()));
  }

/*! See ECL>>instance.d>>si_allocate_instance */
T_sp core__allocateInstance(T_sp theClass, size_t numberOfSlots) {
  Class_sp cl = gc::As<Class_sp>(theClass);
  if (!cl->has_creator()) {
    IMPLEMENT_MEF(BF("Handle no allocator class: %s slots: %d") % _rep_(theClass) % numberOfSlots);
  }
  Creator_sp creator = gctools::As<Creator_sp>(cl->class_creator());
  Instance_sp obj = gctools::As<Instance_sp>(creator->creator_allocate());
  obj->instanceClassSet(gc::As<Class_sp>(theClass));
  obj->initializeSlots(cl->get_instance_stamp(),numberOfSlots);
  return obj;
}

/*! See ECL>>instance.d>>si_allocate_raw_instance */
CL_LISPIFY_NAME(allocate_raw_instance);
CL_DEFUN T_sp core__allocate_raw_instance(T_sp orig, T_sp tclass, size_t numberOfSlots) {
  Class_sp class_ = gc::As<Class_sp>(tclass);
  if (class_->_theCreator->creates_classes()) {
    return core__allocate_raw_class(orig,tclass,numberOfSlots);
  }
  T_sp toutput = core__allocateInstance(tclass, numberOfSlots);
  Instance_sp output = toutput.asOrNull<Instance_O>();
  if (!output) {
    SIMPLE_ERROR(BF("Could not convert a newly allocated instance of %s to Instance_sp - this going to require implementing the new Instance_O derived Kinds") % _rep_(tclass));
  }
  if (orig.nilp()) {
    orig = output;
  } else if (Instance_sp iorig = orig.asOrNull<Instance_O>()) {
    iorig->instanceClassSet(gc::As<Class_sp>(tclass));
    iorig->_Rack = output->_Rack; // orig->adoptSlots(output);
  }
  return (orig);
}


size_t Instance_O::rack_stamp_offset() {
  SimpleVector_O dummy_rack(0);
  return (char*)&(dummy_rack.operator[](0))-(char*)&dummy_rack;
}



void Instance_O::archiveBase(ArchiveP node) {
  if (node->saving()) {
    if (this->_isgf || this->isCallable()) {
      SIMPLE_ERROR(BF("You cannot archive FUNCALLABLE instances or generic-functions"));
    }
    SYMBOL_EXPORT_SC_(KeywordPkg, iclass);
    //	    Symbol_sp className = this->_Class->name();
    //	    node->attribute(kw::_sym_iclass,className);
    for (int i(1); i < this->_Rack->length(); ++i) {
      node->pushVector((*this->_Rack)[i]);
    }
  } else {
    this->_isgf = false;
    this->_entryPoint = NULL;
#if 1
    Symbol_sp className = node->getKind();
    //	    node->attribute(kw::_sym_iclass,className);
    Class_sp cl = gc::As<Class_sp>(eval::funcall(cl::_sym_findClass, className, _lisp->_true()));
    this->_Class = cl;
    this->initializeSlots(cl->get_instance_stamp(),node->vectorSize());
#endif
    size_t idx(1);
    if (node->vectorSize() != this->numberOfSlots()) {
      SIMPLE_ERROR(BF("While loading archive class %s mismatch in number of slots - expected %d - loaded %d slots") % _rep_(this->_Class) % this->numberOfSlots() % node->vectorSize());
    }
    node->mapVector([this, &idx](T_sp value) {
        (*this->_Rack)[idx++] = value;
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



SYMBOL_EXPORT_SC_(ClosPkg, setFuncallableInstanceFunction);
SYMBOL_EXPORT_SC_(CorePkg, instanceClassSet);



T_sp Instance_O::instanceClassSet(Class_sp mc) {
  this->_Class = mc;
  return (this->sharedThis<Instance_O>());
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(this->_Rack);
#endif
}

T_sp Instance_O::instanceRef(size_t idx) const {
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(this->_Rack);
#endif
#if DEBUG_CLOS >= 2
  printf("\nMLOG INSTANCE-REF[%d] of Instance %p --->%s\n", idx, (void *)(this), this->_Rack[idx+1]->__repr__().c_str());
#endif
  return ((*this->_Rack)[idx+1]);
}
T_sp Instance_O::instanceSet(size_t idx, T_sp val) {
#if DEBUG_CLOS >= 2
  printf("\nMLOG SI-INSTANCE-SET[%d] of Instance %p to val: %s\n", idx, (void *)(this), val->__repr__().c_str());
#endif
  (*this->_Rack)[idx+1] = val;
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(this->_Rack);
#endif
  return val;
}

string Instance_O::__repr__() const {
  stringstream ss;
  ss << "#S(";
  if (Class_sp mc = this->_Class.asOrNull<Class_O>()) {
    ss << mc->classNameAsString() << " ";
  } else {
    ss << "<ADD SUPPORT FOR INSTANCE _CLASS=" << _rep_(this->_Class) << " >";
  }
  if (this->isgf()) {
      ss << _rep_(this->GFUN_NAME());
  }
  {
    ss << " #slots[" << this->numberOfSlots() << "]";
#if 0
    for (size_t i(1); i < this->numberOfSlots(); ++i) {
      T_sp obj = this->_Rack[i];
      ss << "        :slot" << i << " ";
      if (obj) {
        stringstream sslot;
        if ((obj).consp()) {
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
  ss << ")" ;
  return ss.str();
}

T_sp Instance_O::copyInstance() const {
  Instance_sp iobj = gc::As<Instance_sp>(core__allocateInstance(this->_Class,1));
  iobj->_isgf = this->_isgf;
  iobj->_Rack = this->_Rack;
  iobj->_entryPoint = this->_entryPoint;
  iobj->_Sig = this->_Sig;
  return iobj;
}

void Instance_O::reshapeInstance(int delta) {
  size_t copySize = this->_Rack->length();
  if (delta<0) copySize += delta;
  SimpleVector_sp newRack = SimpleVector_O::make(this->_Rack->length()+delta,_Unbound<T_O>(),true,copySize,&(*this->_Rack)[0]);
  this->_Rack = newRack;
}
/*
  memcpy(aux->instance.slots, x->instance.slots,
  (delta < 0 ? aux->instance.length : x->instance.length) *
  sizeof(cl_object));
  x->instance = aux->instance;
*/

SYMBOL_SC_(ClosPkg, standardOptimizedReaderMethod);
SYMBOL_SC_(ClosPkg, standardOptimizedWriterMethod);

void Instance_O::ensureClosure(DispatchFunction_fptr_type entryPoint) {
  this->_entryPoint = entryPoint;
};

T_sp Instance_O::setFuncallableInstanceFunction(T_sp functionOrT) {
  if (this->_isgf == CLASP_USER_DISPATCH) {
    this->reshapeInstance(-1);
    this->_isgf = CLASP_NOT_FUNCALLABLE;
  }
  SYMBOL_EXPORT_SC_(ClPkg, standardGenericFunction);
  SYMBOL_SC_(ClosPkg, standardOptimizedReaderFunction);
  SYMBOL_SC_(ClosPkg, standardOptimizedWriterFunction);
  SYMBOL_SC_(ClosPkg, invalidated_dispatch_function );
  if (functionOrT == _lisp->_true()) {
    this->_isgf = CLASP_STANDARD_DISPATCH;
    Instance_O::ensureClosure(&generic_function_dispatch);
  } else if (functionOrT == cl::_sym_standardGenericFunction) {
    this->_isgf = CLASP_RESTRICTED_DISPATCH;
    Instance_O::ensureClosure(&generic_function_dispatch);
  } else if (functionOrT == clos::_sym_invalidated_dispatch_function) {
    this->_isgf = CLASP_INVALIDATED_DISPATCH;
    Instance_O::ensureClosure(&invalidated_dispatch);
  } else if (functionOrT.nilp()) {
    this->_isgf = CLASP_NOT_FUNCALLABLE;
    Instance_O::ensureClosure(&not_funcallable_dispatch);
  } else if (functionOrT == clos::_sym_standardOptimizedReaderMethod) {
    /* WARNING: We assume that f(a,...) behaves as f(a,b) */
    this->_isgf = CLASP_READER_DISPATCH;
    // TODO: Switch to using slotReaderDispatch like ECL for improved performace
    //	    this->_Entry = &slotReaderDispatch;
    //Instance_O::ensureClosure(&generic_function_dispatch);
    Instance_O::ensureClosure(&optimized_slot_reader_dispatch);
  } else if (functionOrT == clos::_sym_standardOptimizedWriterMethod) {
    /* WARNING: We assume that f(a,...) behaves as f(a,b) */
    this->_isgf = CLASP_WRITER_DISPATCH;
    Instance_O::ensureClosure(&optimized_slot_writer_dispatch);
  } else if (gc::IsA<CompiledDispatchFunction_sp>(functionOrT)) {
    this->_isgf = CLASP_STRANDH_DISPATCH;
    this->GFUN_DISPATCHER_set(functionOrT);
    Instance_O::ensureClosure(gc::As_unsafe<CompiledDispatchFunction_sp>(functionOrT)->entryPoint());
  } else if (!cl__functionp(functionOrT)) {
    TYPE_ERROR(functionOrT, cl::_sym_function);
    //SIMPLE_ERROR(BF("Wrong type argument: %s") % functionOrT->__repr__());
  } else {
    this->reshapeInstance(+1);
    (*this->_Rack)[this->_Rack->length() - 1] = functionOrT;
    this->_isgf = CLASP_USER_DISPATCH;
    Instance_O::ensureClosure(&user_function_dispatch);
  }
  return ((this->sharedThis<Instance_O>()));
}

T_sp Instance_O::userFuncallableInstanceFunction() const
{
  if (this->_isgf == CLASP_USER_DISPATCH) {
    T_sp user_dispatch_fn = (*this->_Rack)[this->_Rack->length()-1];
    return user_dispatch_fn;
  }
  // Otherwise return NIL
  return _Nil<T_O>();
}

bool Instance_O::genericFunctionP() const {
  return (this->_isgf);
}

bool Instance_O::equalp(T_sp obj) const {
  if (!obj.generalp()) return false;
  if (this == obj.unsafe_general()) return true;
  if (Instance_sp iobj = obj.asOrNull<Instance_O>()) {
    if (this->_Class != iobj->_Class) return false;
    if (this->stamp() != iobj->stamp()) return false;
    for (size_t i(1), iEnd(this->_Rack->length()); i < iEnd; ++i) {
      if (!cl__equalp((*this->_Rack)[i], (*iobj->_Rack)[i])) return false;
    }
    return true;
  }
  return false;
}

void Instance_O::sxhash_(HashGenerator &hg) const {
  if (hg.isFilling())
    hg.hashObject(this->_Class);
  for (size_t i(1), iEnd(this->_Rack->length()); i < iEnd; ++i) {
    if (!(*this->_Rack)[i].unboundp() && hg.isFilling())
      hg.hashObject((*this->_Rack)[i]);
    else
      break;
  }
}

void Instance_O::LISP_INVOKE() {
  IMPLEMENT_ME();
#if 0
  ASSERT(this->_Entry!=NULL);
  LispCompiledFunctionIHF _frame(my_thread->invocationHistoryStack(),this->asSmartPtr());
  return(( (this->_Entry)(*this,nargs,args)));
#endif
}

void Instance_O::describe(T_sp stream) {
  stringstream ss;
  ss << (BF("Instance\n")).str();
  ss << (BF("isgf %d\n") % this->_isgf).str();
  ss << (BF("_Class: %s\n") % _rep_(this->_Class).c_str()).str();
  for (int i(1); i < this->_Rack->length(); ++i) {
    ss << (BF("_Rack[%d]: %s\n") % i % _rep_((*this->_Rack)[i]).c_str()).str();
  }
  clasp_write_string(ss.str(), stream);
}


CL_DEFUN bool core__call_history_entry_key_contains_specializer(SimpleVector_sp key, T_sp specializer) {
  if (specializer.consp()) {
    Cons_sp eql_spec(gc::As_unsafe<Cons_sp>(specializer));
    // Check and remove eql specializer
    for ( size_t i(0); i<key->length(); ++i ) {
      if (!(*key)[i].consp()) continue;
      if (cl__eql((*key)[i],oCadr(eql_spec))) return true;
    }
  } else {
    // Check and remove class specializer
    for ( size_t i(0); i<key->length(); ++i ) {
      if ((*key)[i].consp()) continue;
      if ((*key)[i] == specializer) return true;
    }
  }
  return false;
}
  

CL_DEFUN bool core__specializer_key_match(SimpleVector_sp x, SimpleVector_sp entry_key) {
  if (x->length() != entry_key->length()) return false;
#ifdef DEBUG_GFDISPATCH
  if (_sym_STARdebug_dispatchSTAR->symbolValue().notnilp()) {
    printf("%s:%d   specializer_key_match    key: %s\n", __FILE__, __LINE__, _rep_(x).c_str());
    printf("%s:%d                      entry_key: %s\n", __FILE__, __LINE__, _rep_(entry_key).c_str());
  }
#endif
  for ( size_t i(0); i<x->length(); ++i ) {
    // If eql specializer then match the specializer value
#ifdef DEBUG_GFDISPATCH
  if (_sym_STARdebug_dispatchSTAR->symbolValue().notnilp()) {
    printf("%s:%d   arg index %lu\n", __FILE__, __LINE__, i);
  }
#endif
    if ((*x)[i].consp()) {
      if (!(*entry_key)[i].consp()) goto NOMATCH;
      T_sp eql_spec_x = oCar((*x)[i]);
      T_sp eql_spec_y = oCar((*entry_key)[i]);
#ifdef DEBUG_GFDISPATCH
  if (_sym_STARdebug_dispatchSTAR->symbolValue().notnilp()) {
    printf("%s:%d   eql_spec_x -> %s  eql_spec_y -> %s\n", __FILE__, __LINE__, _rep_(eql_spec_x).c_str(),_rep_(eql_spec_y).c_str());
  }
#endif
      if (!cl__eql(eql_spec_x,eql_spec_y)) goto NOMATCH; //gc::As_unsafe<Cons_sp>(eql_spec_y)->memberEql(eql_spec_x)) goto NOMATCH;
    } else {
      if ((*x)[i] != (*entry_key)[i]) goto NOMATCH;
    }
  }
#ifdef DEBUG_GFDISPATCH
  if (_sym_STARdebug_dispatchSTAR->symbolValue().notnilp()) {
    printf("%s:%d       MATCHED!!!!\n",__FILE__,__LINE__);
  }
#endif
  return true;
 NOMATCH:
#ifdef DEBUG_GFDISPATCH
  if (_sym_STARdebug_dispatchSTAR->symbolValue().notnilp()) {
    printf("%s:%d       no match\n",__FILE__,__LINE__);
  }
#endif
  return false;
}




CL_DEFUN List_sp core__call_history_find_key(List_sp generic_function_call_history, SimpleVector_sp key) {
#ifdef DEBUG_GFDISPATCH
  if (_sym_STARdebug_dispatchSTAR->symbolValue().notnilp()) {
    printf("%s:%d   call_history_find_key    key: %s\n", __FILE__, __LINE__, _rep_(key).c_str());
  }
#endif
  for ( auto cur : generic_function_call_history ) {
    ASSERT(oCar(cur).consp());
    Cons_sp entry = gc::As_unsafe<Cons_sp>(oCar(cur));
    ASSERT(gc::IsA<SimpleVector_sp>(oCar(entry)));
    SimpleVector_sp entry_key = gc::As_unsafe<SimpleVector_sp>(oCar(entry));
    if (core__specializer_key_match(key,entry_key)) return cur;
  }
  return _Nil<T_O>();
}
           
    
    
/*! Return true if an entry was pushed */
CL_DEFUN bool core__generic_function_call_history_push_new(Instance_sp generic_function, SimpleVector_sp key, T_sp effective_method )
{
#ifdef DEBUG_GFDISPATCH
  if (_sym_STARdebug_dispatchSTAR->symbolValue().notnilp()) {
    printf("%s:%d   generic_function_call_history_push_new    gf: %s\n        key: %s\n          em: %s\n", __FILE__, __LINE__, _rep_(generic_function).c_str(), _rep_(key).c_str(), _rep_(effective_method).c_str());
  }
#endif
  List_sp call_history(generic_function->GFUN_CALL_HISTORY());
  if (call_history.nilp()) {
    generic_function->GFUN_CALL_HISTORY_set(Cons_O::createList(Cons_O::create(key,effective_method)));
    return true;
  }
  // Search for existing entry
  List_sp found = core__call_history_find_key(call_history,key);
  if (found.nilp()) {
    generic_function->GFUN_CALL_HISTORY_set(Cons_O::create(Cons_O::create(key,effective_method),generic_function->GFUN_CALL_HISTORY()));
    return true;
  }
  return false;
}


CL_DEFUN void core__generic_function_call_history_remove_entries_with_specializers(Instance_sp generic_function, List_sp specializers ) {
//  printf("%s:%d Remember to remove entries with subclasses of specializer: %s\n", __FILE__, __LINE__, _rep_(specializer).c_str());
#ifdef DEBUG_GFDISPATCH
  if (_sym_STARdebug_dispatchSTAR->symbolValue().notnilp()) {
    printf("%s:%d   generic-function_call_history_remove_entries_with_specializers   gf: %s\n        specializers: %s\n", __FILE__, __LINE__, _rep_(generic_function).c_str(), _rep_(specializers).c_str());
  }
#endif
  List_sp call_history(generic_function->GFUN_CALL_HISTORY());
  if (call_history.notnilp()) {
    for ( auto cur_specializer : specializers ) {
      List_sp edited(_Nil<T_O>());
      T_sp one_specializer = oCar(cur_specializer);
      for ( List_sp cur = call_history; cur.consp(); ) {
        ASSERT(oCar(cur).consp());
        Cons_sp entry = gc::As_unsafe<Cons_sp>(oCar(cur));
        ASSERT(gc::IsA<SimpleVector_sp>(oCar(entry)));
        SimpleVector_sp entry_key = gc::As_unsafe<SimpleVector_sp>(oCar(entry));
#ifdef DEBUG_GFDISPATCH
        if (_sym_STARdebug_dispatchSTAR->symbolValue().notnilp()) {
          printf("%s:%d         check if entry_key: %s   contains specializer: %s\n", __FILE__, __LINE__, _rep_(entry_key).c_str(), _rep_(one_specializer).c_str());
        }
#endif
        if (core__call_history_entry_key_contains_specializer(entry_key,one_specializer)) {
#ifdef DEBUG_GFDISPATCH
        if (_sym_STARdebug_dispatchSTAR->symbolValue().notnilp()) {
          printf("%s:%d       IT DOES!!!\n", __FILE__, __LINE__ );
        }
#endif
          cur = oCdr(cur);
        } else {
#ifdef DEBUG_GFDISPATCH
        if (_sym_STARdebug_dispatchSTAR->symbolValue().notnilp()) {
          printf("%s:%d       it does not - keeping entry!!!\n", __FILE__, __LINE__ );
        }
#endif
          Cons_sp save = gc::As_unsafe<Cons_sp>(cur);
          cur = oCdr(cur);
          save->rplacd(edited);
          edited = save;
        }
      }
      call_history = edited;
    }
    generic_function->GFUN_CALL_HISTORY_set(call_history);
  }
}
 



};

