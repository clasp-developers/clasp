/*
    File: instance.h
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
#ifndef _core_instance_H_
#define _core_instance_H_

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/instance.fwd.h>

#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__0_
#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__1_
#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__2_
#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__3_
#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__4_
#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__5_
#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__6_
#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__7_
#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__8_
#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__9_
#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__10_
#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__11_
// may need more later
#include <clasp/gctools/gc_interface.h>
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__0_
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__1_
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__2_
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__3_
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__4_
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__5_
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__6_
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__7_
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__8_
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__9_
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__10_
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__11_

/*! Different values for Instance_O.isgf */
#define ECL_NOT_FUNCALLABLE 0
#define ECL_STANDARD_DISPATCH 1
#define ECL_RESTRICTED_DISPATCH 2
#define ECL_READER_DISPATCH 3
#define ECL_WRITER_DISPATCH 4
#define ECL_USER_DISPATCH 5

namespace core {

#if 0 // moved to foundation.h
/*! Shouldn't this derive from a Functoid - it doesn't need a closedEnvironment */
class InstanceClosure : public FunctionClosure {
public:
  GenericFunctionPtr entryPoint;
  Instance_sp instance;

public:
  DISABLE_NEW();
  InstanceClosure(T_sp name, GenericFunctionPtr ep, Instance_sp inst)
      : FunctionClosure(name), entryPoint(ep), instance(inst){};
  virtual size_t templatedSizeof() const { return sizeof(*this); };
  virtual const char *describe() const { return "InstanceClosure"; };
  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION();
  LambdaListHandler_sp lambdaListHandler() const { return _Nil<LambdaListHandler_O>(); };
  T_sp lambdaList() const;
};
#endif

class Instance_O : public Function_O {
  LISP_BASE1(Function_O);
  LISP_CLASS(core, CorePkg, Instance_O, "Instance");
  friend class Class_O;
  void archiveBase(ArchiveP node);

public: // ctor/dtor for classes with shared virtual base
  explicit Instance_O() : Function_O(), _isgf(ECL_NOT_FUNCALLABLE), _Class(_Nil<Class_O>()), _Sig(_Nil<T_O>()){};
  virtual ~Instance_O(){};
GCPROTECTED: // instance variables here
  int _isgf;
  Class_sp _Class;
  gctools::Vec0<T_sp> _Slots;
  /*! Mimicking ECL instance->sig generation signature
        Jul 2014 - I think it is pointed to the class slots in case they change - then the instances can be updated*/
  T_sp _Sig;

public:
  bool isCallable() const { return (bool)(this->closure); };
#if 0
    public: // Functions that mimic ECL_XXXX_XXXX macros (eg: ECL_CLASS_SLOTS(x))
	// Instances that represent CL classes have slots that are hard-coded to represent
	// class instance variables
	const Class_sp& _CLASS_OF() const	{ return this->_Class;};
	T_sp& _SPECIALIZER_FLAG()  	{ return this->_Slots[0];};
	T_sp& _SPECIALIZER_OBJECT() 	{ return this->_Slots[3];};
	T_sp& _CLASS_NAME()   		{ return this->_Slots[3+0];}; // 3
	T_sp& _CLASS_SUPERIORS() 	{ return this->_Slots[3+1];}; // 4
	T_sp& _CLASS_INFERIORS() 	{ return this->_Slots[3+2];}; // 5 
	T_sp& _CLASS_SLOTS() 	{ return this->_Slots[3+3];}; // 6
	T_sp& _CLASS_CPL() 	{ return this->_Slots[3+4];};

#endif
public: // Generic function ECL macros are replicated here
  T_sp GFUN_NAME() const { return this->_Slots[0]; };
  T_sp GFUN_SPECIALIZERS() const { return this->_Slots[1]; };
  T_sp GFUN_COMB() const { return this->_Slots[2]; };

public: // The hard-coded indexes above are defined below to be used by Class
protected:
  void initializeSlots(int numberOfSlots);
  void ensureClosure(GenericFunctionPtr entryPoint);

public:
  static T_sp allocateInstance(T_sp _theClass, int numberOfSlots = 0);
  static T_sp allocateRawInstance(T_sp orig, T_sp _theClass, int numberOfSlots);

private:
  void reshapeInstance(int delta);

public:
  virtual void LISP_INVOKE();

public: // Functions here
  int numberOfSlots() const { return this->_Slots.size(); };
  /*! Return number of slots if not nil otherwise nil */
  T_sp oinstancepSTAR() const;
  /*! Return number of slots if not nil otherwise nil */
  T_sp oinstancep() const;

  int isgf() const { return this->_isgf; };

  Class_sp _instanceClass() const { return this->_Class; };

  T_sp instanceClassSet(Class_sp mc);

  virtual T_sp instanceSigSet();
  virtual T_sp instanceSig() const;

  bool macroP() const { return false; };
  Symbol_sp functionKind() const { return kw::_sym_function; };
  void setKind(Symbol_sp k);

  virtual bool equalp(T_sp obj) const;
  virtual void sxhash_(HashGenerator &hg) const;

  /*! Return the value of a slot */
  T_sp instanceRef(int idx) const; // { return this->_Slots[idx];};
  /*! Set the value of a slot and return the new value */
  T_sp instanceSet(int idx, T_sp val); // { this->_Slots[idx] = val; return val;};

  string __repr__() const;

  T_sp copyInstance() const;

  T_sp setFuncallableInstanceFunction(T_sp functionOrT);

  bool genericFunctionP() const;

  void describe(T_sp stream);

  void __write__(T_sp sout) const; // Look in write_ugly.cc

}; // Instance class

}; // core namespace
template <>
struct gctools::GCInfo<core::Instance_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};
TRANSLATE(core::Instance_O);

#endif /* _core_instance_H_ */
