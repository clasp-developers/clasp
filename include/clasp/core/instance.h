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
// may need more later
#include <clasp/gctools/gc_interface.h>

/*! Different values for Instance_O.isgf */
#define CLASP_NOT_FUNCALLABLE 0
#define CLASP_STANDARD_DISPATCH 1
#define CLASP_RESTRICTED_DISPATCH 2
#define CLASP_READER_DISPATCH 3
#define CLASP_WRITER_DISPATCH 4
#define CLASP_USER_DISPATCH 5
#define CLASP_STRANDH_DISPATCH 6
#define CLASP_INVALIDATED_DISPATCH 7

namespace core {

  class Instance_O : public Function_O {
    LISP_CLASS(core, CorePkg, Instance_O, "Instance",Function_O);
    friend class Class_O;
    void archiveBase(ArchiveP node);
  public: // ctor/dtor for classes with shared virtual base
  Instance_O() : Base(entry_point), _isgf(CLASP_NOT_FUNCALLABLE), _entryPoint(NULL), _Class(_Nil<Class_O>()), _Sig(_Nil<T_O>()){};
    virtual ~Instance_O(){};
  public:
    Class_sp _Class;
  public: // generic function specific instance variables here
    int _isgf;
    DispatchFunction_fptr_type _entryPoint;
  public: // Slots
    SimpleVector_sp _Rack;
  /*! Mimicking ECL instance->sig generation signature
        This is pointed to the class slots in case they change 
        - then the instances can be updated*/
    T_sp _Sig;
  public:
    bool isCallable() const { return (bool)(this->_entryPoint); };
  public: // These indices MUST match the order in +standard-generic-function-slots+
    T_sp GFUN_NAME() const { return this->instanceRef(0); };
    T_sp GFUN_SPECIALIZERS() const { return this->instanceRef(1); };
    T_sp GFUN_COMB() const { return this->instanceRef(2); };
    T_sp GFUN_DISPATCHER() const { return this->instanceRef(3);};
    void GFUN_DISPATCHER_set(T_sp f)  { this->instanceSet(3,f);};
    T_sp GFUN_CALL_HISTORY() const { return this->instanceRef(4);};
    void GFUN_CALL_HISTORY_set(T_sp h);
    T_sp GFUN_LAMBDA_LIST() const { return this->instanceRef(5);};
    void GFUN_LAMBDA_LIST_set(T_sp lambda_list) {
      if (this->instanceRef(5).unboundp() && lambda_list.nilp()) {
        printf("%s:%d Ignoring GFUN_LAMBDA_LIST_SET - returning\n", __FILE__, __LINE__ );
        return;
      }
      this->instanceSet(5,lambda_list);
    };
  public:
  // Add support for Function_O methods
    T_sp name() const { ASSERT(this->isgf()); return this->GFUN_NAME(); };
    virtual Symbol_sp functionKind() const { IMPLEMENT_ME(); };
    virtual T_sp closedEnvironment() const { IMPLEMENT_ME(); };
    virtual T_sp setSourcePosInfo(T_sp sourceFile, size_t filePos, int lineno, int column) { IMPLEMENT_ME(); };
//  virtual T_mv functionSourcePos() const { IMPLEMENT_ME();;
    virtual T_sp cleavir_ast() const { return _Nil<T_O>(); };
    virtual void setf_cleavir_ast(T_sp ast) { SIMPLE_ERROR(BF("Generic functions cannot be inlined"));};
    virtual List_sp declares() const { IMPLEMENT_ME(); };
    virtual T_sp docstring() const { IMPLEMENT_ME(); };
    virtual void *functionAddress() const { IMPLEMENT_ME(); };
    virtual bool macroP() const { return false; };
    virtual void set_kind(Symbol_sp k);
    virtual Symbol_sp getKind() const { return kw::_sym_function; };
    virtual int sourceFileInfoHandle() const { IMPLEMENT_ME(); };
    virtual size_t filePos() const { return 0; }
    virtual int lineNumber() const { return 0; }
    virtual int column() const { return 0; };
    virtual LambdaListHandler_sp lambdaListHandler() const { IMPLEMENT_ME(); };
    virtual void setAssociatedFunctions(List_sp funcs) { NOT_APPLICABLE(); };
  public: // The hard-coded indexes above are defined below to be used by Class
    void initializeSlots(Fixnum stamp, size_t numberOfSlots);
    void ensureClosure(DispatchFunction_fptr_type entryPoint);
    virtual void setf_lambda_list(List_sp lambda_list) { if (!this->_isgf) {SIMPLE_ERROR(BF("Cannot set lambda list of non gf function ll->%s") % _rep_(lambda_list));} this->GFUN_LAMBDA_LIST_set(lambda_list); }; //{ this->_lambda_list = lambda_list; };
    virtual T_sp lambda_list() const { return this->GFUN_LAMBDA_LIST(); };
  public:
    static size_t rack_stamp_offset();
  private:
    void reshapeInstance(int delta);
  public:
    virtual void LISP_INVOKE();

  public: // Functions here
    Fixnum stamp() const { return (*this->_Rack)[0].unsafe_fixnum();};
    void stamp_set(Fixnum s) { (*this->_Rack)[0] = clasp_make_fixnum(s); };
    size_t numberOfSlots() const { return this->_Rack->length()-1; };
  /*! Return number of slots if not nil otherwise nil */
    T_sp oinstancepSTAR() const;
  /*! Return number of slots if not nil otherwise nil */
    T_sp oinstancep() const;

    CL_DEFMETHOD int isgf() const { return this->_isgf; };

    Class_sp _instanceClass() const { return this->_Class; };

    T_sp instanceClassSet(Class_sp mc);

    virtual T_sp instanceSigSet();
    virtual T_sp instanceSig() const;


    virtual bool equalp(T_sp obj) const;
    virtual void sxhash_(HashGenerator &hg) const;

  /*! Return the value of a slot */
    T_sp instanceRef(size_t idx) const;
  /*! Set the value of a slot and return the new value */
    T_sp instanceSet(size_t idx, T_sp val);

    string __repr__() const;

    T_sp copyInstance() const;

    T_sp setFuncallableInstanceFunction(T_sp functionOrT);

    T_sp userFuncallableInstanceFunction() const;

    bool genericFunctionP() const;

    void describe(T_sp stream);

    void __write__(T_sp sout) const; // Look in write_ugly.cc

    static  LCC_RETURN LISP_CALLING_CONVENTION();

  }; // Instance class

}; // core namespace
template <>
struct gctools::GCInfo<core::Instance_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};


namespace gctools {
 /*! Specialize TaggedCast for Instance_O - always use dynamic_cast */
 template <typename FROM>
struct TaggedCast<core::Instance_O *, FROM> {
   typedef core::Instance_O *ToType;
   typedef FROM FromType;
   inline static bool isA(FromType ptr) {
     if (tagged_generalp(ptr)) {
      // Maybe
       core::General_O* raw_client = (core::General_O*)untag_general<FromType>(ptr);
       core::Instance_O* iptr = dynamic_cast<core::Instance_O*>(raw_client);
       return iptr!=NULL;
     }
     return false;
   }
   inline static core::Instance_O* castOrNULL(FromType client) {
     if ( tagged_generalp(client) ) {
      // maybe
       core::General_O* raw_client = (core::General_O*)untag_general<FromType>(client);
       core::Instance_O* iclient = dynamic_cast<core::Instance_O*>(raw_client);
       if ( iclient ) return tag_general<ToType>(iclient);
       return NULL;
     }
     return NULL;
   }
 };
};


namespace core {

  List_sp core__call_history_find_key(List_sp generic_function_call_history, SimpleVector_sp key);

  bool core__generic_function_call_history_push_new(Instance_sp generic_function, SimpleVector_sp key, T_sp effective_method);

  void core__generic_function_call_history_remove_entries_with_specializer(Instance_sp generic_function, T_sp specializer);

  T_sp core__allocateInstance(T_sp theClass, size_t numberOfSlots);
};


#endif /* _core_instance_H_ */
