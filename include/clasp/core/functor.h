#ifndef functor_h
#define functor_h

#include <clasp/core/object.h>
#include <clasp/core/symbol.h>

namespace kw {
  EXTERN_SYMBOL(dispatch_function);
};
namespace core {
  EXTERN_SYMBOL(arguments);
};
namespace cl {
  EXTERN_SYMBOL(generic_function);
};

namespace core {
  FORWARD(Function);
  FORWARD(NamedFunction);
  FORWARD(Closure);
  FORWARD(BuiltinClosure);
  FORWARD(FunctionClosure);
  FORWARD(Closure);
  FORWARD(ClosureWithSlots);
};

template <>
struct gctools::GCInfo<core::Function_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};
template <>
struct gctools::GCInfo<core::FunctionClosure_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};
template <>
struct gctools::GCInfo<core::BuiltinClosure_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

#ifdef DEBUG_FUNCTION_CALL_COUNTER
#define INCREMENT_FUNCTION_CALL_COUNTER(x) ++x->_TimesCalled
#else
#define INCREMENT_FUNCTION_CALL_COUNTER(x)
#endif
namespace core {

  /* The following MUST MATCH %function-description% in cmpintrinsics.lsp
   */ 
  struct FunctionDescription {
    void* functionPrototype;
    gctools::GCRootsInModule* gcrootsInModule;
    int*  sourceHandleP;
    intptr_t functionNameIndex;
    intptr_t lambdaListIndex;
    intptr_t docstringIndex;
    intptr_t linenoIndex;
    intptr_t columnIndex;
    intptr_t fileposIndex;
  };


  /*! Function_O is a Funcallable object that adds no fields to anything that inherits from it
*/
  class Function_O : public General_O {
    LISP_ABSTRACT_CLASS(core,ClPkg,Function_O,"FUNCTION",General_O);
  public:
    std::atomic<claspFunction>    entry;
#ifdef DEBUG_FUNCTION_CALL_COUNTER
#error "Create an API to keep track of function calls using virtual functions"
//    size_t _TimesCalled;  Do not use tho
#endif
  public:
    virtual const char *describe() const { return "Function - subclass must implement describe()"; };
    virtual size_t templatedSizeof() const { return sizeof(*this); };
  public:
    Function_O(claspFunction ptr)
      : entry(ptr)
#ifdef DEBUG_FUNCTION_CALL_COUNTER
      , _TimesCalled(0)
#endif
    {
#ifdef _DEBUG_BUILD
      if (((uintptr_t)ptr)&0x7 || !ptr) {
        printf("%s:%d Something other than a function pointer was passed to initialize Function_O::entry -> %p\n", __FILE__, __LINE__, ptr );
        abort();
      }
#endif
    };
    Pointer_sp function_pointer() const;
    virtual bool compiledP() const { return false; };
    virtual bool interpretedP() const { return false; };
    virtual bool builtinP() const { return false; };
    virtual T_sp sourcePosInfo() const { return _Nil<T_O>(); };
    CL_DEFMETHOD virtual T_sp functionName() const = 0;
    CL_DEFMETHOD Symbol_sp functionKind() const { return this->getKind(); };
    CL_DEFMETHOD List_sp function_declares() const { return this->declares(); };
    CL_DEFMETHOD T_sp functionLambdaListHandler() const {
      return this->lambdaListHandler();
    }
    CL_DEFMETHOD virtual void setf_lambda_list(List_sp lambda_list) = 0;
    virtual T_sp closedEnvironment() const = 0;
    virtual T_sp setSourcePosInfo(T_sp sourceFile, size_t filePos, int lineno, int column) = 0;
    virtual T_mv functionSourcePos() const;
    virtual List_sp declares() const = 0;
    CL_DEFMETHOD virtual T_sp docstring() const = 0;
    CL_DEFMETHOD virtual bool macroP() const = 0;
    virtual void set_kind(Symbol_sp k) = 0;
    virtual Symbol_sp getKind() const = 0;
    virtual int sourceFileInfoHandle() const = 0;
    virtual size_t filePos() const { return 0; }
    virtual int lineNumber() const { return 0; }
    virtual int column() const { return 0; };
    virtual LambdaListHandler_sp lambdaListHandler() const = 0;
    virtual T_sp lambda_list() const = 0;
    virtual string __repr__() const;
    CL_DEFMETHOD virtual T_mv function_description() const {SUBIMP();};
    CL_DEFMETHOD virtual T_sp function_literal_vector_copy() const {SUBIMP();};
    virtual ~Function_O() {};
  };
};

template <>
struct gctools::GCInfo<core::NamedFunction_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
  extern bool cl__stringp(T_sp obj);
  extern void lisp_error_sprintf(const char* file, int line, const char* fmt, ...);
  
SMART(LambdaListHandler);
SMART(NamedFunction);
/*! NamedFunction_O inherits from Function_O and
 *  adds a function name field to anything that inherits from it.
 */
 class NamedFunction_O : public Function_O {
  LISP_ABSTRACT_CLASS(core, CorePkg, NamedFunction_O, "NamedFunction",Function_O);
public:
  T_sp _name;
 NamedFunction_O(claspFunction fptr,T_sp name) : Function_O(fptr), _name(name) {
    if (cl__stringp(name)) {
      printf("%s:%d   function name is a string: %s\n", __FILE__, __LINE__, _rep_(name).c_str());
      lisp_error_sprintf(__FILE__, __LINE__, "Function name is string %s", _rep_(name).c_str());
    }
  };
  virtual ~NamedFunction_O(){};
public:
  CL_LISPIFY_NAME("core:functionName");
  CL_DEFMETHOD T_sp functionName() const {
    return this->_name;
  }
  // defined in write_ugly.cc
  virtual void __write__(T_sp) const;

};
};

template <>
struct gctools::GCInfo<core::Closure_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
  /*! Closure_O
   *  Can have an environment associated with it 
   */
class Closure_O : public NamedFunction_O {
    LISP_CLASS(core,CorePkg,Closure_O,"Closure",NamedFunction_O);
public:
 Closure_O(claspFunction fptr, T_sp name ) : Base(fptr,name) {};
public:
  virtual const char *describe() const { return "Closure"; };
};
};


namespace core {
  /*! This is the class that stores source info */
  class FunctionClosure_O : public Closure_O {
    LISP_CLASS(core,CorePkg,FunctionClosure_O,"FunctionClosure",Closure_O);
  public:
  //  T_sp _SourcePosInfo;
    Symbol_sp kind;
    Fixnum _sourceFileInfoHandle;
    Fixnum _filePos;
    Fixnum _lineno;
    Fixnum _column;
  public:
#define SOURCE_INFO core::Fixnum sourceFileInfoHandle, core::Fixnum filePos, core::Fixnum lineno, core::Fixnum column
#define SOURCE_INFO_PASS sourceFileInfoHandle, filePos, lineno, column
  FunctionClosure_O(claspFunction fptr, T_sp name, Symbol_sp k)
    : Closure_O(fptr,name), kind(k) {};
  FunctionClosure_O(claspFunction fptr, T_sp name)
    : Closure_O(fptr,name), kind(kw::_sym_function) {};
    static FunctionClosure_sp create(fnLispCallingConvention fptr, T_sp name, T_sp function_kind) {
      FunctionClosure_sp fc = gctools::GC<FunctionClosure_O>::allocate(fptr,name,function_kind);
      return fc;
    }
    CL_DEFMETHOD List_sp source_info() const;
    CL_DEFMETHOD void set_source_info(List_sp source_info);
    virtual size_t templatedSizeof() const { return sizeof(*this); };
    virtual const char *describe() const { return "FunctionClosure"; };
    void set_kind(Symbol_sp k) { this->kind = k; };
    Symbol_sp getKind() const { return this->kind; };
    bool macroP() const;
    T_sp sourcePosInfo() const; // { return this->_SourcePosInfo; };
    virtual T_sp setSourcePosInfo(T_sp sourceFile, size_t filePos, int lineno, int column);
    virtual int sourceFileInfoHandle() const;
    virtual size_t filePos() const;
    virtual int lineNumber() const;
    virtual int column() const;
    virtual LambdaListHandler_sp lambdaListHandler() const {SUBIMP();};
    virtual T_sp lambda_list() const {SUBIMP();};
    virtual void setf_lambda_list(List_sp lambda_list) {SUBIMP();};
    virtual List_sp declares() const {NOT_APPLICABLE();};
    virtual T_sp docstring() const {NOT_APPLICABLE();};
    virtual T_sp closedEnvironment() const { return _Nil<T_O>();};
    virtual T_mv function_description() const;
    virtual T_sp function_literal_vector_copy() const;
  };

  class BuiltinClosure_O : public FunctionClosure_O {
    LISP_CLASS(core,CorePkg,BuiltinClosure_O,"BuiltinClosure",FunctionClosure_O);
  public:
    LambdaListHandler_sp _lambdaListHandler;
    List_sp _declares;
    T_sp _docstring;
  public:
  BuiltinClosure_O(claspFunction fptr, T_sp name, Symbol_sp k)
    : FunctionClosure_O(fptr, name, k){};
  BuiltinClosure_O(claspFunction fptr, T_sp name )
    : FunctionClosure_O(fptr,name) {}
    void finishSetup(LambdaListHandler_sp llh, Symbol_sp k) {
      this->_lambdaListHandler = llh;
      this->kind = k;
    }
    T_sp closedEnvironment() const { return _Nil<T_O>(); };
    virtual T_sp lambda_list() const;
    virtual void setf_lambda_list(List_sp lambda_list);
    virtual size_t templatedSizeof() const { return sizeof(*this); };
    virtual const char *describe() const { return "BuiltinClosure"; };
    bool builtinP() const { return true; };
    LambdaListHandler_sp lambdaListHandler() const { return this->_lambdaListHandler; };
    T_sp docstring() const { return this->_docstring; };
    List_sp declares() const { return this->_declares; };
    virtual T_mv function_description() const;
  };

}

namespace core {
  extern LCC_RETURN interpretedClosureEntryPoint(LCC_ARGS_FUNCALL_ELLIPSIS);
   
  class ClosureWithSlots_O final : public core::FunctionClosure_O {
    LISP_CLASS(core,CorePkg,ClosureWithSlots_O,"ClosureWithSlots",core::FunctionClosure_O);
    typedef enum { interpretedClosure, bclaspClosure, cclaspClosure } ClosureType;
#define ENVIRONMENT_SLOT 0
#define INTERPRETED_CLOSURE_SLOTS  3
#define INTERPRETED_CLOSURE_ENVIRONMENT_SLOT ENVIRONMENT_SLOT
#define INTERPRETED_CLOSURE_FORM_SLOT 1
#define INTERPRETED_CLOSURE_LAMBDA_LIST_HANDLER_SLOT 2
#define BCLASP_CLOSURE_SLOTS  1
#define BCLASP_CLOSURE_ENVIRONMENT_SLOT ENVIRONMENT_SLOT
  public:
    FunctionDescription* _FunctionDescription;
    core::T_sp _lambdaList;
    ClosureType   closureType;
  //! Slots must be the last field
    typedef core::T_sp value_type;
    gctools::GCArray_moveable<value_type> _Slots;
  public:
    virtual const char *describe() const { return "CompiledClosure"; };
    virtual size_t templatedSizeof() const {
      printf("%s:%d templatedSizeof called on closure %s - it is probably incorrect because we don't account for the slots\n", __FILE__, __LINE__, _rep_(this->asSmartPtr()).c_str());
      return sizeof(*this);
    };
  public:
    static ClosureWithSlots_sp make_interpreted_closure(T_sp name, T_sp type, T_sp lambda_list, LambdaListHandler_sp lambda_list_handler, T_sp declares, T_sp docstring, T_sp form, T_sp environment, SOURCE_INFO) {
      ClosureWithSlots_sp closure =
        gctools::GC<core::ClosureWithSlots_O>::allocate_container(INTERPRETED_CLOSURE_SLOTS,
                                                                  &interpretedClosureEntryPoint,
                                                                  (core::FunctionDescription*)NULL,
                                                                  name,
                                                                  type,
                                                                  lambda_list);
      closure->closureType = interpretedClosure;
      (*closure)[INTERPRETED_CLOSURE_FORM_SLOT] = form;
      (*closure)[INTERPRETED_CLOSURE_ENVIRONMENT_SLOT] = environment;
      if (lambda_list_handler.nilp()) {
        printf("%s:%d  A NIL lambda-list-handler was passed for %s lambdalist: %s\n", __FILE__, __LINE__, _rep_(name).c_str(), _rep_(lambda_list).c_str());
        abort();
      }
      (*closure)[INTERPRETED_CLOSURE_LAMBDA_LIST_HANDLER_SLOT] = lambda_list_handler;
      return closure;
    }
    static ClosureWithSlots_sp make_bclasp_closure(T_sp name, claspFunction ptr, T_sp type, T_sp lambda_list, T_sp environment) {
      ClosureWithSlots_sp closure = 
        gctools::GC<core::ClosureWithSlots_O>::allocate_container(BCLASP_CLOSURE_SLOTS,
                                                                  ptr,
                                                                  (core::FunctionDescription*)NULL,
                                                                  name,
                                                                  type,
                                                                  lambda_list);
      closure->closureType = bclaspClosure;
      (*closure)[BCLASP_CLOSURE_ENVIRONMENT_SLOT] = environment;
      return closure;
    }
    static ClosureWithSlots_sp make_cclasp_closure(T_sp name, claspFunction ptr, T_sp type, T_sp lambda_list, SOURCE_INFO) {
      ClosureWithSlots_sp closure = 
        gctools::GC<core::ClosureWithSlots_O>::allocate_container(0,
                                                                  ptr,
                                                                  (core::FunctionDescription*)NULL,
                                                                  name,
                                                                  type,
                                                                  lambda_list);
      closure->closureType = cclaspClosure;
      return closure;
    }
  public:
  ClosureWithSlots_O(size_t capacity,
                     claspFunction ptr,
                     FunctionDescription* functionDescription,
                     core::T_sp functionName,
                     core::Symbol_sp type,
                     core::T_sp ll)
    : Base(ptr,functionName, type),
      _FunctionDescription(functionDescription),
      _lambdaList(ll),
      closureType(cclaspClosure),
      _Slots(capacity,_Unbound<T_O>(),true) {};
    virtual string __repr__() const;
    core::T_sp lambda_list() const { return this->_lambdaList; };
    void setf_lambda_list(core::List_sp lambda_list) { this->_lambdaList = lambda_list; };
    core::LambdaListHandler_sp lambdaListHandler() const {
      switch (this->closureType) {
      case interpretedClosure:
          return (*this)[INTERPRETED_CLOSURE_LAMBDA_LIST_HANDLER_SLOT];
      case bclaspClosure:
          return _Nil<T_O>();
      case cclaspClosure:
          return _Nil<T_O>();
      };
    }
    CL_DEFMETHOD T_sp closedEnvironment() const {
      ASSERT(this->closureType!=cclaspClosure); // Never call on a cclaspClosure
      return (*this)[ENVIRONMENT_SLOT];
    };      
    CL_DEFMETHOD T_O*& closedEnvironment_rawRef() {
      ASSERT(this->closureType!=cclaspClosure); // Never call on a cclaspClosure
      return (*this)[ENVIRONMENT_SLOT].rawRef_();
    };      
    bool compiledP() const {
      return (this->closureType!=interpretedClosure);
    }
    bool interpretedP() const {
      return (this->closureType==interpretedClosure);
    }
      
    inline T_sp &operator[](size_t idx) {
      BOUNDS_ASSERT(idx<this->_Slots._Length);
      return this->_Slots[idx];
    };
    inline const T_sp &operator[](size_t idx) const {
      BOUNDS_ASSERT(idx<this->_Slots._Length);
      return this->_Slots[idx];
    };
    T_sp code() const;
    virtual T_mv function_description() const final;
    virtual T_sp function_literal_vector_copy() const final;
  };
};

namespace core {
  void core__closure_slots_dump(Closure_sp func);

};

namespace core {
#define LCC_FUNCALL
#include <clasp/core/lispCallingConvention.h>
#undef LCC_FUNCALL
};


#endif
