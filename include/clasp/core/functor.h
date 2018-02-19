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
  FORWARD(CompiledFunction);
  FORWARD(ClosureWithFrame);
  FORWARD(ClosureWithSlots);
  FORWARD(InterpretedClosure);
  FORWARD(CompiledClosure);
  FORWARD(CompiledDispatchFunction);
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
template <>
struct gctools::GCInfo<core::InstanceClosure_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};
template <>
struct gctools::GCInfo<core::InterpretedClosure_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

template <>
struct gctools::GCInfo<core::CompiledClosure_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};
template <>
struct gctools::GCInfo<core::CompiledDispatchFunction_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};


template <>
struct gctools::GCInfo<core::CompiledFunction_O> {
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
    virtual string nameAsString() const {SUBIMP();};
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
    CL_DEFMETHOD virtual T_sp cleavir_ast() const = 0;
    CL_DEFMETHOD virtual void setf_cleavir_ast(T_sp ast) = 0;
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
SMART(LambdaListHandler);
SMART(NamedFunction);
/*! NamedFunction_O inherits from Function_O and
 *  adds a function name field to anything that inherits from it.
 */
 class NamedFunction_O : public Function_O {
  LISP_ABSTRACT_CLASS(core, CorePkg, NamedFunction_O, "NamedFunction",Function_O);
public:
  T_sp _name;
 NamedFunction_O(claspFunction fptr,T_sp name) : Function_O(fptr), _name(name) {};
  virtual ~NamedFunction_O(){};
public:
  CL_LISPIFY_NAME("core:functionName");
  CL_DEFMETHOD T_sp functionName() const {
    return this->_name;
  }

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
  virtual string nameAsString() const;
};
};


namespace core {
  /*! This is the class that stores source info */
  class FunctionClosure_O : public Closure_O {
    LISP_CLASS(core,CorePkg,FunctionClosure_O,"FunctionClosure",Closure_O);
  public:
  //  T_sp _SourcePosInfo;
    T_sp _cleavir_ast;
    Symbol_sp kind;
    Fixnum _sourceFileInfoHandle;
    Fixnum _filePos;
    Fixnum _lineno;
    Fixnum _column;
  public:
#define SOURCE_INFO core::Fixnum sourceFileInfoHandle, core::Fixnum filePos, core::Fixnum lineno, core::Fixnum column
#define SOURCE_INFO_PASS sourceFileInfoHandle, filePos, lineno, column
  FunctionClosure_O(claspFunction fptr, T_sp name, Symbol_sp k, SOURCE_INFO)
    : Closure_O(fptr,name), kind(k), _cleavir_ast(_Nil<T_O>()), _sourceFileInfoHandle(sourceFileInfoHandle), _filePos(filePos), _lineno(lineno), _column(column){};
  FunctionClosure_O(claspFunction fptr, T_sp name)
    : Closure_O(fptr,name), kind(kw::_sym_function), _cleavir_ast(_Nil<T_O>()), _sourceFileInfoHandle(0), _filePos(0), _lineno(0), _column(0){};
    static FunctionClosure_sp create(fnLispCallingConvention fptr, T_sp name, T_sp function_kind, SOURCE_INFO ) {
      FunctionClosure_sp fc = gctools::GC<FunctionClosure_O>::allocate(fptr,name,function_kind,SOURCE_INFO_PASS);
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
    virtual T_sp cleavir_ast() const { return this->_cleavir_ast; };
    virtual void setf_cleavir_ast(T_sp ast) { this->_cleavir_ast = ast; };
    virtual LambdaListHandler_sp lambdaListHandler() const {SUBIMP();};
    virtual T_sp lambda_list() const {SUBIMP();};
    virtual void setf_lambda_list(List_sp lambda_list) {SUBIMP();};
    virtual List_sp declares() const {NOT_APPLICABLE();};
    virtual T_sp docstring() const {NOT_APPLICABLE();};
    virtual T_sp closedEnvironment() const { return _Nil<T_O>();};
  };

  class BuiltinClosure_O : public FunctionClosure_O {
    LISP_CLASS(core,CorePkg,BuiltinClosure_O,"BuiltinClosure",FunctionClosure_O);
  public:
    LambdaListHandler_sp _lambdaListHandler;
    List_sp _declares;
    T_sp _docstring;
  public:
  BuiltinClosure_O(claspFunction fptr, T_sp name, Symbol_sp k, SOURCE_INFO)
    : FunctionClosure_O(fptr, name, k, SOURCE_INFO_PASS){};
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
  };

}

namespace core {
    class TemplatedFunctionBase_O : public BuiltinClosure_O {
    LISP_CLASS(core,CorePkg,TemplatedFunctionBase_O,"TemplatedFunctionBase",BuiltinClosure_O);
    TemplatedFunctionBase_O(claspFunction fptr, T_sp name, Symbol_sp k, SOURCE_INFO)
    : BuiltinClosure_O(fptr, name, k, SOURCE_INFO_PASS){};
    TemplatedFunctionBase_O(claspFunction fptr, T_sp name )
    : BuiltinClosure_O(fptr,name) {}
    };
};

namespace core {
  class ClosureWithSlots_O final : public core::FunctionClosure_O {
    LISP_CLASS(core,CorePkg,ClosureWithSlots_O,"ClosureWithSlots",core::FunctionClosure_O);
  public:
    core::T_sp _lambdaList;
  //! Slots must be the last field
    typedef core::T_sp value_type;
    gctools::GCArray_moveable<value_type> _Slots;
  public:
    virtual const char *describe() const { return "CompiledClosure"; };
    virtual size_t templatedSizeof() const { return sizeof(*this); };
  public:
  ClosureWithSlots_O(size_t capacity,
                     claspFunction ptr,
                     core::T_sp functionName,
                     core::Symbol_sp type,
                     core::T_sp ll,
                     SOURCE_INFO)
    : Base(ptr,functionName, type, SOURCE_INFO_PASS),
      _lambdaList(ll), 
      _Slots(capacity,_Unbound<T_O>(),true) {};
    bool compiledP() const { return true; };
    core::T_sp lambda_list() const { return this->_lambdaList; };
    void setf_lambda_list(core::List_sp lambda_list) { this->_lambdaList = lambda_list; };
    core::LambdaListHandler_sp lambdaListHandler() const { return _Nil<core::LambdaListHandler_O>(); };
    inline T_sp &operator[](size_t idx) {
      BOUNDS_ASSERT(idx<this->_Slots._Length);
      return this->_Slots[idx];
    };
    inline const T_sp &operator[](size_t idx) const {
      BOUNDS_ASSERT(idx<this->_Slots._Length);
      return this->_Slots[idx];
    };
  };
};

// This class may be deprecated
namespace core {
class ClosureWithFrame_O : public FunctionClosure_O {
  LISP_CLASS(core,CorePkg,ClosureWithFrame_O,"ClosureWithFrame",FunctionClosure_O);
public:
  T_sp _closedEnvironment;
public:
  CL_DEFMETHOD T_sp closedEnvironment() const { ASSERT(this->_closedEnvironment.generalp()); return this->_closedEnvironment;};
 ClosureWithFrame_O(claspFunction fptr, T_sp fn, Symbol_sp k, T_sp env, SOURCE_INFO) : Base(fptr,fn,k,SOURCE_INFO_PASS), _closedEnvironment(env) {
    ASSERT(env.nilp() || env.asOrNull<Environment_O>() );
  };
  virtual size_t templatedSizeof() const { return sizeof(*this); };
  virtual const char *describe() const { return "ClosureWithFrame"; };
};

};


namespace core {
  /*! Keeps track of how many InterpretedClosure calls there are
*/
  extern uint64_t global_interpreted_closure_calls;

  LCC_RETURN interpretedClosureEntryPoint(LCC_ARGS_FUNCALL_ELLIPSIS);
  
class InterpretedClosure_O : public ClosureWithFrame_O {
    LISP_CLASS(core,CorePkg,InterpretedClosure_O,"InterpretedClosure",ClosureWithFrame_O);
public:
  LambdaListHandler_sp _lambdaListHandler;
  List_sp _declares;
  T_sp _docstring;
  List_sp _code;
public:
  InterpretedClosure_O(T_sp fn, Symbol_sp k, LambdaListHandler_sp llh, List_sp dec, T_sp doc, T_sp env, List_sp c, SOURCE_INFO);
  virtual size_t templatedSizeof() const { return sizeof(*this); };
  virtual const char *describe() const { return "InterpretedClosure"; };
  bool interpretedP() const { return true; };
  T_sp docstring() const { return this->_docstring; };
  List_sp declares() const { return this->_declares; };
  CL_DEFMETHOD List_sp code() const { return this->_code; };
  LambdaListHandler_sp lambdaListHandler() const { return this->_lambdaListHandler; };
  T_sp lambda_list() const;
  void setf_lambda_list(List_sp lambda_list);
};

};

namespace core {
  SMART(LambdaListHandler);
  SMART(NamedFunction);
  class CompiledFunction_O : public Closure_O {
    LISP_CLASS(core, ClPkg, CompiledFunction_O, "CompiledFunction",Closure_O);

#if defined(XML_ARCHIVE)
    void archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
  public:
  CompiledFunction_O(claspFunction fptr, T_sp name) : Base(fptr, name){};
    virtual ~CompiledFunction_O(){};

  public:
#if 0
    static CompiledFunction_sp make(Closure_sp c) {
      GC_ALLOCATE(CompiledFunction_O, f);
      ASSERT(c.generalp());
      f->closure = c;
    //            printf("%s:%d Returning CompiledFunction_sp func=%p &f=%p\n", __FILE__, __LINE__, f.px_ref(), &f);
      return f;
    }
#endif
  public:
  };
};

#ifdef USE_COMPILED_CLOSURE
namespace core {
class CompiledClosure_O : public core::ClosureWithFrame_O {
//  friend void dump_funcs(core::CompiledFunction_sp compiledFunction);
  LISP_CLASS(core,CorePkg,CompiledClosure_O,"CompiledClosure",core::ClosureWithFrame_O);
public:
  core::T_sp _lambdaList;
 public:
  virtual const char *describe() const { return "CompiledClosure"; };
  virtual size_t templatedSizeof() const { return sizeof(*this); };
public:
 CompiledClosure_O(claspFunction fptr,
                   core::T_sp functionName,
                   core::Symbol_sp type,
                   core::T_sp renv,
                   core::T_sp ll,
                   SOURCE_INFO)
   : Base(fptr, functionName, type, renv, SOURCE_INFO_PASS), _lambdaList(ll){};
  bool compiledP() const { return true; };
  core::T_sp lambda_list() const;
  void setf_lambda_list(core::List_sp lambda_list);
  core::LambdaListHandler_sp lambdaListHandler() const { return _Nil<core::LambdaListHandler_O>(); };
};
};
#endif

namespace llvmo {
  FORWARD(ModuleHandle);
};

namespace core {
  LCC_RETURN compiledDispatchFunctionDummyEntryPoint(LCC_ARGS_FUNCALL_ELLIPSIS);
  class CompiledDispatchFunction_O : public core::Closure_O {
    LISP_CLASS(core,CorePkg,CompiledDispatchFunction_O,"CompiledDispatchFunction",core::Closure_O);
  public:
//    DispatchFunction_fptr_type _dispatchEntryPoint;
    llvmo::ModuleHandle_sp  _llvmModule;
  public:
    virtual const char *describe() const { return "CompiledDispatchFunction"; };
    virtual size_t templatedSizeof() const { return sizeof(*this); };
  public:
  CompiledDispatchFunction_O(core::T_sp functionName, core::Symbol_sp type, claspFunction ptr, llvmo::ModuleHandle_sp module ) : Base(ptr,functionName), _llvmModule(module) {};
    bool compiledP() const { return true; };
    core::T_sp lambda_list() const { return Cons_O::createList(cl::_sym_generic_function, core::_sym_arguments); };
    void setf_lambda_list(core::List_sp lambda_list);
    CL_DEFMETHOD T_sp llvm_module() const { return this->_llvmModule;};

  public:
    T_sp closedEnvironment() const { return _Nil<T_O>(); };
    virtual T_sp setSourcePosInfo(T_sp sourceFile, size_t filePos, int lineno, int column) {NOT_APPLICABLE();};
    virtual T_sp cleavir_ast() const {NOT_APPLICABLE();};
    virtual void setf_cleavir_ast(T_sp ast) {NOT_APPLICABLE();};
    T_sp docstring() const { NOT_APPLICABLE();};
    void set_kind(Symbol_sp k) { NOT_APPLICABLE();};
    Symbol_sp getKind() const { return kw::_sym_dispatch_function;};
    virtual int sourceFileInfoHandle() const {return 0; };
    virtual LambdaListHandler_sp lambdaListHandler() const {NOT_APPLICABLE();};
    virtual List_sp declares() const {NOT_APPLICABLE();};
    virtual bool macroP() const {NOT_APPLICABLE();};

    
  };
};

namespace core {

class MacroClosure_O : public BuiltinClosure_O {
    LISP_CLASS(core,CorePkg,MacroClosure_O,"MacroClosure",BuiltinClosure_O);
private:
  typedef T_mv (*MacroPtr)(List_sp, T_sp);
  MacroPtr mptr;
public:
  virtual const char *describe() const { return "MacroClosure"; };
  // constructor
 MacroClosure_O(Symbol_sp name, MacroPtr ptr, SOURCE_INFO) : BuiltinClosure_O(entry_point,name, kw::_sym_macro, SOURCE_INFO_PASS), mptr(ptr) {}
  size_t templatedSizeof() const { return sizeof(MacroClosure_O); };
  virtual Symbol_sp getKind() const { return kw::_sym_macro; };
  static LCC_RETURN LISP_CALLING_CONVENTION();
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
