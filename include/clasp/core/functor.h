#ifndef functor_h
#define functor_h


namespace core {
  FORWARD(Function);
  FORWARD(NamedFunction);
  FORWARD(Closure);
  FORWARD(BuiltinClosure);
  FORWARD(FunctionClosure);
  FORWARD(Closure);
  FORWARD(InterpretedClosure);
  FORWARD(CompiledClosure);
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

namespace core {
  /*! Function_O is a Funcallable object that adds no fields to anything that inherits from it
*/
  class Function_O : public General_O {
    LISP_ABSTRACT_CLASS(core,ClPkg,Function_O,"FUNCTION",General_O);
  public:
  virtual const char *describe() const { return "Function - subclass must implement describe()"; };
  inline LCC_RETURN operator()(LCC_ARGS_ELLIPSIS) {
    VaList_S lcc_arglist_s;
    va_start(lcc_arglist_s._Args, LCC_VA_START_ARG);
    LCC_SPILL_REGISTER_ARGUMENTS_TO_VA_LIST(lcc_arglist_s);
    core::T_O *lcc_arglist = lcc_arglist_s.asTaggedPtr();
    return this->invoke_va_list(LCC_PASS_ARGS);
  }

  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION() {
    printf("Subclass of Functoid must implement 'activate'\n");
    abort();
  };
  virtual size_t templatedSizeof() const { return sizeof(*this); };
public:
  Function_O()  {};
  virtual T_sp name() const = 0;
  virtual string nameAsString() const {SUBIMP();};
  virtual bool compiledP() const { return false; };
  virtual bool interpretedP() const { return false; };
  virtual bool builtinP() const { return false; };
  virtual T_sp sourcePosInfo() const { return _Nil<T_O>(); };
  CL_DEFMETHOD T_sp functionName() const { return this->name(); };
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
  virtual void *functionAddress() const = 0;
  CL_DEFMETHOD virtual bool macroP() const = 0;
  virtual void set_kind(Symbol_sp k) = 0;
  virtual Symbol_sp getKind() const = 0;
  virtual int sourceFileInfoHandle() const = 0;
  virtual size_t filePos() const { return 0; }
  virtual int lineNumber() const { return 0; }
  virtual int column() const { return 0; };
  virtual LambdaListHandler_sp lambdaListHandler() const = 0;
  virtual T_sp lambda_list() const = 0;
  CL_DEFMETHOD virtual void setAssociatedFunctions(List_sp funcs) = 0;
  virtual string __repr__() const;
};
};

#if 0
namespace core {
  struct ClosureWithSlots_O : public Function_O {
  public:
    typedef T_sp value_type;
  public:
    fnLispCallingConvention _FunctionPointer;
    gctools::GCArray_moveable<value_type> _Slots;
  ClosureWithSlots_O(size_t num_slots, T_sp name, fnLispCallingConvention fptr=NULL )
    : Function_O(name)
      , _Slots(_Unbound<T_O>(),num_slots) {}
  };

  inline CL_DEFUN size_t core__sizeof_header_and_closure_with_slots(size_t numberOfSlots) {
    return gc::sizeof_container_with_header<ClosureWithSlots_O>(numberOfSlots);
  };

#if 0
  inline gctools::tagged_ptr<ClosureWithSlots> initialize_closure_with_slots(void* block, T_sp name, fnLispCallingConvention fptr, size_t num_slots, ... ) {
    gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(block);
    ClosureWithSlots* closure = reinterpret_cast<ClosureWithSlots*>((char*)block+gctools::global_alignup_sizeof_header);
    new(header) gctools::Header_s(gctools::GCKind<ClosureWithSlots>::Kind);
    new(closure) ClosureWithSlots(name,fptr,num_slots);
    va_list valist;
    va_start(valist, num_slots);
    for ( size_t i=0; i<num_slots; ++i ) {
      closure->_Slots[i] = ClosureWithSlots::value_type(va_arg(valist,T_O*));
    }
    va_end(valist);
    return gctools::tagged_ptr<ClosureWithSlots>((gctools::Tagged)gctools::tag_general<ClosureWithSlots*>(closure));
  }

  inline ClosureWithSlots::value_type closure_with_slots_read_slot(gctools::tagged_ptr<ClosureWithSlots> tagged_closure, size_t index)
  {
    ClosureWithSlots* closure = gctools::untag_general(tagged_closure.theObject);
    return closure->_Slots[index];
  }

  inline void closure_with_slots_write_slot(gctools::tagged_ptr<ClosureWithSlots> tagged_closure, size_t index, ClosureWithSlots::value_type val)
  {
    ClosureWithSlots* closure = gctools::untag_general(tagged_closure.theObject);
    closure->_Slots[index] = val;
  }
#endif
};
#endif



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
 NamedFunction_O(T_sp name) : _name(name) {};
  virtual ~NamedFunction_O(){};
public:
    T_sp name() const { return this->_name; };
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
 Closure_O(T_sp name) : Base(name) {};
public:
  virtual const char *describe() const { return "Closure"; };
  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION() {
    printf("Subclass of Closure must implement 'activate'\n");
    abort();
  };
  virtual string nameAsString() const;
};
};


namespace core {
  class FunctionClosure_O : public Closure_O {
    LISP_CLASS(core,CorePkg,FunctionClosure_O,"FunctionClosure",Closure_O);
  public:
  //  T_sp _SourcePosInfo;
    Symbol_sp kind;
    T_sp _cleavir_ast;
    Fixnum _sourceFileInfoHandle;
    Fixnum _filePos;
    Fixnum _lineno;
    Fixnum _column;
  public:
#define SOURCE_INFO core::Fixnum sourceFileInfoHandle, core::Fixnum filePos, core::Fixnum lineno, core::Fixnum column
#define SOURCE_INFO_PASS sourceFileInfoHandle, filePos, lineno, column
  FunctionClosure_O(T_sp name, Symbol_sp k, SOURCE_INFO)
    : Closure_O(name), kind(k), _cleavir_ast(_Nil<T_O>()), _sourceFileInfoHandle(sourceFileInfoHandle), _filePos(filePos), _lineno(lineno), _column(column){};
  FunctionClosure_O(T_sp name)
    : Closure_O(name), kind(kw::_sym_function), _cleavir_ast(_Nil<T_O>()), _sourceFileInfoHandle(0), _filePos(0), _lineno(0), _column(0){};
    static FunctionClosure_sp create(T_sp name, T_sp function_kind, SOURCE_INFO ) {
      FunctionClosure_sp fc = gctools::GC<FunctionClosure_O>::allocate(name,function_kind,SOURCE_INFO_PASS);
      return fc;
    }
    virtual size_t templatedSizeof() const { return sizeof(*this); };
    virtual const char *describe() const { return "FunctionClosure"; };
    LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION() { SIMPLE_ERROR(BF("Subclass must implement")); };
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
    virtual void* functionAddress() const { NOT_APPLICABLE();};
    virtual void setAssociatedFunctions(List_sp funcs) {NOT_APPLICABLE();};
  };

  class BuiltinClosure_O : public FunctionClosure_O {
    LISP_CLASS(core,CorePkg,BuiltinClosure_O,"BuiltinClosure",FunctionClosure_O);
  public:
    LambdaListHandler_sp _lambdaListHandler;
    List_sp _declares;
    T_sp _docstring;
  public:
  BuiltinClosure_O(T_sp name, Symbol_sp k, SOURCE_INFO)
    : FunctionClosure_O(name, k, SOURCE_INFO_PASS){};
  BuiltinClosure_O(T_sp name)
    : FunctionClosure_O(name) {}
    void finishSetup(LambdaListHandler_sp llh, Symbol_sp k) {
      this->_lambdaListHandler = llh;
      this->kind = k;
    }
    T_sp closedEnvironment() const { return _Nil<T_O>(); };
    virtual T_sp lambda_list() const;
    virtual void setf_lambda_list(List_sp lambda_list);
    virtual size_t templatedSizeof() const { return sizeof(*this); };
    virtual const char *describe() const { return "BuiltinClosure"; };
    LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION();
    bool builtinP() const { return true; };
    LambdaListHandler_sp lambdaListHandler() const { return this->_lambdaListHandler; };
    T_sp docstring() const { return this->_docstring; };
    List_sp declares() const { return this->_declares; };
  };

}

namespace core {
  class ClosureWithSlots_O final : public core::FunctionClosure_O {
    LISP_CLASS(core,CorePkg,ClosureWithSlots_O,"ClosureWithSlots",core::FunctionClosure_O);
  public:
    core::T_sp llvmFunction;
    core::CompiledClosure_fptr_type fptr;
    core::T_sp associatedFunctions;
    core::T_sp _lambdaList;
  //! Slots must be the last field
    typedef core::T_sp value_type;
    gctools::GCArray_moveable<value_type> _Slots;
  public:
    virtual const char *describe() const { return "CompiledClosure"; };
    virtual size_t templatedSizeof() const { return sizeof(*this); };
    virtual void *functionAddress() const { return (void *)this->fptr; }
  public:
  ClosureWithSlots_O(size_t capacity,
                     core::T_sp functionName,
                     core::Symbol_sp type,
                     core::CompiledClosure_fptr_type ptr,
                     core::T_sp llvmFunc,
                     core::T_sp assocFuncs,
                     core::T_sp ll,
                     SOURCE_INFO)
    : Base(functionName, type, SOURCE_INFO_PASS),
      fptr(ptr),
      associatedFunctions(assocFuncs),
      _lambdaList(ll), 
      _Slots(_Unbound<T_O>(),capacity) {};
    void setAssociatedFunctions(core::List_sp assocFuncs) { this->associatedFunctions = assocFuncs; };
    bool compiledP() const { return true; };
    core::T_sp lambda_list() const { return this->_lambdaList; };
    void setf_lambda_list(core::List_sp lambda_list) { this->_lambdaList = lambda_list; };
    core::LambdaListHandler_sp lambdaListHandler() const { return _Nil<core::LambdaListHandler_O>(); };
    inline T_sp &operator[](int idx) {
      ASSERT(idx>=0 && idx<this->_Slots._Capacity);
#ifdef DEBUG_FRAME_BOUNDS
      if ( idx<0 || idx >= this->_Slots._Capacity) {
        printf("%s:%d Caught out of bounds access to ValueFrame_O idx=%d capacity=%d\n", __FILE__, __LINE__, idx, this->_Slots._Capacity );
      }
#endif
      return this->_Slots[idx];
    };
    inline const T_sp &operator[](int idx) const {
      ASSERT(idx>=0 && idx<this->_Slots._Capacity);
#ifdef DEBUG_FRAME_BOUNDS
      if ( idx<0 || idx >= this->_Slots._Capacity) {
        printf("%s:%d Caught out of bounds access to ValueFrame_O idx=%d capacity=%d\n", __FILE__, __LINE__, idx, this->_Slots._Capacity );
      }
#endif
      return this->_Slots[idx];
    };
    inline LCC_RETURN LISP_CALLING_CONVENTION() {
      core::InvocationHistoryFrame _frame(Closure_sp(this), lcc_arglist, _Nil<T_O>());
      core::T_O* tagged_closure = gctools::tag_general(this);
      return (*(this->fptr))(LCC_PASS_ARGS_ENV(tagged_closure));
    };
  };
};
namespace core {
class ClosureWithFrame_O : public FunctionClosure_O {
  LISP_CLASS(core,CorePkg,ClosureWithFrame_O,"ClosureWithFrame",FunctionClosure_O);
public:
  T_sp _closedEnvironment;
public:
  T_sp closedEnvironment() const { return this->_closedEnvironment;};
 ClosureWithFrame_O(T_sp fn, Symbol_sp k, T_sp env, SOURCE_INFO) : Base(fn,k,SOURCE_INFO_PASS), _closedEnvironment(env) {};
  virtual size_t templatedSizeof() const { return sizeof(*this); };
  virtual const char *describe() const { return "ClosureWithFrame"; };
};

};


namespace core {
  /*! Keeps track of how many InterpretedClosure calls there are
*/
  extern uint64_t global_interpreted_closure_calls;
  
class InterpretedClosure_O : public ClosureWithFrame_O {
    LISP_CLASS(core,CorePkg,InterpretedClosure_O,"InterpretedClosure",ClosureWithFrame_O);
public:
  LambdaListHandler_sp _lambdaListHandler;
  List_sp _declares;
  T_sp _docstring;
  List_sp _code;
public:
  DISABLE_NEW();
  InterpretedClosure_O(T_sp fn, Symbol_sp k, LambdaListHandler_sp llh, List_sp dec, T_sp doc, T_sp env, List_sp c, SOURCE_INFO);
  virtual size_t templatedSizeof() const { return sizeof(*this); };
  virtual const char *describe() const { return "InterpretedClosure"; };
  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION();
  bool interpretedP() const { return true; };
  T_sp docstring() const { return this->_docstring; };
  List_sp declares() const { return this->_declares; };
  List_sp code() const { return this->_code; };
  LambdaListHandler_sp lambdaListHandler() const { return this->_lambdaListHandler; };
  T_sp lambda_list() const;
  void setf_lambda_list(List_sp lambda_list);
};

};


namespace core {
class CompiledClosure_O : public core::ClosureWithFrame_O {
//  friend void dump_funcs(core::CompiledFunction_sp compiledFunction);
  LISP_CLASS(core,CorePkg,CompiledClosure_O,"CompiledClosure",core::ClosureWithFrame_O);
public:
  core::T_sp llvmFunction;
  core::CompiledClosure_fptr_type fptr;
  core::T_sp associatedFunctions;
  core::T_sp _lambdaList;
 public:
  virtual const char *describe() const { return "CompiledClosure"; };
  virtual size_t templatedSizeof() const { return sizeof(*this); };
  virtual void *functionAddress() const { return (void *)this->fptr; }

public:
  CompiledClosure_O(core::T_sp functionName, core::Symbol_sp type, core::CompiledClosure_fptr_type ptr, core::T_sp llvmFunc, core::T_sp renv, core::T_sp assocFuncs,
                  core::T_sp ll, SOURCE_INFO)
      : Base(functionName, type, renv, SOURCE_INFO_PASS), fptr(ptr), associatedFunctions(assocFuncs), _lambdaList(ll){};
  void setAssociatedFunctions(core::List_sp assocFuncs) { this->associatedFunctions = assocFuncs; };
  bool compiledP() const { return true; };
  core::T_sp lambda_list() const;
  void setf_lambda_list(core::List_sp lambda_list);
  core::LambdaListHandler_sp lambdaListHandler() const { return _Nil<core::LambdaListHandler_O>(); };
  DISABLE_NEW();
  inline LCC_RETURN LISP_CALLING_CONVENTION() {
    core::InvocationHistoryFrame _frame(Closure_sp(this), lcc_arglist, this->_closedEnvironment);
    core::T_O* tagged_closure = gctools::tag_general(this);
    return (*(this->fptr))(LCC_PASS_ARGS_ENV(tagged_closure));
  };
};
};



#endif

