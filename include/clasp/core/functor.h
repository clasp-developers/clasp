#ifndef functor_h
#define functor_h


namespace core {
  class Functoid : public General_O {
  FRIEND_GC_SCANNER(Functoid);
public:
  virtual const char *describe() const { return "Functoid - subclass must implement describe()"; };
  inline LCC_RETURN operator()(LCC_ARGS_ELLIPSIS) {
    VaList_S lcc_arglist_s;
    va_start(lcc_arglist_s._Args, LCC_VA_START_ARG);
    LCC_SPILL_REGISTER_ARGUMENTS_TO_VA_LIST(lcc_arglist_s);
    core::T_O *lcc_arglist = lcc_arglist_s.asTaggedPtr();
    return this->invoke_va_list(LCC_PASS_ARGS);
  }

  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION() {
    printf("Subclass of Functoid must implement 'activate'\n");
    exit(1);
  };
  virtual size_t templatedSizeof() const { return sizeof(*this); };

public:
  T_sp name;

public:
  Functoid(T_sp n);
  string nameAsString();
  virtual ~Functoid(){};
};
};

namespace core {
  struct FixedData {
    T_sp _XPtr;
    T_sp _YPtr;
    double _DoubleZ;
  };
  struct SlotData {
    T_sp _APtr;
    T_sp _BPtr;
    double _C;
  };
  struct ClosureWithRecords : public Functoid {
    typedef SlotData value_type;
  public:
    T_sp   _DummyT_OPtr;
    FixedData _Fixed;
    fnLispCallingConvention _FunctionPointer;
    gctools::GCArray_moveable<value_type,0> _Slots;
    ClosureWithRecords(size_t capacity, T_sp name )
    : Functoid(name)
      , _Slots(capacity) {}
  };

  struct ClosureWithSlots : public Functoid {
  public:
    typedef T_sp value_type;
  public:
    T_sp   _DummyT_OPtr;
    FixedData _Fixed;
    fnLispCallingConvention _FunctionPointer;
    gctools::GCArray_moveable<value_type,0> _Slots;
  ClosureWithSlots(size_t num_slots, T_sp name, fnLispCallingConvention fptr=NULL )
    : Functoid(name)
      , _Slots(num_slots) {}
  };

  inline CL_DEFUN size_t core__sizeof_header_and_closure_with_slots(size_t numberOfSlots) {
    return gctools::global_alignup_sizeof_header + gctools::AlignUp(sizeof(ClosureWithSlots)) + sizeof(ClosureWithSlots::value_type)*numberOfSlots;
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



namespace core { 
class Closure : public Functoid {
public:
  T_sp closedEnvironment;

public:
  Closure(T_sp name, T_sp env) : Functoid(name), closedEnvironment(env){};
  virtual ~Closure(){};

public:
  virtual void setAssociatedFunctions(core::List_sp assocFuncs){};
  virtual const char *describe() const { return "Closure"; };
  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION() {
    printf("Subclass of Closure must implement 'activate'\n");
    exit(1);
  };

  virtual void *functionAddress() const { return NULL; };
  virtual T_sp sourcePosInfo() const { return _Nil<T_O>(); };
  virtual bool macroP() const = 0;
  virtual void setKind(Symbol_sp k) = 0;
  virtual Symbol_sp getKind() const = 0;
  virtual bool compiledP() const { return false; };
  virtual bool interpretedP() const { return false; };
  virtual bool builtinP() const { return false; };
  virtual int sourceFileInfoHandle() const;
  virtual size_t filePos() const { return 0; }
  virtual int lineNumber() const { return 0; }
  virtual int column() const { return 0; };
  virtual LambdaListHandler_sp lambdaListHandler() const = 0;
  virtual T_sp lambdaList() const = 0;
  virtual void setf_lambda_list(T_sp lambda_list) = 0;
  virtual T_sp docstring() const;
  virtual List_sp declares() const;
  virtual T_sp cleavir_ast() const;
  virtual void setf_cleavir_ast(T_sp ast);
  virtual T_sp setSourcePosInfo(T_sp sourceFile, size_t filePos, int lineno, int column);
};
};


namespace core {

  class Creator : public General_O {
  public:
  // Some Creators don't actually allocate anything -
  // classes that don't have default allocators
    virtual bool allocates() const { return true; };
  /*! If this is the allocator for a primary CxxAdapter class then return true, */
    virtual int duplicationLevel() const { return 0; };
    virtual size_t templatedSizeof() const = 0;
    virtual gc::tagged_pointer<Creator> duplicateForClassName(core::Symbol_sp className) {
      printf("Subclass must implement Creator::duplicateForClassName\n");
      exit(1);
    };
    virtual void describe() const = 0;
    virtual core::T_sp allocate() = 0;
  };
};
namespace core {
class FunctionClosure : public Closure {
public:
  //  T_sp _SourcePosInfo;
  Symbol_sp kind;
  T_sp _cleavir_ast;
  Fixnum _sourceFileInfoHandle;
  Fixnum _filePos;
  Fixnum _lineno;
  Fixnum _column;

public:
  DISABLE_NEW();
#define SOURCE_INFO core::Fixnum sourceFileInfoHandle, core::Fixnum filePos, core::Fixnum lineno, core::Fixnum column
#define SOURCE_INFO_PASS sourceFileInfoHandle, filePos, lineno, column
  FunctionClosure(T_sp name, Symbol_sp k, T_sp env, SOURCE_INFO)
      : Closure(name, env), kind(k), _cleavir_ast(_Nil<T_O>()), _sourceFileInfoHandle(sourceFileInfoHandle), _filePos(filePos), _lineno(lineno), _column(column){};
  FunctionClosure(T_sp name)
      : Closure(name, _Nil<T_O>()), kind(kw::_sym_function), _cleavir_ast(_Nil<T_O>()), _sourceFileInfoHandle(0), _filePos(0), _lineno(0), _column(0){};

  virtual size_t templatedSizeof() const { return sizeof(*this); };

  virtual const char *describe() const { return "SingleDispatchGenericFunctoid"; };
  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION() { SIMPLE_ERROR(BF("Subclass must implement")); };
  void setKind(Symbol_sp k) { this->kind = k; };
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
};

class BuiltinClosure : public FunctionClosure {
public:
  LambdaListHandler_sp _lambdaListHandler;

public:
  DISABLE_NEW();
  BuiltinClosure(T_sp name, Symbol_sp k, SOURCE_INFO)
      : FunctionClosure(name, k, _Nil<T_O>(), SOURCE_INFO_PASS){};
  BuiltinClosure(T_sp name)
      : FunctionClosure(name) {}
  void finishSetup(LambdaListHandler_sp llh, Symbol_sp k) {
    this->_lambdaListHandler = llh;
    this->kind = k;
  }
  virtual T_sp lambdaList() const;
  virtual void setf_lambda_list(T_sp lambda_list);
  virtual size_t templatedSizeof() const { return sizeof(*this); };
  virtual const char *describe() const { return "BuiltinClosure"; };
  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION();
  bool builtinP() const { return true; };
  LambdaListHandler_sp lambdaListHandler() const { return this->_lambdaListHandler; };
};

/*! Shouldn't this derive from a Functoid - it doesn't need a closedEnvironment */
class InstanceClosure : public FunctionClosure {
public:
  GenericFunctionPtr entryPoint;
  Instance_sp instance;
  T_sp lambda_list;
public:
  DISABLE_NEW();
  InstanceClosure(T_sp name, GenericFunctionPtr ep, Instance_sp inst)
    : FunctionClosure(name), entryPoint(ep), instance(inst), lambda_list(_Nil<T_O>()){};
  virtual size_t templatedSizeof() const { return sizeof(*this); };
  virtual const char *describe() const { return "InstanceClosure"; };
  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION();
  LambdaListHandler_sp lambdaListHandler() const { return _Nil<LambdaListHandler_O>(); };
  T_sp lambdaList() const { return this->lambda_list; };
  void setf_lambda_list(T_sp ll) { this->lambda_list = ll; };
};
}

#endif
