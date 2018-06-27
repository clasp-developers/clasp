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
  FORWARD(Closure);
  FORWARD(BuiltinClosure);
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
    int sourceFileInfoIndex;
    int functionNameIndex;
    int lambdaListIndex;
    int docstringIndex;
    int lineno;
    int column;
    int filepos;
    int declareIndex;
  };

 FunctionDescription* makeFunctionDescription(T_sp functionName, T_sp lambda_list=_Unbound<T_O>(), T_sp docstring=_Unbound<T_O>(), SourceFileInfo_sp sourceFileInfo=_Unbound<SourceFileInfo_O>(), int lineno=0, int column=0, int filePos=0, T_sp declares = _Nil<core::T_O>());
//  FunctionDescription* makeFunctionDescription(T_sp functionName, T_sp lambda_list, T_sp docstring, SourcePosInfo_sp sourcePosInfo, T_sp functionType = kw::_sym_function, T_sp declares = _Nil<core::T_O>());



  /*! Function_O is a Funcallable object that adds no fields to anything that inherits from it
*/
  class Function_O : public General_O {
    LISP_ABSTRACT_CLASS(core,ClPkg,Function_O,"FUNCTION",General_O);
  public:
    std::atomic<claspFunction>    entry;
    FunctionDescription* _FunctionDescription;
    Symbol_sp            _FunctionType;
  public:
    virtual const char *describe() const { return "Function - subclass must implement describe()"; };
    virtual size_t templatedSizeof() const { return sizeof(*this); };
  public:
  Function_O(claspFunction ptr, FunctionDescription* functionDescription)
    : entry(ptr), _FunctionDescription(functionDescription), _FunctionType(kw::_sym_function)
    {
#ifdef _DEBUG_BUILD
      if (((uintptr_t)ptr)&0x7 || !ptr) {
        printf("%s:%d Something other than a function pointer was passed to initialize Function_O::entry -> %p\n", __FILE__, __LINE__, ptr );
        abort();
      }
#endif
    };
    T_sp fdescInfo(int index) const {
      T_sp result((gctools::Tagged)this->_FunctionDescription->gcrootsInModule->get(this->_FunctionDescription->functionNameIndex));
      return result;
    }
    CL_LISPIFY_NAME("core:functionName");
    CL_DEFMETHOD T_sp functionName() const {
      return this->fdescInfo(this->_FunctionDescription->functionNameIndex);
    }
    virtual T_sp getKind() const {
      return this->_FunctionType;
    }
    void setKind(Symbol_sp k) { this->_FunctionType = k;};
    T_sp docstring() const {
      T_sp result((gctools::Tagged)this->_FunctionDescription->gcrootsInModule->get(this->_FunctionDescription->docstringIndex));
      return result;
    }
    T_sp declares() const {
      T_sp result((gctools::Tagged)this->_FunctionDescription->gcrootsInModule->get(this->_FunctionDescription->declareIndex));
      return result;
    }
    void setf_lambdaList(T_sp lambda_list) {
      this->_FunctionDescription->gcrootsInModule->set(this->_FunctionDescription->lambdaListIndex,lambda_list.tagged_());
    }
    T_sp sourceFileInfo() const {
      T_sp result((gctools::Tagged)this->_FunctionDescription->gcrootsInModule->get(this->_FunctionDescription->sourceFileInfoIndex));
      return result;
    }

    void setf_sourceFileInfo(T_sp sourceFileInfo) const {
      this->_FunctionDescription->gcrootsInModule->set(this->_FunctionDescription->sourceFileInfoIndex,sourceFileInfo.tagged_());
    }
size_t filePos() const {
      return this->_FunctionDescription->filepos;
    }
    void setf_filePos(int filePos) { this->_FunctionDescription->filepos = filePos; };
    int lineNumber() const {
      return this->_FunctionDescription->lineno;
    }
    int lineno() const {
      return this->_FunctionDescription->lineno;
    }
    void setf_lineno(int lineno) { this->_FunctionDescription->lineno = lineno; };
    virtual int column() const {
      return this->_FunctionDescription->column;
    }
    void setf_column(int x) { this->_FunctionDescription->column = x; };

    T_mv function_description() const;
    virtual void __write__(T_sp) const;
    
    Pointer_sp function_pointer() const;
    CL_DEFMETHOD bool macroP() const { return this->_FunctionType != kw::_sym_macro; };
    virtual bool compiledP() const { return false; };
    virtual bool interpretedP() const { return false; };
    virtual bool builtinP() const { return false; };
    virtual T_sp sourcePosInfo() const { return _Nil<T_O>(); };
    CL_DEFMETHOD virtual T_sp functionKind() const { return this->getKind(); };
    CL_DEFMETHOD List_sp function_declares() const { return this->declares(); };
    CL_DEFMETHOD T_sp functionLambdaListHandler() const {
      return this->lambdaListHandler();
    }
    virtual T_sp closedEnvironment() const {SUBIMP();};
    T_sp setSourcePosInfo(T_sp sourceFile, size_t filePos, int lineno, int column);
    virtual T_mv functionSourcePos() const;
    virtual LambdaListHandler_sp lambdaListHandler() const {SUBIMP();};
    virtual T_sp lambdaList() const;
    virtual string __repr__() const;
    CL_DEFMETHOD virtual T_sp function_literal_vector_copy() const {SUBIMP();};
    virtual ~Function_O() {};
  };
};

namespace core {
  extern bool cl__stringp(T_sp obj);
  extern void lisp_error_sprintf(const char* file, int line, const char* fmt, ...);
  
SMART(LambdaListHandler);
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
class Closure_O : public Function_O {
    LISP_CLASS(core,CorePkg,Closure_O,"Closure",Function_O);
public:
 Closure_O(claspFunction fptr, FunctionDescription* fdesc ) : Base(fptr,fdesc) {};
public:
  virtual const char *describe() const { return "Closure"; };
};
};


namespace core {
#define SOURCE_INFO core::Fixnum sourceFileInfoHandle, core::Fixnum filePos, core::Fixnum lineno, core::Fixnum column
#define SOURCE_INFO_PASS sourceFileInfoHandle, filePos, lineno, column
  
  class BuiltinClosure_O : public Closure_O {
    LISP_CLASS(core,CorePkg,BuiltinClosure_O,"BuiltinClosure",Closure_O);
  public:
    LambdaListHandler_sp _lambdaListHandler;
  public:
  BuiltinClosure_O(claspFunction fptr, FunctionDescription* fdesc)
    : Closure_O(fptr, fdesc), _lambdaListHandler(_Unbound<LambdaListHandler_O>())  {};
  BuiltinClosure_O(claspFunction fptr, FunctionDescription* fdesc, LambdaListHandler_sp llh)
    : Closure_O(fptr, fdesc), _lambdaListHandler(llh)  {};
    void finishSetup(LambdaListHandler_sp llh, Symbol_sp k) {
      this->_lambdaListHandler = llh;
      this->setKind(k);
    }
    T_sp closedEnvironment() const { return _Nil<T_O>(); };
    virtual size_t templatedSizeof() const { return sizeof(*this); };
    virtual const char *describe() const { return "BuiltinClosure"; };
    bool builtinP() const { return true; };
    LambdaListHandler_sp lambdaListHandler() const { return this->_lambdaListHandler; };
  };

}

namespace core {
  extern LCC_RETURN interpretedClosureEntryPoint(LCC_ARGS_FUNCALL_ELLIPSIS);
   
  class ClosureWithSlots_O final : public core::Closure_O {
    LISP_CLASS(core,CorePkg,ClosureWithSlots_O,"ClosureWithSlots",core::Closure_O);
    typedef enum { interpretedClosure, bclaspClosure, cclaspClosure } ClosureType;
#define ENVIRONMENT_SLOT 0
#define INTERPRETED_CLOSURE_SLOTS  3
#define INTERPRETED_CLOSURE_ENVIRONMENT_SLOT ENVIRONMENT_SLOT
#define INTERPRETED_CLOSURE_FORM_SLOT 1
#define INTERPRETED_CLOSURE_LAMBDA_LIST_HANDLER_SLOT 2
#define BCLASP_CLOSURE_SLOTS  1
#define BCLASP_CLOSURE_ENVIRONMENT_SLOT ENVIRONMENT_SLOT
  public:
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
    static ClosureWithSlots_sp make_interpreted_closure(T_sp name, T_sp type, T_sp lambda_list, LambdaListHandler_sp lambda_list_handler, T_sp declares, T_sp docstring, T_sp form, T_sp environment, SOURCE_INFO);
    
    static ClosureWithSlots_sp make_bclasp_closure(T_sp name, claspFunction ptr, T_sp type, T_sp lambda_list, T_sp environment);
    
    static ClosureWithSlots_sp make_cclasp_closure(T_sp name, claspFunction ptr, T_sp type, T_sp lambda_list, SOURCE_INFO);
  public:
  ClosureWithSlots_O(size_t capacity,
                     claspFunction ptr,
                     FunctionDescription* functionDescription)
    : Base(ptr, functionDescription),
      _Slots(capacity,_Unbound<T_O>(),true) {};
    virtual string __repr__() const;
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
