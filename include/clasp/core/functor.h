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
  FORWARD(ClosureWithSlots);
};

template <>
struct gctools::GCInfo<core::ObjectFile_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
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

  /*! Set to something other than NIL to dump functions as they are defined at startup */
  
extern char* global_dump_functions;
  
  /* The following MUST MATCH %function-description% in cmpintrinsics.lsp
Each thread maintains a current GCRootsInModule structure that stores roots
used by the FunctionDescription objects.  Every time a Function_O object is created
a FunctionDescription is allocated using 'new' and if the GCRootsInModule can still fit
all of the slots (two currently) indicated by the fields that end in 'Index' then that
GCRootsInModule* is written into the FunctionDescription and the indices into the
GCRootsInModule are written into the FunctionDescription.  Then the function description
objects that need to be managed by the GC are written into the GCRootsInModule object.
A pointer to the new FunctionDescription object is then written into the instance
of the Function_O subclass.  A virtual function in the Function_O is used to
recover the pointer to the FunctionDescription object for the Function_O.
I used a virtual function because different subclasses store the FunctionDescription*
pointer at different offsets so that FuncallableInstance_O can have its _Class and _Rack
fields at the same offset as Instance_O.
   */ 
struct FunctionDescription {
// There are six slots below that end with Index
    // They need space opened up in the GCRoots vector
  static const size_t Roots = 2;
  void* functionPrototype;
  gctools::GCRootsInModule* gcrootsInModule;
  size_t sourcePathname_functionName_Index;
  size_t lambdaList_docstring_Index;
  int lineno;
  int column;
  int filepos;
};

FunctionDescription* makeFunctionDescription(T_sp functionName, T_sp lambda_list=_Unbound<T_O>(), T_sp docstring=_Unbound<T_O>(), T_sp sourcePathname=_Unbound<T_O>(), int lineno=-1, int column=-1, int filePos=-1);

void validateFunctionDescription(const char* filename, size_t lineno, Function_sp function);

};


namespace core {

FORWARD(ObjectFile);
class ObjectFile_O : public General_O {
  LISP_ABSTRACT_CLASS(core,CorePkg,ObjectFile_O,"OBJECT-FILE",General_O);
public:
  void*   _ObjectFilePtr;
  size_t  _ObjectFileSize;
  ObjectFile_O(void* ptr, size_t sz) : _ObjectFilePtr(ptr), _ObjectFileSize(sz) {};
  ObjectFile_O() : _ObjectFilePtr(NULL), _ObjectFileSize(0) {};
  ~ObjectFile_O();
  virtual string __repr__() const;
};

};



namespace core {
  /*! Function_O is a Funcallable object that adds no fields to anything that inherits from it
*/
  class Function_O : public General_O {
    LISP_ABSTRACT_CLASS(core,ClPkg,Function_O,"FUNCTION",General_O);
  public:
    std::atomic<claspFunction>    entry;
  public:
    virtual const char *describe() const { return "Function - subclass must implement describe()"; };
    virtual size_t templatedSizeof() const { return sizeof(*this); };
  public:
  Function_O(claspFunction ptr)
    : entry(ptr)
    {
#ifdef _DEBUG_BUILD
      if (((uintptr_t)ptr)&0x7 || !ptr) {
        printf("%s:%d Something other than a function pointer was passed to initialize Function_O::entry -> %p\n", __FILE__, __LINE__, ptr );
        abort();
      }
#endif
    };
    virtual FunctionDescription* fdesc() const = 0;
    // Rewrite the function-description pointer - used in direct-calls.lsp
    
    virtual void set_fdesc(FunctionDescription* address) = 0;

#if 0
    T_sp fdescInfo(int index) const {
      T_sp result((gctools::Tagged)this->fdesc()->gcrootsInModule->getTaggedIndex(LITERAL_TAG_CHAR,this->fdesc()->sourcePosition_functionName_Index));
      return result;
    }
#endif
    CL_LISPIFY_NAME("core:functionName");
    CL_DEFMETHOD virtual T_sp functionName() const {
      Cons_sp cell((gctools::Tagged)this->fdesc()->gcrootsInModule->getTaggedIndex(LITERAL_TAG_CHAR,this->fdesc()->sourcePathname_functionName_Index));
      return CONS_CDR(cell);
    }
    T_sp docstring() const {
      Cons_sp cell((gctools::Tagged)this->fdesc()->gcrootsInModule->getTaggedIndex(LITERAL_TAG_CHAR,this->fdesc()->lambdaList_docstring_Index));
      return CONS_CDR(cell);
    }
    CL_DEFMETHOD void setf_lambdaList(T_sp lambda_list) {
      Cons_sp cell((gctools::Tagged)this->fdesc()->gcrootsInModule->getTaggedIndex(LITERAL_TAG_CHAR,this->fdesc()->lambdaList_docstring_Index));
      cell->rplaca(lambda_list);
    }
    CL_DEFMETHOD T_sp sourcePathname() const {
      Cons_sp cell((gctools::Tagged)this->fdesc()->gcrootsInModule->getTaggedIndex(LITERAL_TAG_CHAR,this->fdesc()->sourcePathname_functionName_Index));
      T_sp result = CONS_CAR(cell);
      return result;
    }
    void setf_sourcePathname(T_sp sourceFileName) const {
      Cons_sp cell((gctools::Tagged)this->fdesc()->gcrootsInModule->getTaggedIndex(LITERAL_TAG_CHAR,this->fdesc()->sourcePathname_functionName_Index));
      cell->rplaca(sourceFileName);
    }
    void setf_docstring(T_sp x) const {
      Cons_sp cell((gctools::Tagged)this->fdesc()->gcrootsInModule->getTaggedIndex(LITERAL_TAG_CHAR,this->fdesc()->lambdaList_docstring_Index));
      cell->rplacd(x);
    }
    size_t filePos() const {
      return this->fdesc()->filepos;
    }
    void setf_filePos(int filePos) { this->fdesc()->filepos = filePos; };
    int lineNumber() const {
      return this->fdesc()->lineno;
    }
    int lineno() const {
      return this->fdesc()->lineno;
    }
    void setf_lineno(int lineno) { this->fdesc()->lineno = lineno; };
    virtual int column() const {
      return this->fdesc()->column;
    }
    void setf_column(int x) { this->fdesc()->column = x; };

    Pointer_sp function_description_address() const;
    void setf_function_description_address(Pointer_sp address);
    
    virtual ObjectFile_sp objectFile() const;
    virtual void setf_objectFile(ObjectFile_sp address);
    
    T_mv function_description() const;
    virtual void __write__(T_sp) const;
    
    Pointer_sp function_pointer() const;
    virtual bool compiledP() const { return false; };
    virtual bool interpretedP() const { return false; };
    virtual bool builtinP() const { return false; };
    virtual T_sp sourcePosInfo() const { return _Nil<T_O>(); };
    CL_DEFMETHOD T_sp functionLambdaListHandler() const {
      return this->lambdaListHandler();
    }
    virtual T_sp closedEnvironment() const {SUBIMP();};
    T_sp setSourcePosInfo(T_sp sourceFile, size_t filePos, int lineno, int column);
    virtual T_mv functionSourcePos() const;
    virtual T_sp lambdaListHandler() const {SUBIMP();};
    virtual T_sp lambdaList() const {
      Cons_sp cell((gctools::Tagged)this->fdesc()->gcrootsInModule->getTaggedIndex(LITERAL_TAG_CHAR,this->fdesc()->lambdaList_docstring_Index));
      return CONS_CAR(cell);
    }
    virtual string __repr__() const;
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
    FunctionDescription* _FunctionDescription;
    ObjectFile_sp                 _ObjectFile;
  public:
  Closure_O(claspFunction fptr, FunctionDescription* fdesc ) : Base(fptr), _FunctionDescription(fdesc) {
      describeFunction();
    };
  public:
  virtual ObjectFile_sp objectFile() const { return this->_ObjectFile; };
  virtual void setf_objectFile(ObjectFile_sp address) { this->_ObjectFile = address;};
    virtual FunctionDescription* fdesc() const { return this->_FunctionDescription; };
    virtual void set_fdesc(FunctionDescription* fdesc) { this->_FunctionDescription = fdesc; };
    virtual const char *describe() const { return "Closure"; };
    void describeFunction() const;
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
    void finishSetup(LambdaListHandler_sp llh) {
      this->_lambdaListHandler = llh;
    }
    T_sp closedEnvironment() const { return _Nil<T_O>(); };
    virtual size_t templatedSizeof() const { return sizeof(*this); };
    virtual const char *describe() const { return "BuiltinClosure"; };
    bool builtinP() const { return true; };
    T_sp lambdaListHandler() const { return this->_lambdaListHandler; };
  };

}

namespace core {
  extern LCC_RETURN interpretedClosureEntryPoint(LCC_ARGS_FUNCALL_ELLIPSIS);
  extern LCC_RETURN unboundFunctionEntryPoint(LCC_ARGS_FUNCALL_ELLIPSIS);
  extern LCC_RETURN unboundSetfFunctionEntryPoint(LCC_ARGS_FUNCALL_ELLIPSIS);
   
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
      //! Slots must be the last field
    typedef core::T_sp value_type;
  public:
    ClosureType   closureType;
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
                     FunctionDescription* functionDescription,
                     ClosureType nclosureType)
    : Base(ptr, functionDescription),
      closureType(nclosureType),
      _Slots(capacity,_Unbound<T_O>(),true) {};
    virtual string __repr__() const;
    core::T_sp lambdaListHandler() const {
      switch (this->closureType) {
      case interpretedClosure:
          return (*this)[INTERPRETED_CLOSURE_LAMBDA_LIST_HANDLER_SLOT];
      case bclaspClosure:
          return _Nil<T_O>();
      case cclaspClosure:
          return _Nil<T_O>();
      };
    }
    CL_DEFMETHOD T_sp interpretedSourceCode();
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
    bool openP();
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
