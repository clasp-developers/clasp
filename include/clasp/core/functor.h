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
  FORWARD(GlobalEntryPoint);
  FORWARD(LocalEntryPoint);
  FORWARD(Function);
  FORWARD(Closure);
  FORWARD(BuiltinClosure);
  FORWARD(ClosureWithSlots);
};

namespace llvmo {
FORWARD(ObjectFile);
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

template <>
struct gctools::GCInfo<core::GlobalEntryPoint_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = collectable_immobile;
};

template <>
struct gctools::GCInfo<core::LocalEntryPoint_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = collectable_immobile;
};

namespace core {
FORWARD(FunctionDescription);
GlobalEntryPoint_sp ensureEntryPoint(GlobalEntryPoint_sp ep, claspFunction entry_point);
};
#define ENSURE_ENTRY_POINT(_ep_,_entry_point_) core::ensureEntryPoint(_ep_,_entry_point_)

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
 FORWARD(FunctionDescription);
 class FunctionDescription_O : public General_O {
   LISP_CLASS(core,CorePkg,FunctionDescription_O,"FunctionDescription",General_O);
 public:
  /* vtable */                                 //  1 vtable from General_O
   T_sp _functionName;                         //  2 function-name
   T_sp _sourcePathname;                       //  3 source-info
   T_sp _lambdaList;                           //  4 lambda-list 
   T_sp _docstring;                            //  5 docstring
   T_sp _declares;                             //  6 declares
   int lineno;                                 //  7 lineno
   int column;                                 //  8 column
   int filepos;                                //  9 filepos
 public:
   FunctionDescription_O() {};
 public:
  // Accessors
   T_sp sourcePathname() const;
   void setf_sourcePathname(T_sp);
   T_sp functionName() const;
   void setf_functionName(T_sp);
   T_sp lambdaList() const;
   void setf_lambdaList(T_sp);
   T_sp docstring() const;
   void setf_docstring(T_sp);
   T_sp declares() const;
   void setf_declares(T_sp);
 public:
   // Custom hash-tables need an equality and hashing function
   bool function_description_equal(T_sp other) const;
   Fixnum function_description_sxhash_equal() const;
 };


 FORWARD(EntryPointBase);
 class EntryPointBase_O : public General_O {
   LISP_CLASS(core,CorePkg,EntryPointBase_O,"EntryPointBase",General_O);
 public:
   CLASP_DEFAULT_CTOR EntryPointBase_O() {};
 public:
   FunctionDescription_sp _FunctionDescription;
 public:
  // Accessors
   EntryPointBase_O(FunctionDescription_sp fdesc) : _FunctionDescription(fdesc) {  };
   CL_DEFMETHOD FunctionDescription_sp functionDescription() const { return this->_FunctionDescription; };
   virtual Pointer_sp defaultEntryAddress() const;
 };

 FORWARD(CodeEntryPoint);
 class CodeEntryPoint_O : public EntryPointBase_O {
   LISP_CLASS(core,CorePkg,CodeEntryPoint_O,"CodeEntryPoint",EntryPointBase_O);
 public:
   CLASP_DEFAULT_CTOR CodeEntryPoint_O() {};
 public:
   llvmo::CodeBase_sp _Code;                       //  10 code
 public:
  // Accessors
   CodeEntryPoint_O(FunctionDescription_sp fdesc, llvmo::CodeBase_sp code) : EntryPointBase_O(fdesc), _Code(code) {  };
 public:
   virtual void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup) { SIMPLE_ERROR(BF("Subclass must implement")); };
   void fixupOneCodePointer(snapshotSaveLoad::Fixup* fixup, void** ptr);
   CL_DEFMETHOD llvmo::CodeBase_sp EntryPoint_code() const { return this->_Code; };
 };

 FORWARD(LocalEntryPoint);
 class LocalEntryPoint_O : public CodeEntryPoint_O {
   LISP_CLASS(core,CorePkg,LocalEntryPoint_O,"LocalEntryPoint",CodeEntryPoint_O);
 public:
   void* _EntryPoint;
 public:
  // Accessors
   LocalEntryPoint_O(FunctionDescription_sp fdesc, void* entry_point, llvmo::CodeBase_sp code ) : CodeEntryPoint_O(fdesc,code), _EntryPoint(entry_point) {};
 public:
   virtual void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup );
   virtual Pointer_sp defaultEntryAddress() const;
};

FORWARD(LocalEntryPointGenerator);
class LocalEntryPointGenerator_O : public EntryPointBase_O {
   LISP_CLASS(core,CorePkg,LocalEntryPointGenerator_O,"LocalEntryPointGenerator",EntryPointBase_O);
 public:
  T_sp _entry_point_indices;
 public:
  // Accessors
  LocalEntryPointGenerator_O( FunctionDescription_sp fdesc, T_sp entry_point_indices ) : EntryPointBase_O(fdesc), _entry_point_indices(entry_point_indices) {};
 };

FORWARD(GlobalEntryPoint);
 class GlobalEntryPoint_O : public CodeEntryPoint_O {
   LISP_CLASS(core,CorePkg,GlobalEntryPoint_O,"GlobalEntryPoint",CodeEntryPoint_O);
 public:
   void* _EntryPoints[NUMBER_OF_ENTRY_POINTS];
 public:
  // Accessors
   GlobalEntryPoint_O(FunctionDescription_sp fdesc, void* entry_point, llvmo::CodeBase_sp code) : CodeEntryPoint_O(fdesc, code), _EntryPoints{entry_point} {};
 public:
   virtual void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup );
   virtual Pointer_sp defaultEntryAddress() const;
 };

FORWARD(GlobalEntryPointGenerator);
class GlobalEntryPointGenerator_O : public EntryPointBase_O {
   LISP_CLASS(core,CorePkg,GlobalEntryPointGenerator_O,"GlobalEntryPointGenerator",EntryPointBase_O);
 public:
  T_sp _entry_point_indices;
 public:
  // Accessors
  GlobalEntryPointGenerator_O(FunctionDescription_sp fdesc, T_sp entry_point_indices ) : EntryPointBase_O(fdesc), _entry_point_indices(entry_point_indices) {};
 };


GlobalEntryPoint_sp makeGlobalEntryPointAndFunctionDescription(T_sp functionName,
                                                               claspFunction entryPoint,
                                                               T_sp lambda_list=_Unbound<T_O>(),
                                                               T_sp docstring=_Nil<T_O>(),
                                                               T_sp declares=_Nil<T_O>(),
                                                               T_sp sourcePathname=_Nil<T_O>(),
                                                               int lineno=-1,
                                                               int column=-1,
                                                               int filePos=-1);


FunctionDescription_sp makeFunctionDescription(T_sp functionName,
                                                T_sp lambda_list=_Unbound<T_O>(),
                                                T_sp docstring=_Nil<T_O>(),
                                                T_sp declares=_Nil<T_O>(),
                                                T_sp sourcePathname=_Nil<T_O>(),
                                                int lineno=-1,
                                                int column=-1,
                                                int filePos=-1);


GlobalEntryPoint_sp makeGlobalEntryPoint( FunctionDescription_sp fdesc,
                                          claspFunction      entry_point
                                          );

LocalEntryPoint_sp makeLocalEntryPoint(FunctionDescription_sp fdesc,
                                       claspFunction      entry_point
                                       );

GlobalEntryPoint_sp makeGlobalEntryPointFromGenerator(GlobalEntryPointGenerator_sp ep, void** fptrs);
LocalEntryPoint_sp makeLocalEntryPointFromGenerator(LocalEntryPointGenerator_sp ep, void** fptrs);


GlobalEntryPoint_sp makeGlobalEntryPointCopy(GlobalEntryPoint_sp original, claspFunction entry_point=NULL);
void validateFunctionDescription(const char* filename, size_t lineno, Function_sp function);

};


namespace core {
  /*! Function_O is a Funcallable object that adds no fields to anything that inherits from it
*/
  class Function_O : public General_O {
    LISP_ABSTRACT_CLASS(core,ClPkg,Function_O,"FUNCTION",General_O);
  public:
    CLASP_DEFAULT_CTOR Function_O() {};
  public:
    std::atomic<GlobalEntryPoint_sp>    _EntryPoint;
  public:
    virtual const char *describe() const { return "Function - subclass must implement describe()"; };
    virtual size_t templatedSizeof() const { return sizeof(*this); };
  public:
  Function_O(GlobalEntryPoint_sp ptr)
      : _EntryPoint(ptr)
    {
#ifdef _DEBUG_BUILD
      if (!ptr.generalp()) {
        printf("%s:%d Something other than a function-description pointer was passed to initialize Function_O::_FunctionDescription -> %p\n", __FILE__, __LINE__, ptr.raw_() );
        abort();
      }
#endif
    };
    claspFunction entry() const { return (claspFunction)(this->_EntryPoint.load()->_EntryPoints[0]); }
    virtual FunctionDescription_sp fdesc() const { return this->_EntryPoint.load()->_FunctionDescription; };
    // Rewrite the function-description pointer - used in direct-calls.lsp
    
//    virtual void set_fdesc(FunctionDescription_sp address) { this->_FunctionDescription.store(address); };


    CL_LISPIFY_NAME("core:entry-point");
    CL_DEFMETHOD T_sp entryPoint() const {
      return this->_EntryPoint.load();
    }
    
    CL_LISPIFY_NAME("core:functionName");
    CL_DEFMETHOD virtual T_sp functionName() const {
      return this->fdesc()->functionName();
    }
    CL_DEFMETHOD void setf_functionName(T_sp name) {
      this->fdesc()->setf_functionName(name);
    }
    T_sp docstring() const {
      return this->fdesc()->docstring();
    }
    CL_DEFMETHOD void setf_lambdaList(T_sp lambda_list) {
      this->fdesc()->setf_lambdaList(lambda_list);
    }
    CL_DEFMETHOD T_sp sourcePathname() const {
      return this->fdesc()->sourcePathname();
    }
    void setf_sourcePathname(T_sp sourceFileName) const {
      this->fdesc()->setf_sourcePathname(sourceFileName);
    }
    void setf_docstring(T_sp x) const {
      this->fdesc()->setf_docstring(x);
    }
    void setf_declares(T_sp x) const {
      this->fdesc()->setf_declares(x);
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
      return this->fdesc()->lambdaList();
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
  CLASP_DEFAULT_CTOR Closure_O() {};
public:
  Closure_O(GlobalEntryPoint_sp ep ) : Base(ep) {};
  public:
    virtual const char *describe() const override { return "Closure"; };
    void describeFunction() const;
  };
};


namespace core {
#define SOURCE_INFO core::Fixnum sourceFileInfoHandle, core::Fixnum filePos, core::Fixnum lineno, core::Fixnum column
#define SOURCE_INFO_PASS sourceFileInfoHandle, filePos, lineno, column
  
  class BuiltinClosure_O : public Closure_O {
    LISP_CLASS(core,CorePkg,BuiltinClosure_O,"BuiltinClosure",Closure_O);
  public:
    CLASP_DEFAULT_CTOR BuiltinClosure_O() {};

  public:
    LambdaListHandler_sp _lambdaListHandler;
  public:
  BuiltinClosure_O(GlobalEntryPoint_sp ep)
    : Closure_O(ep), _lambdaListHandler(_Unbound<LambdaListHandler_O>())  {};
  BuiltinClosure_O(GlobalEntryPoint_sp ep, LambdaListHandler_sp llh)
    : Closure_O(ep), _lambdaListHandler(llh)  {};
    void finishSetup(LambdaListHandler_sp llh) {
      this->_lambdaListHandler = llh;
    }
    T_sp closedEnvironment() const override { return _Nil<T_O>(); };
    virtual size_t templatedSizeof() const override { return sizeof(*this); };
    // Fixup the code pointers
    virtual void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup ) { SIMPLE_ERROR(BF("Subclass must implement")); };
    void fixupOneCodePointer( snapshotSaveLoad::Fixup* fixup, void** address, size_t size );
    virtual const char *describe() const override { return "BuiltinClosure"; };
    bool builtinP() const override { return true; };
    T_sp lambdaListHandler() const override { return this->_lambdaListHandler; };
  };

}

namespace core {
  extern LCC_RETURN interpretedClosureEntryPoint(LCC_ARGS_FUNCALL_ELLIPSIS);
  extern LCC_RETURN unboundFunctionEntryPoint(LCC_ARGS_FUNCALL_ELLIPSIS);
  extern LCC_RETURN unboundSetfFunctionEntryPoint(LCC_ARGS_FUNCALL_ELLIPSIS);
   
  class ClosureWithSlots_O final : public core::BuiltinClosure_O::BuiltinClosure_O::Closure_O {
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
    virtual const char *describe() const override { return "CompiledClosure"; };
    virtual size_t templatedSizeof() const override {
      return gctools::sizeof_container<ClosureWithSlots_O>(this->_Slots.size());
    };
  public:
    static ClosureWithSlots_sp make_interpreted_closure(T_sp name, T_sp type, T_sp lambda_list, LambdaListHandler_sp lambda_list_handler, T_sp declares, T_sp docstring, T_sp form, T_sp environment, SOURCE_INFO);
    
    static ClosureWithSlots_sp make_bclasp_closure(T_sp name, claspFunction ptr, T_sp type, T_sp lambda_list, T_sp environment);
    
    static ClosureWithSlots_sp make_cclasp_closure(T_sp name, claspFunction ptr, T_sp type, T_sp lambda_list, SOURCE_INFO);
  public:
  ClosureWithSlots_O(size_t capacity,
                     claspFunction functionPtr,
                     GlobalEntryPoint_sp ep,
                     ClosureType nclosureType)
      : Base(ENSURE_ENTRY_POINT(ep,functionPtr)),
        closureType(nclosureType),
        _Slots(capacity,_Unbound<T_O>(),true) {};
    virtual string __repr__() const override;
    core::T_sp lambdaListHandler() const override {
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
    CL_DEFMETHOD T_sp closedEnvironment() const override {
      ASSERT(this->closureType!=cclaspClosure); // Never call on a cclaspClosure
      return (*this)[ENVIRONMENT_SLOT];
    };      
    CL_DEFMETHOD T_O*& closedEnvironment_rawRef() {
      ASSERT(this->closureType!=cclaspClosure); // Never call on a cclaspClosure
      return (*this)[ENVIRONMENT_SLOT].rawRef_();
    };      
    bool compiledP() const override {
      return (this->closureType!=interpretedClosure);
    }
    bool interpretedP() const override {
      return (this->closureType==interpretedClosure);
    }
    bool openP();
    inline T_sp &operator[](size_t idx) {
      BOUNDS_ASSERT(idx<this->_Slots.length());
      return this->_Slots[idx];
    };
    inline const T_sp &operator[](size_t idx) const {
      BOUNDS_ASSERT(idx<this->_Slots.length());
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
