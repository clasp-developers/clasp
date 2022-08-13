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
  FORWARD(EntryPoint);
  FORWARD(GlobalEntryPoint);
  FORWARD(LocalEntryPoint);
  FORWARD(Function);
  FORWARD(ClosureBase);
  FORWARD(BuiltinClosure);
  FORWARD(Closure);
  FORWARD(BytecodeModule);
  FORWARD(SimpleVector_byte32_t);
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

  /*! Set to something other than NIL to dump functions as they are defined at startup */
  
extern char* global_dump_functions;
  
  /* The following MUST MATCH %function-description% in cmpintrinsics.lisp
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
   std::string __repr__() const;
 };

};

namespace core {

extern std::atomic<uint64_t> global_interpreted_closure_calls;

  /*! Function_O is a Funcallable object that adds no fields to anything that inherits from it
*/
  class Function_O : public General_O {
    LISP_ABSTRACT_CLASS(core,ClPkg,Function_O,"FUNCTION",General_O);
  public:
    CLASP_DEFAULT_CTOR Function_O() {};
    Function_O(EntryPoint_O* ep) : _EntryPoint(EntryPoint_sp((gctools::Tagged)(gctools::tag_general<EntryPoint_O*>(ep)))) {
      ASSERT(!tagged_general_p<EntryPoint_O>(ep)); // on entry should not be tagged
    };
  public:
    std::atomic<EntryPoint_sp>    _EntryPoint;
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
    ClaspXepGeneralFunction entry() const;
    ClaspXep0Function entry_0() const;
    ClaspXep1Function entry_1() const;
    ClaspXep2Function entry_2() const;
    ClaspXep3Function entry_3() const;
    ClaspXep4Function entry_4() const;
    ClaspXep5Function entry_5() const;

    virtual FunctionDescription_sp fdesc() const;
    // Rewrite the function-description pointer - used in direct-calls.lisp
    //  virtual void set_fdesc(FunctionDescription_sp address) { this->_FunctionDescription.store(address); };


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
    
    virtual void __write__(T_sp) const;
    
    Pointer_sp function_pointer() const;
    virtual bool compiledP() const { return false; };
    virtual bool interpretedP() const { return false; };
    virtual bool builtinP() const { return false; };
    virtual T_sp sourcePosInfo() const { return nil<T_O>(); };
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

 FORWARD(EntryPoint);
 class EntryPoint_O : public Function_O {
   LISP_CLASS(core,CorePkg,EntryPoint_O,"EntryPoint",Function_O);
 public:
   CLASP_DEFAULT_CTOR EntryPoint_O() {};
 public:
   FunctionDescription_sp _FunctionDescription;
 public:
  // Accessors
   EntryPoint_O(FunctionDescription_sp fdesc) : Function_O(this), _FunctionDescription(fdesc) {  };
   CL_DEFMETHOD FunctionDescription_sp functionDescription() const { return this->_FunctionDescription; };
   virtual Pointer_sp defaultEntryAddress() const;
 };

 FORWARD(CodeEntryPoint);
 class CodeEntryPoint_O : public EntryPoint_O {
   LISP_CLASS(core,CorePkg,CodeEntryPoint_O,"CodeEntryPoint",EntryPoint_O);
 public:
   CLASP_DEFAULT_CTOR CodeEntryPoint_O() {};
 public:
   T_sp _Code;                       //  10 code
 public:
  // Accessors
   CodeEntryPoint_O(FunctionDescription_sp fdesc, T_sp code) : EntryPoint_O(fdesc), _Code(code) {  };
 public:
   virtual void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup) { SIMPLE_ERROR(("Subclass must implement")); };
   void fixupOneCodePointer(snapshotSaveLoad::Fixup* fixup, void** ptr);
   CL_DEFMETHOD T_sp EntryPoint_code() const { return this->_Code; };
 };

 FORWARD(LocalEntryPoint);
 class LocalEntryPoint_O : public CodeEntryPoint_O {
   LISP_CLASS(core,CorePkg,LocalEntryPoint_O,"LocalEntryPoint",CodeEntryPoint_O);
 public:
   ClaspLocalFunction _EntryPoint;
 public:
  // Accessors
   LocalEntryPoint_O(FunctionDescription_sp fdesc, const ClaspLocalFunction& entry_point, T_sp code );
 public:
   virtual void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup );
   virtual Pointer_sp defaultEntryAddress() const;
   string __repr__() const;
};

FORWARD(LocalEntryPointGenerator);
class LocalEntryPointGenerator_O : public EntryPoint_O {
   LISP_CLASS(core,CorePkg,LocalEntryPointGenerator_O,"LocalEntryPointGenerator",EntryPoint_O);
 public:
  T_sp _entry_point_indices;
 public:
  // Accessors
   LocalEntryPointGenerator_O( FunctionDescription_sp fdesc, T_sp entry_point_indices ) : EntryPoint_O(fdesc), _entry_point_indices(entry_point_indices) {
     //ASSERT(cl__length(entry_point_indices)==1);
   };
  std::string __repr__() const;
 };

FORWARD(GlobalEntryPoint);
 class GlobalEntryPoint_O : public CodeEntryPoint_O {
   LISP_CLASS(core,CorePkg,GlobalEntryPoint_O,"GlobalEntryPoint",CodeEntryPoint_O);
 public:
   /*! A general entry point at 0 and fixed arity entry points from 1...(NUMBER_OF_ENTRY_POINTS-1)
       The arity for each entry point from 1... starts with ENTRY_POINT_ARITY_BEGIN
   */
   ClaspXepFunction _EntryPoints;
   T_sp _localEntryPoint;
 public:
  // Accessors
   GlobalEntryPoint_O(FunctionDescription_sp fdesc, const ClaspXepFunction& entry_point, T_sp code, T_sp localEntryPoint );
 public:
   virtual void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup );
   virtual Pointer_sp defaultEntryAddress() const;
   T_mv sectionedEntryInfo() const;
   T_sp lineTable() const;
   llvmo::ObjectFile_sp code() const;
   T_sp localEntryPoint() const;
   string __repr__() const;
 };


// Fulfill the role of bytecode_function
FORWARD(GlobalBytecodeEntryPoint);
 class GlobalBytecodeEntryPoint_O : public CodeEntryPoint_O {
   LISP_CLASS(core,CorePkg,GlobalBytecodeEntryPoint_O,"GlobalBytecodeEntryPoint",CodeEntryPoint_O);
 public:
   /*! A general entry point at 0 and fixed arity entry points from 1...(NUMBER_OF_ENTRY_POINTS-1)
       The arity for each entry point from 1... starts with ENTRY_POINT_ARITY_BEGIN
   */
   // All code entry points
   ClaspXepFunction _EntryPoints;
   // The frame size this function needs for local variables.
   unsigned short   _LocalsFrameSize;
  // Number of required args.
   unsigned short   _Required;
  // Number of optional args.
   unsigned short   _Optional;
  // The frame slot for the &REST arg if there is one.
   unsigned short   _RestSlot;
  // A fixed offset (to be added to key-consts) for where the stack slots for the keyword arguments are.
   unsigned short    _KeyStart;
  // An array of named &KEY symbols.
   T_sp             _KeyConsts;
  // Packed flags for:
  //  1. If lambda list has &REST
  //  2. If lambda list has &KEY
  //  3. If lambda list has &ALLOW-OTHER-KEYS
   unsigned char    _Flags;
  // Number of closure values in the environment
   unsigned int     _EnvironmentSize;
  // Entry points into the bytes vector in the containing module.
  // These are offsets instead of an interior pointers to make dumping/loading/GC considerations easier.
   unsigned int     _EntryPcs[NUMBER_OF_ENTRY_POINTS];
 public:
  // Accessors
   GlobalBytecodeEntryPoint_O(FunctionDescription_sp fdesc,
                              const ClaspXepFunction& entry_point,
                              T_sp code,
                              unsigned short localsFrameSize,
                              unsigned short required,
                              unsigned short optional,
                              unsigned short restSlot,
                              unsigned short keyStart,
                              T_sp keyConsts,
                              unsigned char flags,
                              unsigned int environmentSize,
                              unsigned int entryPcs[NUMBER_OF_ENTRY_POINTS] );

 public:
   virtual void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup );
   virtual Pointer_sp defaultEntryAddress() const;
   BytecodeModule_sp code() const;
   string __repr__() const;

   CL_DEFMETHOD Fixnum localsFrameSize() const { return this->_LocalsFrameSize; };
   CL_DEFMETHOD Fixnum required() const { return this->_Required; };
   CL_DEFMETHOD Fixnum optional() const { return this->_Optional; };
   CL_DEFMETHOD Fixnum restSlot() const { return this->_RestSlot; };
   CL_DEFMETHOD Fixnum keyStart() const { return this->_KeyStart; };
   CL_DEFMETHOD T_sp keyConsts() const { return this->_KeyConsts; };
   CL_DEFMETHOD Fixnum flags() const { return this->_Flags; };
   CL_DEFMETHOD Fixnum environmentSize() const { return this->_EnvironmentSize; };
   SimpleVector_byte32_t_sp entryPcs() const;

 };


FORWARD(GlobalEntryPointGenerator);
class GlobalEntryPointGenerator_O : public EntryPoint_O {
   LISP_CLASS(core,CorePkg,GlobalEntryPointGenerator_O,"GlobalEntryPointGenerator",EntryPoint_O);
 public:
  T_sp _entry_point_indices;
  size_t _localEntryPointIndex;
 public:
  // Accessors
  GlobalEntryPointGenerator_O(FunctionDescription_sp fdesc, T_sp entry_point_indices, size_t lepIndex) : EntryPoint_O(fdesc), _entry_point_indices(entry_point_indices), _localEntryPointIndex(lepIndex) {};
  std::string __repr__() const;
  size_t localEntryPointIndex() const;
 };

FunctionDescription_sp makeFunctionDescription(T_sp functionName,
                                                     T_sp lambda_list=unbound<T_O>(),
                                                     T_sp docstring=nil<T_O>(),
                                                     T_sp declares=nil<T_O>(),
                                                     T_sp sourcePathname=nil<T_O>(),
                                                     int lineno=-1,
                                                     int column=-1,
                                                     int filePos=-1);


LocalEntryPoint_sp makeLocalEntryPoint(FunctionDescription_sp fdesc,
                                       const ClaspLocalFunction& entry_point
                                       );

GlobalEntryPoint_sp makeGlobalEntryPoint( FunctionDescription_sp fdesc,
                                          const ClaspXepFunction& entry_point,
                                          T_sp lep
                                          );

template <typename Wrapper>
GlobalEntryPoint_sp templated_makeGlobalEntryPoint(FunctionDescription_sp fdesc, T_sp lep) {
  ClaspXepFunction xep;
  xep.setup<Wrapper>();
  return makeGlobalEntryPoint( fdesc, xep, lep );
}


GlobalEntryPoint_sp makeGlobalEntryPointCopy(GlobalEntryPoint_sp original, const ClaspXepFunction& = ClaspXepFunction() );

template <typename Wrapper>
GlobalEntryPoint_sp templated_makeGlobalEntryPointCopy(GlobalEntryPoint_sp original) {
  ClaspXepFunction xep;
  xep.setup<Wrapper>();
  return makeGlobalEntryPointCopy(original,xep);
}


template <typename Wrapper>
GlobalEntryPoint_sp makeGlobalEntryPointAndFunctionDescription(T_sp functionName,
                                                               T_sp localEntryPoint,
                                                               T_sp lambda_list=unbound<T_O>(),
                                                               T_sp docstring=nil<T_O>(),
                                                               T_sp declares=nil<T_O>(),
                                                               T_sp sourcePathname=nil<T_O>(),
                                                               int lineno=-1,
                                                               int column=-1,
                                                               int filePos=-1) {
  FunctionDescription_sp fdesc = makeFunctionDescription(functionName,
                                                         lambda_list,
                                                         docstring,
                                                         declares,
                                                         sourcePathname,
                                                         lineno,
                                                         column,
                                                         filePos );
  return templated_makeGlobalEntryPoint<Wrapper>(fdesc,localEntryPoint);
};



GlobalBytecodeEntryPoint_sp core__makeGlobalBytecodeEntryPoint(FunctionDescription_sp fdesc,
                                                               BytecodeModule_sp module,
                                                               size_t localsFrameSize,
                                                               size_t required,
                                                               size_t optional,
                                                               size_t restSlot,
                                                               size_t keyStart,
                                                               T_sp keyConsts,
                                                               size_t flags,
                                                               size_t environmentSize,
                                                               List_sp pcIndices );


GlobalEntryPoint_sp makeGlobalEntryPointFromGenerator(GlobalEntryPointGenerator_sp ep, gctools::GCRootsInModule* roots, void** fptrs);
LocalEntryPoint_sp makeLocalEntryPointFromGenerator(LocalEntryPointGenerator_sp ep, void** fptrs);


void validateFunctionDescription(const char* filename, size_t lineno, Function_sp function);

};



namespace core {
  extern bool cl__stringp(T_sp obj);
  extern void lisp_error_sprintf(const char* file, int line, const char* fmt, ...);
  
SMART(LambdaListHandler);
};

namespace core {
//#define SOURCE_INFO_PASS sourceFileInfoHandle, filePos, lineno, column
  
  class BuiltinClosure_O : public Function_O {
    LISP_CLASS(core,CorePkg,BuiltinClosure_O,"BuiltinClosure",Function_O);
  public:
    CLASP_DEFAULT_CTOR BuiltinClosure_O() {};
  public:
    LambdaListHandler_sp _lambdaListHandler;
  public:
  BuiltinClosure_O(GlobalEntryPoint_sp ep)
    : Function_O(ep), _lambdaListHandler(unbound<LambdaListHandler_O>())  {};
  BuiltinClosure_O(GlobalEntryPoint_sp ep, LambdaListHandler_sp llh)
    : Function_O(ep), _lambdaListHandler(llh)  {};
    void finishSetup(LambdaListHandler_sp llh) {
      this->_lambdaListHandler = llh;
    }
    T_sp closedEnvironment() const override { return nil<T_O>(); };
    virtual size_t templatedSizeof() const override { return sizeof(*this); };
    // Fixup the code pointers
    void fixupOneCodePointer( snapshotSaveLoad::Fixup* fixup, void** address, size_t size );
    virtual const char *describe() const override { return "BuiltinClosure"; };
    bool builtinP() const override { return true; };
    T_sp lambdaListHandler() const override { return this->_lambdaListHandler; };
  };

}

namespace core {

  class Closure_O final : public core::Function_O {
    LISP_CLASS(core,CorePkg,Closure_O,"Closure",core::Function_O);
    typedef enum { bytecodeClosure, interpretedClosure, bclaspClosure, cclaspClosure } ClosureType;
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
      return gctools::sizeof_container<Closure_O>(this->_Slots.size());
    };
  public:
    static Closure_sp make_interpreted_closure(T_sp name, T_sp type, T_sp lambda_list, LambdaListHandler_sp lambda_list_handler, T_sp declares, T_sp docstring, T_sp form, T_sp environment, core::Fixnum sourceFileInfoHandle, core::Fixnum filePos, core::Fixnum lineno, core::Fixnum column);

    static Closure_sp make_bytecode_closure(GlobalBytecodeEntryPoint_sp entryPoint, size_t closedOverSlots);

    static Closure_sp make_bclasp_closure(T_sp name, const ClaspXepFunction& ptr, T_sp type, T_sp lambda_list, T_sp environment, T_sp localEntryPoint);

    static Closure_sp make_cclasp_closure(T_sp name, const ClaspXepFunction& ptr, T_sp type, T_sp lambda_list, T_sp localEntryPoint, core::Fixnum sourceFileInfoHandle, core::Fixnum filePos, core::Fixnum lineno, core::Fixnum column );
  public:
    Closure_O(size_t capacity,
                       GlobalEntryPoint_sp ep,
                       ClosureType nclosureType)
        : Base(ep),
          closureType(nclosureType),
          _Slots(capacity,unbound<T_O>(),true) {};
    virtual string __repr__() const override;
    core::T_sp lambdaListHandler() const override {
      switch (this->closureType) {
      case interpretedClosure:
          return (*this)[INTERPRETED_CLOSURE_LAMBDA_LIST_HANDLER_SLOT];
      case bytecodeClosure:
          return nil<T_O>();
      case bclaspClosure:
          return nil<T_O>();
      case cclaspClosure:
          return nil<T_O>();
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
  void core__closure_slots_dump(Function_sp func);

};

namespace core {
#define LCC_FUNCALL
#include <clasp/core/lispCallingConvention.h>
#undef LCC_FUNCALL
};


namespace core {
typedef gctools::return_type (*trampoline_function)(void* fn, core::T_O* closure, size_t nargs, core::T_O** args );
extern trampoline_function interpreter_trampoline;

typedef gctools::return_type (*bytecode_trampoline_function)(void* fn, unsigned char* pc, core::T_O* closure, size_t nargs, core::T_O** args );
extern bytecode_trampoline_function bytecode_trampoline;

};

#endif
