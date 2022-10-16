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
  FORWARD(SimpleFun);
  FORWARD(GlobalSimpleFun);
  FORWARD(LocalSimpleFun);
  FORWARD(Function);
  FORWARD(ClosureBase);
  FORWARD(BuiltinClosure);
  FORWARD(Closure);
  FORWARD(BytecodeModule);
  FORWARD(GFBytecodeModule);
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

#ifdef DEBUG_FUNCTION_CALL_COUNTER
#define INCREMENT_FUNCTION_CALL_COUNTER(x) ++x->_TimesCalled
#else
#define INCREMENT_FUNCTION_CALL_COUNTER(x)
#endif

template <>
struct gctools::GCInfo<core::GlobalSimpleFun_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = collectable_immobile;
};

template <>
struct gctools::GCInfo<core::LocalSimpleFun_O> {
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

  /*! Function_O is a Funcallable object that adds no fields to anything that inherits from it
*/
  class Function_O : public General_O {
    LISP_ABSTRACT_CLASS(core,ClPkg,Function_O,"FUNCTION",General_O);
  public:
    CLASP_DEFAULT_CTOR Function_O() {};
    Function_O(SimpleFun_O* ep) : _TheSimpleFun(SimpleFun_sp((gctools::Tagged)(gctools::tag_general<SimpleFun_O*>(ep)))) {
      ASSERT(!gctools::tagged_generalp<SimpleFun_O*>(ep)); // on entry should not be tagged
    };
  public:
    std::atomic<SimpleFun_sp>    _TheSimpleFun;
  public:
    virtual const char *describe() const { return "Function - subclass must implement describe()"; };
    virtual size_t templatedSizeof() const { return sizeof(*this); };
  public:
  Function_O(SimpleFun_sp ptr)
      : _TheSimpleFun(ptr)
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


    CL_DEFMETHOD SimpleFun_sp entryPoint() const {
      SimpleFun_sp ep = this->_TheSimpleFun.load();
      ASSERT(ep.generalp());
      return ep;
    }

    CL_DEFMETHOD void setSimpleFun(SimpleFun_sp ep) {
      ASSERT(ep.generalp());
      return this->_TheSimpleFun.store(ep);
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
    virtual bool compiledP() const;
    virtual bool builtinP() const { return false; };
    virtual T_sp sourcePosInfo() const { return nil<T_O>(); };
    CL_DEFMETHOD T_sp functionLambdaListHandler() const {
      return this->lambdaListHandler();
    }
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

 FORWARD(SimpleFun);
 class SimpleFun_O : public Function_O {
   LISP_CLASS(core,CorePkg,SimpleFun_O,"SimpleFun",Function_O);
 public:
   CLASP_DEFAULT_CTOR SimpleFun_O() {};
 public:
   FunctionDescription_sp _FunctionDescription;
 public:
  // Accessors
   SimpleFun_O(FunctionDescription_sp fdesc) : Function_O(this), _FunctionDescription(fdesc) {  };
   CL_DEFMETHOD FunctionDescription_sp functionDescription() const { return this->_FunctionDescription; };
   virtual Pointer_sp defaultEntryAddress() const;
 };

 FORWARD(CodeSimpleFun);
 class CodeSimpleFun_O : public SimpleFun_O {
   LISP_CLASS(core,CorePkg,CodeSimpleFun_O,"CodeSimpleFun",SimpleFun_O);
 public:
   CLASP_DEFAULT_CTOR CodeSimpleFun_O() {};
 public:
   T_sp _Code;                       //  10 code
 public:
  // Accessors
   CodeSimpleFun_O(FunctionDescription_sp fdesc, T_sp code) : SimpleFun_O(fdesc), _Code(code) {  };
 public:
   virtual void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup) {
     printf("%s:%d:%s Subclass must implement\n", __FILE__, __LINE__, __FUNCTION__ );
     abort();
   }
   void fixupOneCodePointer(snapshotSaveLoad::Fixup* fixup, void** ptr);
   CL_DEFMETHOD T_sp SimpleFun_code() const { return this->_Code; };
 };

 FORWARD(LocalSimpleFun);
 class LocalSimpleFun_O : public CodeSimpleFun_O {
   LISP_CLASS(core,CorePkg,LocalSimpleFun_O,"LocalSimpleFun",CodeSimpleFun_O);
 public:
   ClaspLocalFunction _Entry;
 public:
  // Accessors
   LocalSimpleFun_O(FunctionDescription_sp fdesc, const ClaspLocalFunction& entry_point, T_sp code );
 public:
   virtual void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup );
   virtual Pointer_sp defaultEntryAddress() const;
   string __repr__() const;
};

FORWARD(LocalSimpleFunGenerator);
class LocalSimpleFunGenerator_O : public SimpleFun_O {
   LISP_CLASS(core,CorePkg,LocalSimpleFunGenerator_O,"LocalSimpleFunGenerator",SimpleFun_O);
 public:
  T_sp _entry_point_indices;
 public:
  // Accessors
   LocalSimpleFunGenerator_O( FunctionDescription_sp fdesc, T_sp entry_point_indices ) : SimpleFun_O(fdesc), _entry_point_indices(entry_point_indices) {
     //ASSERT(cl__length(entry_point_indices)==1);
   };
  std::string __repr__() const;
 };

FORWARD(GlobalSimpleFunBase);
 class GlobalSimpleFunBase_O : public CodeSimpleFun_O {
   LISP_CLASS(core,CorePkg,GlobalSimpleFunBase_O,"GlobalSimpleFunBase",CodeSimpleFun_O);
 public:
   /*! A general entry point at 0 and fixed arity entry points from 1...(NUMBER_OF_ENTRY_POINTS-1)
       The arity for each entry point from 1... starts with ENTRY_POINT_ARITY_BEGIN
   */
   ClaspXepFunction _EntryPoints;
 public:
  // Accessors
   GlobalSimpleFunBase_O(FunctionDescription_sp fdesc, const ClaspXepFunction& entry_point, T_sp code );
   GlobalSimpleFunBase_O() {};
 public:
   virtual void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup );
 };


FORWARD(GlobalSimpleFun);
 class GlobalSimpleFun_O : public GlobalSimpleFunBase_O {
   LISP_CLASS(core,CorePkg,GlobalSimpleFun_O,"GlobalSimpleFun",GlobalSimpleFunBase_O);
 public:
   T_sp _localSimpleFun;
 public:
  // Accessors
   GlobalSimpleFun_O(FunctionDescription_sp fdesc, const ClaspXepFunction& entry_point, T_sp code, T_sp localSimpleFun );
 public:
   virtual Pointer_sp defaultEntryAddress() const;
   T_mv sectionedEntryInfo() const;
   T_sp lineTable() const;
   llvmo::ObjectFile_sp code() const;
   T_sp localSimpleFun() const;
   string __repr__() const;
 };


// Fulfill the role of bytecode_function
FORWARD(GlobalBytecodeSimpleFun);
 class GlobalBytecodeSimpleFun_O : public GlobalSimpleFunBase_O {
   LISP_CLASS(core,CorePkg,GlobalBytecodeSimpleFun_O,"GlobalBytecodeSimpleFun",GlobalSimpleFunBase_O);
 public:
   // The frame size this function needs for local variables.
   unsigned short   _LocalsFrameSize;
  // Number of closure values in the environment
   unsigned int     _EnvironmentSize;
  // Entry point into the bytes vector in the containing module.
  // This is an offset instead of an interior pointer to make dumping/loading/GC considerations easier.
   unsigned int     _EntryPcN;
   BytecodeTrampolineFunction _Trampoline;
 public:
  // Accessors
   GlobalBytecodeSimpleFun_O(FunctionDescription_sp fdesc,
                              const ClaspXepFunction& entry_point,
                              T_sp code,
                              unsigned short localsFrameSize,
                              unsigned int environmentSize,
                              unsigned int entryPcN,
                              BytecodeTrampolineFunction trampoline);

 public:
   virtual Pointer_sp defaultEntryAddress() const;
   BytecodeModule_sp code() const;
   string __repr__() const;

   virtual void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup);

   CL_DEFMETHOD Fixnum localsFrameSize() const { return this->_LocalsFrameSize; };
   CL_DEFMETHOD Fixnum environmentSize() const { return this->_EnvironmentSize; };
   size_t entryPcN() const;

 };



FORWARD(GlobalSimpleFunGenerator);
class GlobalSimpleFunGenerator_O : public SimpleFun_O {
   LISP_CLASS(core,CorePkg,GlobalSimpleFunGenerator_O,"GlobalSimpleFunGenerator",SimpleFun_O);
 public:
  T_sp _entry_point_indices;
  size_t _localSimpleFunIndex;
 public:
  // Accessors
  GlobalSimpleFunGenerator_O(FunctionDescription_sp fdesc, T_sp entry_point_indices, size_t lepIndex) : SimpleFun_O(fdesc), _entry_point_indices(entry_point_indices), _localSimpleFunIndex(lepIndex) {};
  std::string __repr__() const;
  size_t localSimpleFunIndex() const;
 };

FunctionDescription_sp makeFunctionDescription(T_sp functionName,
                                                     T_sp lambda_list=unbound<T_O>(),
                                                     T_sp docstring=nil<T_O>(),
                                                     T_sp declares=nil<T_O>(),
                                                     T_sp sourcePathname=nil<T_O>(),
                                                     int lineno=-1,
                                                     int column=-1,
                                                     int filePos=-1);


LocalSimpleFun_sp makeLocalSimpleFun(FunctionDescription_sp fdesc,
                                       const ClaspLocalFunction& entry_point
                                       );

GlobalSimpleFun_sp makeGlobalSimpleFun( FunctionDescription_sp fdesc,
                                          const ClaspXepFunction& entry_point,
                                          T_sp lep=nil<core::T_O>()
                                          );

template <typename Wrapper>
GlobalSimpleFun_sp templated_makeGlobalSimpleFun(FunctionDescription_sp fdesc, T_sp lep) {
  ClaspXepFunction xep;
  xep.setup<Wrapper>();
  return makeGlobalSimpleFun( fdesc, xep, lep );
}


GlobalSimpleFun_sp makeGlobalSimpleFunCopy(GlobalSimpleFun_sp original, const ClaspXepFunction& = ClaspXepFunction() );

template <typename Wrapper>
GlobalSimpleFun_sp templated_makeGlobalSimpleFunCopy(GlobalSimpleFun_sp original) {
  ClaspXepFunction xep;
  xep.setup<Wrapper>();
  return makeGlobalSimpleFunCopy(original,xep);
}


template <typename Wrapper>
GlobalSimpleFun_sp makeGlobalSimpleFunAndFunctionDescription(T_sp functionName,
                                                               T_sp localSimpleFun,
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
  return templated_makeGlobalSimpleFun<Wrapper>(fdesc,localSimpleFun);
};



GlobalBytecodeSimpleFun_sp core__makeGlobalBytecodeSimpleFun(FunctionDescription_sp fdesc,
                                                               BytecodeModule_sp module,
                                                               size_t localsFrameSize,
                                                               size_t environmentSize,
                                                               size_t pcIndex,
                                                               Pointer_sp trampoline);


GlobalSimpleFun_sp makeGlobalSimpleFunFromGenerator(GlobalSimpleFunGenerator_sp ep, gctools::GCRootsInModule* roots, void** fptrs);
LocalSimpleFun_sp makeLocalSimpleFunFromGenerator(LocalSimpleFunGenerator_sp ep, void** fptrs);


};



namespace core {
  extern bool cl__stringp(T_sp obj);
  extern void lisp_error_sprintf(const char* file, int line, const char* fmt, ...);
  
};


namespace core {

  class Closure_O final : public core::Function_O {
    LISP_CLASS(core,CorePkg,Closure_O,"Closure",core::Function_O);
    typedef enum { bytecodeClosure, cclaspClosure } ClosureType;
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
    static Closure_sp make_bytecode_closure(GlobalBytecodeSimpleFun_sp entryPoint, size_t closedOverSlots);

    static Closure_sp make_cclasp_closure(T_sp name, const ClaspXepFunction& ptr, T_sp type, T_sp lambda_list, T_sp localSimpleFun, core::Fixnum sourceFileInfoHandle, core::Fixnum filePos, core::Fixnum lineno, core::Fixnum column );
  public:
    Closure_O(size_t capacity,
                       SimpleFun_sp ep,
                       ClosureType nclosureType)
        : Base(ep),
          closureType(nclosureType),
          _Slots(capacity,unbound<T_O>(),true) {};
    virtual string __repr__() const override;
    core::T_sp lambdaListHandler() const override {
      switch (this->closureType) {
      case bytecodeClosure:
          return nil<T_O>();
      case cclaspClosure:
          return nil<T_O>();
      };
    }
    bool compiledP() const override {
      return true;
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
typedef gctools::return_type (*bytecode_trampoline_function)(unsigned char* pc, core::T_O* closure, size_t nargs, core::T_O** args );
extern bytecode_trampoline_function bytecode_trampoline;

};

#endif
