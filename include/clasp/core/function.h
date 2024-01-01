#pragma once

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
FORWARD(SimpleVector_byte32_t);
}; // namespace core

namespace llvmo {
FORWARD(ObjectFile);
};

template <> struct gctools::GCInfo<core::Function_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

#ifdef DEBUG_FUNCTION_CALL_COUNTER
#define INCREMENT_FUNCTION_CALL_COUNTER(x) ++x->_TimesCalled
#else
#define INCREMENT_FUNCTION_CALL_COUNTER(x)
#endif

template <> struct gctools::GCInfo<core::GlobalSimpleFun_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = collectable_immobile;
};

template <> struct gctools::GCInfo<core::LocalSimpleFun_O> {
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
  LISP_CLASS(core, CorePkg, FunctionDescription_O, "FunctionDescription", General_O);

public:
  /* vtable */          //  1 vtable from General_O
  T_sp _functionName;   //  2 function-name
  T_sp _sourcePathname; //  3 source-info
  T_sp _lambdaList;     //  4 lambda-list
  T_sp _docstring;      //  5 docstring
  T_sp _declares;       //  6 declares
  int lineno;           //  7 lineno
  int column;           //  8 column
  int filepos;          //  9 filepos
public:
  FunctionDescription_O(){};

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

}; // namespace core

namespace core {

/*! Function_O is a Funcallable object that adds no fields to anything that inherits from it
 */
class Function_O : public General_O {
  LISP_ABSTRACT_CLASS(core, ClPkg, Function_O, "FUNCTION", General_O);

public:
  CLASP_DEFAULT_CTOR Function_O(){};
  Function_O(SimpleFun_O* ep) : _TheSimpleFun(SimpleFun_sp((gctools::Tagged)(gctools::tag_general<SimpleFun_O*>(ep)))) {
    ASSERT(!gctools::tagged_generalp<SimpleFun_O*>(ep)); // on entry should not be tagged
  };

public:
  std::atomic<SimpleFun_sp> _TheSimpleFun;

public:
  virtual size_t templatedSizeof() const { return sizeof(*this); };

public:
  Function_O(SimpleFun_sp ptr) : _TheSimpleFun(ptr) {
#ifdef _DEBUG_BUILD
    if (!ptr.generalp()) {
      printf("%s:%d Something other than a function-description pointer was passed to initialize Function_O::_FunctionDescription "
             "-> %p\n",
             __FILE__, __LINE__, ptr.raw_());
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
    SimpleFun_sp ep = this->_TheSimpleFun.load(std::memory_order_relaxed);
    ASSERT(ep.generalp());
    return ep;
  }

  CL_DEFMETHOD void setSimpleFun(SimpleFun_sp ep) {
    ASSERT(ep.generalp());
    return this->_TheSimpleFun.store(ep, std::memory_order_relaxed);
  }

  CL_LISPIFY_NAME("core:functionName");
  CL_DEFMETHOD virtual T_sp functionName() const { return this->fdesc()->functionName(); }
  CL_DEFMETHOD void setf_functionName(T_sp name) { this->fdesc()->setf_functionName(name); }
  T_sp docstring() const { return this->fdesc()->docstring(); }
  CL_DEFMETHOD void setf_lambdaList(T_sp lambda_list) { this->fdesc()->setf_lambdaList(lambda_list); }
  CL_DEFMETHOD T_sp sourcePathname() const { return this->fdesc()->sourcePathname(); }
  void setf_sourcePathname(T_sp sourceFileName) const { this->fdesc()->setf_sourcePathname(sourceFileName); }
  void setf_docstring(T_sp x) const { this->fdesc()->setf_docstring(x); }
  void setf_declares(T_sp x) const { this->fdesc()->setf_declares(x); }
  size_t filePos() const { return this->fdesc()->filepos; }
  void setf_filePos(int filePos) { this->fdesc()->filepos = filePos; };
  int lineNumber() const { return this->fdesc()->lineno; }
  int lineno() const { return this->fdesc()->lineno; }
  void setf_lineno(int lineno) { this->fdesc()->lineno = lineno; };
  virtual int column() const { return this->fdesc()->column; }
  void setf_column(int x) { this->fdesc()->column = x; };

  virtual void __write__(T_sp) const;

  Pointer_sp function_pointer() const;
  virtual bool compiledP() const;
  virtual bool builtinP() const { return false; };
  virtual T_sp sourcePosInfo() const { return nil<T_O>(); };
  CL_DEFMETHOD T_sp functionLambdaListHandler() const { return this->lambdaListHandler(); }
  T_sp setSourcePosInfo(T_sp sourceFile, size_t filePos, int lineno, int column);
  virtual T_mv functionSourcePos() const;
  virtual T_sp lambdaListHandler() const { SUBIMP(); };
  virtual T_sp lambdaList() const { return this->fdesc()->lambdaList(); }
  virtual string __repr__() const;
  virtual ~Function_O(){};
};
}; // namespace core

namespace core {

FORWARD(SimpleFun);
class SimpleFun_O : public Function_O {
  LISP_CLASS(core, CorePkg, SimpleFun_O, "SimpleFun", Function_O);

public:
  CLASP_DEFAULT_CTOR SimpleFun_O(){};

public:
  FunctionDescription_sp _FunctionDescription;
  T_sp _Code; //  10 code
public:
  // Accessors
  SimpleFun_O(FunctionDescription_sp fdesc, T_sp code)
    : Function_O(this), _FunctionDescription(fdesc), _Code(code){};
  CL_DEFMETHOD FunctionDescription_sp functionDescription() const { return this->_FunctionDescription; };
  virtual Pointer_sp defaultEntryAddress() const;

public:
  virtual void fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup) {
    printf("%s:%d:%s Subclass must implement\n", __FILE__, __LINE__, __FUNCTION__);
    abort();
  }
  void fixupOneCodePointer(snapshotSaveLoad::Fixup* fixup, void** ptr);
  CL_DEFMETHOD T_sp SimpleFun_code() const { return this->_Code; };
};

FORWARD(LocalSimpleFun);
class LocalSimpleFun_O : public SimpleFun_O {
  LISP_CLASS(core, CorePkg, LocalSimpleFun_O, "LocalSimpleFun", SimpleFun_O);

public:
  ClaspLocalFunction _Entry;

public:
  // Accessors
  LocalSimpleFun_O(FunctionDescription_sp fdesc, const ClaspLocalFunction& entry_point, T_sp code);

public:
  virtual void fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup);
  virtual Pointer_sp defaultEntryAddress() const;
  string __repr__() const;
};

// This and GlobalSimpleFunGenerator are used in FASLs to indicate functions.
// The loader will create actual local/global simple funs based on these generators.
FORWARD(LocalSimpleFunGenerator);
class LocalSimpleFunGenerator_O : public General_O {
  LISP_CLASS(core, CorePkg, LocalSimpleFunGenerator_O, "LocalSimpleFunGenerator", General_O);

public:
  FunctionDescription_sp _FunctionDescription;
  T_sp _entry_point_indices;

public:
  // Accessors
  LocalSimpleFunGenerator_O(FunctionDescription_sp fdesc, T_sp entry_point_indices)
      : _FunctionDescription(fdesc), _entry_point_indices(entry_point_indices){
                                // ASSERT(cl__length(entry_point_indices)==1);
                            };
  std::string __repr__() const;
  CL_DEFMETHOD FunctionDescription_sp functionDescription() const { return this->_FunctionDescription; };
};

FORWARD(GlobalSimpleFunBase);
class GlobalSimpleFunBase_O : public SimpleFun_O {
  LISP_CLASS(core, CorePkg, GlobalSimpleFunBase_O, "GlobalSimpleFunBase", SimpleFun_O);

public:
  /*! A general entry point at 0 and fixed arity entry points from 1...(NUMBER_OF_ENTRY_POINTS-1)
      The arity for each entry point from 1... starts with ENTRY_POINT_ARITY_BEGIN
  */
  ClaspXepFunction _EntryPoints;

public:
  // Accessors
  GlobalSimpleFunBase_O(FunctionDescription_sp fdesc, const ClaspXepFunction& entry_point, T_sp code);
  GlobalSimpleFunBase_O(){};

public:
  virtual void fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup);
};

FORWARD(GlobalSimpleFun);
class GlobalSimpleFun_O : public GlobalSimpleFunBase_O {
  LISP_CLASS(core, CorePkg, GlobalSimpleFun_O, "GlobalSimpleFun", GlobalSimpleFunBase_O);

public:
  T_sp _localSimpleFun;

public:
  // Accessors
  GlobalSimpleFun_O(FunctionDescription_sp fdesc, const ClaspXepFunction& entry_point, T_sp code, T_sp localSimpleFun);

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
  LISP_CLASS(core, CorePkg, GlobalBytecodeSimpleFun_O, "GlobalBytecodeSimpleFun", GlobalSimpleFunBase_O);

public:
  // The frame size this function needs for local variables.
  unsigned short _LocalsFrameSize;
  // Number of closure values in the environment
  unsigned int _EnvironmentSize;
  // Entry point into the bytes vector in the containing module.
  // This is an offset instead of an interior pointer to make dumping/loading/GC considerations easier.
  unsigned int _EntryPcN;
  // Size of this function in bytes - used for debugging
  unsigned int _BytecodeSize;
  BytecodeTrampolineFunction _Trampoline;
  // How many times this function has been called.
  // Tracked to get data about what functions should be optimized.
  // uint16 is "small" so this will roll over eventually, but we
  // will probably compile it before that point.
  std::atomic<uint16_t> _CallCount = 0;

public:
  // Accessors
  GlobalBytecodeSimpleFun_O(FunctionDescription_sp fdesc, const ClaspXepFunction& entry_point, T_sp code,
                            unsigned short localsFrameSize, unsigned int environmentSize, unsigned int entryPcN,
                            unsigned int bytecodeSize, BytecodeTrampolineFunction trampoline);

public:
  virtual Pointer_sp defaultEntryAddress() const;
  BytecodeModule_sp code() const;
  string __repr__() const;

  bool compiledP() const override { return true; }
  virtual void fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup);

  CL_DEFMETHOD Fixnum localsFrameSize() const { return this->_LocalsFrameSize; };
  CL_DEFMETHOD Fixnum environmentSize() const { return this->_EnvironmentSize; };
  size_t entryPcN() const;
  CL_LISPIFY_NAME(GlobalBytecodeSimpleFun/bytecode-size)
  CL_DEFMETHOD Fixnum bytecodeSize() const { return this->_BytecodeSize; }
  // Used for bytecode debug info; see function.cc
  T_sp start() const;
  T_sp end() const;
  CL_LISPIFY_NAME(GlobalBytecodeSimpleFun/call-count)
  CL_DEFMETHOD Fixnum callCount() const { return this->_CallCount.load(std::memory_order_relaxed); }
  inline void countCall() {
    // We use this instead of ++ to get a weak memory ordering.
    this->_CallCount.fetch_add(1, std::memory_order_relaxed);
  }
};

FORWARD(GlobalSimpleFunGenerator);
class GlobalSimpleFunGenerator_O : public General_O {
  LISP_CLASS(core, CorePkg, GlobalSimpleFunGenerator_O, "GlobalSimpleFunGenerator", General_O);

public:
  FunctionDescription_sp _FunctionDescription;
  T_sp _entry_point_indices;
  size_t _localSimpleFunIndex;

public:
  // Accessors
  GlobalSimpleFunGenerator_O(FunctionDescription_sp fdesc, T_sp entry_point_indices, size_t lepIndex)
      : _FunctionDescription(fdesc), _entry_point_indices(entry_point_indices), _localSimpleFunIndex(lepIndex){};
  std::string __repr__() const;
  size_t localSimpleFunIndex() const;
  CL_DEFMETHOD FunctionDescription_sp functionDescription() const { return this->_FunctionDescription; };
};

FunctionDescription_sp makeFunctionDescription(T_sp functionName, T_sp lambda_list = unbound<T_O>(), T_sp docstring = nil<T_O>(),
                                               T_sp declares = nil<T_O>(), T_sp sourcePathname = nil<T_O>(), int lineno = -1,
                                               int column = -1, int filePos = -1);

LocalSimpleFun_sp makeLocalSimpleFun(FunctionDescription_sp fdesc, const ClaspLocalFunction& entry_point);

GlobalSimpleFun_sp makeGlobalSimpleFun(FunctionDescription_sp fdesc, const ClaspXepFunction& entry_point,
                                       T_sp lep = nil<core::T_O>());

template <typename Wrapper>
GlobalSimpleFun_sp makeGlobalSimpleFunAndFunctionDescription(T_sp functionName, T_sp localSimpleFun,
                                                             T_sp lambda_list = unbound<T_O>(), T_sp docstring = nil<T_O>(),
                                                             T_sp declares = nil<T_O>(), T_sp sourcePathname = nil<T_O>(),
                                                             int lineno = -1, int column = -1, int filePos = -1) {
  FunctionDescription_sp fdesc =
      makeFunctionDescription(functionName, lambda_list, docstring, declares, sourcePathname, lineno, column, filePos);
  return makeGlobalSimpleFun(fdesc, ClaspXepFunction::make<Wrapper>(), localSimpleFun);
};

GlobalBytecodeSimpleFun_sp core__makeGlobalBytecodeSimpleFun(FunctionDescription_sp fdesc, BytecodeModule_sp module,
                                                             size_t localsFrameSize, size_t environmentSize, size_t pcIndex,
                                                             size_t bytecodeSize, Pointer_sp trampoline);

GlobalSimpleFun_sp makeGlobalSimpleFunFromGenerator(GlobalSimpleFunGenerator_sp ep, gctools::GCRootsInModule* roots, void** fptrs);
LocalSimpleFun_sp makeLocalSimpleFunFromGenerator(LocalSimpleFunGenerator_sp ep, void** fptrs);

}; // namespace core

namespace core {
extern bool cl__stringp(T_sp obj);
extern void lisp_error_sprintf(const char* file, int line, const char* fmt, ...);

}; // namespace core

namespace core {

class Closure_O final : public core::Function_O {
  LISP_CLASS(core, CorePkg, Closure_O, "Closure", core::Function_O);

public:
  //! Slots must be the last field
  typedef core::T_sp value_type;

public:
  gctools::GCArray_moveable<value_type> _Slots;

public:
  virtual size_t templatedSizeof() const override { return gctools::sizeof_container<Closure_O>(this->_Slots.size()); };

public:
  static Closure_sp make_bytecode_closure(GlobalBytecodeSimpleFun_sp entryPoint, size_t closedOverSlots);

public:
  Closure_O(size_t capacity, SimpleFun_sp ep) : Base(ep), _Slots(capacity, unbound<T_O>(), true){};
  virtual string __repr__() const override;
  // FIXME: DELETE ME
  core::T_sp lambdaListHandler() const override { return nil<T_O>(); }
  bool compiledP() const override { return true; }
  bool openP();
  inline T_sp& operator[](size_t idx) {
    BOUNDS_ASSERT(idx < this->_Slots.length());
    return this->_Slots[idx];
  };
  inline const T_sp& operator[](size_t idx) const {
    BOUNDS_ASSERT(idx < this->_Slots.length());
    return this->_Slots[idx];
  };
};

}; // namespace core

namespace core {

FORWARD(FunctionCell)
class FunctionCell_O : public Function_O {
  LISP_CLASS(core, CorePkg, FunctionCell_O, "FunctionCell", Function_O);

public:
  FunctionCell_O(GlobalSimpleFun_sp ep, Function_sp function) : Base(ep), _Function(function) {}

public:
  std::atomic<Function_sp> _Function;

public:
  static FunctionCell_sp make(T_sp name, Function_sp initial);
  static FunctionCell_sp make(T_sp name); // unbound
public:
  Function_sp real_function() const {
    // relaxed because nobody should be synchronizing on this,
    // but in practice it's probably irrelevant what we do?
    return this->_Function.load(std::memory_order_relaxed);
  }
  void real_function_set(Function_sp fun) { this->_Function.store(fun, std::memory_order_relaxed); }
  void fmakunbound(T_sp name);
  bool fboundp();
  // like real_function() but signals an error if we are un-fbound.
  Function_sp fdefinition() const;

public:
  // probably unnecessary
  virtual bool compiledP() const { return this->real_function()->compiledP(); }

public:
  static inline LCC_RETURN entry_point_n(core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args) {
    SETUP_CLOSURE(FunctionCell_O, closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    DO_DRAG_CXX_CALLS();
    // We need to be sure to load the real function only once to avoid race conditions.
    Function_sp funcallable_closure = closure->real_function();
    GlobalSimpleFunBase_sp simpleFun = gc::As_assert<GlobalSimpleFunBase_sp>(funcallable_closure->_TheSimpleFun.load());
    ClaspXepGeneralFunction entry_point = (ClaspXepGeneralFunction)simpleFun->_EntryPoints[0];
    return (entry_point)(funcallable_closure.raw_(), lcc_nargs, lcc_args);
  }

  static inline LCC_RETURN entry_point_0(core::T_O* lcc_closure) {
    SETUP_CLOSURE(FunctionCell_O, closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    DO_DRAG_CXX_CALLS();
    Function_sp funcallable_closure = closure->real_function();
    const ClaspXepFunction& xep = gc::As_assert<GlobalSimpleFunBase_sp>(funcallable_closure->_TheSimpleFun.load())->_EntryPoints;
    return xep.invoke_0(funcallable_closure.raw_());
  }

  static inline LCC_RETURN entry_point_1(core::T_O* lcc_closure, core::T_O* lcc_farg0) {
    SETUP_CLOSURE(FunctionCell_O, closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    DO_DRAG_CXX_CALLS();
    Function_sp funcallable_closure = closure->real_function();
    const ClaspXepFunction& xep = gc::As_assert<GlobalSimpleFunBase_sp>(funcallable_closure->_TheSimpleFun.load())->_EntryPoints;
    return xep.invoke_1(funcallable_closure.raw_(), lcc_farg0);
  }

  static inline LCC_RETURN entry_point_2(core::T_O* lcc_closure, core::T_O* lcc_farg0, core::T_O* lcc_farg1) {
    SETUP_CLOSURE(FunctionCell_O, closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    DO_DRAG_CXX_CALLS();
    Function_sp funcallable_closure = closure->real_function();
    const ClaspXepFunction& xep = gc::As_assert<GlobalSimpleFunBase_sp>(funcallable_closure->_TheSimpleFun.load())->_EntryPoints;
    return xep.invoke_2(funcallable_closure.raw_(), lcc_farg0, lcc_farg1);
  }

  static inline LCC_RETURN entry_point_3(core::T_O* lcc_closure, core::T_O* lcc_farg0, core::T_O* lcc_farg1, core::T_O* lcc_farg2) {
    SETUP_CLOSURE(FunctionCell_O, closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    DO_DRAG_CXX_CALLS();
    Function_sp funcallable_closure = closure->real_function();
    const ClaspXepFunction& xep = gc::As_assert<GlobalSimpleFunBase_sp>(funcallable_closure->_TheSimpleFun.load())->_EntryPoints;
    return xep.invoke_3(funcallable_closure.raw_(), lcc_farg0, lcc_farg1, lcc_farg2);
  }

  static inline LCC_RETURN entry_point_4(core::T_O* lcc_closure, core::T_O* lcc_farg0, core::T_O* lcc_farg1, core::T_O* lcc_farg2,
                                         core::T_O* lcc_farg3) {
    SETUP_CLOSURE(FunctionCell_O, closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    DO_DRAG_CXX_CALLS();
    Function_sp funcallable_closure = closure->real_function();
    const ClaspXepFunction& xep = gc::As_assert<GlobalSimpleFunBase_sp>(funcallable_closure->_TheSimpleFun.load())->_EntryPoints;
    return xep.invoke_4(funcallable_closure.raw_(), lcc_farg0, lcc_farg1, lcc_farg2, lcc_farg3);
  }

  static inline LCC_RETURN entry_point_5(core::T_O* lcc_closure, core::T_O* lcc_farg0, core::T_O* lcc_farg1, core::T_O* lcc_farg2,
                                         core::T_O* lcc_farg3, core::T_O* lcc_farg4) {
    SETUP_CLOSURE(FunctionCell_O, closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    DO_DRAG_CXX_CALLS();
    Function_sp funcallable_closure = closure->real_function();
    const ClaspXepFunction& xep = gc::As_assert<GlobalSimpleFunBase_sp>(funcallable_closure->_TheSimpleFun.load())->_EntryPoints;
    return xep.invoke_5(funcallable_closure.raw_(), lcc_farg0, lcc_farg1, lcc_farg2, lcc_farg3, lcc_farg4);
  }
};

}; // namespace core

namespace core {
void core__closure_slots_dump(Function_sp func);

};

namespace core {
#define LCC_FUNCALL
#include <clasp/core/lispCallingConvention.h>
#undef LCC_FUNCALL
}; // namespace core

namespace core {
typedef gctools::return_type (*bytecode_trampoline_function)(unsigned char* pc, core::T_O* closure, size_t nargs, core::T_O** args);
extern bytecode_trampoline_function bytecode_trampoline;

}; // namespace core
