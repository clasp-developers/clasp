/*
    File: lisp.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
#ifndef Lisp_H //[
#define Lisp_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/object.h>
#include <clasp/core/lispStream.fwd.h>
#include <clasp/core/character.fwd.h>
#include <clasp/core/cons.h>
#include <clasp/core/mpPackage.fwd.h>
#include <clasp/core/numbers.h>
#include <clasp/core/pointer.fwd.h>
#include <clasp/core/hashTable.fwd.h>
#include <clasp/core/sourceFileInfo.fwd.h>
#include <clasp/core/numbers.fwd.h>
#include <clasp/core/bignum.h>
#include <clasp/core/evaluator.fwd.h>
#include <clasp/core/cache.h>
#include <clasp/core/translators.h>
#include <clasp/core/commandLineOptions.h>
#include <clasp/core/loadTimeValues.fwd.h>
#include <clasp/core/readtable.fwd.h>
#include <clasp/core/singleDispatchGenericFunction.fwd.h>

namespace cl {
extern core::Symbol_sp& _sym_eq;
};

namespace mpip {
class Mpi_O;
typedef gc::smart_ptr<Mpi_O> Mpi_sp;
};

namespace clbind {
class ClassRep_O;
typedef gc::smart_ptr<ClassRep_O> ClassRep_sp;
};
namespace core {


extern CommandLineOptions *global_options;

class Bundle;
class CallStack;
SMART(Intrinsic);
SMART(NamedFunction);
SMART(Reader);
SMART(FunctionValueEnvironment);
SMART(Package);
SMART(Path);
SMART(Binder);
SMART(SpecialForm);
SMART(Hierarchy);
SMART(Environment);

 void core__stack_monitor(T_sp fn=_Nil<T_O>());
void af_stackSizeWarning(size_t size);
 T_sp cl__sort(List_sp sequence, T_sp predicate, T_sp key=_Nil<core::T_O>() );
 
List_sp cl__member(T_sp item, T_sp list, T_sp key = _Nil<T_O>(), T_sp test = cl::_sym_eq, T_sp test_not = _Nil<T_O>());
[[noreturn]]void core__invoke_internal_debugger(T_sp condition);

class SymbolClassHolderPair {
public:
  SymbolClassHolderPair(Symbol_sp s, ClassHolder_sp c) : symbol(s), theClassHolder(c){};
  Symbol_sp symbol;
  ClassHolder_sp theClassHolder;
};

/*! A structure that stores the integer byte/word ordering information for this processor.
      It is calculated when the Lisp environment starts up and can be accessed by routines to 
      handle slicing up uint64_t */
class IntegerOrdering //: public gctools::GCIgnoreClass
    {
public:
  bool _BigEndian;
  bool _MostSignificantWordFirst;
  int _mpz_import_word_order;
  int _mpz_import_size;
  int _mpz_import_endian;

  IntegerOrdering() {
    // Setup the contents here in the constructor
    unsigned int endian_test = 1;
    char *endian_test_ptr = (char *)&endian_test;
    this->_mpz_import_endian = 0; // use native endianness (see mpz_import)
    this->_mpz_import_size = sizeof(int);
    if (*endian_test_ptr == 1) {
      this->_BigEndian = false;
      this->_MostSignificantWordFirst = false;
      this->_mpz_import_word_order = -1;
    } else if (*(endian_test_ptr + sizeof(int) - 1) == 1) {
      this->_BigEndian = true;
      this->_MostSignificantWordFirst = true;
      this->_mpz_import_word_order = 1;
    } else {
throw_hard_error("What the heck? - the Endian test failed when starting up Lisp environment");
    }
  }
};

/*! Class for defining new packages and doing package dependent 
     exposure of classes/functions/globals.  You can create more than one
     PackageHolder for a package to expose successive things.
     If you invoke the Ctor and the package hasn't been created then
     it will be created and you must save the Package before
     the Exposer is destructed or the package will also be
     destructed.
     If you invoke the Exposer ctor when the package exists in the
     lisp interpreter then it will be gotten from the lisp interpreter and
     used to expose things.
     For instance, the lisp interpreter creates a CorePackage to expose
     the clasp stuff and then the python interface creates another one and
     gets the Package_sp for the core package from lisp and then 
     exposes the python classes/functions/globals */
 class Exposer_O : public General_O {
   FRIEND_GC_SCANNER(Exposer_O);
   LISP_ABSTRACT_CLASS(core,CorePkg,Exposer_O,"Exposer",General_O);
public:
  typedef enum { candoClasses,
                 candoFunctions,
                 candoGlobals,
                 pythonClasses,
                 pythonFunctions,
                 pythonGlobals } WhatToExpose;
 private:
  // The package is put here
  Package_sp _Package;
  string _PackageName;
public:

  /*! CTor that looks up a Package with packageName and if it
	  doesn't exist it makes it - no nicknames allowed */
  Exposer_O(Lisp_sp lisp, const string &packageName);

  virtual ~Exposer_O();

  /*! Return the packageName */
  string packageName() const { return this->_PackageName; };

  /*! Return the Package that this Exposer holds */
  Package_sp package() const { return this->_Package; };

  /* Evaluate the code that exposes the package Classes/Functions/Globals
	 to Cando-lisp or to Python depending on the value of (what) */
  virtual void expose(core::Lisp_sp lisp, WhatToExpose what) const = 0;
};

template <typename oclass>
class class_;

class Lisp_O {
  friend T_mv core__file_scope(T_sp sourceFile);
  friend gctools::Layout_code* gctools::get_stamp_layout_codes();
  struct GCRoots //: public gctools::HeapRoot
  {
    T_sp _TerminalIO;
    //! Threads
#ifdef CLASP_THREADS
    List_sp _ActiveThreads;
    mutable mp::SharedMutex _ActiveThreadsMutex;
    List_sp _DefaultSpecialBindings;
    mutable mp::SharedMutex _DefaultSpecialBindingsMutex;
#endif
    // FIXME: Remove this mutex - I've switched to thread-safe hash tables
    //        and its not needed
#ifdef CLASP_THREADS
    mutable mp::SharedMutex _SyspropMutex;
#endif
    HashTable_sp _Sysprop;
    // ---------
    //! Associate class names with classes
#ifdef CLASP_THREADS
    //! Protect _ClassTable
    mutable mp::SharedMutex _ClassTableMutex;
#endif
    HashTable_sp _ClassTable;
    Integer_sp _IntegerOverflowAdjust;
    CharacterInfo charInfo;
    gctools::Vec0<core::Symbol_sp> _ClassSymbolsHolderUnshiftedNowhere;
    gctools::Vec0<Instance_sp> staticClassesUnshiftedNowhere;
    gctools::Vec0<Symbol_sp> staticClassSymbolsUnshiftedNowhere;
    gctools::Vec0<Creator_sp> staticInstanceCreatorsUnshiftedNowhere;
    std::atomic<T_sp>    _JITDylibs; // Maintain a list of loaded JITDylibs 
//    DynamicBindingStack _Bindings;
    gctools::Vec0<FileScope_sp> _SourceFiles;
    map<string, int> _SourceFileIndices; // map<string,FileScope_sp> 	_SourceFiles;
#ifdef CLASP_THREADS
    //! Protect _SourceFiles
    mutable mp::SharedMutex _SourceFilesMutex;
#endif

    /*! Store CATCH info */
// THREAD_CHANGE    List_sp _CatchInfo;
    /* The global class table that maps class symbols to classes */
    gctools::Vec0<SymbolClassHolderPair> bootClassTable;
    //	    SymbolDict<Instance_O>		_BootClassTable;
    /*! When compiled files are loaded, they need to create
	      LOAD-TIME-VALUEs and QUOTEd objects using C++ calls at runtime.
	      Those objects are stored here as a map on the compiled file name. */
//    HashTableEqual_sp _LoadTimeValueArrays; // map<string,LoadTimeValues_sp> _LoadTimeValueArrays;
    List_sp _CommandLineArguments;
    //! Package names to packages
    gctools::Vec0<Package_sp> _Packages;
    map<string, int> _PackageNameIndexMap;
#ifdef CLASP_THREADS
    mutable mp::SharedMutex _PackagesMutex;
#endif
#if 0
    HashTableEq_sp _SetfDefinitions;
#ifdef CLASP_THREADS
    mutable mp::SharedMutex _SetfDefinitionsMutex;
#endif
#endif
    bool _MpiEnabled;
    int _MpiRank;
    int _MpiSize;
    mpip::Mpi_sp  _MpiWorld;
    //! Class_map
    gctools::Vec0<clbind::ClassRep_sp> _ClassMap;
    //! Any class defined here needs to be added to predicates.cc::clos__classp
    Instance_sp   _TheClass;
    Instance_sp   _TheBuiltInClass;
    Instance_sp   _TheStandardClass;
    Instance_sp   _TheStructureClass;
    Instance_sp   _TheDerivableCxxClass;
    Instance_sp   _TheClbindCxxClass;
    Package_sp _CorePackage;
    Package_sp _KeywordPackage;
    Package_sp _CommonLispPackage;
    HashTableEq_sp _SpecialForms;
    /*! Store a table of generic functions - should this be a HashTable?  What about (setf XXX) generic functions? Aug2013 */
    HashTableEqual_sp _SingleDispatchGenericFunctionHashTableEqual;
#ifdef CLASP_THREADS
    mutable mp::SharedMutex _SingleDispatchGenericFunctionHashTableEqualMutex;
#endif

#ifdef DEBUG_MONITOR_SUPPORT
    mutable mp::SharedMutex _MonitorMutex;
    std::ofstream _MonitorStream;
#endif
    
    /*! True object */
    T_sp _TrueObject;
    //-----
    DoubleFloat_sp _RehashSize;
    DoubleFloat_sp _RehashThreshold;
    T_sp _NullStream;
    List_sp _ThePathnameTranslations; /* alist */
#ifdef CLASP_THREADS
    mutable mp::SharedMutex _ThePathnameTranslationsMutex;
#endif
    Complex_sp _ImaginaryUnit;
    Complex_sp _ImaginaryUnitNegative;
    Ratio_sp _PlusHalf;
    Ratio_sp _MinusHalf;
    SingleFloat_sp _SingleFloatMinusZero;
    SingleFloat_sp _SingleFloatPlusZero;
    DoubleFloat_sp _DoubleFloatMinusZero;
    DoubleFloat_sp _DoubleFloatPlusZero;
    SingleFloat_sp _SingleFloatOne;
    DoubleFloat_sp _DoubleFloatOne;
#ifdef CLASP_LONG_FLOAT
    LongFloat_sp _LongFloatMinusZero;
    LongFloat_sp _LongFloatPlusZero;
    LongFloat_sp _LongFloatOne;
#endif // ifdef CLASP_LONG_FLOAT
    bool _Booted;
    mutable mp::SharedMutex _UnixSignalHandlersMutex;
    List_sp _UnixSignalHandlers;
    GCRoots();
  };

  friend class IncrementTraceLevel;
  friend class MultipleValues;
  friend class CoreExposer;
  friend class ConditionHandlerManager;
  friend class BootStrapCoreSymbolMap;
  friend T_sp sp_eval_when(List_sp args, T_sp env);
  friend List_sp core__all_source_files();
  template <class oclass>
  friend void define_base_class(Instance_sp co, Instance_sp cob, uint &classesUpdated);
  template <class oclass>
  friend T_sp core__put_sysprop(T_sp key, T_sp area, T_sp value);
  friend T_mv core__get_sysprop(T_sp key, T_sp area);

  friend void core__clear_gfun_hash(T_sp what);
  //	/* disable scrape */ LISP_CLASS(core,CorePkg,Lisp_O,"Lisp",T_O);
public:
  static void initializeGlobals(Lisp_sp lisp);
  
public:
  static void lisp_initSymbols(Lisp_sp lisp);

public:
  static const int MaxFunctionArguments; //<! See ecl/src/c/main.d:163 ecl_make_cache(64,4096)
  static const int SingleDispatchMethodCacheSize;

public:
  void initialize();
public:
  GCRoots _Roots;
  // Trap INTERN of a specific symbol to help resolve symbol conflicts
  bool _TrapIntern;
  std::string _TrapInternPackage;
  std::string _TrapInternName;
  uint _StackWarnSize;
  uint _StackSampleCount;
  uint _StackSampleSize;
  uint _StackSampleMax;

public:
  bool _PrintSymbolsProperly;

public:
  typedef char *Argv[];
  /*!Raw argc*/
  int _Argc;
  /*! Raw argv */
  vector<string> _Argv;

 public:
#ifdef CLASP_THREADS
  void add_process(mp::Process_sp process);
  void remove_process(mp::Process_sp process);
  List_sp processes() const;
  void push_default_special_binding(Symbol_sp sym, T_sp form);
  List_sp copy_default_special_bindings() const;
#endif
public:
  /*! Map source file path strings to FileScope_sp */
  uint _ReplCounter;
  /*! Store paths to important directories */
  Bundle *_Bundle;
  /*! Stores whether the system is big-endian or not */
  IntegerOrdering _IntegerOrdering;
  DebugStream *_DebugStream;
  uint _SingleStepLevel;
  int _TraceLevel;
  int _DebuggerLevel;
  /*! Global environment initialization hooks can be added until
	  the environment is started up.
	*/
  bool _LockGlobalInitialization;
  vector<InitializationCallback> _GlobalInitializationCallbacks;
  bool _Interactive;
  string _FunctionName;
  /*! Define the name of a source file that is evaluated
	 * before everything else to extend the environment
	 */
  string _RCFileName;
  bool _IgnoreInitImage;
  bool _IgnoreInitLsp; // true if the startup shouldn't be loaded
  /*! Keep track of every new environment that is created */
  uint _EnvironmentId;

 public:
  /*! Callbacks for making packages and exporting symbols */
  MakePackageCallback _MakePackageCallback;
  ExportSymbolCallback _ExportSymbolCallback;
  int _RequireLevel;
  bool _CoreBuiltInClassesInitialized;
  bool _BuiltInClassesInitialized;
  bool _PackagesInitialized;
  bool _EnvironmentInitialized;
  bool _NilsCreated;
  string _LastCompileErrorMessage;
  bool _BootClassTableIsValid;
  /*! Store the maximum path length for the system */
  int _PathMax;
  // ------------------------------------------------------------
  // ------------------------------------------------------------
  // ------------------------------------------------------------
  // ------------------------------------------------------------

public:
  /*! Signal a problem if the stack gets too full*/
  inline void stack_monitor() {
    int x;
    char *xaddr = (char *)(&x);
    size_t stack = (size_t)(((const char*)my_thread_low_level->_StackTop) - xaddr);
    if (stack > _lisp->_StackWarnSize) {
      af_stackSizeWarning(stack);
    }
  }

public:
  DebugStream &debugLog() {
    return *(this->_DebugStream);
  };

public:
  uint nextReplCounter() { return ++this->_ReplCounter; };

public:
  void setupMpi(bool mpiEnabled, int mpiRank, int mpiSize);
  bool mpiEnabled() { return this->_Roots._MpiEnabled; }
  int mpiRank() { return this->_Roots._MpiRank; }
  int mpiSize() { return this->_Roots._MpiSize; }

public:
  Str8Ns_sp get_Str8Ns_buffer_string();
  void put_Str8Ns_buffer_string(Str8Ns_sp str);
  StrWNs_sp get_StrWNs_buffer_string();
  void put_StrWNs_buffer_string(StrWNs_sp str);

public:
  IntegerOrdering const &integer_ordering() const { return this->_IntegerOrdering; };

public:
  void mapClassNamesAndClasses(KeyValueMapper *mapper);

public:
  //	List_sp catchPushTag(T_sp tag);
  //	void catchUnwindTag(List_sp catchStore);
  //	List_sp catchFindTag(T_sp tag);
public:
  List_sp pathnameTranslations_() const { return this->_Roots._ThePathnameTranslations; };
  void setPathnameTranslations_(List_sp pnt) { this->_Roots._ThePathnameTranslations = pnt; };
  /*! Return the maximum path length for the system */
  int pathMax() const { return this->_PathMax; };

public:
  bool bootClassTableIsValid() const { return this->_BootClassTableIsValid; };

public:
  CharacterInfo &characterInfo() { return this->_Roots.charInfo; };

public:
  gctools::Vec0<core::Symbol_sp> &classSymbolsHolder() { return this->_Roots._ClassSymbolsHolderUnshiftedNowhere; };

public:
  FileScope_mv getOrRegisterFileScope(const string &fileName);

public:
  /*! Get the LoadTimeValues_sp that corresponds to the name.
	  If it doesn't exist then make one and return it. */
//NEW_LTV  LoadTimeValues_sp getOrCreateLoadTimeValues(const string &name, size_t numberOfLoadTimeValues = 0);
//NEW_LTV  List_sp loadTimeValuesIds() const;
//NEW_LTV  T_sp loadTimeValue(const string &name, int idx);
//NEW_LTV  Symbol_sp loadTimeSymbol(const string &name, int idx);
//NEW_LTV  LoadTimeValues_sp findLoadTimeValues(const string &name);
  //NEW_LTV LoadTimeValues_sp findLoadTimeValuesWithNameContaining(const string &name, int &count);

public:
  /*! Keep track of every source file that is read by the system */
  //	FileScope_sp getSourceFileInfo(const string& fileName);
  /*! Maintain a database of every source file read by the system */
  void setFileScope(const string &fileName, FileScope_sp fileInfo);

public:
  /*! Takes the place of ECL trap_fpe_bits - for now trap everything */
  int trapFpeBits() { return ~0; };

public:
#if 0
	void printvWrite(const char* buffer);
	void printvWriteChar(char c);
	void printvFlush();
	void printvSetLinePrefix(const string& prompt) { this->_PrintvLinePrefix = prompt;};
#endif
public: // numerical constants
  Complex_sp imaginaryUnit() const { return this->_Roots._ImaginaryUnit; };
  Complex_sp imaginaryUnitNegative() const { return this->_Roots._ImaginaryUnitNegative; };
  Ratio_sp plusHalf() const { return this->_Roots._PlusHalf; };
  Ratio_sp minusHalf() const { return this->_Roots._MinusHalf; };
  SingleFloat_sp singleFloatMinusZero() const { return this->_Roots._SingleFloatMinusZero; };
  SingleFloat_sp singleFloatPlusZero() const { return this->_Roots._SingleFloatPlusZero; };
  DoubleFloat_sp doubleFloatMinusZero() const { return this->_Roots._DoubleFloatMinusZero; };
  DoubleFloat_sp doubleFloatPlusZero() const { return this->_Roots._DoubleFloatPlusZero; };
  SingleFloat_sp singleFloatOne() const { return this->_Roots._SingleFloatOne; };
  DoubleFloat_sp doubleFloatOne() const { return this->_Roots._DoubleFloatOne; };
#ifdef CLASP_LONG_FLOAT
  LongFloat_sp longFloatMinusZero() const { return this->_Roots._LongFloatMinusZero; };
  LongFloat_sp longFloatPlusZero() const { return this->_Roots._LongFloatPlusZero; };
  LongFloat_sp longFloatOne() const { return this->_Roots._LongFloatOne; };
#endif // ifdef CLASP_LONG_FLOAT
public:
  Cache_sp singleDispatchMethodCachePtr() const { return my_thread->_SingleDispatchMethodCachePtr; };
public:
  /*! Setup makePackage and exportSymbol callbacks */
  void setMakePackageAndExportSymbolCallbacks(MakePackageCallback mpc, ExportSymbolCallback esc);

public:
  bool isSingleStepOn() { return this->_SingleStepLevel != UndefinedUnsignedInt; };
  void setSingleStepLevel(uint level) { this->_SingleStepLevel = level; };
  uint getSingleStepLevel() const { return this->_SingleStepLevel; };

public:
  List_sp allPackagesAsCons() const;

public:
  /*! Add a function to trace */
  void add_trace(Function_sp func);
  /*! Remove a function to trace */
  void remove_trace(Function_sp func);

  /*! Return a list of functions being traced */
  List_sp trace_functions() const;

public:
//  inline DynamicBindingStack &bindings() { return this->_Roots._Bindings; };

public:
  /*! Add a pair of symbolIDs that provide an accessor get/setf pair */
  void add_accessor_pair(Symbol_sp getter, Symbol_sp setter);
  /*! Add a SetfExpander associated with the symbol */
  //	void addSetfExpander(Symbol_sp symbol, SetfExpander_sp expander);
  /*! Lookup the SetfExpander by symbol - if not found return nil */
  //	SetfExpander_sp lookupSetfExpander(Symbol_sp symbol) const;
protected:
  List_sp getConditionHandlers();
  void setConditionHandlers(List_sp handlers);
  List_sp getRestartHandlers();
  void setRestartHandlers(List_sp handlers);

public:
#if 0
  //THREAD_CHANGE
  Bignum_sp bigRegister0() { return this->_Roots._BignumRegister0; };
  Bignum_sp bigRegister1() { return this->_Roots._BignumRegister1; };
  Bignum_sp bigRegister2() { return this->_Roots._BignumRegister2; };
#endif
Integer_sp integerOverflowAdjust() { return this->_Roots._IntegerOverflowAdjust; };

public:
  bool isEnvironmentInitialized() { return this->_EnvironmentInitialized; };
  uint nextEnvironmentId();

public:
#if defined(XML_ARCHIVE)
  /*! Like read but uses serialization code to deserialize objects from a stream */
  T_sp sread(T_sp sin, bool eofErrorP, T_sp eofValue);
  /*! Like print but uses serialization to serialize objects to a stream */
  void sprint(T_sp obj, T_sp sout);
#endif // defined(XML_ARCHIVE)
public:
  void print(boost::format fmt);
  void prin1(boost::format fmt);

public:
  /*! Create a setfDefinition */
  //	void set_setfDefinition(Symbol_sp fnName, Function_sp fnDef);
  /*! Return the function or nil if not found */
  //	Function_sp get_setfDefinition(Symbol_sp fnName) const;
  /*! Return true if the definition was found */
  //	bool remove_setfDefinition(Symbol_sp fnName);

public:
  void incrementDebuggerLevel() { this->_DebuggerLevel++; };
  void decrementDebuggerLevel() { this->_DebuggerLevel--; };
  int debuggerLevel() const { return this->_DebuggerLevel; };

public:
  /*! This function is called whenever a symbol is exported 
	  it will be used to callback to Python to install python callable functions
	*/
  void exportingSymbol(Symbol_sp sym);

  void dump_apropos(const char *part) const;

  void dump_backtrace(int numcol = 50);

public:
  DoubleFloat_sp rehashSize() const { return this->_Roots._RehashSize; };
  DoubleFloat_sp rehashThreshold() const { return this->_Roots._RehashThreshold; };
  T_sp nullStream() const { return this->_Roots._NullStream; };

public:
  //	void load(T_sp filespec, bool verbose=false, bool print=false, bool ifDoesNotExist=true );
public:
  /*! Constantly updated line number as programs execute
	 */
  uint _LineNumber;

public:
  gctools::Vec0<Package_sp> &packages() { return this->_Roots._Packages; };
  Package_sp corePackage() { return this->_Roots._CorePackage; };
  Package_sp keywordPackage() { return this->_Roots._KeywordPackage; };
  Package_sp commonLispPackage() { return this->_Roots._CommonLispPackage; };

private:
  void parseStringIntoPackageAndSymbolName(const string &name, bool &packageDefined, Package_sp &package, string &symbolName, bool &exported) const;

public:
  //        ActivationFrame_sp& activationFrameNil() { return this->_Roots._ActivationFrameNil; };
  T_sp _true() const { return this->_Roots._TrueObject; };
  T_sp _not(T_sp x) const { return this->_boolean(!x.isTrue()); };
  T_sp _false() const { return _Nil<T_O>(); };
  T_sp _boolean(bool b) const {
    if (b)
      return this->_true();
    return this->_false();
  };

public:
  //	void createHiddenBinder();
  /*! Return the hidden binder
	 */
  //	Binder_sp hiddenBinder();
public:
  /*! Return true if running a graphical environment
	 */
  bool graphical();
  //	void setGraphical(bool g) { this->_Graphical = g;};
public:
  bool CoreBuiltInClassesInitialized() { return this->_CoreBuiltInClassesInitialized; };
  bool BuiltInClassesInitialized() { return this->_BuiltInClassesInitialized; };
  bool NilsCreated() { return this->_NilsCreated; };

public:
  /*! Pass the mpi process rank in (rank) */
  static Lisp_sp createLispEnvironment(bool mpiEnabled, int mpiRank, int mpiSize);

  /*! Call this to setup the lisp environment
	 */
  void startupLispEnvironment(Bundle *bundle);
  /*! Call this to shut down the lisp environment before it is destructed
	 */
  void shutdownLispEnvironment();

  void setCurrentWorkingDirectory(Path_sp path);
  Path_sp getCurrentWorkingDirectory();

  T_sp getCurrentReadTable();

  template <class oclass>
  gctools::smart_ptr<Cons_O> cons(gctools::smart_ptr<oclass> obj, gctools::smart_ptr<Cons_O> tail) {
    return this->create<Cons_O>(obj, tail);
  }
  /*! Zero argument creator */
  template <class oclass>
  gctools::smart_ptr<oclass> create() {
    return oclass::create();
  }
  /*! One argument creator */
  template <class oclass, typename targ1>
  gctools::smart_ptr<oclass> create(targ1 x) {
    gctools::smart_ptr<oclass> res = oclass::create(x);
    return res;
  }
  /*! Two argument creator */
  template <class oclass, typename targ1, typename targ2>
  gctools::smart_ptr<oclass> create(targ1 x, targ2 y) {
    gctools::smart_ptr<oclass> res = oclass::create(x, y);
    return res;
  }
  /*! Three argument creator */
  template <class oclass, typename targ1, typename targ2, typename targ3>
  gctools::smart_ptr<oclass> create(targ1 x, targ2 y, targ3 z) {
    gctools::smart_ptr<oclass> res = oclass::create(x, y, z, _lisp);
    return res;
  }

public:
  Bundle &bundle() { return *this->_Bundle; };

public:
  bool isInteractive() { return this->_Interactive; };
  void setInteractive(bool b) { this->_Interactive = b; };

  /*! Lookup a single-dispatch-ggeneric-function in the _SingleDispatchGenericFunctionHashTableEqual by name
	 If errorp == true then throw an exception if the single-dispatch-generic-function is not
	 found otherwise return nil */
  static SingleDispatchGenericFunctionClosure_sp find_single_dispatch_generic_function(T_sp gfName, bool errorp = true);
  /*! Associate a generic function with a symbol by name */
  static SingleDispatchGenericFunctionClosure_sp setf_find_single_dispatch_generic_function(T_sp gfName, SingleDispatchGenericFunctionClosure_sp gf);
  /*! Clear all generic functions */
  static void forget_all_single_dispatch_generic_functions();
  HashTableEqual_sp singleDispatchGenericFunctionHashTableEqual() const { return this->_Roots._SingleDispatchGenericFunctionHashTableEqual; };

private:
  static void setupSpecialSymbols();
  static void finalizeSpecialSymbols();

public:

  /*! Lookup a class in the _ClassTable by name
	 If errorp == true then throw an exception if the class is not
	found otherwise return nil */
  T_sp boot_findClassHolder(Symbol_sp className, bool errorp = true) const;
  /*! associate a class in the _ClassTable by name */
  Instance_sp boot_setf_findClass(Symbol_sp className, Instance_sp mc);

  /*! Move all _BootClassTable class definitions into a hash-table in *class-name-hash-table* */
  void switchToClassNameHashTable();

public:
  void setBuiltInClassesInitialized(bool b) { this->_BuiltInClassesInitialized = b; };
  void throwIfBuiltInClassesNotInitialized();

  void defineMethod(const string &name, Symbol_sp classSymbol, Function_sp methoid, const string &arguments, const string &docString, bool autoExport);

public:
  Symbol_sp errorUndefinedSymbol(const char *symbolName);

  //	string getRenderFileName() { return this->_RenderFileName; };

public: // Hierarchy stuff
        //	Hierarchy_sp defaultHierarchy() { return this->_DefaultHierarchy;};
        //	void derive(T_sp tag, T_sp parent);
        //	bool isA(T_sp tag, T_sp ancestor);
public:
  //
  // Initialize the packages for this environment
  //
  void initializePackages();

public: // Functions for manipulating special forms
  Symbol_sp defineSpecialOperator(const string &package, const string &formName, SpecialFormCallback cb, const string &args = "", const string &docstring = "");
  T_sp specialFormOrNil(Symbol_sp sym);

public:
  bool recognizesModule(const string &fileName);
  void addModule(const string &fileName);

  int getRequireLevel() { return this->_RequireLevel; };
  void pushRequireLevel() { this->_RequireLevel++; };
  void popRequireLevel() { this->_RequireLevel--; };

  /*! Install a package using the newer Exposer idiom */
  void installPackage(const Exposer_O *package);
  /*! Create nils for all classes that don't have them yet */
  //	void	createNils();
  /*! When global initialization is locked then no more callbacks can be added
	 * and globals can be initialized
	 */
  bool isGlobalInitializationAllowed() { return this->_LockGlobalInitialization; };
  void installGlobalInitializationCallback(InitializationCallback c);

  /*! Find symbol or nil */
  Symbol_sp findSymbol(string const &symbolName, T_sp optionalPackageDesignator) const;

  /*! Find symbol or nil - symbolName can have package:[:]name form */
  Symbol_sp findSymbol(const string &symbolName /*, T_sp optionalPackageDesignator = nil */) const;

  //	void createGlobalMacro(Symbol_sp sym, MacroCallback func);

public:
  void defvar(Symbol_sp sym, T_sp obj);
  void defconstant(Symbol_sp sym, T_sp obj);

  // Environment stuff

  string allLocalNames();
  List_sp allLocalNamesAsCons();

  /*! The core REPL loop.  Set printResults to true if you want
	  each eval result to be printed. Return false if (quit) or (exit) was evaluated.
	  sin - an input stream designator
	*/
  T_mv readEvalPrint(T_sp sin, T_sp environ, bool printResults, bool prompt);
  T_mv readEvalPrintString(const string &string, T_sp environ, bool printResults);
  void readEvalPrintInteractive();

  /*! Find symbol or intern - symbolName can have package:[:]name form	*/
  Symbol_mv intern(const string &symbolName, T_sp optionalPackageDesignator);

  /*! Find symbol or intern - symbolName can have package:[:]name form. */
  Symbol_sp intern(const string &symbolName /*, T_sp optionalPackageDesignator = nil */);

  /*! Find symbol or intern - symbolName can have package:[:]name form. */
  Symbol_sp intern(const string &symbolName, const string &pkgName);

  /*! Intern the symbol into the package with the given name. Warn if the symbol already exists */
  Symbol_sp internUniqueWithPackageName(const string &packageName, const string &symbolName);

  /*! Intern the symbol into the package with the given name */
  Symbol_sp internWithPackageName(const string &packageName, const string &symbolName);

  /*! Intern the symbol - if it doesn't have a package prefix in the symbolName then use packageName as the package.
          If the symbolName has the form XXX:YYY then intern YYY in package XXX and export it.
          If the symbolName has the form XXX::YYY then intern YYY in package XXX as an internal symbol.
          If the symbolName has the form YYY then intern YYY in the package (packageName) as an internal symbol. */
  Symbol_sp internWithDefaultPackageName(const string &defaultPackageName, const string &possiblePackagePrefixedSymbolName);

  /*! Find the keyword symbol with the given name or intern it */
  Symbol_sp internKeyword(const string &keywordName);

  Symbol_sp getClassSymbolForClassName(const string &symbolName);

  //	void setGlobal(Symbol_sp sym, T_sp obj);
  //	void setGlobalIfNotDefined(Symbol_sp sym, T_sp obj);
  //	T_sp valueGlobal(Symbol_sp sym);
  //	bool isGlobalDefined(Symbol_sp sym);

  /*! Push a new variable stack frame
	 */
  //	Binder_sp _pushFrame();
  //	void _pushFrame(Binder_sp);
  //	void _popFrame();
  /*! Return the value currently bound to the symbol
	 */
  //	T_sp value(Symbol_sp sym);
  /*! Set the value of the symbol in the local frame */
  //	void	setLocal(Symbol_sp sym, T_sp obj);

  /*! Update the value of a symbol
	 */
  //	void	update(Symbol_sp sym, T_sp obj);

  /*! Update the value of a symbol or define it in the local namespace
	 * if it doesn't exist
	 */
  //	void	updateOrDefine(Symbol_sp sym, T_sp obj);

  /*! Return true if classSymbol is the id for a class that has the baseClassSymbol
	 */
  //	bool subClassOrder(Symbol_sp baseClassSymbol,Symbol_sp classSymbol);

  bool recognizesPackage(const string &packageName) const;
  T_sp findPackage_no_lock(const string &packageName, bool errorp = false) const;
  T_sp findPackage(const string &packageName, bool errorp = false) const;
  void inPackage(const string &packageName);
  void selectPackage(Package_sp pack);
  Package_sp getCurrentPackage() const;
  void mapNameToPackage(const string &name, Package_sp pkg);
  void unmapNameToPackage(const string &name);
  void finishPackageSetup(const string &packageName, list<string> const &nicknames, list<string> const &usePackages, list<string> const& shadow = {});
  Package_sp makePackage(const string &packageName, list<string> const &nicknames, list<string> const &usePackages, list<string> const& shadow = {});
  void remove_package(const string& package_name);
  bool usePackage(const string &packageName);

  List_sp getBackTrace() const;
  string backTraceAsString(int numcol = 50) const;

  Path_sp translateLogicalPathname(T_sp logicalPathName);

  /*! Return a path to an existing file indicated by logicalPathName.
	  If the file is in the current directory return it.
	  Otherwise use the *PATH* variable to search for paths that could contain the logicalPathName file */
  Path_sp translateLogicalPathnameUsingPaths(T_sp logicalPathName);

  /*! Parse the command line arguments and return a
	 * Cons of code if code was provided on the command line or through a script file
	 * Otherwise return nil
	 */
  void parseCommandLineArguments(int argc, char *argv[], const CommandLineOptions& options );

  List_sp getCommandLineArguments() { return this->_Roots._CommandLineArguments; };

  Intrinsic_sp getIntrinsic(const string &name);
  string getMethodName(uint methodId);
  uint getMethodId(const string &methodName);

public:
  void initializeClassManager();
  void initializeEnvironments();

  /*! Run the program, if returns _ExitStatus != 0 there was an error */
  int run();

  string errorMessage();

  //	void setProgram(List_sp p) { this->_Program = p; };
  //!Wipes out namespace and fills it with new values
  void initializeEnvironment();

  void addClassNameToPackageAsDynamic(const string &package, const string &name, Instance_sp cl);
  void addClassSymbol(Symbol_sp classSymbol, Creator_sp creator, Symbol_sp base1ClassSymbol ); //, Symbol_sp base2ClassSymbol = UNDEFINED_SYMBOL, Symbol_sp base3ClassSymbol = UNDEFINED_SYMBOL);
//  void addClass(Symbol_sp classSymbol, Instance_sp theClass);
  //	void addClass( Symbol_sp classSymbol);

  string __repr__() const;

  /*! From gdb - start trace on functions using function names separated by spaces.
	  If the names string is empty then dump all the functions currently being
	  traced */
  void gdb_trace_by_name(const char *names);

  /*! From gdb - stop trace on functions using function names separated by spaces.
	  if the names string is empty then untrace all functions. */
  void gdb_untrace_by_name(const char *name);

  explicit Lisp_O();
  virtual ~Lisp_O(){};
};


 /*! Use RAII to safely allocate a buffer */
 
struct SafeBufferStr8Ns {
  Str8Ns_sp _Buffer;
  SafeBufferStr8Ns() {
    this->_Buffer = _lisp->get_Str8Ns_buffer_string();
  };
  ~SafeBufferStr8Ns() {
    _lisp->put_Str8Ns_buffer_string(this->_Buffer);
  };
  Str8Ns_sp string() const {return this->_Buffer;};
};

 struct SafeBufferStrWNs {
  StrWNs_sp _Buffer;
  SafeBufferStrWNs() {
    this->_Buffer = _lisp->get_StrWNs_buffer_string();
  };
  ~SafeBufferStrWNs() {
    _lisp->put_StrWNs_buffer_string(this->_Buffer);
  };
  StrWNs_sp string() const {return this->_Buffer;};
};

void initializeLisp();

class LispHolder //: public gctools::StackRoot
    {
private:
  Lisp_sp _Lisp;

public:
  /*! Pass the mpiProcess rank in (rank) or set to 0 if there is only one process */
  LispHolder(bool mpiEnabled, int mpiRank, int mpiSize);

  virtual void startup(int argc, char *argv[], const string &appPathEnvironmentVariable);

  virtual ~LispHolder();
};

class ChangePackage : public gctools::StackBoundClass {
private:
  Package_sp _SavedPackage;

public:
  explicit ChangePackage(Package_sp newPackage);
  virtual ~ChangePackage();
};
};

namespace core {

  void cl__cerror(T_sp cformat, T_sp eformat, List_sp arguments);
  T_mv cl__macroexpand_1(T_sp form, T_sp env);
  T_mv cl__macroexpand(T_sp form, T_sp env);

  List_sp cl__assoc(T_sp item, List_sp alist, T_sp key, T_sp test = cl::_sym_eq, T_sp test_not = _Nil<T_O>());

  T_sp cl__find_class(Symbol_sp symbol, bool errorp = true, T_sp env = _Nil<T_O>());
  T_sp core__setf_find_class(T_sp newValue, Symbol_sp name);

  void cl__error(T_sp err, List_sp initializers);
};

//TRANSLATE(core::Lisp_O);

#define INTERN(x) _lisp->internWithPackageName(x, CurrentPkg)

namespace core {
  void initialize_Lisp_O();
#ifdef DEBUG_MONITOR_SUPPORT
std::string core__monitor_directory();
FILE* monitor_file(const std::string& filename_prefix);
#endif

#ifdef DEBUG_MONITOR
  void monitor_message(const std::string& msg);
#define MONITOR(x) core::monitor_message((x).str());
#else
#define MONITOR(x)
#endif
extern bool global_Started;
};


#endif //]
