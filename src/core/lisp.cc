/*
    File: lisp.cc
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
// #define DEBUG_LEVEL_FULL

#include <errno.h>
#include <dlfcn.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <iomanip>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunneeded-internal-declaration"
// #pragma GCC diagnostic ignored "-Wunused-local-typedef"
#include <filesystem>
#include <boost/algorithm/string.hpp>
#include <boost/program_options.hpp>
#pragma GCC diagnostic pop
// #i n c l u d e	"boost/fstream.hpp"
#include <clasp/core/foundation.h>
#include <clasp/gctools/gc_interface.fwd.h>
#include <clasp/gctools/gc_interface.h>
#include <clasp/gctools/source_info.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/mpip/claspMpi.h>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/posixTime.h>
#include <clasp/core/allClSymbols.h>
#include <clasp/core/candoOpenMp.h>
#include <clasp/core/exceptions.h>
#include <clasp/core/primitives.h>
#include <clasp/core/commandLineOptions.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/compiler.h>
#include <clasp/core/lisp.h>
#include <clasp/core/lispList.h>
#include <clasp/core/loadTimeValues.h>
#include <clasp/core/bundle.h>
#include <clasp/core/bformat.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/hashTableEqualp.h>
#include <clasp/core/pointer.h>
#include <clasp/core/cons.h>
#include <clasp/core/documentation.h>
#include <clasp/core/backquote.h>
#include <clasp/core/bformat.h>
#include <clasp/core/extensionPackage.h>
#include <clasp/core/myReadLine.h>
#include <clasp/core/numbers.h>
#include <clasp/core/bits.h>
#include <clasp/core/load.h>
#include <clasp/core/bignum.h>
// #i n c l u d e "setfExpander.h"
#include <clasp/core/ql.h>
#include <clasp/core/array.h>
#include <clasp/core/commonLispPackage.h>
#include <clasp/core/keywordPackage.h>
#include <clasp/core/package.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/sysprop.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/debugger.h>
#include <clasp/core/debugger2.h>
#include <clasp/core/numberToString.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/lispReader.h>
#include <clasp/core/write_object.h>
#include <clasp/core/write_ugly.h>
// #include <clasp/core/clcenv.h>
#include <clasp/core/pathname.h>
#include <clasp/core/print.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/bootStrapCoreSymbolMap.h>
#include <clasp/core/numerics.h>
#include <clasp/core/singleDispatchGenericFunction.h>
#include <clasp/core/designators.h>
#include <clasp/core/unixfsys.h>
#include <clasp/core/sort.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/instance.h>
#include <clasp/core/character.h>
#include <clasp/core/predicates.h>
#include <clasp/core/primitives.h>
#include <clasp/core/package.h>
#include <clasp/core/symbol.h>
#include <clasp/core/null.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/sequence.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/float_to_digits.h>
#include <clasp/core/num_arith.h>
#include <clasp/core/num_co.h>
#include <clasp/core/lispDefinitions.h>
#include <clasp/core/externalObject.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/primitives.h>
#include <clasp/core/readtable.h>
#include <clasp/core/unwind.h> // call_with_variable_bound
#include <clasp/llvmo/intrinsics.h>
#include <clasp/llvmo/code.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/core/wrappers.h>
#ifdef CLASP_THREADS
#include <clasp/core/mpPackage.h>
#endif
#ifdef READLINE
extern "C" char* readline(const char* prompt);
extern "C" void add_history(char* line);
#endif

#ifndef SCRAPING
#define ALL_INITIALIZERS_EXTERN
#include INITIALIZERS_INC_H
#undef ALL_INITIALIZERS_EXTERN
#endif

#ifndef SCRAPING
#define ALL_TERMINATORS_EXTERN
#include TERMINATORS_INC_H
#undef ALL_TERMINATORS_EXTERN
#endif

//
// globals_ definition
//

core::globals_t* globals_;

namespace core {

CommandLineOptions* global_options = NULL;
bool global_initialize_builtin_classes = false;

const int Lisp::MaxFunctionArguments = 64; //<! See ecl/src/c/main.d:163 ecl_make_cache(64,4096)

struct FindApropos : public KeyValueMapper //, public gctools::StackRoot
{
public:
  HashTable_sp _symbols;
  SimpleString_sp _substr;
  FindApropos(SimpleString_sp str) {
    this->_substr = str;
    this->_symbols = HashTable_O::createEq();
  };
  virtual bool mapKeyValue(T_sp key, T_sp value) {
    //    Bignum_sp skey = gc::As<Bignum_sp>(key);
    Symbol_sp svalue = gc::As<Symbol_sp>(value);
    SimpleString_sp symbolName = svalue->symbolName();
    SimpleString_sp upcasedSymbolName = cl__string_upcase(symbolName);
    T_sp pos = core__search_string(this->_substr, 0, this->_substr->length(), upcasedSymbolName, 0, upcasedSymbolName->length());
    if (pos.notnilp()) {
      LOG("    It is apropos");
      this->_symbols->setf_gethash(svalue, nil<T_O>());
    }
    return true;
  }
};

//
// Constructor
//
Lisp::GCRoots::GCRoots()
    : _ClaspJIT(nil<T_O>()), _AllObjectFiles(nil<T_O>()), _AllCodeBlocks(nil<T_O>()), _AllLibraries(nil<T_O>()),
      _AllBytecodeModules(nil<T_O>()),
#ifdef CLASP_THREADS
      _UnboundCellFunctionEntryPoint(unbound<SimpleFun_O>()), _ActiveThreads(nil<T_O>()), _DefaultSpecialBindings(nil<T_O>()),
#endif
      _NullStream(nil<T_O>()), _PrintSymbolsProperly(false), _TheSystemIsUp(false),
      _Started(false) {
  this->_JITDylibs.store(nil<core::T_O>());
  this->_SingleDispatchGenericFunctions.store(nil<core::T_O>());
};

Lisp::Lisp() : _Booted(false), _MpiEnabled(false), _MpiRank(0), _MpiSize(1), _BootClassTableIsValid(true), _TrapFpeBits(FE_DIVBYZERO | FE_INVALID | FE_OVERFLOW) {
  //  this->_Roots._Bindings.reserve(1024); // moved to Lisp::initialize()
}

void Lisp::shutdownLispEnvironment() {
  this->_Booted = false;
  if (globals_->_DebugStream != NULL)
    globals_->_DebugStream->beginNode(DEBUG_TOPLEVEL);
  delete globals_->_Bundle;
  if (globals_->_DebugStream != NULL) {
    globals_->_DebugStream->endNode(DEBUG_TOPLEVEL);
    delete globals_->_DebugStream;
  }
  //  my_thread->destroy_sigaltstack();
}

void Lisp::lisp_initSymbols(LispPtr lisp) {}

/*! Allocations go here
 */
void Lisp::initialize() {
  //  printf("%s:%d Initializing _lisp\n", __FILE__, __LINE__ );

  this->_Roots.charInfo.initialize();
  this->_Roots._SourceFileIndices = HashTable_O::createEqual();
  this->_Roots._PackageNameIndexMap = HashTable_O::createEqual();
  { HashTable_sp pt = HashTable_O::createEqualp();
    pt->setupThreadSafeHashTable();
    this->_Roots._ThePathnameTranslations = pt;
  }
}

template <class oclass> void setup_static_classSymbol(BootStrapCoreSymbolMap const& sidMap) {
  DEPRECATED();
  oclass::___set_static_ClassSymbol(sidMap.find_symbol(oclass::static_packageName(), oclass::static_className()));
}

string dump_instanceClass_info(Instance_sp co, LispPtr prog) {
  stringstream ss;
  ss << "------------------------------------- class" << _rep_(co->_className()) << std::endl;
  ;
  LOG("Dumping info: {}", co->dumpInfo());
  ss << co->dumpInfo();
  return ss.str();
}

void Lisp::setupSpecialSymbols() {
  RAII_DISABLE_INTERRUPTS();
  SimpleBaseString_sp name_nil = SimpleBaseString_O::make("NIL");
  Null_sp symbol_nil = gctools::GC<Null_O>::allocate(name_nil); // ::create_at_boot("NIL");
  if (!gc::IsA<Null_sp>(symbol_nil)) {
    printf("%s:%d:%s The NIL symbol failed gc::IsA<Null_sp>(symbol_nil)\n", __FILE__, __LINE__, __FUNCTION__);
    abort();
  }
  SimpleBaseString_sp name_unbound = SimpleBaseString_O::make("UNBOUND");
  Symbol_sp symbol_unbound = gctools::GC<Symbol_O>::allocate(name_unbound);
  SimpleBaseString_sp name_no_thread_local_binding = SimpleBaseString_O::make("NO-THREAD-LOCAL-BINDING");
  Symbol_sp symbol_no_thread_local_binding = gctools::GC<Symbol_O>::allocate(name_no_thread_local_binding);
  SimpleBaseString_sp name_no_key = SimpleBaseString_O::make("NO_KEY");
  Symbol_sp symbol_no_key = gctools::GC<Symbol_O>::allocate(name_no_key);
  SimpleBaseString_sp name_deleted = SimpleBaseString_O::make("DELETED");
  Symbol_sp symbol_deleted = gctools::GC<Symbol_O>::allocate(name_deleted);
  SimpleBaseString_sp name_same_as_key = SimpleBaseString_O::make("SAME-AS-KEY");
  Symbol_sp symbol_same_as_key = gctools::GC<Symbol_O>::allocate(name_same_as_key);
  // TODO: Ensure that these globals are updated by the garbage collector
  gctools::global_tagged_Symbol_OP_nil = reinterpret_cast<Symbol_O*>(symbol_nil.raw_());
  symbol_unbound->_HomePackage = symbol_nil;
  symbol_no_thread_local_binding->_HomePackage = symbol_nil;
  symbol_no_key->_HomePackage = symbol_nil;
  symbol_deleted->_HomePackage = symbol_nil;
  symbol_same_as_key->_HomePackage = symbol_nil;
}

void Lisp::finalizeSpecialSymbols() {
  Symbol_sp symbol_nil = gctools::smart_ptr<Symbol_O>((gc::Tagged)gctools::global_tagged_Symbol_OP_nil);
  symbol_nil->setf_symbolValue(nil<T_O>());
  symbol_nil->setf_name(SimpleBaseString_O::make("NIL"));
  symbol_nil->setPackage(_lisp->findPackage("COMMON-LISP"));
  symbol_nil->setf_plist(nil<T_O>());
  //    	Symbol_sp symbol_unbound = gctools::smart_ptr<Symbol_O>(gctools::global_Symbol_OP_unbound);
  //    	Symbol_sp symbol_deleted = gctools::smart_ptr<Symbol_O>(gctools::global_Symbol_OP_deleted);
  //    	Symbol_sp symbol_same_as_key = gctools::smart_ptr<Symbol_O>(gctools::global_Symbol_OP_same_as_key);
}

LispPtr Lisp::createLispEnvironment(bool mpiEnabled, int mpiRank, int mpiSize) {
  Lisp::setupSpecialSymbols();
  // Now NIL is defined so we can finish initializing the main thread ThreadLocalState
  my_thread->finish_initialization_main_thread(nil<core::T_O>());
  ::_lisp.thePointer = gctools::RootClassAllocator<Lisp>::allocate().thePointer;
  _lisp->initialize();
  _lisp->setupMpi(mpiEnabled, mpiRank, mpiSize);
  LOG("The lisp environment DebugStream has been created");
  Lisp::finalizeSpecialSymbols();
  return _lisp;
}

void Lisp::setupMpi(bool mpiEnabled, int mpiRank, int mpiSize) {
  this->_MpiEnabled = mpiEnabled;
  this->_MpiRank = mpiRank;
  this->_MpiSize = mpiSize;
}

int global_monitor_pid = 0;
std::string global_monitor_dir = "";

#ifdef DEBUG_MONITOR_SUPPORT
std::string ensure_monitor_directory_exists_no_lock() {
  if (global_monitor_dir == "" && getpid() != global_monitor_pid) {
    struct stat st = {0};
    stringstream sd;
    if (global_monitor_dir == "") {
      global_monitor_dir = "/tmp/";
    }
    sd << global_monitor_dir;
    sd << "clasp-log-" << getpid() << "/";
    std::string dir = sd.str();
    global_monitor_dir = dir;
    if (stat(dir.c_str(), &st) == -1) {
      mkdir(dir.c_str(), 0700);
    }
  }
  return global_monitor_dir;
}
#endif

#ifdef DEBUG_MONITOR_SUPPORT
CL_UNWIND_COOP(true); // ok since e_m_d_e_n_l does not lisp-unwind
DOCGROUP(clasp);
CL_DEFUN std::string core__monitor_directory() {
  WITH_READ_WRITE_LOCK(globals_->_MonitorMutex);
  return ensure_monitor_directory_exists_no_lock();
}
#endif

#ifdef DEBUG_MONITOR_SUPPORT
FILE* monitor_file(const std::string& name) {
  if (my_thread->_MonitorFiles.find(name) != my_thread->_MonitorFiles.end()) {
    return my_thread->_MonitorFiles[name];
  }
  stringstream ss;
  ss << core__monitor_directory();
  ss << name << "-" << my_thread->_Tid;
  FILE* file = fopen(ss.str().c_str(), "w");
  my_thread->_MonitorFiles[name] = file;
  return file;
}
#endif

void ensure_monitor_file_exists_no_lock() {
#ifdef DEBUG_MONITOR
  std::string dir = ensure_monitor_directory_exists_no_lock();
  stringstream ss;
  ss << dir << "log.txt";
  if (_lisp->_Roots._MonitorStream.is_open())
    _lisp->_Roots._MonitorStream.close();
  _lisp->_Roots._MonitorStream.open(ss.str(), std::fstream::out);
  if (_lisp->_Roots._MonitorStream.is_open()) {
    fprintf(stderr, "%s:%d   Opened file %s for logging\n", __FILE__, __LINE__, ss.str().c_str());
    _lisp->_Roots._MonitorStream << "Start logging\n";
    _lisp->_Roots._MonitorStream.flush();
  } else {
    fprintf(stderr, "%s:%d   Could not open file %s for logging\n", __FILE__, __LINE__, ss.str().c_str());
  }
#endif
}

void monitor_message(const std::string& msg) {
#ifdef DEBUG_MONITOR
  WITH_READ_WRITE_LOCK(globals_->_MonitorMutex);
  if (getpid() != global_monitor_pid) {
    ensure_monitor_file_exists_no_lock();
    if (global_monitor_pid != 0) {
      _lisp->_Roots._MonitorStream << "Forked from process " << global_monitor_pid << "\n";
    }
    global_monitor_pid = getpid();
  }
  _lisp->_Roots._MonitorStream << msg;
  _lisp->_Roots._MonitorStream.flush();
#endif
}

CL_UNWIND_COOP(true); // ok since monitor_message doesn't lisp-unwind
DOCGROUP(clasp);
CL_DEFUN void core__monitor_write(const std::string& msg) { monitor_message(msg); }

CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN void core__set_debug_start_code(T_sp on) { global_debug_start_code = on.notnilp(); }

void Lisp::initializeMainThread() {
  mp::Process_sp main_process =
      mp::Process_O::make_process(INTERN_(core, top_level), nil<T_O>(), _lisp->copy_default_special_bindings(), nil<T_O>(), 0);
  my_thread->initialize_thread(main_process, false);
}

void Lisp::startupLispEnvironment() {

#ifdef DEBUG_FLAGS_SET
  printf("%s:%d There are DEBUG_xxxx flags on - check the top of configure_clasp.h !!!!\n", __FILE__, __LINE__);
#endif

  MONITOR(BF("Starting lisp environment\n"));
  global_dump_functions = getenv("CLASP_DUMP_FUNCTIONS");
  char* debug_start_code = getenv("CLASP_DEBUG_START_CODE");
  if (debug_start_code) {
    printf("%s:%d Turning on *debug-byte-code*\n", __FILE__, __LINE__);
    global_debug_start_code = true;
  }

  //
  // Initialize the symbols
  //
  Symbol_sp symbol_nil = gctools::smart_ptr<Symbol_O>((gc::Tagged)gctools::global_tagged_Symbol_OP_nil);
  symbol_nil->fmakunbound();
  symbol_nil->fmakunbound_setf();
  { // Trap symbols as they are interned
    if (offsetof(Function_O, _TheSimpleFun) != offsetof(FuncallableInstance_O, _TheSimpleFun)) {
      printf("%s:%d  The offsetf(Function_O,entry)/%lu!=offsetof(FuncallableInstance_O,entry)/%lu!!!!\n", __FILE__, __LINE__,
             offsetof(Function_O, _TheSimpleFun), offsetof(FuncallableInstance_O, _TheSimpleFun));
      printf("        These must match for Clasp to be able to function\n");
      abort();
    }
    if (offsetof(Instance_O, _Rack) != offsetof(FuncallableInstance_O, _Rack)) {
      printf("%s:%d  The offsetf(Instance_O,_Rack)/%lu!=offsetof(FuncallableInstance_O,_Rack)/%lu!!!!\n", __FILE__, __LINE__,
             offsetof(Instance_O, _Rack), offsetof(FuncallableInstance_O, _Rack));
      printf("        These must match for Clasp to be able to function\n");
      abort();
    }
    stringstream sdebug;
    // gctools::get_immediate_info(); // discard result, just testing
#ifdef DEBUG_PROGRESS
    printf("%s:%d startupLispEnvironment\n", __FILE__, __LINE__);
#endif
  }
  //
  // Define the _lisp global
  //
  ::_lisp = gctools::tagged_pointer<Lisp>(this); // this->sharedThis<Lisp_O>();
  //	initializeProfiler(this->profiler(),_lisp);
  this->_CoreBuiltInClassesInitialized = false;
  this->_PackagesInitialized = false;
  this->_BuiltInClassesInitialized = false;
  this->_NilsCreated = false;
  this->_EnvironmentInitialized = false;
  this->_EnvironmentId = 0;
  //
  // Setup the core package aka the sys package
  //
  CoreExposer_sp coreExposer;
  {
    initialize_clasp();
    _lisp->_Roots._CorePackage = gc::As<Package_sp>(_lisp->findPackage(CorePkg));
    _lisp->_Roots._KeywordPackage = gc::As<Package_sp>(_lisp->findPackage(KeywordPkg));
    _lisp->_Roots._CommonLispPackage = gc::As<Package_sp>(_lisp->findPackage(ClPkg));
    _lisp->_Roots._CorePackage->setSystemPackageP(true);
    _lisp->_Roots._KeywordPackage->setSystemPackageP(true);
    _lisp->_Roots._CommonLispPackage->setSystemPackageP(true);
    // Set up implementation packages
    _lisp->_Roots._CommonLispPackage->addImplementationPackage(_lisp->_Roots._CorePackage);
    _lisp->_Roots._CommonLispPackage->addImplementationPackage(_lisp->findPackage(ClosPkg).as<Package_O>());
    _lisp->_Roots._CommonLispPackage->addImplementationPackage(_lisp->findPackage(CompPkg).as<Package_O>());

    _lisp->findPackage(ClosPkg).as<Package_O>()->addImplementationPackage(_lisp->_Roots._CorePackage);
    _lisp->findPackage(ClosPkg).as<Package_O>()->addImplementationPackage(_lisp->findPackage(CompPkg).as<Package_O>());

    _lisp->findPackage(ExtPkg).as<Package_O>()->addImplementationPackage(_lisp->_Roots._CorePackage);
    _lisp->findPackage(ExtPkg).as<Package_O>()->addImplementationPackage(_lisp->findPackage(ClosPkg).as<Package_O>());

    _lisp->_Roots._CommonLispPackage->setLockedP(true);
    //_lisp->_Roots._CorePackage->setLockedP(true);
    //_lisp->findPackage(CompPkg).as<Package_O>()->setLockedP(true);
    _lisp->findPackage(ClosPkg).as<Package_O>()->setLockedP(true);
    _lisp->findPackage(ExtPkg).as<Package_O>()->setLockedP(true);
    //
    // fixme2022 Rip this package out if we don't need it to store the reference compiler
    //
    _lisp->makePackage("CMPREF", {}, {"COMMON-LISP"}, {});

#ifdef DEFINE_CL_SYMBOLS
    initializeAllClSymbols(_lisp->_Roots._CommonLispPackage);
#endif
    coreExposer = gc::GC<CoreExposer_O>::allocate(_lisp);
    coreExposer->define_essential_globals(_lisp);
    this->_PackagesInitialized = true;
  }
  this->_EnvironmentInitialized = true;
  this->_BuiltInClassesInitialized = true;
  //	LOG("ALL CLASSES: %s"% this->dumpClasses() );
  //    this->createNils();
  {
#ifdef DEBUG_ON
//    rootClassManager().debugDump();
#endif
  }
  //
  // Finish initializing Lisp object
  //
#ifdef DEBUG_PROGRESS
  printf("%s:%d startupLispEnvironment initialize everything\n", __FILE__, __LINE__);
#endif
  {
    initialize_Lisp();
    core::HashTable_sp ht = core::HashTable_O::createEql();
    core::_sym_STARcxxDocumentationSTAR->defparameter(ht);
    Readtable_sp readtable = Readtable_O::create_standard_readtable();
    cl::_sym_STARreadtableSTAR->defparameter(readtable);
    initialize_functions();
    globals_->_Bundle->setup_pathname_translations();
#ifdef DEBUG_PROGRESS
    printf("%s:%d startupLispEnvironment initialize_classes_and_methods\n", __FILE__, __LINE__);
#endif
    initialize_classes_and_methods();
    // core__satiateSingleDispatchGenericFunctions();
#ifdef DEBUG_PROGRESS
    printf("%s:%d startupLispEnvironment initialize_source_info\n", __FILE__, __LINE__);
#endif
    initialize_source_info();
#ifdef DEBUG_PROGRESS
    printf("%s:%d startupLispEnvironment initialize_backquote\n", __FILE__, __LINE__);
#endif
    initialize_backquote();
#ifdef DEBUG_PROGRESS
    printf("%s:%d startupLispEnvironment initialize_compiler_primitives\n", __FILE__, __LINE__);
#endif
    initialize_compiler_primitives(_lisp);
#ifdef DEBUG_PROGRESS
    printf("%s:%d startupLispEnvironment initialize_bits\n", __FILE__, __LINE__);
#endif
    initialize_bits();
    // Rest may be unnecessary after new boot-strapping approach is developed
#ifdef DEFINE_CL_SYMBOLS
    initializeAllClSymbolsFunctions();
#endif
    coreExposer->expose(_lisp, Exposer_O::candoClasses);
    //	    initializeCandoClos(_lisp);
  }
  coreExposer->expose(_lisp, Exposer_O::candoFunctions);
  coreExposer->expose(_lisp, Exposer_O::candoGlobals);
  {
    for (vector<InitializationCallback>::iterator ic = globals_->_GlobalInitializationCallbacks.begin();
         ic != globals_->_GlobalInitializationCallbacks.end(); ic++) {
      (*ic)(_lisp);
    }
  }
  this->switchToClassNameHashTable();
  {
    FILE* null_out = fopen("/dev/null", "w");
    this->_Roots._NullStream = CFileStream_O::make(str_create("/dev/null"), null_out, StreamDirection::io);
    this->_Roots._ImaginaryUnit = Complex_O::create(0.0, 1.0);
    this->_Roots._ImaginaryUnitNegative = Complex_O::create(0.0, -1.0);
    this->_Roots._PlusHalf = Ratio_O::create_primitive(make_fixnum(1), make_fixnum(2));

    core::getcwd(true); // set *default-pathname-defaults*
  };
  //
  // Initialize the main thread info
  //
  this->initializeMainThread();

  this->_Roots._PrintSymbolsProperly = true;
  mpip::Mpi_O::initializeGlobals(_lisp);
  feenableexcept(_TrapFpeBits);
  fedisableexcept(~_TrapFpeBits);
  _lisp->_Roots._Started = true;

  //
  // Pause if CLASP_PAUSE_INIT environment variable is defined
  //
  {
    char* pause_startup = getenv("CLASP_PAUSE_INIT");
    if (pause_startup) {
#ifdef USE_USER_SIGNAL
      gctools::wait_for_user_signal("Paused for CLASP_PAUSE_INIT");
#else
      printf("%s:%d PID = %d Paused after initialization - press enter to continue: \n", __FILE__, __LINE__, getpid());
      fflush(stdout);
      getchar();
#endif
    }
  }
  //
  // Create the first process
  //
  mp::_sym_STARcurrent_processSTAR->defparameter(my_thread->_Process);
  this->add_process(my_thread->_Process);
  my_thread->_Process->_Phase = mp::Running;
  this->_Booted = true;

  globals_->_InitFileName = "sys:src;lisp;" KERNEL_NAME ";init.lisp";
}

/*! Get a Str8Ns buffer string from the BufferStr8NsPool.*/
Str8Ns_sp Lisp::get_Str8Ns_buffer_string() {
  /* BufferStr8NsPool must be thread local */
  unlikely_if(!my_thread->_BufferStr8NsPool) {
    // Lazy initialize
    my_thread->_BufferStr8NsPool = nil<T_O>();
  }
  unlikely_if(my_thread->_BufferStr8NsPool.nilp()) {
    // If list is empty, link in a buffer
    Str8Ns_sp one = Str8Ns_O::make(256, ' ', true, clasp_make_fixnum(0));
    my_thread->_BufferStr8NsPool = Cons_O::create(one, my_thread->_BufferStr8NsPool);
  }
  Str8Ns_sp ret = gc::As<Str8Ns_sp>(oCar(my_thread->_BufferStr8NsPool));
  my_thread->_BufferStr8NsPool = oCdr(my_thread->_BufferStr8NsPool);
  ret->fillPointerSet(0);
  return ret;
}

/*! Return a buffer string to the BufferStr8NsPool
 */
void Lisp::put_Str8Ns_buffer_string(Str8Ns_sp str) {
  my_thread->_BufferStr8NsPool = Cons_O::create(str, my_thread->_BufferStr8NsPool);
}

/*! Get a StrWNs buffer string from the BufferStrWNsPool.*/
StrWNs_sp Lisp::get_StrWNs_buffer_string() {
  /* BufferStrWNsPool must be thread local */
  unlikely_if(!my_thread->_BufferStrWNsPool) {
    // Lazy initialize
    my_thread->_BufferStrWNsPool = nil<T_O>();
  }
  unlikely_if(my_thread->_BufferStrWNsPool.nilp()) {
    // If list is empty, link in a buffer
    StrWNs_sp one = StrWNs_O::make(256, ' ', true, clasp_make_fixnum(0));
    my_thread->_BufferStrWNsPool = Cons_O::create(one, my_thread->_BufferStrWNsPool);
  }
  StrWNs_sp ret = gc::As<StrWNs_sp>(oCar(my_thread->_BufferStrWNsPool));
  my_thread->_BufferStrWNsPool = oCdr(my_thread->_BufferStrWNsPool);
  ret->fillPointerSet(0);
  return ret;
}

/*! Return a buffer string to the BufferStrWNsPool
 */
void Lisp::put_StrWNs_buffer_string(StrWNs_sp str) {
  my_thread->_BufferStrWNsPool = Cons_O::create(str, my_thread->_BufferStrWNsPool);
}

T_sp Lisp::getCurrentReadTable() { return cl::_sym_STARreadtableSTAR->symbolValue(); }

#if 0
void Lisp::setMakePackageAndExportSymbolCallbacks(MakePackageCallback mpc, ExportSymbolCallback esc) {
  
  LOG("Setting MakePackageCallback and ExportSymbolCallback");
  this->_MakePackageCallback = mpc;
  this->_ExportSymbolCallback = esc;
}
#endif

#ifdef CLASP_THREADS
void Lisp::add_process(mp::Process_sp process) {
  WITH_READ_WRITE_LOCK(globals_->_ActiveThreadsMutex);
  this->_Roots._ActiveThreads = Cons_O::create(process, this->_Roots._ActiveThreads);
#ifdef DEBUG_ADD_PROCESS
  printf("%s:%d Added process %s @%p active threads now: %s\n", __FILE__, __LINE__, _rep_(process).c_str(), (void*)process.raw_(),
         _rep_(this->_Roots._ActiveThreads).c_str());
  fflush(stdout);
#endif
}

void Lisp::remove_process(mp::Process_sp process) {
  {
    WITH_READ_WRITE_LOCK(globals_->_ActiveThreadsMutex);
    T_sp cur = this->_Roots._ActiveThreads;
    mp::Process_sp p;
    if (cur.consp()) {
      p = gc::As<mp::Process_sp>(cons_car(cur));
      if (process == p) {
        // If the process is the first in the list, just set the ActiveThreads to the cdr.
        this->_Roots._ActiveThreads = cons_cdr(cur);
        return;
      } else {
        // Iterate through to find the process and rplacd it out.
        T_sp next = cons_cdr(cur);
        while (next.consp()) {
          p = gc::As<mp::Process_sp>(cons_car(next));
          if (p == process) {
            gc::As_unsafe<Cons_sp>(cur)->rplacd(cons_cdr(next));
            return;
          }
          cur = next;
          next = cons_cdr(next);
        }
      }
    }
  } // lock release
  SIMPLE_ERROR("Could not find process {}", _rep_(process));
}

List_sp Lisp::processes() const {
  WITH_READ_LOCK(globals_->_ActiveThreadsMutex);
  return cl__copy_list(this->_Roots._ActiveThreads);
}

void Lisp::push_default_special_binding(Symbol_sp symbol, T_sp form) {
  WITH_READ_WRITE_LOCK(globals_->_DefaultSpecialBindingsMutex);
  Cons_sp pair = Cons_O::create(symbol, form);
  this->_Roots._DefaultSpecialBindings = Cons_O::create(pair, this->_Roots._DefaultSpecialBindings);
}

List_sp Lisp::copy_default_special_bindings() const {
  WITH_READ_LOCK(globals_->_DefaultSpecialBindingsMutex);
  return cl__copy_list(this->_Roots._DefaultSpecialBindings);
}

#endif

void Lisp::defvar(Symbol_sp sym, T_sp obj) {

  sym->makeSpecial();
  sym->setf_symbolValue(obj);
}

void Lisp::defconstant(Symbol_sp sym, T_sp obj) {

  sym->makeSpecial();
  sym->setf_symbolValue(obj);
  sym->setReadOnly(true);
}

void Lisp::installPackage(const Exposer_O* pkg) {
  LOG("Installing package[{}]", _rep_(pkg->name()));
  int firstNewGlobalCallback = globals_->_GlobalInitializationCallbacks.end() - globals_->_GlobalInitializationCallbacks.begin();
  ChangePackage change(gc::As<Package_sp>(_lisp->findPackage(pkg->packageName())));
  { pkg->expose(_lisp, Exposer_O::candoClasses); }
  { pkg->expose(_lisp, Exposer_O::candoFunctions); }
  { pkg->expose(_lisp, Exposer_O::candoGlobals); }

  {
    for (vector<InitializationCallback>::iterator ic = globals_->_GlobalInitializationCallbacks.begin() + firstNewGlobalCallback;
         ic != globals_->_GlobalInitializationCallbacks.end(); ic++) {
      (*ic)(_lisp);
    }
  }
}

void Lisp::installGlobalInitializationCallback(InitializationCallback c) { globals_->_GlobalInitializationCallbacks.push_back(c); }

void Lisp::addClassNameToPackageAsDynamic(const string& package, const string& name, Instance_sp mc) {
  Symbol_sp classSymbol = _lisp->intern(name, gc::As<Package_sp>(_lisp->findPackage(package, true)));
  classSymbol->exportYourself();
  classSymbol->setf_symbolValue(mc);
  //    this->globalEnvironment()->extend(classSymbol,mc);
  //    mc->__setLambdaListHandlerString(mc->getInstanceBaseClass()->__getLambdaListHandlerString());
}

/*! Add the class with (className) to the current package
 */
void Lisp::addClassSymbol(Symbol_sp classSymbol, Creator_sp alloc, Symbol_sp base1ClassSymbol) {
  LOG("Lisp::addClass classSymbol({}) baseClassSymbol1({}) baseClassSymbol2({})", _rep_(classSymbol), base1ClassSymbol,
      base2ClassSymbol);
  Instance_sp cc = Instance_O::create(classSymbol, _lisp->_Roots._TheBuiltInClass, alloc);
  printf("%s:%d --> Adding class[%s]\n", __FILE__, __LINE__, _rep_(classSymbol).c_str());
  core__setf_find_class(cc, classSymbol);
  cc->addInstanceBaseClass(base1ClassSymbol);
  ASSERTF((bool)alloc, "_creator for {} is NULL!!!", _rep_(classSymbol));
  cc->CLASS_set_creator(alloc);
}

void Lisp::mapNameToPackage(String_sp sname, Package_sp pkg) {
  int packageIndex;
  {
    WITH_READ_WRITE_LOCK(globals_->_PackagesMutex);
    for (packageIndex = 0; packageIndex < this->_Roots._Packages.size(); ++packageIndex) {
      if (this->_Roots._Packages[packageIndex] == pkg) {
        this->_Roots._PackageNameIndexMap->setf_gethash(sname, make_fixnum(packageIndex));
        return;
      }
    }
  }
  SIMPLE_ERROR("Could not find package with (nick)name: {}", _rep_(pkg->name()));
}

void Lisp::mapNameToPackage(const string& name, Package_sp pkg) {
  mapNameToPackage(SimpleBaseString_O::make(name), pkg);
}

void Lisp::unmapNameToPackage(String_sp sname) {
  {
    WITH_READ_WRITE_LOCK(globals_->_PackagesMutex);
    T_sp it = this->_Roots._PackageNameIndexMap->gethash(sname);
    if (it.nilp()) {
      goto package_unfound;
    }
    this->_Roots._PackageNameIndexMap->remhash(sname);
    return;
  }
package_unfound:
  SIMPLE_ERROR("Could not find package with (nick)name: {}", _rep_(sname));
}

void Lisp::finishPackageSetup(const string& pkgname, list<string> const& nicknames, list<string> const& usePackages,
                              list<std::string> const& shadow) {
  T_sp tpkg = _lisp->findPackage(pkgname, false);
  if (tpkg.nilp()) {
    this->makePackage(pkgname, nicknames, usePackages, shadow);
    return;
  }
  Package_sp pkg = gc::As_unsafe<Package_sp>(tpkg);
  {
    ql::list nn;
    for (auto name : nicknames) {
      SimpleBaseString_sp str = SimpleBaseString_O::make(name);
      nn << str;
    }
    pkg->setNicknames(nn.cons());
  }
  {
    ql::list sn;
    for (auto name : usePackages) {
      Package_sp other = gc::As<Package_sp>(_lisp->findPackage(name, true));
      pkg->usePackage(other);
    }
  }
};

Package_sp Lisp::makePackage(const string& name, list<string> const& nicknames, list<string> const& usePackages,
                             list<std::string> const& shadow) {
  /* This function is written somewhat bizarrely for lock safety reasons.
   * The trick is that the error infrastructure, among other things, uses the package system.
   * Therefore, if we lock the system, then signal an error, the error function will call findPackage or
   * something, it will try to grab the lock, and it will fail because we already have it.
   * Additionally, CLHS specifies that the errors are CORRECTABLE, so we could hypothetically have the user
   * in the debugger while the package system is locked - that won't work.
   * Instead we do this: Grab the lock. Try the operation. If the operation succeeds, just return.
   * If it fails, goto (yes, really) outside the lock scope so that we ungrab it, and signal an cerror there.
   * */
 start:
  string usedNickName;
  Package_sp packageUsingNickName;
  Package_sp existing_package;
  SimpleString_sp nonexistentUsedPackage;
  {
    WITH_READ_WRITE_LOCK(globals_->_PackagesMutex);
    SimpleBaseString_sp sname = SimpleBaseString_O::make(name);
    T_sp it = this->_Roots._PackageNameIndexMap->gethash(sname);
    if (it.notnilp()) {
      ASSERT(it.fixnump());
      int existing_package_id = it.unsafe_fixnum();
      existing_package = this->_Roots._Packages[existing_package_id];
      goto name_exists;
    }
    // first check the nicknames, before we create the package, so that we
    // don't leave packages partly created
    for (list<string>::const_iterator it = nicknames.begin(); it != nicknames.end(); it++) {
      string nickName = *it;
      SimpleBaseString_sp snickName = SimpleBaseString_O::make(nickName);
      T_sp nit = this->_Roots._PackageNameIndexMap->gethash(snickName);
      if (nit.notnilp() && nickName != name) {
        ASSERT(nit.fixnump());
        int existingIndex = nit.unsafe_fixnum();
        usedNickName = nickName;
        packageUsingNickName = this->_Roots._Packages[existingIndex];
        goto nickname_exists;
      }
    }
    // Now we know that there is no conflict about the package name or
    // nicknames, so actually create the package
    LOG("Creating package with name[{}]", name);
    Package_sp newPackage = Package_O::create(name);
    int packageIndex = this->_Roots._Packages.size();
    {
      this->_Roots._PackageNameIndexMap->setf_gethash(sname, make_fixnum(packageIndex));
      this->_Roots._Packages.push_back(newPackage);
    }
    {
      List_sp cnicknames(nil<T_O>());
      for (list<string>::const_iterator it = nicknames.begin(); it != nicknames.end(); it++) {
        string nickName = *it;
        SimpleBaseString_sp snickName = SimpleBaseString_O::make(nickName);
        T_sp nit2 = this->_Roots._PackageNameIndexMap->gethash(snickName);
        if (nit2.notnilp() && nickName != name) {
          // should not happen, since we just tested that
          ASSERT(nit2.fixnump());
          int existingIndex = nit2.unsafe_fixnum();
          usedNickName = nickName;
          packageUsingNickName = this->_Roots._Packages[existingIndex];
          goto nickname_exists;
        }
        this->_Roots._PackageNameIndexMap->setf_gethash(snickName, make_fixnum(packageIndex));
        cnicknames = Cons_O::create(snickName, cnicknames);
      }
      newPackage->setNicknames(cnicknames);
    }
    for (auto x : shadow) {
      SimpleBaseString_sp sx = SimpleBaseString_O::make(x);
      newPackage->shadow(sx);
    }
    for (list<string>::const_iterator jit = usePackages.begin(); jit != usePackages.end(); jit++) {
      T_sp tUsePkg = this->findPackage_no_lock(*jit);
      if (tUsePkg.isA<Package_O>()) {
        Package_sp usePkg = tUsePkg.as_unsafe<Package_O>();
        LOG("Using package[{}]", _rep_(usePkg->name()));
        newPackage->usePackage(usePkg);
      } else {
        nonexistentUsedPackage = SimpleBaseString_O::make(*jit);
        goto nonexistent_used_package;
      }
    }
    if (globals_->_MakePackageCallback != NULL) {
      LOG("Calling _MakePackageCallback with package[{}]", name);
      globals_->_MakePackageCallback(name, _lisp);
    } else {
      LOG("_MakePackageCallback is NULL - not calling callback");
    }
    return newPackage;
  }
// A correctable error is signaled if the package-name or any of the nicknames
// is already the name or nickname of an existing package.
// The correction is to delete the existing package.
 name_exists:
  CEpackage_error("There already exists a package with name: ~a", "Delete existing package", existing_package, 1,
                  SimpleBaseString_O::make(name));
  cl__delete_package(existing_package);
  goto start;
 nickname_exists:
  CEpackage_error("There already exists a package with nickname: ~a", "Delete existing package", existing_package, 1,
                  SimpleBaseString_O::make(name));
  cl__delete_package(existing_package);
  goto start;
 nonexistent_used_package:
  // FIXME: It might be nicer to let the error be correctable,
  // e.g. by not trying to USE the nonexistent package.
  // (The standard actually leaves this situation undefined.)
  PACKAGE_ERROR(nonexistentUsedPackage);
}

Package_sp Lisp::makePackage(SimpleString_sp name, List_sp nicknames, List_sp use) {
 start:
  T_sp existingPackage;
  SimpleString_sp nonexistentUsedPackage;
  // We need to coerce the nicknames for setNicknames so do that first.
  ql::list qnicknames;
  for (auto nc : nicknames) qnicknames << coerce::simple_string(oCar(nc));
  List_sp cnicknames = qnicknames.cons();
  {
    WITH_READ_WRITE_LOCK(globals_->_PackagesMutex);
    // Before creating the package, we check if the names or nicknames
    // conflict with existing packages. This prevents us from half-making
    // packages.
    existingPackage = this->findPackage_no_lock(name);
    if (existingPackage.notnilp()) goto name_exists;
    for (auto nc : cnicknames) {
      existingPackage = this->findPackage_no_lock(oCar(nc).as_unsafe<SimpleString_O>());
      if (existingPackage.notnilp()) goto name_exists;
    }
    // We're good, make the package.
    Package_sp newPackage = Package_O::create(name);
    Fixnum_sp packageIndex = make_fixnum(this->_Roots._Packages.size());
    {
      this->_Roots._PackageNameIndexMap->setf_gethash(name, packageIndex);
      this->_Roots._Packages.push_back(newPackage);
    }
    // Assign nicknames.
    for (auto nc : cnicknames) {
      this->_Roots._PackageNameIndexMap->setf_gethash(oCar(nc), packageIndex);
    }
    newPackage->setNicknames(cnicknames);
    // Use packages.
    for (auto u : use) {
      Package_sp usePkg = oCar(u).as_assert<Package_O>();
      // FIXME: usePackage may signal errors & grabs read lock!
      newPackage->usePackage(usePkg);
    }
    // Done!
    return newPackage;
  }
 name_exists:
  CEpackage_error("There already exists a package with name: ~a", "Delete existing package", existingPackage.as_unsafe<Package_O>(), 1, name);
  cl__delete_package(existingPackage);
  goto start;
 nonexistent_used_package:
  PACKAGE_ERROR(nonexistentUsedPackage);
}

T_sp Lisp::findPackage_no_lock(const string& name) const {
  return this->findPackage_no_lock(SimpleBaseString_O::make(name));
}

T_sp Lisp::findPackage(const string& name, bool errorp) const {
  return this->findPackage(SimpleBaseString_O::make(name), errorp);
}

T_sp Lisp::findPackage_no_lock(String_sp name) const {
  // Check local nicknames first.
  if (_lisp->_Roots._TheSystemIsUp) {
    T_sp local = this->getCurrentPackage()->findPackageByLocalNickname(name);
    if (local.notnilp())
      return local;
  }
  // OK, now global names.
  T_sp fi = this->_Roots._PackageNameIndexMap->gethash(name);
  if (fi.nilp()) {
    return nil<Package_O>(); // return nil if no package found
  }
  ASSERT(fi.fixnump());
  Package_sp getPackage = this->_Roots._Packages[fi.unsafe_fixnum()];
  return getPackage;
}

T_sp Lisp::findPackage(String_sp name, bool errorp) const {
  {
    WITH_READ_LOCK(globals_->_PackagesMutex);
    T_sp res = this->findPackage_no_lock(name);
    if (!errorp || res.isA<Package_O>())
      return res;
  }
  // Signal the error only after releasing the lock.
  PACKAGE_ERROR(name);
}

void Lisp::remove_package(String_sp name) {
  WITH_READ_WRITE_LOCK(globals_->_PackagesMutex);
  T_sp fi = this->_Roots._PackageNameIndexMap->gethash(name);
  if (fi.nilp()) {
    PACKAGE_ERROR(name);
  }
  this->_Roots._PackageNameIndexMap->remhash(name);
  this->_Roots._Packages[fi.unsafe_fixnum()]->setZombieP(true);
}

bool Lisp::recognizesPackage(const string& packageName) const {
  WITH_READ_LOCK(globals_->_PackagesMutex);
  SimpleBaseString_sp sname = SimpleBaseString_O::make(packageName);
  T_sp pi = this->_Roots._PackageNameIndexMap->gethash(sname);
  return (pi.notnilp());
}

List_sp Lisp::allPackagesAsCons() const {
  WITH_READ_LOCK(globals_->_PackagesMutex);
  gctools::Vec0<Package_sp> TempPackages;
  for (int packageIndex = 0; packageIndex < this->_Roots._Packages.size(); ++packageIndex) {
    // As a workaround, don't list previously deleted packages
    if (!this->_Roots._Packages[packageIndex]->getZombieP()) {
      TempPackages.push_back(this->_Roots._Packages[packageIndex]);
    }
  }
  return asCons(TempPackages);
}

void Lisp::inPackage(const string& p) {
  WITH_READ_LOCK(globals_->_PackagesMutex);
  SimpleBaseString_sp sname = SimpleBaseString_O::make(p);
  T_sp pi = this->_Roots._PackageNameIndexMap->gethash(sname);
  if (pi.nilp()) {
    SIMPLE_ERROR("I do not recognize package: {}", p);
  }
  this->selectPackage(this->_Roots._Packages[pi.unsafe_fixnum()]);
}

Package_sp Lisp::getCurrentPackage() const {
  // At startup the *package* symbol may not yet
  // be defined or bound to a package - in that case just say we are in the core package
  //
  Package_sp cur;
  if (!cl::_sym_STARpackageSTAR || !cl::_sym_STARpackageSTAR->specialP()) {
    return this->_Roots._CorePackage;
  }
  return gc::As<Package_sp>(cl::_sym_STARpackageSTAR->symbolValue());
}

void Lisp::selectPackage(Package_sp pack) { cl::_sym_STARpackageSTAR->setf_symbolValue(pack); }

void Lisp::throwIfBuiltInClassesNotInitialized() {
  if (this->_BuiltInClassesInitialized)
    return;
  SIMPLE_ERROR("Cpp-classes are not initialized");
}

Path_sp Lisp::translateLogicalPathname(T_sp obj) {
  if (cl__stringp(obj)) {
    String_sp logicalPathName = gc::As_unsafe<String_sp>(obj);
    string fileName = logicalPathName->get_std_string();
    return Path_O::create(fileName);
    SIMPLE_ERROR("include {} error, file does not exist", fileName);
  } else {
    SIMPLE_ERROR("Finish implementing Lisp::translateLogicalPathname");
  }
}

Path_sp Lisp::translateLogicalPathnameUsingPaths(T_sp obj) {
  if (cl__stringp(obj)) {
    String_sp logicalPathName = gc::As_unsafe<String_sp>(obj);
    string fileName = logicalPathName->get_std_string();
    LOG("Looking for file: {}", fileName.c_str());
    LOG("Looking in current directory");
    std::filesystem::path onePath("./");
    onePath /= fileName;
    if (std::filesystem::exists(onePath)) {
      return Path_O::create(onePath.string());
    }
    Symbol_sp pathSym = _sym_STARPATHSTAR;
    List_sp pathList = pathSym->symbolValue();
    LOG("PATH variable = {}", _rep_(pathList).c_str());
    while (pathList.notnilp()) {
      std::filesystem::path onePath(gc::As<String_sp>(oCar(pathList))->get_std_string());
      onePath /= fileName;
      LOG("Checking path[{}]", onePath.string());
      if (std::filesystem::exists(onePath)) {
        return Path_O::create(onePath.string());
      }
      pathList = oCdr(pathList);
    }
    SIMPLE_ERROR("include {} error, file does not exist", fileName);
  } else {
    SIMPLE_ERROR("Finish implementing Lisp::translateLogicalPathname");
  }
}

uint Lisp::nextEnvironmentId() {
  this->_EnvironmentId++;
  return this->_EnvironmentId;
}

unsigned char global_python_vm_codes_literal[] =
#define PYTHON_OPCODES
#include <virtualMachine.h>
    ;
#undef PYTHON_OPCODES

extern "C" {
unsigned char* global_python_virtual_machine_codes;
uintptr_t global_python_virtual_machine_codes_size;
unsigned char* global_python_class_layouts;
uintptr_t global_python_class_layouts_size;
};

void dumpDebuggingLayouts() {
  global_python_virtual_machine_codes = (unsigned char*)global_python_vm_codes_literal;
  global_python_virtual_machine_codes_size = sizeof(global_python_vm_codes_literal);
  stringstream fout;
#if defined(USE_PRECISE_GC)
  gctools::walk_stamp_field_layout_tables(gctools::lldb_info, fout);
  llvmo::dump_objects_for_debugger(fout, "");
#else
  dumpBoehmLayoutTables(fout);
  llvmo::dump_objects_for_debugger(fout, "");
#endif
  size_t sz = fout.str().size();
  global_python_class_layouts = (unsigned char*)malloc(sz + 1);
  memcpy(global_python_class_layouts, fout.str().c_str(), sz);
  global_python_class_layouts[sz] = '\0';
  global_python_class_layouts_size = fout.str().size();
}

void Lisp::parseCommandLineArguments(const CommandLineOptions& options) {
  LOG("Parsing what is left over into lisp environment arguments");
  gctools::Vec0<T_sp> vargs;
  for (auto arg : options._LispArguments) {
    vargs.push_back(SimpleBaseString_O::make(arg));
  }
  SimpleVector_sp args = SimpleVector_O::make(vargs);
  LOG(" Command line arguments are being set in Lisp to: {}", _rep_(args));
  SYMBOL_EXPORT_SC_(CorePkg, STARcommandLineArgumentsSTAR);
  _sym_STARcommandLineArgumentsSTAR->defparameter(args);

  if (options._PauseForDebugger) {
    printf("The PID is  %d  - press enter to continue\n", getpid());
    string temp;
    std::cin >> temp;
  }

  List_sp features = cl::_sym_STARfeaturesSTAR->symbolValue();
  for (auto feature : options._Features) {
    features = Cons_O::create(_lisp->internKeyword(feature), features);
  }
  cl::_sym_STARfeaturesSTAR->setf_symbolValue(features);
  // Set additional features for debugging flags
  //  pass a dummy stringstream that builds a report
  stringstream ss;
  gctools::debugging_configuration(true, false, ss);

  SYMBOL_EXPORT_SC_(CorePkg, STARprintVersionOnStartupSTAR);
  _sym_STARprintVersionOnStartupSTAR->defparameter(_lisp->_boolean(options._Version));
  SYMBOL_EXPORT_SC_(CorePkg, STARsilentStartupSTAR);
  _sym_STARsilentStartupSTAR->defparameter(_lisp->_boolean(options._SilentStartup));
  if (!options._SilentStartup) {
    stringstream sdebug;
    bool debugging = gctools::debugging_configuration(false, true, sdebug);
    if (debugging) {
      printf("%s:%d Debugging flags are set - configuration:\n%s", __FILE__, __LINE__, sdebug.str().c_str());
    }
    printf("%s:%d  Lisp smart_ptr width -> %d  sizeof(Lisp) -> %d\n", __FILE__, __LINE__, (int)(sizeof(_lisp->_Roots) / 8),
           (int)sizeof(Lisp));
  }

  SYMBOL_EXPORT_SC_(CorePkg, STARcommandLineImageSTAR);
  switch (options._StartupType) {
  case cloImageFile:
    _sym_STARcommandLineImageSTAR->defparameter(cl__pathname(SimpleBaseString_O::make(options._StartupFile)));
    break;
  case cloBaseImage:
    _sym_STARcommandLineImageSTAR->defparameter(core__startup_image_pathname(false));
    break;
  case cloExtensionImage:
    _sym_STARcommandLineImageSTAR->defparameter(core__startup_image_pathname(true));
    break;
  default:
    break;
  }
}

T_mv Lisp::readEvalPrint(T_sp stream, T_sp environ, bool printResults, bool prompt) {
  T_mv result = Values(nil<T_O>());
  MultipleValues& mvn = core::lisp_multipleValues();
  while (1) {
    try {
      if (prompt) {
        stringstream prompts;
        prompts << std::endl;
        Symbol_sp pkgSym = cl::_sym_STARpackageSTAR;
        T_sp pkgVal = pkgSym->symbolValue();
        Package_sp curPackage = gc::As<Package_sp>(pkgVal);
        prompts << _rep_(curPackage->name()) << "> ";
        clasp_write_string(prompts.str(), stream);
      }
      T_sp expression = cl__read(stream, nil<T_O>(), unbound<T_O>(), nil<T_O>());
      if (expression.unboundp())
        break;
      if (_sym_STARechoReplReadSTAR->symbolValue().isTrue()) {
        string suppress;
        if (cl::_sym_STARread_suppressSTAR->symbolValue().isTrue()) {
          suppress = "SUPPRESSED";
          if (expression.notnilp()) {
            SIMPLE_ERROR("*read-suppress* is true but the following expression was read: {}", _rep_(expression));
          }
        }
        clasp_write_string(fmt::format(";;--read-{}-------------\n#|\n{}\n----------|#\n", suppress.c_str(), _rep_(expression)));
      }
      if (cl__keywordp(expression)) {
        ql::list tplCmd;
        tplCmd << expression;
        while (T_sp exp = cl__read(stream, nil<T_O>(), unbound<T_O>(), nil<T_O>())) {
          if (exp.unboundp())
            break;
          tplCmd << exp;
        }
        if (_sym_STARtopLevelCommandHookSTAR->symbolValue().notnilp()) {
          eval::funcall(_sym_STARtopLevelCommandHookSTAR->symbolValue(), tplCmd.cons());
        } else {
          clasp_write_string(
              fmt::format("Cannot interpret {} - define core::*top-level-command-hook*", _rep_(Cons_O::createList(tplCmd.cons()))));
        }
      } else if (expression.notnilp()) {
        result = eval::evaluate(expression, environ);
        gctools::Vec0<core::T_sp /*,gctools::RootedGCHolder*/> vresults;
        vresults.resize(result.number_of_values());
        if (result.number_of_values() > 0) {
          vresults[0] = result;
          if (result.number_of_values() > 1) {
            for (int i(1); i < result.number_of_values(); ++i) {
              vresults[i] = mvn.valueGet(i, result.number_of_values());
            }
          }
        }
        if (printResults) {
          for (int i(0); i < vresults.size(); i++) {
            T_sp obj = vresults[i];
            //			    this->print(BF("; --> %s\n")% _rep_(obj));
            eval::funcall(cl::_sym_print, obj);
          }
        }
      }
    } catch (DebuggerSaysAbortToRepl& err) {
      printf("%s:%d Aborting to repl\n", __FILE__, __LINE__);
    }
  };
  return result;
}

T_mv Lisp::readEvalPrintString(const string& code, T_sp environ, bool printResults) {

  StringInputStream_sp sin = gc::As_unsafe<StringInputStream_sp>(StringInputStream_O::make(code));
  T_mv result = this->readEvalPrint(sin, environ, printResults, false);
  cl__close(sin);
  return result;
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(lowLevelRepl - this is a built in repl for when the top-level repl isn't available)dx");
DOCGROUP(clasp);
CL_DEFUN void core__low_level_repl() {
  List_sp features = cl::_sym_STARfeaturesSTAR->symbolValue();
  if (features.notnilp()) {
    if (global_options->_Interactive) {
      _lisp->readEvalPrint(cl::_sym_STARterminal_ioSTAR->symbolValue(), nil<T_O>(), true, true);
    }
  }
};

CL_DOCSTRING(R"dx(Return a list of all single dispatch generic functions.)dx");
DOCGROUP(clasp);
CL_DEFUN List_sp core__singleDispatchGenericFunctions() {
  List_sp result = _lisp->_Roots._SingleDispatchGenericFunctions.load();
  return result;
}

DOCGROUP(clasp);
CL_DEFUN T_sp core__startup_type() {
  switch (global_options->_StartupType) {
  case cloNone:
    return INTERN_(kw, none);
  case cloInitLisp:
    return INTERN_(kw, init_lisp);
  case cloBaseImage:
    return INTERN_(kw, base_image);
  case cloExtensionImage:
    return INTERN_(kw, extension_image);
  case cloImageFile:
    return INTERN_(kw, image_file);
  case cloSnapshotFile:
    return INTERN_(kw, snapshot_file);
  case cloEmbeddedSnapshot:
    return INTERN_(kw, embedded_snapshot);
  }
}

DOCGROUP(clasp);
CL_DEFUN bool core__noinform_p() { return global_options->_NoInform; }

DOCGROUP(clasp);
CL_DEFUN bool core__noprint_p() { return global_options->_NoPrint; }

CL_DOCSTRING(R"dx(Enable the system debugger if it has been disabled by disable-debugger.)dx");
DOCGROUP(clasp);
CL_DEFUN void ext__enable_debugger() { global_options->_DebuggerDisabled = false; }

CL_DOCSTRING(R"dx(Disable the system debugger)dx");
CL_DOCSTRING_LONG(
    R"dx(If the debugger is disabled, then if invoke-debugger is called, *invoke-debugger-hook* and/or *debugger-hook* are called as normal. However, if the default debugger would be entered, Clasp will instead dump a backtrace and exit with a non-zero code.)dx");
DOCGROUP(clasp);
CL_DEFUN void ext__disable_debugger() { global_options->_DebuggerDisabled = true; }

DOCGROUP(clasp);
CL_DEFUN bool core__debugger_disabled_p() { return global_options->_DebuggerDisabled; }

DOCGROUP(clasp);
CL_DEFUN bool core__is_interactive_lisp() { return global_options->_Interactive; }

// This conses, which is kind of stupid, but we only call it once.
DOCGROUP(clasp);
CL_DEFUN String_sp core__rc_file_name() {
  // FIXME: Unicode filenames?
  return SimpleBaseString_O::make(global_options->_RCFileName);
}

DOCGROUP(clasp);
CL_DEFUN bool core__no_rc_p() { return global_options->_NoRc; }

void Lisp::readEvalPrintInteractive() {
  core::clasp_write_string("Clasp (copyright Christian E. Schafmeister 2014-2023)\n"
                           "Low level repl\n");
  this->readEvalPrint(cl::_sym_STARterminal_ioSTAR->symbolValue(), nil<T_O>(), true, true);
  stream_terpri(cl::_sym_STARterminal_ioSTAR->symbolValue());
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(stackUsed)dx");
DOCGROUP(clasp);
CL_DEFUN size_t core__stack_used() {
  int x;
  char* xaddr = (char*)(&x);
  if (xaddr > my_thread_low_level->_StackTop) {
    printf("%s:%d There is a problem with the stack _lisp->_StackTop@%p is below the current stack pointer@%p\n", __FILE__,
           __LINE__, my_thread_low_level->_StackTop, xaddr);
    abort();
  }
  size_t stack = (size_t)((const char*)my_thread_low_level->_StackTop - xaddr);
  return stack;
};

static bool global_invokedInternalDebugger = false;

struct ExceptionSafeResetInvokedInternalDebugger {
  ExceptionSafeResetInvokedInternalDebugger() { global_invokedInternalDebugger = true; };
  virtual ~ExceptionSafeResetInvokedInternalDebugger() { global_invokedInternalDebugger = false; }
};

void stackSizeWarning(size_t stackUsed) {
  if (!global_invokedInternalDebugger) {
    int x;
    char* xaddr = (char*)(&x);
    printf("%s:%d Stack is getting full currently at %zu bytes - warning at %u bytes  top@%p current@%p\n", __FILE__, __LINE__,
           stackUsed, globals_->_StackWarnSize, my_thread_low_level->_StackTop, xaddr);
    ExceptionSafeResetInvokedInternalDebugger safe;
    core__invoke_internal_debugger(nil<core::T_O>());
  }
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(Return the stack warn size)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__stack_limit() { return clasp_make_fixnum(globals_->_StackWarnSize); };

CL_LAMBDA(&optional (exit-value 0));
CL_DECLARE();
CL_DOCSTRING(R"dx(exit)dx");
DOCGROUP(clasp);
[[noreturn]] CL_DEFUN void core__exit(int exitValue) {
#ifdef CLASP_APPLE_SILICON
  exit(exitValue);
#else
  gctools::global_debuggerOnSIGABRT = false;
  if (exitValue != 0) {
    if (core::_sym_STARexit_backtraceSTAR->symbolValue().notnilp()) {
      dbg_safe_backtrace();
    }
  }
  if (getenv("CLASP_TIME_EXIT")) {
    atexit(first_exit);
  }
  VirtualMachine& vm = my_thread->_VM;
  vm.shutdown();
  exit(exitValue);
#endif
};

CL_LAMBDA(&optional (exit-value 0));
CL_DECLARE();
CL_DOCSTRING(R"dx(C exit)dx");
DOCGROUP(clasp);
CL_DEFUN void core__cexit(int exitValue) { exit(exitValue); };

CL_LAMBDA(&optional (exit-value 0));
CL_DECLARE();
CL_DOCSTRING(R"dx(C exit)dx");
DOCGROUP(clasp);
CL_DEFUN void core__c_UNDERSCORE_exit(int exitValue) { _exit(exitValue); };

CL_LAMBDA(&optional (exit-value 0));
CL_DECLARE();
CL_DOCSTRING(R"dx(quit)dx");
DOCGROUP(clasp);
CL_DEFUN void core__quit(int exitValue) { core__exit(exitValue); };

CL_DOCSTRING(R"dx(abort)dx");
DOCGROUP(clasp);
CL_DEFUN void core__cabort() { abort(); };

CL_LAMBDA(key datum alist);
CL_DECLARE();
CL_DOCSTRING(R"dx(acons)dx");
DOCGROUP(clasp);
CL_DEFUN List_sp cl__acons(T_sp key, T_sp val, T_sp alist) {
  Cons_sp acons = Cons_O::create(key, val);
  return Cons_O::create(acons, alist);
}

CL_LAMBDA(item alist &key key test test-not);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(assoc)dx");
DOCGROUP(clasp);
CL_DEFUN List_sp cl__assoc(T_sp item, List_sp alist, T_sp key, T_sp test, T_sp test_not) {
  if (alist.nilp())
    return alist;
  return alist.asCons()->assoc(item, key, test, test_not);
}

CL_LAMBDA(item list &key key test test-not);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(See CLHS member)dx");
DOCGROUP(clasp);
CL_DEFUN List_sp cl__member(T_sp item, T_sp tlist, T_sp key, T_sp test, T_sp test_not) {
  if (tlist.consp()) {
    Cons_sp list = gc::As_unsafe<Cons_sp>(tlist);
    return (list->member(item, key, test, test_not));
  }
  if (tlist.nilp())
    return nil<T_O>();
  ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_member, 2, tlist, cl::_sym_list);
  UNREACHABLE();
}

CL_LAMBDA(item list test test-not key);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(Like member but if a key function is provided then apply it to the item. See ecl::list.d::member1)dx");
DOCGROUP(clasp);
CL_DEFUN List_sp core__member1(T_sp item, List_sp list, T_sp test, T_sp test_not, T_sp key) {
  if (list.nilp())
    return list;
  return list.asCons()->member1(item, key, test, test_not);
}

/*
  __BEGIN_DOC( candoScript.general.getline, subsection, getline)
  \scriptCmdRet{getline}{}{String::result}

  Read a line from stdin
  __END_DOC
*/

CL_LAMBDA("&optional (prompt \"\")");
CL_DECLARE();
CL_DOCSTRING(R"dx(getline)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__getline(String_sp prompt) {
  ASSERT(cl__stringp(prompt));
  string res;
  string sprompt(prompt->get_std_string());
  bool end_of_transmission;
  res = myReadLine(sprompt.c_str(), end_of_transmission);
  SimpleBaseString_sp result = SimpleBaseString_O::make(res);
  return result;
}

CL_DOCSTRING(R"dx(lookup-class-with-stamp)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__lookup_class_with_stamp(Fixnum stamp) {
  HashTable_sp classNames = _lisp->_Roots._ClassTable;
  T_sp foundClass = nil<T_O>();
  classNames->maphash([stamp, &foundClass](T_sp key, T_sp tclass_) {
    Instance_sp class_ = gc::As<Instance_sp>(tclass_);
    if (class_->CLASS_stamp_for_instances() == stamp) {
      foundClass = class_;
    }
  });
  return foundClass;
}

CL_LAMBDA(symbol &optional env);
CL_DECLARE();
CL_DOCSTRING(R"dx(Return the class holder that contains the class.)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__find_class_holder(Symbol_sp symbol, T_sp env) {
#ifdef SYMBOL_CLASS
  return symbol->find_class_holder();
#else
  //  ASSERTF(env.nilp(), "Handle non nil environment");
  // Should only be single threaded here
  if (_lisp->bootClassTableIsValid()) {
    return _lisp->boot_findClassHolder(symbol, false);
  }
  // Use the same global variable that ECL uses
  bool foundp;
  ClassHolder_sp cell;
  HashTable_sp classNames = _lisp->_Roots._ClassTable;
  T_mv mc = classNames->gethash(symbol, nil<T_O>());
  MultipleValues& mvn = core::lisp_multipleValues();
  foundp = mvn.valueGet(1, mc.number_of_values()).notnilp();
  if (!foundp) {
    cell = ClassHolder_O::create(unbound<Instance_O>());
    classNames->setf_gethash(symbol, cell);
  } else {
    cell = gc::As_unsafe<ClassHolder_sp>(mc);
  }
  return cell;
#endif
}

CL_LAMBDA(symbol &optional (errorp t) env);
CL_DECLARE();
CL_DOCSTRING(R"dx(find-class)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__find_class(Symbol_sp symbol, bool errorp, T_sp env) {
  // ASSERTF(env.nilp(), "Handle non nil environment");
  T_sp ch = core__find_class_holder(symbol, env);
  if (ch.nilp()) {
    printf("%s:%d core__find_class_holder returned NIL for symbol %s\n", __FILE__, __LINE__, symbol->formattedName(true).c_str());
    abort();
  }
  ClassHolder_sp cell = gc::As<ClassHolder_sp>(ch);
  if (cell->class_unboundp()) {
    if (errorp) {
      ERROR(ext::_sym_undefinedClass, Cons_O::createList(kw::_sym_name, symbol));
    }
    return nil<T_O>();
  }
  return cell->class_get();
}

CL_LAMBDA(new-value name);
CL_DECLARE();
CL_DOCSTRING(R"dx(setf_find_class, set value to NIL to remove the class name )dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__setf_find_class(T_sp newValue, Symbol_sp name) {
#ifdef SYMBOL_CLASS
  name->setf_find_class(newValue);
  return newValue;
#else
  if (_lisp->bootClassTableIsValid()) {
    if (newValue.nilp()) {
      printf("%s:%d Trying to (setf-find-class nil %s) when bootClassTableIsValid (while boostrapping)\n", __FILE__, __LINE__,
             _rep_(name).c_str());
    }
    return _lisp->boot_setf_findClass(name, gc::As<Instance_sp>(newValue));
  }
  HashTable_sp ht = _lisp->_Roots._ClassTable;
  T_sp tcell = ht->gethash(name, nil<T_O>());
  if (tcell.notnilp()) {
    ClassHolder_sp cell = gc::As_unsafe<ClassHolder_sp>(tcell);
    if (newValue.nilp()) {
      cell->class_mkunbound();
      return nil<T_O>();
    }
    // Replace the class in the CAR of the cell
    cell->class_set(gc::As_unsafe<Instance_sp>(newValue));
    return newValue;
  }
  // tcell is NIL
  if (newValue.notnilp()) {
    // newValue is not NIL
    ClassHolder_sp cell = ClassHolder_O::create(gc::As_unsafe<Instance_sp>(newValue));
    ht->hash_table_setf_gethash(name, cell);
  }
  return newValue;
#endif
};

CL_LAMBDA(partialPath);
CL_DECLARE();
CL_DOCSTRING(R"dx(findFileInLispPath)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__find_file_in_lisp_path(String_sp partialPath) {
  ASSERT(cl__stringp(partialPath));
  LOG("PartialPath=[{}]", partialPath->get());
  Path_sp fullPath = _lisp->translateLogicalPathnameUsingPaths(partialPath);
  LOG("fullPath is {}", fullPath->asString());
  return fullPath;
}

CL_LAMBDA(name-desig);
CL_DECLARE();
CL_DOCSTRING(R"dx(See CLHS: find-package)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__find_package(T_sp name_desig) {
  if (Package_sp pkg = name_desig.asOrNull<Package_O>())
    return pkg;
  String_sp name = coerce::stringDesignator(name_desig);
  // TODO: Support wide string package names
  return _lisp->findPackage(name);
}

CL_LAMBDA(package-designator);
CL_DECLARE();
CL_DOCSTRING(R"dx(selectPackage)dx");
DOCGROUP(clasp);
CL_DEFUN void core__select_package(T_sp package_designator) {
  Package_sp pkg = coerce::packageDesignator(package_designator);
  _lisp->selectPackage(pkg);
}

/*
  __BEGIN_DOC(candoScript.general.mpiEnabled,mpiEnabled)
  \scriptCmdRet{mpiEnabled}{}{}

  Return true if MPI is enabled.
  __END_DOC
*/

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(mpi_enabled)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__mpi_enabled() { return _lisp->_boolean(_lisp->mpiEnabled()); }

/*
  __BEGIN_DOC(candoScript.general.mpiRank,mpiRank)
  \scriptCmdRet{mpiRank}{}{}

  Return the mpi rank or 0 if not enabled.
  __END_DOC
*/
CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(Return the mpi_rank or 0 if mpi is disabled)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__mpi_rank() { return make_fixnum(_lisp->mpiRank()); }

/*
  __BEGIN_DOC(candoScript.general.mpiSize,mpiSize)
  \scriptCmdRet{mpiSize}{}{}

  Return the mpi rank or 0 if not enabled.
  __END_DOC
*/

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(Return mpi_size or 0 if mpi is not enabled)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__mpi_size() { return make_fixnum(_lisp->mpiSize()); }

CL_LAMBDA(form &optional env);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(macroexpand_1)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv cl__macroexpand_1(T_sp form, T_sp env) {
  T_sp expansionFunction = nil<T_O>();
  if (form.consp()) {
    Cons_sp cform(reinterpret_cast<gctools::Tagged>(form.raw_()));
    T_sp head = cons_car(cform);
    if (cl__symbolp(head)) {
      expansionFunction = cl__macro_function(gc::As_unsafe<Symbol_sp>(head), env);
    }
  } else if (Symbol_sp sform = form.asOrNull<Symbol_O>()) {
    expansionFunction = ext__symbol_macro(sform, env);
  }
  if (expansionFunction.notnilp()) {
    T_sp macroexpandHook = cl::_sym_STARmacroexpand_hookSTAR->symbolValue();
    Function_sp hookFunc = coerce::calledFunctionDesignator(macroexpandHook);
    T_sp expanded = eval::funcall(hookFunc, expansionFunction, form, env);
    return (Values(expanded, _lisp->_true()));
  } else
    return Values(form, nil<T_O>());
}

CL_LAMBDA(form &optional env);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(macroexpand)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv cl__macroexpand(T_sp form, T_sp env) {
  ASSERT(env.generalp());
  bool sawAMacro = false;
  bool expandedMacro = false;
  uint macroExpansionCount = 0;
  if (_sym_STARdebugMacroexpandSTAR->symbolValue().isTrue()) {
    printf("%s:%d - macroexpanding --> %s\n", __FILE__, 2551, _rep_(form).c_str());
  }
  MultipleValues& mvn = core::lisp_multipleValues();
  T_sp cur = form;
  do {
    T_mv rmv = cl__macroexpand_1(cur, env);
    cur = rmv;
    sawAMacro = gc::As<T_sp>(mvn.valueGet(1, rmv.number_of_values())).isTrue();
    expandedMacro |= sawAMacro;
    macroExpansionCount++;
    if (macroExpansionCount > 100) {
      SIMPLE_ERROR("Macro expansion happened {} times - You may have a macro expansion infinite loop", macroExpansionCount);
    }
  } while (sawAMacro);
  if (_sym_STARdebugMacroexpandSTAR->symbolValue().isTrue()) {
    printf("%s:%d -     after macroexpanding --> %s\n", __FILE__, 2565, _rep_(cur).c_str());
  }
  return (Values(cur, _lisp->_boolean(expandedMacro)));
};

void searchForApropos(List_sp packages, SimpleString_sp insubstring, bool print_values) {
  SimpleString_sp substring = cl__string_upcase(insubstring);
  FindApropos apropos(substring);
  LOG("Searching for symbols apropos to({})", substring);
  for (auto cur : packages) {
    Package_sp pkg = gc::As<Package_sp>(oCar(cur));
    pkg->mapExternals(&apropos);
    pkg->mapInternals(&apropos);
  }
  apropos._symbols->mapHash([&print_values](T_sp key, T_sp dummy) {
    stringstream ss;
    Symbol_sp sym = gc::As<Symbol_sp>(key);
    ss << std::setw(50) << std::setfill(' ') << (sym)->fullName();
    if ((sym)->specialP() || (sym)->fboundp()) {
      if ((sym)->fboundp()) {
        ss << " ";
        ss << cl__class_of(cl__symbol_function((sym)))->_classNameAsString();
        T_sp tfn = cl__symbol_function(sym);
        if (!tfn.unboundp() && gc::IsA<Function_sp>(tfn)) {
          if (sym->macroP())
            ss << "(MACRO)";
        }
      }
      if (!(sym)->boundP()) {
        ss << " !!UNDEFINED!!";
      } else {
        if ((sym)->specialP() || (sym)->boundP()) {
          ss << " VALUE";
          if (print_values) {
            stringstream sval;
            T_sp symVal = (sym)->symbolValue();
            sval << _rep_(symVal);
            ss << ": " << sval.str().substr(0, 50);
          }
        }
      }
    }
    core::clasp_writeln_string(ss.str());
  });
}

/*
  __BEGIN_DOC(candoScript.general.apropos,apropos)
  \scriptCmdRet{apropos}{}{Text::substring [packageName]}

  Return every symbol that contains the (substring)
  __END_DOC
*/

CL_LAMBDA(string-desig &optional package-desig);
CL_DECLARE();
CL_DOCSTRING(R"dx(apropos)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__apropos(T_sp string_desig, T_sp package_desig) {
  ASSERT(cl__stringp(string_desig));
  // TODO: Switch to proper common lisp strings
  String_sp string = coerce::stringDesignator(string_desig);
  SimpleString_sp substring = coerce::simple_string(string);
  List_sp packages(nil<List_V>());
  if (package_desig.nilp()) {
    packages = _lisp->allPackagesAsCons();
  } else {
    Package_sp pkg = coerce::packageDesignator(package_desig);
    packages = Cons_O::create(pkg, nil<T_O>());
  }
  searchForApropos(packages, substring, false);
  return (Values(nil<T_O>()));
}

/*! I should probably get the key for each element first and then sort */
class OrderBySortFunction {
private:
  T_sp _SortFunction;
  T_sp _KeyFunction;
  Cons_sp _args;

public:
  OrderBySortFunction(Function_sp proc, Function_sp key) {
    this->_SortFunction = proc;
    this->_KeyFunction = key;
    this->_args = Cons_O::createList(nil<T_O>(), nil<T_O>());
  }
  bool operator()(T_sp x, T_sp y) {
    if (this->_KeyFunction.nilp()) {
      return T_sp(eval::funcall(this->_SortFunction, x, y)).isTrue();
    } else {
      T_sp kx = eval::funcall(this->_KeyFunction, x);
      T_sp ky = eval::funcall(this->_KeyFunction, y);
      return T_sp(eval::funcall(this->_SortFunction, kx, ky)).isTrue();
    }
  }
};

CL_LAMBDA(sequence predicate &key key);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(Like CLHS: sort but the sequence is not destructively sorted. Instead a new sequence is returned.)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__sort(List_sp sequence, T_sp predicate, T_sp key) {
  gctools::Vec0<T_sp> sorted;
  Function_sp sortProc = coerce::functionDesignator(predicate);
  LOG("Unsorted data: {}", _rep_(sequence));
  if (cl__length(sequence) == 0)
    return nil<T_O>();
  fillVec0FromCons(sorted, sequence);
  LOG("Sort function: {}", _rep_(sortProc));
  OrderBySortFunction orderer(sortProc,
                              key.nilp() ? coerce::functionDesignator(cl::_sym_identity) : coerce::functionDesignator(key));
  sort::quickSortVec0(sorted, 0, sorted.size(), orderer);
  List_sp result = asCons(sorted);
  return result;
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(debugLogOn)dx");
DOCGROUP(clasp);
CL_DEFUN void core__debug_log_on() {
  _lisp->debugLog().setSuppressMessages(false);
  LOG("Turning debugLogOn");
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(debugLogOff)dx");
DOCGROUP(clasp);
CL_DEFUN void core__debug_log_off() { _lisp->debugLog().setSuppressMessages(true); }

CL_LAMBDA(symDes &optional (packageDes *package*));
CL_DECLARE();
CL_DOCSTRING(R"dx(CLHS: export)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__export(T_sp symDes, T_sp packageDes) {
  List_sp symbols = coerce::listOfSymbols(symDes);
  Package_sp package = coerce::packageDesignator(packageDes);
  for (auto sym : symbols) {
    package->_export2(gc::As<Symbol_sp>(oCar(sym)));
  }
  return _lisp->_true();
}

CL_LAMBDA(symbols &optional package);
CL_DECLARE();
CL_DOCSTRING(R"dx(Unexport the symbols from the package. See CLHS.)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__unexport(T_sp symbolsDes, T_sp packageDes) {
  List_sp symbols = coerce::listOfSymbols(symbolsDes);
  Package_sp package = coerce::packageDesignator(packageDes);
  for (auto sym : symbols) {
    package->unexport(gc::As<Symbol_sp>(oCar(sym)));
  }
  return _lisp->_true();
}

CL_LAMBDA(symbol-name &optional (package-desig *package*));
CL_DECLARE();
CL_DOCSTRING(R"dx(See CLHS: intern)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv cl__intern(String_sp symbol_name, T_sp package_desig) {
  Package_sp package = coerce::packageDesignator(package_desig);
  if (gc::IsA<StrNs_sp>(symbol_name)) {
    return package->intern(gc::As_unsafe<StrNs_sp>(symbol_name)->asMinimalSimpleString());
  } else if (gc::IsA<SimpleString_sp>(symbol_name)) {
    return package->intern(gc::As_unsafe<SimpleString_sp>(symbol_name));
  }
  TYPE_ERROR(symbol_name, cl::_sym_string);
}

CL_LAMBDA(continue-string datum initializers);
CL_DECLARE();
CL_DOCSTRING(R"dx(universalErrorHandler)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__universal_error_handler(T_sp continueString, T_sp datum, List_sp initializers) {
  if (cl__stringp(datum)) {
    cl__format(_lisp->_true(), datum, initializers);
  } else {
    stringstream ss;
    ss << "datum: " << _rep_(datum) << " " << _rep_(initializers);
    printf("%s\n", ss.str().c_str());
  }
  dbg_hook("universalErrorHandler");
  if (global_options->_Interactive) {
    core__invoke_internal_debugger(nil<T_O>());
  }
  abort();
};

CL_LAMBDA(&optional condition);
CL_DECLARE();
CL_DOCSTRING(R"dx(invokeInternalDebugger)dx");
DOCGROUP(clasp);
[[noreturn]] CL_DEFUN void core__invoke_internal_debugger(T_sp condition) {
  clasp_write_string(fmt::format("{}:{} core__invoke_internal_debugger --> {}\n", __FILE__, __LINE__, _rep_(condition).c_str()));
  early_debug(condition, false);
  printf("%s:%d Cannot continue\n", __FILE__, __LINE__);
  abort();
};

CL_LAMBDA();
CL_DOCSTRING(R"dx(invokeInternalDebuggerFromGdb)dx");
DOCGROUP(clasp);
CL_DEFUN void core__invoke_internal_debugger_from_gdb() {
  eval::funcall(_sym_invokeInternalDebugger);
  SIMPLE_ERROR("This should never happen");
};

CL_LAMBDA(datum &rest arguments);
CL_DECLARE((optimize (debug 3)));
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
NEVER_OPTIMIZE
CL_DEFUN void cl__error(T_sp datum, List_sp initializers) {
  // These are volatile in an effort to make them available to debuggers.
  [[maybe_unused]] volatile T_sp saved_datum = datum;
  [[maybe_unused]] volatile List_sp saved_initializers = initializers;
  T_sp objErrorDepth = _sym_STARnestedErrorDepthSTAR->symbolValue();
  int nestedErrorDepth;
  /* *nested-error-depth* should be a fixnum, but if it's not we can't signal
   * an error without infinite regression. As sort of a KLUDGE, we just bind
   * it to zero instead. This does mean that if nested error handlers
   * repeatedly reset it to some non-fixnum, unchecked infinite error depth
   * could occur. FIXME. */
  if (gc::IsA<Fixnum_sp>(objErrorDepth))
    nestedErrorDepth = unbox_fixnum(objErrorDepth);
  else
    nestedErrorDepth = 0;
  if (nestedErrorDepth > 10) {
    fprintf(stderr, "%s:%d -- *nested-error-depth* --> %d  datum: %s\n", __FILE__, __LINE__, nestedErrorDepth, _rep_(datum).c_str());
    if (initializers.notnilp()) {
      fprintf(stderr, "               initializers: %s\n", _rep_(initializers).c_str());
    }
    fprintf(stderr, "Dumping backtrace\n");
    dbg_safe_backtrace();
    fflush(stderr);
    gctools::truly_abort();
  }
  call_with_variable_bound(_sym_STARnestedErrorDepthSTAR, make_fixnum(nestedErrorDepth + 1), [&]() {
    if (_sym_universalErrorHandler->fboundp()) {
      Function_sp fn = _sym_universalErrorHandler->symbolFunction();
      eval::funcall(fn, nil<T_O>(), datum, initializers);
    }
    THROW_HARD_ERROR("cl__error should never return because universal-error-handler should never return - but it did");
  });
}

CL_LAMBDA(cformat eformat &rest arguments);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(See CLHS cerror)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__cerror(T_sp cformat, T_sp eformat, List_sp arguments) {
  eval::funcall(_sym_universalErrorHandler, cformat, eformat, arguments);
  return nil<T_O>();
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(Turn on the stepper.)dx");
DOCGROUP(clasp);
CL_DEFUN void core__set_breakstep() { my_thread->_Breakstep = true; }

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(Turn off the stepper.)dx");
DOCGROUP(clasp);
CL_DEFUN void core__unset_breakstep() { my_thread->_Breakstep = false; }

CL_LAMBDA ();
CL_DECLARE();
CL_DOCSTRING(R"dx(Return whether we are stepping or not.)dx");
DOCGROUP(clasp);
CL_DEFUN bool core__breakstepping_p() { return my_thread->_Breakstep; }

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(Return a string representation of the object)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__repr(T_sp obj) {
  SimpleBaseString_sp res = SimpleBaseString_O::make(_rep_(obj));
  return res;
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(not)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__not(T_sp x) { return _lisp->_boolean(!x.isTrue()); };

Instance_sp Lisp::boot_setf_findClass(Symbol_sp className, Instance_sp mc) {
  //    printf("%s:%d    boot_setf_findClass for %s\n", __FILE__, __LINE__, _rep_(className).c_str());
  for (auto it = this->_Roots.bootClassTable.begin(); it != this->_Roots.bootClassTable.end(); ++it) {
    if (it->symbol == className) {
      it->theClassHolder->class_set(mc);
      return mc;
    }
  }
  SymbolClassHolderPair sc(className, ClassHolder_O::create(mc));
  this->_Roots.bootClassTable.push_back(sc);
  return mc;
}

T_sp Lisp::boot_findClassHolder(Symbol_sp className, bool errorp) const {
  ASSERTF(this->_BootClassTableIsValid, "Never use Lisp::findClass after boot - use cl::_sym_findClass");
  for (auto it = this->_Roots.bootClassTable.begin(); it != this->_Roots.bootClassTable.end(); ++it) {
    if (it->symbol == className)
      return it->theClassHolder;
  }
  return nil<T_O>();
}

/*! After the core classes are defined and we have hash-tables, move all class definitions
  into the *class-name-hash-table*.  We do this because we can't put stuff into a hash-table
  until the hash-table class is defined and we need classes in the *class-name-hash-table* once
  CLOS starts up because that is where ECL expects to find them. */
void Lisp::switchToClassNameHashTable() {
  ASSERTF(this->_BootClassTableIsValid, "switchToClassNameHashTable should only be called once after boot");
  HashTable_sp ht = _lisp->_Roots._ClassTable;
  for (auto it = this->_Roots.bootClassTable.begin(); it != this->_Roots.bootClassTable.end(); ++it) {
    ht->hash_table_setf_gethash(it->symbol, it->theClassHolder);
  }
  this->_Roots.bootClassTable.clear();
  this->_BootClassTableIsValid = false;
}

void Lisp::parseStringIntoPackageAndSymbolName(const string& name, bool& packageDefined, Package_sp& package, string& symbolName,
                                               bool& exported) const {

  packageDefined = true; // this will be true in most cases below
  LOG("Parsing string[{}] into package and symbol name", name);
  if (name[0] == ':') {
    LOG("It's a keyword");
    package = this->_Roots._KeywordPackage;
    symbolName = name.substr(1, 99999);
    exported = true;
    return;
  }
  if (name[0] == '&') {
    LOG("It's an & symbol");
    package = this->_Roots._CorePackage;
    symbolName = name;
    exported = true;
    return;
  }
  size_t colonPos = name.find_first_of(":");
  exported = true;
  if (colonPos == string::npos) {
    LOG("Package wasn't defined");
    symbolName = name;
    packageDefined = false;
    return;
  }
  bool doubleColon = false;
  size_t secondPart = colonPos + 1;
  if (name[secondPart] == ':') {
    LOG("It's a non-exported symbol");
    exported = false;
    doubleColon = true;
    secondPart++;
    if (name.find_first_of(":", secondPart) != string::npos) {
      SIMPLE_ERROR("There can only be one ':' or '::' in a symbol name");
    }
  }
  package = gc::As<Package_sp>(this->findPackage(name.substr(0, colonPos), true));
  symbolName = name.substr(secondPart, 99999);
  LOG("It's a packaged symbol ({} :: {})", _rep_(package->name()), symbolName);
  return;
}

Symbol_mv Lisp::intern(const string& name, T_sp optionalPackageDesignator) {
  Package_sp package;
  string symbolName;
  symbolName = name;
  package = coerce::packageDesignator(optionalPackageDesignator);
  ASSERTNOTNULL(package);
  ASSERT(package.notnilp());
  SimpleBaseString_sp sname = SimpleBaseString_O::make(symbolName);
  T_mv symStatus = package->intern(sname);
  Symbol_sp sym = gc::As<Symbol_sp>(symStatus);
  MultipleValues& mvn = core::lisp_multipleValues();
  T_sp status = mvn.second(symStatus.number_of_values());
  return Values(sym, status);
}

/*! The optionalPackageDesignator is nil */
Symbol_sp Lisp::intern(string const& symbolName) {
  Package_sp curPackage = this->getCurrentPackage();
  ASSERTNOTNULL(curPackage);
  return this->intern(symbolName, curPackage);
}

Symbol_sp Lisp::findSymbol(const string& name, T_sp optionalPackageDesignator) const {
  Package_sp package;
  string symbolName;
  bool exported, packageDefined;
  this->parseStringIntoPackageAndSymbolName(name, packageDefined, package, symbolName, exported);
  if (!packageDefined) {
    package = coerce::packageDesignator(optionalPackageDesignator);
  }
  return gc::As<Symbol_sp>(package->findSymbol(symbolName));
}

Symbol_sp Lisp::findSymbol(const string& symbolName /*, T_sp optionalPackageDesignator = nil */) const {
  return this->findSymbol(symbolName, nil<T_O>());
}

Symbol_sp Lisp::intern(string const& symbolName, string const& packageName) {
  T_sp package = this->findPackage(packageName);
  return this->intern(symbolName, package);
}

Symbol_sp Lisp::internUniqueWithPackageName(string const& packageName, string const& symbolName) {
  T_sp package = this->findPackage(packageName);
  Symbol_mv symStatus = this->intern(symbolName, package);
  //  T_sp status = symStatus.second();
  return symStatus;
}

Symbol_sp Lisp::internWithPackageName(string const& packageName, string const& symbolName) {
  Package_sp package = gc::As<Package_sp>(this->findPackage(packageName, true));
  return this->intern(symbolName, package);
}

Symbol_sp Lisp::internWithDefaultPackageName(string const& defaultPackageName, string const& possiblePackagePrefixedSymbolName) {
  size_t pkgSep = possiblePackagePrefixedSymbolName.find(':', 0);
  if (pkgSep == string::npos) {
    return this->internWithPackageName(defaultPackageName, possiblePackagePrefixedSymbolName);
  }
  size_t symbolNameStart = pkgSep + 1;
  bool exportit = true;
  if (symbolNameStart < possiblePackagePrefixedSymbolName.size() - 1) {
    if (possiblePackagePrefixedSymbolName[symbolNameStart] == ':') {
      ++symbolNameStart;
      exportit = false;
    }
  }
  string packageName = possiblePackagePrefixedSymbolName.substr(0, pkgSep);
  string symbolName = possiblePackagePrefixedSymbolName.substr(symbolNameStart, possiblePackagePrefixedSymbolName.size());
  Symbol_sp sym = this->internWithPackageName(packageName, symbolName);
  if (exportit)
    sym->exportYourself();
  return sym;
}

Symbol_sp Lisp::internKeyword(const string& name) {
  string realName = name;
  boost::to_upper(realName);
  SimpleBaseString_sp str_real_name = SimpleBaseString_O::make(realName);
  return gc::As<Symbol_sp>(this->_Roots._KeywordPackage->intern(str_real_name));
}

void Lisp::dump_apropos(const char* part) const {
  SimpleBaseString_sp substring = SimpleBaseString_O::make(std::string(part));
  List_sp packages = _lisp->allPackagesAsCons();
  searchForApropos(packages, substring, true);
}

bool Lisp::load(int& exitCode) {
  MultipleValues& mvn = core::lisp_multipleValues();
  switch (global_options->_StartupType) {
  case cloInitLisp: {
    Pathname_sp initPathname = cl__pathname(SimpleBaseString_O::make(globals_->_InitFileName));
    if (!global_options->_SilentStartup) {
      printf("Loading image %s\n", _rep_(initPathname).c_str());
    }
    T_mv result = core__load_no_package_set(initPathname);
    if (result.nilp()) {
      T_sp err = mvn.second(result.number_of_values());
      printf("Could not load %s error: %s\n", _rep_(initPathname).c_str(), _rep_(err).c_str());
      exitCode = 1;
      return false;
    }
  } break;
  case cloBaseImage:
  case cloExtensionImage:
  case cloImageFile:
      if (startup_functions_are_waiting()) {
        startup_functions_invoke(NULL);
      } else {
        Pathname_sp initPathname = gc::As<Pathname_sp>(_sym_STARcommandLineImageSTAR->symbolValue());
        if (!global_options->_SilentStartup) {
          printf("Loading image %s\n", _rep_(initPathname).c_str());
        }
        T_mv result = eval::funcall(cl::_sym_load, initPathname); // core__load_bundle(initPathname);
        if (result.nilp()) {
          T_sp err = mvn.second(result.number_of_values());
          printf("Could not load bundle %s error: %s\n", _rep_(initPathname).c_str(), _rep_(err).c_str());
          exitCode = 1;
          return false;
        }
        char* pause_startup = getenv("CLASP_PAUSE_OBJECTS_ADDED");
        if (pause_startup) {
          gctools::setup_user_signal();
          gctools::wait_for_user_signal("Paused at startup after object files added");
        }
      }
      break;
  default:
      break;
  }
  return true;
};

int Lisp::run() {
  if (ext::_sym_STARtoplevel_hookSTAR->symbolValue().notnilp()) {
    core::T_sp fn = ext::_sym_STARtoplevel_hookSTAR->symbolValue();
    core::eval::funcall(fn);
  } else if (global_options->_Interactive) {
    this->readEvalPrintInteractive();
  }

  return 0;
};

FileScope_mv Lisp::getOrRegisterFileScope(const string& fileName) {
  SimpleBaseString_sp sfileName = SimpleBaseString_O::make(fileName);
  {
    WITH_READ_LOCK(globals_->_SourceFilesMutex);
    T_sp it = this->_Roots._SourceFileIndices->gethash(sfileName);
    if (it.notnilp()) {
      FileScope_sp sfi = this->_Roots._SourceFiles[it.unsafe_fixnum()];
      return Values(sfi, make_fixnum(it.unsafe_fixnum()));
    }
  }
  {
    WITH_READ_WRITE_LOCK(globals_->_SourceFilesMutex);
    T_sp it = this->_Roots._SourceFileIndices->gethash(sfileName);
    if (it.nilp()) {
      if (this->_Roots._SourceFiles.size() == 0) {
        FileScope_sp unknown = FileScope_O::create("-unknown-file-", 0);
        this->_Roots._SourceFiles.push_back(unknown);
      }
      int idx = this->_Roots._SourceFiles.size();
      this->_Roots._SourceFileIndices->setf_gethash(sfileName, make_fixnum(idx));
      FileScope_sp sfi = FileScope_O::create(fileName, idx);
      this->_Roots._SourceFiles.push_back(sfi);
      return Values(sfi, make_fixnum(idx));
    }
    FileScope_sp sfi = this->_Roots._SourceFiles[it.unsafe_fixnum()];
    return Values(sfi, make_fixnum(it.unsafe_fixnum()));
  }
}

CL_DOCSTRING(R"dx(List all of the source files)dx");
DOCGROUP(clasp);
CL_DEFUN List_sp core__all_source_files() {
  WITH_READ_LOCK(globals_->_SourceFilesMutex);
  return _lisp->_Roots._SourceFileIndices->keysAsCons();
}

void Lisp::mapClassNamesAndClasses(KeyValueMapper* mapper) {
  if (this->_BootClassTableIsValid) {
    SIMPLE_ERROR("What do I do here?");
  } else {
    HashTable_sp ht = _lisp->_Roots._ClassTable;
    ht->lowLevelMapHash(mapper);
  }
}

string Lisp::__repr__() const {
  stringstream ss;
  ss << "Lisp object";
  return ss.str();
};

SYMBOL_EXPORT_SC_(CorePkg, selectPackage);

void Lisp::initializeGlobals(LispPtr lisp) {}

LispHolder::LispHolder(bool mpiEnabled, int mpiRank, int mpiSize) {
  this->lisp_ = Lisp::createLispEnvironment(mpiEnabled, mpiRank, mpiSize);
}

void LispHolder::startup(const CommandLineOptions& options) {
  ::_lisp = this->lisp_;

  if (!globals_->_Bundle) {
    globals_->_Bundle = new Bundle(options._ExecutableName);
  }
  // Start up lisp
  this->lisp_->startupLispEnvironment();

  // Run the initializers
#ifndef SCRAPING
#define ALL_INITIALIZERS_CALLS
#include INITIALIZERS_INC_H
#undef ALL_INITIALIZERS_CALLS
#endif

  _lisp->parseCommandLineArguments(options);
}

LispHolder::~LispHolder() {
#ifndef SCRAPING
#define ALL_TERMINATORS_CALLS
#include TERMINATORS_INC_H
#undef ALL_TERMINATORS_CALLS
#endif
  this->lisp_->shutdownLispEnvironment();
}

Exposer_O::Exposer_O(LispPtr lisp, const string& packageName) {
  this->_PackageName = packageName;
  if (!lisp->recognizesPackage(packageName)) {
    list<string> lnnames;
    list<string> lpkgs;
    lisp->makePackage(packageName, lnnames, lpkgs);
  }
}

ChangePackage::ChangePackage(Package_sp newPackage) : _SavedPackage(_lisp->getCurrentPackage()) {
  _lisp->selectPackage(newPackage);
}

ChangePackage::~ChangePackage() { _lisp->selectPackage(this->_SavedPackage); }

SYMBOL_SC_(CorePkg, find_single_dispatch_generic_function);
SYMBOL_SC_(CorePkg, setf_find_single_dispatch_generic_function);
SYMBOL_SC_(CorePkg, forget_all_single_dispatch_generic_functions);
SYMBOL_SC_(CorePkg, invokeInternalDebugger);
SYMBOL_SC_(CorePkg, invokeInternalDebuggerFromGdb);
SYMBOL_SC_(CorePkg, universalErrorHandler);
SYMBOL_SC_(CorePkg, stackUsed);
SYMBOL_SC_(CorePkg, exit);
SYMBOL_SC_(CorePkg, quit);
SYMBOL_SC_(CorePkg, getline);
SYMBOL_SC_(ExtPkg, system);
SYMBOL_EXPORT_SC_(ClPkg, apropos);
SYMBOL_EXPORT_SC_(ClPkg, export);
SYMBOL_EXPORT_SC_(ClPkg, intern);
SYMBOL_SC_(CorePkg, isTopLevelScript);
SYMBOL_SC_(CorePkg, sourcePathname);
SYMBOL_SC_(CorePkg, sourceLineColumn);
SYMBOL_SC_(CorePkg, findFileInLispPath);
SYMBOL_EXPORT_SC_(ClPkg, findClass);
SYMBOL_SC_(CorePkg, setf_findClass);
SYMBOL_SC_(CorePkg, isAssignableTo);
SYMBOL_SC_(CorePkg, isSubClassOf);
SYMBOL_SC_(CorePkg, repr);
SYMBOL_EXPORT_SC_(ClPkg, error);
SYMBOL_EXPORT_SC_(ClPkg, cerror);
SYMBOL_EXPORT_SC_(ExtPkg, setenv);
SYMBOL_EXPORT_SC_(ExtPkg, getenv);
SYMBOL_EXPORT_SC_(ClPkg, not );
SYMBOL_SC_(CorePkg, debugLogOn);
SYMBOL_SC_(CorePkg, debugLogOff);
SYMBOL_SC_(CorePkg, mpi_enabled);
SYMBOL_SC_(CorePkg, mpi_rank);
SYMBOL_SC_(CorePkg, mpi_size);
SYMBOL_SC_(CorePkg, sorted);
SYMBOL_EXPORT_SC_(ClPkg, sort);
SYMBOL_EXPORT_SC_(ClPkg, macroexpand_1);
SYMBOL_EXPORT_SC_(ClPkg, macroexpand);
SYMBOL_SC_(CorePkg, database_dir);
SYMBOL_SC_(CorePkg, script_dir);
SYMBOL_SC_(CorePkg, libraryPath);
SYMBOL_SC_(CorePkg, lispCodePath);
SYMBOL_SC_(CorePkg, setCurrentWorkingDirectory);
SYMBOL_EXPORT_SC_(ClPkg, acons);
SYMBOL_EXPORT_SC_(ClPkg, assoc);
SYMBOL_EXPORT_SC_(ClPkg, member);
SYMBOL_SC_(CorePkg, member1);
SYMBOL_EXPORT_SC_(ClPkg, find_package);

void initialize_Lisp(){};

}; // namespace core
