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
#define DEBUG_LEVEL_FULL

#ifdef USEBOOSTPYTHON
#include <clasp/core/useBoostPython.h>
#endif

#include <errno.h>
#include <dlfcn.h>
#include <sys/wait.h>
#include <stdlib.h>
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunneeded-internal-declaration"
//#pragma GCC diagnostic ignored "-Wunused-local-typedef"
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/program_options.hpp>
#pragma GCC diagnostic pop
//#i n c l u d e	"boost/fstream.hpp"
#include <clasp/gctools/gc_interface.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/allClSymbols.h>
#include <clasp/core/candoOpenMp.h>
#include <clasp/core/exceptions.h>
#include <clasp/core/commandLineOptions.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/compiler.h>
#include <clasp/core/lisp.h>
#include <clasp/core/lispList.h>
#include <clasp/core/loadTimeValues.h>
#include <clasp/core/profiler.h>
#include <clasp/core/bundle.h>
#include <clasp/core/bformat.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/pointer.h>
#include <clasp/core/cons.h>
#include <clasp/core/specialForm.h>
#include <clasp/core/documentation.h>
#include <clasp/core/backquote.h>
#include <clasp/core/testing.h>
#include <clasp/core/bformat.h>
#include <clasp/core/cache.h>
#include <clasp/core/environment.h>
#include <clasp/core/extensionPackage.h>
#include <clasp/core/myReadLine.h>
#include <clasp/core/binder.h>
#include <clasp/core/numbers.h>
#include <clasp/core/bits.h>
#include <clasp/core/load.h>
#include <clasp/core/bignum.h>
//#i n c l u d e "setfExpander.h"
#include <clasp/core/standardObject.h>
#include <clasp/core/ql.h>
#include <clasp/core/str.h>
#include <clasp/core/commonLispPackage.h>
#include <clasp/core/keywordPackage.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/sysprop.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/debugger.h>
#include <clasp/core/builtInClass.h>
#include <clasp/core/standardClass.h>
#include <clasp/core/numberToString.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/lispReader.h>
#include <clasp/core/write_object.h>
#include <clasp/core/write_ugly.h>
#include <clasp/core/lispMath.h>
#include <clasp/core/clcenv.h>
#include <clasp/core/pathname.h>
#include <clasp/core/print.h>
#include <clasp/core/genericFunction.h>
#include <clasp/core/multipleValues.h>
#if defined(XML_ARCHIVE)
#include <xmlLoadArchive.h>
#include <xmlSaveArchive.h>
#endif // defined(XML_ARCHIVE)
#if defined(OLD_SERIALIZE)
#include <clasp/core/serialize.h>
#endif // defined(OLD_SERIALIZE)
#include <clasp/core/bootStrapCoreSymbolMap.h>
#include <clasp/core/numerics.h>
#include <clasp/core/reader.h>
//#i n c l u d e "genericFunction.h"
#include <clasp/core/singleDispatchGenericFunction.h>
#include <clasp/core/designators.h>
#include <clasp/core/unixfsys.h>
#include <clasp/core/sort.h>
#include <clasp/core/bitVector.h>
#include <clasp/core/character.h>
#include <clasp/core/predicates.h>
#include <clasp/core/primitives.h>
#include <clasp/core/package.h>
#include <clasp/core/symbol.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/sequence.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/float_to_digits.h>
#include <clasp/core/num_arith.h>
#include <clasp/core/num_co.h>
#include <clasp/core/lispDefinitions.h>
#include <clasp/core/externalObject.h>
#include <clasp/core/initializeClasses.h>
#include <clasp/core/holder.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/stacks.h>
#include <clasp/core/primitives.h>
#include <clasp/core/readtable.h>
#include <clasp/llvmo/intrinsics.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/python_wrappers.h>

#ifdef READLINE
extern "C" char *readline(const char *prompt);
extern "C" void add_history(char *line);
#endif

/*! A function that creates the source-main: host */
#ifndef PROGRAM_CLBIND
extern void create_source_main_host();
#endif

#ifndef SCRAPING
#define ALL_INITIALIZERS_EXTERN
#include INITIALIZERS_INC_H
#undef ALL_INITIALIZERS_EXTERN
#endif

namespace core {

#if 0
__thread ThreadInfo *threadLocalInfoPtr;
#endif

const int Lisp_O::MaxFunctionArguments = 64; //<! See ecl/src/c/main.d:163 ecl_make_cache(64,4096)
const int Lisp_O::MaxClosSlots = 3;          //<! See ecl/src/c/main.d:164 ecl_make_cache(3,4096)
const int Lisp_O::ClosCacheSize = 1024 * 16;
const int Lisp_O::SingleDispatchMethodCacheSize = 1024 * 8;

extern void lispScannerDebug(std::istream &sin);
extern string getLispError();

int intArray[10];
int& _int_0 = intArray[0];
int& _int_1 = intArray[1];

SMART(BuiltInClass);

struct FindApropos : public KeyValueMapper //, public gctools::StackRoot
                     {
public:
  HashTable_sp _symbols;
  string _substr;
  FindApropos(const string &str) {
    this->_substr = str;
    this->_symbols = HashTableEq_O::create_default();
  };
  virtual bool mapKeyValue(T_sp key, T_sp value) {
    //    Bignum_sp skey = gc::As<Bignum_sp>(key);
    Symbol_sp svalue = gc::As<Symbol_sp>(value);
    string symbolName = lisp_symbolNameAsString(svalue);
    string::size_type pos = symbolName.find(this->_substr);
    //	    LOG(BF("Looking at symbol(%s) for (%s) found: %d") % symbolName % this->_substring % pos );
    if (pos != string::npos) {
      LOG(BF("    It is apropos"));
      this->_symbols->setf_gethash(svalue, _Nil<T_O>());
    }
    return true;
  }
};

//
// Constructor
//
Lisp_O::GCRoots::GCRoots() :
  // _BufferStringPool(_Nil<T_O>()),
                             _MultipleValuesCur(NULL),
                             _BignumRegister0(_Unbound<Bignum_O>()),
                             _BignumRegister1(_Unbound<Bignum_O>()),
                             _BignumRegister2(_Unbound<Bignum_O>())
                             ,
                             _SystemProperties(_Nil<T_O>()),
                             _CatchInfo(_Nil<T_O>()),
                             _SpecialForms(_Unbound<HashTableEq_O>()),
                             _NullStream(_Nil<T_O>()),
                             _PathnameTranslations(_Nil<T_O>()) {}

Lisp_O::Lisp_O() : _StackWarnSize(gctools::_global_stack_max_size * 0.9), // 6MB default stack size before warnings
                   _StackSampleCount(0),
                   _StackSampleSize(0),
                   _StackSampleMax(0),
                   _PrintSymbolsProperly(false),
                   _ReplCounter(1),
                   _Bundle(NULL),
                   _DebugStream(NULL),
                   _SingleStepLevel(UndefinedUnsignedInt),
                   _MpiEnabled(false),
                   _MpiRank(0),
                   _MpiSize(1),
                   _Interactive(true),
                   _EmbeddedInPython(false),
                   _BootClassTableIsValid(true),
                   _PathMax(CLASP_MAXPATHLEN) {
//  this->_Roots._Bindings.reserve(1024); // moved to Lisp_O::initialize()
  this->_TrapIntern = false;
  this->_TrapInternPackage = "";
  this->_TrapInternName = "";
  this->_GlobalInitializationCallbacks.clear();
  this->_MakePackageCallback = NULL;
  this->_ExportSymbolCallback = NULL;
#ifdef CLOS
  this->_Roots._SlotCachePtr.reset_();
  this->_Roots._MethodCachePtr.reset_();
#endif
}

void Lisp_O::shutdownLispEnvironment() {
  if (this->_DebugStream != NULL) {
    this->_DebugStream->beginNode(DEBUG_TOPLEVEL);
  }
  this->_Roots._CommandLineArguments.reset_();
  this->_Roots._Packages.clear();
  //	this->_Roots._HiddenBinder.reset();
  //	this->_Roots._SpecialForms.clear();
  this->_Roots._TrueObject.reset_();

  //    this->_ClassesByClassSymbol.clear();
  if (this->_Bundle != NULL) {
    delete this->_Bundle;
  }
  if (this->_DebugStream != NULL) {
    this->_DebugStream->endNode(DEBUG_TOPLEVEL);
    delete this->_DebugStream;
  }
}

void Lisp_O::lisp_initSymbols(Lisp_sp lisp) {
  Package_sp corePackage = lisp->_Roots._CorePackage;
}

/*! Allocations go here
*/
void Lisp_O::initialize() {
//  printf("%s:%d Initializing _lisp\n", __FILE__, __LINE__ );
  this->_Roots.charInfo.initialize();
}

void Lisp_O::addToStarModulesStar(Symbol_sp sym) {
  _OF();
  List_sp list = cl::_sym_STARmodulesSTAR->symbolValue();
  list = Cons_O::create(sym, list);
  cl::_sym_STARmodulesSTAR->setf_symbolValue(list);
}

template <class oclass>
void setup_static_classSymbol(BootStrapCoreSymbolMap const &sidMap) {
  oclass::___set_static_ClassSymbol(sidMap.lookupSymbol(oclass::static_packageName(), oclass::static_className()));
}

string dump_instanceClass_info(Class_sp co, Lisp_sp prog) {
  stringstream ss;
  ss << "------------------------------------- class" << _rep_(co->className()) << std::endl;
  ;
  LOG(BF("Dumping info: %s") % co->dumpInfo());
  ss << co->dumpInfo();
  return ss.str();
}
template <class oclass>
void dump_info(BuiltInClass_sp co, Lisp_sp lisp) {
  LOG(BF("-------    dump_info    --------------- className: %s @ %X") % oclass::static_className() % co.get());
  LOG(BF("%s::static_classSymbol() = %d") % oclass::static_className() % oclass::static_classSymbol());
  LOG(BF("%s::Base::static_classSymbol() = %d") % oclass::static_className() % oclass::Base::static_classSymbol());
  LOG(BF("%s::static_newNil_callback() = %X") % oclass::static_className() % (void *)(oclass::static_allocator));
  //    LOG(BF("%s")%dump_instanceClass_info(co,lisp));
}

void Lisp_O::setupSpecialSymbols() {
  Symbol_sp symbol_nil = Symbol_O::create_at_boot("NIL");
  Symbol_sp symbol_unbound = Symbol_O::create_at_boot("UNBOUND");
  Symbol_sp symbol_deleted = Symbol_O::create_at_boot("DELETED");
  Symbol_sp symbol_sameAsKey = Symbol_O::create_at_boot("SAME-AS-KEY");
  //TODO: Ensure that these globals are updated by the garbage collector
  gctools::global_tagged_Symbol_OP_nil = reinterpret_cast<Symbol_O *>(symbol_nil.raw_());
  gctools::global_tagged_Symbol_OP_unbound = reinterpret_cast<Symbol_O *>(symbol_unbound.raw_());
  gctools::global_tagged_Symbol_OP_deleted = reinterpret_cast<Symbol_O *>(symbol_deleted.raw_());
  gctools::global_tagged_Symbol_OP_sameAsKey = reinterpret_cast<Symbol_O *>(symbol_sameAsKey.raw_());
  symbol_unbound->_HomePackage = symbol_nil;
  symbol_deleted->_HomePackage = symbol_nil;
  symbol_sameAsKey->_HomePackage = symbol_nil;
}

void Lisp_O::finalizeSpecialSymbols() {
  Symbol_sp symbol_nil = gctools::smart_ptr<Symbol_O>((gc::Tagged)gctools::global_tagged_Symbol_OP_nil);
  symbol_nil->setf_symbolValue(_Nil<T_O>());
  symbol_nil->setf_name(Str_O::create("NIL"));
  symbol_nil->setPackage(_lisp->findPackage("COMMON-LISP"));
  //    	Symbol_sp symbol_unbound = gctools::smart_ptr<Symbol_O>(gctools::global_Symbol_OP_unbound);
  //    	Symbol_sp symbol_deleted = gctools::smart_ptr<Symbol_O>(gctools::global_Symbol_OP_deleted);
  //    	Symbol_sp symbol_sameAsKey = gctools::smart_ptr<Symbol_O>(gctools::global_Symbol_OP_sameAsKey);
}

/*! Run some quick tests to determine if eval::funcall is working properly.
Changes to the calling convention in lispCallingConvention.h must synchronize with code in evaluator.h
Sometimes they get out of sync and this code is designed to trap that.
*/
void run_quick_tests() {
#define TEST_ASSERT_ALWAYS(_x) \
  if (!(_x))                   \
    SIMPLE_ERROR(BF("Test failed"));
  List_sp val1 = eval::funcall(cl::_sym_list, cl::_sym_nil);
  TEST_ASSERT_ALWAYS(cl__length(val1) == 1);
  List_sp val2 = eval::funcall(cl::_sym_list, cl::_sym_nil, cl::_sym_nil);
  TEST_ASSERT_ALWAYS(cl__length(val2) == 2);
  List_sp val3 = eval::funcall(cl::_sym_list, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil);
  TEST_ASSERT_ALWAYS(cl__length(val3) == 3);
  List_sp val4 = eval::funcall(cl::_sym_list, clasp_make_fixnum(1), clasp_make_fixnum(2), clasp_make_fixnum(3), clasp_make_fixnum(4));
  TEST_ASSERT_ALWAYS(cl__length(val4) == 4);
  List_sp val5 = eval::funcall(cl::_sym_list, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil);
  TEST_ASSERT_ALWAYS(cl__length(val5) == 5);
  List_sp val6 = eval::funcall(cl::_sym_list, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil);
  TEST_ASSERT_ALWAYS(cl__length(val6) == 6);
  List_sp val7 = eval::funcall(cl::_sym_list, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil);
  TEST_ASSERT_ALWAYS(cl__length(val7) == 7);

  T_sp num = clasp_make_fixnum(63);
  Real_sp r = gc::As<Real_sp>(num);
}
Lisp_sp Lisp_O::createLispEnvironment(bool mpiEnabled, int mpiRank, int mpiSize) {
  Lisp_O::setupSpecialSymbols();
  ::_lisp = gctools::RootClassAllocator<Lisp_O>::allocate();
  _lisp->initialize();
  _lisp->setupMpi(mpiEnabled, mpiRank, mpiSize);
  //	lisp->__setWeakThis(lisp);
  //	lisp->__resetInitializationOwner();
  _lisp->_DebugStream = new DebugStream(mpiRank);
  LOG(BF("The lisp environment DebugStream has been created"));
  Lisp_O::finalizeSpecialSymbols();
  return _lisp;
}

void Lisp_O::setupMpi(bool mpiEnabled, int mpiRank, int mpiSize) {
  this->_MpiEnabled = mpiEnabled;
  this->_MpiRank = mpiRank;
  this->_MpiSize = mpiSize;
}

#ifdef USE_REFCOUNT
void testContainers() {
  Fixnum_sp fn = make_fixnum(1);
  printf("%s:%d  fn@%p  referenceCount() = %d\n", __FILE__, __LINE__, fn.raw_(), fn->referenceCount());
  gctools::Vec0<T_sp> container;
  container.push_back(fn);
  printf("%s:%d  after push_back to container fn@%p  referenceCount() = %d\n", __FILE__, __LINE__, fn.raw_(), fn->referenceCount());
  Fixnum_sp fn2 = gc::As<Fixnum_sp>(container.back());
  printf("%s:%d  after back to container fn@%p  referenceCount() = %d\n", __FILE__, __LINE__, fn.raw_(), fn->referenceCount());
  container.pop_back();
  printf("%s:%d  after pop_back to container fn@%p  referenceCount() = %d\n", __FILE__, __LINE__, fn.raw_(), fn->referenceCount());
  printf("%s:%d  fn2@%p  referenceCount() = %d\n", __FILE__, __LINE__, fn2.raw_(), fn2->referenceCount());
};
#endif

void testStrings() {
  //  Str_sp str = Str_O::create("This is a test");
  //        printf("%s:%d  Str_sp = %s\n", __FILE__, __LINE__, str->c_str() );
}

void Lisp_O::startupLispEnvironment(Bundle *bundle) {
  { // Trap symbols as they are interned
    stringstream sdebug;
    gctools::get_immediate_info(); // discard result, just testing
#ifdef DEBUG_PROGRESS
    printf("%s:%d startupLispEnvironment\n", __FILE__, __LINE__ );
#endif
    char* trapInterncP = getenv("CLASP_TRAP_INTERN");
    if ( trapInterncP ) {
      this->_TrapIntern = true;
      std::string trapIntern(trapInterncP);
      size_t sep = trapIntern.find(':');
      if (sep == string::npos) {
        printf("You must provide a symbol name of the form PKG:NAME or PKG::NAME\n");
        abort();
      }
      size_t nameStart = sep + 1;
      if (trapIntern[nameStart] == ':') ++nameStart;
      this->_TrapInternPackage = trapIntern.substr(0, sep);
      this->_TrapInternName = trapIntern.substr(nameStart, 9999999);
    }
  }
  this->_Mode = FLAG_EXECUTE;
  ::_lisp = gctools::tagged_pointer<Lisp_O>(this); // this->sharedThis<Lisp_O>();
  //	initializeProfiler(this->profiler(),_lisp);
  this->_TraceLevel = 0;
  this->_DebuggerLevel = 0;
  this->_CoreBuiltInClassesInitialized = false;
  this->_PackagesInitialized = false;
  this->_BuiltInClassesInitialized = false;
  this->_NilsCreated = false;
  this->_EnvironmentInitialized = false;
  this->_EnvironmentId = 0;
  this->_Roots._CommandLineArguments.reset_();
  this->_Bundle = bundle;
  CoreExposer_sp coreExposer;
  BuiltInClass_sp classDummy;
  {
    _BLOCK_TRACE("Initialize core classes");
    initialize_clasp();
    _lisp->_Roots._CorePackage = gc::As<Package_sp>(_lisp->findPackage(CorePkg));
    _lisp->_Roots._KeywordPackage = gc::As<Package_sp>(_lisp->findPackage(KeywordPkg));
    _lisp->_Roots._KeywordPackage->setKeywordPackage(true);
    _lisp->_Roots._CommonLispPackage = gc::As<Package_sp>(_lisp->findPackage(ClPkg));
    initializeAllClSymbols(_lisp->_Roots._CommonLispPackage);
    coreExposer = gc::GC<CoreExposer_O>::allocate(_lisp);
    coreExposer->define_essential_globals(_lisp);
    this->_PackagesInitialized = true;
  }
  {
    _BLOCK_TRACE("Create some housekeeping objects");
    this->_Roots._LoadTimeValueArrays = HashTableEqual_O::create_default();
    this->_Roots._SetfDefinitions = HashTableEq_O::create_default();
    this->_Roots._SingleDispatchGenericFunctionTable = HashTableEq_O::create_default();
  }
  this->_EnvironmentInitialized = true;
  this->_BuiltInClassesInitialized = true;
  //	LOG(BF("ALL CLASSES: %s")% this->dumpClasses() );
  //    this->createNils();
  {
    _BLOCK_TRACE("Dump of all BuiltInClass classes");
#ifdef DEBUG_ON
//    rootClassManager().debugDump();
#endif
  }
  //
  // Finish initializing Lisp object
  //
#ifdef DEBUG_PROGRESS
    printf("%s:%d startupLispEnvironment initialize everything\n", __FILE__, __LINE__ );
#endif
  this->_Roots._CommandLineArguments = _Nil<T_O>();
  {
    _BLOCK_TRACE("Initialize other code"); // needs _TrueObject
    initialize_Lisp_O();
    core::HashTableEql_sp ht = core::HashTableEql_O::create_default();
    core::_sym_STARcxxDocumentationSTAR->defparameter(ht);
    initialize_functions();
    eval::defineSpecialOperatorsAndMacros(this->_Roots._CorePackage);
#ifdef DEBUG_PROGRESS
    printf("%s:%d startupLispEnvironment initialize_classes_and_methods\n", __FILE__, __LINE__ );
#endif
    initialize_classes_and_methods();
#ifdef DEBUG_PROGRESS
    printf("%s:%d startupLispEnvironment initialize_source_info\n", __FILE__, __LINE__ );
#endif
    initialize_source_info();
#ifdef DEBUG_PROGRESS
    printf("%s:%d startupLispEnvironment initialize_cache\n", __FILE__, __LINE__ );
#endif
    initialize_cache();
#ifdef DEBUG_PROGRESS
    printf("%s:%d startupLispEnvironment initialize_backquote\n", __FILE__, __LINE__ );
#endif
    initialize_backquote();
#ifdef DEBUG_PROGRESS
    printf("%s:%d startupLispEnvironment initialize_compiler_primitives\n", __FILE__, __LINE__ );
#endif
    initialize_compiler_primitives(_lisp);
#ifdef DEBUG_PROGRESS
    printf("%s:%d startupLispEnvironment initialize_bits\n", __FILE__, __LINE__ );
#endif
    initialize_bits();
    // Rest may be unnecessary after new boot-strapping approach is developed
#ifdef DEBUG_CL_SYMBOLS
    initializeAllClSymbolsFunctions();
#endif
    coreExposer->expose(_lisp, Exposer_O::candoClasses);
    //	    initializeCandoClos(_lisp);
  }
  {
    // Run some tests to make sure that lisp calling convention is ok.
    run_quick_tests();

    // Setup the pathname translation
    this->_Bundle->setup_pathname_translations();
      
    // -------------------------------------------------------------
    /* Call the function defined in main.cc that creates the source-main: host */
#ifndef PROGRAM_CLBIND
    create_source_main_host();
#endif
  }
  coreExposer->expose(_lisp, Exposer_O::candoFunctions);
  coreExposer->expose(_lisp, Exposer_O::candoGlobals);
  {
    _BLOCK_TRACE("Call global initialization callbacks");
    for (vector<InitializationCallback>::iterator ic = this->_GlobalInitializationCallbacks.begin();
         ic != this->_GlobalInitializationCallbacks.end(); ic++) {
      (*ic)(_lisp);
    }
  }
#if 0
  Path_sp startupWorkingDir = Path_O::create(bundle->getStartupWorkingDir());
  this->defconstant(_sym_STARcurrent_working_directorySTAR, _Nil<Path_O>());
  this->setCurrentWorkingDirectory(startupWorkingDir);
#endif
  this->switchToClassNameHashTable();
  {
    _BLOCK_TRACE("Setup system values");
    FILE *null_out = fopen("/dev/null", "w");
    this->_Roots._NullStream = IOStreamStream_O::makeIO("/dev/null", null_out);
    this->_Roots._RehashSize = DoubleFloat_O::create(2.0);
    this->_Roots._RehashThreshold = DoubleFloat_O::create(0.9);
    this->_Roots._ImaginaryUnit = Complex_O::create(0.0, 1.0);
    this->_Roots._ImaginaryUnitNegative = Complex_O::create(0.0, -1.0);
    this->_Roots._PlusHalf = Ratio_O::create(make_fixnum(1), make_fixnum(2));
    this->_Roots._MinusHalf = Ratio_O::create(make_fixnum(-1), make_fixnum(2));
    this->_Roots._SingleFloatOne = clasp_make_single_float(1.0);
    this->_Roots._DoubleFloatOne = DoubleFloat_O::create(1.0);
    this->_Roots._SingleFloatMinusZero = clasp_make_single_float(-0.0);
    this->_Roots._SingleFloatPlusZero = clasp_make_single_float(0.0);
    this->_Roots._DoubleFloatMinusZero = DoubleFloat_O::create(-0.0);
    this->_Roots._DoubleFloatPlusZero = DoubleFloat_O::create(0.0);
#ifdef CLASP_LONG_FLOAT
    this->_Roots._LongFloatOne = LongFloat_O::create(1.0);
    this->_Roots._LongFloatMinusZero = LongFloat_O::create(-0.0l);
    this->_Roots._LongFloatPlusZero = LongFloat_O::create(0.0l);
#endif // ifdef CLASP_LONG_FLOAT
    this->_Roots._BformatStringOutputStream = clasp_make_string_output_stream();
    this->_Roots._BignumRegister0 = Bignum_O::create(0);
    this->_Roots._BignumRegister1 = Bignum_O::create(0);
    this->_Roots._BignumRegister2 = Bignum_O::create(0);
    Real_sp bits = gc::As<Real_sp>(clasp_make_fixnum(gc::fixnum_bits));
    Real_sp two = gc::As<Real_sp>(clasp_make_fixnum(2));
    this->_Roots._IntegerOverflowAdjust = cl__expt(two, bits); // clasp_make_fixnum(2),clasp_make_fixnum(gc::fixnum_bits));
    getcwd(true);                                             // set *default-pathname-defaults*
  };
  {
    _BLOCK_TRACE("Creating Caches for SingleDispatchGenericFunctions");
    this->_Roots._SingleDispatchMethodCachePtr = gc::GC<Cache_O>::allocate();
    this->_Roots._SingleDispatchMethodCachePtr->setup(2, SingleDispatchMethodCacheSize);
  }
  {
    _BLOCK_TRACE("Creating Caches for CLOS");
    this->_Roots._MethodCachePtr = gctools::GC<Cache_O>::allocate();
    this->_Roots._MethodCachePtr->setup(MaxFunctionArguments, ClosCacheSize);
    this->_Roots._SlotCachePtr = gctools::GC<Cache_O>::allocate();
    this->_Roots._SlotCachePtr->setup(MaxClosSlots, ClosCacheSize);
  }
  {
    _BLOCK_TRACE("Start printing symbols properly");
    this->_PrintSymbolsProperly = true;
  }
}

/*! Get a buffer string from the BufferStringPool */
StrWithFillPtr_sp Lisp_O::get_buffer_string() {
  /* BufferStringPool must be thread local */
  if (!my_thread->_BufferStringPool) {
    // Lazy initialize
    my_thread->_BufferStringPool = _Nil<T_O>();
    StrWithFillPtr_sp one = StrWithFillPtr_O::create(' ', 256, 0, true);
    my_thread->_BufferStringPool = Cons_O::create(one, my_thread->_BufferStringPool);
  } else if (my_thread->_BufferStringPool.nilp()) {
    // If list is empty, link in a buffer
    StrWithFillPtr_sp one = StrWithFillPtr_O::create(' ', 256, 0, true);
    my_thread->_BufferStringPool = Cons_O::create(one, my_thread->_BufferStringPool);
  }
  StrWithFillPtr_sp ret = gc::As<StrWithFillPtr_sp>(oCar(my_thread->_BufferStringPool));
  my_thread->_BufferStringPool = oCdr(my_thread->_BufferStringPool);
  ret->setFillPointer(0);
  return ret;
}

/*! Return a buffer string to the BufferStringPool
*/
void Lisp_O::put_buffer_string(StrWithFillPtr_sp str) {
  my_thread->_BufferStringPool = Cons_O::create(str, my_thread->_BufferStringPool);
}

ReadTable_sp Lisp_O::getCurrentReadTable() {
  return gc::As<ReadTable_sp>(cl::_sym_STARreadtableSTAR->symbolValue());
}

void Lisp_O::setMakePackageAndExportSymbolCallbacks(MakePackageCallback mpc, ExportSymbolCallback esc) {
  _OF();
  LOG(BF("Setting MakePackageCallback and ExportSymbolCallback"));
  this->_MakePackageCallback = mpc;
  this->_ExportSymbolCallback = esc;
}

#if defined(OLD_SERIALIZE)
T_sp Lisp_O::sread(T_sp sin, bool eofErrorP, T_sp eofValue) {
  _OF();
  ReadSerializer_sp reader = _lisp->create<ReadSerializer_O>();
  T_sp obj = reader->read(sin, eofErrorP, eofValue);
  return obj;
}

void Lisp_O::sprint(T_sp obj, T_sp sout) {
  _OF();
  WriteSerializer_sp writer = _lisp->create<WriteSerializer_O>();
  writer->addObject(obj);
  writer->write(sout);
}
#endif // defined(OLD_SERIALIZER)

void Lisp_O::print(boost::format fmt) {
  _OF();
  TRY_BOOST_FORMAT_STRING(fmt, fmt_str);
  if (cl::_sym_print->fboundp()) {
    eval::funcall(cl::_sym_print, Str_O::create(fmt_str));
  } else {
    printf("%s\n", fmt.str().c_str());
  }
}

void Lisp_O::prin1(boost::format fmt) {
  TRY_BOOST_FORMAT_STRING(fmt, fmt_str);
  eval::funcall(cl::_sym_prin1, Str_O::create(fmt_str));
}

List_sp Lisp_O::loadTimeValuesIds() const {
  List_sp names = _Nil<T_O>();
  this->_Roots._LoadTimeValueArrays->mapHash([&names](T_sp key, T_sp val) {
                names = Cons_O::create(key,names);
  });
  return names;
}

/*! How is this going to work with moving garbage collection?
     We return a reference to the LoadTimeValues_sp smart_ptr in the LoadtimeValueArrays hash-table
    What happens when this moves????    Disaster!!!!!!!   */
LoadTimeValues_sp Lisp_O::getOrCreateLoadTimeValues(const string &name, int numberOfLoadTimeValues, int numberOfLoadTimeSymbols) {
  Str_sp key = Str_O::create(name);
  T_sp it = this->_Roots._LoadTimeValueArrays->gethash(key, _Nil<T_O>());
  if (it.nilp()) {
    LoadTimeValues_sp vo = LoadTimeValues_O::make(numberOfLoadTimeValues, numberOfLoadTimeSymbols);
    this->_Roots._LoadTimeValueArrays->setf_gethash(key, vo);
    return vo; // gctools::smart_ptr<LoadTimeValues_O>(reinterpret_cast<LoadTimeValues_O*>(vo.pbase()));
  }
  LoadTimeValues_sp ltv = gc::As<LoadTimeValues_sp>(it);
  return ltv; // return gctools::smart_ptr<LoadTimeValues_O>(reinterpret_cast<LoadTimeValues_O*>(ltv.pbase()));
}

LoadTimeValues_sp Lisp_O::findLoadTimeValues(const string &name) {
  Str_sp key = Str_O::create(name);
  T_sp it = this->_Roots._LoadTimeValueArrays->gethash(key, _Nil<T_O>());
  if (it.nilp())
    return _Nil<LoadTimeValues_O>();
  return gc::As<LoadTimeValues_sp>(it);
}
LoadTimeValues_sp Lisp_O::findLoadTimeValuesWithNameContaining(const string &name, int &count) {
  LoadTimeValues_sp result = _Nil<LoadTimeValues_O>();
  count = 0;
  this->_Roots._LoadTimeValueArrays->mapHash([&count, &result, &name](T_sp key, T_sp val) -> void {
                if ( gc::As<Str_sp>(key)->find(name,0).notnilp() ) {
                    result = gc::As<LoadTimeValues_sp>(val);
		    ++count;
                }
  });
  return result;
}

void Lisp_O::defvar(Symbol_sp sym, T_sp obj) {
  _OF();
  sym->makeSpecial();
  sym->setf_symbolValue(obj);
}

void Lisp_O::defconstant(Symbol_sp sym, T_sp obj) {
  _OF();
  sym->makeSpecial();
  sym->setf_symbolValue(obj);
  sym->setReadOnly(true);
}

T_sp Lisp_O::error(const boost::format &fmt) {
  _OF();
  return CandoException_O::create(fmt);
}

Symbol_sp Lisp_O::errorUndefinedSymbol(const char *sym) {
  _OF();
  stringstream ss;
  ss << "Unknown symbol(" << sym << ")";
  SIMPLE_ERROR(BF("%s") % ss.str());
}

Symbol_sp Lisp_O::defineSpecialOperator(const string &packageName, const string &rawFormName, SpecialFormCallback cb, const string &argstring, const string &docstring) {
  _OF();
  string formName = lispify_symbol_name(rawFormName);
  Symbol_sp sym = _lisp->internWithPackageName(packageName, formName);
  sym->exportYourself();
  sym->setf_symbolFunction(_lisp->_true());
  SpecialForm_sp special = SpecialForm_O::create(sym, cb);
  if (this->_Roots._SpecialForms.unboundp()) {
    this->_Roots._SpecialForms = HashTableEq_O::create_default();
  }
  ASSERTP(!this->_Roots._SpecialForms->contains(sym), "You cant define a special form with the symbol(" + formName + ") it has already been defined");
  this->_Roots._SpecialForms->setf_gethash(sym, special);
  return sym;
}

T_sp Lisp_O::specialFormOrNil(Symbol_sp sym) {
  if (sym.nilp())
    return _Nil<T_O>();
  return this->_Roots._SpecialForms->gethash(sym);
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("listOfAllSpecialOperators");
CL_DEFUN T_sp core__list_of_all_special_operators() {
  List_sp sos(_Nil<T_O>());
  _lisp->_Roots._SpecialForms->maphash([&sos](T_sp key, T_sp val) {
      sos = Cons_O::create(key,sos);
  });
  return sos;
}

void Lisp_O::installPackage(const Exposer_O *pkg) {
  _OF();
  LOG(BF("Installing package[%s]") % pkg->packageName());
  int firstNewGlobalCallback = this->_GlobalInitializationCallbacks.end() - this->_GlobalInitializationCallbacks.begin();
  ChangePackage change(pkg->package());
  //    this->inPackage(pkg->packageName());
  {
    _BLOCK_TRACE("Initializing classes");
    pkg->expose(_lisp, Exposer_O::candoClasses);
  }
  {
    _BLOCK_TRACE("Creating nils for built-in classes");
    LOG(BF("Nils aren't created here anymore - they are created when the class is registered"));
    //	this->createNils();
  }
  {
    _BLOCK_TRACE("Initializing functions");
    pkg->expose(_lisp, Exposer_O::candoFunctions);
  }
  {
    _BLOCK_TRACE("Initializing globals");
    pkg->expose(_lisp, Exposer_O::candoGlobals);
  }

  {
    _BLOCK_TRACE("Call global initialization callbacks");
    for (vector<InitializationCallback>::iterator ic = this->_GlobalInitializationCallbacks.begin() + firstNewGlobalCallback;
         ic != this->_GlobalInitializationCallbacks.end(); ic++) {
      (*ic)(_lisp);
    }
  }
}

void Lisp_O::installGlobalInitializationCallback(InitializationCallback c) {
  this->_GlobalInitializationCallbacks.push_back(c);
}

#if defined(XML_ARCHIVE)
void Lisp_O::archive(::core::ArchiveP node) {
  _OF();
  SIMPLE_ERROR(BF("Never archive Lisp objects"));
}
#endif // defined(XML_ARCHIVE)

void Lisp_O::addClassNameToPackageAsDynamic(const string &package, const string &name, Class_sp mc) {
  Symbol_sp classSymbol = _lisp->intern(name, gc::As<Package_sp>(_lisp->findPackage(package, true)));
  classSymbol->exportYourself();
  classSymbol->setf_symbolValue(mc);
  //    this->globalEnvironment()->extend(classSymbol,mc);
  //    mc->__setLambdaListHandlerString(mc->getInstanceBaseClass()->__getLambdaListHandlerString());
}

/*! Add the class with (className) to the current package
 */
void Lisp_O::addClass(Symbol_sp classSymbol,
                      Creator_sp alloc,
                      Symbol_sp base1ClassSymbol )
//  ,
//                      Symbol_sp base2ClassSymbol,
//                      Symbol_sp base3ClassSymbol) {
{
  LOG(BF("Lisp_O::addClass classSymbol(%s) baseClassSymbol1(%u) baseClassSymbol2(%u)") % _rep_(classSymbol) % base1ClassSymbol % base2ClassSymbol);
  ASSERTP(IS_SYMBOL_DEFINED(BuiltInClass_O::static_classSymbol()),
          "You cannot create a BuiltInClass before the BuiltIn!Class is defined");
  Class_sp cc;
  if (classSymbol == StandardObject_O::static_classSymbol()) {
    IMPLEMENT_ME(); // WHEN DO StandardClasses get created with addClass?????
  } else {
    LOG(BF("Adding BuiltInClass with classSymbol(%d)") % classSymbol);
    cc = BuiltInClass_O::create(classSymbol);
  }
  printf("%s:%d --> Adding class[%s]\n", __FILE__, __LINE__, _rep_(classSymbol).c_str());
  core__setf_find_class(cc, classSymbol, true, _Nil<T_O>());
  if (IS_SYMBOL_DEFINED(base1ClassSymbol)) {
    cc->addInstanceBaseClass(base1ClassSymbol);
  } else {
    SIMPLE_ERROR(BF("There must be one base class"));
  }
#if 0
  if (IS_SYMBOL_DEFINED(base2ClassSymbol)) {
    cc->addInstanceBaseClass(base2ClassSymbol);
  }
  if (IS_SYMBOL_DEFINED(base3ClassSymbol)) {
    cc->addInstanceBaseClass(base3ClassSymbol);
  }
#endif
  ASSERTF((bool)alloc, BF("_creator for %s is NULL!!!") % _rep_(classSymbol));
  cc->setCreator(alloc);
}
/*! Add the class with (className) to the current package
 */
void Lisp_O::addClass(Symbol_sp classSymbol, Class_sp theClass, Creator_sp allocator) {
  //	printf("%s:%d:%s  Adding class with symbol %s -- _allocator=%p unless we initialize it properly\n", __FILE__,__LINE__,__FUNCTION__,_rep_(classSymbol).c_str(), allocator );
  LOG(BF("Lisp_O::addClass classSymbol(%s)") % _rep_(classSymbol));
  //	printf("%s:%d --> Adding class[%s]\n", __FILE__, __LINE__, _rep_(classSymbol).c_str() );
  core__setf_find_class(theClass, classSymbol, true, _Nil<T_O>());
  //        IMPLEMENT_MEF(BF("Pass an AllocateInstanceFunctor"));
  theClass->setCreator(allocator);
}

StandardClass_sp Lisp_O::defineStandardClass(Symbol_sp name, T_sp baseClassesDesignator, List_sp slotSpecifiers) {
  _OF();
  IMPLEMENT_MEF(BF("Implement defineStandardClass"));
}

void Lisp_O::exportToPython(Symbol_sp sym) const {
  _OF();
  if (this->_ExportSymbolCallback == NULL) {
    LOG(BF("Could not export symbol[%s] because _ExportSymbolCallback is NULL") % _rep_(sym));
  } else {
    LOG(BF("Exporting symbol[%s]") % _rep_(sym));
    this->_ExportSymbolCallback(sym, _lisp);
  }
}

void Lisp_O::mapNameToPackage(const string &name, Package_sp pkg) {
  int packageIndex;
  for (packageIndex = 0; packageIndex < this->_Roots._Packages.size(); ++packageIndex) {
    if (this->_Roots._Packages[packageIndex] == pkg)
      goto FOUND;
  }
  SIMPLE_ERROR(BF("Could not find package with (nick)name: %s") % pkg->getName());
FOUND:
  this->_PackageNameIndexMap[name] = packageIndex;
}

void Lisp_O::unmapNameToPackage(const string &name) {
  map<string, int>::iterator it;
  it = this->_PackageNameIndexMap.find(name);
  if (it == this->_PackageNameIndexMap.end()) {
    SIMPLE_ERROR(BF("Could not find package with (nick)name: %s") % name);
  }
  this->_PackageNameIndexMap.erase(it);
}

Package_sp Lisp_O::makePackage(const string &name, list<string> const &nicknames, list<string> const &usePackages) {
  map<string, int>::iterator it = this->_PackageNameIndexMap.find(name);
  if (it != this->_PackageNameIndexMap.end()) {
    SIMPLE_ERROR(BF("There already exists a package with name: %s") % name);
  }
  LOG(BF("Creating package with name[%s]") % name);
  Package_sp newPackage = Package_O::create(name);
  int packageIndex = this->_Roots._Packages.size();
  {
    //            printf("%s:%d Lisp_O::makePackage name: %s   index: %d   newPackage@%p\n", __FILE__, __LINE__, name.c_str(), packageIndex, newPackage.raw_());

    this->_PackageNameIndexMap[name] = packageIndex;
    this->_Roots._Packages.push_back(newPackage);
  }
  List_sp cnicknames(_Nil<T_O>());
  for (list<string>::const_iterator it = nicknames.begin(); it != nicknames.end(); it++) {
    string nickName = *it;
    if (this->_PackageNameIndexMap.count(nickName) > 0 && nickName != name) {
      int existingIndex = this->_PackageNameIndexMap[nickName];
      SIMPLE_ERROR(BF("Package nickname[%s] is already being used by package[%s]") % nickName % this->_Roots._Packages[existingIndex]->getName());
    }
    this->_PackageNameIndexMap[nickName] = packageIndex;
    cnicknames = Cons_O::create(Str_O::create(nickName), cnicknames);
  }
  newPackage->setNicknames(cnicknames);
  for (list<string>::const_iterator jit = usePackages.begin(); jit != usePackages.end(); jit++) {
    Package_sp usePkg = gc::As<Package_sp>(this->findPackage(*jit, true));
    LOG(BF("Using package[%s]") % usePkg->getName());
    newPackage->usePackage(usePkg);
  }
  if (this->_MakePackageCallback != NULL) {
    LOG(BF("Calling _MakePackageCallback with package[%s]") % name);
    this->_MakePackageCallback(name, _lisp);
  } else {
    LOG(BF("_MakePackageCallback is NULL - not calling callback"));
  }
  return newPackage;
}

T_sp Lisp_O::findPackage(const string &name, bool errorp) const {
  //        printf("%s:%d Lisp_O::findPackage name: %s\n", __FILE__, __LINE__, name.c_str());
  map<string, int>::const_iterator fi = this->_PackageNameIndexMap.find(name);
  if (fi == this->_PackageNameIndexMap.end()) {
    if (errorp) {
      PACKAGE_ERROR(Str_O::create(name));
    }
    return _Nil<Package_O>(); // return nil if no package found
  }
  //        printf("%s:%d Lisp_O::findPackage index: %d\n", __FILE__, __LINE__, fi->second );
  Package_sp getPackage = this->_Roots._Packages[fi->second];
  //        printf("%s:%d Lisp_O::findPackage pkg@%p\n", __FILE__, __LINE__, getPackage.raw_());
  return getPackage;
}

T_sp Lisp_O::sourceDatabase() const {
  _OF();
  // At startup the *package* symbol may not yet
  // be defined or bound to a package - in that case just say we are in the core package
  //
  T_sp cur;
  if (IS_SYMBOL_UNDEFINED(_sym_STARsourceDatabaseSTAR)) {
    return _Nil<T_O>();
  }
  if (!_sym_STARsourceDatabaseSTAR->specialP()) {
    return _Nil<T_O>();
  }
  cur = _sym_STARsourceDatabaseSTAR->symbolValue();
  if (cur.nilp())
    return cur;
  return gc::As<SourceManager_sp>(cur);
}

Package_sp Lisp_O::getCurrentPackage() const {
  _OF();
  // At startup the *package* symbol may not yet
  // be defined or bound to a package - in that case just say we are in the core package
  //
  Package_sp cur;
  if (IS_SYMBOL_UNDEFINED(cl::_sym_STARpackageSTAR)) {
    cur = this->_Roots._CorePackage;
    goto DONE;
  }
  if (!cl::_sym_STARpackageSTAR->specialP()) {
    cur = this->_Roots._CorePackage;
    goto DONE;
  }
  cur = gc::As<Package_sp>(cl::_sym_STARpackageSTAR->symbolValue());
DONE:
  ASSERTNOTNULL(cur);
  return cur;
}

void Lisp_O::selectPackage(Package_sp pack) {
  _OF();
  cl::_sym_STARpackageSTAR->setf_symbolValue(pack);
}

bool Lisp_O::recognizesPackage(const string &packageName) const {
  map<string, int>::const_iterator pi = this->_PackageNameIndexMap.find(packageName);
  return (pi != this->_PackageNameIndexMap.end());
}

void Lisp_O::inPackage(const string &p) {
  _OF();
  map<string, int>::const_iterator pi = this->_PackageNameIndexMap.find(p);
  if (pi == this->_PackageNameIndexMap.end()) {
    ASSERTP(this->recognizesPackage(p), "I do not recognize package: " + p);
  }
  this->selectPackage(this->_Roots._Packages[pi->second]);
}

void Lisp_O::throwIfBuiltInClassesNotInitialized() {
  if (this->_BuiltInClassesInitialized)
    return;
  SIMPLE_ERROR(BF("Cpp-classes are not initialized"));
}

Path_sp Lisp_O::translateLogicalPathname(T_sp obj) {
  if (Str_sp logicalPathName = obj.asOrNull<Str_O>()) {
    string fileName = logicalPathName->get();
    return Path_O::create(fileName);
    SIMPLE_ERROR(BF("include " + fileName + " error, file does not exist"));
  } else {
    SIMPLE_ERROR(BF("Finish implementing Lisp_O::translateLogicalPathname"));
  }
}

Path_sp Lisp_O::translateLogicalPathnameUsingPaths(T_sp obj) {
  if (Str_sp logicalPathName = obj.asOrNull<Str_O>()) {
    string fileName = logicalPathName->get();
    LOG(BF("Looking for file: %s") % fileName.c_str());
    LOG(BF("Looking in current directory"));
    boost_filesystem::path onePath("./");
    onePath /= fileName;
    if (boost_filesystem::exists(onePath)) {
      return Path_O::create(onePath.string());
    }
    Symbol_sp pathSym = _sym_STARPATHSTAR;
    List_sp pathList = pathSym->symbolValue();
    LOG(BF("PATH variable = %s") % _rep_(pathList).c_str());
    while (pathList.notnilp()) {
      boost_filesystem::path onePath(gc::As<Str_sp>(oCar(pathList))->get());
      onePath /= fileName;
      LOG(BF("Checking path[%s]") % onePath.string());
      if (boost_filesystem::exists(onePath)) {
        return Path_O::create(onePath.string());
      }
      pathList = oCdr(pathList);
    }
    SIMPLE_ERROR(BF("include " + fileName + " error, file does not exist"));
  } else {
    SIMPLE_ERROR(BF("Finish implementing Lisp_O::translateLogicalPathname"));
  }
}

uint Lisp_O::nextEnvironmentId() {
  this->_EnvironmentId++;
  return this->_EnvironmentId;
}

#define DLINE() printf("%s:%d debug\n", __FILE__, __LINE__);

void Lisp_O::parseCommandLineArguments(int argc, char *argv[], bool compileInputFile) {
  int endArg = argc;
  for (int i = 0; i < argc; ++i) {
    if (strcmp(argv[i], "--") == 0) {
      endArg = i;
    }
  }

  //
  // Pass whatever is left over to the Lisp environment
  //
  LOG(BF("Parsing what is left over into lisp environment arguments"));
  gctools::Vec0<T_sp> vargs;
  for (int j(endArg + 1); j < argc; ++j) {
    vargs.push_back(Str_O::create(argv[j]));
  }
  VectorObjects_sp args = VectorObjects_O::create(vargs);
  LOG(BF(" Command line arguments are being set in Lisp to: %s") % _rep_(args));
  SYMBOL_EXPORT_SC_(CorePkg, STARcommandLineArgumentsSTAR);
  _sym_STARcommandLineArgumentsSTAR->defparameter(args);

  CommandLineOptions options(endArg, argv);

  if (options._PauseForDebugger) {
    printf("The PID is  %d  - press enter to continue\n", getpid());
    string temp;
    std::cin >> temp;
  }

  List_sp features = cl::_sym_STARfeaturesSTAR->symbolValue();
  for (int i = 0; i < options._Features.size(); ++i) {
    features = Cons_O::create(_lisp->internKeyword(lispify_symbol_name(options._Features[i])), features);
  }
  features = Cons_O::create(_lisp->internKeyword("SILENCE-CCLASP-COMPILE-WARNINGS"), features);
  features = Cons_O::create(_lisp->internKeyword("CLASP"), features);
#ifdef _TARGET_OS_DARWIN
  features = Cons_O::create(_lisp->internKeyword("DARWIN"), features);
  features = Cons_O::create(_lisp->internKeyword("BSD"), features);
  features = Cons_O::create(_lisp->internKeyword("OS-UNIX"), features);
  features = Cons_O::create(_lisp->internKeyword("UNIX"), features);
  features = Cons_O::create(_lisp->internKeyword("X86-64"), features);
#endif
#ifdef _TARGET_OS_LINUX
  features = Cons_O::create(_lisp->internKeyword("UNIX"), features);
  features = Cons_O::create(_lisp->internKeyword("OS-UNIX"), features);
  features = Cons_O::create(_lisp->internKeyword("LINUX"), features);
  features = Cons_O::create(_lisp->internKeyword("X86-64"), features);
#endif
#if (LLVM_VERSION_X100>=380)
  features = Cons_O::create(_lisp->internKeyword("LLVM38"), features);
#endif
#if (LLVM_VERSION_X100>=390)
  features = Cons_O::create(_lisp->internKeyword("LLVM39"), features);
#endif
#ifdef VARARGS
  features = Cons_O::create(_lisp->internKeyword("VARARGS"), features);
#endif
#ifdef POLYMORPHIC_SMART_PTR
  features = Cons_O::create(_lisp->internKeyword("POLYMORPHIC-SMART-PTR"), features);
#endif
#ifdef _DEBUG_BUILD
  features = Cons_O::create(_lisp->internKeyword("DEBUG-BUILD"), features);
#else // _RELEASE_BUILD
  features = Cons_O::create(_lisp->internKeyword("RELEASE-BUILD"), features);
#endif
#ifdef USE_REFCOUNT
  features = Cons_O::create(_lisp->internKeyword("USE-REFCOUNT"), features);
#endif
#ifdef USE_BOEHM
#ifdef USE_CXX_DYNAMIC_CAST
  features = Cons_O::create(_lisp->internKeyword("USE-BOEHMDC"), features);
#else
  features = Cons_O::create(_lisp->internKeyword("USE-BOEHM"), features);
#endif
#endif
#ifdef USE_MPS
  // Informs CL that MPS is being used
  features = Cons_O::create(_lisp->internKeyword("USE-MPS"), features);
#ifdef USE_AMC_POOL
  // Informs that the Automatic-Mostly-Copying Pool is being used
  features = Cons_O::create(_lisp->internKeyword("USE-AMC-POOL"), features);
#endif
#endif
#ifdef USE_EXPENSIVE_BACKTRACE
  features = Cons_O::create(_lisp->internKeyword("USE-EXPENSIVE-BACKTRACE"), features);
#endif

  cl::_sym_STARfeaturesSTAR->setf_symbolValue(features);

  SYMBOL_EXPORT_SC_(CorePkg, STARprintVersionOnStartupSTAR);
  _sym_STARprintVersionOnStartupSTAR->defparameter(_lisp->_boolean(options._Version));
  SYMBOL_EXPORT_SC_(CorePkg, STARsilentStartupSTAR);
  _sym_STARsilentStartupSTAR->defparameter(_lisp->_boolean(options._SilentStartup));
  if (!options._SilentStartup) {
    stringstream sdebug;
    bool debugging = gctools::debugging_configuration(sdebug);
    if ( debugging ) {
      printf("%s:%d Debugging flags are set - configuration:\n%s\n", __FILE__, __LINE__, sdebug.str().c_str());
    }
  }

  //	this->_FunctionName = execName;
  this->_RCFileName = "sys:" KERNEL_NAME ";init.lsp";

  this->_IgnoreInitImage = options._DontLoadImage;
  this->_IgnoreInitLsp = options._DontLoadInitLsp;

  SYMBOL_EXPORT_SC_(CorePkg, STARcommandLineLoadEvalSequenceSTAR);
  List_sp loadEvals = _Nil<T_O>();
  for (auto it : options._LoadEvalList) {
    Cons_sp one;
    if (it.first == cloEval) {
      one = Cons_O::create(kw::_sym_eval, Str_O::create(it.second));
    } else {
      one = Cons_O::create(kw::_sym_load, Str_O::create(it.second));
    }
    loadEvals = Cons_O::create(one, loadEvals);
  }
  _sym_STARcommandLineLoadEvalSequenceSTAR->defparameter(cl__nreverse(loadEvals));

  this->_Interactive = options._Interactive;
  if (this->_Interactive) {
    List_sp features = cl::_sym_STARfeaturesSTAR->symbolValue();
    features = Cons_O::create(KW("interactive"), features);
    cl::_sym_STARfeaturesSTAR->setf_symbolValue(features);
  }
  if (options._NoRc) {
    List_sp features = cl::_sym_STARfeaturesSTAR->symbolValue();
    features = Cons_O::create(KW("no-rc"), features);
    cl::_sym_STARfeaturesSTAR->setf_symbolValue(features);
  }

  if (options._GotRandomNumberSeed) {
    seedRandomNumberGenerators(options._RandomNumberSeed);
  } else {
    seedRandomNumberGenerators(this->mpiRank());
  }
  if (options._HasImageFile) {
    SYMBOL_EXPORT_SC_(CorePkg, STARcommandLineImageSTAR);
    _sym_STARcommandLineImageSTAR->defparameter(cl__pathname(Str_O::create(options._ImageFile)));
  } else {
    _sym_STARcommandLineImageSTAR->defparameter(core__startup_image_pathname());
  }
  {
    this->_TrapIntern = false;
    if (options._TrapIntern != "") {
      this->_TrapIntern = true;
      size_t sep = options._TrapIntern.find(':');
      if (sep == string::npos) {
        printf("You must provide a symbol name of the form PKG:NAME or PKG::NAME\n");
        abort();
      }
      size_t nameStart = sep + 1;
      if (options._TrapIntern[nameStart] == ':')
        ++nameStart;
      this->_TrapInternPackage = options._TrapIntern.substr(0, sep);
      this->_TrapInternName = options._TrapIntern.substr(nameStart, 9999999);
      printf("%s:%d Trapping INTERN of symbol %s in package %s\n", __FILE__, __LINE__, this->_TrapInternName.c_str(), this->_TrapInternPackage.c_str() );
    }
  }
  LOG(BF("lisp->_ScriptInFile(%d)  lisp->_FileNameOrCode(%s)") % this->_ScriptInFile % this->_FileNameOrCode);
}

T_mv Lisp_O::readEvalPrint(T_sp stream, T_sp environ, bool printResults, bool prompt) {
  T_mv result = Values(_Nil<T_O>());
  DynamicScopeManager scope(_sym_STARcurrentSourceFileInfoSTAR, core__source_file_info(stream));
  while (1) {
    TRY() {
      if (prompt) {
        stringstream prompts;
        prompts << std::endl
                << gc::As<Package_sp>(cl::_sym_STARpackageSTAR->symbolValue())->getName() << "> ";
        clasp_write_string(prompts.str(), stream);
      }
#ifdef USE_SOURCE_DATABASE
      DynamicScopeManager innerScope(_sym_STARsourceDatabaseSTAR, SourceManager_O::create());
#else
      DynamicScopeManager innerScope(_sym_STARsourceDatabaseSTAR, _Nil<T_O>());
#endif
      innerScope.pushSpecialVariableAndSet(_sym_STARcurrentSourcePosInfoSTAR, core__input_stream_source_pos_info(stream));
      T_sp expression = cl__read(stream, _Nil<T_O>(), _Unbound<T_O>(), _Nil<T_O>());
      if (expression.unboundp())
        break;
      _sym_STARcurrentSourcePosInfoSTAR->setf_symbolValue(core__walk_to_find_source_pos_info(expression, _sym_STARcurrentSourcePosInfoSTAR->symbolValue()));
      if (_sym_STARechoReplReadSTAR->symbolValue().isTrue()) {
        string suppress;
        if (cl::_sym_STARread_suppressSTAR->symbolValue().isTrue()) {
          suppress = "SUPPRESSED";
          if (expression.notnilp()) {
            SIMPLE_ERROR(BF("*read-suppress* is true but the following expression was read: %s") % _rep_(expression));
          }
        }
        this->print(BF(";;--read-%s-------------\n#|\n%s\n|#----------\n") % suppress.c_str() % _rep_(expression));
      }
      _BLOCK_TRACEF(BF("---REPL read[%s]") % expression->__repr__());
      if (cl__keywordp(expression)) {
        ql::list tplCmd;
        tplCmd << expression;
        while (T_sp exp = cl__read(stream, _Nil<T_O>(), _Unbound<T_O>(), _Nil<T_O>())) {
          if (exp.unboundp())
            break;
          tplCmd << exp;
        }
        if (_sym_STARtopLevelCommandHookSTAR->symbolValue().notnilp()) {
          eval::funcall(_sym_STARtopLevelCommandHookSTAR->symbolValue(), tplCmd.cons());
        } else {
          core__bformat(_lisp->_true(), "Cannot interpret %s - define core::*top-level-command-hook*", Cons_O::createList(tplCmd.cons()));
        }
      } else if (expression.notnilp()) {
        result = eval::funcall(core::_sym_STAReval_with_env_hookSTAR->symbolValue(), expression, environ);
        gctools::Vec0<core::T_sp /*,gctools::RootedGCHolder*/> vresults;
        vresults.resize(result.number_of_values());
        if (result.number_of_values() > 0) {
          vresults[0] = result;
          if (result.number_of_values() > 1) {
            for (int i(1); i < result.number_of_values(); ++i) {
              vresults[i] = result.valueGet_(i);
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
    }
    catch (Condition &err) {
      // Catch condition from reader means just ask for another s-exp if
      // interactive and terminate if batch
      this->print(BF("%s:%d Caught Condition from reader\n") % __FILE__ % __LINE__);
      abort();
      //		this->reportConditionAndTerminateProgramIfBatch(err.conditionObject());
    }
    catch (DebuggerSaysAbortToRepl &abort) {
      this->print(BF("%s:%d aborted to repl\n") % __FILE__ % __LINE__);
      // Do nothing
    }
    catch (HardError &err) {
      this->print(BF("Should never happen - catch and convert to Condition below - HardError: %s") % err.message());
      IMPLEMENT_ME();
      //		this->enterDebugger();
    }
  }
  return result;
}

T_mv Lisp_O::readEvalPrintString(const string &code, T_sp environ, bool printResults) {
  _OF();
  StringInputStream_sp sin = StringInputStream_O::make(code);
  T_mv result = this->readEvalPrint(sin, environ, printResults, false);
  cl__close(sin);
  return result;
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("lowLevelRepl - this is a built in repl for when the top-level repl isn't available");
CL_DEFUN void core__low_level_repl() {
  List_sp features = cl::_sym_STARfeaturesSTAR->symbolValue();
  if ( features.notnilp() ) {
    List_sp interactive = gc::As<Cons_sp>(features)->memberEq(kw::_sym_interactive);
    if ( interactive.notnilp() ) {
      _lisp->readEvalPrint(cl::_sym_STARterminal_ioSTAR->symbolValue(), _Nil<T_O>(), true, true);
    }
  }
};

void Lisp_O::readEvalPrintInteractive() {
  Cons_sp expression;
  //	TopLevelIHF topFrame(my_thread->invocationHistoryStack(),_Nil<T_O>());
  this->readEvalPrint(cl::_sym_STARterminal_ioSTAR->symbolValue(), _Nil<T_O>(), true, true);
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("stackUsed");
CL_DEFUN size_t core__stack_used() {
  int x;
  char *xaddr = (char *)(&x);
  if ( xaddr > _lisp->_StackTop ) {
    printf("%s:%d There is a problem with the stack _lisp->_StackTop@%p is below the current stack pointer@%p\n", __FILE__, __LINE__, _lisp->_StackTop, xaddr );
    abort();
  }
  size_t stack = (size_t)(_lisp->_StackTop - xaddr);
  return stack;
};

static bool global_invokedInternalDebugger = false;

struct ExceptionSafeResetInvokedInternalDebugger {
  ExceptionSafeResetInvokedInternalDebugger() {
    global_invokedInternalDebugger = true;
  };
  virtual ~ExceptionSafeResetInvokedInternalDebugger() {
    global_invokedInternalDebugger = false;
  }
};

#define ARGS_af_stackSizeWarning "(arg)"
#define DECL_af_stackSizeWarning ""
#define DOCS_af_stackSizeWarning "stackSizeWarning"
void af_stackSizeWarning(size_t stackUsed) {
  if (!global_invokedInternalDebugger) {
    int x;
    char *xaddr = (char *)(&x);
    printf("%s:%d Stack is getting full currently at %zu bytes - warning at %u bytes  top@%p current@%p\n",
           __FILE__, __LINE__,
           stackUsed, _lisp->_StackWarnSize,
           _lisp->_StackTop, xaddr );
    ExceptionSafeResetInvokedInternalDebugger safe;
    core__invoke_internal_debugger(_Nil<core::T_O>());
  }
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("monitor stack for problems - warn if getting too large");
CL_DEFUN void core__stack_monitor() {
  uint stackUsed = core__stack_used();
  if (stackUsed > _lisp->_StackSampleMax)
    _lisp->_StackSampleMax = stackUsed;
  if (_lisp->_StackSampleSize > 0) {
    _lisp->_StackSampleCount++;
    if (_lisp->_StackSampleCount >= _lisp->_StackSampleSize) {
      printf("STACK-USED samples: %u high-water: %u     %s:%d\n",
             _lisp->_StackSampleSize,
             _lisp->_StackSampleMax,
             __FILE__, __LINE__);
      _lisp->_StackSampleCount = 0;
      _lisp->_StackSampleMax = 0;
    }
  }
  if (stackUsed > _lisp->_StackWarnSize) {
    af_stackSizeWarning(stackUsed);
  }
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("Return the soft and hard limits of the stack");
CL_DEFUN T_mv core__stack_limit() {
  return Values(clasp_make_fixnum(_lisp->_StackWarnSize));
};

CL_LAMBDA(&key warn-size sample-size);
CL_DECLARE();
CL_DOCSTRING("setupStackMonitor");
CL_DEFUN void core__setup_stack_monitor(T_sp warnSize, T_sp sampleSize) {
  if (!warnSize.nilp()) {
    _lisp->_StackWarnSize = unbox_fixnum(gc::As<Fixnum_sp>(warnSize));
  }
  if (!sampleSize.nilp()) {
    _lisp->_StackSampleSize = unbox_fixnum(gc::As<Fixnum_sp>(sampleSize));
    _lisp->_StackSampleCount = 0;
    _lisp->_StackSampleMax = 0;
  }
};

CL_LAMBDA(&optional (exit-value 0));
CL_DECLARE();
CL_DOCSTRING("exit");
CL_DEFUN void core__exit(int exitValue) {
  global_debuggerOnSIGABRT = false;
  throw(ExitProgram(exitValue));
};

CL_LAMBDA(&optional (exit-value 0));
CL_DECLARE();
CL_DOCSTRING("quit");
CL_DEFUN void core__quit(int exitValue) {
  core__exit(exitValue);
};

CL_LAMBDA(key datum alist);
CL_DECLARE();
CL_DOCSTRING("acons");
CL_DEFUN List_sp cl__acons(T_sp key, T_sp val, List_sp alist) {
  Cons_sp acons = Cons_O::create(key, val);
  return Cons_O::create(acons, alist);
}

CL_LAMBDA(item alist &key key test test-not);
CL_DECLARE();
CL_DOCSTRING("assoc");
CL_DEFUN List_sp cl__assoc(T_sp item, List_sp alist, T_sp key, T_sp test, T_sp test_not) {
  if (alist.nilp())
    return alist;
  return alist.asCons()->assoc(item, key, test, test_not);
}

CL_LAMBDA(item list &key key test test-not);
CL_DECLARE();
CL_DOCSTRING("See CLHS member");
CL_DEFUN List_sp cl__member(T_sp item, T_sp tlist, T_sp key, T_sp test, T_sp test_not) {
  if (tlist.nilp())
    return _Nil<T_O>();
  if (Cons_sp list = tlist.asOrNull<Cons_O>()) {
    return (list->member(item, key, test, test_not));
  }
  QERROR_WRONG_TYPE_NTH_ARG(2, tlist, cl::_sym_list);
  UNREACHABLE();
}

CL_LAMBDA(item list &key key test test-not);
CL_DECLARE();
CL_DOCSTRING("See CLHS memberTest");
CL_DEFUN List_sp core__member_test(T_sp item, List_sp list, T_sp key, T_sp test, T_sp test_not) {
  if (list.nilp())
    return list;
  return (list.asCons()->member(item, key, test, test_not));
}

CL_LAMBDA(item list test test-not key);
CL_DECLARE();
CL_DOCSTRING("Like member but if a key function is provided then apply it to the item. See ecl::list.d::member1");
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
CL_DOCSTRING("getline");
CL_DEFUN T_mv core__getline(Str_sp prompt) {
  string res;
  string sprompt(prompt->get());
  bool end_of_transmission;
  res = myReadLine(sprompt.c_str(), end_of_transmission);
  Str_sp result = Str_O::create(res);
  return (Values(result));
}

/*
  __BEGIN_DOC( candoScript.general.system, subsection,system)
  \scriptCmdRet{system}{}{String::command}

  Invoke a system call using the UNIX system function call.
  __END_DOC
*/

/*
  __BEGIN_DOC( candoScript.general.render, subsection, render)
  \scriptCmdRet{render}{ object}{renderedObject}

  Render an object into a graphical representation of the \scriptArg{object} that can be viewed using "candoView". Save the result into a file using the "save" command and view it later.
  __END_DOC
*/


CL_LAMBDA(symbol &optional (errorp t) environment);
CL_DECLARE();
CL_DOCSTRING("findClass");
CL_DEFUN Class_mv cl__find_class(Symbol_sp symbol, bool errorp, T_sp env) {
  if (_lisp->bootClassTableIsValid()) {
    return Values(_lisp->boot_findClass(symbol, errorp));
  }
  ASSERTF(env.nilp(), BF("Handle non nil environment"));
  // Use the same global variable that ECL uses
  SYMBOL_EXPORT_SC_(CorePkg, STARclassNameHashTableSTAR);
  HashTable_sp classNames = gc::As<HashTable_sp>(_sym_STARclassNameHashTableSTAR->symbolValue());
  T_mv mc = classNames->gethash(symbol, _Nil<T_O>());
  T_sp cla = mc;
  bool foundp = mc.valueGet_(1).notnilp();
  if (!foundp) {
    if (errorp) {
      SIMPLE_ERROR(BF("Could not find class %s") % _rep_(symbol));
    }
    return (Values(_Nil<Class_O>()));
  }
  Class_sp omc = gc::As<Class_sp>(cla);
#if DEBUG_CLOS >= 3
  printf("\nMLOG find-class returning class %p name--> %s\n", (void *)(result.get()), symbol->__repr__().c_str());
#endif
  return (Values(omc));
}

CL_LAMBDA(new-value name);
CL_DECLARE();
CL_DOCSTRING("setf_findClass");
CL_DEFUN Class_mv core__setf_find_class(T_sp newValue, Symbol_sp name, bool errorp, T_sp env) {
  if (!clos__classp(newValue)) {
    SIMPLE_ERROR(BF("Classes in cando have to be subclasses of Class unlike ECL which uses Instances to represent classes - while trying to (setf find-class) of %s you gave: %s") % _rep_(name) % _rep_(newValue));
  }
  if (_lisp->bootClassTableIsValid()) {
    return Values(_lisp->boot_setf_findClass(name, gc::As<Class_sp>(newValue)));
  }
  HashTable_sp ht = gc::As<HashTable_sp>(_sym_STARclassNameHashTableSTAR->symbolValue());
  T_sp oldClass = eval::funcall(cl::_sym_findClass, name, _Nil<T_O>());
  if (clos__classp(oldClass)) {
    SIMPLE_ERROR(BF("The built-in class associated to the CL specifier %s cannot be changed") % _rep_(name));
  } else if (newValue.nilp()) {
    ht->remhash(name);
  } else {
    ht->hash_table_setf_gethash(name, newValue);
  }
  return Values(gc::As<Class_sp>(newValue));
};

/*
  __BEGIN_DOC(candoScript.general.dumpEnvironment,dumpEnvironment)
  \scriptCmdRet{dumpEnvironment}{}{Text::packageName}

  Dump the current environment.
  __END_DOC
*/

CL_LAMBDA(partialPath);
CL_DECLARE();
CL_DOCSTRING("findFileInLispPath");
CL_DEFUN T_mv core__find_file_in_lisp_path(Str_sp partialPath) {
  LOG(BF("PartialPath=[%s]") % partialPath->get());
  Path_sp fullPath = _lisp->translateLogicalPathnameUsingPaths(partialPath);
  LOG(BF("fullPath is %s") % fullPath->asString());
  return (Values(fullPath));
}

CL_LAMBDA(name-desig);
CL_DECLARE();
CL_DOCSTRING("See CLHS: find-package");
CL_DEFUN T_sp cl__find_package(T_sp name_desig) {
  if (Package_sp pkg = name_desig.asOrNull<Package_O>())
    return pkg;
  Str_sp name = coerce::stringDesignator(name_desig);
  return _lisp->findPackage(name->get());
}

CL_LAMBDA(package-designator);
CL_DECLARE();
CL_DOCSTRING("selectPackage");
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
CL_DOCSTRING("mpi_enabled");
CL_DEFUN T_mv core__mpi_enabled() {
  return (Values(_lisp->_boolean(_lisp->mpiEnabled())));
}

/*
  __BEGIN_DOC(candoScript.general.mpiRank,mpiRank)
  \scriptCmdRet{mpiRank}{}{}

  Return the mpi rank or 0 if not enabled.
  __END_DOC
*/
CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("Return the mpi_rank or 0 if mpi is disabled");
CL_DEFUN T_mv core__mpi_rank() {
  return (Values(make_fixnum(_lisp->mpiRank())));
}

/*
  __BEGIN_DOC(candoScript.general.mpiSize,mpiSize)
  \scriptCmdRet{mpiSize}{}{}

  Return the mpi rank or 0 if not enabled.
  __END_DOC
*/

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("Return mpi_size or 0 if mpi is not enabled");
CL_DEFUN T_mv core__mpi_size() {
  return (Values(make_fixnum(_lisp->mpiSize())));
}

CL_LAMBDA(form &optional env);
CL_DECLARE();
CL_DOCSTRING("macroexpand_1");
CL_DEFUN T_mv cl__macroexpand_1(T_sp form, T_sp env) {
  T_sp expansionFunction = _Nil<T_O>();
  if (form.nilp()) {
    return form;
  } else if (form.consp()) {
    Cons_sp cform(reinterpret_cast<gctools::Tagged>(form.raw_()));
    T_sp head = cons_car(cform);
    if (cl__symbolp(head)) {
      Symbol_sp headSymbol = gc::As<Symbol_sp>(head);
      if (env.nilp()) {
        expansionFunction = eval::funcall(cl::_sym_macroFunction, headSymbol, env);
      } else if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
        expansionFunction = eval::funcall(cl::_sym_macroFunction, headSymbol, env);
      } else if (clcenv::Entry_sp ce = env.asOrNull<clcenv::Entry_O>() ) {
        expansionFunction = eval::funcall(cl::_sym_macroFunction, headSymbol, ce);
      } else {
        // It must be a Cleavir environment
        if (cleavirEnv::_sym_macroFunction->fboundp()) {
          expansionFunction = eval::funcall(cleavirEnv::_sym_macroFunction, headSymbol, env);
        }
      }
    }
    if (expansionFunction.notnilp()) {
      T_sp macroexpandHook = cl::_sym_STARmacroexpand_hookSTAR->symbolValue();
      Function_sp hookFunc = coerce::functionDesignator(macroexpandHook);
      T_sp expanded = eval::funcall(hookFunc, expansionFunction, form, env);
      return (Values(expanded, _lisp->_true()));
    }
    return (Values(form, _Nil<T_O>()));
  } else if (Symbol_sp sform = form.asOrNull<Symbol_O>()) {
    if (env.nilp()) {
      expansionFunction = core__lookup_symbol_macro(sform, env);
    } else if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
      expansionFunction = core__lookup_symbol_macro(sform, eenv);
    } else if (clcenv::Entry_sp cenv = env.asOrNull<clcenv::Entry_O>() ) {
      clcenv::Info_sp info = clcenv::variable_info(cenv,sform);
      if (clcenv::SymbolMacroInfo_sp smi = info.asOrNull<clcenv::SymbolMacroInfo_O>() ) {
        expansionFunction = smi->_Expansion;
      }
    } else {
      // It must be a Cleavir environment
      if (cleavirEnv::_sym_symbolMacroExpansion->fboundp()) {
        T_sp expanded = eval::funcall(cleavirEnv::_sym_symbolMacroExpansion, sform, env);
        if (expanded == sform) {
          return Values(sform, _Nil<T_O>());
        }
        return Values(expanded, _lisp->_true());
      } else {
        SIMPLE_ERROR(BF("Illegal environment for MACROEXPAND-1 of symbol-macro %s") % _rep_(sform));
      }
    }
    if (expansionFunction.notnilp()) {
      T_sp macroexpandHook = cl::_sym_STARmacroexpand_hookSTAR->symbolValue();
      Function_sp hookFunc = coerce::functionDesignator(macroexpandHook);
      T_sp expanded = eval::funcall(hookFunc, expansionFunction, form, env);
      if (expanded != form) {
        return (Values(expanded, _lisp->_true()));
      }
    }
    return Values(form, _Nil<T_O>());
  }
  return Values(form, _Nil<T_O>());
}

CL_LAMBDA(form &optional env);
CL_DECLARE();
CL_DOCSTRING("macroexpand");
CL_DEFUN T_mv cl__macroexpand(T_sp form, T_sp env) {
  ASSERT(env.generalp());
  bool sawAMacro = false;
  bool expandedMacro = false;
  uint macroExpansionCount = 0;
  if (_sym_STARdebugMacroexpandSTAR->symbolValue().isTrue()) {
    printf("%s:%d - macroexpanding --> %s\n", __FILE__, 2551, _rep_(form).c_str());
  }
  T_sp cur = form;
  do {
    T_mv mv = cl__macroexpand_1(cur, env);
    cur = mv;
    sawAMacro = gc::As<T_sp>(mv.valueGet_(1)).isTrue();
    expandedMacro |= sawAMacro;
    macroExpansionCount++;
    if (macroExpansionCount > 100) {
      SIMPLE_ERROR(BF("Macro expansion happened %d times - You may have a macro expansion infinite loop") % macroExpansionCount);
    }
  } while (sawAMacro);
  if (_sym_STARdebugMacroexpandSTAR->symbolValue().isTrue()) {
    printf("%s:%d -     after macroexpanding --> %s\n", __FILE__, 2565, _rep_(cur).c_str());
  }
  return (Values(cur, _lisp->_boolean(expandedMacro)));
};

void searchForApropos(List_sp packages, const string &raw_substring, bool print_values) {
  string substring = lispify_symbol_name(raw_substring);
  FindApropos apropos(substring);
  LOG(BF("Searching for symbols apropos to(%s)") % substring);
  for (auto cur : packages) {
    Package_sp pkg = gc::As<Package_sp>(oCar(cur));
    pkg->mapExternals(&apropos);
    pkg->mapInternals(&apropos);
  }
  apropos._symbols->mapHash([&print_values](T_sp key, T_sp dummy) {
                stringstream ss;
                Symbol_sp sym = gc::As<Symbol_sp>(key);
                ss << (BF("%50s") % (sym)->fullName()).str();
                if ( (sym)->specialP() || (sym)->fboundp() )
                {
                    if ( (sym)->fboundp() )
                    {
                        ss << " ";
                        ss << cl__class_of(cl__symbol_function((sym)))->classNameAsString();
			Function_sp fn = cl__symbol_function(sym);
                        if ( !fn.unboundp() && gc::As<Function_sp>(cl__symbol_function(sym))->macroP() )
                        {
                            ss << "(MACRO)";
                        }
                    }
                    if ( !(sym)->symbolValueUnsafe() ) {
                        ss << " !!UNDEFINED!!";
                    } else {
                        if ( (sym)->specialP() || (sym)->symbolValueUnsafe() )
                        {
                            ss << " VALUE";
                            if ( print_values )
                            {
                                stringstream sval;
                                T_sp symVal = (sym)->symbolValueUnsafe();
                                sval << _rep_(symVal);
                                ss << ": " << sval.str().substr(0,50);
                            }
                        }
                    }
                }
                _lisp->print(BF("%s") % ss.str());
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
CL_DOCSTRING("apropos");
CL_DEFUN T_sp cl__apropos(Str_sp string_desig, T_sp package_desig) {
  string substring = coerce::stringDesignator(string_desig)->get();
  List_sp packages(_Nil<List_V>());
  if (package_desig.nilp()) {
    packages = _lisp->allPackagesAsCons();
  } else {
    Package_sp pkg = coerce::packageDesignator(package_desig);
    packages = Cons_O::create(pkg, _Nil<T_O>());
  }
  searchForApropos(packages, substring, false);
  return (Values(_Nil<T_O>()));
}

/*
  __BEGIN_DOC(candoScript.general.funcall,funcall)
  \scriptCmdRet{funcall}{}{Function arg1 arg2 ...}

  Evaluate the function with the arguments.
  __END_DOC
*/

/*
  __BEGIN_DOC(candoScript.general.apply,apply)
  \scriptCmdRet{apply}{}{Function argList}

  Evaluate the function with the argument list.
  __END_DOC
*/

class OrderByLessThan {
public:
  bool operator()(T_sp x, T_sp y) {
    if (x.generalp()) {
      return x.unsafe_general()->operator<(y);
    }
    SIMPLE_ERROR(BF("Add support for operator< for class: %s") % _rep_(cl__class_of(x)));
  }
};

CL_LAMBDA(unsorted);
CL_DECLARE();
CL_DOCSTRING("Sort the list in ascending order using operator< and return the sorted list");
CL_DEFUN List_sp core__sorted(List_sp unsorted) {
  gctools::Vec0<T_sp /*,gctools::RootedGCHolder*/> sorted;
  if (cl__length(unsorted) == 0)
    return _Nil<T_O>();
  fillVec0FromCons(sorted, unsorted);
  OrderByLessThan orderer;
  sort::quickSort(sorted.begin(), sorted.end(), orderer);
  return asCons(sorted);
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
    this->_args = Cons_O::createList(_Nil<T_O>(), _Nil<T_O>());
  }
  bool operator()(T_sp x, T_sp y) {
    if ( this->_KeyFunction.nilp() ) {
      return T_sp(eval::funcall(this->_SortFunction, x, y)).isTrue();
    } else {
      T_sp kx = eval::funcall(this->_KeyFunction,x);
      T_sp ky = eval::funcall(this->_KeyFunction,y);
      return T_sp(eval::funcall(this->_SortFunction, kx, ky)).isTrue();
    }
  }
};

CL_LAMBDA(sequence predicate &key key);
CL_DECLARE();
CL_DOCSTRING("Like CLHS: sort but does not support key");
CL_DEFUN T_sp cl__sort(List_sp sequence, T_sp predicate, T_sp key) {
  gctools::Vec0<T_sp> sorted;
  Function_sp sortProc = coerce::functionDesignator(predicate);
  LOG(BF("Unsorted data: %s") % _rep_(sequence));
  if (cl__length(sequence) == 0)
    return _Nil<T_O>();
  fillVec0FromCons(sorted, sequence);
  LOG(BF("Sort function: %s") % _rep_(sortProc));
  OrderBySortFunction orderer(sortProc,key);
  sort::quickSort(sorted.begin(), sorted.end(), orderer);
  List_sp result = asCons(sorted);
  return result;
}

/*
  __BEGIN_DOC(candoScript.general.sourceFileName,sourceFileName)
  \scriptCmdRet{sourceFileName}{}{Cons::}

  Return the current file name and line number in a two element Cons.
  __END_DOC
*/

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("Return the current sourceFileName");
CL_DEFUN T_mv core__source_file_name() {
  Cons_sp ppcons;
  InvocationHistoryFrame *frame = my_thread->_InvocationHistoryStack;
  Function_sp closure = frame->function();
  int sourceFileInfoHandle = closure->sourceFileInfoHandle();
  string sourcePath = gc::As<SourceFileInfo_sp>(core__source_file_info(make_fixnum(sourceFileInfoHandle)))->namestring();
  Path_sp path = Path_O::create(sourcePath);
  Path_sp parent_path = path->parent_path();
  return Values(Str_O::create(path->fileName()), Str_O::create(parent_path->asString()));
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("sourceLineColumn");
CL_DEFUN T_mv core__source_line_column() {
  InvocationHistoryFrame *frame = my_thread->_InvocationHistoryStack;
  Function_sp closure = frame->function();
  return Values(make_fixnum(closure->lineNumber()), make_fixnum(closure->column()));
}

/*
  __BEGIN_DOC(candoScript.general.backtrace,backtrace)
  \scriptCmdRet{backtrace}{}{Cons::}

  Return a backtrace as a list of SourceCodeCons.
  __END_DOC
*/

/*
  __BEGIN_DOC(candoScript.general.databaseDir,databaseDir)
  \scriptCmdRet{databaseDir}{}{Text::}

  Return the path for the database directory.
  __END_DOC
*/



/*
  __BEGIN_DOC(candoScript.general.databaseDir,databaseDir)
  \scriptCmdRet{databaseDir}{}{Text::}

  Return the path for the database directory.
  __END_DOC
*/

/*
  __BEGIN_DOC(candoScript.general.changeWorkingDirectory,changeWorkingDirectory)
  \scriptCmdRet{changeWorkingDirectory}{}{Text::}

  Change the current working directory.
  __END_DOC
*/

/*
  __BEGIN_DOC(candoScript.general.isTopLevelScript)

  Return a true if this is a top level script or false if its an include file.
  __END_DOC
*/

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("isTopLevelScript");
CL_DEFUN T_mv core__is_top_level_script() {
  LOG(BF("isTopLevelScript = %d") % _lisp->getRequireLevel());
  T_sp top = _lisp->_boolean(_lisp->getRequireLevel() == 0);
  return (Values(top));
}

/*
  __BEGIN_DOC(candoScript.general.debugLogOn,debugLogOn)
  \scriptCmd{debugLogOn}{true/false:bool}

  Turn on or off writing debug statements to the debug log. This is useful when running
  long scripts that crash, you can turn of debug logging up to the point where
  the crash happens and then examine the output.
  __END_DOC
*/

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("debugLogOn");
CL_DEFUN void core__debug_log_on() {
  _lisp->debugLog().setSuppressMessages(false);
  LOG(BF("Turning debugLogOn"));
}

/*
  __BEGIN_DOC(candoScript.general.debugLogOff,debugLogOff)
  \scriptCmd{debugLogOff}{true/false:bool}

  Turn on or off writing debug statements to the debug log. This is useful when running
  long scripts that crash, you can turn of debug logging up to the point where
  the crash happens and then examine the output.
  __END_DOC
*/

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("debugLogOff");
CL_DEFUN void core__debug_log_off() {
  _lisp->debugLog().setSuppressMessages(true);
}

/*
  __BEGIN_DOC(candoScript.general.export,export)
  \scriptCmd{export}{symbols...}

  Tell the symbols that they can be exported.

  __END_DOC
*/
CL_LAMBDA(symDes &optional (packageDes *package*));
CL_DECLARE();
CL_DOCSTRING("CLHS: export");
CL_DEFUN T_sp cl__export(T_sp symDes, T_sp packageDes) {
  List_sp symbols = coerce::listOfSymbols(symDes);
  Package_sp package = coerce::packageDesignator(packageDes);
  for (auto sym : symbols) {
    package->_export2(gc::As<Symbol_sp>(oCar(sym)));
  }
  return _lisp->_true();
}

CL_LAMBDA(symbolsDesig);
CL_DECLARE();
CL_DOCSTRING("exportToPython");
CL_DEFUN void core__export_to_python(T_sp symbolsDesig) {
  List_sp symbols = coerce::listOfSymbols(symbolsDesig);
  for (Cons_sp cur : symbols) {
    Symbol_sp one = gc::As<Symbol_sp>(oCar(cur));
    LOG(BF("Exporting symbol[%s] to python") % _rep_(one));
    _lisp->exportToPython(one);
  }
}

CL_LAMBDA(symbol-name &optional (package-desig *package*));
CL_DECLARE();
CL_DOCSTRING("See CLHS: intern");
CL_DEFUN T_mv cl__intern(Str_sp symbol_name, T_sp package_desig) {
  Package_sp package = coerce::packageDesignator(package_desig);
  return (package->intern(symbol_name));
}

CL_LAMBDA(continue-string datum initializers);
CL_DECLARE();
CL_DOCSTRING("universalErrorHandler");
CL_DEFUN T_mv core__universal_error_handler(T_sp continueString, T_sp datum, List_sp initializers) {
  if (cl__stringp(datum)) {
    cl__format(_lisp->_true(), datum, initializers);
  } else {
    stringstream ss;
    ss << "datum: " << _rep_(datum) << " " << _rep_(initializers);
    printf("%s\n", ss.str().c_str());
  }
  dbg_hook("universalErrorHandler");
  core__invoke_internal_debugger(_Nil<T_O>());
  abort();
};

CL_LAMBDA(&optional condition);
CL_DECLARE();
CL_DOCSTRING("invokeInternalDebugger");
CL_DEFUN void core__invoke_internal_debugger(T_sp condition) {
  stringstream ss;
  if (condition.nilp()) {
    LispDebugger debugger;
    debugger.invoke();
  } else {
    _lisp->print(BF("%s:%d core__invoke_internal_debugger --> %s") % __FILE__ % __LINE__ % _rep_(condition).c_str());
    LispDebugger debugger(condition);
    debugger.invoke();
  }
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("singleDispatchGenericFunctionTable");
CL_DEFUN HashTable_sp core__single_dispatch_generic_function_table() {
  return _lisp->singleDispatchGenericFunctionTable();
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("invokeInternalDebuggerFromGdb");
CL_DEFUN void core__invoke_internal_debugger_from_gdb() {
  eval::funcall(_sym_invokeInternalDebugger);
  SIMPLE_ERROR(BF("This should never happen"));
};

CL_LAMBDA(datum &rest arguments);
CL_DECLARE();
CL_DOCSTRING("See CLHS error");
CL_DEFUN void cl__error(T_sp datum, List_sp initializers) {
  int nestedErrorDepth = unbox_fixnum(gc::As<Fixnum_sp>(_sym_STARnestedErrorDepthSTAR->symbolValue()));
  if (nestedErrorDepth > 10) {
    // TODO: Disable this code once error handling and conditions work properly
    // It's only here to identify errors that would cause infinite looping
    // as we get error handling and conditions working properly
    printf("%s:%d -- *nested-error-depth* --> %d  datum: %s\n", __FILE__, __LINE__, nestedErrorDepth, _rep_(datum).c_str());
    asm("int $03");
    if (initializers.notnilp()) {
      printf("               initializers: %s\n", _rep_(initializers).c_str());
    }
    printf("Dumping backtrace\n");
    core__low_level_backtrace();
  }
  ++nestedErrorDepth;
  DynamicScopeManager scope(_sym_STARnestedErrorDepthSTAR, make_fixnum(nestedErrorDepth));
  if (_sym_universalErrorHandler->fboundp()) {
    Function_sp fn = _sym_universalErrorHandler->symbolFunction();
    eval::funcall(fn, _Nil<T_O>(), datum, initializers);
  }
  THROW_HARD_ERROR(BF("cl__error should never return because universal-error-handler should never return - but it did"));
}

CL_LAMBDA(cformat eformat &rest arguments);
CL_DECLARE();
CL_DOCSTRING("See CLHS cerror");
CL_DEFUN void cl__cerror(T_sp cformat, T_sp eformat, List_sp arguments) {
  eval::funcall(_sym_universalErrorHandler, cformat, eformat, arguments);
}

/*
  __BEGIN_DOC(candoScript.general.load,load)
  \scriptCmd{load}{Text::fileName}

  Open the \sa{fileName}, compile and evaluate its contents.
  It looks through all of the directories in the global variable PATH and then
  the Scripts directory in the Cando application directory.

  __END_DOC
*/

/*
  __BEGIN_DOC(candoScript.general.isAssignableTo,isAssignableTo)
  \scriptInfixRet{Object::object}{isAssignableTo}{Class::classObject}{Bool::}

  Return true if \scriptArg{object} can be assigned to a C++ variable of class \scriptArg{classObject}.
  __END_DOC
*/

#if 0
CL_LAMBDA(tag secondArgument);
CL_DECLARE();
CL_DOCSTRING("isAssignableTo");
CL_DEFUN T_mv core__is_assignable_to(T_sp tag, Class_sp mc) {
  LOG(BF("Checking if instances of class(%s) is assignable to variables of class(%s)") % cl__class_of(tag)->className() % cl__class_of(mc)->className());
  bool io = (tag->isAssignableToByClassSymbol(mc->name()));
  return (Values(_lisp->_boolean(io)));
}
#endif

/*
  __BEGIN_DOC(candoScript.general.isSubClassOf,isSubClassOf)
  \scriptInfixRet{Object::object}{isSubClassOf}{Class::classObject}{Bool::}

  Return true if \scriptArg{object} can be assigned to a C++ variable of class \scriptArg{classObject}.
  __END_DOC
*/

CL_LAMBDA(tag mc);
CL_DECLARE();
CL_DOCSTRING("isSubClassOf");
CL_DEFUN T_mv core__is_sub_class_of(Class_sp tag, Class_sp mc) {
  LOG(BF("Checking if instances of class(%s) is assignable to variables of class(%s)") % tag->className() % mc->className());
  bool io = tag->isSubClassOf(mc);
  return (Values(_lisp->_boolean(io)));
}

/*
  __BEGIN_DOC(candoScript.general.repr,repr)
  \scriptCmdRet{repr}{object}{string}

  Return a string representation of the object.
  __END_DOC
*/

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("Return a string representation of the object");
CL_DEFUN T_mv core__repr(T_sp obj) {
  Str_sp res = Str_O::create(_rep_(obj));
  return (Values(res));
}

/*
  __BEGIN_DOC(candoScript.general.list,list)
  \scriptCmdRet{list}{object1 object2 ...}{list}\par
  \scriptCmdRet{:}{object1 object2 ...}{list}

  Return a list formed by evaluating the arguments.
  __END_DOC
*/

/*
  __BEGIN_DOC(candoScript.general.not,not)
  \scriptCmdRet{not}{boolA}{bool}\par

  Return not boolA.
  __END_DOC
*/

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("not");
CL_DEFUN T_mv cl__not(T_sp x) {
  return (Values(_lisp->_boolean(!x.isTrue())));
};

/*
  __BEGIN_DOC(candoScript.general.printPushPrefix,printPushPrefix)
  \scriptCmd{printPushPrefixln}{args ...}\par

  Push a prefix to be printed everytime print is called the arguments followed by a new line.
  __END_DOC
*/

/*
  __BEGIN_DOC(candoScript.general.printPopPrefix,printPopPrefix)
  \scriptCmd{printPopPrefixln}{args ...}\par

  Pop a prefix to be printed everytime print is called the arguments followed by a new line.
  __END_DOC
*/

/*
  __BEGIN_DOC(candoScript.general.print,print)
  \scriptCmd{println}{args ...}\par

  Print new line followed by string representations of the arguments.
  __END_DOC
*/

Symbol_sp Lisp_O::getClassSymbolForClassName(const string &name) {
  Class_sp mc = this->classFromClassName(name);
  Symbol_sp sym = mc->className();
  ASSERTNOTNULL(sym);
  return sym;
}

T_sp Lisp_O::createObjectOfClass(T_sp mc) {
  if (clos__classp(mc)) {
    LOG(BF("createObjectOfClass(%s)") % _rep_(mc));
    IMPLEMENT_ME();
    T_sp obj = gc::As<Class_sp>(mc)->allocate_newNil();
    if ( obj.generalp() ) {
      obj.unsafe_general()->initialize();
    } else if ( obj.consp() ) {
      // Nothing
    } else {
      SIMPLE_ERROR(BF("Add support to initialize %s") % _rep_(cl__class_of(obj)));
    }
    return obj;
  }
  SIMPLE_ERROR(BF("Handle createObjectOfClass when mc is not a Class"));
}

void Lisp_O::setEmbeddedInPython(bool b) {
  _OF();
  LOG(BF("EmbeddedInPython is being set to[%d]") % b);
  this->_EmbeddedInPython = b;
}

Class_sp Lisp_O::boot_setf_findClass(Symbol_sp className, Class_sp mc) {
  for (auto it = this->_Roots.bootClassTable.begin(); it != this->_Roots.bootClassTable.end(); ++it) {
    if (it->symbol == className) {
      it->theClass = mc;
      return mc;
    }
  }
  SymbolClassPair sc(className, mc);
  this->_Roots.bootClassTable.push_back(sc);
  return mc;
}

Class_sp Lisp_O::boot_findClass(Symbol_sp className, bool errorp) const {
  ASSERTF(this->_BootClassTableIsValid,
          BF("Never use Lisp_O::findClass after boot - use cl::_sym_findClass"));
  for (auto it = this->_Roots.bootClassTable.begin(); it != this->_Roots.bootClassTable.end(); ++it) {
    if (it->symbol == className)
      return it->theClass;
  }
  return _Nil<Class_O>();
}

/*! After the core classes are defined and we have hash-tables, move all class definitions
  into the *class-name-hash-table*.  We do this because we can't put stuff into a hash-table
  until the hash-table class is defined and we need classes in the *class-name-hash-table* once
  CLOS starts up because that is where ECL expects to find them. */
void Lisp_O::switchToClassNameHashTable() {
  ASSERTF(this->_BootClassTableIsValid, BF("switchToClassNameHashTable should only be called once after boot"));
  HashTable_sp ht = gc::As<HashTable_sp>(_sym_STARclassNameHashTableSTAR->symbolValue());
#if 0
	for ( SymbolDict<Class_O>::iterator it=this->_Roots._BootClassTable.begin();
	      it!=this->_Roots._BootClassTable.end(); it++ )
	{
	    ht->hash_table_setf_gethash(it->first,it->second);
	}
#else
  for (auto it = this->_Roots.bootClassTable.begin(); it != this->_Roots.bootClassTable.end(); ++it) {
    ht->hash_table_setf_gethash(it->symbol, it->theClass);
  }
  this->_Roots.bootClassTable.clear();
#endif
  this->_BootClassTableIsValid = false;
}

CL_LAMBDA(gf-symbol &optional errorp);
CL_DOCSTRING("Lookup a single dispatch generic function. If errorp is truen and the generic function isn't found throw an exception");
CL_LISPIFY_NAME(find_single_dispatch_generic_function);
CL_DEFUN T_sp Lisp_O::find_single_dispatch_generic_function(Symbol_sp gfSym, bool errorp) {
  T_sp fn = _lisp->_Roots._SingleDispatchGenericFunctionTable->gethash(gfSym, _Nil<T_O>());
  if (fn.nilp()) {
    if (errorp) {
      SIMPLE_ERROR(BF("No single-dispatch-generic-function named %s") % _rep_(gfSym));
    }
    return _Nil<T_O>();
  }
  return gc::As<SingleDispatchGenericFunctionClosure_sp>(fn);
}

CL_LAMBDA(gf-symbol gf)
CL_LISPIFY_NAME(setf_find_single_dispatch_generic_function);
CL_DOCSTRING("Define a single dispatch generic function");
CL_DEFUN T_sp Lisp_O::setf_find_single_dispatch_generic_function(Symbol_sp gfName, SingleDispatchGenericFunctionClosure_sp gf) {
  _lisp->_Roots._SingleDispatchGenericFunctionTable->setf_gethash(gfName, gf);
  return gf;
}

CL_LISPIFY_NAME(forget_all_single_dispatch_generic_functions);
CL_DEFUN void Lisp_O::forget_all_single_dispatch_generic_functions() {
  _lisp->_Roots._SingleDispatchGenericFunctionTable->clrhash();
}

#if 0
    GenericFunction_sp Lisp_O::setf_findGenericFunction(Symbol_sp gfName, GenericFunction_sp mc)
    {_OF();
	this->_GenericFunctionTable[gfName] = mc;
	return mc;
    }


    GenericFunction_sp Lisp_O::findGenericFunction(Symbol_sp gfName, bool errorp) const
    {_OF();
	SymbolMap<GenericFunction_O>::const_iterator fi = this->_GenericFunctionTable.find(gfName);
	if ( fi == this->_GenericFunctionTable.end() )
	{
	    if ( errorp )
	    {
		SIMPLE_ERROR(BF("No generic function named %s") % gfName->__repr__() );
	    }
	    return _Nil<GenericFunction_O>();
	}
	return fi->second;
    }

#endif

#if 0
    Class_sp Lisp_O::classFromClassSymbol(Symbol_sp cid) const
    {
	DEPRECIATED();
#if 0
	return this->findClass(cid,true);
#endif
    }
#endif

string Lisp_O::classNameFromClassSymbol(Symbol_sp cid) {
  DEPRECIATED();
#if 0
	Class_sp mc = this->classFromClassSymbol(cid);
	return mc->getPackagedName();
#endif
}

Class_sp Lisp_O::classFromClassName(const string &name) {
  _OF();
  DEPRECIATED();
  //    return sym->symbolValue().as<Class_O>();
}

void Lisp_O::parseStringIntoPackageAndSymbolName(const string &name, bool &packageDefined, Package_sp &package, string &symbolName, bool &exported) const {
  _OF();
  packageDefined = true; // this will be true in most cases below
  LOG(BF("Parsing string[%s] into package and symbol name") % name);
  if (name[0] == ':') {
    LOG(BF("It's a keyword"));
    package = this->_Roots._KeywordPackage;
    symbolName = name.substr(1, 99999);
    exported = true;
    return;
  }
  if (name[0] == '&') {
    LOG(BF("It's an & symbol"));
    package = this->_Roots._CorePackage;
    symbolName = name;
    exported = true;
    return;
  }
  size_t colonPos = name.find_first_of(":");
  exported = true;
  if (colonPos == string::npos) {
    LOG(BF("Package wasn't defined"));
    symbolName = name;
    packageDefined = false;
    return;
  }
  bool doubleColon = false;
  size_t secondPart = colonPos + 1;
  if (name[secondPart] == ':') {
    LOG(BF("It's a non-exported symbol"));
    exported = false;
    doubleColon = true;
    secondPart++;
    if (name.find_first_of(":", secondPart) != string::npos) {
      SIMPLE_ERROR(BF("There can only be one ':' or '::' in a symbol name"));
    }
  }
  package = gc::As<Package_sp>(this->findPackage(name.substr(0, colonPos), true));
  symbolName = name.substr(secondPart, 99999);
  LOG(BF("It's a packaged symbol (%s :: %s)") % package->getName() % symbolName);
  return;
}

Symbol_mv Lisp_O::intern(const string &name, T_sp optionalPackageDesignator) {
#if DEBUG_ENVIRONMENT_CREATION
//    ASSERT(this->_PackagesInitialized);
#endif
  Package_sp package;
  string symbolName;
  bool exported, packageDefined;
  this->parseStringIntoPackageAndSymbolName(name, packageDefined, package, symbolName, exported);
  if (!packageDefined) {
    package = coerce::packageDesignator(optionalPackageDesignator);
  }
  ASSERTNOTNULL(package);
  ASSERT(package.notnilp());
  Str_sp sname = Str_O::create(symbolName);
  T_mv symStatus = package->intern(sname);
  Symbol_sp sym = gc::As<Symbol_sp>(symStatus);
  T_sp status = symStatus.second();
  return Values(sym, status);
}

/*! The optionalPackageDesignator is nil */
Symbol_sp Lisp_O::intern(string const &symbolName) {
  Package_sp curPackage = this->getCurrentPackage();
  ASSERTNOTNULL(curPackage);
  return this->intern(symbolName, curPackage);
}

Symbol_sp Lisp_O::findSymbol(const string &name, T_sp optionalPackageDesignator) const {
#if DEBUG_ENVIRONMENT_CREATION
  ASSERT(this->_PackagesInitialized);
#endif
  Package_sp package;
  string symbolName;
  bool exported, packageDefined;
  this->parseStringIntoPackageAndSymbolName(name, packageDefined, package, symbolName, exported);
  if (!packageDefined) {
    package = coerce::packageDesignator(optionalPackageDesignator);
  }
  return gc::As<Symbol_sp>(package->findSymbol(symbolName));
}

Symbol_sp Lisp_O::findSymbol(const string &symbolName /*, T_sp optionalPackageDesignator = nil */) const {
  return this->findSymbol(symbolName, _Nil<T_O>());
}

Symbol_sp Lisp_O::intern(string const &symbolName, string const &packageName) {
  T_sp package = this->findPackage(packageName);
  return this->intern(symbolName, package);
}

Symbol_sp Lisp_O::internUniqueWithPackageName(string const &packageName, string const &symbolName) {
  T_sp package = this->findPackage(packageName);
  Symbol_mv symStatus = this->intern(symbolName, package);
  //  T_sp status = symStatus.second();
  return symStatus;
}

Symbol_sp Lisp_O::internWithPackageName(string const &packageName, string const &symbolName) {
  Package_sp package = gc::As<Package_sp>(this->findPackage(packageName, true));
  return this->intern(symbolName, package);
}

Symbol_sp Lisp_O::internWithDefaultPackageName(string const &defaultPackageName,
                                               string const &possiblePackagePrefixedSymbolName) {
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

Symbol_sp Lisp_O::internKeyword(const string &name) {
  string realName = name;
  boost::to_upper(realName);
  Str_sp str_real_name = Str_O::create(realName);
  return gc::As<Symbol_sp>(this->_Roots._KeywordPackage->intern(str_real_name));
}

void Lisp_O::dump_apropos(const char *part) const {
  _OF();
  string substring = part;
  List_sp packages = _lisp->allPackagesAsCons();
  searchForApropos(packages, substring, true);
}

List_sp Lisp_O::allPackagesAsCons() const {
  return asCons(this->_Roots._Packages);
}

#if 0
InvocationHistoryStack &Lisp_O::invocationHistoryStack() {
  return this->_Roots._InvocationHistoryStack;
}
#endif

void Lisp_O::dump_backtrace(int numcol) {
  _OF();
  string bt = backtrace_as_string();
  _lisp->print(BF("%s") % bt);
}

#if 0
    void Lisp_O::gdb_trace_by_name(const char* names)
    {_OF();
	string snames = lispify_symbol_name(names);
	if ( snames == "" )
	{
	    _lisp->print(BF("Tracing: "));
	    for ( auto ti=this->_Roots._TraceFunctions.begin(); ti!=this->_Roots._TraceFunctions.end(); ti++ )
	    {
		_lisp->print(BF("%s") % _rep_((*ti)->getFunctionName()) );
	    }
	    return;
	}
	vector<string> vnames = split(snames," ");
	for ( vector<string>::iterator it=vnames.begin(); it!=vnames.end(); it++ )
	{
	    Symbol_sp sym = this->intern(*it);
	    if ( sym->fboundp() )
	    {
		Function_sp fn = sym->symbolFunction();
		this->_Roots._TraceFunctions.addUnique(fn,_lisp);
		_lisp->print(BF("trace: %s") % _rep_(sym) );
	    } else
	    {
		_lisp->print(BF("Cannot trace function[%s] - it doesnt' exist") % _rep_(sym) );
	    }
	}
    }




    void Lisp_O::gdb_untrace_by_name(const char* names)
    {_OF();
	string snames = lispify_symbol_name(names);
	if ( snames == "" )
	{
	    _lisp->print(BF("untracing all"));
	    this->_Roots._TraceFunctions.clear();
	    return;
	}
	vector<string> vnames = split(snames," ");
	for ( auto it=vnames.begin(); it!=vnames.end(); it++ )
	{
	    Symbol_sp sym = this->intern(*it);
	    if ( sym->fboundp() )
	    {
		Function_sp fn = sym->symbolFunction();
		if (this->_Roots._TraceFunctions.count(fn)>0) this->_Roots._TraceFunctions.erase(fn);
	    }
	}
    }
#endif


void Lisp_O::run() {
  if ( initializer_functions_are_waiting() ) {
    initializer_functions_invoke();
  }
#ifndef SCRAPING
#define ALL_INITIALIZERS_CALLS
#include INITIALIZERS_INC_H
#undef ALL_INITIALIZERS_CALLS
#endif
  
#ifdef DEBUG_PROGRESS
  printf("%s:%d run\n", __FILE__, __LINE__ );
#endif

  // If the user adds "-f debug-startup" to the command line
  // then set core::*debug-startup* to true
  // This will print timings of top-level forms as they load at startup
  // See llvmo::intrinsics.cc
  // cl__member isn't available yet so check for the feature by hand.
  for (auto cur : (List_sp)cl::_sym_STARfeaturesSTAR->symbolValue()) {
    if (oCar(cur) == kw::_sym_debugStartup) {
      printf("%s:%d Setting core:*debug-startup* to T\n", __FILE__, __LINE__);
      _sym_STARdebugStartupSTAR->setf_symbolValue(_lisp->_true());
    }
  }
  if (!this->_IgnoreInitImage) {
    if ( startup_functions_are_waiting() ) {
      startup_functions_invoke();
    } else {
      Pathname_sp initPathname = gc::As<Pathname_sp>(_sym_STARcommandLineImageSTAR->symbolValue());
      DynamicScopeManager scope(_sym_STARuseInterpreterForEvalSTAR, _lisp->_true());
      T_mv result = eval::funcall(cl::_sym_load, initPathname); // core__load_bundle(initPathname);
      if (result.nilp()) {
        T_sp err = result.second();
        printf("Could not load bundle %s error: %s\n", _rep_(initPathname).c_str(), _rep_(err).c_str());
      }
    }
  } else if (!this->_IgnoreInitLsp) {
    // Assume that if there is no program then
    // we want an interactive script
    //
    {
      _BLOCK_TRACEF(BF("Evaluating initialization code in(%s)") % this->_RCFileName);
      Pathname_sp initPathname = cl__pathname(Str_O::create(this->_RCFileName));
      T_mv result = eval::funcall(cl::_sym_load, initPathname);
      if (result.nilp()) {
        T_sp err = result.second();
        printf("Could not load %s error: %s\n", _rep_(initPathname).c_str(), _rep_(err).c_str());
      }
    }
  } else {
    _BLOCK_TRACE("Interactive REPL");
      //
      // Implement a Read-Eval-Print-Loop
      //
    this->print(BF("Clasp (copyright Christian E. Schafmeister 2014)\n"));
    this->print(BF("Low level repl\n"));
    while (1) {
      this->readEvalPrintInteractive();
    }
  }
};

SourceFileInfo_mv Lisp_O::getOrRegisterSourceFileInfo(const string &fileName, T_sp sourceDebugNamestring, size_t sourceDebugOffset, bool useLineno) {
  map<string, int>::iterator it = this->_SourceFileIndices.find(fileName);
  if (it == this->_SourceFileIndices.end()) {
    if (this->_Roots._SourceFiles.size() == 0) {
      SourceFileInfo_sp unknown = SourceFileInfo_O::create("-unknown-file-", 0, sourceDebugNamestring, sourceDebugOffset);
      this->_Roots._SourceFiles.push_back(unknown);
    }
    int idx = this->_Roots._SourceFiles.size();
    this->_SourceFileIndices[fileName] = idx;
    SourceFileInfo_sp sfi = SourceFileInfo_O::create(fileName, idx, sourceDebugNamestring, sourceDebugOffset, useLineno);
    this->_Roots._SourceFiles.push_back(sfi);
    return Values(sfi, make_fixnum(idx));
  }
  SourceFileInfo_sp sfi = this->_Roots._SourceFiles[it->second];
  if (sourceDebugNamestring.notnilp()) {
    sfi->_SourceDebugNamestring = gc::As<Str_sp>(sourceDebugNamestring);
    sfi->_SourceDebugOffset = sourceDebugOffset;
    sfi->_TrackLineno = useLineno;
  }
  return Values(sfi, make_fixnum(it->second));
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("List all of the source files");
CL_DEFUN List_sp core__all_source_files() {
  List_sp list = _Nil<T_O>();
  for (auto it : _lisp->_SourceFileIndices) {
    Str_sp sf = Str_O::create(it.first);
    list = Cons_O::create(sf, list);
  };
  return list;
}

void Lisp_O::mapClassNamesAndClasses(KeyValueMapper *mapper) {
  if (this->_BootClassTableIsValid) {
#if 0
	    for ( SymbolDict<Class_O>::iterator it=this->_Roots._BootClassTable.begin();
		  it!=this->_Roots._BootClassTable.end(); it++ )
	    {
		if (!mapper->mapKeyValue(it->first,it->second)) break;
	    }
#else
    for (auto it = this->_Roots.bootClassTable.begin(); it != this->_Roots.bootClassTable.end(); ++it) {
      if (!mapper->mapKeyValue(it->symbol, it->theClass))
        return;
    }
    return;
#endif
  } else {
    HashTable_sp ht = gc::As<HashTable_sp>(_sym_STARclassNameHashTableSTAR->symbolValue());
    ht->lowLevelMapHash(mapper);
  }
}

string Lisp_O::__repr__() const {
  stringstream ss;
  ss << "Lisp_O object";
  return ss.str();
};

SYMBOL_EXPORT_SC_(CorePkg, selectPackage);

void Lisp_O::initializeGlobals(Lisp_sp lisp) {
}




LispHolder::LispHolder(bool mpiEnabled, int mpiRank, int mpiSize) {
  this->_Lisp = Lisp_O::createLispEnvironment(mpiEnabled, mpiRank, mpiSize);
}

void LispHolder::startup(int argc, char *argv[], const string &appPathEnvironmentVariable) {
  this->_Lisp->_StackTop = gctools::_global_stack_marker; // (char *)&argc;
  ::_lisp = this->_Lisp;

  const char *argv0 = "./";
  if (argc > 0)
    argv0 = argv[0];
  this->_Lisp->_Argc = argc;
  for (int i = 0; i < argc; ++i) {
    this->_Lisp->_Argv.push_back(string(argv[i]));
  }
  Bundle *bundle = new Bundle();
  bundle->initialize(argv0, appPathEnvironmentVariable);
  this->_Lisp->startupLispEnvironment(bundle);
#if 0
	if (_lisp->mpiEnabled())
	{
	    stringstream ss;
	    ss << "P"<<_lisp->mpiRank()<<":";
	    printvPushPrefix(ss.str());
	}
#endif
  _lisp->parseCommandLineArguments(argc, argv, true);
}

LispHolder::~LispHolder() {
  this->_Lisp->shutdownLispEnvironment();
}

Exposer_O::Exposer_O(Lisp_sp lisp, const string &packageName, const char *nicknames[]) {
  if (!lisp->recognizesPackage(packageName)) {
    list<string> lnnames;
    for (int i = 0; strcmp(nicknames[i], "") != 0; i++) {
      lnnames.push_front(nicknames[i]);
    }
    list<string> lp;
    this->_Package = lisp->makePackage(packageName, lnnames, lp);
  } else {
    this->_Package = gc::As<Package_sp>(lisp->findPackage(packageName, true));
  }
  this->_PackageName = packageName;
}

Exposer_O::Exposer_O(Lisp_sp lisp, const string &packageName) {
  if (!lisp->recognizesPackage(packageName)) {
    list<string> lnnames;
    list<string> lpkgs;
    this->_Package = lisp->makePackage(packageName, lnnames, lpkgs);
  } else {
    this->_Package = gc::As<Package_sp>(lisp->findPackage(packageName, true));
  }
  this->_PackageName = packageName;
}

Exposer_O::~Exposer_O(){};



ChangePackage::ChangePackage(Package_sp newPackage) : _SavedPackage(_lisp->getCurrentPackage()) {
  _lisp->selectPackage(newPackage);
}

ChangePackage::~ChangePackage() {
  _lisp->selectPackage(this->_SavedPackage);
}

SYMBOL_SC_(CorePkg, find_single_dispatch_generic_function);
SYMBOL_SC_(CorePkg, setf_find_single_dispatch_generic_function);
SYMBOL_SC_(CorePkg, forget_all_single_dispatch_generic_functions);
SYMBOL_SC_(CorePkg, stackMonitor);
SYMBOL_SC_(CorePkg, setupStackMonitor);
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
SYMBOL_SC_(CorePkg, sourceFileName);
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
SYMBOL_EXPORT_SC_(ClPkg, not);
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
SYMBOL_SC_(CorePkg, exportToPython);
SYMBOL_EXPORT_SC_(ClPkg, find_package);

void initialize_Lisp_O() {
};
};
