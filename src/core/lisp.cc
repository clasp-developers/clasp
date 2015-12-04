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
#include <clasp/core/executables.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/lispReader.h>
#include <clasp/core/write_object.h>
#include <clasp/core/write_ugly.h>
#include <clasp/core/lispMath.h>
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
#include <clasp/core/executables.h>
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
//#i n c l u d e "clos.h"
#include <clasp/core/wrappers.h>
#include <clasp/core/python_wrappers.h>

#ifdef READLINE
extern "C" char *readline(const char *prompt);
extern "C" void add_history(char *line);
#endif

/*! A function that creates the source-main: host */
extern void create_source_main_host();

namespace core {

__thread ThreadInfo *threadLocalInfoPtr;

const int Lisp_O::MaxFunctionArguments = 64; //<! See ecl/src/c/main.d:163 ecl_make_cache(64,4096)
const int Lisp_O::MaxClosSlots = 3;          //<! See ecl/src/c/main.d:164 ecl_make_cache(3,4096)
const int Lisp_O::ClosCacheSize = 1024 * 16;
const int Lisp_O::SingleDispatchMethodCacheSize = 1024 * 8;

extern void lispScannerDebug(std::istream &sin);
extern string getLispError();

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
Lisp_O::GCRoots::GCRoots() : _BufferStringPool(_Nil<T_O>()), _MultipleValuesCur(NULL), _BignumRegister0(_Unbound<Bignum_O>()), _BignumRegister1(_Unbound<Bignum_O>()), _BignumRegister2(_Unbound<Bignum_O>())
                             //                               , _TraceFunctions(_Unbound<HashTable_O>())
                             ,
                             _SystemProperties(_Nil<T_O>()), _CatchInfo(_Nil<T_O>()), _SpecialForms(_Unbound<HashTableEq_O>()), _NullStream(_Nil<T_O>()), _PathnameTranslations(_Nil<T_O>()) {}

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
  this->_Roots._Bindings.reserve(1024);
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

void Lisp_O::initialize() {
  // do nothing
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
  _G();
  stringstream ss;
  ss << "------------------------------------- class" << _rep_(co->className()) << std::endl;
  ;
  LOG(BF("Dumping info: %s") % co->dumpInfo());
  ss << co->dumpInfo();
  return ss.str();
}
template <class oclass>
void dump_info(BuiltInClass_sp co, Lisp_sp lisp) {
  _G();
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
  TEST_ASSERT_ALWAYS(cl_length(val1) == 1);
  List_sp val2 = eval::funcall(cl::_sym_list, cl::_sym_nil, cl::_sym_nil);
  TEST_ASSERT_ALWAYS(cl_length(val2) == 2);
  List_sp val3 = eval::funcall(cl::_sym_list, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil);
  TEST_ASSERT_ALWAYS(cl_length(val3) == 3);
  List_sp val4 = eval::funcall(cl::_sym_list, clasp_make_fixnum(1), clasp_make_fixnum(2), clasp_make_fixnum(3), clasp_make_fixnum(4));
  TEST_ASSERT_ALWAYS(cl_length(val4) == 4);
  List_sp val5 = eval::funcall(cl::_sym_list, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil);
  TEST_ASSERT_ALWAYS(cl_length(val5) == 5);
  List_sp val6 = eval::funcall(cl::_sym_list, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil);
  TEST_ASSERT_ALWAYS(cl_length(val6) == 6);
  List_sp val7 = eval::funcall(cl::_sym_list, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil, cl::_sym_nil);
  TEST_ASSERT_ALWAYS(cl_length(val7) == 7);

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
  gctools::tagged_pointer<CoreExposer> coreExposerPtr;
  BuiltInClass_sp classDummy;
  {
    _BLOCK_TRACE("Initialize core classes");
    coreExposerPtr = CoreExposer::create_core_packages_and_classes();
    // TODO: Should this be a WeakKeyHashTable?
    {
      _BLOCK_TRACE("Define important predefined symbols for CorePkg");
      coreExposerPtr->define_essential_globals(_lisp);
      this->_PackagesInitialized = true;
    }
  }
  {
    _BLOCK_TRACE("Create some housekeeping objects");
    this->_Roots._LoadTimeValueArrays = HashTableEqual_O::create_default();
    this->_Roots._SetfDefinitions = HashTableEq_O::create_default();
    this->_Roots._SingleDispatchGenericFunctionTable = HashTableEq_O::create_default();
  }
  {
    _BLOCK_TRACE("Initialize special forms and macros");
    this->_EnvironmentInitialized = true;
    eval::defineSpecialOperatorsAndMacros(this->_Roots._CorePackage);
    //	    this->createHiddenBinder();
  }
  this->_BuiltInClassesInitialized = true;
  //	LOG(BF("ALL CLASSES: %s")% this->dumpClasses() );
  //    this->createNils();
  {
    _BLOCK_TRACE("Dump of all BuiltInClass classes");
#ifdef DEBUG_ON
//    rootClassManager().debugDump();
#endif
  }

  //	LOG(BF("Package(%s) symbols: %s")% this->_CorePackage->getName() % this->_CorePackage->allSymbols() );
  //	LOG(BF("Package(%s) symbols: %s")% keywordPackage->getName() % keywordPackage->allSymbols() );

  //
  // Finish initializing Lisp object
  //
  this->_Roots._CommandLineArguments = _Nil<T_O>();
  {
    _BLOCK_TRACE("Initialize other code"); // needs _TrueObject
#define Use_CorePkg
//#include "core_initScripting_inc.h"
#undef Use_CorePkg

    //            testStrings();
    initialize_functions();
    core::HashTableEql_sp ht = core::HashTableEql_O::create_default();
    initialize_source_info(ht);
    core::_sym_STARcxxDocumentationSTAR->defparameter(ht);
    initialize_object();
    initialize_foundation();
    initialize_primitives();
    initialize_stacks();
    initialize_documentation_primitives(_lisp);
    initialize_compiler_primitives(_lisp);
    initialize_cache();
    initialize_backquote(_lisp);
#ifdef DEBUG_CL_SYMBOLS
    initializeAllClSymbolsFunctions();
#endif
    initialize_sequence();
    initialize_list();
    initialize_bits();
    initialize_predicates();
    initialize_bformat(_lisp);
    initialize_sysprop();
    initialize_testing();
    initialize_profile();
    initialize_designators();
    initialize_debugging();
    initialize_math();
    initialize_string();
    initialize_unixfsys();
    initialize_lispStream();
    initialize_pathname();
    initialize_numberToString();
    initialize_print();
    initialize_load();
    initialize_num_arith();
    initialize_num_co();
    initialize_float_to_digits();
    initialize_write_object();
    initialize_write_ugly_object();

    // initialize math routines - all are initialized in initialize_numbers
    initialize_numbers();

    ext::initialize_extension_functions();
#ifdef CLOS
    initialize_genericFunction();
#endif
    initialize_conditions();
    initialize_exceptions();

    coreExposerPtr->expose(_lisp, Exposer::candoClasses);
    //	    initializeCandoClos(_lisp);
  }
  {
    // Run some tests to make sure that lisp calling convention is ok.
    run_quick_tests();

    // setup the SYS logical-pathname-translations
    {
      Cons_sp pts = Cons_O::createList(
                                       Cons_O::createList(Str_O::create("sys:**;*.*"), bundle->getSysPathname())
        /* ,  more here */
                                       );
      af_pathnameTranslations(Str_O::create("sys"), _lisp->_true(), pts);
    }

    // setup the TMP logical-pathname-translations
    Cons_sp entryTmp = Cons_O::createList(Str_O::create("tmp:**;*.*"),
                                          cl_pathname(Str_O::create("/tmp/**/*.*")));
    Cons_sp ptsTmp = Cons_O::createList(entryTmp
                                        /* ,  more here */
                                        );
    af_pathnameTranslations(Str_O::create("tmp"), _lisp->_true(), ptsTmp);

    // setup the APP-CONTENTS logical-pathname-translations
    Cons_sp appc = Cons_O::createList(
        Cons_O::createList(Str_O::create("app-contents:**;*.*"), bundle->getAppContentsPathname())
        /* , more here */
        );
    af_pathnameTranslations(Str_O::create("app-contents"), _lisp->_true(), appc);

    // setup the APP-RESOURCES logical-pathname-translations
    Cons_sp app = Cons_O::createList(
        Cons_O::createList(Str_O::create("app-resources:**;*.*"), bundle->getAppContentsResourcesPathname())
        /* , more here */
        );
    af_pathnameTranslations(Str_O::create("app-resources"), _lisp->_true(), app);

    // setup the build;system pathnames
    {
      Cons_sp p = Cons_O::createList(
          Cons_O::createList(Str_O::create("**;*.*"), cl_pathname(Str_O::create("APP-RESOURCES:lisp;build;system;min-bitcode;**;*.*"))));
      af_pathnameTranslations(Str_O::create("min-bitcode"), _lisp->_true(), p);
    }
    {
      Cons_sp p = Cons_O::createList(
          Cons_O::createList(Str_O::create("**;*.*"), cl_pathname(Str_O::create("APP-RESOURCES:lisp;build;system;full-bitcode;**;*.*"))));
      af_pathnameTranslations(Str_O::create("full-bitcode"), _lisp->_true(), p);
    }
    {
      Cons_sp p = Cons_O::createList(
          Cons_O::createList(Str_O::create("**;*.*"), cl_pathname(Str_O::create("APP-RESOURCES:lisp;build;system;cclasp-bitcode;**;*.*"))));
      af_pathnameTranslations(Str_O::create("cclasp-bitcode"), _lisp->_true(), p);
    }
    {
      Cons_sp p = Cons_O::createList(
          Cons_O::createList(Str_O::create("**;*.*"), cl_pathname(Str_O::create("APP-RESOURCES:lisp;build;system;min-boehmdc;**;*.*"))));
      af_pathnameTranslations(Str_O::create("min-boehmdc"), _lisp->_true(), p);
    }
    {
      Cons_sp p = Cons_O::createList(
          Cons_O::createList(Str_O::create("**;*.*"), cl_pathname(Str_O::create("APP-RESOURCES:lisp;build;system;full-boehmdc;**;*.*"))));
      af_pathnameTranslations(Str_O::create("full-boehmdc"), _lisp->_true(), p);
    }
    {
      Cons_sp p = Cons_O::createList(
          Cons_O::createList(Str_O::create("**;*.*"), cl_pathname(Str_O::create("APP-RESOURCES:lisp;build;system;cclasp-boehmdc;**;*.*"))));
      af_pathnameTranslations(Str_O::create("cclasp-boehmdc"), _lisp->_true(), p);
    }
    {
      Cons_sp p = Cons_O::createList(
          Cons_O::createList(Str_O::create("**;*.*"), cl_pathname(Str_O::create("APP-RESOURCES:lisp;build;system;min-boehm;**;*.*"))));
      af_pathnameTranslations(Str_O::create("min-boehm"), _lisp->_true(), p);
    }
    {
      Cons_sp p = Cons_O::createList(
          Cons_O::createList(Str_O::create("**;*.*"), cl_pathname(Str_O::create("APP-RESOURCES:lisp;build;system;full-boehm;**;*.*"))));
      af_pathnameTranslations(Str_O::create("full-boehm"), _lisp->_true(), p);
    }
    {
      Cons_sp p = Cons_O::createList(
          Cons_O::createList(Str_O::create("**;*.*"), cl_pathname(Str_O::create("APP-RESOURCES:lisp;build;system;cclasp-boehm;**;*.*"))));
      af_pathnameTranslations(Str_O::create("cclasp-boehm"), _lisp->_true(), p);
    }
    {
      Cons_sp p = Cons_O::createList(
          Cons_O::createList(Str_O::create("**;*.*"), cl_pathname(Str_O::create("APP-RESOURCES:lisp;build;system;min-mps;**;*.*"))));
      af_pathnameTranslations(Str_O::create("min-mps"), _lisp->_true(), p);
    }
    {
      Cons_sp p = Cons_O::createList(
          Cons_O::createList(Str_O::create("**;*.*"), cl_pathname(Str_O::create("APP-RESOURCES:lisp;build;system;full-mps;**;*.*"))));
      af_pathnameTranslations(Str_O::create("full-mps"), _lisp->_true(), p);
    }
    {
      Cons_sp p = Cons_O::createList(
          Cons_O::createList(Str_O::create("**;*.*"), cl_pathname(Str_O::create("APP-RESOURCES:lisp;build;system;cclasp-mps;**;*.*"))));
      af_pathnameTranslations(Str_O::create("cclasp-mps"), _lisp->_true(), p);
    }
    /* Call the function defined in main.cc that creates the source-main: host */
    create_source_main_host();
  }
  //
  //
  //
  this->exposeCando();
  Lisp_O::initializeGlobals(_lisp);
  coreExposerPtr->expose(_lisp, Exposer::candoFunctions);
  coreExposerPtr->expose(_lisp, Exposer::candoGlobals);
  {
    _BLOCK_TRACE("Call global initialization callbacks");
    for (vector<InitializationCallback>::iterator ic = this->_GlobalInitializationCallbacks.begin();
         ic != this->_GlobalInitializationCallbacks.end(); ic++) {
      (*ic)(_lisp);
    }
  }

  Path_sp startupWorkingDir = Path_O::create(bundle->getStartupWorkingDir());
  this->defconstant(_sym_STARcurrent_working_directorySTAR, _Nil<Path_O>());
  this->setCurrentWorkingDirectory(startupWorkingDir);

  this->switchToClassNameHashTable();

  {
    _BLOCK_TRACE("Setup system values");
    FILE *null_out = fopen("/tmp/null", "w");
    this->_Roots._NullStream = IOStreamStream_O::makeIO("/tmp/null", null_out);
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
    this->_Roots._IntegerOverflowAdjust = cl_expt(two, bits); // clasp_make_fixnum(2),clasp_make_fixnum(gc::fixnum_bits));
    getcwd(true);                                             // set *default-pathname-defaults*
  };
  {
    _BLOCK_TRACE("Creating Caches for SingleDispatchGenericFunctions");
    this->_Roots._SingleDispatchMethodCachePtr = gctools::ClassAllocator<Cache>::allocateClass();
    this->_Roots._SingleDispatchMethodCachePtr->setup(2, SingleDispatchMethodCacheSize);
  }
  {
    _BLOCK_TRACE("Creating Caches for CLOS");
    this->_Roots._MethodCachePtr = gctools::ClassAllocator<Cache>::allocateClass();
    this->_Roots._MethodCachePtr->setup(MaxFunctionArguments, ClosCacheSize);
    this->_Roots._SlotCachePtr = gctools::ClassAllocator<Cache>::allocateClass();
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
  if (this->_Roots._BufferStringPool.nilp()) {
    StrWithFillPtr_sp one = StrWithFillPtr_O::create(' ', 256, 0, true);
    this->_Roots._BufferStringPool = Cons_O::create(one, _Nil<T_O>());
  }
  StrWithFillPtr_sp ret = gc::As<StrWithFillPtr_sp>(oCar(this->_Roots._BufferStringPool));
  this->_Roots._BufferStringPool = oCdr(this->_Roots._BufferStringPool);
  ret->setFillPointer(0);
  return ret;
}

/*! Return a buffer string to the BufferStringPool
*/
void Lisp_O::put_buffer_string(StrWithFillPtr_sp str) {
  this->_Roots._BufferStringPool = Cons_O::create(str, this->_Roots._BufferStringPool);
}

void Lisp_O::setCurrentWorkingDirectory(Path_sp dir) {
  _sym_STARcurrent_working_directorySTAR->setf_symbolValueReadOnlyOverRide(dir);
}

Path_sp Lisp_O::getCurrentWorkingDirectory() {
  return gc::As<Path_sp>(_sym_STARcurrent_working_directorySTAR->symbolValue());
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
  _G();
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
  _G();
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
    return _Nil<SpecialForm_O>();
  return this->_Roots._SpecialForms->gethash(sym);
}

#define ARGS_core_listOfAllSpecialOperators "()"
#define DECL_core_listOfAllSpecialOperators ""
#define DOCS_core_listOfAllSpecialOperators "listOfAllSpecialOperators"
T_sp core_listOfAllSpecialOperators() {
  List_sp sos(_Nil<T_O>());
  _lisp->_Roots._SpecialForms->maphash([&sos](T_sp key, T_sp val) {
      sos = Cons_O::create(key,sos);
  });
  return sos;
}

void Lisp_O::installPackage(const Exposer *pkg) {
  _OF();
  LOG(BF("Installing package[%s]") % pkg->packageName());
  int firstNewGlobalCallback = this->_GlobalInitializationCallbacks.end() - this->_GlobalInitializationCallbacks.begin();
  ChangePackage change(pkg->package());
  //    this->inPackage(pkg->packageName());
  {
    _BLOCK_TRACE("Initializing classes");
    pkg->expose(_lisp, Exposer::candoClasses);
  }
  {
    _BLOCK_TRACE("Creating nils for built-in classes");
    LOG(BF("Nils aren't created here anymore - they are created when the class is registered"));
    //	this->createNils();
  }
  {
    _BLOCK_TRACE("Initializing functions");
    pkg->expose(_lisp, Exposer::candoFunctions);
  }
  {
    _BLOCK_TRACE("Initializing globals");
    pkg->expose(_lisp, Exposer::candoGlobals);
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
  _G();
  Symbol_sp classSymbol = _lisp->intern(name, gc::As<Package_sp>(_lisp->findPackage(package, true)));
  classSymbol->exportYourself();
  classSymbol->setf_symbolValue(mc);
  //    this->globalEnvironment()->extend(classSymbol,mc);
  //    mc->__setLambdaListHandlerString(mc->getInstanceBaseClass()->__getLambdaListHandlerString());
}

/*! Add the class with (className) to the current package
 */
void Lisp_O::addClass(Symbol_sp classSymbol,
                      gctools::tagged_pointer<Creator> alloc,
                      Symbol_sp base1ClassSymbol,
                      Symbol_sp base2ClassSymbol,
                      Symbol_sp base3ClassSymbol) {
  _G();
  DEPRECIATED();
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
  af_setf_findClass(cc, classSymbol, true, _Nil<T_O>());
  if (IS_SYMBOL_DEFINED(base1ClassSymbol)) {
    cc->addInstanceBaseClass(base1ClassSymbol);
  } else {
    SIMPLE_ERROR(BF("There must be one base class"));
  }
  if (IS_SYMBOL_DEFINED(base2ClassSymbol)) {
    cc->addInstanceBaseClass(base2ClassSymbol);
  }
  if (IS_SYMBOL_DEFINED(base3ClassSymbol)) {
    cc->addInstanceBaseClass(base3ClassSymbol);
  }
  ASSERTF((bool)alloc, BF("_creator for %s is NULL!!!") % _rep_(classSymbol));
  cc->setCreator(alloc);
}

/*! Add the class with (className) to the current package
 */
void Lisp_O::addClass(Symbol_sp classSymbol, Class_sp theClass, gc::tagged_pointer<Creator> allocator) {
  _G();
  //	printf("%s:%d:%s  Adding class with symbol %s -- _allocator=%p unless we initialize it properly\n", __FILE__,__LINE__,__FUNCTION__,_rep_(classSymbol).c_str(), allocator );
  LOG(BF("Lisp_O::addClass classSymbol(%s)") % _rep_(classSymbol));
  //	printf("%s:%d --> Adding class[%s]\n", __FILE__, __LINE__, _rep_(classSymbol).c_str() );
  af_setf_findClass(theClass, classSymbol, true, _Nil<T_O>());
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
  _G();
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
  _G();
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
  _G();
  if (this->_BuiltInClassesInitialized)
    return;
  SIMPLE_ERROR(BF("Cpp-classes are not initialized"));
}

Path_sp Lisp_O::translateLogicalPathname(T_sp obj) {
  _G();
  if (Str_sp logicalPathName = obj.asOrNull<Str_O>()) {
    string fileName = logicalPathName->get();
    return Path_O::create(fileName);
    SIMPLE_ERROR(BF("include " + fileName + " error, file does not exist"));
  } else {
    SIMPLE_ERROR(BF("Finish implementing Lisp_O::translateLogicalPathname"));
  }
}

Path_sp Lisp_O::translateLogicalPathnameUsingPaths(T_sp obj) {
  _G();
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
  printf("%s:%d  USE-AMC-POOL is turned on\n", __FILE__, __LINE__);
  features = Cons_O::create(_lisp->internKeyword("USE-AMC-POOL"), features);
#endif
#endif

  cl::_sym_STARfeaturesSTAR->setf_symbolValue(features);

  SYMBOL_EXPORT_SC_(CorePkg, STARprintVersionOnStartupSTAR);
  _sym_STARprintVersionOnStartupSTAR->defparameter(_lisp->_boolean(options._Version));
  SYMBOL_EXPORT_SC_(CorePkg, STARsilentStartupSTAR);
  _sym_STARsilentStartupSTAR->defparameter(_lisp->_boolean(options._SilentStartup));

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
  _sym_STARcommandLineLoadEvalSequenceSTAR->defparameter(cl_nreverse(loadEvals));

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
    _sym_STARcommandLineImageSTAR->defparameter(cl_pathname(Str_O::create(options._ImageFile)));
  } else {
    _sym_STARcommandLineImageSTAR->defparameter(core_startupImagePathname());
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
      printf("%s:%d Trapping INTERN of symbol %s in package %s\n", __FILE__, __LINE__, this->_TrapInternPackage.c_str(), this->_TrapInternName.c_str());
    }
  }
  LOG(BF("lisp->_ScriptInFile(%d)  lisp->_FileNameOrCode(%s)") % this->_ScriptInFile % this->_FileNameOrCode);
}

T_mv Lisp_O::readEvalPrint(T_sp stream, T_sp environ, bool printResults, bool prompt) {
  T_mv result = Values(_Nil<T_O>());
  DynamicScopeManager scope(_sym_STARcurrentSourceFileInfoSTAR, core_sourceFileInfo(stream));
  while (1) {
    TRY() {
      if (prompt) {
        stringstream prompts;
        prompts << std::endl
                << gc::As<Package_sp>(cl::_sym_STARpackageSTAR->symbolValue())->getName() << "> ";
        clasp_write_string(prompts.str(), stream);
      }
      DynamicScopeManager innerScope(_sym_STARsourceDatabaseSTAR, SourceManager_O::create());
      innerScope.pushSpecialVariableAndSet(_sym_STARcurrentSourcePosInfoSTAR, core_inputStreamSourcePosInfo(stream));
      T_sp expression = cl_read(stream, _Nil<T_O>(), _Unbound<T_O>(), _Nil<T_O>());
      if (expression.unboundp())
        break;
      _sym_STARcurrentSourcePosInfoSTAR->setf_symbolValue(core_walkToFindSourcePosInfo(expression, _sym_STARcurrentSourcePosInfoSTAR->symbolValue()));
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
      if (af_keywordP(expression)) {
        ql::list tplCmd;
        tplCmd << expression;
        while (T_sp exp = cl_read(stream, _Nil<T_O>(), _Unbound<T_O>(), _Nil<T_O>())) {
          if (exp.unboundp())
            break;
          tplCmd << exp;
        }
        if (_sym_STARtopLevelCommandHookSTAR->symbolValue().notnilp()) {
          eval::funcall(_sym_STARtopLevelCommandHookSTAR->symbolValue(), tplCmd.cons());
        } else {
          af_bformat(_lisp->_true(), "Cannot interpret %s - define core::*top-level-command-hook*", Cons_O::createList(tplCmd.cons()));
        }
      } else if (expression.notnilp()) {
        result = eval::funcall(core::_sym_STAReval_with_env_hookSTAR->symbolValue(), expression, environ);
        gctools::Vec0<core::T_sp /*,gctools::RootedGCHolder*/> vresults;
        vresults.resize(result.number_of_values());
        if (result.number_of_values() > 0) {
          vresults[0] = result;
          if (result.number_of_values() > 1) {
            for (int i(1); i < result.number_of_values(); ++i) {
              vresults[i] = result.valueGet(i);
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
  cl_close(sin);
  return result;
}

#define ARGS_af_lowLevelRepl "()"
#define DECL_af_lowLevelRepl ""
#define DOCS_af_lowLevelRepl "lowLevelRepl - this is a built in repl for when the top-level repl isn't available"
void af_lowLevelRepl() {
  _G();
  _lisp->readEvalPrint(cl::_sym_STARterminal_ioSTAR->symbolValue(), _Nil<T_O>(), true, true);
};

void Lisp_O::readEvalPrintInteractive() {
  _OF();
  Cons_sp expression;
  //	TopLevelIHF topFrame(_lisp->invocationHistoryStack(),_Nil<T_O>());
  this->readEvalPrint(cl::_sym_STARterminal_ioSTAR->symbolValue(), _Nil<T_O>(), true, true);
}

#define ARGS_af_stackUsed "()"
#define DECL_af_stackUsed ""
#define DOCS_af_stackUsed "stackUsed"
uint af_stackUsed() {
  _G();
  int x;
  char *xaddr = (char *)(&x);
  uint stack = (uint)(_lisp->_StackTop - xaddr);
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
  _G();
  if (!global_invokedInternalDebugger) {
    printf("%s:%d Stack is getting full currently at %u bytes - warning at %u bytes\n",
           __FILE__, __LINE__,
           stackUsed, _lisp->_StackWarnSize);
    ExceptionSafeResetInvokedInternalDebugger safe;
    af_invokeInternalDebugger(_Nil<core::T_O>());
  }
};

#define ARGS_af_stackMonitor "()"
#define DECL_af_stackMonitor ""
#define DOCS_af_stackMonitor "monitor stack for problems - warn if getting too large"
void af_stackMonitor() {
  _G();
  uint stackUsed = af_stackUsed();
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

#define ARGS_af_stackLimit "()"
#define DECL_af_stackLimit ""
#define DOCS_af_stackLimit "Return the soft and hard limits of the stack"
T_mv af_stackLimit() {
  return Values(clasp_make_fixnum(_lisp->_StackWarnSize));
};

#define ARGS_af_setupStackMonitor "(&key warn-size sample-size)"
#define DECL_af_setupStackMonitor ""
#define DOCS_af_setupStackMonitor "setupStackMonitor"
void af_setupStackMonitor(T_sp warnSize, T_sp sampleSize) {
  _G();
  if (!warnSize.nilp()) {
    _lisp->_StackWarnSize = unbox_fixnum(gc::As<Fixnum_sp>(warnSize));
  }
  if (!sampleSize.nilp()) {
    _lisp->_StackSampleSize = unbox_fixnum(gc::As<Fixnum_sp>(sampleSize));
    _lisp->_StackSampleCount = 0;
    _lisp->_StackSampleMax = 0;
  }
};

#define ARGS_core_exit "(&optional (exit-value 0))"
#define DECL_core_exit ""
#define DOCS_core_exit "exit"
void core_exit(int exitValue) {
  _global_debuggerOnSIGABRT = false;
  throw(ExitProgram(exitValue));
};

#define ARGS_core_quit "(&optional (exit-value 0))"
#define DECL_core_quit ""
#define DOCS_core_quit "quit"
void core_quit(int exitValue) {
  _G();
  core_exit(exitValue);
};

#define ARGS_af_acons "(key datum alist)"
#define DECL_af_acons ""
#define DOCS_af_acons "acons"
List_sp af_acons(T_sp key, T_sp val, List_sp alist) {
  _G();
  Cons_sp acons = Cons_O::create(key, val);
  return Cons_O::create(acons, alist);
}

#define ARGS_cl_assoc "(item alist &key key test test-not)"
#define DECL_cl_assoc ""
#define DOCS_cl_assoc "assoc"
List_sp cl_assoc(T_sp item, List_sp alist, T_sp key, T_sp test, T_sp test_not) {
  _G();
  if (alist.nilp())
    return alist;
  return alist.asCons()->assoc(item, key, test, test_not);
}

#define ARGS_cl_member "(item list &key key test test-not)"
#define DECL_cl_member ""
#define DOCS_cl_member "See CLHS member"
List_sp cl_member(T_sp item, T_sp tlist, T_sp key, T_sp test, T_sp test_not) {
  _G();
  if (tlist.nilp())
    return _Nil<T_O>();
  if (Cons_sp list = tlist.asOrNull<Cons_O>()) {
    return (list->member(item, key, test, test_not));
  }
  QERROR_WRONG_TYPE_NTH_ARG(2, tlist, cl::_sym_list);
  UNREACHABLE();
}

#define ARGS_af_memberTest "(item list &key key test test-not)"
#define DECL_af_memberTest ""
#define DOCS_af_memberTest "See CLHS memberTest"
#define FILE_af_memberTest __FILE__
#define LINE_af_memberTest __LINE__
List_sp af_memberTest(T_sp item, List_sp list, T_sp key, T_sp test, T_sp test_not) {
  _G();
  if (list.nilp())
    return list;
  return (list.asCons()->member(item, key, test, test_not));
}

#define ARGS_af_member1 "(item list test test-not key)"
#define DECL_af_member1 ""
#define DOCS_af_member1 "Like member but if a key function is provided then apply it to the item. See ecl::list.d::member1"
#define FILE_af_member1 __FILE__
#define LINE_af_member1 __LINE__
List_sp af_member1(T_sp item, List_sp list, T_sp test, T_sp test_not, T_sp key) {
  _G();
  if (list.nilp())
    return list;
  return list.asCons()->member1(item, key, test, test_not);
}

#define ARGS_ext_setenv "(name value)"
#define DECL_ext_setenv ""
#define DOCS_ext_setenv "Set environment variable NAME to VALUE"
void ext_setenv(Str_sp name, Str_sp value) {
  _G();
  setenv(name->get().c_str(), value->get().c_str(), 1);
}

#define ARGS_ext_getenv "(name)"
#define DECL_ext_getenv ""
#define DOCS_ext_getenv "Get environment variable NAME"
T_sp ext_getenv(Str_sp name) {
  _G();
  string s = name->get();
  char *e = getenv(s.c_str());
  string value;
  if (e) {
    return Str_O::create(e);
  }
  return _Nil<T_O>();
}

/*
  __BEGIN_DOC( candoScript.general.getline, subsection, getline)
  \scriptCmdRet{getline}{}{String::result}

  Read a line from stdin
  __END_DOC
*/

#define ARGS_af_getline "(&optional (prompt \"\"))"
#define DECL_af_getline ""
#define DOCS_af_getline "getline"
T_mv af_getline(Str_sp prompt) {
  _G();
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

#define ARGS_ext_system "(cmd)"
#define DECL_ext_system ""
#define DOCS_ext_system "system"
T_mv ext_system(Str_sp cmd) {
  _G();
  string command = cmd->get();
  int ret = system(command.c_str());
  if (ret == 0) {
    return Values(core::make_fixnum(0));
  } else {
    return Values(core::make_fixnum(ret), Str_O::create(std::strerror(errno)));
  }
}

#define ARGS_ext_vfork_execvp "(call-and-arguments)"
#define DECL_ext_vfork_execvp ""
#define DOCS_ext_vfork_execvp "vfork_execvp"
T_mv ext_vfork_execvp(List_sp call_and_arguments) {
  if (call_and_arguments.nilp())
    return Values0<T_O>();
  std::vector<char const *> execvp_args(cl_length(call_and_arguments) + 1);
  size_t idx = 0;
  for (auto cur : call_and_arguments) {
    Str_sp sarg = gc::As<Str_sp>(oCar(cur));
    char *arg = (char *)malloc(sarg->size() + 1);
    std::strcpy(arg, sarg->c_str());
    execvp_args[idx++] = arg;
  }
  execvp_args[idx] = NULL;
  pid_t child_PID = vfork();
  if (child_PID >= 0) {
    if (child_PID == 0) {
      // Child
      execvp(execvp_args[0], (char *const *)execvp_args.data());
      printf("%s:%d execvp returned with errno=%d   strerror(errno) = %s\n", __FILE__, __LINE__, errno, strerror(errno));
      for (int i = 0; execvp_args[i] != NULL; ++i) {
        printf("    arg#%d  %s\n", i, execvp_args[i]);
      }
      printf("  cannot continue... exiting... sorry...\n");
      _exit(0); // Should never reach
    } else {
      // Parent
      int status;
      pid_t wait_ret = wait(&status);
      // Clean up args
      for (int i(0); i < execvp_args.size() - 1; ++i)
        free((void *)execvp_args[i]);
      if (wait_ret >= 0) {
        if (wait_ret != child_PID) {
          printf("%s:%d wait return PID(%d) that did not match child(%d)\n", __FILE__, __LINE__, wait_ret, child_PID);
        }
        return Values(_Nil<T_O>(), clasp_make_fixnum(child_PID));
      }
      // error
      return Values(clasp_make_fixnum(errno), Str_O::create(std::strerror(errno)));
    }
  } else {
    // Clean up args
    for (int i(0); i < execvp_args.size() - 1; ++i)
      free((void *)execvp_args[i]);
    return Values(clasp_make_fixnum(-1), Str_O::create(std::strerror(errno)));
  }
}

/*
  __BEGIN_DOC( candoScript.general.render, subsection, render)
  \scriptCmdRet{render}{ object}{renderedObject}

  Render an object into a graphical representation of the \scriptArg{object} that can be viewed using "candoView". Save the result into a file using the "save" command and view it later.
  __END_DOC
*/

#if defined(XML_ARCHIVE)
/*
  __BEGIN_DOC( candoScript.general.saveCando, subsection, saveCando)
  \scriptCmd{save}{Object::object Text::fileName}

  Save the \sa{object} to the \sa{fileName} in Cando-OML format.
  __END_DOC
*/
#define ARGS_af_saveCando "(obj pathDesignator)"
#define DECL_af_saveCando ""
#define DOCS_af_saveCando "saveCando"
T_mv af_saveCando(T_sp obj, T_sp pathDesignator) {
  _G();
  Path_sp path = coerce::pathDesignator(pathDesignator);
  T_sp sout = cl_open(path,
                      kw::_sym_output,
                      cl::_sym_standard_char,
                      _Nil<Symbol_O>(),
                      _Nil<Symbol_O>(),
                      kw::_sym_default);
  _lisp->sprint(obj, sout);
  sout->close();
  return (Values(_Nil<T_O>()));
}

#define ARGS_af_loadCando "(pathDesignator)"
#define DECL_af_loadCando ""
#define DOCS_af_loadCando "loadCando"
T_mv af_loadCando(T_sp pathDesignator) {
  _G();
  Path_sp path = coerce::pathDesignator(pathDesignator);
  T_sp sin = cl_open(path, kw::_sym_input, cl::_sym_standard_char, _Nil<Symbol_O>(), _Nil<Symbol_O>(), kw::_sym_default);
  T_sp obj = _lisp->sread(sin.as<Stream_O>(), true, _Nil<T_O>());
  sin->close();
  return (Values(obj));
}
#endif // defined(XML_ARCHIVE)

#define ARGS_cl_findClass "(symbol &optional (errorp t) environment)"
#define DECL_cl_findClass ""
#define DOCS_cl_findClass "findClass"
Class_mv cl_findClass(Symbol_sp symbol, bool errorp, T_sp env) {
  if (_lisp->bootClassTableIsValid()) {
    return Values(_lisp->boot_findClass(symbol, errorp));
  }
  ASSERTF(env.nilp(), BF("Handle non nil environment"));
  // Use the same global variable that ECL uses
  SYMBOL_EXPORT_SC_(CorePkg, STARclassNameHashTableSTAR);
  HashTable_sp classNames = gc::As<HashTable_sp>(_sym_STARclassNameHashTableSTAR->symbolValue());
  T_mv mc = classNames->gethash(symbol, _Nil<T_O>());
  T_sp cla = mc;
  bool foundp = mc.valueGet(1).notnilp();
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

#define ARGS_af_setf_findClass "(new-value name)"
#define DECL_af_setf_findClass ""
#define DOCS_af_setf_findClass "setf_findClass"
Class_mv af_setf_findClass(T_sp newValue, Symbol_sp name, bool errorp, T_sp env) {
  _G();
  if (!af_classp(newValue)) {
    SIMPLE_ERROR(BF("Classes in cando have to be subclasses of Class unlike ECL which uses Instances to represent classes - while trying to (setf find-class) of %s you gave: %s") % _rep_(name) % _rep_(newValue));
  }
  if (_lisp->bootClassTableIsValid()) {
    return Values(_lisp->boot_setf_findClass(name, gc::As<Class_sp>(newValue)));
  }
  HashTable_sp ht = gc::As<HashTable_sp>(_sym_STARclassNameHashTableSTAR->symbolValue());
  T_sp oldClass = eval::funcall(cl::_sym_findClass, name, _Nil<T_O>());
  if (af_classp(oldClass)) {
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

#define ARGS_af_findFileInLispPath "(partialPath)"
#define DECL_af_findFileInLispPath ""
#define DOCS_af_findFileInLispPath "findFileInLispPath"
T_mv af_findFileInLispPath(Str_sp partialPath) {
  _G();
  LOG(BF("PartialPath=[%s]") % partialPath->get());
  Path_sp fullPath = _lisp->translateLogicalPathnameUsingPaths(partialPath);
  LOG(BF("fullPath is %s") % fullPath->asString());
  return (Values(fullPath));
}

#define ARGS_af_find_package "(name_desig)"
#define DECL_af_find_package ""
#define DOCS_af_find_package "See CLHS: find-package"
T_sp af_find_package(T_sp name_desig) {
  _G();
  if (Package_sp pkg = name_desig.asOrNull<Package_O>())
    return pkg;
  Str_sp name = coerce::stringDesignator(name_desig);
  return _lisp->findPackage(name->get());
}

#define ARGS_af_selectPackage "(package-designator)"
#define DECL_af_selectPackage ""
#define DOCS_af_selectPackage "selectPackage"
void af_selectPackage(T_sp package_designator) {
  _G();
  Package_sp pkg = coerce::packageDesignator(package_designator);
  _lisp->selectPackage(pkg);
}

/*
  __BEGIN_DOC(candoScript.general.mpiEnabled,mpiEnabled)
  \scriptCmdRet{mpiEnabled}{}{}

  Return true if MPI is enabled.
  __END_DOC
*/

#define ARGS_af_mpi_enabled "()"
#define DECL_af_mpi_enabled ""
#define DOCS_af_mpi_enabled "mpi_enabled"
T_mv af_mpi_enabled() {
  _G();
  return (Values(_lisp->_boolean(_lisp->mpiEnabled())));
}

/*
  __BEGIN_DOC(candoScript.general.mpiRank,mpiRank)
  \scriptCmdRet{mpiRank}{}{}

  Return the mpi rank or 0 if not enabled.
  __END_DOC
*/
#define ARGS_af_mpi_rank "()"
#define DECL_af_mpi_rank ""
#define DOCS_af_mpi_rank "Return the mpi_rank or 0 if mpi is disabled"
T_mv af_mpi_rank() {
  _G();
  return (Values(make_fixnum(_lisp->mpiRank())));
}

/*
  __BEGIN_DOC(candoScript.general.mpiSize,mpiSize)
  \scriptCmdRet{mpiSize}{}{}

  Return the mpi rank or 0 if not enabled.
  __END_DOC
*/

#define ARGS_af_mpi_size "()"
#define DECL_af_mpi_size ""
#define DOCS_af_mpi_size "Return mpi_size or 0 if mpi is not enabled"
T_mv af_mpi_size() {
  _G();
  return (Values(make_fixnum(_lisp->mpiSize())));
}

#define ARGS_cl_macroexpand_1 "(form &optional env)"
#define DECL_cl_macroexpand_1 ""
#define DOCS_cl_macroexpand_1 "macroexpand_1"
T_mv cl_macroexpand_1(T_sp form, T_sp env) {
  _G();
  T_sp expansionFunction = _Nil<T_O>();
  if (form.nilp()) {
    return form;
  } else if (Cons_sp cform = form.asOrNull<Cons_O>()) {
    T_sp head = oCar(cform);
    if (cl_symbolp(head)) {
      Symbol_sp headSymbol = gc::As<Symbol_sp>(head);
      if (env.nilp()) {
        expansionFunction = eval::funcall(cl::_sym_macroFunction, headSymbol, env);
      } else if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
        expansionFunction = eval::funcall(cl::_sym_macroFunction, headSymbol, env);
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
      expansionFunction = core_lookup_symbol_macro(sform, env);
    } else if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
      expansionFunction = core_lookup_symbol_macro(sform, eenv);
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

#define ARGS_cl_macroexpand "(form &optional env)"
#define DECL_cl_macroexpand ""
#define DOCS_cl_macroexpand "macroexpand"
T_mv cl_macroexpand(T_sp form, T_sp env) {
  _G();
  bool sawAMacro = false;
  bool expandedMacro = false;
  uint macroExpansionCount = 0;
  if (_sym_STARdebugMacroexpandSTAR->symbolValue().isTrue()) {
    printf("%s:%d - macroexpanding --> %s\n", __FILE__, 2551, _rep_(form).c_str());
  }
  T_sp cur = form;
  do {
    T_mv mv = cl_macroexpand_1(cur, env);
    cur = mv;
    sawAMacro = gc::As<T_sp>(mv.valueGet(1)).isTrue();
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
  _G();
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
                        ss << af_classOf(af_symbolFunction((sym)))->classNameAsString();
			Function_sp fn = af_symbolFunction(sym);
                        if ( !fn.unboundp() && fn->closure && gc::As<Function_sp>(af_symbolFunction(sym))->closure->macroP() )
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

#define ARGS_af_apropos "(string_desig &optional package_desig)"
#define DECL_af_apropos ""
#define DOCS_af_apropos "apropos"
T_sp af_apropos(Str_sp string_desig, T_sp package_desig) {
  _G();
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
    return x->operator<(y);
  }
};

#define ARGS_af_sorted "(unsorted)"
#define DECL_af_sorted ""
#define DOCS_af_sorted "Sort the list in ascending order using operator< and return the sorted list"
List_sp af_sorted(List_sp unsorted) {
  _G();
  gctools::Vec0<T_sp /*,gctools::RootedGCHolder*/> sorted;
  if (cl_length(unsorted) == 0)
    return _Nil<T_O>();
  fillVec0FromCons(sorted, unsorted);
  OrderByLessThan orderer;
  sort::quickSort(sorted.begin(), sorted.end(), orderer);
  return asCons(sorted);
}

class OrderBySortFunction {
private:
  Function_sp _SortFunction;
  Cons_sp _args;

public:
  OrderBySortFunction(Function_sp proc) {
    this->_SortFunction = proc;
    this->_args = Cons_O::createList(_Nil<T_O>(), _Nil<T_O>());
  }
  bool operator()(T_sp x, T_sp y) {
    return T_sp(eval::funcall(this->_SortFunction, x, y)).isTrue();
  }
};

#define ARGS_af_sort "(sequence predicate)"
#define DECL_af_sort ""
#define DOCS_af_sort "Like CLHS: sort but does not support key"
T_sp af_sort(List_sp sequence, T_sp predicate) {
  _G();
  gctools::Vec0<T_sp> sorted;
  Function_sp sortProc = coerce::functionDesignator(predicate);
  LOG(BF("Unsorted data: %s") % _rep_(sequence));
  if (cl_length(sequence) == 0)
    return _Nil<T_O>();
  fillVec0FromCons(sorted, sequence);
  LOG(BF("Sort function: %s") % _rep_(sortProc));
  OrderBySortFunction orderer(sortProc);
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

#define ARGS_af_sourceFileName "()"
#define DECL_af_sourceFileName ""
#define DOCS_af_sourceFileName "Return the current sourceFileName"
T_mv af_sourceFileName() {
  _G();
  Cons_sp ppcons;
  InvocationHistoryFrame *frame = _lisp->invocationHistoryStack().top();
  gctools::tagged_pointer<Closure> closure = frame->closure;
  int sourceFileInfoHandle = closure->sourceFileInfoHandle();
  string sourcePath = gc::As<SourceFileInfo_sp>(core_sourceFileInfo(make_fixnum(sourceFileInfoHandle)))->namestring();
  Path_sp path = Path_O::create(sourcePath);
  Path_sp parent_path = path->parent_path();
  return Values(Str_O::create(path->fileName()), Str_O::create(parent_path->asString()));
}

#define ARGS_af_sourceLineColumn "()"
#define DECL_af_sourceLineColumn ""
#define DOCS_af_sourceLineColumn "sourceLineColumn"
T_mv af_sourceLineColumn() {
  InvocationHistoryFrame *frame = _lisp->invocationHistoryStack().top();
  gctools::tagged_pointer<Closure> closure = frame->closure;
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

#define ARGS_af_script_dir "()"
#define DECL_af_script_dir ""
#define DOCS_af_script_dir "script_dir"
Path_mv af_script_dir() {
  _G();
  Path_sp dir = Path_O::create(_lisp->bundle().getLispDir());
  return (Values(dir));
}

#define ARGS_af_libraryPath "(&optional rel-path)"
#define DECL_af_libraryPath ""
#define DOCS_af_libraryPath "Returns the absolute path to the library directory - if rel-path is not nil then it prepends the library directory path to rel-path and returns that"
Path_mv af_libraryPath(T_sp relPathDesignator) {
  _G();
  if (relPathDesignator.notnilp()) {
    Path_sp relPath = coerce::pathDesignator(relPathDesignator);
    boost_filesystem::path lp = _lisp->bundle().getLibDir();
    lp /= relPath->getPath();
    return (Values(Path_O::create(lp)));
  }
  return (Values(Path_O::create(_lisp->bundle().getLibDir())));
}

#define ARGS_af_lispCodePath "(&optional rel-path)"
#define DECL_af_lispCodePath ""
#define DOCS_af_lispCodePath "Returns the absolute path to the lisp code directory - if rel-path is not nil then it prepends the directory path to rel-path and returns that"
Path_mv af_lispCodePath(T_sp relPathDesignator) {
  _G();
  if (relPathDesignator.notnilp()) {
    Path_sp relPath = coerce::pathDesignator(relPathDesignator);
    boost_filesystem::path lp = _lisp->bundle().getLibDir();
    lp /= relPath->getPath();
    return (Values(Path_O::create(lp)));
  }
  return (Values(Path_O::create(_lisp->bundle().getLibDir())));
}

/*
  __BEGIN_DOC(candoScript.general.databaseDir,databaseDir)
  \scriptCmdRet{databaseDir}{}{Text::}

  Return the path for the database directory.
  __END_DOC
*/

#define ARGS_af_database_dir "()"
#define DECL_af_database_dir ""
#define DOCS_af_database_dir "database_dir"
Path_mv af_database_dir() {
  _G();
  Path_sp dir = Path_O::create(_lisp->bundle().getDatabasesDir());
  return (Values(dir));
}

/*
  __BEGIN_DOC(candoScript.general.changeWorkingDirectory,changeWorkingDirectory)
  \scriptCmdRet{changeWorkingDirectory}{}{Text::}

  Change the current working directory.
  __END_DOC
*/

#define ARGS_af_setCurrentWorkingDirectory "(dir)"
#define DECL_af_setCurrentWorkingDirectory ""
#define DOCS_af_setCurrentWorkingDirectory "setCurrentWorkingDirectory"
T_mv af_setCurrentWorkingDirectory(Str_sp dir) {
  _G();
  Path_sp cwd = Path_O::create(dir->get());
  _lisp->setCurrentWorkingDirectory(cwd);
  return (Values(dir));
}

/*
  __BEGIN_DOC(candoScript.general.isTopLevelScript)

  Return a true if this is a top level script or false if its an include file.
  __END_DOC
*/

#define ARGS_af_isTopLevelScript "()"
#define DECL_af_isTopLevelScript ""
#define DOCS_af_isTopLevelScript "isTopLevelScript"
T_mv af_isTopLevelScript() {
  _G();
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

#define ARGS_af_debugLogOn "()"
#define DECL_af_debugLogOn ""
#define DOCS_af_debugLogOn "debugLogOn"
void af_debugLogOn() {
  _G();
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

#define ARGS_af_debugLogOff "()"
#define DECL_af_debugLogOff ""
#define DOCS_af_debugLogOff "debugLogOff"
void af_debugLogOff() {
  _G();
  _lisp->debugLog().setSuppressMessages(true);
}

/*
  __BEGIN_DOC(candoScript.general.export,export)
  \scriptCmd{export}{symbols...}

  Tell the symbols that they can be exported.

  __END_DOC
*/
#define ARGS_cl_export "(symDes &optional (packageDes *package*))"
#define DECL_cl_export ""
#define DOCS_cl_export "CLHS: export"
T_sp cl_export(T_sp symDes, T_sp packageDes) {
  _G();
  List_sp symbols = coerce::listOfSymbols(symDes);
  Package_sp package = coerce::packageDesignator(packageDes);
  for (auto sym : symbols) {
    package->_export2(gc::As<Symbol_sp>(oCar(sym)));
  }
  return _lisp->_true();
}

#define ARGS_af_exportToPython "(symbolsDesig)"
#define DECL_af_exportToPython ""
#define DOCS_af_exportToPython "exportToPython"
void af_exportToPython(T_sp symbolsDesig) {
  _G();
  List_sp symbols = coerce::listOfSymbols(symbolsDesig);
  for (Cons_sp cur : symbols) {
    Symbol_sp one = gc::As<Symbol_sp>(oCar(cur));
    LOG(BF("Exporting symbol[%s] to python") % _rep_(one));
    _lisp->exportToPython(one);
  }
}

#define ARGS_cl_intern "(symbol_name &optional (package-desig *package*))"
#define DECL_cl_intern ""
#define DOCS_cl_intern "See CLHS: intern"
T_mv cl_intern(Str_sp symbol_name, T_sp package_desig) {
  _G();
  Package_sp package = coerce::packageDesignator(package_desig);
  return (package->intern(symbol_name->get()));
}

#define ARGS_af_universalErrorHandler "(continue-string datum initializers)"
#define DECL_af_universalErrorHandler ""
#define DOCS_af_universalErrorHandler "universalErrorHandler"
T_mv af_universalErrorHandler(T_sp continueString, T_sp datum, List_sp initializers) {
  _G();
  if (af_stringP(datum)) {
    af_format(_lisp->_true(), datum, initializers);
  } else {
    stringstream ss;
    ss << "datum: " << _rep_(datum) << " " << _rep_(initializers);
    printf("%s\n", ss.str().c_str());
  }
  dbg_hook("universalErrorHandler");
  af_invokeInternalDebugger(_Nil<T_O>());
  abort();
};

#define ARGS_af_invokeInternalDebugger "(&optional condition)"
#define DECL_af_invokeInternalDebugger ""
#define DOCS_af_invokeInternalDebugger "invokeInternalDebugger"
void af_invokeInternalDebugger(T_sp condition) {
  _G();
  stringstream ss;
  if (condition.nilp()) {
    LispDebugger debugger;
    debugger.invoke();
  } else {
    _lisp->print(BF("%s:%d af_invokeInternalDebugger --> %s") % __FILE__ % __LINE__ % _rep_(condition).c_str());
    LispDebugger debugger(condition);
    debugger.invoke();
  }
};

#define ARGS_core_singleDispatchGenericFunctionTable "()"
#define DECL_core_singleDispatchGenericFunctionTable ""
#define DOCS_core_singleDispatchGenericFunctionTable "singleDispatchGenericFunctionTable"
HashTable_sp core_singleDispatchGenericFunctionTable() {
  _G();
  return _lisp->singleDispatchGenericFunctionTable();
};

extern "C" {

#define ARGS_af_invokeInternalDebuggerFromGdb "()"
#define DECL_af_invokeInternalDebuggerFromGdb ""
#define DOCS_af_invokeInternalDebuggerFromGdb "invokeInternalDebuggerFromGdb"
void af_invokeInternalDebuggerFromGdb() {
  _G();
  eval::funcall(_sym_invokeInternalDebugger);
  SIMPLE_ERROR(BF("This should never happen"));
};
};

#define ARGS_cl_error "(datum &rest arguments)"
#define DECL_cl_error ""
#define DOCS_cl_error "See CLHS error"
void cl_error(T_sp datum, List_sp initializers) {
  _G();
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
    core_lowLevelBacktrace();
  }
  ++nestedErrorDepth;
  DynamicScopeManager scope(_sym_STARnestedErrorDepthSTAR, make_fixnum(nestedErrorDepth));
  if (_sym_universalErrorHandler->fboundp()) {
    Function_sp fn = _sym_universalErrorHandler->symbolFunction();
    eval::funcall(fn, _Nil<T_O>(), datum, initializers);
  }
  THROW_HARD_ERROR(BF("cl_error should never return because universal-error-handler should never return - but it did"));
}

#define ARGS_cl_cerror "(cformat eformat &rest arguments)"
#define DECL_cl_cerror ""
#define DOCS_cl_cerror "See CLHS cerror"
void cl_cerror(T_sp cformat, T_sp eformat, List_sp arguments) {
  _G();
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

#define ARGS_af_isAssignableTo "(tag secondArgument)"
#define DECL_af_isAssignableTo ""
#define DOCS_af_isAssignableTo "isAssignableTo"
T_mv af_isAssignableTo(T_sp tag, Class_sp mc) {
  _G();
  LOG(BF("Checking if instances of class(%s) is assignable to variables of class(%s)") % af_classOf(tag)->className() % af_classOf(mc)->className());
  bool io = (tag->isAssignableToByClassSymbol(mc->name()));
  return (Values(_lisp->_boolean(io)));
}

/*
  __BEGIN_DOC(candoScript.general.isSubClassOf,isSubClassOf)
  \scriptInfixRet{Object::object}{isSubClassOf}{Class::classObject}{Bool::}

  Return true if \scriptArg{object} can be assigned to a C++ variable of class \scriptArg{classObject}.
  __END_DOC
*/

#define ARGS_af_isSubClassOf "(tag mc)"
#define DECL_af_isSubClassOf ""
#define DOCS_af_isSubClassOf "isSubClassOf"
T_mv af_isSubClassOf(Class_sp tag, Class_sp mc) {
  _G();
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

#define ARGS_af_repr "(arg)"
#define DECL_af_repr ""
#define DOCS_af_repr "Return a string representation of the object"
T_mv af_repr(T_sp obj) {
  _G();
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

#define ARGS_af_not "(arg)"
#define DECL_af_not ""
#define DOCS_af_not "not"
T_mv af_not(T_sp x) {
  _G();
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
  _G();
  if (af_classp(mc)) {
    LOG(BF("createObjectOfClass(%s)") % _rep_(mc));
    IMPLEMENT_ME();
    T_sp obj = gc::As<Class_sp>(mc)->allocate_newNil();
    obj->initialize();
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
  _G();
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
  _G();
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

#define ARGS_Lisp_O_find_single_dispatch_generic_function "(gf-symbol &optional errorp)"
#define DECL_Lisp_O_find_single_dispatch_generic_function ""
#define DOCS_Lisp_O_find_single_dispatch_generic_function "Lookup a single dispatch generic function. If errorp is truen and the generic function isn't found throw an exception"
SingleDispatchGenericFunction_sp Lisp_O::find_single_dispatch_generic_function(Symbol_sp gfSym, bool errorp) {
  _G();
  T_sp fn = _lisp->_Roots._SingleDispatchGenericFunctionTable->gethash(gfSym, _Nil<T_O>());
  if (fn.nilp()) {
    if (errorp) {
      SIMPLE_ERROR(BF("No single-dispatch-generic-function named %s") % _rep_(gfSym));
    }
    return _Nil<SingleDispatchGenericFunction_O>();
  }
  return gc::As<SingleDispatchGenericFunction_sp>(fn);
}

#define ARGS_Lisp_O_setf_find_single_dispatch_generic_function "(gf-symbol gf)"
#define DECL_Lisp_O_setf_find_single_dispatch_generic_function ""
#define DOCS_Lisp_O_setf_find_single_dispatch_generic_function "Define a single dispatch generic function "
SingleDispatchGenericFunction_sp Lisp_O::setf_find_single_dispatch_generic_function(Symbol_sp gfName, SingleDispatchGenericFunction_sp gf) {
  _G();
  _lisp->_Roots._SingleDispatchGenericFunctionTable->setf_gethash(gfName, gf);
  return gf;
}

#define ARGS_Lisp_O_forget_all_single_dispatch_generic_functions "()"
#define DECL_Lisp_O_forget_all_single_dispatch_generic_functions ""
#define DOCS_Lisp_O_forget_all_single_dispatch_generic_functions "Forget all single dispatch functions"
void Lisp_O::forget_all_single_dispatch_generic_functions() {
  _G();
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
    {_G();
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
#if 0
	Symbol_sp sym = this->findSymbol(name);
	if ( sym.nilp() )
	{
	    SIMPLE_ERROR(BF("Could not find class with name: %s") % name );
	}
	return _lisp->findClass(sym);
#endif
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
  _G();
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
  T_mv symStatus = package->intern(symbolName);
  Symbol_sp sym = gc::As<Symbol_sp>(symStatus);
  T_sp status = symStatus.second();
  return Values(sym, status);
}

/*! The optionalPackageDesignator is nil */
Symbol_sp Lisp_O::intern(string const &symbolName) {
  _G();
  Package_sp curPackage = this->getCurrentPackage();
  ASSERTNOTNULL(curPackage);
  return this->intern(symbolName, curPackage);
}

Symbol_sp Lisp_O::findSymbol(const string &name, T_sp optionalPackageDesignator) const {
  _G();
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
  _G();
  return this->findSymbol(symbolName, _Nil<T_O>());
}

Symbol_sp Lisp_O::intern(string const &symbolName, string const &packageName) {
  _G();
  T_sp package = this->findPackage(packageName);
  return this->intern(symbolName, package);
}

Symbol_sp Lisp_O::internUniqueWithPackageName(string const &packageName, string const &symbolName) {
  _G();
  T_sp package = this->findPackage(packageName);
  Symbol_mv symStatus = this->intern(symbolName, package);
  //  T_sp status = symStatus.second();
  return symStatus;
}

Symbol_sp Lisp_O::internWithPackageName(string const &packageName, string const &symbolName) {
  _G();
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
  _G();
  string realName = name;
  if (name[0] == ':') {
    realName = name.substr(1, 99999);
  }
  size_t colonPos = realName.find_first_of(":");
  if (colonPos != string::npos) {
    SIMPLE_ERROR(BF("You cannot intern[%s] as a keyword - it has package designating ':' characters in it at pos[%d]") % realName % colonPos);
  }
  boost::to_upper(realName);
  return gc::As<Symbol_sp>(this->_Roots._KeywordPackage->intern(realName));
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

InvocationHistoryStack &Lisp_O::invocationHistoryStack() {
  return this->_Roots._InvocationHistoryStack;
}

void Lisp_O::dump_backtrace(int numcol) {
  _OF();
  string bt = this->invocationHistoryStack().asString();
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

#if 0
    string Lisp_O::backTraceAsString(int numcol) const
    {
	stringstream strace;
	Cons_sp btReversed = this->getBackTrace();
	List_sp bt = btReversed->reverse();
	strace << "Cando-backtrace number of entries: " << cl_length(bt) <<std::endl;
	while ( bt.notnilp() )
	{
	    T_sp code = bt->ocar();
	    stringstream sline;
	    if ( code->consP() )
	    {
		List_sp entry = code;
		if ( entry->hasParsePos() )
		{
		    sline << core_sourceFileInfo(entry)->permanentFileName() << ":" << af_lineno(entry) << " " << entry->__repr__();
		} else
		{
		    sline << "no-function: " << entry->__repr__();
		}
	    } else
	    {
		sline << "no-function: " << code->__repr__();
	    }
	    strace << sline.str().substr(0,numcol) << std::endl;
	    bt = bt->cdr();
	}
	strace << "----- backtrace done ------" << std::endl;
	return strace.str();
    }
#endif

void Lisp_O::run() {
  _G();

  // If the user adds "-f debug-startup" to the command line
  // then set core::*debug-startup* to true
  // This will print timings of top-level forms as they load at startup
  // See llvmo::intrinsics.cc
  // cl_member isn't available yet so check for the feature by hand.
  for (auto cur : (List_sp)cl::_sym_STARfeaturesSTAR->symbolValue()) {
    if (oCar(cur) == kw::_sym_debugStartup) {
      printf("%s:%d Setting core:*debug-startup* to T\n", __FILE__, __LINE__);
      core::_sym_STARdebugStartupSTAR->setf_symbolValue(_lisp->_true());
    }
  }
  //	printf("%s:%d core:*debug-startup* is: %s\n", __FILE__, __LINE__, _rep_(core::_sym_STARdebugStartupSTAR->symbolValue()).c_str());
  if (!this->_IgnoreInitImage) {
    Pathname_sp initPathname = gc::As<Pathname_sp>(_sym_STARcommandLineImageSTAR->symbolValue());
    DynamicScopeManager scope(core::_sym_STARuseInterpreterForEvalSTAR, _lisp->_true());
    T_mv result = eval::funcall(cl::_sym_load, initPathname); // core_loadBundle(initPathname);
    if (result.nilp()) {
      T_sp err = result.second();
      printf("Could not load bundle %s error: %s\n", _rep_(initPathname).c_str(), _rep_(err).c_str());
    }
  } else if (!this->_IgnoreInitLsp) {
    // Assume that if there is no program then
    // we want an interactive script
    //
    {
      _BLOCK_TRACEF(BF("Evaluating initialization code in(%s)") % this->_RCFileName);
      Pathname_sp initPathname = cl_pathname(Str_O::create(this->_RCFileName));
      T_mv result = eval::funcall(cl::_sym_load, initPathname);
      if (result.nilp()) {
        T_sp err = result.second();
        printf("Could not load %s error: %s\n", _rep_(initPathname).c_str(), _rep_(err).c_str());
      }
    }
  } else {
    {
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
    LOG(BF("Leaving lisp run"));
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

#define ARGS_core_allSourceFiles "()"
#define DECL_core_allSourceFiles ""
#define DOCS_core_allSourceFiles "List all of the source files"
List_sp core_allSourceFiles() {
  List_sp list = _Nil<T_O>();
  for (auto it : _lisp->_SourceFileIndices) {
    Str_sp sf = Str_O::create(it.first);
    list = Cons_O::create(sf, list);
  };
  return list;
}

void Lisp_O::mapClassNamesAndClasses(KeyValueMapper *mapper) {
  _G();
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
  _G();
  stringstream ss;
  ss << "Lisp_O object";
  return ss.str();
};

void Lisp_O::initializeGlobals(Lisp_sp lisp) {
  _G();
  LOG(BF("Lisp_O::initializeGlobals"));
  SYMBOL_EXPORT_SC_(CorePkg, selectPackage);
  Defun(selectPackage);
  CoreDefun(allSourceFiles);
}

void Lisp_O::exposeCando() {
  _G();
  //	printf("%s:%d in core::Lisp_O::exposeCando\n", __FILE__, __LINE__ );
  SYMBOL_SC_(CorePkg, find_single_dispatch_generic_function);
  af_def(CorePkg, "find-single-dispatch-generic-function",
         &Lisp_O::find_single_dispatch_generic_function,
         ARGS_Lisp_O_find_single_dispatch_generic_function,
         DECL_Lisp_O_find_single_dispatch_generic_function,
         DOCS_Lisp_O_find_single_dispatch_generic_function);
  SYMBOL_SC_(CorePkg, setf_find_single_dispatch_generic_function);
  af_def(CorePkg, "setf-find-single-dispatch-generic-function",
         &Lisp_O::setf_find_single_dispatch_generic_function,
         ARGS_Lisp_O_setf_find_single_dispatch_generic_function,
         DECL_Lisp_O_setf_find_single_dispatch_generic_function,
         DOCS_Lisp_O_setf_find_single_dispatch_generic_function);
  SYMBOL_SC_(CorePkg, forget_all_single_dispatch_generic_functions);
  af_def(CorePkg, "forget-all-single-dispatch-generic-functions",
         &Lisp_O::forget_all_single_dispatch_generic_functions,
         ARGS_Lisp_O_forget_all_single_dispatch_generic_functions,
         DECL_Lisp_O_forget_all_single_dispatch_generic_functions,
         DOCS_Lisp_O_forget_all_single_dispatch_generic_functions);

  CoreDefun(listOfAllSpecialOperators);
  CoreDefun(singleDispatchGenericFunctionTable);
  SYMBOL_SC_(CorePkg, stackMonitor);
  Defun(stackMonitor);
  Defun(stackLimit);
  Defun(lowLevelRepl);
  SYMBOL_SC_(CorePkg, setupStackMonitor);
  Defun(setupStackMonitor);

  SYMBOL_SC_(CorePkg, invokeInternalDebugger);
  Defun(invokeInternalDebugger);
  SYMBOL_SC_(CorePkg, invokeInternalDebuggerFromGdb);
  Defun(invokeInternalDebuggerFromGdb);
  SYMBOL_SC_(CorePkg, universalErrorHandler);
  Defun(universalErrorHandler);
  SYMBOL_SC_(CorePkg, stackUsed);
  Defun(stackUsed);

  SYMBOL_SC_(CorePkg, exit);
  CoreDefun(exit);
  SYMBOL_SC_(CorePkg, quit);
  CoreDefun(quit);
#if defined(XML_ARCHIVE)
  SYMBOL_SC_(CorePkg, serialize_xml);
  Defun(serialize_xml);
  SYMBOL_SC_(CorePkg, deserialize_xml);
  Defun(deserialize_xml);
  SYMBOL_SC_(CorePkg, render);
  Defun(render);
  SYMBOL_SC_(CorePkg, saveCando);
  Defun(saveCando);
  SYMBOL_SC_(CorePkg, loadCando);
  Defun(loadCando);
#endif // defined(XML_ARCHIVE)
  SYMBOL_SC_(CorePkg, getline);
  Defun(getline);

  SYMBOL_SC_(ExtPkg, system);
  ExtDefun(system);
  ExtDefun(vfork_execvp);

  SYMBOL_EXPORT_SC_(ClPkg, apropos);
  Defun(apropos);

  af_def(CorePkg, "brcl-apropos", &af_apropos, ARGS_af_apropos, DECL_af_apropos, DOCS_af_apropos);

  SYMBOL_EXPORT_SC_(ClPkg, export);
  ClDefun(export);
  SYMBOL_EXPORT_SC_(ClPkg, intern);
  ClDefun(intern);
  //	defNoWrapPackage(CorePkg,"apply", &prim_apply,_LISP);
  SYMBOL_SC_(CorePkg, isTopLevelScript);
  Defun(isTopLevelScript);

  //	defNoWrapPackage(CorePkg,"allGlobalNames", &prim_allGlobalNames ,_LISP);
  //	defNoWrapPackage(CorePkg,"locals", &prim_locals,_LISP);
  SYMBOL_SC_(CorePkg, sourceFileName);
  Defun(sourceFileName);
  SYMBOL_SC_(CorePkg, sourceLineColumn);
  Defun(sourceLineColumn);
  //	SYMBOL_SC_(CorePkg,backtrace);
  //	Defun(backtrace);
  //	defNoWrapPackage(CorePkg,"globals", &prim_globals,_LISP);
  SYMBOL_SC_(CorePkg, findFileInLispPath);
  Defun(findFileInLispPath);

  SYMBOL_EXPORT_SC_(ClPkg, findClass);
  ClDefun(findClass);
  SYMBOL_SC_(CorePkg, setf_findClass);
  Defun(setf_findClass);

  //	defNoWrapPackage(CorePkg,"print", &prim_print ,_LISP);

  SYMBOL_SC_(CorePkg, isAssignableTo);
  Defun(isAssignableTo);
  SYMBOL_SC_(CorePkg, isSubClassOf);
  Defun(isSubClassOf);

  //	defNoWrapPackage(CorePkg,"derive", &prim_derive ,_LISP);
  //	defNoWrapPackage(CorePkg,"isA", &prim_isA ,_LISP);

  //	defNoWrapPackage(CorePkg,"parseConsOfStrings", &prim_parseConsOfStrings ,_LISP);

  //	defNoWrapPackage(CorePkg,"sub", &prim_sub ,_LISP);
  //	defNoWrapPackage(CorePkg,"-", &prim_sub ,_LISP);
  //	defNoWrapPackage(CorePkg,"div", &prim_div ,_LISP);
  //    defNoWrapPackage(CorePkg,"mod", &prim_mod ,_LISP);
  //	defNoWrapPackage(CorePkg,"/", &prim_div ,_LISP);
  //	defNoWrapPackage(CorePkg,"mul", &prim_mul ,_LISP);
  //	defNoWrapPackage(CorePkg,"*", &prim_mul ,_LISP);
  //	defNoWrapPackage(CorePkg,"className", &prim_className ,_LISP);

  SYMBOL_SC_(CorePkg, repr);
  Defun(repr);

  SYMBOL_EXPORT_SC_(ClPkg, error);
  ClDefun(error);
  SYMBOL_EXPORT_SC_(ClPkg, cerror);
  ClDefun(cerror);
  SYMBOL_EXPORT_SC_(ExtPkg, setenv);
  ExtDefun(setenv);
  SYMBOL_EXPORT_SC_(ExtPkg, getenv);
  ExtDefun(getenv);
  SYMBOL_EXPORT_SC_(ClPkg, not);
  Defun(not);

  SYMBOL_SC_(CorePkg, debugLogOn);
  Defun(debugLogOn);
  SYMBOL_SC_(CorePkg, debugLogOff);
  Defun(debugLogOff);

  // mpi commands that are always built in
  SYMBOL_SC_(CorePkg, mpi_enabled);
  Defun(mpi_enabled);
  SYMBOL_SC_(CorePkg, mpi_rank);
  Defun(mpi_rank);
  SYMBOL_SC_(CorePkg, mpi_size);
  Defun(mpi_size);
  // Basic tests
  //	defNoWrapPackage(CorePkg,"consp",&prim_consp,_LISP);
  //	defNoWrapPackage(CorePkg,"symbolp",&prim_symbolp,_LISP);

  // aliases for "list" command

  SYMBOL_SC_(CorePkg, sorted);
  Defun(sorted);
  SYMBOL_EXPORT_SC_(ClPkg, sort);
  Defun(sort);
  SYMBOL_EXPORT_SC_(ClPkg, macroexpand_1);
  ClDefun(macroexpand_1);
  SYMBOL_EXPORT_SC_(ClPkg, macroexpand);
  ClDefun(macroexpand);

  // information functions
  SYMBOL_SC_(CorePkg, database_dir);
  Defun(database_dir);
  SYMBOL_SC_(CorePkg, script_dir);
  Defun(script_dir);
  SYMBOL_SC_(CorePkg, libraryPath);
  Defun(libraryPath);
  SYMBOL_SC_(CorePkg, lispCodePath);
  Defun(lispCodePath);
  //	defNoWrapPackage(CorePkg,"dumpHidden", &prim_dumpHidden,_LISP);

  SYMBOL_SC_(CorePkg, setCurrentWorkingDirectory);
  Defun(setCurrentWorkingDirectory);

  SYMBOL_EXPORT_SC_(ClPkg, acons);
  Defun(acons);
  SYMBOL_EXPORT_SC_(ClPkg, assoc);
  ClDefun(assoc);
  SYMBOL_EXPORT_SC_(ClPkg, member);
  ClDefun(member);
  Defun(memberTest);

  SYMBOL_SC_(CorePkg, member1);
  Defun(member1);

  SYMBOL_SC_(CorePkg, exportToPython);
  Defun(exportToPython);

  SYMBOL_EXPORT_SC_(ClPkg, find_package);
  Defun(find_package);
}

void Lisp_O::exposePython() {
  _G();
#if 0
	Symbol_sp (Lisp_O::*intern1)(const string&) = &Lisp_O::intern;
	PYTHON_CLASS(CorePkg,Lisp,"","",_lisp)
	    .def("intern",intern1)
#if 0
	    .def("true",&Lisp_O::_true)
	    .def("false",&Lisp_O::onil)
	    .def("nil",&Lisp_O::onil)
#endif
	    ;

//    boost::python::def("loadArchive",boost::python::wrapped_function(CorePkg,"loadArchive",&prim_loadArchive,ARGS_prim_loadArchive,DOCS_prim_loadArchive,lisp));
#if 0
//	boost::python::def_raw(CorePkg,"export",&prim_export,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"apply", &prim_apply,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"testomp", &prim_testomp,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"isTopLevelScript", &prim_isTopLevelScript ,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"allGlobalNames", &prim_allGlobalNames ,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"locals", &prim_locals,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"sourceFileLine", &prim_sourceFileLine,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"backtrace", &prim_backtrace,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"globals", &prim_globals,ARGS_empty,DOCS_empty,_LISP);

	boost::python::def_raw(CorePkg,"load", &prim_load,ARGS_prim_load,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"include",&prim_include,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"testScanner", &prim_testScanner ,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"printPushPrefix", &prim_printPushPrefix ,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"printPopPrefix", &prim_printPopPrefix ,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"println", &prim_println ,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"print", &prim_print ,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"prin1", &prim_prin1 ,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"prim-dump", &prim_dump ,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"isAssignableTo", &prim_isAssignableTo ,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"isSubClassOf", &prim_isSubClassOf,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"derive", &prim_derive ,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"isA", &prim_isA ,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"dumpln", &prim_dumpln ,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"parseConsOfStrings", &prim_parseConsOfStrings ,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"exit", &prim_exit ,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"quit", &prim_exit ,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"funcall", &prim_funcall ,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"apply", &prim_apply ,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"sub", &prim_sub ,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"-", &prim_sub ,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"div", &prim_div ,ARGS_empty,DOCS_empty,_LISP);
//    boost::python::def_raw(CorePkg,"mod", &prim_mod ,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"/", &prim_div ,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"mul", &prim_mul ,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"*", &prim_mul ,ARGS_empty,DOCS_empty,_LISP);
#if 0
	boost::python::def_raw(CorePkg,"listref", &prim_listref ,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"@", &prim_listref ,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"car", &prim_car ,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"cdr", &prim_cdr ,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"cddr", &prim_cddr ,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"cdddr", &prim_cdddr ,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"cadr", &prim_cadr ,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"caddr", &prim_caddr ,ARGS_empty,DOCS_empty,_LISP);
#endif
//    boost::python::def_raw(CorePkg,"cons", &prim_cons ,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"className", &prim_className ,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"repr", &prim_repr ,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"not", &prim_not,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"debugLogOn",&prim_debugLogOn,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"debugLogOff",&prim_debugLogOff,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"dumpClasses",&prim_dumpClasses,ARGS_empty,DOCS_empty,_LISP);

	// Basic tests
//	boost::python::def_raw(CorePkg,"consp",&prim_consp,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"symbolp",&prim_symbolp,ARGS_empty,DOCS_empty,_LISP);

	// aliases for "list" command

//	boost::python::def_raw(CorePkg,"comparer", &prim_list ,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"sorted", &prim_sorted ,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"sort", &prim_sort,ARGS_empty,DOCS_empty,_LISP);
	// information functions
//	boost::python::def_raw(CorePkg,"databaseDir", &prim_databaseDir,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"scriptDir", &prim_scriptDir,ARGS_empty,DOCS_empty,_LISP);
//	boost::python::def_raw(CorePkg,"dumpHidden", &prim_dumpHidden,ARGS_empty,DOCS_empty,_LISP);

	boost::python::def_raw(CorePkg,"setCurrentWorkingDirectory",&prim_setCurrentWorkingDirectory,ARGS_empty,DOCS_empty,_LISP);
#endif
	initializePythonPrimitives(_LISP);
#endif
}

LispHolder::LispHolder(bool mpiEnabled, int mpiRank, int mpiSize) {
  this->_Lisp = Lisp_O::createLispEnvironment(mpiEnabled, mpiRank, mpiSize);
}

void LispHolder::startup(int argc, char *argv[], const string &appPathEnvironmentVariable) {
  this->_Lisp->_StackTop = (char *)&argc;
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

Exposer::Exposer(Lisp_sp lisp, const string &packageName, const char *nicknames[]) {
  _G();
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

Exposer::Exposer(Lisp_sp lisp, const string &packageName) {
  _G();
  if (!lisp->recognizesPackage(packageName)) {
    list<string> lnnames;
    list<string> lpkgs;
    this->_Package = lisp->makePackage(packageName, lnnames, lpkgs);
  } else {
    this->_Package = gc::As<Package_sp>(lisp->findPackage(packageName, true));
  }
  this->_PackageName = packageName;
}

Exposer::~Exposer(){};

//    EXPOSE_CLASS(core,Lisp_O);

ChangePackage::ChangePackage(Package_sp newPackage) : _SavedPackage(_lisp->getCurrentPackage()) {
  _lisp->selectPackage(newPackage);
}

ChangePackage::~ChangePackage() {
  _lisp->selectPackage(this->_SavedPackage);
}
};
