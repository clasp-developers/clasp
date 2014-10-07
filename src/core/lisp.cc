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
#define	DEBUG_LEVEL_FULL


#ifdef USEBOOSTPYTHON
#include "useBoostPython.h"
#endif

#include <stdlib.h>
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunneeded-internal-declaration"
//#pragma GCC diagnostic ignored "-Wunused-local-typedef"
#include "boost/filesystem.hpp"
#include "boost/algorithm/string.hpp"
#include "boost/program_options.hpp"
#pragma GCC diagnostic pop
//#i n c l u d e	"boost/fstream.hpp"
#include "foundation.h"
#include "object.h"
#ifdef DEBUG_CL_SYMBOLS
#include "allClSymbols.h"
#endif
#include "candoOpenMp.h"
#include "exceptions.h"
#include "commandLineOptions.h"
#include "symbolTable.h"
#include "compiler.h"
#include "lisp.h"
#include "lispList.h"
#include "loadTimeValues.h"
#include "profiler.h"
#include "bundle.h"
#include "bformat.h"
#include "stringSet.h"
#include "hashTableEq.h"
#include "hashTableEqual.h"
#include "pointer.h"
#include "cons.h"
#include "specialForm.h"
#include "documentation.h"
#include "backquote.h"
#include "testing.h"
#include "bformat.h"
#include "cache.h"
#include "environment.h"
#include "extensionPackage.h"
#include "binder.h"
#include "numbers.h"
#include "load.h"
#include "bignum.h"
//#i n c l u d e "setfExpander.h"
#include "standardObject.h"
#include "ql.h"
#include "str.h"
#include "commonLispPackage.h"
#include "keywordPackage.h"
#include "fileSystem.h"
#include "sysprop.h"
#include "hashTableEql.h"
#include "debugger.h"
#include "builtInClass.h"
#include "standardClass.h"
#include "numberToString.h"
#include "executables.h"
#include "myReadLine.h"
#include "sourceFileInfo.h"
#include "lispStream.h"
#include "lispReader.h"
#include "write_object.h"
#include "write_ugly.h"
#include "lispMath.h"
#include "pathname.h"
#include "print.h"
#include "core/genericFunction.h"
#include "multipleValues.h"
#if defined(XML_ARCHIVE)
#include "xmlLoadArchive.h"
#include "xmlSaveArchive.h"
#endif // defined(XML_ARCHIVE)
#if defined(OLD_SERIALIZE)
#include "serialize.h"
#endif // defined(OLD_SERIALIZE)
#include "bootStrapCoreSymbolMap.h"
#include "numerics.h"
#include "reader.h"
//#i n c l u d e "genericFunction.h"
#include "singleDispatchGenericFunction.h"
#include "executables.h"
#include "designators.h"
#include "unixfsys.h"
#include "sort.h"
#include "bitVector.h"
#include "character.h"
#include "predicates.h"
#include "primitives.h"
#include "package.h"
#include "symbol.h"
#include "lambdaListHandler.h"
#include "sequence.h"
#include "evaluator.h"
#include "float_to_digits.h"
#include "num_arith.h"
#include "num_co.h"
#include "lispDefinitions.h"
#include "myReadLine.h"
#include "externalObject.h"
#include "initializeClasses.h"
#include "holder.h"
#include "core/corePackage.h"
#include "core/stacks.h"
#include "primitives.h"
#include "readtable.h"
//#i n c l u d e "clos.h"
#include "wrappers.h"
#include "python_wrappers.h"

#ifdef	READLINE
extern "C" char *readline( const char* prompt);
extern "C" void add_history(char* line);
#endif


namespace core
{

    const int Lisp_O::MaxFunctionArguments = 64; //<! See ecl/src/c/main.d:163 ecl_make_cache(64,4096)
    const int Lisp_O::MaxClosSlots = 3; //<! See ecl/src/c/main.d:164 ecl_make_cache(3,4096)
    const int Lisp_O::ClosCacheSize = 65536;
    const int Lisp_O::SingleDispatchMethodCacheSize = 65536;


    extern void lispScannerDebug(std::istream& sin);
    extern string	getLispError();


    SMART(BuiltInClass);




    struct FindApropos : public KeyValueMapper //, public gctools::StackRoot
    {
    public:
	HashTable_sp    _symbols;
	string		_substr;
	FindApropos(const string& str) {
            this->_substr = str;
            this->_symbols = HashTableEq_O::create_default();
        };
	virtual bool mapKeyValue(T_sp key, T_sp value)
	{
	    Bignum_sp skey = key.as<Bignum_O>();
	    Symbol_sp svalue = value.as<Symbol_O>();
	    string symbolName = lisp_symbolNameAsString(svalue);
	    string::size_type pos = symbolName.find(this->_substr);
//	    LOG(BF("Looking at symbol(%s) for (%s) found: %d") % symbolName % this->_substring % pos );
	    if ( pos != string::npos )
	    {
		LOG(BF("    It is apropos"));
                this->_symbols->setf_gethash(svalue,_Nil<T_O>());
	    }
	    return true;
	}

    };





//
// Constructor
//
    Lisp_O::GCRoots::GCRoots() : _BignumRegister0(_Unbound<Bignum_O>())
                               , _BignumRegister1(_Unbound<Bignum_O>())
                               , _BignumRegister2(_Unbound<Bignum_O>())
//                               , _TraceFunctions(_Unbound<HashTable_O>())
                               , _SystemProperties(_Unbound<HashTable_O>())
                               , _CatchInfo(_Nil<Cons_O>())
                               ,  _SpecialForms(_Unbound<HashTableEq_O>())
                               , _ActivationFrameNil(_Nil<ActivationFrame_O>())
                               , _SingleDispatchMethodCachePtr(NULL)
                               , _MethodCachePtr(NULL)
                               , _SlotCachePtr(NULL)
                               , _NullStream(_Nil<Stream_O>())
                               , _PathnameTranslations(_Nil<Cons_O>())
    {}

    Lisp_O::Lisp_O() : _StackWarnSize(15*1024*1024), // 8MB default stack size before warnings
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
		       _PathMax(MAXPATHLEN)
    {
	this->_Roots._Bindings.reserve(1024);
	this->_GlobalInitializationCallbacks.clear();
	this->_MakePackageCallback = NULL;
	this->_ExportSymbolCallback = NULL;
#ifdef CLOS
	this->_Roots._SlotCachePtr = NULL;
	this->_Roots._MethodCachePtr = NULL;
#endif
    }


    void Lisp_O::shutdownLispEnvironment()
    {
	if ( this->_DebugStream != NULL )
	{
	    this->_DebugStream->beginNode(DEBUG_TOPLEVEL);
	}
	this->_Roots._CommandLineArguments.reset();
	this->_Roots._Packages.clear();
//	this->_Roots._HiddenBinder.reset();
//	this->_Roots._SpecialForms.clear();
	this->_Roots._TrueObject.reset();

//    this->_ClassesByClassSymbol.clear();
	if ( this->_Bundle != NULL )
	{
	    delete this->_Bundle;
	}
	if ( this->_DebugStream!=NULL )
	{
	    this->_DebugStream->endNode(DEBUG_TOPLEVEL);
	    delete this->_DebugStream;
	}
    }

#if 0
    Cons_sp Lisp_O::catchPushTag(T_sp tag)
    {
        Cons_sp one = Cons_O::create(tag,this->_Roots._CatchInfo);
        this->_Roots._CatchInfo = one;
        return one;
    }

    void Lisp_O::catchUnwindTag(Cons_sp catchStore)
    {_G();
        this->_Roots._CatchInfo = cCdr(catchStore);
    }

    Cons_sp Lisp_O::catchFindTag(T_sp tag)
    {_G();
        for ( Cons_sp cur=this->_Roots._CatchInfo; cur.notnilp(); cur=cCdr(cur) )
        {
            if ( af_eq(tag,oCar(cur)) ) return cur;
        }
        return _Nil<Cons_O>();
    }
#endif



    void print_startup_info()
    {
#if 1
	printf("%s:%d BRIDGE-COMMON-LISP startup\n", __FILE__, __LINE__ );
#endif
    };




    Lisp_O::~Lisp_O()
    {
	// nothing is left to be done here
    }

    void Lisp_O::lisp_initSymbols(Lisp_sp lisp)
    {
	Package_sp corePackage = lisp->_Roots._CorePackage;
    }


    void	Lisp_O::initialize()
    {
        this->_Roots._MultipleValues.initialize();
    }



    void Lisp_O::addToStarModulesStar(Symbol_sp sym)
    {_OF();
	Cons_sp list = cl::_sym_STARmodulesSTAR->symbolValue().as_or_nil<Cons_O>();
	list = Cons_O::create(sym,list);
	cl::_sym_STARmodulesSTAR->setf_symbolValue(list);
    }



    template <class oclass>
    void setup_static_classSymbol(BootStrapCoreSymbolMap const& sidMap)
    {
	oclass::___set_static_ClassSymbol(sidMap.lookupSymbol(oclass::static_packageName(),oclass::static_className()));
    }



    string dump_instanceClass_info(Class_sp co, Lisp_sp prog)
    {_G();
	stringstream ss;
	ss << "------------------------------------- class" << _rep_(co->className()) << std::endl;;
	LOG(BF("Dumping info: %s") % co->dumpInfo() );
	ss << co->dumpInfo();
	return ss.str();
    }
    template <class oclass>
    void dump_info(BuiltInClass_sp co, Lisp_sp lisp)
    {_G();
	LOG(BF("-------    dump_info    --------------- className: %s @ %X")% oclass::static_className() % co.get());
	LOG(BF("%s::static_classSymbol() = %d")% oclass::static_className() % oclass::static_classSymbol() );
	LOG(BF("%s::Base::static_classSymbol() = %d")% oclass::static_className() % oclass::Base::static_classSymbol() );
	LOG(BF("%s::static_newNil_callback() = %X")% oclass::static_className() % (void*)(oclass::static_allocator) );
//    LOG(BF("%s")%dump_instanceClass_info(co,lisp));
    }





    Lisp_sp Lisp_O::createLispEnvironment(bool mpiEnabled, int mpiRank, int mpiSize )
    {
        ::_lisp = gctools::RootClassAllocator<Lisp_O>::allocate();
	_lisp->initialize();
        _lisp->setupMpi(mpiEnabled,mpiRank,mpiSize);
//	lisp->__setWeakThis(lisp);
//	lisp->__resetInitializationOwner();
	_lisp->_DebugStream = new DebugStream(mpiRank);
	LOG(BF("The lisp environment DebugStream has been created"));
	return _lisp;
    }



    void Lisp_O::setupMpi(bool mpiEnabled, int mpiRank, int mpiSize)
    {
	this->_MpiEnabled = mpiEnabled;
	this->_MpiRank = mpiRank;
	this->_MpiSize = mpiSize;
    }


#ifdef USE_REFCOUNT
    void testContainers()
    {
        Fixnum_sp fn = Fixnum_O::create(1);
        printf("%s:%d  fn@%p  referenceCount() = %d\n", __FILE__, __LINE__, fn.px_ref(), fn->referenceCount() );
        gctools::Vec0<T_sp> container;
        container.push_back(fn);
        printf("%s:%d  after push_back to container fn@%p  referenceCount() = %d\n", __FILE__, __LINE__, fn.px_ref(), fn->referenceCount() );
        Fixnum_sp fn2 = container.back().as<Fixnum_O>();
        printf("%s:%d  after back to container fn@%p  referenceCount() = %d\n", __FILE__, __LINE__, fn.px_ref(), fn->referenceCount() );
        container.pop_back();
        printf("%s:%d  after pop_back to container fn@%p  referenceCount() = %d\n", __FILE__, __LINE__, fn.px_ref(), fn->referenceCount() );
        printf("%s:%d  fn2@%p  referenceCount() = %d\n", __FILE__, __LINE__, fn2.px_ref(), fn2->referenceCount() );
    };
#endif


    void testStrings()
    {
        Str_sp str = Str_O::create("This is a test");
//        printf("%s:%d  Str_sp = %s\n", __FILE__, __LINE__, str->c_str() );
    }

    void Lisp_O::startupLispEnvironment(Bundle* bundle)
    {

	/*! There was a problem iterating over maps and sets when garbage collection
	  they were going into infinite loops - so I was testing them here within
	  the context of the entire package */
//	testIterators();
#ifdef USE_REFCOUNT
        testContainers();
#endif

	this->_Mode = FLAG_EXECUTE;

	::_lisp = this; // this->sharedThis<Lisp_O>();

//	initializeProfiler(this->profiler(),_lisp);
	this->_TraceLevel = 0;
	this->_DebuggerLevel = 0;
	this->_CoreBuiltInClassesInitialized = false;
	this->_PackagesInitialized = false;
	this->_BuiltInClassesInitialized = false;
	this->_NilsCreated = false;
	this->_EnvironmentInitialized = false;
	this->_EnvironmentId = 0;
	this->_Roots._CommandLineArguments.reset();

	this->_Bundle = bundle;


        CoreExposer* coreExposerPtr = NULL;
	BuiltInClass_sp classDummy;

#ifdef DEBUG_CL_SYMBOLS
        initializeAllClSymbols();
#endif


	{ _BLOCK_TRACE("Initialize core classes");
	    coreExposerPtr = CoreExposer::create_core_packages_and_classes();
// TODO: Should this be a WeakKeyHashTable?
	    {_BLOCK_TRACE("Define important predefined symbols for CorePkg");
		coreExposerPtr->define_essential_globals(_lisp);
		this->_PackagesInitialized = true;
	    }
	}
	{_BLOCK_TRACE("Create some housekeeping objects");
            this->_Roots._LoadTimeValueArrays = HashTableEqual_O::create_default();
            this->_Roots._SetfDefinitions = HashTableEq_O::create_default();
            this->_Roots._SingleDispatchGenericFunctionTable = HashTableEq_O::create_default();
	}
	{ _BLOCK_TRACE("Initialize special forms and macros");
	    this->_EnvironmentInitialized = true;
	    eval::defineSpecialOperatorsAndMacros(this->_Roots._CorePackage);
//	    this->createHiddenBinder();
	}
	this->_BuiltInClassesInitialized = true;
//	LOG(BF("ALL CLASSES: %s")% this->dumpClasses() );
//    this->createNils();
	{_BLOCK_TRACE("Dump of all BuiltInClass classes");
#ifdef	DEBUG_ON
	    //    rootClassManager().debugDump();
#endif
	}


//	LOG(BF("Package(%s) symbols: %s")% this->_CorePackage->getName() % this->_CorePackage->allSymbols() );
//	LOG(BF("Package(%s) symbols: %s")% keywordPackage->getName() % keywordPackage->allSymbols() );

	//
	// Finish initializing Lisp object
	//
	this->_Roots._CommandLineArguments = _Nil<Cons_O>();
#if 0
	{_BLOCK_TRACE("Initialize scripting stuff");
#include "core_initScripting_inc.h"
	}
#endif
	{_BLOCK_TRACE("Initialize other code"); // needs _TrueObject
#define Use_CorePkg
//#include "core_initScripting_inc.h"
#undef Use_CorePkg

//            testStrings();



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
	    initialize_predicates();
	    initialize_bformat(_lisp);
	    initialize_sysprop();
	    initialize_testing();
	    initialize_profile();
	    initialize_designators();
	    initialize_debugging();
	    initialize_math();
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

	    coreExposerPtr->expose(_lisp,Exposer::candoClasses);
//	    initializeCandoClos(_lisp);
	}
	{

	    // setup the SYS logical-pathname-translations
	    Cons_sp pts = Cons_O::createList(
		Cons_O::createList(Str_O::create("sys:**;*.*"), bundle->getSysPathname())
		/* ,  more here */
		);
	    af_pathnameTranslations(Str_O::create("sys"),_lisp->_true(),pts);

            // setup the APP-RESOURCES logical-pathname-translations
            Cons_sp app = Cons_O::createList(
                Cons_O::createList(Str_O::create("app-resources:**;*.*"), bundle->getAppContentsResourcesPathname())
                /* , more here */
                );
	    af_pathnameTranslations(Str_O::create("app-resources"),_lisp->_true(),app);

            // setup the build;system pathnames
            {
                Cons_sp p = Cons_O::createList(
                    Cons_O::createList(Str_O::create("**;*.*"),cl_pathname(Str_O::create("SYS:build;system;min-boehm;**;*.*"))));
                af_pathnameTranslations(Str_O::create("min-boehm"),_lisp->_true(),p);
            }
            {
                Cons_sp p = Cons_O::createList(
                    Cons_O::createList(Str_O::create("**;*.*"),cl_pathname(Str_O::create("SYS:build;system;full-boehm;**;*.*"))));
                af_pathnameTranslations(Str_O::create("full-boehm"),_lisp->_true(),p);
            }
            {
                Cons_sp p = Cons_O::createList(
                    Cons_O::createList(Str_O::create("**;*.*"),cl_pathname(Str_O::create("SYS:build;system;min-mps;**;*.*"))));
                af_pathnameTranslations(Str_O::create("min-mps"),_lisp->_true(),p);
            }
            {
                Cons_sp p = Cons_O::createList(
                    Cons_O::createList(Str_O::create("**;*.*"),cl_pathname(Str_O::create("SYS:build;system;full-mps;**;*.*"))));
                af_pathnameTranslations(Str_O::create("full-mps"),_lisp->_true(),p);
            }

	}
#if 0  // I shouldn't be using PATH - I should be using PATHNAMEs
	{_BLOCK_TRACE("Initializing special variable PATH");
	    boost_filesystem::path scriptPath = bundle->getLispDir();
	    Cons_sp path = this->create<Cons_O>(Str_O::create("./"));
	    if ( !scriptPath.empty() )
	    {
		Str_sp scriptDir = Str_O::create(scriptPath.string());
		path = Cons_O::create(scriptDir,path);
	    }
	    Symbol_sp sym = _sym_STARPATHSTAR;
	    this->defvar(sym,path);
	    this->_RequireLevel = 0;
	}
#endif
	//
	//
	//
	this->exposeCando();
	Lisp_O::initializeGlobals(_lisp);
	coreExposerPtr->expose(_lisp,Exposer::candoFunctions);
	coreExposerPtr->expose(_lisp,Exposer::candoGlobals);
	{_BLOCK_TRACE("Call global initialization callbacks");
	    for ( vector<InitializationCallback>::iterator ic = this->_GlobalInitializationCallbacks.begin();
		  ic!=this->_GlobalInitializationCallbacks.end(); ic++ )
	    {
		(*ic)(_lisp);
	    }
	}

	Path_sp startupWorkingDir = Path_O::create(bundle->getStartupWorkingDir());
	this->defconstant(_sym_STARcurrent_working_directorySTAR,_Nil<Path_O>());
	this->setCurrentWorkingDirectory(startupWorkingDir);

	this->switchToClassNameHashTable();

	{_BLOCK_TRACE("Setup system values");
	    FILE* null_out = fopen("/tmp/null","w");
	    this->_Roots._NullStream = IOStreamStream_O::makeIO("/tmp/null",null_out);
	    this->_Roots._RehashSize = DoubleFloat_O::create(2.0);
	    this->_Roots._RehashThreshold = DoubleFloat_O::create(0.9);
	    this->_Roots._ImaginaryUnit = Complex_O::create(0.0,1.0);
	    this->_Roots._ImaginaryUnitNegative = Complex_O::create(0.0,-1.0);
	    this->_Roots._PlusHalf = Ratio_O::create(Fixnum_O::create(1),Fixnum_O::create(2));
	    this->_Roots._MinusHalf = Ratio_O::create(Fixnum_O::create(-1),Fixnum_O::create(2));
	    this->_Roots._SingleFloatOne = SingleFloat_O::create(1.0);
	    this->_Roots._DoubleFloatOne = DoubleFloat_O::create(1.0);
	    this->_Roots._SingleFloatMinusZero = SingleFloat_O::create(-0.0);
	    this->_Roots._SingleFloatPlusZero = SingleFloat_O::create(0.0);
	    this->_Roots._DoubleFloatMinusZero = DoubleFloat_O::create(-0.0);
	    this->_Roots._DoubleFloatPlusZero = DoubleFloat_O::create(0.0);
#ifdef CLASP_LONG_FLOAT
	    this->_Roots._LongFloatOne = LongFloat_O::create(1.0);
	    this->_Roots._LongFloatMinusZero = LongFloat_O::create(-0.0l);
	    this->_Roots._LongFloatPlusZero = LongFloat_O::create(0.0l);
#endif // ifdef CLASP_LONG_FLOAT
	    this->_Roots._BignumRegister0 = Bignum_O::create(0);
	    this->_Roots._BignumRegister1 = Bignum_O::create(0);
	    this->_Roots._BignumRegister2 = Bignum_O::create(0);
	    getcwd(true); // set *default-pathname-defaults*
	};
	{_BLOCK_TRACE("Creating Caches for SingleDispatchGenericFunctions");
	    this->_Roots._SingleDispatchMethodCachePtr = gctools::ClassAllocator<Cache>::allocateClass();
            this->_Roots._SingleDispatchMethodCachePtr->setup(2,SingleDispatchMethodCacheSize);
        }
	{_BLOCK_TRACE("Creating Caches for CLOS");
	    this->_Roots._MethodCachePtr = gctools::ClassAllocator<Cache>::allocateClass();
	    this->_Roots._MethodCachePtr->setup(MaxFunctionArguments,ClosCacheSize);
	    this->_Roots._SlotCachePtr = gctools::ClassAllocator<Cache>::allocateClass();
	    this->_Roots._SlotCachePtr->setup(MaxClosSlots,ClosCacheSize);
	}
        {_BLOCK_TRACE("Start printing symbols properly");
            this->_PrintSymbolsProperly = true;
        }
#if 0 // wtf is this???
	if ( this->_dont_load_startup )
	{_BLOCK_TRACE("Load startup code");
//	    Pathname_sp initPathname = cl_pathname(Str_O::create("sys:brcl/init.lsp"));
	    Path_sp corePath = Path_O::create(this->_Bundle->getLispDir());
	    corePath->path_append("/init");
	    corePath->path_append("/coreFile.lisp");
	    {_BLOCK_TRACEF(BF("Loading core-file with path: %s") % corePath->asString() );
		this->load(corePath);
	    }
	} else
	{
	    LOG(BF("Not loading startup code"));
	}
#endif
    }




    void Lisp_O::setCurrentWorkingDirectory(Path_sp dir)
    {
	_sym_STARcurrent_working_directorySTAR->setf_symbolValueReadOnlyOverRide(dir);
    }

    Path_sp Lisp_O::getCurrentWorkingDirectory()
    {
	return _sym_STARcurrent_working_directorySTAR->symbolValue().as<Path_O>();
    }



    ReadTable_sp Lisp_O::getCurrentReadTable()
    {
	return cl::_sym_STARreadtableSTAR->symbolValue().as<ReadTable_O>();
    }

    void Lisp_O::setMakePackageAndExportSymbolCallbacks(MakePackageCallback mpc, ExportSymbolCallback esc)
    {_OF();
	LOG(BF("Setting MakePackageCallback and ExportSymbolCallback"));
	this->_MakePackageCallback = mpc;
	this->_ExportSymbolCallback = esc;
    }





#if defined(OLD_SERIALIZE)
    T_sp Lisp_O::sread(Stream_sp sin, bool eofErrorP, T_sp eofValue )
    {_OF();
	ReadSerializer_sp reader = _lisp->create<ReadSerializer_O>();
	T_sp obj = reader->read(sin,eofErrorP,eofValue);
	return obj;
    }

    void Lisp_O::sprint(T_sp obj, Stream_sp sout )
    {_OF();
	WriteSerializer_sp writer = _lisp->create<WriteSerializer_O>();
	writer->addObject(obj);
	writer->write(sout);
    }
#endif // defined(OLD_SERIALIZER)


    void Lisp_O::set_setfDefinition(Symbol_sp fnName, Function_sp fnDef)
    {_G();
	this->_Roots._SetfDefinitions->setf_gethash(fnName,fnDef);
    }

    Function_sp Lisp_O::get_setfDefinition(Symbol_sp fnName) const
    {_G();
        return this->_Roots._SetfDefinitions->gethash(fnName,_Nil<T_O>()).as<Function_O>();
    }

    bool Lisp_O::remove_setfDefinition(Symbol_sp fnName)
    {_G();
        if (this->_Roots._SetfDefinitions->contains(fnName) ) {
            this->_Roots._SetfDefinitions->remhash(fnName);
            return true;
        }
        return false;
    }





    void Lisp_O::print(boost::format fmt)
    {_OF();
	TRY_BOOST_FORMAT_STRING(fmt,fmt_str);
	if ( cl::_sym_print->fboundp() ) {
	    eval::funcall(cl::_sym_print,Str_O::create(fmt_str));
	} else {
	    printf("%s\n", fmt.str().c_str() );
	}
    }

    void Lisp_O::prin1(boost::format fmt)
    {
	TRY_BOOST_FORMAT_STRING(fmt,fmt_str);
	eval::funcall(cl::_sym_prin1,Str_O::create(fmt_str));
    }



    Cons_sp Lisp_O::loadTimeValuesIds() const
    {_G();
	Cons_sp names = _Nil<Cons_O>();
        this->_Roots._LoadTimeValueArrays->mapHash( [&names] (T_sp key, T_sp val) {
                names = Cons_O::create(key,names);
            } );
	return names;
    }


    /*! How is this going to work with moving garbage collection?
     We return a reference to the LoadTimeValues_sp smart_ptr in the LoadtimeValueArrays hash-table
    What happens when this moves????    Disaster!!!!!!!   */
    LoadTimeValues_sp Lisp_O::getOrCreateLoadTimeValues(const string& name,int numberOfLoadTimeValues, int numberOfLoadTimeSymbols)
    {_G();
        Str_sp key = Str_O::create(name);
        T_sp it = this->_Roots._LoadTimeValueArrays->gethash(key,_Nil<T_O>());
	if ( it.nilp() )
	{
	    LoadTimeValues_sp vo = LoadTimeValues_O::make(numberOfLoadTimeValues, numberOfLoadTimeSymbols);
	    this->_Roots._LoadTimeValueArrays->setf_gethash(key,vo);
            return gctools::smart_ptr<LoadTimeValues_O>(reinterpret_cast<LoadTimeValues_O*>(vo.pbase()));
	}
        LoadTimeValues_sp ltv = it.as<LoadTimeValues_O>();
        return gctools::smart_ptr<LoadTimeValues_O>(reinterpret_cast<LoadTimeValues_O*>(ltv.pbase()));
    }


    LoadTimeValues_sp Lisp_O::findLoadTimeValues(const string& name)
    {
        Str_sp key = Str_O::create(name);
        T_sp it = this->_Roots._LoadTimeValueArrays->gethash(key,_Nil<T_O>());
	if ( it.nilp() ) return _Nil<LoadTimeValues_O>();
        return it.as<LoadTimeValues_O>();
    }
    LoadTimeValues_sp Lisp_O::findLoadTimeValuesWithNameContaining(const string& name)
    {
        LoadTimeValues_sp result = _Nil<LoadTimeValues_O>();
        this->_Roots._LoadTimeValueArrays->terminatingMapHash( [&result,&name] (T_sp key, T_sp val) -> bool {
                if ( key.as<Str_O>()->find(name,0).notnilp() ) {
                    result = val.as<LoadTimeValues_O>();
                    return false;
                }
                return true;
            });
        return result;
    }


    void Lisp_O::defvar(Symbol_sp sym, T_sp obj)
    {_OF();
	sym->makeSpecial();
	sym->setf_symbolValue(obj);
    }

    void Lisp_O::defconstant(Symbol_sp sym, T_sp obj)
    {_OF();
	sym->makeSpecial();
	sym->setf_symbolValue(obj);
	sym->setReadOnly(true);
    }


#if 0
    void Lisp_O::setOutputStream(ostream* o)
    {_OF();
	if ( this->_freeOutputStream )
	{
	    delete this->_outputStream;
	}
	this->_freeOutputStream = true;
	this->_outputStream = o;
    }

    std::ostream& Lisp_O::outputStream()
    {
	return *(this->_outputStream);
    }
#endif


    T_sp Lisp_O::error(const boost::format& fmt)
    {_OF();
	return CandoException_O::create(fmt);
    }


    Symbol_sp Lisp_O::errorUndefinedSymbol(const char* sym)
    {_OF();
	stringstream ss;
	ss << "Unknown symbol("<<sym<<")";
	SIMPLE_ERROR(BF("%s") % ss.str());
    }








#if 0
    void Lisp_O::createHiddenBinder()
    {_G();
	this->_Roots._HiddenBinder = this->create<Binder_O>();
    }

    Binder_sp Lisp_O::hiddenBinder()
    {_G();
	ASSERTNOTNULL(this->_HiddenBinder);
	ASSERT(this->_Roots._HiddenBinder.notnilp());
	return this->_Roots._HiddenBinder;
    }
#endif

    Symbol_sp Lisp_O::defineSpecialOperator(const string& packageName, const string& rawFormName, SpecialFormCallback cb, const string& argstring, const string& docstring )
    {_OF();
	string formName = lispify_symbol_name(rawFormName);
	Symbol_sp sym = _lisp->internWithPackageName(packageName,formName);
        sym->exportYourself();
	SpecialForm_sp special = SpecialForm_O::create(sym,cb);
        if ( this->_Roots._SpecialForms.unboundp() ) {
            this->_Roots._SpecialForms = HashTableEq_O::create_default();
        }
	ASSERTP(!this->_Roots._SpecialForms->contains(sym),"You cant define a special form with the symbol("+formName+") it has already been defined");
	this->_Roots._SpecialForms->setf_gethash(sym,special);
	return sym;
    }









    SpecialForm_sp Lisp_O::specialFormOrNil(Symbol_sp sym)
    {
	if ( sym.nilp() ) return _Nil<SpecialForm_O>();
        return this->_Roots._SpecialForms->gethash(sym,_Nil<SpecialForm_O>()).as<SpecialForm_O>();
    }







    void Lisp_O::installPackage(const Exposer* pkg)
    {_OF();
	LOG(BF("Installing package[%s]") % pkg->packageName() );
	int firstNewGlobalCallback = this->_GlobalInitializationCallbacks.end()-this->_GlobalInitializationCallbacks.begin();
	ChangePackage change(pkg->package());
//    this->inPackage(pkg->packageName());
	{_BLOCK_TRACE("Initializing classes");
	    pkg->expose(_lisp,Exposer::candoClasses);
	}
	{_BLOCK_TRACE("Creating nils for built-in classes");
	    LOG(BF("Nils aren't created here anymore - they are created when the class is registered"));
//	this->createNils();
	}
	{_BLOCK_TRACE("Initializing functions");
	    pkg->expose(_lisp,Exposer::candoFunctions);
	}
	{_BLOCK_TRACE("Initializing globals");
	    pkg->expose(_lisp,Exposer::candoGlobals);
	}


	{_BLOCK_TRACE("Call global initialization callbacks");
	    for ( vector<InitializationCallback>::iterator ic = this->_GlobalInitializationCallbacks.begin()+firstNewGlobalCallback;
		  ic!=this->_GlobalInitializationCallbacks.end(); ic++ )
	    {
		(*ic)(_lisp);
	    }
	}

    }


    void Lisp_O::installGlobalInitializationCallback(InitializationCallback c)
    {
	this->_GlobalInitializationCallbacks.push_back(c);
    }



#if defined(XML_ARCHIVE)
    void	Lisp_O::archive(::core::ArchiveP node)
    {_OF();
	SIMPLE_ERROR(BF("Never archive Lisp objects"));
    }
#endif // defined(XML_ARCHIVE)


    void Lisp_O::addClassNameToPackageAsDynamic(const string& package, const string& name, Class_sp mc)
    {_G();
	Symbol_sp classSymbol = _lisp->intern(name,_lisp->findPackage(package));
	classSymbol->exportYourself();
	classSymbol->setf_symbolValue(mc);
//    this->globalEnvironment()->extend(classSymbol,mc);
//    mc->__setLambdaListHandlerString(mc->getInstanceBaseClass()->__getLambdaListHandlerString());
    }

/*! Add the class with (className) to the current package
 */
    void Lisp_O::addClass(Symbol_sp classSymbol,
			  Creator* alloc,
			  Symbol_sp base1ClassSymbol,
			  Symbol_sp base2ClassSymbol,
			  Symbol_sp base3ClassSymbol )
    {_G();
        DEPRECIATED();
	LOG(BF("Lisp_O::addClass classSymbol(%s) baseClassSymbol1(%u) baseClassSymbol2(%u)")
	    % _rep_(classSymbol) % base1ClassSymbol % base2ClassSymbol  );
	ASSERTP(IS_SYMBOL_DEFINED(BuiltInClass_O::static_classSymbol()),
		"You cannot create a BuiltInClass before the BuiltIn!Class is defined");
	Class_sp cc;
	if ( classSymbol == StandardObject_O::static_classSymbol() )
	{
	    IMPLEMENT_ME(); // WHEN DO StandardClasses get created with addClass?????
	} else {
	    LOG(BF("Adding BuiltInClass with classSymbol(%d)") % classSymbol );
	    cc = BuiltInClass_O::create(classSymbol);
	}
	printf("%s:%d --> Adding class[%s]\n", __FILE__, __LINE__, _rep_(classSymbol).c_str() );
	af_setf_findClass(cc,classSymbol,true,_Nil<Environment_O>());
	if ( IS_SYMBOL_DEFINED(base1ClassSymbol))
	{
	    cc->addInstanceBaseClass(base1ClassSymbol);
	} else
	{
	    SIMPLE_ERROR(BF("There must be one base class"));
	}
	if ( IS_SYMBOL_DEFINED(base2ClassSymbol))
	{
	    cc->addInstanceBaseClass(base2ClassSymbol);
	}
	if ( IS_SYMBOL_DEFINED(base3ClassSymbol))
	{
	    cc->addInstanceBaseClass(base3ClassSymbol);
	}
	ASSERTF(alloc!=NULL,BF("_creator for %s is NULL!!!") % _rep_(classSymbol) );
	cc->setCreator(alloc);
    }




/*! Add the class with (className) to the current package
 */
    void Lisp_O::addClass(Symbol_sp classSymbol, Class_sp theClass, Creator* allocator )
    {_G();
//	printf("%s:%d:%s  Adding class with symbol %s -- _allocator=%p unless we initialize it properly\n", __FILE__,__LINE__,__FUNCTION__,_rep_(classSymbol).c_str(), allocator );
	LOG(BF("Lisp_O::addClass classSymbol(%s)") % _rep_(classSymbol) );
//	printf("%s:%d --> Adding class[%s]\n", __FILE__, __LINE__, _rep_(classSymbol).c_str() );
	af_setf_findClass(theClass,classSymbol,true,_Nil<Environment_O>());
//        IMPLEMENT_MEF(BF("Pass an AllocateInstanceFunctor"));
	theClass->setCreator(allocator);
    }



#if 0
/*! Add the class with (className) to the current package
 */
    void Lisp_O::addClass(Symbol_sp classSymbol )
    {_G();
        DEPRECIATED();
//	printf("%s:%d:%s  Adding class with symbol %s -- It will have a NULL _allocator unless we initialize it properly\n", __FILE__,__LINE__,__FUNCTION__,_rep_(classSymbol).c_str() );
	LOG(BF("Lisp_O::addClass classSymbol(%s)") % _rep_(classSymbol) );
	ASSERTP(BuiltInClass_O::static_classSymbol(),
		"You cannot create a BuiltInClass before the BuiltInClass is defined");
	Class_sp cc;
	if ( classSymbol == StandardObject_O::static_classSymbol() )
	{
	    IMPLEMENT_ME(); // WHEN DO StandardClasses get created with addClass?????
	} else {
	    LOG(BF("Adding BuiltInClass with classSymbol(%d)") % classSymbol );
	    cc = BuiltInClass_O::create(classSymbol);
	}
        IMPLEMENT_MEF(BF("Identify from where this is coming from and set up the allocator"));
        this->addClass(classSymbol,cc,NULL);
    }
#endif

    StringSet_sp Lisp_O::allClassNames()
    {_G();
	DEPRECIATED();
    }





    StandardClass_sp Lisp_O::defineStandardClass(Symbol_sp name, T_sp baseClassesDesignator, Cons_sp slotSpecifiers )
    {_OF();
	IMPLEMENT_MEF(BF("Implement defineStandardClass"));
    }






    void Lisp_O::exportToPython(Symbol_sp sym) const
    {_OF();
	if ( this->_ExportSymbolCallback == NULL )
	{
	    LOG(BF("Could not export symbol[%s] because _ExportSymbolCallback is NULL") % _rep_(sym) );
	} else
	{
	    LOG(BF("Exporting symbol[%s]") % _rep_(sym) );
	    this->_ExportSymbolCallback(sym,_lisp);
	}
    }


    void Lisp_O::mapNameToPackage(const string& name, Package_sp pkg)
    {
        int packageIndex;
        for ( packageIndex=0; packageIndex<this->_Roots._Packages.size(); ++packageIndex )
        {
            if ( this->_Roots._Packages[packageIndex] == pkg ) goto FOUND;
        }
        SIMPLE_ERROR(BF("Could not find package with (nick)name: %s") % pkg->getName());
    FOUND:
        this->_PackageNameIndexMap[name] = packageIndex;
    }


    void Lisp_O::unmapNameToPackage(const string& name)
    {
        map<string,int>::iterator it;
        it = this->_PackageNameIndexMap.find(name);
        if ( it==this->_PackageNameIndexMap.end() ) {
            SIMPLE_ERROR(BF("Could not find package with (nick)name: %s") % name );
        }
        this->_PackageNameIndexMap.erase(it);
    }




    Package_sp Lisp_O::makePackage(const string& name,list<string> const& nicknames, list<string> const& usePackages)
    {_G();
	map<string,int>::iterator it = this->_PackageNameIndexMap.find(name);
	if ( it != this->_PackageNameIndexMap.end() ) {
	    SIMPLE_ERROR(BF("There already exists a package with name: %s") % name);
	}
	LOG(BF("Creating package with name[%s]") % name);
	Package_sp newPackage = Package_O::create(name);
	int packageIndex = this->_Roots._Packages.size();
	{
//            printf("%s:%d Lisp_O::makePackage name: %s   index: %d   newPackage@%p\n", __FILE__, __LINE__, name.c_str(), packageIndex, newPackage.px_ref());

	    this->_PackageNameIndexMap[name] = packageIndex;
	    this->_Roots._Packages.push_back(newPackage);
	}
	Cons_sp cnicknames(_Nil<Cons_O>());
	for ( list<string>::const_iterator it=nicknames.begin(); it!=nicknames.end(); it++ )
	{
	    string nickName = *it;
	    if ( this->_PackageNameIndexMap.count(nickName) > 0 && nickName != name  )
	    {
		int existingIndex = this->_PackageNameIndexMap[nickName];
		SIMPLE_ERROR(BF("Package nickname[%s] is already being used by package[%s]") % nickName % this->_Roots._Packages[existingIndex]->getName());
	    }
	    this->_PackageNameIndexMap[nickName] = packageIndex;
	    cnicknames = Cons_O::create(Str_O::create(nickName),cnicknames);
	}
	newPackage->setNicknames(cnicknames);

	for ( list<string>::const_iterator jit=usePackages.begin(); jit!=usePackages.end(); jit++ )
	{
	    Package_sp usePkg = this->findPackage(*jit);
	    if ( usePkg.nilp() ) {
		SIMPLE_ERROR(BF("Could not find package %s to use") % (*jit) );
	    }
	    LOG(BF("Using package[%s]") % usePkg->getName() );
	    newPackage->usePackage(usePkg);
	}
	if ( this->_MakePackageCallback != NULL )
	{
	    LOG(BF("Calling _MakePackageCallback with package[%s]") % name );
	    this->_MakePackageCallback(name,_lisp);
	} else
	{
	    LOG(BF("_MakePackageCallback is NULL - not calling callback"));
	}
	return newPackage;
    }


    Package_sp Lisp_O::findPackage(const string& name) const
    {_G();
//        printf("%s:%d Lisp_O::findPackage name: %s\n", __FILE__, __LINE__, name.c_str());
	map<string,int>::const_iterator fi = this->_PackageNameIndexMap.find(name);
	if ( fi == this->_PackageNameIndexMap.end() )
	{
	    return _Nil<Package_O>(); // return nil if no package found
	}
//        printf("%s:%d Lisp_O::findPackage index: %d\n", __FILE__, __LINE__, fi->second );
	Package_sp getPackage = this->_Roots._Packages[fi->second];
//        printf("%s:%d Lisp_O::findPackage pkg@%p\n", __FILE__, __LINE__, getPackage.px_ref());
	return getPackage;
    }


    SourceManager_sp Lisp_O::sourceDatabase() const
    {_OF();
	// At startup the *package* symbol may not yet
	// be defined or bound to a package - in that case just say we are in the core package
	//
	T_sp cur;
	if ( IS_SYMBOL_UNDEFINED(_sym_STARsourceDatabaseSTAR) )
	{
	    return _Nil<SourceManager_O>();
	}
	if ( !_sym_STARsourceDatabaseSTAR->specialP() )
	{
	    return _Nil<SourceManager_O>();
	}
	cur = _sym_STARsourceDatabaseSTAR->symbolValue();
        if ( cur.nilp() ) return cur;
        return cur.as<SourceManager_O>();
    }


    Package_sp Lisp_O::getCurrentPackage() const
    {_OF();
	// At startup the *package* symbol may not yet
	// be defined or bound to a package - in that case just say we are in the core package
	//
	Package_sp cur;
	if ( IS_SYMBOL_UNDEFINED(cl::_sym_STARpackageSTAR) )
	{
	    cur = this->_Roots._CorePackage;
	    goto DONE;
	}
	if ( !cl::_sym_STARpackageSTAR->specialP() )
	{
	    cur = this->_Roots._CorePackage;
	    goto DONE;
	}
	cur = cl::_sym_STARpackageSTAR->symbolValue().as<Package_O>();
    DONE:
	ASSERTNOTNULL(cur);
	return cur;
    }

    void Lisp_O::selectPackage(Package_sp pack)
    {_OF();
	cl::_sym_STARpackageSTAR->setf_symbolValue(pack);
    }

    bool Lisp_O::recognizesPackage(const string& packageName ) const
    {
	map<string,int>::const_iterator pi = this->_PackageNameIndexMap.find(packageName);
	return ( pi!=this->_PackageNameIndexMap.end() );
    }

    void Lisp_O::inPackage(const string& p)
    {_OF();
	map<string,int>::const_iterator pi = this->_PackageNameIndexMap.find(p);
	if ( pi==this->_PackageNameIndexMap.end() )
	{
	    ASSERTP(this->recognizesPackage(p),"I do not recognize package: "+p);
	}
	this->selectPackage(this->_Roots._Packages[pi->second]);
    }






    void Lisp_O::throwIfBuiltInClassesNotInitialized()
    {_G();
	if ( this->_BuiltInClassesInitialized ) return;
	SIMPLE_ERROR(BF("Cpp-classes are not initialized"));
    }




    Path_sp Lisp_O::translateLogicalPathname(T_sp obj)
    {_G();
	if ( Str_sp logicalPathName = obj.asOrNull<Str_O>() )
	{
	    string fileName = logicalPathName->get();
	    return Path_O::create(fileName);
	    SIMPLE_ERROR(BF("include "+fileName+" error, file does not exist"));
	} else
	{
	    SIMPLE_ERROR(BF("Finish implementing Lisp_O::translateLogicalPathname"));
	}
    }



    Path_sp Lisp_O::translateLogicalPathnameUsingPaths(T_sp obj)
    {_G();
	if ( Str_sp logicalPathName = obj.asOrNull<Str_O>() )
	{
	    string fileName = logicalPathName->get();
	    LOG(BF("Looking for file: %s") % fileName.c_str()  );
	    LOG(BF("Looking in current directory"));
	    boost_filesystem::path onePath("./");
	    onePath /= fileName;
	    if ( boost_filesystem::exists(onePath) )
	    {
		return Path_O::create(onePath.string());
	    }
	    Symbol_sp pathSym = _sym_STARPATHSTAR;
	    Cons_sp pathList = pathSym->symbolValue().as_or_nil<Cons_O>();
	    LOG(BF("PATH variable = %s") % _rep_(pathList).c_str()  );
	    while ( pathList.notnilp() )
	    {
		boost_filesystem::path onePath(oCar(pathList).as<Str_O>()->get());
		onePath /= fileName;
		LOG(BF("Checking path[%s]") % onePath.string() );
		if ( boost_filesystem::exists(onePath) )
		{
		    return Path_O::create(onePath.string());
		}
		pathList = cCdr(pathList);
	    }
	    SIMPLE_ERROR(BF("include "+fileName+" error, file does not exist"));
	} else
	{
	    SIMPLE_ERROR(BF("Finish implementing Lisp_O::translateLogicalPathname"));
	}
    }


    uint Lisp_O::nextEnvironmentId()
    {
	this->_EnvironmentId++;
	return this->_EnvironmentId;
    }

#define DLINE() printf("%s:%d debug\n", __FILE__, __LINE__);

    void Lisp_O::parseCommandLineArguments(int argc,char* argv[], bool compileInputFile)
    {_G();
        int endArg = argc;
        for ( int i=0; i<argc; ++i ) {
            if ( strcmp(argv[i],"--") == 0 ) {
                endArg = i;
            }
        }

	//
	// Pass whatever is left over to the Lisp environment
	//
	LOG(BF("Parsing what is left over into lisp environment arguments") );
        gctools::Vec0<T_sp> vargs;
        for ( int j(endArg+1); j<argc; ++j ) {
            vargs.push_back(Str_O::create(argv[j]));
        }
	VectorObjects_sp args = VectorObjects_O::create(vargs);
	LOG(BF(" Command line arguments are being set in Lisp to: %s") % _rep_(args) );
	SYMBOL_EXPORT_SC_(CorePkg,STARcommandLineArgumentsSTAR);
	_sym_STARcommandLineArgumentsSTAR->defparameter(args);

	CommandLineOptions options(endArg,argv);

	Cons_sp features = cl::_sym_STARfeaturesSTAR->symbolValue().as_or_nil<Cons_O>();
	for ( int i=0; i<options._Features.size(); ++i )
	{
	    features = Cons_O::create(_lisp->internKeyword(lispify_symbol_name(options._Features[i])),features);
	}
        features = Cons_O::create(_lisp->internKeyword("CLASP"),features);
#ifdef _TARGET_OS_DARWIN
        features = Cons_O::create(_lisp->internKeyword("DARWIN"),features);
        features = Cons_O::create(_lisp->internKeyword("BSD"),features);
        features = Cons_O::create(_lisp->internKeyword("OS-UNIX"),features);
        features = Cons_O::create(_lisp->internKeyword("UNIX"),features);
#endif
#ifdef _TARGET_OS_LINUX
        features = Cons_O::create(_lisp->internKeyword("UNIX"),features);
        features = Cons_O::create(_lisp->internKeyword("OS-UNIX"),features);
        features = Cons_O::create(_lisp->internKeyword("LINUX"),features);
#endif
#ifdef VARARGS
	features = Cons_O::create(_lisp->internKeyword("VARARGS"),features);
#endif
#ifdef POLYMORPHIC_SMART_PTR
	features = Cons_O::create(_lisp->internKeyword("POLYMORPHIC-SMART-PTR"),features);
#endif
#ifdef _DEBUG_BUILD
        features = Cons_O::create(_lisp->internKeyword("DEBUG-BUILD"),features);
#else // _RELEASE_BUILD
        features = Cons_O::create(_lisp->internKeyword("RELEASE-BUILD"),features);
#endif
#ifdef USE_SHARP_EQUAL_HASH_TABLES
	features = Cons_O::create(_lisp->internKeyword("USE-SHARP-EQUAL-HASH-TABLES"),features);
#endif
#ifdef USE_REFCOUNT
        features = Cons_O::create(_lisp->internKeyword("USE-REFCOUNT"),features);
#endif
#ifdef USE_BOEHM
        features = Cons_O::create(_lisp->internKeyword("USE-BOEHM"),features);
#endif
#ifdef USE_MPS
        // Informs CL that MPS is being used
        features = Cons_O::create(_lisp->internKeyword("USE-MPS"),features);
#ifdef USE_TAGGED_PTR_P0
// USE_TAGGED_PTR_P0  determines whether a p0 pointer,  the most-derived-pointer is stored
//         in the tagged_ptr.   If you turn it on then tagged_ptr uses twice as much
//         memory
        features = Cons_O::create(_lisp->internKeyword("USE-TAGGED-PTR-P0"), features);
#endif // USE_TAGGED_PTR_P0
#ifdef USE_AMC_POOL
        // Informs that the Automatic-Mostly-Copying Pool is being used
        printf("%s:%d  USE-AMC-POOL is turned on\n", __FILE__, __LINE__ );
        features = Cons_O::create(_lisp->internKeyword("USE-AMC-POOL"), features);
#endif
#endif

	cl::_sym_STARfeaturesSTAR->setf_symbolValue(features);

	SYMBOL_EXPORT_SC_(CorePkg,STARprintVersionOnStartupSTAR);
	_sym_STARprintVersionOnStartupSTAR->defparameter(_lisp->_boolean(options._Version));
	SYMBOL_EXPORT_SC_(CorePkg,STARsilentStartupSTAR);
	_sym_STARsilentStartupSTAR->defparameter(_lisp->_boolean(options._SilentStartup));

//	this->_FunctionName = execName;
	this->_RCFileName = "sys:" KERNEL_NAME ";init.lsp";

        this->_IgnoreInitImage = options._DontLoadImage;
	this->_IgnoreInitLsp = options._DontLoadInitLsp;

        SYMBOL_EXPORT_SC_(CorePkg,STARcommandLineLoadEvalSequenceSTAR);
        Cons_sp loadEvals = _Nil<Cons_O>();
        for ( auto it : options._LoadEvalList ) {
            Cons_sp one;
            if ( it.first==cloEval ) {
                one = Cons_O::create(kw::_sym_eval,Str_O::create(it.second));
            } else {
                one = Cons_O::create(kw::_sym_load,Str_O::create(it.second));
            }
            loadEvals = Cons_O::create(one,loadEvals);
        }
        _sym_STARcommandLineLoadEvalSequenceSTAR->defparameter(cl_nreverse(loadEvals));

        this->_Interactive = options._Interactive;
        if ( this->_Interactive ) {
            Cons_sp features = cl::_sym_STARfeaturesSTAR->symbolValue().as_or_nil<Cons_O>();
            features = Cons_O::create(KW("interactive"),features);
            cl::_sym_STARfeaturesSTAR->setf_symbolValue(features);
        }
        if ( options._NoRc ) {
            Cons_sp features = cl::_sym_STARfeaturesSTAR->symbolValue().as_or_nil<Cons_O>();
            features = Cons_O::create(KW("no-rc"),features);
            cl::_sym_STARfeaturesSTAR->setf_symbolValue(features);
        }


	if (options._GotRandomNumberSeed) {
	    seedRandomNumberGenerators(options._RandomNumberSeed);
	} else {
	    seedRandomNumberGenerators(this->mpiRank());
	}
	if ( options._HasImageFile ) {
            SYMBOL_EXPORT_SC_(CorePkg,STARcommandLineImageSTAR);
	    _sym_STARcommandLineImageSTAR->defparameter(cl_pathname(Str_O::create(options._ImageFile)));
        } else {
            _sym_STARcommandLineImageSTAR->defparameter(core_startupImagePathname());
        }
	LOG(BF("lisp->_ScriptInFile(%d)  lisp->_FileNameOrCode(%s)") % this->_ScriptInFile % this->_FileNameOrCode );
    }








    T_mv Lisp_O::readEvalPrint(T_sp inputStream, Environment_sp environ, bool printResults)
    {_OF();
	T_mv result = Values(_Nil<T_O>());
	Stream_sp sin = coerce::inputStreamDesignator(inputStream);
	DynamicScopeManager scopeCurrentLineNumber(_sym_STARcurrentSourceFileInfoSTAR,af_sourceFileInfo(sin));
	while (1)
	{
	    TRY()
	    {
		_sym_STARcurrentLinenoSTAR->setf_symbolValue(Fixnum_O::create(clasp_input_lineno(sin)));
		_sym_STARcurrentColumnSTAR->setf_symbolValue(Fixnum_O::create(clasp_input_column(sin)));
		T_sp expression = read_lisp_object(sin,false,_Unbound<T_O>(),false);
		if ( expression.unboundp() ) break;
//		TopLevelIHF frame(_lisp->invocationHistoryStack(),expression);
//		_lisp->invocationHistoryStack().setExpressionForTop(expression);
//		_lisp->invocationHistoryStack().setActivationFrameForTop(_Nil<ActivationFrame_O>());
//		PushCodeStack codeStack(expression,_Nil<Environment_O>(),_lisp);
		if ( _sym_STARechoReplReadSTAR->symbolValue().isTrue() )
		{
		    string suppress;
		    if ( cl::_sym_STARread_suppressSTAR->symbolValue().isTrue() )
		    {
			suppress = "SUPPRESSED";
			if ( expression.notnilp() )
			{
			    SIMPLE_ERROR(BF("*read-suppress* is true but the following expression was read: %s") % _rep_(expression) );
			}
		    }
		    this->print(BF(";;--read-%s-------------\n#|\n%s\n|#----------\n") % suppress.c_str() % _rep_(expression) );
		}
		_BLOCK_TRACEF(BF("---REPL read[%s]") % expression->__repr__() );
		if ( af_keywordP(expression) ) {
		    ql::list tplCmd;
		    tplCmd << expression;
		    while ( T_sp exp = read_lisp_object(sin,false,_Unbound<T_O>(),false) ) {
			if (exp.unboundp()) break;
			tplCmd << exp;
		    }
		    if ( _sym_STARtopLevelCommandHookSTAR->symbolValue().notnilp() ) {
			eval::funcall(_sym_STARtopLevelCommandHookSTAR->symbolValue(),tplCmd.cons());
		    } else {
			af_bformat(_lisp->_true(),"Cannot interpret %s - define core::*top-level-command-hook*", Cons_O::createList(tplCmd.cons()));
		    }
		} else if ( expression.notnilp() )
		{
		    result = eval::af_topLevelEvalWithEnv(expression,environ);
                    gctools::Vec0<core::T_sp/*,gctools::RootedGCHolder*/> vresults;
                    vresults.resize(result.number_of_values());
                    if ( result.number_of_values() > 0 ) {
                        vresults[0] = result;
                        if ( result.number_of_values() > 1 ) {
                            for ( int i(1); i<result.number_of_values(); ++i ) {
                                vresults[i] = result.valueGet(i);
                            }
                        }
                    }
		    if ( printResults )
		    {
			for ( int i(0); i<vresults.size(); i++ )
			{
			    T_sp obj = vresults[i];
//			    this->print(BF("; --> %s\n")% _rep_(obj));
			    eval::funcall(cl::_sym_print,obj);
			}
		    }
		}
	    }
	    catch (Condition& err)
	    {
		// Catch condition from reader means just ask for another s-exp if
		// interactive and terminate if batch
		this->print(BF("%s:%d Caught Condition from reader") % __FILE__ % __LINE__ );
		exit(1);
//		this->reportConditionAndTerminateProgramIfBatch(err.conditionObject());
	    }
	    catch (DebuggerSaysAbortToRepl& abort)
	    {
		this->print(BF("%s:%d aborted to repl:") % __FILE__ % __LINE__ );
		// Do nothing
	    }
	    catch (HardError& err)
	    {
		this->print(BF("Should never happen - catch and convert to Condition below - HardError: %s")
			    % err.message() );
		IMPLEMENT_ME();
//		this->enterDebugger();
	    }
	}
	return result;
    }



    T_mv Lisp_O::readEvalPrintString(const string& code, Environment_sp environ, bool printResults )
    {_OF();
	StringInputStream_sp sin = StringInputStream_O::make(code);
	DynamicScopeManager scopeCurrentLineNumber(_sym_STARcurrentLinenoSTAR,Fixnum_O::create(clasp_input_lineno(sin)));
	DynamicScopeManager scopeCurrentColumn(_sym_STARcurrentColumnSTAR,Fixnum_O::create(clasp_input_column(sin)));
	T_mv result = this->readEvalPrint(sin,environ,printResults);
	cl_close(sin);
	return result;
    }






#define ARGS_af_lowLevelRepl "()"
#define DECL_af_lowLevelRepl ""
#define DOCS_af_lowLevelRepl "lowLevelRepl - this is a built in repl for when the top-level repl isn't available"
    void af_lowLevelRepl()
    {_G();
	_lisp->readEvalPrintInteractive();
    };



    void Lisp_O::readEvalPrintInteractive()
    {_OF();
	Cons_sp expression;
//	TopLevelIHF topFrame(_lisp->invocationHistoryStack(),_Nil<T_O>());
	DynamicScopeManager scopeCurrentLineNumber(_sym_STARcurrentLinenoSTAR,Fixnum_O::create(0));
	DynamicScopeManager scopeCurrentColumn(_sym_STARcurrentColumnSTAR,Fixnum_O::create(0));
	while(1) {
	  string line;
	  stringstream prompt;
	  prompt << cl::_sym_STARpackageSTAR->symbolValue().as<Package_O>()->getName() << ">>> ";
	  line = myReadLine(prompt.str());
	  StringInputStream_sp sin = StringInputStream_O::make(line);
	  this->readEvalPrint(sin,_Nil<Environment_O>(),true);
	}
    }






#define ARGS_af_stackUsed "()"
#define DECL_af_stackUsed ""
#define DOCS_af_stackUsed "stackUsed"
    uint af_stackUsed()
    {_G();
	int x;
	char* xaddr = (char*)(&x);
	uint stack = (uint)(_lisp->_StackTop-xaddr);
	return stack;
    };


    static bool global_invokedInternalDebugger = false;

    struct ExceptionSafeResetInvokedInternalDebugger {
        ExceptionSafeResetInvokedInternalDebugger()
        {
            global_invokedInternalDebugger = true;
        };
        virtual ~ExceptionSafeResetInvokedInternalDebugger()
        {
            global_invokedInternalDebugger = false;
        }
    };


#define ARGS_af_stackSizeWarning "(arg)"
#define DECL_af_stackSizeWarning ""
#define DOCS_af_stackSizeWarning "stackSizeWarning"
    void af_stackSizeWarning(uint stackUsed)
    {_G();
        if ( !global_invokedInternalDebugger ) {
            printf("%s:%d Stack is getting full currently at %u bytes - warning at %u bytes\n",
                   __FILE__, __LINE__,
                   stackUsed, _lisp->_StackWarnSize );
            ExceptionSafeResetInvokedInternalDebugger safe;
            af_invokeInternalDebugger(_Nil<core::T_O>());
        }
    };





#define ARGS_af_stackMonitor "()"
#define DECL_af_stackMonitor ""
#define DOCS_af_stackMonitor "monitor stack for problems - warn if getting too large"
    void af_stackMonitor()
    {_G();
	uint stackUsed = af_stackUsed();
	if ( stackUsed > _lisp->_StackSampleMax ) _lisp->_StackSampleMax = stackUsed;
	if ( _lisp->_StackSampleSize > 0 )
	{
	    _lisp->_StackSampleCount++;
	    if ( _lisp->_StackSampleCount >= _lisp->_StackSampleSize )
	    {
		printf("STACK-USED samples: %u high-water: %u     %s:%d\n",
		       _lisp->_StackSampleSize,
		       _lisp->_StackSampleMax,
		       __FILE__, __LINE__ );
		_lisp->_StackSampleCount = 0;
		_lisp->_StackSampleMax = 0;
	    }
	}
	if ( stackUsed > _lisp->_StackWarnSize )
	{
	    af_stackSizeWarning(stackUsed);
	}
    };







#define ARGS_af_setupStackMonitor "(&key warn-size sample-size)"
#define DECL_af_setupStackMonitor ""
#define DOCS_af_setupStackMonitor "setupStackMonitor"
    void af_setupStackMonitor(Fixnum_sp warnSize, Fixnum_sp sampleSize )
    {_G();
	if ( !warnSize.nilp() )
	{
	    _lisp->_StackWarnSize = warnSize->get();
	}
	if ( !sampleSize.nilp() )
	{
	    _lisp->_StackSampleSize = sampleSize->get();
	    _lisp->_StackSampleCount = 0;
	    _lisp->_StackSampleMax = 0;
	}
    };









#define ARGS_af_exit "(&optional (exit-value 0))"
#define DECL_af_exit ""
#define DOCS_af_exit "exit"
    void af_exit(int exitValue)
    {_G();
	throw(ExitProgram(exitValue));
    };

#define ARGS_af_quit "(&optional (exit-value 0))"
#define DECL_af_quit ""
#define DOCS_af_quit "quit"
    void af_quit(int exitValue)
    {_G();
	throw(ExitProgram(exitValue));
    };








#define ARGS_af_acons "(key datum alist)"
#define DECL_af_acons ""
#define DOCS_af_acons "acons"
    Cons_sp af_acons(T_sp key, T_sp val, Cons_sp alist)
    {_G();
	Cons_sp acons = Cons_O::create(key,val);
	return Cons_O::create(acons,alist);
    }






#define ARGS_af_assoc "(item alist &key key test test-not)"
#define DECL_af_assoc ""
#define DOCS_af_assoc "assoc"
    Cons_sp af_assoc(T_sp item, Cons_sp alist, T_sp key, T_sp test, T_sp test_not)
    {_G();
	if ( alist.nilp() ) return alist;
	return alist->assoc(item,key,test,test_not);
    }


#define ARGS_af_member "(item list &key key test test-not)"
#define	DECL_af_member	""
#define	DOCS_af_member	"See CLHS member"
#define	FILE_af_member	__FILE__
#define	LINE_af_member	__LINE__
    Cons_sp af_member(T_sp item, T_sp tlist, T_sp key, T_sp test, T_sp test_not)
    {_G();
	if ( tlist.nilp() ) return _Nil<Cons_O>();
	if ( Cons_sp list = tlist.asOrNull<Cons_O>() ) {
	    return(list->member(item,key,test,test_not));
	}
	QERROR_WRONG_TYPE_NTH_ARG(2,tlist,cl::_sym_list);
	UNREACHABLE();
    }

#define ARGS_af_memberTest "(item list &key key test test-not)"
#define	DECL_af_memberTest	""
#define	DOCS_af_memberTest	"See CLHS memberTest"
#define	FILE_af_memberTest	__FILE__
#define	LINE_af_memberTest	__LINE__
    Cons_sp af_memberTest(T_sp item, Cons_sp list, T_sp key, T_sp test, T_sp test_not)
    {_G();
	if ( list.nilp() ) return list;
	return(list->member(item,key,test,test_not));
    }


#define ARGS_af_member1 "(item list test test-not key)"
#define	DECL_af_member1	""
#define	DOCS_af_member1	"Like member but if a key function is provided then apply it to the item. See ecl::list.d::member1"
#define	FILE_af_member1	__FILE__
#define	LINE_af_member1	__LINE__
    Cons_sp af_member1(T_sp item, Cons_sp list, T_sp test, T_sp test_not, T_sp key)
    {_G();
	if ( list.nilp() ) return list;
	return list->member1(item,key,test,test_not);
    }

















#define	ARGS_ext_setenv	"(name value)"
#define	DECL_ext_setenv	""
#define	DOCS_ext_setenv	"Set environment variable NAME to VALUE"
    void ext_setenv(Str_sp name, Str_sp value)
    {_G();
	setenv(name->get().c_str(),value->get().c_str(),1);
    }



#define	ARGS_ext_getenv	"(name)"
#define	DECL_ext_getenv	""
#define	DOCS_ext_getenv	"Get environment variable NAME"
    T_sp ext_getenv(Str_sp name)
    {_G();
	string s = name->get();
        char* e = getenv(s.c_str());
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
    T_mv af_getline(Str_sp prompt)
    {_G();
	string res;
#ifdef	READLINE
	char* line_read;
	/* Get a line from the user. */
	line_read = readline(prompt->get().c_str());
	if ( line_read != NULL )
	{
	    if (*line_read) add_history(line_read);
	    res = line_read;
	    free(line_read);
	}
#else
	if ( prompt->get() != "" )
	{
	    _lisp->print(BF("%s ") % prompt->get() );
	    _lisp->printvFlush();
	}
	getline(cin,res);
#endif
	Str_sp result = Str_O::create(res);
	return(Values(result));
    }







/*
  __BEGIN_DOC( candoScript.general.system, subsection,system)
  \scriptCmdRet{system}{}{String::command}

  Invoke a system call using the UNIX system function call.
  __END_DOC
*/



#define ARGS_af_system "(cmd)"
#define DECL_af_system ""
#define DOCS_af_system "system"
    T_mv af_system(Str_sp cmd)
    {_G();
	string command = cmd->get();
	int ret = system(command.c_str());
	core::Fixnum_sp iret = core::Fixnum_O::create(ret);
	return(Values(iret));
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
    T_mv af_saveCando(T_sp obj, T_sp pathDesignator)
    {_G();
	Path_sp path = coerce::pathDesignator(pathDesignator);
	Stream_sp sout = cl_open(path,
				 kw::_sym_output,
				 cl::_sym_standard_char,
				 _Nil<Symbol_O>(),
				 _Nil<Symbol_O>(),
				 kw::_sym_default);
	_lisp->sprint(obj,sout);
	sout->close();
	return(Values(_Nil<T_O>()));
    }



#define ARGS_af_loadCando "(pathDesignator)"
#define DECL_af_loadCando ""
#define DOCS_af_loadCando "loadCando"
    T_mv af_loadCando(T_sp pathDesignator)
    {_G();
	Path_sp path = coerce::pathDesignator(pathDesignator);
	Stream_sp sin = cl_open(path,kw::_sym_input,cl::_sym_standard_char,_Nil<Symbol_O>(),_Nil<Symbol_O>(),kw::_sym_default);
	T_sp obj = _lisp->sread(sin.as<Stream_O>(),true,_Nil<T_O>());
	sin->close();
	return(Values(obj));
    }
#endif // defined(XML_ARCHIVE)







#define ARGS_af_findClass "(symbol &optional (errorp t) environment)"
#define DECL_af_findClass ""
#define DOCS_af_findClass "findClass"
    Class_mv af_findClass(Symbol_sp symbol, bool errorp, Environment_sp env )
    {_G();
	if ( _lisp->bootClassTableIsValid() )
	{
	    return Values(_lisp->boot_findClass(symbol,errorp));
	}
	ASSERTF(env.nilp(),BF("Handle non nil environment"));
	// Use the same global variable that ECL uses
	SYMBOL_SC_(CorePkg,STARclassNameHashTableSTAR);
	HashTable_sp classNames = _sym_STARclassNameHashTableSTAR->symbolValue().as<HashTable_O>();
	T_mv mc = classNames->gethash(symbol,_Nil<T_O>());
	Class_sp omc = mc.as_or_nil<Class_O>();
	bool foundp = mc.valueGet(1).notnilp();
	if ( !foundp )
	{
	    if (errorp)
	    {
		SIMPLE_ERROR(BF("Could not find class %s") % _rep_(symbol) );
	    }
	    return(Values(_Nil<Class_O>()));
	}
#if DEBUG_CLOS>=3
	printf("\nMLOG find-class returning class %p name--> %s\n", (void*)(result.get()), symbol->__repr__().c_str() );
#endif
	return(Values(omc));
    }





#define ARGS_af_setf_findClass "(new-value name)"
#define DECL_af_setf_findClass ""
#define DOCS_af_setf_findClass "setf_findClass"
    Class_mv af_setf_findClass(T_sp newValue, Symbol_sp name, bool errorp, Environment_sp env)
    {_G();
	if ( !af_classp(newValue) )
	{
	    SIMPLE_ERROR(BF("Classes in cando have to be subclasses of Class unlike ECL which uses Instances to represent classes - while trying to (setf find-class) of %s you gave: %s") % _rep_(name) % _rep_(newValue) );
	}
	if ( _lisp->bootClassTableIsValid() )
	{
	    return Values(_lisp->boot_setf_findClass(name,newValue.as<Class_O>()));
	}
	HashTable_sp ht = _sym_STARclassNameHashTableSTAR->symbolValue().as<HashTable_O>();
	T_sp oldClass = eval::funcall(cl::_sym_findClass, name, _Nil<T_O>());
	if ( af_classp(oldClass) )
	{
	    SIMPLE_ERROR(BF("The built-in class associated to the CL specifier %s cannot be changed") % _rep_(name) );
	} else if ( newValue.nilp() )
	{
	    ht->remhash(name);
	} else
	{
	    ht->hash_table_setf_gethash(name,newValue);
	}
	return Values(newValue.as<Class_O>());
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
    T_mv af_findFileInLispPath(Str_sp partialPath)
    {_G();
	LOG(BF("PartialPath=[%s]") % partialPath->get());
	Path_sp fullPath = _lisp->translateLogicalPathnameUsingPaths(partialPath);
	LOG(BF("fullPath is %s") % fullPath->asString());
	return(Values(fullPath));
    }




#define ARGS_af_find_package "(name_desig)"
#define DECL_af_find_package ""
#define DOCS_af_find_package "See CLHS: find-package"
    Package_mv af_find_package(T_sp name_desig)
    {_G();
	if ( af_packageP(name_desig) ) return(Values(name_desig.as<Package_O>()));
	Str_sp name = coerce::stringDesignator(name_desig);
	Package_sp pkg = _lisp->findPackage(name->get());
	return(Values(pkg));
    }






#define DOCS_af_selectPackage "selectPackage"
#define LOCK_af_selectPackage 1
#define ARGS_af_selectPackage "(package-designator)"
#define DECL_af_selectPackage ""
    void af_selectPackage(T_sp package_designator)
    {_G();
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
    T_mv af_mpi_enabled()
    {_G();
	return(Values(_lisp->_boolean(_lisp->mpiEnabled())));
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
    T_mv af_mpi_rank()
    {_G();
	return(Values(Fixnum_O::create(_lisp->mpiRank())));
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
    T_mv af_mpi_size()
    {_G();
	return(Values(Fixnum_O::create(_lisp->mpiSize())));
    }








#define ARGS_af_macroexpand_1 "(form &optional env)"
#define DECL_af_macroexpand_1 ""
#define DOCS_af_macroexpand_1 "macroexpand_1"
    T_mv af_macroexpand_1(T_sp form, Environment_sp env)
    {_G();
	Function_sp expansionFunction = _Nil<Function_O>();
	if ( Cons_sp cform = form.asOrNull<Cons_O>() )
	{
	    T_sp head = oCar(cform);
	    if ( af_symbolp(head) )
	    {
		Symbol_sp headSymbol = head.as<Symbol_O>();
		if ( _lisp->specialFormOrNil(headSymbol).nilp() )
		{
		    Function_sp func = af_interpreter_lookup_macro(headSymbol,env);
		    if ( func.notnilp() && func->closure->macroP() )
		    {
			expansionFunction = func;
		    }
		}
	    }
	} else if ( Symbol_sp sform = form.asOrNull<Symbol_O>() )
	{
	    Function_sp func = af_interpreter_lookup_symbol_macro(sform,env);
	    if ( func.notnilp() )
	    {
		expansionFunction = func;
	    }
	}
	if ( expansionFunction.notnilp() )
	{
	    T_sp macroexpandHook = cl::_sym_STARmacroexpand_hookSTAR->symbolValue();
	    Function_sp hookFunc = coerce::functionDesignator(macroexpandHook);
            ValueFrame_sp macroHookArgs = ValueFrame_O::create_fill_args(_Nil<ActivationFrame_O>(),expansionFunction,form,env);
	    InvocationHistoryFrame _frame(hookFunc->closure,macroHookArgs);
	    T_sp expanded = eval::applyToActivationFrame(hookFunc,macroHookArgs); // eval::applyFunctionToActivationFrame(hookFunc,macroHookArgs);
            if ( _lisp->sourceDatabase().notnilp() ) {
                _lisp->sourceDatabase()->duplicateSourceInfoForMacroExpansion(form,expansionFunction,expanded);
            }
	    return(Values(expanded,_lisp->_true()) );
	}
	return(Values(form,_lisp->_false()) );
    }




#define ARGS_af_macroexpand "(form &optional env)"
#define DECL_af_macroexpand ""
#define DOCS_af_macroexpand "macroexpand"
    T_mv af_macroexpand(T_sp form, Environment_sp env)
    {_G();
	bool sawAMacro = false;
	bool expandedMacro = false;
	uint macroExpansionCount = 0;
	if (_sym_STARdebugMacroexpandSTAR->symbolValue().isTrue())
	{
	    printf("%s:%d - macroexpanding --> %s\n", __FILE__, 2551, _rep_(form).c_str() );
	}
	T_sp cur = form;
	do {
	    T_mv mv = af_macroexpand_1(cur,env);
	    cur = mv;
	    sawAMacro = mv.valueGet(1).as<T_O>().isTrue();
	    expandedMacro |= sawAMacro;
	    macroExpansionCount++;
	    if ( macroExpansionCount > 100 )
	    {
		SIMPLE_ERROR(BF("Macro expansion happened %d times - You may have a macro expansion infinite loop") % macroExpansionCount );
	    }
	} while ( sawAMacro );
	if (_sym_STARdebugMacroexpandSTAR->symbolValue().isTrue())
	{
	    printf("%s:%d -     after macroexpanding --> %s\n", __FILE__, 2565, _rep_(cur).c_str() );
	}
	return(Values(cur,_lisp->_boolean(expandedMacro)) );
    };






    void searchForApropos(Cons_sp packages,const string& raw_substring, bool print_values)
    {_G();
	string substring = lispify_symbol_name(raw_substring);
	FindApropos apropos(substring);
	LOG(BF("Searching for symbols apropos to(%s)") % substring);
	for ( Cons_sp cur = packages; cur.notnilp(); cur=cCdr(cur) )
	{
	    Package_sp pkg = oCar(cur).as<Package_O>();
	    pkg->mapExternals(&apropos);
	    pkg->mapInternals(&apropos);
	}
        apropos._symbols->mapHash( [&print_values] (T_sp key, T_sp dummy) {
                stringstream ss;
                Symbol_sp sym = key.as<Symbol_O>();
                ss << (BF("%50s") % (sym)->fullName()).str();
                if ( (sym)->specialP() || (sym)->fboundp() )
                {
                    if ( (sym)->fboundp() )
                    {
                        ss << " ";
                        ss << af_classOf(af_symbolFunction((sym)))->classNameAsString();
                        if ( af_symbolFunction(sym)->closure->macroP() )
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
            } );
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
    T_mv af_apropos(Str_sp string_desig, T_sp package_desig)
    {_G();
	string substring = coerce::stringDesignator(string_desig)->get();
	Cons_sp packages(_Nil<Cons_O>());
	if ( package_desig.nilp() )
	{
	    packages = _lisp->allPackagesAsCons();
	} else
	{
	    Package_sp pkg = coerce::packageDesignator(package_desig);
	    packages = Cons_O::create(pkg,_Nil<Cons_O>());
	}
	searchForApropos(packages,substring,false);
	return(Values(_Nil<T_O>()));
    }






/*
  __BEGIN_DOC(candoScript.general.funcall,funcall)
  \scriptCmdRet{funcall}{}{Function arg1 arg2 ...}

  Evaluate the function with the arguments.
  __END_DOC
*/




#define ARGS_af_funcall "(function_desig &rest args)"
#define DECL_af_funcall ""
#define DOCS_af_funcall "See CLHS: funcall"
    T_mv af_funcall(T_sp function_desig, Cons_sp args)
    {_G();
	Function_sp func = coerce::functionDesignator(function_desig);
        if ( func.nilp() ) {
            ERROR_UNDEFINED_FUNCTION(function_desig);
        }
	Cons_sp passArgs = args;
	ValueFrame_sp frame(ValueFrame_O::create(passArgs,_Nil<ActivationFrame_O>()));
	return eval::applyToActivationFrame(func,frame); // func->INVOKE(frame->length(),frame->argArray());//return eval::applyFunctionToActivationFrame(func,frame);
    }





/*
  __BEGIN_DOC(candoScript.general.apply,apply)
  \scriptCmdRet{apply}{}{Function argList}

  Evaluate the function with the argument list.
  __END_DOC
*/



#define ARGS_af_apply "(head &rest args)"
#define DECL_af_apply ""
#define DOCS_af_apply "apply"
    T_mv af_apply(T_sp head, T_sp args)
    {_G();
	/* Special case when apply is called with one arg and that arg is an ActivationFrame
	   APPLY directly to that ActivationFrame */
	int lenArgs = cl_length(args);
	if ( lenArgs == 0 ) {
	    SIMPLE_ERROR(BF("Illegal number of arguments %d") % lenArgs );
	}
	if ( lenArgs == 1 && oCar(args).notnilp() )
	{
	    Function_sp func = coerce::functionDesignator(head);
            if ( func.nilp() ) {
                ERROR_UNDEFINED_FUNCTION(head);
            }
	    if ( ActivationFrame_sp singleFrame = oCar(args).asOrNull<ActivationFrame_O>() )
	    {
		return eval::applyToActivationFrame(func,singleFrame); // return func->INVOKE(singleFrame->length(),singleFrame->argArray()); // return eval::applyFunctionToActivationFrame(func,singleFrame);
	    }
	}
	T_sp last = oCar(cl_last(args));
	if ( !af_listp(last) ) {
	    SIMPLE_ERROR(BF("Last argument is not a list"));
	}
	int lenFirst = lenArgs-1;
	int lenRest = cl_length(last);
	int nargs = lenFirst + lenRest;
	ValueFrame_sp frame(ValueFrame_O::create(nargs,_Nil<ActivationFrame_O>()));
	T_sp obj = args;
	for ( int i(0); i<lenFirst; ++i ) {
	    frame->operator[](i) = oCar(obj);
	    obj = oCdr(obj);
	}
        T_sp cur = last;
	for ( int i(lenFirst); i<nargs; ++i ) {
	    frame->operator[](i) = oCar(cur);
	    cur = oCdr(cur);
	}
	Function_sp func = coerce::functionDesignator(head);
	return eval::applyToActivationFrame(func,frame);
    }










    class	OrderByLessThan
    {
    public:
	bool operator()(T_sp x, T_sp y )
	{
	    return x->operator<(y);
	}
    };





#define ARGS_af_sorted "(unsorted)"
#define DECL_af_sorted ""
#define DOCS_af_sorted "Sort the list in ascending order using operator< and return the sorted list"
    Cons_sp af_sorted(Cons_sp unsorted)
    {_G();
        gctools::Vec0<T_sp/*,gctools::RootedGCHolder*/>     sorted;
	if ( cl_length(unsorted) == 0 ) return _Nil<Cons_O>();
	fillVec0FromCons(sorted,unsorted);
	OrderByLessThan orderer;
	sort::quickSort(sorted.begin(),sorted.end(),orderer);
        Cons_sp result;
	result = asCons(sorted);
	return result;
    }


#if 0 // refactored
    Cons_mv af_sorted(Cons_sp unsorted)
    {_G();
	VectorObjects_sp sorted(VectorObjects_O::create());
	if ( cl_length(unsorted) == 0 ) return(Values(_Nil<Cons_O>()));
	sorted->fillFromCons(unsorted);
	OrderByLessThan orderer;
	sort::quickSort(sorted->begin(),sorted->end(),orderer,_lisp);
	Cons_sp result = sorted->asCons();
	return(Values(result));
    }
#endif

    class	OrderBySortFunction
    {
    private:
	Function_sp _SortFunction;
	Cons_sp 	_args;
    public:
	OrderBySortFunction(Function_sp proc)
	{
	    this->_SortFunction = proc;
	    this->_args = Cons_O::createList(_Nil<T_O>(),_Nil<T_O>());
	}
	bool operator()(T_sp x, T_sp y )
	{
	    return eval::funcall(this->_SortFunction,x,y);
	}
    };





#define ARGS_af_sort "(sequence predicate)"
#define DECL_af_sort ""
#define DOCS_af_sort "Like CLHS: sort but does not support key"
    T_sp af_sort(Cons_sp sequence, T_sp predicate )
    {_G();
        gctools::Vec0<T_sp/*,gctools::RootedGCHolder*/> sorted;
	Function_sp sortProc = coerce::functionDesignator(predicate);
	LOG(BF("Unsorted data: %s") % _rep_(sequence) );
	if ( cl_length(sequence) == 0 ) return _Nil<Cons_O>();
	fillVec0FromCons(sorted,sequence);
	LOG(BF("Sort function: %s") % _rep_(sortProc) );
	OrderBySortFunction orderer(sortProc);
	sort::quickSort(sorted.begin(),sorted.end(),orderer);
	Cons_sp result = asCons(sorted);
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
    T_mv af_sourceFileName()
    {_G();
	Cons_sp ppcons;
	InvocationHistoryFrame* frame = _lisp->invocationHistoryStack().top();
	string sourcePath = frame->sourcePathName();
	Path_sp path = Path_O::create(sourcePath);
	Path_sp parent_path = path->parent_path();
	return Values(Str_O::create(path->fileName()),Str_O::create(parent_path->asString()));
    }


#define ARGS_af_sourceLineColumn "()"
#define DECL_af_sourceLineColumn ""
#define DOCS_af_sourceLineColumn "sourceLineColumn"
    T_mv af_sourceLineColumn()
    {_G();
	InvocationHistoryFrame* frame = _lisp->invocationHistoryStack().top();
	return Values(Fixnum_O::create(frame->lineNumber()),Fixnum_O::create(frame->column()));
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
    Path_mv af_script_dir()
    {_G();
	Path_sp dir = Path_O::create(_lisp->bundle().getLispDir());
	return(Values(dir));
    }


#define ARGS_af_libraryPath "(&optional rel-path)"
#define DECL_af_libraryPath ""
#define DOCS_af_libraryPath "Returns the absolute path to the library directory - if rel-path is not nil then it prepends the library directory path to rel-path and returns that"
    Path_mv af_libraryPath(T_sp relPathDesignator)
    {_G();
	if ( relPathDesignator.notnilp() )
	{
	    Path_sp relPath = coerce::pathDesignator(relPathDesignator);
	    boost_filesystem::path lp = _lisp->bundle().getLibDir();
	    lp /= relPath->getPath();
	    return(Values(Path_O::create(lp)));
	}
	return(Values(Path_O::create(_lisp->bundle().getLibDir())));
    }


#define ARGS_af_lispCodePath "(&optional rel-path)"
#define DECL_af_lispCodePath ""
#define DOCS_af_lispCodePath "Returns the absolute path to the lisp code directory - if rel-path is not nil then it prepends the directory path to rel-path and returns that"
    Path_mv af_lispCodePath(T_sp relPathDesignator)
    {_G();
	if ( relPathDesignator.notnilp() )
	{
	    Path_sp relPath = coerce::pathDesignator(relPathDesignator);
	    boost_filesystem::path lp = _lisp->bundle().getLibDir();
	    lp /= relPath->getPath();
	    return(Values(Path_O::create(lp)));
	}
	return(Values(Path_O::create(_lisp->bundle().getLibDir())));
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
    Path_mv af_database_dir()
    {_G();
	Path_sp dir = Path_O::create(_lisp->bundle().getDatabasesDir());
	return(Values(dir));
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
    T_mv af_setCurrentWorkingDirectory(Str_sp dir)
    {_G();
	Path_sp cwd = Path_O::create(dir->get());
	_lisp->setCurrentWorkingDirectory(cwd);
	return(Values(dir));
    }


/*
  __BEGIN_DOC(candoScript.general.isTopLevelScript)

  Return a true if this is a top level script or false if its an include file.
  __END_DOC
*/




#define ARGS_af_isTopLevelScript "()"
#define DECL_af_isTopLevelScript ""
#define DOCS_af_isTopLevelScript "isTopLevelScript"
    T_mv af_isTopLevelScript()
    {_G();
	LOG(BF("isTopLevelScript = %d") % _lisp->getRequireLevel()  );
	T_sp top = _lisp->_boolean(_lisp->getRequireLevel() == 0);
	return(Values(top));
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
    void af_debugLogOn()
    {_G();
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
    void af_debugLogOff()
    {_G();
	_lisp->debugLog().setSuppressMessages(true);
    }






/*
  __BEGIN_DOC(candoScript.general.export,export)
  \scriptCmd{export}{symbols...}

  Tell the symbols that they can be exported.

  __END_DOC
*/
#define ARGS_af_export "(symDes &optional (packageDes *package*))"
#define DECL_af_export ""
#define DOCS_af_export "CLHS: export"
    void af_export(T_sp symDes,T_sp packageDes)
    {_G();
	Cons_sp symbols = coerce::listOfSymbols(symDes);
	Package_sp package = coerce::packageDesignator(packageDes);
	package->_export(symbols);
    }






#define ARGS_af_exportToPython "(symbolsDesig)"
#define DECL_af_exportToPython ""
#define DOCS_af_exportToPython "exportToPython"
    void af_exportToPython(T_sp symbolsDesig)
    {_G();
	Cons_sp symbols = coerce::listOfSymbols(symbolsDesig);
	for ( Cons_sp cur = symbols; cur.notnilp(); cur = cCdr(cur) )
	{
	    Symbol_sp one = oCar(cur).as<Symbol_O>();
	    LOG(BF("Exporting symbol[%s] to python") % _rep_(one) );
	    _lisp->exportToPython(one);
	}
    }



#define ARGS_af_intern "(symbol_name &optional (package-desig *package*))"
#define DECL_af_intern ""
#define DOCS_af_intern "See CLHS: intern"
    T_mv af_intern(Str_sp symbol_name, T_sp package_desig)
    {_G();
	Package_sp package = coerce::packageDesignator(package_desig);
	return(package->intern(symbol_name->get()));
    }







#define ARGS_af_universalErrorHandler "(continue-string datum initializers)"
#define DECL_af_universalErrorHandler ""
#define DOCS_af_universalErrorHandler "universalErrorHandler"
    T_mv af_universalErrorHandler(T_sp continueString, T_sp datum, Cons_sp initializers)
    {_G();
	if ( af_stringP(datum) ) {
	    af_format(_lisp->_true(),datum,initializers);
	} else {
	    stringstream ss;
	    ss << "datum: " << _rep_(datum) << " " << _rep_(initializers);
	    printf("%s\n", ss.str().c_str() );
	}
	dbg_hook("universalErrorHandler");
	af_invokeInternalDebugger(_Nil<T_O>());
	exit(1);
    };




#define ARGS_af_invokeInternalDebugger "(&optional condition)"
#define DECL_af_invokeInternalDebugger ""
#define DOCS_af_invokeInternalDebugger "invokeInternalDebugger"
    void af_invokeInternalDebugger(T_sp condition)
    {_G();
	stringstream ss;
	if ( condition.nilp() )
	{
	    LispDebugger debugger;
  	    debugger.invoke();
	} else
	{
	    _lisp->print(BF("%s:%d af_invokeInternalDebugger --> %s")
			 % __FILE__
			 % __LINE__
			 % _rep_(condition).c_str() );
	    LispDebugger debugger(condition);
	    debugger.invoke();
	}
    };



#define ARGS_core_singleDispatchGenericFunctionTable "()"
#define DECL_core_singleDispatchGenericFunctionTable ""
#define DOCS_core_singleDispatchGenericFunctionTable "singleDispatchGenericFunctionTable"
    HashTable_sp core_singleDispatchGenericFunctionTable()
    {_G();
        return _lisp->singleDispatchGenericFunctionTable();
    };


extern "C"
{

#define ARGS_af_invokeInternalDebuggerFromGdb "()"
#define DECL_af_invokeInternalDebuggerFromGdb ""
#define DOCS_af_invokeInternalDebuggerFromGdb "invokeInternalDebuggerFromGdb"
    void af_invokeInternalDebuggerFromGdb()
    {_G();
	eval::funcall(_sym_invokeInternalDebugger);
	SIMPLE_ERROR(BF("This should never happen"));
    };

};





#define ARGS_cl_error "(datum &rest arguments)"
#define DECL_cl_error ""
#define DOCS_cl_error "See CLHS error"
    void cl_error(T_sp datum, Cons_sp initializers)
    {_G();
	int nestedErrorDepth = _sym_STARnestedErrorDepthSTAR->symbolValue().as<Fixnum_O>()->get();
	if ( nestedErrorDepth > 10 )
	{
	    // TODO: Disable this code once error handling and conditions work properly
	    // It's only here to identify errors that would cause infinite looping
	    // as we get error handling and conditions working properly
	    printf("%s:%d -- *nested-error-depth* --> %d  datum: %s\n", __FILE__, __LINE__, nestedErrorDepth, _rep_(datum).c_str() );
	    if ( initializers.notnilp() )
	    {
		printf("               initializers: %s\n", _rep_(initializers).c_str() );
	    }
	}
	++nestedErrorDepth;
	DynamicScopeManager scope(_sym_STARnestedErrorDepthSTAR,Fixnum_O::create(nestedErrorDepth));
	if ( _sym_universalErrorHandler->fboundp() )
	{
	    Function_sp fn = _sym_universalErrorHandler->symbolFunction();
	    eval::funcall(fn,_Nil<T_O>(),datum,initializers);
	}
	THROW_HARD_ERROR(BF("cl_error should never return because universal-error-handler should never return - but it did"));
    }




#define ARGS_cl_cerror "(cformat eformat &rest arguments)"
#define DECL_cl_cerror ""
#define DOCS_cl_cerror "See CLHS cerror"
    void cl_cerror(T_sp cformat, T_sp eformat, Cons_sp arguments)
    {_G();
        eval::funcall(_sym_universalErrorHandler,cformat, eformat, arguments);
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
    T_mv af_isAssignableTo(T_sp tag, Class_sp mc)
    {_G();
	LOG(BF("Checking if instances of class(%s) is assignable to variables of class(%s)")% af_classOf(tag)->className() % af_classOf(mc)->className() );
	bool io = (tag->isAssignableToByClassSymbol(mc->name()));
	return(Values(_lisp->_boolean(io)));
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
    T_mv af_isSubClassOf(Class_sp tag, Class_sp mc)
    {_G();
	LOG(BF("Checking if instances of class(%s) is assignable to variables of class(%s)")% tag->className() % mc->className() );
	bool io = tag->isSubClassOf(mc);
	return(Values(_lisp->_boolean(io)));
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
    T_mv af_repr(T_sp obj)
    {_G();
	Str_sp res = Str_O::create(_rep_(obj));
	return(Values(res));
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
    T_mv af_not(T_sp x)
    {_G();
	return(Values(_lisp->_boolean(!x.isTrue())));
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




    Symbol_sp Lisp_O::getClassSymbolForClassName(const string& name)
    {
	Class_sp mc = this->classFromClassName(name);
	Symbol_sp sym = mc->className();
	ASSERTNOTNULL(sym);
	return sym;
    }


    T_sp Lisp_O::createObjectOfClass(T_sp mc)
    {_G();
	if (af_classp(mc))
	{
	    LOG(BF("createObjectOfClass(%s)") % _rep_(mc));
	    IMPLEMENT_ME();
	    T_sp obj = mc.as<Class_O>()->allocate_newNil();
	    obj->initialize();
	    return obj;
	}
	SIMPLE_ERROR(BF("Handle createObjectOfClass when mc is not a Class"));
    }



    void Lisp_O::setEmbeddedInPython(bool b)
    {_OF();
	LOG(BF("EmbeddedInPython is being set to[%d]") % b);
	this->_EmbeddedInPython = b;
    }



    Class_sp Lisp_O::boot_setf_findClass(Symbol_sp className, Class_sp mc)
    {
#if 0
	ASSERTF(this->_BootClassTableIsValid,
		BF("Never use Lisp_O::setf_findClass after boot - use af_setf_findClass"));
	this->_Roots._BootClassTable[className] = mc;
#else
        for ( auto it = this->_Roots.bootClassTable.begin(); it!=this->_Roots.bootClassTable.end(); ++it ) {
            if ( it->symbol == className ) {
                it->theClass = mc;
                return mc;
            }
        }
        SymbolClassPair sc(className,mc);
        this->_Roots.bootClassTable.push_back(sc);
#endif
        return mc;
    }


    Class_sp Lisp_O::boot_findClass(Symbol_sp className, bool errorp) const
    {_G();
	ASSERTF(this->_BootClassTableIsValid,
		BF("Never use Lisp_O::findClass after boot - use cl::_sym_findClass"));
#if 0
	SymbolDict<Class_O>::const_iterator fi = this->_Roots._BootClassTable.find(className);
	if ( fi == this->_Roots._BootClassTable.end() )
	{
	    if ( errorp )
	    {
		SIMPLE_ERROR(BF("No class named %s") % _rep_(className) );
	    }
	    return _Nil<Class_O>();
	}
	return fi->second;
#else
        for ( auto it = this->_Roots.bootClassTable.begin(); it!=this->_Roots.bootClassTable.end(); ++it ) {
            if ( it->symbol == className ) return it->theClass;
        }
        return _Nil<Class_O>();
#endif
    }


/*! After the core classes are defined and we have hash-tables, move all class definitions
  into the *class-name-hash-table*.  We do this because we can't put stuff into a hash-table
  until the hash-table class is defined and we need classes in the *class-name-hash-table* once
  CLOS starts up because that is where ECL expects to find them. */
    void Lisp_O::switchToClassNameHashTable()
    {_G();
	ASSERTF(this->_BootClassTableIsValid,BF("switchToClassNameHashTable should only be called once after boot"));
	HashTable_sp ht = _sym_STARclassNameHashTableSTAR->symbolValue().as<HashTable_O>();
#if 0
	for ( SymbolDict<Class_O>::iterator it=this->_Roots._BootClassTable.begin();
	      it!=this->_Roots._BootClassTable.end(); it++ )
	{
	    ht->hash_table_setf_gethash(it->first,it->second);
	}
#else
        for ( auto it = this->_Roots.bootClassTable.begin(); it!=this->_Roots.bootClassTable.end(); ++it ) {
            ht->hash_table_setf_gethash(it->symbol,it->theClass);
        }
        this->_Roots.bootClassTable.clear();
#endif
	this->_BootClassTableIsValid = false;
    }





#define ARGS_Lisp_O_find_single_dispatch_generic_function "(gf-symbol &optional errorp)"
#define DECL_Lisp_O_find_single_dispatch_generic_function ""
#define DOCS_Lisp_O_find_single_dispatch_generic_function "Lookup a single dispatch generic function. If errorp is truen and the generic function isn't found throw an exception"
    SingleDispatchGenericFunction_sp Lisp_O::find_single_dispatch_generic_function(Symbol_sp gfSym, bool errorp)
    {_G();
        SingleDispatchGenericFunction_sp fn = _lisp->_Roots._SingleDispatchGenericFunctionTable->gethash(gfSym,_Nil<T_O>()).as<SingleDispatchGenericFunction_O>();

	if ( fn.nilp() )
	{
	    if ( errorp )
	    {
		SIMPLE_ERROR(BF("No single-dispatch-generic-function named %s") % _rep_(gfSym) );
	    }
	    return _Nil<SingleDispatchGenericFunction_O>();
	}
	return fn;
    }





#define ARGS_Lisp_O_setf_find_single_dispatch_generic_function "(gf-symbol gf)"
#define DECL_Lisp_O_setf_find_single_dispatch_generic_function ""
#define DOCS_Lisp_O_setf_find_single_dispatch_generic_function "Define a single dispatch generic function "
    SingleDispatchGenericFunction_sp Lisp_O::setf_find_single_dispatch_generic_function(Symbol_sp gfName, SingleDispatchGenericFunction_sp gf)
    {_G();
	_lisp->_Roots._SingleDispatchGenericFunctionTable->setf_gethash(gfName, gf);
	return gf;
    }


#define ARGS_Lisp_O_forget_all_single_dispatch_generic_functions "()"
#define DECL_Lisp_O_forget_all_single_dispatch_generic_functions ""
#define DOCS_Lisp_O_forget_all_single_dispatch_generic_functions "Forget all single dispatch functions"
    void Lisp_O::forget_all_single_dispatch_generic_functions()
    {_G();
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



    string Lisp_O::classNameFromClassSymbol(Symbol_sp cid)
    {
	DEPRECIATED();
#if 0
	Class_sp mc = this->classFromClassSymbol(cid);
	return mc->getPackagedName();
#endif
    }





    Class_sp Lisp_O::classFromClassName(const string& name)
    {_OF();
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



    void Lisp_O::parseStringIntoPackageAndSymbolName(const string& name, bool& packageDefined, Package_sp& package, string& symbolName, bool& exported) const
    {_OF();
	packageDefined = true; // this will be true in most cases below
	LOG(BF("Parsing string[%s] into package and symbol name") % name );
	if ( name[0] == ':' )
	{
	    LOG(BF("It's a keyword"));
	    package = this->_Roots._KeywordPackage;
	    symbolName = name.substr(1,99999);
	    exported = true;
	    return;
	}
	if ( name[0] == '&' )
	{
	    LOG(BF("It's an & symbol"));
	    package = this->_Roots._CorePackage;
	    symbolName = name;
	    exported = true;
	    return;
	}
	size_t colonPos = name.find_first_of(":");
	exported = true;
	if ( colonPos == string::npos )
	{
	    LOG(BF("Package wasn't defined"));
	    symbolName = name;
	    packageDefined = false;
	    return;
	}
	bool doubleColon = false;
	size_t secondPart = colonPos+1;
	if ( name[secondPart] == ':' )
	{
	    LOG(BF("It's a non-exported symbol"));
	    exported = false;
	    doubleColon = true;
	    secondPart++;
	    if ( name.find_first_of(":",secondPart) != string::npos )
	    {
		SIMPLE_ERROR(BF("There can only be one ':' or '::' in a symbol name"));
	    }
	}
	package = this->findPackage(name.substr(0,colonPos));
	symbolName = name.substr(secondPart,99999);
	LOG(BF("It's a packaged symbol (%s :: %s)") % package->getName()% symbolName );
	return;
    }




    Symbol_mv Lisp_O::intern(const string& name, T_sp optionalPackageDesignator )
    {_G();
#if DEBUG_ENVIRONMENT_CREATION
//    ASSERT(this->_PackagesInitialized);
#endif
	Package_sp package;
	string symbolName;
	bool exported, packageDefined;
	this->parseStringIntoPackageAndSymbolName(name,packageDefined,package,symbolName,exported);
	if ( !packageDefined )
	{
	    package = coerce::packageDesignator(optionalPackageDesignator);
	}
	ASSERTNOTNULL(package);
        if ( package.nilp() ) {
            SIMPLE_ERROR(BF("The package %s has not been found") % _rep_(optionalPackageDesignator).c_str());
        }
        T_mv symStatus = package->intern(symbolName);
        Symbol_sp sym = symStatus;
        T_sp status = symStatus.second();
        return Values(sym,status);
    }

/*! The optionalPackageDesignator is nil */
    Symbol_sp Lisp_O::intern(string const& symbolName)
    {_G();
	Package_sp curPackage = this->getCurrentPackage();
	ASSERTNOTNULL(curPackage);
	return this->intern(symbolName,curPackage);
    }




    Symbol_sp Lisp_O::findSymbol(const string& name,T_sp optionalPackageDesignator) const
    {_G();
#if DEBUG_ENVIRONMENT_CREATION
	ASSERT(this->_PackagesInitialized);
#endif
	Package_sp package;
	string symbolName;
	bool exported,packageDefined;
	this->parseStringIntoPackageAndSymbolName(name,packageDefined,package,symbolName,exported);
	if ( !packageDefined )
	{
	    package = coerce::packageDesignator(optionalPackageDesignator);
	}
	return package->findSymbol(symbolName).as<Symbol_O>();
    }


    Symbol_sp Lisp_O::findSymbol(const string& symbolName /*, T_sp optionalPackageDesignator = nil */) const
    {_G();
	return this->findSymbol(symbolName,_Nil<T_O>());
    }


    Symbol_sp Lisp_O::intern(string const& symbolName,string const& packageName)
    {_G();
	Package_sp package = this->findPackage(packageName);
	return this->intern(symbolName,package);
    }


    Symbol_sp Lisp_O::internUniqueWithPackageName(string const& packageName, string const& symbolName)
    {_G();
	Package_sp package = this->findPackage(packageName);
        Symbol_mv symStatus = this->intern(symbolName,package);
        T_sp status = symStatus.second();
#if 0
        if ( status.notnilp() ) {
            printf("%s:%d WARNING! Lisp_O::internUniqueWithPackageName found existing symbol: %s\n",
                   __FILE__, __LINE__, symbolName.c_str());
        }
#endif
	return symStatus;
    }


    Symbol_sp Lisp_O::internWithPackageName(string const& packageName, string const& symbolName)
    {_G();
	Package_sp package = this->findPackage(packageName);
	return this->intern(symbolName,package);
    }


    Symbol_sp Lisp_O::internWithDefaultPackageName(string const& defaultPackageName,
                                                   string const& possiblePackagePrefixedSymbolName )
    {
        size_t pkgSep = possiblePackagePrefixedSymbolName.find(':',0);
        if (pkgSep == string::npos) {
            return this->internWithPackageName(defaultPackageName,possiblePackagePrefixedSymbolName);
        }
        size_t symbolNameStart = pkgSep+1;
        bool exportit = true;
        if (symbolNameStart<possiblePackagePrefixedSymbolName.size()-1) {
            if (possiblePackagePrefixedSymbolName[symbolNameStart] == ':') {
                ++symbolNameStart;
                exportit = false;
            }
        }
        string packageName = possiblePackagePrefixedSymbolName.substr(0,pkgSep);
        string symbolName = possiblePackagePrefixedSymbolName.substr(symbolNameStart,possiblePackagePrefixedSymbolName.size());
        Symbol_sp sym = this->internWithPackageName(packageName,symbolName);
        if ( exportit ) sym->exportYourself();
        return sym;
    }





    Symbol_sp Lisp_O::internKeyword(const string& name)
    {_G();
	string realName = name;
	if ( name[0] == ':' )
	{
	    realName = name.substr(1,99999);
	}
	size_t colonPos = realName.find_first_of(":");
	if ( colonPos != string::npos )
	{
	    SIMPLE_ERROR(BF("You cannot intern[%s] as a keyword - it has package designating ':' characters in it at pos[%d]") % realName % colonPos);
	}
	boost::to_upper(realName);
	return this->_Roots._KeywordPackage->intern(realName).as<Symbol_O>();
    }






    void Lisp_O::dump_apropos(const char* part) const
    {_OF();
	string substring = part;
	Cons_sp packages = _lisp->allPackagesAsCons();
	searchForApropos(packages,substring,true);
    }


    Cons_sp Lisp_O::allPackagesAsCons() const
    {
	return asCons(this->_Roots._Packages);
    }

    InvocationHistoryStack& Lisp_O::invocationHistoryStack()
    {
	return this->_Roots._InvocationHistoryStack;
    }


    MultipleValues& Lisp_O::multipleValues()
    {
	return this->_Roots._MultipleValues;
    }

    void Lisp_O::dump_backtrace(int numcol)
    {_OF();
	string bt = this->invocationHistoryStack().asString();
	_lisp->print(BF("%s")%bt);
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
	Cons_sp bt = btReversed->reverse().as_or_nil<Cons_O>();;
	strace << "Cando-backtrace number of entries: " << cl_length(bt) <<std::endl;
	while ( bt.notnilp() )
	{
	    T_sp code = bt->ocar();
	    stringstream sline;
	    if ( code->consP() )
	    {
		Cons_sp entry = code.as_or_nil<Cons_O>();
		if ( entry->hasParsePos() )
		{
		    sline << af_sourceFileInfo(entry)->permanentFileName() << ":" << af_lineno(entry) << " " << entry->__repr__();
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



    void Lisp_O::run()
    {_G();
//	this->inPackage("CORE-USER");
	//
	// Compile and evaluate the .rc code to extend the environment in lisp
	//
	if ( !this->_IgnoreInitImage )
	{
            Pathname_sp initPathname = _sym_STARcommandLineImageSTAR->symbolValue().as<Pathname_O>();
            T_mv result = core_loadBundle(initPathname);
            if ( result.nilp() ) {
                T_sp err = result.second();
                printf("Could not load bundle %s error: %s\n", _rep_(initPathname).c_str(), _rep_(err).c_str() );
            }
	} else if ( !this->_IgnoreInitLsp ) {
	    // Assume that if there is no program then
	    // we want an interactive script
	    //
	    {_BLOCK_TRACEF(BF("Evaluating initialization code in(%s)") % this->_RCFileName );
		Pathname_sp initPathname = cl_pathname(Str_O::create(this->_RCFileName));
		T_mv result = af_load(initPathname);
                if ( result.nilp() ) {
                    T_sp err = result.second();
                    printf("Could not load %s\n", _rep_(initPathname).c_str() );
                }
	    }
        } else {
	    {_BLOCK_TRACE("Interactive REPL");
		//
		// Implement a Read-Eval-Print-Loop
		//
		this->print(BF("Clasp (copyright Christian E. Schafmeister 2014)\n"));
		this->print(BF("Low level repl\n"));
		while ( 1 ) {
		    this->readEvalPrintInteractive();
		}
	    }
	    LOG(BF("Leaving lisp run"));
	}
    };



    SourceFileInfo_mv Lisp_O::sourceFileInfo(const string& fileName)
    {
        map<string,int>::iterator it = this->_SourceFileIndices.find(fileName);
        if ( it == this->_SourceFileIndices.end() ) {
            if ( this->_Roots._SourceFiles.size() == 0 ) {
                SourceFileInfo_sp unknown = SourceFileInfo_O::create("-no-file-",0);
                this->_Roots._SourceFiles.push_back(unknown);
            }
            int idx = this->_Roots._SourceFiles.size();
            this->_SourceFileIndices[fileName] = idx;
            SourceFileInfo_sp sfi = SourceFileInfo_O::create(fileName,idx);
            this->_Roots._SourceFiles.push_back(sfi);
            return Values(sfi,Fixnum_O::create(idx));
        }
        return Values(this->_Roots._SourceFiles[it->second],Fixnum_O::create(it->second));
    }


#define ARGS_af_sourceFileInfo "(name)"
#define DECL_af_sourceFileInfo ""
#define DOCS_af_sourceFileInfo "sourceFileInfo given a source name (string) or pathname or integer, return the source-file-info structure and the integer index"
    SourceFileInfo_mv af_sourceFileInfo(T_sp sourceFile)
    {
        if ( sourceFile.nilp() ) {
            return af_sourceFileInfo(Fixnum_O::create(0));
        } else if ( Str_sp strSourceFile = sourceFile.asOrNull<Str_O>() ) {
            return _lisp->sourceFileInfo(strSourceFile->get());
        } else if ( Pathname_sp pnSourceFile = sourceFile.asOrNull<Pathname_O>() ) {
            return _lisp->sourceFileInfo(af_namestring(pnSourceFile)->get());
        } else if ( Fixnum_sp fnSourceFile = sourceFile.asOrNull<Fixnum_O>() ) {
            if ( fnSourceFile->get() >= _lisp->_Roots._SourceFiles.size() ) {
                SIMPLE_ERROR(BF("Illegal index %d for source file info") % fnSourceFile->get() );
            }
            return Values(_lisp->_Roots._SourceFiles[fnSourceFile->get()],fnSourceFile);
        } else if ( Stream_sp so = sourceFile.asOrNull<Stream_O>() ) {
            T_sp sfi = clasp_input_source_file_info(so);
            return af_sourceFileInfo(sfi);
        } else if ( SourceFileInfo_sp sfi = sourceFile.asOrNull<SourceFileInfo_O>() ) {
            return _lisp->sourceFileInfo(sfi->namestring());
        } else if ( SourcePosInfo_sp spi = sourceFile.asOrNull<SourcePosInfo_O>() ) {
            return af_sourceFileInfo(Fixnum_O::create(spi->_FileId));
        }
        SIMPLE_ERROR(BF("Add support for source-file-info for ~a") % _rep_(sourceFile));
    };




    void Lisp_O::mapClassNamesAndClasses(KeyValueMapper* mapper)
    {_G();
	if ( this->_BootClassTableIsValid )
	{
#if 0
	    for ( SymbolDict<Class_O>::iterator it=this->_Roots._BootClassTable.begin();
		  it!=this->_Roots._BootClassTable.end(); it++ )
	    {
		if (!mapper->mapKeyValue(it->first,it->second)) break;
	    }
#else
            for ( auto it = this->_Roots.bootClassTable.begin(); it!=this->_Roots.bootClassTable.end(); ++it ) {
                if (!mapper->mapKeyValue(it->symbol,it->theClass)) return;
            }
            return;
#endif
        } else
              {
                  HashTable_sp ht = _sym_STARclassNameHashTableSTAR->symbolValue().as<HashTable_O>();
                  ht->lowLevelMapHash(mapper);
              }
        }


    string	Lisp_O::__repr__() const
    {_G();
	stringstream ss;
	ss << "Lisp_O object";
	return ss.str();
    };






    void Lisp_O::initializeGlobals(Lisp_sp lisp)
    {_G();
	LOG(BF("Lisp_O::initializeGlobals"));
	SYMBOL_EXPORT_SC_(CorePkg,selectPackage);
	Defun(selectPackage);
    }

    void Lisp_O::exposeCando()
    {_G();
	SYMBOL_SC_(CorePkg,find_single_dispatch_generic_function);
	af_def(CorePkg,"find-single-dispatch-generic-function",
	       &Lisp_O::find_single_dispatch_generic_function,
	       ARGS_Lisp_O_find_single_dispatch_generic_function,
	       DECL_Lisp_O_find_single_dispatch_generic_function,
	       DOCS_Lisp_O_find_single_dispatch_generic_function);
	SYMBOL_SC_(CorePkg,setf_find_single_dispatch_generic_function);
	af_def(CorePkg,"setf-find-single-dispatch-generic-function",
	       &Lisp_O::setf_find_single_dispatch_generic_function,
	       ARGS_Lisp_O_setf_find_single_dispatch_generic_function,
	       DECL_Lisp_O_setf_find_single_dispatch_generic_function,
	       DOCS_Lisp_O_setf_find_single_dispatch_generic_function);
	SYMBOL_SC_(CorePkg,forget_all_single_dispatch_generic_functions);
	af_def(CorePkg,"forget-all-single-dispatch-generic-functions",
	       &Lisp_O::forget_all_single_dispatch_generic_functions,
	       ARGS_Lisp_O_forget_all_single_dispatch_generic_functions,
	       DECL_Lisp_O_forget_all_single_dispatch_generic_functions,
	       DOCS_Lisp_O_forget_all_single_dispatch_generic_functions);

        CoreDefun(singleDispatchGenericFunctionTable);
	SYMBOL_SC_(CorePkg,stackMonitor);
	Defun(stackMonitor);
	Defun(lowLevelRepl);
	SYMBOL_SC_(CorePkg,setupStackMonitor);
	Defun(setupStackMonitor);

	SYMBOL_SC_(CorePkg,invokeInternalDebugger);
	Defun(invokeInternalDebugger);
	SYMBOL_SC_(CorePkg,invokeInternalDebuggerFromGdb);
	Defun(invokeInternalDebuggerFromGdb);
	SYMBOL_SC_(CorePkg,universalErrorHandler);
	Defun(universalErrorHandler);
	SYMBOL_SC_(CorePkg,stackUsed);
	Defun(stackUsed);



	SYMBOL_SC_(CorePkg,exit);
	Defun(exit);
	SYMBOL_SC_(CorePkg,quit);
	Defun(quit);
#if defined(XML_ARCHIVE)
	SYMBOL_SC_(CorePkg,serialize_xml);
	Defun(serialize_xml);
	SYMBOL_SC_(CorePkg,deserialize_xml);
	Defun(deserialize_xml);
	SYMBOL_SC_(CorePkg,render);
	Defun(render);
	SYMBOL_SC_(CorePkg,saveCando);
	Defun(saveCando);
	SYMBOL_SC_(CorePkg,loadCando);
	Defun(loadCando);
#endif // defined(XML_ARCHIVE)
	SYMBOL_SC_(CorePkg,getline);
	Defun(getline);

	SYMBOL_SC_(CorePkg,system);
	Defun(system);

	SYMBOL_EXPORT_SC_(ClPkg,apropos);
	Defun(apropos);

	af_def(CorePkg,"brcl-apropos",&af_apropos,ARGS_af_apropos,DECL_af_apropos,DOCS_af_apropos);

	SYMBOL_EXPORT_SC_(ClPkg,export);
	Defun(export);
	SYMBOL_EXPORT_SC_(ClPkg,intern);
	Defun(intern);
//	defNoWrapPackage(CorePkg,"apply", &prim_apply,_LISP);
	SYMBOL_SC_(CorePkg,isTopLevelScript);
	Defun(isTopLevelScript);

//	defNoWrapPackage(CorePkg,"allGlobalNames", &prim_allGlobalNames ,_LISP);
//	defNoWrapPackage(CorePkg,"locals", &prim_locals,_LISP);
	SYMBOL_SC_(CorePkg,sourceFileName);
	Defun(sourceFileName);
	SYMBOL_SC_(CorePkg,sourceLineColumn);
	Defun(sourceLineColumn);
//	SYMBOL_SC_(CorePkg,backtrace);
//	Defun(backtrace);
//	defNoWrapPackage(CorePkg,"globals", &prim_globals,_LISP);
	SYMBOL_SC_(CorePkg,findFileInLispPath);
	Defun(findFileInLispPath);

	SYMBOL_EXPORT_SC_(ClPkg,findClass);
	Defun(findClass);
	SYMBOL_SC_(CorePkg,setf_findClass);
	Defun(setf_findClass);

//	defNoWrapPackage(CorePkg,"print", &prim_print ,_LISP);

	SYMBOL_SC_(CorePkg,isAssignableTo);
	Defun(isAssignableTo);
	SYMBOL_SC_(CorePkg,isSubClassOf);
	Defun(isSubClassOf);

//	defNoWrapPackage(CorePkg,"derive", &prim_derive ,_LISP);
//	defNoWrapPackage(CorePkg,"isA", &prim_isA ,_LISP);

//	defNoWrapPackage(CorePkg,"parseConsOfStrings", &prim_parseConsOfStrings ,_LISP);

	SYMBOL_EXPORT_SC_(ClPkg,funcall);
	Defun(funcall);
	SYMBOL_EXPORT_SC_(ClPkg,apply);
	Defun(apply);
//	defNoWrapPackage(CorePkg,"sub", &prim_sub ,_LISP);
//	defNoWrapPackage(CorePkg,"-", &prim_sub ,_LISP);
//	defNoWrapPackage(CorePkg,"div", &prim_div ,_LISP);
//    defNoWrapPackage(CorePkg,"mod", &prim_mod ,_LISP);
//	defNoWrapPackage(CorePkg,"/", &prim_div ,_LISP);
//	defNoWrapPackage(CorePkg,"mul", &prim_mul ,_LISP);
//	defNoWrapPackage(CorePkg,"*", &prim_mul ,_LISP);
//	defNoWrapPackage(CorePkg,"className", &prim_className ,_LISP);

	SYMBOL_SC_(CorePkg,repr);
	Defun(repr);

	SYMBOL_EXPORT_SC_(ClPkg,error);
	ClDefun(error);
	SYMBOL_EXPORT_SC_(ClPkg,cerror);
	ClDefun(cerror);
	SYMBOL_EXPORT_SC_(ExtPkg,setenv);
	ExtDefun(setenv);
	SYMBOL_EXPORT_SC_(ExtPkg,getenv);
	ExtDefun(getenv);
	SYMBOL_EXPORT_SC_(ClPkg,not);
	Defun(not);

	SYMBOL_SC_(CorePkg,debugLogOn);
	Defun(debugLogOn);
	SYMBOL_SC_(CorePkg,debugLogOff);
	Defun(debugLogOff);

	// mpi commands that are always built in
	SYMBOL_SC_(CorePkg,mpi_enabled);
	Defun(mpi_enabled);
	SYMBOL_SC_(CorePkg,mpi_rank);
	Defun(mpi_rank);
	SYMBOL_SC_(CorePkg,mpi_size);
	Defun(mpi_size);
	// Basic tests
//	defNoWrapPackage(CorePkg,"consp",&prim_consp,_LISP);
//	defNoWrapPackage(CorePkg,"symbolp",&prim_symbolp,_LISP);

	// aliases for "list" command

	SYMBOL_SC_(CorePkg,sorted);
	Defun(sorted);
	SYMBOL_EXPORT_SC_(ClPkg,sort);
	Defun(sort);
	SYMBOL_EXPORT_SC_(ClPkg,macroexpand_1);
	Defun(macroexpand_1);
	SYMBOL_EXPORT_SC_(ClPkg,macroexpand);
	Defun(macroexpand);

	// information functions
	SYMBOL_SC_(CorePkg,database_dir);
	Defun(database_dir);
	SYMBOL_SC_(CorePkg,script_dir);
	Defun(script_dir);
	SYMBOL_SC_(CorePkg,libraryPath);
	Defun(libraryPath);
	SYMBOL_SC_(CorePkg,lispCodePath);
	Defun(lispCodePath);
//	defNoWrapPackage(CorePkg,"dumpHidden", &prim_dumpHidden,_LISP);

	SYMBOL_SC_(CorePkg,setCurrentWorkingDirectory);
	Defun(setCurrentWorkingDirectory);

	SYMBOL_EXPORT_SC_(ClPkg,acons);
	Defun(acons);
	SYMBOL_EXPORT_SC_(ClPkg,assoc);
	Defun(assoc);
	SYMBOL_EXPORT_SC_(ClPkg,member);
	Defun(member);
	Defun(memberTest);

	SYMBOL_SC_(CorePkg,member1);
	Defun(member1);

	SYMBOL_SC_(CorePkg,exportToPython);
	Defun(exportToPython);

	SYMBOL_EXPORT_SC_(ClPkg,find_package);
	Defun(find_package);

        Defun(sourceFileInfo);

    }



    void Lisp_O::exposePython()
    {_G();
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
#if 0  //These are now Object methods
	boost::python::def_raw(CorePkg,"eq", &prim_eq,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"==", &prim_eq,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"ne", &prim_ne,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"!=", &prim_ne,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"lt", &prim_lt,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"<", &prim_lt,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"gt", &prim_gt,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,">", &prim_gt,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"le", &prim_le,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"<=", &prim_le,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,"ge", &prim_ge,ARGS_empty,DOCS_empty,_LISP);
	boost::python::def_raw(CorePkg,">=", &prim_ge,ARGS_empty,DOCS_empty,_LISP);
#endif
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









    LispHolder::LispHolder(bool mpiEnabled, int mpiRank, int mpiSize )
    {
	this->_Lisp = Lisp_O::createLispEnvironment(mpiEnabled,mpiRank,mpiSize);
    }




    void LispHolder::startup(int argc, char* argv[], const string& appPathEnvironmentVariable )
    {
	this->_Lisp->_StackTop = (char*)&argc;
	::_lisp = this->_Lisp;
	const char* argv0 = "./";
	if ( argc > 0 ) argv0 = argv[0];
        this->_Lisp->_Argc = argc;
        for ( int i=0; i<argc; ++i ) {
            this->_Lisp->_Argv.push_back(string(argv[i]));
        }
	Bundle* bundle = new Bundle();
	bundle->initialize(argv0,appPathEnvironmentVariable);
	this->_Lisp->startupLispEnvironment(bundle);
#if 0
	if (_lisp->mpiEnabled())
	{
	    stringstream ss;
	    ss << "P"<<_lisp->mpiRank()<<":";
	    printvPushPrefix(ss.str());
	}
#endif
	_lisp->parseCommandLineArguments(argc,argv,true);
    }




    LispHolder::~LispHolder()
    {
	this->_Lisp->shutdownLispEnvironment();
    }




    Exposer::Exposer(Lisp_sp lisp, const string& packageName, const char* nicknames[])
    {_G();
	if ( !lisp->recognizesPackage(packageName) )
	{
	    list<string> lnnames;
	    for ( int i=0; strcmp(nicknames[i],"")!=0; i++ )
	    {
		lnnames.push_front(nicknames[i]);
	    }
	    list<string> lp;
	    this->_Package = lisp->makePackage(packageName,lnnames,lp);
	} else
	{
	    this->_Package = lisp->findPackage(packageName);
	}
	this->_PackageName = packageName;
    }


    Exposer::Exposer(Lisp_sp lisp, const string& packageName )
    {_G();
	if ( !lisp->recognizesPackage(packageName) )
	{
	    list<string> lnnames;
	    list<string> lpkgs;
	    this->_Package = lisp->makePackage(packageName,lnnames,lpkgs);
	} else
	{
	    this->_Package = lisp->findPackage(packageName);
	}
	this->_PackageName = packageName;
    }


    Exposer::~Exposer() {};





//    EXPOSE_CLASS(core,Lisp_O);


    ChangePackage::ChangePackage(Package_sp newPackage) : _SavedPackage(_lisp->getCurrentPackage())
    {
	_lisp->selectPackage(newPackage);
    }

    ChangePackage::~ChangePackage()
    {
	_lisp->selectPackage(this->_SavedPackage);
    }



};
