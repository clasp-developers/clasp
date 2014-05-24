#ifndef	Lisp_H //[
#define Lisp_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "holder.h"
#include "lightProfiler.h"
#include "lispStream.fwd.h"
#include "executables.fwd.h"
#include "cons.h"
#include "numbers.h"
#include "pointer.fwd.h"
#include "hashTable.fwd.h"
#include "sourceFileInfo.fwd.h"
#include "numbers.fwd.h"
#include "bignum.h"
#include "evaluator.fwd.h"
//#i n c l u d e "f u n c t ionptr.h"
#include "cache.h"
#include "translators.h"
#include "stringSet.fwd.h"
//#i n c l u d e "setfExpander.fwd.h"
//#i n c l u d e "environment.h"
#include "executables.h"
#include "loadTimeValues.fwd.h"
#include "readtable.fwd.h"
//#i n c l u d e "genericFunction.fwd.h"
#include "singleDispatchGenericFunction.fwd.h"
//#i n c l u d e "executableEnvironment.h"
//#i n c l u d e "lispDefinitions.h"


namespace cl {
    extern core::Symbol_sp 	_sym_eq;
};


namespace core 
{


    class Bundle;
    class CallStack;
    SMART(Intrinsic);
    SMART(Function);
    SMART(Reader);
    SMART(FunctionValueEnvironment);
    SMART(Class);
    SMART(BuiltInClass);
    SMART(Package);
    SMART(Path);
    SMART(StandardClass);
    SMART(Binder);
    SMART(SpecialForm);
    SMART(Hierarchy);
    SMART(Environment);

    Cons_sp af_member(T_sp item, T_sp list, T_sp key=_Nil<T_O>(), T_sp test=cl::_sym_eq, T_sp test_not=_Nil<T_O>());
    void af_invokeInternalDebugger(T_sp condition);



    /*! A structure that stores the integer byte/word ordering information for this processor.
      It is calculated when the Lisp environment starts up and can be accessed by routines to 
      handle slicing up uint64_t */
    class IntegerOrdering //: public gctools::GCIgnoreClass
    {
    public:
	bool	_BigEndian;
	bool	_MostSignificantWordFirst;
	int	_mpz_import_word_order;
	int	_mpz_import_size;
	int	_mpz_import_endian;

	IntegerOrdering()
	{
	    // Setup the contents here in the constructor 
	    unsigned int endian_test = 1;
	    char* endian_test_ptr = (char*)&endian_test;
	    this->_mpz_import_endian = 0; // use native endianness (see mpz_import)
	    this->_mpz_import_size = sizeof(int);
	    if ( *endian_test_ptr == 1)
	    {
		this->_BigEndian = false;
		this->_MostSignificantWordFirst = false;
		this->_mpz_import_word_order = -1;
	    } else if ( *(endian_test_ptr+sizeof(int)-1) == 1)
	    {
		this->_BigEndian = true;
		this->_MostSignificantWordFirst = true;
		this->_mpz_import_word_order = 1;
	    } else
	    {
		THROW_HARD_ERROR(BF("What the heck? - the Endian test failed when starting up Lisp environment"));
	    }
	}
    };



    /*! Virtual class for defining new packages and doing package dependent 
     exposure of classes/functions/globals.  You can create more than one
     PackageHolder for a package to expose successive things.
     If you invoke the Ctor and the package hasn't been created then
     it will be created and you must save the Package before
     before the PackageExposer is destructed or the package will also be
     destructed.
     If you invoke the PackageExposer ctor when the package exists in the
     lisp interpreter then it will be gotten from the lisp interpreter and
     used to expose things.
     For instance, the lisp interpreter creates a CorePackage to expose
     the cando stuff and then the python interface creates another one and
     gets the Package_sp for the core package from lisp and then 
     exposes the python classes/functions/globals */
    class PackageExposer
    {
    public:
	typedef enum { candoClasses, candoFunctions, candoGlobals,
		       pythonClasses, pythonFunctions, pythonGlobals } WhatToExpose;
    private:
	// The package is put here
	Package_sp 	_Package;
	string		_PackageName;
    public:
	/*! CTor that looks up a Package with packageName and if it
	  doesn't exist it makes it - allows nicknames */
	PackageExposer(Lisp_sp lisp, const string& packageName, const char* nicknames[]);


	/*! CTor that looks up a Package with packageName and if it
	  doesn't exist it makes it - no nicknames allowed */
	PackageExposer(Lisp_sp lisp, const string& packageName );

	virtual ~PackageExposer();

        DECLARE_onHeapScanGCRoots();

	/*! Return the packageName */
	string packageName() const { return this->_PackageName;};

	/*! Return the Package that this PackageExposer holds */
	Package_sp package() const { return this->_Package;};

	/* Evaluate the code that exposes the package Classes/Functions/Globals
	 to Cando-lisp or to Python depending on the value of (what) */
	virtual void expose(core::Lisp_sp lisp, WhatToExpose what) const = 0;
    };


// THESE NEED TO BE DEFINED WHERE THEY ARE USED
//
//#define EXTERN_SYMBOL(__sym) extern core::Symbol_sp __sym;
//#define	STATIC_SYMBOL(__sym) core::Symbol_sp __sym = UNDEFINED_SYMBOL;

//#define	DEFINE_FIRST_SYMBOL_FOR_PACKAGE(pkgName) {}
//#define CREATE_SYMBOL(sidsym,rsid,pkg,name) sidsym = _lisp->internWithPackageName(pkg,name);
#define DEFAULT_LOOKUP_SYMBOL(pkg,name) _lisp->internWithPackageName(pkg,name)

#define	FLAG_Continue	1
#define	FLAG_Break 	2
#define	FLAG_Return	4
#define	FLAG_Exit	8


    typedef	T_sp(*PrimFuncPtr)(Cons_sp ,Lisp_sp);





    template <typename oclass>
	class class_;


/*! Interpreter/compiler modes  see Lisp_O::_Mode
  See ecl::compiler.d
*/
#define	FLAG_LOAD	0x01
#define	FLAG_COMPILE	0x02
#define	FLAG_EXECUTE	0x04
#define FLAG_ONLY_LOAD	0x08

    class PushLispMode;



    class Lisp_O
    {
	struct GCRoots : public gctools::HeapRoot {
	/*! The invocation history stack this should be per thread */
	    InvocationHistoryStack 	_InvocationHistoryStack;
	/*! Multiple values - this should be per thread */
	    MultipleValues 		_MultipleValues;
	    /*! Bignum registers should be one per thread */
	    Bignum_sp 			_BignumRegister0;
	    Bignum_sp 			_BignumRegister1;
	    Bignum_sp 			_BignumRegister2;

//	    HashTable_sp                _TraceFunctions;
	    HashTable_sp 		_SystemProperties;
	    DynamicBindingStack 	_Bindings;
	    /*! Map source file path strings to SourceFileInfo_sp */
	    HashTableEqual_sp           _SourceFiles; // map<string,SourceFileInfo_sp> 	_SourceFiles;
	    /*! Store CATCH info */
	    Cons_sp 			_CatchInfo;
	    /* The global class table that maps class symbols to classes */
	    SymbolMap<Class_O>		_BootClassTable;
	    /*! When compiled files are loaded, they need to create
	      LOAD-TIME-VALUEs and QUOTEd objects using C++ calls at runtime.
	      Those objects are stored here as a map on the compiled file name. */
	    HashTableEqual_sp           _LoadTimeValueArrays; // map<string,LoadTimeValues_sp> _LoadTimeValueArrays;
	    Cons_sp 			_CommandLineArguments;
            gctools::Vec0<Package_sp>	_Packages;
	    SymbolMap<Function_O>	_SetfDefinitions;
	    Package_sp 			_CorePackage;
	    Package_sp 			_KeywordPackage;
	    Package_sp 			_CommonLispPackage;
	    HashTableEq_sp                _SpecialForms;
	    /*! Store a table of generic functions - should this be a HashTable?  What about (setf XXX) generic functions? Aug2013 */
	    SymbolMap<SingleDispatchGenericFunction_O> 	_SingleDispatchGenericFunctionTable;
	    /*! True object */
	    T_sp 			_TrueObject;
            /*! This is needed by the compiler */
            ActivationFrame_sp          _ActivationFrameNil;

#if CLOS
	    /*! Generic functions method cache */
	    Cache* 			_MethodCachePtr;
	    /*! Generic functions slot cache */
	    Cache* 			_SlotCachePtr;
#endif
	    DoubleFloat_sp 		_RehashSize;
	    DoubleFloat_sp 		_RehashThreshold;
	    Stream_sp 			_NullStream;
	    Cons_sp 			_PathnameTranslations; /* alist */
	    SourceManager_sp 		_SourceManager;
	    Complex_sp 			_ImaginaryUnit;
	    Complex_sp 			_ImaginaryUnitNegative;
	    Ratio_sp 			_PlusHalf;
	    Ratio_sp 			_MinusHalf;
	    SingleFloat_sp 		_SingleFloatMinusZero;
	    SingleFloat_sp 		_SingleFloatPlusZero;
	    DoubleFloat_sp 		_DoubleFloatMinusZero;
	    DoubleFloat_sp 		_DoubleFloatPlusZero;
	    SingleFloat_sp 		_SingleFloatOne;
	    DoubleFloat_sp 		_DoubleFloatOne;
#ifdef BRCL_LONG_FLOAT
	    LongFloat_sp 		_LongFloatMinusZero;
	    LongFloat_sp 		_LongFloatPlusZero;
	    LongFloat_sp 		_LongFloatOne;
#endif // ifdef BRCL_LONG_FLOAT

	    GCRoots();

            DECLARE_onHeapScanGCRoots();
	};

	friend class IncrementTraceLevel;
	friend class MultipleValues;
	friend class PushLispMode;
	friend class CoreExposer;
	friend class ConditionHandlerManager;	
	friend class BootStrapCoreSymbolMap;
	friend T_sp sp_eval_when( Cons_sp args, Environment_sp env );
	template <class oclass> friend void define_base_class(Class_sp co, Class_sp cob, uint& classesUpdated );
	template <class oclass> friend BuiltInClass_sp hand_initialize_allocatable_class(uint& classesHandInitialized, Lisp_sp lisp, BuiltInClass_sp _class );
	friend T_mv af_candoTrace( Cons_sp whole, Environment_sp env);
	friend T_mv af_candoUntrace( Cons_sp whole, Environment_sp env );
	friend T_mv af_put_sysprop(T_sp key, T_sp area, T_sp value);
	friend T_mv af_get_sysprop(T_sp key, T_sp area);

	friend void af_clearGfunHash(T_sp what);
//	/* disable scrape */ LISP_BASE1(T_O);
//	/* disable scrape */ LISP_CLASS(core,CorePkg,Lisp_O,"Lisp");
    public:
	static void initializeGlobals(Lisp_sp lisp);

	friend T_sp prim_getForm(Function_sp e, Cons_sp args, Environment_sp environ, Lisp_sp lisp);
	template <class oclass> friend BuiltInClass_sp hand_initialize_class(uint& classesHandInitialized, Lisp_sp prog,BuiltInClass_sp c);
    public:
	static 	void lisp_initSymbols(Lisp_sp lisp);

    public:
	static const int MaxFunctionArguments = 64; //<! See ecl/src/c/main.d:163 ecl_make_cache(64,4096)
	static const int MaxClosSlots = 3; //<! See ecl/src/c/main.d:164 ecl_make_cache(3,4096)
	static const int ClosCacheSize = 65536;
    public:
	void initialize();
#if defined(XML_ARCHIVE)
	void	archive(ArchiveP node);
#endif // defined(XML_ARCHIVE)
    public:
        DECLARE_onHeapScanGCRoots();
    public:
	GCRoots 		_Roots;
	char*			_StackTop;
	uint			_StackWarnSize;
	uint			_StackSampleCount;
	uint			_StackSampleSize;
	uint			_StackSampleMax;
    private:
	uint			_Mode;
	/*! Store paths to important directories */
	Bundle*			_Bundle;
	/*! Stores whether the system is big-endian or not */
	IntegerOrdering		_IntegerOrdering;
	DebugStream*            _DebugStream;
	uint			_SingleStepLevel;
	int			_TraceLevel;
	int			_DebuggerLevel;
	LightProfiler*           _profiler;
	/*! Global environment initialization hooks can be added until
	  the environment is started up.
	*/
	bool			_LockGlobalInitialization;
	vector<InitializationCallback>	_GlobalInitializationCallbacks;
	bool	_MpiEnabled;
	int	_MpiRank;
	int	_MpiSize;
	bool			_Interactive;
	bool			_EmbeddedInPython;
	string			_FunctionName;
	/*! Define the name of a source file that is evaluated
	 * before everything else to extend the environment
	 */
	string			_RCFileName;
	bool			_dont_load_startup; // true if the startup shouldn't be loaded
	bool			_ScriptInFile;
	string			_FileNameOrCode;
	map<string,int>		_PackageNameIndexMap;
	/*! Keep track of every new environment that is created */
	uint			_EnvironmentId;
    private:
	/*! Callbacks for making packages and exporting symbols */
	MakePackageCallback	_MakePackageCallback;
	ExportSymbolCallback	_ExportSymbolCallback;
	int			_RequireLevel;
	bool			_CoreBuiltInClassesInitialized;
	bool			_BuiltInClassesInitialized;
	bool			_PackagesInitialized;
	bool			_EnvironmentInitialized;
	bool			_NilsCreated;
	string                  _LastCompileErrorMessage;
	bool			_BootClassTableIsValid;
	/*! Store the maximum path length for the system */
	int			_PathMax;
	// ------------------------------------------------------------
	// ------------------------------------------------------------
	// ------------------------------------------------------------
	// ------------------------------------------------------------
    public:
	InvocationHistoryStack&	invocationHistoryStack();
	MultipleValues&		multipleValues();
    public:
	LightProfiler& profiler() { return *(this->_profiler);};
	DebugStream& debugLog() {HARD_ASSERT(this->_DebugStream!=NULL);return *(this->_DebugStream);};
//	vector<string>& printfPrefixStack() { return this->_printfPrefixStack;};
    public:
	void setupMpi(bool mpiEnabled, int mpiRank, int mpiSize);
	bool mpiEnabled() { return this->_MpiEnabled;}
	int mpiRank() { return this->_MpiRank;}
	int mpiSize() { return this->_MpiSize;}
    public:
	IntegerOrdering const&  integer_ordering() const { return this->_IntegerOrdering;};
    public:
	void mapClassNamesAndClasses(KeyValueMapper* mapper);
    public:
	Cons_sp catchPushTag(T_sp tag);
	void catchUnwindTag(Cons_sp catchStore);
	Cons_sp catchFindTag(T_sp tag);
    public:
	Cons_sp pathnameTranslations() const { return this->_Roots._PathnameTranslations; };
	void setPathnameTranslations(Cons_sp pnt) { this->_Roots._PathnameTranslations = pnt;};
	/*! Return the maximum path length for the system */
	int pathMax() const { return this->_PathMax; };
    public:
	bool bootClassTableIsValid() const { return this->_BootClassTableIsValid;};
    public:
	/*! Get the LoadTimeValues_sp that corresponds to the name.
	  If it doesn't exist then make one and return it. */
	LoadTimeValues_sp& getOrCreateLoadTimeValues(const string& name, int numberOfLoadTimeValues=0, int numberOfLoadTimeSymbols=0);
	Cons_sp loadTimeValuesIds() const;
	T_sp loadTimeValue(const string& name, int idx);
	Symbol_sp loadTimeSymbol(const string& name, int idx);
	LoadTimeValues_sp findLoadTimeValues(const string& name);
    public:
    /*! Keep track of every source file that is read by the system */
	SourceFileInfo_sp getSourceFileInfo(const string& fileName);
    /*! Maintain a database of every source file read by the system */
	void setSourceFileInfo(const string& fileName, SourceFileInfo_sp fileInfo);

    public:
	/*! Takes the place of ECL trap_fpe_bits - for now trap everything */
	int trapFpeBits() { return ~0;};
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
	Ratio_sp plusHalf() const { return this->_Roots._PlusHalf;};
	Ratio_sp minusHalf() const { return this->_Roots._MinusHalf;};
	SingleFloat_sp singleFloatMinusZero() const { return this->_Roots._SingleFloatMinusZero;};
	SingleFloat_sp singleFloatPlusZero() const { return this->_Roots._SingleFloatPlusZero;};
	DoubleFloat_sp doubleFloatMinusZero() const { return this->_Roots._DoubleFloatMinusZero;};
	DoubleFloat_sp doubleFloatPlusZero() const { return this->_Roots._DoubleFloatPlusZero;};
	SingleFloat_sp singleFloatOne() const { return this->_Roots._SingleFloatOne;};
	DoubleFloat_sp doubleFloatOne() const { return this->_Roots._DoubleFloatOne;};
#ifdef BRCL_LONG_FLOAT
	LongFloat_sp longFloatMinusZero() const { return this->_Roots._LongFloatMinusZero;};
	LongFloat_sp longFloatPlusZero() const { return this->_Roots._LongFloatPlusZero;};
	LongFloat_sp longFloatOne() const { return this->_Roots._LongFloatOne;};
#endif // ifdef BRCL_LONG_FLOAT
    public:
	Cache* methodCachePtr() const { return this->_Roots._MethodCachePtr;};
	Cache* slotCachePtr() const { return this->_Roots._SlotCachePtr;};

    public:
	/*! Setup makePackage and exportSymbol callbacks */
	void setMakePackageAndExportSymbolCallbacks(MakePackageCallback mpc,ExportSymbolCallback esc);
    public:
	uint mode() const { return this->_Mode;};
    public:
	/*! Get access to the SourceManager of the Common Lisp environment */
	SourceManager_sp sourceManager() const { return this->_Roots._SourceManager;};
    public:
	bool isSingleStepOn() { return this->_SingleStepLevel!=UndefinedUnsignedInt;};
	void setSingleStepLevel(uint level) { this->_SingleStepLevel = level;};
	uint getSingleStepLevel() const { return this->_SingleStepLevel;};

    public:
	Cons_sp allPackagesAsCons() const;

    public:
	/*! Add a function to trace */
	void add_trace(Function_sp func);
	/*! Remove a function to trace */
	void remove_trace(Function_sp func);

	/*! Return a list of functions being traced */
	Cons_sp trace_functions() const;

    public:
	DynamicBindingStack& bindings() { return this->_Roots._Bindings;};

    public:
	/*! Add a pair of symbolIDs that provide an accessor get/setf pair */
	void add_accessor_pair(Symbol_sp getter, Symbol_sp setter);
	/*! Add a SetfExpander associated with the symbol */
//	void addSetfExpander(Symbol_sp symbol, SetfExpander_sp expander);
	/*! Lookup the SetfExpander by symbol - if not found return nil */
//	SetfExpander_sp lookupSetfExpander(Symbol_sp symbol) const;
    protected:
	Cons_sp getConditionHandlers();
	void setConditionHandlers(Cons_sp handlers);
	Cons_sp getRestartHandlers();
	void setRestartHandlers(Cons_sp handlers);
    public:
	Bignum_sp bigRegister0() { return this->_Roots._BignumRegister0;};
	Bignum_sp bigRegister1() { return this->_Roots._BignumRegister1;};
	Bignum_sp bigRegister2() { return this->_Roots._BignumRegister2;};
    public:
	bool isEnvironmentInitialized() { return this->_EnvironmentInitialized;};
	uint nextEnvironmentId();
    public:
#if defined(XML_ARCHIVE)
	/*! Like read but uses serialization code to deserialize objects from a stream */
	T_sp sread(Stream_sp sin, bool eofErrorP, T_sp eofValue);
	/*! Like print but uses serialization to serialize objects to a stream */
	void sprint(T_sp obj, Stream_sp sout );
#endif // defined(XML_ARCHIVE)
    public:
	void print(boost::format fmt);
	void prin1(boost::format fmt);
    public:
	/*! Create a setfDefinition */
	void set_setfDefinition(Symbol_sp fnName, Function_sp fnDef);
	/*! Return the function or nil if not found */
	Function_sp get_setfDefinition(Symbol_sp fnName) const;
	/*! Return true if the definition was found */
	bool remove_setfDefinition(Symbol_sp fnName);
	
    public:
	void incrementDebuggerLevel() { this->_DebuggerLevel++;};
	void decrementDebuggerLevel() { this->_DebuggerLevel--;};
	int debuggerLevel() const { return this->_DebuggerLevel;};
    public:
	/*! This function is called whenever a symbol is exported 
	  it will be used to callback to Python to install python callable functions
	*/
	void exportingSymbol(Symbol_sp sym);

	void dump_apropos(const char* part) const;

	void dump_backtrace(int numcol=50);

    public:
	DoubleFloat_sp rehashSize() const {return this->_Roots._RehashSize;};
	DoubleFloat_sp rehashThreshold() const { return this->_Roots._RehashThreshold;};
	Stream_sp nullStream() const { return this->_Roots._NullStream;};
    public:
	//	void load(T_sp filespec, bool verbose=false, bool print=false, bool ifDoesNotExist=true );
    public:	
	/*! Constantly updated line number as programs execute
	 */
	uint			_LineNumber;

    public:
        gctools::Vec0<Package_sp>& packages() { return this->_Roots._Packages;};
	Package_sp keywordPackage() { return this->_Roots._KeywordPackage;};
    private:
	void parseStringIntoPackageAndSymbolName(const string& name, bool& packageDefined, Package_sp& package, string& symbolName, bool& exported ) const;
    public:
        ActivationFrame_sp& activationFrameNil() { return this->_Roots._ActivationFrameNil; };
	T_sp	_true() const { return this->_Roots._TrueObject;};
	T_sp	_not(T_sp x) const { return this->_boolean(!x.isTrue());};
	T_sp	_false() const { return _Nil<T_O>(); };
	T_sp	_boolean(bool b) const { if (b) return this->_true(); return this->_false();};
    public:
//	void createHiddenBinder();
	/*! Return the hidden binder
	 */
//	Binder_sp hiddenBinder();
    public:
	/*! Return true if running a graphical environment
	 */
	bool	graphical();
//	void setGraphical(bool g) { this->_Graphical = g;};
    public:
	bool	CoreBuiltInClassesInitialized() { return this->_CoreBuiltInClassesInitialized;};
	bool	BuiltInClassesInitialized() { return this->_BuiltInClassesInitialized;};
	bool	NilsCreated() { return this->_NilsCreated;};
    public:
	/*! Pass the mpi process rank in (rank) */
	static Lisp_sp createLispEnvironment(bool mpiEnabled, int mpiRank, int mpiSize );

	/*! Call this to setup the lisp environment
	 */
	void startupLispEnvironment(Bundle* bundle);
	/*! Call this to shut down the lisp environment before it is destructed
	 */
	void shutdownLispEnvironment();


	void setCurrentWorkingDirectory(Path_sp path);
	Path_sp getCurrentWorkingDirectory();

	ReadTable_sp getCurrentReadTable();

	template <class oclass>
	mem::smart_ptr<Cons_O> cons(mem::smart_ptr<oclass> obj,mem::smart_ptr<Cons_O> tail)
	{
	    return this->create<Cons_O>(obj,tail);
	}
	template <class oclass>
	mem::smart_ptr<oclass> create()
	{
            GC_ALLOCATE(oclass,res);
	    return res;
	}
	/*! One argument creator */
	template <class oclass,typename targ1>
	mem::smart_ptr<oclass> create(targ1 x)
	{
	    mem::smart_ptr<oclass> res = oclass::create(x);
	    return res;
	}
	/*! Two argument creator */
	template <class oclass,typename targ1, typename targ2>
	mem::smart_ptr<oclass> create(targ1 x, targ2 y)
	{
	    mem::smart_ptr<oclass> res = oclass::create(x,y);
	    return res;
	}
	/*! Three argument creator */
	template <class oclass,typename targ1, typename targ2, typename targ3>
	mem::smart_ptr<oclass> create(targ1 x, targ2 y, targ3 z)
	{
	    mem::smart_ptr<oclass> res = oclass::create(x,y,z,_lisp);
	    return res;
	}
    public:
	Bundle&		bundle() { return *this->_Bundle;};
    public:
	bool isInteractive() { return this->_Interactive;};
	void setInteractive(bool b) { this->_Interactive = b;};
	bool isEmbeddedInPython() { return this->_EmbeddedInPython;};
	void setEmbeddedInPython(bool b);



	/*! Lookup a single-dispatch-ggeneric-function in the _SingleDispatchGenericFunctionTable by name
	 If errorp == true then throw an exception if the single-dispatch-generic-function is not
	 found otherwise return nil */
	static SingleDispatchGenericFunction_sp find_single_dispatch_generic_function(Symbol_sp gfSym, bool errorp=true);
	/*! Associate a generic function with a symbol by name */
	static SingleDispatchGenericFunction_sp setf_find_single_dispatch_generic_function(Symbol_sp gfSym, SingleDispatchGenericFunction_sp gf);
	/*! Clear all generic functions */
	static void forget_all_single_dispatch_generic_functions();


#if 0
	/*! Lookup a generic-function in the _GenericFunctionTable by name
	 If errorp == true then throw an exception if the generic-function is not
	 found otherwise return nil */
	GenericFunction_sp findGenericFunction(Symbol_sp gfSym, bool errorp=true) const;
	/*! Associate a generic function with a symbol by name */
	GenericFunction_sp setf_findGenericFunction(Symbol_sp gfSym, GenericFunction_sp gf);
	/*! Clear all generic functions */
	void forgetAllGenericFunctions();
#endif



	/*! Lookup a class in the _ClassTable by name
	 If errorp == true then throw an exception if the class is not
	found otherwise return nil */
	Class_sp boot_findClass(Symbol_sp className, bool errorp=true) const;
	/*! associate a class in the _ClassTable by name */
	 Class_sp boot_setf_findClass(Symbol_sp className, Class_sp mc);

	/*! Move all _BootClassTable class definitions into a hash-table in *class-name-hash-table* */
	void switchToClassNameHashTable();


//	Class_sp classFromClassSymbol(Symbol_sp cid) const;
	Class_sp classFromClassName(const string& name);
	string classNameFromClassSymbol(Symbol_sp cid);
    public:
	void setBuiltInClassesInitialized(bool b) { this->_BuiltInClassesInitialized = b;};
	void throwIfBuiltInClassesNotInitialized();

	void defineMethod(const string& name, Symbol_sp classSymbol, Functoid* methoid, const string& arguments, const string& docString, bool autoExport );
    public:
	Symbol_sp errorUndefinedSymbol(const char* symbolName);

//	string getRenderFileName() { return this->_RenderFileName; };

    public:	// Hierarchy stuff
//	Hierarchy_sp defaultHierarchy() { return this->_DefaultHierarchy;};
//	void derive(T_sp tag, T_sp parent);
//	bool isA(T_sp tag, T_sp ancestor);
    public:
	//
	// Initialize the packages for this environment
	//
	void initializePackages();

    public: // Functions for generating errors to THROW
	T_sp error(const boost::format& fmt);

    public:	// Functions for manipulating special forms
	Symbol_sp defineSpecialOperator(const string& package, const string& formName, SpecialFormCallback cb, const string& args = "", const string& docstring = "");
	SpecialForm_sp specialFormOrNil(Symbol_sp sym);
    public:
	bool recognizesModule(const string& fileName);
	void addModule(const string& fileName);

	int	getRequireLevel() { return this->_RequireLevel;};
	void	pushRequireLevel() { this->_RequireLevel++; };
	void	popRequireLevel() { this->_RequireLevel--; };
#if 0 // moved condition handlers into the environment
	void 	pushConditionHandlers(Cons_sp handlers);
	void	popConditionHandlers();
#endif
	/*! Install a package using the newer PackageExposer idiom */
	void	installPackage(const PackageExposer* package );
	/*! Create nils for all classes that don't have them yet */
//	void	createNils();
	/*! When global initialization is locked then no more callbacks can be added
	 * and globals can be initialized
	 */
	bool isGlobalInitializationAllowed() { return this->_LockGlobalInitialization; };
	void	installGlobalInitializationCallback(InitializationCallback c);


	/*! Find symbol or nil */
	Symbol_sp findSymbol(string const& symbolName, T_sp optionalPackageDesignator ) const;


	/*! Find symbol or nil - symbolName can have package:[:]name form */
	Symbol_sp findSymbol(const string& symbolName /*, T_sp optionalPackageDesignator = nil */) const;

//	void createGlobalMacro(Symbol_sp sym, MacroCallback func);


    public:
	void defvar(Symbol_sp sym, T_sp obj);
	void defconstant(Symbol_sp sym, T_sp obj);

	// Environment stuff

	string allLocalNames();
	Cons_sp allLocalNamesAsCons();

	/*! The core REPL loop.  Set printResults to true if you want
	  each eval result to be printed. Return false if (quit) or (exit) was evaluated.
	  sin - an input stream designator
	*/
	T_mv readEvalPrint(T_sp sin, Environment_sp environ, bool printResults);
	T_mv readEvalPrintString(const string& string, Environment_sp environ, bool printResults );
	void readEvalPrintInteractive();


	/*! Find symbol or intern - symbolName can have package:[:]name form	*/
	Symbol_sp intern(const string& symbolName, T_sp optionalPackageDesignator);


	/*! Find symbol or intern - symbolName can have package:[:]name form. */
	Symbol_sp intern(const string& symbolName /*, T_sp optionalPackageDesignator = nil */); 

	/*! Find symbol or intern - symbolName can have package:[:]name form. */
	Symbol_sp intern(const string& symbolName , const string& pkgName );

	/*! Intern the symbol into the package with the given name */
	Symbol_sp internWithPackageName(const string& packageName, const string& symbolName);


	/*! Find the keyword symbol with the given name or intern it */
	Symbol_sp internKeyword(const string& keywordName);

	/*! Export a symbol to python */
	void exportToPython(Symbol_sp sym) const;


	Symbol_sp getClassSymbolForClassName(const string& symbolName);

	/*! Return all class names */
	StringSet_sp allClassNames();

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

	bool recognizesPackage(const string& packageName) const;
	Package_sp findPackage(const string& packageName) const;
	void inPackage(const string& packageName);
	void selectPackage(Package_sp pack);
	Package_sp getCurrentPackage() const;
	Package_sp makePackage(const string& packageName, list<string> const& nicknames, list<string> const& usePackages);
	bool usePackage(const string& packageName);

	Cons_sp	getBackTrace() const;
	string backTraceAsString(int numcol=50) const;

	Path_sp translateLogicalPathname(T_sp logicalPathName);

	/*! Return a path to an existing file indicated by logicalPathName.
	  If the file is in the current directory return it.
	  Otherwise use the *PATH* variable to search for paths that could contain the logicalPathName file */
	Path_sp translateLogicalPathnameUsingPaths(T_sp logicalPathName);

	/*! Parse the command line arguments and return a
	 * Cons of code if code was provided on the command line or through a script file
	 * Otherwise return nil
	 */
	void parseCommandLineArguments(int argc, char * argv[], bool compile);

	Cons_sp getCommandLineArguments() { return this->_Roots._CommandLineArguments; };

	/*! Create a StandardClass and leave the baseClass and instance variable
	 * definition for later
	 */
	StandardClass_sp defineStandardClass(Symbol_sp name, T_sp baseClassesDesignator, Cons_sp slotSpecifier );

	T_sp	createObjectOfClass(T_sp cl);
	Intrinsic_sp	getIntrinsic(const string& name);
	string		getMethodName(uint methodId);
	uint		getMethodId(const string& methodName);

//	Function_sp	lookupMethod(Symbol_sp, Class_sp classSymbol, T_sp receiver );



    public:

#if 0
	/*! Special function for accessing the contents of global variable _DATABASE_
	 */
	bool hasCandoDatabase();
	void setCandoDatabase(CandoDatabase_sp cdb);
	void	loadCandoDatabase(const string& fileName, uint verbosity = 0);
#endif

	void addToStarModulesStar(Symbol_sp sym);

	void initializeClassManager();
	void initializeEnvironments();

	/*! Parse a string and return the object that it represents
	 */
//	T_sp parseCodeString(const string& str);

	/*! Run the program, if _ExitStatus != 0 there was an error */
	void run();

	string errorMessage();

//	void setProgram(Cons_sp p) { this->_Program = p; };
	//!Wipes out namespace and fills it with new values
	void initializeEnvironment();

	void addClassNameToPackageAsDynamic(const string& package, const string& name, Class_sp cl);
	void addClass( Symbol_sp classSymbol, AllocatorFunctor* alloc, Symbol_sp base1ClassSymbol, Symbol_sp base2ClassSymbol = UNDEFINED_SYMBOL, Symbol_sp base3ClassSymbol = UNDEFINED_SYMBOL );
	void addClass( Symbol_sp classSymbol,Class_sp theClass,AllocatorFunctor* allocator);
//	void addClass( Symbol_sp classSymbol);

	string __repr__() const ;



	/*! From gdb - start trace on functions using function names separated by spaces.
	  If the names string is empty then dump all the functions currently being
	  traced */
	void gdb_trace_by_name(const char* names);

	/*! From gdb - stop trace on functions using function names separated by spaces.
	  if the names string is empty then untrace all functions. */
	void gdb_untrace_by_name(const char* name);


	void exposeCando();
	void exposePython();


	explicit Lisp_O();
	virtual ~Lisp_O();
    };




/*! Scoped change of lisp mode */
class PushLispMode
{
private:
    uint	_OldMode;
public:
    PushLispMode(uint newMode) {this->_OldMode = _lisp->_Mode; _lisp->_Mode = newMode;};
    virtual ~PushLispMode() { _lisp->_Mode = this->_OldMode;};
};







    void	initializeLisp();





    class LispHolder : public gctools::StackRoot
    {
    private:
        Lisp_sp _Lisp;
    public:
	/*! Pass the mpiProcess rank in (rank) or set to 0 if there is only one process */
	LispHolder(bool mpiEnabled, int mpiRank, int mpiSize );
        DECLARE_onStackScanGCRoots(); 


	virtual void startup(int argc, char* argv[], const string& appPathEnvironmentVariable );

	virtual ~LispHolder();
    };


    class ChangePackage : public gctools::StackBoundClass
    {
    private:	
	Package_sp	_SavedPackage;
    public:
	explicit ChangePackage(Package_sp newPackage);
	virtual ~ChangePackage();
    };



};




namespace core
{

    T_mv af_macroexpand_1(T_sp form, Environment_sp env);
    T_mv af_macroexpand(T_sp form, Environment_sp env);

    Cons_sp af_assoc(T_sp item, Cons_sp alist, T_sp key, T_sp test=cl::_sym_eq, T_sp test_not=_Nil<T_O>());

    Class_mv af_findClass(Symbol_sp symbol, bool errorp=true, Environment_sp env=_Nil<Environment_O>());
    Class_mv af_setf_findClass(T_sp newValue, Symbol_sp name, bool errorp, Environment_sp env );


    void af_stackMonitor();

    void af_error(T_sp err, Cons_sp initializers);
};



//TRANSLATE(core::Lisp_O);


#define INTERN(x) _lisp->internWithPackageName(x,CurrentPkg)



#endif //]


