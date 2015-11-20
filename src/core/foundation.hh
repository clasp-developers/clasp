

#ifndef FOUNDATION_H //[
#define FOUNDATION_H

#include "gcInterface.h"

// Switching to new AtomTree kinematics
// This breaks a lot of old code and depreciates a lot of other code
#define KINEMATICS 1

//
// While converting to subclasses for energy components
// use only a few components at a time
//
#define USE_ALL_ENERGY_COMPONENTS 1

//
// (C) 2004 Christian E. Schafmeister
//
//
// Turn on global debugging options here
// Otherwise comment this out and let individual modules
// set their debugging level
//
//

//
//	Turn debugging on or off
//		define DEBUG_LEVEL_FULL at the top of a .cc file
//
// #define DEBUG_MEMORY	// debug memory create/delete
#define DEFINE_ASSERTS 1
#ifdef SOURCE_DEBUG
#ifdef DEBUG_LEVEL_FULL_REFCOUNT //[
#define DEBUG_ON 1
#define CALLSTACK_ON 1
#define REFCOUNTDEBUG 1
#else                   //][
#ifdef DEBUG_LEVEL_FULL //[
#define DEBUG_ON 1
#define CALLSTACK_ON 1
#define DEFINE_ASSERTS 1
#else                     //][
#ifdef DEBUG_LEVEL_SILENT //[
#define CALLSTACK_SILENT 1
//#undef	DEBUG_ON
#else  //][
//#undef	CALLSTACK_ON
//#undef	CALLSTACK_SILENT
//#undef	DEBUG_ON
#endif //]
#endif //]
#endif //]
#endif

// Empty macros to scrape symbol and keyword symbol declarations from
// the source code
#define SYMBOL_EXPORT_SC_(x)
#define SYMBOL_SC_(x)
#define KEYWORD_SC_(x)

#define ARGUMENT_SC_(x)
#define ARGUMENT_EXPORT_SC_(x)

/*! Use this here in the header to declare an extern function if you want */
#define EXTERN_FN(x) extern T_sp fn_##x(Executable_sp exec, Cons_sp args, Environment_sp env, Lisp_sp lisp);

/*! Use this used to bind the C++ function fn_##x that will have the name (x) in Lisp (with "_" converted to "-") */
#define DEFUN(pkg, x) defun(pkg, #x, &fn_##x, ARGS_fn_##x, DOCS_fn_##x, _lisp);

/*! Use this used to bind the C++ function fn_##x that will have the name (x) in Lisp (with "_" converted to "-") */
#define DEFUN_EXPORT(pkg, x) defun(pkg, #x, &fn_##x, ARGS_fn_##x, DOCS_fn_##x, _lisp);

/*! Define a PAIR of accessor functions, the getter and setter */
#define DEFACCESSORS(pkg, x)                                \
  defun(pkg, #x, &fn_##x, ARGS_fn_##x, DOCS_fn_##x, _lisp); \
  _lisp->add_accessor_pair(_sym_##x, _sym_setf_##x);

/*! Define a PAIR of accessor functions, the getter and setter */
#define DEFACCESSORS_EXPORT(pkg, x)                         \
  defun(pkg, #x, &fn_##x, ARGS_fn_##x, DOCS_fn_##x, _lisp); \
  _lisp->add_accessor_pair(_sym_##x, _sym_setf_##x);

#define EXTERN_GENERIC(x) extern T_sp gf_##x(Executable_sp exec, Cons_sp args, Environment_sp env, Lisp_sp lisp);

/*! Use this in initializeCandoPrimitives to define a function
  This is a little more complicated than it needs to be to try and avoid unused variable warnings */
#define DEFGENERIC(pkg, x) defgeneric(pkg, #x, &gf_##x, ARGS_gf_##x, DOCS_gf_##x, _lisp);

/*! Use this in initializeCandoPrimitives to attach methods to the generic function */
#define DEFMETHOD(x, id) defmethod(_sym_##x, md_##x##id, ARGS_md_##x##id, DOCS_md_##x##id, _lisp);

//
// Define this if you want to debug energy evaluation
//
#define TURN_ENERGY_FUNCTION_DEBUG_ON 1

//
// For Production code set PRODUCTION_CODE to 1
// this will remove some tests
// that are in the non-debugging code for catching
// very rare conditions but slow things down
//

#define PRODUCTION_CODE 1

/*!
 * Log all steps of score evaluation
 * This really slows things down
 */
#define DEBUG_SCORE_EVALUATION 0

/*!
 * Save a text description of a alchemist state within every AlchemistState object
 * This slows things down
 */
#ifdef DEBUG_ON
#define SAVE_STATE_DESCRIPTION 1
#else
#define SAVE_STATE_DESCRIPTION 0
#endif

//
//#define	DEBUG_LEVEL_FULL_REFCOUNT
//
//
// To just debug object creation/destruction define REFCOUNTDEBUG
//#define	REFCOUNTDEBUG

//
// Turn on DEBUG_SIGNALS to log commands signal/catchSignal/propagate calls
// even when no other debugging is going on
#define DEBUG_SIGNALS

/*!
 * In Archive object data can either be stored as separate strings
 * or it can be stored as in a MultiStringBuffer
 *
 * Set to 1 if you want to use a MultiStringBuffer 
 * Set to 0 if you don't
 */
#define ARCHIVE_MULTI_STRING_BUFFER 1

typedef unsigned int uint;

/*! Symbol id's are uints that allow quick lookup of symbols from C++.
  See Lisp_O::createPredefinedSymbol */
typedef uint sid;

#include "serializerNode.fwd.h"

namespace core {
extern bool _ClassesAreInitialized;
typedef uint handleType;
const uint handleNumberFlag = 1 << 31;
const uint handleNumberMask = handleNumberFlag - 1;
const uint MaxHandle = handleNumberFlag - 1;
const uint IllegalHandle = MaxHandle;
const handleType EmptyStringHandle = 0;
const handleType UniqueIdHandle = 1;
};

/*! Associate a namespace name with a Package.
  This is scraped out of the code by "registerClasses.py"
*/

#define NAMESPACE_PACKAGE_ASSOCIATION(x, y, z) static const std::string y = z;

//
// Turn this on when you want to track changes to an object
// in the debug log
//
// Use OBJECT_VP(,(shift())) macro to write data into the log tagged with the object name
//
#ifdef SOURCE_DEBUG
#define DEBUG_OBJECT_ON
#endif

#define UndefinedUnsignedInt UINT_MAX

//
// Define exceptions
//

/*!\defgroup CompileTimeDebugging Compile time debugging*/
/*@{*/
/*!
 * The macro SOURCE_DEBUG must be defined for any source level debugging
 * to take place.
 *
 * To control the compile time debugging level in the code
 * define the macros: DEBUG_LEVEL_FULL, DEBUG_LEVEL_SILENT, and DEBUG_LEVEL_NONE
 * on the FIRST LINE of a .cc file.  In the foundation.h file it will be expanded
 * into DEBUG_ON, CALLSTACK_ON and CALLSTACK_SILENT defines that will be interpreted by
 * exceptions.h to turn on/off VP0 macros and _FUNCTION_TRACE and _BLOCK_TRACE messages.
 *
 * 	- DEBUG_LEVEL_FULL_REFCOUNT
 * 		- Turns on debugging messages (VP0) and callstack tracing to the
 * 		output file. Also turn on reference counting debugging.
 * 	- DEBUG_LEVEL_FULL
 * 		- Turns on debugging messages (VP0) and callstack tracing to the
 * 		output file.
 * 	- DEBUG_LEVEL_SILENT
 * 		- No debugging output (VP0 and call traces) will be sent to the
 * 			debug output file but a call stack
 * 			will be maintained for backtracing
 * 			in the event of a crash.
 * 	- DEBUG_LEVEL_NONE
 * 		- All compile time debugging will be disabled
 *
 * Depreciated: TURN_DEBUG_ON, TURN_DEBUG_OFF
 */
/*@}*/

//
// Depreciated, switch to DEBUG_LEVEL_FULL, DEBUG_LEVEL_BACKTRACE, DEBUG_LEVEL_NONE
//
#ifdef TURN_DEBUG_ON
//#define	DEBUG_ON	1
//#define	CALLSTACK_ON	1
#endif

//
//
//	For debugging memory
//
//
#ifdef CDEBUG_FULL
#define REFCOUNTDEBUG
#endif

#ifdef _DEBUG
#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>
#define REFCOUNTDEBUG
#endif

//
// Trap failed BOOST_ASSERT invocations
// so that they call my code and throw a proper exception
//
#define BOOST_ENABLE_ASSERT_HANDLER 1

//
//
// If we are using boost::python then include a few extra files
//
//

#include "boostSmartPointers.h"

#define DEFINE_O_SHARED(zclass)                      \
  class zclass##_O;                                  \
  typedef boost::shared_ptr<zclass##_O> zclass##_sp; \
  typedef boost::shared_ptr<zclass##_O> zclass##_rp;

#define DEFINE_O_WEAK(zclass) \
  class zclass##_O;           \
  typedef boost::weak_ptr<zclass##_O> zclass##_wp;

#define DEFINE_O_SMART_POINTERS(zclass)              \
  class zclass##_O;                                  \
  typedef boost::shared_ptr<zclass##_O> zclass##_sp; \
  typedef boost::shared_ptr<zclass##_O> zclass##_rp; \
  typedef boost::weak_ptr<zclass##_O> zclass##_wp;

// This ensures that smart pointers are only declared once
// for each compilation
#define SMART(zclass) \
  DEFINE_O_SMART_POINTERS(zclass)

#define FORWARD(zclass) \
  DEFINE_O_SMART_POINTERS(zclass)

#define _ARG(ty, env, pkg, name) from_object<ty>::convert(env->lookup(pkg, name))

#include <string>
#include <vector>
#include <queue>
#include "boost/format.hpp"
using namespace std;

namespace core {
class T_O;
typedef boost::shared_ptr<T_O> T_sp;
typedef boost::weak_ptr<T_O> T_wp;
}

namespace core {

typedef double real; // For now real is double, may be float in the future

typedef vector<string> VectorStrings;
typedef vector<int> VectorInts;
typedef vector<double> VectorDoubles;

//
//
// prototypes for functions defined in foundation.cc

class StringStack {
private:
  vector<string> parts;

public:
  void clear() { this->parts.clear(); };
  void push(const string &s) { this->parts.push_back(s); };
  void pop();
  string all(const string &separator);
};

/*! Escape all white space (spaces, cr, tab) */
string escapeWhiteSpace(const string &inp);
/*! Unescape all whitespace (spaces/cr/tab) */
string unEscapeWhiteSpace(const string &inp);

string trimWhiteSpace(const string &inp);
string stripCharacters(const string &inp, const string &strip);

vector<string> split(const string &str, const string &delim = " \t");
void tokenize(const string &str,
              vector<string> &tokens,
              const string &delimiters = " ");
void queueSplitString(const string &str,
                      queue<string> &tokens,
                      const string &delimiters = " ");

string stringUpper(const string &s);
string stringUpper(const char *s);

string concatenateVectorStrings(VectorStrings strs);
void appendVectorStrings(VectorStrings &app, VectorStrings strs);

bool wildcmp(string sWild,
             string sRegular);

/*
 * Documentation macros
 */

#define DOC_METHOD()
#define DOC_FUNCTION()

typedef struct {
  string _Key;
  int _Enum;
} NullTerminatedEnumAssociation;

typedef unsigned int AtomFlags; // Minimum 32 bits

#ifndef MAX
#define MAX(x, y) ((x) < (y) ? (y) : (x))
#endif //MAX
#ifndef MIN
#define MIN(x, y) ((x) > (y) ? (y) : (x))
#endif //MIN

typedef unsigned int AtomHandle; // Implement the AtomHandle as an index into
                                 // a vector of atoms that maintains a constant
                                 // order even when the Residue is copied.

typedef vector<AtomHandle> VectorAtomHandle;

// Used to define atom handles to the connect atoms
// in the previous and next residues
#define ATOMHANDLE_PREV0 -11
#define ATOMHANDLE_PREV1 -12
#define ATOMHANDLE_PREV2 -13
#define ATOMHANDLE_NEXT0 -21
#define ATOMHANDLE_NEXT1 -22
#define ATOMHANDLE_NEXT2 -23

#ifdef WIN32
//#define	assert(x)   {if (!(x)) abort(); }
// # p r a g m a warning( disable : 4290 )
#endif

class Lisp_O;
typedef boost::shared_ptr<Lisp_O> Lisp_sp;
typedef boost::weak_ptr<Lisp_O> Lisp_wp;
class Executable_O;
typedef boost::shared_ptr<Executable_O> Executable_sp;
class Cons_O;
typedef boost::shared_ptr<Cons_O> Cons_sp;
class Environment_O;
typedef boost::shared_ptr<Environment_O> Environment_sp;
class Symbol_O;
typedef boost::shared_ptr<Symbol_O> Symbol_sp;
typedef void (*ExposeCandoFunction)(Lisp_sp);
typedef void (*ExposePythonFunction)(Lisp_sp);
typedef T_sp (*FormCallback)(Executable_sp, Cons_sp args, Environment_sp environment, Lisp_sp lisp);
typedef T_sp (*SpecialFormCallback)(Cons_sp, Environment_sp, Lisp_sp);
typedef T_sp (*MacroCallback)(Executable_sp, Cons_sp args, Environment_sp environment, Lisp_sp lisp);
typedef void (*MakePackageCallback)(string const &packageName, Lisp_sp);
typedef void (*ExportSymbolCallback)(Symbol_sp symbol, Lisp_sp);

class LispObject {
private:
  Lisp_wp _WeakLisp;

public:
  Lisp_sp lisp() { return this->_WeakLisp.lock(); };
  LispObject(const Lisp_sp &lisp) : _WeakLisp(lisp){};
  LispObject(bool bogus) { this->_WeakLisp.reset(); /* Constructor used only when WeakLisp will be set later */ };
  void setLisp(const Lisp_sp &lisp) { this->_WeakLisp = lisp; };
};

/*! Search for multiple occurances of a string and replace it
 * \param str The string that is modified
 * \param search The string to search for
 * \param replace The string to replace with
 */
string searchAndReplaceString(const string &str, const string &search, const string &replace, Lisp_sp lisp);
};

typedef core::T_sp (*AllocatorCallback)(core::Lisp_sp);

//
//
//

//
// These macros are scraped by an external program
// and used to create "initClasses.inc" which includes
// calls to initialize anything you want before the program
// runs.
//
//		__INITIALIZE_PYTHON and __INITIALIZE_PYTHON_AFTER
//		initialize the Python interface after all of the classes have been registered
//
//		__INITIALIZE and __INITIALIZE_AFTER
//		initialize other stuff after all the Python interfaces have been registered
#define __INITIALIZE_PYTHON(x)          // Do nothing
#define __INITIALIZE_PYTHON_AFTER(x, y) // Do nothing
#define __INITIALIZE(p, x)              // Do nothing
#define __INITIALIZE_AFTER(x, y)        // Do nothing

namespace core {

/*! Create the class hierarchy
 */
extern void registerAllClasses();
// extern void throwIfClassesNotInitialized();

/*! Expose c++ to CandoScript
 */
class Lisp_O;
typedef boost::shared_ptr<Lisp_O> Lisp_sp;
extern void exposeClassesAndDefineNils(Lisp_sp);

/*! Expose c++ to Python
 */
extern void initializePythonScript();

// ---------------------------------------------------
//  Object oriented programming stuff
//  ------

class Cons_O;
typedef boost::shared_ptr<Cons_O> Cons_sp;

class MetaClass_O;
typedef boost::shared_ptr<MetaClass_O> MetaClass_sp;

class Executable_O;

class Symbol_O;
typedef boost::shared_ptr<Symbol_O> Symbol_sp;

class SymbolToEnumConverter_O;
typedef boost::shared_ptr<SymbolToEnumConverter_O> SymbolToEnumConverter_sp;
};

namespace core {

class Functoid {
public:
  virtual T_sp invoke(Executable_sp e, Cons_sp args, Environment_sp environment, Lisp_sp lisp);
  virtual ~Functoid(){};
};

class DebugStream;

/* Callbacks that initialize the Lisp environment have this structure*/

typedef void (*InitializationCallback)(Lisp_sp);

void error(const char *errorString);
void errorFormatted(boost::format fmt);
void errorFormatted(const char *errorString);
void errorFormatted(const string &msg);

//
//  Lisp class access functions for when we only have a forward
//  definition for the Lisp_O class and for any other object
//
Lisp_sp lisp_lisp(Lisp_sp lisp);
Lisp_sp lisp_fromObject(T_sp obj);
void lisp_throwUnexpectedType(Lisp_sp lisp, T_sp offendingObject, uint foundTypeId, uint expectedTypeId);
void lisp_throwLispError(Lisp_sp l, const string &str);
void lisp_throwLispError(Lisp_sp l, const boost::format &fmt);
core::T_sp lisp_true(Lisp_sp lisp);
core::T_sp lisp_false(Lisp_sp lisp);
bool lisp_BuiltInClassesInitialized(Lisp_sp lisp);
bool lisp_BuiltInClassesInitialized(Lisp_sp lisp);
bool lisp_NilsCreated(Lisp_sp lisp);
Symbol_sp lisp_symbolNil(Lisp_sp lisp);
MetaClass_sp lisp_findClassBySIDOrNil(Lisp_sp lisp, uint SID);
void lisp_exposeClass(Lisp_sp lisp, const string &className, ExposeCandoFunction exposeCandoFunction, ExposePythonFunction exposePythonFunction);
void lisp_addClassByClassSIDToEnvironment(Lisp_sp lisp, const string &package, const string &name, uint classSID);
void lisp_addClass(Lisp_sp lisp, const string &package, const string &name, uint classSID, AllocatorCallback cb, uint baseClassSID1, uint baseClassSID2 = UndefinedUnsignedInt);
void lisp_throwIfBuiltInClassesNotInitialized(Lisp_sp lisp);
string lisp_classNameFromClassSID(Lisp_sp lisp, uint classSID);
MetaClass_sp lisp_classFromClassSID(Lisp_sp lisp, uint classSID);
void lisp_setGlobalInt(Lisp_sp lisp, const string &package, const string &n, uint val);
bool lisp_isClassName(Lisp_sp lisp, const string &n);
uint lisp_getClassSIDForClassName(Lisp_sp lisp, const string &n);
bool lisp_subClassOrder(Lisp_sp lisp, uint baseClassSID, uint classSID);
string lisp_convertCNameToLispName(Lisp_sp lisp, string const &cname, bool convertUnderscoreToDash = true);
T_sp lisp_onil(Lisp_sp lisp);
Cons_sp lisp_cnil(Lisp_sp lisp);
T_sp lisp_apply(Lisp_sp lisp, T_sp funcDesig, Cons_sp args);
void lisp_defun(Lisp_sp lisp, const string &packageName, const string &name,
                Functoid *, const string &arguments = "", const string &docstring = "", bool autoExport = true);
void lisp_defgeneric(Lisp_sp lisp, const string &packageName, const string &name,
                     Functoid *, const string &arguments = "", const string &docstring = "", bool autoExport = true);
void lisp_defmethod(Lisp_sp lisp, sid gfSID, Functoid *func, const string &arguments, const string &docstring);

void lisp_defineSingleDispatchMethod(Lisp_sp lisp, const string &name, uint classSID,
                                     Functoid *, const string &arguments = "", const string &docstring = "", bool autoExport = true);

void lisp_defineSetf(Lisp_sp lisp, const string &name, uint classSID,
                     Functoid *, const string &arguments = "", const string &docstring = "", bool autoExport = true);

core::T_sp lisp_hiddenBinderLookup(Lisp_sp lisp, Symbol_sp sym);

//
// You define what is being sent to the debug log using these constants
#define DEBUG_CPPCODE 1
#define DEBUG_LISP 2
#define DEBUG_SCRIPT 4
#define DEBUG_SHOUT 8
#define DEBUG_ALL (DEBUG_SCRIPT | DEBUG_LISP | DEBUG_CPPCODE)
bool lisp_debugIsOn(Lisp_sp lisp, const char *fileName, uint debugFlags = DEBUG_CPPCODE);
void lisp_debugLogWrite(Lisp_sp lisp, const char *fileName, const char *funcName, uint lineNumber, const boost::format &message, uint debugFlags = DEBUG_CPPCODE);

DebugStream *lisp_debugLog(Lisp_sp lisp);
T_sp lisp_ocar(Lisp_sp lisp, Cons_sp args);
T_sp lisp_ocadr(Lisp_sp lisp, Cons_sp args);
T_sp lisp_ocaddr(Lisp_sp lisp, Cons_sp args);
Symbol_sp lisp_internKeyword(Lisp_sp lisp, const string &name);
Symbol_sp lisp_intern(Lisp_sp lisp, const string &name);
Symbol_sp lisp_intern(Lisp_sp lisp, const string &pkgName, const string &symName);
sid lisp_lookupSID(Lisp_sp lisp, Symbol_sp sym);
string symbol_fullName(Symbol_sp s);
void lisp_logException(Lisp_sp lisp, const char *file, const char *fn, int line, const char *structure, const string &message);
bool lisp_isGlobalInitializationAllowed(Lisp_sp lisp);
void lisp_installGlobalInitializationCallback(Lisp_sp lisp, InitializationCallback initGlobals);
Symbol_sp lisp_predefinedSymbol(Lisp_sp lisp, uint symId);
void lisp_hiddenBinderExtend(Lisp_sp lisp, Symbol_sp sym, T_sp obj);
void lisp_extendSymbolToEnumConverter(Lisp_sp lisp, SymbolToEnumConverter_sp conv, const string &name, const string &archiveName, int value);
int lisp_lookupEnumForSymbol(Lisp_sp lisp, uint predefSymId, T_sp symbol);
core::T_sp lisp_lookupSymbolForEnum(Lisp_sp lisp, uint predefSymId, int enumVal);
};

#include "exceptions.h"

#include <boost/random.hpp>

namespace core {

typedef unsigned char ubyte;

/* Callbacks that return a boolean value have this structure */
typedef bool (*BoolReturnCallback)(Lisp_sp);

/* Callbacks that return an integer value have this structure */
typedef int (*IntReturnCallback)(Lisp_sp);

/* Callbacks that return an object have this structure */
typedef T_sp (*ObjectReturnCallback)(Lisp_sp);

/*
 * These callback types are used to redirect screen output depending if we are
 * on a console or using WxWidgets
 */

typedef void (*PrintvWriteCallback)(const Lisp_sp &lisp, const char *outputBuffer);
typedef void (*PrintvWriteCharCallback)(const Lisp_sp &lisp, char outputChar);
typedef void (*PrintvFlushCallback)(const Lisp_sp &lisp);

string trimSourceFilePathName(string const &fullPathName);

/*! Set the name of the file that should receive output
	 * This isn't used in a GUI
	 */
void printvSetOutputFileName(const string &fileName, const Lisp_sp &lisp);
/*! Set the prefix that is written at the begining of every line of output
	 */
void printvPushPrefix(const string &prefix, Lisp_sp lisp);
void printvPopPrefix(Lisp_sp lisp);
void printvShowPrefix(const Lisp_sp &lisp);
/*! Print a line of output with a prefix set by printvPushPrefix
	 * Use this instead of printf
	 */
//void	printv(const Lisp_sp& lisp, const char* fmt, ... );
void print(const Lisp_sp &lisp, const string &text);
void println(const Lisp_sp &lisp, const string &text);

void printv_prompt(const Lisp_sp &lisp);

void foundation_printv_write(const Lisp_sp &lisp, const char *outputBuffer);
void foundation_printv_writeChar(const Lisp_sp &lisp, char c);
void foundation_printv_flush(const Lisp_sp &lisp);

//
// Thread safety
//

#define IN_PLUG_PREFIX '-'
#define OUT_PLUG_PREFIX '+'
};

#ifdef USEBOOSTPYTHON
#include <cctype>
#include "mainBoostPython.h"
#endif

#endif //]
