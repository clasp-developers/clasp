/*
    File: foundation.h
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
#ifndef CORE_H //[
#define CORE_H

// Debug flow control

/*! Turn this on to force turn on xxx_ASSERT messages in release code*/
//#ifndef DEBUG_ASSERT
//#define DEBUG_ASSERT
//#endif

// To debug memory usage turn this on and then you can mark
// objects as they are allocated with an integer using gctools::MARKER
#ifdef USE_BOEHM
//#define USE_BOEHM_MEMORY_MARKER
#endif
#ifndef APPLICATION_CONFIG
//#error "You must define the APPLICATION_CONFIG as something like <clasp/main/application.config>"
#define APPLICATION_CONFIG <clasp/main/application.config>
#endif

// Load the waf config file
#include "config.h"
// Checkif we are running the static analyzer
// Modify the preprocessor settings for the static analyzer
// Turn on USE_MPS and turn off USE_BOEHM
#ifdef RUNNING_GC_BUILDER
#undef USE_BOEHM
#define USE_MPS
#endif
/*! Configure the application Clasp or Cando currently */
#include APPLICATION_CONFIG

/*! Old way of doing #= and ## used alists which are slow
  Switch to hash-tables to speed things up */
#define USE_SHARP_EQUAL_HASH_TABLES 1

#ifdef DEBUG_RELEASE
#define DONT_OPTIMIZE_WHEN_DEBUG_RELEASE __attribute__((optnone))
#else
#define DONT_OPTIMIZE_WHEN_DEBUG_RELEASE 
#endif
#define DONT_OPTIMIZE_ALWAYS __attribute__((optnone))
#define ALWAYS_INLINE __attribute((always_inline))
#define NOINLINE __attribute__((noinline))
#define MAYBE_INLINE __attribute__((noinline))

#define RUN_ALL_FUNCTION_NAME "RUN-ALL"
#define CLASP_CTOR_FUNCTION_NAME "CLASP-CTOR"

#ifdef CLASP_THREADS
#include <atomic>
#endif

namespace std {
class type_info;
};

//
// USE_TAGGED_PTR_P0  determines whether a p0 pointer,  the most-derived-pointer is stored
//         in the tagged_ptr.   If you turn it on then tagged_ptr uses twice as much
//         memory
//
//
// Don't use P0 ptr with BOEHM
#ifdef USE_MPS
//#define USE_TAGGED_PTR_P0 1
#endif

#define DLL_PUBLIC __attribute__((visibility("default")))
#include <limits>
#include <typeinfo>

#ifdef CLASP_THREADS
#include <pthread.h>
#endif

#include <map>

#define VARARGS

#include <clasp/core/gcInterface.h>
//#include <cstdio>

//! Macro for attribute that causes symbols to be exposed
#define ATTR_WEAK __attribute__((weak))

#if defined(DEBUG_RECURSIVE_ALLOCATIONS)
#define DO_DEBUG_MPS_RECURSIVE_ALLOCATIONS() gctools::RecursiveAllocationCounter rac;
#else
#define DO_DEBUG_MPS_RECURSIVE_ALLOCATIONS()
#endif


#define clasp_unlikely(x) __builtin_expect(!!(x), 0)
#define clasp_likely(x) __builtin_expect(!!(x), 1)
#define UNLIKELY(x) clasp_unlikely(x)
#define LIKELY(x) clasp_likely(x)
// unlikely_if in lowercase to differentiate from LIKELY_if
#define unlikely_if(x) if (UNLIKELY(x))
// LIKELY_if in caps to make it stand out
#define LIKELY_if(x) if (LIKELY(x))

typedef std::size_t class_id;


/*! Configure architecture dependent types */
#include <clasp/core/configure_clasp.h>

// BASE TYPES CL_POINTER_T AND CL_VALUE_T
// A 64bit or 32bit value holding representing a Lisp Object - either
// tagged or untagged.
//typedef uintptr_clasp_t cl_pointer_t;
//typedef uintptr_clasp_t cl_value_t;

/*! Use old Conditions system baked into C++
  OLD_CONDITIONS = 1
  otherwise OLD_CONDITIONS = 0
*/
#define OLD_CONDITIONS 0

/*! Check for NULLs being put into data structures throughout the code
  - this is very slow but will trap errors - turn off unless debugging
  set to 1 to turn on checking of NULLS using invokePossibleMultipleValueFunction
  set to 0 if you want to turn it off
*/
#define DEBUG_TRAP_NULLS 0

/*! Set to 1 to support CLOS_STREAMS
  Undefine if you dont want CLOS_STREAMS
 */
#define CLOS_STREAMS 1

//! Use factory functions in chem
#define INIT_TO_FACTORIES 1

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

//#undef SOURCE_DEBUG
//#define ENABLE_PROFILING 1
//
// Turn on INIT_DEBUG if you want LISP initialization log messages
// - there are lots and startup will be very slow
#undef INIT_DEBUG

// Debug CLOS 0 - no debug 1 - slight debug 2 - verbose debug
#define DEBUG_CLOS 0 // 2

/*! If CLOS is being implemented */
#define CLOS 1

#define MULTIPLE_VALUES_LIMIT 64

#ifdef SOURCE_DEBUG
 #ifdef DEBUG_LEVEL_FULL //[
  #define DEBUG_ON 1
  #define CALLSTACK_ON 1
  #define DEBUG_ASSERT 1
 #else                     //][
  #ifdef DEBUG_LEVEL_SILENT //[
   #define CALLSTACK_SILENT 1
  #endif //]
 #endif //]
#endif

/*! Use this in initializeCandoPrimitives to define a function
  This is a little more complicated than it needs to be to try and avoid unused variable warnings */
#define DEFGENERIC(pkg, x) defgeneric(pkg, #x, &gf_##x, ARGS_gf_##x, DOCS_gf_##x, _lisp);

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

#include <cstddef>

typedef unsigned char uchar;
typedef unsigned int uint;
typedef size_t UnknownType;
typedef unsigned char byte;
typedef size_t _Index; //

struct size_t_pair {
  size_t start;
  size_t end;
  size_t length;
};

#if defined(OLD_SERIALIZE)
#include <serializerNode.fwd.h>
#endif

namespace core {
  extern bool _ClassesAreInitialized;
  typedef uint handleType;
  const uint handleNumberFlag = (uint)(1 << 31);
  const uint handleNumberMask = handleNumberFlag - 1;
  const uint MaxHandle = handleNumberFlag - 1;
  const uint IllegalHandle = MaxHandle;
  const handleType EmptyStringHandle = 0;
  const handleType UniqueIdHandle = 1;

};

/*! Associate a namespace name with a Package.
  This is scraped out of the code by "registerClasses.py"
*/


#define UndefinedUnsignedInt UINT_MAX
#define UNDEF_UINT UndefinedUnsignedInt

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
 * 	- DEBUG_LEVEL_FULL
 * 		- Turns on debugging messages (LOG) and callstack tracing to the
 * 		output file.
 * 	- DEBUG_LEVEL_SILENT
 * 		- No debugging output (LOG and call traces) will be sent to the
 * 			debug output file but a call stack
 * 			will be maintained for backtracing
 * 			in the event of a crash.
 * 	- DEBUG_LEVEL_NONE
 * 		- All runtime debugging will be disabled
 *
 */
/*@}*/




#include <string>
#include <vector>
#include <queue>
#include <list>
#include <map>
#include <set>
#include <ostream>
#include <iostream>
#include <sstream>

using string = std::string;
using type_info = std::type_info;
template <typename X, typename Y>
using map = std::map<X, Y>;
template <typename X>
using vector = std::vector<X>;
using stringstream = std::stringstream;
template <typename X, typename Y>
using pair = std::pair<X, Y>;
template <typename X>
using list = std::list<X>;
template <typename X, typename Y>
using multimap = std::multimap<X, Y>;
template <typename X>
using set = std::set<X>;
template <typename X>
using deque = std::deque<X>;



#ifdef WIN32
#include <limits>
typedef __int64 LongLongInt;
#define LongLongMax LLONG_MAX
#define myMAXFLOAT FLT_MAX
#define atoll(x) (_atoi64(x))
#elif __PGI
#include <math.h>
#include <limits.h>
typedef long long int LongLongInt;
#define LongLongMax LONGLONG_MAX
#define myMAXFLOAT HUGE
#else
#include <math.h>
typedef long long int LongLongInt;
#define LongLongMax LLONG_MAX
#define myMAXFLOAT HUGE
#endif

#define LongLongMaxScale 4096 // was 256
#define LongLongIntBoundary LongLongMax / LongLongMaxScale



/* --------------------------------------------------
   --------------------------------------------------

   Memory Pool System stuff

   --------------------------------------------------
   --------------------------------------------------
*/
/*! Usage
  STATIC_ROOT_FRAME_BEGIN(StaticFrame) {
  ...
  }
  STATIC_ROOT_FRAME_BEGIN(StaticFrame,staticFrame);
*/
#define STATIC_ROOT_FRAME_BEGIN(X) struct X : public gctools::HeapRoot
#define STATIC_ROOT_FRAME_END(X, N) \
  ;                                 \
  static X *N = NULL;               \
  if (!N)                           \
    N = new X();

void dbg_hook(const char *errorString);

namespace core {

//    bool internal_isTrue(const void* T_spPtr);
};

/*! Class registration code - each registered class gets a unique number associated with it */

#include <boost/operators.hpp>

namespace gctools {
/*! Inheriting from this class indicates that the derived class
      includes smart_ptr's but is only ever instantiated on the stack.
      This means the conservative garbage collector will see the smart_ptr's
      when it scans the stack.
    */
class StackBoundClass {};

/*! Inheriting from this class indicates that the derived class
      contains absolutely no smart_ptrs or weak_ptrs either directly
      or indirectly - DON'T PUT ANY POINTERS IN THE DERIVED CLASS */
class GCIgnoreClass {};
};

namespace reg {

  struct null_type : public gctools::GCIgnoreClass {};
  class_id const unknown_class = (std::numeric_limits<class_id>::max)();
  class type_id
    : public boost::less_than_comparable<type_id> {
  public:
  type_id()
    : id(&typeid(null_type)) {}

  type_id(std::type_info const &id)
    : id(&id) {}

    bool operator!=(type_id const &other) const {
      return *id != *other.id;
    }

    bool operator==(type_id const &other) const {
        return *id == *other.id;
    }

    bool operator<(type_id const &other) const {
      return id->before(*other.id);
    }

    char const *name() const {
      return id->name();
    }

    std::type_info const *get_type_info() const { return this->id; };

  private:
    std::type_info const *id;
    };

  class_id allocate_class_id(type_id const &cls);
  template <class T>
    struct registered_class {
      static class_id const id;
    };
  template <class T>
    class_id const registered_class<T>::id = allocate_class_id(typeid(T));
  template <class T>
    struct registered_class<T const>
    : registered_class<T> {};
};

/*! This function is provided by the main.cc file */
std::string program_name();

namespace core {
  class T_O;
  class Symbol_O;
  class Cons_O;
  class General_O;

  [[noreturn]]void lisp_error_sprintf(const char* file, int lineno, const char* function, const char* fmt, ... );
  [[noreturn]]void lisp_errorDereferencedNonPointer(core::T_O *objP);
  [[noreturn]]void lisp_errorBadCast(class_id toType, class_id fromType, core::T_O *objP);
  [[noreturn]]void lisp_errorBadCastFromT_O(class_id toType, core::T_O *objP);
  [[noreturn]]void lisp_errorBadCastToFixnum(class_id fromType, core::T_O *objP);
  [[noreturn]]void lisp_errorBadCastFromT_OToCons_O(core::T_O *objP);
  [[noreturn]]void lisp_errorBadCastFromSymbol_O(class_id toType, core::Symbol_O *objP);
  [[noreturn]]void lisp_errorUnexpectedType(class_id expectedTyp, class_id givenTyp, core::T_O *objP);
  [[noreturn]]void lisp_errorUnexpectedNil(class_id expectedTyp);
  [[noreturn]]void lisp_errorDereferencedNil();
  [[noreturn]]void lisp_error_no_stamp(void* obj);
  [[noreturn]]void lisp_errorDereferencedUnbound();
  [[noreturn]]void lisp_errorIllegalDereference(void *v);
  [[noreturn]]void lisp_errorExpectedList(core::T_O* objP);

  template <typename To, typename From, typename ObjPtrType>
    [[noreturn]]void lisp_errorCast(ObjPtrType objP) {
    class_id to_typ = reg::registered_class<To>::id;
    class_id from_typ = reg::registered_class<From>::id;
    lisp_errorBadCast(to_typ, from_typ, reinterpret_cast<core::T_O *>(objP));
    __builtin_unreachable();
  }
};

namespace core {
  class MultipleValues;
  MultipleValues &lisp_multipleValues();
//  MultipleValues &lisp_callArgs();
};

extern void clasp_mps_debug_allocation(const char *poolName, void *base, void *objAddr, int size, int kind);
extern void clasp_mps_debug_fix1_before(void *base, void *smartAddr);
extern void clasp_mps_debug_fix_before(void *pbase, void *px, int offset);
extern void clasp_mps_debug_fix_after(void *pbase, void *px);
extern void clasp_mps_debug_container(const char *ctype, const char *name, int size);
//extern void clasp_mps_debug_scan_object(gctools::GCObject*  obj);

#define LCC_MACROS
#include <clasp/core/lispCallingConvention.h>
#undef LCC_MACROS

#if 1 // from here

/*! Should be thread_local on linux or __thread on OS X */
#define THREAD_LOCAL thread_local

#include <clasp/core/scrape.h>
#include <clasp/gctools/memoryManagement.h>
#include <clasp/gctools/gcalloc.h>
#include <clasp/gctools/containers.h>
#include <clasp/gctools/threadLocalStacks.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/mpPackage.fwd.h>
#include <clasp/gctools/threadlocal.h>

#include <clasp/gctools/multiple_value_pointers.h>


namespace gctools {
  void register_thread(mp::Process_sp process, void* stackTop);
  void unregister_thread(mp::Process_sp process);
};
   


#if 0
namespace dummy_namespace {
#pragma clang diagnostic push
#pragma clang diagnostic warning "-Wc++98-compat"
/*! These tests can be used to determine if there is a non-trivial copy constructor
    or destructor that would prevent smart_ptr and multiple_values from being
    passed in registers or returned in registers.

    Uncomment the #if 0 and look for copy constructors, destructors
    IGNORE constructors - they are harmless for passing in registers.
*/


/*! If this union generates a compile-time error then multiple_values isn't
      trivial and it will be passed as a pointer to the struct in memory
      rather than in registers. */
  union multiple_value_ptr_union_generates_a_compile_time_error_then_multiple_values_isnt_trivial {
    gctools::multiple_values<core::T_O> _multiple_values;
    uintptr_clasp_t _uintptr;
  };

  /*! If this union generates a compile-time error then smart_ptr isn't
      trivial and it will be passed as a pointer to the struct in memory
      rather than in registers. */
  union smart_ptr_union_generates_a_compile_time_error_then_smart_ptr_isnt_trivial {
    gctools::smart_ptr<core::T_O> _smart_ptr;
    uintptr_clasp_t _uintptr;
  };
#pragma clang diagnostic pop
};
#endif


namespace core {
class Instance_O;
typedef gc::smart_ptr<Instance_O> Instance_sp;
class FuncallableInstance_O;
typedef gc::smart_ptr<FuncallableInstance_O> FuncallableInstance_sp;

#define LCC_PROTOTYPES
#include <clasp/core/lispCallingConvention.h>
#undef LCC_PROTOTYPES
};

namespace core {
core::T_sp lisp_true();
uint lisp_hash(uintptr_clasp_t v);
};





//#include <clasp/gctools/gcweak.h>

#include <clasp/gctools/managedStatic.h>

#include <clasp/gctools/gc_interface.h>


class _RootDummyClass;


#define EXTERN_SYMBOL(sym) extern core::Symbol_sp& _sym_##sym;


namespace core {

typedef double real; // For now real is double, may be float in the future

typedef vector<string> VectorStrings;
typedef vector<int> VectorInts;
typedef vector<double> VectorDoubles;

//
//
// prototypes for functions defined in foundation.cc

class StringStack : public gctools::GCIgnoreClass {
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
                      std::queue<string> &tokens,
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

struct NullTerminatedEnumAssociation {
  NullTerminatedEnumAssociation(const string &key, int eval) : _Key(key), _Enum(eval){};
  string _Key;
  int _Enum;
};

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

class ActivationFrame_O;
class Environment_O;
typedef gctools::smart_ptr<ActivationFrame_O> ActivationFrame_sp;
};

/*! A type for an array of arguments */
typedef core::T_O **ArgArray;

namespace core {
typedef T_mv (*SpecialFormCallback)(List_sp, T_sp);
typedef void (*MakePackageCallback)(string const &packageName, Lisp_sp);
typedef void (*ExportSymbolCallback)(Symbol_sp symbol, Lisp_sp);

 typedef void (*module_startup_function_type)(gctools::Tagged);
 typedef void (*module_shutdown_function_type)();

/* A few symbols associated with error handling that everything needs */
extern Symbol_sp& _sym_error;
extern Symbol_sp& _sym_setThrowPosition;
extern Symbol_sp& _sym_makeCondition;
extern Symbol_sp& _sym_simpleError;
/*! Search for multiple occurances of a string and replace it
 * \param str The string that is modified
 * \param search The string to search for
 * \param replace The string to replace with
 */
string searchAndReplaceString(const string &str, const string &search, const string &replace, Lisp_sp lisp);

/* The CallingConvention for Common Lisp functions is a pointer to where the multiple value result
   should be written, the closed over environment for the function, the number of args, three explicit args that will pass in registers (or be NULL)
   and a varargs list */
typedef void (*LispCallingConventionPtr)(T_mv *result, int nargs, T_sp arg1, T_sp arg2, T_sp arg3, va_list rest);
}

#include <clasp/core/core_globals.h>

namespace kw {
extern core::Symbol_sp& _sym_formatControl;
extern core::Symbol_sp& _sym_formatArguments;
};

// Can I get rid of this?
#define IS_SYMBOL_DEFINED(x) (x)
#define IS_SYMBOL_UNDEFINED(x) (!x)
#define UNDEFINED_SYMBOL (_Unbound<core::Symbol_O>())


//
//
//

namespace core {

/*! Create the class hierarchy
 */
extern void registerAllClasses();
// extern void throwIfClassesNotInitialized();

/*! Expose c++ to CandoScript
 */
extern void exposeClassesAndDefineNils(Lisp_sp);

/*! Expose c++ to Python
 */
extern void initializePythonScript();

// ---------------------------------------------------
//  Object oriented programming stuff
//  ------

class Cons_O;
typedef gctools::smart_ptr<Cons_O> Cons_sp;

 class General_O;
typedef gctools::smart_ptr<General_O> General_sp;

class Symbol_O;
typedef gctools::smart_ptr<Symbol_O> Symbol_sp;

 class Function_O;
 typedef gctools::smart_ptr<Function_O> Function_sp;
 
class SymbolToEnumConverter_O;
typedef gctools::smart_ptr<SymbolToEnumConverter_O> SymbolToEnumConverter_sp;
}

namespace gctools {
  class Layout_code;
  // Defined in clasp/src/gctools/gc_interface.cc
  extern Layout_code* get_stamp_layout_codes();
};

#if defined(USE_BOEHM)
#define FRIEND_GC_SCANNER(nscl) friend gctools::Layout_code* gctools::get_stamp_layout_codes();
#endif
#if defined(USE_MPS)
#ifdef RUNNING_GC_BUILDER
#define FRIEND_GC_SCANNER(nscl)
#else
//#define FRIEND_GC_SCANNER(theclass) friend GC_RESULT gctools::obj_scan_helper<theclass>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t & _mps_ufs, mps_word_t _mps_wt, mps_addr_t & client);
#define FRIEND_GC_SCANNER(dummy) friend gctools::Layout_code* gctools::get_stamp_layout_codes();
#endif
#endif


namespace core {

#define _NEW_(x) (new x)

  class DebugStream;

/* Callbacks that initialize the Lisp environment have this structure*/

  typedef void (*InitializationCallback)(Lisp_sp);

  [[noreturn]]void errorFormatted(const char *errorString);
  [[noreturn]]void errorFormatted(const string &msg);

//
//  Lisp class access functions for when we only have a forward
//  definition for the Lisp_O class and for any other object
//
  string _rep_(T_sp obj);
/*! Convert underscores to "-" and "STAR" to "*" and "AMP" to "&"
      to convert a C++ name to a lisp symbol */
  string lispify_symbol_name(string const &name);
  string magic_name(const string& name, const string& optional_package="");
  void colon_split(const string& name, string& package_part, string& symbol_part);
 
  Symbol_sp lispify_intern_keyword(string const &name);
//    Symbol_sp lispify_intern2(string const& name, string const& packageName);
// lisp_lispifyAndInternWithPackageNameIfNotGiven
  Symbol_sp lispify_intern(const string &name, const string &packageName = "", bool exportSymbol = true);
//    Symbol_sp lispify_intern_export(string const& name, string const& packageName);
  Symbol_sp lisp_upcase_intern(string const &name, string const &packageName);
  Symbol_sp lisp_upcase_intern_export(string const &name, string const &packageName);
  void* lisp_to_void_ptr(T_sp o);
  T_sp lisp_from_void_ptr(void* p);
 
  List_sp lisp_copy_default_special_bindings();
/*! Write characters to the stream */
#if 0
  gc::GCStack &lisp_threadLocalStack();
#endif

  Lisp_sp lisp_fromObject(T_sp obj);
  string lisp_currentPackageName();
  string lisp_classNameAsString(Instance_sp c);
  void lisp_throwUnexpectedType(T_sp offendingObject, Symbol_sp expectedTypeId);
  core::T_sp lisp_false();
  T_sp lisp_ArgArrayToCons(int nargs, ArgArray args);
  T_sp lisp_va_list_toCons(va_list vargs);
//bool lisp_fixnumP(core::T_sp obj);
//gctools::Fixnum lisp_asFixnum(core::T_sp obj);
/*! Create a SourcePosInfo object for a C++ function */
  SourcePosInfo_sp lisp_createSourcePosInfo(const string &sourceFile, size_t filePos, int lineno);

  T_sp lisp_lookup_reader_patch(T_sp patches, T_sp key, bool &found);
//bool lisp_characterP(core::T_sp obj);
  bool lisp_BuiltInClassesInitialized();
  Instance_sp lisp_built_in_class();
  Instance_sp  lisp_standard_class();
  Instance_sp  lisp_clbind_cxx_class();
  Instance_sp  lisp_derivable_cxx_class();
  void lisp_pushClassSymbolOntoSTARallCxxClassesSTAR(Symbol_sp classSymbol);
  void lisp_defparameter(Symbol_sp sym, T_sp val);
  T_sp lisp_symbolValue(Symbol_sp sym);
  string symbol_symbolName(Symbol_sp);
  string symbol_packageName(Symbol_sp);
  Symbol_sp lisp_symbolNil();
  bool lisp_boundp(Symbol_sp s);
  T_sp lisp_adjust_array(T_sp array, T_sp new_size, T_sp fill_pointer);
  [[noreturn]]void lisp_errorCannotAllocateInstanceWithMissingDefaultConstructor(T_sp theClassSymbol);
  T_sp lisp_boot_findClassBySymbolOrNil(Symbol_sp sym);
  void lisp_addClassSymbol(Symbol_sp classSymbol, gctools::smart_ptr<Creator_O> cb, Symbol_sp baseClassSymbol1); //, Symbol_sp baseClassSymbol2 = UNDEFINED_SYMBOL, Symbol_sp baseClassSymbol3 = UNDEFINED_SYMBOL);
  void lisp_addClass(Symbol_sp classSymbol);
//void lisp_addClassAndInitialize(Symbol_sp classSymbol, gctools::smart_ptr<Creator> cb, Symbol_sp baseClassSymbol1, Symbol_sp baseClassSymbol2 = UNDEFINED_SYMBOL, Symbol_sp baseClassSymbol3 = UNDEFINED_SYMBOL);
  void lisp_throwIfBuiltInClassesNotInitialized();
  Instance_sp lisp_classFromClassSymbol(Symbol_sp classSymbol);
  Instance_sp lisp_instance_class(T_sp obj);
  Instance_sp lisp_static_class(T_sp obj);
  Function_sp lisp_symbolFunction(Symbol_sp sym);
  string lisp_symbolNameAsString(Symbol_sp sym);
  T_sp lisp_createStr(const string &str);
  T_sp lisp_createFixnum(int num);
  T_sp lisp_createList(T_sp a1);
  T_sp lisp_createList(T_sp a1, T_sp a2);
  T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3);
  T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3, T_sp a4);
  T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3, T_sp a4, T_sp a5);
  T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3, T_sp a4, T_sp a5, T_sp a6);
  T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3, T_sp a4, T_sp a5, T_sp a6, T_sp a7);
  T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3, T_sp a4, T_sp a5, T_sp a6, T_sp a7, T_sp a8);

//    void lisp_setGlobalInt(const string& package, const string& n, uint val );
//    Symbol_sp lisp_allocate_packageless_sid(string const& n);
  Symbol_sp lisp_getClassSymbolForClassName(const string &n);
  string lisp_convertCNameToLispName(string const &cname, bool convertUnderscoreToDash = true);
  T_sp lisp_apply(T_sp funcDesig, ActivationFrame_sp args);
  List_sp lisp_parse_arguments(const string &packageName, const string &args);
  List_sp lisp_parse_declares(const string &packageName, const string &declarestring);
  LambdaListHandler_sp lisp_function_lambda_list_handler(List_sp lambda_list, List_sp declares, std::set<int> pureOutValues = std::set<int>());

  void lisp_defineSingleDispatchMethod(T_sp name,
                                       Symbol_sp classSymbol,
                                       BuiltinClosure_sp,
                                       size_t TemplateDispatchOn = 0,
                                       bool useTemplateDispatchOn = false,
                                       const string &lambda_list = "",
                                       const string &declares = "",
                                       const string &docstring = "",
                                       bool autoExport = true,
                                       int number_of_required_arguments = -1,
                                       std::set<int> pureOutIndices = std::set<int>());


  void lisp_defmacro(Symbol_sp name, const string &packageName,
                     BuiltinClosure_sp, const string &arguments = "", const string &declarestring = "",
                     const string &docstring = "");
  void lisp_defun(Symbol_sp name, const string &packageName,
                  BuiltinClosure_sp, const string &arguments = "", const string &declarestring = "",
                  const string &docstring = "", const string &sourceFile = "",
                  int sourceLine = 0, int number_of_required_arguments = 0,
                  const std::set<int> &skipIndices = std::set<int>());
  void lisp_defun_setf(Symbol_sp name, const string &packageName,
                       BuiltinClosure_sp, const string &arguments = "", const string &declarestring = "",
                       const string &docstring = "", const string &sourceFile = "",
                       int sourceLine = 0, int number_of_required_arguments = 0,
                       const std::set<int> &skipIndices = std::set<int>());
  void lisp_defmethod(Symbol_sp gfSymbol, Function_sp, const string &arguments, const string &docstring);

  void lisp_defsetfSingleDispatchMethod(Lisp_sp lisp, const string &name, Symbol_sp classSymbol,
                                        Function_sp, const string &arguments = "", const string &declares = "", const string &docstring = "", bool autoExport = true);

  void lisp_defsetf(const string &name, Symbol_sp classSymbol,
                    Function_sp, const string &arguments = "", const string &docstring = "", bool autoExport = true);

  
  core::T_sp lisp_hiddenBinderLookup(Symbol_sp sym);

//
// You define what is being sent to the debug log using these constants
#define DEBUG_CPP_FUNCTION 0x0001
#define DEBUG_CPP_BLOCK 0x0002
#define DEBUG_SCRIPT 0x0003
#define DEBUG_SHOUT 0x0004
#define DEBUG_TOPLEVEL 0x0005
#define DEBUG_LOG 0x0006
#define DEBUG_EXCEPTION 0x0007
#define DEBUG_LINE_COL 0x0009

#define DEBUG_INTERP_LISP_FUNCTION 0x00000100
#define DEBUG_INTERP_LISP_LET 0x00000200
#define DEBUG_INTERP_LISP_LET_STAR 0x00000300
#define DEBUG_INTERP_LISP_BLOCK 0x00000400
#define DEBUG_INTERP_LISP_TAGBODY 0x00000500
#define DEBUG_INTERP_LISP_LAMBDA 0x00000600
#define DEBUG_INTERP_LISP_FLET 0x00000700
#define DEBUG_INTERP_LISP_LABELS 0x00000800
#define DEBUG_INTERP_LISP_CALL 0x00000900
#define DEBUG_INTERP_LISP_PROGN 0x00000A00
#define DEBUG_INTERP_LISP_CATCH 0x00000B00
#define DEBUG_INTERP_LISP_UNWIND_PROTECT 0x00000C00

#define DEBUG_COMPILED_LISP_FUNCTION 0x00010000
#define DEBUG_COMPILED_LISP_LET 0x00020000
#define DEBUG_COMPILED_LISP_LET_STAR 0x00030000
#define DEBUG_COMPILED_LISP_BLOCK 0x00040000
#define DEBUG_COMPILED_LISP_TAGBODY 0x00050000
#define DEBUG_COMPILED_LISP_LAMBDA 0x00060000
#define DEBUG_COMPILED_LISP_FLET 0x00070000
#define DEBUG_COMPILED_LISP_LABELS 0x00080000
#define DEBUG_COMPILED_LISP_CALL 0x00090000
#define DEBUG_COMPILED_LISP_PROGN 0x000A0000
#define DEBUG_COMPILED_LISP_CATCH 0x000B0000
#define DEBUG_COMPILED_LISP_UNWIND_PROTECT 0x000C0000

  bool lisp_debugIsOn(const char *fileName, uint debugFlag = DEBUG_CPP_FUNCTION);

  DebugStream *lisp_debugLog();
/*! Return a string representation of the object */
  string lisp_rep(T_sp obj);
  Symbol_sp lisp_internKeyword(const string &name);
  Symbol_sp lisp_intern(const string &name);
  Symbol_sp lisp_intern(const string &symbolName, const string &packageName);
  T_sp lisp_VectorObjectsFromMultipleValues(T_mv values);
  string symbol_fullName(Symbol_sp s);
  void lisp_logException(const char *file, const char *fn, int line, const char *structure, T_sp condition);
//    bool lisp_isGlobalInitializationAllowed(Lisp_sp lisp);
//    void lisp_installGlobalInitializationCallback( InitializationCallback initGlobals);
  void lisp_extendSymbolToEnumConverter(SymbolToEnumConverter_sp conv, Symbol_sp const &name, Symbol_sp const &archiveName, int value);
/*! Return the index of the top of the stack */
  size_t lisp_pushCatchThrowException(T_sp throwTag, T_sp value);
  int lisp_lookupEnumForSymbol(Symbol_sp predefSymId, T_sp symbol);
  core::Symbol_sp lisp_lookupSymbolForEnum(Symbol_sp predefSymId, int enumVal);

/*! Register source info for the object in the current source database */
  core::T_sp lisp_registerSourceInfo(T_sp obj, SourceFileInfo_sp sfo, size_t filePos, int lineno, int column);
  core::T_sp lisp_registerSourcePosInfo(T_sp obj, SourcePosInfo_sp spi);




};

#include <clasp/gctools/interrupt.h>
#include <clasp/gctools/threadlocal.h>


namespace core {
  class InvocationHistoryStack;
  class InvocationHistoryFrame;
  InvocationHistoryStack* thread_local_invocation_history_stack();
  InvocationHistoryFrame* thread_local_invocation_history_stack_top_frame();
  void thread_local_invocation_history_stack_push_frame(InvocationHistoryFrame* frame);
  int thread_local_invocation_bindings_size();
};



#include <clasp/core/exceptions.h>


namespace kw {
extern core::Symbol_sp& _sym_function;
extern core::Symbol_sp& _sym_macro;
};


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

typedef void (*PrintvWriteCallback)(const char *outputBuffer);
typedef void (*PrintvWriteCharCallback)(char outputChar);
typedef void (*PrintvFlushCallback)();

const char *trimSourceFilePathName(const char *fullPathName);

#define IN_PLUG_PREFIX '-'
#define OUT_PLUG_PREFIX '+'
};

namespace llvm_interface {

typedef void (*llvmAddSymbolCallbackType)(const core::Symbol_sp &sym);
extern llvmAddSymbolCallbackType addSymbol;
}

#include <clasp/core/clasp_gmpxx.h>

namespace reg {

void lisp_associateClassIdWithClassSymbol(class_id cid, core::Symbol_sp sym);

template <class T>
void lisp_registerClassSymbol(core::Symbol_sp sym) {
  class_id cid = registered_class<T>::id;
  lisp_associateClassIdWithClassSymbol(cid, sym);
}

core::Symbol_sp lisp_classSymbolFromClassId(class_id cid);

template <class T>
core::Symbol_sp lisp_classSymbol() {
  class_id cid = registered_class<T>::id;
  core::Symbol_sp sym = lisp_classSymbolFromClassId(cid); //_lisp->classSymbolsHolder()[cid];
  if (sym.nilp()) {
    string typen = typeid(T).name();
    sym = core::lisp_intern(typen);
    // Fall through - intern the name and return the symbol
  }
  return sym;
}
};
#define KERNEL_NAME "kernel"

#define clasp_disable_interrupts()
#define clasp_enable_interrupts()

#ifdef DMALLOC
#include <dmalloc.h>
#endif


namespace core {
struct cl_env {};
typedef cl_env *cl_env_ptr;
inline cl_env_ptr clasp_process_env() { return NULL; };
inline void clasp_disable_interrupts_env(const cl_env_ptr){};
inline void clasp_enable_interrupts_env(const cl_env_ptr){};
//    inline void clasp_disable_interrupts() {};
//    inline void clasp_enable_interrupts() {};
};

namespace core {
typedef va_list clasp_va_list;
#define clasp_va_start va_start
List_sp clasp_grab_rest_args(va_list args, int nargs);
#define clasp_va_end va_end
};


namespace core {
  extern size_t debug_InvocationHistoryFrame;
};

#endif
#endif //]
