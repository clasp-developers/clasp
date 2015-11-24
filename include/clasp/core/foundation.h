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
#ifndef FOUNDATION_H //[
#define FOUNDATION_H

/*! Turn this on to force turn on xxx_ASSERT messages in release code*/
//#ifndef DEBUG_ASSERTS
//#define DEBUG_ASSERTS
//#endif

// To debug memory usage turn this on and then you can mark
// objects as they are allocated with an integer using gctools::MARKER
#ifdef USE_BOEHM
//#define USE_BOEHM_MEMORY_MARKER
#endif
#ifndef APPLICATION_CONFIG
#define APPLICATION_CONFIG <clasp/main/application.config>
#endif
/*! Configure the application Clasp or Cando currently */
#include APPLICATION_CONFIG

/*! Old way of doing #= and ## used alists which are slow
  Switch to hash-tables to speed things up */
#define USE_SHARP_EQUAL_HASH_TABLES 1

#define ALWAYS_INLINE __attribute__((always_inline))
#define NOINLINE __attribute__((noinline))
#define MAYBE_INLINE __attribute__((noinline))

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

#include <map>

#define CLASP_MAIN_FUNCTION_NAME "CLASP_MAIN"

#define VARARGS

#ifdef USEBOOSTPYTHON
#include <clasp/core/mainBoostPython.h>
#endif
#include <clasp/core/gcInterface.h>
//#include <cstdio>

//! Macro for attribute that causes symbols to be exposed
#define ATTR_WEAK __attribute__((weak))

#if defined(DEBUG_TELEMETRY)
#define DEBUG_MPS_ALLOCATION(poolName, addr, gcobject_addr, size, kind) clasp_mps_debug_allocation(poolName, addr, gcobject_addr, size, kind)
#else
#define DEBUG_MPS_ALLOCATION(poolName, addr, gcobject_addr, size, kind)
//#define DEBUG_MPS_FIX1_BEFORE(base,smartaddr)
#endif

#define clasp_unlikely(x) __builtin_expect(!!(x), 0)
#define clasp_likely(x) __builtin_expect(!!(x), 1)
#define UNLIKELY(x) clasp_unlikely(x)
#define LIKELY(x) clasp_likely(x)

typedef std::size_t class_id;

/*! Configure architecture dependent types */
#include <clasp/core/config.h>

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

/*! Set the following to 1 if you want to use intrusive smart_ptrs
  and 0 if you want to use boost::shared_ptr */

#define USE_INTRUSIVE_SMART_PTR 1

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
#ifdef DEBUG_LEVEL_FULL_REFCOUNT //[
#define DEBUG_ON 1
#define CALLSTACK_ON 1
#define REFCOUNTDEBUG 1
#else                   //][
#ifdef DEBUG_LEVEL_FULL //[
#define DEBUG_ON 1
#define CALLSTACK_ON 1
#define DEBUG_ASSERTS 1
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
#define SYMBOL_EXPORT_SC_(p, x)
#define SYMBOL_SC_(p, x)
#define INTERN_(_p_, _x_) (_p_::_sym_##_x_)

/*! Use this used to bind the C++ function fn_##x that will have the name (x) in Lisp (with "_" converted to "-") */
#define DEFUN(pkg, x) defun(pkg, #x, &fn_##x, ARGS_fn_##x, DECL_fn_##x, DOCS_fn_##x, LOCK_fn_##x, _lisp);

#define Defun(x) core::af_def(CurrentPkg, #x, &af_##x, ARGS_af_##x, DECL_af_##x, DOCS_af_##x, __FILE__, __LINE__);
#define Defun_maker(pkg, x) core::af_def(pkg, "make-" #x, &(x##_O::make), ARGS_##x##_O_make, DECL_##x##_O_make, DOCS_##x##_O_make);

#define ClDefun(x) core::af_def(CorePkg, "COMMON-LISP:" #x, &cl_##x, ARGS_cl_##x, DECL_cl_##x, DOCS_cl_##x, __FILE__, __LINE__);
#define CompDefun(x) core::af_def(CompPkg, #x, &comp_##x, ARGS_comp_##x, DECL_comp_##x, DOCS_comp_##x, __FILE__, __LINE__);
#define ExtDefun(x) core::af_def(ExtPkg, #x, &ext_##x, ARGS_ext_##x, DECL_ext_##x, DOCS_ext_##x, __FILE__, __LINE__);
#define ClosDefun(x) core::af_def(ClosPkg, #x, &clos_##x, ARGS_clos_##x, DECL_clos_##x, DOCS_clos_##x, __FILE__, __LINE__);
#define CoreDefun(x) core::af_def(CorePkg, #x, &core_##x, ARGS_core_##x, DECL_core_##x, DOCS_core_##x, __FILE__, __LINE__);

/*! Use this used to bind the C++ function fn_##x that will have the name (x) in Lisp (with "_" converted to "-") */
//#define DEFUN_EXPORT(pkg,x) defun(pkg, #x, &fn_##x, ARGS_fn_##x, DECL_fn_##x, DOCS_fn_##x, LOCK_fn_##x, _lisp);

/*! Use this used to bind the C++ function fn_##x that will have the name (x) in Lisp (with "_" converted to "-") */
//#define DEFUN_NAME(pkg,x,lispname) defun(pkg, lispname, &fn_##x, ARGS_fn_##x, DECL_fn_##x, DOCS_fn_##x, LOCK_fn_##x,  _lisp);

/*! Use this used to bind the C++ function fn_##x that will have the name (x) in Lisp (with "_" converted to "-") */
//#define DEFUN_NAME_EXPORT(pkg,x,lispname) defun(pkg, lispname, &fn_##x, ARGS_fn_##x, DECL_fn_##x, DOCS_fn_##x, LOCK_fn_##x, _lisp);

#if 0
/*! Define a PAIR of accessor functions, the getter and setter */
#define DEFACCESSORS(x)                                                   \
  af_def(CurrentPkg, #x, &af_##x, ARGS_af_##x, DOCS_af_##x, LOCK_af_##x); \
  _lisp->add_accessor_pair(_sym_##x, _sym_setf_##x);

/*! Define a PAIR of accessor functions, the getter and setter */
#define DEFACCESSORS_EXPORT(x)                                                         \
  af_def(CurrentPkg, #x, &af_##x, ARGS_af_##x, DECL_af_##x, DOCS_af_##x, LOCK_af_##x); \
  _lisp->add_accessor_pair(_sym_##x, _sym_setf_##x);
#endif

/*! Use this in initializeCandoPrimitives to define a function
  This is a little more complicated than it needs to be to try and avoid unused variable warnings */
#define DEFGENERIC(pkg, x) defgeneric(pkg, #x, &gf_##x, ARGS_gf_##x, DOCS_gf_##x, _lisp);

#if 0
/*! Use this in initializeCandoPrimitives to attach methods to the generic function */
#define DEFMETHOD(x, id) defmethod(_sym_##x, md_##x##id, ARGS_md_##x##id, DECL_md_##x##id, DOCS_md_##x##id, _lisp);
#endif

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

#include <cstddef>

typedef unsigned int uint;
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
}

/*! Associate a namespace name with a Package.
  This is scraped out of the code by "registerClasses.py"
*/

#define NAMESPACE_PACKAGE_ASSOCIATION(x, y, z) \
  static const std::string y = z;              \
  namespace x {                                \
  static const std::string CurrentPkg = z;     \
  }

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

//
//
//	For debugging memory
//
//
#ifdef CDEBUG_FULL
#define REFCOUNTDEBUG
#endif

#if 0
#ifdef _DEBUG
#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <cstdio>
#include <crtdbg.h>
#define REFCOUNTDEBUG
#endif
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

#include <string>
#include <vector>
#include <queue>
#include <list>
#include <map>
#include <set>
#include <ostream>
#include <iostream>
#include <boost/format.hpp>

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

#if defined(USE_REFCOUNT)
namespace boost {
template <class T>
void intrusive_ptr_add_ref(T *p);
template <class T>
void intrusive_ptr_release(T *p);
};
#endif

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

namespace core {
class T_O;
typedef T_O FIXNUM;
typedef T_O STACK_FRAME;
class Cons_O;
class Pointer_O;
class Vector_O;
class VectorObjects_O;
class Number_O;
class Integer_O;
class LoadTimeValues_O;
/* AMS pool classes */
class Symbol_O;
class Null_O;
class Stream_O;
class SourcePosInfo_O;
class SourceFileInfo_O;
class WeakKeyHashTable_O;
class WeakKeyMapping_O;
class DynamicScopeManager;

class Functoid;
class FunctionClosure;
class BuiltinClosure;
class InterpretedClosure;
};
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
    : public boost::less_than_comparable<type_id>,
      public gctools::GCIgnoreClass {
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
struct registered_class : gctools::GCIgnoreClass {
  static class_id const id;
};
template <class T>
class_id const registered_class<T>::id = allocate_class_id(typeid(T));
template <class T>
struct registered_class<T const>
    : registered_class<T> {};
};

#define BF boost::format

namespace core {
extern int _global_signalTrap;
extern bool _global_debuggerOnSIGABRT; // If this is false then SIGABRT is processed normally and it will lead to termination of the program. See core_exit!
void lisp_pollSignals();
};
#define SET_SIGNAL(s) \
  { core::_global_signalTrap = s; }
#define POLL_SIGNALS() core::lisp_pollSignals();

void lisp_errorDereferencedNonPointer(core::T_O *objP);
void lisp_errorBadCast(class_id toType, class_id fromType, core::T_O *objP);
void lisp_errorBadCastFromT_O(class_id toType, core::T_O *objP);
void lisp_errorBadCastToFixnum(class_id fromType, core::T_O *objP);
void lisp_errorBadCastFromT_OToCons_O(core::T_O *objP);
void lisp_errorBadCastFromSymbol_O(class_id toType, core::Symbol_O *objP);
void lisp_errorUnexpectedType(class_id expectedTyp, class_id givenTyp, core::T_O *objP);
void lisp_errorUnexpectedNil(class_id expectedTyp);
void lisp_errorDereferencedNil();
void lisp_errorDereferencedUnbound();
void lisp_errorIllegalDereference(void *v);

template <typename To, typename From, typename ObjPtrType>
void __attribute__((noreturn)) lisp_errorCast(ObjPtrType objP) {
  class_id to_typ = reg::registered_class<To>::id;
  class_id from_typ = reg::registered_class<From>::id;
  lisp_errorBadCast(to_typ, from_typ, reinterpret_cast<core::T_O *>(objP));
  __builtin_unreachable();
}

namespace core {
struct ThreadInfo;
class MultipleValues;
void lisp_setThreadLocalInfoPtr(ThreadInfo *address);
MultipleValues &lisp_multipleValues();
MultipleValues &lisp_callArgs();
};

extern void clasp_mps_debug_allocation(const char *poolName, void *base, void *objAddr, int size, int kind);
extern void clasp_mps_debug_fix1_before(void *base, void *smartAddr);
extern void clasp_mps_debug_fix_before(void *pbase, void *px, int offset);
extern void clasp_mps_debug_fix_after(void *pbase, void *px);
extern void clasp_mps_debug_container(const char *ctype, const char *name, int size);
//extern void clasp_mps_debug_scan_object(gctools::GCObject*  obj);

namespace gctools {
struct return_type {
  core::T_O *ret0;
  size_t nvals;
  return_type() : ret0(NULL), nvals(0){};
  return_type(core::T_O *r0, size_t nv) : ret0(r0), nvals(nv){};
};
};

namespace core {
#define LCC_MACROS
#include <clasp/core/lispCallingConvention.h>
#undef LCC_MACROS
};

#include <clasp/gctools/memoryManagement.h>

namespace core {
typedef gctools::smart_ptr<T_O> T_sp;
typedef T_sp SEQUENCE_sp;
typedef T_sp LIST_sp;
typedef gctools::smart_ptr<Cons_O> Cons_sp;
typedef gctools::smart_ptr<VectorObjects_O> VectorObjects_sp;
typedef gctools::smart_ptr<Stream_O> Stream_sp;
typedef gctools::smart_ptr<SourcePosInfo_O> SourcePosInfo_sp;
typedef gctools::smart_ptr<SourceFileInfo_O> SourceFileInfo_sp;
};

#include <clasp/gctools/containers.h>

#include <clasp/core/multipleValues.h>

namespace core {
class Instance_O;
typedef gc::smart_ptr<Instance_O> Instance_sp;

#define LCC_PROTOTYPES
#include <clasp/core/lispCallingConvention.h>
#undef LCC_PROTOTYPES
};

namespace core {
core::T_sp lisp_true();
uint lisp_hash(uintptr_t v);
};

#include <clasp/gctools/gcweak.h>

#include <clasp/gctools/managedStatic.h>

#include <clasp/gctools/gcstring.h>

#include <clasp/gctools/gc_interface.h>

#define DEFINE_O_SMART_POINTERS(zclass)                                    \
  class zclass##_O;                                                        \
  typedef gctools::smart_ptr<zclass##_O> zclass##_sp; /* Stack pointers */ \
  typedef gctools::smart_ptr<zclass##_O> zclass##_wp;                      \
  typedef gctools::multiple_values<zclass##_O> zclass##_mv;

// This ensures that smart pointers are only declared once
// for each compilation
#define SMART(zclass) \
  DEFINE_O_SMART_POINTERS(zclass)

#define FORWARD(zclass) \
  DEFINE_O_SMART_POINTERS(zclass)

namespace core {
typedef gctools::smart_ptr</* TODO: use const */ T_O> T_sp;

class Class_O;
typedef gctools::smart_ptr<Class_O> Class_sp;

class Number_O;
typedef gctools::smart_ptr<Number_O> Number_sp;

class VectorObjects_O;
typedef gctools::smart_ptr<VectorObjects_O> VectorObjects_sp;

class Symbol_O;
typedef gctools::smart_ptr<Symbol_O> Symbol_sp;

class Pointer_O;
typedef gctools::smart_ptr<Pointer_O> Pointer_sp;

void lisp_error_simple(const char *functionName, const char *fileName, int lineNumber, const boost::format &fmt);
};

class _RootDummyClass;

namespace core {
void lisp_throwLispError(const string &str);
void lisp_throwLispError(const boost::format &fmt);
};

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
class Instance_O;
typedef gctools::smart_ptr<Instance_O> Instance_sp;

//    typedef T_mv (*ActivationFrameFunctionPtr)(ActivationFrame_sp);

class Lisp_O;
typedef gctools::tagged_pointer<Lisp_O> Lisp_sp;
class Function_O;
typedef gctools::smart_ptr<Function_O> Function_sp;
class Str_O;
typedef gctools::smart_ptr<Str_O> Str_sp;
class StrWithFillPtr_O;
typedef gctools::smart_ptr<StrWithFillPtr_O> StrWithFillPtr_sp;
#ifdef USE_HEAP_FIXNUM
class Fixnum_O;
typedef gctools::smart_ptr<Fixnum_O> Fixnum_sp;
#endif
class LambdaListHandler_O;
typedef gctools::smart_ptr<LambdaListHandler_O> LambdaListHandler_sp;
class Environment_O;
typedef gctools::smart_ptr<Environment_O> Environment_sp;
class Symbol_O;
typedef gctools::smart_ptr<Symbol_O> Symbol_sp;
typedef void (*ExposeCandoFunction)(Lisp_sp);
typedef void (*ExposePythonFunction)(Lisp_sp);
typedef T_mv (*SpecialFormCallback)(List_sp, T_sp);
typedef void (*MakePackageCallback)(string const &packageName, Lisp_sp);
typedef void (*ExportSymbolCallback)(Symbol_sp symbol, Lisp_sp);

class Package_O;
typedef gctools::smart_ptr<Package_O> Package_sp;

/* A few symbols associated with error handling that everything needs */
extern Symbol_sp _sym_error;
extern Symbol_sp _sym_setThrowPosition;
extern Symbol_sp _sym_makeCondition;
extern Symbol_sp _sym_simpleError;
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
extern core::Symbol_sp _sym_formatControl;
extern core::Symbol_sp _sym_formatArguments;
};

// Can I get rid of this?
#define IS_SYMBOL_DEFINED(x) (x)
#define IS_SYMBOL_UNDEFINED(x) (!x)
#define UNDEFINED_SYMBOL (_Unbound<core::Symbol_O>())
//#define UNDEFINED_SYMBOL _global_undefined_symbol
//extern core::Symbol_sp _global_undefined_symbol;

namespace core {

class Creator {
  struct metadata_always_fix_pointers_to_derived_classes;

public:
  // Some Creators don't actually allocate anything -
  // classes that don't have default allocators
  virtual bool allocates() const { return true; };
  /*! If this is the allocator for a primary CxxAdapter class then return true, */
  virtual int duplicationLevel() const { return 0; };
  virtual size_t templatedSizeof() const = 0;
  virtual gc::tagged_pointer<Creator> duplicateForClassName(core::Symbol_sp className) {
    printf("Subclass must implement Creator::duplicateForClassName\n");
    exit(1);
  };
  virtual void describe() const = 0;
  virtual core::T_sp allocate() = 0;
};
};

//
//
//

//
// These macros are scraped by an external program
// and used to create "initClasses_inc.h" which includes
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
extern void exposeClassesAndDefineNils(Lisp_sp);

/*! Expose c++ to Python
 */
extern void initializePythonScript();

// ---------------------------------------------------
//  Object oriented programming stuff
//  ------

class Cons_O;
typedef gctools::smart_ptr<Cons_O> Cons_sp;

class Class_O;
typedef gctools::smart_ptr<Class_O> Class_sp;

class Function_O;

class Symbol_O;
typedef gctools::smart_ptr<Symbol_O> Symbol_sp;

class SymbolToEnumConverter_O;
typedef gctools::smart_ptr<SymbolToEnumConverter_O> SymbolToEnumConverter_sp;
}

#if defined(USE_BOEHM)
#define FRIEND_GC_SCANNER(nscl)
#endif
#if defined(USE_MPS)
#ifdef RUNNING_GC_BUILDER
#define FRIEND_GC_SCANNER(nscl)
#else
#define FRIEND_GC_SCANNER(theclass) friend GC_RESULT gctools::obj_scan_helper<theclass>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t & _mps_ufs, mps_word_t _mps_wt, mps_addr_t & client);
#endif
#endif

namespace core {

typedef gctools::Fixnum cl_index;

#define _NEW_(x) (new x)

class DebugStream;

/* Callbacks that initialize the Lisp environment have this structure*/

typedef void (*InitializationCallback)(Lisp_sp);

void errorFormatted(boost::format fmt);
void errorFormatted(const char *errorString);
void errorFormatted(const string &msg);

//
//  Lisp class access functions for when we only have a forward
//  definition for the Lisp_O class and for any other object
//
string _rep_(T_sp obj);
/*! Convert underscores to "-" and "STAR" to "*" and "AMP" to "&"
      to convert a C++ name to a lisp symbol */
string lispify_symbol_name(string const &name);
Symbol_sp lispify_intern_keyword(string const &name);
//    Symbol_sp lispify_intern2(string const& name, string const& packageName);
// lisp_lispifyAndInternWithPackageNameIfNotGiven
Symbol_sp lispify_intern(const string &name, const string &packageName, bool exportSymbol = true);
//    Symbol_sp lispify_intern_export(string const& name, string const& packageName);
Symbol_sp lisp_upcase_intern(string const &name, string const &packageName);
Symbol_sp lisp_upcase_intern_export(string const &name, string const &packageName);

/*! Write characters to the stream */
void lisp_write(const boost::format &fmt, T_sp stream);
gc::GCStack &lisp_threadLocalStack();

Lisp_sp lisp_fromObject(T_sp obj);
string lisp_currentPackageName();
string lisp_classNameAsString(Class_sp c);
void lisp_throwUnexpectedType(T_sp offendingObject, Symbol_sp expectedTypeId);
core::T_sp lisp_false();
T_sp lisp_ArgArrayToCons(int nargs, ArgArray args);
T_sp lisp_va_list_toCons(va_list vargs);
bool lisp_fixnumP(core::T_sp obj);
gctools::Fixnum lisp_asFixnum(core::T_sp obj);
/*! Create a SourcePosInfo object for a C++ function */
SourcePosInfo_sp lisp_createSourcePosInfo(const string &sourceFile, size_t filePos, int lineno);

T_sp lisp_lookup_reader_patch(T_sp patches, T_sp key, bool &found);
bool lisp_characterP(core::T_sp obj);
bool lisp_BuiltInClassesInitialized();
void lisp_pushClassSymbolOntoSTARallCxxClassesSTAR(Symbol_sp classSymbol);
void lisp_symbolSetSymbolValue(Symbol_sp sym, T_sp val);
string symbol_symbolName(Symbol_sp);
string symbol_packageName(Symbol_sp);
string symbol_repr(Symbol_sp);
Symbol_sp lisp_symbolNil();
T_sp lisp_boot_findClassBySymbolOrNil(Symbol_sp sym);
void lisp_exposeClass(const string &className, ExposeCandoFunction exposeCandoFunction, ExposePythonFunction exposePythonFunction);
void lisp_addClass(Symbol_sp classSymbol, gctools::tagged_pointer<Creator> cb, Symbol_sp baseClassSymbol1, Symbol_sp baseClassSymbol2 = UNDEFINED_SYMBOL, Symbol_sp baseClassSymbol3 = UNDEFINED_SYMBOL);
void lisp_addClass(Symbol_sp classSymbol);
void lisp_addClassAndInitialize(Symbol_sp classSymbol, gctools::tagged_pointer<Creator> cb, Symbol_sp baseClassSymbol1, Symbol_sp baseClassSymbol2 = UNDEFINED_SYMBOL, Symbol_sp baseClassSymbol3 = UNDEFINED_SYMBOL);
void lisp_throwIfBuiltInClassesNotInitialized();
string lisp_classNameFromClassSymbol(Symbol_sp classSymbol);
Class_sp lisp_classFromClassSymbol(Symbol_sp classSymbol);
Class_sp lisp_instance_class(T_sp obj);
Class_sp lisp_static_class(T_sp obj);
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
void lisp_error_condition(const char *functionName, const char *fileName, int lineNumber, T_sp baseCondition, T_sp initializers);
void lisp_error(T_sp datum, T_sp arguments);

//    void lisp_setGlobalInt(const string& package, const string& n, uint val );
//    Symbol_sp lisp_allocate_packageless_sid(string const& n);
Symbol_sp lisp_getClassSymbolForClassName(const string &n);
string lisp_convertCNameToLispName(string const &cname, bool convertUnderscoreToDash = true);
T_sp lisp_apply(T_sp funcDesig, ActivationFrame_sp args);
List_sp lisp_parse_arguments(const string &packageName, const string &args);
List_sp lisp_parse_declares(const string &packageName, const string &declarestring);
LambdaListHandler_sp lisp_function_lambda_list_handler(List_sp lambda_list, List_sp declares, std::set<int> pureOutValues = std::set<int>());
#if 0
    void lisp_defun_lispify_name(const string& packageName, const string& name,
				 Functoid*, const string& arguments="", const string& declarestring="",
				 const string& docstring="", int locked=1, bool autoExport=true, int number_of_required_arguments=0 );
#endif
void lisp_defmacro(Symbol_sp name, const string &packageName,
                   gc::tagged_pointer<BuiltinClosure>, const string &arguments = "", const string &declarestring = "",
                   const string &docstring = "", bool autoExport = true);
void lisp_defun(Symbol_sp name, const string &packageName,
                gc::tagged_pointer<BuiltinClosure>, const string &arguments = "", const string &declarestring = "",
                const string &docstring = "", const string &sourceFile = "", int sourceLine = 0, bool autoExport = true, int number_of_required_arguments = 0, const std::set<int> &skipIndices = std::set<int>());
void lisp_defgeneric(const string &packageName, const string &name,
                     Functoid *, const string &arguments = "", const string &docstring = "", bool autoExport = true);
void lisp_defmethod(Symbol_sp gfSymbol, Functoid *func, const string &arguments, const string &docstring);

void lisp_defineSingleDispatchMethod(Symbol_sp name,
                                     Symbol_sp classSymbol,
                                     gc::tagged_pointer<BuiltinClosure>,
                                     int TemplateDispatchOn = 0,
                                     const string &lambda_list = "",
                                     const string &declares = "",
                                     const string &docstring = "",
                                     bool autoExport = true,
                                     int number_of_required_arguments = -1,
                                     std::set<int> pureOutIndices = std::set<int>());

void lisp_defsetfSingleDispatchMethod(Lisp_sp lisp, const string &name, Symbol_sp classSymbol,
                                      Functoid *, const string &arguments = "", const string &declares = "", const string &docstring = "", bool autoExport = true);

void lisp_defsetf(Lisp_sp lisp, const string &name, Symbol_sp classSymbol,
                  Functoid *, const string &arguments = "", const string &docstring = "", bool autoExport = true);

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
void lisp_debugLogWrite(const char *fileName, const char *funcName, uint lineNumber, uint column, const boost::format &message, uint debugFlags = DEBUG_CPP_FUNCTION);

DebugStream *lisp_debugLog();
T_sp lisp_ocar(List_sp args);
T_sp lisp_ocadr(List_sp args);
T_sp lisp_ocaddr(List_sp args);
/*! Return a string representation of the object */
string lisp_rep(T_sp obj);
Symbol_sp lisp_internKeyword(const string &name);
Symbol_sp lisp_intern(const string &name);
Symbol_sp lisp_intern(const string &symbolName, const string &packageName);
T_sp lisp_VectorObjectsFromMultipleValues(T_mv values);
/*! Search the sequence SEQ for the object OBJ and return its index and true if found - otherwise false and IDX is undef */
bool lisp_search(T_sp seq, T_sp obj, int &idx);
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
#if 0    
    core::SourcePosInfo_sp lisp_registerSourceInfoFromStream(T_sp obj
                                                             , T_sp stream);

#endif
class Functoid {
  struct metadata_always_fix_pointers_to_derived_classes;
  FRIEND_GC_SCANNER(Functoid);

public:
  virtual const char *describe() const { return "Functoid - subclass must implement describe()"; };
  inline LCC_RETURN operator()(LCC_ARGS_ELLIPSIS) {
    VaList_S lcc_arglist_s;
    va_start(lcc_arglist_s._Args, LCC_VA_START_ARG);
    LCC_SPILL_REGISTER_ARGUMENTS_TO_VA_LIST(lcc_arglist_s);
    core::T_O *lcc_arglist = lcc_arglist_s.asTaggedPtr();
    return this->invoke_va_list(LCC_PASS_ARGS);
  }

  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION() {
    printf("Subclass of Functoid must implement 'activate'\n");
    exit(1);
  };
  virtual size_t templatedSizeof() const { return sizeof(*this); };

public:
  T_sp name;

public:
  Functoid(T_sp n);
  string nameAsString();
  virtual ~Functoid(){};
};

class Closure : public Functoid {
public:
  T_sp closedEnvironment;

public:
  Closure(T_sp name, T_sp env) : Functoid(name), closedEnvironment(env){};
  virtual ~Closure(){};

public:
  virtual void setAssociatedFunctions(core::List_sp assocFuncs){};
  virtual const char *describe() const { return "Closure"; };
  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION() {
    printf("Subclass of Closure must implement 'activate'\n");
    exit(1);
  };

  virtual void *functionAddress() const { return NULL; };
  virtual T_sp sourcePosInfo() const { return _Nil<T_O>(); };
  virtual bool macroP() const = 0;
  virtual void setKind(Symbol_sp k) = 0;
  virtual Symbol_sp getKind() const = 0;
  virtual bool compiledP() const { return false; };
  virtual bool interpretedP() const { return false; };
  virtual bool builtinP() const { return false; };
  virtual int sourceFileInfoHandle() const;
  virtual size_t filePos() const { return 0; }
  virtual int lineNumber() const { return 0; }
  virtual int column() const { return 0; };
  virtual LambdaListHandler_sp lambdaListHandler() const = 0;
  virtual T_sp lambdaList() const = 0;
  virtual T_sp docstring() const;
  virtual List_sp declares() const;
  virtual T_sp cleavir_ast() const;
  virtual void setf_cleavir_ast(T_sp ast);
};
};
#include <clasp/core/exceptions.h>

#define DISABLE_NEW()                                                                        \
  void *operator new(size_t s) { DEPRECIATEDP("Disabled new"); };                            \
  void *operator new(size_t s, const std::nothrow_t &tag) { DEPRECIATEDP("Disabled new"); }; \
  void *operator new(size_t s, void *ptr) { return ptr; };

namespace kw {
extern core::Symbol_sp _sym_function;
extern core::Symbol_sp _sym_macro;
};

namespace core {
class FunctionClosure : public Closure {
public:
  //  T_sp _SourcePosInfo;
  Symbol_sp kind;
  T_sp _cleavir_ast;
  Fixnum _sourceFileInfoHandle;
  Fixnum _filePos;
  Fixnum _lineno;
  Fixnum _column;

public:
  DISABLE_NEW();
#define SOURCE_INFO core::Fixnum sourceFileInfoHandle, core::Fixnum filePos, core::Fixnum lineno, core::Fixnum column
#define SOURCE_INFO_PASS sourceFileInfoHandle, filePos, lineno, column
  FunctionClosure(T_sp name, Symbol_sp k, T_sp env, SOURCE_INFO)
      : Closure(name, env), kind(k), _cleavir_ast(_Nil<T_O>()), _sourceFileInfoHandle(sourceFileInfoHandle), _filePos(filePos), _lineno(lineno), _column(column){};
  FunctionClosure(T_sp name)
      : Closure(name, _Nil<T_O>()), kind(kw::_sym_function), _cleavir_ast(_Nil<T_O>()), _sourceFileInfoHandle(0), _filePos(0), _lineno(0), _column(0){};

  virtual size_t templatedSizeof() const { return sizeof(*this); };

  virtual const char *describe() const { return "SingleDispatchGenericFunctoid"; };
  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION() { SIMPLE_ERROR(BF("Subclass must implement")); };
  void setKind(Symbol_sp k) { this->kind = k; };
  Symbol_sp getKind() const { return this->kind; };
  bool macroP() const;
  T_sp sourcePosInfo() const; // { return this->_SourcePosInfo; };
  T_sp setSourcePosInfo(T_sp sourceFile, size_t filePos, int lineno, int column);
  virtual int sourceFileInfoHandle() const;
  virtual size_t filePos() const;
  virtual int lineNumber() const;
  virtual int column() const;
  virtual T_sp cleavir_ast() const { return this->_cleavir_ast; };
  virtual void setf_cleavir_ast(T_sp ast) { this->_cleavir_ast = ast; };
};

class BuiltinClosure : public FunctionClosure {
public:
  LambdaListHandler_sp _lambdaListHandler;

public:
  DISABLE_NEW();
  BuiltinClosure(T_sp name, Symbol_sp k, SOURCE_INFO)
      : FunctionClosure(name, k, _Nil<T_O>(), SOURCE_INFO_PASS){};
  BuiltinClosure(T_sp name)
      : FunctionClosure(name) {}
  void finishSetup(LambdaListHandler_sp llh, Symbol_sp k) {
    this->_lambdaListHandler = llh;
    this->kind = k;
  }
  virtual T_sp lambdaList() const;
  virtual size_t templatedSizeof() const { return sizeof(*this); };
  virtual const char *describe() const { return "BuiltinClosure"; };
  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION();
  bool builtinP() const { return true; };
  LambdaListHandler_sp lambdaListHandler() const { return this->_lambdaListHandler; };
};

/*! Shouldn't this derive from a Functoid - it doesn't need a closedEnvironment */
class InstanceClosure : public FunctionClosure {
public:
  GenericFunctionPtr entryPoint;
  Instance_sp instance;

public:
  DISABLE_NEW();
  InstanceClosure(T_sp name, GenericFunctionPtr ep, Instance_sp inst)
      : FunctionClosure(name), entryPoint(ep), instance(inst){};
  virtual size_t templatedSizeof() const { return sizeof(*this); };
  virtual const char *describe() const { return "InstanceClosure"; };
  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION();
  LambdaListHandler_sp lambdaListHandler() const { return _Nil<LambdaListHandler_O>(); };
  T_sp lambdaList() const;
};
}

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

#ifdef USEBOOSTPYTHON
#include <cctype>
#include <clasp/core/mainBoostPython.h>
#endif

#define TRY_BOOST_FORMAT_STRING(__fmt, __fmt_str)                                                   \
  string __fmt_str;                                                                                 \
  try {                                                                                             \
    __fmt_str = __fmt.str();                                                                        \
  } catch (const std::exception &exc) {                                                             \
    lisp_throwLispError(BF("Error - could not get string from boost::format --> %s") % exc.what()); \
  }

#include <clasp/core/clasp_gmpxx.h>

namespace reg {
#if 0
    struct ClassSymbolsHolder {
        gctools::Vec0<core::Symbol_sp>  _Symbols;
    };

    extern ClassSymbolsHolder   globalClassSymbolsVectorHolder;
#endif

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

#define EXECUTABLE_NAME "clasp"
#define KERNEL_NAME "kernel"

#include <boost/filesystem.hpp>

namespace boost_filesystem = boost::filesystem;

namespace core {
void initialize_foundation();
}

#define clasp_disable_interrupts()
#define clasp_enable_interrupts()

#define unlikely_if(x) if (UNLIKELY(x))

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
/*! Allocate an atomic buffer with malloc */
char *clasp_alloc_atomic(size_t buffer_size);
/*! The buffer above must be deallocated using this call*/
void clasp_dealloc(char *buffer);
};

namespace core {
typedef va_list clasp_va_list;
#define clasp_va_start va_start
List_sp clasp_grab_rest_args(va_list args, int nargs);
#define clasp_va_end va_end
};

#endif //]
