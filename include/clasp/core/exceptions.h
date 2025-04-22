#pragma once
/*
    File: exceptions.h
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
//
// (C) 2004 Christian E. Schafmeister
//

#include <string>
#include <exception>
#include <sstream>
#include <iostream>
#include <fstream>
#include <string.h>
#include <set>

#include <clasp/core/object.fwd.h>
#include <clasp/core/symbol.fwd.h>
#include <clasp/core/evaluator.fwd.h>
#include <clasp/core/lispStream.fwd.h>
#include <clasp/core/primitives.fwd.h>

namespace cl {
extern core::Symbol_sp& _sym_cellError;
};
namespace kw {
extern core::Symbol_sp& _sym_name;
};
namespace core {
[[noreturn]] extern void lisp_error(gctools::smart_ptr<T_O> baseCondition, gctools::smart_ptr<T_O> initializers);
};

/* ------------------------------------------------------------
 * Macros for errors and diagnostics
 */
/*! Indicate the function does not return */
#define NO_RETURN

#define INTERNAL_ERROR(_msg_) THROW_HARD_ERROR(_msg_)
#define TESTING() printf("%s:%d:%s Testing\n", __FILE__, __LINE__, __FUNCTION__);
#define TESTINGF(fmt) printf("%s:%d:%s Testing: %s\n", __FILE__, __LINE__, __FUNCTION__, (fmt).str().c_str());
#define NO_INITIALIZERS_ERROR(_type_)                                                                                              \
  {                                                                                                                                \
    lisp_error(_type_, nil<core::Cons_O>());                                                                                       \
    THROW_NEVER_REACH();                                                                                                           \
  }
#define FUNCTION_DESCRIPTION_ERROR() SIMPLE_ERROR("Do something about function-description");
#define SIMPLE_WARN(...) core::eval::funcall(cl::_sym_warn, core::SimpleBaseString_O::make(fmt::format(__VA_ARGS__)))
#define ERROR(_type_, _initializers_) lisp_error(_type_, _initializers_)
#define SIMPLE_ERROR(...) ::core::lisp_error_simple(__FUNCTION__, __FILE__, __LINE__, fmt::format(__VA_ARGS__))
#define CLASP_ERROR(string, ...) ::core::lisp_error(::core::_sym_simpleProgramError, ::core::Cons_O::createList(kw::_sym_format_control, ::core::SimpleBaseString_O::make(string), kw::_sym_format_arguments, ::core::Cons_O::createList(__VA_ARGS__)))
/*! Error for when an index is out of range - eg: beyond the end of a string */
#define TYPE_ERROR_INDEX(_seq_, _idx_)                                                                                             \
  ERROR(cl::_sym_simpleTypeError,                                                                                                  \
        core::lisp_createList(kw::_sym_format_control, core::lisp_createStr("~S is not a valid index into the object ~S"),         \
                              kw::_sym_format_arguments, core::lisp_createList(make_fixnum(_idx_), _seq_), kw::_sym_expected_type, \
                              core::lisp_createList(cl::_sym_integer, make_fixnum(0), make_fixnum((_seq_)->length() - 1)),         \
                              kw::_sym_datum, make_fixnum(_idx_)));

#define TYPE_ERROR_INDEX_VARIABLE(message, seqobject, idxobject, len)                                                              \
  ERROR(cl::_sym_simpleTypeError,                                                                                                  \
        core::lisp_createList(kw::_sym_format_control, core::lisp_createStr(message), kw::_sym_format_arguments,                   \
                              core::lisp_createList(idxobject, seqobject), kw::_sym_expected_type,                                 \
                              core::lisp_createList(cl::_sym_integer, make_fixnum(0), make_fixnum(len - 1)), kw::_sym_datum,       \
                              idxobject));

#define TYPE_ERROR_PROPER_LIST(_lst_)                                                                                              \
  ERROR(cl::_sym_simpleTypeError,                                                                                                  \
        core::lisp_createList(kw::_sym_format_control, core::lisp_createStr("~S is not a proper list"), kw::_sym_format_arguments, \
                              core::lisp_createList(_lst_), kw::_sym_expected_type, cl::_sym_cons, kw::_sym_datum, _lst_));

// this might be a copy-paste error _obj_ is declared, but _lst_ is used
// the text also does not work fit the error name
// can't find a use in all clasp
#define TYPE_ERROR_NO_FILL_POINTER(_obj_)                                                                                          \
  ERROR(cl::_sym_simpleTypeError,                                                                                                  \
        core::lisp_createList(kw::_sym_format_control, core::lisp_createStr("~S is not a proper list"), kw::_sym_format_arguments, \
                              core::lisp_createList(_lst_), kw::_sym_expected_type, cl::_sym_cons, kw::_sym_datum, _lst_));

#define TYPE_ERROR_LIST(_lst_)                                                                                                     \
  ERROR(cl::_sym_simpleTypeError,                                                                                                  \
        core::lisp_createList(kw::_sym_format_control, core::lisp_createStr("~S is not a list"), kw::_sym_format_arguments,        \
                              core::lisp_createList(_lst_), kw::_sym_expected_type, cl::_sym_cons, kw::_sym_datum, _lst_));

#define TYPE_ERROR(_datum_, _expectedType_)                                                                                        \
  ERROR(::cl::_sym_type_error, core::lisp_createList(kw::_sym_datum, _datum_, kw::_sym_expected_type, _expectedType_))
#define PROGRAM_ERROR() ERROR(cl::_sym_programError, (nil<T_O>()))
#define SIMPLE_PROGRAM_ERROR(message, ...)                                                                                         \
  ERROR(core::_sym_simpleProgramError, core::lisp_createList(kw::_sym_format_control, core::lisp_createStr(message),               \
                                                             kw::_sym_format_arguments, core::lisp_createList(__VA_ARGS__)))

#define FILE_ERROR(_file_) ERROR(cl::_sym_fileError, core::lisp_createList(kw::_sym_pathname, _file_))
#define CANNOT_OPEN_FILE_ERROR(_file_) FILE_ERROR(_file_)
#define UNRECOGNIZED_KEYWORD_ARGUMENTS_ERROR(obj) ERROR(core::_sym_unrecognizedKeywordArgumentsError, obj)
// the following class does not exist in conditions.lisp and is not used
// #define INVALID_KEYWORD_ARGUMENT_ERROR(obj) ERROR(core::_sym_invalidKeywordArgumentError, obj)
#define STREAM_ERROR(st) ERROR(cl::_sym_streamError, core::lisp_createList(kw::_sym_stream, st))
// core::_sym_simplePackageError with message and datum
#define SIMPLE_PACKAGE_ERROR(message, datum)                                                                                       \
  ERROR(core::_sym_simplePackageError,                                                                                             \
        core::lisp_createList(kw::_sym_format_control, core::lisp_createStr(message), kw::_sym_package,                            \
                              core::lisp_createStr(datum), kw::_sym_format_arguments,                                              \
                              core::lisp_createList(core::lisp_createStr(datum))))
#define SIMPLE_PACKAGE_ERROR_2_args(message, datum1, datum2)                                                                       \
  ERROR(core::_sym_simplePackageError,                                                                                             \
        core::lisp_createList(kw::_sym_format_control, core::lisp_createStr(message), kw::_sym_format_arguments,                   \
                              core::lisp_createList(core::lisp_createStr(datum1), core::lisp_createStr(datum2))))
#define CORRECTABLE_PACKAGE_ERROR(cmessage, message, pack)                                                                         \
  eval::funcall(core::_sym_universalErrorHandler, core::lisp_createStr(cmessage), core::_sym_simplePackageError,                   \
                core::lisp_createList(kw::_sym_format_control, core::lisp_createStr(message), kw::_sym_package,                    \
                                      core::lisp_createStr(pack), kw::_sym_format_arguments,                                       \
                                      core::lisp_createList(core::lisp_createStr(pack))));
#define CORRECTABLE_PACKAGE_ERROR2(cmessage, message, pack, arg)                                                                   \
  eval::funcall(core::_sym_universalErrorHandler, core::lisp_createStr(cmessage), core::_sym_simplePackageError,                   \
                core::lisp_createList(kw::_sym_format_control, core::lisp_createStr(message), kw::_sym_package,                    \
                                      core::lisp_createStr(pack), kw::_sym_format_arguments,                                       \
                                      core::lisp_createList(core::lisp_createStr(pack), core::lisp_createStr(arg))));

#define PACKAGE_ERROR(p) ERROR(cl::_sym_package_error, core::lisp_createList(kw::_sym_package, p))
#define ERROR_END_OF_FILE(st) ERROR(cl::_sym_endOfFile, core::lisp_createList(kw::_sym_stream, st))
#define CLOSED_STREAM_ERROR(st) ERROR(core::_sym_closedStream, core::lisp_createList(kw::_sym_stream, st))

#define READER_ERROR(_fmt_, _fmtArgs_, _stream_) core__reader_error_internal(__FILE__, __LINE__, _fmt_, _fmtArgs_, _stream_)
#define PARSE_ERROR(_fmt_, _fmtArgs_) core__reader_error_internal(__FILE__, __LINE__, _fmt_, _fmtArgs_, nil<Stream_O>())

#define PRINT_NOT_READABLE_ERROR(obj) ERROR(cl::_sym_printNotReadable, core::lisp_createList(kw::_sym_object, obj));
#define CELL_ERROR(name) ERROR(cl::_sym_cellError, core::lisp_createList(kw::_sym_name, name))
#define UNBOUND_VARIABLE_ERROR(name) ERROR(::cl::_sym_unboundVariable, core::lisp_createList(kw::_sym_name, name))
#define KEY_NOT_FOUND_ERROR(_key_) SIMPLE_ERROR("Key {} not found", _key_)
#define CONTROL_ERROR() NO_INITIALIZERS_ERROR(cl::_sym_controlError);

#define WRONG_TYPE_ARG(_datum_, _expectedType_)                                                                                    \
  core__wrong_type_argument(__FILE__, __LINE__, core::lisp_intern(__FUNCTION__, CurrentPkg), _datum_, _expectedType_)

#define FUNCTION_WRONG_TYPE_ARG(function_name, _datum_, _expectedType_)                                                            \
  core__function_wrong_type_argument(function_name, _datum_, _expectedType_)

#define ERROR_WRONG_TYPE_KEY_ARG(_fn_, _key_, _value_, _type_) af_wrongTypeKeyArg(__FILE__, __LINE__, _fn_, _key_, _value_, _type_)

#define ERROR_WRONG_TYPE_ONLY_ARG(_fn_, _datum_, _expectedType_)                                                                   \
  af_wrongTypeOnlyArg(__FILE__, __LINE__, _fn_, _datum_, _expectedType_)

#define ERROR_WRONG_TYPE_NTH_ARG(_fn_, _nth_, _datum_, _expectedType_)                                                             \
  core__wrong_type_nth_arg(__FILE__, __LINE__, _fn_, _nth_, _datum_, _expectedType_)

#define ARITHMETIC_ERROR(_operation_, _operands_)                                                                                  \
  ERROR(cl::_sym_arithmeticError, core::lisp_createList(kw::_sym_operation, _operation_, kw::_sym_operands, _operands_))

#define ERROR_DIVISION_BY_ZERO(_x_, _y_)                                                                                           \
  ERROR(cl::_sym_divisionByZero,                                                                                                   \
        core::lisp_createList(kw::_sym_operation, cl::_sym__DIVIDE_, kw::_sym_operands, core::lisp_createList(_x_, _y_)))

#define DIVISION_BY_ZERO(_operation_, _operands_)                                                                                  \
  ERROR(cl::_sym_divisionByZero, core::lisp_createList(kw::_sym_operation, _operation_, kw::_sym_operands, _operands_))

#define FLOATING_POINT_OVERFLOW(_operation_, _operands_)                                                                                  \
  ERROR(cl::_sym_floatingPointOverflow, core::lisp_createList(kw::_sym_operation, _operation_, kw::_sym_operands, _operands_))

#define FLOATING_POINT_UNDERFLOW(_operation_, _operands_)                                                                                  \
  ERROR(cl::_sym_floatingPointUnderflow, core::lisp_createList(kw::_sym_operation, _operation_, kw::_sym_operands, _operands_))

#define FLOATING_POINT_INEXACT(_operation_, _operands_)                                                                                  \
  ERROR(cl::_sym_floatingPointInexact, core::lisp_createList(kw::_sym_operation, _operation_, kw::_sym_operands, _operands_))

#define FLOATING_POINT_INVALID_OPERATION(_operation_, _operands_)                                                                                  \
  ERROR(cl::_sym_floatingPointInvalidOperation, core::lisp_createList(kw::_sym_operation, _operation_, kw::_sym_operands, _operands_))

#define ERROR_UNDEFINED_FUNCTION(_f_) ERROR(::cl::_sym_undefinedFunction, core::lisp_createList(kw::_sym_name, _f_));
#define FE_ERROR(_type_, _args_)

namespace core {

class DebugStream;

#pragma GCC visibility push(default)
class ATTR_WEAK CatchThrow : public std::exception {
  virtual void keyFunctionForVtable() ATTR_WEAK;

private:
  T_sp _Tag;

public:
  CatchThrow(T_sp tag) : _Tag(tag){};
  T_sp getTag() { return this->_Tag; };
  const char* what() const noexcept override { return "Lisp Throw exception"; }
};

class ATTR_WEAK Unwind : public std::exception {
  virtual void keyFunctionForVtable() ATTR_WEAK;

private:
  void* _Frame; // NOT GC'd use FIXNUM tagged_ptr
  size_t _Index;

public:
  ATTR_WEAK Unwind(void* frame, size_t index)
    : _Frame(frame), _Index(index){};
  void* getFrame() const { return this->_Frame; };
  size_t index() const { return this->_Index; };
  const char* what() const noexcept override { return "Lisp Unwind exception"; }
};

#pragma GCC visibility pop

[[noreturn]] void throwTooFewArgumentsError(core::T_sp closure, size_t given, size_t required);

[[noreturn]] void throwTooManyArgumentsError(core::T_sp closure, size_t given, size_t required);

[[noreturn]] void throwOddKeywordsError(core::T_sp closure);
[[noreturn]] void throwUnrecognizedKeywordArgumentError(core::T_sp closure, core::T_sp kw);

[[noreturn]] void wrongNumberOfArguments(core::T_sp closure, size_t givenNumberOfArguments, size_t requiredNumberOfArguments);

/*! Set a break-point in _trapThrow to catch
 * every exception except those thrown by THROW_noTrap
 */
extern void _trapThrow(const string& fileName, uint line, const string& msg);

/*! If DEBUG_SOURCE macro is defined then this class uses RAII to log entry/exit of C++ functions
          An instance of this struct is declared by the _G() macro */
struct CxxFunctionInvocationLogger {
  CxxFunctionInvocationLogger(const char* cPSourceFile, const char* cPFunctionName, uint lineNumber);
  virtual ~CxxFunctionInvocationLogger();
};

#define lisp_THROW_base(l, x, m)                                                                                                   \
  {                                                                                                                                \
    core::eval::funcall(lisp_symbolFunction(core::_sym_setThrowPosition), x, core::lisp_createStr(__FILE__),                       \
                        core::lisp_createStr(__FUNCTION__), core::lisp_createFixnum(__LINE__));                                    \
    core::lisp_logException(__FILE__, __FUNCTION__, __LINE__, m, x);                                                               \
    core::eval::funcall(core::_sym_error, x);                                                                                      \
    throw_hard_error("Never reach here in lisp_THROW_base");                                                                       \
  }

#define UNREACHABLE() throw_hard_error("Should never reach here")

// Make it easier to find "try" statements
#define TRY() try

#define IMPLEMENT_ME() SIMPLE_ERROR("Implement function {}:{} {}", __FILE__, __LINE__, __FUNCTION__)
#define IMPLEMENT_MEF(msg) SIMPLE_ERROR("Implement function {}:{} {} {}", __FILE__, __LINE__, __FUNCTION__, msg)

#define WARN_IMPLEMENT_ME() fmt::print("{}:{}:{} Implement function\n", __FILE__, __LINE__, __FUNCTION__);
#define WARN_IMPLEMENT_MEF(msg) fmt::print("Implement function {}:{} {} {}", __FILE__, __LINE__, __FUNCTION__, msg.str());

#define NOT_SUPPORTED()                                                                                                            \
  SIMPLE_ERROR("Subclass({}) does not support the function({}) file({}) lineNumber({})", this->className(), __FUNCTION__,          \
               __FILE__, __LINE__);

#define NOT_APPLICABLE() throw_hard_error_not_applicable_method(__FUNCTION__);
#define N_A_() NOT_APPLICABLE()
#define INCOMPLETE(bf)                                                                                                             \
  SIMPLE_ERROR("Finish implementing me!!!! function({}) file({}) lineNumber({}): {}", __FUNCTION__, __FILE__, __LINE__, (bf).str())
#define FIX_ME() SIMPLE_ERROR("Fix me!!! function({}) file({}) lineNumber({})", __FUNCTION__, __FILE__, __LINE__);
#define DEPRECATED() SIMPLE_ERROR("Depreciated!!! function({}) file({}) lineNumber(%d)", __FUNCTION__, __FILE__, __LINE__);
#define STUB() fmt::print("{}:{}>{}  stub\n", __FILE__, __LINE__, __FUNCTION__)

#define MAY_BE_DEPRECATED()                                                                                                        \
  fmt::print("May be depreciated!!! function({}) file({}) lineNumber({})", __FUNCTION__, __FILE__, __LINE__);
#define DEPRECATEDP(s) SIMPLE_ERROR("Depreciated!!! function({}) file({}) lineNumber(%d) %s", __FUNCTION__, __FILE_, __LINE__, (s));

FORWARD(Cons);

#ifdef DEBUG_ASSERT
#define HARD_ASSERT(t)                                                                                                             \
  if (!(t)) {                                                                                                                      \
    core::errorFormatted("HARD_ASSERT failed");                                                                                    \
    abort();                                                                                                                       \
  };
#define HARD_ASSERTF(t, ...)                                                                                                       \
  if (!(t)) {                                                                                                                      \
    core::errorFormatted(fmt::format(__VA_ARGS__));                                                                                \
    abort();                                                                                                                       \
  };
#else
#define HARD_ASSERT(t)                                                                                                             \
  {}
#define HARD_ASSERTF(t, ...)                                                                                                       \
  {}
#endif

#define THROW_NEVER_REACH() throw(HardError(__FILE__, __FUNCTION__, __LINE__, "Should never get here"));

#define MAXSOURCEFILENAME 1024
class DebugStream {
private:
  bool _Enabled;

public:
  bool isEnabled() { return this->_Enabled; };

private:
  string _LogFileName;
  std::ofstream _OutStream;
  bool _OutStreamOpen;
  bool _SuppressMessages;
  std::set<string> _DebugFileNames;
  bool _DebugAll;
  bool _DebugLisp;
  bool _DebugScript;

private:
  int DebugCallDepth;
  const char* _DebugTraceSourceFileName;
  int DebugTraceLineNumber;
  bool DebugLogAsXml;
  bool DebugLogAddBrackets;
  int DebugPositionCounter;
  int DebugLogProcessRank;

public:
  bool getDebugLogAddBrackets() { return this->DebugLogAddBrackets; };
  bool getDebugLogAsXml() { return this->DebugLogAsXml; };
  int getDebugCallDepth() { return this->DebugCallDepth; };
  void open();
  void close();
  void finalClose();
  bool isOn(const char* fileName, uint debugFlags);
  DebugStream& beginNode(uint debugFlags);
  DebugStream& beginNode(uint debugFlags, const char* sourceFile, const char* functionName, uint lineNumber, uint col,
                         const string& message);
  DebugStream& endNode(uint debugFlags);
  DebugStream& writeRaw(const string& data);
  DebugStream& writeText(const string& data);
  DebugStream& writeTextCr(const string& data);
  DebugStream& writeInt(uint i);
  DebugStream& writePtr(void*);
  DebugStream& writeLn();
  DebugStream& log(const string& msg);

  string nextPosition();

  bool debugAll() { return this->_DebugAll; };
  bool debugLisp() { return this->_DebugLisp; };
  bool debugScript() { return this->_DebugScript; };
  void addDebugFileName(const string& fn) { this->_DebugFileNames.insert(fn); };
  bool recognizesDebugFileName(const string& fn);
  string allDebugFileNames();

  void setSuppressMessages(bool s);
  bool getSuppressMessages() { return this->_SuppressMessages; };

  //	void	setTraceFileLine(const char* f, uint l);
  //	void	setDebugPrefix(const char* s);
  //	const char*	getTraceFile();
  //	uint	getTraceLine();
  const char* getDebugPrefix();

  void lazyInitialize();

  //	_LispCallStackHolder& lispCallStack();

  DebugStream(int rank);
  virtual ~DebugStream();
};

extern void debugLogProcessRank(int rank);
extern void debugBreakPoint();

void debugSuppressMessages(bool s);
/*! Write to the debug log file without any translation of special
 *   XML characters
 */
// void	debug=RawWrite( const char* fmt, ... );
/*! Write to the debug log file and translate XML characters
 */
// void	debug=Printf( const char* fmt, ... );

// char*	internal=BufferPrintf( const char* fmt, ... );

extern bool stackmap_log;

#ifdef DEBUG_STACKMAPS
#define STACKMAP_LOG(msg) printf msg
#else
#define STACKMAP_LOG(msg)
#endif

#ifdef DEBUG_ON
// #error "TURN OFF DEBUG_ON"

#define HARD_BREAK_POINT() __asm int 3;
#define lisp_LOG(___fmt)                                                                                                           \
  {                                                                                                                                \
    if (core::lisp_debugIsOn(__FILE__)) {                                                                                          \
      core::lisp_debugLogWrite(__FILE__, __FUNCTION__, __LINE__, 0, ___fmt);                                                       \
    }                                                                                                                              \
  }
#define LOG(...) lisp_LOG(fmt::format(__VA_ARGS__))

#define lisp_SHOUT(___fmt)                                                                                                         \
  {                                                                                                                                \
    if (core::lisp_debugIsOn(__FILE__, DEBUG_SHOUT)) {                                                                             \
      core::lisp_debugLogWrite(__FILE__, __FUNCTION__, __LINE__, 0, ___fmt);                                                       \
    }                                                                                                                              \
  }
#define SHOUT(...) lisp_SHOUT(fmt::format(__VA_ARGS__))

#else // DEBUG_ON
#define HARD_BREAK_POINT()                                                                                                         \
  {}
#define LOG(...)                                                                                                                   \
  {}
#define lisp_LOG(___fmt)                                                                                                           \
  {}
#define SHOUT(...)                                                                                                                 \
  {}
#define lisp_SHOUT(___fmt)                                                                                                         \
  {}

#endif // DEBUG_ON

void assert_failure(const char* file, size_t line, const char* func, const char* msg);
void assert_failure_bounds_error_lt(const char* file, size_t line, const char* func, int64_t x, int64_t y);

#ifdef DEBUG_ASSERT
#define lisp_ASSERT(x)                                                                                                             \
  if (!(x))                                                                                                                        \
  ::core::assert_failure(__FILE__, __LINE__, __FUNCTION__, #x)
#define ASSERT(x) lisp_ASSERT(x)
#define ASSERT_DO(x)                                                                                                               \
  do {                                                                                                                             \
    x;                                                                                                                             \
  } while (0)
#endif
#ifdef DEBUG_BOUNDS_ASSERT
#define lisp_BOUNDS_ASSERT(x)                                                                                                      \
  if (!(x))                                                                                                                        \
  ::core::assert_failure(__FILE__, __LINE__, __FUNCTION__, #x)
#define BOUNDS_ASSERT(x) lisp_BOUNDS_ASSERT(x)
#define BOUNDS_ASSERT_LT(x, y)                                                                                                     \
  {                                                                                                                                \
    if (!((x) < (y)))                                                                                                              \
      ::core::assert_failure_bounds_error_lt(__FILE__, __LINE__, __FUNCTION__, x, y);                                              \
  }
#else
#define BOUNDS_ASSERT(x)
#define BOUNDS_ASSERT_LT(x, y)
#endif
#ifdef DEBUG_ASSERT
#define ASSERT_DO(x)                                                                                                               \
  do {                                                                                                                             \
  } while (0)
#define lisp_ASSERTP(x, e)                                                                                                         \
  if (!(x))                                                                                                                        \
    ::core::assert_failure(__FILE__, __LINE__, __FUNCTION__, (e));
#define ASSERTP(x, e) lisp_ASSERTP(x, e)
#define lisp_ASSERTF(x, e)                                                                                                         \
  if (!(x))                                                                                                                        \
    ::core::assert_failure(__FILE__, __LINE__, __FUNCTION__, (e).c_str());
#define ASSERTF(x, ...) lisp_ASSERTF(x, fmt::format(__VA_ARGS__))
#define ASSERT_eq(x, y)                                                                                                            \
  if (!(x == y)) {                                                                                                                 \
    SIMPLE_ERROR("Assertion [{}=={}] failed values are ({}) and ({})", #x, #y, (x), (y));                                          \
  }
#define ASSERT_lessThan(x, y)                                                                                                      \
  if (!(x < y)) {                                                                                                                  \
    SIMPLE_ERROR("Assertion [{}<{}] failed values are ({}) and ({})", #x, #y, (x), (y));                                           \
  }
#define ASSERT_lt(x, y) ASSERT_lessThan(x, y)
#define ASSERT_greaterThan(x, y)                                                                                                   \
  if (!(x > y)) {                                                                                                                  \
    SIMPLE_ERROR("Assertion [{}>{}] failed values are ({}) and ({})", #x, #y, (x), (y));                                           \
  }
#define ASSERT_gt(x, y) ASSERT_greaterThan(x, y)

#if DEBUG_TRAP_NULLS == 1
#define ASSERTNOTNULLP(x, e)                                                                                                       \
  if (!(x)) {                                                                                                                      \
    SIMPLE_ERROR("Assertion failed! (shared_ptr: {} is NULL) {}", #x, (e));                                                        \
  };
#else
#define ASSERTNOTNULLP(x, e)
#endif

#define ASSERTNOTNULL(x) ASSERTNOTNULLP(x, "")
#define ANN(x) ASSERTNOTNULL(x)
#else // DEBUG_ASSERT
#define lisp_ASSERT(l, x)                                                                                                          \
  {}
#define ASSERT(x)                                                                                                                  \
  {}
#define lisp_ASSERTP(l, x, e)                                                                                                      \
  {}
#define ASSERTP(x, e)                                                                                                              \
  {}
#define IF_DEBUG_ON(x)
#define lisp_ASSERTF(l, x, f)                                                                                                      \
  {}
#define ASSERTF(x, ...)                                                                                                            \
  {}
#define ASSERT_eq(x, y)                                                                                                            \
  {}
#define ASSERT_lessThan(x, y)                                                                                                      \
  {}
#define ASSERT_lt(x, y)                                                                                                            \
  {}
#define ASSERT_greaterThan(x, y)                                                                                                   \
  {}
#define ASSERT_gt(x, y)                                                                                                            \
  {}
#define ASSERTNOTNULLP(x, e)                                                                                                       \
  {}
#define ASSERTNOTNULL(x)                                                                                                           \
  {}
#define ANN(x)                                                                                                                     \
  {}
#endif

#ifdef INIT_DEBUG
#define INIT_LOG(___fmt) LOG(___fmt)
#else
#define INIT_LOG(___fmt)                                                                                                           \
  {}
#endif

//
// Define the _lisp variable but don't create a debugging stack frame
//
#define LOG_CXX_FUNCTION_INVOCATION()                                                                                              \
  core::CxxFunctionInvocationLogger __cxxFunctionInvocationLogger(__FILE__, __FUNCTION__, __LINE__);

#ifdef CALLSTACK_ON //[
#define LOG_CXX_FUNCTION_INVOCATION()                                                                                              \
  core::CxxFunctionInvocationLogger __cxxFunctionInvocationLogger(__FILE__, __FUNCTION__, __LINE__);

#define _G() LOG_CXX_FUNCTION_INVOCATION();
#define _OF() _G();
#else //][
#define _G()
#define _OF()
#endif //]

string debugFlagsAsNodeName(uint flags);

/*! When the debugger is invoked the user can say that they want to abort the current
          evaluate in the REPL */

class DebuggerSaysAbortToRepl {};

/*! When an error is thrown and the debugger is invoked, the
          user can say they want to continue with a user provided return value. */
class DebuggerSaysContinue {
private:
  core::T_mv _ReturnObject;

public:
  DebuggerSaysContinue(T_mv ro) { this->_ReturnObject = ro; };
  T_mv returnObject() { return this->_ReturnObject; };
};

[[noreturn]] void core__function_wrong_type_argument(Symbol_sp function, T_sp value, T_sp type);

[[noreturn]] void core__wrong_type_argument(const string& sourceFile, int lineno, Symbol_sp function, T_sp value, T_sp type);

void af_wrongTypeKeyArg(const string& sourceFile, int lineno, Symbol_sp function, T_sp key, T_sp value, T_sp type);

[[noreturn]] void core__wrong_type_nth_arg(const string& sourceFile, int lineno, Symbol_sp function, int narg, T_sp value,
                                           T_sp type);

[[noreturn]] void af_wrongTypeOnlyArg(const string& sourceFile, int lineno, Symbol_sp function, T_sp value, T_sp type);

void core__wrong_index(const string& sourceFile, int lineno, Symbol_sp function, T_sp array, int which, T_sp index,
                       int noninc_index);

void core__reader_error_internal(const string& sourceFile, uint lineno, String_sp fmt, List_sp fmtargs, T_sp stream = nil<T_O>());

T_sp core__signal_simple_error(T_sp baseCondition, T_sp continueMessage, T_sp formatControl, T_sp formatArgs, T_sp args);

[[noreturn]] void file_libc_error(T_sp error_type, T_sp stream, const char* msg, int narg, ...);
[[noreturn]] void FEerror(const string& fmt, int numArgs, ...);
void FEtype_error_list(T_sp thing);
void FElibc_error(const char* fmt, int nargs, ...);
void FEcannot_open(T_sp fname);
void FEdoes_not_exist(T_sp fname);
void FEexists(T_sp fname);
void FEargument_number_error(T_sp supplied, T_sp min, T_sp max);
T_sp CEerror(T_sp c, const char* fmt, int numArgs, ...);

void FEpackage_error(const char* fmt, T_sp package, int nargs, ...);
void CEpackage_lock_violation(T_sp pkg, const char* fmt, int nargs, ...);
void CEpackage_error(const char* fmt, const char* continue_message, T_sp package, int nargs, ...);
void Warn(T_sp datum, List_sp arguments);

void clasp_internal_error(const char* error);

[[noreturn]] void wrong_number_of_arguments(T_O* closure, std::size_t given, std::size_t expected);

}; // namespace core
