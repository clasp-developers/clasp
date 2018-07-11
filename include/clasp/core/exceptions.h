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

#ifndef EXCEPTIONS_H
#define EXCEPTIONS_H 1
#include <string>
#include <exception>
#include <sstream>
#include <iostream>
#include <fstream>
#include <string.h>
#include <set>

#include <clasp/core/object.fwd.h>
#include <clasp/core/symbol.fwd.h>
#include <clasp/core/stacks.h>
#include <clasp/core/evaluator.fwd.h>
#include <clasp/core/lispStream.fwd.h>
#include <clasp/core/primitives.fwd.h>

namespace cl {
extern core::Symbol_sp& _sym_cellError;
};
namespace kw {
extern core::Symbol_sp& _sym_name;
};

/* ------------------------------------------------------------
 * Macros for errors and diagnostics
 */
/*! Indicate the function does not return */
#define NO_RETURN

#define INTERNAL_ERROR(_msg_) THROW_HARD_ERROR(_msg_)
#define TESTING() printf("%s:%d:%s Testing\n", __FILE__, __LINE__, __FUNCTION__);
#define TESTINGF(fmt) printf("%s:%d:%s Testing: %s\n", __FILE__, __LINE__, __FUNCTION__, (fmt).str().c_str());
#define NO_INITIALIZERS_ERROR(_type_)                                                     \
  {                                                                                       \
    lisp_error( _type_, _Nil<core::Cons_O>()); \
    THROW_NEVER_REACH();                                                                  \
  }
#define SIMPLE_WARN(_boost_fmt_) core::eval::funcall(cl::_sym_warn, core::SimpleBaseString_O::make((_boost_fmt_).str()))
#define ERROR(_type_, _initializers_) lisp_error( _type_, _initializers_)
#define SIMPLE_ERROR_SPRINTF(...) ::core::lisp_error_sprintf(__FILE__, __LINE__, __FUNCTION__, __VA_ARGS__)
#define SIMPLE_ERROR(_boost_fmt_) ::core::lisp_error_simple(__FUNCTION__, __FILE__, __LINE__, _boost_fmt_)
#define NOT_ENVIRONMENT_ERROR(_e_)                                                                  \
  ERROR(cl::_sym_simpleTypeError,                                                                      \
        core::lisp_createList(kw::_sym_formatControl, core::lisp_createStr("~S is not a bclasp environment"), \
                              kw::_sym_formatArguments, core::lisp_createList(_e_),                  \
                              kw::_sym_expectedType, core::_sym_Environment_O, \
                              kw::_sym_datum, _e_));
#define SIMPLE_ERROR_BF(_str_) SIMPLE_ERROR(BF(_str_))

/*! Error for when an index is out of range - eg: beyond the end of a string */
#define OLD_TYPE_ERROR_INDEX(_seq_, _idx_) \
  ERROR(cl::_sym_simpleTypeError, \
        core::lisp_createList(kw::_sym_formatControl, core::lisp_createStr("~S is not a valid index into the object ~S"), \
                              kw::_sym_formatArguments, core::lisp_createList(clasp_make_fixnum(_idx_), _seq_), \
                              kw::_sym_expectedType, core::lisp_createList(cl::_sym_integer, clasp_make_fixnum(0), make_fixnum((gc::IsA<Instance_sp>(_seq_) ? gc::As<Instance_sp>(_seq_)->numberOfSlots() : (_seq_)->length()) - 1)), \
                              kw::_sym_datum, clasp_make_fixnum(_idx_)));

/*! Error for when an index is out of range - eg: beyond the end of a string */
#define TYPE_ERROR_INDEX(_seq_, _idx_) \
  ERROR(cl::_sym_simpleTypeError, \
        core::lisp_createList(kw::_sym_formatControl, core::lisp_createStr("~S is not a valid index into the object ~S"), \
                              kw::_sym_formatArguments, core::lisp_createList(make_fixnum(_idx_), _seq_), \
                              kw::_sym_expectedType, core::lisp_createList(cl::_sym_integer, \
                                                                           make_fixnum(0), \
                                                                           make_fixnum((_seq_)->length()-1)), \
                              kw::_sym_datum, make_fixnum(_idx_)));

#define TYPE_ERROR_INDEX_VARIABLE(message, seqobject, idxobject,len) \
  ERROR(cl::_sym_simpleTypeError, \
        core::lisp_createList(kw::_sym_formatControl, core::lisp_createStr(message), \
                              kw::_sym_formatArguments, core::lisp_createList(idxobject, seqobject), \
                              kw::_sym_expectedType, core::lisp_createList(cl::_sym_integer, \
                                                                           make_fixnum(0), \
                                                                           make_fixnum(len-1)), \
                              kw::_sym_datum, idxobject));

#define TYPE_ERROR_PROPER_LIST(_lst_)                                                                  \
  ERROR(cl::_sym_simpleTypeError,                                                                      \
        core::lisp_createList(kw::_sym_formatControl, core::lisp_createStr("~S is not a proper list"), \
                              kw::_sym_formatArguments, core::lisp_createList(_lst_),                  \
                              kw::_sym_expectedType, cl::_sym_cons,                                    \
                              kw::_sym_datum, _lst_));

// this might be a copy-paste error _obj_ is declared, but _lst_ is used
// the text also does not work fit the error name
// can't find a use in all clasp
#define TYPE_ERROR_NO_FILL_POINTER(_obj_)                                                                  \
  ERROR(cl::_sym_simpleTypeError,                                                                      \
        core::lisp_createList(kw::_sym_formatControl, core::lisp_createStr("~S is not a proper list"), \
                              kw::_sym_formatArguments, core::lisp_createList(_lst_),                  \
                              kw::_sym_expectedType, cl::_sym_cons,                                    \
                              kw::_sym_datum, _lst_));

#define TYPE_ERROR_LIST(_lst_)                                                                  \
  ERROR(cl::_sym_simpleTypeError,                                                               \
        core::lisp_createList(kw::_sym_formatControl, core::lisp_createStr("~S is not a list"), \
                              kw::_sym_formatArguments, core::lisp_createList(_lst_),           \
                              kw::_sym_expectedType, cl::_sym_cons,                             \
                              kw::_sym_datum, _lst_));

#define TYPE_ERROR(_datum_, _expectedType_) ERROR(::cl::_sym_typeError, core::lisp_createList(kw::_sym_datum, _datum_, kw::_sym_expectedType, _expectedType_))
#define PROGRAM_ERROR() ERROR(cl::_sym_programError, (_Nil<T_O>()))
#define SIMPLE_PROGRAM_ERROR(message, datum)                                                     \
        ERROR(core::_sym_simpleProgramError,                                                     \
              core::lisp_createList(kw::_sym_formatControl, core::lisp_createStr(message),kw::_sym_formatArguments, core::lisp_createList(datum)))
#define SIMPLE_PROGRAM_ERROR_2_ARGS(message, datum1, datum2)                                                     \
        ERROR(core::_sym_simpleProgramError,                                                     \
              core::lisp_createList(kw::_sym_formatControl, core::lisp_createStr(message),kw::_sym_formatArguments, core::lisp_createList(datum1, datum2)))

#define FILE_ERROR(_file_) ERROR(cl::_sym_fileError, core::lisp_createList(kw::_sym_pathname, _file_))
#define CANNOT_OPEN_FILE_ERROR(_file_) FILE_ERROR(_file_)
#define TOO_FEW_ARGUMENTS_ERROR() NO_INITIALIZERS_ERROR(core::_sym_tooFewArgumentsError)
//#define TOO_MANY_ARGUMENTS_ERROR() NO_INITIALIZERS_ERROR(core::_sym_tooManyArgumentsError)
//#define UNRECOGNIZED_KEYWORD_ARGUMENTS_ERROR(obj) ERROR(core::_sym_unrecognizedKeywordArgumentsError,obj)
#define INVALID_KEYWORD_ARGUMENT_ERROR(obj) ERROR(core::_sym_invalidKeywordArgumentError, obj)
#define STREAM_ERROR(st) ERROR(cl::_sym_streamError, core::lisp_createList(kw::_sym_stream, st))
#define PACKAGE_ERROR(p) ERROR(cl::_sym_package_error, core::lisp_createList(kw::_sym_package, p))
#define ERROR_END_OF_FILE(st) ERROR(cl::_sym_endOfFile, core::lisp_createList(kw::_sym_stream, st))
#define CLOSED_STREAM_ERROR(st) ERROR(core::_sym_closedStream, core::lisp_createList(kw::_sym_stream, st))

#define READER_ERROR(_fmt_, _fmtArgs_, _stream_) cl__reader_error(__FILE__, __LINE__, _fmt_, _fmtArgs_, _stream_)
#define PARSE_ERROR(_fmt_, _fmtArgs_) cl__reader_error(__FILE__, __LINE__, _fmt_, _fmtArgs_, _Nil<Stream_O>())

#define PRINT_NOT_READABLE_ERROR(obj) ERROR(cl::_sym_printNotReadable, core::lisp_createList(kw::_sym_object, obj));
#define CELL_ERROR(name) ERROR(cl::_sym_cellError, core::lisp_createList(kw::_sym_name, name))
#define UNBOUND_VARIABLE_ERROR(name) ERROR(::cl::_sym_unboundVariable, core::lisp_createList(kw::_sym_name, name))
#define KEY_NOT_FOUND_ERROR(_key_) SIMPLE_ERROR(BF("Key %s not found") % _key_)
#define CONTROL_ERROR() NO_INITIALIZERS_ERROR(cl::_sym_controlError);

#define WRONG_TYPE_ARG(_datum_, _expectedType_) core__wrong_type_argument(__FILE__, __LINE__, core::lisp_intern(__FUNCTION__, CurrentPkg), _datum_, _expectedType_)

#define FUNCTION_WRONG_TYPE_ARG(function_name, _datum_, _expectedType_) core__function_wrong_type_argument( function_name, _datum_, _expectedType_)

#define ERROR_WRONG_TYPE_KEY_ARG(_fn_, _key_, _value_, _type_) af_wrongTypeKeyArg(__FILE__, __LINE__, _fn_, _key_, _value_, _type_)

#define ERROR_WRONG_TYPE_ONLY_ARG(_fn_, _datum_, _expectedType_) af_wrongTypeOnlyArg(__FILE__, __LINE__, _fn_, _datum_, _expectedType_)

#define ERROR_WRONG_TYPE_NTH_ARG(_fn_, _nth_, _datum_, _expectedType_) core__wrong_type_nth_arg(__FILE__, __LINE__, _fn_, _nth_, _datum_, _expectedType_)

#define QERROR_WRONG_TYPE_NTH_ARG(_nth_, _datum_, _expectedType_) core__wrong_type_nth_arg(__FILE__, __LINE__, core::lisp_intern(__FUNCTION__, CurrentPkg), _nth_, _datum_, _expectedType_)

#define ARITHMATIC_ERROR(_operation_, _operands_) ERROR(cl::_sym_arithmaticError, core::lisp_createList(kw::_sym_operation, _operation_, kw::_sym_operands, _operands_))

#define ERROR_DIVISION_BY_ZERO(_x_, _y_) ERROR(cl::_sym_divisionByZero, core::lisp_createList(kw::_sym_operation, cl::_sym__DIVIDE_, kw::_sym_operands, core::lisp_createList(_x_, _y_)))

#define ERROR_UNDEFINED_FUNCTION(_f_) ERROR(::cl::_sym_undefinedFunction, core::lisp_createList(kw::_sym_name, _f_));
#define FE_ERROR(_type_, _args_)

namespace core {

class DebugStream;

/*! To exit the program throw this exception
 */
 class ExitProgramException : public std::exception{
private:
  int _ExitResult;

public:
  ExitProgramException(int result) : _ExitResult(result){};
  int getExitResult() { return this->_ExitResult; };
};

struct SaveLispAndDie {
   string _FileName;
 SaveLispAndDie(const std::string& filename) : _FileName(filename) {};
 };
 
/*! To exit the program throw this exception
 */
class TerminateProgramIfBatch {
private:
  int _ExitResult;
  string _Message;

public:
  TerminateProgramIfBatch(int result, string const &message) : _ExitResult(result), _Message(message){};
  string message() const { return this->_Message; };
  int getExitResult() { return this->_ExitResult; };
};

#if 1
#pragma GCC visibility push(default)
class ATTR_WEAK CatchThrow {
  virtual void keyFunctionForVtable() ATTR_WEAK;

private:
  int _Frame;

public:
  CatchThrow(int frame) : _Frame(frame){};
  int getFrame() { return this->_Frame; };
  /*ATTR_WEAK*/ virtual ~CatchThrow(){};
};
#else
#pragma GCC visibility push(default)
class ATTR_WEAK CatchThrow {
  virtual void keyFunctionForVtable() ATTR_WEAK;

private:
  T_sp _ThrownTag;
  T_mv _ReturnedObject;

public:
  CatchThrow(T_sp thrownTag, T_mv ret) {
    this->_ThrownTag = thrownTag;
    this->_ReturnedObject = ret;
  }
  ~T_sp getThrownTag() { return this->_ThrownTag; };
  T_mv getReturnedObject() { return this->_ReturnedObject; };
  /*ATTR_WEAK*/ virtual ~CatchThrow(){};
};
#endif

class ATTR_WEAK ReturnFrom //: public gctools::HeapRoot
    {
  virtual void keyFunctionForVtable() ATTR_WEAK; // MUST BE FIRST VIRTUAL FUNCTION
private:
  T_O* _Handle;
public:
  ReturnFrom(T_O* handle) {
    this->_Handle = handle;
  }
  T_O* getHandle() const { return this->_Handle; };
  /*ATTR_WEAK*/ virtual ~ReturnFrom(){};
};

/*! Thrown by slot_ref when a slot_ref call fails because the symbol_name is invalid */

class SlotRefFailed {
};


class ATTR_WEAK LexicalGo {
  virtual void keyFunctionForVtable() ATTR_WEAK;

private:
  T_O* _Handle;
  int _Index;

public:
  ATTR_WEAK LexicalGo(T_O* handle, int index) : _Handle(handle), _Index(index){};
  T_O* getHandle() const { return this->_Handle; };
  int index() const { return this->_Index; };
  /*ATTR_WEAK*/ virtual ~LexicalGo(){};
};

class ATTR_WEAK DynamicGo //: public gctools::HeapRoot
    {
  virtual void keyFunctionForVtable() ATTR_WEAK;

private:
  T_O*   _Handle;
  size_t _Index;
public:
  ATTR_WEAK DynamicGo(T_O* handle, size_t index) : _Handle(handle), _Index(index){};
  /*ATTR_WEAK*/ virtual ~DynamicGo(){};
  T_O* getHandle() const { return this->_Handle; };
  size_t index() const { return this->_Index; };
};

class ATTR_WEAK Unwind {
  virtual void keyFunctionForVtable() ATTR_WEAK;

private:
  T_O *_Frame; // NOT GC'd use FIXNUM tagged_ptr
  size_t _Index;

public:
  ATTR_WEAK Unwind(T_O *frame, size_t index) : _Frame(frame), _Index(index){};
  /*ATTR_WEAK*/ virtual ~Unwind(){};
  T_O *getFrame() const { return this->_Frame; };
  size_t index() const { return this->_Index; };
};

struct TooManyArgumentsError {
  int givenNumberOfArguments;
  int requiredNumberOfArguments;
  TooManyArgumentsError(int given, int required);
};

class TooFewArgumentsError {
private:
  TooFewArgumentsError();

public:
  int givenNumberOfArguments;
  int requiredNumberOfArguments;
  TooFewArgumentsError(int given, int required);
};

struct UnrecognizedKeywordArgumentError {
  core::T_sp argument;
  UnrecognizedKeywordArgumentError(core::T_sp arg) : argument(arg){};
};

#pragma GCC visibility pop

void throwTooFewArgumentsError(size_t given, size_t required);

void throwTooManyArgumentsError(size_t given, size_t required);

void throwUnrecognizedKeywordArgumentError(T_sp kw);

void wrongNumberOfArguments(size_t givenNumberOfArguments, size_t requiredNumberOfArguments);

/*! Used by the debugger to resume the read-eval-print-loop */
class ResumeREPL {
};

/*! Set a break-point in _trapThrow to catch
         * every exception except those thrown by THROW_noTrap
         */
extern void _trapThrow(const string &fileName, uint line, const string &msg);

/*! If DEBUG_SOURCE macro is defined then this class uses RAII to log entry/exit of C++ functions
          An instance of this struct is declared by the _G() macro */
struct CxxFunctionInvocationLogger {
  CxxFunctionInvocationLogger(const char *cPSourceFile, const char *cPFunctionName, uint lineNumber);
  virtual ~CxxFunctionInvocationLogger();
};

#define lisp_THROW_base(l, x, m) { \
    core::eval::funcall(lisp_symbolFunction(core::_sym_setThrowPosition), x, core::lisp_createStr(__FILE__), core::lisp_createStr(__FUNCTION__), core::lisp_createFixnum(__LINE__)); \
    core::lisp_logException(__FILE__, __FUNCTION__, __LINE__, m, x); \
    core::eval::funcall(core::_sym_error, x); \
    throw_hard_error("Never reach here in lisp_THROW_base"); \
  }

#define UNREACHABLE() throw_hard_error("Should never reach here")

// Make it easier to find "try" statements
#define TRY() try

#define IMPLEMENT_ME() SIMPLE_ERROR(BF("Implement function %s:%d %s") % __FILE__ % __LINE__ % __FUNCTION__)
#define IMPLEMENT_MEF(msg) SIMPLE_ERROR(BF("Implement function %s:%s %s %s") % __FILE__ % __LINE__ % __FUNCTION__ % msg)
 
#define NOT_SUPPORTED() SIMPLE_ERROR(BF("Subclass(%s) does not support the function(%s) file(%s) lineNumber(%d)") % this->className() % __FUNCTION__ % __FILE__ % __LINE__);


#define HALT(bfmsg) THROW_HARD_ERROR(BF("%s:%d HALTED --- %s\n") % __FILE__ % __LINE__ % (bfmsg).str());
#define NOT_APPLICABLE() throw_hard_error_not_applicable_method(__FUNCTION__);
#define N_A_() NOT_APPLICABLE()
#define INCOMPLETE(bf) SIMPLE_ERROR(BF("Finish implementing me!!!! function(%s) file(%s) lineNumber(%s): %s") % __FUNCTION__ % __FILE__ % __LINE__ % (bf).str())
#define FIX_ME() SIMPLE_ERROR(BF("Fix me!!! function(%s) file(%s) lineNumber(%s)") % __FUNCTION__ % __FILE__ % __LINE__);
#define DEPRECATED() SIMPLE_ERROR(BF("Depreciated!!! function(%s) file(%s) lineNumber(%d)") % __FUNCTION__ % __FILE__ % __LINE__);
#define STUB() printf("%s:%d>%s  stub\n", __FILE__, __LINE__, __FUNCTION__ )

#define MAY_BE_DEPRECATED() printf("%s\n", (BF("May be depreciated!!! function(%s) file(%s) lineNumber(%d)") % __FUNCTION__ % __FILE__ % __LINE__).str().c_str());
#define DEPRECATEDP(s) SIMPLE_ERROR(BF("Depreciated!!! function(%s) file(%s) lineNumber(%d) %s") % __FUNCTION__ % __FILE__ % __LINE__ % (s));

FORWARD(Cons);

#ifdef DEBUG_ASSERT
#define HARD_ASSERT(t)                                                                   \
  if (!(t)) {                                                                            \
    core::errorFormatted("HARD_ASSERT failed");                                          \
    throw(HardError(__FILE__, __FUNCTION__, __LINE__, "Assertion " #t " failed")); \
  };
#define HARD_ASSERTF(t, fmt) \
  if (!(t)) { \
    core::errorFormatted(fmt); \
    throw(core::HardError(__FILE__, __FUNCTION__, __LINE__, BF("Assertion %s failed: %s") % #t % (fmt).str())); \
  };
#else
#define HARD_ASSERT(t) \
  {}
#define HARD_ASSERTF(t, f) \
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
  const char *_DebugTraceSourceFileName;
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
  bool isOn(const char *fileName, uint debugFlags);
  DebugStream &beginNode(uint debugFlags);
  DebugStream &beginNode(uint debugFlags, const char *sourceFile, const char *functionName, uint lineNumber, uint col, const string &message);
  DebugStream &endNode(uint debugFlags);
  DebugStream &writeRaw(const string &data);
  DebugStream &writeText(const string &data);
  DebugStream &writeTextCr(const string &data);
  DebugStream &writeInt(uint i);
  DebugStream &writePtr(void *);
  DebugStream &writeLn();
  DebugStream &log(const string &msg);
//  DebugStream &log(const boost::format &fmt);

  string nextPosition();

  bool debugAll() { return this->_DebugAll; };
  bool debugLisp() { return this->_DebugLisp; };
  bool debugScript() { return this->_DebugScript; };
  void addDebugFileName(const string &fn) { this->_DebugFileNames.insert(fn); };
  bool recognizesDebugFileName(const string &fn);
  string allDebugFileNames();

  void setSuppressMessages(bool s);
  bool getSuppressMessages() { return this->_SuppressMessages; };

  //	void	setTraceFileLine(const char* f, uint l);
  //	void	setDebugPrefix(const char* s);
  //	const char*	getTraceFile();
  //	uint	getTraceLine();
  const char *getDebugPrefix();

  void lazyInitialize();

  //	_LispCallStackHolder& lispCallStack();

  DebugStream(int rank);
  virtual ~DebugStream();
};

extern void debugLogProcessRank(int rank);
extern void debugBreakPoint();

#define EXCEPTION_ATOM_NOT_FOUND 1
#define EXCEPTION_FRAGMENT_NOT_FOUND 2
#define EXCEPTION_LOOP_OVER_NULL 3
#define EXCEPTION_CONTENT_NOT_FOUND 4
#define EXCEPTION_255_TYPES_ALLOWED_MAX 5
#define EXCEPTION_BOND_NOT_FOUND 6
#define EXCEPTION_ANGLE_NOT_FOUND 7
#define EXCEPTION_TORSION_NOT_FOUND 8
#define EXCEPTION_TYPE_NOT_FOUND 9
#define EXCEPTION_NONBOND_NOT_FOUND 10
#define EXCEPTION_FILE_ERROR 11
#define EXCEPTION_ZERO_VECTOR_CANNOT_BE_NORMALIZED 12
#define EXCEPTION_ILLEGAL_LOOP 13
#define EXCEPTION_DKP_NOT_FOUND 14
#define EXCEPTION_INVALID_MOE_HEADER 15
#define EXCEPTION_UNKNOWN_MOE_TYPE 16
#define EXCEPTION_UNKNOWN_MOE_ATOM_TYPE 17

typedef struct {
  int exception;
  char message[500];
} Exception;

#define NOT_DONE_YET() \
  { printf("\n\n\n%s:%d FUNCTION NOT DONE YET!!!!\n\n\n\n", __FILE__, __LINE__); };

#define EXCEPTION_ID(e) (e.exception)
#define EXCEPTION_MESSAGE(e) (e.message)

void debugSuppressMessages(bool s);
/*! Write to the debug log file without any translation of special
 *   XML characters
 */
// void	debug=RawWrite( const char* fmt, ... );
/*! Write to the debug log file and translate XML characters
 */
// void	debug=Printf( const char* fmt, ... );

// char*	internal=BufferPrintf( const char* fmt, ... );

#ifdef DEBUG_ON
//#error "TURN OFF DEBUG_ON"
#define TESTMEMORY()

#define HARD_BREAK_POINT() __asm int 3;
#define lisp_LOG(___fmt)                                                     \
  {                                                                          \
    if (core::lisp_debugIsOn(__FILE__)) {                                    \
      core::lisp_debugLogWrite(__FILE__, __FUNCTION__, __LINE__, 0, ___fmt); \
    }                                                                        \
  }
#define LOG(___fmt) lisp_LOG(___fmt)

#define lisp_SHOUT(___fmt)                                                   \
  {                                                                          \
    if (core::lisp_debugIsOn(__FILE__, DEBUG_SHOUT)) {                       \
      core::lisp_debugLogWrite(__FILE__, __FUNCTION__, __LINE__, 0, ___fmt); \
    }                                                                        \
  }
#define SHOUT(___fmt) lisp_SHOUT(___fmt)

#else //DEBUG_ON
#define TESTMEMORY()
#define HARD_BREAK_POINT() \
  {}
#define LOG(___fmt) \
  {}
#define lisp_LOG(___fmt) \
  {}
#define SHOUT(___fmt) \
  {}
#define lisp_SHOUT(___fmt) \
  {}

#endif //DEBUG_ON


 void assert_failure(const char* file, size_t line, const char* func, const char* msg);
 
#ifdef DEBUG_ASSERT
#define lisp_ASSERT(x) if (!(x)) ::core::assert_failure(__FILE__,__LINE__,__FUNCTION__,#x)
#define ASSERT(x) lisp_ASSERT(x)
#endif
#ifdef DEBUG_BOUNDS_ASSERT
#define lisp_BOUNDS_ASSERT(x) if (!(x)) ::core::assert_failure(__FILE__,__LINE__,__FUNCTION__,#x)
#define BOUNDS_ASSERT(x) lisp_BOUNDS_ASSERT(x)
 #else
#define BOUNDS_ASSERT(x)
#endif
#ifdef DEBUG_ASSERT
#define lisp_ASSERTP( x, e) if (!(x)) ::core::assert_failure(__FILE__,__LINE__,__FUNCTION__,(e));
#define ASSERTP(x, e) lisp_ASSERTP(x, e)
#define lisp_ASSERTF( x, e) if (!(x)) ::core::assert_failure(__FILE__,__LINE__,__FUNCTION__,(e).str().c_str());
#define ASSERTF(x, f) lisp_ASSERTF(x, f)
#define ASSERT_eq(x, y)                                                                           \
  if (!(x == y)) {                                                                                \
    SIMPLE_ERROR(BF("Assertion [%s==%s] failed values are (%s) and (%s)") % #x % #y % (x) % (y)); \
  }
#define ASSERT_lessThan(x, y)                                                                    \
  if (!(x < y)) {                                                                                \
    SIMPLE_ERROR(BF("Assertion [%s<%s] failed values are (%s) and (%s)") % #x % #y % (x) % (y)); \
  }
#define ASSERT_lt(x, y) ASSERT_lessThan(x, y)
#define ASSERT_greaterThan(x, y)                                                                 \
  if (!(x > y)) {                                                                                \
    SIMPLE_ERROR(BF("Assertion [%s>%s] failed values are (%s) and (%s)") % #x % #y % (x) % (y)); \
  }
#define ASSERT_gt(x, y) ASSERT_greaterThan(x, y)
#define FATAL_ASSERTP(x, e)                                                                             \
  if (!(x)) {                                                                                           \
    SIMPLE_ERROR(_lisp, core::LispError_O::create(BF("Assertion [%s] failed - %s") % #x % (e), _lisp)); \
  };

#if DEBUG_TRAP_NULLS == 1
#define ASSERTNOTNULLP(x, e)                                                      \
  if (!(x)) {                                                                     \
    SIMPLE_ERROR(BF("Assertion failed! (shared_ptr: %s is NULL) %s") % #x % (e)); \
  };
#else
#define ASSERTNOTNULLP(x, e)
#endif

#define ASSERTNOTNULL(x) ASSERTNOTNULLP(x, "")
#define ANN(x) ASSERTNOTNULL(x)
#else // DEBUG_ASSERT
#define lisp_ASSERT(l, x) \
  {}
#define ASSERT(x) \
  {}
#define lisp_ASSERTP(l, x, e) \
  {}
#define ASSERTP(x, e) \
  {}
#define IF_DEBUG_ON(x)
#define lisp_ASSERTF(l, x, f) \
  {}
#define ASSERTF(x, f) \
  {}
#define ASSERT_eq(x, y) \
  {}
#define ASSERT_lessThan(x, y) \
  {}
#define ASSERT_lt(x, y) \
  {}
#define ASSERT_greaterThan(x, y) \
  {}
#define ASSERT_gt(x, y) \
  {}
#define FATAL_ASSERTP(x, e) \
  {}
#define ASSERTNOTNULLP(x, e) \
  {}
#define ASSERTNOTNULL(x) \
  {}
#define ANN(x) \
  {}
#endif

#ifdef INIT_DEBUG
#define INIT_LOG(___fmt) LOG(___fmt)
#else
#define INIT_LOG(___fmt) \
  {}
#endif

extern void _stackTraceEnter(uint debugFlags);
extern void _stackTraceLineNumberAndColumnUpdate(uint ln, uint col);
extern void _stackTraceExit();
extern void _stackTraceDump();
extern void _stackTraceTakeSnapshot();
extern string _stackTraceAsString();

//
// Define the _lisp variable but don't create a debugging stack frame
//
#define LOG_CXX_FUNCTION_INVOCATION() core::CxxFunctionInvocationLogger __cxxFunctionInvocationLogger(__FILE__, __FUNCTION__, __LINE__);

#ifdef CALLSTACK_ON //[
#define _G()
#define _OF() _G();
#define _lisp_BLOCK_TRACEF(__f) \
  {} // core::_StackTrace _B_stackTrace(__FILE__,"LexicalScope",__LINE__,0,DEBUG_CPP_BLOCK,__f)
#define _lisp_BLOCK_TRACE(__s) _lisp_BLOCK_TRACEF(BF("%s") % (__s))
#define _BLOCK_TRACEF(f) _lisp_BLOCK_TRACEF(f)
#define _BLOCK_TRACE(s) _lisp_BLOCK_TRACEF(BF("%s") % (s))
#else //][
#define _G() 
#define _OF()
#define _lisp_BLOCK_TRACEF(__f)
#define _lisp_BLOCK_TRACE(__s)
#define _BLOCK_TRACE(f)
#define _BLOCK_TRACEF(f)
#define _lisp_BLOCK_TRACEF(__f)
#endif //]

string debugFlagsAsNodeName(uint flags);

/*! When the debugger is invoked the user can say that they want to abort the current
          evaluate in the REPL */

class DebuggerSaysAbortToRepl {
};

/*! When an error is thrown and the debugger is invoked, the
          user can say they want to continue with a user provided return value. */
class DebuggerSaysContinue {
private:
  core::T_mv _ReturnObject;

public:
  DebuggerSaysContinue(T_mv ro) { this->_ReturnObject = ro; };
  T_mv returnObject() { return this->_ReturnObject; };
};

void core__function_wrong_type_argument(Symbol_sp function, T_sp value, T_sp type);

void core__wrong_type_argument(const string &sourceFile, int lineno, Symbol_sp function, T_sp value, T_sp type);

void af_wrongTypeKeyArg(const string &sourceFile, int lineno, Symbol_sp function, T_sp key, T_sp value, T_sp type);

void core__wrong_type_nth_arg(const string &sourceFile, int lineno, Symbol_sp function, int narg, T_sp value, T_sp type);

void af_wrongTypeOnlyArg(const string &sourceFile, int lineno, Symbol_sp function, T_sp value, T_sp type);

void core__wrong_index(const string &sourceFile, int lineno, Symbol_sp function, T_sp array, int which, T_sp index, int noninc_index);

 void cl__reader_error(const string &sourceFile, uint lineno,
                    String_sp fmt, List_sp fmtargs, T_sp stream = _Nil<T_O>());

void assert_type_integer(T_sp p, int idx);

T_sp core__signal_simple_error(T_sp baseCondition, T_sp continueMessage, T_sp formatControl, T_sp formatArgs, T_sp args);

[[noreturn]] void FEerror(const string &fmt, int numArgs, ...);
void FEtype_error_list(T_sp thing);
void FElibc_error(const char *fmt, int nargs, ...);
void FEcannot_open(T_sp fn);
 void FEargument_number_error(T_sp supplied, T_sp min, T_sp max);
T_sp CEerror(T_sp c, const char *fmt, int numArgs, ...);

void FEpackage_error(const char *fmt, T_sp package, int nargs, ...);
void CEpackage_error(const char *fmt, const char *continue_message, T_sp package, int nargs, ...);
void Warn(T_sp datum, List_sp arguments);

void clasp_internal_error(const char *error);

};

#endif
