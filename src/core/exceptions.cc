/*
    File: exceptions.cc
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
//#define DEBUG_LEVEL_FULL
// This should be the last TURN_DEBUG_off turned off when compiling production code

//
// (C) 2004 Christian E. Schafmeister
//

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <iostream>
#include <fstream>
#include <string>
#include <set>
#include <vector>
#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/exceptions.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/bformat.h>
#include <clasp/core/array.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/object.h>
#include <clasp/core/wrappers.h>

#ifdef WIN32
#define vsnprintf _vsnprintf
#endif

#define MAX_DEBUG_CALL_DEPTH 200

namespace core {

bool stackmap_log = true;

void assert_failure(const char* file, size_t line, const char* func, const char* msg)
{
  SIMPLE_ERROR(BF("%s:%d:%s  Assertion failure: %s") % file % line % func % msg);
}

void assert_failure_bounds_error_lt(const char* file, size_t line, const char* func, int64_t x, int64_t y)
{
  SIMPLE_ERROR(BF("%s:%d:%s  Assertion failure: bounds error - %s must be less than %s") % file % line % func % x % y);
}

/*! These are here just so that the clang compiler
      will assign the __attribute__((weak)) to the vtable of each of these classes
    */
void CatchThrow::keyFunctionForVtable(){};
void ReturnFrom::keyFunctionForVtable(){};
void DynamicGo::keyFunctionForVtable(){};
void Unwind::keyFunctionForVtable(){};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("Returns the list of active CL:CATCH tags. Strictly for debugging.");
CL_DEFUN List_sp core__active_catch_tags() {
  return my_thread->catchTags();
}

// The control transfer part of CL:THROW
[[noreturn]] void clasp_throw(T_sp tag) {
  // Check the list of catches in place to make sure the tag is there.
  bool found = false;
  for (auto tag_cons : my_thread->catchTags()) {
    if (tag == oCar(tag_cons)) {
      found = true;
      break;
    }
  }
  if (!found) CONTROL_ERROR();
  my_thread->_unwinds++;
  my_thread_low_level->_start_unwind = std::chrono::high_resolution_clock::now();
  throw CatchThrow(tag);
}

SYMBOL_EXPORT_SC_(KeywordPkg,called_function);
void throwTooFewArgumentsError(core::T_sp closure, size_t given, size_t required) {
  lisp_error(core::_sym_wrongNumberOfArguments,
             lisp_createList(kw::_sym_called_function, closure,
                             kw::_sym_givenNargs, make_fixnum(given),
                             kw::_sym_minNargs, make_fixnum(required)));
}

void throwTooManyArgumentsError(core::T_sp closure, size_t given, size_t required) {
  lisp_error(core::_sym_wrongNumberOfArguments,
             lisp_createList(kw::_sym_called_function, closure,
                             kw::_sym_givenNargs, make_fixnum(given),
                             kw::_sym_maxNargs, make_fixnum(required)));
}

void throwUnrecognizedKeywordArgumentError(core::T_sp closure, T_sp kw) {
  lisp_error(core::_sym_unrecognizedKeywordArgumentError,
             lisp_createList(kw::_sym_unrecognizedKeyword, kw,
                             kw::_sym_called_function,closure));
}

void wrongNumberOfArguments(core::T_sp closure, size_t givenNumberOfArguments, size_t requiredNumberOfArguments) {
  if (givenNumberOfArguments < requiredNumberOfArguments)
    throwTooFewArgumentsError(closure,givenNumberOfArguments, requiredNumberOfArguments);
  else
    throwTooManyArgumentsError(closure,givenNumberOfArguments, requiredNumberOfArguments);
}

#define DebugOpenLeft "{{{ "
#define DebugOpenRight ""
#define DebugClose "}}}"

//#define	DebugOpenLeft "/* {{{ "
//#define DebugOpenRight " */"
//#define	DebugClose "/* }}} */"

CxxFunctionInvocationLogger::CxxFunctionInvocationLogger(const char *cPSourceFile, const char *cPFunctionName, uint lineNumber) {
  _lisp->debugLog().beginNode(DEBUG_CPP_FUNCTION, cPSourceFile, cPFunctionName, lineNumber, 0, "");
};
CxxFunctionInvocationLogger::~CxxFunctionInvocationLogger() {
  _lisp->debugLog().endNode(DEBUG_CPP_FUNCTION);
};

size_t global_signal_simple_error_depth = 0;

struct SignalSimpleErrorTrap {
  SignalSimpleErrorTrap() {
    global_signal_simple_error_depth++;
  }
  ~SignalSimpleErrorTrap() {
    global_signal_simple_error_depth--;
  }
};
    

CL_LAMBDA(base-condition continue-message format-control format-args &rest args);
CL_DECLARE();
CL_DOCSTRING("signalSimpleError");
CL_DEFUN T_sp core__signal_simple_error(T_sp baseCondition, T_sp continueMessage, T_sp formatControl, T_sp formatArgs, T_sp args) {
  SignalSimpleErrorTrap depth;
  printf("%s:%d core__signal_simple_error  caught because signal-simple-error is not installed yet\n", __FILE__, __LINE__);
  printf("%s:%d baseCondition: %s\n", __FILE__, __LINE__, _rep_(baseCondition).c_str());
  printf("%s:%d formatControl: %s\n", __FILE__, __LINE__, _rep_(formatControl).c_str());
  printf("%s:%d    formatArgs: %s\n", __FILE__, __LINE__, _rep_(formatArgs).c_str());
  if ( global_signal_simple_error_depth > 3 ) {
    printf("%s:%d    signal-simple-error depth is %zu - aborting\n", __FILE__, __LINE__, global_signal_simple_error_depth );
    abort();
  }
  printf("%s:%d  About to try and FORMAT the error\n", __FILE__, __LINE__);
  cl__format(_lisp->_true(), formatControl, formatArgs);
  dbg_hook("core__signal_simple_error");
  core__invoke_internal_debugger(_Nil<core::T_O>());
  printf("%s:%d  Continuing...\n", __FILE__, __LINE__);
  return _Nil<T_O>();
};

void _trapThrow(const string &fileName, uint line, const string &msg) {
  /* do nothing */
}

string debugFlagsAsNodeName(uint flags) {
  if (flags == (DEBUG_LOG))
    return "LOG";
  if (flags == (DEBUG_LINE_COL))
    return "LINE-COL";
  if (flags == (DEBUG_CPP_FUNCTION))
    return "C++Func";
  if (flags == (DEBUG_CPP_BLOCK))
    return "C++Block";
  if (flags == DEBUG_INTERP_LISP_FUNCTION)
    return "iL-Func";
  if (flags == DEBUG_INTERP_LISP_LET)
    return "iL-LET";
  if (flags == DEBUG_INTERP_LISP_LET_STAR)
    return "iL-LET*";
  if (flags == DEBUG_INTERP_LISP_BLOCK)
    return "iL-BLOCK";
  if (flags == DEBUG_INTERP_LISP_TAGBODY)
    return "iL-TAGBODY";
  if (flags == DEBUG_INTERP_LISP_LAMBDA)
    return "iL-LAMBDA";
  if (flags == DEBUG_INTERP_LISP_FLET)
    return "iL-FLET";
  if (flags == DEBUG_INTERP_LISP_LABELS)
    return "iL-LABELS";
  if (flags == DEBUG_INTERP_LISP_CALL)
    return "iL-CALL";
  if (flags == DEBUG_COMPILED_LISP_FUNCTION)
    return "cL-Func";
  if (flags == DEBUG_COMPILED_LISP_LET)
    return "cL-LET";
  if (flags == DEBUG_COMPILED_LISP_LET_STAR)
    return "cL-LET*";
  if (flags == DEBUG_COMPILED_LISP_BLOCK)
    return "cL-BLOCK";
  if (flags == DEBUG_COMPILED_LISP_TAGBODY)
    return "cL-TAGBODY";
  if (flags == DEBUG_COMPILED_LISP_LAMBDA)
    return "cL-LAMBDA";
  if (flags == DEBUG_COMPILED_LISP_FLET)
    return "cL-FLET";
  if (flags == DEBUG_COMPILED_LISP_LABELS)
    return "cL-LABELS";
  if (flags == DEBUG_COMPILED_LISP_CALL)
    return "cL-CALL";
  if (flags == DEBUG_COMPILED_LISP_CATCH)
    return "cL-CATCH";
  if (flags == DEBUG_COMPILED_LISP_UNWIND_PROTECT)
    return "cL-UNWIND-PROTECT";
  if (flags == (DEBUG_TOPLEVEL))
    return "TOPLEVEL";
  stringstream ss;
  ss << "UnknownNodeName[" << std::hex << flags << "]";
  return ss.str();
}

void debugBreakPoint() {
  // Do nothing
}

CL_DEFUN void add_debug_filename(const string& name) {
  lisp_debugLog()->addDebugFileName(name);
}

DebugStream::DebugStream(int rank) : DebugLogAsXml(false) {
  this->_Enabled = true;
  this->_OutStreamOpen = false;
  this->DebugPositionCounter = 0;
  this->DebugCallDepth = 0;
  this->DebugLogAddBrackets = true;
  this->DebugLogProcessRank = rank;
  this->_SuppressMessages = false;
  //    this->_InvocationHistoryStack.setTraceFileLine("--Debugging off--",0);
  //    this->_DebugPrefix = "--Debugging off--";
  this->_DebugFileNames.clear();
  stringstream ss;
  pid_t pid = getpid();
  ss << "/tmp/_claspDebug_" << pid << ".log";
  this->_LogFileName = ss.str();
  char *cstr = getenv("CLASP_DEBUG");
  if (cstr != NULL) {
    printf("=== Initializing source code debug/log system\n");
    printf("--- Writing log to file: %s\n", this->_LogFileName.c_str());
    printf("--- Writing log messages for source files listed in CLASP_DEBUG environment variable\n");
    printf("--- CLASP_DEBUG environment variable is: %s\n", cstr);
    fflush(stdout);
    string allFileNames = cstr;
    vector<string> fileNameVector;
    tokenize(allFileNames, fileNameVector, ",");
    this->_DebugAll = false;
    this->_DebugScript = false;
    for (vector<string>::iterator fi = fileNameVector.begin();
         fi != fileNameVector.end(); fi++) {
      if ((*fi) == "ALL") {
        this->_DebugAll = true;
        printf("--- Logging all c-code source files\n");
        continue;
      }
      if ((*fi) == "LISP") {
        this->_DebugLisp = true;
        printf("--- Logging all lisp functions\n");
      }
      if ((*fi) == "SCRIPT") {
        this->_DebugScript = true;
        printf("--- Logging all script source files\n");
        continue;
      } else if ((*fi) == "DELAY") {
        printf("--- Turning off initial logging messages - use debugLogOn to turn them on\n");
        this->setSuppressMessages(true);
        continue;
      }
      this->addDebugFileName(*fi);
    }
  } else {
    this->_Enabled = false;
    return;
  }
  this->DebugLogAsXml = false;
  this->_OutStream.open(this->_LogFileName.c_str(), std::ios_base::out);
  this->beginNode(DEBUG_TOPLEVEL);
  this->close();
}

DebugStream::~DebugStream() {
  if (!this->_Enabled)
    return;
#if 0
    this->_OutStream.open(this->_LogFileName.c_str(),std::ios_base::app);
#endif
  this->endNode(DEBUG_TOPLEVEL);
  this->_OutStream.close();
}

bool DebugStream::recognizesDebugFileName(const string &fn) {
  if (!this->_Enabled)
    return false;
  if (this->debugAll())
    return true;
  if (this->_DebugFileNames.count(fn) > 0)
    return true;
  return false;
}
string DebugStream::allDebugFileNames() {
  if (!this->_Enabled)
    return "--none--";
  stringstream ss;
  set<string>::iterator fi;
  for (fi = this->_DebugFileNames.begin(); fi != this->_DebugFileNames.end(); fi++) {
    ss << "output debugging info for(" << *fi << ")" << std::endl;
  }
  return ss.str();
}

string DebugStream::nextPosition() {
  stringstream ss;
  ss.str("");
  ss << "p" << this->DebugPositionCounter << "p";
  this->DebugPositionCounter++;
  return ss.str();
}

DebugStream &DebugStream::beginNode(uint debugFlags) {
  if (this->DebugLogAsXml) {
    this->writeRaw((BF("<%s>\n") % debugFlagsAsNodeName(debugFlags)).str());
  } else {
    this->writeRaw((BF("%s%s%s\n") % DebugOpenLeft % debugFlagsAsNodeName(debugFlags) % DebugOpenRight).str());
  }
  return *this;
}

DebugStream &DebugStream::beginNode(uint debugFlags,
                                    const char *cPsourceFile,
                                    const char *cPfunctionName,
                                    uint lineNumber,
                                    uint column,
                                    const string &message) {
  const char *shortSourceFile = trimSourceFilePathName(cPsourceFile);
  if (shortSourceFile == NULL)
    shortSourceFile = "-begin-node-no-file-";
  if (this->DebugLogAsXml) {
    string stuff = (BF("<%s s=\"%s\" f=\"%s\" l=\"%d\">\n") % debugFlagsAsNodeName(debugFlags) % shortSourceFile % cPfunctionName % lineNumber).str();
    this->writeRaw(stuff);
    if (message != "")
      this->writeTextCr(message);
  } else {
    string stuff = (BF("%s%s {%s:%s:%d}%s") % DebugOpenLeft % debugFlagsAsNodeName(debugFlags) % cPfunctionName % shortSourceFile % lineNumber % DebugOpenRight).str();
    this->writeRaw(stuff);
    this->writeTextCr(message);
  }
  return *this;
}

DebugStream &DebugStream::endNode(uint flags) {
  if (this->DebugLogAsXml) {
    string stuff = (BF("</%s>\n") % debugFlagsAsNodeName(flags)).str();
    this->writeRaw(stuff);
  } else {
    string stuff = (BF("%s\n") % DebugClose).str();
    this->writeRaw(stuff);
  }
  return *this;
}

DebugStream &DebugStream::writeRaw(const string &data) {
  if (!this->_Enabled)
    return *this;
  if (this->_SuppressMessages)
    return *this;
  this->open();
  this->_OutStream << data;
  this->_OutStream.flush();
  this->close();
  return *this;
}

DebugStream &DebugStream::writeText(const string &data) {
  if (!this->_Enabled)
    return *this;
  if (this->_SuppressMessages)
    return *this;
  this->open();
  const char *cp;
  for (cl_index i=0; i<data.size(); ++i ) {
    cp = &data[i];
    if (this->DebugLogAsXml) {
      switch (*cp) {
      case '\0':
          this->_OutStream << "&NUL;";
          break;
      case '<':
        this->_OutStream << "&lt;";
        break;
      case '>':
        this->_OutStream << "&gt;";
        break;
      case '&':
        this->_OutStream << "&amp;";
        break;
      case '\'':
        this->_OutStream << "&apos;";
        break;
      case '"':
        this->_OutStream << "&quot;";
        break;
      default:
        this->_OutStream << *cp;
        break;
      }
    } else {
      this->_OutStream << *cp;
    }
  }
  //    this->_OutStream << data;
  this->_OutStream.flush();
  this->close();
  return *this;
}

DebugStream &DebugStream::writeTextCr(const string &msg) {
  this->writeText(msg);
  this->writeRaw("\n");
  return *this;
}

DebugStream &DebugStream::writeInt(uint i) {
  if (!this->_Enabled)
    return *this;
  if (this->_SuppressMessages)
    return *this;
  this->open();
  this->_OutStream << i;
  this->close();
  return *this;
}

DebugStream &DebugStream::writePtr(void *i) {
  if (!this->_Enabled)
    return *this;
  if (this->_SuppressMessages)
    return *this;
  this->open();
  this->_OutStream << i;
  this->close();
  return *this;
}

DebugStream &DebugStream::writeLn() {
  if (!this->_Enabled)
    return *this;
  if (this->_SuppressMessages)
    return *this;
  this->open();
  this->_OutStream << std::endl;
  this->close();
  return *this;
}

void DebugStream::open() {
#if 0 // don't open and close the debug stream all the time
	if ( !this->_Enabled ) return;
	if ( this->_OutStreamOpen ) return;
// #p r a g m a omp critical ( open )
    {
	this->_OutStream.open(this->_LogFileName.c_str(),std::ios_base::app);
	if ( this->_OutStream.fail() ) {
	    printf( "DEBUG LOG(())@%lX COULD NOT OPEN FILE(%s) FAILED!!!\n", (unsigned long)(this), this->_LogFileName.c_str());
	    abort();
	}
	this->_OutStreamOpen = true;
    }
#endif
}

void DebugStream::close() {
  if (!this->_Enabled)
    return;
#if 0
    this->_OutStream.close();
#endif
}

void DebugStream::finalClose() {
  if (!this->_Enabled)
    return;
  this->_OutStream.close();
}

/*!
 * Return true if this message will be logged and false if not
 * Pass the __FILENAME__ preprocessor string to test for source file by source file logging.
 */
bool DebugStream::isOn(const char *fileName, uint debugFlags) {
  if (!this->_Enabled)
    return false;
  if (debugFlags == DEBUG_SHOUT)
    return true;
  if (this->getSuppressMessages())
    return false;
  if (this->debugAll())
    return true;
  //    if ( this->debugLisp() && (debugFlags&DEBUG_LISP) ) return true;
  //    if ( this->debugScript() && (debugFlags&DEBUG_SCRIPT) ) return true;
  const char *name = fileName + strlen(fileName);
  while (name >= fileName && !((*name == '\\') || (*name == '/')))
    name--;
  name++;
  if (!this->recognizesDebugFileName(name))
    return false;
  return true;
}

void DebugStream::setSuppressMessages(bool s) {
  string stateName;
  if (s)
    stateName = "OFF";
  else
    stateName = "ON";
  this->writeRaw((BF("<DebugMessages state=\"%s\"/>\n") % stateName).str());
  this->_SuppressMessages = s;
}

char *internalPrintf(const Lisp_sp &lisp, const char *fmt, va_list arg_ptr) {
  char *outBuffer;
  int n;
  n = vasprintf(&outBuffer, fmt, arg_ptr);
  if (outBuffer == NULL) {
    SIMPLE_ERROR(BF("Could not allocate a large enough internalPrintf buffer"));
  }
  return outBuffer;
}

#define ARGS_af_wrongTypeKeyArg "(source-file lineno function narg value type)"
#define DECL_af_wrongTypeKeyArg ""
#define DOCS_af_wrongTypeKeyArg "wrongTypeKeyArg"
void af_wrongTypeKeyArg(const string &sourceFile, int lineno,
                        Symbol_sp function,
                        T_sp key, T_sp value, T_sp type) {
  stringstream message;
  message << "In ";
  if (function.nilp()) {
    message << "an anonymous function ~A";
  } else {
    message << "function ~A";
  }
  message << "the value of the argument ~S is ~&   ~S~&which is not of the expected type ~A";
  eval::funcall(_sym_signalSimpleError,
                cl::_sym_simpleTypeError,           //arg0
                _Nil<T_O>(),                  // arg1
                SimpleBaseString_O::make(message.str()), // arg2
                Cons_O::createList(function, key, value, type),
                kw::_sym_expected_type, type,
                kw::_sym_datum, value);
};

#define ARGS_af_wrongTypeOnlyArg "(source-file lineno function narg value type)"
#define DECL_af_wrongTypeOnlyArg ""
#define DOCS_af_wrongTypeOnlyArg "wrongTypeOnlyArg"
void af_wrongTypeOnlyArg(const string &sourceFile, int lineno, Symbol_sp function, T_sp value, T_sp type) {
  stringstream message;
  if (function.nilp()) {
    message << "In an anonymous function,";
    message << "the value of the only argument is~&  ~S~&which is ";
    message << "not of the expected type ~A";
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_simpleTypeError,           //arg0
                  _Nil<T_O>(),                  // arg1
                  SimpleBaseString_O::make(message.str()), // arg2
                  Cons_O::createList(value, type),
                  kw::_sym_expected_type, type,
                  kw::_sym_datum, value);
  } else {
    message << "In function ~A,";
    message << "the value of the only argument is~&  ~S~&which is ";
    message << "not of the expected type ~A";
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_simpleTypeError,           //arg0
                  _Nil<T_O>(),                  // arg1
                  SimpleBaseString_O::make(message.str()), // arg2
                  Cons_O::createList(function, value, type),
                  kw::_sym_expected_type, type,
                  kw::_sym_datum, value);
  }
  printf("%s:%d This should never be reached\n", __FILE__, __LINE__ );
  abort();
};

CL_DOCSTRING("functionWrongTypeArgument");
CL_DEFUN void core__function_wrong_type_argument(Symbol_sp function, T_sp value, T_sp type) {
  stringstream message;
  if (function.nilp()) {
    message << "In an anonymous function, "
               "the value of an argument is~&  ~S~&which is "
               "not of the expected type ~A";
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_simpleTypeError,           //arg0
                  _Nil<T_O>(),                  // arg1
                  SimpleBaseString_O::make(message.str()), // arg2
                  Cons_O::createList(value, type),
                  kw::_sym_expected_type, type,
                  kw::_sym_datum, value);
  } else {
    message << "In function ~A, "
               "the value of an argument is~&  ~S~&which is "
               "not of the expected type ~A";
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_simpleTypeError,           //arg0
                  _Nil<T_O>(),                  // arg1
                  SimpleBaseString_O::make(message.str()), // arg2
                  Cons_O::createList(function, value, type),
                  kw::_sym_expected_type, type,
                  kw::_sym_datum, value);
  }
};

CL_LAMBDA(source-file lineno function narg value type);
CL_DECLARE();
CL_DOCSTRING("wrongTypeArgument");
CL_DEFUN void core__wrong_type_argument(const string &sourceFile, int lineno, Symbol_sp function, T_sp value, T_sp type) {
  stringstream message;
  if (function.nilp()) {
    message << "In an anonymous function, "
               "the value of an argument is~&  ~S~&which is "
               "not of the expected type ~A";
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_simpleTypeError,           //arg0
                  _Nil<T_O>(),                  // arg1
                  SimpleBaseString_O::make(message.str()), // arg2
                  Cons_O::createList(value, type),
                  kw::_sym_expected_type, type,
                  kw::_sym_datum, value);
  } else {
    message << "In function ~A, "
               "the value of an argument is~&  ~S~&which is "
               "not of the expected type ~A";
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_simpleTypeError,           //arg0
                  _Nil<T_O>(),                  // arg1
                  SimpleBaseString_O::make(message.str()), // arg2
                  Cons_O::createList(function, value, type),
                  kw::_sym_expected_type, type,
                  kw::_sym_datum, value);
  }
};

CL_LAMBDA(source-file lineno function narg value type);
CL_DECLARE();
CL_DOCSTRING("wrongTypeNthArg");
CL_DEFUN void core__wrong_type_nth_arg(const string &sourceFile, int lineno, Symbol_sp function, int narg, T_sp value, T_sp type) {
  if (function.nilp()) {
    stringstream message;
    message << "In an anonymous function, "
               "the value of the ~:R argument is~&  ~S~&which is "
               "not of the expected type ~A";
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_simpleTypeError,           //arg0
                  _Nil<T_O>(),                  // arg1
                  SimpleBaseString_O::make(message.str()), // arg2
                  Cons_O::createList(make_fixnum(narg), value, type),
                  kw::_sym_expected_type, type,
                  kw::_sym_datum, value);
  } else {
    stringstream message;
    message << "In function ~A, "
               "the value of the ~:R argument is~&  ~S~&which is "
               "not of the expected type ~A";
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_simpleTypeError,           //arg0
                  _Nil<T_O>(),                  // arg1
                  SimpleBaseString_O::make(message.str()), // arg2
                  Cons_O::createList(function, make_fixnum(narg), value, type),
                  kw::_sym_expected_type, type,
                  kw::_sym_datum, value);
  }
};

CL_LAMBDA(source-file lineno function narg value type);
CL_DECLARE();
CL_DOCSTRING("wrongIndex");
CL_DEFUN void core__wrong_index(const string &sourceFile, int lineno, Symbol_sp function, T_sp array, int which, T_sp index, int nonincl_limit) {
  if (function.nilp()) {
    const char *message1 =
        "In an anonymous function, "
        "the ~*index into the object~% ~A.~%"
        "takes a value ~D out of the range ~A.";
    const char *message2 =
        "In an anonymous function, "
        "the ~:R index into the object~% ~A~%"
        "takes a value ~D out of the range ~A.";
    T_sp limit = Integer_O::create((gc::Fixnum)(nonincl_limit - 1));
    T_sp type = Cons_O::createList(cl::_sym_Integer_O, make_fixnum(0), limit);
    const char *msg = (which < 0) ? message1 : message2;
    SimpleBaseString_sp message = SimpleBaseString_O::make(msg);
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_simpleTypeError, //arg0
                  _Nil<T_O>(),        // arg1
                  message,            // arg2
                  Cons_O::createList(make_fixnum(which + 1), array, index, type),
                  kw::_sym_expected_type, type,
                  kw::_sym_datum, index);
  } else {
    const char *message1 =
        "In function ~A, "
        "the ~*index into the object~% ~A.~%"
        "takes a value ~D out of the range ~A.";
    const char *message2 =
        "In function ~A, "
        "the ~:R index into the object~% ~A~%"
        "takes a value ~D out of the range ~A.";
    T_sp limit = Integer_O::create((gc::Fixnum)(nonincl_limit - 1));
    T_sp type = Cons_O::createList(cl::_sym_Integer_O, make_fixnum(0), limit);
    const char *msg = (which < 0) ? message1 : message2;
    SimpleBaseString_sp message = SimpleBaseString_O::make(msg);
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_simpleTypeError, //arg0
                  _Nil<T_O>(),        // arg1
                  message,            // arg2
                  Cons_O::createList(function, make_fixnum(which + 1), array, index, type),
                  kw::_sym_expected_type, type,
                  kw::_sym_datum, index);
  }
};

//no need to call this from lisp
void core__reader_error_internal(const string &sourceFile, uint lineno,
                    String_sp fmt, List_sp fmtargs, T_sp stream) {
  ASSERT(cl__stringp(fmt));
  if (stream.nilp()) {
    eval::funcall(_sym_signalSimpleError,
                  core::_sym_simpleParseError,
                  _Nil<T_O>(), // not correctable
                  fmt,         // format control
                  fmtargs      // format args
                  );
  } else {
    eval::funcall(_sym_signalSimpleError,
                  core::_sym_simpleReaderError,
                  _Nil<T_O>(), // not correctable
                  fmt,         // format control
                  fmtargs,     // format args
                  kw::_sym_stream, stream);
  }
};

void FEerror(const string &fmt, int nargs, ...) {
  SimpleBaseString_sp sfmt = SimpleBaseString_O::make(fmt);
  va_list args;
  va_start(args, nargs);
  ql::list l;
  while (nargs) {
    T_sp arg = gctools::smart_ptr<T_O>((gc::Tagged)(va_arg(args, T_O *)));
    l << arg;
    --nargs;
  }
  eval::funcall(core::_sym_universalErrorHandler, _Nil<T_O>() // not correctable
                ,
                sfmt, l.cons());
  UNREACHABLE();
}

void FElibc_error(const char *msg, int nargs, ...) {
  T_sp error = SimpleBaseString_O::make(strerror(errno));
  SimpleBaseString_sp smsg = SimpleBaseString_O::make(msg);
  va_list args;
  va_start(args, nargs);
  List_sp l = clasp_grab_rest_args(args, nargs);
  clasp_va_end(args);
  FEerror("~?~%C library explanation: ~A.", 3,
          smsg.raw_(), l.raw_(),
          error.raw_());
}

void FEcannot_open(T_sp fileName) {
  cl__error(cl::_sym_fileError, Cons_O::createList(kw::_sym_pathname, fileName));
}

SYMBOL_EXPORT_SC_(CorePkg,argument_number_error);
SYMBOL_EXPORT_SC_(KeywordPkg,supplied);
SYMBOL_EXPORT_SC_(KeywordPkg,min);
SYMBOL_EXPORT_SC_(KeywordPkg,max);
void FEargument_number_error(T_sp supplied, T_sp min, T_sp max) {
  cl__error(core::_sym_argument_number_error,
           core::Cons_O::createList(kw::_sym_supplied, supplied,
                                    kw::_sym_min, min,
                                    kw::_sym_max, max));
}

T_sp CEerror(T_sp c, const char *err, int narg, ...) {
  clasp_va_list args;
  clasp_va_start(args, narg);
  T_sp result = eval::funcall(core::_sym_universalErrorHandler,
                              c,                  // correctable
                              SimpleBaseString_O::make(err), // continue format string
                              clasp_grab_rest_args(args, narg));
  clasp_va_end(args);
  return result;
}

void CEpackage_error(const char *fmt,
                     const char *continue_message,
                     T_sp package,
                     int nargs, ...) {
  clasp_va_list args;
  clasp_va_start(args, nargs);
  List_sp fmtargs = clasp_grab_rest_args(args, nargs);
  clasp_va_end(args);
  if (fmtargs.nilp()) fmtargs = Cons_O::create(package,_Nil<T_O>());
  eval::funcall(core::_sym_signalSimpleError,
                core::_sym_simplePackageError,
                SimpleBaseString_O::make(continue_message),
                SimpleBaseString_O::make(std::string(fmt)),
                fmtargs,
                kw::_sym_package,
                package);
}

void FEpackage_error(const char *fmt,
                     T_sp package,
                     int nargs, ...) {
  clasp_va_list args;
  clasp_va_start(args, nargs);
  List_sp fmtargs = clasp_grab_rest_args(args, nargs);
  clasp_va_end(args);
  if (fmtargs.nilp()) fmtargs = Cons_O::create(package,_Nil<T_O>());
  eval::funcall(core::_sym_signalSimpleError,
                core::_sym_simplePackageError,
                _Nil<T_O>(),
                SimpleBaseString_O::make(std::string(fmt)),
                fmtargs,
                kw::_sym_package,
                package);
}

void Warn(T_sp datum, List_sp arguments) {
  eval::applyLastArgsPLUSFirst(cl::_sym_warn, arguments, datum);
}

void clasp_internal_error(const char *msg) {
  printf("%s:%d %s\n", __FILE__, __LINE__, msg);
  SIMPLE_ERROR(BF("Internal error: %s\n") % msg);
}

SYMBOL_EXPORT_SC_(CorePkg, signalSimpleError);
SYMBOL_EXPORT_SC_(CorePkg, wrongTypeNthArg);
SYMBOL_EXPORT_SC_(CorePkg, wrongIndex);

};
