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
#define DEBUG_LEVEL_FULL
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
#include <clasp/core/str.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/object.h>
#include <clasp/core/wrappers.h>

#ifdef WIN32
#define vsnprintf _vsnprintf
#endif

#define MAX_DEBUG_CALL_DEPTH 200

namespace core {

/*! These are here just so that the clang compiler
      will assign the __attribute__((weak)) to the vtable of each of these classes
    */
void CatchThrow::keyFunctionForVtable(){};
void ReturnFrom::keyFunctionForVtable(){};
void LexicalGo::keyFunctionForVtable(){};
void DynamicGo::keyFunctionForVtable(){};
void Unwind::keyFunctionForVtable(){};

#if 0
    GC_RESULT ReturnFrom::onHeapScanGCRoots(GC_SCAN_ARGS_PROTOTYPE)
#ifdef USE_MPS
            {
                MPS_SCAN_BEGIN(GC_SCAN_STATE) {
                    SMART_PTR_FIX(this->_ReturnedObject);
                } MPS_SCAN_END(GC_SCAN_STATE);
                return GC_RES_OK;
            };
#else
            {
                return GC_RES_OK;
            };
#endif
#endif

TooFewArgumentsError::TooFewArgumentsError(int given, int required) : givenNumberOfArguments(given), requiredNumberOfArguments(required) {
  printf("%s:%d Constructed TooFewArgumentsError given %d required %d\n", __FILE__, __LINE__, given, required);
};
TooManyArgumentsError::TooManyArgumentsError(int given, int required) : givenNumberOfArguments(given), requiredNumberOfArguments(required){};

void throwTooFewArgumentsError(size_t given, size_t required) {
  SIMPLE_ERROR(BF("Too few arguments given %d required %d") % given % required);
  //        throw(TooFewArgumentsError(given,required));
}

void throwTooManyArgumentsError(size_t given, size_t required) {
  SIMPLE_ERROR(BF("Too many arguments error given: %d required: %d") % given % required);
  //        throw(TooManyArgumentsError(given,required));
}

void throwUnrecognizedKeywordArgumentError(T_sp kw) {
  SIMPLE_ERROR(BF("Unrecognized keyword argument error: %s") % _rep_(kw));
  //        throw(UnrecognizedKeywordArgumentError(kw));
}

void wrongNumberOfArguments(std::size_t givenNumberOfArguments, std::size_t requiredNumberOfArguments) {
  if (givenNumberOfArguments < requiredNumberOfArguments)
    throwTooFewArgumentsError(givenNumberOfArguments, requiredNumberOfArguments);
  else
    throwTooManyArgumentsError(givenNumberOfArguments, requiredNumberOfArguments);
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

#define ARGS_af_signalSimpleError "(base-condition continue-message format-control format-args &rest args)"
#define DECL_af_signalSimpleError ""
#define DOCS_af_signalSimpleError "signalSimpleError"
T_sp af_signalSimpleError(T_sp baseCondition, T_sp continueMessage, T_sp formatControl, T_sp formatArgs, T_sp args) {
  printf("%s:%d af_signalSimpleError  caught because signal-simple-error is not installed yet\n", __FILE__, __LINE__);
  printf("%s\n", _rep_(baseCondition).c_str());
  af_format(_lisp->_true(), formatControl, formatArgs);
  dbg_hook("af_signalSimpleError");
  af_invokeInternalDebugger(_Nil<core::T_O>());
  printf("%s:%d  Continuing...\n", __FILE__, __LINE__);
  return _Nil<T_O>();
};

void _trapThrow(const string &fileName, uint line, const string &msg) {
  /* do nothing */
}

HardError::HardError(const char *sourceFile, const char *functionName, uint lineNumber, const boost::format &fmt) {
  TRY_BOOST_FORMAT_STRING(fmt, fmt_str);
  this->_Message = (BF("File: %s Function:%s LineNumber:%d\n%s") % sourceFile % functionName % lineNumber % fmt_str).str();
}

HardError::HardError(const char *sourceFile, const char *functionName, uint lineNumber, const string &msg) {
  this->_Message = (BF("File: %s Function:%s LineNumber:%d\n%s") % sourceFile % functionName % lineNumber % msg).str();
}

string HardError::message() {
  return this->_Message;
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
  boost::format fileName("_candoDebug%05d.log");
  fileName % this->DebugLogProcessRank;
  this->_LogFileName = fileName.str();
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
  for (const char *cp = data.c_str(); *cp; cp++) {
    if (this->DebugLogAsXml) {
      switch (*cp) {
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
  _G();
  char *outBuffer;
  int n;
  n = vasprintf(&outBuffer, fmt, arg_ptr);
  if (outBuffer == NULL) {
    SIMPLE_ERROR(BF("Could not allocate a large enough internalPrintf buffer"));
  }
  return outBuffer;
}

void _stackTraceEnter_WriteEntryToLog(int entryIndex) { // Dont use --> _G();
  IMPLEMENT_ME();
}

void _stackTraceEnter(uint debugFlags) {
  IMPLEMENT_ME();
}

void _stackTraceLineNumberAndColumnUpdate(uint ln, uint col) {
}

void _stackTraceExit() {
  IMPLEMENT_ME();
}

void _stackTraceDump() {
  IMPLEMENT_ME();
}

#ifdef USEBOOSTPYTHON

#endif

#define ARGS_af_wrongTypeKeyArg "(source-file lineno function narg value type)"
#define DECL_af_wrongTypeKeyArg ""
#define DOCS_af_wrongTypeKeyArg "wrongTypeKeyArg"
void af_wrongTypeKeyArg(const string &sourceFile, int lineno,
                        Symbol_sp function,
                        T_sp key, T_sp value, T_sp type) {
  _G();
  stringstream message;
  message << "In ";
  if (function.nilp()) {
    message << "an anonymous function ~A";
  } else {
    message << "function ~A";
  }
  message << "the value of the argument ~S is ~&   ~S~&which is not of the expected type ~A";
  eval::funcall(_sym_signalSimpleError,
                cl::_sym_typeError,           //arg0
                _Nil<T_O>(),                  // arg1
                Str_O::create(message.str()), // arg2
                Cons_O::createList(function, key, value, type),
                kw::_sym_expectedType, type,
                kw::_sym_datum, value);
};

#define ARGS_af_wrongTypeOnlyArg "(source-file lineno function narg value type)"
#define DECL_af_wrongTypeOnlyArg ""
#define DOCS_af_wrongTypeOnlyArg "wrongTypeOnlyArg"
void af_wrongTypeOnlyArg(const string &sourceFile, int lineno, Symbol_sp function, T_sp value, T_sp type) {
  _G();
  stringstream message;
  if (function.nilp()) {
    message << "In an anonymous function,";
    message << "the value of the only argument is~&  ~S~&which is ";
    message << "not of the expected type ~A";
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_typeError,           //arg0
                  _Nil<T_O>(),                  // arg1
                  Str_O::create(message.str()), // arg2
                  Cons_O::createList(value, type),
                  kw::_sym_expectedType, type,
                  kw::_sym_datum, value);
  } else {
    message << "In function ~A,";
    message << "the value of the only argument is~&  ~S~&which is ";
    message << "not of the expected type ~A";
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_typeError,           //arg0
                  _Nil<T_O>(),                  // arg1
                  Str_O::create(message.str()), // arg2
                  Cons_O::createList(function, value, type),
                  kw::_sym_expectedType, type,
                  kw::_sym_datum, value);
  }
};

#define ARGS_af_wrongTypeArgument "(source-file lineno function narg value type)"
#define DECL_af_wrongTypeArgument ""
#define DOCS_af_wrongTypeArgument "wrongTypeArgument"
void af_wrongTypeArgument(const string &sourceFile, int lineno, Symbol_sp function, T_sp value, T_sp type) {
  _G();
  stringstream message;
  if (function.nilp()) {
    message << "In an anonymous function, "
               "the value of an argument is~&  ~S~&which is "
               "not of the expected type ~A";
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_typeError,           //arg0
                  _Nil<T_O>(),                  // arg1
                  Str_O::create(message.str()), // arg2
                  Cons_O::createList(value, type),
                  kw::_sym_expectedType, type,
                  kw::_sym_datum, value);
  } else {
    message << "In function ~A, "
               "the value of an argument is~&  ~S~&which is "
               "not of the expected type ~A";
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_typeError,           //arg0
                  _Nil<T_O>(),                  // arg1
                  Str_O::create(message.str()), // arg2
                  Cons_O::createList(function, value, type),
                  kw::_sym_expectedType, type,
                  kw::_sym_datum, value);
  }
};

#define ARGS_af_wrongTypeNthArg "(source-file lineno function narg value type)"
#define DECL_af_wrongTypeNthArg ""
#define DOCS_af_wrongTypeNthArg "wrongTypeNthArg"
void af_wrongTypeNthArg(const string &sourceFile, int lineno, Symbol_sp function, int narg, T_sp value, T_sp type) {
  if (function.nilp()) {
    stringstream message;
    message << "In an anonymous function, "
               "the value of the ~:R argument is~&  ~S~&which is "
               "not of the expected type ~A";
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_typeError,           //arg0
                  _Nil<T_O>(),                  // arg1
                  Str_O::create(message.str()), // arg2
                  Cons_O::createList(make_fixnum(narg), value, type),
                  kw::_sym_expectedType, type,
                  kw::_sym_datum, value);
  } else {
    stringstream message;
    message << "In function ~A, "
               "the value of the ~:R argument is~&  ~S~&which is "
               "not of the expected type ~A";
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_typeError,           //arg0
                  _Nil<T_O>(),                  // arg1
                  Str_O::create(message.str()), // arg2
                  Cons_O::createList(function, make_fixnum(narg), value, type),
                  kw::_sym_expectedType, type,
                  kw::_sym_datum, value);
  }
};

#define ARGS_af_wrongIndex "(source-file lineno function narg value type)"
#define DECL_af_wrongIndex ""
#define DOCS_af_wrongIndex "wrongIndex"
void af_wrongIndex(const string &sourceFile, int lineno, Symbol_sp function, T_sp array, int which, T_sp index, int nonincl_limit) {
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
    Str_sp message = Str_O::create(msg);
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_typeError, //arg0
                  _Nil<T_O>(),        // arg1
                  message,            // arg2
                  Cons_O::createList(make_fixnum(which + 1), array, index, type),
                  kw::_sym_expectedType, type,
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
    Str_sp message = Str_O::create(msg);
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_typeError, //arg0
                  _Nil<T_O>(),        // arg1
                  message,            // arg2
                  Cons_O::createList(function, make_fixnum(which + 1), array, index, type),
                  kw::_sym_expectedType, type,
                  kw::_sym_datum, index);
  }
};

#define ARGS_af_readerError "(sourceFileName lineno functionName fmt fmtargs stream)"
#define DECL_af_readerError ""
#define DOCS_af_readerError "readerError"
void af_readerError(const string &sourceFile, uint lineno, Symbol_sp function,
                    Str_sp fmt, List_sp fmtargs, T_sp stream) {
  _G();
  if (stream.nilp()) {
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_parseError,
                  _Nil<T_O>(), // not correctable
                  fmt,         // format control
                  fmtargs      // format args
                  );
  } else {
    eval::funcall(_sym_signalSimpleError,
                  cl::_sym_readerError,
                  _Nil<T_O>(), // not correctable
                  fmt,         // format control
                  fmtargs,     // format args
                  kw::_sym_stream, stream);
  }
};

void assert_type_integer(int index, T_sp p) {
  if (!gc::IsA<Integer_sp>(p)) {
    QERROR_WRONG_TYPE_NTH_ARG(index, p, cl::_sym_Integer_O);
  }
}

void FEerror(const string &fmt, int nargs, ...) {
  Str_sp sfmt = Str_O::create(fmt);
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
  T_sp error = Str_O::create(strerror(errno));
  Str_sp smsg = Str_O::create(msg);
  va_list args;
  va_start(args, nargs);
  List_sp l = clasp_grab_rest_args(args, nargs);
  clasp_va_end(args);
  FEerror("~?~%C library explanation: ~A.", 3,
          smsg.raw_(), l.raw_(),
          error.raw_());
}

void FEcannot_open(T_sp fileName) {
  cl_error(cl::_sym_fileError, Cons_O::createList(kw::_sym_pathname, fileName));
}

SYMBOL_EXPORT_SC_(CorePkg,argument_number_error);
SYMBOL_EXPORT_SC_(KeywordPkg,supplied);
SYMBOL_EXPORT_SC_(KeywordPkg,min);
SYMBOL_EXPORT_SC_(KeywordPkg,max);
void FEargument_number_error(T_sp supplied, T_sp min, T_sp max) {
  cl_error(core::_sym_argument_number_error,
           core::Cons_O::createList(kw::_sym_supplied, supplied,
                                    kw::_sym_min, min,
                                    kw::_sym_max, max));
}

T_sp CEerror(T_sp c, const char *err, int narg, ...) {
  clasp_va_list args;
  clasp_va_start(args, narg);
  T_sp result = eval::funcall(core::_sym_universalErrorHandler,
                              c,                  // correctable
                              Str_O::create(err), // continue format string
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
  if (fmtargs.nilp())
    fmtargs = Cons_O::create(package);
  eval::funcall(core::_sym_signalSimpleError,
                cl::_sym_package_error,
                Str_O::create(continue_message),
                Str_O::create(std::string(fmt)),
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
  if (fmtargs.nilp())
    fmtargs = Cons_O::create(package);
  eval::funcall(core::_sym_signalSimpleError,
                cl::_sym_package_error,
                _Nil<T_O>(),
                Str_O::create(std::string(fmt)),
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

void initialize_exceptions() {
  SYMBOL_EXPORT_SC_(CorePkg, signalSimpleError);
  Defun(signalSimpleError);
  SYMBOL_EXPORT_SC_(CorePkg, wrongTypeNthArg);
  Defun(wrongTypeNthArg)
      SYMBOL_EXPORT_SC_(CorePkg, wrongIndex);
  Defun(wrongIndex)
};
};
