/*
    File: debugger.cc
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

#include <csignal>
#include <execinfo.h>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/conditions.h>
#include <clasp/core/arguments.h>
#include <clasp/core/myReadLine.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/environment.h>
#include <clasp/core/debugger.h>
#include <clasp/core/primitives.h>
#include <clasp/core/vectorObjects.h>
#include <clasp/core/write_ugly.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/wrappers.h>

namespace core {

void start_debugger() {
  _G();
  LispDebugger dbg(_Nil<T_O>());
  dbg.invoke();
}

LispDebugger::LispDebugger(T_sp condition) : _CanContinue(false), _Condition(condition) {
  _G();
  _lisp->incrementDebuggerLevel();
  af_gotoIhsTop();
}

LispDebugger::LispDebugger() : _CanContinue(true) {
  _G();
  this->_Condition = _Nil<T_O>();
  _lisp->incrementDebuggerLevel();
  af_gotoIhsTop();
}

void LispDebugger::printExpression() {
  _G();
  InvocationHistoryFrameIterator_sp frame = this->currentFrame();
  stringstream ss;
  ss << frame->frame()->asString();
  _lisp->print(BF("%s\n") % ss.str());
}

InvocationHistoryFrameIterator_sp LispDebugger::currentFrame() const {
  InvocationHistoryFrameIterator_sp frame = core_getInvocationHistoryFrame(af_ihsCurrentFrame());
  if (frame->isValid())
    return frame;
  THROW_HARD_ERROR(BF("%s:%d Could not get frame") % __FILE__ % __LINE__);
}

T_sp LispDebugger::invoke() {
  _G();
  //	DebuggerIHF debuggerStack(_lisp->invocationHistoryStack(),_Nil<ActivationFrame_O>());
  if (this->_Condition.notnilp()) {
    _lisp->print(BF("Debugger entered with condition: %s") % _rep_(this->_Condition));
  }
  this->printExpression();
  _lisp->print(BF("The following restarts are available:"));
  _lisp->print(BF("ABORT      a    Abort to REPL"));
  while (1) {
    string line;
    stringstream sprompt;
    sprompt << "Frame-" << this->currentFrame()->index() << "-";
    sprompt << "Dbg";
    if (af_ihsEnv(af_ihsCurrentFrame()).notnilp()) {
      sprompt << "(+ENV)";
    }
    sprompt << "[" << _lisp->debuggerLevel() << "]>";
    bool end_of_transmission(false);
    line = myReadLine(sprompt.str(), end_of_transmission);
    if (end_of_transmission)
      throw core::ExitProgram(0);
    char cmd;
    if (line[0] == ':') {
      cmd = line[1];
    } else
      cmd = 'e';

    switch (cmd) {
    case '?':
    case 'h': {
      _lisp->print(BF(":?      - help"));
      _lisp->print(BF(":h      - help"));
      _lisp->print(BF("sexp - evaluate sexp"));
      _lisp->print(BF(":c sexp - continue - return values of evaluating sexp"));
      _lisp->print(BF(":v      - list local environment"));
      _lisp->print(BF(":x      - print current expression"));
      _lisp->print(BF(":e      - evaluate an expression with interpreter"));
      _lisp->print(BF(":b      - print backtrace"));
      _lisp->print(BF(":p      - goto previous frame"));
      _lisp->print(BF(":n      - goto next frame"));
      _lisp->print(BF(":D      - dissasemble current function"));
      _lisp->print(BF(":a      - abort and return to top repl"));
      _lisp->print(BF(":l      - invoke debugger by calling core::dbg_hook (set break point in gdb"));
      _lisp->print(BF(":g ##   - jump to frame ##"));
      break;
    }
    case 'l':
      dbg_hook("invoked from debugger");
      break;
    case 'g': {
      int is;
      for (is = 1; is < line.size(); is++) {
        if (line[is] >= '0' && line[is] <= '9')
          break;
      }
      if (is < line.size()) {
        string sexp = line.substr(is, 99999);
        int frameIdx = atoi(sexp.c_str());
        if (frameIdx < 0)
          frameIdx = 0;
        if (frameIdx > af_ihsTop()) {
          frameIdx = af_ihsTop();
        }
        _lisp->print(BF("Switching to frame: %d") % frameIdx);
        af_setIhsCurrentFrame(frameIdx);
        this->printExpression();
      } else {
        _lisp->print(BF("You must provide a frame number\n"));
      }
      break;
    }
    case 'p':
      af_gotoIhsPrev();
      this->printExpression();
      break;
    case 'n':
      af_gotoIhsNext();
      this->printExpression();
      break;
    case 'D': {
      Function_sp func = af_ihsFun(af_ihsCurrentFrame());
      _lisp->print(BF("Current function: %s\n") % _rep_(func));
      eval::funcall(cl::_sym_disassemble, func);
      break;
    }
    case 'b': {
      af_ihsBacktrace(_lisp->_true(), _Nil<T_O>());
      break;
    }
    case 'x': {
      this->printExpression();
      break;
    }
    case 'v': {
      this->printExpression();
      T_sp env = af_ihsEnv(af_ihsCurrentFrame());
      _lisp->print(BF("activationFrame->%p    .nilp()->%d  .nilp()->%d") % env.raw_() % env.nilp() % env.nilp());
      if (env.notnilp()) {
        _lisp->print(BF("%s") % gc::As<Environment_sp>(env)->environmentStackAsString());
      } else {
        _lisp->print(BF("-- Only global environment available --"));
      }
      break;
    }
    case 'a': {
      throw(DebuggerSaysAbortToRepl());
    }
    case 'c': {
      if (this->_CanContinue) {
        if (line.size() < 3) {
          return _Nil<T_O>();
        }
        string sexp = line.substr(3, 99999);
        //		    ControlSingleStep singleStep(false);
        T_mv result;
        T_sp env = af_ihsEnv(af_ihsCurrentFrame());
        //		    DebuggerIHF dbgFrame(_lisp->invocationHistoryStack(),Environment_O::clasp_getActivationFrame(env));
        result = _lisp->readEvalPrintString(sexp, env, true);
        if (!result) {
          result = Values(_Nil<T_O>());
        }
        _lisp->print(BF("Continuing with result: %s") % _rep_(result));
        return result;
        //		    throw(DebuggerSaysContinue(result));
      }
      _lisp->print(BF("You cannot resume after condition thrown"));
      break;
    };
    case 'e': {
      string sexp = line.substr(0, 99999);
      //		ControlSingleStep singleStep(false);
      T_sp env = af_ihsEnv(af_ihsCurrentFrame());
      //		DebuggerIHF dbgFrame(_lisp->invocationHistoryStack(),Environment_O::clasp_getActivationFrame(env));
      try {
        _lisp->readEvalPrintString(sexp, env, true);
      } catch (DebuggerSaysAbortToRepl &err) {
        // nothing
      }
      break;
    }
    case 'i': {
      string sexp = line.substr(2, 99999);
      //		ControlSingleStep singleStep(false);
      T_sp env = af_ihsEnv(af_ihsCurrentFrame());
      //		DebuggerIHF dbgFrame(_lisp->invocationHistoryStack(),Environment_O::clasp_getActivationFrame(env));
      try {
        DynamicScopeManager scope(comp::_sym_STARimplicit_compile_hookSTAR, comp::_sym_implicit_compile_hook_default->symbolFunction());
        _lisp->readEvalPrintString(sexp, env, true);
      } catch (DebuggerSaysAbortToRepl &err) {
        // nothing
      }
      break;
    }
    default: {
      _lisp->print(BF("Unknown command[%c] - try '?'") % cmd);
    }
    }
  }
}

#define ARGS_core_lowLevelBacktrace "()"
#define DECL_core_lowLevelBacktrace ""
#define DOCS_core_lowLevelBacktrace "lowLevelBacktrace"
void core_lowLevelBacktrace() {
  InvocationHistoryStack &ihs = _lisp->invocationHistoryStack();
  InvocationHistoryFrame *top = ihs.top();
  if (top == NULL) {
    printf("Empty InvocationHistoryStack\n");
    return;
  }
  printf("From bottom to top invocation-history-stack frames = %d\n", top->_Index + 1);
  for (InvocationHistoryFrame *cur = top; cur != NULL; cur = cur->_Previous) {
    string name = "-no-name-";
    gctools::tagged_pointer<Closure> closure = cur->closure;
    if (!closure) {
      name = "-NO-CLOSURE-";
    } else {
      if (closure->name.notnilp()) {
        try {
          name = _rep_(closure->name);
        } catch (...) {
          name = "-BAD-NAME-";
        }
      }
    }
    /*Nilable?*/ T_sp sfi = core_sourceFileInfo(make_fixnum(closure->sourceFileInfoHandle()));
    string sourceName = "cannot-determine";
    if (sfi.notnilp()) {
      sourceName = gc::As<SourceFileInfo_sp>(sfi)->fileName();
    }
    printf("_Index: %4d  Frame@%p(previous=%p)  closure@%p  closure->name[%40s]  line: %3d  file: %s\n", cur->_Index, cur, cur->_Previous, closure, name.c_str(), closure->lineNumber(), sourceName.c_str());
  }
  printf("----Done\n");
}

#define ARGS_core_clibBacktrace "(depth)"
#define DECL_core_clibBacktrace ""
#define DOCS_core_clibBacktrace "backtrace"
void core_clibBacktrace(int depth) {
  _G();
// Play with Unix backtrace(3)
#define BACKTRACE_SIZE 1024
  printf("Entered core_clibBacktrace - symbol: %s\n", _rep_(INTERN_(core, theClibBacktraceFunctionSymbol)).c_str());
  void *buffer[BACKTRACE_SIZE];
  char *funcname = (char *)malloc(1024);
  size_t funcnamesize = 1024;
  int nptrs;
  nptrs = backtrace(buffer, BACKTRACE_SIZE);
  char **strings = backtrace_symbols(buffer, nptrs);
  if (strings == NULL) {
    printf("No backtrace available\n");
    return;
  } else {
    for (int i = 0; i < nptrs; ++i) {
      if (i >= depth)
        break;
      std::string front = std::string(strings[i], 57);
      char *fnName = &strings[i][59];
      char *fnCur = fnName;
      int len = 0;
      for (; *fnCur; ++fnCur) {
        if (*fnCur == ' ')
          break;
        ++len;
      }
      int status;
      fnName[len] = '\0';
      char *rest = &fnName[len + 1];
      char *ret = abi::__cxa_demangle(fnName, funcname, &funcnamesize, &status);
      if (status == 0) {
        funcname = ret; // use possibly realloc()-ed string
        printf("  %s %s %s\n", front.c_str(), funcname, rest);
      } else {
        // demangling failed. Output function name as a C function with
        // no arguments.
        printf("  %s\n", strings[i]);
      }
    }
  }
  if (strings)
    free(strings);
  if (funcname)
    free(funcname);
};

#define ARGS_af_framePointers "()"
#define DECL_af_framePointers ""
#define DOCS_af_framePointers "framePointers"
void af_framePointers() {
  void *fp = __builtin_frame_address(0); // Constant integer only
  if (fp != NULL)
    printf("Frame pointer --> %p\n", fp);
};
};

extern "C" {

namespace core {
#define ARGS_af_gotoIhsTop "()"
#define DECL_af_gotoIhsTop ""
#define DOCS_af_gotoIhsTop "gotoIhsTop"
void af_gotoIhsTop() {
  _G();
  _sym_STARihsCurrentSTAR->setf_symbolValue(make_fixnum(af_ihsTop()));
};

#define ARGS_af_gotoIhsPrev "()"
#define DECL_af_gotoIhsPrev ""
#define DOCS_af_gotoIhsPrev "gotoIhsPrev"
void af_gotoIhsPrev() {
  _G();
  int ihsCur = af_ihsCurrentFrame();
  _sym_STARihsCurrentSTAR->setf_symbolValue(make_fixnum(af_ihsPrev(ihsCur)));
};

#define ARGS_af_gotoIhsNext "()"
#define DECL_af_gotoIhsNext ""
#define DOCS_af_gotoIhsNext "gotoIhsNext"
void af_gotoIhsNext() {
  _G();
  int ihsCur = af_ihsCurrentFrame();
  _sym_STARihsCurrentSTAR->setf_symbolValue(make_fixnum(af_ihsNext(ihsCur)));
};

#define ARGS_af_gotoIhsFrame "(frame-index)"
#define DECL_af_gotoIhsFrame ""
#define DOCS_af_gotoIhsFrame "gotoIhsFrame"
void af_gotoIhsFrame(int frame_index) {
  _G();
  if (frame_index < 0)
    frame_index = 0;
  if (frame_index >= af_ihsTop())
    frame_index = af_ihsTop() - 1;
  int ihsCur = frame_index;
  _sym_STARihsCurrentSTAR->setf_symbolValue(make_fixnum(ihsCur));
};

#define ARGS_af_printCurrentIhsFrame "()"
#define DECL_af_printCurrentIhsFrame ""
#define DOCS_af_printCurrentIhsFrame "printCurrentIhsFrame"
void af_printCurrentIhsFrame() {
  _G();
  int ihsCur = af_ihsCurrentFrame();
  Function_sp fun = af_ihsFun(ihsCur);
  printf("Frame[%d] %s\n", ihsCur, _rep_(fun).c_str());
};

#define ARGS_af_printCurrentIhsFrameEnvironment "()"
#define DECL_af_printCurrentIhsFrameEnvironment ""
#define DOCS_af_printCurrentIhsFrameEnvironment "printCurrentIhsFrameEnvironment"
void af_printCurrentIhsFrameEnvironment() {
  T_sp args = af_ihsArguments(af_ihsCurrentFrame());
  if (args.notnilp()) {
    VectorObjects_sp vargs = gc::As<VectorObjects_sp>(args);
    for (int i = 0; i < cl_length(vargs); ++i) {
      _lisp->print(BF("arg%s --> %s") % i % _rep_(vargs->elt(i)));
    }
  } else {
    _lisp->print(BF("Args not available"));
  }
  T_sp env = af_ihsEnv(af_ihsCurrentFrame());
  if (env.notnilp()) {
    printf("%s\n", gc::As<Environment_sp>(env)->environmentStackAsString().c_str());
  } else {
    printf("-- Only global environment available --\n");
  }
}

#define ARGS_af_evalPrint "(arg)"
#define DECL_af_evalPrint ""
#define DOCS_af_evalPrint "evalPrint"
void af_evalPrint(const string &expr) {
  _G();
  printf("If this locks up then there was an error in the evaluation\n");
  printf("Figure out how to make debugger.cc>>af_evalPrint always return\n");
  int ihsCur = af_ihsCurrentFrame();
  T_sp env = af_ihsEnv(ihsCur);
  _lisp->readEvalPrintString(expr, env, true);
};

void dbg_lowLevelDescribe(T_sp obj) {
  if (obj.valistp()) {
    // Convert the T_sp object into a VaList_sp object
    VaList_sp vl = VaList_sp((gc::Tagged)obj.raw_());
    printf("Original va_list at: %p\n", &((VaList_S *)gc::untag_valist(reinterpret_cast<VaList_S *>(obj.raw_())))->_Args);
    // Create a copy of the VaList_S with a va_copy of the va_list
    VaList_S vlcopy_s(*vl);
    VaList_sp vlcopy(&vlcopy_s);
    printf("VaList_sp\n");
    for (size_t i(0); i < LCC_VA_LIST_NUMBER_OF_ARGUMENTS(vlcopy); ++i) {
      printf("entry %3d --> %s\n", i, _rep_(LCC_NEXT_ARG(vlcopy, i)).c_str());
    }
  } else if (obj.fixnump()) {
    printf("fixnum_tag: %ld\n", obj.unsafe_fixnum());
  } else if (obj.single_floatp()) {
    printf("single-float: %f\n", obj.unsafe_single_float());
  } else if (obj.characterp()) {
    printf("character: %d #\\%c\n", obj.unsafe_character(), obj.unsafe_character());
  } else if (obj.generalp()) {
    printf("other_tag: %p  typeid: %s\n", &*obj, typeid(obj).name());
    printf("More info:\n");
    printf("%s\n", _rep_(obj).c_str());
  } else if (obj.consp()) {
    printf("cons_tag: %p  typeid: %s\n", &*obj, typeid(obj).name());
    printf("List:  \n");
    for (auto c : coerce_to_list(obj)) {
      printf("@%p > car@%p  cdr@%p : %s\n", c.raw_(), oCar(c).raw_(), oCdr(c).raw_(), _rep_(oCar(c)).c_str());
    }
    return;
  } else {
    printf("lowLevelDescribe handle: %p\n", obj.raw_());
  }
  fflush(stdout);
}

void dbg_mv_lowLevelDescribe(T_mv mv_obj) {
  gc::Vec0<core::T_sp> values;
  mv_obj.saveToVec0(values);
  for (int i(0), iEnd(values.size()); i < iEnd; ++i) {
    printf("Multiple value#%d\n", i);
    dbg_lowLevelDescribe(values[i]);
  }
  fflush(stdout);
}

void dbg_describe_tagged_T_Optr(T_O *p) {
  gctools::headerDescribe(p);
  T_sp obj((gctools::Tagged) reinterpret_cast<T_O *>(p));
  dbg_lowLevelDescribe(obj);
}

void dbg_describe_tagged_T_Optr_header(T_O *p) {
  gctools::headerDescribe(p);
}

extern void dbg_describe(T_sp obj);
void dbg_describe(T_sp obj) {
  DynamicScopeManager(_sym_STARenablePrintPrettySTAR, _Nil<T_O>());
  stringstream ss;
  printf("dbg_describe object class--> %s\n", _rep_(obj->__class()->className()).c_str());
  ss << _rep_(obj);
  printf("dbg_describe: %s\n", ss.str().c_str());
  fflush(stdout);
}

void dbg_describe_cons(Cons_sp obj) {
  DynamicScopeManager(_sym_STARenablePrintPrettySTAR, _Nil<T_O>());
  stringstream ss;
  printf("dbg_describe object class--> %s\n", _rep_(obj->__class()->className()).c_str());
  ss << _rep_(obj);
  printf("dbg_describe: %s\n", ss.str().c_str());
}

void dbg_describe_symbol(Symbol_sp obj) {
  DynamicScopeManager(_sym_STARenablePrintPrettySTAR, _Nil<T_O>());
  stringstream ss;
  printf("dbg_describe object class--> %s\n", _rep_(obj->__class()->className()).c_str());
  ss << _rep_(obj);
  printf("dbg_describe: %s\n", ss.str().c_str());
}

void dbg_describeActivationFrame(ActivationFrame_sp obj) {
  DynamicScopeManager(_sym_STARenablePrintPrettySTAR, _Nil<T_O>());
  stringstream ss;
  printf("dbg_describe ActivationFrame class--> %s\n", _rep_(obj->__class()->className()).c_str());
  ss << _rep_(obj);
  printf("dbg_describe: %s\n", ss.str().c_str());
}

void dbg_describeTPtr(uintptr_t raw) {
  if (raw == 0) {
    printf("dbg_describe: NULL\n");
    return;
  }
  T_sp obj = gctools::smart_ptr<T_O>(raw);
  printf("dbg_describeTPtr Raw pointer value: %p\n", obj.raw_());
  DynamicScopeManager(_sym_STARenablePrintPrettySTAR, _Nil<T_O>());
  stringstream ss;
  printf("dbg_describe object class--> %s\n", _rep_(obj->__class()->className()).c_str());
  ss << _rep_(obj);
  printf("dbg_describe: %s\n", ss.str().c_str());
  fflush(stdout);
}

void dbg_printTPtr(uintptr_t raw, bool print_pretty) {
  core::T_sp sout = cl::_sym_STARstandard_outputSTAR->symbolValue();
  T_sp obj = gctools::smart_ptr<T_O>((gc::Tagged)raw);
  clasp_write_string((BF("dbg_printTPtr Raw pointer value: %p\n") % (void *)obj.raw_()).str(), sout);
  DynamicScopeManager scope(_sym_STARenablePrintPrettySTAR, _Nil<T_O>());
  scope.pushSpecialVariableAndSet(cl::_sym_STARprint_readablySTAR, _lisp->_boolean(print_pretty));
  clasp_write_string((BF("dbg_printTPtr object class --> %s\n") % _rep_(obj->__class()->className())).str(), sout);
  fflush(stdout);
  write_ugly_object(obj, sout);
  clasp_force_output(sout);
}
};

/*! Sets the flag that controlC has been pressed so that when
      the process continues it will drop into the debugging repl */
void dbg_controlC() {
  SET_SIGNAL(SIGINT);
  printf("%s:%d   Simulating SIGINT (Control-C) signal - debugging REPL will start up when you continue\n", __FILE__, __LINE__);
}
};

namespace core {

void initialize_debugging() {
  CoreDefun(clibBacktrace);
  CoreDefun(lowLevelBacktrace);
  Defun(framePointers);
  SYMBOL_EXPORT_SC_(CorePkg, printCurrentIhsFrameEnvironment);
  Defun(printCurrentIhsFrameEnvironment);
}
};
