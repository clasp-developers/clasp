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
//#define DEBUG_LEVEL_FULL

#include <csignal>
#include <execinfo.h>
#include <clasp/core/foundation.h>
#ifdef USE_LIBUNWIND
#include <libunwind.h>
#endif
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/arguments.h>
#include <clasp/core/myReadLine.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/environment.h>
#include <clasp/core/debugger.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/primitives.h>
#include <clasp/core/array.h>
#include <clasp/core/write_ugly.h>
#include <clasp/core/lispStream.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/core/wrappers.h>

namespace core {

void start_debugger() {
  LispDebugger dbg(_Nil<T_O>());
  dbg.invoke();
}

LispDebugger::LispDebugger(T_sp condition) : _CanContinue(false), _Condition(condition) {
  _lisp->incrementDebuggerLevel();
  af_gotoIhsTop();
}

LispDebugger::LispDebugger() : _CanContinue(true) {
  this->_Condition = _Nil<T_O>();
  _lisp->incrementDebuggerLevel();
  af_gotoIhsTop();
}

void LispDebugger::printExpression() {
  int index = core__ihs_current_frame();
  InvocationHistoryFrameIterator_sp frame = this->currentFrame();
  if (frame->isValid()) {
    stringstream ss;
    ss << frame->frame()->asString(index);
    _lisp->print(BF("%s\n") % ss.str());
  } else {
    _lisp->print(BF("No frame.\n"));
  }
}

InvocationHistoryFrameIterator_sp LispDebugger::currentFrame() const {
  int index = core__ihs_current_frame();
  InvocationHistoryFrameIterator_sp frame = core__get_invocation_history_frame(index);
  return frame;
}

size_t global_low_level_debugger_depth = 0;

T_sp LispDebugger::invoke() {
  if ( cl::_sym_STARfeaturesSTAR
       && cl::_sym_STARfeaturesSTAR->symbolValue()
       && cl::_sym_STARfeaturesSTAR->symbolValue().consp()
       && !gctools::As<Cons_sp>(cl::_sym_STARfeaturesSTAR->symbolValue())->memberEq(kw::_sym_interactive)) {
    printf("This is not an interactive session and the low-level debugger was entered - aborting\n");
    abort();
  }
  ++global_low_level_debugger_depth;
  if ( global_low_level_debugger_depth > 10 ) {
    printf("This is not an interactive session and the low-level debugger was entered too many times - exiting\n");
    exit(1);
  }
  //	DebuggerIHF debuggerStack(my_thread->invocationHistoryStack(),_Nil<ActivationFrame_O>());
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
    if (core__ihs_env(core__ihs_current_frame()).notnilp()) {
      sprompt << "(+ENV)";
    }
    sprompt << "[" << _lisp->debuggerLevel() << "]>";
    bool end_of_transmission(false);
    line = myReadLine(sprompt.str(), end_of_transmission);
    if (end_of_transmission) {
      printf("%s:%d Exiting debugger\n", __FILE__, __LINE__ );
      throw core::ExitProgramException(0);
    }
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
        if (frameIdx > core__ihs_top()) {
          frameIdx = core__ihs_top();
        }
        _lisp->print(BF("Switching to frame: %d") % frameIdx);
        core__set_ihs_current_frame(frameIdx);
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
      T_sp func = core__ihs_fun(core__ihs_current_frame());
      _lisp->print(BF("Current function: %s\n") % _rep_(func));
      eval::funcall(cl::_sym_disassemble, func);
      break;
    }
    case 'b': {
      core__ihs_backtrace(_lisp->_true(), _Nil<T_O>());
      break;
    }
    case 'x': {
      this->printExpression();
      break;
    }
    case 'v': {
      this->printExpression();
      T_sp env = core__ihs_env(core__ihs_current_frame());
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
        T_mv result;
        T_sp env = core__ihs_env(core__ihs_current_frame());
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
      T_sp env = core__ihs_env(core__ihs_current_frame());
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
      T_sp env = core__ihs_env(core__ihs_current_frame());
      //		DebuggerIHF dbgFrame(my_thread->invocationHistoryStack(),Environment_O::clasp_getActivationFrame(env));
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

#if 0
CL_DEFUN void core__test_backtrace() {
  InvocationHistoryFrame *top = my_thread->_InvocationHistoryStack;
  if (top == NULL) {
    printf("Empty InvocationHistoryStack\n");
    return;
  }
  int index = 0;
  for (InvocationHistoryFrame *cur = top; cur != NULL; cur = cur->_Previous) {
    T_sp frame = cur->valist_sp();
    printf("Frame[%d] = %p\n", index, frame.raw_());
    ++index;
  }
  printf("----Done\n");
}
#endif

SYMBOL_EXPORT_SC_(CorePkg,make_shadow_backtrace_frame);

CL_DEFUN List_sp core__shadow_backtrace_as_list() {
  const InvocationHistoryFrame *top = my_thread->_InvocationHistoryStackTop;
  if (top == NULL) {
    return _Nil<T_O>();
  }
  ql::list result;
  int index = 0;
  for (const InvocationHistoryFrame *cur = top; cur != NULL; cur = cur->_Previous) {
    if (cur->_Previous) {
      T_sp frame = eval::funcall(_sym_make_shadow_backtrace_frame,
                                 INTERN_(kw,index), make_fixnum(index),
                                 INTERN_(kw,frame_address), Pointer_O::create((void*)cur),
                                 INTERN_(kw,function_name), gc::As<Closure_sp>(cur->function())->functionName(),
                                 INTERN_(kw,function), cur->function(),
                                 INTERN_(kw,arguments), cur->arguments(),
                                 INTERN_(kw,environment), cur->function());
      result << frame;
      ++index;
    }
  }
  return result.cons();
}


bool search_for_matching_close_bracket(const std::string& sin, size_t& pos, stringstream& sacc) {
  for ( size_t i=pos; i<sin.size(); ++i ) {
    if (sin[i] == '>') {
      pos = i+1;
      return true;
    }
    if (sin[i] == '<') {
      pos = i;
      return search_for_matching_close_bracket(sin,pos,sacc);
    }
    sacc << sin[i];
  }
  return false;
}


std::string global_smart_ptr_head = "gctools::smart_ptr<";

bool mangle_next_smartPtr(const std::string& str, size_t& pos, stringstream& sout) {
  size_t smartPtrStart = str.find(global_smart_ptr_head,pos);
  if (smartPtrStart != std::string::npos ) {
    sout << str.substr(pos,smartPtrStart-pos);
    size_t bracketStart = smartPtrStart+global_smart_ptr_head.size(); // length of "gctools::smartPtr<"
    stringstream sinner;
    search_for_matching_close_bracket(str,bracketStart,sinner);
    std::string innerType = sinner.str();
    if (innerType.size() > 2) {
      if (innerType.substr(innerType.size()-2,2) == "_O") {
        sout << innerType.substr(0,innerType.size()-2);
        sout << "_sp";
      } else if (innerType.substr(innerType.size()-2,2) == "_V") {
        sout << innerType.substr(0,innerType.size()-2);
        sout << "_sp";
      } else {
        sout << innerType << "_sp";
      }
    } else {
      sout << innerType;
    }
    pos = bracketStart;
    return true;
  }
  sout << str.substr(pos,str.size()-pos);
  return false;
}


CL_DEFUN SimpleBaseString_sp core__ever_so_slightly_mangle_cxx_names(const std::string& raw_name)
{
  stringstream sout;
  size_t pos = 0;
  while (mangle_next_smartPtr(raw_name,pos,sout));
  return SimpleBaseString_O::make(sout.str());
}
  
  

void low_level_backtrace(bool with_args) {
  const InvocationHistoryFrame *top = my_thread->_InvocationHistoryStackTop;
  if (top == NULL) {
    printf("Empty InvocationHistoryStack\n");
    return;
  }
  int index = 0;
  for (const InvocationHistoryFrame *cur = top; cur != NULL; cur = cur->_Previous) {
    string name = "-no-name-";
    T_sp tclosure = cur->function();
    if (!tclosure) {
      name = "-NO-CLOSURE-";
    } else if (tclosure.generalp()){
      General_sp closure = gc::As_unsafe<General_sp>(tclosure);
      if (closure.nilp()) {
        name = "NIL";
      } else if (gc::IsA<Function_sp>(closure)) {
        Function_sp func = gc::As_unsafe<Function_sp>(closure);
        if (func->functionName().notnilp()) {
          try {
            name = _rep_(func->functionName());
          } catch (...) {
            name = "-BAD-NAME-";
          }
        }
        /*Nilable?*/ T_sp sfi = core__source_file_info(func->sourceFileInfo());
        string sourceName = "cannot-determine";
        if (sfi.notnilp()) {
          sourceName = gc::As<SourceFileInfo_sp>(sfi)->fileName();
        }
        printf("#%4d frame@%p closure@%p %s/%3d\n    %40s ", index, cur, closure.raw_(), sourceName.c_str(), func->lineNumber(), name.c_str() );
        if (with_args) {
          SimpleVector_sp args = cur->arguments();
          for ( size_t i(0), iEnd(args->length()); i<iEnd; ++i ) { printf( " %s@%p", _rep_((*args)[i]).c_str(), (*args)[i].raw_()); }
        }
        printf("\n");
        goto SKIP_PRINT;
      } else {
        name = _rep_(closure);
      }
    } else {
      name = "-BAD-CLOSURE-";
    }
    printf("_Index: %4d  Frame@%p(previous=%p)  closure@%p  closure->name[%40s]\n",
           index, cur, cur->_Previous, tclosure.raw_(), name.c_str() );
  SKIP_PRINT:
    ++index;
  }
  printf("----Done\n");
}

CL_LAMBDA(&optional with_args);
CL_DECLARE();
CL_DOCSTRING("lowLevelBacktrace");
CL_DEFUN void core__low_level_backtrace() {
  low_level_backtrace(false);
}


CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("lowLevelBacktrace");
CL_DEFUN void core__low_level_backtrace_with_args() {
  low_level_backtrace(true);
}


int safe_backtrace(void**& return_buffer)
{
#define START_BACKTRACE_SIZE 512
  size_t num = START_BACKTRACE_SIZE;
  for ( int i=0; i<100; ++i ) {
    void** buffer = (void**)malloc(sizeof(void*)*num);
    size_t returned = backtrace(buffer,num);
    if (returned < num) {
      return_buffer = buffer;
      return returned;
    }
    free(buffer);
    num = num*2;
  }
  printf("%s:%d Couldn't get backtrace\n", __FILE__, __LINE__ );
  abort();
}


CL_DEFUN T_sp core__maybe_demangle(core::String_sp s)
{
  char *funcname = (char *)malloc(1024);
  size_t funcnamesize = 1024;
  std::string fnName = s->get_std_string();
  int status;
  char *ret = abi::__cxa_demangle(fnName.c_str(), funcname, &funcnamesize, &status);
  if (status == 0) {
    std::string demangled(funcname);
    free(ret);
    return core__ever_so_slightly_mangle_cxx_names(demangled);
  } else {
    if (funcname) free(funcname);
    return _Nil<T_O>();
  }
}

CL_DEFUN T_sp core__libunwind_backtrace_as_list() {
#ifdef USE_LIBUNWIND
  unw_context_t context;
  unw_getcontext(&context);
  unw_cursor_t cursor;
  unw_init_local(&cursor,&context);
  char buffer[1024];
  unw_word_t offset;
  int step;
  do {
    int res = unw_get_proc_name(&cursor,buffer,1024,&offset);
    if ( res < 0 ) {
      printf("%s:%d unw_get_proc_name returned error %d\n", __FILE__, __LINE__, res);
    } else {
      printf("%s:%d  %s\n", __FILE__, __LINE__, buffer );
    }
    unw_proc_info_t proc_info;
    int pi_res = unw_get_proc_info(&cursor,&proc_info);
    if (pi_res<0) {
      printf("%s:%d unw_get_proc_info returned error %d\n", __FILE__, __LINE__, pi_res);
    } else {
      printf("          start: %p   end: %p\n", (void*)proc_info.start_ip, (void*)proc_info.end_ip);
    }
    step = unw_step(&cursor);
    if ( step < 0 ) {
      printf("%s:%d unw_step returned error %d\n", __FILE__, __LINE__, step);
    }
  } while (step>0);
  printf("%s:%d  End of backtrace\n", __FILE__, __LINE__);
#endif
  return _Nil<T_O>();
}

CL_LAMBDA(&optional (depth 0));
CL_DECLARE();
CL_DOCSTRING("backtrace");
CL_DEFUN T_sp core__clib_backtrace_as_list() {
  char *funcname = (char *)malloc(1024);
  size_t funcnamesize = 1024;
  void** buffer = NULL;
  uintptr_t stackTop = (uintptr_t)my_thread->_StackTop;
  int nptrs = safe_backtrace(buffer);
  char **strings = backtrace_symbols(buffer, nptrs);
  if (strings == NULL) {
    if (buffer) free(buffer);
    return _Nil<T_O>();
  } else {
    ql::list result;
    void* bp = __builtin_frame_address(0);
    for (int i = 0; i < nptrs; ++i) {
      std::string str(strings[i]);
      SimpleBaseString_sp sstr = SimpleBaseString_O::make(str);
      Pointer_sp ptr = Pointer_O::create(buffer[i]);
      //printf("func: %s  bp = %p\n", str.c_str(), bp);
      T_sp frame;
      if (bp) {
        frame = Pointer_O::create(bp);
        bp = *(void**)bp;
        // Once we hit what we consider the top (bottom) stack pointer stop looking
        // by setting bp to NULL
        if ((uintptr_t)bp>stackTop) bp = NULL;
      } else {
        frame = _Nil<T_O>();
      }
      result << Cons_O::createList(ptr,sstr,frame);
    }
    if (buffer) free(buffer);
    if (strings) free(strings);
    return result.cons();
  }
};

CL_LAMBDA(&optional (depth 0));
CL_DECLARE();
CL_DOCSTRING("backtrace");
CL_DEFUN void core__clib_backtrace(int depth) {
// Play with Unix backtrace(3)
#define BACKTRACE_SIZE 1024
  printf("Entered core__clib_backtrace - symbol: %s\n", _rep_(INTERN_(core, theClibBacktraceFunctionSymbol)).c_str());
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
      if (depth && i >= depth)
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

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("framePointers");
CL_DEFUN void core__frame_pointers() {
  void *fp = __builtin_frame_address(0); // Constant integer only
  if (fp != NULL)
    printf("Frame pointer --> %p\n", fp);
};
};

namespace core {

#define ARGS_af_gotoIhsTop "()"
#define DECL_af_gotoIhsTop ""
#define DOCS_af_gotoIhsTop "gotoIhsTop"
void af_gotoIhsTop() {
  _sym_STARihsCurrentSTAR->setf_symbolValue(make_fixnum(core__ihs_top()));
};

#define ARGS_af_gotoIhsPrev "()"
#define DECL_af_gotoIhsPrev ""
#define DOCS_af_gotoIhsPrev "gotoIhsPrev"
void af_gotoIhsPrev() {
  int ihsCur = core__ihs_current_frame();
  _sym_STARihsCurrentSTAR->setf_symbolValue(make_fixnum(core__ihs_prev(ihsCur)));
};

#define ARGS_af_gotoIhsNext "()"
#define DECL_af_gotoIhsNext ""
#define DOCS_af_gotoIhsNext "gotoIhsNext"
void af_gotoIhsNext() {
  int ihsCur = core__ihs_current_frame();
  _sym_STARihsCurrentSTAR->setf_symbolValue(make_fixnum(core__ihs_next(ihsCur)));
};

#define ARGS_af_gotoIhsFrame "(frame-index)"
#define DECL_af_gotoIhsFrame ""
#define DOCS_af_gotoIhsFrame "gotoIhsFrame"
void af_gotoIhsFrame(int frame_index) {
  if (frame_index < 0)
    frame_index = 0;
  if (frame_index >= core__ihs_top())
    frame_index = core__ihs_top() - 1;
  int ihsCur = frame_index;
  _sym_STARihsCurrentSTAR->setf_symbolValue(make_fixnum(ihsCur));
};

#define ARGS_af_printCurrentIhsFrame "()"
#define DECL_af_printCurrentIhsFrame ""
#define DOCS_af_printCurrentIhsFrame "printCurrentIhsFrame"
void af_printCurrentIhsFrame() {
  int ihsCur = core__ihs_current_frame();
  T_sp fun = core__ihs_fun(ihsCur);
  printf("Frame[%d] %s\n", ihsCur, _rep_(fun).c_str());
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("printCurrentIhsFrameEnvironment");
CL_DEFUN void core__print_current_ihs_frame_environment() {
  T_sp args = core__ihs_arguments(core__ihs_current_frame());
  if (args.notnilp()) {
    VectorObjects_sp vargs = gc::As<VectorObjects_sp>(args);
    for (int i = 0; i < cl__length(vargs); ++i) {
      _lisp->print(BF("arg%s --> %s") % i % _rep_(vargs->rowMajorAref(i)));
    }
  } else {
    _lisp->print(BF("Args not available"));
  }
  T_sp env = core__ihs_env(core__ihs_current_frame());
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
  printf("If this locks up then there was an error in the evaluation\n");
  printf("Figure out how to make debugger.cc>>af_evalPrint always return\n");
  int ihsCur = core__ihs_current_frame();
  T_sp env = core__ihs_env(ihsCur);
  _lisp->readEvalPrintString(expr, env, true);
};


CL_DEFUN void core__lowLevelDescribe(T_sp obj) {
  dbg_lowLevelDescribe(obj);
}

void dbg_VaList_sp_describe(T_sp obj) {
    // Convert the T_sp object into a VaList_sp object
    VaList_sp vl = VaList_sp((gc::Tagged)obj.raw_());
    printf("Original va_list at: %p\n", &((Vaslist *)gc::untag_vaslist(reinterpret_cast<Vaslist *>(obj.raw_())))->_Args);
    // Create a copy of the Vaslist with a va_copy of the va_list
    Vaslist vlcopy_s(*vl);
    VaList_sp vlcopy(&vlcopy_s);
    printf("Calling dump_Vaslist_ptr\n");
    bool atHead = dump_Vaslist_ptr(&vlcopy_s);
    if (atHead) {
      for (size_t i(0), iEnd(vlcopy->remaining_nargs()); i < iEnd; ++i) {
        T_sp v = vlcopy->next_arg();
        printf("entry@%p %3zu --> %s\n", v.raw_(), i, _rep_(v).c_str());
      }
    }
}

void dbg_lowLevelDescribe(T_sp obj) {
  if (obj.valistp()) {
    dbg_VaList_sp_describe(obj);
  } else if (obj.fixnump()) {
    printf("fixnum_tag: %" PFixnum "\n", obj.unsafe_fixnum());
  } else if (obj.single_floatp()) {
    printf("single-float: %f\n", obj.unsafe_single_float());
  } else if (obj.characterp()) {
    printf("character: %d #\\%c\n", obj.unsafe_character(), obj.unsafe_character());
  } else if (obj.generalp()) {
    printf("vtable-ptr: %p  typeid: %s\n", &*obj, typeid(obj.unsafe_general()).name());
    printf("className-> %s\n", obj.unsafe_general()->className().c_str());
    printf("contents-> [%s]\n", _rep_(obj).c_str());
    if ( Closure_sp closure = obj.asOrNull<Closure_O>() ) {
      core__closure_slots_dump(closure);
    }
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
  client_describe(p);
  T_sp obj((gctools::Tagged) reinterpret_cast<T_O *>(p));
  dbg_lowLevelDescribe(obj);
}

void dbg_describe_tagged_T_Optr_header(T_O *p) {
  client_describe(p);
}

extern void dbg_describe(T_sp obj);
void dbg_describe(T_sp obj) {
  DynamicScopeManager scope(_sym_STARenablePrintPrettySTAR, _Nil<T_O>());
  stringstream ss;
  printf("dbg_describe object class--> %s\n", _rep_(cl__class_of(obj)->_className()).c_str());
  ss << _rep_(obj);
  printf("dbg_describe: %s\n", ss.str().c_str());
  fflush(stdout);
}

void dbg_describe_cons(Cons_sp obj) {
  DynamicScopeManager scope(_sym_STARenablePrintPrettySTAR, _Nil<T_O>());
  stringstream ss;
  printf("dbg_describe object class--> CONS\n");
  ss << _rep_(obj);
  printf("dbg_describe: %s\n", ss.str().c_str());
}

void dbg_describe_symbol(Symbol_sp obj) {
  DynamicScopeManager scope(_sym_STARenablePrintPrettySTAR, _Nil<T_O>());
  stringstream ss;
  printf("dbg_describe object class--> %s\n", _rep_(obj->__class()->_className()).c_str());
  ss << _rep_(obj);
  printf("dbg_describe: %s\n", ss.str().c_str());
}

void dbg_describeActivationFrame(ActivationFrame_sp obj) {
  DynamicScopeManager scope(_sym_STARenablePrintPrettySTAR, _Nil<T_O>());
  stringstream ss;
  printf("dbg_describe ActivationFrame class--> %s\n", _rep_(obj->__class()->_className()).c_str());
  ss << _rep_(obj);
  printf("dbg_describe: %s\n", ss.str().c_str());
}

void dbg_describeTPtr(uintptr_clasp_t raw) {
  if (raw == 0) {
    printf("dbg_describe: NULL\n");
    return;
  }
  T_sp obj = gctools::smart_ptr<T_O>(raw);
  printf("dbg_describeTPtr Raw pointer value: %p\n", obj.raw_());
  DynamicScopeManager scope(_sym_STARenablePrintPrettySTAR, _Nil<T_O>());
  stringstream ss;
  printf("dbg_describe object class--> %s\n", _rep_(lisp_instance_class(obj)->_className()).c_str());
  ss << _rep_(obj);
  printf("dbg_describe: %s\n", ss.str().c_str());
  fflush(stdout);
}

void dbg_printTPtr(uintptr_clasp_t raw, bool print_pretty) {
  core::T_sp sout = cl::_sym_STARstandard_outputSTAR->symbolValue();
  T_sp obj = gctools::smart_ptr<T_O>((gc::Tagged)raw);
  clasp_write_string((BF("dbg_printTPtr Raw pointer value: %p\n") % (void *)obj.raw_()).str(), sout);
  DynamicScopeManager scope(_sym_STARenablePrintPrettySTAR, _Nil<T_O>());
  scope.pushSpecialVariableAndSet(cl::_sym_STARprint_readablySTAR, _lisp->_boolean(print_pretty));
  clasp_write_string((BF("dbg_printTPtr object class --> %s\n") % _rep_(lisp_instance_class(obj)->_className())).str(), sout);
  fflush(stdout);
  write_ugly_object(obj, sout);
  clasp_force_output(sout);
}

#if 0
/*! Sets the flag that controlC has been pressed so that when
      the process continues it will drop into the debugging repl */
void dbg_controlC() {
  SET_SIGNAL(SIGINT);
  printf("%s:%d   Simulating SIGINT (Control-C) signal - debugging REPL will start up when you continue\n", __FILE__, __LINE__);
}
#endif
};



extern "C" {

void tprint(void* ptr)
{
  core::dbg_printTPtr((uintptr_clasp_t) ptr,false);
}

void c_ehs() {
  printf("%s:%d ExceptionStack summary\n%s\n", __FILE__, __LINE__, my_thread->exceptionStack().summary().c_str());
}

void c_bt() {
  core::eval::funcall(core::_sym_bt->symbolFunction());
};

void c_btcl() {
  core::eval::funcall(core::_sym_btcl->symbolFunction());
};

void tsymbol(void* ptr)
{
  printf("%s:%d Looking up symbol at ptr->%p\n", __FILE__, __LINE__, ptr);
  core::T_sp result = llvmo::llvm_sys__lookup_jit_symbol_info(ptr);
  printf("      Result -> %s\n", _rep_(result).c_str());
}

};
namespace core {

  SYMBOL_EXPORT_SC_(CorePkg, printCurrentIhsFrameEnvironment);

};
