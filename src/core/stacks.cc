/*
    File: stacks.cc
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
#include <execinfo.h>
#include <clasp/core/foundation.h>
#include <clasp/core/stacks.h>
#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/designators.h>
#include <clasp/core/array.h>
#include <clasp/core/instance.h>
#include <clasp/core/mpPackage.h>
#include <clasp/core/primitives.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/write_ugly.h>
#include <clasp/core/symbol.h>
#include <clasp/core/wrappers.h>

int global_debug_ihs = 0;

extern "C" {
extern void dump_backtrace_n_frames(size_t n);
};

namespace core {


#ifdef DEBUG_IHS
// Maintain a shadow stack of function pointers when DEBUG_IHS is turned on
// This will slow the system down, but will provide function names of functions
// on the stack when an IHS problem is 
std::vector<void*>     global_debug_ihs_shadow_stack;
#endif

// TODO Get rid of this definition of global_debug_ihs
int global_debug_ihs = 0;

void validate_InvocationHistoryStack(int pushPop, const InvocationHistoryFrame* frame, const InvocationHistoryFrame* stackTop) {
  if (pushPop) {
    printf(" IHS PUSH");
  } else {
    printf(" IHS POP ");
  }
  if (global_debug_ihs==1) {
    printf("%s:%d      Dumping %d frames\n", __FILE__, __LINE__, 10);
    dump_backtrace_n_frames(10);
  }
  printf("  stack@%p  frame@%p frame->_Previous@%p\n", stackTop, frame, frame->_Previous);
  if (global_debug_ihs==2) invocation_history_stack_dump(frame,"Validating stack",0);
  fflush(stdout);
}
  


void error_InvocationHistoryStack(const InvocationHistoryFrame* frame, const InvocationHistoryFrame* stackTop) {
//  printf("%s:%d  Error in closure @%p\n", __FILE__, __LINE__, (void*)tagged_closure);
  printf("\n%s:%d ------- the my_thread->_InvocationHistoryStackTop pointer @%p\n", __FILE__, __LINE__, my_thread->_InvocationHistoryStackTop );
  printf("          does not match the current frame being popped @%p  closure@%p  func_ptr@%p\n", frame, frame->function().raw_(), gc::As<Function_sp>(frame->function())->entry.load() );
  if (my_thread->_InvocationHistoryStackTop < frame ) {
    printf("     It appears that a function did not clean up after itself\n");
    printf("     and at least one frame was left on the stack when it should have been popped\n");
    printf("     since my_thread->_InvocationHistoryStackTop < frame (stack grows down)\n");
  } else {
    printf("     my_thread->_InvocationHistoryStackTop > frame   !!!!\n");
    printf("     This will happen if a function calls cc_pop_InvocationHistoryStack TWICE before it exits - that means bad code generation\n");
  }
#ifdef DEBUG_IHS
  void* fn_ptr = NULL;
  printf("   To debug this do the following:\n");
  printf("     (1) Recompile EVERYTHING (including CL code) with DEBUG_IHS turned on - this is to ensure that old code in the image is recompiled\n");
  printf("     (2) Run under the debugger and reproduce the error\n");
  printf("     (3) Set a break point on one (TO BE DETERMINED) of the backtrace return addresses below\n");
  backtrace_symbols_fd((void* const *)&my_thread->_IHSBacktrace[0],IHS_BACKTRACE_SIZE,1);
  printf("              (Note: This may not be true if the function above is a commonly called one\n");
  printf("                - in that event you need to find some other function to break on when the bad function is on the stack\n");
  printf("     (4) Reproduce the error, breaking in the function above\n");
  printf("     (5) Type:   expr global_debug_ihs = 1    (this turns on additional debugging)\n");
  printf("     (6) continue running,  every push_InvocationHistoryFrame & pop_InvocationHistoryFrame call will cause some of the stack frames to be dumped\n");
  printf("     (7) when the error is hit and the debugger is reentered - look at the last push/pop partial stack dump - the bad function should be the top one.\n");
  printf("   Also, look at the list of function symbols below - from the bottom up are the functions whose frames should have been popped off the stack but were not\n");
  printf("        One of them should contain the bad code\n");
  backtrace_symbols_fd(&global_debug_ihs_shadow_stack[0],global_debug_ihs_shadow_stack.size(),1);
#else
  printf("         Turn on DEBUG_IHS and rebuild everything to assist in debugging this problem - when you rerun you will get more advice on how to fix this\n");
#endif
  printf("       - It is very likely that a cleanup form what would have invoked cc_pop_InvocationHistoryFrame was not evaluated\n");
  printf("         this is usually due to a CALL being used to call a function that throws an exception and unwinds the stack rather than an INVOKE\n");
  printf("         or a function was returned from or unwound through and cc_pop_InvocationHistoryFrame was not called\n");
  invocation_history_stack_dump(frame,"Stack from frame\n",0);
  abort();
};



/*! Return the index of the stack entry with the matching key.
      If return -1 then the key wasn't found */
int ExceptionStack::findKey(FrameKind kind, T_sp key) {
  for (int i(this->_Stack.size() - 1); i >= 0; --i) {
    if (this->_Stack[i]._FrameKind == kind && this->_Stack[i]._Key == key)
      return i;
  }
  return -1;
}

Vector_sp ExceptionStack::backtrace() {
  if (this->_Stack.size() == 0) {
    return _Nil<Vector_O>();
  }
  printf("%s:%d ExceptionStack::backtrace stack size = %zu\n", __FILE__, __LINE__, this->_Stack.size());
  Vector_sp result = core__make_vector(_Nil<T_O>(), this->_Stack.size(), false, make_fixnum((int)(this->_Stack.size())));
  for (int i(0), iEnd(this->_Stack.size()); i < iEnd; ++i) {
    Symbol_sp kind;
    SYMBOL_EXPORT_SC_(KeywordPkg, catchFrame);
    SYMBOL_EXPORT_SC_(KeywordPkg, blockFrame);
    SYMBOL_EXPORT_SC_(KeywordPkg, tagbodyFrame);
    SYMBOL_EXPORT_SC_(KeywordPkg, landingPadFrame);
    switch (this->_Stack[i]._FrameKind) {
    case NullFrame:
      kind = _Nil<Symbol_O>();
      break;
    case CatchFrame:
      kind = kw::_sym_catchFrame;
      break;
    case BlockFrame:
      kind = kw::_sym_blockFrame;
      break;
    case TagbodyFrame:
      kind = kw::_sym_tagbodyFrame;
      break;
    case LandingPadFrame:
      kind = kw::_sym_landingPadFrame;
      break;
    };
    result->rowMajorAset(i, Cons_O::create(kind, this->_Stack[i]._Key));
  }
  return result;
}


void InvocationHistoryFrame::validate() const {
#if 0
  T_sp res((gctools::Tagged)(((core::T_O**)(this->_args->reg_save_area))[LCC_CLOSURE_REGISTER]));
  if (res) {
    if (gc::IsA<Function_sp>(res)) {
      return;
    }
    printf("%s:%d  There is a problem in the creation of an InvocationHistoryFrame - bad function: %p\n:", __FILE__, __LINE__, res.raw_());
    printf("        Closure -> %s\n", _rep_(res).c_str());
    abort();
  }
  printf("%s:%d  There is a problem in the creation of an InvocationHistoryFrame - bad function: %p\n:", __FILE__, __LINE__, res.raw_());
  abort();
#endif
}

T_sp InvocationHistoryFrame::function() const
  {
    Function_sp res((gctools::Tagged)(((core::T_O**)(this->_args->reg_save_area))[LCC_CLOSURE_REGISTER]));
    if ( !res ) {
      return _Nil<T_O>();
    }
    return res;
  }

void* InvocationHistoryFrame::register_save_area() const
  {
    return (this->_args->reg_save_area);
  }

DONT_OPTIMIZE_WHEN_DEBUG_RELEASE SimpleVector_sp InvocationHistoryFrame::arguments() const {
  size_t numberOfArguments = this->_remaining_nargs;
  va_list cargs;
  va_copy(cargs,this->_args);
  T_O* objRaw;
  if (numberOfArguments > CALL_ARGUMENTS_LIMIT) {
    va_end(cargs);
    return SimpleVector_O::make(0);
  }
  SimpleVector_sp vargs = SimpleVector_O::make(numberOfArguments);
  for (size_t i(0); i < numberOfArguments; ++i) {
    //objRaw = this->valist_sp().indexed_arg(i);
    objRaw = va_arg(cargs,core::T_O*);
    (*vargs)[i] = T_sp((gc::Tagged)objRaw);
  }
  va_end(cargs);
  return vargs;
}

string InvocationHistoryFrame::argumentsAsString(int maxWidth) const {
  SimpleVector_sp vargs = this->arguments();
  T_sp sout = clasp_make_string_output_stream();
  int nargs = cl__length(vargs);
  for (int i(0); i < nargs; ++i) {
    T_sp obj = vargs->rowMajorAref(i);
    if (Instance_sp iobj = obj.asOrNull<Instance_O>()) {
      clasp_write_string("#<", sout);
      write_ugly_object(cl__class_of(obj), sout);
      clasp_write_string("> ", sout);
    } else {
      write_ugly_object(obj, sout);
      clasp_write_char(' ', sout);
    }
  }
  String_sp strres = gc::As<String_sp>(cl__get_output_stream_string(sout));
  if (cl__length(strres) > maxWidth) {
    return strres->get().substr(0, maxWidth) + "...";
  }
  return strres->get();
}

string InvocationHistoryFrame::asStringLowLevel(Closure_sp closure,int index) const {
  if (!closure) {
    return "InvocationHistoryFrame::asStringLowLevel NULL closure";
  };
  T_sp funcNameObj = closure->functionName();
  string funcName = _rep_(funcNameObj);
  uint lineNumber = closure->lineNumber();
  uint column = closure->column();
  FileScope_sp sfi = gc::As<FileScope_sp>(core__file_scope(closure->sourcePathname()));
  string sourceFileName = sfi->fileName();
  stringstream ss;
  string closureType = "/?";
  if (closure) {
    if (closure->interpretedP()) {
      closureType = "/i";
    } else if (closure->compiledP()) {
      closureType = "/c";
    } else if (closure->builtinP()) {
      closureType = "/b";
    }
  } else
    closureType = "toplevel";
  string sargs = this->argumentsAsString(256);
  ss << (BF("#%3d%2s %20s %5d (%s %s)") % index % closureType % sourceFileName % lineNumber % funcName % sargs).str();
  //	ss << std::endl;
  //	ss << (BF("     activationFrame->%p") % this->activationFrame().get()).str();
  return ss.str();
}

string InvocationHistoryFrame::asString(int index) const {
  string name;
  return this->asStringLowLevel(gc::As<Closure_sp>(this->function()),index);
}

void InvocationHistoryFrame::dump(int index) const {
  string dump = this->asString(index);
  printf("%s\n", dump.c_str());
}

void invocation_history_stack_dump(const InvocationHistoryFrame* frame, const char* msg, size_t frames)
{
  printf("%s\n", msg);
  const InvocationHistoryFrame* cur = frame;
  if (frames == 0) {
    frames = 9999999999;
  }
  size_t count=0;
  printf("   my_thread->_InvocationHistoryStackTop -> %p\n", my_thread->_InvocationHistoryStackTop);
  while (cur) {
    if (frames==0) break;
    --frames;
    T_sp tfn = cur->function();
    if (gc::IsA<Function_sp>(tfn)) {
      Function_sp fn = gc::As_unsafe<Function_sp>(tfn);
      printf("    frame[%4lu] @%p (previous %p)  this->_args@%p this->_args->reg_save_area@%p closure@%p  fptr@%p: %s\n", count, cur, cur->_Previous, &cur->_args, cur->_args->reg_save_area, tfn.raw_(), (void*)fn->entry.load(), _rep_(fn->functionName()).c_str());
    } else {
      printf("    frame[%4lu] @%p (previous %p)  this->_args@%p this->_args->reg_save_area@%p  BAD-CLOSURE-INFO closure@%p\n", count, cur, cur->_Previous, &cur->_args, cur->_args->reg_save_area, tfn.raw_() );
    }
    cur = cur->_Previous;
    ++count;
  }
  printf("     DONE -----\n");
}

size_t invocation_history_stack_depth(const InvocationHistoryFrame* frame)
{
  size_t count = 0;
  const InvocationHistoryFrame* cur = frame;
  while (cur) {
    cur = cur->_Previous;
    ++count;
  }
  return count;
}


size_t backtrace_size() {
  const InvocationHistoryFrame* frame = my_thread->_InvocationHistoryStackTop;
  size_t count = 0;
  while (frame) {
    frame = frame->_Previous;
    ++count;
  }
  return count;
}

string backtrace_as_string() {
  stringstream ss;
  ss.str("");
  ss << std::endl;
  const InvocationHistoryFrame* frame = my_thread->_InvocationHistoryStackTop;
  ss << "--------STACK TRACE--------" << std::endl;
  int ihsCur = core__ihs_current_frame();
  const InvocationHistoryFrame* cur = frame;
  int i = 0;
  while (cur) {
    if (i == ihsCur) {
      ss << "-->";
    } else {
      ss << "   ";
    }
    ss << "frame";
    ss << cur->asString(i) << std::endl;
    cur = cur->_Previous;
    ++i;
  }
  return ss.str();
}


};

extern "C" {
void dump_backtrace() {
  const core::InvocationHistoryFrame* frame = my_thread->_InvocationHistoryStackTop;
  invocation_history_stack_dump(frame,"STACK TRACE",0);
}
void dump_backtrace_n_frames(size_t n) {
  const core::InvocationHistoryFrame* frame = my_thread->_InvocationHistoryStackTop;
  invocation_history_stack_dump(frame,"STACK TRACE",n);
}
};

namespace core {

CL_DEFUN void core__dump_debug_ihs_shadow_stack() {
#ifdef DEBUG_IHS
  printf("global_debug_ihs_shadow_stack\n");
  backtrace_symbols_fd(&global_debug_ihs_shadow_stack[0],global_debug_ihs_shadow_stack.size(),1);
  printf("my_thread->_IHSBacktrace\n");
  backtrace_symbols_fd((void* const *)&my_thread->_IHSBacktrace[0],IHS_BACKTRACE_SIZE,1);
#endif
}




}
