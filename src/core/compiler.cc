/*
    File: compiler.cc
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

// #define EXPOSE_DLOPEN
// #define EXPOSE_DLLOAD
#define DEBUG_LEVEL_FULL
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/core/cxxObject.h>
#include <clasp/core/record.h>
#include <clasp/core/lisp.h>
#include <dlfcn.h>
#include <clasp/core/environment.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/lightProfiler.h>
#include <clasp/core/designators.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/str.h>
#include <clasp/core/compiler.h>
#include <clasp/core/sequence.h>
#include <clasp/core/pathname.h>
#include <clasp/core/unixfsys.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/environment.h>
#include <clasp/core/cleavirPrimopsPackage.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/activationFrame.h>
#include <clasp/core/pointer.h>
#include <clasp/core/environment.h>
#include <clasp/llvmo/intrinsics.h>
#include <clasp/core/wrappers.h>

#ifdef _TARGET_OS_DARWIN
#import <mach-o/dyld.h>
#endif


namespace core {

#define INITIALIZER_CAPACITY_INIT 128
#define INITIALIZER_CAPACITY_MULTIPLIER 2
size_t global_initializer_capacity = 0;
size_t global_initializer_count = 0;
InitializerFunction* global_initializer_functions = NULL;

void register_initializer_function(InitializerFunction fptr)
{
//  printf("%s:%d In register_initializer_function --> %p\n", __FILE__, __LINE__, fptr);
  if ( global_initializer_functions == NULL ) {
    global_initializer_capacity = INITIALIZER_CAPACITY_INIT;
    global_initializer_count = 0;
    global_initializer_functions = (InitializerFunction*)malloc(global_initializer_capacity*sizeof(InitializerFunction));
  } else {
    if ( global_initializer_count == global_initializer_capacity ) {
      global_initializer_capacity = global_initializer_capacity*INITIALIZER_CAPACITY_MULTIPLIER;
      global_initializer_functions = (InitializerFunction*)realloc(global_initializer_functions,global_initializer_capacity*sizeof(InitializerFunction));
    }
  }
  global_initializer_functions[global_initializer_count] = fptr;
  global_initializer_count++;
};

/*! Return the number of initializer_functions that are waiting to be run*/
size_t initializer_functions_are_waiting()
{
//  printf("%s:%d initializer_functions_are_waiting returning %lu\n", __FILE__, __LINE__, global_initializer_count );
  return global_initializer_count;
};

/*! Invoke the initializer functions and clear the array of initializer functions */
void initializer_functions_invoke()
{
  if (global_initializer_count>0) {
#if 0
    printf("%s:%d In initializer_functions_invoke - there are %lu initializer functions\n", __FILE__, __LINE__, global_initializer_count );
    for ( size_t i = 0; i<global_initializer_count; ++i ) {
      InitializerFunction fn = global_initializer_functions[i];
      printf("%s:%d     Initializer fn[%lu]@%p\n", __FILE__, __LINE__, i, fn );
    }
    printf("%s:%d Starting to call the initializer functions\n", __FILE__, __LINE__ );
#endif
    for ( size_t i = 0; i<global_initializer_count; ++i ) {
      InitializerFunction fn = global_initializer_functions[i];
//      printf("%s:%d     About to invoke fn@%p\n", __FILE__, __LINE__, fn );
      (fn)();
    }
    global_initializer_count = 0;
    global_initializer_capacity = 0;
    free(global_initializer_functions);
    global_initializer_functions = NULL;
//    printf("%s:%d Done with startup_functions_invoke()\n", __FILE__, __LINE__ );
  }
}

};


namespace core {

#define STARTUP_FUNCTION_CAPACITY_INIT 128
#define STARTUP_FUNCTION_CAPACITY_MULTIPLIER 2
size_t global_startup_capacity = 0;
size_t global_startup_count = 0;
fnLispCallingConvention* global_startup_functions = NULL;

void register_startup_function(fnLispCallingConvention fptr)
{
#ifdef DEBUG_STARTUP
  printf("%s:%d In register_startup_function --> %p\n", __FILE__, __LINE__, fptr);
#endif
  if ( global_startup_functions == NULL ) {
    global_startup_capacity = STARTUP_FUNCTION_CAPACITY_INIT;
    global_startup_count = 0;
    global_startup_functions = (fnLispCallingConvention*)malloc(global_startup_capacity*sizeof(fnLispCallingConvention));
  } else {
    if ( global_startup_count == global_startup_capacity ) {
      global_startup_capacity = global_startup_capacity*STARTUP_FUNCTION_CAPACITY_MULTIPLIER;
      global_startup_functions = (fnLispCallingConvention*)realloc(global_startup_functions,global_startup_capacity*sizeof(fnLispCallingConvention));
    }
  }
  global_startup_functions[global_startup_count] = fptr;
  global_startup_count++;
};

/*! Return the number of startup_functions that are waiting to be run*/
size_t startup_functions_are_waiting()
{
#ifdef DEBUG_STARTUP
  printf("%s:%d startup_functions_are_waiting returning %lu\n", __FILE__, __LINE__, global_startup_count );
#endif
  return global_startup_count;
};

/*! Invoke the startup functions and clear the array of startup functions */
void startup_functions_invoke()
{
  // Save the current list
  size_t startup_count = global_startup_count;
  fnLispCallingConvention* startup_functions = global_startup_functions;
  // Prepare to accumulate a new list
  global_startup_count = 0;
  global_startup_capacity = 0;
  global_startup_functions = NULL;
  // Invoke the current list
  if (startup_count>0) {
#ifdef DEBUG_STARTUP
    printf("%s:%d In startup_functions_invoke - there are %lu startup functions\n", __FILE__, __LINE__, startup_count );
    for ( size_t i = 0; i<startup_count; ++i ) {
      fnLispCallingConvention fn = startup_functions[i];
      printf("%s:%d     Startup fn[%lu]@%p\n", __FILE__, __LINE__, i, fn );
    }
    printf("%s:%d Starting to call the startup functions\n", __FILE__, __LINE__ );
#endif
    for ( size_t i = 0; i<startup_count; ++i ) {
      fnLispCallingConvention fn = startup_functions[i];
#ifdef DEBUG_STARTUP
      printf("%s:%d     About to invoke fn@%p\n", __FILE__, __LINE__, fn );
#endif
      T_mv result = (fn)(LCC_PASS_MAIN());
    }
#ifdef DEBUG_STARTUP
    printf("%s:%d Done with startup_functions_invoke()\n", __FILE__, __LINE__ );
#endif
  }
}



CL_DEFUN void core__startup_functions_invoke()
{
  startup_functions_invoke();
  printf("%s:%d startup_functions_invoke returned -   this should never happen\n", __FILE__, __LINE__ );
  abort();
};


};

extern "C" {
gctools::return_type wrapped_test(core::T_O* arg0, core::T_O* arg1, core::T_O* arg2 )
{
  printf("%s:%d wrapped_test called (%p, %p, %p)\n", __FILE__, __LINE__, arg0, arg1, arg2 );
  return gctools::return_type();
}

};
namespace core {

int f(Environment_sp &e) {
  (void)e;
  return 1;
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("Print info about booting");
CL_DEFUN void core__help_booting() {
  printf("Useful *features*\n"
         ":ecl-min (should be clasp-min),  :bclasp, :cclasp  -- Tells Clasp what stage it's in and where to get its init file.\n"
         ":notify-on-compile (core:*notify-on-compile*) - prints messages whenever COMPILE is invoked at startup\n"
         ":trace-startup (core:*trace-startup*) - prints messages and timing for running the main function of the compiled code of each system file at startup\n"
         ":debug-startup (core:*debug-startup*) - prints a message and timing for running each top level function\n"
         "\n"
         "Commands (all in CORE package)\n"
         "(load-system <start> <end> &key interp (system *init-files*))   - Load the system files\n"
         "(compile-min) - Compile a minimal system\n"
         "(compile-full) - Compile a full system\n"
         "(compile-kernel-file filename &key reload load-bitcode recompile)   - Compile a system file and put the bitcode in the correct directory\n"
         "(link-system start end prologue-form epilogue-form &key (system *init-files*)) - Link an image together\n"
         "(default-prologue-form &optional features) - Returns a prologue form for link-system\n"
         "(default-epilogue-form) - Returns an epilogue form for link-system\n");
}

CL_LAMBDA(pow2);
CL_DECLARE();
CL_DOCSTRING("Evaluate a TaggedCast 2^pow2 times");
CL_DEFUN Fixnum_sp core__test_tagged_cast(Fixnum_sp pow2) __attribute__((optnone)) {
  Fixnum fpow2 = clasp_to_fixnum(pow2);
  Fixnum times = 1;
  times = times << fpow2;
  printf("%s:%d  fpow2 = %ld  times = %ld\n", __FILE__, __LINE__, fpow2, times);
  Environment_sp env = ValueEnvironment_O::createForNumberOfEntries(5, _Nil<T_O>());
  Fixnum i;
  Fixnum v = 0;
  for (i = 0; i < times; ++i) {
    f(env);
    Environment_sp e = env.asOrNull<Environment_O>();
    v += f(e);
  }
  return Integer_O::create(v);
}

CL_LAMBDA(reps num);
CL_DECLARE();
CL_DOCSTRING("Calculate the num Fibonacci number reps times");
CL_DEFUN Integer_sp core__cxx_fibn(Fixnum_sp reps, Fixnum_sp num) {
  long int freps = clasp_to_fixnum(reps);
  long int fnum = clasp_to_fixnum(num);
  long int p1, p2, z;
  for (long int r = 0; r < freps; ++r) {
    p1 = 1;
    p2 = 1;
    long int rnum = fnum - 2;
    for (long int i = 0; i < rnum; ++i) {
      z = p1 + p2;
      p2 = p1;
      p1 = z;
    }
  }
  return Integer_O::create(z);
}

T_sp varArgsList(int n_args, ...) {
  DEPRECIATED();
  va_list ap;
  va_start(ap, n_args);
  Cons_O::CdrType_sp first = _Nil<Cons_O::CdrType_O>();
  Cons_O::CdrType_sp *curP = &first; // gctools::StackRootedPointerToSmartPtr<Cons_O::CdrType_O> cur(&first);
  for (int i = 1; i <= n_args; ++i) {
    T_sp obj = *(va_arg(ap, const T_sp *));
    Cons_sp one = Cons_O::create(obj);
    *curP = one;          // cur.setPointee(one); // *cur = one;
    curP = one->cdrPtr(); // cur.setPointer(one->cdrPtr()); // cur = one->cdrPtr();
  }
  va_end(ap);
  return first;
}

CL_LAMBDA(object &optional is-function);
CL_DECLARE();
CL_DOCSTRING("mangleName");
CL_DEFUN T_mv core__mangle_name(Symbol_sp sym, bool is_function) {
  Str_sp name;
  if (!is_function) {
    if (sym.nilp())
      name = Str_O::create("CLASP_NIL");
    else if (sym == _lisp->_true())
      name = Str_O::create("CLASP_T");
    else {
      stringstream ss;
      ss << "SYM(" << sym->symbolName()->get() << ")";
      name = Str_O::create(ss.str());
    }
    return Values(_Nil<T_O>(), name, make_fixnum(0), make_fixnum(CALL_ARGUMENTS_LIMIT));
  }
  Function_sp fsym = coerce::functionDesignator(sym);
  if ( BuiltinClosure_sp  bcc = fsym.asOrNull<BuiltinClosure_O>()) {
    (void)bcc; // suppress warning
    return Values(_lisp->_true(), Str_O::create("Provide-c-func-name"), make_fixnum(0), make_fixnum(CALL_ARGUMENTS_LIMIT));
  }
  return Values(_Nil<T_O>(), Str_O::create("Provide-func-name"), make_fixnum(0), make_fixnum(CALL_ARGUMENTS_LIMIT));
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("startupImagePathname - returns a pathname based on *features* :ECL-MIN, :USE-MPS, :BCLASP");
CL_DEFUN T_sp core__startup_image_pathname() {
  stringstream ss;
  ss << "build:";
  ss << VARIANT_NAME;
  ss << "/image.fasl";
  Str_sp spath = Str_O::create(ss.str());
  Pathname_sp pn = cl__pathname(spath);
  return pn;
};


  
CL_LAMBDA(name &optional verbose print external-format);
CL_DECLARE();
CL_DOCSTRING("loadBundle");
CL_DEFUN T_mv core__load_bundle(T_sp pathDesig, T_sp verbose, T_sp print, T_sp external_format) {
  /* Define the source file */
  SourceFileInfo_sp sfi = core__source_file_info(pathDesig);
  DynamicScopeManager scope(_sym_STARcurrentSourceFileInfoSTAR, sfi);
#ifdef USE_SOURCE_DATABASE
  scope.pushSpecialVariableAndSet(_sym_STARsourceDatabaseSTAR, SourceManager_O::create());
#else
  scope.pushSpecialVariableAndSet(_sym_STARsourceDatabaseSTAR, _Nil<T_O>());
#endif
  scope.pushSpecialVariableAndSet(_sym_STARcurrentSourcePosInfoSTAR, SourcePosInfo_O::create(0, 0, 0, 0));
  scope.pushSpecialVariableAndSet(cl::_sym_STARreadtableSTAR, cl::_sym_STARreadtableSTAR->symbolValue());
  scope.pushSpecialVariableAndSet(cl::_sym_STARpackageSTAR, cl::_sym_STARpackageSTAR->symbolValue());
  Pathname_sp path = cl__pathname(pathDesig);
  if (cl__probe_file(path).notnilp())
    goto LOAD;
  path->_Type = Str_O::create("bundle");
  if (cl__probe_file(path).notnilp())
    goto LOAD;
  path->_Type = Str_O::create("fasl");
  if (cl__probe_file(path).notnilp())
    goto LOAD;
  path->_Type = Str_O::create("dylib");
  if (cl__probe_file(path).notnilp())
    goto LOAD;
  path->_Type = Str_O::create("so");
  if (cl__probe_file(path).notnilp())
    goto LOAD;
  SIMPLE_ERROR(BF("Could not find bundle %s") % _rep_(pathDesig));
LOAD:
  Str_sp nameStr = cl__namestring(cl__probe_file(path));
  string name = nameStr->get();

  /* Look up the initialization function. */
  string stem = cl__string_downcase(gc::As<Str_sp>(path->_Name))->get();
  size_t dsp = 0;
  if ((dsp = stem.find("_d")) != string::npos)
    stem = stem.substr(0, dsp);
  else if ((dsp = stem.find("_o")) != string::npos)
    stem = stem.substr(0, dsp);

  int mode = RTLD_NOW | RTLD_LOCAL; // | RTLD_FIRST;
  // Check if we already have this dynamic library loaded
  map<string, void *>::iterator handleIt = _lisp->openDynamicLibraryHandles().find(name);
  if (handleIt != _lisp->openDynamicLibraryHandles().end()) {
    dlclose(handleIt->second);
    //	    printf("%s:%d Closing the existing dynamic library %s\n", __FILE__, __LINE__, name.c_str());
    _lisp->openDynamicLibraryHandles().erase(handleIt);
  }
  //	printf("%s:%d Loading dynamic library: %s\n", __FILE__, __LINE__, name.c_str());
  void *handle = dlopen(name.c_str(), mode);
  if (handle == NULL) {
    string error = dlerror();
    SIMPLE_ERROR(BF("Error in dlopen: %s") % error);
    //    return (Values(_Nil<T_O>(), Str_O::create(error)));
  }
  _lisp->openDynamicLibraryHandles()[name] = handle;
  if (startup_functions_are_waiting()) {
    startup_functions_invoke();
  } else {
    SIMPLE_ERROR(BF("There were no global ctors - there have to be global ctors for load-bundle"));
  }
  T_mv result;
  cc_invoke_startup_functions();
  return (Values(Pointer_O::create(handle), _Nil<T_O>()));
};

#ifdef EXPOSE_DLLOAD
CL_DOCSTRING("dlload - Open a dynamic library and evaluate the 'init_XXXX' extern C function. Returns (values returned-value error-message(or nil if no error)");
CL_DEFUN T_mv core__dlload(T_sp pathDesig) {
  string lib_extension = ".dylib";
#ifdef _TARGET_OS_DARWIN
  lib_extension = ".dylib";
#endif
#ifdef _TARGET_OS_LINUX
  lib_extension = ".so";
#endif
  int mode = RTLD_NOW | RTLD_LOCAL;
  Path_sp path = coerce::pathDesignator(pathDesig);
  Path_sp pathWithProperExtension = path->replaceExtension(lib_extension);
  string ts = pathWithProperExtension->asString();
  printf("%s:%d Loading with core__dlload %s\n", __FILE__, __LINE__, ts.c_str());
  void *handle = dlopen(ts.c_str(), mode);
  if (handle == NULL) {
    string error = dlerror();
    return (Values(_Nil<T_O>(), Str_O::create(error)));
  }
  string stem = path->stem();
  size_t dsp = 0;
  if ((dsp = stem.find("_d")) != string::npos) {
    stem = stem.substr(0, dsp);
  }
  stringstream ss;
  ss << "___kernel_" << stem;
  string initName;
  string kernelInitName = ss.str();
  initName = kernelInitName;
  InitFnPtr mainFunctionPointer = (InitFnPtr)dlsym(handle, kernelInitName.c_str());
  if (mainFunctionPointer == NULL) {
    ss.str("");
    ss << "___user_" << stem;
    string userInitName = ss.str();
    initName = userInitName;
    mainFunctionPointer = (InitFnPtr)dlsym(handle, userInitName.c_str());
    if (mainFunctionPointer == NULL) {
      SIMPLE_ERROR(BF("Could not find initialization function %s or %s") % kernelInitName % userInitName);
    }
  }
  //	printf("Found function %s at address %p\n", initName.c_str(), mainFunctionPointer);
  T_mv result;
  ActivationFrame_sp frame = _Nil<ActivationFrame_O>();
  (*mainFunctionPointer)();
  return (Values(Pointer_O::create(handle), _Nil<T_O>()));
}
#endif

CL_LAMBDA(pathDesig);
CL_DECLARE();
CL_DOCSTRING("dlopen - Open a dynamic library and return the handle. Returns (values returned-value error-message(or nil if no error))");
CL_DEFUN T_mv core__dlopen(T_sp pathDesig) {
  string lib_extension = ".dylib";
  int mode = RTLD_NOW | RTLD_LOCAL;
  Path_sp path = coerce::pathDesignator(pathDesig);
  string ts0 = path->asString();
  void *handle = dlopen(ts0.c_str(), mode);
  if (!handle) {
    printf("%s:%d Could not open %s  error: %s\n", __FILE__, __LINE__, ts0.c_str(), dlerror());
    string error = dlerror();
    return (Values(_Nil<T_O>(), Str_O::create(error)));
  }
  return (Values(Pointer_O::create(handle), _Nil<T_O>()));
}

CL_LAMBDA(name &optional (handle :rtld-default));
CL_DECLARE();
CL_DOCSTRING("(dlsym handle name) handle is from dlopen or :rtld-next, :rtld-self, :rtld-default or :rtld-main-only (see dlsym man page) returns ptr or nil if not found.");
CL_DEFUN T_sp core__dlsym(Str_sp name, T_sp ohandle) {
  void *handle = NULL;
  if (ohandle.nilp()) {
    SIMPLE_ERROR(BF("Invalid ohandle passed -> nil"));
  } else if (Pointer_sp phandle = ohandle.asOrNull<Pointer_O>()) {
    handle = phandle->ptr();
  } else if (gc::IsA<Symbol_sp>(ohandle)) {
    Symbol_sp sym = ohandle.asOrNull<Symbol_O>();
    SYMBOL_EXPORT_SC_(KeywordPkg, rtld_default);
    SYMBOL_EXPORT_SC_(KeywordPkg, rtld_next);
    SYMBOL_EXPORT_SC_(KeywordPkg, rtld_self);
    SYMBOL_EXPORT_SC_(KeywordPkg, rtld_main_only);
    if (sym == kw::_sym_rtld_default) {
      handle = RTLD_DEFAULT;
    } else if (sym == kw::_sym_rtld_next) {
      handle = RTLD_NEXT;
#ifndef _TARGET_OS_LINUX
    } else if (sym == kw::_sym_rtld_self) //NOT PORTABLE TO LINUX
    {
      handle = RTLD_SELF;
    } else if (sym == kw::_sym_rtld_main_only) {
      handle = RTLD_MAIN_ONLY;
#endif
    } else {
      SIMPLE_ERROR(BF("Illegal keyword[%s] for dlsym - only :rtld-next :rtld-self :rtld-default :rtld-main-only are allowed") % _rep_(sym));
    }
  }
  string ts = name->get();
  void *ptr = dlsym(handle, ts.c_str());
  if (ptr == NULL) {
    return _Nil<T_O>();
  }
  return Pointer_O::create(ptr);
}

CL_LAMBDA(addr);
CL_DECLARE();
CL_DOCSTRING("(call dladdr with the address and return nil if not found or the contents of the Dl_info structure as multiple values)");
CL_DEFUN void core__call_dl_main_function(Pointer_sp addr) {
  InitFnPtr mainFunctionPointer = (InitFnPtr)addr->ptr();
  (*mainFunctionPointer)(LCC_PASS_ARGS0_VA_LIST_INITFNPTR());
}

CL_LAMBDA(addr);
CL_DECLARE();
CL_DOCSTRING("(call dladdr with the address and return nil if not found or the contents of the Dl_info structure as multiple values)");
CL_DEFUN T_mv core__dladdr(Integer_sp addr) {
  uint64_t val = clasp_to_uint64(addr);
  void *ptr = (void *)val;
  Dl_info info;
  int ret = dladdr(ptr, &info);
  if (!ret) {
    return Values(_Nil<T_O>());
  } else {
    return Values(Str_O::create(info.dli_fname),
                  Pointer_O::create(info.dli_fbase),
                  Str_O::create(info.dli_sname),
                  Pointer_O::create(info.dli_saddr));
  }
}

CL_LAMBDA(form &optional environment);
CL_DEFUN T_mv compiler__implicit_compile_hook_default(T_sp form, T_sp env) {
  // Convert the form into a thunk and return like COMPILE does
  LambdaListHandler_sp llh = LambdaListHandler_O::create(0);
  Cons_sp code = Cons_O::create(form, _Nil<T_O>());
  T_sp source_manager = _lisp->sourceDatabase();
  T_sp sourcePosInfo = _Nil<T_O>();
  if ( SourceManager_sp db = source_manager.asOrNull<SourceManager_O>() ) {
    sourcePosInfo = db->duplicateSourcePosInfo(form, code);
  }
  stringstream ss;
  ss << "repl" << _lisp->nextReplCounter();
  Symbol_sp name = _lisp->intern(ss.str());
  InterpretedClosure_sp ic =
    gc::GC<InterpretedClosure_O>::allocate(name, kw::_sym_function, llh, _Nil<T_O>(), _Nil<T_O>(), env, code, SOURCE_POS_INFO_FIELDS(sourcePosInfo));
  Function_sp thunk = ic;
  return eval::funcall(thunk);
};

#if 0
CL_LAMBDA(fn &rest args);
CL_DECLARE();
CL_DOCSTRING("applysPerSecond");
CL_DEFUN T_sp core__applys_per_second(T_sp fn, List_sp args) {
  LightTimer timer;
  int nargs = cl__length(args);
  ALLOC_STACK_VALUE_FRAME(frameImpl, frame, nargs);
  for (int pow = 0; pow < 16; ++pow) {
    int times = 1 << pow * 2;
    timer.reset();
    timer.start();
    // Fill frame here
    for (int i(0); i < times; ++i) {
      eval::applyLastArgsPLUSFirst(fn, args);
    }
    timer.stop();
    if (timer.getAccumulatedTime() > 0.1) {
      return DoubleFloat_O::create(((double)times) / timer.getAccumulatedTime());
    }
  }
  printf("%s:%d The function %s is too fast\n", __FILE__, __LINE__, _rep_(fn).c_str());
  return _Nil<T_O>();
}
#endif
};

extern "C" {
__attribute__((noinline)) int callByValue(core::T_sp v1, core::T_sp v2, core::T_sp v3, core::T_sp v4) {
  ASSERT(v1.fixnump());
  ASSERT(v2.fixnump());
  ASSERT(v3.fixnump());
  ASSERT(v4.fixnump());
  int f1 = v1.unsafe_fixnum();
  int f2 = v2.unsafe_fixnum();
  int f3 = v3.unsafe_fixnum();
  int f4 = v4.unsafe_fixnum();
  int res = f1 + f2 + f3 + f4;
  return res;
}

__attribute__((noinline)) int callByPointer(core::T_O *v1, core::T_O *v2, core::T_O *v3, core::T_O *v4) {
  ASSERT(gctools::tagged_fixnump(v1));
  ASSERT(gctools::tagged_fixnump(v2));
  ASSERT(gctools::tagged_fixnump(v3));
  ASSERT(gctools::tagged_fixnump(v4));
  int f1 = gctools::untag_fixnum(v1);
  int f2 = gctools::untag_fixnum(v2);
  int f3 = gctools::untag_fixnum(v3);
  int f4 = gctools::untag_fixnum(v4);
  int res = f1 + f2 + f3 + f4;
  return res;
}

__attribute__((noinline)) int callByConstRef(const core::T_sp &v1, const core::T_sp &v2, const core::T_sp &v3, const core::T_sp &v4) {
  ASSERT(v1.fixnump());
  ASSERT(v2.fixnump());
  ASSERT(v3.fixnump());
  ASSERT(v4.fixnump());
  int f1 = v1.unsafe_fixnum();
  int f2 = v2.unsafe_fixnum();
  int f3 = v3.unsafe_fixnum();
  int f4 = v4.unsafe_fixnum();
  int res = f1 + f2 + f3 + f4;
  return res;
}
};

namespace core {

#if 0
CL_LAMBDA(stage fn &rest args);
CL_DECLARE();
CL_DOCSTRING("globalFuncallCyclesPerSecond");
CL_DEFUN     T_sp core__global_funcall_cycles_per_second(int stage, Symbol_sp fn, List_sp args )
    {
        LightTimer timer;
        int nargs = cl__length(args);
        T_sp v1, v2, v3;
        T_O* rawArgs[64];
        int nargs = af_length(args);
        Cons_sp cur = args;
        for ( int i=0; i<nargs; ++i ) {
            rawArgs[i] = oCar(cur).raw_();
            cur=cCdr(cur);
        }
        ALLOC_STACK_VALUE_FRAME(frameImpl,frame,nargs);
        for ( int pow=0; pow<16; ++pow ) {
            int times = 1 << pow*2;  // the number of times to run the inner loop
            timer.reset();
            timer.start();   // Wrap a timer around the repeated inner loop
            T_sp cur = args;
            // Fill frame here
            for ( int i(0); i<times; ++i ) {
                // Compare to call by value
                callByValue(v1,v2,v3,v4);
                if ( stage>=1 ) {
                    closure = va_symbolFunction(&fn);
                    if ( stage>=2 ) {
                        switch (nargs) {
                        case 0:
                            mv_FUNCALL(&result_mv,closure,NULL,NULL,NULL);
                            break;
                        case 1:
                            mv_FUNCALL(&result_mv,closure,rawArg[0],NULL,NULL);
                            break;
                        case 2:
                            mv_FUNCALL(&result_mv,closure,rawArg[0],rawArg[1],NULL);
                            break;
                        case 3:
                            mv_FUNCALL(&result_mv,closure,rawArg[0],rawArg[1],rawArg[2]);
                            break;

                        mv_FUNCALL(&result_mv,closure,
                        int nargs = cl__length(args);
                        if ( stage>=3 ) { // This is expensive
                            ValueFrame_sp frame(ValueFrame_O::create_fill_numExtraArgs(nargs,_Nil<ActivationFrame_O>()));
                            if ( stage>=4 ) {
                                Cons_sp cur = args;
                                for ( int i=nargs; i<nargs; ++i ) {
                                    frame->operator[](i) = oCar(cur);
                                    cur=cCdr(cur);
                                }
                                if ( stage >= 5) {
                                    Closure* closureP = func->closure;
                                    ASSERTF(closureP,BF("In applyToActivationFrame the closure for %s is NULL") % _rep_(fn));
                                    eval::applyClosureToActivationFrame(closureP,frame);
                                }
                            }
                        }
                    }
                }
            }
            timer.stop();
            if ( timer.getAccumulatedTime() > 0.1 ) {
                return DoubleFloat_O::create(((double)times)/timer.getAccumulatedTime());
            }
        }
        printf("%s:%d The function %s is too fast\n", __FILE__, __LINE__, _rep_(fn).c_str());
        return _Nil<T_O>();
    }
#endif

#if 0
CL_LAMBDA(stage fn args);
CL_DECLARE();
CL_DOCSTRING("partialApplysPerSecond");
CL_DEFUN T_sp core__partial_applys_per_second(int stage, T_sp fn, List_sp args) {
  LightTimer timer;
  int nargs = cl__length(args);
  T_sp v1, v2, v3, v4;
  ALLOC_STACK_VALUE_FRAME(frameImpl, frame, nargs);
  for (int pow = 0; pow < 16; ++pow) {
    int times = 1 << pow * 2; // the number of times to run the inner loop
    timer.reset();
    timer.start(); // Wrap a timer around the repeated inner loop
    // Fill frame here
    for (int i(0); i < times; ++i) {
      // Compare to call by value
      callByValue(v1, v2, v3, v4);
      if (stage >= 1) {
#if 0
                    Function_O* func = reinterpret_cast<NamedFunction_O*>(fn.px_ref());
#else

        Function_sp func;
        func = eval::lookupFunction(fn, _Nil<T_O>());
#endif
        if (stage >= 2) {
#if 0 // This is the fastest alternative I can think of relative to cl__length()
                        int nargs;
                        if ( args.nilp() ) {
                            nargs = 0;
                        } else {
                            nargs = args->fastUnsafeLength();
                        }
#else
          int nargs = cl__length(args);
#endif
          if (stage >= 3) { // This is expensive
#if 1 // heap based frame
            ValueFrame_sp frame(ValueFrame_O::create_fill_numExtraArgs(nargs, _Nil<ActivationFrame_O>()));
            if (stage >= 4) {
              List_sp cur = args;
              for (int i = 0; i < nargs; ++i) {
                frame->operator[](i) = oCar(cur);
                cur = oCdr(cur);
              }
#endif
              if (stage >= 5) {
                Closure_sp closureP = func->closure;
                ASSERTF(closureP, BF("In applyToActivationFrame the closure for %s is NULL") % _rep_(fn));
                eval::applyClosureToActivationFrame(closureP, frame);
              }
            }
          }
        }
      }
    }
    timer.stop();
    if (timer.getAccumulatedTime() > 0.1) {
      return DoubleFloat_O::create(((double)times) / timer.getAccumulatedTime());
    }
  }
  printf("%s:%d The function %s is too fast\n", __FILE__, __LINE__, _rep_(fn).c_str());
  return _Nil<T_O>();
}
#endif

T_sp allocFixnum() {
  Fixnum_sp fn = make_fixnum(3);
  return fn;
}

void dynamicCastArg(T_sp a) {
  Cons_sp c = gc::As<Cons_sp>(a);
  (void)c; // suppress warning
}

void allocateValueFrame5() {
  ValueFrame_sp v = ValueFrame_O::create(5, _Nil<ActivationFrame_O>());
  (void)v;
}
#if 0
void allocateStackFrame5() {
  STACK_FRAME(buff, frame, 5);
}
#endif
 
Cons_sp consList5() {
  T_sp val = _Nil<T_O>();
  return Cons_O::createList(val, val, val, val, val);
};

T_sp bitOLogicWithObjects() {
  T_sp val = _lisp->_true();
  T_sp val2 = _Nil<T_O>();
  if (val2.nilp()) {
    return val;
  }
  return val2;
};

T_sp allocCons() {
  Cons_sp fn = Cons_O::create(_Nil<T_O>(),_Nil<T_O>());
  return fn;
}

T_sp lexicalFrameLookup(T_sp fr, int depth, int index) {
  ASSERT(fr.isA<ActivationFrame_O>());
  ActivationFrame_sp af = gctools::reinterpret_cast_smart_ptr<ActivationFrame_O>(fr);
  T_sp val = core::value_frame_lookup_reference(af, depth, index);
  return val;
}

#if 0
CL_LAMBDA(op &optional arg);
CL_DECLARE();
CL_DOCSTRING("operationsPerSecond");
CL_DEFUN T_mv core__operations_per_second(int op, T_sp arg) {
  gc::frame::Frame frame1(5);
  int val = 0;
  for (int i = 0; i < 5; ++i)
    frame1[i] = make_fixnum(++val).raw_();
  
  ALLOC_STACK_VALUE_FRAME(frameImpl2, frame2, 5);
  frame::SetParentFrame(frame2, frame1);
  T_O **values2 = frame::ValuesArray(frame1);
  for (int i = 0; i < 5; ++i)
    values2[i] = make_fixnum(++val).raw_();
  ALLOC_STACK_VALUE_FRAME(frameImpl3, frame3, 5);
  frame::SetParentFrame(frame3, frame2);
  T_O **values3 = frame::ValuesArray(frame1);
  for (int i = 0; i < 5; ++i)
    values3[i] = make_fixnum(++val).raw_();
  LightTimer timer;
  T_sp v1, v2, v3, v4;
  T_sp ocons = Cons_O::create(_Nil<T_O>(), _Nil<T_O>());
  int times = 0;
  for (int pow = 5; pow < 16; pow = pow + 2) {
    times = 1 << pow * 2; // the number of times to run the inner loop
    timer.reset();
    timer.start(); // Wrap a timer around the repeated inner loop
    // Fill frame here
    for (int i(0); i < times; ++i) {
      // Compare to call by value
      switch (op) {
      case 0:
        break;
      case 1: {
        callByValue(v1, v2, v3, v4);
        break;
      }
      case 2: {
        allocFixnum();
        break;
      }
      case 3: {
        dynamicCastArg(ocons);
        break;
      }
      case 4: {
        allocateValueFrame5();
        break;
      }
      case 5: {
        allocateStackFrame5();
        break;
      }
      case 6: {
        consList5();
        break;
      }
      case 7: {
        bitOLogicWithObjects();
        break;
      }
      case 8: {
        allocCons();
        break;
      }
      case 9: {
        lexicalFrameLookup(frame3, 0, 0);
        break;
      }
      case 10: {
        lexicalFrameLookup(frame3, 2, 0);
        break;
      }
      default:
        break;
      }
    }
    timer.stop();
    if (timer.getAccumulatedTime() > 0.5)
      break;
  }
  string name;
  switch (op) {
  case 0:
    name = "nothing";
    break;
  case 1:
    name = "callByValue-3args";
    break;
  case 2:
    name = "allocate fixnum on heap";
    break;
  case 3:
    name = "dynamic cast down";
    break;
  case 4:
    name = "alloc value frame on heap, 5 elements";
    break;
  case 5:
    name = "alloc stack frame, 5 elements";
    break;
  case 6:
    name = "cons list 5 elements";
    break;
  case 7:
    name = "logic with T and nil";
    break;
  case 8:
    name = "alloc/abandon cons on heap";
    break;
  case 9:
    name = "lexicalFrameLookup(frame3,0,0)";
    break;
  case 10:
    name = "lexicalFrameLookup(frame3,2,3)";
    break;
  default:
    return Values(_Nil<T_O>());
  }
  return Values(DoubleFloat_O::create(((double)times) / timer.getAccumulatedTime()), Str_O::create(name));
}
#endif
#if 0
CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("callsByValuePerSecond");
CL_DEFUN T_sp core__calls_by_value_per_second() {
  LightTimer timer;
  T_sp v1 = gc::make_tagged_fixnum<core::T_O>(1);
  T_sp v2 = gc::make_tagged_fixnum<core::T_O>(2);
  T_sp v3 = gc::make_tagged_fixnum<core::T_O>(3);
  T_sp v4 = gc::make_tagged_fixnum<core::T_O>(4);
  printf("%s:%d Starting %s\n", __FILE__, __LINE__, __FUNCTION__);
  int pow;
  int res;
  for (pow = 0; pow < 32; ++pow) {
    size_t times = 1 << pow * 2;
    timer.reset();
    timer.start();
    res = 0;
    for (size_t i = 0; i < times; ++i) {
      v1 = gc::make_tagged_fixnum<core::T_O>(i);
      res += callByValue(v1, v2, v3, v4);
    }
    timer.stop();
    if (timer.getAccumulatedTime() > 0.5) {
      printf("%s:%d return %s pow = %d res=%d \n", __FILE__, __LINE__, __FUNCTION__, pow, res);
      return DoubleFloat_O::create(((double)times) / timer.getAccumulatedTime());
    }
  }
  printf("%s:%d Fell through %s pow = %d res= %d \n", __FILE__, __LINE__, __FUNCTION__, pow, res);
  return _Nil<T_O>();
}
#endif
#if 0
CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("callsByConstantReferencePerSecond");
CL_DEFUN T_sp core__calls_by_constant_reference_per_second() {
  LightTimer timer;
  T_sp v1 = gc::make_tagged_fixnum<core::T_O>(1);
  T_sp v2 = gc::make_tagged_fixnum<core::T_O>(2);
  T_sp v3 = gc::make_tagged_fixnum<core::T_O>(3);
  T_sp v4 = gc::make_tagged_fixnum<core::T_O>(4);
  printf("%s:%d Starting %s\n", __FILE__, __LINE__, __FUNCTION__);
  int pow;
  int res;
  for (pow = 0; pow < 32; ++pow) {
    size_t times = 1 << pow * 2;
    timer.reset();
    timer.start();
    res = 0;
    for (size_t i = 0; i < times; ++i) {
      v1 = gc::make_tagged_fixnum<core::T_O>(i);
      res += callByConstRef(v1, v2, v3, v4);
    }
    timer.stop();
    if (timer.getAccumulatedTime() > 0.5) {
      printf("%s:%d return %s pow = %d res=%d \n", __FILE__, __LINE__, __FUNCTION__, pow, res);
      return DoubleFloat_O::create(((double)times) / timer.getAccumulatedTime());
    }
  }
  printf("%s:%d Fell through %s pow = %d res = %d\n", __FILE__, __LINE__, __FUNCTION__, pow, res);
  return _Nil<T_O>();
}
#endif
#if 0
CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("callsByPointerPerSecond");
CL_DEFUN T_sp core__calls_by_pointer_per_second() {
  LightTimer timer;
  T_sp v1 = gc::make_tagged_fixnum<core::T_O>(1);
  T_sp v2 = gc::make_tagged_fixnum<core::T_O>(2);
  T_sp v3 = gc::make_tagged_fixnum<core::T_O>(3);
  T_sp v4 = gc::make_tagged_fixnum<core::T_O>(4);
  printf("%s:%d Starting %s\n", __FILE__, __LINE__, __FUNCTION__);
  int pow, res;
  for (pow = 0; pow < 32; ++pow) {
    size_t times = 1 << pow * 2;
    timer.reset();
    timer.start();
    res = 0;
    for (size_t i = 0; i < times; ++i) {
      v1 = gc::make_tagged_fixnum<core::T_O>(i);
      res += callByPointer(v1.raw_(), v2.raw_(), v3.raw_(), v4.raw_());
    }
    timer.stop();
    if (timer.getAccumulatedTime() > 0.5) {
      printf("%s:%d return %s pow = %d res=%d \n", __FILE__, __LINE__, __FUNCTION__, pow, res);
      return DoubleFloat_O::create(((double)times) / timer.getAccumulatedTime());
    }
  }
  printf("%s:%d Fell through %s pow = %d res=%d \n", __FILE__, __LINE__, __FUNCTION__, pow, res);
  return _Nil<T_O>();
}
#endif

CL_LAMBDA(sym val thunk);
CL_DECLARE();
CL_DOCSTRING("callWithVariableBound");
CL_DEFUN T_mv core__call_with_variable_bound(Symbol_sp sym, T_sp val, T_sp thunk) {
  DynamicScopeManager scope(sym, val);
  return eval::funcall(thunk);
  // Don't put anything in here - don't mess up the MV return
}

CL_LAMBDA(protected-fn cleanup-fn);
CL_DECLARE();
CL_DOCSTRING("funwind_protect");
CL_DEFUN T_mv core__funwind_protect(T_sp protected_fn, T_sp cleanup_fn) {
  T_mv result;
  try {
#ifdef DEBUG_FLOW_CONTROL
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
      printf("%s:%d In funwind_protect try\n", __FILE__, __LINE__);
      printf("   %s\n", my_thread->exceptionStack().summary().c_str());
    }
#endif
    Closure_sp closure = protected_fn.asOrNull<core::Closure_O>();
    ASSERT(closure);
    MAKE_STACK_FRAME(frame,closure.raw_(),0);
    LCC_CALL_WITH_ARGS_IN_FRAME(result,closure,*frame);
  } catch (...) {
#ifdef DEBUG_FLOW_CONTROL
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
      printf("%s:%d In funwind_protect catch(...) just caught\n", __FILE__, __LINE__);
      printf("   %s\n", my_thread->exceptionStack().summary().c_str());
    }
#endif
// Save any return value that may be in the multiple value return array
    gctools::Vec0<T_sp> savemv;
    T_mv tresult;
    tresult.readFromMultipleValue0();
    tresult.saveToVec0(savemv);
    {
      Closure_sp closure = cleanup_fn.asOrNull<Closure_O>();
      ASSERT(closure);
      MAKE_STACK_FRAME(frame,closure.raw_(),0);
      LCC_CALL_WITH_ARGS_IN_FRAME(tresult,closure,*frame);
      // T_mv tresult = closure->invoke_va_list(LCC_PASS_ARGS0_VA_LIST(closure.raw_()));
    }
#if 1 // See comment above about 22a8d7b1
    tresult.loadFromVec0(savemv);
    tresult.saveToMultipleValue0();
#endif
#ifdef DEBUG_FLOW_CONTROL
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
      printf("%s:%d In funwind_protect catch(...)    about to rethrow\n", __FILE__, __LINE__);
      printf("   %s\n", my_thread->exceptionStack().summary().c_str());
    }
#endif
    throw;
  }
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("%s:%d In funwind_protect  normal exit\n", __FILE__, __LINE__);
    printf("   %s\n", my_thread->exceptionStack().summary().c_str());
  }
#endif
  gctools::Vec0<T_sp> savemv;
  result.saveToVec0(savemv);
  {
    T_mv tresult;
    Closure_sp closure = cleanup_fn.asOrNull<Closure_O>();
    ASSERT(closure);
    MAKE_STACK_FRAME(frame,closure.raw_(),0);
    LCC_CALL_WITH_ARGS_IN_FRAME(tresult,closure,*frame);
    //tresult = closure->invoke_va_list(LCC_PASS_ARGS0_VA_LIST(closure.raw_()));
  }
  result.loadFromVec0(savemv);
  return result;
}

CL_LAMBDA(function-designator &rest functions);
CL_DECLARE();
CL_DOCSTRING("multipleValueFuncall");
CL_DEFUN T_mv core__multiple_value_funcall(T_sp funcDesignator, List_sp functions) {
  Function_sp fmv = coerce::functionDesignator(funcDesignator);
  Closure_sp func = fmv.asOrNull<Closure_O>();
  ASSERT(func);
  MAKE_STACK_FRAME(frame, func.raw_(), MultipleValues::MultipleValuesLimit);
  size_t numArgs = 0;
  size_t idx = 0;
  MultipleValues& mv = lisp_multipleValues();
  for (auto cur : functions) {
    Function_sp func = gc::As<Function_sp>(oCar(cur));
    T_mv result = eval::funcall(func);
    ASSERT(idx < MultipleValues::MultipleValuesLimit);
    (*frame)[idx] = result.raw_();
    ++idx;
    for (size_t i = 1, iEnd(result.number_of_values()); i < iEnd; ++i) {
      ASSERT(idx < MultipleValues::MultipleValuesLimit);
      (*frame)[idx] = mv._Values[i];
      ++idx;
    }
  }
  frame->set_number_of_arguments(idx);
  T_mv result;
  LCC_CALL_WITH_ARGS_IN_FRAME(result, func, *frame);
  return result;
}

CL_LAMBDA(func1 func2);
CL_DECLARE();
CL_DOCSTRING("multipleValueProg1_Function - evaluate func1, save the multiple values and then evaluate func2 and restore the multiple values");
CL_DEFUN T_mv core__multiple_value_prog1_function(Function_sp func1, Function_sp func2) {
  MultipleValues mvFunc1;
  ASSERT((func1) && func1.notnilp());
  T_mv result = eval::funcall(func1);
  multipleValuesSaveToMultipleValues(result,&mvFunc1);
  eval::funcall(func2);
  return multipleValuesLoadFromMultipleValues(&mvFunc1);
}

CL_LAMBDA(tag func);
CL_DECLARE();
CL_DOCSTRING("catchFunction");
CL_DEFUN T_mv core__catch_function(T_sp tag, Function_sp thunk) {
  T_mv result;
  int frame = my_thread->exceptionStack().push(CatchFrame, tag);
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("%s:%d In cc_catch tag@%p thisFrame: %d\n", __FILE__, __LINE__, tag.raw_(), frame);
    printf("   %s\n", my_thread->exceptionStack().summary().c_str());
  }
#endif
  try {
    core::Closure_sp closure = thunk.asOrNull<Closure_O>();
    ASSERT(closure);
    MAKE_STACK_FRAME(frame,closure.raw_(),0);
    LCC_CALL_WITH_ARGS_IN_FRAME(result,closure,*frame);
    // result = closure->invoke_va_list(LCC_PASS_ARGS0_VA_LIST(closure.raw_()));
  } catch (CatchThrow &catchThrow) {
    if (catchThrow.getFrame() != frame) {
#ifdef DEBUG_FLOW_CONTROL
      if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
        printf("- - - - - Rethrowing CatchThrow targetFrame[%d] (thisFrame is: %d)\n", catchThrow.getFrame(), frame);
      }
#endif
      throw catchThrow;
    }
    result = gctools::multiple_values<T_O>::createFromValues();
  }
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("- - - - - Matched CatchThrow (thisFrame is: %d)\n", frame);
    printf("- - - - - Unwinding to thisFrame: %d\n", frame);
  }
#endif
  my_thread->exceptionStack().unwind(frame);
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("%s:%d  After cc_catch unwind\n", __FILE__, __LINE__);
    printf("   %s\n", my_thread->exceptionStack().summary().c_str());
  }
#endif
  return result;
}

CL_LAMBDA(tag result);
CL_DECLARE();
CL_DOCSTRING("throwFunction TODO: The semantics are not followed here - only the first return value is returned!!!!!!!!");
CL_DEFUN void core__throw_function(T_sp tag, T_sp result_form) {
  int frame = my_thread->exceptionStack().findKey(CatchFrame, tag);
  if (frame < 0) {
    CONTROL_ERROR();
  }
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("%s:%d In cc_throw     throwing CatchThrow to reach targetFrame[%d]\n", __FILE__, __LINE__, frame);
    printf("   %s\n", my_thread->exceptionStack().summary().c_str());
  }
#endif
  T_mv result;
  Closure_sp closure = result_form.asOrNull<Closure_O>();
  ASSERT(closure);
  MAKE_STACK_FRAME(frame0,closure.raw_(),0);
  LCC_CALL_WITH_ARGS_IN_FRAME(result,closure,*frame0);
  //result = closure->invoke_va_list(LCC_PASS_ARGS0_VA_LIST(closure.raw_()));
  result.saveToMultipleValue0();
  throw CatchThrow(frame);
}

CL_LAMBDA(symbols values func);
CL_DECLARE();
CL_DOCSTRING("progvFunction");
CL_DEFUN T_mv core__progv_function(List_sp symbols, List_sp values, Function_sp func) {
  DynamicScopeManager manager;
  for (auto curSym : symbols) {
    Symbol_sp symbol = gc::As<Symbol_sp>(oCar(curSym));
    T_sp value = oCar(values);
    manager.pushSpecialVariableAndSet(symbol, value);
    values = oCdr(values);
  }
  T_mv result = eval::funcall(func);
  return result;
}

 CL_DEFUN T_mv core__declared_global_inline_p(T_sp name)
 {
   return gc::As<HashTableEqual_sp>(_sym_STARfunctions_to_inlineSTAR->symbolValue())->gethash(name);
 }

  CL_DEFUN T_mv core__declared_global_notinline_p(T_sp name)
 {
   return gc::As<HashTableEqual_sp>(_sym_STARfunctions_to_notinlineSTAR->symbolValue())->gethash(name);
 }

 SYMBOL_EXPORT_SC_(CompPkg, STARimplicit_compile_hookSTAR);
 SYMBOL_EXPORT_SC_(CompPkg, implicit_compile_hook_default);
 SYMBOL_EXPORT_SC_(CompPkg, STARall_functions_for_one_compileSTAR);
 SYMBOL_SC_(CorePkg, dlopen);
 SYMBOL_SC_(CorePkg, dlsym);
 SYMBOL_SC_(CorePkg, dladdr);
  SYMBOL_EXPORT_SC_(CorePkg, callWithVariableBound);

void initialize_compiler_primitives(Lisp_sp lisp) {
  //	SYMBOL_SC_(CorePkg,processDeclarations);
  comp::_sym_STARimplicit_compile_hookSTAR->defparameter(comp::_sym_implicit_compile_hook_default->symbolFunction());
  cleavirPrimops::_sym_callWithVariableBound->setf_symbolFunction(_sym_callWithVariableBound->symbolFunction());
}

}; /* namespace */


        
