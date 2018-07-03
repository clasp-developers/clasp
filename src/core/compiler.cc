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
//#define DEBUG_LEVEL_FULL

#include <dlfcn.h>
#ifdef _TARGET_OS_DARWIN
#import <mach-o/dyld.h>
#endif

#include <clasp/core/foundation.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/core/cxxObject.h>
#include <clasp/core/record.h>
#include <clasp/core/lisp.h>
#include <clasp/core/environment.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/lightProfiler.h>
#include <clasp/core/designators.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/array.h>
#include <clasp/core/character.h>
#include <clasp/core/functor.h>
#include <clasp/core/compiler.h>
#include <clasp/core/sequence.h>
#include <clasp/core/debugger.h>
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
//  printf("%s:%d initializer_functions_are_waiting returning %" PRu "\n", __FILE__, __LINE__, global_initializer_count );
  return global_initializer_count;
};

/*! Invoke the initializer functions and clear the array of initializer functions */
void initializer_functions_invoke()
{
  if (global_initializer_count>0) {
#if 0
    printf("%s:%d In initializer_functions_invoke - there are %" PRu " initializer functions\n", __FILE__, __LINE__, global_initializer_count );
    for ( size_t i = 0; i<global_initializer_count; ++i ) {
      InitializerFunction fn = global_initializer_functions[i];
      printf("%s:%d     Initializer fn[%" PRu "]@%p\n", __FILE__, __LINE__, i, fn );
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
fnStartUp* global_startup_functions = NULL;

void register_startup_function(fnStartUp fptr)
{
#ifdef DEBUG_STARTUP
  printf("%s:%d In register_startup_function --> %p\n", __FILE__, __LINE__, fptr);
#endif
  if ( global_startup_functions == NULL ) {
    global_startup_capacity = STARTUP_FUNCTION_CAPACITY_INIT;
    global_startup_count = 0;
    global_startup_functions = (fnStartUp*)malloc(global_startup_capacity*sizeof(fnStartUp));
  } else {
    if ( global_startup_count == global_startup_capacity ) {
      global_startup_capacity = global_startup_capacity*STARTUP_FUNCTION_CAPACITY_MULTIPLIER;
      global_startup_functions = (fnStartUp*)realloc(global_startup_functions,global_startup_capacity*sizeof(fnStartUp));
    }
  }
  global_startup_functions[global_startup_count] = fptr;
  global_startup_count++;
};

/*! Return the number of startup_functions that are waiting to be run*/
size_t startup_functions_are_waiting()
{
#ifdef DEBUG_STARTUP
  printf("%s:%d startup_functions_are_waiting returning %" PRu "\n", __FILE__, __LINE__, global_startup_count );
#endif
  return global_startup_count;
};

/*! Invoke the startup functions and clear the array of startup functions */
void startup_functions_invoke()
{
  // Save the current list
  size_t startup_count = global_startup_count;
  fnStartUp* startup_functions = global_startup_functions;
  // Prepare to accumulate a new list
  global_startup_count = 0;
  global_startup_capacity = 0;
  global_startup_functions = NULL;
  // Invoke the current list
  if (startup_count>0) {
#ifdef DEBUG_STARTUP
    printf("%s:%d In startup_functions_invoke - there are %" PRu " startup functions\n", __FILE__, __LINE__, startup_count );
    for ( size_t i = 0; i<startup_count; ++i ) {
      fnStartUp fn = startup_functions[i];
      printf("%s:%d     Startup fn[%" PRu "]@%p\n", __FILE__, __LINE__, i, fn );
    }
    printf("%s:%d Starting to call the startup functions\n", __FILE__, __LINE__ );
#endif
    for ( size_t i = 0; i<startup_count; ++i ) {
      fnStartUp fn = startup_functions[i];
#ifdef DEBUG_STARTUP
      printf("%s:%d     About to invoke fn@%p\n", __FILE__, __LINE__, fn );
#endif
//      T_mv result = (fn)(LCC_PASS_MAIN());
      (fn)(); // invoke the startup function
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
         ":clasp-min,  :bclasp, :cclasp  -- Tells Clasp what stage it's in and where to get its init file.\n"
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


CL_DOCSTRING("Return the rdtsc performance timer value");
CL_DEFUN Fixnum core__rdtsc(){
    unsigned int lo,hi;
    __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
    return ((uint64_t)hi << 32) | lo;
}


CL_LAMBDA(pow2);
CL_DECLARE();
CL_DOCSTRING("Evaluate a TaggedCast 2^pow2 times");
CL_DEFUN Fixnum_sp core__test_tagged_cast(Fixnum_sp pow2) __attribute__((optnone)) {
  Fixnum fpow2 = pow2.unsafe_fixnum();
  Fixnum times = 1;
  times = times << fpow2;
  printf("%s:%d  fpow2 = %" PRF " times = %" PRF "\n", __FILE__, __LINE__, fpow2, times);
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
  long int freps = reps.unsafe_fixnum();
  long int fnum = num.unsafe_fixnum();
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
  return Integer_O::create((gctools::Fixnum)z);
}

T_sp varArgsList(int n_args, ...) {
  DEPRECATED();
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
  SimpleBaseString_sp name;
  if (!is_function) {
    if (sym.nilp())
      name = SimpleBaseString_O::make("CLASP_NIL");
    else if (sym == _lisp->_true())
      name = SimpleBaseString_O::make("CLASP_T");
    else {
      stringstream ss;
      ss << "SYM(" << sym->symbolName()->get() << ")";
      name = SimpleBaseString_O::make(ss.str());
    }
    return Values(_Nil<T_O>(), name, make_fixnum(0), make_fixnum(CALL_ARGUMENTS_LIMIT));
  }
  Function_sp fsym = coerce::functionDesignator(sym);
  if ( BuiltinClosure_sp  bcc = fsym.asOrNull<BuiltinClosure_O>()) {
    (void)bcc; // suppress warning
    return Values(_lisp->_true(), SimpleBaseString_O::make("Provide-c-func-name"), make_fixnum(0), make_fixnum(CALL_ARGUMENTS_LIMIT));
  }
  return Values(_Nil<T_O>(), SimpleBaseString_O::make("Provide-func-name"), make_fixnum(0), make_fixnum(CALL_ARGUMENTS_LIMIT));
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("startupImagePathname - returns a pathname based on *features* :CLASP-MIN, :USE-MPS, :BCLASP");
CL_DEFUN T_sp core__startup_image_pathname() {
  stringstream ss;
  ss << "app-fasl:cclasp-" << VARIANT_NAME << "-image";
  T_sp mode = core::_sym_STARclasp_build_modeSTAR->symbolValue();
  if (mode == kw::_sym_object) {
    ss << ".fasl";
  } else if (mode == kw::_sym_bitcode) {
    ss << ".fasl";
  } else if (mode == kw::_sym_fasl) {
    ss << ".lfasl";
  } else {
    SIMPLE_ERROR(BF("Add support for *clasp-build-mode* = %s") % _rep_(mode));
  }
  String_sp spath = SimpleBaseString_O::make(ss.str());
  Pathname_sp pn = cl__pathname(spath);
  return pn;
};

CL_LAMBDA(name &optional verbose print external-format);
CL_DECLARE();
CL_DOCSTRING("load-binary");
CL_DEFUN T_mv core__load_binary(T_sp pathDesig, T_sp verbose, T_sp print, T_sp external_format) {
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
  if (pathDesig.nilp()) SIMPLE_ERROR(BF("load-binary was about to pass nil to pathname"));
  Pathname_sp path = cl__pathname(pathDesig);
  if (cl__probe_file(path).notnilp())
    goto LOAD;
  path->_Type = SimpleBaseString_O::make("bundle");
  if (cl__probe_file(path).notnilp())
    goto LOAD;
  path->_Type = SimpleBaseString_O::make("fasl");
  if (cl__probe_file(path).notnilp())
    goto LOAD;
  path->_Type = SimpleBaseString_O::make("fasb"); // ECL uses fasb
  if (cl__probe_file(path).notnilp())
    goto LOAD;
  path->_Type = SimpleBaseString_O::make("dylib");
  if (cl__probe_file(path).notnilp())
    goto LOAD;
  path->_Type = SimpleBaseString_O::make("so");
  if (cl__probe_file(path).notnilp())
    goto LOAD;
  SIMPLE_ERROR(BF("Could not find bundle %s") % _rep_(pathDesig));
LOAD:
  String_sp nameStr = cl__namestring(cl__probe_file(path));
  string name = nameStr->get();

  /* Look up the initialization function. */
  string stem = cl__string_downcase(gc::As<String_sp>(path->_Name))->get();
  size_t dsp = 0;
  if ((dsp = stem.find("_d")) != string::npos)
    stem = stem.substr(0, dsp);
  else if ((dsp = stem.find("_o")) != string::npos)
    stem = stem.substr(0, dsp);

  int mode = RTLD_NOW | RTLD_GLOBAL; // | RTLD_FIRST;
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
    //    return (Values(_Nil<T_O>(), SimpleBaseString_O::make(error)));
  }
  _lisp->openDynamicLibraryHandles()[name] = handle;
  if (startup_functions_are_waiting()) {
    startup_functions_invoke();
  } else {
    SIMPLE_ERROR(BF("This is not a proper FASL file - there were no global ctors - there have to be global ctors for load-bundle"));
  }
  T_mv result;
  cc_invoke_startup_functions();
  return (Values(Pointer_O::create(handle), _Nil<T_O>()));
};

#ifdef EXPOSE_DLLOAD
CL_DOCSTRING("dlload - Open a dynamic library and evaluate the 'init_XXXX' extern C function. Returns (values returned-value error-message(or nil if no error)");
CL_DEFUN T_mv core__dlload(T_sp pathDesig) {
  string lib_extension;
#ifdef _TARGET_OS_DARWIN
  lib_extension = ".dylib";
#endif
#if defined( _TARGET_OS_LINUX) || defined( _TARGET_OS_FREEBSD)
  lib_extension = ".so";
#endif
  int mode = RTLD_NOW | RTLD_GLOBAL;
  Path_sp path = coerce::pathDesignator(pathDesig);
  Path_sp pathWithProperExtension = path->replaceExtension(lib_extension);
  string ts = pathWithProperExtension->asString();
  printf("%s:%d Loading with core__dlload %s\n", __FILE__, __LINE__, ts.c_str());
  void *handle = dlopen(ts.c_str(), mode);
  if (handle == NULL) {
    string error = dlerror();
    return (Values(_Nil<T_O>(), SimpleBaseString_O::make(error)));
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

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
std::tuple< void *, string > do_dlopen(const string& str_path, const int n_mode) {
  void * p_handle = nullptr;
  std::string str_error{ "" };

  dlerror(); // clear any previous error

  p_handle = dlopen( str_path.c_str(), n_mode );

  if ( ! p_handle ) {
    str_error = dlerror();
    fprintf( stderr, "%s:%d Could not open %s - error: %s\n",
             __FILE__, __LINE__, str_path.c_str(), str_error.c_str());
  }

  return std::make_tuple( p_handle, str_error );
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
std::tuple< int, string > do_dlclose(void * p_handle) {

  std::string str_error{ "" };
  int n_rc = 0;

  dlerror(); // clear any previous error

  if( ! p_handle ) {
    str_error = "Library handle is invalid (NULL/NIL)!";
    n_rc = -1;
  }
  else {
    n_rc = dlclose( p_handle );

    if ( n_rc != 0 ) {
      str_error = dlerror();
      fprintf( stderr, "%s:%d Could not close dynamic library (handle %p) - error: %s !\n",
             __FILE__, __LINE__, p_handle, str_error.c_str());
    }
  }

  return std::make_tuple( n_rc, str_error );
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
CL_DOCSTRING("dlopen - Open a dynamic library and return the handle. Returns (values returned-value error-message(or nil if no error))");
CL_DEFUN T_mv core__dlopen(T_sp pathDesig) {

  int mode = RTLD_NOW | RTLD_GLOBAL;
  
  if (pathDesig.nilp()) SIMPLE_ERROR(BF("%s was about to pass nil to pathname") % __FUNCTION__);
  Pathname_sp path = cl__pathname(pathDesig);
  string ts0 = gc::As<String_sp>(cl__namestring(path))->get_std_string();

  auto result = do_dlopen( ts0, mode );
  void * handle = std::get<0>( result );

  if( handle == nullptr ) {
    return (Values(_Nil<T_O>(), SimpleBaseString_O::make( get<1>( result ))));
  }
  return (Values(Pointer_O::create(handle), _Nil<T_O>()));
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
std::tuple< void *, string > do_dlsym( void * p_handle, const char * pc_symbol ) {
  std::string str_error{ "" };
  void *p_sym = nullptr;
  dlerror(); // clear any earlier error
  p_sym = dlsym( p_handle, pc_symbol );
  if( p_sym == nullptr ) {
    str_error = dlerror();
    fprintf( stderr, "%s:%d Could not get symbol address in dynamic library (handle %p) - error: %s !\n",
             __FILE__, __LINE__, p_handle, str_error.c_str() );
  }
  return std::make_tuple( p_sym, str_error );
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
CL_DOCSTRING("(dlsym handle name) handle is pointer from dlopen or :rtld-next, :rtld-self, :rtld-default or :rtld-main-only (see dlsym man page) returns ptr or nil if not found.");
CL_DEFUN T_sp core__dlsym(T_sp ohandle, String_sp name) {
  void *handle = NULL;
  if (ohandle.nilp()) {
    SIMPLE_ERROR(BF("Invalid ohandle passed -> nil"));
  } else if (Pointer_sp phandle = ohandle.asOrNull<Pointer_O>()) {
    handle = phandle->ptr();
  } else if (Symbol_sp sym = ohandle.asOrNull<Symbol_O>() ) {
//    printf("%s:%d handle is symbol: %s\n", __FILE__, __LINE__, _rep_(sym).c_str());
    SYMBOL_EXPORT_SC_(KeywordPkg, rtld_default);
    SYMBOL_EXPORT_SC_(KeywordPkg, rtld_next);
    SYMBOL_EXPORT_SC_(KeywordPkg, rtld_self);
    SYMBOL_EXPORT_SC_(KeywordPkg, rtld_main_only);
    if (sym == kw::_sym_rtld_default) {
//      printf("%s:%d handle is RDLD_DEFAULT\n", __FILE__, __LINE__ );
      handle = RTLD_DEFAULT;
//      printf("%s:%d RTLD_DEFAULT = %p\n", __FILE__, __LINE__, RTLD_DEFAULT);
//      printf("%s:%d handle = %p\n", __FILE__, __LINE__, handle);
    } else if (sym == kw::_sym_rtld_next) {
      handle = RTLD_NEXT;
#ifdef _TARGET_OS_DARWIN
    } else if (sym == kw::_sym_rtld_self) //NOT PORTABLE TO LINUX
    {
      handle = RTLD_SELF;
    } else if (sym == kw::_sym_rtld_main_only) {
      handle = RTLD_MAIN_ONLY;
#endif
    } else {
      SIMPLE_ERROR(BF("Illegal keyword[%s] for dlsym - only :rtld-next :rtld-self :rtld-default :rtld-main-only are allowed") % _rep_(sym));
    }
  } else {
    SIMPLE_ERROR(BF("Illegal handle argument[%s] for dlsym only a pointer or :rtld-next :rtld-self :rtld-default :rtld-main-only are allowed") % _rep_(ohandle));
  }
//  printf("%s:%d handle = %p\n", __FILE__, __LINE__, handle);
  string ts = name->get_std_string();
  auto result = do_dlsym(handle, ts.c_str());
  void * p_sym = std::get<0>( result );
  if( p_sym == nullptr ) {
    return ( Values(_Nil<T_O>(), SimpleBaseString_O::make( get<1>( result ))) );
  }
  return ( Values(Pointer_O::create( p_sym ), _Nil<T_O>()) );
}

CL_DOCSTRING("(call dladdr with the address and return nil if not found or the contents of the Dl_info structure as multiple values)");
CL_DEFUN void core__call_dl_main_function(Pointer_sp addr) {
  InitFnPtr mainFunctionPointer = (InitFnPtr)addr->ptr();
  (*mainFunctionPointer)(LCC_PASS_ARGS0_VA_LIST_INITFNPTR());
}

CL_DOCSTRING("(call dladdr with the address and return nil if not found or the contents of the Dl_info structure as multiple values)");
CL_DEFUN T_mv core__dladdr(Integer_sp addr) {
  uint64_t val = clasp_to_uint64(addr);
  void *ptr = (void *)val;
  Dl_info info;
  int ret = dladdr(ptr, &info);
  if (!ret) {
    return Values(_Nil<T_O>());
  } else {
    return Values(SimpleBaseString_O::make(info.dli_fname),
                  Pointer_O::create(info.dli_fbase),
                  SimpleBaseString_O::make(info.dli_sname),
                  Pointer_O::create(info.dli_saddr));
  }
}

CL_LAMBDA(form &optional env);
CL_DEFUN T_mv compiler__implicit_compile_hook_default(T_sp form, T_sp env) {
  // Convert the form into a thunk and return like COMPILE does
  LambdaListHandler_sp llh = LambdaListHandler_O::create(0);
  Cons_sp code = Cons_O::create(form, _Nil<T_O>());
  T_sp source_manager = _lisp->sourceDatabase();
  T_sp sourcePosInfo = _Nil<T_O>();
  stringstream ss;
  ss << "repl" << _lisp->nextReplCounter();
  Symbol_sp name = _lisp->intern(ss.str());
  ClosureWithSlots_sp ic = ClosureWithSlots_O::make_interpreted_closure(name,
                                                                        kw::_sym_function,
                                                                        _Nil<T_O>(),
                                                                        llh,
                                                                        _Nil<T_O>(),
                                                                        _Nil<T_O>(),
                                                                        code, env, SOURCE_POS_INFO_FIELDS(sourcePosInfo));
  Function_sp thunk = ic;
  return (thunk->entry.load())(LCC_PASS_ARGS0_ELLIPSIS(thunk.raw_()));
//  return eval::funcall(thunk);
};

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

T_sp allocFixnum() {
  Fixnum_sp fn = make_fixnum(3);
  return fn;
}

void dynamicCastArg(T_sp a) {
  Cons_sp c = gc::As<Cons_sp>(a);
  (void)c; // suppress warning
}

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

CL_LAMBDA(sym val thunk);
CL_DECLARE();
CL_DOCSTRING("callWithVariableBound");
CL_DEFUN T_mv core__call_with_variable_bound(Symbol_sp sym, T_sp val, T_sp thunk) {
  DynamicScopeManager scope(sym, val);
  Function_sp func = gc::As_unsafe<Function_sp>(thunk);
  return (func->entry.load())(LCC_PASS_ARGS0_ELLIPSIS(func.raw_()));
  // Don't put anything in here - don't mess up the MV return
}

}


extern "C" {
LCC_RETURN call_with_variable_bound(core::T_O* tsym, core::T_O* tval, core::T_O* tthunk) {
  core::Symbol_sp sym((gctools::Tagged)tsym);
  core::T_sp val((gctools::Tagged)tval);
  core::Function_sp func((gctools::Tagged)tthunk);
  core::DynamicScopeManager scope(sym, val);
  return (func->entry.load())(LCC_PASS_ARGS0_ELLIPSIS(func.raw_()));
}

};

namespace core {

// try/catch approach does work
CL_DEFUN T_mv core__funwind_protect(T_sp protected_fn, T_sp cleanup_fn) {
  T_mv result;
  try {
    Closure_sp closure = gc::As_unsafe<Closure_sp>(protected_fn);
    ASSERT(closure);
    result = closure->entry.load()(LCC_PASS_ARGS0_ELLIPSIS(closure.raw_()));
  }
  catch (...)
  {
#if 0
    void* primary_exception = __cxxabiv1::__cxa_current_primary_exception();
    printf("%s:%d  primary_exception = %p\n", __FILE__, __LINE__, primary_exception );
    __cxxabiv1::__cxa_decrement_exception_refcount(primary_exception);
#endif
// Save any return value that may be in the multiple value return array
    gctools::Vec0<T_sp> savemv;
    T_mv tresult;
    tresult.readFromMultipleValue0();
    tresult.saveToVec0(savemv);
    {
      Closure_sp closure = gc::As_unsafe<Closure_sp>(cleanup_fn);
      tresult = closure->entry.load()(LCC_PASS_ARGS0_ELLIPSIS(closure.raw_()));
    }
    tresult.loadFromVec0(savemv);
    tresult.saveToMultipleValue0();
    throw;  // __cxa_rethrow
  }
  gctools::Vec0<T_sp> savemv;
  result.saveToVec0(savemv);
  {
    T_mv tresult;
    Closure_sp closure = gc::As_unsafe<Closure_sp>(cleanup_fn);
    tresult = closure->entry.load()(LCC_PASS_ARGS0_ELLIPSIS(closure.raw_()));
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
    T_mv result = (func->entry.load())(LCC_PASS_ARGS0_ELLIPSIS(func.raw_()));
//    T_mv result = eval::funcall(func);
    ASSERT(idx < MultipleValues::MultipleValuesLimit);
    if (result.number_of_values() > 0  ) {
        (*frame)[idx] = result.raw_();
        ++idx;
        for (size_t i = 1, iEnd(result.number_of_values()); i < iEnd; ++i) {
          ASSERT(idx < MultipleValues::MultipleValuesLimit);
          (*frame)[idx] = mv._Values[i];
          ++idx;
        }
      }
  }
  frame->set_number_of_arguments(idx);
  Vaslist valist_s(frame);
  VaList_sp args(&valist_s);
  return funcall_consume_valist_<Closure_O>(func.tagged_(),args);
}

CL_LAMBDA(func1 func2);
CL_DECLARE();
CL_DOCSTRING("multipleValueProg1_Function - evaluate func1, save the multiple values and then evaluate func2 and restore the multiple values");
CL_DEFUN T_mv core__multiple_value_prog1_function(Function_sp first_func, Function_sp second_func) {
  MultipleValues mvFunc1;
  ASSERT((first_func) && first_func.notnilp());
  T_mv result = (first_func->entry.load())(LCC_PASS_ARGS0_ELLIPSIS(first_func.raw_()));
  //  T_mv result = eval::funcall(first_func);
  multipleValuesSaveToMultipleValues(result,&mvFunc1);
  (second_func->entry.load())(LCC_PASS_ARGS0_ELLIPSIS(second_func.raw_()));
  // eval::funcall(func2);
  return multipleValuesLoadFromMultipleValues(&mvFunc1);
}

CL_LAMBDA(tag func);
CL_DECLARE();
CL_DOCSTRING("catchFunction");
CL_DEFUN T_mv core__catch_function(T_sp tag, Function_sp thunk) {
  T_mv result;
  int frame = my_thread->exceptionStack().push(CatchFrame, tag);
  try {
    core::Closure_sp closure = thunk.asOrNull<Closure_O>();
    ASSERT(closure);
    result = closure->entry.load()(LCC_PASS_ARGS0_ELLIPSIS(closure.raw_()));
  } catch (CatchThrow &catchThrow) {
    if (catchThrow.getFrame() != frame) {
      throw catchThrow;
    }
    result = gctools::multiple_values<T_O>::createFromValues();
  }
  my_thread->exceptionStack().unwind(frame);
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
  T_mv result;
  Closure_sp closure = result_form.asOrNull<Closure_O>();
  ASSERT(closure);
  result = closure->entry.load()(LCC_PASS_ARGS0_ELLIPSIS(closure.raw_()));
  result.saveToMultipleValue0();
#ifdef DEBUG_TRACK_UNWINDS
  global_CatchThrow_count++;
#endif
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
  T_mv result = (func->entry.load())(LCC_PASS_ARGS0_ELLIPSIS(func.raw_()));
  // T_mv result = eval::funcall(func);
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

  // Initialize raw object translators needed for Foreign Language Interface support 
  llvmo::initialize_raw_translators(); // See file intrinsics.cc!

  comp::_sym_STARimplicit_compile_hookSTAR->defparameter(comp::_sym_implicit_compile_hook_default->symbolFunction());
  cleavirPrimops::_sym_callWithVariableBound->setf_symbolFunction(_sym_callWithVariableBound->symbolFunction());

  return;
}

}; /* namespace */


extern "C" {

void clasp_trap(core::T_O* val) {
  printf("%s:%d  clasp_trap called with %p\n", __FILE__, __LINE__, val);
}

void clasp_silent_trap(core::T_O* obj) {
  // Do nothing
}

};
