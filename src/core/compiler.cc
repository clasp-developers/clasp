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
// #define DEBUG_LEVEL_FULL

#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <dlfcn.h>
#ifdef _TARGET_OS_DARWIN
#import <mach-o/dyld.h>
#endif

#include <clasp/core/foundation.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/gctools/snapshotSaveLoad.h>
#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/core/cxxObject.h>
#include <clasp/core/record.h>
#include <clasp/core/lisp.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/sort.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/lightProfiler.h>
#include <clasp/core/designators.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/array.h>
#include <clasp/core/character.h>
#include <clasp/core/function.h>
#include <clasp/core/compiler.h>
#include <clasp/core/sequence.h>
#include <clasp/core/debugger.h>
#include <clasp/core/pathname.h>
#include <clasp/core/pointer.h>
#include <clasp/core/unixfsys.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/cleavirPrimopsPackage.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/bytecode_compiler.h>
#include <clasp/core/pointer.h>
#include <clasp/llvmo/intrinsics.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/jit.h>
#include <clasp/llvmo/code.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/unwind.h> // funwind_protect

#define FASO_VERSION 1

namespace core {

std::atomic<size_t> global_jit_compile_counter;
std::atomic<size_t> global_jit_unique_counter;

DOCGROUP(clasp);
CL_DEFUN size_t core__get_jit_compile_counter() { return global_jit_compile_counter.load(); }

DOCGROUP(clasp);
CL_DEFUN void core__update_max_jit_compile_counter(size_t val) {
  if (val < global_jit_compile_counter.load())
    return;
  size_t expected;
  do {
    expected = global_jit_compile_counter.load();
  } while (!global_jit_compile_counter.compare_exchange_weak(expected, val));
  //  printf("%s:%d:%s Set max_jit_compile_counter to %lu\n", __FILE__, __LINE__, __FUNCTION__, val );
}

DOCGROUP(clasp);
CL_DEFUN size_t core__next_jit_compile_counter() { return ++global_jit_compile_counter; }

DOCGROUP(clasp);
CL_DEFUN size_t core__next_jit_unique_counter() { return ++global_jit_unique_counter; }

MaybeDebugStartup::MaybeDebugStartup(void* fp, const char* n) : fptr(fp), start_dispatcher_count(0) {
  if (n)
    this->name = n;
  this->start_jit_compile_counter = global_jit_compile_counter;
  if (core::_sym_STARdebugStartupSTAR->symbolValue().notnilp()) {
    this->started = true;
    this->start = std::chrono::steady_clock::now();
    if (clos::_sym_dispatcher_count->fboundp()) {
      core::T_sp nu = core::eval::funcall(clos::_sym_dispatcher_count);
      this->start_dispatcher_count = nu.unsafe_fixnum();
    } else {
      this->start_dispatcher_count = 0;
    }
  } else
    this->started = false;
};

NEVER_OPTIMIZE MaybeDebugStartup::~MaybeDebugStartup() {
  if (core::_sym_STARdebugStartupSTAR->symbolValue().notnilp() && this->started) {
    auto end = std::chrono::steady_clock::now();
    auto us = std::chrono::duration_cast<std::chrono::microseconds>(end - this->start);
    size_t end_dispatcher_count = 0;

    if (clos::_sym_dispatcher_count->fboundp()) {
      core::T_sp nu = core::eval::funcall(clos::_sym_dispatcher_count);
      end_dispatcher_count = nu.unsafe_fixnum();
    }
    size_t dispatcher_delta = end_dispatcher_count - this->start_dispatcher_count;
    stringstream name_;
    if (this->name != "")
      name_ << this->name << " ";
    Dl_info di;
    int found = dladdr((void*)this->fptr, &di);
    if (found && di.dli_sname) {
      name_ << di.dli_sname;
    } else {
      name_ << "NONAME";
    }
    if (name_.str() == "")
      name_ << (void*)this->fptr;
    printf("%s us %zu gfds %zu jits: %s\n", _rep_(Integer_O::create(us.count())).c_str(), dispatcher_delta,
           (global_jit_compile_counter - this->start_jit_compile_counter), name_.str().c_str());
  }
}

}; // namespace core

namespace core {

#define INITIALIZER_CAPACITY_INIT 128
#define INITIALIZER_CAPACITY_MULTIPLIER 2
size_t global_initializer_capacity = 0;
size_t global_initializer_count = 0;
InitializerFunction* global_initializer_functions = NULL;

void register_initializer_function(InitializerFunction fptr) {
  //  printf("%s:%d In register_initializer_function --> %p\n", __FILE__, __LINE__, fptr);
  if (global_initializer_functions == NULL) {
    global_initializer_capacity = INITIALIZER_CAPACITY_INIT;
    global_initializer_count = 0;
    global_initializer_functions = (InitializerFunction*)malloc(global_initializer_capacity * sizeof(InitializerFunction));
  } else {
    if (global_initializer_count == global_initializer_capacity) {
      global_initializer_capacity = global_initializer_capacity * INITIALIZER_CAPACITY_MULTIPLIER;
      global_initializer_functions =
          (InitializerFunction*)realloc(global_initializer_functions, global_initializer_capacity * sizeof(InitializerFunction));
    }
  }
  global_initializer_functions[global_initializer_count] = fptr;
  global_initializer_count++;
};

/*! Return the number of initializer_functions that are waiting to be run*/
size_t initializer_functions_are_waiting() {
  //  printf("%s:%d initializer_functions_are_waiting returning %" PRu "\n", __FILE__, __LINE__, global_initializer_count );
  return global_initializer_count;
};

/*! Invoke the initializer functions and clear the array of initializer functions */
void initializer_functions_invoke() {
  if (global_initializer_count > 0) {
#if 0
    printf("%s:%d In initializer_functions_invoke - there are %" PRu " initializer functions\n", __FILE__, __LINE__, global_initializer_count );
    for ( size_t i = 0; i<global_initializer_count; ++i ) {
      InitializerFunction fn = global_initializer_functions[i];
      printf("%s:%d     Initializer fn[%" PRu "]@%p\n", __FILE__, __LINE__, i, fn );
    }
    printf("%s:%d Starting to call the initializer functions\n", __FILE__, __LINE__ );
#endif
    for (size_t i = 0; i < global_initializer_count; ++i) {
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

// These functions are used to access a runtime module's literals vector.
// The vector is a T_sp[]. This could possibly be done with some normal
// CFFI accessor instead.

CL_DEFUN T_sp core__literals_vref(Pointer_sp lvec, size_t index) {
  return ((T_sp*)(lvec->ptr()))[index];
}

CL_LISPIFY_NAME("core:literals_vref");
CL_DEFUN_SETF T_sp core__literals_vset(T_sp val, Pointer_sp lvec, size_t index)
{
  ((T_sp*)(lvec->ptr()))[index] = val;
  return val;
}

// Get the address of the redirect function with the given arity.
// This is used by clasp-cleavir to construct functions.
CL_DEFUN Pointer_sp core__xep_redirect_address(size_t arity) {
  switch (arity) {
  case 0: return Pointer_O::create((void*)general_entry_point_redirect_0);
  case 1: return Pointer_O::create((void*)general_entry_point_redirect_1);
  case 2: return Pointer_O::create((void*)general_entry_point_redirect_2);
  case 3: return Pointer_O::create((void*)general_entry_point_redirect_3);
  case 4: return Pointer_O::create((void*)general_entry_point_redirect_4);
  case 5: return Pointer_O::create((void*)general_entry_point_redirect_5);
  case 6: return Pointer_O::create((void*)general_entry_point_redirect_6);
  case 7: return Pointer_O::create((void*)general_entry_point_redirect_7);
  default: SIMPLE_ERROR("BUG: Invalid arity for redirect: %zu", arity);
  }
}

}; // namespace core

namespace core {

StartupInfo global_startup;

std::atomic<size_t> global_next_startup_position;

DOCGROUP(clasp);
CL_DEFUN size_t core__next_startup_position() { return global_next_startup_position++; }

/*! Static initializers will run and try to register startup functions.
They will do this into the global_startup structure.
Once the main code starts up - this startup info needs to be transfered
to the my_thread thread-local storage.
So right after the my_thread variable is initialized, this must be called for
the main thread. */

void transfer_StartupInfo_to_my_thread() {
  if (!my_thread) {
    printf("%s:%d my_thread is NULL - you cannot transfer StartupInfo\n", __FILE__, __LINE__);
  }
  my_thread->_Startup = global_startup;
}

void register_startup_function(const StartUp& one_startup) {
#ifdef DEBUG_STARTUP
  printf("%s:%d In register_startup_function type: %d at %p\n", __FILE__, __LINE__, one_startup._Type, one_startup._Function);
#endif
  StartupInfo* startup = NULL;
  // if my_thread is defined - then use its startup info
  // otherwise use the global_startup info.
  // This will only happen in cclasp when startup functions
  // are registered from static constructors.
  if (my_thread) {
    startup = &my_thread->_Startup;
  } else {
    startup = &global_startup;
  }
  if (startup->_functions == NULL) {
    startup->_capacity = STARTUP_FUNCTION_CAPACITY_INIT;
    startup->_count = 0;
    startup->_functions = (StartUp*)malloc(startup->_capacity * sizeof(StartUp));
  } else {
    if (startup->_count == startup->_capacity) {
      startup->_capacity = startup->_capacity * STARTUP_FUNCTION_CAPACITY_MULTIPLIER;
      startup->_functions = (StartUp*)realloc(startup->_functions, startup->_capacity * sizeof(StartUp));
    }
  }
  startup->_functions[startup->_count] = one_startup;
  startup->_count++;
};

/*! Return the number of startup_functions that are waiting to be run*/
size_t startup_functions_are_waiting() {
#ifdef DEBUG_STARTUP
  fmt::print("{}:{} startup_functions_are_waiting returning {}\n", __FILE__, __LINE__, my_thread->_Startup._count);
#endif
  return my_thread->_Startup._count;
};

/*! Invoke the startup functions and clear the array of startup functions */
void startup_functions_invoke(T_O* literals) {
  size_t startup_count = 0;
  StartUp* startup_functions = NULL;
  {
    // Save the current list
    startup_count = my_thread->_Startup._count;
    startup_functions = my_thread->_Startup._functions;
    // Prepare to accumulate a new list
    my_thread->_Startup._count = 0;
    my_thread->_Startup._capacity = 0;
    my_thread->_Startup._functions = NULL;
  }
  // Invoke the current list
  core::T_O* result = NULL;
  if (startup_count > 0) {
    sort::quickSortMemory(startup_functions, 0, startup_count);
#ifdef DEBUG_STARTUP
    printf("%s:%d In startup_functions_invoke - there are %" PRsize_t " startup functions\n", __FILE__, __LINE__, startup_count);
    for (size_t i = 0; i < startup_count; ++i) {
      StartUp& startup = startup_functions[i];
      printf("%s:%d     Startup fn[%" PRsize_t "] -> %p\n", __FILE__, __LINE__, startup._Position, startup._Function);
    }
    printf("%s:%d Starting to call the startup functions\n", __FILE__, __LINE__);
#endif
    StartUp previous;
    for (size_t i = 0; i < startup_count; ++i) {
      StartUp& startup = startup_functions[i];
      if (startup._Position == previous._Position) {
        printf("%s:%d At startup there were two adjacent startup functions with the same position value %lu - this could mean a "
               "startup order catastrophe\n",
               __FILE__, __LINE__, startup._Position);
      }
      previous = startup;
      switch (startup._Type) {
      case StartUp::T_O_function:
        ((T_OStartUp)startup._Function)(literals); // invoke the startup function
        break;
      case StartUp::void_function:
        ((voidStartUp)startup._Function)();
        printf("%s:%d:%s Returning NULL startup_functions_invoke\n", __FILE__, __LINE__, __FUNCTION__);
      }
    }
#ifdef DEBUG_STARTUP
    printf("%s:%d Done with startup_functions_invoke()\n", __FILE__, __LINE__);
#endif
    free(startup_functions);
  }
}

} // namespace core

extern "C" {

NOINLINE void start_dtrace() {
  printf("%s:%d start_dtrace\n", __FILE__, __LINE__);
  // Do nothing
};

NOINLINE void stop_dtrace() {
  printf("%s:%d stop_dtrace\n", __FILE__, __LINE__);
  // do nothing
}
}

namespace core {

DOCGROUP(clasp);
NOINLINE CL_DEFUN T_mv core__trigger_dtrace_start(T_sp closure) {
  start_dtrace();
  return eval::funcall(closure);
}

DOCGROUP(clasp);
NOINLINE CL_DEFUN T_sp core__trigger_dtrace_stop() {
  stop_dtrace();
  return nil<T_O>();
}

}; // namespace core

extern "C" {
gctools::return_type wrapped_test(core::T_O* arg0, core::T_O* arg1, core::T_O* arg2) {
  printf("%s:%d wrapped_test called (%p, %p, %p)\n", __FILE__, __LINE__, arg0, arg1, arg2);
  return gctools::return_type();
}
};
namespace core {

int f(Environment_sp& e) {
  (void)e;
  return 1;
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(Print info about booting)dx");
DOCGROUP(clasp);
CL_DEFUN void core__help_booting() {
  printf("Useful *features*\n"
         ":clasp-min,  :bclasp, :cclasp  -- Tells Clasp what stage it's in and where to get its init file.\n"
         ":notify-on-compile (core:*notify-on-compile*) - prints messages whenever COMPILE is invoked at startup\n"
         ":trace-startup (core:*trace-startup*) - prints messages and timing for running the main function of the compiled code of "
         "each system file at startup\n"
         ":debug-startup (core:*debug-startup*) - prints a message and timing for running each top level function\n"
         "\n"
         "Commands (all in CORE package)\n"
         "(load-system <start> <end> &key interp (system *init-files*))   - Load the system files\n"
         "(compile-min) - Compile a minimal system\n"
         "(compile-full) - Compile a full system\n"
         "(compile-kernel-file filename &key reload load-bitcode recompile)   - Compile a system file and put the bitcode in the "
         "correct directory\n"
         "(link-system start end prologue-form epilogue-form &key (system *init-files*)) - Link an image together\n"
         "(default-prologue-form &optional features) - Returns a prologue form for link-system\n"
         "(default-epilogue-form) - Returns an epilogue form for link-system\n");
}

CL_DOCSTRING(R"dx(Return the rdtsc performance timer value)dx");
DOCGROUP(clasp);
CL_DEFUN Fixnum core__rdtsc() {
#if defined(__i386__) || defined(__x86_64__)
  unsigned int lo, hi;
  __asm__ __volatile__("rdtsc" : "=a"(lo), "=d"(hi));
  return ((uint64_t)hi << 32) | lo;
#else
  SIMPLE_ERROR("No support for RDTSC on this architecture");
#endif
}

CL_LAMBDA(object &optional is-function);
CL_DECLARE();
CL_DOCSTRING(R"dx(mangleName)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__mangle_name(Symbol_sp sym, bool is_function) {
  SimpleBaseString_sp name;
  if (!is_function) {
    if (sym.nilp())
      name = SimpleBaseString_O::make("CLASP_NIL");
    else if (sym == _lisp->_true())
      name = SimpleBaseString_O::make("CLASP_T");
    else {
      stringstream ss;
      ss << "SYM(" << sym->symbolName()->get_std_string() << ")";
      name = SimpleBaseString_O::make(ss.str());
    }
    return Values(nil<T_O>(), name, make_fixnum(0), make_fixnum(CALL_ARGUMENTS_LIMIT));
  }
  return Values(nil<T_O>(), SimpleBaseString_O::make("Provide-func-name"), make_fixnum(0), make_fixnum(CALL_ARGUMENTS_LIMIT));
}

bool startup_snapshot_is_stale(const std::string& snapshotFileName) {
  stringstream ss;
  std::string executablePath;
  core::executablePath(executablePath);
  std::filesystem::path executable(executablePath);
  std::filesystem::path snapshot(snapshotFileName);
  if (!std::filesystem::exists(snapshot))
    return true;
  return (std::filesystem::last_write_time(executable) > std::filesystem::last_write_time(snapshot));
};

CL_LAMBDA("&optional (stage #\\c)");
CL_DECLARE();
CL_DOCSTRING(R"dx(startupImagePathname - returns a pathname based on *features* :CLASP-MIN, :USE-MPS, :BCLASP)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__startup_image_pathname(bool extension) {
  stringstream ss;
  ss << "sys:lib;images;" << (extension ? "extension" : "base");
  T_sp mode = comp::_sym_STARdefault_output_typeSTAR->symbolValue();
  if (mode == kw::_sym_faso) {
    ss << ".nfasl"; // ss << ".faso";
  } else if (mode == kw::_sym_bytecode) {
    ss << ".fasl";
  } else {
    SIMPLE_ERROR("Add support for *default-output-type* = {}", _rep_(mode));
  }
  String_sp spath = SimpleBaseString_O::make(ss.str());
  Pathname_sp pn = cl__pathname(spath);
  return pn;
};

int global_jit_pid = -1;
FILE* global_jit_log_stream = NULL;
bool global_jit_log_symbols = false;

void jit_register_symbol(const std::string& name, size_t size, void* address) {
  WITH_READ_WRITE_LOCK(globals_->_JITLogMutex);
  int gpid = getpid();
  if (global_jit_log_stream && (global_jit_pid != gpid)) {
    fclose(global_jit_log_stream);
    global_jit_log_stream = NULL;
    global_jit_pid = -1;
  }
  if (global_jit_pid == -1) {
    global_jit_pid = gpid;
    stringstream filename;
    filename << "/tmp/perf-" << gpid << ".map";
    global_jit_log_stream = fopen(filename.str().c_str(), "w");
  }
  if (global_jit_log_stream) {
    char nameBuffer[1024];
    char* namecur = nameBuffer;
    char prevchar = ' ';
    for (int i = 0; i < name.size() && i < 1023; i++) {
      if (name[i] == '\r')
        continue;
      if (name[i] == '\n')
        continue;
      if (name[i] < 32 && name[i] == prevchar)
        continue;
      *namecur = name[i];
      prevchar = name[i];
      namecur++;
    }
    *namecur = '\0';
    fprintf(global_jit_log_stream, "%0lx %lx %s\n", (uintptr_t)address, size, nameBuffer);
    fflush(global_jit_log_stream);
  }
}

CL_DEFUN void core__jit_register_symbol(const std::string& name, size_t size, void* address) {
  if (global_jit_log_symbols) {
    jit_register_symbol(name, size, address);
  }
}

void startup_shutdown_names(size_t id, const std::string& prefix, std::string& start, std::string& shutdown) {
  stringstream sstart;
  stringstream sshutdown;
  if (prefix != "") {
    sstart << prefix << "-";
    sshutdown << prefix << "-";
  }
  sstart << clasp_startup_FUNCTION_NAME << "_" << id;
  sshutdown << clasp_shutdown_FUNCTION_NAME << "_" << id;
  start = sstart.str();
  shutdown = sshutdown.str();
}

CL_DOCSTRING(R"dx(Return the startup function name and the linkage based on the current dynamic environment)dx");
CL_DOCSTRING_LONG(R"dx(The name contains the id as part of itself. Return (values startup-name linkage shutdown-name).)dx");
DOCGROUP(clasp);
CL_LAMBDA(&optional (id 0) prefix);
CL_DEFUN T_mv core__startup_linkage_shutdown_names(size_t id, core::T_sp tprefix) {
  std::string prefix;
  if (gc::IsA<String_sp>(tprefix)) {
    prefix = gc::As<String_sp>(tprefix)->get_std_string();
  } else if (tprefix.notnilp()) {
    SIMPLE_ERROR("Illegal prefix for startup function name: {}", _rep_(tprefix));
  }
  std::string start;
  std::string shutdown;
  startup_shutdown_names(id, prefix, start, shutdown);
  Symbol_sp linkage_type = llvmo::_sym_ExternalLinkage;
  return Values(core::SimpleBaseString_O::make(start), linkage_type, core::SimpleBaseString_O::make(shutdown));
};

DOCGROUP(clasp);
CL_LAMBDA(&optional (id 0) prefix);
CL_DEFUN T_mv core__startup_linkage(size_t id, core::T_sp prefix) {
  T_mv result = core__startup_linkage_shutdown_names(id, prefix);
  T_sp result1 = result;
  MultipleValues& mvn = core::lisp_multipleValues();
  T_sp result2 = mvn.second(result.number_of_values());
  return Values(result1, result2);
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
std::tuple<void*, string> do_dlopen(const string& str_path, const int n_mode) {
  void* p_handle = nullptr;
  std::string str_error{""};

  dlerror(); // clear any previous error

  p_handle = dlopen(str_path.c_str(), n_mode);

  if (!p_handle) {
    str_error = dlerror();
    // fprintf( stderr, "%s:%d Could not open %s - error: %s\n", __FILE__, __LINE__, str_path.c_str(), str_error.c_str());
  }

  return std::make_tuple(p_handle, str_error);
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
std::tuple<int, string> do_dlclose(void* p_handle) {

  std::string str_error{""};
  int n_rc = 0;

  dlerror(); // clear any previous error

  if (!p_handle) {
    str_error = "Library handle is invalid (NULL/NIL)!";
    n_rc = -1;
  } else {
    n_rc = dlclose(p_handle);

    if (n_rc != 0) {
      str_error = dlerror();
      fprintf(stderr, "%s:%d Could not close dynamic library (handle %p) - error: %s !\n", __FILE__, __LINE__, p_handle,
              str_error.c_str());
    }
  }

  return std::make_tuple(n_rc, str_error);
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
CL_DOCSTRING(R"dx(dlopen - Open a dynamic library and return the handle.)dx");
CL_DOCSTRING_LONG(R"dx(Returns (values returned-value error-message(or nil if no error)))dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__dlopen(T_sp pathDesig) {

  int mode = RTLD_NOW | RTLD_GLOBAL;

  if (pathDesig.nilp())
    SIMPLE_ERROR("{} was about to pass nil to pathname", __FUNCTION__);
  Pathname_sp path = cl__pathname(pathDesig);
  string ts0 = gc::As<String_sp>(cl__namestring(path))->get_std_string();

  auto result = do_dlopen(ts0, mode);
  void* handle = std::get<0>(result);

  if (handle == nullptr) {
    return (Values(nil<T_O>(), SimpleBaseString_O::make(get<1>(result))));
  }
  return (Values(Pointer_O::create(handle), nil<T_O>()));
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
std::tuple<void*, string> do_dlsym(void* p_handle, const char* pc_symbol) {
  std::string str_error{""};
  void* p_sym = nullptr;
  dlerror(); // clear any earlier error
             //  printf("%s:%d:%s  pc_symbol: %s\n", __FILE__, __LINE__, __FUNCTION__, pc_symbol );
  p_sym = dlsym(p_handle, pc_symbol);
  if (p_sym == nullptr) {
    str_error = dlerror();
  }
  if (global_options->_ExportedSymbolsCheck) {
    maybe_register_symbol_using_dladdr(p_sym);
  }
  return std::make_tuple(p_sym, str_error);
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
CL_DOCSTRING(
    R"dx((dlsym handle name) handle is pointer from dlopen or :rtld-next, :rtld-self, :rtld-default or :rtld-main-only (see dlsym man page) returns ptr or nil if not found.)dx")
DOCGROUP(clasp);
CL_DEFUN T_sp core__dlsym(T_sp ohandle, String_sp name) {
  void* handle = NULL;
  if (ohandle.nilp()) {
    SIMPLE_ERROR("Invalid ohandle passed -> nil");
  } else if (Pointer_sp phandle = ohandle.asOrNull<Pointer_O>()) {
    handle = phandle->ptr();
  } else if (Symbol_sp sym = ohandle.asOrNull<Symbol_O>()) {
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
    } else if (sym == kw::_sym_rtld_self) { // NOT PORTABLE TO LINUX
      handle = RTLD_SELF;
    } else if (sym == kw::_sym_rtld_main_only) {
      handle = RTLD_MAIN_ONLY;
#endif
    } else {
      SIMPLE_ERROR("Illegal keyword[{}] for dlsym - only :rtld-next :rtld-self :rtld-default :rtld-main-only are allowed",
                   _rep_(sym));
    }
  } else {
    SIMPLE_ERROR(
        ("Illegal handle argument[%s] for dlsym only a pointer or :rtld-next :rtld-self :rtld-default :rtld-main-only are allowed"),
        _rep_(ohandle));
  }
  string ts = name->get_std_string();

  //  printf("%s:%d:%s  handle = %p  symbol = |%s|\n", __FILE__, __LINE__, __FUNCTION__, handle, ts.c_str());
  auto result = do_dlsym(handle, ts.c_str());
  void* p_sym = std::get<0>(result);
  if (p_sym == nullptr) {
    return (Values(nil<T_O>(), SimpleBaseString_O::make(get<1>(result))));
  }
  return (Values(Pointer_O::create(p_sym), nil<T_O>()));
}

CL_DOCSTRING(
    R"dx((call dladdr with the address and return nil if not found or the contents of the Dl_info structure as multiple values))dx")
DOCGROUP(clasp);
CL_DEFUN void core__call_dl_main_function(Pointer_sp addr) {
  ClaspXepGeneralFunction mainFunctionPointer = (ClaspXepGeneralFunction)addr->ptr();
  (*mainFunctionPointer)(nil<core::T_O>().raw_(), 0, NULL);
}

CL_DOCSTRING(
    R"dx((call dladdr with the address and return nil if not found or the contents of the Dl_info structure as multiple values))dx")
DOCGROUP(clasp);
CL_DEFUN T_mv core__dladdr(Pointer_sp addr) {
  uint64_t val = (uint64_t)addr->ptr();
  void* ptr = (void*)val;
  Dl_info info;
  int ret = dladdr(ptr, &info);
  if (!ret) {
    return Values(nil<T_O>());
  } else {
    if (info.dli_sname != NULL && info.dli_saddr != NULL) {
      return Values(SimpleBaseString_O::make(info.dli_fname), Pointer_O::create(info.dli_fbase),
                    SimpleBaseString_O::make(info.dli_sname), Pointer_O::create(info.dli_saddr));
    } else {
      return Values(SimpleBaseString_O::make(info.dli_fname), Pointer_O::create(info.dli_fbase), SimpleBaseString_O::make("NULL"),
                    Pointer_O::create(info.dli_saddr));
    }
  }
}

}; // namespace core

namespace core {

CL_LAMBDA(symbol value thunk);
CL_DECLARE();
CL_DOCSTRING(R"dx(Call THUNK with the given SYMBOL bound to to the given VALUE.)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__call_with_variable_bound(Symbol_sp sym, T_sp val, Function_sp thunk) {
  DynamicScopeManager scope(sym, val);
  return thunk->funcall();
}

} // namespace core

namespace core {

CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN T_mv core__funwind_protect(T_sp protected_fn, T_sp cleanup_fn) {
  return funwind_protect([&]() { return eval::funcall(protected_fn); }, [&]() { eval::funcall(cleanup_fn); });
}

CL_LAMBDA(function &rest thunks);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(multipleValueFuncall)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__multiple_value_funcall(Function_sp fmv, List_sp thunks) {
  MAKE_STACK_FRAME(frame, MultipleValues::MultipleValuesLimit);
  size_t idx = 0;
  for (auto cur : thunks) {
    Function_sp tfunc = gc::As<Function_sp>(oCar(cur));
    auto result = tfunc->funcall();
    ASSERT(idx < MultipleValues::MultipleValuesLimit);
    gctools::fill_frame_multiple_value_return(frame, idx, result);
  }
  return fmv->apply_raw(idx, frame->arguments(0));
}

CL_LAMBDA(tag func);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(catchFunction)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__catch_function(T_sp tag, Function_sp thunk) {
  return call_with_catch(tag, [&]() { return eval::funcall(thunk); });
}

CL_LAMBDA(tag result);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(Like CL:THROW, but takes a thunk)dx");
DOCGROUP(clasp);
CL_DEFUN void core__throw_function(T_sp tag, Function_sp result_form) {
  T_mv result = eval::funcall(result_form);
  MultipleValues& mv = lisp_multipleValues();
  mv.saveToMultipleValue0(result);
  sjlj_throw(tag);
}

CL_LAMBDA(symbols values func);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(progvFunction)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__progv_function(List_sp symbols, List_sp values, Function_sp func) {
  return fprogv(symbols, values, [&]() { return eval::funcall(func); });
}

CL_DEFUN T_mv core__progv_env_function(T_sp env, List_sp symbols, List_sp values,
                                       Function_sp thunk) {
  return fprogv_env(env, symbols, values, [&]() { return eval::funcall(thunk); });
}

DOCGROUP(clasp);
CL_DEFUN T_mv core__declared_global_inline_p(T_sp name) {
  return gc::As<HashTable_sp>(_sym_STARfunctions_to_inlineSTAR->symbolValue())->gethash(name);
}

DOCGROUP(clasp);
CL_DEFUN T_mv core__declared_global_notinline_p(T_sp name) {
  return gc::As<HashTable_sp>(_sym_STARfunctions_to_notinlineSTAR->symbolValue())->gethash(name);
}

DOCGROUP(clasp);
CL_DEFUN T_sp core__run_function(T_sp object) {
  std::string name = gc::As<String_sp>(object)->get_std_string();
  T_sp thandle = _sym_STARcurrent_dlopen_handleSTAR->symbolValue();
  uintptr_t handle = 0;
  if (thandle.notnilp() && gc::IsA<Pointer_sp>(thandle)) {
    handle = (uintptr_t)gc::As_unsafe<Pointer_sp>(thandle)->ptr();
  }
  ClaspXepGeneralFunction func = (ClaspXepGeneralFunction)dlsym((void*)handle, name.c_str());
//  printf("%s:%d:%s running function %s  at %p\n", __FILE__, __LINE__, __FUNCTION__, name.c_str(), (void*)func);
#ifdef DEBUG_SLOW
  MaybeDebugStartup startup((void*)func);
#endif
  if (func != nullptr) {
    LCC_RETURN ret = func(nil<T_O>().raw_(), 0, NULL);
    core::T_sp res((gctools::Tagged)ret.ret0[0]);
    core::T_sp val = res;
    return val;
  }
  return nil<T_O>();
}

DOCGROUP(clasp);
CL_DEFUN T_sp core__make_builtin_class(T_sp object) {
  printf("%s:%d:%s  with %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(object).c_str());
  SIMPLE_ERROR("Add support for core__make_builtin_class");
}

DOCGROUP(clasp);
CL_DEFUN T_sp core__handle_creator(T_sp object) { SIMPLE_ERROR("Handle-creator for {}", _rep_(object).c_str()); }

SYMBOL_SC_(CorePkg, dlopen);
SYMBOL_SC_(CorePkg, dlsym);
SYMBOL_SC_(CorePkg, dladdr);
SYMBOL_EXPORT_SC_(CorePkg, callWithVariableBound);

void initialize_compiler_primitives(LispPtr lisp) {

  // Initialize raw object translators needed for Foreign Language Interface support
  llvmo::initialize_raw_translators(); // See file intrinsics.cc!

  cleavirPrimop::_sym_callWithVariableBound->setf_symbolFunction(_sym_callWithVariableBound->symbolFunction());
  comp::_sym_STARcodeWalkerSTAR->defparameter(nil<T_O>());
  comp::_sym_STARsourceLocationsSTAR->makeSpecial();
  comp::_sym_STARbtb_compile_hookSTAR->defparameter(nil<T_O>());
  comp::_sym_STARautocompile_hookSTAR->defparameter(nil<T_O>());
  {
    Fixnum_sp one = clasp_make_fixnum(1);
    comp::_sym_STARoptimizeSTAR->defparameter(
        Cons_O::createList(Cons_O::createList(cl::_sym_compilation_speed, one), Cons_O::createList(cl::_sym_debug, one),
                           Cons_O::createList(cl::_sym_space, one), Cons_O::createList(cl::_sym_speed, one),
                           Cons_O::createList(cl::_sym_safety, one)));
  }
}

}; // namespace core

namespace core {

std::atomic<int> global_sigchld_count{0};

DOCGROUP(clasp);
CL_DEFUN int core__sigchld_count() { return global_sigchld_count; }

DOCGROUP(clasp);
CL_DEFUN int core__decf_sigchld_count() {
  global_sigchld_count--;
  return global_sigchld_count;
}

void sigchld(int signal) { global_sigchld_count++; }

DOCGROUP(clasp);
CL_DEFUN void core__install_sigchld() { signal(SIGCHLD, sigchld); }

DOCGROUP(clasp);
CL_DEFUN void core__uninstall_sigchld() { signal(SIGCHLD, SIG_DFL); }

DOCGROUP(clasp);
CL_DEFUN void core__call4(int x, int y, int z, int w) {
  printf("%s:%d call4 args: %d, %d, %d, %d\n", __FILE__, __LINE__, x, y, z, w);
}

/* Match the offset in the alist to the expected offset
 */
void expect_offset(T_sp key, T_sp alist, size_t expected) {
  List_sp pair = core__alist_assoc_eq(alist, key);
  if (pair.nilp()) {
    SIMPLE_ERROR("Could not find key {} in alist {}", _rep_(key), _rep_(alist));
  }
  T_sp value = CONS_CDR(pair);
  if (!value.fixnump()) {
    SIMPLE_ERROR("The value {} in alist {} at key {} must be a fixnum", _rep_(value), _rep_(alist), _rep_(key));
  }
  if (value.unsafe_fixnum() != expected) {
    SIMPLE_ERROR("The value {} in alist {} at key {} must match the C++ tagged offset of {}", _rep_(value), _rep_(alist),
                 _rep_(key), expected);
  }
}

}; // namespace core

SYMBOL_EXPORT_SC_(CorePkg, two_arg_STAR);

extern "C" {

void clasp_trap(core::T_O* val) { printf("%s:%d  clasp_trap called with %p\n", __FILE__, __LINE__, val); }

void clasp_silent_trap(core::T_O* obj) {
  // Do nothing
}
};
