#pragma once
/*
    File: compiler.h
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

#include <clasp/core/object.h>
#include <clasp/core/bundle.h>
#include <chrono> // for MaybeDebugStartup timing

namespace llvmo {
FORWARD(ClaspJIT);
}

namespace core {

bool startup_snapshot_is_stale(const std::string& snapshotFileName);
T_sp core__startup_image_pathname(bool extension);
T_mv core__load_binary_directory(T_sp pathDesig, T_sp verbose = nil<T_O>(), T_sp print = nil<T_O>(),
                                 T_sp external_format = kw::_sym_default);
T_mv core__load_binary(T_sp pathDesig, T_sp verbose = nil<T_O>(), T_sp print = nil<T_O>(), T_sp external_format = kw::_sym_default);

T_mv compiler__implicit_compile_hook_default(T_sp form, T_sp env);
llvmo::ClaspJIT_sp compiler__jit_engine();

void initialize_compiler_primitives(LispPtr lisp);

void core__jit_register_symbol(const std::string& name, size_t size, void* address);

}; // namespace core

namespace core {
typedef void (*InitializerFunction)();
void register_initializer_function(InitializerFunction fptr);
size_t initializer_functions_are_waiting();
void initializer_functions_invoke();

/*! Register an void foo() function to be run once Clasp has initialized all of its core
functionality but before any Common Lisp startup functions are invoked. */
struct Initializer {
  inline Initializer(InitializerFunction fn) { register_initializer_function(fn); }
};

size_t startup_functions_are_waiting();
void startup_functions_invoke(T_O* literals_or_null);

std::tuple<void*, string> do_dlopen(const string& str_path, const int n_mode);
std::tuple<int, string> do_dlclose(void* p_handle);
std::tuple<void*, string> do_dlsym(void* p_handle, const char* pc_symbol);

}; // namespace core

namespace core {
struct MaybeDebugStartup {
  bool started;
  std::chrono::time_point<std::chrono::steady_clock> start;
  std::string name;
  size_t start_jit_compile_counter;
  void* fptr;
  size_t start_dispatcher_count;
  MaybeDebugStartup(void* fp, const char* n = NULL);
  ~MaybeDebugStartup();
};

void core__update_max_jit_compile_counter(size_t val);
size_t core__get_jit_compile_counter();
size_t core__next_jit_compile_counter();

}; // namespace core

namespace core {
void expect_offset(T_sp key, T_sp alist, size_t expected);
};

namespace core {
void start_code_interpreter(gctools::GCRootsInModule* roots, char* bytecode, size_t nbytes, bool log);
void core__throw_function(T_sp tag, T_sp result_form);
void register_startup_function(const StartUp& startup);
void transfer_StartupInfo_to_my_thread();
T_mv core__startup_linkage_shutdown_names(size_t id = 0, core::T_sp prefix = nil<core::T_O>());
void clasp_unpack_faso(const std::string& path_designator);
void startup_shutdown_names(size_t id, const std::string& prefix, std::string& start, std::string& shutdown);
extern bool global_jit_log_symbols;
} // namespace core
