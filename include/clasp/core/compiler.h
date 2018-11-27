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
#ifndef _core_compiler_H_
#define _core_compiler_H_

#include <clasp/core/object.h>

namespace core {

T_sp varArgsList(int numArgs, ...);

T_sp core__startup_image_pathname();
T_mv core__load_bundle(T_sp pathDesig, T_sp verbose = _Nil<T_O>(), T_sp print = _Nil<T_O>(), T_sp external_format = kw::_sym_default);

T_mv compiler__implicit_compile_hook_default(T_sp form, T_sp env);

void initialize_compiler_primitives(Lisp_sp lisp);
};

namespace core {
  typedef void (*InitializerFunction)();
  void register_initializer_function(InitializerFunction fptr);
  size_t initializer_functions_are_waiting();
  void initializer_functions_invoke();

  /*! Register an void foo() function to be run once Clasp has initialized all of its core
functionality but before any Common Lisp startup functions are invoked. */
  struct Initializer {
    inline Initializer(InitializerFunction fn) {
      register_initializer_function(fn);
    }
  };


  size_t startup_functions_are_waiting();
  void startup_functions_invoke();


  std::tuple< void *, string > do_dlopen(const string& str_path, const int n_mode);
  std::tuple< int, string > do_dlclose(void * p_handle);
  std::tuple< void *, string > do_dlsym( void * p_handle, const char * pc_symbol );

};

typedef void*(*fnStartUp)();

namespace core {
  void core__throw_function(T_sp tag, T_sp result_form);
  void register_startup_function(fnStartUp fptr);
void register_internal_functions(uintptr_t handle, const claspFunction* funcs, const char** names, size_t len);
}

#endif /* _compiler_H_ */
