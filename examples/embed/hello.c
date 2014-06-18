#include <stdio.h>
#include <ecl/ecl.h>

int main (int argc, char **argv) {
  /* Initialize ECL */
  cl_boot(argc, argv);

  /* Initialize the library we linked in. Each library
   * has to be initialized. It is best if all libraries
   * are joined using ASDF:MAKE-BUILD.
   */
  extern void init_lib_HELLO_LISP(cl_object);
  ecl_init_module(NULL, init_lib_HELLO_LISP);

  cl_eval(c_string_to_object("(hello-lisp)"));

  cl_shutdown();
  return 0;
}
