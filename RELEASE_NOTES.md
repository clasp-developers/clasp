# Version 2.0.0 Pending (LLVM14)

## Added
* Lisp based koga metabuilder that outputs Ninja build files.
* basic Debian packaging files.
* `core:*extension-systems*`, `core:*initialize-hooks*` and
  `core:*terminate-hooks` dynamic variables have been added to support new
  extension loading method. `core:*extension-systems*` is a list of keywords
  that name extension systems to load after Clasp starts and before `--load`
  and `--eval` command line options are processed. The remaining two variables
  are lists of functions that are called to do initialization before a REPL is
  started and termination after the REPL exits.
* `--script <file>` command line option which equivalent to passing `--norc`,
  `--noinform` and `--non-interactive`. Any shebang in `<file>` will also be 
  skipped.
* Asynchronous external process control with `ext:run-program`.
* Function `ext:temporary-directory` that returns the directory used for
  temporary files.
* Function `ext:printing-char-p` that returns non-NIL for graphic characters
  that are not blank glyphs. This is an extension of the ANSI specification
  that defines "printing" characters as graphic characters aside from the space
  character.

## Changed
* `core:lisp-implementation-id` and `core:clasp-git-full-commit` only return
  non-`NIL` values if Clasp was built in a git working tree.
* `graphic-char-p`, `alpha-char-p`, `alphanumericp`, `upper-case-p`, 
  `lower-case-p`, `both-case-p`, `char-upcase` and `char-downcase` now no longer 
  depend on C++ locale functions and are now generated directly from the Unicode 
  character tables.
* Loading of extensions such as Cando no longer uses startup scripts via LOAD.
  Instead the systems associated with each extension are loaded via QL:QUICKLOAD
  or as a fallback ASDF:LOAD-SYSTEM.
* Behavior of `--rc` command line option has changed. Relative paths passed via
  this option are no longer assumed to be located in the user's home directory.
* The logical hosts used by Clasp to locate source code and other components of
  Clasp has been changed. Only the reserved logical host SYS is now used. The
  default mappings for a system installed to `/usr/` are
  1. `SYS:LIB;**;*.*.*` ↦ `/usr/lib/clasp/**/*.*`
  2. `SYS:GENERATED;**;*.*.*` ↦ `/usr/share/clasp/generated/**/*.*`
  3. `SYS:EXECUTABLE;**;*.*.*` ↦ `/usr/bin/**/*.*`
  4. `SYS:QUICKLISP;**;*.*.*` ↦ `~/quicklisp/**/*.*`
  5. `SYS:**;*.*.*` ↦ `/usr/share/clasp/**/*.*`
* ASDF systems that are loaded as part the cclasp image are now marked as
  immutable thereby preventing ASDF from overwriting them. These systems include
  the systems acclimation, alexandria, clasp-cleavir, cleavir-ast-to-bir, 
  cleavir-ast, cleavir-ast-transformations, cleavir-attributes, cleavir-bir, 
  cleavir-bir-transformations, cleavir-compilation-policy, cleavir-conditions,
  cleavir-cst-to-ast, cleavir-ctype, cleavir-environment, 
  cleavir-io,cleavir-meter, cleavir-primop, cleavir-set, cleavir-stealth-mixins, 
  closer-mop, concrete-syntax-tree, concrete-syntax-tree-base,
  concrete-syntax-tree-destructuring, concrete-syntax-tree-lambda-list,
  eclector, and eclector-concrete-syntax-tree.
* Source code file references for Lisp and C/C++ files compiled as part of the
  Clasp binary or images are now stored using logical pathnames.

## Removed
* `core:*extensions-startup-loads*` and `core:*extensions-startup-evals*`
  dynamic variables have been removed since they are no longer used.
* The `--resources-dir` command line option has been removed. Equivalent
  behavior is achieved with the `CLASP_HOME` environment variable.

## Enhancements
* `make-instance` and CLOS slot access functions can be used with structure 
  objects.
* The stepper, accessible through `step`, now has basic functionality.
* `gctools:save-lisp-and-die` now accepts a key `:executable` which can be used
  to create an executable binary with the snapshot embedded in the binary.
* `garbage-collect`, `finalize`, and `save-lisp-and-die` are now exported from
  the `ext` interface package.

## Optimizations
* Arguments to and return values from local functions (e.g. from FLET) are 
  passed unboxed in some common cases.
* Nonlocal exits are much faster in most cases, the exception being when
  the exit goes through uncooperative C++ code.
* Types inferred for many standard functions are tighter.
* Calls to some local functions with &rest parameters are more efficient.
* LENGTH is now a "vaslistable" function; &rest parameters that are only
  used for vaslistable functions can be compiled to avoid consing.
* Multiple value calls and APPLY calls to known functions can sometimes
  be optimized.
* Some MAKE-ARRAY calls are compiled more efficiently.
* Unused calls to many more (side-effect-free) standard functions are deleted.
* Accesses to 1D simple arrays of known element type are a bit faster.
* A virtual machine has been defined and implemented, greatly increasing the
  speed of evaluation of code that doesn't usually need to be optimized
  (for example, compile-time evaluations).
* The building process has been streamlined by replacing several of the
  bootstrapping components with the virtual machine.
* Discriminating functions now execute faster.
* The compiler now performs inlining much faster.

## Fixes
* Replace hard coded paths to `nm` in snapshot code with NM_BINARY macro value
  set by configure.
* Clasp can now be built directly from source. Resolves issue [#175][].
* Snapshots now parse command line options such as `--noinform`, `--noprint`,
  `--quit`, and `--disable-debugger`.
* Source locations for warnings from errors during constant folding now
  print correctly.
* Unused calls that must remain in safe code are no longer deleted.
* Prevent negative zero remainder in core__next_ftruncate. Fixes [#1368][].

# Version 1.0.0 (LLVM13) 2022-03-26

## Added
* ed hooks functionality for `ed` function. Accessible via `*ed-functions*`
  dynamic variable.
* Implemented `save-lisp-and-die`. This saves the state of a running environment 
  for loading and fast startup later. Our most complex environment Cando starts 
  up in ~4 seconds, which is 10x faster than the old startup that loaded 
  libraries.
* Atomics interface for lock-free concurrent programming, in the `mp:` package.
* Garbage collection hooks including finalizers.
* `clasp-debug` interface so that IDEs like SLIME can retrieve backtraces and
  more to present during debugging.

## Enhancements
* New compiled library format called FASP - it uses concatenated object files.
* Specialized arrays for sub-byte integer types (int2, int4, etc.)
* Source tracking: Code locations are associated with source locations via
  DWARF, to aid in debugging and project navigation.
* Fully integrated the customizable reader [ECLECTOR][]

## Fixes
* Fixed many errors identified by the ansi-test-suite [ANSI][]

# Version 0.5.0

* Full integration of the Cleavir/[SICL][] compiler by Robert Strandh
* A nascent type inference system within Cleavir by Alex Wood
* LLVM whole program, Link-Time-Optimization
* Multi threading compatible with Bordeaux threads
* Unicode, 32bit wide characters
* Specialized arrays for many primitive types (int8, int16, int32, int64, 
  double, float)
* CFFI (C Foreign Function Interface)
* New build system based on waf (https://github.com/waf-project/waf)
* Code and file cleanup
* Better Emacs and Slime support including jumping to source (C++ and CL), 
  tab-completion and lambda-list display
* Many, many improvements and bug fixes

# Version 0.4.0

* Clasp has a completely new compiler! The Cleavir Common Lisp compiler 
  developed by Robert Strandh as part of his SICL project. Cleavir is an 
  advanced compiler that generates fast code and is an excellent compiler for 
  testing advanced optimization algorithms. Cleavir carries out escape analysis 
  to properly assign variables to the stack/heap and it does aggressive 
  inlining. The best code that Cleavir generates has clocked within a factor of 
  four of the speed of C. For more details see [SICL][]
* Fixnum, character and single-float types are now immediate values and fixnums 
  are tagged using `#b000` in the bottom three bits to allow fast integer 
  arithmetic.
* General object pointers are tagged, cons cells have a separate tag to 
  accelerate are tagged.
* Building Clasp has been accelerated and now takes about 2.5 hours on a 
  reasonably powerful Linux system and 3.5 hours on OS X.
* Clbind library allows programmers to expose external C++ libraries to Clasp 
  Common Lisp.
* The build system has been improved and externals-clasp has been eliminated as 
  a requirement.
* Clasp version 0.4.0 is a Boehm only version of Clasp. The Memory Pool System 
  garbage collector is being tuned for Clasp and is planned to be in the 0.5.0 
  release.

# Version 0.11

* Added ASDF support. This is still alpha.  Compile the ASDF module using 
  `(core:compile-asdf)`. After that you can load the module using 
  `(load "sys:kernel;asdf;build;asdf.bundle")`. It takes between 15-30 seconds 
  to load (this is why I'm integrating Cleavir).
* Added `:CLASP` to `*features*` and removed `:ECL` from `*features*`. Clasp 
  will continue to mimic the underlying ECL functionality so that Common Lisp 
  code that supports ECL can be made to support Clasp by converting `#+ecl` to 
  `#+(or ecl clasp)` and `#-ecl` to `#-(or ecl clasp)`
* Added code to generate object files directly from Clasp. The LLVM bitcode 
  compiler "llc" no longer needs to be in the PATH for Clasp to generate object 
  files from Common Lisp source. The "ld" linker does need to be accessible.

[SICL]: https://github.com/robert-strandh/SICL
[ANSI]: https://gitlab.common-lisp.net/ansi-test/ansi-test
[ECLECTOR]: https://github.com/s-expressionists/Eclector
[#175]: https://github.com/clasp-developers/clasp/issues/175
[#1368]: https://github.com/clasp-developers/clasp/issues/1368
