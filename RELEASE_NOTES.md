# Version 1.0.0 (LLVM13) 2022-03-26

* Add ed hooks functionality for `ed` function. Accessible via `*ed-functions*`
  dynamic variable.
* New compiled library format called FASP - it uses concatenated object files.
* Implemented `save-lisp-and-die`. This saves the state of a running environment 
  for loading and fast startup later. Our most complex environment Cando starts 
  up in ~4 seconds, which is 10x faster than the old startup that loaded 
  libraries.
* Atomics interface for lock-free concurrent programming, in the `mp:` package.
* Garbage collection hooks including finalizers.
* Specialized arrays for sub-byte integer types (int2, int4, etc.)
* Source tracking: Code locations are associated with source locations via
  DWARF, to aid in debugging and project navigation.
* `clasp-debug` interface so that IDEs like SLIME can retrieve backtraces and
  more to present during debugging.

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
