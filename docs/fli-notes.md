CLASP FOREIGN LANGUAGE INTERFACE  (FLI)
=======================================

    Document Revision .... : X.01.01
    Document Status ...... : IN WORK
    Document Maturity Level: DRAFT - NOT FOR PRODUCTION USE !

### Revision History
(newest first):

    X.01.01: 2016-12-02: DRAFT, IN WORK - committed to dev-cffi for preview / review.
    X.01.00: 2016-12-01: DRAFT, IN WORK - Created / Initial version.

By <b>[Frank Goenninger][]</b> < <frank.goenninger@goenninger.net> >, [dg1sbg][] on Github

[Frank Goenninger]: http://ham-and-eggs-from-frgo.blogspot.de

[dg1sbg]: https://github.com/dg1sbg

### TODO / ISSUES

<b>Feature requests concerning this document</b>

`001`: Shinmera, 2016-12-03<br>
frgo: I like to see a high-level overview that explains what kind of parts of Clasp that it connects to, how they work, and what the rationale is behind the design.

### Clasp is *fun* !
A quote of [Dr. Christian E. Schafmeister][], [drmeister][] on Github, the ceator of Clasp, in #clasp on 2016-12-02:

[Dr. Christian E. Schafmeister]: https://drmeister.wordpress.com

[drmeister]: https://github.com/drmeister

> 17:31 <drmeister> What makes it hard to talk about is
>                   I've got C++ classes whose instances are Common Lisp
>                   classes whose instances are instances of C++ classes.
>                   Sheesh.

### Table of Contents

* I. [PART I: THEORY OF OPERATION](#id-part-i)
    * I.1 [Introduction](#id-i.1)
    * I.2 [Basic Concepts](#id-i.2)
        * I.2.1 [Interaction between C++ Land and Lisp Land](#id-i.2.1)
    * I.3 [Type Translators](#id-i.3)

* II. [PART II: IMPLEMENTATION NOTES](#id-part-ii)
    * II.1 [Source Code](#id-ii.1)
        * II.1.1 [Source Code Files](#id-ii.1.1)
    * II.2 [Source Code Rules & Conventions](#id-ii.2)
    * II.3 [Calling Variadic Foreign Functions](#id-ii.3)

* * * * *

<div id="id-part-i">
# PART I: THEORY OF OPERATION #

<div id="id-i.1">
## I.1 Introduction ##

(Note: This is not a general introduction into FLIs for Common Lisp implementatios but rather a short note highlighting Clasp specialties!)
<br>
<br>
Clasp's FLI is designed to closely match the specification and requirements of CFFI-SYS, the system specification of the Common Foreign Function Interface CFFI. See [CFFI-SYS Documentation][] for more information.

[CFFI-SYS Documentation]: https://common-lisp.net/project/cffi/spec/cffi-sys-spec.html
For each CFFI macro or function a corresponding one is defined in Clasp's FLI.

<div id="id-i.2">
## I.2 Basic Concepts ##

<div id="id-i.2.1">
### I.2.1 Interaction between C++ Land and Lisp Land ###




<div id="id-i.3">
## I.3 Type Translators ##


* * * * *

<div id="id-part-ii">
# PART II : IMPLEMENTATION NOTES #

<div id="id-ii.1">
## II.1 Source Code ##

<div id="id-ii.1.1">
### II.1.1 Source Code Files ###

The implementation of the FLI is realized with the following files:

1. `include/clasp/core/fli.h`
<br>Links to source:  [Github: drmeister, branch dev, fli.h][] / [Github: dg1sbg, branch dev-cffi, fli.h][]

[Github: dg1sbg, branch dev-cffi, fli.h]: https://github.com/dg1sbg/clasp/blob/dev-cffi/include/clasp/core/fli.h

[Github: drmeister, branch dev, fli.h]: https://github.com/drmeister/clasp/blob/dev/include/clasp/core/fli.h

    This file is the C++ header file for the C++ land FLI implementation. It defines the main FLI C++ classes `ForeignData_O` and `ForeignTypeSpec_O`.

2. `src/core/fli.cc`
<br>Links to source: [Github: drmeister, branch dev, fli.cc][] / [Github: dg1sbg, branch dev-cffi, fli.cc][]

[Github: dg1sbg, branch dev-cffi, fli.cc]: https://github.com/dg1sbg/clasp/blob/dev-cffi/src/core/fli.cc

[Github: drmeister, branch dev, fli.cc]: https://github.com/drmeister/clasp/blob/dev/src/core/fli.cc

    This is the C++ source file of the FLI. It represents the main parts of the FLI in C++ land.

3. `src/lisp/kernel/lsp/fli.lisp`
<br>Links to source:  [Github: drmeister, branch dev, fli.lisp][] / [Github: dg1sbg, branch dev-cffi, fli.lisp][]

[Github: dg1sbg, branch dev-cffi, fli.lisp]: https://github.com/dg1sbg/clasp/blob/dev-cffi/src/lisp/kernel/lsp/fli.lisp

[Github: drmeister, branch dev, fli.lisp]: https://github.com/drmeister/clasp/blob/dev/src/lisp/kernel/lsp/fli.lisp

    This is the Common Lisp source file of the FLI. It represents the main parts of the FLI in Common Lisp land.

4. `src/llvmo/intrinsics.cc`
<br>Links to source:  [Github: drmeister, branch dev, intrinsics.cc][] / [Github: dg1sbg, branch dev-cffi, intrinsics.cc][]

[Github: dg1sbg, branch dev-cffi, intrinsics.cc]: https://github.com/dg1sbg/clasp/blob/dev-cffi/src/llvmo/intrinsics.cc

[Github: drmeister, branch dev, intrinsics.cc]: https://github.com/drmeister/clasp/blob/dev/src/llvmo/intrinsics.cc

    This file holds those functions of the FLI that need to be speed optimized, using techniques like inlining and link time optimization.

5. `src/lisp/kernel/cmp/cmpintrinsics.lisp`
<br>Links to source:  [Github: drmeister, branch dev, cmpintrinsics.lisp][] / [Github: dg1sbg, branch dev-cffi, cmpintrinsics.lisp][]

[Github: dg1sbg, branch dev-cffi, cmpintrinsics.lisp]: https://github.com/dg1sbg/clasp/blob/dev-cffi/src/lisp/kernel/cmp/cmpintrinsics.lisp

[Github: drmeister, branch dev, cmpintrinsics.lisp]: https://github.com/drmeister/clasp/blob/dev/src/lisp/kernel/cmp/cmpintrinsics.lisp

    This file contains - as part of function `#'define-primitives-in-module` - the primitve definitions for all data value type translator functions that are beung called when defining a C++ to Lisp callback function ([line 813][]).

[line 813]: https://github.com/dg1sbg/clasp/blob/dev-cffi/src/lisp/kernel/cmp/cmpintrinsics.lisp#L813

<div id="id-ii.1.2">
### II.1.2 Source Code Integration ###

As of the writing of version X.01.00 of this document, development was done in the [dev-cffi branch of dg1sbg's forked Clasp Github repository][].
[dev-cffi branch of dg1sbg's forked Clasp Github repository]: https://github.com/dg1sbg/clasp/tree/dev-cffi

Integration with the main development branch of [drmeister's repository, branch dev][] was done merging DEV's changes into DEV-CFFI source code and then back-integrating the DEV-CFFI source code into the DEV branch.
[drmeister's repository, branch dev]: https://github.com/drmeister/clasp/tree/dev

<div id="id-ii.2">
## II.2 Source Code Rules & Conventions ##

The following rules and conventions have been used to implement the FLI:

1. All FLI Lisp functions and macros are prefixed with a % sign - indicating that these macros and functions are meant to not be used directly by a user of the FLI, except when implementing low level access to Clasp's FLI on purpose.

* * * * *

<div id="id-ii.3">
## II.3 Calling Variadic Foreign Functions ##

A variadic callee -- `printf`, `open`, `fcntl`, `shm_open` -- must be called through a
*variadic* function type that names how many of its parameters are fixed. Calling it through
an ordinary function type is undefined behaviour in C, and the two are not interchangeable in
practice:

* On **x86-64 SysV** variadic arguments are passed in the same registers as fixed ones, so the
  mistake is invisible.
* On **Darwin arm64** variadic arguments are passed on the **stack**. A call lowered as
  non-variadic leaves them in registers, the callee reads an unwritten stack slot, and every
  variadic argument arrives as **zero**.

The usual symptom is a mode or flag that silently does nothing. `shm_open(name, flags, 0600)`
creates its object with `st_mode` `0`, which cannot then be reopened; `fcntl(fd, F_SETFD,
FD_CLOEXEC)` returns success without setting the flag.

### API

    (clasp-ffi:%foreign-funcall-varargs NAME FIXED-COUNT &rest ARGUMENTS)
    (clasp-ffi:%foreign-funcall-pointer-varargs PTR FIXED-COUNT &rest ARGUMENTS)

`ARGUMENTS` alternate type and value and end with the return type, exactly as for
`%FOREIGN-FUNCALL`. `FIXED-COUNT` is how many of them are the callee's **fixed** parameters;
the rest are variadic. It must be a literal integer, because it selects the fixed prefix when
the function type is built at compile time, and it is validated at macroexpansion.

    ;; int fcntl(int fd, int cmd, ...);  -- two fixed parameters
    (clasp-ffi:%foreign-funcall-varargs "fcntl" 2 :int fd :int f-setfd :int fd-cloexec :int)

`%FOREIGN-FUNCALL` is unchanged and still builds a non-variadic type, which is correct for the
overwhelming majority of callees. Only reach for the variadic forms when the C declaration has
an ellipsis.

### Implementation

`CMP:FUNCTION-TYPE-CREATE-ON-THE-FLY` accepts the foreign-type list in two shapes:

    (return-type (arg-types ...))                 ; non-variadic
    (return-type (arg-types ...) fixed-count)     ; variadic

The third element is optional, so the representation is backward compatible: the BIR
instruction, the translator and the foreign-caller cache all treat the signature as opaque data
and needed no change.

### CFFI

CFFI already carries the fixed/variadic split down to the backend, and falls back to appending
the two lists -- discarding the distinction -- when the backend does not define
`%FOREIGN-FUNCALL-VARARGS`. Clasp's backend defines it, so

    (cffi:foreign-funcall-varargs "shm_open" (:string name :int flags) :int #o600 :int)

passes the mode correctly.
