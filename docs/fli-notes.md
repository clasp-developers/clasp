CLASP FOREIGN LANGUAGE INTERFACE  (FLI)
=======================================

Document Revision: X.01.00 - DRAFT
<br> Document Status: IN WORK
<br>
<br> Revision History:
<br> 2016-12-01: Created / Intial version.

* * * * *

# PART I : THEORY OF OPERATION #

## I.1 Introduction ##

Clasp's FLI is implemented to closely match the specification and requirementsof CFFI-SYS, the system specification of the Common Foreign Function Interface. See [CFFI-SYS Documentation][] for more information.

[CFFI-SYS Documentation]: https://common-lisp.net/project/cffi/spec/cffi-sys-spec.html

## I.2 Basic concept ##



## I.3 ##

# PART II : IMPLEMENTATION NOTES #

## II.1 Source Code ##

The implementation of the FLI is realized with the following files:

1. `include/clasp/core/fli.h`
<br>Links to source:  [Github: drmeister, branch dev][] / [Github: dg1sbg, branch dev-cffi][]

[Github: dg1sbg, branch dev-cffi]: https://github.com/dg1sbg/clasp/blob/dev-cffi/include/clasp/core/fli.h

[Github: drmeister, branch dev]: https://github.com/drmeister/clasp/blob/dev/include/clasp/core/fli.h

2. `src/core/fli.cc`
<br>Links to source: [Github: drmeister, branch dev][] / [Github: dg1sbg, branch dev-cffi][]

[Github: dg1sbg, branch dev-cffi]: https://github.com/dg1sbg/clasp/blob/dev-cffi/src/core/fli.cc

[Github: drmeister, branch dev]: https://github.com/drmeister/clasp/blob/dev/src/core/fli.cc

3. `src/lisp/kernel/lsp/fli.lsp`
<br>Links to source:  [Github: drmeister, branch dev][] / [Github: dg1sbg, branch dev-cffi][]

[Github: dg1sbg, branch dev-cffi]: https://github.com/dg1sbg/clasp/blob/dev-cffi/src/lisp/kernel/lsp/fli.sp

[Github: drmeister, branch dev]: https://github.com/drmeister/clasp/blob/dev/src/lisp/kernel/lsp/fli.lsp

4. `src/llvmo/intrinsics.cc`
<br>Links to source:  [Github: drmeister, branch dev][] / [Github: dg1sbg, branch dev-cffi][]

[Github: dg1sbg, branch dev-cffi]: https://github.com/dg1sbg/clasp/blob/dev-cffi/src/llvmo/intrinsics.cc

[Github: drmeister, branch dev]: https://github.com/drmeister/clasp/blob/dev/src/llvmo/intrinsics.cc

5. `src/lisp/kernel/cmp/cmpintrinsics.lsp`
<br>Links to source:  [Github: drmeister, branch dev][] / [Github: dg1sbg, branch dev-cffi][]

[Github: dg1sbg, branch dev-cffi]: https://github.com/dg1sbg/clasp/blob/dev-cffi/src/lisp/kernel/cmp/cmpintrinsics.lsp

[Github: drmeister, branch dev]: https://github.com/drmeister/clasp/blob/dev/src/lisp/kernel/cmp/cmpintrinsics.lsp
