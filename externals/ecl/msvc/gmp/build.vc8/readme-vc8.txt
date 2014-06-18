
=====================================================
A. Compiling GMP and MPFR with the Visual Studio 2005
=====================================================

These VC++ build projects are based on GMP 4.2.1 and MPFR 2.2.1. 
Some files in GMP 4.2.1 need to be modified to compile with VC++.
These have been moved into the vc8 build directory to avoid 
making changes to GMP files. But this means that these files may
need to be updated if the related GMP files change. Building
MPFR is optional as this is no longer ditributed with GMP.

STEP ONE
========

First obtain the GMP and MPFR distributions, named gmp-<ver> and
mpfr-<ver> where <ver> are the versions being used.  Unzip the
gmp-<ver> files in the ZIP archive into a directory tree with the
gmp-<ver> directory as its root.  Unzip the MPFR files so that
the root mpfr-<ver> directory is within the gmp-<ver> directory.
That is, the directories mpfr and mpfr-<ver> are at the same level
in the directory tree. Then delete the mpfr directory and rename
the directory mpfr-<ver> to mpfr. You should then apply any patches
that are needed for either or both GMP and MPFR.  Then rename the
mparam_h.in file in the mpfr distribution to mparam.h.

Unzip the files in this distribution so that they are merged into
the above directory tree with the directories

  build.vc8    -- build files for gmp and mpfr
  mpn/x86i     -- the YASM x86 assembler files (Pentium family)
  mpn/amd64i   -- the YASM x64 assembler files (AMD64)

within the gmp root directory gmp-<ver>.

STEP TWO
========

If you wish to use the assembler files you will also need the YASM
open source x86 assembler (r1438 or later) for Windows which can be 
obtained from:

  http://www.tortall.net/projects/yasm/

This assembler should be placed in the bin directory used by VC++,
which, for Visual Stduio 2005, is typically:

 C:\Program Files (x86)\Microsoft Visual Studio 8\VC\bin

You will also need to move the yasm.rules file from this distribution 
into the directory where Visual Studio 2005 expects to find it, which 
is typically:

 C:\Program Files (x86)\Microsoft Visual Studio 8\VC\VCProjectDefaults
 
Alternatively you can configure the path for rules files in the VC++ 
configuration dialogue.

The NASM assembler is no longer supported as it cannot assemble 64-bit
instructions and also has problems with include file directory handling.

STEP THREE
==========

Visual Studiio 2005 can be started for building the 32 or 64 bit versions
of GMP and MPFR by clicking on the *.sln file in the build.vc8 directory.

GMP and MPFR are built using the appropriate build projects. Select the 
desired library and then set the desired configuration:

    win32 or x64
    release or debug
    
To build GMP dynamic link libraries (DLLs) choose one (or more) of:

    dll_gmp_amd64  - GMP DLL using AMD64 assembler (x64)
    dll_gmp_gc     - GMP DLL using generic C (win32 & x64)
    dll_gmp_p0     - GMP DLL using Pentium assembler (win32)
    dll_gmp_p3     - GMP DLL using Pentium III assembler (win32)
    dll_gmp_p4     - GMP DLL using Pentium IV assembler (win32)

To build GMP static libraries choose one (or more) of:

    lib_gmp_amd64  - GMP library using AMD64 assembler (x64)
    lib_gmp_gc     - GMP library using generic C (win32 & x64)
    lib_gmp_p0     - GMP library using Pentium assembler (win32)
    lib_gmp_p3     - GMP library using Pentium III assembler (win32
    lib_gmp_p4     - GMP library using Pentium IV assembler (win32)

Before any of these libraries is built the appropriate GMP configuration 
file is automatically copied into config.h.  After a static library is 
built it is then copied to the file gmp.lib in the 'lib' sub-directory 
within the VC++ solution folder (build.vc8). Simlarly when a DLL is built, 
the resulting DLL, its export libraries and its debug symbol file are 
copied to the files gmp.dll, gmp.exp, gmp.lib and gmp.pdb within the 
'dll' sub-directory.
 
This means that the 'dll' and 'lib' sub-directories respectively contain 
the last GMP DLLs and static libraries built.  These are then the libraries
used to build the MPFR and GMPXX libraries described later.

The GMP DLL projects include the C++ files. If you do not want these the
relevent files needed to be excluded from the DLL(s) you want to build. Go
to the 'cpp' subdirectory of their build project in the IDE and exclude all 
the files in this subdirectory from the build process.

All the DLLs and static libraries are multi-threaded and are linked to the 
multi-threaded Microsoft run-time libraries (DLLs are linked to DLL run time
libraries and static libraries are linked to run time static libraries).

Within the 'dll' and 'lib' sub-directories used for output the structure is:

   DLL or LIB 
      Win32
         Release
         Debug
      x64
         Release
         Debug   

in order to enable the appropriate library for the desired target 
platform to be located.

STEP FOUR
=========

After a GMP library has been built, other libraries can be built.
These always use the last GMP library (of the same type) that has 
been built.

To build the MPFR DLL use:
    
    dll_mpfr       - MPFR DLL using generic C (win32 & x64)
  
To build the MPFR static library use:

    lib_mpfr           - MPFR static library (win32 & x64)

To build the GMP C+ library wrapper use:

    lib_gmpxx          - GMP C++ wrapper static library (win32 & x64)

The MPFR static library build assumes that this is intended to work with 
the lib_gmp or lib_gmpxx static libraries. 

If more than one gmp DLL is built, please remember that when the MPFR DLL 
is built it will be linked to the last gmp DLL that is built. Alternatively 
you can edit the MPFR linker property page to link to a specific export 
library. The debug versions of these DLLs and libraries are built in the 
same way.

STEP FIVE (Tests)
=================

All the remaining projects are for GMP testing.  In Vusual Studio 2005 these
are in the Tests project folder and its sub-folders but Visual C++ Express 
doesn't support project folders so they have to be identified manually. 
These test cover only GMP at the moment and must be built and run manually.
The tests can only be built with the static libraries because they use 
internal symbols that are not exported by the DLLs.

=====================
B. Using GMP and MPFR
=====================

Many applications that rely on GMP and MPFR also include the gmp.h
and mpfr.h header files but it is impossible to be certain that
these will match the versions of GMP and MPFR that are built by
this distribution. Hence when this distribution is being used
with a GMP based application it is important to ensure that any
GMP and MPFR header files used by such an application are those
that are supplied here and not those that might have been supplied
with the application itself.  This will often be only gmp.h and
mpfr.h but some other header files may also be involved.

The static libraries and DLLs built here use the _cdecl calling
convention in which exported symbols have their C names prefixed
with an extra '_' character.  Some applications expect the _stdcall
convention to be used in which there is an underscore prefix and a
suffix of '@n' where n is the number of bytes used for the function
arguments on the stack.  Such applications will need to be modified
to work with the GMP and MPFR DLLs and libraries provided here. The
alternative of attempting to build GMP and MPFR using the _stdcall
convention is not recommended (and won't work with the assembler
based builds anyway). This is further complicated if the builds for
x64 are used since the conventions here are different once again.

1. Using the Static Libraries
=============================

To build a GMP C or C++ based application using the the static
libraries all that needs to be done is to add the GMP or GMPXX
static libraries to the application build process.  To build an
MPFR based application add the MPFR library and the GMP or GMPXX
library as appropriate.

It is, of course, important to ensure that any libraries that are
used have been built for the target platform.

2. Using the DLL Export Libraries
=================================

There are two ways of linking to a DLL. The first way is to use
one or more of the DLL export libraries built as described earlier
(note that these are not the same as static libraries although
they are used in a similar way when an application is built).

If you intend to use the DLL export libraries in an application
you need to:

   a. ensure that the application can locate the GMP and/or
      the MPFR DLLs in question when it is run.  This involves
      putting the DLL(s) on a recognised directory path.

   b. define __GMP_LIBGMP_DLL and/or __MPFR_LIBGMP_DLL when
      the application is built in order to ensure that GMP
      and/or MPFR symbols, which are DLL export symbols, are
      properly recognised as such.

3. Using DLL Dynamic loading
============================

The second way of linking to a DLL is to use dynamic loading.
This is more complex and will not be discussed here. The VC++
documentation describes how to use DLLs in this way.

==============================
KNOWN BUILD ISSUES FOR VC++ v8
==============================

1. A few test files require minor modifications to compile in this 
   build as follows;
   
File: tests\tests.h
25 25
26 26 #include "config.h"
27 27
28 28 #include <setjmp.h>  /* for jmp_buf */
29 29
30 30 #if defined (__cplusplus)
--------------------------------------------
   31 using namespace std;    /* BRG */
--------------------------------------------
31 32 extern "C" {
32 33 #endif
--------------------------------------------
33
--------------------------------------------
34 34
35 35 #ifdef __cplusplus
36 36 #define ANYARGS  ...
37 37 #else
38 38 #define ANYARGS
39 39 #endif
--------------------------------------------

File: tests\misc.c
24  24  #include <ctype.h>
25  25  #include <signal.h>
26  26  #include <stdio.h>
27  27  #include <stdlib.h>     /* for getenv */
28  28  #include <string.h>
29  29
------------------------------------------------------------------------
30      #if HAVE_FLOAT_H
------------------------------------------------------------------------
    30  #if HAVE_FLOAT_H || defined( _MSC_VER ) /* BRG */
------------------------------------------------------------------------
31  31  #include <float.h>      /* for DBL_MANT_DIG */
32  32  #endif
33  33
34  34  #if TIME_WITH_SYS_TIME
35  35  # include <sys/time.h>  /* for struct timeval */
36  36  # include <time.h>
------------------------------------------------------------------------
------------------------------------------------------------------------
474 474   case 1: rc = 3; break;  /* tozero  */
475 475   case 2: rc = 2; break;  /* up      */
476 476   case 3: rc = 1; break;  /* down    */
477 477   default:
478 478     return 0;
479 479   }
------------------------------------------------------------------------
    480 #if defined( _MSC_VER )
    481   {     unsigned int cw;
    482         _controlfp_s(&cw, 0, 0);
    483         _controlfp_s(&cw, (cw & ~0xC00) | (rc << 10), _MCW_RC);
    484   }
    485 #else
------------------------------------------------------------------------
480 486   x86_fldcw ((x86_fstcw () & ~0xC00) | (rc << 10));
------------------------------------------------------------------------
    487 #endif
------------------------------------------------------------------------
481 488   return 1;
482 489 #endif
483 490
484 491   return 0;
485 492 }
486 493
487 494 /* Return the hardware floating point rounding mode, or -1 if unknown. */
488 495 int
489 496 tests_hardware_getround (void)
490 497 {
491 498 #if HAVE_HOST_CPU_FAMILY_x86
------------------------------------------------------------------------
492       switch ((x86_fstcw () & ~0xC00) >> 10) {
------------------------------------------------------------------------
    499   unsigned int cw;
    500 #if defined( _MSC_VER )
    501   _controlfp_s(&cw, 0, 0);
    502 #else
    503   cw = x86_fstcw();
    504 #endif
    505
    506   switch ((cw & ~0xC00) >> 10) {
------------------------------------------------------------------------
493 507   case 0: return 0; break;  /* nearest */
494 508   case 1: return 3; break;  /* down    */
495 509   case 2: return 2; break;  /* up      */
496 510   case 3: return 1; break;  /* tozero  */
497 511   }
498 512 #endif
------------------------------------------------------------------------

File: tests\mpz\t-perfsqr.c
23 23 #include <stdlib.h>
24 24
25 25 #include "gmp.h"
26 26 #include "gmp-impl.h"
27 27 #include "tests.h"
28 28
------------------------------------------------------------------------
   29 #ifdef _MSC_VER     /* BRG */
   30 #include "perfsqr.h"
   31 #else
------------------------------------------------------------------------
29 32 #include "mpn/perfsqr.h"
------------------------------------------------------------------------
30
------------------------------------------------------------------------
   33 #endif
------------------------------------------------------------------------
31 34
32 35 /* check_modulo() exercises mpz_perfect_square_p on squares which cover each
33 36    possible quadratic residue to each divisor used within
34 37    mpn_perfect_square_p, ensuring those residues aren't incorrectly claimed
35 38    to be non-residues.
36 39
------------------------------------------------------------------------

File: tests\mpn\t-perfsqr.c
23 23 #include <stdlib.h>
24 24
25 25 #include "gmp.h"
26 26 #include "gmp-impl.h"
27 27 #include "tests.h"
28 28
------------------------------------------------------------------------
   29 #ifdef _MSC_VER     /* BRG */
   30 #include "perfsqr.h"
   31 #else
------------------------------------------------------------------------
29 32 #include "mpn/perfsqr.h"
------------------------------------------------------------------------
30
------------------------------------------------------------------------
   33 #endif
------------------------------------------------------------------------
31 34
32 35 #define PERFSQR_MOD_MASK   ((CNST_LIMB(1) << PERFSQR_MOD_BITS) - 1)
33 36
34 37 void
35 38 check_mod_2 (mp_limb_t d, mp_limb_t inv, mp_limb_t got_hi, mp_limb_t got_lo)
36 39 {
------------------------------------------------------------------------

================
Acknowledgements
================

My thanks to:

1. The GMP team for their work on GMP and the MPFR team for their work on MPFR
2. Sam Krasnik and Mike Loehr for suggestions on how to improve
   and correct errors in earlier releases.
3. Patrick Pelissier and Vincent Lefèvre for helping to resolve
   VC++ issues in MPFR.

Brian Gladman, December 2006
