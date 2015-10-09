Clasp (Common Lisp and C++ and LLVM)
===============

# Clasp is getting ready for a major new release (As of Oct 6, 2015)

**If you have questions come ask them on IRC at freenode #clasp**

Clasp is a Common Lisp implementation that interoperates with C++ and uses LLVM for just-in-time (JIT) compilation to native code.
See http://drmeister.wordpress.com/2014/09/18/announcing-clasp/ for the initial announcement.

Clasp is not yet a full ANSI compliant Common Lisp - if you find differences between Clasp and the Common Lisp standard they are considered bugs in Clasp and please feel free to report them.

## Systems Clasp has built on

Clasp requires clang 3.6 or higher to compile

|  Systems that Clasp is known to build on  |
| ----------------------------------------- |
| OS X 10.10 using Xcode 6.4  [IMPORTANT - see (https://github.com/drmeister/clasp/wiki/Building-Clasp-on-OS-X-requires-using-the-open-source-version-of-Clang)          |
|  Debian Testing(Jessie) - [see issue #22](https://github.com/drmeister/clasp/issues/22)   |
|  Debian Unstable(Sid)                     |
|  Gentoo Linux - [see issue #20](https://github.com/drmeister/clasp/issues/20)             |
|  Arch Linux                               |
|  OpenSuse 13.1 - [see issue #19](https://github.com/drmeister/clasp/issues/19)            |
|  Debian Stable (Wheezy) - [see issue #21](https://github.com/drmeister/clasp/issues/21)   |
|  Ubuntu 14.04 - [see issue #26](https://github.com/drmeister/clasp/issues/26)             |

If you experience problems with the systems above please submit an issue here or come see us on **#clasp on irc.freenode.net**

## Building Clasp

To build Clasp from within the top level directory do the following.

1) Clasp has a list of dependencies that we are working on - see below

2) Copy local.config.darwin or local.config.linux to local.config depending on your system

3) Edit local.config and configure it for your system. The following configuration variables affect the build process.

| Variable                                   |   Description                                                           |
| ------------------------------------------ | ----------------------------------------------------------------------- |
| **TARGET_OS**                              | Currently either _linux_ or _darwin_                                    |
| **PJOBS**                                  | The number of processors you have available to build with.              |
|                                            | Set PJOBS <= the number of cores you have.                              |
|                                            | Also if you have less than 8GB memory you should set PJOBS to 2 or 1,   |
|                                            | otherwise your system will swap like crazy                              |
| **CLASP_CXXFLAGS**                         | For instance, adding -v  it will print more debugging info              |
|                                            | during the build                                                        |
| **CLASP_LINKFLAGS**                        | Add your local library dependencies here using -L and -l linker options |
4) Make both the MPS and Boehm versions of Clasp (Currently only Boehm version is built).
<pre># <b>make</b></pre>

5) Add the directory in $**CLASP_BUILD_TARGET_DIR**/MacOS (OS X) or $**CLASP_BUILD_TARGET_DIR**/bin (linux) (from local.config) to your PATH<br>

6) To run the Boehm version of Clasp use
<pre># <b>clasp_boehm_o</b></pre>

and to run the Boehm version of Clasp use
<pre># <b>clasp_boehm_o</b></pre>

7) When the Clasp REPL prompt appears you can type Common Lisp commands.
<pre>Starting Clasp xxxxx   loading image... it takes a few seconds
Loading .clasprc
Top level.
&gt; <b>(defun hello-world () (print "Clasp is running.  Huzzah!!!"))</b>

HELLO-WORLD
&gt; <b>(hello-world)</b>

"Clasp is running.  Huzzah!!!"
"Clasp is running.  Huzzah!!!"
&gt; <b>(quit)</b>
</pre>

The recommended way to run Clasp is using Slime and the latest Slime supports Clasp.

Clasp also runs ASDF and Quicklisp.

## External libraries

These are the requirements as of Oct, 2015.<br>
1. llvm-3.6
1. clang-3.6 (needs clang header files)
1. boost libraries
2. autoreconf  (dh-autoreconf on Ubuntu)
1. gmp-6.0.0 (compile with --enable-cxx)
1. expat-2.0.1
1. zlib-1.2.8
1. readline-6.2

## Acknowledgments

Clasp was supported by the Defense Threat Reduction Agency (DOD-DTRA) (HDTRA1-09-1-0009) the National Institutes of Health (NIH/NIGMS Grant number: 2R01GM067866-07A2) and the National Science Foundation (Grant number: 1300231)
