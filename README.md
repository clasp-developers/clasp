Clasp 0.4.0 (Common Lisp and C++ and LLVM)
===============

## Description of Clasp

Clasp is a Common Lisp implementation that interoperates with C++ and uses LLVM for just-in-time (JIT) compilation to native code.
See http://drmeister.wordpress.com/2014/09/18/announcing-clasp/ for the initial announcement.

It is designed as a Common Lisp implementation that can be easily extended using C++ libraries.

**If you have questions come ask them on IRC at freenode #clasp**

## Building Clasp

Clasp builds on Linux and OS X. Clasp is dependent on several libraries including llvm3.6, clang3.6, the boost libraries.

<a href="https://github.com/drmeister/clasp/wiki/Building-Clasp-0.4-on-Ubuntu">An example of how to configure Ubuntu Linux to build Clasp</a>

To build Clasp from within the top level directory do the following.

<pre>
clasp$ make
</pre>

More control over the build process can be gained by setting up a local.config file.

1. Copy local.config.template to local.config in the clasp top level directory
1. Edit local.config and configure it for your system. The following configuration variables affect the build process.

| Variable                                   |   Description                                                           |
| ------------------------------------------ | ----------------------------------------------------------------------- |
| **TARGET_OS**                              | Currently either _Linux_ or _Darwin_                                    |
| **PJOBS**                                  | The number of processors you have available to build with. Set PJOBS <= the number of cores you have.  |

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

## Using Clasp

The recommended way to run Clasp is using Slime and the latest Slime supports Clasp.

Clasp also runs ASDF and Quicklisp.

## External libraries

These are the requirements as of Oct, 2015.

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
