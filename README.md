# Clasp — Bringing Common Lisp and C++ Together

## What Is This?
Clasp is a new [Common Lisp](https://common-lisp.net/) implementation that seamlessly interoperates with C++ libraries and programs using [LLVM](http://llvm.org/) for compilation to native code. This allows Clasp to take advantage of a vast array of preexisting libraries and programs, such as out of the scientific computing ecosystem. Embedding them in a Common Lisp environment allows you to make use of rapid prototyping, incremental development, and other capabilities that make it a powerful language.

## New Features
* Clasp has a completely new, <a href="https://drmeister.wordpress.com/2015/07/30/timing-data-comparing-cclasp-to-c-sbcl-and-python/">optimizing/inlining compiler called cclasp!</a>
* Fixnum, character and single-float types are immediate values.
* General object pointers and cons pointers are tagged for speed.
* Clbind library allows programmers to expose external C++ libraries.
* Lots of bug fixes and stability improvements.

## Getting Clasp
Precompiled and prepackaged versions of Clasp will be available for a limited number of distributions. Check the [releases](https://github.com/drmeister/clasp/releases) to see if there is something available for you.

At the moment, Clasp is supported on Linux and Mac OS X. On these systems, you should be able to build it from source if a pre-made package is not available or workable for you. In case you cannot get it to compile even with the instructions below, the quickest way to get help is to either [file an issue](#reporting-problems), or to [chat with us directly](#irc).

Building on most systems will take around 4GB of RAM and ~2 hours with a relatively modern processor, so be prepared to watch a movie or do some other useful work until Clasp is all done.

### Building on Linux
For most distributions that have the listed [dependencies](#external-dependencies) available as packages, the compilation should be straightforward. Simply clone Clasp and run `make` from the root of it.

If the system is too dumb to find some of the dependencies or fails for other reasons, you might have to manually adjust configuration variables. For this, copy `local.config.template` to `local.config` and edit it as appropriate. If you lack the required dependencies, try [compiling with externals-clasp](#building-with-externals-clasp).

The compilation output will be in the `build/clasp` directory. To launch Clasp, run `build/clasp/bin/clasp_boehm_o`.

Clasp has been successfully built on

* **Ubuntu 14.04**, see [this wiki entry](https://github.com/drmeister/clasp/wiki/Building-Clasp-0.4-on-Ubuntu)
* **Debian Jessie**
* **Debian Sid**
* **Debian Wheezy**
* **OpenSuse 13.1**
* **Gentoo**
* **Arch**, currently requires downgrading Clang and LLVM to 3.6 .

### Building on OS X
First you will need what is listed for OS X under the [dependencies](#external-dependencies). Next you need an additional step that is documented [on the wiki](https://github.com/drmeister/clasp/wiki/Building-Clasp-on-OS-X-requires-using-the-open-source-version-of-Clang). The rest of the procedure is the same as for [building with externals-clasp](#building-with-externals-clasp).

The compilation output will be in the `build/clasp` directory. To launch Clasp, run `build/clasp/MacOS/clasp_boehm_o`.

### Building With Externals-Clasp
If your system does not provide the [external dependencies](#external-dependencies) as required by Clasp, you can use this approach instead, which will compile them for you.

Clone [externals-clasp](https://github.com/drmeister/externals-clasp) to a directory on your system. Next, create a `local.config` containing `export GCC_TOOLCHAIN = /usr` if you are on Linux and `export TOOLSET = clang` if you are on OS X. Next, simply run `make` from the root of it. This will take some time to complete; maybe play a round of pinball or [chat on IRC for a bit](#irc).

The next step is building Clasp itself. For this, clone it to a different folder and copy the `local.config.template` file within it to `local.config`. Next, open it up and make sure to uncomment and adapt the `EXTERNALS_CLASP_DIR` line to point to the location where you compiled externals-clasp. Something like `export EXTERNALS_CLASP_DIR = /opt/externals-clasp`. Finally it's time to kick off the build process. Simply run `make` from the Clasp root.

### External Dependencies
#### Linux
Simply install the appropriate packages with your package manager.

* **llvm** 3.6
* **clang** 3.6, including headers.
* **boost**
* **autoreconf** (dh-autoreconf on Ubuntu)
* **gmp** 6.0.0, compiled with --enable-cxx
* **expat** 2.0.1
* **zlib** 1.2.8
* **readline** 6.2

#### OS X
Use either [brew](http://brew.sh/) or [ports](https://www.macports.org/) to install the dependencies besides Xcode. Make sure the binaries are in your `PATH`.

* **Xcode** 6.4
* **Xcode command-line tools**
* **automake**
* **autoconf**
* **libtool**
* **pkg-config**

## Common Lisp Ecosystem Support
Clasp supports [SLIME](https://common-lisp.net/project/slime/), [ASDF](https://common-lisp.net/project/asdf/), and [Quicklisp](https://www.quicklisp.org/beta/). As such, development as in other Common Lisp implementations should be rather straight forward.

Note that Clasp does not currently support several of the staple features such as [CFFI](https://github.com/drmeister/clasp/issues/162), [Bordeaux-Threads](https://github.com/drmeister/clasp/issues/163), and [Unicode](https://github.com/drmeister/clasp/issues/164).

## Reporting Problems
Generally you can report problems in two fashions, either by [opening an issue ticket](https://github.com/drmeister/clasp/issues/new) or by [chatting with us directly](#irc). In both cases, though, you should have the following pieces handy in order for us to be able to help you out as quickly and painlessly as possible.

* Your operating system name and version.
* The versions of the [external libraries](#external-dependencies) that you have installed.
* A paste of the build log or failure point that you reached.
* Patience.

## IRC
Clasp has an IRC channel on [Freenode](https://freenode.net/) called [#clasp](irc://irc.freenode.net/#clasp). If you have questions, problems, suggestions, or generally would like to just hang out with us devs, come and stop by!

## My Blog

<a href="https://drmeister.wordpress.com">More details on Clasp.</a>

## Acknowledgments
Clasp was supported by the Defense Threat Reduction Agency (DOD-DTRA) (HDTRA1-09-1-0009) the National Institutes of Health (NIH/NIGMS Grant number: 2R01GM067866-07A2) and the National Science Foundation (Grant number: 1300231).
