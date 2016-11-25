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
At the moment, Clasp is supported on Linux and Mac OS X. On these systems, you should be able to build it from source. In case you cannot get it to compile even with the instructions below, the quickest way to get help is to either [file an issue](#reporting-problems), or to [chat with us directly](#irc).

Building on most systems will take around 4GB of RAM and ~2 hours with a relatively modern processor, so be prepared to watch a movie or do some other useful work until Clasp is all done.

In the future, precompiled and prepackaged versions of Clasp will be available for a limited number of distributions. Check the [releases](https://github.com/drmeister/clasp/releases) to see if there is something available for you.

### Building Externals-Clasp
Currently, no systems provide the advanced version of llvm/clang [external dependencies](#external-dependencies) as required by Clasp, so you must compile them.

Clone [externals-clasp](https://github.com/drmeister/externals-clasp) to a directory on your system. Follow the directions to build it.

This will take some time to complete; maybe play a round of pinball or [chat on IRC for a bit](#irc).

Next, copy `clasp/wscript.config.template` (a Python source file) to `clasp/wscript.config` and uncomment the line containing ` ##export EXTERNALS_SOURCE_DIR = $(HOME)/Development/externals-clasp` and change it to the directory where your externals-clasp is situated. Next, go to the instructions for building on linux or OS X. 



### Building on Linux

Run ```./waf update_submodules configure``` and then ```./waf build_cboehm```.
The compilation output will be in the `build` directory. To launch Clasp, run `build/clasp`.

Clasp has been successfully built on

* **Ubuntu 14.04**, see [this wiki entry](https://github.com/drmeister/clasp/wiki/Building-Clasp-0.4-on-Ubuntu)
* **Debian Jessie**
* **Debian Sid**
* **Debian Wheezy**
* **OpenSuse 13.1**
* **Gentoo**
* **Arch**

### Building on OS X
First you will need what is listed for OS X under the [dependencies](#external-dependencies). Next you need an additional step that is documented [on the wiki](https://github.com/drmeister/clasp/wiki/Building-Clasp-on-OS-X-requires-using-the-open-source-version-of-Clang). The rest of the procedure is the same as for [building with externals-clasp](#building-with-externals-clasp).

Run ```./waf update_submodules configure``` and then ```./waf build_cboehm```.
The compilation output will be in the `build` directory. To launch Clasp, run `build/clasp`.

### External Dependencies
#### Linux
Simply install the appropriate packages with your package manager.

* **llvm** 4.0 (use externals-clasp)
* **clang** 4.0 (use externals-clasp)
* **boost**
* **gmp** 6.0.0, compiled with --enable-cxx
* **zlib** 1.2.8

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
