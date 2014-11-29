Clasp
===============

<a href="http://drmeister.wordpress.com/2014/09/26/building-clasp-and-externals-clasp/">-- Update Nov 29, 2014 --  Please read regarding installing Clasp</a>

**If you have questions come ask them on IRC at freenode #clasp**

Clasp is a Common Lisp implementation that interoperates with C++ and uses LLVM for just-in-time (JIT) compilation to native code.
See http://drmeister.wordpress.com/2014/09/18/announcing-clasp/ for the announcement.

Clasp is not yet a full ANSI compliant Common Lisp - if you find differences between Clasp and the Common Lisp standard they are considered bugs in Clasp and please feel free to report them.

**Libraries that clasp depends on can be obtained using the repository: <a href="https://github.com/drmeister/externals-clasp.git">externals-clasp</a>**<br>
Currently you must build externals-clasp prior to building Clasp - we will eliminate the need for this in future releases.

## Systems Clasp has built on

Clasp needs an advanced C++ compiler that supports C++11 (minimum clang 3.5 or gcc 4.8).

Clasp also needs a very specific version of the llvm/clang 3.6 libraries that are not part of a standard release.
This version of the llvm/clang3.6 is included in externals-clasp.  Incidentally, the externals-clasp/llvm3.6 builds a clang3.6 compiler, which can be used by the Clasp build system to compile Clasp.

|  Systems that Clasp is known to build on  |
| ----------------------------------------- |
|  OS X 10.9.5 using Xcode 6.0.1            |
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

1) You need to download and build the <a href="https://github.com/drmeister/externals-clasp">externals-clasp repository</a><br>
- it contains all of the external libraries that Clasp depends on and it downloads the specific version of LLVM 3.6 that Clasp needs.

2) Copy local.config.darwin or local.config.linux to local.config depending on your system

3) Edit local.config and configure it for your system. The following configuration variables affect the build process.

| Variable                                   |   Description                                                           |
| ------------------------------------------ | ----------------------------------------------------------------------- |
| **CLASP_BUILD_TARGET_DIR**                 | This defines where make will put the Clasp application. I use $HOME/local/clasp |
| **EXTERNALS_BUILD_TARGET_DIR**             | This defines where Clasp build will find the externals-clasp libraries.  I use $HOME/local/externals-clasp. This directory must be different from that of **CLASP_BUILD_TARGET_DIR**. |
| **TARGET_OS**                              | Currently either _linux_ or _darwin_                                    |
| **PJOBS**                                  | The number of processors you have available to build with.              |
|                                            | Set PJOBS <= the number of cores you have.                              |
|                                            | Also if you have less than 8GB memory you should set PJOBS to 2 or 1,   |
|                                            | otherwise your system will swap like crazy                              |
| **CXXFLAGS**                               | If you set this export CXXFLAGS = -v  it will print more debugging info |
|                                            | during the build                                                        |

4) Make both the mps and boehm versions of Clasp (see note 1 for other options).
<pre># <b>make</b></pre>

If you see the error "fatal error: 'core_scrape_flag.h' file not found" just stop the build with control-C and type "make" again. It will sort itself out.  It's something to do with the order in which boost-build builds things but I haven't sorted it out yet.

5) Add the directory in $**CLASP_BUILD_TARGET_DIR**/MacOS (OS X) or $**CLASP_BUILD_TARGET_DIR**/bin (linux) (from local.config) to your PATH<br>

6) To run the MPS version of Clasp use
<pre># <b>clasp_mps_o</b></pre>

and to run the Boehm version of Clasp use
<pre># <b>clasp_boehm_o</b></pre>

7) When the Clasp REPL prompt appears you can type Common Lisp commands.
<pre>Starting Clasp 0.1... loading image... it takes a few seconds
Loading .clasprc
Top level.
&gt; <b>(defun hello-world () (print "Clasp is running.  Huzzah!!!"))</b>

HELLO-WORLD
&gt; <b>(hello-world)</b>

"Clasp is running.  Huzzah!!!"
"Clasp is running.  Huzzah!!!"
&gt; <b>(quit)</b>
</pre>
Clasp will run within Emacs using \*inferior-lisp\* and in the future Clasp will have a SLIME interface (volunteer programmers will be showered with appreciation and praise!).

Note 1:  You can make just one version of Clasp
<pre># <b>make boostbuildv2-build</b> </pre>
and then to make the boehm version of Clasp use
<pre># <b>make clasp-boehm</b></pre>
or to make the mps version of Clasp use
<pre># <b>make clasp-mps</b></pre>

## External libraries

If you want to install the external libraries separately its more complicated because Clasp requires a particular version of LLVM/Clang3.6 which hasn't been officially released yet but is present in externals-clang.
This should all become easier in a couple of months when LLVM/Clang3.6 is released.<br>
These are the requirements as of Sep 28, 2014.<br>
LLVM/clang 3.5 compiler<br>
Boost build v2<br>
boost libraries ver 1.55<br>
Boehm 7.2<br>
gmp-6.0.0<br>
expat-2.0.1<br>
zlib-1.2.8<br>
readline-6.2<br>


## Acknowledgments

Clasp was supported by the Defense Threat Reduction Agency (DOD-DTRA) (HDTRA1-09-1-0009) the National Institutes of Health (NIH/NIGMS Grant number: 2R01GM067866-07A2) and the National Science Foundation (Grant number: 1300231)
