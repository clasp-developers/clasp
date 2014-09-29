Clasp
===============

<a href="http://drmeister.wordpress.com/2014/09/26/building-clasp-and-externals-clasp/">-- Update Sept 28, 2014 --  Please read regarding installing Clasp</a>


Clasp is a Common Lisp implementation that interoperates with C++ and uses LLVM for just-in-time (JIT) compilation to native code.  

See http://drmeister.wordpress.com/2014/09/18/announcing-clasp/ for the announcement.

Clasp is not yet a full ANSI compliant Common Lisp - if you find differences between Clasp and the Common Lisp standard they are considered bugs in Clasp and please feel free to report them.

**Libraries that clasp depends on can be obtained using the repository: <a href="https://github.com/drmeister/externals-clasp.git">externals-clasp</a>**<br>
You must build externals-clasp prior to building Clasp

**BUILDING CLASP**

Clasp uses a lot of leading edge C++11 language features and so it needs a minimum of Clang 3.5 or gcc 4.8 to compile externals-clasp.   externals-clasp installs a local version of Clang 3.6 that is used to compile Clasp.

| Systems that Clasp is known to build on |
| -------------------------------------- |
| OS X 10.9.5 using Xcode 6.0.1          |
| Debian Jessie(Testing) - after apt-get ncurses-dev, libbz2-dev (for externals-clasp)|
| Systems that Clasp had problems building on |
| ----------------------------------------|
| Debian Wheezy with with gcc 4.9!!! See note below|

Note: We ran into a problem installing on a Debian Wheezy system upgraded to gcc 4.9.  I'm doing some things to try and get around this.

To build Clasp from within the top level directory do the following.

1) You need to down/build the <a href="https://github.com/drmeister/externals-clasp">externals-clasp repository</a><br>
- it contains all of the external libraries that Clasp depends on and it downloads the specific version of LLVM 3.6 that Clasp needs.

2) Copy local.config.darwin or local.config.linux to local.config depending on your system

3) Edit local.config and configure it for your system<br>
As in externals-clasp the following configuration variables are important.

| Variable  |   Description 
| ------------- | --------------|
| **CLASP_BUILD_TARGET_DIR**    | This defines where make will put the Clasp application  |
|                               | I use $HOME/local/clasp |
| **EXTERNALS_BUILD_TARGET_DIR**  | This defines where Clasp build will find the externals-clasp libraries  |
|                                 | I use $HOME/local/externals-clasp |
|**TARGET_OS**                    |Currently either _linux_ or _darwin_|
|**PJOBS**                        |The number of processors you have available to build with|


4) Type:    _make_        to build mps and boehm versions of Clasp<br>
   or type: _make boostbuildv2-build_      followed by<br>
     either _make-boehm_  to make the boehm version of Clasp<br>
         or _make-mps_    to make the MPS version of Clasp
         
If you see the error "fatal error: 'core_scrape_flag.h' file not found" just stop the build with control-C and type "make" again. It will sort itself out.  It's something to do with the order things are built in but I haven't tracked it down yet.

5) Install the directory in $**CLASP_BUILD_TARGET_DIR**/MacOS or $**CLASP_BUILD_TARGET_DIR**/bin (from local.config) in your path<br>
   then type: clasp_mps_o     to start the Lisp REPL of the MPS version of Clasp<br>
   or type:   clasp_boehm_o   to start the Lisp REPL of the Boehm version of Clasp

6) Type: (print "Hello world")  in the REPL and away you go (more documentation to follow)


If you want to install the libraries separately its more complicated because Clasp requires a particular version of LLVM/Clang3.6 which hasn't been officially released yet but is present in externals-clang.
This should all become easier in a couple of months when LLVM/Clang3.6 is released.<br>
These are the requirements as of Sep 28, 2014.<br>
LLVM/clang 3.5 COMPILER (on some systems Clang3.6 will work but there can be problems with installed header files)<br>
Boost build v2<br>
boost libraries ver 1.55<br>
Boehm 7.2<br>
gmp-5.0.5<br>
expat-2.0.1<br>
zlib-1.2.8<br>
readline-6.2<br>
