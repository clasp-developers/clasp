Clasp
===============

<a href="http://drmeister.wordpress.com/2014/09/26/building-clasp-and-externals-clasp/">Please read regarding installing Clasp on Sept 25, 2014</a>


Clasp is a Common Lisp implementation that interoperates with C++ and uses LLVM for just-in-time (JIT) compilation to native code.

See http://drmeister.wordpress.com/2014/09/18/announcing-clasp/ for the announcement.

Clasp is not yet a full ANSI compliant Common Lisp - if you find differences between Clasp and the Common Lisp standard they are considered bugs in Clasp and please feel free to report them.

**Libraries that clasp depends on can be obtained using the repository: externals-clasp**<br>
https://github.com/drmeister/externals-clasp.git<br>
You must build externals-clasp prior to building Clasp

**BUILDING CLASP**

Clasp has been compiled on OS X 10.9.5 using Xcode 6.0.1

Clasp has been compiled on recent (post 2013) versions of Ubuntu Linux

To build Clasp from within the top level directory do the following.

1) Strongly consider downloading/building the <a href="https://github.com/drmeister/externals-clasp">externals-clasp repository</a><br>
- it contains all of the external libraries that LLVM depends on.

2) Copy local.config.darwin or local.config.linux to local.config

3) Edit local.config and configure it for your system<br>
As in externals-clasp the following configuration variables are important.

| Variable  |   Description 
| ------------- | --------------|
| **CLASP_BUILD_TARGET_DIR**  | This defines where make will put the Clasp application  |
|        | I use $HOME/local/clasp |
| **EXTERNALS_BUILD_TARGET_DIR**  | This defines where Clasp build will find the externals-clasp libraries  |
|   | I use $HOME/local/externals-clasp |
|**TARGET-OS**                    |Currently either _linux_ or _darwin_|
|**PJOBS**                        |The number of processors you have available to build with|


4) Type:    _make_        to build mps and boehm versions of Clasp<br>
   or type: _make-boehm_  to make the boehm version of Clasp<br>
   or type: _make-mps_    to make the MPS version of Clasp

5) Install the directory in $**CLASP_BUILD_TARGET_DIR**/MacOS or $**CLASP_BUILD_TARGET_DIR**/bin (from local.config) in your path<br>
   then type: clasp_mps_o     to start the Lisp REPL of the MPS version of Clasp<br>
   or type:   clasp_boehm_o   to start the Lisp REPL of the Boehm version of Clasp

6) Type: (print "Hello world")  in the REPL and away you go (more documentation to follow)


If you want to install the libraries separately they are:<br>
Contact me for more info - I can add more details to what is below.<br>
Boost build v2<br>
boost libraries ver 1.55<br>
Boehm 7.2<br>
LLVM/clang 3.5<br>
ecl ver 12<br>
gmp-5.0.5<br>
expat-2.0.1<br>
zlib-1.2.8<br>
readline-6.2<br>
