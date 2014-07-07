Clasp
===============
Clasp is a Common Lisp that interoperates with C++ and uses LLVM for just-in-time (JIT) compilation to native code.

Clasp is not yet a full featured Common Lisp - if you find differences between Clasp and the Common Lisp standard it is considered a bug in Clasp and please feel free to report it.

Libraries that clasp depends on can be obtained using the repo: externals-clasp
https://github.com/drmeister/externals-clasp.git


To build everything from within the top level directory do the following.

1) Ensure that the llvm tools "llc" are in your PATH

2) Copy local.config.template to local.config

3) Edit local.config (ignored by the git repo) and configure it for your system

4) make


If you want to install the libraries separately they are:
Contact me for more info - I can add more details to what is below.
Boost build v2
Boehm 7.2
LLVM 3.5 (top of tree 3.4)
Clang
ecl
gmp
expat
zlib
openmpi
readline
                                                                                                        
