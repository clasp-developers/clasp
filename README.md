Clasp
===============
Clasp is a Common Lisp implementation that interoperates with C++ and uses LLVM for just-in-time (JIT) compilation to native code.

See http://drmeister.wordpress.com/2014/09/18/announcing-clasp/ for the announcement.

Clasp is not yet a full ANSI compliant Common Lisp - if you find differences between Clasp and the Common Lisp standard they are considered bugs in Clasp and please feel free to report them.

Libraries that clasp depends on can be obtained using the repository: externals-clasp
https://github.com/drmeister/externals-clasp.git
You can build externals-clasp or you can configure your environment by hand.

To build clasp from within the top level directory do the following.

1) Ensure that the llvm tools "llc" and the clasp exectuables (at the correct versions as in externals-clasp) are in your PATH

2) Copy local.config.template to local.config

3) Edit local.config and configure it for your system

4) make


If you want to install the libraries separately they are:
Contact me for more info - I can add more details to what is below.
Boost build v2<p>
boost libraries ver 1.55<p>
Boehm 7.2<p>
LLVM/clang 3.5<p>
ecl ver 12<p>
gmp-5.0.5<p>
expat-2.0.1<p>
zlib-1.2.8<p>
readline-6.2<p>
                                                                                                        
