#|#########################################################################
# Copyright (C) 1997-2011 Ufasoft                                         #
# http://ufasoft.com   mailto:support@ufasoft.com                         #
#                                                                         #
# This program is free software; you can redistribute it and/or modify it #
# under the terms of the GNU General Public License as published by the   #
# Free Software Foundation; either version 3, or (at your option) any     #
# later version.                                                          #
#                                                                         #
# This program is distributed in the hope that it will be useful,         #
# but WITHOUT ANY WARRANTY; without even the implied warranty of          #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           #
# GNU General Public License for more details.                            #
#                                                                         #
# You should have received a copy of the GNU General Public License       #
# along with this program; If not, see <http://www.gnu.org/licenses/>     #
########################################################################=|#


(CL::setq CL::*package* (CL::find-package "SYS"))

(CL::export '(*big-endian* concat-pnames))

(CL::setq CL::*package* (CL::find-package "CL"))

(CL::export '(
  &allow-other-keys &aux &body &environment &key &optional &rest &whole
  * ** *** + ++ +++ - / // /// /= 1+ 1- < <= = > >=
  *break-on-signals* *compile-file-pathname* *compile-file-truename* *compile-print* *compile-verbose* *debug-io* *debugger-hook*
  *default-pathname-defaults* *error-output* *features* *gensym-counter* *load-pathname* *load-print* *load-truename*
  *load-verbose* *macroexpand-hook* *modules* *package* *print-array* *print-base* *print-case* *print-circle* *print-escape*
  *print-gensym* *print-length* *print-level* *print-lines* *print-miser-width* *print-pprint-dispatch* *print-pretty*
  *print-radix* *print-readably* *print-right-margin* *query-io* *random-state* *read-base* *read-default-float-format*
  *read-eval* *read-suppress* *readtable* *standard-input* *standard-output* *terminal-io* *trace-output*
  abort abs acons acos acosh add-method adjoin adjust-array adjustable-array-p allocate-instance alpha-char-p alphanumericp and
  append apply apropos apropos-list aref arithmetic-error arithmetic-error-operands arithmetic-error-operation array
  array-dimension array-dimension-limit array-dimensions array-displacement array-element-type array-has-fill-pointer-p
  array-in-bounds-p array-rank array-rank-limit array-row-major-index array-total-size array-total-size-limit arrayp ash asin
  asinh assert assoc assoc-if assoc-if-not atan atanh atom base-char base-string bignum bit bit-and bit-andc1 bit-andc2 bit-eqv
  bit-ior bit-nand bit-nor bit-not bit-orc1 bit-orc2 bit-vector bit-vector-p bit-xor block boole boole-1 boole-2 boole-and
  boole-andc1 boole-andc2 boole-c1 boole-c2 boole-clr boole-eqv boole-ior boole-nand boole-nor boole-orc1 boole-orc2 boole-set
  boole-xor boolean both-case-p boundp break broadcast-stream broadcast-stream-streams built-in-class butlast byte byte-position
  byte-size caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar cadddr caddr cadr call-arguments-limit
  call-method call-next-method car case catch ccase cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr
  cdddr cddr cdr ceiling cell-error cell-error-name cerror change-class char char-code char-code-limit char-downcase char-equal
  char-greaterp char-int char-lessp char-name char-not-equal char-not-greaterp char-not-lessp char-upcase char/= char< char<=
  char=  char> char>= character characterp check-type cis class class-name class-of clear-input clear-output close clrhash
  code-char coerce compilation-speed compile compile-file compile-file-pathname compiled-function compiled-function-p
  compiler-macro compiler-macro-function complement complex complexp compute-applicable-methods compute-restarts concatenate
  concatenated-stream concatenated-stream-streams cond condition conjugate cons consp constantly constantp continue control-error
  copy-alist copy-list copy-pprint-dispatch copy-readtable copy-seq copy-structure copy-symbol copy-tree cos cosh count count-if
  count-if-not ctypecase debug decf declaim declaration declare decode-float decode-universal-time defclass defconstant defgeneric
  define-compiler-macro define-condition define-method-combination define-modify-macro define-setf-expander define-symbol-macro
  defmacro defmethod defpackage defparameter defsetf defstruct deftype defun defvar delete delete-duplicates delete-file delete-if
  delete-if-not delete-package denominator deposit-field describe describe-object destructuring-bind digit-char digit-char-p
  directory directory-namestring disassemble division-by-zero do do* do-all-symbols do-external-symbols do-symbols documentation
  dolist dotimes double-float double-float-epsilon double-float-negative-epsilon dpb dribble dynamic-extent ecase echo-stream
  echo-stream-input-stream echo-stream-output-stream ed eighth elt encode-universal-time end-of-file endp enough-namestring
  ensure-directories-exist ensure-generic-function eq eql equal equalp error etypecase eval eval-when evenp every exp export expt
  extended-char fboundp fceiling fdefinition ffloor fifth file-author file-error file-error-pathname file-length file-namestring
  file-position file-stream file-string-length file-write-date fill fill-pointer find find-all-symbols find-class find-if
  find-if-not find-method find-package find-restart find-symbol finish-output first fixnum flet float float-digits float-precision
  float-radix float-sign floating-point-inexact floating-point-invalid-operation floating-point-overflow floating-point-underflow
  floatp floor fmakunbound force-output format formatter fourth fresh-line fround ftruncate ftype funcall function
  function-keywords function-lambda-expression functionp gcd generic-function gensym gentemp get get-decoded-time
  get-dispatch-macro-character get-internal-real-time get-internal-run-time get-macro-character get-output-stream-string
  get-properties get-setf-expansion get-universal-time getf gethash go graphic-char-p handler-bind handler-case hash-table
  hash-table-count hash-table-p hash-table-rehash-size hash-table-rehash-threshold hash-table-size hash-table-test host-namestring
  identity if ignorable ignore ignore-errors imagpart import in-package incf initialize-instance inline input-stream-p inspect
  integer integer-decode-float integer-length integerp interactive-stream-p intern internal-time-units-per-second intersection
  invalid-method-error invoke-debugger invoke-restart invoke-restart-interactively isqrt keyword keywordp labels lambda
  lambda-list-keywords lambda-parameters-limit last lcm ldb ldb-test ldiff least-negative-double-float least-negative-long-float
  least-negative-normalized-double-float least-negative-normalized-long-float least-negative-normalized-short-float
  least-negative-normalized-single-float least-negative-short-float least-negative-single-float least-positive-double-float
  least-positive-long-float least-positive-normalized-double-float least-positive-normalized-long-float
  least-positive-normalized-short-float least-positive-normalized-single-float least-positive-short-float
  least-positive-single-float length let let* lisp-implementation-type lisp-implementation-version list list* list-all-packages
  list-length listen listp load load-logical-pathname-translations load-time-value locally log logand logandc1 logandc2 logbitp
  logcount logeqv logical-pathname logical-pathname-translations logior lognand lognor lognot logorc1 logorc2 logtest logxor
  long-float long-float-epsilon long-float-negative-epsilon long-site-name loop loop-finish lower-case-p machine-instance
  machine-type machine-version macro-function macroexpand macroexpand-1 macrolet make-array make-broadcast-stream
  make-concatenated-stream make-condition make-dispatch-macro-character make-echo-stream make-hash-table make-instance
  make-instances-obsolete make-list make-load-form make-load-form-saving-slots make-method make-package make-pathname
  make-random-state make-sequence make-string make-string-input-stream make-string-output-stream make-symbol make-synonym-stream
  make-two-way-stream makunbound map map-into mapc mapcan mapcar mapcon maphash mapl maplist mask-field max member member-if
  member-if-not merge merge-pathnames method method-combination method-combination-error method-qualifiers min minusp mismatch mod
  most-negative-double-float most-negative-fixnum most-negative-long-float most-negative-short-float most-negative-single-float
  most-positive-double-float most-positive-fixnum most-positive-long-float most-positive-short-float most-positive-single-float
  muffle-warning multiple-value-bind multiple-value-call multiple-value-list multiple-value-prog1 multiple-value-setq
  multiple-values-limit name-char namestring nbutlast nconc next-method-p nil nintersection ninth no-applicable-method
  no-next-method not notany notevery notinline nreconc nreverse nset-difference nset-exclusive-or nstring-capitalize
  nstring-downcase nstring-upcase nsublis nsubst nsubst-if nsubst-if-not nsubstitute nsubstitute-if nsubstitute-if-not nth
  nth-value nthcdr null number numberp numerator nunion oddp open open-stream-p optimize or otherwise output-stream-p package
  package-error package-error-package package-name package-nicknames package-shadowing-symbols package-use-list
  package-used-by-list packagep pairlis parse-error parse-integer parse-namestring pathname pathname-device pathname-directory
  pathname-host pathname-match-p pathname-name pathname-type pathname-version pathnamep peek-char phase pi plusp pop position
  position-if position-if-not pprint pprint-dispatch pprint-exit-if-list-exhausted pprint-fill pprint-indent pprint-linear
  pprint-logical-block pprint-newline pprint-pop pprint-tab pprint-tabular prin1 prin1-to-string princ princ-to-string print
  print-not-readable print-not-readable-object print-object print-unreadable-object probe-file proclaim prog prog* prog1 prog2
  progn program-error progv provide psetf psetq push pushnew quote random random-state random-state-p rassoc rassoc-if
  rassoc-if-not ratio rational rationalize rationalp read read-byte read-char read-char-no-hang read-delimited-list
  read-from-string read-line read-preserving-whitespace read-sequence reader-error readtable readtable-case readtablep real realp
  realpart reduce reinitialize-instance rem remf remhash remove remove-duplicates remove-if remove-if-not remove-method remprop
  rename-file rename-package replace require rest restart restart-bind restart-case restart-name return return-from revappend
  reverse room rotatef round row-major-aref rplaca rplacd safety satisfies sbit scale-float schar search second sequence
  serious-condition set set-difference set-dispatch-macro-character set-exclusive-or set-macro-character set-pprint-dispatch
  set-syntax-from-char setf setq seventh shadow shadowing-import shared-initialize shiftf short-float short-float-epsilon
  short-float-negative-epsilon short-site-name signal signed-byte signum simple-array simple-base-string simple-bit-vector
  simple-bit-vector-p simple-condition simple-condition-format-arguments simple-condition-format-control simple-error
  simple-string simple-string-p simple-type-error simple-vector simple-vector-p simple-warning sin single-float
  single-float-epsilon single-float-negative-epsilon sinh sixth sleep slot-boundp slot-exists-p slot-makunbound slot-missing
  slot-unbound slot-value software-type software-version some sort space special special-operator-p speed sqrt stable-sort
  standard standard-char standard-char-p standard-class standard-generic-function standard-method standard-object step
  storage-condition store-value stream stream-element-type stream-error stream-error-stream stream-external-format streamp string
  string-capitalize string-downcase string-equal string-greaterp string-left-trim string-lessp string-not-equal
  string-not-greaterp string-not-lessp string-right-trim string-stream string-trim string-upcase string/= string< string<= string=
  string> string>= stringp structure structure-class structure-object style-warning sublis subseq subsetp subst subst-if
  subst-if-not substitute substitute-if substitute-if-not subtypep svref sxhash symbol symbol-function symbol-macrolet symbol-name
  symbol-package symbol-plist symbol-value symbolp synonym-stream synonym-stream-symbol t tagbody tailp tan tanh tenth terpri the
  third throw time trace translate-logical-pathname translate-pathname tree-equal truename truncate two-way-stream
  two-way-stream-input-stream two-way-stream-output-stream type type-error type-error-datum type-error-expected-type type-of
  typecase typep unbound-slot unbound-slot-instance unbound-variable undefined-function unexport unintern union unless unread-char
  unsigned-byte untrace unuse-package unwind-protect update-instance-for-different-class update-instance-for-redefined-class
  upgraded-array-element-type upgraded-complex-part-type upper-case-p use-package use-value user-homedir-pathname values
  values-list variable vector vector-pop vector-push vector-push-extend vectorp warn warning when wild-pathname-p with-accessors
  with-compilation-unit with-condition-restarts with-hash-table-iterator with-input-from-string with-open-file with-open-stream
  with-output-to-string with-package-iterator with-simple-restart with-slots with-standard-io-syntax write write-byte write-char
  write-line write-sequence write-string write-to-string y-or-n-p yes-or-no-p zerop
  ))

(setq *package* (find-package "EXT"))

(CL::export
 '(*applyhook* *args* argv *evalhook* evalhook applyhook re-export special-variable-p exit quit
    english simple-condition-format-string
    *load-compiling* notspecial probe-pathname
    mapcap maplap
    *ERROR-HANDLER* *driver* *break-driver* show-stack   
    special-operator load-time-eval symbol-macro function-macro encoding
    gc finalize string-concat
    preliminary stack-overflow-error
    
    weak-pointer make-weak-pointer weak-pointer-p weak-pointer-value                          
    weak-list make-weak-list weak-list-p weak-list-list                                       
    weak-and-relation make-weak-and-relation weak-and-relation-p                              
    weak-and-relation-list                                                                    
    weak-or-relation make-weak-or-relation weak-or-relation-p                                 
    weak-or-relation-list                                                                     
    weak-mapping make-weak-mapping weak-mapping-p weak-mapping-pair                           
    weak-mapping-value                                                                        
    weak-and-mapping make-weak-and-mapping weak-and-mapping-p                                 
    weak-and-mapping-pair weak-and-mapping-value                                              
    weak-or-mapping make-weak-or-mapping weak-or-mapping-p weak-or-mapping-pair               
    weak-or-mapping-value                                                                     
    weak-alist make-weak-alist weak-alist-p weak-alist-type weak-alist-contents               
    weak-alist-assoc weak-alist-rassoc weak-alist-value                                       
    
    expand-form symbol-macro-expand global-symbol-macro
    package-shortest-name package-case-inverted-p package-case-sensitive-p
    
    read-char-sequence write-char-sequence
	read-byte-sequence write-byte-sequence

    
    source-program-error source-program-error-form source-program-error-detail
    package-lock    
    fasthash-eq stablehash-eq fasthash-eql stablehash-eql fasthash-equal stablehash-equal
    getenv shell
    dir delete-directory
    list-length-dotted list-length-proper proper-list-p elastic-newline
    base-char-code-limit
    defun-ff
    compiler-let
    
    ))
    
(setq *package* (find-package "CLOS"))

;;; Exports:
(CL::export
 '(;; names of functions and macros:
 		metaobject
          ;; MOP for dependents
          add-dependent remove-dependent map-dependents update-dependent
          ;; MOP for slot definitions
          slot-definition standard-slot-definition
          direct-slot-definition standard-direct-slot-definition
          effective-slot-definition standard-effective-slot-definition
          slot-definition-name
          slot-definition-initform slot-definition-initfunction
          slot-definition-type slot-definition-allocation
          slot-definition-initargs
          slot-definition-readers slot-definition-writers
          slot-definition-location
          ;; MOP for slot access
          slot-value-using-class slot-boundp-using-class
          slot-makunbound-using-class
          standard-instance-access funcallable-standard-instance-access
          ;; MOP for classes
          class forward-referenced-class
          built-in-class structure-class standard-class
          class-name class-direct-superclasses class-precedence-list
          class-direct-subclasses class-direct-slots class-slots
          class-direct-default-initargs class-default-initargs class-prototype
          class-finalized-p finalize-inheritance
          compute-direct-slot-definition-initargs direct-slot-definition-class
          compute-class-precedence-list
          compute-slots compute-effective-slot-definition
          compute-effective-slot-definition-initargs
          effective-slot-definition-class
          compute-default-initargs
          validate-superclass add-direct-subclass remove-direct-subclass
          standard-accessor-method
          standard-reader-method standard-writer-method
          reader-method-class writer-method-class
          ensure-class ensure-class-using-class
          ;; MOP for specializers
          specializer eql-specializer
          specializer-direct-generic-functions specializer-direct-methods
          add-direct-method remove-direct-method
          eql-specializer-object intern-eql-specializer
          ;; MOP for methods
          method standard-method
          method-function method-generic-function method-lambda-list
          method-specializers method-qualifiers accessor-method-slot-definition
          extract-lambda-list extract-specializer-names
          ;; MOP for method combinations
          find-method-combination compute-effective-method
          ;; MOP for generic functions
          funcallable-standard-class funcallable-standard-object
          set-funcallable-instance-function
          generic-function-name generic-function-methods
          generic-function-method-class generic-function-lambda-list
          generic-function-method-combination
          generic-function-argument-precedence-order
          generic-function-declarations
          compute-discriminating-function
          compute-applicable-methods compute-applicable-methods-using-classes
          compute-effective-method-as-function
          ensure-generic-function ensure-generic-function-using-class
          ;; CLISP specific symbols
          generic-flet generic-labels no-primary-method
          method-call-error method-call-type-error
          method-call-error-generic-function
          method-call-error-method method-call-error-argument-list
          standard-stablehash structure-stablehash
          clos-warning gf-already-called-warning gf-replacing-method-warning
 
   slot-value slot-boundp slot-makunbound slot-exists-p with-slots
   with-accessors documentation
   find-class class-of defclass defmethod call-next-method next-method-p
   defgeneric generic-function
   no-applicable-method no-next-method
   find-method add-method remove-method
   compute-applicable-methods method-qualifiers function-keywords
   slot-missing slot-unbound
   print-object describe-object
   make-instance allocate-instance initialize-instance reinitialize-instance
   shared-initialize ensure-generic-function
   make-load-form make-load-form-saving-slots
   change-class update-instance-for-different-class
   update-instance-for-redefined-class make-instances-obsolete
   ;; names of classes:
   class standard-class structure-class built-in-class
   standard-object structure-object
   generic-function standard-generic-function method standard-method   
   ;; other symbols:
   standard
   
   )) ; method combination

(setq *package* (find-package "CUSTOM"))

(CL::export '(*suppress-check-redefinition* *load-paths* *report-error-print-backtrace* *user-commands* *compiled-file-types*))

(CL::setq CL::*package* (CL::find-package "CHARSET"))
(CL::export '(utf-8))


(CL::setq CL::*package* (CL::find-package "CL"))
