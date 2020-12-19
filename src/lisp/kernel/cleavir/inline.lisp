(in-package :clasp-cleavir)

(defpackage "PRIMOP"
  (:export #:inlined-two-arg-+
           #:inlined-two-arg--
           #:inlined-two-arg-*
           #:inlined-two-arg-/
           #:inlined-two-arg-<
           #:inlined-two-arg-<=
           #:inlined-two-arg-=
           #:inlined-two-arg->
           #:inlined-two-arg->=
           ))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (setq core:*defun-inline-hook* 'defun-inline-hook))

;;;; Standard CL function types

;;; Install ftypes and types ASAP so that the bootstrap benefits from
;;; type information.

(deftype function-name ()
  '(or symbol (cons (eql setf) (cons symbol null))))

(deftype lambda-expression ()
  '(cons (eql lambda) (cons list t)))

(deftype compound-form ()
  '(cons (or symbol lambda-expression)))

(deftype form ()
  '(or symbol compound-form (not (or symbol cons))))

(deftype environment () 't)

(deftype macro-function () '(function (form environment) *))
(deftype compiler-macro-function () 'macro-function)

(deftype declaration-specifier () '(cons symbol))

(deftype type-specifier ()
  '(or symbol (cons symbol) class))

(deftype maybe (type) `(or null ,type))

(deftype sequence-index () 'sys:index)
(deftype end-index-designator () '(maybe sequence-index))

(deftype valid-array-dimension ()
  `(integer 0 (,array-dimension-limit)))
(deftype valid-array-index ()
  `(integer 0 (,(1- array-dimension-limit))))
(deftype valid-array-size ()
  `(integer 0 (,array-total-size-limit)))
(deftype valid-row-major-index ()
  `(integer 0 (,(1- array-total-size-limit))))
(deftype valid-array-rank ()
  `(integer 0 (,array-rank-limit)))
(deftype valid-array-axis ()
  `(integer 0 (,(1- array-rank-limit))))

(deftype function-designator (&optional lambda-list return-values)
  `(or symbol (function ,lambda-list ,return-values)))
(deftype extended-function-designator (&optional lambda-list return-values)
  `(or function-name (function ,lambda-list ,return-values)))

(deftype predicate-function () '(function (t) boolean))
(deftype predicate () '(function-designator (t)))

(deftype class-designator () '(or class symbol))

(deftype format-control ()
  ;; FORMATTER returns functions like this.
  '(or string (function (stream &rest t) list)))

;; NOTE: In CLHS, "condition designator" actually means what we denote
;; (condition-designator &rest t). It's more than one object.
(deftype condition-designator () '(or symbol format-control condition))
(deftype restart-designator () '(or restart (and symbol (not null))))
(deftype character-designator () '(or character symbol (string 1)))
(deftype string-designator () '(or string symbol character))
(deftype package-designator () '(or package string-designator))
(deftype list-designator (&optional (element-type t)) `(or list ,element-type))
(deftype pathname-designator () '(or string stream pathname))
(deftype external-file-format-designator () 't) ; implementation dependent
(deftype stream-designator () '(or stream boolean))
(deftype file-position-designator () '(or (member :start :end) (integer 0)))
(deftype readtable-designator () '(or null readtable))

(deftype radix () '(integer 2 36))
(deftype boole-spec () 't) ; implementation defined
(deftype byte-specifier () 't) ; implementation defined

(deftype character-code () `(integer 0 (,char-code-limit)))

(deftype key () '(maybe predicate))
(deftype test () '(function-designator (t t)))

(deftype sequence-function (positional-parameters more-keys return-values)
  `(function (,@positional-parameters &key (:from-end t) (:key key)
                                      (:start sequence-index) (:end end-index-designator)
                                      ,@more-keys)
             ,return-values))
(deftype test-sequence-function (positional-parameters more-keys return-values)
  `(sequence-function ,positional-parameters ((:test test) (:test-not test)
                                              ,@more-keys)
                      ,return-values))

(deftype valid-physical-pathname-host ()
  '(or string list (eql :unspecific)))
(deftype valid-pathname-device () 't) ; impl defined
(deftype valid-pathname-directory () 't) ; impl defined
(deftype valid-pathname-name () 't) ; impl defined
(deftype valid-pathname-type () '(or string (member nil :wild :unspecific)))
(deftype valid-pathname-version () '(or (integer 0) (member :wild :newest :unspecific nil)))
(deftype pathname-case () '(member :common :local))
(deftype logical-host () 't) ; impl defined
(deftype logical-host-designator () '(or string logical-host))

(deftype physical-pathname () '(and pathname (not logical-pathname)))

(deftype pprint-dispatch-table () 't) ; impl defined, i think

(deftype write-function (&optional ret)
  `(function (t &key (:array t) (:base radix)
                (:case (member :upcase :downcase :capitalize))
                (:circle t) (:escape t) (:gensym t)
                (:length (maybe (integer 0))) (:level (maybe (integer 0)))
                (:lines (maybe (integer 0))) (:miser-width (maybe (integer 0)))
                (:pprint-dispatch pprint-dispatch-table)
                (:pretty t) (:radix t) (:readably t)
                (:right-margin (maybe (integer 0)))
                (:stream stream-designator))
             ,ret))

(deftype case-sensitivity-mode () '(or :upcase :downcase :preserve :invert))

(deftype universal-time () '(integer 0))
;; CLHS specifies time zones must be "rational multiples" of 1/3600.
;; Presumably the intent is that the denominator is a divisor of 3600,
;; (so no time zones that are a seventh of an hour less than GMT),
;; but that's inexpressible in the CL type system anyway.
(deftype time-zone () '(rational -24 24))
(deftype second () '(integer 0 59))
(deftype minute () '(integer 0 59))
(deftype hour () '(integer 0 23))
(deftype date () '(integer 1 31)) ; "actually depends on the month and year, of course"
(deftype month () '(integer 1 12))
(deftype year () '(integer 0))
(deftype day-of-week () '(integer 0 6))
(deftype dst-flag () 't)

;;; Chapter 3 Evaluation and Compilation

(declaim (ftype (function ((maybe function-name) &optional (or lambda-expression function))
                          (values (or function-name compiled-function) t t))
                compile)
         (ftype (function (form) *) eval)
         (ftype (function (function-name &optional environment)
                          (maybe compiler-macro-function))
                compiler-macro-function)
         (ftype (function ((maybe compiler-macro-function) function-name &optional environment)
                          (maybe compiler-macro-function))
                (setf compiler-macro-function))
         (ftype (function (function-name &optional environment)
                          (maybe macro-function))
                macro-function)
         (ftype (function ((maybe macro-function) function-name &optional environment)
                          (maybe macro-function))
                (setf macro-function))
         (ftype (function (form &optional environment) (values form t))
                macroexpand macroexpand-1)
         (ftype (function (declaration-specifier) *) proclaim)
         (ftype (function (symbol) t) special-operator-p)
         (ftype (function (form &optional environment) t) constantp))

;;; Chapter 4 Types and Classes

(declaim (ftype (function (t type-specifier) t) coerce)
         (ftype (function (type-specifier type-specifier &optional environment)
                          (values t t))
                subtypep)
         (ftype (function (t) type-specifier) type-of)
         (ftype (function (t type-specifier &optional environment) t) typep)
         (ftype (function (type-error) t) type-error-datum)
         (ftype (function (type-error) type-specifier) type-error-expected-type))

;;; Chapter 5 Data and Control Flow

(declaim (ftype (function (function-designator &rest t)) apply)
         (ftype (function (function-name) t) fdefinition) ; special-form/macro case. fixme
         (ftype (function (function function-name) function) (setf fdefinition))
         (ftype (function (function-name) t) fboundp)
         (ftype (function (function-name) function-name) fmakunbound)
         (ftype (function (function-designator &rest t)) funcall)
         (ftype (function (function) (values (maybe lambda-expression) t t))
                function-lambda-expression)
         (ftype predicate-function functionp)
         (ftype predicate-function compiled-function-p)
         (ftype (function (t) boolean) not)
         (ftype (function (t t) t) eq eql equal equalp)
         (ftype (function (t) t) function)
         (ftype (function (function) function) complement)
         (ftype (function (t) function) constantly)
         (ftype (function (function-designator &rest sequence) t)
                every some notevery notany)
         (ftype (function (&rest t)) values)
         (ftype (function (list)) values-list)
         (ftype (function (form &optional environment)
                          (values list list list form form))
                get-setf-expansion))

;;; Chapter 6 Iteration
;;; no functions

;;; Chapter 7 Objects

(declaim (ftype (function (method) (values list t)) function-keywords)
         (ftype (function (function-name &key &allow-other-keys) generic-function)
                ensure-generic-function) ; could add keys
         (ftype (function (class &key &allow-other-keys) t) allocate-instance)
         (ftype (function (t &key &allow-other-keys) t) initialize-instance)
         (ftype (function (t &key &allow-other-keys) t) reinitialize-instance)
         (ftype (function (t (or list (eql t)) &key &allow-other-keys) t)
                shared-initialize)
         (ftype (function (t t &key &allow-other-keys))
                update-instance-for-different-class)
         (ftype (function (t list list list &key &allow-other-keys) t)
                update-instance-for-redefined-class)
         (ftype (function (t class-designator &key &allow-other-keys) t)
                change-class)
         (ftype (function (t symbol) t)
                slot-boundp slot-exists-p slot-makunbound slot-value)
         (ftype (function
                 (class t symbol (member setf slot-boundp slot-makunbound slot-value) &optional t)
                 t)
                slot-missing)
         (ftype (function (class t symbol) t) slot-unbound)
         (ftype (function (method) list) method-qualifiers)
         (ftype (function (generic-function &rest t)) no-applicable-method)
         (ftype (function (generic-function method &rest t)) no-next-method)
         (ftype (function (generic-function method) generic-function)
                remove-method add-method)
         (ftype (function (class-designator &key &allow-other-keys) t) make-instance)
         (ftype (function (class-designator) class-designator) make-instances-obsolete)
         (ftype (function (t &optional environment) (values form &optional form))
                make-load-form)
         (ftype (function (t &key (:slot-names list) (:environment environment))
                          (values form form))
                make-load-form-saving-slots)
         (ftype (function (symbol &optional t environment) (maybe class)) find-class)
         (ftype (function ((maybe class) symbol &optional t environment) (maybe class))
                (setf find-class))
         (ftype (function () t) next-method-p) ; ignored by clasp
         (ftype (function (&rest t)) call-next-method) ; ditto
         (ftype (function (generic-function list) list) compute-applicable-methods)
         (ftype (function (generic-function list list &optional t) (maybe method))
                find-method)
         (ftype (function (class) symbol) class-name)
         (ftype (function (symbol class) symbol) (setf class-name))
         (ftype (function (t) class) class-of)
         (ftype (function (unbound-slot) t) unbound-slot-instance))

;;; Chapter 8 Structures

(declaim (ftype (function (structure-object) structure-object) copy-structure))

;;; Chapter 9 Conditions

(declaim (ftype (function (cell-error) t) cell-error-name)
         (ftype (function (condition-designator &rest t) nil) error)
         (ftype (function (format-control condition-designator &rest t) null) cerror)
         (ftype (function (method format-control &rest t)) invalid-method-error)
         (ftype (function (format-control &rest t)) method-combination-error)
         (ftype (function (condition-designator &rest t) null) signal warn)
         (ftype (function (condition) format-control) simple-condition-format-control)
         (ftype (function (condition) list) simple-condition-format-arguments)
         (ftype (function (condition) nil) invoke-debugger)
         (ftype (function (&optional format-control &rest t) null) break)
         (ftype (function (type-specifier &key &allow-other-keys) condition) make-condition)
         (ftype (function (&optional (maybe condition)) list) compute-restarts)
         (ftype (function (restart-designator &optional (maybe condition))
                          (maybe restart))
                find-restart)
         (ftype (function (restart-designator &rest t)) invoke-restart)
         (ftype (function (restart-designator)) invoke-restart-interactively)
         (ftype (function (restart) symbol) restart-name)
         (ftype (function (&optional (maybe condition)) nil) abort muffle-warning)
         (ftype (function (&optional (maybe condition)) null) continue store-value use-value))

;;; Chapter 10 Symbols

(declaim (ftype predicate-function symbolp keywordp boundp)
         (ftype (function (string) symbol) make-symbol)
         (ftype (function (symbol &optional t) symbol) copy-symbol)
         (ftype (function (&optional (or string (integer 0))) symbol) gensym)
         (ftype (function (&optional string package-designator) symbol) gentemp)
         (ftype (function (symbol)) symbol-function) ; see FDEFINITION note
         (ftype (function (function symbol) function) (setf symbol-function))
         (ftype (function (symbol) string) symbol-name)
         (ftype (function (symbol) (maybe package)) symbol-package)
         (ftype (function (symbol) list) symbol-plist)
         (ftype (function (list symbol) list) (setf symbol-plist))
         (ftype (function (symbol) t) symbol-value)
         (ftype (function (t symbol) t) (setf symbol-value))
         (ftype (function (symbol t &optional t) t) get)
         (ftype (function (t symbol t &optional t) t) (setf get))
         (ftype (function (symbol t) t) remprop)
         (ftype (function (symbol) symbol) makunbound)
         (ftype (function (symbol t) t) set))

;;; Chapter 11 Packages

(declaim (ftype (function ((list-designator symbol) &optional package-designator) (eql t))
                export import shadowing-import unexport)
         (ftype (function (string &optional package-designator)
                          (values symbol (member :inherited :internal :external nil)))
                find-symbol intern)
         (ftype (function ((or package string-designator)) (maybe package)) find-package)
         (ftype (function (string-designator) list) find-all-symbols)
         (ftype (function () list) list-all-packages)
         (ftype (function (package-designator package-designator &optional list) package)
                rename-package)
         (ftype (function ((list-designator string-designator) &optional package-designator) (eql t))
                shadow)
         (ftype (function (package-designator) t) delete-package)
         (ftype (function (string-designator &key (:nicknames list) (:use list)) package)
                make-package)
         (ftype (function (symbol &optional package-designator) t) unintern)
         (ftype (function ((list-designator package-designator) &optional package-designator) (eql t))
                use-package unuse-package)
         (ftype (function (package-designator) (maybe string)) package-name)
         (ftype (function (package-designator) list)
                package-nicknames package-shadowing-symbols package-use-list package-used-by-list)
         (ftype predicate-function packagep)
         (ftype (function (package-error) package-designator) package-error-package))

;;; Chapter 12 Numbers

(declaim (ftype (function (&rest number) t) = /=)
         (ftype (function (&rest real) t) < > <= >=)
         (ftype (function (&rest real) real) max min)
         (ftype (function (real) t) plusp minusp)
         (ftype (function (number) t) zerop)
         (ftype (function (real &optional real) (values integer real))
                ;; note: the optional real is nonzero. FIXME?
                floor ceiling truncate round)
         (ftype (function (real &optional real) (values float real))
                ffloor fceiling ftruncate fround)
         (ftype (function (number) number)
                sin cos tan asin acos sinh cosh tanh asinh acosh atanh
                1+ 1- exp signum sqrt conjugate phase)
         (ftype (function (number &optional number) number) atan)
         (ftype (function (&rest number) number) * +)
         ;; note: for /, the &rest are nonzero
         (ftype (function (number &rest number) - /))
         (ftype (function (number) real) abs realpart imagpart)
         (ftype (function (integer) t) evenp oddp)
         (ftype (function (number number) number) expt)
         (ftype (function (&rest integer) (integer 0)) gcd lcm)
         ;; note: first argument is nonzero
         (ftype (function (number &optional number) number) log)
         (ftype (function (real real) real) mod rem)
         (ftype (function ((integer 0)) (integer 0)) isqrt)
         (ftype (function (&optional (or random-state boolean)) random-state) make-random-state)
         (ftype (function ((or (integer (0)) (float (0.0))) &optional random-state)
                          (or (float 0.0) (integer 0)))
                random)
         (ftype predicate-function random-state-p numberp complexp realp rationalp integerp floatp)
         (ftype (function (real) complex) cis)
         (ftype (function (real &optional real) (or rational complex)) complex)
         (ftype (function (type-specifier &optional environment) type-specifier)
                upgraded-complex-part-type)
         (ftype (function (rational) integer) numerator)
         (ftype (function (rational) (integer (0))) denominator)
         (ftype (function (real) rational) rational rationalize)
         (ftype (function (integer integer) integer)
                ash logandc1 logandc2 lognand lognor logorc1 logorc2)
         (ftype (function (integer) (integer 0)) integer-length logcount)
         (ftype (function (string &key (:start sequence-index) (:end end-index-designator)
                                  (:radix radix) (:junk-allowed t))
                          (values (maybe integer) sequence-index))
                parse-integer)
         (ftype (function (boole-spec integer integer) integer) boole)
         (ftype (function (&rest integer) integer) logand logeqv logior logxor)
         (ftype (function (integer) integer) lognot)
         (ftype (function ((integer 0) integer) t) logbitp)
         (ftype (function (integer integer) t) logtest)
         (ftype (function ((integer 0) (integer 0)) byte-specifier) byte)
         (ftype (function (byte-specifier) (integer 0)) byte-size byte-position)
         (ftype (function (integer byte-specifier integer) integer) deposit-field dpb)
         (ftype (function (byte-specifier integer) (integer 0)) ldb mask-field)
         (ftype (function ((integer 0) byte-specifier integer) (integer 0))
                (setf ldb) (setf mask-field))
         (ftype (function (byte-specifier integer) t) ldb-test)
         (ftype (function (float) (values float integer float)) decode-float)
         (ftype (function (float integer) float) scale-float)
         (ftype (function (float) integer) float-radix)
         (ftype (function (float &optional float) float) float-sign)
         (ftype (function (float) (integer 0)) float-digits float-precision)
         (ftype (function (float) (values integer integer (member -1 1))) integer-decode-float)
         (ftype (function (real &optional float) float) float)
         (ftype (function (arithmetic-error) list) arithmetic-error-operands)
         (ftype (function (arithmetic-error) function-designator) arithmetic-error-operation))

;;; Chapter 13 Characters

(declaim (ftype (function (&rest character) t)
                char= char/= char< char> char<= char>=
                char-equal char-not-equal char-lessp char-greaterp
                char-not-greaterp char-not-lessp)
         (ftype (function (character-designator) character) character)
         (ftype predicate-function characterp)
         (ftype (function (character) t)
                alpha-char-p alphanumericp graphic-char-p standard-char-p
                upper-case-p lower-case-p both-case-p)
         (ftype (function ((integer 0) &optional radix) (maybe character)) digit-char)
         (ftype (function (character &optional radix) (maybe (integer 0 36))) digit-char-p)
         (ftype (function (character) character) char-upcase char-downcase)
         (ftype (function (character) character-code) char-code)
         (ftype (function (character) (integer 0)) char-int)
         (ftype (function (character-code) (maybe character)) code-char)
         (ftype (function (character) (maybe string)) char-name)
         (ftype (function (string-designator) (maybe character)) name-char))

;;; Chapter 14 Conses

(declaim (ftype (function (t t) cons) cons)
         (ftype predicate-function consp atom listp null)
         (ftype (function (cons t) cons) rplaca rplacd)
         (ftype (function (list) t)
                car cdr caar cadr cdar cddr caaar caadr cadar cdaar caddr cdadr cddar cdddr
                caaaar caaadr caadar cadaar cdaaar caaddr cadadr cdaadr caddar cdadar cddaar
                cadddr cdaddr cddadr cdddar cddddr
                first second third fourth fifth sixth seventh eighth ninth tenth rest)
         (ftype (function (t list) t)
                (setf car) (setf cdr) (setf caar) (setf cadr) (setf cdar) (setf cddr)
                (setf caaar) (setf caadr) (setf cadar) (setf cdaar) (setf caddr) (setf cdadr)
                (setf cddar) (setf cdddr) (setf caaaar) (setf caaadr) (setf caadar)
                (setf cadaar) (setf cdaaar) (setf caaddr) (setf cadadr) (setf cdaadr)
                (setf caddar) (setf cdadar) (setf cddaar) (setf cadddr) (setf cdaddr)
                (setf cddadr) (setf cdddar) (setf cddddr) (setf rest)
                (setf first) (setf second) (setf third) (setf fourth) (setf fifth)
                (setf sixth) (setf seventh) (setf eighth) (setf ninth) (setf tenth))
         (ftype (function (t) t) copy-tree)
         (ftype (function (list t &key (:key key) (:test test) (:test-not test)) t)
                sublis nsublis)
         (ftype (function (t t t &key (:key key)) t)
                subst-if subst-if-not nsubst-if nsubst-if-not)
         (ftype (function (t t &key (:test test) (:test-not test)) t) tree-equal)
         (ftype (function (list) list) copy-list)
         (ftype (function (&rest t) list) list)
         (ftype (function (&rest t) t) list*)
         (ftype (function (list) (maybe (integer 0))) list-length)
         (ftype (function ((integer 0) &key (:initial-element t)) list) make-list)
         (ftype (function ((integer 0) list) t) nth nthcdr)
         (ftype (function (t (integer 0) list) t) (setf nth))
         (ftype (function (list) t) endp)
         (ftype (function (&rest t) list) nconc)
         (ftype (function (&rest t) append) t)
         (ftype (function (list t) list) revappend nreconc)
         (ftype (function (list &optional (integer 0)) list) butlast nbutlast)
         (ftype (function (list &optional (integer 0)) t) last)
         (ftype (function (list t) list) ldiff)
         (ftype (function (t list) t) tailp)
         (ftype (function (t list &key (:key key) (:test test) (:test-not test)) list) member)
         (ftype (function (predicate list &key (:key key)) list) member-if member-if-not)
         (ftype (function (function-designator list &rest list) list)
                mapc mapcar mapcan mapl maplist mapcon)
         (ftype (function (t t list) list) acons)
         (ftype (function (t list &key (:key key) (:test test) (:test-not test)) (maybe cons))
                assoc rassoc)
         (ftype (function (predicate list &key (:key key)) (maybe cons))
                assoc-if assoc-if-not rassoc-if rassoc-if-not)
         (ftype (function (list) list) copy-alist)
         (ftype (function (list list &optional list) list) pairlis)
         (ftype (function (list list) (values t t list)) get-properties)
         (ftype (function (list t &optional t) t) getf)
         (ftype (function (t list t &optional t) t) (setf getf))
         (ftype (function (list list &key (:key key) (:test test) (:test-not test)) list)
                intersection nintersection set-difference nset-difference
                set-exclusive-or nset-exclusive-or union nunion)
         (ftype (function (t list &key (:key key) (:test test) (:test-not test)) list) adjoin)
         (ftype (function (list list &key (:key key) (:test test) (:test-not test)) t) subsetp))

;;; Chapter 15 Arrays

(declaim (ftype (function ((list-designator valid-array-dimension)
                           &key (:element-type type-specifier) (:initial-element t)
                           (:initial-contents t) (:adjustable t)
                           (:fill-pointer (or boolean valid-row-major-index))
                           (:displaced-to (maybe array))
                           (:displaced-index-offset valid-row-major-index))
                          array)
                make-array)
         (ftype (function (array (list-designator valid-array-dimension)
                                 &key (:element-type type-specifier) (:initial-element t)
                                 (:initial-contents t) (:adjustable t)
                                 (:fill-pointer (or boolean valid-row-major-index))
                                 (:displaced-to (maybe array))
                                 (:displaced-index-offset valid-row-major-index))
                          array)
                adjust-array)
         (ftype (function (array) t) adjustable-array-p array-has-fill-pointer-p)
         (ftype (function (array &rest valid-array-index) t) aref)
         (ftype (function (t array &rest valid-array-index) t) (setf aref))
         (ftype (function (array valid-array-axis) valid-array-dimension) array-dimension)
         (ftype (function (array) list) array-dimensions)
         (ftype (function (array) type-specifier) array-element-type)
         (ftype (function (array) (values (maybe array) valid-row-major-index))
                array-displacement)
         (ftype (function (array &rest integer) t) array-in-bounds-p)
         (ftype (function (array) valid-array-rank) array-rank)
         (ftype (function (array &rest valid-array-index) valid-row-major-index)
                array-row-major-index)
         (ftype (function (array) valid-array-size) array-total-size)
         (ftype predicate-function arrayp simple-vector-p vectorp bit-vector-p simple-bit-vector-p)
         (ftype (function (vector) valid-array-index) fill-pointer)
         (ftype (function (valid-array-index vector) valid-array-index) (setf fill-pointer))
         (ftype (function (array valid-row-major-index) t) row-major-aref)
         (ftype (function (t array valid-row-major-index) t) (setf row-major-aref))
         (ftype (function (type-specifier &optional environment) type-specifier)
                upgraded-array-element-type)
         (ftype (function (simple-vector valid-array-index) t) svref)
         (ftype (function (t simple-vector valid-array-index) t) (setf svref))
         (ftype (function (&rest t) (vector t)) vector)
         (ftype (function (vector) t) vector-pop)
         (ftype (function (t vector) (maybe valid-array-index)) vector-push)
         (ftype (function (t vector &optional (integer 0)) valid-array-index) vector-push-extend)
         (ftype (function ((array bit) &rest valid-array-index) bit) bit)
         (ftype (function (bit (array bit) &rest valid-array-index) bit) (setf bit))
         (ftype (function ((simple-array bit) &rest valid-array-index) bit) sbit)
         (ftype (function (bit (simple-array bit) &rest valid-array-index) bit) (setf sbit))
         (ftype (function ((array bit) (array bit) &optional (or boolean (array bit))) (array bit))
                bit-and bit-andc1 bit-andc2 bit-eqv bit-ior bit-nand bit-nor
                bit-orc1 bit-orc2 bit-xor)
         (ftype (function ((array bit) &optional (or boolean (array bit))) (array bit)) bit-not))

;;; Chapter 16 Strings

(declaim (ftype predicate-function simple-string-p stringp)
         (ftype (function (string valid-array-index) character) char)
         (ftype (function (character string valid-array-index) character) (setf char))
         (ftype (function (simple-string valid-array-index) character) schar)
         (ftype (function (character simple-string valid-array-index) character) (setf schar))
         (ftype (function (string-designator) string) string)
         (ftype (function (string-designator &key
                                             (:start sequence-index) (:end end-index-designator))
                          string)
                string-upcase string-downcase string-capitalize)
         (ftype (function (string &key (:start sequence-index) (:end end-index-designator)) string)
                nstring-upcase nstring-downcase nstring-capitalize)
         (ftype (function (sequence string-designator) string)
                string-trim string-left-trim string-right-trim)
         (ftype (function (string-designator string-designator
                                             &key (:start1 sequence-index)
                                             (:end1 end-index-designator)
                                             (:start2 sequence-index)
                                             (:end2 end-index-designator))
                          (maybe sequence-index))
                string/= string< string> string<= string>=
                string-not-equal string-lessp string-greaterp
                string-not-greaterp string-not-lessp)
         (ftype (function (string-designator string-designator
                                             &key (:start1 sequence-index)
                                             (:end1 end-index-designator)
                                             (:start2 sequence-index)
                                             (:end2 end-index-designator))
                          t)
                string= string-equal)
         (ftype (function (valid-array-dimension &key (:initial-element character)
                                                 (:element-type type-specifier))
                          simple-string)
                make-string))

;;; Chapter 17 Sequences

(declaim (ftype (function (sequence) sequence) copy-seq)
         (ftype (function (sequence sequence-index) t) elt)
         (ftype (function (t sequence sequence-index) t) (setf elt))
         (ftype (function (sequence t &key (:start sequence-index) (:end end-index-designator))
                          sequence)
                fill) ; clhs bug: says item must be a sequence
         (ftype (function (type-specifier (integer 0) &key (:initial-element t)) sequence)
                make-sequence)
         (ftype (function (sequence sequence-index &optional end-index-designator) sequence)
                subseq)
         (ftype (function (sequence sequence sequence-index &optional end-index-designator)
                          sequence)
                (setf subseq))
         (ftype (function (type-specifier function-designator sequence &rest sequence)
                          (or sequence null))
                map)
         (ftype (function (sequence function-designator &rest sequence) sequence) map-into)
         (ftype (sequence-function (function-designator sequence) ((:initial-value t)) t)
                reduce)
         (ftype (test-sequence-function (t sequence) () (integer 0)) count)
         (ftype (sequence-function (predicate sequence) () (integer 0))
                count-if count-if-not)
         (ftype (function (sequence) (integer 0)) length)
         (ftype (function (sequence) sequence) reverse nreverse)
         (ftype (function (sequence (function-designator (t t)) &key (:key key)) sequence)
                sort stable-sort)
         (ftype (test-sequence-function (t sequence) () t) find)
         (ftype (sequence-function (predicate sequence) () t)
                find-if find-if-not)
         (ftype (test-sequence-function (t sequence) () (maybe sequence-index)) position)
         (ftype (sequence-function (predicate sequence) () (maybe sequence-index))
                position-if position-if-not)
         (ftype (function (sequence sequence &key (:from-end t) (:key key)
                                    (:test test) (:test-not test)
                                    (:start1 sequence-index) (:end1 end-index-designator)
                                    (:start2 sequence-index) (:end2 end-index-designator))
                          (maybe sequence-index))
                search mismatch)
         (ftype (function (sequence sequence &key
                                    (:start1 sequence-index) (:end1 end-index-designator)
                                    (:start2 sequence-index) (:end2 end-index-designator))
                          sequence)
                replace)
         (ftype (test-sequence-function (t t sequence) ((:count (maybe integer))) sequence)
                substitute nsubstitute)
         (ftype (sequence-function (t t sequence) ((:count (maybe integer))) sequence)
                substitute-if substitute-if-not nsubstitute-if nsubstitute-if-not)
         (ftype (function (type-specifier &rest sequence) sequence) concatenate)
         (ftype (function (type-specifier sequence sequence (function-designator (t t))
                                          &key (:key key))
                          sequence)
                merge)
         (ftype (test-sequence-function (t sequence) ((:count (maybe integer))) sequence)
                remove delete)
         (ftype (sequence-function (predicate sequence) ((:count (maybe integer))) sequence)
                remove-if remove-if-not delete-if delete-if-not)
         (ftype (test-sequence-function (sequence) () sequence)
                remove-duplicates delete-duplicates))

;;; Chapter 18 Hash Tables

(declaim (ftype (function (&key (:test (function-designator (t t))) (:size (integer 0))
                                (:rehash-size (or (integer 1) (float (1.0))))
                                (:rehash-threshold (real 0 1)))
                          hash-table)
                make-hash-table)
         (ftype predicate-function hash-table-p)
         (ftype (function (hash-table) (integer 0)) hash-table-count hash-table-size)
         (ftype (function (hash-table) (or (integer 1) (float (1.0)))) hash-table-rehash-size)
         (ftype (function (hash-table) (real 0 1)) hash-table-rehash-threshold)
         (ftype (function (hash-table) (function-designator (t t))) hash-table-test)
         (ftype (function (t hash-table &optional t) (values t t)) gethash)
         (ftype (function (t t hash-table &optional t) t) (setf gethash))
         (ftype (function (t hash-table) t) remhash)
         (ftype (function ((function-designator (t t)) hash-table) null) maphash)
         (ftype (function (hash-table) hash-table) clrhash)
         (ftype (function (t) (and fixnum (integer 0))) sxhash))

;;; Chapter 19 Filenames

(declaim (ftype (function (pathname-designator) pathname) pathname)
         (ftype (function (&key (:host valid-physical-pathname-host)
                                (:device valid-pathname-device)
                                (:directory valid-pathname-directory)
                                (:name valid-pathname-name)
                                (:type valid-pathname-type)
                                (:version valid-pathname-version)
                                (:defaults pathname-designator)
                                (:case pathname-case))
                          pathname)
                make-pathname)
         (ftype predicate-function pathnamep)
         (ftype (function (pathname-designator &key (:case pathname-case)) valid-physical-pathname-host)
                pathname-host)
         (ftype (function (pathname-designator &key (:case pathname-case)) valid-pathname-device)
                pathname-device)
         (ftype (function (pathname-designator &key (:case pathname-case)) valid-pathname-directory)
                pathname-directory)
         (ftype (function (pathname-designator &key (:case pathname-case)) valid-pathname-name)
                pathname-name)
         (ftype (function (pathname-designator &key (:case pathname-case)) valid-pathname-type)
                pathname-type)
         (ftype (function (pathname-designator) valid-pathname-version) pathname-version)
         (ftype (function (string) t) load-logical-pathname-translations)
         (ftype (function (logical-host-designator) list) logical-pathname-translations)
         (ftype (function (list logical-host-designator) list)
                (setf logical-pathname-translations))
         (ftype (function ((or logical-pathname string stream)) logical-pathname)
                logical-pathname)
         (ftype (function (pathname-designator) (maybe string))
                namestring file-namestring directory-namestring host-namestring)
         (ftype (function (pathname-designator &optional pathname-designator) (maybe string))
                enough-namestring)
         (ftype (function ((or string pathname stream)
                           &optional (or valid-physical-pathname-host logical-host null)
                           pathname-designator
                           &key (:start sequence-index) (:end end-index-designator)
                           (:junk-allowed t))
                          (values (maybe pathname) sequence-index))
                parse-namestring)
         (ftype (function (pathname-designator
                           &optional (member :host :device :directory :name :type nil))
                          t)
                wild-pathname-p)
         (ftype (function (pathname-designator pathname-designator) t) pathname-match-p)
         ;; valid keyword arguments are implementation-defined
         (ftype (function ((or pathname-designator string) &key &allow-other-keys) pathname)
                translate-logical-pathname)
         ;; ditto
         (ftype (function (pathname-designator pathname-designator pathname-designator
                                               &key &allow-other-keys)
                          pathname)
                translate-pathname)
         (ftype (function (pathname-designator &optional pathname-designator valid-pathname-version)
                          pathname)
                merge-pathnames))

;;; Chapter 20 Files

;; keys are impl defined
(declaim (ftype (function (pathname-designator &key &allow-other-keys) list) directory)
         (ftype (function (pathname-designator) (maybe pathname)) probe-file)
         (ftype (function (pathname-designator &key (:verbose t))
                          (values pathname-designator t))
                ensure-directories-exist)
         (ftype (function (pathname-designator) physical-pathname) truename)
         (ftype (function (pathname-designator) (maybe string)) file-author)
         (ftype (function (pathname-designator) (maybe universal-time)) file-write-date)
         (ftype (function (pathname-designator (and pathname-designator (not stream)) &key (if-exists t)) ; FIXME: give a more precise type here.
                          (values pathname physical-pathname physical-pathname))
                rename-file)
         (ftype (function (pathname-designator) (eql t)) delete-file)
         (ftype (function (file-error) pathname-designator) file-error-pathname))

;;; Chapter 21 Streams
(declaim (ftype (function (stream) t)
                input-stream-p output-stream-p interactive-stream-p open-stream-p)
         (ftype (function (stream) type-specifier) stream-element-type)
         (ftype predicate-function streamp)
         (ftype (function (stream &optional t t) t) read-byte) ; can be eof value
         (ftype (function (integer stream) integer) write-byte)
         (ftype (function (&optional (or character boolean) stream-designator t t t) t)
                peek-char)
         (ftype (function (&optional stream-designator t t t) t)
                read-char read-char-no-hang)
         (ftype (function (&optional stream-designator) null) terpri)
         (ftype (function (&optional stream-designator) t) fresh-line)
         (ftype (function (character &optional stream-designator) null) unread-char)
         (ftype (function (character &optional stream-designator) character) write-char)
         (ftype (function (&optional stream-designator t t t) (values t t)) read-line)
         (ftype (function (string &optional stream-designator
                                  &key (:start sequence-index) (:end end-index-designator))
                          string)
                write-string write-line)
         (ftype (function (sequence stream &key (:start sequence-index) (:end end-index-designator))
                          sequence-index)
                read-sequence)
         (ftype (function (sequence stream &key (:start sequence-index) (:end end-index-designator))
                          sequence)
                write-sequence)
         (ftype (function (stream) (maybe (integer 0))) file-length)
         (ftype (function (stream &optional file-position-designator) t) file-position)
         (ftype (function (stream (or string character)) (maybe (integer 0))) file-string-length)
         (ftype (function (pathname-designator
                           &key (:direction (member :input :output :io :probe))
                           (:element-type type-specifier)
                           (:if-exists (member :error :new-version :rename :rename-and-delete
                                               :overwrite :append :supersede nil))
                           (:if-does-not-exist (member :error :create nil))
                           (:external-format external-file-format-designator))
                          (maybe stream))
                open)
         (ftype (function (stream) t) stream-external-format)
         (ftype (function (stream &key (:abort t)) t) close)
         (ftype (function (&optional stream-designator) t) listen)
         (ftype (function (&optional stream-designator) null)
                clear-input finish-output force-output clear-output)
         (ftype (function (&optional format-control &rest t) t) y-or-n-p yes-or-no-p)
         (ftype (function (symbol) synonym-stream) make-synonym-stream)
         (ftype (function (synonym-stream) symbol) synonym-stream-symbol)
         (ftype (function (broadcast-stream) list) broadcast-stream-streams)
         (ftype (function (&rest stream) broadcast-stream) make-broadcast-stream)
         (ftype (function (stream stream) two-way-stream) make-two-way-stream)
         (ftype (function (two-way-stream) stream)
                two-way-stream-input-stream two-way-stream-output-stream)
         (ftype (function (echo-stream) stream) echo-stream-input-stream echo-stream-output-stream)
         (ftype (function (stream stream) echo-stream) make-echo-stream)
         (ftype (function (concatenated-stream) list) concatenated-stream-streams)
         (ftype (function (&rest stream) concatenated-stream) make-concatenated-stream)
         (ftype (function (stream) string) get-output-stream-string)
         (ftype (function (string &optional sequence-index end-index-designator) string-stream)
                make-string-input-stream)
         (ftype (function (&key (:element-type type-specifier)) string-stream)
                make-string-output-stream)
         (ftype (function (stream-error) stream) stream-error-stream))

;;; Chapter 22 Printer

(declaim (ftype (function (&optional (maybe pprint-dispatch-table))
                          pprint-dispatch-table)
                copy-pprint-dispatch)
         (ftype (function (t &optional (maybe pprint-dispatch-table))
                          (values function-designator t))
                pprint-dispatch)
         (ftype (function (stream-designator t &optional t t) null)
                pprint-fill pprint-linear)
         (ftype (function (stream-designator t &optional t t (integer 0)) null)
                pprint-tabular)
         (ftype (function ((member :block :current) real &optional stream-designator)
                          null)
                pprint-indent)
         (ftype (function ((member :linear :fill :miser :mandatory)
                           &optional stream-designator)
                          null)
                pprint-newline)
         (ftype (function ((member :line :section :line-relative :section-relative)
                           (integer 0) (integer 0) &optional stream-designator)
                          null)
                pprint-tab)
         (ftype (function (t stream) t) print-object)
         (ftype (function (type-specifier (or function function-name null)
                                          &optional real pprint-dispatch-table)
                          null)
                set-pprint-dispatch)
         (ftype (write-function t) write)
         (ftype (function (t &optional stream-designator) t) prin1 princ print)
         (ftype (function (t &optional stream-designator) (values)) pprint)
         (ftype (write-function string) write-to-string)
         (ftype (function (t) string) prin1-to-string princ-to-string)
         (ftype (function (print-not-readable) t) print-not-readable-object)
         (ftype (function ((or boolean stream string) format-control &rest t)
                          (maybe string))
                format))

;;; Chapter 23 Reader

(declaim (ftype (function (&optional readtable-designator (maybe readtable)) readtable)
                copy-readtable)
         (ftype (function (character &optional t readtable) (eql t))
                make-dispatch-macro-character)
         (ftype (function (&optional stream-designator t t t) t)
                read read-preserving-whitespace)
         (ftype (function (character &optional stream-designator t) list)
                read-delimited-list)
         (ftype (function (string &optional t t
                                  &key (:start sequence-index) (:end end-index-designator)
                                  (:preserve-whitespace t))
                          (values t sequence-index))
                read-from-string)
         (ftype (function (readtable) case-sensitivity-mode) readtable-case)
         (ftype (function (case-sensitivity-mode readtable) case-sensitivity-mode)
                (setf readtable-case))
         (ftype predicate-function readtablep)
         (ftype (function (character character &optional readtable-designator)
                          (maybe function-designator))
                get-dispatch-macro-character)
         (ftype (function (character character function-designator
                                     &optional readtable-designator)
                          (eql t))
                set-dispatch-macro-character)
         (ftype (function (character &optional readtable-designator)
                          (values (maybe (function-designator (t t))) t))
                get-macro-character)
         (ftype (function (character (function-designator (t t))
                                     &optional t readtable-designator)
                          (eql t))
                set-macro-character)
         (ftype (function (character character &optional readtable readtable-designator)
                          (eql t))
                set-syntax-from-char))

;;; Chapter 24 System Construction

(declaim (ftype (function (pathname-designator &key (:output-file pathname-designator)
                                               (:verbose t) (:print t)
                                               (:external-format external-file-format-designator))
                          (values (maybe pathname) t t))
                compile-file)
         (ftype (function (pathname-designator &key (:output-file pathname-designator)
                                               &allow-other-keys)
                          pathname)
                compile-file-pathname)
         (ftype (function ((or stream pathname-designator)
                           &key (:verbose t) (:print t) (:if-does-not-exist t)
                           (:external-format external-file-format-designator))
                          t)
                load)
         (ftype (function (string-designator)) provide)
         (ftype (function (string-designator &optional (list-designator pathname-designator)))
                require))

;;; Chapter 25 Environment

(declaim (ftype (function (universal-time &optional time-zone)
                          (values second minute hour date month year
                                  day-of-week dst-flag time-zone))
                decode-universal-time)
         (ftype (function (second minute hour date month year &optional time-zone)
                          universal-time)
                encode-universal-time)
         (ftype (function () universal-time) get-universal-time)
         (ftype (function () (values second minute hour date month year
                                     day-of-week dst-flag time-zone))
                get-decoded-time)
         (ftype (function ((real 0)) null) sleep)
         (ftype (function (string-designator &optional (maybe package-designator))
                          (values))
                apropos)
         (ftype (function (string-designator &optional (maybe package-designator)) list)
                apropos-list)
         (ftype (function (t &optional stream-designator) (values)) describe)
         (ftype (function (t stream)) describe-object)
         (ftype (function () (integer 0)) get-internal-real-time get-internal-run-time)
         (ftype (function ((or lambda-expression extended-function-designator) &key) null) ; clasp has custom keywords. maybe give a more precise type here.
                disassemble)
         (ftype (function (t symbol) (maybe string)) documentation)
         (ftype (function (string t symbol) string) (setf documentation))
         (ftype (function (&optional (member t nil :default))) room)
         (ftype (function (&optional (or null pathname string function-name))) ed)
         (ftype (function (t)) inspect)
         (ftype (function (&optional pathname-designator)) dribble)
         (ftype (function () (maybe string))
                lisp-implementation-type lisp-implementation-version
                short-site-name long-site-name
                machine-instance machine-type machine-version
                software-type software-version)
         (ftype (function (&optional (or string list (eql :unspecific))) (maybe pathname))
                user-homedir-pathname))

;;;; End standard CL function types.

;;;; Function types for Clasp specific operators.

(declaim (ftype (function (number number) number)
                core:two-arg-+ core:two-arg-*
                core:two-arg-- core:two-arg-/)) ; for / we also have that the denominator can't be zero.
(declaim (ftype (function (real real) t)
                core:two-arg-< core:two-arg-> core:two-arg-<=
                core:two-arg->= core:two-arg-=))
(declaim (ftype (function (character character) t)
                core:two-arg-char-equal core:two-arg-char-greaterp
                core:two-arg-char-lessp core:two-arg-char-not-greaterp
                core:two-arg-char-not-lessp core:two-arg-char<
                core:two-arg-char<= core:two-arg-char> core:two-arg-char>=))

;;;; End function types for Clasp specific operators.

(progn
  #+(or)
  (eval-when (:execute)
    (setq core:*echo-repl-read* t))
  
  #+(or)
  (defmacro debug-inline (msg &rest msg-args)
    `(progn
       (core:bformat t "debug-inline>> ")
       (core:bformat t ,msg ,@msg-args)
       (core:bformat t "%N")
       (finish-output)))
  (defmacro debug-inline (msg &rest msg-args)
    (declare (ignore msg msg-args))
    nil))

;;; This defines compiler macros that only come into effect when using cclasp.
;;; This is useful when their expansions involve cleavir-only special operators.
;;; Syntax is the same as define-compiler-macro, except that the lambda-list
;;; MUST start with (&whole something ...) for things to work.
;;; This macro is a little janky in that it doesn't work with declarations.
(defmacro define-cleavir-compiler-macro (name lambda-list &body body)
  `(define-compiler-macro ,name (,@lambda-list)
     ;; I just picked this since it's the first variable in auto-compile.lisp.
     (unless (eq cmp:*cleavir-compile-hook* 'bir-compile)
       (return-from ,(core:function-block-name name) ,(second lambda-list)))
     ,@body))

;;; If FORM is of the form #'valid-function-name, return valid-function-name.
;;; FIXME?: Give up on expansion and warn if it's invalid?
(defun constant-function-form (form env)
  (declare (ignore env))
  (and (consp form) (eq (first form) 'function)
       (consp (cdr form)) (null (cddr form))
       (core:valid-function-name-p (second form))
       (second form)))

(define-cleavir-compiler-macro funcall
    (&whole form function &rest arguments &environment env)
  ;; If we have (funcall #'foo ...), we might be able to apply the FOO compiler macro.
  ;; Failing that, we can at least skip any coercion - #'foo is obviously a function.
  ;; (funcall #'(setf foo) ...) is fairly common, so this is nice to do.
  (let ((name (constant-function-form function env)))
    (when name
      (return-from funcall
        (let* ((func-info (cleavir-env:function-info env name))
               (notinline (and func-info
                               (eq 'notinline (cleavir-env:inline func-info))))
               ;; We can't get this from the func-info because it might be
               ;; a local-function-info, which doesn't have that slot.
               (cmf (compiler-macro-function name env)))
          (if (and cmf (not notinline))
              (funcall *macroexpand-hook* cmf form env)
              `(cleavir-primop:funcall ,function ,@arguments))))))
  `(cleavir-primop:funcall
    (core:coerce-fdesignator ,function)
    ,@arguments))

(define-cleavir-compiler-macro values (&whole form &rest values)
  `(cleavir-primop:values ,@values))

;;; Written as a compiler macro to avoid confusing bclasp.
(define-cleavir-compiler-macro multiple-value-bind (&whole form vars values-form &body body)
  (let ((syms (loop for var in vars collecting (gensym (symbol-name var)))))
    ;; NOTE: We ought to be able to use LET-UNINITIALIZED here. However,
    ;; Cleavir works badly with this in some situations. See bug #866.
    `(let (,@syms)
       (cleavir-primop:multiple-value-setq (,@syms) ,values-form)
       (let (,@(loop for var in vars for sym in syms
                     collecting `(,var ,sym)))
         ,@body))))

;;; Ditto, a compiler macro to avoid confusing bclasp.
(define-cleavir-compiler-macro ext:with-current-source-form
    (&whole f (&rest forms) &body body)
  `(cleavir-cst-to-ast:with-current-source-form (,@forms) ,@body))

;;; NOTE: The following two macros don't actually rely on anything cleavir-specific
;;; for validity. However, they do rely on their efficiency being from
;;; multiple-value-bind being efficient, which it is not without the above version.

(define-compiler-macro nth-value (&whole form n expr &environment env)
  (let ((n (and (constantp n env) (ext:constant-form-value n env))))
    (if (or (null n) (> n 100)) ; completely arbitrary limit
        form
        (let ((dummies (loop repeat n collect (gensym "DUMMY")))
              (keeper (gensym "SMARTIE")))
          `(multiple-value-bind (,@dummies ,keeper) ,expr
             (declare (ignore ,@dummies))
             ,keeper)))))

;;; I'm not sure I understand the order of evaluation issues entirely,
;;; so I'm antsy about using the m-v-setq primop directly... and this
;;; equivalence is guaranteed.
;;; SETF VALUES will expand into a multiple-value-bind, which will use
;;; the m-v-setq primop as above, so it works out about the same.
;;; Not a cleavir macro because all we need is setf.
(define-compiler-macro multiple-value-setq ((&rest vars) form)
  ;; SETF VALUES will return no values if it sets none, but m-v-setq
  ;; always returns the primary value.
  (if (null vars)
      `(values ,form)
      `(values (setf (values ,@vars) ,form))))

;;; This stupid little macro is to tighten up
;;; (if (and (fixnump x) (>= x c1) (< x c2)) ...)
;;; which is useful for bounds checks.
;;; The compiler can't optimize this condition very well as I write this.
;;; NOTE: Evaluates VAL more than once.
#+(or)
(defmacro if-in-bounds ((val low high) then else)
  `(core::local-block nil
     (if (cleavir-primop:typeq ,val fixnum)
         (if (cleavir-primop:fixnum-not-less ,val ,low)
             (if (cleavir-primop:fixnum-less ,val ,high)
                 (return ,then)))
         (error 'type-error :datum ,val :expected-type 'fixnum))
     ,else))

(define-cleavir-compiler-macro cl:eq (x y)
  `(if (cleavir-primop:eq ,x ,y) t nil))

(declaim (ftype (function (t t) boolean) cl:eq eql))
(progn
  (debug-inline "eq")
  (defun cl:eq (x y)
    (if (cleavir-primop:eq x y) t nil)))

(progn
  (debug-inline "eql")
  (declaim (inline cl:eql))
  (defun eql (x y)
    (cond ((cleavir-primop:eq x y) t)
          ((typep x 'core::eq-incomparable)
           (if (typep y 'core::eq-incomparable)
               (core:eql-underlying x y)
               nil))
          (t nil))))

#+(or)
(progn
  ;; Really want a bit-vector-equal intrinsic here.
  ;; Maybe even separate simple and not vectors.
  
  (defmacro type2case (x y fail &rest cases)
    (let ((sx (gensym "X")) (sy (gensym "Y")))
      `(let ((,sx ,x) (,sy ,y))
         (cond ,@(loop for (type . body) in cases
                       collect `((typep ,sx ',type)
                                 (if (typep ,sy ',type)
                                     (progn ,@body)
                                     ,fail))
                       collect `((typep ,sy ',type) ,fail))
               (t ,fail)))))
  
  (defun equal (x y)
    (or (eql x y)
        (type2case x y nil
                   (cons (and (equal (car x) (car y))
                              (equal (cdr x) (cdr y))))
                   (string (string= x y))
                   (bit-vector (bit-vector-equal x y))
                   (pathname (pathname-equal x y)))))
  
  (defun hash-table-equalp (x y)
    (and (eq (hash-table-count x) (hash-table-count y))
         (eq (hash-table-test x) (hash-table-test y))
         ;; since the number of entries is the same,
         ;; we don't need to check for extra keys in y.
         (maphash (lambda (k v)
                    (multiple-value-bind (otherv present)
                        (gethash k y)
                      (unless present
                        (return-from hash-table-equalp nil))
                      (unless (equalp v otherv)
                        (return-from hash-table-equalp nil))))
                  x)
         t))
  
  (defun equalp (x y)
    (or (eq x y)
        (type2case x y nil
                   (character (char-equal x y))
                   (number (= x y))
                   (cons (and (equalp (car x) (car y))
                              (equalp (cdr x) (cdr y))))
                   (array (array-equalp x y))
                   (structure-object (structure-equalp x y))
                   (hash-table (hash-table-equalp x y))
                   (pathname (pathname-equal x y)))))
  )

;;; Type predicates.
(macrolet ((defpred (name type)
             ;; We have to be careful about recursion - if one of these ended up
             ;; as a TYPEP call there could be disastrous recursion.
             ;; Here's a sanity check to make sure the type is something simple
             ;; enough to be done as a header check.
             ;; Since some aren't actually THAT simple but still won't be typep
             ;; it's not actually used, but, you know, it's there.
             #+(or)
             (unless (or (member type '(fixnum cons character single-float))
                         (gethash type core:+type-header-value-map+))
               (error "BUG: See comment in inline.lisp DEFPRED"))
             `(progn
                (debug-inline ,(symbol-name name))
                (define-cleavir-compiler-macro ,name (object)
                  `(if (cleavir-primop:typeq ,object ,',type) t nil))
                (defun ,name (o)
                  (if (cleavir-primop:typeq o ,type) t nil))))
           (defpreds (&rest rest)
             `(progn
                ,@(loop for (fun name) on rest by #'cddr
                        collect `(defpred ,fun ,name)))))
  ;; Ideally, we want to cover standard type predicates, plus everything with a
  ;; core::simple-type-predicate,= as those will show up from TYPEP.

  ;; numbers are a bit weird cos of fixnums, but nonetheless
  ;; shouldn't revert to typep.
  ;; string is (or simple-base-string simple-character-string) so should be ok.
  ;; list is (or cons null) so should be ok.
  ;; atom is (not cons)
  (defpreds consp cons
    core:fixnump fixnum
    characterp character
    core:single-float-p single-float

    arrayp array
    complexp complex
    core:double-float-p double-float
;;    floatp float
    functionp function
    hash-table-p core:hash-table-base
;;    integerp integer
;;    listp list
    ;; null null ; defined with EQ below
;;    numberp number
    random-state-p random-state
;;    rationalp rational
;;    realp real
    packagep package
    pathnamep pathname
    core:data-vector-p core:abstract-simple-vector
;;    simple-array-p simple-array
    simple-bit-vector-p simple-bit-vector
    simple-string-p simple-string
    simple-vector-p simple-vector
;;    stringp string
    symbolp symbol
;;    vectorp vector)
    )
  ;; standard predicates we can't define like this
  #+(or)
  (defpreds
    ;; standard-char-p standard-char ; not actually a type pred - only accepts chars
      ;; streamp stream ; no good as it's an extensible class... FIXME do it anyway?
      compiled-function-p compiled-function))

(define-cleavir-compiler-macro atom (&whole form object)
  `(if (cleavir-primop:typeq ,object cons) nil t))

(declaim (ftype (function (&rest t) list) list))
(declaim (ftype (function (&rest t) list) list*))
(declaim (ftype (function (t t) cons) cons))

(declaim (ftype (function (list) t) car cdr))
(progn
  (debug-inline "car")
  (declaim (inline cl:car))
  (defun cl:car (x)
    (if (cleavir-primop:typeq x cons)
        (cleavir-primop:car x)
        (if (cleavir-primop:the (not cons) x)
            (error 'type-error :datum x :expected-type 'list)
            nil))))

(progn
  (debug-inline "cdr")
  (declaim (inline cl:cdr))
  (defun cl:cdr (x)
    (if (cleavir-primop:typeq x cons) ;; (consp x)
        (cleavir-primop:cdr x)
        (if (cleavir-primop:the (not cons) x)
            (error 'type-error :datum x :expected-type 'list)
            nil))))

(defmacro defcr (name &rest ops)
  `(progn
     (debug-inline ,(symbol-name name))
     (declaim (inline ,name)
              (ftype (function (list) t) ,name))
     (defun ,name (x)
       ,(labels ((rec (ops)
                   (if ops
                       `(,(first ops) ,(rec (rest ops)))
                       'x)))
          (rec ops)))))

(defcr caar   car car)
(defcr cadr   car cdr)
(defcr cdar   cdr car)
(defcr cddr   cdr cdr)
(defcr caaar  car car car)
(defcr caadr  car car cdr)
(defcr cadar  car cdr car)
(defcr caddr  car cdr cdr)
(defcr cdaar  cdr car car)
(defcr cdadr  cdr car cdr)
(defcr cddar  cdr cdr car)
(defcr cdddr  cdr cdr cdr)
(defcr caaaar car car car car)
(defcr caaadr car car car cdr)
(defcr caadar car car cdr car)
(defcr caaddr car car cdr cdr)
(defcr cadaar car cdr car car)
(defcr cadadr car cdr car cdr)
(defcr caddar car cdr cdr car)
(defcr cadddr car cdr cdr cdr)
(defcr cdaaar cdr car car car)
(defcr cdaadr cdr car car cdr)
(defcr cdadar cdr car cdr car)
(defcr cdaddr cdr car cdr cdr)
(defcr cddaar cdr cdr car car)
(defcr cddadr cdr cdr car cdr)
(defcr cdddar cdr cdr cdr car)
(defcr cddddr cdr cdr cdr cdr)

(defcr rest    cdr)
(defcr first   car)
(defcr second  car cdr)
(defcr third   car cdr cdr)
(defcr fourth  car cdr cdr cdr)
(defcr fifth   car cdr cdr cdr cdr)
(defcr sixth   car cdr cdr cdr cdr cdr)
(defcr seventh car cdr cdr cdr cdr cdr cdr)
(defcr eighth  car cdr cdr cdr cdr cdr cdr cdr)
(defcr ninth   car cdr cdr cdr cdr cdr cdr cdr cdr)
(defcr tenth   car cdr cdr cdr cdr cdr cdr cdr cdr cdr)

(debug-inline "rplaca")

(progn
  (declaim (inline cl:rplaca))
  (defun cl:rplaca (p v)
    (if (cleavir-primop:typeq p cons)
        (progn
          (cleavir-primop:rplaca p v)
          p)
        (error 'type-error :datum p :expected-type 'cons))))

(progn
  (declaim (inline cl:rplacd))
  (defun cl:rplacd (p v)
    (if (cleavir-primop:typeq p cons)
        (progn
          (cleavir-primop:rplacd p v)
          p)
        (error 'type-error :datum p :expected-type 'cons))))


(debug-inline "primop")

#+(or)
(progn
  (defmacro define-with-contagion (inlined-name comparison (x y) fixnum single-float double-float generic)
    (declare (ignore comparison)) ; this will be used to control fp behavior, see CLHS 12.1.4.1
    `(progn
       (declaim (inline ,inlined-name))
       (defun ,inlined-name (,x ,y)
         (tagbody
            ;; FIXME: The "generic" jumps should actually coerce and then jump
            ;; to a specialized one.
            (cond ((cleavir-primop:typeq ,x fixnum)
                   (if (cleavir-primop:typeq ,y fixnum)
                       (go fixnum)
                       (go generic)))
                  ((cleavir-primop:typeq ,x single-float)
                   (cond ((cleavir-primop:typeq ,y single-float)
                          (go single-float))
                         #+(or)
                         ((cleavir-primop:typeq ,y double-float)
                          (setf ,x (cleavir-primop:coerce single-float double-float ,x))
                          (go double-float))
                         (t (go generic))))
                  ((cleavir-primop:typeq ,x double-float)
                   (cond ((cleavir-primop:typeq ,y double-float)
                          (go double-float))
                         #+(or)
                         ((cleavir-primop:typeq ,y single-float)
                          (setf ,y (cleavir-primop:coerce single-float double-float ,y))
                          (go double-float))
                         (t (go generic))))
                  (t (go generic)))
          fixnum (return-from ,inlined-name ,@fixnum)
          single-float (return-from ,inlined-name ,@single-float)
          double-float (return-from ,inlined-name ,@double-float)
          generic (return-from ,inlined-name ,@generic)))))
  (define-with-contagion primop:inlined-two-arg-+ nil (x y)
    ((cleavir-primop:let-uninitialized (z)
       (if (cleavir-primop:fixnum-add x y z)
           z
           (core:convert-overflow-result-to-bignum z))))
    ((cleavir-primop:float-add single-float x y))
    ((cleavir-primop:float-add double-float x y))
    ((core:two-arg-+ x y)))
  (define-with-contagion primop:inlined-two-arg-- nil (x y)
    ((cleavir-primop:let-uninitialized (z)
       (if (cleavir-primop:fixnum-sub x y z)
           z
           (core:convert-overflow-result-to-bignum z))))
    ((cleavir-primop:float-sub single-float x y))
    ((cleavir-primop:float-sub double-float x y))
    ((core:two-arg-- x y)))
  (define-with-contagion primop:inlined-two-arg-* nil (x y)
    ((go generic))             ; FIXME: fixnum arithmetic!
    ((cleavir-primop:float-mul single-float x y))
    ((cleavir-primop:float-mul double-float x y))
    ((core:two-arg-* x y)))
  (define-with-contagion primop:inlined-two-arg-/ nil (x y)
    ((go generic))             ; FIXME: fixnum arithmetic!
    ((cleavir-primop:float-div single-float x y))
    ((cleavir-primop:float-div double-float x y))
    ((core:two-arg-/ x y)))
  (defmacro defcomparison (inline-name fixnum-op float-op generic-name)
    `(define-with-contagion ,inline-name t (x y)
       ((if (,fixnum-op x y) t nil))
       ((if (,float-op single-float x y) t nil))
       ((if (,float-op double-float x y) t nil))
       ((,generic-name x y))))
  (defcomparison primop:inlined-two-arg-<
    cleavir-primop:fixnum-less        cleavir-primop:float-less        core:two-arg-<)
  (defcomparison primop:inlined-two-arg-<=
    cleavir-primop:fixnum-not-greater cleavir-primop:float-not-greater core:two-arg-<=)
  (defcomparison primop:inlined-two-arg-=
    cleavir-primop:fixnum-equal       cleavir-primop:float-equal       core:two-arg-=)
  (defcomparison primop:inlined-two-arg->
    cleavir-primop:fixnum-greater     cleavir-primop:float-greater     core:two-arg->)
  (defcomparison primop:inlined-two-arg->=
    cleavir-primop:fixnum-not-less    cleavir-primop:float-not-less    core:two-arg->=))

#+(or)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-cleavir-compiler-macro + (&whole form &rest numbers)
    (core:expand-associative '+ 'primop:inlined-two-arg-+ numbers 0))
  (define-cleavir-compiler-macro - (&whole form minuend &rest subtrahends)
    (if (core:proper-list-p subtrahends)
        (if subtrahends
            `(primop:inlined-two-arg-- ,minuend ,(core:expand-associative '+ 'primop:inlined-two-arg-+ subtrahends 0))
            `(core:negate ,minuend))
        (error "The - operator can not be part of a form that is a dotted list.")))
  (define-cleavir-compiler-macro * (&whole form &rest numbers)
    (core:expand-associative '* 'primop:inlined-two-arg-* numbers 1))
  (define-cleavir-compiler-macro / (&whole form dividend &rest divisors)
    (if (core:proper-list-p divisors)
        (if divisors
            `(primop:inlined-two-arg-/ ,dividend (* ,@divisors))
            `(primop:inlined-two-arg-/ 1 ,dividend))
        (error "The / operator can not be part of a form that is a dotted list.")))
  (define-cleavir-compiler-macro < (&whole form &rest numbers)
    (core:expand-compare form 'primop:inlined-two-arg-< numbers 'real))
  (define-cleavir-compiler-macro <= (&whole form &rest numbers)
    (core:expand-compare form 'primop:inlined-two-arg-<= numbers 'real))
  (define-cleavir-compiler-macro = (&whole form &rest numbers)
    (core:expand-compare form 'primop:inlined-two-arg-= numbers 'number))
  (define-cleavir-compiler-macro /= (&whole form &rest numbers)
    (core:expand-uncompare form 'primop:inlined-two-arg-= numbers 'number))
  (define-cleavir-compiler-macro > (&whole form &rest numbers)
    (core:expand-compare form 'primop:inlined-two-arg-> numbers 'real))
  (define-cleavir-compiler-macro >= (&whole form &rest numbers)
    (core:expand-compare form 'primop:inlined-two-arg->= numbers 'real))
  (define-cleavir-compiler-macro 1+ (&whole form x)
    `(primop:inlined-two-arg-+ ,x 1))
  (define-cleavir-compiler-macro 1- (&whole form x)
    `(primop:inlined-two-arg-- ,x 1)))

;;; ------------------------------------------------------------
;;;
;;; Array functions
;;;

(debug-inline "array-total-size")
(declaim (inline array-total-size)
         (ftype (function (array) sys:index) array-total-size))
(defun array-total-size (array)
  (etypecase array
    ((simple-array * (*)) (core::vector-length array))
    ;; MDArray
    (array (core::%array-total-size array))))

(debug-inline "array-rank")
(declaim (inline array-rank)
         (ftype (function (array) (integer 0 #.array-rank-limit)) array-rank))
(defun array-rank (array)
  (etypecase array
    ((simple-array * (*)) 1)
    (array (core::%array-rank array))))

#+(or)
(progn
(debug-inline "array-dimension")
(declaim (inline array-dimension))
(defun array-dimension (array axis-number)
  (etypecase array
    ((simple-array * (*))
     (if (zerop axis-number)
         (core::vector-length array)
         (error "Invalid axis number ~d for array of rank ~d" axis-number 1)))
    (array
     (if-in-bounds (axis-number 0 (core::%array-rank array))
                   (core::%array-dimension array axis-number)
                   (error "Invalid axis number ~d for array of rank ~d"
                          axis-number (core::%array-rank array))))))
)

;; Unsafe version for array-row-major-index
(debug-inline "%array-dimension")
(declaim (inline %array-dimension)
         (ftype (function (array) (integer 0 #.array-dimension-limit)) %array-dimension))
(defun %array-dimension (array axis-number)
  (etypecase array
    ((simple-array * (*))
     (core::vector-length array))
    (array (core::array-dimension array axis-number))))

#+(or)
(progn
(debug-inline "svref")
(declaim (inline svref))
(defun svref (vector index)
  (if (typep vector 'simple-vector)
      (let ((ats (core::vector-length vector)))
        (if-in-bounds (index 0 ats)
                      (cleavir-primop:aref vector index t t t)
                      (error "Invalid index ~d for vector of length ~d" index ats)))
      (error 'type-error :datum vector :expected-type 'simple-vector)))
)

#+(or)
(progn
(declaim (inline (setf svref)))
(defun (setf svref) (value vector index)
  (if (typep vector 'simple-vector)
      (let ((ats (core::vector-length vector)))
        (if-in-bounds (index 0 ats)
                      (progn (cleavir-primop:aset vector index value t t t)
                             value)
                      (error "Invalid index ~d for vector of length ~d" index ats)))
      (error 'type-error :datum vector :expected-type 'simple-vector)))
)

;;; Unsafe versions to use that don't check bounds (but still check type)
#+(or)
(progn
(debug-inline "svref/no-bounds-check")
(declaim (inline svref/no-bounds-check))
(defun svref/no-bounds-check (vector index)
  (if (typep vector 'simple-vector)
      (if (cleavir-primop:typeq index fixnum)
          (cleavir-primop:aref vector index t t t)
          (error 'type-error :datum index :expected-type 'fixnum))
      (error 'type-error :datum vector :expected-type 'simple-vector)))
)

#+(or)
(progn
(declaim (inline (setf svref/no-bounds-check)))
(defun (setf svref/no-bounds-check) (value vector index)
  (if (typep vector 'simple-vector)
      (if (cleavir-primop:typeq index fixnum)
          (progn (cleavir-primop:aset vector index value t t t)
                 value)
          (error 'type-error :datum index :expected-type 'fixnum))
      (error 'type-error :datum vector :expected-type 'simple-vector)))
)

#+(or)
(define-cleavir-compiler-macro svref (&whole whole vector index &environment env)
  (if (environment-has-policy-p env 'core::insert-array-bounds-checks)
      whole
      `(svref/no-bounds-check ,vector ,index)))
#+(or)
(define-cleavir-compiler-macro (setf svref)
    (&whole whole value vector index &environment env)
  (if (environment-has-policy-p env 'core::insert-array-bounds-checks)
      whole
      `(funcall #'(setf svref/no-bounds-check) ,value ,vector ,index)))

#+(or)
(progn
  (debug-inline "core:vref")
  (declaim (inline core:vref))
  (defun core:vref (array index)
    ;; FIXME: type inference should be able to remove the redundant
    ;; checking that it's an array... maybe?
    (macrolet ((mycase (&rest specs)
                 `(typecase array
                    ,@(loop for (type boxed) in specs
                            collect `((simple-array ,type (*))
                                      (cleavir-primop:aref array index ,type t ,boxed)))
                    (t
                     (core:bformat t "vref array-element-type: %s%N" (array-element-type array))
                     (error "BUG: vref unknown vector ~a" array)))))
      (mycase (t t) (base-char nil) (character nil)
              (double-float nil) (single-float nil)
              (fixnum nil)
              (ext:integer64 nil) (ext:integer32 nil)
              (ext:integer16 nil) (ext:integer8 nil)
              (ext:byte64 nil) (ext:byte32 nil)
              (ext:byte16 nil) (ext:byte8 nil)
              (bit t)))))

;;; This is unsafe in that it doesn't bounds check.
;;; It DOES check that the value is of the correct type,
;;; because this is the only place we know the type.
#+(or)
(progn
  (declaim (inline (setf core:vref)))
  (defun (setf core:vref) (value array index)
    (macrolet ((mycase (&rest specs)
                 `(typecase array
                    ,@(loop for (type boxed) in specs
                            collect `((simple-array ,type (*))
                                      (unless (typep value ',type)
                                        (error 'type-error :datum value :expected-type ',type))
                                      (cleavir-primop:aset array index value ,type t ,boxed)
                                      value))
                    ;; should be unreachable
                    (t (error "BUG: Unknown vector ~a" array)))))
      (mycase (t t) (base-char nil) (character nil)
              (double-float nil) (single-float nil)
              (fixnum nil)
              (ext:integer64 nil) (ext:integer32 nil)
              (ext:integer16 nil) (ext:integer8 nil)
              (ext:byte64 nil) (ext:byte32 nil)
              (ext:byte16 nil) (ext:byte8 nil)
              (bit t)))))

;;; Array indices are all fixnums. If we're sure sizes are valid, we don't want
;;; to use general arithmetic. We can just use this to do unsafe modular arithmetic.
;;; (Used in this file only)
#+(or)
(defmacro add-indices (a b)
  `(cleavir-primop:let-uninitialized (z)
     (if (cleavir-primop:fixnum-add ,a ,b z) z z)))

;; FIXME: Duplicate code from seqmacros.lsp.
#+(or)
(defmacro with-array-data ((arrayname offsetname array) &body body)
  `(multiple-value-bind (,arrayname ,offsetname)
       (let ((,arrayname ,array) (,offsetname 0))
         (loop
           (if (cleavir-primop:typeq ,arrayname core:abstract-simple-vector)
               (return (values ,arrayname ,offsetname)))
           (psetf ,arrayname (core::%displacement ,arrayname)
                  ,offsetname (add-indices
                               ,offsetname
                               (core::%displaced-index-offset ,arrayname)))))
     (declare (type (simple-array * (*)) ,arrayname)
              (type fixnum ,offsetname))
     ,@body))

#+(or)
(progn
(declaim (inline vector-read))
(defun vector-read (vector index)
  ;; first bounds check. Use the original arguments.
  ;; second, undisplace. This can be done independently
  ;; of the index, meaning it could potentially be
  ;; moved out of loops, though that can invite inconsistency
  ;; in a multithreaded environment.
  ;; NOTE: vector-length will be the fill pointer, if there is one.
  ;; ALSO NOTE: This function is only used in ELT. We know already
  ;; that vector really is a vector.
  (let ((max (core::vector-length vector)))
    (if-in-bounds (index 0 max)
                  nil
                  (error 'core:sequence-out-of-bounds
                         :datum index :expected-type `(integer 0 (,max))
                         :object vector))
    (with-array-data (underlying-array offset vector)
      ;; Okay, now array is a vector/simple, and index is valid.
      ;; This function takes care of element type discrimination.
      (vref underlying-array (add-indices index offset)))))
)

#+(or)
(progn
(declaim (inline vector-set))
(defun vector-set (vector index value)
  ;; NOTE: This function is only used in CORE:SETF-ELT. We know already
  ;; that vector really is a vector.
  (let ((max (core::vector-length vector)))
    (if-in-bounds (index 0 max)
                  nil
                  (error 'core:sequence-out-of-bounds
                         :datum index :expected-type `(integer 0 (,max))
                         :object vector))
    (with-array-data (underlying-array offset vector)
      ;; Okay, now array is a vector/simple, and index is valid.
      ;; This function takes care of element type discrimination.
      (setf (vref underlying-array (add-indices index offset)) value))))
)

#+(or)
(progn
(declaim (inline row-major-aref/no-bounds-check))
(defun row-major-aref/no-bounds-check (array index)
  ;; First, undisplace. This can be done independently
  ;; of the index, meaning it could potentially be
  ;; moved out of loops, though that can invite inconsistency
  ;; in a multithreaded environment.
  (with-array-data (underlying-array offset array)
    ;; Array is a vector/simple, and we assume index is valid.
    (core:vref underlying-array (add-indices index offset))))
)

#+(or)
(progn
(declaim (inline cl:row-major-aref))
(defun cl:row-major-aref (array index)
  (let ((max (etypecase array
               ((simple-array * (*)) (core::vector-length array))
               (array (core::%array-total-size array)))))
    (if-in-bounds (index 0 max)
                  nil
                  (error 'core:row-major-out-of-bounds :datum index
                                                       :expected-type `(integer 0 (,max))
                                                       :object array)))
  (row-major-aref/no-bounds-check array index))
(define-cleavir-compiler-macro row-major-aref (&whole whole array index &environment env)
  (if (environment-has-policy-p env 'core::insert-array-bounds-checks)
      whole
      `(row-major-aref/no-bounds-check ,array ,index)))
)

#+(or)
(progn
(declaim (inline row-major-aset/no-bounds-check))
(defun row-major-aset/no-bounds-check (array index value)
  (with-array-data (underlying-array offset array)
    (setf (core:vref underlying-array (add-indices index offset)) value)))
)

#+(or)
(progn
(declaim (inline core:row-major-aset))
(defun core:row-major-aset (array index value)
  (let ((max (etypecase array
               ((simple-array * (*)) (core::vector-length array))
               (array (core::%array-total-size array)))))
    (if-in-bounds (index 0 max)
                  nil
                  (error 'core:row-major-out-of-bounds :datum index
                                                       :expected-type `(integer 0 (,max))
                                                       :object array)))
  (row-major-aset/no-bounds-check array index value))
(define-cleavir-compiler-macro core:row-major-aset
    (&whole whole array index value &environment env)
  (if (environment-has-policy-p env 'core::insert-array-bounds-checks)
      whole
      `(row-major-aset/no-bounds-check ,array ,index ,value)))
)

(declaim (inline schar (setf schar) char (setf char))
         (ftype (function (simple-string sys:index) character) schar)
         (ftype (function (string sys:index) character) char)
         (ftype (function (character simple-string sys:index) character) (setf schar))
         (ftype (function (character string sys:index) character) (setf char)))
(defun schar (string index)
  (row-major-aref (the simple-string string) index))
(defun (setf schar) (value string index)
  (core:row-major-aset (the simple-string string) index value))

(defun char (string index)
  (row-major-aref (the string string) index))
(defun (setf char) (value string index)
  (core:row-major-aset (the string string) index value))

(defun row-major-index-computer (dimsyms subscripts)
  ;; assumes once-only is taken care of.
  ;; array is a symbol bound to an array.
  ;; dimsyms is a list of symbols bound to the array dimensions of the array.
  ;; subscripts is a list of symbols bound to the indices we're computing for.
  (let ((subsyms (loop for sub in subscripts collect (gensym "SUB"))))
    (cond
      ((null subscripts) '0)
      ;; special case the one dimensional case to avoid multiplication.
      ;; FIXME: That SHOULD be handled by constant propagation and all,
      ;; allowing this special case to be axed, but we don't have that yet.
      ((null (rest subscripts)) (first subscripts))
      (t
       `(let* ((,(first subsyms) 1) ;; this is the troubling constant.
               ,@(loop for dimsym in (reverse dimsyms)
                       for subsym in (cdr subsyms)
                       for lastsym in subsyms
                       collect `(,subsym (* ,lastsym ,dimsym))))
          (declare (type fixnum ,@subsyms))
          (+ ,@(loop for sub in subscripts
                     for subsym in (reverse subsyms)
                     collect `(* ,sub ,subsym))))))))

;;; Insert some form if the policy is in effect, otherwise nil.
;;; intended use is like ,@(when-policy ...)
(defun when-policy (env policy form)
  (when (environment-has-policy-p env policy) (list form)))

;;; FIXME: core::%array-dimension won't work for simple vectors. Might need to
;;; shuffle type checks around to do things properly.
#+(or)
(define-cleavir-compiler-macro array-row-major-index
    (&whole form array &rest subscripts &environment env)
  ;; FIXME: Cleavir arithmetic is not yet clever enough for this to be fast in the
  ;; >1dimensional case. We need wrapped fixnum multiplication and addition, basically,
  ;; where overflow jumps to an error.
  ;; As such, we don't expand (using the C++ definition) for these cases.
  (if (> (length subscripts) 1)
      form
      (let* ((rank (length subscripts))
             (sarray (gensym "ARRAY"))
             (ssubscripts (loop repeat rank collecting (gensym "SUBSCRIPT")))
             (dimsyms (loop repeat rank collecting (gensym "DIMENSION")))
             (rmindex (gensym "ROW-MAJOR-INDEX")))
        ;; First up, once-only the array and subscripts.
        `(let ((,sarray ,array)
               ,@(loop for ssub in ssubscripts for sub in subscripts
                       collecting `(,ssub ,sub)))
           (declare (type fixnum ,@ssubscripts))
           ;; Now verify that the rank is correct (maybe)
           ,@(when-policy
              env 'core::insert-array-bounds-checks
              `(core:check-rank ,sarray ,rank))
           ;; We need the array dimensions, so bind those
           (let (,@(loop for dimsym in dimsyms
                         for axis below rank
                         collect `(,dimsym (core::%array-dimension ,sarray ,axis))))
             (declare (type fixnum ,@dimsyms))
             ;; Check that the index is valid (maybe)
             ,@(when (environment-has-policy-p env 'core::insert-array-bounds-checks)
                 (loop for ssub in ssubscripts
                       for dimsym in dimsyms
                       for axis below rank
                       collect `(core:check-index ,ssub ,dimsym ,axis)))
             ;; Now we know we're good, do the actual computation
             ,(row-major-index-computer dimsyms ssubscripts))))))

#+(or)
(define-cleavir-compiler-macro aref (&whole form array &rest subscripts
                                            &environment env)
  ;; FIXME: See tragic comment above in array-row-major-index.
  (if (or (> (length subscripts) 1) (null subscripts))
      form
      (let ((sarray (gensym "ARRAY"))
            (index0 (gensym "INDEX0")))
        `(let ((,sarray ,array)
               (,index0 ,(first subscripts)))
           ,@(when-policy env 'insert-type-checks
              `(if (cleavir-primop:typeq ,sarray array)
                   nil
                   (error 'type-error :datum ,sarray :expected-type '(array * 1))))
           ,@(when-policy
              env 'core::insert-array-bounds-checks
              `(core::multiple-value-foreign-call
                "cm_check_index"
                ,index0
                (if (cleavir-primop:typeq ,sarray core:abstract-simple-vector)
                    (core::vector-length ,sarray)
                    (core::%array-dimension ,sarray 0))
                0))
           (with-array-data (data offset ,sarray)
             (core::MULTIPLE-VALUE-FOREIGN-CALL "cm_vref" data (add-indices offset ,index0)))))))

#+(or)
(define-cleavir-compiler-macro (setf aref) (&whole form new array &rest subscripts
                                                   &environment env)
  (if (or (> (length subscripts) 1) (null subscripts))
      form
      (let ((sarray (gensym "ARRAY"))
            (index0 (gensym "INDEX0")))
        `(let ((,sarray ,array)
               (,index0 ,(first subscripts)))
           ,@(when-policy
              env 'core::insert-array-bounds-checks
              `(core::multiple-value-foreign-call
                "cm_check_index"
                ,index0
                (if (cleavir-primop:typeq ,sarray core:abstract-simple-vector)
                    (core::vector-length ,sarray)
                    (core::%array-dimension ,sarray 0))
                0))
           (with-array-data (data offset ,sarray)
             (core::MULTIPLE-VALUE-FOREIGN-CALL "cm_vset" data (add-indices offset ,index0) ,new))))))

;;; ------------------------------------------------------------
;;;
;;; Sequence functions
;;;

(declaim (ftype (function (sequence) sys:index) length))
(progn
  (debug-inline "length")
  (declaim (inline length))
  (defun length (sequence)
    (typecase sequence
      (cons (core:cons-length sequence))
      ;; note: vector-length returns the fill pointer if there is one.
      (vector (core::vector-length sequence))
      (null 0)
      (t (sequence:length sequence)))))

#+(or)
(progn
  (debug-inline "elt")
  (declaim (inline elt))
  (defun elt (sequence index)
    (etypecase sequence
      (cons
       (let ((cell (nthcdr index sequence)))
         (cond ((consp cell) (car (cleavir-primop:the cons cell)))
               ((null cell) ; Ran out of conses - index is too large.
                (error 'core:sequence-out-of-bounds
                       :datum index :object sequence
                       :expected-type `(integer 0 ,(1- (core:cons-length sequence)))))
               (t ; improper list.
                (error 'type-error :datum sequence :expected-type 'sequence)))))
      (vector (vector-read sequence index))
      (null (error 'core:sequence-out-of-bounds :datum index :expected-type '(integer 0 (0))
                                                :object sequence))
      (t (sequence:elt sequence index))))

  (debug-inline "core:setf-elt")
  (declaim (inline core:setf-elt))
  (defun core:setf-elt (sequence index new-value)
    (typecase sequence
      (cons
       (let ((cell (nthcdr index sequence)))
         (cond ((consp cell) (setf (car (cleavir-primop:the cons cell)) new-value))
               ((null cell) ; Ran out of conses - index is too large.
                (error 'core:sequence-out-of-bounds
                       :datum index :object sequence
                       :expected-type `(integer 0 ,(1- (core:cons-length sequence)))))
               (t ; improper list.
                (error 'type-error :datum sequence :expected-type 'sequence)))))
      (vector (vector-set sequence index new-value))
      (null (error 'core:sequence-out-of-bounds :datum index :expected-type '(integer 0 (0))
                                                :object sequence))
      (t (setf (sequence:elt sequence index) new-value)))))

;;; Redefinition of C++ function.
;;; NOTE: This will be faster if we use a generic function or implement typecase
;;;  in terms of generic function dispatch.
(declaim (inline core:coerce-fdesignator)
         (ftype (function ((or function symbol)) function)
                core:coerce-fdesignator))
(defun core:coerce-fdesignator (fdesignator)
  "Take a CL function designator and spit out a function."
  (etypecase fdesignator
    (function fdesignator)
    (symbol (fdefinition fdesignator))))

;;; ------------------------------------------------------------
;;;
;;;  Copied from clasp/src/lisp/kernel/lsp/pprint.lsp
;;;    and put here so that the inline definition is available
;;;
(in-package "SI")

(declaim (inline index-posn posn-index posn-column))
(defun index-posn (index stream)
  (declare (type index index) (type pretty-stream stream))
  (+ index (pretty-stream-buffer-offset stream)))
(defun posn-index (posn stream)
  (declare (type posn posn) (type pretty-stream stream))
  (- posn (pretty-stream-buffer-offset stream)))
(defun posn-column (posn stream)
  (declare (type posn posn) (type pretty-stream stream))
  (index-column (posn-index posn stream) stream))
