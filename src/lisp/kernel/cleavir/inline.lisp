(in-package :clasp-cleavir)

#+(or)
(eval-when (:execute)
  (format t "Setting core:*echo-repl-read* to T~%")
  (setq core:*echo-repl-read* t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf cmp::*debug-create-call* nil))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (setq core:*defun-inline-hook* 'defun-inline-hook))

;;;; Standard CL function types

;;; Install ftypes and types ASAP so that the bootstrap benefits from
;;; type information.

;; "Strict FUNCTION" - indicate that exactly this many values are returned.
(deftype sfunction (&optional lambda-list return-values)
  (cond ((eq return-values '*) ; no info
         `(function ,lambda-list ,return-values))
        ((and (consp return-values) (eq (car return-values) 'values))
         (assert (null (intersection return-values lambda-list-keywords)))
         `(function ,lambda-list (values ,@(rest return-values) &rest nil)))
        (t `(function ,lambda-list (values ,return-values &rest nil)))))

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
  `(or symbol (sfunction ,lambda-list ,return-values)))
(deftype extended-function-designator (&optional lambda-list return-values)
  `(or function-name (sfunction ,lambda-list ,return-values)))

(deftype predicate-function () '(sfunction (t) boolean))
(deftype predicate () '(function-designator (t)))

(deftype class-designator () '(or class symbol))

(deftype format-control ()
  ;; FORMATTER returns functions like this.
  '(or string (sfunction (stream &rest t) list)))

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
(deftype boole-spec ()
  ;; on clasp, boole specs are contiguous from 0. and right now the member type
  ;; does not work very efficiently.
  `(integer 0
            ,(max boole-1 boole-2 boole-and boole-andc1 boole-andc2
                  boole-c1 boole-c2 boole-clr boole-eqv boole-ior
                  boole-nand boole-nor boole-orc1 boole-orc2
                  boole-set boole-xor))
  #+(or)
  `(member ,BOOLE-1 ,BOOLE-2 ,BOOLE-AND ,BOOLE-ANDC1 ,BOOLE-ANDC2
           ,BOOLE-C1 ,BOOLE-C2 ,BOOLE-CLR ,BOOLE-EQV ,BOOLE-IOR
           ,BOOLE-NAND ,BOOLE-NOR ,BOOLE-ORC1 ,BOOLE-ORC2 ,BOOLE-SET ,BOOLE-XOR))

(deftype byte-specifier () 't) ; implementation defined

(deftype character-code () `(integer 0 (,char-code-limit)))

(deftype key () '(maybe predicate))
(deftype test () '(function-designator (t t)))

(deftype sequence-function (positional-parameters more-keys return-value)
  `(sfunction (,@positional-parameters &key (:from-end t) (:key key)
                                      (:start sequence-index) (:end end-index-designator)
                                      ,@more-keys)
             ,return-value))
(deftype test-sequence-function (positional-parameters more-keys return-value)
  `(sequence-function ,positional-parameters ((:test test) (:test-not test)
                                              ,@more-keys)
                      ,return-value))

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
  `(sfunction (t &key (:array t) (:base radix)
                 (:case (member :upcase :downcase :capitalize))
                 (:circle t) (:escape t) (:gensym t)
                 (:length (maybe (integer 0))) (:level (maybe (integer 0)))
                 (:lines (maybe (integer 0))) (:miser-width (maybe (integer 0)))
                 (:pprint-dispatch pprint-dispatch-table)
                 (:pretty t) (:radix t) (:readably t)
                 (:right-margin (maybe (integer 0)))
                 (:stream stream-designator))
              ,ret))

(deftype case-sensitivity-mode () '(member :upcase :downcase :preserve :invert))

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

(declaim (ftype (sfunction ((maybe function-name) &optional (or lambda-expression function))
                           (values (or function-name compiled-function) t t))
                compile)
         (ftype (function (form) *) eval)
         (ftype (sfunction (function-name &optional environment)
                           (maybe compiler-macro-function))
                compiler-macro-function)
         (ftype (sfunction ((maybe compiler-macro-function) function-name &optional environment)
                           (maybe compiler-macro-function))
                (setf compiler-macro-function))
         (ftype (sfunction (function-name &optional environment)
                           (maybe macro-function))
                macro-function)
         (ftype (sfunction ((maybe macro-function) function-name &optional environment)
                           (maybe macro-function))
                (setf macro-function))
         (ftype (sfunction (form &optional environment) (values form t))
                macroexpand macroexpand-1)
         (ftype (function (declaration-specifier) *) proclaim)
         (ftype (sfunction (symbol) t) special-operator-p)
         ;; Although technically constantp is specified to take a form, in practice
         ;; it can get called on some unevaluable things, and the ansi tests do this.
         (ftype (sfunction (t &optional environment) t) constantp))

;;; Chapter 4 Types and Classes

(declaim (ftype (sfunction (t type-specifier) t) coerce)
         (ftype (sfunction (type-specifier type-specifier &optional environment)
                           (values t t))
                subtypep)
         (ftype (sfunction (t) type-specifier) type-of)
         (ftype (sfunction (t type-specifier &optional environment) t) typep)
         (ftype (sfunction (type-error) t) type-error-datum)
         (ftype (sfunction (type-error) type-specifier) type-error-expected-type))

;;; Chapter 5 Data and Control Flow

(declaim (ftype (function (function-designator &rest t)) apply)
         (ftype (sfunction (function-name) t) fdefinition) ; special-form/macro case. fixme
         (ftype (sfunction (function function-name) function) (setf fdefinition))
         (ftype (sfunction (function-name) t) fboundp)
         (ftype (sfunction (function-name) function-name) fmakunbound)
         (ftype (function (function-designator &rest t)) funcall)
         (ftype (sfunction (function) (values (maybe lambda-expression) t t))
                function-lambda-expression)
         (ftype predicate-function functionp)
         (ftype predicate-function compiled-function-p)
         (ftype (sfunction (t) boolean) not)
         (ftype (sfunction (t t) t) eq eql equal equalp)
         (ftype (sfunction (function) function) complement)
         (ftype (sfunction (t) function) constantly)
         (ftype (sfunction (function-designator &rest sequence) t)
                every some notevery notany)
         (ftype (function (&rest t)) values)
         ;; FIXME: values-list also accepts valists, but the type system doesn't
         ;; really know about those. Also they probably ideally wouldn't be
         ;; first-class regardless.
         (ftype (function (t)) values-list)
         (ftype (sfunction (form &optional environment)
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
                 (class t symbol (member setf slot-boundp slot-makunbound slot-value #+clasp mp:cas) &optional t)
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
         (ftype (sfunction (t &key (:slot-names list) (:environment environment))
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

(declaim (ftype (sfunction (structure-object) structure-object) copy-structure))

;;; Chapter 9 Conditions

(declaim (ftype (sfunction (cell-error) t) cell-error-name)
         (ftype (function (condition-designator &rest t) nil) error)
         (ftype (sfunction (format-control condition-designator &rest t) null) cerror)
         (ftype (function (method format-control &rest t)) invalid-method-error)
         (ftype (function (format-control &rest t)) method-combination-error)
         (ftype (sfunction (condition-designator &rest t) null) signal warn)
         (ftype (sfunction (condition) t) simple-condition-format-control)
         (ftype (sfunction (condition) t) simple-condition-format-arguments)
         (ftype (function (condition) nil) invoke-debugger)
         (ftype (sfunction (&optional format-control &rest t) null) break)
         (ftype (function (type-specifier &key &allow-other-keys) condition) make-condition)
         (ftype (sfunction (&optional (maybe condition)) list) compute-restarts)
         (ftype (sfunction (restart-designator &optional (maybe condition))
                           (maybe restart))
                find-restart)
         (ftype (function (restart-designator &rest t)) invoke-restart)
         (ftype (function (restart-designator)) invoke-restart-interactively)
         (ftype (sfunction (restart) symbol) restart-name)
         (ftype (function (&optional (maybe condition)) nil) abort muffle-warning)
         (ftype (sfunction (&optional (maybe condition)) null) continue)
         (ftype (sfunction (t &optional (maybe condition)) null) store-value use-value))

;;; Chapter 10 Symbols

(declaim (ftype predicate-function symbolp keywordp boundp)
         (ftype (sfunction (string) symbol) make-symbol)
         (ftype (sfunction (symbol &optional t) symbol) copy-symbol)
         (ftype (sfunction (&optional (or string (integer 0))) symbol) gensym)
         (ftype (sfunction (&optional string package-designator) symbol) gentemp)
         (ftype (sfunction (symbol)) symbol-function) ; see FDEFINITION note
         (ftype (sfunction (function symbol) function) (setf symbol-function))
         (ftype (sfunction (symbol) string) symbol-name)
         (ftype (sfunction (symbol) (maybe package)) symbol-package)
         (ftype (sfunction (symbol) list) symbol-plist)
         (ftype (sfunction (list symbol) list) (setf symbol-plist))
         (ftype (sfunction (symbol) t) symbol-value)
         (ftype (sfunction (t symbol) t) (setf symbol-value))
         (ftype (sfunction (symbol t &optional t) t) get)
         (ftype (sfunction (t symbol t &optional t) t) (setf get))
         (ftype (sfunction (symbol t) t) remprop)
         (ftype (sfunction (symbol) symbol) makunbound)
         (ftype (sfunction (symbol t) t) set))

;;; Chapter 11 Packages

(declaim (ftype (sfunction ((list-designator symbol) &optional package-designator) (eql t))
                export import shadowing-import unexport)
         (ftype (sfunction (string &optional package-designator)
                           (values symbol (member :inherited :internal :external nil)))
                find-symbol intern)
         (ftype (sfunction ((or package string-designator)) (maybe package)) find-package)
         (ftype (sfunction (string-designator) list) find-all-symbols)
         (ftype (sfunction () list) list-all-packages)
         (ftype (sfunction (package-designator package-designator &optional list) package)
                rename-package)
         (ftype (sfunction ((list-designator string-designator) &optional package-designator) (eql t))
                shadow)
         (ftype (sfunction (package-designator) t) delete-package)
         (ftype (sfunction (string-designator &key (:nicknames list) (:use list)) package)
                make-package)
         (ftype (sfunction (symbol &optional package-designator) t) unintern)
         (ftype (sfunction ((list-designator package-designator) &optional package-designator) (eql t))
                use-package unuse-package)
         (ftype (sfunction (package-designator) (maybe string)) package-name)
         (ftype (function (package-designator) list)
                package-nicknames package-shadowing-symbols package-use-list package-used-by-list)
         (ftype predicate-function packagep)
         (ftype (sfunction (package-error) t) package-error-package))

;;; Chapter 12 Numbers

(declaim (ftype (sfunction (&rest number) t) = /=)
         (ftype (sfunction (&rest real) t) < > <= >=)
         (ftype (sfunction (&rest real) real) max min)
         (ftype (sfunction (real) t) plusp minusp)
         (ftype (sfunction (number) t) zerop)
         (ftype (sfunction (real &optional real) (values integer real))
                ;; note: the optional real is nonzero. FIXME?
                floor ceiling truncate round)
         (ftype (sfunction (real &optional real) (values float real))
                ffloor fceiling ftruncate fround)
         (ftype (sfunction (number) number)
                sin cos tan asin acos sinh cosh tanh asinh acosh atanh
                1+ 1- exp signum sqrt conjugate phase)
         (ftype (sfunction (number &optional number) number) atan)
         (ftype (sfunction (&rest number) number) * +)
         ;; note: for /, the &rest are nonzero
         (ftype (sfunction (number &rest number) - /))
         (ftype (sfunction (number) real) abs realpart imagpart)
         (ftype (sfunction (integer) t) evenp oddp)
         (ftype (sfunction (number number) number) expt)
         (ftype (sfunction (&rest integer) (integer 0)) gcd lcm)
         ;; note: first argument is nonzero
         (ftype (sfunction (number &optional number) number) log)
         (ftype (sfunction (real real) real) mod rem)
         (ftype (sfunction ((integer 0)) (integer 0)) isqrt)
         (ftype (sfunction (&optional (or random-state boolean)) random-state) make-random-state)
         (ftype (sfunction ((or (integer (0)) (float (0.0))) &optional random-state)
                           (or (float 0.0) (integer 0)))
                random)
         (ftype predicate-function random-state-p numberp complexp realp rationalp integerp floatp)
         (ftype (sfunction (real) complex) cis)
         (ftype (sfunction (real &optional real) (or rational complex)) complex)
         (ftype (sfunction (type-specifier &optional environment) type-specifier)
                upgraded-complex-part-type)
         (ftype (sfunction (rational) integer) numerator)
         (ftype (sfunction (rational) (integer (0))) denominator)
         (ftype (sfunction (real) rational) rational rationalize)
         (ftype (sfunction (integer integer) integer)
                ash logandc1 logandc2 lognand lognor logorc1 logorc2)
         (ftype (sfunction (integer) (integer 0)) integer-length logcount)
         (ftype (sfunction (string &key (:start sequence-index) (:end end-index-designator)
                                   (:radix radix) (:junk-allowed t))
                           (values (maybe integer) sequence-index))
                parse-integer)
         (ftype (sfunction (boole-spec integer integer) integer) boole)
         (ftype (sfunction (&rest integer) integer) logand logeqv logior logxor)
         (ftype (sfunction (integer) integer) lognot)
         (ftype (sfunction ((integer 0) integer) t) logbitp)
         (ftype (sfunction (integer integer) t) logtest)
         (ftype (sfunction ((integer 0) (integer 0)) byte-specifier) byte)
         (ftype (sfunction (byte-specifier) (integer 0)) byte-size byte-position)
         (ftype (sfunction (integer byte-specifier integer) integer) deposit-field dpb)
         (ftype (sfunction (byte-specifier integer) (integer 0)) ldb mask-field)
         (ftype (sfunction ((integer 0) byte-specifier integer) (integer 0))
                (setf ldb) (setf mask-field))
         (ftype (sfunction (byte-specifier integer) t) ldb-test)
         (ftype (sfunction (float) (values float integer float)) decode-float)
         (ftype (sfunction (float integer) float) scale-float)
         (ftype (sfunction (float) integer) float-radix)
         (ftype (sfunction (float &optional float) float) float-sign)
         (ftype (sfunction (float) (integer 0)) float-digits float-precision)
         (ftype (sfunction (float) (values integer integer (member -1 1))) integer-decode-float)
         (ftype (sfunction (real &optional float) float) float)
         (ftype (sfunction (arithmetic-error) t) arithmetic-error-operands)
         (ftype (sfunction (arithmetic-error) t) arithmetic-error-operation))

;;; Chapter 13 Characters

(declaim (ftype (sfunction (&rest character) t)
                char= char/= char< char> char<= char>=
                char-equal char-not-equal char-lessp char-greaterp
                char-not-greaterp char-not-lessp)
         (ftype (sfunction (character-designator) character) character)
         (ftype predicate-function characterp)
         (ftype (sfunction (character) t)
                alpha-char-p alphanumericp graphic-char-p standard-char-p
                upper-case-p lower-case-p both-case-p)
         (ftype (sfunction ((integer 0) &optional radix) (maybe character)) digit-char)
         (ftype (sfunction (character &optional radix) (maybe (integer 0 36))) digit-char-p)
         (ftype (sfunction (character) character) char-upcase char-downcase)
         (ftype (sfunction (character) character-code) char-code)
         (ftype (sfunction (character) (integer 0)) char-int)
         (ftype (sfunction (character-code) (maybe character)) code-char)
         (ftype (sfunction (character) (maybe string)) char-name)
         (ftype (sfunction (string-designator) (maybe character)) name-char))

;;; Chapter 14 Conses

(declaim (ftype (sfunction (t t) cons) cons)
         (ftype predicate-function consp atom listp null)
         (ftype (sfunction (cons t) cons) rplaca rplacd)
         (ftype (sfunction (list) t)
                car cdr caar cadr cdar cddr caaar caadr cadar cdaar caddr cdadr cddar cdddr
                caaaar caaadr caadar cadaar cdaaar caaddr cadadr cdaadr caddar cdadar cddaar
                cadddr cdaddr cddadr cdddar cddddr
                first second third fourth fifth sixth seventh eighth ninth tenth rest)
         (ftype (sfunction (t list) t)
                (setf car) (setf cdr) (setf caar) (setf cadr) (setf cdar) (setf cddr)
                (setf caaar) (setf caadr) (setf cadar) (setf cdaar) (setf caddr) (setf cdadr)
                (setf cddar) (setf cdddr) (setf caaaar) (setf caaadr) (setf caadar)
                (setf cadaar) (setf cdaaar) (setf caaddr) (setf cadadr) (setf cdaadr)
                (setf caddar) (setf cdadar) (setf cddaar) (setf cadddr) (setf cdaddr)
                (setf cddadr) (setf cdddar) (setf cddddr) (setf rest)
                (setf first) (setf second) (setf third) (setf fourth) (setf fifth)
                (setf sixth) (setf seventh) (setf eighth) (setf ninth) (setf tenth))
         (ftype (sfunction (t) t) copy-tree)
         (ftype (sfunction (list t &key (:key key) (:test test) (:test-not test)) t)
                sublis nsublis)
         (ftype (sfunction (t t t &key (:key key)) t)
                subst-if subst-if-not nsubst-if nsubst-if-not)
         (ftype (sfunction (t t &key (:test test) (:test-not test)) t) tree-equal)
         (ftype (sfunction (list) list) copy-list)
         (ftype (sfunction (&rest t) list) list)
         (ftype (sfunction (&rest t) t) list*)
         (ftype (sfunction (list) (maybe (integer 0))) list-length)
         (ftype (sfunction ((integer 0) &key (:initial-element t)) list) make-list)
         (ftype (sfunction ((integer 0) list) t) nth nthcdr)
         (ftype (sfunction (t (integer 0) list) t) (setf nth))
         (ftype (sfunction (list) t) endp)
         (ftype (sfunction (&rest t) list) nconc)
         (ftype (sfunction (&rest t) append) t)
         (ftype (sfunction (list t) list) revappend nreconc)
         (ftype (sfunction (list &optional (integer 0)) list) butlast nbutlast)
         (ftype (sfunction (list &optional (integer 0)) t) last)
         (ftype (sfunction (list t) list) ldiff)
         (ftype (sfunction (t list) t) tailp)
         (ftype (sfunction (t list &key (:key key) (:test test) (:test-not test)) list) member)
         (ftype (sfunction (predicate list &key (:key key)) list) member-if member-if-not)
         (ftype (sfunction (function-designator list &rest list) list)
                mapc mapcar mapcan mapl maplist mapcon)
         ;; Although the standard says the alist is an alist, it also says
         ;; (in the non normative notes) that acons = cons cons, which doesn't have
         ;; such a requirement; and really I don't see much value in strictness here.
         (ftype (sfunction (t t t) list) acons)
         (ftype (sfunction (t list &key (:key key) (:test test) (:test-not test)) (maybe cons))
                assoc rassoc)
         (ftype (sfunction (predicate list &key (:key key)) (maybe cons))
                assoc-if assoc-if-not rassoc-if rassoc-if-not)
         (ftype (sfunction (list) list) copy-alist)
         (ftype (sfunction (list list &optional list) list) pairlis)
         (ftype (sfunction (list list) (values t t list)) get-properties)
         (ftype (sfunction (list t &optional t) t) getf)
         (ftype (sfunction (t list t &optional t) t) (setf getf))
         (ftype (sfunction (list list &key (:key key) (:test test) (:test-not test)) list)
                intersection nintersection set-difference nset-difference
                set-exclusive-or nset-exclusive-or union nunion)
         (ftype (sfunction (t list &key (:key key) (:test test) (:test-not test)) list) adjoin)
         (ftype (sfunction (list list &key (:key key) (:test test) (:test-not test)) t) subsetp))

;;; Chapter 15 Arrays

(declaim (ftype (sfunction ((list-designator valid-array-dimension)
                            &key (:element-type type-specifier) (:initial-element t)
                            (:initial-contents t) (:adjustable t)
                            (:fill-pointer (or boolean valid-row-major-index))
                            (:displaced-to (maybe array))
                            (:displaced-index-offset valid-row-major-index))
                           array)
                make-array)
         (ftype (sfunction (array (list-designator valid-array-dimension)
                                  &key (:element-type type-specifier) (:initial-element t)
                                  (:initial-contents t) (:adjustable t)
                                  (:fill-pointer (or boolean valid-row-major-index))
                                  (:displaced-to (maybe array))
                                  (:displaced-index-offset valid-row-major-index))
                           array)
                adjust-array)
         (ftype (sfunction (array) t) adjustable-array-p array-has-fill-pointer-p)
         (ftype (sfunction (array &rest valid-array-index) t) aref)
         (ftype (sfunction (t array &rest valid-array-index) t) (setf aref))
         (ftype (sfunction (array valid-array-axis) valid-array-dimension) array-dimension)
         (ftype (sfunction (array) list) array-dimensions)
         (ftype (sfunction (array) type-specifier) array-element-type)
         (ftype (sfunction (array) (values (maybe array) valid-row-major-index))
                array-displacement)
         (ftype (sfunction (array &rest integer) t) array-in-bounds-p)
         (ftype (sfunction (array) valid-array-rank) array-rank)
         (ftype (sfunction (array &rest valid-array-index) valid-row-major-index)
                array-row-major-index)
         (ftype (sfunction (array) valid-array-size) array-total-size)
         (ftype predicate-function arrayp simple-vector-p vectorp bit-vector-p simple-bit-vector-p)
         (ftype (sfunction (vector) valid-array-index) fill-pointer)
         (ftype (sfunction (valid-array-index vector) valid-array-index) (setf fill-pointer))
         (ftype (sfunction (array valid-row-major-index) t) row-major-aref)
         (ftype (sfunction (t array valid-row-major-index) t) (setf row-major-aref))
         (ftype (sfunction (type-specifier &optional environment) type-specifier)
                upgraded-array-element-type)
         (ftype (sfunction (simple-vector valid-array-index) t) svref)
         (ftype (sfunction (t simple-vector valid-array-index) t) (setf svref))
         (ftype (sfunction (&rest t) (vector t)) vector)
         (ftype (sfunction (vector) t) vector-pop)
         (ftype (sfunction (t vector) (maybe valid-array-index)) vector-push)
         (ftype (sfunction (t vector &optional (integer 0)) valid-array-index) vector-push-extend)
         (ftype (sfunction ((array bit) &rest valid-array-index) bit) bit)
         (ftype (sfunction (bit (array bit) &rest valid-array-index) bit) (setf bit))
         (ftype (sfunction ((simple-array bit) &rest valid-array-index) bit) sbit)
         (ftype (sfunction (bit (simple-array bit) &rest valid-array-index) bit) (setf sbit))
         (ftype (sfunction ((array bit) (array bit) &optional (or boolean (array bit))) (array bit))
                bit-and bit-andc1 bit-andc2 bit-eqv bit-ior bit-nand bit-nor
                bit-orc1 bit-orc2 bit-xor)
         (ftype (sfunction ((array bit) &optional (or boolean (array bit))) (array bit)) bit-not))

;;; Chapter 16 Strings

(declaim (ftype predicate-function simple-string-p stringp)
         (ftype (sfunction (string valid-array-index) character) char)
         (ftype (sfunction (character string valid-array-index) character) (setf char))
         (ftype (sfunction (simple-string valid-array-index) character) schar)
         (ftype (sfunction (character simple-string valid-array-index) character) (setf schar))
         (ftype (sfunction (string-designator) string) string)
         (ftype (sfunction (string-designator &key
                                             (:start sequence-index) (:end end-index-designator))
                          string)
                string-upcase string-downcase string-capitalize)
         (ftype (sfunction (string &key (:start sequence-index) (:end end-index-designator)) string)
                nstring-upcase nstring-downcase nstring-capitalize)
         (ftype (sfunction (sequence string-designator) string)
                string-trim string-left-trim string-right-trim)
         (ftype (sfunction (string-designator string-designator
                                              &key (:start1 sequence-index)
                                              (:end1 end-index-designator)
                                              (:start2 sequence-index)
                                              (:end2 end-index-designator))
                           (maybe sequence-index))
                string/= string< string> string<= string>=
                string-not-equal string-lessp string-greaterp
                string-not-greaterp string-not-lessp)
         (ftype (sfunction (string-designator string-designator
                                              &key (:start1 sequence-index)
                                              (:end1 end-index-designator)
                                              (:start2 sequence-index)
                                              (:end2 end-index-designator))
                           t)
                string= string-equal)
         (ftype (sfunction (valid-array-dimension &key (:initial-element character)
                                                  (:element-type type-specifier))
                           simple-string)
                make-string))

;;; Chapter 17 Sequences

(declaim (ftype (sfunction (sequence) sequence) copy-seq)
         (ftype (sfunction (sequence sequence-index) t) elt)
         (ftype (sfunction (t sequence sequence-index) t) (setf elt))
         (ftype (sfunction (sequence t &key (:start sequence-index) (:end end-index-designator))
                           sequence)
                fill) ; clhs bug: says item must be a sequence
         (ftype (sfunction (type-specifier (integer 0) &key (:initial-element t)) sequence)
                make-sequence)
         (ftype (sfunction (sequence sequence-index &optional end-index-designator) sequence)
                subseq)
         (ftype (sfunction (sequence sequence sequence-index &optional end-index-designator)
                          sequence)
                (setf subseq))
         (ftype (sfunction (type-specifier function-designator sequence &rest sequence)
                          (or sequence null))
                map)
         (ftype (sfunction (sequence function-designator &rest sequence) sequence) map-into)
         (ftype (sequence-function (function-designator sequence) ((:initial-value t)) t)
                reduce)
         (ftype (test-sequence-function (t sequence) () (integer 0)) count)
         (ftype (sequence-function (predicate sequence) () (integer 0))
                count-if count-if-not)
         (ftype (sfunction (sequence) (integer 0)) length)
         (ftype (sfunction (sequence) sequence) reverse nreverse)
         (ftype (sfunction (sequence (function-designator (t t)) &key (:key key)) sequence)
                sort stable-sort)
         (ftype (test-sequence-function (t sequence) () t) find)
         (ftype (sequence-function (predicate sequence) () t)
                find-if find-if-not)
         (ftype (test-sequence-function (t sequence) () (maybe sequence-index)) position)
         (ftype (sequence-function (predicate sequence) () (maybe sequence-index))
                position-if position-if-not)
         (ftype (sfunction (sequence sequence &key (:from-end t) (:key key)
                                     (:test test) (:test-not test)
                                     (:start1 sequence-index) (:end1 end-index-designator)
                                     (:start2 sequence-index) (:end2 end-index-designator))
                           (maybe sequence-index))
                search mismatch)
         (ftype (sfunction (sequence sequence &key
                                     (:start1 sequence-index) (:end1 end-index-designator)
                                     (:start2 sequence-index) (:end2 end-index-designator))
                           sequence)
                replace)
         (ftype (test-sequence-function (t t sequence) ((:count (maybe integer))) sequence)
                substitute nsubstitute)
         (ftype (sequence-function (t t sequence) ((:count (maybe integer))) sequence)
                substitute-if substitute-if-not nsubstitute-if nsubstitute-if-not)
         (ftype (sfunction (type-specifier &rest sequence) sequence) concatenate)
         (ftype (sfunction (type-specifier sequence sequence (function-designator (t t))
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

(declaim (ftype (sfunction (&key (:test (function-designator (t t))) (:size (integer 0))
                                 (:rehash-size (or (integer 1) (float (1.0))))
                                 (:rehash-threshold (real 0 1)))
                           hash-table)
                make-hash-table)
         (ftype predicate-function hash-table-p)
         (ftype (sfunction (hash-table) (integer 0)) hash-table-count hash-table-size)
         (ftype (sfunction (hash-table) (or (integer 1) (float (1.0)))) hash-table-rehash-size)
         (ftype (sfunction (hash-table) (real 0 1)) hash-table-rehash-threshold)
         (ftype (sfunction (hash-table) (function-designator (t t))) hash-table-test)
         (ftype (sfunction (t hash-table &optional t) (values t t)) gethash)
         (ftype (sfunction (t t hash-table &optional t) t) (setf gethash))
         (ftype (sfunction (t hash-table) t) remhash)
         (ftype (sfunction ((function-designator (t t)) hash-table) null) maphash)
         (ftype (sfunction (hash-table) hash-table) clrhash)
         (ftype (sfunction (t) (and fixnum (integer 0))) sxhash))

;;; Chapter 19 Filenames

(declaim (ftype (sfunction (pathname-designator) pathname) pathname)
         (ftype (sfunction (&key (:host valid-physical-pathname-host)
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
         (ftype (sfunction (pathname-designator &key (:case pathname-case)) valid-physical-pathname-host)
                pathname-host)
         (ftype (sfunction (pathname-designator &key (:case pathname-case)) valid-pathname-device)
                pathname-device)
         (ftype (sfunction (pathname-designator &key (:case pathname-case)) valid-pathname-directory)
                pathname-directory)
         (ftype (sfunction (pathname-designator &key (:case pathname-case)) valid-pathname-name)
                pathname-name)
         (ftype (sfunction (pathname-designator &key (:case pathname-case)) valid-pathname-type)
                pathname-type)
         (ftype (sfunction (pathname-designator) valid-pathname-version) pathname-version)
         (ftype (sfunction (string) t) load-logical-pathname-translations)
         (ftype (sfunction (logical-host-designator) list) logical-pathname-translations)
         (ftype (sfunction (list logical-host-designator) list)
                (setf logical-pathname-translations))
         (ftype (sfunction ((or logical-pathname string stream)) logical-pathname)
                logical-pathname)
         (ftype (sfunction (pathname-designator) (maybe string))
                namestring file-namestring directory-namestring host-namestring)
         (ftype (sfunction (pathname-designator &optional pathname-designator) (maybe string))
                enough-namestring)
         (ftype (sfunction ((or string pathname stream)
                            &optional (or valid-physical-pathname-host logical-host null)
                            pathname-designator
                            &key (:start sequence-index) (:end end-index-designator)
                            (:junk-allowed t))
                           (values (maybe pathname) sequence-index))
                parse-namestring)
         ;; See note on set-pprint-dispatch for why arg1 is not pathname-designator.
         (ftype (sfunction (t
                            &optional (member :host :device :directory :name :type
                                              :version nil))
                           t)
                wild-pathname-p)
         (ftype (sfunction (pathname-designator pathname-designator) t) pathname-match-p)
         ;; valid keyword arguments are implementation-defined
         (ftype (sfunction ((or pathname-designator string) &key &allow-other-keys) pathname)
                translate-logical-pathname)
         ;; ditto
         (ftype (sfunction (pathname-designator pathname-designator pathname-designator
                                                &key &allow-other-keys)
                           pathname)
                translate-pathname)
         (ftype (sfunction (pathname-designator &optional pathname-designator valid-pathname-version)
                           pathname)
                merge-pathnames))

;;; Chapter 20 Files

;; keys are impl defined
(declaim (ftype (sfunction (pathname-designator &key &allow-other-keys) list) directory)
         (ftype (sfunction (pathname-designator) (maybe pathname)) probe-file)
         (ftype (sfunction (pathname-designator &key (:verbose t))
                           (values pathname-designator t))
                ensure-directories-exist)
         (ftype (sfunction (pathname-designator) physical-pathname) truename)
         (ftype (sfunction (pathname-designator) (maybe string)) file-author)
         (ftype (sfunction (pathname-designator) (maybe universal-time)) file-write-date)
         (ftype (sfunction (pathname-designator (and pathname-designator (not stream)) &key (if-exists t)) ; FIXME: give a more precise type here.
                          (values pathname physical-pathname physical-pathname))
                rename-file)
         (ftype (sfunction (pathname-designator) (eql t)) delete-file)
         (ftype (sfunction (file-error) t) file-error-pathname))

;;; Chapter 21 Streams
(declaim (ftype (sfunction (stream) t)
                input-stream-p output-stream-p interactive-stream-p open-stream-p)
         (ftype (sfunction (stream) type-specifier) stream-element-type)
         (ftype predicate-function streamp)
         (ftype (sfunction (stream &optional t t) t) read-byte) ; can be eof value
         (ftype (sfunction (integer stream) integer) write-byte)
         (ftype (sfunction (&optional (or character boolean) stream-designator t t t) t)
                peek-char)
         (ftype (sfunction (&optional stream-designator t t t) t)
                read-char read-char-no-hang)
         (ftype (sfunction (&optional stream-designator) null) terpri)
         (ftype (sfunction (&optional stream-designator) t) fresh-line)
         (ftype (sfunction (character &optional stream-designator) null) unread-char)
         (ftype (sfunction (character &optional stream-designator) character) write-char)
         (ftype (sfunction (&optional stream-designator t t t) (values t t)) read-line)
         (ftype (sfunction (string &optional stream-designator
                                  &key (:start sequence-index) (:end end-index-designator))
                          string)
                write-string write-line)
         (ftype (sfunction (sequence stream &key (:start sequence-index) (:end end-index-designator))
                           sequence-index)
                read-sequence)
         (ftype (sfunction (sequence stream &key (:start sequence-index) (:end end-index-designator))
                           sequence)
                write-sequence)
         (ftype (sfunction (stream) (maybe (integer 0))) file-length)
         (ftype (sfunction (stream &optional file-position-designator) t) file-position)
         (ftype (sfunction (stream (or string character)) (maybe (integer 0))) file-string-length)
         (ftype (sfunction (pathname-designator
                            &key (:direction (member :input :output :io :probe))
                            (:element-type type-specifier)
                            (:if-exists (member :error :new-version :rename :rename-and-delete
                                                :overwrite :append :supersede nil))
                            (:if-does-not-exist (member :error :create nil))
                            (:external-format external-file-format-designator))
                           (maybe stream))
                open)
         (ftype (sfunction (stream) t) stream-external-format)
         (ftype (sfunction (stream &key (:abort t)) t) close)
         (ftype (sfunction (&optional stream-designator) t) listen)
         (ftype (sfunction (&optional stream-designator) null)
                clear-input finish-output force-output clear-output)
         (ftype (sfunction (&optional format-control &rest t) t) y-or-n-p yes-or-no-p)
         (ftype (sfunction (symbol) synonym-stream) make-synonym-stream)
         (ftype (sfunction (synonym-stream) symbol) synonym-stream-symbol)
         (ftype (sfunction (broadcast-stream) list) broadcast-stream-streams)
         (ftype (sfunction (&rest stream) broadcast-stream) make-broadcast-stream)
         (ftype (sfunction (stream stream) two-way-stream) make-two-way-stream)
         (ftype (sfunction (two-way-stream) stream)
                two-way-stream-input-stream two-way-stream-output-stream)
         (ftype (sfunction (echo-stream) stream) echo-stream-input-stream echo-stream-output-stream)
         (ftype (sfunction (stream stream) echo-stream) make-echo-stream)
         (ftype (sfunction (concatenated-stream) list) concatenated-stream-streams)
         (ftype (sfunction (&rest stream) concatenated-stream) make-concatenated-stream)
         (ftype (sfunction (stream) string) get-output-stream-string)
         (ftype (sfunction (string &optional sequence-index end-index-designator) string-stream)
                make-string-input-stream)
         (ftype (sfunction (&key (:element-type type-specifier)) string-stream)
                make-string-output-stream)
         (ftype (sfunction (stream-error) t) stream-error-stream))

;;; Chapter 22 Printer

(declaim (ftype (sfunction (&optional (maybe pprint-dispatch-table))
                           pprint-dispatch-table)
                copy-pprint-dispatch)
         (ftype (sfunction (t &optional (maybe pprint-dispatch-table))
                           (values function-designator t))
                pprint-dispatch)
         (ftype (sfunction (stream-designator t &optional t t) null)
                pprint-fill pprint-linear)
         (ftype (sfunction (stream-designator t &optional t t (integer 0)) null)
                pprint-tabular)
         ;; First argument would be (member :block :current), except see
         ;; SET-PPRINT-DISPATCH below.
         (ftype (sfunction (t real &optional stream-designator) null)
                pprint-indent)
         ;; ditto with (member :linear :fill :miser :mandatory)
         (ftype (sfunction (t &optional stream-designator) null)
                pprint-newline)
         ;; ditto (member :line :section :line-relative :section-relative)
         (ftype (sfunction (t (integer 0) (integer 0) &optional stream-designator)
                           null)
                pprint-tab)
         (ftype (sfunction (t stream) t) print-object)
         ;; The third argument could be REAL, except that this function is specified
         ;; to signal an error if it's not, _regardless of safety level_. At safety 0,
         ;; if it can determine that the input is not a REAL, it may just dump in an
         ;; unsafe unreachability marker.
         ;; FIXME: It might be preferable to instead keep the error even in
         ;; unreachable code - it means increased code size, but eh.
         (ftype (sfunction (type-specifier (or function function-name null)
                                           &optional t pprint-dispatch-table)
                           null)
                set-pprint-dispatch)
         (ftype (write-function t) write)
         (ftype (sfunction (t &optional stream-designator) t) prin1 princ print)
         (ftype (sfunction (t &optional stream-designator) (values)) pprint)
         (ftype (write-function string) write-to-string)
         (ftype (sfunction (t) string) prin1-to-string princ-to-string)
         (ftype (sfunction (print-not-readable) t) print-not-readable-object)
         (ftype (sfunction ((or boolean stream string) format-control &rest t)
                           (maybe string))
                format))

;;; Chapter 23 Reader

(declaim (ftype (sfunction (&optional readtable-designator (maybe readtable)) readtable)
                copy-readtable)
         (ftype (sfunction (character &optional t readtable) (eql t))
                make-dispatch-macro-character)
         (ftype (sfunction (&optional stream-designator t t t) t)
                read read-preserving-whitespace)
         (ftype (sfunction (character &optional stream-designator t) list)
                read-delimited-list)
         (ftype (sfunction (string &optional t t
                                   &key (:start sequence-index) (:end end-index-designator)
                                   (:preserve-whitespace t))
                           (values t sequence-index))
                read-from-string)
         (ftype (sfunction (readtable) case-sensitivity-mode) readtable-case)
         (ftype (sfunction (case-sensitivity-mode readtable) case-sensitivity-mode)
                (setf readtable-case))
         (ftype predicate-function readtablep)
         (ftype (sfunction (character character &optional readtable-designator)
                           (maybe function-designator))
                get-dispatch-macro-character)
         (ftype (sfunction (character character function-designator
                                      &optional readtable-designator)
                           (eql t))
                set-dispatch-macro-character)
         (ftype (sfunction (character &optional readtable-designator)
                           (values (maybe (function-designator (t t))) t))
                get-macro-character)
         (ftype (sfunction (character (function-designator (t t))
                                      &optional t readtable-designator)
                           (eql t))
                set-macro-character)
         (ftype (sfunction (character character &optional readtable readtable-designator)
                           (eql t))
                set-syntax-from-char))

;;; Chapter 24 System Construction

(declaim (ftype (sfunction (pathname-designator &key (:output-file pathname-designator)
                                                (:verbose t) (:print t)
                                                (:external-format external-file-format-designator))
                           (values (maybe pathname) t t))
                compile-file)
         (ftype (sfunction (pathname-designator &key (:output-file pathname-designator)
                                                &allow-other-keys)
                           pathname)
                compile-file-pathname)
         (ftype (sfunction ((or stream pathname-designator)
                            &key (:verbose t) (:print t) (:if-does-not-exist t)
                            (:external-format external-file-format-designator))
                           t)
                load)
         (ftype (sfunction (string-designator)) provide)
         (ftype (sfunction (string-designator &optional (list-designator pathname-designator)))
                require))

;;; Chapter 25 Environment

(declaim (ftype (sfunction (universal-time &optional time-zone)
                           (values second minute hour date month year
                                   day-of-week dst-flag time-zone))
                decode-universal-time)
         (ftype (sfunction (second minute hour date month year &optional time-zone)
                           universal-time)
                encode-universal-time)
         (ftype (sfunction () universal-time) get-universal-time)
         (ftype (sfunction () (values second minute hour date month year
                                      day-of-week dst-flag time-zone))
                get-decoded-time)
         (ftype (sfunction ((real 0)) null) sleep)
         (ftype (sfunction (string-designator &optional (maybe package-designator))
                           (values))
                apropos)
         (ftype (sfunction (string-designator &optional (maybe package-designator)) list)
                apropos-list)
         (ftype (sfunction (t &optional stream-designator) (values)) describe)
         (ftype (sfunction (t stream)) describe-object)
         (ftype (sfunction () (integer 0)) get-internal-real-time get-internal-run-time)
         (ftype (sfunction ((or lambda-expression extended-function-designator) &key) null) ; clasp has custom keywords. maybe give a more precise type here.
                disassemble)
         (ftype (sfunction (t symbol) (maybe string)) documentation)
         (ftype (sfunction (string t symbol) string) (setf documentation))
         (ftype (sfunction (&optional (member t nil :default))) room)
         (ftype (sfunction (&optional (or null pathname string function-name))) ed)
         (ftype (sfunction (t)) inspect)
         (ftype (sfunction (&optional pathname-designator)) dribble)
         (ftype (sfunction () (maybe string))
                lisp-implementation-type lisp-implementation-version
                short-site-name long-site-name
                machine-instance machine-type machine-version
                software-type software-version)
         (ftype (sfunction (&optional (or string list (eql :unspecific))) (maybe pathname))
                user-homedir-pathname))

;;;; End standard CL function types.

;;;; Function types for Clasp specific operators.

(declaim (ftype (sfunction (number number) number)
                core:two-arg-+ core:two-arg-*
                core:two-arg-- core:two-arg-/)) ; for / we also have that the denominator can't be zero.
(declaim (ftype (sfunction (real real) t)
                core:two-arg-< core:two-arg-> core:two-arg-<=
                core:two-arg->=)
         (ftype (sfunction (number number) t) core:two-arg-=))
(declaim (ftype (sfunction (character character) t)
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
       (core:fmt t "debug-inline>> ")
       (core:fmt t ,msg ,@msg-args)
       (core:fmt t "%N")
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
  (unless (and (consp lambda-list)
               (eq (first lambda-list) '&whole))
    (warn "BUG: Bad ~a for ~a" 'define-cleavir-compiler-macro name))
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
        (let* ((func-info (cleavir-env:function-info
                           clasp-cleavir:*clasp-system* env name))
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

;;; We do this so that the compiler only has one form of variadic call to
;;; worry about. Also, perhaps counterintuitively, mv-call is actually
;;; easier for the compiler to work with than APPLY is, and more general
;;; besides.
(define-cleavir-compiler-macro apply (&whole form function &rest arguments)
  (if (null arguments)
      form ; invalid, let runtime handle the error
      `(multiple-value-call ,function
         ,@(loop for arg in (butlast arguments)
                 collect `(values ,arg))
         (values-list ,(first (last arguments))))))

(define-cleavir-compiler-macro values (&whole form &rest values)
  `(cleavir-primop:values ,@values))

;;; A compiler macro to avoid confusing bclasp.
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

(define-cleavir-compiler-macro cl:eq (&whole form x y)
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
                (define-cleavir-compiler-macro ,name (&whole form object)
                  `(cleavir-primop:typeq ,object ,',type))
                (defun ,name (o)
                  (cleavir-primop:typeq o ,type))))
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

;;; So. CAR and CDR are commonly used enough that doing a full function call for them
;;; noticeably slows down plenty of code (e.g. try cl-bench TAKL), even in case we
;;; have no type information about the list.
;;; But it's convenient for the compiler to just see CAR and CDR calls, since it can
;;; use the fancy inference on cons types (see type.lisp).
;;; So we provide this inline definition, but also put a notinline function call in
;;; it. The transform code (in bir-to-bmir.lisp) will take note of the declared type
;;; and reduce that "notinline" car to a primop, but that happens after type inference
;;; and so the compiler can use the type information.
;;; This is a KLUDGE, but it really seems to help in a lot of code.
(declaim (inline car cdr))
(defun car (list)
  "Return the first object in a list."
  (declare (optimize (safety 0)) (notinline car))
  (if (cleavir-primop:typeq list cons)
      (car (the cons list))
      (if (eq list nil)
          (the null list)
          (error 'type-error :datum list :expected-type 'list))))
(defun cdr (list)
  "Return all but the first object in a list."
  (declare (optimize (safety 0)) (notinline cdr))
  (typecase list
    (cons (cdr (the cons list)))
    (null (the null list))
    (t (error 'type-error :datum list :expected-type 'list))))

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

(define-cleavir-compiler-macro core:set-breakstep (&whole form)
  ;; Because the primop is for-effect, it must not be placed in a
  ;; position where it's expected to return anything.
  `(progn (core::primop core:set-breakstep) nil))
(define-cleavir-compiler-macro core:unset-breakstep (&whole form)
  `(progn (core::primop core:unset-breakstep) nil))

;;; ------------------------------------------------------------
;;;
;;; Array functions
;;;

(debug-inline "array-total-size")
(declaim (inline array-total-size)
         (ftype (function (array) sys:index) array-total-size))
(defun array-total-size (array)
  (etypecase array
    ((simple-array * (*)) (core::primop core::vector-length array))
    ;; MDArray
    (array (core::primop core::%array-total-size array))))

(debug-inline "array-rank")
(declaim (inline array-rank)
         (ftype (function (array) (integer 0 #.array-rank-limit)) array-rank))
(defun array-rank (array)
  (etypecase array
    ((simple-array * (*)) 1)
    (array (core::primop core::%array-rank array))))

(declaim (inline schar (setf schar) char (setf char))
         (ftype (function (simple-string sys:index) character) schar)
         (ftype (function (string sys:index) character) char)
         (ftype (function (character simple-string sys:index) character) (setf schar))
         (ftype (function (character string sys:index) character) (setf char)))
(defun schar (string index)
  ;; We use DECLARE instead of THE here so as to skip needless
  ;; multiple-value checkers. cclasp is not yet smart enough to realize that in
  ;; (fun (the string foo)) the type check function for STRING does not need to
  ;; preserve non-primary values.
  (declare (type simple-string string))
  (row-major-aref string index))
(defun (setf schar) (value string index)
  (declare (type simple-string string))
  (setf (row-major-aref string index) value))

(defun char (string index)
  (declare (type string string))
  (row-major-aref string index))
(defun (setf char) (value string index)
  (declare (type string string))
  (setf (row-major-aref string index) value))

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

;;; ------------------------------------------------------------
;;;
;;; Sequence functions
;;;

(declaim (ftype (function (sequence) sys:index) length))

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
(declaim (ftype (function (t) function) core:coerce-to-function))

;;; ------------------------------------------------------------
;;;
;;;  Copied from clasp/src/lisp/kernel/lsp/pprint.lisp
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

#+(or)
(eval-when (:execute)
  (format t "Setting core:*echo-repl-read* to NIL~%")
  (setq core:*echo-repl-read* nil))
