;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;
;;; PROCLAMATIONS of ECL and ANSI Common Lisp functions
;;;
;;; Copyright (c) 2010, Juan Jose Garcia Ripoll
;;;    Copying of this file is authorized to users who have executed the true
;;;    and proper "License Agreement for ECL".
;;;
;;; The function proclamations are created with PROCLAIM-FUNCTION, as in
;;;
;;;	(PROCLAMATION function-name ([arg-type]*) return-type
;;;		&rest {:no-sp-change|:pure|:reader|:no-side-effects})
;;;
;;; with the following interpretation: ARG-TYPE and RETURN-TYPE denote the most
;;; general types for the input and output values of this function. If the
;;; compiler detects that some of the values passed to this function does not
;;; match these types, it will generate an error. In addition to this, ECL
;;; contemplates different function properties:
;;;
;;; :NO-SP-CHANGE indicates that the function does not change the value of any
;;;        special variable, and it is used to perform code transformations.
;;;
;;; :NO-SIDE-EFFECTS is slightly stronger, as it indicates that the function
;;;        does not change variables or the content of objects in the
;;;        thread environment. Note the following:
;;;
;;;    - Allocating memory, creating objects, etc is not considered a side
;;;      effect, as it does not affect the code flow.
;;;    - Similarly, signalling errors is not considered a side effect.
;;;    - The environment may be changed by other threads. This is taken
;;;      into account (see below).
;;;
;;; :READER indicates that the function not only has no side effects, but its
;;;        value depends only on its arguments. However, :READER specifies that
;;;        the arguments are mutable.
;;;
;;; :PURE is the strictest class of functions. They have no side effects, the
;;;        output only depends on the arguments, the arguments are immutable
;;;        objects and the function call can be optimized away when the
;;;        arguments are constant.
;;;

(in-package "C")

(defun parse-function-proclamation
    (name arg-types return-type &rest properties)
  (when (sys:get-sysprop name 'proclaimed-arg-types)
    (warn "Duplicate proclamation for ~A" name))
  (#-new-cmp proclaim-function #+new-cmp c-env::proclaim-function
   name (list arg-types return-type))
  (loop for p in properties
     do (case p
          (:no-sp-change
           (sys:put-sysprop name 'no-sp-change t))
          ((:predicate :pure)
           (sys:put-sysprop name 'pure t)
           (sys:put-sysprop name 'no-side-effects t))
          ((:no-side-effects :reader)
           (sys:put-sysprop name 'no-side-effects t))
          (otherwise
           (error "Unknown property ~S in function proclamation for ~S"
                  p name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUXILIARY TYPES
;;

(deftype array-rank-index ()
  '(integer 0 #.(1- array-rank-limit)))
(deftype bit-array ()
  '(array bit *))
(deftype association-list ()
  'list
  #+(or)
  '(or null (cons cons association-list)))
(deftype byte-specifier ()
  "The type of the output of BYTE."
  '(cons integer-length integer-length))
(deftype character-code ()
  '(integer 0 #.(1- char-code-limit)))
(deftype character-designator ()
  '(or character string-designator))
(deftype external-file-format ()
  '(or symbol list))
(deftype declaration-specifier ()
  "Element that can appear in a DECLARE form"
  'list)
(deftype digit-weight ()
  '(integer 0 35))
(deftype environment ()
  "Environment used by compiler and interpreter"
  'list)
(deftype form ()
  "Valid lisp form"
  t)
(deftype format-control ()
  "Format control for FORMAT. It can be a string or a function returned by FORMATTER."
  '(or string function))
(deftype function-designator ()
  "An object that denotes a function and which can be a symbol or a function."
  '(or symbol function))
(deftype function-name ()
  "Valid name of a function, typically a symbol or (SETF symbol)"
  '(or list symbol))
(deftype gen-bool ()
  "Generalized boolean type"
  't)
(deftype integer-length ()
  "A type that fits maximum number of bits that an integer may have in this system"
  'ext:array-index)
(deftype natural ()
  "Non-negative number"
  '(integer 0 *))
(deftype package-designator ()
  '(or string-designator package))
(deftype pathname-designator ()
  '(or pathname string file-stream))
(deftype pathname-device ()
  '(or string (member nil :unspecific)))
(deftype pathname-directory ()
  '(or string list (member :wild :unspecific)))
(deftype pathname-host ()
  '(or string list (member nil :unspecific)))
(deftype pathname-name ()
  '(or string (member nil :wild :unspecific)))
(deftype pathname-type ()
  '(or string (member nil :wild :unspecific)))
(deftype pathname-version ()
  '(or unsigned-byte (member nil :wild :newest :unspecific)))
(deftype proper-list ()
  'list
  #+(or)
  '(or null (cons t proper-list)))
(deftype property-list ()
  'list
  #+(or)
  '(or null (cons t (cons t property-list))))
(deftype radix ()
  '(integer 2 36))
(deftype readtable-designator ()
  '(or null readtable))
(deftype restart-designator ()
  "Either a symbol naming a restart, or the restart object itself."
  '(or (and symbol (not null)) restart))
(deftype sequence-index ()
  '(integer 0 #.array-total-size-limit))
(deftype stream-designator ()
  '(or stream (member t nil)))
(deftype string-designator ()
  '(or symbol string character))
(deftype tree ()
  't)
(deftype type-specifier ()
  "Name or object representing a time."
  '(or symbol class list))
(deftype universal-time ()
  "Time represented as a non-negative number of seconds measured from the beginning of 1900."
  'unsigned-byte)
(deftype time-zone ()
  t)
(deftype ext:instance ()
  'standard-object)

(proclaim '(notinline ext:constantp-inner ext:constant-form-value))

(eval-when (:compile-toplevel :execute)
(defparameter +proclamations+ '(
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ALL FUNCTION DECLARATIONS
;;;
;;;
;;; ANSI SECTIONS
;;;
;;; 3. EVALUATION AND COMPILATION
;;;

(proclamation compile (function-name &optional (or list function))
              (values (or function-name function) gen-bool gen-bool))
(proclamation compiler-macro-function (function-name &optional environment)
              function)
(proclamation constantp (t &optional environment) gen-bool)
(proclamation eval (form) (values &rest t))
(proclamation macro-function (symbol &optional environment) function)
(proclamation macroexpand (form &optional environment) (values form gen-bool))
(proclamation macroexpand-1 (form &optional environment) (values form gen-bool))
(proclamation proclaim (declaration-specifier) (values &rest t))
(proclamation special-operator-p (symbol) gen-bool :pure)

;; ECL extensions:
(proclamation si:specialp (symbol) gen-bool :predicate)
(proclamation si::do-defsetf (symbol (or symbol function)) t)
(proclamation si::do-define-setf-method (symbol function) t)
(proclamation ext:constant-form-value (t &optional environment) t)
(proclamation ext:constantp-inner (t &optional environment) gen-bool)

;;;
;;; 4. TYPES AND CLASSES
;;;

(proclamation coerce (t type-specifier) t)
(proclamation subtypep (type-specifier type-specifier &optional environment)
              (values gen-bool gen-bool))
(proclamation type-of (t) type-specifier)
(proclamation typep (t type-specifier &optional environment) gen-bool)

; Slot accessors:
; (proclamation type-error-datum (condition) t)
; (proclamation type-error-expected-type (condition) t)

;; ECL EXTENSIONS
(proclamation si::coerce-to-vector (t type-specifier t gen-bool) vector :no-side-effects)
(proclamation si::coerce-to-function (function-designator) function :no-side-effects)

;;;
;;; 5. DATA AND CONTROL FLOW
;;;

(proclamation apply (function-designator &rest t) (values &rest t))
(proclamation fdefinition (function-name) (or list function (member 'SPECIAL)) :reader)
(proclamation fboundp (function-name) gen-bool :reader)
(proclamation fmakunbound (function-name) function-name)
(proclamation funcall (function-designator &rest t) (values &rest t))
(proclamation function-lambda-expression (function) (values list gen-bool t) :pure)
(proclamation functionp (t) gen-bool :pure)
(proclamation compiled-function-p (t) gen-bool :pure)
(proclamation not (gen-bool) boolean :pure)
(proclamation eq (t t) gen-bool :pure)
(proclamation eql (t t) gen-bool :pure)
(proclamation equal (t t) gen-bool :pure)
(proclamation equalp (t t) gen-bool :pure)
(proclamation identity (t) t)
(proclamation complement (function) function)
(proclamation constantly (t) function)
(proclamation every (function-designator sequence &rest sequence) gen-bool)
(proclamation some (function-designator sequence &rest sequence) t)
(proclamation notevery (function-designator sequence &rest sequence) gen-bool)
(proclamation notany (function-designator sequence &rest sequence) gen-bool)
(proclamation values-list (list) (values &rest t))
(proclamation get-setf-expansion (t &optional environment) (values t t t t t))

;; ECL extensions

(proclamation si:fset (function-name function &optional gen-bool t) function)
(proclamation si:setf-definition (function-name createp) list)
(proclamation ext:compiled-function-name (function) (or null function-name))
(proclamation si:compiled-function-block (function) (or null si::codeblock))
(proclamation ext:compiled-function-file (function) (values t t))

(proclamation ext:constantly-t () function)

(proclamation si:ihs-top () si::index)
(proclamation si:ihs-fun (si::index) (or null function-designator))
(proclamation si:ihs-env (si::index) environment)
(proclamation si:frs-top () si::index)
(proclamation si:frs-bds (si::index) si::index)
(proclamation si:frs-tag (si::index) t)
(proclamation si:frs-ihs (si::index) si::index)
(proclamation si:bds-top () si::index)
(proclamation si:bds-var (si::index) symbol)
(proclamation si:bds-val (si::index) t)
(proclamation si:sch-frs-base (si::index si::index) (or null si::index))

(proclamation si::ccase-error (t t t) t)
(proclamation si::ecase-error (t t) t)
(proclamation si::etypecase-error (t t) t)
(proclamation si::ctypecase-error (t t t) t)
(proclamation si::do-check-type (t t t t) t)

(proclamation si::dm-too-many-arguments (t) t)
(proclamation si::dm-too-few-arguments (t) t)

;;;
;;; 7. OBJECTS
;;;

#+clos
(proclamation ensure-generic-function (function-name &rest t) generic-function)
#+clos
(proclamation slot-boundp (si::instance symbol) gen-bool)
#+clos
(proclamation slot-exists-p (si::instance symbol) gen-bool)
#+clos
(proclamation slot-makunbound (si::instance symbol) si::instance)
#+clos
(proclamation slot-value (si::instance symbol) t)
#+clos
(proclamation make-load-form-saving-slots (t &rest t) (values t t))
#+clos
(proclamation find-class (symbol &optional gen-bool environment)
                   (or class null))
#+clos
(proclamation class-of (t) class :no-side-effects)

;; Slot accessors:
; (proclamation unbound-slot-instance (condition) si::instance :predicate)

#+clos
(proclamation clos::standard-instance-set (t ext:instance t) t)
#+clos
(proclamation clos:std-compute-applicable-methods (generic-function list) list)
#+clos
(proclamation clos:std-compute-effective-method (generic-function method-combination list) function)
#+clos
(proclamation clos:compute-effective-method-function (generic-function method-combination list) function)
#+clos
(proclamation clos::update-instance (ext:instance) (values))
#+clos
(proclamation clos::slot-value-set (t si::instance symbol) t)
#+clos
(proclamation clos:extract-lambda-list (list) list)
#+clos
(proclamation clos:extract-specializer-names (list) list)

;;;
;;; 8. STRUCTURES
;;;

(proclamation copy-structure (t) t)

;; ECL extensions
(proclamation si:make-structure (t &rest t) structure-object)
(proclamation si:structure-name (structure-object) symbol :reader)
(proclamation si:structure-ref (structure-object t fixnum) t :reader)
(proclamation si:structure-set (structure-object t fixnum t) t)
(proclamation si:structurep (t) gen-bool :predicate)
(proclamation si:structure-subtype-p (t t) gen-bool :predicate)

;;;
;;; 9. CONDITIONS
;;;

(proclamation error (t &rest t) (values))
;; FIXME! It is not clear from the specification whether CERROR actually
;; returns values. However ECL is actually using the fact that it returns
;; the value from CONTINUE.
(proclamation cerror (format-control t &rest t) (values &rest t))
(proclamation invalid-method-error (method format-control &rest t) (values))
(proclamation method-combination-error (format-control &rest t) (values))
(proclamation signal (t &rest t) null)
(proclamation warn (t &rest t) null)
(proclamation invoke-debugger (condition) (values))
(proclamation break (&optional format-control &rest t) null)
(proclamation make-condition (type-specifier &rest t) condition)
(proclamation compute-restarts (&optional (or null condition)) list)
(proclamation find-restart
                   (restart-designator &optional (or null condition))
                   restart)
(proclamation invoke-restart (restart-designator &rest t)
                   (values &rest t))
(proclamation invoke-restart-interactively (restart-designator)
                   (values &rest t))
(proclamation abort (&optional (or null condition)) (values))
(proclamation continue (&optional (or null condition)) null)
(proclamation muffle-warning (&optional (or null condition)) (values))
(proclamation store-value (t &optional (or null condition)) null)
(proclamation use-value (t &optional (or null condition)) null)

;; Slot accessors:
;; (proclamation cell-error-name (cell-error) t)
;; (proclamation simple-condition-format-control (simple-condition) t)
;; (proclamation simple-condition-format-arguments (simple-condition) t)
;; (proclamation restart-name (restart) t)

;; ECL extensions
(proclamation ext:catch-signal (fixnum gen-bool &key) null)
(proclamation si:bind-simple-restarts (t t) list)
(proclamation si:bind-simple-handlers (t t) list)

;;;
;;; 10. SYMBOLS
;;;

(proclamation symbolp (t) gen-bool :pure)
(proclamation keywordp (t) gen-bool :reader)
(proclamation make-symbol (string) symbol)
(proclamation copy-symbol (symbol &optional gen-bool) symbol)
(proclamation gensym (&optional (or string natural)) symbol)
(proclamation gentemp (&optional string package-designator) symbol)
(proclamation symbol-function (symbol)
                   (or function list (member 'special))
                   :reader)
(proclamation symbol-name (symbol) string :pure)
(proclamation symbol-package (symbol) (or package null) :reader)
(proclamation symbol-plist (symbol) list :reader)
(proclamation symbol-value (symbol) t :reader)
(proclamation get (symbol t &optional t) t :no-side-effects)
(proclamation remprop (symbol t) gen-bool)
(proclamation boundp (symbol) gen-bool :reader)
(proclamation makunbound (symbol) symbol)
(proclamation set (symbol t) t)

;; ECL extensions:
(proclamation si:*make-special (symbol) symbol)
(proclamation si:*make-constant (symbol t) symbol)
(proclamation si:put-f (list t t) list)
(proclamation si:rem-f (list t) (values list boolean))
(proclamation si:set-symbol-plist (symbol list) list)
(proclamation si:putprop (symbol t t) t)
(proclamation si:put-sysprop (t t t) t)
(proclamation si:get-sysprop (t t) (values t boolean))
(proclamation si:rem-sysprop (t t) boolean)
(proclamation si:put-properties (symbol &rest t) symbol :no-sp-change)


;;;
;;; 11. PACKAGES
;;;

(proclamation export (list &optional package-designator) t)
(proclamation find-symbol (string &optional package-designator)
              (values symbol (member :inherited :external :internal nil)))
(proclamation find-package (package-designator) (or package null))
(proclamation find-all-symbols (string-designator) list)
(proclamation import (list &optional package-designator) t)
(proclamation list-all-packages () list)
(proclamation rename-package (package-designator package-designator
                              &optional list) package)
(proclamation shadow (list &optional package-designator) t)
(proclamation shadowing-import (list &optional package-designator) t)
(proclamation delete-package (package-designator) gen-bool)
(proclamation make-package (string-designator &rest t) package)
(proclamation unexport (list &optional package-designator) t)
(proclamation unintern (symbol &optional package-designator) gen-bool)
(proclamation unuse-package (list &optional package-designator) t)
(proclamation use-package (list &optional package-designator) t)
(proclamation intern (string &optional package-designator)
              (values symbol (member :inherited :external :internal nil)))
(proclamation package-name (package-designator) (or string null) :reader)
(proclamation package-nicknames (package-designator) list :reader)
(proclamation package-shadowing-symbols (package-designator) list :reader)
(proclamation package-use-list (package-designator) list :reader)
(proclamation package-used-by-list (package-designator) list :reader)
(proclamation packagep (t) gen-bool :pure)

;; Slot accessor:
;; (proclamation package-error-package (condition) package)

;; ECL extensions
(proclamation si:select-package (package-designator) package)
(proclamation si:package-hash-tables (package-designator)
                   (values hash-table hash-table list) :reader)
(proclamation ext:package-lock (package-designator gen-bool) package)

;;;
;;; 12. NUMBERS
;;;

(proclamation = (number &rest number) gen-bool :pure)
(proclamation /= (number &rest number) gen-bool :pure)
(proclamation < (real &rest real) gen-bool :pure)
(proclamation > (real &rest real) gen-bool :pure)
(proclamation <= (real &rest real) gen-bool :pure)
(proclamation >= (real &rest real) gen-bool :pure)
(proclamation max (real &rest real) real :pure)
(proclamation min (real &rest real) real :pure)
(proclamation minusp (real) gen-bool :pure)
(proclamation plusp (real) gen-bool :pure)
(proclamation zerop (number) gen-bool :pure)
(proclamation floor (real &optional real) (values integer real) :pure)
(proclamation ceiling (real &optional real) (values integer real) :pure)
(proclamation truncate (real &optional real) (values integer real) :pure)
(proclamation round (real &optional real) (values integer real) :pure)
(proclamation ffloor (real &optional real) (values float real) :pure)
(proclamation fceiling (real &optional real) (values float real) :pure)
(proclamation ftruncate (real &optional real) (values float real) :pure)
(proclamation fround (real &optional real) (values float real) :pure)
(proclamation cos (number) number :pure)
(proclamation sin (number) number :pure)
(proclamation tan (number) number :pure)
(proclamation cosh (number) number :pure)
(proclamation sinh (number) number :pure)
(proclamation tanh (number) number :pure)
(proclamation acos (number) number :pure)
(proclamation asin (number) number :pure)
(proclamation atan (number &optional real) number :pure)
(proclamation acosh (number) number :pure)
(proclamation asinh (number) number :pure)
(proclamation atanh (number) number :pure)
(proclamation * (&rest number) number :pure)
(proclamation + (&rest number) number :pure)
(proclamation - (&rest number) number :pure)
(proclamation / (number &rest number) number :pure)
(proclamation 1+ (number) number :pure)
(proclamation 1- (number) number :pure)
(proclamation abs (number) (real 0 *) :pure)
(proclamation evenp (integer) gen-bool :pure)
(proclamation oddp (integer) gen-bool :pure)
(proclamation exp (number) number :pure)
(proclamation expt (number number) number :pure)
(proclamation gcd (&rest integer) unsigned-byte :pure)
(proclamation lcm (&rest integer) unsigned-byte :pure)
(proclamation log (number &optional number) number :pure)
(proclamation mod (real real) real :pure)
(proclamation rem (real real) real :pure)
(proclamation signum (number) number :pure)
(proclamation sqrt (number) number :pure)
(proclamation isqrt (natural) natural :pure)
(proclamation make-random-state (&optional (or random-state (member nil t)))
              random-state :no-side-effects)
(proclamation random ((or (integer 0 *) (float 0 *)) &optional random-state)
              (or (integer 0 *) (float 0 *)))
(proclamation random-state-p (t) gen-bool :pure)
(proclamation numberp (t) gen-bool :pure)
(proclamation cis (real) complex :pure)
(proclamation complex (real &optional real) number :pure)
(proclamation complexp (t) gen-bool :pure)
(proclamation conjugate (number) number :pure)
(proclamation phase (number) (real #.(- pi) #.pi) :pure)
(proclamation realpart (number) real :pure)
(proclamation imagpart (number) real :pure)
(proclamation upgraded-complex-part-type
              (type-specifier &optional environment)
              type-specifier
              :no-side-effects)
(proclamation realp (t) gen-bool :pure)
(proclamation numerator (rational) integer :pure)
(proclamation denominator (rational) unsigned-byte :pure)
(proclamation rational (real) rational :pure)
(proclamation rationalize (real) rational :pure)
(proclamation rationalp (t) gen-bool :pure)
(proclamation ash (integer integer) integer :pure)
(proclamation integer-length (integer) integer-length :pure)
(proclamation integerp (t) gen-bool :pure)
(proclamation parse-integer (string &rest t)
              (values integer ext:array-index)
              :no-side-effects)
(proclamation boole ((integer 0 15) integer integer) integer :pure)
(proclamation logand (&rest integer) integer :pure)
(proclamation logandc1 (integer integer) integer :pure)
(proclamation logandc2 (integer integer) integer :pure)
(proclamation logeqv (&rest integer) integer :pure)
(proclamation logior (&rest integer) integer :pure)
(proclamation lognand (integer integer) integer :pure)
(proclamation lognor (integer integer) integer :pure)
(proclamation lognot (integer) integer :pure)
(proclamation logorc1 (integer integer) integer :pure)
(proclamation logorc2 (integer integer) integer :pure)
(proclamation logxor (&rest integer) integer :pure)
(proclamation logbitp (unsigned-byte integer) gen-bool :pure)
(proclamation logcount (integer) integer-length :pure)
(proclamation logtest (integer integer) gen-bool :pure)
(proclamation byte (unsigned-byte unsigned-byte) byte-specifier :pure)
(proclamation byte-size (byte-specifier) integer-length :pure)
(proclamation byte-position (byte-specifier) integer-length :pure)
(proclamation deposit-field (integer byte-specifier integer) integer :pure)
(proclamation dpb (integer byte-specifier integer) integer :pure)
(proclamation ldb (byte-specifier integer) unsigned-byte :pure)
(proclamation ldb-test (byte-specifier integer) gen-bool :pure)
(proclamation mask-field (byte-specifier integer) unsigned-byte :pure)
(proclamation decode-float (float) (values float integer float) :pure)
(proclamation scale-float (float integer) float :pure)
(proclamation float-radix (float) fixnum :pure)
(proclamation float-sign (float &optional float) float :pure)
(proclamation float-digits (float)
              (integer 0 #.(float-digits (coerce 1.0 'long-float)))
              :pure)
(proclamation float-precision (float)
              (integer 0 #.(float-digits (coerce 1.0 'long-float)))
              :pure)
(proclamation integer-decode-float (float)
              (values integer integer (member -1 1))
              :pure)
(proclamation float (number &optional float) float :pure)
(proclamation floatp (t) gen-bool :pure)

;; Slot accessors:
;; (proclamation arithmetic-error-operands (condition) t)
;; (proclamation arithmetic-error-operation (condition) t)

;; ECL extensions
(proclamation si:bit-array-op (t t t t) (array bit))
(proclamation ext:fixnump (t) gen-bool :pure)
(proclamation si:ratiop (t) gen-bool :pure)
(proclamation si:short-float-p (t) gen-bool :pure)
(proclamation si:single-float-p (t) gen-bool :pure)
(proclamation si:double-float-p (t) gen-bool :pure)
(proclamation si:long-float-p (t) gen-bool :pure)

;; Virtual functions added by the compiler
(proclamation shift>> (*) nil :pure)
(proclamation shift<< (*) nil :pure)
(proclamation c::ldb1 (fixnum fixnum fixnum) fixnum :no-side-effects)


;;;
;;; 13. CHARACTERS
;;;

(proclamation char= (character &rest character) gen-bool :pure)
(proclamation char/= (character &rest character) gen-bool :pure)
(proclamation char< (character &rest character) gen-bool :pure)
(proclamation char> (character &rest character) gen-bool :pure)
(proclamation char<= (character &rest character) gen-bool :pure)
(proclamation char>= (character &rest character) gen-bool :pure)
(proclamation char-equal (character &rest character) gen-bool :pure)
(proclamation char-not-equal (character &rest character) gen-bool :pure)
(proclamation char-lessp (character &rest character) gen-bool :pure)
(proclamation char-greaterp (character &rest character) gen-bool :pure)
(proclamation char-not-greaterp (character &rest character) gen-bool :pure)
(proclamation char-not-lessp (character &rest character) gen-bool :pure)
(proclamation character (character-designator) character)
(proclamation characterp (t) gen-bool :pure)
(proclamation alpha-char-p (character) gen-bool :pure)
(proclamation alphanumericp (character) gen-bool :pure)
(proclamation digit-char (digit-weight &optional radix) character :pure)
(proclamation digit-char-p (character &optional radix)
              (or digit-weight null)
              :pure)
(proclamation graphic-char-p (character) gen-bool :pure)
(proclamation standard-char-p (character) gen-bool :pure)
(proclamation char-upcase (character) character :pure)
(proclamation char-downcase (character) character :pure)
(proclamation upper-case-p (character) gen-bool :pure)
(proclamation lower-case-p (character) gen-bool :pure)
(proclamation both-case-p (character) gen-bool :pure)
(proclamation char-code (character) character-code :pure)
(proclamation char-int (character) character-code :pure)
(proclamation code-char (character-code) (or character null) :pure)
(proclamation char-name (character) (or string null) :pure)
(proclamation name-char (string-designator) (or character null) :pure)

;; ECL extensions
(proclamation si:base-char-p (t) gen-bool :predicate)

;;;
;;; 14. CONSES
;;;

(proclamation cons (t t) cons :no-side-effects)
(proclamation consp (t) gen-bool :pure)
(proclamation atom (t) gen-bool :pure)
(proclamation rplaca (cons t) cons)
(proclamation rplacd (cons t) cons)
(proclamation car (list) t :reader)
(proclamation cdr (list) t :reader)
(proclamation caar (list) t :reader)
(proclamation cadr (list) t :reader)
(proclamation cdar (list) t :reader)
(proclamation cddr (list) t :reader)
(proclamation caaar (list) t :reader)
(proclamation caadr (list) t :reader)
(proclamation cadar (list) t :reader)
(proclamation caddr (list) t :reader)
(proclamation cdaar (list) t :reader)
(proclamation cdadr (list) t :reader)
(proclamation cddar (list) t :reader)
(proclamation cdddr (list) t :reader)
(proclamation caaaar (list) t :reader)
(proclamation caaadr (list) t :reader)
(proclamation caadar (list) t :reader)
(proclamation caaddr (list) t :reader)
(proclamation cadaar (list) t :reader)
(proclamation cadadr (list) t :reader)
(proclamation caddar (list) t :reader)
(proclamation cadddr (list) t :reader)
(proclamation cdaaar (list) t :reader)
(proclamation cdaadr (list) t :reader)
(proclamation cdadar (list) t :reader)
(proclamation cdaddr (list) t :reader)
(proclamation cddaar (list) t :reader)
(proclamation cddadr (list) t :reader)
(proclamation cdddar (list) t :reader)
(proclamation cddddr (list) t :reader)
(proclamation copy-tree (tree) tree :no-side-effects)
(proclamation sublis (association-list tree &key) tree)
(proclamation nsublis (association-list tree &key) tree)
(proclamation subst (t t tree &key) tree)
(proclamation subst-if (t function-designator tree &key) tree)
(proclamation subst-if-not (t function-designator tree &key) tree)
(proclamation nsubst (t t tree &key) tree)
(proclamation nsubst-if (t function-designator tree &key) tree)
(proclamation nsubst-if-not (t function-designator tree &key) tree)
(proclamation tree-equal (tree tree &key) gen-bool :predicate)
(proclamation copy-list (list) list :no-side-effects)
(proclamation list (&rest t) list :no-side-effects)
(proclamation list* (&rest t) t :no-side-effects)
(proclamation list-length (list) (or null si::index) :no-side-effects)
(proclamation listp (t) gen-bool :pure)
(proclamation make-list (si::index &key) list :no-side-effects)
(proclamation first (list) t :reader)
(proclamation second (list) t :reader)
(proclamation third (list) t :reader)
(proclamation fourth (list) t :reader)
(proclamation fifth (list) t :reader)
(proclamation sixth (list) t :reader)
(proclamation seventh (list) t :reader)
(proclamation eighth (list) t :reader)
(proclamation ninth (list) t :reader)
(proclamation tenth (list) t :reader)
(proclamation nth (unsigned-byte list) t :reader)
(proclamation endp (list) gen-bool :predicate)
(proclamation null (t) gen-bool :predicate)
(proclamation nconc (&rest t) t)
(proclamation append (&rest t) t :no-side-effects)
(proclamation revappend (list t) t :no-side-effects)
(proclamation nreconc (list t) t)
(proclamation butlast (list &optional unsigned-byte) list :no-side-effects)
(proclamation nbutlast (list &optional unsigned-byte) list)
(proclamation last (list &optional unsigned-byte) t :reader)
(proclamation ldiff (list t) list :no-side-effects)
(proclamation tailp (t list) gen-bool :reader)
(proclamation nthcdr (fixnum list) t :reader)
(proclamation rest (list) t :no-side-effects)
(proclamation member (t proper-list &key) proper-list)
(proclamation member-if (function-designator proper-list &key) proper-list)
(proclamation member-if-not (function-designator proper-list &key)
              proper-list)
(proclamation mapc (function-designator proper-list &rest proper-list)
              proper-list)
(proclamation mapcar (function-designator proper-list &rest proper-list)
              proper-list)
(proclamation mapcan (function-designator proper-list &rest proper-list)
              proper-list)
(proclamation mapl (function-designator proper-list &rest proper-list)
              proper-list)
(proclamation maplist (function-designator proper-list &rest proper-list)
              proper-list)
(proclamation mapcon (function-designator proper-list &rest proper-list)
              proper-list)
(proclamation acons (t t association-list) association-list :no-side-effects)
(proclamation assoc (t association-list &key) t)
(proclamation assoc-if (function-designator association-list &key) t)
(proclamation assoc-if-not (function-designator association-list &key) t)
(proclamation copy-alist (association-list) association-list :no-side-effects)
(proclamation pairlis (proper-list proper-list &optional association-list)
              association-list :no-side-effects)
(proclamation rassoc (t association-list &key) t)
(proclamation rassoc-if (function-designator association-list &key) t)
(proclamation rassoc-if-not (function-designator association-list &key) t)
(proclamation get-properties (property-list proper-list)
              (values t t list) :no-side-effects)
(proclamation getf (property-list t &optional t) t :reader)
(proclamation intersection (proper-list proper-list &key) proper-list)
(proclamation nintersection (proper-list proper-list &key) proper-list)
(proclamation adjoin (t proper-list &key) proper-list)
(proclamation set-difference (proper-list proper-list &key) proper-list)
(proclamation nset-difference (proper-list proper-list &key) proper-list)
(proclamation set-exclusive-or (proper-list proper-list &key) proper-list)
(proclamation nset-exclusive-or (proper-list proper-list &key) proper-list)
(proclamation subsetp (proper-list proper-list &key) gen-bool :predicate)
(proclamation union (proper-list proper-list &key) proper-list)
(proclamation nunion (proper-list proper-list &key) proper-list)

;; ECL extensions
(proclamation si:member1 (t proper-list t t t) list)
(proclamation si:memq (t proper-list) list)
(proclamation si:cons-car (cons) t :reader)
(proclamation si:cons-cdr (cons) t :reader)
(proclamation si::proper-list-p (t) gen-bool :predicate)

;;;
;;; 15. ARRAYS
;;;

(proclamation make-array ((or ext:array-index list) &key)
              array :no-side-effects)
(proclamation adjust-array (array (or ext:array-index list) &key) array)
(proclamation adjustable-array-p (array) gen-bool :pure)
(proclamation aref (array &rest ext:array-index) t :reader)
(proclamation array-dimension (array array-rank-index)
              ext:array-index :reader)
(proclamation array-dimensions (array) list :reader)
(proclamation array-element-type (array) type-specifier :pure)
(proclamation array-has-fill-pointer-p (array) gen-bool :pure)
(proclamation array-displacement (array)
              (values (or array null) ext:array-index)
              :reader)
(proclamation array-in-bounds-p (array &rest ext:array-index) gen-bool
              :no-side-effects)
(proclamation array-rank (array) array-rank-index :reader)
(proclamation array-row-major-index (array &rest ext:array-index)
              ext:array-index :no-side-effects)
(proclamation array-total-size (array) ext:array-index :reader)
(proclamation arrayp (t) gen-bool :pure)
(proclamation fill-pointer (vector) ext:array-index :reader)
(proclamation row-major-aref (array ext:array-index) t :reader)
(proclamation upgraded-array-element-type
              (type-specifier &optional environment)
              type-specifier :no-side-effects)
(proclamation simple-vector-p (t) gen-bool :pure)
(proclamation svref (simple-vector ext:array-index) t :reader)
(proclamation vector (&rest t) vector :no-side-effects)
(proclamation vector-pop (vector) t)
(proclamation vector-push (t vector) (or ext:array-index null))
(proclamation vector-push-extend (t vector &optional ext:array-index)
              ext:array-index)
(proclamation vectorp (t) gen-bool :pure)
(proclamation bit (bit-array &rest ext:array-index) bit :reader)
(proclamation sbit ((simple-array bit) &rest ext:array-index)
              bit :reader)
(proclamation bit-and (bit-array bit-array &optional
                       (or bit-array (member t nil)))
              bit-array :no-side-effects)
(proclamation bit-andc1 (bit-array bit-array &optional
                         (or bit-array (member t nil)))
              bit-array :no-side-effects)
(proclamation bit-andc2 (bit-array bit-array &optional
                         (or bit-array (member t nil)))
              bit-array :no-side-effects)
(proclamation bit-eqv (bit-array bit-array &optional
                       (or bit-array (member t nil)))
              bit-array :no-side-effects)
(proclamation bit-ior (bit-array bit-array &optional
                       (or bit-array (member t nil)))
              bit-array :no-side-effects)
(proclamation bit-nand (bit-array bit-array &optional
                        (or bit-array (member t nil)))
              bit-array :no-side-effects)
(proclamation bit-nor (bit-array bit-array &optional
                            (or bit-array (member t nil)))
              bit-array :no-side-effects)
(proclamation bit-orc1 (bit-array bit-array &optional
                        (or bit-array (member t nil)))
              bit-array :no-side-effects)
(proclamation bit-orc2 (bit-array bit-array &optional
                        (or bit-array (member t nil)))
              bit-array :no-side-effects)
(proclamation bit-xor (bit-array bit-array &optional
                       (or bit-array (member t nil)))
              bit-array :no-side-effects)
(proclamation bit-not (bit-array &optional (or bit-array (member t nil)))
              bit-array :no-side-effects)
(proclamation bit-vector-p (t) gen-bool :pure)
(proclamation simple-bit-vector-p (t) t :pure)

;; ECL extensions
(proclamation si:make-pure-array (t t t t t t) array)
(proclamation si:make-vector (t t t t t t) vector)
(proclamation si:aset (array t &rest t) t)
(proclamation si:row-major-aset (array ext:array-index t) t)
(proclamation si:svset (simple-vector ext:array-index t) t)
(proclamation si:fill-pointer-set (vector ext:array-index) ext:array-index)
(proclamation si:replace-array (array array) array)

;;;
;;; 16. STRINGS
;;;

(proclamation simple-string-p (t) gen-bool :pure)
(proclamation char (string ext:array-index) character :reader)
(proclamation schar (simple-string ext:array-index) character :reader)
(proclamation string (string-designator) string :no-side-effects)
(proclamation string-upcase (string-designator &key)
              string :no-side-effects)
(proclamation string-downcase (string-designator &key)
              string :no-side-effects)
(proclamation string-capitalize (string-designator &key)
              string :no-side-effects)
(proclamation nstring-upcase (string &key) string)
(proclamation nstring-downcase (string &key) string)
(proclamation nstring-capitalize (string &key) string)
(proclamation string-trim (sequence string-designator)
              string :no-side-effects)
(proclamation string-left-trim (sequence string-designator)
              string :no-side-effects)
(proclamation string-right-trim (sequence string-designator)
              string :no-side-effects)
(proclamation string= (string-designator string-designator &key)
              gen-bool :no-side-effects)
(proclamation string/= (string-designator string-designator &key)
              (or ext:array-index null) :no-side-effects)
(proclamation string< (string-designator string-designator &key)
              (or ext:array-index null) :no-side-effects)
(proclamation string> (string-designator string-designator &key)
              (or ext:array-index null) :no-side-effects)
(proclamation string<= (string-designator string-designator &key)
              (or ext:array-index null) :no-side-effects)
(proclamation string>= (string-designator string-designator &key)
              (or ext:array-index null) :no-side-effects)
(proclamation string-equal (string-designator string-designator &key)
              gen-bool :no-side-effects)
(proclamation string-not-equal (string-designator string-designator &key)
              (or ext:array-index null) :no-side-effects)
(proclamation string-lessp (string-designator string-designator &key)
              (or ext:array-index null) :no-side-effects)
(proclamation string-greaterp (string-designator string-designator &key)
              (or ext:array-index null) :no-side-effects)
(proclamation string-not-lessp (string-designator string-designator &key)
              (or ext:array-index null) :no-side-effects)
(proclamation string-not-greaterp (string-designator string-designator &key)
              (or ext:array-index null) :no-side-effects)
(proclamation stringp (t) gen-bool :predicate)
(proclamation make-string (ext:array-index &key) string :no-side-effects)

;; ECL extensions:
(proclamation si:base-string-p (t) gen-bool :pure)
(proclamation si:char-set (string ext:array-index character) character)
(proclamation si:schar-set (string ext:array-index character) character)
(proclamation si:base-string-concatenate (&rest base-string) base-string)

;;;
;;; 17. SEQUENCES
;;;

(proclamation copy-seq (sequence) sequence :no-side-effects)
(proclamation elt (sequence sequence-index) t :no-side-effects)
(proclamation fill (sequence t &key) sequence)
(proclamation make-sequence (type-specifier sequence-index &key)
              sequence :no-side-effects)
(proclamation subseq (sequence sequence-index &optional (or sequence-index null))
              sequence :no-side-effects)
(proclamation map (type-specifier function-designator sequence &rest sequence)
              sequence)
(proclamation map-into (sequence function-designator &rest sequence)
              sequence)
(proclamation reduce (function-designator sequence &key) t)
(proclamation count (t sequence &key) sequence-index)
(proclamation count-if (function-designator sequence &key) sequence-index)
(proclamation count-if-not (function-designator sequence &key) sequence-index)
(proclamation length (sequence) sequence-index :no-side-effects)
(proclamation reverse (sequence) sequence :no-side-effects)
(proclamation nreverse (sequence) sequence)
(proclamation sort (sequence function-designator &key) sequence)
(proclamation stable-sort (sequence function-designator &key) sequence)
(proclamation find (t sequence &key) t :no-side-effects)
(proclamation find-if (function-designator sequence &key) t)
(proclamation find-if-not (function-designator sequence &key) t)
(proclamation position (t sequence &key) (or null sequence-index))
(proclamation position-if (function-designator sequence &key)
              (or null sequence-index))
(proclamation position-if-not (function-designator sequence &key)
              (or null sequence-index))
(proclamation search (sequence sequence &key)
              (or null sequence-index) :no-side-effects)
(proclamation mismatch (sequence sequence &key)
              (or null sequence-index) :no-side-effects)
(proclamation replace (sequence sequence &key) sequence)
(proclamation substitute (t t sequence &key) sequence)
(proclamation substitute-if (t function-designator sequence &key) sequence)
(proclamation substitute-if-not (t function-designator sequence &key) sequence)
(proclamation nsubstitute (t t sequence &key) sequence)
(proclamation nsubstitute-if (t function-designator sequence &key) sequence)
(proclamation nsubstitute-if-not (t function-designator sequence &key) sequence)
(proclamation concatenate (type-specifier &rest sequence) sequence
                   :no-side-effects)
(proclamation merge (type-specifier sequence sequence function-designator &key)
                   sequence)
(proclamation remove (t sequence &key) sequence)
(proclamation remove-if (function-designator sequence &key) sequence)
(proclamation remove-if-not (function-designator sequence &key) sequence)
(proclamation delete (t sequence &key) sequence)
(proclamation delete-if (function-designator sequence &key) sequence)
(proclamation delete-if-not (function-designator sequence &key) sequence)
(proclamation remove-duplicates (sequence &key) sequence)
(proclamation delete-duplicates (sequence &key) sequence)

;; ECL extensions:
(proclamation si:elt-set (sequence sequence-index t) t)
(proclamation si::make-seq-iterator (sequence &optional sequence-index)
              t :no-side-effects)
(proclamation si::seq-iterator-ref (sequence t) t :reader)
(proclamation si::seq-iterator-set (sequence t t) t :no-sp-change)
(proclamation si::seq-iterator-next (sequence t) t :reader)
(proclamation si::sequence-start-end
              (t sequence sequence-index (or null sequence-index))
              (values fixnum fixnum fixnum) :no-side-effects)
(proclamation si::sequence-count ((or null integer)) fixnum :no-side-effects)

;;;
;;; 18. HASH TABLES
;;;

(proclamation make-hash-table (&key) hash-table :no-side-effects)
(proclamation hash-table-p (t) gen-bool :pure)
(proclamation hash-table-count (hash-table) ext:array-index :reader)
(proclamation hash-table-rehash-size (hash-table)
              (or (integer 1 *) (float (1.0) *))
              :pure)
(proclamation hash-table-rehash-threshold (hash-table) (real 0 1) :pure)
(proclamation hash-table-size (hash-table) ext:array-index :reader)
(proclamation hash-table-test (hash-table) function-designator :pure)
(proclamation gethash (t hash-table &optional t) (values t gen-bool) :reader)
(proclamation remhash (t hash-table) gen-bool)
(proclamation maphash (function-designator hash-table) null)
(proclamation clrhash (hash-table) hash-table)
(proclamation sxhash (t) (integer 0 #.most-positive-fixnum) :no-side-effects)

;; ECL extensions
(proclamation si:hash-set (t hash-table t) t)

;;;
;;; 19. FILENAMES
;;;
;;; Note that the pathname interrogation functions could be pure, were it
;;; not for the fact that they depend on the environment, as they accept
;;; pathname designators, which include strings that should be parsed in
;;; that environment.

(proclamation pathname (pathname-designator) pathname :no-side-effects)
(proclamation make-pathname (&key) pathname :no-side-effects)
(proclamation pathnamep (t) gen-bool :pure)
(proclamation pathname-host (pathname-designator) pathname-host :no-side-effects)
(proclamation pathname-device (pathname-designator) pathname-device :no-side-effects)
(proclamation pathname-directory (pathname-designator) pathname-directory :no-side-effects)
(proclamation pathname-name (pathname-designator) pathname-name :no-side-effects)
(proclamation pathname-type (pathname-designator) pathname-type :no-side-effects)
(proclamation pathname-version (pathname-designator) pathname-version :no-side-effects)
(proclamation load-logical-pathname-translations (string) gen-bool)
(proclamation logical-pathname-translations (string) list)
(proclamation logical-pathname (pathname-designator) logical-pathname)
(proclamation namestring (pathname-designator) (or string null)
              :no-side-effects)
(proclamation file-namestring (pathname-designator) (or string null)
              :no-side-effects)
(proclamation directory-namestring (pathname-designator) (or string null)
              :no-side-effects)
(proclamation host-namestring (pathname-designator) (or string null)
              :no-side-effects)
(proclamation enough-namestring (pathname-designator &optional pathname-designator)
              (or string null)
              :no-side-effects)
(proclamation parse-namestring (pathname-designator &optional pathname-host
                                pathname-designator &key)
              (values (or pathname null) (or ext:array-index null))
              :no-side-effects)
(proclamation wild-pathname-p (pathname-designator
                               &optional (member :host :device :directory :name
                                                 :type :version nil))
              gen-bool :no-side-effects)
(proclamation pathname-match-p (pathname-designator pathname-designator)
              gen-bool :no-side-effects)
(proclamation translate-logical-pathname (pathname-designator &key) pathname
              :no-side-effects)
(proclamation translate-pathname (pathname-designator pathname-designator
                                                      pathname-designator &key)
              pathname :no-side-effects)
(proclamation merge-pathnames (pathname-designator
                               &optional pathname-designator
                               pathname-version)
              pathname :no-side-effects)

;;;
;;; 20. FILES
;;;

(proclamation directory (pathname-designator &key) list)
(proclamation probe-file (pathname-designator) (or pathname null))
(proclamation ensure-directories-exist (pathname &key)
              (values pathname gen-bool))
(proclamation truename (pathname-designator) pathname)
(proclamation file-author (pathname-designator) (or string null))
(proclamation file-write-date (pathname-designator) (or universal-time null))
(proclamation rename-file (pathname-designator pathname-designator)
              (values pathname pathname pathname))
(proclamation delete-file (pathname-designator) t)

;; Slot accessors:
;; (proclamation file-error-pathname (condition) pathname-designator)

;; ECL extensions
(proclamation ext:file-kind (pathname-designator gen-bool) symbol)
(proclamation ext:chdir (pathname-designator &optional gen-bool) pathname)
(proclamation ext:getcwd (&optional gen-bool) pathname)
(proclamation ext:mkstemp (pathname-designator) (or null pathname))
(proclamation ext:copy-file (pathname-designator pathname-designator) gen-bool)

(proclamation si:mkdir (pathname-designator unsigned-byte) string)
(proclamation si:rmdir (pathname-designator) null)


;;;
;;; 21. STREAMS
;;;

(proclamation input-stream-p (stream) gen-bool :reader)
(proclamation output-stream-p (stream) gen-bool :reader)
(proclamation interactive-stream-p (stream) gen-bool :reader)
(proclamation open-stream-p (stream) gen-bool :reader)
(proclamation stream-element-type (stream) type-specifier :reader)
(proclamation streamp (t) gen-bool :pure)
(proclamation read-byte (stream &optional gen-bool t) t)
(proclamation write-byte (integer stream) integer)
(proclamation peek-char (&optional (or character boolean)
                                   stream-designator
                                   gen-bool
                                   t
                                   gen-bool)
              t)
(proclamation read-char (&optional stream-designator gen-bool t gen-bool) t)
(proclamation read-char-no-hang (&optional stream-designator gen-bool t gen-bool) t)
(proclamation terpri (&optional stream-designator) null)
(proclamation fresh-line (&optional stream-designator) gen-bool)
(proclamation unread-char (character &optional stream-designator) null)
(proclamation write-char (character &optional stream-designator) character)
(proclamation read-line (&optional stream-designator gen-bool t gen-bool)
              (values t gen-bool))
(proclamation write-string (string &optional stream-designator &key) string)
(proclamation write-line (string &optional stream-designator &key) string)
(proclamation read-sequence (sequence stream &key) sequence-index)
(proclamation write-sequence (sequence stream &key) sequence)
(proclamation file-length (stream) (or unsigned-byte null))
(proclamation file-position (stream &optional file-position-designator) t)
(proclamation file-string-length (stream (or string character))
              (or unsigned-byte null))
(proclamation open (pathname-designator &key) (or file-stream null))
(proclamation stream-external-format (stream) external-file-format :reader)
(proclamation close (stream &key) t)
(proclamation listen (&optional stream-designator) gen-bool)
(proclamation clear-input (&optional stream-designator) null)
(proclamation finish-output (&optional stream-designator) null)
(proclamation force-output (&optional stream-designator) null)
(proclamation clear-output (&optional stream-designator) null)
(proclamation y-or-n-p (&optional format-control &rest t) gen-bool)
(proclamation yes-or-no-p (&optional format-control &rest t) gen-bool)
(proclamation make-synonym-stream (symbol) synonym-stream)
(proclamation synonym-stream-symbol (synonym-stream) symbol)
(proclamation broadcast-stream-streams (broadcast-stream) list :pure)
(proclamation make-broadcast-stream (&rest stream) broadcast-stream)
(proclamation make-two-way-stream (stream stream) two-way-stream)
(proclamation two-way-stream-input-stream (two-way-stream) stream :pure)
(proclamation two-way-stream-output-stream (two-way-stream) stream :pure)
(proclamation echo-stream-output-stream (echo-stream) stream :pure)
(proclamation echo-stream-input-stream (echo-stream) stream :pure)
(proclamation make-echo-stream (stream stream) echo-stream)
(proclamation concatenated-stream-streams (concatenated-stream)
              list :reader)
(proclamation make-concatenated-stream (&rest stream)
              concatenated-stream)
(proclamation get-output-stream-string (string-stream) string :reader)
(proclamation make-string-input-stream (string &optional
                                               ext:array-index
                                               (or ext:array-index null))
              string-stream :reader)
(proclamation make-string-output-stream (&key) string-stream :reader)

;; Slot accessors:
;; (proclamation stream-error-stream (condition) stream)

;; ECL extensions:
(proclamation si:make-string-output-stream-from-string (string)
              string-stream :reader)
#+wants-sockets
(proclamation si:open-client-stream (t unsigned-byte) stream)
#+wants-sockets
(proclamation si:open-server-stream (unsigned-byte) stream)
#+wants-sockets
(proclamation si:open-unix-socket-stream (base-string) stream)
#+wants-sockets
(proclamation si:lookup-host-entry (t) (values (or null string) list list))
(proclamation si:copy-stream (stream stream) t)
(proclamation si:make-encoding (t) t)
(proclamation si:load-encoding (t) t)

;;;
;;; 22. PRINT
;;;

(proclamation copy-pprint-dispatch (&optional (or si::pprint-dispatch-table null))
              si::pprint-dispatch-table
              :no-side-effects)
(proclamation pprint-dispatch (t &optional (or si::pprint-dispatch-table null))
              (values function-designator gen-bool))
(proclamation pprint-fill (stream-designator t &optional gen-bool gen-bool)
              null)
(proclamation pprint-linear (stream-designator t &optional gen-bool gen-bool)
              null)
(proclamation pprint-tabular (stream-designator t &optional gen-bool gen-bool
                                                unsigned-byte)
              null)
(proclamation pprint-indent ((member :block :current) real
                             &optional stream-designator)
              null)
(proclamation pprint-newline ((member :linear :fill :miser :mandatory)
                              &optional stream-designator)
              null)
(proclamation pprint-tab ((member :line :section :line-relative :section-relative)
                          unsigned-byte unsigned-byte &optional stream-designator)
              null)
(proclamation set-pprint-dispatch (type-specifier
                                   (or function-designator null)
                                   &optional real si::pprint-dispatch-table)
              null)
(proclamation write (t &key) t)
(proclamation prin1 (t &optional stream-designator) t)
(proclamation princ (t &optional stream-designator) t)
(proclamation print (t &optional stream-designator) t)
(proclamation pprint (t &optional stream-designator) (values))
(proclamation write-to-string (t &key) string)
(proclamation prin1-to-string (t) string)
(proclamation princ-to-string (t) string)
(proclamation format ((or stream-designator string)
                      format-control &rest t)
              (or null string))

;; Slot accessor:
;; (proclamation print-not-readable-object (condition) t)

;;;
;;; 23. READER
;;;

(proclamation copy-readtable (&optional readtable-designator (or readtable null))
              readtable :no-side-effects)
(proclamation make-dispatch-macro-character
              (character &optional gen-bool readtable)
              (member t))
(proclamation read (&optional stream-designator gen-bool t gen-bool) t)
(proclamation read-preserving-whitespace
              (&optional stream-designator gen-bool t gen-bool) t)
(proclamation read-delimited-list (character &optional stream-designator gen-bool)
              list)
(proclamation read-from-string (string &optional gen-bool t &key)
              (values t ext:array-index))
(proclamation readtable-case (readtable)
              (member :upcase :downcase :preserve :invert)
              :reader)
(proclamation readtablep (t) gen-bool :pure)
(proclamation get-dispatch-macro-character
              (character character &optional readtable-designator)
              (or function-designator null)
              :reader)
(proclamation set-dispatch-macro-character
              (character character function-designator
               &optional readtable-designator)
              (member t))
(proclamation get-macro-character
              (character &optional readtable-designator)
              (values (or function-designator null) gen-bool)
              :reader)
(proclamation set-macro-character
              (character function-designator
               &optional gen-bool readtable-designator)
              (member t))
(proclamation set-syntax-from-char
              (character character &optional readtable readtable-designator)
              (member t))

;; ECL extensions:
(proclamation si:string-to-object (string &optional t) t)
(proclamation si:standard-readtable () readtable)

;;;
;;; 24. SYSTEM CONSTRUCTION
;;;

(proclamation compile-file (pathname-designator &key)
              (values (or pathname null) gen-bool gen-bool))
(proclamation compile-file-pathname (pathname-designator &key)
              pathname :no-side-effects)
(proclamation load ((or stream pathname-designator) &key) gen-bool)
(proclamation provide (string-designator) t)
(proclamation require (string-designator &optional list) t)

;; ECL extensions
(proclamation si:clear-compiler-properties (symbol) t)


;;;
;;; 25. ENVIRONMENT
;;;

(proclamation decode-universal-time (universal-time &optional time-zone)
              (values (integer 0 59)
                      (integer 0 59)
                      (integer 0 23)
                      (integer 1 31)
                      (integer 1 12)
                      unsigned-byte
                      (integer 0 6)
                      gen-bool
                      time-zone)
              :pure)
(proclamation encode-universal-time ((integer 0 59)
                                     (integer 0 59)
                                     (integer 0 23)
                                     (integer 1 31)
                                     (integer 1 12)
                                     unsigned-byte
                                     &optional time-zone)
              universal-time
              :pure)
(proclamation get-universal-time () universal-time :no-side-effects)
(proclamation get-decoded-time ()
              (values (integer 0 59)
                      (integer 0 59)
                      (integer 0 23)
                      (integer 1 31)
                      (integer 1 12)
                      unsigned-byte
                      (integer 0 6)
                      gen-bool
                      time-zone)
              :no-side-effects)
(proclamation sleep ((real 0 *)) null)
(proclamation apropos (string-designator &optional (or null package-designator))
              (values))
(proclamation apropos-list
              (string-designator &optional (or null package-designator))
              list)
(proclamation describe (t &optional stream-designator) (values))
(proclamation get-internal-real-time () unsigned-byte :no-side-effects)
(proclamation get-internal-run-time () unsigned-byte :no-side-effects)
(proclamation disassemble ((or function-designator list)) null)
(proclamation room (&optional (member t nil :default)) (values &rest t))
(proclamation ed (&optional (or null pathname string function-name))
              (values &rest t))
(proclamation inspect (t) (values &rest t))
(proclamation dribble (&optional pathname-designator) (values &rest t))
(proclamation lisp-implementation-type () (or string null) :pure)
(proclamation lisp-implementation-version () (or string null) :pure)
(proclamation short-site-name () (or string null) :pure)
(proclamation long-site-name () (or string null) :pure)
(proclamation machine-instance () (or string null) :pure)
(proclamation machine-type () (or string null) :pure)
(proclamation machine-version () (or string null) :pure)
(proclamation software-type () (or string null) :pure)
(proclamation software-version () (or string null) :pure)
(proclamation user-homedir-pathname (&optional pathname-host)
              (or pathname null) :no-side-effects)

;; ECL extensions

(proclamation ext::lisp-implementation-vcs-id () string :pure)
(proclamation si::room-report () (values t t t t t t t t))
(proclamation si::reset-gbc-count () t)
(proclamation ext:gc (&optional gen-bool) t)
(proclamation ext:quit (&optional fixnum) t)
(proclamation ext:argc () sequence-index)
(proclamation ext:argv (unsigned-byte) base-string)
(proclamation ext:getenv (string) (or null string))
(proclamation ext:environ () list)
(proclamation ext:system (string) fixnum)
(proclamation ext:getpid () si::index)
(proclamation ext:make-pipe () (or two-way-stream null))
(proclamation ext:run-program (string list &key)
              (values (or null two-way-stream)
                      (or null integer)
                      ext:external-process))

(proclamation ext:make-weak-pointer (t) ext:weak-pointer :no-side-effects)
(proclamation ext:weak-pointer-value (ext:weak-pointer) t)

(proclamation si:unbound () t :pure)
(proclamation si:traced-old-definition (t) t :no-side-effects)

#+clos
(proclamation si:allocate-raw-instance (t t fixnum) ext:instance)
#+clos
(proclamation clos:safe-instance-ref (t fixnum) t)
#+clos
(proclamation si:instance-ref (t fixnum) t :reader)
#+clos
(proclamation si::instance-sig (standard-object) list :reader)
#+clos
(proclamation si:instance-set (t fixnum t) t)
#+clos
(proclamation si:instance-class (t) class :reader)
#+clos
(proclamation si:instance-class-set (t t) t)
#+clos
(proclamation si:instancep (t) t :pure)
#+clos
(proclamation si:sl-boundp (t) t :reader)
#+clos
(proclamation si:sl-makunbound (t fixnum) t)
#+clos
(proclamation clos:standard-instance-access (standard-object t) t :reader)
#+clos
(proclamation clos:funcallable-standard-instance-access
              (clos:funcallable-standard-object t)
              t :reader)
#+clos
(proclamation associate-methods-to-gfun (generic-function *)
              generic-function)
#+clos
(proclamation clos::need-to-make-load-form-p (t t) gen-bool :pure)

#+clos
(proclamation clos::load-defclass (t t t t) t)

;;;
;;; A. FFI
;;;

(proclamation si:pointer (t) unsigned-byte)
(proclamation si:foreign-data-p (t) gen-bool :pure)

;;;
;;; CDR-5 http://cdr.eurolisp.org/document/5/extra-num-types.html
;;;

(proclamation ext:negative-fixnum-p (t) gen-bool :pure) 
(proclamation ext:non-negative-fixnum-p (t) gen-bool :pure) 
(proclamation ext:non-positive-fixnum-p (t) gen-bool :pure) 
(proclamation ext:positive-fixnum-p (t) gen-bool :pure) 
(proclamation ext:array-index-p (t) gen-bool :pure) 

(proclamation ext:negative-integer-p (t) gen-bool :pure) 
(proclamation ext:non-negative-integer-p (t) gen-bool :pure) 
(proclamation ext:non-positive-integer-p (t) gen-bool :pure) 
(proclamation ext:positive-integer-p (t) gen-bool :pure) 

(proclamation ext:negative-rational-p (t) gen-bool :pure) 
(proclamation ext:non-negative-rational-p (t) gen-bool :pure) 
(proclamation ext:non-positive-rational-p (t) gen-bool :pure) 
(proclamation ext:positive-rational-p (t) gen-bool :pure) 

(proclamation ext:negative-ratio-p (t) gen-bool :pure) 
(proclamation ext:non-negative-ratio-p (t) gen-bool :pure) 
(proclamation ext:non-positive-ratio-p (t) gen-bool :pure) 
(proclamation ext:positive-ratio-p (t) gen-bool :pure) 

(proclamation ext:negative-real-p (t) gen-bool :pure) 
(proclamation ext:non-negative-real-p (t) gen-bool :pure) 
(proclamation ext:non-positive-real-p (t) gen-bool :pure) 
(proclamation ext:positive-real-p (t) gen-bool :pure) 

(proclamation ext:negative-float-p (t) gen-bool :pure) 
(proclamation ext:non-negative-float-p (t) gen-bool :pure) 
(proclamation ext:non-positive-float-p (t) gen-bool :pure) 
(proclamation ext:positive-float-p (t) gen-bool :pure) 

(proclamation ext:negative-short-float-p (t) gen-bool :pure) 
(proclamation ext:non-negative-short-float-p (t) gen-bool :pure) 
(proclamation ext:non-positive-short-float-p (t) gen-bool :pure) 
(proclamation ext:positive-short-float-p (t) gen-bool :pure) 

(proclamation ext:negative-single-float-p (t) gen-bool :pure) 
(proclamation ext:non-negative-single-float-p (t) gen-bool :pure) 
(proclamation ext:non-positive-single-float-p (t) gen-bool :pure) 
(proclamation ext:positive-single-float-p (t) gen-bool :pure) 

(proclamation ext:negative-double-float-p (t) gen-bool :pure) 
(proclamation ext:non-negative-double-float-p (t) gen-bool :pure) 
(proclamation ext:non-positive-double-float-p (t) gen-bool :pure) 
(proclamation ext:positive-double-float-p (t) gen-bool :pure) 

(proclamation ext:negative-long-float-p (t) gen-bool :pure) 
(proclamation ext:non-negative-long-float-p (t) gen-bool :pure) 
(proclamation ext:non-positive-long-float-p (t) gen-bool :pure) 
(proclamation ext:positive-long-float-p (t) gen-bool :pure) 

))) ; eval-when

(loop for i in '#.(mapcar #'rest +proclamations+)
   do (apply #'parse-function-proclamation i))

