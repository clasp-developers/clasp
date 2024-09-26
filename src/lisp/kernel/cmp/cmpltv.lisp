(defpackage #:cmpltv
  (:use #:cl)
  (:export #:with-constants
           #:ensure-constant #:add-constant #:find-constant-index)
  (:export #:instruction #:creator #:vcreator #:effect)
  (:export #:write-bytecode #:encode)
  (:export #:bytecode-compile-stream)
  ;; introspection
  (:export #:load-bytecode-stream #:load-bytecode)
  (:export #:write-fasl #:save-fasl)
  (:export #:concatenate-fasls #:concatenate-fasl-files
           #:link-bytecode-fasl-files))

(in-package #:cmpltv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Debugging
;;;

(defvar *debug-compiler* nil)

(defmacro dbgprint (message &rest args)
  `(when *debug-compiler*
     (let ((*print-level* 2) (*print-length* 1) (*print-circle* t))
       (format *error-output* ,(concatenate 'string "~&; " message "~%")
               ,@args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initial processing level: Reflects only the necessary recursion
;;; structure, not necessarily the eventual underlying representation.
;;; We collect a sequence of specialized "instructions" that, when executed,
;;; will create and initialize the LTV table.

(defclass instruction () ())
;;; An instruction that allocates or otherwise creates an object.
;;; The object may be fully initialized or may require further initialization.
(defclass creator (instruction)
  ((%index :initform nil :initarg :index :accessor index
           :type (integer 0))))
;;; A creator for which a prototype value (which the eventual LTV will be
;;; similar to) is available.
(defclass vcreator (creator)
  ((%prototype :initarg :prototype :reader prototype)))

(defclass vcreator-reference ()
  ((%prototype :reader prototype
               :initarg :prototype)))

(defun pindex (object)
  (etypecase object
    (creator (index object))
    (vcreator-reference (prototype object))))

(defmethod print-object ((object creator) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~d" (index object)))
  object)

(defmethod print-object ((object vcreator) stream)
  (print-unreadable-object (object stream :type t)
    (if (slot-boundp object '%prototype)
        (prin1 (prototype object) stream)
        (write-string "[no prototype]" stream))
    (format stream " ~d" (index object)))
  object)

;;; An instruction that performs some action for effect. This can include
;;; initialization as well as arbitrary side effects (as from make-load-form).
(defclass effect (instruction) ())

;;; TODO: Abbreviate with list/dotted list, but make sure
;;; coalescence is still really possible.
(defclass cons-creator (vcreator) ())

(defclass rplaca-init (effect)
  ((%cons :initarg :cons :reader rplac-cons :type cons-creator)
   (%value :initarg :value :reader rplac-value :type creator)))

(defmethod print-object ((object rplaca-init) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~d ~d"
            (pindex (rplac-cons object)) (pindex (rplac-value object))))
  object)

(defclass rplacd-init (effect)
  ((%cons :initarg :cons :reader rplac-cons :type cons-creator)
   (%value :initarg :value :reader rplac-value :type creator)))

(defmethod print-object ((object rplacd-init) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~d ~d"
            (pindex (rplac-cons object)) (pindex (rplac-value object))))
  object)

;;; dimensions and element-type are encoded with the array since
;;; they shouldn't really need to be coalesced.
(defclass array-creator (vcreator)
  ((%dimensions :initarg :dimensions :reader dimensions)
   (%packing-info :initarg :packing-info :reader packing-info)
   (%uaet-code :initarg :uaet-code :reader uaet-code)))

;; row-major.
(defclass setf-aref (effect)
  ((%array :initarg :array :reader setf-aref-array :type array-creator)
   (%index :initarg :index :reader setf-aref-index :type (integer 0))
   (%value :initarg :value :reader setf-aref-value :type creator)))

(defclass hash-table-creator (vcreator)
  ((%test :initarg :test :reader hash-table-creator-test :type symbol)
   (%count :initarg :count :reader hash-table-creator-count
           :type (integer 0))))

(defclass setf-gethash (effect)
  ((%hash-table :initarg :hash-table :reader setf-gethash-hash-table
                :type hash-table-creator)
   (%key :initarg :key :reader setf-gethash-key :type creator)
   (%value :initarg :value :reader setf-gethash-value :type creator)))

(defclass symbol-creator (vcreator)
  (;; Is there actually a point to trying to coalesce symbol names?
   (%name :initarg :name :reader symbol-creator-name :type creator)))

(defclass interned-symbol-creator (symbol-creator)
  ((%package :initarg :package :reader symbol-creator-package :type creator)))

(defclass package-creator (vcreator)
  (;; Is there actually a point to trying to coalesce package names?
   ;; Also, some symbols (CL, KEYWORD) could probably be dumped without
   ;; a general package reference.
   (%name :initarg :name :reader package-creator-name :type creator)))

(defclass number-creator (vcreator) ())
(defclass sb64-creator (number-creator) ())
(defclass bignum-creator (number-creator) ())
(defclass ratio-creator (number-creator)
  ((%numerator :initarg :numerator :reader ratio-creator-numerator
               :type creator)
   (%denominator :initarg :denominator :reader ratio-creator-denominator
                 :type creator)))
(defclass complex-creator (number-creator)
  ((%realpart :initarg :realpart :reader complex-creator-realpart
              :type creator)
   (%imagpart :initarg :imagpart :reader complex-creator-imagpart
              :type creator)))
#+short-float
(defclass short-float-creator (number-creator) ())
(defclass single-float-creator (number-creator) ())
(defclass double-float-creator (number-creator) ())
#+long-float
(defclass long-float-creator (number-creator) ())

(defclass character-creator (vcreator) ())

;;; FIXME: Trying to coalesce all this stuff might be pointless.
;;; But maybe not - lots of stuff probably shares a type, I guess.
(defclass pathname-creator (vcreator)
  ((%host :initarg :host :reader pathname-creator-host :type creator)
   (%device :initarg :device :reader pathname-creator-device :type creator)
   (%directory :initarg :directory :reader pathname-creator-directory
               :type creator)
   (%name :initarg :name :reader pathname-creator-name :type creator)
   (%type :initarg :type :reader pathname-creator-type :type creator)
   (%version :initarg :version :reader pathname-creator-version :type creator)))

(defclass fdefinition-lookup (creator)
  ((%name :initarg :name :reader name :type creator)))

;;; Look up the "cell" for a function binding - something that the VM's
;;; FDEFINITION instruction can get an actual function out of.
;;; The nature of this cell is implementation-dependent.
;;; In a simple implementation, the "cell" can just be the function name,
;;; and the FDEFINITION instruction just does CL:FDEFINITION.
(defclass fcell-lookup (creator)
  ((%name :initarg :name :reader name :type creator)))

;;; Look up the "cell" for special variable binding. This is used by the
;;; SPECIAL-BIND, SYMBOL-VALUE, and SYMBOL-VALUE-SET VM instructions
;;; as a lookup key for the binding, as well as for establishing new
;;; local bindings.
;;; The nature of this cell is implementation-dependent.
;;; In a simple implementation, the "cell" can just be the symbol itself,
;;; and the SYMBOL-VALUE instruction just does CL:SYMBOL-VALUE, etc.
(defclass vcell-lookup (creator)
  ((%name :initarg :name :reader name :type creator)))

;;; And the "cell" for the loader environment.
(defclass environment-lookup (creator) ())

;;; Get a special or constant variable's value (as by SYMBOL-VALUE).
;;; This is useful for direct dumping of some forms, as well as to allow
;;; usage of non-dumpable DEFCONSTANTs in code (see #1517).
(defclass vdefinition (creator)
  ((%name :initarg :name :reader name :type creator)))

(defclass general-creator (vcreator)
  (;; Reference to a function designator to call to allocate the object,
   ;; e.g. a function made of the first return value from make-load-form.
   ;; The function returns the new value as its primary.
   ;; Other values are ignored.
   ;; FIXME: Maybe should be a definite function, but this would require
   ;; an FDEFINITION instruction.
   (%function :initarg :function :reader general-function
              :type creator)
   ;; List of arguments (creators) to be passed to the function.
   (%arguments :initarg :arguments :reader general-arguments :type list)))

(defclass general-initializer (effect)
  (;; Reference to a function designator to call to initialize the object,
   ;; e.g. a function made of the second return value from make-load-form.
   ;; The function's return values are ignored.
   (%function :initarg :function :reader general-function
              :type creator)
   ;; List of arguments (creators) to be passed to the function.
   (%arguments :initarg :arguments :reader general-arguments :type list)))

;;; Created from certain make-load-form results.
(defclass class-creator (vcreator)
  ((%name :initarg :name :reader class-creator-name)))

(defclass singleton-creator (vcreator) ())

(defclass load-time-value-creator (creator)
  (;; Reference to a function to call to evaluate the load form.
   ;; It's called with no arguments and returns the value.
   (%function :initarg :function :reader load-time-value-creator-function
              :type creator)
   ;; Boolean indicating whether the LTV is read-only. Unused for now.
   (%read-only-p :initarg :read-only-p :type boolean
                 :reader load-time-value-creator-read-only-p)
   ;; The original form, for debugging/display
   (%form :initarg :form :reader load-time-value-creator-form)
   ;; The info object, for similarity checking
   (%info :initarg :info :reader load-time-value-creator-info)))

(defclass init-object-array (instruction)
  ((%count :initarg :count :reader init-object-array-count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Attributes are bonus, possibly implementation-defined stuff also in the file.
;;; Based closely on Java attributes, the loader has to ignore any it doesn't
;;; understand, so it's verboten for attributes to do anything semantically
;;; important in general. And, attributes include inline information about their
;;; size, so they can be skipped if not understood.
;;; Unlike Java attributes, our attributes are instructions in the normal
;;; sequence. This is so that, for example, functions can be annotated with
;;; source or other debug information before they are called.

(defclass attribute (effect)
  (;; Creator for the name of the attribute, a string.
   ;; FIXME: Do this more cleanly.
   (%name :reader name :initarg :name :type creator)))

;;; Used by disltv so that FASLs with unknown attributes can round-trip
;;; without losing any info.
(defclass unknown-attr (attribute)
  ((%bytes :initarg :bytes :reader bytes
           :type (simple-array (unsigned-byte 8) (*)))))

(defclass name-attr (attribute)
  ((%name :initform (ensure-constant "name"))
   (%object :initarg :object :reader object :type creator)
   (%objname :initarg :objname :reader objname :type creator)))

(defclass docstring-attr (attribute)
  ((%name :initform (ensure-constant "docstring"))
   (%object :initarg :object :reader object :type creator)
   (%docstring :initarg :docstring :reader docstring :type creator)))

(defclass lambda-list-attr (attribute)
  ((%name :initform (ensure-constant "lambda-list"))
   (%function :initarg :function :reader ll-function :type creator)
   (%lambda-list :initarg :lambda-list :reader lambda-list :type creator)))

(defclass function-native-attr (attribute)
  ((%name :initform (ensure-constant "clasp:function-native"))
   (%function :initarg :function :reader ll-function :type creator)
   ;; Name of the main function (string)
   (%main :initarg :main :reader main :type creator)
   ;; Name of the XEP array (string)
   (%xep :initarg :xep :reader xep :type creator)))

#+clasp
(defclass spi-attr (attribute)
  ((%name :initform (ensure-constant "clasp:source-pos-info"))
   (%function :initarg :function :reader spi-attr-function :type creator)
   (%pathname :initarg :pathname :reader spi-attr-pathname :type creator)
   (%lineno :initarg :lineno :reader lineno :type (unsigned-byte 64))
   (%column :initarg :column :reader column :type (unsigned-byte 64))
   (%filepos :initarg :filepos :reader filepos :type (unsigned-byte 64))))

#+clasp
(defclass module-debug-attr (attribute)
  ((%name :initform (ensure-constant "clasp:module-debug-info"))
   (%module :initarg :module :reader module)
   (%infos :initarg :infos :reader infos :type sequence)))

#+clasp
(defclass module-mutable-ltv-attr (attribute)
  ((%name :initform (ensure-constant "clasp:module-mutable-ltv"))
   (%module :initarg :module :reader module)
   (%indices :initarg :indices :reader indices :type sequence)))

#+clasp
(defclass module-native-attr (attribute)
  ((%name :initform (ensure-constant "clasp:module-native"))
   (%module :initarg :module :reader module :type creator)
   (%code :initarg :code :reader code
          :type (simple-array (unsigned-byte 8) (*)))
   (%literals :initarg :literals :reader module-native-attr-literals
              :type simple-vector)))

#+clasp
(defclass debug-info-function ()
  ((%function :initarg :function :reader di-function :type creator)))

#+clasp
(defclass debug-info ()
  ((%start :initarg :start :reader di-start :type (unsigned-byte 32))
   (%end :initarg :end :reader di-end :type (unsigned-byte 32))))

#+clasp
(defclass debug-info-var ()
  ((%name :initarg :name :reader name :type creator)
   (%index :initarg :frame-index :reader frame-index :type (unsigned-byte 16))
   (%cellp :initarg :cellp :reader cellp :type boolean)
   (%dynamic-extent-p :initarg :dxp :reader dynamic-extent-p :type boolean)
   (%ignore :initarg :ignore :reader di-ignore
            :type (member nil cl:ignore cl:ignorable))
   (%inline :initarg :inline :reader di-inline
            :type (member nil cl:inline cl:notinline))
   ;; other declarations (type, user defined)
   (%decls :initarg :decls :reader decls :type list)))
#+clasp
(defclass debug-info-vars (debug-info)
  ((%vars :initarg :vars :reader vars :type list)))

#+clasp
(defclass debug-info-location (debug-info)
  ((%pathname :initarg :pathname :reader di-pathname :type creator)
   (%lineno :initarg :lineno :reader lineno :type (unsigned-byte 64))
   (%column :initarg :column :reader column :type (unsigned-byte 64))
   (%filepos :initarg :filepos :reader filepos :type (unsigned-byte 64))))

#+clasp
(defclass debug-info-decls (debug-info)
  ((%decls :initarg :decls :reader decls :type creator)))

#+clasp
(defclass debug-info-the (debug-info)
  ((%type :initarg :type :reader di-type :type creator)
   (%receiving :initarg :receiving :reader di-receiving :type (signed-byte 32))))

#+clasp
(defclass debug-ast-if (debug-info)
  ((%receiving :initarg :receiving :reader di-receiving :type (signed-byte 32))))

#+clasp
(defclass debug-ast-tagbody (debug-info)
  ((%tags :initarg :tags :reader di-tags :type list)))

#+clasp
(defclass debug-info-block (debug-info)
  ((%name :initarg :name :reader name :type creator)
   (%receiving :initarg :receiving :reader di-receiving :type (signed-byte 32))))

#+clasp
(defclass debug-info-exit (debug-info)
  ((%receiving :initarg :receiving :reader di-receiving :type (signed-byte 32))))

#+clasp
(defclass debug-info-macroexpansion (debug-info)
  ((%macro-name :initarg :macro-name :reader di-macro-name)))

;;;

;;; Return true iff the value is similar to the existing creator.
(defgeneric similarp (creator value)
  (:method (creator value) (declare (ignore creator value)) nil))
;;;

(defmethod similarp ((creator vcreator) value)
  (eql (prototype creator) value))

(defmethod similarp ((creator load-time-value-creator) ltvi)
  (eql (load-time-value-creator-info creator) ltvi))

;;; EQL hash table from objects to creators.
(defvar *coalesce*)

;;; Another EQL hash table for out-of-band objects that are also "coalesced".
;;; So far this means cfunctions and modules.
;;; This a separate variable because perverse code could use an out-of-band
;;; object in band (e.g. compiling a literal module) and we don't want to
;;; confuse those things.
(defvar *oob-coalesce*)

;;; For function cells. EQUAL since function names can be lists.
(defvar *fcell-coalesce*)
;;; And variable cells.
(defvar *vcell-coalesce*)
;;; Since there's only ever at most one environment cell, it's just
;;; stored directly in this variable rather than a table.
(defvar *environment-coalesce*)

;; Look up a value in the existing instructions.
;; On success returns the creator, otherwise NIL.
;; Could be extended with coalescence relations or made more efficient,
;; for example by multiple tables discriminated by type.
(defun %find-constant (value)
  (values (gethash value *coalesce*))
  #+(or)
  (find-if (lambda (c) (and (typep c 'creator) (similarp c value)))
           sequence))

(defun find-oob (value)
  (values (gethash value *oob-coalesce*)))

(defun find-fcell (name) (values (gethash name *fcell-coalesce*)))
(defun find-vcell (name) (values (gethash name *vcell-coalesce*)))

(defun find-environment () *environment-coalesce*)

;;; List of instructions to be executed by the loader.
;;; In reverse.
(defvar *instructions*)

;;; Stack of objects we are in the middle of computing creation forms for.
;;; This is used to detect circular dependencies.
;;; We only do this for MAKE-LOAD-FORM because we assume our own
;;; computations never recurse inappropriately. If they do, it's a bug,
;;; rather than the user's problem.
(defvar *creating*)

(defmacro with-constants ((&key) &body body)
  `(let ((*instructions* nil) (*creating* nil)
         (*coalesce* (make-hash-table)) (*oob-coalesce* (make-hash-table))
         (*fcell-coalesce* (make-hash-table :test #'equal))
         (*vcell-coalesce* (make-hash-table))
         (*environment-coalesce* nil))
     ,@body))

(defun find-constant (value)
  (%find-constant value #+(or) *instructions*))

(defun find-constant-index (value)
  (let ((creator (%find-constant value *instructions*)))
    (if creator
        (index creator)
        nil)))

(defun add-instruction (instruction)
  (push instruction *instructions*)
  instruction)

(defun add-creator (value instruction)
  (setf (gethash value *coalesce*) instruction)
  (add-instruction instruction))

(defun add-oob (key instruction)
  (setf (gethash key *oob-coalesce*) instruction)
  (add-instruction instruction))

(defun add-fcell (key instruction)
  (setf (gethash key *fcell-coalesce*) instruction)
  (add-instruction instruction))

(defun add-vcell (key instruction)
  (setf (gethash key *vcell-coalesce*) instruction)
  (add-instruction instruction))

(defun add-environment (instruction)
  (setf *environment-coalesce* instruction)
  (add-instruction instruction))

(defgeneric add-constant (value))

(defun ensure-constant (value)
  (let ((creator (or (find-constant value) (add-constant value))))
    creator))

;;; Given a form, get a constant handle to a function that at load time will
;;; have the effect of evaluating the form in a null lexical environment.
(defun add-form (form &optional env)
  ;; PROGN so that (declare ...) expressions for example correctly cause errors.
  (add-function
   (bytecode-cf-compile-lexpr `(lambda () (progn ,form)) env)))

(defmethod add-constant ((value cons))
  ;; We special case proper lists so as to avoid deep stack-blowing
  ;; recursion when faced with long lists, which are quite common.
  ;; (Tens of thousands of entries are required to actually blow the
  ;;  stack, but recursion is less efficient here anyway.)
  ;; TODO: Better actual representation in the FASL is possible, e.g. an
  ;; instruction to make lists directly, instead of tens of thousands of
  ;; cons initializations, but it's slightly complicated to reconcile that
  ;; with the possibility of sublists being coalesced.
  (cond ((core:proper-list-p value)
         ;; First, register each cons in the list.
         (mapl
          (lambda (c)
            (add-creator c (make-instance 'cons-creator :prototype value)))
          value)
         ;; Then go through and initialize them.
         (mapl
          (lambda (c)
            (let ((const (find-constant c)))
              (assert const)
              (add-instruction (make-instance 'rplaca-init
                                 :cons const
                                 :value (ensure-constant (car c))))
              (add-instruction (make-instance 'rplacd-init
                                 :cons const
                                 :value (ensure-constant (cdr c))))))
          value)
         (find-constant value))
        (t
         (let ((cons (add-creator
                      value (make-instance 'cons-creator :prototype value))))
           (add-instruction (make-instance 'rplaca-init
                              :cons cons :value (ensure-constant (car value))))
           (add-instruction (make-instance 'rplacd-init
                              :cons cons :value (ensure-constant (cdr value))))
           cons))))

(defmethod add-constant ((value array))
  (let* ((uaet (array-element-type value))
         (info (array-packing-info value))
         (info-type (first info))
         (uaet-code (find-uaet-code uaet))
         (arr (add-creator
               value
               (make-instance 'array-creator
                 :prototype value :dimensions (array-dimensions value)
                 :packing-info info :uaet-code uaet-code))))
    (when (eq info-type t) ; general - dump setf-arefs for elements.
      ;; (we have to separate initialization here in case the array
      ;;  contains itself. packed arrays can't contain themselves)
      (loop for i below (array-total-size value)
            do (add-instruction
                (make-instance 'setf-aref
                  :array arr :index i
                  :value (ensure-constant (row-major-aref value i))))))
    arr))

(defmethod add-constant ((value hash-table))
  (let ((ht (add-creator
             value
             (make-instance 'hash-table-creator :prototype value
                            :test (hash-table-test value)
                            :count (hash-table-count value)))))
    (maphash (lambda (k v)
               (add-instruction
                (make-instance 'setf-gethash
                  :hash-table ht
                  :key (ensure-constant k) :value (ensure-constant v))))
             value)
    ht))

(defmethod add-constant ((value symbol))
  (add-creator
   value
   (let ((package (symbol-package value)))
     (if package
         (make-instance 'interned-symbol-creator
           :prototype value
           :name (ensure-constant (symbol-name value))
           :package (ensure-constant package))
         (make-instance 'symbol-creator
           :prototype value
           :name (ensure-constant (symbol-name value)))))))

(defmethod add-constant ((value (eql nil)))
  (add-creator value (make-instance 'singleton-creator :prototype value)))
(defmethod add-constant ((value (eql t)))
  (add-creator value (make-instance 'singleton-creator :prototype value)))

(defmethod add-constant ((value package))
  (add-creator value
               (make-instance 'package-creator
                 :prototype value
                 :name (ensure-constant (package-name value)))))

(defmethod add-constant ((value integer))
  (add-creator
   value
   (etypecase value
     ;; TODO? Could have different opcodes for smaller integers.
     ((signed-byte 64) (make-instance 'sb64-creator :prototype value))
     (integer (make-instance 'bignum-creator :prototype value)))))

#+short-float
(defmethod add-constant ((value short-float))
  (add-creator
   value
   (make-instance 'short-float-creator :prototype value)))

(defmethod add-constant ((value single-float))
  (add-creator
   value
   (make-instance 'single-float-creator :prototype value)))

(defmethod add-constant ((value double-float))
  (add-creator
   value
   (make-instance 'double-float-creator :prototype value)))

#+long-float
(defmethod add-constant ((value long-float))
  (add-creator
   value
   (make-instance 'long-float-creator :prototype value)))

(defmethod add-constant ((value ratio))
  ;; In most cases it's probably pointless to try to coalesce the numerator
  ;; and denominator. It would probably be smarter to have a small ratio
  ;; where the number is embedded versus a large ratio where they're indirect.
  (add-creator
   value
   (make-instance 'ratio-creator :prototype value
                  :numerator (ensure-constant (numerator value))
                  :denominator (ensure-constant (denominator value)))))

(defmethod add-constant ((value complex))
  ;; Similar considerations to ratios here.
  (add-creator
   value
   (make-instance 'complex-creator :prototype value
                  :realpart (ensure-constant (realpart value))
                  :imagpart (ensure-constant (imagpart value)))))

(defmethod add-constant ((value character))
  (add-creator value (make-instance 'character-creator :prototype value)))

(defmethod add-constant ((value pathname))
  (add-creator
   value
   (make-instance 'pathname-creator
     :prototype value
     :host (ensure-constant (pathname-host value))
     :device (ensure-constant (pathname-device value))
     :directory (ensure-constant (pathname-directory value))
     :name (ensure-constant (pathname-name value))
     :type (ensure-constant (pathname-type value))
     :version (ensure-constant (pathname-version value)))))

(define-condition circular-dependency (error)
  ((%path :initarg :path :reader path)
   (%value :initarg :value :reader value))
  (:report (lambda (condition stream)
             (format stream "circular dependency detected: ~s~%~t~{~s~^ ~}"
                     (value condition) (path condition)))))

(defconstant +max-call-args+ (ash 1 16))

(defun function-form-p (form)
  (and (consp form) (eq (car form) 'cl:function)
       (consp (cdr form)) (null (cddr form))))

(defun lambda-expression-p (form)
  (and (consp form) (eq (car form) 'cl:lambda)))

;;; Return true iff the proper list FORM represents a call to a global
;;; function with all constant, #', or dumpable arguments (and not too many).
;;; Note that allowing these recursively dumpable forms may result in slightly
;;; subpar outcomes - for example we're not smart enough to turn the (LIST)
;;; arguments that appear in LOAD-DEFCLASS calls into constant NILs.
;;; But I (Bike) believe that's offset by the value of not making the loader
;;; make and run a one-time-use bytecode function.
(defun directly-creatable-form-p (form &optional env)
  (or (constantp form env)
      ;; constantp includes non-symbols-or-lists, so this typecase is exhaustive
      (typecase form
        (symbol
         (not (nth-value 1 (macroexpand-1 form env))))
        (cons
         (or (function-form-p form)
             (lambda-expression-p form)
             (and (symbolp (car form))
                  (not (macro-function (car form)))
                  (not (special-operator-p (car form)))
                  (< (length (rest form)) +max-call-args+)
                  (every (lambda (f)
                           (directly-creatable-form-p f env))
                         (rest form))))))))

;;; Given a form and environment,
;;; add FASL instructions to evaluate the form for its value.
;;; This is used directly in creation forms, and also to
;;; add argument forms for simple creation and initializaiton forms.
;;; To use this function, you must have already checked that the form
;;; is directly creatable with directly-creatable-form-p.
(defun add-direct-creator-form (form env)
  (cond ((constantp form env)
         (ensure-constant (ext:constant-form-value form env)))
        ;; special variable (constants are caught by constantp above)
        ((symbolp form)
         (add-instruction
          (make-instance 'vdefinition :name (ensure-constant form))))
        ;; (find-class 'something)
        ((and (eq (car form) 'cl:find-class)
              (= (length form) 2)
              (constantp (second form) env))
         (add-instruction
          (make-instance 'class-creator
            :name (ensure-constant
                   (ext:constant-form-value (second form) env)))))
        ;; (lambda ...)
        ((lambda-expression-p form)
         (add-function (bytecode-cf-compile-lexpr form env)))
        ((function-form-p form)
         (if (lambda-expression-p (second form))
             (add-function (bytecode-cf-compile-lexpr (second form) env))
             (add-instruction
              (make-instance 'fdefinition-lookup
                :name (ensure-constant (second form))))))
        ;; must be a recursive directly creatable form.
        (t
         (add-instruction
          (make-instance 'general-creator
            :function (add-instruction
                       (make-instance 'fdefinition-lookup
                         :name (ensure-constant (car form))))
            :arguments (mapcar (lambda (f) (add-direct-creator-form f env))
                               (rest form)))))))

;;; Make a possibly-special creator based on an MLF creation form.
(defun add-creation-form-creator (value form &optional env)
  (let ((*creating* (cons value *creating*)))
    (if (directly-creatable-form-p form env)
        (let ((inst (add-direct-creator-form form env)))
          (when (typep inst 'vcreator)
            (reinitialize-instance inst :prototype value))
          inst)
        (add-instruction
         (make-instance 'general-creator
           :prototype value
           :function (add-form form env) :arguments ())))))

;;; Make a possibly-special initializer.
(defun add-initializer-form (form &optional env)
  (cond ((constantp form env) nil) ; do nothing (good for e.g. defun's return)
        ((and (symbolp form) (not (nth-value 1 (macroexpand-1 form env))))
         ;; also do nothing. this comes up for e.g. the *PACKAGE* returned from
         ;; top-level IN-PACKAGE forms.
         nil)
        ((directly-creatable-form-p form env)
         (flet ((direct (f)
                  (add-direct-creator-form f env)))
           (if (eq (car form) 'cl:funcall)
               ;; cut off the funcall - general-initializer does the call itself.
               ;; this commonly arises from e.g. (funcall #'(setf fdefinition ...)
               (add-instruction
                (make-instance 'general-initializer
                  :function (direct (second form))
                  :arguments (mapcar #'direct (cddr form))))
               (add-instruction
                (make-instance 'general-initializer
                  :function (add-instruction
                             (make-instance 'fdefinition-lookup
                               :name (ensure-constant (car form))))
                  :arguments (mapcar #'direct (rest form)))))))
        (t ; give up
         (add-instruction
          (make-instance 'general-initializer
            :function (add-form form env) :arguments ())))))

(defvar *initializer-destination* nil)

(defvar *initializer-map* nil)

(defmethod add-constant ((value t))
  (cond ((not (member value *creating*))
         (multiple-value-bind (create initialize)
             (make-load-form value)
           (let ((*initializer-map* (or *initializer-map* (make-hash-table))))
             (prog1
                 ;; We do this instead of ADD-CREATOR because
                 ;; ADD-CREATION-FORM-CREATOR has already added the instructions.
                 (setf (gethash value *coalesce*)
                       (add-creation-form-creator value create))
               ;; WARNING: If object initializations ever need instructions
               ;; moved besides GENERAL-INITIALIZER, they have to be part of
               ;; these TYPEPs.
               (setf *instructions* (nconc (remove-if (lambda (x)
                                                        (typep x '(or setf-literals attribute general-initializer)))
                                                      (gethash value *initializer-map*))
                                           *instructions*))
               (let* ((*initializer-destination* value)
                      (instructions (let (*instructions*)
                                      (add-initializer-form initialize)
                                      *instructions*)))
                 (if (eql *initializer-destination* value)
                     (setf *instructions* (nconc instructions *instructions*))
                     (setf (gethash *initializer-destination* *initializer-map*)
                           (nconc (gethash *initializer-destination* *initializer-map*)
                                  instructions))))
               (setf *instructions* (nconc (remove-if (lambda (x)
                                                        (not (typep x '(or setf-literals attribute general-initializer))))
                                                      (gethash value *initializer-map*))
                                           *instructions*))))))
        (*initializer-destination*
         (let ((p (position *initializer-destination* *creating*)))
           (when (or (null p)
                     (> (position value *creating*) p))
             (setf *initializer-destination* value)))
         (make-instance 'vcreator-reference :prototype value))
        (t
         (error 'circular-dependency :value value :path *creating*))))

;;; Loop over the instructions, assigning indices to the creators.
;;; This only affects their position in the similar vector, not the order
;;; the instructions must be executed in.
;;; The instructions must be in forward order, i.e. reversed from how they're
;;; pushed in above. (FIXME: The reversal is too awkward.)
;;; This could probably be done in one pass somehow?
(defun assign-indices (instructions)
  (let ((next-index 0))
    (map nil (lambda (inst)
               (cond ((and (typep inst 'creator) (not (index inst)))
                      (setf (index inst) next-index)
                      (incf next-index))
                     ((typep inst 'init-object-array) (setf next-index 0))))
         instructions))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Bytecode
;;;
;;; "bytecode" is actually a little strong. This "bytecode" consists of a
;;; sequence of "instructions" that must be executed sequentially.
;;; There's no other control flow. There is no data structure involved other
;;; than the array of constants being produced (so e.g. no operand stack).
;;; All multibyte values are big-endian. All indices are one byte, or two
;;; bytes, or etc. powers of two based on how many constants there are. E.g. if
;;; there are 200 constants indices will be one byte, but if there are 300
;;; indices will be two bytes.

;;; STREAM is a ub8 stream.
(defgeneric encode (instruction stream))

;; how many bytes are needed to represent an index?
(defvar *index-bytes*)

;;; Write an n-byte integer to a ub8 stream, big-endian.
(defun write-b (int n stream)
  ;; write-sequence is better for this, but I don't think we can really
  ;; use it without consing or touching memory generally.
  (loop for i from (* (1- n) 8) downto 0 by 8
        for byte = (ldb (byte 8 i) int)
        do (write-byte byte stream)))

(defun write-b128 (word stream) (write-b word 16 stream))
(defun write-b80  (word stream) (write-b word 10 stream))
(defun write-b64  (word stream) (write-b word 8 stream))
(defun write-b32  (word stream) (write-b word 4 stream))
(defun write-b16  (word stream) (write-b word 2 stream))

(defconstant +magic+ #x8d7498b1) ; randomly chosen bytes.

(defun write-magic (stream) (write-b32 +magic+ stream))

(defparameter *major-version* 0)
(defparameter *minor-version* 15)

(defun write-version (stream)
  (write-b16 *major-version* stream)
  (write-b16 *minor-version* stream))

;; Used in disltv as well.
(defun write-bytecode (instructions stream)
  (let* ((*index-bytes* 1) ; dummy; set by init-object-array instructions
         (ninsts (length instructions)))
    (assign-indices instructions)
    (dbgprint "Instructions:~{~&~a~}" instructions)
    (write-magic stream)
    (write-version stream)
    (write-b64 ninsts stream)
    (map nil (lambda (inst)
               (when (typep inst 'init-object-array)
                 ;; Next highest power of two bytes, roughly
                 (setf *index-bytes*
                       (ash 1 (1- (ceiling (integer-length (init-object-array-count inst)) 8)))))
               (encode inst stream))
         instructions)))

(defun %write-bytecode (stream)
  ;; lol efficiency with the reverse
  (let ((nobjs (count-if (lambda (i) (typep i 'creator)) *instructions*)))
    (write-bytecode (cons (make-instance 'init-object-array :count nobjs)
                          (reverse *instructions*))
                    stream)))

(defun opcode (mnemonic)
  (let ((inst (assoc mnemonic cmpref:+bytecode-ltv-ops+ :test #'equal)))
    (if inst
        (second inst)
        (error "unknown mnemonic ~a" mnemonic))))

(defun write-mnemonic (mnemonic stream) (write-byte (opcode mnemonic) stream))

(defun write-index (creator stream)
  (when (typep creator 'vcreator-reference)
    (setf creator (find-if (lambda (instruction)
                             (and (typep instruction 'vcreator)
                                  (eql (prototype instruction) (prototype creator))))
                           *instructions*)))
  (let ((position (index creator)))
    (ecase *index-bytes*
      ((1) (write-byte position stream))
      ((2) (write-b16 position stream))
      ((4) (write-b32 position stream))
      ((8) (write-b64 position stream)))))

(defmethod encode ((inst cons-creator) stream)
  (write-mnemonic :cons stream))

(defmethod encode ((inst rplaca-init) stream)
  (write-mnemonic :rplaca stream)
  (write-index (rplac-cons inst) stream)
  (write-index (rplac-value inst) stream))

(defmethod encode ((inst rplacd-init) stream)
  (write-mnemonic :rplacd stream)
  (write-index (rplac-cons inst) stream)
  (write-index (rplac-value inst) stream))

(defun write-dimensions (dimensions stream)
  (let ((rank (length dimensions)))
    (unless (< rank 256)
      (error "Can't dump an array of ~d dimensions" rank))
    (write-byte rank stream))
  ;; Only two bytes for now. Might want different opcodes for larger
  ;; (or smaller?) dimensions.
  (unless (< (reduce #'* dimensions) 65536)
    (error "Can't dump an array with ~d elements" (reduce #'* dimensions)))
  (dolist (dim dimensions)
    (write-b16 dim stream)))

(defmacro write-sub-byte (array stream nbits)
  (let ((perbyte (floor 8 nbits))
        (a (gensym "ARRAY")) (s (gensym "STREAM")))
    `(let* ((,a ,array) (,s ,stream) (total-size (array-total-size ,a)))
       (multiple-value-bind (full-bytes remainder) (floor total-size ,perbyte)
         (loop for byteindex below full-bytes
               for index = (* ,perbyte byteindex)
               for byte = (logior
                           ,@(loop for i below perbyte
                                   for shift = (- 8 (* i nbits) nbits)
                                   for rma = `(row-major-aref ,a (+ index ,i))
                                   collect `(ash ,rma ,shift)))
               do (write-byte byte ,s))
         ;; write remainder
         (unless (zerop remainder)
           (let* ((index (* ,perbyte full-bytes))
                  (byte 0))
             (loop for i below remainder
                   for shift = (- 8 (* i ,nbits) ,nbits)
                   for rma = (row-major-aref ,a (+ index i))
                   do (setf (ldb (byte ,nbits shift) byte) rma))
             (write-byte byte ,s)))))))

(defun write-utf8-codepoint (cpoint stream)
  (cond ((< cpoint #x80) ; one byte
	 (write-byte cpoint stream))
	((< cpoint #x800) ; two
	 (write-byte (logior #b11000000 (ldb (byte 5  6) cpoint)) stream)
	 (write-byte (logior #b10000000 (ldb (byte 6  0) cpoint)) stream))
	((< cpoint #x10000) ; three
	 (write-byte (logior #b11100000 (ldb (byte 4 12) cpoint)) stream)
	 (write-byte (logior #b10000000 (ldb (byte 6  6) cpoint)) stream)
	 (write-byte (logior #b10000000 (ldb (byte 6  0) cpoint)) stream))
	((< cpoint #x110000) ; four
	 (write-byte (logior #b11110000 (ldb (byte 3 18) cpoint)) stream)
	 (write-byte (logior #b10000000 (ldb (byte 6 12) cpoint)) stream)
	 (write-byte (logior #b10000000 (ldb (byte 6  6) cpoint)) stream)
	 (write-byte (logior #b10000000 (ldb (byte 6  0) cpoint)) stream))
	(t ; not allowed by RFC3629
	 (error "Code point #x~x is out of range for UTF-8" cpoint))))

(defmethod encode ((inst array-creator) stream)
  (write-mnemonic :make-array stream)
  (write-byte (uaet-code inst) stream)
  (let* ((packing-info (packing-info inst))
         (dims (dimensions inst))
         (packing-type (first packing-info))
         (packing-code (getf cmpref:+uaet-codes+ (second packing-info))))
    (write-byte packing-code stream)
    (write-dimensions dims stream)
    (macrolet ((dump (&rest forms)
                 `(loop with arr = (prototype inst)
                        for i below (array-total-size arr)
                        for elem = (row-major-aref arr i)
                        do ,@forms)))
      (cond ((equal packing-type 'nil)) ; just need dims
            ((equal packing-type 'base-char)
             (dump (write-byte (char-code elem) stream)))
            ((equal packing-type 'character)
             (dump (write-utf8-codepoint (char-code elem) stream)))
            #+short-float
            ((equal packing-type 'short-float)
             (dump (write-b16 (ext:short-float-to-bits elem) stream)))
            ((equal packing-type 'single-float)
             (dump (write-b32 (ext:single-float-to-bits elem) stream)))
            ((equal packing-type 'double-float)
             (dump (write-b64 (ext:double-float-to-bits elem) stream)))
            #+long-float/binary80
            ((equal packing-type 'long-float)
             (dump (write-b80 (ext:long-float-to-bits elem) stream)))
            #+long-float/binary128
            ((equal packing-type 'long-float)
             (dump (write-b128 (ext:long-float-to-bits elem) stream)))
            ((equal packing-type '(complex single-float))
             (dump (write-b32 (ext:single-float-to-bits (realpart elem))
                              stream)
                   (write-b32 (ext:single-float-to-bits (imagpart elem))
                              stream)))
            ((equal packing-type '(complex double-float))
             (dump (write-b64 (ext:double-float-to-bits (realpart elem))
                              stream)
                   (write-b64 (ext:double-float-to-bits (imagpart elem))
                              stream)))
            ((equal packing-type 'bit)
             (write-sub-byte (prototype inst) stream 1))
            ((equal packing-type '(unsigned-byte 2))
             (write-sub-byte (prototype inst) stream 2))
            ((equal packing-type '(unsigned-byte 4))
             (write-sub-byte (prototype inst) stream 4))
            ((equal packing-type '(unsigned-byte 8))
             ;; can't use write-sequence in general since
             ;; the array may be multidimensional.
             (dump (write-byte elem stream)))
            ((equal packing-type '(unsigned-byte 16))
             (dump (write-b16 elem stream)))
            ((equal packing-type '(unsigned-byte 32))
             (dump (write-b32 elem stream)))
            ((equal packing-type '(unsigned-byte 64))
             (dump (write-b64 elem stream)))
            ((equal packing-type '(signed-byte 8))
             (dump (write-byte (ldb (byte 8 0) elem) stream)))
            ((equal packing-type '(signed-byte 16))
             (dump (write-b16 elem stream)))
            ((equal packing-type '(signed-byte 32))
             (dump (write-b32 elem stream)))
            ((equal packing-type '(signed-byte 64))
             (dump (write-b64 elem stream)))
            ;; TODO: Signed bytes
            ((equal packing-type 't)) ; handled by setf-aref instructions
            (t (error "BUG: Unknown packing-type ~s" packing-type))))))

(defmethod encode ((inst setf-aref) stream)
  (write-mnemonic :setf-row-major-aref stream)
  (write-index (setf-aref-array inst) stream)
  (write-b16 (setf-aref-index inst) stream)
  (write-index (setf-aref-value inst) stream))

;;; Arrays are encoded with two codes: One for the packing, and one
;;; for the element type. The latter is in place so that, hopefully,
;;; arrays can be dumped portably. These two codes do not necessarily
;;; coincide: for example a general (T) array full of ub8s could be
;;; encoded as ub8s but still be loaded as a general array.
;;; (This is not done right now.)
;;; FIXME: Not sure how to deal with nonportable element types, such
;;; as clasp's vec3 arrays, or sbcl's ub7 etc. For now the similarity
;;; of arrays is weaker than the language standard mandates.
;;; The portability concern is that, for example, Clasp will have
;;; array element type of ext:byte8 instead of (unsigned-byte 8). In
;;; that case we want to dump as (unsigned-byte 8) and Clasp's loader
;;; will upgrade to ext:byte8 no problem.
;;; TODO: For version 1, put more thought into these IDs.
(defvar +array-packing-infos+
  '((nil                    :nil)
    (base-char              :base-char)
    (character              :character)
    (short-float            #+short-float :binary16
                            #-short-float :binary32)
    (single-float           :binary32)
    (double-float           :binary64)
    (long-float             #+long-float/binary80  :binary80
                            #+long-float/binary128 :binary128
                            #-long-float           :binary64)
    ((complex short-float)  #+short-float :complex-binary16
                            #-short-float :complex-binary32)
    ((complex single-float) :complex-binary32)
    ((complex double-float) :complex-binary64)
    ((complex long-float)   #+long-float/binary80  :complex-binary80
                            #+long-float/binary128 :complex-binary128
                            #-long-float           :complex-binary64)
    (bit                    :unsigned-byte1)
    ((unsigned-byte 2)      :unsigned-byte2)
    ((unsigned-byte 4)      :unsigned-byte4)
    ((unsigned-byte 8)      :unsigned-byte8)
    ((unsigned-byte 16)     :unsigned-byte16)
    ((unsigned-byte 32)     :unsigned-byte32)
    ((unsigned-byte 64)     :unsigned-byte64)
    ((unsigned-byte 128)    :unsigned-byte128)
    ((signed-byte 8)        :signed-byte8)
    ((signed-byte 16)       :signed-byte16)
    ((signed-byte 32)       :signed-byte32)
    ((signed-byte 64)       :signed-byte64)
    ((signed-byte 128)      :signed-byte128)
    (t                      :t)))

(defun %uaet-info (uaet)
  (dolist (info +array-packing-infos+)
    (when (subtypep uaet (first info))
      (return-from %uaet-info info)))
  ;; subtypep not doing so well. default to general.
  (assoc t +array-packing-infos+))

(defun find-uaet-code (uaet)
  (getf cmpref:+uaet-codes+ (second (%uaet-info uaet))))

(defun array-packing-info (array)
  ;; TODO? As mentioned above, we could pack arrays more efficiently
  ;; than suggested by their element type. Iterating over every array
  ;; checking might be a little too slow though?
  ;; Also wouldn't work for NIL arrays, but who's dumping NIL arrays?
  (%uaet-info (array-element-type array)))

(defmethod encode ((inst hash-table-creator) stream)
  (let* (;; TODO: Custom hash-table tests.
         ;; NOTE that for non-custom hash table tests, the standard
         ;; guarantees that hash-table-test returns a symbol.
         (testcode (ecase (hash-table-creator-test inst)
                     ((eq) #b00)
                     ((eql) #b01)
                     ((equal) #b10)
                     ((equalp) #b11)))
         ;; For now, only allow counts up to #xffff.
         ;; Since the count is just a hint, bigger hash tables can still
         ;; be dumped okay.
         ;; efficiency NOTE: The size passed to make-hash-table really
         ;; specifies a capacity, so for example if we have an HT with 56
         ;; entries, make a 56-entry similar hash table, and start filling it
         ;; up, it might be rehashed and resized during initialization as it
         ;; reaches the rehash threshold. I am not sure how to deal with this
         ;; in a portable fashion. (we could just invert a provided rehash-size?)
         (count (min (hash-table-creator-count inst) #xffff)))
    (write-mnemonic :make-hash-table stream)
    (write-byte testcode stream)
    (write-b16 count stream)))

(defmethod encode ((inst setf-gethash) stream)
  (write-mnemonic :setf-gethash stream)
  (write-index (setf-gethash-hash-table inst) stream)
  (write-index (setf-gethash-key inst) stream)
  (write-index (setf-gethash-value inst) stream))

(defmethod encode ((inst singleton-creator) stream)
  (ecase (prototype inst)
    ((nil) (write-mnemonic :nil stream))
    ((t) (write-mnemonic :t stream))))

(defmethod encode ((inst symbol-creator) stream)
  (write-mnemonic :make-symbol stream)
  (write-index (symbol-creator-name inst) stream))

(defmethod encode ((inst interned-symbol-creator) stream)
  (write-mnemonic :intern stream)
  (write-index (symbol-creator-package inst) stream)
  (write-index (symbol-creator-name inst) stream))

(defmethod encode ((inst package-creator) stream)
  (write-mnemonic :find-package stream)
  (write-index (package-creator-name inst) stream))

(defmethod encode ((inst character-creator) stream)
  (write-mnemonic :make-character stream)
  (write-b32 (char-code (prototype inst)) stream))

(defmethod encode ((inst pathname-creator) stream)
  (write-mnemonic :make-pathname stream)
  (write-index (pathname-creator-host inst) stream)
  (write-index (pathname-creator-device inst) stream)
  (write-index (pathname-creator-directory inst) stream)
  (write-index (pathname-creator-name inst) stream)
  (write-index (pathname-creator-type inst) stream)
  (write-index (pathname-creator-version inst) stream))

(defmethod encode ((inst sb64-creator) stream)
  (write-mnemonic :make-sb64 stream)
  (write-b64 (prototype inst) stream))

(defmethod encode ((inst bignum-creator) stream)
  ;; uses sign-magnitude representation.
  (write-mnemonic :make-bignum stream)
  (let* ((number (prototype inst))
         (anumber (abs number))
         (nwords (ceiling (integer-length anumber) 64))
         (negp (minusp number)))
    (write-b64 (if negp (- nwords) nwords) stream)
    (loop for i from nwords above 0
          for pos = (* (1- i) 64)
          for word = (ldb (byte 64 pos) anumber)
          do (write-b64 word stream))))

#+short-float/binary16
(defmethod encode ((inst short-float-creator) stream)
  (write-mnemonic :make-binary16 stream)
  (write-b16 (ext:short-float-to-bits (prototype inst)) stream))

(defmethod encode ((inst single-float-creator) stream)
  (write-mnemonic :make-binary32 stream)
  (write-b32 (ext:single-float-to-bits (prototype inst)) stream))

(defmethod encode ((inst double-float-creator) stream)
  (write-mnemonic :make-binary64 stream)
  (write-b64 (ext:double-float-to-bits (prototype inst)) stream))

#+long-float/binary80
(defmethod encode ((inst long-float-creator) stream)
  (write-mnemonic :make-binary80 stream)
  (write-b80 (ext:long-float-to-bits (prototype inst)) stream))

#+long-float/binary128
(defmethod encode ((inst long-float-creator) stream)
  (write-mnemonic :make-binary128 stream)
  (write-b128 (ext:long-float-to-bits (prototype inst)) stream))

(defmethod encode ((inst ratio-creator) stream)
  (write-mnemonic :ratio stream)
  (write-index (ratio-creator-numerator inst) stream)
  (write-index (ratio-creator-denominator inst) stream))

(defmethod encode ((inst complex-creator) stream)
  (write-mnemonic :complex stream)
  (write-index (complex-creator-realpart inst) stream)
  (write-index (complex-creator-imagpart inst) stream))

(defmethod encode ((inst fdefinition-lookup) stream)
  (write-mnemonic :fdefinition stream)
  (write-index (name inst) stream))

(defmethod encode ((inst fcell-lookup) stream)
  (write-mnemonic :fcell stream)
  (write-index (name inst) stream))

(defmethod encode ((inst vcell-lookup) stream)
  (write-mnemonic :vcell stream)
  (write-index (name inst) stream))

(defmethod encode ((inst environment-lookup) stream)
  (write-mnemonic :environment stream))

(defmethod encode ((inst vdefinition) stream)
  (write-mnemonic :symbol-value stream)
  (write-index (name inst) stream))

(defmethod encode ((inst general-creator) stream)
  (write-mnemonic :funcall-create stream)
  (write-index (general-function inst) stream)
  (write-b16 (length (general-arguments inst)) stream)
  (loop for arg in (general-arguments inst)
        do (write-index arg stream)))

(defmethod encode ((inst general-initializer) stream)
  (write-mnemonic :funcall-initialize stream)
  (write-index (general-function inst) stream)
  (write-b16 (length (general-arguments inst)) stream)
  (loop for arg in (general-arguments inst)
        do (write-index arg stream)))

(defmethod encode ((inst class-creator) stream)
  (write-mnemonic :find-class stream)
  (write-index (class-creator-name inst) stream))

(defmethod encode ((inst load-time-value-creator) stream)
  (write-mnemonic :funcall-create stream)
  (write-index (load-time-value-creator-function inst) stream)
  ;; no arguments
  (write-b16 0 stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File compiler
;;;   

(defun bytecode-cf-compile-lexpr (lambda-expression environment)
  (let ((module (cmp:module/make))
        (environment (or environment (cmp:make-null-lexical-environment))))
    ;; Compile the code.
    (cmp:bytecompile-into module lambda-expression environment)))

(defun bytecode-compile-file-form (form env)
  (add-initializer-form form env))

(defclass bytefunction-creator (creator)
  ((%cfunction :initarg :cfunction :reader cfunction)
   (%module :initarg :module :reader module)
   (%nlocals :initarg :nlocals :reader nlocals :type (unsigned-byte 16))
   (%nclosed :initarg :nclosed :reader nclosed :type (unsigned-byte 16))
   (%entry-point :initarg :entry-point :reader entry-point
                 :type (unsigned-byte 32))
   (%size :initarg :size :reader size :type (unsigned-byte 32))))

;;; Given a CFUNCTION, generate a creator for the eventual runtime function.
(defun %add-function (value module-creator)
  (let ((inst
          (add-oob
           value
           (make-instance 'bytefunction-creator
             :cfunction value
             :module module-creator
             :nlocals (cmp:cfunction/nlocals value)
             :nclosed (length (cmp:cfunction/closed value))
             :entry-point (cmp:annotation/module-position
                           (cmp:cfunction/entry-point value))
             :size (cmp:cfunction/final-size value)))))
    (add-instruction (make-instance 'name-attr
                       :object inst
                       :objname (ensure-constant
                                 (cmp:cfunction/name value))))
    (when (cmp:cfunction/doc value)
      (add-instruction (make-instance 'docstring-attr
                         :object inst
                         :docstring (ensure-constant
                                     (cmp:cfunction/doc value)))))
    (add-instruction (make-instance 'lambda-list-attr
                       :function inst
                       :lambda-list (ensure-constant
                                     (cmp:cfunction/lambda-list value))))
    #+clasp ; source info
    (let ((cspi core:*current-source-pos-info*))
      (add-instruction
       (make-instance 'spi-attr
         :function inst
         :pathname (ensure-constant
                    (core:file-scope-pathname
                     (core:file-scope
                      (core:source-pos-info-file-handle cspi))))
         :lineno (core:source-pos-info-lineno cspi)
         :column (core:source-pos-info-column cspi)
         :filepos (core:source-pos-info-filepos cspi))))
    inst))

(defun add-function (cfunction)
  ;; We use this somewhat awkward structure because modules frequently
  ;; include their cfunctions (in literals or debug info), so if we just
  ;; went straight to %add-function we'd end up with duplicate functions.
  (let ((module (ensure-module (cmp:cfunction/module cfunction))))
    (or (find-oob cfunction)
        (%add-function cfunction module))))

(defmethod encode ((inst bytefunction-creator) stream)
  ;; four bytes for the entry point, two for the nlocals and nclosed,
  ;; then indices.
  (write-mnemonic :make-bytecode-function stream)
  (write-b32 (entry-point inst) stream)
  (write-b32 (size inst) stream)
  (write-b16 (nlocals inst) stream)
  (write-b16 (nclosed inst) stream)
  (write-index (module inst) stream))

;;; Having this be a vcreator with a prototype is a slight abuse of notation,
;;; since what we actually create is a bytecode module, not a compiler module.
;;; But this allows functions in the same module to share their module.
;;; This does mean that if someone tries to dump a literal compiler module
;;; they will hit problems. Unlikely, but nonetheless, FIXME
(defclass bytemodule-creator (vcreator)
  ((%lispcode :initform nil :initarg :lispcode :reader bytemodule-lispcode)))

(defclass setf-literals (effect)
  ((%module :initarg :module :reader setf-literals-module :type creator)
   ;; The literals are not practically coalesceable and are always a T vector,
   ;; so they're just encoded inline.
   (%literals :initarg :literals :reader setf-literals-literals
              :type simple-vector)))

(defmethod print-object ((object setf-literals) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a~{ ~a~}" (pindex (setf-literals-module object))
            (map 'list #'pindex (setf-literals-literals object))))
  object)

(defgeneric ensure-module-literal (literal-info))

(defmethod ensure-module-literal ((info cmp:constant-info))
  (ensure-constant (cmp:constant-info/value info)))

(defun ensure-function (cfunction)
  (or (find-oob cfunction) (add-function cfunction)))

(defmethod ensure-module-literal ((info cmp:cfunction))
  (ensure-function info))

(defmethod ensure-module-literal ((info cmp:load-time-value-info))
  (add-instruction
   (make-instance 'load-time-value-creator
     :function (add-form (cmp:load-time-value-info/form info))
     :read-only-p (cmp:load-time-value-info/read-only-p info)
     :form (cmp:load-time-value-info/form info)
     :info info)))

(defun ensure-fcell (name)
  (or (find-fcell name)
      (add-fcell name
                 (make-instance 'fcell-lookup
                   :name (ensure-constant name)))))

(defmethod ensure-module-literal ((info cmp:function-cell-info))
  (ensure-fcell (cmp:function-cell-info/fname info)))

(defun ensure-vcell (name)
  (or (find-vcell name)
      (add-vcell name
                 (make-instance 'vcell-lookup
                   :name (ensure-constant name)))))

(defmethod ensure-module-literal ((info cmp:variable-cell-info))
  (ensure-vcell (cmp:variable-cell-info/vname info)))

(defmethod ensure-module-literal ((info cmp:env-info))
  (or (find-environment)
      (add-environment (make-instance 'environment-lookup))))

(defgeneric process-debug-info (debug-info))

(defmethod process-debug-info ((info cmp:cfunction))
  (make-instance 'debug-info-function
    :function (ensure-function info)))

(defmethod process-debug-info ((item core:bytecode-debug-vars))
  (make-instance 'debug-info-vars
    :start (core:bytecode-debug-info/start item)
    :end (core:bytecode-debug-info/end item)
    :vars (loop for bdv
                  in (core:bytecode-debug-vars/bindings item)
                for adecls = (core:bytecode-debug-var/decls bdv)
                for ignore = (loop for d in adecls
                                   when (eq d 'cl:ignore) return d
                                   when (eq d 'cl:ignorable) return d)
                for inline = (loop for d in adecls
                                   when (eq d 'cl:inline) return d
                                   when (eq d 'cl:notinline) return d)
                for decls = (set-difference adecls
                                            '(cl:dynamic-extent cl:ignore
                                              cl:ignorable cl:inline
                                              cl:notinline))
                collect (make-instance 'debug-info-var
                          :name (ensure-constant
                                 (core:bytecode-debug-var/name bdv))
                          :frame-index (core:bytecode-debug-var/frame-index bdv)
                          :cellp (core:bytecode-debug-var/cellp bdv)
                          :dxp (not (not (member 'cl:dynamic-extent adecls)))
                          :ignore ignore :inline inline
                          :decls (mapcar #'ensure-constant decls)))))

(defmethod process-debug-info ((item core:bytecode-debug-location))
  (let ((spi (core:bytecode-debug-location/location item)))
    (make-instance 'debug-info-location
      :start (core:bytecode-debug-info/start item)
      :end (core:bytecode-debug-info/end item)
      :pathname (ensure-constant
                 (core:file-scope-pathname
                  (core:file-scope
                   (core:source-pos-info-file-handle spi))))
      :lineno (core:source-pos-info-lineno spi)
      :column (core:source-pos-info-column spi)
      :filepos (core:source-pos-info-filepos spi))))

(defmethod process-debug-info ((item core:bytecode-ast-decls))
  (make-instance 'debug-info-decls
    :start (core:bytecode-debug-info/start item)
    :end (core:bytecode-debug-info/end item)
    :decls (ensure-constant (core:bytecode-ast-decls/decls item))))

(defmethod process-debug-info ((item core:bytecode-ast-the))
  (make-instance 'debug-info-the
    :start (core:bytecode-debug-info/start item)
    :end (core:bytecode-debug-info/end item)
    :type (ensure-constant (core:bytecode-ast-the/type item))
    :receiving (core:bytecode-ast-the/receiving item)))

(defmethod process-debug-info ((item core:bytecode-ast-if))
  (make-instance 'debug-ast-if
    :start (core:bytecode-debug-info/start item)
    :end (core:bytecode-debug-info/end item)
    :receiving (core:bytecode-ast-if/receiving item)))

(defmethod process-debug-info ((item core:bytecode-ast-tagbody))
  (make-instance 'debug-ast-tagbody
    :start (core:bytecode-debug-info/start item)
    :end (core:bytecode-debug-info/end item)
    :tags (loop for (tag . ip)
                  in (core:bytecode-ast-tagbody/tags item)
                collect (cons (ensure-constant tag) ip))))

(defmethod process-debug-info ((item core:bytecode-ast-block))
  (make-instance 'debug-info-block
    :start (core:bytecode-debug-info/start item)
    :end (core:bytecode-debug-info/end item)
    :name (ensure-constant (core:bytecode-ast-block/name item))
    :receiving (core:bytecode-ast-block/receiving item)))

(defmethod process-debug-info ((item core:bytecode-debug-macroexpansion))
  (make-instance 'debug-info-macroexpansion
    :start (core:bytecode-debug-info/start item)
    :end (core:bytecode-debug-info/end item)
    :macro-name (ensure-constant
                 (core:bytecode-debug-macroexpansion/macro-name item))))

(defun mutable-LTVs (literals)
  (loop for lit across literals
        for i from 0
        when (and (typep lit 'cmp:load-time-value-info)
                  (not (cmp:load-time-value-info/read-only-p lit)))
          collect i))

#+clasp
(defvar *native-compile-file-all* nil)

(defun add-module (value)
  ;; Add the module first to prevent recursion.
  (cmp:module/link value)
  (let* ((bytecode (cmp:module/create-bytecode value))
         (literals (cmp:module/literals value))
         #+clasp
         (info (cmp:module/create-debug-info value))
         (mod
           (add-oob
            value
            (make-instance 'bytemodule-creator
              :prototype value :lispcode bytecode)))
         (cliterals
           (map 'simple-vector #'ensure-module-literal literals)))
    ;; Modules can indirectly refer to themselves recursively through
    ;; cfunctions, so we need to 2stage it here.
    (add-instruction
     (make-instance 'setf-literals :module mod :literals cliterals))
    #+clasp ; debug info
    (add-instruction
     (make-instance 'module-debug-attr
       :module mod
       :infos (map 'vector #'process-debug-info info)))
    #+clasp ; mutable LTVs
    (let ((mutables (mutable-LTVs literals)))
      (when mutables
        (add-instruction
         (make-instance 'module-mutable-ltv-attr
           :module mod
           :indices mutables))))
    ;; Native compilation.
    #+clasp
    (when *native-compile-file-all*
      (let* ((native (funcall (find-symbol "COMPILE-CMODULE"
                                           "CLASP-BYTECODE-TO-BIR")
                              bytecode info literals
                              :debug-namestring (namestring cmp::*compile-file-source-debug-pathname*)))
             (code (funcall (find-symbol "NMODULE-CODE"
                                         "CLASP-BYTECODE-TO-BIR")
                            native))
             (nlits (funcall (find-symbol "NMODULE-LITERALS"
                                          "CLASP-BYTECODE-TO-BIR")
                             native)))
        (add-instruction
         (make-instance 'module-native-attr
           :module mod
           :code code
           :literals (native-literals cliterals nlits)))
        ;; Add attributes for the functions as well.
        ;; We do this here instead of in the CFUNCTION methods because
        ;; of the recursive nature of functions referring to modules
        ;; referring to functions yada yada bla bla.
        (loop with fmap = (funcall (find-symbol "NMODULE-FMAP" "CLASP-BYTECODE-TO-BIR") native)
              for i across info
              when (typep i 'cmp:cfunction)
                do (let ((m (assoc i fmap)))
                     (assert m)
                     (destructuring-bind (main xep) (rest m)
                       (add-instruction
                        (make-instance 'function-native-attr
                          :function (ensure-function i)
                          :main (ensure-constant main)
                          :xep (ensure-constant xep))))))))
    mod))

(defun native-literals (cliterals nlits)
  (map 'vector (lambda (lit)
                 (if (integerp lit)
                     (aref cliterals lit)
                     (ensure-module-literal lit)))
       nlits))

(defun ensure-module (module)
  (or (find-oob module) (add-module module)))

(defmethod encode ((inst bytemodule-creator) stream)
  ;; Write instructions.
  (write-mnemonic :make-bytecode-module stream)
  (let* ((lispcode (bytemodule-lispcode inst))
         (len (length lispcode)))
    (when (> len #.(ash 1 32))
      (error "Bytecode length is ~d, too long to dump" len))
    (write-b32 len stream)
    (write-sequence lispcode stream)))

(defmethod encode ((inst setf-literals) stream)
  (write-mnemonic :setf-literals stream)
  (write-index (setf-literals-module inst) stream)
  (let ((literals (setf-literals-literals inst)))
    (write-b16 (length literals) stream)
    (loop for creator across literals
          do (write-index creator stream))))

;;;

(defmethod encode :before ((attr attribute) stream)
  (write-mnemonic :attribute stream)
  (write-index (name attr) stream))

(defmethod encode ((attr unknown-attr) stream)
  (write-b32 (length (bytes attr)) stream)
  (write-sequence (bytes attr) stream))

(defmethod encode ((attr name-attr) stream)
  (write-b32 (+ *index-bytes* *index-bytes*) stream)
  (write-index (object attr) stream)
  (write-index (objname attr) stream))

(defmethod encode ((attr docstring-attr) stream)
  (write-b32 (+ *index-bytes* *index-bytes*) stream)
  (write-index (object attr) stream)
  (write-index (docstring attr) stream))

(defmethod encode ((attr lambda-list-attr) stream)
  (write-b32 (+ *index-bytes* *index-bytes*) stream)
  (write-index (ll-function attr) stream)
  (write-index (lambda-list attr) stream))

(defmethod encode ((attr function-native-attr) stream)
  (write-b32 (* 3 *index-bytes*) stream)
  (write-index (ll-function attr) stream)
  (write-index (main attr) stream)
  (write-index (xep attr) stream))

#+clasp
(defmethod encode ((attr spi-attr) stream)
  ;; Write the length.
  (write-b32 (+ *index-bytes* *index-bytes* 8 8 8) stream)
  ;; And the data.
  (write-index (spi-attr-function attr) stream)
  (write-index (spi-attr-pathname attr) stream)
  (write-b64 (lineno attr) stream)
  (write-b64 (column attr) stream)
  (write-b64 (filepos attr) stream))

(defun debug-info-opcode (mnemonic)
  (or (getf cmpref:+debug-info-ops+ mnemonic)
      (error "unknown debug info mnemonic ~a" mnemonic)))

(defun write-debug-info-mnemonic (mnemonic stream)
  (write-byte (debug-info-opcode mnemonic) stream))

(defgeneric info-length (info))

(defmethod encode ((info debug-info-function) stream)
  (write-debug-info-mnemonic :function stream)
  (write-index (di-function info) stream))
(defmethod info-length ((info debug-info-function))
  (+ 1 *index-bytes*))

;;; Compute the FLAGS byte for a bytecode-debug-var.
;;; This is 00NNDIIC: C = cellp, D = dynamic-extent-p,
;;; NN = 01 for inline, 10 for notinline, 00 for default
;;; II = 01 for ignore, 10 for ignorable, 00 for default
(defun bdv-flags (bdv)
  (let ((result 0))
    (setf (ldb (byte 2 4) result)
          (ecase (di-inline bdv) (cl:inline #b01) (cl:notinline #b10) ((nil) #b00))
          (ldb (byte 1 3) result)
          (if (dynamic-extent-p bdv) #b1 #b0)
          (ldb (byte 2 1) result)
          (ecase (di-ignore bdv) (cl:ignore #b01) (cl:ignorable #b10) ((nil) #b00))
          (ldb (byte 1 0) result)
          (if (cellp bdv) #b1 #b0))
    result))

(defmethod encode ((info debug-info-vars) stream)
  (write-debug-info-mnemonic :vars stream)
  (write-b32 (di-start info) stream)
  (write-b32 (di-end info) stream)
  (let ((vars (vars info)))
    (write-b16 (length vars) stream)
    (loop for var in vars
          do (write-index (name var) stream)
             (write-b16 (frame-index var) stream)
             (write-byte (bdv-flags var) stream)
             (write-b16 (length (decls var)) stream)
             (loop for decl in (decls var)
                   do (write-index decl stream)))))
(defmethod info-length ((info debug-info-vars))
  (+ 1 4 4 2
     (loop for var in (vars info)
           sum (+ *index-bytes* 2 1 2)
           sum (* *index-bytes* (length (decls var))))))

(defmethod encode ((info debug-info-location) stream)
  (write-debug-info-mnemonic :location stream)
  (write-b32 (di-start info) stream)
  (write-b32 (di-end info) stream)
  (write-index (di-pathname info) stream)
  (write-b64 (lineno info) stream)
  (write-b64 (column info) stream)
  (write-b64 (filepos info) stream))
(defmethod info-length ((info debug-info-location))
  (+ 1 4 4 *index-bytes* 8 8 8))

(defmethod encode ((info debug-info-decls) stream)
  (write-debug-info-mnemonic :decls stream)
  (write-b32 (di-start info) stream)
  (write-b32 (di-end info) stream)
  (write-index (decls info) stream))
(defmethod info-length ((info debug-info-decls))
  (+ 1 4 4 *index-bytes*))

(defmethod encode ((info debug-info-the) stream)
  (write-debug-info-mnemonic :the stream)
  (write-b32 (di-start info) stream)
  (write-b32 (di-end info) stream)
  (write-index (di-type info) stream)
  (write-b32 (di-receiving info) stream))
(defmethod info-length ((info debug-info-the))
  (+ 1 4 4 *index-bytes* 4))

(defmethod encode ((info debug-ast-if) stream)
  (write-debug-info-mnemonic :if stream)
  (write-b32 (di-start info) stream)
  (write-b32 (di-end info) stream)
  (write-b32 (di-receiving info) stream))
(defmethod info-length ((info debug-ast-if))
  (+ 1 4 4 4))

(defmethod encode ((info debug-ast-tagbody) stream)
  (write-debug-info-mnemonic :tagbody stream)
  (write-b32 (di-start info) stream)
  (write-b32 (di-end info) stream)
  (write-b16 (length (di-tags info)) stream)
  (loop for (tag . ip) in (di-tags info)
        do (write-index tag stream)
           (write-b32 ip stream)))
(defmethod info-length ((info debug-ast-tagbody))
  (+ 1 4 4 2 (* (length (di-tags info))
                (+ *index-bytes* 4))))

(defmethod encode ((info debug-info-block) stream)
  (write-debug-info-mnemonic :block stream)
  (write-b32 (di-start info) stream)
  (write-b32 (di-end info) stream)
  (write-index (name info) stream)
  (write-b32 (di-receiving info) stream))
(defmethod info-length ((info debug-info-block))
  (+ 1 4 4 *index-bytes* 4))

(defmethod encode ((info debug-info-exit) stream)
  (write-debug-info-mnemonic :exit stream)
  (write-b32 (di-start info) stream)
  (write-b32 (di-end info) stream)
  (write-b32 (di-receiving info) stream))
(defmethod info-length ((info debug-info-exit))
  (+ 1 4 4 4))

(defmethod encode ((info debug-info-macroexpansion) stream)
  (write-debug-info-mnemonic :macro stream)
  (write-b32 (di-start info) stream)
  (write-b32 (di-end info) stream)
  (write-index (di-macro-name info) stream))
(Defmethod info-length ((info debug-info-macroexpansion))
  (+ 1 4 4 *index-bytes*))

(defmethod encode ((attr module-debug-attr) stream)
  ;; Write the length in bytes.
  (write-b32 (reduce #'+ (infos attr) :key #'info-length) stream)
  ;; Module index
  (write-index (module attr) stream)
  (let ((infos (infos attr)))
    ;; Number of infos.
    (write-b32 (length infos) stream)
    ;; The infos
    (map nil (lambda (info) (encode info stream)) infos)))

(defmethod encode ((attr module-mutable-ltv-attr) stream)
  (let ((indices (indices attr)))
    (write-b32 (+ 2 *index-bytes* (* 2 (length indices)))
               stream) ; length of attr
    (write-index (module attr) stream)
    (write-b16 (length indices) stream)
    (loop for index in indices
          do (write-b16 index stream))))

(defmethod encode ((attr module-native-attr) stream)
  (let ((code (code attr))
        (lits (module-native-attr-literals attr)))
    (write-b32 (+ *index-bytes*
                  4 (length code) 2 (* *index-bytes* (length lits)))
               stream)
    (write-index (module attr) stream)
    (write-b32 (length code) stream)
    (write-sequence code stream)
    (write-b16 (length lits) stream)
    (loop for creator across lits
          do (write-index creator stream))))

(defmethod encode ((init init-object-array) stream)
  (write-mnemonic :init-object-array stream)
  (write-b64 (init-object-array-count init) stream))

;;;

(defvar *compile-time-too*)

(defun bytecode-compile-toplevel-progn (forms env)
  (dolist (form forms)
    (bytecode-compile-toplevel form env)))

(defun bytecode-compile-toplevel-eval-when (situations forms env)
  (let ((ct (or (member :compile-toplevel situations)
                (member 'cl:compile situations)))
        (lt (or (member :load-toplevel situations)
                (member 'cl:load situations)))
        (e (or (member :execute situations)
               (member 'cl:eval situations)))
        (ctt *compile-time-too*))
    ;; Following CLHS figure 3-7 pretty exactly.
    (cond ((or (and ct lt) (and lt e ctt)) ; process compile-time-too
           (let ((*compile-time-too* t))
             (bytecode-compile-toplevel-progn forms env)))
          ((or (and lt e (not ctt)) (and (not ct) lt (not e)))
           ;; process not-compile-time
           (let ((*compile-time-too* nil))
             (bytecode-compile-toplevel-progn forms env)))
          ((or (and ct (not lt)) (and (not ct) (not lt) e ctt))
           ;; evaluate
           (funcall (cmp:bytecompile `(lambda () (progn ,@forms)) env)))
          (t
           ;; (or (and (not ct) (not lt) e (not ctt)) (and (not ct) (not lt) (not e)))
           ;; discard
           nil))))

(defun bytecode-compile-toplevel-locally (body env)
  (multiple-value-bind (decls body docs specials)
      (core:process-declarations body nil)
    (declare (ignore decls docs))
    (let* ((env (or env (cmp:make-null-lexical-environment)))
           (new-env
             (if specials
                 (cmp:lexenv/add-specials env specials)
                 env)))
      (bytecode-compile-toplevel-progn body new-env))))

(defun bytecode-compile-toplevel-macrolet (bindings body env)
  (let ((macros nil)
        (env (or env (cmp:make-null-lexical-environment))))
    (dolist (binding bindings)
      (let* ((name (car binding)) (lambda-list (cadr binding))
             (body (cddr binding))
             (eform (ext:parse-macro name lambda-list body env))
             (aenv (cmp:lexenv/macroexpansion-environment env))
             (expander (cmp:bytecompile eform aenv))
             (info (cmp:local-macro-info/make expander)))
        (push (cons name info) macros)))
    (bytecode-compile-toplevel-locally
     body (cmp:lexenv/make
           (cmp:lexenv/vars env)
           (cmp:lexenv/tags env) (cmp:lexenv/blocks env)
           (append macros (cmp:lexenv/funs env))
           (cmp:lexenv/decls env) (cmp:lexenv/frame-end env)))))

(defun bytecode-compile-toplevel-symbol-macrolet (bindings body env)
  (let ((smacros nil) (env (or env (cmp:make-null-lexical-environment))))
    (dolist (binding bindings)
      (push (cons (car binding)
                  (cmp:symbol-macro-var-info/make
                   (lambda (form env)
                     (declare (ignore form env))
                     (cadr binding))))
            smacros))
    (bytecode-compile-toplevel-locally
     body (cmp:lexenv/make
           (append (nreverse smacros) (cmp:lexenv/vars env))
           (cmp:lexenv/tags env) (cmp:lexenv/blocks env)
           (cmp:lexenv/funs env) (cmp:lexenv/decls env)
           (cmp:lexenv/frame-end env)))))

(defun bytecode-compile-toplevel (form &optional (env (cmp:make-null-lexical-environment)))
  (let ((core:*current-source-pos-info*
          (or (gethash form cmp:*source-locations*)
              core:*current-source-pos-info*))
        (form (macroexpand form env)))
    (if (consp form)
        (case (car form)
          ((progn) (bytecode-compile-toplevel-progn (cdr form) env))
          ((eval-when)
           (bytecode-compile-toplevel-eval-when (cadr form) (cddr form) env))
          ((locally) (bytecode-compile-toplevel-locally (cdr form) env))
          ((macrolet)
           (bytecode-compile-toplevel-macrolet (cadr form) (cddr form) env))
          ((symbol-macrolet)
           (bytecode-compile-toplevel-symbol-macrolet (cadr form) (cddr form) env))
          (otherwise
           (when *compile-time-too*
             (funcall (cmp:bytecompile `(lambda () (progn ,form)) env)))
           (bytecode-compile-file-form form env)))
        (progn
          (when *compile-time-too*
            (funcall (cmp:bytecompile `(lambda () (progn ,form)) env)))
          (bytecode-compile-file-form form env)))))

;; input is a character stream.
(defun bytecode-compile-stream (input output-path
                                &key (environment
                                      (cmp:make-null-lexical-environment))
                                &allow-other-keys)
  ;; *COMPILE-PRINT* is defined later in compile-file.lisp.
  (declare (special *compile-print*))
  (with-constants ()
    ;; Read and compile the forms.
    (loop with eof = (gensym "EOF")
          with *compile-time-too* = nil
          with eclector.reader:*client* = (make-instance 'cmp::clasp-tracking-elector-client)
          with cfsdp = (core:file-scope cmp::*compile-file-source-debug-pathname*)
          with cfsdl = cmp::*compile-file-source-debug-lineno*
          with cfsdo = cmp::*compile-file-source-debug-offset*
          for core:*current-source-pos-info*
            = (core:input-stream-source-pos-info input cfsdp cfsdl cfsdo)
          for cmp:*source-locations* = (make-hash-table :test #'eq)
          for form = (eclector.parse-result:read eclector.reader:*client* input nil eof)
          until (eq form eof)
          do (when *compile-print*
               (cmp::describe-form form))
             (bytecode-compile-toplevel form environment))
    ;; Write out the FASL bytecode.
    (cmp:with-atomic-file-rename (temp-output-path output-path)
      (with-open-file (output temp-output-path
                              :direction :output
                              :if-does-not-exist :create
                              :element-type '(unsigned-byte 8))

        (%write-bytecode output))))
  (truename output-path))
