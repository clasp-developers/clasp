(in-package #:cc-bmir)

(defclass fixnump (bir:one-input bir:conditional-test) ())
(defclass consp (bir:one-input bir:conditional-test) ())
(defclass characterp (bir:one-input bir:conditional-test) ())
(defclass single-float-p (bir:one-input bir:conditional-test) ())
(defclass generalp (bir:one-input bir:conditional-test) ())
(defclass headerq (bir:one-input bir:conditional-test)
  ((%info :initarg :info :reader info)))

;;;

;;; This instruction represents a low-level representation change, such as a
;;; NOP, putting into or removing from a multiple value structure, or box
;;; related operations.
(defclass cast (bir:one-input bir:one-output bir:instruction) ())

(defmethod bir::disassemble-instruction-extra append ((inst cast))
  (list (cc-bmir:rtype (bir:input inst))
        (cc-bmir:rtype (bir:output inst))))

;;; This instruction is lowered from bir:fixed-values-save.
;;; It takes multiple values as input and outputs a list of objects.
;;; Unlike a cast, it accepts only multiple values with a specific count.
(defclass mtf (bir:one-input bir:one-output bir:instruction)
  ((%nvalues :initarg :nvalues :reader bir:nvalues)))

(defmethod bir::disassemble-instruction-extra append ((inst mtf))
  (list (bir:nvalues inst)))

;;; This instruction is lowered from bir:values-collect when the rtypes of
;;; all inputs are a fixed number of values. This instruction appends those
;;; values together (i.e. has no runtime existence).
(defclass append-values (bir:one-output bir:instruction) ())

;;; This instruction is lowered from bir:mv-call when the number of
;;; arguments can be determined by the compiler. It is compiled into a
;;; simple call instead of an actual mv-sensitive call.
(defclass fixed-mv-call (bir:mv-call)
  (;; The determined argument count.
   (%nvalues :initarg :nvalues :reader bir:nvalues)))

;;; Ditto, but for local calls.
(defclass fixed-mv-local-call (bir:mv-local-call)
  ((%nvalues :initarg :nvalues :reader bir:nvalues)))

;;; This is a possible lowering of bir:constant-reference.
;;; We use an instruction because for unboxed constants we don't need the
;;; constant to be registered in the module or anything.
(defclass unboxed-constant-reference (bir:no-input bir:one-output
                                      bir:instruction)
  ((%value :initarg :value :reader bir:constant-value)))
(defmethod bir::disassemble-instruction-extra append
    ((inst unboxed-constant-reference))
  (list (bir:constant-value inst)))

;;;

(cleavir-stealth-mixins:define-stealth-mixin datum () bir:datum
  ((%rtype :initarg :rtype :initform :unassigned :accessor rtype)))

(cleavir-stealth-mixins:define-stealth-mixin
    load-time-value (datum) bir:load-time-value
  ((%rtype :initform '(:object))))

(cleavir-stealth-mixins:define-stealth-mixin
    constant (datum) bir:constant
  ((%rtype :initform '(:object))))

(cleavir-stealth-mixins:define-stealth-mixin
    function-cell (datum) bir:function-cell
  ((%rtype :initform '(:object))))
(cleavir-stealth-mixins:define-stealth-mixin
    variable-cell (datum) bir:variable-cell
  ((%rtype :initform '(:object))))
