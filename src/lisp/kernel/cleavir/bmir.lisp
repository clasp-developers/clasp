(in-package #:cc-bmir)

(defclass fixnump (bir:conditional-test) ())
(defclass consp (bir:conditional-test) ())
(defclass characterp (bir:conditional-test) ())
(defclass single-float-p (bir:conditional-test) ())
(defclass generalp (bir:conditional-test) ())
(defclass headerq (bir:conditional-test)
  ((%info :initarg :info :reader info)))

;;;

(defclass memref2 (bir:one-input bir:one-output bir:instruction)
  ((%offset :initarg :offset :reader offset :type integer)))

(defclass load (cc-bir:atomic bir:one-input bir:one-output bir:instruction)
  ())

(defclass store (cc-bir:atomic bir:no-output bir:instruction)
  ())

(defclass cas (cc-bir:atomic bir:one-output bir:instruction)
  ())

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

(defclass datum (bir:datum)
  ((%rtype :initarg :rtype :accessor rtype)))

(defclass output (datum bir:output) ())
(defclass phi (datum bir:phi) ())
(defclass variable (datum bir:variable) ())

(defmethod rtype ((datum bir:argument)) '(:object))
(defmethod rtype ((datum bir:load-time-value)) '(:object))
(defmethod rtype ((datum bir:constant)) '(:object))
