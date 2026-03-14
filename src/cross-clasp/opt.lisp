(in-package #:cross-clasp)

;;; Defines a bunch of functions that are used in
;;; Clasp's compiler macro expansions.

(defun clos::classp (object) (typep object 'class))
(defun core::short-float-p (object) (typep object 'short-float))
(defun core::single-float-p (object) (typep object 'single-float))
(defun core::double-float-p (object) (typep object 'double-float))
(defun core::long-float-p (object) (typep object 'long-float))

(defun core::fixnump (object)
  ;; Make sure we use Clasp's idea of a fixnum.
  ;; the constants are defined by runtime-variables.lisp.
  (and (integerp object)
    (>= object (constant-form-value 'most-negative-fixnum))
    (<= object (constant-form-value 'most-positive-fixnum))))

(defun core::apply0 (function args) (apply function args))
(defun core::apply1 (function args a0) (apply function a0 args))
(defun core::apply2 (function args a0 a1) (apply function a0 a1 args))
(defun core::apply3 (function args a0 a1 a2)
  (apply function a0 a1 a2 args))
(defun core::apply4 (function args &rest rest)
  (multiple-value-call function
    (values-list rest) (values-list args)))

(defun core::two-arg-+ (x y) (+ x y))
(defun core::two-arg-* (x y) (* x y))
(defun core::two-arg-- (x y) (- x y))
(defun core::negate (x) (- x))
(defun core::two-arg-/ (x y) (/ x y))
(defun core::reciprocal (x) (/ x))
(defun core::two-arg-< (x y) (< x y))
(defun core::two-arg-<= (x y) (<= x y))
(defun core::two-arg-> (x y) (> x y))
(defun core::two-arg->= (x y) (>= x y))
(defun core::two-arg-= (x y) (= x y))

(defun core::logand-2op (x y) (logand x y))
(defun core::logior-2op (x y) (logior x y))

;; Use classes as "class holders".
(defun core::find-class-holder (name)
  (find-compiler-class name nil))
;; FIXME: These two oughtn't be in ext
(defun ext::class-unboundp (holder) (null holder))
(defun ext::class-get (holder) holder)
#|
(defun core::to-short-float (n) (float n 0s0))
(defun core::to-single-float (n) (float n 0f0))
(defun core::to-double-float (n) (float n 0d0))
(defun core::to-long-float (n) (float n 0l0))
|#
(defun core::make-vector (element-type dimension &optional adjustablep
                          fill-pointer displaced-to
                          (displaced-index-offset 0)
                          initial-element iesp)
  (cond
    (iesp
     (make-array dimension :element-type element-type
                           :adjustable adjustablep :fill-pointer fill-pointer
                           :initial-element initial-element))
    (displaced-to
     (make-array dimension :element-type element-type
                           :adjustable adjustablep :fill-pointer fill-pointer
                           :displaced-to displaced-to
                           :displaced-index-offset displaced-index-offset))
    (t
     (make-array dimension :element-type element-type
                           :adjustable adjustablep :fill-pointer fill-pointer))))

(defun core::concatenate-into-sequence (result &rest seqs)
  (loop for pos = 0 then (+ pos (length seq))
        for seq in seqs
        do (replace result seq :start1 pos))
  result)
#|
(defun core::every/1 (predicate sequence) (every predicate sequence))
(defun core::stringify (thing) (prin1-to-string thing))
|#
(defun core::make-simple-vector-character
    (dimension initial-element iesp)
  (if iesp
      (make-array dimension :initial-element initial-element
                            :element-type 'character)
      (make-array dimension :element-type 'character)))

(defun core::coerce-to-list (object) (coerce object 'list))

(define-condition undefined-type-warning (style-warning)
  ((%name :initarg :name :reader name)
   (%origin :initarg :origin :reader origin)
   (%kind :initform 'type))
  (:report (lambda (condition stream)
             (format stream "Undefined type ~s" (name condition)))))

(defun cmp::warn-undefined-type (origin type)
  (warn 'undefined-type-warning :name type :origin origin))

(define-condition cannot-coerce-warning (style-warning)
  ((%name :initarg :name :reader name)
   (%origin :initarg :origin :reader origin))
  (:report (lambda (condition stream)
             (format stream "Cannot coerce to type ~s: unknown or not defined for coerce" (name condition)))))

(defun cmp::warn-cannot-coerce (origin type)
  (warn 'cannot-coerce-warning :name type :origin origin))
