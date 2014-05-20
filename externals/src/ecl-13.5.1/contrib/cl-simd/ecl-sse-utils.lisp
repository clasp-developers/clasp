;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;
;;; Copyright (c) 2010, Alexander Gavrilov (angavrilov@gmail.com)
;;;
;;; This file defines some extensions to the base intrinsic set,
;;; and other utility functions.
;;;

(in-package #:SSE)

;;; Helper macros and functions
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Try using a matching inverse function name
  (defun lookup-flip (arg pairs &key no-reverse)
    (and (consp arg)
         (let ((fix (or (cdr (assoc (first arg) pairs))
                        (unless no-reverse
                          (car (rassoc (first arg) pairs))))))
           (cond ((eq fix :identity)
                  (assert (null (cddr arg)))
                  (second arg))
                 (fix
                  `(,fix ,@(rest arg)))
                 (t nil)))))
  ;; Macroexpand, plus compiler expand some specific names
  (defun expand-condition (form env)
    (setq form (macroexpand form env))
    (loop while (and (consp form)
                     (symbolp (first form))
                     (get (first form) 'expand-in-condition))
       do (setq form (c::cmp-expand-macro (compiler-macro-function (first form))
                                          form env)))
    form)
  ;; Checks if the form is an unary call
  (defun is-unary? (form op)
    (and (consp form)
         (eq (first form) op)
         (null (cddr form))))
  ;; IF-style function expander
  (defun expand-if-macro (condition then-value else-value env if-f not-f or-f and-f andnot-f type-name zero-val &key flip)
    (let* ((condition (expand-condition condition env))
           (then-value (macroexpand then-value env))
           (else-value (macroexpand else-value env))
           (then-zero? (equal then-value zero-val))
           (else-zero? (equal else-value zero-val)))
      (cond ((is-unary? condition not-f)
             (expand-if-macro (second condition) else-value then-value
                              env if-f not-f or-f and-f andnot-f type-name zero-val
                              :flip (not flip)))
            ((and then-zero? else-zero?)
             zero-val)
            (then-zero?
             `(,andnot-f ,condition ,else-value))
            (else-zero?
             `(,and-f ,condition ,then-value))
            (t
             (let* ((csym (gensym))
                    (args `((,and-f ,csym ,then-value)
                            (,andnot-f ,csym ,else-value))))
               `(let ((,csym ,condition))
                  (declare (type ,type-name ,csym)
                           (:read-only ,csym))
                  (,or-f ,@(if flip (reverse args) args)))))))))

(defmacro def-utility (name arg-types ret-type expansion &key expand-args expand-in-condition)
  "Defines and exports a function & compiler macro with the specified expansion."
  (let* ((anames (mapcar #'make-arg-name (make-arg-nums arg-types))))
    `(progn
       (export ',name)
       (eval-when (:compile-toplevel :load-toplevel)
         ,@(if expand-in-condition
               `((setf (get ',name 'expand-in-condition) t)))
         (define-compiler-macro ,name (&environment env ,@anames)
           (declare (ignorable env))
           ,@(loop for arg in (if (eq expand-args t) anames expand-args)
                collect `(setq ,arg (macroexpand ,arg env)))
           ,expansion))
       (proclaim '(ftype (function ,(mapcar #'declaim-arg-type-of arg-types) ,ret-type) ,name))
       (defun ,name ,anames
         (declare (optimize (speed 0) (debug 0) (safety 1)))
         (let ,(mapcar #'list anames anames)
           (declare ,@(loop for an in anames and at in arg-types
                         collect `(type ,at ,an)))
           ;; Depends on the compiler macro being expanded:
           (,name ,@anames))))))

(defmacro def-if-function (name type-name postfix)
  `(def-utility ,name (,type-name ,type-name ,type-name) ,type-name
                (expand-if-macro arg0 arg1 arg2 env
                                 ',name
                                 ',(intern (format nil "NOT-~A" postfix))
                                 ',(intern (format nil "OR-~A" postfix))
                                 ',(intern (format nil "AND-~A" postfix))
                                 ',(intern (format nil "ANDNOT-~A" postfix))
                                 ',type-name
                                 '(,(intern (format nil "SETZERO-~A" postfix))))))

;;; Aligned array allocation

(deftype sse-array (elt-type &optional dims)
  "Type of arrays efficiently accessed by SSE aref intrinsics and returned by make-sse-array.
Should be assumed to be SIMPLE-ARRAY, except that displacing with MAKE-SSE-ARRAY is allowed."
  (when (eq elt-type '*)
    (c::cmperr "SSE-ARRAY must have a specific element type."))
  (let ((upgraded (upgraded-array-element-type elt-type)))
    (when (member upgraded '(t bit))
      (c::cmperr "Invalid SSE-ARRAY element type: ~S" elt-type))
    (unless (subtypep upgraded elt-type)
      (c::cmpwarn "SSE-ARRAY element type ~S has been upgraded to ~S" elt-type upgraded))
    `(array ,upgraded ,dims)))

(defun make-sse-array (dimensions &rest args &key (element-type '(unsigned-byte 8)) displaced-to &allow-other-keys)
  "Allocates an SSE-ARRAY aligned to the 16-byte boundary. May flatten displacement chains for performance reasons."
  (if displaced-to
      (apply #'make-array dimensions args)
      (multiple-value-bind (elt-size adj-type)
          (array-element-type-byte-size element-type)
        (when (eq adj-type t)
          (error "Cannot use element type T with SSE."))
        (sys::remf args :element-type)
        (let* ((full-size (if (numberp dimensions)
                              dimensions
                              (reduce #'* dimensions)))
               (padded-size (+ full-size (ceiling 15 elt-size)))
               (array (apply #'make-array padded-size :element-type adj-type args))
               (misalign (ffi:c-inline (array) (:object) :int
                           "(((unsigned long)(#0)->array.self.b8) & 15)"
                           :one-liner t))
               (offset (/ (if (> misalign 0) (- 16 misalign) 0) elt-size)))
          (make-array dimensions :element-type element-type
                      :displaced-to array :displaced-index-offset offset)))))

;;; Single-float tools

;; Constants

(defmacro set-true-ss ()
  (load-time-value (make-pack-of-bin #xFFFFFFFF :as 'float-sse-pack)))

(defmacro set-true-ps ()
  (load-time-value (make-pack-of-bin -1 :as 'float-sse-pack)))

(eval-when (:compile-toplevel :load-toplevel)
  (define-symbol-macro 0.0-ps (setzero-ps))

  (define-symbol-macro true-ss (set-true-ss))
  (define-symbol-macro false-ss (setzero-ps))

  (define-symbol-macro true-ps (set-true-ps))
  (define-symbol-macro false-ps (setzero-ps)))

;; Bitwise if

(def-if-function if-ps float-sse-pack #:ps)

;; Arithmetic negation (xor with negative zero)

(def-utility neg-ss (float-sse-pack) float-sse-pack
             `(xor-ps ,arg0 ,(load-time-value (make-pack-of-bin #x80000000 :as 'float-sse-pack))))

(def-utility neg-ps (float-sse-pack) float-sse-pack
             `(xor-ps ,arg0 ,(load-time-value
                              (make-pack-of-bin #x80000000800000008000000080000000 :as 'float-sse-pack))))

;; Logical inversion

(def-utility not-ps (float-sse-pack) float-sse-pack
             (or (lookup-flip arg0 '((=-ps . /=-ps)
                                     (<-ps . /<-ps)
                                     (<=-ps . /<=-ps)
                                     (>-ps . />-ps)
                                     (>=-ps . />=-ps)
                                     (cmpord-ps . cmpunord-ps)
                                     (not-ps . :identity)))
                 `(xor-ps ,arg0 true-ps))
             :expand-args t)

;; Shuffle

(defun shuffle-ps (x y mask)
  (declare (optimize (speed 0) (debug 0) (safety 1))
           (type t x y mask))
  (check-type x sse-pack)
  (check-type y sse-pack)
  (check-type mask (unsigned-byte 8))
  (ffi:c-inline (x y mask) (:object :object :int) :float-sse-pack
    "_mm_setr_ps(
       (#0)->sse.data.sf[(#2)&3],
       (#0)->sse.data.sf[((#2)>>2)&3],
       (#1)->sse.data.sf[((#2)>>4)&3],
       (#1)->sse.data.sf[((#2)>>6)&3]
     )" :one-liner t))

;;; Double-float tools

;; Constants

(defmacro set-true-sd ()
  (load-time-value (make-pack-of-bin #xFFFFFFFFFFFFFFFF :as 'double-sse-pack)))

(defmacro set-true-pd ()
  (load-time-value (make-pack-of-bin -1 :as 'double-sse-pack)))

(eval-when (:compile-toplevel :load-toplevel)
  (define-symbol-macro 0.0-pd (setzero-pd))

  (define-symbol-macro true-sd (set-true-sd))
  (define-symbol-macro false-sd (setzero-pd))

  (define-symbol-macro true-pd (set-true-pd))
  (define-symbol-macro false-pd (setzero-pd)))

;; Bitwise if

(def-if-function if-pd double-sse-pack #:pd)

;; Arithmetic negation (xor with negative zero)

(def-utility neg-sd (double-sse-pack) double-sse-pack
             `(xor-pd ,arg0
                      ,(load-time-value
                        (make-pack-of-bin #x8000000000000000 :as 'double-sse-pack))))

(def-utility neg-pd (double-sse-pack) double-sse-pack
             `(xor-pd ,arg0
                      ,(load-time-value
                        (make-pack-of-bin #x80000000000000008000000000000000 :as 'double-sse-pack))))

;; Logical inversion

(def-utility not-pd (double-sse-pack) double-sse-pack
             (or (lookup-flip arg0 '((=-pd . /=-pd)
                                     (<-pd . /<-pd)
                                     (<=-pd . /<=-pd)
                                     (>-pd . />-pd)
                                     (>=-pd . />=-pd)
                                     (cmpord-pd . cmpunord-pd)
                                     (not-pd . :identity)))
                 `(xor-pd ,arg0 true-pd))
             :expand-args t)

;; Shuffle

(defun shuffle-pd (x y mask)
  (declare (optimize (speed 0) (debug 0) (safety 1))
           (type t x y mask))
  (check-type x sse-pack)
  (check-type y sse-pack)
  (check-type mask (unsigned-byte 2))
  (ffi:c-inline (x y mask) (:object :object :int) :double-sse-pack
    "_mm_setr_pd(
       (#0)->sse.data.df[(#2)&1],
       (#1)->sse.data.df[((#2)>>1)&1]
     )" :one-liner t))

;;; Integer tools

;; Constants

(defmacro set-true-pi ()
  (load-time-value (make-pack-of-bin -1 :as 'int-sse-pack)))

(eval-when (:compile-toplevel :load-toplevel)
  (define-symbol-macro 0-pi (setzero-pi))

  (define-symbol-macro true-pi (set-true-pi))
  (define-symbol-macro false-pi (setzero-pi)))

;; Bitwise if

(def-if-function if-pi float-sse-pack #:pi)

;; Arithmetic negation (subtract from 0)

(macrolet ((frob (name subf)
             `(def-utility ,name (int-sse-pack) int-sse-pack
                           `(,',subf (setzero-pi) ,arg0))))
  (frob neg-pi8  sub-pi8)
  (frob neg-pi16 sub-pi16)
  (frob neg-pi32 sub-pi32)
  (frob neg-pi64 sub-pi64))

;; Logical inversion

(def-utility not-pi (int-sse-pack) int-sse-pack
             (or (lookup-flip arg0 '((<=-pi8 . >-pi8)
                                     (<=-pi16 . >-pi16)
                                     (<=-pi32 . >-pi32)
                                     (>=-pi8 . <-pi8)
                                     (>=-pi16 . <-pi16)
                                     (>=-pi32 . <-pi32)
                                     (/=-pi8 . =-pi8)
                                     (/=-pi16 . =-pi16)
                                     (/=-pi32 . =-pi32)
                                     (not-pi . :identity))
                              :no-reverse t)
                 `(xor-pi ,arg0 true-pi))
             :expand-args t)

(macrolet ((frob (name code)
             `(def-utility ,name (int-sse-pack int-sse-pack) int-sse-pack
                           ,code
                           :expand-in-condition t)))

  (frob <=-pi8  `(not-pi (>-pi8 ,arg0 ,arg1)))
  (frob <=-pi16 `(not-pi (>-pi16 ,arg0 ,arg1)))
  (frob <=-pi32 `(not-pi (>-pi32 ,arg0 ,arg1)))

  (frob >=-pi8  `(not-pi (<-pi8 ,arg0 ,arg1)))
  (frob >=-pi16 `(not-pi (<-pi16 ,arg0 ,arg1)))
  (frob >=-pi32 `(not-pi (<-pi32 ,arg0 ,arg1)))

  (frob /=-pi8  `(not-pi (=-pi8 ,arg0 ,arg1)))
  (frob /=-pi16 `(not-pi (=-pi16 ,arg0 ,arg1)))
  (frob /=-pi32 `(not-pi (=-pi32 ,arg0 ,arg1))))

;; Shifts

(defun slli-pi (x shift)
  (declare (optimize (speed 0) (debug 0) (safety 1))
           (type t x shift))
  (check-type x sse-pack)
  (check-type shift (unsigned-byte 8))
  (ffi:c-inline (x shift) (:object :int) :object
    "cl_object rv = ecl_make_int_sse_pack(_mm_setzero_si128());
     unsigned bshift=(#1), i;
     for (i = 0; i + bshift < 16; i++)
       rv->sse.data.b8[i+bshift] = (#0)->sse.data.b8[i];
     @(return) = rv;"))

(defun srli-pi (x shift)
  (declare (optimize (speed 0) (debug 0) (safety 1))
           (type t x shift))
  (check-type x sse-pack)
  (check-type shift (unsigned-byte 8))
  (ffi:c-inline (x shift) (:object :int) :object
    "cl_object rv = ecl_make_int_sse_pack(_mm_setzero_si128());
     int bshift=(#1), i;
     for (i = 16 - bshift - 1; i >= 0; i--)
       rv->sse.data.b8[i] = (#0)->sse.data.b8[i+bshift];
     @(return) = rv;"))

;; Extract & insert

(defun extract-pi16 (x index)
  (declare (optimize (speed 0) (debug 0) (safety 1))
           (type t x index))
  (check-type x sse-pack)
  (check-type index (unsigned-byte 8))
  (ffi:c-inline (x index) (:object :int) :fixnum
    "*((unsigned short*)&(#0)->sse.data.b8[((#1)&3)*2])"
    :one-liner t))

(defun insert-pi16 (x ival index)
  (declare (optimize (speed 0) (debug 0) (safety 1))
           (type t x ival index))
  (check-type x sse-pack)
  (check-type index (unsigned-byte 8))
  (ffi:c-inline (x ival index) (:int-sse-pack :int :int) :object
    "cl_object rv = ecl_make_int_sse_pack(#0);
     *((unsigned short*)&rv->sse.data.b8[((#2)&3)*2]) = (unsigned short)(#1);
     @(return) = rv;"))

;; Shuffles

(defun shuffle-pi32 (x mask)
  (declare (optimize (speed 0) (debug 0) (safety 1))
           (type t x mask))
  (check-type x sse-pack)
  (check-type mask (unsigned-byte 8))
  (ffi:c-inline (x mask) (:object :int) :int-sse-pack
    "unsigned *pd = (unsigned*)(#0)->sse.data.b8;
     @(return) = _mm_setr_epi32(pd[(#1)&3],pd[((#1)>>2)&3],pd[((#1)>>4)&3],pd[((#1)>>6)&3]);"))

(defun shufflelo-pi16 (x mask)
  (declare (optimize (speed 0) (debug 0) (safety 1))
           (type t x mask))
  (check-type x sse-pack)
  (check-type mask (unsigned-byte 8))
  (ffi:c-inline (x mask) (:object :int) :int-sse-pack
    "unsigned short *pd = (unsigned short*)(#0)->sse.data.b8;
     @(return) = _mm_setr_epi16(
       pd[(#1)&3],pd[((#1)>>2)&3],pd[((#1)>>4)&3],pd[(((#1)>>6)&3)],
       pd[4], pd[5], pd[6], pd[7]
     );"))

(defun shufflehi-pi16 (x mask)
  (declare (optimize (speed 0) (debug 0) (safety 1))
           (type t x mask))
  (check-type x sse-pack)
  (check-type mask (unsigned-byte 8))
  (ffi:c-inline (x mask) (:object :int) :int-sse-pack
    "unsigned short *pb = (unsigned short*)(#0)->sse.data.b8, *pd = pb+4;
     @(return) = _mm_setr_epi16(
       pb[0], pb[1], pb[2], pb[3],
       pd[(#1)&3],pd[((#1)>>2)&3],pd[((#1)>>4)&3],pd[(((#1)>>6)&3)]
     );"))

