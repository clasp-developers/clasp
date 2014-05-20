;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;
;;; Copyright (c) 2010, Alexander Gavrilov (angavrilov@gmail.com)
;;;
;;; This file implements some common utility functions.
;;;

(in-package #:SSE)

;;; CPU control

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (ftype (function (&rest t) (unsigned-byte 32)) cpu-mxcsr-bits))
  (defun cpu-mxcsr-bits (&rest tags)
    (loop with mask = 0
       for tag in tags
       for bit = (if (listp tag)
                     (apply #'cpu-mxcsr-bits tag)
                     (ecase tag
                       (:except-invalid #x1)
                       (:except-denormal #x2)
                       (:except-divide-zero #x4)
                       (:except-overflow #x8)
                       (:except-underflow #x10)
                       (:except-precision #x20)
                       (:except-all #x3F)
                       (:denormals-are-zero #x40)
                       (:mask-invalid #x80)
                       (:mask-denormal #x100)
                       (:mask-divide-zero #x200)
                       (:mask-overflow #x400)
                       (:mask-underflow #x800)
                       (:mask-precision #x1000)
                       (:mask-all #x1f80)
                       (:round-nearest 0)
                       (:round-negative #x2000)
                       (:round-positive #x4000)
                       (:round-zero #x6000)
                       (:round-bits #x6000)
                       (:flush-to-zero #x8000)))
       do (setf mask (logior mask bit))
       finally (return mask)))
  (defun expand-cpu-mxcsr-bits (tags on-fail)
    (loop for tag in tags
       when (keywordp tag) collect tag into kwds
       else collect tag into rest
       finally
         (return
           (cond ((and kwds rest)
                  `(logior ,(apply #'cpu-mxcsr-bits kwds)
                           (cpu-mxcsr-bits ,@rest)))
                 (kwds
                  (apply #'cpu-mxcsr-bits kwds))
                 (t on-fail))))))

(define-compiler-macro cpu-mxcsr-bits (&whole whole &rest tags)
  (expand-cpu-mxcsr-bits tags whole))

(defmacro with-saved-mxcsr (&body code)
  (let ((v (gensym "CSR")))
    `(let ((,v (cpu-mxcsr)))
       (declare (type (unsigned-byte 32) ,v)
                #+ecl (:read-only ,v))
       (unwind-protect (progn ,@code)
         (%set-cpu-mxcsr ,v)))))

#+nil
(defun cpu-check-exceptions (&rest tags)
  (let ((mask (logand (cpu-mxcsr-bits (or tags :except-all))
                      (cpu-mxcsr-bits :except-all)))
        (csr (get-cpu-mxcsr)))
    (declare (optimize (safety 0) (speed 3) (debug 0))
             (type fixnum csr mask))
    (not (zerop (logand mask csr)))))

#+nil
(define-compiler-macro cpu-check-exceptions (&whole whole &rest tags)
  (let ((bits (expand-cpu-mxcsr-bits (or tags '(except-all)) nil)))
    (if (integerp bits)
        `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
           (not (zerop (logand (cpu-get-mxcsr)
                               ,(logand bits (cpu-mxcsr-bits :except-all))))))
        whole)))

#+nil
(macrolet ((foo (&rest names)
             (let* ((kwds (mapcar (lambda (x) (intern (format nil "MASK-~A" x) :keyword)) names))
                    (pvars (mapcar (lambda (x) (intern (format nil "~A-P" x))) names)))
               `(defun cpu-mask-exceptions (&key
                                            ,@(mapcar (lambda (n p) `(,n nil ,p)) names pvars)
                                            (other nil rest-p))
                  (let ((set-bits (logior ,@(mapcar (lambda (n k) `(if ,n (cpu-mxcsr-bits ,k) 0)) names kwds)))
                        (arg-bits (logior ,@(mapcar (lambda (p k) `(if ,p (cpu-mxcsr-bits ,k) 0)) pvars kwds))))
                    (%set-cpu-mxcsr
                     (the fixnum
                       (if (not rest-p)
                           (logior set-bits (logand (get-cpu-mxcsr) (lognot arg-bits)))
                           (logior set-bits
                                   (if other (logand (cpu-mxcsr-bits :mask-all) (lognot arg-bits)) 0)
                                   (logiand (get-cpu-mxcsr) (lognot (cpu-mxcsr-bits :mask-all)))))))
                    nil)))))
  (foo invalid denormal divide-zero overflow underflow precision))

(defun cpu-configure-rounding (&key round-to
                               (denormals-are-zero nil daz-p)
                               (flush-to-zero nil ftz-p))
  (let ((set 0)
        (mask 0))
    (when round-to
      (setf mask (cpu-mxcsr-bits :round-bits)
            set (ecase round-to
                  (:zero (cpu-mxcsr-bits :round-zero))
                  (:negative (cpu-mxcsr-bits :round-negative))
                  (:positive (cpu-mxcsr-bits :round-positive))
                  (:nearest (cpu-mxcsr-bits :round-nearest)))))
    (when daz-p
      (setf mask (logior mask (cpu-mxcsr-bits :denormals-are-zero)))
      (when denormals-are-zero
        (setf set (logior set (cpu-mxcsr-bits :denormals-are-zero)))))
    (when ftz-p
      (setf mask (logior mask (cpu-mxcsr-bits :flush-to-zero)))
      (when flush-to-zero
        (setf set (logior set (cpu-mxcsr-bits :flush-to-zero)))))
    (setf (cpu-mxcsr)
          (the (unsigned-byte 32)
            (logior set (logand (cpu-mxcsr) (lognot mask)))))
    nil))

