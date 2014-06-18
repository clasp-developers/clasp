;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;
;;; Dumbly translated from C code at: http://github.com/jj1bdx/sfmt-extstate

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cl-simd))

(defpackage #:sfmt-test
  (:use #:common-lisp #:sse))

(in-package #:sfmt-test)

(deftype uint32 () '(unsigned-byte 32))
(deftype uint32-vector () '(sse-array uint32 (*)))

(defconstant +mexp+ 19937)
(defconstant +n+ (1+ (floor +mexp+ 128)))
(defconstant +pos1+ 122)
(defconstant +sl1+ 18)
(defconstant +sl2+ 1)
(defconstant +sr1+ 11)
(defconstant +sr2+ 1)

(defconstant +msk1+ #xdfffffef)
(defconstant +msk2+ #xddfecb7f)
(defconstant +msk3+ #xbffaffff)
(defconstant +msk4+ #xbffffff6)

(defconstant +parity1+ #x00000001)
(defconstant +parity2+ #x00000000)
(defconstant +parity3+ #x00000000)
(defconstant +parity4+ #x13c9e684)

(defconstant +uint32-mask+ #xFFFFFFFF)

(defvar *work-buffer* (make-sse-array (* +n+ 4) :element-type 'uint32))

(defun period-certification (buffer)
  (declare (type uint32-vector buffer))
  (let ((inner (logxor (logand (aref buffer 0) +parity1+)
                       (logand (aref buffer 1) +parity2+)
                       (logand (aref buffer 2) +parity3+)
                       (logand (aref buffer 3) +parity4+))))
    (loop for i = 16 then (ash i -1) while (> i 0)
       do (setf inner (logxor inner (ash inner (- i)))))
    (when (logtest inner 1)
      (return-from period-certification)))
  (loop
     for i from 0 to 3
     for parity in (load-time-value (list +parity1+ +parity2+ +parity3+ +parity4+))
     do (loop
           for work = 1 then (ash work 1)
           for j from 0 below 32
           when (/= 0 (logand work parity))
           do (progn
                (setf (aref buffer i)
                      (logxor (aref buffer i) work))
                (return-from period-certification)))))

(defun init-gen-rand (seed buffer)
  (declare (type uint32 seed)
           (type uint32-vector buffer))
  (setf (aref buffer 0) seed)
  (loop for i from 1 below (array-total-size buffer)
     do (setf (aref buffer i)
              (logand +uint32-mask+
                      (+ i
                         (* 1812433253 (logxor (aref buffer (1- i))
                                               (ash (aref buffer (1- i)) -30)))))))
  (period-certification buffer))

;; Should be an inline function, but it's broken in ECL
(defmacro recursion (a b c d mask)
  `(let ((x ,a)
         (y (srli-pi32 ,b +sr1+))
         (z (srli-pi ,c +sr2+))
         (v (slli-pi32 ,d +sl1+))
         (m ,mask))
     (xor-pi (xor-pi (xor-pi z x) v)
             (xor-pi (slli-pi x +sl2+)
                     (and-pi y m)))))

(defmacro sfmt-aref (buf idx)
  `(row-major-aref-api ,buf (the fixnum (* 4 (the fixnum ,idx)))))

(defun gen-rand-all (buffer)
  (declare (optimize (speed 3) #+ecl (safety 0) (debug 0)
                     #+sbcl (sb-c::insert-array-bounds-checks 0))
           (type uint32-vector buffer))
  #+ecl (check-type buffer uint32-vector)
  (assert (= (array-total-size buffer) (* +n+ 4)))
  (let ((mask (set-pu32 +msk4+ +msk3+ +msk2+ +msk1+))
        (r1 (sfmt-aref buffer (- +n+ 2)))
        (r2 (sfmt-aref buffer (- +n+ 1))))
    (declare (type int-sse-pack mask r1 r2))
    (macrolet ((twist (delta)
                 `(psetq r1 r2
                         r2 (setf (sfmt-aref buffer i)
                                  (recursion (sfmt-aref buffer i)
                                             (sfmt-aref buffer (+ i (the fixnum ,delta)))
                                             r1 r2 mask)))))
      (loop for i fixnum from 0 below (- +n+ +pos1+)
         do (twist +pos1+))
      (loop for i fixnum from (- +n+ +pos1+) below +n+
         do (twist (- +pos1+ +n+))))))

(defun gen-rand-array (output buffer)
  (declare (optimize (speed 3) #+ecl (safety 0) (debug 0)
                     #+sbcl (sb-c::insert-array-bounds-checks 0))
           (type uint32-vector buffer output))
  #+ecl (check-type buffer uint32-vector)
  #+ecl (check-type output uint32-vector)
  (assert (= (array-total-size buffer) (* +n+ 4)))
  (let ((mask (set-pu32 +msk4+ +msk3+ +msk2+ +msk1+))
        (size (floor (array-total-size output) 4))
        (r1 (sfmt-aref buffer (- +n+ 2)))
        (r2 (sfmt-aref buffer (- +n+ 1))))
    (declare (type int-sse-pack mask r1 r2)
             (type fixnum size))
    (assert (> size (* +n+ 2)))
    (macrolet ((twist (tgt src1 delta1 src2 delta2)
                 `(psetq r1 r2
                         r2 (setf (sfmt-aref ,tgt i)
                                  (recursion (sfmt-aref ,src1 (- i (the fixnum ,delta1)))
                                             (sfmt-aref ,src2 (+ i (the fixnum ,delta2)))
                                             r1 r2 mask)))))
      (loop for i fixnum from 0 below (- +n+ +pos1+)
         do (twist output buffer 0 buffer +pos1+))
      (loop for i fixnum from (- +n+ +pos1+) below +n+
         do (twist output buffer 0 output (- +pos1+ +n+)))
      (loop for i fixnum from +n+ below (- size +n+)
         do (twist output output +n+ output (- +pos1+ +n+)))
      #+ ()
      (loop for j fixnum from 0 below (- (* 2 +n+) size)
         do (setf (sfmt-aref buffer j)
                  (sfmt-aref output (+ j (the fixnum (- size +n+))))))
      (loop
         for i fixnum from (- size +n+) below size
         for j fixnum from 0 below +n+ ;(max 0 (- (* 2 +n+) size))
         do (twist output output +n+ output (- +pos1+ +n+))
         do (setf (sfmt-aref buffer j) r2))
      output)))

(defun test ()
  (let ((out (make-sse-array 10000 :element-type 'uint32)))
    (init-gen-rand 1234 *work-buffer*)
    (gen-rand-array out *work-buffer*)
    (assert (equal (coerce (subseq out 995 1000) 'list)
                   '(2499610950 3057240914 1662679783 461224431 1168395933)))
    (gen-rand-array out *work-buffer*)
    (assert (equal (coerce (subseq out 995 1000) 'list)
                   '(648219337 458306832 3674950976 4030368244 2918117049)))))

(dotimes (i 10)
  (test))

