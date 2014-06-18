;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;
;;; Copyright (c) 2010, Alexander Gavrilov (angavrilov@gmail.com)
;;;
;;; This file implements VOP-wrapping functions and non-primitive
;;; extensions to the core intrinsic set.
;;;

(in-package #:SSE)

;;; Materialize the intrinsic functions.

;; Since VOPs are activated only on load, actual functions that
;; wrap them have to be defined in a different file. This is a
;; hack to generate the functions from the same macro invocations
;; as the VOPS.

(macrolet ((def-float-set-intrinsic (pubname fname atype aregtype rtype move)
             (declare (ignore aregtype move))
             `(progn
                (defun ,fname (arg)
                  (declare (type ,atype arg))
                  (truly-the ,rtype (%primitive ,fname arg)))
                ;; Public function - includes coercion
                (export ',pubname)
                (declaim (ftype (function (real) ,rtype) ,pubname)
                         (inline ,pubname))
                (defun ,pubname (arg) (,fname (coerce arg ',atype)))))
           (def-unary-intrinsic (name rtype insn cost c-name &key immediate-arg &allow-other-keys)
             (declare (ignore insn cost c-name))
             (unless immediate-arg
               `(defun ,name (x)
                  (declare (type sse-pack x))
                  (truly-the ,rtype (%primitive ,name x)))))
           (def-binary-intrinsic (name rtype insn cost c-name &key immediate-arg &allow-other-keys)
             (declare (ignore insn cost c-name))
             (unless immediate-arg
               `(defun ,name (x y ,@(if immediate-arg '(imm)))
                  (declare (type sse-pack x y))
                  (truly-the ,rtype (%primitive ,name x y)))))
           (def-sse-int-intrinsic (name itype rtype insn cost c-name &key immediate-arg &allow-other-keys)
             (declare (ignore insn cost c-name))
             (unless immediate-arg
               `(defun ,name (x iv)
                  (declare (type sse-pack x)
                           (type ,itype iv))
                  (truly-the ,rtype (%primitive ,name x iv)))))
           (def-comparison-intrinsic (name arg-type insn cost c-name &key &allow-other-keys)
             (declare (ignore insn cost c-name arg-type))
             `(defun ,name (x y)
                (declare (type sse-pack x y))
                (truly-the boolean (,name x y))))
           (def-load-intrinsic (name rtype insn c-name &key register-arg &allow-other-keys)
             (declare (ignore insn c-name))
             (let* ((vop (symbolicate "%" name))
                    (valarg (if register-arg '(value))))
               `(progn
                  (declaim (inline ,name))
                  (defun ,name (,@valarg pointer &optional (offset 0))
                    (declare ,@(if register-arg '((type sse-pack value)))
                             (type system-area-pointer pointer)
                             (type signed-word offset))
                    ,(if rtype
                         `(truly-the ,rtype (,vop ,@valarg pointer offset 1 0))
                         `(,vop ,@valarg pointer offset 1 0))))))
           (def-store-intrinsic (name rtype insn c-name &key setf-name &allow-other-keys)
             (declare (ignore insn c-name))
             (let* ((vop (symbolicate "%" name)))
               `(progn
                  (declaim (inline ,name))
                  (defun ,name (pointer value &optional (offset 0))
                    (declare (type system-area-pointer pointer)
                             (type sse-pack value)
                             (type signed-word offset))
                    (,vop pointer offset 1 0 value)
                    (truly-the ,rtype value))
                  ,(if setf-name
                       `(defsetf ,setf-name (pointer &optional (offset 0)) (value)
                          `(,',name ,pointer ,value ,offset)))))))
  ;; Load the definition list
  #.(loop for name being each present-symbol
       when (get name 'intrinsic-spec)
       collect it into specs
       finally (return `(progn ,@specs))))

;;; Helper functions and macros

(defmacro def-utility (name args rtype &body code)
  `(progn
     (export ',name)
     (declaim (ftype (function ,(mapcar (constantly 'sse-pack) args) ,rtype) ,name)
              (inline ,name))
     (defun ,name ,args ,@code)))

(defmacro def-if-function (name rtype postfix)
  (let* ((not-x (symbolicate "NOT-" postfix))
         (or-x (symbolicate "OR-" postfix))
         (and-x (symbolicate "AND-" postfix))
         (andn-x (symbolicate "ANDNOT-" postfix))
         (xor-x (symbolicate "XOR-" postfix))
         (true (%make-sse-pack #xFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFF))
         (false (%make-sse-pack 0 0)))
    `(progn
       (export ',name)
       (defknown ,name (sse-pack sse-pack sse-pack) ,rtype (foldable flushable))
       (defun ,name (condition true-val false-val)
         (,or-x (,and-x condition true-val)
                (,andn-x condition false-val)))
       ;; Instead of inlining, use a transform so that the splice
       ;; rule has a chance to apply. This depends on transform
       ;; definitions behaving like a LIFO:
       (deftransform ,name ((condition true-val false-val) * *)
         "Expand the conditional."
         '(,or-x (,and-x condition true-val) (,andn-x condition false-val)))
       (def-splice-transform ,name ((,not-x cond) tv fv) (,name cond fv tv))
       ;; NOT elimination and partial constant folding for bitwise ops:
       (def-splice-transform ,not-x ((,not-x arg1)) arg1)
       (def-splice-transform ,and-x (arg1 (,not-x arg2)) (,andn-x arg2 arg1))
       (def-splice-transform ,and-x ((,not-x arg1) arg2) (,andn-x arg1 arg2))
       (def-splice-transform ,andn-x ((,not-x arg1) arg2) (,and-x arg1 arg2))
       (%deftransform ',or-x '(function * *) #'commutative-arg-swap "place constant arg last")
       (%deftransform ',and-x '(function * *) #'commutative-arg-swap "place constant arg last")
       (%deftransform ',xor-x '(function * *) #'commutative-arg-swap "place constant arg last")
       (deftransform ,or-x ((arg1 arg2) (* (constant-arg (member ,true))) *) ,true)
       (deftransform ,or-x ((arg1 arg2) (* (constant-arg (member ,false))) *) 'arg1)
       (deftransform ,and-x ((arg1 arg2) (* (constant-arg (member ,true))) *) 'arg1)
       (deftransform ,and-x ((arg1 arg2) (* (constant-arg (member ,false))) *) ,false)
       (deftransform ,xor-x ((arg1 arg2) (* (constant-arg (member ,false))) *) 'arg1)
       (deftransform ,andn-x ((arg1 arg2) (* (constant-arg (member ,true))) *) 'arg1)
       (deftransform ,andn-x ((arg1 arg2) (* (constant-arg (member ,false))) *) ,false)
       (deftransform ,andn-x ((arg1 arg2) ((constant-arg (member ,true)) *) *) ,false)
       (deftransform ,andn-x ((arg1 arg2) ((constant-arg (member ,false)) *) *) 'arg2))))

(defmacro def-not-cmp-pairs (not-fun &rest pairs)
  `(progn
     ,@(loop for (a b) on pairs by #'cddr
          collect `(def-splice-transform ,not-fun ((,a arg1 arg2)) (,b arg1 arg2))
          collect `(def-splice-transform ,not-fun ((,b arg1 arg2)) (,a arg1 arg2)))))

;;; CPU control

(defun cpu-mxcsr ()
  (cpu-mxcsr))

(defun %set-cpu-mxcsr (x)
  (declare (type (unsigned-byte 32) x))
  (%set-cpu-mxcsr x))

(defsetf cpu-mxcsr %set-cpu-mxcsr)

(defun cpu-load-fence () (cpu-load-fence))
(defun cpu-store-fence () (cpu-store-fence))
(defun cpu-memory-fence () (cpu-memory-fence))

(defun cpu-pause () (cpu-pause))

;;; Single-float

;; Constants

(define-symbol-macro 0.0-ps (truly-the float-sse-pack #.(%make-sse-pack 0 0)))

(define-symbol-macro true-ss (truly-the float-sse-pack #.(%make-sse-pack #xFFFFFFFF 0)))
(define-symbol-macro true-ps (truly-the float-sse-pack #.(%make-sse-pack #xFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFF)))

(define-symbol-macro false-ss (truly-the float-sse-pack #.(%make-sse-pack 0 0)))
(define-symbol-macro false-ps (truly-the float-sse-pack #.(%make-sse-pack 0 0)))

;; Initialization

(declaim (inline set1-ps set-ps setr-ps setzero-ps))

(defun set1-ps (val)
  (let ((valv (set-ss val)))
    (shuffle-ps valv valv 0)))

(defun set-ps (x3 x2 x1 x0)
  (movelh-ps (unpacklo-ps (set-ss x0) (set-ss x1))
             (unpacklo-ps (set-ss x2) (set-ss x3))))

(defun setr-ps (x0 x1 x2 x3)
  (movelh-ps (unpacklo-ps (set-ss x0) (set-ss x1))
             (unpacklo-ps (set-ss x2) (set-ss x3))))

(defun setzero-ps () 0.0-ps)

;; Arithmetic negation

(def-utility neg-ss (arg) float-sse-pack
  (xor-ps arg #.(%make-sse-pack #x80000000 0)))

(def-utility neg-ps (arg) float-sse-pack
  (xor-ps arg #.(%make-sse-pack #x8000000080000000 #x8000000080000000)))

;; Bitwise operations

(def-if-function if-ps float-sse-pack #:ps)

;; Comparisons

(def-utility >-ss (x y) float-sse-pack (<-ss y x))
(def-utility >-ps (x y) float-sse-pack (<-ps y x))
(def-utility >=-ss (x y) float-sse-pack (<=-ss y x))
(def-utility >=-ps (x y) float-sse-pack (<=-ps y x))
(def-utility />-ss (x y) float-sse-pack (/<-ss y x))
(def-utility />-ps (x y) float-sse-pack (/<-ps y x))
(def-utility />=-ss (x y) float-sse-pack (/<=-ss y x))
(def-utility />=-ps (x y) float-sse-pack (/<=-ps y x))

(def-not-cmp-pairs not-ps
    =-ps /=-ps <-ps /<-ps <=-ps /<=-ps >-ps />-ps >=-ps />=-ps cmpord-ps cmpunord-ps)

;; Shuffle

(declaim (inline %sse-pack-to-int %int-to-sse-pack %shuffle-subints))

(defun %sse-pack-to-int (pack)
  (logior (%sse-pack-low pack) (ash (%sse-pack-high pack) 64)))

(defun %int-to-sse-pack (val &aux (mask #xFFFFFFFFFFFFFFFF))
  (%make-sse-pack (logand val mask) (logand (ash val -64) mask)))

(defun %shuffle-subints (xval yval imm bit-cnt &aux (mask (1- (ash 1 bit-cnt))))
  (flet ((bits (idx)
           (logand 3 (ash imm (* -2 idx))))
         (val (src idx)
           (logand mask (ash src (* (- bit-cnt) idx)))))
    (logior (val xval (bits 0))
            (ash (val xval (bits 1)) bit-cnt)
            (ash (val yval (bits 2)) (* 2 bit-cnt))
            (ash (val yval (bits 3)) (* 3 bit-cnt)))))

(defun shuffle-ps (x y imm)
  (declare (type sse-pack x y))
  (let* ((xval (%sse-pack-to-int x))
         (yval (%sse-pack-to-int y)))
    (truly-the float-sse-pack (%int-to-sse-pack (%shuffle-subints xval yval imm 32)))))

;;; Double-float

;; Constants

(define-symbol-macro 0.0-pd (truly-the double-sse-pack #.(%make-sse-pack 0 0)))

(define-symbol-macro true-sd (truly-the double-sse-pack #.(%make-sse-pack #xFFFFFFFFFFFFFFFF 0)))
(define-symbol-macro true-pd (truly-the double-sse-pack #.(%make-sse-pack #xFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFF)))

(define-symbol-macro false-sd (truly-the double-sse-pack #.(%make-sse-pack 0 0)))
(define-symbol-macro false-pd (truly-the double-sse-pack #.(%make-sse-pack 0 0)))

;; Initialization

(declaim (inline set1-pd set-pd setr-pd setzero-pd))

(defun set1-pd (val)
  (let ((valv (set-sd val)))
    (shuffle-pd valv valv 0)))

(defun set-pd (x1 x0)
  (unpacklo-pd (set-sd x0) (set-sd x1)))

(defun setr-pd (x0 x1)
  (unpacklo-pd (set-sd x0) (set-sd x1)))

(defun setzero-pd () 0.0-pd)

;; Arithmetic negation

(def-utility neg-sd (arg) double-sse-pack
  (xor-pd arg #.(%make-sse-pack #x8000000000000000 0)))

(def-utility neg-pd (arg) double-sse-pack
  (xor-pd arg #.(%make-sse-pack #x8000000000000000 #x8000000000000000)))

;; Bitwise operations

(def-if-function if-pd double-sse-pack #:pd)

;; Comparisons

(def-utility >-sd (x y) double-sse-pack (<-sd y x))
(def-utility >-pd (x y) double-sse-pack (<-pd y x))
(def-utility >=-sd (x y) double-sse-pack (<=-sd y x))
(def-utility >=-pd (x y) double-sse-pack (<=-pd y x))
(def-utility />-sd (x y) double-sse-pack (/<-sd y x))
(def-utility />-pd (x y) double-sse-pack (/<-pd y x))
(def-utility />=-sd (x y) double-sse-pack (/<=-sd y x))
(def-utility />=-pd (x y) double-sse-pack (/<=-pd y x))

(def-not-cmp-pairs not-pd
    =-pd /=-pd <-pd /<-pd <=-pd /<=-pd >-pd />-pd >=-pd />=-pd cmpord-pd cmpunord-pd)

;; Shuffle

(defun shuffle-pd (x y imm)
  (declare (type sse-pack x y))
  (truly-the double-sse-pack
             (%make-sse-pack (if (logtest imm 1) (%sse-pack-high x) (%sse-pack-low x))
                             (if (logtest imm 2) (%sse-pack-high y) (%sse-pack-low y)))))

;;; Integer

;; Constants

(define-symbol-macro 0-pi (truly-the int-sse-pack #.(%make-sse-pack 0 0)))

(define-symbol-macro true-pi (truly-the int-sse-pack #.(%make-sse-pack #xFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFF)))

(define-symbol-macro false-pi (truly-the int-sse-pack #.(%make-sse-pack 0 0)))

;; Initialization

(macrolet ((defset (name type)
             `(defun ,name (x)
                (declare (type ,type x))
                (,name x))))
  (defset %set-int (signed-byte 64))
  (defset %set-uint (unsigned-byte 64))
  (defset convert-si32-to-pi (signed-byte 32))
  (defset convert-su32-to-pi (unsigned-byte 32))
  (defset convert-si64-to-pi (signed-byte 64))
  (defset convert-su64-to-pi (unsigned-byte 64)))

(macrolet ((defset1 (name setter type shuffle &rest expands)
             `(progn
                (export ',name)
                (declaim (inline ,name))
                (defun ,name (arg)
                  (let ((val (,setter (the ,type arg))))
                    (declare (type int-sse-pack val))
                    ,@(loop for x in expands collect `(setq val (,x val val)))
                    (shuffle-pi32 val ,shuffle))))))
  (defset1 set1-pi8  %set-int  fixnum #4r0000 unpacklo-pi8 unpacklo-pi16)
  (defset1 set1-pi16 %set-int  fixnum #4r0000 unpacklo-pi16)
  (defset1 set1-pi32 %set-int  (signed-byte 32) #4r0000)
  (defset1 set1-pu32 %set-uint (unsigned-byte 32) #4r0000)
  (defset1 set1-pi64 %set-int  (signed-byte 64) #4r1010)
  (defset1 set1-pu64 %set-uint (unsigned-byte 64) #4r1010))

(macrolet ((defset (name rname setter type depth)
             (let* ((names (loop for i from 0 below (ash 1 depth)
                              collect (symbolicate (format nil "X~A" i))))
                    (funcs #(unpacklo-pi64 unpacklo-pi32 unpacklo-pi16 unpacklo-pi8))
                    (body (loop for i downfrom depth to 0
                             for bv = (mapcar (lambda (x) `(,setter (the ,type ,x))) names)
                             then (loop for (a b) on bv by #'cddr
                                     collect `(,(svref funcs i) ,a ,b))
                             finally (return (first bv)))))
               `(progn
                  (export ',name)
                  (export ',rname)
                  (declaim (inline ,name ,rname))
                  (defun ,name (,@(reverse names)) ,body)
                  (defun ,rname (,@names) ,body)))))
  (defset set-pi8  setr-pi8  %set-int  fixnum 4)
  (defset set-pi16 setr-pi16 %set-int  fixnum 3)
  (defset set-pi32 setr-pi32 %set-int  (signed-byte 32) 2)
  (defset set-pu32 setr-pu32 %set-uint (unsigned-byte 32) 2)
  (defset set-pi64 setr-pi64 %set-int  (signed-byte 64) 1)
  (defset set-pu64 setr-pu64 %set-uint (unsigned-byte 64) 1))

(declaim (inline setzero-pi))
(defun setzero-pi () 0-pi)

;; Masked move

(export 'maskmoveu-pi)

(declaim (inline maskmoveu-pi))

(defun maskmoveu-pi (value mask pointer &optional (offset 0))
  (declare (type sse-pack value mask)
           (type system-area-pointer pointer)
           (type fixnum offset))
  (%maskmoveu-pi value mask pointer offset))

;; Arithmetic negation (subtract from 0)

(macrolet ((frob (name subf)
             `(def-utility ,name (arg) int-sse-pack (,subf 0-pi arg))))
  (frob neg-pi8  sub-pi8)
  (frob neg-pi16 sub-pi16)
  (frob neg-pi32 sub-pi32)
  (frob neg-pi64 sub-pi64))

;; Bitwise operations

(def-if-function if-pi int-sse-pack #:pi)

;; Comparisons

(def-utility <-pi8 (x y) int-sse-pack (>-pi8 y x))
(def-utility <-pi16 (x y) int-sse-pack (>-pi16 y x))
(def-utility <-pi32 (x y) int-sse-pack (>-pi32 y x))

(def-utility <=-pi8 (x y) int-sse-pack (not-pi (>-pi8 x y)))
(def-utility <=-pi16 (x y) int-sse-pack (not-pi (>-pi16 x y)))
(def-utility <=-pi32 (x y) int-sse-pack (not-pi (>-pi32 x y)))

(def-utility >=-pi8 (x y) int-sse-pack (not-pi (>-pi8 y x)))
(def-utility >=-pi16 (x y) int-sse-pack (not-pi (>-pi16 y x)))
(def-utility >=-pi32 (x y) int-sse-pack (not-pi (>-pi32 y x)))

(def-utility /=-pi8 (x y) int-sse-pack (not-pi (=-pi8 x y)))
(def-utility /=-pi16 (x y) int-sse-pack (not-pi (=-pi16 x y)))
(def-utility /=-pi32 (x y) int-sse-pack (not-pi (=-pi32 x y)))

;; Shifts

(defun slli-pi (x imm)
  (declare (type sse-pack x))
  (truly-the int-sse-pack
             (if (> imm 15)
                 0-pi
                 (%int-to-sse-pack (ash (%sse-pack-to-int x) (* 8 imm))))))

(defun srli-pi (x imm)
  (declare (type sse-pack x))
  (truly-the int-sse-pack
             (if (> imm 15)
                 0-pi
                 (%int-to-sse-pack (ash (%sse-pack-to-int x) (* -8 imm))))))

;; Extract & insert

(defun extract-pi16 (x imm)
  (declare (type sse-pack x))
  (logand #xFFFF
          (ash (%sse-pack-to-int x)
               (- (* 16 (logand imm 7))))))

(defun insert-pi16 (x intv imm)
  (declare (type sse-pack x))
  (let ((shift (* 16 (logand imm 7))))
    (truly-the int-sse-pack
               (%int-to-sse-pack
                (logior (logand (%sse-pack-to-int x)
                                (lognot (ash #xFFFF shift)))
                        (ash (logand intv #xFFFF) shift))))))

;; Shuffle

(defun shuffle-pi32 (x imm)
  (declare (type sse-pack x))
  (let* ((xval (%sse-pack-to-int x)))
    (truly-the int-sse-pack (%int-to-sse-pack (%shuffle-subints xval xval imm 32)))))

(defun shufflelo-pi16 (x imm)
  (declare (type sse-pack x))
  (let* ((xval (%sse-pack-low x)))
    (truly-the int-sse-pack (%make-sse-pack (%shuffle-subints xval xval imm 16)
                                            (%sse-pack-high x)))))

(defun shufflehi-pi16 (x imm)
  (declare (type sse-pack x))
  (let* ((xval (%sse-pack-high x)))
    (truly-the int-sse-pack (%make-sse-pack (%sse-pack-low x)
                                            (%shuffle-subints xval xval imm 16)))))

