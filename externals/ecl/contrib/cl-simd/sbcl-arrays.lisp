;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;
;;; Copyright (c) 2010, Alexander Gavrilov (angavrilov@gmail.com)
;;;
;;; This file contains the groundwork for vectorized
;;; array access intrinsics.
;;;

(in-package #:SSE)

;; SSE array element size calculation

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun sse-elt-shift-from-saetp (info)
    (and info
         (subtypep (saetp-specifier info) 'number)
         (not (saetp-fixnum-p info))
         (case (saetp-n-bits info)
           (8 0) (16 1) (32 2) (64 3) (128 4)))))

(defglobal %%size-shift-table%%
    (let ((arr (make-array (1+ widetag-mask) :initial-element nil)))
      (loop
         for info across *specialized-array-element-type-properties*
         for shift = (sse-elt-shift-from-saetp info)
         when shift
         do (setf (svref arr (saetp-typecode info)) shift))
      arr)
  "A table of element size shifts for supported SSE array types.")

(declaim (inline sse-elt-shift-of)
         (ftype (function (t) (integer 0 4)) sse-elt-shift-of))

(defun sse-elt-shift-of (obj)
  "Returns the SSE element size shift for the given object,
or fails if it is not a valid SSE vector."
  (declare (optimize (safety 0)))
  (the (integer 0 4)
    (or (svref %%size-shift-table%%
               (if (sb-vm::%other-pointer-p obj)
                   (%other-pointer-widetag obj)
                   0))
        (error 'type-error
               :datum obj
               :expected-type 'sse-array))))

;;; Type and allocation

(deftype sse-array (&optional (elt-type '* et-p) dims)
  "Type of arrays efficiently accessed by SSE aref intrinsics and returned by make-sse-array.
Should be assumed to be SIMPLE-ARRAY, except that displacing with MAKE-SSE-ARRAY is allowed."
  (if (eq elt-type '*)
      (progn
        (when et-p
          (error "SSE-ARRAY must have a specific element type."))
        `(simple-array * ,dims))
      (let* ((upgraded (upgraded-array-element-type elt-type))
             (shift (sse-elt-shift-from-saetp (find-saetp upgraded))))
        (when (null shift)
          (error "Invalid SSE-ARRAY element type: ~S" elt-type))
        (unless (subtypep upgraded elt-type)
          (warn "SSE-ARRAY element type ~S has been upgraded to ~S" elt-type upgraded))
        `(simple-array ,upgraded ,dims))))

(defun make-sse-array (dimensions &key (element-type '(unsigned-byte 8)) (initial-element nil ie-p) displaced-to (displaced-index-offset 0))
  "Allocates an SSE-ARRAY aligned to the 16-byte boundary. Flattens displacement chains for performance reasons."
  (let* ((upgraded (upgraded-array-element-type element-type))
         (shift (sse-elt-shift-from-saetp (find-saetp upgraded))))
    (when (null shift)
      (error "Invalid SSE-ARRAY element type: ~S" element-type))
    (if displaced-to
        ;; Fake displacement by allocating a simple-array header
        (let* ((dimensions (if (listp dimensions) dimensions (list dimensions)))
               (rank (length dimensions))
               (count (reduce #'* dimensions)))
          (unless (subtypep element-type (array-element-type displaced-to))
            (error "can't displace an array of type ~S into another of type ~S"
                   element-type (array-element-type displaced-to)))
          (with-array-data ((data displaced-to)
                            (start displaced-index-offset)
                            (end))
            (unless (= start 0)
              (error "SSE-ARRAY does not support displaced index offset."))
            (unless (<= count end)
              (array-bounding-indices-bad-error data start count))
            (if (= rank 1)
                (progn
                  (when (< count end)
                    (warn "SSE-ARRAY displaced size extended to the full length of the vector."))
                  data)
                (let ((new-array (make-array-header simple-array-widetag rank)))
                  (set-array-header new-array data count nil 0 dimensions nil t)))))
        ;; X86-64 vectors are already aligned to 16 bytes
        (apply #'make-array dimensions :element-type upgraded
               (if ie-p (list :initial-element initial-element))))))

;;; AREF intrinsic definition helpers

(defconstant +vector-data-fixup+
  (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
  "Offset from a tagged vector pointer to its data")

(defmacro array-data-expr (array-var &optional is-vector)
  (ecase is-vector
    (:yes array-var)
    (:no `(%array-data-vector ,array-var))
    ((nil)
     `(if (array-header-p ,array-var)
          (%array-data-vector ,array-var)
          ,array-var))))

;; Depends on the vector-length field being in the same place
;; as the array fill pointer, which for simple-array is equal
;; to the total size.
(defknown %sse-array-size (simple-array fixnum) array-total-size (flushable always-translatable))

(define-vop (%sse-array-size/0)
  (:translate %sse-array-size)
  (:args (array :scs (descriptor-reg)))
  (:arg-types * (:constant (integer 0 0)))
  (:info gap)
  (:ignore gap)
  (:policy :fast-safe)
  (:results (result :scs (any-reg)))
  (:result-types tagged-num)
  (:generator 3
    (loadw result array vector-length-slot other-pointer-lowtag)))

(define-vop (%sse-array-size %sse-array-size/0)
  (:arg-types * (:constant (integer 1 16)))
  (:ignore)
  (:temporary (:sc any-reg) tmp)
  (:generator 8
    (loadw result array vector-length-slot other-pointer-lowtag)
    (inst mov tmp (fixnumize gap))
    (inst cmp result tmp)
    (inst cmov :ng tmp result)
    (inst sub result tmp)))

(defmacro with-sse-data (((sap-var data-var array) (offset-var index)) &body code)
  ;; Compute a SAP and offset for the specified array and index. Check bounds.
  (with-unique-names (data-index data-end elt-shift access-size)
    (once-only ((array array)
                (index index))
      `(locally
           (declare (optimize (insert-array-bounds-checks 0)))
         (with-array-data ((,data-var ,array)
                           (,data-index ,index)
                           (,data-end))
           (let* ((,sap-var (int-sap (get-lisp-obj-address ,data-var)))
                  (,elt-shift (sse-elt-shift-of ,data-var))
                  (,access-size (ash 16 (- ,elt-shift)))
                  (,offset-var (+ (ash ,data-index ,elt-shift) +vector-data-fixup+)))
             (declare (type system-area-pointer ,sap-var)
                      (type fixnum ,offset-var))
             (unless (<= 0 ,data-index (+ ,data-index ,access-size) ,data-end)
               (array-bounding-indices-bad-error ,array ,index (+ ,index ,access-size)))
             ,@code))))))

(defun sse-array-info-or-give-up (lvar ref-size)
  ;; Look up the SSE element size and check if it is definitely a vector
  (let ((type (lvar-type lvar)))
    (unless (and (array-type-p type)
                 (not (array-type-complexp type)))
      (give-up-ir1-transform "not a simple array"))
    (let* ((etype (array-type-specialized-element-type type))
           (shift (sse-elt-shift-from-saetp
                   (if (eq etype *wild-type*) nil
                       (find-saetp-by-ctype etype)))))
      (unless shift
        (give-up-ir1-transform "not a known SSE-compatible array element type: ~S"
                               (type-specifier etype)))
      (values (ash 1 shift)                 ; step
              (ash (1- ref-size) (- shift)) ; gap
              (and (listp (array-type-dimensions type))
                   (if (null (cdr (array-type-dimensions type))) :yes :no))))))

(defmacro def-aref-intrinsic (postfix rtype reader writer &key (ref-size 16))
  (let* ((rm-aref (symbolicate "ROW-MAJOR-AREF-" postfix))
         (rm-aset (if writer (symbolicate "ROW-MAJOR-ASET-" postfix)))
         (aref (symbolicate "AREF-" postfix))
         (aset (if writer (symbolicate "%ASET-" postfix)))
         (reader-vop (symbolicate "%" reader))
         (reader/ix-vop (symbolicate "%" reader "/IX"))
         (writer-vop (if writer (symbolicate "%" writer)))
         (writer/ix-vop (if writer (symbolicate "%" writer "/IX")))
         (rtype (or rtype '(values)))
         (index-expression
          (if (= ref-size 0)
              ``(the signed-word index)
              ``(the signed-word (%check-bound array (%sse-array-size array ,gap) index)))))
    `(progn
       ;; ROW-MAJOR-AREF
       (export ',rm-aref)
       (defknown ,rm-aref (array index) ,rtype (foldable flushable))
       (defun ,rm-aref (array index)
         (with-sse-data ((sap data array)
                         (offset index))
           (,reader-vop sap offset 1 0)))
       ;;
       (deftransform ,rm-aref ((array index) (simple-array t) * :important t)
         ,(format nil "open-code ~A" rm-aref)
         (multiple-value-bind (step gap is-vector) (sse-array-info-or-give-up array ,ref-size)
           (declare (ignorable gap))
           `(,',reader/ix-vop (array-data-expr array ,is-vector)
                              ,,index-expression
                              ,step ,+vector-data-fixup+)))
       ;; AREF
       (export ',aref)
       (defknown ,aref (array &rest index) ,rtype (foldable flushable))
       (defun ,aref (array &rest indices)
         (declare (truly-dynamic-extent indices))
         (with-sse-data ((sap data array)
                         (offset (%array-row-major-index array indices)))
           (,reader-vop sap offset 1 0)))
       ;;
       (defoptimizer (,aref derive-type) ((array &rest indices) node)
         (assert-array-rank array (length indices))
         (values-specifier-type ',rtype))
       (deftransform ,aref ((array &rest indices) (simple-array &rest t) * :important t)
         ,(format nil "open-code ~A" aref)
         (multiple-value-bind (step gap is-vector) (sse-array-info-or-give-up array ,ref-size)
           (declare (ignorable gap))
           (let ((syms (make-gensym-list (length indices))))
             `(lambda (array ,@syms)
                (let ((index ,(if (eq is-vector :yes) (first syms)
                                  `(array-row-major-index array ,@syms))))
                  (,',reader/ix-vop (array-data-expr array ,is-vector)
                                    ,,index-expression
                                    ,step ,+vector-data-fixup+))))))
       ,@(if writer
             `(;; ROW-MAJOR-ASET
               (defknown ,rm-aset (array index sse-pack) ,rtype (unsafe))
               (defsetf ,rm-aref ,rm-aset)
               (defun ,rm-aset (array index new-value)
                 (with-sse-data ((sap data array)
                                 (offset index))
                   (,writer-vop sap offset 1 0 (the ,rtype new-value))
                   new-value))
               ;;
               (deftransform ,rm-aset ((array index value) (simple-array t t) * :important t)
                 ,(format nil "open-code ~A" rm-aset)
                 (multiple-value-bind (step gap is-vector) (sse-array-info-or-give-up array ,ref-size)
                   (declare (ignorable gap))
                   `(progn
                      (,',writer/ix-vop (array-data-expr array ,is-vector)
                                        ,,index-expression
                                        ,step ,+vector-data-fixup+
                                        (the sse-pack value))
                      value)))
               ;; %ASET
               (defknown ,aset (array &rest t) ,rtype (unsafe))
               (defsetf ,aref ,aset)
               (defun ,aset (array &rest stuff)
                 (let ((new-value (car (last stuff))))
                   (with-sse-data ((sap data array)
                                   (offset (%array-row-major-index array (nbutlast stuff))))
                     (,writer-vop sap offset 1 0 (the ,rtype new-value))
                     new-value)))
               ;;
               (defoptimizer (,aset derive-type) ((array &rest stuff) node)
                 (assert-array-rank array (1- (length stuff)))
                 (assert-lvar-type (car (last stuff)) (specifier-type 'sse-pack)
                                   (lexenv-policy (node-lexenv node)))
                 (specifier-type ',rtype))
               (deftransform ,aset ((array &rest stuff) (simple-array &rest t) * :important t)
                 ,(format nil "open-code ~A" aset)
                 (multiple-value-bind (step gap is-vector) (sse-array-info-or-give-up array ,ref-size)
                   (declare (ignorable gap))
                   (let ((syms (make-gensym-list (length stuff))))
                     `(lambda (array ,@syms)
                        (let ((index ,(if (eq is-vector :yes) (first syms)
                                          `(array-row-major-index array ,@(butlast syms)))))
                          (,',writer/ix-vop (array-data-expr array ,is-vector)
                                            ,,index-expression
                                            ,step ,+vector-data-fixup+
                                            (the sse-pack ,(car (last syms)))))
                        ,(car (last syms)))))))))))

