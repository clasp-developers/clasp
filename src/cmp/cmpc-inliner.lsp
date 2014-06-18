;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;; CMPC-INLINER -- Open coding functions as C expressions
;;;;

(in-package "COMPILER")

(defun inlined-arg-loc (arg)
  (second arg))

(defun inlined-arg-type (arg)
  (first arg))

(defun inlined-arg-rep-type (arg)
  (loc-representation-type (second arg)))

(defmacro define-c-inliner (fname lambda-list &body body)
  `(setf (gethash ',fname *cinline-dispatch-table*)
	 #'(lambda ,lambda-list (block nil ,@body))))

(defun apply-inliner (fname return-type inlined-args)
  (let ((fd (gethash fname *cinline-dispatch-table*)))
    (if fd
	(apply fd return-type inlined-args)
	(default-c-inliner fname return-type inlined-args))))

(defun default-c-inliner (fname return-type inlined-args)
  (let* ((arg-types (mapcar #'first inlined-args))
         (ii (inline-function fname arg-types return-type)))
    (and ii (apply-inline-info ii inlined-args))))

;;;
;;; inline-function:
;;;   locs are typed locs as produced by inline-args
;;;   returns NIL if inline expansion of the function is not possible
;;;
(defun inline-function (fname arg-types return-type &optional (return-rep-type 'any))
  ;; Those functions that use INLINE-FUNCTION must rebind
  ;; the variable *INLINE-BLOCKS*.
  (and (inline-possible fname)
       (not (gethash fname *c2-dispatch-table*))
       (let* ((dest-rep-type (loc-representation-type *destination*))
              (ii (get-inline-info fname arg-types return-type return-rep-type)))
         ii)))

(defun apply-inline-info (ii inlined-locs)
  (let* ((arg-types (inline-info-arg-types ii))
         (out-rep-type (inline-info-return-rep-type ii))
         (out-type (inline-info-return-type ii))
         (side-effects-p (function-may-have-side-effects (inline-info-name ii)))
         (fun (inline-info-expansion ii))
         (one-liner (inline-info-one-liner ii)))
    (produce-inline-loc inlined-locs arg-types (list out-rep-type)
                        fun side-effects-p one-liner)))

(defun choose-inline-info (ia ib return-type return-rep-type)
  (declare (ignore return-type))
  (cond
    ;; Only accept inliners that have the right rep type
    ((not (or (eq return-rep-type 'any)
              (eq return-rep-type :void)
              (let ((info-type (inline-info-return-rep-type ib)))
                (or (eq return-rep-type info-type)
                    ;; :bool can be coerced to any other location type
                    (eq info-type :bool)))))
     ia)
    ((null ia)
     ib)
    ;; Keep the first one, which is typically the least safe but fastest. 
    ((equal (inline-info-arg-types ia) (inline-info-arg-types ib))
     ia)
    ;; More specific?
    ((every #'type>= (inline-info-arg-types ia) (inline-info-arg-types ib))
     ib)
    ;; Keep the first one, which is typically the least safe but fastest. 
    (t
     ia)))

(defun get-inline-info (fname types return-type return-rep-type)
  (declare (si::c-local))
  (let ((output nil))
    (unless (safe-compile)
      (dolist (x (inline-information fname ':INLINE-UNSAFE))
        (let ((other (inline-type-matches x types return-type)))
          (when other
            (setf output (choose-inline-info output other return-type return-rep-type))))))
    (dolist (x (inline-information fname ':INLINE-SAFE))
      (let ((other (inline-type-matches x types return-type)))
        (when other
          (setf output (choose-inline-info output other return-type return-rep-type)))))
    (dolist (x (inline-information fname ':INLINE-ALWAYS))
      (let ((other (inline-type-matches x types return-type)))
        (when other
          (setf output (choose-inline-info output other return-type return-rep-type)))))
    (when (and (null output)
               (inline-information fname 'should-be-inlined)
               (>= (cmp-env-optimization 'speed) 1))
      (cmpwarn-style "Could not inline call to ~S ~S - performance may be degraded."
                     fname types))
    output))

(defun to-fixnum-float-type (type)
  (dolist (i '(FIXNUM DOUBLE-FLOAT SINGLE-FLOAT #+long-float LONG-FLOAT)
           nil)
    (when (type>= i type)
      (return i))))

(defun maximum-float-type (t1 t2)
  (cond ((null t1)
         t2)
        #+long-float
        ((or (eq t1 'LONG-FLOAT) (eq t2 'LONG-FLOAT))
         'LONG-FLOAT)
        ((or (eq t1 'DOUBLE-FLOAT) (eq t2 'DOUBLE-FLOAT))
         'DOUBLE-FLOAT)
        ((or (eq t1 'SINGLE-FLOAT) (eq t2 'SINGLE-FLOAT))
         'SINGLE-FLOAT)
        (T
         'FIXNUM)))

(defun inline-type-matches (inline-info arg-types return-type)
  (when (and (not (inline-info-multiple-values inline-info))
	     (member *destination* '(VALUES RETURN)))
    (return-from inline-type-matches nil))
  (let* ((rts nil)
         (number-max nil))
    ;;
    ;; Check that the argument types match those of the inline expression
    ;;
    (do* ((arg-types arg-types (cdr arg-types))
	  (types (inline-info-arg-types inline-info) (cdr types)))
         ((or (endp arg-types) (endp types))
          (when (or arg-types types)
            (return-from inline-type-matches nil)))
      (let* ((arg-type (first arg-types))
             (type (first types)))
        (cond ((eq type 'FIXNUM-FLOAT)
               (let ((new-type (to-fixnum-float-type arg-type)))
                 (unless new-type
                   (return-from inline-type-matches nil))
                 (push new-type rts)
                 (setq number-max (maximum-float-type number-max new-type))))
              #+sse2
              ;; Allow implicit casts between SSE subtypes to kick in
              ((and (type>= 'ext:sse-pack type)
                    (type>= 'ext:sse-pack arg-type))
               (push type rts))
              ((type>= type arg-type)
               (push type rts))
              (t (return-from inline-type-matches nil)))))
    ;;
    ;; Now there is an optional check of the return type. This check is
    ;; only used when enforced by the inliner.
    ;;
    (when (or (eq (inline-info-return-rep-type inline-info) :bool)
              (null (inline-info-exact-return-type inline-info))
              (and (policy-assume-right-type)
                   (let ((inline-return-type (inline-info-return-type inline-info)))
                     (if number-max
                         ;; for arithmetic operators we take the maximal
                         ;; type as possible result type. Note that FIXNUM
                         ;; is not an option, because the product, addition
                         ;; or difference of fixnums may be a larger
                         ;; integer.
                         (and (setf number-max (if (eq number-max 'fixnum)
                                                   'integer
                                                   number-max))
                              (type>= inline-return-type number-max)
                              (type>= number-max return-type))
                         ;; no contravariance
                         (type>= inline-return-type return-type)))))
      (let ((inline-info (copy-structure inline-info)))
        (setf (inline-info-arg-types inline-info)
              (nreverse rts))
        inline-info))))

