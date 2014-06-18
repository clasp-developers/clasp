;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;; CMPCT --  Optimizer for several constant values

;;;;  Copyright (c) 2003, Juan Jose Garcia Ripoll.
;;;;
;;;;    ECoLisp is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

(defparameter +optimizable-constants+ '())

(defun c1constant-value (val &key always only-small-values)
  (cond
   ((let ((x (assoc val *optimizable-constants*)))
      (when x
       (pushnew "#include <float.h>" *clines-string-list*)
       (setf x (cdr x))
       (if (listp x)
           (c1expr x)
           x))))
   ((eq val nil) (c1nil))
   ((eq val t) (c1t))
   ((sys::fixnump val)
    (make-c1form* 'LOCATION :type 'FIXNUM :args (list 'FIXNUM-VALUE val)))
   ((characterp val)
    (make-c1form* 'LOCATION :type 'CHARACTER
		  :args (list 'CHARACTER-VALUE (char-code val))))
   ((typep val 'DOUBLE-FLOAT)
    (when (and (ext:float-nan-p val) (not only-small-values))
      (cmperr "Cannot externalize value ~A" val))
    (make-c1form* 'LOCATION :type 'DOUBLE-FLOAT
		  :args (list 'DOUBLE-FLOAT-VALUE val (add-object val))))
   ((typep val 'SINGLE-FLOAT)
    (when (and (ext:float-nan-p val) (not only-small-values))
      (cmperr "Cannot externalize value ~A" val))
    (make-c1form* 'LOCATION :type 'SINGLE-FLOAT
		  :args (list 'SINGLE-FLOAT-VALUE val (add-object val))))
   ((typep val 'LONG-FLOAT)
    (when (and (ext:float-nan-p val) (not only-small-values))
      (cmperr "Cannot externalize value ~A" val))
    (make-c1form* 'LOCATION :type 'LONG-FLOAT
		  :args (list 'LONG-FLOAT-VALUE val (add-object val))))
   #+sse2
   ((typep val 'EXT:SSE-PACK)
    (c1constant-value/sse val))
   (only-small-values nil)
   (always
    (make-c1form* 'LOCATION :type (object-type val)
		  :args (add-object val)))
   (t nil)))

#+sse2
(defun c1constant-value/sse (value)
  (let* ((bytes (ext:sse-pack-to-vector value '(unsigned-byte 8)))
         (elt-type (ext:sse-pack-element-type value)))
    (multiple-value-bind (wrapper rtype)
        (case elt-type
          (single-float (values "_mm_castsi128_ps" :float-sse-pack))
          (double-float (values "_mm_castsi128_pd" :double-sse-pack))
          (otherwise    (values ""                 :int-sse-pack)))
      `(c-inline () () ,rtype
		 ,(format nil "~A(_mm_setr_epi8(~{~A~^,~}))"
			  wrapper (coerce bytes 'list))
		 :one-liner t :side-effects nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; KNOWN OPTIMIZABLE CONSTANTS
;;;

(defun make-single-constant-optimizer (name c-value)
  (cond ((symbolp name)
	 (let* ((value (symbol-value name))
		(type (lisp-type->rep-type (type-of value))))
	   (cons value `(c-inline () () ,type ,c-value
				  :one-liner t :side-effects nil))))
	((floatp name)
	 (let* ((value name)
                      (type (type-of value))
		(loc-type (case type
			    (single-float 'single-float-value)
			    (double-float 'double-float-value)
			    (long-float 'long-float-value)))
		(location (make-vv :location c-value :value value)))
	   (cons value (make-c1form* 'LOCATION :type type
				     :args (list loc-type value location)))))
	(t
	 (cons name (make-c1form* 'LOCATION :type (type-of name)
				  :args (make-vv :location c-value
						 :value name))))))

(defun make-optimizable-constants (machine)
  (loop for (value name) in (optimizable-constants-list machine)
     collect (make-single-constant-optimizer value name)))

(defun optimizable-constants-list (machine)
  (append
   ;; Constants that appear everywhere
   '(
     ;; Order is important: on platforms where 0.0 and -0.0 are the same
     ;; the last one is prioritized.
     (#.(coerce 0 'single-float) "cl_core.singlefloat_zero")
     (#.(coerce 0 'double-float) "cl_core.doublefloat_zero")
     (#.(coerce -0.0 'single-float) "cl_core.singlefloat_minus_zero")
     (#.(coerce -0.0 'double-float) "cl_core.doublefloat_minus_zero")
     #+long-float
     (#.(coerce 0 'long-float) "cl_core.longfloat_zero")
     #+long-float
     (#.(coerce -0.0 'long-float) "cl_core.longfloat_minus_zero")

     ;; We temporarily remove this constant, because the bytecodes compiler
     ;; does not know how to externalize it.
     ;;(#.(si::standard-readtable) "cl_core.standard_readtable")

     (#.(find-package :cl) "cl_core.lisp_package")
     (#.(find-package :cl-user) "cl_core.user_package")
     (#.(find-package :keyword) "cl_core.keyword_package")
     (#.(find-package :clos) "cl_core.clos_package")
     #+threads
     (#.(find-package :mp) "cl_core.mp_package")
     )
   (when (eq machine +default-machine+)
     ;; Constants which are not portable
     `((MOST-POSITIVE-SHORT-FLOAT "FLT_MAX")
       (MOST-POSITIVE-SINGLE-FLOAT "FLT_MAX")
       
       (MOST-NEGATIVE-SHORT-FLOAT "-FLT_MAX")
       (MOST-NEGATIVE-SINGLE-FLOAT "-FLT_MAX")

       (LEAST-POSITIVE-SHORT-FLOAT "FLT_MIN")
       (LEAST-POSITIVE-SINGLE-FLOAT "FLT_MIN")
       (LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT "FLT_MIN")
       (LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT" FLT_MIN")

       (LEAST-NEGATIVE-SHORT-FLOAT "-FLT_MIN")
       (LEAST-NEGATIVE-SINGLE-FLOAT "-FLT_MIN")
       (LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT "-FLT_MIN")
       (LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT "-FLT_MIN")

       (MOST-POSITIVE-DOUBLE-FLOAT "DBL_MAX")
       (MOST-NEGATIVE-DOUBLE-FLOAT "-DBL_MAX")
       (LEAST-POSITIVE-DOUBLE-FLOAT "DBL_MIN")
       (LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT "DBL_MIN")
       (LEAST-NEGATIVE-DOUBLE-FLOAT "-DBL_MIN")
       (LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT "-DBL_MIN")

       #+long-float
       ,@'(
	   (MOST-POSITIVE-LONG-FLOAT "LDBL_MAX")
	   (MOST-NEGATIVE-LONG-FLOAT "-LDBL_MAX")
	   (LEAST-POSITIVE-LONG-FLOAT "LDBL_MIN")
	   (LEAST-POSITIVE-NORMALIZED-LONG-FLOAT" LDBL_MIN")
	   (LEAST-NEGATIVE-LONG-FLOAT "-LDBL_MIN")
	   (LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT "-LDBL_MIN")
	   )))))
