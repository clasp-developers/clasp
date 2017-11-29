;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;;  SEQMACROS -- Macros that are used to expand sequence routines
;;;;

(in-package "SYSTEM")

(defmacro with-count ((count &optional (value count) &key (output nil output-p))
                      &body body)
  (setf body `(locally ,@body))
  `(let ((,count (sequence-count ,value)))
     (declare (fixnum ,count))
     ,(if output-p
	  `(if (plusp ,count)
	       ,body
	       ,output)
	  body)))

(defmacro with-predicate ((predicate) &body body)
  `(let ((,predicate (coerce-fdesignator ,predicate)))
     (declare (function ,predicate))
     (macrolet ((,predicate (&rest args)
		  `(locally (declare (optimize (safety 0) (speed 3)))
		     (funcall ,',predicate ,@args))))
       ,@body)))

(defmacro with-key ((akey) &body body)
  `(let ((,akey (if ,akey (coerce-fdesignator ,akey) #'identity)))
     (declare (function ,akey))
     (macrolet ((,akey (value)
		  `(locally (declare (optimize (safety 0) (speed 3)))
		     (funcall ,',akey ,value))))
       ,@body)))

(defmacro with-tests (&whole whole (test test-not &optional key) &body body)
  (with-unique-names (%test %test-not %test-fn)
    `(let* ((,%test ,test)
	    (,%test-not ,test-not)
	    (,%test-fn (if ,%test
			  (progn (when ,%test-not (test-error))
				 (coerce-fdesignator ,%test))
			  (if ,%test-not
			      (coerce-fdesignator ,%test-not)
			      #'eql))))
       (declare (function ,%test-fn))
       (macrolet ((compare (v1 v2)
		    `(locally (declare (optimize (safety 0) (speed 3)))
		       (if ,',%test-not
			   (not (funcall ,',%test-fn ,v1 ,v2))
			   (funcall ,',%test-fn ,v1 ,v2)))))
	 ,@(if key `((with-key (,key) ,@body)) body)))))

(defmacro with-start-end ((start end seq &optional (length (gensym) length-p))
                          &body body)
  `(multiple-value-bind (,start ,end ,length)
       (sequence-start-end 'subseq ,seq ,start ,end) 
     (declare (fixnum ,start ,end ,length)
	      ,@(unless length-p `((ignorable ,length))))
     ,@body))

(defmacro reckless (&body body)
  `(locally (declare (optimize (safety 0) (speed 3) (debug 0))) ,@body))

(defmacro do-vector ((elt vector start end
                      &key from-end output setter (index (gensym)))
                     &body body)
  (with-unique-names (%vector %count)
    (when setter
      (setf body `((macrolet ((,setter (value)
                                `(reckless (si:row-major-aset ,',%vector
                                                              ,',index
                                                              ,value))))
                     ,@body))))
    (if from-end
	`(do* ((,%vector ,vector)
	       (,index ,end)
	       (,%count ,start))
	      ((= ,index ,%count) ,output)
	   (declare (fixnum ,index ,%count)
		    (vector ,%vector))
	   (let ((,elt (reckless (aref ,%vector (setf ,index (1- ,index))))))
	     ,@body))
	`(do* ((,%vector ,vector)
	       (,index ,start (1+ ,index))
	       (,%count ,end))
	      ((= ,index ,%count) ,output)
	   (declare (fixnum ,index ,%count)
		    (vector ,%vector))
	   (let ((,elt (reckless (aref ,%vector ,index))))
	     ,@body)))))

(defmacro do-sublist ((elt list start end &key output
                       setter (index (gensym)))
                      &body body)
  (with-unique-names (%sublist %count)
    (when setter
      (setf body `((macrolet ((,setter (value)
                                `(reckless (rplaca ,',%sublist ,value))))
                     ,@body))))
    `(do* ((,index ,start (1+ ,index))
	   (,%sublist (nthcdr ,index ,list) (cdr ,%sublist))
	   (,%count (- ,end ,index) (1- ,%count)))
	  ((<= ,%count 0) ,output)
       (declare (fixnum ,index ,%count)
		(cons ,%sublist))
       (let ((,elt (car ,%sublist)))
	 ,@body))))

(defmacro do-sequence ((elt sequence start end &rest args
                       &key setter index output specialize)
		       &body body)
  (if specialize
      (with-unique-names (%sequence)
        (setf args (copy-list args))
        (remf args :specialize)
        (setf args (list* elt %sequence start end args))
	`(let ((,%sequence ,sequence))
	   (if (listp ,%sequence)
	       (do-sublist ,args ,@body)
	       (do-vector ,args ,@body))))
      (with-unique-names (%sequence %start %i %count)
	`(do* ((,%sequence ,sequence)
	       (,index ,start (1+ ,index))
	       (,%i (make-seq-iterator ,%sequence ,index)
		    (seq-iterator-next ,%sequence ,%i))
	       (,%count (- ,end ,start) (1- ,%count)))
	      ((or (seq-iterator-endp ,%sequence ,%i) (not (plusp ,%count))) ,output)
	   (let ((,elt (seq-iterator-ref ,%sequence ,%i)))
	     ,@body)))))

(defmacro do-sequences ((elt-list seq-list &key output) &body body)
  (with-unique-names (%iterators %sequences)
    `(do* ((,%sequences ,seq-list)
           (,%iterators (mapcar #'make-seq-iterator ,%sequences))
           (,elt-list (copy-list ,%sequences)))
          ((null (setf ,elt-list
                       (seq-iterator-list-pop ,elt-list
                                              ,%sequences
                                              ,%iterators)))
           ,@(and output (list output)))
       ,@body)))

