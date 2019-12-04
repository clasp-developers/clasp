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
       (sequence-start-end ,seq ,start ,end) 
     (declare (fixnum ,start ,end ,length)
	      ,@(unless length-p `((ignorable ,length))))
     ,@body))

(defmacro reckless (&body body)
  `(locally (declare (optimize (safety 0) (speed 3) (debug 0))) ,@body))

(defmacro dovaslist ((elt vaslist &optional result) &body body)
  (once-only (vaslist)
    `(do ((,elt (core:vaslist-pop ,vaslist) (core:vaslist-pop ,vaslist)))
         (nil)
       ,@body
       (when (zerop (core:vaslist-length ,vaslist)) (return ,result)))))

(defmacro dovector ((elt sequence &optional result) &body body)
  (with-unique-names (%vector %index %limit)
    (once-only (sequence)
      `(with-array-data ((,%vector ,sequence) ,%index)
         (do ((,%limit (+ ,%index (length ,sequence)))
              (,%index ,%index (1+ ,%index)))
             ((= ,%index ,%limit) ,result)
           (let ((,elt (core:vref ,%vector ,%index)))
             ,@body))))))

(defmacro dosequence ((elt sequence &optional result) &body body)
  (once-only (sequence)
    `(cond ((listp ,sequence)
            (dolist (,elt ,sequence ,result) ,@body))
           ((vectorp ,sequence)
            (dovector (,elt ,sequence ,result) ,@body))
           (t (error-not-a-sequence sequence)))))

(defmacro do-subvector ((elt vector start end
                         &key from-end output setter (index (gensym "INDEX")))
                        &body body)
  (with-unique-names (%vector %limit %uindex %offset)
    (let ((body (if setter
                    `((macrolet ((,setter (value)
                                   `(setf (vref ,',%vector ,',%uindex) ,value)))
                        ,@body))
                    body)))
      `(with-array-data ((,%vector ,vector) ,%offset)
         ,(if from-end
              `(do ((,%uindex (+ ,%offset ,end))
                    (,%limit (+ ,%offset ,start)))
                   ((= ,%uindex ,%limit) ,output)
                 (let ((,elt (vref ,%vector (decf ,%uindex))))
                   ;; The index is bound in coordinates of the original array.
                   ;; ...but since in actual uses of do-subvector we don't
                   ;; use the index in every index, this is slightly faster.
                   (symbol-macrolet ((,index (- ,%uindex ,%offset)))
                     ,@body)))
              `(do ((,%uindex (+ ,%offset ,start) (1+ ,%uindex))
                    (,%limit (+ ,%offset ,end)))
                   ((= ,%uindex ,%limit) ,output)
                 (let ((,elt (vref ,%vector ,%uindex)))
                   (symbol-macrolet ((,index (- ,%uindex ,%offset)))
                     ,@body))))))))

(defmacro do-sublist ((elt list start end &key output
                       setter (index (gensym)))
                      &body body)
  (with-unique-names (%sublist %count)
    (let ((body (if setter
                    `((macrolet ((,setter (value)
                                   `(reckless (rplaca ,',%sublist ,value))))
                        ,@body))
                    body)))
    `(do* ((,index ,start (1+ ,index))
           (,%sublist (nthcdr ,index ,list) (cdr ,%sublist))
           (,%count (- ,end ,index) (1- ,%count)))
          ((<= ,%count 0) ,output)
       (declare (fixnum ,index ,%count)
                (cons ,%sublist))
       (let ((,elt (car ,%sublist)))
         ,@body)))))

(defmacro do-subsequence ((elt sequence start end &rest args
                           &key setter index output specialize)
                          &body body)
  (with-unique-names (%sequence)
    (let ((args (list* elt %sequence start end args)))
      `(let ((,%sequence ,sequence))
         (cond ((listp ,%sequence)
                (do-sublist ,args ,@body))
               ((vectorp ,%sequence)
                (do-subvector ,args ,@body))
               (t (error-not-a-sequence ,%sequence)))))))

(defmacro do-sequences ((elt-list seq-list &key output) &body body)
  (with-unique-names (%iterators %sequences)
    `(do* ((,%sequences ,seq-list)
           (,%iterators (mapcar #'make-seq-iterator ,%sequences))
           ;; Initial elements of this list don't actually matter
           ;; as it is mutated by seq-iterator-list-pop.
           (,elt-list (copy-list ,%sequences)))
          ((null (setf ,elt-list
                       (seq-iterator-list-pop ,elt-list
                                              ,%sequences
                                              ,%iterators)))
           ,@(and output (list output)))
       ,@body)))

