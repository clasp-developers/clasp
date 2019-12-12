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

(defmacro sequence:with-sequence-iterator
    ((&whole vars
      &optional iterator limit from-end-v
        step endp element set-element index copy)
     (sequence &key from-end (start 0) end)
     &body body)
  (declare (ignore iterator limit from-end-v
                   step endp element set-element index copy))
  (let* ((ignored nil)
         (vars (mapcar (lambda (v)
                         (or v
                             (let ((name (gensym)))
                               (push name ignored)
                               name)))
                       vars)))
    `(multiple-value-bind (,@vars)
         (%make-sequence-iterator ,sequence ,from-end ,start ,end)
       (declare (type function ,@(nthcdr 3 vars))
                (ignore ,@ignored))
       ,@body)))

#+(or)
(defmacro sequence:with-sequence-iterator-macros
    ((step endp elt setf index copy)
     (sequence &rest args &key from-end start end)
     &body body)
  (declare (ignore from-end start end))
  (let ((nstate (gensym "STATE")) (nlimit (gensym "LIMIT"))
        (nfrom-end (gensym "FROM-END")) (nstep (gensym "STEP"))
        (nendp (gensym "ENDP")) (nelt (gensym "ELT"))
        (nsetf (gensym "SETF")) (nindex (gensym "INDEX"))
        (ncopy (gensym "COPY")))
    `(sequence:with-sequence-iterator
         (,nstate ,nlimit ,nfrom-end
                  ,nstep ,nendp ,nelt ,nsetf ,nindex ,ncopy)
         (,sequence ,@args)
       (macrolet ((,step ()
                    (list 'setq ,nstate
                          (list 'funcall ,nstep ,sequence ,nstate ,nfrom-end)))
                  (,endp ()
                    (list 'funcall ,nendp ,sequence ,nstate ,nlimit ,nfrom-end))
                  (,elt () (list 'funcall ,nelt ,sequence ,nstate))
                  (,setf (new-value)
                    (list 'funcall ,nsetf new-value ,sequence ,nstate))
                  (,index () (list 'funcall ,nindex ,sequence ,nstate))
                  (,copy () (list 'funcall ,ncopy ,sequence ,nstate)))
         ,@body))))

(defmacro dosequence-general ((elt sequence &optional result) &body body)
  (with-unique-names (%it %limit %from-end %step %endp %elt)
    (once-only (sequence)
      `(sequence:with-sequence-iterator
           (,%it ,%limit ,%from-end ,%step ,%endp ,%elt)
           (,sequence)
         (do ((,%it ,%it (reckless
                          (funcall ,%step ,sequence ,%it ,%from-end))))
             ((reckless
               (funcall ,%endp ,sequence ,%it ,%limit ,%from-end))
              ,result)
           (let ((,elt (reckless (funcall ,%elt ,sequence ,%it))))
             (tagbody ,@body)))))))

;;; NOTE: Might want to consider when we want a triplified body
;;; and when we don't with some more care.
(defmacro sequence:dosequence ((elt sequence &optional result) &body body)
  (once-only (sequence)
    `(cond ((listp ,sequence)
            (dolist (,elt ,sequence ,result) ,@body))
           ((vectorp ,sequence)
            (dovector (,elt ,sequence ,result) ,@body))
           (t
            (dosequence-general (,elt ,sequence ,result) ,@body)))))

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

(defmacro do-general-subsequence ((elt sequence start end
                                   &key output setter
                                     from-end (index nil indexp))
                                  &body body)
  (with-unique-names (%it %limit %from-end %step %endp %elt %set)
    (let ((body (if setter
                    `((macrolet ((,setter (value)
                                   `(reckless
                                     (funcall ,',%set
                                              ,value ,',sequence ,',%it))))
                        ,@body))
                    body)))
      (once-only (start)
        `(sequence:with-sequence-iterator (,%it ,%limit ,%from-end
                                                ,%step ,%endp ,%elt ,%set)
             (,sequence :start ,start :end ,end :from-end ,from-end)
           (do (,@(when indexp `((,index ,start (1+ ,index))))
                (,%it ,%it (reckless
                            (funcall ,%step ,sequence ,%it ,%from-end))))
               ((reckless (funcall ,%endp ,sequence ,%it ,%limit ,%from-end))
                ,output)
             ,@(when indexp `((declare (fixnum ,index))))
             (let (,@(when elt `((,elt (reckless
                                        (funcall ,%elt ,sequence ,%it))))))
               ,@body)))))))

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
               (t
                (do-general-subsequence ,args ,@body)))))))

;;; Iterate over a variable number of sequences at once.
;;; Once any sequence runs out of elements, OUTPUT is evaluated and returned.
;;; SEQ-LIST is evaluated: its value must be a list of (proper) sequences.
;;; ELT-LIST is a symbol. In the body, it will be bound to a list of the
;;;  same length, containing the elements of the sequences for this iter.
;;; Iterating over a variable number of lists requires consing and should
;;; be avoided as far as is practical.
(defmacro do-sequence-list ((elt-list seq-list &optional output) &body body)
  (with-unique-names (%sequences %iterators)
    `(let ((,%sequences ,seq-list))
       (multiple-value-bind (,elt-list ,%iterators)
           (lists-for-do-sequence-list ,%sequences)
         (do ()
             ((not (seq-iterator-list-pop ,elt-list ,%sequences ,%iterators))
              ,@(and output (list output)))
           ,@body)))))
