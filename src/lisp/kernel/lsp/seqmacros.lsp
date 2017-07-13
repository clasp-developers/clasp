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
                                `(reckless (si::aset ,',%vector
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

;;; The following code is complicated but we do not have LOOP, so we use MAPCAR
;;; in places where I would prefer LOOP. It turned out okay though, I think.

(defun gensym-list (list &optional x)
  (if x
      (mapcar (lambda (y) (declare (ignore y)) (gensym x)) list)
      (mapcar (lambda (y) (declare (ignore y)) (gensym)) list)))

;;; Somewhat harder to understand version of do-sequences
;;; where the list of sequences is known at compile time.
;;; CALLER is a symbol. It is bound to a function of one argument,
;;; a function. CALLER calls this function with elements of the
;;; sequences as arguments.
;;; So, (do-static-sequences (c (list 1 2 3) (list 4 5)) (c #'print))
;;; will print 5, then 7, then stop.
(defmacro do-static-sequences ((caller &rest sequences) &body body)
  (let ((seqs (gensym-list sequences "SEQUENCE"))
        (iters (gensym-list sequences "ITERATOR")))
    `(block nil
       (let (,@(mapcar #'list seqs sequences))
         (do (,@(mapcar (lambda (s i)
                          `(,i (make-seq-iterator ,s) (seq-iterator-next ,s ,i)))
                        seqs iters))
             ((or ,@(mapcar (lambda (s i) `(seq-iterator-endp ,s ,i)) seqs iters)))
           ;; We should just have a local function, but as of July 2017 we do very
           ;; badly at eliminating unneeded closures.
           (macrolet ((,caller (fun)
                        (list 'funcall fun ,@(mapcar (lambda (s i) `'(seq-iterator-ref ,s ,i))
                                                     seqs iters))))
             (tagbody ,@body))
           #+(or)
           (flet ((,caller (fun)
                    (funcall fun ,@(mapcar (lambda (s i) `(seq-iterator-ref ,s ,i))
                                           seqs iters))))
             (declare (inline ,caller))
             (tagbody ,@body)))))))

(define-compiler-macro map-for-effect
    (function sequence &rest more-sequences)
  (let* ((fun (gensym "FUNCTION")))
    `(let ((,fun (coerce-fdesignator ,function)))
       (do-static-sequences (call ,sequence ,@more-sequences)
         (call ,fun))
       nil)))

(define-compiler-macro map
    (&whole form result-type function sequence &rest more-sequences &environment env)
  (declare (ignore env))
  (if (constantp result-type)
      (let ((result-type (eval result-type))) ; constant-form-value
        (if result-type
            (let* ((fun (gensym "FUNCTION"))
                   (output (gensym "OUTPUT"))
                   (output-iter (gensym "OUTPUT-ITER"))
                   (sequences (cons sequence more-sequences))
                   (seqs (gensym-list sequences "SEQUENCE")))
              ;; We might turn this into an assertion in later stages.
              `(the (values ,result-type &rest nil)
                    ;; this is basically MAP-INTO, except we don't bother checking
                    ;; for the end of output iteration.
                    (let* ((,fun (coerce-fdesignator ,function))
                           ;; Have to (redundantly) once-only these because
                           ;; we need the lengths.
                           ,@(mapcar #'list seqs sequences)
                           (,output (make-sequence
                                     ',result-type
                                     (min ,@(mapcar (lambda (s) `(length ,s)) seqs))))
                           (,output-iter (make-seq-iterator ,output)))
                      (do-static-sequences (call ,@seqs)
                        (seq-iterator-set ,output ,output-iter (call ,fun))
                        (setq ,output-iter (seq-iterator-next ,output ,output-iter)))
                      ,output)))
            `(map-for-effect ,function ,sequence ,@more-sequences)))
      form))

;;; MAP-INTO has special behavior on vectors with fill pointers, so we specialize.
(defmacro map-into-usual (output fun &rest seqs)
  (let ((output-iter (gensym "OUTPUT-ITERATOR")))
    `(let ((,output-iter (make-seq-iterator ,output)))
       (do-static-sequences (call ,@seqs)
         (when (seq-iterator-endp ,output ,output-iter) (return))
         (seq-iterator-set ,output ,output-iter (call ,fun))
         (setq ,output-iter (seq-iterator-next ,output ,output-iter))))))

(defmacro map-into-fp (output fun &rest seqs)
  (let ((output-index (gensym "OUTPUT-INDEX"))
        (output-size (gensym "OUTPUT-SIZE")))
    `(let ((,output-index 0)
           (,output-size (array-dimension ,output 0)))
       (do-static-sequences (call ,@seqs)
         (when (= ,output-index ,output-size) (return))
         (setf (aref ,output ,output-index) (call ,fun))
         (incf ,output-index))
       (setf (fill-pointer ,output) ,output-index))))

(define-compiler-macro map-into (result function &rest sequences)
  ;; handle multiple evaluation up here
  (let ((output (gensym "OUTPUT"))
        (fun (gensym "FUNCTION"))
        (seqs (gensym-list sequences "SEQUENCE")))
    `(let ((,output ,result)
           (,fun (coerce-fdesignator ,function))
           ,@(mapcar #'list seqs sequences))
       (if (and (vectorp ,output) (array-has-fill-pointer-p, output))
           (map-into-fp ,output ,fun ,@seqs)
           (map-into-usual ,output ,fun ,@seqs))
       ,output)))
