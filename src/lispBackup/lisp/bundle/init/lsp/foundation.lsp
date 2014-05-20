
;;(in-package :core)
(eval-when (eval compile load)
  (select-package :core))


(export '(T cons))

(defparameter *dump-defun-definitions* nil)
(defparameter *dump-defmacro-definitions* *dump-defun-definitions*)

(defconstant lambda-list-keywords '( &ALLOW-OTHER-KEYS
				    &AUX &BODY &ENVIRONMENT &KEY
				    &OPTIONAL &REST
				    &WHOLE) )
(export '(lambda-list-keywords))


(*fset 'fset
       #'(lambda (whole env)
	   `(*fset ,(cadr whole) ,(caddr whole) ,(cadddr whole)))
       t)

;; Temporary check-type - everything is true
(fset 'check-type 
      #'(lambda (whole env) t)
      t)
(export 'check-type)


(fset '1- #'(lambda (num) (- num 1)))
(fset '1+ #'(lambda (num) (+ num 1)))



(si::fset 'defun
	  #'(lambda (def env)
	      (let* ((name (second def))
		     (func `(function (ext::lambda-block ,@(cdr def)))))
		(ext:register-with-pde def `(si::fset ',name ,func))))
	  t)
(export '(defun))



(si::fset 'and
	  #'(lambda (whole env)
	      (let ((forms (cdr whole)))
		(if (null forms)
		    t
		    (if (null (cdr forms))
			(car forms)
			`(if ,(car forms)
			     (and ,@(cdr forms)))))))
	  t)


(si::fset 'or
	  #'(lambda (whole env)
	      (let ((forms (cdr whole)))
		(if (null forms)
		    nil
		    (if ( null (cdr forms))
			(car forms)
			(let ((tmp (gensym)))
			  `(let ((,tmp ,(car forms)))
			     (if ,tmp
				 ,tmp
				 (or ,@(cdr forms)))))))))
	  t )
(export '(and or))

(defun constantly (object)
  #'(lambda (&rest arguments) object))

(export 'constantly)

(defun simple-program-error (e1 &rest args)
  (eval `(error ,e1 ,@args)))


(fset 'return #'(lambda (whole env)
		  (let ((val (cadr whole)))
		    `(return-from nil ,val)))
      t)


#| --- loose this - its in evalmacros where ecl had it |#
#+ecl-min
(si::fset 'psetq #'(lambda (whole env)
		     "Syntax: (psetq {var form}*)
Similar to SETQ, but evaluates all FORMs first, and then assigns each value to
the corresponding VAR.  Returns NIL."
		     (let ((l (cdr whole))
			   (forms nil)
			   (bindings nil))
		       (block nil
			 (tagbody
			  top
			    (if (endp l) (return (list* 'LET* (nreverse bindings) (nreverse (cons nil forms)))))
			    (let ((sym (gensym)))
			      (push (list sym (cadr l)) bindings)
			      (push (list 'setq (car l) sym) forms))
			    (setq l (cddr l))
			    (go top)))))
	  t )

(export 'psetq)
(export '(do do*))



(fset 'when #'(lambda (def env)
		`(if ,(cadr def) (progn ,@(cddr def))))
      t)
(export 'when)


(fset 'unless #'(lambda (def env)
		  `(if ,(cadr def) nil (progn ,@(cddr def))))
      t)
(export 'unless)



(defun si::while-until (test body jmp-op)
  (declare (si::c-local))
  (let ((label (gensym))
	(exit (gensym)))
    `(TAGBODY
        (GO ,exit)
      ,label
        ,@body
      ,exit
	(,jmp-op ,test (GO ,label)))))

(fset 'si::while #'(lambda (def env)
		     (si::while-until (cadr def) (cddr def) 'when))
      t)


(fset 'si::until #'(lambda (def env)
		     (si::while-until (cadr def) (cddr def) 'unless))
      t)


(export '(until while))


(fset 'multiple-value-bind 
      #'(lambda (def env)
	  (let ((vars (cadr def))
		(multiple-value-form (caddr def))
		(body (cdddr def))
		(restname (gensym)))
	    `(multiple-value-call #'(lambda (&optional ,@(mapcar #'list vars) &rest ,restname ) ,@body) ,multiple-value-form)))
      t)
(export '(multiple-value-bind))



(fset 'cons-cdr #'(lambda (def env) `(cdr ,(cadr def))) t)


(si::fset 'truly-the
	  #'(lambda-block truly-the (args env)
			  `(the ,@(cdr args)))
	  t)


(si::fset 'prog1 #'(lambda (whole env)
		     (let ((sym (gensym))
			   (first (cadr whole))
			   (body (cddr whole)))
		       (if body
			   `(let ((,sym ,first))
			      ,@body
			      ,sym)
			   first)))
	  t)
(export 'prog1)


(*make-special '*bytecodes-compiler*)
(setq *bytecodes-compiler* nil)





;;
;; TODO: Rewrite this in C++ when you get the chance - a lot of stuff depends on it
;;  "Concatenate LISTS by changing them."

(defun nconc (&rest lists)
  (setq lists (do ((p lists (cdr p)))
		  ((or (car p) (null p)) p)))
  (do* ((top (car lists))
	(splice top)
	(here (cdr lists) (cdr here)))
      ((null here) top)
    (rplacd (last splice) (car here))
    (if (car here)
      (setq splice (car here)))))


;;
;;   "Return true if OBJECT is the same as some tail of LIST, otherwise false."
;;
(defun tailp (object list)
  (if (null list)
      (null object)
    (do ((list list (cdr list)))
	((atom (cdr list)) (or (eql object list) (eql object (cdr list))))
      (if (eql object list)
	  (return t)))))

;;
;;   "Return a copy of LIST before the part which is the same as OBJECT."
;;
(defun ldiff (list object)
  (unless (eql list object)
    (do* ((result (list (car list)))
	  (splice result)
	  (list (cdr list) (cdr list)))
	((atom list) (if (eql list object) (rplacd splice nil)) result)
      (if (eql list object)
	  (return result)
	(setq splice (cdr (rplacd splice (list (car list)))))))))

;; stuff




;; in-package macro is re-defined in evalmacros.lsp
(si::fset 'in-package #'(lambda (whole env)
			  `(eval-when (:compile-toplevel :load-toplevel :execute)
			     (si::select-package ,(string (cadr whole)))))
	  t)


(export '(in-package defmacro push pop incf decf))

;;   "Add ITEM to LIST unless it is already a member."
(defun adjoin (item list &key key (test #'eql) test-not)
  (when test-not
    (setq test (complement test-not)))
  (if (member (apply-key key item) list :key key :test test)
      list
    (cons item list)))
(export 'adjoin)






(fset 'apply-key #'(lambda (w e)
		     (let ((key (cadr w))
			   (element (caddr w)))
		       `(if ,key
			    (funcall ,key ,element)
			    ,element)))
      t)

;;
;; This is defined in ecl>>lsp>>load.d
;; It keeps track of the current source location
;; I should use it.
(defparameter *source-location* '("-no-file-" 0 . 0))


;; Required by REGISTER-GLOBAL in cmp/cmpvar.lsp
(si::fset 'pushnew #'(lambda (w e)
		       (let ((item (cadr w))
			     (place (caddr w)))
			 `(setq ,place (adjoin ,item ,place))))
	  t)



(defun hash-table-iterator (hash-table)
  (let ((number-of-hashes (hash-table-number-of-hashes hash-table))
	(hash 0)
	(cur-alist))
    (labels ((advance-hash-table-iterator ()
	       (if cur-alist
		   (progn
		     (setq cur-alist (cdr cur-alist))
		     (unless cur-alist
		       (setq hash (1+ hash))
		       (advance-hash-table-iterator)))
		   (if (< hash number-of-hashes)
		       (progn
			 (setq cur-alist (hash-table-alist-at-hash hash-table hash))
			 (unless cur-alist
			   (setq hash (1+ hash))
			   (advance-hash-table-iterator)))
		       nil))))
      (function (lambda ()
	(if (>= hash number-of-hashes)
	    nil
	    (progn
	      (advance-hash-table-iterator)
	      (when (< hash number-of-hashes)
		(values t (caar cur-alist) (cdar cur-alist))))))))))

(defun signal-type-error (type expected-type)
  (break "type error"))


(defun inform (fmt &rest args)
  (apply #'bformat t fmt args))

(si::fset 'cons-car #'(lambda (w e)
			`(car ,(cadr w)))
	  t)


(export '(concatenate 1+ 1- lexical-var special-var cons-car))

(defvar *print-pretty* t)
(defvar *print-level* nil)
(defvar *print-length* nil)
(defvar *print-base* 10)
(defvar *print-radix* nil)
(defvar *read-default-float-format* 'double-float)


(defun proclaim (spec)
  (bformat t "In foundation.lsp>>proclaim [ADD SUPPORT FOR THIS] --> %s\n" spec))
(export 'proclaim)


(defvar *class-name-hash-table* (make-hash-table :test #'eq))
(export '*class-name-hash-table*)
