;-*- Mode:     Lisp -*-
;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Compiler regression tests

(in-package :cl-test)

;;; Description:
;;;
;;;	The interpreter selectively complains when assigning a variable
;;;	that has not been declared as special and is not local.
;;;
;;; Fixed: 03/2006 (juanjo)
;;;
(deftest int-0001-global-setq
    (mapcar
     (lambda (ext:*action-on-undefined-variable*)
       (handler-case
	   (progn (eval `(setq ,(gensym) 1)) :no-error)
	 (error (c) :error)))
     '(nil ERROR))
  (:no-error :error))

;;; Date: 24/04/2010 (Juanjo)
;;; Fixed: 24/04/2010 (Juanjo)
;;; Description:
;;;     The interpreter does not increase the lexical environment depth when
;;;     optimizing certain forms (LIST, LIST*, CONS...) and thus causes some
;;;     of the arguments to be eagerly evaluated.
;;;
(deftest int-0002-list-optimizer-error
    (with-output-to-string (*standard-output*)
      (eval '(list (print 1) (progn (print 2) (print 3)))))
  "
1 
2 
3 ")

