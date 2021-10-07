;;;; Esrap example: a simple grammar with scopes and symbol tables.

(cl:require :esrap)

(cl:defpackage #:symbol-table
  (:use #:cl #:esrap))

(cl:in-package #:symbol-table)

;;; Use the :AROUND construction to maintain a stack of symbol tables
;;; during parsing.
;;;
;;; It is important to note that the bodies of :AROUND options are
;;; executed during result construction, not parsing. Therefore,
;;; :AROUND cannot be used to introduce context sensitivity into
;;; parsing. However, this can be done when using functions as
;;; terminals, see example-function-terminals.lisp.

(declaim (special *symbol-table*))
(defvar *symbol-table* nil)

(defstruct (symbol-table
            (:constructor make-symbol-table (&optional %parent)))
  (%table (make-hash-table :test #'equal))
  %parent)

(defun lookup/direct (name &optional (table *symbol-table*))
  (values (gethash name (symbol-table-%table table))))

(defun lookup (name &optional (table *symbol-table*))
  (or (lookup/direct name table)
      (alexandria:when-let ((parent (symbol-table-%parent table)))
        (lookup name parent))))

(defun (setf lookup) (new-value name &optional (table *symbol-table*))
  (when (lookup/direct name table)
    (error "~@<Duplicate name: ~S.~@:>"
           name))
  (setf (gethash name (symbol-table-%table table)) new-value))



(defrule whitespace
    (+ (or #\Space #\Tab #\Newline))
  (:constant nil))

(defrule name
    (+ (alphanumericp character))
  (:text t))

(defrule type
    (+ (alphanumericp character))
  (:text t))

(defrule declaration
    (and name #\: type)
  (:destructure (name colon type)
    (declare (ignore colon))
    (setf (lookup name) (list name :type type))
    (values)))

(defrule use
    name
  (:lambda (name)
    (list :use (or (lookup name)
                   (error "~@<Undeclared variable: ~S.~@:>"
                          name)))))

(defrule statement
    (+ (or scope declaration use))
  (:lambda (items)
    (remove nil items)))

(defrule statement/ws
    (and statement (? whitespace))
  (:function first))

(defrule scope
    (and (and #\{ (? whitespace))
         (* statement/ws)
         (and #\} (? whitespace)))
  (:function second)
  (:around ()
    (let ((*symbol-table* (make-symbol-table *symbol-table*)))
      (list* :scope (apply #'append (call-transform))))))

(parse 'scope "{
  a:int
  a
  {
    a
    b:double
    a
    b
    {
      a:string
      a
      b
    }
    a
    b
  }
  a
}")
