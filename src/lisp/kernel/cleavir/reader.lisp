
(in-package :clasp-cleavir)

(defun core:make-atom-cst (source-info object)
  (make-instance 'concrete-syntax-tree:atom-cst :source-info source-info :raw object))


(defun assemble-raw (object)
  (cons (raw (car object)) (raw (cdr object)))

(defun core:make-cons-cst (source-info object)
  (error "make-cons-cst - what should I do with object: ~s" object))

