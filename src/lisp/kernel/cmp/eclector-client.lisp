(cl:in-package #:cmp)

;;; So that non-cst-client can inherit behaviour
(defclass clasp-eclector-client-mixin ()())

(defclass clasp-cst-client (eclector.concrete-syntax-tree:cst-client clasp-eclector-client-mixin) ())

;; singleton- don't bother with constructor
(defvar *cst-client*
  (locally (declare (notinline make-instance))
    (make-instance 'clasp-cst-client)))

(defmethod eclector.parse-result:source-position
    ((client clasp-cst-client) stream)
  (cmp:compile-file-source-pos-info stream))

(defmethod eclector.reader:find-character ((client clasp-eclector-client-mixin) name)
  ;; This variable is defined in define-unicode-tables, which is loaded later.
  (declare (special *additional-clasp-character-names*))
  (or (call-next-method)
      (gethash name *additional-clasp-character-names*)
      (simple-unicode-name name)))

(defun map-make-structure-arguments (initargs)
  (loop for all = initargs then (cddr all)
        for key = (first all) and value = (second all)
        while all
        append (list
                (if (keywordp key) key (intern (string key) :keyword))
                value)))

(defmethod eclector.reader:make-structure-instance
    ((client clasp-eclector-client-mixin) name initargs)
  ;;; see discussion in https://github.com/s-expressionists/Eclector/issues/63
  ;;; initargs might be string-designators, not keywords, need to transform
  (core::make-structure
   name
   (map-make-structure-arguments initargs)))

(defmethod eclector.reader:wrap-in-quasiquote
    ((client clasp-eclector-client-mixin) form)
  (list 'core:quasiquote form))
(defmethod eclector.reader:wrap-in-unquote
    ((client clasp-eclector-client-mixin) form)
  (list 'core::unquote form))
(defmethod eclector.reader:wrap-in-unquote-splicing
    ((client clasp-eclector-client-mixin) form)
  (list 'core::unquote-splice form))
