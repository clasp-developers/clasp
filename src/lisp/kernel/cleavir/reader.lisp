(in-package :clasp-cleavir)

(defmethod concrete-syntax-tree::all-lambda-list-keywords append ((client clasp-64bit))
  '(core:&va-rest))

(defmethod concrete-syntax-tree::allowed-lambda-list-keywords append
    ((client clasp-64bit) (lambda-list concrete-syntax-tree:ordinary-lambda-list))
  '(core:&va-rest))

(defmethod concrete-syntax-tree::allowed-lambda-list-keywords append
    ((client clasp-64bit) (lambda-list concrete-syntax-tree:specialized-lambda-list))
  '(core:&va-rest))


(defclass keyword-va-rest (cst:lambda-list-keyword) ())
(cst:define-keyword-scanner-action keyword-va-rest core:&va-rest)

(defclass clasp-ordinary-lambda-list (cst:ordinary-lambda-list) ())

(defclass clasp-ordinary-rest-parameter-group (cst:ordinary-rest-parameter-group)
  ())

(defparameter *clasp-ordinary-rest-parameter-group*
  '((clasp-ordinary-rest-parameter-group cst:<-
     cst:keyword-rest cst:simple-variable)
    (clasp-ordinary-rest-parameter-group cst:<-
     keyword-va-rest cst:simple-variable)))

(defparameter *clasp-ordinary-lambda-list*
  '((clasp-ordinary-lambda-list cst:<-
     cst:ordinary-required-parameter-group
     (cst:? cst:ordinary-optional-parameter-group)
     (cst:? clasp-ordinary-rest-parameter-group)
     (cst:? cst:ordinary-key-parameter-group)
     (cst:? cst:aux-parameter-group))))

(defparameter *clasp-ordinary-lambda-list-grammar*
  (cst::generate-grammar 'clasp-ordinary-lambda-list
                         (append *clasp-ordinary-lambda-list*
                                 *clasp-ordinary-rest-parameter-group*
                                 cst:*standard-grammar*)))

(cst::define-top-level-parser parse-clasp-ordinary-lambda-list
  *clasp-ordinary-lambda-list-grammar*
  clasp-ordinary-lambda-list)

(defmethod cleavir-cst-to-ast:lambda-list-from-parameter-group
    ((parameter-group clasp-ordinary-rest-parameter-group) entries)
  (cons (etypecase (cst:keyword parameter-group)
          (cst:keyword-rest '&rest)
          (keyword-va-rest 'core:&va-rest))
        entries))
