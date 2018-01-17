(in-package :clasp-cleavir)

(defmethod concrete-syntax-tree::all-lambda-list-keywords append ((client clasp-64bit))
  '(&optional
    &key
    &allow-other-keys
    &rest
    core:&va-rest
    &body
    &aux
    &environment
    &whole))

#+sbcl
(defmethod concrete-syntax-tree::all-lambda-list-keywords append ((client sbcl))
  '(sb-int:&more))

(defmethod concrete-syntax-tree::allowed-lambda-list-keywords append
    ((client clasp-64bit) (lambda-list concrete-syntax-tree::ordinary-lambda-list))
  '(&optional
    &key
    &allow-other-keys
    &rest
    core:&va-rest
    &aux))

(defmethod concrete-syntax-tree::allowed-lambda-list-keywords append
    ((client clasp-64bit) (lambda-list concrete-syntax-tree::generic-function-lambda-list))
  '(&optional
    &key
    &allow-other-keys
    &rest))

(defmethod concrete-syntax-tree::allowed-lambda-list-keywords append
    ((client clasp-64bit) (lambda-list concrete-syntax-tree::specialized-lambda-list))
  '(&optional
    &key
    &allow-other-keys
    &rest
    core:&va-rest
    &aux))

(defmethod concrete-syntax-tree::allowed-lambda-list-keywords append
    ((client clasp-64bit) (lambda-list concrete-syntax-tree::macro-lambda-list))
  '(&optional
    &key
    &allow-other-keys
    &rest
    &body
    &aux
    &environment
    &whole))

(defmethod concrete-syntax-tree::allowed-lambda-list-keywords append
    ((client clasp-64bit) (lambda-list concrete-syntax-tree::destructuring-lambda-list))
  '(&optional
    &key
    &allow-other-keys
    &rest
    &body
    &aux
    &whole))

(defmethod concrete-syntax-tree::allowed-lambda-list-keywords append
    ((client clasp-64bit) (lambda-list concrete-syntax-tree::defsetf-lambda-list))
  '(&optional
    &key
    &allow-other-keys
    &rest
    &environment))

