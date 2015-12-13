(in-package :cscrape)

(define-condition bad-cl-defun ()
  ((tag :initarg :tag :accessor tag)
   (other-kind :initarg :other-kind :accessor other-kind)
   (other-tag :initarg :other-tag :accessor other-tag)))

(defun error-if-bad-expose-function-setup* (tag kind other-tag)
  (declare (optimize (debug 3)))
  (unless (and (string= (tags:file tag) (tags:file other-tag))
               (< (- (tags:line tag) (tags:line other-tag)) 8))
    (error 'bad-cl-defun :tag tag :other-kind kind :other-tag other-tag)))
                   
(defun error-if-bad-expose-function-setup (tag cur-lambda cur-declare cur-docstring)
  (when cur-lambda (error-if-bad-expose-function-setup* tag "LAMBDA" cur-lambda))
  (when cur-declare (error-if-bad-expose-function-setup* tag "DECLARE" cur-declare))
  (when cur-docstring (error-if-bad-expose-function-setup* tag "DOCSTRING" cur-docstring)))
  

(defun interpret-tags (tags)
  (declare (optimize (debug 3)))
  (let ((source-info (gather-source-files tags)))
    (calculate-character-offsets source-info))
  (let (cur-lambda
        cur-docstring
        cur-declare
        cur-namespace
        (namespace-to-assoc (make-hash-table :test #'equal))
        (package-to-assoc (make-hash-table :test #'equal)))
    (dolist (tag tags)
      (cond
        ((typep tag 'tags:namespace-package-association-tag)
         (setf (gethash (tags:namespace tag) namespace-to-assoc) tag)
         (setf (gethash (tags:package-var tag) package-to-assoc) tag))
        ((typep tag 'tags:namespace-tag)
         (setf cur-namespace (tags:namespace tag)))
        ((typep tag 'tags:lambda-tag)
                                        ; Lambda starts the declaration of a binding
         (setf cur-lambda tag))
        ((typep tag 'tags:docstring-tag)
         (setf cur-docstring tag))
        ((typep tag 'tags:declare-tag)
         (setf cur-declare tag))
        ((typep tag 'tags:expose-code-tag)
                                        ; Fill in latest info since last expose-code-tag
         (error-if-bad-expose-function-setup tag cur-lambda cur-declare cur-docstring)
         (setf (tags:lambda-tag tag) cur-lambda
               (tags:declare-tag tag) cur-declare
               (tags:docstring-tag tag) cur-docstring
               (tags:namespace-tag tag) cur-namespace)
         (setf cur-lambda nil
               cur-docstring nil
               cur-declare nil))
        ((typep tag 'tags:symbol-tag)
                                        ; Fill in missing info
         (unless (tags:c++-name% tag)
           (setf (tags:c++-name% tag) (tags:name% tag)))
         (unless (tags:namespace% tag)
           (let ((nsp-assoc (gethash (tags:package% tag) package-to-assoc)))
             (setf (tags:namespace% tag) (tags:namespace nsp-assoc))))
         (unless (tags:package% tag)
           (let ((nsp-assoc (gethash (tags:namespace% tag) namespace-to-assoc)))
             (setf (tags:package% tag) (tags:package-var nsp-assoc)))))))))
