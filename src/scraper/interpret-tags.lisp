(in-package :cscrape)

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
         (setf cur-lambda tag)
         (setf cur-docstring nil)
         (setf cur-declare nil))
        ((typep tag 'tags:docstring-tag)
         (setf cur-docstring tag))
        ((typep tag 'tags:declare-tag)
         (setf cur-declare tag))
        ((typep tag 'tags:expose-code-tag)
                                        ; Fill in latest info since LAMBDA
         (setf (tags:lambda-tag tag) cur-lambda)
         (setf (tags:declare-tag tag) cur-declare)
         (setf (tags:docstring-tag tag) cur-docstring)
         (setf (tags:namespace-tag tag) cur-namespace))
        ((typep tag 'tags:symbol-tag)
                                        ; Fill in missing info
         (unless (slot-boundp tag 'tags:c++-name%)
           (setf (tags:c++-name% tag) (tags:name% tag)))
         (unless (slot-boundp tag 'tags:namespace%)
           (let ((nsp-assoc (gethash (tags:package% tag) package-to-assoc)))
             (setf (tags:namespace% tag) (tags:namespace nsp-assoc))))
         (unless (slot-boundp tag 'tags:package%))
           (let ((nsp-assoc (gethash (tags:namespace% tag) namespace-to-assoc)))
             (setf (tags:package% tag) (tags:package-var nsp-assoc))))))))
