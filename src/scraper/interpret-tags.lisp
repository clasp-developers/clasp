(in-package :cscrape)

(define-condition bad-cl-defun ()
  ((tag :initarg :tag :accessor tag)
   (other-kind :initarg :other-kind :accessor other-kind)
   (other-tag :initarg :other-tag :accessor other-tag)))

(define-condition interpret-error ()
  ())
  
(define-condition missing-base (interpret-error)
  ((tag :initarg :tag :accessor tag))
  (:report (lambda (condition stream)
             (format stream "~a Missing base for ~a ~a."
                     (tags:source-pos tag)
                     (tags:code tag)
                     (tags:identifier tag)))))

(define-condition missing-namespace (interpret-error)
  ((tag :initarg :tag :accessor tag))
  (:report (lambda (condition stream)
             (format stream "~a Missing namespace for ~a ~a."
                     (tags:source-pos tag)
                     (tags:code tag)
                     (tags:identifier tag)))))

(define-condition namespace-mismatch (interpret-error)
  ((tag :initarg :tag :accessor tag))
  (:report (lambda (condition stream)
             (format stream "~a Mismatch between enclosed namespace and ~a ~a"
                     (tags:source-pos tag)
                     (tags:code tag)
                     (tags:identifier tag)))))
   
(defun error-if-bad-expose-function-setup* (tag kind other-tag)
  (declare (optimize (debug 3)))
  (unless (and (string= (tags:file tag) (tags:file other-tag))
               (< (- (tags:line tag) (tags:line other-tag)) 8))
    (error 'bad-cl-defun :tag tag :other-kind kind :other-tag other-tag)))
                   
(defun error-if-bad-expose-function-setup (tag cur-lambda cur-declare cur-docstring)
  (when cur-lambda (error-if-bad-expose-function-setup* tag "LAMBDA" cur-lambda))
  (when cur-declare (error-if-bad-expose-function-setup* tag "DECLARE" cur-declare))
  (when cur-docstring (error-if-bad-expose-function-setup* tag "DOCSTRING" cur-docstring)))

(defgeneric namespaced-class-name (namespace-tag tag))
  
(defun namespaced-class-name (namespace-tag (tag base-tag))
  (if (search "::" (tags:name% tag))
      (tags:name% tag)
      (format nil "~a::~a"
              (tags:namespace% namespace-tag)
              (tags:name% tag))))

(defun namespaced-class-name (namespace-tag (tag lisp-class-tag))
  (if (search "::" (tags:name% tag))
      (tags:name% tag)
      (format nil "~a::~a"
              (tags:namespace% namespace-tag)
              (tags:name% tag))))

(defclass exposed-class ()
  ((namespace% :initarg :namespace% :accessor namespace%)
   (package% :initarg :package% :accessor package%)
   (name% :initarg :name% :accessor name%)
   (base-class-name% :initarg :base-class-name% :accessor base-class-name%)
   (class-symbol% :initarg :class-symbol% :accessor class-symbol%)
   (methods :initarg :methods :accessor methods)))

(defclass exposed-internal-class (exposed-class) ())
(defclass exposed-external-class (exposed-class) ())


(defun class-key (namespace class-name)
  (declare (optimize debug))
  (format nil "~a::~a" namespace class-name))

(defun class-key-from-lisp-class-tag (tag)
  (declare (optimize debug))
  (class-key (tags:namespace% tag) (tags:c++-class% tag)))

(defun interpret-tags (tags)
  (declare (optimize (debug 3)))
  (let ((source-info (gather-source-files tags)))
    (calculate-character-offsets source-info))
  (let (cur-lambda
        cur-name
        cur-docstring
        cur-declare
        cur-namespace-tag
        cur-base
        cur-class
        (namespace-to-assoc (make-hash-table :test #'equal))
        (package-to-assoc (make-hash-table :test #'equal))
        (classes (make-hash-table :test #'equal)))
    (dolist (tag tags)
      (etypecase tag
        (tags:namespace-package-association-tag
         (setf (gethash (tags:namespace tag) namespace-to-assoc) tag)
         (setf (gethash (tags:package-var tag) package-to-assoc) tag))
        (tags:namespace-tag
         (setf cur-namespace-tag tag))
        (tags:name-tag
         (setf cur-name tag))
        (tags:lambda-tag
         (setf cur-lambda tag))
        (tags:docstring-tag
         (setf cur-docstring tag))
        (tags:declare-tag
         (setf cur-declare tag))
        (tags:expose-code-tag
         (error-if-bad-expose-function-setup tag cur-lambda cur-declare cur-docstring)
         (setf (tags:lambda-tag tag) cur-lambda
               (tags:name-tag tag) cur-name
               (tags:declare-tag tag) cur-declare
               (tags:docstring-tag tag) cur-docstring
               (tags:namespace-tag tag) cur-namespace-tag)
         (when (typep tag 'tags:expose-method-tag)
           ;; methods in header files need to get
           ;; their class from cur-class
           (unless (tags:class-name% tag)
             (unless cur-class
               (error "There is no cur-class defined when defining method ~a" (tags:method-name tag)))
             (setf (tags:class-name% tag) (tags:c++-class% cur-class)))
           (let ((class-key (class-key (tags:namespace (tags:namespace-tag tag)) (tags:class-name% tag))))
             (setf (tags:class-tag tag) (gethash class-key classes))))
         (setf cur-lambda nil
               cur-name nil
               cur-docstring nil
               cur-declare nil))
        (tags:lisp-base-tag
         (setf cur-base tag))
        (tags:lisp-internal-class-tag
         (unless cur-base (error 'missing-base :tag tag))
         (unless cur-namespace (error 'missing-namespace :tag tag))
         (let ((base-class-name (namespaced-class-name cur-namespace-tag cur-base)))
           (unless (string= (tags:namespace% cur-namespace-tag) (tags:namespace% tag))
             (error 'namespace-mismatch :tag tag))
           (setf cur-base nil
                 cur-class tag)
           (let ((class-key (namespaced-class-name cur-namespace-tag tag)))
             (setf (gethash class-key classes) tag)))
        (tags:lisp-external-class-tag
         (unless cur-namespace (error 'missing-namespace :tag tag))
         (let ((base-class-name (namespaced-class-name cur-namespace-tag cur-base)))
           (unless (string= (tags:namespace% cur-namespace-tag) (tags:namespace% tag))
             (error 'namespace-mismatch :tag tag))
           (setf cur-base nil
                 cur-class tag)
           (let ((class-key (namespaced-class-name cur-namespace-tag tag)))
             (setf (gethash class-key classes) tag)))
         (tags:symbol-tag
          (unless (tags:c++-name% tag)
            (setf (tags:c++-name% tag) (tags:name% tag)))
          (unless (tags:namespace% tag)
            (let ((nsp-assoc (gethash (tags:package% tag) package-to-assoc)))
              (setf (tags:namespace% tag) (tags:namespace nsp-assoc))))
          (unless (tags:package% tag)
            (let ((nsp-assoc (gethash (tags:namespace% tag) namespace-to-assoc)))
              (setf (tags:package% tag) (tags:package-var nsp-assoc)))))))))


(defun interpret-exposed-classes (tags)
  (declare (optimize debug))
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (tag tags)
      (typecase tag
        (tags:lisp-class-tag
         (let ((class-key (class-key-from-lisp-class-tag tag)))
           (unless (gethash class-key ht)
             (setf (gethash class-key ht) (make-instance 'exposed-class
                                                         :namespace% (tags:namespace% tag)
                                                         :kind% (tags:kind% tag)
                                                         :package% (tags:package% tag)
                                                         :c++WORKING WORKING
                                                         :methods% nil)))))
        (tags:expose-method-tag
         (let* ((class-key (class-key-from-lisp-class-tag (tags:class-tag tag)))
                (class-info (gethash class-key ht)))
           (unless class-info
             (error "There is no class info for ~a" class-key))
           (pushnew tag (methods class-info) :test #'string= :key (lambda (v) (tags:method-name v)))))))
    ht))
