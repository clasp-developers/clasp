(in-package :tags)


(define-condition unknown-tag ()
  ((tag :initarg :tag :accessor unknown-tag-tag))
  (:report (lambda (condition stream)
             (format stream "Unknown tag ~a" (unknown-tag-tag condition)))))

(define-condition bad-tag ()
  ((context :initarg :context :accessor bad-tag-context)
   (tag :initarg :tag :accessor bad-tag-tag))
  (:report (lambda (condition stream)
             (format stream "Bad tag ~a at |~a|" (bad-tag-tag condition) (bad-tag-context condition)))))

(defclass tag-handler ()
  ((begin-tag :initarg :begin-tag)
   (handler-code :initarg :handler-code :accessor handler-code)))

(defclass tags:tag ()
  ((file% :initarg :file% :reader tags:file%)
   (line% :initarg :line% :reader tags:line%)))

(defclass tags:source-tag (tag)
  ((character-offset% :initform nil :initarg :character-offset% :accessor tags:character-offset%)))

(defclass tags:cl-initializer-tag (source-tag)
  ((character-offset% :initform nil :initarg :character-offset% :accessor tags:character-offset%)
   (signature-text% :initform nil :initarg :signature-text% :accessor signature-text%)))

(defclass tags:gc-managed-type-tag (tag)
  ((c++type% :initform nil :initarg :c++type% :accessor c++type%)))

(defclass tags:package-use-tag (tag)
  ((name% :initform nil :initarg :name% :accessor name%)))

(defclass tags:package-shadow-tag (tag)
  ((name% :initform nil :initarg :name% :accessor name%)))

(defclass tags:package-nickname-tag (tag)
  ((name% :initform nil :initarg :name% :accessor name%)))


(defclass tags:lisp-class-tag (source-tag)
  ((namespace% :initform nil :initarg :namespace% :reader tags:namespace%)
   (package% :initform nil :initarg :package% :reader tags:package%)
   (name% :initarg :name% :reader tags:name%)
   (class-symbol% :initarg :class-symbol% :reader tags:class-symbol%)
   (base% :initarg :base% :reader tags:base%)))

(defclass tags:lisp-internal-class-tag (lisp-class-tag) ())

(defclass tags:lisp-external-class-tag (lisp-class-tag) ())

(defclass tags:name-base-tag (tag)
  ((name% :initarg :name% :reader tags:name%)))

(defclass tags:cl-name-tag (name-base-tag) ())

(defclass tags:cl-lispify-name-tag (name-base-tag) ())

(defclass tags:cl-pkg-name-tag (name-base-tag)
  ((package% :initarg :package% :reader tags:package%)))


(defclass tags:meta-class-tag (tag)
  ((meta-class% :initarg :meta-class% :reader tags:meta-class%)))

(defun tags:maybe-meta-class (namespace-tag tag)
  (if tag
      (if (search "::" (meta-class% tag))
          (meta-class% tag)
          (format nil "~a::~a" (namespace% namespace-tag) (meta-class% tag)))
      "_lisp->_Roots._TheBuiltInClass"))

(defclass tags:cl-lambda-tag (tag)
  ((lambda-list% :initarg :lambda-list% :reader tags:lambda-list%)))

(defun maybe-lambda-list (tag)
  (if tag
      (lambda-list% tag)
      nil))

(defclass tags:cl-docstring-tag (tag)
  ((docstring% :initarg :docstring% :reader tags:docstring%)))

(defun maybe-docstring (tag)
  (if tag
      (docstring% tag)
      "\"\""))

(defclass tags:cl-declare-tag (tag)
  ((declare-form% :initarg :declare-form% :reader tags:declare-form%)))

(defun maybe-declare (tag)
  (if tag
      (declare-form% tag)
      ""))

(defclass tags:internal-code-tag (source-tag)
  ((signature-text% :initarg :signature-text% :reader tags:signature-text%)))

(defclass tags:external-code-tag (source-tag)
  ((pointer% :initarg :pointer% :reader tags:pointer%)))

(defclass tags:cl-defun-tag (internal-code-tag) ())

(defclass tags:cl-extern-defun-tag (external-code-tag) ())

(defclass tags:cl-defmethod-tag (internal-code-tag) ())

(defclass tags:cl-def-class-method-tag (internal-code-tag) ())

(defclass tags:cl-extern-defmethod-tag (external-code-tag)
  ((class-name% :initarg :class-name% :accessor class-name%)))

(defclass tags:namespace-tag (tag)
  ((namespace% :initarg :namespace% :reader tags:namespace%)))

(defclass tags:cl-begin-enum-tag (source-tag)
  ((type% :initarg :type% :accessor type%)
   (symbol% :initarg :symbol% :accessor symbol%)
   (description% :initarg :description% :accessor description%)))

(defclass tags:cl-value-enum-tag (source-tag)
  ((symbol% :initarg :symbol% :accessor symbol%)
   (value% :initarg :value% :accessor value%)))

(defclass tags:cl-end-enum-tag (source-tag)
  ((symbol% :initarg :symbol% :accessor symbol%)))

(defclass tags:symbol-tag (tag)
  ((namespace% :initform nil :initarg :namespace% :reader tags:namespace%)
   (package% :initform nil :initarg :package% :reader tags:package%)
   (c++-name% :initform nil :initarg :c++-name% :reader tags:c++-name%)
   (lisp-name% :initarg :lisp-name% :reader tags:lisp-name%)))

(defclass tags:symbol-external-tag (symbol-tag) ())
(defclass tags:symbol-shadow-external-tag (symbol-external-tag) ())
(defclass tags:detailed-symbol-external-tag (symbol-external-tag) ())
(defclass tags:symbol-intern-tag (symbol-external-tag) ())
(defclass tags:symbol-internal-tag (symbol-tag) ())

(defclass tags:namespace-package-association-tag (source-tag)
  ((namespace% :initarg :namespace% :reader tags:namespace%)
   (package% :initarg :package% :reader tags:package%)
   (package-str% :initarg :package-str% :reader tags:package-str%)))

(defun maybe-unquote (str)
  (string-trim '(#\") str))



(defmacro add-tag-handler (tag-handlers tag-class-symbol begin-tag code)
  `(progn
     (setf (gethash ,begin-tag ,tag-handlers) (make-instance 'tag-handler
                                                           :begin-tag ,begin-tag
                                                           :handler-code ,code)
         (gethash ,tag-class-symbol *tag-codes*) ,begin-tag)))

(progn
  (defparameter *tag-codes* (make-hash-table :test #'equal))
  (defparameter *tag-handlers* (make-hash-table :test #'equal))
  (add-tag-handler *tag-handlers* 'lisp-internal-class-tag "LISP_CLASS_TAG"
                   #'(lambda (bufs) ;(declare (core:lambda-name lambda-tag-handler))
                       (let ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:lisp-internal-class-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :namespace% (getf plist :namespace)
                                        :package% (getf plist :package)
                                        :name% (getf plist :class)
                                        :base% (getf plist :base)
                                        :class-symbol% (getf plist :class-symbol)))))
  (add-tag-handler *tag-handlers* 'lisp-external-class-tag "LISP_EXTERNAL_CLASS_TAG"
                   #'(lambda (bufs) ;(declare (core:lambda-name lambda-tag-handler))
                       (let ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:lisp-external-class-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :namespace% (getf plist :namespace)
                                        :package% (getf plist :package)
                                        :name% (getf plist :class)
                                        :class-symbol% (getf plist :class-symbol)
                                        :base% (getf plist :base)))))
  (add-tag-handler *tag-handlers* 'gc-managed-type-tag "GC_MANAGED_TYPE_TAG"
                   #'(lambda (bufs)
                       (let ((plist (read (cscrape:buffer-stream bufs))))
                         (let ((key (maybe-unquote (getf plist :type))))
;;;                           (format *debug-io* "Creating gc-managed-type-tag: ~a~%" key)
                           (make-instance 'tags:gc-managed-type-tag
                                          :file% (getf plist :file)
                                          :line% (getf plist :line)
                                          :c++type% key)))))
  (add-tag-handler *tag-handlers* 'cl-lispify-name-tag "CL_LISPIFY_NAME_TAG"
                   #'(lambda (bufs) ;(declare (core:lambda-name lambda-tag-handler))
                       (let ((plist (read (cscrape:buffer-stream bufs))))
                         (let ((cl-name (maybe-unquote (getf plist :cl-name))))
                           (make-instance 'tags:cl-lispify-name-tag
                                          :file% (getf plist :file)
                                          :line% (getf plist :line)
                                          :name% cl-name)))))
  (add-tag-handler *tag-handlers* 'cl-name-tag "CL_NAME_TAG"
                   #'(lambda (bufs) ;(declare (core:lambda-name lambda-tag-handler))
                       (let ((plist (read (cscrape:buffer-stream bufs))))
                         (let ((cl-name (maybe-unquote (getf plist :cl-name))))
                           (make-instance 'tags:cl-name-tag
                                          :file% (getf plist :file)
                                          :line% (getf plist :line)
                                          :name% cl-name)))))
  (add-tag-handler *tag-handlers* 'cl-pkg-name-tag "CL_PKG_NAME_TAG"
                   #'(lambda (bufs)
                       (let ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:cl-pkg-name-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :name% (maybe-unquote (getf plist :cl-name))
                                        :package% (getf plist :cl-pkg)))))
  (add-tag-handler *tag-handlers* 'meta-class-tag "META_CLASS_TAG"
                   #'(lambda (bufs)
                       (let ((plist (read (cscrape:buffer-stream bufs))))
                         (let ((meta-class (getf plist :meta-class)))
                           (make-instance 'tags:meta-class-tag
                                          :file% (getf plist :file)
                                          :line% (getf plist :line)
                                          :meta-class% meta-class)))))
  (add-tag-handler *tag-handlers* 'cl-lambda-tag "CL_LAMBDA_TAG"
                   #'(lambda (bufs)
                       (let ((plist (read (cscrape:buffer-stream bufs))))
                         (let ((lambda-list (getf plist :lambda-list)))
                           (when (and (> (length lambda-list) 1)
                                      (char= (elt lambda-list 0) #\"))
                             (setf lambda-list (read-from-string lambda-list)))
                           (make-instance 'tags:cl-lambda-tag
                                          :file% (getf plist :file)
                                          :line% (getf plist :line)
                                          :lambda-list% lambda-list)))))
  (add-tag-handler *tag-handlers* 'cl-docstring-tag "CL_DOCSTRING_TAG"
                   #'(lambda (bufs)
                       (let ((plist (read (cscrape:buffer-stream bufs)))
                             (docstring (cscrape:read-string-to-tag bufs cscrape:*end-tag*)))
                         (make-instance 'tags:cl-docstring-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :docstring% docstring))))
  (add-tag-handler *tag-handlers* 'cl-declare-tag "CL_DECLARE_TAG"
                   #'(lambda (bufs)
                       (let ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:cl-declare-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :declare-form% (getf plist :declare)))))
  (add-tag-handler *tag-handlers* 'cl-defun-tag "CL_DEFUN_TAG"
                   #'(lambda (bufs)
                       (declare (optimize speed))
                       (let* ((plist (read (cscrape:buffer-stream bufs)))
                              (signature-text (cscrape:read-string-to-character bufs #\) t)))
                         (make-instance 'tags:cl-defun-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :signature-text% signature-text))))
  (add-tag-handler *tag-handlers* 'cl-defmethod-tag "CL_DEFMETHOD_TAG"
                   #'(lambda (bufs)
                       (declare (optimize speed))
                       (let* ((plist (read (cscrape:buffer-stream bufs)))
                              (signature-text (cscrape:read-string-to-character bufs #\) t)))
                         (make-instance 'tags:cl-defmethod-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :signature-text% signature-text))))
  (add-tag-handler *tag-handlers* 'cl-def-class-method-tag "CL_DEF_CLASS_METHOD_TAG"
                   #'(lambda (bufs)
                       (declare (optimize speed))
                       (let* ((plist (read (cscrape:buffer-stream bufs)))
                              (signature-text (cscrape:read-string-to-character bufs #\) t)))
                         (make-instance 'tags:cl-def-class-method-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :signature-text% signature-text))))
  (add-tag-handler *tag-handlers* 'cl-extern-defun-tag "CL_EXTERN_DEFUN_TAG"
                   #'(lambda (bufs)
                       (declare (optimize speed))
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:cl-extern-defun-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :pointer% (getf plist :pointer)))))
  (add-tag-handler *tag-handlers* 'cl-extern-defmethod-tag "CL_EXTERN_DEFMETHOD_TAG"
                   #'(lambda (bufs)
                       (declare (optimize speed))
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:cl-extern-defmethod-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :class-name% (getf plist :type)
                                        :pointer% (getf plist :pointer)))))
  (add-tag-handler *tag-handlers* 'symbol-internal-tag "SYMBOL_INTERNAL"
                   #'(lambda (bufs) ;(declare (core:lambda-name namespace-tag-handler))
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:symbol-internal-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :package% (getf plist :PACKAGE)
                                        :lisp-name% (getf plist :NAME)))))
  (add-tag-handler *tag-handlers* 'symbol-external-tag "SYMBOL_EXTERNAL"
                   #'(lambda (bufs) ;(declare (core:lambda-name namespace-tag-handler))
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:symbol-external-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :package% (getf plist :PACKAGE)
                                        :lisp-name% (getf plist :NAME)
                                        :c++-name% (getf plist :NAME)))))
  (add-tag-handler *tag-handlers* 'symbol-shadow-external-tag "SYMBOL_SHADOW_EXTERNAL"
                   #'(lambda (bufs) ;(declare (core:lambda-name namespace-tag-handler))
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:symbol-shadow-external-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :package% (getf plist :PACKAGE)
                                        :lisp-name% (getf plist :NAME)
                                        :c++-name% (getf plist :NAME)))))
  (add-tag-handler *tag-handlers* 'detailed-symbol-external-tag "DETAILED_SYMBOL_EXTERNAL"
                   #'(lambda (bufs) ;(declare (core:lambda-name namespace-tag-handler))
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:detailed-symbol-external-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :package% (getf plist :PACKAGE)
                                        :c++-name% (getf plist :CXX-NAME)
                                        :lisp-name% (getf plist :NAME)))))
  (add-tag-handler *tag-handlers* 'symbol-intern-tag "SYMBOL_INTERN"
                   #'(lambda (bufs) ;(declare (core:lambda-name namespace-tag-handler))
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:symbol-intern-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :namespace% (getf plist :namespace)
                                        :c++-name% (getf plist :name)
                                        :lisp-name% (getf plist :name)))))
  (add-tag-handler *tag-handlers* 'cl-begin-enum-tag "CL_BEGIN_ENUM_TAG"
                   #'(lambda (bufs)
                       (declare (optimize (speed 3)))
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:cl-begin-enum-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :type% (getf plist :type)
                                        :symbol% (getf plist :symbol)
                                        :description% (getf plist :description)))))
  (add-tag-handler *tag-handlers* 'cl-value-enum-tag "CL_VALUE_ENUM_TAG"
                   #'(lambda (bufs)
                       (declare (optimize (speed 3)))
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:cl-value-enum-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :symbol% (getf plist :symbol)
                                        :value% (getf plist :value)))))
  (add-tag-handler *tag-handlers* 'cl-end-enum-tag "CL_END_ENUM_TAG"
                   #'(lambda (bufs)
                       (declare (optimize (speed 3)))
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:cl-end-enum-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :symbol% (getf plist :symbol)))))
  (add-tag-handler *tag-handlers* 'package-use-tag "PACKAGE_USE_TAG"
                   #'(lambda (bufs)
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:package-use-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :name% (getf plist :name)))))
  (add-tag-handler *tag-handlers* 'package-shadow-tag "PACKAGE_SHADOW_TAG"
                   #'(lambda (bufs)
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:package-shadow-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :name% (getf plist :name)))))
  (add-tag-handler *tag-handlers* 'package-nickname-tag "PACKAGE_NICKNAME_TAG"
                   #'(lambda (bufs)
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:package-nickname-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :name% (getf plist :name)))))
  (add-tag-handler *tag-handlers* 'namespace-package-association-tag
                   "NAMESPACE_PACKAGE_ASSOCIATION_TAG"
                   #'(lambda (bufs) ;(declare (core:lambda-name namespace-tag-handler))
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:namespace-package-association-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :namespace% (getf plist :namespace)
                                        :package% (getf plist :package)
                                        :package-str% (getf plist :package-name)))))
  (add-tag-handler *tag-handlers* 'cl-initializer-tag "CL_INITIALIZER_TAG"
                   #'(lambda (bufs)
                       (let* ((plist (read (cscrape:buffer-stream bufs)))
                              (signature-text (cscrape:read-string-to-character bufs #\) t)))
                         (make-instance 'tags:cl-initializer-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :signature-text% signature-text))))
  ;; Hangle more tags here
  )

(defun tags:tag-code (tag)
  (let ((tag-class-symbol (class-name (class-of tag))))
    (gethash tag-class-symbol *tag-codes*)))

(defgeneric tags:identifier (tag))
(defmethod identifier ((tag t)) "")
(defmethod identifier ((tag lisp-class-tag))
  "* Description 
Return the name of the class for the tag."
  (name% tag))

(defgeneric tags:source-pos (tag))
(defmethod source-pos ((tag tag))
  (format nil "~a:~d" (file% tag) (line% tag)))
