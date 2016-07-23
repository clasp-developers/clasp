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

(defclass tags:initializer-tag (source-tag)
  ((character-offset% :initform nil :initarg :character-offset% :accessor tags:character-offset%)
   (signature-text% :initform nil :initarg :signature-text% :accessor signature-text%)))

(defclass tags:package-use-tag (tag)
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

(defclass tags:name-tag (name-base-tag) ())

(defclass tags:lispify-name-tag (name-base-tag) ())

(defclass tags:pkg-name-tag (name-base-tag)
  ((package% :initarg :package% :reader tags:package%)))


(defclass tags:meta-class-tag (tag)
  ((meta-class% :initarg :meta-class% :reader tags:meta-class%)))

(defun tags:maybe-meta-class (namespace-tag tag)
  (if tag
      (if (search "::" (meta-class% tag))
          (meta-class% tag)
          (format nil "~a::~a" (namespace% namespace-tag) (meta-class% tag)))
      "core::BuiltInClass_O"))

(defclass tags:lambda-tag (tag)
  ((lambda-list% :initarg :lambda-list% :reader tags:lambda-list%)))

(defun maybe-lambda-list (tag)
  (if tag
      (lambda-list% tag)
      nil))

(defclass tags:docstring-tag (tag)
  ((docstring% :initarg :docstring% :reader tags:docstring%)))

(defun maybe-docstring (tag)
  (if tag
      (docstring% tag)
      "\"\""))

(defclass tags:declare-tag (tag)
  ((declare-form% :initarg :declare-form% :reader tags:declare-form%)))

(defun maybe-declare (tag)
  (if tag
      (declare-form% tag)
      ""))

(defclass tags:internal-code-tag (source-tag)
  ((signature-text% :initarg :signature-text% :reader tags:signature-text%)))

(defclass tags:external-code-tag (source-tag)
  ((pointer% :initarg :pointer% :reader tags:pointer%)))

(defclass tags:expose-internal-function-tag (internal-code-tag) ())

(defclass tags:expose-external-function-tag (external-code-tag) ())

(defclass tags:expose-internal-method-tag (internal-code-tag) ())

(defclass tags:expose-external-method-tag (external-code-tag)
  ((class-name% :initarg :class-name% :accessor class-name%)))

(defclass tags:namespace-tag (tag)
  ((namespace% :initarg :namespace% :reader tags:namespace%)))

(defclass tags:begin-enum-tag (source-tag)
  ((type% :initarg :type% :accessor type%)
   (symbol% :initarg :symbol% :accessor symbol%)
   (description% :initarg :description% :accessor description%)))

(defclass tags:value-enum-tag (source-tag)
  ((symbol% :initarg :symbol% :accessor symbol%)
   (value% :initarg :value% :accessor value%)))

(defclass tags:end-enum-tag (source-tag)
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
  `(setf (gethash ,begin-tag ,tag-handlers) (make-instance 'tag-handler
                                                           :begin-tag ,begin-tag
                                                           :handler-code ,code)
         (gethash ,tag-class-symbol *tag-codes*) ,begin-tag))

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
  (add-tag-handler *tag-handlers* 'lispify-name-tag "LISPIFY_NAME_TAG"
                   #'(lambda (bufs) ;(declare (core:lambda-name lambda-tag-handler))
                       (let ((plist (read (cscrape:buffer-stream bufs))))
                         (let ((cl-name (maybe-unquote (getf plist :cl-name))))
                           (make-instance 'tags:lispify-name-tag
                                          :file% (getf plist :file)
                                          :line% (getf plist :line)
                                          :name% cl-name)))))
  (add-tag-handler *tag-handlers* 'name-tag "NAME_TAG"
                   #'(lambda (bufs) ;(declare (core:lambda-name lambda-tag-handler))
                       (let ((plist (read (cscrape:buffer-stream bufs))))
                         (let ((cl-name (maybe-unquote (getf plist :cl-name))))
                           (make-instance 'tags:name-tag
                                          :file% (getf plist :file)
                                          :line% (getf plist :line)
                                          :name% cl-name)))))
  (add-tag-handler *tag-handlers* 'pkg-name-tag "PKG_NAME_TAG"
                   #'(lambda (bufs)
                       (let ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:pkg-name-tag
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
  (add-tag-handler *tag-handlers* 'lambda-tag "LAMBDA_TAG"
                   #'(lambda (bufs)
                       (let ((plist (read (cscrape:buffer-stream bufs))))
                         (let ((lambda-list (getf plist :lambda-list)))
                           (when (and (> (length lambda-list) 1)
                                      (char= (elt lambda-list 0) #\"))
                             (setf lambda-list (read-from-string lambda-list)))
                           (make-instance 'tags:lambda-tag
                                          :file% (getf plist :file)
                                          :line% (getf plist :line)
                                          :lambda-list% lambda-list)))))
  (add-tag-handler *tag-handlers* 'docstring-tag "DOCSTRING_TAG"
                   #'(lambda (bufs)
                       (let ((plist (read (cscrape:buffer-stream bufs)))
                             (docstring (cscrape:read-string-to-tag bufs cscrape:*end-tag*)))
                         (make-instance 'tags:docstring-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :docstring% docstring))))
  (add-tag-handler *tag-handlers* 'declare-tag "DECLARE_TAG"
                   #'(lambda (bufs)
                       (let ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:declare-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :declare-form% (getf plist :declare)))))
  (add-tag-handler *tag-handlers* 'expose-internal-function-tag "EXPOSE_FUNCTION"
                   #'(lambda (bufs)
                       (declare (optimize debug))
                       (let* ((plist (read (cscrape:buffer-stream bufs)))
                              (signature-text (cscrape:read-string-to-character bufs #\) t)))
                         (make-instance 'tags:expose-internal-function-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :signature-text% signature-text))))
  (add-tag-handler *tag-handlers* 'expose-internal-method-tag "EXPOSE_METHOD"
                   #'(lambda (bufs)
                       (declare (optimize debug))
                       (let* ((plist (read (cscrape:buffer-stream bufs)))
                              (signature-text (cscrape:read-string-to-character bufs #\) t)))
                         (make-instance 'tags:expose-internal-method-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :signature-text% signature-text))))
  (add-tag-handler *tag-handlers* 'expose-external-function-tag "EXTERN_DEFUN"
                   #'(lambda (bufs)
                       (declare (optimize debug))
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:expose-external-function-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :pointer% (getf plist :pointer)))))
  (add-tag-handler *tag-handlers* 'expose-external-method-tag "EXTERN_DEFMETHOD"
                   #'(lambda (bufs)
                       (declare (optimize debug))
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:expose-external-method-tag
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
  (add-tag-handler *tag-handlers* 'begin-enum-tag "BEGIN_ENUM_TAG"
                   #'(lambda (bufs)
                       (declare (optimize (debug 3)))
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:begin-enum-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :type% (getf plist :type)
                                        :symbol% (getf plist :symbol)
                                        :description% (getf plist :description)))))
  (add-tag-handler *tag-handlers* 'value-enum-tag "VALUE_ENUM_TAG"
                   #'(lambda (bufs)
                       (declare (optimize (debug 3)))
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:value-enum-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :symbol% (getf plist :symbol)
                                        :value% (getf plist :value)))))
  (add-tag-handler *tag-handlers* 'end-enum-tag "END_ENUM_TAG"
                   #'(lambda (bufs)
                       (declare (optimize (debug 3)))
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:end-enum-tag
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
  (add-tag-handler *tag-handlers* 'initializer-tag "EXPOSE_INITIALIZE"
                   #'(lambda (bufs)
                       (let* ((plist (read (cscrape:buffer-stream bufs)))
                              (signature-text (cscrape:read-string-to-character bufs #\) t)))
                         (make-instance 'tags:initializer-tag
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
