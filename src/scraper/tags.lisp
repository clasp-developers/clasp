(in-package :tags)

(defvar *default-priority* 10
  "Default priority for defining functions is 10")

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

(defclass tags:cl-pregcstartup-tag (source-tag)
  ((character-offset% :initform nil :initarg :character-offset% :accessor tags:character-offset%)
   (signature-text% :initform nil :initarg :signature-text% :accessor signature-text%)))

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

(defclass tags:cl-priority-tag (tag)
  ((priority% :initarg :priority% :reader tags:priority%)))

(defun maybe-priority (tag)
  "Default priority is 10"
  (if tag
      (parse-integer (priority% tag))
      *default-priority*))


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

(defclass tags:cl-defun-setf-tag (internal-code-tag) ())

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

(defparameter *tag-handlers* (make-hash-table :test #'equal))
(defparameter *tag-codes* (make-hash-table :test #'equal))

(defun add-tag-handler (tag-class-symbol begin-tag code)
  (setf (gethash begin-tag *tag-handlers*)
        (make-instance 'tag-handler :begin-tag begin-tag :handler-code code)
        (gethash tag-class-symbol *tag-codes*)
        begin-tag))

;; Defines a tag handler that returns an instance of the given class
;; using the given initargs.
;; initargs for file and line are included already.
;; BUFS will be bound to the buffer and PLIST to the result of (read (cscrape:buffer-stream bufs))
;;  in the environment the key values are evaluated in.
(defmacro define-tag-handler (tag-class-symbol begin-tag tag-class &rest keys)
  `(add-tag-handler ',tag-class-symbol ',begin-tag
                    (lambda (bufs)
                      (let ((plist (read (cscrape:buffer-stream bufs))))
                        (make-instance ',tag-class
                                       :file% (getf plist :file)
                                       :line% (getf plist :line)
                                       ,@keys)))))

(defun wrapped-in-parentheses-p (str)
  (let ((sis (make-string-input-stream str)))
    (when (char= (elt str 0) #\()
      (read-char sis)
      (handler-case
          (handler-bind
              ((SB-INT:SIMPLE-READER-PACKAGE-ERROR
                 (lambda (err) 
                   (invoke-restart 'use-value (find-package :keyword)))))
            (read-delimited-list #\) sis))
        (end-of-file (err)
          (declare (ignore err))
          (return-from wrapped-in-parentheses-p nil)))
      (= (file-position sis) (length str)))))

(defun verify-lambda-list (orig-str)
  (if (> (length orig-str) 0)
      ;; Clip one level of parentheses
      (let ((str (string-trim " " orig-str)))
        (when (char= (elt str 0) #\")
          (setf str (read-from-string str)))
        (when (wrapped-in-parentheses-p str)
          (setf str (subseq str 1 (1- (length str)))))
        ;; Add a level of parentheses
        (let ((pstr (format nil "(~a)" str)))
          (unless (wrapped-in-parentheses-p pstr)
            (error "Invalid lambda list - there are unbalanced parentheses in pstr ~s" pstr)))
        str)
      orig-str))

(progn
  (define-tag-handler lisp-internal-class-tag "LISP_CLASS_TAG" tags:lisp-internal-class-tag
    :namespace% (getf plist :namespace)
    :package% (getf plist :package)
    :name% (getf plist :class)
    :base% (getf plist :base)
    :class-symbol% (getf plist :class-symbol))
  (define-tag-handler lisp-external-class-tag "LISP_EXTERNAL_CLASS_TAG" tags:lisp-external-class-tag
    :namespace% (getf plist :namespace)
    :package% (getf plist :package)
    :name% (getf plist :class)
    :class-symbol% (getf plist :class-symbol)
    :base% (getf plist :base))
  (define-tag-handler gc-managed-type-tag "GC_MANAGED_TYPE_TAG" tags:gc-managed-type-tag
    :c++type% (maybe-unquote (getf plist :type)))
  (define-tag-handler cl-lispify-name-tag "CL_LISPIFY_NAME_TAG" tags:cl-lispify-name-tag
    :name% (maybe-unquote (getf plist :cl-name)))
  (define-tag-handler gc-managed-type-tag "CL_NAME_TAG" tags:cl-name-tag
    :name% (maybe-unquote (getf plist :cl-name)))
  (define-tag-handler cl-pkg-name-tag "CL_PKG_NAME_TAG" tags:cl-pkg-name-tag
    :name% (maybe-unquote (getf plist :cl-name))
    :package% (getf plist :cl-pkg))
  (define-tag-handler meta-class-tag "META_CLASS_TAG" tags:meta-class-tag
    :meta-class% (getf plist :meta-class))
  (define-tag-handler cl-lambda-tag "CL_LAMBDA_TAG" tags:cl-lambda-tag
    :lambda-list% (let* ((raw-lambda-list (getf plist :lambda-list))
                         (lambda-list (verify-lambda-list raw-lambda-list)))
                    lambda-list))
  (define-tag-handler cl-docstring-tag "CL_DOCSTRING_TAG" tags:cl-docstring-tag
    :docstring% (cscrape:read-string-to-tag bufs cscrape:+end-tag+))
  (define-tag-handler cl-priority-tag "CL_PRIORITY_TAG" tags:cl-priority-tag
    :priority% (cscrape:read-string-to-tag bufs cscrape:+end-tag+))
  (define-tag-handler cl-declare-tag "CL_DECLARE_TAG" tags:cl-declare-tag
    :declare-form% (getf plist :declare))
  (define-tag-handler cl-defun-tag "CL_DEFUN_TAG" tags:cl-defun-tag
    :signature-text% (cscrape:read-string-to-character bufs #\) t))
  (define-tag-handler cl-defun-setf-tag "CL_DEFUN_SETF_TAG" tags:cl-defun-setf-tag
    :signature-text% (cscrape:read-string-to-character bufs #\) t))
  (define-tag-handler cl-defmethod-tag "CL_DEFMETHOD_TAG" tags:cl-defmethod-tag
    :signature-text% (cscrape:read-string-to-character bufs #\) t))
  (define-tag-handler cl-def-class-method-tag "CL_DEF_CLASS_METHOD_TAG" tags:cl-def-class-method-tag
    :signature-text% (cscrape:read-string-to-character bufs #\) t))
  (define-tag-handler cl-extern-defun-tag "CL_EXTERN_DEFUN_TAG" tags:cl-extern-defun-tag
    :pointer% (getf plist :pointer))
  (define-tag-handler cl-extern-defmethod-tag "CL_EXTERN_DEFMETHOD_TAG" tags:cl-extern-defmethod-tag
    :class-name% (getf plist :type)
    :pointer% (getf plist :pointer))
  (define-tag-handler symbol-internal-tag "SYMBOL_INTERNAL" tags:symbol-internal-tag
    :package% (getf plist :PACKAGE)
    :lisp-name% (getf plist :NAME))
  (define-tag-handler symbol-external-tag "SYMBOL_EXTERNAL" tags:symbol-external-tag
    :package% (getf plist :PACKAGE)
    :lisp-name% (getf plist :NAME)
    :c++-name% (getf plist :NAME))
  (define-tag-handler symbol-shadow-external-tag "SYMBOL_SHADOW_EXTERNAL" tags:symbol-shadow-external-tag
    :package% (getf plist :PACKAGE)
    :lisp-name% (getf plist :NAME)
    :c++-name% (getf plist :NAME))
  (define-tag-handler detailed-symbol-external-tag "DETAILED_SYMBOL_EXTERNAL" tags:detailed-symbol-external-tag
    :package% (getf plist :PACKAGE)
    :c++-name% (getf plist :CXX-NAME)
    :lisp-name% (getf plist :NAME))
  (define-tag-handler symbol-intern-tag "SYMBOL_INTERN" tags:symbol-intern-tag
    :namespace% (getf plist :namespace)
    :c++-name% (getf plist :name)
    :lisp-name% (getf plist :name))
  (define-tag-handler cl-begin-enum-tag "CL_BEGIN_ENUM_TAG" tags:cl-begin-enum-tag
    :type% (getf plist :type)
    :symbol% (getf plist :symbol)
    :description% (getf plist :description))
  (define-tag-handler cl-value-enum-tag "CL_VALUE_ENUM_TAG" tags:cl-value-enum-tag
    :symbol% (getf plist :symbol)
    :value% (getf plist :value))
  (define-tag-handler cl-end-enum-tag "CL_END_ENUM_TAG" tags:cl-end-enum-tag
    :symbol% (getf plist :symbol))
  (define-tag-handler package-use-tag "PACKAGE_USE_TAG" tags:package-use-tag
    :name% (getf plist :name))
  (define-tag-handler package-shadow-tag "PACKAGE_SHADOW_TAG" tags:package-shadow-tag
    :name% (getf plist :name))
  (define-tag-handler package-nickname-tag "PACKAGE_NICKNAME_TAG" tags:package-nickname-tag
    :name% (getf plist :name))
  (define-tag-handler namespace-package-association-tag "NAMESPACE_PACKAGE_ASSOCIATION_TAG"
    tags:namespace-package-association-tag
    :namespace% (getf plist :namespace)
    :package% (getf plist :package)
    :package-str% (getf plist :package-name))
  (define-tag-handler cl-pregcstartup-tag "CL_PRE_GC_STARTUP_TAG" tags:cl-pregcstartup-tag
    :signature-text% (cscrape:read-string-to-character bufs #\) t))  
  (define-tag-handler cl-initializer-tag "CL_INITIALIZER_TAG" tags:cl-initializer-tag
    :signature-text% (cscrape:read-string-to-character bufs #\) t))
  ;; Handle more tags here
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
