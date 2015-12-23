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

(defun extract-function-name-from-signature (sig)
  (declare (optimize (debug 3)))
  (let* ((tsig (string-trim '(#\newline #\space #\tab #\*) sig))
         (first-space (position-if
                       (lambda (c) (or (char= c #\newline)
                                       (char= c #\space)
                                       (char= c #\tab)))
                       tsig))
         (open-paren (position #\( tsig :test #'char=)))
    ;; trim * to avoid pointers
    (string-trim '(#\newline #\space #\tab #\*) (subseq tsig first-space open-paren))))
    
(defun extract-method-name-from-signature (sig)
  (declare (optimize debug))
  (let* ((trimmed-sig (string-trim '(#\newline #\space #\tab) sig))
         (just-sig (if (string= (subseq trimmed-sig 0 7) "virtual") ;; trim out virtual
                       (subseq trimmed-sig 7)
                       trimmed-sig))
         (class-method-name (extract-function-name-from-signature just-sig)))
   (let* ((colon-colon-pos (search "::" class-method-name :test #'string=))
          (class-name (and colon-colon-pos (subseq class-method-name 0 colon-colon-pos)))
          (method-name (if colon-colon-pos
                           (subseq class-method-name (+ 2 colon-colon-pos) nil)
                           class-method-name)))
      (values class-name method-name))))

(defclass tag-handler ()
  ((begin-tag :initarg :begin-tag)
   (handler-code :initarg :handler-code :accessor handler-code)))

(defclass tags:tag ()
  ((file% :initarg :file% :reader tags:file%)
   (line% :initarg :line% :reader tags:line%)))

(defclass tags:lisp-base-tag (tag)
  ((name% :initform nil :initarg :name% :reader tags:name%)))

(defclass tags:source-tag (tag)
  ((character-offset% :initform nil :initarg :character-offset% :reader tags:character-offset%)))

(defclass tags:lisp-class-tag (source-tag)
  ((namespace% :initform nil :initarg :namespace% :reader tags:namespace%)
   (package% :initform nil :initarg :package% :reader tags:package%)
   (name% :initarg :name% :reader tags:name%)
   (class-symbol% :initarg :class-symbol% :reader tags:class-symbol%)))

(defclass tags:lisp-internal-class-tag (lisp-class-tag) ())

(defclass tags:lisp-external-class-tag (lisp-class-tag)
  ((c++-base% :initform nil :initarg :c++-base% :reader tags:c++-base%)))

(defclass tags:name-tag (tag)
  ((name% :initarg :name% :reader tags:name%)))

(defclass tags:lambda-tag (tag)
  ((lambda-list% :initarg :lambda-list% :reader tags:lambda-list%)))

(defclass tags:docstring-tag (tag)
  ((docstring% :initarg :docstring% :reader tags:docstring%)))

(defclass tags:declare-tag (tag)
  ((declare-form% :initarg :declare-form% :reader tags:declare-form%)))

(defclass tags:extern-defun-tag (source-tag)
  ((pointer% :initarg :pointer% :reader tags:pointer%)))

(defclass tags:extern-defmethod-tag (extern-defun-tag)
  ((type% :initarg :type% :reader tags:type%)))
  
(defclass tags:expose-code-tag (source-tag)
  ((signature-text% :initarg :signature-text% :reader tags:signature-text%)))

(defclass tags:expose-function-tag (expose-code-tag)
  ((function-name% :initarg :function-name% :reader tags:function-name%)))

(defclass tags:expose-method-tag (expose-code-tag)
  ((class-name% :initform nil :initarg :class-name% :reader tags:class-name%)
   (method-name% :initarg :method-name% :reader tags:method-name%)))

(defclass tags:namespace-tag (tag)
  ((namespace% :initarg :namespace% :reader tags:namespace%)))

(defclass tags:symbol-tag (tag)
  ((namespace% :initform nil :initarg :namespace% :reader tags:namespace%)
   (package% :initform nil :initarg :package% :reader tags:package%)
   (c++-name% :initform nil :initarg :c++-name% :reader tags:c++-name%)
   (name% :initarg :name% :reader tags:name%)
   (exported% :initarg :exported% :reader tags:exported%)))

(defclass tags:namespace-package-association-tag (tag)
  ((namespace% :initarg :namespace% :reader tags:namespace%)
   (package-var% :initarg :package-var% :reader tags:package-var%)
   (package-str% :initarg :package-str% :reader tags:package-str%)))

(defmacro add-tag-handler (tag-handlers tag-class-symbol begin-tag code)
  `(setf (gethash ,begin-tag ,tag-handlers) (make-instance 'tag-handler
                                                           :begin-tag ,begin-tag
                                                           :handler-code ,code)
         (gethash (,tag-class-symbol *tag-codes*) ,begin-tag)))

(progn
  (defparameter *tag-codes* (make-hash-table :test #'equal))
  (defparameter *tag-handlers* (make-hash-table :test #'equal))
  (add-tag-handler handlers 'lisp-base-tag "LISP_BASE1_TAG"
                   #'(lambda (bufs) ;(declare (core:lambda-name lambda-tag-handler))
                       (let ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:lisp-base-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :c++-base% (getf plist :base)))))
  (add-tag-handler handlers 'lisp-internal-class-tag "LISP_CLASS_TAG"
                   #'(lambda (bufs) ;(declare (core:lambda-name lambda-tag-handler))
                       (let ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:lisp-internal-class-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :namespace% (getf plist :namespace)
                                        :package% (getf plist :package)
                                        :name% (getf plist :class)
                                        :class-symbol% (getf plist :class-symbol)))))
  (add-tag-handler handlers 'lisp-external-class-tag "LISP_EXTERNAL_CLASS_TAG"
                   #'(lambda (bufs) ;(declare (core:lambda-name lambda-tag-handler))
                       (let ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:lisp-external-class-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :namespace% (getf plist :namespace)
                                        :package% (getf plist :package)
                                        :name% (getf plist :class)
                                        :class-symbol% (getf plist :class-symbol)
                                        :c++-base% (getf plist :base)))))
  (add-tag-handler handlers 'name-tag "NAME_TAG"
                   #'(lambda (bufs) ;(declare (core:lambda-name lambda-tag-handler))
                       (let ((plist (read (cscrape:buffer-stream bufs))))
                         (let ((cl-name (getf plist :cl-name)))
                           (make-instance 'tags:name-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :name% cl-name)))))
  (add-tag-handler handlers 'lambda-tag "LAMBDA_TAG"
                   #'(lambda (bufs) ;(declare (core:lambda-name lambda-tag-handler))
                       (let ((plist (read (cscrape:buffer-stream bufs))))
                         (let ((lambda-list (getf plist :lambda-list)))
                           (when (and (> (length lambda-list) 1)
                                      (char= (elt lambda-list 0) #\"))
                             (setf lambda-list (read-from-string lambda-list)))
                           (make-instance 'tags:lambda-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :lambda-list% lambda-list)))))
  (add-tag-handler handlers 'docstring-tag "DOCSTRING_TAG"
                   #'(lambda (bufs) ;(declare (core:lambda-name docstring-tag-handler))
                       (let ((plist (read (cscrape:buffer-stream bufs)))
                             (docstring (cscrape:read-string-to-tag bufs cscrape:*end-tag*)))
                         (make-instance 'tags:docstring-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :docstring% docstring))))
  (add-tag-handler handlers 'declare-tag "DECLARE_TAG"
                   #'(lambda (bufs) ;(declare (core:lambda-name declare-tag-handler))
                       (let ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:declare-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :declare-form% (getf plist :declare)))))
  (add-tag-handler handlers 'expose-function-tag "EXPOSE_FUNCTION"
                   #'(lambda (bufs) ;(declare (core:lambda-name expose-function-tag-handler))
                       ;;                         (format t "EXPOSE_FUNCTION ~a~%" (cscrape::buffer-peek bufs))
                       (let* ((plist (read (cscrape:buffer-stream bufs)))
                              (signature-text (cscrape:read-string-to-character bufs #\) t))
                              (function-name (extract-function-name-from-signature signature-text)))
                         (make-instance 'tags:expose-function-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :function-name% function-name
                                        :signature-text% signature-text))))
  (add-tag-handler handlers 'expose-method-tag "EXPOSE_METHOD"
                   #'(lambda (bufs) ;(declare (core:lambda-name expose-function-tag-handler))
                       (let* ((plist (read (cscrape:buffer-stream bufs)))
                              (signature-text (cscrape:read-string-to-character bufs #\) t)))
                         (multiple-value-bind (class-name method-name)
                             (extract-method-name-from-signature signature-text)
                           (make-instance 'tags:expose-method-tag
                                          :file% (getf plist :file)
                                          :line% (getf plist :line)
                                          :class-name% class-name
                                          :method-name% method-name
                                          :signature-text% signature-text)))))
  (add-tag-handler handlers 'symbol-internal-tag "SYMBOL_INTERNAL"
                   #'(lambda (bufs) ;(declare (core:lambda-name namespace-tag-handler))
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:symbol-internal-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :package% (getf plist :PACKAGE)
                                        :name% (getf plist :NAME)))))
  (add-tag-handler handlers 'symbol-external-tag "SYMBOL_EXTERNAL"
                   #'(lambda (bufs) ;(declare (core:lambda-name namespace-tag-handler))
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:symbol-external-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :package% (getf plist :PACKAGE)
                                        :name% (getf plist :NAME)))))
  (add-tag-handler handlers 'symbol-intern-tag "SYMBOL_INTERN"
                   #'(lambda (bufs) ;(declare (core:lambda-name namespace-tag-handler))
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:symbol-intern-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :namespace% (getf plist :namespace)
                                        :name% (getf plist :name)))))
  (add-tag-handler handlers 'namespace-package-association-tag
                   "NAMESPACE_PACKAGE_ASSOCIATION_TAG"
                   #'(lambda (bufs) ;(declare (core:lambda-name namespace-tag-handler))
                       (let* ((plist (read (cscrape:buffer-stream bufs))))
                         (make-instance 'tags:namespace-package-association-tag
                                        :file% (getf plist :file)
                                        :line% (getf plist :line)
                                        :namespace% (getf plist :namespace)
                                        :package-var% (getf plist :package)
                                        :package-str% (getf plist :package-name))))))

(defun tags:code (tag)
  (let ((tag-class-symbol (class-name (class-of tag))))
    (gethash tag-class-symbol *tag-codes*)))

(defgeneric tags:identifier (tag))
(defmethod identifier ((tag t)) "")
(defmethod identifier ((tag lisp-class-tag))
  "* Description 
Return the name of the class for the tag."
  (c++-class tag))

(defgeneric tags:source-pos (tag))
(defmethod source-pos ((tag tag))
  (format nil "~a:~d" (file% tag) (line% tag)))
