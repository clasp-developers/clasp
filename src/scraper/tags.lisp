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
  (with-input-from-string (sin sig)
    (let ((*readtable* (copy-readtable)))
      (setf (readtable-case *readtable*) :preserve)
      (let ((return-type (read sin))
            (function-name (read sin)))
        (declare (ignore return-type))
        (string function-name)))))

(defclass tag-handler ()
  ((begin-tag :initarg :begin-tag)
   (handler-code :initarg :handler-code :accessor handler-code)))

(defclass lisp-base-tag ()
  ((c++-base% :initarg :c++-base% :accessor c++-base%)))

(defclass lisp-class-tag ()
  ((namespace% :initarg :namespace% :accessor namespace%)
   (package% :initarg :package% :accessor package%)
   (c++-base% :initarg :c++-base% :accessor c++-base%)
   (c++-class% :initarg :c++-class% :accessor c++-class%)
   (class-symbol% :initarg :class-symbol% :accessor class-symbol%)))

(defclass lambda-tag ()
  ((lambda-list :initarg :lambda-list :accessor lambda-list)))

(defclass docstring-tag ()
  ((docstring :initarg :docstring :accessor docstring)))

(defclass declare-tag ()
  ((declare-form :initarg :declare-form :accessor declare-form)))

(defclass source-tag-mixin ()
  ((file :initarg :file :accessor file)
   (line :initarg :line :accessor line)
   (character-offset :initarg :character-offset :accessor character-offset)))
  
(defclass expose-code-tag (source-tag-mixin)
  ((function-name :initarg :function-name :accessor function-name)
   (signature-text :initarg :signature-text :accessor signature-text)
   (lambda-tag :initarg :lambda-tag :accessor lambda-tag)
   (declare-tag :initarg :declare-tag :accessor declare-tag)
   (docstring-tag :initarg :docstring-tag :accessor docstring-tag)
   (namespace-tag :initarg :namespace-tag :accessor namespace-tag)))

(defclass expose-function-tag (expose-code-tag) ())

(defclass expose-method-tag (expose-code-tag)
  ((class-tag :initarg :class-tag :accessor class-tag)))

(defclass namespace-tag ()
  ((namespace :initarg :namespace :accessor namespace)))

(defclass symbol-tag ()
  ((namespace% :initarg :namespace% :accessor namespace%)
   (package% :initarg :package% :accessor package%)
   (c++-name% :initarg :c++-name% :accessor c++-name%)
   (name% :initarg :name% :accessor name%)
   (exported% :initarg :exported% :accessor exported%)))

(defclass namespace-package-association-tag ()
  ((namespace :initarg :namespace :accessor namespace)
   (package-var :initarg :package-var :accessor package-var)
   (package-str :initarg :package-str :accessor package-str)))

(defmacro add-tag-handler (tag-handlers begin-tag code)
  `(setf (gethash ,begin-tag ,tag-handlers)
         (make-instance 'tag-handler
                        :begin-tag ,begin-tag
                        :handler-code ,code)))

(defun make-handler-hash-table ()
  (declare (optimize (debug 3)))
  (let ((handlers (make-hash-table :test #'equal)))
    (add-tag-handler handlers "LISP_BASE"
                     #'(lambda (bufs) ;(declare (core:lambda-name lambda-tag-handler))
                         (make-instance 'tags:lisp-base-tag
                                        :c++-base% (cscrape:read-string-to-tag bufs cscrape:*end-tag*))))
    (add-tag-handler handlers "LISP_CLASS"
                     #'(lambda (bufs) ;(declare (core:lambda-name lambda-tag-handler))
                         (prog1
                             (make-instance 'tags:lisp-class-tag
                                            :namespace% (read (cscrape:buffer-stream bufs))
                                            :package% (read (cscrape:buffer-stream bufs))
                                            :c++-class% (read (cscrape:buffer-stream bufs))
                                            :class-symbol% (read (cscrape:buffer-stream bufs)))
                           (cscrape:skip-tag bufs cscrape:*end-tag*))))
    (add-tag-handler handlers "LISP_EXTERNAL_CLASS"
                     #'(lambda (bufs) ;(declare (core:lambda-name lambda-tag-handler))
                         (prog1
                             (let ((namespace (read (cscrape:buffer-stream bufs)))
                                   (package (read (cscrape:buffer-stream bufs)))
                                   (external-class (read (cscrape:buffer-stream bufs)))
                                   (c++-class (read (cscrape:buffer-stream bufs)))
                                   (class-symbol (read (cscrape:buffer-stream bufs)))
                                   (c++-base (cscrape:read-string-to-tag bufs cscrape:*end-tag*)))
                               (make-instance 'tags:lisp-class-tag
                                              :namespace% namespace
                                              :package% package
                                              :c++-class% c++-class
                                              :class-symbol% class-symbol
                                              :c++-base% c++-base))
                           (cscrape:skip-tag bufs cscrape:*end-tag*))))
    (add-tag-handler handlers "LAMBDA_BEGIN"
                     #'(lambda (bufs) ;(declare (core:lambda-name lambda-tag-handler))
                         (prog1
                             (make-instance 'tags:lambda-tag
                                            :lambda-list (read (cscrape:buffer-stream bufs)))
                           (cscrape:skip-tag bufs cscrape:*end-tag*))))
    (add-tag-handler handlers "DOCSTRING_BEGIN"
                     #'(lambda (bufs) ;(declare (core:lambda-name docstring-tag-handler))
                         (let ((docstr (cscrape:read-string-to-tag bufs cscrape:*end-tag*)))
                               (make-instance 'tags:docstring-tag
                                              :docstring docstr))))
    (add-tag-handler handlers "DECLARE_BEGIN"
                     #'(lambda (bufs) ;(declare (core:lambda-name declare-tag-handler))
                         (prog1
                             (make-instance 'tags:declare-tag
                                            :declare-form (read (cscrape:buffer-stream bufs)))
                           (cscrape:skip-tag bufs cscrape:*end-tag*))))
    (add-tag-handler handlers "EXPOSE_FUNCTION"
                     #'(lambda (bufs) ;(declare (core:lambda-name expose-function-tag-handler))
                         (let* ((file (read (cscrape:buffer-stream bufs)))
                                (line (read (cscrape:buffer-stream bufs)))
                                (signature-text (cscrape:read-string-to-character bufs #\) t))
                                (function-name (extract-function-name-from-signature signature-text)))
                           (make-instance 'tags:expose-function-tag
                                          :file file
                                          :line line
                                          :function-name function-name
                                          :signature-text signature-text))))
    (add-tag-handler handlers "NAMESPACE"
                     #'(lambda (bufs) ;(declare (core:lambda-name namespace-tag-handler))
                         (let* ((cur-pos (file-position (cscrape:buffer-stream bufs)))
                                (next (cscrape:read-string-to-character bufs #\space))
                                (c++namespace-keyword (string-trim " " next))
                                (namespace-name (cscrape:read-string-to-character bufs #\space)))
                           (unless (string= c++namespace-keyword "namespace")
                             (let ((cur-pos-context (subseq (cscrape:buffer bufs) cur-pos (+ cur-pos 128))))
                               (error 'bad-tag :tag "NAMESPACE"
                                      :context cur-pos-context )))
                           (make-instance 'tags:namespace-tag
                                          :namespace namespace-name))))
    (add-tag-handler handlers "SYMBOL_INTERNAL"
                     #'(lambda (bufs) ;(declare (core:lambda-name namespace-tag-handler))
                         (let* ((cur-pos (file-position (cscrape:buffer-stream bufs)))
                                (pkg-name (cscrape:read-string-to-character bufs #\space))
                                (sym-name (cscrape:read-string-to-character bufs #\space)))
                           (declare (ignore cur-pos))
                           (cscrape:skip-tag bufs cscrape:*end-tag*)
                           (make-instance 'tags:symbol-tag
                                          :package% pkg-name
                                          :exported% nil
                                          :name% sym-name))))
    (add-tag-handler handlers "SYMBOL_EXTERNAL"
                     #'(lambda (bufs) ;(declare (core:lambda-name namespace-tag-handler))
                         (let* ((cur-pos (file-position (cscrape:buffer-stream bufs)))
                                (pkg-name (cscrape:read-string-to-character bufs #\space))
                                (sym-name (cscrape:read-string-to-character bufs #\space)))
                           (declare (ignore cur-pos))
                           (cscrape:skip-tag bufs cscrape:*end-tag*)
                           (make-instance 'tags:symbol-tag
                                          :package% pkg-name
                                          :exported% t
                                          :name% sym-name))))
    (add-tag-handler handlers "SYMBOL_INTERN"
                     #'(lambda (bufs) ;(declare (core:lambda-name namespace-tag-handler))
                         (let* ((cur-pos (file-position (cscrape:buffer-stream bufs)))
                                (ns-name (cscrape:read-string-to-character bufs #\space))
                                (sym-name (cscrape:read-string-to-character bufs #\space)))
                           (declare (ignore cur-pos))
                           (cscrape:skip-tag bufs cscrape:*end-tag*)
                           (make-instance 'tags:symbol-tag
                                          :namespace% ns-name
                                          :exported% t
                                          :name% sym-name))))
    (add-tag-handler handlers "NAMESPACE_PACKAGE_ASSOCIATION"
                     #'(lambda (bufs) ;(declare (core:lambda-name namespace-tag-handler))
                         (let* ((cur-pos (file-position (cscrape:buffer-stream bufs)))
                                (ns-name (cscrape:read-string-to-character bufs #\space))
                                (pkg (cscrape:read-string-to-character bufs #\space))
                                (pkg-name (cscrape:read-string-to-character bufs #\space)))
                           (declare (ignore cur-pos))
                           (cscrape:skip-tag bufs cscrape:*end-tag*)
                           (make-instance 'tags:namespace-package-association-tag
                                          :namespace ns-name
                                          :package-var pkg
                                          :package-str pkg-name))))
    handlers))
