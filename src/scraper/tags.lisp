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
  (let* ((tsig (string-trim '(#\newline #\space #\tab) sig))
         (first-space (position-if
                       (lambda (c) (or (char= c #\newline) (char= c #\space) (char= c #\tab)))
                       tsig))
         (open-paren (position #\( tsig :test #'char=)))
    (string-trim '(#\newline #\space #\tab) (subseq tsig first-space open-paren))))
    
(defun extract-method-name-from-signature (sig)
  (let* ((class-method-name (extract-function-name-from-signature sig))
         (colon-colon-pos (search "::" class-method-name :test #'string=))
         (class-name (subseq class-method-name 0 colon-colon-pos))
         (method-name (subseq class-method-name (+ 2 colon-colon-pos) nil)))
    (values class-name method-name)))

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

(defclass code-tag ()
  ((file :initarg :file :accessor file)
   (line :initarg :line :accessor line)))
  
(defclass lambda-tag (code-tag)
  ((lambda-list :initarg :lambda-list :accessor lambda-list)))

(defclass docstring-tag (code-tag)
  ((docstring :initarg :docstring :accessor docstring)))

(defclass declare-tag (code-tag)
  ((declare-form :initarg :declare-form :accessor declare-form)))

(defclass source-tag (code-tag)
  ((character-offset :initarg :character-offset :accessor character-offset)))
  
(defclass expose-code-tag (source-tag)
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
    (add-tag-handler handlers "LISP_BASE1_TAG"
                     #'(lambda (bufs) ;(declare (core:lambda-name lambda-tag-handler))
                         (let ((plist (read (cscrape:buffer-stream bufs))))
                           (make-instance 'tags:lisp-base-tag
                                          :c++-base% (getf plist :base)))))
    (add-tag-handler handlers "LISP_CLASS_TAG"
                     #'(lambda (bufs) ;(declare (core:lambda-name lambda-tag-handler))
                         (let ((plist (read (cscrape:buffer-stream bufs))))
                           (make-instance 'tags:lisp-class-tag
                                          :namespace% (getf plist :namespace)
                                          :package% (getf plist :package)
                                          :c++-class% (getf plist :class)
                                          :class-symbol% (getf plist :class-symbol)))))
    (add-tag-handler handlers "LISP_EXTERNAL_CLASS_TAG"
                     #'(lambda (bufs) ;(declare (core:lambda-name lambda-tag-handler))
                         (let ((plist (read (cscrape:buffer-stream bufs))))
                             (make-instance 'tags:lisp-class-tag
                                            :namespace% (getf plist :namespace)
                                            :package% (getf plist :package)
                                            :c++-class% (getf plist :class)
                                            :class-symbol% (getf plist :class-symbol)
                                            :c++-base% (getf plist :base)))))
    (add-tag-handler handlers "LAMBDA_TAG"
                     #'(lambda (bufs) ;(declare (core:lambda-name lambda-tag-handler))
                         (let ((plist (read (cscrape:buffer-stream bufs))))
                           (make-instance 'tags:lambda-tag
                                          :file (getf plist :file)
                                          :line (getf plist :line)
                                          :lambda-list (getf plist :lambda-list)))))
    (add-tag-handler handlers "DOCSTRING_TAG"
                     #'(lambda (bufs) ;(declare (core:lambda-name docstring-tag-handler))
                         (let ((plist (read (cscrape:buffer-stream bufs)))
                               (docstring (cscrape:read-string-to-tag bufs cscrape:*end-tag*)))
                           (make-instance 'tags:docstring-tag
                                          :file (getf plist :file)
                                          :line (getf plist :line)
                                          :docstring docstring))))
    (add-tag-handler handlers "DECLARE_TAG"
                     #'(lambda (bufs) ;(declare (core:lambda-name declare-tag-handler))
                         (let ((plist (read (cscrape:buffer-stream bufs))))
                           (make-instance 'tags:declare-tag
                                          :file (getf plist :file)
                                          :line (getf plist :line)
                                          :declare-form (getf plist :declare)))))
    (add-tag-handler handlers "EXPOSE_FUNCTION"
                     #'(lambda (bufs) ;(declare (core:lambda-name expose-function-tag-handler))
;;                         (format t "EXPOSE_FUNCTION ~a~%" (cscrape::buffer-peek bufs))
                         (let* ((plist (read (cscrape:buffer-stream bufs)))
                                (signature-text (cscrape:read-string-to-character bufs #\) t))
                                (function-name (extract-function-name-from-signature signature-text)))
                           (make-instance 'tags:expose-function-tag
                                          :file (getf plist :file)
                                          :line (getf plist :line)
                                          :function-name function-name
                                          :signature-text signature-text))))
    (add-tag-handler handlers "EXPOSE_METHOD"
                     #'(lambda (bufs) ;(declare (core:lambda-name expose-function-tag-handler))
                         (let* ((plist (read (cscrape:buffer-stream bufs)))
                                (signature-text (cscrape:read-string-to-character bufs #\) t)))
                           (multiple-value-bind (class-name method-name)
                               (extract-method-name-from-signature signature-text)
                             (make-instance 'tags:expose-method-tag
                                            :file (getf plist :file)
                                            :line (getf plist :line)
                                            :class-name class-name
                                            :method-name function-name
                                            :signature-text signature-text)))))
    (add-tag-handler handlers "NAMESPACE_TAG"
                     #'(lambda (bufs) ;(declare (core:lambda-name namespace-tag-handler))
                         (let* ((cur-pos (file-position (cscrape:buffer-stream bufs)))
                                (c++namespace-keyword (let ((*package* (find-package :keyword)))
                                                        (read (cscrape:buffer-stream bufs))))
                                (namespace-name (string (let ((*readtable* (copy-readtable))
                                                              (*package* (find-package :keyword)))
                                                          (setf (readtable-case *readtable*) :preserve)
                                                          (read (cscrape:buffer-stream bufs))))))
                           (unless (eq c++namespace-keyword :namespace)
                             (let ((cur-pos-context (subseq (cscrape:buffer bufs) cur-pos (+ cur-pos 128))))
                               (error 'bad-tag :tag "NAMESPACE_TAG"
                                      :context cur-pos-context )))
                           (make-instance 'tags:namespace-tag
                                          :namespace namespace-name))))
    (add-tag-handler handlers "SYMBOL_INTERNAL"
                     #'(lambda (bufs) ;(declare (core:lambda-name namespace-tag-handler))
                         (let* ((plist (read (cscrape:buffer-stream bufs))))
                           (make-instance 'tags:symbol-tag
                                          :package% (getf plist :PACKAGE)
                                          :exported% nil
                                          :name% (getf plist :NAME)))))
    (add-tag-handler handlers "SYMBOL_EXTERNAL"
                     #'(lambda (bufs) ;(declare (core:lambda-name namespace-tag-handler))
                         (let* ((plist (read (cscrape:buffer-stream bufs))))
                           (make-instance 'tags:symbol-tag
                                          :package% (getf plist :PACKAGE)
                                          :exported% t
                                          :name% (getf plist :NAME)))))
    (add-tag-handler handlers "SYMBOL_INTERN"
                     #'(lambda (bufs) ;(declare (core:lambda-name namespace-tag-handler))
                         (let* ((plist (read (cscrape:buffer-stream bufs))))
                           (make-instance 'tags:symbol-tag
                                          :namespace% (getf plist :namespace)
                                          :exported% t
                                          :name% (getf plist :name)))))
    (add-tag-handler handlers "NAMESPACE_PACKAGE_ASSOCIATION_TAG"
                     #'(lambda (bufs) ;(declare (core:lambda-name namespace-tag-handler))
                         (let* ((plist (read (cscrape:buffer-stream bufs))))
                           (declare (ignore cur-pos))
                           (make-instance 'tags:namespace-package-association-tag
                                          :namespace (getf plist :namespace)
                                          :package-var (getf plist :package)
                                          :package-str (getf plist :package-name)))))
    handlers))
