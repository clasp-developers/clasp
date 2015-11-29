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


(defclass lambda-tag ()
  ((lambda-list :initarg :lambda-list :accessor lambda-list)))

(defclass docstring-tag ()
  ((docstring :initarg :docstring :accessor docstring)))

(defclass declare-tag ()
  ((declare-form :initarg :declare-form :accessor declare-form)))

(defclass expose-function-tag ()
  ((file :initarg :file :accessor file)
   (line :initarg :line :accessor line)
   (signature-text :initarg :signature-text :accessor signature-text)))

(defclass namespace-tag ()
  ((namespace :initarg :namespace :accessor namespace)))


(defmacro add-tag-handler (tag-handlers begin-tag code)
  `(setf (gethash ,begin-tag ,tag-handlers)
         (make-instance 'tag-handler
                        :begin-tag ,begin-tag
                        :handler-code ,code)))

(defun make-handler-hash-table ()
  (declare (optimize (debug 3)))
  (let ((handlers (make-hash-table :test #'equal)))
    (add-tag-handler handlers "LAMBDA_BEGIN"
                     #'(lambda (bufs) ;(declare (core:lambda-name lambda-tag-handler))
                         (prog1
                             (make-instance 'tags:lambda-tag
                                            :lambda-list (read (cscrape:buffer-stream bufs)))
                           (cscrape:skip-tag bufs cscrape:*end-tag*))))
    (add-tag-handler handlers "DOCSTRING_BEGIN"
                     #'(lambda (bufs) ;(declare (core:lambda-name docstring-tag-handler))
                         (prog1
                             (make-instance 'tags:docstring-tag
                                            :docstring (read (cscrape:buffer-stream bufs)))
                           (cscrape:skip-tag bufs cscrape:*end-tag*))))

    (add-tag-handler handlers "DECLARE_BEGIN"
                     #'(lambda (bufs) ;(declare (core:lambda-name declare-tag-handler))
                         (prog1
                             (make-instance 'tags:declare-tag
                                            :declare-form (read (cscrape:buffer-stream bufs)))
                           (cscrape:skip-tag bufs cscrape:*end-tag*))))

    (add-tag-handler handlers "EXPOSE_FUNCTION"
                     #'(lambda (bufs) ;(declare (core:lambda-name expose-function-tag-handler))
                         (let ((file (read (cscrape:buffer-stream bufs)))
                               (line (read (cscrape:buffer-stream bufs)))
                               (signature-text (cscrape:read-string-to-character bufs #\) t)))
                           (make-instance 'tags:expose-function-tag
                                          :file file
                                          :line line
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
    handlers))
