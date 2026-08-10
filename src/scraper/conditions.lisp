(in-package :cscrape)

(define-condition interpret-error () ())

(define-condition missing-base (interpret-error)
  ((tag :initarg :tag :accessor tag))
  (:report (lambda (condition stream)
             (format stream "~a Missing base for ~a ~a."
                     (tags:source-pos (tag condition))
                     (tags:tag-code (tag condition))
                     (tags:identifier (tag condition))))))

(define-condition tag-error (interpret-error)
  ((message :initarg :message :accessor message)
   (tag :initarg :tag :accessor tag)
   (message-args :initform nil :initarg :message-args :accessor message-args))
  (:report (lambda (condition stream)
             (format stream "~a ~a."
                     (tags:source-pos (tag condition))
                     (apply #'format nil (message condition) (message-args condition))))))

(define-condition bad-pointer (interpret-error)
  ((pointer-text :initarg :pointer-text :accessor pointer-text))
  (:report (lambda (condition stream)
             (format stream "~a Bad pointer text ~a."
                     (tags:source-pos (tag condition))
                     (tags:pointer% (tag condition))))))

(define-condition missing-namespace (interpret-error)
  ((tag :initarg :tag :accessor tag))
  (:report (lambda (condition stream)
             (format stream "~a Missing namespace for ~a ~a."
                     (tags:source-pos (tag condition))
                     (tags:tag-code (tag condition))
                     (tags:identifier (tag condition))))))

(define-condition namespace-mismatch (interpret-error)
  ((tag :initarg :tag :accessor tag))
  (:report (lambda (condition stream)
             (format stream "~a Mismatch between enclosed namespace and ~a ~a"
                     (tags:source-pos (tag condition))
                     (tags:tag-code (tag condition))
                     (tags:identifier (tag condition))))))

(defun error-if-bad-expose-info-setup* (tag other-tag)
  (declare (optimize (speed 3)))
  (unless (and (string= (tags:file% tag) (tags:file% other-tag))
               (< (- (tags:line% tag) (tags:line% other-tag)) 20))
    (error 'bad-cl-defun/defmethod :tag tag :other-tag other-tag)))

(defun error-if-bad-expose-info-setup (tag cur-name cur-lambda cur-declare cur-docstring cur-docstring-long &optional cur-priority)
  (when cur-name (error-if-bad-expose-info-setup* tag cur-name))
  (when cur-lambda (error-if-bad-expose-info-setup* tag cur-lambda))
  (when cur-declare (error-if-bad-expose-info-setup* tag cur-declare))
  (when cur-docstring (error-if-bad-expose-info-setup* tag cur-docstring))
  (when cur-docstring-long (error-if-bad-expose-info-setup* tag cur-docstring-long))
  (when cur-priority (error-if-bad-expose-info-setup* tag cur-priority))  )

;;; A WARNING, not an INTERPRET-ERROR like everything above, because the check that signals it
;;; is a heuristic - it cannot do C++ name resolution, so it can flag a type that is genuinely
;;; visible at global scope.  Re-parent it to INTERPRET-ERROR if it turns out to be clean
;;; across the tree.
(define-condition unqualified-signature-type (warning)
  ((tag :initarg :tag :accessor tag)
   (kind :initarg :kind :accessor kind)
   (type-name :initarg :type-name :accessor type-name)
   (namespace :initarg :namespace :accessor namespace))
  (:report (lambda (condition stream)
             (format stream "~a ~(~a~) type ~s is unqualified - the generated wrapper is ~
                             emitted outside namespace ~a.  Did you mean ~a::~a?"
                     (tags:source-pos (tag condition))
                     (kind condition)
                     (type-name condition)
                     (namespace condition)
                     (namespace condition)
                     (string-trim "*& " (type-name condition))))))
