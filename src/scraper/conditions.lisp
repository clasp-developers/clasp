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

(defun error-if-bad-expose-info-setup (tag cur-name cur-lambda cur-declare cur-docstring)
  (when cur-name (error-if-bad-expose-info-setup* tag cur-name))
  (when cur-lambda (error-if-bad-expose-info-setup* tag cur-lambda))
  (when cur-declare (error-if-bad-expose-info-setup* tag cur-declare))
  (when cur-docstring (error-if-bad-expose-info-setup* tag cur-docstring)))
