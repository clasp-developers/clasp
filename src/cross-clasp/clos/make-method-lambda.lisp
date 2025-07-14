(in-package #:cross-clasp.clasp.clos)

(defgeneric %make-method-lambda
    (generic-function method lambda-expression environment))

;;; A sham environment that definitely doesn't have call-next-method or
;;; next-method-p bound, since we apparently can't rely on that. Not sure why.
(defclass cnmless ()
  ((%underlying :initarg :underlying :reader underlying)))

(defmethod trucler:describe-block (client (env cnmless) name)
  (trucler:describe-block client (underlying env) name))
(defmethod trucler:describe-declarations (client (env cnmless))
  (trucler:describe-declarations client (underlying env)))
(defmethod trucler:describe-function (client (env cnmless) name)
  (case name
    ((call-next-method next-method-p) nil)
    (t (trucler:describe-function client (underlying env) name))))
(defmethod trucler:describe-optimize (client (env cnmless))
  (trucler:describe-optimize client (underlying env)))
(defmethod trucler:describe-tag (client (env cnmless) name)
  (trucler:describe-tag client (underlying env) name))
(defmethod trucler:describe-variable (client (env cnmless) name)
  (trucler:describe-variable client (underlying env) name))
(defmethod trucler:global-environment (client (env cnmless))
  (trucler:global-environment client (underlying env)))

;;; Return two values indicating the use of CALL-NEXT-METHOD and NEXT-METHOD-P
;;; in the lambda. Each value can be either T, meaning used in a call, or
;;; FUNCTION, meaning used generally (e.g. by #'call-next-method), or NIL meaning
;;; not referenced at all.
;;; To do this, we compile the body in the environment, and note
;;; if there are unresolved references to call-next-method or next-method-p.
;;; We run silent by suppressing all warnings and errors. If compilation fails
;;; we assume the worst (that it does refer to next methods, somehow).
;;; T is never returned for either value because we cannot distinguish the ways
;;; in which the functions are referenced, but Clasp uses the above convention.
(defun walk-method-lambda (method-lambda environment)
  (let ((cnm-p nil) (nmp-p nil)
        ;; Block compilation unit output on abort
        (*error-output* (make-broadcast-stream))
        (environment (make-instance 'cnmless :underlying environment)))
    (handler-bind ((maclina.compile:unknown-function
                     (lambda (c)
                       (case (maclina.compile:name c)
                         (call-next-method (setf cnm-p 'function))
                         (next-method-p (setf nmp-p 'function)))))
                   (warning #'muffle-warning)
                   (error (lambda (c)
                            (return-from walk-method-lambda
                              (values 'function 'function)))))
      ;; Compile without linking. This is important so that for example
      ;; it doesn't bother trying to resolve load-time-value forms.
      (maclina.compile:compile-into (maclina.compile:make-cmodule)
                                    method-lambda environment))
    (values cnm-p nmp-p)))

;;; Given a parsed lambda list, reconstruct it.
;;; This is used after getting the specializers out.
(defun reconstruct-lambda-list (required optional rest keys aokp aux keyp)
  `(,@required
    ,@(when optional '(&optional)) ,@(loop for opt in optional
                                           for (var default -p) = opt
                                           collect (if -p
                                                       opt
                                                       (list var default)))
    ,@(when rest `(&rest ,rest))
    ,@(when keyp '(&key)) ,@(loop for key in keys
                                  for ((keyword var) default -p) = key
                                  collect (if -p
                                              key
                                              (list (list keyword var) default)))
    ;; Keyword checking is done by the GF, so methods should not check again
    ,@(when (or keyp aokp) '(&allow-other-keys))
    ,@(when aux `(&aux ,@aux))))

(defun method-inner-lambda-list (lambda-list)
  (multiple-value-call #'reconstruct-lambda-list
   (alexandria:parse-ordinary-lambda-list lambda-list)))

(defmethod %make-method-lambda ((gf standard-generic-function)
                                (method standard-method)
                                lambda-expression environment)
  (declare (ignore gf method))
  (assert (typep lambda-expression '(cons (eql lambda) (cons list t))))
  (multiple-value-bind (cnm-p nmp-p)
      (walk-method-lambda lambda-expression environment)
    (let ((lambda-list (second lambda-expression)) (body (cddr lambda-expression)))
      (multiple-value-bind (body decls doc)
          (alexandria:parse-body body
                                 :documentation t :whole lambda-expression)
        (let ((lambda-name (loop for (declare . decs) in decls
                                 for p = (assoc 'core:lambda-name decs)
                                 when p return (second p))))
          (values
           `(lambda (.method-args. .next-methods.)
              ,@(when doc (list doc))
              ,@(when lambda-name `((declare (cross-clasp.clasp.core:lambda-name
                                              ,lambda-name))))
              (flet (,@(when cnm-p
                         `((call-next-method (&rest args)
                             (if (null .next-methods.)
                                 ;; FIXME: should call no-next-method.
                                 ;; This is hard, because the method doesn't exist
                                 ;; when this method is created.
                                 (error "No next method")
                                 (funcall (method-function (car .next-methods.))
                                          (if (null args) .method-args. args)
                                          (rest .next-methods.))))))
                     ,@(when nmp-p
                         `((next-method-p () (not (null .next-methods.))))))
                (apply (lambda (,(method-inner-lambda-list lambda-list))
                         ,@decls ,@body)
                       .method-args.)))
           (list ''call-next-method-p `',cnm-p ''next-method-p-p `',nmp-p)))))))
