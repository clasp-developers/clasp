(in-package #:clasp-sandbox)

(defmethod sicl-genv:setf-expander (symbol clasp-cleavir:clasp-global-environment)
  ;; SICL and clasp give setf expanders different arguments, so we have to translate.
  (let ((f (system:get-sysprop symbol 'system::setf-method)))
    (if f
        (lambda (form env)
          (apply f env (rest form)))
        nil)))
(defmethod sicl-genv:setf-expander (symbol (environment null))
  (sicl-genv:setf-expander symbol clasp-cleavir:*clasp-env*))

(defmethod sicl-genv:default-setf-expander ((environment clasp-cleavir:clasp-global-environment))
  (lambda (form env)
    (declare (ignore env))
    (if (symbolp form)
        (let ((store (gensym "store-get-setf-expansion")))
          (values nil nil (list store) `(setq ,form ,store) form))
        (let ((temps (loop for arg in (rest form) collect (gensym)))
              (store (gensym "store-get-setf-expansion")))
          (values temps (rest form) (list store)
                  `(funcall #'(setf ,(first form)) ,store ,@temps)
                  `(,(first form) ,@temps))))))
(defmethod sicl-genv:default-setf-expander ((environment null))
  (sicl-genv:default-setf-expander clasp-cleavir:*clasp-env*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; REDEFINE GLOBAL CL FUNCTION: CL:GET-SETF-EXPANSION
;;;
;;; DANGER (but it should work)

(defun cl:get-setf-expansion (form &optional env)
  (sicl-genv:get-setf-expansion form env))
