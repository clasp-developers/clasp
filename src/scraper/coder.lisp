(in-package :cscraper)

(defun group-expose-functions-by-namespace (tags)
  (let ((ns-hashes (make-hash-table :test #'equal))
        current-namespace)
    (loop for tag in tags
         (cond
           ((typep tag 'namespace-tag)
            (setq current-namespace (tags:namespace tag)))
           ((typep tag 'expose-function-tag)
            (or current-namespace (error "There must be a current namespace defined"))
            (let ((ns-list (gethash current-namespace ns-hashes) nil))
              (push tag ns-list)
              (setf (gethash current-namespace ns-hashes) ns-list)))
           (t #|nothing|#)))
    ns-hashes))



(defun generate-expose-function-code (sout ns-grouped-expose-functions)
  (format sout "#ifdef EXPOSE_FUNCTION_SIGNATURES~%")
  (maphash (lambda (ns funcs)
             (format sout "namespace ~a {~%" ns)
             (dolist (f funcs)
               (format sout "  // ~a~%" (tags:function-name f))
               (format sout "    ~a;~%" (tags:signature-text f)))
             (format sout "};~%"))
           ns-grouped-expose-functions)
  (format sout "#endif // EXPOSE_FUNCTION_SIGNATURES~%"))


(defun generate-expose-function-code (sout ns-grouped-expose-functions)
  (format sout "#ifdef EXPOSE_FUNCTION_SIGNATURES~%")
  (maphash (lambda (ns funcs)
             (format sout "namespace ~a {~%" ns)
             (dolist (f funcs)
               (format sout "  // ~a~%" (tags:function-name f))
               (format sout "    ~a;~%" (tags:signature-text f)))
             (format sout "};~%"))
           ns-grouped-expose-functions)
  (format sout "#endif // EXPOSE_FUNCTION_SIGNATURES~%"))

