(in-package #:core)

(defun check-package-lock (name operation) ;; testing
  (let ((package (symbol-package name)))
    (when (and package (ext:package-locked-p package)
               (not (member
                     *package*
                     (ext:package-implemented-by-list package))))
      (package-lock-violation package
                              "trying to ~s ~s"
                              operation name))))

(defvar *compiler-macros* (make-hash-table :test #'equal :thread-safe t))

(defun compiler-macro-function (name &optional environment)
  (declare (ignore environment))
  (values (gethash name *compiler-macros*)))

(defun (setf compiler-macro-function) (cmf name &optional environment)
  (declare (ignore environment))
  (check-package-lock (function-block-name name)
                      'define-compiler-macro)
  ;; Basically ETYPECASE.
  (if (functionp cmf)
      (setf (gethash name *compiler-macros*) cmf)
      (if (null cmf)
          (progn (remhash name *compiler-macros*) nil)
          (error 'type-error :datum cmf :expected-type '(or function null)))))

(defun compiler-macroexpand-1 (form &optional env)
  (if (atom form)
      form
      (or
       (and (eq (car form) 'cl:funcall)
            (listp (cadr form))
            (eq (car (cadr form)) 'cl:function)
            (let ((expander (compiler-macro-function (cadr (cadr form)) env)))
              (if expander
                  (funcall *macroexpand-hook* expander form env)
                  form)))
       (let ((expander (compiler-macro-function (car form) env)))
         (if expander
             (funcall *macroexpand-hook* expander form env)
             form)))))

(defun compiler-macroexpand (form &optional env)
  (let ((expansion (compiler-macroexpand-1 form env)))
    (if (eq expansion form)
        (return-from compiler-macroexpand form)
        (compiler-macroexpand expansion env))))

(export '(compiler-macroexpand-1 compiler-macroexpand))
