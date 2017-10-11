;;; Functions that are more "explicit" versions of Lisp ones.
;;; Generally this means they take only required arguments, there are no designators,
;;; and some arguments are assumed to have been checked for validity already.
;;; This means they do not depend on different global environments and need only be compiled and linked once.

;;; They can be compiled+linked in whatever environment with standard CL, thus flagrant use of macros etc.

(defpackage #:explicit
  (:export #:funcall #:apply)
  (:export #:member #:member-not #:member-if #:member-if-not)
  (:export #:eval #:compile))

(defun explicit:funcall (function &rest arguments)
  (explicit:apply function arguments))
(define-compiler-macro explicit:funcall (function &rest arguments)
  `(cleavir-primop:funcall ,function ,@arguments))

#+clasp
(defun explicit:apply (function &rest spreadable-arguments)
  (cl:apply function spreadable-arguments))

(defun explicit:member (item list test key)
  (loop for ls on list
        when (explicit:funcall test item (explicit:funcall key (car ls)))
          return ls))
(defun explicit:member-not (item list test key)
  (loop for ls on list
        unless (explicit:funcall test item (explicit:funcall key (car ls)))
          return ls))

(defun explicit:member-if (test list key)
  (loop for ls on list
        when (explicit:funcall test (explicit:funcall key (car list)))
          return ls))
(defun explicit:member-if-not (test list key)
  (loop for ls on list
        unless (explicit:funcall test (explicit:funcall key (car list)))
          return ls))

(defun explicit:assoc (item list test key)
  (loop for pair in list
        when (and (not (null pair))
                  (explicit:funcall test item (explicit:funcall key (car pair))))
          return pair))
(defun explicit:assoc-not (item list test key)
  (loop for pair in list
        when (and (not (null pair))
                  (not (explicit:funcall test item (explicit:funcall key (car pair)))))
          return pair))

(defun explicit:assoc-if (test list key)
  (loop for pair in list
        when (and (not (null pair))
                  (explicit:funcall test (explicit:funcall key (car pair))))
          return pair))
(defun explicit:assoc-if-not (test list key)
  (loop for pair in list
        when (and (not (null pair))
                  (not (explicit:funcall test (explicit:funcall key (car pair)))))
          return pair))

#+(or)
(progn
#+clasp
(defun explicit:maphash (function table)
  (maphash function table))

(defun explicit:signal (condition)
  (signal condition))

(defun explicit:error (condition)
  (explicit:signal condition)
  ;; TODO: infinite error protect?
  ;; Actually, invoke-debugger might be super environment dependent?
  (explicit:invoke-debugger condition))

(defun explicit:warn (condition error-output)
  (restart-case (explicit:signal condition)
    (muffle-warning ()
      :report "Skip warning."
      (return-from explicit:warn nil)))
  (format error-output "~&;;; Warning: ~A~%" condition))
)

#+clasp
(defun explicit:eval (form environment)
  (clasp-cleavir::cclasp-eval form environment))

#+clasp
(defun explicit:compile (lambda-expression environment)
  (cmp:compile-in-env nil lambda-expression environment cmp:*cleavir-compile-hook* 'llvm-sys:external-linkage))
