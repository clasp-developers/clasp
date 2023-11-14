(in-package :core)

#+(or)
(eval-when (:execute)
  (format t "Setting core:*echo-repl-read* to T~%")
  (setq core:*echo-repl-read* t))

(eval-when (:execute :load-toplevel)


;;; Wipe out macros we can't have for cleavir

  (fmakunbound 'cleavir-primop:car)
  (fmakunbound 'cleavir-primop:cdr)
;;;  (fmakunbound 'core::header-stamp-case)
  )

;;; --------------------------------------------------
;;;
;;; Compile the builtin function wrappers
;;;
;;;

(defun describe-wrappers()
  (dolist (name sys:*builtin-function-names*)
    (format t "About to compile ~a ~a~%" name (cmp:builtin-wrapper-form name))))

(defun compile-wrappers ()
  (dolist (name sys:*builtin-function-names*)
    (when (cmp:builtin-wrapper-form name)
      (format t "Compiling wrapper for ~a ~a~%" name (cmp:builtin-wrapper-form name))
      (compile name))))

#-(and)
(eval-when (:compile-toplevel :execute)
  (compile-wrappers))
