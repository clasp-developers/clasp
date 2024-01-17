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

(defun compile-wrappers ()
  (dolist (name sys:*builtin-function-names*)
    (setf (fdefinition name) (compile nil (fdefinition name)))))

#-(and)
(eval-when (:compile-toplevel :execute)
  (compile-wrappers))
