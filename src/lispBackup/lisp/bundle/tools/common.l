
(make-package :common)
(in-package :common)
(core::use-package 'core)
(use-package 'u)

(export 'filter
	)




;;
;; Applies fn to the contents of args and
;; returns a list of those args for which fn returns true
;;
(defun filter ( fn args )
  (let* ((first (Cons () ()))
	 (cur first))
    (for arg args
	 (let ((res (apply fn arg)))
	   (when (notNil res)
	     (let ((one (Cons arg)))
	       (setCdr cur one)
	       (setq cur one))
	     )))
    (cdr first)))

