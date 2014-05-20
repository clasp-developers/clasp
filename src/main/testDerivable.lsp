
(defclass my-arg-adjuster (ast-tooling:derivable-arguments-adjuster)
  ())


(core:defvirtual ast-tooling:derivable-arguments-adjuster-adjust ((self my-arg-adjuster) args)
  (setq args (concatenate 'vector args #("-DAnotherArg")))
  (format t "in my-arg-adjuster arguments-adjuster-adjust with args: ~a~%" args)
  args)


(defparameter *a* (make-instance 'my-arg-adjuster))


(defun run-test ()
  (ast-tooling:test-derivable *a* #("-DArg1" "-DArgTest2")))
