

(defmacro lewp-log (format &rest args)
  `(format t ,format ,@args))


(defun estimate-code-size-1 ()
  (throw 'estimate-code-size 3))

(defun estimate-code-size (x env)
  (declare (si::c-local))
  (lewp-log "Entered estimate-code-size~%")
  (let ((result (catch 'estimate-code-size
		  (estimate-code-size-1) )))
    (lewp-log "Leaving estimate-code-size~%")
    result
    )
  )


(estimate-code-size 1 nil)
