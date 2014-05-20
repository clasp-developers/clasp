


(defun test-mpi ()
  (if (eql (mpi:rank mpi:*world*) 0)
      (print "Manager")
      (format t "Worker ~a~%" (mpi:rank mpi:*world*))))

(test-mpi)



(defparameter *compile-commands-path* (probe-file "brcl:src;main;compile_commands.json"))



(defun find-globals-locals-parameters (&key testing)
  (let ((results (asttooling:ast-search '(asttooling:global-variables asttooling:local-variables)
				     *compile-commands-path*
			    (if testing *testing-files* nil))))
    (let ((globals (cdr (assoc 'asttooling:global-variables results)))
	  (locals (cdr (assoc 'asttooling:local-variables results)))
	  (parameters (cdr (assoc 'asttooling:parameters results))))
      (values globals locals parameters))))
