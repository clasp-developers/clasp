
(defmacro do-cond (&environment env)
  (block lewp-body
    (let ((rbefore '(nil nil nil))
	  (rafter  '(( WHEN ( ATOM I ) ( GO END-LEWP ) ) (LEWP-REALLY-DESETQ I (CDR I  )  ) nil  ))
	  (threshold 40)
	  (flagvar nil)
	  )
      (do ((bb rbefore (cdr bb)) (aa rafter (cdr aa)) (lastdiff nil) (count 0) (inc nil))
	  ((progn
	     (lewp-log "Testing if bb is null - bb[~a] (null bb)[~a]~%" bb (null bb))
	     (null bb))
	   (lewp-log "Leaving do~%")
	   (return-from lewp-body)) ;Did it.
	(dbg-i32 4000001)
	(lewp-log "bb --> ~a~%" bb)
	(cond ((progn
		 (dbg-i32 4000002)
		 (lewp-log "First cond test - always fails ~%")
		 nil)
	       (lewp-log "Never evaluate me~%")
	       )
	      ((progn
		 (lewp-log "Second test~%")
		 (not (equal (car bb) (car aa))))
	       (lewp-log "succeeded not equal (car bb) --> ~a~%" (car bb))
	       (setq lastdiff bb count 0))
	      ((progn
		 (dbg-i32 4000003)
		 (lewp-log "or test~%")
		 (or (not (setq inc (estimate-code-size (car bb) env)))
		     (> (incf count inc) threshold)))
	       (lewp-log "succeeded or (car bb) --> ~a~%" (car bb))
	       ;; Ok, we have found a non-duplicatable piece of code.  Everything
	       ;; chronologically after it must be in the central body.
	       ;; Everything chronologically at and after lastdiff goes into the
	       ;; central body under a flag test.
	       (let ((then nil) (else nil))
		 (do () (nil)
		   (push (pop rbefore) else)
		   (push (pop rafter) then)
		   (when (eq rbefore (cdr lastdiff))
		     (lewp-log "Returning from upper inner do~%")
		     (return)))
		 (unless flagvar
		   (push `(setq ,(setq flagvar *lewp-iteration-flag-variable*) t) else))
		 (push `(if ,flagvar ,(pify (psimp then)) ,(pify (psimp else)))
		       main-body))
	       ;; Everything chronologically before lastdiff until the non-duplicatable form (car bb) 
	       ;; is the same in rbefore and rafter so just copy it into the body
	       (do () (nil)
		 (pop rafter)
		 (push (pop rbefore) main-body)
		 (when (eq rbefore (cdr bb))
		   (lewp-log "Returning from lower inner do~%")
		   (return)))
	       (lewp-log "Returning from lowest return~%")
	       (return))
	      )
	(dbg-i32 4000004)
	(lewp-log "Dropped through bottom of cond~%")
	)
      )
    )
  )


(do-cond)
