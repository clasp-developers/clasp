(defmacro fmt-log (&rest args) `(print (list "FMT-LOG" ,@args)))


(let ((colonp nil)
      (atsignp t)
      (params nil)
      (directives (#( 'FORMAT-DIRECTIVE "~@[~A~]" 3 5 #\A nil nil nil ) #( 'FORMAT-DIRECTIVE "~@[~A~]" 5 7 #\] nil nil nil )  ))
      (args '( test )))
#|  (setf args
	(progn
	  (fmt-log "line 2177 args: " args)
	  (if atsignp
	      (progn
		(fmt-log "line 2180 args: " args)
		(if colonp
		    (error 'format-error
			   :complaint
			   "Cannot specify both the colon and at-sign modifiers.")
		    (progn
		      (fmt-log "line 2182 args:" args)
		      (if (cdr sublists)
			  (error 'format-error
				 :complaint
				 "Can only specify one section")
			  (progn
			    (fmt-log "line 2188 params: " params " args: " args)
			    #||
			    (interpret-bind-defaults () params
			    (let ((prev-args args)
			    (arg (next-arg)))
			    (if arg
			    (interpret-directive-list stream
			    (car sublists)
			    orig-args
			    prev-args)
			    args)))
			    ||#
			    )))))
	      (if colonp
		  (if (= (length sublists) 2)
		      #||
		      (interpret-bind-defaults () params
		      (if (next-arg)
		      (interpret-directive-list stream (car sublists)
		      orig-args args)
		      (interpret-directive-list stream (cadr sublists)
		      orig-args args)))
		      ||#
		      (error 'format-error
			     :complaint
			     "Must specify exactly two sections."))
		  (progn
		    (fmt-log "line 50")
		    #|
		    (interpret-bind-defaults ((index (next-arg))) params
		    (let* ((default (and last-semi-with-colon-p
		    (pop sublists)))
		    (last (1- (length sublists)))
		    (sublist
		    (if (<= 0 index last)
		    (nth (- last index) sublists)
		    default)))
		    (interpret-directive-list stream sublist orig-args
		    args)))
		    |#
		    ))
		    ))
  )
|#
  )

