
(defvar	*ff* (loadArchive (% "%s/gaff.cxml" (databaseDir ))))

(defvar *m* (Minimizer :matter (Aggregate) :forceField *ff*))

(defun minStuff () (throwMinimizerExceededMaxSteps *m*))


(handler-case
 (handler-case
  (progn
    (print "Hello")
    (minStuff)
    )
  (MathException (er) (print (% "Caught %s" (message er))))
  )
 (MinimizerError (er) (print (% "Caught in outer handler-case msg=%s" (message er))))
 )
