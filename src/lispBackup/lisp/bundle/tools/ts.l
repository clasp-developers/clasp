
(include "scoring.l")

(score:scorer :name "test"
	      :setup '(progn
			(print "Setup code")
			(print (format "ScorerVirtualMachine = %s" (repr *svm*) ))))