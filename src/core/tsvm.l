
(require "CANDO/tools/scoring.l")
(use-package :score)

(apropos "declare-")
(global *scorer* (scorer :name :test
			       :declares '( progn
					    (declare-int :i1 10)
					    (declare-string :testRan "Test ran: ")
					    (declare-string :space " ")
					    )
			       
			       :code '(
				       (printString :testRan )
				       (random01)
				       (popPrintReal)
				       (printCr)
				       (recordScorerState))))
(global *ss* (ScorerState))
(run *scorer* *ss*)
(print (% "Log = %s" (scoreLogContents *ss*)))

