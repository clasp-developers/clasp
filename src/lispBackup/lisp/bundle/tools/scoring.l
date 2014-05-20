

(make-package :scoring)
(in-package :scoring)
(l:use-package :l)
(use-package :m)

(print "Installing :scoring package")


;
;
(defun simplify-upoint ( svm svmSym providerSymArg monomerAlias atomAlias )
  (let ((ref (upoint svm providerSymArg monomerAlias atomAlias)))
    `(pointReference ,svmSym (quote ,ref))))

(defun simplify-tpoint ( svm svmSym providerSymArg monomerAlias atomAlias )
  (let ((ref (tpoint svm providerSymArg monomerAlias atomAlias)))
    `(pointReference ,svmSym (quote ,ref))))







(global *score-setup-macros* (Binder))

(global *score-macros* (Binder))
(extend *score-macros* :upoint #'simplify-upoint )
(extend *score-macros* :tpoint #'simplify-tpoint )

(log (% "Description of *score-macros* = %s" (description *score-macros*)))

(defun score-macro-lookup (sym env)
  (when (isA sym Symbol)
    (let ((kwSym (asKeywordSymbol sym)))
      (log (% "score-macro-lookup sym(%s) is a symbol" (repr kwSym)))
      (log (% "Looking in binder(%s)" (description env)))
      (when (contains env kwSym)
	(log (% "====> The Binder contains the keyword symbol(%s)" (repr kwSym)))
	(let ((result (lookup env kwSym)))
	  (log (% " ---------> returning result %s" (repr result )))
	  result )))))
   

(defun score-optimize-list (svm cons env)
  (map (lambda ( entry ) (score-optimize svm entry env)) cons ))

(defun score-optimize (svm obj env)
  (if (atomQ obj)
      obj
    (let* ((head (car obj))
	   (macroExec (score-macro-lookup head env)))
      (log (% "head symbol = %s  and it is a macro = %d" (repr head) (repr macroExec)))
      (if macroExec
	  (let ((optimized (apply macroExec svm (cdr obj))))
	    (log (% "-----> Optimized code: %s" (repr optimized)))
	    optimized)
	(let* ((optimized (score-optimize-list svm (cdr obj) env))
	       (result ``obj ( ,(car obj) ,@optimized ) ))
	  result)))))


(defun score-optimize-procedure (svm proc env)
  (let* ((argumentHandler (getArgumentHandler proc))
	(code (getCode proc))
	(optimizedCode (score-optimize svm code env))
	(optimizedProc (Procedure argumentHandler optimizedCode)))
	(print ">>>>>>>>> Before optimization code=")
	(print (% "%s" (repr code)))
	(print (% ">>>>>>>> after = %s" (repr optimizedCode)))
    optimizedProc))



(defun scorer ( &key name declares moveAndScoreCode scoreCode )
  ; set dynamic variable *svm* here and all setup code and
  ; scoring code will refer to this virtual machine
  ; This is a cool Lisp capability
  (let ((svm (ScorerVirtualMachine)))
    (log "scorer start")
    (eraseProgram svm)
    (apply declares svm) ; evaluate the declarations to setup the svm
    (log (debugDump svm))
;; New-way
    (let ((optimized-move-code (score-optimize-procedure svm moveAndScoreCode *score-macros*)))
      (setMoveAndScoreProgram svm (eval optimized-move-code)))
    (let ((optimized-score-code (score-optimize-procedure svm scoreCode *score-macros*)))
      (setScoreProgram svm (eval optimized-score-code)))
;; Old-way
;;    (let ((optimized-move-code (score-optimize svm moveAndScoreCode *score-macros*)))
;;      (setMoveAndScoreProgram svm (eval optimized-move-code)))
;;    (let ((optimized-score-code (score-optimize svm scoreCode *score-macros*)))
;;      (setScoreProgram svm (eval optimized-score-code)))
    svm ))



(export 'scorer)

