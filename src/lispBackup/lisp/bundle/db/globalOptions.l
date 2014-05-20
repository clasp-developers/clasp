					; fixed code
(global *globalOptions* (Binder))

(defun setGlobalOption (optionName optionValue )
  (assert (symbolp optionName) (% "Option[%s] is of class[%s] and it should be a Symbol"
				     optionName (className optionName)))
  (print (% "Setting global option(%s) to value(%s)" (repr optionName )(repr optionValue )))
  (extend *globalOptions* optionName optionValue ))

(defun setGlobalOptionDefaultValue (optionName optionValue )
  (assert (symbolp optionName) (% "Option[%s] is of class[%s] and it should be a Symbol"
				     optionName (className optionName)))
  (print (% "Setting default value of global option(%s) to value(%s)" (repr optionName )(repr optionValue )))
  (when (not (contains *globalOptions* optionName ))
    (print (% "Setting global option default value (%s) = %s" optionName optionValue ))
    (extend *globalOptions* optionName optionValue ))
  )

(defun getGlobalOption (optionName )
  (assert (symbolp optionName) (% "Option[%s] is of class[%s] and it should be a Symbol"
				     optionName (className optionName)))
  (lookup *globalOptions* optionName ))
