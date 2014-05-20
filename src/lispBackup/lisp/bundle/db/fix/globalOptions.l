; fixed code
(global *globalOptions*  (ObjectDictionary   ) )

(defun setGlobalOption  (optionName  optionValue   ) ;END1
(println (%   "Setting global option(%s) to value(%s)"   (repr  optionName   )(repr  optionValue   ) ) )

(put   *globalOptions*   optionName  optionValue   )
)

(defun setGlobalOptionDefaultValue  (optionName  optionValue   ) ;END1
(println (%   "Setting default value of global option(%s) to value(%s)"   (repr  optionName   )(repr  optionValue   ) ) )

(ifTrue (contains   *globalOptions*   optionName   ) ;END1
(return )
)

(println (%   "Setting global option default value (%s) = %s"   optionName  optionValue   ) )

(put   *globalOptions*   optionName  optionValue   )
)

(defun getGlobalOption  (optionName   ) ;END1
(return (get   *globalOptions*   optionName   ) )
)
