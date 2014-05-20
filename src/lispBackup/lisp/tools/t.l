(when (isTopLevelScript)
  (print (% "In top level script first arg=%s" (repr (@ *ARGS* 0)))))