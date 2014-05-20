(defparameter rs (ReadSerializer))

(with-open-file (sin "_test.dat" :direction :input)
  (readFromStream rs sin))


(do () ((not (hasObject rs)))
  (multiple-value-bind (obj name) (nextObject rs)
      (print (% "Name[%s] Object[%s]" name obj))))


(print (% "Object with name[%s] = %s" "binder" (namedObject rs "binder")))
