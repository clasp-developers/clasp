
(eval-when (:execute :compile-toplevel :load-toplevel)
  (defvar *a* 10)
  (defvar *b* (load-time-value (+ *a* 10)))
  )

(print (list "*a* -> " *a* ))
(print (list "*b* -> " *b* ))

