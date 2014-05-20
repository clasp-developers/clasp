

(defmacro blog (fmt &rest args)
  `(progn
     (bformat t "BLOG --> ")
     (bformat t ,fmt ,@args)
     (bformat t "\n")
     ))
