					; 1
(defun foo (x)                          ; 2
  (print (list "First line x -->" x))   ; 3
  (print "second line")                 ; 4
  (gdb)                                 ; 5
  (print "last line")                   ; 6
  )                                     ; 7
					; 8
(foo 1)                                 ; 9
