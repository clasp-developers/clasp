(defun testdefun (x y)
  (print "testdefun")
  (+ x y))

(test (eql (testdefun 1 2) 3))
