(princ "======================================================================") (terpri)

(princ "We are now executing FILE1.LSP") (terpri)

(defun test-function (x y)
  (format t "~D + ~D is equal to ~D~%" x y (+ x y)))

(princ "TEST-FUNCTION has been created") (terpri)
