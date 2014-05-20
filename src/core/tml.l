
(defvar *a* '(a b (c d) (e f)))

(print "About to defmacro")

(defvar *svm* "GlobalSvm")

(defun print-svm ()
  (print "In function *svm*")
  (print *svm* ))

(print "About to macroexpand")

(defun scorer (code)
  (let ((*svm* "MySvm"))
    (print-svm)
    (macroexpand-1 code)))



(defvar *b* (scorer '(c
		       (if a
			   (c m n)
			 )
)))
(print *b*)
(print "")

  