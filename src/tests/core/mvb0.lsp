;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(defun a ()
  (values 1 2))

(multiple-value-bind
      (a b)
    (let ((x 1))
      (multiple-value-bind
	    (a b)
	  (progn
	    (funcall #'a))
	(values a b)))
  (print (list a b)))

