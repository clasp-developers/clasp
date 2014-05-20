

(defun file-forms (path)
  "Sucks up an entire file from PATH into a list of forms (sexprs),
      returning two values: the list of forms and the number of forms read."
  (with-open-file (s path)
    (loop with eof = (list nil)
          for form = (read s nil eof)
          until (eq form eof)
          collect form into forms
          counting t into form-count
          finally (return (values forms form-count)))))



(file-forms "closette.src")

(defparameter a (file-forms "t.src"))

(defparameter b (car a))
(nth 3 (nth 3 b))



(defun symbol-to-c (sym)
  (with-output-to-string (sout)
    (with-input-from-string (sin (symbol-name sym))
      (do ((c (read-char sin nil)
	      (read-char sin nil)))
	  ((null c))
 	(cond
	  ((eql c #\-) (format sout "_"))
	  ((eql c #\*) (format sout "Star"))
	  ((upper-case-p c) (format sout "~a" (char-downcase c)))
 	  (t (format sout "~a" c))
 	  )
	)
      )
    )
  )

(defgeneric emit (head tail))


(defun handle-defun (function-name args &rest body)
  (with-output-to-string (sout)
    (format sout "#define ARGS_fn_~a \"~a\"~%" (symbol-to-c function-name) args)))

(handle-defun 'aaa '(a b c) ())
(defmethod emit ((head (eql 'defun)) tail)
  (apply #'handle-defun tail))


(emit 'defun '(a 3))




