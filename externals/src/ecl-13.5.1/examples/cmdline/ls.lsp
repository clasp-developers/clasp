#|
To create a program called ls.exe type the following lines
from a lisp prompt:

  (compile-file "ls.lsp" :output-file "ls.o" :system-p t)
  (c::build-program "ls" :lisp-files '("ls.o"))

NOTE: The content of this file must match the example in the
      documentation.
|#

(setq ext:*help-message* "
ls [--help | -?] filename*

     Lists the file that match the given patterns.
")

(defun print-directory (pathnames)
 (format t "~{~A~%~}"
  (mapcar #'(lambda (x) (enough-namestring x (si::getcwd)))
	  (mapcan #'directory (or pathnames '("*.*" "*/"))))))

(defconstant +ls-rules+
'(("--help" 0 (progn (princ ext:*help-message* *standard-output*) (ext:quit 0)))
  ("-?" 0 (progn (princ ext:*help-message* *standard-output*) (ext:quit 0)))
  ("*DEFAULT*" 1 (print-directory 1) :stop)))

(let ((ext:*lisp-init-file-list* NIL)) ; No initialization files
  (handler-case (ext:process-command-args :rules +ls-rules+)
    (error (c)
       (princ ext:*help-message* *error-output*)
       (ext:quit 1))))
(ext:quit 0)
