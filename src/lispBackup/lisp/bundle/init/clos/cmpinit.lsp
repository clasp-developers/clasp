;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
(defvar std-compile (symbol-function 'compile-file))
(defun compile-file (file &key (output-file (make-pathname :type "o" :defaults file)))
  (funcall std-compile
	   file
	   :c-file t :h-file t :data-file t :system-p t
	   :output-file nil))

;(setq compiler:*cc* (concatenate 'STRING compiler:*cc* " -I../h"))
