;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;

;(load "../cmp/init")

(defun lcs1 (file)
       (compile-file file
                     :c-file t :h-file t :data-file t :output-file nil
                     :system-p t))

(proclaim '(special sys::*indent-formatted-output*))

(defun compile-if-needed (file)
  (let ((cfile-date (file-write-date (merge-pathnames file #".c"))))
    (when (or (not cfile-date)
	      (> (file-write-date (merge-pathnames file #".lsp"))
		 cfile-date))
      (compile-file file :c-file t :h-file t :data-file t
		    :output-file nil :system-p t)))
  )
#|
(compile-if-needed "defmacro")
(compile-if-needed "evalmacros")
(compile-if-needed "top")
(compile-if-needed "module")
(compile-if-needed "predlib")
(compile-if-needed "setf")
(compile-if-needed "arraylib")
(compile-if-needed "assert")
(compile-if-needed "defstruct")
(compile-if-needed "describe")
(compile-if-needed "iolib")
(compile-if-needed "listlib")
(compile-if-needed "mislib")
(compile-if-needed "numlib")
(compile-if-needed "packlib")
(compile-if-needed "seq")
(compile-if-needed "seqlib")
(compile-if-needed "trace")
;(compile-if-needed "thread")
;(compile-if-needed "loop")
(bye)
|#
